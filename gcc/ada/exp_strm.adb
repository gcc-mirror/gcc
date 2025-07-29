------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ S T R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Exp_Util;       use Exp_Util;
with Mutably_Tagged; use Mutably_Tagged;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Rtsfind;        use Rtsfind;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;

package body Exp_Strm is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Array_Read_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id);
   --  Common routine shared to build either an array Read procedure or an
   --  array Write procedure, Nam is Name_Read or Name_Write to select which.
   --  Pnam is the defining identifier for the constructed procedure. The
   --  other parameters are as for Build_Array_Read_Procedure.

   procedure Build_Record_Read_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id);
   --  Common routine shared to build a record Read Write procedure, Nam
   --  is Name_Read or Name_Write to select which. Pnam is the defining
   --  identifier for the constructed procedure. The other parameters are
   --  as for Build_Record_Read_Procedure.

   procedure Build_Stream_Function
     (Typ   : Entity_Id;
      Decl  : out Node_Id;
      Fnam  : Entity_Id;
      Decls : List_Id;
      Stms  : List_Id);
   --  Called to build an array or record stream function. The first three
   --  arguments are the same as Build_Record_Or_Elementary_Input_Function.
   --  Decls and Stms are the declarations and statements for the body and
   --  The parameter Fnam is the name of the constructed function.

   function Has_Stream_Standard_Rep (U_Type : Entity_Id) return Boolean;
   --  This function is used to test the type U_Type, to determine if it has
   --  a standard representation from a streaming point of view. Standard means
   --  that it has a standard representation (e.g. no enumeration rep clause),
   --  and the size of the root type is the same as the streaming size (which
   --  is defined as value specified by a Stream_Size clause if present, or
   --  the Esize of U_Type if not).

   function Make_Stream_Subprogram_Name
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id;
   --  Return the entity that identifies the stream subprogram for type Typ
   --  that is identified by the given Nam. This procedure deals with the
   --  difference between tagged types (where a single subprogram associated
   --  with the type is generated) and all other cases (where a subprogram
   --  is generated at the point of the stream attribute reference). The
   --  Loc parameter is used as the Sloc of the created entity.

   function Stream_Base_Type (E : Entity_Id) return Entity_Id;
   --  For an array type whose whose first subtype is constrained, return
   --  the first subtype. For the internal representation type corresponding
   --  to a mutably tagged type, return the mutably tagged type. Otherwise,
   --  return the base type. Similar to Exp_Put_Image.Put_Image_Base_Type.

   --------------------------------
   -- Build_Array_Input_Function --
   --------------------------------

   --  The function we build looks like

   --    function typSI[_nnn] (S : access RST) return Typ is
   --      L1 : constant Index_Type_1 := Index_Type_1'Input (S);
   --      H1 : constant Index_Type_1 := Index_Type_1'Input (S);
   --      L2 : constant Index_Type_2 := Index_Type_2'Input (S);
   --      H2 : constant Index_Type_2 := Index_Type_2'Input (S);
   --      ..
   --      Ln : constant Index_Type_n := Index_Type_n'Input (S);
   --      Hn : constant Index_Type_n := Index_Type_n'Input (S);
   --
   --      V : Typ'Base (L1 .. H1, L2 .. H2, ... Ln .. Hn)

   --    begin
   --      Typ'Read (S, V);
   --      return V;
   --    end typSI[_nnn]

   --  Note: the suffix [_nnn] is present for untagged types, where we generate
   --  a local subprogram at the point of the occurrence of the attribute
   --  reference, so the name must be unique.

   procedure Build_Array_Input_Function
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Loc    : constant Source_Ptr := Sloc (Typ);
      Dim    : constant Pos := Number_Dimensions (Typ);
      Lnam   : Name_Id;
      Hnam   : Name_Id;
      Decls  : List_Id;
      Ranges : List_Id;
      Stms   : List_Id;
      Rstmt  : Node_Id;
      Indx   : Node_Id;
      Odecl  : Node_Id;

   begin
      Decls := New_List;
      Ranges := New_List;
      Indx  := First_Index (Typ);
      for J in 1 .. Dim loop
         Lnam := New_External_Name ('L', J);
         Hnam := New_External_Name ('H', J);

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Lnam),
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Etype (Indx), Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
                 Attribute_Name => Name_Input,
                 Expressions    => New_List (Make_Identifier (Loc, Name_S)))));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Hnam),
             Constant_Present    => True,
             Object_Definition   =>
                   New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
                 Attribute_Name => Name_Input,
                 Expressions    => New_List (Make_Identifier (Loc, Name_S)))));

         Append_To (Ranges,
           Make_Range (Loc,
             Low_Bound  => Make_Identifier (Loc, Lnam),
             High_Bound => Make_Identifier (Loc, Hnam)));

         Next_Index (Indx);
      end loop;

      --  If the type is constrained, use it directly. Otherwise build a
      --  subtype indication with the proper bounds.

      if Is_Constrained (Typ) then
         Odecl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Object_Definition   => New_Occurrence_Of (Typ, Loc));

      else
         Odecl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Object_Definition   =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (Stream_Base_Type (Typ), Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc, Ranges)));
      end if;

      Rstmt :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Typ, Loc),
          Attribute_Name => Name_Read,
          Expressions    => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Identifier (Loc, Name_V)));

      Stms := New_List (
         Make_Extended_Return_Statement (Loc,
           Return_Object_Declarations => New_List (Odecl),
           Handled_Statement_Sequence =>
             Make_Handled_Sequence_Of_Statements (Loc, New_List (Rstmt))));

      Fnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Stream_Input));

      Build_Stream_Function (Typ, Decl, Fnam, Decls, Stms);
   end Build_Array_Input_Function;

   ----------------------------------
   -- Build_Array_Output_Procedure --
   ----------------------------------

   procedure Build_Array_Output_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);
      Stms : List_Id;
      Indx : Node_Id;

   begin
      --  Build series of statements to output bounds

      Indx := First_Index (Typ);
      Stms := New_List;

      for J in 1 .. Number_Dimensions (Typ) loop
         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
             Attribute_Name => Name_Write,
             Expressions    => New_List (
               Make_Identifier (Loc, Name_S),
               Make_Attribute_Reference (Loc,
                 Prefix         => Make_Identifier (Loc, Name_V),
                 Attribute_Name => Name_First,
                 Expressions    => New_List (
                   Make_Integer_Literal (Loc, J))))));

         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
             Attribute_Name => Name_Write,
             Expressions    => New_List (
               Make_Identifier (Loc, Name_S),
               Make_Attribute_Reference (Loc,
                 Prefix         => Make_Identifier (Loc, Name_V),
                 Attribute_Name => Name_Last,
                 Expressions    => New_List (
                   Make_Integer_Literal (Loc, J))))));

         Next_Index (Indx);
      end loop;

      --  Append Write attribute to write array elements

      Append_To (Stms,
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Typ, Loc),
          Attribute_Name => Name_Write,
          Expressions => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Identifier (Loc, Name_V))));

      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Stream_Output));

      Build_Stream_Procedure (Typ, Decl, Pnam, Stms, Outp => False);
   end Build_Array_Output_Procedure;

   --------------------------------
   -- Build_Array_Read_Procedure --
   --------------------------------

   procedure Build_Array_Read_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Typ);

   begin
      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Stream_Read));
      Build_Array_Read_Write_Procedure (Typ, Decl, Pnam, Name_Read);
   end Build_Array_Read_Procedure;

   --------------------------------------
   -- Build_Array_Read_Write_Procedure --
   --------------------------------------

   --  The form of the array read/write procedure is as follows:

   --    procedure pnam (S : access RST, V : [out] Typ) is
   --    begin
   --       for L1 in V'Range (1) loop
   --          for L2 in V'Range (2) loop
   --             ...
   --                for Ln in V'Range (n) loop
   --                   Component_Type'Read/Write (S, V (L1, L2, .. Ln));
   --                end loop;
   --             ..
   --          end loop;
   --       end loop
   --    end pnam;

   --  The out keyword for V is supplied in the Read case

   procedure Build_Array_Read_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);
      Ndim : constant Pos        := Number_Dimensions (Typ);
      Ctyp : constant Entity_Id  := Component_Type (Typ);

      Stm  : Node_Id;
      Exl  : List_Id;
      RW   : Entity_Id;

   begin
      --  First build the inner attribute call

      Exl := New_List;

      for J in 1 .. Ndim loop
         Append_To (Exl, Make_Identifier (Loc, New_External_Name ('L', J)));
      end loop;

      Stm :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Stream_Base_Type (Ctyp), Loc),
          Attribute_Name => Nam,
          Expressions => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Indexed_Component (Loc,
              Prefix      => Make_Identifier (Loc, Name_V),
              Expressions => Exl)));

      --  The corresponding stream attribute for the component type of the
      --  array may be user-defined, and be frozen after the type for which
      --  we are generating the stream subprogram. In that case, freeze the
      --  stream attribute of the component type, whose declaration could not
      --  generate any additional freezing actions in any case.

      if Nam = Name_Read then
         RW := TSS (Base_Type (Ctyp), TSS_Stream_Read);
      else
         RW := TSS (Base_Type (Ctyp), TSS_Stream_Write);
      end if;

      if Present (RW)
        and then not Is_Frozen (RW)
      then
         Set_Is_Frozen (RW);
      end if;

      --  Now this is the big loop to wrap that statement up in a sequence
      --  of loops. The first time around, Stm is the attribute call. The
      --  second and subsequent times, Stm is an inner loop.

      for J in 1 .. Ndim loop
         Stm :=
           Make_Implicit_Loop_Statement (Typ,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Loc,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Loc,
                     Defining_Identifier =>
                       Make_Defining_Identifier (Loc,
                         Chars => New_External_Name ('L', Ndim - J + 1)),

                     Discrete_Subtype_Definition =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => Make_Identifier (Loc, Name_V),
                         Attribute_Name => Name_Range,

                         Expressions => New_List (
                           Make_Integer_Literal (Loc, Ndim - J + 1))))),

             Statements => New_List (Stm));

      end loop;

      Build_Stream_Procedure
        (Typ, Decl, Pnam, New_List (Stm), Outp => Nam = Name_Read);
   end Build_Array_Read_Write_Procedure;

   ---------------------------------
   -- Build_Array_Write_Procedure --
   ---------------------------------

   procedure Build_Array_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Typ);
   begin
      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Stream_Write));
      Build_Array_Read_Write_Procedure (Typ, Decl, Pnam, Name_Write);
   end Build_Array_Write_Procedure;

   ---------------------------------
   -- Build_Elementary_Input_Call --
   ---------------------------------

   function Build_Elementary_Input_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (Prefix (N));
      U_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      FST     : constant Entity_Id  := First_Subtype (U_Type);
      Strm    : constant Node_Id    := First (Expressions (N));
      Targ    : constant Node_Id    := Next (Strm);
      P_Size  : constant Uint       := Get_Stream_Size (FST);
      Res     : Node_Id;
      Lib_RE  : RE_Id;

   begin
      --  Check first for Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

      if Rt_Type = Standard_Boolean
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_I_B;

      elsif Rt_Type = Standard_Character
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_I_C;

      elsif Rt_Type = Standard_Wide_Character
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_I_WC;

      elsif Rt_Type = Standard_Wide_Wide_Character
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_I_WWC;

      --  Floating point types

      elsif Is_Floating_Point_Type (U_Type) then

         --  Question: should we use P_Size or Rt_Type to distinguish between
         --  possible floating point types? If a non-standard size or a stream
         --  size is specified, then we should certainly use the size. But if
         --  we have two types the same (notably Short_Float_Size = Float_Size
         --  which is close to universally true, and Long_Long_Float_Size =
         --  Long_Float_Size, true on most targets except the x86), then we
         --  would really rather use the root type, so that if people want to
         --  fiddle with System.Stream_Attributes to get inter-target portable
         --  streams, they get the size they expect. Consider in particular the
         --  case of a stream written on an x86, with 96-bit Long_Long_Float
         --  being read into a non-x86 target with 64 bit Long_Long_Float. A
         --  special version of System.Stream_Attributes can deal with this
         --  provided the proper type is always used.

         --  To deal with these two requirements we add the special checks
         --  on equal sizes and use the root type to distinguish.

         if P_Size <= Standard_Short_Float_Size
           and then (Standard_Short_Float_Size /= Standard_Float_Size
                     or else Rt_Type = Standard_Short_Float)
         then
            Lib_RE := RE_I_SF;

         elsif P_Size <= Standard_Float_Size then
            Lib_RE := RE_I_F;

         elsif P_Size <= Standard_Long_Float_Size
           and then (Standard_Long_Float_Size /= Standard_Long_Long_Float_Size
                       or else Rt_Type = Standard_Long_Float)
         then
            Lib_RE := RE_I_LF;

         else
            Lib_RE := RE_I_LLF;
         end if;

      --  Signed integer types. Also includes signed fixed-point types and
      --  enumeration types with a signed representation.

      --  Note on signed integer types. We do not consider types as signed for
      --  this purpose if they have no negative numbers, or if they have biased
      --  representation. The reason is that the value in either case basically
      --  represents an unsigned value.

      --  For example, consider:

      --     type W is range 0 .. 2**32 - 1;
      --     for W'Size use 32;

      --  This is a signed type, but the representation is unsigned, and may
      --  be outside the range of a 32-bit signed integer, so this must be
      --  treated as 32-bit unsigned.

      --  Similarly, if we have

      --     type W is range -1 .. +254;
      --     for W'Size use 8;

      --  then the representation is unsigned

      elsif not Is_Unsigned_Type (FST)

        --  The following set of tests gets repeated many times, we should
        --  have an abstraction defined ???

        and then
          (Is_Fixed_Point_Type (U_Type)
             or else
           Is_Enumeration_Type (U_Type)
             or else
           (Is_Signed_Integer_Type (U_Type)
              and then not Has_Biased_Representation (FST)))

      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_I_SSI;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_I_SI;

         elsif P_Size = 24 then
            Lib_RE := RE_I_I24;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_I_I;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_I_LI;

         elsif P_Size <= Standard_Long_Long_Integer_Size then
            Lib_RE := RE_I_LLI;

         else
            Lib_RE := RE_I_LLLI;
         end if;

      --  Unsigned integer types, also includes unsigned fixed-point types
      --  and enumeration types with an unsigned representation (note that
      --  we know they are unsigned because we already tested for signed).

      --  Also includes signed integer types that are unsigned in the sense
      --  that they do not include negative numbers. See above for details.

      elsif Is_Modular_Integer_Type    (U_Type)
        or else Is_Fixed_Point_Type    (U_Type)
        or else Is_Enumeration_Type    (U_Type)
        or else Is_Signed_Integer_Type (U_Type)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_I_SSU;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_I_SU;

         elsif P_Size = 24 then
            Lib_RE := RE_I_U24;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_I_U;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_I_LU;

         elsif P_Size <= Standard_Long_Long_Integer_Size then
            Lib_RE := RE_I_LLU;

         else
            Lib_RE := RE_I_LLLU;
         end if;

      else pragma Assert (Is_Access_Type (U_Type));
         if Present (P_Size) and then P_Size > System_Address_Size then
            Lib_RE := RE_I_AD;
         else
            Lib_RE := RE_I_AS;
         end if;
      end if;

      --  Call the function, and do an unchecked conversion of the result
      --  to the actual type of the prefix. If the target is a discriminant,
      --  and we are in the body of the default implementation of a 'Read
      --  attribute, set target type to force a constraint check (13.13.2(35)).
      --  If the type of the discriminant is currently private, add another
      --  unchecked conversion from the full view.

      if Nkind (Targ) = N_Identifier
        and then Is_Internal_Name (Chars (Targ))
        and then Is_TSS (Scope (Entity (Targ)), TSS_Stream_Read)
      then
         Res :=
           Unchecked_Convert_To (Base_Type (U_Type),
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Lib_RE), Loc),
               Parameter_Associations => New_List (
                 Relocate_Node (Strm))));

         Set_Do_Range_Check (Res);

         if Base_Type (P_Type) /= Base_Type (U_Type) then
            Res := Unchecked_Convert_To (Base_Type (P_Type), Res);
         end if;

         return Res;

      else
         Res :=
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (RTE (Lib_RE), Loc),
             Parameter_Associations => New_List (
               Relocate_Node (Strm)));

         --  Now convert to the base type if we do not have a biased type. Note
         --  that we did not do this in some older versions, and the result was
         --  losing a required range check in the case where 'Input is being
         --  called from 'Read.

         if not Has_Biased_Representation (P_Type) then
            return Unchecked_Convert_To (Base_Type (P_Type), Res);

         --  For the biased case, the conversion to the base type loses the
         --  biasing, so just convert to Ptype. This is not quite right, and
         --  for example may lose a corner case CE test, but it is such a
         --  rare case that for now we ignore it ???

         else
            return Unchecked_Convert_To (P_Type, Res);
         end if;
      end if;
   end Build_Elementary_Input_Call;

   ---------------------------------
   -- Build_Elementary_Write_Call --
   ---------------------------------

   function Build_Elementary_Write_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (Prefix (N));
      U_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      Rt_Type : constant Entity_Id  := Root_Type (U_Type);
      FST     : constant Entity_Id  := First_Subtype (U_Type);
      Strm    : constant Node_Id    := First (Expressions (N));
      Item    : constant Node_Id    := Next (Strm);
      P_Size  : Uint;
      Lib_RE  : RE_Id;
      Libent  : Entity_Id;

   begin
      --  Compute the size of the stream element. This is either the size of
      --  the first subtype or if given the size of the Stream_Size attribute.

      if Has_Stream_Size_Clause (FST) then
         P_Size := Static_Integer (Expression (Stream_Size_Clause (FST)));
      else
         P_Size := Esize (FST);
      end if;

      --  Find the routine to be called

      --  Check for First Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol. However, this special handling only applies
      --  if they have standard representation, otherwise they are treated like
      --  any other enumeration type.

      if Rt_Type = Standard_Boolean
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_W_B;

      elsif Rt_Type = Standard_Character
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_W_C;

      elsif Rt_Type = Standard_Wide_Character
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_W_WC;

      elsif Rt_Type = Standard_Wide_Wide_Character
        and then Has_Stream_Standard_Rep (U_Type)
      then
         Lib_RE := RE_W_WWC;

      --  Floating point types

      elsif Is_Floating_Point_Type (U_Type) then

         --  Question: should we use P_Size or Rt_Type to distinguish between
         --  possible floating point types? If a non-standard size or a stream
         --  size is specified, then we should certainly use the size. But if
         --  we have two types the same (notably Short_Float_Size = Float_Size
         --  which is close to universally true, and Long_Long_Float_Size =
         --  Long_Float_Size, true on most targets except the x86), then we
         --  would really rather use the root type, so that if people want to
         --  fiddle with System.Stream_Attributes to get inter-target portable
         --  streams, they get the size they expect. Consider in particular the
         --  case of a stream written on an x86, with 96-bit Long_Long_Float
         --  being read into a non-x86 target with 64 bit Long_Long_Float. A
         --  special version of System.Stream_Attributes can deal with this
         --  provided the proper type is always used.

         --  To deal with these two requirements we add the special checks
         --  on equal sizes and use the root type to distinguish.

         if P_Size <= Standard_Short_Float_Size
           and then (Standard_Short_Float_Size /= Standard_Float_Size
                      or else Rt_Type = Standard_Short_Float)
         then
            Lib_RE := RE_W_SF;

         elsif P_Size <= Standard_Float_Size then
            Lib_RE := RE_W_F;

         elsif P_Size <= Standard_Long_Float_Size
           and then (Standard_Long_Float_Size /= Standard_Long_Long_Float_Size
                      or else Rt_Type = Standard_Long_Float)
         then
            Lib_RE := RE_W_LF;

         else
            Lib_RE := RE_W_LLF;
         end if;

      --  Signed integer types. Also includes signed fixed-point types and
      --  signed enumeration types share this circuitry.

      --  Note on signed integer types. We do not consider types as signed for
      --  this purpose if they have no negative numbers, or if they have biased
      --  representation. The reason is that the value in either case basically
      --  represents an unsigned value.

      --  For example, consider:

      --     type W is range 0 .. 2**32 - 1;
      --     for W'Size use 32;

      --  This is a signed type, but the representation is unsigned, and may
      --  be outside the range of a 32-bit signed integer, so this must be
      --  treated as 32-bit unsigned.

      --  Similarly, the representation is also unsigned if we have:

      --     type W is range -1 .. +254;
      --     for W'Size use 8;

      --  forcing a biased and unsigned representation

      elsif not Is_Unsigned_Type (FST)
        and then
          (Is_Fixed_Point_Type (U_Type)
             or else
           Is_Enumeration_Type (U_Type)
             or else
           (Is_Signed_Integer_Type (U_Type)
              and then not Has_Biased_Representation (FST)))
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_W_SSI;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_W_SI;

         elsif P_Size = 24 then
            Lib_RE := RE_W_I24;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_W_I;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_W_LI;

         elsif P_Size <= Standard_Long_Long_Integer_Size then
            Lib_RE := RE_W_LLI;

         else
            Lib_RE := RE_W_LLLI;
         end if;

      --  Unsigned integer types, also includes unsigned fixed-point types
      --  and unsigned enumeration types (note we know they are unsigned
      --  because we already tested for signed above).

      --  Also includes signed integer types that are unsigned in the sense
      --  that they do not include negative numbers. See above for details.

      elsif Is_Modular_Integer_Type    (U_Type)
        or else Is_Fixed_Point_Type    (U_Type)
        or else Is_Enumeration_Type    (U_Type)
        or else Is_Signed_Integer_Type (U_Type)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_W_SSU;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_W_SU;

         elsif P_Size = 24 then
            Lib_RE := RE_W_U24;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_W_U;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_W_LU;

         elsif P_Size <= Standard_Long_Long_Integer_Size then
            Lib_RE := RE_W_LLU;

         else
            Lib_RE := RE_W_LLLU;
         end if;

      else pragma Assert (Is_Access_Type (U_Type));

         if Present (P_Size) and then P_Size > System_Address_Size then
            Lib_RE := RE_W_AD;
         else
            Lib_RE := RE_W_AS;
         end if;
      end if;

      --  Unchecked-convert parameter to the required type (i.e. the type of
      --  the corresponding parameter, and call the appropriate routine.

      Libent := RTE (Lib_RE);

      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Libent, Loc),
          Parameter_Associations => New_List (
            Relocate_Node (Strm),
            Unchecked_Convert_To (Etype (Next_Formal (First_Formal (Libent))),
              Relocate_Node (Item))));
   end Build_Elementary_Write_Call;

   -----------------------------------------
   -- Build_Mutable_Record_Read_Procedure --
   -----------------------------------------

   procedure Build_Mutable_Record_Read_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);

      Out_Formal : Node_Id;
      --  Expression denoting the out formal parameter

      Dcls : constant List_Id := New_List;
      --  Declarations for the 'Read body

      Stms : constant List_Id := New_List;
      --  Statements for the 'Read body

      Disc : Entity_Id;
      --  Entity of the discriminant being processed

      Tmp_For_Disc : Entity_Id;
      --  Temporary object used to read the value of Disc

      Tmps_For_Discs : constant List_Id := New_List;
      --  List of object declarations for temporaries holding the read values
      --  for the discriminants.

      Cstr : constant List_Id := New_List;
      --  List of constraints to be applied on temporary record

      Discriminant_Checks : constant List_Id := New_List;
      --  List of discriminant checks to be performed if the actual object
      --  is constrained.

      Tmp : constant Entity_Id := Make_Defining_Identifier (Loc, Name_V);
      --  Temporary record must hide formal (assignments to components of the
      --  record are always generated with V as the identifier for the record).

      Constrained_Stms : List_Id := New_List;
      --  Statements within the block where we have the constrained temporary

   begin
      --  A mutable type cannot be a tagged type, so we generate a new name
      --  for the stream procedure.

      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Stream_Read));

      if Is_Unchecked_Union (Typ) then

         --  If this is an unchecked union, the stream procedure is erroneous,
         --  because there are no discriminants to read.

         --  This should generate a warning ???

         Append_To (Stms,
           Make_Raise_Program_Error (Loc,
             Reason => PE_Unchecked_Union_Restriction));

         Build_Stream_Procedure (Typ, Decl, Pnam, Stms, Outp => True);
         return;
      end if;

      Disc := First_Discriminant (Typ);

      Out_Formal :=
        Make_Selected_Component (Loc,
          Prefix        => New_Occurrence_Of (Pnam, Loc),
          Selector_Name => Make_Identifier (Loc, Name_V));

      --  Generate Reads for the discriminants of the type. The discriminants
      --  need to be read before the rest of the components, so that variants
      --  are initialized correctly. The discriminants must be read into temp
      --  variables so an incomplete Read (interrupted by an exception, for
      --  example) does not alter the passed object.

      while Present (Disc) loop
         Tmp_For_Disc := Make_Defining_Identifier (Loc,
                           New_External_Name (Chars (Disc), "D"));

         Append_To (Tmps_For_Discs,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tmp_For_Disc,
             Object_Definition   => New_Occurrence_Of (Etype (Disc), Loc)));
         Set_No_Initialization (Last (Tmps_For_Discs));

         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Etype (Disc), Loc),
             Attribute_Name => Name_Read,
             Expressions    => New_List (
               Make_Identifier (Loc, Name_S),
               New_Occurrence_Of (Tmp_For_Disc, Loc))));

         Append_To (Cstr,
           Make_Discriminant_Association (Loc,
             Selector_Names => New_List (New_Occurrence_Of (Disc, Loc)),
             Expression     => New_Occurrence_Of (Tmp_For_Disc, Loc)));

         Append_To (Discriminant_Checks,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => New_Occurrence_Of (Tmp_For_Disc, Loc),
                 Right_Opnd =>
                   Make_Selected_Component (Loc,
                     Prefix        => New_Copy_Tree (Out_Formal),
                     Selector_Name => New_Occurrence_Of (Disc, Loc))),
             Reason => CE_Discriminant_Check_Failed));
         Next_Discriminant (Disc);
      end loop;

      --  Generate reads for the components of the record (including those
      --  that depend on discriminants).

      Build_Record_Read_Write_Procedure (Typ, Decl, Pnam, Name_Read);

      --  Save original statement sequence for component assignments, and
      --  replace it with Stms.

      Constrained_Stms := Statements (Handled_Statement_Sequence (Decl));
      Set_Handled_Statement_Sequence (Decl,
        Make_Handled_Sequence_Of_Statements (Loc,
          Statements => Stms));

      --  If Typ has controlled components (i.e. if it is classwide or
      --  Has_Controlled), or components constrained using the discriminants
      --  of Typ, then we need to ensure that all component assignments are
      --  performed on an object that has been appropriately constrained
      --  prior to being initialized. To this effect, we wrap the component
      --  assignments in a block where V is a constrained temporary.

      Append_To (Dcls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Tmp,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Occurrence_Of (Base_Type (Typ), Loc),
              Constraint   =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => Cstr))));

      --  AI05-023-1: Insert discriminant check prior to initialization of the
      --  constrained temporary.

      Append_To (Stms,
        Make_Implicit_If_Statement (Pnam,
          Condition =>
            Make_Attribute_Reference (Loc,
              Prefix         => New_Copy_Tree (Out_Formal),
              Attribute_Name => Name_Constrained),
          Then_Statements => Discriminant_Checks));

      --  Now insert back original component assignments, wrapped in a block
      --  in which V is the constrained temporary.

      Append_To (Stms,
        Make_Block_Statement (Loc,
          Declarations               => Dcls,
          Handled_Statement_Sequence => Parent (Constrained_Stms)));

      Append_To (Constrained_Stms,
        Make_Assignment_Statement (Loc,
          Name       => Out_Formal,
          Expression => Make_Identifier (Loc, Name_V)));

      Set_Declarations (Decl, Tmps_For_Discs);
   end Build_Mutable_Record_Read_Procedure;

   ------------------------------------------
   -- Build_Mutable_Record_Write_Procedure --
   ------------------------------------------

   procedure Build_Mutable_Record_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc   : constant Source_Ptr := Sloc (Typ);
      Stms  : List_Id;
      Disc  : Entity_Id;
      D_Ref : Node_Id;

   begin
      Stms := New_List;
      Disc := First_Discriminant (Typ);

      --  Generate Writes for the discriminants of the type
      --  If the type is an unchecked union, use the default values of
      --  the discriminants, because they are not stored.

      while Present (Disc) loop
         if Is_Unchecked_Union (Typ) then
            D_Ref :=
               New_Copy_Tree (Discriminant_Default_Value (Disc));
         else
            D_Ref :=
              Make_Selected_Component (Loc,
                Prefix        => Make_Identifier (Loc, Name_V),
                Selector_Name => New_Occurrence_Of (Disc, Loc));
         end if;

         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Etype (Disc), Loc),
               Attribute_Name => Name_Write,
               Expressions    => New_List (
                 Make_Identifier (Loc, Name_S),
                 D_Ref)));

         Next_Discriminant (Disc);
      end loop;

      --  A mutable type cannot be a tagged type, so we generate a new name
      --  for the stream procedure.

      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Stream_Write));
      Build_Record_Read_Write_Procedure (Typ, Decl, Pnam, Name_Write);

      --  Write the discriminants before the rest of the components, so
      --  that discriminant values are properly set of variants, etc.

      if Is_Non_Empty_List (
        Statements (Handled_Statement_Sequence (Decl)))
      then
         Insert_List_Before
            (First (Statements (Handled_Statement_Sequence (Decl))), Stms);
      else
         Set_Statements (Handled_Statement_Sequence (Decl), Stms);
      end if;
   end Build_Mutable_Record_Write_Procedure;

   -----------------------------------------------
   -- Build_Record_Or_Elementary_Input_Function --
   -----------------------------------------------

   --  The function we build looks like

   --    function InputN (S : access RST) return Typ is
   --      C1 : constant Disc_Type_1;
   --      Discr_Type_1'Read (S, C1);
   --      C2 : constant Disc_Type_2;
   --      Discr_Type_2'Read (S, C2);
   --      ...
   --      Cn : constant Disc_Type_n;
   --      Discr_Type_n'Read (S, Cn);
   --      V : Typ (C1, C2, .. Cn)

   --    begin
   --      Typ'Read (S, V);
   --      return V;
   --    end InputN

   --  The discriminants are of course only present in the case of a record
   --  with discriminants. In the case of a record with no discriminants, or
   --  an elementary type, then no Cn constants are defined.

   procedure Build_Record_Or_Elementary_Input_Function
     (Typ            : Entity_Id;
      Decl           : out Node_Id;
      Fnam           : out Entity_Id)
   is
      Loc        : constant Source_Ptr := Sloc (Typ);
      B_Typ      : constant Entity_Id := Underlying_Type (Base_Type (Typ));
      Cn         : Name_Id;
      Constr     : List_Id;
      Decls      : List_Id;
      Discr      : Entity_Id;
      Discr_Elmt : Elmt_Id            := No_Elmt;
      J          : Pos;
      Obj_Decl   : Node_Id;
      Odef       : Node_Id;
      Stms       : List_Id;

   begin
      Decls  := New_List;
      Constr := New_List;

      J := 1;

      --  In the presence of multiple instantiations (as in uses of the Booch
      --  components) the base type may be private, and the underlying type
      --  already constrained, in which case there's no discriminant constraint
      --  to construct.

      if Has_Discriminants (Typ)
        and then No (Discriminant_Default_Value (First_Discriminant (Typ)))
        and then not Is_Constrained (Underlying_Type (B_Typ))
      then
         Discr := First_Discriminant (B_Typ);

         --  If the prefix subtype is constrained, then retrieve the first
         --  element of its constraint.

         if Is_Constrained (Typ) then
            Discr_Elmt := First_Elmt (Discriminant_Constraint (Typ));
         end if;

         while Present (Discr) loop
            Cn := New_External_Name ('C', J);

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Make_Defining_Identifier (Loc, Cn),
                Object_Definition   =>
                  New_Occurrence_Of (Etype (Discr), Loc));

            --  If this is an access discriminant, do not perform default
            --  initialization. The discriminant is about to get its value
            --  from Read, and if the type is null excluding we do not want
            --  spurious warnings on an initial null value.

            if Is_Access_Type (Etype (Discr)) then
               Set_No_Initialization (Decl);
            end if;

            Append_To (Decls, Decl);
            Append_To (Decls,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Etype (Discr), Loc),
                Attribute_Name => Name_Read,
                Expressions    => New_List (
                  Make_Identifier (Loc, Name_S),
                  Make_Identifier (Loc, Cn))));

            Append_To (Constr, Make_Identifier (Loc, Cn));

            --  If the prefix subtype imposes a discriminant constraint, then
            --  check that each discriminant value equals the value read.

            if Present (Discr_Elmt) then
               Append_To (Decls,
                 Make_Raise_Constraint_Error (Loc,
                   Condition => Make_Op_Ne (Loc,
                                  Left_Opnd  =>
                                    New_Occurrence_Of
                                      (Defining_Identifier (Decl), Loc),
                                  Right_Opnd =>
                                    New_Copy_Tree (Node (Discr_Elmt))),
                   Reason    => CE_Discriminant_Check_Failed));

               Next_Elmt (Discr_Elmt);
            end if;

            Next_Discriminant (Discr);
            J := J + 1;
         end loop;

         Odef :=
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Occurrence_Of (B_Typ, Loc),
             Constraint   =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => Constr));

      --  If no discriminants, then just use the type with no constraint

      else
         Odef := New_Occurrence_Of (B_Typ, Loc);
      end if;

      --  Create an extended return statement encapsulating the result object
      --  and 'Read call, which is needed in general for proper handling of
      --  build-in-place results (such as when the result type is inherently
      --  limited).

      Obj_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          Object_Definition => Odef);

      --  If the type is an access type, do not perform default initialization.
      --  The object is about to get its value from Read, and if the type is
      --  null excluding we do not want spurious warnings on an initial null.

      if Is_Access_Type (B_Typ) then
         Set_No_Initialization (Obj_Decl);
      end if;

      Stms := New_List (
        Make_Extended_Return_Statement (Loc,
          Return_Object_Declarations => New_List (Obj_Decl),
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Attribute_Reference (Loc,
                  Prefix         => New_Occurrence_Of (B_Typ, Loc),
                  Attribute_Name => Name_Read,
                  Expressions    => New_List (
                    Make_Identifier (Loc, Name_S),
                    Make_Identifier (Loc, Name_V)))))));

      Fnam := Make_Stream_Subprogram_Name (Loc, B_Typ, TSS_Stream_Input);

      Build_Stream_Function (B_Typ, Decl, Fnam, Decls, Stms);
   end Build_Record_Or_Elementary_Input_Function;

   -------------------------------------------------
   -- Build_Record_Or_Elementary_Output_Procedure --
   -------------------------------------------------

   procedure Build_Record_Or_Elementary_Output_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc      : constant Source_Ptr := Sloc (Typ);
      Stms     : List_Id;
      Disc     : Entity_Id;
      Disc_Ref : Node_Id;

   begin
      Stms := New_List;

      --  Note that of course there will be no discriminants for the elementary
      --  type case, so Has_Discriminants will be False. Note that the language
      --  rules do not allow writing the discriminants in the defaulted case,
      --  because those are written by 'Write.

      if Has_Discriminants (Typ)
        and then No (Discriminant_Default_Value (First_Discriminant (Typ)))
      then
         Disc := First_Discriminant (Typ);
         while Present (Disc) loop

            --  If the type is an unchecked union, it must have default
            --  discriminants (this is checked earlier), and those defaults
            --  are written out to the stream.

            if Is_Unchecked_Union (Typ) then
               Disc_Ref := New_Copy_Tree (Discriminant_Default_Value (Disc));

            else
               Disc_Ref :=
                 Make_Selected_Component (Loc,
                   Prefix        => Make_Identifier (Loc, Name_V),
                   Selector_Name => New_Occurrence_Of (Disc, Loc));
            end if;

            Append_To (Stms,
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (Stream_Base_Type (Etype (Disc)), Loc),
                Attribute_Name => Name_Write,
                Expressions    => New_List (
                  Make_Identifier (Loc, Name_S),
                  Disc_Ref)));

            Next_Discriminant (Disc);
         end loop;
      end if;

      Append_To (Stms,
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Typ, Loc),
          Attribute_Name => Name_Write,
          Expressions    => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Identifier (Loc, Name_V))));

      Pnam := Make_Stream_Subprogram_Name (Loc, Typ, TSS_Stream_Output);

      Build_Stream_Procedure (Typ, Decl, Pnam, Stms, Outp => False);
   end Build_Record_Or_Elementary_Output_Procedure;

   ---------------------------------
   -- Build_Record_Read_Procedure --
   ---------------------------------

   procedure Build_Record_Read_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Typ);
   begin
      Pnam := Make_Stream_Subprogram_Name (Loc, Typ, TSS_Stream_Read);
      Build_Record_Read_Write_Procedure (Typ, Decl, Pnam, Name_Read);
   end Build_Record_Read_Procedure;

   ---------------------------------------
   -- Build_Record_Read_Write_Procedure --
   ---------------------------------------

   --  The form of the record read/write procedure is as shown by the
   --  following example for a case with one discriminant case variant:

   --    procedure pnam (S : access RST, V : [out] Typ) is
   --    begin
   --       Component_Type'Read/Write (S, V.component);
   --       Component_Type'Read/Write (S, V.component);
   --       ...
   --       Component_Type'Read/Write (S, V.component);
   --
   --       case V.discriminant is
   --          when choices =>
   --             Component_Type'Read/Write (S, V.component);
   --             Component_Type'Read/Write (S, V.component);
   --             ...
   --             Component_Type'Read/Write (S, V.component);
   --
   --          when choices =>
   --             Component_Type'Read/Write (S, V.component);
   --             Component_Type'Read/Write (S, V.component);
   --             ...
   --             Component_Type'Read/Write (S, V.component);
   --          ...
   --       end case;
   --    end pnam;

   --  The out keyword for V is supplied in the Read case

   procedure Build_Record_Read_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);
      Rdef : Node_Id;
      Stms : List_Id;
      Typt : Entity_Id;

      In_Limited_Extension : Boolean := False;
      --  Set to True while processing the record extension definition
      --  for an extension of a limited type (for which an ancestor type
      --  has an explicit Nam attribute definition).

      function Make_Component_List_Attributes (CL : Node_Id) return List_Id;
      --  Returns a sequence of attributes to process the components that
      --  are referenced in the given component list.

      function Make_Field_Attribute (C : Entity_Id) return Node_Id;
      --  Given C, the entity for a discriminant or component, build
      --  an attribute for the corresponding field values.

      function Make_Field_Attributes (Clist : List_Id) return List_Id;
      --  Given Clist, a component items list, construct series of attributes
      --  for fieldwise processing of the corresponding components.

      ------------------------------------
      -- Make_Component_List_Attributes --
      ------------------------------------

      function Make_Component_List_Attributes (CL : Node_Id) return List_Id is
         CI : constant List_Id := Component_Items (CL);
         VP : constant Node_Id := Variant_Part (CL);

         Result : List_Id;
         Alts   : List_Id;
         V      : Node_Id;
         DC     : Node_Id;
         DCH    : List_Id;
         D_Ref  : Node_Id;

      begin
         Result := Make_Field_Attributes (CI);

         if Present (VP) then
            Alts := New_List;

            V := First_Non_Pragma (Variants (VP));
            while Present (V) loop
               DCH := New_List;

               DC := First (Discrete_Choices (V));
               while Present (DC) loop
                  Append_To (DCH, New_Copy_Tree (DC));
                  Next (DC);
               end loop;

               Append_To (Alts,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => DCH,
                   Statements =>
                     Make_Component_List_Attributes (Component_List (V))));
               Next_Non_Pragma (V);
            end loop;

            --  Note: in the following, we make sure that we use new occurrence
            --  of for the selector, since there are cases in which we make a
            --  reference to a hidden discriminant that is not visible.

            --  If the enclosing record is an unchecked_union, we use the
            --  default expressions for the discriminant (it must exist)
            --  because we cannot generate a reference to it, given that
            --  it is not stored.

            if Is_Unchecked_Union (Scope (Entity (Name (VP)))) then
               D_Ref :=
                 New_Copy_Tree
                   (Discriminant_Default_Value (Entity (Name (VP))));
            else
               D_Ref :=
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Name_V),
                    Selector_Name =>
                      New_Occurrence_Of (Entity (Name (VP)), Loc));
            end if;

            Append_To (Result,
              Make_Case_Statement (Loc,
                Expression   => D_Ref,
                Alternatives => Alts));
         end if;

         return Result;
      end Make_Component_List_Attributes;

      --------------------------
      -- Make_Field_Attribute --
      --------------------------

      function Make_Field_Attribute (C : Entity_Id) return Node_Id is
         Field_Typ : constant Entity_Id := Stream_Base_Type (Etype (C));
         Selected  : Node_Id;

         TSS_Names : constant array (Name_Input .. Name_Write) of
                       TSS_Name_Type :=
                        (Name_Read   => TSS_Stream_Read,
                         Name_Write  => TSS_Stream_Write,
                         Name_Input  => TSS_Stream_Input,
                         Name_Output => TSS_Stream_Output,
                         others      => TSS_Null);
         pragma Assert (TSS_Names (Nam) /= TSS_Null);

      begin
         if In_Limited_Extension
           and then Is_Limited_Type (Field_Typ)
           and then No (Find_Inherited_TSS (Field_Typ, TSS_Names (Nam)))
         then
            --  The declaration is illegal per 13.13.2(9/1), and this is
            --  enforced in Exp_Ch3.Check_Stream_Attributes. Keep the caller
            --  happy by returning a null statement.

            return Make_Null_Statement (Loc);
         end if;

         Selected := Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Name_V),
                       Selector_Name => New_Occurrence_Of (C, Loc));

         if Is_Mutably_Tagged_CW_Equivalent_Type (Etype (C)) then
            Make_Mutably_Tagged_Conversion
              (Selected,
               Typ => Get_Corresponding_Mutably_Tagged_Type_If_Present
                        (Etype (C)));
         end if;

         return
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Field_Typ, Loc),
             Attribute_Name => Nam,
             Expressions    => New_List (Make_Identifier (Loc, Name_S),
                                         Selected));
      end Make_Field_Attribute;

      ---------------------------
      -- Make_Field_Attributes --
      ---------------------------

      function Make_Field_Attributes (Clist : List_Id) return List_Id is
         Item   : Node_Id;
         Result : constant List_Id := New_List;

      begin
         --  Loop through components, skipping all internal components, which
         --  are not part of the value (e.g. _Tag), except that we don't skip
         --  the _Parent, since we do want to process that recursively. If
         --  _Parent is an interface type, being abstract with no components
         --  there is no need to handle it.

         Item := First (Clist);
         while Present (Item) loop
            if Nkind (Item) = N_Component_Declaration
              and then
                ((Chars (Defining_Identifier (Item)) = Name_uParent
                 and then not Is_Interface
                   (Etype (Defining_Identifier (Item))))
                or else
                  not Is_Internal_Name (Chars (Defining_Identifier (Item))))
            then
               Append_To
                 (Result,
                  Make_Field_Attribute (Defining_Identifier (Item)));
            end if;

            Next (Item);
         end loop;

         return Result;
      end Make_Field_Attributes;

   --  Start of processing for Build_Record_Read_Write_Procedure

   begin
      --  For the protected type case, use corresponding record

      if Is_Protected_Type (Typ) then
         Typt := Corresponding_Record_Type (Typ);
      else
         Typt := Typ;
      end if;

      --  Note that we do nothing with the discriminants, since Read and
      --  Write do not read or write the discriminant values. All handling
      --  of discriminants occurs in the Input and Output subprograms.

      Rdef := Type_Definition
                (Declaration_Node (Base_Type (Underlying_Type (Typt))));
      Stms := Empty_List;

      --  In record extension case, the fields we want, including the _Parent
      --  field representing the parent type, are to be found in the extension.
      --  Note that we will naturally process the _Parent field using the type
      --  of the parent, and hence its stream attributes, which is appropriate.

      if Nkind (Rdef) = N_Derived_Type_Definition then
         Rdef := Record_Extension_Part (Rdef);

         if Is_Limited_Type (Typt) then
            In_Limited_Extension := True;
         end if;
      end if;

      if Present (Component_List (Rdef)) then
         Append_List_To (Stms,
           Make_Component_List_Attributes (Component_List (Rdef)));
      end if;

      Build_Stream_Procedure
        (Typ, Decl, Pnam, Stms, Outp => Nam = Name_Read);
   end Build_Record_Read_Write_Procedure;

   ----------------------------------
   -- Build_Record_Write_Procedure --
   ----------------------------------

   procedure Build_Record_Write_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Typ);
   begin
      Pnam := Make_Stream_Subprogram_Name (Loc, Typ, TSS_Stream_Write);
      Build_Record_Read_Write_Procedure (Typ, Decl, Pnam, Name_Write);
   end Build_Record_Write_Procedure;

   -------------------------------
   -- Build_Stream_Attr_Profile --
   -------------------------------

   function Build_Stream_Attr_Profile
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : TSS_Name_Type) return List_Id
   is
      Profile : List_Id;

   begin
      --  (Ada 2005: AI-441): Set the null-excluding attribute because it has
      --  no semantic meaning in Ada 95 but it is a requirement in Ada 2005.

      Profile := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_S),
          Parameter_Type      =>
          Make_Access_Definition (Loc,
             Null_Exclusion_Present => True,
             Subtype_Mark => New_Occurrence_Of (
               Class_Wide_Type (RTE (RE_Root_Stream_Type)), Loc))));

      if Nam /= TSS_Stream_Input then
         Append_To (Profile,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Out_Present         => (Nam = TSS_Stream_Read),
             Parameter_Type      => New_Occurrence_Of (Typ, Loc)));
      end if;

      return Profile;
   end Build_Stream_Attr_Profile;

   ---------------------------
   -- Build_Stream_Function --
   ---------------------------

   procedure Build_Stream_Function
     (Typ   : Entity_Id;
      Decl  : out Node_Id;
      Fnam  : Entity_Id;
      Decls : List_Id;
      Stms  : List_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);
      Spec : Node_Id;

   begin
      --  Construct function specification

      --  (Ada 2005: AI-441): Set the null-excluding attribute because it has
      --  no semantic meaning in Ada 95 but it is a requirement in Ada 2005.

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,

          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, Name_S),
              Parameter_Type      =>
                Make_Access_Definition (Loc,
                  Null_Exclusion_Present => True,
                  Subtype_Mark           =>
                    New_Occurrence_Of
                      (Class_Wide_Type (RTE (RE_Root_Stream_Type)), Loc)))),

          Result_Definition => New_Occurrence_Of (Typ, Loc));

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification              => Spec,
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_Stream_Function;

   ----------------------------
   -- Build_Stream_Procedure --
   ----------------------------

   procedure Build_Stream_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Stms : List_Id;
      Outp : Boolean)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);
      Spec : Node_Id;

   begin
      --  Construct procedure specification

      --  (Ada 2005: AI-441): Set the null-excluding attribute because it has
      --  no semantic meaning in Ada 95 but it is a requirement in Ada 2005.

      Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => Pnam,

          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, Name_S),
              Parameter_Type      =>
                Make_Access_Definition (Loc,
                  Null_Exclusion_Present => True,
                  Subtype_Mark           =>
                    New_Occurrence_Of
                      (Class_Wide_Type (RTE (RE_Root_Stream_Type)), Loc))),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
              Out_Present         => Outp,
              Parameter_Type      => New_Occurrence_Of (Typ, Loc))));

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification              => Spec,
          Declarations               => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_Stream_Procedure;

   -----------------------------
   -- Has_Stream_Standard_Rep --
   -----------------------------

   function Has_Stream_Standard_Rep (U_Type : Entity_Id) return Boolean is
      Siz : Uint;

   begin
      if Has_Non_Standard_Rep (U_Type) then
         return False;
      end if;

      if Has_Stream_Size_Clause (U_Type) then
         Siz := Static_Integer (Expression (Stream_Size_Clause (U_Type)));
      else
         Siz := Esize (First_Subtype (U_Type));
      end if;

      return Siz = Esize (Root_Type (U_Type));
   end Has_Stream_Standard_Rep;

   ---------------------------------
   -- Make_Stream_Subprogram_Name --
   ---------------------------------

   function Make_Stream_Subprogram_Name
     (Loc : Source_Ptr;
      Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id
   is
      Sname : Name_Id;

   begin
      --  For tagged types, we are dealing with a TSS associated with the
      --  declaration, so we use the standard primitive function name. For
      --  other types, generate a local TSS name since we are generating
      --  the subprogram at the point of use.

      if Is_Tagged_Type (Typ) then
         Sname := Make_TSS_Name (Typ, Nam);
      else
         Sname := Make_TSS_Name_Local (Typ, Nam);
      end if;

      return Make_Defining_Identifier (Loc, Sname);
   end Make_Stream_Subprogram_Name;

   ----------------------
   -- Stream_Base_Type --
   ----------------------

   function Stream_Base_Type (E : Entity_Id) return Entity_Id is
   begin
      if Is_Class_Wide_Equivalent_Type (E) then
         return Corresponding_Mutably_Tagged_Type (E);
      end if;

      if Is_Array_Type (E)
        and then Is_First_Subtype (E)
      then
         return E;
      else
         return Base_Type (E);
      end if;
   end Stream_Base_Type;

end Exp_Strm;
