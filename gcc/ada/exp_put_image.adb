------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        E X P _ P U T _ I M A G E                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2020-2025, Free Software Foundation, Inc.      --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Csets;          use Csets;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Lib;            use Lib;
with Mutably_Tagged; use Mutably_Tagged;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Output;         use Output;
with Rtsfind;        use Rtsfind;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;

package body Exp_Put_Image is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Put_Image_Proc
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Stms : List_Id);
   --  Build an array or record Put_Image procedure. Stms is the list of
   --  statements for the body and Pnam is the name of the constructed
   --  procedure. (The declaration list is always null.)

   function Make_Put_Image_Name
     (Loc : Source_Ptr; Typ : Entity_Id) return Entity_Id;
   --  Return the entity that identifies the Put_Image subprogram for Typ. This
   --  procedure deals with the difference between tagged types (where a single
   --  subprogram associated with the type is generated) and all other cases
   --  (where a subprogram is generated at the point of the attribute
   --  reference). The Loc parameter is used as the Sloc of the created entity.

   function Put_Image_Base_Type (E : Entity_Id) return Entity_Id;
   --  Returns the base type, except for an array type whose whose first
   --  subtype is constrained, in which case it returns the first subtype.

   -------------------------------------
   -- Build_Array_Put_Image_Procedure --
   -------------------------------------

   procedure Build_Array_Put_Image_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);

      function Wrap_In_Loop
        (Stms : List_Id;
         Dim : Pos;
         Index_Subtype : Entity_Id;
         Between_Proc : RE_Id) return Node_Id;
      --  Wrap Stms in a loop and if statement of the form:
      --
      --     if V'First (Dim) <= V'Last (Dim) then -- nonempty range?
      --        declare
      --           LDim : Index_Type_For_Dim := V'First (Dim);
      --        begin
      --           loop
      --              Stms;
      --              exit when LDim = V'Last (Dim);
      --              Between_Proc (S);
      --              LDim := Index_Type_For_Dim'Succ (LDim);
      --           end loop;
      --        end;
      --     end if;
      --
      --  This is called once per dimension, from inner to outer.

      function Wrap_In_Loop
        (Stms : List_Id;
         Dim : Pos;
         Index_Subtype : Entity_Id;
         Between_Proc : RE_Id) return Node_Id
      is
         Index : constant Entity_Id :=
           Make_Defining_Identifier
             (Loc, Chars => New_External_Name ('L', Dim));
         Decl : constant Node_Id :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Index,
             Object_Definition =>
               New_Occurrence_Of (Index_Subtype, Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix         => Make_Identifier (Loc, Name_V),
                 Attribute_Name => Name_First,
                 Expressions => New_List (
                   Make_Integer_Literal (Loc, Dim))));
         Loop_Stm : constant Node_Id :=
           Make_Implicit_Loop_Statement (Typ, Statements => Stms);
         Exit_Stm : constant Node_Id :=
           Make_Exit_Statement (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd => New_Occurrence_Of (Index, Loc),
                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix         =>
                       Make_Identifier (Loc, Name_V),
                     Attribute_Name => Name_Last,
                     Expressions => New_List (
                       Make_Integer_Literal (Loc, Dim)))));
         Increment : constant Node_Id :=
           Make_Increment (Loc, Index, Index_Subtype);
         Between : constant Node_Id :=
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (Between_Proc), Loc),
             Parameter_Associations => New_List
               (Make_Identifier (Loc, Name_S)));
         Block : constant Node_Id :=
           Make_Block_Statement (Loc,
             Declarations               => New_List (Decl),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Loop_Stm)));
      begin
         Append_To (Stms, Exit_Stm);
         Append_To (Stms, Between);
         Append_To (Stms, Increment);
         --  Note that we're appending to the Stms list passed in

         return
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Le (Loc,
                 Left_Opnd  =>
                   Make_Attribute_Reference (Loc,
                     Prefix => Make_Identifier (Loc, Name_V),
                     Attribute_Name => Name_First,
                     Expressions => New_List (
                       Make_Integer_Literal (Loc, Dim))),
                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => Make_Identifier (Loc, Name_V),
                     Attribute_Name => Name_Last,
                     Expressions => New_List (
                       Make_Integer_Literal (Loc, Dim)))),
             Then_Statements => New_List (Block));
      end Wrap_In_Loop;

      Ndim : constant Pos        := Number_Dimensions (Typ);
      Ctyp : constant Entity_Id  := Component_Type (Typ);

      Stm         : Node_Id;
      Exl         : constant List_Id := New_List;
      PI_Entity   : Entity_Id;

      Indices : array (1 .. Ndim) of Entity_Id;

   --  Start of processing for Build_Array_Put_Image_Procedure

   begin
      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars => Make_TSS_Name_Local (Typ, TSS_Put_Image));

      --  Get the Indices

      declare
         Index_Subtype : Node_Id := First_Index (Typ);
      begin
         for Dim in 1 .. Ndim loop
            Indices (Dim) := Etype (Index_Subtype);
            Next_Index (Index_Subtype);
         end loop;
         pragma Assert (No (Index_Subtype));
      end;

      --  Build the inner attribute call

      for Dim in 1 .. Ndim loop
         Append_To (Exl, Make_Identifier (Loc, New_External_Name ('L', Dim)));
      end loop;

      Stm :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Put_Image_Base_Type (Ctyp), Loc),
          Attribute_Name => Name_Put_Image,
          Expressions => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Indexed_Component (Loc,
              Prefix      => Make_Identifier (Loc, Name_V),
              Expressions => Exl)));

      --  The corresponding attribute for the component type of the array might
      --  be user-defined, and frozen after the array type. In that case,
      --  freeze the Put_Image attribute of the component type, whose
      --  declaration could not generate any additional freezing actions in any
      --  case.

      PI_Entity := TSS (Base_Type (Ctyp), TSS_Put_Image);

      if Present (PI_Entity) and then not Is_Frozen (PI_Entity) then
         Set_Is_Frozen (PI_Entity);
      end if;

      --  Loop through the dimensions, innermost first, generating a loop for
      --  each dimension.

      declare
         Stms : List_Id := New_List (Stm);
      begin
         for Dim in reverse 1 .. Ndim loop
            declare
               New_Stms : constant List_Id := New_List;
               Between_Proc : RE_Id;
            begin
               --  For a one-dimensional array of elementary type, use
               --  RE_Simple_Array_Between. The same applies to the last
               --  dimension of a multidimensional array.

               if Is_Elementary_Type (Ctyp) and then Dim = Ndim then
                  Between_Proc := RE_Simple_Array_Between;
               else
                  Between_Proc := RE_Array_Between;
               end if;

               Append_To (New_Stms,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Occurrence_Of (RTE (RE_Array_Before), Loc),
                   Parameter_Associations => New_List
                     (Make_Identifier (Loc, Name_S))));

               Append_To
                 (New_Stms,
                  Wrap_In_Loop (Stms, Dim, Indices (Dim), Between_Proc));

               Append_To (New_Stms,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Occurrence_Of (RTE (RE_Array_After), Loc),
                   Parameter_Associations => New_List
                     (Make_Identifier (Loc, Name_S))));

               Stms := New_Stms;
            end;
         end loop;

         Build_Put_Image_Proc (Loc, Typ, Decl, Pnam, Stms);
      end;
   end Build_Array_Put_Image_Procedure;

   -------------------------------------
   -- Build_Elementary_Put_Image_Call --
   -------------------------------------

   function Build_Elementary_Put_Image_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (Prefix (N));
      U_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      Sink    : constant Node_Id    := First (Expressions (N));
      Item    : constant Node_Id    := Next (Sink);
      P_Size  : constant Uint       := Esize (U_Type);
      Lib_RE  : RE_Id;

   begin
      if Is_Signed_Integer_Type (U_Type) then
         if P_Size <= Standard_Integer_Size then
            Lib_RE := RE_Put_Image_Integer;
         elsif P_Size <= Standard_Long_Long_Integer_Size then
            Lib_RE := RE_Put_Image_Long_Long_Integer;
         else
            pragma Assert (P_Size <= Standard_Long_Long_Long_Integer_Size);
            Lib_RE := RE_Put_Image_Long_Long_Long_Integer;
         end if;

      elsif Is_Modular_Integer_Type (U_Type) then
         if P_Size <= Standard_Integer_Size then -- Yes, Integer
            Lib_RE := RE_Put_Image_Unsigned;
         elsif P_Size <= Standard_Long_Long_Integer_Size then
            Lib_RE := RE_Put_Image_Long_Long_Unsigned;
         else
            pragma Assert (P_Size <= Standard_Long_Long_Long_Integer_Size);
            Lib_RE := RE_Put_Image_Long_Long_Long_Unsigned;
         end if;

      elsif Is_Access_Type (U_Type) then
         if Is_Access_Protected_Subprogram_Type (Base_Type (U_Type)) then
            Lib_RE := RE_Put_Image_Access_Prot_Subp;
         elsif Is_Access_Subprogram_Type (Base_Type (U_Type)) then
            Lib_RE := RE_Put_Image_Access_Subp;
         elsif P_Size = System_Address_Size then
            Lib_RE := RE_Put_Image_Thin_Pointer;
         else
            pragma Assert (P_Size = 2 * System_Address_Size);
            Lib_RE := RE_Put_Image_Fat_Pointer;
         end if;

      else
         pragma Assert
           (Is_Enumeration_Type (U_Type) or else Is_Real_Type (U_Type));

         --  For other elementary types, generate:
         --
         --     Wide_Wide_Put (Root_Buffer_Type'Class (Sink),
         --       U_Type'Wide_Wide_Image (Item));
         --
         --  It would be more elegant to do it the other way around (define
         --  '[[Wide_]Wide_]Image in terms of 'Put_Image). But this is easier
         --  to implement, because we already have support for
         --  'Wide_Wide_Image. Furthermore, we don't want to remove the
         --  existing support for '[[Wide_]Wide_]Image, because we don't
         --  currently plan to support 'Put_Image on restricted runtimes.

         --  We can't do this:
         --
         --     Put_UTF_8 (Sink, U_Type'Image (Item));
         --
         --  because we need to generate UTF-8, but 'Image for enumeration
         --  types uses the character encoding of the source file.
         --
         --  Note that this is putting a leading space for reals.

         declare
            Image : constant Node_Id :=
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (U_Type, Loc),
                Attribute_Name => Name_Wide_Wide_Image,
                Expressions => New_List (Relocate_Node (Item)));
            Sink_Exp : constant Node_Id :=
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of
                    (Class_Wide_Type (RTE (RE_Root_Buffer_Type)), Loc),
                Expression   => Relocate_Node (Sink));
            Put_Call : constant Node_Id :=
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Wide_Wide_Put), Loc),
                Parameter_Associations => New_List
                  (Sink_Exp, Image));
         begin
            --  We have built a dispatching call to handle calls to
            --  descendants (since they are not available through rtsfind).
            --  Further details available in the body of Put_String_Exp.

            return Put_Call;
         end;
      end if;

      --  Unchecked-convert parameter to the required type (i.e. the type of
      --  the corresponding parameter), and call the appropriate routine.
      --  We could use a normal type conversion for scalars, but the
      --  "unchecked" is needed for access and private types.

      declare
         Libent : constant Entity_Id := RTE (Lib_RE);
      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Libent, Loc),
             Parameter_Associations => New_List (
               Relocate_Node (Sink),
               Unchecked_Convert_To
                (Etype (Next_Formal (First_Formal (Libent))),
                 Relocate_Node (Item))));
      end;
   end Build_Elementary_Put_Image_Call;

   ---------------------------------
   -- Build_String_Put_Image_Call --
   ---------------------------------

   function Build_String_Put_Image_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (Prefix (N));
      U_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      R       : constant Entity_Id  := Root_Type (U_Type);
      Sink    : constant Node_Id    := First (Expressions (N));
      Item    : constant Node_Id    := Next (Sink);
      Lib_RE  : RE_Id;
      use Stand;
   begin
      pragma Assert (Is_String_Type (U_Type));
      pragma Assert (not RTU_Loaded (Interfaces_C)
        or else Enclosing_Lib_Unit_Entity (U_Type)
                  /= RTU_Entity (Interfaces_C));

      if R = Standard_String then
         Lib_RE := RE_Put_Image_String;
      elsif R = Standard_Wide_String then
         Lib_RE := RE_Put_Image_Wide_String;
      elsif R = Standard_Wide_Wide_String then
         Lib_RE := RE_Put_Image_Wide_Wide_String;

      else
         --  Handle custom string types. For example:

         --     type T is array (1 .. 10) of Character;
         --     Obj : T := (others => 'A');
         --     ...
         --     Put (Obj'Image);

         declare
            C_Type : Entity_Id;

         begin
            if Is_Private_Type (R) then
               C_Type := Component_Type (Full_View (R));
            else
               C_Type := Component_Type (R);
            end if;

            C_Type := Root_Type (Underlying_Type (C_Type));

            if C_Type = Standard_Character then
               Lib_RE := RE_Put_Image_String;
            elsif C_Type = Standard_Wide_Character then
               Lib_RE := RE_Put_Image_Wide_String;
            elsif C_Type = Standard_Wide_Wide_Character then
               Lib_RE := RE_Put_Image_Wide_Wide_String;
            else
               raise Program_Error;
            end if;
         end;
      end if;

      --  Convert parameter to the required type (i.e. the type of the
      --  corresponding parameter), and call the appropriate routine.
      --  We set the Conversion_OK flag in case the type is private.

      declare
         Libent : constant Entity_Id := RTE (Lib_RE);
         Conv   : constant Node_Id :=
           OK_Convert_To
            (Etype (Next_Formal (First_Formal (Libent))),
             Relocate_Node (Item));
      begin
         --  Do not output string delimiters if this is part of an
         --  interpolated string literal.

         if Nkind (Parent (N)) = N_Expression_With_Actions
           and then Nkind (Original_Node (Parent (N)))
                      = N_Interpolated_String_Literal
         then
            return
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Libent, Loc),
                Parameter_Associations => New_List (
                  Relocate_Node (Sink),
                  Conv,
                  New_Occurrence_Of (Stand.Standard_False, Loc)));
         else
            return
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Libent, Loc),
                Parameter_Associations => New_List (
                  Relocate_Node (Sink),
                  Conv));
         end if;
      end;
   end Build_String_Put_Image_Call;

   ------------------------------------
   -- Build_Protected_Put_Image_Call --
   ------------------------------------

   --  For "Protected_Type'Put_Image (S, Protected_Object)", build:
   --
   --    Put_Image_Protected (S);
   --
   --  The protected object is not passed.

   function Build_Protected_Put_Image_Call (N : Node_Id) return Node_Id is
      Loc    : constant Source_Ptr := Sloc (N);
      Sink   : constant Node_Id    := First (Expressions (N));
      Lib_RE : constant RE_Id      := RE_Put_Image_Protected;
      Libent : constant Entity_Id  := RTE (Lib_RE);
   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Libent, Loc),
          Parameter_Associations => New_List (
            Relocate_Node (Sink)));
   end Build_Protected_Put_Image_Call;

   -------------------------------
   -- Build_Task_Put_Image_Call --
   -------------------------------

   --  For "Task_Type'Put_Image (S, Task_Object)", build:
   --
   --    Put_Image_Task (S, Task_Object'Identity);
   --
   --  The task object is not passed; its Task_Id is.

   function Build_Task_Put_Image_Call (N : Node_Id) return Node_Id is
      Loc    : constant Source_Ptr := Sloc (N);
      Sink   : constant Node_Id    := First (Expressions (N));
      Item   : constant Node_Id    := Next (Sink);
      Lib_RE : constant RE_Id      := RE_Put_Image_Task;
      Libent : constant Entity_Id  := RTE (Lib_RE);

      Task_Id : constant Node_Id :=
        Make_Attribute_Reference (Loc,
          Prefix => Relocate_Node (Item),
          Attribute_Name => Name_Identity,
          Expressions => No_List);

   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Libent, Loc),
          Parameter_Associations => New_List (
            Relocate_Node (Sink),
            Task_Id));
   end Build_Task_Put_Image_Call;

   --------------------------------------
   -- Build_Record_Put_Image_Procedure --
   --------------------------------------

   --  The form of the record Put_Image procedure is as shown by the
   --  following example:

   --    procedure Put_Image (S : in out Sink'Class; V : Typ) is
   --    begin
   --       Component_Type'Put_Image (S, V.component);
   --       Component_Type'Put_Image (S, V.component);
   --       ...
   --       Component_Type'Put_Image (S, V.component);
   --
   --       case V.discriminant is
   --          when choices =>
   --             Component_Type'Put_Image (S, V.component);
   --             Component_Type'Put_Image (S, V.component);
   --             ...
   --             Component_Type'Put_Image (S, V.component);
   --
   --          when choices =>
   --             Component_Type'Put_Image (S, V.component);
   --             Component_Type'Put_Image (S, V.component);
   --             ...
   --             Component_Type'Put_Image (S, V.component);
   --          ...
   --       end case;
   --    end Put_Image;

   procedure Build_Record_Put_Image_Procedure
     (Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Typ);
      Btyp : constant Entity_Id := Base_Type (Typ);
      pragma Assert (not Is_Class_Wide_Type (Btyp));
      pragma Assert (not Is_Unchecked_Union (Btyp));

      First_Time : Boolean := True;

      function Make_Component_List_Attributes (CL : Node_Id) return List_Id;
      --  Returns a sequence of Component_Type'Put_Image attribute_references
      --  to process the components that are referenced in the given component
      --  list. Called for the main component list, and then recursively for
      --  variants.

      function Make_Component_Attributes (Clist : List_Id) return List_Id;
      --  Given Clist, a component items list, construct series of
      --  Component_Type'Put_Image attribute_references for componentwise
      --  processing of the corresponding components. Called for the
      --  discriminants, and then from Make_Component_List_Attributes for each
      --  list (including in variants).

      procedure Append_Component_Attr (Clist : List_Id; C : Entity_Id);
      --  Given C, the entity for a discriminant or component, build a call to
      --  Component_Type'Put_Image for the corresponding component value, and
      --  append it onto Clist. Called from Make_Component_Attributes.

      function Make_Component_Name (C : Entity_Id) return Node_Id;
      --  Create a call that prints "Comp_Name => "

      function Null_Record_Default_Implementation_OK
        (Null_Record_Type : Entity_Id) return Boolean
      is
        (if Has_Aspect (Null_Record_Type, Aspect_Put_Image)
           then False
         elsif not Is_Derived_Type
                     (Implementation_Base_Type (Null_Record_Type))
           then True
         else Null_Record_Default_Implementation_OK
                (Implementation_Base_Type (Etype (Null_Record_Type))));
      --  return True iff ok to emit "(NULL RECORD)" for given null record type

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
         Result := Make_Component_Attributes (CI);

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

            --  Note: in the following, we use New_Occurrence_Of for the
            --  selector, since there are cases in which we make a reference
            --  to a hidden discriminant that is not visible.

            D_Ref :=
               Make_Selected_Component (Loc,
                 Prefix        => Make_Identifier (Loc, Name_V),
                 Selector_Name =>
                   New_Occurrence_Of (Entity (Name (VP)), Loc));

            Append_To (Result,
              Make_Case_Statement (Loc,
                Expression   => D_Ref,
                Alternatives => Alts));
         end if;

         return Result;
      end Make_Component_List_Attributes;

      ---------------------------
      -- Append_Component_Attr --
      ---------------------------

      procedure Append_Component_Attr (Clist : List_Id; C : Entity_Id) is
         Component_Typ : constant Entity_Id :=
           Put_Image_Base_Type
             (Get_Corresponding_Mutably_Tagged_Type_If_Present (Etype (C)));
      begin
         if Ekind (C) /= E_Void then
            Append_To (Clist,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Component_Typ, Loc),
                Attribute_Name => Name_Put_Image,
                Expressions    => New_List (
                  Make_Identifier (Loc, Name_S),
                  Make_Selected_Component (Loc,
                    Prefix        => Make_Identifier (Loc, Name_V),
                    Selector_Name => New_Occurrence_Of (C, Loc)))));
         end if;
      end Append_Component_Attr;

      -------------------------------
      -- Make_Component_Attributes --
      -------------------------------

      function Make_Component_Attributes (Clist : List_Id) return List_Id is
         Item   : Node_Id;
         Result : List_Id;

      begin
         Result := New_List;

         if Present (Clist) then
            Item := First (Clist);

            --  Loop through components, skipping all internal components,
            --  which are not part of the value (e.g. _Tag), except that we
            --  don't skip the _Parent, since we do want to process that
            --  recursively.

            while Present (Item) loop
               if Nkind (Item) in
                    N_Component_Declaration | N_Discriminant_Specification
               then
                  if Chars (Defining_Identifier (Item)) = Name_uParent then
                     declare
                        Parent_Type : constant Entity_Id :=
                          Implementation_Base_Type
                            (Etype (Defining_Identifier (Item)));

                        Parent_Aspect_Spec : constant Node_Id :=
                          Find_Aspect (Parent_Type, Aspect_Put_Image);

                        Parent_Type_Decl : constant Node_Id :=
                          Declaration_Node (Parent_Type);

                        Parent_Rdef : Node_Id :=
                          Type_Definition (Parent_Type_Decl);
                     begin
                        --  If parent type has an noninherited
                        --  explicitly-specified Put_Image aspect spec, then
                        --  display parent part by calling specified procedure,
                        --  and then use extension-aggregate syntax for the
                        --  remaining components as per RM 4.10(15/5);
                        --  otherwise, "look through" the parent component
                        --  to its components - we don't want the image text
                        --  to include mention of an "_parent" component.

                        if Present (Parent_Aspect_Spec) and then
                          Entity (Parent_Aspect_Spec) = Parent_Type
                        then
                           Append_Component_Attr
                             (Result, Defining_Identifier (Item));

                           --  Omit the " with " if no subsequent components.

                           if not Is_Null_Extension_Of
                                    (Descendant => Typ,
                                     Ancestor => Parent_Type)
                           then
                              Append_To (Result,
                                 Make_Procedure_Call_Statement (Loc,
                                   Name =>
                                     New_Occurrence_Of
                                       (RTE (RE_Put_UTF_8), Loc),
                                   Parameter_Associations => New_List
                                     (Make_Identifier (Loc, Name_S),
                                      Make_String_Literal (Loc, " with "))));
                           end if;
                        else
                           if Nkind (Parent_Rdef) = N_Derived_Type_Definition
                           then
                              Parent_Rdef :=
                                Record_Extension_Part (Parent_Rdef);
                           end if;

                           if Present (Component_List (Parent_Rdef)) then
                              Append_List_To (Result,
                                 Make_Component_List_Attributes
                                   (Component_List (Parent_Rdef)));
                           end if;
                        end if;
                     end;

                  elsif not Is_Internal_Name
                              (Chars (Defining_Identifier (Item)))
                  then
                     if First_Time then
                        First_Time := False;
                     else
                        Append_To (Result,
                          Make_Procedure_Call_Statement (Loc,
                            Name =>
                              New_Occurrence_Of (RTE (RE_Record_Between), Loc),
                            Parameter_Associations => New_List
                              (Make_Identifier (Loc, Name_S))));
                     end if;

                     Append_To (Result, Make_Component_Name (Item));
                     Append_Component_Attr
                       (Result, Defining_Identifier (Item));
                  end if;
               end if;

               Next (Item);
            end loop;
         end if;

         return Result;
      end Make_Component_Attributes;

      -------------------------
      -- Make_Component_Name --
      -------------------------

      function Make_Component_Name (C : Entity_Id) return Node_Id is
         Name : constant Name_Id := Chars (Defining_Identifier (C));
         pragma Assert (Name /= Name_uParent);

         function To_Upper (S : String) return String;
         --  Same as Ada.Characters.Handling.To_Upper, but withing
         --  Ada.Characters.Handling seems to cause mailserver problems.

         --------------
         -- To_Upper --
         --------------

         function To_Upper (S : String) return String is
         begin
            return Result : String := S do
               for Char of Result loop
                  Char := Fold_Upper (Char);
               end loop;
            end return;
         end To_Upper;

      --  Start of processing for Make_Component_Name

      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Put_UTF_8), Loc),
             Parameter_Associations => New_List
               (Make_Identifier (Loc, Name_S),
                Make_String_Literal (Loc,
                  To_Upper (Get_Name_String (Name)) & " => ")));
      end Make_Component_Name;

      Stms : constant List_Id := New_List;
      Rdef : Node_Id;
      Type_Decl : constant Node_Id :=
        Declaration_Node (Base_Type (Underlying_Type (Btyp)));

   --  Start of processing for Build_Record_Put_Image_Procedure

   begin
      if Ada_Version < Ada_2022
        or else not Put_Image_Enabled (Btyp)
      then
         --  generate a very simple Put_Image implementation

         if Is_RTE (Typ, RE_Root_Buffer_Type) then
            --  Avoid introducing a cyclic dependency between
            --  Ada.Strings.Text_Buffers and System.Put_Images.

            Append_To (Stms,
              Make_Raise_Program_Error (Loc,
              Reason => PE_Explicit_Raise));
         else
            declare
               Type_Name : String_Id;
            begin
               --  If aspect Discard_Names is enabled the intention is to
               --  prevent type names from leaking into object file. Instead,
               --  we emit string that is different from the ones from the
               --  default implementations of the Put_Image attribute.

               if Global_Discard_Names or else Discard_Names (Typ) then
                  Start_String;
                  Store_String_Chars ("(DISCARDED TYPE NAME)");
                  Type_Name := End_String;
               else
                  Type_Name :=
                    Fully_Qualified_Name_String (Btyp, Append_NUL => False);
               end if;

               Append_To (Stms,
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Occurrence_Of (RTE (RE_Put_Image_Unknown), Loc),
                   Parameter_Associations => New_List
                     (Make_Identifier (Loc, Name_S),
                        Make_String_Literal (Loc,
                          Type_Name))));
            end;
         end if;

      elsif Is_Null_Record_Type (Btyp, Ignore_Privacy => True)
        and then Null_Record_Default_Implementation_OK (Btyp)
      then

         --  Interface types take this path.

         Append_To (Stms,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Put_UTF_8), Loc),
             Parameter_Associations => New_List
               (Make_Identifier (Loc, Name_S),
                Make_String_Literal (Loc, "(NULL RECORD)"))));

      elsif Is_Derived_Type (Btyp)
         and then (not Is_Tagged_Type (Btyp) or else Is_Null_Extension (Btyp))
      then
         declare
            Parent_Type : constant Entity_Id := Base_Type (Etype (Btyp));
         begin
            Append_To (Stms,
              Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Parent_Type, Loc),
              Attribute_Name => Name_Put_Image,
              Expressions    => New_List (
                                  Make_Identifier (Loc, Name_S),
                                  Make_Type_Conversion (Loc,
                                    Subtype_Mark => New_Occurrence_Of
                                                      (Parent_Type, Loc),
                                    Expression => Make_Identifier
                                                    (Loc, Name_V)))));
         end;

      else
         Append_To (Stms,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Record_Before), Loc),
             Parameter_Associations => New_List
               (Make_Identifier (Loc, Name_S))));

         --  Generate Put_Images for the discriminants of the type

         Append_List_To (Stms,
           Make_Component_Attributes
             (Discriminant_Specifications (Type_Decl)));

         Rdef := Type_Definition (Type_Decl);

         --  In the record extension case, the components we want are to be
         --  found in the extension (although we have to process the
         --  _Parent component to find inherited components).

         if Nkind (Rdef) = N_Derived_Type_Definition then
            Rdef := Record_Extension_Part (Rdef);
         end if;

         if Present (Component_List (Rdef)) then
            Append_List_To (Stms,
              Make_Component_List_Attributes (Component_List (Rdef)));
         end if;

         Append_To (Stms,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Record_After), Loc),
             Parameter_Associations => New_List
               (Make_Identifier (Loc, Name_S))));
      end if;

      Pnam := Make_Put_Image_Name (Loc, Btyp);
      Build_Put_Image_Proc (Loc, Btyp, Decl, Pnam, Stms);
   end Build_Record_Put_Image_Procedure;

   -----------------------------
   -- Build_Put_Image_Profile --
   -----------------------------

   function Build_Put_Image_Profile
     (Loc : Source_Ptr; Typ : Entity_Id) return List_Id
   is
   begin
      return New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_S),
          In_Present          => True,
          Out_Present         => True,
          Parameter_Type      =>
            New_Occurrence_Of
              (Class_Wide_Type (RTE (RE_Root_Buffer_Type)), Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)));
   end Build_Put_Image_Profile;

   --------------------------
   -- Build_Put_Image_Proc --
   --------------------------

   procedure Build_Put_Image_Proc
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Stms : List_Id)
   is
      Spec : constant Node_Id :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => Pnam,
          Parameter_Specifications => Build_Put_Image_Profile (Loc, Typ));
   begin
      Decl :=
        Make_Subprogram_Body (Loc,
          Specification              => Spec,
          Declarations               => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));
   end Build_Put_Image_Proc;

   ----------------------------------
   -- Build_Unknown_Put_Image_Call --
   ----------------------------------

   function Build_Unknown_Put_Image_Call (N : Node_Id) return Node_Id is
      Loc    : constant Source_Ptr := Sloc (N);
      Sink   : constant Node_Id    := First (Expressions (N));
      Lib_RE : constant RE_Id      := RE_Put_Image_Unknown;
      Libent : constant Entity_Id  := RTE (Lib_RE);
   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Libent, Loc),
          Parameter_Associations => New_List (
            Relocate_Node (Sink),
            Make_String_Literal (Loc,
              Exp_Util.Fully_Qualified_Name_String (
                Entity (Prefix (N)), Append_NUL => False))));
   end Build_Unknown_Put_Image_Call;

   -----------------------
   -- Put_Image_Enabled --
   -----------------------

   function Put_Image_Enabled (Typ : Entity_Id) return Boolean is
   begin
      --  If this function returns False for a non-scalar type Typ, then
      --    a) calls to Typ'Image will result in calls to
      --       System.Put_Images.Put_Image_Unknown to generate the image.
      --    b) If Typ is a tagged type, then similarly the implementation
      --       of Typ's Put_Image procedure will call Put_Image_Unknown
      --       and will ignore its formal parameter of type Typ.
      --       Note that Typ will still have a Put_Image procedure
      --       in this case, albeit one with a simplified implementation.
      --
      --  The name "Sink" here is a short nickname for
      --  "Ada.Strings.Text_Buffers.Root_Buffer_Type".
      --

      --  Put_Image does not work for Remote_Types. We check the containing
      --  package, rather than the type itself, because we want to include
      --  types in the private part of a Remote_Types package.

      if Is_Remote_Types (Scope (Typ))
        or else Is_Remote_Call_Interface (Typ)
      then
         return False;
      end if;

      --  No sense in generating code for Put_Image if there are errors. This
      --  avoids certain cascade errors.

      if Total_Errors_Detected > 0 then
         return False;
      end if;

      --  If type Sink is unavailable in this runtime, disable Put_Image
      --  altogether.

      if No_Run_Time_Mode or else not RTE_Available (RE_Root_Buffer_Type) then
         return False;
      end if;

      if Is_Tagged_Type (Typ) then
         if Is_Class_Wide_Type (Typ) then
            return Put_Image_Enabled (Find_Specific_Type (Base_Type (Typ)));
         elsif Present (Find_Aspect (Typ, Aspect_Put_Image,
                                     Or_Rep_Item => True))
         then
            null;
         elsif Is_Derived_Type (Typ) then
            return Put_Image_Enabled (Etype (Base_Type (Typ)));
         elsif Is_Predefined_Unit (Get_Code_Unit (Typ)) then
            return False;
         end if;
      end if;

      --  ???Disable Put_Image on type Root_Buffer_Type declared in
      --  Ada.Strings.Text_Buffers. Note that we can't call Is_RTU on
      --  Ada_Strings_Text_Buffers, because it's not known yet (we might be
      --  compiling it). But this is insufficient to allow support for tagged
      --  predefined types.

      declare
         Parent_Scope : constant Entity_Id := Scope (Scope (Typ));
      begin
         if Present (Parent_Scope)
           and then Is_RTU (Parent_Scope, Ada_Strings)
           and then Chars (Scope (Typ)) = Name_Find ("text_buffers")
         then
            return False;
         end if;
      end;

      --  Disable for CPP types, because the components are unavailable on the
      --  Ada side.

      if Is_Tagged_Type (Typ)
        and then Convention (Typ) = Convention_CPP
        and then Is_CPP_Class (Root_Type (Typ))
      then
         return False;
      end if;

      --  Disable for unchecked unions, because there is no way to know the
      --  discriminant value, and therefore no way to know which components
      --  should be printed.

      if Is_Unchecked_Union (Typ) then
         return False;
      end if;

      return True;
   end Put_Image_Enabled;

   -------------------------
   -- Make_Put_Image_Name --
   -------------------------

   function Make_Put_Image_Name
     (Loc : Source_Ptr; Typ : Entity_Id) return Entity_Id
   is
      Sname : Name_Id;
   begin
      --  For tagged types, we are dealing with a TSS associated with the
      --  declaration, so we use the standard primitive function name. For
      --  other types, generate a local TSS name since we are generating
      --  the subprogram at the point of use.

      if Is_Tagged_Type (Typ) then
         Sname := Make_TSS_Name (Typ, TSS_Put_Image);
      else
         Sname := Make_TSS_Name_Local (Typ, TSS_Put_Image);
      end if;

      return Make_Defining_Identifier (Loc, Sname);
   end Make_Put_Image_Name;

   ---------------------------------
   -- Image_Should_Call_Put_Image --
   ---------------------------------

   function Image_Should_Call_Put_Image (N : Node_Id) return Boolean is
   begin
      if Ada_Version < Ada_2022 then
         return False;
      end if;

      --  In Ada 2022, T'Image calls T'Put_Image if there is an explicit
      --  (or inherited) aspect_specification for Put_Image, or if
      --  U_Type'Image is illegal in pre-2022 versions of Ada.

      declare
         U_Type : constant Entity_Id := Underlying_Type (Entity (Prefix (N)));
      begin
         if Has_Aspect (U_Type, Aspect_Put_Image)
           or else not Is_Scalar_Type (U_Type)
         then
            return True;
         end if;

         --  Deal with Itypes. One case where this is needed is for a
         --  fixed-point type with a Put_Image aspect specification.

         --  ??? Should we be checking for Itype case here, or in Has_Aspect?
         --  In other words, do we want to do what we are doing here for all
         --  aspects, not just for Put_Image?

         if Is_Itype (U_Type)
           and then Nkind (Associated_Node_For_Itype (U_Type)) in
                      N_Full_Type_Declaration | N_Subtype_Declaration
           and then Has_Aspect (Defining_Identifier
                                  (Associated_Node_For_Itype (U_Type)),
                                Aspect_Put_Image)
         then
            return True;
         end if;

         return False;
      end;
   end Image_Should_Call_Put_Image;

   ----------------------
   -- Build_Image_Call --
   ----------------------

   function Build_Image_Call (N : Node_Id) return Node_Id is
      --  For T'[[Wide_]Wide_]Image (X) Generate an Expression_With_Actions
      --  node:
      --
      --     do
      --        S : Buffer;
      --        U_Type'Put_Image (S, X);
      --        Result : constant [[Wide_]Wide_]String :=
      --          [[Wide_[Wide_]]Get (S);
      --        Destroy (S);
      --     in Result end
      --
      --  where U_Type is the underlying type, as needed to bypass privacy.

      Loc : constant Source_Ptr := Sloc (N);
      U_Type : constant Entity_Id := Underlying_Type (Entity (Prefix (N)));
      Sink_Entity : constant Entity_Id :=
        Make_Temporary (Loc, 'S');
      Sink_Decl : constant Node_Id :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Sink_Entity,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Buffer_Type), Loc));

      Image_Prefix : constant Node_Id :=
        Duplicate_Subexpr (First (Expressions (N)));

      Put_Im : constant Node_Id :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (U_Type, Loc),
          Attribute_Name => Name_Put_Image,
          Expressions    => New_List (
            New_Occurrence_Of (Sink_Entity, Loc),
            Image_Prefix));
      Result_Entity : constant Entity_Id :=
        Make_Temporary (Loc, 'R');

      subtype Image_Name_Id is Name_Id with Static_Predicate =>
        Image_Name_Id in Name_Image | Name_Wide_Image | Name_Wide_Wide_Image;
      --  Attribute names that will be mapped to the corresponding result types
      --  and functions.

      Attribute_Name_Id : constant Name_Id :=
        (if Attribute_Name (N) = Name_Img then Name_Image
         else Attribute_Name (N));

      Result_Typ    : constant Entity_Id :=
        (case Image_Name_Id'(Attribute_Name_Id) is
            when Name_Image           => Stand.Standard_String,
            when Name_Wide_Image      => Stand.Standard_Wide_String,
            when Name_Wide_Wide_Image => Stand.Standard_Wide_Wide_String);
      Get_Func_Id   : constant RE_Id :=
        (case Image_Name_Id'(Attribute_Name_Id) is
            when Name_Image           => RE_Get,
            when Name_Wide_Image      => RE_Wide_Get,
            when Name_Wide_Wide_Image => RE_Wide_Wide_Get);

      Result_Decl : constant Node_Id :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Result_Entity,
          Object_Definition =>
            New_Occurrence_Of (Result_Typ, Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (Get_Func_Id), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Sink_Entity, Loc))));
      Actions : List_Id;

      function Put_String_Exp (String_Exp : Node_Id;
                               Wide_Wide  : Boolean := False) return Node_Id;
      --  Generate a call to evaluate a String (or Wide_Wide_String, depending
      --  on the Wide_Wide Boolean parameter) expression and output it into
      --  the buffer.

      --------------------
      -- Put_String_Exp --
      --------------------

      function Put_String_Exp (String_Exp : Node_Id;
                               Wide_Wide  : Boolean := False) return Node_Id is
         Put_Id : constant RE_Id :=
           (if Wide_Wide then RE_Wide_Wide_Put else RE_Put_UTF_8);

         --  We could build a nondispatching call here, but to make
         --  that work we'd have to change Rtsfind spec to make available
         --  corresponding callees out of Ada.Strings.Text_Buffers.Unbounded
         --  (as opposed to from Ada.Strings.Text_Buffers). Seems simpler to
         --  introduce a type conversion and leave it to the optimizer to
         --  eliminate the dispatching. This does not *introduce* any problems
         --  if a no-dispatching-allowed restriction is in effect, since we
         --  are already in the middle of generating a call to T'Class'Image.

         Sink_Exp : constant Node_Id :=
           Make_Type_Conversion (Loc,
             Subtype_Mark =>
               New_Occurrence_Of
                 (Class_Wide_Type (RTE (RE_Root_Buffer_Type)), Loc),
             Expression   => New_Occurrence_Of (Sink_Entity, Loc));
      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (Put_Id), Loc),
             Parameter_Associations => New_List (Sink_Exp, String_Exp));
      end Put_String_Exp;

      --  Local variables

      Tag_Node : Node_Id;

   --  Start of processing for Build_Image_Call

   begin
      if Is_Class_Wide_Type (U_Type) then

         --  For interface types we must generate code to displace the pointer
         --  to the object to reference the base of the underlying object.

         --  Generate:
         --    To_Tag_Ptr (Image_Prefix'Address).all

         --  Note that Image_Prefix'Address is recursively expanded into a
         --  call to Ada.Tags.Base_Address (Image_Prefix'Address).

         if Is_Interface (U_Type) then
            Tag_Node :=
              Make_Explicit_Dereference (Loc,
                Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                  Make_Attribute_Reference (Loc,
                    Prefix => Duplicate_Subexpr (Image_Prefix),
                    Attribute_Name => Name_Address)));

         --  Common case

         else
            Tag_Node :=
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (Image_Prefix),
                Attribute_Name => Name_Tag);
         end if;

         --  Generate qualified-expression syntax; qualification name comes
         --  from calling Ada.Tags.Wide_Wide_Expanded_Name.

         declare
            --  The copy of Image_Prefix will be evaluated before the
            --  original, which is ok if no side effects are involved.

            pragma Assert (Side_Effect_Free (Image_Prefix));

            Specific_Type_Name : constant Node_Id :=
              Put_String_Exp
                (Make_Function_Call (Loc,
                   Name => New_Occurrence_Of
                             (RTE (RE_Wide_Wide_Expanded_Name), Loc),
                   Parameter_Associations => New_List (Tag_Node)),
                 Wide_Wide => True);

            Qualification : constant Node_Id :=
              Put_String_Exp (Make_String_Literal (Loc, "'"));
         begin
            Actions := New_List
                         (Sink_Decl,
                          Specific_Type_Name,
                          Qualification,
                          Put_Im,
                          Result_Decl);
         end;
      else
         Actions := New_List (Sink_Decl, Put_Im, Result_Decl);
      end if;

      --  To avoid leaks, we need to manage the secondary stack, because Get is
      --  returning a String allocated thereon. It might be cleaner to let the
      --  normal mechanisms for functions returning on the secondary stack call
      --  Set_Uses_Sec_Stack, but this expansion of 'Image is happening too
      --  late for that.

      Set_Uses_Sec_Stack (Current_Scope);

      return Make_Expression_With_Actions (Loc,
        Actions    => Actions,
        Expression => New_Occurrence_Of (Result_Entity, Loc));
   end Build_Image_Call;

   ------------------------------
   -- Preload_Root_Buffer_Type --
   ------------------------------

   Preload_Root_Buffer_Type_Done : Boolean := False;
   --  True if Preload_Root_Buffer_Type has already done its work;
   --  no need to do it again in that case.

   Debug_Unit_Walk : Boolean renames Debug_Flag_Dot_WW;

   procedure Preload_Root_Buffer_Type (Compilation_Unit : Node_Id) is
      Ignore : Entity_Id;
   begin
      if Preload_Root_Buffer_Type_Done then
         return;
      end if;

      --  We can't call RTE (RE_Root_Buffer_Type) for at least some
      --  predefined units, because it would introduce cyclic dependences.
      --  The package where Root_Buffer_Type is declared, for example, and
      --  things it depends on.
      --
      --  It's only needed for tagged types, so don't do it unless Put_Image is
      --  enabled for tagged types, and we've seen a tagged type. Note that
      --  Tagged_Seen is set True by the parser if the "tagged" reserved word
      --  is seen; this flag tells us whether we have any tagged types.
      --  It's unfortunate to have this Tagged_Seen processing so scattered
      --  about, but we need to know if there are tagged types where this is
      --  called in Analyze_Compilation_Unit, before we have analyzed any type
      --  declarations. This mechanism also prevents doing
      --  RTE (RE_Root_Buffer_Type) when compiling the compiler itself.
      --  Packages Ada.Strings.Buffer_Types and friends are not included
      --  in the compiler.

      if not In_Predefined_Unit (Compilation_Unit)
        and then Tagged_Seen
        and then not No_Run_Time_Mode
      then
         Preload_Root_Buffer_Type_Done := True;

         --  Don't do it if type Root_Buffer_Type is unavailable in the
         --  runtime.

         if RTE_Available (RE_Root_Buffer_Type) then
            if Debug_Unit_Walk then
               Write_Line ("Preload_Root_Buffer_Type: ");
               Write_Unit_Info
                 (Get_Cunit_Unit_Number (Compilation_Unit),
                  Unit (Compilation_Unit));
            end if;

            Ignore := RTE (RE_Root_Buffer_Type);
         end if;
      end if;
   end Preload_Root_Buffer_Type;

   -------------------------
   -- Put_Image_Base_Type --
   -------------------------

   function Put_Image_Base_Type (E : Entity_Id) return Entity_Id is
   begin
      if Is_Array_Type (E) and then Is_First_Subtype (E) then
         return E;
      elsif Is_Private_Type (Base_Type (E)) and not Is_Private_Type (E) then
         return Implementation_Base_Type (E);
      else
         return Base_Type (E);
      end if;
   end Put_Image_Base_Type;

end Exp_Put_Image;
