------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        E X P _ P U T _ I M A G E                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2020, Free Software Foundation, Inc.           --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Exp_Tss;  use Exp_Tss;
with Exp_Util;
with Debug;    use Debug;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem_Aux;  use Sem_Aux;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Exp_Put_Image is

   Tagged_Put_Image_Enabled : Boolean renames Debug_Flag_Underscore_Z;
   --  ???Set True to enable Put_Image for at least some tagged types

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
     (Nod  : Node_Id;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc  : constant Source_Ptr := Sloc (Nod);

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
           Make_Implicit_Loop_Statement (Nod, Statements => Stms);
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
      FST     : constant Entity_Id  := First_Subtype (U_Type);
      Sink    : constant Node_Id    := First (Expressions (N));
      Item    : constant Node_Id    := Next (Sink);
      P_Size  : constant Uint       := Esize (FST);
      Lib_RE  : RE_Id;

   begin
      if Is_Signed_Integer_Type (U_Type) then
         if P_Size <= Standard_Integer_Size then
            Lib_RE := RE_Put_Image_Integer;
         else
            pragma Assert (P_Size <= Standard_Long_Long_Integer_Size);
            Lib_RE := RE_Put_Image_Long_Long_Integer;
         end if;

      elsif Is_Modular_Integer_Type (U_Type) then
         if P_Size <= Standard_Integer_Size then -- Yes, Integer
            Lib_RE := RE_Put_Image_Unsigned;
         else
            pragma Assert (P_Size <= Standard_Long_Long_Integer_Size);
            Lib_RE := RE_Put_Image_Long_Long_Unsigned;
         end if;

      elsif Is_Access_Type (U_Type) then
         if Is_Access_Protected_Subprogram_Type (U_Type) then
            Lib_RE := RE_Put_Image_Access_Prot_Subp;
         elsif Is_Access_Subprogram_Type (U_Type) then
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
         --     Put_Wide_Wide_String (Sink, U_Type'Wide_Wide_Image (Item));
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
            Put_Call : constant Node_Id :=
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Put_Wide_Wide_String), Loc),
                Parameter_Associations => New_List
                  (Relocate_Node (Sink), Image));
         begin
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

   -------------------------------------
   -- Build_String_Put_Image_Call --
   -------------------------------------

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
      if R = Standard_String then
         Lib_RE := RE_Put_Image_String;
      elsif R = Standard_Wide_String then
         Lib_RE := RE_Put_Image_Wide_String;
      elsif R = Standard_Wide_Wide_String then
         Lib_RE := RE_Put_Image_Wide_Wide_String;
      else
         raise Program_Error;
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
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Libent, Loc),
             Parameter_Associations => New_List (
               Relocate_Node (Sink),
               Conv));
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

   ------------------------------------
   -- Build_Task_Put_Image_Call --
   ------------------------------------

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
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      pragma Assert (Typ = Base_Type (Typ));
      pragma Assert (not Is_Unchecked_Union (Typ));

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

      --------------------------------
      -- Append_Component_Attr --
      --------------------------------

      procedure Append_Component_Attr (Clist : List_Id; C : Entity_Id) is
         Component_Typ : constant Entity_Id := Put_Image_Base_Type (Etype (C));
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
            --  recursively. If _Parent is an interface type, being abstract
            --  with no components there is no need to handle it.

            while Present (Item) loop
               if Nkind_In (Item, N_Component_Declaration,
                                  N_Discriminant_Specification)
                 and then
                   ((Chars (Defining_Identifier (Item)) = Name_uParent
                       and then not Is_Interface
                                      (Etype (Defining_Identifier (Item))))
                     or else
                    not Is_Internal_Name (Chars (Defining_Identifier (Item))))
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
                  Append_Component_Attr (Result, Defining_Identifier (Item));
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
      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (RTE (RE_Put_UTF_8), Loc),
             Parameter_Associations => New_List
               (Make_Identifier (Loc, Name_S),
                Make_String_Literal (Loc, Get_Name_String (Name) & " => ")));
      end Make_Component_Name;

      Stms : constant List_Id := New_List;
      Rdef : Node_Id;
      Type_Decl : constant Node_Id :=
        Declaration_Node (Base_Type (Underlying_Type (Typ)));

   --  Start of processing for Build_Record_Put_Image_Procedure

   begin
      Append_To (Stms,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (RTE (RE_Record_Before), Loc),
          Parameter_Associations => New_List
            (Make_Identifier (Loc, Name_S))));

      --  Generate Put_Images for the discriminants of the type

      Append_List_To (Stms,
        Make_Component_Attributes (Discriminant_Specifications (Type_Decl)));

      Rdef := Type_Definition (Type_Decl);

      --  In the record extension case, the components we want, including the
      --  _Parent component representing the parent type, are to be found in
      --  the extension. We will process the _Parent component using the type
      --  of the parent.

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

      Pnam := Make_Put_Image_Name (Loc, Typ);
      Build_Put_Image_Proc (Loc, Typ, Decl, Pnam, Stms);
   end Build_Record_Put_Image_Procedure;

   -------------------------------
   -- Build_Put_Image_Profile --
   -------------------------------

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
            New_Occurrence_Of (Class_Wide_Type (RTE (RE_Sink)), Loc)),

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

   ------------------------------------
   -- Build_Unknown_Put_Image_Call --
   ------------------------------------

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

   ----------------------
   -- Enable_Put_Image --
   ----------------------

   function Enable_Put_Image (Typ : Entity_Id) return Boolean is
   begin
      --  There's a bit of a chicken&egg problem. The compiler is likely to
      --  have trouble if we refer to the Put_Image of Sink itself, because
      --  Sink is part of the parameter profile:
      --
      --     function Sink'Put_Image (S : in out Sink'Class; V : T);
      --
      --  Likewise, the Ada.Strings.Text_Output package, where Sink is
      --  declared, depends on various other packages, so if we refer to
      --  Put_Image of types declared in those other packages, we could create
      --  cyclic dependencies. Therefore, we disable Put_Image for some
      --  types. It's not clear exactly what types should be disabled. Scalar
      --  types are OK, even if predefined, because calls to Put_Image of
      --  scalar types are expanded inline. We certainly want to be able to use
      --  Integer'Put_Image, for example.

      --  ???Temporarily disable to work around bugs:
      --
      --  Put_Image does not work for Remote_Types. We check the containing
      --  package, rather than the type itself, because we want to include
      --  types in the private part of a Remote_Types package.
      --
      --  Put_Image on tagged types triggers some bugs.

      if Is_Remote_Types (Scope (Typ))
        or else (Is_Tagged_Type (Typ) and then In_Predefined_Unit (Typ))
        or else (Is_Tagged_Type (Typ) and then not Tagged_Put_Image_Enabled)
      then
         return False;
      end if;

      --  End of workarounds.

      --  No sense in generating code for Put_Image if there are errors. This
      --  avoids certain cascade errors.

      if Total_Errors_Detected > 0 then
         return False;
      end if;

      --  If type Sink is unavailable in this runtime, disable Put_Image
      --  altogether.

      if No_Run_Time_Mode or else not RTE_Available (RE_Sink) then
         return False;
      end if;

      --  ???Disable Put_Image on type Sink declared in
      --  Ada.Strings.Text_Output. Note that we can't call Is_RTU on
      --  Ada_Strings_Text_Output, because it's not known yet (we might be
      --  compiling it). But this is insufficient to allow support for tagged
      --  predefined types.

      declare
         Parent_Scope : constant Entity_Id := Scope (Scope (Typ));
      begin
         if Present (Parent_Scope)
           and then Is_RTU (Parent_Scope, Ada_Strings)
           and then Chars (Scope (Typ)) = Name_Find ("text_output")
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
   end Enable_Put_Image;

   ---------------------------------
   -- Make_Put_Image_Name --
   ---------------------------------

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

   function Image_Should_Call_Put_Image (N : Node_Id) return Boolean is
   begin
      if Ada_Version < Ada_2020 then
         return False;
      end if;

      --  In Ada 2020, T'Image calls T'Put_Image if there is an explicit
      --  aspect_specification for Put_Image, or if U_Type'Image is illegal
      --  in pre-2020 versions of Ada.

      declare
         U_Type : constant Entity_Id := Underlying_Type (Entity (Prefix (N)));
      begin
         if Present (TSS (U_Type, TSS_Put_Image)) then
            return True;
         end if;

         return not Is_Scalar_Type (U_Type);
      end;
   end Image_Should_Call_Put_Image;

   function Build_Image_Call (N : Node_Id) return Node_Id is
      --  For T'Image (X) Generate an Expression_With_Actions node:
      --
      --     do
      --        S : Buffer := New_Buffer;
      --        U_Type'Put_Image (S, X);
      --        Result : constant String := Get (S);
      --        Destroy (S);
      --     in Result end
      --
      --  where U_Type is the underlying type, as needed to bypass privacy.

      Loc : constant Source_Ptr := Sloc (N);
      U_Type : constant Entity_Id := Underlying_Type (Entity (Prefix (N)));
      Sink_Entity : constant Entity_Id :=
        Make_Defining_Identifier (Loc, Chars => New_Internal_Name ('S'));
      Sink_Decl : constant Node_Id :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Sink_Entity,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Buffer), Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (RE_New_Buffer), Loc),
              Parameter_Associations => Empty_List));
      Put_Im : constant Node_Id :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (U_Type, Loc),
          Attribute_Name => Name_Put_Image,
          Expressions    => New_List (
            New_Occurrence_Of (Sink_Entity, Loc),
            New_Copy_Tree (First (Expressions (N)))));
      Result_Entity : constant Entity_Id :=
        Make_Defining_Identifier (Loc, Chars => New_Internal_Name ('R'));
      Result_Decl : constant Node_Id :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Result_Entity,
          Object_Definition =>
            New_Occurrence_Of (Stand.Standard_String, Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (RE_Get), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Sink_Entity, Loc))));
      Image : constant Node_Id :=
        Make_Expression_With_Actions (Loc,
          Actions => New_List (Sink_Decl, Put_Im, Result_Decl),
          Expression => New_Occurrence_Of (Result_Entity, Loc));
   begin
      return Image;
   end Build_Image_Call;

   ------------------
   -- Preload_Sink --
   ------------------

   procedure Preload_Sink (Compilation_Unit : Node_Id) is
   begin
      --  We can't call RTE (RE_Sink) for at least some predefined units,
      --  because it would introduce cyclic dependences. The package where Sink
      --  is declared, for example, and things it depends on.
      --
      --  It's only needed for tagged types, so don't do it unless Put_Image is
      --  enabled for tagged types, and we've seen a tagged type. Note that
      --  Tagged_Seen is set True by the parser if the "tagged" reserved word
      --  is seen; this flag tells us whether we have any tagged types.
      --  It's unfortunate to have this Tagged_Seen processing so scattered
      --  about, but we need to know if there are tagged types where this is
      --  called in Analyze_Compilation_Unit, before we have analyzed any type
      --  declarations. This mechanism also prevents doing RTE (RE_Sink) when
      --  compiling the compiler itself. Packages Ada.Strings.Text_Output and
      --  friends are not included in the compiler.
      --
      --  Don't do it if type Sink is unavailable in the runtime.

      if not In_Predefined_Unit (Compilation_Unit)
        and then Tagged_Put_Image_Enabled
        and then Tagged_Seen
        and then not No_Run_Time_Mode
        and then RTE_Available (RE_Sink)
      then
         declare
            Ignore : constant Entity_Id := RTE (RE_Sink);
         begin
            null;
         end;
      end if;
   end Preload_Sink;

   -------------------------
   -- Put_Image_Base_Type --
   -------------------------

   function Put_Image_Base_Type (E : Entity_Id) return Entity_Id is
   begin
      if Is_Array_Type (E) and then Is_First_Subtype (E) then
         return E;
      else
         return Base_Type (E);
      end if;
   end Put_Image_Base_Type;

end Exp_Put_Image;
