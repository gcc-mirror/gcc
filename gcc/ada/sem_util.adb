------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Errout;   use Errout;
with Elists;   use Elists;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Scans;    use Scans;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Style;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uname;    use Uname;

package body Sem_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Build_Component_Subtype
     (C   : List_Id;
      Loc : Source_Ptr;
      T   : Entity_Id) return Node_Id;
   --  This function builds the subtype for Build_Actual_Subtype_Of_Component
   --  and Build_Discriminal_Subtype_Of_Component. C is a list of constraints,
   --  Loc is the source location, T is the original subtype.

   function Is_Fully_Initialized_Variant (Typ : Entity_Id) return Boolean;
   --  Subsidiary to Is_Fully_Initialized_Type. For an unconstrained type
   --  with discriminants whose default values are static, examine only the
   --  components in the selected variant to determine whether all of them
   --  have a default.

   function Has_Null_Extension (T : Entity_Id) return Boolean;
   --  T is a derived tagged type. Check whether the type extension is null.
   --  If the parent type is fully initialized, T can be treated as such.

   --------------------------------
   -- Add_Access_Type_To_Process --
   --------------------------------

   procedure Add_Access_Type_To_Process (E : Entity_Id; A : Entity_Id) is
      L : Elist_Id;

   begin
      Ensure_Freeze_Node (E);
      L := Access_Types_To_Process (Freeze_Node (E));

      if No (L) then
         L := New_Elmt_List;
         Set_Access_Types_To_Process (Freeze_Node (E), L);
      end if;

      Append_Elmt (A, L);
   end Add_Access_Type_To_Process;

   -----------------------
   -- Alignment_In_Bits --
   -----------------------

   function Alignment_In_Bits (E : Entity_Id) return Uint is
   begin
      return Alignment (E) * System_Storage_Unit;
   end Alignment_In_Bits;

   -----------------------------------------
   -- Apply_Compile_Time_Constraint_Error --
   -----------------------------------------

   procedure Apply_Compile_Time_Constraint_Error
     (N      : Node_Id;
      Msg    : String;
      Reason : RT_Exception_Code;
      Ent    : Entity_Id  := Empty;
      Typ    : Entity_Id  := Empty;
      Loc    : Source_Ptr := No_Location;
      Rep    : Boolean    := True;
      Warn   : Boolean    := False)
   is
      Stat : constant Boolean := Is_Static_Expression (N);
      Rtyp : Entity_Id;

   begin
      if No (Typ) then
         Rtyp := Etype (N);
      else
         Rtyp := Typ;
      end if;

      Discard_Node
        (Compile_Time_Constraint_Error (N, Msg, Ent, Loc, Warn => Warn));

      if not Rep then
         return;
      end if;

      --  Now we replace the node by an N_Raise_Constraint_Error node
      --  This does not need reanalyzing, so set it as analyzed now.

      Rewrite (N,
        Make_Raise_Constraint_Error (Sloc (N),
          Reason => Reason));
      Set_Analyzed (N, True);
      Set_Etype (N, Rtyp);
      Set_Raises_Constraint_Error (N);

      --  If the original expression was marked as static, the result is
      --  still marked as static, but the Raises_Constraint_Error flag is
      --  always set so that further static evaluation is not attempted.

      if Stat then
         Set_Is_Static_Expression (N);
      end if;
   end Apply_Compile_Time_Constraint_Error;

   --------------------------
   -- Build_Actual_Subtype --
   --------------------------

   function Build_Actual_Subtype
     (T : Entity_Id;
      N : Node_Or_Entity_Id) return Node_Id
   is
      Obj : Node_Id;

      Loc         : constant Source_Ptr := Sloc (N);
      Constraints : List_Id;
      Decl        : Node_Id;
      Discr       : Entity_Id;
      Hi          : Node_Id;
      Lo          : Node_Id;
      Subt        : Entity_Id;
      Disc_Type   : Entity_Id;

   begin
      if Nkind (N) = N_Defining_Identifier then
         Obj := New_Reference_To (N, Loc);
      else
         Obj := N;
      end if;

      if Is_Array_Type (T) then
         Constraints := New_List;

         for J in 1 .. Number_Dimensions (T) loop

            --  Build an array subtype declaration with the nominal
            --  subtype and the bounds of the actual. Add the declaration
            --  in front of the local declarations for the subprogram, for
            --  analysis before any reference to the formal in the body.

            Lo :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  Duplicate_Subexpr_No_Checks (Obj, Name_Req => True),
                Attribute_Name => Name_First,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, J)));

            Hi :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  Duplicate_Subexpr_No_Checks (Obj, Name_Req => True),
                Attribute_Name => Name_Last,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, J)));

            Append (Make_Range (Loc, Lo, Hi), Constraints);
         end loop;

      --  If the type has unknown discriminants there is no constrained
      --  subtype to build. This is never called for a formal or for a
      --  lhs, so returning the type is ok ???

      elsif Has_Unknown_Discriminants (T) then
         return T;

      else
         Constraints := New_List;

         if Is_Private_Type (T) and then No (Full_View (T)) then

            --  Type is a generic derived type. Inherit discriminants from
            --  Parent type.

            Disc_Type := Etype (Base_Type (T));
         else
            Disc_Type := T;
         end if;

         Discr := First_Discriminant (Disc_Type);

         while Present (Discr) loop
            Append_To (Constraints,
              Make_Selected_Component (Loc,
                Prefix =>
                  Duplicate_Subexpr_No_Checks (Obj),
                Selector_Name => New_Occurrence_Of (Discr, Loc)));
            Next_Discriminant (Discr);
         end loop;
      end if;

      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (T,  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => Constraints)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Actual_Subtype;

   ---------------------------------------
   -- Build_Actual_Subtype_Of_Component --
   ---------------------------------------

   function Build_Actual_Subtype_Of_Component
     (T : Entity_Id;
      N : Node_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (N);
      P         : constant Node_Id    := Prefix (N);
      D         : Elmt_Id;
      Id        : Node_Id;
      Indx_Type : Entity_Id;

      Deaccessed_T : Entity_Id;
      --  This is either a copy of T, or if T is an access type, then it is
      --  the directly designated type of this access type.

      function Build_Actual_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      function Build_Actual_Record_Constraint return List_Id;
      --  Similar to previous one, for discriminated components constrained
      --  by the discriminant of the enclosing object.

      -----------------------------------
      -- Build_Actual_Array_Constraint --
      -----------------------------------

      function Build_Actual_Array_Constraint return List_Id is
         Constraints : constant List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (Deaccessed_T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

            if Denotes_Discriminant (Old_Lo) then
               Lo :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (P),
                   Selector_Name => New_Occurrence_Of (Entity (Old_Lo), Loc));

            else
               Lo := New_Copy_Tree (Old_Lo);

               --  The new bound will be reanalyzed in the enclosing
               --  declaration. For literal bounds that come from a type
               --  declaration, the type of the context must be imposed, so
               --  insure that analysis will take place. For non-universal
               --  types this is not strictly necessary.

               Set_Analyzed (Lo, False);
            end if;

            if Denotes_Discriminant (Old_Hi) then
               Hi :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (P),
                   Selector_Name => New_Occurrence_Of (Entity (Old_Hi), Loc));

            else
               Hi := New_Copy_Tree (Old_Hi);
               Set_Analyzed (Hi, False);
            end if;

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Actual_Array_Constraint;

      ------------------------------------
      -- Build_Actual_Record_Constraint --
      ------------------------------------

      function Build_Actual_Record_Constraint return List_Id is
         Constraints : constant List_Id := New_List;
         D           : Elmt_Id;
         D_Val       : Node_Id;

      begin
         D := First_Elmt (Discriminant_Constraint (Deaccessed_T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               D_Val :=  Make_Selected_Component (Loc,
                 Prefix => New_Copy_Tree (P),
                Selector_Name => New_Occurrence_Of (Entity (Node (D)), Loc));

            else
               D_Val := New_Copy_Tree (Node (D));
            end if;

            Append (D_Val, Constraints);
            Next_Elmt (D);
         end loop;

         return Constraints;
      end Build_Actual_Record_Constraint;

   --  Start of processing for Build_Actual_Subtype_Of_Component

   begin
      if In_Default_Expression then
         return Empty;

      elsif Nkind (N) = N_Explicit_Dereference then
         if Is_Composite_Type (T)
           and then not Is_Constrained (T)
           and then not (Is_Class_Wide_Type (T)
                          and then Is_Constrained (Root_Type (T)))
           and then not Has_Unknown_Discriminants (T)
         then
            --  If the type of the dereference is already constrained, it
            --  is an actual subtype.

            if Is_Array_Type (Etype (N))
              and then Is_Constrained (Etype (N))
            then
               return Empty;
            else
               Remove_Side_Effects (P);
               return Build_Actual_Subtype (T, N);
            end if;
         else
            return Empty;
         end if;
      end if;

      if Ekind (T) = E_Access_Subtype then
         Deaccessed_T := Designated_Type (T);
      else
         Deaccessed_T := T;
      end if;

      if Ekind (Deaccessed_T) = E_Array_Subtype then
         Id := First_Index (Deaccessed_T);

         while Present (Id) loop
            Indx_Type := Underlying_Type (Etype (Id));

            if Denotes_Discriminant (Type_Low_Bound  (Indx_Type)) or else
               Denotes_Discriminant (Type_High_Bound (Indx_Type))
            then
               Remove_Side_Effects (P);
               return
                 Build_Component_Subtype (
                   Build_Actual_Array_Constraint, Loc, Base_Type (T));
            end if;

            Next_Index (Id);
         end loop;

      elsif Is_Composite_Type (Deaccessed_T)
        and then Has_Discriminants (Deaccessed_T)
        and then not Has_Unknown_Discriminants (Deaccessed_T)
      then
         D := First_Elmt (Discriminant_Constraint (Deaccessed_T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               Remove_Side_Effects (P);
               return
                 Build_Component_Subtype (
                   Build_Actual_Record_Constraint, Loc, Base_Type (T));
            end if;

            Next_Elmt (D);
         end loop;
      end if;

      --  If none of the above, the actual and nominal subtypes are the same

      return Empty;
   end Build_Actual_Subtype_Of_Component;

   -----------------------------
   -- Build_Component_Subtype --
   -----------------------------

   function Build_Component_Subtype
     (C   : List_Id;
      Loc : Source_Ptr;
      T   : Entity_Id) return Node_Id
   is
      Subt : Entity_Id;
      Decl : Node_Id;

   begin
      --  Unchecked_Union components do not require component subtypes

      if Is_Unchecked_Union (T) then
         return Empty;
      end if;

      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Base_Type (T),  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => C)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Component_Subtype;

   --------------------------------------------
   -- Build_Discriminal_Subtype_Of_Component --
   --------------------------------------------

   function Build_Discriminal_Subtype_Of_Component
     (T : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (T);
      D   : Elmt_Id;
      Id  : Node_Id;

      function Build_Discriminal_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      function Build_Discriminal_Record_Constraint return List_Id;
      --  Similar to previous one, for discriminated components constrained
      --  by the discriminant of the enclosing object.

      ----------------------------------------
      -- Build_Discriminal_Array_Constraint --
      ----------------------------------------

      function Build_Discriminal_Array_Constraint return List_Id is
         Constraints : constant List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

            if Denotes_Discriminant (Old_Lo) then
               Lo := New_Occurrence_Of (Discriminal (Entity (Old_Lo)), Loc);

            else
               Lo := New_Copy_Tree (Old_Lo);
            end if;

            if Denotes_Discriminant (Old_Hi) then
               Hi := New_Occurrence_Of (Discriminal (Entity (Old_Hi)), Loc);

            else
               Hi := New_Copy_Tree (Old_Hi);
            end if;

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Discriminal_Array_Constraint;

      -----------------------------------------
      -- Build_Discriminal_Record_Constraint --
      -----------------------------------------

      function Build_Discriminal_Record_Constraint return List_Id is
         Constraints : constant List_Id := New_List;
         D           : Elmt_Id;
         D_Val       : Node_Id;

      begin
         D := First_Elmt (Discriminant_Constraint (T));
         while Present (D) loop
            if Denotes_Discriminant (Node (D)) then
               D_Val :=
                 New_Occurrence_Of (Discriminal (Entity (Node (D))), Loc);

            else
               D_Val := New_Copy_Tree (Node (D));
            end if;

            Append (D_Val, Constraints);
            Next_Elmt (D);
         end loop;

         return Constraints;
      end Build_Discriminal_Record_Constraint;

   --  Start of processing for Build_Discriminal_Subtype_Of_Component

   begin
      if Ekind (T) = E_Array_Subtype then
         Id := First_Index (T);

         while Present (Id) loop
            if Denotes_Discriminant (Type_Low_Bound  (Etype (Id))) or else
               Denotes_Discriminant (Type_High_Bound (Etype (Id)))
            then
               return Build_Component_Subtype
                 (Build_Discriminal_Array_Constraint, Loc, T);
            end if;

            Next_Index (Id);
         end loop;

      elsif Ekind (T) = E_Record_Subtype
        and then Has_Discriminants (T)
        and then not Has_Unknown_Discriminants (T)
      then
         D := First_Elmt (Discriminant_Constraint (T));
         while Present (D) loop
            if Denotes_Discriminant (Node (D)) then
               return Build_Component_Subtype
                 (Build_Discriminal_Record_Constraint, Loc, T);
            end if;

            Next_Elmt (D);
         end loop;
      end if;

      --  If none of the above, the actual and nominal subtypes are the same

      return Empty;
   end Build_Discriminal_Subtype_Of_Component;

   ------------------------------
   -- Build_Elaboration_Entity --
   ------------------------------

   procedure Build_Elaboration_Entity (N : Node_Id; Spec_Id : Entity_Id) is
      Loc       : constant Source_Ptr       := Sloc (N);
      Unum      : constant Unit_Number_Type := Get_Source_Unit (Loc);
      Decl      : Node_Id;
      P         : Natural;
      Elab_Ent  : Entity_Id;

   begin
      --  Ignore if already constructed

      if Present (Elaboration_Entity (Spec_Id)) then
         return;
      end if;

      --  Construct name of elaboration entity as xxx_E, where xxx
      --  is the unit name with dots replaced by double underscore.
      --  We have to manually construct this name, since it will
      --  be elaborated in the outer scope, and thus will not have
      --  the unit name automatically prepended.

      Get_Name_String (Unit_Name (Unum));

      --  Replace the %s by _E

      Name_Buffer (Name_Len - 1 .. Name_Len) := "_E";

      --  Replace dots by double underscore

      P := 2;
      while P < Name_Len - 2 loop
         if Name_Buffer (P) = '.' then
            Name_Buffer (P + 2 .. Name_Len + 1) :=
              Name_Buffer (P + 1 .. Name_Len);
            Name_Len := Name_Len + 1;
            Name_Buffer (P) := '_';
            Name_Buffer (P + 1) := '_';
            P := P + 3;
         else
            P := P + 1;
         end if;
      end loop;

      --  Create elaboration flag

      Elab_Ent :=
        Make_Defining_Identifier (Loc, Chars => Name_Find);
      Set_Elaboration_Entity (Spec_Id, Elab_Ent);

      if No (Declarations (Aux_Decls_Node (N))) then
         Set_Declarations (Aux_Decls_Node (N), New_List);
      end if;

      Decl :=
         Make_Object_Declaration (Loc,
           Defining_Identifier => Elab_Ent,
           Object_Definition   =>
             New_Occurrence_Of (Standard_Boolean, Loc),
           Expression          =>
             New_Occurrence_Of (Standard_False, Loc));

      Append_To (Declarations (Aux_Decls_Node (N)), Decl);
      Analyze (Decl);

      --  Reset True_Constant indication, since we will indeed
      --  assign a value to the variable in the binder main.

      Set_Is_True_Constant (Elab_Ent, False);
      Set_Current_Value    (Elab_Ent, Empty);

      --  We do not want any further qualification of the name (if we did
      --  not do this, we would pick up the name of the generic package
      --  in the case of a library level generic instantiation).

      Set_Has_Qualified_Name       (Elab_Ent);
      Set_Has_Fully_Qualified_Name (Elab_Ent);
   end Build_Elaboration_Entity;

   -----------------------------------
   -- Cannot_Raise_Constraint_Error --
   -----------------------------------

   function Cannot_Raise_Constraint_Error (Expr : Node_Id) return Boolean is
   begin
      if Compile_Time_Known_Value (Expr) then
         return True;

      elsif Do_Range_Check (Expr) then
         return False;

      elsif Raises_Constraint_Error (Expr) then
         return False;

      else
         case Nkind (Expr) is
            when N_Identifier =>
               return True;

            when N_Expanded_Name =>
               return True;

            when N_Selected_Component =>
               return not Do_Discriminant_Check (Expr);

            when N_Attribute_Reference =>
               if Do_Overflow_Check (Expr) then
                  return False;

               elsif No (Expressions (Expr)) then
                  return True;

               else
                  declare
                     N : Node_Id := First (Expressions (Expr));

                  begin
                     while Present (N) loop
                        if Cannot_Raise_Constraint_Error (N) then
                           Next (N);
                        else
                           return False;
                        end if;
                     end loop;

                     return True;
                  end;
               end if;

            when N_Type_Conversion =>
               if Do_Overflow_Check (Expr)
                 or else Do_Length_Check (Expr)
                 or else Do_Tag_Check (Expr)
               then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Expression (Expr));
               end if;

            when N_Unchecked_Type_Conversion =>
               return Cannot_Raise_Constraint_Error (Expression (Expr));

            when N_Unary_Op =>
               if Do_Overflow_Check (Expr) then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when N_Op_Divide |
                 N_Op_Mod    |
                 N_Op_Rem
            =>
               if Do_Division_Check (Expr)
                 or else Do_Overflow_Check (Expr)
               then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Left_Opnd (Expr))
                      and then
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when N_Op_Add                    |
                 N_Op_And                    |
                 N_Op_Concat                 |
                 N_Op_Eq                     |
                 N_Op_Expon                  |
                 N_Op_Ge                     |
                 N_Op_Gt                     |
                 N_Op_Le                     |
                 N_Op_Lt                     |
                 N_Op_Multiply               |
                 N_Op_Ne                     |
                 N_Op_Or                     |
                 N_Op_Rotate_Left            |
                 N_Op_Rotate_Right           |
                 N_Op_Shift_Left             |
                 N_Op_Shift_Right            |
                 N_Op_Shift_Right_Arithmetic |
                 N_Op_Subtract               |
                 N_Op_Xor
            =>
               if Do_Overflow_Check (Expr) then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Left_Opnd (Expr))
                      and then
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when others =>
               return False;
         end case;
      end if;
   end Cannot_Raise_Constraint_Error;

   --------------------------
   -- Check_Fully_Declared --
   --------------------------

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id) is
   begin
      if Ekind (T) = E_Incomplete_Type then

         --  Ada 2005 (AI-50217): If the type is available through a limited
         --  with_clause, verify that its full view has been analyzed.

         if From_With_Type (T)
           and then Present (Non_Limited_View (T))
           and then Ekind (Non_Limited_View (T)) /= E_Incomplete_Type
         then
            --  The non-limited view is fully declared
            null;

         else
            Error_Msg_NE
              ("premature usage of incomplete}", N, First_Subtype (T));
         end if;

      elsif Has_Private_Component (T)
        and then not Is_Generic_Type (Root_Type (T))
        and then not In_Default_Expression
      then

         --  Special case: if T is the anonymous type created for a single
         --  task or protected object, use the name of the source object.

         if Is_Concurrent_Type (T)
           and then not Comes_From_Source (T)
           and then Nkind (N) = N_Object_Declaration
         then
            Error_Msg_NE ("type of& has incomplete component", N,
              Defining_Identifier (N));

         else
            Error_Msg_NE
              ("premature usage of incomplete}", N, First_Subtype (T));
         end if;
      end if;
   end Check_Fully_Declared;

   -----------------------
   -- Check_Obsolescent --
   -----------------------

   procedure Check_Obsolescent (Nam : Entity_Id; N : Node_Id) is
      W : Node_Id;

   begin
      --  Note that we always allow obsolescent references in the compiler
      --  itself and the run time, since we assume that we know what we are
      --  doing in such cases. For example the calls in Ada.Characters.Handling
      --  to its own obsolescent subprograms are just fine.

      if Is_Obsolescent (Nam) and then not GNAT_Mode then
         Check_Restriction (No_Obsolescent_Features, N);

         if Warn_On_Obsolescent_Feature then
            if Is_Package_Or_Generic_Package (Nam) then
               Error_Msg_NE ("with of obsolescent package&?", N, Nam);
            else
               Error_Msg_NE ("call to obsolescent subprogram&?", N, Nam);
            end if;

            --  Output additional warning if present

            W := Obsolescent_Warning (Nam);

            if Present (W) then
               Name_Buffer (1) := '|';
               Name_Buffer (2) := '?';
               Name_Len := 2;

               --  Add characters to message, and output message

               for J in 1 .. String_Length (Strval (W)) loop
                  Add_Char_To_Name_Buffer (''');
                  Add_Char_To_Name_Buffer
                    (Get_Character (Get_String_Char (Strval (W), J)));
               end loop;

               Error_Msg_N (Name_Buffer (1 .. Name_Len), N);
            end if;
         end if;
      end if;
   end Check_Obsolescent;

   ------------------------------------------
   -- Check_Potentially_Blocking_Operation --
   ------------------------------------------

   procedure Check_Potentially_Blocking_Operation (N : Node_Id) is
      S   : Entity_Id;

   begin
      --  N is one of the potentially blocking operations listed in 9.5.1(8).
      --  When pragma Detect_Blocking is active, the run time will raise
      --  Program_Error. Here we only issue a warning, since we generally
      --  support the use of potentially blocking operations in the absence
      --  of the pragma.

      --  Indirect blocking through a subprogram call cannot be diagnosed
      --  statically without interprocedural analysis, so we do not attempt
      --  to do it here.

      S := Scope (Current_Scope);
      while Present (S) and then S /= Standard_Standard loop
         if Is_Protected_Type (S) then
            Error_Msg_N
              ("potentially blocking operation in protected operation?", N);

            return;
         end if;

         S := Scope (S);
      end loop;
   end Check_Potentially_Blocking_Operation;

   ---------------
   -- Check_VMS --
   ---------------

   procedure Check_VMS (Construct : Node_Id) is
   begin
      if not OpenVMS_On_Target then
         Error_Msg_N
           ("this construct is allowed only in Open'V'M'S", Construct);
      end if;
   end Check_VMS;

   ----------------------------------
   -- Collect_Primitive_Operations --
   ----------------------------------

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id is
      B_Type         : constant Entity_Id := Base_Type (T);
      B_Decl         : constant Node_Id   := Original_Node (Parent (B_Type));
      B_Scope        : Entity_Id          := Scope (B_Type);
      Op_List        : Elist_Id;
      Formal         : Entity_Id;
      Is_Prim        : Boolean;
      Formal_Derived : Boolean := False;
      Id             : Entity_Id;

   begin
      --  For tagged types, the primitive operations are collected as they
      --  are declared, and held in an explicit list which is simply returned.

      if Is_Tagged_Type (B_Type) then
         return Primitive_Operations (B_Type);

      --  An untagged generic type that is a derived type inherits the
      --  primitive operations of its parent type. Other formal types only
      --  have predefined operators, which are not explicitly represented.

      elsif Is_Generic_Type (B_Type) then
         if Nkind (B_Decl) = N_Formal_Type_Declaration
           and then Nkind (Formal_Type_Definition (B_Decl))
             = N_Formal_Derived_Type_Definition
         then
            Formal_Derived := True;
         else
            return New_Elmt_List;
         end if;
      end if;

      Op_List := New_Elmt_List;

      if B_Scope = Standard_Standard then
         if B_Type = Standard_String then
            Append_Elmt (Standard_Op_Concat, Op_List);

         elsif B_Type = Standard_Wide_String then
            Append_Elmt (Standard_Op_Concatw, Op_List);

         else
            null;
         end if;

      elsif (Is_Package_Or_Generic_Package (B_Scope)
              and then
                Nkind (Parent (Declaration_Node (First_Subtype (T)))) /=
                                                            N_Package_Body)
        or else Is_Derived_Type (B_Type)
      then
         --  The primitive operations appear after the base type, except
         --  if the derivation happens within the private part of B_Scope
         --  and the type is a private type, in which case both the type
         --  and some primitive operations may appear before the base
         --  type, and the list of candidates starts after the type.

         if In_Open_Scopes (B_Scope)
           and then Scope (T) = B_Scope
           and then In_Private_Part (B_Scope)
         then
            Id := Next_Entity (T);
         else
            Id := Next_Entity (B_Type);
         end if;

         while Present (Id) loop

            --  Note that generic formal subprograms are not
            --  considered to be primitive operations and thus
            --  are never inherited.

            if Is_Overloadable (Id)
              and then Nkind (Parent (Parent (Id)))
                         not in N_Formal_Subprogram_Declaration
            then
               Is_Prim := False;

               if Base_Type (Etype (Id)) = B_Type then
                  Is_Prim := True;
               else
                  Formal := First_Formal (Id);
                  while Present (Formal) loop
                     if Base_Type (Etype (Formal)) = B_Type then
                        Is_Prim := True;
                        exit;

                     elsif Ekind (Etype (Formal)) = E_Anonymous_Access_Type
                       and then Base_Type
                         (Designated_Type (Etype (Formal))) = B_Type
                     then
                        Is_Prim := True;
                        exit;
                     end if;

                     Next_Formal (Formal);
                  end loop;
               end if;

               --  For a formal derived type, the only primitives are the
               --  ones inherited from the parent type. Operations appearing
               --  in the package declaration are not primitive for it.

               if Is_Prim
                 and then (not Formal_Derived
                            or else Present (Alias (Id)))
               then
                  Append_Elmt (Id, Op_List);
               end if;
            end if;

            Next_Entity (Id);

            --  For a type declared in System, some of its operations
            --  may appear in  the target-specific extension to System.

            if No (Id)
              and then Chars (B_Scope) = Name_System
              and then Scope (B_Scope) = Standard_Standard
              and then Present_System_Aux
            then
               B_Scope := System_Aux_Id;
               Id := First_Entity (System_Aux_Id);
            end if;
         end loop;
      end if;

      return Op_List;
   end Collect_Primitive_Operations;

   -----------------------------------
   -- Compile_Time_Constraint_Error --
   -----------------------------------

   function Compile_Time_Constraint_Error
     (N    : Node_Id;
      Msg  : String;
      Ent  : Entity_Id  := Empty;
      Loc  : Source_Ptr := No_Location;
      Warn : Boolean  := False) return Node_Id
   is
      Msgc : String (1 .. Msg'Length + 2);
      Msgl : Natural;
      Wmsg : Boolean;
      P    : Node_Id;
      OldP : Node_Id;
      Msgs : Boolean;
      Eloc : Source_Ptr;

   begin
      --  A static constraint error in an instance body is not a fatal error.
      --  we choose to inhibit the message altogether, because there is no
      --  obvious node (for now) on which to post it. On the other hand the
      --  offending node must be replaced with a constraint_error in any case.

      --  No messages are generated if we already posted an error on this node

      if not Error_Posted (N) then
         if Loc /= No_Location then
            Eloc := Loc;
         else
            Eloc := Sloc (N);
         end if;

         --  Make all such messages unconditional

         Msgc (1 .. Msg'Length) := Msg;
         Msgc (Msg'Length + 1) := '!';
         Msgl := Msg'Length + 1;

         --  Message is a warning, even in Ada 95 case

         if Msg (Msg'Length) = '?' then
            Wmsg := True;

         --  In Ada 83, all messages are warnings. In the private part and
         --  the body of an instance, constraint_checks are only warnings.
         --  We also make this a warning if the Warn parameter is set.

         elsif Warn
           or else (Ada_Version = Ada_83 and then Comes_From_Source (N))
         then
            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Wmsg := True;

         elsif In_Instance_Not_Visible then
            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Wmsg := True;

         --  Otherwise we have a real error message (Ada 95 static case)

         else
            Wmsg := False;
         end if;

         --  Should we generate a warning? The answer is not quite yes. The
         --  very annoying exception occurs in the case of a short circuit
         --  operator where the left operand is static and decisive. Climb
         --  parents to see if that is the case we have here. Conditional
         --  expressions with decisive conditions are a similar situation.

         Msgs := True;
         P := N;
         loop
            OldP := P;
            P := Parent (P);

            --  And then with False as left operand

            if Nkind (P) = N_And_Then
              and then Compile_Time_Known_Value (Left_Opnd (P))
              and then Is_False (Expr_Value (Left_Opnd (P)))
            then
               Msgs := False;
               exit;

            --  OR ELSE with True as left operand

            elsif Nkind (P) = N_Or_Else
              and then Compile_Time_Known_Value (Left_Opnd (P))
              and then Is_True (Expr_Value (Left_Opnd (P)))
            then
               Msgs := False;
               exit;

            --  Conditional expression

            elsif Nkind (P) = N_Conditional_Expression then
               declare
                  Cond : constant Node_Id := First (Expressions (P));
                  Texp : constant Node_Id := Next (Cond);
                  Fexp : constant Node_Id := Next (Texp);

               begin
                  if Compile_Time_Known_Value (Cond) then

                     --  Condition is True and we are in the right operand

                     if Is_True (Expr_Value (Cond))
                       and then OldP = Fexp
                     then
                        Msgs := False;
                        exit;

                     --  Condition is False and we are in the left operand

                     elsif Is_False (Expr_Value (Cond))
                       and then OldP = Texp
                     then
                        Msgs := False;
                        exit;
                     end if;
                  end if;
               end;

            --  Special case for component association in aggregates, where
            --  we want to keep climbing up to the parent aggregate.

            elsif Nkind (P) = N_Component_Association
              and then Nkind (Parent (P)) = N_Aggregate
            then
               null;

            --  Keep going if within subexpression

            else
               exit when Nkind (P) not in N_Subexpr;
            end if;
         end loop;

         if Msgs then
            if Present (Ent) then
               Error_Msg_NEL (Msgc (1 .. Msgl), N, Ent, Eloc);
            else
               Error_Msg_NEL (Msgc (1 .. Msgl), N, Etype (N), Eloc);
            end if;

            if Wmsg then
               if Inside_Init_Proc then
                  Error_Msg_NEL
                    ("\?& will be raised for objects of this type",
                     N, Standard_Constraint_Error, Eloc);
               else
                  Error_Msg_NEL
                    ("\?& will be raised at run time",
                     N, Standard_Constraint_Error, Eloc);
               end if;
            else
               Error_Msg_NEL
                 ("\static expression raises&!",
                  N, Standard_Constraint_Error, Eloc);
            end if;
         end if;
      end if;

      return N;
   end Compile_Time_Constraint_Error;

   -----------------------
   -- Conditional_Delay --
   -----------------------

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id) is
   begin
      if Has_Delayed_Freeze (Old_Ent) and then not Is_Frozen (Old_Ent) then
         Set_Has_Delayed_Freeze (New_Ent);
      end if;
   end Conditional_Delay;

   --------------------
   -- Current_Entity --
   --------------------

   --  The currently visible definition for a given identifier is the
   --  one most chained at the start of the visibility chain, i.e. the
   --  one that is referenced by the Node_Id value of the name of the
   --  given identifier.

   function Current_Entity (N : Node_Id) return Entity_Id is
   begin
      return Get_Name_Entity_Id (Chars (N));
   end Current_Entity;

   -----------------------------
   -- Current_Entity_In_Scope --
   -----------------------------

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id is
      E  : Entity_Id;
      CS : constant Entity_Id := Current_Scope;

      Transient_Case : constant Boolean := Scope_Is_Transient;

   begin
      E := Get_Name_Entity_Id (Chars (N));

      while Present (E)
        and then Scope (E) /= CS
        and then (not Transient_Case or else Scope (E) /= Scope (CS))
      loop
         E := Homonym (E);
      end loop;

      return E;
   end Current_Entity_In_Scope;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Entity_Id is
   begin
      if Scope_Stack.Last = -1 then
         return Standard_Standard;
      else
         declare
            C : constant Entity_Id :=
                  Scope_Stack.Table (Scope_Stack.Last).Entity;
         begin
            if Present (C) then
               return C;
            else
               return Standard_Standard;
            end if;
         end;
      end if;
   end Current_Scope;

   ------------------------
   -- Current_Subprogram --
   ------------------------

   function Current_Subprogram return Entity_Id is
      Scop : constant Entity_Id := Current_Scope;

   begin
      if Is_Subprogram (Scop) or else Is_Generic_Subprogram (Scop) then
         return Scop;
      else
         return Enclosing_Subprogram (Scop);
      end if;
   end Current_Subprogram;

   ---------------------
   -- Defining_Entity --
   ---------------------

   function Defining_Entity (N : Node_Id) return Entity_Id is
      K   : constant Node_Kind := Nkind (N);
      Err : Entity_Id := Empty;

   begin
      case K is
         when
           N_Subprogram_Declaration                 |
           N_Abstract_Subprogram_Declaration        |
           N_Subprogram_Body                        |
           N_Package_Declaration                    |
           N_Subprogram_Renaming_Declaration        |
           N_Subprogram_Body_Stub                   |
           N_Generic_Subprogram_Declaration         |
           N_Generic_Package_Declaration            |
           N_Formal_Subprogram_Declaration
         =>
            return Defining_Entity (Specification (N));

         when
           N_Component_Declaration                  |
           N_Defining_Program_Unit_Name             |
           N_Discriminant_Specification             |
           N_Entry_Body                             |
           N_Entry_Declaration                      |
           N_Entry_Index_Specification              |
           N_Exception_Declaration                  |
           N_Exception_Renaming_Declaration         |
           N_Formal_Object_Declaration              |
           N_Formal_Package_Declaration             |
           N_Formal_Type_Declaration                |
           N_Full_Type_Declaration                  |
           N_Implicit_Label_Declaration             |
           N_Incomplete_Type_Declaration            |
           N_Loop_Parameter_Specification           |
           N_Number_Declaration                     |
           N_Object_Declaration                     |
           N_Object_Renaming_Declaration            |
           N_Package_Body_Stub                      |
           N_Parameter_Specification                |
           N_Private_Extension_Declaration          |
           N_Private_Type_Declaration               |
           N_Protected_Body                         |
           N_Protected_Body_Stub                    |
           N_Protected_Type_Declaration             |
           N_Single_Protected_Declaration           |
           N_Single_Task_Declaration                |
           N_Subtype_Declaration                    |
           N_Task_Body                              |
           N_Task_Body_Stub                         |
           N_Task_Type_Declaration
         =>
            return Defining_Identifier (N);

         when N_Subunit =>
            return Defining_Entity (Proper_Body (N));

         when
           N_Function_Instantiation                 |
           N_Function_Specification                 |
           N_Generic_Function_Renaming_Declaration  |
           N_Generic_Package_Renaming_Declaration   |
           N_Generic_Procedure_Renaming_Declaration |
           N_Package_Body                           |
           N_Package_Instantiation                  |
           N_Package_Renaming_Declaration           |
           N_Package_Specification                  |
           N_Procedure_Instantiation                |
           N_Procedure_Specification
         =>
            declare
               Nam : constant Node_Id := Defining_Unit_Name (N);

            begin
               if Nkind (Nam) in N_Entity then
                  return Nam;

               --  For Error, make up a name and attach to declaration
               --  so we can continue semantic analysis

               elsif Nam = Error then
                  Err :=
                    Make_Defining_Identifier (Sloc (N),
                      Chars => New_Internal_Name ('T'));
                  Set_Defining_Unit_Name (N, Err);

                  return Err;
               --  If not an entity, get defining identifier

               else
                  return Defining_Identifier (Nam);
               end if;
            end;

         when N_Block_Statement =>
            return Entity (Identifier (N));

         when others =>
            raise Program_Error;

      end case;
   end Defining_Entity;

   --------------------------
   -- Denotes_Discriminant --
   --------------------------

   function Denotes_Discriminant
     (N               : Node_Id;
      Check_Protected : Boolean := False) return Boolean
   is
      E : Entity_Id;
   begin
      if not Is_Entity_Name (N)
        or else No (Entity (N))
      then
         return False;
      else
         E := Entity (N);
      end if;

      --  If we are checking for a protected type, the discriminant may have
      --  been rewritten as the corresponding discriminal of the original type
      --  or of the corresponding concurrent record, depending on whether we
      --  are in the spec or body of the protected type.

      return Ekind (E) = E_Discriminant
        or else
          (Check_Protected
            and then Ekind (E) = E_In_Parameter
            and then Present (Discriminal_Link (E))
            and then
              (Is_Protected_Type (Scope (Discriminal_Link (E)))
                or else
                  Is_Concurrent_Record_Type (Scope (Discriminal_Link (E)))));

   end Denotes_Discriminant;

   -----------------------------
   -- Depends_On_Discriminant --
   -----------------------------

   function Depends_On_Discriminant (N : Node_Id) return Boolean is
      L : Node_Id;
      H : Node_Id;

   begin
      Get_Index_Bounds (N, L, H);
      return Denotes_Discriminant (L) or else Denotes_Discriminant (H);
   end Depends_On_Discriminant;

   -------------------------
   -- Designate_Same_Unit --
   -------------------------

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id) return Boolean
   is
      K1 : constant Node_Kind := Nkind (Name1);
      K2 : constant Node_Kind := Nkind (Name2);

      function Prefix_Node (N : Node_Id) return Node_Id;
      --  Returns the parent unit name node of a defining program unit name
      --  or the prefix if N is a selected component or an expanded name.

      function Select_Node (N : Node_Id) return Node_Id;
      --  Returns the defining identifier node of a defining program unit
      --  name or  the selector node if N is a selected component or an
      --  expanded name.

      -----------------
      -- Prefix_Node --
      -----------------

      function Prefix_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Name (N);

         else
            return Prefix (N);
         end if;
      end Prefix_Node;

      -----------------
      -- Select_Node --
      -----------------

      function Select_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Defining_Identifier (N);

         else
            return Selector_Name (N);
         end if;
      end Select_Node;

   --  Start of processing for Designate_Next_Unit

   begin
      if (K1 = N_Identifier or else
          K1 = N_Defining_Identifier)
        and then
         (K2 = N_Identifier or else
          K2 = N_Defining_Identifier)
      then
         return Chars (Name1) = Chars (Name2);

      elsif
         (K1 = N_Expanded_Name      or else
          K1 = N_Selected_Component or else
          K1 = N_Defining_Program_Unit_Name)
        and then
         (K2 = N_Expanded_Name      or else
          K2 = N_Selected_Component or else
          K2 = N_Defining_Program_Unit_Name)
      then
         return
           (Chars (Select_Node (Name1)) = Chars (Select_Node (Name2)))
             and then
               Designate_Same_Unit (Prefix_Node (Name1), Prefix_Node (Name2));

      else
         return False;
      end if;
   end Designate_Same_Unit;

   ----------------------------
   -- Enclosing_Generic_Body --
   ----------------------------

   function Enclosing_Generic_Body
     (N : Node_Id) return Node_Id
   is
      P    : Node_Id;
      Decl : Node_Id;
      Spec : Node_Id;

   begin
      P := Parent (N);
      while Present (P) loop
         if Nkind (P) = N_Package_Body
           or else Nkind (P) = N_Subprogram_Body
         then
            Spec := Corresponding_Spec (P);

            if Present (Spec) then
               Decl := Unit_Declaration_Node (Spec);

               if Nkind (Decl) = N_Generic_Package_Declaration
                 or else Nkind (Decl) = N_Generic_Subprogram_Declaration
               then
                  return P;
               end if;
            end if;
         end if;

         P := Parent (P);
      end loop;

      return Empty;
   end Enclosing_Generic_Body;

   ----------------------------
   -- Enclosing_Generic_Unit --
   ----------------------------

   function Enclosing_Generic_Unit
     (N : Node_Id) return Node_Id
   is
      P    : Node_Id;
      Decl : Node_Id;
      Spec : Node_Id;

   begin
      P := Parent (N);
      while Present (P) loop
         if Nkind (P) = N_Generic_Package_Declaration
           or else Nkind (P) = N_Generic_Subprogram_Declaration
         then
            return P;

         elsif Nkind (P) = N_Package_Body
           or else Nkind (P) = N_Subprogram_Body
         then
            Spec := Corresponding_Spec (P);

            if Present (Spec) then
               Decl := Unit_Declaration_Node (Spec);

               if Nkind (Decl) = N_Generic_Package_Declaration
                 or else Nkind (Decl) = N_Generic_Subprogram_Declaration
               then
                  return Decl;
               end if;
            end if;
         end if;

         P := Parent (P);
      end loop;

      return Empty;
   end Enclosing_Generic_Unit;

   -------------------------------
   -- Enclosing_Lib_Unit_Entity --
   -------------------------------

   function Enclosing_Lib_Unit_Entity return Entity_Id is
      Unit_Entity : Entity_Id := Current_Scope;

   begin
      --  Look for enclosing library unit entity by following scope links.
      --  Equivalent to, but faster than indexing through the scope stack.

      while (Present (Scope (Unit_Entity))
        and then Scope (Unit_Entity) /= Standard_Standard)
        and not Is_Child_Unit (Unit_Entity)
      loop
         Unit_Entity := Scope (Unit_Entity);
      end loop;

      return Unit_Entity;
   end Enclosing_Lib_Unit_Entity;

   -----------------------------
   -- Enclosing_Lib_Unit_Node --
   -----------------------------

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id is
      Current_Node : Node_Id := N;

   begin
      while Present (Current_Node)
        and then Nkind (Current_Node) /= N_Compilation_Unit
      loop
         Current_Node := Parent (Current_Node);
      end loop;

      if Nkind (Current_Node) /= N_Compilation_Unit then
         return Empty;
      end if;

      return Current_Node;
   end Enclosing_Lib_Unit_Node;

   --------------------------
   -- Enclosing_Subprogram --
   --------------------------

   function Enclosing_Subprogram (E : Entity_Id) return Entity_Id is
      Dynamic_Scope : constant Entity_Id := Enclosing_Dynamic_Scope (E);

   begin
      if Dynamic_Scope = Standard_Standard then
         return Empty;

      elsif Ekind (Dynamic_Scope) = E_Subprogram_Body then
         return Corresponding_Spec (Parent (Parent (Dynamic_Scope)));

      elsif Ekind (Dynamic_Scope) = E_Block then
         return Enclosing_Subprogram (Dynamic_Scope);

      elsif Ekind (Dynamic_Scope) = E_Task_Type then
         return Get_Task_Body_Procedure (Dynamic_Scope);

      elsif Convention (Dynamic_Scope) = Convention_Protected then
         return Protected_Body_Subprogram (Dynamic_Scope);

      else
         return Dynamic_Scope;
      end if;
   end Enclosing_Subprogram;

   ------------------------
   -- Ensure_Freeze_Node --
   ------------------------

   procedure Ensure_Freeze_Node (E : Entity_Id) is
      FN : Node_Id;

   begin
      if No (Freeze_Node (E)) then
         FN := Make_Freeze_Entity (Sloc (E));
         Set_Has_Delayed_Freeze (E);
         Set_Freeze_Node (E, FN);
         Set_Access_Types_To_Process (FN, No_Elist);
         Set_TSS_Elist (FN, No_Elist);
         Set_Entity (FN, E);
      end if;
   end Ensure_Freeze_Node;

   ----------------
   -- Enter_Name --
   ----------------

   procedure Enter_Name (Def_Id : Entity_Id) is
      C : constant Entity_Id := Current_Entity (Def_Id);
      E : constant Entity_Id := Current_Entity_In_Scope (Def_Id);
      S : constant Entity_Id := Current_Scope;

      function Is_Private_Component_Renaming (N : Node_Id) return Boolean;
      --  Recognize a renaming declaration that is introduced for private
      --  components of a protected type. We treat these as weak declarations
      --  so that they are overridden by entities with the same name that
      --  come from source, such as formals or local variables of a given
      --  protected declaration.

      -----------------------------------
      -- Is_Private_Component_Renaming --
      -----------------------------------

      function Is_Private_Component_Renaming (N : Node_Id) return Boolean is
      begin
         return not Comes_From_Source (N)
           and then not Comes_From_Source (Current_Scope)
           and then Nkind (N) = N_Object_Renaming_Declaration;
      end Is_Private_Component_Renaming;

   --  Start of processing for Enter_Name

   begin
      Generate_Definition (Def_Id);

      --  Add new name to current scope declarations. Check for duplicate
      --  declaration, which may or may not be a genuine error.

      if Present (E) then

         --  Case of previous entity entered because of a missing declaration
         --  or else a bad subtype indication. Best is to use the new entity,
         --  and make the previous one invisible.

         if Etype (E) = Any_Type then
            Set_Is_Immediately_Visible (E, False);

         --  Case of renaming declaration constructed for package instances.
         --  if there is an explicit declaration with the same identifier,
         --  the renaming is not immediately visible any longer, but remains
         --  visible through selected component notation.

         elsif Nkind (Parent (E)) = N_Package_Renaming_Declaration
           and then not Comes_From_Source (E)
         then
            Set_Is_Immediately_Visible (E, False);

         --  The new entity may be the package renaming, which has the same
         --  same name as a generic formal which has been seen already.

         elsif Nkind (Parent (Def_Id)) = N_Package_Renaming_Declaration
            and then not Comes_From_Source (Def_Id)
         then
            Set_Is_Immediately_Visible (E, False);

         --  For a fat pointer corresponding to a remote access to subprogram,
         --  we use the same identifier as the RAS type, so that the proper
         --  name appears in the stub. This type is only retrieved through
         --  the RAS type and never by visibility, and is not added to the
         --  visibility list (see below).

         elsif Nkind (Parent (Def_Id)) = N_Full_Type_Declaration
           and then Present (Corresponding_Remote_Type (Def_Id))
         then
            null;

         --  A controller component for a type extension overrides the
         --  inherited component.

         elsif Chars (E) = Name_uController then
            null;

         --  Case of an implicit operation or derived literal. The new entity
         --  hides the implicit one,  which is removed from all visibility,
         --  i.e. the entity list of its scope, and homonym chain of its name.

         elsif (Is_Overloadable (E) and then Is_Inherited_Operation (E))
           or else Is_Internal (E)
         then
            declare
               Prev     : Entity_Id;
               Prev_Vis : Entity_Id;
               Decl     : constant Node_Id := Parent (E);

            begin
               --  If E is an implicit declaration, it cannot be the first
               --  entity in the scope.

               Prev := First_Entity (Current_Scope);

               while Present (Prev)
                 and then Next_Entity (Prev) /= E
               loop
                  Next_Entity (Prev);
               end loop;

               if No (Prev) then

                  --  If E is not on the entity chain of the current scope,
                  --  it is an implicit declaration in the generic formal
                  --  part of a generic subprogram. When analyzing the body,
                  --  the generic formals are visible but not on the entity
                  --  chain of the subprogram. The new entity will become
                  --  the visible one in the body.

                  pragma Assert
                    (Nkind (Parent (Decl)) = N_Generic_Subprogram_Declaration);
                  null;

               else
                  Set_Next_Entity (Prev, Next_Entity (E));

                  if No (Next_Entity (Prev)) then
                     Set_Last_Entity (Current_Scope, Prev);
                  end if;

                  if E = Current_Entity (E) then
                     Prev_Vis := Empty;

                  else
                     Prev_Vis := Current_Entity (E);
                     while Homonym (Prev_Vis) /= E loop
                        Prev_Vis := Homonym (Prev_Vis);
                     end loop;
                  end if;

                  if Present (Prev_Vis)  then

                     --  Skip E in the visibility chain

                     Set_Homonym (Prev_Vis, Homonym (E));

                  else
                     Set_Name_Entity_Id (Chars (E), Homonym (E));
                  end if;
               end if;
            end;

         --  This section of code could use a comment ???

         elsif Present (Etype (E))
           and then Is_Concurrent_Type (Etype (E))
           and then E = Def_Id
         then
            return;

         elsif Is_Private_Component_Renaming (Parent (Def_Id)) then
            return;

         --  In the body or private part of an instance, a type extension
         --  may introduce a component with the same name as that of an
         --  actual. The legality rule is not enforced, but the semantics
         --  of the full type with two components of the same name are not
         --  clear at this point ???

         elsif In_Instance_Not_Visible  then
            null;

         --  When compiling a package body, some child units may have become
         --  visible. They cannot conflict with local entities that hide them.

         elsif Is_Child_Unit (E)
           and then In_Open_Scopes (Scope (E))
           and then not Is_Immediately_Visible (E)
         then
            null;

         --  Conversely, with front-end inlining we may compile the parent
         --  body first, and a child unit subsequently. The context is now
         --  the parent spec, and body entities are not visible.

         elsif Is_Child_Unit (Def_Id)
           and then Is_Package_Body_Entity (E)
           and then not In_Package_Body (Current_Scope)
         then
            null;

         --  Case of genuine duplicate declaration

         else
            Error_Msg_Sloc := Sloc (E);

            --  If the previous declaration is an incomplete type declaration
            --  this may be an attempt to complete it with a private type.
            --  The following avoids confusing cascaded errors.

            if Nkind (Parent (E)) = N_Incomplete_Type_Declaration
              and then Nkind (Parent (Def_Id)) = N_Private_Type_Declaration
            then
               Error_Msg_N
                 ("incomplete type cannot be completed" &
                        " with a private declaration",
                    Parent (Def_Id));
               Set_Is_Immediately_Visible (E, False);
               Set_Full_View (E, Def_Id);

            elsif Ekind (E) = E_Discriminant
              and then Present (Scope (Def_Id))
              and then Scope (Def_Id) /= Current_Scope
            then
               --  An inherited component of a record conflicts with
               --  a new discriminant. The discriminant is inserted first
               --  in the scope, but the error should be posted on it, not
               --  on the component.

               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Msg_N ("& conflicts with declaration#", E);
               return;

            --  If the name of the unit appears in its own context clause,
            --  a dummy package with the name has already been created, and
            --  the error emitted. Try to continue quietly.

            elsif Error_Posted (E)
              and then Sloc (E) = No_Location
              and then Nkind (Parent (E)) = N_Package_Specification
              and then Current_Scope = Standard_Standard
            then
               Set_Scope (Def_Id, Current_Scope);
               return;

            else
               Error_Msg_N ("& conflicts with declaration#", Def_Id);

               --  Avoid cascaded messages with duplicate components in
               --  derived types.

               if Ekind (E) = E_Component
                 or else Ekind (E) = E_Discriminant
               then
                  return;
               end if;
            end if;

            if Nkind (Parent (Parent (Def_Id)))
                 = N_Generic_Subprogram_Declaration
              and then Def_Id =
                Defining_Entity (Specification (Parent (Parent (Def_Id))))
            then
               Error_Msg_N ("\generic units cannot be overloaded", Def_Id);
            end if;

            --  If entity is in standard, then we are in trouble, because
            --  it means that we have a library package with a duplicated
            --  name. That's hard to recover from, so abort!

            if S = Standard_Standard then
               raise Unrecoverable_Error;

            --  Otherwise we continue with the declaration. Having two
            --  identical declarations should not cause us too much trouble!

            else
               null;
            end if;
         end if;
      end if;

      --  If we fall through, declaration is OK , or OK enough to continue

      --  If Def_Id is a discriminant or a record component we are in the
      --  midst of inheriting components in a derived record definition.
      --  Preserve their Ekind and Etype.

      if Ekind (Def_Id) = E_Discriminant
        or else Ekind (Def_Id) = E_Component
      then
         null;

      --  If a type is already set, leave it alone (happens whey a type
      --  declaration is reanalyzed following a call to the optimizer)

      elsif Present (Etype (Def_Id)) then
         null;

      --  Otherwise, the kind E_Void insures that premature uses of the entity
      --  will be detected. Any_Type insures that no cascaded errors will occur

      else
         Set_Ekind (Def_Id, E_Void);
         Set_Etype (Def_Id, Any_Type);
      end if;

      --  Inherited discriminants and components in derived record types are
      --  immediately visible. Itypes are not.

      if Ekind (Def_Id) = E_Discriminant
        or else Ekind (Def_Id) = E_Component
        or else (No (Corresponding_Remote_Type (Def_Id))
                 and then not Is_Itype (Def_Id))
      then
         Set_Is_Immediately_Visible (Def_Id);
         Set_Current_Entity         (Def_Id);
      end if;

      Set_Homonym       (Def_Id, C);
      Append_Entity     (Def_Id, S);
      Set_Public_Status (Def_Id);

      --  Warn if new entity hides an old one

      if Warn_On_Hiding
        and then Present (C)
        and then Length_Of_Name (Chars (C)) /= 1
        and then Comes_From_Source (C)
        and then Comes_From_Source (Def_Id)
        and then In_Extended_Main_Source_Unit (Def_Id)
      then
         Error_Msg_Sloc := Sloc (C);
         Error_Msg_N ("declaration hides &#?", Def_Id);
      end if;
   end Enter_Name;

   --------------------------
   -- Explain_Limited_Type --
   --------------------------

   procedure Explain_Limited_Type (T : Entity_Id; N : Node_Id) is
      C : Entity_Id;

   begin
      --  For array, component type must be limited

      if Is_Array_Type (T) then
         Error_Msg_Node_2 := T;
         Error_Msg_NE
           ("component type& of type& is limited", N, Component_Type (T));
         Explain_Limited_Type (Component_Type (T), N);

      elsif Is_Record_Type (T) then

         --  No need for extra messages if explicit limited record

         if Is_Limited_Record (Base_Type (T)) then
            return;
         end if;

         --  Otherwise find a limited component. Check only components that
         --  come from source, or inherited components that appear in the
         --  source of the ancestor.

         C := First_Component (T);
         while Present (C) loop
            if Is_Limited_Type (Etype (C))
              and then
                (Comes_From_Source (C)
                   or else
                     (Present (Original_Record_Component (C))
                       and then
                         Comes_From_Source (Original_Record_Component (C))))
            then
               Error_Msg_Node_2 := T;
               Error_Msg_NE ("\component& of type& has limited type", N, C);
               Explain_Limited_Type (Etype (C), N);
               return;
            end if;

            Next_Component (C);
         end loop;

         --  The type may be declared explicitly limited, even if no component
         --  of it is limited, in which case we fall out of the loop.
         return;
      end if;
   end Explain_Limited_Type;

   -------------------------------------
   -- Find_Corresponding_Discriminant --
   -------------------------------------

   function Find_Corresponding_Discriminant
     (Id  : Node_Id;
      Typ : Entity_Id) return Entity_Id
   is
      Par_Disc : Entity_Id;
      Old_Disc : Entity_Id;
      New_Disc : Entity_Id;

   begin
      Par_Disc := Original_Record_Component (Original_Discriminant (Id));

      --  The original type may currently be private, and the discriminant
      --  only appear on its full view.

      if Is_Private_Type (Scope (Par_Disc))
        and then not Has_Discriminants (Scope (Par_Disc))
        and then Present (Full_View (Scope (Par_Disc)))
      then
         Old_Disc := First_Discriminant (Full_View (Scope (Par_Disc)));
      else
         Old_Disc := First_Discriminant (Scope (Par_Disc));
      end if;

      if Is_Class_Wide_Type (Typ) then
         New_Disc := First_Discriminant (Root_Type (Typ));
      else
         New_Disc := First_Discriminant (Typ);
      end if;

      while Present (Old_Disc) and then Present (New_Disc) loop
         if Old_Disc = Par_Disc  then
            return New_Disc;
         else
            Next_Discriminant (Old_Disc);
            Next_Discriminant (New_Disc);
         end if;
      end loop;

      --  Should always find it

      raise Program_Error;
   end Find_Corresponding_Discriminant;

   -----------------------------
   -- Find_Static_Alternative --
   -----------------------------

   function Find_Static_Alternative (N : Node_Id) return Node_Id is
      Expr   : constant Node_Id := Expression (N);
      Val    : constant Uint    := Expr_Value (Expr);
      Alt    : Node_Id;
      Choice : Node_Id;

   begin
      Alt := First (Alternatives (N));

      Search : loop
         if Nkind (Alt) /= N_Pragma then
            Choice := First (Discrete_Choices (Alt));

            while Present (Choice) loop

               --  Others choice, always matches

               if Nkind (Choice) = N_Others_Choice then
                  exit Search;

               --  Range, check if value is in the range

               elsif Nkind (Choice) = N_Range then
                  exit Search when
                    Val >= Expr_Value (Low_Bound (Choice))
                      and then
                    Val <= Expr_Value (High_Bound (Choice));

               --  Choice is a subtype name. Note that we know it must
               --  be a static subtype, since otherwise it would have
               --  been diagnosed as illegal.

               elsif Is_Entity_Name (Choice)
                 and then Is_Type (Entity (Choice))
               then
                  exit Search when Is_In_Range (Expr, Etype (Choice));

               --  Choice is a subtype indication

               elsif Nkind (Choice) = N_Subtype_Indication then
                  declare
                     C : constant Node_Id := Constraint (Choice);
                     R : constant Node_Id := Range_Expression (C);

                  begin
                     exit Search when
                       Val >= Expr_Value (Low_Bound (R))
                         and then
                       Val <= Expr_Value (High_Bound (R));
                  end;

               --  Choice is a simple expression

               else
                  exit Search when Val = Expr_Value (Choice);
               end if;

               Next (Choice);
            end loop;
         end if;

         Next (Alt);
         pragma Assert (Present (Alt));
      end loop Search;

      --  The above loop *must* terminate by finding a match, since
      --  we know the case statement is valid, and the value of the
      --  expression is known at compile time. When we fall out of
      --  the loop, Alt points to the alternative that we know will
      --  be selected at run time.

      return Alt;
   end Find_Static_Alternative;

   ------------------
   -- First_Actual --
   ------------------

   function First_Actual (Node : Node_Id) return Node_Id is
      N : Node_Id;

   begin
      if No (Parameter_Associations (Node)) then
         return Empty;
      end if;

      N := First (Parameter_Associations (Node));

      if Nkind (N) = N_Parameter_Association then
         return First_Named_Actual (Node);
      else
         return N;
      end if;
   end First_Actual;

   -------------------------
   -- Full_Qualified_Name --
   -------------------------

   function Full_Qualified_Name (E : Entity_Id) return String_Id is
      Res : String_Id;
      pragma Warnings (Off, Res);

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id;
      --  Compute recursively the qualified name without NUL at the end

      ----------------------------------
      -- Internal_Full_Qualified_Name --
      ----------------------------------

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id is
         Ent         : Entity_Id := E;
         Parent_Name : String_Id := No_String;

      begin
         --  Deals properly with child units

         if Nkind (Ent) = N_Defining_Program_Unit_Name then
            Ent := Defining_Identifier (Ent);
         end if;

         --  Compute qualification recursively (only "Standard" has no scope)

         if Present (Scope (Scope (Ent))) then
            Parent_Name := Internal_Full_Qualified_Name (Scope (Ent));
         end if;

         --  Every entity should have a name except some expanded blocks
         --  don't bother about those.

         if Chars (Ent) = No_Name then
            return Parent_Name;
         end if;

         --  Add a period between Name and qualification

         if Parent_Name /= No_String then
            Start_String (Parent_Name);
            Store_String_Char (Get_Char_Code ('.'));

         else
            Start_String;
         end if;

         --  Generates the entity name in upper case

         Get_Decoded_Name_String (Chars (Ent));
         Set_All_Upper_Case;
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         return End_String;
      end Internal_Full_Qualified_Name;

   --  Start of processing for Full_Qualified_Name

   begin
      Res := Internal_Full_Qualified_Name (E);
      Store_String_Char (Get_Char_Code (ASCII.nul));
      return End_String;
   end Full_Qualified_Name;

   -----------------------
   -- Gather_Components --
   -----------------------

   procedure Gather_Components
     (Typ           : Entity_Id;
      Comp_List     : Node_Id;
      Governed_By   : List_Id;
      Into          : Elist_Id;
      Report_Errors : out Boolean)
   is
      Assoc           : Node_Id;
      Variant         : Node_Id;
      Discrete_Choice : Node_Id;
      Comp_Item       : Node_Id;

      Discrim       : Entity_Id;
      Discrim_Name  : Node_Id;
      Discrim_Value : Node_Id;

   begin
      Report_Errors := False;

      if No (Comp_List) or else Null_Present (Comp_List) then
         return;

      elsif Present (Component_Items (Comp_List)) then
         Comp_Item := First (Component_Items (Comp_List));

      else
         Comp_Item := Empty;
      end if;

      while Present (Comp_Item) loop

         --  Skip the tag of a tagged record, the interface tags, as well
         --  as all items that are not user components (anonymous types,
         --  rep clauses, Parent field, controller field).

         if Nkind (Comp_Item) = N_Component_Declaration then
            declare
               Comp : constant Entity_Id := Defining_Identifier (Comp_Item);
            begin
               if not Is_Tag (Comp)
                 and then Chars (Comp) /= Name_uParent
                 and then Chars (Comp) /= Name_uController
               then
                  Append_Elmt (Comp, Into);
               end if;
            end;
         end if;

         Next (Comp_Item);
      end loop;

      if No (Variant_Part (Comp_List)) then
         return;
      else
         Discrim_Name := Name (Variant_Part (Comp_List));
         Variant := First_Non_Pragma (Variants (Variant_Part (Comp_List)));
      end if;

      --  Look for the discriminant that governs this variant part.
      --  The discriminant *must* be in the Governed_By List

      Assoc := First (Governed_By);
      Find_Constraint : loop
         Discrim := First (Choices (Assoc));
         exit Find_Constraint when Chars (Discrim_Name) = Chars (Discrim)
           or else (Present (Corresponding_Discriminant (Entity (Discrim)))
                      and then
                    Chars (Corresponding_Discriminant (Entity (Discrim)))
                         = Chars  (Discrim_Name))
           or else Chars (Original_Record_Component (Entity (Discrim)))
                         = Chars (Discrim_Name);

         if No (Next (Assoc)) then
            if not Is_Constrained (Typ)
              and then Is_Derived_Type (Typ)
              and then Present (Stored_Constraint (Typ))
            then

               --  If the type is a tagged type with inherited discriminants,
               --  use the stored constraint on the parent in order to find
               --  the values of discriminants that are otherwise hidden by an
               --  explicit constraint. Renamed discriminants are handled in
               --  the code above.

               --  If several parent discriminants are renamed by a single
               --  discriminant of the derived type, the call to obtain the
               --  Corresponding_Discriminant field only retrieves the last
               --  of them. We recover the constraint on the others from the
               --  Stored_Constraint as well.

               declare
                  D : Entity_Id;
                  C : Elmt_Id;

               begin
                  D := First_Discriminant (Etype (Typ));
                  C := First_Elmt (Stored_Constraint (Typ));

                  while Present (D)
                    and then Present (C)
                  loop
                     if Chars (Discrim_Name) = Chars (D) then
                        if Is_Entity_Name (Node (C))
                          and then Entity (Node (C)) = Entity (Discrim)
                        then
                           --  D is renamed by Discrim, whose value is
                           --  given in Assoc.

                           null;

                        else
                           Assoc :=
                             Make_Component_Association (Sloc (Typ),
                               New_List
                                 (New_Occurrence_Of (D, Sloc (Typ))),
                                  Duplicate_Subexpr_No_Checks (Node (C)));
                        end if;
                        exit Find_Constraint;
                     end if;

                     D := Next_Discriminant (D);
                     Next_Elmt (C);
                  end loop;
               end;
            end if;
         end if;

         if No (Next (Assoc)) then
            Error_Msg_NE (" missing value for discriminant&",
              First (Governed_By), Discrim_Name);
            Report_Errors := True;
            return;
         end if;

         Next (Assoc);
      end loop Find_Constraint;

      Discrim_Value := Expression (Assoc);

      if not Is_OK_Static_Expression (Discrim_Value) then
         Error_Msg_FE
           ("value for discriminant & must be static!",
            Discrim_Value, Discrim);
         Why_Not_Static (Discrim_Value);
         Report_Errors := True;
         return;
      end if;

      Search_For_Discriminant_Value : declare
         Low  : Node_Id;
         High : Node_Id;

         UI_High          : Uint;
         UI_Low           : Uint;
         UI_Discrim_Value : constant Uint := Expr_Value (Discrim_Value);

      begin
         Find_Discrete_Value : while Present (Variant) loop
            Discrete_Choice := First (Discrete_Choices (Variant));
            while Present (Discrete_Choice) loop

               exit Find_Discrete_Value when
                 Nkind (Discrete_Choice) = N_Others_Choice;

               Get_Index_Bounds (Discrete_Choice, Low, High);

               UI_Low  := Expr_Value (Low);
               UI_High := Expr_Value (High);

               exit Find_Discrete_Value when
                 UI_Low <= UI_Discrim_Value
                   and then
                 UI_High >= UI_Discrim_Value;

               Next (Discrete_Choice);
            end loop;

            Next_Non_Pragma (Variant);
         end loop Find_Discrete_Value;
      end Search_For_Discriminant_Value;

      if No (Variant) then
         Error_Msg_NE
           ("value of discriminant & is out of range", Discrim_Value, Discrim);
         Report_Errors := True;
         return;
      end  if;

      --  If we have found the corresponding choice, recursively add its
      --  components to the Into list.

      Gather_Components (Empty,
        Component_List (Variant), Governed_By, Into, Report_Errors);
   end Gather_Components;

   ------------------------
   -- Get_Actual_Subtype --
   ------------------------

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);
      Utyp : Entity_Id := Underlying_Type (Typ);
      Decl : Node_Id;
      Atyp : Entity_Id;

   begin
      if No (Utyp) then
         Utyp := Typ;
      end if;

      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))
      then
         return Actual_Subtype (Entity (N));

      --  Actual subtype of unchecked union is always itself. We never need
      --  the "real" actual subtype. If we did, we couldn't get it anyway
      --  because the discriminant is not available. The restrictions on
      --  Unchecked_Union are designed to make sure that this is OK.

      elsif Is_Unchecked_Union (Base_Type (Utyp)) then
         return Typ;

      --  Here for the unconstrained case, we must find actual subtype
      --  No actual subtype is available, so we must build it on the fly.

      --  Checking the type, not the underlying type, for constrainedness
      --  seems to be necessary. Maybe all the tests should be on the type???

      elsif (not Is_Constrained (Typ))
           and then (Is_Array_Type (Utyp)
                      or else (Is_Record_Type (Utyp)
                                and then Has_Discriminants (Utyp)))
           and then not Has_Unknown_Discriminants (Utyp)
           and then not (Ekind (Utyp) = E_String_Literal_Subtype)
      then
         --  Nothing to do if in default expression

         if In_Default_Expression then
            return Typ;

         elsif Is_Private_Type (Typ)
           and then not Has_Discriminants (Typ)
         then
            --  If the type has no discriminants, there is no subtype to
            --  build, even if the underlying type is discriminated.

            return Typ;

         --  Else build the actual subtype

         else
            Decl := Build_Actual_Subtype (Typ, N);
            Atyp := Defining_Identifier (Decl);

            --  If Build_Actual_Subtype generated a new declaration then use it

            if Atyp /= Typ then

               --  The actual subtype is an Itype, so analyze the declaration,
               --  but do not attach it to the tree, to get the type defined.

               Set_Parent (Decl, N);
               Set_Is_Itype (Atyp);
               Analyze (Decl, Suppress => All_Checks);
               Set_Associated_Node_For_Itype (Atyp, N);
               Set_Has_Delayed_Freeze (Atyp, False);

               --  We need to freeze the actual subtype immediately. This is
               --  needed, because otherwise this Itype will not get frozen
               --  at all, and it is always safe to freeze on creation because
               --  any associated types must be frozen at this point.

               Freeze_Itype (Atyp, N);
               return Atyp;

            --  Otherwise we did not build a declaration, so return original

            else
               return Typ;
            end if;
         end if;

      --  For all remaining cases, the actual subtype is the same as
      --  the nominal type.

      else
         return Typ;
      end if;
   end Get_Actual_Subtype;

   -------------------------------------
   -- Get_Actual_Subtype_If_Available --
   -------------------------------------

   function Get_Actual_Subtype_If_Available (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);

   begin
      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))
      then
         return Actual_Subtype (Entity (N));

      --  Otherwise the Etype of N is returned unchanged

      else
         return Typ;
      end if;
   end Get_Actual_Subtype_If_Available;

   -------------------------------
   -- Get_Default_External_Name --
   -------------------------------

   function Get_Default_External_Name (E : Node_Or_Entity_Id) return Node_Id is
   begin
      Get_Decoded_Name_String (Chars (E));

      if Opt.External_Name_Imp_Casing = Uppercase then
         Set_Casing (All_Upper_Case);
      else
         Set_Casing (All_Lower_Case);
      end if;

      return
        Make_String_Literal (Sloc (E),
          Strval => String_From_Name_Buffer);
   end Get_Default_External_Name;

   ---------------------------
   -- Get_Enum_Lit_From_Pos --
   ---------------------------

   function Get_Enum_Lit_From_Pos
     (T   : Entity_Id;
      Pos : Uint;
      Loc : Source_Ptr) return Node_Id
   is
      Lit : Node_Id;

   begin
      --  In the case where the literal is of type Character, Wide_Character
      --  or Wide_Wide_Character or of a type derived from them, there needs
      --  to be some special handling since there is no explicit chain of
      --  literals to search. Instead, an N_Character_Literal node is created
      --  with the appropriate Char_Code and Chars fields.

      if Root_Type (T) = Standard_Character
        or else Root_Type (T) = Standard_Wide_Character
        or else Root_Type (T) = Standard_Wide_Wide_Character
      then
         Set_Character_Literal_Name (UI_To_CC (Pos));
         return
           Make_Character_Literal (Loc,
             Chars              => Name_Find,
             Char_Literal_Value => Pos);

      --  For all other cases, we have a complete table of literals, and
      --  we simply iterate through the chain of literal until the one
      --  with the desired position value is found.
      --

      else
         Lit := First_Literal (Base_Type (T));
         for J in 1 .. UI_To_Int (Pos) loop
            Next_Literal (Lit);
         end loop;

         return New_Occurrence_Of (Lit, Loc);
      end if;
   end Get_Enum_Lit_From_Pos;

   ------------------------
   -- Get_Generic_Entity --
   ------------------------

   function Get_Generic_Entity (N : Node_Id) return Entity_Id is
      Ent : constant Entity_Id := Entity (Name (N));
   begin
      if Present (Renamed_Object (Ent)) then
         return Renamed_Object (Ent);
      else
         return Ent;
      end if;
   end Get_Generic_Entity;

   ----------------------
   -- Get_Index_Bounds --
   ----------------------

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id) is
      Kind : constant Node_Kind := Nkind (N);
      R    : Node_Id;

   begin
      if Kind = N_Range then
         L := Low_Bound (N);
         H := High_Bound (N);

      elsif Kind = N_Subtype_Indication then
         R := Range_Expression (Constraint (N));

         if R = Error then
            L := Error;
            H := Error;
            return;

         else
            L := Low_Bound  (Range_Expression (Constraint (N)));
            H := High_Bound (Range_Expression (Constraint (N)));
         end if;

      elsif Is_Entity_Name (N) and then Is_Type (Entity (N)) then
         if Error_Posted (Scalar_Range (Entity (N))) then
            L := Error;
            H := Error;

         elsif Nkind (Scalar_Range (Entity (N))) = N_Subtype_Indication then
            Get_Index_Bounds (Scalar_Range (Entity (N)), L, H);

         else
            L := Low_Bound  (Scalar_Range (Entity (N)));
            H := High_Bound (Scalar_Range (Entity (N)));
         end if;

      else
         --  N is an expression, indicating a range with one value

         L := N;
         H := N;
      end if;
   end Get_Index_Bounds;

   ----------------------------------
   -- Get_Library_Unit_Name_string --
   ----------------------------------

   procedure Get_Library_Unit_Name_String (Decl_Node : Node_Id) is
      Unit_Name_Id : constant Unit_Name_Type := Get_Unit_Name (Decl_Node);

   begin
      Get_Unit_Name_String (Unit_Name_Id);

      --  Remove seven last character (" (spec)" or " (body)")

      Name_Len := Name_Len - 7;
      pragma Assert (Name_Buffer (Name_Len + 1) = ' ');
   end Get_Library_Unit_Name_String;

   ------------------------
   -- Get_Name_Entity_Id --
   ------------------------

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id is
   begin
      return Entity_Id (Get_Name_Table_Info (Id));
   end Get_Name_Entity_Id;

   ---------------------------
   -- Get_Referenced_Object --
   ---------------------------

   function Get_Referenced_Object (N : Node_Id) return Node_Id is
      R   : Node_Id := N;

   begin
      while Is_Entity_Name (R)
        and then Present (Renamed_Object (Entity (R)))
      loop
         R := Renamed_Object (Entity (R));
      end loop;

      return R;
   end Get_Referenced_Object;

   -------------------------
   -- Get_Subprogram_Body --
   -------------------------

   function Get_Subprogram_Body (E : Entity_Id) return Node_Id is
      Decl : Node_Id;

   begin
      Decl := Unit_Declaration_Node (E);

      if Nkind (Decl) = N_Subprogram_Body then
         return Decl;

      --  The below comment is bad, because it is possible for
      --  Nkind (Decl) to be an N_Subprogram_Body_Stub ???

      else           --  Nkind (Decl) = N_Subprogram_Declaration

         if Present (Corresponding_Body (Decl)) then
            return Unit_Declaration_Node (Corresponding_Body (Decl));

         --  Imported subprogram case

         else
            return Empty;
         end if;
      end if;
   end Get_Subprogram_Body;

   -----------------------------
   -- Get_Task_Body_Procedure --
   -----------------------------

   function Get_Task_Body_Procedure (E : Entity_Id) return Node_Id is
   begin
      --  Note: A task type may be the completion of a private type with
      --  discriminants. when performing elaboration checks on a task
      --  declaration, the current view of the type may be the private one,
      --  and the procedure that holds the body of the task is held in its
      --  underlying type.

      return Task_Body_Procedure (Underlying_Type (Root_Type (E)));
   end Get_Task_Body_Procedure;

   -----------------------
   -- Has_Access_Values --
   -----------------------

   function Has_Access_Values (T : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Underlying_Type (T);

   begin
      --  Case of a private type which is not completed yet. This can only
      --  happen in the case of a generic format type appearing directly, or
      --  as a component of the type to which this function is being applied
      --  at the top level. Return False in this case, since we certainly do
      --  not know that the type contains access types.

      if No (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         return Has_Access_Values (Component_Type (Typ));

      elsif Is_Record_Type (Typ) then
         declare
            Comp : Entity_Id;

         begin
            Comp := First_Entity (Typ);
            while Present (Comp) loop
               if (Ekind (Comp) = E_Component
                     or else
                   Ekind (Comp) = E_Discriminant)
                 and then Has_Access_Values (Etype (Comp))
               then
                  return True;
               end if;

               Next_Entity (Comp);
            end loop;
         end;

         return False;

      else
         return False;
      end if;
   end Has_Access_Values;

   ----------------------
   -- Has_Declarations --
   ----------------------

   function Has_Declarations (N : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (N);
   begin
      return    K = N_Accept_Statement
        or else K = N_Block_Statement
        or else K = N_Compilation_Unit_Aux
        or else K = N_Entry_Body
        or else K = N_Package_Body
        or else K = N_Protected_Body
        or else K = N_Subprogram_Body
        or else K = N_Task_Body
        or else K = N_Package_Specification;
   end Has_Declarations;

   -------------------------------------------
   -- Has_Discriminant_Dependent_Constraint --
   -------------------------------------------

   function Has_Discriminant_Dependent_Constraint
     (Comp : Entity_Id) return Boolean
   is
      Comp_Decl  : constant Node_Id := Parent (Comp);
      Subt_Indic : constant Node_Id :=
                     Subtype_Indication (Component_Definition (Comp_Decl));
      Constr     : Node_Id;
      Assn       : Node_Id;

   begin
      if Nkind (Subt_Indic) = N_Subtype_Indication then
         Constr := Constraint (Subt_Indic);

         if Nkind (Constr) = N_Index_Or_Discriminant_Constraint then
            Assn := First (Constraints (Constr));
            while Present (Assn) loop
               case Nkind (Assn) is
                  when N_Subtype_Indication |
                       N_Range              |
                       N_Identifier
                  =>
                     if Depends_On_Discriminant (Assn) then
                        return True;
                     end if;

                  when N_Discriminant_Association =>
                     if Depends_On_Discriminant (Expression (Assn)) then
                        return True;
                     end if;

                  when others =>
                     null;

               end case;

               Next (Assn);
            end loop;
         end if;
      end if;

      return False;
   end Has_Discriminant_Dependent_Constraint;

   --------------------
   -- Has_Infinities --
   --------------------

   function Has_Infinities (E : Entity_Id) return Boolean is
   begin
      return
        Is_Floating_Point_Type (E)
          and then Nkind (Scalar_Range (E)) = N_Range
          and then Includes_Infinities (Scalar_Range (E));
   end Has_Infinities;

   ------------------------
   -- Has_Null_Extension --
   ------------------------

   function Has_Null_Extension (T : Entity_Id) return Boolean is
      B     : constant Entity_Id := Base_Type (T);
      Comps : Node_Id;
      Ext   : Node_Id;

   begin
      if Nkind (Parent (B)) = N_Full_Type_Declaration
        and then Present (Record_Extension_Part (Type_Definition (Parent (B))))
      then
         Ext := Record_Extension_Part (Type_Definition (Parent (B)));

         if Present (Ext) then
            if Null_Present (Ext) then
               return True;
            else
               Comps := Component_List (Ext);

               --  The null component list is rewritten during analysis to
               --  include the parent component. Any other component indicates
               --  that the extension was not originally null.

               return Null_Present (Comps)
                 or else No (Next (First (Component_Items (Comps))));
            end if;
         else
            return False;
         end if;

      else
         return False;
      end if;
   end Has_Null_Extension;

   ---------------------------
   -- Has_Private_Component --
   ---------------------------

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean is
      Btype     : Entity_Id := Base_Type (Type_Id);
      Component : Entity_Id;

   begin
      if Error_Posted (Type_Id)
        or else Error_Posted (Btype)
      then
         return False;
      end if;

      if Is_Class_Wide_Type (Btype) then
         Btype := Root_Type (Btype);
      end if;

      if Is_Private_Type (Btype) then
         declare
            UT : constant Entity_Id := Underlying_Type (Btype);
         begin
            if No (UT) then

               if No (Full_View (Btype)) then
                  return not Is_Generic_Type (Btype)
                    and then not Is_Generic_Type (Root_Type (Btype));

               else
                  return not Is_Generic_Type (Root_Type (Full_View (Btype)));
               end if;

            else
               return not Is_Frozen (UT) and then Has_Private_Component (UT);
            end if;
         end;
      elsif Is_Array_Type (Btype) then
         return Has_Private_Component (Component_Type (Btype));

      elsif Is_Record_Type (Btype) then

         Component := First_Component (Btype);
         while Present (Component) loop

            if Has_Private_Component (Etype (Component)) then
               return True;
            end if;

            Next_Component (Component);
         end loop;

         return False;

      elsif Is_Protected_Type (Btype)
        and then Present (Corresponding_Record_Type (Btype))
      then
         return Has_Private_Component (Corresponding_Record_Type (Btype));

      else
         return False;
      end if;
   end Has_Private_Component;

   ----------------
   -- Has_Stream --
   ----------------

   function Has_Stream (T : Entity_Id) return Boolean is
      E : Entity_Id;

   begin
      if No (T) then
         return False;

      elsif Is_RTE (Root_Type (T), RE_Root_Stream_Type) then
         return True;

      elsif Is_Array_Type (T) then
         return Has_Stream (Component_Type (T));

      elsif Is_Record_Type (T) then
         E := First_Component (T);
         while Present (E) loop
            if Has_Stream (Etype (E)) then
               return True;
            else
               Next_Component (E);
            end if;
         end loop;

         return False;

      elsif Is_Private_Type (T) then
         return Has_Stream (Underlying_Type (T));

      else
         return False;
      end if;
   end Has_Stream;

   --------------------------
   -- Has_Tagged_Component --
   --------------------------

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Is_Private_Type (Typ)
        and then Present (Underlying_Type (Typ))
      then
         return Has_Tagged_Component (Underlying_Type (Typ));

      elsif Is_Array_Type (Typ) then
         return Has_Tagged_Component (Component_Type (Typ));

      elsif Is_Tagged_Type (Typ) then
         return True;

      elsif Is_Record_Type (Typ) then
         Comp := First_Component (Typ);

         while Present (Comp) loop
            if Has_Tagged_Component (Etype (Comp)) then
               return True;
            end if;

            Comp := Next_Component (Typ);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Tagged_Component;

   -----------------
   -- In_Instance --
   -----------------

   function In_Instance return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Package
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance;

   ----------------------
   -- In_Instance_Body --
   ----------------------

   function In_Instance_Body return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then In_Package_Body (S)
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Body;

   -----------------------------
   -- In_Instance_Not_Visible --
   -----------------------------

   function In_Instance_Not_Visible return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then (In_Package_Body (S) or else In_Private_Part (S))
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Not_Visible;

   ------------------------------
   -- In_Instance_Visible_Part --
   ------------------------------

   function In_Instance_Visible_Part return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if Ekind (S) = E_Package
           and then Is_Generic_Instance (S)
           and then not In_Package_Body (S)
           and then not In_Private_Part (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Visible_Part;

   ----------------------
   -- In_Packiage_Body --
   ----------------------

   function In_Package_Body return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if Ekind (S) = E_Package
           and then In_Package_Body (S)
         then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end In_Package_Body;

   --------------------------------------
   -- In_Subprogram_Or_Concurrent_Unit --
   --------------------------------------

   function In_Subprogram_Or_Concurrent_Unit return Boolean is
      E : Entity_Id;
      K : Entity_Kind;

   begin
      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         K := Ekind (E);

         if K in Subprogram_Kind
           or else K in Concurrent_Kind
           or else K in Generic_Subprogram_Kind
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;
   end In_Subprogram_Or_Concurrent_Unit;

   ---------------------
   -- In_Visible_Part --
   ---------------------

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean is
   begin
      return
        Is_Package_Or_Generic_Package (Scope_Id)
          and then In_Open_Scopes (Scope_Id)
          and then not In_Package_Body (Scope_Id)
          and then not In_Private_Part (Scope_Id);
   end In_Visible_Part;

   ---------------------------------
   -- Insert_Explicit_Dereference --
   ---------------------------------

   procedure Insert_Explicit_Dereference (N : Node_Id) is
      New_Prefix : constant Node_Id := Relocate_Node (N);
      Ent        : Entity_Id := Empty;
      Pref       : Node_Id;
      I          : Interp_Index;
      It         : Interp;
      T          : Entity_Id;

   begin
      Save_Interps (N, New_Prefix);
      Rewrite (N,
        Make_Explicit_Dereference (Sloc (N), Prefix => New_Prefix));

      Set_Etype (N, Designated_Type (Etype (New_Prefix)));

      if Is_Overloaded (New_Prefix) then

         --  The deference is also overloaded, and its interpretations are the
         --  designated types of the interpretations of the original node.

         Set_Etype (N, Any_Type);
         Get_First_Interp (New_Prefix, I, It);

         while Present (It.Nam) loop
            T := It.Typ;

            if Is_Access_Type (T) then
               Add_One_Interp (N, Designated_Type (T), Designated_Type (T));
            end if;

            Get_Next_Interp (I, It);
         end loop;

         End_Interp_List;

      else
         --  Prefix is unambiguous: mark the original prefix (which might
         --  Come_From_Source) as a reference, since the new (relocated) one
         --  won't be taken into account.

         if Is_Entity_Name (New_Prefix) then
            Ent := Entity (New_Prefix);

         --  For a retrieval of a subcomponent of some composite object,
         --  retrieve the ultimate entity if there is one.

         elsif Nkind (New_Prefix) = N_Selected_Component
           or else Nkind (New_Prefix) = N_Indexed_Component
         then
            Pref := Prefix (New_Prefix);

            while Present (Pref)
              and then
                (Nkind (Pref) = N_Selected_Component
                  or else Nkind (Pref) = N_Indexed_Component)
            loop
               Pref := Prefix (Pref);
            end loop;

            if Present (Pref) and then Is_Entity_Name (Pref) then
               Ent := Entity (Pref);
            end if;
         end if;

         if Present (Ent) then
            Generate_Reference (Ent, New_Prefix);
         end if;
      end if;
   end Insert_Explicit_Dereference;

   -------------------
   -- Is_AAMP_Float --
   -------------------

   function Is_AAMP_Float (E : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Type (E));

      return AAMP_On_Target
         and then Is_Floating_Point_Type (E)
         and then E = Base_Type (E);
   end Is_AAMP_Float;

   -------------------------
   -- Is_Actual_Parameter --
   -------------------------

   function Is_Actual_Parameter (N : Node_Id) return Boolean is
      PK : constant Node_Kind := Nkind (Parent (N));

   begin
      case PK is
         when N_Parameter_Association =>
            return N = Explicit_Actual_Parameter (Parent (N));

         when N_Function_Call | N_Procedure_Call_Statement =>
            return Is_List_Member (N)
              and then
                List_Containing (N) = Parameter_Associations (Parent (N));

         when others =>
            return False;
      end case;
   end Is_Actual_Parameter;

   ---------------------
   -- Is_Aliased_View --
   ---------------------

   function Is_Aliased_View (Obj : Node_Id) return Boolean is
      E : Entity_Id;

   begin
      if Is_Entity_Name (Obj) then

         E := Entity (Obj);

         return
           (Is_Object (E)
             and then
               (Is_Aliased (E)
                  or else (Present (Renamed_Object (E))
                             and then Is_Aliased_View (Renamed_Object (E)))))

           or else ((Is_Formal (E)
                      or else Ekind (E) = E_Generic_In_Out_Parameter
                      or else Ekind (E) = E_Generic_In_Parameter)
                    and then Is_Tagged_Type (Etype (E)))

           or else ((Ekind (E) = E_Task_Type
                      or else Ekind (E) = E_Protected_Type)
                    and then In_Open_Scopes (E))

            --  Current instance of type

           or else (Is_Type (E) and then E = Current_Scope)
           or else (Is_Incomplete_Or_Private_Type (E)
                     and then Full_View (E) = Current_Scope);

      elsif Nkind (Obj) = N_Selected_Component then
         return Is_Aliased (Entity (Selector_Name (Obj)));

      elsif Nkind (Obj) = N_Indexed_Component then
         return Has_Aliased_Components (Etype (Prefix (Obj)))
           or else
             (Is_Access_Type (Etype (Prefix (Obj)))
               and then
              Has_Aliased_Components
                (Designated_Type (Etype (Prefix (Obj)))));

      elsif Nkind (Obj) = N_Unchecked_Type_Conversion
        or else Nkind (Obj) = N_Type_Conversion
      then
         return Is_Tagged_Type (Etype (Obj))
           and then Is_Aliased_View (Expression (Obj));

      elsif Nkind (Obj) = N_Explicit_Dereference then
         return Nkind (Original_Node (Obj)) /= N_Function_Call;

      else
         return False;
      end if;
   end Is_Aliased_View;

   -------------------------
   -- Is_Ancestor_Package --
   -------------------------

   function Is_Ancestor_Package
     (E1  : Entity_Id;
      E2  : Entity_Id) return Boolean
   is
      Par : Entity_Id;

   begin
      Par := E2;
      while Present (Par)
        and then Par /= Standard_Standard
      loop
         if Par = E1 then
            return True;
         end if;

         Par := Scope (Par);
      end loop;

      return False;
   end Is_Ancestor_Package;

   ----------------------
   -- Is_Atomic_Object --
   ----------------------

   function Is_Atomic_Object (N : Node_Id) return Boolean is

      function Object_Has_Atomic_Components (N : Node_Id) return Boolean;
      --  Determines if given object has atomic components

      function Is_Atomic_Prefix (N : Node_Id) return Boolean;
      --  If prefix is an implicit dereference, examine designated type

      function Is_Atomic_Prefix (N : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (N)) then
            return
              Has_Atomic_Components (Designated_Type (Etype (N)));
         else
            return Object_Has_Atomic_Components (N);
         end if;
      end Is_Atomic_Prefix;

      function Object_Has_Atomic_Components (N : Node_Id) return Boolean is
      begin
         if Has_Atomic_Components (Etype (N))
           or else Is_Atomic (Etype (N))
         then
            return True;

         elsif Is_Entity_Name (N)
           and then (Has_Atomic_Components (Entity (N))
                      or else Is_Atomic (Entity (N)))
         then
            return True;

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            return Is_Atomic_Prefix (Prefix (N));

         else
            return False;
         end if;
      end Object_Has_Atomic_Components;

   --  Start of processing for Is_Atomic_Object

   begin
      if Is_Atomic (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Atomic (Entity (N)))
      then
         return True;

      elsif Nkind (N) = N_Indexed_Component
        or else Nkind (N) = N_Selected_Component
      then
         return Is_Atomic_Prefix (Prefix (N));

      else
         return False;
      end if;
   end Is_Atomic_Object;

   --------------------------------------
   -- Is_Controlling_Limited_Procedure --
   --------------------------------------

   function Is_Controlling_Limited_Procedure
     (Proc_Nam : Entity_Id) return Boolean
   is
      Param_Typ : Entity_Id := Empty;

   begin
      if Ekind (Proc_Nam) = E_Procedure
        and then Present (Parameter_Specifications (Parent (Proc_Nam)))
      then
         Param_Typ := Etype (Parameter_Type (First (
                        Parameter_Specifications (Parent (Proc_Nam)))));

      --  In this case where an Itype was created, the procedure call has been
      --  rewritten.

      elsif Present (Associated_Node_For_Itype (Proc_Nam))
        and then Present (Original_Node (Associated_Node_For_Itype (Proc_Nam)))
        and then
          Present (Parameter_Associations
                     (Associated_Node_For_Itype (Proc_Nam)))
      then
         Param_Typ :=
           Etype (First (Parameter_Associations
                          (Associated_Node_For_Itype (Proc_Nam))));
      end if;

      if Present (Param_Typ) then
         return
           Is_Interface (Param_Typ)
             and then Is_Limited_Record (Param_Typ);
      end if;

      return False;
   end Is_Controlling_Limited_Procedure;

   ----------------------------------------------
   -- Is_Dependent_Component_Of_Mutable_Object --
   ----------------------------------------------

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id) return   Boolean
   is
      P           : Node_Id;
      Prefix_Type : Entity_Id;
      P_Aliased   : Boolean := False;
      Comp        : Entity_Id;

      function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean;
      --  Returns True if and only if Comp is declared within a variant part

      --------------------------------
      -- Is_Declared_Within_Variant --
      --------------------------------

      function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean is
         Comp_Decl : constant Node_Id   := Parent (Comp);
         Comp_List : constant Node_Id   := Parent (Comp_Decl);
      begin
         return Nkind (Parent (Comp_List)) = N_Variant;
      end Is_Declared_Within_Variant;

   --  Start of processing for Is_Dependent_Component_Of_Mutable_Object

   begin
      if Is_Variable (Object) then

         if Nkind (Object) = N_Selected_Component then
            P := Prefix (Object);
            Prefix_Type := Etype (P);

            if Is_Entity_Name (P) then

               if Ekind (Entity (P)) = E_Generic_In_Out_Parameter then
                  Prefix_Type := Base_Type (Prefix_Type);
               end if;

               if Is_Aliased (Entity (P)) then
                  P_Aliased := True;
               end if;

            --  A discriminant check on a selected component may be
            --  expanded into a dereference when removing side-effects.
            --  Recover the original node and its type, which may be
            --  unconstrained.

            elsif Nkind (P) = N_Explicit_Dereference
              and then not (Comes_From_Source (P))
            then
               P := Original_Node (P);
               Prefix_Type := Etype (P);

            else
               --  Check for prefix being an aliased component ???
               null;

            end if;

            --  A heap object is constrained by its initial value

            --  Ada 2005 AI-363:if the designated type is a type with a
            --  constrained partial view, the resulting heap object is not
            --  constrained, and a renaming of the component is now unsafe.

            if Is_Access_Type (Prefix_Type)
              and then
                 not Has_Constrained_Partial_View
                   (Designated_Type (Prefix_Type))
            then
               return False;

            elsif Nkind (P) = N_Explicit_Dereference
              and then not Has_Constrained_Partial_View (Prefix_Type)
            then
               return False;
            end if;

            Comp :=
              Original_Record_Component (Entity (Selector_Name (Object)));

            --  As per AI-0017, the renaming is illegal in a generic body,
            --  even if the subtype is indefinite.

            if not Is_Constrained (Prefix_Type)
              and then (not Is_Indefinite_Subtype (Prefix_Type)
                         or else
                          (Is_Generic_Type (Prefix_Type)
                            and then Ekind (Current_Scope) = E_Generic_Package
                            and then In_Package_Body (Current_Scope)))

              and then (Is_Declared_Within_Variant (Comp)
                          or else Has_Discriminant_Dependent_Constraint (Comp))
              and then not P_Aliased
            then
               return True;

            else
               return
                 Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));

            end if;

         elsif Nkind (Object) = N_Indexed_Component
           or else Nkind (Object) = N_Slice
         then
            return Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));

         --  A type conversion that Is_Variable is a view conversion:
         --  go back to the denoted object.

         elsif Nkind (Object) = N_Type_Conversion then
            return
              Is_Dependent_Component_Of_Mutable_Object (Expression (Object));
         end if;
      end if;

      return False;
   end Is_Dependent_Component_Of_Mutable_Object;

   ---------------------
   -- Is_Dereferenced --
   ---------------------

   function Is_Dereferenced (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);
   begin
      return
         (Nkind (P) = N_Selected_Component
            or else
          Nkind (P) = N_Explicit_Dereference
            or else
          Nkind (P) = N_Indexed_Component
            or else
          Nkind (P) = N_Slice)
        and then Prefix (P) = N;
   end Is_Dereferenced;

   ----------------------
   -- Is_Descendent_Of --
   ----------------------

   function Is_Descendent_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
      T    : Entity_Id;
      Etyp : Entity_Id;

   begin
      pragma Assert (Nkind (T1) in N_Entity);
      pragma Assert (Nkind (T2) in N_Entity);

      T := Base_Type (T1);

      --  Immediate return if the types match

      if T = T2 then
         return True;

      --  Comment needed here ???

      elsif Ekind (T) = E_Class_Wide_Type then
         return Etype (T) = T2;

      --  All other cases

      else
         loop
            Etyp := Etype (T);

            --  Done if we found the type we are looking for

            if Etyp = T2 then
               return True;

            --  Done if no more derivations to check

            elsif T = T1
              or else T = Etyp
            then
               return False;

            --  Following test catches error cases resulting from prev errors

            elsif No (Etyp) then
               return False;

            elsif Is_Private_Type (T) and then Etyp = Full_View (T) then
               return False;

            elsif Is_Private_Type (Etyp) and then Full_View (Etyp) = T then
               return False;
            end if;

            T := Base_Type (Etyp);
         end loop;
      end if;

      raise Program_Error;
   end Is_Descendent_Of;

   ------------------------------
   -- Is_Descendent_Of_Address --
   ------------------------------

   function Is_Descendent_Of_Address (T1 : Entity_Id) return Boolean is
   begin
      --  If Address has not been loaded, answer must be False

      if not RTU_Loaded (System) then
         return False;

      --  Otherwise we can get the entity we are interested in without
      --  causing an unwanted dependency on System, and do the test.

      else
         return Is_Descendent_Of (T1, Base_Type (RTE (RE_Address)));
      end if;
   end Is_Descendent_Of_Address;

   --------------
   -- Is_False --
   --------------

   function Is_False (U : Uint) return Boolean is
   begin
      return (U = 0);
   end Is_False;

   ---------------------------
   -- Is_Fixed_Model_Number --
   ---------------------------

   function Is_Fixed_Model_Number (U : Ureal; T : Entity_Id) return Boolean is
      S : constant Ureal := Small_Value (T);
      M : Urealp.Save_Mark;
      R : Boolean;
   begin
      M := Urealp.Mark;
      R := (U = UR_Trunc (U / S) * S);
      Urealp.Release (M);
      return R;
   end Is_Fixed_Model_Number;

   -------------------------------
   -- Is_Fully_Initialized_Type --
   -------------------------------

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         if Is_Fully_Initialized_Type (Component_Type (Typ)) then
            return True;
         end if;

         --  An interesting case, if we have a constrained type one of whose
         --  bounds is known to be null, then there are no elements to be
         --  initialized, so all the elements are initialized!

         if Is_Constrained (Typ) then
            declare
               Indx     : Node_Id;
               Indx_Typ : Entity_Id;
               Lbd, Hbd : Node_Id;

            begin
               Indx := First_Index (Typ);
               while Present (Indx) loop

                  if Etype (Indx) = Any_Type then
                     return False;

                  --  If index is a range, use directly

                  elsif Nkind (Indx) = N_Range then
                     Lbd := Low_Bound  (Indx);
                     Hbd := High_Bound (Indx);

                  else
                     Indx_Typ := Etype (Indx);

                     if Is_Private_Type (Indx_Typ)  then
                        Indx_Typ := Full_View (Indx_Typ);
                     end if;

                     if No (Indx_Typ) then
                        return False;
                     else
                        Lbd := Type_Low_Bound  (Indx_Typ);
                        Hbd := Type_High_Bound (Indx_Typ);
                     end if;
                  end if;

                  if Compile_Time_Known_Value (Lbd)
                    and then Compile_Time_Known_Value (Hbd)
                  then
                     if Expr_Value (Hbd) < Expr_Value (Lbd) then
                        return True;
                     end if;
                  end if;

                  Next_Index (Indx);
               end loop;
            end;
         end if;

         --  If no null indexes, then type is not fully initialized

         return False;

      --  Record types

      elsif Is_Record_Type (Typ) then
         if Has_Discriminants (Typ)
           and then
             Present (Discriminant_Default_Value (First_Discriminant (Typ)))
           and then Is_Fully_Initialized_Variant (Typ)
         then
            return True;
         end if;

         --  Controlled records are considered to be fully initialized if
         --  there is a user defined Initialize routine. This may not be
         --  entirely correct, but as the spec notes, we are guessing here
         --  what is best from the point of view of issuing warnings.

         if Is_Controlled (Typ) then
            declare
               Utyp : constant Entity_Id := Underlying_Type (Typ);

            begin
               if Present (Utyp) then
                  declare
                     Init : constant Entity_Id :=
                              (Find_Prim_Op
                                 (Underlying_Type (Typ), Name_Initialize));

                  begin
                     if Present (Init)
                       and then Comes_From_Source (Init)
                       and then not
                         Is_Predefined_File_Name
                           (File_Name (Get_Source_File_Index (Sloc (Init))))
                     then
                        return True;

                     elsif Has_Null_Extension (Typ)
                        and then
                          Is_Fully_Initialized_Type
                            (Etype (Base_Type (Typ)))
                     then
                        return True;
                     end if;
                  end;
               end if;
            end;
         end if;

         --  Otherwise see if all record components are initialized

         declare
            Ent : Entity_Id;

         begin
            Ent := First_Entity (Typ);

            while Present (Ent) loop
               if Chars (Ent) = Name_uController then
                  null;

               elsif Ekind (Ent) = E_Component
                 and then (No (Parent (Ent))
                             or else No (Expression (Parent (Ent))))
                 and then not Is_Fully_Initialized_Type (Etype (Ent))
               then
                  return False;
               end if;

               Next_Entity (Ent);
            end loop;
         end;

         --  No uninitialized components, so type is fully initialized.
         --  Note that this catches the case of no components as well.

         return True;

      elsif Is_Concurrent_Type (Typ) then
         return True;

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);

         begin
            if No (U) then
               return False;
            else
               return Is_Fully_Initialized_Type (U);
            end if;
         end;

      else
         return False;
      end if;
   end Is_Fully_Initialized_Type;

   ----------------------------------
   -- Is_Fully_Initialized_Variant --
   ----------------------------------

   function Is_Fully_Initialized_Variant (Typ : Entity_Id) return Boolean is
      Loc           : constant Source_Ptr := Sloc (Typ);
      Constraints   : constant List_Id    := New_List;
      Components    : constant Elist_Id   := New_Elmt_List;
      Comp_Elmt     : Elmt_Id;
      Comp_Id       : Node_Id;
      Comp_List     : Node_Id;
      Discr         : Entity_Id;
      Discr_Val     : Node_Id;
      Report_Errors : Boolean;

   begin
      if Serious_Errors_Detected > 0 then
         return False;
      end if;

      if Is_Record_Type (Typ)
        and then Nkind (Parent (Typ)) = N_Full_Type_Declaration
        and then Nkind (Type_Definition (Parent (Typ))) = N_Record_Definition
      then
         Comp_List := Component_List (Type_Definition (Parent (Typ)));
         Discr := First_Discriminant (Typ);

         while Present (Discr) loop
            if Nkind (Parent (Discr)) = N_Discriminant_Specification then
               Discr_Val := Expression (Parent (Discr));

               if Present (Discr_Val)
                 and then Is_OK_Static_Expression (Discr_Val)
               then
                  Append_To (Constraints,
                    Make_Component_Association (Loc,
                      Choices    => New_List (New_Occurrence_Of (Discr, Loc)),
                      Expression => New_Copy (Discr_Val)));
               else
                  return False;
               end if;
            else
               return False;
            end if;

            Next_Discriminant (Discr);
         end loop;

         Gather_Components
           (Typ           => Typ,
            Comp_List     => Comp_List,
            Governed_By   => Constraints,
            Into          => Components,
            Report_Errors => Report_Errors);

         --  Check that each component present is fully initialized

         Comp_Elmt := First_Elmt (Components);

         while Present (Comp_Elmt) loop
            Comp_Id := Node (Comp_Elmt);

            if Ekind (Comp_Id) = E_Component
              and then (No (Parent (Comp_Id))
                         or else No (Expression (Parent (Comp_Id))))
              and then not Is_Fully_Initialized_Type (Etype (Comp_Id))
            then
               return False;
            end if;

            Next_Elmt (Comp_Elmt);
         end loop;

         return True;

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);

         begin
            if No (U) then
               return False;
            else
               return Is_Fully_Initialized_Variant (U);
            end if;
         end;
      else
         return False;
      end if;
   end Is_Fully_Initialized_Variant;

   ----------------------------
   -- Is_Inherited_Operation --
   ----------------------------

   function Is_Inherited_Operation (E : Entity_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (Parent (E));
   begin
      pragma Assert (Is_Overloadable (E));
      return Kind = N_Full_Type_Declaration
        or else Kind = N_Private_Extension_Declaration
        or else Kind = N_Subtype_Declaration
        or else (Ekind (E) = E_Enumeration_Literal
                  and then Is_Derived_Type (Etype (E)));
   end Is_Inherited_Operation;

   -----------------------------
   -- Is_Library_Level_Entity --
   -----------------------------

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean is
   begin
      --  The following is a small optimization, and it also handles
      --  properly discriminals, which in task bodies might appear in
      --  expressions before the corresponding procedure has been
      --  created, and which therefore do not have an assigned scope.

      if Ekind (E) in Formal_Kind then
         return False;
      end if;

      --  Normal test is simply that the enclosing dynamic scope is Standard

      return Enclosing_Dynamic_Scope (E) = Standard_Standard;
   end Is_Library_Level_Entity;

   ---------------------------------
   -- Is_Local_Variable_Reference --
   ---------------------------------

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean is
   begin
      if not Is_Entity_Name (Expr) then
         return False;

      else
         declare
            Ent : constant Entity_Id := Entity (Expr);
            Sub : constant Entity_Id := Enclosing_Subprogram (Ent);
         begin
            if Ekind (Ent) /= E_Variable
                 and then
               Ekind (Ent) /= E_In_Out_Parameter
            then
               return False;
            else
               return Present (Sub) and then Sub = Current_Subprogram;
            end if;
         end;
      end if;
   end Is_Local_Variable_Reference;

   ---------------
   -- Is_Lvalue --
   ---------------

   function Is_Lvalue (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);

   begin
      case Nkind (P) is

         --  Test left side of assignment

         when N_Assignment_Statement =>
            return N = Name (P);

         --  Test prefix of component or attribute

         when N_Attribute_Reference  |
              N_Expanded_Name        |
              N_Explicit_Dereference |
              N_Indexed_Component    |
              N_Reference            |
              N_Selected_Component   |
              N_Slice                =>
            return N = Prefix (P);

         --  Test subprogram parameter (we really should check the
         --  parameter mode, but it is not worth the trouble)

         when N_Function_Call            |
              N_Procedure_Call_Statement |
              N_Accept_Statement         |
              N_Parameter_Association    =>
            return True;

         --  Test for appearing in a conversion that itself appears
         --  in an lvalue context, since this should be an lvalue.

         when N_Type_Conversion =>
            return Is_Lvalue (P);

         --  Test for appearence in object renaming declaration

         when N_Object_Renaming_Declaration =>
            return True;

         --  All other references are definitely not Lvalues

         when others =>
            return False;

      end case;
   end Is_Lvalue;

   -------------------------
   -- Is_Object_Reference --
   -------------------------

   function Is_Object_Reference (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Is_Object (Entity (N));

      else
         case Nkind (N) is
            when N_Indexed_Component | N_Slice =>
               return
                 Is_Object_Reference (Prefix (N))
                   or else Is_Access_Type (Etype (Prefix (N)));

            --  In Ada95, a function call is a constant object; a procedure
            --  call is not.

            when N_Function_Call =>
               return Etype (N) /= Standard_Void_Type;

            --  A reference to the stream attribute Input is a function call

            when N_Attribute_Reference =>
               return Attribute_Name (N) = Name_Input;

            when N_Selected_Component =>
               return
                 Is_Object_Reference (Selector_Name (N))
                   and then
                     (Is_Object_Reference (Prefix (N))
                        or else Is_Access_Type (Etype (Prefix (N))));

            when N_Explicit_Dereference =>
               return True;

            --  A view conversion of a tagged object is an object reference

            when N_Type_Conversion =>
               return Is_Tagged_Type (Etype (Subtype_Mark (N)))
                 and then Is_Tagged_Type (Etype (Expression (N)))
                 and then Is_Object_Reference (Expression (N));

            --  An unchecked type conversion is considered to be an object if
            --  the operand is an object (this construction arises only as a
            --  result of expansion activities).

            when N_Unchecked_Type_Conversion =>
               return True;

            when others =>
               return False;
         end case;
      end if;
   end Is_Object_Reference;

   -----------------------------------
   -- Is_OK_Variable_For_Out_Formal --
   -----------------------------------

   function Is_OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean is
   begin
      Note_Possible_Modification (AV);

      --  We must reject parenthesized variable names. The check for
      --  Comes_From_Source is present because there are currently
      --  cases where the compiler violates this rule (e.g. passing
      --  a task object to its controlled Initialize routine).

      if Paren_Count (AV) > 0 and then Comes_From_Source (AV) then
         return False;

      --  A variable is always allowed

      elsif Is_Variable (AV) then
         return True;

      --  Unchecked conversions are allowed only if they come from the
      --  generated code, which sometimes uses unchecked conversions for out
      --  parameters in cases where code generation is unaffected. We tell
      --  source unchecked conversions by seeing if they are rewrites of an
      --  original Unchecked_Conversion function call, or of an explicit
      --  conversion of a function call.

      elsif Nkind (AV) = N_Unchecked_Type_Conversion then
         if Nkind (Original_Node (AV)) = N_Function_Call then
            return False;

         elsif Comes_From_Source (AV)
           and then Nkind (Original_Node (Expression (AV))) = N_Function_Call
         then
            return False;

         elsif Nkind (Original_Node (AV)) = N_Type_Conversion then
            return Is_OK_Variable_For_Out_Formal (Expression (AV));

         else
            return True;
         end if;

      --  Normal type conversions are allowed if argument is a variable

      elsif Nkind (AV) = N_Type_Conversion then
         if Is_Variable (Expression (AV))
           and then Paren_Count (Expression (AV)) = 0
         then
            Note_Possible_Modification (Expression (AV));
            return True;

         --  We also allow a non-parenthesized expression that raises
         --  constraint error if it rewrites what used to be a variable

         elsif Raises_Constraint_Error (Expression (AV))
            and then Paren_Count (Expression (AV)) = 0
            and then Is_Variable (Original_Node (Expression (AV)))
         then
            return True;

         --  Type conversion of something other than a variable

         else
            return False;
         end if;

      --  If this node is rewritten, then test the original form, if that is
      --  OK, then we consider the rewritten node OK (for example, if the
      --  original node is a conversion, then Is_Variable will not be true
      --  but we still want to allow the conversion if it converts a variable).

      elsif Original_Node (AV) /= AV then
         return Is_OK_Variable_For_Out_Formal (Original_Node (AV));

      --  All other non-variables are rejected

      else
         return False;
      end if;
   end Is_OK_Variable_For_Out_Formal;

   -----------------------------------
   -- Is_Partially_Initialized_Type --
   -----------------------------------

   function Is_Partially_Initialized_Type (Typ : Entity_Id) return Boolean is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then

         --  If component type is partially initialized, so is array type

         if Is_Partially_Initialized_Type (Component_Type (Typ)) then
            return True;

         --  Otherwise we are only partially initialized if we are fully
         --  initialized (this is the empty array case, no point in us
         --  duplicating that code here).

         else
            return Is_Fully_Initialized_Type (Typ);
         end if;

      elsif Is_Record_Type (Typ) then

         --  A discriminated type is always partially initialized

         if Has_Discriminants (Typ) then
            return True;

         --  A tagged type is always partially initialized

         elsif Is_Tagged_Type (Typ) then
            return True;

         --  Case of non-discriminated record

         else
            declare
               Ent : Entity_Id;

               Component_Present : Boolean := False;
               --  Set True if at least one component is present. If no
               --  components are present, then record type is fully
               --  initialized (another odd case, like the null array).

            begin
               --  Loop through components

               Ent := First_Entity (Typ);
               while Present (Ent) loop
                  if Ekind (Ent) = E_Component then
                     Component_Present := True;

                     --  If a component has an initialization expression then
                     --  the enclosing record type is partially initialized

                     if Present (Parent (Ent))
                       and then Present (Expression (Parent (Ent)))
                     then
                        return True;

                     --  If a component is of a type which is itself partially
                     --  initialized, then the enclosing record type is also.

                     elsif Is_Partially_Initialized_Type (Etype (Ent)) then
                        return True;
                     end if;
                  end if;

                  Next_Entity (Ent);
               end loop;

               --  No initialized components found. If we found any components
               --  they were all uninitialized so the result is false.

               if Component_Present then
                  return False;

               --  But if we found no components, then all the components are
               --  initialized so we consider the type to be initialized.

               else
                  return True;
               end if;
            end;
         end if;

      --  Concurrent types are always fully initialized

      elsif Is_Concurrent_Type (Typ) then
         return True;

      --  For a private type, go to underlying type. If there is no underlying
      --  type then just assume this partially initialized. Not clear if this
      --  can happen in a non-error case, but no harm in testing for this.

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);
         begin
            if No (U) then
               return True;
            else
               return Is_Partially_Initialized_Type (U);
            end if;
         end;

      --  For any other type (are there any?) assume partially initialized

      else
         return True;
      end if;
   end Is_Partially_Initialized_Type;

   ------------------------------------
   -- Is_Potentially_Persistent_Type --
   ------------------------------------

   function Is_Potentially_Persistent_Type (T : Entity_Id) return Boolean is
      Comp : Entity_Id;
      Indx : Node_Id;

   begin
      --  For private type, test corrresponding full type

      if Is_Private_Type (T) then
         return Is_Potentially_Persistent_Type (Full_View (T));

      --  Scalar types are potentially persistent

      elsif Is_Scalar_Type (T) then
         return True;

      --  Record type is potentially persistent if not tagged and the types of
      --  all it components are potentially persistent, and no component has
      --  an initialization expression.

      elsif Is_Record_Type (T)
        and then not Is_Tagged_Type (T)
        and then not Is_Partially_Initialized_Type (T)
      then
         Comp := First_Component (T);
         while Present (Comp) loop
            if not Is_Potentially_Persistent_Type (Etype (Comp)) then
               return False;
            else
               Next_Entity (Comp);
            end if;
         end loop;

         return True;

      --  Array type is potentially persistent if its component type is
      --  potentially persistent and if all its constraints are static.

      elsif Is_Array_Type (T) then
         if not Is_Potentially_Persistent_Type (Component_Type (T)) then
            return False;
         end if;

         Indx := First_Index (T);
         while Present (Indx) loop
            if not Is_OK_Static_Subtype (Etype (Indx)) then
               return False;
            else
               Next_Index (Indx);
            end if;
         end loop;

         return True;

      --  All other types are not potentially persistent

      else
         return False;
      end if;
   end Is_Potentially_Persistent_Type;

   -----------------------------
   -- Is_RCI_Pkg_Spec_Or_Body --
   -----------------------------

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean is

      function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean;
      --  Return True if the unit of Cunit is an RCI package declaration

      ---------------------------
      -- Is_RCI_Pkg_Decl_Cunit --
      ---------------------------

      function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean is
         The_Unit : constant Node_Id := Unit (Cunit);

      begin
         if Nkind (The_Unit) /= N_Package_Declaration then
            return False;
         end if;

         return Is_Remote_Call_Interface (Defining_Entity (The_Unit));
      end Is_RCI_Pkg_Decl_Cunit;

   --  Start of processing for Is_RCI_Pkg_Spec_Or_Body

   begin
      return Is_RCI_Pkg_Decl_Cunit (Cunit)
        or else
         (Nkind (Unit (Cunit)) = N_Package_Body
           and then Is_RCI_Pkg_Decl_Cunit (Library_Unit (Cunit)));
   end Is_RCI_Pkg_Spec_Or_Body;

   -----------------------------------------
   -- Is_Remote_Access_To_Class_Wide_Type --
   -----------------------------------------

   function Is_Remote_Access_To_Class_Wide_Type
     (E : Entity_Id) return Boolean
   is
      D : Entity_Id;

      function Comes_From_Limited_Private_Type_Declaration
        (E : Entity_Id) return Boolean;
      --  Check that the type is declared by a limited type declaration,
      --  or else is derived from a Remote_Type ancestor through private
      --  extensions.

      -------------------------------------------------
      -- Comes_From_Limited_Private_Type_Declaration --
      -------------------------------------------------

      function Comes_From_Limited_Private_Type_Declaration
        (E : Entity_Id) return Boolean
      is
         N : constant Node_Id := Declaration_Node (E);

      begin
         if Nkind (N) = N_Private_Type_Declaration
           and then Limited_Present (N)
         then
            return True;
         end if;

         if Nkind (N) = N_Private_Extension_Declaration then
            return
              Comes_From_Limited_Private_Type_Declaration (Etype (E))
                or else
                 (Is_Remote_Types (Etype (E))
                    and then Is_Limited_Record (Etype (E))
                    and then Has_Private_Declaration (Etype (E)));
         end if;

         return False;
      end Comes_From_Limited_Private_Type_Declaration;

   --  Start of processing for Is_Remote_Access_To_Class_Wide_Type

   begin
      if not (Is_Remote_Call_Interface (E)
               or else Is_Remote_Types (E))
        or else Ekind (E) /= E_General_Access_Type
      then
         return False;
      end if;

      D := Designated_Type (E);

      if Ekind (D) /= E_Class_Wide_Type then
         return False;
      end if;

      return Comes_From_Limited_Private_Type_Declaration
               (Defining_Identifier (Parent (D)));
   end Is_Remote_Access_To_Class_Wide_Type;

   -----------------------------------------
   -- Is_Remote_Access_To_Subprogram_Type --
   -----------------------------------------

   function Is_Remote_Access_To_Subprogram_Type
     (E : Entity_Id) return Boolean
   is
   begin
      return (Ekind (E) = E_Access_Subprogram_Type
                or else (Ekind (E) = E_Record_Type
                           and then Present (Corresponding_Remote_Type (E))))
        and then (Is_Remote_Call_Interface (E)
                   or else Is_Remote_Types (E));
   end Is_Remote_Access_To_Subprogram_Type;

   --------------------
   -- Is_Remote_Call --
   --------------------

   function Is_Remote_Call (N : Node_Id) return Boolean is
   begin
      if Nkind (N) /= N_Procedure_Call_Statement
        and then Nkind (N) /= N_Function_Call
      then
         --  An entry call cannot be remote

         return False;

      elsif Nkind (Name (N)) in N_Has_Entity
        and then Is_Remote_Call_Interface (Entity (Name (N)))
      then
         --  A subprogram declared in the spec of a RCI package is remote

         return True;

      elsif Nkind (Name (N)) = N_Explicit_Dereference
        and then Is_Remote_Access_To_Subprogram_Type
                   (Etype (Prefix (Name (N))))
      then
         --  The dereference of a RAS is a remote call

         return True;

      elsif Present (Controlling_Argument (N))
        and then Is_Remote_Access_To_Class_Wide_Type
          (Etype (Controlling_Argument (N)))
      then
         --  Any primitive operation call with a controlling argument of
         --  a RACW type is a remote call.

         return True;
      end if;

      --  All other calls are local calls

      return False;
   end Is_Remote_Call;

   ----------------------
   -- Is_Renamed_Entry --
   ----------------------

   function Is_Renamed_Entry (Proc_Nam : Entity_Id) return Boolean is
      Orig_Node : Node_Id := Empty;
      Subp_Decl : Node_Id := Parent (Parent (Proc_Nam));

      function Is_Entry (Nam : Node_Id) return Boolean;
      --  Determine whether Nam is an entry. Traverse selectors
      --  if there are nested selected components.

      --------------
      -- Is_Entry --
      --------------

      function Is_Entry (Nam : Node_Id) return Boolean is
      begin
         if Nkind (Nam) = N_Selected_Component then
            return Is_Entry (Selector_Name (Nam));
         end if;

         return Ekind (Entity (Nam)) = E_Entry;
      end Is_Entry;

   --  Start of processing for Is_Renamed_Entry

   begin
      if Present (Alias (Proc_Nam)) then
         Subp_Decl := Parent (Parent (Alias (Proc_Nam)));
      end if;

      --  Look for a rewritten subprogram renaming declaration

      if Nkind (Subp_Decl) = N_Subprogram_Declaration
        and then Present (Original_Node (Subp_Decl))
      then
         Orig_Node := Original_Node (Subp_Decl);
      end if;

      --  The rewritten subprogram is actually an entry

      if Present (Orig_Node)
        and then Nkind (Orig_Node) = N_Subprogram_Renaming_Declaration
        and then Is_Entry (Name (Orig_Node))
      then
         return True;
      end if;

      return False;
   end Is_Renamed_Entry;

   ----------------------
   -- Is_Selector_Name --
   ----------------------

   function Is_Selector_Name (N : Node_Id) return Boolean is
   begin
      if not Is_List_Member (N) then
         declare
            P : constant Node_Id   := Parent (N);
            K : constant Node_Kind := Nkind (P);
         begin
            return
              (K = N_Expanded_Name          or else
               K = N_Generic_Association    or else
               K = N_Parameter_Association  or else
               K = N_Selected_Component)
              and then Selector_Name (P) = N;
         end;

      else
         declare
            L : constant List_Id := List_Containing (N);
            P : constant Node_Id := Parent (L);
         begin
            return (Nkind (P) = N_Discriminant_Association
                     and then Selector_Names (P) = L)
              or else
                   (Nkind (P) = N_Component_Association
                     and then Choices (P) = L);
         end;
      end if;
   end Is_Selector_Name;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (N : Node_Id) return Boolean is
   begin
      return
        Nkind (N) in N_Statement_Other_Than_Procedure_Call
          or else Nkind (N) = N_Procedure_Call_Statement;
   end Is_Statement;

   -----------------
   -- Is_Transfer --
   -----------------

   function Is_Transfer (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      if Kind = N_Return_Statement
           or else
         Kind = N_Goto_Statement
           or else
         Kind = N_Raise_Statement
           or else
         Kind = N_Requeue_Statement
      then
         return True;

      elsif (Kind = N_Exit_Statement or else Kind in N_Raise_xxx_Error)
        and then No (Condition (N))
      then
         return True;

      elsif Kind = N_Procedure_Call_Statement
        and then Is_Entity_Name (Name (N))
        and then Present (Entity (Name (N)))
        and then No_Return (Entity (Name (N)))
      then
         return True;

      elsif Nkind (Original_Node (N)) = N_Raise_Statement then
         return True;

      else
         return False;
      end if;
   end Is_Transfer;

   -------------
   -- Is_True --
   -------------

   function Is_True (U : Uint) return Boolean is
   begin
      return (U /= 0);
   end Is_True;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (N : Node_Id) return Boolean is

      Orig_Node : constant Node_Id := Original_Node (N);
      --  We do the test on the original node, since this is basically a
      --  test of syntactic categories, so it must not be disturbed by
      --  whatever rewriting might have occurred. For example, an aggregate,
      --  which is certainly NOT a variable, could be turned into a variable
      --  by expansion.

      function In_Protected_Function (E : Entity_Id) return Boolean;
      --  Within a protected function, the private components of the
      --  enclosing protected type are constants. A function nested within
      --  a (protected) procedure is not itself protected.

      function Is_Variable_Prefix (P : Node_Id) return Boolean;
      --  Prefixes can involve implicit dereferences, in which case we
      --  must test for the case of a reference of a constant access
      --  type, which can never be a variable.

      ---------------------------
      -- In_Protected_Function --
      ---------------------------

      function In_Protected_Function (E : Entity_Id) return Boolean is
         Prot : constant Entity_Id := Scope (E);
         S    : Entity_Id;

      begin
         if not Is_Protected_Type (Prot) then
            return False;
         else
            S := Current_Scope;
            while Present (S) and then S /= Prot loop
               if Ekind (S) = E_Function
                 and then Scope (S) = Prot
               then
                  return True;
               end if;

               S := Scope (S);
            end loop;

            return False;
         end if;
      end In_Protected_Function;

      ------------------------
      -- Is_Variable_Prefix --
      ------------------------

      function Is_Variable_Prefix (P : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (P)) then
            return not Is_Access_Constant (Root_Type (Etype (P)));

         --  For the case of an indexed component whose prefix has a packed
         --  array type, the prefix has been rewritten into a type conversion.
         --  Determine variable-ness from the converted expression.

         elsif Nkind (P) = N_Type_Conversion
           and then not Comes_From_Source (P)
           and then Is_Array_Type (Etype (P))
           and then Is_Packed (Etype (P))
         then
            return Is_Variable (Expression (P));

         else
            return Is_Variable (P);
         end if;
      end Is_Variable_Prefix;

   --  Start of processing for Is_Variable

   begin
      --  Definitely OK if Assignment_OK is set. Since this is something that
      --  only gets set for expanded nodes, the test is on N, not Orig_Node.

      if Nkind (N) in N_Subexpr and then Assignment_OK (N) then
         return True;

      --  Normally we go to the original node, but there is one exception
      --  where we use the rewritten node, namely when it is an explicit
      --  dereference. The generated code may rewrite a prefix which is an
      --  access type with an explicit dereference. The dereference is a
      --  variable, even though the original node may not be (since it could
      --  be a constant of the access type).

      elsif Nkind (N) = N_Explicit_Dereference
        and then Nkind (Orig_Node) /= N_Explicit_Dereference
        and then Is_Access_Type (Etype (Orig_Node))
      then
         return Is_Variable_Prefix (Original_Node (Prefix (N)));

      --  A function call is never a variable

      elsif Nkind (N) = N_Function_Call then
         return False;

      --  All remaining checks use the original node

      elsif Is_Entity_Name (Orig_Node) then
         declare
            E : constant Entity_Id := Entity (Orig_Node);
            K : constant Entity_Kind := Ekind (E);

         begin
            return (K = E_Variable
                      and then Nkind (Parent (E)) /= N_Exception_Handler)
              or else  (K = E_Component
                          and then not In_Protected_Function (E))
              or else  K = E_Out_Parameter
              or else  K = E_In_Out_Parameter
              or else  K = E_Generic_In_Out_Parameter

               --  Current instance of type:

              or else (Is_Type (E) and then In_Open_Scopes (E))
              or else (Is_Incomplete_Or_Private_Type (E)
                        and then In_Open_Scopes (Full_View (E)));
         end;

      else
         case Nkind (Orig_Node) is
            when N_Indexed_Component | N_Slice =>
               return Is_Variable_Prefix (Prefix (Orig_Node));

            when N_Selected_Component =>
               return Is_Variable_Prefix (Prefix (Orig_Node))
                 and then Is_Variable (Selector_Name (Orig_Node));

            --  For an explicit dereference, the type of the prefix cannot
            --  be an access to constant or an access to subprogram.

            when N_Explicit_Dereference =>
               declare
                  Typ : constant Entity_Id := Etype (Prefix (Orig_Node));
               begin
                  return Is_Access_Type (Typ)
                    and then not Is_Access_Constant (Root_Type (Typ))
                    and then Ekind (Typ) /= E_Access_Subprogram_Type;
               end;

            --  The type conversion is the case where we do not deal with the
            --  context dependent special case of an actual parameter. Thus
            --  the type conversion is only considered a variable for the
            --  purposes of this routine if the target type is tagged. However,
            --  a type conversion is considered to be a variable if it does not
            --  come from source (this deals for example with the conversions
            --  of expressions to their actual subtypes).

            when N_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node))
                 and then
                   (not Comes_From_Source (Orig_Node)
                      or else
                        (Is_Tagged_Type (Etype (Subtype_Mark (Orig_Node)))
                          and then
                         Is_Tagged_Type (Etype (Expression (Orig_Node)))));

            --  GNAT allows an unchecked type conversion as a variable. This
            --  only affects the generation of internal expanded code, since
            --  calls to instantiations of Unchecked_Conversion are never
            --  considered variables (since they are function calls).
            --  This is also true for expression actions.

            when N_Unchecked_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node));

            when others =>
               return False;
         end case;
      end if;
   end Is_Variable;

   ------------------------
   -- Is_Volatile_Object --
   ------------------------

   function Is_Volatile_Object (N : Node_Id) return Boolean is

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean;
      --  Determines if given object has volatile components

      function Is_Volatile_Prefix (N : Node_Id) return Boolean;
      --  If prefix is an implicit dereference, examine designated type

      ------------------------
      -- Is_Volatile_Prefix --
      ------------------------

      function Is_Volatile_Prefix (N : Node_Id) return Boolean is
         Typ  : constant Entity_Id := Etype (N);

      begin
         if Is_Access_Type (Typ) then
            declare
               Dtyp : constant Entity_Id := Designated_Type (Typ);

            begin
               return Is_Volatile (Dtyp)
                 or else Has_Volatile_Components (Dtyp);
            end;

         else
            return Object_Has_Volatile_Components (N);
         end if;
      end Is_Volatile_Prefix;

      ------------------------------------
      -- Object_Has_Volatile_Components --
      ------------------------------------

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean is
         Typ : constant Entity_Id := Etype (N);

      begin
         if Is_Volatile (Typ)
           or else Has_Volatile_Components (Typ)
         then
            return True;

         elsif Is_Entity_Name (N)
           and then (Has_Volatile_Components (Entity (N))
                      or else Is_Volatile (Entity (N)))
         then
            return True;

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            return Is_Volatile_Prefix (Prefix (N));

         else
            return False;
         end if;
      end Object_Has_Volatile_Components;

   --  Start of processing for Is_Volatile_Object

   begin
      if Is_Volatile (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Volatile (Entity (N)))
      then
         return True;

      elsif Nkind (N) = N_Indexed_Component
        or else Nkind (N) = N_Selected_Component
      then
         return Is_Volatile_Prefix (Prefix (N));

      else
         return False;
      end if;
   end Is_Volatile_Object;

   -------------------------
   -- Kill_Current_Values --
   -------------------------

   procedure Kill_Current_Values (Ent : Entity_Id) is
   begin
      if Is_Object (Ent) then
         Kill_Checks (Ent);
         Set_Current_Value (Ent, Empty);

         if not Can_Never_Be_Null (Ent) then
            Set_Is_Known_Non_Null (Ent, False);
         end if;

         Set_Is_Known_Null (Ent, False);
      end if;
   end Kill_Current_Values;

   procedure Kill_Current_Values is
      S : Entity_Id;

      procedure Kill_Current_Values_For_Entity_Chain (E : Entity_Id);
      --  Clear current value for entity E and all entities chained to E

      ------------------------------------------
      -- Kill_Current_Values_For_Entity_Chain --
      ------------------------------------------

      procedure Kill_Current_Values_For_Entity_Chain (E : Entity_Id) is
         Ent : Entity_Id;
      begin
         Ent := E;
         while Present (Ent) loop
            Kill_Current_Values (Ent);
            Next_Entity (Ent);
         end loop;
      end Kill_Current_Values_For_Entity_Chain;

   --  Start of processing for Kill_Current_Values

   begin
      --  Kill all saved checks, a special case of killing saved values

      Kill_All_Checks;

      --  Loop through relevant scopes, which includes the current scope and
      --  any parent scopes if the current scope is a block or a package.

      S := Current_Scope;
      Scope_Loop : loop

         --  Clear current values of all entities in current scope

         Kill_Current_Values_For_Entity_Chain (First_Entity (S));

         --  If scope is a package, also clear current values of all
         --  private entities in the scope.

         if Ekind (S) = E_Package
              or else
            Ekind (S) = E_Generic_Package
              or else
            Is_Concurrent_Type (S)
         then
            Kill_Current_Values_For_Entity_Chain (First_Private_Entity (S));
         end if;

         --  If this is a block or nested package, deal with parent

         if Ekind (S) = E_Block
           or else (Ekind (S) = E_Package
                      and then not Is_Library_Level_Entity (S))
         then
            S := Scope (S);
         else
            exit Scope_Loop;
         end if;
      end loop Scope_Loop;
   end Kill_Current_Values;

   --------------------------
   -- Kill_Size_Check_Code --
   --------------------------

   procedure Kill_Size_Check_Code (E : Entity_Id) is
   begin
      if (Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
        and then Present (Size_Check_Code (E))
      then
         Remove (Size_Check_Code (E));
         Set_Size_Check_Code (E, Empty);
      end if;
   end Kill_Size_Check_Code;

   -------------------------
   -- New_External_Entity --
   -------------------------

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ') return Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value,
              New_External_Name
                (Chars (Related_Id), Suffix, Suffix_Index, Prefix));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);
      Set_Public_Status  (N);

      if Kind in Type_Kind then
         Init_Size_Align (N);
      end if;

      return N;
   end New_External_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character) return Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value, New_Internal_Name (Id_Char));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);

      if Kind in Type_Kind then
         Init_Size_Align (N);
      end if;

      return N;
   end New_Internal_Entity;

   -----------------
   -- Next_Actual --
   -----------------

   function Next_Actual (Actual_Id : Node_Id) return Node_Id is
      N  : Node_Id;

   begin
      --  If we are pointing at a positional parameter, it is a member of
      --  a node list (the list of parameters), and the next parameter
      --  is the next node on the list, unless we hit a parameter
      --  association, in which case we shift to using the chain whose
      --  head is the First_Named_Actual in the parent, and then is
      --  threaded using the Next_Named_Actual of the Parameter_Association.
      --  All this fiddling is because the original node list is in the
      --  textual call order, and what we need is the declaration order.

      if Is_List_Member (Actual_Id) then
         N := Next (Actual_Id);

         if Nkind (N) = N_Parameter_Association then
            return First_Named_Actual (Parent (Actual_Id));
         else
            return N;
         end if;

      else
         return Next_Named_Actual (Parent (Actual_Id));
      end if;
   end Next_Actual;

   procedure Next_Actual (Actual_Id : in out Node_Id) is
   begin
      Actual_Id := Next_Actual (Actual_Id);
   end Next_Actual;

   -----------------------
   -- Normalize_Actuals --
   -----------------------

   --  Chain actuals according to formals of subprogram. If there are no named
   --  associations, the chain is simply the list of Parameter Associations,
   --  since the order is the same as the declaration order. If there are named
   --  associations, then the First_Named_Actual field in the N_Function_Call
   --  or N_Procedure_Call_Statement node points to the Parameter_Association
   --  node for the parameter that comes first in declaration order. The
   --  remaining named parameters are then chained in declaration order using
   --  Next_Named_Actual.

   --  This routine also verifies that the number of actuals is compatible with
   --  the number and default values of formals, but performs no type checking
   --  (type checking is done by the caller).

   --  If the matching succeeds, Success is set to True and the caller proceeds
   --  with type-checking. If the match is unsuccessful, then Success is set to
   --  False, and the caller attempts a different interpretation, if there is
   --  one.

   --  If the flag Report is on, the call is not overloaded, and a failure to
   --  match can be reported here, rather than in the caller.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean)
   is
      Actuals     : constant List_Id := Parameter_Associations (N);
      Actual      : Node_Id   := Empty;
      Formal      : Entity_Id;
      Last        : Node_Id := Empty;
      First_Named : Node_Id := Empty;
      Found       : Boolean;

      Formals_To_Match : Integer := 0;
      Actuals_To_Match : Integer := 0;

      procedure Chain (A : Node_Id);
      --  Add named actual at the proper place in the list, using the
      --  Next_Named_Actual link.

      function Reporting return Boolean;
      --  Determines if an error is to be reported. To report an error, we
      --  need Report to be True, and also we do not report errors caused
      --  by calls to init procs that occur within other init procs. Such
      --  errors must always be cascaded errors, since if all the types are
      --  declared correctly, the compiler will certainly build decent calls!

      -----------
      -- Chain --
      -----------

      procedure Chain (A : Node_Id) is
      begin
         if No (Last) then

            --  Call node points to first actual in list

            Set_First_Named_Actual (N, Explicit_Actual_Parameter (A));

         else
            Set_Next_Named_Actual (Last, Explicit_Actual_Parameter (A));
         end if;

         Last := A;
         Set_Next_Named_Actual (Last, Empty);
      end Chain;

      ---------------
      -- Reporting --
      ---------------

      function Reporting return Boolean is
      begin
         if not Report then
            return False;

         elsif not Within_Init_Proc then
            return True;

         elsif Is_Init_Proc (Entity (Name (N))) then
            return False;

         else
            return True;
         end if;
      end Reporting;

   --  Start of processing for Normalize_Actuals

   begin
      if Is_Access_Type (S) then

         --  The name in the call is a function call that returns an access
         --  to subprogram. The designated type has the list of formals.

         Formal := First_Formal (Designated_Type (S));
      else
         Formal := First_Formal (S);
      end if;

      while Present (Formal) loop
         Formals_To_Match := Formals_To_Match + 1;
         Next_Formal (Formal);
      end loop;

      --  Find if there is a named association, and verify that no positional
      --  associations appear after named ones.

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      while Present (Actual)
        and then Nkind (Actual) /= N_Parameter_Association
      loop
         Actuals_To_Match := Actuals_To_Match + 1;
         Next (Actual);
      end loop;

      if No (Actual) and Actuals_To_Match = Formals_To_Match then

         --  Most common case: positional notation, no defaults

         Success := True;
         return;

      elsif Actuals_To_Match > Formals_To_Match then

         --  Too many actuals: will not work

         if Reporting then
            if Is_Entity_Name (Name (N)) then
               Error_Msg_N ("too many arguments in call to&", Name (N));
            else
               Error_Msg_N ("too many arguments in call", N);
            end if;
         end if;

         Success := False;
         return;
      end if;

      First_Named := Actual;

      while Present (Actual) loop
         if Nkind (Actual) /= N_Parameter_Association then
            Error_Msg_N
              ("positional parameters not allowed after named ones", Actual);
            Success := False;
            return;

         else
            Actuals_To_Match := Actuals_To_Match + 1;
         end if;

         Next (Actual);
      end loop;

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      Formal := First_Formal (S);
      while Present (Formal) loop

         --  Match the formals in order. If the corresponding actual
         --  is positional,  nothing to do. Else scan the list of named
         --  actuals to find the one with the right name.

         if Present (Actual)
           and then Nkind (Actual) /= N_Parameter_Association
         then
            Next (Actual);
            Actuals_To_Match := Actuals_To_Match - 1;
            Formals_To_Match := Formals_To_Match - 1;

         else
            --  For named parameters, search the list of actuals to find
            --  one that matches the next formal name.

            Actual := First_Named;
            Found  := False;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (Formal) then
                  Found := True;
                  Chain (Actual);
                  Actuals_To_Match := Actuals_To_Match - 1;
                  Formals_To_Match := Formals_To_Match - 1;
                  exit;
               end if;

               Next (Actual);
            end loop;

            if not Found then
               if Ekind (Formal) /= E_In_Parameter
                 or else No (Default_Value (Formal))
               then
                  if Reporting then
                     if (Comes_From_Source (S)
                          or else Sloc (S) = Standard_Location)
                       and then Is_Overloadable (S)
                     then
                        if No (Actuals)
                          and then
                           (Nkind (Parent (N)) = N_Procedure_Call_Statement
                             or else
                           (Nkind (Parent (N)) = N_Function_Call
                             or else
                            Nkind (Parent (N)) = N_Parameter_Association))
                          and then Ekind (S) /= E_Function
                        then
                           Set_Etype (N, Etype (S));
                        else
                           Error_Msg_Name_1 := Chars (S);
                           Error_Msg_Sloc := Sloc (S);
                           Error_Msg_NE
                             ("missing argument for parameter & " &
                                "in call to % declared #", N, Formal);
                        end if;

                     elsif Is_Overloadable (S) then
                        Error_Msg_Name_1 := Chars (S);

                        --  Point to type derivation that generated the
                        --  operation.

                        Error_Msg_Sloc := Sloc (Parent (S));

                        Error_Msg_NE
                          ("missing argument for parameter & " &
                             "in call to % (inherited) #", N, Formal);

                     else
                        Error_Msg_NE
                          ("missing argument for parameter &", N, Formal);
                     end if;
                  end if;

                  Success := False;
                  return;

               else
                  Formals_To_Match := Formals_To_Match - 1;
               end if;
            end if;
         end if;

         Next_Formal (Formal);
      end loop;

      if Formals_To_Match = 0 and then Actuals_To_Match = 0 then
         Success := True;
         return;

      else
         if Reporting then

            --  Find some superfluous named actual that did not get
            --  attached to the list of associations.

            Actual := First (Actuals);

            while Present (Actual) loop
               if Nkind (Actual) = N_Parameter_Association
                 and then Actual /= Last
                 and then No (Next_Named_Actual (Actual))
               then
                  Error_Msg_N ("unmatched actual & in call",
                    Selector_Name (Actual));
                  exit;
               end if;

               Next (Actual);
            end loop;
         end if;

         Success := False;
         return;
      end if;
   end Normalize_Actuals;

   --------------------------------
   -- Note_Possible_Modification --
   --------------------------------

   procedure Note_Possible_Modification (N : Node_Id) is
      Modification_Comes_From_Source : constant Boolean :=
                                         Comes_From_Source (Parent (N));

      Ent : Entity_Id;
      Exp : Node_Id;

   begin
      --  Loop to find referenced entity, if there is one

      Exp := N;
      loop
         <<Continue>>
         Ent := Empty;

         if Is_Entity_Name (Exp) then
            Ent := Entity (Exp);

            --  If the entity is missing, it is an undeclared identifier,
            --  and there is nothing to annotate.

            if No (Ent) then
               return;
            end if;

         elsif Nkind (Exp) = N_Explicit_Dereference then
            declare
               P : constant Node_Id := Prefix (Exp);

            begin
               if Nkind (P) = N_Selected_Component
                 and then Present (
                   Entry_Formal (Entity (Selector_Name (P))))
               then
                  --  Case of a reference to an entry formal

                  Ent := Entry_Formal (Entity (Selector_Name (P)));

               elsif Nkind (P) = N_Identifier
                 and then Nkind (Parent (Entity (P))) = N_Object_Declaration
                 and then Present (Expression (Parent (Entity (P))))
                 and then Nkind (Expression (Parent (Entity (P))))
                   = N_Reference
               then
                  --  Case of a reference to a value on which
                  --  side effects have been removed.

                  Exp := Prefix (Expression (Parent (Entity (P))));
                  goto Continue;

               else
                  return;

               end if;
            end;

         elsif     Nkind (Exp) = N_Type_Conversion
           or else Nkind (Exp) = N_Unchecked_Type_Conversion
         then
            Exp := Expression (Exp);
            goto Continue;

         elsif     Nkind (Exp) = N_Slice
           or else Nkind (Exp) = N_Indexed_Component
           or else Nkind (Exp) = N_Selected_Component
         then
            Exp := Prefix (Exp);
            goto Continue;

         else
            return;
         end if;

         --  Now look for entity being referenced

         if Present (Ent) then
            if Is_Object (Ent) then
               if Comes_From_Source (Exp)
                 or else Modification_Comes_From_Source
               then
                  Set_Never_Set_In_Source (Ent, False);
               end if;

               Set_Is_True_Constant (Ent, False);
               Set_Current_Value    (Ent, Empty);
               Set_Is_Known_Null    (Ent, False);

               if not Can_Never_Be_Null (Ent) then
                  Set_Is_Known_Non_Null (Ent, False);
               end if;

               --  Follow renaming chain

               if (Ekind (Ent) = E_Variable or else Ekind (Ent) = E_Constant)
                 and then Present (Renamed_Object (Ent))
               then
                  Exp := Renamed_Object (Ent);
                  goto Continue;
               end if;

               --  Generate a reference only if the assignment comes from
               --  source. This excludes, for example, calls to a dispatching
               --  assignment operation when the left-hand side is tagged.

               if Modification_Comes_From_Source then
                  Generate_Reference (Ent, Exp, 'm');
               end if;
            end if;

            Kill_Checks (Ent);
            return;
         end if;
      end loop;
   end Note_Possible_Modification;

   -------------------------
   -- Object_Access_Level --
   -------------------------

   function Object_Access_Level (Obj : Node_Id) return Uint is
      E : Entity_Id;

   --  Returns the static accessibility level of the view denoted
   --  by Obj.  Note that the value returned is the result of a
   --  call to Scope_Depth.  Only scope depths associated with
   --  dynamic scopes can actually be returned.  Since only
   --  relative levels matter for accessibility checking, the fact
   --  that the distance between successive levels of accessibility
   --  is not always one is immaterial (invariant: if level(E2) is
   --  deeper than level(E1), then Scope_Depth(E1) < Scope_Depth(E2)).

   begin
      if Is_Entity_Name (Obj) then
         E := Entity (Obj);

         --  If E is a type then it denotes a current instance.
         --  For this case we add one to the normal accessibility
         --  level of the type to ensure that current instances
         --  are treated as always being deeper than than the level
         --  of any visible named access type (see 3.10.2(21)).

         if Is_Type (E) then
            return Type_Access_Level (E) +  1;

         elsif Present (Renamed_Object (E)) then
            return Object_Access_Level (Renamed_Object (E));

         --  Similarly, if E is a component of the current instance of a
         --  protected type, any instance of it is assumed to be at a deeper
         --  level than the type. For a protected object (whose type is an
         --  anonymous protected type) its components are at the same level
         --  as the type itself.

         elsif not Is_Overloadable (E)
           and then Ekind (Scope (E)) = E_Protected_Type
           and then Comes_From_Source (Scope (E))
         then
            return Type_Access_Level (Scope (E)) + 1;

         else
            return Scope_Depth (Enclosing_Dynamic_Scope (E));
         end if;

      elsif Nkind (Obj) = N_Selected_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Indexed_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Explicit_Dereference then

         --  If the prefix is a selected access discriminant then
         --  we make a recursive call on the prefix, which will
         --  in turn check the level of the prefix object of
         --  the selected discriminant.

         if Nkind (Prefix (Obj)) = N_Selected_Component
           and then Ekind (Etype (Prefix (Obj))) = E_Anonymous_Access_Type
           and then
             Ekind (Entity (Selector_Name (Prefix (Obj)))) = E_Discriminant
         then
            return Object_Access_Level (Prefix (Obj));
         else
            return Type_Access_Level (Etype (Prefix (Obj)));
         end if;

      elsif Nkind (Obj) = N_Type_Conversion
        or else Nkind (Obj) = N_Unchecked_Type_Conversion
      then
         return Object_Access_Level (Expression (Obj));

      --  Function results are objects, so we get either the access level
      --  of the function or, in the case of an indirect call, the level of
      --  of the access-to-subprogram type.

      elsif Nkind (Obj) = N_Function_Call then
         if Is_Entity_Name (Name (Obj)) then
            return Subprogram_Access_Level (Entity (Name (Obj)));
         else
            return Type_Access_Level (Etype (Prefix (Name (Obj))));
         end if;

      --  For convenience we handle qualified expressions, even though
      --  they aren't technically object names.

      elsif Nkind (Obj) = N_Qualified_Expression then
         return Object_Access_Level (Expression (Obj));

      --  Otherwise return the scope level of Standard.
      --  (If there are cases that fall through
      --  to this point they will be treated as
      --  having global accessibility for now. ???)

      else
         return Scope_Depth (Standard_Standard);
      end if;
   end Object_Access_Level;

   -----------------------
   -- Private_Component --
   -----------------------

   function Private_Component (Type_Id : Entity_Id) return Entity_Id is
      Ancestor  : constant Entity_Id := Base_Type (Type_Id);

      function Trace_Components
        (T     : Entity_Id;
         Check : Boolean) return Entity_Id;
      --  Recursive function that does the work, and checks against circular
      --  definition for each subcomponent type.

      ----------------------
      -- Trace_Components --
      ----------------------

      function Trace_Components
         (T     : Entity_Id;
          Check : Boolean) return Entity_Id
       is
         Btype     : constant Entity_Id := Base_Type (T);
         Component : Entity_Id;
         P         : Entity_Id;
         Candidate : Entity_Id := Empty;

      begin
         if Check and then Btype = Ancestor then
            Error_Msg_N ("circular type definition", Type_Id);
            return Any_Type;
         end if;

         if Is_Private_Type (Btype)
           and then not Is_Generic_Type (Btype)
         then
            if Present (Full_View (Btype))
              and then Is_Record_Type (Full_View (Btype))
              and then not Is_Frozen (Btype)
            then
               --  To indicate that the ancestor depends on a private type,
               --  the current Btype is sufficient. However, to check for
               --  circular definition we must recurse on the full view.

               Candidate := Trace_Components (Full_View (Btype), True);

               if Candidate = Any_Type then
                  return Any_Type;
               else
                  return Btype;
               end if;

            else
               return Btype;
            end if;

         elsif Is_Array_Type (Btype) then
            return Trace_Components (Component_Type (Btype), True);

         elsif Is_Record_Type (Btype) then
            Component := First_Entity (Btype);
            while Present (Component) loop

               --  Skip anonymous types generated by constrained components

               if not Is_Type (Component) then
                  P := Trace_Components (Etype (Component), True);

                  if Present (P) then
                     if P = Any_Type then
                        return P;
                     else
                        Candidate := P;
                     end if;
                  end if;
               end if;

               Next_Entity (Component);
            end loop;

            return Candidate;

         else
            return Empty;
         end if;
      end Trace_Components;

   --  Start of processing for Private_Component

   begin
      return Trace_Components (Type_Id, False);
   end Private_Component;

   -----------------------
   -- Process_End_Label --
   -----------------------

   procedure Process_End_Label
     (N   : Node_Id;
      Typ : Character;
      Ent  : Entity_Id)
   is
      Loc  : Source_Ptr;
      Nam  : Node_Id;

      Label_Ref : Boolean;
      --  Set True if reference to end label itself is required

      Endl : Node_Id;
      --  Gets set to the operator symbol or identifier that references
      --  the entity Ent. For the child unit case, this is the identifier
      --  from the designator. For other cases, this is simply Endl.

      procedure Generate_Parent_Ref (N : Node_Id);
      --  N is an identifier node that appears as a parent unit reference
      --  in the case where Ent is a child unit. This procedure generates
      --  an appropriate cross-reference entry.

      -------------------------
      -- Generate_Parent_Ref --
      -------------------------

      procedure Generate_Parent_Ref (N : Node_Id) is
         Parent_Ent : Entity_Id;

      begin
         --  Search up scope stack. The reason we do this is that normal
         --  visibility analysis would not work for two reasons. First in
         --  some subunit cases, the entry for the parent unit may not be
         --  visible, and in any case there can be a local entity that
         --  hides the scope entity.

         Parent_Ent := Current_Scope;
         while Present (Parent_Ent) loop
            if Chars (Parent_Ent) = Chars (N) then

               --  Generate the reference. We do NOT consider this as a
               --  reference for unreferenced symbol purposes, but we do
               --  force a cross-reference even if the end line does not
               --  come from source (the caller already generated the
               --  appropriate Typ for this situation).

               Generate_Reference
                 (Parent_Ent, N, 'r', Set_Ref => False, Force => True);
               Style.Check_Identifier (N, Parent_Ent);
               return;
            end if;

            Parent_Ent := Scope (Parent_Ent);
         end loop;

         --  Fall through means entity was not found -- that's odd, but
         --  the appropriate thing is simply to ignore and not generate
         --  any cross-reference for this entry.

         return;
      end Generate_Parent_Ref;

   --  Start of processing for Process_End_Label

   begin
      --  If no node, ignore. This happens in some error situations,
      --  and also for some internally generated structures where no
      --  end label references are required in any case.

      if No (N) then
         return;
      end if;

      --  Nothing to do if no End_Label, happens for internally generated
      --  constructs where we don't want an end label reference anyway.
      --  Also nothing to do if Endl is a string literal, which means
      --  there was some prior error (bad operator symbol)

      Endl := End_Label (N);

      if No (Endl) or else Nkind (Endl) = N_String_Literal then
         return;
      end if;

      --  Reference node is not in extended main source unit

      if not In_Extended_Main_Source_Unit (N) then

         --  Generally we do not collect references except for the
         --  extended main source unit. The one exception is the 'e'
         --  entry for a package spec, where it is useful for a client
         --  to have the ending information to define scopes.

         if Typ /= 'e' then
            return;

         else
            Label_Ref := False;

            --  For this case, we can ignore any parent references,
            --  but we need the package name itself for the 'e' entry.

            if Nkind (Endl) = N_Designator then
               Endl := Identifier (Endl);
            end if;
         end if;

      --  Reference is in extended main source unit

      else
         Label_Ref := True;

         --  For designator, generate references for the parent entries

         if Nkind (Endl) = N_Designator then

            --  Generate references for the prefix if the END line comes
            --  from source (otherwise we do not need these references)

            if Comes_From_Source (Endl) then
               Nam := Name (Endl);
               while Nkind (Nam) = N_Selected_Component loop
                  Generate_Parent_Ref (Selector_Name (Nam));
                  Nam := Prefix (Nam);
               end loop;

               Generate_Parent_Ref (Nam);
            end if;

            Endl := Identifier (Endl);
         end if;
      end if;

      --  If the end label is not for the given entity, then either we have
      --  some previous error, or this is a generic instantiation for which
      --  we do not need to make a cross-reference in this case anyway. In
      --  either case we simply ignore the call.

      if Chars (Ent) /= Chars (Endl) then
         return;
      end if;

      --  If label was really there, then generate a normal reference
      --  and then adjust the location in the end label to point past
      --  the name (which should almost always be the semicolon).

      Loc := Sloc (Endl);

      if Comes_From_Source (Endl) then

         --  If a label reference is required, then do the style check
         --  and generate an l-type cross-reference entry for the label

         if Label_Ref then
            if Style_Check then
               Style.Check_Identifier (Endl, Ent);
            end if;
            Generate_Reference (Ent, Endl, 'l', Set_Ref => False);
         end if;

         --  Set the location to point past the label (normally this will
         --  mean the semicolon immediately following the label). This is
         --  done for the sake of the 'e' or 't' entry generated below.

         Get_Decoded_Name_String (Chars (Endl));
         Set_Sloc (Endl, Sloc (Endl) + Source_Ptr (Name_Len));
      end if;

      --  Now generate the e/t reference

      Generate_Reference (Ent, Endl, Typ, Set_Ref => False, Force => True);

      --  Restore Sloc, in case modified above, since we have an identifier
      --  and the normal Sloc should be left set in the tree.

      Set_Sloc (Endl, Loc);
   end Process_End_Label;

   ------------------
   -- Real_Convert --
   ------------------

   --  We do the conversion to get the value of the real string by using
   --  the scanner, see Sinput for details on use of the internal source
   --  buffer for scanning internal strings.

   function Real_Convert (S : String) return Node_Id is
      Save_Src : constant Source_Buffer_Ptr := Source;
      Negative : Boolean;

   begin
      Source := Internal_Source_Ptr;
      Scan_Ptr := 1;

      for J in S'Range loop
         Source (Source_Ptr (J)) := S (J);
      end loop;

      Source (S'Length + 1) := EOF;

      if Source (Scan_Ptr) = '-' then
         Negative := True;
         Scan_Ptr := Scan_Ptr + 1;
      else
         Negative := False;
      end if;

      Scan;

      if Negative then
         Set_Realval (Token_Node, UR_Negate (Realval (Token_Node)));
      end if;

      Source := Save_Src;
      return Token_Node;
   end Real_Convert;

   ---------------------
   -- Rep_To_Pos_Flag --
   ---------------------

   function Rep_To_Pos_Flag (E : Entity_Id; Loc : Source_Ptr) return Node_Id is
   begin
      return New_Occurrence_Of
               (Boolean_Literals (not Range_Checks_Suppressed (E)), Loc);
   end Rep_To_Pos_Flag;

   --------------------
   -- Require_Entity --
   --------------------

   procedure Require_Entity (N : Node_Id) is
   begin
      if Is_Entity_Name (N) and then No (Entity (N)) then
         if Total_Errors_Detected /= 0 then
            Set_Entity (N, Any_Id);
         else
            raise Program_Error;
         end if;
      end if;
   end Require_Entity;

   ------------------------------
   -- Requires_Transient_Scope --
   ------------------------------

   --  A transient scope is required when variable-sized temporaries are
   --  allocated in the primary or secondary stack, or when finalization
   --  actions must be generated before the next instruction.

   function Requires_Transient_Scope (Id : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Underlying_Type (Id);

   --  Start of processing for Requires_Transient_Scope

   begin
      --  This is a private type which is not completed yet. This can only
      --  happen in a default expression (of a formal parameter or of a
      --  record component). Do not expand transient scope in this case

      if No (Typ) then
         return False;

      --  Do not expand transient scope for non-existent procedure return

      elsif Typ = Standard_Void_Type then
         return False;

      --  Elementary types do not require a transient scope

      elsif Is_Elementary_Type (Typ) then
         return False;

      --  Generally, indefinite subtypes require a transient scope, since the
      --  back end cannot generate temporaries, since this is not a valid type
      --  for declaring an object. It might be possible to relax this in the
      --  future, e.g. by declaring the maximum possible space for the type.

      elsif Is_Indefinite_Subtype (Typ) then
         return True;

      --  Functions returning tagged types may dispatch on result so their
      --  returned value is allocated on the secondary stack. Controlled
      --  type temporaries need finalization.

      elsif Is_Tagged_Type (Typ)
        or else Has_Controlled_Component (Typ)
      then
         return True;

      --  Record type

      elsif Is_Record_Type (Typ) then

         --  In GCC 2, discriminated records always require a transient
         --  scope because the back end otherwise tries to allocate a
         --  variable length temporary for the particular variant.

         if Opt.GCC_Version = 2
           and then Has_Discriminants (Typ)
         then
            return True;

         --  For GCC 3, or for a non-discriminated record in GCC 2, we are
         --  OK if none of the component types requires a transient scope.
         --  Note that we already know that this is a definite type (i.e.
         --  has discriminant defaults if it is a discriminated record).

         else
            declare
               Comp : Entity_Id;
            begin
               Comp := First_Entity (Typ);
               while Present (Comp) loop
                  if Ekind (Comp) = E_Component
                     and then Requires_Transient_Scope (Etype (Comp))
                  then
                     return True;
                  else
                     Next_Entity (Comp);
                  end if;
               end loop;
            end;

            return False;
         end if;

      --  String literal types never require transient scope

      elsif Ekind (Typ) = E_String_Literal_Subtype then
         return False;

      --  Array type. Note that we already know that this is a constrained
      --  array, since unconstrained arrays will fail the indefinite test.

      elsif Is_Array_Type (Typ) then

         --  If component type requires a transient scope, the array does too

         if Requires_Transient_Scope (Component_Type (Typ)) then
            return True;

         --  Otherwise, we only need a transient scope if the size is not
         --  known at compile time.

         else
            return not Size_Known_At_Compile_Time (Typ);
         end if;

      --  All other cases do not require a transient scope

      else
         return False;
      end if;
   end Requires_Transient_Scope;

   --------------------------
   -- Reset_Analyzed_Flags --
   --------------------------

   procedure Reset_Analyzed_Flags (N : Node_Id) is

      function Clear_Analyzed
        (N : Node_Id) return Traverse_Result;
      --  Function used to reset Analyzed flags in tree. Note that we do
      --  not reset Analyzed flags in entities, since there is no need to
      --  renalalyze entities, and indeed, it is wrong to do so, since it
      --  can result in generating auxiliary stuff more than once.

      --------------------
      -- Clear_Analyzed --
      --------------------

      function Clear_Analyzed
        (N : Node_Id) return Traverse_Result
      is
      begin
         if not Has_Extension (N) then
            Set_Analyzed (N, False);
         end if;

         return OK;
      end Clear_Analyzed;

      function Reset_Analyzed is
        new Traverse_Func (Clear_Analyzed);

      Discard : Traverse_Result;
      pragma Warnings (Off, Discard);

   --  Start of processing for Reset_Analyzed_Flags

   begin
      Discard := Reset_Analyzed (N);
   end Reset_Analyzed_Flags;

   ---------------------------
   -- Safe_To_Capture_Value --
   ---------------------------

   function Safe_To_Capture_Value
     (N   : Node_Id;
      Ent : Entity_Id) return Boolean
   is
   begin
      --  The only entities for which we track constant values are variables,
      --  out parameters and in out parameters, so check if we have this case.

      if Ekind (Ent) /= E_Variable
           and then
         Ekind (Ent) /= E_Out_Parameter
           and then
         Ekind (Ent) /= E_In_Out_Parameter
      then
         return False;
      end if;

      --  Skip volatile and aliased variables, since funny things might
      --  be going on in these cases which we cannot necessarily track.
      --  Also skip any variable for which an address clause is given.

      --  Should we have a flag Has_Address_Clause ???

      if Treat_As_Volatile (Ent)
        or else Is_Aliased (Ent)
        or else Present (Address_Clause (Ent))
      then
         return False;
      end if;

      --  OK, all above conditions are met. We also require that the scope
      --  of the reference be the same as the scope of the entity, not
      --  counting packages and blocks.

      declare
         E_Scope : constant Entity_Id := Scope (Ent);
         R_Scope : Entity_Id;

      begin
         R_Scope := Current_Scope;
         while R_Scope /= Standard_Standard loop
            exit when R_Scope = E_Scope;

            if Ekind (R_Scope) /= E_Package
                 and then
               Ekind (R_Scope) /= E_Block
            then
               return False;
            else
               R_Scope := Scope (R_Scope);
            end if;
         end loop;
      end;

      --  We also require that the reference does not appear in a context
      --  where it is not sure to be executed (i.e. a conditional context
      --  or an exception handler).

      declare
         Desc : Node_Id;
         P    : Node_Id;

      begin
         Desc := N;
         P    := Parent (N);
         while Present (P) loop
            if Nkind (P) = N_If_Statement
              or else  Nkind (P) = N_Case_Statement
              or else (Nkind (P) = N_And_Then and then Desc = Right_Opnd (P))
              or else (Nkind (P) = N_Or_Else and then Desc = Right_Opnd (P))
              or else  Nkind (P) = N_Exception_Handler
              or else  Nkind (P) = N_Selective_Accept
              or else  Nkind (P) = N_Conditional_Entry_Call
              or else  Nkind (P) = N_Timed_Entry_Call
              or else  Nkind (P) = N_Asynchronous_Select
            then
               return False;
            else
               Desc := P;
               P    := Parent (P);
            end if;
         end loop;
      end;

      --  OK, looks safe to set value

      return True;
   end Safe_To_Capture_Value;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (N1, N2 : Node_Id) return Boolean is
      K1 : constant Node_Kind := Nkind (N1);
      K2 : constant Node_Kind := Nkind (N2);

   begin
      if (K1 = N_Identifier or else K1 = N_Defining_Identifier)
        and then (K2 = N_Identifier or else K2 = N_Defining_Identifier)
      then
         return Chars (N1) = Chars (N2);

      elsif (K1 = N_Selected_Component or else K1 = N_Expanded_Name)
        and then (K2 = N_Selected_Component or else K2 = N_Expanded_Name)
      then
         return Same_Name (Selector_Name (N1), Selector_Name (N2))
           and then Same_Name (Prefix (N1), Prefix (N2));

      else
         return False;
      end if;
   end Same_Name;

   ---------------
   -- Same_Type --
   ---------------

   function Same_Type (T1, T2 : Entity_Id) return Boolean is
   begin
      if T1 = T2 then
         return True;

      elsif not Is_Constrained (T1)
        and then not Is_Constrained (T2)
        and then Base_Type (T1) = Base_Type (T2)
      then
         return True;

      --  For now don't bother with case of identical constraints, to be
      --  fiddled with later on perhaps (this is only used for optimization
      --  purposes, so it is not critical to do a best possible job)

      else
         return False;
      end if;
   end Same_Type;

   ------------------------
   -- Scope_Is_Transient --
   ------------------------

   function Scope_Is_Transient  return Boolean is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Is_Transient;
   end Scope_Is_Transient;

   ------------------
   -- Scope_Within --
   ------------------

   function Scope_Within (Scope1, Scope2 : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope1;
      while Scop /= Standard_Standard loop
         Scop := Scope (Scop);

         if Scop = Scope2 then
            return True;
         end if;
      end loop;

      return False;
   end Scope_Within;

   --------------------------
   -- Scope_Within_Or_Same --
   --------------------------

   function Scope_Within_Or_Same (Scope1, Scope2 : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope1;
      while Scop /= Standard_Standard loop
         if Scop = Scope2 then
            return True;
         else
            Scop := Scope (Scop);
         end if;
      end loop;

      return False;
   end Scope_Within_Or_Same;

   ------------------------
   -- Set_Current_Entity --
   ------------------------

   --  The given entity is to be set as the currently visible definition
   --  of its associated name (i.e. the Node_Id associated with its name).
   --  All we have to do is to get the name from the identifier, and
   --  then set the associated Node_Id to point to the given entity.

   procedure Set_Current_Entity (E : Entity_Id) is
   begin
      Set_Name_Entity_Id (Chars (E), E);
   end Set_Current_Entity;

   ---------------------------------
   -- Set_Entity_With_Style_Check --
   ---------------------------------

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Entity_Id) is
      Val_Actual : Entity_Id;
      Nod        : Node_Id;

   begin
      Set_Entity (N, Val);

      if Style_Check
        and then not Suppress_Style_Checks (Val)
        and then not In_Instance
      then
         if Nkind (N) = N_Identifier then
            Nod := N;

         elsif Nkind (N) = N_Expanded_Name then
            Nod := Selector_Name (N);

         else
            return;
         end if;

         --  A special situation arises for derived operations, where we want
         --  to do the check against the parent (since the Sloc of the derived
         --  operation points to the derived type declaration itself).

         Val_Actual := Val;
         while not Comes_From_Source (Val_Actual)
           and then Nkind (Val_Actual) in N_Entity
           and then (Ekind (Val_Actual) = E_Enumeration_Literal
                      or else Is_Subprogram (Val_Actual)
                      or else Is_Generic_Subprogram (Val_Actual))
           and then Present (Alias (Val_Actual))
         loop
            Val_Actual := Alias (Val_Actual);
         end loop;

         --  Renaming declarations for generic actuals do not come from source,
         --  and have a different name from that of the entity they rename, so
         --  there is no style check to perform here.

         if Chars (Nod) = Chars (Val_Actual) then
            Style.Check_Identifier (Nod, Val_Actual);
         end if;
      end if;

      Set_Entity (N, Val);
   end Set_Entity_With_Style_Check;

   ------------------------
   -- Set_Name_Entity_Id --
   ------------------------

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id) is
   begin
      Set_Name_Table_Info (Id, Int (Val));
   end Set_Name_Entity_Id;

   ---------------------
   -- Set_Next_Actual --
   ---------------------

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id) is
   begin
      if Nkind (Parent (Ass1_Id)) = N_Parameter_Association then
         Set_First_Named_Actual (Parent (Ass1_Id), Ass2_Id);
      end if;
   end Set_Next_Actual;

   -----------------------
   -- Set_Public_Status --
   -----------------------

   procedure Set_Public_Status (Id : Entity_Id) is
      S : constant Entity_Id := Current_Scope;

   begin
      --  Everything in the scope of Standard is public

      if S = Standard_Standard then
         Set_Is_Public (Id);

      --  Entity is definitely not public if enclosing scope is not public

      elsif not Is_Public (S) then
         return;

      --  An object declaration that occurs in a handled sequence of statements
      --  is the declaration for a temporary object generated by the expander.
      --  It never needs to be made public and furthermore, making it public
      --  can cause back end problems if it is of variable size.

      elsif Nkind (Parent (Id)) = N_Object_Declaration
        and then
          Nkind (Parent (Parent (Id))) = N_Handled_Sequence_Of_Statements
      then
         return;

      --  Entities in public packages or records are public

      elsif Ekind (S) = E_Package or Is_Record_Type (S) then
         Set_Is_Public (Id);

      --  The bounds of an entry family declaration can generate object
      --  declarations that are visible to the back-end, e.g. in the
      --  the declaration of a composite type that contains tasks.

      elsif Is_Concurrent_Type (S)
        and then not Has_Completion (S)
        and then Nkind (Parent (Id)) = N_Object_Declaration
      then
         Set_Is_Public (Id);
      end if;
   end Set_Public_Status;

   ----------------------------
   -- Set_Scope_Is_Transient --
   ----------------------------

   procedure Set_Scope_Is_Transient (V : Boolean := True) is
   begin
      Scope_Stack.Table (Scope_Stack.Last).Is_Transient := V;
   end Set_Scope_Is_Transient;

   -------------------
   -- Set_Size_Info --
   -------------------

   procedure Set_Size_Info (T1, T2 : Entity_Id) is
   begin
      --  We copy Esize, but not RM_Size, since in general RM_Size is
      --  subtype specific and does not get inherited by all subtypes.

      Set_Esize                     (T1, Esize                     (T2));
      Set_Has_Biased_Representation (T1, Has_Biased_Representation (T2));

      if Is_Discrete_Or_Fixed_Point_Type (T1)
           and then
         Is_Discrete_Or_Fixed_Point_Type (T2)
      then
         Set_Is_Unsigned_Type       (T1, Is_Unsigned_Type          (T2));
      end if;
      Set_Alignment                 (T1, Alignment                 (T2));
   end Set_Size_Info;

   --------------------
   -- Static_Integer --
   --------------------

   function Static_Integer (N : Node_Id) return Uint is
   begin
      Analyze_And_Resolve (N, Any_Integer);

      if N = Error
        or else Error_Posted (N)
        or else Etype (N) = Any_Type
      then
         return No_Uint;
      end if;

      if Is_Static_Expression (N) then
         if not Raises_Constraint_Error (N) then
            return Expr_Value (N);
         else
            return No_Uint;
         end if;

      elsif Etype (N) = Any_Type then
         return No_Uint;

      else
         Flag_Non_Static_Expr
           ("static integer expression required here", N);
         return No_Uint;
      end if;
   end Static_Integer;

   --------------------------
   -- Statically_Different --
   --------------------------

   function Statically_Different (E1, E2 : Node_Id) return Boolean is
      R1 : constant Node_Id := Get_Referenced_Object (E1);
      R2 : constant Node_Id := Get_Referenced_Object (E2);
   begin
      return     Is_Entity_Name (R1)
        and then Is_Entity_Name (R2)
        and then Entity (R1) /= Entity (R2)
        and then not Is_Formal (Entity (R1))
        and then not Is_Formal (Entity (R2));
   end Statically_Different;

   -----------------------------
   -- Subprogram_Access_Level --
   -----------------------------

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint is
   begin
      if Present (Alias (Subp)) then
         return Subprogram_Access_Level (Alias (Subp));
      else
         return Scope_Depth (Enclosing_Dynamic_Scope (Subp));
      end if;
   end Subprogram_Access_Level;

   -----------------
   -- Trace_Scope --
   -----------------

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String) is
   begin
      if Debug_Flag_W then
         for J in 0 .. Scope_Stack.Last loop
            Write_Str ("  ");
         end loop;

         Write_Str (Msg);
         Write_Name (Chars (E));
         Write_Str ("   line ");
         Write_Int (Int (Get_Logical_Line_Number (Sloc (N))));
         Write_Eol;
      end if;
   end Trace_Scope;

   -----------------------
   -- Transfer_Entities --
   -----------------------

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id) is
      Ent : Entity_Id := First_Entity (From);

   begin
      if No (Ent) then
         return;
      end if;

      if (Last_Entity (To)) = Empty then
         Set_First_Entity (To, Ent);
      else
         Set_Next_Entity (Last_Entity (To), Ent);
      end if;

      Set_Last_Entity (To, Last_Entity (From));

      while Present (Ent) loop
         Set_Scope (Ent, To);

         if not Is_Public (Ent) then
            Set_Public_Status (Ent);

            if Is_Public (Ent)
              and then Ekind (Ent) = E_Record_Subtype

            then
               --  The components of the propagated Itype must be public
               --  as well.

               declare
                  Comp : Entity_Id;

               begin
                  Comp := First_Entity (Ent);
                  while Present (Comp) loop
                     Set_Is_Public (Comp);
                     Next_Entity (Comp);
                  end loop;
               end;
            end if;
         end if;

         Next_Entity (Ent);
      end loop;

      Set_First_Entity (From, Empty);
      Set_Last_Entity (From, Empty);
   end Transfer_Entities;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level (Typ : Entity_Id) return Uint is
      Btyp : Entity_Id;

   begin
      --  If the type is an anonymous access type we treat it as being
      --  declared at the library level to ensure that names such as
      --  X.all'access don't fail static accessibility checks.

      --  Ada 2005 (AI-230): In case of anonymous access types that are
      --  component_definition or discriminants of a nonlimited type,
      --  the level is the same as that of the enclosing component type.

      Btyp := Base_Type (Typ);

      if Ekind (Btyp) in Access_Kind then
         if Ekind (Btyp) = E_Anonymous_Access_Type
           and then not Is_Local_Anonymous_Access (Typ) -- Ada 2005 (AI-230)
         then
            return Scope_Depth (Standard_Standard);
         end if;

         Btyp := Root_Type (Btyp);

         --  The accessibility level of anonymous acccess types associated with
         --  discriminants is that of the current instance of the type, and
         --  that's deeper than the type itself (AARM 3.10.2 (12.3.21)).

         if Ekind (Typ) = E_Anonymous_Access_Type
           and then Present (Associated_Node_For_Itype (Typ))
           and then Nkind (Associated_Node_For_Itype (Typ)) =
                                                 N_Discriminant_Specification
         then
            return Scope_Depth (Enclosing_Dynamic_Scope (Btyp)) + 1;
         end if;
      end if;

      return Scope_Depth (Enclosing_Dynamic_Scope (Btyp));
   end Type_Access_Level;

   --------------------------
   -- Unit_Declaration_Node --
   --------------------------

   function Unit_Declaration_Node (Unit_Id : Entity_Id) return Node_Id is
      N : Node_Id := Parent (Unit_Id);

   begin
      --  Predefined operators do not have a full function declaration

      if Ekind (Unit_Id) = E_Operator then
         return N;
      end if;

      while Nkind (N) /= N_Abstract_Subprogram_Declaration
        and then Nkind (N) /= N_Formal_Package_Declaration
        and then Nkind (N) /= N_Function_Instantiation
        and then Nkind (N) /= N_Generic_Package_Declaration
        and then Nkind (N) /= N_Generic_Subprogram_Declaration
        and then Nkind (N) /= N_Package_Declaration
        and then Nkind (N) /= N_Package_Body
        and then Nkind (N) /= N_Package_Instantiation
        and then Nkind (N) /= N_Package_Renaming_Declaration
        and then Nkind (N) /= N_Procedure_Instantiation
        and then Nkind (N) /= N_Protected_Body
        and then Nkind (N) /= N_Subprogram_Declaration
        and then Nkind (N) /= N_Subprogram_Body
        and then Nkind (N) /= N_Subprogram_Body_Stub
        and then Nkind (N) /= N_Subprogram_Renaming_Declaration
        and then Nkind (N) /= N_Task_Body
        and then Nkind (N) /= N_Task_Type_Declaration
        and then Nkind (N) not in N_Formal_Subprogram_Declaration
        and then Nkind (N) not in N_Generic_Renaming_Declaration
      loop
         N := Parent (N);
         pragma Assert (Present (N));
      end loop;

      return N;
   end Unit_Declaration_Node;

   ------------------------------
   -- Universal_Interpretation --
   ------------------------------

   function Universal_Interpretation (Opnd : Node_Id) return Entity_Id is
      Index : Interp_Index;
      It    : Interp;

   begin
      --  The argument may be a formal parameter of an operator or subprogram
      --  with multiple interpretations, or else an expression for an actual.

      if Nkind (Opnd) = N_Defining_Identifier
        or else not Is_Overloaded (Opnd)
      then
         if Etype (Opnd) = Universal_Integer
           or else Etype (Opnd) = Universal_Real
         then
            return Etype (Opnd);
         else
            return Empty;
         end if;

      else
         Get_First_Interp (Opnd, Index, It);
         while Present (It.Typ) loop
            if It.Typ = Universal_Integer
              or else It.Typ = Universal_Real
            then
               return It.Typ;
            end if;

            Get_Next_Interp (Index, It);
         end loop;

         return Empty;
      end if;
   end Universal_Interpretation;

   ----------------------
   -- Within_Init_Proc --
   ----------------------

   function Within_Init_Proc return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while not Is_Overloadable (S) loop
         if S = Standard_Standard then
            return False;
         else
            S := Scope (S);
         end if;
      end loop;

      return Is_Init_Proc (S);
   end Within_Init_Proc;

   ----------------
   -- Wrong_Type --
   ----------------

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id) is
      Found_Type : constant Entity_Id := First_Subtype (Etype (Expr));
      Expec_Type : constant Entity_Id := First_Subtype (Expected_Type);

      function Has_One_Matching_Field return Boolean;
      --  Determines if Expec_Type is a record type with a single component or
      --  discriminant whose type matches the found type or is one dimensional
      --  array whose component type matches the found type.

      ----------------------------
      -- Has_One_Matching_Field --
      ----------------------------

      function Has_One_Matching_Field return Boolean is
         E : Entity_Id;

      begin
         if Is_Array_Type (Expec_Type)
           and then Number_Dimensions (Expec_Type) = 1
           and then
             Covers (Etype (Component_Type (Expec_Type)), Found_Type)
         then
            return True;

         elsif not Is_Record_Type (Expec_Type) then
            return False;

         else
            E := First_Entity (Expec_Type);
            loop
               if No (E) then
                  return False;

               elsif (Ekind (E) /= E_Discriminant
                       and then Ekind (E) /= E_Component)
                 or else (Chars (E) = Name_uTag
                           or else Chars (E) = Name_uParent)
               then
                  Next_Entity (E);

               else
                  exit;
               end if;
            end loop;

            if not Covers (Etype (E), Found_Type) then
               return False;

            elsif Present (Next_Entity (E)) then
               return False;

            else
               return True;
            end if;
         end if;
      end Has_One_Matching_Field;

   --  Start of processing for Wrong_Type

   begin
      --  Don't output message if either type is Any_Type, or if a message
      --  has already been posted for this node. We need to do the latter
      --  check explicitly (it is ordinarily done in Errout), because we
      --  are using ! to force the output of the error messages.

      if Expec_Type = Any_Type
        or else Found_Type = Any_Type
        or else Error_Posted (Expr)
      then
         return;

      --  In  an instance, there is an ongoing problem with completion of
      --  type derived from private types. Their structure is what Gigi
      --  expects, but the  Etype is the parent type rather than the
      --  derived private type itself. Do not flag error in this case. The
      --  private completion is an entity without a parent, like an Itype.
      --  Similarly, full and partial views may be incorrect in the instance.
      --  There is no simple way to insure that it is consistent ???

      elsif In_Instance then

         if Etype (Etype (Expr)) = Etype (Expected_Type)
           and then
             (Has_Private_Declaration (Expected_Type)
               or else Has_Private_Declaration (Etype (Expr)))
           and then No (Parent (Expected_Type))
         then
            return;
         end if;
      end if;

      --  An interesting special check. If the expression is parenthesized
      --  and its type corresponds to the type of the sole component of the
      --  expected record type, or to the component type of the expected one
      --  dimensional array type, then assume we have a bad aggregate attempt.

      if Nkind (Expr) in N_Subexpr
        and then Paren_Count (Expr) /= 0
        and then Has_One_Matching_Field
      then
         Error_Msg_N ("positional aggregate cannot have one component", Expr);

      --  Another special check, if we are looking for a pool-specific access
      --  type and we found an E_Access_Attribute_Type, then we have the case
      --  of an Access attribute being used in a context which needs a pool-
      --  specific type, which is never allowed. The one extra check we make
      --  is that the expected designated type covers the Found_Type.

      elsif Is_Access_Type (Expec_Type)
        and then Ekind (Found_Type) = E_Access_Attribute_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_General_Access_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_Anonymous_Access_Type
        and then Covers
          (Designated_Type (Expec_Type), Designated_Type (Found_Type))
      then
         Error_Msg_N ("result must be general access type!", Expr);
         Error_Msg_NE ("add ALL to }!", Expr, Expec_Type);

      --  If the expected type is an anonymous access type, as for access
      --  parameters and discriminants, the error is on the designated types.

      elsif Ekind (Expec_Type) = E_Anonymous_Access_Type then
         if Comes_From_Source (Expec_Type) then
            Error_Msg_NE ("expected}!", Expr, Expec_Type);
         else
            Error_Msg_NE
              ("expected an access type with designated}",
                 Expr, Designated_Type (Expec_Type));
         end if;

         if Is_Access_Type (Found_Type)
           and then not Comes_From_Source (Found_Type)
         then
            Error_Msg_NE
              ("found an access type with designated}!",
                Expr, Designated_Type (Found_Type));
         else
            if From_With_Type (Found_Type) then
               Error_Msg_NE ("found incomplete}!", Expr, Found_Type);
               Error_Msg_NE
                 ("\possibly missing with_clause on&", Expr,
                   Scope (Found_Type));
            else
               Error_Msg_NE ("found}!", Expr, Found_Type);
            end if;
         end if;

      --  Normal case of one type found, some other type expected

      else
         --  If the names of the two types are the same, see if some
         --  number of levels of qualification will help. Don't try
         --  more than three levels, and if we get to standard, it's
         --  no use (and probably represents an error in the compiler)
         --  Also do not bother with internal scope names.

         declare
            Expec_Scope : Entity_Id;
            Found_Scope : Entity_Id;

         begin
            Expec_Scope := Expec_Type;
            Found_Scope := Found_Type;

            for Levels in Int range 0 .. 3 loop
               if Chars (Expec_Scope) /= Chars (Found_Scope) then
                  Error_Msg_Qual_Level := Levels;
                  exit;
               end if;

               Expec_Scope := Scope (Expec_Scope);
               Found_Scope := Scope (Found_Scope);

               exit when Expec_Scope = Standard_Standard
                 or else Found_Scope = Standard_Standard
                 or else not Comes_From_Source (Expec_Scope)
                 or else not Comes_From_Source (Found_Scope);
            end loop;
         end;

         if Is_Record_Type (Expec_Type)
           and then Present (Corresponding_Remote_Type (Expec_Type))
         then
            Error_Msg_NE ("expected}!", Expr,
                          Corresponding_Remote_Type (Expec_Type));
         else
            Error_Msg_NE ("expected}!", Expr, Expec_Type);
         end if;

         if Is_Entity_Name (Expr)
           and then Is_Package_Or_Generic_Package (Entity (Expr))
         then
            Error_Msg_N ("found package name!", Expr);

         elsif Is_Entity_Name (Expr)
           and then
             (Ekind (Entity (Expr)) = E_Procedure
                or else
              Ekind (Entity (Expr)) = E_Generic_Procedure)
         then
            if Ekind (Expec_Type) = E_Access_Subprogram_Type then
               Error_Msg_N
                 ("found procedure name, possibly missing Access attribute!",
                   Expr);
            else
               Error_Msg_N ("found procedure name instead of function!", Expr);
            end if;

         elsif Nkind (Expr) = N_Function_Call
           and then Ekind (Expec_Type) = E_Access_Subprogram_Type
           and then Etype (Designated_Type (Expec_Type)) = Etype (Expr)
           and then No (Parameter_Associations (Expr))
         then
            Error_Msg_N
              ("found function name, possibly missing Access attribute!",
               Expr);

         --  Catch common error: a prefix or infix operator which is not
         --  directly visible because the type isn't.

         elsif Nkind (Expr) in N_Op
            and then Is_Overloaded (Expr)
            and then not Is_Immediately_Visible (Expec_Type)
            and then not Is_Potentially_Use_Visible (Expec_Type)
            and then not In_Use (Expec_Type)
            and then Has_Compatible_Type (Right_Opnd (Expr), Expec_Type)
         then
            Error_Msg_N
              ("operator of the type is not directly visible!", Expr);

         elsif Ekind (Found_Type) = E_Void
           and then Present (Parent (Found_Type))
           and then Nkind (Parent (Found_Type)) = N_Full_Type_Declaration
         then
            Error_Msg_NE ("found premature usage of}!", Expr, Found_Type);

         else
            Error_Msg_NE ("found}!", Expr, Found_Type);
         end if;

         Error_Msg_Qual_Level := 0;
      end if;
   end Wrong_Type;

end Sem_Util;
