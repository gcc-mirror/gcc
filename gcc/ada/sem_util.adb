------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with Treepr; -- ???For debugging code below

with Aspects;  use Aspects;
with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch11; use Exp_Ch11;
with Exp_Disp; use Exp_Disp;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet.Sp; use Namet.Sp;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Attr; use Sem_Attr;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Warn; use Sem_Warn;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stand;    use Stand;
with Style;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uname;    use Uname;

with GNAT.HTable; use GNAT.HTable;

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

   function Has_Enabled_Property
     (Item_Id  : Entity_Id;
      Property : Name_Id) return Boolean;
   --  Subsidiary to routines Async_xxx_Enabled and Effective_xxx_Enabled.
   --  Determine whether an abstract state or a variable denoted by entity
   --  Item_Id has enabled property Property.

   function Has_Null_Extension (T : Entity_Id) return Boolean;
   --  T is a derived tagged type. Check whether the type extension is null.
   --  If the parent type is fully initialized, T can be treated as such.

   function Is_Fully_Initialized_Variant (Typ : Entity_Id) return Boolean;
   --  Subsidiary to Is_Fully_Initialized_Type. For an unconstrained type
   --  with discriminants whose default values are static, examine only the
   --  components in the selected variant to determine whether all of them
   --  have a default.

   type Null_Status_Kind is
     (Is_Null,
      --  This value indicates that a subexpression is known to have a null
      --  value at compile time.

      Is_Non_Null,
      --  This value indicates that a subexpression is known to have a non-null
      --  value at compile time.

      Unknown);
      --  This value indicates that it cannot be determined at compile time
      --  whether a subexpression yields a null or non-null value.

   function Null_Status (N : Node_Id) return Null_Status_Kind;
   --  Determine whether subexpression N of an access type yields a null value,
   --  a non-null value, or the value cannot be determined at compile time. The
   --  routine does not take simple flow diagnostics into account, it relies on
   --  static facts such as the presence of null exclusions.

   function Old_Requires_Transient_Scope (Id : Entity_Id) return Boolean;
   function New_Requires_Transient_Scope (Id : Entity_Id) return Boolean;
   --  ???We retain the old and new algorithms for Requires_Transient_Scope for
   --  the time being. New_Requires_Transient_Scope is used by default; the
   --  debug switch -gnatdQ can be used to do Old_Requires_Transient_Scope
   --  instead. The intent is to use this temporarily to measure before/after
   --  efficiency. Note: when this temporary code is removed, the documentation
   --  of dQ in debug.adb should be removed.

   procedure Results_Differ
     (Id      : Entity_Id;
      Old_Val : Boolean;
      New_Val : Boolean);
   --  ???Debugging code. Called when the Old_Val and New_Val differ. This
   --  routine will be removed eventially when New_Requires_Transient_Scope
   --  becomes Requires_Transient_Scope and Old_Requires_Transient_Scope is
   --  eliminated.

   ------------------------------
   --  Abstract_Interface_List --
   ------------------------------

   function Abstract_Interface_List (Typ : Entity_Id) return List_Id is
      Nod : Node_Id;

   begin
      if Is_Concurrent_Type (Typ) then

         --  If we are dealing with a synchronized subtype, go to the base
         --  type, whose declaration has the interface list.

         --  Shouldn't this be Declaration_Node???

         Nod := Parent (Base_Type (Typ));

         if Nkind (Nod) = N_Full_Type_Declaration then
            return Empty_List;
         end if;

      elsif Ekind (Typ) = E_Record_Type_With_Private then
         if Nkind (Parent (Typ)) = N_Full_Type_Declaration then
            Nod := Type_Definition (Parent (Typ));

         elsif Nkind (Parent (Typ)) = N_Private_Type_Declaration then
            if Present (Full_View (Typ))
              and then
                Nkind (Parent (Full_View (Typ))) = N_Full_Type_Declaration
            then
               Nod := Type_Definition (Parent (Full_View (Typ)));

            --  If the full-view is not available we cannot do anything else
            --  here (the source has errors).

            else
               return Empty_List;
            end if;

         --  Support for generic formals with interfaces is still missing ???

         elsif Nkind (Parent (Typ)) = N_Formal_Type_Declaration then
            return Empty_List;

         else
            pragma Assert
              (Nkind (Parent (Typ)) = N_Private_Extension_Declaration);
            Nod := Parent (Typ);
         end if;

      elsif Ekind (Typ) = E_Record_Subtype then
         Nod := Type_Definition (Parent (Etype (Typ)));

      elsif Ekind (Typ) = E_Record_Subtype_With_Private then

         --  Recurse, because parent may still be a private extension. Also
         --  note that the full view of the subtype or the full view of its
         --  base type may (both) be unavailable.

         return Abstract_Interface_List (Etype (Typ));

      else pragma Assert ((Ekind (Typ)) = E_Record_Type);
         if Nkind (Parent (Typ)) = N_Formal_Type_Declaration then
            Nod := Formal_Type_Definition (Parent (Typ));
         else
            Nod := Type_Definition (Parent (Typ));
         end if;
      end if;

      return Interface_List (Nod);
   end Abstract_Interface_List;

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

   --------------------------
   -- Add_Block_Identifier --
   --------------------------

   procedure Add_Block_Identifier (N : Node_Id; Id : out Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      pragma Assert (Nkind (N) = N_Block_Statement);

      --  The block already has a label, return its entity

      if Present (Identifier (N)) then
         Id := Entity (Identifier (N));

      --  Create a new block label and set its attributes

      else
         Id := New_Internal_Entity (E_Block, Current_Scope, Loc, 'B');
         Set_Etype  (Id, Standard_Void_Type);
         Set_Parent (Id, N);

         Set_Identifier (N, New_Occurrence_Of (Id, Loc));
         Set_Block_Node (Id, Identifier (N));
      end if;
   end Add_Block_Identifier;

   ----------------------------
   -- Add_Global_Declaration --
   ----------------------------

   procedure Add_Global_Declaration (N : Node_Id) is
      Aux_Node : constant Node_Id := Aux_Decls_Node (Cunit (Current_Sem_Unit));

   begin
      if No (Declarations (Aux_Node)) then
         Set_Declarations (Aux_Node, New_List);
      end if;

      Append_To (Declarations (Aux_Node), N);
      Analyze (N);
   end Add_Global_Declaration;

   --------------------------------
   -- Address_Integer_Convert_OK --
   --------------------------------

   function Address_Integer_Convert_OK (T1, T2 : Entity_Id) return Boolean is
   begin
      if Allow_Integer_Address
        and then ((Is_Descendant_Of_Address  (T1)
                    and then Is_Private_Type (T1)
                    and then Is_Integer_Type (T2))
                            or else
                  (Is_Descendant_Of_Address  (T2)
                    and then Is_Private_Type (T2)
                    and then Is_Integer_Type (T1)))
      then
         return True;
      else
         return False;
      end if;
   end Address_Integer_Convert_OK;

   -------------------
   -- Address_Value --
   -------------------

   function Address_Value (N : Node_Id) return Node_Id is
      Expr : Node_Id := N;

   begin
      loop
         --  For constant, get constant expression

         if Is_Entity_Name (Expr)
           and then Ekind (Entity (Expr)) = E_Constant
         then
            Expr := Constant_Value (Entity (Expr));

         --  For unchecked conversion, get result to convert

         elsif Nkind (Expr) = N_Unchecked_Type_Conversion then
            Expr := Expression (Expr);

         --  For (common case) of To_Address call, get argument

         elsif Nkind (Expr) = N_Function_Call
           and then Is_Entity_Name (Name (Expr))
           and then Is_RTE (Entity (Name (Expr)), RE_To_Address)
         then
            Expr := First (Parameter_Associations (Expr));

            if Nkind (Expr) = N_Parameter_Association then
               Expr := Explicit_Actual_Parameter (Expr);
            end if;

         --  We finally have the real expression

         else
            exit;
         end if;
      end loop;

      return Expr;
   end Address_Value;

   -----------------
   -- Addressable --
   -----------------

   --  For now, just 8/16/32/64

   function Addressable (V : Uint) return Boolean is
   begin
      return V = Uint_8  or else
             V = Uint_16 or else
             V = Uint_32 or else
             V = Uint_64;
   end Addressable;

   function Addressable (V : Int) return Boolean is
   begin
      return V = 8  or else
             V = 16 or else
             V = 32 or else
             V = 64;
   end Addressable;

   ---------------------------------
   -- Aggregate_Constraint_Checks --
   ---------------------------------

   procedure Aggregate_Constraint_Checks
     (Exp       : Node_Id;
      Check_Typ : Entity_Id)
   is
      Exp_Typ : constant Entity_Id  := Etype (Exp);

   begin
      if Raises_Constraint_Error (Exp) then
         return;
      end if;

      --  Ada 2005 (AI-230): Generate a conversion to an anonymous access
      --  component's type to force the appropriate accessibility checks.

      --  Ada 2005 (AI-231): Generate conversion to the null-excluding type to
      --  force the corresponding run-time check

      if Is_Access_Type (Check_Typ)
        and then Is_Local_Anonymous_Access (Check_Typ)
      then
         Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp, Check_Typ);
         Check_Unset_Reference (Exp);
      end if;

      --  What follows is really expansion activity, so check that expansion
      --  is on and is allowed. In GNATprove mode, we also want check flags to
      --  be added in the tree, so that the formal verification can rely on
      --  those to be present. In GNATprove mode for formal verification, some
      --  treatment typically only done during expansion needs to be performed
      --  on the tree, but it should not be applied inside generics. Otherwise,
      --  this breaks the name resolution mechanism for generic instances.

      if not Expander_Active
        and (Inside_A_Generic or not Full_Analysis or not GNATprove_Mode)
      then
         return;
      end if;

      if Is_Access_Type (Check_Typ)
        and then Can_Never_Be_Null (Check_Typ)
        and then not Can_Never_Be_Null (Exp_Typ)
      then
         Install_Null_Excluding_Check (Exp);
      end if;

      --  First check if we have to insert discriminant checks

      if Has_Discriminants (Exp_Typ) then
         Apply_Discriminant_Check (Exp, Check_Typ);

      --  Next emit length checks for array aggregates

      elsif Is_Array_Type (Exp_Typ) then
         Apply_Length_Check (Exp, Check_Typ);

      --  Finally emit scalar and string checks. If we are dealing with a
      --  scalar literal we need to check by hand because the Etype of
      --  literals is not necessarily correct.

      elsif Is_Scalar_Type (Exp_Typ)
        and then Compile_Time_Known_Value (Exp)
      then
         if Is_Out_Of_Range (Exp, Base_Type (Check_Typ)) then
            Apply_Compile_Time_Constraint_Error
              (Exp, "value not in range of}??", CE_Range_Check_Failed,
               Ent => Base_Type (Check_Typ),
               Typ => Base_Type (Check_Typ));

         elsif Is_Out_Of_Range (Exp, Check_Typ) then
            Apply_Compile_Time_Constraint_Error
              (Exp, "value not in range of}??", CE_Range_Check_Failed,
               Ent => Check_Typ,
               Typ => Check_Typ);

         elsif not Range_Checks_Suppressed (Check_Typ) then
            Apply_Scalar_Range_Check (Exp, Check_Typ);
         end if;

      --  Verify that target type is also scalar, to prevent view anomalies
      --  in instantiations.

      elsif (Is_Scalar_Type (Exp_Typ)
              or else Nkind (Exp) = N_String_Literal)
        and then Is_Scalar_Type (Check_Typ)
        and then Exp_Typ /= Check_Typ
      then
         if Is_Entity_Name (Exp)
           and then Ekind (Entity (Exp)) = E_Constant
         then
            --  If expression is a constant, it is worthwhile checking whether
            --  it is a bound of the type.

            if (Is_Entity_Name (Type_Low_Bound (Check_Typ))
                 and then Entity (Exp) = Entity (Type_Low_Bound (Check_Typ)))
              or else
               (Is_Entity_Name (Type_High_Bound (Check_Typ))
                 and then Entity (Exp) = Entity (Type_High_Bound (Check_Typ)))
            then
               return;

            else
               Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
               Analyze_And_Resolve (Exp, Check_Typ);
               Check_Unset_Reference (Exp);
            end if;

         --  Could use a comment on this case ???

         else
            Rewrite (Exp, Convert_To (Check_Typ, Relocate_Node (Exp)));
            Analyze_And_Resolve (Exp, Check_Typ);
            Check_Unset_Reference (Exp);
         end if;

      end if;
   end Aggregate_Constraint_Checks;

   -----------------------
   -- Alignment_In_Bits --
   -----------------------

   function Alignment_In_Bits (E : Entity_Id) return Uint is
   begin
      return Alignment (E) * System_Storage_Unit;
   end Alignment_In_Bits;

   --------------------------------------
   -- All_Composite_Constraints_Static --
   --------------------------------------

   function All_Composite_Constraints_Static
     (Constr : Node_Id) return Boolean
   is
   begin
      if No (Constr) or else Error_Posted (Constr) then
         return True;
      end if;

      case Nkind (Constr) is
         when N_Subexpr =>
            if Nkind (Constr) in N_Has_Entity
              and then Present (Entity (Constr))
            then
               if Is_Type (Entity (Constr)) then
                  return
                    not Is_Discrete_Type (Entity (Constr))
                      or else Is_OK_Static_Subtype (Entity (Constr));
               end if;

            elsif Nkind (Constr) = N_Range then
               return
                 Is_OK_Static_Expression (Low_Bound (Constr))
                   and then
                 Is_OK_Static_Expression (High_Bound (Constr));

            elsif Nkind (Constr) = N_Attribute_Reference
              and then Attribute_Name (Constr) = Name_Range
            then
               return
                 Is_OK_Static_Expression
                   (Type_Low_Bound (Etype (Prefix (Constr))))
                     and then
                 Is_OK_Static_Expression
                   (Type_High_Bound (Etype (Prefix (Constr))));
            end if;

            return
              not Present (Etype (Constr)) -- previous error
                or else not Is_Discrete_Type (Etype (Constr))
                or else Is_OK_Static_Expression (Constr);

         when N_Discriminant_Association =>
            return All_Composite_Constraints_Static (Expression (Constr));

         when N_Range_Constraint =>
            return
              All_Composite_Constraints_Static (Range_Expression (Constr));

         when N_Index_Or_Discriminant_Constraint =>
            declare
               One_Cstr : Entity_Id;
            begin
               One_Cstr := First (Constraints (Constr));
               while Present (One_Cstr) loop
                  if not All_Composite_Constraints_Static (One_Cstr) then
                     return False;
                  end if;

                  Next (One_Cstr);
               end loop;
            end;

            return True;

         when N_Subtype_Indication =>
            return
              All_Composite_Constraints_Static (Subtype_Mark (Constr))
                and then
              All_Composite_Constraints_Static (Constraint (Constr));

         when others =>
            raise Program_Error;
      end case;
   end All_Composite_Constraints_Static;

   ---------------------------------
   -- Append_Inherited_Subprogram --
   ---------------------------------

   procedure Append_Inherited_Subprogram (S : Entity_Id) is
      Par : constant Entity_Id := Alias (S);
      --  The parent subprogram

      Scop : constant Entity_Id := Scope (Par);
      --  The scope of definition of the parent subprogram

      Typ : constant Entity_Id := Defining_Entity (Parent (S));
      --  The derived type of which S is a primitive operation

      Decl   : Node_Id;
      Next_E : Entity_Id;

   begin
      if Ekind (Current_Scope) = E_Package
        and then In_Private_Part (Current_Scope)
        and then Has_Private_Declaration (Typ)
        and then Is_Tagged_Type (Typ)
        and then Scop = Current_Scope
      then
         --  The inherited operation is available at the earliest place after
         --  the derived type declaration ( RM 7.3.1 (6/1)). This is only
         --  relevant for type extensions. If the parent operation appears
         --  after the type extension, the operation is not visible.

         Decl := First
                   (Visible_Declarations
                     (Package_Specification (Current_Scope)));
         while Present (Decl) loop
            if Nkind (Decl) = N_Private_Extension_Declaration
              and then Defining_Entity (Decl) = Typ
            then
               if Sloc (Decl) > Sloc (Par) then
                  Next_E := Next_Entity (Par);
                  Set_Next_Entity (Par, S);
                  Set_Next_Entity (S, Next_E);
                  return;

               else
                  exit;
               end if;
            end if;

            Next (Decl);
         end loop;
      end if;

      --  If partial view is not a type extension, or it appears before the
      --  subprogram declaration, insert normally at end of entity list.

      Append_Entity (S, Current_Scope);
   end Append_Inherited_Subprogram;

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
      Stat   : constant Boolean := Is_Static_Expression (N);
      R_Stat : constant Node_Id :=
                 Make_Raise_Constraint_Error (Sloc (N), Reason => Reason);
      Rtyp   : Entity_Id;

   begin
      if No (Typ) then
         Rtyp := Etype (N);
      else
         Rtyp := Typ;
      end if;

      Discard_Node
        (Compile_Time_Constraint_Error (N, Msg, Ent, Loc, Warn => Warn));

      --  In GNATprove mode, do not replace the node with an exception raised.
      --  In such a case, either the call to Compile_Time_Constraint_Error
      --  issues an error which stops analysis, or it issues a warning in
      --  a few cases where a suitable check flag is set for GNATprove to
      --  generate a check message.

      if not Rep or GNATprove_Mode then
         return;
      end if;

      --  Now we replace the node by an N_Raise_Constraint_Error node
      --  This does not need reanalyzing, so set it as analyzed now.

      Rewrite (N, R_Stat);
      Set_Analyzed (N, True);

      Set_Etype (N, Rtyp);
      Set_Raises_Constraint_Error (N);

      --  Now deal with possible local raise handling

      Possible_Local_Raise (N, Standard_Constraint_Error);

      --  If the original expression was marked as static, the result is
      --  still marked as static, but the Raises_Constraint_Error flag is
      --  always set so that further static evaluation is not attempted.

      if Stat then
         Set_Is_Static_Expression (N);
      end if;
   end Apply_Compile_Time_Constraint_Error;

   ---------------------------
   -- Async_Readers_Enabled --
   ---------------------------

   function Async_Readers_Enabled (Id : Entity_Id) return Boolean is
   begin
      return Has_Enabled_Property (Id, Name_Async_Readers);
   end Async_Readers_Enabled;

   ---------------------------
   -- Async_Writers_Enabled --
   ---------------------------

   function Async_Writers_Enabled (Id : Entity_Id) return Boolean is
   begin
      return Has_Enabled_Property (Id, Name_Async_Writers);
   end Async_Writers_Enabled;

   --------------------------------------
   -- Available_Full_View_Of_Component --
   --------------------------------------

   function Available_Full_View_Of_Component (T : Entity_Id) return Boolean is
      ST  : constant Entity_Id := Scope (T);
      SCT : constant Entity_Id := Scope (Component_Type (T));
   begin
      return In_Open_Scopes (ST)
        and then In_Open_Scopes (SCT)
        and then Scope_Depth (ST) >= Scope_Depth (SCT);
   end Available_Full_View_Of_Component;

   -------------------
   -- Bad_Attribute --
   -------------------

   procedure Bad_Attribute
     (N    : Node_Id;
      Nam  : Name_Id;
      Warn : Boolean := False)
   is
   begin
      Error_Msg_Warn := Warn;
      Error_Msg_N ("unrecognized attribute&<<", N);

      --  Check for possible misspelling

      Error_Msg_Name_1 := First_Attribute_Name;
      while Error_Msg_Name_1 <= Last_Attribute_Name loop
         if Is_Bad_Spelling_Of (Nam, Error_Msg_Name_1) then
            Error_Msg_N -- CODEFIX
              ("\possible misspelling of %<<", N);
            exit;
         end if;

         Error_Msg_Name_1 := Error_Msg_Name_1 + 1;
      end loop;
   end Bad_Attribute;

   --------------------------------
   -- Bad_Predicated_Subtype_Use --
   --------------------------------

   procedure Bad_Predicated_Subtype_Use
     (Msg            : String;
      N              : Node_Id;
      Typ            : Entity_Id;
      Suggest_Static : Boolean := False)
   is
      Gen            : Entity_Id;

   begin
      --  Avoid cascaded errors

      if Error_Posted (N) then
         return;
      end if;

      if Inside_A_Generic then
         Gen := Current_Scope;
         while Present (Gen) and then  Ekind (Gen) /= E_Generic_Package loop
            Gen := Scope (Gen);
         end loop;

         if No (Gen) then
            return;
         end if;

         if Is_Generic_Formal (Typ) and then Is_Discrete_Type (Typ) then
            Set_No_Predicate_On_Actual (Typ);
         end if;

      elsif Has_Predicates (Typ) then
         if Is_Generic_Actual_Type (Typ) then

            --  The restriction on loop parameters is only that the type
            --  should have no dynamic predicates.

            if Nkind (Parent (N)) = N_Loop_Parameter_Specification
              and then not Has_Dynamic_Predicate_Aspect (Typ)
              and then Is_OK_Static_Subtype (Typ)
            then
               return;
            end if;

            Gen := Current_Scope;
            while not Is_Generic_Instance (Gen) loop
               Gen := Scope (Gen);
            end loop;

            pragma Assert (Present (Gen));

            if Ekind (Gen) = E_Package and then In_Package_Body (Gen) then
               Error_Msg_Warn := SPARK_Mode /= On;
               Error_Msg_FE (Msg & "<<", N, Typ);
               Error_Msg_F ("\Program_Error [<<", N);

               Insert_Action (N,
                 Make_Raise_Program_Error (Sloc (N),
                   Reason => PE_Bad_Predicated_Generic_Type));

            else
               Error_Msg_FE (Msg & "<<", N, Typ);
            end if;

         else
            Error_Msg_FE (Msg, N, Typ);
         end if;

         --  Emit an optional suggestion on how to remedy the error if the
         --  context warrants it.

         if Suggest_Static and then Has_Static_Predicate (Typ) then
            Error_Msg_FE ("\predicate of & should be marked static", N, Typ);
         end if;
      end if;
   end Bad_Predicated_Subtype_Use;

   -----------------------------------------
   -- Bad_Unordered_Enumeration_Reference --
   -----------------------------------------

   function Bad_Unordered_Enumeration_Reference
     (N : Node_Id;
      T : Entity_Id) return Boolean
   is
   begin
      return Is_Enumeration_Type (T)
        and then Warn_On_Unordered_Enumeration_Type
        and then not Is_Generic_Type (T)
        and then Comes_From_Source (N)
        and then not Has_Pragma_Ordered (T)
        and then not In_Same_Extended_Unit (N, T);
   end Bad_Unordered_Enumeration_Reference;

   --------------------------
   -- Build_Actual_Subtype --
   --------------------------

   function Build_Actual_Subtype
     (T : Entity_Id;
      N : Node_Or_Entity_Id) return Node_Id
   is
      Loc : Source_Ptr;
      --  Normally Sloc (N), but may point to corresponding body in some cases

      Constraints : List_Id;
      Decl        : Node_Id;
      Discr       : Entity_Id;
      Hi          : Node_Id;
      Lo          : Node_Id;
      Subt        : Entity_Id;
      Disc_Type   : Entity_Id;
      Obj         : Node_Id;

   begin
      Loc := Sloc (N);

      if Nkind (N) = N_Defining_Identifier then
         Obj := New_Occurrence_Of (N, Loc);

         --  If this is a formal parameter of a subprogram declaration, and
         --  we are compiling the body, we want the declaration for the
         --  actual subtype to carry the source position of the body, to
         --  prevent anomalies in gdb when stepping through the code.

         if Is_Formal (N) then
            declare
               Decl : constant Node_Id := Unit_Declaration_Node (Scope (N));
            begin
               if Nkind (Decl) = N_Subprogram_Declaration
                 and then Present (Corresponding_Body (Decl))
               then
                  Loc := Sloc (Corresponding_Body (Decl));
               end if;
            end;
         end if;

      else
         Obj := N;
      end if;

      if Is_Array_Type (T) then
         Constraints := New_List;
         for J in 1 .. Number_Dimensions (T) loop

            --  Build an array subtype declaration with the nominal subtype and
            --  the bounds of the actual. Add the declaration in front of the
            --  local declarations for the subprogram, for analysis before any
            --  reference to the formal in the body.

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

         --  Type T is a generic derived type, inherit the discriminants from
         --  the parent type.

         if Is_Private_Type (T)
           and then No (Full_View (T))

            --  T was flagged as an error if it was declared as a formal
            --  derived type with known discriminants. In this case there
            --  is no need to look at the parent type since T already carries
            --  its own discriminants.

           and then not Error_Posted (T)
         then
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

      Subt := Make_Temporary (Loc, 'S', Related_Node => N);
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Occurrence_Of (T,  Loc),
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
      Index_Typ : Entity_Id;

      Desig_Typ : Entity_Id;
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
         Indx := First_Index (Desig_Typ);
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
         D := First_Elmt (Discriminant_Constraint (Desig_Typ));
         while Present (D) loop
            if Denotes_Discriminant (Node (D)) then
               D_Val := Make_Selected_Component (Loc,
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
      --  Why the test for Spec_Expression mode here???

      if In_Spec_Expression then
         return Empty;

      --  More comments for the rest of this body would be good ???

      elsif Nkind (N) = N_Explicit_Dereference then
         if Is_Composite_Type (T)
           and then not Is_Constrained (T)
           and then not (Is_Class_Wide_Type (T)
                          and then Is_Constrained (Root_Type (T)))
           and then not Has_Unknown_Discriminants (T)
         then
            --  If the type of the dereference is already constrained, it is an
            --  actual subtype.

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
         Desig_Typ := Designated_Type (T);
      else
         Desig_Typ := T;
      end if;

      if Ekind (Desig_Typ) = E_Array_Subtype then
         Id := First_Index (Desig_Typ);
         while Present (Id) loop
            Index_Typ := Underlying_Type (Etype (Id));

            if Denotes_Discriminant (Type_Low_Bound  (Index_Typ))
                 or else
               Denotes_Discriminant (Type_High_Bound (Index_Typ))
            then
               Remove_Side_Effects (P);
               return
                 Build_Component_Subtype
                   (Build_Actual_Array_Constraint, Loc, Base_Type (T));
            end if;

            Next_Index (Id);
         end loop;

      elsif Is_Composite_Type (Desig_Typ)
        and then Has_Discriminants (Desig_Typ)
        and then not Has_Unknown_Discriminants (Desig_Typ)
      then
         if Is_Private_Type (Desig_Typ)
           and then No (Discriminant_Constraint (Desig_Typ))
         then
            Desig_Typ := Full_View (Desig_Typ);
         end if;

         D := First_Elmt (Discriminant_Constraint (Desig_Typ));
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

   ---------------------------------
   -- Build_Class_Wide_Clone_Body --
   ---------------------------------

   procedure Build_Class_Wide_Clone_Body
     (Spec_Id : Entity_Id;
      Bod     : Node_Id)
   is
      Loc        : constant Source_Ptr := Sloc (Bod);
      Clone_Id   : constant Entity_Id  := Class_Wide_Clone (Spec_Id);
      Clone_Body : Node_Id;

   begin
      --  The declaration of the class-wide clone was created when the
      --  corresponding class-wide condition was analyzed.

      Clone_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Subprogram_Spec (Parent (Clone_Id)),
          Declarations               => Declarations (Bod),
          Handled_Statement_Sequence => Handled_Statement_Sequence (Bod));

      --  The new operation is internal and overriding indicators do not apply
      --  (the original primitive may have carried one).

      Set_Must_Override (Specification (Clone_Body), False);
      Insert_Before (Bod, Clone_Body);
      Analyze (Clone_Body);
   end Build_Class_Wide_Clone_Body;

   ---------------------------------
   -- Build_Class_Wide_Clone_Call --
   ---------------------------------

   function Build_Class_Wide_Clone_Call
     (Loc     : Source_Ptr;
      Decls   : List_Id;
      Spec_Id : Entity_Id;
      Spec    : Node_Id) return Node_Id
   is
      Clone_Id : constant Entity_Id := Class_Wide_Clone (Spec_Id);
      Par_Type : constant Entity_Id := Find_Dispatching_Type (Spec_Id);

      Actuals    : List_Id;
      Call       : Node_Id;
      Formal     : Entity_Id;
      New_Body   : Node_Id;
      New_F_Spec : Entity_Id;
      New_Formal : Entity_Id;

   begin
      Actuals    := Empty_List;
      Formal     := First_Formal (Spec_Id);
      New_F_Spec := First (Parameter_Specifications (Spec));

      --  Build parameter association for call to class-wide clone.

      while Present (Formal) loop
         New_Formal := Defining_Identifier (New_F_Spec);

         --  If controlling argument and operation is inherited, add conversion
         --  to parent type for the call.

         if Etype (Formal) = Par_Type
           and then not Is_Empty_List (Decls)
         then
            Append_To (Actuals,
              Make_Type_Conversion (Loc,
                New_Occurrence_Of (Par_Type, Loc),
                New_Occurrence_Of (New_Formal, Loc)));

         else
            Append_To (Actuals, New_Occurrence_Of (New_Formal, Loc));
         end if;

         Next_Formal (Formal);
         Next (New_F_Spec);
      end loop;

      if Ekind (Spec_Id) = E_Procedure then
         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name                   => New_Occurrence_Of (Clone_Id, Loc),
             Parameter_Associations => Actuals);
      else
         Call :=
           Make_Simple_Return_Statement (Loc,
            Expression =>
              Make_Function_Call (Loc,
                Name                   => New_Occurrence_Of (Clone_Id, Loc),
                Parameter_Associations => Actuals));
      end if;

      New_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Subprogram_Spec (Spec),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (Call),
              End_Label  => Make_Identifier (Loc, Chars (Spec_Id))));

      return New_Body;
   end Build_Class_Wide_Clone_Call;

   ---------------------------------
   -- Build_Class_Wide_Clone_Decl --
   ---------------------------------

   procedure Build_Class_Wide_Clone_Decl (Spec_Id : Entity_Id) is
      Loc      : constant Source_Ptr := Sloc (Spec_Id);
      Clone_Id : constant Entity_Id  :=
                   Make_Defining_Identifier (Loc,
                     New_External_Name (Chars (Spec_Id), Suffix => "CL"));

      Decl : Node_Id;
      Spec : Node_Id;

   begin
      Spec := Copy_Subprogram_Spec (Parent (Spec_Id));
      Set_Must_Override      (Spec, False);
      Set_Must_Not_Override  (Spec, False);
      Set_Defining_Unit_Name (Spec, Clone_Id);

      Decl := Make_Subprogram_Declaration (Loc, Spec);
      Append (Decl, List_Containing (Unit_Declaration_Node (Spec_Id)));

      --  Link clone to original subprogram, for use when building body and
      --  wrapper call to inherited operation.

      Set_Class_Wide_Clone (Spec_Id, Clone_Id);
   end Build_Class_Wide_Clone_Decl;

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

      Subt := Make_Temporary (Loc, 'S');
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Occurrence_Of (Base_Type (T),  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => C)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Component_Subtype;

   ---------------------------
   -- Build_Default_Subtype --
   ---------------------------

   function Build_Default_Subtype
     (T : Entity_Id;
      N : Node_Id) return Entity_Id
   is
      Loc  : constant Source_Ptr := Sloc (N);
      Disc : Entity_Id;

      Bas : Entity_Id;
      --  The base type that is to be constrained by the defaults

   begin
      if not Has_Discriminants (T) or else Is_Constrained (T) then
         return T;
      end if;

      Bas := Base_Type (T);

      --  If T is non-private but its base type is private, this is the
      --  completion of a subtype declaration whose parent type is private
      --  (see Complete_Private_Subtype in Sem_Ch3). The proper discriminants
      --  are to be found in the full view of the base. Check that the private
      --  status of T and its base differ.

      if Is_Private_Type (Bas)
        and then not Is_Private_Type (T)
        and then Present (Full_View (Bas))
      then
         Bas := Full_View (Bas);
      end if;

      Disc := First_Discriminant (T);

      if No (Discriminant_Default_Value (Disc)) then
         return T;
      end if;

      declare
         Act         : constant Entity_Id := Make_Temporary (Loc, 'S');
         Constraints : constant List_Id := New_List;
         Decl        : Node_Id;

      begin
         while Present (Disc) loop
            Append_To (Constraints,
              New_Copy_Tree (Discriminant_Default_Value (Disc)));
            Next_Discriminant (Disc);
         end loop;

         Decl :=
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Act,
             Subtype_Indication  =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark => New_Occurrence_Of (Bas, Loc),
                 Constraint   =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => Constraints)));

         Insert_Action (N, Decl);

         --  If the context is a component declaration the subtype declaration
         --  will be analyzed when the enclosing type is frozen, otherwise do
         --  it now.

         if Ekind (Current_Scope) /= E_Record_Type then
            Analyze (Decl);
         end if;

         return Act;
      end;
   end Build_Default_Subtype;

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
      --  Similar to previous one, for discriminated components constrained by
      --  the discriminant of the enclosing object.

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
            if Denotes_Discriminant (Type_Low_Bound  (Etype (Id)))
                 or else
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
      Loc      : constant Source_Ptr := Sloc (N);
      Decl     : Node_Id;
      Elab_Ent : Entity_Id;

      procedure Set_Package_Name (Ent : Entity_Id);
      --  Given an entity, sets the fully qualified name of the entity in
      --  Name_Buffer, with components separated by double underscores. This
      --  is a recursive routine that climbs the scope chain to Standard.

      ----------------------
      -- Set_Package_Name --
      ----------------------

      procedure Set_Package_Name (Ent : Entity_Id) is
      begin
         if Scope (Ent) /= Standard_Standard then
            Set_Package_Name (Scope (Ent));

            declare
               Nam : constant String := Get_Name_String (Chars (Ent));
            begin
               Name_Buffer (Name_Len + 1) := '_';
               Name_Buffer (Name_Len + 2) := '_';
               Name_Buffer (Name_Len + 3 .. Name_Len + Nam'Length + 2) := Nam;
               Name_Len := Name_Len + Nam'Length + 2;
            end;

         else
            Get_Name_String (Chars (Ent));
         end if;
      end Set_Package_Name;

   --  Start of processing for Build_Elaboration_Entity

   begin
      --  Ignore call if already constructed

      if Present (Elaboration_Entity (Spec_Id)) then
         return;

      --  Ignore in ASIS mode, elaboration entity is not in source and plays
      --  no role in analysis.

      elsif ASIS_Mode then
         return;

      --  Do not generate an elaboration entity in GNATprove move because the
      --  elaboration counter is a form of expansion.

      elsif GNATprove_Mode then
         return;

      --  See if we need elaboration entity

      --  We always need an elaboration entity when preserving control flow, as
      --  we want to remain explicit about the unit's elaboration order.

      elsif Opt.Suppress_Control_Flow_Optimizations then
         null;

      --  We always need an elaboration entity for the dynamic elaboration
      --  model, since it is needed to properly generate the PE exception for
      --  access before elaboration.

      elsif Dynamic_Elaboration_Checks then
         null;

      --  For the static model, we don't need the elaboration counter if this
      --  unit is sure to have no elaboration code, since that means there
      --  is no elaboration unit to be called. Note that we can't just decide
      --  after the fact by looking to see whether there was elaboration code,
      --  because that's too late to make this decision.

      elsif Restriction_Active (No_Elaboration_Code) then
         return;

      --  Similarly, for the static model, we can skip the elaboration counter
      --  if we have the No_Multiple_Elaboration restriction, since for the
      --  static model, that's the only purpose of the counter (to avoid
      --  multiple elaboration).

      elsif Restriction_Active (No_Multiple_Elaboration) then
         return;
      end if;

      --  Here we need the elaboration entity

      --  Construct name of elaboration entity as xxx_E, where xxx is the unit
      --  name with dots replaced by double underscore. We have to manually
      --  construct this name, since it will be elaborated in the outer scope,
      --  and thus will not have the unit name automatically prepended.

      Set_Package_Name (Spec_Id);
      Add_Str_To_Name_Buffer ("_E");

      --  Create elaboration counter

      Elab_Ent := Make_Defining_Identifier (Loc, Chars => Name_Find);
      Set_Elaboration_Entity (Spec_Id, Elab_Ent);

      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Elab_Ent,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Short_Integer, Loc),
          Expression          => Make_Integer_Literal (Loc, Uint_0));

      Push_Scope (Standard_Standard);
      Add_Global_Declaration (Decl);
      Pop_Scope;

      --  Reset True_Constant indication, since we will indeed assign a value
      --  to the variable in the binder main. We also kill the Current_Value
      --  and Last_Assignment fields for the same reason.

      Set_Is_True_Constant (Elab_Ent, False);
      Set_Current_Value    (Elab_Ent, Empty);
      Set_Last_Assignment  (Elab_Ent, Empty);

      --  We do not want any further qualification of the name (if we did not
      --  do this, we would pick up the name of the generic package in the case
      --  of a library level generic instantiation).

      Set_Has_Qualified_Name       (Elab_Ent);
      Set_Has_Fully_Qualified_Name (Elab_Ent);
   end Build_Elaboration_Entity;

   --------------------------------
   -- Build_Explicit_Dereference --
   --------------------------------

   procedure Build_Explicit_Dereference
     (Expr : Node_Id;
      Disc : Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Expr);
      I   : Interp_Index;
      It  : Interp;

   begin
      --  An entity of a type with a reference aspect is overloaded with
      --  both interpretations: with and without the dereference. Now that
      --  the dereference is made explicit, set the type of the node properly,
      --  to prevent anomalies in the backend. Same if the expression is an
      --  overloaded function call whose return type has a reference aspect.

      if Is_Entity_Name (Expr) then
         Set_Etype (Expr, Etype (Entity (Expr)));

         --  The designated entity will not be examined again when resolving
         --  the dereference, so generate a reference to it now.

         Generate_Reference (Entity (Expr), Expr);

      elsif Nkind (Expr) = N_Function_Call then

         --  If the name of the indexing function is overloaded, locate the one
         --  whose return type has an implicit dereference on the desired
         --  discriminant, and set entity and type of function call.

         if Is_Overloaded (Name (Expr)) then
            Get_First_Interp (Name (Expr), I, It);

            while Present (It.Nam) loop
               if Ekind ((It.Typ)) = E_Record_Type
                 and then First_Entity ((It.Typ)) = Disc
               then
                  Set_Entity (Name (Expr), It.Nam);
                  Set_Etype (Name (Expr), Etype (It.Nam));
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;

         --  Set type of call from resolved function name.

         Set_Etype (Expr, Etype (Name (Expr)));
      end if;

      Set_Is_Overloaded (Expr, False);

      --  The expression will often be a generalized indexing that yields a
      --  container element that is then dereferenced, in which case the
      --  generalized indexing call is also non-overloaded.

      if Nkind (Expr) = N_Indexed_Component
        and then Present (Generalized_Indexing (Expr))
      then
         Set_Is_Overloaded (Generalized_Indexing (Expr), False);
      end if;

      Rewrite (Expr,
        Make_Explicit_Dereference (Loc,
          Prefix =>
            Make_Selected_Component (Loc,
              Prefix        => Relocate_Node (Expr),
              Selector_Name => New_Occurrence_Of (Disc, Loc))));
      Set_Etype (Prefix (Expr), Etype (Disc));
      Set_Etype (Expr, Designated_Type (Etype (Disc)));
   end Build_Explicit_Dereference;

   ---------------------------
   -- Build_Overriding_Spec --
   ---------------------------

   function Build_Overriding_Spec
     (Op  : Entity_Id;
      Typ : Entity_Id) return Node_Id
   is
      Loc     : constant Source_Ptr := Sloc (Typ);
      Par_Typ : constant Entity_Id := Find_Dispatching_Type (Op);
      Spec    : constant Node_Id := Specification (Unit_Declaration_Node (Op));

      Formal_Spec : Node_Id;
      Formal_Type : Node_Id;
      New_Spec    : Node_Id;

   begin
      New_Spec := Copy_Subprogram_Spec (Spec);

      Formal_Spec := First (Parameter_Specifications (New_Spec));
      while Present (Formal_Spec) loop
         Formal_Type := Parameter_Type (Formal_Spec);

         if Is_Entity_Name (Formal_Type)
           and then Entity (Formal_Type) = Par_Typ
         then
            Rewrite (Formal_Type, New_Occurrence_Of (Typ, Loc));
         end if;

         --  Nothing needs to be done for access parameters

         Next (Formal_Spec);
      end loop;

      return New_Spec;
   end Build_Overriding_Spec;

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
                     N : Node_Id;

                  begin
                     N := First (Expressions (Expr));
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
                  return Cannot_Raise_Constraint_Error (Expression (Expr));
               end if;

            when N_Unchecked_Type_Conversion =>
               return Cannot_Raise_Constraint_Error (Expression (Expr));

            when N_Unary_Op =>
               if Do_Overflow_Check (Expr) then
                  return False;
               else
                  return Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when N_Op_Divide
               | N_Op_Mod
               | N_Op_Rem
            =>
               if Do_Division_Check (Expr)
                    or else
                  Do_Overflow_Check (Expr)
               then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Left_Opnd  (Expr))
                      and then
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when N_Op_Add
               | N_Op_And
               | N_Op_Concat
               | N_Op_Eq
               | N_Op_Expon
               | N_Op_Ge
               | N_Op_Gt
               | N_Op_Le
               | N_Op_Lt
               | N_Op_Multiply
               | N_Op_Ne
               | N_Op_Or
               | N_Op_Rotate_Left
               | N_Op_Rotate_Right
               | N_Op_Shift_Left
               | N_Op_Shift_Right
               | N_Op_Shift_Right_Arithmetic
               | N_Op_Subtract
               | N_Op_Xor
            =>
               if Do_Overflow_Check (Expr) then
                  return False;
               else
                  return
                    Cannot_Raise_Constraint_Error (Left_Opnd  (Expr))
                      and then
                    Cannot_Raise_Constraint_Error (Right_Opnd (Expr));
               end if;

            when others =>
               return False;
         end case;
      end if;
   end Cannot_Raise_Constraint_Error;

   -----------------------------------------
   -- Check_Dynamically_Tagged_Expression --
   -----------------------------------------

   procedure Check_Dynamically_Tagged_Expression
     (Expr        : Node_Id;
      Typ         : Entity_Id;
      Related_Nod : Node_Id)
   is
   begin
      pragma Assert (Is_Tagged_Type (Typ));

      --  In order to avoid spurious errors when analyzing the expanded code,
      --  this check is done only for nodes that come from source and for
      --  actuals of generic instantiations.

      if (Comes_From_Source (Related_Nod)
           or else In_Generic_Actual (Expr))
        and then (Is_Class_Wide_Type (Etype (Expr))
                   or else Is_Dynamically_Tagged (Expr))
        and then Is_Tagged_Type (Typ)
        and then not Is_Class_Wide_Type (Typ)
      then
         Error_Msg_N ("dynamically tagged expression not allowed!", Expr);
      end if;
   end Check_Dynamically_Tagged_Expression;

   --------------------------
   -- Check_Fully_Declared --
   --------------------------

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id) is
   begin
      if Ekind (T) = E_Incomplete_Type then

         --  Ada 2005 (AI-50217): If the type is available through a limited
         --  with_clause, verify that its full view has been analyzed.

         if From_Limited_With (T)
           and then Present (Non_Limited_View (T))
           and then Ekind (Non_Limited_View (T)) /= E_Incomplete_Type
         then
            --  The non-limited view is fully declared

            null;

         else
            Error_Msg_NE
              ("premature usage of incomplete}", N, First_Subtype (T));
         end if;

      --  Need comments for these tests ???

      elsif Has_Private_Component (T)
        and then not Is_Generic_Type (Root_Type (T))
        and then not In_Spec_Expression
      then
         --  Special case: if T is the anonymous type created for a single
         --  task or protected object, use the name of the source object.

         if Is_Concurrent_Type (T)
           and then not Comes_From_Source (T)
           and then Nkind (N) = N_Object_Declaration
         then
            Error_Msg_NE
              ("type of& has incomplete component",
               N, Defining_Identifier (N));
         else
            Error_Msg_NE
              ("premature usage of incomplete}",
               N, First_Subtype (T));
         end if;
      end if;
   end Check_Fully_Declared;

   -------------------------------------------
   -- Check_Function_With_Address_Parameter --
   -------------------------------------------

   procedure Check_Function_With_Address_Parameter (Subp_Id : Entity_Id) is
      F : Entity_Id;
      T : Entity_Id;

   begin
      F := First_Formal (Subp_Id);
      while Present (F) loop
         T := Etype (F);

         if Is_Private_Type (T) and then Present (Full_View (T)) then
            T := Full_View (T);
         end if;

         if Is_Descendant_Of_Address (T) or else Is_Limited_Type (T) then
            Set_Is_Pure (Subp_Id, False);
            exit;
         end if;

         Next_Formal (F);
      end loop;
   end Check_Function_With_Address_Parameter;

   -------------------------------------
   -- Check_Function_Writable_Actuals --
   -------------------------------------

   procedure Check_Function_Writable_Actuals (N : Node_Id) is
      Writable_Actuals_List : Elist_Id := No_Elist;
      Identifiers_List      : Elist_Id := No_Elist;
      Aggr_Error_Node       : Node_Id  := Empty;
      Error_Node            : Node_Id  := Empty;

      procedure Collect_Identifiers (N : Node_Id);
      --  In a single traversal of subtree N collect in Writable_Actuals_List
      --  all the actuals of functions with writable actuals, and in the list
      --  Identifiers_List collect all the identifiers that are not actuals of
      --  functions with writable actuals. If a writable actual is referenced
      --  twice as writable actual then Error_Node is set to reference its
      --  second occurrence, the error is reported, and the tree traversal
      --  is abandoned.

      function Get_Function_Id (Call : Node_Id) return Entity_Id;
      --  Return the entity associated with the function call

      procedure Preanalyze_Without_Errors (N : Node_Id);
      --  Preanalyze N without reporting errors. Very dubious, you can't just
      --  go analyzing things more than once???

      -------------------------
      -- Collect_Identifiers --
      -------------------------

      procedure Collect_Identifiers (N : Node_Id) is

         function Check_Node (N : Node_Id) return Traverse_Result;
         --  Process a single node during the tree traversal to collect the
         --  writable actuals of functions and all the identifiers which are
         --  not writable actuals of functions.

         function Contains (List : Elist_Id; N : Node_Id) return Boolean;
         --  Returns True if List has a node whose Entity is Entity (N)

         ----------------
         -- Check_Node --
         ----------------

         function Check_Node (N : Node_Id) return Traverse_Result is
            Is_Writable_Actual : Boolean := False;
            Id                 : Entity_Id;

         begin
            if Nkind (N) = N_Identifier then

               --  No analysis possible if the entity is not decorated

               if No (Entity (N)) then
                  return Skip;

               --  Don't collect identifiers of packages, called functions, etc

               elsif Ekind_In (Entity (N), E_Package,
                                           E_Function,
                                           E_Procedure,
                                           E_Entry)
               then
                  return Skip;

               --  For rewritten nodes, continue the traversal in the original
               --  subtree. Needed to handle aggregates in original expressions
               --  extracted from the tree by Remove_Side_Effects.

               elsif Is_Rewrite_Substitution (N) then
                  Collect_Identifiers (Original_Node (N));
                  return Skip;

               --  For now we skip aggregate discriminants, since they require
               --  performing the analysis in two phases to identify conflicts:
               --  first one analyzing discriminants and second one analyzing
               --  the rest of components (since at run time, discriminants are
               --  evaluated prior to components): too much computation cost
               --  to identify a corner case???

               elsif Nkind (Parent (N)) = N_Component_Association
                  and then Nkind_In (Parent (Parent (N)),
                                     N_Aggregate,
                                     N_Extension_Aggregate)
               then
                  declare
                     Choice : constant Node_Id := First (Choices (Parent (N)));

                  begin
                     if Ekind (Entity (N)) = E_Discriminant then
                        return Skip;

                     elsif Expression (Parent (N)) = N
                       and then Nkind (Choice) = N_Identifier
                       and then Ekind (Entity (Choice)) = E_Discriminant
                     then
                        return Skip;
                     end if;
                  end;

               --  Analyze if N is a writable actual of a function

               elsif Nkind (Parent (N)) = N_Function_Call then
                  declare
                     Call   : constant Node_Id := Parent (N);
                     Actual : Node_Id;
                     Formal : Node_Id;

                  begin
                     Id := Get_Function_Id (Call);

                     --  In case of previous error, no check is possible

                     if No (Id) then
                        return Abandon;
                     end if;

                     if Ekind_In (Id, E_Function, E_Generic_Function)
                       and then Has_Out_Or_In_Out_Parameter (Id)
                     then
                        Formal := First_Formal (Id);
                        Actual := First_Actual (Call);
                        while Present (Actual) and then Present (Formal) loop
                           if Actual = N then
                              if Ekind_In (Formal, E_Out_Parameter,
                                                   E_In_Out_Parameter)
                              then
                                 Is_Writable_Actual := True;
                              end if;

                              exit;
                           end if;

                           Next_Formal (Formal);
                           Next_Actual (Actual);
                        end loop;
                     end if;
                  end;
               end if;

               if Is_Writable_Actual then

                  --  Skip checking the error in non-elementary types since
                  --  RM 6.4.1(6.15/3) is restricted to elementary types, but
                  --  store this actual in Writable_Actuals_List since it is
                  --  needed to perform checks on other constructs that have
                  --  arbitrary order of evaluation (for example, aggregates).

                  if not Is_Elementary_Type (Etype (N)) then
                     if not Contains (Writable_Actuals_List, N) then
                        Append_New_Elmt (N, To => Writable_Actuals_List);
                     end if;

                  --  Second occurrence of an elementary type writable actual

                  elsif Contains (Writable_Actuals_List, N) then

                     --  Report the error on the second occurrence of the
                     --  identifier. We cannot assume that N is the second
                     --  occurrence (according to their location in the
                     --  sources), since Traverse_Func walks through Field2
                     --  last (see comment in the body of Traverse_Func).

                     declare
                        Elmt : Elmt_Id;

                     begin
                        Elmt := First_Elmt (Writable_Actuals_List);
                        while Present (Elmt)
                           and then Entity (Node (Elmt)) /= Entity (N)
                        loop
                           Next_Elmt (Elmt);
                        end loop;

                        if Sloc (N) > Sloc (Node (Elmt)) then
                           Error_Node := N;
                        else
                           Error_Node := Node (Elmt);
                        end if;

                        Error_Msg_NE
                          ("value may be affected by call to & "
                           & "because order of evaluation is arbitrary",
                           Error_Node, Id);
                        return Abandon;
                     end;

                  --  First occurrence of a elementary type writable actual

                  else
                     Append_New_Elmt (N, To => Writable_Actuals_List);
                  end if;

               else
                  if Identifiers_List = No_Elist then
                     Identifiers_List := New_Elmt_List;
                  end if;

                  Append_Unique_Elmt (N, Identifiers_List);
               end if;
            end if;

            return OK;
         end Check_Node;

         --------------
         -- Contains --
         --------------

         function Contains
           (List : Elist_Id;
            N    : Node_Id) return Boolean
         is
            pragma Assert (Nkind (N) in N_Has_Entity);

            Elmt : Elmt_Id;

         begin
            if List = No_Elist then
               return False;
            end if;

            Elmt := First_Elmt (List);
            while Present (Elmt) loop
               if Entity (Node (Elmt)) = Entity (N) then
                  return True;
               else
                  Next_Elmt (Elmt);
               end if;
            end loop;

            return False;
         end Contains;

         ------------------
         -- Do_Traversal --
         ------------------

         procedure Do_Traversal is new Traverse_Proc (Check_Node);
         --  The traversal procedure

      --  Start of processing for Collect_Identifiers

      begin
         if Present (Error_Node) then
            return;
         end if;

         if Nkind (N) in N_Subexpr and then Is_OK_Static_Expression (N) then
            return;
         end if;

         Do_Traversal (N);
      end Collect_Identifiers;

      ---------------------
      -- Get_Function_Id --
      ---------------------

      function Get_Function_Id (Call : Node_Id) return Entity_Id is
         Nam : constant Node_Id := Name (Call);
         Id  : Entity_Id;

      begin
         if Nkind (Nam) = N_Explicit_Dereference then
            Id := Etype (Nam);
            pragma Assert (Ekind (Id) = E_Subprogram_Type);

         elsif Nkind (Nam) = N_Selected_Component then
            Id := Entity (Selector_Name (Nam));

         elsif Nkind (Nam) = N_Indexed_Component then
            Id := Entity (Selector_Name (Prefix (Nam)));

         else
            Id := Entity (Nam);
         end if;

         return Id;
      end Get_Function_Id;

      -------------------------------
      -- Preanalyze_Without_Errors --
      -------------------------------

      procedure Preanalyze_Without_Errors (N : Node_Id) is
         Status : constant Boolean := Get_Ignore_Errors;
      begin
         Set_Ignore_Errors (True);
         Preanalyze (N);
         Set_Ignore_Errors (Status);
      end Preanalyze_Without_Errors;

   --  Start of processing for Check_Function_Writable_Actuals

   begin
      --  The check only applies to Ada 2012 code on which Check_Actuals has
      --  been set, and only to constructs that have multiple constituents
      --  whose order of evaluation is not specified by the language.

      if Ada_Version < Ada_2012
        or else not Check_Actuals (N)
        or else (not (Nkind (N) in N_Op)
                  and then not (Nkind (N) in N_Membership_Test)
                  and then not Nkind_In (N, N_Range,
                                            N_Aggregate,
                                            N_Extension_Aggregate,
                                            N_Full_Type_Declaration,
                                            N_Function_Call,
                                            N_Procedure_Call_Statement,
                                            N_Entry_Call_Statement))
        or else (Nkind (N) = N_Full_Type_Declaration
                  and then not Is_Record_Type (Defining_Identifier (N)))

        --  In addition, this check only applies to source code, not to code
        --  generated by constraint checks.

        or else not Comes_From_Source (N)
      then
         return;
      end if;

      --  If a construct C has two or more direct constituents that are names
      --  or expressions whose evaluation may occur in an arbitrary order, at
      --  least one of which contains a function call with an in out or out
      --  parameter, then the construct is legal only if: for each name N that
      --  is passed as a parameter of mode in out or out to some inner function
      --  call C2 (not including the construct C itself), there is no other
      --  name anywhere within a direct constituent of the construct C other
      --  than the one containing C2, that is known to refer to the same
      --  object (RM 6.4.1(6.17/3)).

      case Nkind (N) is
         when N_Range =>
            Collect_Identifiers (Low_Bound (N));
            Collect_Identifiers (High_Bound (N));

         when N_Membership_Test
            | N_Op
         =>
            declare
               Expr : Node_Id;

            begin
               Collect_Identifiers (Left_Opnd (N));

               if Present (Right_Opnd (N)) then
                  Collect_Identifiers (Right_Opnd (N));
               end if;

               if Nkind_In (N, N_In, N_Not_In)
                 and then Present (Alternatives (N))
               then
                  Expr := First (Alternatives (N));
                  while Present (Expr) loop
                     Collect_Identifiers (Expr);

                     Next (Expr);
                  end loop;
               end if;
            end;

         when N_Full_Type_Declaration =>
            declare
               function Get_Record_Part (N : Node_Id) return Node_Id;
               --  Return the record part of this record type definition

               function Get_Record_Part (N : Node_Id) return Node_Id is
                  Type_Def : constant Node_Id := Type_Definition (N);
               begin
                  if Nkind (Type_Def) = N_Derived_Type_Definition then
                     return Record_Extension_Part (Type_Def);
                  else
                     return Type_Def;
                  end if;
               end Get_Record_Part;

               Comp   : Node_Id;
               Def_Id : Entity_Id := Defining_Identifier (N);
               Rec    : Node_Id   := Get_Record_Part (N);

            begin
               --  No need to perform any analysis if the record has no
               --  components

               if No (Rec) or else No (Component_List (Rec)) then
                  return;
               end if;

               --  Collect the identifiers starting from the deepest
               --  derivation. Done to report the error in the deepest
               --  derivation.

               loop
                  if Present (Component_List (Rec)) then
                     Comp := First (Component_Items (Component_List (Rec)));
                     while Present (Comp) loop
                        if Nkind (Comp) = N_Component_Declaration
                          and then Present (Expression (Comp))
                        then
                           Collect_Identifiers (Expression (Comp));
                        end if;

                        Next (Comp);
                     end loop;
                  end if;

                  exit when No (Underlying_Type (Etype (Def_Id)))
                    or else Base_Type (Underlying_Type (Etype (Def_Id)))
                              = Def_Id;

                  Def_Id := Base_Type (Underlying_Type (Etype (Def_Id)));
                  Rec := Get_Record_Part (Parent (Def_Id));
               end loop;
            end;

         when N_Entry_Call_Statement
            | N_Subprogram_Call
         =>
            declare
               Id     : constant Entity_Id := Get_Function_Id (N);
               Formal : Node_Id;
               Actual : Node_Id;

            begin
               Formal := First_Formal (Id);
               Actual := First_Actual (N);
               while Present (Actual) and then Present (Formal) loop
                  if Ekind_In (Formal, E_Out_Parameter,
                                       E_In_Out_Parameter)
                  then
                     Collect_Identifiers (Actual);
                  end if;

                  Next_Formal (Formal);
                  Next_Actual (Actual);
               end loop;
            end;

         when N_Aggregate
            | N_Extension_Aggregate
         =>
            declare
               Assoc     : Node_Id;
               Choice    : Node_Id;
               Comp_Expr : Node_Id;

            begin
               --  Handle the N_Others_Choice of array aggregates with static
               --  bounds. There is no need to perform this analysis in
               --  aggregates without static bounds since we cannot evaluate
               --  if the N_Others_Choice covers several elements. There is
               --  no need to handle the N_Others choice of record aggregates
               --  since at this stage it has been already expanded by
               --  Resolve_Record_Aggregate.

               if Is_Array_Type (Etype (N))
                 and then Nkind (N) = N_Aggregate
                 and then Present (Aggregate_Bounds (N))
                 and then Compile_Time_Known_Bounds (Etype (N))
                 and then Expr_Value (High_Bound (Aggregate_Bounds (N)))
                            >
                          Expr_Value (Low_Bound (Aggregate_Bounds (N)))
               then
                  declare
                     Count_Components   : Uint := Uint_0;
                     Num_Components     : Uint;
                     Others_Assoc       : Node_Id;
                     Others_Choice      : Node_Id := Empty;
                     Others_Box_Present : Boolean := False;

                  begin
                     --  Count positional associations

                     if Present (Expressions (N)) then
                        Comp_Expr := First (Expressions (N));
                        while Present (Comp_Expr) loop
                           Count_Components := Count_Components + 1;
                           Next (Comp_Expr);
                        end loop;
                     end if;

                     --  Count the rest of elements and locate the N_Others
                     --  choice (if any)

                     Assoc := First (Component_Associations (N));
                     while Present (Assoc) loop
                        Choice := First (Choices (Assoc));
                        while Present (Choice) loop
                           if Nkind (Choice) = N_Others_Choice then
                              Others_Assoc       := Assoc;
                              Others_Choice      := Choice;
                              Others_Box_Present := Box_Present (Assoc);

                           --  Count several components

                           elsif Nkind_In (Choice, N_Range,
                                                   N_Subtype_Indication)
                             or else (Is_Entity_Name (Choice)
                                       and then Is_Type (Entity (Choice)))
                           then
                              declare
                                 L, H : Node_Id;
                              begin
                                 Get_Index_Bounds (Choice, L, H);
                                 pragma Assert
                                   (Compile_Time_Known_Value (L)
                                     and then Compile_Time_Known_Value (H));
                                 Count_Components :=
                                   Count_Components
                                     + Expr_Value (H) - Expr_Value (L) + 1;
                              end;

                           --  Count single component. No other case available
                           --  since we are handling an aggregate with static
                           --  bounds.

                           else
                              pragma Assert (Is_OK_Static_Expression (Choice)
                                or else Nkind (Choice) = N_Identifier
                                or else Nkind (Choice) = N_Integer_Literal);

                              Count_Components := Count_Components + 1;
                           end if;

                           Next (Choice);
                        end loop;

                        Next (Assoc);
                     end loop;

                     Num_Components :=
                       Expr_Value (High_Bound (Aggregate_Bounds (N))) -
                         Expr_Value (Low_Bound (Aggregate_Bounds (N))) + 1;

                     pragma Assert (Count_Components <= Num_Components);

                     --  Handle the N_Others choice if it covers several
                     --  components

                     if Present (Others_Choice)
                       and then (Num_Components - Count_Components) > 1
                     then
                        if not Others_Box_Present then

                           --  At this stage, if expansion is active, the
                           --  expression of the others choice has not been
                           --  analyzed. Hence we generate a duplicate and
                           --  we analyze it silently to have available the
                           --  minimum decoration required to collect the
                           --  identifiers.

                           if not Expander_Active then
                              Comp_Expr := Expression (Others_Assoc);
                           else
                              Comp_Expr :=
                                New_Copy_Tree (Expression (Others_Assoc));
                              Preanalyze_Without_Errors (Comp_Expr);
                           end if;

                           Collect_Identifiers (Comp_Expr);

                           if Writable_Actuals_List /= No_Elist then

                              --  As suggested by Robert, at current stage we
                              --  report occurrences of this case as warnings.

                              Error_Msg_N
                                ("writable function parameter may affect "
                                 & "value in other component because order "
                                 & "of evaluation is unspecified??",
                                 Node (First_Elmt (Writable_Actuals_List)));
                           end if;
                        end if;
                     end if;
                  end;

               --  For an array aggregate, a discrete_choice_list that has
               --  a nonstatic range is considered as two or more separate
               --  occurrences of the expression (RM 6.4.1(20/3)).

               elsif Is_Array_Type (Etype (N))
                 and then Nkind (N) = N_Aggregate
                 and then Present (Aggregate_Bounds (N))
                 and then not Compile_Time_Known_Bounds (Etype (N))
               then
                  --  Collect identifiers found in the dynamic bounds

                  declare
                     Count_Components : Natural := 0;
                     Low, High        : Node_Id;

                  begin
                     Assoc := First (Component_Associations (N));
                     while Present (Assoc) loop
                        Choice := First (Choices (Assoc));
                        while Present (Choice) loop
                           if Nkind_In (Choice, N_Range,
                                                   N_Subtype_Indication)
                             or else (Is_Entity_Name (Choice)
                                       and then Is_Type (Entity (Choice)))
                           then
                              Get_Index_Bounds (Choice, Low, High);

                              if not Compile_Time_Known_Value (Low) then
                                 Collect_Identifiers (Low);

                                 if No (Aggr_Error_Node) then
                                    Aggr_Error_Node := Low;
                                 end if;
                              end if;

                              if not Compile_Time_Known_Value (High) then
                                 Collect_Identifiers (High);

                                 if No (Aggr_Error_Node) then
                                    Aggr_Error_Node := High;
                                 end if;
                              end if;

                           --  The RM rule is violated if there is more than
                           --  a single choice in a component association.

                           else
                              Count_Components := Count_Components + 1;

                              if No (Aggr_Error_Node)
                                and then Count_Components > 1
                              then
                                 Aggr_Error_Node := Choice;
                              end if;

                              if not Compile_Time_Known_Value (Choice) then
                                 Collect_Identifiers (Choice);
                              end if;
                           end if;

                           Next (Choice);
                        end loop;

                        Next (Assoc);
                     end loop;
                  end;
               end if;

               --  Handle ancestor part of extension aggregates

               if Nkind (N) = N_Extension_Aggregate then
                  Collect_Identifiers (Ancestor_Part (N));
               end if;

               --  Handle positional associations

               if Present (Expressions (N)) then
                  Comp_Expr := First (Expressions (N));
                  while Present (Comp_Expr) loop
                     if not Is_OK_Static_Expression (Comp_Expr) then
                        Collect_Identifiers (Comp_Expr);
                     end if;

                     Next (Comp_Expr);
                  end loop;
               end if;

               --  Handle discrete associations

               if Present (Component_Associations (N)) then
                  Assoc := First (Component_Associations (N));
                  while Present (Assoc) loop

                     if not Box_Present (Assoc) then
                        Choice := First (Choices (Assoc));
                        while Present (Choice) loop

                           --  For now we skip discriminants since it requires
                           --  performing the analysis in two phases: first one
                           --  analyzing discriminants and second one analyzing
                           --  the rest of components since discriminants are
                           --  evaluated prior to components: too much extra
                           --  work to detect a corner case???

                           if Nkind (Choice) in N_Has_Entity
                             and then Present (Entity (Choice))
                             and then Ekind (Entity (Choice)) = E_Discriminant
                           then
                              null;

                           elsif Box_Present (Assoc) then
                              null;

                           else
                              if not Analyzed (Expression (Assoc)) then
                                 Comp_Expr :=
                                   New_Copy_Tree (Expression (Assoc));
                                 Set_Parent (Comp_Expr, Parent (N));
                                 Preanalyze_Without_Errors (Comp_Expr);
                              else
                                 Comp_Expr := Expression (Assoc);
                              end if;

                              Collect_Identifiers (Comp_Expr);
                           end if;

                           Next (Choice);
                        end loop;
                     end if;

                     Next (Assoc);
                  end loop;
               end if;
            end;

         when others =>
            return;
      end case;

      --  No further action needed if we already reported an error

      if Present (Error_Node) then
         return;
      end if;

      --  Check violation of RM 6.20/3 in aggregates

      if Present (Aggr_Error_Node)
        and then Writable_Actuals_List /= No_Elist
      then
         Error_Msg_N
           ("value may be affected by call in other component because they "
            & "are evaluated in unspecified order",
            Node (First_Elmt (Writable_Actuals_List)));
         return;
      end if;

      --  Check if some writable argument of a function is referenced

      if Writable_Actuals_List /= No_Elist
        and then Identifiers_List /= No_Elist
      then
         declare
            Elmt_1 : Elmt_Id;
            Elmt_2 : Elmt_Id;

         begin
            Elmt_1 := First_Elmt (Writable_Actuals_List);
            while Present (Elmt_1) loop
               Elmt_2 := First_Elmt (Identifiers_List);
               while Present (Elmt_2) loop
                  if Entity (Node (Elmt_1)) = Entity (Node (Elmt_2)) then
                     case Nkind (Parent (Node (Elmt_2))) is
                        when N_Aggregate
                           | N_Component_Association
                           | N_Component_Declaration
                        =>
                           Error_Msg_N
                             ("value may be affected by call in other "
                              & "component because they are evaluated "
                              & "in unspecified order",
                              Node (Elmt_2));

                        when N_In
                           | N_Not_In
                        =>
                           Error_Msg_N
                             ("value may be affected by call in other "
                              & "alternative because they are evaluated "
                              & "in unspecified order",
                              Node (Elmt_2));

                        when others =>
                           Error_Msg_N
                             ("value of actual may be affected by call in "
                              & "other actual because they are evaluated "
                              & "in unspecified order",
                           Node (Elmt_2));
                     end case;
                  end if;

                  Next_Elmt (Elmt_2);
               end loop;

               Next_Elmt (Elmt_1);
            end loop;
         end;
      end if;
   end Check_Function_Writable_Actuals;

   --------------------------------
   -- Check_Implicit_Dereference --
   --------------------------------

   procedure Check_Implicit_Dereference (N : Node_Id;  Typ : Entity_Id) is
      Disc  : Entity_Id;
      Desig : Entity_Id;
      Nam   : Node_Id;

   begin
      if Nkind (N) = N_Indexed_Component
        and then Present (Generalized_Indexing (N))
      then
         Nam := Generalized_Indexing (N);
      else
         Nam := N;
      end if;

      if Ada_Version < Ada_2012
        or else not Has_Implicit_Dereference (Base_Type (Typ))
      then
         return;

      elsif not Comes_From_Source (N)
        and then Nkind (N) /= N_Indexed_Component
      then
         return;

      elsif Is_Entity_Name (Nam) and then Is_Type (Entity (Nam)) then
         null;

      else
         Disc := First_Discriminant (Typ);
         while Present (Disc) loop
            if Has_Implicit_Dereference (Disc) then
               Desig := Designated_Type (Etype (Disc));
               Add_One_Interp (Nam, Disc, Desig);

               --  If the node is a generalized indexing, add interpretation
               --  to that node as well, for subsequent resolution.

               if Nkind (N) = N_Indexed_Component then
                  Add_One_Interp (N, Disc, Desig);
               end if;

               --  If the operation comes from a generic unit and the context
               --  is a selected component, the selector name may be global
               --  and set in the instance already. Remove the entity to
               --  force resolution of the selected component, and the
               --  generation of an explicit dereference if needed.

               if In_Instance
                 and then Nkind (Parent (Nam)) = N_Selected_Component
               then
                  Set_Entity (Selector_Name (Parent (Nam)), Empty);
               end if;

               exit;
            end if;

            Next_Discriminant (Disc);
         end loop;
      end if;
   end Check_Implicit_Dereference;

   ----------------------------------
   -- Check_Internal_Protected_Use --
   ----------------------------------

   procedure Check_Internal_Protected_Use (N : Node_Id; Nam : Entity_Id) is
      S    : Entity_Id;
      Prot : Entity_Id;

   begin
      Prot := Empty;

      S := Current_Scope;
      while Present (S) loop
         if S = Standard_Standard then
            exit;

         elsif Ekind (S) = E_Function
           and then Ekind (Scope (S)) = E_Protected_Type
         then
            Prot := Scope (S);
            exit;
         end if;

         S := Scope (S);
      end loop;

      if Present (Prot)
        and then Scope (Nam) = Prot
        and then Ekind (Nam) /= E_Function
      then
         --  An indirect function call (e.g. a callback within a protected
         --  function body) is not statically illegal. If the access type is
         --  anonymous and is the type of an access parameter, the scope of Nam
         --  will be the protected type, but it is not a protected operation.

         if Ekind (Nam) = E_Subprogram_Type
           and then Nkind (Associated_Node_For_Itype (Nam)) =
                      N_Function_Specification
         then
            null;

         elsif Nkind (N) = N_Subprogram_Renaming_Declaration then
            Error_Msg_N
              ("within protected function cannot use protected procedure in "
               & "renaming or as generic actual", N);

         elsif Nkind (N) = N_Attribute_Reference then
            Error_Msg_N
              ("within protected function cannot take access of protected "
               & "procedure", N);

         else
            Error_Msg_N
              ("within protected function, protected object is constant", N);
            Error_Msg_N
              ("\cannot call operation that may modify it", N);
         end if;
      end if;

      --  Verify that an internal call does not appear within a precondition
      --  of a protected operation. This implements AI12-0166.
      --  The precondition aspect has been rewritten as a pragma Precondition
      --  and we check whether the scope of the called subprogram is the same
      --  as that of the entity to which the aspect applies.

      if Convention (Nam) = Convention_Protected then
         declare
            P : Node_Id;

         begin
            P := Parent (N);
            while Present (P) loop
               if Nkind (P) = N_Pragma
                 and then Chars (Pragma_Identifier (P)) = Name_Precondition
                 and then From_Aspect_Specification (P)
                 and then
                   Scope (Entity (Corresponding_Aspect (P))) = Scope (Nam)
               then
                  Error_Msg_N
                    ("internal call cannot appear in precondition of "
                     & "protected operation", N);
                  return;

               elsif Nkind (P) = N_Pragma
                 and then Chars (Pragma_Identifier (P)) = Name_Contract_Cases
               then
                  --  Check whether call is in a case guard. It is legal in a
                  --  consequence.

                  P := N;
                  while Present (P) loop
                     if Nkind (Parent (P)) = N_Component_Association
                       and then P /= Expression (Parent (P))
                     then
                        Error_Msg_N
                          ("internal call cannot appear in case guard in a "
                           & "contract case", N);
                     end if;

                     P := Parent (P);
                  end loop;

                  return;

               elsif Nkind (P) = N_Parameter_Specification
                 and then Scope (Current_Scope) = Scope (Nam)
                 and then Nkind_In (Parent (P), N_Entry_Declaration,
                                                N_Subprogram_Declaration)
               then
                  Error_Msg_N
                    ("internal call cannot appear in default for formal of "
                     & "protected operation", N);
                  return;
               end if;

               P := Parent (P);
            end loop;
         end;
      end if;
   end Check_Internal_Protected_Use;

   ---------------------------------------
   -- Check_Later_Vs_Basic_Declarations --
   ---------------------------------------

   procedure Check_Later_Vs_Basic_Declarations
     (Decls          : List_Id;
      During_Parsing : Boolean)
   is
      Body_Sloc : Source_Ptr;
      Decl      : Node_Id;

      function Is_Later_Declarative_Item (Decl : Node_Id) return Boolean;
      --  Return whether Decl is considered as a declarative item.
      --  When During_Parsing is True, the semantics of Ada 83 is followed.
      --  When During_Parsing is False, the semantics of SPARK is followed.

      -------------------------------
      -- Is_Later_Declarative_Item --
      -------------------------------

      function Is_Later_Declarative_Item (Decl : Node_Id) return Boolean is
      begin
         if Nkind (Decl) in N_Later_Decl_Item then
            return True;

         elsif Nkind (Decl) = N_Pragma then
            return True;

         elsif During_Parsing then
            return False;

         --  In SPARK, a package declaration is not considered as a later
         --  declarative item.

         elsif Nkind (Decl) = N_Package_Declaration then
            return False;

         --  In SPARK, a renaming is considered as a later declarative item

         elsif Nkind (Decl) in N_Renaming_Declaration then
            return True;

         else
            return False;
         end if;
      end Is_Later_Declarative_Item;

   --  Start of processing for Check_Later_Vs_Basic_Declarations

   begin
      Decl := First (Decls);

      --  Loop through sequence of basic declarative items

      Outer : while Present (Decl) loop
         if not Nkind_In (Decl, N_Subprogram_Body, N_Package_Body, N_Task_Body)
           and then Nkind (Decl) not in N_Body_Stub
         then
            Next (Decl);

            --  Once a body is encountered, we only allow later declarative
            --  items. The inner loop checks the rest of the list.

         else
            Body_Sloc := Sloc (Decl);

            Inner : while Present (Decl) loop
               if not Is_Later_Declarative_Item (Decl) then
                  if During_Parsing then
                     if Ada_Version = Ada_83 then
                        Error_Msg_Sloc := Body_Sloc;
                        Error_Msg_N
                          ("(Ada 83) decl cannot appear after body#", Decl);
                     end if;
                  else
                     Error_Msg_Sloc := Body_Sloc;
                     Check_SPARK_05_Restriction
                       ("decl cannot appear after body#", Decl);
                  end if;
               end if;

               Next (Decl);
            end loop Inner;
         end if;
      end loop Outer;
   end Check_Later_Vs_Basic_Declarations;

   ---------------------------
   -- Check_No_Hidden_State --
   ---------------------------

   procedure Check_No_Hidden_State (Id : Entity_Id) is
      function Has_Null_Abstract_State (Pkg : Entity_Id) return Boolean;
      --  Determine whether the entity of a package denoted by Pkg has a null
      --  abstract state.

      -----------------------------
      -- Has_Null_Abstract_State --
      -----------------------------

      function Has_Null_Abstract_State (Pkg : Entity_Id) return Boolean is
         States : constant Elist_Id := Abstract_States (Pkg);

      begin
         --  Check first available state of related package. A null abstract
         --  state always appears as the sole element of the state list.

         return
           Present (States)
             and then Is_Null_State (Node (First_Elmt (States)));
      end Has_Null_Abstract_State;

      --  Local variables

      Context     : Entity_Id := Empty;
      Not_Visible : Boolean   := False;
      Scop        : Entity_Id;

   --  Start of processing for Check_No_Hidden_State

   begin
      pragma Assert (Ekind_In (Id, E_Abstract_State, E_Variable));

      --  Find the proper context where the object or state appears

      Scop := Scope (Id);
      while Present (Scop) loop
         Context := Scop;

         --  Keep track of the context's visibility

         Not_Visible := Not_Visible or else In_Private_Part (Context);

         --  Prevent the search from going too far

         if Context = Standard_Standard then
            return;

         --  Objects and states that appear immediately within a subprogram or
         --  inside a construct nested within a subprogram do not introduce a
         --  hidden state. They behave as local variable declarations.

         elsif Is_Subprogram (Context) then
            return;

         --  When examining a package body, use the entity of the spec as it
         --  carries the abstract state declarations.

         elsif Ekind (Context) = E_Package_Body then
            Context := Spec_Entity (Context);
         end if;

         --  Stop the traversal when a package subject to a null abstract state
         --  has been found.

         if Ekind_In (Context, E_Generic_Package, E_Package)
           and then Has_Null_Abstract_State (Context)
         then
            exit;
         end if;

         Scop := Scope (Scop);
      end loop;

      --  At this point we know that there is at least one package with a null
      --  abstract state in visibility. Emit an error message unconditionally
      --  if the entity being processed is a state because the placement of the
      --  related package is irrelevant. This is not the case for objects as
      --  the intermediate context matters.

      if Present (Context)
        and then (Ekind (Id) = E_Abstract_State or else Not_Visible)
      then
         Error_Msg_N ("cannot introduce hidden state &", Id);
         Error_Msg_NE ("\package & has null abstract state", Id, Context);
      end if;
   end Check_No_Hidden_State;

   ----------------------------------------
   -- Check_Nonvolatile_Function_Profile --
   ----------------------------------------

   procedure Check_Nonvolatile_Function_Profile (Func_Id : Entity_Id) is
      Formal : Entity_Id;

   begin
      --  Inspect all formal parameters

      Formal := First_Formal (Func_Id);
      while Present (Formal) loop
         if Is_Effectively_Volatile (Etype (Formal)) then
            Error_Msg_NE
              ("nonvolatile function & cannot have a volatile parameter",
               Formal, Func_Id);
         end if;

         Next_Formal (Formal);
      end loop;

      --  Inspect the return type

      if Is_Effectively_Volatile (Etype (Func_Id)) then
         Error_Msg_NE
           ("nonvolatile function & cannot have a volatile return type",
            Result_Definition (Parent (Func_Id)), Func_Id);
      end if;
   end Check_Nonvolatile_Function_Profile;

   -----------------------------
   -- Check_Part_Of_Reference --
   -----------------------------

   procedure Check_Part_Of_Reference (Var_Id : Entity_Id; Ref : Node_Id) is
      Conc_Typ : constant Entity_Id := Encapsulating_State (Var_Id);
      Decl     : Node_Id;
      OK_Use   : Boolean := False;
      Par      : Node_Id;
      Prag_Nam : Name_Id;
      Spec_Id  : Entity_Id;

   begin
      --  Traverse the parent chain looking for a suitable context for the
      --  reference to the concurrent constituent.

      Par := Parent (Ref);
      while Present (Par) loop
         if Nkind (Par) = N_Pragma then
            Prag_Nam := Pragma_Name (Par);

            --  A concurrent constituent is allowed to appear in pragmas
            --  Initial_Condition and Initializes as this is part of the
            --  elaboration checks for the constituent (SPARK RM 9.3).

            if Nam_In (Prag_Nam, Name_Initial_Condition, Name_Initializes) then
               OK_Use := True;
               exit;

            --  When the reference appears within pragma Depends or Global,
            --  check whether the pragma applies to a single task type. Note
            --  that the pragma is not encapsulated by the type definition,
            --  but this is still a valid context.

            elsif Nam_In (Prag_Nam, Name_Depends, Name_Global) then
               Decl := Find_Related_Declaration_Or_Body (Par);

               if Nkind (Decl) = N_Object_Declaration
                 and then Defining_Entity (Decl) = Conc_Typ
               then
                  OK_Use := True;
                  exit;
               end if;
            end if;

         --  The reference appears somewhere in the definition of the single
         --  protected/task type (SPARK RM 9.3).

         elsif Nkind_In (Par, N_Single_Protected_Declaration,
                              N_Single_Task_Declaration)
           and then Defining_Entity (Par) = Conc_Typ
         then
            OK_Use := True;
            exit;

         --  The reference appears within the expanded declaration or the body
         --  of the single protected/task type (SPARK RM 9.3).

         elsif Nkind_In (Par, N_Protected_Body,
                              N_Protected_Type_Declaration,
                              N_Task_Body,
                              N_Task_Type_Declaration)
         then
            Spec_Id := Unique_Defining_Entity (Par);

            if Present (Anonymous_Object (Spec_Id))
              and then Anonymous_Object (Spec_Id) = Conc_Typ
            then
               OK_Use := True;
               exit;
            end if;

         --  The reference has been relocated within an internally generated
         --  package or subprogram. Assume that the reference is legal as the
         --  real check was already performed in the original context of the
         --  reference.

         elsif Nkind_In (Par, N_Package_Body,
                              N_Package_Declaration,
                              N_Subprogram_Body,
                              N_Subprogram_Declaration)
           and then not Comes_From_Source (Par)
         then
            --  Continue to examine the context if the reference appears in a
            --  subprogram body which was previously an expression function.

            if Nkind (Par) = N_Subprogram_Body
              and then Was_Expression_Function (Par)
            then
               null;

            --  Otherwise the reference is legal

            else
               OK_Use := True;
               exit;
            end if;

         --  The reference has been relocated to an inlined body for GNATprove.
         --  Assume that the reference is legal as the real check was already
         --  performed in the original context of the reference.

         elsif GNATprove_Mode
           and then Nkind (Par) = N_Subprogram_Body
           and then Chars (Defining_Entity (Par)) = Name_uParent
         then
            OK_Use := True;
            exit;
         end if;

         Par := Parent (Par);
      end loop;

      --  The reference is illegal as it appears outside the definition or
      --  body of the single protected/task type.

      if not OK_Use then
         Error_Msg_NE
           ("reference to variable & cannot appear in this context",
            Ref, Var_Id);
         Error_Msg_Name_1 := Chars (Var_Id);

         if Ekind (Conc_Typ) = E_Protected_Type then
            Error_Msg_NE
              ("\% is constituent of single protected type &", Ref, Conc_Typ);
         else
            Error_Msg_NE
              ("\% is constituent of single task type &", Ref, Conc_Typ);
         end if;
      end if;
   end Check_Part_Of_Reference;

   ------------------------------------------
   -- Check_Potentially_Blocking_Operation --
   ------------------------------------------

   procedure Check_Potentially_Blocking_Operation (N : Node_Id) is
      S : Entity_Id;

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
              ("potentially blocking operation in protected operation??", N);
            return;
         end if;

         S := Scope (S);
      end loop;
   end Check_Potentially_Blocking_Operation;

   ------------------------------------
   --  Check_Previous_Null_Procedure --
   ------------------------------------

   procedure Check_Previous_Null_Procedure
     (Decl : Node_Id;
      Prev : Entity_Id)
   is
   begin
      if Ekind (Prev) = E_Procedure
        and then Nkind (Parent (Prev)) = N_Procedure_Specification
        and then Null_Present (Parent (Prev))
      then
         Error_Msg_Sloc := Sloc (Prev);
         Error_Msg_N
           ("declaration cannot complete previous null procedure#", Decl);
      end if;
   end Check_Previous_Null_Procedure;

   ---------------------------------
   -- Check_Result_And_Post_State --
   ---------------------------------

   procedure Check_Result_And_Post_State (Subp_Id : Entity_Id) is
      procedure Check_Result_And_Post_State_In_Pragma
        (Prag        : Node_Id;
         Result_Seen : in out Boolean);
      --  Determine whether pragma Prag mentions attribute 'Result and whether
      --  the pragma contains an expression that evaluates differently in pre-
      --  and post-state. Prag is a [refined] postcondition or a contract-cases
      --  pragma. Result_Seen is set when the pragma mentions attribute 'Result

      function Has_In_Out_Parameter (Subp_Id : Entity_Id) return Boolean;
      --  Determine whether subprogram Subp_Id contains at least one IN OUT
      --  formal parameter.

      -------------------------------------------
      -- Check_Result_And_Post_State_In_Pragma --
      -------------------------------------------

      procedure Check_Result_And_Post_State_In_Pragma
        (Prag        : Node_Id;
         Result_Seen : in out Boolean)
      is
         procedure Check_Conjunct (Expr : Node_Id);
         --  Check an individual conjunct in a conjunction of Boolean
         --  expressions, connected by "and" or "and then" operators.

         procedure Check_Conjuncts (Expr : Node_Id);
         --  Apply the post-state check to every conjunct in an expression, in
         --  case this is a conjunction of Boolean expressions. Otherwise apply
         --  it to the expression as a whole.

         procedure Check_Expression (Expr : Node_Id);
         --  Perform the 'Result and post-state checks on a given expression

         function Is_Function_Result (N : Node_Id) return Traverse_Result;
         --  Attempt to find attribute 'Result in a subtree denoted by N

         function Is_Trivial_Boolean (N : Node_Id) return Boolean;
         --  Determine whether source node N denotes "True" or "False"

         function Mentions_Post_State (N : Node_Id) return Boolean;
         --  Determine whether a subtree denoted by N mentions any construct
         --  that denotes a post-state.

         procedure Check_Function_Result is
           new Traverse_Proc (Is_Function_Result);

         --------------------
         -- Check_Conjunct --
         --------------------

         procedure Check_Conjunct (Expr : Node_Id) is
            function Adjust_Message (Msg : String) return String;
            --  Prepend a prefix to the input message Msg denoting that the
            --  message applies to a conjunct in the expression, when this
            --  is the case.

            function Applied_On_Conjunct return Boolean;
            --  Returns True if the message applies to a conjunct in the
            --  expression, instead of the whole expression.

            --------------------
            -- Adjust_Message --
            --------------------

            function Adjust_Message (Msg : String) return String is
            begin
               if Applied_On_Conjunct then
                  return "conjunct in " & Msg;
               else
                  return Msg;
               end if;
            end Adjust_Message;

            -------------------------
            -- Applied_On_Conjunct --
            -------------------------

            function Applied_On_Conjunct return Boolean is
            begin
               --  Expr is the conjunct of an enclosing "and" expression

               return Nkind (Parent (Expr)) in N_Subexpr

                 --  or Expr is a conjunct of an enclosing "and then"
                 --  expression in a postcondition aspect that was split into
                 --  multiple pragmas. The first conjunct has the "and then"
                 --  expression as Original_Node, and other conjuncts have
                 --  Split_PCC set to True.

                 or else Nkind (Original_Node (Expr)) = N_And_Then
                 or else Split_PPC (Prag);
            end Applied_On_Conjunct;

            --  Local variables

            Err_Node : Node_Id;
            --  Error node when reporting a warning on a (refined)
            --  postcondition.

         --  Start of processing for Check_Conjunct

         begin
            if Applied_On_Conjunct then
               Err_Node := Expr;
            else
               Err_Node := Prag;
            end if;

            if not Is_Trivial_Boolean (Expr)
              and then not Mentions_Post_State (Expr)
            then
               if Pragma_Name (Prag) = Name_Contract_Cases then
                  Error_Msg_NE (Adjust_Message
                    ("contract case does not check the outcome of calling "
                     & "&?T?"), Expr, Subp_Id);

               elsif Pragma_Name (Prag) = Name_Refined_Post then
                  Error_Msg_NE (Adjust_Message
                    ("refined postcondition does not check the outcome of "
                     & "calling &?T?"), Err_Node, Subp_Id);

               else
                  Error_Msg_NE (Adjust_Message
                    ("postcondition does not check the outcome of calling "
                     & "&?T?"), Err_Node, Subp_Id);
               end if;
            end if;
         end Check_Conjunct;

         ---------------------
         -- Check_Conjuncts --
         ---------------------

         procedure Check_Conjuncts (Expr : Node_Id) is
         begin
            if Nkind_In (Expr, N_Op_And, N_And_Then) then
               Check_Conjuncts (Left_Opnd (Expr));
               Check_Conjuncts (Right_Opnd (Expr));
            else
               Check_Conjunct (Expr);
            end if;
         end Check_Conjuncts;

         ----------------------
         -- Check_Expression --
         ----------------------

         procedure Check_Expression (Expr : Node_Id) is
         begin
            if not Is_Trivial_Boolean (Expr) then
               Check_Function_Result (Expr);
               Check_Conjuncts (Expr);
            end if;
         end Check_Expression;

         ------------------------
         -- Is_Function_Result --
         ------------------------

         function Is_Function_Result (N : Node_Id) return Traverse_Result is
         begin
            if Is_Attribute_Result (N) then
               Result_Seen := True;
               return Abandon;

            --  Continue the traversal

            else
               return OK;
            end if;
         end Is_Function_Result;

         ------------------------
         -- Is_Trivial_Boolean --
         ------------------------

         function Is_Trivial_Boolean (N : Node_Id) return Boolean is
         begin
            return
              Comes_From_Source (N)
                and then Is_Entity_Name (N)
                and then (Entity (N) = Standard_True
                            or else
                          Entity (N) = Standard_False);
         end Is_Trivial_Boolean;

         -------------------------
         -- Mentions_Post_State --
         -------------------------

         function Mentions_Post_State (N : Node_Id) return Boolean is
            Post_State_Seen : Boolean := False;

            function Is_Post_State (N : Node_Id) return Traverse_Result;
            --  Attempt to find a construct that denotes a post-state. If this
            --  is the case, set flag Post_State_Seen.

            -------------------
            -- Is_Post_State --
            -------------------

            function Is_Post_State (N : Node_Id) return Traverse_Result is
               Ent : Entity_Id;

            begin
               if Nkind_In (N, N_Explicit_Dereference, N_Function_Call) then
                  Post_State_Seen := True;
                  return Abandon;

               elsif Nkind_In (N, N_Expanded_Name, N_Identifier) then
                  Ent := Entity (N);

                  --  Treat an undecorated reference as OK

                  if No (Ent)

                    --  A reference to an assignable entity is considered a
                    --  change in the post-state of a subprogram.

                    or else Ekind_In (Ent, E_Generic_In_Out_Parameter,
                                           E_In_Out_Parameter,
                                           E_Out_Parameter,
                                           E_Variable)

                    --  The reference may be modified through a dereference

                    or else (Is_Access_Type (Etype (Ent))
                              and then Nkind (Parent (N)) =
                                         N_Selected_Component)
                  then
                     Post_State_Seen := True;
                     return Abandon;
                  end if;

               elsif Nkind (N) = N_Attribute_Reference then
                  if Attribute_Name (N) = Name_Old then
                     return Skip;

                  elsif Attribute_Name (N) = Name_Result then
                     Post_State_Seen := True;
                     return Abandon;
                  end if;
               end if;

               return OK;
            end Is_Post_State;

            procedure Find_Post_State is new Traverse_Proc (Is_Post_State);

         --  Start of processing for Mentions_Post_State

         begin
            Find_Post_State (N);

            return Post_State_Seen;
         end Mentions_Post_State;

         --  Local variables

         Expr  : constant Node_Id :=
                   Get_Pragma_Arg
                     (First (Pragma_Argument_Associations (Prag)));
         Nam   : constant Name_Id := Pragma_Name (Prag);
         CCase : Node_Id;

      --  Start of processing for Check_Result_And_Post_State_In_Pragma

      begin
         --  Examine all consequences

         if Nam = Name_Contract_Cases then
            CCase := First (Component_Associations (Expr));
            while Present (CCase) loop
               Check_Expression (Expression (CCase));

               Next (CCase);
            end loop;

         --  Examine the expression of a postcondition

         else pragma Assert (Nam_In (Nam, Name_Postcondition,
                                          Name_Refined_Post));
            Check_Expression (Expr);
         end if;
      end Check_Result_And_Post_State_In_Pragma;

      --------------------------
      -- Has_In_Out_Parameter --
      --------------------------

      function Has_In_Out_Parameter (Subp_Id : Entity_Id) return Boolean is
         Formal : Entity_Id;

      begin
         --  Traverse the formals looking for an IN OUT parameter

         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            if Ekind (Formal) = E_In_Out_Parameter then
               return True;
            end if;

            Next_Formal (Formal);
         end loop;

         return False;
      end Has_In_Out_Parameter;

      --  Local variables

      Items        : constant Node_Id := Contract (Subp_Id);
      Subp_Decl    : constant Node_Id := Unit_Declaration_Node (Subp_Id);
      Case_Prag    : Node_Id := Empty;
      Post_Prag    : Node_Id := Empty;
      Prag         : Node_Id;
      Seen_In_Case : Boolean := False;
      Seen_In_Post : Boolean := False;
      Spec_Id      : Entity_Id;

   --  Start of processing for Check_Result_And_Post_State

   begin
      --  The lack of attribute 'Result or a post-state is classified as a
      --  suspicious contract. Do not perform the check if the corresponding
      --  swich is not set.

      if not Warn_On_Suspicious_Contract then
         return;

      --  Nothing to do if there is no contract

      elsif No (Items) then
         return;
      end if;

      --  Retrieve the entity of the subprogram spec (if any)

      if Nkind (Subp_Decl) = N_Subprogram_Body
        and then Present (Corresponding_Spec (Subp_Decl))
      then
         Spec_Id := Corresponding_Spec (Subp_Decl);

      elsif Nkind (Subp_Decl) = N_Subprogram_Body_Stub
        and then Present (Corresponding_Spec_Of_Stub (Subp_Decl))
      then
         Spec_Id := Corresponding_Spec_Of_Stub (Subp_Decl);

      else
         Spec_Id := Subp_Id;
      end if;

      --  Examine all postconditions for attribute 'Result and a post-state

      Prag := Pre_Post_Conditions (Items);
      while Present (Prag) loop
         if Nam_In (Pragma_Name_Unmapped (Prag),
                    Name_Postcondition, Name_Refined_Post)
           and then not Error_Posted (Prag)
         then
            Post_Prag := Prag;
            Check_Result_And_Post_State_In_Pragma (Prag, Seen_In_Post);
         end if;

         Prag := Next_Pragma (Prag);
      end loop;

      --  Examine the contract cases of the subprogram for attribute 'Result
      --  and a post-state.

      Prag := Contract_Test_Cases (Items);
      while Present (Prag) loop
         if Pragma_Name (Prag) = Name_Contract_Cases
           and then not Error_Posted (Prag)
         then
            Case_Prag := Prag;
            Check_Result_And_Post_State_In_Pragma (Prag, Seen_In_Case);
         end if;

         Prag := Next_Pragma (Prag);
      end loop;

      --  Do not emit any errors if the subprogram is not a function

      if not Ekind_In (Spec_Id, E_Function, E_Generic_Function) then
         null;

      --  Regardless of whether the function has postconditions or contract
      --  cases, or whether they mention attribute 'Result, an IN OUT formal
      --  parameter is always treated as a result.

      elsif Has_In_Out_Parameter (Spec_Id) then
         null;

      --  The function has both a postcondition and contract cases and they do
      --  not mention attribute 'Result.

      elsif Present (Case_Prag)
        and then not Seen_In_Case
        and then Present (Post_Prag)
        and then not Seen_In_Post
      then
         Error_Msg_N
           ("neither postcondition nor contract cases mention function "
            & "result?T?", Post_Prag);

      --  The function has contract cases only and they do not mention
      --  attribute 'Result.

      elsif Present (Case_Prag) and then not Seen_In_Case then
         Error_Msg_N ("contract cases do not mention result?T?", Case_Prag);

      --  The function has postconditions only and they do not mention
      --  attribute 'Result.

      elsif Present (Post_Prag) and then not Seen_In_Post then
         Error_Msg_N
           ("postcondition does not mention function result?T?", Post_Prag);
      end if;
   end Check_Result_And_Post_State;

   -----------------------------
   -- Check_State_Refinements --
   -----------------------------

   procedure Check_State_Refinements
     (Context      : Node_Id;
      Is_Main_Unit : Boolean := False)
   is
      procedure Check_Package (Pack : Node_Id);
      --  Verify that all abstract states of a [generic] package denoted by its
      --  declarative node Pack have proper refinement. Recursively verify the
      --  visible and private declarations of the [generic] package for other
      --  nested packages.

      procedure Check_Packages_In (Decls : List_Id);
      --  Seek out [generic] package declarations within declarative list Decls
      --  and verify the status of their abstract state refinement.

      function SPARK_Mode_Is_Off (N : Node_Id) return Boolean;
      --  Determine whether construct N is subject to pragma SPARK_Mode Off

      -------------------
      -- Check_Package --
      -------------------

      procedure Check_Package (Pack : Node_Id) is
         Body_Id : constant Entity_Id := Corresponding_Body (Pack);
         Spec    : constant Node_Id   := Specification (Pack);
         States  : constant Elist_Id  :=
                     Abstract_States (Defining_Entity (Pack));

         State_Elmt : Elmt_Id;
         State_Id   : Entity_Id;

      begin
         --  Do not verify proper state refinement when the package is subject
         --  to pragma SPARK_Mode Off because this disables the requirement for
         --  state refinement.

         if SPARK_Mode_Is_Off (Pack) then
            null;

         --  State refinement can only occur in a completing packge body. Do
         --  not verify proper state refinement when the body is subject to
         --  pragma SPARK_Mode Off because this disables the requirement for
         --  state refinement.

         elsif Present (Body_Id)
           and then SPARK_Mode_Is_Off (Unit_Declaration_Node (Body_Id))
         then
            null;

         --  Do not verify proper state refinement when the package is an
         --  instance as this check was already performed in the generic.

         elsif Present (Generic_Parent (Spec)) then
            null;

         --  Otherwise examine the contents of the package

         else
            if Present (States) then
               State_Elmt := First_Elmt (States);
               while Present (State_Elmt) loop
                  State_Id := Node (State_Elmt);

                  --  Emit an error when a non-null state lacks any form of
                  --  refinement.

                  if not Is_Null_State (State_Id)
                    and then not Has_Null_Refinement (State_Id)
                    and then not Has_Non_Null_Refinement (State_Id)
                  then
                     Error_Msg_N ("state & requires refinement", State_Id);
                  end if;

                  Next_Elmt (State_Elmt);
               end loop;
            end if;

            Check_Packages_In (Visible_Declarations (Spec));
            Check_Packages_In (Private_Declarations (Spec));
         end if;
      end Check_Package;

      -----------------------
      -- Check_Packages_In --
      -----------------------

      procedure Check_Packages_In (Decls : List_Id) is
         Decl : Node_Id;

      begin
         if Present (Decls) then
            Decl := First (Decls);
            while Present (Decl) loop
               if Nkind_In (Decl, N_Generic_Package_Declaration,
                                  N_Package_Declaration)
               then
                  Check_Package (Decl);
               end if;

               Next (Decl);
            end loop;
         end if;
      end Check_Packages_In;

      -----------------------
      -- SPARK_Mode_Is_Off --
      -----------------------

      function SPARK_Mode_Is_Off (N : Node_Id) return Boolean is
         Id   : constant Entity_Id := Defining_Entity (N);
         Prag : constant Node_Id   := SPARK_Pragma (Id);

      begin
         --  Default the mode to "off" when the context is an instance and all
         --  SPARK_Mode pragmas found within are to be ignored.

         if Ignore_SPARK_Mode_Pragmas (Id) then
            return True;

         else
            return
              Present (Prag)
                and then Get_SPARK_Mode_From_Annotation (Prag) = Off;
         end if;
      end SPARK_Mode_Is_Off;

   --  Start of processing for Check_State_Refinements

   begin
      --  A block may declare a nested package

      if Nkind (Context) = N_Block_Statement then
         Check_Packages_In (Declarations (Context));

      --  An entry, protected, subprogram, or task body may declare a nested
      --  package.

      elsif Nkind_In (Context, N_Entry_Body,
                               N_Protected_Body,
                               N_Subprogram_Body,
                               N_Task_Body)
      then
         --  Do not verify proper state refinement when the body is subject to
         --  pragma SPARK_Mode Off because this disables the requirement for
         --  state refinement.

         if not SPARK_Mode_Is_Off (Context) then
            Check_Packages_In (Declarations (Context));
         end if;

      --  A package body may declare a nested package

      elsif Nkind (Context) = N_Package_Body then
         Check_Package (Unit_Declaration_Node (Corresponding_Spec (Context)));

         --  Do not verify proper state refinement when the body is subject to
         --  pragma SPARK_Mode Off because this disables the requirement for
         --  state refinement.

         if not SPARK_Mode_Is_Off (Context) then
            Check_Packages_In (Declarations (Context));
         end if;

      --  A library level [generic] package may declare a nested package

      elsif Nkind_In (Context, N_Generic_Package_Declaration,
                               N_Package_Declaration)
        and then Is_Main_Unit
      then
         Check_Package (Context);
      end if;
   end Check_State_Refinements;

   ------------------------------
   -- Check_Unprotected_Access --
   ------------------------------

   procedure Check_Unprotected_Access
     (Context : Node_Id;
      Expr    : Node_Id)
   is
      Cont_Encl_Typ : Entity_Id;
      Pref_Encl_Typ : Entity_Id;

      function Enclosing_Protected_Type (Obj : Node_Id) return Entity_Id;
      --  Check whether Obj is a private component of a protected object.
      --  Return the protected type where the component resides, Empty
      --  otherwise.

      function Is_Public_Operation return Boolean;
      --  Verify that the enclosing operation is callable from outside the
      --  protected object, to minimize false positives.

      ------------------------------
      -- Enclosing_Protected_Type --
      ------------------------------

      function Enclosing_Protected_Type (Obj : Node_Id) return Entity_Id is
      begin
         if Is_Entity_Name (Obj) then
            declare
               Ent : Entity_Id := Entity (Obj);

            begin
               --  The object can be a renaming of a private component, use
               --  the original record component.

               if Is_Prival (Ent) then
                  Ent := Prival_Link (Ent);
               end if;

               if Is_Protected_Type (Scope (Ent)) then
                  return Scope (Ent);
               end if;
            end;
         end if;

         --  For indexed and selected components, recursively check the prefix

         if Nkind_In (Obj, N_Indexed_Component, N_Selected_Component) then
            return Enclosing_Protected_Type (Prefix (Obj));

         --  The object does not denote a protected component

         else
            return Empty;
         end if;
      end Enclosing_Protected_Type;

      -------------------------
      -- Is_Public_Operation --
      -------------------------

      function Is_Public_Operation return Boolean is
         S : Entity_Id;
         E : Entity_Id;

      begin
         S := Current_Scope;
         while Present (S) and then S /= Pref_Encl_Typ loop
            if Scope (S) = Pref_Encl_Typ then
               E := First_Entity (Pref_Encl_Typ);
               while Present (E)
                 and then E /= First_Private_Entity (Pref_Encl_Typ)
               loop
                  if E = S then
                     return True;
                  end if;

                  Next_Entity (E);
               end loop;
            end if;

            S := Scope (S);
         end loop;

         return False;
      end Is_Public_Operation;

   --  Start of processing for Check_Unprotected_Access

   begin
      if Nkind (Expr) = N_Attribute_Reference
        and then Attribute_Name (Expr) = Name_Unchecked_Access
      then
         Cont_Encl_Typ := Enclosing_Protected_Type (Context);
         Pref_Encl_Typ := Enclosing_Protected_Type (Prefix (Expr));

         --  Check whether we are trying to export a protected component to a
         --  context with an equal or lower access level.

         if Present (Pref_Encl_Typ)
           and then No (Cont_Encl_Typ)
           and then Is_Public_Operation
           and then Scope_Depth (Pref_Encl_Typ) >=
                                       Object_Access_Level (Context)
         then
            Error_Msg_N
              ("??possible unprotected access to protected data", Expr);
         end if;
      end if;
   end Check_Unprotected_Access;

   ------------------------------
   -- Check_Unused_Body_States --
   ------------------------------

   procedure Check_Unused_Body_States (Body_Id : Entity_Id) is
      procedure Process_Refinement_Clause
        (Clause : Node_Id;
         States : Elist_Id);
      --  Inspect all constituents of refinement clause Clause and remove any
      --  matches from body state list States.

      procedure Report_Unused_Body_States (States : Elist_Id);
      --  Emit errors for each abstract state or object found in list States

      -------------------------------
      -- Process_Refinement_Clause --
      -------------------------------

      procedure Process_Refinement_Clause
        (Clause : Node_Id;
         States : Elist_Id)
      is
         procedure Process_Constituent (Constit : Node_Id);
         --  Remove constituent Constit from body state list States

         -------------------------
         -- Process_Constituent --
         -------------------------

         procedure Process_Constituent (Constit : Node_Id) is
            Constit_Id : Entity_Id;

         begin
            --  Guard against illegal constituents. Only abstract states and
            --  objects can appear on the right hand side of a refinement.

            if Is_Entity_Name (Constit) then
               Constit_Id := Entity_Of (Constit);

               if Present (Constit_Id)
                 and then Ekind_In (Constit_Id, E_Abstract_State,
                                                E_Constant,
                                                E_Variable)
               then
                  Remove (States, Constit_Id);
               end if;
            end if;
         end Process_Constituent;

         --  Local variables

         Constit : Node_Id;

      --  Start of processing for Process_Refinement_Clause

      begin
         if Nkind (Clause) = N_Component_Association then
            Constit := Expression (Clause);

            --  Multiple constituents appear as an aggregate

            if Nkind (Constit) = N_Aggregate then
               Constit := First (Expressions (Constit));
               while Present (Constit) loop
                  Process_Constituent (Constit);
                  Next (Constit);
               end loop;

            --  Various forms of a single constituent

            else
               Process_Constituent (Constit);
            end if;
         end if;
      end Process_Refinement_Clause;

      -------------------------------
      -- Report_Unused_Body_States --
      -------------------------------

      procedure Report_Unused_Body_States (States : Elist_Id) is
         Posted     : Boolean := False;
         State_Elmt : Elmt_Id;
         State_Id   : Entity_Id;

      begin
         if Present (States) then
            State_Elmt := First_Elmt (States);
            while Present (State_Elmt) loop
               State_Id := Node (State_Elmt);

               --  Constants are part of the hidden state of a package, but the
               --  compiler cannot determine whether they have variable input
               --  (SPARK RM 7.1.1(2)) and cannot classify them properly as a
               --  hidden state. Do not emit an error when a constant does not
               --  participate in a state refinement, even though it acts as a
               --  hidden state.

               if Ekind (State_Id) = E_Constant then
                  null;

               --  Generate an error message of the form:

               --    body of package ... has unused hidden states
               --      abstract state ... defined at ...
               --      variable ... defined at ...

               else
                  if not Posted then
                     Posted := True;
                     SPARK_Msg_N
                       ("body of package & has unused hidden states", Body_Id);
                  end if;

                  Error_Msg_Sloc := Sloc (State_Id);

                  if Ekind (State_Id) = E_Abstract_State then
                     SPARK_Msg_NE
                       ("\abstract state & defined #", Body_Id, State_Id);

                  else
                     SPARK_Msg_NE ("\variable & defined #", Body_Id, State_Id);
                  end if;
               end if;

                  Next_Elmt (State_Elmt);
            end loop;
         end if;
      end Report_Unused_Body_States;

      --  Local variables

      Prag    : constant Node_Id := Get_Pragma (Body_Id, Pragma_Refined_State);
      Spec_Id : constant Entity_Id := Spec_Entity (Body_Id);
      Clause  : Node_Id;
      States  : Elist_Id;

   --  Start of processing for Check_Unused_Body_States

   begin
      --  Inspect the clauses of pragma Refined_State and determine whether all
      --  visible states declared within the package body participate in the
      --  refinement.

      if Present (Prag) then
         Clause := Expression (Get_Argument (Prag, Spec_Id));
         States := Collect_Body_States (Body_Id);

         --  Multiple non-null state refinements appear as an aggregate

         if Nkind (Clause) = N_Aggregate then
            Clause := First (Component_Associations (Clause));
            while Present (Clause) loop
               Process_Refinement_Clause (Clause, States);
               Next (Clause);
            end loop;

         --  Various forms of a single state refinement

         else
            Process_Refinement_Clause (Clause, States);
         end if;

         --  Ensure that all abstract states and objects declared in the
         --  package body state space are utilized as constituents.

         Report_Unused_Body_States (States);
      end if;
   end Check_Unused_Body_States;

   -----------------
   -- Choice_List --
   -----------------

   function Choice_List (N : Node_Id) return List_Id is
   begin
      if Nkind (N) = N_Iterated_Component_Association then
         return Discrete_Choices (N);
      else
         return Choices (N);
      end if;
   end Choice_List;

   -------------------------
   -- Collect_Body_States --
   -------------------------

   function Collect_Body_States (Body_Id : Entity_Id) return Elist_Id is
      function Is_Visible_Object (Obj_Id : Entity_Id) return Boolean;
      --  Determine whether object Obj_Id is a suitable visible state of a
      --  package body.

      procedure Collect_Visible_States
        (Pack_Id : Entity_Id;
         States  : in out Elist_Id);
      --  Gather the entities of all abstract states and objects declared in
      --  the visible state space of package Pack_Id.

      ----------------------------
      -- Collect_Visible_States --
      ----------------------------

      procedure Collect_Visible_States
        (Pack_Id : Entity_Id;
         States  : in out Elist_Id)
      is
         Item_Id : Entity_Id;

      begin
         --  Traverse the entity chain of the package and inspect all visible
         --  items.

         Item_Id := First_Entity (Pack_Id);
         while Present (Item_Id) and then not In_Private_Part (Item_Id) loop

            --  Do not consider internally generated items as those cannot be
            --  named and participate in refinement.

            if not Comes_From_Source (Item_Id) then
               null;

            elsif Ekind (Item_Id) = E_Abstract_State then
               Append_New_Elmt (Item_Id, States);

            elsif Ekind_In (Item_Id, E_Constant, E_Variable)
              and then Is_Visible_Object (Item_Id)
            then
               Append_New_Elmt (Item_Id, States);

            --  Recursively gather the visible states of a nested package

            elsif Ekind (Item_Id) = E_Package then
               Collect_Visible_States (Item_Id, States);
            end if;

            Next_Entity (Item_Id);
         end loop;
      end Collect_Visible_States;

      -----------------------
      -- Is_Visible_Object --
      -----------------------

      function Is_Visible_Object (Obj_Id : Entity_Id) return Boolean is
      begin
         --  Objects that map generic formals to their actuals are not visible
         --  from outside the generic instantiation.

         if Present (Corresponding_Generic_Association
                       (Declaration_Node (Obj_Id)))
         then
            return False;

         --  Constituents of a single protected/task type act as components of
         --  the type and are not visible from outside the type.

         elsif Ekind (Obj_Id) = E_Variable
           and then Present (Encapsulating_State (Obj_Id))
           and then Is_Single_Concurrent_Object (Encapsulating_State (Obj_Id))
         then
            return False;

         else
            return True;
         end if;
      end Is_Visible_Object;

      --  Local variables

      Body_Decl : constant Node_Id := Unit_Declaration_Node (Body_Id);
      Decl      : Node_Id;
      Item_Id   : Entity_Id;
      States    : Elist_Id := No_Elist;

   --  Start of processing for Collect_Body_States

   begin
      --  Inspect the declarations of the body looking for source objects,
      --  packages and package instantiations. Note that even though this
      --  processing is very similar to Collect_Visible_States, a package
      --  body does not have a First/Next_Entity list.

      Decl := First (Declarations (Body_Decl));
      while Present (Decl) loop

         --  Capture source objects as internally generated temporaries cannot
         --  be named and participate in refinement.

         if Nkind (Decl) = N_Object_Declaration then
            Item_Id := Defining_Entity (Decl);

            if Comes_From_Source (Item_Id)
              and then Is_Visible_Object (Item_Id)
            then
               Append_New_Elmt (Item_Id, States);
            end if;

         --  Capture the visible abstract states and objects of a source
         --  package [instantiation].

         elsif Nkind (Decl) = N_Package_Declaration then
            Item_Id := Defining_Entity (Decl);

            if Comes_From_Source (Item_Id) then
               Collect_Visible_States (Item_Id, States);
            end if;
         end if;

         Next (Decl);
      end loop;

      return States;
   end Collect_Body_States;

   ------------------------
   -- Collect_Interfaces --
   ------------------------

   procedure Collect_Interfaces
     (T               : Entity_Id;
      Ifaces_List     : out Elist_Id;
      Exclude_Parents : Boolean := False;
      Use_Full_View   : Boolean := True)
   is
      procedure Collect (Typ : Entity_Id);
      --  Subsidiary subprogram used to traverse the whole list
      --  of directly and indirectly implemented interfaces

      -------------
      -- Collect --
      -------------

      procedure Collect (Typ : Entity_Id) is
         Ancestor   : Entity_Id;
         Full_T     : Entity_Id;
         Id         : Node_Id;
         Iface      : Entity_Id;

      begin
         Full_T := Typ;

         --  Handle private types and subtypes

         if Use_Full_View
           and then Is_Private_Type (Typ)
           and then Present (Full_View (Typ))
         then
            Full_T := Full_View (Typ);

            if Ekind (Full_T) = E_Record_Subtype then
               Full_T := Etype (Typ);

               if Present (Full_View (Full_T)) then
                  Full_T := Full_View (Full_T);
               end if;
            end if;
         end if;

         --  Include the ancestor if we are generating the whole list of
         --  abstract interfaces.

         if Etype (Full_T) /= Typ

            --  Protect the frontend against wrong sources. For example:

            --    package P is
            --      type A is tagged null record;
            --      type B is new A with private;
            --      type C is new A with private;
            --    private
            --      type B is new C with null record;
            --      type C is new B with null record;
            --    end P;

           and then Etype (Full_T) /= T
         then
            Ancestor := Etype (Full_T);
            Collect (Ancestor);

            if Is_Interface (Ancestor) and then not Exclude_Parents then
               Append_Unique_Elmt (Ancestor, Ifaces_List);
            end if;
         end if;

         --  Traverse the graph of ancestor interfaces

         if Is_Non_Empty_List (Abstract_Interface_List (Full_T)) then
            Id := First (Abstract_Interface_List (Full_T));
            while Present (Id) loop
               Iface := Etype (Id);

               --  Protect against wrong uses. For example:
               --    type I is interface;
               --    type O is tagged null record;
               --    type Wrong is new I and O with null record; -- ERROR

               if Is_Interface (Iface) then
                  if Exclude_Parents
                    and then Etype (T) /= T
                    and then Interface_Present_In_Ancestor (Etype (T), Iface)
                  then
                     null;
                  else
                     Collect (Iface);
                     Append_Unique_Elmt (Iface, Ifaces_List);
                  end if;
               end if;

               Next (Id);
            end loop;
         end if;
      end Collect;

   --  Start of processing for Collect_Interfaces

   begin
      pragma Assert (Is_Tagged_Type (T) or else Is_Concurrent_Type (T));
      Ifaces_List := New_Elmt_List;
      Collect (T);
   end Collect_Interfaces;

   ----------------------------------
   -- Collect_Interface_Components --
   ----------------------------------

   procedure Collect_Interface_Components
     (Tagged_Type     : Entity_Id;
      Components_List : out Elist_Id)
   is
      procedure Collect (Typ : Entity_Id);
      --  Subsidiary subprogram used to climb to the parents

      -------------
      -- Collect --
      -------------

      procedure Collect (Typ : Entity_Id) is
         Tag_Comp   : Entity_Id;
         Parent_Typ : Entity_Id;

      begin
         --  Handle private types

         if Present (Full_View (Etype (Typ))) then
            Parent_Typ := Full_View (Etype (Typ));
         else
            Parent_Typ := Etype (Typ);
         end if;

         if Parent_Typ /= Typ

            --  Protect the frontend against wrong sources. For example:

            --    package P is
            --      type A is tagged null record;
            --      type B is new A with private;
            --      type C is new A with private;
            --    private
            --      type B is new C with null record;
            --      type C is new B with null record;
            --    end P;

           and then Parent_Typ /= Tagged_Type
         then
            Collect (Parent_Typ);
         end if;

         --  Collect the components containing tags of secondary dispatch
         --  tables.

         Tag_Comp := Next_Tag_Component (First_Tag_Component (Typ));
         while Present (Tag_Comp) loop
            pragma Assert (Present (Related_Type (Tag_Comp)));
            Append_Elmt (Tag_Comp, Components_List);

            Tag_Comp := Next_Tag_Component (Tag_Comp);
         end loop;
      end Collect;

   --  Start of processing for Collect_Interface_Components

   begin
      pragma Assert (Ekind (Tagged_Type) = E_Record_Type
        and then Is_Tagged_Type (Tagged_Type));

      Components_List := New_Elmt_List;
      Collect (Tagged_Type);
   end Collect_Interface_Components;

   -----------------------------
   -- Collect_Interfaces_Info --
   -----------------------------

   procedure Collect_Interfaces_Info
     (T               : Entity_Id;
      Ifaces_List     : out Elist_Id;
      Components_List : out Elist_Id;
      Tags_List       : out Elist_Id)
   is
      Comps_List : Elist_Id;
      Comp_Elmt  : Elmt_Id;
      Comp_Iface : Entity_Id;
      Iface_Elmt : Elmt_Id;
      Iface      : Entity_Id;

      function Search_Tag (Iface : Entity_Id) return Entity_Id;
      --  Search for the secondary tag associated with the interface type
      --  Iface that is implemented by T.

      ----------------
      -- Search_Tag --
      ----------------

      function Search_Tag (Iface : Entity_Id) return Entity_Id is
         ADT : Elmt_Id;
      begin
         if not Is_CPP_Class (T) then
            ADT := Next_Elmt (Next_Elmt (First_Elmt (Access_Disp_Table (T))));
         else
            ADT := Next_Elmt (First_Elmt (Access_Disp_Table (T)));
         end if;

         while Present (ADT)
           and then Is_Tag (Node (ADT))
           and then Related_Type (Node (ADT)) /= Iface
         loop
            --  Skip secondary dispatch table referencing thunks to user
            --  defined primitives covered by this interface.

            pragma Assert (Has_Suffix (Node (ADT), 'P'));
            Next_Elmt (ADT);

            --  Skip secondary dispatch tables of Ada types

            if not Is_CPP_Class (T) then

               --  Skip secondary dispatch table referencing thunks to
               --  predefined primitives.

               pragma Assert (Has_Suffix (Node (ADT), 'Y'));
               Next_Elmt (ADT);

               --  Skip secondary dispatch table referencing user-defined
               --  primitives covered by this interface.

               pragma Assert (Has_Suffix (Node (ADT), 'D'));
               Next_Elmt (ADT);

               --  Skip secondary dispatch table referencing predefined
               --  primitives.

               pragma Assert (Has_Suffix (Node (ADT), 'Z'));
               Next_Elmt (ADT);
            end if;
         end loop;

         pragma Assert (Is_Tag (Node (ADT)));
         return Node (ADT);
      end Search_Tag;

   --  Start of processing for Collect_Interfaces_Info

   begin
      Collect_Interfaces (T, Ifaces_List);
      Collect_Interface_Components (T, Comps_List);

      --  Search for the record component and tag associated with each
      --  interface type of T.

      Components_List := New_Elmt_List;
      Tags_List       := New_Elmt_List;

      Iface_Elmt := First_Elmt (Ifaces_List);
      while Present (Iface_Elmt) loop
         Iface := Node (Iface_Elmt);

         --  Associate the primary tag component and the primary dispatch table
         --  with all the interfaces that are parents of T

         if Is_Ancestor (Iface, T, Use_Full_View => True) then
            Append_Elmt (First_Tag_Component (T), Components_List);
            Append_Elmt (Node (First_Elmt (Access_Disp_Table (T))), Tags_List);

         --  Otherwise search for the tag component and secondary dispatch
         --  table of Iface

         else
            Comp_Elmt := First_Elmt (Comps_List);
            while Present (Comp_Elmt) loop
               Comp_Iface := Related_Type (Node (Comp_Elmt));

               if Comp_Iface = Iface
                 or else Is_Ancestor (Iface, Comp_Iface, Use_Full_View => True)
               then
                  Append_Elmt (Node (Comp_Elmt), Components_List);
                  Append_Elmt (Search_Tag (Comp_Iface), Tags_List);
                  exit;
               end if;

               Next_Elmt (Comp_Elmt);
            end loop;
            pragma Assert (Present (Comp_Elmt));
         end if;

         Next_Elmt (Iface_Elmt);
      end loop;
   end Collect_Interfaces_Info;

   ---------------------
   -- Collect_Parents --
   ---------------------

   procedure Collect_Parents
     (T             : Entity_Id;
      List          : out Elist_Id;
      Use_Full_View : Boolean := True)
   is
      Current_Typ : Entity_Id := T;
      Parent_Typ  : Entity_Id;

   begin
      List := New_Elmt_List;

      --  No action if the if the type has no parents

      if T = Etype (T) then
         return;
      end if;

      loop
         Parent_Typ := Etype (Current_Typ);

         if Is_Private_Type (Parent_Typ)
           and then Present (Full_View (Parent_Typ))
           and then Use_Full_View
         then
            Parent_Typ := Full_View (Base_Type (Parent_Typ));
         end if;

         Append_Elmt (Parent_Typ, List);

         exit when Parent_Typ = Current_Typ;
         Current_Typ := Parent_Typ;
      end loop;
   end Collect_Parents;

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
      Is_Type_In_Pkg : Boolean;
      Formal_Derived : Boolean := False;
      Id             : Entity_Id;

      function Match (E : Entity_Id) return Boolean;
      --  True if E's base type is B_Type, or E is of an anonymous access type
      --  and the base type of its designated type is B_Type.

      -----------
      -- Match --
      -----------

      function Match (E : Entity_Id) return Boolean is
         Etyp : Entity_Id := Etype (E);

      begin
         if Ekind (Etyp) = E_Anonymous_Access_Type then
            Etyp := Designated_Type (Etyp);
         end if;

         --  In Ada 2012 a primitive operation may have a formal of an
         --  incomplete view of the parent type.

         return Base_Type (Etyp) = B_Type
           or else
             (Ada_Version >= Ada_2012
               and then Ekind (Etyp) = E_Incomplete_Type
               and then Full_View (Etyp) = B_Type);
      end Match;

   --  Start of processing for Collect_Primitive_Operations

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
           and then Nkind (Formal_Type_Definition (B_Decl)) =
                                           N_Formal_Derived_Type_Definition
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

      --  Locate the primitive subprograms of the type

      else
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

         --  In Ada 2012, If the type has an incomplete partial view, there
         --  may be primitive operations declared before the full view, so
         --  we need to start scanning from the incomplete view, which is
         --  earlier on the entity chain.

         elsif Nkind (Parent (B_Type)) = N_Full_Type_Declaration
           and then Present (Incomplete_View (Parent (B_Type)))
         then
            Id := Defining_Entity (Incomplete_View (Parent (B_Type)));

            --  If T is a derived from a type with an incomplete view declared
            --  elsewhere, that incomplete view is irrelevant, we want the
            --  operations in the scope of T.

            if Scope (Id) /= Scope (B_Type) then
               Id := Next_Entity (B_Type);
            end if;

         else
            Id := Next_Entity (B_Type);
         end if;

         --  Set flag if this is a type in a package spec

         Is_Type_In_Pkg :=
           Is_Package_Or_Generic_Package (B_Scope)
             and then
               Nkind (Parent (Declaration_Node (First_Subtype (T)))) /=
                                                           N_Package_Body;

         while Present (Id) loop

            --  Test whether the result type or any of the parameter types of
            --  each subprogram following the type match that type when the
            --  type is declared in a package spec, is a derived type, or the
            --  subprogram is marked as primitive. (The Is_Primitive test is
            --  needed to find primitives of nonderived types in declarative
            --  parts that happen to override the predefined "=" operator.)

            --  Note that generic formal subprograms are not considered to be
            --  primitive operations and thus are never inherited.

            if Is_Overloadable (Id)
              and then (Is_Type_In_Pkg
                         or else Is_Derived_Type (B_Type)
                         or else Is_Primitive (Id))
              and then Nkind (Parent (Parent (Id)))
                         not in N_Formal_Subprogram_Declaration
            then
               Is_Prim := False;

               if Match (Id) then
                  Is_Prim := True;

               else
                  Formal := First_Formal (Id);
                  while Present (Formal) loop
                     if Match (Formal) then
                        Is_Prim := True;
                        exit;
                     end if;

                     Next_Formal (Formal);
                  end loop;
               end if;

               --  For a formal derived type, the only primitives are the ones
               --  inherited from the parent type. Operations appearing in the
               --  package declaration are not primitive for it.

               if Is_Prim
                 and then (not Formal_Derived or else Present (Alias (Id)))
               then
                  --  In the special case of an equality operator aliased to
                  --  an overriding dispatching equality belonging to the same
                  --  type, we don't include it in the list of primitives.
                  --  This avoids inheriting multiple equality operators when
                  --  deriving from untagged private types whose full type is
                  --  tagged, which can otherwise cause ambiguities. Note that
                  --  this should only happen for this kind of untagged parent
                  --  type, since normally dispatching operations are inherited
                  --  using the type's Primitive_Operations list.

                  if Chars (Id) = Name_Op_Eq
                    and then Is_Dispatching_Operation (Id)
                    and then Present (Alias (Id))
                    and then Present (Overridden_Operation (Alias (Id)))
                    and then Base_Type (Etype (First_Entity (Id))) =
                               Base_Type (Etype (First_Entity (Alias (Id))))
                  then
                     null;

                  --  Include the subprogram in the list of primitives

                  else
                     Append_Elmt (Id, Op_List);
                  end if;
               end if;
            end if;

            Next_Entity (Id);

            --  For a type declared in System, some of its operations may
            --  appear in the target-specific extension to System.

            if No (Id)
              and then B_Scope = RTU_Entity (System)
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
      Warn : Boolean    := False) return Node_Id
   is
      Msgc : String (1 .. Msg'Length + 3);
      --  Copy of message, with room for possible ?? or << and ! at end

      Msgl : Natural;
      Wmsg : Boolean;
      Eloc : Source_Ptr;

   --  Start of processing for Compile_Time_Constraint_Error

   begin
      --  If this is a warning, convert it into an error if we are in code
      --  subject to SPARK_Mode being set On, unless Warn is True to force a
      --  warning. The rationale is that a compile-time constraint error should
      --  lead to an error instead of a warning when SPARK_Mode is On, but in
      --  a few cases we prefer to issue a warning and generate both a suitable
      --  run-time error in GNAT and a suitable check message in GNATprove.
      --  Those cases are those that likely correspond to deactivated SPARK
      --  code, so that this kind of code can be compiled and analyzed instead
      --  of being rejected.

      Error_Msg_Warn := Warn or SPARK_Mode /= On;

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

         --  Copy message to Msgc, converting any ? in the message into <
         --  instead, so that we have an error in GNATprove mode.

         Msgl := Msg'Length;

         for J in 1 .. Msgl loop
            if Msg (J) = '?' and then (J = 1 or else Msg (J - 1) /= ''') then
               Msgc (J) := '<';
            else
               Msgc (J) := Msg (J);
            end if;
         end loop;

         --  Message is a warning, even in Ada 95 case

         if Msg (Msg'Last) = '?' or else Msg (Msg'Last) = '<' then
            Wmsg := True;

         --  In Ada 83, all messages are warnings. In the private part and the
         --  body of an instance, constraint_checks are only warnings. We also
         --  make this a warning if the Warn parameter is set.

         elsif Warn
           or else (Ada_Version = Ada_83 and then Comes_From_Source (N))
           or else In_Instance_Not_Visible
         then
            Msgl := Msgl + 1;
            Msgc (Msgl) := '<';
            Msgl := Msgl + 1;
            Msgc (Msgl) := '<';
            Wmsg := True;

         --  Otherwise we have a real error message (Ada 95 static case) and we
         --  make this an unconditional message. Note that in the warning case
         --  we do not make the message unconditional, it seems reasonable to
         --  delete messages like this (about exceptions that will be raised)
         --  in dead code.

         else
            Wmsg := False;
            Msgl := Msgl + 1;
            Msgc (Msgl) := '!';
         end if;

         --  One more test, skip the warning if the related expression is
         --  statically unevaluated, since we don't want to warn about what
         --  will happen when something is evaluated if it never will be
         --  evaluated.

         if not Is_Statically_Unevaluated (N) then
            if Present (Ent) then
               Error_Msg_NEL (Msgc (1 .. Msgl), N, Ent, Eloc);
            else
               Error_Msg_NEL (Msgc (1 .. Msgl), N, Etype (N), Eloc);
            end if;

            if Wmsg then

               --  Check whether the context is an Init_Proc

               if Inside_Init_Proc then
                  declare
                     Conc_Typ : constant Entity_Id :=
                                  Corresponding_Concurrent_Type
                                    (Entity (Parameter_Type (First
                                      (Parameter_Specifications
                                        (Parent (Current_Scope))))));

                  begin
                     --  Don't complain if the corresponding concurrent type
                     --  doesn't come from source (i.e. a single task/protected
                     --  object).

                     if Present (Conc_Typ)
                       and then not Comes_From_Source (Conc_Typ)
                     then
                        Error_Msg_NEL
                          ("\& [<<", N, Standard_Constraint_Error, Eloc);

                     else
                        if GNATprove_Mode then
                           Error_Msg_NEL
                             ("\& would have been raised for objects of this "
                              & "type", N, Standard_Constraint_Error, Eloc);
                        else
                           Error_Msg_NEL
                             ("\& will be raised for objects of this type??",
                              N, Standard_Constraint_Error, Eloc);
                        end if;
                     end if;
                  end;

               else
                  Error_Msg_NEL ("\& [<<", N, Standard_Constraint_Error, Eloc);
               end if;

            else
               Error_Msg ("\static expression fails Constraint_Check", Eloc);
               Set_Error_Posted (N);
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

   ----------------------------
   -- Contains_Refined_State --
   ----------------------------

   function Contains_Refined_State (Prag : Node_Id) return Boolean is
      function Has_State_In_Dependency (List : Node_Id) return Boolean;
      --  Determine whether a dependency list mentions a state with a visible
      --  refinement.

      function Has_State_In_Global (List : Node_Id) return Boolean;
      --  Determine whether a global list mentions a state with a visible
      --  refinement.

      function Is_Refined_State (Item : Node_Id) return Boolean;
      --  Determine whether Item is a reference to an abstract state with a
      --  visible refinement.

      -----------------------------
      -- Has_State_In_Dependency --
      -----------------------------

      function Has_State_In_Dependency (List : Node_Id) return Boolean is
         Clause : Node_Id;
         Output : Node_Id;

      begin
         --  A null dependency list does not mention any states

         if Nkind (List) = N_Null then
            return False;

         --  Dependency clauses appear as component associations of an
         --  aggregate.

         elsif Nkind (List) = N_Aggregate
           and then Present (Component_Associations (List))
         then
            Clause := First (Component_Associations (List));
            while Present (Clause) loop

               --  Inspect the outputs of a dependency clause

               Output := First (Choices (Clause));
               while Present (Output) loop
                  if Is_Refined_State (Output) then
                     return True;
                  end if;

                  Next (Output);
               end loop;

               --  Inspect the outputs of a dependency clause

               if Is_Refined_State (Expression (Clause)) then
                  return True;
               end if;

               Next (Clause);
            end loop;

            --  If we get here, then none of the dependency clauses mention a
            --  state with visible refinement.

            return False;

         --  An illegal pragma managed to sneak in

         else
            raise Program_Error;
         end if;
      end Has_State_In_Dependency;

      -------------------------
      -- Has_State_In_Global --
      -------------------------

      function Has_State_In_Global (List : Node_Id) return Boolean is
         Item : Node_Id;

      begin
         --  A null global list does not mention any states

         if Nkind (List) = N_Null then
            return False;

         --  Simple global list or moded global list declaration

         elsif Nkind (List) = N_Aggregate then

            --  The declaration of a simple global list appear as a collection
            --  of expressions.

            if Present (Expressions (List)) then
               Item := First (Expressions (List));
               while Present (Item) loop
                  if Is_Refined_State (Item) then
                     return True;
                  end if;

                  Next (Item);
               end loop;

            --  The declaration of a moded global list appears as a collection
            --  of component associations where individual choices denote
            --  modes.

            else
               Item := First (Component_Associations (List));
               while Present (Item) loop
                  if Has_State_In_Global (Expression (Item)) then
                     return True;
                  end if;

                  Next (Item);
               end loop;
            end if;

            --  If we get here, then the simple/moded global list did not
            --  mention any states with a visible refinement.

            return False;

         --  Single global item declaration

         elsif Is_Entity_Name (List) then
            return Is_Refined_State (List);

         --  An illegal pragma managed to sneak in

         else
            raise Program_Error;
         end if;
      end Has_State_In_Global;

      ----------------------
      -- Is_Refined_State --
      ----------------------

      function Is_Refined_State (Item : Node_Id) return Boolean is
         Elmt    : Node_Id;
         Item_Id : Entity_Id;

      begin
         if Nkind (Item) = N_Null then
            return False;

         --  States cannot be subject to attribute 'Result. This case arises
         --  in dependency relations.

         elsif Nkind (Item) = N_Attribute_Reference
           and then Attribute_Name (Item) = Name_Result
         then
            return False;

         --  Multiple items appear as an aggregate. This case arises in
         --  dependency relations.

         elsif Nkind (Item) = N_Aggregate
           and then Present (Expressions (Item))
         then
            Elmt := First (Expressions (Item));
            while Present (Elmt) loop
               if Is_Refined_State (Elmt) then
                  return True;
               end if;

               Next (Elmt);
            end loop;

            --  If we get here, then none of the inputs or outputs reference a
            --  state with visible refinement.

            return False;

         --  Single item

         else
            Item_Id := Entity_Of (Item);

            return
              Present (Item_Id)
                and then Ekind (Item_Id) = E_Abstract_State
                and then Has_Visible_Refinement (Item_Id);
         end if;
      end Is_Refined_State;

      --  Local variables

      Arg : constant Node_Id :=
              Get_Pragma_Arg (First (Pragma_Argument_Associations (Prag)));
      Nam : constant Name_Id := Pragma_Name (Prag);

   --  Start of processing for Contains_Refined_State

   begin
      if Nam = Name_Depends then
         return Has_State_In_Dependency (Arg);

      else pragma Assert (Nam = Name_Global);
         return Has_State_In_Global (Arg);
      end if;
   end Contains_Refined_State;

   -------------------------
   -- Copy_Component_List --
   -------------------------

   function Copy_Component_List
     (R_Typ : Entity_Id;
      Loc   : Source_Ptr) return List_Id
   is
      Comp  : Node_Id;
      Comps : constant List_Id := New_List;

   begin
      Comp := First_Component (Underlying_Type (R_Typ));
      while Present (Comp) loop
         if Comes_From_Source (Comp) then
            declare
               Comp_Decl : constant Node_Id := Declaration_Node (Comp);
            begin
               Append_To (Comps,
                 Make_Component_Declaration (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Chars (Comp)),
                   Component_Definition =>
                     New_Copy_Tree
                       (Component_Definition (Comp_Decl), New_Sloc => Loc)));
            end;
         end if;

         Next_Component (Comp);
      end loop;

      return Comps;
   end Copy_Component_List;

   -------------------------
   -- Copy_Parameter_List --
   -------------------------

   function Copy_Parameter_List (Subp_Id : Entity_Id) return List_Id is
      Loc    : constant Source_Ptr := Sloc (Subp_Id);
      Plist  : List_Id;
      Formal : Entity_Id;

   begin
      if No (First_Formal (Subp_Id)) then
         return No_List;
      else
         Plist  := New_List;
         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Append_To (Plist,
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Sloc (Formal), Chars (Formal)),
                In_Present          => In_Present (Parent (Formal)),
                Out_Present         => Out_Present (Parent (Formal)),
                Parameter_Type      =>
                  New_Occurrence_Of (Etype (Formal), Loc),
                Expression          =>
                  New_Copy_Tree (Expression (Parent (Formal)))));

            Next_Formal (Formal);
         end loop;
      end if;

      return Plist;
   end Copy_Parameter_List;

   ----------------------------
   -- Copy_SPARK_Mode_Aspect --
   ----------------------------

   procedure Copy_SPARK_Mode_Aspect (From : Node_Id; To : Node_Id) is
      pragma Assert (not Has_Aspects (To));
      Asp : Node_Id;

   begin
      if Has_Aspects (From) then
         Asp := Find_Aspect (Defining_Entity (From), Aspect_SPARK_Mode);

         if Present (Asp) then
            Set_Aspect_Specifications (To, New_List (New_Copy_Tree (Asp)));
            Set_Has_Aspects (To, True);
         end if;
      end if;
   end Copy_SPARK_Mode_Aspect;

   --------------------------
   -- Copy_Subprogram_Spec --
   --------------------------

   function Copy_Subprogram_Spec (Spec : Node_Id) return Node_Id is
      Def_Id      : Node_Id;
      Formal_Spec : Node_Id;
      Result      : Node_Id;

   begin
      --  The structure of the original tree must be replicated without any
      --  alterations. Use New_Copy_Tree for this purpose.

      Result := New_Copy_Tree (Spec);

      --  However, the spec of a null procedure carries the corresponding null
      --  statement of the body (created by the parser), and this cannot be
      --  shared with the new subprogram spec.

      if Nkind (Result) = N_Procedure_Specification then
         Set_Null_Statement (Result, Empty);
      end if;

      --  Create a new entity for the defining unit name

      Def_Id := Defining_Unit_Name (Result);
      Set_Defining_Unit_Name (Result,
        Make_Defining_Identifier (Sloc (Def_Id), Chars (Def_Id)));

      --  Create new entities for the formal parameters

      if Present (Parameter_Specifications (Result)) then
         Formal_Spec := First (Parameter_Specifications (Result));
         while Present (Formal_Spec) loop
            Def_Id := Defining_Identifier (Formal_Spec);
            Set_Defining_Identifier (Formal_Spec,
              Make_Defining_Identifier (Sloc (Def_Id), Chars (Def_Id)));

            Next (Formal_Spec);
         end loop;
      end if;

      return Result;
   end Copy_Subprogram_Spec;

   --------------------------------
   -- Corresponding_Generic_Type --
   --------------------------------

   function Corresponding_Generic_Type (T : Entity_Id) return Entity_Id is
      Inst : Entity_Id;
      Gen  : Entity_Id;
      Typ  : Entity_Id;

   begin
      if not Is_Generic_Actual_Type (T) then
         return Any_Type;

      --  If the actual is the actual of an enclosing instance, resolution
      --  was correct in the generic.

      elsif Nkind (Parent (T)) = N_Subtype_Declaration
        and then Is_Entity_Name (Subtype_Indication (Parent (T)))
        and then
          Is_Generic_Actual_Type (Entity (Subtype_Indication (Parent (T))))
      then
         return Any_Type;

      else
         Inst := Scope (T);

         if Is_Wrapper_Package (Inst) then
            Inst := Related_Instance (Inst);
         end if;

         Gen  :=
           Generic_Parent
             (Specification (Unit_Declaration_Node (Inst)));

         --  Generic actual has the same name as the corresponding formal

         Typ := First_Entity (Gen);
         while Present (Typ) loop
            if Chars (Typ) = Chars (T) then
               return Typ;
            end if;

            Next_Entity (Typ);
         end loop;

         return Any_Type;
      end if;
   end Corresponding_Generic_Type;

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

   ----------------------------
   -- Current_Scope_No_Loops --
   ----------------------------

   function Current_Scope_No_Loops return Entity_Id is
      S : Entity_Id;

   begin
      --  Examine the scope stack starting from the current scope and skip any
      --  internally generated loops.

      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Ekind (S) = E_Loop and then not Comes_From_Source (S) then
            S := Scope (S);
         else
            exit;
         end if;
      end loop;

      return S;
   end Current_Scope_No_Loops;

   ------------------------
   -- Current_Subprogram --
   ------------------------

   function Current_Subprogram return Entity_Id is
      Scop : constant Entity_Id := Current_Scope;
   begin
      if Is_Subprogram_Or_Generic_Subprogram (Scop) then
         return Scop;
      else
         return Enclosing_Subprogram (Scop);
      end if;
   end Current_Subprogram;

   ----------------------------------
   -- Deepest_Type_Access_Level --
   ----------------------------------

   function Deepest_Type_Access_Level (Typ : Entity_Id) return Uint is
   begin
      if Ekind (Typ) = E_Anonymous_Access_Type
        and then not Is_Local_Anonymous_Access (Typ)
        and then Nkind (Associated_Node_For_Itype (Typ)) = N_Object_Declaration
      then
         --  Typ is the type of an Ada 2012 stand-alone object of an anonymous
         --  access type.

         return
           Scope_Depth (Enclosing_Dynamic_Scope
                         (Defining_Identifier
                           (Associated_Node_For_Itype (Typ))));

      --  For generic formal type, return Int'Last (infinite).
      --  See comment preceding Is_Generic_Type call in Type_Access_Level.

      elsif Is_Generic_Type (Root_Type (Typ)) then
         return UI_From_Int (Int'Last);

      else
         return Type_Access_Level (Typ);
      end if;
   end Deepest_Type_Access_Level;

   ---------------------
   -- Defining_Entity --
   ---------------------

   function Defining_Entity
     (N               : Node_Id;
      Empty_On_Errors : Boolean := False) return Entity_Id
   is
      Err : Entity_Id := Empty;

   begin
      case Nkind (N) is
         when N_Abstract_Subprogram_Declaration
            | N_Expression_Function
            | N_Formal_Subprogram_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Package_Declaration
            | N_Subprogram_Body
            | N_Subprogram_Body_Stub
            | N_Subprogram_Declaration
            | N_Subprogram_Renaming_Declaration
         =>
            return Defining_Entity (Specification (N));

         when N_Component_Declaration
            | N_Defining_Program_Unit_Name
            | N_Discriminant_Specification
            | N_Entry_Body
            | N_Entry_Declaration
            | N_Entry_Index_Specification
            | N_Exception_Declaration
            | N_Exception_Renaming_Declaration
            | N_Formal_Object_Declaration
            | N_Formal_Package_Declaration
            | N_Formal_Type_Declaration
            | N_Full_Type_Declaration
            | N_Implicit_Label_Declaration
            | N_Incomplete_Type_Declaration
            | N_Iterator_Specification
            | N_Loop_Parameter_Specification
            | N_Number_Declaration
            | N_Object_Declaration
            | N_Object_Renaming_Declaration
            | N_Package_Body_Stub
            | N_Parameter_Specification
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration
            | N_Protected_Body
            | N_Protected_Body_Stub
            | N_Protected_Type_Declaration
            | N_Single_Protected_Declaration
            | N_Single_Task_Declaration
            | N_Subtype_Declaration
            | N_Task_Body
            | N_Task_Body_Stub
            | N_Task_Type_Declaration
         =>
            return Defining_Identifier (N);

         when N_Subunit =>
            return Defining_Entity (Proper_Body (N));

         when N_Function_Instantiation
            | N_Function_Specification
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Package_Body
            | N_Package_Instantiation
            | N_Package_Renaming_Declaration
            | N_Package_Specification
            | N_Procedure_Instantiation
            | N_Procedure_Specification
         =>
            declare
               Nam : constant Node_Id := Defining_Unit_Name (N);

            begin
               if Nkind (Nam) in N_Entity then
                  return Nam;

               --  For Error, make up a name and attach to declaration so we
               --  can continue semantic analysis.

               elsif Nam = Error then
                  if Empty_On_Errors then
                     return Empty;
                  else
                     Err := Make_Temporary (Sloc (N), 'T');
                     Set_Defining_Unit_Name (N, Err);

                     return Err;
                  end if;

               --  If not an entity, get defining identifier

               else
                  return Defining_Identifier (Nam);
               end if;
            end;

         when N_Block_Statement
            | N_Loop_Statement
         =>
            return Entity (Identifier (N));

         when others =>
            if Empty_On_Errors then
               return Empty;
            else
               raise Program_Error;
            end if;
      end case;
   end Defining_Entity;

   --------------------------
   -- Denotes_Discriminant --
   --------------------------

   function Denotes_Discriminant
     (N                : Node_Id;
      Check_Concurrent : Boolean := False) return Boolean
   is
      E : Entity_Id;

   begin
      if not Is_Entity_Name (N) or else No (Entity (N)) then
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
          (Check_Concurrent
            and then Ekind (E) = E_In_Parameter
            and then Present (Discriminal_Link (E))
            and then
              (Is_Concurrent_Type (Scope (Discriminal_Link (E)))
                or else
                  Is_Concurrent_Record_Type (Scope (Discriminal_Link (E)))));
   end Denotes_Discriminant;

   -------------------------
   -- Denotes_Same_Object --
   -------------------------

   function Denotes_Same_Object (A1, A2 : Node_Id) return Boolean is
      Obj1 : Node_Id := A1;
      Obj2 : Node_Id := A2;

      function Has_Prefix (N : Node_Id) return Boolean;
      --  Return True if N has attribute Prefix

      function Is_Renaming (N : Node_Id) return Boolean;
      --  Return true if N names a renaming entity

      function Is_Valid_Renaming (N : Node_Id) return Boolean;
      --  For renamings, return False if the prefix of any dereference within
      --  the renamed object_name is a variable, or any expression within the
      --  renamed object_name contains references to variables or calls on
      --  nonstatic functions; otherwise return True (RM 6.4.1(6.10/3))

      ----------------
      -- Has_Prefix --
      ----------------

      function Has_Prefix (N : Node_Id) return Boolean is
      begin
         return
           Nkind_In (N,
             N_Attribute_Reference,
             N_Expanded_Name,
             N_Explicit_Dereference,
             N_Indexed_Component,
             N_Reference,
             N_Selected_Component,
             N_Slice);
      end Has_Prefix;

      -----------------
      -- Is_Renaming --
      -----------------

      function Is_Renaming (N : Node_Id) return Boolean is
      begin
         return Is_Entity_Name (N)
           and then Present (Renamed_Entity (Entity (N)));
      end Is_Renaming;

      -----------------------
      -- Is_Valid_Renaming --
      -----------------------

      function Is_Valid_Renaming (N : Node_Id) return Boolean is

         function Check_Renaming (N : Node_Id) return Boolean;
         --  Recursive function used to traverse all the prefixes of N

         function Check_Renaming (N : Node_Id) return Boolean is
         begin
            if Is_Renaming (N)
              and then not Check_Renaming (Renamed_Entity (Entity (N)))
            then
               return False;
            end if;

            if Nkind (N) = N_Indexed_Component then
               declare
                  Indx : Node_Id;

               begin
                  Indx := First (Expressions (N));
                  while Present (Indx) loop
                     if not Is_OK_Static_Expression (Indx) then
                        return False;
                     end if;

                     Next_Index (Indx);
                  end loop;
               end;
            end if;

            if Has_Prefix (N) then
               declare
                  P : constant Node_Id := Prefix (N);

               begin
                  if Nkind (N) = N_Explicit_Dereference
                    and then Is_Variable (P)
                  then
                     return False;

                  elsif Is_Entity_Name (P)
                    and then Ekind (Entity (P)) = E_Function
                  then
                     return False;

                  elsif Nkind (P) = N_Function_Call then
                     return False;
                  end if;

                  --  Recursion to continue traversing the prefix of the
                  --  renaming expression

                  return Check_Renaming (P);
               end;
            end if;

            return True;
         end Check_Renaming;

      --  Start of processing for Is_Valid_Renaming

      begin
         return Check_Renaming (N);
      end Is_Valid_Renaming;

   --  Start of processing for Denotes_Same_Object

   begin
      --  Both names statically denote the same stand-alone object or parameter
      --  (RM 6.4.1(6.5/3))

      if Is_Entity_Name (Obj1)
        and then Is_Entity_Name (Obj2)
        and then Entity (Obj1) = Entity (Obj2)
      then
         return True;
      end if;

      --  For renamings, the prefix of any dereference within the renamed
      --  object_name is not a variable, and any expression within the
      --  renamed object_name contains no references to variables nor
      --  calls on nonstatic functions (RM 6.4.1(6.10/3)).

      if Is_Renaming (Obj1) then
         if Is_Valid_Renaming (Obj1) then
            Obj1 := Renamed_Entity (Entity (Obj1));
         else
            return False;
         end if;
      end if;

      if Is_Renaming (Obj2) then
         if Is_Valid_Renaming (Obj2) then
            Obj2 := Renamed_Entity (Entity (Obj2));
         else
            return False;
         end if;
      end if;

      --  No match if not same node kind (such cases are handled by
      --  Denotes_Same_Prefix)

      if Nkind (Obj1) /= Nkind (Obj2) then
         return False;

      --  After handling valid renamings, one of the two names statically
      --  denoted a renaming declaration whose renamed object_name is known
      --  to denote the same object as the other (RM 6.4.1(6.10/3))

      elsif Is_Entity_Name (Obj1) then
         if Is_Entity_Name (Obj2) then
            return Entity (Obj1) = Entity (Obj2);
         else
            return False;
         end if;

      --  Both names are selected_components, their prefixes are known to
      --  denote the same object, and their selector_names denote the same
      --  component (RM 6.4.1(6.6/3)).

      elsif Nkind (Obj1) = N_Selected_Component then
         return Denotes_Same_Object (Prefix (Obj1), Prefix (Obj2))
           and then
             Entity (Selector_Name (Obj1)) = Entity (Selector_Name (Obj2));

      --  Both names are dereferences and the dereferenced names are known to
      --  denote the same object (RM 6.4.1(6.7/3))

      elsif Nkind (Obj1) = N_Explicit_Dereference then
         return Denotes_Same_Object (Prefix (Obj1), Prefix (Obj2));

      --  Both names are indexed_components, their prefixes are known to denote
      --  the same object, and each of the pairs of corresponding index values
      --  are either both static expressions with the same static value or both
      --  names that are known to denote the same object (RM 6.4.1(6.8/3))

      elsif Nkind (Obj1) = N_Indexed_Component then
         if not Denotes_Same_Object (Prefix (Obj1), Prefix (Obj2)) then
            return False;
         else
            declare
               Indx1 : Node_Id;
               Indx2 : Node_Id;

            begin
               Indx1 := First (Expressions (Obj1));
               Indx2 := First (Expressions (Obj2));
               while Present (Indx1) loop

                  --  Indexes must denote the same static value or same object

                  if Is_OK_Static_Expression (Indx1) then
                     if not Is_OK_Static_Expression (Indx2) then
                        return False;

                     elsif Expr_Value (Indx1) /= Expr_Value (Indx2) then
                        return False;
                     end if;

                  elsif not Denotes_Same_Object (Indx1, Indx2) then
                     return False;
                  end if;

                  Next (Indx1);
                  Next (Indx2);
               end loop;

               return True;
            end;
         end if;

      --  Both names are slices, their prefixes are known to denote the same
      --  object, and the two slices have statically matching index constraints
      --  (RM 6.4.1(6.9/3))

      elsif Nkind (Obj1) = N_Slice
        and then Denotes_Same_Object (Prefix (Obj1), Prefix (Obj2))
      then
         declare
            Lo1, Lo2, Hi1, Hi2 : Node_Id;

         begin
            Get_Index_Bounds (Etype (Obj1), Lo1, Hi1);
            Get_Index_Bounds (Etype (Obj2), Lo2, Hi2);

            --  Check whether bounds are statically identical. There is no
            --  attempt to detect partial overlap of slices.

            return Denotes_Same_Object (Lo1, Lo2)
                     and then
                   Denotes_Same_Object (Hi1, Hi2);
         end;

      --  In the recursion, literals appear as indexes

      elsif Nkind (Obj1) = N_Integer_Literal
              and then
            Nkind (Obj2) = N_Integer_Literal
      then
         return Intval (Obj1) = Intval (Obj2);

      else
         return False;
      end if;
   end Denotes_Same_Object;

   -------------------------
   -- Denotes_Same_Prefix --
   -------------------------

   function Denotes_Same_Prefix (A1, A2 : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (A1) then
         if Nkind_In (A2, N_Selected_Component, N_Indexed_Component)
           and then not Is_Access_Type (Etype (A1))
         then
            return Denotes_Same_Object (A1, Prefix (A2))
              or else Denotes_Same_Prefix (A1, Prefix (A2));
         else
            return False;
         end if;

      elsif Is_Entity_Name (A2) then
         return Denotes_Same_Prefix (A1 => A2, A2 => A1);

      elsif Nkind_In (A1, N_Selected_Component, N_Indexed_Component, N_Slice)
              and then
            Nkind_In (A2, N_Selected_Component, N_Indexed_Component, N_Slice)
      then
         declare
            Root1, Root2   : Node_Id;
            Depth1, Depth2 : Nat := 0;

         begin
            Root1 := Prefix (A1);
            while not Is_Entity_Name (Root1) loop
               if not Nkind_In
                 (Root1, N_Selected_Component, N_Indexed_Component)
               then
                  return False;
               else
                  Root1 := Prefix (Root1);
               end if;

               Depth1 := Depth1 + 1;
            end loop;

            Root2 := Prefix (A2);
            while not Is_Entity_Name (Root2) loop
               if not Nkind_In (Root2, N_Selected_Component,
                                       N_Indexed_Component)
               then
                  return False;
               else
                  Root2 := Prefix (Root2);
               end if;

               Depth2 := Depth2 + 1;
            end loop;

            --  If both have the same depth and they do not denote the same
            --  object, they are disjoint and no warning is needed.

            if Depth1 = Depth2 then
               return False;

            elsif Depth1 > Depth2 then
               Root1 := Prefix (A1);
               for J in 1 .. Depth1 - Depth2 - 1 loop
                  Root1 := Prefix (Root1);
               end loop;

               return Denotes_Same_Object (Root1, A2);

            else
               Root2 := Prefix (A2);
               for J in 1 .. Depth2 - Depth1 - 1 loop
                  Root2 := Prefix (Root2);
               end loop;

               return Denotes_Same_Object (A1, Root2);
            end if;
         end;

      else
         return False;
      end if;
   end Denotes_Same_Prefix;

   ----------------------
   -- Denotes_Variable --
   ----------------------

   function Denotes_Variable (N : Node_Id) return Boolean is
   begin
      return Is_Variable (N) and then Paren_Count (N) = 0;
   end Denotes_Variable;

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

   --  Start of processing for Designate_Same_Unit

   begin
      if Nkind_In (K1, N_Identifier, N_Defining_Identifier)
           and then
         Nkind_In (K2, N_Identifier, N_Defining_Identifier)
      then
         return Chars (Name1) = Chars (Name2);

      elsif Nkind_In (K1, N_Expanded_Name,
                          N_Selected_Component,
                          N_Defining_Program_Unit_Name)
              and then
            Nkind_In (K2, N_Expanded_Name,
                          N_Selected_Component,
                          N_Defining_Program_Unit_Name)
      then
         return
           (Chars (Select_Node (Name1)) = Chars (Select_Node (Name2)))
             and then
               Designate_Same_Unit (Prefix_Node (Name1), Prefix_Node (Name2));

      else
         return False;
      end if;
   end Designate_Same_Unit;

   ---------------------------------------------
   -- Diagnose_Iterated_Component_Association --
   ---------------------------------------------

   procedure Diagnose_Iterated_Component_Association (N : Node_Id) is
      Def_Id : constant Entity_Id := Defining_Identifier (N);
      Aggr   : Node_Id;

   begin
      --  Determine whether the iterated component association appears within
      --  an aggregate. If this is the case, raise Program_Error because the
      --  iterated component association cannot be left in the tree as is and
      --  must always be processed by the related aggregate.

      Aggr := N;
      while Present (Aggr) loop
         if Nkind (Aggr) = N_Aggregate then
            raise Program_Error;

         --  Prevent the search from going too far

         elsif Is_Body_Or_Package_Declaration (Aggr) then
            exit;
         end if;

         Aggr := Parent (Aggr);
      end loop;

      --  At this point it is known that the iterated component association is
      --  not within an aggregate. This is really a quantified expression with
      --  a missing "all" or "some" quantifier.

      Error_Msg_N ("missing quantifier", Def_Id);

      --  Rewrite the iterated component association as True to prevent any
      --  cascaded errors.

      Rewrite (N, New_Occurrence_Of (Standard_True, Sloc (N)));
      Analyze (N);
   end Diagnose_Iterated_Component_Association;

   ---------------------------------
   -- Dynamic_Accessibility_Level --
   ---------------------------------

   function Dynamic_Accessibility_Level (Expr : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (Expr);

      function Make_Level_Literal (Level : Uint) return Node_Id;
      --  Construct an integer literal representing an accessibility level
      --  with its type set to Natural.

      ------------------------
      -- Make_Level_Literal --
      ------------------------

      function Make_Level_Literal (Level : Uint) return Node_Id is
         Result : constant Node_Id := Make_Integer_Literal (Loc, Level);

      begin
         Set_Etype (Result, Standard_Natural);
         return Result;
      end Make_Level_Literal;

      --  Local variables

      E : Entity_Id;

   --  Start of processing for Dynamic_Accessibility_Level

   begin
      if Is_Entity_Name (Expr) then
         E := Entity (Expr);

         if Present (Renamed_Object (E)) then
            return Dynamic_Accessibility_Level (Renamed_Object (E));
         end if;

         if Is_Formal (E) or else Ekind_In (E, E_Variable, E_Constant) then
            if Present (Extra_Accessibility (E)) then
               return New_Occurrence_Of (Extra_Accessibility (E), Loc);
            end if;
         end if;
      end if;

      --  Unimplemented: Ptr.all'Access, where Ptr has Extra_Accessibility ???

      case Nkind (Expr) is

         --  For access discriminant, the level of the enclosing object

         when N_Selected_Component =>
            if Ekind (Entity (Selector_Name (Expr))) = E_Discriminant
              and then Ekind (Etype (Entity (Selector_Name (Expr)))) =
                                            E_Anonymous_Access_Type
            then
               return Make_Level_Literal (Object_Access_Level (Expr));
            end if;

         when N_Attribute_Reference =>
            case Get_Attribute_Id (Attribute_Name (Expr)) is

               --  For X'Access, the level of the prefix X

               when Attribute_Access =>
                  return Make_Level_Literal
                           (Object_Access_Level (Prefix (Expr)));

               --  Treat the unchecked attributes as library-level

               when Attribute_Unchecked_Access
                  | Attribute_Unrestricted_Access
               =>
                  return Make_Level_Literal (Scope_Depth (Standard_Standard));

               --  No other access-valued attributes

               when others =>
                  raise Program_Error;
            end case;

         when N_Allocator =>

            --  Unimplemented: depends on context. As an actual parameter where
            --  formal type is anonymous, use
            --    Scope_Depth (Current_Scope) + 1.
            --  For other cases, see 3.10.2(14/3) and following. ???

            null;

         when N_Type_Conversion =>
            if not Is_Local_Anonymous_Access (Etype (Expr)) then

               --  Handle type conversions introduced for a rename of an
               --  Ada 2012 stand-alone object of an anonymous access type.

               return Dynamic_Accessibility_Level (Expression (Expr));
            end if;

         when others =>
            null;
      end case;

      return Make_Level_Literal (Type_Access_Level (Etype (Expr)));
   end Dynamic_Accessibility_Level;

   ------------------------
   -- Discriminated_Size --
   ------------------------

   function Discriminated_Size (Comp : Entity_Id) return Boolean is
      function Non_Static_Bound (Bound : Node_Id) return Boolean;
      --  Check whether the bound of an index is non-static and does denote
      --  a discriminant, in which case any object of the type (protected or
      --  otherwise) will have a non-static size.

      ----------------------
      -- Non_Static_Bound --
      ----------------------

      function Non_Static_Bound (Bound : Node_Id) return Boolean is
      begin
         if Is_OK_Static_Expression (Bound) then
            return False;

         --  If the bound is given by a discriminant it is non-static
         --  (A static constraint replaces the reference with the value).
         --  In an protected object the discriminant has been replaced by
         --  the corresponding discriminal within the protected operation.

         elsif Is_Entity_Name (Bound)
           and then
             (Ekind (Entity (Bound)) = E_Discriminant
               or else Present (Discriminal_Link (Entity (Bound))))
         then
            return False;

         else
            return True;
         end if;
      end Non_Static_Bound;

      --  Local variables

      Typ   : constant Entity_Id := Etype (Comp);
      Index : Node_Id;

   --  Start of processing for Discriminated_Size

   begin
      if not Is_Array_Type (Typ) then
         return False;
      end if;

      if Ekind (Typ) = E_Array_Subtype then
         Index := First_Index (Typ);
         while Present (Index) loop
            if Non_Static_Bound (Low_Bound (Index))
              or else Non_Static_Bound (High_Bound (Index))
            then
               return False;
            end if;

            Next_Index (Index);
         end loop;

         return True;
      end if;

      return False;
   end Discriminated_Size;

   -----------------------------------
   -- Effective_Extra_Accessibility --
   -----------------------------------

   function Effective_Extra_Accessibility (Id : Entity_Id) return Entity_Id is
   begin
      if Present (Renamed_Object (Id))
        and then Is_Entity_Name (Renamed_Object (Id))
      then
         return Effective_Extra_Accessibility (Entity (Renamed_Object (Id)));
      else
         return Extra_Accessibility (Id);
      end if;
   end Effective_Extra_Accessibility;

   -----------------------------
   -- Effective_Reads_Enabled --
   -----------------------------

   function Effective_Reads_Enabled (Id : Entity_Id) return Boolean is
   begin
      return Has_Enabled_Property (Id, Name_Effective_Reads);
   end Effective_Reads_Enabled;

   ------------------------------
   -- Effective_Writes_Enabled --
   ------------------------------

   function Effective_Writes_Enabled (Id : Entity_Id) return Boolean is
   begin
      return Has_Enabled_Property (Id, Name_Effective_Writes);
   end Effective_Writes_Enabled;

   ------------------------------
   -- Enclosing_Comp_Unit_Node --
   ------------------------------

   function Enclosing_Comp_Unit_Node (N : Node_Id) return Node_Id is
      Current_Node : Node_Id;

   begin
      Current_Node := N;
      while Present (Current_Node)
        and then Nkind (Current_Node) /= N_Compilation_Unit
      loop
         Current_Node := Parent (Current_Node);
      end loop;

      if Nkind (Current_Node) /= N_Compilation_Unit then
         return Empty;
      else
         return Current_Node;
      end if;
   end Enclosing_Comp_Unit_Node;

   --------------------------
   -- Enclosing_CPP_Parent --
   --------------------------

   function Enclosing_CPP_Parent (Typ : Entity_Id) return Entity_Id is
      Parent_Typ : Entity_Id := Typ;

   begin
      while not Is_CPP_Class (Parent_Typ)
         and then Etype (Parent_Typ) /= Parent_Typ
      loop
         Parent_Typ := Etype (Parent_Typ);

         if Is_Private_Type (Parent_Typ) then
            Parent_Typ := Full_View (Base_Type (Parent_Typ));
         end if;
      end loop;

      pragma Assert (Is_CPP_Class (Parent_Typ));
      return Parent_Typ;
   end Enclosing_CPP_Parent;

   ---------------------------
   -- Enclosing_Declaration --
   ---------------------------

   function Enclosing_Declaration (N : Node_Id) return Node_Id is
      Decl : Node_Id := N;

   begin
      while Present (Decl)
        and then not (Nkind (Decl) in N_Declaration
                        or else
                      Nkind (Decl) in N_Later_Decl_Item)
      loop
         Decl := Parent (Decl);
      end loop;

      return Decl;
   end Enclosing_Declaration;

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

   function Enclosing_Lib_Unit_Entity
      (E : Entity_Id := Current_Scope) return Entity_Id
   is
      Unit_Entity : Entity_Id;

   begin
      --  Look for enclosing library unit entity by following scope links.
      --  Equivalent to, but faster than indexing through the scope stack.

      Unit_Entity := E;
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
      Encl_Unit : Node_Id;

   begin
      Encl_Unit := Enclosing_Comp_Unit_Node (N);
      while Present (Encl_Unit)
        and then Nkind (Unit (Encl_Unit)) = N_Subunit
      loop
         Encl_Unit := Library_Unit (Encl_Unit);
      end loop;

      pragma Assert (Nkind (Encl_Unit) = N_Compilation_Unit);
      return Encl_Unit;
   end Enclosing_Lib_Unit_Node;

   -----------------------
   -- Enclosing_Package --
   -----------------------

   function Enclosing_Package (E : Entity_Id) return Entity_Id is
      Dynamic_Scope : constant Entity_Id := Enclosing_Dynamic_Scope (E);

   begin
      if Dynamic_Scope = Standard_Standard then
         return Standard_Standard;

      elsif Dynamic_Scope = Empty then
         return Empty;

      elsif Ekind_In (Dynamic_Scope, E_Package, E_Package_Body,
                      E_Generic_Package)
      then
         return Dynamic_Scope;

      else
         return Enclosing_Package (Dynamic_Scope);
      end if;
   end Enclosing_Package;

   -------------------------------------
   -- Enclosing_Package_Or_Subprogram --
   -------------------------------------

   function Enclosing_Package_Or_Subprogram (E : Entity_Id) return Entity_Id is
      S : Entity_Id;

   begin
      S := Scope (E);
      while Present (S) loop
         if Is_Package_Or_Generic_Package (S)
           or else Ekind (S) = E_Package_Body
         then
            return S;

         elsif Is_Subprogram_Or_Generic_Subprogram (S)
           or else Ekind (S) = E_Subprogram_Body
         then
            return S;

         else
            S := Scope (S);
         end if;
      end loop;

      return Empty;
   end Enclosing_Package_Or_Subprogram;

   --------------------------
   -- Enclosing_Subprogram --
   --------------------------

   function Enclosing_Subprogram (E : Entity_Id) return Entity_Id is
      Dynamic_Scope : constant Entity_Id := Enclosing_Dynamic_Scope (E);

   begin
      if Dynamic_Scope = Standard_Standard then
         return Empty;

      elsif Dynamic_Scope = Empty then
         return Empty;

      elsif Ekind (Dynamic_Scope) = E_Subprogram_Body then
         return Corresponding_Spec (Parent (Parent (Dynamic_Scope)));

      elsif Ekind (Dynamic_Scope) = E_Block
        or else Ekind (Dynamic_Scope) = E_Return_Statement
      then
         return Enclosing_Subprogram (Dynamic_Scope);

      elsif Ekind (Dynamic_Scope) = E_Task_Type then
         return Get_Task_Body_Procedure (Dynamic_Scope);

      elsif Ekind (Dynamic_Scope) = E_Limited_Private_Type
        and then Present (Full_View (Dynamic_Scope))
        and then Ekind (Full_View (Dynamic_Scope)) = E_Task_Type
      then
         return Get_Task_Body_Procedure (Full_View (Dynamic_Scope));

      --  No body is generated if the protected operation is eliminated

      elsif Convention (Dynamic_Scope) = Convention_Protected
        and then not Is_Eliminated (Dynamic_Scope)
        and then Present (Protected_Body_Subprogram (Dynamic_Scope))
      then
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
           and then Ekind (Def_Id) = E_Record_Type
           and then Present (Corresponding_Remote_Type (Def_Id))
         then
            null;

         --  Case of an implicit operation or derived literal. The new entity
         --  hides the implicit one,  which is removed from all visibility,
         --  i.e. the entity list of its scope, and homonym chain of its name.

         elsif (Is_Overloadable (E) and then Is_Inherited_Operation (E))
           or else Is_Internal (E)
         then
            declare
               Decl     : constant Node_Id := Parent (E);
               Prev     : Entity_Id;
               Prev_Vis : Entity_Id;

            begin
               --  If E is an implicit declaration, it cannot be the first
               --  entity in the scope.

               Prev := First_Entity (Current_Scope);
               while Present (Prev) and then Next_Entity (Prev) /= E loop
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

                  if Present (Prev_Vis) then

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

         --  If the homograph is a protected component renaming, it should not
         --  be hiding the current entity. Such renamings are treated as weak
         --  declarations.

         elsif Is_Prival (E) then
            Set_Is_Immediately_Visible (E, False);

         --  In this case the current entity is a protected component renaming.
         --  Perform minimal decoration by setting the scope and return since
         --  the prival should not be hiding other visible entities.

         elsif Is_Prival (Def_Id) then
            Set_Scope (Def_Id, Current_Scope);
            return;

         --  Analogous to privals, the discriminal generated for an entry index
         --  parameter acts as a weak declaration. Perform minimal decoration
         --  to avoid bogus errors.

         elsif Is_Discriminal (Def_Id)
           and then Ekind (Discriminal_Link (Def_Id)) = E_Entry_Index_Parameter
         then
            Set_Scope (Def_Id, Current_Scope);
            return;

         --  In the body or private part of an instance, a type extension may
         --  introduce a component with the same name as that of an actual. The
         --  legality rule is not enforced, but the semantics of the full type
         --  with two components of same name are not clear at this point???

         elsif In_Instance_Not_Visible then
            null;

         --  When compiling a package body, some child units may have become
         --  visible. They cannot conflict with local entities that hide them.

         elsif Is_Child_Unit (E)
           and then In_Open_Scopes (Scope (E))
           and then not Is_Immediately_Visible (E)
         then
            null;

         --  Conversely, with front-end inlining we may compile the parent body
         --  first, and a child unit subsequently. The context is now the
         --  parent spec, and body entities are not visible.

         elsif Is_Child_Unit (Def_Id)
           and then Is_Package_Body_Entity (E)
           and then not In_Package_Body (Current_Scope)
         then
            null;

         --  Case of genuine duplicate declaration

         else
            Error_Msg_Sloc := Sloc (E);

            --  If the previous declaration is an incomplete type declaration
            --  this may be an attempt to complete it with a private type. The
            --  following avoids confusing cascaded errors.

            if Nkind (Parent (E)) = N_Incomplete_Type_Declaration
              and then Nkind (Parent (Def_Id)) = N_Private_Type_Declaration
            then
               Error_Msg_N
                 ("incomplete type cannot be completed with a private " &
                  "declaration", Parent (Def_Id));
               Set_Is_Immediately_Visible (E, False);
               Set_Full_View (E, Def_Id);

            --  An inherited component of a record conflicts with a new
            --  discriminant. The discriminant is inserted first in the scope,
            --  but the error should be posted on it, not on the component.

            elsif Ekind (E) = E_Discriminant
              and then Present (Scope (Def_Id))
              and then Scope (Def_Id) /= Current_Scope
            then
               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Msg_N ("& conflicts with declaration#", E);
               return;

            --  If the name of the unit appears in its own context clause, a
            --  dummy package with the name has already been created, and the
            --  error emitted. Try to continue quietly.

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

               if Ekind_In (E, E_Component, E_Discriminant) then
                  return;
               end if;
            end if;

            if Nkind (Parent (Parent (Def_Id))) =
                                             N_Generic_Subprogram_Declaration
              and then Def_Id =
                Defining_Entity (Specification (Parent (Parent (Def_Id))))
            then
               Error_Msg_N ("\generic units cannot be overloaded", Def_Id);
            end if;

            --  If entity is in standard, then we are in trouble, because it
            --  means that we have a library package with a duplicated name.
            --  That's hard to recover from, so abort.

            if S = Standard_Standard then
               raise Unrecoverable_Error;

            --  Otherwise we continue with the declaration. Having two
            --  identical declarations should not cause us too much trouble.

            else
               null;
            end if;
         end if;
      end if;

      --  If we fall through, declaration is OK, at least OK enough to continue

      --  If Def_Id is a discriminant or a record component we are in the midst
      --  of inheriting components in a derived record definition. Preserve
      --  their Ekind and Etype.

      if Ekind_In (Def_Id, E_Discriminant, E_Component) then
         null;

      --  If a type is already set, leave it alone (happens when a type
      --  declaration is reanalyzed following a call to the optimizer).

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

      --  Unless the Itype is for a record type with a corresponding remote
      --  type (what is that about, it was not commented ???)

      if Ekind_In (Def_Id, E_Discriminant, E_Component)
        or else
          ((not Is_Record_Type (Def_Id)
             or else No (Corresponding_Remote_Type (Def_Id)))
            and then not Is_Itype (Def_Id))
      then
         Set_Is_Immediately_Visible (Def_Id);
         Set_Current_Entity         (Def_Id);
      end if;

      Set_Homonym       (Def_Id, C);
      Append_Entity     (Def_Id, S);
      Set_Public_Status (Def_Id);

      --  Declaring a homonym is not allowed in SPARK ...

      if Present (C) and then Restriction_Check_Required (SPARK_05) then
         declare
            Enclosing_Subp : constant Node_Id := Enclosing_Subprogram (Def_Id);
            Enclosing_Pack : constant Node_Id := Enclosing_Package (Def_Id);
            Other_Scope    : constant Node_Id := Enclosing_Dynamic_Scope (C);

         begin
            --  ... unless the new declaration is in a subprogram, and the
            --  visible declaration is a variable declaration or a parameter
            --  specification outside that subprogram.

            if Present (Enclosing_Subp)
              and then Nkind_In (Parent (C), N_Object_Declaration,
                                             N_Parameter_Specification)
              and then not Scope_Within_Or_Same (Other_Scope, Enclosing_Subp)
            then
               null;

            --  ... or the new declaration is in a package, and the visible
            --  declaration occurs outside that package.

            elsif Present (Enclosing_Pack)
              and then not Scope_Within_Or_Same (Other_Scope, Enclosing_Pack)
            then
               null;

            --  ... or the new declaration is a component declaration in a
            --  record type definition.

            elsif Nkind (Parent (Def_Id)) = N_Component_Declaration then
               null;

            --  Don't issue error for non-source entities

            elsif Comes_From_Source (Def_Id)
              and then Comes_From_Source (C)
            then
               Error_Msg_Sloc := Sloc (C);
               Check_SPARK_05_Restriction
                 ("redeclaration of identifier &#", Def_Id);
            end if;
         end;
      end if;

      --  Warn if new entity hides an old one

      if Warn_On_Hiding and then Present (C)

        --  Don't warn for record components since they always have a well
        --  defined scope which does not confuse other uses. Note that in
        --  some cases, Ekind has not been set yet.

        and then Ekind (C) /= E_Component
        and then Ekind (C) /= E_Discriminant
        and then Nkind (Parent (C)) /= N_Component_Declaration
        and then Ekind (Def_Id) /= E_Component
        and then Ekind (Def_Id) /= E_Discriminant
        and then Nkind (Parent (Def_Id)) /= N_Component_Declaration

        --  Don't warn for one character variables. It is too common to use
        --  such variables as locals and will just cause too many false hits.

        and then Length_Of_Name (Chars (C)) /= 1

        --  Don't warn for non-source entities

        and then Comes_From_Source (C)
        and then Comes_From_Source (Def_Id)

        --  Don't warn unless entity in question is in extended main source

        and then In_Extended_Main_Source_Unit (Def_Id)

        --  Finally, the hidden entity must be either immediately visible or
        --  use visible (i.e. from a used package).

        and then
          (Is_Immediately_Visible (C)
             or else
           Is_Potentially_Use_Visible (C))
      then
         Error_Msg_Sloc := Sloc (C);
         Error_Msg_N ("declaration hides &#?h?", Def_Id);
      end if;
   end Enter_Name;

   ---------------
   -- Entity_Of --
   ---------------

   function Entity_Of (N : Node_Id) return Entity_Id is
      Id : Entity_Id;

   begin
      Id := Empty;

      if Is_Entity_Name (N) then
         Id := Entity (N);

         --  Follow a possible chain of renamings to reach the root renamed
         --  object.

         while Present (Id)
           and then Is_Object (Id)
           and then Present (Renamed_Object (Id))
         loop
            if Is_Entity_Name (Renamed_Object (Id)) then
               Id := Entity (Renamed_Object (Id));
            else
               Id := Empty;
               exit;
            end if;
         end loop;
      end if;

      return Id;
   end Entity_Of;

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
           ("\component type& of type& is limited", N, Component_Type (T));
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

   ---------------------------------------
   -- Expression_Of_Expression_Function --
   ---------------------------------------

   function Expression_Of_Expression_Function
     (Subp : Entity_Id) return Node_Id
   is
      Expr_Func : Node_Id;

   begin
      pragma Assert (Is_Expression_Function_Or_Completion (Subp));

      if Nkind (Original_Node (Subprogram_Spec (Subp))) =
           N_Expression_Function
      then
         Expr_Func := Original_Node (Subprogram_Spec (Subp));

      elsif Nkind (Original_Node (Subprogram_Body (Subp))) =
              N_Expression_Function
      then
         Expr_Func := Original_Node (Subprogram_Body (Subp));

      else
         pragma Assert (False);
         null;
      end if;

      return Original_Node (Expression (Expr_Func));
   end Expression_Of_Expression_Function;

   -------------------------------
   -- Extensions_Visible_Status --
   -------------------------------

   function Extensions_Visible_Status
     (Id : Entity_Id) return Extensions_Visible_Mode
   is
      Arg  : Node_Id;
      Decl : Node_Id;
      Expr : Node_Id;
      Prag : Node_Id;
      Subp : Entity_Id;

   begin
      --  When a formal parameter is subject to Extensions_Visible, the pragma
      --  is stored in the contract of related subprogram.

      if Is_Formal (Id) then
         Subp := Scope (Id);

      elsif Is_Subprogram_Or_Generic_Subprogram (Id) then
         Subp := Id;

      --  No other construct carries this pragma

      else
         return Extensions_Visible_None;
      end if;

      Prag := Get_Pragma (Subp, Pragma_Extensions_Visible);

      --  In certain cases analysis may request the Extensions_Visible status
      --  of an expression function before the pragma has been analyzed yet.
      --  Inspect the declarative items after the expression function looking
      --  for the pragma (if any).

      if No (Prag) and then Is_Expression_Function (Subp) then
         Decl := Next (Unit_Declaration_Node (Subp));
         while Present (Decl) loop
            if Nkind (Decl) = N_Pragma
              and then Pragma_Name (Decl) = Name_Extensions_Visible
            then
               Prag := Decl;
               exit;

            --  A source construct ends the region where Extensions_Visible may
            --  appear, stop the traversal. An expanded expression function is
            --  no longer a source construct, but it must still be recognized.

            elsif Comes_From_Source (Decl)
              or else
                (Nkind_In (Decl, N_Subprogram_Body,
                                 N_Subprogram_Declaration)
                  and then Is_Expression_Function (Defining_Entity (Decl)))
            then
               exit;
            end if;

            Next (Decl);
         end loop;
      end if;

      --  Extract the value from the Boolean expression (if any)

      if Present (Prag) then
         Arg := First (Pragma_Argument_Associations (Prag));

         if Present (Arg) then
            Expr := Get_Pragma_Arg (Arg);

            --  When the associated subprogram is an expression function, the
            --  argument of the pragma may not have been analyzed.

            if not Analyzed (Expr) then
               Preanalyze_And_Resolve (Expr, Standard_Boolean);
            end if;

            --  Guard against cascading errors when the argument of pragma
            --  Extensions_Visible is not a valid static Boolean expression.

            if Error_Posted (Expr) then
               return Extensions_Visible_None;

            elsif Is_True (Expr_Value (Expr)) then
               return Extensions_Visible_True;

            else
               return Extensions_Visible_False;
            end if;

         --  Otherwise the aspect or pragma defaults to True

         else
            return Extensions_Visible_True;
         end if;

      --  Otherwise aspect or pragma Extensions_Visible is not inherited or
      --  directly specified. In SPARK code, its value defaults to "False".

      elsif SPARK_Mode = On then
         return Extensions_Visible_False;

      --  In non-SPARK code, aspect or pragma Extensions_Visible defaults to
      --  "True".

      else
         return Extensions_Visible_True;
      end if;
   end Extensions_Visible_Status;

   -----------------
   -- Find_Actual --
   -----------------

   procedure Find_Actual
     (N        : Node_Id;
      Formal   : out Entity_Id;
      Call     : out Node_Id)
   is
      Context  : constant Node_Id := Parent (N);
      Actual   : Node_Id;
      Call_Nam : Node_Id;

   begin
      if Nkind_In (Context, N_Indexed_Component, N_Selected_Component)
        and then N = Prefix (Context)
      then
         Find_Actual (Context, Formal, Call);
         return;

      elsif Nkind (Context) = N_Parameter_Association
        and then N = Explicit_Actual_Parameter (Context)
      then
         Call := Parent (Context);

      elsif Nkind_In (Context, N_Entry_Call_Statement,
                               N_Function_Call,
                               N_Procedure_Call_Statement)
      then
         Call := Context;

      else
         Formal := Empty;
         Call   := Empty;
         return;
      end if;

      --  If we have a call to a subprogram look for the parameter. Note that
      --  we exclude overloaded calls, since we don't know enough to be sure
      --  of giving the right answer in this case.

      if Nkind_In (Call, N_Entry_Call_Statement,
                         N_Function_Call,
                         N_Procedure_Call_Statement)
      then
         Call_Nam := Name (Call);

         --  A call to a protected or task entry appears as a selected
         --  component rather than an expanded name.

         if Nkind (Call_Nam) = N_Selected_Component then
            Call_Nam := Selector_Name (Call_Nam);
         end if;

         if Is_Entity_Name (Call_Nam)
           and then Present (Entity (Call_Nam))
           and then Is_Overloadable (Entity (Call_Nam))
           and then not Is_Overloaded (Call_Nam)
         then
            --  If node is name in call it is not an actual

            if N = Call_Nam then
               Formal := Empty;
               Call   := Empty;
               return;
            end if;

            --  Fall here if we are definitely a parameter

            Actual := First_Actual (Call);
            Formal := First_Formal (Entity (Call_Nam));
            while Present (Formal) and then Present (Actual) loop
               if Actual = N then
                  return;

               --  An actual that is the prefix in a prefixed call may have
               --  been rewritten in the call, after the deferred reference
               --  was collected. Check if sloc and kinds and names match.

               elsif Sloc (Actual) = Sloc (N)
                 and then Nkind (Actual) = N_Identifier
                 and then Nkind (Actual) = Nkind (N)
                 and then Chars (Actual) = Chars (N)
               then
                  return;

               else
                  Actual := Next_Actual (Actual);
                  Formal := Next_Formal (Formal);
               end if;
            end loop;
         end if;
      end if;

      --  Fall through here if we did not find matching actual

      Formal := Empty;
      Call   := Empty;
   end Find_Actual;

   ---------------------------
   -- Find_Body_Discriminal --
   ---------------------------

   function Find_Body_Discriminal
     (Spec_Discriminant : Entity_Id) return Entity_Id
   is
      Tsk  : Entity_Id;
      Disc : Entity_Id;

   begin
      --  If expansion is suppressed, then the scope can be the concurrent type
      --  itself rather than a corresponding concurrent record type.

      if Is_Concurrent_Type (Scope (Spec_Discriminant)) then
         Tsk := Scope (Spec_Discriminant);

      else
         pragma Assert (Is_Concurrent_Record_Type (Scope (Spec_Discriminant)));

         Tsk := Corresponding_Concurrent_Type (Scope (Spec_Discriminant));
      end if;

      --  Find discriminant of original concurrent type, and use its current
      --  discriminal, which is the renaming within the task/protected body.

      Disc := First_Discriminant (Tsk);
      while Present (Disc) loop
         if Chars (Disc) = Chars (Spec_Discriminant) then
            return Discriminal (Disc);
         end if;

         Next_Discriminant (Disc);
      end loop;

      --  That loop should always succeed in finding a matching entry and
      --  returning. Fatal error if not.

      raise Program_Error;
   end Find_Body_Discriminal;

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
         if Old_Disc = Par_Disc then
            return New_Disc;
         end if;

         Next_Discriminant (Old_Disc);
         Next_Discriminant (New_Disc);
      end loop;

      --  Should always find it

      raise Program_Error;
   end Find_Corresponding_Discriminant;

   ----------------------------------
   -- Find_Enclosing_Iterator_Loop --
   ----------------------------------

   function Find_Enclosing_Iterator_Loop (Id : Entity_Id) return Entity_Id is
      Constr : Node_Id;
      S      : Entity_Id;

   begin
      --  Traverse the scope chain looking for an iterator loop. Such loops are
      --  usually transformed into blocks, hence the use of Original_Node.

      S := Id;
      while Present (S) and then S /= Standard_Standard loop
         if Ekind (S) = E_Loop
           and then Nkind (Parent (S)) = N_Implicit_Label_Declaration
         then
            Constr := Original_Node (Label_Construct (Parent (S)));

            if Nkind (Constr) = N_Loop_Statement
              and then Present (Iteration_Scheme (Constr))
              and then Nkind (Iterator_Specification
                                (Iteration_Scheme (Constr))) =
                                                 N_Iterator_Specification
            then
               return S;
            end if;
         end if;

         S := Scope (S);
      end loop;

      return Empty;
   end Find_Enclosing_Iterator_Loop;

   ------------------------------------
   -- Find_Loop_In_Conditional_Block --
   ------------------------------------

   function Find_Loop_In_Conditional_Block (N : Node_Id) return Node_Id is
      Stmt : Node_Id;

   begin
      Stmt := N;

      if Nkind (Stmt) = N_If_Statement then
         Stmt := First (Then_Statements (Stmt));
      end if;

      pragma Assert (Nkind (Stmt) = N_Block_Statement);

      --  Inspect the statements of the conditional block. In general the loop
      --  should be the first statement in the statement sequence of the block,
      --  but the finalization machinery may have introduced extra object
      --  declarations.

      Stmt := First (Statements (Handled_Statement_Sequence (Stmt)));
      while Present (Stmt) loop
         if Nkind (Stmt) = N_Loop_Statement then
            return Stmt;
         end if;

         Next (Stmt);
      end loop;

      --  The expansion of attribute 'Loop_Entry produced a malformed block

      raise Program_Error;
   end Find_Loop_In_Conditional_Block;

   --------------------------
   -- Find_Overlaid_Entity --
   --------------------------

   procedure Find_Overlaid_Entity
     (N   : Node_Id;
      Ent : out Entity_Id;
      Off : out Boolean)
   is
      Expr : Node_Id;

   begin
      --  We are looking for one of the two following forms:

      --    for X'Address use Y'Address

      --  or

      --    Const : constant Address := expr;
      --    ...
      --    for X'Address use Const;

      --  In the second case, the expr is either Y'Address, or recursively a
      --  constant that eventually references Y'Address.

      Ent := Empty;
      Off := False;

      if Nkind (N) = N_Attribute_Definition_Clause
        and then Chars (N) = Name_Address
      then
         Expr := Expression (N);

         --  This loop checks the form of the expression for Y'Address,
         --  using recursion to deal with intermediate constants.

         loop
            --  Check for Y'Address

            if Nkind (Expr) = N_Attribute_Reference
              and then Attribute_Name (Expr) = Name_Address
            then
               Expr := Prefix (Expr);
               exit;

               --  Check for Const where Const is a constant entity

            elsif Is_Entity_Name (Expr)
              and then Ekind (Entity (Expr)) = E_Constant
            then
               Expr := Constant_Value (Entity (Expr));

            --  Anything else does not need checking

            else
               return;
            end if;
         end loop;

         --  This loop checks the form of the prefix for an entity, using
         --  recursion to deal with intermediate components.

         loop
            --  Check for Y where Y is an entity

            if Is_Entity_Name (Expr) then
               Ent := Entity (Expr);
               return;

            --  Check for components

            elsif
              Nkind_In (Expr, N_Selected_Component, N_Indexed_Component)
            then
               Expr := Prefix (Expr);
               Off := True;

            --  Anything else does not need checking

            else
               return;
            end if;
         end loop;
      end if;
   end Find_Overlaid_Entity;

   -------------------------
   -- Find_Parameter_Type --
   -------------------------

   function Find_Parameter_Type (Param : Node_Id) return Entity_Id is
   begin
      if Nkind (Param) /= N_Parameter_Specification then
         return Empty;

      --  For an access parameter, obtain the type from the formal entity
      --  itself, because access to subprogram nodes do not carry a type.
      --  Shouldn't we always use the formal entity ???

      elsif Nkind (Parameter_Type (Param)) = N_Access_Definition then
         return Etype (Defining_Identifier (Param));

      else
         return Etype (Parameter_Type (Param));
      end if;
   end Find_Parameter_Type;

   -----------------------------------
   -- Find_Placement_In_State_Space --
   -----------------------------------

   procedure Find_Placement_In_State_Space
     (Item_Id   : Entity_Id;
      Placement : out State_Space_Kind;
      Pack_Id   : out Entity_Id)
   is
      Context : Entity_Id;

   begin
      --  Assume that the item does not appear in the state space of a package

      Placement := Not_In_Package;
      Pack_Id   := Empty;

      --  Climb the scope stack and examine the enclosing context

      Context := Scope (Item_Id);
      while Present (Context) and then Context /= Standard_Standard loop
         if Ekind (Context) = E_Package then
            Pack_Id := Context;

            --  A package body is a cut off point for the traversal as the item
            --  cannot be visible to the outside from this point on. Note that
            --  this test must be done first as a body is also classified as a
            --  private part.

            if In_Package_Body (Context) then
               Placement := Body_State_Space;
               return;

            --  The private part of a package is a cut off point for the
            --  traversal as the item cannot be visible to the outside from
            --  this point on.

            elsif In_Private_Part (Context) then
               Placement := Private_State_Space;
               return;

            --  When the item appears in the visible state space of a package,
            --  continue to climb the scope stack as this may not be the final
            --  state space.

            else
               Placement := Visible_State_Space;

               --  The visible state space of a child unit acts as the proper
               --  placement of an item.

               if Is_Child_Unit (Context) then
                  return;
               end if;
            end if;

         --  The item or its enclosing package appear in a construct that has
         --  no state space.

         else
            Placement := Not_In_Package;
            return;
         end if;

         Context := Scope (Context);
      end loop;
   end Find_Placement_In_State_Space;

   ------------------------
   -- Find_Specific_Type --
   ------------------------

   function Find_Specific_Type (CW : Entity_Id) return Entity_Id is
      Typ : Entity_Id := Root_Type (CW);

   begin
      if Ekind (Typ) = E_Incomplete_Type then
         if From_Limited_With (Typ) then
            Typ := Non_Limited_View (Typ);
         else
            Typ := Full_View (Typ);
         end if;
      end if;

      if Is_Private_Type (Typ)
        and then not Is_Tagged_Type (Typ)
        and then Present (Full_View (Typ))
      then
         return Full_View (Typ);
      else
         return Typ;
      end if;
   end Find_Specific_Type;

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
                  exit Search when Is_In_Range (Expr, Etype (Choice),
                                                Assume_Valid => False);

               --  Choice is a subtype indication

               elsif Nkind (Choice) = N_Subtype_Indication then
                  declare
                     C : constant Node_Id := Constraint (Choice);
                     R : constant Node_Id := Range_Expression (C);

                  begin
                     exit Search when
                       Val >= Expr_Value (Low_Bound  (R))
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

      --  The above loop *must* terminate by finding a match, since we know the
      --  case statement is valid, and the value of the expression is known at
      --  compile time. When we fall out of the loop, Alt points to the
      --  alternative that we know will be selected at run time.

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

   -------------
   -- Fix_Msg --
   -------------

   function Fix_Msg (Id : Entity_Id; Msg : String) return String is
      Is_Task   : constant Boolean :=
                    Ekind_In (Id, E_Task_Body, E_Task_Type)
                      or else Is_Single_Task_Object (Id);
      Msg_Last  : constant Natural := Msg'Last;
      Msg_Index : Natural;
      Res       : String (Msg'Range) := (others => ' ');
      Res_Index : Natural;

   begin
      --  Copy all characters from the input message Msg to result Res with
      --  suitable replacements.

      Msg_Index := Msg'First;
      Res_Index := Res'First;
      while Msg_Index <= Msg_Last loop

         --  Replace "subprogram" with a different word

         if Msg_Index <= Msg_Last - 10
           and then Msg (Msg_Index .. Msg_Index + 9) = "subprogram"
         then
            if Ekind_In (Id, E_Entry, E_Entry_Family) then
               Res (Res_Index .. Res_Index + 4) := "entry";
               Res_Index := Res_Index + 5;

            elsif Is_Task then
               Res (Res_Index .. Res_Index + 8) := "task type";
               Res_Index := Res_Index + 9;

            else
               Res (Res_Index .. Res_Index + 9) := "subprogram";
               Res_Index := Res_Index + 10;
            end if;

            Msg_Index := Msg_Index + 10;

         --  Replace "protected" with a different word

         elsif Msg_Index <= Msg_Last - 9
           and then Msg (Msg_Index .. Msg_Index + 8) = "protected"
           and then Is_Task
         then
            Res (Res_Index .. Res_Index + 3) := "task";
            Res_Index := Res_Index + 4;
            Msg_Index := Msg_Index + 9;

         --  Otherwise copy the character

         else
            Res (Res_Index) := Msg (Msg_Index);
            Msg_Index := Msg_Index + 1;
            Res_Index := Res_Index + 1;
         end if;
      end loop;

      return Res (Res'First .. Res_Index - 1);
   end Fix_Msg;

   -------------------------
   -- From_Nested_Package --
   -------------------------

   function From_Nested_Package (T : Entity_Id) return Boolean is
      Pack : constant Entity_Id := Scope (T);

   begin
      return
        Ekind (Pack) = E_Package
          and then not Is_Frozen (Pack)
          and then not Scope_Within_Or_Same (Current_Scope, Pack)
          and then In_Open_Scopes (Scope (Pack));
   end From_Nested_Package;

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
               if not Is_Tag (Comp) and then Chars (Comp) /= Name_uParent then
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
                       Chars (Corresponding_Discriminant (Entity (Discrim))) =
                                                       Chars  (Discrim_Name))
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
                  while Present (D) and then Present (C) loop
                     if Chars (Discrim_Name) = Chars (D) then
                        if Is_Entity_Name (Node (C))
                          and then Entity (Node (C)) = Entity (Discrim)
                        then
                           --  D is renamed by Discrim, whose value is given in
                           --  Assoc.

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

                     Next_Discriminant (D);
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

         --  If the variant part is governed by a discriminant of the type
         --  this is an error. If the variant part and the discriminant are
         --  inherited from an ancestor this is legal (AI05-120) unless the
         --  components are being gathered for an aggregate, in which case
         --  the caller must check Report_Errors.

         if Scope (Original_Record_Component
                     ((Entity (First (Choices (Assoc)))))) = Typ
         then
            Error_Msg_FE
              ("value for discriminant & must be static!",
               Discrim_Value, Discrim);
            Why_Not_Static (Discrim_Value);
         end if;

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

      --  The case statement must include a variant that corresponds to the
      --  value of the discriminant, unless the discriminant type has a
      --  static predicate. In that case the absence of an others_choice that
      --  would cover this value becomes a run-time error (3.8,1 (21.1/2)).

      if No (Variant)
        and then not Has_Static_Predicate (Etype (Discrim_Name))
      then
         Error_Msg_NE
           ("value of discriminant & is out of range", Discrim_Value, Discrim);
         Report_Errors := True;
         return;
      end  if;

      --  If we have found the corresponding choice, recursively add its
      --  components to the Into list. The nested components are part of
      --  the same record type.

      if Present (Variant) then
         Gather_Components
           (Typ, Component_List (Variant), Governed_By, Into, Report_Errors);
      end if;
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
         --  Nothing to do if in spec expression (why not???)

         if In_Spec_Expression then
            return Typ;

         elsif Is_Private_Type (Typ) and then not Has_Discriminants (Typ) then

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

   ------------------------
   -- Get_Body_From_Stub --
   ------------------------

   function Get_Body_From_Stub (N : Node_Id) return Node_Id is
   begin
      return Proper_Body (Unit (Library_Unit (N)));
   end Get_Body_From_Stub;

   ---------------------
   -- Get_Cursor_Type --
   ---------------------

   function Get_Cursor_Type
     (Aspect : Node_Id;
      Typ    : Entity_Id) return Entity_Id
   is
      Assoc    : Node_Id;
      Func     : Entity_Id;
      First_Op : Entity_Id;
      Cursor   : Entity_Id;

   begin
      --  If error already detected, return

      if Error_Posted (Aspect) then
         return Any_Type;
      end if;

      --  The cursor type for an Iterable aspect is the return type of a
      --  non-overloaded First primitive operation. Locate association for
      --  First.

      Assoc := First (Component_Associations (Expression (Aspect)));
      First_Op  := Any_Id;
      while Present (Assoc) loop
         if Chars (First (Choices (Assoc))) = Name_First then
            First_Op := Expression (Assoc);
            exit;
         end if;

         Next (Assoc);
      end loop;

      if First_Op = Any_Id then
         Error_Msg_N ("aspect Iterable must specify First operation", Aspect);
         return Any_Type;
      end if;

      Cursor := Any_Type;

      --  Locate function with desired name and profile in scope of type
      --  In the rare case where the type is an integer type, a base type
      --  is created for it, check that the base type of the first formal
      --  of First matches the base type of the domain.

      Func := First_Entity (Scope (Typ));
      while Present (Func) loop
         if Chars (Func) = Chars (First_Op)
           and then Ekind (Func) = E_Function
           and then Present (First_Formal (Func))
           and then Base_Type (Etype (First_Formal (Func))) = Base_Type (Typ)
           and then No (Next_Formal (First_Formal (Func)))
         then
            if Cursor /= Any_Type then
               Error_Msg_N
                 ("Operation First for iterable type must be unique", Aspect);
               return Any_Type;
            else
               Cursor := Etype (Func);
            end if;
         end if;

         Next_Entity (Func);
      end loop;

      --  If not found, no way to resolve remaining primitives.

      if Cursor = Any_Type then
         Error_Msg_N
           ("No legal primitive operation First for Iterable type", Aspect);
      end if;

      return Cursor;
   end Get_Cursor_Type;

   function Get_Cursor_Type (Typ : Entity_Id) return Entity_Id is
   begin
      return Etype (Get_Iterable_Type_Primitive (Typ, Name_First));
   end Get_Cursor_Type;

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

   --------------------------
   -- Get_Enclosing_Object --
   --------------------------

   function Get_Enclosing_Object (N : Node_Id) return Entity_Id is
   begin
      if Is_Entity_Name (N) then
         return Entity (N);
      else
         case Nkind (N) is
            when N_Indexed_Component
               | N_Selected_Component
               | N_Slice
            =>
               --  If not generating code, a dereference may be left implicit.
               --  In thoses cases, return Empty.

               if Is_Access_Type (Etype (Prefix (N))) then
                  return Empty;
               else
                  return Get_Enclosing_Object (Prefix (N));
               end if;

            when N_Type_Conversion =>
               return Get_Enclosing_Object (Expression (N));

            when others =>
               return Empty;
         end case;
      end if;
   end Get_Enclosing_Object;

   ---------------------------
   -- Get_Enum_Lit_From_Pos --
   ---------------------------

   function Get_Enum_Lit_From_Pos
     (T   : Entity_Id;
      Pos : Uint;
      Loc : Source_Ptr) return Node_Id
   is
      Btyp : Entity_Id := Base_Type (T);
      Lit  : Node_Id;
      LLoc : Source_Ptr;

   begin
      --  In the case where the literal is of type Character, Wide_Character
      --  or Wide_Wide_Character or of a type derived from them, there needs
      --  to be some special handling since there is no explicit chain of
      --  literals to search. Instead, an N_Character_Literal node is created
      --  with the appropriate Char_Code and Chars fields.

      if Is_Standard_Character_Type (T) then
         Set_Character_Literal_Name (UI_To_CC (Pos));

         return
           Make_Character_Literal (Loc,
             Chars              => Name_Find,
             Char_Literal_Value => Pos);

      --  For all other cases, we have a complete table of literals, and
      --  we simply iterate through the chain of literal until the one
      --  with the desired position value is found.

      else
         if Is_Private_Type (Btyp) and then Present (Full_View (Btyp)) then
            Btyp := Full_View (Btyp);
         end if;

         Lit := First_Literal (Btyp);
         for J in 1 .. UI_To_Int (Pos) loop
            Next_Literal (Lit);

            --  If Lit is Empty, Pos is not in range, so raise Constraint_Error
            --  inside the loop to avoid calling Next_Literal on Empty.

            if No (Lit) then
               raise Constraint_Error;
            end if;
         end loop;

         --  Create a new node from Lit, with source location provided by Loc
         --  if not equal to No_Location, or by copying the source location of
         --  Lit otherwise.

         LLoc := Loc;

         if LLoc = No_Location then
            LLoc := Sloc (Lit);
         end if;

         return New_Occurrence_Of (Lit, LLoc);
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

   -------------------------------------
   -- Get_Incomplete_View_Of_Ancestor --
   -------------------------------------

   function Get_Incomplete_View_Of_Ancestor (E : Entity_Id) return Entity_Id is
      Cur_Unit  : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);
      Par_Scope : Entity_Id;
      Par_Type  : Entity_Id;

   begin
      --  The incomplete view of an ancestor is only relevant for private
      --  derived types in child units.

      if not Is_Derived_Type (E)
        or else not Is_Child_Unit (Cur_Unit)
      then
         return Empty;

      else
         Par_Scope := Scope (Cur_Unit);
         if No (Par_Scope) then
            return Empty;
         end if;

         Par_Type := Etype (Base_Type (E));

         --  Traverse list of ancestor types until we find one declared in
         --  a parent or grandparent unit (two levels seem sufficient).

         while Present (Par_Type) loop
            if Scope (Par_Type) = Par_Scope
              or else Scope (Par_Type) = Scope (Par_Scope)
            then
               return Par_Type;

            elsif not Is_Derived_Type (Par_Type) then
               return Empty;

            else
               Par_Type := Etype (Base_Type (Par_Type));
            end if;
         end loop;

         --  If none found, there is no relevant ancestor type.

         return Empty;
      end if;
   end Get_Incomplete_View_Of_Ancestor;

   ----------------------
   -- Get_Index_Bounds --
   ----------------------

   procedure Get_Index_Bounds
     (N             : Node_Id;
      L             : out Node_Id;
      H             : out Node_Id;
      Use_Full_View : Boolean := False)
   is
      function Scalar_Range_Of_Type (Typ : Entity_Id) return Node_Id;
      --  Obtain the scalar range of type Typ. If flag Use_Full_View is set and
      --  Typ qualifies, the scalar range is obtained from the full view of the
      --  type.

      --------------------------
      -- Scalar_Range_Of_Type --
      --------------------------

      function Scalar_Range_Of_Type (Typ : Entity_Id) return Node_Id is
         T : Entity_Id := Typ;

      begin
         if Use_Full_View and then Present (Full_View (T)) then
            T := Full_View (T);
         end if;

         return Scalar_Range (T);
      end Scalar_Range_Of_Type;

      --  Local variables

      Kind : constant Node_Kind := Nkind (N);
      Rng  : Node_Id;

   --  Start of processing for Get_Index_Bounds

   begin
      if Kind = N_Range then
         L := Low_Bound (N);
         H := High_Bound (N);

      elsif Kind = N_Subtype_Indication then
         Rng := Range_Expression (Constraint (N));

         if Rng = Error then
            L := Error;
            H := Error;
            return;

         else
            L := Low_Bound  (Range_Expression (Constraint (N)));
            H := High_Bound (Range_Expression (Constraint (N)));
         end if;

      elsif Is_Entity_Name (N) and then Is_Type (Entity (N)) then
         Rng := Scalar_Range_Of_Type (Entity (N));

         if Error_Posted (Rng) then
            L := Error;
            H := Error;

         elsif Nkind (Rng) = N_Subtype_Indication then
            Get_Index_Bounds (Rng, L, H);

         else
            L := Low_Bound  (Rng);
            H := High_Bound (Rng);
         end if;

      else
         --  N is an expression, indicating a range with one value

         L := N;
         H := N;
      end if;
   end Get_Index_Bounds;

   -----------------------------
   -- Get_Interfacing_Aspects --
   -----------------------------

   procedure Get_Interfacing_Aspects
     (Iface_Asp : Node_Id;
      Conv_Asp  : out Node_Id;
      EN_Asp    : out Node_Id;
      Expo_Asp  : out Node_Id;
      Imp_Asp   : out Node_Id;
      LN_Asp    : out Node_Id;
      Do_Checks : Boolean := False)
   is
      procedure Save_Or_Duplication_Error
        (Asp : Node_Id;
         To  : in out Node_Id);
      --  Save the value of aspect Asp in node To. If To already has a value,
      --  then this is considered a duplicate use of aspect. Emit an error if
      --  flag Do_Checks is set.

      -------------------------------
      -- Save_Or_Duplication_Error --
      -------------------------------

      procedure Save_Or_Duplication_Error
        (Asp : Node_Id;
         To  : in out Node_Id)
      is
      begin
         --  Detect an extra aspect and issue an error

         if Present (To) then
            if Do_Checks then
               Error_Msg_Name_1 := Chars (Identifier (Asp));
               Error_Msg_Sloc   := Sloc (To);
               Error_Msg_N ("aspect % previously given #", Asp);
            end if;

         --  Otherwise capture the aspect

         else
            To := Asp;
         end if;
      end Save_Or_Duplication_Error;

      --  Local variables

      Asp    : Node_Id;
      Asp_Id : Aspect_Id;

      --  The following variables capture each individual aspect

      Conv : Node_Id := Empty;
      EN   : Node_Id := Empty;
      Expo : Node_Id := Empty;
      Imp  : Node_Id := Empty;
      LN   : Node_Id := Empty;

   --  Start of processing for Get_Interfacing_Aspects

   begin
      --  The input interfacing aspect should reside in an aspect specification
      --  list.

      pragma Assert (Is_List_Member (Iface_Asp));

      --  Examine the aspect specifications of the related entity. Find and
      --  capture all interfacing aspects. Detect duplicates and emit errors
      --  if applicable.

      Asp := First (List_Containing (Iface_Asp));
      while Present (Asp) loop
         Asp_Id := Get_Aspect_Id (Asp);

         if Asp_Id = Aspect_Convention then
            Save_Or_Duplication_Error (Asp, Conv);

         elsif Asp_Id = Aspect_External_Name then
            Save_Or_Duplication_Error (Asp, EN);

         elsif Asp_Id = Aspect_Export then
            Save_Or_Duplication_Error (Asp, Expo);

         elsif Asp_Id = Aspect_Import then
            Save_Or_Duplication_Error (Asp, Imp);

         elsif Asp_Id = Aspect_Link_Name then
            Save_Or_Duplication_Error (Asp, LN);
         end if;

         Next (Asp);
      end loop;

      Conv_Asp := Conv;
      EN_Asp   := EN;
      Expo_Asp := Expo;
      Imp_Asp  := Imp;
      LN_Asp   := LN;
   end Get_Interfacing_Aspects;

   ---------------------------------
   -- Get_Iterable_Type_Primitive --
   ---------------------------------

   function Get_Iterable_Type_Primitive
     (Typ : Entity_Id;
      Nam : Name_Id) return Entity_Id
   is
      Funcs : constant Node_Id := Find_Value_Of_Aspect (Typ, Aspect_Iterable);
      Assoc : Node_Id;

   begin
      if No (Funcs) then
         return Empty;

      else
         Assoc := First (Component_Associations (Funcs));
         while Present (Assoc) loop
            if Chars (First (Choices (Assoc))) = Nam then
               return Entity (Expression (Assoc));
            end if;

            Assoc := Next (Assoc);
         end loop;

         return Empty;
      end if;
   end Get_Iterable_Type_Primitive;

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

   --------------------------
   -- Get_Max_Queue_Length --
   --------------------------

   function Get_Max_Queue_Length (Id : Entity_Id) return Uint is
      pragma Assert (Is_Entry (Id));
      Prag : constant Entity_Id := Get_Pragma (Id, Pragma_Max_Queue_Length);

   begin
      --  A value of 0 represents no maximum specified, and entries and entry
      --  families with no Max_Queue_Length aspect or pragma default to it.

      if not Present (Prag) then
         return Uint_0;
      end if;

      return Intval (Expression (First (Pragma_Argument_Associations (Prag))));
   end Get_Max_Queue_Length;

   ------------------------
   -- Get_Name_Entity_Id --
   ------------------------

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id is
   begin
      return Entity_Id (Get_Name_Table_Int (Id));
   end Get_Name_Entity_Id;

   ------------------------------
   -- Get_Name_From_CTC_Pragma --
   ------------------------------

   function Get_Name_From_CTC_Pragma (N : Node_Id) return String_Id is
      Arg : constant Node_Id :=
              Get_Pragma_Arg (First (Pragma_Argument_Associations (N)));
   begin
      return Strval (Expr_Value_S (Arg));
   end Get_Name_From_CTC_Pragma;

   -----------------------
   -- Get_Parent_Entity --
   -----------------------

   function Get_Parent_Entity (Unit : Node_Id) return Entity_Id is
   begin
      if Nkind (Unit) = N_Package_Body
        and then Nkind (Original_Node (Unit)) = N_Package_Instantiation
      then
         return Defining_Entity
                  (Specification (Instance_Spec (Original_Node (Unit))));
      elsif Nkind (Unit) = N_Package_Instantiation then
         return Defining_Entity (Specification (Instance_Spec (Unit)));
      else
         return Defining_Entity (Unit);
      end if;
   end Get_Parent_Entity;

   -------------------
   -- Get_Pragma_Id --
   -------------------

   function Get_Pragma_Id (N : Node_Id) return Pragma_Id is
   begin
      return Get_Pragma_Id (Pragma_Name_Unmapped (N));
   end Get_Pragma_Id;

   ------------------------
   -- Get_Qualified_Name --
   ------------------------

   function Get_Qualified_Name
     (Id     : Entity_Id;
      Suffix : Entity_Id := Empty) return Name_Id
   is
      Suffix_Nam : Name_Id := No_Name;

   begin
      if Present (Suffix) then
         Suffix_Nam := Chars (Suffix);
      end if;

      return Get_Qualified_Name (Chars (Id), Suffix_Nam, Scope (Id));
   end Get_Qualified_Name;

   function Get_Qualified_Name
     (Nam    : Name_Id;
      Suffix : Name_Id   := No_Name;
      Scop   : Entity_Id := Current_Scope) return Name_Id
   is
      procedure Add_Scope (S : Entity_Id);
      --  Add the fully qualified form of scope S to the name buffer. The
      --  format is:
      --    s-1__s__

      ---------------
      -- Add_Scope --
      ---------------

      procedure Add_Scope (S : Entity_Id) is
      begin
         if S = Empty then
            null;

         elsif S = Standard_Standard then
            null;

         else
            Add_Scope (Scope (S));
            Get_Name_String_And_Append (Chars (S));
            Add_Str_To_Name_Buffer ("__");
         end if;
      end Add_Scope;

   --  Start of processing for Get_Qualified_Name

   begin
      Name_Len := 0;
      Add_Scope (Scop);

      --  Append the base name after all scopes have been chained

      Get_Name_String_And_Append (Nam);

      --  Append the suffix (if present)

      if Suffix /= No_Name then
         Add_Str_To_Name_Buffer ("__");
         Get_Name_String_And_Append (Suffix);
      end if;

      return Name_Find;
   end Get_Qualified_Name;

   -----------------------
   -- Get_Reason_String --
   -----------------------

   procedure Get_Reason_String (N : Node_Id) is
   begin
      if Nkind (N) = N_String_Literal then
         Store_String_Chars (Strval (N));

      elsif Nkind (N) = N_Op_Concat then
         Get_Reason_String (Left_Opnd (N));
         Get_Reason_String (Right_Opnd (N));

      --  If not of required form, error

      else
         Error_Msg_N
           ("Reason for pragma Warnings has wrong form", N);
         Error_Msg_N
           ("\must be string literal or concatenation of string literals", N);
         return;
      end if;
   end Get_Reason_String;

   --------------------------------
   -- Get_Reference_Discriminant --
   --------------------------------

   function Get_Reference_Discriminant (Typ : Entity_Id) return Entity_Id is
      D : Entity_Id;

   begin
      D := First_Discriminant (Typ);
      while Present (D) loop
         if Has_Implicit_Dereference (D) then
            return D;
         end if;
         Next_Discriminant (D);
      end loop;

      return Empty;
   end Get_Reference_Discriminant;

   ---------------------------
   -- Get_Referenced_Object --
   ---------------------------

   function Get_Referenced_Object (N : Node_Id) return Node_Id is
      R : Node_Id;

   begin
      R := N;
      while Is_Entity_Name (R)
        and then Present (Renamed_Object (Entity (R)))
      loop
         R := Renamed_Object (Entity (R));
      end loop;

      return R;
   end Get_Referenced_Object;

   ------------------------
   -- Get_Renamed_Entity --
   ------------------------

   function Get_Renamed_Entity (E : Entity_Id) return Entity_Id is
      R : Entity_Id;

   begin
      R := E;
      while Present (Renamed_Entity (R)) loop
         R := Renamed_Entity (R);
      end loop;

      return R;
   end Get_Renamed_Entity;

   -----------------------
   -- Get_Return_Object --
   -----------------------

   function Get_Return_Object (N : Node_Id) return Entity_Id is
      Decl : Node_Id;

   begin
      Decl := First (Return_Object_Declarations (N));
      while Present (Decl) loop
         exit when Nkind (Decl) = N_Object_Declaration
           and then Is_Return_Object (Defining_Identifier (Decl));
         Next (Decl);
      end loop;

      pragma Assert (Present (Decl));
      return Defining_Identifier (Decl);
   end Get_Return_Object;

   ---------------------------
   -- Get_Subprogram_Entity --
   ---------------------------

   function Get_Subprogram_Entity (Nod : Node_Id) return Entity_Id is
      Subp    : Node_Id;
      Subp_Id : Entity_Id;

   begin
      if Nkind (Nod) = N_Accept_Statement then
         Subp := Entry_Direct_Name (Nod);

      elsif Nkind (Nod) = N_Slice then
         Subp := Prefix (Nod);

      else
         Subp := Name (Nod);
      end if;

      --  Strip the subprogram call

      loop
         if Nkind_In (Subp, N_Explicit_Dereference,
                            N_Indexed_Component,
                            N_Selected_Component)
         then
            Subp := Prefix (Subp);

         elsif Nkind_In (Subp, N_Type_Conversion,
                               N_Unchecked_Type_Conversion)
         then
            Subp := Expression (Subp);

         else
            exit;
         end if;
      end loop;

      --  Extract the entity of the subprogram call

      if Is_Entity_Name (Subp) then
         Subp_Id := Entity (Subp);

         if Ekind (Subp_Id) = E_Access_Subprogram_Type then
            Subp_Id := Directly_Designated_Type (Subp_Id);
         end if;

         if Is_Subprogram (Subp_Id) then
            return Subp_Id;
         else
            return Empty;
         end if;

      --  The search did not find a construct that denotes a subprogram

      else
         return Empty;
      end if;
   end Get_Subprogram_Entity;

   -----------------------------
   -- Get_Task_Body_Procedure --
   -----------------------------

   function Get_Task_Body_Procedure (E : Entity_Id) return Node_Id is
   begin
      --  Note: A task type may be the completion of a private type with
      --  discriminants. When performing elaboration checks on a task
      --  declaration, the current view of the type may be the private one,
      --  and the procedure that holds the body of the task is held in its
      --  underlying type.

      --  This is an odd function, why not have Task_Body_Procedure do
      --  the following digging???

      return Task_Body_Procedure (Underlying_Type (Root_Type (E)));
   end Get_Task_Body_Procedure;

   -------------------------
   -- Get_User_Defined_Eq --
   -------------------------

   function Get_User_Defined_Eq (E : Entity_Id) return Entity_Id is
      Prim : Elmt_Id;
      Op   : Entity_Id;

   begin
      Prim := First_Elmt (Collect_Primitive_Operations (E));
      while Present (Prim) loop
         Op := Node (Prim);

         if Chars (Op) = Name_Op_Eq
           and then Etype (Op) = Standard_Boolean
           and then Etype (First_Formal (Op)) = E
           and then Etype (Next_Formal (First_Formal (Op))) = E
         then
            return Op;
         end if;

         Next_Elmt (Prim);
      end loop;

      return Empty;
   end Get_User_Defined_Eq;

   ---------------
   -- Get_Views --
   ---------------

   procedure Get_Views
     (Typ       : Entity_Id;
      Priv_Typ  : out Entity_Id;
      Full_Typ  : out Entity_Id;
      Full_Base : out Entity_Id;
      CRec_Typ  : out Entity_Id)
   is
      IP_View : Entity_Id;

   begin
      --  Assume that none of the views can be recovered

      Priv_Typ  := Empty;
      Full_Typ  := Empty;
      Full_Base := Empty;
      CRec_Typ  := Empty;

      --  The input type is the corresponding record type of a protected or a
      --  task type.

      if Ekind (Typ) = E_Record_Type
        and then Is_Concurrent_Record_Type (Typ)
      then
         CRec_Typ  := Typ;
         Full_Typ  := Corresponding_Concurrent_Type (CRec_Typ);
         Full_Base := Base_Type (Full_Typ);
         Priv_Typ  := Incomplete_Or_Partial_View (Full_Typ);

      --  Otherwise the input type denotes an arbitrary type

      else
         IP_View := Incomplete_Or_Partial_View (Typ);

         --  The input type denotes the full view of a private type

         if Present (IP_View) then
            Priv_Typ := IP_View;
            Full_Typ := Typ;

         --  The input type is a private type

         elsif Is_Private_Type (Typ) then
            Priv_Typ := Typ;
            Full_Typ := Full_View (Priv_Typ);

         --  Otherwise the input type does not have any views

         else
            Full_Typ := Typ;
         end if;

         if Present (Full_Typ) then
            Full_Base := Base_Type (Full_Typ);

            if Ekind_In (Full_Typ, E_Protected_Type, E_Task_Type) then
               CRec_Typ := Corresponding_Record_Type (Full_Typ);
            end if;
         end if;
      end if;
   end Get_Views;

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
            --  Loop to Check components

            Comp := First_Component_Or_Discriminant (Typ);
            while Present (Comp) loop

               --  Check for access component, tag field does not count, even
               --  though it is implemented internally using an access type.

               if Has_Access_Values (Etype (Comp))
                 and then Chars (Comp) /= Name_uTag
               then
                  return True;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;
         end;

         return False;

      else
         return False;
      end if;
   end Has_Access_Values;

   ------------------------------
   -- Has_Compatible_Alignment --
   ------------------------------

   function Has_Compatible_Alignment
     (Obj         : Entity_Id;
      Expr        : Node_Id;
      Layout_Done : Boolean) return Alignment_Result
   is
      function Has_Compatible_Alignment_Internal
        (Obj         : Entity_Id;
         Expr        : Node_Id;
         Layout_Done : Boolean;
         Default     : Alignment_Result) return Alignment_Result;
      --  This is the internal recursive function that actually does the work.
      --  There is one additional parameter, which says what the result should
      --  be if no alignment information is found, and there is no definite
      --  indication of compatible alignments. At the outer level, this is set
      --  to Unknown, but for internal recursive calls in the case where types
      --  are known to be correct, it is set to Known_Compatible.

      ---------------------------------------
      -- Has_Compatible_Alignment_Internal --
      ---------------------------------------

      function Has_Compatible_Alignment_Internal
        (Obj         : Entity_Id;
         Expr        : Node_Id;
         Layout_Done : Boolean;
         Default     : Alignment_Result) return Alignment_Result
      is
         Result : Alignment_Result := Known_Compatible;
         --  Holds the current status of the result. Note that once a value of
         --  Known_Incompatible is set, it is sticky and does not get changed
         --  to Unknown (the value in Result only gets worse as we go along,
         --  never better).

         Offs : Uint := No_Uint;
         --  Set to a factor of the offset from the base object when Expr is a
         --  selected or indexed component, based on Component_Bit_Offset and
         --  Component_Size respectively. A negative value is used to represent
         --  a value which is not known at compile time.

         procedure Check_Prefix;
         --  Checks the prefix recursively in the case where the expression
         --  is an indexed or selected component.

         procedure Set_Result (R : Alignment_Result);
         --  If R represents a worse outcome (unknown instead of known
         --  compatible, or known incompatible), then set Result to R.

         ------------------
         -- Check_Prefix --
         ------------------

         procedure Check_Prefix is
         begin
            --  The subtlety here is that in doing a recursive call to check
            --  the prefix, we have to decide what to do in the case where we
            --  don't find any specific indication of an alignment problem.

            --  At the outer level, we normally set Unknown as the result in
            --  this case, since we can only set Known_Compatible if we really
            --  know that the alignment value is OK, but for the recursive
            --  call, in the case where the types match, and we have not
            --  specified a peculiar alignment for the object, we are only
            --  concerned about suspicious rep clauses, the default case does
            --  not affect us, since the compiler will, in the absence of such
            --  rep clauses, ensure that the alignment is correct.

            if Default = Known_Compatible
              or else
                (Etype (Obj) = Etype (Expr)
                  and then (Unknown_Alignment (Obj)
                             or else
                               Alignment (Obj) = Alignment (Etype (Obj))))
            then
               Set_Result
                 (Has_Compatible_Alignment_Internal
                    (Obj, Prefix (Expr), Layout_Done, Known_Compatible));

            --  In all other cases, we need a full check on the prefix

            else
               Set_Result
                 (Has_Compatible_Alignment_Internal
                    (Obj, Prefix (Expr), Layout_Done, Unknown));
            end if;
         end Check_Prefix;

         ----------------
         -- Set_Result --
         ----------------

         procedure Set_Result (R : Alignment_Result) is
         begin
            if R > Result then
               Result := R;
            end if;
         end Set_Result;

      --  Start of processing for Has_Compatible_Alignment_Internal

      begin
         --  If Expr is a selected component, we must make sure there is no
         --  potentially troublesome component clause and that the record is
         --  not packed if the layout is not done.

         if Nkind (Expr) = N_Selected_Component then

            --  Packing generates unknown alignment if layout is not done

            if Is_Packed (Etype (Prefix (Expr))) and then not Layout_Done then
               Set_Result (Unknown);
            end if;

            --  Check prefix and component offset

            Check_Prefix;
            Offs := Component_Bit_Offset (Entity (Selector_Name (Expr)));

         --  If Expr is an indexed component, we must make sure there is no
         --  potentially troublesome Component_Size clause and that the array
         --  is not bit-packed if the layout is not done.

         elsif Nkind (Expr) = N_Indexed_Component then
            declare
               Typ : constant Entity_Id := Etype (Prefix (Expr));

            begin
               --  Packing generates unknown alignment if layout is not done

               if Is_Bit_Packed_Array (Typ) and then not Layout_Done then
                  Set_Result (Unknown);
               end if;

               --  Check prefix and component offset (or at least size)

               Check_Prefix;
               Offs := Indexed_Component_Bit_Offset (Expr);
               if Offs = No_Uint then
                  Offs := Component_Size (Typ);
               end if;
            end;
         end if;

         --  If we have a null offset, the result is entirely determined by
         --  the base object and has already been computed recursively.

         if Offs = Uint_0 then
            null;

         --  Case where we know the alignment of the object

         elsif Known_Alignment (Obj) then
            declare
               ObjA : constant Uint := Alignment (Obj);
               ExpA : Uint          := No_Uint;
               SizA : Uint          := No_Uint;

            begin
               --  If alignment of Obj is 1, then we are always OK

               if ObjA = 1 then
                  Set_Result (Known_Compatible);

               --  Alignment of Obj is greater than 1, so we need to check

               else
                  --  If we have an offset, see if it is compatible

                  if Offs /= No_Uint and Offs > Uint_0 then
                     if Offs mod (System_Storage_Unit * ObjA) /= 0 then
                        Set_Result (Known_Incompatible);
                     end if;

                     --  See if Expr is an object with known alignment

                  elsif Is_Entity_Name (Expr)
                    and then Known_Alignment (Entity (Expr))
                  then
                     ExpA := Alignment (Entity (Expr));

                     --  Otherwise, we can use the alignment of the type of
                     --  Expr given that we already checked for
                     --  discombobulating rep clauses for the cases of indexed
                     --  and selected components above.

                  elsif Known_Alignment (Etype (Expr)) then
                     ExpA := Alignment (Etype (Expr));

                     --  Otherwise the alignment is unknown

                  else
                     Set_Result (Default);
                  end if;

                  --  If we got an alignment, see if it is acceptable

                  if ExpA /= No_Uint and then ExpA < ObjA then
                     Set_Result (Known_Incompatible);
                  end if;

                  --  If Expr is not a piece of a larger object, see if size
                  --  is given. If so, check that it is not too small for the
                  --  required alignment.

                  if Offs /= No_Uint then
                     null;

                     --  See if Expr is an object with known size

                  elsif Is_Entity_Name (Expr)
                    and then Known_Static_Esize (Entity (Expr))
                  then
                     SizA := Esize (Entity (Expr));

                     --  Otherwise, we check the object size of the Expr type

                  elsif Known_Static_Esize (Etype (Expr)) then
                     SizA := Esize (Etype (Expr));
                  end if;

                  --  If we got a size, see if it is a multiple of the Obj
                  --  alignment, if not, then the alignment cannot be
                  --  acceptable, since the size is always a multiple of the
                  --  alignment.

                  if SizA /= No_Uint then
                     if SizA mod (ObjA * Ttypes.System_Storage_Unit) /= 0 then
                        Set_Result (Known_Incompatible);
                     end if;
                  end if;
               end if;
            end;

         --  If we do not know required alignment, any non-zero offset is a
         --  potential problem (but certainly may be OK, so result is unknown).

         elsif Offs /= No_Uint then
            Set_Result (Unknown);

         --  If we can't find the result by direct comparison of alignment
         --  values, then there is still one case that we can determine known
         --  result, and that is when we can determine that the types are the
         --  same, and no alignments are specified. Then we known that the
         --  alignments are compatible, even if we don't know the alignment
         --  value in the front end.

         elsif Etype (Obj) = Etype (Expr) then

            --  Types are the same, but we have to check for possible size
            --  and alignments on the Expr object that may make the alignment
            --  different, even though the types are the same.

            if Is_Entity_Name (Expr) then

               --  First check alignment of the Expr object. Any alignment less
               --  than Maximum_Alignment is worrisome since this is the case
               --  where we do not know the alignment of Obj.

               if Known_Alignment (Entity (Expr))
                 and then UI_To_Int (Alignment (Entity (Expr))) <
                                                    Ttypes.Maximum_Alignment
               then
                  Set_Result (Unknown);

                  --  Now check size of Expr object. Any size that is not an
                  --  even multiple of Maximum_Alignment is also worrisome
                  --  since it may cause the alignment of the object to be less
                  --  than the alignment of the type.

               elsif Known_Static_Esize (Entity (Expr))
                 and then
                   (UI_To_Int (Esize (Entity (Expr))) mod
                     (Ttypes.Maximum_Alignment * Ttypes.System_Storage_Unit))
                                                                        /= 0
               then
                  Set_Result (Unknown);

                  --  Otherwise same type is decisive

               else
                  Set_Result (Known_Compatible);
               end if;
            end if;

         --  Another case to deal with is when there is an explicit size or
         --  alignment clause when the types are not the same. If so, then the
         --  result is Unknown. We don't need to do this test if the Default is
         --  Unknown, since that result will be set in any case.

         elsif Default /= Unknown
           and then (Has_Size_Clause      (Etype (Expr))
                       or else
                     Has_Alignment_Clause (Etype (Expr)))
         then
            Set_Result (Unknown);

         --  If no indication found, set default

         else
            Set_Result (Default);
         end if;

         --  Return worst result found

         return Result;
      end Has_Compatible_Alignment_Internal;

   --  Start of processing for Has_Compatible_Alignment

   begin
      --  If Obj has no specified alignment, then set alignment from the type
      --  alignment. Perhaps we should always do this, but for sure we should
      --  do it when there is an address clause since we can do more if the
      --  alignment is known.

      if Unknown_Alignment (Obj) then
         Set_Alignment (Obj, Alignment (Etype (Obj)));
      end if;

      --  Now do the internal call that does all the work

      return
        Has_Compatible_Alignment_Internal (Obj, Expr, Layout_Done, Unknown);
   end Has_Compatible_Alignment;

   ----------------------
   -- Has_Declarations --
   ----------------------

   function Has_Declarations (N : Node_Id) return Boolean is
   begin
      return Nkind_In (Nkind (N), N_Accept_Statement,
                                  N_Block_Statement,
                                  N_Compilation_Unit_Aux,
                                  N_Entry_Body,
                                  N_Package_Body,
                                  N_Protected_Body,
                                  N_Subprogram_Body,
                                  N_Task_Body,
                                  N_Package_Specification);
   end Has_Declarations;

   ---------------------------------
   -- Has_Defaulted_Discriminants --
   ---------------------------------

   function Has_Defaulted_Discriminants (Typ : Entity_Id) return Boolean is
   begin
      return Has_Discriminants (Typ)
       and then Present (First_Discriminant (Typ))
       and then Present (Discriminant_Default_Value
                           (First_Discriminant (Typ)));
   end Has_Defaulted_Discriminants;

   -------------------
   -- Has_Denormals --
   -------------------

   function Has_Denormals (E : Entity_Id) return Boolean is
   begin
      return Is_Floating_Point_Type (E) and then Denorm_On_Target;
   end Has_Denormals;

   -------------------------------------------
   -- Has_Discriminant_Dependent_Constraint --
   -------------------------------------------

   function Has_Discriminant_Dependent_Constraint
     (Comp : Entity_Id) return Boolean
   is
      Comp_Decl  : constant Node_Id := Parent (Comp);
      Subt_Indic : Node_Id;
      Constr     : Node_Id;
      Assn       : Node_Id;

   begin
      --  Discriminants can't depend on discriminants

      if Ekind (Comp) = E_Discriminant then
         return False;

      else
         Subt_Indic := Subtype_Indication (Component_Definition (Comp_Decl));

         if Nkind (Subt_Indic) = N_Subtype_Indication then
            Constr := Constraint (Subt_Indic);

            if Nkind (Constr) = N_Index_Or_Discriminant_Constraint then
               Assn := First (Constraints (Constr));
               while Present (Assn) loop
                  case Nkind (Assn) is
                     when N_Identifier
                        | N_Range
                        | N_Subtype_Indication
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
      end if;

      return False;
   end Has_Discriminant_Dependent_Constraint;

   --------------------------------------
   -- Has_Effectively_Volatile_Profile --
   --------------------------------------

   function Has_Effectively_Volatile_Profile
     (Subp_Id : Entity_Id) return Boolean
   is
      Formal : Entity_Id;

   begin
      --  Inspect the formal parameters looking for an effectively volatile
      --  type.

      Formal := First_Formal (Subp_Id);
      while Present (Formal) loop
         if Is_Effectively_Volatile (Etype (Formal)) then
            return True;
         end if;

         Next_Formal (Formal);
      end loop;

      --  Inspect the return type of functions

      if Ekind_In (Subp_Id, E_Function, E_Generic_Function)
        and then Is_Effectively_Volatile (Etype (Subp_Id))
      then
         return True;
      end if;

      return False;
   end Has_Effectively_Volatile_Profile;

   --------------------------
   -- Has_Enabled_Property --
   --------------------------

   function Has_Enabled_Property
     (Item_Id  : Entity_Id;
      Property : Name_Id) return Boolean
   is
      function Protected_Object_Has_Enabled_Property return Boolean;
      --  Determine whether a protected object denoted by Item_Id has the
      --  property enabled.

      function State_Has_Enabled_Property return Boolean;
      --  Determine whether a state denoted by Item_Id has the property enabled

      function Variable_Has_Enabled_Property return Boolean;
      --  Determine whether a variable denoted by Item_Id has the property
      --  enabled.

      -------------------------------------------
      -- Protected_Object_Has_Enabled_Property --
      -------------------------------------------

      function Protected_Object_Has_Enabled_Property return Boolean is
         Constits     : constant Elist_Id := Part_Of_Constituents (Item_Id);
         Constit_Elmt : Elmt_Id;
         Constit_Id   : Entity_Id;

      begin
         --  Protected objects always have the properties Async_Readers and
         --  Async_Writers (SPARK RM 7.1.2(16)).

         if Property = Name_Async_Readers
           or else Property = Name_Async_Writers
         then
            return True;

         --  Protected objects that have Part_Of components also inherit their
         --  properties Effective_Reads and Effective_Writes
         --  (SPARK RM 7.1.2(16)).

         elsif Present (Constits) then
            Constit_Elmt := First_Elmt (Constits);
            while Present (Constit_Elmt) loop
               Constit_Id := Node (Constit_Elmt);

               if Has_Enabled_Property (Constit_Id, Property) then
                  return True;
               end if;

               Next_Elmt (Constit_Elmt);
            end loop;
         end if;

         return False;
      end Protected_Object_Has_Enabled_Property;

      --------------------------------
      -- State_Has_Enabled_Property --
      --------------------------------

      function State_Has_Enabled_Property return Boolean is
         Decl     : constant Node_Id := Parent (Item_Id);
         Opt      : Node_Id;
         Opt_Nam  : Node_Id;
         Prop     : Node_Id;
         Prop_Nam : Node_Id;
         Props    : Node_Id;

      begin
         --  The declaration of an external abstract state appears as an
         --  extension aggregate. If this is not the case, properties can never
         --  be set.

         if Nkind (Decl) /= N_Extension_Aggregate then
            return False;
         end if;

         --  When External appears as a simple option, it automatically enables
         --  all properties.

         Opt := First (Expressions (Decl));
         while Present (Opt) loop
            if Nkind (Opt) = N_Identifier
              and then Chars (Opt) = Name_External
            then
               return True;
            end if;

            Next (Opt);
         end loop;

         --  When External specifies particular properties, inspect those and
         --  find the desired one (if any).

         Opt := First (Component_Associations (Decl));
         while Present (Opt) loop
            Opt_Nam := First (Choices (Opt));

            if Nkind (Opt_Nam) = N_Identifier
              and then Chars (Opt_Nam) = Name_External
            then
               Props := Expression (Opt);

               --  Multiple properties appear as an aggregate

               if Nkind (Props) = N_Aggregate then

                  --  Simple property form

                  Prop := First (Expressions (Props));
                  while Present (Prop) loop
                     if Chars (Prop) = Property then
                        return True;
                     end if;

                     Next (Prop);
                  end loop;

                  --  Property with expression form

                  Prop := First (Component_Associations (Props));
                  while Present (Prop) loop
                     Prop_Nam := First (Choices (Prop));

                     --  The property can be represented in two ways:
                     --      others   => <value>
                     --    <property> => <value>

                     if Nkind (Prop_Nam) = N_Others_Choice
                       or else (Nkind (Prop_Nam) = N_Identifier
                                 and then Chars (Prop_Nam) = Property)
                     then
                        return Is_True (Expr_Value (Expression (Prop)));
                     end if;

                     Next (Prop);
                  end loop;

               --  Single property

               else
                  return Chars (Props) = Property;
               end if;
            end if;

            Next (Opt);
         end loop;

         return False;
      end State_Has_Enabled_Property;

      -----------------------------------
      -- Variable_Has_Enabled_Property --
      -----------------------------------

      function Variable_Has_Enabled_Property return Boolean is
         function Is_Enabled (Prag : Node_Id) return Boolean;
         --  Determine whether property pragma Prag (if present) denotes an
         --  enabled property.

         ----------------
         -- Is_Enabled --
         ----------------

         function Is_Enabled (Prag : Node_Id) return Boolean is
            Arg1 : Node_Id;

         begin
            if Present (Prag) then
               Arg1 := First (Pragma_Argument_Associations (Prag));

               --  The pragma has an optional Boolean expression, the related
               --  property is enabled only when the expression evaluates to
               --  True.

               if Present (Arg1) then
                  return Is_True (Expr_Value (Get_Pragma_Arg (Arg1)));

               --  Otherwise the lack of expression enables the property by
               --  default.

               else
                  return True;
               end if;

            --  The property was never set in the first place

            else
               return False;
            end if;
         end Is_Enabled;

         --  Local variables

         AR : constant Node_Id :=
                Get_Pragma (Item_Id, Pragma_Async_Readers);
         AW : constant Node_Id :=
                Get_Pragma (Item_Id, Pragma_Async_Writers);
         ER : constant Node_Id :=
                Get_Pragma (Item_Id, Pragma_Effective_Reads);
         EW : constant Node_Id :=
                Get_Pragma (Item_Id, Pragma_Effective_Writes);

      --  Start of processing for Variable_Has_Enabled_Property

      begin
         --  A non-effectively volatile object can never possess external
         --  properties.

         if not Is_Effectively_Volatile (Item_Id) then
            return False;

         --  External properties related to variables come in two flavors -
         --  explicit and implicit. The explicit case is characterized by the
         --  presence of a property pragma with an optional Boolean flag. The
         --  property is enabled when the flag evaluates to True or the flag is
         --  missing altogether.

         elsif Property = Name_Async_Readers    and then Is_Enabled (AR) then
            return True;

         elsif Property = Name_Async_Writers    and then Is_Enabled (AW) then
            return True;

         elsif Property = Name_Effective_Reads  and then Is_Enabled (ER) then
            return True;

         elsif Property = Name_Effective_Writes and then Is_Enabled (EW) then
            return True;

         --  The implicit case lacks all property pragmas

         elsif No (AR) and then No (AW) and then No (ER) and then No (EW) then
            if Is_Protected_Type (Etype (Item_Id)) then
               return Protected_Object_Has_Enabled_Property;
            else
               return True;
            end if;

         else
            return False;
         end if;
      end Variable_Has_Enabled_Property;

   --  Start of processing for Has_Enabled_Property

   begin
      --  Abstract states and variables have a flexible scheme of specifying
      --  external properties.

      if Ekind (Item_Id) = E_Abstract_State then
         return State_Has_Enabled_Property;

      elsif Ekind (Item_Id) = E_Variable then
         return Variable_Has_Enabled_Property;

      --  By default, protected objects only have the properties Async_Readers
      --  and Async_Writers. If they have Part_Of components, they also inherit
      --  their properties Effective_Reads and Effective_Writes
      --  (SPARK RM 7.1.2(16)).

      elsif Ekind (Item_Id) = E_Protected_Object then
         return Protected_Object_Has_Enabled_Property;

      --  Otherwise a property is enabled when the related item is effectively
      --  volatile.

      else
         return Is_Effectively_Volatile (Item_Id);
      end if;
   end Has_Enabled_Property;

   -------------------------------------
   -- Has_Full_Default_Initialization --
   -------------------------------------

   function Has_Full_Default_Initialization (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;
      Prag : Node_Id;

   begin
      --  A type subject to pragma Default_Initial_Condition is fully default
      --  initialized when the pragma appears with a non-null argument. Since
      --  any type may act as the full view of a private type, this check must
      --  be performed prior to the specialized tests below.

      if Has_DIC (Typ) then
         Prag := Get_Pragma (Typ, Pragma_Default_Initial_Condition);
         pragma Assert (Present (Prag));

         return Is_Verifiable_DIC_Pragma (Prag);
      end if;

      --  A scalar type is fully default initialized if it is subject to aspect
      --  Default_Value.

      if Is_Scalar_Type (Typ) then
         return Has_Default_Aspect (Typ);

      --  An array type is fully default initialized if its element type is
      --  scalar and the array type carries aspect Default_Component_Value or
      --  the element type is fully default initialized.

      elsif Is_Array_Type (Typ) then
         return
           Has_Default_Aspect (Typ)
             or else Has_Full_Default_Initialization (Component_Type (Typ));

      --  A protected type, record type, or type extension is fully default
      --  initialized if all its components either carry an initialization
      --  expression or have a type that is fully default initialized. The
      --  parent type of a type extension must be fully default initialized.

      elsif Is_Record_Type (Typ) or else Is_Protected_Type (Typ) then

         --  Inspect all entities defined in the scope of the type, looking for
         --  uninitialized components.

         Comp := First_Entity (Typ);
         while Present (Comp) loop
            if Ekind (Comp) = E_Component
              and then Comes_From_Source (Comp)
              and then No (Expression (Parent (Comp)))
              and then not Has_Full_Default_Initialization (Etype (Comp))
            then
               return False;
            end if;

            Next_Entity (Comp);
         end loop;

         --  Ensure that the parent type of a type extension is fully default
         --  initialized.

         if Etype (Typ) /= Typ
           and then not Has_Full_Default_Initialization (Etype (Typ))
         then
            return False;
         end if;

         --  If we get here, then all components and parent portion are fully
         --  default initialized.

         return True;

      --  A task type is fully default initialized by default

      elsif Is_Task_Type (Typ) then
         return True;

      --  Otherwise the type is not fully default initialized

      else
         return False;
      end if;
   end Has_Full_Default_Initialization;

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

   --------------------
   -- Has_Interfaces --
   --------------------

   function Has_Interfaces
     (T             : Entity_Id;
      Use_Full_View : Boolean := True) return Boolean
   is
      Typ : Entity_Id := Base_Type (T);

   begin
      --  Handle concurrent types

      if Is_Concurrent_Type (Typ) then
         Typ := Corresponding_Record_Type (Typ);
      end if;

      if not Present (Typ)
        or else not Is_Record_Type (Typ)
        or else not Is_Tagged_Type (Typ)
      then
         return False;
      end if;

      --  Handle private types

      if Use_Full_View and then Present (Full_View (Typ)) then
         Typ := Full_View (Typ);
      end if;

      --  Handle concurrent record types

      if Is_Concurrent_Record_Type (Typ)
        and then Is_Non_Empty_List (Abstract_Interface_List (Typ))
      then
         return True;
      end if;

      loop
         if Is_Interface (Typ)
           or else
             (Is_Record_Type (Typ)
               and then Present (Interfaces (Typ))
               and then not Is_Empty_Elmt_List (Interfaces (Typ)))
         then
            return True;
         end if;

         exit when Etype (Typ) = Typ

            --  Handle private types

            or else (Present (Full_View (Etype (Typ)))
                      and then Full_View (Etype (Typ)) = Typ)

            --  Protect frontend against wrong sources with cyclic derivations

            or else Etype (Typ) = T;

         --  Climb to the ancestor type handling private types

         if Present (Full_View (Etype (Typ))) then
            Typ := Full_View (Etype (Typ));
         else
            Typ := Etype (Typ);
         end if;
      end loop;

      return False;
   end Has_Interfaces;

   --------------------------
   -- Has_Max_Queue_Length --
   --------------------------

   function Has_Max_Queue_Length (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind (Id) = E_Entry
          and then Present (Get_Pragma (Id, Pragma_Max_Queue_Length));
   end Has_Max_Queue_Length;

   ---------------------------------
   -- Has_No_Obvious_Side_Effects --
   ---------------------------------

   function Has_No_Obvious_Side_Effects (N : Node_Id) return Boolean is
   begin
      --  For now handle literals, constants, and non-volatile variables and
      --  expressions combining these with operators or short circuit forms.

      if Nkind (N) in N_Numeric_Or_String_Literal then
         return True;

      elsif Nkind (N) = N_Character_Literal then
         return True;

      elsif Nkind (N) in N_Unary_Op then
         return Has_No_Obvious_Side_Effects (Right_Opnd (N));

      elsif Nkind (N) in N_Binary_Op or else Nkind (N) in N_Short_Circuit then
         return Has_No_Obvious_Side_Effects (Left_Opnd  (N))
                   and then
                Has_No_Obvious_Side_Effects (Right_Opnd (N));

      elsif Nkind (N) = N_Expression_With_Actions
        and then Is_Empty_List (Actions (N))
      then
         return Has_No_Obvious_Side_Effects (Expression (N));

      elsif Nkind (N) in N_Has_Entity then
         return Present (Entity (N))
           and then Ekind_In (Entity (N), E_Variable,
                                          E_Constant,
                                          E_Enumeration_Literal,
                                          E_In_Parameter,
                                          E_Out_Parameter,
                                          E_In_Out_Parameter)
           and then not Is_Volatile (Entity (N));

      else
         return False;
      end if;
   end Has_No_Obvious_Side_Effects;

   -----------------------------
   -- Has_Non_Null_Refinement --
   -----------------------------

   function Has_Non_Null_Refinement (Id : Entity_Id) return Boolean is
      Constits : Elist_Id;

   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      Constits := Refinement_Constituents (Id);

      --  For a refinement to be non-null, the first constituent must be
      --  anything other than null.

      return
        Present (Constits)
          and then Nkind (Node (First_Elmt (Constits))) /= N_Null;
   end Has_Non_Null_Refinement;

   ----------------------------------
   -- Has_Non_Trivial_Precondition --
   ----------------------------------

   function Has_Non_Trivial_Precondition (P : Entity_Id) return Boolean is
      Cont : constant Node_Id := Find_Aspect (P, Aspect_Pre);
   begin
      return Present (Cont)
        and then Class_Present (Cont)
        and then not Is_Entity_Name (Expression (Cont));
   end Has_Non_Trivial_Precondition;

   -------------------
   -- Has_Null_Body --
   -------------------

   function Has_Null_Body (Proc_Id : Entity_Id) return Boolean is
      Body_Id : Entity_Id;
      Decl    : Node_Id;
      Spec    : Node_Id;
      Stmt1   : Node_Id;
      Stmt2   : Node_Id;

   begin
      Spec := Parent (Proc_Id);
      Decl := Parent (Spec);

      --  Retrieve the entity of the procedure body (e.g. invariant proc).

      if Nkind (Spec) = N_Procedure_Specification
        and then Nkind (Decl) = N_Subprogram_Declaration
      then
         Body_Id := Corresponding_Body (Decl);

      --  The body acts as a spec

      else
         Body_Id := Proc_Id;
      end if;

      --  The body will be generated later

      if No (Body_Id) then
         return False;
      end if;

      Spec := Parent (Body_Id);
      Decl := Parent (Spec);

      pragma Assert
        (Nkind (Spec) = N_Procedure_Specification
          and then Nkind (Decl) = N_Subprogram_Body);

      Stmt1 := First (Statements (Handled_Statement_Sequence (Decl)));

      --  Look for a null statement followed by an optional return
      --  statement.

      if Nkind (Stmt1) = N_Null_Statement then
         Stmt2 := Next (Stmt1);

         if Present (Stmt2) then
            return Nkind (Stmt2) = N_Simple_Return_Statement;
         else
            return True;
         end if;
      end if;

      return False;
   end Has_Null_Body;

   ------------------------
   -- Has_Null_Exclusion --
   ------------------------

   function Has_Null_Exclusion (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Access_Definition
            | N_Access_Function_Definition
            | N_Access_Procedure_Definition
            | N_Access_To_Object_Definition
            | N_Allocator
            | N_Derived_Type_Definition
            | N_Function_Specification
            | N_Subtype_Declaration
         =>
            return Null_Exclusion_Present (N);

         when N_Component_Definition
            | N_Formal_Object_Declaration
            | N_Object_Renaming_Declaration
         =>
            if Present (Subtype_Mark (N)) then
               return Null_Exclusion_Present (N);
            else pragma Assert (Present (Access_Definition (N)));
               return Null_Exclusion_Present (Access_Definition (N));
            end if;

         when N_Discriminant_Specification =>
            if Nkind (Discriminant_Type (N)) = N_Access_Definition then
               return Null_Exclusion_Present (Discriminant_Type (N));
            else
               return Null_Exclusion_Present (N);
            end if;

         when N_Object_Declaration =>
            if Nkind (Object_Definition (N)) = N_Access_Definition then
               return Null_Exclusion_Present (Object_Definition (N));
            else
               return Null_Exclusion_Present (N);
            end if;

         when N_Parameter_Specification =>
            if Nkind (Parameter_Type (N)) = N_Access_Definition then
               return Null_Exclusion_Present (Parameter_Type (N));
            else
               return Null_Exclusion_Present (N);
            end if;

         when others =>
            return False;
      end case;
   end Has_Null_Exclusion;

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

   -------------------------
   -- Has_Null_Refinement --
   -------------------------

   function Has_Null_Refinement (Id : Entity_Id) return Boolean is
      Constits : Elist_Id;

   begin
      pragma Assert (Ekind (Id) = E_Abstract_State);
      Constits := Refinement_Constituents (Id);

      --  For a refinement to be null, the state's sole constituent must be a
      --  null.

      return
        Present (Constits)
          and then Nkind (Node (First_Elmt (Constits))) = N_Null;
   end Has_Null_Refinement;

   -------------------------------
   -- Has_Overriding_Initialize --
   -------------------------------

   function Has_Overriding_Initialize (T : Entity_Id) return Boolean is
      BT   : constant Entity_Id := Base_Type (T);
      P    : Elmt_Id;

   begin
      if Is_Controlled (BT) then
         if Is_RTU (Scope (BT), Ada_Finalization) then
            return False;

         elsif Present (Primitive_Operations (BT)) then
            P := First_Elmt (Primitive_Operations (BT));
            while Present (P) loop
               declare
                  Init : constant Entity_Id := Node (P);
                  Formal : constant Entity_Id := First_Formal (Init);
               begin
                  if Ekind (Init) = E_Procedure
                    and then Chars (Init) = Name_Initialize
                    and then Comes_From_Source (Init)
                    and then Present (Formal)
                    and then Etype (Formal) = BT
                    and then No (Next_Formal (Formal))
                    and then (Ada_Version < Ada_2012
                               or else not Null_Present (Parent (Init)))
                  then
                     return True;
                  end if;
               end;

               Next_Elmt (P);
            end loop;
         end if;

         --  Here if type itself does not have a non-null Initialize operation:
         --  check immediate ancestor.

         if Is_Derived_Type (BT)
           and then Has_Overriding_Initialize (Etype (BT))
         then
            return True;
         end if;
      end if;

      return False;
   end Has_Overriding_Initialize;

   --------------------------------------
   -- Has_Preelaborable_Initialization --
   --------------------------------------

   function Has_Preelaborable_Initialization (E : Entity_Id) return Boolean is
      Has_PE : Boolean;

      procedure Check_Components (E : Entity_Id);
      --  Check component/discriminant chain, sets Has_PE False if a component
      --  or discriminant does not meet the preelaborable initialization rules.

      ----------------------
      -- Check_Components --
      ----------------------

      procedure Check_Components (E : Entity_Id) is
         Ent : Entity_Id;
         Exp : Node_Id;

         function Is_Preelaborable_Expression (N : Node_Id) return Boolean;
         --  Returns True if and only if the expression denoted by N does not
         --  violate restrictions on preelaborable constructs (RM-10.2.1(5-9)).

         ---------------------------------
         -- Is_Preelaborable_Expression --
         ---------------------------------

         function Is_Preelaborable_Expression (N : Node_Id) return Boolean is
            Exp           : Node_Id;
            Assn          : Node_Id;
            Choice        : Node_Id;
            Comp_Type     : Entity_Id;
            Is_Array_Aggr : Boolean;

         begin
            if Is_OK_Static_Expression (N) then
               return True;

            elsif Nkind (N) = N_Null then
               return True;

            --  Attributes are allowed in general, even if their prefix is a
            --  formal type. (It seems that certain attributes known not to be
            --  static might not be allowed, but there are no rules to prevent
            --  them.)

            elsif Nkind (N) = N_Attribute_Reference then
               return True;

            --  The name of a discriminant evaluated within its parent type is
            --  defined to be preelaborable (10.2.1(8)). Note that we test for
            --  names that denote discriminals as well as discriminants to
            --  catch references occurring within init procs.

            elsif Is_Entity_Name (N)
              and then
                (Ekind (Entity (N)) = E_Discriminant
                  or else (Ekind_In (Entity (N), E_Constant, E_In_Parameter)
                            and then Present (Discriminal_Link (Entity (N)))))
            then
               return True;

            elsif Nkind (N) = N_Qualified_Expression then
               return Is_Preelaborable_Expression (Expression (N));

            --  For aggregates we have to check that each of the associations
            --  is preelaborable.

            elsif Nkind_In (N, N_Aggregate, N_Extension_Aggregate) then
               Is_Array_Aggr := Is_Array_Type (Etype (N));

               if Is_Array_Aggr then
                  Comp_Type := Component_Type (Etype (N));
               end if;

               --  Check the ancestor part of extension aggregates, which must
               --  be either the name of a type that has preelaborable init or
               --  an expression that is preelaborable.

               if Nkind (N) = N_Extension_Aggregate then
                  declare
                     Anc_Part : constant Node_Id := Ancestor_Part (N);

                  begin
                     if Is_Entity_Name (Anc_Part)
                       and then Is_Type (Entity (Anc_Part))
                     then
                        if not Has_Preelaborable_Initialization
                                 (Entity (Anc_Part))
                        then
                           return False;
                        end if;

                     elsif not Is_Preelaborable_Expression (Anc_Part) then
                        return False;
                     end if;
                  end;
               end if;

               --  Check positional associations

               Exp := First (Expressions (N));
               while Present (Exp) loop
                  if not Is_Preelaborable_Expression (Exp) then
                     return False;
                  end if;

                  Next (Exp);
               end loop;

               --  Check named associations

               Assn := First (Component_Associations (N));
               while Present (Assn) loop
                  Choice := First (Choices (Assn));
                  while Present (Choice) loop
                     if Is_Array_Aggr then
                        if Nkind (Choice) = N_Others_Choice then
                           null;

                        elsif Nkind (Choice) = N_Range then
                           if not Is_OK_Static_Range (Choice) then
                              return False;
                           end if;

                        elsif not Is_OK_Static_Expression (Choice) then
                           return False;
                        end if;

                     else
                        Comp_Type := Etype (Choice);
                     end if;

                     Next (Choice);
                  end loop;

                  --  If the association has a <> at this point, then we have
                  --  to check whether the component's type has preelaborable
                  --  initialization. Note that this only occurs when the
                  --  association's corresponding component does not have a
                  --  default expression, the latter case having already been
                  --  expanded as an expression for the association.

                  if Box_Present (Assn) then
                     if not Has_Preelaborable_Initialization (Comp_Type) then
                        return False;
                     end if;

                  --  In the expression case we check whether the expression
                  --  is preelaborable.

                  elsif
                    not Is_Preelaborable_Expression (Expression (Assn))
                  then
                     return False;
                  end if;

                  Next (Assn);
               end loop;

               --  If we get here then aggregate as a whole is preelaborable

               return True;

            --  All other cases are not preelaborable

            else
               return False;
            end if;
         end Is_Preelaborable_Expression;

      --  Start of processing for Check_Components

      begin
         --  Loop through entities of record or protected type

         Ent := E;
         while Present (Ent) loop

            --  We are interested only in components and discriminants

            Exp := Empty;

            case Ekind (Ent) is
               when E_Component =>

                  --  Get default expression if any. If there is no declaration
                  --  node, it means we have an internal entity. The parent and
                  --  tag fields are examples of such entities. For such cases,
                  --  we just test the type of the entity.

                  if Present (Declaration_Node (Ent)) then
                     Exp := Expression (Declaration_Node (Ent));
                  end if;

               when E_Discriminant =>

                  --  Note: for a renamed discriminant, the Declaration_Node
                  --  may point to the one from the ancestor, and have a
                  --  different expression, so use the proper attribute to
                  --  retrieve the expression from the derived constraint.

                  Exp := Discriminant_Default_Value (Ent);

               when others =>
                  goto Check_Next_Entity;
            end case;

            --  A component has PI if it has no default expression and the
            --  component type has PI.

            if No (Exp) then
               if not Has_Preelaborable_Initialization (Etype (Ent)) then
                  Has_PE := False;
                  exit;
               end if;

            --  Require the default expression to be preelaborable

            elsif not Is_Preelaborable_Expression (Exp) then
               Has_PE := False;
               exit;
            end if;

         <<Check_Next_Entity>>
            Next_Entity (Ent);
         end loop;
      end Check_Components;

   --  Start of processing for Has_Preelaborable_Initialization

   begin
      --  Immediate return if already marked as known preelaborable init. This
      --  covers types for which this function has already been called once
      --  and returned True (in which case the result is cached), and also
      --  types to which a pragma Preelaborable_Initialization applies.

      if Known_To_Have_Preelab_Init (E) then
         return True;
      end if;

      --  If the type is a subtype representing a generic actual type, then
      --  test whether its base type has preelaborable initialization since
      --  the subtype representing the actual does not inherit this attribute
      --  from the actual or formal. (but maybe it should???)

      if Is_Generic_Actual_Type (E) then
         return Has_Preelaborable_Initialization (Base_Type (E));
      end if;

      --  All elementary types have preelaborable initialization

      if Is_Elementary_Type (E) then
         Has_PE := True;

      --  Array types have PI if the component type has PI

      elsif Is_Array_Type (E) then
         Has_PE := Has_Preelaborable_Initialization (Component_Type (E));

      --  A derived type has preelaborable initialization if its parent type
      --  has preelaborable initialization and (in the case of a derived record
      --  extension) if the non-inherited components all have preelaborable
      --  initialization. However, a user-defined controlled type with an
      --  overriding Initialize procedure does not have preelaborable
      --  initialization.

      elsif Is_Derived_Type (E) then

         --  If the derived type is a private extension then it doesn't have
         --  preelaborable initialization.

         if Ekind (Base_Type (E)) = E_Record_Type_With_Private then
            return False;
         end if;

         --  First check whether ancestor type has preelaborable initialization

         Has_PE := Has_Preelaborable_Initialization (Etype (Base_Type (E)));

         --  If OK, check extension components (if any)

         if Has_PE and then Is_Record_Type (E) then
            Check_Components (First_Entity (E));
         end if;

         --  Check specifically for 10.2.1(11.4/2) exception: a controlled type
         --  with a user defined Initialize procedure does not have PI. If
         --  the type is untagged, the control primitives come from a component
         --  that has already been checked.

         if Has_PE
           and then Is_Controlled (E)
           and then Is_Tagged_Type (E)
           and then Has_Overriding_Initialize (E)
         then
            Has_PE := False;
         end if;

      --  Private types not derived from a type having preelaborable init and
      --  that are not marked with pragma Preelaborable_Initialization do not
      --  have preelaborable initialization.

      elsif Is_Private_Type (E) then
         return False;

      --  Record type has PI if it is non private and all components have PI

      elsif Is_Record_Type (E) then
         Has_PE := True;
         Check_Components (First_Entity (E));

      --  Protected types must not have entries, and components must meet
      --  same set of rules as for record components.

      elsif Is_Protected_Type (E) then
         if Has_Entries (E) then
            Has_PE := False;
         else
            Has_PE := True;
            Check_Components (First_Entity (E));
            Check_Components (First_Private_Entity (E));
         end if;

      --  Type System.Address always has preelaborable initialization

      elsif Is_RTE (E, RE_Address) then
         Has_PE := True;

      --  In all other cases, type does not have preelaborable initialization

      else
         return False;
      end if;

      --  If type has preelaborable initialization, cache result

      if Has_PE then
         Set_Known_To_Have_Preelab_Init (E);
      end if;

      return Has_PE;
   end Has_Preelaborable_Initialization;

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
                            and then
                         not Is_Generic_Type (Root_Type (Btype));
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

   ----------------------
   -- Has_Signed_Zeros --
   ----------------------

   function Has_Signed_Zeros (E : Entity_Id) return Boolean is
   begin
      return Is_Floating_Point_Type (E) and then Signed_Zeros_On_Target;
   end Has_Signed_Zeros;

   ------------------------------
   -- Has_Significant_Contract --
   ------------------------------

   function Has_Significant_Contract (Subp_Id : Entity_Id) return Boolean is
      Subp_Nam : constant Name_Id := Chars (Subp_Id);

   begin
      --  _Finalizer procedure

      if Subp_Nam = Name_uFinalizer then
         return False;

      --  _Postconditions procedure

      elsif Subp_Nam = Name_uPostconditions then
         return False;

      --  Predicate function

      elsif Ekind (Subp_Id) = E_Function
        and then Is_Predicate_Function (Subp_Id)
      then
         return False;

      --  TSS subprogram

      elsif Get_TSS_Name (Subp_Id) /= TSS_Null then
         return False;

      else
         return True;
      end if;
   end Has_Significant_Contract;

   -----------------------------
   -- Has_Static_Array_Bounds --
   -----------------------------

   function Has_Static_Array_Bounds (Typ : Node_Id) return Boolean is
      Ndims : constant Nat := Number_Dimensions (Typ);

      Index : Node_Id;
      Low   : Node_Id;
      High  : Node_Id;

   begin
      --  Unconstrained types do not have static bounds

      if not Is_Constrained (Typ) then
         return False;
      end if;

      --  First treat string literals specially, as the lower bound and length
      --  of string literals are not stored like those of arrays.

      --  A string literal always has static bounds

      if Ekind (Typ) = E_String_Literal_Subtype then
         return True;
      end if;

      --  Treat all dimensions in turn

      Index := First_Index (Typ);
      for Indx in 1 .. Ndims loop

         --  In case of an illegal index which is not a discrete type, return
         --  that the type is not static.

         if not Is_Discrete_Type (Etype (Index))
           or else Etype (Index) = Any_Type
         then
            return False;
         end if;

         Get_Index_Bounds (Index, Low, High);

         if Error_Posted (Low) or else Error_Posted (High) then
            return False;
         end if;

         if Is_OK_Static_Expression (Low)
              and then
            Is_OK_Static_Expression (High)
         then
            null;
         else
            return False;
         end if;

         Next (Index);
      end loop;

      --  If we fall through the loop, all indexes matched

      return True;
   end Has_Static_Array_Bounds;

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

   ----------------
   -- Has_Suffix --
   ----------------

   function Has_Suffix (E : Entity_Id; Suffix : Character) return Boolean is
   begin
      Get_Name_String (Chars (E));
      return Name_Buffer (Name_Len) = Suffix;
   end Has_Suffix;

   ----------------
   -- Add_Suffix --
   ----------------

   function Add_Suffix (E : Entity_Id; Suffix : Character) return Name_Id is
   begin
      Get_Name_String (Chars (E));
      Add_Char_To_Name_Buffer (Suffix);
      return Name_Find;
   end Add_Suffix;

   -------------------
   -- Remove_Suffix --
   -------------------

   function Remove_Suffix (E : Entity_Id; Suffix : Character) return Name_Id is
   begin
      pragma Assert (Has_Suffix (E, Suffix));
      Get_Name_String (Chars (E));
      Name_Len := Name_Len - 1;
      return Name_Find;
   end Remove_Suffix;

   ----------------------------------
   -- Replace_Null_By_Null_Address --
   ----------------------------------

   procedure Replace_Null_By_Null_Address (N : Node_Id) is
      procedure Replace_Null_Operand (Op : Node_Id; Other_Op : Node_Id);
      --  Replace operand Op with a reference to Null_Address when the operand
      --  denotes a null Address. Other_Op denotes the other operand.

      --------------------------
      -- Replace_Null_Operand --
      --------------------------

      procedure Replace_Null_Operand (Op : Node_Id; Other_Op : Node_Id) is
      begin
         --  Check the type of the complementary operand since the N_Null node
         --  has not been decorated yet.

         if Nkind (Op) = N_Null
           and then Is_Descendant_Of_Address (Etype (Other_Op))
         then
            Rewrite (Op, New_Occurrence_Of (RTE (RE_Null_Address), Sloc (Op)));
         end if;
      end Replace_Null_Operand;

   --  Start of processing for Replace_Null_By_Null_Address

   begin
      pragma Assert (Relaxed_RM_Semantics);
      pragma Assert (Nkind_In (N, N_Null,
                                  N_Op_Eq,
                                  N_Op_Ge,
                                  N_Op_Gt,
                                  N_Op_Le,
                                  N_Op_Lt,
                                  N_Op_Ne));

      if Nkind (N) = N_Null then
         Rewrite (N, New_Occurrence_Of (RTE (RE_Null_Address), Sloc (N)));

      else
         declare
            L : constant Node_Id := Left_Opnd  (N);
            R : constant Node_Id := Right_Opnd (N);

         begin
            Replace_Null_Operand (L, Other_Op => R);
            Replace_Null_Operand (R, Other_Op => L);
         end;
      end if;
   end Replace_Null_By_Null_Address;

   --------------------------
   -- Has_Tagged_Component --
   --------------------------

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Is_Private_Type (Typ) and then Present (Underlying_Type (Typ)) then
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

            Next_Component (Comp);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Tagged_Component;

   -----------------------------
   -- Has_Undefined_Reference --
   -----------------------------

   function Has_Undefined_Reference (Expr : Node_Id) return Boolean is
      Has_Undef_Ref : Boolean := False;
      --  Flag set when expression Expr contains at least one undefined
      --  reference.

      function Is_Undefined_Reference (N : Node_Id) return Traverse_Result;
      --  Determine whether N denotes a reference and if it does, whether it is
      --  undefined.

      ----------------------------
      -- Is_Undefined_Reference --
      ----------------------------

      function Is_Undefined_Reference (N : Node_Id) return Traverse_Result is
      begin
         if Is_Entity_Name (N)
           and then Present (Entity (N))
           and then Entity (N) = Any_Id
         then
            Has_Undef_Ref := True;
            return Abandon;
         end if;

         return OK;
      end Is_Undefined_Reference;

      procedure Find_Undefined_References is
        new Traverse_Proc (Is_Undefined_Reference);

   --  Start of processing for Has_Undefined_Reference

   begin
      Find_Undefined_References (Expr);

      return Has_Undef_Ref;
   end Has_Undefined_Reference;

   ----------------------------
   -- Has_Volatile_Component --
   ----------------------------

   function Has_Volatile_Component (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Has_Volatile_Components (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         return Is_Volatile (Component_Type (Typ));

      elsif Is_Record_Type (Typ) then
         Comp := First_Component (Typ);
         while Present (Comp) loop
            if Is_Volatile_Object (Comp) then
               return True;
            end if;

            Comp := Next_Component (Comp);
         end loop;
      end if;

      return False;
   end Has_Volatile_Component;

   -------------------------
   -- Implementation_Kind --
   -------------------------

   function Implementation_Kind (Subp : Entity_Id) return Name_Id is
      Impl_Prag : constant Node_Id := Get_Rep_Pragma (Subp, Name_Implemented);
      Arg       : Node_Id;
   begin
      pragma Assert (Present (Impl_Prag));
      Arg := Last (Pragma_Argument_Associations (Impl_Prag));
      return Chars (Get_Pragma_Arg (Arg));
   end Implementation_Kind;

   --------------------------
   -- Implements_Interface --
   --------------------------

   function Implements_Interface
     (Typ_Ent         : Entity_Id;
      Iface_Ent       : Entity_Id;
      Exclude_Parents : Boolean := False) return Boolean
   is
      Ifaces_List : Elist_Id;
      Elmt        : Elmt_Id;
      Iface       : Entity_Id := Base_Type (Iface_Ent);
      Typ         : Entity_Id := Base_Type (Typ_Ent);

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      if not Has_Interfaces (Typ) then
         return False;
      end if;

      if Is_Class_Wide_Type (Iface) then
         Iface := Root_Type (Iface);
      end if;

      Collect_Interfaces (Typ, Ifaces_List);

      Elmt := First_Elmt (Ifaces_List);
      while Present (Elmt) loop
         if Is_Ancestor (Node (Elmt), Typ, Use_Full_View => True)
           and then Exclude_Parents
         then
            null;

         elsif Node (Elmt) = Iface then
            return True;
         end if;

         Next_Elmt (Elmt);
      end loop;

      return False;
   end Implements_Interface;

   ------------------------------------
   -- In_Assertion_Expression_Pragma --
   ------------------------------------

   function In_Assertion_Expression_Pragma (N : Node_Id) return Boolean is
      Par  : Node_Id;
      Prag : Node_Id := Empty;

   begin
      --  Climb the parent chain looking for an enclosing pragma

      Par := N;
      while Present (Par) loop
         if Nkind (Par) = N_Pragma then
            Prag := Par;
            exit;

         --  Precondition-like pragmas are expanded into if statements, check
         --  the original node instead.

         elsif Nkind (Original_Node (Par)) = N_Pragma then
            Prag := Original_Node (Par);
            exit;

         --  The expansion of attribute 'Old generates a constant to capture
         --  the result of the prefix. If the parent traversal reaches
         --  one of these constants, then the node technically came from a
         --  postcondition-like pragma. Note that the Ekind is not tested here
         --  because N may be the expression of an object declaration which is
         --  currently being analyzed. Such objects carry Ekind of E_Void.

         elsif Nkind (Par) = N_Object_Declaration
           and then Constant_Present (Par)
           and then Stores_Attribute_Old_Prefix (Defining_Entity (Par))
         then
            return True;

         --  Prevent the search from going too far

         elsif Is_Body_Or_Package_Declaration (Par) then
            return False;
         end if;

         Par := Parent (Par);
      end loop;

      return
        Present (Prag)
          and then Assertion_Expression_Pragma (Get_Pragma_Id (Prag));
   end In_Assertion_Expression_Pragma;

   ----------------------
   -- In_Generic_Scope --
   ----------------------

   function In_Generic_Scope (E : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Scope (E);
      while Present (S) and then S /= Standard_Standard loop
         if Is_Generic_Unit (S) then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Generic_Scope;

   -----------------
   -- In_Instance --
   -----------------

   function In_Instance return Boolean is
      Curr_Unit : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);
      S         : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Is_Generic_Instance (S) then

            --  A child instance is always compiled in the context of a parent
            --  instance. Nevertheless, the actuals are not analyzed in an
            --  instance context. We detect this case by examining the current
            --  compilation unit, which must be a child instance, and checking
            --  that it is not currently on the scope stack.

            if Is_Child_Unit (Curr_Unit)
              and then Nkind (Unit (Cunit (Current_Sem_Unit))) =
                                                     N_Package_Instantiation
              and then not In_Open_Scopes (Curr_Unit)
            then
               return False;
            else
               return True;
            end if;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance;

   ----------------------
   -- In_Instance_Body --
   ----------------------

   function In_Instance_Body return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Ekind_In (S, E_Function, E_Procedure)
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
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Ekind_In (S, E_Function, E_Procedure)
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
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
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

   ---------------------
   -- In_Package_Body --
   ---------------------

   function In_Package_Body return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while Present (S) and then S /= Standard_Standard loop
         if Ekind (S) = E_Package and then In_Package_Body (S) then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end In_Package_Body;

   --------------------------
   -- In_Pragma_Expression --
   --------------------------

   function In_Pragma_Expression (N : Node_Id; Nam : Name_Id) return Boolean is
      P : Node_Id;
   begin
      P := Parent (N);
      loop
         if No (P) then
            return False;
         elsif Nkind (P) = N_Pragma and then Pragma_Name (P) = Nam then
            return True;
         else
            P := Parent (P);
         end if;
      end loop;
   end In_Pragma_Expression;

   ---------------------------
   -- In_Pre_Post_Condition --
   ---------------------------

   function In_Pre_Post_Condition (N : Node_Id) return Boolean is
      Par     : Node_Id;
      Prag    : Node_Id := Empty;
      Prag_Id : Pragma_Id;

   begin
      --  Climb the parent chain looking for an enclosing pragma

      Par := N;
      while Present (Par) loop
         if Nkind (Par) = N_Pragma then
            Prag := Par;
            exit;

         --  Prevent the search from going too far

         elsif Is_Body_Or_Package_Declaration (Par) then
            exit;
         end if;

         Par := Parent (Par);
      end loop;

      if Present (Prag) then
         Prag_Id := Get_Pragma_Id (Prag);

         return
           Prag_Id = Pragma_Post
             or else Prag_Id = Pragma_Post_Class
             or else Prag_Id = Pragma_Postcondition
             or else Prag_Id = Pragma_Pre
             or else Prag_Id = Pragma_Pre_Class
             or else Prag_Id = Pragma_Precondition;

      --  Otherwise the node is not enclosed by a pre/postcondition pragma

      else
         return False;
      end if;
   end In_Pre_Post_Condition;

   -------------------------------------
   -- In_Reverse_Storage_Order_Object --
   -------------------------------------

   function In_Reverse_Storage_Order_Object (N : Node_Id) return Boolean is
      Pref : Node_Id;
      Btyp : Entity_Id := Empty;

   begin
      --  Climb up indexed components

      Pref := N;
      loop
         case Nkind (Pref) is
            when N_Selected_Component =>
               Pref := Prefix (Pref);
               exit;

            when N_Indexed_Component =>
               Pref := Prefix (Pref);

            when others =>
               Pref := Empty;
               exit;
         end case;
      end loop;

      if Present (Pref) then
         Btyp := Base_Type (Etype (Pref));
      end if;

      return Present (Btyp)
        and then (Is_Record_Type (Btyp) or else Is_Array_Type (Btyp))
        and then Reverse_Storage_Order (Btyp);
   end In_Reverse_Storage_Order_Object;

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
      return Is_Package_Or_Generic_Package (Scope_Id)
        and then In_Open_Scopes (Scope_Id)
        and then not In_Package_Body (Scope_Id)
        and then not In_Private_Part (Scope_Id);
   end In_Visible_Part;

   --------------------------------
   -- Incomplete_Or_Partial_View --
   --------------------------------

   function Incomplete_Or_Partial_View (Id : Entity_Id) return Entity_Id is
      function Inspect_Decls
        (Decls : List_Id;
         Taft  : Boolean := False) return Entity_Id;
      --  Check whether a declarative region contains the incomplete or partial
      --  view of Id.

      -------------------
      -- Inspect_Decls --
      -------------------

      function Inspect_Decls
        (Decls : List_Id;
         Taft  : Boolean := False) return Entity_Id
      is
         Decl  : Node_Id;
         Match : Node_Id;

      begin
         Decl := First (Decls);
         while Present (Decl) loop
            Match := Empty;

            --  The partial view of a Taft-amendment type is an incomplete
            --  type.

            if Taft then
               if Nkind (Decl) = N_Incomplete_Type_Declaration then
                  Match := Defining_Identifier (Decl);
               end if;

            --  Otherwise look for a private type whose full view matches the
            --  input type. Note that this checks full_type_declaration nodes
            --  to account for derivations from a private type where the type
            --  declaration hold the partial view and the full view is an
            --  itype.

            elsif Nkind_In (Decl, N_Full_Type_Declaration,
                                  N_Private_Extension_Declaration,
                                  N_Private_Type_Declaration)
            then
               Match := Defining_Identifier (Decl);
            end if;

            --  Guard against unanalyzed entities

            if Present (Match)
              and then Is_Type (Match)
              and then Present (Full_View (Match))
              and then Full_View (Match) = Id
            then
               return Match;
            end if;

            Next (Decl);
         end loop;

         return Empty;
      end Inspect_Decls;

      --  Local variables

      Prev : Entity_Id;

   --  Start of processing for Incomplete_Or_Partial_View

   begin
      --  Deferred constant or incomplete type case

      Prev := Current_Entity_In_Scope (Id);

      if Present (Prev)
        and then (Is_Incomplete_Type (Prev) or else Ekind (Prev) = E_Constant)
        and then Present (Full_View (Prev))
        and then Full_View (Prev) = Id
      then
         return Prev;
      end if;

      --  Private or Taft amendment type case

      declare
         Pkg      : constant Entity_Id := Scope (Id);
         Pkg_Decl : Node_Id := Pkg;

      begin
         if Present (Pkg)
           and then Ekind_In (Pkg, E_Generic_Package, E_Package)
         then
            while Nkind (Pkg_Decl) /= N_Package_Specification loop
               Pkg_Decl := Parent (Pkg_Decl);
            end loop;

            --  It is knows that Typ has a private view, look for it in the
            --  visible declarations of the enclosing scope. A special case
            --  of this is when the two views have been exchanged - the full
            --  appears earlier than the private.

            if Has_Private_Declaration (Id) then
               Prev := Inspect_Decls (Visible_Declarations (Pkg_Decl));

               --  Exchanged view case, look in the private declarations

               if No (Prev) then
                  Prev := Inspect_Decls (Private_Declarations (Pkg_Decl));
               end if;

               return Prev;

            --  Otherwise if this is the package body, then Typ is a potential
            --  Taft amendment type. The incomplete view should be located in
            --  the private declarations of the enclosing scope.

            elsif In_Package_Body (Pkg) then
               return Inspect_Decls (Private_Declarations (Pkg_Decl), True);
            end if;
         end if;
      end;

      --  The type has no incomplete or private view

      return Empty;
   end Incomplete_Or_Partial_View;

   ----------------------------------
   -- Indexed_Component_Bit_Offset --
   ----------------------------------

   function Indexed_Component_Bit_Offset (N : Node_Id) return Uint is
      Exp : constant Node_Id   := First (Expressions (N));
      Typ : constant Entity_Id := Etype (Prefix (N));
      Off : constant Uint      := Component_Size (Typ);
      Ind : Node_Id;

   begin
      --  Return early if the component size is not known or variable

      if Off = No_Uint or else Off < Uint_0 then
         return No_Uint;
      end if;

      --  Deal with the degenerate case of an empty component

      if Off = Uint_0 then
         return Off;
      end if;

      --  Check that both the index value and the low bound are known

      if not Compile_Time_Known_Value (Exp) then
         return No_Uint;
      end if;

      Ind := First_Index (Typ);
      if No (Ind) then
         return No_Uint;
      end if;

      if Nkind (Ind) = N_Subtype_Indication then
         Ind := Constraint (Ind);

         if Nkind (Ind) = N_Range_Constraint then
            Ind := Range_Expression (Ind);
         end if;
      end if;

      if Nkind (Ind) /= N_Range
        or else not Compile_Time_Known_Value (Low_Bound (Ind))
      then
         return No_Uint;
      end if;

      --  Return the scaled offset

      return Off * (Expr_Value (Exp) - Expr_Value (Low_Bound ((Ind))));
   end Indexed_Component_Bit_Offset;

   ----------------------------
   -- Inherit_Rep_Item_Chain --
   ----------------------------

   procedure Inherit_Rep_Item_Chain (Typ : Entity_Id; From_Typ : Entity_Id) is
      Item      : Node_Id;
      Next_Item : Node_Id;

   begin
      --  There are several inheritance scenarios to consider depending on
      --  whether both types have rep item chains and whether the destination
      --  type already inherits part of the source type's rep item chain.

      --  1) The source type lacks a rep item chain
      --     From_Typ ---> Empty
      --
      --     Typ --------> Item (or Empty)

      --  In this case inheritance cannot take place because there are no items
      --  to inherit.

      --  2) The destination type lacks a rep item chain
      --     From_Typ ---> Item ---> ...
      --
      --     Typ --------> Empty

      --  Inheritance takes place by setting the First_Rep_Item of the
      --  destination type to the First_Rep_Item of the source type.
      --     From_Typ ---> Item ---> ...
      --                    ^
      --     Typ -----------+

      --  3.1) Both source and destination types have at least one rep item.
      --  The destination type does NOT inherit a rep item from the source
      --  type.
      --     From_Typ ---> Item ---> Item
      --
      --     Typ --------> Item ---> Item

      --  Inheritance takes place by setting the Next_Rep_Item of the last item
      --  of the destination type to the First_Rep_Item of the source type.
      --     From_Typ -------------------> Item ---> Item
      --                                    ^
      --     Typ --------> Item ---> Item --+

      --  3.2) Both source and destination types have at least one rep item.
      --  The destination type DOES inherit part of the rep item chain of the
      --  source type.
      --     From_Typ ---> Item ---> Item ---> Item
      --                              ^
      --     Typ --------> Item ------+

      --  This rare case arises when the full view of a private extension must
      --  inherit the rep item chain from the full view of its parent type and
      --  the full view of the parent type contains extra rep items. Currently
      --  only invariants may lead to such form of inheritance.

      --     type From_Typ is tagged private
      --       with Type_Invariant'Class => Item_2;

      --     type Typ is new From_Typ with private
      --       with Type_Invariant => Item_4;

      --  At this point the rep item chains contain the following items

      --     From_Typ -----------> Item_2 ---> Item_3
      --                            ^
      --     Typ --------> Item_4 --+

      --  The full views of both types may introduce extra invariants

      --     type From_Typ is tagged null record
      --       with Type_Invariant => Item_1;

      --     type Typ is new From_Typ with null record;

      --  The full view of Typ would have to inherit any new rep items added to
      --  the full view of From_Typ.

      --     From_Typ -----------> Item_1 ---> Item_2 ---> Item_3
      --                            ^
      --     Typ --------> Item_4 --+

      --  To achieve this form of inheritance, the destination type must first
      --  sever the link between its own rep chain and that of the source type,
      --  then inheritance 3.1 takes place.

      --  Case 1: The source type lacks a rep item chain

      if No (First_Rep_Item (From_Typ)) then
         return;

      --  Case 2: The destination type lacks a rep item chain

      elsif No (First_Rep_Item (Typ)) then
         Set_First_Rep_Item (Typ, First_Rep_Item (From_Typ));

      --  Case 3: Both the source and destination types have at least one rep
      --  item. Traverse the rep item chain of the destination type to find the
      --  last rep item.

      else
         Item      := Empty;
         Next_Item := First_Rep_Item (Typ);
         while Present (Next_Item) loop

            --  Detect a link between the destination type's rep chain and that
            --  of the source type. There are two possibilities:

            --    Variant 1
            --                  Next_Item
            --                      V
            --       From_Typ ---> Item_1 --->
            --                      ^
            --       Typ -----------+
            --
            --       Item is Empty

            --    Variant 2
            --                              Next_Item
            --                                  V
            --       From_Typ ---> Item_1 ---> Item_2 --->
            --                                  ^
            --       Typ --------> Item_3 ------+
            --                      ^
            --                     Item

            if Has_Rep_Item (From_Typ, Next_Item) then
               exit;
            end if;

            Item      := Next_Item;
            Next_Item := Next_Rep_Item (Next_Item);
         end loop;

         --  Inherit the source type's rep item chain

         if Present (Item) then
            Set_Next_Rep_Item (Item, First_Rep_Item (From_Typ));
         else
            Set_First_Rep_Item (Typ, First_Rep_Item (From_Typ));
         end if;
      end if;
   end Inherit_Rep_Item_Chain;

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
        Make_Explicit_Dereference (Sloc (Parent (N)),
          Prefix => New_Prefix));

      Set_Etype (N, Designated_Type (Etype (New_Prefix)));

      if Is_Overloaded (New_Prefix) then

         --  The dereference is also overloaded, and its interpretations are
         --  the designated types of the interpretations of the original node.

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
            Pref := New_Prefix;

         --  For a retrieval of a subcomponent of some composite object,
         --  retrieve the ultimate entity if there is one.

         elsif Nkind_In (New_Prefix, N_Selected_Component,
                                     N_Indexed_Component)
         then
            Pref := Prefix (New_Prefix);
            while Present (Pref)
              and then Nkind_In (Pref, N_Selected_Component,
                                       N_Indexed_Component)
            loop
               Pref := Prefix (Pref);
            end loop;

            if Present (Pref) and then Is_Entity_Name (Pref) then
               Ent := Entity (Pref);
            end if;
         end if;

         --  Place the reference on the entity node

         if Present (Ent) then
            Generate_Reference (Ent, Pref);
         end if;
      end if;
   end Insert_Explicit_Dereference;

   ------------------------------------------
   -- Inspect_Deferred_Constant_Completion --
   ------------------------------------------

   procedure Inspect_Deferred_Constant_Completion (Decls : List_Id) is
      Decl : Node_Id;

   begin
      Decl := First (Decls);
      while Present (Decl) loop

         --  Deferred constant signature

         if Nkind (Decl) = N_Object_Declaration
           and then Constant_Present (Decl)
           and then No (Expression (Decl))

            --  No need to check internally generated constants

           and then Comes_From_Source (Decl)

            --  The constant is not completed. A full object declaration or a
            --  pragma Import complete a deferred constant.

           and then not Has_Completion (Defining_Identifier (Decl))
         then
            Error_Msg_N
              ("constant declaration requires initialization expression",
              Defining_Identifier (Decl));
         end if;

         Decl := Next (Decl);
      end loop;
   end Inspect_Deferred_Constant_Completion;

   -----------------------------
   -- Install_Generic_Formals --
   -----------------------------

   procedure Install_Generic_Formals (Subp_Id : Entity_Id) is
      E : Entity_Id;

   begin
      pragma Assert (Is_Generic_Subprogram (Subp_Id));

      E := First_Entity (Subp_Id);
      while Present (E) loop
         Install_Entity (E);
         Next_Entity (E);
      end loop;
   end Install_Generic_Formals;

   ------------------------
   -- Install_SPARK_Mode --
   ------------------------

   procedure Install_SPARK_Mode (Mode : SPARK_Mode_Type; Prag : Node_Id) is
   begin
      SPARK_Mode        := Mode;
      SPARK_Mode_Pragma := Prag;
   end Install_SPARK_Mode;

   -----------------------------
   -- Is_Actual_Out_Parameter --
   -----------------------------

   function Is_Actual_Out_Parameter (N : Node_Id) return Boolean is
      Formal : Entity_Id;
      Call   : Node_Id;
   begin
      Find_Actual (N, Formal, Call);
      return Present (Formal) and then Ekind (Formal) = E_Out_Parameter;
   end Is_Actual_Out_Parameter;

   -------------------------
   -- Is_Actual_Parameter --
   -------------------------

   function Is_Actual_Parameter (N : Node_Id) return Boolean is
      PK : constant Node_Kind := Nkind (Parent (N));

   begin
      case PK is
         when N_Parameter_Association =>
            return N = Explicit_Actual_Parameter (Parent (N));

         when N_Subprogram_Call =>
            return Is_List_Member (N)
              and then
                List_Containing (N) = Parameter_Associations (Parent (N));

         when others =>
            return False;
      end case;
   end Is_Actual_Parameter;

   --------------------------------
   -- Is_Actual_Tagged_Parameter --
   --------------------------------

   function Is_Actual_Tagged_Parameter (N : Node_Id) return Boolean is
      Formal : Entity_Id;
      Call   : Node_Id;
   begin
      Find_Actual (N, Formal, Call);
      return Present (Formal) and then Is_Tagged_Type (Etype (Formal));
   end Is_Actual_Tagged_Parameter;

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
                      or else Ekind_In (E, E_Generic_In_Out_Parameter,
                                           E_Generic_In_Parameter))
                    and then Is_Tagged_Type (Etype (E)))

           or else (Is_Concurrent_Type (E) and then In_Open_Scopes (E))

           --  Current instance of type, either directly or as rewritten
           --  reference to the current object.

           or else (Is_Entity_Name (Original_Node (Obj))
                     and then Present (Entity (Original_Node (Obj)))
                     and then Is_Type (Entity (Original_Node (Obj))))

           or else (Is_Type (E) and then E = Current_Scope)

           or else (Is_Incomplete_Or_Private_Type (E)
                     and then Full_View (E) = Current_Scope)

           --  Ada 2012 AI05-0053: the return object of an extended return
           --  statement is aliased if its type is immutably limited.

           or else (Is_Return_Object (E)
                     and then Is_Limited_View (Etype (E)));

      elsif Nkind (Obj) = N_Selected_Component then
         return Is_Aliased (Entity (Selector_Name (Obj)));

      elsif Nkind (Obj) = N_Indexed_Component then
         return Has_Aliased_Components (Etype (Prefix (Obj)))
           or else
             (Is_Access_Type (Etype (Prefix (Obj)))
               and then Has_Aliased_Components
                          (Designated_Type (Etype (Prefix (Obj)))));

      elsif Nkind_In (Obj, N_Unchecked_Type_Conversion, N_Type_Conversion) then
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
     (E1 : Entity_Id;
      E2 : Entity_Id) return Boolean
   is
      Par : Entity_Id;

   begin
      Par := E2;
      while Present (Par) and then Par /= Standard_Standard loop
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

      ----------------------
      -- Is_Atomic_Prefix --
      ----------------------

      function Is_Atomic_Prefix (N : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (N)) then
            return
              Has_Atomic_Components (Designated_Type (Etype (N)));
         else
            return Object_Has_Atomic_Components (N);
         end if;
      end Is_Atomic_Prefix;

      ----------------------------------
      -- Object_Has_Atomic_Components --
      ----------------------------------

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

         elsif Nkind (N) = N_Selected_Component
           and then Is_Atomic (Entity (Selector_Name (N)))
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
      --  Predicate is not relevant to subprograms

      if Is_Entity_Name (N) and then Is_Overloadable (Entity (N)) then
         return False;

      elsif Is_Atomic (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Atomic (Entity (N)))
      then
         return True;

      elsif Nkind (N) = N_Selected_Component
        and then Is_Atomic (Entity (Selector_Name (N)))
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

   -----------------------------
   -- Is_Atomic_Or_VFA_Object --
   -----------------------------

   function Is_Atomic_Or_VFA_Object (N : Node_Id) return Boolean is
   begin
      return Is_Atomic_Object (N)
        or else (Is_Object_Reference (N)
                   and then Is_Entity_Name (N)
                   and then (Is_Volatile_Full_Access (Entity (N))
                                or else
                             Is_Volatile_Full_Access (Etype (Entity (N)))));
   end Is_Atomic_Or_VFA_Object;

   -------------------------
   -- Is_Attribute_Result --
   -------------------------

   function Is_Attribute_Result (N : Node_Id) return Boolean is
   begin
      return Nkind (N) = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Result;
   end Is_Attribute_Result;

   -------------------------
   -- Is_Attribute_Update --
   -------------------------

   function Is_Attribute_Update (N : Node_Id) return Boolean is
   begin
      return Nkind (N) = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Update;
   end Is_Attribute_Update;

   ------------------------------------
   -- Is_Body_Or_Package_Declaration --
   ------------------------------------

   function Is_Body_Or_Package_Declaration (N : Node_Id) return Boolean is
   begin
      return Nkind_In (N, N_Entry_Body,
                          N_Package_Body,
                          N_Package_Declaration,
                          N_Protected_Body,
                          N_Subprogram_Body,
                          N_Task_Body);
   end Is_Body_Or_Package_Declaration;

   -----------------------
   -- Is_Bounded_String --
   -----------------------

   function Is_Bounded_String (T : Entity_Id) return Boolean is
      Under : constant Entity_Id := Underlying_Type (Root_Type (T));

   begin
      --  Check whether T is ultimately derived from Ada.Strings.Superbounded.
      --  Super_String, or one of the [Wide_]Wide_ versions. This will
      --  be True for all the Bounded_String types in instances of the
      --  Generic_Bounded_Length generics, and for types derived from those.

      return Present (Under)
        and then (Is_RTE (Root_Type (Under), RO_SU_Super_String) or else
                  Is_RTE (Root_Type (Under), RO_WI_Super_String) or else
                  Is_RTE (Root_Type (Under), RO_WW_Super_String));
   end Is_Bounded_String;

   -------------------------
   -- Is_Child_Or_Sibling --
   -------------------------

   function Is_Child_Or_Sibling
     (Pack_1 : Entity_Id;
      Pack_2 : Entity_Id) return Boolean
   is
      function Distance_From_Standard (Pack : Entity_Id) return Nat;
      --  Given an arbitrary package, return the number of "climbs" necessary
      --  to reach scope Standard_Standard.

      procedure Equalize_Depths
        (Pack           : in out Entity_Id;
         Depth          : in out Nat;
         Depth_To_Reach : Nat);
      --  Given an arbitrary package, its depth and a target depth to reach,
      --  climb the scope chain until the said depth is reached. The pointer
      --  to the package and its depth a modified during the climb.

      ----------------------------
      -- Distance_From_Standard --
      ----------------------------

      function Distance_From_Standard (Pack : Entity_Id) return Nat is
         Dist : Nat;
         Scop : Entity_Id;

      begin
         Dist := 0;
         Scop := Pack;
         while Present (Scop) and then Scop /= Standard_Standard loop
            Dist := Dist + 1;
            Scop := Scope (Scop);
         end loop;

         return Dist;
      end Distance_From_Standard;

      ---------------------
      -- Equalize_Depths --
      ---------------------

      procedure Equalize_Depths
        (Pack           : in out Entity_Id;
         Depth          : in out Nat;
         Depth_To_Reach : Nat)
      is
      begin
         --  The package must be at a greater or equal depth

         if Depth < Depth_To_Reach then
            raise Program_Error;
         end if;

         --  Climb the scope chain until the desired depth is reached

         while Present (Pack) and then Depth /= Depth_To_Reach loop
            Pack  := Scope (Pack);
            Depth := Depth - 1;
         end loop;
      end Equalize_Depths;

      --  Local variables

      P_1       : Entity_Id := Pack_1;
      P_1_Child : Boolean   := False;
      P_1_Depth : Nat       := Distance_From_Standard (P_1);
      P_2       : Entity_Id := Pack_2;
      P_2_Child : Boolean   := False;
      P_2_Depth : Nat       := Distance_From_Standard (P_2);

   --  Start of processing for Is_Child_Or_Sibling

   begin
      pragma Assert
        (Ekind (Pack_1) = E_Package and then Ekind (Pack_2) = E_Package);

      --  Both packages denote the same entity, therefore they cannot be
      --  children or siblings.

      if P_1 = P_2 then
         return False;

      --  One of the packages is at a deeper level than the other. Note that
      --  both may still come from different hierarchies.

      --        (root)           P_2
      --        /    \            :
      --       X     P_2    or    X
      --       :                  :
      --      P_1                P_1

      elsif P_1_Depth > P_2_Depth then
         Equalize_Depths
           (Pack           => P_1,
            Depth          => P_1_Depth,
            Depth_To_Reach => P_2_Depth);
         P_1_Child := True;

      --        (root)           P_1
      --        /    \            :
      --      P_1     X     or    X
      --              :           :
      --             P_2         P_2

      elsif P_2_Depth > P_1_Depth then
         Equalize_Depths
           (Pack           => P_2,
            Depth          => P_2_Depth,
            Depth_To_Reach => P_1_Depth);
         P_2_Child := True;
      end if;

      --  At this stage the package pointers have been elevated to the same
      --  depth. If the related entities are the same, then one package is a
      --  potential child of the other:

      --      P_1
      --       :
      --       X    became   P_1 P_2   or vice versa
      --       :
      --      P_2

      if P_1 = P_2 then
         if P_1_Child then
            return Is_Child_Unit (Pack_1);

         else pragma Assert (P_2_Child);
            return Is_Child_Unit (Pack_2);
         end if;

      --  The packages may come from the same package chain or from entirely
      --  different hierarcies. To determine this, climb the scope stack until
      --  a common root is found.

      --        (root)      (root 1)  (root 2)
      --        /    \         |         |
      --      P_1    P_2      P_1       P_2

      else
         while Present (P_1) and then Present (P_2) loop

            --  The two packages may be siblings

            if P_1 = P_2 then
               return Is_Child_Unit (Pack_1) and then Is_Child_Unit (Pack_2);
            end if;

            P_1 := Scope (P_1);
            P_2 := Scope (P_2);
         end loop;
      end if;

      return False;
   end Is_Child_Or_Sibling;

   -----------------------------
   -- Is_Concurrent_Interface --
   -----------------------------

   function Is_Concurrent_Interface (T : Entity_Id) return Boolean is
   begin
      return Is_Interface (T)
        and then
          (Is_Protected_Interface (T)
            or else Is_Synchronized_Interface (T)
            or else Is_Task_Interface (T));
   end Is_Concurrent_Interface;

   -----------------------
   -- Is_Constant_Bound --
   -----------------------

   function Is_Constant_Bound (Exp : Node_Id) return Boolean is
   begin
      if Compile_Time_Known_Value (Exp) then
         return True;

      elsif Is_Entity_Name (Exp) and then Present (Entity (Exp)) then
         return Is_Constant_Object (Entity (Exp))
           or else Ekind (Entity (Exp)) = E_Enumeration_Literal;

      elsif Nkind (Exp) in N_Binary_Op then
         return Is_Constant_Bound (Left_Opnd (Exp))
           and then Is_Constant_Bound (Right_Opnd (Exp))
           and then Scope (Entity (Exp)) = Standard_Standard;

      else
         return False;
      end if;
   end Is_Constant_Bound;

   ---------------------------
   --  Is_Container_Element --
   ---------------------------

   function Is_Container_Element (Exp : Node_Id) return Boolean is
      Loc  : constant Source_Ptr := Sloc (Exp);
      Pref : constant Node_Id   := Prefix (Exp);

      Call : Node_Id;
      --  Call to an indexing aspect

      Cont_Typ : Entity_Id;
      --  The type of the container being accessed

      Elem_Typ : Entity_Id;
      --  Its element type

      Indexing : Entity_Id;
      Is_Const : Boolean;
      --  Indicates that constant indexing is used, and the element is thus
      --  a constant.

      Ref_Typ : Entity_Id;
      --  The reference type returned by the indexing operation

   begin
      --  If C is a container, in a context that imposes the element type of
      --  that container, the indexing notation C (X) is rewritten as:

      --    Indexing (C, X).Discr.all

      --  where Indexing is one of the indexing aspects of the container.
      --  If the context does not require a reference, the construct can be
      --  rewritten as

      --    Element (C, X)

      --  First, verify that the construct has the proper form

      if not Expander_Active then
         return False;

      elsif Nkind (Pref) /= N_Selected_Component then
         return False;

      elsif Nkind (Prefix (Pref)) /= N_Function_Call then
         return False;

      else
         Call    := Prefix (Pref);
         Ref_Typ := Etype (Call);
      end if;

      if not Has_Implicit_Dereference (Ref_Typ)
        or else No (First (Parameter_Associations (Call)))
        or else not Is_Entity_Name (Name (Call))
      then
         return False;
      end if;

      --  Retrieve type of container object, and its iterator aspects

      Cont_Typ := Etype (First (Parameter_Associations (Call)));
      Indexing := Find_Value_Of_Aspect (Cont_Typ, Aspect_Constant_Indexing);
      Is_Const := False;

      if No (Indexing) then

         --  Container should have at least one indexing operation

         return False;

      elsif Entity (Name (Call)) /= Entity (Indexing) then

         --  This may be a variable indexing operation

         Indexing := Find_Value_Of_Aspect (Cont_Typ, Aspect_Variable_Indexing);

         if No (Indexing)
           or else Entity (Name (Call)) /= Entity (Indexing)
         then
            return False;
         end if;

      else
         Is_Const := True;
      end if;

      Elem_Typ := Find_Value_Of_Aspect (Cont_Typ, Aspect_Iterator_Element);

      if No (Elem_Typ) or else Entity (Elem_Typ) /= Etype (Exp) then
         return False;
      end if;

      --  Check that the expression is not the target of an assignment, in
      --  which case the rewriting is not possible.

      if not Is_Const then
         declare
            Par : Node_Id;

         begin
            Par := Exp;
            while Present (Par)
            loop
               if Nkind (Parent (Par)) = N_Assignment_Statement
                 and then Par = Name (Parent (Par))
               then
                  return False;

               --  A renaming produces a reference, and the transformation
               --  does not apply.

               elsif Nkind (Parent (Par)) = N_Object_Renaming_Declaration then
                  return False;

               elsif Nkind_In
                 (Nkind (Parent (Par)), N_Function_Call,
                                        N_Procedure_Call_Statement,
                                        N_Entry_Call_Statement)
               then
                  --  Check that the element is not part of an actual for an
                  --  in-out parameter.

                  declare
                     F : Entity_Id;
                     A : Node_Id;

                  begin
                     F := First_Formal (Entity (Name (Parent (Par))));
                     A := First (Parameter_Associations (Parent (Par)));
                     while Present (F) loop
                        if A = Par and then Ekind (F) /= E_In_Parameter then
                           return False;
                        end if;

                        Next_Formal (F);
                        Next (A);
                     end loop;
                  end;

                  --  E_In_Parameter in a call: element is not modified.

                  exit;
               end if;

               Par := Parent (Par);
            end loop;
         end;
      end if;

      --  The expression has the proper form and the context requires the
      --  element type. Retrieve the Element function of the container and
      --  rewrite the construct as a call to it.

      declare
         Op : Elmt_Id;

      begin
         Op := First_Elmt (Primitive_Operations (Cont_Typ));
         while Present (Op) loop
            exit when Chars (Node (Op)) = Name_Element;
            Next_Elmt (Op);
         end loop;

         if No (Op) then
            return False;

         else
            Rewrite (Exp,
              Make_Function_Call (Loc,
                Name                   => New_Occurrence_Of (Node (Op), Loc),
                Parameter_Associations => Parameter_Associations (Call)));
            Analyze_And_Resolve (Exp, Entity (Elem_Typ));
            return True;
         end if;
      end;
   end Is_Container_Element;

   ----------------------------
   -- Is_Contract_Annotation --
   ----------------------------

   function Is_Contract_Annotation (Item : Node_Id) return Boolean is
   begin
      return Is_Package_Contract_Annotation (Item)
               or else
             Is_Subprogram_Contract_Annotation (Item);
   end Is_Contract_Annotation;

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

   -----------------------------
   -- Is_CPP_Constructor_Call --
   -----------------------------

   function Is_CPP_Constructor_Call (N : Node_Id) return Boolean is
   begin
      return Nkind (N) = N_Function_Call
        and then Is_CPP_Class (Etype (Etype (N)))
        and then Is_Constructor (Entity (Name (N)))
        and then Is_Imported (Entity (Name (N)));
   end Is_CPP_Constructor_Call;

   -------------------------
   -- Is_Current_Instance --
   -------------------------

   function Is_Current_Instance (N : Node_Id) return Boolean is
      Typ : constant Entity_Id := Entity (N);
      P   : Node_Id;

   begin
      --  Simplest case: entity is a concurrent type and we are currently
      --  inside the body. This will eventually be expanded into a
      --  call to Self (for tasks) or _object (for protected objects).

      if Is_Concurrent_Type (Typ) and then In_Open_Scopes (Typ) then
         return True;

      else
         --  Check whether the context is a (sub)type declaration for the
         --  type entity.

         P := Parent (N);
         while Present (P) loop
            if Nkind_In (P, N_Full_Type_Declaration,
                            N_Private_Type_Declaration,
                            N_Subtype_Declaration)
              and then Comes_From_Source (P)
              and then Defining_Entity (P) = Typ
            then
               return True;

            --  A subtype name may appear in an aspect specification for a
            --  Predicate_Failure aspect, for which we do not construct a
            --  wrapper procedure. The subtype will be replaced by the
            --  expression being tested when the corresponding predicate
            --  check is expanded.

            elsif Nkind (P) = N_Aspect_Specification
              and then Nkind (Parent (P)) = N_Subtype_Declaration
            then
               return True;

            elsif Nkind (P) = N_Pragma
              and then
                Get_Pragma_Id (P) = Pragma_Predicate_Failure
            then
               return True;
            end if;

            P := Parent (P);
         end loop;
      end if;

      --  In any other context this is not a current occurrence

      return False;
   end Is_Current_Instance;

   --------------------
   -- Is_Declaration --
   --------------------

   function Is_Declaration (N : Node_Id) return Boolean is
   begin
      return
        Is_Declaration_Other_Than_Renaming (N)
          or else Is_Renaming_Declaration (N);
   end Is_Declaration;

   ----------------------------------------
   -- Is_Declaration_Other_Than_Renaming --
   ----------------------------------------

   function Is_Declaration_Other_Than_Renaming (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Abstract_Subprogram_Declaration
            | N_Exception_Declaration
            | N_Expression_Function
            | N_Full_Type_Declaration
            | N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
            | N_Number_Declaration
            | N_Object_Declaration
            | N_Package_Declaration
            | N_Private_Extension_Declaration
            | N_Private_Type_Declaration
            | N_Subprogram_Declaration
            | N_Subtype_Declaration
         =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Declaration_Other_Than_Renaming;

   --------------------------------
   -- Is_Declared_Within_Variant --
   --------------------------------

   function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean is
      Comp_Decl : constant Node_Id := Parent (Comp);
      Comp_List : constant Node_Id := Parent (Comp_Decl);
   begin
      return Nkind (Parent (Comp_List)) = N_Variant;
   end Is_Declared_Within_Variant;

   ----------------------------------------------
   -- Is_Dependent_Component_Of_Mutable_Object --
   ----------------------------------------------

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id) return Boolean
   is
      P           : Node_Id;
      Prefix_Type : Entity_Id;
      P_Aliased   : Boolean := False;
      Comp        : Entity_Id;

      Deref : Node_Id := Object;
      --  Dereference node, in something like X.all.Y(2)

   --  Start of processing for Is_Dependent_Component_Of_Mutable_Object

   begin
      --  Find the dereference node if any

      while Nkind_In (Deref, N_Indexed_Component,
                             N_Selected_Component,
                             N_Slice)
      loop
         Deref := Prefix (Deref);
      end loop;

      --  Ada 2005: If we have a component or slice of a dereference,
      --  something like X.all.Y (2), and the type of X is access-to-constant,
      --  Is_Variable will return False, because it is indeed a constant
      --  view. But it might be a view of a variable object, so we want the
      --  following condition to be True in that case.

      if Is_Variable (Object)
        or else (Ada_Version >= Ada_2005
                  and then Nkind (Deref) = N_Explicit_Dereference)
      then
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

            --  A discriminant check on a selected component may be expanded
            --  into a dereference when removing side-effects. Recover the
            --  original node and its type, which may be unconstrained.

            elsif Nkind (P) = N_Explicit_Dereference
              and then not (Comes_From_Source (P))
            then
               P := Original_Node (P);
               Prefix_Type := Etype (P);

            else
               --  Check for prefix being an aliased component???

               null;

            end if;

            --  A heap object is constrained by its initial value

            --  Ada 2005 (AI-363): Always assume the object could be mutable in
            --  the dereferenced case, since the access value might denote an
            --  unconstrained aliased object, whereas in Ada 95 the designated
            --  object is guaranteed to be constrained. A worst-case assumption
            --  has to apply in Ada 2005 because we can't tell at compile
            --  time whether the object is "constrained by its initial value",
            --  despite the fact that 3.10.2(26/2) and 8.5.1(5/2) are semantic
            --  rules (these rules are acknowledged to need fixing). We don't
            --  impose this more stringent checking for earlier Ada versions or
            --  when Relaxed_RM_Semantics applies (the latter for CodePeer's
            --  benefit, though it's unclear on why using -gnat95 would not be
            --  sufficient???).

            if Ada_Version < Ada_2005 or else Relaxed_RM_Semantics then
               if Is_Access_Type (Prefix_Type)
                 or else Nkind (P) = N_Explicit_Dereference
               then
                  return False;
               end if;

            else pragma Assert (Ada_Version >= Ada_2005);
               if Is_Access_Type (Prefix_Type) then

                  --  If the access type is pool-specific, and there is no
                  --  constrained partial view of the designated type, then the
                  --  designated object is known to be constrained.

                  if Ekind (Prefix_Type) = E_Access_Type
                    and then not Object_Type_Has_Constrained_Partial_View
                                   (Typ  => Designated_Type (Prefix_Type),
                                    Scop => Current_Scope)
                  then
                     return False;

                  --  Otherwise (general access type, or there is a constrained
                  --  partial view of the designated type), we need to check
                  --  based on the designated type.

                  else
                     Prefix_Type := Designated_Type (Prefix_Type);
                  end if;
               end if;
            end if;

            Comp :=
              Original_Record_Component (Entity (Selector_Name (Object)));

            --  As per AI-0017, the renaming is illegal in a generic body, even
            --  if the subtype is indefinite.

            --  Ada 2005 (AI-363): In Ada 2005 an aliased object can be mutable

            if not Is_Constrained (Prefix_Type)
              and then (Is_Definite_Subtype (Prefix_Type)
                         or else
                           (Is_Generic_Type (Prefix_Type)
                             and then Ekind (Current_Scope) = E_Generic_Package
                             and then In_Package_Body (Current_Scope)))

              and then (Is_Declared_Within_Variant (Comp)
                         or else Has_Discriminant_Dependent_Constraint (Comp))
              and then (not P_Aliased or else Ada_Version >= Ada_2005)
            then
               return True;

            --  If the prefix is of an access type at this point, then we want
            --  to return False, rather than calling this function recursively
            --  on the access object (which itself might be a discriminant-
            --  dependent component of some other object, but that isn't
            --  relevant to checking the object passed to us). This avoids
            --  issuing wrong errors when compiling with -gnatc, where there
            --  can be implicit dereferences that have not been expanded.

            elsif Is_Access_Type (Etype (Prefix (Object))) then
               return False;

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
      return Nkind_In (P, N_Selected_Component,
                          N_Explicit_Dereference,
                          N_Indexed_Component,
                          N_Slice)
        and then Prefix (P) = N;
   end Is_Dereferenced;

   ----------------------
   -- Is_Descendant_Of --
   ----------------------

   function Is_Descendant_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
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
   end Is_Descendant_Of;

   ----------------------------------------
   -- Is_Descendant_Of_Suspension_Object --
   ----------------------------------------

   function Is_Descendant_Of_Suspension_Object
     (Typ : Entity_Id) return Boolean
   is
      Cur_Typ : Entity_Id;
      Par_Typ : Entity_Id;

   begin
      --  Climb the type derivation chain checking each parent type against
      --  Suspension_Object.

      Cur_Typ := Base_Type (Typ);
      while Present (Cur_Typ) loop
         Par_Typ := Etype (Cur_Typ);

         --  The current type is a match

         if Is_Suspension_Object (Cur_Typ) then
            return True;

         --  Stop the traversal once the root of the derivation chain has been
         --  reached. In that case the current type is its own base type.

         elsif Cur_Typ = Par_Typ then
            exit;
         end if;

         Cur_Typ := Base_Type (Par_Typ);
      end loop;

      return False;
   end Is_Descendant_Of_Suspension_Object;

   ---------------------------------------------
   -- Is_Double_Precision_Floating_Point_Type --
   ---------------------------------------------

   function Is_Double_Precision_Floating_Point_Type
     (E : Entity_Id) return Boolean is
   begin
      return Is_Floating_Point_Type (E)
        and then Machine_Radix_Value (E) = Uint_2
        and then Machine_Mantissa_Value (E) = UI_From_Int (53)
        and then Machine_Emax_Value (E) = Uint_2 ** Uint_10
        and then Machine_Emin_Value (E) = Uint_3 - (Uint_2 ** Uint_10);
   end Is_Double_Precision_Floating_Point_Type;

   -----------------------------
   -- Is_Effectively_Volatile --
   -----------------------------

   function Is_Effectively_Volatile (Id : Entity_Id) return Boolean is
   begin
      if Is_Type (Id) then

         --  An arbitrary type is effectively volatile when it is subject to
         --  pragma Atomic or Volatile.

         if Is_Volatile (Id) then
            return True;

         --  An array type is effectively volatile when it is subject to pragma
         --  Atomic_Components or Volatile_Components or its component type is
         --  effectively volatile.

         elsif Is_Array_Type (Id) then
            declare
               Anc : Entity_Id := Base_Type (Id);
            begin
               if Is_Private_Type (Anc) then
                  Anc := Full_View (Anc);
               end if;

               --  Test for presence of ancestor, as the full view of a private
               --  type may be missing in case of error.

               return
                 Has_Volatile_Components (Id)
                   or else
                 (Present (Anc)
                   and then Is_Effectively_Volatile (Component_Type (Anc)));
            end;

         --  A protected type is always volatile

         elsif Is_Protected_Type (Id) then
            return True;

         --  A descendant of Ada.Synchronous_Task_Control.Suspension_Object is
         --  automatically volatile.

         elsif Is_Descendant_Of_Suspension_Object (Id) then
            return True;

         --  Otherwise the type is not effectively volatile

         else
            return False;
         end if;

      --  Otherwise Id denotes an object

      else
         return
           Is_Volatile (Id)
             or else Has_Volatile_Components (Id)
             or else Is_Effectively_Volatile (Etype (Id));
      end if;
   end Is_Effectively_Volatile;

   ------------------------------------
   -- Is_Effectively_Volatile_Object --
   ------------------------------------

   function Is_Effectively_Volatile_Object (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Is_Effectively_Volatile (Entity (N));

      elsif Nkind (N) = N_Indexed_Component then
         return Is_Effectively_Volatile_Object (Prefix (N));

      elsif Nkind (N) = N_Selected_Component then
         return
           Is_Effectively_Volatile_Object (Prefix (N))
             or else
           Is_Effectively_Volatile_Object (Selector_Name (N));

      else
         return False;
      end if;
   end Is_Effectively_Volatile_Object;

   -------------------
   -- Is_Entry_Body --
   -------------------

   function Is_Entry_Body (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind_In (Id, E_Entry, E_Entry_Family)
          and then Nkind (Unit_Declaration_Node (Id)) = N_Entry_Body;
   end Is_Entry_Body;

   --------------------------
   -- Is_Entry_Declaration --
   --------------------------

   function Is_Entry_Declaration (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind_In (Id, E_Entry, E_Entry_Family)
          and then Nkind (Unit_Declaration_Node (Id)) = N_Entry_Declaration;
   end Is_Entry_Declaration;

   ------------------------------------
   -- Is_Expanded_Priority_Attribute --
   ------------------------------------

   function Is_Expanded_Priority_Attribute (E : Entity_Id) return Boolean is
   begin
      return
        Nkind (E) = N_Function_Call
          and then not Configurable_Run_Time_Mode
          and then (Entity (Name (E)) = RTE (RE_Get_Ceiling)
                     or else Entity (Name (E)) = RTE (RO_PE_Get_Ceiling));
   end Is_Expanded_Priority_Attribute;

   ----------------------------
   -- Is_Expression_Function --
   ----------------------------

   function Is_Expression_Function (Subp : Entity_Id) return Boolean is
   begin
      if Ekind_In (Subp, E_Function, E_Subprogram_Body) then
         return
           Nkind (Original_Node (Unit_Declaration_Node (Subp))) =
             N_Expression_Function;
      else
         return False;
      end if;
   end Is_Expression_Function;

   ------------------------------------------
   -- Is_Expression_Function_Or_Completion --
   ------------------------------------------

   function Is_Expression_Function_Or_Completion
     (Subp : Entity_Id) return Boolean
   is
      Subp_Decl : Node_Id;

   begin
      if Ekind (Subp) = E_Function then
         Subp_Decl := Unit_Declaration_Node (Subp);

         --  The function declaration is either an expression function or is
         --  completed by an expression function body.

         return
           Is_Expression_Function (Subp)
             or else (Nkind (Subp_Decl) = N_Subprogram_Declaration
                       and then Present (Corresponding_Body (Subp_Decl))
                       and then Is_Expression_Function
                                  (Corresponding_Body (Subp_Decl)));

      elsif Ekind (Subp) = E_Subprogram_Body then
         return Is_Expression_Function (Subp);

      else
         return False;
      end if;
   end Is_Expression_Function_Or_Completion;

   -----------------------
   -- Is_EVF_Expression --
   -----------------------

   function Is_EVF_Expression (N : Node_Id) return Boolean is
      Orig_N : constant Node_Id := Original_Node (N);
      Alt    : Node_Id;
      Expr   : Node_Id;
      Id     : Entity_Id;

   begin
      --  Detect a reference to a formal parameter of a specific tagged type
      --  whose related subprogram is subject to pragma Expresions_Visible with
      --  value "False".

      if Is_Entity_Name (N) and then Present (Entity (N)) then
         Id := Entity (N);

         return
           Is_Formal (Id)
             and then Is_Specific_Tagged_Type (Etype (Id))
             and then Extensions_Visible_Status (Id) =
                      Extensions_Visible_False;

      --  A case expression is an EVF expression when it contains at least one
      --  EVF dependent_expression. Note that a case expression may have been
      --  expanded, hence the use of Original_Node.

      elsif Nkind (Orig_N) = N_Case_Expression then
         Alt := First (Alternatives (Orig_N));
         while Present (Alt) loop
            if Is_EVF_Expression (Expression (Alt)) then
               return True;
            end if;

            Next (Alt);
         end loop;

      --  An if expression is an EVF expression when it contains at least one
      --  EVF dependent_expression. Note that an if expression may have been
      --  expanded, hence the use of Original_Node.

      elsif Nkind (Orig_N) = N_If_Expression then
         Expr := Next (First (Expressions (Orig_N)));
         while Present (Expr) loop
            if Is_EVF_Expression (Expr) then
               return True;
            end if;

            Next (Expr);
         end loop;

      --  A qualified expression or a type conversion is an EVF expression when
      --  its operand is an EVF expression.

      elsif Nkind_In (N, N_Qualified_Expression,
                         N_Unchecked_Type_Conversion,
                         N_Type_Conversion)
      then
         return Is_EVF_Expression (Expression (N));

      --  Attributes 'Loop_Entry, 'Old, and 'Update are EVF expressions when
      --  their prefix denotes an EVF expression.

      elsif Nkind (N) = N_Attribute_Reference
        and then Nam_In (Attribute_Name (N), Name_Loop_Entry,
                                             Name_Old,
                                             Name_Update)
      then
         return Is_EVF_Expression (Prefix (N));
      end if;

      return False;
   end Is_EVF_Expression;

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
      --  Scalar types

      if Is_Scalar_Type (Typ) then

         --  A scalar type with an aspect Default_Value is fully initialized

         --  Note: Iniitalize/Normalize_Scalars also ensure full initialization
         --  of a scalar type, but we don't take that into account here, since
         --  we don't want these to affect warnings.

         return Has_Default_Aspect (Typ);

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         if Is_Fully_Initialized_Type (Component_Type (Typ))
           or else (Ada_Version >= Ada_2012 and then Has_Default_Aspect (Typ))
         then
            return True;
         end if;

         --  An interesting case, if we have a constrained type one of whose
         --  bounds is known to be null, then there are no elements to be
         --  initialized, so all the elements are initialized.

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

                     if Is_Private_Type (Indx_Typ) then
                        Indx_Typ := Full_View (Indx_Typ);
                     end if;

                     if No (Indx_Typ) or else Etype (Indx_Typ) = Any_Type then
                        return False;
                     else
                        Lbd := Type_Low_Bound  (Indx_Typ);
                        Hbd := Type_High_Bound (Indx_Typ);
                     end if;
                  end if;

                  if Compile_Time_Known_Value (Lbd)
                       and then
                     Compile_Time_Known_Value (Hbd)
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

         --  We consider bounded string types to be fully initialized, because
         --  otherwise we get false alarms when the Data component is not
         --  default-initialized.

         if Is_Bounded_String (Typ) then
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
                              (Find_Optional_Prim_Op
                                 (Underlying_Type (Typ), Name_Initialize));

                  begin
                     if Present (Init)
                       and then Comes_From_Source (Init)
                       and then not In_Predefined_Unit (Init)
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
               if Ekind (Ent) = E_Component
                 and then (No (Parent (Ent))
                            or else No (Expression (Parent (Ent))))
                 and then not Is_Fully_Initialized_Type (Etype (Ent))

                  --  Special VM case for tag components, which need to be
                  --  defined in this case, but are never initialized as VMs
                  --  are using other dispatching mechanisms. Ignore this
                  --  uninitialized case. Note that this applies both to the
                  --  uTag entry and the main vtable pointer (CPP_Class case).

                 and then (Tagged_Type_Expansion or else not Is_Tag (Ent))
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
      pragma Warnings (Off, Report_Errors);

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

   ------------------------------------
   -- Is_Generic_Declaration_Or_Body --
   ------------------------------------

   function Is_Generic_Declaration_Or_Body (Decl : Node_Id) return Boolean is
      Spec_Decl : Node_Id;

   begin
      --  Package/subprogram body

      if Nkind_In (Decl, N_Package_Body, N_Subprogram_Body)
        and then Present (Corresponding_Spec (Decl))
      then
         Spec_Decl := Unit_Declaration_Node (Corresponding_Spec (Decl));

      --  Package/subprogram body stub

      elsif Nkind_In (Decl, N_Package_Body_Stub, N_Subprogram_Body_Stub)
        and then Present (Corresponding_Spec_Of_Stub (Decl))
      then
         Spec_Decl :=
           Unit_Declaration_Node (Corresponding_Spec_Of_Stub (Decl));

      --  All other cases

      else
         Spec_Decl := Decl;
      end if;

      --  Rather than inspecting the defining entity of the spec declaration,
      --  look at its Nkind. This takes care of the case where the analysis of
      --  a generic body modifies the Ekind of its spec to allow for recursive
      --  calls.

      return
        Nkind_In (Spec_Decl, N_Generic_Package_Declaration,
                             N_Generic_Subprogram_Declaration);
   end Is_Generic_Declaration_Or_Body;

   ----------------------------
   -- Is_Inherited_Operation --
   ----------------------------

   function Is_Inherited_Operation (E : Entity_Id) return Boolean is
      pragma Assert (Is_Overloadable (E));
      Kind : constant Node_Kind := Nkind (Parent (E));
   begin
      return Kind = N_Full_Type_Declaration
        or else Kind = N_Private_Extension_Declaration
        or else Kind = N_Subtype_Declaration
        or else (Ekind (E) = E_Enumeration_Literal
                  and then Is_Derived_Type (Etype (E)));
   end Is_Inherited_Operation;

   -------------------------------------
   -- Is_Inherited_Operation_For_Type --
   -------------------------------------

   function Is_Inherited_Operation_For_Type
     (E   : Entity_Id;
      Typ : Entity_Id) return Boolean
   is
   begin
      --  Check that the operation has been created by the type declaration

      return Is_Inherited_Operation (E)
        and then Defining_Identifier (Parent (E)) = Typ;
   end Is_Inherited_Operation_For_Type;

   --------------------------------------
   -- Is_Inlinable_Expression_Function --
   --------------------------------------

   function Is_Inlinable_Expression_Function
     (Subp : Entity_Id) return Boolean
   is
      Return_Expr : Node_Id;

   begin
      if Is_Expression_Function_Or_Completion (Subp)
        and then Has_Pragma_Inline_Always (Subp)
        and then Needs_No_Actuals (Subp)
        and then No (Contract (Subp))
        and then not Is_Dispatching_Operation (Subp)
        and then Needs_Finalization (Etype (Subp))
        and then not Is_Class_Wide_Type (Etype (Subp))
        and then not (Has_Invariants (Etype (Subp)))
        and then Present (Subprogram_Body (Subp))
        and then Was_Expression_Function (Subprogram_Body (Subp))
      then
         Return_Expr := Expression_Of_Expression_Function (Subp);

         --  The returned object must not have a qualified expression and its
         --  nominal subtype must be statically compatible with the result
         --  subtype of the expression function.

         return
           Nkind (Return_Expr) = N_Identifier
             and then Etype (Return_Expr) = Etype (Subp);
      end if;

      return False;
   end Is_Inlinable_Expression_Function;

   -----------------
   -- Is_Iterator --
   -----------------

   function Is_Iterator (Typ : Entity_Id) return Boolean is
      function Denotes_Iterator (Iter_Typ : Entity_Id) return Boolean;
      --  Determine whether type Iter_Typ is a predefined forward or reversible
      --  iterator.

      ----------------------
      -- Denotes_Iterator --
      ----------------------

      function Denotes_Iterator (Iter_Typ : Entity_Id) return Boolean is
      begin
         --  Check that the name matches, and that the ultimate ancestor is in
         --  a predefined unit, i.e the one that declares iterator interfaces.

         return
           Nam_In (Chars (Iter_Typ), Name_Forward_Iterator,
                                     Name_Reversible_Iterator)
             and then In_Predefined_Unit (Root_Type (Iter_Typ));
      end Denotes_Iterator;

      --  Local variables

      Iface_Elmt : Elmt_Id;
      Ifaces     : Elist_Id;

   --  Start of processing for Is_Iterator

   begin
      --  The type may be a subtype of a descendant of the proper instance of
      --  the predefined interface type, so we must use the root type of the
      --  given type. The same is done for Is_Reversible_Iterator.

      if Is_Class_Wide_Type (Typ)
        and then Denotes_Iterator (Root_Type (Typ))
      then
         return True;

      elsif not Is_Tagged_Type (Typ) or else not Is_Derived_Type (Typ) then
         return False;

      elsif Present (Find_Value_Of_Aspect (Typ, Aspect_Iterable)) then
         return True;

      else
         Collect_Interfaces (Typ, Ifaces);

         Iface_Elmt := First_Elmt (Ifaces);
         while Present (Iface_Elmt) loop
            if Denotes_Iterator (Node (Iface_Elmt)) then
               return True;
            end if;

            Next_Elmt (Iface_Elmt);
         end loop;

         return False;
      end if;
   end Is_Iterator;

   ----------------------------
   -- Is_Iterator_Over_Array --
   ----------------------------

   function Is_Iterator_Over_Array (N : Node_Id) return Boolean is
      Container     : constant Node_Id   := Name (N);
      Container_Typ : constant Entity_Id := Base_Type (Etype (Container));
   begin
      return Is_Array_Type (Container_Typ);
   end Is_Iterator_Over_Array;

   ------------
   -- Is_LHS --
   ------------

   --  We seem to have a lot of overlapping functions that do similar things
   --  (testing for left hand sides or lvalues???).

   function Is_LHS (N : Node_Id) return Is_LHS_Result is
      P : constant Node_Id := Parent (N);

   begin
      --  Return True if we are the left hand side of an assignment statement

      if Nkind (P) = N_Assignment_Statement then
         if Name (P) = N then
            return Yes;
         else
            return No;
         end if;

      --  Case of prefix of indexed or selected component or slice

      elsif Nkind_In (P, N_Indexed_Component, N_Selected_Component, N_Slice)
        and then N = Prefix (P)
      then
         --  Here we have the case where the parent P is N.Q or N(Q .. R).
         --  If P is an LHS, then N is also effectively an LHS, but there
         --  is an important exception. If N is of an access type, then
         --  what we really have is N.all.Q (or N.all(Q .. R)). In either
         --  case this makes N.all a left hand side but not N itself.

         --  If we don't know the type yet, this is the case where we return
         --  Unknown, since the answer depends on the type which is unknown.

         if No (Etype (N)) then
            return Unknown;

         --  We have an Etype set, so we can check it

         elsif Is_Access_Type (Etype (N)) then
            return No;

         --  OK, not access type case, so just test whole expression

         else
            return Is_LHS (P);
         end if;

      --  All other cases are not left hand sides

      else
         return No;
      end if;
   end Is_LHS;

   -----------------------------
   -- Is_Library_Level_Entity --
   -----------------------------

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean is
   begin
      --  The following is a small optimization, and it also properly handles
      --  discriminals, which in task bodies might appear in expressions before
      --  the corresponding procedure has been created, and which therefore do
      --  not have an assigned scope.

      if Is_Formal (E) then
         return False;
      end if;

      --  Normal test is simply that the enclosing dynamic scope is Standard

      return Enclosing_Dynamic_Scope (E) = Standard_Standard;
   end Is_Library_Level_Entity;

   --------------------------------
   -- Is_Limited_Class_Wide_Type --
   --------------------------------

   function Is_Limited_Class_Wide_Type (Typ : Entity_Id) return Boolean is
   begin
      return
        Is_Class_Wide_Type (Typ)
          and then (Is_Limited_Type (Typ) or else From_Limited_With (Typ));
   end Is_Limited_Class_Wide_Type;

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
            if not Ekind_In (Ent, E_Variable, E_In_Out_Parameter) then
               return False;
            else
               return Present (Sub) and then Sub = Current_Subprogram;
            end if;
         end;
      end if;
   end Is_Local_Variable_Reference;

   -----------------------
   -- Is_Name_Reference --
   -----------------------

   function Is_Name_Reference (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Present (Entity (N)) and then Is_Object (Entity (N));
      end if;

      case Nkind (N) is
         when N_Indexed_Component
            | N_Slice
         =>
            return
              Is_Name_Reference (Prefix (N))
                or else Is_Access_Type (Etype (Prefix (N)));

         --  Attributes 'Input, 'Old and 'Result produce objects

         when N_Attribute_Reference =>
            return
              Nam_In (Attribute_Name (N), Name_Input, Name_Old, Name_Result);

         when N_Selected_Component =>
            return
              Is_Name_Reference (Selector_Name (N))
                and then
                  (Is_Name_Reference (Prefix (N))
                    or else Is_Access_Type (Etype (Prefix (N))));

         when N_Explicit_Dereference =>
            return True;

         --  A view conversion of a tagged name is a name reference

         when N_Type_Conversion =>
            return
              Is_Tagged_Type (Etype (Subtype_Mark (N)))
                and then Is_Tagged_Type (Etype (Expression (N)))
                and then Is_Name_Reference (Expression (N));

         --  An unchecked type conversion is considered to be a name if the
         --  operand is a name (this construction arises only as a result of
         --  expansion activities).

         when N_Unchecked_Type_Conversion =>
            return Is_Name_Reference (Expression (N));

         when others =>
            return False;
      end case;
   end Is_Name_Reference;

   ---------------------------------
   -- Is_Nontrivial_DIC_Procedure --
   ---------------------------------

   function Is_Nontrivial_DIC_Procedure (Id : Entity_Id) return Boolean is
      Body_Decl : Node_Id;
      Stmt      : Node_Id;

   begin
      if Ekind (Id) = E_Procedure and then Is_DIC_Procedure (Id) then
         Body_Decl :=
           Unit_Declaration_Node
             (Corresponding_Body (Unit_Declaration_Node (Id)));

         --  The body of the Default_Initial_Condition procedure must contain
         --  at least one statement, otherwise the generation of the subprogram
         --  body failed.

         pragma Assert (Present (Handled_Statement_Sequence (Body_Decl)));

         --  To qualify as nontrivial, the first statement of the procedure
         --  must be a check in the form of an if statement. If the original
         --  Default_Initial_Condition expression was folded, then the first
         --  statement is not a check.

         Stmt := First (Statements (Handled_Statement_Sequence (Body_Decl)));

         return
           Nkind (Stmt) = N_If_Statement
             and then Nkind (Original_Node (Stmt)) = N_Pragma;
      end if;

      return False;
   end Is_Nontrivial_DIC_Procedure;

   -------------------------
   -- Is_Null_Record_Type --
   -------------------------

   function Is_Null_Record_Type (T : Entity_Id) return Boolean is
      Decl : constant Node_Id := Parent (T);
   begin
      return Nkind (Decl) = N_Full_Type_Declaration
        and then Nkind (Type_Definition (Decl)) = N_Record_Definition
        and then
          (No (Component_List (Type_Definition (Decl)))
            or else Null_Present (Component_List (Type_Definition (Decl))));
   end Is_Null_Record_Type;

   ---------------------
   -- Is_Object_Image --
   ---------------------

   function Is_Object_Image (Prefix : Node_Id) return Boolean is
   begin
      --  When the type of the prefix is not scalar then the prefix is not
      --  valid in any scenario.

      if not Is_Scalar_Type (Etype (Prefix)) then
         return False;
      end if;

      --  Here we test for the case that the prefix is not a type and assume
      --  if it is not then it must be a named value or an object reference.
      --  This is because the parser always checks that prefixes of attributes
      --  are named.

      return not (Is_Entity_Name (Prefix) and then Is_Type (Entity (Prefix)));
   end Is_Object_Image;

   -------------------------
   -- Is_Object_Reference --
   -------------------------

   function Is_Object_Reference (N : Node_Id) return Boolean is
      function Is_Internally_Generated_Renaming (N : Node_Id) return Boolean;
      --  Determine whether N is the name of an internally-generated renaming

      --------------------------------------
      -- Is_Internally_Generated_Renaming --
      --------------------------------------

      function Is_Internally_Generated_Renaming (N : Node_Id) return Boolean is
         P : Node_Id;

      begin
         P := N;
         while Present (P) loop
            if Nkind (P) = N_Object_Renaming_Declaration then
               return not Comes_From_Source (P);
            elsif Is_List_Member (P) then
               return False;
            end if;

            P := Parent (P);
         end loop;

         return False;
      end Is_Internally_Generated_Renaming;

   --  Start of processing for Is_Object_Reference

   begin
      if Is_Entity_Name (N) then
         return Present (Entity (N)) and then Is_Object (Entity (N));

      else
         case Nkind (N) is
            when N_Indexed_Component
               | N_Slice
            =>
               return
                 Is_Object_Reference (Prefix (N))
                   or else Is_Access_Type (Etype (Prefix (N)));

            --  In Ada 95, a function call is a constant object; a procedure
            --  call is not.

            --  Note that predefined operators are functions as well, and so
            --  are attributes that are (can be renamed as) functions.

            when N_Binary_Op
               | N_Function_Call
               | N_Unary_Op
            =>
               return Etype (N) /= Standard_Void_Type;

            --  Attributes references 'Loop_Entry, 'Old, and 'Result yield
            --  objects, even though they are not functions.

            when N_Attribute_Reference =>
               return
                 Nam_In (Attribute_Name (N), Name_Loop_Entry,
                                             Name_Old,
                                             Name_Result)
                   or else Is_Function_Attribute_Name (Attribute_Name (N));

            when N_Selected_Component =>
               return
                 Is_Object_Reference (Selector_Name (N))
                   and then
                     (Is_Object_Reference (Prefix (N))
                       or else Is_Access_Type (Etype (Prefix (N))));

            --  An explicit dereference denotes an object, except that a
            --  conditional expression gets turned into an explicit dereference
            --  in some cases, and conditional expressions are not object
            --  names.

            when N_Explicit_Dereference =>
               return not Nkind_In (Original_Node (N), N_Case_Expression,
                                                       N_If_Expression);

            when N_Type_Conversion =>
               --  A view conversion of a tagged object is an object reference

               return Is_Tagged_Type (Etype (Subtype_Mark (N)))
                 and then Is_Tagged_Type (Etype (Expression (N)))
                 and then Is_Object_Reference (Expression (N));

            --  An unchecked type conversion is considered to be an object if
            --  the operand is an object (this construction arises only as a
            --  result of expansion activities).

            when N_Unchecked_Type_Conversion =>
               return True;

            --  Allow string literals to act as objects as long as they appear
            --  in internally-generated renamings. The expansion of iterators
            --  may generate such renamings when the range involves a string
            --  literal.

            when N_String_Literal =>
               return Is_Internally_Generated_Renaming (Parent (N));

            --  AI05-0003: In Ada 2012 a qualified expression is a name.
            --  This allows disambiguation of function calls and the use
            --  of aggregates in more contexts.

            when N_Qualified_Expression =>
               if Ada_Version <  Ada_2012 then
                  return False;
               else
                  return Is_Object_Reference (Expression (N))
                    or else Nkind (Expression (N)) = N_Aggregate;
               end if;

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
      Note_Possible_Modification (AV, Sure => True);

      --  We must reject parenthesized variable names. Comes_From_Source is
      --  checked because there are currently cases where the compiler violates
      --  this rule (e.g. passing a task object to its controlled Initialize
      --  routine). This should be properly documented in sinfo???

      if Paren_Count (AV) > 0 and then Comes_From_Source (AV) then
         return False;

      --  A variable is always allowed

      elsif Is_Variable (AV) then
         return True;

      --  Generalized indexing operations are rewritten as explicit
      --  dereferences, and it is only during resolution that we can
      --  check whether the context requires an access_to_variable type.

      elsif Nkind (AV) = N_Explicit_Dereference
        and then Ada_Version >= Ada_2012
        and then Nkind (Original_Node (AV)) = N_Indexed_Component
        and then Present (Etype (Original_Node (AV)))
        and then Has_Implicit_Dereference (Etype (Original_Node (AV)))
      then
         return not Is_Access_Constant (Etype (Prefix (AV)));

      --  Unchecked conversions are allowed only if they come from the
      --  generated code, which sometimes uses unchecked conversions for out
      --  parameters in cases where code generation is unaffected. We tell
      --  source unchecked conversions by seeing if they are rewrites of
      --  an original Unchecked_Conversion function call, or of an explicit
      --  conversion of a function call or an aggregate (as may happen in the
      --  expansion of a packed array aggregate).

      elsif Nkind (AV) = N_Unchecked_Type_Conversion then
         if Nkind_In (Original_Node (AV), N_Function_Call, N_Aggregate) then
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
            Note_Possible_Modification (Expression (AV), Sure => True);
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

         --  In Ada 2012, the explicit dereference may be a rewritten call to a
         --  Reference function.

         if Ada_Version >= Ada_2012
           and then Nkind (Original_Node (AV)) = N_Function_Call
           and then
             Has_Implicit_Dereference (Etype (Name (Original_Node (AV))))
         then

            --  Check that this is not a constant reference.

            return not Is_Access_Constant (Etype (Prefix (AV)));

         elsif Has_Implicit_Dereference (Etype (Original_Node (AV))) then
            return
              not Is_Access_Constant (Etype
                (Get_Reference_Discriminant (Etype (Original_Node (AV)))));

         else
            return Is_OK_Variable_For_Out_Formal (Original_Node (AV));
         end if;

      --  All other non-variables are rejected

      else
         return False;
      end if;
   end Is_OK_Variable_For_Out_Formal;

   ----------------------------
   -- Is_OK_Volatile_Context --
   ----------------------------

   function Is_OK_Volatile_Context
     (Context : Node_Id;
      Obj_Ref : Node_Id) return Boolean
   is
      function Is_Protected_Operation_Call (Nod : Node_Id) return Boolean;
      --  Determine whether an arbitrary node denotes a call to a protected
      --  entry, function, or procedure in prefixed form where the prefix is
      --  Obj_Ref.

      function Within_Check (Nod : Node_Id) return Boolean;
      --  Determine whether an arbitrary node appears in a check node

      function Within_Subprogram_Call (Nod : Node_Id) return Boolean;
      --  Determine whether an arbitrary node appears in an entry, function, or
      --  procedure call.

      function Within_Volatile_Function (Id : Entity_Id) return Boolean;
      --  Determine whether an arbitrary entity appears in a volatile function

      ---------------------------------
      -- Is_Protected_Operation_Call --
      ---------------------------------

      function Is_Protected_Operation_Call (Nod : Node_Id) return Boolean is
         Pref : Node_Id;
         Subp : Node_Id;

      begin
         --  A call to a protected operations retains its selected component
         --  form as opposed to other prefixed calls that are transformed in
         --  expanded names.

         if Nkind (Nod) = N_Selected_Component then
            Pref := Prefix (Nod);
            Subp := Selector_Name (Nod);

            return
              Pref = Obj_Ref
                and then Present (Etype (Pref))
                and then Is_Protected_Type (Etype (Pref))
                and then Is_Entity_Name (Subp)
                and then Present (Entity (Subp))
                and then Ekind_In (Entity (Subp), E_Entry,
                                                  E_Entry_Family,
                                                  E_Function,
                                                  E_Procedure);
         else
            return False;
         end if;
      end Is_Protected_Operation_Call;

      ------------------
      -- Within_Check --
      ------------------

      function Within_Check (Nod : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         --  Climb the parent chain looking for a check node

         Par := Nod;
         while Present (Par) loop
            if Nkind (Par) in N_Raise_xxx_Error then
               return True;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end Within_Check;

      ----------------------------
      -- Within_Subprogram_Call --
      ----------------------------

      function Within_Subprogram_Call (Nod : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         --  Climb the parent chain looking for a function or procedure call

         Par := Nod;
         while Present (Par) loop
            if Nkind_In (Par, N_Entry_Call_Statement,
                              N_Function_Call,
                              N_Procedure_Call_Statement)
            then
               return True;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end Within_Subprogram_Call;

      ------------------------------
      -- Within_Volatile_Function --
      ------------------------------

      function Within_Volatile_Function (Id : Entity_Id) return Boolean is
         Func_Id : Entity_Id;

      begin
         --  Traverse the scope stack looking for a [generic] function

         Func_Id := Id;
         while Present (Func_Id) and then Func_Id /= Standard_Standard loop
            if Ekind_In (Func_Id, E_Function, E_Generic_Function) then
               return Is_Volatile_Function (Func_Id);
            end if;

            Func_Id := Scope (Func_Id);
         end loop;

         return False;
      end Within_Volatile_Function;

      --  Local variables

      Obj_Id : Entity_Id;

   --  Start of processing for Is_OK_Volatile_Context

   begin
      --  The volatile object appears on either side of an assignment

      if Nkind (Context) = N_Assignment_Statement then
         return True;

      --  The volatile object is part of the initialization expression of
      --  another object.

      elsif Nkind (Context) = N_Object_Declaration
        and then Present (Expression (Context))
        and then Expression (Context) = Obj_Ref
      then
         Obj_Id := Defining_Entity (Context);

         --  The volatile object acts as the initialization expression of an
         --  extended return statement. This is valid context as long as the
         --  function is volatile.

         if Is_Return_Object (Obj_Id) then
            return Within_Volatile_Function (Obj_Id);

         --  Otherwise this is a normal object initialization

         else
            return True;
         end if;

      --  The volatile object acts as the name of a renaming declaration

      elsif Nkind (Context) = N_Object_Renaming_Declaration
        and then Name (Context) = Obj_Ref
      then
         return True;

      --  The volatile object appears as an actual parameter in a call to an
      --  instance of Unchecked_Conversion whose result is renamed.

      elsif Nkind (Context) = N_Function_Call
        and then Is_Entity_Name (Name (Context))
        and then Is_Unchecked_Conversion_Instance (Entity (Name (Context)))
        and then Nkind (Parent (Context)) = N_Object_Renaming_Declaration
      then
         return True;

      --  The volatile object is actually the prefix in a protected entry,
      --  function, or procedure call.

      elsif Is_Protected_Operation_Call (Context) then
         return True;

      --  The volatile object appears as the expression of a simple return
      --  statement that applies to a volatile function.

      elsif Nkind (Context) = N_Simple_Return_Statement
        and then Expression (Context) = Obj_Ref
      then
         return
           Within_Volatile_Function (Return_Statement_Entity (Context));

      --  The volatile object appears as the prefix of a name occurring in a
      --  non-interfering context.

      elsif Nkind_In (Context, N_Attribute_Reference,
                      N_Explicit_Dereference,
                      N_Indexed_Component,
                      N_Selected_Component,
                      N_Slice)
        and then Prefix (Context) = Obj_Ref
        and then Is_OK_Volatile_Context
                   (Context => Parent (Context),
                    Obj_Ref => Context)
      then
         return True;

      --  The volatile object appears as the prefix of attributes Address,
      --  Alignment, Component_Size, First_Bit, Last_Bit, Position, Size,
      --  Storage_Size.

      elsif Nkind (Context) = N_Attribute_Reference
        and then Prefix (Context) = Obj_Ref
        and then Nam_In (Attribute_Name (Context), Name_Address,
                                                   Name_Alignment,
                                                   Name_Component_Size,
                                                   Name_First_Bit,
                                                   Name_Last_Bit,
                                                   Name_Position,
                                                   Name_Size,
                                                   Name_Storage_Size)
      then
         return True;

      --  The volatile object appears as the expression of a type conversion
      --  occurring in a non-interfering context.

      elsif Nkind_In (Context, N_Type_Conversion,
                               N_Unchecked_Type_Conversion)
        and then Expression (Context) = Obj_Ref
        and then Is_OK_Volatile_Context
                   (Context => Parent (Context),
                    Obj_Ref => Context)
      then
         return True;

      --  The volatile object appears as the expression in a delay statement

      elsif Nkind (Context) in N_Delay_Statement then
         return True;

      --  Allow references to volatile objects in various checks. This is not a
      --  direct SPARK 2014 requirement.

      elsif Within_Check (Context) then
         return True;

      --  Assume that references to effectively volatile objects that appear
      --  as actual parameters in a subprogram call are always legal. A full
      --  legality check is done when the actuals are resolved (see routine
      --  Resolve_Actuals).

      elsif Within_Subprogram_Call (Context) then
         return True;

      --  Otherwise the context is not suitable for an effectively volatile
      --  object.

      else
         return False;
      end if;
   end Is_OK_Volatile_Context;

   ------------------------------------
   -- Is_Package_Contract_Annotation --
   ------------------------------------

   function Is_Package_Contract_Annotation (Item : Node_Id) return Boolean is
      Nam : Name_Id;

   begin
      if Nkind (Item) = N_Aspect_Specification then
         Nam := Chars (Identifier (Item));

      else pragma Assert (Nkind (Item) = N_Pragma);
         Nam := Pragma_Name (Item);
      end if;

      return    Nam = Name_Abstract_State
        or else Nam = Name_Initial_Condition
        or else Nam = Name_Initializes
        or else Nam = Name_Refined_State;
   end Is_Package_Contract_Annotation;

   -----------------------------------
   -- Is_Partially_Initialized_Type --
   -----------------------------------

   function Is_Partially_Initialized_Type
     (Typ              : Entity_Id;
      Include_Implicit : Boolean := True) return Boolean
   is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return Include_Implicit;

      elsif Is_Array_Type (Typ) then

         --  If component type is partially initialized, so is array type

         if Is_Partially_Initialized_Type
              (Component_Type (Typ), Include_Implicit)
         then
            return True;

         --  Otherwise we are only partially initialized if we are fully
         --  initialized (this is the empty array case, no point in us
         --  duplicating that code here).

         else
            return Is_Fully_Initialized_Type (Typ);
         end if;

      elsif Is_Record_Type (Typ) then

         --  A discriminated type is always partially initialized if in
         --  all mode

         if Has_Discriminants (Typ) and then Include_Implicit then
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

                     elsif Is_Partially_Initialized_Type
                             (Etype (Ent), Include_Implicit)
                     then
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
               return Is_Partially_Initialized_Type (U, Include_Implicit);
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
      --  For private type, test corresponding full type

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

   --------------------------------
   -- Is_Potentially_Unevaluated --
   --------------------------------

   function Is_Potentially_Unevaluated (N : Node_Id) return Boolean is
      Par  : Node_Id;
      Expr : Node_Id;

   begin
      Expr := N;
      Par  := Parent (N);

      --  A postcondition whose expression is a short-circuit is broken down
      --  into individual aspects for better exception reporting. The original
      --  short-circuit expression is rewritten as the second operand, and an
      --  occurrence of 'Old in that operand is potentially unevaluated.
      --  See Sem_ch13.adb for details of this transformation.

      if Nkind (Original_Node (Par)) = N_And_Then then
         return True;
      end if;

      while not Nkind_In (Par, N_If_Expression,
                               N_Case_Expression,
                               N_And_Then,
                               N_Or_Else,
                               N_In,
                               N_Not_In,
                               N_Quantified_Expression)
      loop
         Expr := Par;
         Par  := Parent (Par);

         --  If the context is not an expression, or if is the result of
         --  expansion of an enclosing construct (such as another attribute)
         --  the predicate does not apply.

         if Nkind (Par) = N_Case_Expression_Alternative then
            null;

         elsif Nkind (Par) not in N_Subexpr
           or else not Comes_From_Source (Par)
         then
            return False;
         end if;
      end loop;

      if Nkind (Par) = N_If_Expression then
         return Is_Elsif (Par) or else Expr /= First (Expressions (Par));

      elsif Nkind (Par) = N_Case_Expression then
         return Expr /= Expression (Par);

      elsif Nkind_In (Par, N_And_Then, N_Or_Else) then
         return Expr = Right_Opnd (Par);

      elsif Nkind_In (Par, N_In, N_Not_In) then

         --  If the membership includes several alternatives, only the first is
         --  definitely evaluated.

         if Present (Alternatives (Par)) then
            return Expr /= First (Alternatives (Par));

         --  If this is a range membership both bounds are evaluated

         else
            return False;
         end if;

      elsif Nkind (Par) = N_Quantified_Expression then
         return Expr = Condition (Par);

      else
         return False;
      end if;
   end Is_Potentially_Unevaluated;

   ---------------------------------
   -- Is_Protected_Self_Reference --
   ---------------------------------

   function Is_Protected_Self_Reference (N : Node_Id) return Boolean is

      function In_Access_Definition (N : Node_Id) return Boolean;
      --  Returns true if N belongs to an access definition

      --------------------------
      -- In_Access_Definition --
      --------------------------

      function In_Access_Definition (N : Node_Id) return Boolean is
         P : Node_Id;

      begin
         P := Parent (N);
         while Present (P) loop
            if Nkind (P) = N_Access_Definition then
               return True;
            end if;

            P := Parent (P);
         end loop;

         return False;
      end In_Access_Definition;

   --  Start of processing for Is_Protected_Self_Reference

   begin
      --  Verify that prefix is analyzed and has the proper form. Note that
      --  the attributes Elab_Spec, Elab_Body, and Elab_Subp_Body, which also
      --  produce the address of an entity, do not analyze their prefix
      --  because they denote entities that are not necessarily visible.
      --  Neither of them can apply to a protected type.

      return Ada_Version >= Ada_2005
        and then Is_Entity_Name (N)
        and then Present (Entity (N))
        and then Is_Protected_Type (Entity (N))
        and then In_Open_Scopes (Entity (N))
        and then not In_Access_Definition (N);
   end Is_Protected_Self_Reference;

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
   begin
      --  A remote access to class-wide type is a general access to object type
      --  declared in the visible part of a Remote_Types or Remote_Call_
      --  Interface unit.

      return Ekind (E) = E_General_Access_Type
        and then (Is_Remote_Call_Interface (E) or else Is_Remote_Types (E));
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
        and then (Is_Remote_Call_Interface (E) or else Is_Remote_Types (E));
   end Is_Remote_Access_To_Subprogram_Type;

   --------------------
   -- Is_Remote_Call --
   --------------------

   function Is_Remote_Call (N : Node_Id) return Boolean is
   begin
      if Nkind (N) not in N_Subprogram_Call then

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
      --  Determine whether Nam is an entry. Traverse selectors if there are
      --  nested selected components.

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

   -----------------------------
   -- Is_Renaming_Declaration --
   -----------------------------

   function Is_Renaming_Declaration (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Exception_Renaming_Declaration
            | N_Generic_Function_Renaming_Declaration
            | N_Generic_Package_Renaming_Declaration
            | N_Generic_Procedure_Renaming_Declaration
            | N_Object_Renaming_Declaration
            | N_Package_Renaming_Declaration
            | N_Subprogram_Renaming_Declaration
          =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Renaming_Declaration;

   ----------------------------
   -- Is_Reversible_Iterator --
   ----------------------------

   function Is_Reversible_Iterator (Typ : Entity_Id) return Boolean is
      Ifaces_List : Elist_Id;
      Iface_Elmt  : Elmt_Id;
      Iface       : Entity_Id;

   begin
      if Is_Class_Wide_Type (Typ)
        and then Chars (Root_Type (Typ)) = Name_Reversible_Iterator
        and then In_Predefined_Unit (Root_Type (Typ))
      then
         return True;

      elsif not Is_Tagged_Type (Typ) or else not Is_Derived_Type (Typ) then
         return False;

      else
         Collect_Interfaces (Typ, Ifaces_List);

         Iface_Elmt := First_Elmt (Ifaces_List);
         while Present (Iface_Elmt) loop
            Iface := Node (Iface_Elmt);
            if Chars (Iface) = Name_Reversible_Iterator
              and then In_Predefined_Unit (Iface)
            then
               return True;
            end if;

            Next_Elmt (Iface_Elmt);
         end loop;
      end if;

      return False;
   end Is_Reversible_Iterator;

   ----------------------
   -- Is_Selector_Name --
   ----------------------

   function Is_Selector_Name (N : Node_Id) return Boolean is
   begin
      if not Is_List_Member (N) then
         declare
            P : constant Node_Id   := Parent (N);
         begin
            return Nkind_In (P, N_Expanded_Name,
                                N_Generic_Association,
                                N_Parameter_Association,
                                N_Selected_Component)
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

   ---------------------------------
   -- Is_Single_Concurrent_Object --
   ---------------------------------

   function Is_Single_Concurrent_Object (Id : Entity_Id) return Boolean is
   begin
      return
        Is_Single_Protected_Object (Id) or else Is_Single_Task_Object (Id);
   end Is_Single_Concurrent_Object;

   -------------------------------
   -- Is_Single_Concurrent_Type --
   -------------------------------

   function Is_Single_Concurrent_Type (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind_In (Id, E_Protected_Type, E_Task_Type)
          and then Is_Single_Concurrent_Type_Declaration
                     (Declaration_Node (Id));
   end Is_Single_Concurrent_Type;

   -------------------------------------------
   -- Is_Single_Concurrent_Type_Declaration --
   -------------------------------------------

   function Is_Single_Concurrent_Type_Declaration
     (N : Node_Id) return Boolean
   is
   begin
      return Nkind_In (Original_Node (N), N_Single_Protected_Declaration,
                                          N_Single_Task_Declaration);
   end Is_Single_Concurrent_Type_Declaration;

   ---------------------------------------------
   -- Is_Single_Precision_Floating_Point_Type --
   ---------------------------------------------

   function Is_Single_Precision_Floating_Point_Type
     (E : Entity_Id) return Boolean is
   begin
      return Is_Floating_Point_Type (E)
        and then Machine_Radix_Value (E) = Uint_2
        and then Machine_Mantissa_Value (E) = Uint_24
        and then Machine_Emax_Value (E) = Uint_2 ** Uint_7
        and then Machine_Emin_Value (E) = Uint_3 - (Uint_2 ** Uint_7);
   end Is_Single_Precision_Floating_Point_Type;

   --------------------------------
   -- Is_Single_Protected_Object --
   --------------------------------

   function Is_Single_Protected_Object (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind (Id) = E_Variable
          and then Ekind (Etype (Id)) = E_Protected_Type
          and then Is_Single_Concurrent_Type (Etype (Id));
   end Is_Single_Protected_Object;

   ---------------------------
   -- Is_Single_Task_Object --
   ---------------------------

   function Is_Single_Task_Object (Id : Entity_Id) return Boolean is
   begin
      return
        Ekind (Id) = E_Variable
          and then Ekind (Etype (Id)) = E_Task_Type
          and then Is_Single_Concurrent_Type (Etype (Id));
   end Is_Single_Task_Object;

   -------------------------------------
   -- Is_SPARK_05_Initialization_Expr --
   -------------------------------------

   function Is_SPARK_05_Initialization_Expr (N : Node_Id) return Boolean is
      Is_Ok     : Boolean;
      Expr      : Node_Id;
      Comp_Assn : Node_Id;
      Orig_N    : constant Node_Id := Original_Node (N);

   begin
      Is_Ok := True;

      if not Comes_From_Source (Orig_N) then
         goto Done;
      end if;

      pragma Assert (Nkind (Orig_N) in N_Subexpr);

      case Nkind (Orig_N) is
         when N_Character_Literal
            | N_Integer_Literal
            | N_Real_Literal
            | N_String_Literal
         =>
            null;

         when N_Expanded_Name
            | N_Identifier
         =>
            if Is_Entity_Name (Orig_N)
              and then Present (Entity (Orig_N))  --  needed in some cases
            then
               case Ekind (Entity (Orig_N)) is
                  when E_Constant
                     | E_Enumeration_Literal
                     | E_Named_Integer
                     | E_Named_Real
                  =>
                     null;

                  when others =>
                     if Is_Type (Entity (Orig_N)) then
                        null;
                     else
                        Is_Ok := False;
                     end if;
               end case;
            end if;

         when N_Qualified_Expression
            | N_Type_Conversion
         =>
            Is_Ok := Is_SPARK_05_Initialization_Expr (Expression (Orig_N));

         when N_Unary_Op =>
            Is_Ok := Is_SPARK_05_Initialization_Expr (Right_Opnd (Orig_N));

         when N_Binary_Op
            | N_Membership_Test
            | N_Short_Circuit
         =>
            Is_Ok := Is_SPARK_05_Initialization_Expr (Left_Opnd (Orig_N))
                       and then
                         Is_SPARK_05_Initialization_Expr (Right_Opnd (Orig_N));

         when N_Aggregate
            | N_Extension_Aggregate
         =>
            if Nkind (Orig_N) = N_Extension_Aggregate then
               Is_Ok :=
                 Is_SPARK_05_Initialization_Expr (Ancestor_Part (Orig_N));
            end if;

            Expr := First (Expressions (Orig_N));
            while Present (Expr) loop
               if not Is_SPARK_05_Initialization_Expr (Expr) then
                  Is_Ok := False;
                  goto Done;
               end if;

               Next (Expr);
            end loop;

            Comp_Assn := First (Component_Associations (Orig_N));
            while Present (Comp_Assn) loop
               Expr := Expression (Comp_Assn);

               --  Note: test for Present here needed for box assocation

               if Present (Expr)
                 and then not Is_SPARK_05_Initialization_Expr (Expr)
               then
                  Is_Ok := False;
                  goto Done;
               end if;

               Next (Comp_Assn);
            end loop;

         when N_Attribute_Reference =>
            if Nkind (Prefix (Orig_N)) in N_Subexpr then
               Is_Ok := Is_SPARK_05_Initialization_Expr (Prefix (Orig_N));
            end if;

            Expr := First (Expressions (Orig_N));
            while Present (Expr) loop
               if not Is_SPARK_05_Initialization_Expr (Expr) then
                  Is_Ok := False;
                  goto Done;
               end if;

               Next (Expr);
            end loop;

         --  Selected components might be expanded named not yet resolved, so
         --  default on the safe side. (Eg on sparklex.ads)

         when N_Selected_Component =>
            null;

         when others =>
            Is_Ok := False;
      end case;

   <<Done>>
      return Is_Ok;
   end Is_SPARK_05_Initialization_Expr;

   ----------------------------------
   -- Is_SPARK_05_Object_Reference --
   ----------------------------------

   function Is_SPARK_05_Object_Reference (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Present (Entity (N))
           and then
             (Ekind_In (Entity (N), E_Constant, E_Variable)
               or else Ekind (Entity (N)) in Formal_Kind);

      else
         case Nkind (N) is
            when N_Selected_Component =>
               return Is_SPARK_05_Object_Reference (Prefix (N));

            when others =>
               return False;
         end case;
      end if;
   end Is_SPARK_05_Object_Reference;

   -----------------------------
   -- Is_Specific_Tagged_Type --
   -----------------------------

   function Is_Specific_Tagged_Type (Typ : Entity_Id) return Boolean is
      Full_Typ : Entity_Id;

   begin
      --  Handle private types

      if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
         Full_Typ := Full_View (Typ);
      else
         Full_Typ := Typ;
      end if;

      --  A specific tagged type is a non-class-wide tagged type

      return Is_Tagged_Type (Full_Typ) and not Is_Class_Wide_Type (Full_Typ);
   end Is_Specific_Tagged_Type;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (N : Node_Id) return Boolean is
   begin
      return
        Nkind (N) in N_Statement_Other_Than_Procedure_Call
          or else Nkind (N) = N_Procedure_Call_Statement;
   end Is_Statement;

   ---------------------------------------
   -- Is_Subprogram_Contract_Annotation --
   ---------------------------------------

   function Is_Subprogram_Contract_Annotation
     (Item : Node_Id) return Boolean
   is
      Nam : Name_Id;

   begin
      if Nkind (Item) = N_Aspect_Specification then
         Nam := Chars (Identifier (Item));

      else pragma Assert (Nkind (Item) = N_Pragma);
         Nam := Pragma_Name (Item);
      end if;

      return    Nam = Name_Contract_Cases
        or else Nam = Name_Depends
        or else Nam = Name_Extensions_Visible
        or else Nam = Name_Global
        or else Nam = Name_Post
        or else Nam = Name_Post_Class
        or else Nam = Name_Postcondition
        or else Nam = Name_Pre
        or else Nam = Name_Pre_Class
        or else Nam = Name_Precondition
        or else Nam = Name_Refined_Depends
        or else Nam = Name_Refined_Global
        or else Nam = Name_Refined_Post
        or else Nam = Name_Test_Case;
   end Is_Subprogram_Contract_Annotation;

   --------------------------------------------------
   -- Is_Subprogram_Stub_Without_Prior_Declaration --
   --------------------------------------------------

   function Is_Subprogram_Stub_Without_Prior_Declaration
     (N : Node_Id) return Boolean
   is
   begin
      --  A subprogram stub without prior declaration serves as declaration for
      --  the actual subprogram body. As such, it has an attached defining
      --  entity of E_[Generic_]Function or E_[Generic_]Procedure.

      return Nkind (N) = N_Subprogram_Body_Stub
        and then Ekind (Defining_Entity (N)) /= E_Subprogram_Body;
   end Is_Subprogram_Stub_Without_Prior_Declaration;

   ---------------------------------------
   -- Is_Subp_Or_Entry_Inside_Protected --
   ---------------------------------------

   function Is_Subp_Or_Entry_Inside_Protected (E : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      case Ekind (E) is
         when Entry_Kind
            | Subprogram_Kind
         =>
            Scop := Scope (E);

            while Present (Scop) loop
               if Ekind (Scop) = E_Protected_Type then
                  return True;
               end if;
               Scop := Scope (Scop);
            end loop;

            return False;

         when others =>
            return False;
      end case;
   end Is_Subp_Or_Entry_Inside_Protected;

   --------------------------
   -- Is_Suspension_Object --
   --------------------------

   function Is_Suspension_Object (Id : Entity_Id) return Boolean is
   begin
      --  This approach does an exact name match rather than to rely on
      --  RTSfind. Routine Is_Effectively_Volatile is used by clients of the
      --  front end at point where all auxiliary tables are locked and any
      --  modifications to them are treated as violations. Do not tamper with
      --  the tables, instead examine the Chars fields of all the scopes of Id.

      return
        Chars (Id) = Name_Suspension_Object
          and then Present (Scope (Id))
          and then Chars (Scope (Id)) = Name_Synchronous_Task_Control
          and then Present (Scope (Scope (Id)))
          and then Chars (Scope (Scope (Id))) = Name_Ada
          and then Present (Scope (Scope (Scope (Id))))
          and then Scope (Scope (Scope (Id))) = Standard_Standard;
   end Is_Suspension_Object;

   ----------------------------
   -- Is_Synchronized_Object --
   ----------------------------

   function Is_Synchronized_Object (Id : Entity_Id) return Boolean is
      Prag : Node_Id;

   begin
      if Is_Object (Id) then

         --  The object is synchronized if it is of a type that yields a
         --  synchronized object.

         if Yields_Synchronized_Object (Etype (Id)) then
            return True;

         --  The object is synchronized if it is atomic and Async_Writers is
         --  enabled.

         elsif Is_Atomic (Id) and then Async_Writers_Enabled (Id) then
            return True;

         --  A constant is a synchronized object by default

         elsif Ekind (Id) = E_Constant then
            return True;

         --  A variable is a synchronized object if it is subject to pragma
         --  Constant_After_Elaboration.

         elsif Ekind (Id) = E_Variable then
            Prag := Get_Pragma (Id, Pragma_Constant_After_Elaboration);

            return Present (Prag) and then Is_Enabled_Pragma (Prag);
         end if;
      end if;

      --  Otherwise the input is not an object or it does not qualify as a
      --  synchronized object.

      return False;
   end Is_Synchronized_Object;

   ---------------------------------
   -- Is_Synchronized_Tagged_Type --
   ---------------------------------

   function Is_Synchronized_Tagged_Type (E : Entity_Id) return Boolean is
      Kind : constant Entity_Kind := Ekind (Base_Type (E));

   begin
      --  A task or protected type derived from an interface is a tagged type.
      --  Such a tagged type is called a synchronized tagged type, as are
      --  synchronized interfaces and private extensions whose declaration
      --  includes the reserved word synchronized.

      return (Is_Tagged_Type (E)
                and then (Kind = E_Task_Type
                            or else
                          Kind = E_Protected_Type))
            or else
             (Is_Interface (E)
                and then Is_Synchronized_Interface (E))
            or else
             (Ekind (E) = E_Record_Type_With_Private
                and then Nkind (Parent (E)) = N_Private_Extension_Declaration
                and then (Synchronized_Present (Parent (E))
                           or else Is_Synchronized_Interface (Etype (E))));
   end Is_Synchronized_Tagged_Type;

   -----------------
   -- Is_Transfer --
   -----------------

   function Is_Transfer (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      if Kind = N_Simple_Return_Statement
           or else
         Kind = N_Extended_Return_Statement
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

   --------------------------------------
   -- Is_Unchecked_Conversion_Instance --
   --------------------------------------

   function Is_Unchecked_Conversion_Instance (Id : Entity_Id) return Boolean is
      Par : Node_Id;

   begin
      --  Look for a function whose generic parent is the predefined intrinsic
      --  function Unchecked_Conversion, or for one that renames such an
      --  instance.

      if Ekind (Id) = E_Function then
         Par := Parent (Id);

         if Nkind (Par) = N_Function_Specification then
            Par := Generic_Parent (Par);

            if Present (Par) then
               return
                 Chars (Par) = Name_Unchecked_Conversion
                   and then Is_Intrinsic_Subprogram (Par)
                   and then In_Predefined_Unit (Par);
            else
               return
                 Present (Alias (Id))
                   and then Is_Unchecked_Conversion_Instance (Alias (Id));
            end if;
         end if;
      end if;

      return False;
   end Is_Unchecked_Conversion_Instance;

   -------------------------------
   -- Is_Universal_Numeric_Type --
   -------------------------------

   function Is_Universal_Numeric_Type (T : Entity_Id) return Boolean is
   begin
      return T = Universal_Integer or else T = Universal_Real;
   end Is_Universal_Numeric_Type;

   --------------------------------------
   -- Is_Validation_Variable_Reference --
   --------------------------------------

   function Is_Validation_Variable_Reference (N : Node_Id) return Boolean is
      Var    : Node_Id;
      Var_Id : Entity_Id;

   begin
      Var := N;

      --  Use the expression when the context qualifies a reference in some
      --  fashion.

      while Nkind_In (Var, N_Qualified_Expression,
                           N_Type_Conversion,
                           N_Unchecked_Type_Conversion)
      loop
         Var := Expression (Var);
      end loop;

      Var_Id := Empty;

      if Is_Entity_Name (Var) then
         Var_Id := Entity (Var);
      end if;

      return
        Present (Var_Id)
          and then Ekind (Var_Id) = E_Variable
          and then Present (Validated_Object (Var_Id));
   end Is_Validation_Variable_Reference;

   ----------------------------
   -- Is_Variable_Size_Array --
   ----------------------------

   function Is_Variable_Size_Array (E : Entity_Id) return Boolean is
      Idx : Node_Id;

   begin
      pragma Assert (Is_Array_Type (E));

      --  Check if some index is initialized with a non-constant value

      Idx := First_Index (E);
      while Present (Idx) loop
         if Nkind (Idx) = N_Range then
            if not Is_Constant_Bound (Low_Bound (Idx))
              or else not Is_Constant_Bound (High_Bound (Idx))
            then
               return True;
            end if;
         end if;

         Idx := Next_Index (Idx);
      end loop;

      return False;
   end Is_Variable_Size_Array;

   -----------------------------
   -- Is_Variable_Size_Record --
   -----------------------------

   function Is_Variable_Size_Record (E : Entity_Id) return Boolean is
      Comp     : Entity_Id;
      Comp_Typ : Entity_Id;

   begin
      pragma Assert (Is_Record_Type (E));

      Comp := First_Entity (E);
      while Present (Comp) loop
         Comp_Typ := Etype (Comp);

         --  Recursive call if the record type has discriminants

         if Is_Record_Type (Comp_Typ)
           and then Has_Discriminants (Comp_Typ)
           and then Is_Variable_Size_Record (Comp_Typ)
         then
            return True;

         elsif Is_Array_Type (Comp_Typ)
           and then Is_Variable_Size_Array (Comp_Typ)
         then
            return True;
         end if;

         Next_Entity (Comp);
      end loop;

      return False;
   end Is_Variable_Size_Record;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable
     (N                 : Node_Id;
      Use_Original_Node : Boolean := True) return Boolean
   is
      Orig_Node : Node_Id;

      function In_Protected_Function (E : Entity_Id) return Boolean;
      --  Within a protected function, the private components of the enclosing
      --  protected type are constants. A function nested within a (protected)
      --  procedure is not itself protected. Within the body of a protected
      --  function the current instance of the protected type is a constant.

      function Is_Variable_Prefix (P : Node_Id) return Boolean;
      --  Prefixes can involve implicit dereferences, in which case we must
      --  test for the case of a reference of a constant access type, which can
      --  can never be a variable.

      ---------------------------
      -- In_Protected_Function --
      ---------------------------

      function In_Protected_Function (E : Entity_Id) return Boolean is
         Prot : Entity_Id;
         S    : Entity_Id;

      begin
         --  E is the current instance of a type

         if Is_Type (E) then
            Prot := E;

         --  E is an object

         else
            Prot := Scope (E);
         end if;

         if not Is_Protected_Type (Prot) then
            return False;

         else
            S := Current_Scope;
            while Present (S) and then S /= Prot loop
               if Ekind (S) = E_Function and then Scope (S) = Prot then
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
      --  Special check, allow x'Deref(expr) as a variable

      if Nkind (N) = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Deref
      then
         return True;
      end if;

      --  Check if we perform the test on the original node since this may be a
      --  test of syntactic categories which must not be disturbed by whatever
      --  rewriting might have occurred. For example, an aggregate, which is
      --  certainly NOT a variable, could be turned into a variable by
      --  expansion.

      if Use_Original_Node then
         Orig_Node := Original_Node (N);
      else
         Orig_Node := N;
      end if;

      --  Definitely OK if Assignment_OK is set. Since this is something that
      --  only gets set for expanded nodes, the test is on N, not Orig_Node.

      if Nkind (N) in N_Subexpr and then Assignment_OK (N) then
         return True;

      --  Normally we go to the original node, but there is one exception where
      --  we use the rewritten node, namely when it is an explicit dereference.
      --  The generated code may rewrite a prefix which is an access type with
      --  an explicit dereference. The dereference is a variable, even though
      --  the original node may not be (since it could be a constant of the
      --  access type).

      --  In Ada 2005 we have a further case to consider: the prefix may be a
      --  function call given in prefix notation. The original node appears to
      --  be a selected component, but we need to examine the call.

      elsif Nkind (N) = N_Explicit_Dereference
        and then Nkind (Orig_Node) /= N_Explicit_Dereference
        and then Present (Etype (Orig_Node))
        and then Is_Access_Type (Etype (Orig_Node))
      then
         --  Note that if the prefix is an explicit dereference that does not
         --  come from source, we must check for a rewritten function call in
         --  prefixed notation before other forms of rewriting, to prevent a
         --  compiler crash.

         return
           (Nkind (Orig_Node) = N_Function_Call
             and then not Is_Access_Constant (Etype (Prefix (N))))
           or else
             Is_Variable_Prefix (Original_Node (Prefix (N)));

      --  in Ada 2012, the dereference may have been added for a type with
      --  a declared implicit dereference aspect. Check that it is not an
      --  access to constant.

      elsif Nkind (N) = N_Explicit_Dereference
        and then Present (Etype (Orig_Node))
        and then Ada_Version >= Ada_2012
        and then Has_Implicit_Dereference (Etype (Orig_Node))
      then
         return not Is_Access_Constant (Etype (Prefix (N)));

      --  A function call is never a variable

      elsif Nkind (N) = N_Function_Call then
         return False;

      --  All remaining checks use the original node

      elsif Is_Entity_Name (Orig_Node)
        and then Present (Entity (Orig_Node))
      then
         declare
            E : constant Entity_Id := Entity (Orig_Node);
            K : constant Entity_Kind := Ekind (E);

         begin
            return    (K = E_Variable
                        and then Nkind (Parent (E)) /= N_Exception_Handler)
              or else (K = E_Component
                        and then not In_Protected_Function (E))
              or else K = E_Out_Parameter
              or else K = E_In_Out_Parameter
              or else K = E_Generic_In_Out_Parameter

              --  Current instance of type. If this is a protected type, check
              --  we are not within the body of one of its protected functions.

              or else (Is_Type (E)
                        and then In_Open_Scopes (E)
                        and then not In_Protected_Function (E))

              or else (Is_Incomplete_Or_Private_Type (E)
                        and then In_Open_Scopes (Full_View (E)));
         end;

      else
         case Nkind (Orig_Node) is
            when N_Indexed_Component
               | N_Slice
            =>
               return Is_Variable_Prefix (Prefix (Orig_Node));

            when N_Selected_Component =>
               return (Is_Variable (Selector_Name (Orig_Node))
                        and then Is_Variable_Prefix (Prefix (Orig_Node)))
                 or else
                   (Nkind (N) = N_Expanded_Name
                     and then Scope (Entity (N)) = Entity (Prefix (N)));

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

            when N_Unchecked_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node));

            when others =>
               return False;
         end case;
      end if;
   end Is_Variable;

   ------------------------------
   -- Is_Verifiable_DIC_Pragma --
   ------------------------------

   function Is_Verifiable_DIC_Pragma (Prag : Node_Id) return Boolean is
      Args : constant List_Id := Pragma_Argument_Associations (Prag);

   begin
      --  To qualify as verifiable, a DIC pragma must have a non-null argument

      return
        Present (Args)
          and then Nkind (Get_Pragma_Arg (First (Args))) /= N_Null;
   end Is_Verifiable_DIC_Pragma;

   ---------------------------
   -- Is_Visibly_Controlled --
   ---------------------------

   function Is_Visibly_Controlled (T : Entity_Id) return Boolean is
      Root : constant Entity_Id := Root_Type (T);
   begin
      return Chars (Scope (Root)) = Name_Finalization
        and then Chars (Scope (Scope (Root))) = Name_Ada
        and then Scope (Scope (Scope (Root))) = Standard_Standard;
   end Is_Visibly_Controlled;

   --------------------------
   -- Is_Volatile_Function --
   --------------------------

   function Is_Volatile_Function (Func_Id : Entity_Id) return Boolean is
   begin
      pragma Assert (Ekind_In (Func_Id, E_Function, E_Generic_Function));

      --  A function declared within a protected type is volatile

      if Is_Protected_Type (Scope (Func_Id)) then
         return True;

      --  An instance of Ada.Unchecked_Conversion is a volatile function if
      --  either the source or the target are effectively volatile.

      elsif Is_Unchecked_Conversion_Instance (Func_Id)
        and then Has_Effectively_Volatile_Profile (Func_Id)
      then
         return True;

      --  Otherwise the function is treated as volatile if it is subject to
      --  enabled pragma Volatile_Function.

      else
         return
           Is_Enabled_Pragma (Get_Pragma (Func_Id, Pragma_Volatile_Function));
      end if;
   end Is_Volatile_Function;

   ------------------------
   -- Is_Volatile_Object --
   ------------------------

   function Is_Volatile_Object (N : Node_Id) return Boolean is
      function Is_Volatile_Prefix (N : Node_Id) return Boolean;
      --  If prefix is an implicit dereference, examine designated type

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean;
      --  Determines if given object has volatile components

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
      if Nkind (N) = N_Defining_Identifier then
         return Is_Volatile (N) or else Is_Volatile (Etype (N));

      elsif Nkind (N) = N_Expanded_Name then
         return Is_Volatile_Object (Entity (N));

      elsif Is_Volatile (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Volatile (Entity (N)))
      then
         return True;

      elsif Nkind_In (N, N_Indexed_Component, N_Selected_Component)
        and then Is_Volatile_Prefix (Prefix (N))
      then
         return True;

      elsif Nkind (N) = N_Selected_Component
        and then Is_Volatile (Entity (Selector_Name (N)))
      then
         return True;

      else
         return False;
      end if;
   end Is_Volatile_Object;

   ---------------------------
   -- Itype_Has_Declaration --
   ---------------------------

   function Itype_Has_Declaration (Id : Entity_Id) return Boolean is
   begin
      pragma Assert (Is_Itype (Id));
      return Present (Parent (Id))
        and then Nkind_In (Parent (Id), N_Full_Type_Declaration,
                                        N_Subtype_Declaration)
        and then Defining_Entity (Parent (Id)) = Id;
   end Itype_Has_Declaration;

   -------------------------
   -- Kill_Current_Values --
   -------------------------

   procedure Kill_Current_Values
     (Ent                  : Entity_Id;
      Last_Assignment_Only : Boolean := False)
   is
   begin
      if Is_Assignable (Ent) then
         Set_Last_Assignment (Ent, Empty);
      end if;

      if Is_Object (Ent) then
         if not Last_Assignment_Only then
            Kill_Checks (Ent);
            Set_Current_Value (Ent, Empty);

            --  Do not reset the Is_Known_[Non_]Null and Is_Known_Valid flags
            --  for a constant. Once the constant is elaborated, its value is
            --  not changed, therefore the associated flags that describe the
            --  value should not be modified either.

            if Ekind (Ent) = E_Constant then
               null;

            --  Non-constant entities

            else
               if not Can_Never_Be_Null (Ent) then
                  Set_Is_Known_Non_Null (Ent, False);
               end if;

               Set_Is_Known_Null (Ent, False);

               --  Reset the Is_Known_Valid flag unless the type is always
               --  valid. This does not apply to a loop parameter because its
               --  bounds are defined by the loop header and therefore always
               --  valid.

               if not Is_Known_Valid (Etype (Ent))
                 and then Ekind (Ent) /= E_Loop_Parameter
               then
                  Set_Is_Known_Valid (Ent, False);
               end if;
            end if;
         end if;
      end if;
   end Kill_Current_Values;

   procedure Kill_Current_Values (Last_Assignment_Only : Boolean := False) is
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
            Kill_Current_Values (Ent, Last_Assignment_Only);
            Next_Entity (Ent);
         end loop;
      end Kill_Current_Values_For_Entity_Chain;

   --  Start of processing for Kill_Current_Values

   begin
      --  Kill all saved checks, a special case of killing saved values

      if not Last_Assignment_Only then
         Kill_All_Checks;
      end if;

      --  Loop through relevant scopes, which includes the current scope and
      --  any parent scopes if the current scope is a block or a package.

      S := Current_Scope;
      Scope_Loop : loop

         --  Clear current values of all entities in current scope

         Kill_Current_Values_For_Entity_Chain (First_Entity (S));

         --  If scope is a package, also clear current values of all private
         --  entities in the scope.

         if Is_Package_Or_Generic_Package (S)
           or else Is_Concurrent_Type (S)
         then
            Kill_Current_Values_For_Entity_Chain (First_Private_Entity (S));
         end if;

         --  If this is a not a subprogram, deal with parents

         if not Is_Subprogram (S) then
            S := Scope (S);
            exit Scope_Loop when S = Standard_Standard;
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

   --------------------
   -- Known_Non_Null --
   --------------------

   function Known_Non_Null (N : Node_Id) return Boolean is
      Status : constant Null_Status_Kind := Null_Status (N);

      Id  : Entity_Id;
      Op  : Node_Kind;
      Val : Node_Id;

   begin
      --  The expression yields a non-null value ignoring simple flow analysis

      if Status = Is_Non_Null then
         return True;

      --  Otherwise check whether N is a reference to an entity that appears
      --  within a conditional construct.

      elsif Is_Entity_Name (N) and then Present (Entity (N)) then

         --  First check if we are in decisive conditional

         Get_Current_Value_Condition (N, Op, Val);

         if Known_Null (Val) then
            if Op = N_Op_Eq then
               return False;
            elsif Op = N_Op_Ne then
               return True;
            end if;
         end if;

         --  If OK to do replacement, test Is_Known_Non_Null flag

         Id := Entity (N);

         if OK_To_Do_Constant_Replacement (Id) then
            return Is_Known_Non_Null (Id);
         end if;
      end if;

      --  Otherwise it is not possible to determine whether N yields a non-null
      --  value.

      return False;
   end Known_Non_Null;

   ----------------
   -- Known_Null --
   ----------------

   function Known_Null (N : Node_Id) return Boolean is
      Status : constant Null_Status_Kind := Null_Status (N);

      Id  : Entity_Id;
      Op  : Node_Kind;
      Val : Node_Id;

   begin
      --  The expression yields a null value ignoring simple flow analysis

      if Status = Is_Null then
         return True;

      --  Otherwise check whether N is a reference to an entity that appears
      --  within a conditional construct.

      elsif Is_Entity_Name (N) and then Present (Entity (N)) then

         --  First check if we are in decisive conditional

         Get_Current_Value_Condition (N, Op, Val);

         if Known_Null (Val) then
            if Op = N_Op_Eq then
               return True;
            elsif Op = N_Op_Ne then
               return False;
            end if;
         end if;

         --  If OK to do replacement, test Is_Known_Null flag

         Id := Entity (N);

         if OK_To_Do_Constant_Replacement (Id) then
            return Is_Known_Null (Id);
         end if;
      end if;

      --  Otherwise it is not possible to determine whether N yields a null
      --  value.

      return False;
   end Known_Null;

   --------------------------
   -- Known_To_Be_Assigned --
   --------------------------

   function Known_To_Be_Assigned (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);

   begin
      case Nkind (P) is

         --  Test left side of assignment

         when N_Assignment_Statement =>
            return N = Name (P);

         --  Function call arguments are never lvalues

         when N_Function_Call =>
            return False;

         --  Positional parameter for procedure or accept call

         when N_Accept_Statement
            | N_Procedure_Call_Statement
         =>
            declare
               Proc : Entity_Id;
               Form : Entity_Id;
               Act  : Node_Id;

            begin
               Proc := Get_Subprogram_Entity (P);

               if No (Proc) then
                  return False;
               end if;

               --  If we are not a list member, something is strange, so
               --  be conservative and return False.

               if not Is_List_Member (N) then
                  return False;
               end if;

               --  We are going to find the right formal by stepping forward
               --  through the formals, as we step backwards in the actuals.

               Form := First_Formal (Proc);
               Act  := N;
               loop
                  --  If no formal, something is weird, so be conservative
                  --  and return False.

                  if No (Form) then
                     return False;
                  end if;

                  Prev (Act);
                  exit when No (Act);
                  Next_Formal (Form);
               end loop;

               return Ekind (Form) /= E_In_Parameter;
            end;

         --  Named parameter for procedure or accept call

         when N_Parameter_Association =>
            declare
               Proc : Entity_Id;
               Form : Entity_Id;

            begin
               Proc := Get_Subprogram_Entity (Parent (P));

               if No (Proc) then
                  return False;
               end if;

               --  Loop through formals to find the one that matches

               Form := First_Formal (Proc);
               loop
                  --  If no matching formal, that's peculiar, some kind of
                  --  previous error, so return False to be conservative.
                  --  Actually this also happens in legal code in the case
                  --  where P is a parameter association for an Extra_Formal???

                  if No (Form) then
                     return False;
                  end if;

                  --  Else test for match

                  if Chars (Form) = Chars (Selector_Name (P)) then
                     return Ekind (Form) /= E_In_Parameter;
                  end if;

                  Next_Formal (Form);
               end loop;
            end;

         --  Test for appearing in a conversion that itself appears
         --  in an lvalue context, since this should be an lvalue.

         when N_Type_Conversion =>
            return Known_To_Be_Assigned (P);

         --  All other references are definitely not known to be modifications

         when others =>
            return False;
      end case;
   end Known_To_Be_Assigned;

   ---------------------------
   -- Last_Source_Statement --
   ---------------------------

   function Last_Source_Statement (HSS : Node_Id) return Node_Id is
      N : Node_Id;

   begin
      N := Last (Statements (HSS));
      while Present (N) loop
         exit when Comes_From_Source (N);
         Prev (N);
      end loop;

      return N;
   end Last_Source_Statement;

   ----------------------------------
   -- Matching_Static_Array_Bounds --
   ----------------------------------

   function Matching_Static_Array_Bounds
     (L_Typ : Node_Id;
      R_Typ : Node_Id) return Boolean
   is
      L_Ndims : constant Nat := Number_Dimensions (L_Typ);
      R_Ndims : constant Nat := Number_Dimensions (R_Typ);

      L_Index : Node_Id;
      R_Index : Node_Id;
      L_Low   : Node_Id;
      L_High  : Node_Id;
      L_Len   : Uint;
      R_Low   : Node_Id;
      R_High  : Node_Id;
      R_Len   : Uint;

   begin
      if L_Ndims /= R_Ndims then
         return False;
      end if;

      --  Unconstrained types do not have static bounds

      if not Is_Constrained (L_Typ) or else not Is_Constrained (R_Typ) then
         return False;
      end if;

      --  First treat specially the first dimension, as the lower bound and
      --  length of string literals are not stored like those of arrays.

      if Ekind (L_Typ) = E_String_Literal_Subtype then
         L_Low := String_Literal_Low_Bound (L_Typ);
         L_Len := String_Literal_Length (L_Typ);
      else
         L_Index := First_Index (L_Typ);
         Get_Index_Bounds (L_Index, L_Low, L_High);

         if Is_OK_Static_Expression (L_Low)
              and then
            Is_OK_Static_Expression (L_High)
         then
            if Expr_Value (L_High) < Expr_Value (L_Low) then
               L_Len := Uint_0;
            else
               L_Len := (Expr_Value (L_High) - Expr_Value (L_Low)) + 1;
            end if;
         else
            return False;
         end if;
      end if;

      if Ekind (R_Typ) = E_String_Literal_Subtype then
         R_Low := String_Literal_Low_Bound (R_Typ);
         R_Len := String_Literal_Length (R_Typ);
      else
         R_Index := First_Index (R_Typ);
         Get_Index_Bounds (R_Index, R_Low, R_High);

         if Is_OK_Static_Expression (R_Low)
              and then
            Is_OK_Static_Expression (R_High)
         then
            if Expr_Value (R_High) < Expr_Value (R_Low) then
               R_Len := Uint_0;
            else
               R_Len := (Expr_Value (R_High) - Expr_Value (R_Low)) + 1;
            end if;
         else
            return False;
         end if;
      end if;

      if (Is_OK_Static_Expression (L_Low)
            and then
          Is_OK_Static_Expression (R_Low))
        and then Expr_Value (L_Low) = Expr_Value (R_Low)
        and then L_Len = R_Len
      then
         null;
      else
         return False;
      end if;

      --  Then treat all other dimensions

      for Indx in 2 .. L_Ndims loop
         Next (L_Index);
         Next (R_Index);

         Get_Index_Bounds (L_Index, L_Low, L_High);
         Get_Index_Bounds (R_Index, R_Low, R_High);

         if (Is_OK_Static_Expression (L_Low)  and then
             Is_OK_Static_Expression (L_High) and then
             Is_OK_Static_Expression (R_Low)  and then
             Is_OK_Static_Expression (R_High))
           and then (Expr_Value (L_Low)  = Expr_Value (R_Low)
                       and then
                     Expr_Value (L_High) = Expr_Value (R_High))
         then
            null;
         else
            return False;
         end if;
      end loop;

      --  If we fall through the loop, all indexes matched

      return True;
   end Matching_Static_Array_Bounds;

   -------------------
   -- May_Be_Lvalue --
   -------------------

   function May_Be_Lvalue (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);

   begin
      case Nkind (P) is

         --  Test left side of assignment

         when N_Assignment_Statement =>
            return N = Name (P);

         --  Test prefix of component or attribute. Note that the prefix of an
         --  explicit or implicit dereference cannot be an l-value. In the case
         --  of a 'Read attribute, the reference can be an actual in the
         --  argument list of the attribute.

         when N_Attribute_Reference =>
            return (N = Prefix (P)
                     and then Name_Implies_Lvalue_Prefix (Attribute_Name (P)))
                 or else
                   Attribute_Name (P) = Name_Read;

         --  For an expanded name, the name is an lvalue if the expanded name
         --  is an lvalue, but the prefix is never an lvalue, since it is just
         --  the scope where the name is found.

         when N_Expanded_Name =>
            if N = Prefix (P) then
               return May_Be_Lvalue (P);
            else
               return False;
            end if;

         --  For a selected component A.B, A is certainly an lvalue if A.B is.
         --  B is a little interesting, if we have A.B := 3, there is some
         --  discussion as to whether B is an lvalue or not, we choose to say
         --  it is. Note however that A is not an lvalue if it is of an access
         --  type since this is an implicit dereference.

         when N_Selected_Component =>
            if N = Prefix (P)
              and then Present (Etype (N))
              and then Is_Access_Type (Etype (N))
            then
               return False;
            else
               return May_Be_Lvalue (P);
            end if;

         --  For an indexed component or slice, the index or slice bounds is
         --  never an lvalue. The prefix is an lvalue if the indexed component
         --  or slice is an lvalue, except if it is an access type, where we
         --  have an implicit dereference.

         when N_Indexed_Component
            | N_Slice
         =>
            if N /= Prefix (P)
              or else (Present (Etype (N)) and then Is_Access_Type (Etype (N)))
            then
               return False;
            else
               return May_Be_Lvalue (P);
            end if;

         --  Prefix of a reference is an lvalue if the reference is an lvalue

         when N_Reference =>
            return May_Be_Lvalue (P);

         --  Prefix of explicit dereference is never an lvalue

         when N_Explicit_Dereference =>
            return False;

         --  Positional parameter for subprogram, entry, or accept call.
         --  In older versions of Ada function call arguments are never
         --  lvalues. In Ada 2012 functions can have in-out parameters.

         when N_Accept_Statement
            | N_Entry_Call_Statement
            | N_Subprogram_Call
         =>
            if Nkind (P) = N_Function_Call and then Ada_Version < Ada_2012 then
               return False;
            end if;

            --  The following mechanism is clumsy and fragile. A single flag
            --  set in Resolve_Actuals would be preferable ???

            declare
               Proc : Entity_Id;
               Form : Entity_Id;
               Act  : Node_Id;

            begin
               Proc := Get_Subprogram_Entity (P);

               if No (Proc) then
                  return True;
               end if;

               --  If we are not a list member, something is strange, so be
               --  conservative and return True.

               if not Is_List_Member (N) then
                  return True;
               end if;

               --  We are going to find the right formal by stepping forward
               --  through the formals, as we step backwards in the actuals.

               Form := First_Formal (Proc);
               Act  := N;
               loop
                  --  If no formal, something is weird, so be conservative and
                  --  return True.

                  if No (Form) then
                     return True;
                  end if;

                  Prev (Act);
                  exit when No (Act);
                  Next_Formal (Form);
               end loop;

               return Ekind (Form) /= E_In_Parameter;
            end;

         --  Named parameter for procedure or accept call

         when N_Parameter_Association =>
            declare
               Proc : Entity_Id;
               Form : Entity_Id;

            begin
               Proc := Get_Subprogram_Entity (Parent (P));

               if No (Proc) then
                  return True;
               end if;

               --  Loop through formals to find the one that matches

               Form := First_Formal (Proc);
               loop
                  --  If no matching formal, that's peculiar, some kind of
                  --  previous error, so return True to be conservative.
                  --  Actually happens with legal code for an unresolved call
                  --  where we may get the wrong homonym???

                  if No (Form) then
                     return True;
                  end if;

                  --  Else test for match

                  if Chars (Form) = Chars (Selector_Name (P)) then
                     return Ekind (Form) /= E_In_Parameter;
                  end if;

                  Next_Formal (Form);
               end loop;
            end;

         --  Test for appearing in a conversion that itself appears in an
         --  lvalue context, since this should be an lvalue.

         when N_Type_Conversion =>
            return May_Be_Lvalue (P);

         --  Test for appearance in object renaming declaration

         when N_Object_Renaming_Declaration =>
            return True;

         --  All other references are definitely not lvalues

         when others =>
            return False;
      end case;
   end May_Be_Lvalue;

   -----------------------
   -- Mark_Coextensions --
   -----------------------

   procedure Mark_Coextensions (Context_Nod : Node_Id; Root_Nod : Node_Id) is
      Is_Dynamic : Boolean;
      --  Indicates whether the context causes nested coextensions to be
      --  dynamic or static

      function Mark_Allocator (N : Node_Id) return Traverse_Result;
      --  Recognize an allocator node and label it as a dynamic coextension

      --------------------
      -- Mark_Allocator --
      --------------------

      function Mark_Allocator (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Allocator then
            if Is_Dynamic then
               Set_Is_Dynamic_Coextension (N);

            --  If the allocator expression is potentially dynamic, it may
            --  be expanded out of order and require dynamic allocation
            --  anyway, so we treat the coextension itself as dynamic.
            --  Potential optimization ???

            elsif Nkind (Expression (N)) = N_Qualified_Expression
              and then Nkind (Expression (Expression (N))) = N_Op_Concat
            then
               Set_Is_Dynamic_Coextension (N);
            else
               Set_Is_Static_Coextension (N);
            end if;
         end if;

         return OK;
      end Mark_Allocator;

      procedure Mark_Allocators is new Traverse_Proc (Mark_Allocator);

   --  Start of processing for Mark_Coextensions

   begin
      --  An allocator that appears on the right-hand side of an assignment is
      --  treated as a potentially dynamic coextension when the right-hand side
      --  is an allocator or a qualified expression.

      --    Obj := new ...'(new Coextension ...);

      if Nkind (Context_Nod) = N_Assignment_Statement then
         Is_Dynamic :=
           Nkind_In (Expression (Context_Nod), N_Allocator,
                                               N_Qualified_Expression);

      --  An allocator that appears within the expression of a simple return
      --  statement is treated as a potentially dynamic coextension when the
      --  expression is either aggregate, allocator, or qualified expression.

      --    return (new Coextension ...);
      --    return new ...'(new Coextension ...);

      elsif Nkind (Context_Nod) = N_Simple_Return_Statement then
         Is_Dynamic :=
           Nkind_In (Expression (Context_Nod), N_Aggregate,
                                               N_Allocator,
                                               N_Qualified_Expression);

      --  An allocator that appears within the initialization expression of an
      --  object declaration is considered a potentially dynamic coextension
      --  when the initialization expression is an allocator or a qualified
      --  expression.

      --    Obj : ... := new ...'(new Coextension ...);

      --  A similar case arises when the object declaration is part of an
      --  extended return statement.

      --    return Obj : ... := new ...'(new Coextension ...);
      --    return Obj : ... := (new Coextension ...);

      elsif Nkind (Context_Nod) = N_Object_Declaration then
         Is_Dynamic :=
           Nkind_In (Root_Nod, N_Allocator, N_Qualified_Expression)
             or else
               Nkind (Parent (Context_Nod)) = N_Extended_Return_Statement;

      --  This routine should not be called with constructs that cannot contain
      --  coextensions.

      else
         raise Program_Error;
      end if;

      Mark_Allocators (Root_Nod);
   end Mark_Coextensions;

   -----------------
   -- Might_Raise --
   -----------------

   function Might_Raise (N : Node_Id) return Boolean is
      Result : Boolean := False;

      function Process (N : Node_Id) return Traverse_Result;
      --  Set Result to True if we find something that could raise an exception

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         if Nkind_In (N, N_Procedure_Call_Statement,
                         N_Function_Call,
                         N_Raise_Statement,
                         N_Raise_Constraint_Error,
                         N_Raise_Program_Error,
                         N_Raise_Storage_Error)
         then
            Result := True;
            return Abandon;
         else
            return OK;
         end if;
      end Process;

      procedure Set_Result is new Traverse_Proc (Process);

   --  Start of processing for Might_Raise

   begin
      --  False if exceptions can't be propagated

      if No_Exception_Handlers_Set then
         return False;
      end if;

      --  If the checks handled by the back end are not disabled, we cannot
      --  ensure that no exception will be raised.

      if not Access_Checks_Suppressed (Empty)
        or else not Discriminant_Checks_Suppressed (Empty)
        or else not Range_Checks_Suppressed (Empty)
        or else not Index_Checks_Suppressed (Empty)
        or else Opt.Stack_Checking_Enabled
      then
         return True;
      end if;

      Set_Result (N);
      return Result;
   end Might_Raise;

   --------------------------------
   -- Nearest_Enclosing_Instance --
   --------------------------------

   function Nearest_Enclosing_Instance (E : Entity_Id) return Entity_Id is
      Inst : Entity_Id;

   begin
      Inst := Scope (E);
      while Present (Inst) and then Inst /= Standard_Standard loop
         if Is_Generic_Instance (Inst) then
            return Inst;
         end if;

         Inst := Scope (Inst);
      end loop;

      return Empty;
   end Nearest_Enclosing_Instance;

   ----------------------
   -- Needs_One_Actual --
   ----------------------

   function Needs_One_Actual (E : Entity_Id) return Boolean is
      Formal : Entity_Id;

   begin
      --  Ada 2005 or later, and formals present. The first formal must be
      --  of a type that supports prefix notation: a controlling argument,
      --  a class-wide type, or an access to such.

      if Ada_Version >= Ada_2005
        and then Present (First_Formal (E))
        and then No (Default_Value (First_Formal (E)))
        and then
          (Is_Controlling_Formal (First_Formal (E))
            or else Is_Class_Wide_Type (Etype (First_Formal (E)))
            or else Is_Anonymous_Access_Type (Etype (First_Formal (E))))
      then
         Formal := Next_Formal (First_Formal (E));
         while Present (Formal) loop
            if No (Default_Value (Formal)) then
               return False;
            end if;

            Next_Formal (Formal);
         end loop;

         return True;

      --  Ada 83/95 or no formals

      else
         return False;
      end if;
   end Needs_One_Actual;

   ------------------------
   -- New_Copy_List_Tree --
   ------------------------

   function New_Copy_List_Tree (List : List_Id) return List_Id is
      NL : List_Id;
      E  : Node_Id;

   begin
      if List = No_List then
         return No_List;

      else
         NL := New_List;
         E := First (List);

         while Present (E) loop
            Append (New_Copy_Tree (E), NL);
            E := Next (E);
         end loop;

         return NL;
      end if;
   end New_Copy_List_Tree;

   --------------------------------------------------
   -- New_Copy_Tree Auxiliary Data and Subprograms --
   --------------------------------------------------

   use Atree.Unchecked_Access;
   use Atree_Private_Part;

   --  Our approach here requires a two pass traversal of the tree. The
   --  first pass visits all nodes that eventually will be copied looking
   --  for defining Itypes. If any defining Itypes are found, then they are
   --  copied, and an entry is added to the replacement map. In the second
   --  phase, the tree is copied, using the replacement map to replace any
   --  Itype references within the copied tree.

   --  The following hash tables are used to speed up access to the map. They
   --  are declared at library level to avoid elaborating them for every call
   --  to New_Copy_Tree. This can save up to 2% of the entire compilation time
   --  spent in the front end.

   subtype NCT_Header_Num is Int range 0 .. 511;
   --  Defines range of headers in hash tables (512 headers)

   function New_Copy_Hash (E : Entity_Id) return NCT_Header_Num;
   --  Hash function used for hash operations

   -------------------
   -- New_Copy_Hash --
   -------------------

   function New_Copy_Hash (E : Entity_Id) return NCT_Header_Num is
   begin
      return Nat (E) mod (NCT_Header_Num'Last + 1);
   end New_Copy_Hash;

   ---------------
   -- NCT_Assoc --
   ---------------

   --  The hash table NCT_Assoc associates old entities in the table with their
   --  corresponding new entities (i.e. the pairs of entries presented in the
   --  original Map argument are Key-Element pairs).

   package NCT_Assoc is new Simple_HTable (
     Header_Num => NCT_Header_Num,
     Element    => Entity_Id,
     No_Element => Empty,
     Key        => Entity_Id,
     Hash       => New_Copy_Hash,
     Equal      => Types."=");

   ---------------------
   -- NCT_Itype_Assoc --
   ---------------------

   --  The hash table NCT_Itype_Assoc contains entries only for those old
   --  nodes which have a non-empty Associated_Node_For_Itype set. The key
   --  is the associated node, and the element is the new node itself (NOT
   --  the associated node for the new node).

   package NCT_Itype_Assoc is new Simple_HTable (
     Header_Num => NCT_Header_Num,
     Element    => Node_Or_Entity_Id,
     No_Element => Empty,
     Key        => Entity_Id,
     Hash       => New_Copy_Hash,
     Equal      => Types."=");

   -------------------
   -- New_Copy_Tree --
   -------------------

   function New_Copy_Tree
     (Source    : Node_Id;
      Map       : Elist_Id   := No_Elist;
      New_Sloc  : Source_Ptr := No_Location;
      New_Scope : Entity_Id  := Empty) return Node_Id
   is
      EWA_Level : Nat := 0;
      --  By default, copying of defining identifiers is prohibited because
      --  this would introduce an entirely new entity into the tree. The
      --  exception to this general rule is declaration of constants and
      --  variables located in Expression_With_Action nodes.

      EWA_Inner_Scope_Level : Nat := 0;
      --  Level of internal scope of defined in EWAs. Used to avoid creating
      --  variables for declarations located in blocks or subprograms defined
      --  in Expression_With_Action nodes.

      NCT_Hash_Tables_Used : Boolean := False;
      --  Set to True if hash tables are in use. It is intended to speed up the
      --  common case, which is no hash tables in use. This can save up to 8%
      --  of the entire compilation time spent in the front end.

      function Assoc (N : Node_Or_Entity_Id) return Node_Id;
      --  Called during second phase to map entities into their corresponding
      --  copies using the hash table. If the argument is not an entity, or is
      --  not in the hash table, then it is returned unchanged.

      procedure Build_NCT_Hash_Tables;
      --  Builds hash tables

      function Copy_Elist_With_Replacement
        (Old_Elist : Elist_Id) return Elist_Id;
      --  Called during second phase to copy element list doing replacements

      procedure Copy_Entity_With_Replacement (New_Entity : Entity_Id);
      --  Called during the second phase to process a copied Entity. The actual
      --  copy happened during the first phase (so that we could make the entry
      --  in the mapping), but we still have to deal with the descendants of
      --  the copied Entity and copy them where necessary.

      function Copy_List_With_Replacement (Old_List : List_Id) return List_Id;
      --  Called during second phase to copy list doing replacements

      function Copy_Node_With_Replacement (Old_Node : Node_Id) return Node_Id;
      --  Called during second phase to copy node doing replacements

      function In_Map (E : Entity_Id) return Boolean;
      --  Return True if E is one of the old entities specified in the set of
      --  mappings to be applied to entities in the tree (i.e. Map).

      procedure Visit_Elist (E : Elist_Id);
      --  Called during first phase to visit all elements of an Elist

      procedure Visit_Entity (Old_Entity : Entity_Id);
      --  Called during first phase to visit subsidiary fields of a defining
      --  entity which is not an itype, and also create a copy and make an
      --  entry in the replacement map for the new copy.

      procedure Visit_Field (F : Union_Id; N : Node_Id);
      --  Visit a single field, recursing to call Visit_Node or Visit_List if
      --  the field is a syntactic descendant of the current node (i.e. its
      --  parent is Node N).

      procedure Visit_Itype (Old_Itype : Entity_Id);
      --  Called during first phase to visit subsidiary fields of a defining
      --  Itype, and also create a copy and make an entry in the replacement
      --  map for the new copy.

      procedure Visit_List (L : List_Id);
      --  Called during first phase to visit all elements of a List

      procedure Visit_Node (N : Node_Or_Entity_Id);
      --  Called during first phase to visit a node and all its subtrees

      -----------
      -- Assoc --
      -----------

      function Assoc (N : Node_Or_Entity_Id) return Node_Id is
         Ent : Entity_Id;

      begin
         if Nkind (N) not in N_Entity or else not NCT_Hash_Tables_Used then
            return N;

         else
            Ent := NCT_Assoc.Get (Entity_Id (N));

            if Present (Ent) then
               return Ent;
            end if;
         end if;

         return N;
      end Assoc;

      ---------------------------
      -- Build_NCT_Hash_Tables --
      ---------------------------

      procedure Build_NCT_Hash_Tables is
         Assoc : Entity_Id;
         Elmt  : Elmt_Id;
         Key   : Entity_Id;
         Value : Entity_Id;

      begin
         if No (Map) then
            return;
         end if;

         --  Clear both hash tables associated with entry replication since
         --  multiple calls to New_Copy_Tree could cause multiple collisions
         --  and produce long linked lists in individual buckets.

         NCT_Assoc.Reset;
         NCT_Itype_Assoc.Reset;

         Elmt := First_Elmt (Map);
         while Present (Elmt) loop

            --  Extract a (key, value) pair from the map

            Key := Node (Elmt);
            Next_Elmt (Elmt);
            Value := Node (Elmt);

            --  Add the pair in the association hash table

            NCT_Assoc.Set (Key, Value);

            --  Add a link between the associated node of the old Itype and the
            --  new Itype, for updating later when node is copied.

            if Is_Type (Key) then
               Assoc := Associated_Node_For_Itype (Key);

               if Present (Assoc) then
                  NCT_Itype_Assoc.Set (Assoc, Value);
               end if;
            end if;

            Next_Elmt (Elmt);
         end loop;

         NCT_Hash_Tables_Used := True;
      end Build_NCT_Hash_Tables;

      ---------------------------------
      -- Copy_Elist_With_Replacement --
      ---------------------------------

      function Copy_Elist_With_Replacement
        (Old_Elist : Elist_Id) return Elist_Id
      is
         M         : Elmt_Id;
         New_Elist : Elist_Id;

      begin
         if No (Old_Elist) then
            return No_Elist;

         else
            New_Elist := New_Elmt_List;

            M := First_Elmt (Old_Elist);
            while Present (M) loop
               Append_Elmt (Copy_Node_With_Replacement (Node (M)), New_Elist);
               Next_Elmt (M);
            end loop;
         end if;

         return New_Elist;
      end Copy_Elist_With_Replacement;

      ----------------------------------
      -- Copy_Entity_With_Replacement --
      ----------------------------------

      --  This routine exactly parallels its phase one analog Visit_Itype

      procedure Copy_Entity_With_Replacement (New_Entity : Entity_Id) is
      begin
         --  Translate Next_Entity, Scope, and Etype fields, in case they
         --  reference entities that have been mapped into copies.

         Set_Next_Entity (New_Entity, Assoc (Next_Entity (New_Entity)));
         Set_Etype       (New_Entity, Assoc (Etype       (New_Entity)));

         if Present (New_Scope) then
            Set_Scope    (New_Entity, New_Scope);
         else
            Set_Scope    (New_Entity, Assoc (Scope       (New_Entity)));
         end if;

         --  Copy referenced fields

         if Is_Discrete_Type (New_Entity) then
            Set_Scalar_Range (New_Entity,
              Copy_Node_With_Replacement (Scalar_Range (New_Entity)));

         elsif Has_Discriminants (Base_Type (New_Entity)) then
            Set_Discriminant_Constraint (New_Entity,
              Copy_Elist_With_Replacement
                (Discriminant_Constraint (New_Entity)));

         elsif Is_Array_Type (New_Entity) then
            if Present (First_Index (New_Entity)) then
               Set_First_Index (New_Entity,
                 First (Copy_List_With_Replacement
                         (List_Containing (First_Index (New_Entity)))));
            end if;

            if Is_Packed (New_Entity) then
               Set_Packed_Array_Impl_Type (New_Entity,
                 Copy_Node_With_Replacement
                   (Packed_Array_Impl_Type (New_Entity)));
            end if;
         end if;
      end Copy_Entity_With_Replacement;

      --------------------------------
      -- Copy_List_With_Replacement --
      --------------------------------

      function Copy_List_With_Replacement
        (Old_List : List_Id) return List_Id
      is
         New_List : List_Id;
         E        : Node_Id;

      begin
         if Old_List = No_List then
            return No_List;

         else
            New_List := Empty_List;

            E := First (Old_List);
            while Present (E) loop
               Append (Copy_Node_With_Replacement (E), New_List);
               Next (E);
            end loop;

            return New_List;
         end if;
      end Copy_List_With_Replacement;

      --------------------------------
      -- Copy_Node_With_Replacement --
      --------------------------------

      function Copy_Node_With_Replacement
        (Old_Node : Node_Id) return Node_Id
      is
         New_Node : Node_Id;

         procedure Adjust_Named_Associations
           (Old_Node : Node_Id;
            New_Node : Node_Id);
         --  If a call node has named associations, these are chained through
         --  the First_Named_Actual, Next_Named_Actual links. These must be
         --  propagated separately to the new parameter list, because these
         --  are not syntactic fields.

         function Copy_Field_With_Replacement
           (Field : Union_Id) return Union_Id;
         --  Given Field, which is a field of Old_Node, return a copy of it
         --  if it is a syntactic field (i.e. its parent is Node), setting
         --  the parent of the copy to poit to New_Node. Otherwise returns
         --  the field (possibly mapped if it is an entity).

         -------------------------------
         -- Adjust_Named_Associations --
         -------------------------------

         procedure Adjust_Named_Associations
           (Old_Node : Node_Id;
            New_Node : Node_Id)
         is
            Old_E : Node_Id;
            New_E : Node_Id;

            Old_Next : Node_Id;
            New_Next : Node_Id;

         begin
            Old_E := First (Parameter_Associations (Old_Node));
            New_E := First (Parameter_Associations (New_Node));
            while Present (Old_E) loop
               if Nkind (Old_E) = N_Parameter_Association
                 and then Present (Next_Named_Actual (Old_E))
               then
                  if First_Named_Actual (Old_Node) =
                       Explicit_Actual_Parameter (Old_E)
                  then
                     Set_First_Named_Actual
                       (New_Node, Explicit_Actual_Parameter (New_E));
                  end if;

                  --  Now scan parameter list from the beginning, to locate
                  --  next named actual, which can be out of order.

                  Old_Next := First (Parameter_Associations (Old_Node));
                  New_Next := First (Parameter_Associations (New_Node));
                  while Nkind (Old_Next) /= N_Parameter_Association
                    or else Explicit_Actual_Parameter (Old_Next) /=
                                              Next_Named_Actual (Old_E)
                  loop
                     Next (Old_Next);
                     Next (New_Next);
                  end loop;

                  Set_Next_Named_Actual
                    (New_E, Explicit_Actual_Parameter (New_Next));
               end if;

               Next (Old_E);
               Next (New_E);
            end loop;
         end Adjust_Named_Associations;

         ---------------------------------
         -- Copy_Field_With_Replacement --
         ---------------------------------

         function Copy_Field_With_Replacement
           (Field : Union_Id) return Union_Id
         is
         begin
            if Field = Union_Id (Empty) then
               return Field;

            elsif Field in Node_Range then
               declare
                  Old_N : constant Node_Id := Node_Id (Field);
                  New_N : Node_Id;

               begin
                  --  If syntactic field, as indicated by the parent pointer
                  --  being set, then copy the referenced node recursively.

                  if Parent (Old_N) = Old_Node then
                     New_N := Copy_Node_With_Replacement (Old_N);

                     if New_N /= Old_N then
                        Set_Parent (New_N, New_Node);
                     end if;

                  --  For semantic fields, update possible entity reference
                  --  from the replacement map.

                  else
                     New_N := Assoc (Old_N);
                  end if;

                  return Union_Id (New_N);
               end;

            elsif Field in List_Range then
               declare
                  Old_L : constant List_Id := List_Id (Field);
                  New_L : List_Id;

               begin
                  --  If syntactic field, as indicated by the parent pointer,
                  --  then recursively copy the entire referenced list.

                  if Parent (Old_L) = Old_Node then
                     New_L := Copy_List_With_Replacement (Old_L);
                     Set_Parent (New_L, New_Node);

                  --  For semantic list, just returned unchanged

                  else
                     New_L := Old_L;
                  end if;

                  return Union_Id (New_L);
               end;

            --  Anything other than a list or a node is returned unchanged

            else
               return Field;
            end if;
         end Copy_Field_With_Replacement;

      --  Start of processing for Copy_Node_With_Replacement

      begin
         if Old_Node <= Empty_Or_Error then
            return Old_Node;

         elsif Nkind (Old_Node) in N_Entity then
            return Assoc (Old_Node);

         else
            New_Node := New_Copy (Old_Node);

            --  If the node we are copying is the associated node of a
            --  previously copied Itype, then adjust the associated node
            --  of the copy of that Itype accordingly.

            declare
               Ent : constant Entity_Id := NCT_Itype_Assoc.Get (Old_Node);

            begin
               if Present (Ent) then
                  Set_Associated_Node_For_Itype (Ent, New_Node);
               end if;
            end;

            --  Recursively copy descendants

            Set_Field1
              (New_Node, Copy_Field_With_Replacement (Field1 (New_Node)));
            Set_Field2
              (New_Node, Copy_Field_With_Replacement (Field2 (New_Node)));
            Set_Field3
              (New_Node, Copy_Field_With_Replacement (Field3 (New_Node)));
            Set_Field4
              (New_Node, Copy_Field_With_Replacement (Field4 (New_Node)));
            Set_Field5
              (New_Node, Copy_Field_With_Replacement (Field5 (New_Node)));

            --  Adjust Sloc of new node if necessary

            if New_Sloc /= No_Location then
               Set_Sloc (New_Node, New_Sloc);

               --  If we adjust the Sloc, then we are essentially making a
               --  completely new node, so the Comes_From_Source flag should
               --  be reset to the proper default value.

               Set_Comes_From_Source
                 (New_Node, Default_Node.Comes_From_Source);
            end if;

            --  If the node is a call and has named associations, set the
            --  corresponding links in the copy.

            if Nkind_In (Old_Node, N_Entry_Call_Statement,
                                   N_Function_Call,
                                   N_Procedure_Call_Statement)
              and then Present (First_Named_Actual (Old_Node))
            then
               Adjust_Named_Associations (Old_Node, New_Node);
            end if;

            --  Reset First_Real_Statement for Handled_Sequence_Of_Statements.
            --  The replacement mechanism applies to entities, and is not used
            --  here. Eventually we may need a more general graph-copying
            --  routine. For now, do a sequential search to find desired node.

            if Nkind (Old_Node) = N_Handled_Sequence_Of_Statements
              and then Present (First_Real_Statement (Old_Node))
            then
               declare
                  Old_F  : constant Node_Id := First_Real_Statement (Old_Node);
                  N1, N2 : Node_Id;

               begin
                  N1 := First (Statements (Old_Node));
                  N2 := First (Statements (New_Node));

                  while N1 /= Old_F loop
                     Next (N1);
                     Next (N2);
                  end loop;

                  Set_First_Real_Statement (New_Node, N2);
               end;
            end if;
         end if;

         --  All done, return copied node

         return New_Node;
      end Copy_Node_With_Replacement;

      ------------
      -- In_Map --
      ------------

      function In_Map (E : Entity_Id) return Boolean is
         Elmt : Elmt_Id;
         Ent  : Entity_Id;

      begin
         if Present (Map) then
            Elmt := First_Elmt (Map);
            while Present (Elmt) loop
               Ent := Node (Elmt);

               if Ent = E then
                  return True;
               end if;

               Next_Elmt (Elmt);
               Next_Elmt (Elmt);
            end loop;
         end if;

         return False;
      end In_Map;

      -----------------
      -- Visit_Elist --
      -----------------

      procedure Visit_Elist (E : Elist_Id) is
         Elmt : Elmt_Id;
      begin
         if Present (E) then
            Elmt := First_Elmt (E);

            while Elmt /= No_Elmt loop
               Visit_Node (Node (Elmt));
               Next_Elmt (Elmt);
            end loop;
         end if;
      end Visit_Elist;

      ------------------
      -- Visit_Entity --
      ------------------

      procedure Visit_Entity (Old_Entity : Entity_Id) is
         New_E : Entity_Id;

      begin
         pragma Assert (not Is_Itype (Old_Entity));
         pragma Assert (Nkind (Old_Entity) in N_Entity);

         --  Do not duplicate an entity when it is declared within an inner
         --  scope enclosed by an expression with actions.

         if EWA_Inner_Scope_Level > 0 then
            return;

         --  Entity duplication is currently performed only for objects and
         --  types. Relaxing this restriction leads to a performance penalty.

         elsif Ekind_In (Old_Entity, E_Constant, E_Variable) then
            null;

         elsif Is_Type (Old_Entity) then
            null;

         else
            return;
         end if;

         New_E := New_Copy (Old_Entity);

         --  The new entity has all the attributes of the old one, however it
         --  requires a new name for debugging purposes.

         Set_Chars (New_E, New_Internal_Name ('T'));

         --  Add new association to map

         NCT_Assoc.Set (Old_Entity, New_E);
         NCT_Hash_Tables_Used := True;

         --  Visit descendants that eventually get copied

         Visit_Field (Union_Id (Etype (Old_Entity)), Old_Entity);
      end Visit_Entity;

      -----------------
      -- Visit_Field --
      -----------------

      procedure Visit_Field (F : Union_Id; N : Node_Id) is
      begin
         if F = Union_Id (Empty) then
            return;

         elsif F in Node_Range then

            --  Copy node if it is syntactic, i.e. its parent pointer is
            --  set to point to the field that referenced it (certain
            --  Itypes will also meet this criterion, which is fine, since
            --  these are clearly Itypes that do need to be copied, since
            --  we are copying their parent.)

            if Parent (Node_Id (F)) = N then
               Visit_Node (Node_Id (F));
               return;

            --  Another case, if we are pointing to an Itype, then we want
            --  to copy it if its associated node is somewhere in the tree
            --  being copied.

            --  Note: the exclusion of self-referential copies is just an
            --  optimization, since the search of the already copied list
            --  would catch it, but it is a common case (Etype pointing to
            --  itself for an Itype that is a base type).

            elsif Nkind (Node_Id (F)) in N_Entity
              and then Is_Itype (Entity_Id (F))
              and then Node_Id (F) /= N
            then
               declare
                  P : Node_Id;

               begin
                  P := Associated_Node_For_Itype (Node_Id (F));
                  while Present (P) loop
                     if P = Source then
                        Visit_Node (Node_Id (F));
                        return;
                     else
                        P := Parent (P);
                     end if;
                  end loop;

                  --  An Itype whose parent is not being copied definitely
                  --  should NOT be copied, since it does not belong in any
                  --  sense to the copied subtree.

                  return;
               end;
            end if;

         elsif F in List_Range and then Parent (List_Id (F)) = N then
            Visit_List (List_Id (F));
            return;
         end if;
      end Visit_Field;

      -----------------
      -- Visit_Itype --
      -----------------

      procedure Visit_Itype (Old_Itype : Entity_Id) is
         New_Itype : Entity_Id;
         Ent       : Entity_Id;

      begin
         --  Itypes that describe the designated type of access to subprograms
         --  have the structure of subprogram declarations, with signatures,
         --  etc. Either we duplicate the signatures completely, or choose to
         --  share such itypes, which is fine because their elaboration will
         --  have no side effects.

         if Ekind (Old_Itype) = E_Subprogram_Type then
            return;
         end if;

         New_Itype := New_Copy (Old_Itype);

         --  The new Itype has all the attributes of the old one, and we
         --  just copy the contents of the entity. However, the back-end
         --  needs different names for debugging purposes, so we create a
         --  new internal name for it in all cases.

         Set_Chars (New_Itype, New_Internal_Name ('T'));

         --  If our associated node is an entity that has already been copied,
         --  then set the associated node of the copy to point to the right
         --  copy. If we have copied an Itype that is itself the associated
         --  node of some previously copied Itype, then we set the right
         --  pointer in the other direction.

         Ent := NCT_Assoc.Get (Associated_Node_For_Itype (Old_Itype));

         if Present (Ent) then
            Set_Associated_Node_For_Itype (New_Itype, Ent);
         end if;

         Ent := NCT_Itype_Assoc.Get (Old_Itype);

         if Present (Ent) then
            Set_Associated_Node_For_Itype (Ent, New_Itype);

         --  If the hash table has no association for this Itype and its
         --  associated node, enter one now.

         else
            NCT_Itype_Assoc.Set
              (Associated_Node_For_Itype (Old_Itype), New_Itype);
         end if;

         if Present (Freeze_Node (New_Itype)) then
            Set_Is_Frozen (New_Itype, False);
            Set_Freeze_Node (New_Itype, Empty);
         end if;

         --  Add new association to map

         NCT_Assoc.Set (Old_Itype, New_Itype);
         NCT_Hash_Tables_Used := True;

         --  If a record subtype is simply copied, the entity list will be
         --  shared. Thus cloned_Subtype must be set to indicate the sharing.

         if Ekind_In (Old_Itype, E_Class_Wide_Subtype, E_Record_Subtype) then
            Set_Cloned_Subtype (New_Itype, Old_Itype);
         end if;

         --  Visit descendants that eventually get copied

         Visit_Field (Union_Id (Etype (Old_Itype)), Old_Itype);

         if Is_Discrete_Type (Old_Itype) then
            Visit_Field (Union_Id (Scalar_Range (Old_Itype)), Old_Itype);

         elsif Has_Discriminants (Base_Type (Old_Itype)) then
            --  ??? This should involve call to Visit_Field
            Visit_Elist (Discriminant_Constraint (Old_Itype));

         elsif Is_Array_Type (Old_Itype) then
            if Present (First_Index (Old_Itype)) then
               Visit_Field
                 (Union_Id (List_Containing (First_Index (Old_Itype))),
                  Old_Itype);
            end if;

            if Is_Packed (Old_Itype) then
               Visit_Field
                 (Union_Id (Packed_Array_Impl_Type (Old_Itype)), Old_Itype);
            end if;
         end if;
      end Visit_Itype;

      ----------------
      -- Visit_List --
      ----------------

      procedure Visit_List (L : List_Id) is
         N : Node_Id;
      begin
         if L /= No_List then
            N := First (L);

            while Present (N) loop
               Visit_Node (N);
               Next (N);
            end loop;
         end if;
      end Visit_List;

      ----------------
      -- Visit_Node --
      ----------------

      procedure Visit_Node (N : Node_Or_Entity_Id) is
      begin
         if Nkind (N) = N_Expression_With_Actions then
            EWA_Level := EWA_Level + 1;

         elsif EWA_Level > 0
           and then Nkind_In (N, N_Block_Statement,
                                 N_Subprogram_Body,
                                 N_Subprogram_Declaration)
         then
            EWA_Inner_Scope_Level := EWA_Inner_Scope_Level + 1;

         --  Handle case of an Itype, which must be copied

         elsif Nkind (N) in N_Entity and then Is_Itype (N) then

            --  Nothing to do if already in the list. This can happen with an
            --  Itype entity that appears more than once in the tree. Note that
            --  we do not want to visit descendants in this case.

            if Present (NCT_Assoc.Get (Entity_Id (N))) then
               return;
            end if;

            Visit_Itype (N);

         --  Handle defining entities in Expression_With_Action nodes

         elsif Nkind (N) in N_Entity and then EWA_Level > 0 then

            --  Nothing to do if already in the hash table

            if Present (NCT_Assoc.Get (Entity_Id (N))) then
               return;
            end if;

            Visit_Entity (N);
         end if;

         --  Visit descendants

         Visit_Field (Field1 (N), N);
         Visit_Field (Field2 (N), N);
         Visit_Field (Field3 (N), N);
         Visit_Field (Field4 (N), N);
         Visit_Field (Field5 (N), N);

         if EWA_Level > 0
           and then Nkind_In (N, N_Block_Statement,
                                 N_Subprogram_Body,
                                 N_Subprogram_Declaration)
         then
            EWA_Inner_Scope_Level := EWA_Inner_Scope_Level - 1;

         elsif Nkind (N) = N_Expression_With_Actions then
            EWA_Level := EWA_Level - 1;
         end if;
      end Visit_Node;

   --  Start of processing for New_Copy_Tree

   begin
      Build_NCT_Hash_Tables;

      --  Hash table set up if required, now start phase one by visiting top
      --  node (we will recursively visit the descendants).

      Visit_Node (Source);

      --  Now the second phase of the copy can start. First we process all the
      --  mapped entities, copying their descendants.

      if NCT_Hash_Tables_Used then
         declare
            Old_E : Entity_Id := Empty;
            New_E : Entity_Id;

         begin
            NCT_Assoc.Get_First (Old_E, New_E);
            while Present (New_E) loop

               --  Skip entities that were not created in the first phase
               --  (that is, old entities specified by the caller in the set of
               --  mappings to be applied to the tree).

               if Is_Itype (New_E)
                 or else No (Map)
                 or else not In_Map (Old_E)
               then
                  Copy_Entity_With_Replacement (New_E);
               end if;

               NCT_Assoc.Get_Next (Old_E, New_E);
            end loop;
         end;
      end if;

      --  Now we can copy the actual tree

      declare
         Result : constant Node_Id := Copy_Node_With_Replacement (Source);

      begin
         if NCT_Hash_Tables_Used then
            NCT_Assoc.Reset;
            NCT_Itype_Assoc.Reset;
         end if;

         return Result;
      end;
   end New_Copy_Tree;

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
      N : constant Entity_Id := Make_Temporary (Sloc_Value, Id_Char);

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
      --  If we are pointing at a positional parameter, it is a member of a
      --  node list (the list of parameters), and the next parameter is the
      --  next node on the list, unless we hit a parameter association, then
      --  we shift to using the chain whose head is the First_Named_Actual in
      --  the parent, and then is threaded using the Next_Named_Actual of the
      --  Parameter_Association. All this fiddling is because the original node
      --  list is in the textual call order, and what we need is the
      --  declaration order.

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

   ----------------------------------
   -- New_Requires_Transient_Scope --
   ----------------------------------

   function New_Requires_Transient_Scope (Id : Entity_Id) return Boolean is
      function Caller_Known_Size_Record (Typ : Entity_Id) return Boolean;
      --  This is called for untagged records and protected types, with
      --  nondefaulted discriminants. Returns True if the size of function
      --  results is known at the call site, False otherwise. Returns False
      --  if there is a variant part that depends on the discriminants of
      --  this type, or if there is an array constrained by the discriminants
      --  of this type. ???Currently, this is overly conservative (the array
      --  could be nested inside some other record that is constrained by
      --  nondiscriminants). That is, the recursive calls are too conservative.

      function Large_Max_Size_Mutable (Typ : Entity_Id) return Boolean;
      --  Returns True if Typ is a nonlimited record with defaulted
      --  discriminants whose max size makes it unsuitable for allocating on
      --  the primary stack.

      ------------------------------
      -- Caller_Known_Size_Record --
      ------------------------------

      function Caller_Known_Size_Record (Typ : Entity_Id) return Boolean is
         pragma Assert (Typ = Underlying_Type (Typ));

      begin
         if Has_Variant_Part (Typ) and then not Is_Definite_Subtype (Typ) then
            return False;
         end if;

         declare
            Comp : Entity_Id;

         begin
            Comp := First_Entity (Typ);
            while Present (Comp) loop

               --  Only look at E_Component entities. No need to look at
               --  E_Discriminant entities, and we must ignore internal
               --  subtypes generated for constrained components.

               if Ekind (Comp) = E_Component then
                  declare
                     Comp_Type : constant Entity_Id :=
                                   Underlying_Type (Etype (Comp));

                  begin
                     if Is_Record_Type (Comp_Type)
                           or else
                        Is_Protected_Type (Comp_Type)
                     then
                        if not Caller_Known_Size_Record (Comp_Type) then
                           return False;
                        end if;

                     elsif Is_Array_Type (Comp_Type) then
                        if Size_Depends_On_Discriminant (Comp_Type) then
                           return False;
                        end if;
                     end if;
                  end;
               end if;

               Next_Entity (Comp);
            end loop;
         end;

         return True;
      end Caller_Known_Size_Record;

      ------------------------------
      -- Large_Max_Size_Mutable --
      ------------------------------

      function Large_Max_Size_Mutable (Typ : Entity_Id) return Boolean is
         pragma Assert (Typ = Underlying_Type (Typ));

         function Is_Large_Discrete_Type (T : Entity_Id) return Boolean;
         --  Returns true if the discrete type T has a large range

         ----------------------------
         -- Is_Large_Discrete_Type --
         ----------------------------

         function Is_Large_Discrete_Type (T : Entity_Id) return Boolean is
            Threshold : constant Int := 16;
            --  Arbitrary threshold above which we consider it "large". We want
            --  a fairly large threshold, because these large types really
            --  shouldn't have default discriminants in the first place, in
            --  most cases.

         begin
            return UI_To_Int (RM_Size (T)) > Threshold;
         end Is_Large_Discrete_Type;

      --  Start of processing for Large_Max_Size_Mutable

      begin
         if Is_Record_Type (Typ)
           and then not Is_Limited_View (Typ)
           and then Has_Defaulted_Discriminants (Typ)
         then
            --  Loop through the components, looking for an array whose upper
            --  bound(s) depends on discriminants, where both the subtype of
            --  the discriminant and the index subtype are too large.

            declare
               Comp : Entity_Id;

            begin
               Comp := First_Entity (Typ);
               while Present (Comp) loop
                  if Ekind (Comp) = E_Component then
                     declare
                        Comp_Type : constant Entity_Id :=
                                      Underlying_Type (Etype (Comp));

                        Hi   : Node_Id;
                        Indx : Node_Id;
                        Ityp : Entity_Id;

                     begin
                        if Is_Array_Type (Comp_Type) then
                           Indx := First_Index (Comp_Type);

                           while Present (Indx) loop
                              Ityp := Etype (Indx);
                              Hi := Type_High_Bound (Ityp);

                              if Nkind (Hi) = N_Identifier
                                and then Ekind (Entity (Hi)) = E_Discriminant
                                and then Is_Large_Discrete_Type (Ityp)
                                and then Is_Large_Discrete_Type
                                           (Etype (Entity (Hi)))
                              then
                                 return True;
                              end if;

                              Next_Index (Indx);
                           end loop;
                        end if;
                     end;
                  end if;

                  Next_Entity (Comp);
               end loop;
            end;
         end if;

         return False;
      end Large_Max_Size_Mutable;

      --  Local declarations

      Typ : constant Entity_Id := Underlying_Type (Id);

   --  Start of processing for New_Requires_Transient_Scope

   begin
      --  This is a private type which is not completed yet. This can only
      --  happen in a default expression (of a formal parameter or of a
      --  record component). Do not expand transient scope in this case.

      if No (Typ) then
         return False;

      --  Do not expand transient scope for non-existent procedure return or
      --  string literal types.

      elsif Typ = Standard_Void_Type
        or else Ekind (Typ) = E_String_Literal_Subtype
      then
         return False;

      --  If Typ is a generic formal incomplete type, then we want to look at
      --  the actual type.

      elsif Ekind (Typ) = E_Record_Subtype
        and then Present (Cloned_Subtype (Typ))
      then
         return New_Requires_Transient_Scope (Cloned_Subtype (Typ));

      --  Functions returning specific tagged types may dispatch on result, so
      --  their returned value is allocated on the secondary stack, even in the
      --  definite case. We must treat nondispatching functions the same way,
      --  because access-to-function types can point at both, so the calling
      --  conventions must be compatible. Is_Tagged_Type includes controlled
      --  types and class-wide types. Controlled type temporaries need
      --  finalization.

      --  ???It's not clear why we need to return noncontrolled types with
      --  controlled components on the secondary stack.

      elsif Is_Tagged_Type (Typ) or else Has_Controlled_Component (Typ) then
         return True;

      --  Untagged definite subtypes are known size. This includes all
      --  elementary [sub]types. Tasks are known size even if they have
      --  discriminants. So we return False here, with one exception:
      --  For a type like:
      --    type T (Last : Natural := 0) is
      --       X : String (1 .. Last);
      --    end record;
      --  we return True. That's because for "P(F(...));", where F returns T,
      --  we don't know the size of the result at the call site, so if we
      --  allocated it on the primary stack, we would have to allocate the
      --  maximum size, which is way too big.

      elsif Is_Definite_Subtype (Typ) or else Is_Task_Type (Typ) then
         return Large_Max_Size_Mutable (Typ);

      --  Indefinite (discriminated) untagged record or protected type

      elsif Is_Record_Type (Typ) or else Is_Protected_Type (Typ) then
         return not Caller_Known_Size_Record (Typ);

      --  Unconstrained array

      else
         pragma Assert (Is_Array_Type (Typ) and not Is_Definite_Subtype (Typ));
         return True;
      end if;
   end New_Requires_Transient_Scope;

   --------------------------
   -- No_Heap_Finalization --
   --------------------------

   function No_Heap_Finalization (Typ : Entity_Id) return Boolean is
   begin
      if Ekind_In (Typ, E_Access_Type, E_General_Access_Type)
        and then Is_Library_Level_Entity (Typ)
      then
         --  A global No_Heap_Finalization pragma applies to all library-level
         --  named access-to-object types.

         if Present (No_Heap_Finalization_Pragma) then
            return True;

         --  The library-level named access-to-object type itself is subject to
         --  pragma No_Heap_Finalization.

         elsif Present (Get_Pragma (Typ, Pragma_No_Heap_Finalization)) then
            return True;
         end if;
      end if;

      return False;
   end No_Heap_Finalization;

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
      Actual      : Node_Id := Empty;
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
      --  declared correctly, the compiler will certainly build decent calls.

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

         --  Match the formals in order. If the corresponding actual is
         --  positional, nothing to do. Else scan the list of named actuals
         --  to find the one with the right name.

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
                            Nkind_In (Parent (N), N_Procedure_Call_Statement,
                                                  N_Function_Call,
                                                  N_Parameter_Association)
                          and then Ekind (S) /= E_Function
                        then
                           Set_Etype (N, Etype (S));

                        else
                           Error_Msg_Name_1 := Chars (S);
                           Error_Msg_Sloc := Sloc (S);
                           Error_Msg_NE
                             ("missing argument for parameter & "
                              & "in call to % declared #", N, Formal);
                        end if;

                     elsif Is_Overloadable (S) then
                        Error_Msg_Name_1 := Chars (S);

                        --  Point to type derivation that generated the
                        --  operation.

                        Error_Msg_Sloc := Sloc (Parent (S));

                        Error_Msg_NE
                          ("missing argument for parameter & "
                           & "in call to % (inherited) #", N, Formal);

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
                  --  A validity check may introduce a copy of a call that
                  --  includes an extra actual (for example for an unrelated
                  --  accessibility check). Check that the extra actual matches
                  --  some extra formal, which must exist already because
                  --  subprogram must be frozen at this point.

                  if Present (Extra_Formals (S))
                    and then not Comes_From_Source (Actual)
                    and then Nkind (Actual) = N_Parameter_Association
                    and then Chars (Extra_Formals (S)) =
                               Chars (Selector_Name (Actual))
                  then
                     null;
                  else
                     Error_Msg_N
                       ("unmatched actual & in call", Selector_Name (Actual));
                     exit;
                  end if;
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

   procedure Note_Possible_Modification (N : Node_Id; Sure : Boolean) is
      Modification_Comes_From_Source : constant Boolean :=
                                         Comes_From_Source (Parent (N));

      Ent : Entity_Id;
      Exp : Node_Id;

   begin
      --  Loop to find referenced entity, if there is one

      Exp := N;
      loop
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
               --  In formal verification mode, keep track of all reads and
               --  writes through explicit dereferences.

               if GNATprove_Mode then
                  SPARK_Specific.Generate_Dereference (N, 'm');
               end if;

               if Nkind (P) = N_Selected_Component
                 and then Present (Entry_Formal (Entity (Selector_Name (P))))
               then
                  --  Case of a reference to an entry formal

                  Ent := Entry_Formal (Entity (Selector_Name (P)));

               elsif Nkind (P) = N_Identifier
                 and then Nkind (Parent (Entity (P))) = N_Object_Declaration
                 and then Present (Expression (Parent (Entity (P))))
                 and then Nkind (Expression (Parent (Entity (P)))) =
                                                               N_Reference
               then
                  --  Case of a reference to a value on which side effects have
                  --  been removed.

                  Exp := Prefix (Expression (Parent (Entity (P))));
                  goto Continue;

               else
                  return;
               end if;
            end;

         elsif Nkind_In (Exp, N_Type_Conversion,
                              N_Unchecked_Type_Conversion)
         then
            Exp := Expression (Exp);
            goto Continue;

         elsif Nkind_In (Exp, N_Slice,
                              N_Indexed_Component,
                              N_Selected_Component)
         then
            --  Special check, if the prefix is an access type, then return
            --  since we are modifying the thing pointed to, not the prefix.
            --  When we are expanding, most usually the prefix is replaced
            --  by an explicit dereference, and this test is not needed, but
            --  in some cases (notably -gnatc mode and generics) when we do
            --  not do full expansion, we need this special test.

            if Is_Access_Type (Etype (Prefix (Exp))) then
               return;

            --  Otherwise go to prefix and keep going

            else
               Exp := Prefix (Exp);
               goto Continue;
            end if;

         --  All other cases, not a modification

         else
            return;
         end if;

         --  Now look for entity being referenced

         if Present (Ent) then
            if Is_Object (Ent) then
               if Comes_From_Source (Exp)
                 or else Modification_Comes_From_Source
               then
                  --  Give warning if pragma unmodified is given and we are
                  --  sure this is a modification.

                  if Has_Pragma_Unmodified (Ent) and then Sure then

                     --  Note that the entity may be present only as a result
                     --  of pragma Unused.

                     if Has_Pragma_Unused (Ent) then
                        Error_Msg_NE ("??pragma Unused given for &!", N, Ent);
                     else
                        Error_Msg_NE
                          ("??pragma Unmodified given for &!", N, Ent);
                     end if;
                  end if;

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

                  --  If the entity is the loop variable in an iteration over
                  --  a container, retrieve container expression to indicate
                  --  possible modification.

                  if Present (Related_Expression (Ent))
                    and then Nkind (Parent (Related_Expression (Ent))) =
                                                   N_Iterator_Specification
                  then
                     Exp := Original_Node (Related_Expression (Ent));
                  end if;

                  goto Continue;

               --  The expression may be the renaming of a subcomponent of an
               --  array or container. The assignment to the subcomponent is
               --  a modification of the container.

               elsif Comes_From_Source (Original_Node (Exp))
                 and then Nkind_In (Original_Node (Exp), N_Selected_Component,
                                                         N_Indexed_Component)
               then
                  Exp := Prefix (Original_Node (Exp));
                  goto Continue;
               end if;

               --  Generate a reference only if the assignment comes from
               --  source. This excludes, for example, calls to a dispatching
               --  assignment operation when the left-hand side is tagged. In
               --  GNATprove mode, we need those references also on generated
               --  code, as these are used to compute the local effects of
               --  subprograms.

               if Modification_Comes_From_Source or GNATprove_Mode then
                  Generate_Reference (Ent, Exp, 'm');

                  --  If the target of the assignment is the bound variable
                  --  in an iterator, indicate that the corresponding array
                  --  or container is also modified.

                  if Ada_Version >= Ada_2012
                    and then Nkind (Parent (Ent)) = N_Iterator_Specification
                  then
                     declare
                        Domain : constant Node_Id := Name (Parent (Ent));

                     begin
                        --  TBD : in the full version of the construct, the
                        --  domain of iteration can be given by an expression.

                        if Is_Entity_Name (Domain) then
                           Generate_Reference      (Entity (Domain), Exp, 'm');
                           Set_Is_True_Constant    (Entity (Domain), False);
                           Set_Never_Set_In_Source (Entity (Domain), False);
                        end if;
                     end;
                  end if;
               end if;
            end if;

            Kill_Checks (Ent);

            --  If we are sure this is a modification from source, and we know
            --  this modifies a constant, then give an appropriate warning.

            if Sure
              and then Modification_Comes_From_Source
              and then Overlays_Constant (Ent)
              and then Address_Clause_Overlay_Warnings
            then
               declare
                  Addr  : constant Node_Id := Address_Clause (Ent);
                  O_Ent : Entity_Id;
                  Off   : Boolean;

               begin
                  Find_Overlaid_Entity (Addr, O_Ent, Off);

                  Error_Msg_Sloc := Sloc (Addr);
                  Error_Msg_NE
                    ("??constant& may be modified via address clause#",
                     N, O_Ent);
               end;
            end if;

            return;
         end if;

      <<Continue>>
         null;
      end loop;
   end Note_Possible_Modification;

   -----------------
   -- Null_Status --
   -----------------

   function Null_Status (N : Node_Id) return Null_Status_Kind is
      function Is_Null_Excluding_Def (Def : Node_Id) return Boolean;
      --  Determine whether definition Def carries a null exclusion

      function Null_Status_Of_Entity (Id : Entity_Id) return Null_Status_Kind;
      --  Determine the null status of arbitrary entity Id

      function Null_Status_Of_Type (Typ : Entity_Id) return Null_Status_Kind;
      --  Determine the null status of type Typ

      ---------------------------
      -- Is_Null_Excluding_Def --
      ---------------------------

      function Is_Null_Excluding_Def (Def : Node_Id) return Boolean is
      begin
         return
           Nkind_In (Def, N_Access_Definition,
                          N_Access_Function_Definition,
                          N_Access_Procedure_Definition,
                          N_Access_To_Object_Definition,
                          N_Component_Definition,
                          N_Derived_Type_Definition)
             and then Null_Exclusion_Present (Def);
      end Is_Null_Excluding_Def;

      ---------------------------
      -- Null_Status_Of_Entity --
      ---------------------------

      function Null_Status_Of_Entity
        (Id : Entity_Id) return Null_Status_Kind
      is
         Decl : constant Node_Id := Declaration_Node (Id);
         Def  : Node_Id;

      begin
         --  The value of an imported or exported entity may be set externally
         --  regardless of a null exclusion. As a result, the value cannot be
         --  determined statically.

         if Is_Imported (Id) or else Is_Exported (Id) then
            return Unknown;

         elsif Nkind_In (Decl, N_Component_Declaration,
                               N_Discriminant_Specification,
                               N_Formal_Object_Declaration,
                               N_Object_Declaration,
                               N_Object_Renaming_Declaration,
                               N_Parameter_Specification)
         then
            --  A component declaration yields a non-null value when either
            --  its component definition or access definition carries a null
            --  exclusion.

            if Nkind (Decl) = N_Component_Declaration then
               Def := Component_Definition (Decl);

               if Is_Null_Excluding_Def (Def) then
                  return Is_Non_Null;
               end if;

               Def := Access_Definition (Def);

               if Present (Def) and then Is_Null_Excluding_Def (Def) then
                  return Is_Non_Null;
               end if;

            --  A formal object declaration yields a non-null value if its
            --  access definition carries a null exclusion. If the object is
            --  default initialized, then the value depends on the expression.

            elsif Nkind (Decl) = N_Formal_Object_Declaration then
               Def := Access_Definition  (Decl);

               if Present (Def) and then Is_Null_Excluding_Def (Def) then
                  return Is_Non_Null;
               end if;

            --  A constant may yield a null or non-null value depending on its
            --  initialization expression.

            elsif Ekind (Id) = E_Constant then
               return Null_Status (Constant_Value (Id));

            --  The construct yields a non-null value when it has a null
            --  exclusion.

            elsif Null_Exclusion_Present (Decl) then
               return Is_Non_Null;

            --  An object renaming declaration yields a non-null value if its
            --  access definition carries a null exclusion. Otherwise the value
            --  depends on the renamed name.

            elsif Nkind (Decl) = N_Object_Renaming_Declaration then
               Def := Access_Definition (Decl);

               if Present (Def) and then Is_Null_Excluding_Def (Def) then
                  return Is_Non_Null;

               else
                  return Null_Status (Name (Decl));
               end if;
            end if;
         end if;

         --  At this point the declaration of the entity does not carry a null
         --  exclusion and lacks an initialization expression. Check the status
         --  of its type.

         return Null_Status_Of_Type (Etype (Id));
      end Null_Status_Of_Entity;

      -------------------------
      -- Null_Status_Of_Type --
      -------------------------

      function Null_Status_Of_Type (Typ : Entity_Id) return Null_Status_Kind is
         Curr : Entity_Id;
         Decl : Node_Id;

      begin
         --  Traverse the type chain looking for types with null exclusion

         Curr := Typ;
         while Present (Curr) and then Etype (Curr) /= Curr loop
            Decl := Parent (Curr);

            --  Guard against itypes which do not always have declarations. A
            --  type yields a non-null value if it carries a null exclusion.

            if Present (Decl) then
               if Nkind (Decl) = N_Full_Type_Declaration
                 and then Is_Null_Excluding_Def (Type_Definition (Decl))
               then
                  return Is_Non_Null;

               elsif Nkind (Decl) = N_Subtype_Declaration
                 and then Null_Exclusion_Present (Decl)
               then
                  return Is_Non_Null;
               end if;
            end if;

            Curr := Etype (Curr);
         end loop;

         --  The type chain does not contain any null excluding types

         return Unknown;
      end Null_Status_Of_Type;

   --  Start of processing for Null_Status

   begin
      --  An allocator always creates a non-null value

      if Nkind (N) = N_Allocator then
         return Is_Non_Null;

      --  Taking the 'Access of something yields a non-null value

      elsif Nkind (N) = N_Attribute_Reference
        and then Nam_In (Attribute_Name (N), Name_Access,
                                             Name_Unchecked_Access,
                                             Name_Unrestricted_Access)
      then
         return Is_Non_Null;

      --  "null" yields null

      elsif Nkind (N) = N_Null then
         return Is_Null;

      --  Check the status of the operand of a type conversion

      elsif Nkind (N) = N_Type_Conversion then
         return Null_Status (Expression (N));

      --  The input denotes a reference to an entity. Determine whether the
      --  entity or its type yields a null or non-null value.

      elsif Is_Entity_Name (N) and then Present (Entity (N)) then
         return Null_Status_Of_Entity (Entity (N));
      end if;

      --  Otherwise it is not possible to determine the null status of the
      --  subexpression at compile time without resorting to simple flow
      --  analysis.

      return Unknown;
   end Null_Status;

   --------------------------------------
   --  Null_To_Null_Address_Convert_OK --
   --------------------------------------

   function Null_To_Null_Address_Convert_OK
     (N   : Node_Id;
      Typ : Entity_Id := Empty) return Boolean
   is
   begin
      if not Relaxed_RM_Semantics then
         return False;
      end if;

      if Nkind (N) = N_Null then
         return Present (Typ) and then Is_Descendant_Of_Address (Typ);

      elsif Nkind_In (N, N_Op_Eq, N_Op_Ge, N_Op_Gt, N_Op_Le, N_Op_Lt, N_Op_Ne)
      then
         declare
            L : constant Node_Id := Left_Opnd (N);
            R : constant Node_Id := Right_Opnd (N);

         begin
            --  We check the Etype of the complementary operand since the
            --  N_Null node is not decorated at this stage.

            return
              ((Nkind (L) = N_Null
                 and then Is_Descendant_Of_Address (Etype (R)))
              or else
               (Nkind (R) = N_Null
                 and then Is_Descendant_Of_Address (Etype (L))));
         end;
      end if;

      return False;
   end Null_To_Null_Address_Convert_OK;

   -------------------------
   -- Object_Access_Level --
   -------------------------

   --  Returns the static accessibility level of the view denoted by Obj. Note
   --  that the value returned is the result of a call to Scope_Depth. Only
   --  scope depths associated with dynamic scopes can actually be returned.
   --  Since only relative levels matter for accessibility checking, the fact
   --  that the distance between successive levels of accessibility is not
   --  always one is immaterial (invariant: if level(E2) is deeper than
   --  level(E1), then Scope_Depth(E1) < Scope_Depth(E2)).

   function Object_Access_Level (Obj : Node_Id) return Uint is
      function Is_Interface_Conversion (N : Node_Id) return Boolean;
      --  Determine whether N is a construct of the form
      --    Some_Type (Operand._tag'Address)
      --  This construct appears in the context of dispatching calls.

      function Reference_To (Obj : Node_Id) return Node_Id;
      --  An explicit dereference is created when removing side-effects from
      --  expressions for constraint checking purposes. In this case a local
      --  access type is created for it. The correct access level is that of
      --  the original source node. We detect this case by noting that the
      --  prefix of the dereference is created by an object declaration whose
      --  initial expression is a reference.

      -----------------------------
      -- Is_Interface_Conversion --
      -----------------------------

      function Is_Interface_Conversion (N : Node_Id) return Boolean is
      begin
         return Nkind (N) = N_Unchecked_Type_Conversion
           and then Nkind (Expression (N)) = N_Attribute_Reference
           and then Attribute_Name (Expression (N)) = Name_Address;
      end Is_Interface_Conversion;

      ------------------
      -- Reference_To --
      ------------------

      function Reference_To (Obj : Node_Id) return Node_Id is
         Pref : constant Node_Id := Prefix (Obj);
      begin
         if Is_Entity_Name (Pref)
           and then Nkind (Parent (Entity (Pref))) = N_Object_Declaration
           and then Present (Expression (Parent (Entity (Pref))))
           and then Nkind (Expression (Parent (Entity (Pref)))) = N_Reference
         then
            return (Prefix (Expression (Parent (Entity (Pref)))));
         else
            return Empty;
         end if;
      end Reference_To;

      --  Local variables

      E : Entity_Id;

   --  Start of processing for Object_Access_Level

   begin
      if Nkind (Obj) = N_Defining_Identifier
        or else Is_Entity_Name (Obj)
      then
         if Nkind (Obj) = N_Defining_Identifier then
            E := Obj;
         else
            E := Entity (Obj);
         end if;

         if Is_Prival (E) then
            E := Prival_Link (E);
         end if;

         --  If E is a type then it denotes a current instance. For this case
         --  we add one to the normal accessibility level of the type to ensure
         --  that current instances are treated as always being deeper than
         --  than the level of any visible named access type (see 3.10.2(21)).

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
            --  Aliased formals of functions take their access level from the
            --  point of call, i.e. require a dynamic check. For static check
            --  purposes, this is smaller than the level of the subprogram
            --  itself. For procedures the aliased makes no difference.

            if Is_Formal (E)
               and then Is_Aliased (E)
               and then Ekind (Scope (E)) = E_Function
            then
               return Type_Access_Level (Etype (E));

            else
               return Scope_Depth (Enclosing_Dynamic_Scope (E));
            end if;
         end if;

      elsif Nkind_In (Obj, N_Indexed_Component, N_Selected_Component) then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Explicit_Dereference then

         --  If the prefix is a selected access discriminant then we make a
         --  recursive call on the prefix, which will in turn check the level
         --  of the prefix object of the selected discriminant.

         --  In Ada 2012, if the discriminant has implicit dereference and
         --  the context is a selected component, treat this as an object of
         --  unknown scope (see below). This is necessary in compile-only mode;
         --  otherwise expansion will already have transformed the prefix into
         --  a temporary.

         if Nkind (Prefix (Obj)) = N_Selected_Component
           and then Ekind (Etype (Prefix (Obj))) = E_Anonymous_Access_Type
           and then
             Ekind (Entity (Selector_Name (Prefix (Obj)))) = E_Discriminant
           and then
             (not Has_Implicit_Dereference
                    (Entity (Selector_Name (Prefix (Obj))))
               or else Nkind (Parent (Obj)) /= N_Selected_Component)
         then
            return Object_Access_Level (Prefix (Obj));

         --  Detect an interface conversion in the context of a dispatching
         --  call. Use the original form of the conversion to find the access
         --  level of the operand.

         elsif Is_Interface (Etype (Obj))
           and then Is_Interface_Conversion (Prefix (Obj))
           and then Nkind (Original_Node (Obj)) = N_Type_Conversion
         then
            return Object_Access_Level (Original_Node (Obj));

         elsif not Comes_From_Source (Obj) then
            declare
               Ref : constant Node_Id := Reference_To (Obj);
            begin
               if Present (Ref) then
                  return Object_Access_Level (Ref);
               else
                  return Type_Access_Level (Etype (Prefix (Obj)));
               end if;
            end;

         else
            return Type_Access_Level (Etype (Prefix (Obj)));
         end if;

      elsif Nkind_In (Obj, N_Type_Conversion, N_Unchecked_Type_Conversion) then
         return Object_Access_Level (Expression (Obj));

      elsif Nkind (Obj) = N_Function_Call then

         --  Function results are objects, so we get either the access level of
         --  the function or, in the case of an indirect call, the level of the
         --  access-to-subprogram type. (This code is used for Ada 95, but it
         --  looks wrong, because it seems that we should be checking the level
         --  of the call itself, even for Ada 95. However, using the Ada 2005
         --  version of the code causes regressions in several tests that are
         --  compiled with -gnat95. ???)

         if Ada_Version < Ada_2005 then
            if Is_Entity_Name (Name (Obj)) then
               return Subprogram_Access_Level (Entity (Name (Obj)));
            else
               return Type_Access_Level (Etype (Prefix (Name (Obj))));
            end if;

         --  For Ada 2005, the level of the result object of a function call is
         --  defined to be the level of the call's innermost enclosing master.
         --  We determine that by querying the depth of the innermost enclosing
         --  dynamic scope.

         else
            Return_Master_Scope_Depth_Of_Call : declare
               function Innermost_Master_Scope_Depth
                 (N : Node_Id) return Uint;
               --  Returns the scope depth of the given node's innermost
               --  enclosing dynamic scope (effectively the accessibility
               --  level of the innermost enclosing master).

               ----------------------------------
               -- Innermost_Master_Scope_Depth --
               ----------------------------------

               function Innermost_Master_Scope_Depth
                 (N : Node_Id) return Uint
               is
                  Node_Par : Node_Id := Parent (N);

               begin
                  --  Locate the nearest enclosing node (by traversing Parents)
                  --  that Defining_Entity can be applied to, and return the
                  --  depth of that entity's nearest enclosing dynamic scope.

                  while Present (Node_Par) loop
                     case Nkind (Node_Par) is
                        when N_Abstract_Subprogram_Declaration
                           | N_Block_Statement
                           | N_Body_Stub
                           | N_Component_Declaration
                           | N_Entry_Body
                           | N_Entry_Declaration
                           | N_Exception_Declaration
                           | N_Formal_Object_Declaration
                           | N_Formal_Package_Declaration
                           | N_Formal_Subprogram_Declaration
                           | N_Formal_Type_Declaration
                           | N_Full_Type_Declaration
                           | N_Function_Specification
                           | N_Generic_Declaration
                           | N_Generic_Instantiation
                           | N_Implicit_Label_Declaration
                           | N_Incomplete_Type_Declaration
                           | N_Loop_Parameter_Specification
                           | N_Number_Declaration
                           | N_Object_Declaration
                           | N_Package_Declaration
                           | N_Package_Specification
                           | N_Parameter_Specification
                           | N_Private_Extension_Declaration
                           | N_Private_Type_Declaration
                           | N_Procedure_Specification
                           | N_Proper_Body
                           | N_Protected_Type_Declaration
                           | N_Renaming_Declaration
                           | N_Single_Protected_Declaration
                           | N_Single_Task_Declaration
                           | N_Subprogram_Declaration
                           | N_Subtype_Declaration
                           | N_Subunit
                           | N_Task_Type_Declaration
                        =>
                           return Scope_Depth
                                    (Nearest_Dynamic_Scope
                                       (Defining_Entity (Node_Par)));

                        when others =>
                           null;
                     end case;

                     Node_Par := Parent (Node_Par);
                  end loop;

                  pragma Assert (False);

                  --  Should never reach the following return

                  return Scope_Depth (Current_Scope) + 1;
               end Innermost_Master_Scope_Depth;

            --  Start of processing for Return_Master_Scope_Depth_Of_Call

            begin
               return Innermost_Master_Scope_Depth (Obj);
            end Return_Master_Scope_Depth_Of_Call;
         end if;

      --  For convenience we handle qualified expressions, even though they
      --  aren't technically object names.

      elsif Nkind (Obj) = N_Qualified_Expression then
         return Object_Access_Level (Expression (Obj));

      --  Ditto for aggregates. They have the level of the temporary that
      --  will hold their value.

      elsif Nkind (Obj) = N_Aggregate then
         return Object_Access_Level (Current_Scope);

      --  Otherwise return the scope level of Standard. (If there are cases
      --  that fall through to this point they will be treated as having
      --  global accessibility for now. ???)

      else
         return Scope_Depth (Standard_Standard);
      end if;
   end Object_Access_Level;

   ----------------------------------
   -- Old_Requires_Transient_Scope --
   ----------------------------------

   function Old_Requires_Transient_Scope (Id : Entity_Id) return Boolean is
      Typ : constant Entity_Id := Underlying_Type (Id);

   begin
      --  This is a private type which is not completed yet. This can only
      --  happen in a default expression (of a formal parameter or of a
      --  record component). Do not expand transient scope in this case.

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

      elsif not Is_Definite_Subtype (Typ) then
         return True;

      --  Functions returning tagged types may dispatch on result so their
      --  returned value is allocated on the secondary stack. Controlled
      --  type temporaries need finalization.

      elsif Is_Tagged_Type (Typ) or else Has_Controlled_Component (Typ) then
         return True;

      --  Record type

      elsif Is_Record_Type (Typ) then
         declare
            Comp : Entity_Id;

         begin
            Comp := First_Entity (Typ);
            while Present (Comp) loop
               if Ekind (Comp) = E_Component then

                  --  ???It's not clear we need a full recursive call to
                  --  Old_Requires_Transient_Scope here. Note that the
                  --  following can't happen.

                  pragma Assert (Is_Definite_Subtype (Etype (Comp)));
                  pragma Assert (not Has_Controlled_Component (Etype (Comp)));

                  if Old_Requires_Transient_Scope (Etype (Comp)) then
                     return True;
                  end if;
               end if;

               Next_Entity (Comp);
            end loop;
         end;

         return False;

      --  String literal types never require transient scope

      elsif Ekind (Typ) = E_String_Literal_Subtype then
         return False;

      --  Array type. Note that we already know that this is a constrained
      --  array, since unconstrained arrays will fail the indefinite test.

      elsif Is_Array_Type (Typ) then

         --  If component type requires a transient scope, the array does too

         if Old_Requires_Transient_Scope (Component_Type (Typ)) then
            return True;

         --  Otherwise, we only need a transient scope if the size depends on
         --  the value of one or more discriminants.

         else
            return Size_Depends_On_Discriminant (Typ);
         end if;

      --  All other cases do not require a transient scope

      else
         pragma Assert (Is_Protected_Type (Typ) or else Is_Task_Type (Typ));
         return False;
      end if;
   end Old_Requires_Transient_Scope;

   ---------------------------------
   -- Original_Aspect_Pragma_Name --
   ---------------------------------

   function Original_Aspect_Pragma_Name (N : Node_Id) return Name_Id is
      Item     : Node_Id;
      Item_Nam : Name_Id;

   begin
      pragma Assert (Nkind_In (N, N_Aspect_Specification, N_Pragma));

      Item := N;

      --  The pragma was generated to emulate an aspect, use the original
      --  aspect specification.

      if Nkind (Item) = N_Pragma and then From_Aspect_Specification (Item) then
         Item := Corresponding_Aspect (Item);
      end if;

      --  Retrieve the name of the aspect/pragma. Note that Pre, Pre_Class,
      --  Post and Post_Class rewrite their pragma identifier to preserve the
      --  original name.
      --  ??? this is kludgey

      if Nkind (Item) = N_Pragma then
         Item_Nam := Chars (Original_Node (Pragma_Identifier (Item)));

      else
         pragma Assert (Nkind (Item) = N_Aspect_Specification);
         Item_Nam := Chars (Identifier (Item));
      end if;

      --  Deal with 'Class by converting the name to its _XXX form

      if Class_Present (Item) then
         if Item_Nam = Name_Invariant then
            Item_Nam := Name_uInvariant;

         elsif Item_Nam = Name_Post then
            Item_Nam := Name_uPost;

         elsif Item_Nam = Name_Pre then
            Item_Nam := Name_uPre;

         elsif Nam_In (Item_Nam, Name_Type_Invariant,
                                 Name_Type_Invariant_Class)
         then
            Item_Nam := Name_uType_Invariant;

         --  Nothing to do for other cases (e.g. a Check that derived from
         --  Pre_Class and has the flag set). Also we do nothing if the name
         --  is already in special _xxx form.

         end if;
      end if;

      return Item_Nam;
   end Original_Aspect_Pragma_Name;

   --------------------------------------
   -- Original_Corresponding_Operation --
   --------------------------------------

   function Original_Corresponding_Operation (S : Entity_Id) return Entity_Id
   is
      Typ : constant Entity_Id := Find_Dispatching_Type (S);

   begin
      --  If S is an inherited primitive S2 the original corresponding
      --  operation of S is the original corresponding operation of S2

      if Present (Alias (S))
        and then Find_Dispatching_Type (Alias (S)) /= Typ
      then
         return Original_Corresponding_Operation (Alias (S));

      --  If S overrides an inherited subprogram S2 the original corresponding
      --  operation of S is the original corresponding operation of S2

      elsif Present (Overridden_Operation (S)) then
         return Original_Corresponding_Operation (Overridden_Operation (S));

      --  otherwise it is S itself

      else
         return S;
      end if;
   end Original_Corresponding_Operation;

   -------------------
   -- Output_Entity --
   -------------------

   procedure Output_Entity (Id : Entity_Id) is
      Scop : Entity_Id;

   begin
      Scop := Scope (Id);

      --  The entity may lack a scope when it is in the process of being
      --  analyzed. Use the current scope as an approximation.

      if No (Scop) then
         Scop := Current_Scope;
      end if;

      Output_Name (Chars (Id), Scop);
   end Output_Entity;

   -----------------
   -- Output_Name --
   -----------------

   procedure Output_Name (Nam : Name_Id; Scop : Entity_Id := Current_Scope) is
   begin
      Write_Str
        (Get_Name_String
          (Get_Qualified_Name
            (Nam    => Nam,
             Suffix => No_Name,
             Scop   => Scop)));
      Write_Eol;
   end Output_Name;

   ----------------------
   -- Policy_In_Effect --
   ----------------------

   function Policy_In_Effect (Policy : Name_Id) return Name_Id is
      function Policy_In_List (List : Node_Id) return Name_Id;
      --  Determine the mode of a policy in a N_Pragma list

      --------------------
      -- Policy_In_List --
      --------------------

      function Policy_In_List (List : Node_Id) return Name_Id is
         Arg1 : Node_Id;
         Arg2 : Node_Id;
         Prag : Node_Id;

      begin
         Prag := List;
         while Present (Prag) loop
            Arg1 := First (Pragma_Argument_Associations (Prag));
            Arg2 := Next (Arg1);

            Arg1 := Get_Pragma_Arg (Arg1);
            Arg2 := Get_Pragma_Arg (Arg2);

            --  The current Check_Policy pragma matches the requested policy or
            --  appears in the single argument form (Assertion, policy_id).

            if Nam_In (Chars (Arg1), Name_Assertion, Policy) then
               return Chars (Arg2);
            end if;

            Prag := Next_Pragma (Prag);
         end loop;

         return No_Name;
      end Policy_In_List;

      --  Local variables

      Kind : Name_Id;

   --  Start of processing for Policy_In_Effect

   begin
      if not Is_Valid_Assertion_Kind (Policy) then
         raise Program_Error;
      end if;

      --  Inspect all policy pragmas that appear within scopes (if any)

      Kind := Policy_In_List (Check_Policy_List);

      --  Inspect all configuration policy pragmas (if any)

      if Kind = No_Name then
         Kind := Policy_In_List (Check_Policy_List_Config);
      end if;

      --  The context lacks policy pragmas, determine the mode based on whether
      --  assertions are enabled at the configuration level. This ensures that
      --  the policy is preserved when analyzing generics.

      if Kind = No_Name then
         if Assertions_Enabled_Config then
            Kind := Name_Check;
         else
            Kind := Name_Ignore;
         end if;
      end if;

      return Kind;
   end Policy_In_Effect;

   ----------------------------------
   -- Predicate_Tests_On_Arguments --
   ----------------------------------

   function Predicate_Tests_On_Arguments (Subp : Entity_Id) return Boolean is
   begin
      --  Always test predicates on indirect call

      if Ekind (Subp) = E_Subprogram_Type then
         return True;

      --  Do not test predicates on call to generated default Finalize, since
      --  we are not interested in whether something we are finalizing (and
      --  typically destroying) satisfies its predicates.

      elsif Chars (Subp) = Name_Finalize
        and then not Comes_From_Source (Subp)
      then
         return False;

      --  Do not test predicates on any internally generated routines

      elsif Is_Internal_Name (Chars (Subp)) then
         return False;

      --  Do not test predicates on call to Init_Proc, since if needed the
      --  predicate test will occur at some other point.

      elsif Is_Init_Proc (Subp) then
         return False;

      --  Do not test predicates on call to predicate function, since this
      --  would cause infinite recursion.

      elsif Ekind (Subp) = E_Function
        and then (Is_Predicate_Function   (Subp)
                    or else
                  Is_Predicate_Function_M (Subp))
      then
         return False;

      --  For now, no other exceptions

      else
         return True;
      end if;
   end Predicate_Tests_On_Arguments;

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

         if Is_Private_Type (Btype) and then not Is_Generic_Type (Btype) then
            if Present (Full_View (Btype))
              and then Is_Record_Type (Full_View (Btype))
              and then not Is_Frozen (Btype)
            then
               --  To indicate that the ancestor depends on a private type, the
               --  current Btype is sufficient. However, to check for circular
               --  definition we must recurse on the full view.

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
            while Present (Component)
              and then Comes_From_Source (Component)
            loop
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

   ---------------------------
   -- Primitive_Names_Match --
   ---------------------------

   function Primitive_Names_Match (E1, E2 : Entity_Id) return Boolean is
      function Non_Internal_Name (E : Entity_Id) return Name_Id;
      --  Given an internal name, returns the corresponding non-internal name

      ------------------------
      --  Non_Internal_Name --
      ------------------------

      function Non_Internal_Name (E : Entity_Id) return Name_Id is
      begin
         Get_Name_String (Chars (E));
         Name_Len := Name_Len - 1;
         return Name_Find;
      end Non_Internal_Name;

   --  Start of processing for Primitive_Names_Match

   begin
      pragma Assert (Present (E1) and then Present (E2));

      return Chars (E1) = Chars (E2)
        or else
           (not Is_Internal_Name (Chars (E1))
             and then Is_Internal_Name (Chars (E2))
             and then Non_Internal_Name (E2) = Chars (E1))
        or else
           (not Is_Internal_Name (Chars (E2))
             and then Is_Internal_Name (Chars (E1))
             and then Non_Internal_Name (E1) = Chars (E2))
        or else
           (Is_Predefined_Dispatching_Operation (E1)
             and then Is_Predefined_Dispatching_Operation (E2)
             and then Same_TSS (E1, E2))
        or else
           (Is_Init_Proc (E1) and then Is_Init_Proc (E2));
   end Primitive_Names_Match;

   -----------------------
   -- Process_End_Label --
   -----------------------

   procedure Process_End_Label
     (N   : Node_Id;
      Typ : Character;
      Ent : Entity_Id)
   is
      Loc  : Source_Ptr;
      Nam  : Node_Id;
      Scop : Entity_Id;

      Label_Ref : Boolean;
      --  Set True if reference to end label itself is required

      Endl : Node_Id;
      --  Gets set to the operator symbol or identifier that references the
      --  entity Ent. For the child unit case, this is the identifier from the
      --  designator. For other cases, this is simply Endl.

      procedure Generate_Parent_Ref (N : Node_Id; E : Entity_Id);
      --  N is an identifier node that appears as a parent unit reference in
      --  the case where Ent is a child unit. This procedure generates an
      --  appropriate cross-reference entry. E is the corresponding entity.

      -------------------------
      -- Generate_Parent_Ref --
      -------------------------

      procedure Generate_Parent_Ref (N : Node_Id; E : Entity_Id) is
      begin
         --  If names do not match, something weird, skip reference

         if Chars (E) = Chars (N) then

            --  Generate the reference. We do NOT consider this as a reference
            --  for unreferenced symbol purposes.

            Generate_Reference (E, N, 'r', Set_Ref => False, Force => True);

            if Style_Check then
               Style.Check_Identifier (N, E);
            end if;
         end if;
      end Generate_Parent_Ref;

   --  Start of processing for Process_End_Label

   begin
      --  If no node, ignore. This happens in some error situations, and
      --  also for some internally generated structures where no end label
      --  references are required in any case.

      if No (N) then
         return;
      end if;

      --  Nothing to do if no End_Label, happens for internally generated
      --  constructs where we don't want an end label reference anyway. Also
      --  nothing to do if Endl is a string literal, which means there was
      --  some prior error (bad operator symbol)

      Endl := End_Label (N);

      if No (Endl) or else Nkind (Endl) = N_String_Literal then
         return;
      end if;

      --  Reference node is not in extended main source unit

      if not In_Extended_Main_Source_Unit (N) then

         --  Generally we do not collect references except for the extended
         --  main source unit. The one exception is the 'e' entry for a
         --  package spec, where it is useful for a client to have the
         --  ending information to define scopes.

         if Typ /= 'e' then
            return;

         else
            Label_Ref := False;

            --  For this case, we can ignore any parent references, but we
            --  need the package name itself for the 'e' entry.

            if Nkind (Endl) = N_Designator then
               Endl := Identifier (Endl);
            end if;
         end if;

      --  Reference is in extended main source unit

      else
         Label_Ref := True;

         --  For designator, generate references for the parent entries

         if Nkind (Endl) = N_Designator then

            --  Generate references for the prefix if the END line comes from
            --  source (otherwise we do not need these references) We climb the
            --  scope stack to find the expected entities.

            if Comes_From_Source (Endl) then
               Nam  := Name (Endl);
               Scop := Current_Scope;
               while Nkind (Nam) = N_Selected_Component loop
                  Scop := Scope (Scop);
                  exit when No (Scop);
                  Generate_Parent_Ref (Selector_Name (Nam), Scop);
                  Nam := Prefix (Nam);
               end loop;

               if Present (Scop) then
                  Generate_Parent_Ref (Nam, Scope (Scop));
               end if;
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

      --  If label was really there, then generate a normal reference and then
      --  adjust the location in the end label to point past the name (which
      --  should almost always be the semicolon).

      Loc := Sloc (Endl);

      if Comes_From_Source (Endl) then

         --  If a label reference is required, then do the style check and
         --  generate an l-type cross-reference entry for the label

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

      else
         --  In SPARK mode, no missing label is allowed for packages and
         --  subprogram bodies. Detect those cases by testing whether
         --  Process_End_Label was called for a body (Typ = 't') or a package.

         if Restriction_Check_Required (SPARK_05)
           and then (Typ = 't' or else Ekind (Ent) = E_Package)
         then
            Error_Msg_Node_1 := Endl;
            Check_SPARK_05_Restriction
              ("`END &` required", Endl, Force => True);
         end if;
      end if;

      --  Now generate the e/t reference

      Generate_Reference (Ent, Endl, Typ, Set_Ref => False, Force => True);

      --  Restore Sloc, in case modified above, since we have an identifier
      --  and the normal Sloc should be left set in the tree.

      Set_Sloc (Endl, Loc);
   end Process_End_Label;

   --------------------------------
   -- Propagate_Concurrent_Flags --
   --------------------------------

   procedure Propagate_Concurrent_Flags
     (Typ      : Entity_Id;
      Comp_Typ : Entity_Id)
   is
   begin
      if Has_Task (Comp_Typ) then
         Set_Has_Task (Typ);
      end if;

      if Has_Protected (Comp_Typ) then
         Set_Has_Protected (Typ);
      end if;

      if Has_Timing_Event (Comp_Typ) then
         Set_Has_Timing_Event (Typ);
      end if;
   end Propagate_Concurrent_Flags;

   ------------------------------
   -- Propagate_DIC_Attributes --
   ------------------------------

   procedure Propagate_DIC_Attributes
     (Typ      : Entity_Id;
      From_Typ : Entity_Id)
   is
      DIC_Proc : Entity_Id;

   begin
      if Present (Typ) and then Present (From_Typ) then
         pragma Assert (Is_Type (Typ) and then Is_Type (From_Typ));

         --  Nothing to do if both the source and the destination denote the
         --  same type.

         if From_Typ = Typ then
            return;
         end if;

         DIC_Proc := DIC_Procedure (From_Typ);

         --  The setting of the attributes is intentionally conservative. This
         --  prevents accidental clobbering of enabled attributes.

         if Has_Inherited_DIC (From_Typ)
           and then not Has_Inherited_DIC (Typ)
         then
            Set_Has_Inherited_DIC (Typ);
         end if;

         if Has_Own_DIC (From_Typ) and then not Has_Own_DIC (Typ) then
            Set_Has_Own_DIC (Typ);
         end if;

         if Present (DIC_Proc) and then No (DIC_Procedure (Typ)) then
            Set_DIC_Procedure (Typ, DIC_Proc);
         end if;
      end if;
   end Propagate_DIC_Attributes;

   ------------------------------------
   -- Propagate_Invariant_Attributes --
   ------------------------------------

   procedure Propagate_Invariant_Attributes
     (Typ      : Entity_Id;
      From_Typ : Entity_Id)
   is
      Full_IP : Entity_Id;
      Part_IP : Entity_Id;

   begin
      if Present (Typ) and then Present (From_Typ) then
         pragma Assert (Is_Type (Typ) and then Is_Type (From_Typ));

         --  Nothing to do if both the source and the destination denote the
         --  same type.

         if From_Typ = Typ then
            return;
         end if;

         Full_IP := Invariant_Procedure (From_Typ);
         Part_IP := Partial_Invariant_Procedure (From_Typ);

         --  The setting of the attributes is intentionally conservative. This
         --  prevents accidental clobbering of enabled attributes.

         if Has_Inheritable_Invariants (From_Typ)
           and then not Has_Inheritable_Invariants (Typ)
         then
            Set_Has_Inheritable_Invariants (Typ, True);
         end if;

         if Has_Inherited_Invariants (From_Typ)
           and then not Has_Inherited_Invariants (Typ)
         then
            Set_Has_Inherited_Invariants (Typ, True);
         end if;

         if Has_Own_Invariants (From_Typ)
           and then not Has_Own_Invariants (Typ)
         then
            Set_Has_Own_Invariants (Typ, True);
         end if;

         if Present (Full_IP) and then No (Invariant_Procedure (Typ)) then
            Set_Invariant_Procedure (Typ, Full_IP);
         end if;

         if Present (Part_IP) and then No (Partial_Invariant_Procedure (Typ))
         then
            Set_Partial_Invariant_Procedure (Typ, Part_IP);
         end if;
      end if;
   end Propagate_Invariant_Attributes;

   ---------------------------------------
   -- Record_Possible_Part_Of_Reference --
   ---------------------------------------

   procedure Record_Possible_Part_Of_Reference
     (Var_Id : Entity_Id;
      Ref    : Node_Id)
   is
      Encap : constant Entity_Id := Encapsulating_State (Var_Id);
      Refs  : Elist_Id;

   begin
      --  The variable is a constituent of a single protected/task type. Such
      --  a variable acts as a component of the type and must appear within a
      --  specific region (SPARK RM 9.3). Instead of recording the reference,
      --  verify its legality now.

      if Present (Encap) and then Is_Single_Concurrent_Object (Encap) then
         Check_Part_Of_Reference (Var_Id, Ref);

      --  The variable is subject to pragma Part_Of and may eventually become a
      --  constituent of a single protected/task type. Record the reference to
      --  verify its placement when the contract of the variable is analyzed.

      elsif Present (Get_Pragma (Var_Id, Pragma_Part_Of)) then
         Refs := Part_Of_References (Var_Id);

         if No (Refs) then
            Refs := New_Elmt_List;
            Set_Part_Of_References (Var_Id, Refs);
         end if;

         Append_Elmt (Ref, Refs);
      end if;
   end Record_Possible_Part_Of_Reference;

   ----------------
   -- Referenced --
   ----------------

   function Referenced (Id : Entity_Id; Expr : Node_Id) return Boolean is
      Seen : Boolean := False;

      function Is_Reference (N : Node_Id) return Traverse_Result;
      --  Determine whether node N denotes a reference to Id. If this is the
      --  case, set global flag Seen to True and stop the traversal.

      ------------------
      -- Is_Reference --
      ------------------

      function Is_Reference (N : Node_Id) return Traverse_Result is
      begin
         if Is_Entity_Name (N)
           and then Present (Entity (N))
           and then Entity (N) = Id
         then
            Seen := True;
            return Abandon;
         else
            return OK;
         end if;
      end Is_Reference;

      procedure Inspect_Expression is new Traverse_Proc (Is_Reference);

   --  Start of processing for Referenced

   begin
      Inspect_Expression (Expr);
      return Seen;
   end Referenced;

   ------------------------------------
   -- References_Generic_Formal_Type --
   ------------------------------------

   function References_Generic_Formal_Type (N : Node_Id) return Boolean is

      function Process (N : Node_Id) return Traverse_Result;
      --  Process one node in search for generic formal type

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) in N_Has_Entity then
            declare
               E : constant Entity_Id := Entity (N);
            begin
               if Present (E) then
                  if Is_Generic_Type (E) then
                     return Abandon;
                  elsif Present (Etype (E))
                    and then Is_Generic_Type (Etype (E))
                  then
                     return Abandon;
                  end if;
               end if;
            end;
         end if;

         return Atree.OK;
      end Process;

      function Traverse is new Traverse_Func (Process);
      --  Traverse tree to look for generic type

   begin
      if Inside_A_Generic then
         return Traverse (N) = Abandon;
      else
         return False;
      end if;
   end References_Generic_Formal_Type;

   --------------------
   -- Remove_Homonym --
   --------------------

   procedure Remove_Homonym (E : Entity_Id) is
      Prev  : Entity_Id := Empty;
      H     : Entity_Id;

   begin
      if E = Current_Entity (E) then
         if Present (Homonym (E)) then
            Set_Current_Entity (Homonym (E));
         else
            Set_Name_Entity_Id (Chars (E), Empty);
         end if;

      else
         H := Current_Entity (E);
         while Present (H) and then H /= E loop
            Prev := H;
            H    := Homonym (H);
         end loop;

         --  If E is not on the homonym chain, nothing to do

         if Present (H) then
            Set_Homonym (Prev, Homonym (E));
         end if;
      end if;
   end Remove_Homonym;

   ------------------------------
   -- Remove_Overloaded_Entity --
   ------------------------------

   procedure Remove_Overloaded_Entity (Id : Entity_Id) is
      procedure Remove_Primitive_Of (Typ : Entity_Id);
      --  Remove primitive subprogram Id from the list of primitives that
      --  belong to type Typ.

      -------------------------
      -- Remove_Primitive_Of --
      -------------------------

      procedure Remove_Primitive_Of (Typ : Entity_Id) is
         Prims : Elist_Id;

      begin
         if Is_Tagged_Type (Typ) then
            Prims := Direct_Primitive_Operations (Typ);

            if Present (Prims) then
               Remove (Prims, Id);
            end if;
         end if;
      end Remove_Primitive_Of;

      --  Local variables

      Scop    : constant Entity_Id := Scope (Id);
      Formal  : Entity_Id;
      Prev_Id : Entity_Id;

   --  Start of processing for Remove_Overloaded_Entity

   begin
      --  Remove the entity from the homonym chain. When the entity is the
      --  head of the chain, associate the entry in the name table with its
      --  homonym effectively making it the new head of the chain.

      if Current_Entity (Id) = Id then
         Set_Name_Entity_Id (Chars (Id), Homonym (Id));

      --  Otherwise link the previous and next homonyms

      else
         Prev_Id := Current_Entity (Id);
         while Present (Prev_Id) and then Homonym (Prev_Id) /= Id loop
            Prev_Id := Homonym (Prev_Id);
         end loop;

         Set_Homonym (Prev_Id, Homonym (Id));
      end if;

      --  Remove the entity from the scope entity chain. When the entity is
      --  the head of the chain, set the next entity as the new head of the
      --  chain.

      if First_Entity (Scop) = Id then
         Prev_Id := Empty;
         Set_First_Entity (Scop, Next_Entity (Id));

      --  Otherwise the entity is either in the middle of the chain or it acts
      --  as its tail. Traverse and link the previous and next entities.

      else
         Prev_Id := First_Entity (Scop);
         while Present (Prev_Id) and then Next_Entity (Prev_Id) /= Id loop
            Next_Entity (Prev_Id);
         end loop;

         Set_Next_Entity (Prev_Id, Next_Entity (Id));
      end if;

      --  Handle the case where the entity acts as the tail of the scope entity
      --  chain.

      if Last_Entity (Scop) = Id then
         Set_Last_Entity (Scop, Prev_Id);
      end if;

      --  The entity denotes a primitive subprogram. Remove it from the list of
      --  primitives of the associated controlling type.

      if Ekind_In (Id, E_Function, E_Procedure) and then Is_Primitive (Id) then
         Formal := First_Formal (Id);
         while Present (Formal) loop
            if Is_Controlling_Formal (Formal) then
               Remove_Primitive_Of (Etype (Formal));
               exit;
            end if;

            Next_Formal (Formal);
         end loop;

         if Ekind (Id) = E_Function and then Has_Controlling_Result (Id) then
            Remove_Primitive_Of (Etype (Id));
         end if;
      end if;
   end Remove_Overloaded_Entity;

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
   --  allocated on the secondary stack, or when finalization actions must be
   --  generated before the next instruction.

   function Requires_Transient_Scope (Id : Entity_Id) return Boolean is
      Old_Result : constant Boolean := Old_Requires_Transient_Scope (Id);

   begin
      if Debug_Flag_QQ then
         return Old_Result;
      end if;

      declare
         New_Result : constant Boolean := New_Requires_Transient_Scope (Id);

      begin
         --  Assert that we're not putting things on the secondary stack if we
         --  didn't before; we are trying to AVOID secondary stack when
         --  possible.

         if not Old_Result then
            pragma Assert (not New_Result);
            null;
         end if;

         if New_Result /= Old_Result then
            Results_Differ (Id, Old_Result, New_Result);
         end if;

         return New_Result;
      end;
   end Requires_Transient_Scope;

   --------------------
   -- Results_Differ --
   --------------------

   procedure Results_Differ
     (Id      : Entity_Id;
      Old_Val : Boolean;
      New_Val : Boolean)
   is
   begin
      if False then -- False to disable; True for debugging
         Treepr.Print_Tree_Node (Id);

         if Old_Val = New_Val then
            raise Program_Error;
         end if;
      end if;
   end Results_Differ;

   --------------------------
   -- Reset_Analyzed_Flags --
   --------------------------

   procedure Reset_Analyzed_Flags (N : Node_Id) is
      function Clear_Analyzed (N : Node_Id) return Traverse_Result;
      --  Function used to reset Analyzed flags in tree. Note that we do
      --  not reset Analyzed flags in entities, since there is no need to
      --  reanalyze entities, and indeed, it is wrong to do so, since it
      --  can result in generating auxiliary stuff more than once.

      --------------------
      -- Clear_Analyzed --
      --------------------

      function Clear_Analyzed (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) not in N_Entity then
            Set_Analyzed (N, False);
         end if;

         return OK;
      end Clear_Analyzed;

      procedure Reset_Analyzed is new Traverse_Proc (Clear_Analyzed);

   --  Start of processing for Reset_Analyzed_Flags

   begin
      Reset_Analyzed (N);
   end Reset_Analyzed_Flags;

   ------------------------
   -- Restore_SPARK_Mode --
   ------------------------

   procedure Restore_SPARK_Mode
     (Mode : SPARK_Mode_Type;
      Prag : Node_Id)
   is
   begin
      SPARK_Mode        := Mode;
      SPARK_Mode_Pragma := Prag;
   end Restore_SPARK_Mode;

   --------------------------------
   -- Returns_Unconstrained_Type --
   --------------------------------

   function Returns_Unconstrained_Type (Subp : Entity_Id) return Boolean is
   begin
      return Ekind (Subp) = E_Function
        and then not Is_Scalar_Type (Etype (Subp))
        and then not Is_Access_Type (Etype (Subp))
        and then not Is_Constrained (Etype (Subp));
   end Returns_Unconstrained_Type;

   ----------------------------
   -- Root_Type_Of_Full_View --
   ----------------------------

   function Root_Type_Of_Full_View (T : Entity_Id) return Entity_Id is
      Rtyp : constant Entity_Id := Root_Type (T);

   begin
      --  The root type of the full view may itself be a private type. Keep
      --  looking for the ultimate derivation parent.

      if Is_Private_Type (Rtyp) and then Present (Full_View (Rtyp)) then
         return Root_Type_Of_Full_View (Full_View (Rtyp));
      else
         return Rtyp;
      end if;
   end Root_Type_Of_Full_View;

   ---------------------------
   -- Safe_To_Capture_Value --
   ---------------------------

   function Safe_To_Capture_Value
     (N    : Node_Id;
      Ent  : Entity_Id;
      Cond : Boolean := False) return Boolean
   is
   begin
      --  The only entities for which we track constant values are variables
      --  which are not renamings, constants, out parameters, and in out
      --  parameters, so check if we have this case.

      --  Note: it may seem odd to track constant values for constants, but in
      --  fact this routine is used for other purposes than simply capturing
      --  the value. In particular, the setting of Known[_Non]_Null.

      if (Ekind (Ent) = E_Variable and then No (Renamed_Object (Ent)))
            or else
          Ekind_In (Ent, E_Constant, E_Out_Parameter, E_In_Out_Parameter)
      then
         null;

      --  For conditionals, we also allow loop parameters and all formals,
      --  including in parameters.

      elsif Cond and then Ekind_In (Ent, E_Loop_Parameter, E_In_Parameter) then
         null;

      --  For all other cases, not just unsafe, but impossible to capture
      --  Current_Value, since the above are the only entities which have
      --  Current_Value fields.

      else
         return False;
      end if;

      --  Skip if volatile or aliased, since funny things might be going on in
      --  these cases which we cannot necessarily track. Also skip any variable
      --  for which an address clause is given, or whose address is taken. Also
      --  never capture value of library level variables (an attempt to do so
      --  can occur in the case of package elaboration code).

      if Treat_As_Volatile (Ent)
        or else Is_Aliased (Ent)
        or else Present (Address_Clause (Ent))
        or else Address_Taken (Ent)
        or else (Is_Library_Level_Entity (Ent)
                  and then Ekind (Ent) = E_Variable)
      then
         return False;
      end if;

      --  OK, all above conditions are met. We also require that the scope of
      --  the reference be the same as the scope of the entity, not counting
      --  packages and blocks and loops.

      declare
         E_Scope : constant Entity_Id := Scope (Ent);
         R_Scope : Entity_Id;

      begin
         R_Scope := Current_Scope;
         while R_Scope /= Standard_Standard loop
            exit when R_Scope = E_Scope;

            if not Ekind_In (R_Scope, E_Package, E_Block, E_Loop) then
               return False;
            else
               R_Scope := Scope (R_Scope);
            end if;
         end loop;
      end;

      --  We also require that the reference does not appear in a context
      --  where it is not sure to be executed (i.e. a conditional context
      --  or an exception handler). We skip this if Cond is True, since the
      --  capturing of values from conditional tests handles this ok.

      if Cond then
         return True;
      end if;

      declare
         Desc : Node_Id;
         P    : Node_Id;

      begin
         Desc := N;

         --  Seems dubious that case expressions are not handled here ???

         P := Parent (N);
         while Present (P) loop
            if         Nkind (P) = N_If_Statement
              or else  Nkind (P) = N_Case_Statement
              or else (Nkind (P) in N_Short_Circuit
                        and then Desc = Right_Opnd (P))
              or else (Nkind (P) = N_If_Expression
                        and then Desc /= First (Expressions (P)))
              or else  Nkind (P) = N_Exception_Handler
              or else  Nkind (P) = N_Selective_Accept
              or else  Nkind (P) = N_Conditional_Entry_Call
              or else  Nkind (P) = N_Timed_Entry_Call
              or else  Nkind (P) = N_Asynchronous_Select
            then
               return False;

            else
               Desc := P;
               P := Parent (P);

               --  A special Ada 2012 case: the original node may be part
               --  of the else_actions of a conditional expression, in which
               --  case it might not have been expanded yet, and appears in
               --  a non-syntactic list of actions. In that case it is clearly
               --  not safe to save a value.

               if No (P)
                 and then Is_List_Member (Desc)
                 and then No (Parent (List_Containing (Desc)))
               then
                  return False;
               end if;
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

   -----------------
   -- Same_Object --
   -----------------

   function Same_Object (Node1, Node2 : Node_Id) return Boolean is
      N1 : constant Node_Id := Original_Node (Node1);
      N2 : constant Node_Id := Original_Node (Node2);
      --  We do the tests on original nodes, since we are most interested
      --  in the original source, not any expansion that got in the way.

      K1 : constant Node_Kind := Nkind (N1);
      K2 : constant Node_Kind := Nkind (N2);

   begin
      --  First case, both are entities with same entity

      if K1 in N_Has_Entity and then K2 in N_Has_Entity then
         declare
            EN1 : constant Entity_Id := Entity (N1);
            EN2 : constant Entity_Id := Entity (N2);
         begin
            if Present (EN1) and then Present (EN2)
              and then (Ekind_In (EN1, E_Variable, E_Constant)
                         or else Is_Formal (EN1))
              and then EN1 = EN2
            then
               return True;
            end if;
         end;
      end if;

      --  Second case, selected component with same selector, same record

      if K1 = N_Selected_Component
        and then K2 = N_Selected_Component
        and then Chars (Selector_Name (N1)) = Chars (Selector_Name (N2))
      then
         return Same_Object (Prefix (N1), Prefix (N2));

      --  Third case, indexed component with same subscripts, same array

      elsif K1 = N_Indexed_Component
        and then K2 = N_Indexed_Component
        and then Same_Object (Prefix (N1), Prefix (N2))
      then
         declare
            E1, E2 : Node_Id;
         begin
            E1 := First (Expressions (N1));
            E2 := First (Expressions (N2));
            while Present (E1) loop
               if not Same_Value (E1, E2) then
                  return False;
               else
                  Next (E1);
                  Next (E2);
               end if;
            end loop;

            return True;
         end;

      --  Fourth case, slice of same array with same bounds

      elsif K1 = N_Slice
        and then K2 = N_Slice
        and then Nkind (Discrete_Range (N1)) = N_Range
        and then Nkind (Discrete_Range (N2)) = N_Range
        and then Same_Value (Low_Bound (Discrete_Range (N1)),
                             Low_Bound (Discrete_Range (N2)))
        and then Same_Value (High_Bound (Discrete_Range (N1)),
                             High_Bound (Discrete_Range (N2)))
      then
         return Same_Name (Prefix (N1), Prefix (N2));

      --  All other cases, not clearly the same object

      else
         return False;
      end if;
   end Same_Object;

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

   ----------------
   -- Same_Value --
   ----------------

   function Same_Value (Node1, Node2 : Node_Id) return Boolean is
   begin
      if Compile_Time_Known_Value (Node1)
        and then Compile_Time_Known_Value (Node2)
      then
         --  Handle properly compile-time expressions that are not
         --  scalar.

         if Is_String_Type (Etype (Node1)) then
            return Expr_Value_S (Node1) = Expr_Value_S (Node2);

         else
            return Expr_Value (Node1) = Expr_Value (Node2);
         end if;

      elsif Same_Object (Node1, Node2) then
         return True;
      else
         return False;
      end if;
   end Same_Value;

   --------------------
   -- Set_SPARK_Mode --
   --------------------

   procedure Set_SPARK_Mode (Context : Entity_Id) is
   begin
      --  Do not consider illegal or partially decorated constructs

      if Ekind (Context) = E_Void or else Error_Posted (Context) then
         null;

      elsif Present (SPARK_Pragma (Context)) then
         Install_SPARK_Mode
           (Mode => Get_SPARK_Mode_From_Annotation (SPARK_Pragma (Context)),
            Prag => SPARK_Pragma (Context));
      end if;
   end Set_SPARK_Mode;

   -------------------------
   -- Scalar_Part_Present --
   -------------------------

   function Scalar_Part_Present (T : Entity_Id) return Boolean is
      C : Entity_Id;

   begin
      if Is_Scalar_Type (T) then
         return True;

      elsif Is_Array_Type (T) then
         return Scalar_Part_Present (Component_Type (T));

      elsif Is_Record_Type (T) or else Has_Discriminants (T) then
         C := First_Component_Or_Discriminant (T);
         while Present (C) loop
            if Scalar_Part_Present (Etype (C)) then
               return True;
            else
               Next_Component_Or_Discriminant (C);
            end if;
         end loop;
      end if;

      return False;
   end Scalar_Part_Present;

   ------------------------
   -- Scope_Is_Transient --
   ------------------------

   function Scope_Is_Transient return Boolean is
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

   --------------------
   -- Set_Convention --
   --------------------

   procedure Set_Convention (E : Entity_Id; Val : Snames.Convention_Id) is
   begin
      Basic_Set_Convention (E, Val);

      if Is_Type (E)
        and then Is_Access_Subprogram_Type (Base_Type (E))
        and then Has_Foreign_Convention (E)
      then

         --  A pragma Convention in an instance may apply to the subtype
         --  created for a formal, in which case we have already verified
         --  that conventions of actual and formal match and there is nothing
         --  to flag on the subtype.

         if In_Instance then
            null;
         else
            Set_Can_Use_Internal_Rep (E, False);
         end if;
      end if;

      --  If E is an object or component, and the type of E is an anonymous
      --  access type with no convention set, then also set the convention of
      --  the anonymous access type. We do not do this for anonymous protected
      --  types, since protected types always have the default convention.

      if Present (Etype (E))
        and then (Is_Object (E)
                   or else Ekind (E) = E_Component

                   --  Allow E_Void (happens for pragma Convention appearing
                   --  in the middle of a record applying to a component)

                   or else Ekind (E) = E_Void)
      then
         declare
            Typ : constant Entity_Id := Etype (E);

         begin
            if Ekind_In (Typ, E_Anonymous_Access_Type,
                              E_Anonymous_Access_Subprogram_Type)
              and then not Has_Convention_Pragma (Typ)
            then
               Basic_Set_Convention (Typ, Val);
               Set_Has_Convention_Pragma (Typ);

               --  And for the access subprogram type, deal similarly with the
               --  designated E_Subprogram_Type if it is also internal (which
               --  it always is?)

               if Ekind (Typ) = E_Anonymous_Access_Subprogram_Type then
                  declare
                     Dtype : constant Entity_Id := Designated_Type (Typ);
                  begin
                     if Ekind (Dtype) = E_Subprogram_Type
                       and then Is_Itype (Dtype)
                       and then not Has_Convention_Pragma (Dtype)
                     then
                        Basic_Set_Convention (Dtype, Val);
                        Set_Has_Convention_Pragma (Dtype);
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
   end Set_Convention;

   ------------------------
   -- Set_Current_Entity --
   ------------------------

   --  The given entity is to be set as the currently visible definition of its
   --  associated name (i.e. the Node_Id associated with its name). All we have
   --  to do is to get the name from the identifier, and then set the
   --  associated Node_Id to point to the given entity.

   procedure Set_Current_Entity (E : Entity_Id) is
   begin
      Set_Name_Entity_Id (Chars (E), E);
   end Set_Current_Entity;

   ---------------------------
   -- Set_Debug_Info_Needed --
   ---------------------------

   procedure Set_Debug_Info_Needed (T : Entity_Id) is

      procedure Set_Debug_Info_Needed_If_Not_Set (E : Entity_Id);
      pragma Inline (Set_Debug_Info_Needed_If_Not_Set);
      --  Used to set debug info in a related node if not set already

      --------------------------------------
      -- Set_Debug_Info_Needed_If_Not_Set --
      --------------------------------------

      procedure Set_Debug_Info_Needed_If_Not_Set (E : Entity_Id) is
      begin
         if Present (E) and then not Needs_Debug_Info (E) then
            Set_Debug_Info_Needed (E);

            --  For a private type, indicate that the full view also needs
            --  debug information.

            if Is_Type (E)
              and then Is_Private_Type (E)
              and then Present (Full_View (E))
            then
               Set_Debug_Info_Needed (Full_View (E));
            end if;
         end if;
      end Set_Debug_Info_Needed_If_Not_Set;

   --  Start of processing for Set_Debug_Info_Needed

   begin
      --  Nothing to do if argument is Empty or has Debug_Info_Off set, which
      --  indicates that Debug_Info_Needed is never required for the entity.
      --  Nothing to do if entity comes from a predefined file. Library files
      --  are compiled without debug information, but inlined bodies of these
      --  routines may appear in user code, and debug information on them ends
      --  up complicating debugging the user code.

      if No (T)
        or else Debug_Info_Off (T)
      then
         return;

      elsif In_Inlined_Body and then In_Predefined_Unit (T) then
         Set_Needs_Debug_Info (T, False);
      end if;

      --  Set flag in entity itself. Note that we will go through the following
      --  circuitry even if the flag is already set on T. That's intentional,
      --  it makes sure that the flag will be set in subsidiary entities.

      Set_Needs_Debug_Info (T);

      --  Set flag on subsidiary entities if not set already

      if Is_Object (T) then
         Set_Debug_Info_Needed_If_Not_Set (Etype (T));

      elsif Is_Type (T) then
         Set_Debug_Info_Needed_If_Not_Set (Etype (T));

         if Is_Record_Type (T) then
            declare
               Ent : Entity_Id := First_Entity (T);
            begin
               while Present (Ent) loop
                  Set_Debug_Info_Needed_If_Not_Set (Ent);
                  Next_Entity (Ent);
               end loop;
            end;

            --  For a class wide subtype, we also need debug information
            --  for the equivalent type.

            if Ekind (T) = E_Class_Wide_Subtype then
               Set_Debug_Info_Needed_If_Not_Set (Equivalent_Type (T));
            end if;

         elsif Is_Array_Type (T) then
            Set_Debug_Info_Needed_If_Not_Set (Component_Type (T));

            declare
               Indx : Node_Id := First_Index (T);
            begin
               while Present (Indx) loop
                  Set_Debug_Info_Needed_If_Not_Set (Etype (Indx));
                  Indx := Next_Index (Indx);
               end loop;
            end;

            --  For a packed array type, we also need debug information for
            --  the type used to represent the packed array. Conversely, we
            --  also need it for the former if we need it for the latter.

            if Is_Packed (T) then
               Set_Debug_Info_Needed_If_Not_Set (Packed_Array_Impl_Type (T));
            end if;

            if Is_Packed_Array_Impl_Type (T) then
               Set_Debug_Info_Needed_If_Not_Set (Original_Array_Type (T));
            end if;

         elsif Is_Access_Type (T) then
            Set_Debug_Info_Needed_If_Not_Set (Directly_Designated_Type (T));

         elsif Is_Private_Type (T) then
            declare
               FV : constant Entity_Id := Full_View (T);

            begin
               Set_Debug_Info_Needed_If_Not_Set (FV);

               --  If the full view is itself a derived private type, we need
               --  debug information on its underlying type.

               if Present (FV)
                 and then Is_Private_Type (FV)
                 and then Present (Underlying_Full_View (FV))
               then
                  Set_Needs_Debug_Info (Underlying_Full_View (FV));
               end if;
            end;

         elsif Is_Protected_Type (T) then
            Set_Debug_Info_Needed_If_Not_Set (Corresponding_Record_Type (T));

         elsif Is_Scalar_Type (T) then

            --  If the subrange bounds are materialized by dedicated constant
            --  objects, also include them in the debug info to make sure the
            --  debugger can properly use them.

            if Present (Scalar_Range (T))
              and then Nkind (Scalar_Range (T)) = N_Range
            then
               declare
                  Low_Bnd  : constant Node_Id := Type_Low_Bound (T);
                  High_Bnd : constant Node_Id := Type_High_Bound (T);

               begin
                  if Is_Entity_Name (Low_Bnd) then
                     Set_Debug_Info_Needed_If_Not_Set (Entity (Low_Bnd));
                  end if;

                  if Is_Entity_Name (High_Bnd) then
                     Set_Debug_Info_Needed_If_Not_Set (Entity (High_Bnd));
                  end if;
               end;
            end if;
         end if;
      end if;
   end Set_Debug_Info_Needed;

   ----------------------------
   -- Set_Entity_With_Checks --
   ----------------------------

   procedure Set_Entity_With_Checks (N : Node_Id; Val : Entity_Id) is
      Val_Actual : Entity_Id;
      Nod        : Node_Id;
      Post_Node  : Node_Id;

   begin
      --  Unconditionally set the entity

      Set_Entity (N, Val);

      --  The node to post on is the selector in the case of an expanded name,
      --  and otherwise the node itself.

      if Nkind (N) = N_Expanded_Name then
         Post_Node := Selector_Name (N);
      else
         Post_Node := N;
      end if;

      --  Check for violation of No_Fixed_IO

      if Restriction_Check_Required (No_Fixed_IO)
        and then
          ((RTU_Loaded (Ada_Text_IO)
             and then (Is_RTE (Val, RE_Decimal_IO)
                         or else
                       Is_RTE (Val, RE_Fixed_IO)))

         or else
           (RTU_Loaded (Ada_Wide_Text_IO)
             and then (Is_RTE (Val, RO_WT_Decimal_IO)
                         or else
                       Is_RTE (Val, RO_WT_Fixed_IO)))

         or else
           (RTU_Loaded (Ada_Wide_Wide_Text_IO)
             and then (Is_RTE (Val, RO_WW_Decimal_IO)
                         or else
                       Is_RTE (Val, RO_WW_Fixed_IO))))

        --  A special extra check, don't complain about a reference from within
        --  the Ada.Interrupts package itself!

        and then not In_Same_Extended_Unit (N, Val)
      then
         Check_Restriction (No_Fixed_IO, Post_Node);
      end if;

      --  Remaining checks are only done on source nodes. Note that we test
      --  for violation of No_Fixed_IO even on non-source nodes, because the
      --  cases for checking violations of this restriction are instantiations
      --  where the reference in the instance has Comes_From_Source False.

      if not Comes_From_Source (N) then
         return;
      end if;

      --  Check for violation of No_Abort_Statements, which is triggered by
      --  call to Ada.Task_Identification.Abort_Task.

      if Restriction_Check_Required (No_Abort_Statements)
        and then (Is_RTE (Val, RE_Abort_Task))

        --  A special extra check, don't complain about a reference from within
        --  the Ada.Task_Identification package itself!

        and then not In_Same_Extended_Unit (N, Val)
      then
         Check_Restriction (No_Abort_Statements, Post_Node);
      end if;

      if Val = Standard_Long_Long_Integer then
         Check_Restriction (No_Long_Long_Integers, Post_Node);
      end if;

      --  Check for violation of No_Dynamic_Attachment

      if Restriction_Check_Required (No_Dynamic_Attachment)
        and then RTU_Loaded (Ada_Interrupts)
        and then (Is_RTE (Val, RE_Is_Reserved)      or else
                  Is_RTE (Val, RE_Is_Attached)      or else
                  Is_RTE (Val, RE_Current_Handler)  or else
                  Is_RTE (Val, RE_Attach_Handler)   or else
                  Is_RTE (Val, RE_Exchange_Handler) or else
                  Is_RTE (Val, RE_Detach_Handler)   or else
                  Is_RTE (Val, RE_Reference))

        --  A special extra check, don't complain about a reference from within
        --  the Ada.Interrupts package itself!

        and then not In_Same_Extended_Unit (N, Val)
      then
         Check_Restriction (No_Dynamic_Attachment, Post_Node);
      end if;

      --  Check for No_Implementation_Identifiers

      if Restriction_Check_Required (No_Implementation_Identifiers) then

         --  We have an implementation defined entity if it is marked as
         --  implementation defined, or is defined in a package marked as
         --  implementation defined. However, library packages themselves
         --  are excluded (we don't want to flag Interfaces itself, just
         --  the entities within it).

         if (Is_Implementation_Defined (Val)
              or else
                (Present (Scope (Val))
                  and then Is_Implementation_Defined (Scope (Val))))
           and then not (Ekind_In (Val, E_Package, E_Generic_Package)
                          and then Is_Library_Level_Entity (Val))
         then
            Check_Restriction (No_Implementation_Identifiers, Post_Node);
         end if;
      end if;

      --  Do the style check

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
                      or else Is_Subprogram_Or_Generic_Subprogram (Val_Actual))
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
   end Set_Entity_With_Checks;

   ------------------------
   -- Set_Name_Entity_Id --
   ------------------------

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id) is
   begin
      Set_Name_Table_Int (Id, Int (Val));
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

   ----------------------------------
   -- Set_Optimize_Alignment_Flags --
   ----------------------------------

   procedure Set_Optimize_Alignment_Flags (E : Entity_Id) is
   begin
      if Optimize_Alignment = 'S' then
         Set_Optimize_Alignment_Space (E);
      elsif Optimize_Alignment = 'T' then
         Set_Optimize_Alignment_Time (E);
      end if;
   end Set_Optimize_Alignment_Flags;

   -----------------------
   -- Set_Public_Status --
   -----------------------

   procedure Set_Public_Status (Id : Entity_Id) is
      S : constant Entity_Id := Current_Scope;

      function Within_HSS_Or_If (E : Entity_Id) return Boolean;
      --  Determines if E is defined within handled statement sequence or
      --  an if statement, returns True if so, False otherwise.

      ----------------------
      -- Within_HSS_Or_If --
      ----------------------

      function Within_HSS_Or_If (E : Entity_Id) return Boolean is
         N : Node_Id;
      begin
         N := Declaration_Node (E);
         loop
            N := Parent (N);

            if No (N) then
               return False;

            elsif Nkind_In (N, N_Handled_Sequence_Of_Statements,
                               N_If_Statement)
            then
               return True;
            end if;
         end loop;
      end Within_HSS_Or_If;

   --  Start of processing for Set_Public_Status

   begin
      --  Everything in the scope of Standard is public

      if S = Standard_Standard then
         Set_Is_Public (Id);

      --  Entity is definitely not public if enclosing scope is not public

      elsif not Is_Public (S) then
         return;

      --  An object or function declaration that occurs in a handled sequence
      --  of statements or within an if statement is the declaration for a
      --  temporary object or local subprogram generated by the expander. It
      --  never needs to be made public and furthermore, making it public can
      --  cause back end problems.

      elsif Nkind_In (Parent (Id), N_Object_Declaration,
                                   N_Function_Specification)
        and then Within_HSS_Or_If (Id)
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

   -----------------------------
   -- Set_Referenced_Modified --
   -----------------------------

   procedure Set_Referenced_Modified (N : Node_Id; Out_Param : Boolean) is
      Pref : Node_Id;

   begin
      --  Deal with indexed or selected component where prefix is modified

      if Nkind_In (N, N_Indexed_Component, N_Selected_Component) then
         Pref := Prefix (N);

         --  If prefix is access type, then it is the designated object that is
         --  being modified, which means we have no entity to set the flag on.

         if No (Etype (Pref)) or else Is_Access_Type (Etype (Pref)) then
            return;

            --  Otherwise chase the prefix

         else
            Set_Referenced_Modified (Pref, Out_Param);
         end if;

      --  Otherwise see if we have an entity name (only other case to process)

      elsif Is_Entity_Name (N) and then Present (Entity (N)) then
         Set_Referenced_As_LHS           (Entity (N), not Out_Param);
         Set_Referenced_As_Out_Parameter (Entity (N), Out_Param);
      end if;
   end Set_Referenced_Modified;

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

   ------------------------------
   -- Should_Ignore_Pragma_Par --
   ------------------------------

   function Should_Ignore_Pragma_Par (Prag_Name : Name_Id) return Boolean is
      pragma Assert (Compiler_State = Parsing);
      --  This one can't work during semantic analysis, because we don't have a
      --  correct Current_Source_File.

      Result : constant Boolean :=
                 Get_Name_Table_Boolean3 (Prag_Name)
                   and then not Is_Internal_File_Name
                                  (File_Name (Current_Source_File));
   begin
      return Result;
   end Should_Ignore_Pragma_Par;

   ------------------------------
   -- Should_Ignore_Pragma_Sem --
   ------------------------------

   function Should_Ignore_Pragma_Sem (N : Node_Id) return Boolean is
      pragma Assert (Compiler_State = Analyzing);
      Prag_Name : constant Name_Id := Pragma_Name (N);
      Result    : constant Boolean :=
                    Get_Name_Table_Boolean3 (Prag_Name)
                      and then not In_Internal_Unit (N);

   begin
      return Result;
   end Should_Ignore_Pragma_Sem;

   --------------------
   -- Static_Boolean --
   --------------------

   function Static_Boolean (N : Node_Id) return Uint is
   begin
      Analyze_And_Resolve (N, Standard_Boolean);

      if N = Error
        or else Error_Posted (N)
        or else Etype (N) = Any_Type
      then
         return No_Uint;
      end if;

      if Is_OK_Static_Expression (N) then
         if not Raises_Constraint_Error (N) then
            return Expr_Value (N);
         else
            return No_Uint;
         end if;

      elsif Etype (N) = Any_Type then
         return No_Uint;

      else
         Flag_Non_Static_Expr
           ("static boolean expression required here", N);
         return No_Uint;
      end if;
   end Static_Boolean;

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

      if Is_OK_Static_Expression (N) then
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

   --------------------------------------
   -- Subject_To_Loop_Entry_Attributes --
   --------------------------------------

   function Subject_To_Loop_Entry_Attributes (N : Node_Id) return Boolean is
      Stmt : Node_Id;

   begin
      Stmt := N;

      --  The expansion mechanism transform a loop subject to at least one
      --  'Loop_Entry attribute into a conditional block. Infinite loops lack
      --  the conditional part.

      if Nkind_In (Stmt, N_Block_Statement, N_If_Statement)
        and then Nkind (Original_Node (N)) = N_Loop_Statement
      then
         Stmt := Original_Node (N);
      end if;

      return
        Nkind (Stmt) = N_Loop_Statement
          and then Present (Identifier (Stmt))
          and then Present (Entity (Identifier (Stmt)))
          and then Has_Loop_Entry_Attributes (Entity (Identifier (Stmt)));
   end Subject_To_Loop_Entry_Attributes;

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

   -------------------------------
   -- Support_Atomic_Primitives --
   -------------------------------

   function Support_Atomic_Primitives (Typ : Entity_Id) return Boolean is
      Size : Int;

   begin
      --  Verify the alignment of Typ is known

      if not Known_Alignment (Typ) then
         return False;
      end if;

      if Known_Static_Esize (Typ) then
         Size := UI_To_Int (Esize (Typ));

      --  If the Esize (Object_Size) is unknown at compile time, look at the
      --  RM_Size (Value_Size) which may have been set by an explicit rep item.

      elsif Known_Static_RM_Size (Typ) then
         Size := UI_To_Int (RM_Size (Typ));

      --  Otherwise, the size is considered to be unknown.

      else
         return False;
      end if;

      --  Check that the size of the component is 8, 16, 32, or 64 bits and
      --  that Typ is properly aligned.

      case Size is
         when 8 | 16 | 32 | 64 =>
            return Size = UI_To_Int (Alignment (Typ)) * 8;

         when others =>
            return False;
      end case;
   end Support_Atomic_Primitives;

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
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;
   end Trace_Scope;

   -----------------------
   -- Transfer_Entities --
   -----------------------

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id) is
      procedure Set_Public_Status_Of (Id : Entity_Id);
      --  Set the Is_Public attribute of arbitrary entity Id by calling routine
      --  Set_Public_Status. If successful and Id denotes a record type, set
      --  the Is_Public attribute of its fields.

      --------------------------
      -- Set_Public_Status_Of --
      --------------------------

      procedure Set_Public_Status_Of (Id : Entity_Id) is
         Field : Entity_Id;

      begin
         if not Is_Public (Id) then
            Set_Public_Status (Id);

            --  When the input entity is a public record type, ensure that all
            --  its internal fields are also exposed to the linker. The fields
            --  of a class-wide type are never made public.

            if Is_Public (Id)
              and then Is_Record_Type (Id)
              and then not Is_Class_Wide_Type (Id)
            then
               Field := First_Entity (Id);
               while Present (Field) loop
                  Set_Is_Public (Field);
                  Next_Entity (Field);
               end loop;
            end if;
         end if;
      end Set_Public_Status_Of;

      --  Local variables

      Full_Id : Entity_Id;
      Id      : Entity_Id;

   --  Start of processing for Transfer_Entities

   begin
      Id := First_Entity (From);

      if Present (Id) then

         --  Merge the entity chain of the source scope with that of the
         --  destination scope.

         if Present (Last_Entity (To)) then
            Set_Next_Entity (Last_Entity (To), Id);
         else
            Set_First_Entity (To, Id);
         end if;

         Set_Last_Entity (To, Last_Entity (From));

         --  Inspect the entities of the source scope and update their Scope
         --  attribute.

         while Present (Id) loop
            Set_Scope            (Id, To);
            Set_Public_Status_Of (Id);

            --  Handle an internally generated full view for a private type

            if Is_Private_Type (Id)
              and then Present (Full_View (Id))
              and then Is_Itype (Full_View (Id))
            then
               Full_Id := Full_View (Id);

               Set_Scope            (Full_Id, To);
               Set_Public_Status_Of (Full_Id);
            end if;

            Next_Entity (Id);
         end loop;

         Set_First_Entity (From, Empty);
         Set_Last_Entity  (From, Empty);
      end if;
   end Transfer_Entities;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level (Typ : Entity_Id) return Uint is
      Btyp : Entity_Id;

   begin
      Btyp := Base_Type (Typ);

      --  Ada 2005 (AI-230): For most cases of anonymous access types, we
      --  simply use the level where the type is declared. This is true for
      --  stand-alone object declarations, and for anonymous access types
      --  associated with components the level is the same as that of the
      --  enclosing composite type. However, special treatment is needed for
      --  the cases of access parameters, return objects of an anonymous access
      --  type, and, in Ada 95, access discriminants of limited types.

      if Is_Access_Type (Btyp) then
         if Ekind (Btyp) = E_Anonymous_Access_Type then

            --  If the type is a nonlocal anonymous access type (such as for
            --  an access parameter) we treat it as being declared at the
            --  library level to ensure that names such as X.all'access don't
            --  fail static accessibility checks.

            if not Is_Local_Anonymous_Access (Typ) then
               return Scope_Depth (Standard_Standard);

            --  If this is a return object, the accessibility level is that of
            --  the result subtype of the enclosing function. The test here is
            --  little complicated, because we have to account for extended
            --  return statements that have been rewritten as blocks, in which
            --  case we have to find and the Is_Return_Object attribute of the
            --  itype's associated object. It would be nice to find a way to
            --  simplify this test, but it doesn't seem worthwhile to add a new
            --  flag just for purposes of this test. ???

            elsif Ekind (Scope (Btyp)) = E_Return_Statement
              or else
                (Is_Itype (Btyp)
                  and then Nkind (Associated_Node_For_Itype (Btyp)) =
                                                         N_Object_Declaration
                  and then Is_Return_Object
                             (Defining_Identifier
                                (Associated_Node_For_Itype (Btyp))))
            then
               declare
                  Scop : Entity_Id;

               begin
                  Scop := Scope (Scope (Btyp));
                  while Present (Scop) loop
                     exit when Ekind (Scop) = E_Function;
                     Scop := Scope (Scop);
                  end loop;

                  --  Treat the return object's type as having the level of the
                  --  function's result subtype (as per RM05-6.5(5.3/2)).

                  return Type_Access_Level (Etype (Scop));
               end;
            end if;
         end if;

         Btyp := Root_Type (Btyp);

         --  The accessibility level of anonymous access types associated with
         --  discriminants is that of the current instance of the type, and
         --  that's deeper than the type itself (AARM 3.10.2 (12.3.21)).

         --  AI-402: access discriminants have accessibility based on the
         --  object rather than the type in Ada 2005, so the above paragraph
         --  doesn't apply.

         --  ??? Needs completion with rules from AI-416

         if Ada_Version <= Ada_95
           and then Ekind (Typ) = E_Anonymous_Access_Type
           and then Present (Associated_Node_For_Itype (Typ))
           and then Nkind (Associated_Node_For_Itype (Typ)) =
                                                 N_Discriminant_Specification
         then
            return Scope_Depth (Enclosing_Dynamic_Scope (Btyp)) + 1;
         end if;
      end if;

      --  Return library level for a generic formal type. This is done because
      --  RM(10.3.2) says that "The statically deeper relationship does not
      --  apply to ... a descendant of a generic formal type". Rather than
      --  checking at each point where a static accessibility check is
      --  performed to see if we are dealing with a formal type, this rule is
      --  implemented by having Type_Access_Level and Deepest_Type_Access_Level
      --  return extreme values for a formal type; Deepest_Type_Access_Level
      --  returns Int'Last. By calling the appropriate function from among the
      --  two, we ensure that the static accessibility check will pass if we
      --  happen to run into a formal type. More specifically, we should call
      --  Deepest_Type_Access_Level instead of Type_Access_Level whenever the
      --  call occurs as part of a static accessibility check and the error
      --  case is the case where the type's level is too shallow (as opposed
      --  to too deep).

      if Is_Generic_Type (Root_Type (Btyp)) then
         return Scope_Depth (Standard_Standard);
      end if;

      return Scope_Depth (Enclosing_Dynamic_Scope (Btyp));
   end Type_Access_Level;

   ------------------------------------
   -- Type_Without_Stream_Operation  --
   ------------------------------------

   function Type_Without_Stream_Operation
     (T  : Entity_Id;
      Op : TSS_Name_Type := TSS_Null) return Entity_Id
   is
      BT         : constant Entity_Id := Base_Type (T);
      Op_Missing : Boolean;

   begin
      if not Restriction_Active (No_Default_Stream_Attributes) then
         return Empty;
      end if;

      if Is_Elementary_Type (T) then
         if Op = TSS_Null then
            Op_Missing :=
              No (TSS (BT, TSS_Stream_Read))
                or else No (TSS (BT, TSS_Stream_Write));

         else
            Op_Missing := No (TSS (BT, Op));
         end if;

         if Op_Missing then
            return T;
         else
            return Empty;
         end if;

      elsif Is_Array_Type (T) then
         return Type_Without_Stream_Operation (Component_Type (T), Op);

      elsif Is_Record_Type (T) then
         declare
            Comp  : Entity_Id;
            C_Typ : Entity_Id;

         begin
            Comp := First_Component (T);
            while Present (Comp) loop
               C_Typ := Type_Without_Stream_Operation (Etype (Comp), Op);

               if Present (C_Typ) then
                  return C_Typ;
               end if;

               Next_Component (Comp);
            end loop;

            return Empty;
         end;

      elsif Is_Private_Type (T) and then Present (Full_View (T)) then
         return Type_Without_Stream_Operation (Full_View (T), Op);
      else
         return Empty;
      end if;
   end Type_Without_Stream_Operation;

   ----------------------------
   -- Unique_Defining_Entity --
   ----------------------------

   function Unique_Defining_Entity (N : Node_Id) return Entity_Id is
   begin
      return Unique_Entity (Defining_Entity (N));
   end Unique_Defining_Entity;

   -------------------
   -- Unique_Entity --
   -------------------

   function Unique_Entity (E : Entity_Id) return Entity_Id is
      U : Entity_Id := E;
      P : Node_Id;

   begin
      case Ekind (E) is
         when E_Constant =>
            if Present (Full_View (E)) then
               U := Full_View (E);
            end if;

         when Entry_Kind =>
            if Nkind (Parent (E)) = N_Entry_Body then
               declare
                  Prot_Item : Entity_Id;
                  Prot_Type : Entity_Id;

               begin
                  if Ekind (E) = E_Entry then
                     Prot_Type := Scope (E);

                  --  Bodies of entry families are nested within an extra scope
                  --  that contains an entry index declaration

                  else
                     Prot_Type := Scope (Scope (E));
                  end if;

                  --  A protected type may be declared as a private type, in
                  --  which case we need to get its full view.

                  if Is_Private_Type (Prot_Type) then
                     Prot_Type := Full_View (Prot_Type);
                  end if;

                  --  Full view may not be present on error, in which case
                  --  return E by default.

                  if Present (Prot_Type) then
                     pragma Assert (Ekind (Prot_Type) = E_Protected_Type);

                     --  Traverse the entity list of the protected type and
                     --  locate an entry declaration which matches the entry
                     --  body.

                     Prot_Item := First_Entity (Prot_Type);
                     while Present (Prot_Item) loop
                        if Ekind (Prot_Item) in Entry_Kind
                          and then Corresponding_Body (Parent (Prot_Item)) = E
                        then
                           U := Prot_Item;
                           exit;
                        end if;

                        Next_Entity (Prot_Item);
                     end loop;
                  end if;
               end;
            end if;

         when Formal_Kind =>
            if Present (Spec_Entity (E)) then
               U := Spec_Entity (E);
            end if;

         when E_Package_Body =>
            P := Parent (E);

            if Nkind (P) = N_Defining_Program_Unit_Name then
               P := Parent (P);
            end if;

            if Nkind (P) = N_Package_Body
              and then Present (Corresponding_Spec (P))
            then
               U := Corresponding_Spec (P);

            elsif Nkind (P) = N_Package_Body_Stub
              and then Present (Corresponding_Spec_Of_Stub (P))
            then
               U := Corresponding_Spec_Of_Stub (P);
            end if;

         when E_Protected_Body =>
            P := Parent (E);

            if Nkind (P) = N_Protected_Body
              and then Present (Corresponding_Spec (P))
            then
               U := Corresponding_Spec (P);

            elsif Nkind (P) = N_Protected_Body_Stub
              and then Present (Corresponding_Spec_Of_Stub (P))
            then
               U := Corresponding_Spec_Of_Stub (P);

               if Is_Single_Protected_Object (U) then
                  U := Etype (U);
               end if;
            end if;

            if Is_Private_Type (U) then
               U := Full_View (U);
            end if;

         when E_Subprogram_Body =>
            P := Parent (E);

            if Nkind (P) = N_Defining_Program_Unit_Name then
               P := Parent (P);
            end if;

            P := Parent (P);

            if Nkind (P) = N_Subprogram_Body
              and then Present (Corresponding_Spec (P))
            then
               U := Corresponding_Spec (P);

            elsif Nkind (P) = N_Subprogram_Body_Stub
              and then Present (Corresponding_Spec_Of_Stub (P))
            then
               U := Corresponding_Spec_Of_Stub (P);

            elsif Nkind (P) = N_Subprogram_Renaming_Declaration then
               U := Corresponding_Spec (P);
            end if;

         when E_Task_Body =>
            P := Parent (E);

            if Nkind (P) = N_Task_Body
              and then Present (Corresponding_Spec (P))
            then
               U := Corresponding_Spec (P);

            elsif Nkind (P) = N_Task_Body_Stub
              and then Present (Corresponding_Spec_Of_Stub (P))
            then
               U := Corresponding_Spec_Of_Stub (P);

               if Is_Single_Task_Object (U) then
                  U := Etype (U);
               end if;
            end if;

            if Is_Private_Type (U) then
               U := Full_View (U);
            end if;

         when Type_Kind =>
            if Present (Full_View (E)) then
               U := Full_View (E);
            end if;

         when others =>
            null;
      end case;

      return U;
   end Unique_Entity;

   -----------------
   -- Unique_Name --
   -----------------

   function Unique_Name (E : Entity_Id) return String is

      --  Names in E_Subprogram_Body or E_Package_Body entities are not
      --  reliable, as they may not include the overloading suffix. Instead,
      --  when looking for the name of E or one of its enclosing scope, we get
      --  the name of the corresponding Unique_Entity.

      U : constant Entity_Id := Unique_Entity (E);

      function This_Name return String;

      ---------------
      -- This_Name --
      ---------------

      function This_Name return String is
      begin
         return Get_Name_String (Chars (U));
      end This_Name;

   --  Start of processing for Unique_Name

   begin
      if E = Standard_Standard
        or else Has_Fully_Qualified_Name (E)
      then
         return This_Name;

      elsif Ekind (E) = E_Enumeration_Literal then
         return Unique_Name (Etype (E)) & "__" & This_Name;

      else
         declare
            S : constant Entity_Id := Scope (U);
            pragma Assert (Present (S));

         begin
            --  Prefix names of predefined types with standard__, but leave
            --  names of user-defined packages and subprograms without prefix
            --  (even if technically they are nested in the Standard package).

            if S = Standard_Standard then
               if Ekind (U) = E_Package or else Is_Subprogram (U) then
                  return This_Name;
               else
                  return Unique_Name (S) & "__" & This_Name;
               end if;

            --  For intances of generic subprograms use the name of the related
            --  instace and skip the scope of its wrapper package.

            elsif Is_Wrapper_Package (S) then
               pragma Assert (Scope (S) = Scope (Related_Instance (S)));
               --  Wrapper package and the instantiation are in the same scope

               declare
                  Enclosing_Name : constant String :=
                    Unique_Name (Scope (S)) & "__" &
                      Get_Name_String (Chars (Related_Instance (S)));

               begin
                  if Is_Subprogram (U)
                    and then not Is_Generic_Actual_Subprogram (U)
                  then
                     return Enclosing_Name;
                  else
                     return Enclosing_Name & "__" & This_Name;
                  end if;
               end;

            else
               return Unique_Name (S) & "__" & This_Name;
            end if;
         end;
      end if;
   end Unique_Name;

   ---------------------
   -- Unit_Is_Visible --
   ---------------------

   function Unit_Is_Visible (U : Entity_Id) return Boolean is
      Curr        : constant Node_Id   := Cunit (Current_Sem_Unit);
      Curr_Entity : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);

      function Unit_In_Parent_Context (Par_Unit : Node_Id) return Boolean;
      --  For a child unit, check whether unit appears in a with_clause
      --  of a parent.

      function Unit_In_Context (Comp_Unit : Node_Id) return Boolean;
      --  Scan the context clause of one compilation unit looking for a
      --  with_clause for the unit in question.

      ----------------------------
      -- Unit_In_Parent_Context --
      ----------------------------

      function Unit_In_Parent_Context (Par_Unit : Node_Id) return Boolean is
      begin
         if Unit_In_Context (Par_Unit) then
            return True;

         elsif Is_Child_Unit (Defining_Entity (Unit (Par_Unit))) then
            return Unit_In_Parent_Context (Parent_Spec (Unit (Par_Unit)));

         else
            return False;
         end if;
      end Unit_In_Parent_Context;

      ---------------------
      -- Unit_In_Context --
      ---------------------

      function Unit_In_Context (Comp_Unit : Node_Id) return Boolean is
         Clause : Node_Id;

      begin
         Clause := First (Context_Items (Comp_Unit));
         while Present (Clause) loop
            if Nkind (Clause) = N_With_Clause then
               if Library_Unit (Clause) = U then
                  return True;

               --  The with_clause may denote a renaming of the unit we are
               --  looking for, eg. Text_IO which renames Ada.Text_IO.

               elsif
                 Renamed_Entity (Entity (Name (Clause))) =
                                                Defining_Entity (Unit (U))
               then
                  return True;
               end if;
            end if;

            Next (Clause);
         end loop;

         return False;
      end Unit_In_Context;

   --  Start of processing for Unit_Is_Visible

   begin
      --  The currrent unit is directly visible

      if Curr = U then
         return True;

      elsif Unit_In_Context (Curr) then
         return True;

      --  If the current unit is a body, check the context of the spec

      elsif Nkind (Unit (Curr)) = N_Package_Body
        or else
          (Nkind (Unit (Curr)) = N_Subprogram_Body
            and then not Acts_As_Spec (Unit (Curr)))
      then
         if Unit_In_Context (Library_Unit (Curr)) then
            return True;
         end if;
      end if;

      --  If the spec is a child unit, examine the parents

      if Is_Child_Unit (Curr_Entity) then
         if Nkind (Unit (Curr)) in N_Unit_Body then
            return
              Unit_In_Parent_Context
                (Parent_Spec (Unit (Library_Unit (Curr))));
         else
            return Unit_In_Parent_Context (Parent_Spec (Unit (Curr)));
         end if;

      else
         return False;
      end if;
   end Unit_Is_Visible;

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

   ---------------
   -- Unqualify --
   ---------------

   function Unqualify (Expr : Node_Id) return Node_Id is
   begin
      --  Recurse to handle unlikely case of multiple levels of qualification

      if Nkind (Expr) = N_Qualified_Expression then
         return Unqualify (Expression (Expr));

      --  Normal case, not a qualified expression

      else
         return Expr;
      end if;
   end Unqualify;

   -----------------------
   -- Visible_Ancestors --
   -----------------------

   function Visible_Ancestors (Typ : Entity_Id) return Elist_Id is
      List_1 : Elist_Id;
      List_2 : Elist_Id;
      Elmt   : Elmt_Id;

   begin
      pragma Assert (Is_Record_Type (Typ) and then Is_Tagged_Type (Typ));

      --  Collect all the parents and progenitors of Typ. If the full-view of
      --  private parents and progenitors is available then it is used to
      --  generate the list of visible ancestors; otherwise their partial
      --  view is added to the resulting list.

      Collect_Parents
        (T               => Typ,
         List            => List_1,
         Use_Full_View   => True);

      Collect_Interfaces
        (T               => Typ,
         Ifaces_List     => List_2,
         Exclude_Parents => True,
         Use_Full_View   => True);

      --  Join the two lists. Avoid duplications because an interface may
      --  simultaneously be parent and progenitor of a type.

      Elmt := First_Elmt (List_2);
      while Present (Elmt) loop
         Append_Unique_Elmt (Node (Elmt), List_1);
         Next_Elmt (Elmt);
      end loop;

      return List_1;
   end Visible_Ancestors;

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

   ------------------
   -- Within_Scope --
   ------------------

   function Within_Scope (E : Entity_Id; S : Entity_Id) return Boolean is
   begin
      return Scope_Within_Or_Same (Scope (E), S);
   end Within_Scope;

   ----------------
   -- Wrong_Type --
   ----------------

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id) is
      Found_Type : constant Entity_Id := First_Subtype (Etype (Expr));
      Expec_Type : constant Entity_Id := First_Subtype (Expected_Type);

      Matching_Field : Entity_Id;
      --  Entity to give a more precise suggestion on how to write a one-
      --  element positional aggregate.

      function Has_One_Matching_Field return Boolean;
      --  Determines if Expec_Type is a record type with a single component or
      --  discriminant whose type matches the found type or is one dimensional
      --  array whose component type matches the found type. In the case of
      --  one discriminant, we ignore the variant parts. That's not accurate,
      --  but good enough for the warning.

      ----------------------------
      -- Has_One_Matching_Field --
      ----------------------------

      function Has_One_Matching_Field return Boolean is
         E : Entity_Id;

      begin
         Matching_Field := Empty;

         if Is_Array_Type (Expec_Type)
           and then Number_Dimensions (Expec_Type) = 1
           and then Covers (Etype (Component_Type (Expec_Type)), Found_Type)
         then
            --  Use type name if available. This excludes multidimensional
            --  arrays and anonymous arrays.

            if Comes_From_Source (Expec_Type) then
               Matching_Field := Expec_Type;

            --  For an assignment, use name of target

            elsif Nkind (Parent (Expr)) = N_Assignment_Statement
              and then Is_Entity_Name (Name (Parent (Expr)))
            then
               Matching_Field := Entity (Name (Parent (Expr)));
            end if;

            return True;

         elsif not Is_Record_Type (Expec_Type) then
            return False;

         else
            E := First_Entity (Expec_Type);
            loop
               if No (E) then
                  return False;

               elsif not Ekind_In (E, E_Discriminant, E_Component)
                 or else Nam_In (Chars (E), Name_uTag, Name_uParent)
               then
                  Next_Entity (E);

               else
                  exit;
               end if;
            end loop;

            if not Covers (Etype (E), Found_Type) then
               return False;

            elsif Present (Next_Entity (E))
              and then (Ekind (E) = E_Component
                         or else Ekind (Next_Entity (E)) = E_Discriminant)
            then
               return False;

            else
               Matching_Field := E;
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

      --  If one of the types is a Taft-Amendment type and the other it its
      --  completion, it must be an illegal use of a TAT in the spec, for
      --  which an error was already emitted. Avoid cascaded errors.

      elsif Is_Incomplete_Type (Expec_Type)
        and then Has_Completion_In_Body (Expec_Type)
        and then Full_View (Expec_Type) = Etype (Expr)
      then
         return;

      elsif Is_Incomplete_Type (Etype (Expr))
        and then Has_Completion_In_Body (Etype (Expr))
        and then Full_View (Etype (Expr)) = Expec_Type
      then
         return;

      --  In  an instance, there is an ongoing problem with completion of
      --  type derived from private types. Their structure is what Gigi
      --  expects, but the  Etype is the parent type rather than the
      --  derived private type itself. Do not flag error in this case. The
      --  private completion is an entity without a parent, like an Itype.
      --  Similarly, full and partial views may be incorrect in the instance.
      --  There is no simple way to insure that it is consistent ???

      --  A similar view discrepancy can happen in an inlined body, for the
      --  same reason: inserted body may be outside of the original package
      --  and only partial views are visible at the point of insertion.

      elsif In_Instance or else In_Inlined_Body then
         if Etype (Etype (Expr)) = Etype (Expected_Type)
           and then
             (Has_Private_Declaration (Expected_Type)
               or else Has_Private_Declaration (Etype (Expr)))
           and then No (Parent (Expected_Type))
         then
            return;

         elsif Nkind (Parent (Expr)) = N_Qualified_Expression
           and then Entity (Subtype_Mark (Parent (Expr))) = Expected_Type
         then
            return;

         elsif Is_Private_Type (Expected_Type)
           and then Present (Full_View (Expected_Type))
           and then Covers (Full_View (Expected_Type), Etype (Expr))
         then
            return;

         --  Conversely, type of expression may be the private one

         elsif Is_Private_Type (Base_Type (Etype (Expr)))
           and then Full_View (Base_Type (Etype (Expr))) = Expected_Type
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

         if Present (Matching_Field) then
            if Is_Array_Type (Expec_Type) then
               Error_Msg_NE
                 ("\write instead `&''First ='> ...`", Expr, Matching_Field);
            else
               Error_Msg_NE
                 ("\write instead `& ='> ...`", Expr, Matching_Field);
            end if;
         end if;

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
         Error_Msg_N -- CODEFIX
           ("result must be general access type!", Expr);
         Error_Msg_NE -- CODEFIX
           ("add ALL to }!", Expr, Expec_Type);

      --  Another special check, if the expected type is an integer type,
      --  but the expression is of type System.Address, and the parent is
      --  an addition or subtraction operation whose left operand is the
      --  expression in question and whose right operand is of an integral
      --  type, then this is an attempt at address arithmetic, so give
      --  appropriate message.

      elsif Is_Integer_Type (Expec_Type)
        and then Is_RTE (Found_Type, RE_Address)
        and then Nkind_In (Parent (Expr), N_Op_Add, N_Op_Subtract)
        and then Expr = Left_Opnd (Parent (Expr))
        and then Is_Integer_Type (Etype (Right_Opnd (Parent (Expr))))
      then
         Error_Msg_N
           ("address arithmetic not predefined in package System",
            Parent (Expr));
         Error_Msg_N
           ("\possible missing with/use of System.Storage_Elements",
            Parent (Expr));
         return;

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
              ("\\found an access type with designated}!",
                Expr, Designated_Type (Found_Type));
         else
            if From_Limited_With (Found_Type) then
               Error_Msg_NE ("\\found incomplete}!", Expr, Found_Type);
               Error_Msg_Qual_Level := 99;
               Error_Msg_NE -- CODEFIX
                 ("\\missing `WITH &;", Expr, Scope (Found_Type));
               Error_Msg_Qual_Level := 0;
            else
               Error_Msg_NE ("found}!", Expr, Found_Type);
            end if;
         end if;

      --  Normal case of one type found, some other type expected

      else
         --  If the names of the two types are the same, see if some number
         --  of levels of qualification will help. Don't try more than three
         --  levels, and if we get to standard, it's no use (and probably
         --  represents an error in the compiler) Also do not bother with
         --  internal scope names.

         declare
            Expec_Scope : Entity_Id;
            Found_Scope : Entity_Id;

         begin
            Expec_Scope := Expec_Type;
            Found_Scope := Found_Type;

            for Levels in Nat range 0 .. 3 loop
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
            Error_Msg_N ("\\found package name!", Expr);

         elsif Is_Entity_Name (Expr)
           and then Ekind_In (Entity (Expr), E_Procedure, E_Generic_Procedure)
         then
            if Ekind (Expec_Type) = E_Access_Subprogram_Type then
               Error_Msg_N
                 ("found procedure name, possibly missing Access attribute!",
                   Expr);
            else
               Error_Msg_N
                 ("\\found procedure name instead of function!", Expr);
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
            Error_Msg_NE ("\\found premature usage of}!", Expr, Found_Type);

         else
            Error_Msg_NE ("\\found}!", Expr, Found_Type);
         end if;

         --  A special check for cases like M1 and M2 = 0 where M1 and M2 are
         --  of the same modular type, and (M1 and M2) = 0 was intended.

         if Expec_Type = Standard_Boolean
           and then Is_Modular_Integer_Type (Found_Type)
           and then Nkind_In (Parent (Expr), N_Op_And, N_Op_Or, N_Op_Xor)
           and then Nkind (Right_Opnd (Parent (Expr))) in N_Op_Compare
         then
            declare
               Op : constant Node_Id := Right_Opnd (Parent (Expr));
               L  : constant Node_Id := Left_Opnd (Op);
               R  : constant Node_Id := Right_Opnd (Op);

            begin
               --  The case for the message is when the left operand of the
               --  comparison is the same modular type, or when it is an
               --  integer literal (or other universal integer expression),
               --  which would have been typed as the modular type if the
               --  parens had been there.

               if (Etype (L) = Found_Type
                     or else
                   Etype (L) = Universal_Integer)
                 and then Is_Integer_Type (Etype (R))
               then
                  Error_Msg_N
                    ("\\possible missing parens for modular operation", Expr);
               end if;
            end;
         end if;

         --  Reset error message qualification indication

         Error_Msg_Qual_Level := 0;
      end if;
   end Wrong_Type;

   --------------------------------
   -- Yields_Synchronized_Object --
   --------------------------------

   function Yields_Synchronized_Object (Typ : Entity_Id) return Boolean is
      Has_Sync_Comp : Boolean := False;
      Id            : Entity_Id;

   begin
      --  An array type yields a synchronized object if its component type
      --  yields a synchronized object.

      if Is_Array_Type (Typ) then
         return Yields_Synchronized_Object (Component_Type (Typ));

      --  A descendant of type Ada.Synchronous_Task_Control.Suspension_Object
      --  yields a synchronized object by default.

      elsif Is_Descendant_Of_Suspension_Object (Typ) then
         return True;

      --  A protected type yields a synchronized object by default

      elsif Is_Protected_Type (Typ) then
         return True;

      --  A record type or type extension yields a synchronized object when its
      --  discriminants (if any) lack default values and all components are of
      --  a type that yelds a synchronized object.

      elsif Is_Record_Type (Typ) then

         --  Inspect all entities defined in the scope of the type, looking for
         --  components of a type that does not yeld a synchronized object or
         --  for discriminants with default values.

         Id := First_Entity (Typ);
         while Present (Id) loop
            if Comes_From_Source (Id) then
               if Ekind (Id) = E_Component then
                  if Yields_Synchronized_Object (Etype (Id)) then
                     Has_Sync_Comp := True;

                  --  The component does not yield a synchronized object

                  else
                     return False;
                  end if;

               elsif Ekind (Id) = E_Discriminant
                 and then Present (Expression (Parent (Id)))
               then
                  return False;
               end if;
            end if;

            Next_Entity (Id);
         end loop;

         --  Ensure that the parent type of a type extension yields a
         --  synchronized object.

         if Etype (Typ) /= Typ
           and then not Yields_Synchronized_Object (Etype (Typ))
         then
            return False;
         end if;

         --  If we get here, then all discriminants lack default values and all
         --  components are of a type that yields a synchronized object.

         return Has_Sync_Comp;

      --  A synchronized interface type yields a synchronized object by default

      elsif Is_Synchronized_Interface (Typ) then
         return True;

      --  A task type yelds a synchronized object by default

      elsif Is_Task_Type (Typ) then
         return True;

      --  Otherwise the type does not yield a synchronized object

      else
         return False;
      end if;
   end Yields_Synchronized_Object;

   ---------------------------
   -- Yields_Universal_Type --
   ---------------------------

   function Yields_Universal_Type (N : Node_Id) return Boolean is
   begin
      --  Integer and real literals are of a universal type

      if Nkind_In (N, N_Integer_Literal, N_Real_Literal) then
         return True;

      --  The values of certain attributes are of a universal type

      elsif Nkind (N) = N_Attribute_Reference then
         return
           Universal_Type_Attribute (Get_Attribute_Id (Attribute_Name (N)));

      --  ??? There are possibly other cases to consider

      else
         return False;
      end if;
   end Yields_Universal_Type;

end Sem_Util;
