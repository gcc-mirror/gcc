------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Aggr; use Exp_Aggr;
with Exp_Atag; use Exp_Atag;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Disp; use Exp_Disp;
with Exp_Fixd; use Exp_Fixd;
with Exp_Intr; use Exp_Intr;
with Exp_Pakd; use Exp_Pakd;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Exp_VFpt; use Exp_VFpt;
with Freeze;   use Freeze;
with Inline;   use Inline;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Par_SCO;  use Par_SCO;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with SCIL_LL;  use SCIL_LL;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Validsw;  use Validsw;

package body Exp_Ch4 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Binary_Op_Validity_Checks (N : Node_Id);
   pragma Inline (Binary_Op_Validity_Checks);
   --  Performs validity checks for a binary operator

   procedure Build_Boolean_Array_Proc_Call
     (N   : Node_Id;
      Op1 : Node_Id;
      Op2 : Node_Id);
   --  If a boolean array assignment can be done in place, build call to
   --  corresponding library procedure.

   function Current_Anonymous_Master return Entity_Id;
   --  Return the entity of the heterogeneous finalization master belonging to
   --  the current unit (either function, package or procedure). This master
   --  services all anonymous access-to-controlled types. If the current unit
   --  does not have such master, create one.

   procedure Displace_Allocator_Pointer (N : Node_Id);
   --  Ada 2005 (AI-251): Subsidiary procedure to Expand_N_Allocator and
   --  Expand_Allocator_Expression. Allocating class-wide interface objects
   --  this routine displaces the pointer to the allocated object to reference
   --  the component referencing the corresponding secondary dispatch table.

   procedure Expand_Allocator_Expression (N : Node_Id);
   --  Subsidiary to Expand_N_Allocator, for the case when the expression
   --  is a qualified expression or an aggregate.

   procedure Expand_Array_Comparison (N : Node_Id);
   --  This routine handles expansion of the comparison operators (N_Op_Lt,
   --  N_Op_Le, N_Op_Gt, N_Op_Ge) when operating on an array type. The basic
   --  code for these operators is similar, differing only in the details of
   --  the actual comparison call that is made. Special processing (call a
   --  run-time routine)

   function Expand_Array_Equality
     (Nod    : Node_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id;
      Typ    : Entity_Id) return Node_Id;
   --  Expand an array equality into a call to a function implementing this
   --  equality, and a call to it. Loc is the location for the generated nodes.
   --  Lhs and Rhs are the array expressions to be compared. Bodies is a list
   --  on which to attach bodies of local functions that are created in the
   --  process. It is the responsibility of the caller to insert those bodies
   --  at the right place. Nod provides the Sloc value for the generated code.
   --  Normally the types used for the generated equality routine are taken
   --  from Lhs and Rhs. However, in some situations of generated code, the
   --  Etype fields of Lhs and Rhs are not set yet. In such cases, Typ supplies
   --  the type to be used for the formal parameters.

   procedure Expand_Boolean_Operator (N : Node_Id);
   --  Common expansion processing for Boolean operators (And, Or, Xor) for the
   --  case of array type arguments.

   procedure Expand_Short_Circuit_Operator (N : Node_Id);
   --  Common expansion processing for short-circuit boolean operators

   procedure Expand_Compare_Minimize_Eliminate_Overflow (N : Node_Id);
   --  Deal with comparison in MINIMIZED/ELIMINATED overflow mode. This is
   --  where we allow comparison of "out of range" values.

   function Expand_Composite_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id) return Node_Id;
   --  Local recursive function used to expand equality for nested composite
   --  types. Used by Expand_Record/Array_Equality, Bodies is a list on which
   --  to attach bodies of local functions that are created in the process.
   --  It is the responsibility of the caller to insert those bodies at the
   --  right place. Nod provides the Sloc value for generated code. Lhs and Rhs
   --  are the left and right sides for the comparison, and Typ is the type of
   --  the objects to compare.

   procedure Expand_Concatenate (Cnode : Node_Id; Opnds : List_Id);
   --  Routine to expand concatenation of a sequence of two or more operands
   --  (in the list Operands) and replace node Cnode with the result of the
   --  concatenation. The operands can be of any appropriate type, and can
   --  include both arrays and singleton elements.

   procedure Expand_Membership_Minimize_Eliminate_Overflow (N : Node_Id);
   --  N is an N_In membership test mode, with the overflow check mode set to
   --  MINIMIZED or ELIMINATED, and the type of the left operand is a signed
   --  integer type. This is a case where top level processing is required to
   --  handle overflow checks in subtrees.

   procedure Fixup_Universal_Fixed_Operation (N : Node_Id);
   --  N is a N_Op_Divide or N_Op_Multiply node whose result is universal
   --  fixed. We do not have such a type at runtime, so the purpose of this
   --  routine is to find the real type by looking up the tree. We also
   --  determine if the operation must be rounded.

   function Has_Inferable_Discriminants (N : Node_Id) return Boolean;
   --  Ada 2005 (AI-216): A view of an Unchecked_Union object has inferable
   --  discriminants if it has a constrained nominal type, unless the object
   --  is a component of an enclosing Unchecked_Union object that is subject
   --  to a per-object constraint and the enclosing object lacks inferable
   --  discriminants.
   --
   --  An expression of an Unchecked_Union type has inferable discriminants
   --  if it is either a name of an object with inferable discriminants or a
   --  qualified expression whose subtype mark denotes a constrained subtype.

   procedure Insert_Dereference_Action (N : Node_Id);
   --  N is an expression whose type is an access. When the type of the
   --  associated storage pool is derived from Checked_Pool, generate a
   --  call to the 'Dereference' primitive operation.

   function Make_Array_Comparison_Op
     (Typ : Entity_Id;
      Nod : Node_Id) return Node_Id;
   --  Comparisons between arrays are expanded in line. This function produces
   --  the body of the implementation of (a > b), where a and b are one-
   --  dimensional arrays of some discrete type. The original node is then
   --  expanded into the appropriate call to this function. Nod provides the
   --  Sloc value for the generated code.

   function Make_Boolean_Array_Op
     (Typ : Entity_Id;
      N   : Node_Id) return Node_Id;
   --  Boolean operations on boolean arrays are expanded in line. This function
   --  produce the body for the node N, which is (a and b), (a or b), or (a xor
   --  b). It is used only the normal case and not the packed case. The type
   --  involved, Typ, is the Boolean array type, and the logical operations in
   --  the body are simple boolean operations. Note that Typ is always a
   --  constrained type (the caller has ensured this by using
   --  Convert_To_Actual_Subtype if necessary).

   function Minimized_Eliminated_Overflow_Check (N : Node_Id) return Boolean;
   --  For signed arithmetic operations when the current overflow mode is
   --  MINIMIZED or ELIMINATED, we must call Apply_Arithmetic_Overflow_Checks
   --  as the first thing we do. We then return. We count on the recursive
   --  apparatus for overflow checks to call us back with an equivalent
   --  operation that is in CHECKED mode, avoiding a recursive entry into this
   --  routine, and that is when we will proceed with the expansion of the
   --  operator (e.g. converting X+0 to X, or X**2 to X*X). We cannot do
   --  these optimizations without first making this check, since there may be
   --  operands further down the tree that are relying on the recursive calls
   --  triggered by the top level nodes to properly process overflow checking
   --  and remaining expansion on these nodes. Note that this call back may be
   --  skipped if the operation is done in Bignum mode but that's fine, since
   --  the Bignum call takes care of everything.

   procedure Optimize_Length_Comparison (N : Node_Id);
   --  Given an expression, if it is of the form X'Length op N (or the other
   --  way round), where N is known at compile time to be 0 or 1, and X is a
   --  simple entity, and op is a comparison operator, optimizes it into a
   --  comparison of First and Last.

   procedure Process_Transient_Object
     (Decl     : Node_Id;
      Rel_Node : Node_Id);
   --  Subsidiary routine to the expansion of expression_with_actions and if
   --  expressions. Generate all the necessary code to finalize a transient
   --  controlled object when the enclosing context is elaborated or evaluated.
   --  Decl denotes the declaration of the transient controlled object which is
   --  usually the result of a controlled function call. Rel_Node denotes the
   --  context, either an expression_with_actions or an if expression.

   procedure Rewrite_Comparison (N : Node_Id);
   --  If N is the node for a comparison whose outcome can be determined at
   --  compile time, then the node N can be rewritten with True or False. If
   --  the outcome cannot be determined at compile time, the call has no
   --  effect. If N is a type conversion, then this processing is applied to
   --  its expression. If N is neither comparison nor a type conversion, the
   --  call has no effect.

   procedure Tagged_Membership
     (N         : Node_Id;
      SCIL_Node : out Node_Id;
      Result    : out Node_Id);
   --  Construct the expression corresponding to the tagged membership test.
   --  Deals with a second operand being (or not) a class-wide type.

   function Safe_In_Place_Array_Op
     (Lhs : Node_Id;
      Op1 : Node_Id;
      Op2 : Node_Id) return Boolean;
   --  In the context of an assignment, where the right-hand side is a boolean
   --  operation on arrays, check whether operation can be performed in place.

   procedure Unary_Op_Validity_Checks (N : Node_Id);
   pragma Inline (Unary_Op_Validity_Checks);
   --  Performs validity checks for a unary operator

   -------------------------------
   -- Binary_Op_Validity_Checks --
   -------------------------------

   procedure Binary_Op_Validity_Checks (N : Node_Id) is
   begin
      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Left_Opnd (N));
         Ensure_Valid (Right_Opnd (N));
      end if;
   end Binary_Op_Validity_Checks;

   ------------------------------------
   -- Build_Boolean_Array_Proc_Call --
   ------------------------------------

   procedure Build_Boolean_Array_Proc_Call
     (N   : Node_Id;
      Op1 : Node_Id;
      Op2 : Node_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Kind      : constant Node_Kind := Nkind (Expression (N));
      Target    : constant Node_Id   :=
                    Make_Attribute_Reference (Loc,
                      Prefix         => Name (N),
                      Attribute_Name => Name_Address);

      Arg1      : Node_Id := Op1;
      Arg2      : Node_Id := Op2;
      Call_Node : Node_Id;
      Proc_Name : Entity_Id;

   begin
      if Kind = N_Op_Not then
         if Nkind (Op1) in N_Binary_Op then

            --  Use negated version of the binary operators

            if Nkind (Op1) = N_Op_And then
               Proc_Name := RTE (RE_Vector_Nand);

            elsif Nkind (Op1) = N_Op_Or then
               Proc_Name := RTE (RE_Vector_Nor);

            else pragma Assert (Nkind (Op1) = N_Op_Xor);
               Proc_Name := RTE (RE_Vector_Xor);
            end if;

            Call_Node :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Proc_Name, Loc),

                Parameter_Associations => New_List (
                  Target,
                  Make_Attribute_Reference (Loc,
                    Prefix => Left_Opnd (Op1),
                    Attribute_Name => Name_Address),

                  Make_Attribute_Reference (Loc,
                    Prefix => Right_Opnd (Op1),
                    Attribute_Name => Name_Address),

                  Make_Attribute_Reference (Loc,
                    Prefix => Left_Opnd (Op1),
                    Attribute_Name => Name_Length)));

         else
            Proc_Name := RTE (RE_Vector_Not);

            Call_Node :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Proc_Name, Loc),
                Parameter_Associations => New_List (
                  Target,

                  Make_Attribute_Reference (Loc,
                    Prefix => Op1,
                    Attribute_Name => Name_Address),

                  Make_Attribute_Reference (Loc,
                    Prefix => Op1,
                     Attribute_Name => Name_Length)));
         end if;

      else
         --  We use the following equivalences:

         --   (not X) or  (not Y)  =  not (X and Y)  =  Nand (X, Y)
         --   (not X) and (not Y)  =  not (X or Y)   =  Nor  (X, Y)
         --   (not X) xor (not Y)  =  X xor Y
         --   X       xor (not Y)  =  not (X xor Y)  =  Nxor (X, Y)

         if Nkind (Op1) = N_Op_Not then
            Arg1 := Right_Opnd (Op1);
            Arg2 := Right_Opnd (Op2);

            if Kind = N_Op_And then
               Proc_Name := RTE (RE_Vector_Nor);
            elsif Kind = N_Op_Or then
               Proc_Name := RTE (RE_Vector_Nand);
            else
               Proc_Name := RTE (RE_Vector_Xor);
            end if;

         else
            if Kind = N_Op_And then
               Proc_Name := RTE (RE_Vector_And);
            elsif Kind = N_Op_Or then
               Proc_Name := RTE (RE_Vector_Or);
            elsif Nkind (Op2) = N_Op_Not then
               Proc_Name := RTE (RE_Vector_Nxor);
               Arg2 := Right_Opnd (Op2);
            else
               Proc_Name := RTE (RE_Vector_Xor);
            end if;
         end if;

         Call_Node :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Proc_Name, Loc),
             Parameter_Associations => New_List (
               Target,
               Make_Attribute_Reference (Loc,
                 Prefix         => Arg1,
                 Attribute_Name => Name_Address),
               Make_Attribute_Reference (Loc,
                 Prefix         => Arg2,
                 Attribute_Name => Name_Address),
               Make_Attribute_Reference (Loc,
                 Prefix         => Arg1,
                 Attribute_Name => Name_Length)));
      end if;

      Rewrite (N, Call_Node);
      Analyze (N);

   exception
      when RE_Not_Available =>
         return;
   end Build_Boolean_Array_Proc_Call;

   ------------------------------
   -- Current_Anonymous_Master --
   ------------------------------

   function Current_Anonymous_Master return Entity_Id is
      Decls     : List_Id;
      Loc       : Source_Ptr;
      Subp_Body : Node_Id;
      Unit_Decl : Node_Id;
      Unit_Id   : Entity_Id;

   begin
      Unit_Id := Cunit_Entity (Current_Sem_Unit);

      --  Find the entity of the current unit

      if Ekind (Unit_Id) = E_Subprogram_Body then

         --  When processing subprogram bodies, the proper scope is always that
         --  of the spec.

         Subp_Body := Unit_Id;
         while Present (Subp_Body)
           and then Nkind (Subp_Body) /= N_Subprogram_Body
         loop
            Subp_Body := Parent (Subp_Body);
         end loop;

         Unit_Id := Corresponding_Spec (Subp_Body);
      end if;

      Loc := Sloc (Unit_Id);
      Unit_Decl := Unit (Cunit (Current_Sem_Unit));

      --  Find the declarations list of the current unit

      if Nkind (Unit_Decl) = N_Package_Declaration then
         Unit_Decl := Specification (Unit_Decl);
         Decls := Visible_Declarations (Unit_Decl);

         if No (Decls) then
            Decls := New_List (Make_Null_Statement (Loc));
            Set_Visible_Declarations (Unit_Decl, Decls);

         elsif Is_Empty_List (Decls) then
            Append_To (Decls, Make_Null_Statement (Loc));
         end if;

      else
         Decls := Declarations (Unit_Decl);

         if No (Decls) then
            Decls := New_List (Make_Null_Statement (Loc));
            Set_Declarations (Unit_Decl, Decls);

         elsif Is_Empty_List (Decls) then
            Append_To (Decls, Make_Null_Statement (Loc));
         end if;
      end if;

      --  The current unit has an existing anonymous master, traverse its
      --  declarations and locate the entity.

      if Has_Anonymous_Master (Unit_Id) then
         declare
            Decl       : Node_Id;
            Fin_Mas_Id : Entity_Id;

         begin
            Decl := First (Decls);
            while Present (Decl) loop

               --  Look for the first variable in the declarations whole type
               --  is Finalization_Master.

               if Nkind (Decl) = N_Object_Declaration then
                  Fin_Mas_Id := Defining_Identifier (Decl);

                  if Ekind (Fin_Mas_Id) = E_Variable
                    and then Etype (Fin_Mas_Id) = RTE (RE_Finalization_Master)
                  then
                     return Fin_Mas_Id;
                  end if;
               end if;

               Next (Decl);
            end loop;

            --  The master was not found even though the unit was labeled as
            --  having one.

            raise Program_Error;
         end;

      --  Create a new anonymous master

      else
         declare
            First_Decl : constant Node_Id := First (Decls);
            Action     : Node_Id;
            Fin_Mas_Id : Entity_Id;

         begin
            --  Since the master and its associated initialization is inserted
            --  at top level, use the scope of the unit when analyzing.

            Push_Scope (Unit_Id);

            --  Create the finalization master

            Fin_Mas_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Unit_Id), "AM"));

            --  Generate:
            --    <Fin_Mas_Id> : Finalization_Master;

            Action :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Fin_Mas_Id,
                Object_Definition =>
                  New_Occurrence_Of (RTE (RE_Finalization_Master), Loc));

            Insert_Before_And_Analyze (First_Decl, Action);

            --  Mark the unit to prevent the generation of multiple masters

            Set_Has_Anonymous_Master (Unit_Id);

            --  Do not set the base pool and mode of operation on .NET/JVM
            --  since those targets do not support pools and all VM masters
            --  are heterogeneous by default.

            if VM_Target = No_VM then

               --  Generate:
               --    Set_Base_Pool
               --      (<Fin_Mas_Id>, Global_Pool_Object'Unrestricted_Access);

               Action :=
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Set_Base_Pool), Loc),

                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Fin_Mas_Id, Loc),
                     Make_Attribute_Reference (Loc,
                       Prefix =>
                         New_Occurrence_Of (RTE (RE_Global_Pool_Object), Loc),
                       Attribute_Name => Name_Unrestricted_Access)));

               Insert_Before_And_Analyze (First_Decl, Action);

               --  Generate:
               --    Set_Is_Heterogeneous (<Fin_Mas_Id>);

               Action :=
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Set_Is_Heterogeneous), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Fin_Mas_Id, Loc)));

               Insert_Before_And_Analyze (First_Decl, Action);
            end if;

            --  Restore the original state of the scope stack

            Pop_Scope;

            return Fin_Mas_Id;
         end;
      end if;
   end Current_Anonymous_Master;

   --------------------------------
   -- Displace_Allocator_Pointer --
   --------------------------------

   procedure Displace_Allocator_Pointer (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Orig_Node : constant Node_Id := Original_Node (N);
      Dtyp      : Entity_Id;
      Etyp      : Entity_Id;
      PtrT      : Entity_Id;

   begin
      --  Do nothing in case of VM targets: the virtual machine will handle
      --  interfaces directly.

      if not Tagged_Type_Expansion then
         return;
      end if;

      pragma Assert (Nkind (N) = N_Identifier
        and then Nkind (Orig_Node) = N_Allocator);

      PtrT := Etype (Orig_Node);
      Dtyp := Available_View (Designated_Type (PtrT));
      Etyp := Etype (Expression (Orig_Node));

      if Is_Class_Wide_Type (Dtyp) and then Is_Interface (Dtyp) then

         --  If the type of the allocator expression is not an interface type
         --  we can generate code to reference the record component containing
         --  the pointer to the secondary dispatch table.

         if not Is_Interface (Etyp) then
            declare
               Saved_Typ : constant Entity_Id := Etype (Orig_Node);

            begin
               --  1) Get access to the allocated object

               Rewrite (N,
                 Make_Explicit_Dereference (Loc, Relocate_Node (N)));
               Set_Etype (N, Etyp);
               Set_Analyzed (N);

               --  2) Add the conversion to displace the pointer to reference
               --     the secondary dispatch table.

               Rewrite (N, Convert_To (Dtyp, Relocate_Node (N)));
               Analyze_And_Resolve (N, Dtyp);

               --  3) The 'access to the secondary dispatch table will be used
               --     as the value returned by the allocator.

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix         => Relocate_Node (N),
                   Attribute_Name => Name_Access));
               Set_Etype (N, Saved_Typ);
               Set_Analyzed (N);
            end;

         --  If the type of the allocator expression is an interface type we
         --  generate a run-time call to displace "this" to reference the
         --  component containing the pointer to the secondary dispatch table
         --  or else raise Constraint_Error if the actual object does not
         --  implement the target interface. This case corresponds to the
         --  following example:

         --   function Op (Obj : Iface_1'Class) return access Iface_2'Class is
         --   begin
         --      return new Iface_2'Class'(Obj);
         --   end Op;

         else
            Rewrite (N,
              Unchecked_Convert_To (PtrT,
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (RTE (RE_Displace), Loc),
                  Parameter_Associations => New_List (
                    Unchecked_Convert_To (RTE (RE_Address),
                      Relocate_Node (N)),

                    New_Occurrence_Of
                      (Elists.Node
                        (First_Elmt
                          (Access_Disp_Table (Etype (Base_Type (Dtyp))))),
                       Loc)))));
            Analyze_And_Resolve (N, PtrT);
         end if;
      end if;
   end Displace_Allocator_Pointer;

   ---------------------------------
   -- Expand_Allocator_Expression --
   ---------------------------------

   procedure Expand_Allocator_Expression (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Exp    : constant Node_Id    := Expression (Expression (N));
      PtrT   : constant Entity_Id  := Etype (N);
      DesigT : constant Entity_Id  := Designated_Type (PtrT);

      procedure Apply_Accessibility_Check
        (Ref            : Node_Id;
         Built_In_Place : Boolean := False);
      --  Ada 2005 (AI-344): For an allocator with a class-wide designated
      --  type, generate an accessibility check to verify that the level of the
      --  type of the created object is not deeper than the level of the access
      --  type. If the type of the qualified expression is class-wide, then
      --  always generate the check (except in the case where it is known to be
      --  unnecessary, see comment below). Otherwise, only generate the check
      --  if the level of the qualified expression type is statically deeper
      --  than the access type.
      --
      --  Although the static accessibility will generally have been performed
      --  as a legality check, it won't have been done in cases where the
      --  allocator appears in generic body, so a run-time check is needed in
      --  general. One special case is when the access type is declared in the
      --  same scope as the class-wide allocator, in which case the check can
      --  never fail, so it need not be generated.
      --
      --  As an open issue, there seem to be cases where the static level
      --  associated with the class-wide object's underlying type is not
      --  sufficient to perform the proper accessibility check, such as for
      --  allocators in nested subprograms or accept statements initialized by
      --  class-wide formals when the actual originates outside at a deeper
      --  static level. The nested subprogram case might require passing
      --  accessibility levels along with class-wide parameters, and the task
      --  case seems to be an actual gap in the language rules that needs to
      --  be fixed by the ARG. ???

      -------------------------------
      -- Apply_Accessibility_Check --
      -------------------------------

      procedure Apply_Accessibility_Check
        (Ref            : Node_Id;
         Built_In_Place : Boolean := False)
      is
         Pool_Id   : constant Entity_Id := Associated_Storage_Pool (PtrT);
         Cond      : Node_Id;
         Fin_Call  : Node_Id;
         Free_Stmt : Node_Id;
         Obj_Ref   : Node_Id;
         Stmts     : List_Id;

      begin
         if Ada_Version >= Ada_2005
           and then Is_Class_Wide_Type (DesigT)
           and then (Tagged_Type_Expansion or else VM_Target /= No_VM)
           and then not Scope_Suppress.Suppress (Accessibility_Check)
           and then
             (Type_Access_Level (Etype (Exp)) > Type_Access_Level (PtrT)
               or else
                 (Is_Class_Wide_Type (Etype (Exp))
                   and then Scope (PtrT) /= Current_Scope))
         then
            --  If the allocator was built in place, Ref is already a reference
            --  to the access object initialized to the result of the allocator
            --  (see Exp_Ch6.Make_Build_In_Place_Call_In_Allocator). We call
            --  Remove_Side_Effects for cases where the build-in-place call may
            --  still be the prefix of the reference (to avoid generating
            --  duplicate calls). Otherwise, it is the entity associated with
            --  the object containing the address of the allocated object.

            if Built_In_Place then
               Remove_Side_Effects (Ref);
               Obj_Ref := New_Copy_Tree (Ref);
            else
               Obj_Ref := New_Occurrence_Of (Ref, Loc);
            end if;

            --  Step 1: Create the object clean up code

            Stmts := New_List;

            --  Deallocate the object if the accessibility check fails. This
            --  is done only on targets or profiles that support deallocation.

            --    Free (Obj_Ref);

            if RTE_Available (RE_Free) then
               Free_Stmt := Make_Free_Statement (Loc, New_Copy_Tree (Obj_Ref));
               Set_Storage_Pool (Free_Stmt, Pool_Id);

               Append_To (Stmts, Free_Stmt);

            --  The target or profile cannot deallocate objects

            else
               Free_Stmt := Empty;
            end if;

            --  Finalize the object if applicable. Generate:

            --    [Deep_]Finalize (Obj_Ref.all);

            if Needs_Finalization (DesigT) then
               Fin_Call :=
                 Make_Final_Call (
                   Obj_Ref =>
                     Make_Explicit_Dereference (Loc, New_Copy (Obj_Ref)),
                   Typ     => DesigT);

               --  When the target or profile supports deallocation, wrap the
               --  finalization call in a block to ensure proper deallocation
               --  even if finalization fails. Generate:

               --    begin
               --       <Fin_Call>
               --    exception
               --       when others =>
               --          <Free_Stmt>
               --          raise;
               --    end;

               if Present (Free_Stmt) then
                  Fin_Call :=
                    Make_Block_Statement (Loc,
                      Handled_Statement_Sequence =>
                        Make_Handled_Sequence_Of_Statements (Loc,
                          Statements => New_List (Fin_Call),

                        Exception_Handlers => New_List (
                          Make_Exception_Handler (Loc,
                            Exception_Choices => New_List (
                              Make_Others_Choice (Loc)),

                            Statements        => New_List (
                              New_Copy_Tree (Free_Stmt),
                              Make_Raise_Statement (Loc))))));
               end if;

               Prepend_To (Stmts, Fin_Call);
            end if;

            --  Signal the accessibility failure through a Program_Error

            Append_To (Stmts,
              Make_Raise_Program_Error (Loc,
                Condition => New_Occurrence_Of (Standard_True, Loc),
                Reason    => PE_Accessibility_Check_Failed));

            --  Step 2: Create the accessibility comparison

            --  Generate:
            --    Ref'Tag

            Obj_Ref :=
              Make_Attribute_Reference (Loc,
                Prefix         => Obj_Ref,
                Attribute_Name => Name_Tag);

            --  For tagged types, determine the accessibility level by looking
            --  at the type specific data of the dispatch table. Generate:

            --    Type_Specific_Data (Address (Ref'Tag)).Access_Level

            if Tagged_Type_Expansion then
               Cond := Build_Get_Access_Level (Loc, Obj_Ref);

            --  Use a runtime call to determine the accessibility level when
            --  compiling on virtual machine targets. Generate:

            --    Get_Access_Level (Ref'Tag)

            else
               Cond :=
                 Make_Function_Call (Loc,
                   Name                   =>
                     New_Occurrence_Of (RTE (RE_Get_Access_Level), Loc),
                   Parameter_Associations => New_List (Obj_Ref));
            end if;

            Cond :=
              Make_Op_Gt (Loc,
                Left_Opnd  => Cond,
                Right_Opnd =>
                  Make_Integer_Literal (Loc, Type_Access_Level (PtrT)));

            --  Due to the complexity and side effects of the check, utilize an
            --  if statement instead of the regular Program_Error circuitry.

            Insert_Action (N,
              Make_Implicit_If_Statement (N,
                Condition       => Cond,
                Then_Statements => Stmts));
         end if;
      end Apply_Accessibility_Check;

      --  Local variables

      Aggr_In_Place : constant Boolean   := Is_Delayed_Aggregate (Exp);
      Indic         : constant Node_Id   := Subtype_Mark (Expression (N));
      T             : constant Entity_Id := Entity (Indic);
      Node          : Node_Id;
      Tag_Assign    : Node_Id;
      Temp          : Entity_Id;
      Temp_Decl     : Node_Id;

      TagT : Entity_Id := Empty;
      --  Type used as source for tag assignment

      TagR : Node_Id := Empty;
      --  Target reference for tag assignment

   --  Start of processing for Expand_Allocator_Expression

   begin
      --  Handle call to C++ constructor

      if Is_CPP_Constructor_Call (Exp) then
         Make_CPP_Constructor_Call_In_Allocator
           (Allocator => N,
            Function_Call => Exp);
         return;
      end if;

      --  In the case of an Ada 2012 allocator whose initial value comes from a
      --  function call, pass "the accessibility level determined by the point
      --  of call" (AI05-0234) to the function. Conceptually, this belongs in
      --  Expand_Call but it couldn't be done there (because the Etype of the
      --  allocator wasn't set then) so we generate the parameter here. See
      --  the Boolean variable Defer in (a block within) Expand_Call.

      if Ada_Version >= Ada_2012 and then Nkind (Exp) = N_Function_Call then
         declare
            Subp : Entity_Id;

         begin
            if Nkind (Name (Exp)) = N_Explicit_Dereference then
               Subp := Designated_Type (Etype (Prefix (Name (Exp))));
            else
               Subp := Entity (Name (Exp));
            end if;

            Subp := Ultimate_Alias (Subp);

            if Present (Extra_Accessibility_Of_Result (Subp)) then
               Add_Extra_Actual_To_Call
                 (Subprogram_Call => Exp,
                  Extra_Formal    => Extra_Accessibility_Of_Result (Subp),
                  Extra_Actual    => Dynamic_Accessibility_Level (PtrT));
            end if;
         end;
      end if;

      --  Case of tagged type or type requiring finalization

      if Is_Tagged_Type (T) or else Needs_Finalization (T) then

         --  Ada 2005 (AI-318-02): If the initialization expression is a call
         --  to a build-in-place function, then access to the allocated object
         --  must be passed to the function. Currently we limit such functions
         --  to those with constrained limited result subtypes, but eventually
         --  we plan to expand the allowed forms of functions that are treated
         --  as build-in-place.

         if Ada_Version >= Ada_2005
           and then Is_Build_In_Place_Function_Call (Exp)
         then
            Make_Build_In_Place_Call_In_Allocator (N, Exp);
            Apply_Accessibility_Check (N, Built_In_Place => True);
            return;
         end if;

         --  Actions inserted before:
         --    Temp : constant ptr_T := new T'(Expression);
         --    Temp._tag = T'tag;  --  when not class-wide
         --    [Deep_]Adjust (Temp.all);

         --  We analyze by hand the new internal allocator to avoid any
         --  recursion and inappropriate call to Initialize.

         --  We don't want to remove side effects when the expression must be
         --  built in place. In the case of a build-in-place function call,
         --  that could lead to a duplication of the call, which was already
         --  substituted for the allocator.

         if not Aggr_In_Place then
            Remove_Side_Effects (Exp);
         end if;

         Temp := Make_Temporary (Loc, 'P', N);

         --  For a class wide allocation generate the following code:

         --    type Equiv_Record is record ... end record;
         --    implicit subtype CW is <Class_Wide_Subytpe>;
         --    temp : PtrT := new CW'(CW!(expr));

         if Is_Class_Wide_Type (T) then
            Expand_Subtype_From_Expr (Empty, T, Indic, Exp);

            --  Ada 2005 (AI-251): If the expression is a class-wide interface
            --  object we generate code to move up "this" to reference the
            --  base of the object before allocating the new object.

            --  Note that Exp'Address is recursively expanded into a call
            --  to Base_Address (Exp.Tag)

            if Is_Class_Wide_Type (Etype (Exp))
              and then Is_Interface (Etype (Exp))
              and then Tagged_Type_Expansion
            then
               Set_Expression
                 (Expression (N),
                  Unchecked_Convert_To (Entity (Indic),
                    Make_Explicit_Dereference (Loc,
                      Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                        Make_Attribute_Reference (Loc,
                          Prefix         => Exp,
                          Attribute_Name => Name_Address)))));
            else
               Set_Expression
                 (Expression (N),
                  Unchecked_Convert_To (Entity (Indic), Exp));
            end if;

            Analyze_And_Resolve (Expression (N), Entity (Indic));
         end if;

         --  Processing for allocators returning non-interface types

         if not Is_Interface (Directly_Designated_Type (PtrT)) then
            if Aggr_In_Place then
               Temp_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Occurrence_Of (PtrT, Loc),
                   Expression          =>
                     Make_Allocator (Loc,
                       Expression =>
                         New_Occurrence_Of (Etype (Exp), Loc)));

               --  Copy the Comes_From_Source flag for the allocator we just
               --  built, since logically this allocator is a replacement of
               --  the original allocator node. This is for proper handling of
               --  restriction No_Implicit_Heap_Allocations.

               Set_Comes_From_Source
                 (Expression (Temp_Decl), Comes_From_Source (N));

               Set_No_Initialization (Expression (Temp_Decl));
               Insert_Action (N, Temp_Decl);

               Build_Allocate_Deallocate_Proc (Temp_Decl, True);
               Convert_Aggr_In_Allocator (N, Temp_Decl, Exp);

               --  Attach the object to the associated finalization master.
               --  This is done manually on .NET/JVM since those compilers do
               --  no support pools and can't benefit from internally generated
               --  Allocate / Deallocate procedures.

               if VM_Target /= No_VM
                 and then Is_Controlled (DesigT)
                 and then Present (Finalization_Master (PtrT))
               then
                  Insert_Action (N,
                    Make_Attach_Call (
                      Obj_Ref => New_Occurrence_Of (Temp, Loc),
                      Ptr_Typ => PtrT));
               end if;

            else
               Node := Relocate_Node (N);
               Set_Analyzed (Node);

               Temp_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (PtrT, Loc),
                   Expression          => Node);

               Insert_Action (N, Temp_Decl);
               Build_Allocate_Deallocate_Proc (Temp_Decl, True);

               --  Attach the object to the associated finalization master.
               --  This is done manually on .NET/JVM since those compilers do
               --  no support pools and can't benefit from internally generated
               --  Allocate / Deallocate procedures.

               if VM_Target /= No_VM
                 and then Is_Controlled (DesigT)
                 and then Present (Finalization_Master (PtrT))
               then
                  Insert_Action (N,
                    Make_Attach_Call (
                      Obj_Ref =>
                        New_Occurrence_Of (Temp, Loc),
                      Ptr_Typ => PtrT));
               end if;
            end if;

         --  Ada 2005 (AI-251): Handle allocators whose designated type is an
         --  interface type. In this case we use the type of the qualified
         --  expression to allocate the object.

         else
            declare
               Def_Id   : constant Entity_Id := Make_Temporary (Loc, 'T');
               New_Decl : Node_Id;

            begin
               New_Decl :=
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Def_Id,
                   Type_Definition =>
                     Make_Access_To_Object_Definition (Loc,
                       All_Present            => True,
                       Null_Exclusion_Present => False,
                       Constant_Present       =>
                         Is_Access_Constant (Etype (N)),
                       Subtype_Indication     =>
                         New_Occurrence_Of (Etype (Exp), Loc)));

               Insert_Action (N, New_Decl);

               --  Inherit the allocation-related attributes from the original
               --  access type.

               Set_Finalization_Master (Def_Id, Finalization_Master (PtrT));

               Set_Associated_Storage_Pool (Def_Id,
                 Associated_Storage_Pool (PtrT));

               --  Declare the object using the previous type declaration

               if Aggr_In_Place then
                  Temp_Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Temp,
                      Object_Definition   => New_Occurrence_Of (Def_Id, Loc),
                      Expression          =>
                        Make_Allocator (Loc,
                          New_Occurrence_Of (Etype (Exp), Loc)));

                  --  Copy the Comes_From_Source flag for the allocator we just
                  --  built, since logically this allocator is a replacement of
                  --  the original allocator node. This is for proper handling
                  --  of restriction No_Implicit_Heap_Allocations.

                  Set_Comes_From_Source
                    (Expression (Temp_Decl), Comes_From_Source (N));

                  Set_No_Initialization (Expression (Temp_Decl));
                  Insert_Action (N, Temp_Decl);

                  Build_Allocate_Deallocate_Proc (Temp_Decl, True);
                  Convert_Aggr_In_Allocator (N, Temp_Decl, Exp);

               else
                  Node := Relocate_Node (N);
                  Set_Analyzed (Node);

                  Temp_Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Temp,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (Def_Id, Loc),
                      Expression          => Node);

                  Insert_Action (N, Temp_Decl);
                  Build_Allocate_Deallocate_Proc (Temp_Decl, True);
               end if;

               --  Generate an additional object containing the address of the
               --  returned object. The type of this second object declaration
               --  is the correct type required for the common processing that
               --  is still performed by this subprogram. The displacement of
               --  this pointer to reference the component associated with the
               --  interface type will be done at the end of common processing.

               New_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Make_Temporary (Loc, 'P'),
                   Object_Definition   => New_Occurrence_Of (PtrT, Loc),
                   Expression          =>
                     Unchecked_Convert_To (PtrT,
                       New_Occurrence_Of (Temp, Loc)));

               Insert_Action (N, New_Decl);

               Temp_Decl := New_Decl;
               Temp      := Defining_Identifier (New_Decl);
            end;
         end if;

         Apply_Accessibility_Check (Temp);

         --  Generate the tag assignment

         --  Suppress the tag assignment when VM_Target because VM tags are
         --  represented implicitly in objects.

         if not Tagged_Type_Expansion then
            null;

         --  Ada 2005 (AI-251): Suppress the tag assignment with class-wide
         --  interface objects because in this case the tag does not change.

         elsif Is_Interface (Directly_Designated_Type (Etype (N))) then
            pragma Assert (Is_Class_Wide_Type
                            (Directly_Designated_Type (Etype (N))));
            null;

         elsif Is_Tagged_Type (T) and then not Is_Class_Wide_Type (T) then
            TagT := T;
            TagR := New_Occurrence_Of (Temp, Loc);

         elsif Is_Private_Type (T)
           and then Is_Tagged_Type (Underlying_Type (T))
         then
            TagT := Underlying_Type (T);
            TagR :=
              Unchecked_Convert_To (Underlying_Type (T),
                Make_Explicit_Dereference (Loc,
                  Prefix => New_Occurrence_Of (Temp, Loc)));
         end if;

         if Present (TagT) then
            declare
               Full_T : constant Entity_Id := Underlying_Type (TagT);

            begin
               Tag_Assign :=
                 Make_Assignment_Statement (Loc,
                   Name =>
                     Make_Selected_Component (Loc,
                       Prefix => TagR,
                       Selector_Name =>
                         New_Occurrence_Of
                           (First_Tag_Component (Full_T), Loc)),

                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Occurrence_Of
                         (Elists.Node
                           (First_Elmt (Access_Disp_Table (Full_T))), Loc)));
            end;

            --  The previous assignment has to be done in any case

            Set_Assignment_OK (Name (Tag_Assign));
            Insert_Action (N, Tag_Assign);
         end if;

         if Needs_Finalization (DesigT) and then Needs_Finalization (T) then

            --  Generate an Adjust call if the object will be moved. In Ada
            --  2005, the object may be inherently limited, in which case
            --  there is no Adjust procedure, and the object is built in
            --  place. In Ada 95, the object can be limited but not
            --  inherently limited if this allocator came from a return
            --  statement (we're allocating the result on the secondary
            --  stack). In that case, the object will be moved, so we _do_
            --  want to Adjust.

            if not Aggr_In_Place
              and then not Is_Limited_View (T)
            then
               Insert_Action (N,

                 --  An unchecked conversion is needed in the classwide case
                 --  because the designated type can be an ancestor of the
                 --  subtype mark of the allocator.

                 Make_Adjust_Call
                   (Obj_Ref =>
                      Unchecked_Convert_To (T,
                        Make_Explicit_Dereference (Loc,
                          Prefix => New_Occurrence_Of (Temp, Loc))),
                    Typ     => T));
            end if;

            --  Generate:
            --    Set_Finalize_Address (<PtrT>FM, <T>FD'Unrestricted_Access);

            --  Do not generate this call in the following cases:

            --    * .NET/JVM - these targets do not support address arithmetic
            --    and unchecked conversion, key elements of Finalize_Address.

            --    * CodePeer mode - TSS primitive Finalize_Address is not
            --    created in this mode.

            if VM_Target = No_VM
              and then not CodePeer_Mode
              and then Present (Finalization_Master (PtrT))
              and then Present (Temp_Decl)
              and then Nkind (Expression (Temp_Decl)) = N_Allocator
            then
               Insert_Action (N,
                 Make_Set_Finalize_Address_Call
                   (Loc     => Loc,
                    Typ     => T,
                    Ptr_Typ => PtrT));
            end if;
         end if;

         Rewrite (N, New_Occurrence_Of (Temp, Loc));
         Analyze_And_Resolve (N, PtrT);

         --  Ada 2005 (AI-251): Displace the pointer to reference the record
         --  component containing the secondary dispatch table of the interface
         --  type.

         if Is_Interface (Directly_Designated_Type (PtrT)) then
            Displace_Allocator_Pointer (N);
         end if;

      elsif Aggr_In_Place then
         Temp := Make_Temporary (Loc, 'P', N);
         Temp_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => New_Occurrence_Of (PtrT, Loc),
             Expression          =>
               Make_Allocator (Loc,
                 Expression => New_Occurrence_Of (Etype (Exp), Loc)));

         --  Copy the Comes_From_Source flag for the allocator we just built,
         --  since logically this allocator is a replacement of the original
         --  allocator node. This is for proper handling of restriction
         --  No_Implicit_Heap_Allocations.

         Set_Comes_From_Source
           (Expression (Temp_Decl), Comes_From_Source (N));

         Set_No_Initialization (Expression (Temp_Decl));
         Insert_Action (N, Temp_Decl);

         Build_Allocate_Deallocate_Proc (Temp_Decl, True);
         Convert_Aggr_In_Allocator (N, Temp_Decl, Exp);

         --  Attach the object to the associated finalization master. Thisis
         --  done manually on .NET/JVM since those compilers do no support
         --  pools and cannot benefit from internally generated Allocate and
         --  Deallocate procedures.

         if VM_Target /= No_VM
           and then Is_Controlled (DesigT)
           and then Present (Finalization_Master (PtrT))
         then
            Insert_Action (N,
              Make_Attach_Call
                (Obj_Ref => New_Occurrence_Of (Temp, Loc),
                 Ptr_Typ => PtrT));
         end if;

         Rewrite (N, New_Occurrence_Of (Temp, Loc));
         Analyze_And_Resolve (N, PtrT);

      elsif Is_Access_Type (T) and then Can_Never_Be_Null (T) then
         Install_Null_Excluding_Check (Exp);

      elsif Is_Access_Type (DesigT)
        and then Nkind (Exp) = N_Allocator
        and then Nkind (Expression (Exp)) /= N_Qualified_Expression
      then
         --  Apply constraint to designated subtype indication

         Apply_Constraint_Check (Expression (Exp),
           Designated_Type (DesigT),
           No_Sliding => True);

         if Nkind (Expression (Exp)) = N_Raise_Constraint_Error then

            --  Propagate constraint_error to enclosing allocator

            Rewrite (Exp, New_Copy (Expression (Exp)));
         end if;

      else
         Build_Allocate_Deallocate_Proc (N, True);

         --  If we have:
         --    type A is access T1;
         --    X : A := new T2'(...);
         --  T1 and T2 can be different subtypes, and we might need to check
         --  both constraints. First check against the type of the qualified
         --  expression.

         Apply_Constraint_Check (Exp, T, No_Sliding => True);

         if Do_Range_Check (Exp) then
            Set_Do_Range_Check (Exp, False);
            Generate_Range_Check (Exp, DesigT, CE_Range_Check_Failed);
         end if;

         --  A check is also needed in cases where the designated subtype is
         --  constrained and differs from the subtype given in the qualified
         --  expression. Note that the check on the qualified expression does
         --  not allow sliding, but this check does (a relaxation from Ada 83).

         if Is_Constrained (DesigT)
           and then not Subtypes_Statically_Match (T, DesigT)
         then
            Apply_Constraint_Check
              (Exp, DesigT, No_Sliding => False);

            if Do_Range_Check (Exp) then
               Set_Do_Range_Check (Exp, False);
               Generate_Range_Check (Exp, DesigT, CE_Range_Check_Failed);
            end if;
         end if;

         --  For an access to unconstrained packed array, GIGI needs to see an
         --  expression with a constrained subtype in order to compute the
         --  proper size for the allocator.

         if Is_Array_Type (T)
           and then not Is_Constrained (T)
           and then Is_Packed (T)
         then
            declare
               ConstrT      : constant Entity_Id := Make_Temporary (Loc, 'A');
               Internal_Exp : constant Node_Id   := Relocate_Node (Exp);
            begin
               Insert_Action (Exp,
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier => ConstrT,
                   Subtype_Indication  =>
                     Make_Subtype_From_Expr (Internal_Exp, T)));
               Freeze_Itype (ConstrT, Exp);
               Rewrite (Exp, OK_Convert_To (ConstrT, Internal_Exp));
            end;
         end if;

         --  Ada 2005 (AI-318-02): If the initialization expression is a call
         --  to a build-in-place function, then access to the allocated object
         --  must be passed to the function. Currently we limit such functions
         --  to those with constrained limited result subtypes, but eventually
         --  we plan to expand the allowed forms of functions that are treated
         --  as build-in-place.

         if Ada_Version >= Ada_2005
           and then Is_Build_In_Place_Function_Call (Exp)
         then
            Make_Build_In_Place_Call_In_Allocator (N, Exp);
         end if;
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Allocator_Expression;

   -----------------------------
   -- Expand_Array_Comparison --
   -----------------------------

   --  Expansion is only required in the case of array types. For the unpacked
   --  case, an appropriate runtime routine is called. For packed cases, and
   --  also in some other cases where a runtime routine cannot be called, the
   --  form of the expansion is:

   --     [body for greater_nn; boolean_expression]

   --  The body is built by Make_Array_Comparison_Op, and the form of the
   --  Boolean expression depends on the operator involved.

   procedure Expand_Array_Comparison (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Op1  : Node_Id             := Left_Opnd (N);
      Op2  : Node_Id             := Right_Opnd (N);
      Typ1 : constant Entity_Id  := Base_Type (Etype (Op1));
      Ctyp : constant Entity_Id  := Component_Type (Typ1);

      Expr      : Node_Id;
      Func_Body : Node_Id;
      Func_Name : Entity_Id;

      Comp : RE_Id;

      Byte_Addressable : constant Boolean := System_Storage_Unit = Byte'Size;
      --  True for byte addressable target

      function Length_Less_Than_4 (Opnd : Node_Id) return Boolean;
      --  Returns True if the length of the given operand is known to be less
      --  than 4. Returns False if this length is known to be four or greater
      --  or is not known at compile time.

      ------------------------
      -- Length_Less_Than_4 --
      ------------------------

      function Length_Less_Than_4 (Opnd : Node_Id) return Boolean is
         Otyp : constant Entity_Id := Etype (Opnd);

      begin
         if Ekind (Otyp) = E_String_Literal_Subtype then
            return String_Literal_Length (Otyp) < 4;

         else
            declare
               Ityp : constant Entity_Id := Etype (First_Index (Otyp));
               Lo   : constant Node_Id   := Type_Low_Bound (Ityp);
               Hi   : constant Node_Id   := Type_High_Bound (Ityp);
               Lov  : Uint;
               Hiv  : Uint;

            begin
               if Compile_Time_Known_Value (Lo) then
                  Lov := Expr_Value (Lo);
               else
                  return False;
               end if;

               if Compile_Time_Known_Value (Hi) then
                  Hiv := Expr_Value (Hi);
               else
                  return False;
               end if;

               return Hiv < Lov + 3;
            end;
         end if;
      end Length_Less_Than_4;

   --  Start of processing for Expand_Array_Comparison

   begin
      --  Deal first with unpacked case, where we can call a runtime routine
      --  except that we avoid this for targets for which are not addressable
      --  by bytes, and for the JVM/CIL, since they do not support direct
      --  addressing of array components.

      if not Is_Bit_Packed_Array (Typ1)
        and then Byte_Addressable
        and then VM_Target = No_VM
      then
         --  The call we generate is:

         --  Compare_Array_xn[_Unaligned]
         --    (left'address, right'address, left'length, right'length) <op> 0

         --  x = U for unsigned, S for signed
         --  n = 8,16,32,64 for component size
         --  Add _Unaligned if length < 4 and component size is 8.
         --  <op> is the standard comparison operator

         if Component_Size (Typ1) = 8 then
            if Length_Less_Than_4 (Op1)
                 or else
               Length_Less_Than_4 (Op2)
            then
               if Is_Unsigned_Type (Ctyp) then
                  Comp := RE_Compare_Array_U8_Unaligned;
               else
                  Comp := RE_Compare_Array_S8_Unaligned;
               end if;

            else
               if Is_Unsigned_Type (Ctyp) then
                  Comp := RE_Compare_Array_U8;
               else
                  Comp := RE_Compare_Array_S8;
               end if;
            end if;

         elsif Component_Size (Typ1) = 16 then
            if Is_Unsigned_Type (Ctyp) then
               Comp := RE_Compare_Array_U16;
            else
               Comp := RE_Compare_Array_S16;
            end if;

         elsif Component_Size (Typ1) = 32 then
            if Is_Unsigned_Type (Ctyp) then
               Comp := RE_Compare_Array_U32;
            else
               Comp := RE_Compare_Array_S32;
            end if;

         else pragma Assert (Component_Size (Typ1) = 64);
            if Is_Unsigned_Type (Ctyp) then
               Comp := RE_Compare_Array_U64;
            else
               Comp := RE_Compare_Array_S64;
            end if;
         end if;

         Remove_Side_Effects (Op1, Name_Req => True);
         Remove_Side_Effects (Op2, Name_Req => True);

         Rewrite (Op1,
           Make_Function_Call (Sloc (Op1),
             Name => New_Occurrence_Of (RTE (Comp), Loc),

             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         => Relocate_Node (Op1),
                 Attribute_Name => Name_Address),

               Make_Attribute_Reference (Loc,
                 Prefix         => Relocate_Node (Op2),
                 Attribute_Name => Name_Address),

               Make_Attribute_Reference (Loc,
                 Prefix         => Relocate_Node (Op1),
                 Attribute_Name => Name_Length),

               Make_Attribute_Reference (Loc,
                 Prefix         => Relocate_Node (Op2),
                 Attribute_Name => Name_Length))));

         Rewrite (Op2,
           Make_Integer_Literal (Sloc (Op2),
             Intval => Uint_0));

         Analyze_And_Resolve (Op1, Standard_Integer);
         Analyze_And_Resolve (Op2, Standard_Integer);
         return;
      end if;

      --  Cases where we cannot make runtime call

      --  For (a <= b) we convert to not (a > b)

      if Chars (N) = Name_Op_Le then
         Rewrite (N,
           Make_Op_Not (Loc,
             Right_Opnd =>
                Make_Op_Gt (Loc,
                 Left_Opnd  => Op1,
                 Right_Opnd => Op2)));
         Analyze_And_Resolve (N, Standard_Boolean);
         return;

      --  For < the Boolean expression is
      --    greater__nn (op2, op1)

      elsif Chars (N) = Name_Op_Lt then
         Func_Body := Make_Array_Comparison_Op (Typ1, N);

         --  Switch operands

         Op1 := Right_Opnd (N);
         Op2 := Left_Opnd  (N);

      --  For (a >= b) we convert to not (a < b)

      elsif Chars (N) = Name_Op_Ge then
         Rewrite (N,
           Make_Op_Not (Loc,
             Right_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd  => Op1,
                 Right_Opnd => Op2)));
         Analyze_And_Resolve (N, Standard_Boolean);
         return;

      --  For > the Boolean expression is
      --    greater__nn (op1, op2)

      else
         pragma Assert (Chars (N) = Name_Op_Gt);
         Func_Body := Make_Array_Comparison_Op (Typ1, N);
      end if;

      Func_Name := Defining_Unit_Name (Specification (Func_Body));
      Expr :=
        Make_Function_Call (Loc,
          Name => New_Occurrence_Of (Func_Name, Loc),
          Parameter_Associations => New_List (Op1, Op2));

      Insert_Action (N, Func_Body);
      Rewrite (N, Expr);
      Analyze_And_Resolve (N, Standard_Boolean);

   exception
      when RE_Not_Available =>
         return;
   end Expand_Array_Comparison;

   ---------------------------
   -- Expand_Array_Equality --
   ---------------------------

   --  Expand an equality function for multi-dimensional arrays. Here is an
   --  example of such a function for Nb_Dimension = 2

   --  function Enn (A : atyp; B : btyp) return boolean is
   --  begin
   --     if (A'length (1) = 0 or else A'length (2) = 0)
   --          and then
   --        (B'length (1) = 0 or else B'length (2) = 0)
   --     then
   --        return True;    -- RM 4.5.2(22)
   --     end if;

   --     if A'length (1) /= B'length (1)
   --               or else
   --           A'length (2) /= B'length (2)
   --     then
   --        return False;   -- RM 4.5.2(23)
   --     end if;

   --     declare
   --        A1 : Index_T1 := A'first (1);
   --        B1 : Index_T1 := B'first (1);
   --     begin
   --        loop
   --           declare
   --              A2 : Index_T2 := A'first (2);
   --              B2 : Index_T2 := B'first (2);
   --           begin
   --              loop
   --                 if A (A1, A2) /= B (B1, B2) then
   --                    return False;
   --                 end if;

   --                 exit when A2 = A'last (2);
   --                 A2 := Index_T2'succ (A2);
   --                 B2 := Index_T2'succ (B2);
   --              end loop;
   --           end;

   --           exit when A1 = A'last (1);
   --           A1 := Index_T1'succ (A1);
   --           B1 := Index_T1'succ (B1);
   --        end loop;
   --     end;

   --     return true;
   --  end Enn;

   --  Note on the formal types used (atyp and btyp). If either of the arrays
   --  is of a private type, we use the underlying type, and do an unchecked
   --  conversion of the actual. If either of the arrays has a bound depending
   --  on a discriminant, then we use the base type since otherwise we have an
   --  escaped discriminant in the function.

   --  If both arrays are constrained and have the same bounds, we can generate
   --  a loop with an explicit iteration scheme using a 'Range attribute over
   --  the first array.

   function Expand_Array_Equality
     (Nod    : Node_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id;
      Typ    : Entity_Id) return Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (Nod);
      Decls       : constant List_Id    := New_List;
      Index_List1 : constant List_Id    := New_List;
      Index_List2 : constant List_Id    := New_List;

      Actuals   : List_Id;
      Formals   : List_Id;
      Func_Name : Entity_Id;
      Func_Body : Node_Id;

      A : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uA);
      B : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uB);

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;
      --  The parameter types to be used for the formals

      function Arr_Attr
        (Arr : Entity_Id;
         Nam : Name_Id;
         Num : Int) return Node_Id;
      --  This builds the attribute reference Arr'Nam (Expr)

      function Component_Equality (Typ : Entity_Id) return Node_Id;
      --  Create one statement to compare corresponding components, designated
      --  by a full set of indexes.

      function Get_Arg_Type (N : Node_Id) return Entity_Id;
      --  Given one of the arguments, computes the appropriate type to be used
      --  for that argument in the corresponding function formal

      function Handle_One_Dimension
        (N     : Int;
         Index : Node_Id) return Node_Id;
      --  This procedure returns the following code
      --
      --    declare
      --       Bn : Index_T := B'First (N);
      --    begin
      --       loop
      --          xxx
      --          exit when An = A'Last (N);
      --          An := Index_T'Succ (An)
      --          Bn := Index_T'Succ (Bn)
      --       end loop;
      --    end;
      --
      --  If both indexes are constrained and identical, the procedure
      --  returns a simpler loop:
      --
      --      for An in A'Range (N) loop
      --         xxx
      --      end loop
      --
      --  N is the dimension for which we are generating a loop. Index is the
      --  N'th index node, whose Etype is Index_Type_n in the above code. The
      --  xxx statement is either the loop or declare for the next dimension
      --  or if this is the last dimension the comparison of corresponding
      --  components of the arrays.
      --
      --  The actual way the code works is to return the comparison of
      --  corresponding components for the N+1 call. That's neater.

      function Test_Empty_Arrays return Node_Id;
      --  This function constructs the test for both arrays being empty
      --    (A'length (1) = 0 or else A'length (2) = 0 or else ...)
      --      and then
      --    (B'length (1) = 0 or else B'length (2) = 0 or else ...)

      function Test_Lengths_Correspond return Node_Id;
      --  This function constructs the test for arrays having different lengths
      --  in at least one index position, in which case the resulting code is:

      --     A'length (1) /= B'length (1)
      --       or else
      --     A'length (2) /= B'length (2)
      --       or else
      --       ...

      --------------
      -- Arr_Attr --
      --------------

      function Arr_Attr
        (Arr : Entity_Id;
         Nam : Name_Id;
         Num : Int) return Node_Id
      is
      begin
         return
           Make_Attribute_Reference (Loc,
            Attribute_Name => Nam,
            Prefix => New_Occurrence_Of (Arr, Loc),
            Expressions => New_List (Make_Integer_Literal (Loc, Num)));
      end Arr_Attr;

      ------------------------
      -- Component_Equality --
      ------------------------

      function Component_Equality (Typ : Entity_Id) return Node_Id is
         Test : Node_Id;
         L, R : Node_Id;

      begin
         --  if a(i1...) /= b(j1...) then return false; end if;

         L :=
           Make_Indexed_Component (Loc,
             Prefix      => Make_Identifier (Loc, Chars (A)),
             Expressions => Index_List1);

         R :=
           Make_Indexed_Component (Loc,
             Prefix      => Make_Identifier (Loc, Chars (B)),
             Expressions => Index_List2);

         Test := Expand_Composite_Equality
                   (Nod, Component_Type (Typ), L, R, Decls);

         --  If some (sub)component is an unchecked_union, the whole operation
         --  will raise program error.

         if Nkind (Test) = N_Raise_Program_Error then

            --  This node is going to be inserted at a location where a
            --  statement is expected: clear its Etype so analysis will set
            --  it to the expected Standard_Void_Type.

            Set_Etype (Test, Empty);
            return Test;

         else
            return
              Make_Implicit_If_Statement (Nod,
                Condition => Make_Op_Not (Loc, Right_Opnd => Test),
                Then_Statements => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))));
         end if;
      end Component_Equality;

      ------------------
      -- Get_Arg_Type --
      ------------------

      function Get_Arg_Type (N : Node_Id) return Entity_Id is
         T : Entity_Id;
         X : Node_Id;

      begin
         T := Etype (N);

         if No (T) then
            return Typ;

         else
            T := Underlying_Type (T);

            X := First_Index (T);
            while Present (X) loop
               if Denotes_Discriminant (Type_Low_Bound  (Etype (X)))
                    or else
                  Denotes_Discriminant (Type_High_Bound (Etype (X)))
               then
                  T := Base_Type (T);
                  exit;
               end if;

               Next_Index (X);
            end loop;

            return T;
         end if;
      end Get_Arg_Type;

      --------------------------
      -- Handle_One_Dimension --
      ---------------------------

      function Handle_One_Dimension
        (N     : Int;
         Index : Node_Id) return Node_Id
      is
         Need_Separate_Indexes : constant Boolean :=
           Ltyp /= Rtyp or else not Is_Constrained (Ltyp);
         --  If the index types are identical, and we are working with
         --  constrained types, then we can use the same index for both
         --  of the arrays.

         An : constant Entity_Id := Make_Temporary (Loc, 'A');

         Bn       : Entity_Id;
         Index_T  : Entity_Id;
         Stm_List : List_Id;
         Loop_Stm : Node_Id;

      begin
         if N > Number_Dimensions (Ltyp) then
            return Component_Equality (Ltyp);
         end if;

         --  Case where we generate a loop

         Index_T := Base_Type (Etype (Index));

         if Need_Separate_Indexes then
            Bn := Make_Temporary (Loc, 'B');
         else
            Bn := An;
         end if;

         Append (New_Occurrence_Of (An, Loc), Index_List1);
         Append (New_Occurrence_Of (Bn, Loc), Index_List2);

         Stm_List := New_List (
           Handle_One_Dimension (N + 1, Next_Index (Index)));

         if Need_Separate_Indexes then

            --  Generate guard for loop, followed by increments of indexes

            Append_To (Stm_List,
               Make_Exit_Statement (Loc,
                 Condition =>
                   Make_Op_Eq (Loc,
                      Left_Opnd => New_Occurrence_Of (An, Loc),
                      Right_Opnd => Arr_Attr (A, Name_Last, N))));

            Append_To (Stm_List,
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (An, Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Index_T, Loc),
                    Attribute_Name => Name_Succ,
                    Expressions    => New_List (
                      New_Occurrence_Of (An, Loc)))));

            Append_To (Stm_List,
              Make_Assignment_Statement (Loc,
                Name       => New_Occurrence_Of (Bn, Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Index_T, Loc),
                    Attribute_Name => Name_Succ,
                    Expressions    => New_List (
                      New_Occurrence_Of (Bn, Loc)))));
         end if;

         --  If separate indexes, we need a declare block for An and Bn, and a
         --  loop without an iteration scheme.

         if Need_Separate_Indexes then
            Loop_Stm :=
              Make_Implicit_Loop_Statement (Nod, Statements => Stm_List);

            return
              Make_Block_Statement (Loc,
                Declarations => New_List (
                  Make_Object_Declaration (Loc,
                    Defining_Identifier => An,
                    Object_Definition   => New_Occurrence_Of (Index_T, Loc),
                    Expression          => Arr_Attr (A, Name_First, N)),

                  Make_Object_Declaration (Loc,
                    Defining_Identifier => Bn,
                    Object_Definition   => New_Occurrence_Of (Index_T, Loc),
                    Expression          => Arr_Attr (B, Name_First, N))),

                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Loop_Stm)));

         --  If no separate indexes, return loop statement with explicit
         --  iteration scheme on its own

         else
            Loop_Stm :=
              Make_Implicit_Loop_Statement (Nod,
                Statements       => Stm_List,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier         => An,
                        Discrete_Subtype_Definition =>
                          Arr_Attr (A, Name_Range, N))));
            return Loop_Stm;
         end if;
      end Handle_One_Dimension;

      -----------------------
      -- Test_Empty_Arrays --
      -----------------------

      function Test_Empty_Arrays return Node_Id is
         Alist : Node_Id;
         Blist : Node_Id;

         Atest : Node_Id;
         Btest : Node_Id;

      begin
         Alist := Empty;
         Blist := Empty;
         for J in 1 .. Number_Dimensions (Ltyp) loop
            Atest :=
              Make_Op_Eq (Loc,
                Left_Opnd  => Arr_Attr (A, Name_Length, J),
                Right_Opnd => Make_Integer_Literal (Loc, 0));

            Btest :=
              Make_Op_Eq (Loc,
                Left_Opnd  => Arr_Attr (B, Name_Length, J),
                Right_Opnd => Make_Integer_Literal (Loc, 0));

            if No (Alist) then
               Alist := Atest;
               Blist := Btest;

            else
               Alist :=
                 Make_Or_Else (Loc,
                   Left_Opnd  => Relocate_Node (Alist),
                   Right_Opnd => Atest);

               Blist :=
                 Make_Or_Else (Loc,
                   Left_Opnd  => Relocate_Node (Blist),
                   Right_Opnd => Btest);
            end if;
         end loop;

         return
           Make_And_Then (Loc,
             Left_Opnd  => Alist,
             Right_Opnd => Blist);
      end Test_Empty_Arrays;

      -----------------------------
      -- Test_Lengths_Correspond --
      -----------------------------

      function Test_Lengths_Correspond return Node_Id is
         Result : Node_Id;
         Rtest  : Node_Id;

      begin
         Result := Empty;
         for J in 1 .. Number_Dimensions (Ltyp) loop
            Rtest :=
              Make_Op_Ne (Loc,
                Left_Opnd  => Arr_Attr (A, Name_Length, J),
                Right_Opnd => Arr_Attr (B, Name_Length, J));

            if No (Result) then
               Result := Rtest;
            else
               Result :=
                 Make_Or_Else (Loc,
                   Left_Opnd  => Relocate_Node (Result),
                   Right_Opnd => Rtest);
            end if;
         end loop;

         return Result;
      end Test_Lengths_Correspond;

   --  Start of processing for Expand_Array_Equality

   begin
      Ltyp := Get_Arg_Type (Lhs);
      Rtyp := Get_Arg_Type (Rhs);

      --  For now, if the argument types are not the same, go to the base type,
      --  since the code assumes that the formals have the same type. This is
      --  fixable in future ???

      if Ltyp /= Rtyp then
         Ltyp := Base_Type (Ltyp);
         Rtyp := Base_Type (Rtyp);
         pragma Assert (Ltyp = Rtyp);
      end if;

      --  Build list of formals for function

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type      => New_Occurrence_Of (Ltyp, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type      => New_Occurrence_Of (Rtyp, Loc)));

      Func_Name := Make_Temporary (Loc, 'E');

      --  Build statement sequence for function

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Result_Definition => New_Occurrence_Of (Standard_Boolean, Loc)),

          Declarations =>  Decls,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (

                Make_Implicit_If_Statement (Nod,
                  Condition => Test_Empty_Arrays,
                  Then_Statements => New_List (
                    Make_Simple_Return_Statement (Loc,
                      Expression =>
                        New_Occurrence_Of (Standard_True, Loc)))),

                Make_Implicit_If_Statement (Nod,
                  Condition => Test_Lengths_Correspond,
                  Then_Statements => New_List (
                    Make_Simple_Return_Statement (Loc,
                      Expression =>
                        New_Occurrence_Of (Standard_False, Loc)))),

                Handle_One_Dimension (1, First_Index (Ltyp)),

                Make_Simple_Return_Statement (Loc,
                  Expression => New_Occurrence_Of (Standard_True, Loc)))));

         Set_Has_Completion (Func_Name, True);
         Set_Is_Inlined (Func_Name);

         --  If the array type is distinct from the type of the arguments, it
         --  is the full view of a private type. Apply an unchecked conversion
         --  to insure that analysis of the call succeeds.

         declare
            L, R : Node_Id;

         begin
            L := Lhs;
            R := Rhs;

            if No (Etype (Lhs))
              or else Base_Type (Etype (Lhs)) /= Base_Type (Ltyp)
            then
               L := OK_Convert_To (Ltyp, Lhs);
            end if;

            if No (Etype (Rhs))
              or else Base_Type (Etype (Rhs)) /= Base_Type (Rtyp)
            then
               R := OK_Convert_To (Rtyp, Rhs);
            end if;

            Actuals := New_List (L, R);
         end;

         Append_To (Bodies, Func_Body);

         return
           Make_Function_Call (Loc,
             Name                   => New_Occurrence_Of (Func_Name, Loc),
             Parameter_Associations => Actuals);
   end Expand_Array_Equality;

   -----------------------------
   -- Expand_Boolean_Operator --
   -----------------------------

   --  Note that we first get the actual subtypes of the operands, since we
   --  always want to deal with types that have bounds.

   procedure Expand_Boolean_Operator (N : Node_Id) is
      Typ : constant Entity_Id  := Etype (N);

   begin
      --  Special case of bit packed array where both operands are known to be
      --  properly aligned. In this case we use an efficient run time routine
      --  to carry out the operation (see System.Bit_Ops).

      if Is_Bit_Packed_Array (Typ)
        and then not Is_Possibly_Unaligned_Object (Left_Opnd (N))
        and then not Is_Possibly_Unaligned_Object (Right_Opnd (N))
      then
         Expand_Packed_Boolean_Operator (N);
         return;
      end if;

      --  For the normal non-packed case, the general expansion is to build
      --  function for carrying out the comparison (use Make_Boolean_Array_Op)
      --  and then inserting it into the tree. The original operator node is
      --  then rewritten as a call to this function. We also use this in the
      --  packed case if either operand is a possibly unaligned object.

      declare
         Loc       : constant Source_Ptr := Sloc (N);
         L         : constant Node_Id    := Relocate_Node (Left_Opnd  (N));
         R         : constant Node_Id    := Relocate_Node (Right_Opnd (N));
         Func_Body : Node_Id;
         Func_Name : Entity_Id;

      begin
         Convert_To_Actual_Subtype (L);
         Convert_To_Actual_Subtype (R);
         Ensure_Defined (Etype (L), N);
         Ensure_Defined (Etype (R), N);
         Apply_Length_Check (R, Etype (L));

         if Nkind (N) = N_Op_Xor then
            Silly_Boolean_Array_Xor_Test (N, Etype (L));
         end if;

         if Nkind (Parent (N)) = N_Assignment_Statement
           and then Safe_In_Place_Array_Op (Name (Parent (N)), L, R)
         then
            Build_Boolean_Array_Proc_Call (Parent (N), L, R);

         elsif Nkind (Parent (N)) = N_Op_Not
           and then Nkind (N) = N_Op_And
           and then
             Safe_In_Place_Array_Op (Name (Parent (Parent (N))), L, R)
         then
            return;
         else

            Func_Body := Make_Boolean_Array_Op (Etype (L), N);
            Func_Name := Defining_Unit_Name (Specification (Func_Body));
            Insert_Action (N, Func_Body);

            --  Now rewrite the expression with a call

            Rewrite (N,
              Make_Function_Call (Loc,
                Name                   => New_Occurrence_Of (Func_Name, Loc),
                Parameter_Associations =>
                  New_List (
                    L,
                    Make_Type_Conversion
                      (Loc, New_Occurrence_Of (Etype (L), Loc), R))));

            Analyze_And_Resolve (N, Typ);
         end if;
      end;
   end Expand_Boolean_Operator;

   ------------------------------------------------
   -- Expand_Compare_Minimize_Eliminate_Overflow --
   ------------------------------------------------

   procedure Expand_Compare_Minimize_Eliminate_Overflow (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Result_Type : constant Entity_Id := Etype (N);
      --  Capture result type (could be a derived boolean type)

      Llo, Lhi : Uint;
      Rlo, Rhi : Uint;

      LLIB : constant Entity_Id := Base_Type (Standard_Long_Long_Integer);
      --  Entity for Long_Long_Integer'Base

      Check : constant Overflow_Mode_Type := Overflow_Check_Mode;
      --  Current overflow checking mode

      procedure Set_True;
      procedure Set_False;
      --  These procedures rewrite N with an occurrence of Standard_True or
      --  Standard_False, and then makes a call to Warn_On_Known_Condition.

      ---------------
      -- Set_False --
      ---------------

      procedure Set_False is
      begin
         Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
         Warn_On_Known_Condition (N);
      end Set_False;

      --------------
      -- Set_True --
      --------------

      procedure Set_True is
      begin
         Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
         Warn_On_Known_Condition (N);
      end Set_True;

   --  Start of processing for Expand_Compare_Minimize_Eliminate_Overflow

   begin
      --  Nothing to do unless we have a comparison operator with operands
      --  that are signed integer types, and we are operating in either
      --  MINIMIZED or ELIMINATED overflow checking mode.

      if Nkind (N) not in N_Op_Compare
        or else Check not in Minimized_Or_Eliminated
        or else not Is_Signed_Integer_Type (Etype (Left_Opnd (N)))
      then
         return;
      end if;

      --  OK, this is the case we are interested in. First step is to process
      --  our operands using the Minimize_Eliminate circuitry which applies
      --  this processing to the two operand subtrees.

      Minimize_Eliminate_Overflows
        (Left_Opnd (N),  Llo, Lhi, Top_Level => False);
      Minimize_Eliminate_Overflows
        (Right_Opnd (N), Rlo, Rhi, Top_Level => False);

      --  See if the range information decides the result of the comparison.
      --  We can only do this if we in fact have full range information (which
      --  won't be the case if either operand is bignum at this stage).

      if Llo /= No_Uint and then Rlo /= No_Uint then
         case N_Op_Compare (Nkind (N)) is
         when N_Op_Eq =>
            if Llo = Lhi and then Rlo = Rhi and then Llo = Rlo then
               Set_True;
            elsif Llo > Rhi or else Lhi < Rlo then
               Set_False;
            end if;

         when N_Op_Ge =>
            if Llo >= Rhi then
               Set_True;
            elsif Lhi < Rlo then
               Set_False;
            end if;

         when N_Op_Gt =>
            if Llo > Rhi then
               Set_True;
            elsif Lhi <= Rlo then
               Set_False;
            end if;

         when N_Op_Le =>
            if Llo > Rhi then
               Set_False;
            elsif Lhi <= Rlo then
               Set_True;
            end if;

         when N_Op_Lt =>
            if Llo >= Rhi then
               Set_False;
            elsif Lhi < Rlo then
               Set_True;
            end if;

         when N_Op_Ne =>
            if Llo = Lhi and then Rlo = Rhi and then Llo = Rlo then
               Set_False;
            elsif Llo > Rhi or else Lhi < Rlo then
               Set_True;
            end if;
         end case;

         --  All done if we did the rewrite

         if Nkind (N) not in N_Op_Compare then
            return;
         end if;
      end if;

      --  Otherwise, time to do the comparison

      declare
         Ltype : constant Entity_Id := Etype (Left_Opnd (N));
         Rtype : constant Entity_Id := Etype (Right_Opnd (N));

      begin
         --  If the two operands have the same signed integer type we are
         --  all set, nothing more to do. This is the case where either
         --  both operands were unchanged, or we rewrote both of them to
         --  be Long_Long_Integer.

         --  Note: Entity for the comparison may be wrong, but it's not worth
         --  the effort to change it, since the back end does not use it.

         if Is_Signed_Integer_Type (Ltype)
           and then Base_Type (Ltype) = Base_Type (Rtype)
         then
            return;

         --  Here if bignums are involved (can only happen in ELIMINATED mode)

         elsif Is_RTE (Ltype, RE_Bignum) or else Is_RTE (Rtype, RE_Bignum) then
            declare
               Left  : Node_Id := Left_Opnd (N);
               Right : Node_Id := Right_Opnd (N);
               --  Bignum references for left and right operands

            begin
               if not Is_RTE (Ltype, RE_Bignum) then
                  Left := Convert_To_Bignum (Left);
               elsif not Is_RTE (Rtype, RE_Bignum) then
                  Right := Convert_To_Bignum (Right);
               end if;

               --  We rewrite our node with:

               --    do
               --       Bnn : Result_Type;
               --       declare
               --          M : Mark_Id := SS_Mark;
               --       begin
               --          Bnn := Big_xx (Left, Right); (xx = EQ, NT etc)
               --          SS_Release (M);
               --       end;
               --    in
               --       Bnn
               --    end

               declare
                  Blk : constant Node_Id   := Make_Bignum_Block (Loc);
                  Bnn : constant Entity_Id := Make_Temporary (Loc, 'B', N);
                  Ent : RE_Id;

               begin
                  case N_Op_Compare (Nkind (N)) is
                     when N_Op_Eq => Ent := RE_Big_EQ;
                     when N_Op_Ge => Ent := RE_Big_GE;
                     when N_Op_Gt => Ent := RE_Big_GT;
                     when N_Op_Le => Ent := RE_Big_LE;
                     when N_Op_Lt => Ent := RE_Big_LT;
                     when N_Op_Ne => Ent := RE_Big_NE;
                  end case;

                  --  Insert assignment to Bnn into the bignum block

                  Insert_Before
                    (First (Statements (Handled_Statement_Sequence (Blk))),
                     Make_Assignment_Statement (Loc,
                       Name       => New_Occurrence_Of (Bnn, Loc),
                       Expression =>
                         Make_Function_Call (Loc,
                           Name                   =>
                             New_Occurrence_Of (RTE (Ent), Loc),
                           Parameter_Associations => New_List (Left, Right))));

                  --  Now do the rewrite with expression actions

                  Rewrite (N,
                    Make_Expression_With_Actions (Loc,
                      Actions    => New_List (
                        Make_Object_Declaration (Loc,
                          Defining_Identifier => Bnn,
                          Object_Definition   =>
                            New_Occurrence_Of (Result_Type, Loc)),
                        Blk),
                      Expression => New_Occurrence_Of (Bnn, Loc)));
                  Analyze_And_Resolve (N, Result_Type);
               end;
            end;

         --  No bignums involved, but types are different, so we must have
         --  rewritten one of the operands as a Long_Long_Integer but not
         --  the other one.

         --  If left operand is Long_Long_Integer, convert right operand
         --  and we are done (with a comparison of two Long_Long_Integers).

         elsif Ltype = LLIB then
            Convert_To_And_Rewrite (LLIB, Right_Opnd (N));
            Analyze_And_Resolve (Right_Opnd (N), LLIB, Suppress => All_Checks);
            return;

         --  If right operand is Long_Long_Integer, convert left operand
         --  and we are done (with a comparison of two Long_Long_Integers).

         --  This is the only remaining possibility

         else pragma Assert (Rtype = LLIB);
            Convert_To_And_Rewrite (LLIB, Left_Opnd (N));
            Analyze_And_Resolve (Left_Opnd (N), LLIB, Suppress => All_Checks);
            return;
         end if;
      end;
   end Expand_Compare_Minimize_Eliminate_Overflow;

   -------------------------------
   -- Expand_Composite_Equality --
   -------------------------------

   --  This function is only called for comparing internal fields of composite
   --  types when these fields are themselves composites. This is a special
   --  case because it is not possible to respect normal Ada visibility rules.

   function Expand_Composite_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Nod);
      Full_Type : Entity_Id;
      Prim      : Elmt_Id;
      Eq_Op     : Entity_Id;

      function Find_Primitive_Eq return Node_Id;
      --  AI05-0123: Locate primitive equality for type if it exists, and
      --  build the corresponding call. If operation is abstract, replace
      --  call with an explicit raise. Return Empty if there is no primitive.

      -----------------------
      -- Find_Primitive_Eq --
      -----------------------

      function Find_Primitive_Eq return Node_Id is
         Prim_E : Elmt_Id;
         Prim   : Node_Id;

      begin
         Prim_E := First_Elmt (Collect_Primitive_Operations (Typ));
         while Present (Prim_E) loop
            Prim := Node (Prim_E);

            --  Locate primitive equality with the right signature

            if Chars (Prim) = Name_Op_Eq
              and then Etype (First_Formal (Prim)) =
                       Etype (Next_Formal (First_Formal (Prim)))
              and then Etype (Prim) = Standard_Boolean
            then
               if Is_Abstract_Subprogram (Prim) then
                  return
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Explicit_Raise);

               else
                  return
                    Make_Function_Call (Loc,
                      Name                   => New_Occurrence_Of (Prim, Loc),
                      Parameter_Associations => New_List (Lhs, Rhs));
               end if;
            end if;

            Next_Elmt (Prim_E);
         end loop;

         --  If not found, predefined operation will be used

         return Empty;
      end Find_Primitive_Eq;

   --  Start of processing for Expand_Composite_Equality

   begin
      if Is_Private_Type (Typ) then
         Full_Type := Underlying_Type (Typ);
      else
         Full_Type := Typ;
      end if;

      --  If the private type has no completion the context may be the
      --  expansion of a composite equality for a composite type with some
      --  still incomplete components. The expression will not be analyzed
      --  until the enclosing type is completed, at which point this will be
      --  properly expanded, unless there is a bona fide completion error.

      if No (Full_Type) then
         return Make_Op_Eq (Loc, Left_Opnd => Lhs, Right_Opnd => Rhs);
      end if;

      Full_Type := Base_Type (Full_Type);

      --  When the base type itself is private, use the full view to expand
      --  the composite equality.

      if Is_Private_Type (Full_Type) then
         Full_Type := Underlying_Type (Full_Type);
      end if;

      --  Case of array types

      if Is_Array_Type (Full_Type) then

         --  If the operand is an elementary type other than a floating-point
         --  type, then we can simply use the built-in block bitwise equality,
         --  since the predefined equality operators always apply and bitwise
         --  equality is fine for all these cases.

         if Is_Elementary_Type (Component_Type (Full_Type))
           and then not Is_Floating_Point_Type (Component_Type (Full_Type))
         then
            return Make_Op_Eq (Loc, Left_Opnd => Lhs, Right_Opnd => Rhs);

         --  For composite component types, and floating-point types, use the
         --  expansion. This deals with tagged component types (where we use
         --  the applicable equality routine) and floating-point, (where we
         --  need to worry about negative zeroes), and also the case of any
         --  composite type recursively containing such fields.

         else
            return Expand_Array_Equality (Nod, Lhs, Rhs, Bodies, Full_Type);
         end if;

      --  Case of tagged record types

      elsif Is_Tagged_Type (Full_Type) then

         --  Call the primitive operation "=" of this type

         if Is_Class_Wide_Type (Full_Type) then
            Full_Type := Root_Type (Full_Type);
         end if;

         --  If this is derived from an untagged private type completed with a
         --  tagged type, it does not have a full view, so we use the primitive
         --  operations of the private type. This check should no longer be
         --  necessary when these types receive their full views ???

         if Is_Private_Type (Typ)
           and then not Is_Tagged_Type (Typ)
           and then not Is_Controlled (Typ)
           and then Is_Derived_Type (Typ)
           and then No (Full_View (Typ))
         then
            Prim := First_Elmt (Collect_Primitive_Operations (Typ));
         else
            Prim := First_Elmt (Primitive_Operations (Full_Type));
         end if;

         loop
            Eq_Op := Node (Prim);
            exit when Chars (Eq_Op) = Name_Op_Eq
              and then Etype (First_Formal (Eq_Op)) =
                       Etype (Next_Formal (First_Formal (Eq_Op)))
              and then Base_Type (Etype (Eq_Op)) = Standard_Boolean;
            Next_Elmt (Prim);
            pragma Assert (Present (Prim));
         end loop;

         Eq_Op := Node (Prim);

         return
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (Eq_Op, Loc),
             Parameter_Associations =>
               New_List
                 (Unchecked_Convert_To (Etype (First_Formal (Eq_Op)), Lhs),
                  Unchecked_Convert_To (Etype (First_Formal (Eq_Op)), Rhs)));

      --  Case of untagged record types

      elsif Is_Record_Type (Full_Type) then
         Eq_Op := TSS (Full_Type, TSS_Composite_Equality);

         if Present (Eq_Op) then
            if Etype (First_Formal (Eq_Op)) /= Full_Type then

               --  Inherited equality from parent type. Convert the actuals to
               --  match signature of operation.

               declare
                  T : constant Entity_Id := Etype (First_Formal (Eq_Op));

               begin
                  return
                    Make_Function_Call (Loc,
                      Name                  => New_Occurrence_Of (Eq_Op, Loc),
                      Parameter_Associations => New_List (
                        OK_Convert_To (T, Lhs),
                        OK_Convert_To (T, Rhs)));
               end;

            else
               --  Comparison between Unchecked_Union components

               if Is_Unchecked_Union (Full_Type) then
                  declare
                     Lhs_Type      : Node_Id := Full_Type;
                     Rhs_Type      : Node_Id := Full_Type;
                     Lhs_Discr_Val : Node_Id;
                     Rhs_Discr_Val : Node_Id;

                  begin
                     --  Lhs subtype

                     if Nkind (Lhs) = N_Selected_Component then
                        Lhs_Type := Etype (Entity (Selector_Name (Lhs)));
                     end if;

                     --  Rhs subtype

                     if Nkind (Rhs) = N_Selected_Component then
                        Rhs_Type := Etype (Entity (Selector_Name (Rhs)));
                     end if;

                     --  Lhs of the composite equality

                     if Is_Constrained (Lhs_Type) then

                        --  Since the enclosing record type can never be an
                        --  Unchecked_Union (this code is executed for records
                        --  that do not have variants), we may reference its
                        --  discriminant(s).

                        if Nkind (Lhs) = N_Selected_Component
                          and then Has_Per_Object_Constraint
                                     (Entity (Selector_Name (Lhs)))
                        then
                           Lhs_Discr_Val :=
                             Make_Selected_Component (Loc,
                               Prefix        => Prefix (Lhs),
                               Selector_Name =>
                                 New_Copy
                                   (Get_Discriminant_Value
                                      (First_Discriminant (Lhs_Type),
                                       Lhs_Type,
                                       Stored_Constraint (Lhs_Type))));

                        else
                           Lhs_Discr_Val :=
                             New_Copy
                               (Get_Discriminant_Value
                                  (First_Discriminant (Lhs_Type),
                                   Lhs_Type,
                                   Stored_Constraint (Lhs_Type)));

                        end if;
                     else
                        --  It is not possible to infer the discriminant since
                        --  the subtype is not constrained.

                        return
                          Make_Raise_Program_Error (Loc,
                            Reason => PE_Unchecked_Union_Restriction);
                     end if;

                     --  Rhs of the composite equality

                     if Is_Constrained (Rhs_Type) then
                        if Nkind (Rhs) = N_Selected_Component
                          and then Has_Per_Object_Constraint
                                     (Entity (Selector_Name (Rhs)))
                        then
                           Rhs_Discr_Val :=
                             Make_Selected_Component (Loc,
                               Prefix        => Prefix (Rhs),
                               Selector_Name =>
                                 New_Copy
                                   (Get_Discriminant_Value
                                      (First_Discriminant (Rhs_Type),
                                       Rhs_Type,
                                       Stored_Constraint (Rhs_Type))));

                        else
                           Rhs_Discr_Val :=
                             New_Copy
                               (Get_Discriminant_Value
                                  (First_Discriminant (Rhs_Type),
                                   Rhs_Type,
                                   Stored_Constraint (Rhs_Type)));

                        end if;
                     else
                        return
                          Make_Raise_Program_Error (Loc,
                            Reason => PE_Unchecked_Union_Restriction);
                     end if;

                     --  Call the TSS equality function with the inferred
                     --  discriminant values.

                     return
                       Make_Function_Call (Loc,
                         Name => New_Occurrence_Of (Eq_Op, Loc),
                         Parameter_Associations => New_List (
                           Lhs,
                           Rhs,
                           Lhs_Discr_Val,
                           Rhs_Discr_Val));
                  end;

               else
                  return
                    Make_Function_Call (Loc,
                      Name                   => New_Occurrence_Of (Eq_Op, Loc),
                      Parameter_Associations => New_List (Lhs, Rhs));
               end if;
            end if;

         --  Equality composes in Ada 2012 for untagged record types. It also
         --  composes for bounded strings, because they are part of the
         --  predefined environment. We could make it compose for bounded
         --  strings by making them tagged, or by making sure all subcomponents
         --  are set to the same value, even when not used. Instead, we have
         --  this special case in the compiler, because it's more efficient.

         elsif Ada_Version >= Ada_2012 or else Is_Bounded_String (Typ) then

            --  If no TSS has been created for the type, check whether there is
            --  a primitive equality declared for it.

            declare
               Op : constant Node_Id := Find_Primitive_Eq;

            begin
               --  Use user-defined primitive if it exists, otherwise use
               --  predefined equality.

               if Present (Op) then
                  return Op;
               else
                  return Make_Op_Eq (Loc, Lhs, Rhs);
               end if;
            end;

         else
            return Expand_Record_Equality (Nod, Full_Type, Lhs, Rhs, Bodies);
         end if;

      --  Non-composite types (always use predefined equality)

      else
         return Make_Op_Eq (Loc, Left_Opnd => Lhs, Right_Opnd => Rhs);
      end if;
   end Expand_Composite_Equality;

   ------------------------
   -- Expand_Concatenate --
   ------------------------

   procedure Expand_Concatenate (Cnode : Node_Id; Opnds : List_Id) is
      Loc : constant Source_Ptr := Sloc (Cnode);

      Atyp : constant Entity_Id := Base_Type (Etype (Cnode));
      --  Result type of concatenation

      Ctyp : constant Entity_Id := Base_Type (Component_Type (Etype (Cnode)));
      --  Component type. Elements of this component type can appear as one
      --  of the operands of concatenation as well as arrays.

      Istyp : constant Entity_Id := Etype (First_Index (Atyp));
      --  Index subtype

      Ityp : constant Entity_Id := Base_Type (Istyp);
      --  Index type. This is the base type of the index subtype, and is used
      --  for all computed bounds (which may be out of range of Istyp in the
      --  case of null ranges).

      Artyp : Entity_Id;
      --  This is the type we use to do arithmetic to compute the bounds and
      --  lengths of operands. The choice of this type is a little subtle and
      --  is discussed in a separate section at the start of the body code.

      Concatenation_Error : exception;
      --  Raised if concatenation is sure to raise a CE

      Result_May_Be_Null : Boolean := True;
      --  Reset to False if at least one operand is encountered which is known
      --  at compile time to be non-null. Used for handling the special case
      --  of setting the high bound to the last operand high bound for a null
      --  result, thus ensuring a proper high bound in the super-flat case.

      N : constant Nat := List_Length (Opnds);
      --  Number of concatenation operands including possibly null operands

      NN : Nat := 0;
      --  Number of operands excluding any known to be null, except that the
      --  last operand is always retained, in case it provides the bounds for
      --  a null result.

      Opnd : Node_Id;
      --  Current operand being processed in the loop through operands. After
      --  this loop is complete, always contains the last operand (which is not
      --  the same as Operands (NN), since null operands are skipped).

      --  Arrays describing the operands, only the first NN entries of each
      --  array are set (NN < N when we exclude known null operands).

      Is_Fixed_Length : array (1 .. N) of Boolean;
      --  True if length of corresponding operand known at compile time

      Operands : array (1 .. N) of Node_Id;
      --  Set to the corresponding entry in the Opnds list (but note that null
      --  operands are excluded, so not all entries in the list are stored).

      Fixed_Length : array (1 .. N) of Uint;
      --  Set to length of operand. Entries in this array are set only if the
      --  corresponding entry in Is_Fixed_Length is True.

      Opnd_Low_Bound : array (1 .. N) of Node_Id;
      --  Set to lower bound of operand. Either an integer literal in the case
      --  where the bound is known at compile time, else actual lower bound.
      --  The operand low bound is of type Ityp.

      Var_Length : array (1 .. N) of Entity_Id;
      --  Set to an entity of type Natural that contains the length of an
      --  operand whose length is not known at compile time. Entries in this
      --  array are set only if the corresponding entry in Is_Fixed_Length
      --  is False. The entity is of type Artyp.

      Aggr_Length : array (0 .. N) of Node_Id;
      --  The J'th entry in an expression node that represents the total length
      --  of operands 1 through J. It is either an integer literal node, or a
      --  reference to a constant entity with the right value, so it is fine
      --  to just do a Copy_Node to get an appropriate copy. The extra zero'th
      --  entry always is set to zero. The length is of type Artyp.

      Low_Bound : Node_Id;
      --  A tree node representing the low bound of the result (of type Ityp).
      --  This is either an integer literal node, or an identifier reference to
      --  a constant entity initialized to the appropriate value.

      Last_Opnd_Low_Bound : Node_Id;
      --  A tree node representing the low bound of the last operand. This
      --  need only be set if the result could be null. It is used for the
      --  special case of setting the right low bound for a null result.
      --  This is of type Ityp.

      Last_Opnd_High_Bound : Node_Id;
      --  A tree node representing the high bound of the last operand. This
      --  need only be set if the result could be null. It is used for the
      --  special case of setting the right high bound for a null result.
      --  This is of type Ityp.

      High_Bound : Node_Id;
      --  A tree node representing the high bound of the result (of type Ityp)

      Result : Node_Id;
      --  Result of the concatenation (of type Ityp)

      Actions : constant List_Id := New_List;
      --  Collect actions to be inserted

      Known_Non_Null_Operand_Seen : Boolean;
      --  Set True during generation of the assignments of operands into
      --  result once an operand known to be non-null has been seen.

      function Make_Artyp_Literal (Val : Nat) return Node_Id;
      --  This function makes an N_Integer_Literal node that is returned in
      --  analyzed form with the type set to Artyp. Importantly this literal
      --  is not flagged as static, so that if we do computations with it that
      --  result in statically detected out of range conditions, we will not
      --  generate error messages but instead warning messages.

      function To_Artyp (X : Node_Id) return Node_Id;
      --  Given a node of type Ityp, returns the corresponding value of type
      --  Artyp. For non-enumeration types, this is a plain integer conversion.
      --  For enum types, the Pos of the value is returned.

      function To_Ityp (X : Node_Id) return Node_Id;
      --  The inverse function (uses Val in the case of enumeration types)

      ------------------------
      -- Make_Artyp_Literal --
      ------------------------

      function Make_Artyp_Literal (Val : Nat) return Node_Id is
         Result : constant Node_Id := Make_Integer_Literal (Loc, Val);
      begin
         Set_Etype (Result, Artyp);
         Set_Analyzed (Result, True);
         Set_Is_Static_Expression (Result, False);
         return Result;
      end Make_Artyp_Literal;

      --------------
      -- To_Artyp --
      --------------

      function To_Artyp (X : Node_Id) return Node_Id is
      begin
         if Ityp = Base_Type (Artyp) then
            return X;

         elsif Is_Enumeration_Type (Ityp) then
            return
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Ityp, Loc),
                Attribute_Name => Name_Pos,
                Expressions    => New_List (X));

         else
            return Convert_To (Artyp, X);
         end if;
      end To_Artyp;

      -------------
      -- To_Ityp --
      -------------

      function To_Ityp (X : Node_Id) return Node_Id is
      begin
         if Is_Enumeration_Type (Ityp) then
            return
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Ityp, Loc),
                Attribute_Name => Name_Val,
                Expressions    => New_List (X));

         --  Case where we will do a type conversion

         else
            if Ityp = Base_Type (Artyp) then
               return X;
            else
               return Convert_To (Ityp, X);
            end if;
         end if;
      end To_Ityp;

      --  Local Declarations

      Lib_Level_Target : constant Boolean :=
        Nkind (Parent (Cnode)) = N_Object_Declaration
          and then
            Is_Library_Level_Entity (Defining_Identifier (Parent (Cnode)));

      --  If the concatenation declares a library level entity, we call the
      --  built-in concatenation routines to prevent code bloat, regardless
      --  of optimization level. This is space-efficient, and prevent linking
      --  problems when units are compiled with different optimizations.

      Opnd_Typ : Entity_Id;
      Ent      : Entity_Id;
      Len      : Uint;
      J        : Nat;
      Clen     : Node_Id;
      Set      : Boolean;

   --  Start of processing for Expand_Concatenate

   begin
      --  Choose an appropriate computational type

      --  We will be doing calculations of lengths and bounds in this routine
      --  and computing one from the other in some cases, e.g. getting the high
      --  bound by adding the length-1 to the low bound.

      --  We can't just use the index type, or even its base type for this
      --  purpose for two reasons. First it might be an enumeration type which
      --  is not suitable for computations of any kind, and second it may
      --  simply not have enough range. For example if the index type is
      --  -128..+127 then lengths can be up to 256, which is out of range of
      --  the type.

      --  For enumeration types, we can simply use Standard_Integer, this is
      --  sufficient since the actual number of enumeration literals cannot
      --  possibly exceed the range of integer (remember we will be doing the
      --  arithmetic with POS values, not representation values).

      if Is_Enumeration_Type (Ityp) then
         Artyp := Standard_Integer;

      --  If index type is Positive, we use the standard unsigned type, to give
      --  more room on the top of the range, obviating the need for an overflow
      --  check when creating the upper bound. This is needed to avoid junk
      --  overflow checks in the common case of String types.

      --  ??? Disabled for now

      --  elsif Istyp = Standard_Positive then
      --     Artyp := Standard_Unsigned;

      --  For modular types, we use a 32-bit modular type for types whose size
      --  is in the range 1-31 bits. For 32-bit unsigned types, we use the
      --  identity type, and for larger unsigned types we use 64-bits.

      elsif Is_Modular_Integer_Type (Ityp) then
         if RM_Size (Ityp) < RM_Size (Standard_Unsigned) then
            Artyp := Standard_Unsigned;
         elsif RM_Size (Ityp) = RM_Size (Standard_Unsigned) then
            Artyp := Ityp;
         else
            Artyp := RTE (RE_Long_Long_Unsigned);
         end if;

      --  Similar treatment for signed types

      else
         if RM_Size (Ityp) < RM_Size (Standard_Integer) then
            Artyp := Standard_Integer;
         elsif RM_Size (Ityp) = RM_Size (Standard_Integer) then
            Artyp := Ityp;
         else
            Artyp := Standard_Long_Long_Integer;
         end if;
      end if;

      --  Supply dummy entry at start of length array

      Aggr_Length (0) := Make_Artyp_Literal (0);

      --  Go through operands setting up the above arrays

      J := 1;
      while J <= N loop
         Opnd := Remove_Head (Opnds);
         Opnd_Typ := Etype (Opnd);

         --  The parent got messed up when we put the operands in a list,
         --  so now put back the proper parent for the saved operand, that
         --  is to say the concatenation node, to make sure that each operand
         --  is seen as a subexpression, e.g. if actions must be inserted.

         Set_Parent (Opnd, Cnode);

         --  Set will be True when we have setup one entry in the array

         Set := False;

         --  Singleton element (or character literal) case

         if Base_Type (Opnd_Typ) = Ctyp then
            NN := NN + 1;
            Operands (NN) := Opnd;
            Is_Fixed_Length (NN) := True;
            Fixed_Length (NN) := Uint_1;
            Result_May_Be_Null := False;

            --  Set low bound of operand (no need to set Last_Opnd_High_Bound
            --  since we know that the result cannot be null).

            Opnd_Low_Bound (NN) :=
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Istyp, Loc),
                Attribute_Name => Name_First);

            Set := True;

         --  String literal case (can only occur for strings of course)

         elsif Nkind (Opnd) = N_String_Literal then
            Len := String_Literal_Length (Opnd_Typ);

            if Len /= 0 then
               Result_May_Be_Null := False;
            end if;

            --  Capture last operand low and high bound if result could be null

            if J = N and then Result_May_Be_Null then
               Last_Opnd_Low_Bound :=
                 New_Copy_Tree (String_Literal_Low_Bound (Opnd_Typ));

               Last_Opnd_High_Bound :=
                 Make_Op_Subtract (Loc,
                   Left_Opnd  =>
                     New_Copy_Tree (String_Literal_Low_Bound (Opnd_Typ)),
                   Right_Opnd => Make_Integer_Literal (Loc, 1));
            end if;

            --  Skip null string literal

            if J < N and then Len = 0 then
               goto Continue;
            end if;

            NN := NN + 1;
            Operands (NN) := Opnd;
            Is_Fixed_Length (NN) := True;

            --  Set length and bounds

            Fixed_Length (NN) := Len;

            Opnd_Low_Bound (NN) :=
              New_Copy_Tree (String_Literal_Low_Bound (Opnd_Typ));

            Set := True;

         --  All other cases

         else
            --  Check constrained case with known bounds

            if Is_Constrained (Opnd_Typ) then
               declare
                  Index    : constant Node_Id   := First_Index (Opnd_Typ);
                  Indx_Typ : constant Entity_Id := Etype (Index);
                  Lo       : constant Node_Id   := Type_Low_Bound  (Indx_Typ);
                  Hi       : constant Node_Id   := Type_High_Bound (Indx_Typ);

               begin
                  --  Fixed length constrained array type with known at compile
                  --  time bounds is last case of fixed length operand.

                  if Compile_Time_Known_Value (Lo)
                       and then
                     Compile_Time_Known_Value (Hi)
                  then
                     declare
                        Loval : constant Uint := Expr_Value (Lo);
                        Hival : constant Uint := Expr_Value (Hi);
                        Len   : constant Uint :=
                                  UI_Max (Hival - Loval + 1, Uint_0);

                     begin
                        if Len > 0 then
                           Result_May_Be_Null := False;
                        end if;

                        --  Capture last operand bounds if result could be null

                        if J = N and then Result_May_Be_Null then
                           Last_Opnd_Low_Bound :=
                             Convert_To (Ityp,
                               Make_Integer_Literal (Loc, Expr_Value (Lo)));

                           Last_Opnd_High_Bound :=
                             Convert_To (Ityp,
                               Make_Integer_Literal (Loc, Expr_Value (Hi)));
                        end if;

                        --  Exclude null length case unless last operand

                        if J < N and then Len = 0 then
                           goto Continue;
                        end if;

                        NN := NN + 1;
                        Operands (NN) := Opnd;
                        Is_Fixed_Length (NN) := True;
                        Fixed_Length (NN)    := Len;

                        Opnd_Low_Bound (NN) :=
                          To_Ityp
                            (Make_Integer_Literal (Loc, Expr_Value (Lo)));
                        Set := True;
                     end;
                  end if;
               end;
            end if;

            --  All cases where the length is not known at compile time, or the
            --  special case of an operand which is known to be null but has a
            --  lower bound other than 1 or is other than a string type.

            if not Set then
               NN := NN + 1;

               --  Capture operand bounds

               Opnd_Low_Bound (NN) :=
                 Make_Attribute_Reference (Loc,
                   Prefix         =>
                     Duplicate_Subexpr (Opnd, Name_Req => True),
                   Attribute_Name => Name_First);

               --  Capture last operand bounds if result could be null

               if J = N and Result_May_Be_Null then
                  Last_Opnd_Low_Bound :=
                    Convert_To (Ityp,
                      Make_Attribute_Reference (Loc,
                        Prefix         =>
                          Duplicate_Subexpr (Opnd, Name_Req => True),
                        Attribute_Name => Name_First));

                  Last_Opnd_High_Bound :=
                    Convert_To (Ityp,
                      Make_Attribute_Reference (Loc,
                        Prefix         =>
                          Duplicate_Subexpr (Opnd, Name_Req => True),
                        Attribute_Name => Name_Last));
               end if;

               --  Capture length of operand in entity

               Operands (NN) := Opnd;
               Is_Fixed_Length (NN) := False;

               Var_Length (NN) := Make_Temporary (Loc, 'L');

               Append_To (Actions,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Var_Length (NN),
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Artyp, Loc),
                   Expression          =>
                     Make_Attribute_Reference (Loc,
                       Prefix         =>
                         Duplicate_Subexpr (Opnd, Name_Req => True),
                       Attribute_Name => Name_Length)));
            end if;
         end if;

         --  Set next entry in aggregate length array

         --  For first entry, make either integer literal for fixed length
         --  or a reference to the saved length for variable length.

         if NN = 1 then
            if Is_Fixed_Length (1) then
               Aggr_Length (1) := Make_Integer_Literal (Loc, Fixed_Length (1));
            else
               Aggr_Length (1) := New_Occurrence_Of (Var_Length (1), Loc);
            end if;

         --  If entry is fixed length and only fixed lengths so far, make
         --  appropriate new integer literal adding new length.

         elsif Is_Fixed_Length (NN)
           and then Nkind (Aggr_Length (NN - 1)) = N_Integer_Literal
         then
            Aggr_Length (NN) :=
              Make_Integer_Literal (Loc,
                Intval => Fixed_Length (NN) + Intval (Aggr_Length (NN - 1)));

         --  All other cases, construct an addition node for the length and
         --  create an entity initialized to this length.

         else
            Ent := Make_Temporary (Loc, 'L');

            if Is_Fixed_Length (NN) then
               Clen := Make_Integer_Literal (Loc, Fixed_Length (NN));
            else
               Clen := New_Occurrence_Of (Var_Length (NN), Loc);
            end if;

            Append_To (Actions,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Ent,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Artyp, Loc),
                Expression          =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => New_Copy (Aggr_Length (NN - 1)),
                    Right_Opnd => Clen)));

            Aggr_Length (NN) := Make_Identifier (Loc, Chars => Chars (Ent));
         end if;

      <<Continue>>
         J := J + 1;
      end loop;

      --  If we have only skipped null operands, return the last operand

      if NN = 0 then
         Result := Opnd;
         goto Done;
      end if;

      --  If we have only one non-null operand, return it and we are done.
      --  There is one case in which this cannot be done, and that is when
      --  the sole operand is of the element type, in which case it must be
      --  converted to an array, and the easiest way of doing that is to go
      --  through the normal general circuit.

      if NN = 1 and then Base_Type (Etype (Operands (1))) /= Ctyp then
         Result := Operands (1);
         goto Done;
      end if;

      --  Cases where we have a real concatenation

      --  Next step is to find the low bound for the result array that we
      --  will allocate. The rules for this are in (RM 4.5.6(5-7)).

      --  If the ultimate ancestor of the index subtype is a constrained array
      --  definition, then the lower bound is that of the index subtype as
      --  specified by (RM 4.5.3(6)).

      --  The right test here is to go to the root type, and then the ultimate
      --  ancestor is the first subtype of this root type.

      if Is_Constrained (First_Subtype (Root_Type (Atyp))) then
         Low_Bound :=
           Make_Attribute_Reference (Loc,
             Prefix         =>
               New_Occurrence_Of (First_Subtype (Root_Type (Atyp)), Loc),
             Attribute_Name => Name_First);

      --  If the first operand in the list has known length we know that
      --  the lower bound of the result is the lower bound of this operand.

      elsif Is_Fixed_Length (1) then
         Low_Bound := Opnd_Low_Bound (1);

      --  OK, we don't know the lower bound, we have to build a horrible
      --  if expression node of the form

      --     if Cond1'Length /= 0 then
      --        Opnd1 low bound
      --     else
      --        if Opnd2'Length /= 0 then
      --          Opnd2 low bound
      --        else
      --           ...

      --  The nesting ends either when we hit an operand whose length is known
      --  at compile time, or on reaching the last operand, whose low bound we
      --  take unconditionally whether or not it is null. It's easiest to do
      --  this with a recursive procedure:

      else
         declare
            function Get_Known_Bound (J : Nat) return Node_Id;
            --  Returns the lower bound determined by operands J .. NN

            ---------------------
            -- Get_Known_Bound --
            ---------------------

            function Get_Known_Bound (J : Nat) return Node_Id is
            begin
               if Is_Fixed_Length (J) or else J = NN then
                  return New_Copy (Opnd_Low_Bound (J));

               else
                  return
                    Make_If_Expression (Loc,
                      Expressions => New_List (

                        Make_Op_Ne (Loc,
                          Left_Opnd  =>
                            New_Occurrence_Of (Var_Length (J), Loc),
                          Right_Opnd =>
                            Make_Integer_Literal (Loc, 0)),

                        New_Copy (Opnd_Low_Bound (J)),
                        Get_Known_Bound (J + 1)));
               end if;
            end Get_Known_Bound;

         begin
            Ent := Make_Temporary (Loc, 'L');

            Append_To (Actions,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Ent,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Ityp, Loc),
                Expression          => Get_Known_Bound (1)));

            Low_Bound := New_Occurrence_Of (Ent, Loc);
         end;
      end if;

      --  Now we can safely compute the upper bound, normally
      --  Low_Bound + Length - 1.

      High_Bound :=
        To_Ityp (
          Make_Op_Add (Loc,
            Left_Opnd  => To_Artyp (New_Copy (Low_Bound)),
            Right_Opnd =>
              Make_Op_Subtract (Loc,
                Left_Opnd  => New_Copy (Aggr_Length (NN)),
                Right_Opnd => Make_Artyp_Literal (1))));

      --  Note that calculation of the high bound may cause overflow in some
      --  very weird cases, so in the general case we need an overflow check on
      --  the high bound. We can avoid this for the common case of string types
      --  and other types whose index is Positive, since we chose a wider range
      --  for the arithmetic type.

      if Istyp /= Standard_Positive then
         Activate_Overflow_Check (High_Bound);
      end if;

      --  Handle the exceptional case where the result is null, in which case
      --  case the bounds come from the last operand (so that we get the proper
      --  bounds if the last operand is super-flat).

      if Result_May_Be_Null then
         Low_Bound :=
           Make_If_Expression (Loc,
             Expressions => New_List (
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Copy (Aggr_Length (NN)),
                 Right_Opnd => Make_Artyp_Literal (0)),
               Last_Opnd_Low_Bound,
               Low_Bound));

         High_Bound :=
           Make_If_Expression (Loc,
             Expressions => New_List (
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Copy (Aggr_Length (NN)),
                 Right_Opnd => Make_Artyp_Literal (0)),
               Last_Opnd_High_Bound,
               High_Bound));
      end if;

      --  Here is where we insert the saved up actions

      Insert_Actions (Cnode, Actions, Suppress => All_Checks);

      --  Now we construct an array object with appropriate bounds. We mark
      --  the target as internal to prevent useless initialization when
      --  Initialize_Scalars is enabled. Also since this is the actual result
      --  entity, we make sure we have debug information for the result.

      Ent := Make_Temporary (Loc, 'S');
      Set_Is_Internal (Ent);
      Set_Needs_Debug_Info (Ent);

      --  If the bound is statically known to be out of range, we do not want
      --  to abort, we want a warning and a runtime constraint error. Note that
      --  we have arranged that the result will not be treated as a static
      --  constant, so we won't get an illegality during this insertion.

      Insert_Action (Cnode,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Ent,
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Occurrence_Of (Atyp, Loc),
              Constraint   =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => New_List (
                    Make_Range (Loc,
                      Low_Bound  => Low_Bound,
                      High_Bound => High_Bound))))),
        Suppress => All_Checks);

      --  If the result of the concatenation appears as the initializing
      --  expression of an object declaration, we can just rename the
      --  result, rather than copying it.

      Set_OK_To_Rename (Ent);

      --  Catch the static out of range case now

      if Raises_Constraint_Error (High_Bound) then
         raise Concatenation_Error;
      end if;

      --  Now we will generate the assignments to do the actual concatenation

      --  There is one case in which we will not do this, namely when all the
      --  following conditions are met:

      --    The result type is Standard.String

      --    There are nine or fewer retained (non-null) operands

      --    The optimization level is -O0

      --    The corresponding System.Concat_n.Str_Concat_n routine is
      --    available in the run time.

      --    The debug flag gnatd.c is not set

      --  If all these conditions are met then we generate a call to the
      --  relevant concatenation routine. The purpose of this is to avoid
      --  undesirable code bloat at -O0.

      if Atyp = Standard_String
        and then NN in 2 .. 9
        and then (Lib_Level_Target
          or else
            ((Opt.Optimization_Level = 0 or else Debug_Flag_Dot_CC)
               and then not Debug_Flag_Dot_C))
      then
         declare
            RR : constant array (Nat range 2 .. 9) of RE_Id :=
                   (RE_Str_Concat_2,
                    RE_Str_Concat_3,
                    RE_Str_Concat_4,
                    RE_Str_Concat_5,
                    RE_Str_Concat_6,
                    RE_Str_Concat_7,
                    RE_Str_Concat_8,
                    RE_Str_Concat_9);

         begin
            if RTE_Available (RR (NN)) then
               declare
                  Opnds : constant List_Id :=
                            New_List (New_Occurrence_Of (Ent, Loc));

               begin
                  for J in 1 .. NN loop
                     if Is_List_Member (Operands (J)) then
                        Remove (Operands (J));
                     end if;

                     if Base_Type (Etype (Operands (J))) = Ctyp then
                        Append_To (Opnds,
                          Make_Aggregate (Loc,
                            Component_Associations => New_List (
                              Make_Component_Association (Loc,
                                Choices => New_List (
                                  Make_Integer_Literal (Loc, 1)),
                                Expression => Operands (J)))));

                     else
                        Append_To (Opnds, Operands (J));
                     end if;
                  end loop;

                  Insert_Action (Cnode,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Occurrence_Of (RTE (RR (NN)), Loc),
                      Parameter_Associations => Opnds));

                  Result := New_Occurrence_Of (Ent, Loc);
                  goto Done;
               end;
            end if;
         end;
      end if;

      --  Not special case so generate the assignments

      Known_Non_Null_Operand_Seen := False;

      for J in 1 .. NN loop
         declare
            Lo : constant Node_Id :=
                   Make_Op_Add (Loc,
                     Left_Opnd  => To_Artyp (New_Copy (Low_Bound)),
                     Right_Opnd => Aggr_Length (J - 1));

            Hi : constant Node_Id :=
                   Make_Op_Add (Loc,
                     Left_Opnd  => To_Artyp (New_Copy (Low_Bound)),
                     Right_Opnd =>
                       Make_Op_Subtract (Loc,
                         Left_Opnd  => Aggr_Length (J),
                         Right_Opnd => Make_Artyp_Literal (1)));

         begin
            --  Singleton case, simple assignment

            if Base_Type (Etype (Operands (J))) = Ctyp then
               Known_Non_Null_Operand_Seen := True;
               Insert_Action (Cnode,
                 Make_Assignment_Statement (Loc,
                   Name       =>
                     Make_Indexed_Component (Loc,
                       Prefix      => New_Occurrence_Of (Ent, Loc),
                       Expressions => New_List (To_Ityp (Lo))),
                   Expression => Operands (J)),
                 Suppress => All_Checks);

            --  Array case, slice assignment, skipped when argument is fixed
            --  length and known to be null.

            elsif (not Is_Fixed_Length (J)) or else (Fixed_Length (J) > 0) then
               declare
                  Assign : Node_Id :=
                             Make_Assignment_Statement (Loc,
                               Name       =>
                                 Make_Slice (Loc,
                                   Prefix         =>
                                     New_Occurrence_Of (Ent, Loc),
                                   Discrete_Range =>
                                     Make_Range (Loc,
                                       Low_Bound  => To_Ityp (Lo),
                                       High_Bound => To_Ityp (Hi))),
                               Expression => Operands (J));
               begin
                  if Is_Fixed_Length (J) then
                     Known_Non_Null_Operand_Seen := True;

                  elsif not Known_Non_Null_Operand_Seen then

                     --  Here if operand length is not statically known and no
                     --  operand known to be non-null has been processed yet.
                     --  If operand length is 0, we do not need to perform the
                     --  assignment, and we must avoid the evaluation of the
                     --  high bound of the slice, since it may underflow if the
                     --  low bound is Ityp'First.

                     Assign :=
                       Make_Implicit_If_Statement (Cnode,
                         Condition       =>
                           Make_Op_Ne (Loc,
                             Left_Opnd  =>
                               New_Occurrence_Of (Var_Length (J), Loc),
                             Right_Opnd => Make_Integer_Literal (Loc, 0)),
                         Then_Statements => New_List (Assign));
                  end if;

                  Insert_Action (Cnode, Assign, Suppress => All_Checks);
               end;
            end if;
         end;
      end loop;

      --  Finally we build the result, which is a reference to the array object

      Result := New_Occurrence_Of (Ent, Loc);

   <<Done>>
      Rewrite (Cnode, Result);
      Analyze_And_Resolve (Cnode, Atyp);

   exception
      when Concatenation_Error =>

         --  Kill warning generated for the declaration of the static out of
         --  range high bound, and instead generate a Constraint_Error with
         --  an appropriate specific message.

         Kill_Dead_Code (Declaration_Node (Entity (High_Bound)));
         Apply_Compile_Time_Constraint_Error
           (N      => Cnode,
            Msg    => "concatenation result upper bound out of range??",
            Reason => CE_Range_Check_Failed);
   end Expand_Concatenate;

   ---------------------------------------------------
   -- Expand_Membership_Minimize_Eliminate_Overflow --
   ---------------------------------------------------

   procedure Expand_Membership_Minimize_Eliminate_Overflow (N : Node_Id) is
      pragma Assert (Nkind (N) = N_In);
      --  Despite the name, this routine applies only to N_In, not to
      --  N_Not_In. The latter is always rewritten as not (X in Y).

      Result_Type : constant Entity_Id := Etype (N);
      --  Capture result type, may be a derived boolean type

      Loc : constant Source_Ptr := Sloc (N);
      Lop : constant Node_Id    := Left_Opnd (N);
      Rop : constant Node_Id    := Right_Opnd (N);

      --  Note: there are many referencs to Etype (Lop) and Etype (Rop). It
      --  is thus tempting to capture these values, but due to the rewrites
      --  that occur as a result of overflow checking, these values change
      --  as we go along, and it is safe just to always use Etype explicitly.

      Restype : constant Entity_Id := Etype (N);
      --  Save result type

      Lo, Hi : Uint;
      --  Bounds in Minimize calls, not used currently

      LLIB : constant Entity_Id := Base_Type (Standard_Long_Long_Integer);
      --  Entity for Long_Long_Integer'Base (Standard should export this???)

   begin
      Minimize_Eliminate_Overflows (Lop, Lo, Hi, Top_Level => False);

      --  If right operand is a subtype name, and the subtype name has no
      --  predicate, then we can just replace the right operand with an
      --  explicit range T'First .. T'Last, and use the explicit range code.

      if Nkind (Rop) /= N_Range
        and then No (Predicate_Function (Etype (Rop)))
      then
         declare
            Rtyp : constant Entity_Id := Etype (Rop);
         begin
            Rewrite (Rop,
              Make_Range (Loc,
                Low_Bound =>
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_First,
                    Prefix         => New_Occurrence_Of (Rtyp, Loc)),
                High_Bound =>
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Last,
                    Prefix         => New_Occurrence_Of (Rtyp, Loc))));
            Analyze_And_Resolve (Rop, Rtyp, Suppress => All_Checks);
         end;
      end if;

      --  Here for the explicit range case. Note that the bounds of the range
      --  have not been processed for minimized or eliminated checks.

      if Nkind (Rop) = N_Range then
         Minimize_Eliminate_Overflows
           (Low_Bound (Rop), Lo, Hi, Top_Level => False);
         Minimize_Eliminate_Overflows
           (High_Bound (Rop), Lo, Hi, Top_Level => False);

         --  We have A in B .. C, treated as  A >= B and then A <= C

         --  Bignum case

         if Is_RTE (Etype (Lop), RE_Bignum)
           or else Is_RTE (Etype (Low_Bound (Rop)), RE_Bignum)
           or else Is_RTE (Etype (High_Bound (Rop)), RE_Bignum)
         then
            declare
               Blk    : constant Node_Id   := Make_Bignum_Block (Loc);
               Bnn    : constant Entity_Id := Make_Temporary (Loc, 'B', N);
               L      : constant Entity_Id :=
                          Make_Defining_Identifier (Loc, Name_uL);
               Lopnd  : constant Node_Id   := Convert_To_Bignum (Lop);
               Lbound : constant Node_Id   :=
                          Convert_To_Bignum (Low_Bound (Rop));
               Hbound : constant Node_Id   :=
                          Convert_To_Bignum (High_Bound (Rop));

            --  Now we rewrite the membership test node to look like

            --    do
            --       Bnn : Result_Type;
            --       declare
            --          M : Mark_Id := SS_Mark;
            --          L : Bignum  := Lopnd;
            --       begin
            --          Bnn := Big_GE (L, Lbound) and then Big_LE (L, Hbound)
            --          SS_Release (M);
            --       end;
            --    in
            --       Bnn
            --    end

            begin
               --  Insert declaration of L into declarations of bignum block

               Insert_After
                 (Last (Declarations (Blk)),
                  Make_Object_Declaration (Loc,
                    Defining_Identifier => L,
                    Object_Definition   =>
                      New_Occurrence_Of (RTE (RE_Bignum), Loc),
                    Expression          => Lopnd));

               --  Insert assignment to Bnn into expressions of bignum block

               Insert_Before
                 (First (Statements (Handled_Statement_Sequence (Blk))),
                  Make_Assignment_Statement (Loc,
                    Name       => New_Occurrence_Of (Bnn, Loc),
                    Expression =>
                      Make_And_Then (Loc,
                        Left_Opnd =>
                          Make_Function_Call (Loc,
                            Name                   =>
                              New_Occurrence_Of (RTE (RE_Big_GE), Loc),
                            Parameter_Associations => New_List (
                              New_Occurrence_Of (L, Loc),
                              Lbound)),
                        Right_Opnd =>
                          Make_Function_Call (Loc,
                            Name                   =>
                              New_Occurrence_Of (RTE (RE_Big_LE), Loc),
                            Parameter_Associations => New_List (
                              New_Occurrence_Of (L, Loc),
                              Hbound)))));

               --  Now rewrite the node

               Rewrite (N,
                 Make_Expression_With_Actions (Loc,
                   Actions    => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Bnn,
                       Object_Definition   =>
                         New_Occurrence_Of (Result_Type, Loc)),
                     Blk),
                   Expression => New_Occurrence_Of (Bnn, Loc)));
               Analyze_And_Resolve (N, Result_Type);
               return;
            end;

         --  Here if no bignums around

         else
            --  Case where types are all the same

            if Base_Type (Etype (Lop)) = Base_Type (Etype (Low_Bound (Rop)))
                 and then
               Base_Type (Etype (Lop)) = Base_Type (Etype (High_Bound (Rop)))
            then
               null;

            --  If types are not all the same, it means that we have rewritten
            --  at least one of them to be of type Long_Long_Integer, and we
            --  will convert the other operands to Long_Long_Integer.

            else
               Convert_To_And_Rewrite (LLIB, Lop);
               Set_Analyzed (Lop, False);
               Analyze_And_Resolve (Lop, LLIB);

               --  For the right operand, avoid unnecessary recursion into
               --  this routine, we know that overflow is not possible.

               Convert_To_And_Rewrite (LLIB, Low_Bound (Rop));
               Convert_To_And_Rewrite (LLIB, High_Bound (Rop));
               Set_Analyzed (Rop, False);
               Analyze_And_Resolve (Rop, LLIB, Suppress => Overflow_Check);
            end if;

            --  Now the three operands are of the same signed integer type,
            --  so we can use the normal expansion routine for membership,
            --  setting the flag to prevent recursion into this procedure.

            Set_No_Minimize_Eliminate (N);
            Expand_N_In (N);
         end if;

      --  Right operand is a subtype name and the subtype has a predicate. We
      --  have to make sure the predicate is checked, and for that we need to
      --  use the standard N_In circuitry with appropriate types.

      else
         pragma Assert (Present (Predicate_Function (Etype (Rop))));

         --  If types are "right", just call Expand_N_In preventing recursion

         if Base_Type (Etype (Lop)) = Base_Type (Etype (Rop)) then
            Set_No_Minimize_Eliminate (N);
            Expand_N_In (N);

         --  Bignum case

         elsif Is_RTE (Etype (Lop), RE_Bignum) then

            --  For X in T, we want to rewrite our node as

            --    do
            --       Bnn : Result_Type;

            --       declare
            --          M   : Mark_Id := SS_Mark;
            --          Lnn : Long_Long_Integer'Base
            --          Nnn : Bignum;

            --       begin
            --         Nnn := X;

            --         if not Bignum_In_LLI_Range (Nnn) then
            --            Bnn := False;
            --         else
            --            Lnn := From_Bignum (Nnn);
            --            Bnn :=
            --              Lnn in LLIB (T'Base'First) .. LLIB (T'Base'Last)
            --                and then T'Base (Lnn) in T;
            --         end if;
            --
            --          SS_Release (M);
            --       end
            --   in
            --       Bnn
            --   end

            --  A bit gruesome, but there doesn't seem to be a simpler way

            declare
               Blk : constant Node_Id   := Make_Bignum_Block (Loc);
               Bnn : constant Entity_Id := Make_Temporary (Loc, 'B', N);
               Lnn : constant Entity_Id := Make_Temporary (Loc, 'L', N);
               Nnn : constant Entity_Id := Make_Temporary (Loc, 'N', N);
               T   : constant Entity_Id := Etype (Rop);
               TB  : constant Entity_Id := Base_Type (T);
               Nin : Node_Id;

            begin
               --  Mark the last membership operation to prevent recursion

               Nin :=
                 Make_In (Loc,
                   Left_Opnd  => Convert_To (TB, New_Occurrence_Of (Lnn, Loc)),
                   Right_Opnd => New_Occurrence_Of (T, Loc));
               Set_No_Minimize_Eliminate (Nin);

               --  Now decorate the block

               Insert_After
                 (Last (Declarations (Blk)),
                  Make_Object_Declaration (Loc,
                    Defining_Identifier => Lnn,
                    Object_Definition   => New_Occurrence_Of (LLIB, Loc)));

               Insert_After
                 (Last (Declarations (Blk)),
                  Make_Object_Declaration (Loc,
                    Defining_Identifier => Nnn,
                    Object_Definition   =>
                      New_Occurrence_Of (RTE (RE_Bignum), Loc)));

               Insert_List_Before
                 (First (Statements (Handled_Statement_Sequence (Blk))),
                  New_List (
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Nnn, Loc),
                      Expression => Relocate_Node (Lop)),

                    Make_Implicit_If_Statement (N,
                      Condition =>
                        Make_Op_Not (Loc,
                          Right_Opnd =>
                            Make_Function_Call (Loc,
                              Name                   =>
                                New_Occurrence_Of
                                  (RTE (RE_Bignum_In_LLI_Range), Loc),
                              Parameter_Associations => New_List (
                                New_Occurrence_Of (Nnn, Loc)))),

                      Then_Statements => New_List (
                        Make_Assignment_Statement (Loc,
                          Name       => New_Occurrence_Of (Bnn, Loc),
                          Expression =>
                            New_Occurrence_Of (Standard_False, Loc))),

                      Else_Statements => New_List (
                        Make_Assignment_Statement (Loc,
                          Name => New_Occurrence_Of (Lnn, Loc),
                          Expression =>
                            Make_Function_Call (Loc,
                              Name                   =>
                                New_Occurrence_Of (RTE (RE_From_Bignum), Loc),
                              Parameter_Associations => New_List (
                                  New_Occurrence_Of (Nnn, Loc)))),

                        Make_Assignment_Statement (Loc,
                          Name       => New_Occurrence_Of (Bnn, Loc),
                          Expression =>
                            Make_And_Then (Loc,
                              Left_Opnd  =>
                                Make_In (Loc,
                                  Left_Opnd  => New_Occurrence_Of (Lnn, Loc),
                                  Right_Opnd =>
                                    Make_Range (Loc,
                                      Low_Bound  =>
                                        Convert_To (LLIB,
                                          Make_Attribute_Reference (Loc,
                                            Attribute_Name => Name_First,
                                            Prefix         =>
                                              New_Occurrence_Of (TB, Loc))),

                                      High_Bound =>
                                        Convert_To (LLIB,
                                          Make_Attribute_Reference (Loc,
                                            Attribute_Name => Name_Last,
                                            Prefix         =>
                                              New_Occurrence_Of (TB, Loc))))),

                              Right_Opnd => Nin))))));

               --  Now we can do the rewrite

               Rewrite (N,
                 Make_Expression_With_Actions (Loc,
                   Actions    => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Bnn,
                       Object_Definition   =>
                         New_Occurrence_Of (Result_Type, Loc)),
                     Blk),
                   Expression => New_Occurrence_Of (Bnn, Loc)));
               Analyze_And_Resolve (N, Result_Type);
               return;
            end;

         --  Not bignum case, but types don't match (this means we rewrote the
         --  left operand to be Long_Long_Integer).

         else
            pragma Assert (Base_Type (Etype (Lop)) = LLIB);

            --  We rewrite the membership test as (where T is the type with
            --  the predicate, i.e. the type of the right operand)

            --    Lop in LLIB (T'Base'First) .. LLIB (T'Base'Last)
            --      and then T'Base (Lop) in T

            declare
               T   : constant Entity_Id := Etype (Rop);
               TB  : constant Entity_Id := Base_Type (T);
               Nin : Node_Id;

            begin
               --  The last membership test is marked to prevent recursion

               Nin :=
                 Make_In (Loc,
                   Left_Opnd  => Convert_To (TB, Duplicate_Subexpr (Lop)),
                   Right_Opnd => New_Occurrence_Of (T, Loc));
               Set_No_Minimize_Eliminate (Nin);

               --  Now do the rewrite

               Rewrite (N,
                 Make_And_Then (Loc,
                   Left_Opnd  =>
                     Make_In (Loc,
                       Left_Opnd  => Lop,
                       Right_Opnd =>
                         Make_Range (Loc,
                           Low_Bound  =>
                             Convert_To (LLIB,
                               Make_Attribute_Reference (Loc,
                                 Attribute_Name => Name_First,
                                 Prefix => New_Occurrence_Of (TB, Loc))),
                           High_Bound =>
                             Convert_To (LLIB,
                               Make_Attribute_Reference (Loc,
                                 Attribute_Name => Name_Last,
                                 Prefix => New_Occurrence_Of (TB, Loc))))),
                   Right_Opnd => Nin));
               Set_Analyzed (N, False);
               Analyze_And_Resolve (N, Restype);
            end;
         end if;
      end if;
   end Expand_Membership_Minimize_Eliminate_Overflow;

   ------------------------
   -- Expand_N_Allocator --
   ------------------------

   procedure Expand_N_Allocator (N : Node_Id) is
      Etyp : constant Entity_Id  := Etype (Expression (N));
      Loc  : constant Source_Ptr := Sloc (N);
      PtrT : constant Entity_Id  := Etype (N);

      procedure Rewrite_Coextension (N : Node_Id);
      --  Static coextensions have the same lifetime as the entity they
      --  constrain. Such occurrences can be rewritten as aliased objects
      --  and their unrestricted access used instead of the coextension.

      function Size_In_Storage_Elements (E : Entity_Id) return Node_Id;
      --  Given a constrained array type E, returns a node representing the
      --  code to compute the size in storage elements for the given type.
      --  This is done without using the attribute (which malfunctions for
      --  large sizes ???)

      -------------------------
      -- Rewrite_Coextension --
      -------------------------

      procedure Rewrite_Coextension (N : Node_Id) is
         Temp_Id   : constant Node_Id := Make_Temporary (Loc, 'C');
         Temp_Decl : Node_Id;

      begin
         --  Generate:
         --    Cnn : aliased Etyp;

         Temp_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp_Id,
             Aliased_Present     => True,
             Object_Definition   => New_Occurrence_Of (Etyp, Loc));

         if Nkind (Expression (N)) = N_Qualified_Expression then
            Set_Expression (Temp_Decl, Expression (Expression (N)));
         end if;

         Insert_Action (N, Temp_Decl);
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Temp_Id, Loc),
             Attribute_Name => Name_Unrestricted_Access));

         Analyze_And_Resolve (N, PtrT);
      end Rewrite_Coextension;

      ------------------------------
      -- Size_In_Storage_Elements --
      ------------------------------

      function Size_In_Storage_Elements (E : Entity_Id) return Node_Id is
      begin
         --  Logically this just returns E'Max_Size_In_Storage_Elements.
         --  However, the reason for the existence of this function is
         --  to construct a test for sizes too large, which means near the
         --  32-bit limit on a 32-bit machine, and precisely the trouble
         --  is that we get overflows when sizes are greater than 2**31.

         --  So what we end up doing for array types is to use the expression:

         --    number-of-elements * component_type'Max_Size_In_Storage_Elements

         --  which avoids this problem. All this is a bit bogus, but it does
         --  mean we catch common cases of trying to allocate arrays that
         --  are too large, and which in the absence of a check results in
         --  undetected chaos ???

         --  Note in particular that this is a pessimistic estimate in the
         --  case of packed array types, where an array element might occupy
         --  just a fraction of a storage element???

         declare
            Len : Node_Id;
            Res : Node_Id;

         begin
            for J in 1 .. Number_Dimensions (E) loop
               Len :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (E, Loc),
                   Attribute_Name => Name_Length,
                   Expressions    => New_List (Make_Integer_Literal (Loc, J)));

               if J = 1 then
                  Res := Len;

               else
                  Res :=
                    Make_Op_Multiply (Loc,
                      Left_Opnd  => Res,
                      Right_Opnd => Len);
               end if;
            end loop;

            return
              Make_Op_Multiply (Loc,
                Left_Opnd  => Len,
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (Component_Type (E), Loc),
                    Attribute_Name => Name_Max_Size_In_Storage_Elements));
         end;
      end Size_In_Storage_Elements;

      --  Local variables

      Dtyp    : constant Entity_Id := Available_View (Designated_Type (PtrT));
      Desig   : Entity_Id;
      Nod     : Node_Id;
      Pool    : Entity_Id;
      Rel_Typ : Entity_Id;
      Temp    : Entity_Id;

   --  Start of processing for Expand_N_Allocator

   begin
      --  RM E.2.3(22). We enforce that the expected type of an allocator
      --  shall not be a remote access-to-class-wide-limited-private type

      --  Why is this being done at expansion time, seems clearly wrong ???

      Validate_Remote_Access_To_Class_Wide_Type (N);

      --  Processing for anonymous access-to-controlled types. These access
      --  types receive a special finalization master which appears in the
      --  declarations of the enclosing semantic unit. This expansion is done
      --  now to ensure that any additional types generated by this routine or
      --  Expand_Allocator_Expression inherit the proper type attributes.

      if (Ekind (PtrT) = E_Anonymous_Access_Type
           or else (Is_Itype (PtrT) and then No (Finalization_Master (PtrT))))
        and then Needs_Finalization (Dtyp)
      then
         --  Detect the allocation of an anonymous controlled object where the
         --  type of the context is named. For example:

         --     procedure Proc (Ptr : Named_Access_Typ);
         --     Proc (new Designated_Typ);

         --  Regardless of the anonymous-to-named access type conversion, the
         --  lifetime of the object must be associated with the named access
         --  type. Use the finalization-related attributes of this type.

         if Nkind_In (Parent (N), N_Type_Conversion,
                                  N_Unchecked_Type_Conversion)
           and then Ekind_In (Etype (Parent (N)), E_Access_Subtype,
                                                  E_Access_Type,
                                                  E_General_Access_Type)
         then
            Rel_Typ := Etype (Parent (N));
         else
            Rel_Typ := Empty;
         end if;

         --  Anonymous access-to-controlled types allocate on the global pool.
         --  Do not set this attribute on .NET/JVM since those targets do not
         --  support pools.

         if No (Associated_Storage_Pool (PtrT)) and then VM_Target = No_VM then
            if Present (Rel_Typ) then
               Set_Associated_Storage_Pool (PtrT,
                 Associated_Storage_Pool (Rel_Typ));
            else
               Set_Associated_Storage_Pool (PtrT,
                 Get_Global_Pool_For_Access_Type (PtrT));
            end if;
         end if;

         --  The finalization master must be inserted and analyzed as part of
         --  the current semantic unit. Note that the master is updated when
         --  analysis changes current units.

         if Present (Rel_Typ) then
            Set_Finalization_Master (PtrT, Finalization_Master (Rel_Typ));
         else
            Set_Finalization_Master (PtrT, Current_Anonymous_Master);
         end if;
      end if;

      --  Set the storage pool and find the appropriate version of Allocate to
      --  call. Do not overwrite the storage pool if it is already set, which
      --  can happen for build-in-place function returns (see
      --  Exp_Ch4.Expand_N_Extended_Return_Statement).

      if No (Storage_Pool (N)) then
         Pool := Associated_Storage_Pool (Root_Type (PtrT));

         if Present (Pool) then
            Set_Storage_Pool (N, Pool);

            if Is_RTE (Pool, RE_SS_Pool) then
               if VM_Target = No_VM then
                  Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
               end if;

            --  In the case of an allocator for a simple storage pool, locate
            --  and save a reference to the pool type's Allocate routine.

            elsif Present (Get_Rep_Pragma
                             (Etype (Pool), Name_Simple_Storage_Pool_Type))
            then
               declare
                  Pool_Type : constant Entity_Id := Base_Type (Etype (Pool));
                  Alloc_Op  : Entity_Id;
               begin
                  Alloc_Op := Get_Name_Entity_Id (Name_Allocate);
                  while Present (Alloc_Op) loop
                     if Scope (Alloc_Op) = Scope (Pool_Type)
                       and then Present (First_Formal (Alloc_Op))
                       and then Etype (First_Formal (Alloc_Op)) = Pool_Type
                     then
                        Set_Procedure_To_Call (N, Alloc_Op);
                        exit;
                     else
                        Alloc_Op := Homonym (Alloc_Op);
                     end if;
                  end loop;
               end;

            elsif Is_Class_Wide_Type (Etype (Pool)) then
               Set_Procedure_To_Call (N, RTE (RE_Allocate_Any));

            else
               Set_Procedure_To_Call (N,
                 Find_Prim_Op (Etype (Pool), Name_Allocate));
            end if;
         end if;
      end if;

      --  Under certain circumstances we can replace an allocator by an access
      --  to statically allocated storage. The conditions, as noted in AARM
      --  3.10 (10c) are as follows:

      --    Size and initial value is known at compile time
      --    Access type is access-to-constant

      --  The allocator is not part of a constraint on a record component,
      --  because in that case the inserted actions are delayed until the
      --  record declaration is fully analyzed, which is too late for the
      --  analysis of the rewritten allocator.

      if Is_Access_Constant (PtrT)
        and then Nkind (Expression (N)) = N_Qualified_Expression
        and then Compile_Time_Known_Value (Expression (Expression (N)))
        and then Size_Known_At_Compile_Time
                   (Etype (Expression (Expression (N))))
        and then not Is_Record_Type (Current_Scope)
      then
         --  Here we can do the optimization. For the allocator

         --    new x'(y)

         --  We insert an object declaration

         --    Tnn : aliased x := y;

         --  and replace the allocator by Tnn'Unrestricted_Access. Tnn is
         --  marked as requiring static allocation.

         Temp  := Make_Temporary (Loc, 'T', Expression (Expression (N)));
         Desig := Subtype_Mark (Expression (N));

         --  If context is constrained, use constrained subtype directly,
         --  so that the constant is not labelled as having a nominally
         --  unconstrained subtype.

         if Entity (Desig) = Base_Type (Dtyp) then
            Desig := New_Occurrence_Of (Dtyp, Loc);
         end if;

         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Aliased_Present     => True,
             Constant_Present    => Is_Access_Constant (PtrT),
             Object_Definition   => Desig,
             Expression          => Expression (Expression (N))));

         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Temp, Loc),
             Attribute_Name => Name_Unrestricted_Access));

         Analyze_And_Resolve (N, PtrT);

         --  We set the variable as statically allocated, since we don't want
         --  it going on the stack of the current procedure.

         Set_Is_Statically_Allocated (Temp);
         return;
      end if;

      --  Same if the allocator is an access discriminant for a local object:
      --  instead of an allocator we create a local value and constrain the
      --  enclosing object with the corresponding access attribute.

      if Is_Static_Coextension (N) then
         Rewrite_Coextension (N);
         return;
      end if;

      --  Check for size too large, we do this because the back end misses
      --  proper checks here and can generate rubbish allocation calls when
      --  we are near the limit. We only do this for the 32-bit address case
      --  since that is from a practical point of view where we see a problem.

      if System_Address_Size = 32
        and then not Storage_Checks_Suppressed (PtrT)
        and then not Storage_Checks_Suppressed (Dtyp)
        and then not Storage_Checks_Suppressed (Etyp)
      then
         --  The check we want to generate should look like

         --  if Etyp'Max_Size_In_Storage_Elements > 3.5 gigabytes then
         --    raise Storage_Error;
         --  end if;

         --  where 3.5 gigabytes is a constant large enough to accommodate any
         --  reasonable request for. But we can't do it this way because at
         --  least at the moment we don't compute this attribute right, and
         --  can silently give wrong results when the result gets large. Since
         --  this is all about large results, that's bad, so instead we only
         --  apply the check for constrained arrays, and manually compute the
         --  value of the attribute ???

         if Is_Array_Type (Etyp) and then Is_Constrained (Etyp) then
            Insert_Action (N,
              Make_Raise_Storage_Error (Loc,
                Condition =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Size_In_Storage_Elements (Etyp),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc, Uint_7 * (Uint_2 ** 29))),
                Reason    => SE_Object_Too_Large));
         end if;
      end if;

      --  Handle case of qualified expression (other than optimization above)
      --  First apply constraint checks, because the bounds or discriminants
      --  in the aggregate might not match the subtype mark in the allocator.

      if Nkind (Expression (N)) = N_Qualified_Expression then
         Apply_Constraint_Check
           (Expression (Expression (N)), Etype (Expression (N)));

         Expand_Allocator_Expression (N);
         return;
      end if;

      --  If the allocator is for a type which requires initialization, and
      --  there is no initial value (i.e. operand is a subtype indication
      --  rather than a qualified expression), then we must generate a call to
      --  the initialization routine using an expressions action node:

      --     [Pnnn : constant ptr_T := new (T); Init (Pnnn.all,...); Pnnn]

      --  Here ptr_T is the pointer type for the allocator, and T is the
      --  subtype of the allocator. A special case arises if the designated
      --  type of the access type is a task or contains tasks. In this case
      --  the call to Init (Temp.all ...) is replaced by code that ensures
      --  that tasks get activated (see Exp_Ch9.Build_Task_Allocate_Block
      --  for details). In addition, if the type T is a task T, then the
      --  first argument to Init must be converted to the task record type.

      declare
         T         : constant Entity_Id := Entity (Expression (N));
         Args      : List_Id;
         Decls     : List_Id;
         Decl      : Node_Id;
         Discr     : Elmt_Id;
         Init      : Entity_Id;
         Init_Arg1 : Node_Id;
         Temp_Decl : Node_Id;
         Temp_Type : Entity_Id;

      begin
         if No_Initialization (N) then

            --  Even though this might be a simple allocation, create a custom
            --  Allocate if the context requires it. Since .NET/JVM compilers
            --  do not support pools, this step is skipped.

            if VM_Target = No_VM
              and then Present (Finalization_Master (PtrT))
            then
               Build_Allocate_Deallocate_Proc
                 (N           => N,
                  Is_Allocate => True);
            end if;

         --  Case of no initialization procedure present

         elsif not Has_Non_Null_Base_Init_Proc (T) then

            --  Case of simple initialization required

            if Needs_Simple_Initialization (T) then
               Check_Restriction (No_Default_Initialization, N);
               Rewrite (Expression (N),
                 Make_Qualified_Expression (Loc,
                   Subtype_Mark => New_Occurrence_Of (T, Loc),
                   Expression   => Get_Simple_Init_Val (T, N)));

               Analyze_And_Resolve (Expression (Expression (N)), T);
               Analyze_And_Resolve (Expression (N), T);
               Set_Paren_Count     (Expression (Expression (N)), 1);
               Expand_N_Allocator  (N);

            --  No initialization required

            else
               null;
            end if;

         --  Case of initialization procedure present, must be called

         else
            Check_Restriction (No_Default_Initialization, N);

            if not Restriction_Active (No_Default_Initialization) then
               Init := Base_Init_Proc (T);
               Nod  := N;
               Temp := Make_Temporary (Loc, 'P');

               --  Construct argument list for the initialization routine call

               Init_Arg1 :=
                 Make_Explicit_Dereference (Loc,
                   Prefix =>
                     New_Occurrence_Of (Temp, Loc));

               Set_Assignment_OK (Init_Arg1);
               Temp_Type := PtrT;

               --  The initialization procedure expects a specific type. if the
               --  context is access to class wide, indicate that the object
               --  being allocated has the right specific type.

               if Is_Class_Wide_Type (Dtyp) then
                  Init_Arg1 := Unchecked_Convert_To (T, Init_Arg1);
               end if;

               --  If designated type is a concurrent type or if it is private
               --  type whose definition is a concurrent type, the first
               --  argument in the Init routine has to be unchecked conversion
               --  to the corresponding record type. If the designated type is
               --  a derived type, also convert the argument to its root type.

               if Is_Concurrent_Type (T) then
                  Init_Arg1 :=
                    Unchecked_Convert_To (
                      Corresponding_Record_Type (T), Init_Arg1);

               elsif Is_Private_Type (T)
                 and then Present (Full_View (T))
                 and then Is_Concurrent_Type (Full_View (T))
               then
                  Init_Arg1 :=
                    Unchecked_Convert_To
                      (Corresponding_Record_Type (Full_View (T)), Init_Arg1);

               elsif Etype (First_Formal (Init)) /= Base_Type (T) then
                  declare
                     Ftyp : constant Entity_Id := Etype (First_Formal (Init));

                  begin
                     Init_Arg1 := OK_Convert_To (Etype (Ftyp), Init_Arg1);
                     Set_Etype (Init_Arg1, Ftyp);
                  end;
               end if;

               Args := New_List (Init_Arg1);

               --  For the task case, pass the Master_Id of the access type as
               --  the value of the _Master parameter, and _Chain as the value
               --  of the _Chain parameter (_Chain will be defined as part of
               --  the generated code for the allocator).

               --  In Ada 2005, the context may be a function that returns an
               --  anonymous access type. In that case the Master_Id has been
               --  created when expanding the function declaration.

               if Has_Task (T) then
                  if No (Master_Id (Base_Type (PtrT))) then

                     --  The designated type was an incomplete type, and the
                     --  access type did not get expanded. Salvage it now.

                     if not Restriction_Active (No_Task_Hierarchy) then
                        if Present (Parent (Base_Type (PtrT))) then
                           Expand_N_Full_Type_Declaration
                             (Parent (Base_Type (PtrT)));

                        --  The only other possibility is an itype. For this
                        --  case, the master must exist in the context. This is
                        --  the case when the allocator initializes an access
                        --  component in an init-proc.

                        else
                           pragma Assert (Is_Itype (PtrT));
                           Build_Master_Renaming (PtrT, N);
                        end if;
                     end if;
                  end if;

                  --  If the context of the allocator is a declaration or an
                  --  assignment, we can generate a meaningful image for it,
                  --  even though subsequent assignments might remove the
                  --  connection between task and entity. We build this image
                  --  when the left-hand side is a simple variable, a simple
                  --  indexed assignment or a simple selected component.

                  if Nkind (Parent (N)) = N_Assignment_Statement then
                     declare
                        Nam : constant Node_Id := Name (Parent (N));

                     begin
                        if Is_Entity_Name (Nam) then
                           Decls :=
                             Build_Task_Image_Decls
                               (Loc,
                                New_Occurrence_Of
                                  (Entity (Nam), Sloc (Nam)), T);

                        elsif Nkind_In (Nam, N_Indexed_Component,
                                             N_Selected_Component)
                          and then Is_Entity_Name (Prefix (Nam))
                        then
                           Decls :=
                             Build_Task_Image_Decls
                               (Loc, Nam, Etype (Prefix (Nam)));
                        else
                           Decls := Build_Task_Image_Decls (Loc, T, T);
                        end if;
                     end;

                  elsif Nkind (Parent (N)) = N_Object_Declaration then
                     Decls :=
                       Build_Task_Image_Decls
                         (Loc, Defining_Identifier (Parent (N)), T);

                  else
                     Decls := Build_Task_Image_Decls (Loc, T, T);
                  end if;

                  if Restriction_Active (No_Task_Hierarchy) then
                     Append_To (Args,
                       New_Occurrence_Of (RTE (RE_Library_Task_Level), Loc));
                  else
                     Append_To (Args,
                       New_Occurrence_Of
                         (Master_Id (Base_Type (Root_Type (PtrT))), Loc));
                  end if;

                  Append_To (Args, Make_Identifier (Loc, Name_uChain));

                  Decl := Last (Decls);
                  Append_To (Args,
                    New_Occurrence_Of (Defining_Identifier (Decl), Loc));

               --  Has_Task is false, Decls not used

               else
                  Decls := No_List;
               end if;

               --  Add discriminants if discriminated type

               declare
                  Dis : Boolean := False;
                  Typ : Entity_Id;

               begin
                  if Has_Discriminants (T) then
                     Dis := True;
                     Typ := T;

                  elsif Is_Private_Type (T)
                    and then Present (Full_View (T))
                    and then Has_Discriminants (Full_View (T))
                  then
                     Dis := True;
                     Typ := Full_View (T);
                  end if;

                  if Dis then

                     --  If the allocated object will be constrained by the
                     --  default values for discriminants, then build a subtype
                     --  with those defaults, and change the allocated subtype
                     --  to that. Note that this happens in fewer cases in Ada
                     --  2005 (AI-363).

                     if not Is_Constrained (Typ)
                       and then Present (Discriminant_Default_Value
                                          (First_Discriminant (Typ)))
                       and then (Ada_Version < Ada_2005
                                  or else not
                                    Object_Type_Has_Constrained_Partial_View
                                      (Typ, Current_Scope))
                     then
                        Typ := Build_Default_Subtype (Typ, N);
                        Set_Expression (N, New_Occurrence_Of (Typ, Loc));
                     end if;

                     Discr := First_Elmt (Discriminant_Constraint (Typ));
                     while Present (Discr) loop
                        Nod := Node (Discr);
                        Append (New_Copy_Tree (Node (Discr)), Args);

                        --  AI-416: when the discriminant constraint is an
                        --  anonymous access type make sure an accessibility
                        --  check is inserted if necessary (3.10.2(22.q/2))

                        if Ada_Version >= Ada_2005
                          and then
                            Ekind (Etype (Nod)) = E_Anonymous_Access_Type
                        then
                           Apply_Accessibility_Check
                             (Nod, Typ, Insert_Node => Nod);
                        end if;

                        Next_Elmt (Discr);
                     end loop;
                  end if;
               end;

               --  We set the allocator as analyzed so that when we analyze
               --  the if expression node, we do not get an unwanted recursive
               --  expansion of the allocator expression.

               Set_Analyzed (N, True);
               Nod := Relocate_Node (N);

               --  Here is the transformation:
               --    input:  new Ctrl_Typ
               --    output: Temp : constant Ctrl_Typ_Ptr := new Ctrl_Typ;
               --            Ctrl_TypIP (Temp.all, ...);
               --            [Deep_]Initialize (Temp.all);

               --  Here Ctrl_Typ_Ptr is the pointer type for the allocator, and
               --  is the subtype of the allocator.

               Temp_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Temp_Type, Loc),
                   Expression          => Nod);

               Set_Assignment_OK (Temp_Decl);
               Insert_Action (N, Temp_Decl, Suppress => All_Checks);

               Build_Allocate_Deallocate_Proc (Temp_Decl, True);

               --  If the designated type is a task type or contains tasks,
               --  create block to activate created tasks, and insert
               --  declaration for Task_Image variable ahead of call.

               if Has_Task (T) then
                  declare
                     L   : constant List_Id := New_List;
                     Blk : Node_Id;
                  begin
                     Build_Task_Allocate_Block (L, Nod, Args);
                     Blk := Last (L);
                     Insert_List_Before (First (Declarations (Blk)), Decls);
                     Insert_Actions (N, L);
                  end;

               else
                  Insert_Action (N,
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => New_Occurrence_Of (Init, Loc),
                      Parameter_Associations => Args));
               end if;

               if Needs_Finalization (T) then

                  --  Generate:
                  --    [Deep_]Initialize (Init_Arg1);

                  Insert_Action (N,
                    Make_Init_Call
                      (Obj_Ref => New_Copy_Tree (Init_Arg1),
                       Typ     => T));

                  if Present (Finalization_Master (PtrT)) then

                     --  Special processing for .NET/JVM, the allocated object
                     --  is attached to the finalization master. Generate:

                     --    Attach (<PtrT>FM, Root_Controlled_Ptr (Init_Arg1));

                     --  Types derived from [Limited_]Controlled are the only
                     --  ones considered since they have fields Prev and Next.

                     if VM_Target /= No_VM then
                        if Is_Controlled (T) then
                           Insert_Action (N,
                             Make_Attach_Call
                               (Obj_Ref => New_Copy_Tree (Init_Arg1),
                                Ptr_Typ => PtrT));
                        end if;

                     --  Default case, generate:

                     --    Set_Finalize_Address
                     --      (<PtrT>FM, <T>FD'Unrestricted_Access);

                     --  Do not generate this call in CodePeer mode, as TSS
                     --  primitive Finalize_Address is not created in this
                     --  mode.

                     elsif not CodePeer_Mode then
                        Insert_Action (N,
                          Make_Set_Finalize_Address_Call
                            (Loc     => Loc,
                             Typ     => T,
                             Ptr_Typ => PtrT));
                     end if;
                  end if;
               end if;

               Rewrite (N, New_Occurrence_Of (Temp, Loc));
               Analyze_And_Resolve (N, PtrT);
            end if;
         end if;
      end;

      --  Ada 2005 (AI-251): If the allocator is for a class-wide interface
      --  object that has been rewritten as a reference, we displace "this"
      --  to reference properly its secondary dispatch table.

      if Nkind (N) = N_Identifier and then Is_Interface (Dtyp) then
         Displace_Allocator_Pointer (N);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Allocator;

   -----------------------
   -- Expand_N_And_Then --
   -----------------------

   procedure Expand_N_And_Then (N : Node_Id)
     renames Expand_Short_Circuit_Operator;

   ------------------------------
   -- Expand_N_Case_Expression --
   ------------------------------

   procedure Expand_N_Case_Expression (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Cstmt   : Node_Id;
      Decl    : Node_Id;
      Tnn     : Entity_Id;
      Pnn     : Entity_Id;
      Actions : List_Id;
      Ttyp    : Entity_Id;
      Alt     : Node_Id;
      Fexp    : Node_Id;

   begin
      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  We expand

      --    case X is when A => AX, when B => BX ...

      --  to

      --    do
      --       Tnn : typ;
      --       case X is
      --          when A =>
      --             Tnn := AX;
      --          when B =>
      --             Tnn := BX;
      --          ...
      --       end case;
      --    in Tnn end;

      --  However, this expansion is wrong for limited types, and also
      --  wrong for unconstrained types (since the bounds may not be the
      --  same in all branches). Furthermore it involves an extra copy
      --  for large objects. So we take care of this by using the following
      --  modified expansion for non-elementary types:

      --    do
      --       type Pnn is access all typ;
      --       Tnn : Pnn;
      --       case X is
      --          when A =>
      --             T := AX'Unrestricted_Access;
      --          when B =>
      --             T := BX'Unrestricted_Access;
      --          ...
      --       end case;
      --    in Tnn.all end;

      Cstmt :=
        Make_Case_Statement (Loc,
          Expression   => Expression (N),
          Alternatives => New_List);

      Actions := New_List;

      --  Scalar case

      if Is_Elementary_Type (Typ) then
         Ttyp := Typ;

      else
         Pnn := Make_Temporary (Loc, 'P');
         Append_To (Actions,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Pnn,
             Type_Definition     =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present        => True,
                 Subtype_Indication => New_Occurrence_Of (Typ, Loc))));
         Ttyp := Pnn;
      end if;

      Tnn := Make_Temporary (Loc, 'T');

      --  Create declaration for target of expression, and indicate that it
      --  does not require initialization.

      Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Tnn,
          Object_Definition   => New_Occurrence_Of (Ttyp, Loc));
      Set_No_Initialization (Decl);
      Append_To (Actions, Decl);

      --  Now process the alternatives

      Alt := First (Alternatives (N));
      while Present (Alt) loop
         declare
            Aexp  : Node_Id             := Expression (Alt);
            Aloc  : constant Source_Ptr := Sloc (Aexp);
            Stats : List_Id;

         begin
            --  As described above, take Unrestricted_Access for case of non-
            --  scalar types, to avoid big copies, and special cases.

            if not Is_Elementary_Type (Typ) then
               Aexp :=
                 Make_Attribute_Reference (Aloc,
                   Prefix         => Relocate_Node (Aexp),
                   Attribute_Name => Name_Unrestricted_Access);
            end if;

            Stats := New_List (
              Make_Assignment_Statement (Aloc,
                Name       => New_Occurrence_Of (Tnn, Loc),
                Expression => Aexp));

            --  Propagate declarations inserted in the node by Insert_Actions
            --  (for example, temporaries generated to remove side effects).
            --  These actions must remain attached to the alternative, given
            --  that they are generated by the corresponding expression.

            if Present (Sinfo.Actions (Alt)) then
               Prepend_List (Sinfo.Actions (Alt), Stats);
            end if;

            Append_To
              (Alternatives (Cstmt),
               Make_Case_Statement_Alternative (Sloc (Alt),
                 Discrete_Choices => Discrete_Choices (Alt),
                 Statements       => Stats));
         end;

         Next (Alt);
      end loop;

      Append_To (Actions, Cstmt);

      --  Construct and return final expression with actions

      if Is_Elementary_Type (Typ) then
         Fexp := New_Occurrence_Of (Tnn, Loc);
      else
         Fexp :=
           Make_Explicit_Dereference (Loc,
             Prefix => New_Occurrence_Of (Tnn, Loc));
      end if;

      Rewrite (N,
        Make_Expression_With_Actions (Loc,
          Expression => Fexp,
          Actions    => Actions));

      Analyze_And_Resolve (N, Typ);
   end Expand_N_Case_Expression;

   -----------------------------------
   -- Expand_N_Explicit_Dereference --
   -----------------------------------

   procedure Expand_N_Explicit_Dereference (N : Node_Id) is
   begin
      --  Insert explicit dereference call for the checked storage pool case

      Insert_Dereference_Action (Prefix (N));

      --  If the type is an Atomic type for which Atomic_Sync is enabled, then
      --  we set the atomic sync flag.

      if Is_Atomic (Etype (N))
        and then not Atomic_Synchronization_Disabled (Etype (N))
      then
         Activate_Atomic_Synchronization (N);
      end if;
   end Expand_N_Explicit_Dereference;

   --------------------------------------
   -- Expand_N_Expression_With_Actions --
   --------------------------------------

   procedure Expand_N_Expression_With_Actions (N : Node_Id) is

      function Process_Action (Act : Node_Id) return Traverse_Result;
      --  Inspect and process a single action of an expression_with_actions for
      --  transient controlled objects. If such objects are found, the routine
      --  generates code to clean them up when the context of the expression is
      --  evaluated or elaborated.

      --------------------
      -- Process_Action --
      --------------------

      function Process_Action (Act : Node_Id) return Traverse_Result is
      begin
         if Nkind (Act) = N_Object_Declaration
           and then Is_Finalizable_Transient (Act, N)
         then
            Process_Transient_Object (Act, N);
            return Abandon;

         --  Avoid processing temporary function results multiple times when
         --  dealing with nested expression_with_actions.

         elsif Nkind (Act) = N_Expression_With_Actions then
            return Abandon;

         --  Do not process temporary function results in loops. This is done
         --  by Expand_N_Loop_Statement and Build_Finalizer.

         elsif Nkind (Act) = N_Loop_Statement then
            return Abandon;
         end if;

         return OK;
      end Process_Action;

      procedure Process_Single_Action is new Traverse_Proc (Process_Action);

      --  Local variables

      Act : Node_Id;

   --  Start of processing for Expand_N_Expression_With_Actions

   begin
      --  Process the actions as described above

      Act := First (Actions (N));
      while Present (Act) loop
         Process_Single_Action (Act);
         Next (Act);
      end loop;

      --  Deal with case where there are no actions. In this case we simply
      --  rewrite the node with its expression since we don't need the actions
      --  and the specification of this node does not allow a null action list.

      --  Note: we use Rewrite instead of Replace, because Codepeer is using
      --  the expanded tree and relying on being able to retrieve the original
      --  tree in cases like this. This raises a whole lot of issues of whether
      --  we have problems elsewhere, which will be addressed in the future???

      if Is_Empty_List (Actions (N)) then
         Rewrite (N, Relocate_Node (Expression (N)));
      end if;
   end Expand_N_Expression_With_Actions;

   ----------------------------
   -- Expand_N_If_Expression --
   ----------------------------

   --  Deal with limited types and condition actions

   procedure Expand_N_If_Expression (N : Node_Id) is
      procedure Process_Actions (Actions : List_Id);
      --  Inspect and process a single action list of an if expression for
      --  transient controlled objects. If such objects are found, the routine
      --  generates code to clean them up when the context of the expression is
      --  evaluated or elaborated.

      ---------------------
      -- Process_Actions --
      ---------------------

      procedure Process_Actions (Actions : List_Id) is
         Act : Node_Id;

      begin
         Act := First (Actions);
         while Present (Act) loop
            if Nkind (Act) = N_Object_Declaration
              and then Is_Finalizable_Transient (Act, N)
            then
               Process_Transient_Object (Act, N);
            end if;

            Next (Act);
         end loop;
      end Process_Actions;

      --  Local variables

      Loc    : constant Source_Ptr := Sloc (N);
      Cond   : constant Node_Id    := First (Expressions (N));
      Thenx  : constant Node_Id    := Next (Cond);
      Elsex  : constant Node_Id    := Next (Thenx);
      Typ    : constant Entity_Id  := Etype (N);

      Actions : List_Id;
      Cnn     : Entity_Id;
      Decl    : Node_Id;
      Expr    : Node_Id;
      New_If  : Node_Id;
      New_N   : Node_Id;
      Ptr_Typ : Entity_Id;

   --  Start of processing for Expand_N_If_Expression

   begin
      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Fold at compile time if condition known. We have already folded
      --  static if expressions, but it is possible to fold any case in which
      --  the condition is known at compile time, even though the result is
      --  non-static.

      --  Note that we don't do the fold of such cases in Sem_Elab because
      --  it can cause infinite loops with the expander adding a conditional
      --  expression, and Sem_Elab circuitry removing it repeatedly.

      if Compile_Time_Known_Value (Cond) then
         if Is_True (Expr_Value (Cond)) then
            Expr := Thenx;
            Actions := Then_Actions (N);
         else
            Expr := Elsex;
            Actions := Else_Actions (N);
         end if;

         Remove (Expr);

         if Present (Actions) then
            Rewrite (N,
              Make_Expression_With_Actions (Loc,
                Expression => Relocate_Node (Expr),
                Actions    => Actions));
            Analyze_And_Resolve (N, Typ);
         else
            Rewrite (N, Relocate_Node (Expr));
         end if;

         --  Note that the result is never static (legitimate cases of static
         --  if expressions were folded in Sem_Eval).

         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  If the type is limited or unconstrained, we expand as follows to
      --  avoid any possibility of improper copies.

      --  Note: it may be possible to avoid this special processing if the
      --  back end uses its own mechanisms for handling by-reference types ???

      --      type Ptr is access all Typ;
      --      Cnn : Ptr;
      --      if cond then
      --         <<then actions>>
      --         Cnn := then-expr'Unrestricted_Access;
      --      else
      --         <<else actions>>
      --         Cnn := else-expr'Unrestricted_Access;
      --      end if;

      --  and replace the if expression by a reference to Cnn.all.

      --  This special case can be skipped if the back end handles limited
      --  types properly and ensures that no incorrect copies are made.

      if Is_By_Reference_Type (Typ)
        and then not Back_End_Handles_Limited_Types
      then
         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Actions (Then_Actions (N));
         Process_Actions (Else_Actions (N));

         --  Generate:
         --    type Ann is access all Typ;

         Ptr_Typ := Make_Temporary (Loc, 'A');

         Insert_Action (N,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Ptr_Typ,
             Type_Definition     =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present        => True,
                 Subtype_Indication => New_Occurrence_Of (Typ, Loc))));

         --  Generate:
         --    Cnn : Ann;

         Cnn := Make_Temporary (Loc, 'C', N);

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Cnn,
             Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc));

         --  Generate:
         --    if Cond then
         --       Cnn := <Thenx>'Unrestricted_Access;
         --    else
         --       Cnn := <Elsex>'Unrestricted_Access;
         --    end if;

         New_If :=
           Make_Implicit_If_Statement (N,
             Condition       => Relocate_Node (Cond),
             Then_Statements => New_List (
               Make_Assignment_Statement (Sloc (Thenx),
                 Name       => New_Occurrence_Of (Cnn, Sloc (Thenx)),
                 Expression =>
                   Make_Attribute_Reference (Loc,
                     Prefix         => Relocate_Node (Thenx),
                     Attribute_Name => Name_Unrestricted_Access))),

             Else_Statements => New_List (
               Make_Assignment_Statement (Sloc (Elsex),
                 Name       => New_Occurrence_Of (Cnn, Sloc (Elsex)),
                 Expression =>
                   Make_Attribute_Reference (Loc,
                     Prefix         => Relocate_Node (Elsex),
                     Attribute_Name => Name_Unrestricted_Access))));

            New_N :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Cnn, Loc));

      --  For other types, we only need to expand if there are other actions
      --  associated with either branch.

      elsif Present (Then_Actions (N)) or else Present (Else_Actions (N)) then

         --  We now wrap the actions into the appropriate expression

         if Present (Then_Actions (N)) then
            Rewrite (Thenx,
              Make_Expression_With_Actions (Sloc (Thenx),
                Actions    => Then_Actions (N),
                Expression => Relocate_Node (Thenx)));

            Set_Then_Actions (N, No_List);
            Analyze_And_Resolve (Thenx, Typ);
         end if;

         if Present (Else_Actions (N)) then
            Rewrite (Elsex,
              Make_Expression_With_Actions (Sloc (Elsex),
                Actions    => Else_Actions (N),
                Expression => Relocate_Node (Elsex)));

            Set_Else_Actions (N, No_List);
            Analyze_And_Resolve (Elsex, Typ);
         end if;

         return;

      --  If no actions then no expansion needed, gigi will handle it using the
      --  same approach as a C conditional expression.

      else
         return;
      end if;

      --  Fall through here for either the limited expansion, or the case of
      --  inserting actions for non-limited types. In both these cases, we must
      --  move the SLOC of the parent If statement to the newly created one and
      --  change it to the SLOC of the expression which, after expansion, will
      --  correspond to what is being evaluated.

      if Present (Parent (N)) and then Nkind (Parent (N)) = N_If_Statement then
         Set_Sloc (New_If, Sloc (Parent (N)));
         Set_Sloc (Parent (N), Loc);
      end if;

      --  Make sure Then_Actions and Else_Actions are appropriately moved
      --  to the new if statement.

      if Present (Then_Actions (N)) then
         Insert_List_Before
           (First (Then_Statements (New_If)), Then_Actions (N));
      end if;

      if Present (Else_Actions (N)) then
         Insert_List_Before
           (First (Else_Statements (New_If)), Else_Actions (N));
      end if;

      Insert_Action (N, Decl);
      Insert_Action (N, New_If);
      Rewrite (N, New_N);
      Analyze_And_Resolve (N, Typ);
   end Expand_N_If_Expression;

   -----------------
   -- Expand_N_In --
   -----------------

   procedure Expand_N_In (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Restyp : constant Entity_Id  := Etype (N);
      Lop    : constant Node_Id    := Left_Opnd (N);
      Rop    : constant Node_Id    := Right_Opnd (N);
      Static : constant Boolean    := Is_OK_Static_Expression (N);

      Ltyp  : Entity_Id;
      Rtyp  : Entity_Id;

      procedure Substitute_Valid_Check;
      --  Replaces node N by Lop'Valid. This is done when we have an explicit
      --  test for the left operand being in range of its subtype.

      ----------------------------
      -- Substitute_Valid_Check --
      ----------------------------

      procedure Substitute_Valid_Check is
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => Relocate_Node (Lop),
             Attribute_Name => Name_Valid));

         Analyze_And_Resolve (N, Restyp);

         --  Give warning unless overflow checking is MINIMIZED or ELIMINATED,
         --  in which case, this usage makes sense, and in any case, we have
         --  actually eliminated the danger of optimization above.

         if Overflow_Check_Mode not in Minimized_Or_Eliminated then
            Error_Msg_N
              ("??explicit membership test may be optimized away", N);
            Error_Msg_N -- CODEFIX
              ("\??use ''Valid attribute instead", N);
         end if;

         return;
      end Substitute_Valid_Check;

   --  Start of processing for Expand_N_In

   begin
      --  If set membership case, expand with separate procedure

      if Present (Alternatives (N)) then
         Expand_Set_Membership (N);
         return;
      end if;

      --  Not set membership, proceed with expansion

      Ltyp := Etype (Left_Opnd  (N));
      Rtyp := Etype (Right_Opnd (N));

      --  If MINIMIZED/ELIMINATED overflow mode and type is a signed integer
      --  type, then expand with a separate procedure. Note the use of the
      --  flag No_Minimize_Eliminate to prevent infinite recursion.

      if Overflow_Check_Mode in Minimized_Or_Eliminated
        and then Is_Signed_Integer_Type (Ltyp)
        and then not No_Minimize_Eliminate (N)
      then
         Expand_Membership_Minimize_Eliminate_Overflow (N);
         return;
      end if;

      --  Check case of explicit test for an expression in range of its
      --  subtype. This is suspicious usage and we replace it with a 'Valid
      --  test and give a warning for scalar types.

      if Is_Scalar_Type (Ltyp)

        --  Only relevant for source comparisons

        and then Comes_From_Source (N)

        --  In floating-point this is a standard way to check for finite values
        --  and using 'Valid would typically be a pessimization.

        and then not Is_Floating_Point_Type (Ltyp)

        --  Don't give the message unless right operand is a type entity and
        --  the type of the left operand matches this type. Note that this
        --  eliminates the cases where MINIMIZED/ELIMINATED mode overflow
        --  checks have changed the type of the left operand.

        and then Nkind (Rop) in N_Has_Entity
        and then Ltyp = Entity (Rop)

        --  Skip in VM mode, where we have no sense of invalid values. The
        --  warning still seems relevant, but not important enough to worry.

        and then VM_Target = No_VM

        --  Skip this for predicated types, where such expressions are a
        --  reasonable way of testing if something meets the predicate.

        and then not Present (Predicate_Function (Ltyp))
      then
         Substitute_Valid_Check;
         return;
      end if;

      --  Do validity check on operands

      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Left_Opnd (N));
         Validity_Check_Range (Right_Opnd (N));
      end if;

      --  Case of explicit range

      if Nkind (Rop) = N_Range then
         declare
            Lo : constant Node_Id := Low_Bound (Rop);
            Hi : constant Node_Id := High_Bound (Rop);

            Lo_Orig : constant Node_Id := Original_Node (Lo);
            Hi_Orig : constant Node_Id := Original_Node (Hi);

            Lcheck : Compare_Result;
            Ucheck : Compare_Result;

            Warn1 : constant Boolean :=
                      Constant_Condition_Warnings
                        and then Comes_From_Source (N)
                        and then not In_Instance;
            --  This must be true for any of the optimization warnings, we
            --  clearly want to give them only for source with the flag on. We
            --  also skip these warnings in an instance since it may be the
            --  case that different instantiations have different ranges.

            Warn2 : constant Boolean :=
                      Warn1
                        and then Nkind (Original_Node (Rop)) = N_Range
                        and then Is_Integer_Type (Etype (Lo));
            --  For the case where only one bound warning is elided, we also
            --  insist on an explicit range and an integer type. The reason is
            --  that the use of enumeration ranges including an end point is
            --  common, as is the use of a subtype name, one of whose bounds is
            --  the same as the type of the expression.

         begin
            --  If test is explicit x'First .. x'Last, replace by valid check

            --  Could use some individual comments for this complex test ???

            if Is_Scalar_Type (Ltyp)

              --  And left operand is X'First where X matches left operand
              --  type (this eliminates cases of type mismatch, including
              --  the cases where ELIMINATED/MINIMIZED mode has changed the
              --  type of the left operand.

              and then Nkind (Lo_Orig) = N_Attribute_Reference
              and then Attribute_Name (Lo_Orig) = Name_First
              and then Nkind (Prefix (Lo_Orig)) in N_Has_Entity
              and then Entity (Prefix (Lo_Orig)) = Ltyp

            --  Same tests for right operand

              and then Nkind (Hi_Orig) = N_Attribute_Reference
              and then Attribute_Name (Hi_Orig) = Name_Last
              and then Nkind (Prefix (Hi_Orig)) in N_Has_Entity
              and then Entity (Prefix (Hi_Orig)) = Ltyp

              --  Relevant only for source cases

              and then Comes_From_Source (N)

              --  Omit for VM cases, where we don't have invalid values

              and then VM_Target = No_VM
            then
               Substitute_Valid_Check;
               goto Leave;
            end if;

            --  If bounds of type are known at compile time, and the end points
            --  are known at compile time and identical, this is another case
            --  for substituting a valid test. We only do this for discrete
            --  types, since it won't arise in practice for float types.

            if Comes_From_Source (N)
              and then Is_Discrete_Type (Ltyp)
              and then Compile_Time_Known_Value (Type_High_Bound (Ltyp))
              and then Compile_Time_Known_Value (Type_Low_Bound  (Ltyp))
              and then Compile_Time_Known_Value (Lo)
              and then Compile_Time_Known_Value (Hi)
              and then Expr_Value (Type_High_Bound (Ltyp)) = Expr_Value (Hi)
              and then Expr_Value (Type_Low_Bound  (Ltyp)) = Expr_Value (Lo)

              --  Kill warnings in instances, since they may be cases where we
              --  have a test in the generic that makes sense with some types
              --  and not with other types.

              and then not In_Instance
            then
               Substitute_Valid_Check;
               goto Leave;
            end if;

            --  If we have an explicit range, do a bit of optimization based on
            --  range analysis (we may be able to kill one or both checks).

            Lcheck := Compile_Time_Compare (Lop, Lo, Assume_Valid => False);
            Ucheck := Compile_Time_Compare (Lop, Hi, Assume_Valid => False);

            --  If either check is known to fail, replace result by False since
            --  the other check does not matter. Preserve the static flag for
            --  legality checks, because we are constant-folding beyond RM 4.9.

            if Lcheck = LT or else Ucheck = GT then
               if Warn1 then
                  Error_Msg_N ("?c?range test optimized away", N);
                  Error_Msg_N ("\?c?value is known to be out of range", N);
               end if;

               Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
               Analyze_And_Resolve (N, Restyp);
               Set_Is_Static_Expression (N, Static);
               goto Leave;

            --  If both checks are known to succeed, replace result by True,
            --  since we know we are in range.

            elsif Lcheck in Compare_GE and then Ucheck in Compare_LE then
               if Warn1 then
                  Error_Msg_N ("?c?range test optimized away", N);
                  Error_Msg_N ("\?c?value is known to be in range", N);
               end if;

               Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
               Analyze_And_Resolve (N, Restyp);
               Set_Is_Static_Expression (N, Static);
               goto Leave;

            --  If lower bound check succeeds and upper bound check is not
            --  known to succeed or fail, then replace the range check with
            --  a comparison against the upper bound.

            elsif Lcheck in Compare_GE then
               if Warn2 and then not In_Instance then
                  Error_Msg_N ("??lower bound test optimized away", Lo);
                  Error_Msg_N ("\??value is known to be in range", Lo);
               end if;

               Rewrite (N,
                 Make_Op_Le (Loc,
                   Left_Opnd  => Lop,
                   Right_Opnd => High_Bound (Rop)));
               Analyze_And_Resolve (N, Restyp);
               goto Leave;

            --  If upper bound check succeeds and lower bound check is not
            --  known to succeed or fail, then replace the range check with
            --  a comparison against the lower bound.

            elsif Ucheck in Compare_LE then
               if Warn2 and then not In_Instance then
                  Error_Msg_N ("??upper bound test optimized away", Hi);
                  Error_Msg_N ("\??value is known to be in range", Hi);
               end if;

               Rewrite (N,
                 Make_Op_Ge (Loc,
                   Left_Opnd  => Lop,
                   Right_Opnd => Low_Bound (Rop)));
               Analyze_And_Resolve (N, Restyp);
               goto Leave;
            end if;

            --  We couldn't optimize away the range check, but there is one
            --  more issue. If we are checking constant conditionals, then we
            --  see if we can determine the outcome assuming everything is
            --  valid, and if so give an appropriate warning.

            if Warn1 and then not Assume_No_Invalid_Values then
               Lcheck := Compile_Time_Compare (Lop, Lo, Assume_Valid => True);
               Ucheck := Compile_Time_Compare (Lop, Hi, Assume_Valid => True);

               --  Result is out of range for valid value

               if Lcheck = LT or else Ucheck = GT then
                  Error_Msg_N
                    ("?c?value can only be in range if it is invalid", N);

               --  Result is in range for valid value

               elsif Lcheck in Compare_GE and then Ucheck in Compare_LE then
                  Error_Msg_N
                    ("?c?value can only be out of range if it is invalid", N);

               --  Lower bound check succeeds if value is valid

               elsif Warn2 and then Lcheck in Compare_GE then
                  Error_Msg_N
                    ("?c?lower bound check only fails if it is invalid", Lo);

               --  Upper bound  check succeeds if value is valid

               elsif Warn2 and then Ucheck in Compare_LE then
                  Error_Msg_N
                    ("?c?upper bound check only fails for invalid values", Hi);
               end if;
            end if;
         end;

         --  For all other cases of an explicit range, nothing to be done

         goto Leave;

      --  Here right operand is a subtype mark

      else
         declare
            Typ       : Entity_Id        := Etype (Rop);
            Is_Acc    : constant Boolean := Is_Access_Type (Typ);
            Cond      : Node_Id          := Empty;
            New_N     : Node_Id;
            Obj       : Node_Id          := Lop;
            SCIL_Node : Node_Id;

         begin
            Remove_Side_Effects (Obj);

            --  For tagged type, do tagged membership operation

            if Is_Tagged_Type (Typ) then

               --  No expansion will be performed when VM_Target, as the VM
               --  back-ends will handle the membership tests directly (tags
               --  are not explicitly represented in Java objects, so the
               --  normal tagged membership expansion is not what we want).

               if Tagged_Type_Expansion then
                  Tagged_Membership (N, SCIL_Node, New_N);
                  Rewrite (N, New_N);
                  Analyze_And_Resolve (N, Restyp);

                  --  Update decoration of relocated node referenced by the
                  --  SCIL node.

                  if Generate_SCIL and then Present (SCIL_Node) then
                     Set_SCIL_Node (N, SCIL_Node);
                  end if;
               end if;

               goto Leave;

            --  If type is scalar type, rewrite as x in t'First .. t'Last.
            --  This reason we do this is that the bounds may have the wrong
            --  type if they come from the original type definition. Also this
            --  way we get all the processing above for an explicit range.

            --  Don't do this for predicated types, since in this case we
            --  want to check the predicate.

            elsif Is_Scalar_Type (Typ) then
               if No (Predicate_Function (Typ)) then
                  Rewrite (Rop,
                    Make_Range (Loc,
                      Low_Bound =>
                        Make_Attribute_Reference (Loc,
                          Attribute_Name => Name_First,
                          Prefix         => New_Occurrence_Of (Typ, Loc)),

                      High_Bound =>
                        Make_Attribute_Reference (Loc,
                          Attribute_Name => Name_Last,
                          Prefix         => New_Occurrence_Of (Typ, Loc))));
                  Analyze_And_Resolve (N, Restyp);
               end if;

               goto Leave;

            --  Ada 2005 (AI-216): Program_Error is raised when evaluating
            --  a membership test if the subtype mark denotes a constrained
            --  Unchecked_Union subtype and the expression lacks inferable
            --  discriminants.

            elsif Is_Unchecked_Union (Base_Type (Typ))
              and then Is_Constrained (Typ)
              and then not Has_Inferable_Discriminants (Lop)
            then
               Insert_Action (N,
                 Make_Raise_Program_Error (Loc,
                   Reason => PE_Unchecked_Union_Restriction));

               --  Prevent Gigi from generating incorrect code by rewriting the
               --  test as False. What is this undocumented thing about ???

               Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
               goto Leave;
            end if;

            --  Here we have a non-scalar type

            if Is_Acc then
               Typ := Designated_Type (Typ);
            end if;

            if not Is_Constrained (Typ) then
               Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
               Analyze_And_Resolve (N, Restyp);

            --  For the constrained array case, we have to check the subscripts
            --  for an exact match if the lengths are non-zero (the lengths
            --  must match in any case).

            elsif Is_Array_Type (Typ) then
               Check_Subscripts : declare
                  function Build_Attribute_Reference
                    (E   : Node_Id;
                     Nam : Name_Id;
                     Dim : Nat) return Node_Id;
                  --  Build attribute reference E'Nam (Dim)

                  -------------------------------
                  -- Build_Attribute_Reference --
                  -------------------------------

                  function Build_Attribute_Reference
                    (E   : Node_Id;
                     Nam : Name_Id;
                     Dim : Nat) return Node_Id
                  is
                  begin
                     return
                       Make_Attribute_Reference (Loc,
                         Prefix         => E,
                         Attribute_Name => Nam,
                         Expressions    => New_List (
                           Make_Integer_Literal (Loc, Dim)));
                  end Build_Attribute_Reference;

               --  Start of processing for Check_Subscripts

               begin
                  for J in 1 .. Number_Dimensions (Typ) loop
                     Evolve_And_Then (Cond,
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Build_Attribute_Reference
                             (Duplicate_Subexpr_No_Checks (Obj),
                              Name_First, J),
                         Right_Opnd =>
                           Build_Attribute_Reference
                             (New_Occurrence_Of (Typ, Loc), Name_First, J)));

                     Evolve_And_Then (Cond,
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Build_Attribute_Reference
                             (Duplicate_Subexpr_No_Checks (Obj),
                              Name_Last, J),
                         Right_Opnd =>
                           Build_Attribute_Reference
                             (New_Occurrence_Of (Typ, Loc), Name_Last, J)));
                  end loop;

                  if Is_Acc then
                     Cond :=
                       Make_Or_Else (Loc,
                         Left_Opnd =>
                           Make_Op_Eq (Loc,
                             Left_Opnd  => Obj,
                             Right_Opnd => Make_Null (Loc)),
                         Right_Opnd => Cond);
                  end if;

                  Rewrite (N, Cond);
                  Analyze_And_Resolve (N, Restyp);
               end Check_Subscripts;

            --  These are the cases where constraint checks may be required,
            --  e.g. records with possible discriminants

            else
               --  Expand the test into a series of discriminant comparisons.
               --  The expression that is built is the negation of the one that
               --  is used for checking discriminant constraints.

               Obj := Relocate_Node (Left_Opnd (N));

               if Has_Discriminants (Typ) then
                  Cond := Make_Op_Not (Loc,
                    Right_Opnd => Build_Discriminant_Checks (Obj, Typ));

                  if Is_Acc then
                     Cond := Make_Or_Else (Loc,
                       Left_Opnd =>
                         Make_Op_Eq (Loc,
                           Left_Opnd  => Obj,
                           Right_Opnd => Make_Null (Loc)),
                       Right_Opnd => Cond);
                  end if;

               else
                  Cond := New_Occurrence_Of (Standard_True, Loc);
               end if;

               Rewrite (N, Cond);
               Analyze_And_Resolve (N, Restyp);
            end if;

            --  Ada 2012 (AI05-0149): Handle membership tests applied to an
            --  expression of an anonymous access type. This can involve an
            --  accessibility test and a tagged type membership test in the
            --  case of tagged designated types.

            if Ada_Version >= Ada_2012
              and then Is_Acc
              and then Ekind (Ltyp) = E_Anonymous_Access_Type
            then
               declare
                  Expr_Entity : Entity_Id := Empty;
                  New_N       : Node_Id;
                  Param_Level : Node_Id;
                  Type_Level  : Node_Id;

               begin
                  if Is_Entity_Name (Lop) then
                     Expr_Entity := Param_Entity (Lop);

                     if not Present (Expr_Entity) then
                        Expr_Entity := Entity (Lop);
                     end if;
                  end if;

                  --  If a conversion of the anonymous access value to the
                  --  tested type would be illegal, then the result is False.

                  if not Valid_Conversion
                           (Lop, Rtyp, Lop, Report_Errs => False)
                  then
                     Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
                     Analyze_And_Resolve (N, Restyp);

                  --  Apply an accessibility check if the access object has an
                  --  associated access level and when the level of the type is
                  --  less deep than the level of the access parameter. This
                  --  only occur for access parameters and stand-alone objects
                  --  of an anonymous access type.

                  else
                     if Present (Expr_Entity)
                       and then
                         Present
                           (Effective_Extra_Accessibility (Expr_Entity))
                       and then UI_Gt (Object_Access_Level (Lop),
                                       Type_Access_Level (Rtyp))
                     then
                        Param_Level :=
                          New_Occurrence_Of
                            (Effective_Extra_Accessibility (Expr_Entity), Loc);

                        Type_Level :=
                          Make_Integer_Literal (Loc, Type_Access_Level (Rtyp));

                        --  Return True only if the accessibility level of the
                        --  expression entity is not deeper than the level of
                        --  the tested access type.

                        Rewrite (N,
                          Make_And_Then (Loc,
                            Left_Opnd  => Relocate_Node (N),
                            Right_Opnd => Make_Op_Le (Loc,
                                            Left_Opnd  => Param_Level,
                                            Right_Opnd => Type_Level)));

                        Analyze_And_Resolve (N);
                     end if;

                     --  If the designated type is tagged, do tagged membership
                     --  operation.

                     --  *** NOTE: we have to check not null before doing the
                     --  tagged membership test (but maybe that can be done
                     --  inside Tagged_Membership?).

                     if Is_Tagged_Type (Typ) then
                        Rewrite (N,
                          Make_And_Then (Loc,
                            Left_Opnd  => Relocate_Node (N),
                            Right_Opnd =>
                              Make_Op_Ne (Loc,
                                Left_Opnd  => Obj,
                                Right_Opnd => Make_Null (Loc))));

                        --  No expansion will be performed when VM_Target, as
                        --  the VM back-ends will handle the membership tests
                        --  directly (tags are not explicitly represented in
                        --  Java objects, so the normal tagged membership
                        --  expansion is not what we want).

                        if Tagged_Type_Expansion then

                           --  Note that we have to pass Original_Node, because
                           --  the membership test might already have been
                           --  rewritten by earlier parts of membership test.

                           Tagged_Membership
                             (Original_Node (N), SCIL_Node, New_N);

                           --  Update decoration of relocated node referenced
                           --  by the SCIL node.

                           if Generate_SCIL and then Present (SCIL_Node) then
                              Set_SCIL_Node (New_N, SCIL_Node);
                           end if;

                           Rewrite (N,
                             Make_And_Then (Loc,
                               Left_Opnd  => Relocate_Node (N),
                               Right_Opnd => New_N));

                           Analyze_And_Resolve (N, Restyp);
                        end if;
                     end if;
                  end if;
               end;
            end if;
         end;
      end if;

   --  At this point, we have done the processing required for the basic
   --  membership test, but not yet dealt with the predicate.

   <<Leave>>

      --  If a predicate is present, then we do the predicate test, but we
      --  most certainly want to omit this if we are within the predicate
      --  function itself, since otherwise we have an infinite recursion.
      --  The check should also not be emitted when testing against a range
      --  (the check is only done when the right operand is a subtype; see
      --  RM12-4.5.2 (28.1/3-30/3)).

      declare
         PFunc : constant Entity_Id := Predicate_Function (Rtyp);

      begin
         if Present (PFunc)
           and then Current_Scope /= PFunc
           and then Nkind (Rop) /= N_Range
         then
            Rewrite (N,
              Make_And_Then (Loc,
                Left_Opnd  => Relocate_Node (N),
                Right_Opnd => Make_Predicate_Call (Rtyp, Lop, Mem => True)));

            --  Analyze new expression, mark left operand as analyzed to
            --  avoid infinite recursion adding predicate calls. Similarly,
            --  suppress further range checks on the call.

            Set_Analyzed (Left_Opnd (N));
            Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);

            --  All done, skip attempt at compile time determination of result

            return;
         end if;
      end;
   end Expand_N_In;

   --------------------------------
   -- Expand_N_Indexed_Component --
   --------------------------------

   procedure Expand_N_Indexed_Component (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      P   : constant Node_Id    := Prefix (N);
      T   : constant Entity_Id  := Etype (P);
      Atp : Entity_Id;

   begin
      --  A special optimization, if we have an indexed component that is
      --  selecting from a slice, then we can eliminate the slice, since, for
      --  example, x (i .. j)(k) is identical to x(k). The only difference is
      --  the range check required by the slice. The range check for the slice
      --  itself has already been generated. The range check for the
      --  subscripting operation is ensured by converting the subject to
      --  the subtype of the slice.

      --  This optimization not only generates better code, avoiding slice
      --  messing especially in the packed case, but more importantly bypasses
      --  some problems in handling this peculiar case, for example, the issue
      --  of dealing specially with object renamings.

      if Nkind (P) = N_Slice then
         Rewrite (N,
           Make_Indexed_Component (Loc,
             Prefix => Prefix (P),
             Expressions => New_List (
               Convert_To
                 (Etype (First_Index (Etype (P))),
                  First (Expressions (N))))));
         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  Ada 2005 (AI-318-02): If the prefix is a call to a build-in-place
      --  function, then additional actuals must be passed.

      if Ada_Version >= Ada_2005
        and then Is_Build_In_Place_Function_Call (P)
      then
         Make_Build_In_Place_Call_In_Anonymous_Context (P);
      end if;

      --  If the prefix is an access type, then we unconditionally rewrite if
      --  as an explicit dereference. This simplifies processing for several
      --  cases, including packed array cases and certain cases in which checks
      --  must be generated. We used to try to do this only when it was
      --  necessary, but it cleans up the code to do it all the time.

      if Is_Access_Type (T) then
         Insert_Explicit_Dereference (P);
         Analyze_And_Resolve (P, Designated_Type (T));
         Atp := Designated_Type (T);
      else
         Atp := T;
      end if;

      --  Generate index and validity checks

      Generate_Index_Checks (N);

      if Validity_Checks_On and then Validity_Check_Subscripts then
         Apply_Subscript_Validity_Checks (N);
      end if;

      --  If selecting from an array with atomic components, and atomic sync
      --  is not suppressed for this array type, set atomic sync flag.

      if (Has_Atomic_Components (Atp)
           and then not Atomic_Synchronization_Disabled (Atp))
        or else (Is_Atomic (Typ)
                  and then not Atomic_Synchronization_Disabled (Typ))
      then
         Activate_Atomic_Synchronization (N);
      end if;

      --  All done for the non-packed case

      if not Is_Packed (Etype (Prefix (N))) then
         return;
      end if;

      --  For packed arrays that are not bit-packed (i.e. the case of an array
      --  with one or more index types with a non-contiguous enumeration type),
      --  we can always use the normal packed element get circuit.

      if not Is_Bit_Packed_Array (Etype (Prefix (N))) then
         Expand_Packed_Element_Reference (N);
         return;
      end if;

      --  For a reference to a component of a bit packed array, we have to
      --  convert it to a reference to the corresponding Packed_Array_Type.
      --  We only want to do this for simple references, and not for:

      --    Left side of assignment, or prefix of left side of assignment, or
      --    prefix of the prefix, to handle packed arrays of packed arrays,
      --      This case is handled in Exp_Ch5.Expand_N_Assignment_Statement

      --    Renaming objects in renaming associations
      --      This case is handled when a use of the renamed variable occurs

      --    Actual parameters for a procedure call
      --      This case is handled in Exp_Ch6.Expand_Actuals

      --    The second expression in a 'Read attribute reference

      --    The prefix of an address or bit or size attribute reference

      --  The following circuit detects these exceptions

      declare
         Child : Node_Id := N;
         Parnt : Node_Id := Parent (N);

      begin
         loop
            if Nkind (Parnt) = N_Unchecked_Expression then
               null;

            elsif Nkind_In (Parnt, N_Object_Renaming_Declaration,
                                   N_Procedure_Call_Statement)
              or else (Nkind (Parnt) = N_Parameter_Association
                        and then
                          Nkind (Parent (Parnt)) =  N_Procedure_Call_Statement)
            then
               return;

            elsif Nkind (Parnt) = N_Attribute_Reference
              and then Nam_In (Attribute_Name (Parnt), Name_Address,
                                                       Name_Bit,
                                                       Name_Size)
              and then Prefix (Parnt) = Child
            then
               return;

            elsif Nkind (Parnt) = N_Assignment_Statement
              and then Name (Parnt) = Child
            then
               return;

            --  If the expression is an index of an indexed component, it must
            --  be expanded regardless of context.

            elsif Nkind (Parnt) = N_Indexed_Component
              and then Child /= Prefix (Parnt)
            then
               Expand_Packed_Element_Reference (N);
               return;

            elsif Nkind (Parent (Parnt)) = N_Assignment_Statement
              and then Name (Parent (Parnt)) = Parnt
            then
               return;

            elsif Nkind (Parnt) = N_Attribute_Reference
              and then Attribute_Name (Parnt) = Name_Read
              and then Next (First (Expressions (Parnt))) = Child
            then
               return;

            elsif Nkind_In (Parnt, N_Indexed_Component, N_Selected_Component)
              and then Prefix (Parnt) = Child
            then
               null;

            else
               Expand_Packed_Element_Reference (N);
               return;
            end if;

            --  Keep looking up tree for unchecked expression, or if we are the
            --  prefix of a possible assignment left side.

            Child := Parnt;
            Parnt := Parent (Child);
         end loop;
      end;
   end Expand_N_Indexed_Component;

   ---------------------
   -- Expand_N_Not_In --
   ---------------------

   --  Replace a not in b by not (a in b) so that the expansions for (a in b)
   --  can be done. This avoids needing to duplicate this expansion code.

   procedure Expand_N_Not_In (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Cfs : constant Boolean    := Comes_From_Source (N);

   begin
      Rewrite (N,
        Make_Op_Not (Loc,
          Right_Opnd =>
            Make_In (Loc,
              Left_Opnd  => Left_Opnd (N),
              Right_Opnd => Right_Opnd (N))));

      --  If this is a set membership, preserve list of alternatives

      Set_Alternatives (Right_Opnd (N), Alternatives (Original_Node (N)));

      --  We want this to appear as coming from source if original does (see
      --  transformations in Expand_N_In).

      Set_Comes_From_Source (N, Cfs);
      Set_Comes_From_Source (Right_Opnd (N), Cfs);

      --  Now analyze transformed node

      Analyze_And_Resolve (N, Typ);
   end Expand_N_Not_In;

   -------------------
   -- Expand_N_Null --
   -------------------

   --  The only replacement required is for the case of a null of a type that
   --  is an access to protected subprogram, or a subtype thereof. We represent
   --  such access values as a record, and so we must replace the occurrence of
   --  null by the equivalent record (with a null address and a null pointer in
   --  it), so that the backend creates the proper value.

   procedure Expand_N_Null (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Base_Type (Etype (N));
      Agg : Node_Id;

   begin
      if Is_Access_Protected_Subprogram_Type (Typ) then
         Agg :=
           Make_Aggregate (Loc,
             Expressions => New_List (
               New_Occurrence_Of (RTE (RE_Null_Address), Loc),
               Make_Null (Loc)));

         Rewrite (N, Agg);
         Analyze_And_Resolve (N, Equivalent_Type (Typ));

         --  For subsequent semantic analysis, the node must retain its type.
         --  Gigi in any case replaces this type by the corresponding record
         --  type before processing the node.

         Set_Etype (N, Typ);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Null;

   ---------------------
   -- Expand_N_Op_Abs --
   ---------------------

   procedure Expand_N_Op_Abs (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Expr : constant Node_Id := Right_Opnd (N);

   begin
      Unary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Deal with software overflow checking

      if not Backend_Overflow_Checks_On_Target
        and then Is_Signed_Integer_Type (Etype (N))
        and then Do_Overflow_Check (N)
      then
         --  The only case to worry about is when the argument is equal to the
         --  largest negative number, so what we do is to insert the check:

         --     [constraint_error when Expr = typ'Base'First]

         --  with the usual Duplicate_Subexpr use coding for expr

         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Expr),
                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix =>
                       New_Occurrence_Of (Base_Type (Etype (Expr)), Loc),
                     Attribute_Name => Name_First)),
             Reason => CE_Overflow_Check_Failed));
      end if;

      --  Vax floating-point types case

      if Vax_Float (Etype (N)) then
         Expand_Vax_Arith (N);
      end if;
   end Expand_N_Op_Abs;

   ---------------------
   -- Expand_N_Op_Add --
   ---------------------

   procedure Expand_N_Op_Add (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  N + 0 = 0 + N = N for integer types

      if Is_Integer_Type (Typ) then
         if Compile_Time_Known_Value (Right_Opnd (N))
           and then Expr_Value (Right_Opnd (N)) = Uint_0
         then
            Rewrite (N, Left_Opnd (N));
            return;

         elsif Compile_Time_Known_Value (Left_Opnd (N))
           and then Expr_Value (Left_Opnd (N)) = Uint_0
         then
            Rewrite (N, Right_Opnd (N));
            return;
         end if;
      end if;

      --  Arithmetic overflow checks for signed integer/fixed point types

      if Is_Signed_Integer_Type (Typ) or else Is_Fixed_Point_Type (Typ) then
         Apply_Arithmetic_Overflow_Check (N);
         return;

      --  Vax floating-point types case

      elsif Vax_Float (Typ) then
         Expand_Vax_Arith (N);
      end if;
   end Expand_N_Op_Add;

   ---------------------
   -- Expand_N_Op_And --
   ---------------------

   procedure Expand_N_Op_And (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      if Is_Array_Type (Etype (N)) then
         Expand_Boolean_Operator (N);

      elsif Is_Boolean_Type (Etype (N)) then
         Adjust_Condition (Left_Opnd (N));
         Adjust_Condition (Right_Opnd (N));
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);

      elsif Is_Intrinsic_Subprogram (Entity (N)) then
         Expand_Intrinsic_Call (N, Entity (N));

      end if;
   end Expand_N_Op_And;

   ------------------------
   -- Expand_N_Op_Concat --
   ------------------------

   procedure Expand_N_Op_Concat (N : Node_Id) is
      Opnds : List_Id;
      --  List of operands to be concatenated

      Cnode : Node_Id;
      --  Node which is to be replaced by the result of concatenating the nodes
      --  in the list Opnds.

   begin
      --  Ensure validity of both operands

      Binary_Op_Validity_Checks (N);

      --  If we are the left operand of a concatenation higher up the tree,
      --  then do nothing for now, since we want to deal with a series of
      --  concatenations as a unit.

      if Nkind (Parent (N)) = N_Op_Concat
        and then N = Left_Opnd (Parent (N))
      then
         return;
      end if;

      --  We get here with a concatenation whose left operand may be a
      --  concatenation itself with a consistent type. We need to process
      --  these concatenation operands from left to right, which means
      --  from the deepest node in the tree to the highest node.

      Cnode := N;
      while Nkind (Left_Opnd (Cnode)) = N_Op_Concat loop
         Cnode := Left_Opnd (Cnode);
      end loop;

      --  Now Cnode is the deepest concatenation, and its parents are the
      --  concatenation nodes above, so now we process bottom up, doing the
      --  operands.

      --  The outer loop runs more than once if more than one concatenation
      --  type is involved.

      Outer : loop
         Opnds := New_List (Left_Opnd (Cnode), Right_Opnd (Cnode));
         Set_Parent (Opnds, N);

         --  The inner loop gathers concatenation operands

         Inner : while Cnode /= N
                   and then Base_Type (Etype (Cnode)) =
                            Base_Type (Etype (Parent (Cnode)))
         loop
            Cnode := Parent (Cnode);
            Append (Right_Opnd (Cnode), Opnds);
         end loop Inner;

         Expand_Concatenate (Cnode, Opnds);

         exit Outer when Cnode = N;
         Cnode := Parent (Cnode);
      end loop Outer;
   end Expand_N_Op_Concat;

   ------------------------
   -- Expand_N_Op_Divide --
   ------------------------

   procedure Expand_N_Op_Divide (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Lopnd : constant Node_Id    := Left_Opnd (N);
      Ropnd : constant Node_Id    := Right_Opnd (N);
      Ltyp  : constant Entity_Id  := Etype (Lopnd);
      Rtyp  : constant Entity_Id  := Etype (Ropnd);
      Typ   : Entity_Id           := Etype (N);
      Rknow : constant Boolean    := Is_Integer_Type (Typ)
                                       and then
                                         Compile_Time_Known_Value (Ropnd);
      Rval  : Uint;

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Otherwise proceed with expansion of division

      if Rknow then
         Rval := Expr_Value (Ropnd);
      end if;

      --  N / 1 = N for integer types

      if Rknow and then Rval = Uint_1 then
         Rewrite (N, Lopnd);
         return;
      end if;

      --  Convert x / 2 ** y to Shift_Right (x, y). Note that the fact that
      --  Is_Power_Of_2_For_Shift is set means that we know that our left
      --  operand is an unsigned integer, as required for this to work.

      if Nkind (Ropnd) = N_Op_Expon
        and then Is_Power_Of_2_For_Shift (Ropnd)

      --  We cannot do this transformation in configurable run time mode if we
      --  have 64-bit integers and long shifts are not available.

        and then (Esize (Ltyp) <= 32 or else Support_Long_Shifts_On_Target)
      then
         Rewrite (N,
           Make_Op_Shift_Right (Loc,
             Left_Opnd  => Lopnd,
             Right_Opnd =>
               Convert_To (Standard_Natural, Right_Opnd (Ropnd))));
         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  Do required fixup of universal fixed operation

      if Typ = Universal_Fixed then
         Fixup_Universal_Fixed_Operation (N);
         Typ := Etype (N);
      end if;

      --  Divisions with fixed-point results

      if Is_Fixed_Point_Type (Typ) then

         --  No special processing if Treat_Fixed_As_Integer is set, since
         --  from a semantic point of view such operations are simply integer
         --  operations and will be treated that way.

         if not Treat_Fixed_As_Integer (N) then
            if Is_Integer_Type (Rtyp) then
               Expand_Divide_Fixed_By_Integer_Giving_Fixed (N);
            else
               Expand_Divide_Fixed_By_Fixed_Giving_Fixed (N);
            end if;
         end if;

      --  Other cases of division of fixed-point operands. Again we exclude the
      --  case where Treat_Fixed_As_Integer is set.

      elsif (Is_Fixed_Point_Type (Ltyp) or else Is_Fixed_Point_Type (Rtyp))
        and then not Treat_Fixed_As_Integer (N)
      then
         if Is_Integer_Type (Typ) then
            Expand_Divide_Fixed_By_Fixed_Giving_Integer (N);
         else
            pragma Assert (Is_Floating_Point_Type (Typ));
            Expand_Divide_Fixed_By_Fixed_Giving_Float (N);
         end if;

      --  Mixed-mode operations can appear in a non-static universal context,
      --  in which case the integer argument must be converted explicitly.

      elsif Typ = Universal_Real and then Is_Integer_Type (Rtyp) then
         Rewrite (Ropnd,
           Convert_To (Universal_Real, Relocate_Node (Ropnd)));

         Analyze_And_Resolve (Ropnd, Universal_Real);

      elsif Typ = Universal_Real and then Is_Integer_Type (Ltyp) then
         Rewrite (Lopnd,
           Convert_To (Universal_Real, Relocate_Node (Lopnd)));

         Analyze_And_Resolve (Lopnd, Universal_Real);

      --  Non-fixed point cases, do integer zero divide and overflow checks

      elsif Is_Integer_Type (Typ) then
         Apply_Divide_Checks (N);

      --  Deal with Vax_Float

      elsif Vax_Float (Typ) then
         Expand_Vax_Arith (N);
         return;
      end if;
   end Expand_N_Op_Divide;

   --------------------
   -- Expand_N_Op_Eq --
   --------------------

   procedure Expand_N_Op_Eq (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Typ    : constant Entity_Id  := Etype (N);
      Lhs    : constant Node_Id    := Left_Opnd (N);
      Rhs    : constant Node_Id    := Right_Opnd (N);
      Bodies : constant List_Id    := New_List;
      A_Typ  : constant Entity_Id  := Etype (Lhs);

      Typl    : Entity_Id := A_Typ;
      Op_Name : Entity_Id;
      Prim    : Elmt_Id;

      procedure Build_Equality_Call (Eq : Entity_Id);
      --  If a constructed equality exists for the type or for its parent,
      --  build and analyze call, adding conversions if the operation is
      --  inherited.

      function Has_Unconstrained_UU_Component (Typ : Node_Id) return Boolean;
      --  Determines whether a type has a subcomponent of an unconstrained
      --  Unchecked_Union subtype. Typ is a record type.

      -------------------------
      -- Build_Equality_Call --
      -------------------------

      procedure Build_Equality_Call (Eq : Entity_Id) is
         Op_Type : constant Entity_Id := Etype (First_Formal (Eq));
         L_Exp   : Node_Id := Relocate_Node (Lhs);
         R_Exp   : Node_Id := Relocate_Node (Rhs);

      begin
         if Base_Type (Op_Type) /= Base_Type (A_Typ)
           and then not Is_Class_Wide_Type (A_Typ)
         then
            L_Exp := OK_Convert_To (Op_Type, L_Exp);
            R_Exp := OK_Convert_To (Op_Type, R_Exp);
         end if;

         --  If we have an Unchecked_Union, we need to add the inferred
         --  discriminant values as actuals in the function call. At this
         --  point, the expansion has determined that both operands have
         --  inferable discriminants.

         if Is_Unchecked_Union (Op_Type) then
            declare
               Lhs_Type : constant Node_Id := Etype (L_Exp);
               Rhs_Type : constant Node_Id := Etype (R_Exp);

               Lhs_Discr_Vals : Elist_Id;
               --  List of inferred discriminant values for left operand.

               Rhs_Discr_Vals : Elist_Id;
               --  List of inferred discriminant values for right operand.

               Discr : Entity_Id;

            begin
               Lhs_Discr_Vals := New_Elmt_List;
               Rhs_Discr_Vals := New_Elmt_List;

               --  Per-object constrained selected components require special
               --  attention. If the enclosing scope of the component is an
               --  Unchecked_Union, we cannot reference its discriminants
               --  directly. This is why we use the extra parameters of the
               --  equality function of the enclosing Unchecked_Union.

               --  type UU_Type (Discr : Integer := 0) is
               --     . . .
               --  end record;
               --  pragma Unchecked_Union (UU_Type);

               --  1. Unchecked_Union enclosing record:

               --     type Enclosing_UU_Type (Discr : Integer := 0) is record
               --        . . .
               --        Comp : UU_Type (Discr);
               --        . . .
               --     end Enclosing_UU_Type;
               --     pragma Unchecked_Union (Enclosing_UU_Type);

               --     Obj1 : Enclosing_UU_Type;
               --     Obj2 : Enclosing_UU_Type (1);

               --     [. . .] Obj1 = Obj2 [. . .]

               --     Generated code:

               --     if not (uu_typeEQ (obj1.comp, obj2.comp, a, b)) then

               --  A and B are the formal parameters of the equality function
               --  of Enclosing_UU_Type. The function always has two extra
               --  formals to capture the inferred discriminant values for
               --  each discriminant of the type.

               --  2. Non-Unchecked_Union enclosing record:

               --     type
               --       Enclosing_Non_UU_Type (Discr : Integer := 0)
               --     is record
               --        . . .
               --        Comp : UU_Type (Discr);
               --        . . .
               --     end Enclosing_Non_UU_Type;

               --     Obj1 : Enclosing_Non_UU_Type;
               --     Obj2 : Enclosing_Non_UU_Type (1);

               --     ...  Obj1 = Obj2 ...

               --     Generated code:

               --     if not (uu_typeEQ (obj1.comp, obj2.comp,
               --                        obj1.discr, obj2.discr)) then

               --  In this case we can directly reference the discriminants of
               --  the enclosing record.

               --  Process left operand of equality

               if Nkind (Lhs) = N_Selected_Component
                 and then
                   Has_Per_Object_Constraint (Entity (Selector_Name (Lhs)))
               then
                  --  If enclosing record is an Unchecked_Union, use formals
                  --  corresponding to each discriminant. The name of the
                  --  formal is that of the discriminant, with added suffix,
                  --  see Exp_Ch3.Build_Record_Equality for details.

                  if Is_Unchecked_Union
                       (Scope (Entity (Selector_Name (Lhs))))
                  then
                     Discr :=
                       First_Discriminant
                         (Scope (Entity (Selector_Name (Lhs))));
                     while Present (Discr) loop
                        Append_Elmt (
                          Make_Identifier (Loc,
                            Chars => New_External_Name (Chars (Discr), 'A')),
                          To => Lhs_Discr_Vals);
                        Next_Discriminant (Discr);
                     end loop;

                  --  If enclosing record is of a non-Unchecked_Union type, it
                  --  is possible to reference its discriminants directly.

                  else
                     Discr := First_Discriminant (Lhs_Type);
                     while Present (Discr) loop
                        Append_Elmt (
                          Make_Selected_Component (Loc,
                            Prefix => Prefix (Lhs),
                            Selector_Name =>
                              New_Copy
                                (Get_Discriminant_Value (Discr,
                                    Lhs_Type,
                                    Stored_Constraint (Lhs_Type)))),
                          To => Lhs_Discr_Vals);
                        Next_Discriminant (Discr);
                     end loop;
                  end if;

               --  Otherwise operand is on object with a constrained type.
               --  Infer the discriminant values from the constraint.

               else

                  Discr := First_Discriminant (Lhs_Type);
                  while Present (Discr) loop
                     Append_Elmt (
                       New_Copy
                         (Get_Discriminant_Value (Discr,
                             Lhs_Type,
                             Stored_Constraint (Lhs_Type))),
                       To => Lhs_Discr_Vals);
                     Next_Discriminant (Discr);
                  end loop;
               end if;

               --  Similar processing for right operand of equality

               if Nkind (Rhs) = N_Selected_Component
                 and then
                   Has_Per_Object_Constraint (Entity (Selector_Name (Rhs)))
               then
                  if Is_Unchecked_Union
                    (Scope (Entity (Selector_Name (Rhs))))
                  then
                     Discr :=
                       First_Discriminant
                         (Scope (Entity (Selector_Name (Rhs))));
                     while Present (Discr) loop
                        Append_Elmt (
                          Make_Identifier (Loc,
                            Chars => New_External_Name (Chars (Discr), 'B')),
                          To => Rhs_Discr_Vals);
                        Next_Discriminant (Discr);
                     end loop;

                  else
                     Discr := First_Discriminant (Rhs_Type);
                     while Present (Discr) loop
                        Append_Elmt (
                          Make_Selected_Component (Loc,
                            Prefix        => Prefix (Rhs),
                            Selector_Name =>
                              New_Copy (Get_Discriminant_Value
                                          (Discr,
                                           Rhs_Type,
                                           Stored_Constraint (Rhs_Type)))),
                          To => Rhs_Discr_Vals);
                        Next_Discriminant (Discr);
                     end loop;
                  end if;

               else
                  Discr := First_Discriminant (Rhs_Type);
                  while Present (Discr) loop
                     Append_Elmt (
                       New_Copy (Get_Discriminant_Value
                                   (Discr,
                                    Rhs_Type,
                                    Stored_Constraint (Rhs_Type))),
                       To => Rhs_Discr_Vals);
                     Next_Discriminant (Discr);
                  end loop;
               end if;

               --  Now merge the list of discriminant values so that values
               --  of corresponding discriminants are adjacent.

               declare
                  Params : List_Id;
                  L_Elmt : Elmt_Id;
                  R_Elmt : Elmt_Id;

               begin
                  Params := New_List (L_Exp, R_Exp);
                  L_Elmt := First_Elmt (Lhs_Discr_Vals);
                  R_Elmt := First_Elmt (Rhs_Discr_Vals);
                  while Present (L_Elmt) loop
                     Append_To (Params, Node (L_Elmt));
                     Append_To (Params, Node (R_Elmt));
                     Next_Elmt (L_Elmt);
                     Next_Elmt (R_Elmt);
                  end loop;

                  Rewrite (N,
                    Make_Function_Call (Loc,
                      Name                   => New_Occurrence_Of (Eq, Loc),
                      Parameter_Associations => Params));
               end;
            end;

         --  Normal case, not an unchecked union

         else
            Rewrite (N,
              Make_Function_Call (Loc,
                Name                   => New_Occurrence_Of (Eq, Loc),
                Parameter_Associations => New_List (L_Exp, R_Exp)));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
      end Build_Equality_Call;

      ------------------------------------
      -- Has_Unconstrained_UU_Component --
      ------------------------------------

      function Has_Unconstrained_UU_Component
        (Typ : Node_Id) return Boolean
      is
         Tdef  : constant Node_Id :=
                   Type_Definition (Declaration_Node (Base_Type (Typ)));
         Clist : Node_Id;
         Vpart : Node_Id;

         function Component_Is_Unconstrained_UU
           (Comp : Node_Id) return Boolean;
         --  Determines whether the subtype of the component is an
         --  unconstrained Unchecked_Union.

         function Variant_Is_Unconstrained_UU
           (Variant : Node_Id) return Boolean;
         --  Determines whether a component of the variant has an unconstrained
         --  Unchecked_Union subtype.

         -----------------------------------
         -- Component_Is_Unconstrained_UU --
         -----------------------------------

         function Component_Is_Unconstrained_UU
           (Comp : Node_Id) return Boolean
         is
         begin
            if Nkind (Comp) /= N_Component_Declaration then
               return False;
            end if;

            declare
               Sindic : constant Node_Id :=
                          Subtype_Indication (Component_Definition (Comp));

            begin
               --  Unconstrained nominal type. In the case of a constraint
               --  present, the node kind would have been N_Subtype_Indication.

               if Nkind (Sindic) = N_Identifier then
                  return Is_Unchecked_Union (Base_Type (Etype (Sindic)));
               end if;

               return False;
            end;
         end Component_Is_Unconstrained_UU;

         ---------------------------------
         -- Variant_Is_Unconstrained_UU --
         ---------------------------------

         function Variant_Is_Unconstrained_UU
           (Variant : Node_Id) return Boolean
         is
            Clist : constant Node_Id := Component_List (Variant);

         begin
            if Is_Empty_List (Component_Items (Clist)) then
               return False;
            end if;

            --  We only need to test one component

            declare
               Comp : Node_Id := First (Component_Items (Clist));

            begin
               while Present (Comp) loop
                  if Component_Is_Unconstrained_UU (Comp) then
                     return True;
                  end if;

                  Next (Comp);
               end loop;
            end;

            --  None of the components withing the variant were of
            --  unconstrained Unchecked_Union type.

            return False;
         end Variant_Is_Unconstrained_UU;

      --  Start of processing for Has_Unconstrained_UU_Component

      begin
         if Null_Present (Tdef) then
            return False;
         end if;

         Clist := Component_List (Tdef);
         Vpart := Variant_Part (Clist);

         --  Inspect available components

         if Present (Component_Items (Clist)) then
            declare
               Comp : Node_Id := First (Component_Items (Clist));

            begin
               while Present (Comp) loop

                  --  One component is sufficient

                  if Component_Is_Unconstrained_UU (Comp) then
                     return True;
                  end if;

                  Next (Comp);
               end loop;
            end;
         end if;

         --  Inspect available components withing variants

         if Present (Vpart) then
            declare
               Variant : Node_Id := First (Variants (Vpart));

            begin
               while Present (Variant) loop

                  --  One component within a variant is sufficient

                  if Variant_Is_Unconstrained_UU (Variant) then
                     return True;
                  end if;

                  Next (Variant);
               end loop;
            end;
         end if;

         --  Neither the available components, nor the components inside the
         --  variant parts were of an unconstrained Unchecked_Union subtype.

         return False;
      end Has_Unconstrained_UU_Component;

   --  Start of processing for Expand_N_Op_Eq

   begin
      Binary_Op_Validity_Checks (N);

      --  Deal with private types

      if Ekind (Typl) = E_Private_Type then
         Typl := Underlying_Type (Typl);
      elsif Ekind (Typl) = E_Private_Subtype then
         Typl := Underlying_Type (Base_Type (Typl));
      else
         null;
      end if;

      --  It may happen in error situations that the underlying type is not
      --  set. The error will be detected later, here we just defend the
      --  expander code.

      if No (Typl) then
         return;
      end if;

      Typl := Base_Type (Typl);

      --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if that
      --  means we no longer have a comparison operation, we are all done.

      Expand_Compare_Minimize_Eliminate_Overflow (N);

      if Nkind (N) /= N_Op_Eq then
         return;
      end if;

      --  Boolean types (requiring handling of non-standard case)

      if Is_Boolean_Type (Typl) then
         Adjust_Condition (Left_Opnd (N));
         Adjust_Condition (Right_Opnd (N));
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);

      --  Array types

      elsif Is_Array_Type (Typl) then

         --  If we are doing full validity checking, and it is possible for the
         --  array elements to be invalid then expand out array comparisons to
         --  make sure that we check the array elements.

         if Validity_Check_Operands
           and then not Is_Known_Valid (Component_Type (Typl))
         then
            declare
               Save_Force_Validity_Checks : constant Boolean :=
                                              Force_Validity_Checks;
            begin
               Force_Validity_Checks := True;
               Rewrite (N,
                 Expand_Array_Equality
                  (N,
                   Relocate_Node (Lhs),
                   Relocate_Node (Rhs),
                   Bodies,
                   Typl));
               Insert_Actions (N, Bodies);
               Analyze_And_Resolve (N, Standard_Boolean);
               Force_Validity_Checks := Save_Force_Validity_Checks;
            end;

         --  Packed case where both operands are known aligned

         elsif Is_Bit_Packed_Array (Typl)
           and then not Is_Possibly_Unaligned_Object (Lhs)
           and then not Is_Possibly_Unaligned_Object (Rhs)
         then
            Expand_Packed_Eq (N);

         --  Where the component type is elementary we can use a block bit
         --  comparison (if supported on the target) exception in the case
         --  of floating-point (negative zero issues require element by
         --  element comparison), and atomic types (where we must be sure
         --  to load elements independently) and possibly unaligned arrays.

         elsif Is_Elementary_Type (Component_Type (Typl))
           and then not Is_Floating_Point_Type (Component_Type (Typl))
           and then not Is_Atomic (Component_Type (Typl))
           and then not Is_Possibly_Unaligned_Object (Lhs)
           and then not Is_Possibly_Unaligned_Object (Rhs)
           and then Support_Composite_Compare_On_Target
         then
            null;

         --  For composite and floating-point cases, expand equality loop to
         --  make sure of using proper comparisons for tagged types, and
         --  correctly handling the floating-point case.

         else
            Rewrite (N,
              Expand_Array_Equality
                (N,
                 Relocate_Node (Lhs),
                 Relocate_Node (Rhs),
                 Bodies,
                 Typl));
            Insert_Actions      (N, Bodies,           Suppress => All_Checks);
            Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
         end if;

      --  Record Types

      elsif Is_Record_Type (Typl) then

         --  For tagged types, use the primitive "="

         if Is_Tagged_Type (Typl) then

            --  No need to do anything else compiling under restriction
            --  No_Dispatching_Calls. During the semantic analysis we
            --  already notified such violation.

            if Restriction_Active (No_Dispatching_Calls) then
               return;
            end if;

            --  If this is derived from an untagged private type completed with
            --  a tagged type, it does not have a full view, so we use the
            --  primitive operations of the private type. This check should no
            --  longer be necessary when these types get their full views???

            if Is_Private_Type (A_Typ)
              and then not Is_Tagged_Type (A_Typ)
              and then Is_Derived_Type (A_Typ)
              and then No (Full_View (A_Typ))
            then
               --  Search for equality operation, checking that the operands
               --  have the same type. Note that we must find a matching entry,
               --  or something is very wrong.

               Prim := First_Elmt (Collect_Primitive_Operations (A_Typ));

               while Present (Prim) loop
                  exit when Chars (Node (Prim)) = Name_Op_Eq
                    and then Etype (First_Formal (Node (Prim))) =
                             Etype (Next_Formal (First_Formal (Node (Prim))))
                    and then
                      Base_Type (Etype (Node (Prim))) = Standard_Boolean;

                  Next_Elmt (Prim);
               end loop;

               pragma Assert (Present (Prim));
               Op_Name := Node (Prim);

            --  Find the type's predefined equality or an overriding
            --  user- defined equality. The reason for not simply calling
            --  Find_Prim_Op here is that there may be a user-defined
            --  overloaded equality op that precedes the equality that we want,
            --  so we have to explicitly search (e.g., there could be an
            --  equality with two different parameter types).

            else
               if Is_Class_Wide_Type (Typl) then
                  Typl := Root_Type (Typl);
               end if;

               Prim := First_Elmt (Primitive_Operations (Typl));
               while Present (Prim) loop
                  exit when Chars (Node (Prim)) = Name_Op_Eq
                    and then Etype (First_Formal (Node (Prim))) =
                             Etype (Next_Formal (First_Formal (Node (Prim))))
                    and then
                      Base_Type (Etype (Node (Prim))) = Standard_Boolean;

                  Next_Elmt (Prim);
               end loop;

               pragma Assert (Present (Prim));
               Op_Name := Node (Prim);
            end if;

            Build_Equality_Call (Op_Name);

         --  Ada 2005 (AI-216): Program_Error is raised when evaluating the
         --  predefined equality operator for a type which has a subcomponent
         --  of an Unchecked_Union type whose nominal subtype is unconstrained.

         elsif Has_Unconstrained_UU_Component (Typl) then
            Insert_Action (N,
              Make_Raise_Program_Error (Loc,
                Reason => PE_Unchecked_Union_Restriction));

            --  Prevent Gigi from generating incorrect code by rewriting the
            --  equality as a standard False. (is this documented somewhere???)

            Rewrite (N,
              New_Occurrence_Of (Standard_False, Loc));

         elsif Is_Unchecked_Union (Typl) then

            --  If we can infer the discriminants of the operands, we make a
            --  call to the TSS equality function.

            if Has_Inferable_Discriminants (Lhs)
                 and then
               Has_Inferable_Discriminants (Rhs)
            then
               Build_Equality_Call
                 (TSS (Root_Type (Typl), TSS_Composite_Equality));

            else
               --  Ada 2005 (AI-216): Program_Error is raised when evaluating
               --  the predefined equality operator for an Unchecked_Union type
               --  if either of the operands lack inferable discriminants.

               Insert_Action (N,
                 Make_Raise_Program_Error (Loc,
                   Reason => PE_Unchecked_Union_Restriction));

               --  Prevent Gigi from generating incorrect code by rewriting
               --  the equality as a standard False (documented where???).

               Rewrite (N,
                 New_Occurrence_Of (Standard_False, Loc));

            end if;

         --  If a type support function is present (for complex cases), use it

         elsif Present (TSS (Root_Type (Typl), TSS_Composite_Equality)) then
            Build_Equality_Call
              (TSS (Root_Type (Typl), TSS_Composite_Equality));

         --  When comparing two Bounded_Strings, use the primitive equality of
         --  the root Super_String type.

         elsif Is_Bounded_String (Typl) then
            Prim :=
              First_Elmt (Collect_Primitive_Operations (Root_Type (Typl)));

            while Present (Prim) loop
               exit when Chars (Node (Prim)) = Name_Op_Eq
                 and then Etype (First_Formal (Node (Prim))) =
                          Etype (Next_Formal (First_Formal (Node (Prim))))
                 and then Base_Type (Etype (Node (Prim))) = Standard_Boolean;

               Next_Elmt (Prim);
            end loop;

            --  A Super_String type should always have a primitive equality

            pragma Assert (Present (Prim));
            Build_Equality_Call (Node (Prim));

         --  Otherwise expand the component by component equality. Note that
         --  we never use block-bit comparisons for records, because of the
         --  problems with gaps. The backend will often be able to recombine
         --  the separate comparisons that we generate here.

         else
            Remove_Side_Effects (Lhs);
            Remove_Side_Effects (Rhs);
            Rewrite (N,
              Expand_Record_Equality (N, Typl, Lhs, Rhs, Bodies));

            Insert_Actions      (N, Bodies,           Suppress => All_Checks);
            Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
         end if;
      end if;

      --  Test if result is known at compile time

      Rewrite_Comparison (N);

      --  If we still have comparison for Vax_Float, process it

      if Vax_Float (Typl) and then Nkind (N) in N_Op_Compare  then
         Expand_Vax_Comparison (N);
         return;
      end if;

      Optimize_Length_Comparison (N);
   end Expand_N_Op_Eq;

   -----------------------
   -- Expand_N_Op_Expon --
   -----------------------

   procedure Expand_N_Op_Expon (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Typ    : constant Entity_Id  := Etype (N);
      Rtyp   : constant Entity_Id  := Root_Type (Typ);
      Base   : constant Node_Id    := Relocate_Node (Left_Opnd (N));
      Bastyp : constant Node_Id    := Etype (Base);
      Exp    : constant Node_Id    := Relocate_Node (Right_Opnd (N));
      Exptyp : constant Entity_Id  := Etype (Exp);
      Ovflo  : constant Boolean    := Do_Overflow_Check (N);
      Expv   : Uint;
      Temp   : Node_Id;
      Rent   : RE_Id;
      Ent    : Entity_Id;
      Etyp   : Entity_Id;
      Xnode  : Node_Id;

   begin
      Binary_Op_Validity_Checks (N);

      --  CodePeer wants to see the unexpanded N_Op_Expon node

      if CodePeer_Mode then
         return;
      end if;

      --  If either operand is of a private type, then we have the use of an
      --  intrinsic operator, and we get rid of the privateness, by using root
      --  types of underlying types for the actual operation. Otherwise the
      --  private types will cause trouble if we expand multiplications or
      --  shifts etc. We also do this transformation if the result type is
      --  different from the base type.

      if Is_Private_Type (Etype (Base))
        or else Is_Private_Type (Typ)
        or else Is_Private_Type (Exptyp)
        or else Rtyp /= Root_Type (Bastyp)
      then
         declare
            Bt : constant Entity_Id := Root_Type (Underlying_Type (Bastyp));
            Et : constant Entity_Id := Root_Type (Underlying_Type (Exptyp));
         begin
            Rewrite (N,
              Unchecked_Convert_To (Typ,
                Make_Op_Expon (Loc,
                  Left_Opnd  => Unchecked_Convert_To (Bt, Base),
                  Right_Opnd => Unchecked_Convert_To (Et, Exp))));
            Analyze_And_Resolve (N, Typ);
            return;
         end;
      end if;

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Test for case of known right argument where we can replace the
      --  exponentiation by an equivalent expression using multiplication.

      --  Note: use CRT_Safe version of Compile_Time_Known_Value because in
      --  configurable run-time mode, we may not have the exponentiation
      --  routine available, and we don't want the legality of the program
      --  to depend on how clever the compiler is in knowing values.

      if CRT_Safe_Compile_Time_Known_Value (Exp) then
         Expv := Expr_Value (Exp);

         --  We only fold small non-negative exponents. You might think we
         --  could fold small negative exponents for the real case, but we
         --  can't because we are required to raise Constraint_Error for
         --  the case of 0.0 ** (negative) even if Machine_Overflows = False.
         --  See ACVC test C4A012B.

         if Expv >= 0 and then Expv <= 4 then

            --  X ** 0 = 1 (or 1.0)

            if Expv = 0 then

               --  Call Remove_Side_Effects to ensure that any side effects
               --  in the ignored left operand (in particular function calls
               --  to user defined functions) are properly executed.

               Remove_Side_Effects (Base);

               if Ekind (Typ) in Integer_Kind then
                  Xnode := Make_Integer_Literal (Loc, Intval => 1);
               else
                  Xnode := Make_Real_Literal (Loc, Ureal_1);
               end if;

            --  X ** 1 = X

            elsif Expv = 1 then
               Xnode := Base;

            --  X ** 2 = X * X

            elsif Expv = 2 then
               Xnode :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd  => Duplicate_Subexpr (Base),
                   Right_Opnd => Duplicate_Subexpr_No_Checks (Base));

            --  X ** 3 = X * X * X

            elsif Expv = 3 then
               Xnode :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd =>
                     Make_Op_Multiply (Loc,
                       Left_Opnd  => Duplicate_Subexpr (Base),
                       Right_Opnd => Duplicate_Subexpr_No_Checks (Base)),
                   Right_Opnd  => Duplicate_Subexpr_No_Checks (Base));

            --  X ** 4  ->

            --  do
            --    En : constant base'type := base * base;
            --  in
            --    En * En

            else
               pragma Assert (Expv = 4);
               Temp := Make_Temporary (Loc, 'E', Base);

               Xnode :=
                 Make_Expression_With_Actions (Loc,
                   Actions    => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Temp,
                       Constant_Present    => True,
                       Object_Definition   => New_Occurrence_Of (Typ, Loc),
                       Expression =>
                         Make_Op_Multiply (Loc,
                           Left_Opnd  =>
                             Duplicate_Subexpr (Base),
                           Right_Opnd =>
                             Duplicate_Subexpr_No_Checks (Base)))),

                   Expression =>
                     Make_Op_Multiply (Loc,
                       Left_Opnd  => New_Occurrence_Of (Temp, Loc),
                       Right_Opnd => New_Occurrence_Of (Temp, Loc)));
            end if;

            Rewrite (N, Xnode);
            Analyze_And_Resolve (N, Typ);
            return;
         end if;
      end if;

      --  Case of (2 ** expression) appearing as an argument of an integer
      --  multiplication, or as the right argument of a division of a non-
      --  negative integer. In such cases we leave the node untouched, setting
      --  the flag Is_Natural_Power_Of_2_for_Shift set, then the expansion
      --  of the higher level node converts it into a shift.

      --  Another case is 2 ** N in any other context. We simply convert
      --  this to 1 * 2 ** N, and then the above transformation applies.

      --  Note: this transformation is not applicable for a modular type with
      --  a non-binary modulus in the multiplication case, since we get a wrong
      --  result if the shift causes an overflow before the modular reduction.

      --  Note: we used to check that Exptyp was an unsigned type. But that is
      --  an unnecessary check, since if Exp is negative, we have a run-time
      --  error that is either caught (so we get the right result) or we have
      --  suppressed the check, in which case the code is erroneous anyway.

      if Nkind (Base) = N_Integer_Literal
        and then CRT_Safe_Compile_Time_Known_Value (Base)
        and then Expr_Value (Base) = Uint_2
        and then Is_Integer_Type (Root_Type (Exptyp))
        and then Esize (Root_Type (Exptyp)) <= Esize (Standard_Integer)
        and then not Ovflo
      then
         --  First the multiply and divide cases

         if Nkind_In (Parent (N), N_Op_Divide, N_Op_Multiply) then
            declare
               P : constant Node_Id := Parent (N);
               L : constant Node_Id := Left_Opnd (P);
               R : constant Node_Id := Right_Opnd (P);

            begin
               if (Nkind (P) = N_Op_Multiply
                   and then not Non_Binary_Modulus (Typ)
                   and then
                     ((Is_Integer_Type (Etype (L)) and then R = N)
                         or else
                      (Is_Integer_Type (Etype (R)) and then L = N))
                   and then not Do_Overflow_Check (P))
                 or else
                  (Nkind (P) = N_Op_Divide
                    and then Is_Integer_Type (Etype (L))
                    and then Is_Unsigned_Type (Etype (L))
                    and then R = N
                    and then not Do_Overflow_Check (P))
               then
                  Set_Is_Power_Of_2_For_Shift (N);
                  return;
               end if;
            end;

         --  Now the other cases

         elsif not Non_Binary_Modulus (Typ) then
            Rewrite (N,
              Make_Op_Multiply (Loc,
                Left_Opnd  => Make_Integer_Literal (Loc, 1),
                Right_Opnd => Relocate_Node (N)));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;
      end if;

      --  Fall through if exponentiation must be done using a runtime routine

      --  First deal with modular case

      if Is_Modular_Integer_Type (Rtyp) then

         --  Non-binary case, we call the special exponentiation routine for
         --  the non-binary case, converting the argument to Long_Long_Integer
         --  and passing the modulus value. Then the result is converted back
         --  to the base type.

         if Non_Binary_Modulus (Rtyp) then
            Rewrite (N,
              Convert_To (Typ,
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (RTE (RE_Exp_Modular), Loc),
                  Parameter_Associations => New_List (
                    Convert_To (Standard_Integer, Base),
                    Make_Integer_Literal (Loc, Modulus (Rtyp)),
                    Exp))));

         --  Binary case, in this case, we call one of two routines, either the
         --  unsigned integer case, or the unsigned long long integer case,
         --  with a final "and" operation to do the required mod.

         else
            if UI_To_Int (Esize (Rtyp)) <= Standard_Integer_Size then
               Ent := RTE (RE_Exp_Unsigned);
            else
               Ent := RTE (RE_Exp_Long_Long_Unsigned);
            end if;

            Rewrite (N,
              Convert_To (Typ,
                Make_Op_And (Loc,
                  Left_Opnd =>
                    Make_Function_Call (Loc,
                      Name => New_Occurrence_Of (Ent, Loc),
                      Parameter_Associations => New_List (
                        Convert_To (Etype (First_Formal (Ent)), Base),
                        Exp)),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Modulus (Rtyp) - 1))));

         end if;

         --  Common exit point for modular type case

         Analyze_And_Resolve (N, Typ);
         return;

      --  Signed integer cases, done using either Integer or Long_Long_Integer.
      --  It is not worth having routines for Short_[Short_]Integer, since for
      --  most machines it would not help, and it would generate more code that
      --  might need certification when a certified run time is required.

      --  In the integer cases, we have two routines, one for when overflow
      --  checks are required, and one when they are not required, since there
      --  is a real gain in omitting checks on many machines.

      elsif Rtyp = Base_Type (Standard_Long_Long_Integer)
        or else (Rtyp = Base_Type (Standard_Long_Integer)
                  and then
                    Esize (Standard_Long_Integer) > Esize (Standard_Integer))
        or else Rtyp = Universal_Integer
      then
         Etyp := Standard_Long_Long_Integer;

         --  Overflow checking is the only choice on the AAMP target, where
         --  arithmetic instructions check overflow automatically, so only
         --  one version of the exponentiation unit is needed.

         if Ovflo or else AAMP_On_Target then
            Rent := RE_Exp_Long_Long_Integer;
         else
            Rent := RE_Exn_Long_Long_Integer;
         end if;

      elsif Is_Signed_Integer_Type (Rtyp) then
         Etyp := Standard_Integer;

         --  Overflow checking is the only choice on the AAMP target, where
         --  arithmetic instructions check overflow automatically, so only
         --  one version of the exponentiation unit is needed.

         if Ovflo or else AAMP_On_Target then
            Rent := RE_Exp_Integer;
         else
            Rent := RE_Exn_Integer;
         end if;

      --  Floating-point cases, always done using Long_Long_Float. We do not
      --  need separate routines for the overflow case here, since in the case
      --  of floating-point, we generate infinities anyway as a rule (either
      --  that or we automatically trap overflow), and if there is an infinity
      --  generated and a range check is required, the check will fail anyway.

      else
         pragma Assert (Is_Floating_Point_Type (Rtyp));
         Etyp := Standard_Long_Long_Float;
         Rent := RE_Exn_Long_Long_Float;
      end if;

      --  Common processing for integer cases and floating-point cases.
      --  If we are in the right type, we can call runtime routine directly

      if Typ = Etyp
        and then Rtyp /= Universal_Integer
        and then Rtyp /= Universal_Real
      then
         Rewrite (N,
           Make_Function_Call (Loc,
             Name                   => New_Occurrence_Of (RTE (Rent), Loc),
             Parameter_Associations => New_List (Base, Exp)));

      --  Otherwise we have to introduce conversions (conversions are also
      --  required in the universal cases, since the runtime routine is
      --  typed using one of the standard types).

      else
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Rent), Loc),
               Parameter_Associations => New_List (
                 Convert_To (Etyp, Base),
                 Exp))));
      end if;

      Analyze_And_Resolve (N, Typ);
      return;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Op_Expon;

   --------------------
   -- Expand_N_Op_Ge --
   --------------------

   procedure Expand_N_Op_Ge (N : Node_Id) is
      Typ  : constant Entity_Id := Etype (N);
      Op1  : constant Node_Id   := Left_Opnd (N);
      Op2  : constant Node_Id   := Right_Opnd (N);
      Typ1 : constant Entity_Id := Base_Type (Etype (Op1));

   begin
      Binary_Op_Validity_Checks (N);

      --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if that
      --  means we no longer have a comparison operation, we are all done.

      Expand_Compare_Minimize_Eliminate_Overflow (N);

      if Nkind (N) /= N_Op_Ge then
         return;
      end if;

      --  Array type case

      if Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      --  Deal with boolean operands

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);

      --  If we still have comparison, and Vax_Float type, process it

      if Vax_Float (Typ1) and then Nkind (N) in N_Op_Compare then
         Expand_Vax_Comparison (N);
         return;
      end if;

      Optimize_Length_Comparison (N);
   end Expand_N_Op_Ge;

   --------------------
   -- Expand_N_Op_Gt --
   --------------------

   procedure Expand_N_Op_Gt (N : Node_Id) is
      Typ  : constant Entity_Id := Etype (N);
      Op1  : constant Node_Id   := Left_Opnd (N);
      Op2  : constant Node_Id   := Right_Opnd (N);
      Typ1 : constant Entity_Id := Base_Type (Etype (Op1));

   begin
      Binary_Op_Validity_Checks (N);

      --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if that
      --  means we no longer have a comparison operation, we are all done.

      Expand_Compare_Minimize_Eliminate_Overflow (N);

      if Nkind (N) /= N_Op_Gt then
         return;
      end if;

      --  Deal with array type operands

      if Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      --  Deal with boolean type operands

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);

      --  If we still have comparison, and Vax_Float type, process it

      if Vax_Float (Typ1) and then Nkind (N) in N_Op_Compare then
         Expand_Vax_Comparison (N);
         return;
      end if;

      Optimize_Length_Comparison (N);
   end Expand_N_Op_Gt;

   --------------------
   -- Expand_N_Op_Le --
   --------------------

   procedure Expand_N_Op_Le (N : Node_Id) is
      Typ  : constant Entity_Id := Etype (N);
      Op1  : constant Node_Id   := Left_Opnd (N);
      Op2  : constant Node_Id   := Right_Opnd (N);
      Typ1 : constant Entity_Id := Base_Type (Etype (Op1));

   begin
      Binary_Op_Validity_Checks (N);

      --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if that
      --  means we no longer have a comparison operation, we are all done.

      Expand_Compare_Minimize_Eliminate_Overflow (N);

      if Nkind (N) /= N_Op_Le then
         return;
      end if;

      --  Deal with array type operands

      if Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      --  Deal with Boolean type operands

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);

      --  If we still have comparison, and Vax_Float type, process it

      if Vax_Float (Typ1) and then Nkind (N) in N_Op_Compare then
         Expand_Vax_Comparison (N);
         return;
      end if;

      Optimize_Length_Comparison (N);
   end Expand_N_Op_Le;

   --------------------
   -- Expand_N_Op_Lt --
   --------------------

   procedure Expand_N_Op_Lt (N : Node_Id) is
      Typ  : constant Entity_Id := Etype (N);
      Op1  : constant Node_Id   := Left_Opnd (N);
      Op2  : constant Node_Id   := Right_Opnd (N);
      Typ1 : constant Entity_Id := Base_Type (Etype (Op1));

   begin
      Binary_Op_Validity_Checks (N);

      --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if that
      --  means we no longer have a comparison operation, we are all done.

      Expand_Compare_Minimize_Eliminate_Overflow (N);

      if Nkind (N) /= N_Op_Lt then
         return;
      end if;

      --  Deal with array type operands

      if Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      --  Deal with Boolean type operands

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);

      --  If we still have comparison, and Vax_Float type, process it

      if Vax_Float (Typ1) and then Nkind (N) in N_Op_Compare then
         Expand_Vax_Comparison (N);
         return;
      end if;

      Optimize_Length_Comparison (N);
   end Expand_N_Op_Lt;

   -----------------------
   -- Expand_N_Op_Minus --
   -----------------------

   procedure Expand_N_Op_Minus (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      Unary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      if not Backend_Overflow_Checks_On_Target
         and then Is_Signed_Integer_Type (Etype (N))
         and then Do_Overflow_Check (N)
      then
         --  Software overflow checking expands -expr into (0 - expr)

         Rewrite (N,
           Make_Op_Subtract (Loc,
             Left_Opnd  => Make_Integer_Literal (Loc, 0),
             Right_Opnd => Right_Opnd (N)));

         Analyze_And_Resolve (N, Typ);

      --  Vax floating-point types case

      elsif Vax_Float (Etype (N)) then
         Expand_Vax_Arith (N);
      end if;
   end Expand_N_Op_Minus;

   ---------------------
   -- Expand_N_Op_Mod --
   ---------------------

   procedure Expand_N_Op_Mod (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      DDC   : constant Boolean    := Do_Division_Check (N);

      Left  : Node_Id;
      Right : Node_Id;

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;

      pragma Warnings (Off, Lhi);

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      if Is_Integer_Type (Etype (N)) then
         Apply_Divide_Checks (N);

         --  All done if we don't have a MOD any more, which can happen as a
         --  result of overflow expansion in MINIMIZED or ELIMINATED modes.

         if Nkind (N) /= N_Op_Mod then
            return;
         end if;
      end if;

      --  Proceed with expansion of mod operator

      Left  := Left_Opnd (N);
      Right := Right_Opnd (N);

      Determine_Range (Right, ROK, Rlo, Rhi, Assume_Valid => True);
      Determine_Range (Left,  LOK, Llo, Lhi, Assume_Valid => True);

      --  Convert mod to rem if operands are both known to be non-negative, or
      --  both known to be non-positive (these are the cases in which rem and
      --  mod are the same, see (RM 4.5.5(28-30)). We do this since it is quite
      --  likely that this will improve the quality of code, (the operation now
      --  corresponds to the hardware remainder), and it does not seem likely
      --  that it could be harmful. It also avoids some cases of the elaborate
      --  expansion in Modify_Tree_For_C mode below (since Ada rem = C %).

      if (LOK and ROK)
        and then ((Llo >= 0 and then Rlo >= 0)
                    or else
                  (Lhi <= 0 and then Rhi <= 0))
      then
         Rewrite (N,
           Make_Op_Rem (Sloc (N),
             Left_Opnd  => Left_Opnd (N),
             Right_Opnd => Right_Opnd (N)));

         --  Instead of reanalyzing the node we do the analysis manually. This
         --  avoids anomalies when the replacement is done in an instance and
         --  is epsilon more efficient.

         Set_Entity            (N, Standard_Entity (S_Op_Rem));
         Set_Etype             (N, Typ);
         Set_Do_Division_Check (N, DDC);
         Expand_N_Op_Rem (N);
         Set_Analyzed (N);
         return;

      --  Otherwise, normal mod processing

      else
         --  Apply optimization x mod 1 = 0. We don't really need that with
         --  gcc, but it is useful with other back ends (e.g. AAMP), and is
         --  certainly harmless.

         if Is_Integer_Type (Etype (N))
           and then Compile_Time_Known_Value (Right)
           and then Expr_Value (Right) = Uint_1
         then
            --  Call Remove_Side_Effects to ensure that any side effects in
            --  the ignored left operand (in particular function calls to
            --  user defined functions) are properly executed.

            Remove_Side_Effects (Left);

            Rewrite (N, Make_Integer_Literal (Loc, 0));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

         --  If we still have a mod operator and we are in Modify_Tree_For_C
         --  mode, and we have a signed integer type, then here is where we do
         --  the rewrite in terms of Rem. Note this rewrite bypasses the need
         --  for the special handling of the annoying case of largest negative
         --  number mod minus one.

         if Nkind (N) = N_Op_Mod
           and then Is_Signed_Integer_Type (Typ)
           and then Modify_Tree_For_C
         then
            --  In the general case, we expand A mod B as

            --    Tnn : constant typ := A rem B;
            --    ..
            --    (if (A >= 0) = (B >= 0) then Tnn
            --     elsif Tnn = 0 then 0
            --     else Tnn + B)

            --  The comparison can be written simply as A >= 0 if we know that
            --  B >= 0 which is a very common case.

            --  An important optimization is when B is known at compile time
            --  to be 2**K for some constant. In this case we can simply AND
            --  the left operand with the bit string 2**K-1 (i.e. K 1-bits)
            --  and that works for both the positive and negative cases.

            declare
               P2 : constant Nat := Power_Of_Two (Right);

            begin
               if P2 /= 0 then
                  Rewrite (N,
                    Unchecked_Convert_To (Typ,
                      Make_Op_And (Loc,
                        Left_Opnd  =>
                          Unchecked_Convert_To
                            (Corresponding_Unsigned_Type (Typ), Left),
                        Right_Opnd =>
                          Make_Integer_Literal (Loc, 2 ** P2 - 1))));
                  Analyze_And_Resolve (N, Typ);
                  return;
               end if;
            end;

            --  Here for the full rewrite

            declare
               Tnn : constant Entity_Id := Make_Temporary (Sloc (N), 'T', N);
               Cmp : Node_Id;

            begin
               Cmp :=
                 Make_Op_Ge (Loc,
                   Left_Opnd  => Duplicate_Subexpr_No_Checks (Left),
                   Right_Opnd => Make_Integer_Literal (Loc, 0));

               if not LOK or else Rlo < 0 then
                  Cmp :=
                     Make_Op_Eq (Loc,
                       Left_Opnd  => Cmp,
                       Right_Opnd =>
                         Make_Op_Ge (Loc,
                           Left_Opnd  => Duplicate_Subexpr_No_Checks (Right),
                           Right_Opnd => Make_Integer_Literal (Loc, 0)));
               end if;

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Tnn,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Typ, Loc),
                   Expression          =>
                     Make_Op_Rem (Loc,
                       Left_Opnd  => Left,
                       Right_Opnd => Right)));

               Rewrite (N,
                 Make_If_Expression (Loc,
                   Expressions => New_List (
                     Cmp,
                     New_Occurrence_Of (Tnn, Loc),
                     Make_If_Expression (Loc,
                       Is_Elsif    => True,
                       Expressions => New_List (
                         Make_Op_Eq (Loc,
                           Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                           Right_Opnd => Make_Integer_Literal (Loc, 0)),
                         Make_Integer_Literal (Loc, 0),
                         Make_Op_Add (Loc,
                           Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                           Right_Opnd =>
                             Duplicate_Subexpr_No_Checks (Right)))))));

               Analyze_And_Resolve (N, Typ);
               return;
            end;
         end if;

         --  Deal with annoying case of largest negative number mod minus one.
         --  Gigi may not handle this case correctly, because on some targets,
         --  the mod value is computed using a divide instruction which gives
         --  an overflow trap for this case.

         --  It would be a bit more efficient to figure out which targets
         --  this is really needed for, but in practice it is reasonable
         --  to do the following special check in all cases, since it means
         --  we get a clearer message, and also the overhead is minimal given
         --  that division is expensive in any case.

         --  In fact the check is quite easy, if the right operand is -1, then
         --  the mod value is always 0, and we can just ignore the left operand
         --  completely in this case.

         --  This only applies if we still have a mod operator. Skip if we
         --  have already rewritten this (e.g. in the case of eliminated
         --  overflow checks which have driven us into bignum mode).

         if Nkind (N) = N_Op_Mod then

            --  The operand type may be private (e.g. in the expansion of an
            --  intrinsic operation) so we must use the underlying type to get
            --  the bounds, and convert the literals explicitly.

            LLB :=
              Expr_Value
                (Type_Low_Bound (Base_Type (Underlying_Type (Etype (Left)))));

            if ((not ROK) or else (Rlo <= (-1) and then (-1) <= Rhi))
              and then ((not LOK) or else (Llo = LLB))
            then
               Rewrite (N,
                 Make_If_Expression (Loc,
                   Expressions => New_List (
                     Make_Op_Eq (Loc,
                       Left_Opnd => Duplicate_Subexpr (Right),
                       Right_Opnd =>
                         Unchecked_Convert_To (Typ,
                           Make_Integer_Literal (Loc, -1))),
                     Unchecked_Convert_To (Typ,
                       Make_Integer_Literal (Loc, Uint_0)),
                     Relocate_Node (N))));

               Set_Analyzed (Next (Next (First (Expressions (N)))));
               Analyze_And_Resolve (N, Typ);
            end if;
         end if;
      end if;
   end Expand_N_Op_Mod;

   --------------------------
   -- Expand_N_Op_Multiply --
   --------------------------

   procedure Expand_N_Op_Multiply (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Lop : constant Node_Id    := Left_Opnd (N);
      Rop : constant Node_Id    := Right_Opnd (N);

      Lp2 : constant Boolean :=
              Nkind (Lop) = N_Op_Expon and then Is_Power_Of_2_For_Shift (Lop);
      Rp2 : constant Boolean :=
              Nkind (Rop) = N_Op_Expon and then Is_Power_Of_2_For_Shift (Rop);

      Ltyp : constant Entity_Id  := Etype (Lop);
      Rtyp : constant Entity_Id  := Etype (Rop);
      Typ  : Entity_Id           := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Special optimizations for integer types

      if Is_Integer_Type (Typ) then

         --  N * 0 = 0 for integer types

         if Compile_Time_Known_Value (Rop)
           and then Expr_Value (Rop) = Uint_0
         then
            --  Call Remove_Side_Effects to ensure that any side effects in
            --  the ignored left operand (in particular function calls to
            --  user defined functions) are properly executed.

            Remove_Side_Effects (Lop);

            Rewrite (N, Make_Integer_Literal (Loc, Uint_0));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

         --  Similar handling for 0 * N = 0

         if Compile_Time_Known_Value (Lop)
           and then Expr_Value (Lop) = Uint_0
         then
            Remove_Side_Effects (Rop);
            Rewrite (N, Make_Integer_Literal (Loc, Uint_0));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

         --  N * 1 = 1 * N = N for integer types

         --  This optimisation is not done if we are going to
         --  rewrite the product 1 * 2 ** N to a shift.

         if Compile_Time_Known_Value (Rop)
           and then Expr_Value (Rop) = Uint_1
           and then not Lp2
         then
            Rewrite (N, Lop);
            return;

         elsif Compile_Time_Known_Value (Lop)
           and then Expr_Value (Lop) = Uint_1
           and then not Rp2
         then
            Rewrite (N, Rop);
            return;
         end if;
      end if;

      --  Convert x * 2 ** y to Shift_Left (x, y). Note that the fact that
      --  Is_Power_Of_2_For_Shift is set means that we know that our left
      --  operand is an integer, as required for this to work.

      if Rp2 then
         if Lp2 then

            --  Convert 2 ** A * 2 ** B into  2 ** (A + B)

            Rewrite (N,
              Make_Op_Expon (Loc,
                Left_Opnd => Make_Integer_Literal (Loc, 2),
                Right_Opnd =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => Right_Opnd (Lop),
                    Right_Opnd => Right_Opnd (Rop))));
            Analyze_And_Resolve (N, Typ);
            return;

         else
            --  If the result is modular, perform the reduction of the result
            --  appropriately.

            if Is_Modular_Integer_Type (Typ)
              and then not Non_Binary_Modulus (Typ)
            then
               Rewrite (N,
                 Make_Op_And (Loc,
                   Left_Opnd  =>
                     Make_Op_Shift_Left (Loc,
                       Left_Opnd  => Lop,
                       Right_Opnd =>
                         Convert_To (Standard_Natural, Right_Opnd (Rop))),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Modulus (Typ) - 1)));

            else
               Rewrite (N,
                 Make_Op_Shift_Left (Loc,
                   Left_Opnd  => Lop,
                   Right_Opnd =>
                     Convert_To (Standard_Natural, Right_Opnd (Rop))));
            end if;

            Analyze_And_Resolve (N, Typ);
            return;
         end if;

      --  Same processing for the operands the other way round

      elsif Lp2 then
         if Is_Modular_Integer_Type (Typ)
           and then not Non_Binary_Modulus (Typ)
         then
            Rewrite (N,
              Make_Op_And (Loc,
                Left_Opnd  =>
                  Make_Op_Shift_Left (Loc,
                    Left_Opnd  => Rop,
                    Right_Opnd =>
                      Convert_To (Standard_Natural, Right_Opnd (Lop))),
                Right_Opnd =>
                   Make_Integer_Literal (Loc, Modulus (Typ) - 1)));

         else
            Rewrite (N,
              Make_Op_Shift_Left (Loc,
                Left_Opnd  => Rop,
                Right_Opnd =>
                  Convert_To (Standard_Natural, Right_Opnd (Lop))));
         end if;

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  Do required fixup of universal fixed operation

      if Typ = Universal_Fixed then
         Fixup_Universal_Fixed_Operation (N);
         Typ := Etype (N);
      end if;

      --  Multiplications with fixed-point results

      if Is_Fixed_Point_Type (Typ) then

         --  No special processing if Treat_Fixed_As_Integer is set, since from
         --  a semantic point of view such operations are simply integer
         --  operations and will be treated that way.

         if not Treat_Fixed_As_Integer (N) then

            --  Case of fixed * integer => fixed

            if Is_Integer_Type (Rtyp) then
               Expand_Multiply_Fixed_By_Integer_Giving_Fixed (N);

            --  Case of integer * fixed => fixed

            elsif Is_Integer_Type (Ltyp) then
               Expand_Multiply_Integer_By_Fixed_Giving_Fixed (N);

            --  Case of fixed * fixed => fixed

            else
               Expand_Multiply_Fixed_By_Fixed_Giving_Fixed (N);
            end if;
         end if;

      --  Other cases of multiplication of fixed-point operands. Again we
      --  exclude the cases where Treat_Fixed_As_Integer flag is set.

      elsif (Is_Fixed_Point_Type (Ltyp) or else Is_Fixed_Point_Type (Rtyp))
        and then not Treat_Fixed_As_Integer (N)
      then
         if Is_Integer_Type (Typ) then
            Expand_Multiply_Fixed_By_Fixed_Giving_Integer (N);
         else
            pragma Assert (Is_Floating_Point_Type (Typ));
            Expand_Multiply_Fixed_By_Fixed_Giving_Float (N);
         end if;

      --  Mixed-mode operations can appear in a non-static universal context,
      --  in which case the integer argument must be converted explicitly.

      elsif Typ = Universal_Real and then Is_Integer_Type (Rtyp) then
         Rewrite (Rop, Convert_To (Universal_Real, Relocate_Node (Rop)));
         Analyze_And_Resolve (Rop, Universal_Real);

      elsif Typ = Universal_Real and then Is_Integer_Type (Ltyp) then
         Rewrite (Lop, Convert_To (Universal_Real, Relocate_Node (Lop)));
         Analyze_And_Resolve (Lop, Universal_Real);

      --  Non-fixed point cases, check software overflow checking required

      elsif Is_Signed_Integer_Type (Etype (N)) then
         Apply_Arithmetic_Overflow_Check (N);

      --  Deal with VAX float case

      elsif Vax_Float (Typ) then
         Expand_Vax_Arith (N);
         return;
      end if;
   end Expand_N_Op_Multiply;

   --------------------
   -- Expand_N_Op_Ne --
   --------------------

   procedure Expand_N_Op_Ne (N : Node_Id) is
      Typ : constant Entity_Id := Etype (Left_Opnd (N));

   begin
      --  Case of elementary type with standard operator

      if Is_Elementary_Type (Typ)
        and then Sloc (Entity (N)) = Standard_Location
      then
         Binary_Op_Validity_Checks (N);

         --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if
         --  means we no longer have a /= operation, we are all done.

         Expand_Compare_Minimize_Eliminate_Overflow (N);

         if Nkind (N) /= N_Op_Ne then
            return;
         end if;

         --  Boolean types (requiring handling of non-standard case)

         if Is_Boolean_Type (Typ) then
            Adjust_Condition (Left_Opnd (N));
            Adjust_Condition (Right_Opnd (N));
            Set_Etype (N, Standard_Boolean);
            Adjust_Result_Type (N, Typ);
         end if;

         Rewrite_Comparison (N);

         --  If we still have comparison for Vax_Float, process it

         if Vax_Float (Typ) and then Nkind (N) in N_Op_Compare  then
            Expand_Vax_Comparison (N);
            return;
         end if;

      --  For all cases other than elementary types, we rewrite node as the
      --  negation of an equality operation, and reanalyze. The equality to be
      --  used is defined in the same scope and has the same signature. This
      --  signature must be set explicitly since in an instance it may not have
      --  the same visibility as in the generic unit. This avoids duplicating
      --  or factoring the complex code for record/array equality tests etc.

      else
         declare
            Loc : constant Source_Ptr := Sloc (N);
            Neg : Node_Id;
            Ne  : constant Entity_Id := Entity (N);

         begin
            Binary_Op_Validity_Checks (N);

            Neg :=
              Make_Op_Not (Loc,
                Right_Opnd =>
                  Make_Op_Eq (Loc,
                    Left_Opnd =>  Left_Opnd (N),
                    Right_Opnd => Right_Opnd (N)));
            Set_Paren_Count (Right_Opnd (Neg), 1);

            if Scope (Ne) /= Standard_Standard then
               Set_Entity (Right_Opnd (Neg), Corresponding_Equality (Ne));
            end if;

            --  For navigation purposes, we want to treat the inequality as an
            --  implicit reference to the corresponding equality. Preserve the
            --  Comes_From_ source flag to generate proper Xref entries.

            Preserve_Comes_From_Source (Neg, N);
            Preserve_Comes_From_Source (Right_Opnd (Neg), N);
            Rewrite (N, Neg);
            Analyze_And_Resolve (N, Standard_Boolean);
         end;
      end if;

      Optimize_Length_Comparison (N);
   end Expand_N_Op_Ne;

   ---------------------
   -- Expand_N_Op_Not --
   ---------------------

   --  If the argument is other than a Boolean array type, there is no special
   --  expansion required, except for VMS operations on signed integers.

   --  For the packed case, we call the special routine in Exp_Pakd, except
   --  that if the component size is greater than one, we use the standard
   --  routine generating a gruesome loop (it is so peculiar to have packed
   --  arrays with non-standard Boolean representations anyway, so it does not
   --  matter that we do not handle this case efficiently).

   --  For the unpacked case (and for the special packed case where we have non
   --  standard Booleans, as discussed above), we generate and insert into the
   --  tree the following function definition:

   --     function Nnnn (A : arr) is
   --       B : arr;
   --     begin
   --       for J in a'range loop
   --          B (J) := not A (J);
   --       end loop;
   --       return B;
   --     end Nnnn;

   --  Here arr is the actual subtype of the parameter (and hence always
   --  constrained). Then we replace the not with a call to this function.

   procedure Expand_N_Op_Not (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Opnd : Node_Id;
      Arr  : Entity_Id;
      A    : Entity_Id;
      B    : Entity_Id;
      J    : Entity_Id;
      A_J  : Node_Id;
      B_J  : Node_Id;

      Func_Name      : Entity_Id;
      Loop_Statement : Node_Id;

   begin
      Unary_Op_Validity_Checks (N);

      --  For boolean operand, deal with non-standard booleans

      if Is_Boolean_Type (Typ) then
         Adjust_Condition (Right_Opnd (N));
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
         return;
      end if;

      --  For the VMS "not" on signed integer types, use conversion to and from
      --  a predefined modular type.

      if Is_VMS_Operator (Entity (N)) then
         declare
            Rtyp : Entity_Id;
            Utyp : Entity_Id;

         begin
            --  If this is a derived type, retrieve original VMS type so that
            --  the proper sized type is used for intermediate values.

            if Is_Derived_Type (Typ) then
               Rtyp := First_Subtype (Etype (Typ));
            else
               Rtyp := Typ;
            end if;

            --  The proper unsigned type must have a size compatible with the
            --  operand, to prevent misalignment.

            if RM_Size (Rtyp) <= 8 then
               Utyp := RTE (RE_Unsigned_8);

            elsif RM_Size (Rtyp) <= 16 then
               Utyp := RTE (RE_Unsigned_16);

            elsif RM_Size (Rtyp) = RM_Size (Standard_Unsigned) then
               Utyp := RTE (RE_Unsigned_32);

            else
               Utyp := RTE (RE_Long_Long_Unsigned);
            end if;

            Rewrite (N,
              Unchecked_Convert_To (Typ,
                Make_Op_Not (Loc,
                  Unchecked_Convert_To (Utyp, Right_Opnd (N)))));
            Analyze_And_Resolve (N, Typ);
            return;
         end;
      end if;

      --  Only array types need any other processing

      if not Is_Array_Type (Typ) then
         return;
      end if;

      --  Case of array operand. If bit packed with a component size of 1,
      --  handle it in Exp_Pakd if the operand is known to be aligned.

      if Is_Bit_Packed_Array (Typ)
        and then Component_Size (Typ) = 1
        and then not Is_Possibly_Unaligned_Object (Right_Opnd (N))
      then
         Expand_Packed_Not (N);
         return;
      end if;

      --  Case of array operand which is not bit-packed. If the context is
      --  a safe assignment, call in-place operation, If context is a larger
      --  boolean expression in the context of a safe assignment, expansion is
      --  done by enclosing operation.

      Opnd := Relocate_Node (Right_Opnd (N));
      Convert_To_Actual_Subtype (Opnd);
      Arr := Etype (Opnd);
      Ensure_Defined (Arr, N);
      Silly_Boolean_Array_Not_Test (N, Arr);

      if Nkind (Parent (N)) = N_Assignment_Statement then
         if Safe_In_Place_Array_Op (Name (Parent (N)), N, Empty) then
            Build_Boolean_Array_Proc_Call (Parent (N), Opnd, Empty);
            return;

         --  Special case the negation of a binary operation

         elsif Nkind_In (Opnd, N_Op_And, N_Op_Or, N_Op_Xor)
           and then Safe_In_Place_Array_Op
                      (Name (Parent (N)), Left_Opnd (Opnd), Right_Opnd (Opnd))
         then
            Build_Boolean_Array_Proc_Call (Parent (N), Opnd, Empty);
            return;
         end if;

      elsif Nkind (Parent (N)) in N_Binary_Op
        and then Nkind (Parent (Parent (N))) = N_Assignment_Statement
      then
         declare
            Op1 : constant Node_Id := Left_Opnd  (Parent (N));
            Op2 : constant Node_Id := Right_Opnd (Parent (N));
            Lhs : constant Node_Id := Name (Parent (Parent (N)));

         begin
            if Safe_In_Place_Array_Op (Lhs, Op1, Op2) then

               --  (not A) op (not B) can be reduced to a single call

               if N = Op1 and then Nkind (Op2) = N_Op_Not then
                  return;

               elsif N = Op2 and then Nkind (Op1) = N_Op_Not then
                  return;

               --  A xor (not B) can also be special-cased

               elsif N = Op2 and then Nkind (Parent (N)) = N_Op_Xor then
                  return;
               end if;
            end if;
         end;
      end if;

      A := Make_Defining_Identifier (Loc, Name_uA);
      B := Make_Defining_Identifier (Loc, Name_uB);
      J := Make_Defining_Identifier (Loc, Name_uJ);

      A_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Occurrence_Of (A, Loc),
          Expressions => New_List (New_Occurrence_Of (J, Loc)));

      B_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Occurrence_Of (B, Loc),
          Expressions => New_List (New_Occurrence_Of (J, Loc)));

      Loop_Statement :=
        Make_Implicit_Loop_Statement (N,
          Identifier => Empty,

          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier         => J,
                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => Make_Identifier (Loc, Chars (A)),
                      Attribute_Name => Name_Range))),

          Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => B_J,
              Expression => Make_Op_Not (Loc, A_J))));

      Func_Name := Make_Temporary (Loc, 'N');
      Set_Is_Inlined (Func_Name);

      Insert_Action (N,
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Func_Name,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => A,
                  Parameter_Type      => New_Occurrence_Of (Typ, Loc))),
              Result_Definition => New_Occurrence_Of (Typ, Loc)),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => B,
              Object_Definition   => New_Occurrence_Of (Arr, Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Loop_Statement,
                Make_Simple_Return_Statement (Loc,
                  Expression => Make_Identifier (Loc, Chars (B)))))));

      Rewrite (N,
        Make_Function_Call (Loc,
          Name                   => New_Occurrence_Of (Func_Name, Loc),
          Parameter_Associations => New_List (Opnd)));

      Analyze_And_Resolve (N, Typ);
   end Expand_N_Op_Not;

   --------------------
   -- Expand_N_Op_Or --
   --------------------

   procedure Expand_N_Op_Or (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      if Is_Array_Type (Etype (N)) then
         Expand_Boolean_Operator (N);

      elsif Is_Boolean_Type (Etype (N)) then
         Adjust_Condition (Left_Opnd (N));
         Adjust_Condition (Right_Opnd (N));
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);

      elsif Is_Intrinsic_Subprogram (Entity (N)) then
         Expand_Intrinsic_Call (N, Entity (N));

      end if;
   end Expand_N_Op_Or;

   ----------------------
   -- Expand_N_Op_Plus --
   ----------------------

   procedure Expand_N_Op_Plus (N : Node_Id) is
   begin
      Unary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;
   end Expand_N_Op_Plus;

   ---------------------
   -- Expand_N_Op_Rem --
   ---------------------

   procedure Expand_N_Op_Rem (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

      Left  : Node_Id;
      Right : Node_Id;

      Lo : Uint;
      Hi : Uint;
      OK : Boolean;

      Lneg : Boolean;
      Rneg : Boolean;
      --  Set if corresponding operand can be negative

      pragma Unreferenced (Hi);

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      if Is_Integer_Type (Etype (N)) then
         Apply_Divide_Checks (N);

         --  All done if we don't have a REM any more, which can happen as a
         --  result of overflow expansion in MINIMIZED or ELIMINATED modes.

         if Nkind (N) /= N_Op_Rem then
            return;
         end if;
      end if;

      --  Proceed with expansion of REM

      Left  := Left_Opnd (N);
      Right := Right_Opnd (N);

      --  Apply optimization x rem 1 = 0. We don't really need that with gcc,
      --  but it is useful with other back ends (e.g. AAMP), and is certainly
      --  harmless.

      if Is_Integer_Type (Etype (N))
        and then Compile_Time_Known_Value (Right)
        and then Expr_Value (Right) = Uint_1
      then
         --  Call Remove_Side_Effects to ensure that any side effects in the
         --  ignored left operand (in particular function calls to user defined
         --  functions) are properly executed.

         Remove_Side_Effects (Left);

         Rewrite (N, Make_Integer_Literal (Loc, 0));
         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  Deal with annoying case of largest negative number remainder minus
      --  one. Gigi may not handle this case correctly, because on some
      --  targets, the mod value is computed using a divide instruction
      --  which gives an overflow trap for this case.

      --  It would be a bit more efficient to figure out which targets this
      --  is really needed for, but in practice it is reasonable to do the
      --  following special check in all cases, since it means we get a clearer
      --  message, and also the overhead is minimal given that division is
      --  expensive in any case.

      --  In fact the check is quite easy, if the right operand is -1, then
      --  the remainder is always 0, and we can just ignore the left operand
      --  completely in this case.

      Determine_Range (Right, OK, Lo, Hi, Assume_Valid => True);
      Lneg := (not OK) or else Lo < 0;

      Determine_Range (Left,  OK, Lo, Hi, Assume_Valid => True);
      Rneg := (not OK) or else Lo < 0;

      --  We won't mess with trying to find out if the left operand can really
      --  be the largest negative number (that's a pain in the case of private
      --  types and this is really marginal). We will just assume that we need
      --  the test if the left operand can be negative at all.

      if Lneg and Rneg then
         Rewrite (N,
           Make_If_Expression (Loc,
             Expressions => New_List (
               Make_Op_Eq (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Right),
                 Right_Opnd =>
                   Unchecked_Convert_To (Typ, Make_Integer_Literal (Loc, -1))),

               Unchecked_Convert_To (Typ,
                 Make_Integer_Literal (Loc, Uint_0)),

               Relocate_Node (N))));

         Set_Analyzed (Next (Next (First (Expressions (N)))));
         Analyze_And_Resolve (N, Typ);
      end if;
   end Expand_N_Op_Rem;

   -----------------------------
   -- Expand_N_Op_Rotate_Left --
   -----------------------------

   procedure Expand_N_Op_Rotate_Left (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);

      --  If we are in Modify_Tree_For_C mode, there is no rotate left in C,
      --  so we rewrite in terms of logical shifts

      --    Shift_Left (Num, Bits) or Shift_Right (num, Esize - Bits)

      --  where Bits is the shift count mod Esize (the mod operation here
      --  deals with ludicrous large shift counts, which are apparently OK).

      --  What about non-binary modulus ???

      declare
         Loc : constant Source_Ptr := Sloc (N);
         Rtp : constant Entity_Id  := Etype (Right_Opnd (N));
         Typ : constant Entity_Id  := Etype (N);

      begin
         if Modify_Tree_For_C then
            Rewrite (Right_Opnd (N),
              Make_Op_Rem (Loc,
                Left_Opnd  => Relocate_Node (Right_Opnd (N)),
                Right_Opnd => Make_Integer_Literal (Loc, Esize (Typ))));

            Analyze_And_Resolve (Right_Opnd (N), Rtp);

            Rewrite (N,
              Make_Op_Or (Loc,
                Left_Opnd =>
                  Make_Op_Shift_Left (Loc,
                    Left_Opnd  => Left_Opnd (N),
                    Right_Opnd => Right_Opnd (N)),

                Right_Opnd =>
                  Make_Op_Shift_Right (Loc,
                    Left_Opnd  => Duplicate_Subexpr_No_Checks (Left_Opnd (N)),
                    Right_Opnd =>
                      Make_Op_Subtract (Loc,
                        Left_Opnd  => Make_Integer_Literal (Loc, Esize (Typ)),
                        Right_Opnd =>
                          Duplicate_Subexpr_No_Checks (Right_Opnd (N))))));

            Analyze_And_Resolve (N, Typ);
         end if;
      end;
   end Expand_N_Op_Rotate_Left;

   ------------------------------
   -- Expand_N_Op_Rotate_Right --
   ------------------------------

   procedure Expand_N_Op_Rotate_Right (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);

      --  If we are in Modify_Tree_For_C mode, there is no rotate right in C,
      --  so we rewrite in terms of logical shifts

      --    Shift_Right (Num, Bits) or Shift_Left (num, Esize - Bits)

      --  where Bits is the shift count mod Esize (the mod operation here
      --  deals with ludicrous large shift counts, which are apparently OK).

      --  What about non-binary modulus ???

      declare
         Loc : constant Source_Ptr := Sloc (N);
         Rtp : constant Entity_Id  := Etype (Right_Opnd (N));
         Typ : constant Entity_Id  := Etype (N);

      begin
         Rewrite (Right_Opnd (N),
           Make_Op_Rem (Loc,
             Left_Opnd  => Relocate_Node (Right_Opnd (N)),
             Right_Opnd => Make_Integer_Literal (Loc, Esize (Typ))));

         Analyze_And_Resolve (Right_Opnd (N), Rtp);

         if Modify_Tree_For_C then
            Rewrite (N,
              Make_Op_Or (Loc,
                Left_Opnd =>
                  Make_Op_Shift_Right (Loc,
                    Left_Opnd  => Left_Opnd (N),
                    Right_Opnd => Right_Opnd (N)),

                Right_Opnd =>
                  Make_Op_Shift_Left (Loc,
                    Left_Opnd  => Duplicate_Subexpr_No_Checks (Left_Opnd (N)),
                    Right_Opnd =>
                      Make_Op_Subtract (Loc,
                        Left_Opnd  => Make_Integer_Literal (Loc, Esize (Typ)),
                        Right_Opnd =>
                          Duplicate_Subexpr_No_Checks (Right_Opnd (N))))));

            Analyze_And_Resolve (N, Typ);
         end if;
      end;
   end Expand_N_Op_Rotate_Right;

   ----------------------------
   -- Expand_N_Op_Shift_Left --
   ----------------------------

   --  Note: nothing in this routine depends on left as opposed to right shifts
   --  so we share the routine for expanding shift right operations.

   procedure Expand_N_Op_Shift_Left (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);

      --  If we are in Modify_Tree_For_C mode, then ensure that the right
      --  operand is not greater than the word size (since that would not
      --  be defined properly by the corresponding C shift operator).

      if Modify_Tree_For_C then
         declare
            Right : constant Node_Id    := Right_Opnd (N);
            Loc   : constant Source_Ptr := Sloc (Right);
            Typ   : constant Entity_Id  := Etype (N);
            Siz   : constant Uint       := Esize (Typ);
            Orig  : Node_Id;
            OK    : Boolean;
            Lo    : Uint;
            Hi    : Uint;

         begin
            if Compile_Time_Known_Value (Right) then
               if Expr_Value (Right) >= Siz then
                  Rewrite (N, Make_Integer_Literal (Loc, 0));
                  Analyze_And_Resolve (N, Typ);
               end if;

            --  Not compile time known, find range

            else
               Determine_Range (Right, OK, Lo, Hi, Assume_Valid => True);

               --  Nothing to do if known to be OK range, otherwise expand

               if not OK or else Hi >= Siz then

                  --  Prevent recursion on copy of shift node

                  Orig := Relocate_Node (N);
                  Set_Analyzed (Orig);

                  --  Now do the rewrite

                  Rewrite (N,
                     Make_If_Expression (Loc,
                       Expressions => New_List (
                         Make_Op_Ge (Loc,
                           Left_Opnd  => Duplicate_Subexpr_Move_Checks (Right),
                           Right_Opnd => Make_Integer_Literal (Loc, Siz)),
                         Make_Integer_Literal (Loc, 0),
                         Orig)));
                  Analyze_And_Resolve (N, Typ);
               end if;
            end if;
         end;
      end if;
   end Expand_N_Op_Shift_Left;

   -----------------------------
   -- Expand_N_Op_Shift_Right --
   -----------------------------

   procedure Expand_N_Op_Shift_Right (N : Node_Id) is
   begin
      --  Share shift left circuit

      Expand_N_Op_Shift_Left (N);
   end Expand_N_Op_Shift_Right;

   ----------------------------------------
   -- Expand_N_Op_Shift_Right_Arithmetic --
   ----------------------------------------

   procedure Expand_N_Op_Shift_Right_Arithmetic (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);

      --  If we are in Modify_Tree_For_C mode, there is no shift right
      --  arithmetic in C, so we rewrite in terms of logical shifts.

      --    Shift_Right (Num, Bits) or
      --      (if Num >= Sign
      --       then not (Shift_Right (Mask, bits))
      --       else 0)

      --  Here Mask is all 1 bits (2**size - 1), and Sign is 2**(size - 1)

      --  Note: in almost all C compilers it would work to just shift a
      --  signed integer right, but it's undefined and we cannot rely on it.

      --  Note: the above works fine for shift counts greater than or equal
      --  to the word size, since in this case (not (Shift_Right (Mask, bits)))
      --  generates all 1'bits.

      --  What about non-binary modulus ???

      declare
         Loc   : constant Source_Ptr := Sloc (N);
         Typ   : constant Entity_Id  := Etype (N);
         Sign  : constant Uint       := 2 ** (Esize (Typ) - 1);
         Mask  : constant Uint       := (2 ** Esize (Typ)) - 1;
         Left  : constant Node_Id    := Left_Opnd (N);
         Right : constant Node_Id    := Right_Opnd (N);
         Maskx : Node_Id;

      begin
         if Modify_Tree_For_C then

            --  Here if not (Shift_Right (Mask, bits)) can be computed at
            --  compile time as a single constant.

            if Compile_Time_Known_Value (Right) then
               declare
                  Val : constant Uint := Expr_Value (Right);

               begin
                  if Val >= Esize (Typ) then
                     Maskx := Make_Integer_Literal (Loc, Mask);

                  else
                     Maskx :=
                       Make_Integer_Literal (Loc,
                         Intval => Mask - (Mask / (2 ** Expr_Value (Right))));
                  end if;
               end;

            else
               Maskx :=
                 Make_Op_Not (Loc,
                   Right_Opnd =>
                     Make_Op_Shift_Right (Loc,
                       Left_Opnd  => Make_Integer_Literal (Loc, Mask),
                       Right_Opnd => Duplicate_Subexpr_No_Checks (Right)));
            end if;

            --  Now do the rewrite

            Rewrite (N,
              Make_Op_Or (Loc,
                Left_Opnd =>
                  Make_Op_Shift_Right (Loc,
                    Left_Opnd  => Left,
                    Right_Opnd => Right),
                Right_Opnd =>
                  Make_If_Expression (Loc,
                    Expressions => New_List (
                      Make_Op_Ge (Loc,
                        Left_Opnd  => Duplicate_Subexpr_No_Checks (Left),
                        Right_Opnd => Make_Integer_Literal (Loc, Sign)),
                      Maskx,
                      Make_Integer_Literal (Loc, 0)))));
            Analyze_And_Resolve (N, Typ);
         end if;
      end;
   end Expand_N_Op_Shift_Right_Arithmetic;

   --------------------------
   -- Expand_N_Op_Subtract --
   --------------------------

   procedure Expand_N_Op_Subtract (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  N - 0 = N for integer types

      if Is_Integer_Type (Typ)
        and then Compile_Time_Known_Value (Right_Opnd (N))
        and then Expr_Value (Right_Opnd (N)) = 0
      then
         Rewrite (N, Left_Opnd (N));
         return;
      end if;

      --  Arithmetic overflow checks for signed integer/fixed point types

      if Is_Signed_Integer_Type (Typ) or else Is_Fixed_Point_Type (Typ) then
         Apply_Arithmetic_Overflow_Check (N);

      --  VAX floating-point types case

      elsif Vax_Float (Typ) then
         Expand_Vax_Arith (N);
      end if;
   end Expand_N_Op_Subtract;

   ---------------------
   -- Expand_N_Op_Xor --
   ---------------------

   procedure Expand_N_Op_Xor (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      if Is_Array_Type (Etype (N)) then
         Expand_Boolean_Operator (N);

      elsif Is_Boolean_Type (Etype (N)) then
         Adjust_Condition (Left_Opnd (N));
         Adjust_Condition (Right_Opnd (N));
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);

      elsif Is_Intrinsic_Subprogram (Entity (N)) then
         Expand_Intrinsic_Call (N, Entity (N));

      end if;
   end Expand_N_Op_Xor;

   ----------------------
   -- Expand_N_Or_Else --
   ----------------------

   procedure Expand_N_Or_Else (N : Node_Id)
     renames Expand_Short_Circuit_Operator;

   -----------------------------------
   -- Expand_N_Qualified_Expression --
   -----------------------------------

   procedure Expand_N_Qualified_Expression (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Target_Type : constant Entity_Id := Entity (Subtype_Mark (N));

   begin
      --  Do validity check if validity checking operands

      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Operand);
      end if;

      --  Apply possible constraint check

      Apply_Constraint_Check (Operand, Target_Type, No_Sliding => True);

      if Do_Range_Check (Operand) then
         Set_Do_Range_Check (Operand, False);
         Generate_Range_Check (Operand, Target_Type, CE_Range_Check_Failed);
      end if;
   end Expand_N_Qualified_Expression;

   ------------------------------------
   -- Expand_N_Quantified_Expression --
   ------------------------------------

   --  We expand:

   --    for all X in range => Cond

   --  into:

   --        T := True;
   --        for X in range loop
   --           if not Cond then
   --              T := False;
   --              exit;
   --           end if;
   --        end loop;

   --  Similarly, an existentially quantified expression:

   --    for some X in range => Cond

   --  becomes:

   --        T := False;
   --        for X in range loop
   --           if Cond then
   --              T := True;
   --              exit;
   --           end if;
   --        end loop;

   --  In both cases, the iteration may be over a container in which case it is
   --  given by an iterator specification, not a loop parameter specification.

   procedure Expand_N_Quantified_Expression (N : Node_Id) is
      Actions   : constant List_Id    := New_List;
      For_All   : constant Boolean    := All_Present (N);
      Iter_Spec : constant Node_Id    := Iterator_Specification (N);
      Loc       : constant Source_Ptr := Sloc (N);
      Loop_Spec : constant Node_Id    := Loop_Parameter_Specification (N);
      Cond      : Node_Id;
      Flag      : Entity_Id;
      Scheme    : Node_Id;
      Stmts     : List_Id;

   begin
      --  Create the declaration of the flag which tracks the status of the
      --  quantified expression. Generate:

      --    Flag : Boolean := (True | False);

      Flag := Make_Temporary (Loc, 'T', N);

      Append_To (Actions,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Flag,
          Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          =>
            New_Occurrence_Of (Boolean_Literals (For_All), Loc)));

      --  Construct the circuitry which tracks the status of the quantified
      --  expression. Generate:

      --    if [not] Cond then
      --       Flag := (False | True);
      --       exit;
      --    end if;

      Cond := Relocate_Node (Condition (N));

      if For_All then
         Cond := Make_Op_Not (Loc, Cond);
      end if;

      Stmts := New_List (
        Make_Implicit_If_Statement (N,
          Condition       => Cond,
          Then_Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => New_Occurrence_Of (Flag, Loc),
              Expression =>
                New_Occurrence_Of (Boolean_Literals (not For_All), Loc)),
            Make_Exit_Statement (Loc))));

      --  Build the loop equivalent of the quantified expression

      if Present (Iter_Spec) then
         Scheme :=
           Make_Iteration_Scheme (Loc,
             Iterator_Specification => Iter_Spec);
      else
         Scheme :=
           Make_Iteration_Scheme (Loc,
             Loop_Parameter_Specification => Loop_Spec);
      end if;

      Append_To (Actions,
        Make_Loop_Statement (Loc,
          Iteration_Scheme => Scheme,
          Statements       => Stmts,
          End_Label        => Empty));

      --  Transform the quantified expression

      Rewrite (N,
        Make_Expression_With_Actions (Loc,
          Expression => New_Occurrence_Of (Flag, Loc),
          Actions    => Actions));
      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_N_Quantified_Expression;

   ---------------------------------
   -- Expand_N_Selected_Component --
   ---------------------------------

   procedure Expand_N_Selected_Component (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Par   : constant Node_Id    := Parent (N);
      P     : constant Node_Id    := Prefix (N);
      S     : constant Node_Id    := Selector_Name (N);
      Ptyp  : Entity_Id           := Underlying_Type (Etype (P));
      Disc  : Entity_Id;
      New_N : Node_Id;
      Dcon  : Elmt_Id;
      Dval  : Node_Id;

      function In_Left_Hand_Side (Comp : Node_Id) return Boolean;
      --  Gigi needs a temporary for prefixes that depend on a discriminant,
      --  unless the context of an assignment can provide size information.
      --  Don't we have a general routine that does this???

      function Is_Subtype_Declaration return Boolean;
      --  The replacement of a discriminant reference by its value is required
      --  if this is part of the initialization of an temporary generated by a
      --  change of representation. This shows up as the construction of a
      --  discriminant constraint for a subtype declared at the same point as
      --  the entity in the prefix of the selected component. We recognize this
      --  case when the context of the reference is:
      --    subtype ST is T(Obj.D);
      --  where the entity for Obj comes from source, and ST has the same sloc.

      -----------------------
      -- In_Left_Hand_Side --
      -----------------------

      function In_Left_Hand_Side (Comp : Node_Id) return Boolean is
      begin
         return (Nkind (Parent (Comp)) = N_Assignment_Statement
                  and then Comp = Name (Parent (Comp)))
           or else (Present (Parent (Comp))
                     and then Nkind (Parent (Comp)) in N_Subexpr
                     and then In_Left_Hand_Side (Parent (Comp)));
      end In_Left_Hand_Side;

      -----------------------------
      --  Is_Subtype_Declaration --
      -----------------------------

      function Is_Subtype_Declaration return Boolean is
         Par : constant Node_Id := Parent (N);
      begin
         return
           Nkind (Par) = N_Index_Or_Discriminant_Constraint
             and then Nkind (Parent (Parent (Par))) = N_Subtype_Declaration
             and then Comes_From_Source (Entity (Prefix (N)))
             and then Sloc (Par) = Sloc (Entity (Prefix (N)));
      end Is_Subtype_Declaration;

   --  Start of processing for Expand_N_Selected_Component

   begin
      --  Insert explicit dereference if required

      if Is_Access_Type (Ptyp) then

         --  First set prefix type to proper access type, in case it currently
         --  has a private (non-access) view of this type.

         Set_Etype (P, Ptyp);

         Insert_Explicit_Dereference (P);
         Analyze_And_Resolve (P, Designated_Type (Ptyp));

         if Ekind (Etype (P)) = E_Private_Subtype
           and then Is_For_Access_Subtype (Etype (P))
         then
            Set_Etype (P, Base_Type (Etype (P)));
         end if;

         Ptyp := Etype (P);
      end if;

      --  Deal with discriminant check required

      if Do_Discriminant_Check (N) then
         if Present (Discriminant_Checking_Func
                      (Original_Record_Component (Entity (S))))
         then
            --  Present the discriminant checking function to the backend, so
            --  that it can inline the call to the function.

            Add_Inlined_Body
              (Discriminant_Checking_Func
                (Original_Record_Component (Entity (S))));

            --  Now reset the flag and generate the call

            Set_Do_Discriminant_Check (N, False);
            Generate_Discriminant_Check (N);

         --  In the case of Unchecked_Union, no discriminant checking is
         --  actually performed.

         else
            Set_Do_Discriminant_Check (N, False);
         end if;
      end if;

      --  Ada 2005 (AI-318-02): If the prefix is a call to a build-in-place
      --  function, then additional actuals must be passed.

      if Ada_Version >= Ada_2005
        and then Is_Build_In_Place_Function_Call (P)
      then
         Make_Build_In_Place_Call_In_Anonymous_Context (P);
      end if;

      --  Gigi cannot handle unchecked conversions that are the prefix of a
      --  selected component with discriminants. This must be checked during
      --  expansion, because during analysis the type of the selector is not
      --  known at the point the prefix is analyzed. If the conversion is the
      --  target of an assignment, then we cannot force the evaluation.

      if Nkind (Prefix (N)) = N_Unchecked_Type_Conversion
        and then Has_Discriminants (Etype (N))
        and then not In_Left_Hand_Side (N)
      then
         Force_Evaluation (Prefix (N));
      end if;

      --  Remaining processing applies only if selector is a discriminant

      if Ekind (Entity (Selector_Name (N))) = E_Discriminant then

         --  If the selector is a discriminant of a constrained record type,
         --  we may be able to rewrite the expression with the actual value
         --  of the discriminant, a useful optimization in some cases.

         if Is_Record_Type (Ptyp)
           and then Has_Discriminants (Ptyp)
           and then Is_Constrained (Ptyp)
         then
            --  Do this optimization for discrete types only, and not for
            --  access types (access discriminants get us into trouble).

            if not Is_Discrete_Type (Etype (N)) then
               null;

            --  Don't do this on the left hand of an assignment statement.
            --  Normally one would think that references like this would not
            --  occur, but they do in generated code, and mean that we really
            --  do want to assign the discriminant.

            elsif Nkind (Par) = N_Assignment_Statement
              and then Name (Par) = N
            then
               null;

            --  Don't do this optimization for the prefix of an attribute or
            --  the name of an object renaming declaration since these are
            --  contexts where we do not want the value anyway.

            elsif (Nkind (Par) = N_Attribute_Reference
                    and then Prefix (Par) = N)
              or else Is_Renamed_Object (N)
            then
               null;

            --  Don't do this optimization if we are within the code for a
            --  discriminant check, since the whole point of such a check may
            --  be to verify the condition on which the code below depends.

            elsif Is_In_Discriminant_Check (N) then
               null;

            --  Green light to see if we can do the optimization. There is
            --  still one condition that inhibits the optimization below but
            --  now is the time to check the particular discriminant.

            else
               --  Loop through discriminants to find the matching discriminant
               --  constraint to see if we can copy it.

               Disc := First_Discriminant (Ptyp);
               Dcon := First_Elmt (Discriminant_Constraint (Ptyp));
               Discr_Loop : while Present (Dcon) loop
                  Dval := Node (Dcon);

                  --  Check if this is the matching discriminant and if the
                  --  discriminant value is simple enough to make sense to
                  --  copy. We don't want to copy complex expressions, and
                  --  indeed to do so can cause trouble (before we put in
                  --  this guard, a discriminant expression containing an
                  --  AND THEN was copied, causing problems for coverage
                  --  analysis tools).

                  --  However, if the reference is part of the initialization
                  --  code generated for an object declaration, we must use
                  --  the discriminant value from the subtype constraint,
                  --  because the selected component may be a reference to the
                  --  object being initialized, whose discriminant is not yet
                  --  set. This only happens in complex cases involving changes
                  --  or representation.

                  if Disc = Entity (Selector_Name (N))
                    and then (Is_Entity_Name (Dval)
                               or else Compile_Time_Known_Value (Dval)
                               or else Is_Subtype_Declaration)
                  then
                     --  Here we have the matching discriminant. Check for
                     --  the case of a discriminant of a component that is
                     --  constrained by an outer discriminant, which cannot
                     --  be optimized away.

                     if Denotes_Discriminant
                          (Dval, Check_Concurrent => True)
                     then
                        exit Discr_Loop;

                     elsif Nkind (Original_Node (Dval)) = N_Selected_Component
                       and then
                         Denotes_Discriminant
                           (Selector_Name (Original_Node (Dval)), True)
                     then
                        exit Discr_Loop;

                     --  Do not retrieve value if constraint is not static. It
                     --  is generally not useful, and the constraint may be a
                     --  rewritten outer discriminant in which case it is in
                     --  fact incorrect.

                     elsif Is_Entity_Name (Dval)
                       and then
                         Nkind (Parent (Entity (Dval))) = N_Object_Declaration
                       and then Present (Expression (Parent (Entity (Dval))))
                       and then not
                         Is_Static_Expression
                           (Expression (Parent (Entity (Dval))))
                     then
                        exit Discr_Loop;

                     --  In the context of a case statement, the expression may
                     --  have the base type of the discriminant, and we need to
                     --  preserve the constraint to avoid spurious errors on
                     --  missing cases.

                     elsif Nkind (Parent (N)) = N_Case_Statement
                       and then Etype (Dval) /= Etype (Disc)
                     then
                        Rewrite (N,
                          Make_Qualified_Expression (Loc,
                            Subtype_Mark =>
                              New_Occurrence_Of (Etype (Disc), Loc),
                            Expression   =>
                              New_Copy_Tree (Dval)));
                        Analyze_And_Resolve (N, Etype (Disc));

                        --  In case that comes out as a static expression,
                        --  reset it (a selected component is never static).

                        Set_Is_Static_Expression (N, False);
                        return;

                     --  Otherwise we can just copy the constraint, but the
                     --  result is certainly not static. In some cases the
                     --  discriminant constraint has been analyzed in the
                     --  context of the original subtype indication, but for
                     --  itypes the constraint might not have been analyzed
                     --  yet, and this must be done now.

                     else
                        Rewrite (N, New_Copy_Tree (Dval));
                        Analyze_And_Resolve (N);
                        Set_Is_Static_Expression (N, False);
                        return;
                     end if;
                  end if;

                  Next_Elmt (Dcon);
                  Next_Discriminant (Disc);
               end loop Discr_Loop;

               --  Note: the above loop should always find a matching
               --  discriminant, but if it does not, we just missed an
               --  optimization due to some glitch (perhaps a previous
               --  error), so ignore.

            end if;
         end if;

         --  The only remaining processing is in the case of a discriminant of
         --  a concurrent object, where we rewrite the prefix to denote the
         --  corresponding record type. If the type is derived and has renamed
         --  discriminants, use corresponding discriminant, which is the one
         --  that appears in the corresponding record.

         if not Is_Concurrent_Type (Ptyp) then
            return;
         end if;

         Disc := Entity (Selector_Name (N));

         if Is_Derived_Type (Ptyp)
           and then Present (Corresponding_Discriminant (Disc))
         then
            Disc := Corresponding_Discriminant (Disc);
         end if;

         New_N :=
           Make_Selected_Component (Loc,
             Prefix =>
               Unchecked_Convert_To (Corresponding_Record_Type (Ptyp),
                 New_Copy_Tree (P)),
             Selector_Name => Make_Identifier (Loc, Chars (Disc)));

         Rewrite (N, New_N);
         Analyze (N);
      end if;

      --  Set Atomic_Sync_Required if necessary for atomic component

      if Nkind (N) = N_Selected_Component then
         declare
            E   : constant Entity_Id := Entity (Selector_Name (N));
            Set : Boolean;

         begin
            --  If component is atomic, but type is not, setting depends on
            --  disable/enable state for the component.

            if Is_Atomic (E) and then not Is_Atomic (Etype (E)) then
               Set := not Atomic_Synchronization_Disabled (E);

            --  If component is not atomic, but its type is atomic, setting
            --  depends on disable/enable state for the type.

            elsif not Is_Atomic (E) and then Is_Atomic (Etype (E)) then
               Set := not Atomic_Synchronization_Disabled (Etype (E));

            --  If both component and type are atomic, we disable if either
            --  component or its type have sync disabled.

            elsif Is_Atomic (E) and then Is_Atomic (Etype (E)) then
               Set := (not Atomic_Synchronization_Disabled (E))
                        and then
                      (not Atomic_Synchronization_Disabled (Etype (E)));

            else
               Set := False;
            end if;

            --  Set flag if required

            if Set then
               Activate_Atomic_Synchronization (N);
            end if;
         end;
      end if;
   end Expand_N_Selected_Component;

   --------------------
   -- Expand_N_Slice --
   --------------------

   procedure Expand_N_Slice (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

      function Is_Procedure_Actual (N : Node_Id) return Boolean;
      --  Check whether the argument is an actual for a procedure call, in
      --  which case the expansion of a bit-packed slice is deferred until the
      --  call itself is expanded. The reason this is required is that we might
      --  have an IN OUT or OUT parameter, and the copy out is essential, and
      --  that copy out would be missed if we created a temporary here in
      --  Expand_N_Slice. Note that we don't bother to test specifically for an
      --  IN OUT or OUT mode parameter, since it is a bit tricky to do, and it
      --  is harmless to defer expansion in the IN case, since the call
      --  processing will still generate the appropriate copy in operation,
      --  which will take care of the slice.

      procedure Make_Temporary_For_Slice;
      --  Create a named variable for the value of the slice, in cases where
      --  the back-end cannot handle it properly, e.g. when packed types or
      --  unaligned slices are involved.

      -------------------------
      -- Is_Procedure_Actual --
      -------------------------

      function Is_Procedure_Actual (N : Node_Id) return Boolean is
         Par : Node_Id := Parent (N);

      begin
         loop
            --  If our parent is a procedure call we can return

            if Nkind (Par) = N_Procedure_Call_Statement then
               return True;

            --  If our parent is a type conversion, keep climbing the tree,
            --  since a type conversion can be a procedure actual. Also keep
            --  climbing if parameter association or a qualified expression,
            --  since these are additional cases that do can appear on
            --  procedure actuals.

            elsif Nkind_In (Par, N_Type_Conversion,
                                 N_Parameter_Association,
                                 N_Qualified_Expression)
            then
               Par := Parent (Par);

               --  Any other case is not what we are looking for

            else
               return False;
            end if;
         end loop;
      end Is_Procedure_Actual;

      ------------------------------
      -- Make_Temporary_For_Slice --
      ------------------------------

      procedure Make_Temporary_For_Slice is
         Ent  : constant Entity_Id := Make_Temporary (Loc, 'T', N);
         Decl : Node_Id;

      begin
         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => New_Occurrence_Of (Typ, Loc));

         Set_No_Initialization (Decl);

         Insert_Actions (N, New_List (
           Decl,
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Ent, Loc),
             Expression => Relocate_Node (N))));

         Rewrite (N, New_Occurrence_Of (Ent, Loc));
         Analyze_And_Resolve (N, Typ);
      end Make_Temporary_For_Slice;

      --  Local variables

      Pref     : constant Node_Id := Prefix (N);
      Pref_Typ : Entity_Id        := Etype (Pref);

   --  Start of processing for Expand_N_Slice

   begin
      --  Special handling for access types

      if Is_Access_Type (Pref_Typ) then
         Pref_Typ := Designated_Type (Pref_Typ);

         Rewrite (Pref,
           Make_Explicit_Dereference (Sloc (N),
            Prefix => Relocate_Node (Pref)));

         Analyze_And_Resolve (Pref, Pref_Typ);
      end if;

      --  Ada 2005 (AI-318-02): If the prefix is a call to a build-in-place
      --  function, then additional actuals must be passed.

      if Ada_Version >= Ada_2005
        and then Is_Build_In_Place_Function_Call (Pref)
      then
         Make_Build_In_Place_Call_In_Anonymous_Context (Pref);
      end if;

      --  The remaining case to be handled is packed slices. We can leave
      --  packed slices as they are in the following situations:

      --    1. Right or left side of an assignment (we can handle this
      --       situation correctly in the assignment statement expansion).

      --    2. Prefix of indexed component (the slide is optimized away in this
      --       case, see the start of Expand_N_Slice.)

      --    3. Object renaming declaration, since we want the name of the
      --       slice, not the value.

      --    4. Argument to procedure call, since copy-in/copy-out handling may
      --       be required, and this is handled in the expansion of call
      --       itself.

      --    5. Prefix of an address attribute (this is an error which is caught
      --       elsewhere, and the expansion would interfere with generating the
      --       error message).

      if not Is_Packed (Typ) then

         --  Apply transformation for actuals of a function call, where
         --  Expand_Actuals is not used.

         if Nkind (Parent (N)) = N_Function_Call
           and then Is_Possibly_Unaligned_Slice (N)
         then
            Make_Temporary_For_Slice;
         end if;

      elsif Nkind (Parent (N)) = N_Assignment_Statement
        or else (Nkind (Parent (Parent (N))) = N_Assignment_Statement
                  and then Parent (N) = Name (Parent (Parent (N))))
      then
         return;

      elsif Nkind (Parent (N)) = N_Indexed_Component
        or else Is_Renamed_Object (N)
        or else Is_Procedure_Actual (N)
      then
         return;

      elsif Nkind (Parent (N)) = N_Attribute_Reference
        and then Attribute_Name (Parent (N)) = Name_Address
      then
         return;

      else
         Make_Temporary_For_Slice;
      end if;
   end Expand_N_Slice;

   ------------------------------
   -- Expand_N_Type_Conversion --
   ------------------------------

   procedure Expand_N_Type_Conversion (N : Node_Id) is
      Loc          : constant Source_Ptr := Sloc (N);
      Operand      : constant Node_Id    := Expression (N);
      Target_Type  : constant Entity_Id  := Etype (N);
      Operand_Type : Entity_Id           := Etype (Operand);

      procedure Handle_Changed_Representation;
      --  This is called in the case of record and array type conversions to
      --  see if there is a change of representation to be handled. Change of
      --  representation is actually handled at the assignment statement level,
      --  and what this procedure does is rewrite node N conversion as an
      --  assignment to temporary. If there is no change of representation,
      --  then the conversion node is unchanged.

      procedure Raise_Accessibility_Error;
      --  Called when we know that an accessibility check will fail. Rewrites
      --  node N to an appropriate raise statement and outputs warning msgs.
      --  The Etype of the raise node is set to Target_Type.

      procedure Real_Range_Check;
      --  Handles generation of range check for real target value

      function Has_Extra_Accessibility (Id : Entity_Id) return Boolean;
      --  True iff Present (Effective_Extra_Accessibility (Id)) successfully
      --  evaluates to True.

      -----------------------------------
      -- Handle_Changed_Representation --
      -----------------------------------

      procedure Handle_Changed_Representation is
         Temp : Entity_Id;
         Decl : Node_Id;
         Odef : Node_Id;
         Disc : Node_Id;
         N_Ix : Node_Id;
         Cons : List_Id;

      begin
         --  Nothing else to do if no change of representation

         if Same_Representation (Operand_Type, Target_Type) then
            return;

         --  The real change of representation work is done by the assignment
         --  statement processing. So if this type conversion is appearing as
         --  the expression of an assignment statement, nothing needs to be
         --  done to the conversion.

         elsif Nkind (Parent (N)) = N_Assignment_Statement then
            return;

         --  Otherwise we need to generate a temporary variable, and do the
         --  change of representation assignment into that temporary variable.
         --  The conversion is then replaced by a reference to this variable.

         else
            Cons := No_List;

            --  If type is unconstrained we have to add a constraint, copied
            --  from the actual value of the left hand side.

            if not Is_Constrained (Target_Type) then
               if Has_Discriminants (Operand_Type) then
                  Disc := First_Discriminant (Operand_Type);

                  if Disc /= First_Stored_Discriminant (Operand_Type) then
                     Disc := First_Stored_Discriminant (Operand_Type);
                  end if;

                  Cons := New_List;
                  while Present (Disc) loop
                     Append_To (Cons,
                       Make_Selected_Component (Loc,
                         Prefix        =>
                           Duplicate_Subexpr_Move_Checks (Operand),
                         Selector_Name =>
                           Make_Identifier (Loc, Chars (Disc))));
                     Next_Discriminant (Disc);
                  end loop;

               elsif Is_Array_Type (Operand_Type) then
                  N_Ix := First_Index (Target_Type);
                  Cons := New_List;

                  for J in 1 .. Number_Dimensions (Operand_Type) loop

                     --  We convert the bounds explicitly. We use an unchecked
                     --  conversion because bounds checks are done elsewhere.

                     Append_To (Cons,
                       Make_Range (Loc,
                         Low_Bound =>
                           Unchecked_Convert_To (Etype (N_Ix),
                             Make_Attribute_Reference (Loc,
                               Prefix =>
                                 Duplicate_Subexpr_No_Checks
                                   (Operand, Name_Req => True),
                               Attribute_Name => Name_First,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, J)))),

                         High_Bound =>
                           Unchecked_Convert_To (Etype (N_Ix),
                             Make_Attribute_Reference (Loc,
                               Prefix =>
                                 Duplicate_Subexpr_No_Checks
                                   (Operand, Name_Req => True),
                               Attribute_Name => Name_Last,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, J))))));

                     Next_Index (N_Ix);
                  end loop;
               end if;
            end if;

            Odef := New_Occurrence_Of (Target_Type, Loc);

            if Present (Cons) then
               Odef :=
                 Make_Subtype_Indication (Loc,
                   Subtype_Mark => Odef,
                   Constraint =>
                     Make_Index_Or_Discriminant_Constraint (Loc,
                       Constraints => Cons));
            end if;

            Temp := Make_Temporary (Loc, 'C');
            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp,
                Object_Definition   => Odef);

            Set_No_Initialization (Decl, True);

            --  Insert required actions. It is essential to suppress checks
            --  since we have suppressed default initialization, which means
            --  that the variable we create may have no discriminants.

            Insert_Actions (N,
              New_List (
                Decl,
                Make_Assignment_Statement (Loc,
                  Name => New_Occurrence_Of (Temp, Loc),
                  Expression => Relocate_Node (N))),
                Suppress => All_Checks);

            Rewrite (N, New_Occurrence_Of (Temp, Loc));
            return;
         end if;
      end Handle_Changed_Representation;

      -------------------------------
      -- Raise_Accessibility_Error --
      -------------------------------

      procedure Raise_Accessibility_Error is
      begin
         Error_Msg_Warn := SPARK_Mode /= On;
         Rewrite (N,
           Make_Raise_Program_Error (Sloc (N),
             Reason => PE_Accessibility_Check_Failed));
         Set_Etype (N, Target_Type);

         Error_Msg_N ("<<accessibility check failure", N);
         Error_Msg_NE ("\<<& [", N, Standard_Program_Error);
      end Raise_Accessibility_Error;

      ----------------------
      -- Real_Range_Check --
      ----------------------

      --  Case of conversions to floating-point or fixed-point. If range checks
      --  are enabled and the target type has a range constraint, we convert:

      --     typ (x)

      --       to

      --     Tnn : typ'Base := typ'Base (x);
      --     [constraint_error when Tnn < typ'First or else Tnn > typ'Last]
      --     Tnn

      --  This is necessary when there is a conversion of integer to float or
      --  to fixed-point to ensure that the correct checks are made. It is not
      --  necessary for float to float where it is enough to simply set the
      --  Do_Range_Check flag.

      procedure Real_Range_Check is
         Btyp : constant Entity_Id := Base_Type (Target_Type);
         Lo   : constant Node_Id   := Type_Low_Bound  (Target_Type);
         Hi   : constant Node_Id   := Type_High_Bound (Target_Type);
         Xtyp : constant Entity_Id := Etype (Operand);
         Conv : Node_Id;
         Tnn  : Entity_Id;

      begin
         --  Nothing to do if conversion was rewritten

         if Nkind (N) /= N_Type_Conversion then
            return;
         end if;

         --  Nothing to do if range checks suppressed, or target has the same
         --  range as the base type (or is the base type).

         if Range_Checks_Suppressed (Target_Type)
           or else (Lo = Type_Low_Bound  (Btyp)
                      and then
                    Hi = Type_High_Bound (Btyp))
         then
            return;
         end if;

         --  Nothing to do if expression is an entity on which checks have been
         --  suppressed.

         if Is_Entity_Name (Operand)
           and then Range_Checks_Suppressed (Entity (Operand))
         then
            return;
         end if;

         --  Nothing to do if bounds are all static and we can tell that the
         --  expression is within the bounds of the target. Note that if the
         --  operand is of an unconstrained floating-point type, then we do
         --  not trust it to be in range (might be infinite)

         declare
            S_Lo : constant Node_Id := Type_Low_Bound (Xtyp);
            S_Hi : constant Node_Id := Type_High_Bound (Xtyp);

         begin
            if (not Is_Floating_Point_Type (Xtyp)
                 or else Is_Constrained (Xtyp))
              and then Compile_Time_Known_Value (S_Lo)
              and then Compile_Time_Known_Value (S_Hi)
              and then Compile_Time_Known_Value (Hi)
              and then Compile_Time_Known_Value (Lo)
            then
               declare
                  D_Lov : constant Ureal := Expr_Value_R (Lo);
                  D_Hiv : constant Ureal := Expr_Value_R (Hi);
                  S_Lov : Ureal;
                  S_Hiv : Ureal;

               begin
                  if Is_Real_Type (Xtyp) then
                     S_Lov := Expr_Value_R (S_Lo);
                     S_Hiv := Expr_Value_R (S_Hi);
                  else
                     S_Lov := UR_From_Uint (Expr_Value (S_Lo));
                     S_Hiv := UR_From_Uint (Expr_Value (S_Hi));
                  end if;

                  if D_Hiv > D_Lov
                    and then S_Lov >= D_Lov
                    and then S_Hiv <= D_Hiv
                  then
                     Set_Do_Range_Check (Operand, False);
                     return;
                  end if;
               end;
            end if;
         end;

         --  For float to float conversions, we are done

         if Is_Floating_Point_Type (Xtyp)
              and then
            Is_Floating_Point_Type (Btyp)
         then
            return;
         end if;

         --  Otherwise rewrite the conversion as described above

         Conv := Relocate_Node (N);
         Rewrite (Subtype_Mark (Conv), New_Occurrence_Of (Btyp, Loc));
         Set_Etype (Conv, Btyp);

         --  Enable overflow except for case of integer to float conversions,
         --  where it is never required, since we can never have overflow in
         --  this case.

         if not Is_Integer_Type (Etype (Operand)) then
            Enable_Overflow_Check (Conv);
         end if;

         Tnn := Make_Temporary (Loc, 'T', Conv);

         Insert_Actions (N, New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tnn,
             Object_Definition   => New_Occurrence_Of (Btyp, Loc),
             Constant_Present    => True,
             Expression          => Conv),

           Make_Raise_Constraint_Error (Loc,
             Condition =>
              Make_Or_Else (Loc,
                Left_Opnd =>
                  Make_Op_Lt (Loc,
                    Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_First,
                        Prefix =>
                          New_Occurrence_Of (Target_Type, Loc))),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Last,
                        Prefix =>
                          New_Occurrence_Of (Target_Type, Loc)))),
             Reason => CE_Range_Check_Failed)));

         Rewrite (N, New_Occurrence_Of (Tnn, Loc));
         Analyze_And_Resolve (N, Btyp);
      end Real_Range_Check;

      -----------------------------
      -- Has_Extra_Accessibility --
      -----------------------------

      --  Returns true for a formal of an anonymous access type or for
      --  an Ada 2012-style stand-alone object of an anonymous access type.

      function Has_Extra_Accessibility (Id : Entity_Id) return Boolean is
      begin
         if Is_Formal (Id) or else Ekind_In (Id, E_Constant, E_Variable) then
            return Present (Effective_Extra_Accessibility (Id));
         else
            return False;
         end if;
      end Has_Extra_Accessibility;

   --  Start of processing for Expand_N_Type_Conversion

   begin
      --  First remove check marks put by the semantic analysis on the type
      --  conversion between array types. We need these checks, and they will
      --  be generated by this expansion routine, but we do not depend on these
      --  flags being set, and since we do intend to expand the checks in the
      --  front end, we don't want them on the tree passed to the back end.

      if Is_Array_Type (Target_Type) then
         if Is_Constrained (Target_Type) then
            Set_Do_Length_Check (N, False);
         else
            Set_Do_Range_Check (Operand, False);
         end if;
      end if;

      --  Nothing at all to do if conversion is to the identical type so remove
      --  the conversion completely, it is useless, except that it may carry
      --  an Assignment_OK attribute, which must be propagated to the operand.

      if Operand_Type = Target_Type then
         if Assignment_OK (N) then
            Set_Assignment_OK (Operand);
         end if;

         Rewrite (N, Relocate_Node (Operand));
         goto Done;
      end if;

      --  Nothing to do if this is the second argument of read. This is a
      --  "backwards" conversion that will be handled by the specialized code
      --  in attribute processing.

      if Nkind (Parent (N)) = N_Attribute_Reference
        and then Attribute_Name (Parent (N)) = Name_Read
        and then Next (First (Expressions (Parent (N)))) = N
      then
         goto Done;
      end if;

      --  Check for case of converting to a type that has an invariant
      --  associated with it. This required an invariant check. We convert

      --    typ (expr)

      --  into

      --    do invariant_check (typ (expr)) in typ (expr);

      --  using Duplicate_Subexpr to avoid multiple side effects

      --  Note: the Comes_From_Source check, and then the resetting of this
      --  flag prevents what would otherwise be an infinite recursion.

      if Has_Invariants (Target_Type)
        and then Present (Invariant_Procedure (Target_Type))
        and then Comes_From_Source (N)
      then
         Set_Comes_From_Source (N, False);
         Rewrite (N,
           Make_Expression_With_Actions (Loc,
             Actions    => New_List (
               Make_Invariant_Call (Duplicate_Subexpr (N))),
             Expression => Duplicate_Subexpr_No_Checks (N)));
         Analyze_And_Resolve (N, Target_Type);
         goto Done;
      end if;

      --  Here if we may need to expand conversion

      --  If the operand of the type conversion is an arithmetic operation on
      --  signed integers, and the based type of the signed integer type in
      --  question is smaller than Standard.Integer, we promote both of the
      --  operands to type Integer.

      --  For example, if we have

      --     target-type (opnd1 + opnd2)

      --  and opnd1 and opnd2 are of type short integer, then we rewrite
      --  this as:

      --     target-type (integer(opnd1) + integer(opnd2))

      --  We do this because we are always allowed to compute in a larger type
      --  if we do the right thing with the result, and in this case we are
      --  going to do a conversion which will do an appropriate check to make
      --  sure that things are in range of the target type in any case. This
      --  avoids some unnecessary intermediate overflows.

      --  We might consider a similar transformation in the case where the
      --  target is a real type or a 64-bit integer type, and the operand
      --  is an arithmetic operation using a 32-bit integer type. However,
      --  we do not bother with this case, because it could cause significant
      --  inefficiencies on 32-bit machines. On a 64-bit machine it would be
      --  much cheaper, but we don't want different behavior on 32-bit and
      --  64-bit machines. Note that the exclusion of the 64-bit case also
      --  handles the configurable run-time cases where 64-bit arithmetic
      --  may simply be unavailable.

      --  Note: this circuit is partially redundant with respect to the circuit
      --  in Checks.Apply_Arithmetic_Overflow_Check, but we catch more cases in
      --  the processing here. Also we still need the Checks circuit, since we
      --  have to be sure not to generate junk overflow checks in the first
      --  place, since it would be trick to remove them here.

      if Integer_Promotion_Possible (N) then

         --  All conditions met, go ahead with transformation

         declare
            Opnd : Node_Id;
            L, R : Node_Id;

         begin
            R :=
              Make_Type_Conversion (Loc,
                Subtype_Mark => New_Occurrence_Of (Standard_Integer, Loc),
                Expression   => Relocate_Node (Right_Opnd (Operand)));

            Opnd := New_Op_Node (Nkind (Operand), Loc);
            Set_Right_Opnd (Opnd, R);

            if Nkind (Operand) in N_Binary_Op then
               L :=
                 Make_Type_Conversion (Loc,
                   Subtype_Mark => New_Occurrence_Of (Standard_Integer, Loc),
                   Expression   => Relocate_Node (Left_Opnd (Operand)));

               Set_Left_Opnd  (Opnd, L);
            end if;

            Rewrite (N,
              Make_Type_Conversion (Loc,
                Subtype_Mark => Relocate_Node (Subtype_Mark (N)),
                Expression   => Opnd));

            Analyze_And_Resolve (N, Target_Type);
            goto Done;
         end;
      end if;

      --  Do validity check if validity checking operands

      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Operand);
      end if;

      --  Special case of converting from non-standard boolean type

      if Is_Boolean_Type (Operand_Type)
        and then (Nonzero_Is_True (Operand_Type))
      then
         Adjust_Condition (Operand);
         Set_Etype (Operand, Standard_Boolean);
         Operand_Type := Standard_Boolean;
      end if;

      --  Case of converting to an access type

      if Is_Access_Type (Target_Type) then

         --  Apply an accessibility check when the conversion operand is an
         --  access parameter (or a renaming thereof), unless conversion was
         --  expanded from an Unchecked_ or Unrestricted_Access attribute.
         --  Note that other checks may still need to be applied below (such
         --  as tagged type checks).

         if Is_Entity_Name (Operand)
           and then Has_Extra_Accessibility (Entity (Operand))
           and then Ekind (Etype (Operand)) = E_Anonymous_Access_Type
           and then (Nkind (Original_Node (N)) /= N_Attribute_Reference
                      or else Attribute_Name (Original_Node (N)) = Name_Access)
         then
            Apply_Accessibility_Check
              (Operand, Target_Type, Insert_Node => Operand);

         --  If the level of the operand type is statically deeper than the
         --  level of the target type, then force Program_Error. Note that this
         --  can only occur for cases where the attribute is within the body of
         --  an instantiation (otherwise the conversion will already have been
         --  rejected as illegal). Note: warnings are issued by the analyzer
         --  for the instance cases.

         elsif In_Instance_Body
           and then Type_Access_Level (Operand_Type) >
                    Type_Access_Level (Target_Type)
         then
            Raise_Accessibility_Error;

         --  When the operand is a selected access discriminant the check needs
         --  to be made against the level of the object denoted by the prefix
         --  of the selected name. Force Program_Error for this case as well
         --  (this accessibility violation can only happen if within the body
         --  of an instantiation).

         elsif In_Instance_Body
           and then Ekind (Operand_Type) = E_Anonymous_Access_Type
           and then Nkind (Operand) = N_Selected_Component
           and then Object_Access_Level (Operand) >
                      Type_Access_Level (Target_Type)
         then
            Raise_Accessibility_Error;
            goto Done;
         end if;
      end if;

      --  Case of conversions of tagged types and access to tagged types

      --  When needed, that is to say when the expression is class-wide, Add
      --  runtime a tag check for (strict) downward conversion by using the
      --  membership test, generating:

      --      [constraint_error when Operand not in Target_Type'Class]

      --  or in the access type case

      --      [constraint_error
      --        when Operand /= null
      --          and then Operand.all not in
      --            Designated_Type (Target_Type)'Class]

      if (Is_Access_Type (Target_Type)
           and then Is_Tagged_Type (Designated_Type (Target_Type)))
        or else Is_Tagged_Type (Target_Type)
      then
         --  Do not do any expansion in the access type case if the parent is a
         --  renaming, since this is an error situation which will be caught by
         --  Sem_Ch8, and the expansion can interfere with this error check.

         if Is_Access_Type (Target_Type) and then Is_Renamed_Object (N) then
            goto Done;
         end if;

         --  Otherwise, proceed with processing tagged conversion

         Tagged_Conversion : declare
            Actual_Op_Typ   : Entity_Id;
            Actual_Targ_Typ : Entity_Id;
            Make_Conversion : Boolean := False;
            Root_Op_Typ     : Entity_Id;

            procedure Make_Tag_Check (Targ_Typ : Entity_Id);
            --  Create a membership check to test whether Operand is a member
            --  of Targ_Typ. If the original Target_Type is an access, include
            --  a test for null value. The check is inserted at N.

            --------------------
            -- Make_Tag_Check --
            --------------------

            procedure Make_Tag_Check (Targ_Typ : Entity_Id) is
               Cond : Node_Id;

            begin
               --  Generate:
               --    [Constraint_Error
               --       when Operand /= null
               --         and then Operand.all not in Targ_Typ]

               if Is_Access_Type (Target_Type) then
                  Cond :=
                    Make_And_Then (Loc,
                      Left_Opnd =>
                        Make_Op_Ne (Loc,
                          Left_Opnd  => Duplicate_Subexpr_No_Checks (Operand),
                          Right_Opnd => Make_Null (Loc)),

                      Right_Opnd =>
                        Make_Not_In (Loc,
                          Left_Opnd  =>
                            Make_Explicit_Dereference (Loc,
                              Prefix => Duplicate_Subexpr_No_Checks (Operand)),
                          Right_Opnd => New_Occurrence_Of (Targ_Typ, Loc)));

               --  Generate:
               --    [Constraint_Error when Operand not in Targ_Typ]

               else
                  Cond :=
                    Make_Not_In (Loc,
                      Left_Opnd  => Duplicate_Subexpr_No_Checks (Operand),
                      Right_Opnd => New_Occurrence_Of (Targ_Typ, Loc));
               end if;

               Insert_Action (N,
                 Make_Raise_Constraint_Error (Loc,
                   Condition => Cond,
                   Reason    => CE_Tag_Check_Failed));
            end Make_Tag_Check;

         --  Start of processing for Tagged_Conversion

         begin
            --  Handle entities from the limited view

            if Is_Access_Type (Operand_Type) then
               Actual_Op_Typ :=
                 Available_View (Designated_Type (Operand_Type));
            else
               Actual_Op_Typ := Operand_Type;
            end if;

            if Is_Access_Type (Target_Type) then
               Actual_Targ_Typ :=
                 Available_View (Designated_Type (Target_Type));
            else
               Actual_Targ_Typ := Target_Type;
            end if;

            Root_Op_Typ := Root_Type (Actual_Op_Typ);

            --  Ada 2005 (AI-251): Handle interface type conversion

            if Is_Interface (Actual_Op_Typ) then
               Expand_Interface_Conversion (N);
               goto Done;
            end if;

            if not Tag_Checks_Suppressed (Actual_Targ_Typ) then

               --  Create a runtime tag check for a downward class-wide type
               --  conversion.

               if Is_Class_Wide_Type (Actual_Op_Typ)
                 and then Actual_Op_Typ /= Actual_Targ_Typ
                 and then Root_Op_Typ /= Actual_Targ_Typ
                 and then Is_Ancestor (Root_Op_Typ, Actual_Targ_Typ,
                                       Use_Full_View => True)
               then
                  Make_Tag_Check (Class_Wide_Type (Actual_Targ_Typ));
                  Make_Conversion := True;
               end if;

               --  AI05-0073: If the result subtype of the function is defined
               --  by an access_definition designating a specific tagged type
               --  T, a check is made that the result value is null or the tag
               --  of the object designated by the result value identifies T.
               --  Constraint_Error is raised if this check fails.

               if Nkind (Parent (N)) = N_Simple_Return_Statement then
                  declare
                     Func     : Entity_Id;
                     Func_Typ : Entity_Id;

                  begin
                     --  Climb scope stack looking for the enclosing function

                     Func := Current_Scope;
                     while Present (Func)
                       and then Ekind (Func) /= E_Function
                     loop
                        Func := Scope (Func);
                     end loop;

                     --  The function's return subtype must be defined using
                     --  an access definition.

                     if Nkind (Result_Definition (Parent (Func))) =
                          N_Access_Definition
                     then
                        Func_Typ := Directly_Designated_Type (Etype (Func));

                        --  The return subtype denotes a specific tagged type,
                        --  in other words, a non class-wide type.

                        if Is_Tagged_Type (Func_Typ)
                          and then not Is_Class_Wide_Type (Func_Typ)
                        then
                           Make_Tag_Check (Actual_Targ_Typ);
                           Make_Conversion := True;
                        end if;
                     end if;
                  end;
               end if;

               --  We have generated a tag check for either a class-wide type
               --  conversion or for AI05-0073.

               if Make_Conversion then
                  declare
                     Conv : Node_Id;
                  begin
                     Conv :=
                       Make_Unchecked_Type_Conversion (Loc,
                         Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                         Expression   => Relocate_Node (Expression (N)));
                     Rewrite (N, Conv);
                     Analyze_And_Resolve (N, Target_Type);
                  end;
               end if;
            end if;
         end Tagged_Conversion;

      --  Case of other access type conversions

      elsif Is_Access_Type (Target_Type) then
         Apply_Constraint_Check (Operand, Target_Type);

      --  Case of conversions from a fixed-point type

      --  These conversions require special expansion and processing, found in
      --  the Exp_Fixd package. We ignore cases where Conversion_OK is set,
      --  since from a semantic point of view, these are simple integer
      --  conversions, which do not need further processing.

      elsif Is_Fixed_Point_Type (Operand_Type)
        and then not Conversion_OK (N)
      then
         --  We should never see universal fixed at this case, since the
         --  expansion of the constituent divide or multiply should have
         --  eliminated the explicit mention of universal fixed.

         pragma Assert (Operand_Type /= Universal_Fixed);

         --  Check for special case of the conversion to universal real that
         --  occurs as a result of the use of a round attribute. In this case,
         --  the real type for the conversion is taken from the target type of
         --  the Round attribute and the result must be marked as rounded.

         if Target_Type = Universal_Real
           and then Nkind (Parent (N)) = N_Attribute_Reference
           and then Attribute_Name (Parent (N)) = Name_Round
         then
            Set_Rounded_Result (N);
            Set_Etype (N, Etype (Parent (N)));
         end if;

         --  Otherwise do correct fixed-conversion, but skip these if the
         --  Conversion_OK flag is set, because from a semantic point of view
         --  these are simple integer conversions needing no further processing
         --  (the backend will simply treat them as integers).

         if not Conversion_OK (N) then
            if Is_Fixed_Point_Type (Etype (N)) then
               Expand_Convert_Fixed_To_Fixed (N);
               Real_Range_Check;

            elsif Is_Integer_Type (Etype (N)) then
               Expand_Convert_Fixed_To_Integer (N);

            else
               pragma Assert (Is_Floating_Point_Type (Etype (N)));
               Expand_Convert_Fixed_To_Float (N);
               Real_Range_Check;
            end if;
         end if;

      --  Case of conversions to a fixed-point type

      --  These conversions require special expansion and processing, found in
      --  the Exp_Fixd package. Again, ignore cases where Conversion_OK is set,
      --  since from a semantic point of view, these are simple integer
      --  conversions, which do not need further processing.

      elsif Is_Fixed_Point_Type (Target_Type)
        and then not Conversion_OK (N)
      then
         if Is_Integer_Type (Operand_Type) then
            Expand_Convert_Integer_To_Fixed (N);
            Real_Range_Check;
         else
            pragma Assert (Is_Floating_Point_Type (Operand_Type));
            Expand_Convert_Float_To_Fixed (N);
            Real_Range_Check;
         end if;

      --  Case of float-to-integer conversions

      --  We also handle float-to-fixed conversions with Conversion_OK set
      --  since semantically the fixed-point target is treated as though it
      --  were an integer in such cases.

      elsif Is_Floating_Point_Type (Operand_Type)
        and then
          (Is_Integer_Type (Target_Type)
            or else
          (Is_Fixed_Point_Type (Target_Type) and then Conversion_OK (N)))
      then
         --  One more check here, gcc is still not able to do conversions of
         --  this type with proper overflow checking, and so gigi is doing an
         --  approximation of what is required by doing floating-point compares
         --  with the end-point. But that can lose precision in some cases, and
         --  give a wrong result. Converting the operand to Universal_Real is
         --  helpful, but still does not catch all cases with 64-bit integers
         --  on targets with only 64-bit floats.

         --  The above comment seems obsoleted by Apply_Float_Conversion_Check
         --  Can this code be removed ???

         if Do_Range_Check (Operand) then
            Rewrite (Operand,
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Universal_Real, Loc),
                Expression =>
                  Relocate_Node (Operand)));

            Set_Etype (Operand, Universal_Real);
            Enable_Range_Check (Operand);
            Set_Do_Range_Check (Expression (Operand), False);
         end if;

      --  Case of array conversions

      --  Expansion of array conversions, add required length/range checks but
      --  only do this if there is no change of representation. For handling of
      --  this case, see Handle_Changed_Representation.

      elsif Is_Array_Type (Target_Type) then
         if Is_Constrained (Target_Type) then
            Apply_Length_Check (Operand, Target_Type);
         else
            Apply_Range_Check (Operand, Target_Type);
         end if;

         Handle_Changed_Representation;

      --  Case of conversions of discriminated types

      --  Add required discriminant checks if target is constrained. Again this
      --  change is skipped if we have a change of representation.

      elsif Has_Discriminants (Target_Type)
        and then Is_Constrained (Target_Type)
      then
         Apply_Discriminant_Check (Operand, Target_Type);
         Handle_Changed_Representation;

      --  Case of all other record conversions. The only processing required
      --  is to check for a change of representation requiring the special
      --  assignment processing.

      elsif Is_Record_Type (Target_Type) then

         --  Ada 2005 (AI-216): Program_Error is raised when converting from
         --  a derived Unchecked_Union type to an unconstrained type that is
         --  not Unchecked_Union if the operand lacks inferable discriminants.

         if Is_Derived_Type (Operand_Type)
           and then Is_Unchecked_Union (Base_Type (Operand_Type))
           and then not Is_Constrained (Target_Type)
           and then not Is_Unchecked_Union (Base_Type (Target_Type))
           and then not Has_Inferable_Discriminants (Operand)
         then
            --  To prevent Gigi from generating illegal code, we generate a
            --  Program_Error node, but we give it the target type of the
            --  conversion (is this requirement documented somewhere ???)

            declare
               PE : constant Node_Id := Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction);

            begin
               Set_Etype (PE, Target_Type);
               Rewrite (N, PE);

            end;
         else
            Handle_Changed_Representation;
         end if;

      --  Case of conversions of enumeration types

      elsif Is_Enumeration_Type (Target_Type) then

         --  Special processing is required if there is a change of
         --  representation (from enumeration representation clauses).

         if not Same_Representation (Target_Type, Operand_Type) then

            --  Convert: x(y) to x'val (ytyp'val (y))

            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Target_Type, Loc),
                Attribute_Name => Name_Val,
                Expressions    => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Operand_Type, Loc),
                    Attribute_Name => Name_Pos,
                    Expressions    => New_List (Operand)))));

            Analyze_And_Resolve (N, Target_Type);
         end if;

      --  Case of conversions to floating-point

      elsif Is_Floating_Point_Type (Target_Type) then
         Real_Range_Check;
      end if;

      --  At this stage, either the conversion node has been transformed into
      --  some other equivalent expression, or left as a conversion that can be
      --  handled by Gigi, in the following cases:

      --    Conversions with no change of representation or type

      --    Numeric conversions involving integer, floating- and fixed-point
      --    values. Fixed-point values are allowed only if Conversion_OK is
      --    set, i.e. if the fixed-point values are to be treated as integers.

      --  No other conversions should be passed to Gigi

      --  Check: are these rules stated in sinfo??? if so, why restate here???

      --  The only remaining step is to generate a range check if we still have
      --  a type conversion at this stage and Do_Range_Check is set. For now we
      --  do this only for conversions of discrete types.

      if Nkind (N) = N_Type_Conversion
        and then Is_Discrete_Type (Etype (N))
      then
         declare
            Expr : constant Node_Id := Expression (N);
            Ftyp : Entity_Id;
            Ityp : Entity_Id;

         begin
            if Do_Range_Check (Expr)
              and then Is_Discrete_Type (Etype (Expr))
            then
               Set_Do_Range_Check (Expr, False);

               --  Before we do a range check, we have to deal with treating a
               --  fixed-point operand as an integer. The way we do this is
               --  simply to do an unchecked conversion to an appropriate
               --  integer type large enough to hold the result.

               --  This code is not active yet, because we are only dealing
               --  with discrete types so far ???

               if Nkind (Expr) in N_Has_Treat_Fixed_As_Integer
                 and then Treat_Fixed_As_Integer (Expr)
               then
                  Ftyp := Base_Type (Etype (Expr));

                  if Esize (Ftyp) >= Esize (Standard_Integer) then
                     Ityp := Standard_Long_Long_Integer;
                  else
                     Ityp := Standard_Integer;
                  end if;

                  Rewrite (Expr, Unchecked_Convert_To (Ityp, Expr));
               end if;

               --  Reset overflow flag, since the range check will include
               --  dealing with possible overflow, and generate the check. If
               --  Address is either a source type or target type, suppress
               --  range check to avoid typing anomalies when it is a visible
               --  integer type.

               Set_Do_Overflow_Check (N, False);
               if not Is_Descendent_Of_Address (Etype (Expr))
                 and then not Is_Descendent_Of_Address (Target_Type)
               then
                  Generate_Range_Check
                    (Expr, Target_Type, CE_Range_Check_Failed);
               end if;
            end if;
         end;
      end if;

      --  Final step, if the result is a type conversion involving Vax_Float
      --  types, then it is subject for further special processing.

      if Nkind (N) = N_Type_Conversion
        and then (Vax_Float (Operand_Type) or else Vax_Float (Target_Type))
      then
         Expand_Vax_Conversion (N);
         goto Done;
      end if;

      --  Here at end of processing

   <<Done>>
      --  Apply predicate check if required. Note that we can't just call
      --  Apply_Predicate_Check here, because the type looks right after
      --  the conversion and it would omit the check. The Comes_From_Source
      --  guard is necessary to prevent infinite recursions when we generate
      --  internal conversions for the purpose of checking predicates.

      if Present (Predicate_Function (Target_Type))
        and then Target_Type /= Operand_Type
        and then Comes_From_Source (N)
      then
         declare
            New_Expr : constant Node_Id := Duplicate_Subexpr (N);

         begin
            --  Avoid infinite recursion on the subsequent expansion of
            --  of the copy of the original type conversion.

            Set_Comes_From_Source (New_Expr, False);
            Insert_Action (N, Make_Predicate_Check (Target_Type, New_Expr));
         end;
      end if;
   end Expand_N_Type_Conversion;

   -----------------------------------
   -- Expand_N_Unchecked_Expression --
   -----------------------------------

   --  Remove the unchecked expression node from the tree. Its job was simply
   --  to make sure that its constituent expression was handled with checks
   --  off, and now that that is done, we can remove it from the tree, and
   --  indeed must, since Gigi does not expect to see these nodes.

   procedure Expand_N_Unchecked_Expression (N : Node_Id) is
      Exp : constant Node_Id := Expression (N);
   begin
      Set_Assignment_OK (Exp, Assignment_OK (N) or else Assignment_OK (Exp));
      Rewrite (N, Exp);
   end Expand_N_Unchecked_Expression;

   ----------------------------------------
   -- Expand_N_Unchecked_Type_Conversion --
   ----------------------------------------

   --  If this cannot be handled by Gigi and we haven't already made a
   --  temporary for it, do it now.

   procedure Expand_N_Unchecked_Type_Conversion (N : Node_Id) is
      Target_Type  : constant Entity_Id := Etype (N);
      Operand      : constant Node_Id   := Expression (N);
      Operand_Type : constant Entity_Id := Etype (Operand);

   begin
      --  Nothing at all to do if conversion is to the identical type so remove
      --  the conversion completely, it is useless, except that it may carry
      --  an Assignment_OK indication which must be propagated to the operand.

      if Operand_Type = Target_Type then

         --  Code duplicates Expand_N_Unchecked_Expression above, factor???

         if Assignment_OK (N) then
            Set_Assignment_OK (Operand);
         end if;

         Rewrite (N, Relocate_Node (Operand));
         return;
      end if;

      --  If we have a conversion of a compile time known value to a target
      --  type and the value is in range of the target type, then we can simply
      --  replace the construct by an integer literal of the correct type. We
      --  only apply this to integer types being converted. Possibly it may
      --  apply in other cases, but it is too much trouble to worry about.

      --  Note that we do not do this transformation if the Kill_Range_Check
      --  flag is set, since then the value may be outside the expected range.
      --  This happens in the Normalize_Scalars case.

      --  We also skip this if either the target or operand type is biased
      --  because in this case, the unchecked conversion is supposed to
      --  preserve the bit pattern, not the integer value.

      if Is_Integer_Type (Target_Type)
        and then not Has_Biased_Representation (Target_Type)
        and then Is_Integer_Type (Operand_Type)
        and then not Has_Biased_Representation (Operand_Type)
        and then Compile_Time_Known_Value (Operand)
        and then not Kill_Range_Check (N)
      then
         declare
            Val : constant Uint := Expr_Value (Operand);

         begin
            if Compile_Time_Known_Value (Type_Low_Bound (Target_Type))
                 and then
               Compile_Time_Known_Value (Type_High_Bound (Target_Type))
                 and then
               Val >= Expr_Value (Type_Low_Bound (Target_Type))
                 and then
               Val <= Expr_Value (Type_High_Bound (Target_Type))
            then
               Rewrite (N, Make_Integer_Literal (Sloc (N), Val));

               --  If Address is the target type, just set the type to avoid a
               --  spurious type error on the literal when Address is a visible
               --  integer type.

               if Is_Descendent_Of_Address (Target_Type) then
                  Set_Etype (N, Target_Type);
               else
                  Analyze_And_Resolve (N, Target_Type);
               end if;

               return;
            end if;
         end;
      end if;

      --  Nothing to do if conversion is safe

      if Safe_Unchecked_Type_Conversion (N) then
         return;
      end if;

      --  Otherwise force evaluation unless Assignment_OK flag is set (this
      --  flag indicates ??? More comments needed here)

      if Assignment_OK (N) then
         null;
      else
         Force_Evaluation (N);
      end if;
   end Expand_N_Unchecked_Type_Conversion;

   ----------------------------
   -- Expand_Record_Equality --
   ----------------------------

   --  For non-variant records, Equality is expanded when needed into:

   --      and then Lhs.Discr1 = Rhs.Discr1
   --      and then ...
   --      and then Lhs.Discrn = Rhs.Discrn
   --      and then Lhs.Cmp1 = Rhs.Cmp1
   --      and then ...
   --      and then Lhs.Cmpn = Rhs.Cmpn

   --  The expression is folded by the back-end for adjacent fields. This
   --  function is called for tagged record in only one occasion: for imple-
   --  menting predefined primitive equality (see Predefined_Primitives_Bodies)
   --  otherwise the primitive "=" is used directly.

   function Expand_Record_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Result : Node_Id;
      C      : Entity_Id;

      First_Time : Boolean := True;

      function Element_To_Compare (C : Entity_Id) return Entity_Id;
      --  Return the next discriminant or component to compare, starting with
      --  C, skipping inherited components.

      ------------------------
      -- Element_To_Compare --
      ------------------------

      function Element_To_Compare (C : Entity_Id) return Entity_Id is
         Comp : Entity_Id;

      begin
         Comp := C;
         loop
            --  Exit loop when the next element to be compared is found, or
            --  there is no more such element.

            exit when No (Comp);

            exit when Ekind_In (Comp, E_Discriminant, E_Component)
              and then not (

              --  Skip inherited components

              --  Note: for a tagged type, we always generate the "=" primitive
              --  for the base type (not on the first subtype), so the test for
              --  Comp /= Original_Record_Component (Comp) is True for
              --  inherited components only.

              (Is_Tagged_Type (Typ)
                and then Comp /= Original_Record_Component (Comp))

              --  Skip _Tag

              or else Chars (Comp) = Name_uTag

              --  The .NET/JVM version of type Root_Controlled contains two
              --  fields which should not be considered part of the object. To
              --  achieve proper equiality between two controlled objects on
              --  .NET/JVM, skip _Parent whenever it has type Root_Controlled.

              or else (Chars (Comp) = Name_uParent
                        and then VM_Target /= No_VM
                        and then Etype (Comp) = RTE (RE_Root_Controlled))

              --  Skip interface elements (secondary tags???)

              or else Is_Interface (Etype (Comp)));

            Next_Entity (Comp);
         end loop;

         return Comp;
      end Element_To_Compare;

   --  Start of processing for Expand_Record_Equality

   begin
      --  Generates the following code: (assuming that Typ has one Discr and
      --  component C2 is also a record)

      --   True
      --     and then Lhs.Discr1 = Rhs.Discr1
      --     and then Lhs.C1 = Rhs.C1
      --     and then Lhs.C2.C1=Rhs.C2.C1 and then ... Lhs.C2.Cn=Rhs.C2.Cn
      --     and then ...
      --     and then Lhs.Cmpn = Rhs.Cmpn

      Result := New_Occurrence_Of (Standard_True, Loc);
      C := Element_To_Compare (First_Entity (Typ));
      while Present (C) loop
         declare
            New_Lhs : Node_Id;
            New_Rhs : Node_Id;
            Check   : Node_Id;

         begin
            if First_Time then
               First_Time := False;
               New_Lhs := Lhs;
               New_Rhs := Rhs;
            else
               New_Lhs := New_Copy_Tree (Lhs);
               New_Rhs := New_Copy_Tree (Rhs);
            end if;

            Check :=
              Expand_Composite_Equality (Nod, Etype (C),
               Lhs =>
                 Make_Selected_Component (Loc,
                   Prefix        => New_Lhs,
                   Selector_Name => New_Occurrence_Of (C, Loc)),
               Rhs =>
                 Make_Selected_Component (Loc,
                   Prefix        => New_Rhs,
                   Selector_Name => New_Occurrence_Of (C, Loc)),
               Bodies => Bodies);

            --  If some (sub)component is an unchecked_union, the whole
            --  operation will raise program error.

            if Nkind (Check) = N_Raise_Program_Error then
               Result := Check;
               Set_Etype (Result, Standard_Boolean);
               exit;
            else
               Result :=
                 Make_And_Then (Loc,
                   Left_Opnd  => Result,
                   Right_Opnd => Check);
            end if;
         end;

         C := Element_To_Compare (Next_Entity (C));
      end loop;

      return Result;
   end Expand_Record_Equality;

   ---------------------------
   -- Expand_Set_Membership --
   ---------------------------

   procedure Expand_Set_Membership (N : Node_Id) is
      Lop : constant Node_Id := Left_Opnd (N);
      Alt : Node_Id;
      Res : Node_Id;

      function Make_Cond (Alt : Node_Id) return Node_Id;
      --  If the alternative is a subtype mark, create a simple membership
      --  test. Otherwise create an equality test for it.

      ---------------
      -- Make_Cond --
      ---------------

      function Make_Cond (Alt : Node_Id) return Node_Id is
         Cond : Node_Id;
         L    : constant Node_Id := New_Copy (Lop);
         R    : constant Node_Id := Relocate_Node (Alt);

      begin
         if (Is_Entity_Name (Alt) and then Is_Type (Entity (Alt)))
           or else Nkind (Alt) = N_Range
         then
            Cond :=
              Make_In (Sloc (Alt),
                Left_Opnd  => L,
                Right_Opnd => R);
         else
            Cond :=
              Make_Op_Eq (Sloc (Alt),
                Left_Opnd  => L,
                Right_Opnd => R);
         end if;

         return Cond;
      end Make_Cond;

   --  Start of processing for Expand_Set_Membership

   begin
      Remove_Side_Effects (Lop);

      Alt := Last (Alternatives (N));
      Res := Make_Cond (Alt);

      Prev (Alt);
      while Present (Alt) loop
         Res :=
           Make_Or_Else (Sloc (Alt),
             Left_Opnd  => Make_Cond (Alt),
             Right_Opnd => Res);
         Prev (Alt);
      end loop;

      Rewrite (N, Res);
      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_Set_Membership;

   -----------------------------------
   -- Expand_Short_Circuit_Operator --
   -----------------------------------

   --  Deal with special expansion if actions are present for the right operand
   --  and deal with optimizing case of arguments being True or False. We also
   --  deal with the special case of non-standard boolean values.

   procedure Expand_Short_Circuit_Operator (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Left    : constant Node_Id    := Left_Opnd (N);
      Right   : constant Node_Id    := Right_Opnd (N);
      LocR    : constant Source_Ptr := Sloc (Right);
      Actlist : List_Id;

      Shortcut_Value : constant Boolean := Nkind (N) = N_Or_Else;
      Shortcut_Ent   : constant Entity_Id := Boolean_Literals (Shortcut_Value);
      --  If Left = Shortcut_Value then Right need not be evaluated

   begin
      --  Deal with non-standard booleans

      if Is_Boolean_Type (Typ) then
         Adjust_Condition (Left);
         Adjust_Condition (Right);
         Set_Etype (N, Standard_Boolean);
      end if;

      --  Check for cases where left argument is known to be True or False

      if Compile_Time_Known_Value (Left) then

         --  Mark SCO for left condition as compile time known

         if Generate_SCO and then Comes_From_Source (Left) then
            Set_SCO_Condition (Left, Expr_Value_E (Left) = Standard_True);
         end if;

         --  Rewrite True AND THEN Right / False OR ELSE Right to Right.
         --  Any actions associated with Right will be executed unconditionally
         --  and can thus be inserted into the tree unconditionally.

         if Expr_Value_E (Left) /= Shortcut_Ent then
            if Present (Actions (N)) then
               Insert_Actions (N, Actions (N));
            end if;

            Rewrite (N, Right);

         --  Rewrite False AND THEN Right / True OR ELSE Right to Left.
         --  In this case we can forget the actions associated with Right,
         --  since they will never be executed.

         else
            Kill_Dead_Code (Right);
            Kill_Dead_Code (Actions (N));
            Rewrite (N, New_Occurrence_Of (Shortcut_Ent, Loc));
         end if;

         Adjust_Result_Type (N, Typ);
         return;
      end if;

      --  If Actions are present for the right operand, we have to do some
      --  special processing. We can't just let these actions filter back into
      --  code preceding the short circuit (which is what would have happened
      --  if we had not trapped them in the short-circuit form), since they
      --  must only be executed if the right operand of the short circuit is
      --  executed and not otherwise.

      if Present (Actions (N)) then
         Actlist := Actions (N);

         --  We now use an Expression_With_Actions node for the right operand
         --  of the short-circuit form. Note that this solves the traceability
         --  problems for coverage analysis.

         Rewrite (Right,
                  Make_Expression_With_Actions (LocR,
                    Expression => Relocate_Node (Right),
                    Actions    => Actlist));
         Set_Actions (N, No_List);
         Analyze_And_Resolve (Right, Standard_Boolean);

         Adjust_Result_Type (N, Typ);
         return;
      end if;

      --  No actions present, check for cases of right argument True/False

      if Compile_Time_Known_Value (Right) then

         --  Mark SCO for left condition as compile time known

         if Generate_SCO and then Comes_From_Source (Right) then
            Set_SCO_Condition (Right, Expr_Value_E (Right) = Standard_True);
         end if;

         --  Change (Left and then True), (Left or else False) to Left.
         --  Note that we know there are no actions associated with the right
         --  operand, since we just checked for this case above.

         if Expr_Value_E (Right) /= Shortcut_Ent then
            Rewrite (N, Left);

         --  Change (Left and then False), (Left or else True) to Right,
         --  making sure to preserve any side effects associated with the Left
         --  operand.

         else
            Remove_Side_Effects (Left);
            Rewrite (N, New_Occurrence_Of (Shortcut_Ent, Loc));
         end if;
      end if;

      Adjust_Result_Type (N, Typ);
   end Expand_Short_Circuit_Operator;

   -------------------------------------
   -- Fixup_Universal_Fixed_Operation --
   -------------------------------------

   procedure Fixup_Universal_Fixed_Operation (N : Node_Id) is
      Conv : constant Node_Id := Parent (N);

   begin
      --  We must have a type conversion immediately above us

      pragma Assert (Nkind (Conv) = N_Type_Conversion);

      --  Normally the type conversion gives our target type. The exception
      --  occurs in the case of the Round attribute, where the conversion
      --  will be to universal real, and our real type comes from the Round
      --  attribute (as well as an indication that we must round the result)

      if Nkind (Parent (Conv)) = N_Attribute_Reference
        and then Attribute_Name (Parent (Conv)) = Name_Round
      then
         Set_Etype (N, Etype (Parent (Conv)));
         Set_Rounded_Result (N);

      --  Normal case where type comes from conversion above us

      else
         Set_Etype (N, Etype (Conv));
      end if;
   end Fixup_Universal_Fixed_Operation;

   ---------------------------------
   -- Has_Inferable_Discriminants --
   ---------------------------------

   function Has_Inferable_Discriminants (N : Node_Id) return Boolean is

      function Prefix_Is_Formal_Parameter (N : Node_Id) return Boolean;
      --  Determines whether the left-most prefix of a selected component is a
      --  formal parameter in a subprogram. Assumes N is a selected component.

      --------------------------------
      -- Prefix_Is_Formal_Parameter --
      --------------------------------

      function Prefix_Is_Formal_Parameter (N : Node_Id) return Boolean is
         Sel_Comp : Node_Id;

      begin
         --  Move to the left-most prefix by climbing up the tree

         Sel_Comp := N;
         while Present (Parent (Sel_Comp))
           and then Nkind (Parent (Sel_Comp)) = N_Selected_Component
         loop
            Sel_Comp := Parent (Sel_Comp);
         end loop;

         return Ekind (Entity (Prefix (Sel_Comp))) in Formal_Kind;
      end Prefix_Is_Formal_Parameter;

   --  Start of processing for Has_Inferable_Discriminants

   begin
      --  For selected components, the subtype of the selector must be a
      --  constrained Unchecked_Union. If the component is subject to a
      --  per-object constraint, then the enclosing object must have inferable
      --  discriminants.

      if Nkind (N) = N_Selected_Component then
         if Has_Per_Object_Constraint (Entity (Selector_Name (N))) then

            --  A small hack. If we have a per-object constrained selected
            --  component of a formal parameter, return True since we do not
            --  know the actual parameter association yet.

            if Prefix_Is_Formal_Parameter (N) then
               return True;

            --  Otherwise, check the enclosing object and the selector

            else
               return Has_Inferable_Discriminants (Prefix (N))
                 and then Has_Inferable_Discriminants (Selector_Name (N));
            end if;

         --  The call to Has_Inferable_Discriminants will determine whether
         --  the selector has a constrained Unchecked_Union nominal type.

         else
            return Has_Inferable_Discriminants (Selector_Name (N));
         end if;

      --  A qualified expression has inferable discriminants if its subtype
      --  mark is a constrained Unchecked_Union subtype.

      elsif Nkind (N) = N_Qualified_Expression then
         return Is_Unchecked_Union (Etype (Subtype_Mark (N)))
           and then Is_Constrained (Etype (Subtype_Mark (N)));

      --  For all other names, it is sufficient to have a constrained
      --  Unchecked_Union nominal subtype.

      else
         return Is_Unchecked_Union (Base_Type (Etype (N)))
           and then Is_Constrained (Etype (N));
      end if;
   end Has_Inferable_Discriminants;

   -------------------------------
   -- Insert_Dereference_Action --
   -------------------------------

   procedure Insert_Dereference_Action (N : Node_Id) is

      function Is_Checked_Storage_Pool (P : Entity_Id) return Boolean;
      --  Return true if type of P is derived from Checked_Pool;

      -----------------------------
      -- Is_Checked_Storage_Pool --
      -----------------------------

      function Is_Checked_Storage_Pool (P : Entity_Id) return Boolean is
         T : Entity_Id;

      begin
         if No (P) then
            return False;
         end if;

         T := Etype (P);
         while T /= Etype (T) loop
            if Is_RTE (T, RE_Checked_Pool) then
               return True;
            else
               T := Etype (T);
            end if;
         end loop;

         return False;
      end Is_Checked_Storage_Pool;

      --  Local variables

      Typ   : constant Entity_Id  := Etype (N);
      Desig : constant Entity_Id  := Available_View (Designated_Type (Typ));
      Loc   : constant Source_Ptr := Sloc (N);
      Pool  : constant Entity_Id  := Associated_Storage_Pool (Typ);
      Pnod  : constant Node_Id    := Parent (N);

      Addr  : Entity_Id;
      Alig  : Entity_Id;
      Deref : Node_Id;
      Size  : Entity_Id;
      Stmt  : Node_Id;

   --  Start of processing for Insert_Dereference_Action

   begin
      pragma Assert (Nkind (Pnod) = N_Explicit_Dereference);

      --  Do not re-expand a dereference which has already been processed by
      --  this routine.

      if Has_Dereference_Action (Pnod) then
         return;

      --  Do not perform this type of expansion for internally-generated
      --  dereferences.

      elsif not Comes_From_Source (Original_Node (Pnod)) then
         return;

      --  A dereference action is only applicable to objects which have been
      --  allocated on a checked pool.

      elsif not Is_Checked_Storage_Pool (Pool) then
         return;
      end if;

      --  Extract the address of the dereferenced object. Generate:

      --    Addr : System.Address := <N>'Pool_Address;

      Addr := Make_Temporary (Loc, 'P');

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Addr,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Address), Loc),
          Expression          =>
            Make_Attribute_Reference (Loc,
              Prefix         => Duplicate_Subexpr_Move_Checks (N),
              Attribute_Name => Name_Pool_Address)));

      --  Calculate the size of the dereferenced object. Generate:

      --    Size : Storage_Count := <N>.all'Size / Storage_Unit;

      Deref :=
        Make_Explicit_Dereference (Loc,
          Prefix => Duplicate_Subexpr_Move_Checks (N));
      Set_Has_Dereference_Action (Deref);

      Size := Make_Temporary (Loc, 'S');

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Size,

          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Storage_Count), Loc),

          Expression          =>
            Make_Op_Divide (Loc,
              Left_Opnd   =>
                Make_Attribute_Reference (Loc,
                  Prefix         => Deref,
                  Attribute_Name => Name_Size),
               Right_Opnd =>
                 Make_Integer_Literal (Loc, System_Storage_Unit))));

      --  Calculate the alignment of the dereferenced object. Generate:
      --    Alig : constant Storage_Count := <N>.all'Alignment;

      Deref :=
        Make_Explicit_Dereference (Loc,
          Prefix => Duplicate_Subexpr_Move_Checks (N));
      Set_Has_Dereference_Action (Deref);

      Alig := Make_Temporary (Loc, 'A');

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Alig,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Storage_Count), Loc),
          Expression          =>
            Make_Attribute_Reference (Loc,
              Prefix         => Deref,
              Attribute_Name => Name_Alignment)));

      --  A dereference of a controlled object requires special processing. The
      --  finalization machinery requests additional space from the underlying
      --  pool to allocate and hide two pointers. As a result, a checked pool
      --  may mark the wrong memory as valid. Since checked pools do not have
      --  knowledge of hidden pointers, we have to bring the two pointers back
      --  in view in order to restore the original state of the object.

      if Needs_Finalization (Desig) then

         --  Adjust the address and size of the dereferenced object. Generate:
         --    Adjust_Controlled_Dereference (Addr, Size, Alig);

         Stmt :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Adjust_Controlled_Dereference), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Addr, Loc),
               New_Occurrence_Of (Size, Loc),
               New_Occurrence_Of (Alig, Loc)));

         --  Class-wide types complicate things because we cannot determine
         --  statically whether the actual object is truly controlled. We must
         --  generate a runtime check to detect this property. Generate:
         --
         --    if Needs_Finalization (<N>.all'Tag) then
         --       <Stmt>;
         --    end if;

         if Is_Class_Wide_Type (Desig) then
            Deref :=
              Make_Explicit_Dereference (Loc,
                Prefix => Duplicate_Subexpr_Move_Checks (N));
            Set_Has_Dereference_Action (Deref);

            Stmt :=
              Make_Implicit_If_Statement (N,
                Condition       =>
                  Make_Function_Call (Loc,
                    Name                   =>
                      New_Occurrence_Of (RTE (RE_Needs_Finalization), Loc),
                    Parameter_Associations => New_List (
                      Make_Attribute_Reference (Loc,
                        Prefix         => Deref,
                        Attribute_Name => Name_Tag))),
                Then_Statements => New_List (Stmt));
         end if;

         Insert_Action (N, Stmt);
      end if;

      --  Generate:
      --    Dereference (Pool, Addr, Size, Alig);

      Insert_Action (N,
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of
              (Find_Prim_Op (Etype (Pool), Name_Dereference), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Pool, Loc),
            New_Occurrence_Of (Addr, Loc),
            New_Occurrence_Of (Size, Loc),
            New_Occurrence_Of (Alig, Loc))));

      --  Mark the explicit dereference as processed to avoid potential
      --  infinite expansion.

      Set_Has_Dereference_Action (Pnod);

   exception
      when RE_Not_Available =>
         return;
   end Insert_Dereference_Action;

   --------------------------------
   -- Integer_Promotion_Possible --
   --------------------------------

   function Integer_Promotion_Possible (N : Node_Id) return Boolean is
      Operand           : constant Node_Id   := Expression (N);
      Operand_Type      : constant Entity_Id := Etype (Operand);
      Root_Operand_Type : constant Entity_Id := Root_Type (Operand_Type);

   begin
      pragma Assert (Nkind (N) = N_Type_Conversion);

      return

           --  We only do the transformation for source constructs. We assume
           --  that the expander knows what it is doing when it generates code.

           Comes_From_Source (N)

           --  If the operand type is Short_Integer or Short_Short_Integer,
           --  then we will promote to Integer, which is available on all
           --  targets, and is sufficient to ensure no intermediate overflow.
           --  Furthermore it is likely to be as efficient or more efficient
           --  than using the smaller type for the computation so we do this
           --  unconditionally.

           and then
             (Root_Operand_Type = Base_Type (Standard_Short_Integer)
                or else
              Root_Operand_Type = Base_Type (Standard_Short_Short_Integer))

           --  Test for interesting operation, which includes addition,
           --  division, exponentiation, multiplication, subtraction, absolute
           --  value and unary negation. Unary "+" is omitted since it is a
           --  no-op and thus can't overflow.

           and then Nkind_In (Operand, N_Op_Abs,
                                       N_Op_Add,
                                       N_Op_Divide,
                                       N_Op_Expon,
                                       N_Op_Minus,
                                       N_Op_Multiply,
                                       N_Op_Subtract);
   end Integer_Promotion_Possible;

   ------------------------------
   -- Make_Array_Comparison_Op --
   ------------------------------

   --  This is a hand-coded expansion of the following generic function:

   --  generic
   --    type elem is  (<>);
   --    type index is (<>);
   --    type a is array (index range <>) of elem;

   --  function Gnnn (X : a; Y: a) return boolean is
   --    J : index := Y'first;

   --  begin
   --    if X'length = 0 then
   --       return false;

   --    elsif Y'length = 0 then
   --       return true;

   --    else
   --      for I in X'range loop
   --        if X (I) = Y (J) then
   --          if J = Y'last then
   --            exit;
   --          else
   --            J := index'succ (J);
   --          end if;

   --        else
   --           return X (I) > Y (J);
   --        end if;
   --      end loop;

   --      return X'length > Y'length;
   --    end if;
   --  end Gnnn;

   --  Note that since we are essentially doing this expansion by hand, we
   --  do not need to generate an actual or formal generic part, just the
   --  instantiated function itself.

   function Make_Array_Comparison_Op
     (Typ : Entity_Id;
      Nod : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      X : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uX);
      Y : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uY);
      I : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uI);
      J : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uJ);

      Index : constant Entity_Id := Base_Type (Etype (First_Index (Typ)));

      Loop_Statement : Node_Id;
      Loop_Body      : Node_Id;
      If_Stat        : Node_Id;
      Inner_If       : Node_Id;
      Final_Expr     : Node_Id;
      Func_Body      : Node_Id;
      Func_Name      : Entity_Id;
      Formals        : List_Id;
      Length1        : Node_Id;
      Length2        : Node_Id;

   begin
      --  if J = Y'last then
      --     exit;
      --  else
      --     J := index'succ (J);
      --  end if;

      Inner_If :=
        Make_Implicit_If_Statement (Nod,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd => New_Occurrence_Of (J, Loc),
              Right_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix => New_Occurrence_Of (Y, Loc),
                  Attribute_Name => Name_Last)),

          Then_Statements => New_List (
                Make_Exit_Statement (Loc)),

          Else_Statements =>
            New_List (
              Make_Assignment_Statement (Loc,
                Name => New_Occurrence_Of (J, Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (Index, Loc),
                    Attribute_Name => Name_Succ,
                    Expressions => New_List (New_Occurrence_Of (J, Loc))))));

      --  if X (I) = Y (J) then
      --     if ... end if;
      --  else
      --     return X (I) > Y (J);
      --  end if;

      Loop_Body :=
        Make_Implicit_If_Statement (Nod,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd =>
                Make_Indexed_Component (Loc,
                  Prefix      => New_Occurrence_Of (X, Loc),
                  Expressions => New_List (New_Occurrence_Of (I, Loc))),

              Right_Opnd =>
                Make_Indexed_Component (Loc,
                  Prefix      => New_Occurrence_Of (Y, Loc),
                  Expressions => New_List (New_Occurrence_Of (J, Loc)))),

          Then_Statements => New_List (Inner_If),

          Else_Statements => New_List (
            Make_Simple_Return_Statement (Loc,
              Expression =>
                Make_Op_Gt (Loc,
                  Left_Opnd =>
                    Make_Indexed_Component (Loc,
                      Prefix      => New_Occurrence_Of (X, Loc),
                      Expressions => New_List (New_Occurrence_Of (I, Loc))),

                  Right_Opnd =>
                    Make_Indexed_Component (Loc,
                      Prefix      => New_Occurrence_Of (Y, Loc),
                      Expressions => New_List (
                        New_Occurrence_Of (J, Loc)))))));

      --  for I in X'range loop
      --     if ... end if;
      --  end loop;

      Loop_Statement :=
        Make_Implicit_Loop_Statement (Nod,
          Identifier => Empty,

          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier => I,
                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (X, Loc),
                      Attribute_Name => Name_Range))),

          Statements => New_List (Loop_Body));

      --    if X'length = 0 then
      --       return false;
      --    elsif Y'length = 0 then
      --       return true;
      --    else
      --      for ... loop ... end loop;
      --      return X'length > Y'length;
      --    end if;

      Length1 :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (X, Loc),
          Attribute_Name => Name_Length);

      Length2 :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Y, Loc),
          Attribute_Name => Name_Length);

      Final_Expr :=
        Make_Op_Gt (Loc,
          Left_Opnd  => Length1,
          Right_Opnd => Length2);

      If_Stat :=
        Make_Implicit_If_Statement (Nod,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix => New_Occurrence_Of (X, Loc),
                  Attribute_Name => Name_Length),
              Right_Opnd =>
                Make_Integer_Literal (Loc, 0)),

          Then_Statements =>
            New_List (
              Make_Simple_Return_Statement (Loc,
                Expression => New_Occurrence_Of (Standard_False, Loc))),

          Elsif_Parts => New_List (
            Make_Elsif_Part (Loc,
              Condition =>
                Make_Op_Eq (Loc,
                  Left_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Y, Loc),
                      Attribute_Name => Name_Length),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, 0)),

              Then_Statements =>
                New_List (
                  Make_Simple_Return_Statement (Loc,
                     Expression => New_Occurrence_Of (Standard_True, Loc))))),

          Else_Statements => New_List (
            Loop_Statement,
            Make_Simple_Return_Statement (Loc,
              Expression => Final_Expr)));

      --  (X : a; Y: a)

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => X,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Y,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)));

      --  function Gnnn (...) return boolean is
      --    J : index := Y'first;
      --  begin
      --    if ... end if;
      --  end Gnnn;

      Func_Name := Make_Temporary (Loc, 'G');

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Result_Definition => New_Occurrence_Of (Standard_Boolean, Loc)),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => J,
              Object_Definition   => New_Occurrence_Of (Index, Loc),
              Expression =>
                Make_Attribute_Reference (Loc,
                  Prefix => New_Occurrence_Of (Y, Loc),
                  Attribute_Name => Name_First))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (If_Stat)));

      return Func_Body;
   end Make_Array_Comparison_Op;

   ---------------------------
   -- Make_Boolean_Array_Op --
   ---------------------------

   --  For logical operations on boolean arrays, expand in line the following,
   --  replacing 'and' with 'or' or 'xor' where needed:

   --    function Annn (A : typ; B: typ) return typ is
   --       C : typ;
   --    begin
   --       for J in A'range loop
   --          C (J) := A (J) op B (J);
   --       end loop;
   --       return C;
   --    end Annn;

   --  Here typ is the boolean array type

   function Make_Boolean_Array_Op
     (Typ : Entity_Id;
      N   : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

      A : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uA);
      B : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uB);
      C : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uC);
      J : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uJ);

      A_J : Node_Id;
      B_J : Node_Id;
      C_J : Node_Id;
      Op  : Node_Id;

      Formals        : List_Id;
      Func_Name      : Entity_Id;
      Func_Body      : Node_Id;
      Loop_Statement : Node_Id;

   begin
      A_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Occurrence_Of (A, Loc),
          Expressions => New_List (New_Occurrence_Of (J, Loc)));

      B_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Occurrence_Of (B, Loc),
          Expressions => New_List (New_Occurrence_Of (J, Loc)));

      C_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Occurrence_Of (C, Loc),
          Expressions => New_List (New_Occurrence_Of (J, Loc)));

      if Nkind (N) = N_Op_And then
         Op :=
           Make_Op_And (Loc,
             Left_Opnd  => A_J,
             Right_Opnd => B_J);

      elsif Nkind (N) = N_Op_Or then
         Op :=
           Make_Op_Or (Loc,
             Left_Opnd  => A_J,
             Right_Opnd => B_J);

      else
         Op :=
           Make_Op_Xor (Loc,
             Left_Opnd  => A_J,
             Right_Opnd => B_J);
      end if;

      Loop_Statement :=
        Make_Implicit_Loop_Statement (N,
          Identifier => Empty,

          Iteration_Scheme =>
            Make_Iteration_Scheme (Loc,
              Loop_Parameter_Specification =>
                Make_Loop_Parameter_Specification (Loc,
                  Defining_Identifier => J,
                  Discrete_Subtype_Definition =>
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (A, Loc),
                      Attribute_Name => Name_Range))),

          Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => C_J,
              Expression => Op)));

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type      => New_Occurrence_Of (Typ, Loc)));

      Func_Name := Make_Temporary (Loc, 'A');
      Set_Is_Inlined (Func_Name);

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Result_Definition        => New_Occurrence_Of (Typ, Loc)),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => C,
              Object_Definition   => New_Occurrence_Of (Typ, Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Loop_Statement,
                Make_Simple_Return_Statement (Loc,
                  Expression => New_Occurrence_Of (C, Loc)))));

      return Func_Body;
   end Make_Boolean_Array_Op;

   -----------------------------------------
   -- Minimized_Eliminated_Overflow_Check --
   -----------------------------------------

   function Minimized_Eliminated_Overflow_Check (N : Node_Id) return Boolean is
   begin
      return
        Is_Signed_Integer_Type (Etype (N))
          and then Overflow_Check_Mode in Minimized_Or_Eliminated;
   end Minimized_Eliminated_Overflow_Check;

   --------------------------------
   -- Optimize_Length_Comparison --
   --------------------------------

   procedure Optimize_Length_Comparison (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Typ    : constant Entity_Id  := Etype (N);
      Result : Node_Id;

      Left  : Node_Id;
      Right : Node_Id;
      --  First and Last attribute reference nodes, which end up as left and
      --  right operands of the optimized result.

      Is_Zero : Boolean;
      --  True for comparison operand of zero

      Comp : Node_Id;
      --  Comparison operand, set only if Is_Zero is false

      Ent : Entity_Id;
      --  Entity whose length is being compared

      Index : Node_Id;
      --  Integer_Literal node for length attribute expression, or Empty
      --  if there is no such expression present.

      Ityp  : Entity_Id;
      --  Type of array index to which 'Length is applied

      Op : Node_Kind := Nkind (N);
      --  Kind of comparison operator, gets flipped if operands backwards

      function Is_Optimizable (N : Node_Id) return Boolean;
      --  Tests N to see if it is an optimizable comparison value (defined as
      --  constant zero or one, or something else where the value is known to
      --  be positive and in the range of 32-bits, and where the corresponding
      --  Length value is also known to be 32-bits. If result is true, sets
      --  Is_Zero, Ityp, and Comp accordingly.

      function Is_Entity_Length (N : Node_Id) return Boolean;
      --  Tests if N is a length attribute applied to a simple entity. If so,
      --  returns True, and sets Ent to the entity, and Index to the integer
      --  literal provided as an attribute expression, or to Empty if none.
      --  Also returns True if the expression is a generated type conversion
      --  whose expression is of the desired form. This latter case arises
      --  when Apply_Universal_Integer_Attribute_Check installs a conversion
      --  to check for being in range, which is not needed in this context.
      --  Returns False if neither condition holds.

      function Prepare_64 (N : Node_Id) return Node_Id;
      --  Given a discrete expression, returns a Long_Long_Integer typed
      --  expression representing the underlying value of the expression.
      --  This is done with an unchecked conversion to the result type. We
      --  use unchecked conversion to handle the enumeration type case.

      ----------------------
      -- Is_Entity_Length --
      ----------------------

      function Is_Entity_Length (N : Node_Id) return Boolean is
      begin
         if Nkind (N) = N_Attribute_Reference
           and then Attribute_Name (N) = Name_Length
           and then Is_Entity_Name (Prefix (N))
         then
            Ent := Entity (Prefix (N));

            if Present (Expressions (N)) then
               Index := First (Expressions (N));
            else
               Index := Empty;
            end if;

            return True;

         elsif Nkind (N) = N_Type_Conversion
           and then not Comes_From_Source (N)
         then
            return Is_Entity_Length (Expression (N));

         else
            return False;
         end if;
      end Is_Entity_Length;

      --------------------
      -- Is_Optimizable --
      --------------------

      function Is_Optimizable (N : Node_Id) return Boolean is
         Val  : Uint;
         OK   : Boolean;
         Lo   : Uint;
         Hi   : Uint;
         Indx : Node_Id;

      begin
         if Compile_Time_Known_Value (N) then
            Val := Expr_Value (N);

            if Val = Uint_0 then
               Is_Zero := True;
               Comp    := Empty;
               return True;

            elsif Val = Uint_1 then
               Is_Zero := False;
               Comp    := Empty;
               return True;
            end if;
         end if;

         --  Here we have to make sure of being within 32-bits

         Determine_Range (N, OK, Lo, Hi, Assume_Valid => True);

         if not OK
           or else Lo < Uint_1
           or else Hi > UI_From_Int (Int'Last)
         then
            return False;
         end if;

         --  Comparison value was within range, so now we must check the index
         --  value to make sure it is also within 32-bits.

         Indx := First_Index (Etype (Ent));

         if Present (Index) then
            for J in 2 .. UI_To_Int (Intval (Index)) loop
               Next_Index (Indx);
            end loop;
         end if;

         Ityp := Etype (Indx);

         if Esize (Ityp) > 32 then
            return False;
         end if;

         Is_Zero := False;
         Comp := N;
         return True;
      end Is_Optimizable;

      ----------------
      -- Prepare_64 --
      ----------------

      function Prepare_64 (N : Node_Id) return Node_Id is
      begin
         return Unchecked_Convert_To (Standard_Long_Long_Integer, N);
      end Prepare_64;

   --  Start of processing for Optimize_Length_Comparison

   begin
      --  Nothing to do if not a comparison

      if Op not in N_Op_Compare then
         return;
      end if;

      --  Nothing to do if special -gnatd.P debug flag set

      if Debug_Flag_Dot_PP then
         return;
      end if;

      --  Ent'Length op 0/1

      if Is_Entity_Length (Left_Opnd (N))
        and then Is_Optimizable (Right_Opnd (N))
      then
         null;

      --  0/1 op Ent'Length

      elsif Is_Entity_Length (Right_Opnd (N))
        and then Is_Optimizable (Left_Opnd (N))
      then
         --  Flip comparison to opposite sense

         case Op is
            when N_Op_Lt => Op := N_Op_Gt;
            when N_Op_Le => Op := N_Op_Ge;
            when N_Op_Gt => Op := N_Op_Lt;
            when N_Op_Ge => Op := N_Op_Le;
            when others  => null;
         end case;

      --  Else optimization not possible

      else
         return;
      end if;

      --  Fall through if we will do the optimization

      --  Cases to handle:

      --    X'Length = 0  => X'First > X'Last
      --    X'Length = 1  => X'First = X'Last
      --    X'Length = n  => X'First + (n - 1) = X'Last

      --    X'Length /= 0 => X'First <= X'Last
      --    X'Length /= 1 => X'First /= X'Last
      --    X'Length /= n => X'First + (n - 1) /= X'Last

      --    X'Length >= 0 => always true, warn
      --    X'Length >= 1 => X'First <= X'Last
      --    X'Length >= n => X'First + (n - 1) <= X'Last

      --    X'Length > 0  => X'First <= X'Last
      --    X'Length > 1  => X'First < X'Last
      --    X'Length > n  => X'First + (n - 1) < X'Last

      --    X'Length <= 0 => X'First > X'Last (warn, could be =)
      --    X'Length <= 1 => X'First >= X'Last
      --    X'Length <= n => X'First + (n - 1) >= X'Last

      --    X'Length < 0  => always false (warn)
      --    X'Length < 1  => X'First > X'Last
      --    X'Length < n  => X'First + (n - 1) > X'Last

      --  Note: for the cases of n (not constant 0,1), we require that the
      --  corresponding index type be integer or shorter (i.e. not 64-bit),
      --  and the same for the comparison value. Then we do the comparison
      --  using 64-bit arithmetic (actually long long integer), so that we
      --  cannot have overflow intefering with the result.

      --  First deal with warning cases

      if Is_Zero then
         case Op is

            --  X'Length >= 0

            when N_Op_Ge =>
               Rewrite (N,
                 Convert_To (Typ, New_Occurrence_Of (Standard_True, Loc)));
               Analyze_And_Resolve (N, Typ);
               Warn_On_Known_Condition (N);
               return;

            --  X'Length < 0

            when N_Op_Lt =>
               Rewrite (N,
                 Convert_To (Typ, New_Occurrence_Of (Standard_False, Loc)));
               Analyze_And_Resolve (N, Typ);
               Warn_On_Known_Condition (N);
               return;

            when N_Op_Le =>
               if Constant_Condition_Warnings
                 and then Comes_From_Source (Original_Node (N))
               then
                  Error_Msg_N ("could replace by ""'=""?c?", N);
               end if;

               Op := N_Op_Eq;

            when others =>
               null;
         end case;
      end if;

      --  Build the First reference we will use

      Left :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Ent, Loc),
          Attribute_Name => Name_First);

      if Present (Index) then
         Set_Expressions (Left, New_List (New_Copy (Index)));
      end if;

      --  If general value case, then do the addition of (n - 1), and
      --  also add the needed conversions to type Long_Long_Integer.

      if Present (Comp) then
         Left :=
           Make_Op_Add (Loc,
             Left_Opnd  => Prepare_64 (Left),
             Right_Opnd =>
               Make_Op_Subtract (Loc,
                 Left_Opnd  => Prepare_64 (Comp),
                 Right_Opnd => Make_Integer_Literal (Loc, 1)));
      end if;

      --  Build the Last reference we will use

      Right :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Ent, Loc),
          Attribute_Name => Name_Last);

      if Present (Index) then
         Set_Expressions (Right, New_List (New_Copy (Index)));
      end if;

      --  If general operand, convert Last reference to Long_Long_Integer

      if Present (Comp) then
         Right := Prepare_64 (Right);
      end if;

      --  Check for cases to optimize

      --  X'Length = 0  => X'First > X'Last
      --  X'Length < 1  => X'First > X'Last
      --  X'Length < n  => X'First + (n - 1) > X'Last

      if (Is_Zero and then Op = N_Op_Eq)
        or else (not Is_Zero and then Op = N_Op_Lt)
      then
         Result :=
           Make_Op_Gt (Loc,
             Left_Opnd  => Left,
             Right_Opnd => Right);

      --  X'Length = 1  => X'First = X'Last
      --  X'Length = n  => X'First + (n - 1) = X'Last

      elsif not Is_Zero and then Op = N_Op_Eq then
         Result :=
           Make_Op_Eq (Loc,
             Left_Opnd  => Left,
             Right_Opnd => Right);

      --  X'Length /= 0 => X'First <= X'Last
      --  X'Length > 0  => X'First <= X'Last

      elsif Is_Zero and (Op = N_Op_Ne or else Op = N_Op_Gt) then
         Result :=
           Make_Op_Le (Loc,
             Left_Opnd  => Left,
             Right_Opnd => Right);

      --  X'Length /= 1 => X'First /= X'Last
      --  X'Length /= n => X'First + (n - 1) /= X'Last

      elsif not Is_Zero and then Op = N_Op_Ne then
         Result :=
           Make_Op_Ne (Loc,
             Left_Opnd  => Left,
             Right_Opnd => Right);

      --  X'Length >= 1 => X'First <= X'Last
      --  X'Length >= n => X'First + (n - 1) <= X'Last

      elsif not Is_Zero and then Op = N_Op_Ge then
         Result :=
           Make_Op_Le (Loc,
             Left_Opnd  => Left,
                       Right_Opnd => Right);

      --  X'Length > 1  => X'First < X'Last
      --  X'Length > n  => X'First + (n = 1) < X'Last

      elsif not Is_Zero and then Op = N_Op_Gt then
         Result :=
           Make_Op_Lt (Loc,
             Left_Opnd  => Left,
             Right_Opnd => Right);

      --  X'Length <= 1 => X'First >= X'Last
      --  X'Length <= n => X'First + (n - 1) >= X'Last

      elsif not Is_Zero and then Op = N_Op_Le then
         Result :=
           Make_Op_Ge (Loc,
             Left_Opnd  => Left,
             Right_Opnd => Right);

      --  Should not happen at this stage

      else
         raise Program_Error;
      end if;

      --  Rewrite and finish up

      Rewrite (N, Result);
      Analyze_And_Resolve (N, Typ);
      return;
   end Optimize_Length_Comparison;

   ------------------------------
   -- Process_Transient_Object --
   ------------------------------

   procedure Process_Transient_Object
     (Decl     : Node_Id;
      Rel_Node : Node_Id)
   is
      Hook_Context         : Node_Id;
      --  Node on which to insert the hook pointer (as an action)

      Finalization_Context : Node_Id;
      --  Node after which to insert finalization actions

      Finalize_Always : Boolean;
      --  If False, call to finalizer includes a test of whether the
      --  hook pointer is null.

      procedure Find_Enclosing_Contexts (N : Node_Id);
      --  Find the logical context where N appears, and initializae
      --  Hook_Context and Finalization_Context accordingly. Also
      --  sets Finalize_Always.

      -----------------------------
      -- Find_Enclosing_Contexts --
      -----------------------------

      procedure Find_Enclosing_Contexts (N : Node_Id) is
         Par : Node_Id;
         Top : Node_Id;

         Wrapped_Node : Node_Id;
         --  Note: if we are in a transient scope, we want to reuse it as
         --  the context for actions insertion, if possible. But if N is itself
         --  part of the stored actions for the current transient scope,
         --  then we need to insert at the appropriate (inner) location in
         --  the not as an action on Node_To_Be_Wrapped.

         In_Cond_Expr : constant Boolean := Within_Case_Or_If_Expression (N);

      begin
         --  When the node is inside a case/if expression, the lifetime of any
         --  temporary controlled object is extended. Find a suitable insertion
         --  node by locating the topmost case or if expressions.

         if In_Cond_Expr then
            Par := N;
            Top := N;
            while Present (Par) loop
               if Nkind_In (Original_Node (Par), N_Case_Expression,
                                                 N_If_Expression)
               then
                  Top := Par;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Par := Parent (Par);
            end loop;

            --  The topmost case or if expression is now recovered, but it may
            --  still not be the correct place to add generated code. Climb to
            --  find a parent that is part of a declarative or statement list,
            --  and is not a list of actuals in a call.

            Par := Top;
            while Present (Par) loop
               if Is_List_Member (Par)
                 and then not Nkind_In (Par, N_Component_Association,
                                             N_Discriminant_Association,
                                             N_Parameter_Association,
                                             N_Pragma_Argument_Association)
                 and then not Nkind_In
                                (Parent (Par), N_Function_Call,
                                               N_Procedure_Call_Statement,
                                               N_Entry_Call_Statement)

               then
                  Hook_Context := Par;
                  goto Hook_Context_Found;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Par := Parent (Par);
            end loop;

            Hook_Context := Par;
            goto Hook_Context_Found;

         else
            Par := N;
            while Present (Par) loop

               --  Keep climbing past various operators

               if Nkind (Parent (Par)) in N_Op
                 or else Nkind_In (Parent (Par), N_And_Then, N_Or_Else)
               then
                  Par := Parent (Par);
               else
                  exit;
               end if;
            end loop;

            Top := Par;

            --  The node may be located in a pragma in which case return the
            --  pragma itself:

            --    pragma Precondition (... and then Ctrl_Func_Call ...);

            --  Similar case occurs when the node is related to an object
            --  declaration or assignment:

            --    Obj [: Some_Typ] := ... and then Ctrl_Func_Call ...;

            --  Another case to consider is when the node is part of a return
            --  statement:

            --    return ... and then Ctrl_Func_Call ...;

            --  Another case is when the node acts as a formal in a procedure
            --  call statement:

            --    Proc (... and then Ctrl_Func_Call ...);

            if Scope_Is_Transient then
               Wrapped_Node := Node_To_Be_Wrapped;
            else
               Wrapped_Node := Empty;
            end if;

            while Present (Par) loop
               if Par = Wrapped_Node
                 or else Nkind_In (Par, N_Assignment_Statement,
                                        N_Object_Declaration,
                                        N_Pragma,
                                        N_Procedure_Call_Statement,
                                        N_Simple_Return_Statement)
               then
                  Hook_Context := Par;
                  goto Hook_Context_Found;

               --  Prevent the search from going too far

               elsif Is_Body_Or_Package_Declaration (Par) then
                  exit;
               end if;

               Par := Parent (Par);
            end loop;

            --  Return the topmost short circuit operator

            Hook_Context := Top;
         end if;

      <<Hook_Context_Found>>

         --  Special case for Boolean EWAs: capture expression in a temporary,
         --  whose declaration will serve as the context around which to insert
         --  finalization code. The finalization thus remains local to the
         --  specific condition being evaluated.

         if Is_Boolean_Type (Etype (N)) then

            --  In this case, the finalization context is chosen so that
            --  we know at finalization point that the hook pointer is
            --  never null, so no need for a test, we can call the finalizer
            --  unconditionally, except in the case where the object is
            --  created in a specific branch of a conditional expression.

            Finalize_Always :=
               not (In_Cond_Expr
                     or else
                       Nkind_In (Original_Node (N), N_Case_Expression,
                                                    N_If_Expression));

            declare
               Loc  : constant Source_Ptr := Sloc (N);
               Temp : constant Entity_Id := Make_Temporary (Loc, 'E', N);

            begin
               Append_To (Actions (N),
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Occurrence_Of (Etype (N), Loc),
                   Expression          => Expression (N)));
               Finalization_Context := Last (Actions (N));

               Analyze (Last (Actions (N)));

               Set_Expression (N, New_Occurrence_Of (Temp, Loc));
               Analyze (Expression (N));
            end;

         else
            Finalize_Always := False;
            Finalization_Context := Hook_Context;
         end if;
      end Find_Enclosing_Contexts;

      --  Local variables

      Loc       : constant Source_Ptr := Sloc (Decl);
      Obj_Id    : constant Entity_Id  := Defining_Identifier (Decl);
      Obj_Typ   : constant Node_Id    := Etype (Obj_Id);
      Desig_Typ : Entity_Id;
      Expr      : Node_Id;
      Fin_Stmts : List_Id;
      Ptr_Id    : Entity_Id;
      Temp_Id   : Entity_Id;
      Temp_Ins  : Node_Id;

   --  Start of processing for Process_Transient_Object

   begin
      Find_Enclosing_Contexts (Rel_Node);

      --  Step 1: Create the access type which provides a reference to the
      --  transient controlled object.

      if Is_Access_Type (Obj_Typ) then
         Desig_Typ := Directly_Designated_Type (Obj_Typ);
      else
         Desig_Typ := Obj_Typ;
      end if;

      Desig_Typ := Base_Type (Desig_Typ);

      --  Generate:
      --    Ann : access [all] <Desig_Typ>;

      Ptr_Id := Make_Temporary (Loc, 'A');

      Insert_Action (Hook_Context,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Ptr_Id,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present        => Ekind (Obj_Typ) = E_General_Access_Type,
              Subtype_Indication => New_Occurrence_Of (Desig_Typ, Loc))));

      --  Step 2: Create a temporary which acts as a hook to the transient
      --  controlled object. Generate:

      --    Temp : Ptr_Id := null;

      Temp_Id := Make_Temporary (Loc, 'T');

      Insert_Action (Hook_Context,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Temp_Id,
          Object_Definition   => New_Occurrence_Of (Ptr_Id, Loc)));

      --  Mark the temporary as created for the purposes of exporting the
      --  transient controlled object out of the expression_with_action or if
      --  expression. This signals the machinery in Build_Finalizer to treat
      --  this case specially.

      Set_Status_Flag_Or_Transient_Decl (Temp_Id, Decl);

      --  Step 3: Hook the transient object to the temporary

      --  This must be inserted right after the object declaration, so that
      --  the assignment is executed if, and only if, the object is actually
      --  created (whereas the declaration of the hook pointer, and the
      --  finalization call, may be inserted at an outer level, and may
      --  remain unused for some executions, if the actual creation of
      --  the object is conditional).

      --  The use of unchecked conversion / unrestricted access is needed to
      --  avoid an accessibility violation. Note that the finalization code is
      --  structured in such a way that the "hook" is processed only when it
      --  points to an existing object.

      if Is_Access_Type (Obj_Typ) then
         Expr :=
           Unchecked_Convert_To (Ptr_Id, New_Occurrence_Of (Obj_Id, Loc));
      else
         Expr :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Obj_Id, Loc),
             Attribute_Name => Name_Unrestricted_Access);
      end if;

      --  Generate:
      --    Temp := Ptr_Id (Obj_Id);
      --      <or>
      --    Temp := Obj_Id'Unrestricted_Access;

      --  When the transient object is initialized by an aggregate, the hook
      --  must capture the object after the last component assignment takes
      --  place. Only then is the object fully initialized.

      if Ekind (Obj_Id) = E_Variable
        and then Present (Last_Aggregate_Assignment (Obj_Id))
      then
         Temp_Ins := Last_Aggregate_Assignment (Obj_Id);

      --  Otherwise the hook seizes the related object immediately

      else
         Temp_Ins := Decl;
      end if;

      Insert_After_And_Analyze (Temp_Ins,
        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (Temp_Id, Loc),
          Expression => Expr));

      --  Step 4: Finalize the transient controlled object after the context
      --  has been evaluated/elaborated. Generate:

      --    if Temp /= null then
      --       [Deep_]Finalize (Temp.all);
      --       Temp := null;
      --    end if;

      --  When the node is part of a return statement, there is no need to
      --  insert a finalization call, as the general finalization mechanism
      --  (see Build_Finalizer) would take care of the transient controlled
      --  object on subprogram exit. Note that it would also be impossible to
      --  insert the finalization code after the return statement as this will
      --  render it unreachable.

      if Nkind (Finalization_Context) /= N_Simple_Return_Statement then
         Fin_Stmts := New_List (
           Make_Final_Call
             (Obj_Ref =>
                Make_Explicit_Dereference (Loc,
                  Prefix => New_Occurrence_Of (Temp_Id, Loc)),
              Typ     => Desig_Typ),

           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Temp_Id, Loc),
             Expression => Make_Null (Loc)));

         if not Finalize_Always then
            Fin_Stmts := New_List (
              Make_Implicit_If_Statement (Decl,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => New_Occurrence_Of (Temp_Id, Loc),
                    Right_Opnd => Make_Null (Loc)),
                Then_Statements => Fin_Stmts));
         end if;

         Insert_Actions_After (Finalization_Context, Fin_Stmts);
      end if;
   end Process_Transient_Object;

   ------------------------
   -- Rewrite_Comparison --
   ------------------------

   procedure Rewrite_Comparison (N : Node_Id) is
      Warning_Generated : Boolean := False;
      --  Set to True if first pass with Assume_Valid generates a warning in
      --  which case we skip the second pass to avoid warning overloaded.

      Result : Node_Id;
      --  Set to Standard_True or Standard_False

   begin
      if Nkind (N) = N_Type_Conversion then
         Rewrite_Comparison (Expression (N));
         return;

      elsif Nkind (N) not in N_Op_Compare then
         return;
      end if;

      --  Now start looking at the comparison in detail. We potentially go
      --  through this loop twice. The first time, Assume_Valid is set False
      --  in the call to Compile_Time_Compare. If this call results in a
      --  clear result of always True or Always False, that's decisive and
      --  we are done. Otherwise we repeat the processing with Assume_Valid
      --  set to True to generate additional warnings. We can skip that step
      --  if Constant_Condition_Warnings is False.

      for AV in False .. True loop
         declare
            Typ : constant Entity_Id := Etype (N);
            Op1 : constant Node_Id   := Left_Opnd (N);
            Op2 : constant Node_Id   := Right_Opnd (N);

            Res : constant Compare_Result :=
                    Compile_Time_Compare (Op1, Op2, Assume_Valid => AV);
            --  Res indicates if compare outcome can be compile time determined

            True_Result  : Boolean;
            False_Result : Boolean;

         begin
            case N_Op_Compare (Nkind (N)) is
            when N_Op_Eq =>
               True_Result  := Res = EQ;
               False_Result := Res = LT or else Res = GT or else Res = NE;

            when N_Op_Ge =>
               True_Result  := Res in Compare_GE;
               False_Result := Res = LT;

               if Res = LE
                 and then Constant_Condition_Warnings
                 and then Comes_From_Source (Original_Node (N))
                 and then Nkind (Original_Node (N)) = N_Op_Ge
                 and then not In_Instance
                 and then Is_Integer_Type (Etype (Left_Opnd (N)))
                 and then not Has_Warnings_Off (Etype (Left_Opnd (N)))
               then
                  Error_Msg_N
                    ("can never be greater than, could replace by ""'=""?c?",
                     N);
                  Warning_Generated := True;
               end if;

            when N_Op_Gt =>
               True_Result  := Res = GT;
               False_Result := Res in Compare_LE;

            when N_Op_Lt =>
               True_Result  := Res = LT;
               False_Result := Res in Compare_GE;

            when N_Op_Le =>
               True_Result  := Res in Compare_LE;
               False_Result := Res = GT;

               if Res = GE
                 and then Constant_Condition_Warnings
                 and then Comes_From_Source (Original_Node (N))
                 and then Nkind (Original_Node (N)) = N_Op_Le
                 and then not In_Instance
                 and then Is_Integer_Type (Etype (Left_Opnd (N)))
                 and then not Has_Warnings_Off (Etype (Left_Opnd (N)))
               then
                  Error_Msg_N
                    ("can never be less than, could replace by ""'=""?c?", N);
                  Warning_Generated := True;
               end if;

            when N_Op_Ne =>
               True_Result  := Res = NE or else Res = GT or else Res = LT;
               False_Result := Res = EQ;
            end case;

            --  If this is the first iteration, then we actually convert the
            --  comparison into True or False, if the result is certain.

            if AV = False then
               if True_Result or False_Result then
                  Result := Boolean_Literals (True_Result);
                  Rewrite (N,
                    Convert_To (Typ,
                      New_Occurrence_Of (Result, Sloc (N))));
                  Analyze_And_Resolve (N, Typ);
                  Warn_On_Known_Condition (N);
                  return;
               end if;

            --  If this is the second iteration (AV = True), and the original
            --  node comes from source and we are not in an instance, then give
            --  a warning if we know result would be True or False. Note: we
            --  know Constant_Condition_Warnings is set if we get here.

            elsif Comes_From_Source (Original_Node (N))
              and then not In_Instance
            then
               if True_Result then
                  Error_Msg_N
                    ("condition can only be False if invalid values present??",
                     N);
               elsif False_Result then
                  Error_Msg_N
                    ("condition can only be True if invalid values present??",
                     N);
               end if;
            end if;
         end;

         --  Skip second iteration if not warning on constant conditions or
         --  if the first iteration already generated a warning of some kind or
         --  if we are in any case assuming all values are valid (so that the
         --  first iteration took care of the valid case).

         exit when not Constant_Condition_Warnings;
         exit when Warning_Generated;
         exit when Assume_No_Invalid_Values;
      end loop;
   end Rewrite_Comparison;

   ----------------------------
   -- Safe_In_Place_Array_Op --
   ----------------------------

   function Safe_In_Place_Array_Op
     (Lhs : Node_Id;
      Op1 : Node_Id;
      Op2 : Node_Id) return Boolean
   is
      Target : Entity_Id;

      function Is_Safe_Operand (Op : Node_Id) return Boolean;
      --  Operand is safe if it cannot overlap part of the target of the
      --  operation. If the operand and the target are identical, the operand
      --  is safe. The operand can be empty in the case of negation.

      function Is_Unaliased (N : Node_Id) return Boolean;
      --  Check that N is a stand-alone entity

      ------------------
      -- Is_Unaliased --
      ------------------

      function Is_Unaliased (N : Node_Id) return Boolean is
      begin
         return
           Is_Entity_Name (N)
             and then No (Address_Clause (Entity (N)))
             and then No (Renamed_Object (Entity (N)));
      end Is_Unaliased;

      ---------------------
      -- Is_Safe_Operand --
      ---------------------

      function Is_Safe_Operand (Op : Node_Id) return Boolean is
      begin
         if No (Op) then
            return True;

         elsif Is_Entity_Name (Op) then
            return Is_Unaliased (Op);

         elsif Nkind_In (Op, N_Indexed_Component, N_Selected_Component) then
            return Is_Unaliased (Prefix (Op));

         elsif Nkind (Op) = N_Slice then
            return
              Is_Unaliased (Prefix (Op))
                and then Entity (Prefix (Op)) /= Target;

         elsif Nkind (Op) = N_Op_Not then
            return Is_Safe_Operand (Right_Opnd (Op));

         else
            return False;
         end if;
      end Is_Safe_Operand;

   --  Start of processing for Safe_In_Place_Array_Op

   begin
      --  Skip this processing if the component size is different from system
      --  storage unit (since at least for NOT this would cause problems).

      if Component_Size (Etype (Lhs)) /= System_Storage_Unit then
         return False;

      --  Cannot do in place stuff on VM_Target since cannot pass addresses

      elsif VM_Target /= No_VM then
         return False;

      --  Cannot do in place stuff if non-standard Boolean representation

      elsif Has_Non_Standard_Rep (Component_Type (Etype (Lhs))) then
         return False;

      elsif not Is_Unaliased (Lhs) then
         return False;

      else
         Target := Entity (Lhs);
         return Is_Safe_Operand (Op1) and then Is_Safe_Operand (Op2);
      end if;
   end Safe_In_Place_Array_Op;

   -----------------------
   -- Tagged_Membership --
   -----------------------

   --  There are two different cases to consider depending on whether the right
   --  operand is a class-wide type or not. If not we just compare the actual
   --  tag of the left expr to the target type tag:
   --
   --     Left_Expr.Tag = Right_Type'Tag;
   --
   --  If it is a class-wide type we use the RT function CW_Membership which is
   --  usually implemented by looking in the ancestor tables contained in the
   --  dispatch table pointed by Left_Expr.Tag for Typ'Tag

   --  Ada 2005 (AI-251): If it is a class-wide interface type we use the RT
   --  function IW_Membership which is usually implemented by looking in the
   --  table of abstract interface types plus the ancestor table contained in
   --  the dispatch table pointed by Left_Expr.Tag for Typ'Tag

   procedure Tagged_Membership
     (N         : Node_Id;
      SCIL_Node : out Node_Id;
      Result    : out Node_Id)
   is
      Left  : constant Node_Id    := Left_Opnd  (N);
      Right : constant Node_Id    := Right_Opnd (N);
      Loc   : constant Source_Ptr := Sloc (N);

      Full_R_Typ : Entity_Id;
      Left_Type  : Entity_Id;
      New_Node   : Node_Id;
      Right_Type : Entity_Id;
      Obj_Tag    : Node_Id;

   begin
      SCIL_Node := Empty;

      --  Handle entities from the limited view

      Left_Type  := Available_View (Etype (Left));
      Right_Type := Available_View (Etype (Right));

      --  In the case where the type is an access type, the test is applied
      --  using the designated types (needed in Ada 2012 for implicit anonymous
      --  access conversions, for AI05-0149).

      if Is_Access_Type (Right_Type) then
         Left_Type  := Designated_Type (Left_Type);
         Right_Type := Designated_Type (Right_Type);
      end if;

      if Is_Class_Wide_Type (Left_Type) then
         Left_Type := Root_Type (Left_Type);
      end if;

      if Is_Class_Wide_Type (Right_Type) then
         Full_R_Typ := Underlying_Type (Root_Type (Right_Type));
      else
         Full_R_Typ := Underlying_Type (Right_Type);
      end if;

      Obj_Tag :=
        Make_Selected_Component (Loc,
          Prefix        => Relocate_Node (Left),
          Selector_Name =>
            New_Occurrence_Of (First_Tag_Component (Left_Type), Loc));

      if Is_Class_Wide_Type (Right_Type) then

         --  No need to issue a run-time check if we statically know that the
         --  result of this membership test is always true. For example,
         --  considering the following declarations:

         --    type Iface is interface;
         --    type T     is tagged null record;
         --    type DT    is new T and Iface with null record;

         --    Obj1 : T;
         --    Obj2 : DT;

         --  These membership tests are always true:

         --    Obj1 in T'Class
         --    Obj2 in T'Class;
         --    Obj2 in Iface'Class;

         --  We do not need to handle cases where the membership is illegal.
         --  For example:

         --    Obj1 in DT'Class;     --  Compile time error
         --    Obj1 in Iface'Class;  --  Compile time error

         if not Is_Class_Wide_Type (Left_Type)
           and then (Is_Ancestor (Etype (Right_Type), Left_Type,
                                  Use_Full_View => True)
                      or else (Is_Interface (Etype (Right_Type))
                                and then Interface_Present_In_Ancestor
                                           (Typ   => Left_Type,
                                            Iface => Etype (Right_Type))))
         then
            Result := New_Occurrence_Of (Standard_True, Loc);
            return;
         end if;

         --  Ada 2005 (AI-251): Class-wide applied to interfaces

         if Is_Interface (Etype (Class_Wide_Type (Right_Type)))

            --   Support to: "Iface_CW_Typ in Typ'Class"

           or else Is_Interface (Left_Type)
         then
            --  Issue error if IW_Membership operation not available in a
            --  configurable run time setting.

            if not RTE_Available (RE_IW_Membership) then
               Error_Msg_CRT
                 ("dynamic membership test on interface types", N);
               Result := Empty;
               return;
            end if;

            Result :=
              Make_Function_Call (Loc,
                 Name => New_Occurrence_Of (RTE (RE_IW_Membership), Loc),
                 Parameter_Associations => New_List (
                   Make_Attribute_Reference (Loc,
                     Prefix => Obj_Tag,
                     Attribute_Name => Name_Address),
                   New_Occurrence_Of (
                     Node (First_Elmt (Access_Disp_Table (Full_R_Typ))),
                     Loc)));

         --  Ada 95: Normal case

         else
            Build_CW_Membership (Loc,
              Obj_Tag_Node => Obj_Tag,
              Typ_Tag_Node =>
                 New_Occurrence_Of (
                   Node (First_Elmt (Access_Disp_Table (Full_R_Typ))),  Loc),
              Related_Nod => N,
              New_Node    => New_Node);

            --  Generate the SCIL node for this class-wide membership test.
            --  Done here because the previous call to Build_CW_Membership
            --  relocates Obj_Tag.

            if Generate_SCIL then
               SCIL_Node := Make_SCIL_Membership_Test (Sloc (N));
               Set_SCIL_Entity (SCIL_Node, Etype (Right_Type));
               Set_SCIL_Tag_Value (SCIL_Node, Obj_Tag);
            end if;

            Result := New_Node;
         end if;

      --  Right_Type is not a class-wide type

      else
         --  No need to check the tag of the object if Right_Typ is abstract

         if Is_Abstract_Type (Right_Type) then
            Result := New_Occurrence_Of (Standard_False, Loc);

         else
            Result :=
              Make_Op_Eq (Loc,
                Left_Opnd  => Obj_Tag,
                Right_Opnd =>
                  New_Occurrence_Of
                    (Node (First_Elmt (Access_Disp_Table (Full_R_Typ))), Loc));
         end if;
      end if;
   end Tagged_Membership;

   ------------------------------
   -- Unary_Op_Validity_Checks --
   ------------------------------

   procedure Unary_Op_Validity_Checks (N : Node_Id) is
   begin
      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Right_Opnd (N));
      end if;
   end Unary_Op_Validity_Checks;

end Exp_Ch4;
