------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
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

with Accessibility;  use Accessibility;
with Aspects;        use Aspects;
with Atree;          use Atree;
with Checks;         use Checks;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Aggr;       use Exp_Aggr;
with Exp_Ch3;        use Exp_Ch3;
with Exp_Ch6;        use Exp_Ch6;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Ch9;        use Exp_Ch9;
with Exp_Disp;       use Exp_Disp;
with Exp_Fixd;       use Exp_Fixd;
with Exp_Intr;       use Exp_Intr;
with Exp_Pakd;       use Exp_Pakd;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Inline;         use Inline;
with Lib;            use Lib;
with Mutably_Tagged; use Mutably_Tagged;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Par_SCO;        use Par_SCO;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Cat;        use Sem_Cat;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Eval;       use Sem_Eval;
with Sem_Res;        use Sem_Res;
with Sem_Type;       use Sem_Type;
with Sem_Util;       use Sem_Util;
with Sem_Warn;       use Sem_Warn;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with SCIL_LL;        use SCIL_LL;
with Targparm;       use Targparm;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;
with Urealp;         use Urealp;
with Validsw;        use Validsw;
with Warnsw;         use Warnsw;

package body Exp_Ch4 is

   Too_Large_Length_For_Array : constant Unat := Uint_256;
   --  Threshold from which we do not try to create static array temporaries in
   --  order to eliminate dynamic stack allocations.

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

   procedure Displace_Allocator_Pointer (N : Node_Id);
   --  Ada 2005 (AI-251): Subsidiary procedure to Expand_N_Allocator and
   --  Expand_Allocator_Expression. Allocating class-wide interface objects
   --  this routine displaces the pointer to the allocated object to reference
   --  the component referencing the corresponding secondary dispatch table.

   procedure Expand_Allocator_Expression (N : Node_Id);
   --  Subsidiary to Expand_N_Allocator, for the case when the expression
   --  is a qualified expression.

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

   procedure Expand_Nonbinary_Modular_Op (N : Node_Id);
   --  When generating C code or if restriction No_Implicit_Conditionals is in
   --  effect, convert most nonbinary modular arithmetic operations into code
   --  that relies on the expansion of an explicit Mod operator. No expansion
   --  is performed if N is not a nonbinary modular operation.

   procedure Expand_Short_Circuit_Operator (N : Node_Id);
   --  Common expansion processing for short-circuit boolean operators

   procedure Expand_Compare_Minimize_Eliminate_Overflow (N : Node_Id);
   --  Deal with comparison in MINIMIZED/ELIMINATED overflow mode. This is
   --  where we allow comparison of "out of range" values.

   function Expand_Composite_Equality
     (Outer_Type : Entity_Id;
      Nod        : Node_Id;
      Comp_Type  : Entity_Id;
      Lhs        : Node_Id;
      Rhs        : Node_Id) return Node_Id;
   --  Local recursive function used to expand equality for nested composite
   --  types. Used by Expand_Record/Array_Equality. Nod provides the Sloc value
   --  for generated code. Lhs and Rhs are the left and right sides for the
   --  comparison, and Comp_Typ is the type of the objects to compare.
   --  Outer_Type is the composite type containing a component of type
   --  Comp_Type -- used for printing messages.

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

   procedure Get_First_Index_Bounds (T : Entity_Id; Lo, Hi : out Uint);
   --  T is an array whose index bounds are all known at compile time. Return
   --  the value of the low and high bounds of the first index of T.

   function Get_Size_For_Range (Lo, Hi : Uint) return Uint;
   --  Return the size of a small signed integer type covering Lo .. Hi, the
   --  main goal being to return a size lower than that of standard types.

   procedure Insert_Conditional_Object_Declaration
     (Obj_Id : Entity_Id;
      Expr   : Node_Id;
      Decl   : Node_Id);
   --  Expr is the dependent expression of a conditional expression and Decl
   --  is the declaration of an object whose initialization expression is the
   --  conditional expression. Insert in the actions of Expr the declaration
   --  of Obj_Id modeled on Decl and with Expr as initialization expression.

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

   procedure Narrow_Large_Operation (N : Node_Id);
   --  Try to compute the result of a large operation in a narrower type than
   --  its nominal type. This is mainly aimed at getting rid of operations done
   --  in Universal_Integer that can be generated for attributes.

   procedure Optimize_Length_Comparison (N : Node_Id);
   --  Given an expression, if it is of the form X'Length op N (or the other
   --  way round), where N is known at compile time to be 0 or 1, or something
   --  else where the value is known to be nonnegative and in the 32-bit range,
   --  and X is a simple entity, and op is a comparison operator, optimizes it
   --  into a comparison of X'First and X'Last.

   procedure Process_Transients_In_Expression
     (Expr  : Node_Id;
      Stmts : List_Id);
   --  Subsidiary routine to the expansion of expression_with_actions, if and
   --  case expressions. Inspect and process actions list Stmts of expression
   --  Expr for transient objects. If such objects are found, the routine will
   --  generate code to finalize them when the enclosing context is elaborated
   --  or evaluated.

   --  This specific processing is required for these expressions because the
   --  management of transient objects for expressions implemented in Exp_Ch7
   --  cannot deal with nested lists of actions whose effects may outlive the
   --  lists and affect the result of the parent expressions. In these cases,
   --  the lifetime of temporaries created in these lists must be extended to
   --  match that of the enclosing context of the parent expressions and, in
   --  particular, their finalization must be deferred to this context.

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

   ---------------------------------
   -- Build_Cleanup_For_Allocator --
   ---------------------------------

   function Build_Cleanup_For_Allocator
     (Loc     : Source_Ptr;
      Obj_Id  : Entity_Id;
      Pool    : Entity_Id;
      Actions : List_Id) return Node_Id
   is
      Free_Stmt : constant Node_Id :=
        Make_Free_Statement (Loc, New_Occurrence_Of (Obj_Id, Loc));

   begin
      Set_For_Allocator (Free_Stmt);
      Set_Storage_Pool  (Free_Stmt, Pool);

      return
        Make_Block_Statement (Loc,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements         => Actions,
              Exception_Handlers => New_List (
                Make_Exception_Handler (Loc,
                  Exception_Choices => New_List (
                    Make_Others_Choice (Loc)),
                  Statements        => New_List (
                    Free_Stmt,
                    Make_Raise_Statement (Loc))))));
   end Build_Cleanup_For_Allocator;

   -----------------------
   -- Build_Eq_Call --
   -----------------------

   function Build_Eq_Call
     (Typ : Entity_Id;
      Loc : Source_Ptr;
      Lhs : Node_Id;
      Rhs : Node_Id) return Node_Id
   is
      Eq : constant Entity_Id := Get_User_Defined_Equality (Typ);

   begin
      if Present (Eq) then
         if Is_Abstract_Subprogram (Eq) then
            return Result : constant Node_Id :=
              Make_Raise_Program_Error (Loc, Reason =>  PE_Explicit_Raise)
            do
               Set_Etype (Result, Etype (Eq));
            end return;

         else
            return
              Make_Function_Call (Loc,
                Name                   => New_Occurrence_Of (Eq, Loc),
                Parameter_Associations => New_List (Lhs, Rhs));
         end if;
      end if;

      --  If not found, predefined operation will be used

      return Empty;
   end Build_Eq_Call;

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
      Loc            : constant Source_Ptr := Sloc (N);
      Exp            : constant Node_Id    := Unqualify (Expression (N));
      Indic          : constant Node_Id    := Subtype_Mark (Expression (N));
      T              : constant Entity_Id  := Entity (Indic);
      PtrT           : constant Entity_Id  := Etype (N);
      DesigT         : constant Entity_Id  := Designated_Type (PtrT);
      Pool           : constant Node_Id    := Storage_Pool (N);
      Special_Return : constant Boolean    := For_Special_Return_Object (N);
      Special_Pool   : constant Boolean    :=
        Present (Pool)
          and then
            (Is_RTE (Pool, RE_RS_Pool) or else Is_RTE (Pool, RE_SS_Pool));
      Static_Match   : constant Boolean    :=
        not Is_Constrained (DesigT)
          or else Subtypes_Statically_Match (T, DesigT);

      procedure Build_Aggregate_In_Place (Temp : Entity_Id; Typ : Entity_Id);
      --  If Exp is an aggregate to build in place, build the declaration of
      --  object Temp with Typ and initialization expression an uninitialized
      --  allocator for Etype (Exp), then perform in-place aggregate assignment
      --  of Exp into the newly allocated memory.

      procedure Build_Explicit_Assignment (Temp : Entity_Id; Typ : Entity_Id);
      --  Build the declaration of object Temp with Typ and initialization
      --  expression an uninitialized allocator for Etype (Exp), then perform
      --  assignment of Exp into the newly allocated memory.

      procedure Build_Simple_Allocation (Temp : Entity_Id; Typ : Entity_Id);
      --  Build the declaration of object Temp with Typ and initialization
      --  expression the allocator N.

      function Needs_Cleanup return Boolean is
        (not Special_Pool
          and then Is_Definite_Subtype (T)
          and then Nkind (Exp) = N_Function_Call
          and then not (Is_Entity_Name (Name (Exp))
                         and then No_Raise (Entity (Name (Exp))))
          and then not Restriction_Active (No_Exception_Propagation)
          and then RTE_Available (RE_Free)
          and then not Debug_Flag_QQ);
      --  Return True if a cleanup needs to be built to deallocate the memory
      --  when the evaluation of the expression raises an exception. This can
      --  be done only if deallocation is available, but not for special pools
      --  since such pools do not support deallocation. Moreover, this is not
      --  needed for an indefinite allocation because the expression will be
      --  evaluated first, in order to size the allocation. For now, we only
      --  return True for a call to a function that may raise an exception.

      ------------------------------
      -- Build_Aggregate_In_Place --
      ------------------------------

      procedure Build_Aggregate_In_Place (Temp : Entity_Id; Typ : Entity_Id) is
         Temp_Decl : constant Node_Id :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Typ, Loc),
             Expression          =>
               Make_Allocator (Loc,
                 Expression => New_Occurrence_Of (Etype (Exp), Loc)));

      begin
         --  Prevent default initialization of the allocator

         Set_No_Initialization (Expression (Temp_Decl));

         --  Copy the Comes_From_Source flag onto the allocator since logically
         --  this allocator is a replacement of the original allocator. This is
         --  for proper handling of restriction No_Implicit_Heap_Allocations.

         Preserve_Comes_From_Source (Expression (Temp_Decl), N);

         --  Insert the declaration and generate the in-place assignment

         Insert_Action (N, Temp_Decl);
         Convert_Aggr_In_Allocator (N, Temp);
      end Build_Aggregate_In_Place;

      -------------------------------
      -- Build_Explicit_Assignment --
      -------------------------------

      procedure Build_Explicit_Assignment (Temp : Entity_Id; Typ : Entity_Id)
      is
         Assign : constant Node_Id :=
           Make_Assignment_Statement (Loc,
             Name       =>
               Make_Explicit_Dereference (Loc,
                 New_Occurrence_Of (Temp, Loc)),
             Expression => Relocate_Node (Exp));

         Temp_Decl : constant Node_Id :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Typ, Loc),
             Expression          =>
               Make_Allocator (Loc,
                 Expression => New_Occurrence_Of (Etype (Exp), Loc)));

      begin
         --  Prevent default initialization of the allocator

         Set_No_Initialization (Expression (Temp_Decl));

         --  Copy the Comes_From_Source flag onto the allocator since logically
         --  this allocator is a replacement of the original allocator. This is
         --  for proper handling of restriction No_Implicit_Heap_Allocations.

         Preserve_Comes_From_Source (Expression (Temp_Decl), N);

         --  Insert the declaration

         Insert_Action (N, Temp_Decl);

         --  Arrange for the expression to be analyzed again and expanded

         if Is_Delayed_Conditional_Expression (Expression (Assign)) then
            Unanalyze_Delayed_Conditional_Expression (Expression (Assign));
         end if;

         Set_Assignment_OK (Name (Assign));

         --  If the initialization expression is a function call, we do not
         --  adjust after the assignment but, in either case, we do not
         --  finalize before since the target is newly allocated memory.

         if Nkind (Exp) = N_Function_Call then
            Set_No_Ctrl_Actions (Assign);
         else
            Set_No_Finalize_Actions (Assign);
         end if;

         --  Build a cleanup if the assignment may raise an exception

         if Needs_Cleanup then
            Insert_Action (N,
              Build_Cleanup_For_Allocator (Loc,
                Temp, Pool, New_List (Assign)),
              Suppress => All_Checks);
         else
            Insert_Action (N, Assign, Suppress => All_Checks);
         end if;
      end Build_Explicit_Assignment;

      -----------------------------
      -- Build_Simple_Allocation --
      -----------------------------

      procedure Build_Simple_Allocation (Temp : Entity_Id; Typ : Entity_Id) is
         New_N     : constant Node_Id := Relocate_Node (N);
         Temp_Decl : constant Node_Id :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Typ, Loc),
             Expression          => New_N);

      begin
         --  Avoid recursion in the mechanism

         Set_Analyzed (New_N);

         Insert_Action (N, Temp_Decl);
      end Build_Simple_Allocation;

      --  Local variables

      Aggr_In_Place     : Boolean;
      Container_Aggr    : Boolean;
      Delayed_Cond_Expr : Boolean;

      TagT : Entity_Id := Empty;
      --  Type used as source for tag assignment

      TagR : Node_Id := Empty;
      --  Target reference for tag assignment

      Temp : Entity_Id;
      --  Temporary used to hold the result of the allocator

   --  Start of processing for Expand_Allocator_Expression

   begin
      --  Handle call to C++ constructor

      if Is_CPP_Constructor_Call (Exp) then
         Make_CPP_Constructor_Call_In_Allocator
           (Allocator => N,
            Function_Call => Exp);
         return;
      end if;

      --  If we have:
      --    type A is access T1;
      --    X : A := new T2'(...);
      --  T1 and T2 can be different subtypes, and we might need to check
      --  both constraints. First check against the type of the qualified
      --  expression.

      --  Note that we delay applying predicate checks, because this may
      --  cause the creation of a temporary, which is illegal for limited
      --  types and just inefficient in the other cases.

      Apply_Constraint_Check (Exp, T, No_Sliding => True);

      if Do_Range_Check (Exp) then
         Generate_Range_Check (Exp, T, CE_Range_Check_Failed);
      end if;

      --  A check is also needed in cases where the designated subtype is
      --  constrained and differs from the subtype given in the qualified
      --  expression. Note that the check on the qualified expression does
      --  not allow sliding, but this check does (a relaxation from Ada 83).

      if not Static_Match then
         Apply_Constraint_Check (Exp, DesigT, No_Sliding => False);

         if Do_Range_Check (Exp) then
            Generate_Range_Check (Exp, DesigT, CE_Range_Check_Failed);
         end if;
      end if;

      --  Propagate Constraint_Error and return

      if Nkind (Exp) = N_Raise_Constraint_Error then
         Rewrite (N, New_Copy (Exp));
         Set_Etype (N, PtrT);
         return;
      end if;

      --  Check that any anonymous access discriminants are suitable
      --  for use in an allocator.

      --  Note: This check is performed here instead of during analysis
      --  so that we can check against the fully resolved Etype of Exp.

      if Is_Entity_Name (Exp)
        and then Has_Anonymous_Access_Discriminant (Etype (Exp))
        and then Static_Accessibility_Level (Exp, Object_Decl_Level)
                   > Static_Accessibility_Level (N, Object_Decl_Level)
      then
         --  A dynamic check and a warning are generated when we are within
         --  an instance.

         if In_Instance then
            Insert_Action (N,
              Make_Raise_Program_Error (Loc,
                Reason => PE_Accessibility_Check_Failed));

            Error_Msg_Warn := SPARK_Mode /= On;
            Error_Msg_N ("anonymous access discriminant is too deep for use"
                         & " in allocator<<", N);
            Error_Msg_N ("\Program_Error [<<", N);

         --  Otherwise, make the error static

         else
            Error_Msg_N ("anonymous access discriminant is too deep for use"
                          & " in allocator", N);
         end if;
      end if;

      Aggr_In_Place     := Is_Delayed_Aggregate (Exp);
      Delayed_Cond_Expr := Is_Delayed_Conditional_Expression (Exp);
      Container_Aggr    := Nkind (Exp) = N_Aggregate
                             and then Has_Aspect (T, Aspect_Aggregate);

      --  An allocator with a container aggregate as qualified expression must
      --  be rewritten into the form expected by Expand_Container_Aggregate.

      if Container_Aggr then
         Temp := Make_Temporary (Loc, 'P', N);
         Set_Analyzed (Exp, False);
         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => New_Occurrence_Of (PtrT, Loc),
             Expression          => Relocate_Node (N)));

      --  Case of tagged type or type requiring finalization

      elsif Is_Tagged_Type (T) or else Needs_Finalization (T) then

         --  Ada 2005 (AI-318-02): If the initialization expression is a call
         --  to a build-in-place function, then access to the allocated object
         --  must be passed to the function.

         if Is_Build_In_Place_Function_Call (Exp) then
            Make_Build_In_Place_Call_In_Allocator (N, Exp);
            Apply_Accessibility_Check_For_Allocator
              (N, Exp, N, Built_In_Place => True);
            return;

         --  Ada 2005 (AI-318-02): Specialization of the previous case for
         --  expressions containing a build-in-place function call whose
         --  returned object covers interface types, and Expr has calls to
         --  Ada.Tags.Displace to displace the pointer to the returned build-
         --  in-place object to reference the secondary dispatch table of a
         --  covered interface type.

         elsif Present (Unqual_BIP_Iface_Function_Call (Exp)) then
            Make_Build_In_Place_Iface_Call_In_Allocator (N, Exp);
            Apply_Accessibility_Check_For_Allocator
              (N, Exp, N, Built_In_Place => True);
            return;
         end if;

         --  For a class wide allocation generate the following code:

         --    type Equiv_Record is record ... end record;
         --    implicit subtype CW is <Class_Wide_Subytpe>;
         --    Temp : PtrT := new CW'(CW!(expr));

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

         --  If the designated type is class-wide, then the alignment and the
         --  controlled nature of the expression are computed dynamically by
         --  the code generated by Build_Allocate_Deallocate_Proc, which will
         --  thus need to remove side effects from Exp first. But the below
         --  test on Exp needs to have its final form to decide whether or not
         --  to generate an Adjust call, so we preventively remove them here.

         if Is_Class_Wide_Type (DesigT)
           and then Nkind (Exp) = N_Function_Call
           and then not Special_Pool
         then
            Remove_Side_Effects (Exp);
         end if;

         --  Actions inserted before:
         --    Temp : constant PtrT := new T'(Expression);
         --    Temp._tag = T'tag;  --  when not class-wide
         --    [Deep_]Adjust (Temp.all);

         --  We analyze by hand the new internal allocator to avoid any
         --  recursion and inappropriate call to Initialize.

         Temp := Make_Temporary (Loc, 'P', N);

         --  Processing for allocators returning non-interface types

         if not Is_Interface (DesigT) then
            if Aggr_In_Place then
               Build_Aggregate_In_Place (Temp, PtrT);

            elsif Delayed_Cond_Expr or else Needs_Cleanup then
               Build_Explicit_Assignment (Temp, PtrT);

            else
               Build_Simple_Allocation (Temp, PtrT);
            end if;

         --  Ada 2005 (AI-251): Handle allocators whose designated type is an
         --  interface type. In this case we use the type of the qualified
         --  expression to allocate the object.

         else
            declare
               Def_Id   : constant Entity_Id := Make_Temporary (Loc, 'T', N);
               New_Temp : constant Entity_Id := Make_Temporary (Loc, 'P', N);

            begin
               Insert_Action (N,
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Def_Id,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       All_Present        => True,
                       Constant_Present   =>
                         Is_Access_Constant (PtrT),
                       Subtype_Indication =>
                         New_Occurrence_Of (Etype (Exp), Loc))));

               --  Inherit the allocation-related attributes from the original
               --  access type.

               Set_Finalization_Collection
                 (Def_Id, Finalization_Collection (PtrT));

               Set_Associated_Storage_Pool
                 (Def_Id, Associated_Storage_Pool (PtrT));

               --  Declare the object using the previous type declaration

               if Aggr_In_Place then
                  Build_Aggregate_In_Place (New_Temp, Def_Id);

               elsif Delayed_Cond_Expr or else Needs_Cleanup then
                  Build_Explicit_Assignment (New_Temp, Def_Id);

               else
                  Build_Simple_Allocation (New_Temp, Def_Id);
               end if;

               --  Generate an additional object containing the address of the
               --  returned object. The type of this second object declaration
               --  is the correct type required for the common processing that
               --  is still performed by this subprogram. The displacement of
               --  this pointer to reference the component associated with the
               --  interface type will be done at the end of common processing.

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Occurrence_Of (PtrT, Loc),
                   Expression          =>
                     Unchecked_Convert_To (PtrT,
                       New_Occurrence_Of (New_Temp, Loc))));
            end;
         end if;

         --  Generate the tag assignment

         --  Suppress the tag assignment for VM targets because VM tags are
         --  represented implicitly in objects.

         if not Tagged_Type_Expansion then
            null;

         --  Ada 2005 (AI-251): Suppress the tag assignment with class-wide
         --  interface objects because in this case the tag does not change.

         elsif Is_Interface (DesigT) then
            pragma Assert (Is_Class_Wide_Type (DesigT));
            null;

         --  Likewise if the allocator is made for a special return object

         elsif Special_Return then
            null;

         elsif Is_Tagged_Type (T) and then not Is_Class_Wide_Type (T) then
            TagT := T;
            TagR :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Temp, Loc));

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
            Insert_Action (N,
              Make_Tag_Assignment_From_Type
                (Loc, TagR, Underlying_Type (TagT)));
         end if;

         --  Generate an Adjust call if the object will be moved. In Ada 2005,
         --  the object may be inherently limited, in which case there is no
         --  Adjust procedure, and the object is built in place. In Ada 95, the
         --  object can be limited but not inherently limited if this allocator
         --  came from a return statement (we're allocating the result on the
         --  secondary stack); in that case, the object will be moved, so we do
         --  want to Adjust. But the call is always skipped if the allocator is
         --  made for a special return object because it's generated elsewhere.

         --  Needs_Finalization (DesigT) may differ from Needs_Finalization (T)
         --  if one of the two types is class-wide, and the other is not.

         if Needs_Finalization (DesigT)
           and then Needs_Finalization (T)
           and then not Is_Inherently_Limited_Type (T)
           and then not Aggr_In_Place
           and then not Delayed_Cond_Expr
           and then Nkind (Exp) /= N_Function_Call
           and then not Special_Return
         then
            declare
               Adj_Call : constant Node_Id :=
                 Make_Adjust_Call
                   (Obj_Ref =>
                      Unchecked_Convert_To (T,
                        Make_Explicit_Dereference (Loc,
                          Prefix => New_Occurrence_Of (Temp, Loc))),
                    Typ     => T);
               --  An unchecked conversion is needed in the CW case because
               --  the designated type can be an ancestor of the subtype mark
               --  of the allocator.

            begin
               if Present (Adj_Call) then
                  Insert_Action (N, Adj_Call);
               end if;
            end;
         end if;

         --  This needs to done before generating the accessibility check below
         --  because the check comes with cleanup code that invokes Free on the
         --  temporary and, therefore, expects the object to be attached to its
         --  finalization collection if it is controlled.

         Build_Allocate_Deallocate_Proc (Declaration_Node (Temp), Mark => N);

         --  Note: the accessibility check must be inserted after the call to
         --  [Deep_]Adjust to ensure proper completion of the assignment.

         Apply_Accessibility_Check_For_Allocator (N, Exp, Temp);

      --  Case of aggregate built in place

      elsif Aggr_In_Place then
         Temp := Make_Temporary (Loc, 'P', N);
         Build_Aggregate_In_Place (Temp, PtrT);
         Build_Allocate_Deallocate_Proc (Declaration_Node (Temp), Mark => N);

      --  If the initialization expression is a conditional expression whose
      --  expansion has been delayed, assign it explicitly to the allocator,
      --  but only after analyzing it again and expanding it.

      elsif Delayed_Cond_Expr then
         Temp := Make_Temporary (Loc, 'P', N);
         Build_Explicit_Assignment (Temp, PtrT);
         Build_Allocate_Deallocate_Proc (Declaration_Node (Temp), Mark => N);

      --  Default case

      else
         --  Ada 2005 (AI-318-02): If the initialization expression is a call
         --  to a build-in-place function, then access to the allocated object
         --  must be passed to the function.

         if Is_Build_In_Place_Function_Call (Exp) then
            Make_Build_In_Place_Call_In_Allocator (N, Exp);
            return;
         end if;

         if Is_Access_Type (T) and then Can_Never_Be_Null (T) then
            Install_Null_Excluding_Check (Exp);
         end if;

         if Is_Access_Type (DesigT)
           and then Nkind (Exp) = N_Allocator
           and then Nkind (Expression (Exp)) /= N_Qualified_Expression
         then
            --  Apply constraint to designated subtype indication

            Apply_Constraint_Check
              (Expression (Exp), Designated_Type (DesigT), No_Sliding => True);

            --  Propagate Constraint_Error to enclosing allocator

            if Nkind (Expression (Exp)) = N_Raise_Constraint_Error then
               Rewrite (Exp, New_Copy (Expression (Exp)));
            end if;
         end if;

         --  For an access-to-unconstrained-packed-array type, build an
         --  expression with a constrained subtype in order for the code
         --  generator to compute the proper size for the allocator.

         if Is_Packed_Array (T) and then not Is_Constrained (T) then
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

         --  ??? If the allocator is present inside a record type, then the
         --  actions are attached to the current scope, to be inserted and
         --  analyzed on exit from the scope, so we cannot do any rewriting.

         if Is_Record_Type (Current_Scope)
           and then not Is_Frozen (Current_Scope)
         then
            Build_Allocate_Deallocate_Proc (N);
            return;
         end if;

         Temp := Make_Temporary (Loc, 'P', N);
         if Needs_Cleanup then
            Build_Explicit_Assignment (Temp, PtrT);
         else
            Build_Simple_Allocation (Temp, PtrT);
         end if;
         Build_Allocate_Deallocate_Proc (Declaration_Node (Temp), Mark => N);
      end if;

      Rewrite (N, New_Occurrence_Of (Temp, Loc));
      Preserve_Comes_From_Source (N, Original_Node (N));
      Analyze_And_Resolve (N, PtrT);

      Apply_Predicate_Check (N, T, Deref => True);
      if not Static_Match then
         Apply_Predicate_Check (N, DesigT, Deref => True);
      end if;

      --  Ada 2005 (AI-251): Displace the pointer to reference the record
      --  component containing the secondary dispatch table of the interface
      --  type.

      if Is_Interface (DesigT) then
         Displace_Allocator_Pointer (N);
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

         elsif Compile_Time_Known_Bounds (Otyp) then
            declare
               Lo, Hi : Uint;

            begin
               Get_First_Index_Bounds (Otyp, Lo, Hi);
               return Hi < Lo + 3;
            end;

         else
            return False;
         end if;
      end Length_Less_Than_4;

   --  Start of processing for Expand_Array_Comparison

   begin
      --  Deal first with unpacked case, where we can call a runtime routine,
      --  except if the component type is a byte (unsigned) where we can use
      --  a byte-wise comparison if supported on the target (this is disabled
      --  for now in Unnest_Subprogram_Mode for LLVM).

      if not Is_Bit_Packed_Array (Typ1) then
         --  The call we generate is:

         --  Compare_Array_xn[_Unaligned]
         --    (left'address, right'address, left'length, right'length) <op> 0

         --  x = U for unsigned, S for signed
         --  n = 8,16,32,64,128 for component size
         --  Add _Unaligned if length < 4 and component size is 8.
         --  <op> is the standard comparison operator

         if Component_Size (Typ1) = 8 then
            if Is_Unsigned_Type (Ctyp)
              and then not Is_Possibly_Unaligned_Object (Op1)
              and then not Is_Possibly_Unaligned_Slice (Op1)
              and then not Is_Possibly_Unaligned_Object (Op2)
              and then not Is_Possibly_Unaligned_Slice (Op2)
              and then Support_Composite_Compare_On_Target
              and then not Unnest_Subprogram_Mode
            then
               return;

            elsif Length_Less_Than_4 (Op1)
              or else Length_Less_Than_4 (Op2)
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

         elsif Component_Size (Typ1) = 64 then
            if Is_Unsigned_Type (Ctyp) then
               Comp := RE_Compare_Array_U64;
            else
               Comp := RE_Compare_Array_S64;
            end if;

         else pragma Assert (Component_Size (Typ1) = 128);
            if Is_Unsigned_Type (Ctyp) then
               Comp := RE_Compare_Array_U128;
            else
               Comp := RE_Compare_Array_S128;
            end if;
         end if;

         --  Expand to a call only if the runtime function is available,
         --  otherwise fall back to inline code.

         if RTE_Available (Comp) then
            Remove_Side_Effects (Op1, Name_Req => True);
            Remove_Side_Effects (Op2, Name_Req => True);

            declare
               Comp_Call : constant Node_Id :=
                 Make_Function_Call (Loc,
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
                       Attribute_Name => Name_Length)));

               Zero : constant Node_Id :=
                 Make_Integer_Literal (Loc, Intval => Uint_0);

               Comp_Op : Node_Id;

            begin
               case Nkind (N) is
                  when N_Op_Lt =>
                     Comp_Op := Make_Op_Lt (Loc, Comp_Call, Zero);
                  when N_Op_Le =>
                     Comp_Op := Make_Op_Le (Loc, Comp_Call, Zero);
                  when N_Op_Gt =>
                     Comp_Op := Make_Op_Gt (Loc, Comp_Call, Zero);
                  when N_Op_Ge =>
                     Comp_Op := Make_Op_Ge (Loc, Comp_Call, Zero);
                  when others =>
                     raise Program_Error;
               end case;

               Rewrite (N, Comp_Op);
            end;

            Analyze_And_Resolve (N, Standard_Boolean);
            return;
         end if;
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
   --        return true;    -- RM 4.5.2(22)
   --     end if;

   --     if A'length (1) /= B'length (1)
   --               or else
   --           A'length (2) /= B'length (2)
   --     then
   --        return false;   -- RM 4.5.2(23)
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

      First_Idx : Node_Id;
      Formals   : List_Id;
      Func_Name : Entity_Id;
      Func_Body : Node_Id;

      A : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uA);
      B : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uB);

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;
      --  The parameter types to be used for the formals

      New_Lhs : Node_Id;
      New_Rhs : Node_Id;
      --  The LHS and RHS converted to the parameter types

      function Arr_Attr
        (Arr : Entity_Id;
         Nam : Name_Id;
         Dim : Pos) return Node_Id;
      --  This builds the attribute reference Arr'Nam (Dim)

      function Component_Equality (Typ : Entity_Id) return Node_Id;
      --  Create one statement to compare corresponding components, designated
      --  by a full set of indexes.

      function Get_Arg_Type (N : Node_Id) return Entity_Id;
      --  Given one of the arguments, computes the appropriate type to be used
      --  for that argument in the corresponding function formal

      function Handle_One_Dimension
        (N     : Pos;
         Index : Node_Id) return Node_Id;
      --  This procedure returns the following code
      --
      --    declare
      --       An : Index_T := A'First (N);
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
         Dim : Pos) return Node_Id
      is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Nam,
             Prefix         => New_Occurrence_Of (Arr, Loc),
             Expressions    => New_List (Make_Integer_Literal (Loc, Dim)));
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
           (Outer_Type => Typ, Nod => Nod, Comp_Type => Component_Type (Typ),
            Lhs => L, Rhs => R);

         --  This is necessary to give the warning about Program_Error being
         --  raised when some (sub)component is an unchecked_union.

         Preserve_Comes_From_Source (Test, Nod);

         return
           Make_Implicit_If_Statement (Nod,
             Condition       => Make_Op_Not (Loc, Right_Opnd => Test),
             Then_Statements => New_List (
               Make_Simple_Return_Statement (Loc,
                 Expression => New_Occurrence_Of (Standard_False, Loc))));
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
        (N     : Pos;
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
                      Left_Opnd  => New_Occurrence_Of (An, Loc),
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
         --  iteration scheme on its own.

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
         Alist : Node_Id := Empty;
         Blist : Node_Id := Empty;

      begin
         for J in 1 .. Number_Dimensions (Ltyp) loop
            Evolve_Or_Else (Alist,
              Make_Op_Eq (Loc,
                Left_Opnd  => Arr_Attr (A, Name_Length, J),
                Right_Opnd => Make_Integer_Literal (Loc, Uint_0)));

            Evolve_Or_Else (Blist,
              Make_Op_Eq (Loc,
                Left_Opnd  => Arr_Attr (B, Name_Length, J),
                Right_Opnd => Make_Integer_Literal (Loc, Uint_0)));
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
         Result : Node_Id := Empty;

      begin
         for J in 1 .. Number_Dimensions (Ltyp) loop
            Evolve_Or_Else (Result,
              Make_Op_Ne (Loc,
                Left_Opnd  => Arr_Attr (A, Name_Length, J),
                Right_Opnd => Arr_Attr (B, Name_Length, J)));
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
      end if;

      --  If the array type is distinct from the type of the arguments, it
      --  is the full view of a private type. Apply an unchecked conversion
      --  to ensure that analysis of the code below succeeds.

      if No (Etype (Lhs))
        or else Base_Type (Etype (Lhs)) /= Base_Type (Ltyp)
      then
         New_Lhs := OK_Convert_To (Ltyp, Lhs);
      else
         New_Lhs := Lhs;
      end if;

      if No (Etype (Rhs))
        or else Base_Type (Etype (Rhs)) /= Base_Type (Rtyp)
      then
         New_Rhs := OK_Convert_To (Rtyp, Rhs);
      else
         New_Rhs := Rhs;
      end if;

      pragma Assert (Ltyp = Rtyp);
      First_Idx := First_Index (Ltyp);

      --  If optimization is enabled and the array boils down to a couple of
      --  consecutive elements, generate a simple conjunction of comparisons
      --  which should be easier to optimize by the code generator.

      if Optimization_Level > 0
        and then Is_Constrained (Ltyp)
        and then Number_Dimensions (Ltyp) = 1
        and then Compile_Time_Known_Bounds (Ltyp)
        and then Expr_Value (Type_High_Bound (Etype (First_Idx))) =
                   Expr_Value (Type_Low_Bound (Etype (First_Idx))) + 1
      then
         declare
            Ctyp         : constant Entity_Id := Component_Type (Ltyp);
            Low_B        : constant Node_Id :=
              Type_Low_Bound (Etype (First_Idx));
            High_B       : constant Node_Id :=
              Type_High_Bound (Etype (First_Idx));
            L, R         : Node_Id;
            TestL, TestH : Node_Id;

         begin
            L :=
              Make_Indexed_Component (Loc,
                Prefix      => New_Copy_Tree (New_Lhs),
                Expressions => New_List (New_Copy_Tree (Low_B)));

            R :=
              Make_Indexed_Component (Loc,
                Prefix      => New_Copy_Tree (New_Rhs),
                Expressions => New_List (New_Copy_Tree (Low_B)));

            TestL := Expand_Composite_Equality
              (Outer_Type => Ltyp, Nod => Nod, Comp_Type => Ctyp,
               Lhs => L, Rhs => R);

            L :=
              Make_Indexed_Component (Loc,
                Prefix      => New_Lhs,
                Expressions => New_List (New_Copy_Tree (High_B)));

            R :=
              Make_Indexed_Component (Loc,
                Prefix      => New_Rhs,
                Expressions => New_List (New_Copy_Tree (High_B)));

            TestH := Expand_Composite_Equality
              (Outer_Type => Ltyp, Nod => Nod, Comp_Type => Ctyp,
               Lhs => L, Rhs => R);

            return
              Make_And_Then (Loc, Left_Opnd => TestL, Right_Opnd => TestH);
         end;
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

          Declarations => Decls,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (

                Make_Implicit_If_Statement (Nod,
                  Condition       => Test_Empty_Arrays,
                  Then_Statements => New_List (
                    Make_Simple_Return_Statement (Loc,
                      Expression =>
                        New_Occurrence_Of (Standard_True, Loc)))),

                Make_Implicit_If_Statement (Nod,
                  Condition       => Test_Lengths_Correspond,
                  Then_Statements => New_List (
                    Make_Simple_Return_Statement (Loc,
                      Expression => New_Occurrence_Of (Standard_False, Loc)))),

                Handle_One_Dimension (1, First_Idx),

                Make_Simple_Return_Statement (Loc,
                  Expression => New_Occurrence_Of (Standard_True, Loc)))));

      Set_Has_Completion (Func_Name, True);
      Set_Is_Inlined (Func_Name);

      Append_To (Bodies, Func_Body);

      return
        Make_Function_Call (Loc,
          Name                   => New_Occurrence_Of (Func_Name, Loc),
          Parameter_Associations => New_List (New_Lhs, New_Rhs));
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
         R         : Node_Id             := Relocate_Node (Right_Opnd (N));
         Func_Body : Node_Id;
         Func_Name : Entity_Id;

      begin
         Convert_To_Actual_Subtype (L);
         Convert_To_Actual_Subtype (R);
         Ensure_Defined (Etype (L), N);
         Ensure_Defined (Etype (R), N);
         Apply_Length_Check (R, Etype (L));

         if Nkind (N) = N_Op_Xor then
            R := Duplicate_Subexpr (R);
            Silly_Boolean_Array_Xor_Test (N, R, Etype (L));
         end if;

         if Nkind (Parent (N)) = N_Assignment_Statement
           and then Safe_In_Place_Array_Op (Name (Parent (N)), L, R)
         then
            Build_Boolean_Array_Proc_Call (Parent (N), L, R);

         elsif Nkind (Parent (N)) = N_Op_Not
           and then Nkind (N) = N_Op_And
           and then Nkind (Parent (Parent (N))) = N_Assignment_Statement
           and then Safe_In_Place_Array_Op (Name (Parent (Parent (N))), L, R)
         then
            return;
         else
            Func_Body := Make_Boolean_Array_Op (Etype (L), N);
            Func_Name := Defining_Unit_Name (Specification (Func_Body));
            Insert_Action (N, Func_Body);
            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (Func_Name, Loc),
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

      if Present (Llo) and then Present (Rlo) then
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
     (Outer_Type : Entity_Id;
      Nod        : Node_Id;
      Comp_Type  : Entity_Id;
      Lhs        : Node_Id;
      Rhs        : Node_Id) return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Nod);
      Full_Type : Entity_Id;
      Eq_Op     : Entity_Id;

   begin
      if Is_Private_Type (Comp_Type) then
         Full_Type := Underlying_Type (Comp_Type);
      else
         Full_Type := Comp_Type;
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

      --  Case of tagged record types

      if Is_Tagged_Type (Full_Type) then
         Eq_Op := Find_Primitive_Eq (Comp_Type);
         pragma Assert (Present (Eq_Op));

         return
           Make_Function_Call (Loc,
             Name                   => New_Occurrence_Of (Eq_Op, Loc),
             Parameter_Associations =>
               New_List
                 (Unchecked_Convert_To (Etype (First_Formal (Eq_Op)), Lhs),
                  Unchecked_Convert_To (Etype (First_Formal (Eq_Op)), Rhs)));

      --  Case of untagged record types

      elsif Is_Record_Type (Full_Type) then
         --  Equality composes in Ada 2012 for untagged record types. It also
         --  composes for bounded strings, because they are part of the
         --  predefined environment (see 4.5.2(32.1/1)). We could make it
         --  compose for bounded strings by making them tagged, or by making
         --  sure all subcomponents are set to the same value, even when not
         --  used. Instead, we have this special case in the compiler, because
         --  it's more efficient.

         if Ada_Version >= Ada_2012 or else Is_Bounded_String (Comp_Type) then
            declare
               Eq_Call : constant Node_Id :=
                 Build_Eq_Call (Comp_Type, Loc, Lhs, Rhs);

            begin
               if Present (Eq_Call) then
                  return Eq_Call;
               end if;
            end;
         end if;

         --  Check whether a TSS has been created for the type

         Eq_Op := TSS (Full_Type, TSS_Composite_Equality);

         if Present (Eq_Op) then
            declare
               Op_Typ : constant Entity_Id := Etype (First_Formal (Eq_Op));

               L_Exp, R_Exp : Node_Id;

            begin
               --  Adjust operands if necessary to comparison type

               if Base_Type (Full_Type) /= Base_Type (Op_Typ) then
                  L_Exp := OK_Convert_To (Op_Typ, Lhs);
                  R_Exp := OK_Convert_To (Op_Typ, Rhs);

               else
                  L_Exp := Relocate_Node (Lhs);
                  R_Exp := Relocate_Node (Rhs);
               end if;

               return
                 Make_Function_Call (Loc,
                   Name                   => New_Occurrence_Of (Eq_Op, Loc),
                   Parameter_Associations => New_List (L_Exp, R_Exp));
            end;

         else
            return Expand_Record_Equality (Nod, Full_Type, Lhs, Rhs);
         end if;

      --  Case of non-record types (always use predefined equality)

      else
         --  Print a warning if there is a user-defined "=", because it can be
         --  surprising that the predefined "=" takes precedence over it.

         --  Suppress the warning if the "user-defined" one is in the
         --  predefined library, because those are defined to compose
         --  properly by RM-4.5.2(32.1/1). Intrinsics also compose.

         declare
            Op : constant Entity_Id := Find_Primitive_Eq (Comp_Type);
         begin
            if Warn_On_Ignored_Equality
              and then Present (Op)
              and then not In_Predefined_Unit (Base_Type (Comp_Type))
              and then not Is_Intrinsic_Subprogram (Op)
            then
               pragma Assert
                 (Is_First_Subtype (Outer_Type)
                   or else Is_Generic_Actual_Type (Outer_Type));
               Error_Msg_Node_2 := Comp_Type;
               Error_Msg_N
                 ("?_q?""="" for type & uses predefined ""="" for }",
                  Outer_Type);
               Error_Msg_Sloc := Sloc (Op);
               Error_Msg_N ("\?_q?""="" # is ignored here", Outer_Type);
            end if;
         end;

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

      Result_May_Be_Null : Boolean := True;
      --  Reset to False if at least one operand is encountered which is known
      --  at compile time to be non-null. Used for handling the special case
      --  of setting the high bound to the last operand high bound for a null
      --  result, thus ensuring a proper high bound in the superflat case.

      N : constant Nat := List_Length (Opnds);
      --  Number of concatenation operands including possibly null operands

      NN : Nat := 0;
      --  Number of operands excluding any known to be null, except that the
      --  last operand is always retained, in case it provides the bounds for
      --  a null result.

      Opnd : Node_Id := Empty;
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

      Fixed_Length : array (1 .. N) of Unat;
      --  Set to length of operand. Entries in this array are set only if the
      --  corresponding entry in Is_Fixed_Length is True.

      Max_Length : array (1 .. N) of Unat;
      --  Set to the maximum length of operand, or Too_Large_Length_For_Array
      --  if it is not known. Entries in this array are set only if the
      --  corresponding entry in Is_Fixed_Length is False;

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
      --  The J'th entry is an expression node that represents the total length
      --  of operands 1 through J. It is either an integer literal node, or a
      --  reference to a constant entity with the right value, so it is fine
      --  to just do a Copy_Node to get an appropriate copy. The extra zeroth
      --  entry always is set to zero. The length is of type Artyp.

      Max_Aggr_Length : Unat := Too_Large_Length_For_Array;
      --  Set to the maximum total length, or Too_Large_Length_For_Array at
      --  least if it is not known.

      Low_Bound : Node_Id := Empty;
      --  A tree node representing the low bound of the result (of type Ityp).
      --  This is either an integer literal node, or an identifier reference to
      --  a constant entity initialized to the appropriate value.

      High_Bound : Node_Id := Empty;
      --  A tree node representing the high bound of the result (of type Ityp)

      Last_Opnd_Low_Bound : Node_Id := Empty;
      --  A tree node representing the low bound of the last operand. This
      --  need only be set if the result could be null. It is used for the
      --  special case of setting the right low bound for a null result.
      --  This is of type Ityp.

      Last_Opnd_High_Bound : Node_Id := Empty;
      --  A tree node representing the high bound of the last operand. This
      --  need only be set if the result could be null. It is used for the
      --  special case of setting the right high bound for a null result.
      --  This is of type Ityp.

      Result : Node_Id := Empty;
      --  Result of the concatenation (of type Ityp)

      Actions : constant List_Id := New_List;
      --  Collect actions to be inserted

      Known_Non_Null_Operand_Seen : Boolean;
      --  Set True during generation of the assignments of operands into
      --  result once an operand known to be non-null has been seen.

      function Library_Level_Target return Boolean;
      --  Return True if the concatenation is within the expression of the
      --  declaration of a library-level object.

      function Make_Artyp_Literal (Val : Uint) return Node_Id;
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

      --------------------------
      -- Library_Level_Target --
      --------------------------

      function Library_Level_Target return Boolean is
         P : Node_Id := Parent (Cnode);

      begin
         while Present (P) loop
            if Nkind (P) = N_Object_Declaration then
               return Is_Library_Level_Entity (Defining_Identifier (P));

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (P) then
               return False;
            end if;

            P := Parent (P);
         end loop;

         return False;
      end Library_Level_Target;

      ------------------------
      -- Make_Artyp_Literal --
      ------------------------

      function Make_Artyp_Literal (Val : Uint) return Node_Id is
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

      --  Local variables

      Opnd_Typ   : Entity_Id;
      Slice_Rng  : Node_Id;
      Subtyp_Ind : Node_Id;
      Subtyp_Rng : Node_Id;
      Ent        : Entity_Id;
      Len        : Unat;
      J          : Nat;
      Clen       : Node_Id;
      Set        : Boolean;

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

      --  For modular types, we use a 32-bit modular type for types whose size
      --  is in the range 1-31 bits. For 32-bit unsigned types, we use the
      --  identity type, and for larger unsigned types we use a 64-bit type.

      elsif Is_Modular_Integer_Type (Ityp) then
         if RM_Size (Ityp) < Standard_Integer_Size then
            Artyp := Standard_Unsigned;
         elsif RM_Size (Ityp) = Standard_Integer_Size then
            Artyp := Ityp;
         else
            Artyp := Standard_Long_Long_Unsigned;
         end if;

      --  Similar treatment for signed types

      else
         if RM_Size (Ityp) < Standard_Integer_Size then
            Artyp := Standard_Integer;
         elsif RM_Size (Ityp) = Standard_Integer_Size then
            Artyp := Ityp;
         else
            Artyp := Standard_Long_Long_Integer;
         end if;
      end if;

      --  Supply dummy entry at start of length array

      Aggr_Length (0) := Make_Artyp_Literal (Uint_0);

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

            if Len > 0 then
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

            if Is_Constrained (Opnd_Typ)
              and then Compile_Time_Known_Bounds (Opnd_Typ)
            then
               declare
                  Lo, Hi : Uint;

               begin
                  --  Fixed length constrained array type with known at compile
                  --  time bounds is last case of fixed length operand.

                  Get_First_Index_Bounds (Opnd_Typ, Lo, Hi);
                  Len := UI_Max (Hi - Lo + 1, Uint_0);

                  if Len > 0 then
                     Result_May_Be_Null := False;
                  end if;

                  --  Capture last operand bounds if result could be null

                  if J = N and then Result_May_Be_Null then
                     Last_Opnd_Low_Bound :=
                       To_Ityp (Make_Integer_Literal (Loc, Lo));

                     Last_Opnd_High_Bound :=
                       To_Ityp (Make_Integer_Literal (Loc, Hi));
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
                    To_Ityp (Make_Integer_Literal (Loc, Lo));
                  Set := True;
               end;
            end if;

            --  All cases where the length is not known at compile time, or the
            --  special case of an operand which is known to be null but has a
            --  lower bound other than 1 or is other than a string type.

            if not Set then
               NN := NN + 1;

               --  Set low bound of operand and check first the constrained
               --  case with known bound

               if Is_Constrained (Opnd_Typ) then
                  declare
                     Low_Bound : constant Node_Id :=
                       Type_Low_Bound
                         (Underlying_Type (Etype (First_Index (Opnd_Typ))));

                  begin
                     if Compile_Time_Known_Value (Low_Bound) then
                        Opnd_Low_Bound (NN) := New_Copy_Tree (Low_Bound);
                        Set := True;
                     end if;
                  end;
               end if;

               --  Otherwise fall back to the general expression

               if not Set then
                  Opnd_Low_Bound (NN) :=
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        Duplicate_Subexpr (Opnd, Name_Req => True),
                      Attribute_Name => Name_First);
               end if;

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

               --  If the operand is a slice, try to compute an upper bound for
               --  its length.

               if Nkind (Opnd) = N_Slice
                  and then Is_Constrained (Etype (Prefix (Opnd)))
                  and then Compile_Time_Known_Bounds (Etype (Prefix (Opnd)))
               then
                  declare
                     Lo, Hi : Uint;

                  begin
                     Get_First_Index_Bounds (Etype (Prefix (Opnd)), Lo, Hi);
                     Max_Length (NN) := UI_Max (Hi - Lo + 1, Uint_0);
                  end;

               else
                  Max_Length (NN) := Too_Large_Length_For_Array;
               end if;

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
               Max_Aggr_Length := Fixed_Length (1);
            else
               Aggr_Length (1) := New_Occurrence_Of (Var_Length (1), Loc);
               Max_Aggr_Length := Max_Length (1);
            end if;

         --  If entry is fixed length and only fixed lengths so far, make
         --  appropriate new integer literal adding new length.

         elsif Is_Fixed_Length (NN)
           and then Nkind (Aggr_Length (NN - 1)) = N_Integer_Literal
         then
            Aggr_Length (NN) :=
              Make_Integer_Literal (Loc,
                Intval => Fixed_Length (NN) + Intval (Aggr_Length (NN - 1)));
            Max_Aggr_Length := Intval (Aggr_Length (NN));

         --  All other cases, construct an addition node for the length and
         --  create an entity initialized to this length.

         else
            Ent := Make_Temporary (Loc, 'L');

            if Is_Fixed_Length (NN) then
               Clen := Make_Integer_Literal (Loc, Fixed_Length (NN));
               Max_Aggr_Length := Max_Aggr_Length + Fixed_Length (NN);

            else
               Clen := New_Occurrence_Of (Var_Length (NN), Loc);
               Max_Aggr_Length := Max_Aggr_Length + Max_Length (NN);
            end if;

            Append_To (Actions,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Ent,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Artyp, Loc),
                Expression          =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => New_Copy_Tree (Aggr_Length (NN - 1)),
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

      --  We fold the common case where all the low bounds are the same

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
                  return New_Copy_Tree (Opnd_Low_Bound (J));

               else
                  declare
                     Known_Bound : constant Node_Id := Get_Known_Bound (J + 1);
                     Comparison  : constant Compare_Result :=
                       Compile_Time_Compare
                         (Opnd_Low_Bound (J),
                          Known_Bound,
                          Assume_Valid => True);

                  begin
                     if Comparison = EQ then
                        return Known_Bound;

                     else
                        return
                          Make_If_Expression (Loc,
                            Expressions => New_List (

                              Make_Op_Ne (Loc,
                                Left_Opnd  =>
                                  New_Occurrence_Of (Var_Length (J), Loc),
                                Right_Opnd =>
                                  Make_Integer_Literal (Loc, 0)),

                              New_Copy_Tree (Opnd_Low_Bound (J)),
                              Known_Bound));
                     end if;
                  end;
               end if;
            end Get_Known_Bound;

            Known_Bound : constant Node_Id := Get_Known_Bound (1);

         begin
            if Nkind (Known_Bound) /= N_If_Expression then
               Low_Bound := Known_Bound;

            else
               Ent := Make_Temporary (Loc, 'L');

               Append_To (Actions,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Ent,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Ityp, Loc),
                   Expression          => Known_Bound));

               Low_Bound := New_Occurrence_Of (Ent, Loc);
            end if;
         end;
      end if;

      pragma Assert (Present (Low_Bound));

      --  Now we can compute the high bound as Low_Bound + Length - 1

      if Compile_Time_Known_Value (Low_Bound)
        and then Nkind (Aggr_Length (NN)) = N_Integer_Literal
      then
         High_Bound :=
           To_Ityp
             (Make_Artyp_Literal
                (Expr_Value (Low_Bound) + Intval (Aggr_Length (NN)) - 1));

      else
         High_Bound :=
           To_Ityp
             (Make_Op_Add (Loc,
                Left_Opnd  => To_Artyp (New_Copy_Tree (Low_Bound)),
                Right_Opnd =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd  => New_Copy_Tree (Aggr_Length (NN)),
                    Right_Opnd => Make_Artyp_Literal (Uint_1))));

         --  Note that calculation of the high bound may cause overflow in some
         --  very weird cases, so in the general case we need an overflow check
         --  on the high bound. We can avoid this for the common case of string
         --  types and other types whose index is Positive, since we chose a
         --  wider range for the arithmetic type. If checks are suppressed, we
         --  do not set the flag so superfluous warnings may be omitted.

         if Istyp /= Standard_Positive
           and then not Overflow_Checks_Suppressed (Istyp)
         then
            Activate_Overflow_Check (High_Bound);
         end if;
      end if;

      --  Handle the exceptional case where the result is null, in which case
      --  case the bounds come from the last operand (so that we get the proper
      --  bounds if the last operand is superflat).

      if Result_May_Be_Null then
         Low_Bound :=
           Make_If_Expression (Loc,
             Expressions => New_List (
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Copy_Tree (Aggr_Length (NN)),
                 Right_Opnd => Make_Artyp_Literal (Uint_0)),
               Last_Opnd_Low_Bound,
               Low_Bound));

         High_Bound :=
           Make_If_Expression (Loc,
             Expressions => New_List (
               Make_Op_Eq (Loc,
                 Left_Opnd  => New_Copy_Tree (Aggr_Length (NN)),
                 Right_Opnd => Make_Artyp_Literal (Uint_0)),
               Last_Opnd_High_Bound,
               High_Bound));
      end if;

      --  Here is where we insert the saved up actions

      Insert_Actions (Cnode, Actions, Suppress => All_Checks);

      --  If the low bound is known at compile time and not the high bound, but
      --  we have computed a sensible upper bound for the length, then adjust
      --  the high bound for the subtype of the array. This will change it into
      --  a static subtype and thus help the code generator.

      if Compile_Time_Known_Value (Low_Bound)
        and then not Compile_Time_Known_Value (High_Bound)
        and then Max_Aggr_Length < Too_Large_Length_For_Array
      then
         declare
            Known_High_Bound : constant Node_Id :=
              To_Ityp
                (Make_Artyp_Literal
                   (Expr_Value (Low_Bound) +  Max_Aggr_Length - 1));

         begin
            if not Is_Out_Of_Range (Known_High_Bound, Ityp) then
               Slice_Rng  := Make_Range (Loc, Low_Bound, High_Bound);
               High_Bound := Known_High_Bound;

            else
               Slice_Rng := Empty;
            end if;
         end;

      else
         Slice_Rng := Empty;
      end if;

      Subtyp_Rng := Make_Range (Loc, Low_Bound, High_Bound);

      --  If the result cannot be null then the range cannot be superflat

      Set_Cannot_Be_Superflat (Subtyp_Rng, not Result_May_Be_Null);

      --  Now we construct an array object with appropriate bounds. We mark
      --  the target as internal to prevent useless initialization when
      --  Initialize_Scalars is enabled. Also since this is the actual result
      --  entity, we make sure we have debug information for the result.

      Subtyp_Ind :=
        Make_Subtype_Indication (Loc,
          Subtype_Mark => New_Occurrence_Of (Atyp, Loc),
          Constraint   =>
            Make_Index_Or_Discriminant_Constraint (Loc,
              Constraints => New_List (Subtyp_Rng)));

      Ent := Make_Temporary (Loc, 'S');
      Set_Is_Internal       (Ent);
      Set_Debug_Info_Needed (Ent);

      --  If we are concatenating strings and the current scope already uses
      --  the secondary stack, allocate the result also on the secondary stack
      --  to avoid putting too much pressure on the primary stack.

      --  Don't do this if -gnatd.h is set, as this will break the wrapping of
      --  Cnode in an Expression_With_Actions, see Expand_N_Op_Concat.

      if Atyp = Standard_String
        and then Uses_Sec_Stack (Current_Scope)
        and then RTE_Available (RE_SS_Pool)
        and then not Debug_Flag_Dot_H
      then
         --  Generate:
         --     subtype Axx is String (<low-bound> .. <high-bound>)
         --     type Ayy is access Axx;
         --     Rxx : Ayy := new <Axx> [storage_pool = ss_pool];
         --     Sxx : Axx renames Rxx.all;

         declare
            ConstrT : constant Entity_Id := Make_Temporary (Loc, 'A');
            Acc_Typ : constant Entity_Id := Make_Temporary (Loc, 'A');

            Alloc : Node_Id;
            Temp  : Entity_Id;

         begin
            Insert_Action (Cnode,
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => ConstrT,
                Subtype_Indication  => Subtyp_Ind),
              Suppress => All_Checks);

            Freeze_Itype (ConstrT, Cnode);

            Insert_Action (Cnode,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Acc_Typ,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    Subtype_Indication => New_Occurrence_Of (ConstrT, Loc))),
              Suppress => All_Checks);

            Mutate_Ekind (Acc_Typ, E_Access_Type);
            Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));

            Alloc :=
              Make_Allocator (Loc,
                Expression => New_Occurrence_Of (ConstrT, Loc));

            --  This is currently done only for type String, which normally
            --  doesn't have default initialization, but we need to set the
            --  No_Initialization flag in case of either Initialize_Scalars
            --  or Normalize_Scalars.

            Set_No_Initialization (Alloc);

            Temp := Make_Temporary (Loc, 'R', Alloc);
            Insert_Action (Cnode,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp,
                Object_Definition   => New_Occurrence_Of (Acc_Typ, Loc),
                Expression          => Alloc),
              Suppress => All_Checks);

            Insert_Action (Cnode,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Ent,
                Subtype_Mark        => New_Occurrence_Of (ConstrT, Loc),
                Name                =>
                  Make_Explicit_Dereference (Loc,
                    Prefix => New_Occurrence_Of (Temp, Loc))),
              Suppress => All_Checks);
         end;

      else
         --  If the bound is statically known to be out of range, we do not
         --  want to abort, we want a warning and a runtime constraint error.
         --  Note that we have arranged that the result will not be treated
         --  as a static constant, so we won't get an illegality during this
         --  insertion. We also enable checks (in particular range checks) in
         --  case the bounds of Subtyp_Ind are out of range.

         Insert_Action (Cnode,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => Subtyp_Ind));
      end if;

      --  If the result of the concatenation appears as the initializing
      --  expression of an object declaration, we can just rename the
      --  result, rather than copying it.

      Set_OK_To_Rename (Ent);

      --  Catch the static out of range case now

      if Raises_Constraint_Error (High_Bound)
        or else Is_Out_Of_Range (High_Bound, Ityp)
      then
         --  Kill warning generated for the declaration of the static out of
         --  range high bound, and instead generate a Constraint_Error with
         --  an appropriate specific message.

         if Nkind (High_Bound) = N_Integer_Literal then
            Kill_Dead_Code (High_Bound);
            Rewrite (High_Bound, New_Copy_Tree (Low_Bound));

         else
            Kill_Dead_Code (Declaration_Node (Entity (High_Bound)));
         end if;

         Apply_Compile_Time_Constraint_Error
           (N      => Cnode,
            Msg    => "concatenation result upper bound out of range??",
            Reason => CE_Range_Check_Failed);

         return;
      end if;

      --  Now we will generate the assignments to do the actual concatenation

      --  There is one case in which we will not do this, namely when all the
      --  following conditions are met:

      --    The result type is Standard.String

      --    There are nine or fewer retained (non-null) operands

      --    The optimization level is -O0 or the debug flag gnatd.C is set,
      --    and the debug flag gnatd.c is not set.

      --    The corresponding System.Concat_n.Str_Concat_n routine is
      --    available in the run time.

      --  If all these conditions are met then we generate a call to the
      --  relevant concatenation routine. The purpose of this is to avoid
      --  undesirable code bloat at -O0.

      --  If the concatenation is within the declaration of a library-level
      --  object, we call the built-in concatenation routines to prevent code
      --  bloat, regardless of the optimization level. This is space efficient
      --  and prevents linking problems when units are compiled with different
      --  optimization levels.

      if Atyp = Standard_String
        and then NN in 2 .. 9
        and then (((Optimization_Level = 0 or else Debug_Flag_Dot_CC)
                     and then not Debug_Flag_Dot_C)
                  or else Library_Level_Target)
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

                  --  No assignments left to do below

                  NN := 0;
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
                     Left_Opnd  => To_Artyp (New_Copy_Tree (Low_Bound)),
                     Right_Opnd => Aggr_Length (J - 1));

            Hi : constant Node_Id :=
                   Make_Op_Add (Loc,
                     Left_Opnd  => To_Artyp (New_Copy_Tree (Low_Bound)),
                     Right_Opnd =>
                       Make_Op_Subtract (Loc,
                         Left_Opnd  => Aggr_Length (J),
                         Right_Opnd => Make_Artyp_Literal (Uint_1)));

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

            elsif not Is_Fixed_Length (J) or else Fixed_Length (J) > 0 then
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

      --  Finally we build the result, which is either a direct reference to
      --  the array object or a slice of it.

      Result := New_Occurrence_Of (Ent, Loc);

      if Present (Slice_Rng) then
         Result := Make_Slice (Loc, Result, Slice_Rng);
      end if;

   <<Done>>
      pragma Assert (Present (Result));
      Rewrite (Cnode, Result);
      Analyze_And_Resolve (Cnode, Atyp);
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
      --  Entity for Long_Long_Integer'Base

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
                Low_Bound  =>
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
                        Left_Opnd  =>
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

            --         SS_Release (M);
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
                                 Prefix         =>
                                   New_Occurrence_Of (TB, Loc))),
                           High_Bound =>
                             Convert_To (LLIB,
                               Make_Attribute_Reference (Loc,
                                 Attribute_Name => Name_Last,
                                 Prefix         =>
                                   New_Occurrence_Of (TB, Loc))))),
                   Right_Opnd => Nin));
               Set_Analyzed (N, False);
               Analyze_And_Resolve (N, Restype);
            end;
         end if;
      end if;
   end Expand_Membership_Minimize_Eliminate_Overflow;

   ---------------------------------
   -- Expand_Nonbinary_Modular_Op --
   ---------------------------------

   procedure Expand_Nonbinary_Modular_Op (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

      procedure Expand_Modular_Addition;
      --  Expand the modular addition, handling the special case of adding a
      --  constant.

      procedure Expand_Modular_Op;
      --  Compute the general rule: (lhs OP rhs) mod Modulus

      procedure Expand_Modular_Subtraction;
      --  Expand the modular addition, handling the special case of subtracting
      --  a constant.

      -----------------------------
      -- Expand_Modular_Addition --
      -----------------------------

      procedure Expand_Modular_Addition is
      begin
         --  If this is not the addition of a constant or else restriction
         --  No_Implicit_Conditionals is in effect, then compute it using
         --  the general rule: (lhs + rhs) mod Modulus.

         if Nkind (Right_Opnd (N)) /= N_Integer_Literal
           or else Restriction_Active (No_Implicit_Conditionals)
         then
            Expand_Modular_Op;

         --  If this is an addition of a constant, convert it to a subtraction
         --  plus a conditional expression since we can compute it faster than
         --  computing the modulus.

         --      modMinusRhs = Modulus - rhs
         --      if lhs < modMinusRhs then lhs + rhs
         --                           else lhs - modMinusRhs

         else
            declare
               Mod_Minus_Right : constant Uint :=
                                   Modulus (Typ) - Intval (Right_Opnd (N));

               Cond_Expr : Node_Id;
               Then_Expr : Node_Id;
               Else_Expr : Node_Id;

            begin
               --  To prevent spurious visibility issues, convert all
               --  operands to Standard.Unsigned.

               Cond_Expr :=
                 Make_Op_Lt (Loc,
                   Left_Opnd  =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Left_Opnd (N))),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Mod_Minus_Right));

               Then_Expr :=
                 Make_Op_Add (Loc,
                   Left_Opnd  =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Left_Opnd (N))),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Intval (Right_Opnd (N))));

               Else_Expr :=
                 Make_Op_Subtract (Loc,
                   Left_Opnd  =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Left_Opnd (N))),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Mod_Minus_Right));

               Rewrite (N,
                 Unchecked_Convert_To (Typ,
                   Make_If_Expression (Loc,
                     Expressions =>
                       New_List (Cond_Expr, Then_Expr, Else_Expr))));
            end;
         end if;
      end Expand_Modular_Addition;

      -----------------------
      -- Expand_Modular_Op --
      -----------------------

      procedure Expand_Modular_Op is
         --   We will convert to another type (not a nonbinary-modulus modular
         --   type), evaluate the op in that representation, reduce the result,
         --   and convert back to the original type. This means that the
         --   back end does not have to deal with nonbinary-modulus ops.

         Mod_Expr    : Node_Id;
         Op_Expr     : Node_Id;
         Target_Type : Entity_Id;

      begin
         --  Select a target type that is large enough to avoid spurious
         --  intermediate overflow on pre-reduction computation (for
         --  correctness) but is no larger than is needed (for performance).

         declare
            Required_Size : Uint := RM_Size (Etype (N));

         begin
            case Nkind (N) is
               when N_Op_Add | N_Op_Subtract =>
                  --  For example, if modulus is 255 then RM_Size will be 8
                  --  and the range of possible values (before reduction) will
                  --  be 0 .. 508; that range requires 9 bits.
                  Required_Size := Required_Size + 1;

               when N_Op_Multiply =>
                  --  For example, if modulus is 255 then RM_Size will be 8
                  --  and the range of possible values (before reduction) will
                  --  be 0 .. 64,516; that range requires 16 bits.
                  Required_Size := Required_Size * 2;

               when others =>
                  null;
            end case;

            Target_Type := Small_Integer_Type_For (Required_Size, Uns => True);
            pragma Assert (Present (Target_Type));
         end;

         Op_Expr := New_Op_Node (Nkind (N), Loc);
         Set_Left_Opnd (Op_Expr,
           Unchecked_Convert_To (Target_Type, New_Copy_Tree (Left_Opnd (N))));
         Set_Right_Opnd (Op_Expr,
           Unchecked_Convert_To (Target_Type, New_Copy_Tree (Right_Opnd (N))));

         --  ??? Why do this stuff for some ops and not others?
         if Nkind (N) not in N_Op_And | N_Op_Or | N_Op_Xor then

            --  Link this node to the tree to analyze it

            --  If the parent node is an expression with actions we link it to
            --  N since otherwise Force_Evaluation cannot identify if this node
            --  comes from the Expression and rejects generating the temporary.

            if Nkind (Parent (N)) = N_Expression_With_Actions then
               Set_Parent (Op_Expr, N);

            --  Common case

            else
               Set_Parent (Op_Expr, Parent (N));
            end if;

            Analyze (Op_Expr);

            --  Force generating a temporary because in the expansion of this
            --  expression we may generate code that performs this computation
            --  several times.

            Force_Evaluation (Op_Expr, Mode => Strict);
         end if;

         --  Unconditionally add the modulus to the result for a subtraction,
         --  this gets rid of all its peculiarities by cancelling out the
         --  addition of the binary modulus in the case where the subtraction
         --  wraps around in Target_Type.

         if Nkind (N) = N_Op_Subtract then
            Op_Expr :=
               Make_Op_Add (Loc,
                 Left_Opnd  => Op_Expr,
                 Right_Opnd => Make_Integer_Literal (Loc, Modulus (Typ)));
         end if;

         Mod_Expr :=
           Make_Op_Mod (Loc,
             Left_Opnd  => Op_Expr,
             Right_Opnd => Make_Integer_Literal (Loc, Modulus (Typ)));

         Rewrite (N, Unchecked_Convert_To (Typ, Mod_Expr));
      end Expand_Modular_Op;

      --------------------------------
      -- Expand_Modular_Subtraction --
      --------------------------------

      procedure Expand_Modular_Subtraction is
      begin
         --  If this is not the addition of a constant or else restriction
         --  No_Implicit_Conditionals is in effect, then compute it using
         --  the general rule: (lhs - rhs) mod Modulus.

         if Nkind (Right_Opnd (N)) /= N_Integer_Literal
           or else Restriction_Active (No_Implicit_Conditionals)
         then
            Expand_Modular_Op;

         --  If this is an addition of a constant, convert it to a subtraction
         --  plus a conditional expression since we can compute it faster than
         --  computing the modulus.

         --      modMinusRhs = Modulus - rhs
         --      if lhs < rhs then lhs + modMinusRhs
         --                   else lhs - rhs

         else
            declare
               Mod_Minus_Right : constant Uint :=
                                   Modulus (Typ) - Intval (Right_Opnd (N));

               Cond_Expr : Node_Id;
               Then_Expr : Node_Id;
               Else_Expr : Node_Id;

            begin
               Cond_Expr :=
                 Make_Op_Lt (Loc,
                   Left_Opnd  =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Left_Opnd (N))),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Intval (Right_Opnd (N))));

               Then_Expr :=
                 Make_Op_Add (Loc,
                   Left_Opnd  =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Left_Opnd (N))),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Mod_Minus_Right));

               Else_Expr :=
                 Make_Op_Subtract (Loc,
                   Left_Opnd  =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Left_Opnd (N))),
                   Right_Opnd =>
                     Unchecked_Convert_To (Standard_Unsigned,
                       New_Copy_Tree (Right_Opnd (N))));

               Rewrite (N,
                 Unchecked_Convert_To (Typ,
                   Make_If_Expression (Loc,
                     Expressions =>
                       New_List (Cond_Expr, Then_Expr, Else_Expr))));
            end;
         end if;
      end Expand_Modular_Subtraction;

   --  Start of processing for Expand_Nonbinary_Modular_Op

   begin
      --  No action needed if we have a binary modular operand

      if not Non_Binary_Modulus (Typ) then
         return;
      end if;

      case Nkind (N) is
         when N_Op_Add =>
            --  No action needed if front-end expansion is not required and
            --  restriction No_Implicit_Conditionals is not in effect.

            if not Expand_Nonbinary_Modular_Ops
              and then not Restriction_Active (No_Implicit_Conditionals)
            then
               return;
            end if;

            Expand_Modular_Addition;

         when N_Op_Subtract =>
            --  No action needed if front-end expansion is not required and
            --  restriction No_Implicit_Conditionals is not in effect.

            if not Expand_Nonbinary_Modular_Ops
              and then not Restriction_Active (No_Implicit_Conditionals)
            then
               return;
            end if;

            Expand_Modular_Subtraction;

         when N_Op_Minus =>
            --  No action needed if front-end expansion is not required and
            --  restriction No_Implicit_Conditionals is not in effect.

            if not Expand_Nonbinary_Modular_Ops
              and then not Restriction_Active (No_Implicit_Conditionals)
            then
               return;
            end if;

            --  Expand -expr into (0 - expr)

            Rewrite (N,
              Make_Op_Subtract (Loc,
                Left_Opnd  => Make_Integer_Literal (Loc, 0),
                Right_Opnd => Right_Opnd (N)));
            Analyze_And_Resolve (N, Typ);

         when others =>
            --  No action needed only if front-end expansion is not required
            --  because we assume that logical and multiplicative operations
            --  do not involve implicit conditionals.

            if not Expand_Nonbinary_Modular_Ops then
               return;
            end if;

            Expand_Modular_Op;
      end case;

      Analyze_And_Resolve (N, Typ);
   end Expand_Nonbinary_Modular_Op;

   ------------------------
   -- Expand_N_Allocator --
   ------------------------

   procedure Expand_N_Allocator (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      PtrT : constant Entity_Id  := Etype (N);
      Dtyp : constant Entity_Id  := Available_View (Designated_Type (PtrT));
      Etyp : constant Entity_Id  := Etype (Expression (N));

      procedure Rewrite_Coextension (N : Node_Id);
      --  Static coextensions have the same lifetime as the entity they
      --  constrain. Such occurrences can be rewritten as aliased objects
      --  and their unrestricted access used instead of the coextension.

      function Size_In_Storage_Elements (E : Entity_Id) return Node_Id;
      --  Given a constrained array type E, returns a node representing the
      --  code to compute a close approximation of the size in storage elements
      --  for the given type; for indexes that are modular types we compute
      --  'Last - First (instead of 'Length) because for large arrays computing
      --  'Last -'First + 1 causes overflow. This is done without using the
      --  attribute 'Size_In_Storage_Elements (which malfunctions for large
      --  sizes ???).

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
         Idx : Node_Id := First_Index (E);
         Len : Node_Id := Empty;
         Res : Node_Id := Empty;

      begin
         --  Logically this just returns E'Max_Size_In_Storage_Elements.
         --  However, the reason for the existence of this function is to
         --  construct a test for sizes too large, which means near the 32-bit
         --  limit on a 32-bit machine, and precisely the trouble is that we
         --  get overflows when sizes are greater than 2**31.

         --  So what we end up doing for array types is to use the expression:

         --    number-of-elements * component_type'Max_Size_In_Storage_Elements

         --  which avoids this problem. All this is a bit bogus, but it does
         --  mean we catch common cases of trying to allocate arrays that are
         --  too large, and which in the absence of a check results in
         --  undetected chaos ???

         for J in 1 .. Number_Dimensions (E) loop

            if not Is_Modular_Integer_Type (Etype (Idx)) then
               Len :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (E, Loc),
                   Attribute_Name => Name_Length,
                   Expressions    => New_List (Make_Integer_Literal (Loc, J)));

            --  For indexes that are modular types we cannot generate code to
            --  compute 'Length since for large arrays 'Last -'First + 1 causes
            --  overflow; therefore we compute 'Last - 'First (which is not the
            --  exact number of components but it is valid for the purpose of
            --  this runtime check on 32-bit targets).

            else
               declare
                  Len_Minus_1_Expr : Node_Id;
                  Test_Gt          : Node_Id;

               begin
                  Test_Gt :=
                    Make_Op_Gt (Loc,
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (E, Loc),
                        Attribute_Name => Name_Last,
                        Expressions    =>
                          New_List (Make_Integer_Literal (Loc, J))),
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (E, Loc),
                        Attribute_Name => Name_First,
                        Expressions    =>
                          New_List (Make_Integer_Literal (Loc, J))));

                  Len_Minus_1_Expr :=
                    Convert_To (Standard_Unsigned,
                      Make_Op_Subtract (Loc,
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (E, Loc),
                          Attribute_Name => Name_Last,
                          Expressions =>
                            New_List (Make_Integer_Literal (Loc, J))),
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (E, Loc),
                          Attribute_Name => Name_First,
                          Expressions =>
                            New_List (Make_Integer_Literal (Loc, J)))));

                  --  Handle superflat arrays, i.e. arrays with such bounds as
                  --  4 .. 2, to ensure that the result is correct.

                  --  Generate:
                  --    (if X'Last > X'First then X'Last - X'First else 0)

                  Len :=
                    Make_If_Expression (Loc,
                      Expressions => New_List (
                        Test_Gt,
                        Len_Minus_1_Expr,
                        Make_Integer_Literal (Loc, Uint_0)));
               end;
            end if;

            if J = 1 then
               Res := Len;

            else
               pragma Assert (Present (Res));
               Res :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd  => Res,
                   Right_Opnd => Len);
            end if;

            Next_Index (Idx);
         end loop;

         return
           Make_Op_Multiply (Loc,
             Left_Opnd  => Len,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Component_Type (E), Loc),
                 Attribute_Name => Name_Max_Size_In_Storage_Elements));
      end Size_In_Storage_Elements;

      --  Local variables

      Desig      : Entity_Id;
      Init_Expr  : Node_Id;
      Init_Stmts : List_Id;
      Pool       : Entity_Id;
      Rel_Typ    : Entity_Id;
      Target_Ref : Node_Id;
      Temp       : Entity_Id;
      Temp_Decl  : Node_Id;

   --  Start of processing for Expand_N_Allocator

   begin
      --  Warn on the presence of an allocator of an anonymous access type when
      --  enabled, except when it's an object declaration at library level.

      if Warn_On_Anonymous_Allocators
        and then Ekind (PtrT) = E_Anonymous_Access_Type
        and then not (Is_Library_Level_Entity (PtrT)
                       and then Nkind (Associated_Node_For_Itype (PtrT)) =
                                  N_Object_Declaration)
      then
         Error_Msg_N ("?_a?use of an anonymous access type allocator", N);
      end if;

      --  RM E.2.2(17). We enforce that the expected type of an allocator
      --  shall not be a remote access-to-class-wide-limited-private type.
      --  We probably shouldn't be doing this legality check during expansion,
      --  but this is only an issue for Annex E users, and is unlikely to be a
      --  problem in practice.

      Validate_Remote_Access_To_Class_Wide_Type (N);

      --  Processing for anonymous access-to-controlled types. These access
      --  types receive a special finalization collection which appears in the
      --  declarations of the enclosing semantic unit. This expansion is done
      --  now to ensure that any additional types generated by this routine or
      --  Expand_Allocator_Expression inherit the proper type attributes.

      if (Ekind (PtrT) = E_Anonymous_Access_Type
           or else (Is_Itype (PtrT)
                     and then No (Finalization_Collection (PtrT))))
        and then Needs_Finalization (Dtyp)
      then
         --  Detect the allocation of an anonymous controlled object where the
         --  type of the context is named. For example:

         --     procedure Proc (Ptr : Named_Access_Typ);
         --     Proc (new Designated_Typ);

         --  Regardless of the anonymous-to-named access type conversion, the
         --  lifetime of the object must be associated with the named access
         --  type. Use the finalization-related attributes of this type.

         if Nkind (Parent (N)) in N_Type_Conversion
                                | N_Unchecked_Type_Conversion
           and then Ekind (Etype (Parent (N))) in E_Access_Subtype
                                                | E_Access_Type
                                                | E_General_Access_Type
         then
            Rel_Typ := Etype (Parent (N));
         else
            Rel_Typ := Empty;
         end if;

         --  Anonymous access-to-controlled types allocate on the global pool.
         --  Note that this is a "root type only" attribute.

         if No (Associated_Storage_Pool (PtrT)) then
            if Present (Rel_Typ) then
               Set_Associated_Storage_Pool
                 (Root_Type (PtrT), Associated_Storage_Pool (Rel_Typ));
            else
               Set_Associated_Storage_Pool
                 (Root_Type (PtrT), RTE (RE_Global_Pool_Object));
            end if;
         end if;

         --  The finalization collection must be inserted and analyzed as part
         --  of the current semantic unit. Note that the collection is updated
         --  when analysis changes current units. Note that this is a root type
         --  attribute.

         if Present (Rel_Typ) then
            Set_Finalization_Collection
              (Root_Type (PtrT), Finalization_Collection (Rel_Typ));
         else
            Build_Anonymous_Collection (Root_Type (PtrT));
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

            if Is_RTE (Pool, RE_RS_Pool) then
               Set_Procedure_To_Call (N, RTE (RE_RS_Allocate));

            elsif Is_RTE (Pool, RE_SS_Pool) then
               Check_Restriction (No_Secondary_Stack, N);
               Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));

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
                 Find_Storage_Op (Etype (Pool), Name_Allocate));
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

         --  The check on No_Initialization is used here to prevent generating
         --  this runtime check twice when the allocator is locally replaced by
         --  the expander with another one.

         if Is_Array_Type (Etyp) and then not No_Initialization (N) then
            declare
               Cond    : Node_Id;
               Ins_Nod : Node_Id   := N;
               Siz_Typ : Entity_Id := Etyp;
               Expr    : Node_Id;

            begin
               --  For unconstrained array types initialized with a qualified
               --  expression we use its type to perform this check

               if not Is_Constrained (Etyp)
                 and then not No_Initialization (N)
                 and then Nkind (Expression (N)) = N_Qualified_Expression
               then
                  Expr    := Expression (Expression (N));
                  Siz_Typ := Etype (Expression (Expression (N)));

                  --  If the qualified expression has been moved to an internal
                  --  temporary (to remove side effects) then we must insert
                  --  the runtime check before its declaration to ensure that
                  --  the check is performed before the execution of the code
                  --  computing the qualified expression.

                  if Nkind (Expr) = N_Identifier
                    and then Is_Internal_Name (Chars (Expr))
                    and then
                      Nkind (Parent (Entity (Expr))) = N_Object_Declaration
                  then
                     Ins_Nod := Parent (Entity (Expr));
                  else
                     Ins_Nod := Expr;
                  end if;
               end if;

               if Is_Constrained (Siz_Typ)
                 and then Ekind (Siz_Typ) /= E_String_Literal_Subtype
               then
                  --  The largest object is 3.5 gigabytes

                  Cond :=
                    Make_Op_Gt (Loc,
                      Left_Opnd  => Size_In_Storage_Elements (Siz_Typ),
                      Right_Opnd => Make_Integer_Literal (Loc,
                                      Uint_7 * (Uint_2 ** 29)));
                  Insert_Action (Ins_Nod,
                    Make_Raise_Storage_Error (Loc,
                      Condition => Cond,
                      Reason    => SE_Object_Too_Large));

                  if Entity (Cond) = Standard_True then
                     Error_Msg_N
                       ("object too large: Storage_Error will be raised at "
                        & "run time??", N);
                  end if;
               end if;
            end;
         end if;
      end if;

      --  If no storage pool has been specified, or the storage pool
      --  is System.Pool_Global.Global_Pool_Object, and the restriction
      --  No_Standard_Allocators_After_Elaboration is present, then generate
      --  a call to Elaboration_Allocators.Check_Standard_Allocator.

      if Nkind (N) = N_Allocator
        and then (No (Storage_Pool (N))
                   or else Is_RTE (Storage_Pool (N), RE_Global_Pool_Object))
        and then Restriction_Active (No_Standard_Allocators_After_Elaboration)
      then
         Insert_Action (N,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Check_Standard_Allocator), Loc)));
      end if;

      --  Handle case of qualified expression (other than optimization above)

      if Nkind (Expression (N)) = N_Qualified_Expression then
         Expand_Allocator_Expression (N);

      --  If no initialization is necessary, just create a custom Allocate if
      --  the context requires it; that is the case only for allocators built
      --  for the special return objects because, in other cases, the custom
      --  Allocate will be created later during the expansion of the original
      --  allocator without the No_Initialization flag.

      elsif No_Initialization (N) then
         if For_Special_Return_Object (N) then
            Build_Allocate_Deallocate_Proc (Parent (N));
         end if;

      --  If the allocator is for a type which requires initialization, and
      --  there is no initial value (i.e. operand is a subtype indication
      --  rather than a qualified expression), then we must generate a call to
      --  the initialization routine:

      --    Temp : constant PtrT := new T;
      --    Init (Temp.all,...);
      --    ... := Temp.all;

      --  A special case arises if T is a task type or contains tasks. In this
      --  case the call to Init (Temp.all ...) is replaced by code that ensures
      --  that tasks get activated (see Build_Task_Allocate_Block for details).

      else
         --  Apply constraint checks against designated subtype (RM 4.8(10/2)).
         --  Discriminant checks will be generated by the expansion below.

         if Is_Array_Type (Dtyp) then
            Apply_Constraint_Check (Expression (N), Dtyp, No_Sliding => True);

            if Nkind (Expression (N)) = N_Raise_Constraint_Error then
               Rewrite (N, New_Copy (Expression (N)));
               Set_Etype (N, PtrT);
               return;
            end if;
         end if;

         --  First try a simple initialization; if it succeeds, then we just
         --  assign the value to the allocated memory.

         Init_Expr := Build_Default_Simple_Initialization (N, Etyp, Empty);

         if Present (Init_Expr) then
            declare
               Deref : Node_Id;
               Stmt  : Node_Id;

            begin
               --  We set the allocator as analyzed so that when we analyze
               --  the expression node, we do not get an unwanted recursive
               --  expansion of the allocator expression.

               Set_Analyzed (N);

               Temp := Make_Temporary (Loc, 'P');

               --  Generate:
               --    Temp : constant PtrT := new ...;

               Temp_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (PtrT, Loc),
                   Expression          => Relocate_Node (N));

               Insert_Action (N, Temp_Decl, Suppress => All_Checks);

               --  Generate:
               --    Temp.all := ...

               Deref :=
                 Make_Explicit_Dereference (Loc,
                   New_Occurrence_Of (Temp, Loc));

               if Is_Incomplete_Or_Private_Type (Designated_Type (PtrT)) then
                  Deref := Unchecked_Convert_To (Etype (Init_Expr), Deref);
               end if;

               Stmt :=
                 Make_Assignment_Statement (Loc,
                   Name       => Deref,
                   Expression => Init_Expr);
               Set_Assignment_OK (Name (Stmt));

               Insert_Action (N, Stmt, Suppress => All_Checks);
               Build_Allocate_Deallocate_Proc (Temp_Decl);
               Rewrite (N, New_Occurrence_Of (Temp, Loc));
               Analyze_And_Resolve (N, PtrT);

               Apply_Predicate_Check (N, Dtyp, Deref => True);
            end;

         --  Or else build the fully-fledged initialization if need be

         else
            --  For the task case, pass the Master_Id of the access type as
            --  the value of the _Master parameter, and _Chain as the value
            --  of the _Chain parameter (_Chain will be defined as part of
            --  the generated code for the allocator).

            --  In Ada 2005, the context may be a function that returns an
            --  anonymous access type. In that case the Master_Id has been
            --  created when expanding the function declaration.

            if Has_Task (Etyp) then
               if No (Master_Id (Base_Type (PtrT))) then
                  --  The designated type was an incomplete type, and the
                  --  access type did not get expanded. Salvage it now.

                  if Present (Declaration_Node (Base_Type (PtrT))) then
                     Expand_N_Full_Type_Declaration
                       (Declaration_Node (Base_Type (PtrT)));

                  --  When the allocator has a subtype indication then a
                  --  constraint is present and an itype has been added by
                  --  Analyze_Allocator as the subtype of this allocator.

                  --  If an allocator with constraints is called in the
                  --  return statement of a function returning a general
                  --  access type, then propagate to the itype the master
                  --  of the general access type (since it is the master
                  --  associated with the returned object).

                  elsif Is_Itype (PtrT)
                    and then Ekind (Current_Scope) = E_Function
                    and then
                      Ekind (Etype (Current_Scope)) = E_General_Access_Type
                    and then In_Return_Value (N)
                  then
                     Set_Master_Id (PtrT, Master_Id (Etype (Current_Scope)));

                  --  The only other possibility is an itype. For this
                  --  case, the master must exist in the context. This is
                  --  the case when the allocator initializes an access
                  --  component in an init-proc.

                  else
                     pragma Assert (Is_Itype (PtrT));
                     Build_Master_Renaming (PtrT, N);
                  end if;
               end if;

               --  If the context of the allocator is a declaration or an
               --  assignment, we can generate a meaningful image for the
               --  task even though subsequent assignments might remove the
               --  connection between task and entity. We build this image
               --  when the left-hand side is a simple variable, a simple
               --  indexed assignment or a simple selected component.

               if Nkind (Parent (N)) = N_Object_Declaration then
                  Target_Ref := Defining_Identifier (Parent (N));

               elsif Nkind (Parent (N)) = N_Assignment_Statement then
                  declare
                     Nam : constant Node_Id := Name (Parent (N));

                  begin
                     if Is_Entity_Name (Nam) then
                        Target_Ref := Nam;

                     elsif Nkind (Nam) in N_Indexed_Component
                                        | N_Selected_Component
                       and then Is_Entity_Name (Prefix (Nam))
                     then
                        Target_Ref := Nam;

                     else
                        Target_Ref := PtrT;
                     end if;
                  end;

               --  Otherwise we just pass the access type

               else
                  Target_Ref := PtrT;
               end if;

            --  Nothing to pass in the non-task case

            else
               Target_Ref := Empty;
            end if;

            Temp := Make_Temporary (Loc, 'P');

            if Is_Mutably_Tagged_Type (Dtyp) then
               Init_Stmts :=
                 Build_Default_Initialization (N, Etype (Etyp), Temp,
                   For_CW     => False,
                   Target_Ref => Target_Ref);
            else
               Init_Stmts :=
                 Build_Default_Initialization (N, Etyp, Temp,
                   For_CW     => Is_Class_Wide_Type (Dtyp),
                   Target_Ref => Target_Ref);
            end if;

            if Present (Init_Stmts) then
               --  We set the allocator as analyzed so that when we analyze
               --  the expression node, we do not get an unwanted recursive
               --  expansion of the allocator expression.

               Set_Analyzed (N);

               Temp_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (PtrT, Loc),
                   Expression          => Relocate_Node (N));

               Insert_Action (N, Temp_Decl, Suppress => All_Checks);

               --  If the designated type is a task type or contains tasks,
               --  create a specific block to activate the created tasks.

               if Has_Task (Etyp) then
                  declare
                     Actions : constant List_Id := New_List;

                  begin
                     Build_Task_Allocate_Block
                       (Actions, Relocate_Node (N), Init_Stmts);
                     Insert_Actions (N, Actions, Suppress => All_Checks);
                  end;

               else
                  Insert_Actions (N, Init_Stmts, Suppress => All_Checks);
               end if;

               Build_Allocate_Deallocate_Proc (Temp_Decl);
               Rewrite (N, New_Occurrence_Of (Temp, Loc));
               Analyze_And_Resolve (N, PtrT);

               Apply_Predicate_Check (N, Dtyp, Deref => True);

               --  When designated type has Default_Initial_Condition aspects,
               --  make a call to the type's DIC procedure to perform the
               --  checks. Theoretically this might also be needed for cases
               --  where the type doesn't have an init proc, but those should
               --  be very uncommon, and for now we only support the init proc
               --  case. ???

               if Has_DIC (Dtyp)
                 and then Present (DIC_Procedure (Dtyp))
                 and then not Has_Null_Body (DIC_Procedure (Dtyp))
               then
                  Insert_Action (N,
                                 Build_DIC_Call (Loc,
                                   Make_Explicit_Dereference (Loc,
                                     Prefix => New_Occurrence_Of (Temp, Loc)),
                                 Dtyp));
               end if;

               --  Ada 2005 (AI-251): Displace the pointer to reference the
               --  record component containing the secondary dispatch table
               --  of the interface type.

               if Is_Interface (Dtyp) then
                  Displace_Allocator_Pointer (N);
               end if;

            --  No initialization required

            else
               Build_Allocate_Deallocate_Proc (N);
            end if;
         end if;
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
      Loc  : constant Source_Ptr := Sloc (N);
      Par  : constant Node_Id    := Parent (N);
      Scop : constant Entity_Id  := Current_Scope;
      Typ  : constant Entity_Id  := Etype (N);

      function Is_Copy_Type (Typ : Entity_Id) return Boolean;
      --  Return True if we can copy objects of this type when expanding a case
      --  expression.

      function Is_Optimizable_Declaration (N : Node_Id) return Boolean;
      --  Return True if N is an object declaration that can be optimized

      ------------------
      -- Is_Copy_Type --
      ------------------

      function Is_Copy_Type (Typ : Entity_Id) return Boolean is
      begin
         return Is_Elementary_Type (Underlying_Type (Typ));
      end Is_Copy_Type;

      --------------------------------
      -- Is_Optimizable_Declaration --
      --------------------------------

      function Is_Optimizable_Declaration (N : Node_Id) return Boolean is
      begin
         return Nkind (N) = N_Object_Declaration
           and then not (Is_Entity_Name (Object_Definition (N))
                          and then Is_Class_Wide_Type
                                     (Entity (Object_Definition (N))))
           and then not Is_Return_Object (Defining_Identifier (N))
           and then not Is_Copy_Type (Typ);
      end Is_Optimizable_Declaration;

      --  Local variables

      Acts       : List_Id;
      Alt        : Node_Id;
      Case_Stmt  : Node_Id;
      Decl       : Node_Id;
      New_N      : Node_Id;
      Par_Obj    : Node_Id;
      Target     : Entity_Id := Empty;
      Target_Typ : Entity_Id;

      Optimize_Assignment_Stmt : Boolean := False;
      --  Small optimization: when the case expression appears in the context
      --  of a safe assignment statement, expand into

      --    case X is
      --       when A =>
      --          lhs := AX;
      --       when B =>
      --          lhs := BX;
      --       ...
      --    end case;

      --  This makes the expansion much more efficient in the context of an
      --  aggregate converted into assignments.

      Optimize_Return_Stmt : Boolean := False;
      --  Small optimization: when the case expression appears in the context
      --  of a simple return statement, expand into

      --    case X is
      --       when A =>
      --          return AX;
      --       when B =>
      --          return BX;
      --       ...
      --    end case;

      --  This makes the expansion much easier when expressions are calls to
      --  build-in-place functions.

      Optimize_Object_Decl : Boolean := False;
      --  Small optimization: when the case expression appears in the context
      --  of an object declaration of a type not Is_Copy_Type, expand into

      --    case X is
      --       when A =>
      --          then-obj : typ := then_expr;
      --          target :=  then-obj'Unrestricted_Access;
      --       when B =>
      --          else-obj : typ := else-expr;
      --          target :=  else-obj'Unrestricted_Access;
      --       ...
      --    end case
      --
      --    obj : typ renames target.all;

      --  This makes the expansion much easier when expressions are calls to
      --  build-in-place functions.

   --  Start of processing for Expand_N_Case_Expression

   begin
      --  If the expression is in the context of a simple return statement,
      --  possibly through intermediate conditional expressions, we delay
      --  expansion until the (immediate) parent is rewritten as a return
      --  statement (or is already the return statement). Likewise if it is
      --  in the context of an object declaration that can be optimized.

      if not Expansion_Delayed (N) then
         declare
            Uncond_Par : constant Node_Id := Unconditional_Parent (N);
         begin
            if Nkind (Uncond_Par) = N_Simple_Return_Statement
              or else Is_Optimizable_Declaration (Uncond_Par)
            then
               Delay_Conditional_Expressions_Between (N, Uncond_Par);
            end if;
         end;
      end if;

      --  If the expansion of the expression has been delayed, we wait for the
      --  rewriting of its parent as an assignment statement, or as as return
      --  statement or as an object declaration; when that's done, we optimize
      --  the assignment, return or declaration (the purpose of the delaying).

      if Expansion_Delayed (N) then
         if Nkind (Par) = N_Assignment_Statement then
            Optimize_Assignment_Stmt := True;

         elsif Nkind (Par) = N_Simple_Return_Statement then
            Optimize_Return_Stmt := True;

         elsif Is_Optimizable_Declaration (Par) then
            Optimize_Object_Decl := True;

         else
            return;
         end if;

         Set_Expansion_Delayed (N, False);
      end if;

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  If the case expression is a predicate specification, do not expand
      --  because it will need to be recognized and converted to the canonical
      --  predicate form later if it it happens to be static.

      if Ekind (Scop) in E_Function | E_Procedure
        and then Is_Predicate_Function (Scop)
        and then Is_Entity_Name (Expression (N))
        and then Entity (Expression (N)) = First_Entity (Scop)
        and then (Is_Scalar_Type (Etype (Expression (N)))
                   or else Is_String_Type (Etype (Expression (N))))
        and then not Has_Dynamic_Predicate_Aspect (Etype (Expression (N)))
      then
         return;
      end if;

      --  When the type of the case expression is elementary, expand

      --    (case X is when A => AX, when B => BX ...)

      --  into

      --    do
      --       Target : Typ;
      --       case X is
      --          when A =>
      --             <<actions>>
      --             Target := AX;
      --          when B =>
      --             <<actions>>
      --             Target := BX;
      --          ...
      --       end case;
      --    in Target end;

      --  In all other cases expand into

      --       type Ptr_Typ is not null access all Typ;
      --       Target : Ptr_Typ;
      --       case X is
      --          when A =>
      --             <<actions>>
      --             Target := AX'Unrestricted_Access;
      --          when B =>
      --             <<actions>>
      --             Target := BX'Unrestricted_Access;
      --          ...
      --       end case;

      --  and replace the case expression by a reference to Target.all.

      --  This approach avoids extra copies of potentially large objects. It
      --  also allows handling of values of limited or unconstrained types.

      Case_Stmt :=
        Make_Case_Statement (Loc,
          Expression   => Expression (N),
          Alternatives => New_List);

      --  Preserve the original context for which the case statement is being
      --  generated. This is needed by the finalization machinery to prevent
      --  the premature finalization of controlled objects found within the
      --  case statement.

      Set_From_Conditional_Expression (Case_Stmt);
      Acts := New_List;

      --  No need for Target_Typ in the case of statements

      if Optimize_Assignment_Stmt or else Optimize_Return_Stmt then
         Target_Typ := Empty;

      --  Scalar/Copy case

      elsif Is_Copy_Type (Typ) then
         Target_Typ := Typ;

      --  Otherwise create an access type to handle the general case using
      --  'Unrestricted_Access.

      --  Generate:
      --    type Ptr_Typ is not null access all [constant] Typ;

      else
         Target_Typ := Make_Temporary (Loc, 'P');

         Append_To (Acts,
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Target_Typ,
             Type_Definition     =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present            => True,
                 Null_Exclusion_Present => True,
                 Subtype_Indication     => New_Occurrence_Of (Typ, Loc),
                 Constant_Present       =>
                   Optimize_Object_Decl and then Constant_Present (Par))));
      end if;

      --  Create the declaration of the target which captures the value of the
      --  expression.

      --  Generate:
      --    Target : [Ptr_]Typ;

      if Optimize_Assignment_Stmt then
         Remove_Side_Effects (Name (Par), Name_Req => True);

      elsif not Optimize_Return_Stmt then
         Target := Make_Temporary (Loc, 'T');

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Target,
             Object_Definition   => New_Occurrence_Of (Target_Typ, Loc));
         Set_No_Initialization (Decl);

         Append_To (Acts, Decl);
      end if;

      --  Process the alternatives

      Alt := First (Alternatives (N));
      while Present (Alt) loop
         --  When the alternative's expression involves controlled function
         --  calls, generated temporaries are chained on the corresponding
         --  list of actions. These temporaries need to be finalized after
         --  the case expression is evaluated.

         Process_Transients_In_Expression (N, Actions (Alt));

         declare
            Alt_Loc  : constant Source_Ptr := Sloc (Expression (Alt));

            Alt_Expr : Node_Id := Relocate_Node (Expression (Alt));
            LHS      : Node_Id;
            Obj      : Node_Id;
            Stmts    : List_Id;

         begin
            --  Generate:
            --    lhs := AX;

            if Optimize_Assignment_Stmt then
               --  We directly copy the parent node to preserve its flags

               Stmts := New_List (New_Copy (Par));
               Set_Sloc       (First (Stmts), Alt_Loc);
               Set_Name       (First (Stmts), New_Copy_Tree (Name (Par)));
               Set_Expression (First (Stmts), Alt_Expr);

               --  If the expression is itself a conditional expression whose
               --  expansion has been delayed, analyze it again and expand it.

               if Is_Delayed_Conditional_Expression (Alt_Expr) then
                  Unanalyze_Delayed_Conditional_Expression (Alt_Expr);
               end if;

            --  Generate:
            --    return AX;

            elsif Optimize_Return_Stmt then
               Stmts := New_List (
                 Make_Simple_Return_Statement (Alt_Loc,
                   Expression => Alt_Expr));

               --  If the expression is itself a conditional expression whose
               --  expansion has been delayed, analyze it again and expand it.

               if Is_Delayed_Conditional_Expression (Alt_Expr) then
                  Unanalyze_Delayed_Conditional_Expression (Alt_Expr);
               end if;

            --  Generate:
            --    Obj : [constant] Typ := AX;
            --    Target := Obj'Unrestricted_Access;

            elsif Optimize_Object_Decl then
               Obj := Make_Temporary (Loc, 'C', Alt_Expr);

               Insert_Conditional_Object_Declaration (Obj, Alt_Expr, Par);

               Alt_Expr :=
                 Make_Attribute_Reference (Alt_Loc,
                   Prefix         => New_Occurrence_Of (Obj, Alt_Loc),
                   Attribute_Name => Name_Unrestricted_Access);

               LHS := New_Occurrence_Of (Target, Loc);
               Set_Assignment_OK (LHS);

               Stmts := New_List (
                 Make_Assignment_Statement (Alt_Loc,
                   Name       => LHS,
                   Expression => Alt_Expr));

            --  Take the unrestricted access of the expression value for non-
            --  scalar types. This approach avoids big copies and covers the
            --  limited and unconstrained cases.

            --  Generate:
            --    Target := AX'Unrestricted_Access;

            else
               if not Is_Copy_Type (Typ) then
                  --  It's possible that a call to Apply_Length_Check in
                  --  Resolve_Case_Expression rewrote the dependent expression
                  --  into a N_Raise_Constraint_Error. If that's the case, we
                  --  don't create a reference to Unrestricted_Access, but we
                  --  update the type of the N_Raise_Constraint_Error node.

                  if Nkind (Alt_Expr) in N_Raise_Constraint_Error then
                     Set_Etype (Alt_Expr, Target_Typ);
                  else
                     Alt_Expr :=
                       Make_Attribute_Reference (Alt_Loc,
                         Prefix         => Alt_Expr,
                         Attribute_Name => Name_Unrestricted_Access);
                  end if;
               end if;

               LHS := New_Occurrence_Of (Target, Loc);
               Set_Assignment_OK (LHS);

               Stmts := New_List (
                 Make_Assignment_Statement (Alt_Loc,
                   Name       => LHS,
                   Expression => Alt_Expr));
            end if;

            --  Propagate declarations inserted in the node by Insert_Actions
            --  (for example, temporaries generated to remove side effects).
            --  These actions must remain attached to the alternative, given
            --  that they are generated by the corresponding expression.

            if Present (Actions (Alt)) then
               Prepend_List (Actions (Alt), Stmts);
            end if;

            Append_To
              (Alternatives (Case_Stmt),
               Make_Case_Statement_Alternative (Sloc (Alt),
                 Discrete_Choices => Discrete_Choices (Alt),
                 Statements       => Stmts));
         end;

         Next (Alt);
      end loop;

      --  Rewrite the parent statement as a case statement

      if Optimize_Assignment_Stmt or else Optimize_Return_Stmt then
         Rewrite (Par, Case_Stmt);
         Analyze (Par);

      elsif Optimize_Object_Decl then
         Append_To (Acts, Case_Stmt);
         Insert_Actions (Par, Acts);

         New_N :=
           Make_Explicit_Dereference (Loc,
             Prefix => New_Occurrence_Of (Target, Loc));

         --  The renaming is not analyzed so complete the decoration of the
         --  object and set the type of the name directly.

         Par_Obj := Defining_Identifier (Par);
         if Constant_Present (Par) then
            Mutate_Ekind         (Par_Obj, E_Constant);
            Set_Is_True_Constant (Par_Obj);
         else
            Mutate_Ekind (Par_Obj, E_Variable);
         end if;

         Set_Etype (New_N, Etype (Par_Obj));

         Rewrite_Object_Declaration_As_Renaming (Par, New_N);

      --  Otherwise rewrite the case expression itself

      else
         Append_To (Acts, Case_Stmt);

         if Is_Copy_Type (Typ) then
            New_N :=
              Make_Expression_With_Actions (Loc,
                Expression => New_Occurrence_Of (Target, Loc),
                Actions    => Acts);

         else
            Insert_Actions (N, Acts);
            New_N :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Target, Loc));
         end if;

         Rewrite (N, New_N);
         Analyze_And_Resolve (N, Typ);
      end if;
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
      Acts : constant List_Id := Actions (N);

      procedure Force_Boolean_Evaluation (Expr : Node_Id);
      --  Force the evaluation of Boolean expression Expr

      ------------------------------
      -- Force_Boolean_Evaluation --
      ------------------------------

      procedure Force_Boolean_Evaluation (Expr : Node_Id) is
         Loc       : constant Source_Ptr := Sloc (N);
         Flag_Decl : Node_Id;
         Flag_Id   : Entity_Id;

      begin
         --  Relocate the expression to the actions list by capturing its value
         --  in a Boolean flag. Generate:
         --    Flag : constant Boolean := Expr;

         Flag_Id := Make_Temporary (Loc, 'F');

         Flag_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flag_Id,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
             Expression          => Relocate_Node (Expr));

         Append (Flag_Decl, Acts);
         Analyze (Flag_Decl);

         --  Replace the expression with a reference to the flag

         Rewrite (Expression (N), New_Occurrence_Of (Flag_Id, Loc));
         Analyze (Expression (N));
      end Force_Boolean_Evaluation;

   --  Start of processing for Expand_N_Expression_With_Actions

   begin
      --  Do not evaluate the expression when it denotes an entity because the
      --  expression_with_actions node will be replaced by the reference.

      if Is_Entity_Name (Expression (N)) then
         null;

      --  Do not evaluate the expression when there are no actions because the
      --  expression_with_actions node will be replaced by the expression.

      elsif Is_Empty_List (Acts) then
         null;

      --  Force the evaluation of the expression by capturing its value in a
      --  temporary. This ensures that aliases of transient objects do not leak
      --  to the expression of the expression_with_actions node:

      --    do
      --       Trans_Id : Ctrl_Typ := ...;
      --       Alias : ... := Trans_Id;
      --    in ... Alias ... end;

      --  In the example above, Trans_Id cannot be finalized at the end of the
      --  actions list because this may affect the alias and the final value of
      --  the expression_with_actions. Forcing the evaluation encapsulates the
      --  reference to the Alias within the actions list:

      --    do
      --       Trans_Id : Ctrl_Typ := ...;
      --       Alias : ... := Trans_Id;
      --       Val : constant Boolean := ... Alias ...;
      --       <finalize Trans_Id>
      --    in Val end;

      --  Once this transformation is performed, it is safe to finalize the
      --  transient object at the end of the actions list.

      --  Note that Force_Evaluation does not remove side effects in operators
      --  because it assumes that all operands are evaluated and side effect
      --  free. This is not the case when an operand depends implicitly on the
      --  transient object through the use of access types.

      elsif Is_Boolean_Type (Etype (Expression (N))) then
         Force_Boolean_Evaluation (Expression (N));

      --  The expression of an expression_with_actions node may not necessarily
      --  be Boolean when the node appears in an if expression. In this case do
      --  the usual forced evaluation to encapsulate potential aliasing.

      else
         --  A check is also needed since the subtype of the EWA node and the
         --  subtype of the expression may differ (for example, the EWA node
         --  may have a null-excluding access subtype).

         Apply_Constraint_Check (Expression (N), Etype (N));
         Force_Evaluation (Expression (N));
      end if;

      --  Process transient objects found within the actions of the EWA node

      Process_Transients_In_Expression (N, Acts);

      --  Deal with case where there are no actions. In this case we simply
      --  rewrite the node with its expression since we don't need the actions
      --  and the specification of this node does not allow a null action list.

      --  Note: we use Rewrite instead of Replace, because Codepeer is using
      --  the expanded tree and relying on being able to retrieve the original
      --  tree in cases like this. This raises a whole lot of issues of whether
      --  we have problems elsewhere, which will be addressed in the future???

      if Is_Empty_List (Acts) then
         Rewrite (N, Relocate_Node (Expression (N)));
      end if;
   end Expand_N_Expression_With_Actions;

   ----------------------------
   -- Expand_N_If_Expression --
   ----------------------------

   --  Deal with limited types and condition actions

   procedure Expand_N_If_Expression (N : Node_Id) is
      Cond  : constant Node_Id    := First (Expressions (N));
      Loc   : constant Source_Ptr := Sloc (N);
      Thenx : constant Node_Id    := Next (Cond);
      Elsex : constant Node_Id    := Next (Thenx);
      Par   : constant Node_Id    := Parent (N);
      Typ   : constant Entity_Id  := Etype (N);

      Force_Expand : constant Boolean := Is_Anonymous_Access_Actual (N);
      --  Determine if we are dealing with a special case of a conditional
      --  expression used as an actual for an anonymous access type which
      --  forces us to transform the if expression into an expression with
      --  actions in order to create a temporary to capture the level of the
      --  expression in each branch.

      function Is_Copy_Type (Typ : Entity_Id) return Boolean;
      --  Return True if we can copy objects of this type when expanding an if
      --  expression.

      function Is_Optimizable_Declaration (N : Node_Id) return Boolean;
      --  Return True if N is an object declaration that can be optimized

      function OK_For_Single_Subtype (T1, T2 : Entity_Id) return Boolean;
      --  Return true if it is acceptable to use a single subtype for two
      --  dependent expressions of subtype T1 and T2 respectively, which are
      --  unidimensional arrays whose index bounds are known at compile time.

      ------------------
      -- Is_Copy_Type --
      ------------------

      function Is_Copy_Type (Typ : Entity_Id) return Boolean is
         Utyp : constant Entity_Id := Underlying_Type (Typ);

      begin
         return Is_Definite_Subtype (Utyp)
           and then not Is_By_Reference_Type (Utyp);
      end Is_Copy_Type;

      --------------------------------
      -- Is_Optimizable_Declaration --
      --------------------------------

      function Is_Optimizable_Declaration (N : Node_Id) return Boolean is
      begin
         return Nkind (N) = N_Object_Declaration
           and then not (Is_Entity_Name (Object_Definition (N))
                          and then Is_Class_Wide_Type
                                     (Entity (Object_Definition (N))))
           and then not Is_Return_Object (Defining_Identifier (N))
           and then not Is_Copy_Type (Typ);
      end Is_Optimizable_Declaration;

      ---------------------------
      -- OK_For_Single_Subtype --
      ---------------------------

      function OK_For_Single_Subtype (T1, T2 : Entity_Id) return Boolean is
         Lo1, Hi1 : Uint;
         Lo2, Hi2 : Uint;

      begin
         Get_First_Index_Bounds (T1, Lo1, Hi1);
         Get_First_Index_Bounds (T2, Lo2, Hi2);

         --  Return true if the length of the covering subtype is not too large

         return
           UI_Max (Hi1, Hi2) - UI_Min (Lo1, Lo2) < Too_Large_Length_For_Array;
      end OK_For_Single_Subtype;

      --  Local variables

      Actions  : List_Id;
      Decl     : Node_Id;
      Expr     : Node_Id;
      If_Stmt  : Node_Id;
      New_Else : Node_Id;
      New_N    : Node_Id;
      New_Then : Node_Id;

      Optimize_Assignment_Stmt : Boolean := False;
      --  Small optimization: when the if expression appears in the context of
      --  a safe assignment statement, expand into

      --    if cond then
      --       lhs := then-expr;
      --    else
      --       lhs := else-expr;
      --    end if;

      --  This makes the expansion much more efficient in the context of an
      --  aggregate converted into assignments.

      Optimize_Return_Stmt : Boolean := False;
      --  Small optimization: when the if expression appears in the context of
      --  a simple return statement, expand into

      --    if cond then
      --       return then-expr;
      --    else
      --       return else-expr;
      --    end if;

      --  This makes the expansion much easier when expressions are calls to
      --  build-in-place functions.

      Optimize_Object_Decl : Boolean := False;
      --  Small optimization: when the if expression appears in the context of
      --  an object declaration of a type not Is_Copy_Type, expand into

      --    if cond then
      --       then-obj : typ := then_expr;
      --       target :=  then-obj'Unrestricted_Access;
      --    else
      --       else-obj : typ := else-expr;
      --       target :=  else-obj'Unrestricted_Access;
      --    end if;
      --
      --    obj : typ renames target.all;

      --  This makes the expansion much easier when expressions are calls to
      --  build-in-place functions.

   --  Start of processing for Expand_N_If_Expression

   begin
      --  If the expression is in the context of a simple return statement,
      --  possibly through intermediate conditional expressions, we delay
      --  expansion until the (immediate) parent is rewritten as a return
      --  statement (or is already the return statement). Likewise if it is
      --  in the context of an object declaration that can be optimized.
      --  Note that this deals with the case of the elsif part of the if
      --  expression, if it exists.

      if not Expansion_Delayed (N) then
         declare
            Uncond_Par : constant Node_Id := Unconditional_Parent (N);
         begin
            if Nkind (Uncond_Par) = N_Simple_Return_Statement
              or else Is_Optimizable_Declaration (Uncond_Par)
            then
               Delay_Conditional_Expressions_Between (N, Uncond_Par);
            end if;
         end;
      end if;

      --  If the expansion of the expression has been delayed, we wait for the
      --  rewriting of its parent as an assignment statement, or as as return
      --  statement or as an object declaration; when that's done, we optimize
      --  the assignment, return or declaration (the purpose of the delaying).

      if Expansion_Delayed (N) then
         if Nkind (Par) = N_Assignment_Statement then
            Optimize_Assignment_Stmt := True;

         elsif Nkind (Par) = N_Simple_Return_Statement then
            Optimize_Return_Stmt := True;

         elsif Is_Optimizable_Declaration (Par) then
            Optimize_Object_Decl := True;

         else
            return;
         end if;

         Set_Expansion_Delayed (N, False);
      end if;

      --  Deal with non-standard booleans

      Adjust_Condition (Cond);

      --  Check for MINIMIZED/ELIMINATED overflow mode.
      --  Apply_Arithmetic_Overflow_Check will not deal with Then/Else_Actions
      --  so skip this step if any actions are present.

      if Minimized_Eliminated_Overflow_Check (N)
        and then No (Then_Actions (N))
        and then No (Else_Actions (N))
      then
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
            Expr    := Relocate_Node (Thenx);
            Actions := Then_Actions (N);
         else
            Expr    := Relocate_Node (Elsex);
            Actions := Else_Actions (N);
         end if;

         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Transients_In_Expression (N, Actions);

         --  If the expression is itself a conditional expression whose
         --  expansion has been delayed, analyze it again and expand it.

         if Is_Delayed_Conditional_Expression (Expr) then
            Unanalyze_Delayed_Conditional_Expression (Expr);
         end if;

         Insert_Actions (N, Actions);
         Rewrite (N, Expr);
         Analyze_And_Resolve (N, Typ);

         --  Note that the result is never static (legitimate cases of
         --  static if expressions were folded in Sem_Eval).

         Set_Is_Static_Expression (N, False);
         return;

      --  Build an if statement assigning each dependent expression to the
      --  target separately. The (main) use case is aggregate elaboration.

      elsif Optimize_Assignment_Stmt then
         Remove_Side_Effects (Name (Par), Name_Req => True);

         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Transients_In_Expression (N, Then_Actions (N));
         Process_Transients_In_Expression (N, Else_Actions (N));

         --  We directly copy the parent node to preserve its flags

         New_Then := New_Copy (Par);
         Set_Sloc       (New_Then, Sloc (Thenx));
         Set_Name       (New_Then, New_Copy_Tree (Name (Par)));
         Set_Expression (New_Then, Relocate_Node (Thenx));

         --  If the expression is itself a conditional expression whose
         --  expansion has been delayed, analyze it again and expand it.

         if Is_Delayed_Conditional_Expression (Expression (New_Then)) then
            Unanalyze_Delayed_Conditional_Expression (Expression (New_Then));
         end if;

         New_Else := New_Copy (Par);
         Set_Sloc       (New_Else, Sloc (Elsex));
         Set_Name       (New_Else, New_Copy_Tree (Name (Par)));
         Set_Expression (New_Else, Relocate_Node (Elsex));

         if Is_Delayed_Conditional_Expression (Expression (New_Else)) then
            Unanalyze_Delayed_Conditional_Expression (Expression (New_Else));
         end if;

         If_Stmt :=
           Make_Implicit_If_Statement (N,
             Condition       => Relocate_Node (Cond),
             Then_Statements => New_List (New_Then),
             Else_Statements => New_List (New_Else));
         Decl  := Empty;
         New_N := Empty;

         --  Preserve the original context for which the if statement is
         --  being generated. This is needed by the finalization machinery
         --  to prevent the premature finalization of controlled objects
         --  found within the if statement.

         Set_From_Conditional_Expression (If_Stmt);

      --  Build an if statement returning each dependent expression from the
      --  function separately. The main use case is expression function.

      elsif Optimize_Return_Stmt then
         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Transients_In_Expression (N, Then_Actions (N));
         Process_Transients_In_Expression (N, Else_Actions (N));

         New_Then := Relocate_Node (Thenx);

         --  If the expression is itself a conditional expression whose
         --  expansion has been delayed, analyze it again and expand it.

         if Is_Delayed_Conditional_Expression (New_Then) then
            Unanalyze_Delayed_Conditional_Expression (New_Then);
         end if;

         New_Else := Relocate_Node (Elsex);

         --  If the expression is itself a conditional expression whose
         --  expansion has been delayed, analyze it again and expand it.

         if Is_Delayed_Conditional_Expression (New_Else) then
            Unanalyze_Delayed_Conditional_Expression (New_Else);
         end if;

         If_Stmt :=
           Make_Implicit_If_Statement (N,
             Condition       => Relocate_Node (Cond),
             Then_Statements => New_List (
               Make_Simple_Return_Statement (Sloc (New_Then),
                 Expression => New_Then)),
             Else_Statements => New_List (
               Make_Simple_Return_Statement (Sloc (New_Else),
                 Expression => New_Else)));
         Decl  := Empty;
         New_N := Empty;

         --  Preserve the original context for which the if statement is
         --  being generated. This is needed by the finalization machinery
         --  to prevent the premature finalization of controlled objects
         --  found within the if statement.

         Set_From_Conditional_Expression (If_Stmt);

      elsif Optimize_Object_Decl then
         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Transients_In_Expression (N, Then_Actions (N));
         Process_Transients_In_Expression (N, Else_Actions (N));

         declare
            Par_Obj  : constant Entity_Id := Defining_Identifier (Par);
            Then_Obj : constant Entity_Id := Make_Temporary (Loc, 'C', Thenx);
            Else_Obj : constant Entity_Id := Make_Temporary (Loc, 'C', Elsex);
            Ptr_Typ  : constant Entity_Id := Make_Temporary (Loc, 'A');
            Target   : constant Entity_Id := Make_Temporary (Loc, 'C', N);

         begin
            Insert_Conditional_Object_Declaration (Then_Obj, Thenx, Par);
            Insert_Conditional_Object_Declaration (Else_Obj, Elsex, Par);

            --  Generate:
            --    type Ptr_Typ is not null access all [constant] Typ;

            Insert_Action (Par,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Ptr_Typ,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present            => True,
                    Null_Exclusion_Present => True,
                    Subtype_Indication     => New_Occurrence_Of (Typ, Loc),
                    Constant_Present       => Constant_Present (Par))));

            --  Generate:
            --    Target : Ptr_Typ;

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Target,
                Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc));
            Set_No_Initialization (Decl);
            Insert_Action (Par, Decl);

            --  Generate:
            --    if Cond then
            --       Target := <Then_Obj>'Unrestricted_Access;
            --    else
            --       Target := <Else_Obj>'Unrestricted_Access;
            --    end if;

            If_Stmt :=
              Make_Implicit_If_Statement (N,
                Condition       => Relocate_Node (Cond),
                Then_Statements => New_List (
                  Make_Assignment_Statement (Sloc (Thenx),
                    Name       => New_Occurrence_Of (Target, Sloc (Thenx)),
                    Expression =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Then_Obj, Loc),
                        Attribute_Name => Name_Unrestricted_Access))),

                Else_Statements => New_List (
                  Make_Assignment_Statement (Sloc (Elsex),
                    Name       => New_Occurrence_Of (Target, Sloc (Elsex)),
                    Expression =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => New_Occurrence_Of (Else_Obj, Loc),
                        Attribute_Name => Name_Unrestricted_Access))));

            --  Preserve the original context for which the if statement is
            --  being generated. This is needed by the finalization machinery
            --  to prevent the premature finalization of controlled objects
            --  found within the if statement.

            Set_From_Conditional_Expression (If_Stmt);

            New_N :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Target, Loc));

            --  The renaming is not analyzed so complete the decoration of the
            --  object and set the type of the name directly.

            if Constant_Present (Par) then
               Mutate_Ekind         (Par_Obj, E_Constant);
               Set_Is_True_Constant (Par_Obj);
            else
               Mutate_Ekind (Par_Obj, E_Variable);
            end if;

            Set_Etype (New_N, Etype (Par_Obj));
         end;

      --  If the result is a unidimensional unconstrained array but the two
      --  dependent expressions have constrained subtypes with known bounds,
      --  then we expand as follows:

      --      subtype Txx is Typ (<static low-bound> .. <static high-bound>);
      --      Cnn : Txx;
      --      if cond then
      --         <<then actions>>
      --         Cnn (<then low-bound .. then high-bound>) := then-expr;
      --      else
      --         <<else actions>>
      --         Cnn (<else low bound .. else high-bound>) := else-expr;
      --      end if;

      --  and replace the if expression by a slice of Cnn, provided that Txx
      --  is not too large. This will create a static temporary instead of the
      --  dynamic one of the next case and thus help the code generator.

      --  Note that we need to deal with the case where the else expression is
      --  itself such a slice, in order to catch if expressions with more than
      --  two dependent expressions in the source code.

      --  Also note that this creates variables on branches without an explicit
      --  scope, causing troubles with e.g. the LLVM IR, so disable this
      --  optimization when Unnest_Subprogram_Mode (enabled for LLVM).

      elsif Is_Array_Type (Typ)
        and then Number_Dimensions (Typ) = 1
        and then not Is_Constrained (Typ)
        and then not Is_By_Reference_Type (Typ)
        and then Is_Constrained (Etype (Thenx))
        and then Compile_Time_Known_Bounds (Etype (Thenx))
        and then
          ((Is_Constrained (Etype (Elsex))
             and then Compile_Time_Known_Bounds (Etype (Elsex))
             and then OK_For_Single_Subtype (Etype (Thenx), Etype (Elsex)))
            or else
           (Nkind (Elsex) = N_Slice
             and then Is_Constrained (Etype (Prefix (Elsex)))
             and then Compile_Time_Known_Bounds (Etype (Prefix (Elsex)))
             and then
               OK_For_Single_Subtype (Etype (Thenx), Etype (Prefix (Elsex)))))
        and then not Unnest_Subprogram_Mode
      then
         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Transients_In_Expression (N, Then_Actions (N));
         Process_Transients_In_Expression (N, Else_Actions (N));

         declare
            Ityp : constant Entity_Id := Base_Type (Etype (First_Index (Typ)));

            function Build_New_Bound
              (Then_Bnd  : Uint;
               Else_Bnd  : Uint;
               Slice_Bnd : Node_Id) return Node_Id;
            --  Build a new bound from the bounds of the if expression

            function To_Ityp (V : Uint) return Node_Id;
            --  Convert V to an index value in Ityp

            ---------------------
            -- Build_New_Bound --
            ---------------------

            function Build_New_Bound
              (Then_Bnd  : Uint;
               Else_Bnd  : Uint;
               Slice_Bnd : Node_Id) return Node_Id is

            begin
               --  We need to use the special processing for slices only if
               --  they do not have compile-time known bounds; if they do, they
               --  can be treated like any other expressions.

               if Nkind (Elsex) = N_Slice
                 and then not Compile_Time_Known_Bounds (Etype (Elsex))
               then
                  if Compile_Time_Known_Value (Slice_Bnd)
                    and then Expr_Value (Slice_Bnd) = Then_Bnd
                  then
                     return To_Ityp (Then_Bnd);

                  else
                     return Make_If_Expression (Loc,
                       Expressions => New_List (
                         Duplicate_Subexpr (Cond),
                         To_Ityp (Then_Bnd),
                         New_Copy_Tree (Slice_Bnd)));
                  end if;

               elsif Then_Bnd = Else_Bnd then
                  return To_Ityp (Then_Bnd);

               else
                  return Make_If_Expression (Loc,
                    Expressions => New_List (
                      Duplicate_Subexpr (Cond),
                      To_Ityp (Then_Bnd),
                      To_Ityp (Else_Bnd)));
               end if;
            end Build_New_Bound;

            -------------
            -- To_Ityp --
            -------------

            function To_Ityp (V : Uint) return Node_Id is
               Result : constant Node_Id := Make_Integer_Literal (Loc, V);

            begin
               if Is_Enumeration_Type (Ityp) then
                  return
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Ityp, Loc),
                      Attribute_Name => Name_Val,
                      Expressions    => New_List (Result));
               else
                  return Result;
               end if;
            end To_Ityp;

            Ent                  : Node_Id;
            Slice_Lo, Slice_Hi   : Node_Id;
            Subtyp_Ind           : Node_Id;
            Else_Lo, Else_Hi     : Uint;
            Min_Lo, Max_Hi       : Uint;
            Then_Lo, Then_Hi     : Uint;
            Then_List, Else_List : List_Id;

         begin
            Get_First_Index_Bounds (Etype (Thenx), Then_Lo, Then_Hi);

            --  See the rationale in Build_New_Bound

            if Nkind (Elsex) = N_Slice
              and then not Compile_Time_Known_Bounds (Etype (Elsex))
            then
               Slice_Lo := Low_Bound (Discrete_Range (Elsex));
               Slice_Hi := High_Bound (Discrete_Range (Elsex));
               Get_First_Index_Bounds
                 (Etype (Prefix (Elsex)), Else_Lo, Else_Hi);

            else
               Slice_Lo := Empty;
               Slice_Hi := Empty;
               Get_First_Index_Bounds (Etype (Elsex), Else_Lo, Else_Hi);
            end if;

            Min_Lo := UI_Min (Then_Lo, Else_Lo);
            Max_Hi := UI_Max (Then_Hi, Else_Hi);

            --  Now we construct an array object with appropriate bounds and
            --  mark it as internal to prevent useless initialization when
            --  Initialize_Scalars is enabled. Also since this is the actual
            --  result entity, we make sure we have debug information for it.

            Subtyp_Ind :=
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Occurrence_Of (Typ, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => To_Ityp (Min_Lo),
                        High_Bound => To_Ityp (Max_Hi)))));

            Ent := Make_Temporary (Loc, 'C');
            Set_Is_Internal       (Ent);
            Set_Debug_Info_Needed (Ent);

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Ent,
                Object_Definition   => Subtyp_Ind);

            --  If the result of the expression appears as the initializing
            --  expression of an object declaration, we can just rename the
            --  result, rather than copying it.

            Mutate_Ekind (Ent, E_Variable);
            Set_OK_To_Rename (Ent);

            Then_List := New_List (
              Make_Assignment_Statement (Loc,
                Name       =>
                  Make_Slice (Loc,
                    Prefix         => New_Occurrence_Of (Ent, Loc),
                    Discrete_Range =>
                      Make_Range (Loc,
                        Low_Bound  => To_Ityp (Then_Lo),
                        High_Bound => To_Ityp (Then_Hi))),
                Expression => Relocate_Node (Thenx)));

            Set_Suppress_Assignment_Checks (Last (Then_List));

            --  See the rationale in Build_New_Bound

            if Nkind (Elsex) = N_Slice
              and then not Compile_Time_Known_Bounds (Etype (Elsex))
            then
               Else_List := New_List (
                 Make_Assignment_Statement (Loc,
                   Name       =>
                     Make_Slice (Loc,
                       Prefix         => New_Occurrence_Of (Ent, Loc),
                       Discrete_Range =>
                         Make_Range (Loc,
                           Low_Bound  => New_Copy_Tree (Slice_Lo),
                           High_Bound => New_Copy_Tree (Slice_Hi))),
                   Expression => Relocate_Node (Elsex)));

            else
               Else_List := New_List (
                 Make_Assignment_Statement (Loc,
                   Name       =>
                     Make_Slice (Loc,
                       Prefix         => New_Occurrence_Of (Ent, Loc),
                       Discrete_Range =>
                         Make_Range (Loc,
                           Low_Bound  => To_Ityp (Else_Lo),
                           High_Bound => To_Ityp (Else_Hi))),
                   Expression => Relocate_Node (Elsex)));
            end if;

            Set_Suppress_Assignment_Checks (Last (Else_List));

            If_Stmt :=
              Make_Implicit_If_Statement (N,
                Condition       => Duplicate_Subexpr (Cond),
                Then_Statements => Then_List,
                Else_Statements => Else_List);

            New_N :=
              Make_Slice (Loc,
                Prefix         => New_Occurrence_Of (Ent, Loc),
                Discrete_Range => Make_Range (Loc,
                  Low_Bound  => Build_New_Bound (Then_Lo, Else_Lo, Slice_Lo),
                  High_Bound => Build_New_Bound (Then_Hi, Else_Hi, Slice_Hi)));
         end;

      --  If the type cannot be copied, then we expand as follows to avoid the
      --  possibility of improper copying.

      --      type Ptr_Typ is not null access all Typ;
      --      Target : Ptr;
      --      if cond then
      --         <<then actions>>
      --         Target := then-expr'Unrestricted_Access;
      --      else
      --         <<else actions>>
      --         Target := else-expr'Unrestricted_Access;
      --      end if;

      --  and replace the if expression by a reference to Target.all.

      elsif not Is_Copy_Type (Typ) then
         --  When the "then" or "else" expressions involve controlled function
         --  calls, generated temporaries are chained on the corresponding list
         --  of actions. These temporaries need to be finalized after the if
         --  expression is evaluated.

         Process_Transients_In_Expression (N, Then_Actions (N));
         Process_Transients_In_Expression (N, Else_Actions (N));

         declare
            Ptr_Typ : constant Entity_Id := Make_Temporary (Loc, 'A');
            Target  : constant Entity_Id := Make_Temporary (Loc, 'C', N);

         begin
            --  Generate:
            --    type Ptr_Typ is not null access all Typ;

            Insert_Action (N,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Ptr_Typ,
                Type_Definition     =>
                  Make_Access_To_Object_Definition (Loc,
                    All_Present            => True,
                    Null_Exclusion_Present => True,
                    Subtype_Indication     => New_Occurrence_Of (Typ, Loc))));

            --  Generate:
            --    Target : Ptr_Typ;

            Decl :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Target,
                Object_Definition   => New_Occurrence_Of (Ptr_Typ, Loc));
            Set_No_Initialization (Decl);

            --  Generate:
            --    if Cond then
            --       Target := <Thenx>'Unrestricted_Access;
            --    else
            --       Target := <Elsex>'Unrestricted_Access;
            --    end if;

            If_Stmt :=
              Make_Implicit_If_Statement (N,
                Condition       => Relocate_Node (Cond),
                Then_Statements => New_List (
                  Make_Assignment_Statement (Sloc (Thenx),
                    Name       => New_Occurrence_Of (Target, Sloc (Thenx)),
                    Expression =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => Relocate_Node (Thenx),
                        Attribute_Name => Name_Unrestricted_Access))),

                Else_Statements => New_List (
                  Make_Assignment_Statement (Sloc (Elsex),
                    Name       => New_Occurrence_Of (Target, Sloc (Elsex)),
                    Expression =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => Relocate_Node (Elsex),
                        Attribute_Name => Name_Unrestricted_Access))));

            --  Preserve the original context for which the if statement is
            --  being generated. This is needed by the finalization machinery
            --  to prevent the premature finalization of controlled objects
            --  found within the if statement.

            Set_From_Conditional_Expression (If_Stmt);

            New_N :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Target, Loc));
         end;

      --  For other types, we only need to expand if there are other actions
      --  associated with either branch or we need to force expansion to deal
      --  with if expressions used as an actual of an anonymous access type.

      elsif Present (Then_Actions (N))
        or else Present (Else_Actions (N))
        or else Force_Expand
      then
         --  We now wrap the actions into the appropriate expression

         --  We do not need to call Process_Transients_In_Expression on
         --  the list of actions in this case, because the expansion of
         --  Expression_With_Actions will do it.

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

         --  We must force expansion into an expression with actions when
         --  an if expression gets used directly as an actual for an
         --  anonymous access type.

         if Force_Expand then
            declare
               Cnn  : constant Entity_Id := Make_Temporary (Loc, 'C');
               Acts : List_Id;
            begin
               Acts := New_List;

               --  Generate:
               --    Cnn : Ann;

               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Cnn,
                   Object_Definition   => New_Occurrence_Of (Typ, Loc));
               Append_To (Acts, Decl);

               Set_No_Initialization (Decl);

               --  Generate:
               --    if Cond then
               --       Cnn := <Thenx>;
               --    else
               --       Cnn := <Elsex>;
               --    end if;

               If_Stmt :=
                 Make_Implicit_If_Statement (N,
                   Condition       => Relocate_Node (Cond),
                   Then_Statements => New_List (
                     Make_Assignment_Statement (Sloc (Thenx),
                       Name       => New_Occurrence_Of (Cnn, Sloc (Thenx)),
                       Expression => Relocate_Node (Thenx))),

                   Else_Statements => New_List (
                     Make_Assignment_Statement (Sloc (Elsex),
                       Name       => New_Occurrence_Of (Cnn, Sloc (Elsex)),
                       Expression => Relocate_Node (Elsex))));
               Append_To (Acts, If_Stmt);

               --  Generate:
               --    do
               --       ...
               --    in Cnn end;

               Rewrite (N,
                 Make_Expression_With_Actions (Loc,
                   Expression => New_Occurrence_Of (Cnn, Loc),
                   Actions    => Acts));
               Analyze_And_Resolve (N, Typ);
            end;
         end if;

         return;

      --  For the sake of GNATcoverage, generate an intermediate temporary in
      --  the case where the if expression is a condition in an outer decision,
      --  in order to make sure that no branch is shared between the decisions.

      elsif Opt.Suppress_Control_Flow_Optimizations
        and then Nkind (Original_Node (Par)) in N_Case_Expression
                                              | N_Case_Statement
                                              | N_If_Expression
                                              | N_If_Statement
                                              | N_Goto_When_Statement
                                              | N_Loop_Statement
                                              | N_Return_When_Statement
                                              | N_Short_Circuit
      then
         declare
            Cnn  : constant Entity_Id := Make_Temporary (Loc, 'C');
            Acts : List_Id;

         begin
            --  Generate:
            --    do
            --       Cnn : constant Typ := N;
            --    in Cnn end

            Acts := New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Cnn,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Typ, Loc),
                Expression          => Relocate_Node (N)));

            Rewrite (N,
              Make_Expression_With_Actions (Loc,
                Expression => New_Occurrence_Of (Cnn, Loc),
                Actions    => Acts));

            Analyze_And_Resolve (N, Typ);
            return;
         end;

      --  If no actions then no expansion needed, gigi will handle it using the
      --  same approach as a C conditional expression.

      else
         return;
      end if;

      --  Fall through here for either the limited expansion, or the case of
      --  inserting actions for nonlimited types. In both these cases, we must
      --  move the SLOC of the parent If statement to the newly created one and
      --  change it to the SLOC of the expression which, after expansion, will
      --  correspond to what is being evaluated.

      if Present (Par) and then Nkind (Par) = N_If_Statement then
         Set_Sloc (If_Stmt, Sloc (Par));
         Set_Sloc (Par, Loc);
      end if;

      --  Move Then_Actions and Else_Actions, if any, to the new if statement

      if Present (Then_Actions (N)) then
         Prepend_List (Then_Actions (N), Then_Statements (If_Stmt));
      end if;

      if Present (Else_Actions (N)) then
         Prepend_List (Else_Actions (N), Else_Statements (If_Stmt));
      end if;

      --  Rewrite the parent statement as an if statement

      if Optimize_Assignment_Stmt or else Optimize_Return_Stmt then
         Rewrite (Par, If_Stmt);
         Analyze (Par);

      elsif Optimize_Object_Decl then
         Insert_Action (Par, If_Stmt);
         Rewrite_Object_Declaration_As_Renaming (Par, New_N);

      --  Otherwise rewrite the if expression itself

      else
         Insert_Action (N, Decl);
         Insert_Action (N, If_Stmt);
         Rewrite (N, New_N);
         Analyze_And_Resolve (N, Typ);
      end if;
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

      procedure Substitute_Valid_Test;
      --  Replaces node N by Lop'Valid. This is done when we have an explicit
      --  test for the left operand being in range of its subtype.

      ---------------------------
      -- Substitute_Valid_Test --
      ---------------------------

      procedure Substitute_Valid_Test is
         function Is_OK_Object_Reference (Nod : Node_Id) return Boolean;
         --  Determine whether arbitrary node Nod denotes a source object that
         --  may safely act as prefix of attribute 'Valid.

         ----------------------------
         -- Is_OK_Object_Reference --
         ----------------------------

         function Is_OK_Object_Reference (Nod : Node_Id) return Boolean is
            Obj_Ref : constant Node_Id := Original_Node (Nod);
            --  The original operand

         begin
            --  The object reference must be a source construct, otherwise the
            --  codefix suggestion may refer to nonexistent code from a user
            --  perspective.

            return Comes_From_Source (Obj_Ref)
              and then Is_Object_Reference (Unqual_Conv (Obj_Ref));
         end Is_OK_Object_Reference;

      --  Start of processing for Substitute_Valid_Test

      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => Relocate_Node (Lop),
             Attribute_Name => Name_Valid));

         Analyze_And_Resolve (N, Restyp);

         --  Emit a warning when the left-hand operand of the membership test
         --  is a source object, otherwise the use of attribute 'Valid would be
         --  illegal. The warning is not given when overflow checking is either
         --  MINIMIZED or ELIMINATED, as the danger of optimization has been
         --  eliminated above.

         if Is_OK_Object_Reference (Lop)
           and then Overflow_Check_Mode not in Minimized_Or_Eliminated
         then
            Error_Msg_N
              ("??explicit membership test may be optimized away", N);
            Error_Msg_N -- CODEFIX
              ("\??use ''Valid attribute instead", N);
         end if;
      end Substitute_Valid_Test;

      --  Local variables

      Ltyp : Entity_Id;
      Rtyp : Entity_Id;

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

      if Minimized_Eliminated_Overflow_Check (Left_Opnd  (N))
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

        and then Is_Entity_Name (Rop)
        and then Ltyp = Entity (Rop)

        --  Skip this for predicated types, where such expressions are a
        --  reasonable way of testing if something meets the predicate.

        and then No (Predicate_Function (Ltyp))
      then
         Substitute_Valid_Test;
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

            Lo_Orig  : constant Node_Id := Original_Node (Lo);
            Hi_Orig  : constant Node_Id := Original_Node (Hi);
            Rop_Orig : constant Node_Id := Original_Node (Rop);

            Comes_From_Simple_Range_In_Source : constant Boolean :=
              Comes_From_Source (N)
                and then not
                  (Is_Entity_Name (Rop_Orig)
                    and then Is_Type (Entity (Rop_Orig))
                    and then Present (Predicate_Function (Entity (Rop_Orig))));
            --  This is true for a membership test present in the source with a
            --  range or mark for a subtype that is not predicated. As already
            --  explained a few lines above, we do not want to give warnings on
            --  a test with a mark for a subtype that is predicated.

            Warn : constant Boolean :=
                      Constant_Condition_Warnings
                        and then Comes_From_Simple_Range_In_Source
                        and then not In_Instance;
            --  This must be true for any of the optimization warnings, we
            --  clearly want to give them only for source with the flag on. We
            --  also skip these warnings in an instance since it may be the
            --  case that different instantiations have different ranges.

            Lcheck : Compare_Result;
            Ucheck : Compare_Result;

         begin
            --  If test is explicit x'First .. x'Last, replace by 'Valid test

            if Is_Scalar_Type (Ltyp)

              --  Only relevant for source comparisons

              and then Comes_From_Simple_Range_In_Source

              --  And left operand is X'First where X matches left operand
              --  type (this eliminates cases of type mismatch, including
              --  the cases where ELIMINATED/MINIMIZED mode has changed the
              --  type of the left operand.

              and then Nkind (Lo_Orig) = N_Attribute_Reference
              and then Attribute_Name (Lo_Orig) = Name_First
              and then Is_Entity_Name (Prefix (Lo_Orig))
              and then Entity (Prefix (Lo_Orig)) = Ltyp

              --  Same tests for right operand

              and then Nkind (Hi_Orig) = N_Attribute_Reference
              and then Attribute_Name (Hi_Orig) = Name_Last
              and then Is_Entity_Name (Prefix (Hi_Orig))
              and then Entity (Prefix (Hi_Orig)) = Ltyp
            then
               Substitute_Valid_Test;
               goto Leave;
            end if;

            --  If bounds of type are known at compile time, and the end points
            --  are known at compile time and identical, this is another case
            --  for substituting a valid test. We only do this for discrete
            --  types, since it won't arise in practice for float types.

            if Comes_From_Simple_Range_In_Source
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

              --  Similarly, do not rewrite membership as a 'Valid test if
              --  within the predicate function for the type.

              --  Finally, if the original bounds are type conversions, even
              --  if they have been folded into constants, there are different
              --  types involved and 'Valid is not appropriate.

            then
               if In_Instance
                 or else (Ekind (Current_Scope) = E_Function
                           and then Is_Predicate_Function (Current_Scope))
               then
                  null;

               elsif Nkind (Lo_Orig) = N_Type_Conversion
                 or else Nkind (Hi_Orig) = N_Type_Conversion
               then
                  null;

               else
                  Substitute_Valid_Test;
                  goto Leave;
               end if;
            end if;

            --  If we have an explicit range, do a bit of optimization based on
            --  range analysis (we may be able to kill one or both checks).

            Lcheck := Compile_Time_Compare (Lop, Lo, Assume_Valid => False);
            Ucheck := Compile_Time_Compare (Lop, Hi, Assume_Valid => False);

            --  If either check is known to fail, replace result by False since
            --  the other check does not matter. Preserve the static flag for
            --  legality checks, because we are constant-folding beyond RM 4.9.

            if Lcheck = LT or else Ucheck = GT then
               if Warn then
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
               if Warn then
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
               Rewrite (N,
                 Make_Op_Le (Loc,
                   Left_Opnd  => Lop,
                   Right_Opnd => High_Bound (Rop)));
               Analyze_And_Resolve (N, Restyp);
               goto Leave;

            --  Inverse of previous case.

            elsif Ucheck in Compare_LE then
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

            if Warn and then not Assume_No_Invalid_Values then
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
               end if;
            end if;
         end;

         --  Try to narrow the operation

         if Ltyp = Universal_Integer and then Nkind (N) = N_In then
            Narrow_Large_Operation (N);
         end if;

         --  For all other cases of an explicit range, nothing to be done

         goto Leave;

      --  Here right operand is a subtype mark

      else
         declare
            Typ                  : Entity_Id        := Etype (Rop);
            Is_Acc               : constant Boolean := Is_Access_Type (Typ);
            Check_Null_Exclusion : Boolean;
            Cond                 : Node_Id          := Empty;
            New_N                : Node_Id;
            Obj                  : Node_Id          := Lop;
            SCIL_Node            : Node_Id;

         begin
            Remove_Side_Effects (Obj);

            --  For tagged type, do tagged membership operation

            if Is_Tagged_Type (Typ) then

               --  No expansion will be performed for VM targets, as the VM
               --  back ends will handle the membership tests directly.

               if Tagged_Type_Expansion then
                  Tagged_Membership (N, SCIL_Node, New_N);
                  Rewrite (N, New_N);
                  Analyze_And_Resolve (N, Restyp, Suppress => All_Checks);

                  --  Update decoration of relocated node referenced by the
                  --  SCIL node.

                  if Generate_SCIL and then Present (SCIL_Node) then
                     Set_SCIL_Node (N, SCIL_Node);
                  end if;
               end if;

               goto Leave;

            --  If type is scalar type, rewrite as x in t'First .. t'Last.
            --  The reason we do this is that the bounds may have the wrong
            --  type if they come from the original type definition. Also this
            --  way we get all the processing above for an explicit range.

            --  Don't do this for predicated types, since in this case we want
            --  to generate the predicate check at the end of the function.

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

            --  Ada 2005 (AI95-0216 amended by AI12-0162): Program_Error is
            --  raised when evaluating an individual membership test if the
            --  subtype mark denotes a constrained Unchecked_Union subtype
            --  and the expression lacks inferable discriminants.

            elsif Is_Unchecked_Union (Base_Type (Typ))
              and then Is_Constrained (Typ)
              and then not Has_Inferable_Discriminants (Lop)
            then
               Rewrite (N,
                 Make_Expression_With_Actions (Loc,
                   Actions    =>
                     New_List (Make_Raise_Program_Error (Loc,
                       Reason => PE_Unchecked_Union_Restriction)),
                   Expression =>
                     New_Occurrence_Of (Standard_False, Loc)));
               Analyze_And_Resolve (N, Restyp);

               goto Leave;
            end if;

            --  Here we have a non-scalar type

            if Is_Acc then

               --  If the null exclusion checks are not compatible, need to
               --  perform further checks. In other words, we cannot have
               --  Ltyp including null or Lop being null, and Typ excluding
               --  null. All other cases are OK.

               Check_Null_Exclusion :=
                 Can_Never_Be_Null (Typ)
                   and then (not Can_Never_Be_Null (Ltyp)
                              or else Nkind (Lop) = N_Null);
               Typ := Designated_Type (Typ);
            end if;

            if not Is_Constrained (Typ) then
               Cond := New_Occurrence_Of (Standard_True, Loc);

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
               else
                  Cond := New_Occurrence_Of (Standard_True, Loc);
               end if;
            end if;

            if Is_Acc then
               if Check_Null_Exclusion then
                  Cond := Make_And_Then (Loc,
                    Left_Opnd  =>
                      Make_Op_Ne (Loc,
                        Left_Opnd  => Obj,
                        Right_Opnd => Make_Null (Loc)),
                    Right_Opnd => Cond);
               else
                  Cond := Make_Or_Else (Loc,
                    Left_Opnd  =>
                      Make_Op_Eq (Loc,
                        Left_Opnd  => Obj,
                        Right_Opnd => Make_Null (Loc)),
                    Right_Opnd => Cond);
               end if;
            end if;

            Rewrite (N, Cond);
            Analyze_And_Resolve (N, Restyp);

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

                     if No (Expr_Entity) then
                        Expr_Entity := Entity (Lop);
                     end if;
                  end if;

                  --  When restriction No_Dynamic_Accessibility_Checks is in
                  --  effect, expand the membership test to a static value
                  --  since we cannot rely on dynamic levels.

                  if No_Dynamic_Accessibility_Checks_Enabled (Lop) then
                     if Static_Accessibility_Level
                          (Lop, Object_Decl_Level)
                            > Type_Access_Level (Rtyp)
                     then
                        Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
                     else
                        Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
                     end if;
                     Analyze_And_Resolve (N, Restyp);

                  --  If a conversion of the anonymous access value to the
                  --  tested type would be illegal, then the result is False.

                  elsif not Valid_Conversion
                              (Lop, Rtyp, Lop, Report_Errs => False)
                  then
                     Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
                     Analyze_And_Resolve (N, Restyp);

                  --  Apply an accessibility check if the access object has an
                  --  associated access level and when the level of the type is
                  --  less deep than the level of the access parameter. This
                  --  can only occur for access parameters and stand-alone
                  --  objects of an anonymous access type.

                  else
                     Param_Level := Accessibility_Level
                                      (Expr_Entity, Dynamic_Level);

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

                     --  If the designated type is tagged, do tagged membership
                     --  operation.

                     if Is_Tagged_Type (Typ) then

                        --  No expansion will be performed for VM targets, as
                        --  the VM back ends will handle the membership tests
                        --  directly.

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

      Predicate_Check : declare
         function In_Range_Check return Boolean;
         --  Within an expanded range check that may raise Constraint_Error do
         --  not generate a predicate check as well. It is redundant because
         --  the context will add an explicit predicate check, and it will
         --  raise the wrong exception if it fails.

         --------------------
         -- In_Range_Check --
         --------------------

         function In_Range_Check return Boolean is
            P : Node_Id;
         begin
            P := Parent (N);
            while Present (P) loop
               if Nkind (P) = N_Raise_Constraint_Error then
                  return True;

               elsif Nkind (P) in N_Statement_Other_Than_Procedure_Call
                 or else Nkind (P) = N_Procedure_Call_Statement
                 or else Nkind (P) in N_Declaration
               then
                  return False;
               end if;

               P := Parent (P);
            end loop;

            return False;
         end In_Range_Check;

         --  Local variables

         PFunc : constant Entity_Id := Predicate_Function (Rtyp);
         R_Op  : Node_Id;

      --  Start of processing for Predicate_Check

      begin
         if Present (PFunc)
           and then Current_Scope /= PFunc
           and then Nkind (Rop) /= N_Range
         then
            --  First apply the transformation that was skipped above

            if Is_Scalar_Type (Rtyp) then
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

               Analyze_And_Resolve (N, Restyp);
            end if;

            if not In_Range_Check then
               --  Indicate via Static_Mem parameter that this predicate
               --  evaluation is for a membership test.
               R_Op := Make_Predicate_Call (Rtyp, Lop, Static_Mem => True);
            else
               R_Op := New_Occurrence_Of (Standard_True, Loc);
            end if;

            Rewrite (N,
              Make_And_Then (Loc,
                Left_Opnd  => Relocate_Node (N),
                Right_Opnd => R_Op));

            --  Analyze new expression, mark left operand as analyzed to
            --  avoid infinite recursion adding predicate calls. Similarly,
            --  suppress further range checks on the call.

            Set_Analyzed (Left_Opnd (N));
            Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
         end if;
      end Predicate_Check;
   end Expand_N_In;

   --------------------------------
   -- Expand_N_Indexed_Component --
   --------------------------------

   procedure Expand_N_Indexed_Component (N : Node_Id) is

      Wild_Reads_May_Have_Bad_Side_Effects : Boolean
        renames Validity_Check_Subscripts;
      --  This Boolean needs to be True if reading from a bad address can
      --  have a bad side effect (e.g., a segmentation fault that is not
      --  transformed into a Storage_Error exception, or interactions with
      --  memory-mapped I/O) that needs to be prevented. This refers to the
      --  act of reading itself, not to any damage that might be caused later
      --  by making use of whatever value was read. We assume here that
      --  Validity_Check_Subscripts meets this requirement, but introduce
      --  this declaration in order to document this assumption.

      function Is_Renamed_Variable_Name (N : Node_Id) return Boolean;
      --  Returns True if the given name occurs as part of the renaming
      --  of a variable. In this case, the indexing operation should be
      --  treated as a write, rather than a read, with respect to validity
      --  checking. This is because the renamed variable can later be
      --  written to.

      function Type_Requires_Subscript_Validity_Checks_For_Reads
        (Typ : Entity_Id) return Boolean;
      --  If Wild_Reads_May_Have_Bad_Side_Effects is False and we are indexing
      --  into an array of characters in order to read an element, it is ok
      --  if an invalid index value goes undetected. But if it is an array of
      --  pointers or an array of tasks, the consequences of such a read are
      --  potentially more severe and so we want to detect an invalid index
      --  value. This function captures that distinction; this is intended to
      --  be consistent with the "but does not by itself lead to erroneous
      --  ... execution" rule of RM 13.9.1(11).

      ------------------------------
      -- Is_Renamed_Variable_Name --
      ------------------------------

      function Is_Renamed_Variable_Name (N : Node_Id) return Boolean is
         Rover : Node_Id := N;
      begin
         if Is_Variable (N) then
            loop
               declare
                  Rover_Parent : constant Node_Id := Parent (Rover);
               begin
                  case Nkind (Rover_Parent) is
                     when N_Object_Renaming_Declaration =>
                        return Rover = Name (Rover_Parent);

                     when N_Indexed_Component
                        | N_Slice
                        | N_Selected_Component
                     =>
                        exit when Rover /= Prefix (Rover_Parent);
                        Rover := Rover_Parent;

                     --  No need to check for qualified expressions or type
                     --  conversions here, mostly because of the Is_Variable
                     --  test. It is possible to have a view conversion for
                     --  which Is_Variable yields True and which occurs as
                     --  part of an object renaming, but only if the type is
                     --  tagged; in that case this function will not be called.

                     when others =>
                        exit;
                  end case;
               end;
            end loop;
         end if;
         return False;
      end Is_Renamed_Variable_Name;

      -------------------------------------------------------
      -- Type_Requires_Subscript_Validity_Checks_For_Reads --
      -------------------------------------------------------

      function Type_Requires_Subscript_Validity_Checks_For_Reads
        (Typ : Entity_Id) return Boolean
      is
         --  a shorter name for recursive calls
         function Needs_Check (Typ : Entity_Id) return Boolean renames
           Type_Requires_Subscript_Validity_Checks_For_Reads;
      begin
         if Is_Access_Type (Typ)
           or else Is_Tagged_Type (Typ)
           or else Is_Concurrent_Type (Typ)
           or else (Is_Array_Type (Typ)
                     and then Needs_Check (Component_Type (Typ)))
           or else (Is_Scalar_Type (Typ)
                     and then Has_Aspect (Typ, Aspect_Default_Value))
         then
            return True;
         end if;

         if Is_Record_Type (Typ) then
            declare
               Comp : Entity_Id := First_Component_Or_Discriminant (Typ);
            begin
               while Present (Comp) loop
                  if Needs_Check (Etype (Comp)) then
                     return True;
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;
            end;
         end if;

         return False;
      end Type_Requires_Subscript_Validity_Checks_For_Reads;

      --  Local constants

      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      P   : constant Node_Id    := Prefix (N);
      T   : constant Entity_Id  := Etype (P);

   --  Start of processing for Expand_N_Indexed_Component

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

      if Nkind (P) = N_Slice

        --  This optimization is disabled for CodePeer because it can transform
        --  an index-check constraint_error into a range-check constraint_error
        --  and CodePeer cares about that distinction.

        and then not CodePeer_Mode
      then
         Rewrite (N,
           Make_Indexed_Component (Loc,
             Prefix      => Prefix (P),
             Expressions => New_List (
               Convert_To
                 (Etype (First_Index (Etype (P))),
                  First (Expressions (N))))));
         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  Ada 2005 (AI-318-02): If the prefix is a call to a build-in-place
      --  function, then additional actuals must be passed.

      if Is_Build_In_Place_Function_Call (P) then
         Make_Build_In_Place_Call_In_Anonymous_Context (P);

      --  Ada 2005 (AI-318-02): Specialization of the previous case for prefix
      --  containing build-in-place function calls whose returned object covers
      --  interface types.

      elsif Present (Unqual_BIP_Iface_Function_Call (P)) then
         Make_Build_In_Place_Iface_Call_In_Anonymous_Context (P);
      end if;

      --  Generate index and validity checks

      declare
         Dims_Checked : Dimension_Set (Dimensions =>
                                         (if Is_Array_Type (T)
                                          then Number_Dimensions (T)
                                          else 1));
         --  Dims_Checked is used to avoid generating two checks (one in
         --  Generate_Index_Checks, one in Apply_Subscript_Validity_Checks)
         --  for the same index value in cases where the index check eliminates
         --  the need for the validity check. The Is_Array_Type test avoids
         --  cascading errors.

      begin
         Generate_Index_Checks (N, Checks_Generated => Dims_Checked);

         if Validity_Checks_On
           and then (Validity_Check_Subscripts
                      or else Wild_Reads_May_Have_Bad_Side_Effects
                      or else Type_Requires_Subscript_Validity_Checks_For_Reads
                                (Typ)
                      or else Is_Renamed_Variable_Name (N))
         then
            if Validity_Check_Subscripts then
               --  If we index into an array with an uninitialized variable
               --  and we generate an index check that passes at run time,
               --  passing that check does not ensure that the variable is
               --  valid (although it does in the common case where the
               --  object's subtype matches the index subtype).
               --  Consider an uninitialized variable with subtype 1 .. 10
               --  used to index into an array with bounds 1 .. 20 when the
               --  value of the uninitialized variable happens to be 15.
               --  The index check will succeed but the variable is invalid.
               --  If Validity_Check_Subscripts is True then we need to
               --  ensure validity, so we adjust Dims_Checked accordingly.
               Dims_Checked.Elements := (others => False);

            elsif Is_Array_Type (T) then
               --  We are only adding extra validity checks here to
               --  deal with uninitialized variables (but this includes
               --  assigning one uninitialized variable to another). Other
               --  ways of producing invalid objects imply erroneousness, so
               --  the compiler can do whatever it wants for those cases.
               --  If an index type has the Default_Value aspect specified,
               --  then we don't have to worry about the possibility of an
               --  uninitialized variable, so no need for these extra
               --  validity checks.

               declare
                  Idx : Node_Id := First_Index (T);
               begin
                  for No_Check_Needed of Dims_Checked.Elements loop
                     No_Check_Needed := No_Check_Needed
                       or else Has_Aspect (Etype (Idx), Aspect_Default_Value);
                     Next_Index (Idx);
                  end loop;
               end;
            end if;

            Apply_Subscript_Validity_Checks
              (N, No_Check_Needed => Dims_Checked);
         end if;
      end;

      --  If selecting from an array with atomic components, and atomic sync
      --  is not suppressed for this array type, set atomic sync flag.

      if (Has_Atomic_Components (T)
           and then not Atomic_Synchronization_Disabled (T))
        or else (Is_Atomic (Typ)
                  and then not Atomic_Synchronization_Disabled (Typ))
        or else (Is_Entity_Name (P)
                  and then Has_Atomic_Components (Entity (P))
                  and then not Atomic_Synchronization_Disabled (Entity (P)))
      then
         Activate_Atomic_Synchronization (N);
      end if;

      --  All done if the prefix is not a packed array implemented specially

      if not (Is_Packed (Etype (Prefix (N)))
               and then Present (Packed_Array_Impl_Type (Etype (Prefix (N)))))
      then
         return;
      end if;

      --  For packed arrays that are not bit-packed (i.e. the case of an array
      --  with one or more index types with a non-contiguous enumeration type),
      --  we can always use the normal packed element get circuit.

      if not Is_Bit_Packed_Array (Etype (Prefix (N))) then
         Expand_Packed_Element_Reference (N);
         return;
      end if;

      --  For a reference to a component of a bit packed array, we convert it
      --  to a reference to the corresponding Packed_Array_Impl_Type. We only
      --  want to do this for simple references, and not for:

      --    Left side of assignment, or prefix of left side of assignment, or
      --    prefix of the prefix, to handle packed arrays of packed arrays,
      --      This case is handled in Exp_Ch5.Expand_N_Assignment_Statement

      --    Renaming objects in renaming associations
      --      This case is handled when a use of the renamed variable occurs

      --    Actual parameters for a subprogram call
      --      This case is handled in Exp_Ch6.Expand_Actuals

      --    The second expression in a 'Read attribute reference

      --    The prefix of an address or bit or size attribute reference

      --  The following circuit detects these exceptions. Note that we need to
      --  deal with implicit dereferences when climbing up the parent chain,
      --  with the additional difficulty that the type of parents may have yet
      --  to be resolved since prefixes are usually resolved first.

      declare
         Child : Node_Id := N;
         Parnt : Node_Id := Parent (N);

      begin
         loop
            if Nkind (Parnt) = N_Object_Renaming_Declaration then
               return;

            elsif Nkind (Parnt) in N_Subprogram_Call
              or else (Nkind (Parnt) = N_Parameter_Association
                        and then Nkind (Parent (Parnt)) in N_Subprogram_Call)
            then
               return;

            elsif Nkind (Parnt) = N_Attribute_Reference
              and then Attribute_Name (Parnt) in Name_Address
                                               | Name_Bit
                                               | Name_Size
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

            elsif Nkind (Parnt) = N_Indexed_Component
              and then Prefix (Parnt) = Child
            then
               null;

            elsif Nkind (Parnt) = N_Selected_Component
              and then Prefix (Parnt) = Child
              and then not (Present (Etype (Selector_Name (Parnt)))
                              and then
                            Is_Access_Type (Etype (Selector_Name (Parnt))))
            then
               null;

            --  If the parent is a dereference, either implicit or explicit,
            --  then the packed reference needs to be expanded.

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
              Left_Opnd    => Left_Opnd (N),
              Right_Opnd   => Right_Opnd (N),
              Alternatives => Alternatives (N))));

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
   --  it), so that the back end creates the proper value.

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
      Expr : constant Node_Id    := Right_Opnd (N);
      Typ  : constant Entity_Id  := Etype (N);

   begin
      Unary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Abs then
            return;
         end if;
      end if;

      --  Deal with software overflow checking

      if Is_Signed_Integer_Type (Typ)
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
                     Prefix         =>
                       New_Occurrence_Of (Base_Type (Etype (Expr)), Loc),
                     Attribute_Name => Name_First)),
             Reason => CE_Overflow_Check_Failed));

         Set_Do_Overflow_Check (N, False);
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

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Add then
            return;
         end if;
      end if;

      --  Arithmetic overflow checks for signed integer/fixed point types

      if Is_Signed_Integer_Type (Typ) or else Is_Fixed_Point_Type (Typ) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Overflow checks for floating-point if -gnateF mode active

      Check_Float_Op_Overflow (N);

      Expand_Nonbinary_Modular_Op (N);
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

      Expand_Nonbinary_Modular_Op (N);
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

         --  Note: The following code is a temporary workaround for N731-034
         --  and N829-028 and will be kept until the general issue of internal
         --  symbol serialization is addressed. The workaround is kept under a
         --  debug switch to avoid permiating into the general case.

         --  Wrap the node to concatenate into an expression actions node to
         --  keep it nicely packaged. This is useful in the case of an assert
         --  pragma with a concatenation where we want to be able to delete
         --  the concatenation and all its expansion stuff.

         if Debug_Flag_Dot_H then
            declare
               Cnod : constant Node_Id   := New_Copy_Tree (Cnode);
               Typ  : constant Entity_Id := Base_Type (Etype (Cnode));

            begin
               --  Note: use Rewrite rather than Replace here, so that for
               --  example Why_Not_Static can find the original concatenation
               --  node OK!

               Rewrite (Cnode,
                 Make_Expression_With_Actions (Sloc (Cnode),
                   Actions    => New_List (Make_Null_Statement (Sloc (Cnode))),
                   Expression => Cnod));

               Expand_Concatenate (Cnod, Opnds);
               Analyze_And_Resolve (Cnode, Typ);
            end;

         --  Default case

         else
            Expand_Concatenate (Cnode, Opnds);
         end if;

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

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Divide then
            return;
         end if;
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

         if Is_Integer_Type (Rtyp) then
            Expand_Divide_Fixed_By_Integer_Giving_Fixed (N);
         else
            Expand_Divide_Fixed_By_Fixed_Giving_Fixed (N);
         end if;

         --  Deal with divide-by-zero check if back end cannot handle them
         --  and the flag is set indicating that we need such a check. Note
         --  that we don't need to bother here with the case of mixed-mode
         --  (Right operand an integer type), since these will be rewritten
         --  with conversions to a divide with a fixed-point right operand.

         if Nkind (N) = N_Op_Divide
           and then Do_Division_Check (N)
           and then not Backend_Divide_Checks_On_Target
           and then not Is_Integer_Type (Rtyp)
         then
            Set_Do_Division_Check (N, False);
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Op_Eq (Loc,
                    Left_Opnd  => Duplicate_Subexpr_Move_Checks (Ropnd),
                    Right_Opnd => Make_Real_Literal (Loc, Ureal_0)),
                  Reason  => CE_Divide_By_Zero));
         end if;

      --  Other cases of division of fixed-point operands

      elsif Is_Fixed_Point_Type (Ltyp) or else Is_Fixed_Point_Type (Rtyp) then
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
      end if;

      --  Overflow checks for floating-point if -gnateF mode active

      Check_Float_Op_Overflow (N);

      Expand_Nonbinary_Modular_Op (N);
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

      procedure Build_Equality_Call (Eq : Entity_Id);
      --  If a constructed equality exists for the type or for its parent,
      --  build and analyze call, adding conversions if the operation is
      --  inherited.

      function Find_Equality (Prims : Elist_Id) return Entity_Id;
      --  Find a primitive equality function within primitive operation list
      --  Prims.

      function Has_Unconstrained_UU_Component (Typ : Entity_Id) return Boolean;
      --  Determines whether a type has a subcomponent of an unconstrained
      --  Unchecked_Union subtype. Typ is a record type.

      -------------------------
      -- Build_Equality_Call --
      -------------------------

      procedure Build_Equality_Call (Eq : Entity_Id) is
         Op_Typ : constant Entity_Id := Etype (First_Formal (Eq));

         L_Exp, R_Exp : Node_Id;

      begin
         --  Adjust operands if necessary to comparison type

         if Base_Type (A_Typ) /= Base_Type (Op_Typ)
           and then not Is_Class_Wide_Type (A_Typ)
         then
            L_Exp := OK_Convert_To (Op_Typ, Lhs);
            R_Exp := OK_Convert_To (Op_Typ, Rhs);

         else
            L_Exp := Relocate_Node (Lhs);
            R_Exp := Relocate_Node (Rhs);
         end if;

         Rewrite (N,
           Make_Function_Call (Loc,
             Name                   => New_Occurrence_Of (Eq, Loc),
             Parameter_Associations => New_List (L_Exp, R_Exp)));

         Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
      end Build_Equality_Call;

      -------------------
      -- Find_Equality --
      -------------------

      function Find_Equality (Prims : Elist_Id) return Entity_Id is
         function Find_Aliased_Equality (Prim : Entity_Id) return Entity_Id;
         --  Find an equality in a possible alias chain starting from primitive
         --  operation Prim.

         ---------------------------
         -- Find_Aliased_Equality --
         ---------------------------

         function Find_Aliased_Equality (Prim : Entity_Id) return Entity_Id is
            Candid : Entity_Id;

         begin
            --  Inspect each candidate in the alias chain, checking whether it
            --  denotes an equality.

            Candid := Prim;
            while Present (Candid) loop
               if Is_User_Defined_Equality (Candid) then
                  return Candid;
               end if;

               Candid := Alias (Candid);
            end loop;

            return Empty;
         end Find_Aliased_Equality;

         --  Local variables

         Eq_Prim   : Entity_Id;
         Prim_Elmt : Elmt_Id;

      --  Start of processing for Find_Equality

      begin
         --  Assume that the tagged type lacks an equality

         Eq_Prim := Empty;

         --  Inspect the list of primitives looking for a suitable equality
         --  within a possible chain of aliases.

         Prim_Elmt := First_Elmt (Prims);
         while Present (Prim_Elmt) and then No (Eq_Prim) loop
            Eq_Prim := Find_Aliased_Equality (Node (Prim_Elmt));

            Next_Elmt (Prim_Elmt);
         end loop;

         --  A tagged type should always have an equality

         pragma Assert (Present (Eq_Prim));

         return Eq_Prim;
      end Find_Equality;

      ------------------------------------
      -- Has_Unconstrained_UU_Component --
      ------------------------------------

      function Has_Unconstrained_UU_Component
        (Typ : Entity_Id) return Boolean
      is
         function Unconstrained_UU_In_Component_Declaration
           (N : Node_Id) return Boolean;

         function Unconstrained_UU_In_Component_Items
           (L : List_Id) return Boolean;

         function Unconstrained_UU_In_Component_List
           (N : Node_Id) return Boolean;

         function Unconstrained_UU_In_Variant_Part
           (N : Node_Id) return Boolean;
         --  A family of routines that determine whether a particular construct
         --  of a record type definition contains a subcomponent of an
         --  unchecked union type whose nominal subtype is unconstrained.
         --
         --  Individual routines correspond to the production rules of the Ada
         --  grammar, as described in the Ada RM (P).

         -----------------------------------------------
         -- Unconstrained_UU_In_Component_Declaration --
         -----------------------------------------------

         function Unconstrained_UU_In_Component_Declaration
           (N : Node_Id) return Boolean
         is
            pragma Assert (Nkind (N) = N_Component_Declaration);

            Sindic : constant Node_Id :=
                       Subtype_Indication (Component_Definition (N));
         begin
            --  If the component declaration includes a subtype indication
            --  it is not an unchecked_union. Otherwise verify that it carries
            --  the Unchecked_Union flag and is either a record or a private
            --  type. A Record_Subtype declared elsewhere does not qualify,
            --  even if its parent type carries the flag.

            return Nkind (Sindic) in N_Expanded_Name | N_Identifier
              and then Is_Unchecked_Union (Base_Type (Etype (Sindic)))
              and then Ekind (Entity (Sindic)) in
                         E_Private_Type | E_Record_Type;
         end Unconstrained_UU_In_Component_Declaration;

         -----------------------------------------
         -- Unconstrained_UU_In_Component_Items --
         -----------------------------------------

         function Unconstrained_UU_In_Component_Items
           (L : List_Id) return Boolean
         is
            N : Node_Id := First (L);
         begin
            while Present (N) loop
               if Nkind (N) = N_Component_Declaration
                 and then Unconstrained_UU_In_Component_Declaration (N)
               then
                  return True;
               end if;

               Next (N);
            end loop;

            return False;
         end Unconstrained_UU_In_Component_Items;

         ----------------------------------------
         -- Unconstrained_UU_In_Component_List --
         ----------------------------------------

         function Unconstrained_UU_In_Component_List
           (N : Node_Id) return Boolean
         is
            pragma Assert (Nkind (N) = N_Component_List);

            Optional_Variant_Part : Node_Id;
         begin
            if Unconstrained_UU_In_Component_Items (Component_Items (N)) then
               return True;
            end if;

            Optional_Variant_Part := Variant_Part (N);

            return
              Present (Optional_Variant_Part)
              and then
                Unconstrained_UU_In_Variant_Part (Optional_Variant_Part);
         end Unconstrained_UU_In_Component_List;

         --------------------------------------
         -- Unconstrained_UU_In_Variant_Part --
         --------------------------------------

         function Unconstrained_UU_In_Variant_Part
           (N : Node_Id) return Boolean
         is
            pragma Assert (Nkind (N) = N_Variant_Part);

            Variant : Node_Id := First (Variants (N));
         begin
            loop
               if Unconstrained_UU_In_Component_List (Component_List (Variant))
               then
                  return True;
               end if;

               Next (Variant);
               exit when No (Variant);
            end loop;

            return False;
         end Unconstrained_UU_In_Variant_Part;

         Typ_Def : constant Node_Id :=
           Type_Definition (Declaration_Node (Base_Type (Typ)));

         Optional_Component_List : constant Node_Id :=
           Component_List (Typ_Def);

      --  Start of processing for Has_Unconstrained_UU_Component

      begin
         return Present (Optional_Component_List)
           and then
             Unconstrained_UU_In_Component_List (Optional_Component_List);
      end Has_Unconstrained_UU_Component;

      --  Local variables

      Typl : Entity_Id;

   --  Start of processing for Expand_N_Op_Eq

   begin
      Binary_Op_Validity_Checks (N);

      --  Deal with private types

      Typl := Underlying_Type (A_Typ);

      --  It may happen in error situations that the underlying type is not
      --  set. The error will be detected later, here we just defend the
      --  expander code.

      if No (Typl) then
         return;
      end if;

      --  Now get the implementation base type (note that plain Base_Type here
      --  might lead us back to the private type, which is not what we want!)

      Typl := Implementation_Base_Type (Typl);

      --  Equality between variant records results in a call to a routine
      --  that has conditional tests of the discriminant value(s), and hence
      --  violates the No_Implicit_Conditionals restriction.

      if Has_Variant_Part (Typl) then
         declare
            Msg : Boolean;

         begin
            Check_Restriction (Msg, No_Implicit_Conditionals, N);

            if Msg then
               Error_Msg_N
                 ("\comparison of variant records tests discriminants", N);
               return;
            end if;
         end;
      end if;

      --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if that
      --  means we no longer have a comparison operation, we are all done.

      if Minimized_Eliminated_Overflow_Check (Left_Opnd (N)) then
         Expand_Compare_Minimize_Eliminate_Overflow (N);
      end if;

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

         --  When the component type is elementary, we can use a byte-wise
         --  comparison if supported on the target, except in the cases
         --  of floating-point (negative zero issues require element by
         --  element comparison), and full access types (where we must be sure
         --  to load elements independently) and possibly unaligned arrays.

         elsif Is_Elementary_Type (Component_Type (Typl))
           and then not Is_Floating_Point_Type (Component_Type (Typl))
           and then not Is_Full_Access (Component_Type (Typl))
           and then not Is_Possibly_Unaligned_Object (Lhs)
           and then not Is_Possibly_Unaligned_Slice (Lhs)
           and then not Is_Possibly_Unaligned_Object (Rhs)
           and then not Is_Possibly_Unaligned_Slice (Rhs)
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

            --  If this is an untagged private type completed with a derivation
            --  of an untagged private type whose full view is a tagged type,
            --  we use the primitive operations of the private type (since it
            --  does not have a full view, and also because its equality
            --  primitive may have been overridden in its untagged full view).

            if Inherits_From_Tagged_Full_View (A_Typ) then
               Build_Equality_Call
                 (Find_Equality (Collect_Primitive_Operations (A_Typ)));

            --  Find the type's predefined equality or an overriding
            --  user-defined equality. The reason for not simply calling
            --  Find_Prim_Op here is that there may be a user-defined
            --  overloaded equality op that precedes the equality that we
            --  want, so we have to explicitly search (e.g., there could be
            --  an equality with two different parameter types).

            else
               if Is_Class_Wide_Type (Typl) then
                  Typl := Find_Specific_Type (Typl);
               end if;

               Build_Equality_Call
                 (Find_Equality (Primitive_Operations (Typl)));
            end if;

         --  Ada 2005 (AI-216): Program_Error is raised when evaluating the
         --  predefined equality operator for a type which has a subcomponent
         --  of an unchecked union type whose nominal subtype is unconstrained.

         elsif Has_Unconstrained_UU_Component (Typl) then
            Insert_Action (N,
              Make_Raise_Program_Error (Loc,
                Reason => PE_Unchecked_Union_Restriction));

            Rewrite (N,
              New_Occurrence_Of (Standard_False, Loc));

         --  If a type support function is present, e.g. if there is a variant
         --  part, including an unchecked union type, use it.

         elsif Present (TSS (Root_Type (Typl), TSS_Composite_Equality)) then
            Build_Equality_Call
              (TSS (Root_Type (Typl), TSS_Composite_Equality));

         --  When comparing two Bounded_Strings, use the primitive equality of
         --  the root Super_String type.

         elsif Is_Bounded_String (Typl) then
            Build_Equality_Call
              (Find_Equality
                (Collect_Primitive_Operations (Root_Type (Typl))));

         --  Otherwise expand the component by component equality. Note that
         --  we never use block-bit comparisons for records, because of the
         --  problems with gaps. The back end will often be able to recombine
         --  the separate comparisons that we generate here.

         else
            Remove_Side_Effects (Lhs);
            Remove_Side_Effects (Rhs);
            Rewrite (N, Expand_Record_Equality (N, Typl, Lhs, Rhs));

            Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
         end if;

      --  If unnesting, handle elementary types whose Equivalent_Types are
      --  records because there may be padding or undefined fields.

      elsif Unnest_Subprogram_Mode
        and then Ekind (Typl) in E_Class_Wide_Type
                               | E_Class_Wide_Subtype
                               | E_Access_Subprogram_Type
                               | E_Access_Protected_Subprogram_Type
                               | E_Anonymous_Access_Protected_Subprogram_Type
                               | E_Exception_Type
        and then Present (Equivalent_Type (Typl))
        and then Is_Record_Type (Equivalent_Type (Typl))
      then
         Typl := Equivalent_Type (Typl);
         Remove_Side_Effects (Lhs);
         Remove_Side_Effects (Rhs);
         Rewrite (N,
           Expand_Record_Equality (N, Typl,
             Unchecked_Convert_To (Typl, Lhs),
             Unchecked_Convert_To (Typl, Rhs)));

         Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
      end if;

      --  Test if result is known at compile time

      Rewrite_Comparison (N);

      --  Try to narrow the operation

      if Typl = Universal_Integer and then Nkind (N) = N_Op_Eq then
         Narrow_Large_Operation (N);
      end if;

      --  Special optimization of length comparison

      Optimize_Length_Comparison (N);

      --  One more special case: if we have a comparison of X'Result = expr
      --  in floating-point, then if not already there, change expr to be
      --  f'Machine (expr) to eliminate surprise from extra precision.

      if Is_Floating_Point_Type (Typl)
        and then Is_Attribute_Result (Original_Node (Lhs))
      then
         --  Stick in the Typ'Machine call if not already there

         if Nkind (Rhs) /= N_Attribute_Reference
           or else Attribute_Name (Rhs) /= Name_Machine
         then
            Rewrite (Rhs,
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Typl, Loc),
                Attribute_Name => Name_Machine,
                Expressions    => New_List (Relocate_Node (Rhs))));
            Analyze_And_Resolve (Rhs, Typl);
         end if;
      end if;
   end Expand_N_Op_Eq;

   -----------------------
   -- Expand_N_Op_Expon --
   -----------------------

   procedure Expand_N_Op_Expon (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Ovflo : constant Boolean    := Do_Overflow_Check (N);
      Typ   : constant Entity_Id  := Etype (N);
      Rtyp  : constant Entity_Id  := Root_Type (Typ);

      Bastyp : Entity_Id;

      function Wrap_MA (Exp : Node_Id) return Node_Id;
      --  Given an expression Exp, if the root type is Float or Long_Float,
      --  then wrap the expression in a call of Bastyp'Machine, to stop any
      --  extra precision. This is done to ensure that X**A = X**B when A is
      --  a static constant and B is a variable with the same value. For any
      --  other type, the node Exp is returned unchanged.

      -------------
      -- Wrap_MA --
      -------------

      function Wrap_MA (Exp : Node_Id) return Node_Id is
         Loc : constant Source_Ptr := Sloc (Exp);

      begin
         if Rtyp = Standard_Float or else Rtyp = Standard_Long_Float then
            return
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Machine,
                Prefix         => New_Occurrence_Of (Bastyp, Loc),
                Expressions    => New_List (Relocate_Node (Exp)));
         else
            return Exp;
         end if;
      end Wrap_MA;

      --  Local variables

      Base   : Node_Id;
      Ent    : Entity_Id;
      Etyp   : Entity_Id;
      Exp    : Node_Id;
      Exptyp : Entity_Id;
      Expv   : Uint;
      Rent   : RE_Id;
      Temp   : Node_Id;
      Xnode  : Node_Id;

   --  Start of processing for Expand_N_Op_Expon

   begin
      Binary_Op_Validity_Checks (N);

      --  CodePeer wants to see the unexpanded N_Op_Expon node

      if CodePeer_Mode then
         return;
      end if;

      --  Relocation of left and right operands must be done after performing
      --  the validity checks since the generation of validation checks may
      --  remove side effects.

      Base   := Relocate_Node (Left_Opnd (N));
      Bastyp := Etype (Base);
      Exp    := Relocate_Node (Right_Opnd (N));
      Exptyp := Etype (Exp);

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
         --  See ACVC test C4A012B, and it is not worth generating the test.

         --  For small negative exponents, we return the reciprocal of
         --  the folding of the exponentiation for the opposite (positive)
         --  exponent, as required by RM 4.5.6(11/3).

         if abs Expv <= 4 then

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
                 Wrap_MA (
                   Make_Op_Multiply (Loc,
                     Left_Opnd  => Duplicate_Subexpr (Base),
                     Right_Opnd => Duplicate_Subexpr_No_Checks (Base)));

            --  X ** 3 = X * X * X

            elsif Expv = 3 then
               Xnode :=
                 Wrap_MA (
                   Make_Op_Multiply (Loc,
                     Left_Opnd =>
                       Make_Op_Multiply (Loc,
                         Left_Opnd  => Duplicate_Subexpr (Base),
                         Right_Opnd => Duplicate_Subexpr_No_Checks (Base)),
                   Right_Opnd  => Duplicate_Subexpr_No_Checks (Base)));

            --  X ** 4  ->

            --  do
            --    En : constant base'type := base * base;
            --  in
            --    En * En

            elsif Expv = 4 then
               Temp := Make_Temporary (Loc, 'E', Base);

               Xnode :=
                 Make_Expression_With_Actions (Loc,
                   Actions    => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Temp,
                       Constant_Present    => True,
                       Object_Definition   => New_Occurrence_Of (Typ, Loc),
                       Expression =>
                         Wrap_MA (
                           Make_Op_Multiply (Loc,
                             Left_Opnd  =>
                               Duplicate_Subexpr (Base),
                             Right_Opnd =>
                               Duplicate_Subexpr_No_Checks (Base))))),

                   Expression =>
                     Wrap_MA (
                       Make_Op_Multiply (Loc,
                         Left_Opnd  => New_Occurrence_Of (Temp, Loc),
                         Right_Opnd => New_Occurrence_Of (Temp, Loc))));

            --  X ** N = 1.0 / X ** (-N)
            --  N in -4 .. -1

            else
               pragma Assert
                 (Expv = -1 or Expv = -2 or Expv = -3 or Expv = -4);

               Xnode :=
                 Make_Op_Divide (Loc,
                   Left_Opnd  =>
                     Make_Float_Literal (Loc,
                       Radix       => Uint_1,
                       Significand => Uint_1,
                       Exponent    => Uint_0),
                   Right_Opnd =>
                     Make_Op_Expon (Loc,
                       Left_Opnd  => Duplicate_Subexpr (Base),
                       Right_Opnd =>
                         Make_Integer_Literal (Loc,
                           Intval => -Expv)));
            end if;

            Rewrite (N, Xnode);
            Analyze_And_Resolve (N, Typ);
            return;
         end if;
      end if;

      --  Optimize 2 ** expression to shift where possible

      --  Note: we used to check that Exptyp was an unsigned type. But that is
      --  an unnecessary check, since if Exp is negative, we have a run-time
      --  error that is either caught (so we get the right result) or we have
      --  suppressed the check, in which case the code is erroneous anyway.

      if Is_Integer_Type (Rtyp)

        --  The base value must be "safe compile-time known", and exactly 2

        and then Nkind (Base) = N_Integer_Literal
        and then CRT_Safe_Compile_Time_Known_Value (Base)
        and then Expr_Value (Base) = Uint_2

        --  This transformation is not applicable for a modular type with a
        --  nonbinary modulus because shifting makes no sense in that case.

        and then not Non_Binary_Modulus (Typ)
      then
         --  Handle the cases where our parent is a division or multiplication
         --  specially. In these cases we can convert to using a shift at the
         --  parent level if we are not doing overflow checking, since it is
         --  too tricky to combine the overflow check at the parent level.

         if not Ovflo
           and then Nkind (Parent (N)) in N_Op_Divide | N_Op_Multiply
         then
            declare
               P : constant Node_Id := Parent (N);
               L : constant Node_Id := Left_Opnd (P);
               R : constant Node_Id := Right_Opnd (P);

            begin
               if (Nkind (P) = N_Op_Multiply
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

         --  Here we have 2 ** N on its own, so we can convert this into a
         --  shift.

         else
            --  Op_Shift_Left (generated below) has modular-shift semantics;
            --  therefore we might need to generate an overflow check here
            --  if the type is signed.

            if Is_Signed_Integer_Type (Typ) and then Ovflo then
               declare
                  OK : Boolean;
                  Lo : Uint;
                  Hi : Uint;

                  MaxS : constant Uint := Esize (Rtyp) - 2;
                  --  Maximum shift count with no overflow
               begin
                  Determine_Range (Exp, OK, Lo, Hi, Assume_Valid => True);

                  if not OK or else Hi > MaxS then
                     Insert_Action (N,
                       Make_Raise_Constraint_Error (Loc,
                         Condition =>
                           Make_Op_Gt (Loc,
                             Left_Opnd  => Duplicate_Subexpr (Exp),
                             Right_Opnd => Make_Integer_Literal (Loc, MaxS)),
                         Reason    => CE_Overflow_Check_Failed));
                  end if;
               end;
            end if;

            --  Generate Shift_Left (1, Exp)

            Rewrite (N,
              Make_Op_Shift_Left (Loc,
                Left_Opnd  => Make_Integer_Literal (Loc, Uint_1),
                Right_Opnd => Exp));

            Analyze_And_Resolve (N, Typ);
            return;
         end if;
      end if;

      --  Fall through if exponentiation must be done using a runtime routine

      --  First deal with modular case

      if Is_Modular_Integer_Type (Rtyp) then

         --  Nonbinary modular case, we call the special exponentiation
         --  routine for the nonbinary case, converting the argument to
         --  Long_Long_Integer and passing the modulus value. Then the
         --  result is converted back to the base type.

         if Non_Binary_Modulus (Rtyp) then
            Rewrite (N,
              Convert_To (Typ,
                Make_Function_Call (Loc,
                  Name                   =>
                    New_Occurrence_Of (RTE (RE_Exp_Modular), Loc),
                  Parameter_Associations => New_List (
                    Convert_To (RTE (RE_Unsigned), Base),
                    Make_Integer_Literal (Loc, Modulus (Rtyp)),
                    Exp))));

         --  Binary modular case, in this case, we call one of three routines,
         --  either the unsigned integer case, or the unsigned long long
         --  integer case, or the unsigned long long long integer case, with a
         --  final "and" operation to do the required mod.

         else
            if Esize (Rtyp) <= Standard_Integer_Size then
               Ent := RTE (RE_Exp_Unsigned);
            elsif Esize (Rtyp) <= Standard_Long_Long_Integer_Size then
               Ent := RTE (RE_Exp_Long_Long_Unsigned);
            else
               Ent := RTE (RE_Exp_Long_Long_Long_Unsigned);
            end if;

            Rewrite (N,
              Convert_To (Typ,
                Make_Op_And (Loc,
                  Left_Opnd  =>
                    Make_Function_Call (Loc,
                      Name                   => New_Occurrence_Of (Ent, Loc),
                      Parameter_Associations => New_List (
                        Convert_To (Etype (First_Formal (Ent)), Base),
                        Exp)),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Modulus (Rtyp) - 1))));

         end if;

         --  Common exit point for modular type case

         Analyze_And_Resolve (N, Typ);
         return;

      --  Signed integer cases, using either Integer, Long_Long_Integer or
      --  Long_Long_Long_Integer. It is not worth also having routines for
      --  Short_[Short_]Integer, since for most machines it would not help,
      --  and it would generate more code that might need certification when
      --  a certified run time is required.

      --  In the integer cases, we have two routines, one for when overflow
      --  checks are required, and one when they are not required, since there
      --  is a real gain in omitting checks on many machines.

      elsif Is_Signed_Integer_Type (Rtyp) then
         if Esize (Rtyp) <= Standard_Integer_Size then
            Etyp := Standard_Integer;

            if Ovflo then
               Rent := RE_Exp_Integer;
            else
               Rent := RE_Exn_Integer;
            end if;

         elsif Esize (Rtyp) <= Standard_Long_Long_Integer_Size then
            Etyp := Standard_Long_Long_Integer;

            if Ovflo then
               Rent := RE_Exp_Long_Long_Integer;
            else
               Rent := RE_Exn_Long_Long_Integer;
            end if;

         else
            Etyp := Standard_Long_Long_Long_Integer;

            if Ovflo then
               Rent := RE_Exp_Long_Long_Long_Integer;
            else
               Rent := RE_Exn_Long_Long_Long_Integer;
            end if;
         end if;

      --  Floating-point cases. We do not need separate routines for the
      --  overflow case here, since in the case of floating-point, we generate
      --  infinities anyway as a rule (either that or we automatically trap
      --  overflow), and if there is an infinity generated and a range check
      --  is required, the check will fail anyway.

      else
         pragma Assert (Is_Floating_Point_Type (Rtyp));

         --  Short_Float and Float are the same type for GNAT

         if Rtyp = Standard_Short_Float or else Rtyp = Standard_Float then
            Etyp := Standard_Float;
            Rent := RE_Exn_Float;

         elsif Rtyp = Standard_Long_Float then
            Etyp := Standard_Long_Float;
            Rent := RE_Exn_Long_Float;

         else
            Etyp := Standard_Long_Long_Float;
            Rent := RE_Exn_Long_Long_Float;
         end if;
      end if;

      --  Common processing for integer cases and floating-point cases.
      --  If we are in the right type, we can call runtime routine directly

      if Typ = Etyp
        and then not Is_Universal_Numeric_Type (Rtyp)
      then
         Rewrite (N,
           Wrap_MA (
             Make_Function_Call (Loc,
               Name                   => New_Occurrence_Of (RTE (Rent), Loc),
               Parameter_Associations => New_List (Base, Exp))));

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

      if Minimized_Eliminated_Overflow_Check (Op1) then
         Expand_Compare_Minimize_Eliminate_Overflow (N);
      end if;

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

      --  Try to narrow the operation

      if Typ1 = Universal_Integer and then Nkind (N) = N_Op_Ge then
         Narrow_Large_Operation (N);
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

      if Minimized_Eliminated_Overflow_Check (Op1) then
         Expand_Compare_Minimize_Eliminate_Overflow (N);
      end if;

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

      --  Try to narrow the operation

      if Typ1 = Universal_Integer and then Nkind (N) = N_Op_Gt then
         Narrow_Large_Operation (N);
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

      if Minimized_Eliminated_Overflow_Check (Op1) then
         Expand_Compare_Minimize_Eliminate_Overflow (N);
      end if;

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

      --  Try to narrow the operation

      if Typ1 = Universal_Integer and then Nkind (N) = N_Op_Le then
         Narrow_Large_Operation (N);
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

      if Minimized_Eliminated_Overflow_Check (Op1) then
         Expand_Compare_Minimize_Eliminate_Overflow (N);
      end if;

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

      --  Try to narrow the operation

      if Typ1 = Universal_Integer and then Nkind (N) = N_Op_Lt then
         Narrow_Large_Operation (N);
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

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Minus then
            return;
         end if;
      end if;

      if not Backend_Overflow_Checks_On_Target
         and then Is_Signed_Integer_Type (Typ)
         and then Do_Overflow_Check (N)
      then
         --  Software overflow checking expands -expr into (0 - expr)

         Rewrite (N,
           Make_Op_Subtract (Loc,
             Left_Opnd  => Make_Integer_Literal (Loc, 0),
             Right_Opnd => Right_Opnd (N)));

         Analyze_And_Resolve (N, Typ);
      end if;

      Expand_Nonbinary_Modular_Op (N);
   end Expand_N_Op_Minus;

   ---------------------
   -- Expand_N_Op_Mod --
   ---------------------

   procedure Expand_N_Op_Mod (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      DDC   : constant Boolean    := Do_Division_Check (N);

      Is_Stoele_Mod : constant Boolean :=
        Is_RTE (Typ, RE_Address)
          and then Nkind (Right_Opnd (N)) = N_Unchecked_Type_Conversion
          and then
            Is_RTE (Etype (Expression (Right_Opnd (N))), RE_Storage_Offset);
      --  True if this is the special mod operator of System.Storage_Elements

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

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Mod then
            return;
         end if;
      end if;

      --  For the special mod operator of System.Storage_Elements, the checks
      --  are subsumed into the handling of the negative case below.

      if Is_Integer_Type (Typ) and then not Is_Stoele_Mod then
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
      --  that it could be harmful.

      if (LOK and ROK)
        and then ((Llo >= 0 and then Rlo >= 0)
                     or else
                  (Lhi <= 0 and then Rhi <= 0))
        and then not Is_Stoele_Mod
      then
         Rewrite (N,
           Make_Op_Rem (Sloc (N),
             Left_Opnd  => Left_Opnd (N),
             Right_Opnd => Right_Opnd (N)));

         --  Instead of reanalyzing the node we do the analysis manually. This
         --  avoids anomalies when the replacement is done in an instance and
         --  is epsilon more efficient.

         pragma Assert (Entity (N) = Standard_Op_Rem);
         Set_Etype             (N, Typ);
         Set_Do_Division_Check (N, DDC);
         Expand_N_Op_Rem (N);
         Set_Analyzed (N);
         return;

      --  Otherwise, normal mod processing

      else
         --  Apply optimization x mod 1 = 0. We don't really need that with
         --  gcc, but it is useful with other back ends and is certainly
         --  harmless.

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

         --  The negative case makes no sense since it is a case of a mod where
         --  the left argument is unsigned and the right argument is signed. In
         --  accordance with the (spirit of the) permission of RM 13.7.1(16),
         --  we raise CE, and also include the zero case here. Yes, the RM says
         --  PE, but this really is so obviously more like a constraint error.

         if Is_Stoele_Mod and then (not ROK or else Rlo <= 0) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Op_Le (Loc,
                    Left_Opnd  =>
                      Duplicate_Subexpr_No_Checks (Expression (Right)),
                    Right_Opnd => Make_Integer_Literal (Loc, 0)),
                Reason => CE_Overflow_Check_Failed));
            return;
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

            if (not ROK or else (Rlo <= (-1) and then (-1) <= Rhi))
              and then (not LOK or else Llo = LLB)
              and then not CodePeer_Mode
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

      Ltyp : constant Entity_Id := Etype (Lop);
      Rtyp : constant Entity_Id := Etype (Rop);
      Typ  : Entity_Id          := Etype (N);

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

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Multiply then
            return;
         end if;
      end if;

      --  Do required fixup of universal fixed operation

      if Typ = Universal_Fixed then
         Fixup_Universal_Fixed_Operation (N);
         Typ := Etype (N);
      end if;

      --  Multiplications with fixed-point results

      if Is_Fixed_Point_Type (Typ) then

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

      --  Other cases of multiplication of fixed-point operands

      elsif Is_Fixed_Point_Type (Ltyp) or else Is_Fixed_Point_Type (Rtyp) then
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
      end if;

      --  Overflow checks for floating-point if -gnateF mode active

      Check_Float_Op_Overflow (N);

      Expand_Nonbinary_Modular_Op (N);
   end Expand_N_Op_Multiply;

   --------------------
   -- Expand_N_Op_Ne --
   --------------------

   procedure Expand_N_Op_Ne (N : Node_Id) is
      Typ : constant Entity_Id := Etype (Left_Opnd (N));

   begin
      --  Case of elementary type with standard operator. But if unnesting,
      --  handle elementary types whose Equivalent_Types are records because
      --  there may be padding or undefined fields.

      if Is_Elementary_Type (Typ)
        and then Sloc (Entity (N)) = Standard_Location
        and then not (Ekind (Typ) in E_Class_Wide_Type
                              | E_Class_Wide_Subtype
                              | E_Access_Subprogram_Type
                              | E_Access_Protected_Subprogram_Type
                              | E_Anonymous_Access_Protected_Subprogram_Type
                              | E_Exception_Type
                        and then Present (Equivalent_Type (Typ))
                        and then Is_Record_Type (Equivalent_Type (Typ)))
      then
         Binary_Op_Validity_Checks (N);

         --  Deal with overflow checks in MINIMIZED/ELIMINATED mode and if
         --  means we no longer have a /= operation, we are all done.

         if Minimized_Eliminated_Overflow_Check (Left_Opnd (N)) then
            Expand_Compare_Minimize_Eliminate_Overflow (N);
         end if;

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

         --  Try to narrow the operation

         if Typ = Universal_Integer and then Nkind (N) = N_Op_Ne then
            Narrow_Large_Operation (N);
         end if;

      --  For all cases other than elementary types, we rewrite node as the
      --  negation of an equality operation, and reanalyze. The equality to be
      --  used is defined in the same scope and has the same signature. This
      --  signature must be set explicitly since in an instance it may not have
      --  the same visibility as in the generic unit. This avoids duplicating
      --  or factoring the complex code for record/array equality tests etc.

      --  This case is also used for the minimal expansion performed in
      --  GNATprove mode.

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
                    Left_Opnd  => Left_Opnd (N),
                    Right_Opnd => Right_Opnd (N)));

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

      --  No need for optimization in GNATprove mode, where we would rather see
      --  the original source expression.

      if not GNATprove_Mode then
         Optimize_Length_Comparison (N);
      end if;
   end Expand_N_Op_Ne;

   ---------------------
   -- Expand_N_Op_Not --
   ---------------------

   --  If the argument is other than a Boolean array type, there is no special
   --  expansion required, except for dealing with validity checks, and non-
   --  standard boolean representations.

   --  For the packed array case, we call the special routine in Exp_Pakd,
   --  except that if the component size is greater than one, we use the
   --  standard routine generating a gruesome loop (it is so peculiar to have
   --  packed arrays with non-standard Boolean representations anyway, so it
   --  does not matter that we do not handle this case efficiently).

   --  For the unpacked array case (and for the special packed case where we
   --  have non standard Booleans, as discussed above), we generate and insert
   --  into the tree the following function definition:

   --     function Nnnn (A : arr) is
   --       B : arr;
   --     begin
   --       for J in a'range loop
   --          B (J) := not A (J);
   --       end loop;
   --       return B;
   --     end Nnnn;

   --  Here arr is the actual subtype of the parameter (and hence always
   --  constrained). Then we replace the not with a call to this subprogram.

   procedure Expand_N_Op_Not (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (Right_Opnd (N));
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

         elsif Nkind (Opnd) in N_Op_And | N_Op_Or | N_Op_Xor
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

      Expand_Nonbinary_Modular_Op (N);
   end Expand_N_Op_Or;

   ----------------------
   -- Expand_N_Op_Plus --
   ----------------------

   procedure Expand_N_Op_Plus (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      Unary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);
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

   begin
      Binary_Op_Validity_Checks (N);

      --  Check for MINIMIZED/ELIMINATED overflow mode

      if Minimized_Eliminated_Overflow_Check (N) then
         Apply_Arithmetic_Overflow_Check (N);
         return;
      end if;

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Rem then
            return;
         end if;
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
      --  but it is useful with other back ends, and is certainly harmless.

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
      Lneg := not OK or else Lo < 0;

      Determine_Range (Left,  OK, Lo, Hi, Assume_Valid => True);
      Rneg := not OK or else Lo < 0;

      --  We won't mess with trying to find out if the left operand can really
      --  be the largest negative number (that's a pain in the case of private
      --  types and this is really marginal). We will just assume that we need
      --  the test if the left operand can be negative at all.

      if (Lneg and Rneg)
         and then not CodePeer_Mode
      then
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
   end Expand_N_Op_Rotate_Left;

   ------------------------------
   -- Expand_N_Op_Rotate_Right --
   ------------------------------

   procedure Expand_N_Op_Rotate_Right (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);
   end Expand_N_Op_Rotate_Right;

   ----------------------------
   -- Expand_N_Op_Shift_Left --
   ----------------------------

   --  Note: nothing in this routine depends on left as opposed to right shifts
   --  so we share the routine for expanding shift right operations.

   procedure Expand_N_Op_Shift_Left (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);
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

      --  Try to narrow the operation

      if Typ = Universal_Integer then
         Narrow_Large_Operation (N);

         if Nkind (N) /= N_Op_Subtract then
            return;
         end if;
      end if;

      --  N - 0 = N for integer types

      if Is_Integer_Type (Typ)
        and then Compile_Time_Known_Value (Right_Opnd (N))
        and then Expr_Value (Right_Opnd (N)) = Uint_0
      then
         Rewrite (N, Left_Opnd (N));
         return;
      end if;

      --  Arithmetic overflow checks for signed integer/fixed point types

      if Is_Signed_Integer_Type (Typ) or else Is_Fixed_Point_Type (Typ) then
         Apply_Arithmetic_Overflow_Check (N);
      end if;

      --  Overflow checks for floating-point if -gnateF mode active

      Check_Float_Op_Overflow (N);

      Expand_Nonbinary_Modular_Op (N);
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

      Expand_Nonbinary_Modular_Op (N);
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
      Loc         : constant Source_Ptr := Sloc (N);
      Operand     : constant Node_Id    := Expression (N);
      Target_Type : constant Entity_Id  := Entity (Subtype_Mark (N));

   begin
      --  Nothing to do if the operand is an identical qualified expression

      if Nkind (Operand) = N_Qualified_Expression
        and then Entity (Subtype_Mark (Operand)) = Target_Type
      then
         Rewrite (N, Relocate_Node (Operand));
         return;

      --  An allocator expects a qualified expression in all cases

      elsif Nkind (Parent (N)) = N_Allocator then
         null;

      --  Distribute the qualified expression into the dependent expressions
      --  of a delayed conditional expression. The goal is to enable further
      --  optimizations, for example within a return statement, by exposing
      --  the conditional expression.

      elsif Nkind (Operand) = N_Case_Expression
        and then Expansion_Delayed (Operand)
      then
         declare
            New_Alts : constant List_Id := New_List;
            New_Case : constant Node_Id :=
              Make_Case_Expression (Loc,
                Expression   => Relocate_Node (Expression (Operand)),
                Alternatives => New_Alts);

            Alt     : Node_Id;
            New_Alt : Node_Id;

         begin
            Alt := First (Alternatives (Operand));
            while Present (Alt) loop
               New_Alt :=
                 Make_Case_Expression_Alternative (Sloc (Alt),
                   Discrete_Choices => Discrete_Choices (Alt),
                   Expression       =>
                     Make_Qualified_Expression (Loc,
                       Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                       Expression   => Relocate_Node (Expression (Alt))));
               Append_To (New_Alts, New_Alt);
               Set_Actions (New_Alt, Actions (Alt));

               Next (Alt);
            end loop;

            Rewrite (N, New_Case);
            Analyze_And_Resolve (N);
            return;
         end;

      elsif Nkind (Operand) = N_If_Expression
        and then Expansion_Delayed (Operand)
      then
         declare
            Cond   : constant Node_Id := First (Expressions (Operand));
            Thenx  : constant Node_Id := Next (Cond);
            Elsex  : constant Node_Id := Next (Thenx);
            New_If : constant Node_Id :=
              Make_If_Expression (Loc,
                Expressions => New_List (
                  Relocate_Node (Cond),
                  Make_Qualified_Expression (Loc,
                    Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                    Expression   => Relocate_Node (Thenx)),
                  Make_Qualified_Expression (Loc,
                    Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                    Expression   => Relocate_Node (Elsex))));

         begin
            Set_Then_Actions (New_If, Then_Actions (Operand));
            Set_Else_Actions (New_If, Else_Actions (Operand));

            Rewrite (N, New_If);
            Analyze_And_Resolve (N);
            return;
         end;
      end if;

      --  Do validity check if validity checking operands

      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Operand);
      end if;

      Freeze_Before (Operand, Target_Type);

      --  Apply possible constraint check

      Apply_Constraint_Check (Operand, Target_Type, No_Sliding => True);

      --  Apply possible predicate check but, for a delayed aggregate, the
      --  check is effectively delayed until after the aggregate is expanded
      --  into a series of assignments. Likewise for a conditional expression
      --  whose expansion has been delayed.

      if not Is_Delayed_Aggregate (Operand)
        and then not Is_Delayed_Conditional_Expression (Operand)
      then
         Apply_Predicate_Check (Operand, Target_Type);
      end if;

      if Do_Range_Check (Operand) then
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
      Var       : Entity_Id;

   begin
      --  Ensure that the bound variable as well as the type of Name of the
      --  Iter_Spec if present are properly frozen. We must do this before
      --  expansion because the expression is about to be converted into a
      --  loop, and resulting freeze nodes may end up in the wrong place in the
      --  tree.

      if Present (Iter_Spec) then
         Var := Defining_Identifier (Iter_Spec);
      else
         Var := Defining_Identifier (Loop_Spec);
      end if;

      declare
         P : Node_Id := Parent (N);
      begin
         while Nkind (P) in N_Subexpr loop
            P := Parent (P);
         end loop;

         if Present (Iter_Spec) then
            Freeze_Before (P, Etype (Name (Iter_Spec)));
         end if;

         Freeze_Before (P, Etype (Var));
      end;

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
      Ptyp  : constant Entity_Id  := Underlying_Type (Etype (P));
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
      --  Deal with discriminant check required

      if Do_Discriminant_Check (N) then
         if Present (Discriminant_Checking_Func
                      (Original_Record_Component (Entity (S))))
         then
            --  Present the discriminant checking function to the backend, so
            --  that it can inline the call to the function.

            Add_Inlined_Body
              (Discriminant_Checking_Func
                (Original_Record_Component (Entity (S))),
               N);

            --  Now reset the flag and generate the call

            Set_Do_Discriminant_Check (N, False);
            Generate_Discriminant_Check (N);

         --  In the case of Unchecked_Union, no discriminant checking is
         --  actually performed.

         else
            if not Is_Unchecked_Union
                    (Implementation_Base_Type (Etype (Prefix (N))))
              and then not Is_Predefined_Unit (Get_Source_Unit (N))
            then
               Error_Msg_N
                 ("sorry - unable to generate discriminant check for" &
                    " reference to variant component &",
                  Selector_Name (N));
            end if;

            Set_Do_Discriminant_Check (N, False);
         end if;
      end if;

      --  Ada 2005 (AI-318-02): If the prefix is a call to a build-in-place
      --  function, then additional actuals must be passed.

      if Is_Build_In_Place_Function_Call (P) then
         Make_Build_In_Place_Call_In_Anonymous_Context (P);

      --  Ada 2005 (AI-318-02): Specialization of the previous case for prefix
      --  containing build-in-place function calls whose returned object covers
      --  interface types.

      elsif Present (Unqual_BIP_Iface_Function_Call (P)) then
         Make_Build_In_Place_Iface_Call_In_Anonymous_Context (P);
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

            --  Don't do this on the left-hand side of an assignment statement.
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
                  --  of representation.

                  if Disc = Entity (Selector_Name (N))
                    and then (Is_Entity_Name (Dval)
                               or else Compile_Time_Known_Value (Dval)
                               or else Is_Subtype_Declaration)
                  then
                     --  Here we have the matching discriminant. Check for
                     --  the case of a discriminant of a component that is
                     --  constrained by an outer discriminant, which cannot
                     --  be optimized away.

                     if Denotes_Discriminant (Dval, Check_Concurrent => True)
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
                         Is_OK_Static_Expression
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
               Set := not Atomic_Synchronization_Disabled (E)
                        and then
                      not Atomic_Synchronization_Disabled (Etype (E));

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
      --  the back end cannot handle it properly, e.g. when packed types or
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

            elsif Nkind (Par) in N_Type_Conversion
                               | N_Parameter_Association
                               | N_Qualified_Expression
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

      Pref : constant Node_Id := Prefix (N);

   --  Start of processing for Expand_N_Slice

   begin
      --  Ada 2005 (AI-318-02): If the prefix is a call to a build-in-place
      --  function, then additional actuals must be passed.

      if Is_Build_In_Place_Function_Call (Pref) then
         Make_Build_In_Place_Call_In_Anonymous_Context (Pref);

      --  Ada 2005 (AI-318-02): Specialization of the previous case for prefix
      --  containing build-in-place function calls whose returned object covers
      --  interface types.

      elsif Present (Unqual_BIP_Iface_Function_Call (Pref)) then
         Make_Build_In_Place_Iface_Call_In_Anonymous_Context (Pref);
      end if;

      --  The remaining case to be handled is packed slices. We can leave
      --  packed slices as they are in the following situations:

      --    1. Right or left side of an assignment (we can handle this
      --       situation correctly in the assignment statement expansion).

      --    2. Prefix of indexed component (the slide is optimized away in this
      --       case, see the start of Expand_N_Indexed_Component.)

      --    3. Object renaming declaration, since we want the name of the
      --       slice, not the value.

      --    4. Argument to procedure call, since copy-in/copy-out handling may
      --       be required, and this is handled in the expansion of call
      --       itself.

      --    5. Prefix of an address attribute (this is an error which is caught
      --       elsewhere, and the expansion would interfere with generating the
      --       error message) or of a size attribute (because 'Size may change
      --       when applied to the temporary instead of the slice directly).

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
        and then (Attribute_Name (Parent (N)) = Name_Address
                   or else Attribute_Name (Parent (N)) = Name_Size)
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
      Operand_Acc  : Node_Id             := Operand;
      Target_Type  : Entity_Id           := Etype (N);
      Operand_Type : Entity_Id           := Etype (Operand);

      procedure Discrete_Range_Check;
      --  Handles generation of range check for discrete target value

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
      --  The Etype of the raise node is set to Target_Type. Note that in this
      --  case the rest of the processing should be skipped (i.e. the call to
      --  this procedure will be followed by "goto Done").

      procedure Real_Range_Check;
      --  Handles generation of range check for real target value

      function Has_Extra_Accessibility (Id : Entity_Id) return Boolean;
      --  True iff Present (Effective_Extra_Accessibility (Id)) successfully
      --  evaluates to True.

      function Statically_Deeper_Relation_Applies (Targ_Typ : Entity_Id)
        return Boolean;
      --  Given a target type for a conversion, determine whether the
      --  statically deeper accessibility rules apply to it.

      --------------------------
      -- Discrete_Range_Check --
      --------------------------

      --  Case of conversions to a discrete type. We let Generate_Range_Check
      --  do the heavy lifting, after converting a fixed-point operand to an
      --  appropriate integer type.

      procedure Discrete_Range_Check is
         Expr : Node_Id;
         Ityp : Entity_Id;

      --  Start of processing for Discrete_Range_Check

      begin
         --  Nothing more to do if conversion was rewritten

         if Nkind (N) /= N_Type_Conversion then
            return;
         end if;

         Expr := Expression (N);

         --  Clear the Do_Range_Check flag on Expr

         Set_Do_Range_Check (Expr, False);

         --  Nothing to do if range checks suppressed

         if Range_Checks_Suppressed (Target_Type) then
            return;
         end if;

         --  Nothing to do if expression is an entity on which checks have been
         --  suppressed.

         if Is_Entity_Name (Expr)
           and then Range_Checks_Suppressed (Entity (Expr))
         then
            return;
         end if;

         --  Before we do a range check, we have to deal with treating
         --  a fixed-point operand as an integer. The way we do this
         --  is simply to do an unchecked conversion to an appropriate
         --  integer type with the smallest size, so that we can suppress
         --  trivial checks.

         if Is_Fixed_Point_Type (Etype (Expr)) then
            Ityp := Small_Integer_Type_For
                      (Esize (Base_Type (Etype (Expr))), Uns => False);
            Rewrite (Expr, Unchecked_Convert_To (Ityp, Expr));
         end if;

         --  Reset overflow flag, since the range check will include
         --  dealing with possible overflow, and generate the check.

         Set_Do_Overflow_Check (N, False);

         Generate_Range_Check (Expr, Target_Type, CE_Range_Check_Failed);
      end Discrete_Range_Check;

      -----------------------------------
      -- Handle_Changed_Representation --
      -----------------------------------

      procedure Handle_Changed_Representation is
         Temp : Entity_Id;
         Decl : Node_Id;
         Odef : Node_Id;
         N_Ix : Node_Id;
         Cons : List_Id;

      begin
         --  Nothing else to do if no change of representation

         if Has_Compatible_Representation (Target_Type, Operand_Type) then
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
            --  from the actual value of the left-hand side.

            if not Is_Constrained (Target_Type) then
               if Has_Discriminants (Operand_Type) then

                  --  A change of representation can only apply to untagged
                  --  types. We need to build the constraint that applies to
                  --  the target type, using the constraints of the operand.
                  --  The analysis is complicated if there are both inherited
                  --  discriminants and constrained discriminants.
                  --  We iterate over the discriminants of the target, and
                  --  find the discriminant of the same name:

                  --  a) If there is a corresponding discriminant in the object
                  --  then the value is a selected component of the operand.

                  --  b) Otherwise the value of a constrained discriminant is
                  --  found in the stored constraint of the operand.

                  declare
                     Stored : constant Elist_Id :=
                                Stored_Constraint (Operand_Type);
                     --  Stored constraints of the operand. If present, they
                     --  correspond to the discriminants of the parent type.

                     Disc_O : Entity_Id;
                     --  Discriminant of the operand type. Its value in the
                     --  object is captured in a selected component.

                     Disc_T : Entity_Id;
                     --  Discriminant of the target type

                     Elmt : Elmt_Id;

                  begin
                     Disc_O := First_Discriminant (Operand_Type);
                     Disc_T := First_Discriminant (Target_Type);
                     Elmt   := (if Present (Stored)
                                 then First_Elmt (Stored)
                                 else No_Elmt);

                     Cons := New_List;
                     while Present (Disc_T) loop
                        if Present (Disc_O)
                          and then Chars (Disc_T) = Chars (Disc_O)
                        then
                           Append_To (Cons,
                             Make_Selected_Component (Loc,
                               Prefix        =>
                                 Duplicate_Subexpr_Move_Checks (Operand),
                               Selector_Name =>
                                 Make_Identifier (Loc, Chars (Disc_O))));
                           Next_Discriminant (Disc_O);

                        elsif Present (Elmt) then
                           Append_To (Cons, New_Copy_Tree (Node (Elmt)));
                        end if;

                        if Present (Elmt) then
                           Next_Elmt (Elmt);
                        end if;

                        Next_Discriminant (Disc_T);
                     end loop;
                  end;

               elsif Is_Array_Type (Operand_Type) then
                  N_Ix := First_Index (Target_Type);
                  Cons := New_List;

                  for J in 1 .. Number_Dimensions (Operand_Type) loop

                     --  We convert the bounds explicitly. We use an unchecked
                     --  conversion because bounds checks are done elsewhere.

                     Append_To (Cons,
                       Make_Range (Loc,
                         Low_Bound  =>
                           Unchecked_Convert_To (Etype (N_Ix),
                             Make_Attribute_Reference (Loc,
                               Prefix         =>
                                 Duplicate_Subexpr_No_Checks
                                   (Operand, Name_Req => True),
                               Attribute_Name => Name_First,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, J)))),

                         High_Bound =>
                           Unchecked_Convert_To (Etype (N_Ix),
                             Make_Attribute_Reference (Loc,
                               Prefix         =>
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
                   Constraint   =>
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
                  Name       => New_Occurrence_Of (Temp, Loc),
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

         Error_Msg_N ("accessibility check failure<<", N);
         Error_Msg_N ("\Program_Error [<<", N);
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
      --     typ (Tnn)

      --  This is necessary when there is a conversion of integer to float or
      --  to fixed-point to ensure that the correct checks are made. It is not
      --  necessary for the float-to-float case where it is enough to just set
      --  the Do_Range_Check flag on the expression.

      procedure Real_Range_Check is
         Btyp : constant Entity_Id := Base_Type (Target_Type);
         Lo   : constant Node_Id   := Type_Low_Bound  (Target_Type);
         Hi   : constant Node_Id   := Type_High_Bound (Target_Type);

         Conv   : Node_Id;
         Hi_Arg : Node_Id;
         Hi_Val : Node_Id;
         Lo_Arg : Node_Id;
         Lo_Val : Node_Id;
         Expr   : Entity_Id;
         Tnn    : Entity_Id;

      begin
         --  Nothing more to do if conversion was rewritten

         if Nkind (N) /= N_Type_Conversion then
            return;
         end if;

         Expr := Expression (N);

         --  Clear the Do_Range_Check flag on Expr

         Set_Do_Range_Check (Expr, False);

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

         if Is_Entity_Name (Expr)
           and then Range_Checks_Suppressed (Entity (Expr))
         then
            return;
         end if;

         --  Nothing to do if expression was rewritten into a float-to-float
         --  conversion, since this kind of conversion is handled elsewhere.

         if Is_Floating_Point_Type (Etype (Expr))
           and then Is_Floating_Point_Type (Target_Type)
         then
            return;
         end if;

         --  Nothing to do if bounds are all static and we can tell that the
         --  expression is within the bounds of the target. Note that if the
         --  operand is of an unconstrained floating-point type, then we do
         --  not trust it to be in range (might be infinite)

         declare
            S_Lo : constant Node_Id := Type_Low_Bound (Etype (Expr));
            S_Hi : constant Node_Id := Type_High_Bound (Etype (Expr));

         begin
            if (not Is_Floating_Point_Type (Etype (Expr))
                 or else Is_Constrained (Etype (Expr)))
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
                  if Is_Real_Type (Etype (Expr)) then
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
                     return;
                  end if;
               end;
            end if;
         end;

         --  Otherwise rewrite the conversion as described above

         Conv := Convert_To (Btyp, Expr);

         --  If a conversion is necessary, then copy the specific flags from
         --  the original one and also move the Do_Overflow_Check flag since
         --  this new conversion is to the base type.

         if Nkind (Conv) = N_Type_Conversion then
            Set_Conversion_OK  (Conv, Conversion_OK  (N));
            Set_Float_Truncate (Conv, Float_Truncate (N));
            Set_Rounded_Result (Conv, Rounded_Result (N));

            if Do_Overflow_Check (N) then
               Set_Do_Overflow_Check (Conv);
               Set_Do_Overflow_Check (N, False);
            end if;
         end if;

         Tnn := Make_Temporary (Loc, 'T', Conv);

         --  For a conversion from Float to Fixed where the bounds of the
         --  fixed-point type are static, we can obtain a more accurate
         --  fixed-point value by converting the result of the floating-
         --  point expression to an appropriate integer type, and then
         --  performing an unchecked conversion to the target fixed-point
         --  type. The range check can then use the corresponding integer
         --  value of the bounds instead of requiring further conversions.
         --  This preserves the identity:

         --        Fix_Val = Fixed_Type (Float_Type (Fix_Val))

         --  which used to fail when Fix_Val was a bound of the type and
         --  the 'Small was not a representable number.
         --  This transformation requires an integer type large enough to
         --  accommodate a fixed-point value.

         if Is_Ordinary_Fixed_Point_Type (Target_Type)
           and then Is_Floating_Point_Type (Etype (Expr))
           and then RM_Size (Btyp) <= System_Max_Integer_Size
           and then Nkind (Lo) = N_Real_Literal
           and then Nkind (Hi) = N_Real_Literal
         then
            declare
               Expr_Id : constant Entity_Id := Make_Temporary (Loc, 'T', Conv);
               Int_Typ : constant Entity_Id :=
                 Small_Integer_Type_For (RM_Size (Btyp), Uns => False);
               Trunc   : constant Boolean   := Float_Truncate (Conv);

            begin
               Conv := Convert_To (Int_Typ, Expression (Conv));
               Set_Float_Truncate (Conv, Trunc);

               --  Generate a temporary with the integer value. Required in the
               --  CCG compiler to ensure that run-time checks reference this
               --  integer expression (instead of the resulting fixed-point
               --  value because fixed-point values are handled by means of
               --  unsigned integer types).

               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Expr_Id,
                   Object_Definition   => New_Occurrence_Of (Int_Typ, Loc),
                   Constant_Present    => True,
                   Expression          => Conv));

               --  Create integer objects for range checking of result.

               Lo_Arg :=
                 Unchecked_Convert_To
                   (Int_Typ, New_Occurrence_Of (Expr_Id, Loc));

               Lo_Val :=
                 Make_Integer_Literal (Loc, Corresponding_Integer_Value (Lo));

               Hi_Arg :=
                 Unchecked_Convert_To
                   (Int_Typ, New_Occurrence_Of (Expr_Id, Loc));

               Hi_Val :=
                 Make_Integer_Literal (Loc, Corresponding_Integer_Value (Hi));

               --  Rewrite conversion as an integer conversion of the
               --  original floating-point expression, followed by an
               --  unchecked conversion to the target fixed-point type.

               Conv :=
                 Unchecked_Convert_To
                   (Target_Type, New_Occurrence_Of (Expr_Id, Loc));
            end;

         --  All other conversions

         else
            Lo_Arg := New_Occurrence_Of (Tnn, Loc);
            Lo_Val :=
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Target_Type, Loc),
                Attribute_Name => Name_First);

            Hi_Arg := New_Occurrence_Of (Tnn, Loc);
            Hi_Val :=
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Target_Type, Loc),
                Attribute_Name => Name_Last);
         end if;

         --  Build code for range checking. Note that checks are suppressed
         --  here since we don't want a recursive range check popping up.

         Insert_Actions (N, New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tnn,
             Object_Definition   => New_Occurrence_Of (Btyp, Loc),
             Constant_Present    => True,
             Expression          => Conv),

           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Or_Else (Loc,
                 Left_Opnd  =>
                   Make_Op_Lt (Loc,
                     Left_Opnd  => Lo_Arg,
                     Right_Opnd => Lo_Val),

                Right_Opnd =>
                  Make_Op_Gt (Loc,
                    Left_Opnd  => Hi_Arg,
                    Right_Opnd => Hi_Val)),
              Reason   => CE_Range_Check_Failed)),
           Suppress => All_Checks);

         Rewrite (Expr, New_Occurrence_Of (Tnn, Loc));
      end Real_Range_Check;

      -----------------------------
      -- Has_Extra_Accessibility --
      -----------------------------

      --  Returns true for a formal of an anonymous access type or for an Ada
      --  2012-style stand-alone object of an anonymous access type.

      function Has_Extra_Accessibility (Id : Entity_Id) return Boolean is
      begin
         if Is_Formal (Id) or else Ekind (Id) in E_Constant | E_Variable then
            return Present (Effective_Extra_Accessibility (Id));
         else
            return False;
         end if;
      end Has_Extra_Accessibility;

      ----------------------------------------
      -- Statically_Deeper_Relation_Applies --
      ----------------------------------------

      function Statically_Deeper_Relation_Applies (Targ_Typ : Entity_Id)
        return Boolean
      is
      begin
         --  The case where the target type is an anonymous access type is
         --  ignored since they have different semantics and get covered by
         --  various runtime checks depending on context.

         --  Note, the current implementation of this predicate is incomplete
         --  and doesn't fully reflect the rules given in RM 3.10.2 (19) and
         --  (19.1) ???

         return Ekind (Targ_Typ) /= E_Anonymous_Access_Type;
      end Statically_Deeper_Relation_Applies;

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
      --  an Assignment_OK attribute, which must be propagated to the operand
      --  and the Do_Range_Check flag on the operand must be cleared, if any.

      if Operand_Type = Target_Type then
         if Assignment_OK (N) then
            Set_Assignment_OK (Operand);
         end if;

         Set_Do_Range_Check (Operand, False);

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
      --  associated with it. This requires an invariant check. We insert
      --  a call:

      --        invariant_check (typ (expr))

      --  in the code, after removing side effects from the expression.
      --  This is clearer than replacing the conversion into an expression
      --  with actions, because the context may impose additional actions
      --  (tag checks, membership tests, etc.) that conflict with this
      --  rewriting (used previously).

      --  Note: the Comes_From_Source check, and then the resetting of this
      --  flag prevents what would otherwise be an infinite recursion.

      if Has_Invariants (Target_Type)
        and then Present (Invariant_Procedure (Target_Type))
        and then Comes_From_Source (N)
      then
         Set_Comes_From_Source (N, False);
         Remove_Side_Effects (N);
         Insert_Action (N, Make_Invariant_Call (Duplicate_Subexpr (N)));
         goto Done;

      --  AI12-0042: For a view conversion to a class-wide type occurring
      --  within the immediate scope of T, from a specific type that is
      --  a descendant of T (including T itself), an invariant check is
      --  performed on the part of the object that is of type T. (We don't
      --  need to explicitly check for the operand type being a descendant,
      --  just that it's a specific type, because the conversion would be
      --  illegal if it's specific and not a descendant -- downward conversion
      --  is not allowed).

      elsif Is_Class_Wide_Type (Target_Type)
        and then not Is_Class_Wide_Type (Etype (Expression (N)))
        and then Present (Invariant_Procedure (Root_Type (Target_Type)))
        and then Comes_From_Source (N)
        and then Within_Scope (Find_Enclosing_Scope (N), Scope (Target_Type))
      then
         Remove_Side_Effects (N);

         --  Perform the invariant check on a conversion to the class-wide
         --  type's root type.

         declare
            Root_Conv : constant Node_Id :=
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Root_Type (Target_Type), Loc),
                Expression   => Duplicate_Subexpr (Expression (N)));
         begin
            Set_Etype (Root_Conv, Root_Type (Target_Type));

            Insert_Action (N, Make_Invariant_Call (Root_Conv));
            goto Done;
         end;
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
      --  place, since it would be tricky to remove them here.

      if Integer_Promotion_Possible (N) then

         --  All conditions met, go ahead with transformation

         declare
            Opnd : Node_Id;
            L, R : Node_Id;

         begin
            Opnd := New_Op_Node (Nkind (Operand), Loc);

            R := Convert_To (Standard_Integer, Right_Opnd (Operand));
            Set_Right_Opnd (Opnd, R);

            if Nkind (Operand) in N_Binary_Op then
               L := Convert_To (Standard_Integer, Left_Opnd (Operand));
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

      --  If the conversion is from Universal_Integer and requires an overflow
      --  check, try to do an intermediate conversion to a narrower type first
      --  without overflow check, in order to avoid doing the overflow check
      --  in Universal_Integer, which can be a very large type.

      if Operand_Type = Universal_Integer and then Do_Overflow_Check (N) then
         declare
            Lo, Hi, Siz : Uint;
            OK          : Boolean;
            Typ         : Entity_Id;

         begin
            Determine_Range (Operand, OK, Lo, Hi, Assume_Valid => True);

            if OK then
               Siz := Get_Size_For_Range (Lo, Hi);

               --  We use the base type instead of the first subtype because
               --  overflow checks are done in the base type, so this avoids
               --  the need for useless conversions.

               if Siz < System_Max_Integer_Size then
                  Typ := Etype (Integer_Type_For (Siz, Uns => False));

                  Convert_To_And_Rewrite (Typ, Operand);
                  Analyze_And_Resolve
                    (Operand, Typ, Suppress => Overflow_Check);

                  Analyze_And_Resolve (N, Target_Type);
                  goto Done;
               end if;
            end if;
         end;
      end if;

      --  Do validity check if validity checking operands

      if Validity_Checks_On and Validity_Check_Operands then
         Ensure_Valid (Operand);
      end if;

      --  Special case of converting from non-standard boolean type

      if Is_Boolean_Type (Operand_Type)
        and then Nonzero_Is_True (Operand_Type)
      then
         Adjust_Condition (Operand);
         Set_Etype (Operand, Standard_Boolean);
         Operand_Type := Standard_Boolean;
      end if;

      --  Case of converting to an access type

      if Is_Access_Type (Target_Type) then
         --  In terms of accessibility rules, an anonymous access discriminant
         --  is not considered separate from its parent object.

         if Nkind (Operand) = N_Selected_Component
           and then Ekind (Entity (Selector_Name (Operand))) = E_Discriminant
           and then Ekind (Operand_Type) = E_Anonymous_Access_Type
         then
            Operand_Acc := Original_Node (Prefix (Operand));
         end if;

         --  If this type conversion was internally generated by the front end
         --  to displace the pointer to the object to reference an interface
         --  type and the original node was an Unrestricted_Access attribute,
         --  then skip applying accessibility checks (because, according to the
         --  GNAT Reference Manual, this attribute is similar to 'Access except
         --  that all accessibility and aliased view checks are omitted).

         if not Comes_From_Source (N)
           and then Is_Interface (Designated_Type (Target_Type))
           and then Nkind (Original_Node (N)) = N_Attribute_Reference
           and then Attribute_Name (Original_Node (N)) =
                      Name_Unrestricted_Access
         then
            null;

         --  Apply an accessibility check when the conversion operand is an
         --  access parameter (or a renaming thereof), unless conversion was
         --  expanded from an Unchecked_ or Unrestricted_Access attribute,
         --  or for the actual of a class-wide interface parameter. Note that
         --  other checks may still need to be applied below (such as tagged
         --  type checks).

         elsif Is_Entity_Name (Operand_Acc)
           and then Has_Extra_Accessibility (Entity (Operand_Acc))
           and then Ekind (Etype (Operand_Acc)) = E_Anonymous_Access_Type
           and then (Nkind (Original_Node (N)) /= N_Attribute_Reference
                      or else Attribute_Name (Original_Node (N)) = Name_Access)
           and then not No_Dynamic_Accessibility_Checks_Enabled (N)
         then
            if not Comes_From_Source (N)
              and then Nkind (Parent (N)) in N_Function_Call
                                           | N_Parameter_Association
                                           | N_Procedure_Call_Statement
              and then Is_Interface (Designated_Type (Target_Type))
              and then Is_Class_Wide_Type (Designated_Type (Target_Type))
            then
               null;

            else
               Apply_Accessibility_Check
                 (Operand, Target_Type, Insert_Node => Operand);
            end if;

         --  If the level of the operand type is statically deeper than the
         --  level of the target type, then force Program_Error. Note that this
         --  can only occur for cases where the attribute is within the body of
         --  an instantiation, otherwise the conversion will already have been
         --  rejected as illegal.

         --  Note: warnings are issued by the analyzer for the instance cases,
         --  and, since we are late in expansion, a check is performed to
         --  verify that neither the target type nor the operand type are
         --  internally generated - as this can lead to spurious errors when,
         --  for example, the operand type is a result of BIP expansion.

         elsif In_Instance_Body
           and then Statically_Deeper_Relation_Applies (Target_Type)
           and then not Is_Internal (Target_Type)
           and then not Is_Internal (Operand_Type)
           and then
             Type_Access_Level (Operand_Type) > Type_Access_Level (Target_Type)
         then
            Raise_Accessibility_Error;
            goto Done;

         --  When the operand is a selected access discriminant the check needs
         --  to be made against the level of the object denoted by the prefix
         --  of the selected name. Force Program_Error for this case as well
         --  (this accessibility violation can only happen if within the body
         --  of an instantiation).

         elsif In_Instance_Body
           and then Ekind (Operand_Type) = E_Anonymous_Access_Type
           and then Nkind (Operand) = N_Selected_Component
           and then Ekind (Entity (Selector_Name (Operand))) = E_Discriminant
           and then Static_Accessibility_Level (Operand, Zero_On_Dynamic_Level)
                      > Type_Access_Level (Target_Type)
         then
            Raise_Accessibility_Error;
            goto Done;
         end if;
      end if;

      --  Generate a tag check for view conversions of mutably tagged objects,
      --  which are special in nature and require selecting the tag component
      --  from the class-wide equivalent type.

      --  Possibly this could be combined with the logic below for better code
      --  reuse ???

      if Is_View_Conversion (N)
        and then Is_Variable (Operand)
        and then Is_Class_Wide_Equivalent_Type (Etype (Operand))
      then
         --  Generate:
         --    [Constraint_Error when Operand.Tag /= Root_Type]

         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Ne (Loc,
                 Left_Opnd  =>
                   Make_Selected_Component (Loc,
                     Prefix        => Duplicate_Subexpr_No_Checks (Operand),
                     Selector_Name => Make_Identifier (Loc, Name_uTag)),
                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix         => New_Occurrence_Of (Target_Type, Loc),
                     Attribute_Name => Name_Tag)),
             Reason   => CE_Tag_Check_Failed));

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

      elsif (Is_Access_Type (Target_Type)
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
                   Reason    => CE_Tag_Check_Failed),
                 Suppress => All_Checks);
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

            if Is_Interface (Actual_Op_Typ)
                 or else
               Is_Interface (Actual_Targ_Typ)
            then
               Expand_Interface_Conversion (N);
               goto Done;
            end if;

            --  Create a runtime tag check for a downward CW type conversion

            if Is_Class_Wide_Type (Actual_Op_Typ)
              and then Actual_Op_Typ /= Actual_Targ_Typ
              and then Root_Op_Typ /= Actual_Targ_Typ
              and then Is_Ancestor
                         (Root_Op_Typ, Actual_Targ_Typ, Use_Full_View => True)
              and then not Tag_Checks_Suppressed (Actual_Targ_Typ)
            then
               declare
                  Conv : Node_Id;
               begin
                  Make_Tag_Check (Class_Wide_Type (Actual_Targ_Typ));
                  Conv := Unchecked_Convert_To (Target_Type, Expression (N));
                  Rewrite (N, Conv);
                  Analyze_And_Resolve (N, Target_Type);
               end;
            end if;
         end Tagged_Conversion;

      --  Case of other access type conversions

      elsif Is_Access_Type (Target_Type) then
         Apply_Constraint_Check (Operand, Target_Type);

      --  Case of conversions from a fixed-point type

      --  These conversions require special expansion and processing, found in
      --  the Exp_Fixd package. We ignore cases where Conversion_OK is set,
      --  since from a semantic point of view, these are simple integer
      --  conversions, which do not need further processing except for the
      --  generation of range checks, which is performed at the end of this
      --  procedure.

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
            Set_Etype (N, Etype (Parent (N)));
            Target_Type := Etype (N);
            Set_Rounded_Result (N);
         end if;

         if Is_Fixed_Point_Type (Target_Type) then
            Expand_Convert_Fixed_To_Fixed (N);
         elsif Is_Integer_Type (Target_Type) then
            Expand_Convert_Fixed_To_Integer (N);
         else
            pragma Assert (Is_Floating_Point_Type (Target_Type));
            Expand_Convert_Fixed_To_Float (N);
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
         else
            pragma Assert (Is_Floating_Point_Type (Operand_Type));
            Expand_Convert_Float_To_Fixed (N);
         end if;

      --  Case of array conversions

      --  Expansion of array conversions, add required length/range checks but
      --  only do this if there is no change of representation. For handling of
      --  this case, see Handle_Changed_Representation.

      elsif Is_Array_Type (Target_Type) then
         if Is_Constrained (Target_Type) then
            Apply_Length_Check (Operand, Target_Type);
         else
            --  If the object has an unconstrained array subtype with fixed
            --  lower bound, then sliding to that bound may be needed.

            if Is_Fixed_Lower_Bound_Array_Subtype (Target_Type) then
               Expand_Sliding_Conversion (Operand, Target_Type);
            end if;

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

         if not Has_Compatible_Representation (Target_Type, Operand_Type)
           and then not Conversion_OK (N)
         then
            if Optimization_Level > 0
              and then Is_Boolean_Type (Target_Type)
            then
               --  Convert x(y) to (if y then x'(True) else x'(False)).
               --  Use literals, instead of indexing x'val, to enable
               --  further optimizations in the middle-end.

               Rewrite (N,
                 Make_If_Expression (Loc,
                   Expressions => New_List (
                     Operand,
                     Convert_To (Target_Type,
                                 New_Occurrence_Of (Standard_True, Loc)),
                     Convert_To (Target_Type,
                                 New_Occurrence_Of (Standard_False, Loc)))));

            else
               --  Convert: x(y) to x'val (ytyp'pos (y))

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Target_Type, Loc),
                   Attribute_Name => Name_Val,
                   Expressions    => New_List (
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Operand_Type, Loc),
                       Attribute_Name => Name_Pos,
                       Expressions    => New_List (Operand)))));
            end if;

            Analyze_And_Resolve (N, Target_Type);
         end if;
      end if;

      --  At this stage, either the conversion node has been transformed into
      --  some other equivalent expression, or left as a conversion that can be
      --  handled by Gigi.

      --  The only remaining step is to generate a range check if we still have
      --  a type conversion at this stage and Do_Range_Check is set. Note that
      --  we need to deal with at most 8 out of the 9 possible cases of numeric
      --  conversions here, because the float-to-integer case is entirely dealt
      --  with by Apply_Float_Conversion_Check.

      if Nkind (N) = N_Type_Conversion
        and then Do_Range_Check (Expression (N))
      then
         --  Float-to-float conversions

         if Is_Floating_Point_Type (Target_Type)
           and then Is_Floating_Point_Type (Etype (Expression (N)))
         then
            --  Reset overflow flag, since the range check will include
            --  dealing with possible overflow, and generate the check.

            Set_Do_Overflow_Check (N, False);

            Generate_Range_Check
              (Expression (N), Target_Type, CE_Range_Check_Failed);

         --  Discrete-to-discrete conversions or fixed-point-to-discrete
         --  conversions when Conversion_OK is set.

         elsif Is_Discrete_Type (Target_Type)
           and then (Is_Discrete_Type (Etype (Expression (N)))
                      or else (Is_Fixed_Point_Type (Etype (Expression (N)))
                                and then Conversion_OK (N)))
         then
            --  If Address is either a source type or target type,
            --  suppress range check to avoid typing anomalies when
            --  it is a visible integer type.

            if Is_Descendant_Of_Address (Etype (Expression (N)))
              or else Is_Descendant_Of_Address (Target_Type)
            then
               Set_Do_Range_Check (Expression (N), False);
            else
               Discrete_Range_Check;
            end if;

         --  Conversions to floating- or fixed-point when Conversion_OK is set

         elsif Is_Floating_Point_Type (Target_Type)
           or else (Is_Fixed_Point_Type (Target_Type)
                     and then Conversion_OK (N))
         then
            Real_Range_Check;
         end if;

         pragma Assert (not Do_Range_Check (Expression (N)));
      end if;

      --  Here at end of processing

   <<Done>>
      --  Apply predicate check if required. Note that we can't just call
      --  Apply_Predicate_Check here, because the type looks right after
      --  the conversion and it would omit the check. The Comes_From_Source
      --  guard is necessary to prevent infinite recursions when we generate
      --  internal conversions for the purpose of checking predicates.

      --  A view conversion of a tagged object is an object and can appear
      --  in an assignment context, in which case no predicate check applies
      --  to the now-dead value.

      if Nkind (Parent (N)) = N_Assignment_Statement
        and then N = Name (Parent (N))
      then
         null;

      elsif Predicate_Enabled (Target_Type)
        and then Target_Type /= Operand_Type
        and then Comes_From_Source (N)
      then
         declare
            New_Expr : constant Node_Id := Duplicate_Subexpr (N);

         begin
            --  Avoid infinite recursion on the subsequent expansion of the
            --  copy of the original type conversion. When needed, a range
            --  check has already been applied to the expression.

            Set_Comes_From_Source (New_Expr, False);
            Insert_Action (N,
              Make_Predicate_Check (Target_Type, New_Expr),
              Suppress => Range_Check);
         end;
      end if;
   end Expand_N_Type_Conversion;

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
         if Assignment_OK (N) then
            Set_Assignment_OK (Operand);
         end if;

         Rewrite (N, Operand);
         return;
      end if;

      --  Nothing to do if conversion is safe

      if Safe_Unchecked_Type_Conversion (N) then
         return;
      end if;

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

   --  The expression is folded by the back end for adjacent fields. This
   --  function is called for tagged record in only one occasion: for imple-
   --  menting predefined primitive equality (see Predefined_Primitives_Bodies)
   --  otherwise the primitive "=" is used directly.

   function Expand_Record_Equality
     (Nod : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id;
      Rhs : Node_Id) return Node_Id
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
         Comp : Entity_Id := C;

      begin
         while Present (Comp) loop
            --  Skip inherited components

            --  Note: for a tagged type, we always generate the "=" primitive
            --  for the base type (not on the first subtype), so the test for
            --  Comp /= Original_Record_Component (Comp) is True for inherited
            --  components only.

            if (Is_Tagged_Type (Typ)
                and then Comp /= Original_Record_Component (Comp))

            --  Skip _Tag

              or else Chars (Comp) = Name_uTag

            --  Skip interface elements (secondary tags???)

              or else Is_Interface (Etype (Comp))
            then
               Next_Component_Or_Discriminant (Comp);
            else
               return Comp;
            end if;
         end loop;

         return Empty;
      end Element_To_Compare;

   --  Start of processing for Expand_Record_Equality

   begin
      --  Generates the following code: (assuming that Typ has one Discr and
      --  component C2 is also a record)

      --  Lhs.Discr1 = Rhs.Discr1
      --    and then Lhs.C1 = Rhs.C1
      --    and then Lhs.C2.C1=Rhs.C2.C1 and then ... Lhs.C2.Cn=Rhs.C2.Cn
      --    and then ...
      --    and then Lhs.Cmpn = Rhs.Cmpn

      Result := New_Occurrence_Of (Standard_True, Loc);
      C := Element_To_Compare (First_Component_Or_Discriminant (Typ));
      while Present (C) loop
         declare
            New_Lhs : Node_Id;
            New_Rhs : Node_Id;
            Check   : Node_Id;
            Lhs_Sel : Node_Id;
            Rhs_Sel : Node_Id;
            C_Typ   : Entity_Id := Etype (C);

         begin
            if First_Time then
               New_Lhs := Lhs;
               New_Rhs := Rhs;
            else
               New_Lhs := New_Copy_Tree (Lhs);
               New_Rhs := New_Copy_Tree (Rhs);
            end if;

            Lhs_Sel :=
              Make_Selected_Component (Loc,
                Prefix        => New_Lhs,
                Selector_Name => New_Occurrence_Of (C, Loc));
            Rhs_Sel :=
               Make_Selected_Component (Loc,
                 Prefix        => New_Rhs,
                 Selector_Name => New_Occurrence_Of (C, Loc));

            --  Generate mutably tagged conversions in case we encounter a
            --  special class-wide equivalent type.

            if Is_Mutably_Tagged_CW_Equivalent_Type (Etype (C)) then
               C_Typ := Corresponding_Mutably_Tagged_Type (Etype (C));
               Make_Mutably_Tagged_Conversion (Lhs_Sel, C_Typ);
               Make_Mutably_Tagged_Conversion (Rhs_Sel, C_Typ);
            end if;

            Check :=
              Expand_Composite_Equality
                (Outer_Type => Typ,
                 Nod        => Nod,
                 Comp_Type  => C_Typ,
                 Lhs        => Lhs_Sel,
                 Rhs        => Rhs_Sel);

            --  If some (sub)component is an unchecked_union, the whole
            --  operation will raise program error.

            if Nkind (Check) = N_Raise_Program_Error then
               Result := Check;
               Set_Etype (Result, Standard_Boolean);
               exit;
            else
               if First_Time then
                  Result := Check;

               --  Generate logical "and" for CodePeer to simplify the
               --  generated code and analysis.

               elsif CodePeer_Mode then
                  Result :=
                    Make_Op_And (Loc,
                      Left_Opnd  => Result,
                      Right_Opnd => Check);

               else
                  Result :=
                    Make_And_Then (Loc,
                      Left_Opnd  => Result,
                      Right_Opnd => Check);
               end if;
            end if;
         end;

         First_Time := False;
         C := Element_To_Compare (Next_Component_Or_Discriminant (C));
      end loop;

      return Result;
   end Expand_Record_Equality;

   ---------------------------
   -- Expand_Set_Membership --
   ---------------------------

   procedure Expand_Set_Membership (N : Node_Id) is
      Lop : constant Node_Id := Left_Opnd (N);

      function Make_Cond (Alt : Node_Id) return Node_Id;
      --  If the alternative is a subtype mark, create a simple membership
      --  test. Otherwise create an equality test for it.

      ---------------
      -- Make_Cond --
      ---------------

      function Make_Cond (Alt : Node_Id) return Node_Id is
         Cond : Node_Id;
         L    : constant Node_Id := New_Copy_Tree (Lop);
         R    : constant Node_Id := Relocate_Node (Alt);

      begin
         if (Is_Entity_Name (Alt) and then Is_Type (Entity (Alt)))
           or else Nkind (Alt) = N_Range
         then
            Cond := Make_In (Sloc (Alt), Left_Opnd  => L, Right_Opnd => R);

         else
            Cond := Make_Op_Eq (Sloc (Alt), Left_Opnd  => L, Right_Opnd => R);
            Resolve_Membership_Equality (Cond, Etype (Alt));
         end if;

         return Cond;
      end Make_Cond;

      --  Local variables

      Alt : Node_Id;
      Res : Node_Id := Empty;

   --  Start of processing for Expand_Set_Membership

   begin
      Remove_Side_Effects (Lop);

      --  We use left associativity as in the equivalent boolean case. This
      --  kind of canonicalization helps the optimizer of the code generator.

      Alt := First (Alternatives (N));
      while Present (Alt) loop
         Evolve_Or_Else (Res, Make_Cond (Alt));
         Next (Alt);
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

      function Useful (Actions : List_Id) return Boolean;
      --  Return True if Actions contains useful nodes to process

      ------------
      -- Useful --
      ------------

      function Useful (Actions : List_Id) return Boolean is
         Action : Node_Id;
      begin
         Action := First (Actions);

         --  For now "useful" means not N_Variable_Reference_Marker. Consider
         --  stripping other nodes in the future.

         while Present (Action) loop
            if Nkind (Action) /= N_Variable_Reference_Marker then
               return True;
            end if;

            Next (Action);
         end loop;

         return False;
      end Useful;

   --  Start of processing for Expand_Short_Circuit_Operator

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

      if Useful (Actions (N)) then
         Actlist := Actions (N);

         --  Use an Expression_With_Actions node for the right operand of the
         --  short-circuit form. Note that this solves traceability problems
         --  for coverage analysis at the object level.

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

         --  Change (Left and then True), (Left or else False) to Left. Note
         --  that we know there are no actions associated with the right
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
   -- Expand_Unchecked_Union_Equality --
   -------------------------------------

   procedure Expand_Unchecked_Union_Equality (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Eq  : constant Entity_Id  := Entity (Name (N));
      Lhs : constant Node_Id    := First_Actual (N);
      Rhs : constant Node_Id    := Next_Actual (Lhs);

      function Get_Discr_Values (Op : Node_Id; Lhs : Boolean) return Elist_Id;
      --  Return the list of inferred discriminant values for Op

      ----------------------
      -- Get_Discr_Values --
      ----------------------

      function Get_Discr_Values (Op : Node_Id; Lhs : Boolean) return Elist_Id
      is
         Typ    : constant Entity_Id := Etype (Op);
         Values : constant Elist_Id  := New_Elmt_List;

         function Get_Extra_Formal (Nam : Name_Id) return Entity_Id;
         --  Return the extra formal Nam from the current scope, which must be
         --  an equality function for an unchecked union type.

         ----------------------
         -- Get_Extra_Formal --
         ----------------------

         function Get_Extra_Formal (Nam : Name_Id) return Entity_Id is
            Func : constant Entity_Id := Current_Scope;

            Formal : Entity_Id;

         begin
            pragma Assert (Ekind (Func) = E_Function);

            Formal := Extra_Formals (Func);
            while Present (Formal) loop
               if Chars (Formal) = Nam then
                  return Formal;
               end if;

               Formal := Extra_Formal (Formal);
            end loop;

            --  An extra formal of the proper name must be found

            raise Program_Error;
         end Get_Extra_Formal;

         --  Local variables

         Discr  : Entity_Id;

      --  Start of processing for Get_Discr_Values

      begin
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

         --     ... Obj1 = Obj2 ...

         --     Generated code:

         --     if not (uu_typeEQ (obj1.comp, obj2.comp,
         --                        obj1.discr, obj2.discr)) then

         --  In this case we can directly reference the discriminants of
         --  the enclosing record.

         if Nkind (Op) = N_Selected_Component
           and then Has_Per_Object_Constraint (Entity (Selector_Name (Op)))
         then
            --  If enclosing record is an Unchecked_Union, use formals
            --  corresponding to each discriminant. The name of the
            --  formal is that of the discriminant, with added suffix,
            --  see Exp_Ch3.Build_Variant_Record_Equality for details.

            if Is_Unchecked_Union (Scope (Entity (Selector_Name (Op)))) then
               Discr :=
                 First_Discriminant
                   (Scope (Entity (Selector_Name (Op))));
               while Present (Discr) loop
                  Append_Elmt
                    (New_Occurrence_Of
                      (Get_Extra_Formal
                        (New_External_Name
                          (Chars (Discr), (if Lhs then 'A' else 'B'))), Loc),
                     To => Values);
                  Next_Discriminant (Discr);
               end loop;

            --  If enclosing record is of a non-Unchecked_Union type, it
            --  is possible to reference its discriminants directly.

            else
               Discr := First_Discriminant (Typ);
               while Present (Discr) loop
                  Append_Elmt
                    (Make_Selected_Component (Loc,
                       Prefix        => Prefix (Op),
                       Selector_Name =>
                         New_Copy
                           (Get_Discriminant_Value (Discr,
                               Typ,
                               Stored_Constraint (Typ)))),
                     To => Values);
                  Next_Discriminant (Discr);
               end loop;
            end if;

         --  Otherwise operand is on object with a constrained type.
         --  Infer the discriminant values from the constraint.

         else
            Discr := First_Discriminant (Typ);
            while Present (Discr) loop
               Append_Elmt
                 (New_Copy
                    (Get_Discriminant_Value (Discr,
                       Typ,
                       Stored_Constraint (Typ))),
                  To => Values);
               Next_Discriminant (Discr);
            end loop;
         end if;

         return Values;
      end Get_Discr_Values;

   --  Start of processing for Expand_Unchecked_Union_Equality

   begin
      --  Guard against repeated invocation on the same node

      if Present (Next_Actual (Rhs)) then
         return;
      end if;

      --  If we can infer the discriminants of the operands, make a call to Eq

      if Has_Inferable_Discriminants (Lhs)
           and then
         Has_Inferable_Discriminants (Rhs)
      then
         declare
            Lhs_Values : constant Elist_Id := Get_Discr_Values (Lhs, True);
            Rhs_Values : constant Elist_Id := Get_Discr_Values (Rhs, False);

            Formal : Entity_Id;
            L_Elmt : Elmt_Id;
            R_Elmt : Elmt_Id;

         begin
            --  Add the inferred discriminant values as extra actuals

            Formal := Extra_Formals (Eq);
            L_Elmt := First_Elmt (Lhs_Values);
            R_Elmt := First_Elmt (Rhs_Values);

            while Present (L_Elmt) loop
               Analyze_And_Resolve (Node (L_Elmt), Etype (Formal));
               Add_Extra_Actual_To_Call (N, Formal, Node (L_Elmt));

               Formal := Extra_Formal (Formal);

               Analyze_And_Resolve (Node (R_Elmt), Etype (Formal));
               Add_Extra_Actual_To_Call (N, Formal, Node (R_Elmt));

               Formal := Extra_Formal (Formal);
               Next_Elmt (L_Elmt);
               Next_Elmt (R_Elmt);
            end loop;
         end;

      --  Ada 2005 (AI-216): Program_Error is raised when evaluating
      --  the predefined equality operator for an Unchecked_Union type
      --  if either of the operands lack inferable discriminants.

      else
         Insert_Action (N,
           Make_Raise_Program_Error (Loc,
             Reason => PE_Unchecked_Union_Restriction));

         --  Give a warning on source equalities only, otherwise the message
         --  may appear out of place due to internal use. It is unconditional
         --  because it is required by the language.

         if Comes_From_Source (Original_Node (N)) then
            Error_Msg_N
              ("Unchecked_Union discriminants cannot be determined??", N);
            Error_Msg_N
              ("\Program_Error will be raised for equality operation??", N);
         end if;

         Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
      end if;
   end Expand_Unchecked_Union_Equality;

   ------------------------------------
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

      if Etype (Conv) = Universal_Real
        and then Nkind (Parent (Conv)) = N_Attribute_Reference
        and then Attribute_Name (Parent (Conv)) = Name_Round
      then
         Set_Etype (N, Base_Type (Etype (Parent (Conv))));
         Set_Rounded_Result (N);

      --  Normal case where type comes from conversion above us

      else
         Set_Etype (N, Base_Type (Etype (Conv)));
      end if;
   end Fixup_Universal_Fixed_Operation;

   ----------------------------
   -- Get_First_Index_Bounds --
   ----------------------------

   procedure Get_First_Index_Bounds (T : Entity_Id; Lo, Hi : out Uint) is
      Typ  : Entity_Id;

   begin
      pragma Assert (Is_Array_Type (T));

      --  This follows Sem_Eval.Compile_Time_Known_Bounds

      if Ekind (T) = E_String_Literal_Subtype then
         Lo := Expr_Value (String_Literal_Low_Bound (T));
         Hi := Lo + String_Literal_Length (T) - 1;

      else
         Typ := Underlying_Type (Etype (First_Index (T)));

         Lo := Expr_Value (Type_Low_Bound (Typ));
         Hi := Expr_Value (Type_High_Bound (Typ));
      end if;
   end Get_First_Index_Bounds;

   ------------------------
   -- Get_Size_For_Range --
   ------------------------

   function Get_Size_For_Range (Lo, Hi : Uint) return Uint is

      function Is_OK_For_Range (Siz : Uint) return Boolean;
      --  Return True if a signed integer with given size can cover Lo .. Hi

      --------------------------
      -- Is_OK_For_Range --
      --------------------------

      function Is_OK_For_Range (Siz : Uint) return Boolean is
         B : constant Uint := Uint_2 ** (Siz - 1);

      begin
         --  Test B = 2 ** (size - 1) (can accommodate -B .. +(B - 1))

         return Lo >= -B and then Hi >= -B and then Lo < B and then Hi < B;
      end Is_OK_For_Range;

   begin
      --  This is (almost always) the size of Integer

      if Is_OK_For_Range (Uint_32) then
         return Uint_32;

      --  Check 63

      elsif Is_OK_For_Range (Uint_63) then
         return Uint_63;

      --  This is (almost always) the size of Long_Long_Integer

      elsif Is_OK_For_Range (Uint_64) then
         return Uint_64;

      --  Check 127

      elsif Is_OK_For_Range (Uint_127) then
         return Uint_127;

      else
         return Uint_128;
      end if;
   end Get_Size_For_Range;

   -------------------------------------------
   -- Insert_Conditional_Object_Declaration --
   -------------------------------------------

   procedure Insert_Conditional_Object_Declaration
     (Obj_Id : Entity_Id;
      Expr   : Node_Id;
      Decl   : Node_Id)
   is
      Loc      : constant Source_Ptr := Sloc (Expr);
      Obj_Decl : constant Node_Id :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Obj_Id,
          Aliased_Present     => Aliased_Present (Decl),
          Constant_Present    => Constant_Present (Decl),
          Object_Definition   => New_Copy_Tree (Object_Definition (Decl)),
          Expression          => Relocate_Node (Expr));

      Master_Node_Decl : Node_Id;
      Master_Node_Id   : Entity_Id;

   begin
      --  If the expression is itself a conditional expression whose
      --  expansion has been delayed, analyze it again and expand it.

      if Is_Delayed_Conditional_Expression (Expression (Obj_Decl)) then
         Unanalyze_Delayed_Conditional_Expression (Expression (Obj_Decl));
      end if;

      Insert_Action (Expr, Obj_Decl);

      --  If the object needs finalization, we need to insert its Master_Node
      --  manually because 1) the machinery in Exp_Ch7 will not pick it since
      --  it will be declared in the arm of a conditional statement and 2) we
      --  cannot invoke Process_Transients_In_Expression on it since it is not
      --  a transient object (it has the lifetime of the original object).

      if Needs_Finalization (Base_Type (Etype (Obj_Id))) then
         Master_Node_Id := Make_Temporary (Loc, 'N');
         Master_Node_Decl :=
           Make_Master_Node_Declaration (Loc, Master_Node_Id, Obj_Id);

         --  The master is the innermost enclosing non-transient construct

         Insert_Action (Find_Hook_Context (Expr), Master_Node_Decl);

         --  Propagate the relaxed finalization semantics

         Set_Is_Independent
           (Master_Node_Id,
            Has_Relaxed_Finalization (Base_Type (Etype (Obj_Id))));

         --  Generate the attachment of the object to the Master_Node

         Attach_Object_To_Master_Node (Obj_Decl, Master_Node_Id);

         --  Mark the transient object to avoid double finalization

         Set_Is_Finalized_Transient (Obj_Id);
      end if;
   end Insert_Conditional_Object_Declaration;

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

      Context   : constant Node_Id    := Parent (N);
      Ptr_Typ   : constant Entity_Id  := Etype (N);
      Desig_Typ : constant Entity_Id  :=
                    Available_View (Designated_Type (Ptr_Typ));
      Loc       : constant Source_Ptr := Sloc (N);
      Pool      : constant Entity_Id  := Associated_Storage_Pool (Ptr_Typ);

      Addr      : Entity_Id;
      Alig      : Entity_Id;
      Deref     : Node_Id;
      Size      : Entity_Id;
      Size_Bits : Node_Id;
      Stmt      : Node_Id;

   --  Start of processing for Insert_Dereference_Action

   begin
      pragma Assert (Nkind (Context) = N_Explicit_Dereference);

      --  Do not re-expand a dereference which has already been processed by
      --  this routine.

      if Has_Dereference_Action (Context) then
         return;

      --  Do not perform this type of expansion for internally-generated
      --  dereferences.

      elsif not Comes_From_Source (Original_Node (Context)) then
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

      Size_Bits :=
        Make_Attribute_Reference (Loc,
          Prefix         => Deref,
          Attribute_Name => Name_Size);

      --  Special case of an unconstrained array: need to add descriptor size

      if Is_Array_Type (Desig_Typ)
        and then not Is_Constrained (First_Subtype (Desig_Typ))
      then
         Size_Bits :=
           Make_Op_Add (Loc,
             Left_Opnd  =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (First_Subtype (Desig_Typ), Loc),
                 Attribute_Name => Name_Descriptor_Size),
             Right_Opnd => Size_Bits);
      end if;

      Size := Make_Temporary (Loc, 'S');
      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Size,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Storage_Count), Loc),
          Expression          =>
            Make_Op_Divide (Loc,
              Left_Opnd  => Size_Bits,
              Right_Opnd => Make_Integer_Literal (Loc, System_Storage_Unit))));

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

      --  The address manipulation is not performed for access types that are
      --  subject to pragma No_Heap_Finalization because the two pointers do
      --  not exist in the first place. Likewise for designated types that are
      --  subject to relaxed finalization.

      if No_Heap_Finalization (Ptr_Typ) then
         null;

      elsif Needs_Finalization (Desig_Typ)
        and then not Has_Relaxed_Finalization (Desig_Typ)
      then
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

         if Is_Class_Wide_Type (Desig_Typ) then
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

      Set_Has_Dereference_Action (Context);

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

           and then Nkind (Operand) in
                      N_Op_Abs   | N_Op_Add      | N_Op_Divide | N_Op_Expon |
                      N_Op_Minus | N_Op_Multiply | N_Op_Subtract;
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
      J : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uJ);

      C   : Entity_Id;

      A_J : Node_Id;
      B_J : Node_Id;
      C_J : Node_Id;
      Op  : Node_Id;

      Formals        : List_Id;
      Func_Name      : Entity_Id;
      Func_Body      : Node_Id;
      Loop_Statement : Node_Id;

   begin
      C   := Make_Defining_Identifier (Loc, Name_uC);

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
      --  The MINIMIZED mode operates in Long_Long_Integer so we cannot use it
      --  if the type of the expression is already larger.

      return
        Is_Signed_Integer_Type (Etype (N))
          and then Overflow_Check_Mode in Minimized_Or_Eliminated
          and then not (Overflow_Check_Mode = Minimized
                         and then
                        Esize (Etype (N)) > Standard_Long_Long_Integer_Size);
   end Minimized_Eliminated_Overflow_Check;

   ----------------------------
   -- Narrow_Large_Operation --
   ----------------------------

   procedure Narrow_Large_Operation (N : Node_Id) is
      Kind   : constant Node_Kind := Nkind (N);
      Otyp   : constant Entity_Id := Etype (N);
      In_Rng : constant Boolean   := Kind = N_In;
      Binary : constant Boolean   := Kind in N_Binary_Op or else In_Rng;
      Compar : constant Boolean   := Kind in N_Op_Compare or else In_Rng;
      R      : constant Node_Id   := Right_Opnd (N);
      Typ    : constant Entity_Id := Etype (R);
      Tsiz   : constant Uint      := RM_Size (Typ);

      --  Local variables

      L          : Node_Id;
      Llo, Lhi   : Uint;
      Rlo, Rhi   : Uint;
      Lsiz, Rsiz : Uint;
      Nlo, Nhi   : Uint;
      Nsiz       : Uint;
      Ntyp       : Entity_Id;
      Nop        : Node_Id;
      OK         : Boolean;

   --  Start of processing for Narrow_Large_Operation

   begin
      --  First, determine the range of the left operand, if any

      if Binary then
         L := Left_Opnd (N);
         Determine_Range (L, OK, Llo, Lhi, Assume_Valid => True);
         if not OK then
            return;
         end if;

      else
         L   := Empty;
         Llo := Uint_0;
         Lhi := Uint_0;
      end if;

      --  Second, determine the range of the right operand, which can itself
      --  be a range, in which case we take the lower bound of the low bound
      --  and the upper bound of the high bound.

      if In_Rng then
         declare
            Zlo, Zhi : Uint;

         begin
            Determine_Range
              (Low_Bound (R), OK, Rlo, Zhi, Assume_Valid => True);
            if not OK then
               return;
            end if;

            Determine_Range
              (High_Bound (R), OK, Zlo, Rhi, Assume_Valid => True);
            if not OK then
               return;
            end if;
         end;

      else
         Determine_Range (R, OK, Rlo, Rhi, Assume_Valid => True);
         if not OK then
            return;
         end if;
      end if;

      --  Then compute a size suitable for each range

      if Binary then
         Lsiz := Get_Size_For_Range (Llo, Lhi);
      else
         Lsiz := Uint_0;
      end if;

      Rsiz := Get_Size_For_Range (Rlo, Rhi);

      --  Now compute the size of the narrower type

      if Compar then
         --  The type must be able to accommodate the operands

         Nsiz := UI_Max (Lsiz, Rsiz);

      else
         --  The type must be able to accommodate the operand(s) and result.

         --  Note that Determine_Range typically does not report the bounds of
         --  the value as being larger than those of the base type, which means
         --  that it does not report overflow (see also Enable_Overflow_Check).

         Determine_Range (N, OK, Nlo, Nhi, Assume_Valid => True);
         if not OK then
            return;
         end if;

         --  Therefore, if Nsiz is not lower than the size of the original type
         --  here, we cannot be sure that the operation does not overflow.

         Nsiz := Get_Size_For_Range (Nlo, Nhi);
         Nsiz := UI_Max (Nsiz, Lsiz);
         Nsiz := UI_Max (Nsiz, Rsiz);
      end if;

      --  If the size is not lower than the size of the original type, then
      --  there is no point in changing the type, except in the case where
      --  we can remove a conversion to the original type from an operand.

      if Nsiz >= Tsiz
        and then not (Binary
                       and then Nkind (L) = N_Type_Conversion
                       and then Entity (Subtype_Mark (L)) = Typ)
        and then not (Nkind (R) = N_Type_Conversion
                       and then Entity (Subtype_Mark (R)) = Typ)
      then
         return;
      end if;

      --  Now pick the narrower type according to the size. We use the base
      --  type instead of the first subtype because operations are done in
      --  the base type, so this avoids the need for useless conversions.

      if Nsiz <= System_Max_Integer_Size then
         Ntyp := Etype (Integer_Type_For (Nsiz, Uns => False));
      else
         return;
      end if;

      --  Finally, rewrite the operation in the narrower type, but make sure
      --  not to perform name resolution for the operator again.

      Nop := New_Op_Node (Kind, Sloc (N));
      if Nkind (N) in N_Has_Entity then
         Set_Entity (Nop, Entity (N));
      end if;

      if Binary then
         Set_Left_Opnd (Nop, Convert_To (Ntyp, L));
      end if;

      if In_Rng then
         Set_Right_Opnd (Nop,
           Make_Range (Sloc (N),
             Convert_To (Ntyp, Low_Bound (R)),
             Convert_To (Ntyp, High_Bound (R))));
      else
         Set_Right_Opnd (Nop, Convert_To (Ntyp, R));
      end if;

      Rewrite (N, Nop);

      if Compar then
         --  Analyze it with the comparison type and checks suppressed since
         --  the conversions of the operands cannot overflow.

         Analyze_And_Resolve (N, Otyp, Suppress => Overflow_Check);

      else
         --  Analyze it with the narrower type and checks suppressed, but only
         --  when we are sure that the operation does not overflow, see above.

         if Nsiz < Tsiz then
            Analyze_And_Resolve (N, Ntyp, Suppress => Overflow_Check);
         else
            Analyze_And_Resolve (N, Ntyp);
         end if;

         --  Put back a conversion to the original type

         Convert_To_And_Rewrite (Typ, N);
      end if;
   end Narrow_Large_Operation;

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

      Maybe_Superflat : Boolean;
      --  True if we may be in the dynamic superflat case, i.e. Is_Zero is set
      --  to false but the comparison operand can be zero at run time. In this
      --  case, we normally cannot do anything because the canonical formula of
      --  the length is not valid, but there is one exception: when the operand
      --  is itself the length of an array with the same bounds as the array on
      --  the LHS, we can entirely optimize away the comparison.

      Comp : Node_Id;
      --  Comparison operand, set only if Is_Zero is false

      Ent : array (Pos range 1 .. 2) of Entity_Id := (Empty, Empty);
      --  Entities whose length is being compared

      Index : array (Pos range 1 .. 2) of Node_Id := (Empty, Empty);
      --  Integer_Literal nodes for length attribute expressions, or Empty
      --  if there is no such expression present.

      Op : Node_Kind := Nkind (N);
      --  Kind of comparison operator, gets flipped if operands backwards

      function Convert_To_Long_Long_Integer (N : Node_Id) return Node_Id;
      --  Given a discrete expression, returns a Long_Long_Integer typed
      --  expression representing the underlying value of the expression.
      --  This is done with an unchecked conversion to Long_Long_Integer.
      --  We use unchecked conversion to handle the enumeration type case.

      function Is_Entity_Length (N : Node_Id; Num : Pos) return Boolean;
      --  Tests if N is a length attribute applied to a simple entity. If so,
      --  returns True, and sets Ent to the entity, and Index to the integer
      --  literal provided as an attribute expression, or to Empty if none.
      --  Num is the index designating the relevant slot in Ent and Index.
      --  Also returns True if the expression is a generated type conversion
      --  whose expression is of the desired form. This latter case arises
      --  when Apply_Universal_Integer_Attribute_Check installs a conversion
      --  to check for being in range, which is not needed in this context.
      --  Returns False if neither condition holds.

      function Is_Optimizable (N : Node_Id) return Boolean;
      --  Tests N to see if it is an optimizable comparison value (defined as
      --  constant zero or one, or something else where the value is known to
      --  be nonnegative and in the 32-bit range and where the corresponding
      --  Length value is also known to be 32 bits). If result is true, sets
      --  Is_Zero, Maybe_Superflat and Comp accordingly.

      procedure Rewrite_For_Equal_Lengths;
      --  Rewrite the comparison of two equal lengths into either True or False

      ----------------------------------
      -- Convert_To_Long_Long_Integer --
      ----------------------------------

      function Convert_To_Long_Long_Integer (N : Node_Id) return Node_Id is
      begin
         return Unchecked_Convert_To (Standard_Long_Long_Integer, N);
      end Convert_To_Long_Long_Integer;

      ----------------------
      -- Is_Entity_Length --
      ----------------------

      function Is_Entity_Length (N : Node_Id; Num : Pos) return Boolean is
      begin
         if Nkind (N) = N_Attribute_Reference
           and then Attribute_Name (N) = Name_Length
           and then Is_Entity_Name (Prefix (N))
         then
            Ent (Num) := Entity (Prefix (N));

            if Present (Expressions (N)) then
               Index (Num) := First (Expressions (N));
            else
               Index (Num) := Empty;
            end if;

            return True;

         elsif Nkind (N) = N_Type_Conversion
           and then not Comes_From_Source (N)
         then
            return Is_Entity_Length (Expression (N), Num);

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
         Dbl  : Boolean;
         Ityp : Entity_Id;

      begin
         if Compile_Time_Known_Value (N) then
            Val := Expr_Value (N);

            if Val = Uint_0 then
               Is_Zero         := True;
               Maybe_Superflat := False;
               Comp            := Empty;
               return True;

            elsif Val = Uint_1 then
               Is_Zero         := False;
               Maybe_Superflat := False;
               Comp            := Empty;
               return True;
            end if;
         end if;

         --  Here we have to make sure of being within a 32-bit range (take the
         --  full unsigned range so the length of 32-bit arrays is accepted).

         Determine_Range (N, OK, Lo, Hi, Assume_Valid => True);

         if not OK
           or else Lo < Uint_0
           or else Hi > Uint_2 ** 32
         then
            return False;
         end if;

         Maybe_Superflat := (Lo = Uint_0);

         --  Tests if N is also a length attribute applied to a simple entity

         Dbl := Is_Entity_Length (N, 2);

         --  We can deal with the superflat case only if N is also a length

         if Maybe_Superflat and then not Dbl then
            return False;
         end if;

         --  Comparison value was within range, so now we must check the index
         --  value to make sure it is also within 32 bits.

         for K in Pos range 1 .. 2 loop
            Indx := First_Index (Etype (Ent (K)));

            if Present (Index (K)) then
               for J in 2 .. UI_To_Int (Intval (Index (K))) loop
                  Next_Index (Indx);
               end loop;
            end if;

            Ityp := Etype (Indx);

            if Esize (Ityp) > 32 then
               return False;
            end if;

            exit when not Dbl;
         end loop;

         Is_Zero := False;
         Comp := N;
         return True;
      end Is_Optimizable;

      -------------------------------
      -- Rewrite_For_Equal_Lengths --
      -------------------------------

      procedure Rewrite_For_Equal_Lengths is
      begin
         case Op is
            when N_Op_Eq
               | N_Op_Ge
               | N_Op_Le
            =>
               Rewrite (N,
                 Convert_To (Typ,
                    New_Occurrence_Of (Standard_True, Sloc (N))));

            when N_Op_Ne
               | N_Op_Gt
               | N_Op_Lt
            =>
               Rewrite (N,
                 Convert_To (Typ,
                    New_Occurrence_Of (Standard_False, Sloc (N))));

            when others =>
               raise Program_Error;
         end case;

         Analyze_And_Resolve (N, Typ);
      end Rewrite_For_Equal_Lengths;

   --  Start of processing for Optimize_Length_Comparison

   begin
      --  Nothing to do if not a comparison

      if Op not in N_Op_Compare then
         return;
      end if;

      --  Nothing to do if special -gnatd.P debug flag set.

      if Debug_Flag_Dot_PP then
         return;
      end if;

      --  Ent'Length op 0/1

      if Is_Entity_Length (Left_Opnd (N), 1)
        and then Is_Optimizable (Right_Opnd (N))
      then
         null;

      --  0/1 op Ent'Length

      elsif Is_Entity_Length (Right_Opnd (N), 1)
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
          Prefix         => New_Occurrence_Of (Ent (1), Loc),
          Attribute_Name => Name_First);

      if Present (Index (1)) then
         Set_Expressions (Left, New_List (New_Copy (Index (1))));
      end if;

      --  Build the Last reference we will use

      Right :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Ent (1), Loc),
          Attribute_Name => Name_Last);

      if Present (Index (1)) then
         Set_Expressions (Right, New_List (New_Copy (Index (1))));
      end if;

      --  If general value case, then do the addition of (n - 1), and
      --  also add the needed conversions to type Long_Long_Integer.

      --  If n = Y'Length, we rewrite X'First + (n - 1) op X'Last into:

      --    Y'Last + (X'First - Y'First) op X'Last

      --  in the hope that X'First - Y'First can be computed statically.

      if Present (Comp) then
         if Present (Ent (2)) then
            declare
               Y_First : constant Node_Id :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Ent (2), Loc),
                   Attribute_Name => Name_First);
               Y_Last : constant Node_Id :=
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Ent (2), Loc),
                   Attribute_Name => Name_Last);
               R : Compare_Result;

            begin
               if Present (Index (2)) then
                  Set_Expressions (Y_First, New_List (New_Copy (Index (2))));
                  Set_Expressions (Y_Last,  New_List (New_Copy (Index (2))));
               end if;

               Analyze (Left);
               Analyze (Y_First);

               --  If X'First = Y'First, simplify the above formula into a
               --  direct comparison of Y'Last and X'Last.

               R := Compile_Time_Compare (Left, Y_First, Assume_Valid => True);

               if R = EQ then
                  Analyze (Right);
                  Analyze (Y_Last);

                  R := Compile_Time_Compare
                                         (Right, Y_Last, Assume_Valid => True);

                  --  If the pairs of attributes are equal, we are done

                  if R = EQ then
                     Rewrite_For_Equal_Lengths;
                     return;
                  end if;

                  --  If the base types are different, convert both operands to
                  --  Long_Long_Integer, else compare them directly.

                  if Base_Type (Etype (Right)) /= Base_Type (Etype (Y_Last))
                  then
                     Left := Convert_To_Long_Long_Integer (Y_Last);
                  else
                     Left := Y_Last;
                     Comp := Empty;
                  end if;

               --  Otherwise, use the above formula as-is

               else
                  Left :=
                    Make_Op_Add (Loc,
                      Left_Opnd  =>
                        Convert_To_Long_Long_Integer (Y_Last),
                      Right_Opnd =>
                        Make_Op_Subtract (Loc,
                          Left_Opnd  =>
                            Convert_To_Long_Long_Integer (Left),
                          Right_Opnd =>
                            Convert_To_Long_Long_Integer (Y_First)));
               end if;
            end;

         --  General value case

         else
            Left :=
              Make_Op_Add (Loc,
                Left_Opnd  => Convert_To_Long_Long_Integer (Left),
                Right_Opnd =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd  => Convert_To_Long_Long_Integer (Comp),
                    Right_Opnd => Make_Integer_Literal (Loc, 1)));
         end if;
      end if;

      --  We cannot do anything in the superflat case past this point

      if Maybe_Superflat then
         return;
      end if;

      --  If general operand, convert Last reference to Long_Long_Integer

      if Present (Comp) then
         Right := Convert_To_Long_Long_Integer (Right);
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

      --  Rewrite and finish up (we can suppress overflow checks, see above)

      Rewrite (N, Result);
      Analyze_And_Resolve (N, Typ, Suppress => Overflow_Check);
   end Optimize_Length_Comparison;

   --------------------------------------
   -- Process_Transients_In_Expression --
   --------------------------------------

   procedure Process_Transients_In_Expression
     (Expr : Node_Id;
      Stmts : List_Id)
   is
      procedure Process_Transient_In_Expression (Obj_Decl : Node_Id);
      --  Process the object whose declaration Obj_Decl is present in Stmts

      -------------------------------------
      -- Process_Transient_In_Expression --
      -------------------------------------

      procedure Process_Transient_In_Expression (Obj_Decl : Node_Id) is
         Loc    : constant Source_Ptr := Sloc (Obj_Decl);
         Obj_Id : constant Entity_Id  := Defining_Identifier (Obj_Decl);

         Hook_Context : constant Node_Id := Find_Hook_Context (Expr);
         --  The node after which to insert deferred finalization actions. This
         --  is usually the innermost enclosing non-transient construct.

         Fin_Context : Node_Id;
         --  The node after which to insert the finalization actions

         Master_Node_Decl : Node_Id;
         Master_Node_Id   : Entity_Id;
         --  Declaration and entity of the Master_Node respectively

      begin
         --  When the context is a Boolean evaluation, all three nodes capture
         --  the result of their computation in a local temporary:

         --    do
         --       Trans_Id : Ctrl_Typ := ...;
         --       Result : constant Boolean := ... Trans_Id ...;
         --       <finalize Trans_Id>
         --    in Result end;

         --  As a result, the finalization of any transient objects can take
         --  place just after the result is captured, except for the case of
         --  conditional expressions in a simple return statement because the
         --  return statement will be distributed into dependent expressions
         --  (see the special handling of simple return statements below).

         --  ??? could this be extended to elementary types?

         if Is_Boolean_Type (Etype (Expr))
           and then
             (Nkind (Expr) = N_Expression_With_Actions
               or else Nkind (Parent (Expr)) /= N_Simple_Return_Statement)
         then
            Fin_Context := Last (Stmts);

         --  Otherwise the immediate context may not be safe enough to carry
         --  out transient object finalization due to aliasing and nesting of
         --  constructs. Insert calls to [Deep_]Finalize after the innermost
         --  enclosing non-transient construct.

         else
            Fin_Context := Hook_Context;
         end if;

         --  Create the declaration of the Master_Node for the object and
         --  insert it before the context. It will later be picked up by
         --  the general finalization mechanism (see Build_Finalizer).

         Master_Node_Id := Make_Temporary (Loc, 'N');
         Master_Node_Decl :=
           Make_Master_Node_Declaration (Loc, Master_Node_Id, Obj_Id);
         Insert_Action (Hook_Context, Master_Node_Decl);

         --  Generate the attachment of the object to the Master_Node

         Attach_Object_To_Master_Node (Obj_Decl, Master_Node_Id);

         --  When the node is part of a return statement, there is no need
         --  to insert a finalization call, as the general finalization
         --  mechanism (see Build_Finalizer) would take care of the master
         --  on subprogram exit. Note that it would also be impossible to
         --  insert the finalization call after the return statement as
         --  this will render it unreachable.

         if Nkind (Fin_Context) = N_Simple_Return_Statement
           or else Nkind (Parent (Expr)) = N_Simple_Return_Statement
         then
            null;

         --  Finalize the object after the context has been evaluated

         --  Note that the node returned by Find_Hook_Context above may be an
         --  operator, which is not a list member. We must locate the proper
         --  node in the tree after which to insert the finalization call.

         else
            while not Is_List_Member (Fin_Context) loop
               Fin_Context := Parent (Fin_Context);
            end loop;

            pragma Assert (Present (Fin_Context));

            Insert_Action_After (Fin_Context,
              Make_Finalize_Call_For_Node (Loc, Master_Node_Id));
         end if;

         --  Mark the transient object to avoid double finalization

         Set_Is_Finalized_Transient (Obj_Id);
      end Process_Transient_In_Expression;

      --  Local variables

      Decl : Node_Id;

   --  Start of processing for Process_Transients_In_Expression

   begin
      pragma Assert (Nkind (Expr) in N_Case_Expression
                                   | N_Expression_With_Actions
                                   | N_If_Expression);

      Decl := First (Stmts);
      while Present (Decl) loop
         if Nkind (Decl) = N_Object_Declaration
           and then Is_Finalizable_Transient (Decl, Expr)
         then
            Process_Transient_In_Expression (Decl);
         end if;

         Next (Decl);
      end loop;
   end Process_Transients_In_Expression;

   ------------------------
   -- Rewrite_Comparison --
   ------------------------

   procedure Rewrite_Comparison (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

      False_Result : Boolean;
      True_Result  : Boolean;

   begin
      if Nkind (N) = N_Type_Conversion then
         Rewrite_Comparison (Expression (N));
         return;

      elsif Nkind (N) not in N_Op_Compare then
         return;
      end if;

      --  If both operands are static, then the comparison has been already
      --  folded in evaluation.

      pragma Assert
        (not Is_Static_Expression (Left_Opnd (N))
           or else
         not Is_Static_Expression (Right_Opnd (N)));

      --  Determine the potential outcome of the comparison assuming that the
      --  operands are valid and emit a warning when the comparison evaluates
      --  to True or False only in the presence of invalid values.

      Warn_On_Constant_Valid_Condition (N);

      --  Determine the potential outcome of the comparison assuming that the
      --  operands are not valid.

      Test_Comparison
        (Op           => N,
         Assume_Valid => False,
         True_Result  => True_Result,
         False_Result => False_Result);

      --  The outcome is a decisive False or True, rewrite the operator into a
      --  non-static literal.

      if False_Result or True_Result then
         Rewrite (N,
           Convert_To (Typ,
             New_Occurrence_Of (Boolean_Literals (True_Result), Sloc (N))));

         Analyze_And_Resolve (N, Typ);
         Set_Is_Static_Expression (N, False);
         Warn_On_Known_Condition (N);
      end if;
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

         elsif Nkind (Op) in N_Indexed_Component | N_Selected_Component then
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

   --  In both cases if Left_Expr is an access type, we first check whether it
   --  is null.

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

      --  Handle entities from the limited view

      Orig_Right_Type : constant Entity_Id := Available_View (Etype (Right));

      Full_R_Typ   : Entity_Id;
      Left_Type    : Entity_Id := Available_View (Etype (Left));
      Right_Type   : Entity_Id := Orig_Right_Type;
      Obj_Tag      : Node_Id;

   begin
      SCIL_Node := Empty;

      --  We have to examine the corresponding record type when dealing with
      --  protected types instead of the original, unexpanded, type.

      if Ekind (Right_Type) = E_Protected_Type then
         Right_Type := Corresponding_Record_Type (Right_Type);
      end if;

      if Ekind (Left_Type) = E_Protected_Type then
         Left_Type := Corresponding_Record_Type (Left_Type);
      end if;

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

         if not Is_Interface (Left_Type)
           and then not Is_Class_Wide_Type (Left_Type)
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
            --  configurable run-time setting.

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
            --  Issue error if CW_Membership operation not available in a
            --  configurable run-time setting.

            if not RTE_Available (RE_CW_Membership) then
               Error_Msg_CRT
                 ("dynamic membership test on tagged types", N);
               Result := Empty;
               return;
            end if;

            Result :=
              Make_Function_Call (Loc,
                 Name => New_Occurrence_Of (RTE (RE_CW_Membership), Loc),
                 Parameter_Associations => New_List (
                   Obj_Tag,
                   New_Occurrence_Of (
                     Node (First_Elmt (Access_Disp_Table (Full_R_Typ))),
                     Loc)));

            --  Generate the SCIL node for this class-wide membership test.

            if Generate_SCIL then
               SCIL_Node := Make_SCIL_Membership_Test (Sloc (N));
               Set_SCIL_Entity (SCIL_Node, Etype (Right_Type));
               Set_SCIL_Tag_Value (SCIL_Node, Obj_Tag);
            end if;
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

      --  if Left is an access object then generate test of the form:
      --    * if Right_Type excludes null: Left /= null and then ...
      --    * if Right_Type includes null: Left = null or else ...

      if Is_Access_Type (Orig_Right_Type) then
         if Can_Never_Be_Null (Orig_Right_Type) then
            Result := Make_And_Then (Loc,
              Left_Opnd  =>
                Make_Op_Ne (Loc,
                  Left_Opnd  => Left,
                  Right_Opnd => Make_Null (Loc)),
              Right_Opnd => Result);

         else
            Result := Make_Or_Else (Loc,
              Left_Opnd  =>
                Make_Op_Eq (Loc,
                  Left_Opnd  => Left,
                  Right_Opnd => Make_Null (Loc)),
              Right_Opnd => Result);
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
