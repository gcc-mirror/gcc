------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Aggr; use Exp_Aggr;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Disp; use Exp_Disp;
with Exp_Fixd; use Exp_Fixd;
with Exp_Pakd; use Exp_Pakd;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Exp_VFpt; use Exp_VFpt;
with Hostparm; use Hostparm;
with Inline;   use Inline;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Validsw;  use Validsw;

package body Exp_Ch4 is

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Binary_Op_Validity_Checks (N : Node_Id);
   pragma Inline (Binary_Op_Validity_Checks);
   --  Performs validity checks for a binary operator

   procedure Expand_Array_Comparison (N : Node_Id);
   --  This routine handles expansion of the comparison operators (N_Op_Lt,
   --  N_Op_Le, N_Op_Gt, N_Op_Ge) when operating on an array type. The basic
   --  code for these operators is similar, differing only in the details of
   --  the actual comparison call that is made.

   function Expand_Array_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      A_Typ  : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id)
      return   Node_Id;
   --  Expand an array equality into a call to a function implementing this
   --  equality, and a call to it. Loc is the location for the generated
   --  nodes. Typ is the type of the array, and Lhs, Rhs are the array
   --  expressions to be compared. A_Typ is the type of the arguments,
   --  which may be a private type, in which case Typ is its full view.
   --  Bodies is a list on which to attach bodies of local functions that
   --  are created in the process. This is the responsability of the
   --  caller to insert those bodies at the right place. Nod provides
   --  the Sloc value for the generated code.

   procedure Expand_Boolean_Operator (N : Node_Id);
   --  Common expansion processing for Boolean operators (And, Or, Xor)
   --  for the case of array type arguments.

   function Expand_Composite_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id)
      return   Node_Id;
   --  Local recursive function used to expand equality for nested
   --  composite types. Used by Expand_Record/Array_Equality, Bodies
   --  is a list on which to attach bodies of local functions that are
   --  created in the process. This is the responsability of the caller
   --  to insert those bodies at the right place. Nod provides the Sloc
   --  value for generated code.

   procedure Expand_Concatenate_Other (Cnode : Node_Id; Opnds : List_Id);
   --  This routine handles expansion of concatenation operations, where
   --  N is the N_Op_Concat node being expanded and Operands is the list
   --  of operands (at least two are present). The caller has dealt with
   --  converting any singleton operands into singleton aggregates.

   procedure Expand_Concatenate_String (Cnode : Node_Id; Opnds : List_Id);
   --  Routine to expand concatenation of 2-5 operands (in the list Operands)
   --  and replace node Cnode with the result of the contatenation. If there
   --  are two operands, they can be string or character. If there are more
   --  than two operands, then are always of type string (i.e. the caller has
   --  already converted character operands to strings in this case).

   procedure Fixup_Universal_Fixed_Operation (N : Node_Id);
   --  N is either an N_Op_Divide or N_Op_Multiply node whose result is
   --  universal fixed. We do not have such a type at runtime, so the
   --  purpose of this routine is to find the real type by looking up
   --  the tree. We also determine if the operation must be rounded.

   procedure Insert_Dereference_Action (N : Node_Id);
   --  N is an expression whose type is an access. When the type is derived
   --  from Checked_Pool, expands a call to the primitive 'dereference'.

   function Make_Array_Comparison_Op
     (Typ   : Entity_Id;
      Nod   : Node_Id)
      return  Node_Id;
   --  Comparisons between arrays are expanded in line. This function
   --  produces the body of the implementation of (a > b), where a and b
   --  are one-dimensional arrays of some discrete type. The original
   --  node is then expanded into the appropriate call to this function.
   --  Nod provides the Sloc value for the generated code.

   function Make_Boolean_Array_Op
     (Typ  : Entity_Id;
      N    : Node_Id)
      return Node_Id;
   --  Boolean operations on boolean arrays are expanded in line. This
   --  function produce the body for the node N, which is (a and b),
   --  (a or b), or (a xor b). It is used only the normal case and not
   --  the packed case. The type involved, Typ, is the Boolean array type,
   --  and the logical operations in the body are simple boolean operations.
   --  Note that Typ is always a constrained type (the caller has ensured
   --  this by using Convert_To_Actual_Subtype if necessary).

   procedure Rewrite_Comparison (N : Node_Id);
   --  N is the node for a compile time comparison. If this outcome of this
   --  comparison can be determined at compile time, then the node N can be
   --  rewritten with True or False. If the outcome cannot be determined at
   --  compile time, the call has no effect.

   function Tagged_Membership (N : Node_Id) return Node_Id;
   --  Construct the expression corresponding to the tagged membership test.
   --  Deals with a second operand being (or not) a class-wide type.

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

   -----------------------------
   -- Expand_Array_Comparison --
   -----------------------------

   --  Expansion is only required in the case of array types. The form of
   --  the expansion is:

   --     [body for greater_nn; boolean_expression]

   --  The body is built by Make_Array_Comparison_Op, and the form of the
   --  Boolean expression depends on the operator involved.

   procedure Expand_Array_Comparison (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Op1  : Node_Id             := Left_Opnd (N);
      Op2  : Node_Id             := Right_Opnd (N);
      Typ1 : constant Entity_Id  := Base_Type (Etype (Op1));

      Expr      : Node_Id;
      Func_Body : Node_Id;
      Func_Name : Entity_Id;

   begin
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
          Name => New_Reference_To (Func_Name, Loc),
          Parameter_Associations => New_List (Op1, Op2));

      Insert_Action (N, Func_Body);
      Rewrite (N, Expr);
      Analyze_And_Resolve (N, Standard_Boolean);

   end Expand_Array_Comparison;

   ---------------------------
   -- Expand_Array_Equality --
   ---------------------------

   --  Expand an equality function for multi-dimensional arrays. Here is
   --  an example of such a function for Nb_Dimension = 2

   --  function Enn (A : arr; B : arr) return boolean is
   --     J1 : integer;
   --     J2 : integer;
   --
   --  begin
   --     if A'length (1) /= B'length (1) then
   --        return false;
   --     else
   --        J1 := B'first (1);
   --        for I1 in A'first (1) .. A'last (1) loop
   --           if A'length (2) /= B'length (2) then
   --              return false;
   --           else
   --              J2 := B'first (2);
   --              for I2 in A'first (2) .. A'last (2) loop
   --                 if A (I1, I2) /=  B (J1, J2) then
   --                    return false;
   --                 end if;
   --                 J2 := Integer'succ (J2);
   --              end loop;
   --           end if;
   --           J1 := Integer'succ (J1);
   --        end loop;
   --     end if;
   --     return true;
   --  end Enn;

   function Expand_Array_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      A_Typ  : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id)
      return   Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (Nod);
      Actuals     : List_Id;
      Decls       : List_Id := New_List;
      Index_List1 : List_Id := New_List;
      Index_List2 : List_Id := New_List;
      Formals     : List_Id;
      Stats       : Node_Id;
      Func_Name   : Entity_Id;
      Func_Body   : Node_Id;

      A : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uA);
      B : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uB);

      function Component_Equality (Typ : Entity_Id) return Node_Id;
      --  Create one statement to compare corresponding components, designated
      --  by a full set of indices.

      function Loop_One_Dimension
        (N     : Int;
         Index : Node_Id)
         return Node_Id;
      --  Loop over the n'th dimension of the arrays. The single statement
      --  in the body of the loop is a loop over the next dimension, or
      --  the comparison of corresponding components.

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
             Prefix => Make_Identifier (Loc, Chars (A)),
             Expressions => Index_List1);

         R :=
           Make_Indexed_Component (Loc,
             Prefix => Make_Identifier (Loc, Chars (B)),
             Expressions => Index_List2);

         Test := Expand_Composite_Equality
                   (Nod, Component_Type (Typ), L, R, Decls);

         return
           Make_Implicit_If_Statement (Nod,
             Condition => Make_Op_Not (Loc, Right_Opnd => Test),
             Then_Statements => New_List (
               Make_Return_Statement (Loc,
                 Expression => New_Occurrence_Of (Standard_False, Loc))));

      end Component_Equality;

      ------------------------
      -- Loop_One_Dimension --
      ------------------------

      function Loop_One_Dimension
        (N     : Int;
         Index : Node_Id)
         return Node_Id
      is
         I : constant Entity_Id := Make_Defining_Identifier (Loc,
                                                  New_Internal_Name ('I'));
         J : constant Entity_Id := Make_Defining_Identifier (Loc,
                                                  New_Internal_Name ('J'));
         Index_Type  : Entity_Id;
         Stats : Node_Id;

      begin
         if N > Number_Dimensions (Typ) then
            return Component_Equality (Typ);

         else
            --  Generate the following:

            --  j: index_type;
            --  ...

            --  if a'length (n) /= b'length (n) then
            --    return false;
            --  else
            --     j := b'first (n);
            --     for i in a'range (n) loop
            --        --  loop over remaining dimensions.
            --        j := index_type'succ (j);
            --     end loop;
            --  end if;

            --  retrieve index type for current dimension.

            Index_Type := Base_Type (Etype (Index));
            Append (New_Reference_To (I, Loc), Index_List1);
            Append (New_Reference_To (J, Loc), Index_List2);

            --  Declare index for j as a local variable to the function.
            --  Index i is a loop variable.

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => J,
                Object_Definition   => New_Reference_To (Index_Type, Loc)));

            Stats :=
              Make_Implicit_If_Statement (Nod,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Prefix => New_Reference_To (A, Loc),
                        Attribute_Name => Name_Length,
                        Expressions => New_List (
                          Make_Integer_Literal (Loc, N))),
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Prefix => New_Reference_To (B, Loc),
                        Attribute_Name => Name_Length,
                        Expressions => New_List (
                          Make_Integer_Literal (Loc, N)))),

                Then_Statements => New_List (
                  Make_Return_Statement (Loc,
                    Expression => New_Occurrence_Of (Standard_False, Loc))),

                Else_Statements => New_List (

                  Make_Assignment_Statement (Loc,
                    Name       => New_Reference_To (J, Loc),
                    Expression =>
                      Make_Attribute_Reference (Loc,
                        Prefix => New_Reference_To (B, Loc),
                        Attribute_Name => Name_First,
                        Expressions => New_List (
                          Make_Integer_Literal (Loc, N)))),

                  Make_Implicit_Loop_Statement (Nod,
                    Identifier => Empty,
                    Iteration_Scheme =>
                      Make_Iteration_Scheme (Loc,
                        Loop_Parameter_Specification =>
                          Make_Loop_Parameter_Specification (Loc,
                            Defining_Identifier => I,
                            Discrete_Subtype_Definition =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Reference_To (A, Loc),
                                Attribute_Name => Name_Range,
                                Expressions => New_List (
                                  Make_Integer_Literal (Loc, N))))),

                    Statements => New_List (
                      Loop_One_Dimension (N + 1, Next_Index (Index)),
                      Make_Assignment_Statement (Loc,
                        Name => New_Reference_To (J, Loc),
                        Expression =>
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Reference_To (Index_Type, Loc),
                            Attribute_Name => Name_Succ,
                            Expressions => New_List (
                              New_Reference_To (J, Loc))))))));

            return Stats;
         end if;
      end Loop_One_Dimension;

   --  Start of processing for Expand_Array_Equality

   begin
      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type      => New_Reference_To (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      Func_Name := Make_Defining_Identifier (Loc,  New_Internal_Name ('E'));

      Stats := Loop_One_Dimension (1, First_Index (Typ));

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean, Loc)),
          Declarations               =>  Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Stats,
                Make_Return_Statement (Loc,
                  Expression => New_Occurrence_Of (Standard_True, Loc)))));

         Set_Has_Completion (Func_Name, True);

         --  If the array type is distinct from the type of the arguments,
         --  it is the full view of a private type. Apply an unchecked
         --  conversion to insure that analysis of the call succeeds.

         if Base_Type (A_Typ) /= Base_Type (Typ) then
            Actuals := New_List (
              OK_Convert_To (Typ, Lhs),
              OK_Convert_To (Typ, Rhs));
         else
            Actuals := New_List (Lhs, Rhs);
         end if;

         Append_To (Bodies, Func_Body);

         return
           Make_Function_Call (Loc,
             Name => New_Reference_To (Func_Name, Loc),
             Parameter_Associations => Actuals);
   end Expand_Array_Equality;

   -----------------------------
   -- Expand_Boolean_Operator --
   -----------------------------

   --  Note that we first get the actual subtypes of the operands,
   --  since we always want to deal with types that have bounds.

   procedure Expand_Boolean_Operator (N : Node_Id) is
      Typ       : constant Entity_Id  := Etype (N);

   begin
      if Is_Bit_Packed_Array (Typ) then
         Expand_Packed_Boolean_Operator (N);

      else

         --  For the normal non-packed case, the expansion is
         --  to build a function for carrying out the comparison
         --  (using Make_Boolean_Array_Op) and then inserting it
         --  into the tree. The original operator node is then
         --  rewritten as a call to this function.

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

            Func_Body := Make_Boolean_Array_Op (Etype (L), N);
            Func_Name := Defining_Unit_Name (Specification (Func_Body));
            Insert_Action (N, Func_Body);

            --  Now rewrite the expression with a call

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (Func_Name, Loc),
                Parameter_Associations =>
                  New_List
                    (L, Make_Type_Conversion
                          (Loc, New_Reference_To (Etype (L), Loc), R))));

            Analyze_And_Resolve (N, Typ);
         end;
      end if;
   end Expand_Boolean_Operator;

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
      Bodies : List_Id)
      return   Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (Nod);
      Full_Type : Entity_Id;
      Prim      : Elmt_Id;
      Eq_Op     : Entity_Id;

   begin
      if Is_Private_Type (Typ) then
         Full_Type := Underlying_Type (Typ);
      else
         Full_Type := Typ;
      end if;

      --  Defense against malformed private types with no completion
      --  the error will be diagnosed later by check_completion

      if No (Full_Type) then
         return New_Reference_To (Standard_False, Loc);
      end if;

      Full_Type := Base_Type (Full_Type);

      if Is_Array_Type (Full_Type) then

         --  If the operand is an elementary type other than a floating-point
         --  type, then we can simply use the built-in block bitwise equality,
         --  since the predefined equality operators always apply and bitwise
         --  equality is fine for all these cases.

         if Is_Elementary_Type (Component_Type (Full_Type))
           and then not Is_Floating_Point_Type (Component_Type (Full_Type))
         then
            return Make_Op_Eq (Loc, Left_Opnd  => Lhs, Right_Opnd => Rhs);

         --  For composite component types, and floating-point types, use
         --  the expansion. This deals with tagged component types (where
         --  we use the applicable equality routine) and floating-point,
         --  (where we need to worry about negative zeroes), and also the
         --  case of any composite type recursively containing such fields.

         else
            return Expand_Array_Equality
                     (Nod, Full_Type, Typ, Lhs, Rhs, Bodies);
         end if;

      elsif Is_Tagged_Type (Full_Type) then

         --  Call the primitive operation "=" of this type

         if Is_Class_Wide_Type (Full_Type) then
            Full_Type := Root_Type (Full_Type);
         end if;

         --  If this is derived from an untagged private type completed
         --  with a tagged type, it does not have a full view, so we
         --  use the primitive operations of the private type.
         --  This check should no longer be necessary when these
         --  types receive their full views ???

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
                       Etype (Next_Formal (First_Formal (Eq_Op)));
            Next_Elmt (Prim);
            pragma Assert (Present (Prim));
         end loop;

         Eq_Op := Node (Prim);

         return
           Make_Function_Call (Loc,
             Name => New_Reference_To (Eq_Op, Loc),
             Parameter_Associations =>
               New_List
                 (Unchecked_Convert_To (Etype (First_Formal (Eq_Op)), Lhs),
                  Unchecked_Convert_To (Etype (First_Formal (Eq_Op)), Rhs)));

      elsif Is_Record_Type (Full_Type) then
         Eq_Op := TSS (Full_Type, Name_uEquality);

         if Present (Eq_Op) then
            if Etype (First_Formal (Eq_Op)) /= Full_Type then

               --  Inherited equality from parent type. Convert the actuals
               --  to match signature of operation.

               declare
                  T : Entity_Id := Etype (First_Formal (Eq_Op));

               begin
                  return
                    Make_Function_Call (Loc,
                      Name => New_Reference_To (Eq_Op, Loc),
                      Parameter_Associations =>
                        New_List (OK_Convert_To (T, Lhs),
                                  OK_Convert_To (T, Rhs)));
               end;

            else
               return
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (Eq_Op, Loc),
                   Parameter_Associations => New_List (Lhs, Rhs));
            end if;

         else
            return Expand_Record_Equality (Nod, Full_Type, Lhs, Rhs, Bodies);
         end if;

      else
         --  It can be a simple record or the full view of a scalar private

         return Make_Op_Eq (Loc, Left_Opnd => Lhs, Right_Opnd => Rhs);
      end if;
   end Expand_Composite_Equality;

   ------------------------------
   -- Expand_Concatenate_Other --
   ------------------------------

   --  Let n be the number of array operands to be concatenated, Base_Typ
   --  their base type, Ind_Typ their index type, and Arr_Typ the original
   --  array type to which the concatenantion operator applies, then the
   --  following subprogram is constructed:
   --
   --  [function Cnn (S1 : Base_Typ; ...; Sn : Base_Typ) return Base_Typ is
   --      L : Ind_Typ;
   --   begin
   --      if S1'Length /= 0 then
   --         L := XXX;   -->  XXX = S1'First       if Arr_Typ is unconstrained
   --                          XXX = Arr_Typ'First  otherwise
   --      elsif S2'Length /= 0 then
   --         L := YYY;   -->  YYY = S2'First       if Arr_Typ is unconstrained
   --                          YYY = Arr_Typ'First  otherwise
   --      ...
   --      elsif Sn-1'Length /= 0 then
   --         L := ZZZ;   -->  ZZZ = Sn-1'First     if Arr_Typ is unconstrained
   --                          ZZZ = Arr_Typ'First  otherwise
   --      else
   --         return Sn;
   --      end if;
   --
   --      declare
   --         P : Ind_Typ;
   --         H : Ind_Typ :=
   --          Ind_Typ'Val ((((S1'Length - 1) + S2'Length) + ... + Sn'Length)
   --                       + Ind_Typ'Pos (L));
   --         R : Base_Typ (L .. H);
   --      begin
   --         if S1'Length /= 0 then
   --            P := S1'First;
   --            loop
   --               R (L) := S1 (P);
   --               L := Ind_Typ'Succ (L);
   --               exit when P = S1'Last;
   --               P := Ind_Typ'Succ (P);
   --            end loop;
   --         end if;
   --
   --         if S2'Length /= 0 then
   --            L := Ind_Typ'Succ (L);
   --            loop
   --               R (L) := S2 (P);
   --               L := Ind_Typ'Succ (L);
   --               exit when P = S2'Last;
   --               P := Ind_Typ'Succ (P);
   --            end loop;
   --         end if;
   --
   --         ...
   --
   --         if Sn'Length /= 0 then
   --            P := Sn'First;
   --            loop
   --               R (L) := Sn (P);
   --               L := Ind_Typ'Succ (L);
   --               exit when P = Sn'Last;
   --               P := Ind_Typ'Succ (P);
   --            end loop;
   --         end if;
   --
   --         return R;
   --      end;
   --   end Cnn;]

   procedure Expand_Concatenate_Other (Cnode : Node_Id; Opnds : List_Id) is
      Loc      : constant Source_Ptr := Sloc (Cnode);
      Nb_Opnds : constant Nat        := List_Length (Opnds);

      Arr_Typ  : constant Entity_Id := Etype (Entity (Cnode));
      Base_Typ : constant Entity_Id := Base_Type (Etype (Cnode));
      Ind_Typ  : constant Entity_Id := Etype (First_Index (Base_Typ));

      Func_Id     : Node_Id;
      Func_Spec   : Node_Id;
      Param_Specs : List_Id;

      Func_Body  : Node_Id;
      Func_Decls : List_Id;
      Func_Stmts : List_Id;

      L_Decl     : Node_Id;

      If_Stmt    : Node_Id;
      Elsif_List : List_Id;

      Declare_Block : Node_Id;
      Declare_Decls : List_Id;
      Declare_Stmts : List_Id;

      H_Decl   : Node_Id;
      H_Init   : Node_Id;
      P_Decl   : Node_Id;
      R_Decl   : Node_Id;
      R_Constr : Node_Id;
      R_Range  : Node_Id;

      Params  : List_Id;
      Operand : Node_Id;

      function Copy_Into_R_S (I : Nat) return List_Id;
      --  Builds the sequence of statement:
      --    P := Si'First;
      --    loop
      --       R (L) := Si (P);
      --       L := Ind_Typ'Succ (L);
      --       exit when P = Si'Last;
      --       P := Ind_Typ'Succ (P);
      --    end loop;
      --
      --  where i is the input parameter I given.

      function Init_L (I : Nat) return Node_Id;
      --  Builds the statement:
      --    L := Arr_Typ'First;  If Arr_Typ is constrained
      --    L := Si'First;       otherwise (where I is the input param given)

      function H return Node_Id;
      --  Builds reference to identifier H.

      function Ind_Val (E : Node_Id) return Node_Id;
      --  Builds expression Ind_Typ'Val (E);

      function L return Node_Id;
      --  Builds reference to identifier L.

      function L_Pos return Node_Id;
      --  Builds expression Ind_Typ'Pos (L).

      function L_Succ return Node_Id;
      --  Builds expression Ind_Typ'Succ (L).

      function One return Node_Id;
      --  Builds integer literal one.

      function P return Node_Id;
      --  Builds reference to identifier P.

      function P_Succ return Node_Id;
      --  Builds expression Ind_Typ'Succ (P).

      function R return Node_Id;
      --  Builds reference to identifier R.

      function S (I : Nat) return Node_Id;
      --  Builds reference to identifier Si, where I is the value given.

      function S_First (I : Nat) return Node_Id;
      --  Builds expression Si'First, where I is the value given.

      function S_Last (I : Nat) return Node_Id;
      --  Builds expression Si'Last, where I is the value given.

      function S_Length (I : Nat) return Node_Id;
      --  Builds expression Si'Length, where I is the value given.

      function S_Length_Test (I : Nat) return Node_Id;
      --  Builds expression Si'Length /= 0, where I is the value given.

      -------------------
      -- Copy_Into_R_S --
      -------------------

      function Copy_Into_R_S (I : Nat) return List_Id is
         Stmts     : List_Id := New_List;
         P_Start   : Node_Id;
         Loop_Stmt : Node_Id;
         R_Copy    : Node_Id;
         Exit_Stmt : Node_Id;
         L_Inc     : Node_Id;
         P_Inc     : Node_Id;

      begin
         --  First construct the initializations

         P_Start := Make_Assignment_Statement (Loc,
                      Name       => P,
                      Expression => S_First (I));
         Append_To (Stmts, P_Start);

         --  Then build the loop

         R_Copy := Make_Assignment_Statement (Loc,
                     Name       => Make_Indexed_Component (Loc,
                                     Prefix      => R,
                                     Expressions => New_List (L)),
                     Expression => Make_Indexed_Component (Loc,
                                     Prefix      => S (I),
                                     Expressions => New_List (P)));

         L_Inc := Make_Assignment_Statement (Loc,
                    Name       => L,
                    Expression => L_Succ);

         Exit_Stmt := Make_Exit_Statement (Loc,
                        Condition => Make_Op_Eq (Loc, P, S_Last (I)));

         P_Inc := Make_Assignment_Statement (Loc,
                    Name       => P,
                    Expression => P_Succ);

         Loop_Stmt :=
           Make_Implicit_Loop_Statement (Cnode,
             Statements => New_List (R_Copy, L_Inc, Exit_Stmt, P_Inc));

         Append_To (Stmts, Loop_Stmt);

         return Stmts;
      end Copy_Into_R_S;

      -------
      -- H --
      -------

      function H return Node_Id is
      begin
         return Make_Identifier (Loc, Name_uH);
      end H;

      -------------
      -- Ind_Val --
      -------------

      function Ind_Val (E : Node_Id) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Ind_Typ, Loc),
             Attribute_Name => Name_Val,
             Expressions    => New_List (E));
      end Ind_Val;

      ------------
      -- Init_L --
      ------------

      function Init_L (I : Nat) return Node_Id is
         E : Node_Id;

      begin
         if Is_Constrained (Arr_Typ) then
            E := Make_Attribute_Reference (Loc,
                   Prefix         => New_Reference_To (Arr_Typ, Loc),
                   Attribute_Name => Name_First);

         else
            E := S_First (I);
         end if;

         return Make_Assignment_Statement (Loc, Name => L, Expression => E);
      end Init_L;

      -------
      -- L --
      -------

      function L return Node_Id is
      begin
         return Make_Identifier (Loc, Name_uL);
      end L;

      -----------
      -- L_Pos --
      -----------

      function L_Pos return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Ind_Typ, Loc),
             Attribute_Name => Name_Pos,
             Expressions    => New_List (L));
      end L_Pos;

      ------------
      -- L_Succ --
      ------------

      function L_Succ return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Ind_Typ, Loc),
             Attribute_Name => Name_Succ,
             Expressions    => New_List (L));
      end L_Succ;

      ---------
      -- One --
      ---------

      function One return Node_Id is
      begin
         return Make_Integer_Literal (Loc, 1);
      end One;

      -------
      -- P --
      -------

      function P return Node_Id is
      begin
         return Make_Identifier (Loc, Name_uP);
      end P;

      ------------
      -- P_Succ --
      ------------

      function P_Succ return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Ind_Typ, Loc),
             Attribute_Name => Name_Succ,
             Expressions    => New_List (P));
      end P_Succ;

      -------
      -- R --
      -------

      function R return Node_Id is
      begin
         return Make_Identifier (Loc, Name_uR);
      end R;

      -------
      -- S --
      -------

      function S (I : Nat) return Node_Id is
      begin
         return Make_Identifier (Loc, New_External_Name ('S', I));
      end S;

      -------------
      -- S_First --
      -------------

      function S_First (I : Nat) return Node_Id is
      begin
         return Make_Attribute_Reference (Loc,
                  Prefix         => S (I),
                  Attribute_Name => Name_First);
      end S_First;

      ------------
      -- S_Last --
      ------------

      function S_Last (I : Nat) return Node_Id is
      begin
         return Make_Attribute_Reference (Loc,
                  Prefix         => S (I),
                  Attribute_Name => Name_Last);
      end S_Last;

      --------------
      -- S_Length --
      --------------

      function S_Length (I : Nat) return Node_Id is
      begin
         return Make_Attribute_Reference (Loc,
                  Prefix         => S (I),
                  Attribute_Name => Name_Length);
      end S_Length;

      -------------------
      -- S_Length_Test --
      -------------------

      function S_Length_Test (I : Nat) return Node_Id is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => S_Length (I),
             Right_Opnd => Make_Integer_Literal (Loc, 0));
      end S_Length_Test;

   --  Start of processing for Expand_Concatenate_Other

   begin
      --  Construct the parameter specs and the overall function spec

      Param_Specs := New_List;
      for I in 1 .. Nb_Opnds loop
         Append_To
           (Param_Specs,
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Make_Defining_Identifier (Loc, New_External_Name ('S', I)),
              Parameter_Type      => New_Reference_To (Base_Typ, Loc)));
      end loop;

      Func_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('C'));
      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name       => Func_Id,
          Parameter_Specifications => Param_Specs,
          Subtype_Mark             => New_Reference_To (Base_Typ, Loc));

      --  Construct L's object declaration

      L_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uL),
          Object_Definition   => New_Reference_To (Ind_Typ, Loc));

      Func_Decls := New_List (L_Decl);

      --  Construct the if-then-elsif statements

      Elsif_List := New_List;
      for I in 2 .. Nb_Opnds - 1 loop
         Append_To (Elsif_List, Make_Elsif_Part (Loc,
                                  Condition       => S_Length_Test (I),
                                  Then_Statements => New_List (Init_L (I))));
      end loop;

      If_Stmt :=
        Make_Implicit_If_Statement (Cnode,
          Condition       => S_Length_Test (1),
          Then_Statements => New_List (Init_L (1)),
          Elsif_Parts     => Elsif_List,
          Else_Statements => New_List (Make_Return_Statement (Loc,
                                         Expression => S (Nb_Opnds))));

      --  Construct the declaration for H

      P_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uP),
          Object_Definition   => New_Reference_To (Ind_Typ, Loc));

      H_Init := Make_Op_Subtract (Loc, S_Length (1), One);
      for I in 2 .. Nb_Opnds loop
         H_Init := Make_Op_Add (Loc, H_Init, S_Length (I));
      end loop;
      H_Init := Ind_Val (Make_Op_Add (Loc, H_Init, L_Pos));

      H_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uH),
          Object_Definition   => New_Reference_To (Ind_Typ, Loc),
          Expression          => H_Init);

      --  Construct the declaration for R

      R_Range := Make_Range (Loc, Low_Bound => L, High_Bound => H);
      R_Constr :=
        Make_Index_Or_Discriminant_Constraint (Loc,
          Constraints => New_List (R_Range));

      R_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_uR),
          Object_Definition   =>
            Make_Subtype_Indication (Loc,
               Subtype_Mark => New_Reference_To (Base_Typ, Loc),
               Constraint   => R_Constr));

      --  Construct the declarations for the declare block

      Declare_Decls := New_List (P_Decl, H_Decl, R_Decl);

      --  Construct list of statements for the declare block

      Declare_Stmts := New_List;
      for I in 1 .. Nb_Opnds loop
         Append_To (Declare_Stmts,
                    Make_Implicit_If_Statement (Cnode,
                      Condition       => S_Length_Test (I),
                      Then_Statements => Copy_Into_R_S (I)));
      end loop;

      Append_To (Declare_Stmts, Make_Return_Statement (Loc, Expression => R));

      --  Construct the declare block

      Declare_Block := Make_Block_Statement (Loc,
        Declarations               => Declare_Decls,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc, Declare_Stmts));

      --  Construct the list of function statements

      Func_Stmts := New_List (If_Stmt, Declare_Block);

      --  Construct the function body

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              => Func_Spec,
          Declarations               => Func_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Func_Stmts));

      --  Insert the newly generated function in the code. This is analyzed
      --  with all checks off, since we have completed all the checks.

      --  Note that this does *not* fix the array concatenation bug when the
      --  low bound is Integer'first sibce that bug comes from the pointer
      --  dereferencing an unconstrained array. An there we need a constraint
      --  check to make sure the length of the concatenated array is ok. ???

      Insert_Action (Cnode, Func_Body, Suppress => All_Checks);

      --  Construct list of arguments for the function call

      Params := New_List;
      Operand  := First (Opnds);
      for I in 1 .. Nb_Opnds loop
         Append_To (Params, Relocate_Node (Operand));
         Next (Operand);
      end loop;

      --  Insert the function call

      Rewrite
        (Cnode,
         Make_Function_Call (Loc, New_Reference_To (Func_Id, Loc), Params));

      Analyze_And_Resolve (Cnode, Base_Typ);
      Set_Is_Inlined (Func_Id);
   end Expand_Concatenate_Other;

   -------------------------------
   -- Expand_Concatenate_String --
   -------------------------------

   procedure Expand_Concatenate_String (Cnode : Node_Id; Opnds : List_Id) is
      Loc   : constant Source_Ptr := Sloc (Cnode);
      Opnd1 : constant Node_Id    := First (Opnds);
      Opnd2 : constant Node_Id    := Next (Opnd1);
      Typ1  : constant Entity_Id  := Base_Type (Etype (Opnd1));
      Typ2  : constant Entity_Id  := Base_Type (Etype (Opnd2));

      R : RE_Id;
      --  RE_Id value for function to be called

   begin
      --  In all cases, we build a call to a routine giving the list of
      --  arguments as the parameter list to the routine.

      case List_Length (Opnds) is
         when 2 =>
            if Typ1 = Standard_Character then
               if Typ2 = Standard_Character then
                  R := RE_Str_Concat_CC;

               else
                  pragma Assert (Typ2 = Standard_String);
                  R := RE_Str_Concat_CS;
               end if;

            elsif Typ1 = Standard_String then
               if Typ2 = Standard_Character then
                  R := RE_Str_Concat_SC;

               else
                  pragma Assert (Typ2 = Standard_String);
                  R := RE_Str_Concat;
               end if;

            --  If we have anything other than Standard_Character or
            --  Standard_String, then we must have had a serious error
            --  earlier, so we just abandon the attempt at expansion.

            else
               pragma Assert (Serious_Errors_Detected > 0);
               return;
            end if;

         when 3 =>
            R := RE_Str_Concat_3;

         when 4 =>
            R := RE_Str_Concat_4;

         when 5 =>
            R := RE_Str_Concat_5;

         when others =>
            R := RE_Null;
            raise Program_Error;
      end case;

      --  Now generate the appropriate call

      Rewrite (Cnode,
        Make_Function_Call (Sloc (Cnode),
          Name => New_Occurrence_Of (RTE (R), Loc),
          Parameter_Associations => Opnds));

      Analyze_And_Resolve (Cnode, Standard_String);
   end Expand_Concatenate_String;

   ------------------------
   -- Expand_N_Allocator --
   ------------------------

   procedure Expand_N_Allocator (N : Node_Id) is
      PtrT  : constant Entity_Id  := Etype (N);
      Desig : Entity_Id;
      Loc   : constant Source_Ptr := Sloc (N);
      Temp  : Entity_Id;
      Node  : Node_Id;

   begin
      --  RM E.2.3(22). We enforce that the expected type of an allocator
      --  shall not be a remote access-to-class-wide-limited-private type

      --  Why is this being done at expansion time, seems clearly wrong ???

      Validate_Remote_Access_To_Class_Wide_Type (N);

      --  Set the Storage Pool

      Set_Storage_Pool (N, Associated_Storage_Pool (Root_Type (PtrT)));

      if Present (Storage_Pool (N)) then
         if Is_RTE (Storage_Pool (N), RE_SS_Pool) then
            if not Java_VM then
               Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
            end if;
         else
            Set_Procedure_To_Call (N,
              Find_Prim_Op (Etype (Storage_Pool (N)), Name_Allocate));
         end if;
      end if;

      --  Under certain circumstances we can replace an allocator by an
      --  access to statically allocated storage. The conditions, as noted
      --  in AARM 3.10 (10c) are as follows:

      --    Size and initial value is known at compile time
      --    Access type is access-to-constant

      if Is_Access_Constant (PtrT)
        and then Nkind (Expression (N)) = N_Qualified_Expression
        and then Compile_Time_Known_Value (Expression (Expression (N)))
        and then Size_Known_At_Compile_Time (Etype (Expression
                                                    (Expression (N))))
      then
         --  Here we can do the optimization. For the allocator

         --    new x'(y)

         --  We insert an object declaration

         --    Tnn : aliased x := y;

         --  and replace the allocator by Tnn'Unrestricted_Access.
         --  Tnn is marked as requiring static allocation.

         Temp :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

         Desig := Subtype_Mark (Expression (N));

         --  If context is constrained, use constrained subtype directly,
         --  so that the constant is not labelled as having a nomimally
         --  unconstrained subtype.

         if Entity (Desig) = Base_Type (Designated_Type (PtrT)) then
            Desig := New_Occurrence_Of (Designated_Type (PtrT), Loc);
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
             Prefix => New_Occurrence_Of (Temp, Loc),
             Attribute_Name => Name_Unrestricted_Access));

         Analyze_And_Resolve (N, PtrT);

         --  We set the variable as statically allocated, since we don't
         --  want it going on the stack of the current procedure!

         Set_Is_Statically_Allocated (Temp);
         return;
      end if;

      --  If the allocator is for a type which requires initialization, and
      --  there is no initial value (i.e. the operand is a subtype indication
      --  rather than a qualifed expression), then we must generate a call to
      --  the initialization routine. This is done using an expression actions
      --  node:
      --
      --     [Pnnn : constant ptr_T := new (T); Init (Pnnn.all,...); Pnnn]
      --
      --  Here ptr_T is the pointer type for the allocator, and T is the
      --  subtype of the allocator. A special case arises if the designated
      --  type of the access type is a task or contains tasks. In this case
      --  the call to Init (Temp.all ...) is replaced by code that ensures
      --  that the tasks get activated (see Exp_Ch9.Build_Task_Allocate_Block
      --  for details). In addition, if the type T is a task T, then the first
      --  argument to Init must be converted to the task record type.

      if Nkind (Expression (N)) = N_Qualified_Expression then
         declare
            Indic : constant Node_Id   := Subtype_Mark (Expression (N));
            T     : constant Entity_Id := Entity (Indic);
            Exp   : constant Node_Id   := Expression (Expression (N));

            Aggr_In_Place : constant Boolean := Is_Delayed_Aggregate (Exp);

            Tag_Assign : Node_Id;
            Tmp_Node   : Node_Id;

         begin
            if Is_Tagged_Type (T) or else Controlled_Type (T) then

               --    Actions inserted before:
               --              Temp : constant ptr_T := new T'(Expression);
               --   <no CW>    Temp._tag := T'tag;
               --   <CTRL>     Adjust (Finalizable (Temp.all));
               --   <CTRL>     Attach_To_Final_List (Finalizable (Temp.all));

               --  We analyze by hand the new internal allocator to avoid
               --  any recursion and inappropriate call to Initialize
               if not Aggr_In_Place then
                  Remove_Side_Effects (Exp);
               end if;

               Temp :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

               --  For a class wide allocation generate the following code:

               --    type Equiv_Record is record ... end record;
               --    implicit subtype CW is <Class_Wide_Subytpe>;
               --    temp : PtrT := new CW'(CW!(expr));

               if Is_Class_Wide_Type (T) then
                  Expand_Subtype_From_Expr (Empty, T, Indic, Exp);

                  Set_Expression (Expression (N),
                    Unchecked_Convert_To (Entity (Indic), Exp));

                  Analyze_And_Resolve (Expression (N), Entity (Indic));
               end if;

               if Aggr_In_Place then
                  Tmp_Node :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Temp,
                      Object_Definition   => New_Reference_To (PtrT, Loc),
                      Expression          => Make_Allocator (Loc,
                          New_Reference_To (Etype (Exp), Loc)));

                  Set_No_Initialization (Expression (Tmp_Node));
                  Insert_Action (N, Tmp_Node);
                  Convert_Aggr_In_Allocator (Tmp_Node, Exp);
               else
                  Node := Relocate_Node (N);
                  Set_Analyzed (Node);
                  Insert_Action (N,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Temp,
                      Constant_Present    => True,
                      Object_Definition   => New_Reference_To (PtrT, Loc),
                      Expression          => Node));
               end if;

               --  Suppress the tag assignment when Java_VM because JVM tags
               --  are represented implicitly in objects.

               if Is_Tagged_Type (T)
                 and then not Is_Class_Wide_Type (T)
                 and then not Java_VM
               then
                  Tag_Assign :=
                    Make_Assignment_Statement (Loc,
                      Name =>
                        Make_Selected_Component (Loc,
                          Prefix => New_Reference_To (Temp, Loc),
                          Selector_Name =>
                            New_Reference_To (Tag_Component (T), Loc)),

                      Expression =>
                        Unchecked_Convert_To (RTE (RE_Tag),
                          New_Reference_To (Access_Disp_Table (T), Loc)));

                  --  The previous assignment has to be done in any case

                  Set_Assignment_OK (Name (Tag_Assign));
                  Insert_Action (N, Tag_Assign);

               elsif Is_Private_Type (T)
                 and then Is_Tagged_Type (Underlying_Type (T))
                 and then not Java_VM
               then
                  declare
                     Utyp : constant Entity_Id := Underlying_Type (T);
                     Ref  : constant Node_Id :=
                              Unchecked_Convert_To (Utyp,
                                Make_Explicit_Dereference (Loc,
                                  New_Reference_To (Temp, Loc)));

                  begin
                     Tag_Assign :=
                       Make_Assignment_Statement (Loc,
                         Name =>
                           Make_Selected_Component (Loc,
                             Prefix => Ref,
                             Selector_Name =>
                               New_Reference_To (Tag_Component (Utyp), Loc)),

                         Expression =>
                           Unchecked_Convert_To (RTE (RE_Tag),
                             New_Reference_To (
                               Access_Disp_Table (Utyp), Loc)));

                     Set_Assignment_OK (Name (Tag_Assign));
                     Insert_Action (N, Tag_Assign);
                  end;
               end if;

               if Controlled_Type (Designated_Type (PtrT))
                  and then Controlled_Type (T)
               then
                  declare
                     Flist  : Node_Id;
                     Attach : Node_Id;
                     Apool  : constant Entity_Id :=
                                Associated_Storage_Pool (PtrT);

                  begin
                     --  If it is an allocation on the secondary stack
                     --  (i.e. a value returned from a function), the object
                     --  is attached on the caller side as soon as the call
                     --  is completed (see Expand_Ctrl_Function_Call)

                     if Is_RTE (Apool, RE_SS_Pool) then
                        declare
                           F : constant Entity_Id :=
                                 Make_Defining_Identifier (Loc,
                                   New_Internal_Name ('F'));
                        begin
                           Insert_Action (N,
                             Make_Object_Declaration (Loc,
                               Defining_Identifier => F,
                               Object_Definition   => New_Reference_To (RTE
                                (RE_Finalizable_Ptr), Loc)));

                           Flist := New_Reference_To (F, Loc);
                           Attach :=  Make_Integer_Literal (Loc, 1);
                        end;

                     --  Normal case, not a secondary stack allocation

                     else
                        Flist := Find_Final_List (PtrT);
                        Attach :=  Make_Integer_Literal (Loc, 2);
                     end if;

                     if not Aggr_In_Place then
                        Insert_Actions (N,
                          Make_Adjust_Call (
                            Ref          =>

                           --  An unchecked conversion is needed in the
                           --  classwide case because the designated type
                           --  can be an ancestor of the subtype mark of
                           --  the allocator.

                            Unchecked_Convert_To (T,
                              Make_Explicit_Dereference (Loc,
                                New_Reference_To (Temp, Loc))),

                            Typ          => T,
                            Flist_Ref    => Flist,
                            With_Attach  => Attach));
                     end if;
                  end;
               end if;

               Rewrite (N, New_Reference_To (Temp, Loc));
               Analyze_And_Resolve (N, PtrT);

            elsif Aggr_In_Place then
               Temp :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
               Tmp_Node :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Reference_To (PtrT, Loc),
                   Expression          => Make_Allocator (Loc,
                       New_Reference_To (Etype (Exp), Loc)));

               Set_No_Initialization (Expression (Tmp_Node));
               Insert_Action (N, Tmp_Node);
               Convert_Aggr_In_Allocator (Tmp_Node, Exp);
               Rewrite (N, New_Reference_To (Temp, Loc));
               Analyze_And_Resolve (N, PtrT);

            elsif Is_Access_Type (Designated_Type (PtrT))
              and then Nkind (Exp) = N_Allocator
              and then Nkind (Expression (Exp)) /= N_Qualified_Expression
            then
               --  Apply constraint to designated subtype indication.

               Apply_Constraint_Check (Expression (Exp),
                 Designated_Type (Designated_Type (PtrT)),
                 No_Sliding => True);

               if Nkind (Expression (Exp)) = N_Raise_Constraint_Error then

                  --  Propagate constraint_error to enclosing allocator

                  Rewrite (Exp, New_Copy (Expression (Exp)));
               end if;
            else
               --  First check against the type of the qualified expression
               --
               --  NOTE: The commented call should be correct, but for
               --  some reason causes the compiler to bomb (sigsegv) on
               --  ACVC test c34007g, so for now we just perform the old
               --  (incorrect) test against the designated subtype with
               --  no sliding in the else part of the if statement below.
               --  ???
               --
               --  Apply_Constraint_Check (Exp, T, No_Sliding => True);

               --  A check is also needed in cases where the designated
               --  subtype is constrained and differs from the subtype
               --  given in the qualified expression. Note that the check
               --  on the qualified expression does not allow sliding,
               --  but this check does (a relaxation from Ada 83).

               if Is_Constrained (Designated_Type (PtrT))
                 and then not Subtypes_Statically_Match
                                (T, Designated_Type (PtrT))
               then
                  Apply_Constraint_Check
                    (Exp, Designated_Type (PtrT), No_Sliding => False);

               --  The nonsliding check should really be performed
               --  (unconditionally) against the subtype of the
               --  qualified expression, but that causes a problem
               --  with c34007g (see above), so for now we retain this.

               else
                  Apply_Constraint_Check
                    (Exp, Designated_Type (PtrT), No_Sliding => True);
               end if;
            end if;
         end;

      --  Here if not qualified expression case.
      --  In this case, an initialization routine may be required

      else
         declare
            T         : constant Entity_Id  := Entity (Expression (N));
            Init      : Entity_Id;
            Arg1      : Node_Id;
            Args      : List_Id;
            Decls     : List_Id;
            Decl      : Node_Id;
            Discr     : Elmt_Id;
            Flist     : Node_Id;
            Temp_Decl : Node_Id;
            Temp_Type : Entity_Id;

         begin

            if No_Initialization (N) then
               null;

            --  Case of no initialization procedure present

            elsif not Has_Non_Null_Base_Init_Proc (T) then

               --  Case of simple initialization required

               if Needs_Simple_Initialization (T) then
                  Rewrite (Expression (N),
                    Make_Qualified_Expression (Loc,
                      Subtype_Mark => New_Occurrence_Of (T, Loc),
                      Expression   => Get_Simple_Init_Val (T, Loc)));

                  Analyze_And_Resolve (Expression (Expression (N)), T);
                  Analyze_And_Resolve (Expression (N), T);
                  Set_Paren_Count (Expression (Expression (N)), 1);
                  Expand_N_Allocator (N);

               --  No initialization required

               else
                  null;
               end if;

            --  Case of initialization procedure present, must be called

            else
               Init := Base_Init_Proc (T);
               Node := N;
               Temp :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

               --  Construct argument list for the initialization routine call
               --  The CPP constructor needs the address directly

               if Is_CPP_Class (T) then
                  Arg1 := New_Reference_To (Temp, Loc);
                  Temp_Type := T;

               else
                  Arg1 :=
                    Make_Explicit_Dereference (Loc,
                      Prefix => New_Reference_To (Temp, Loc));
                  Set_Assignment_OK (Arg1);
                  Temp_Type := PtrT;

                  --  The initialization procedure expects a specific type.
                  --  if the context is access to class wide, indicate that
                  --  the object being allocated has the right specific type.

                  if Is_Class_Wide_Type (Designated_Type (PtrT)) then
                     Arg1 := Unchecked_Convert_To (T, Arg1);
                  end if;
               end if;

               --  If designated type is a concurrent type or if it is a
               --  private type whose definition is a concurrent type,
               --  the first argument in the Init routine has to be
               --  unchecked conversion to the corresponding record type.
               --  If the designated type is a derived type, we also
               --  convert the argument to its root type.

               if Is_Concurrent_Type (T) then
                  Arg1 :=
                    Unchecked_Convert_To (Corresponding_Record_Type (T), Arg1);

               elsif Is_Private_Type (T)
                 and then Present (Full_View (T))
                 and then Is_Concurrent_Type (Full_View (T))
               then
                  Arg1 :=
                    Unchecked_Convert_To
                      (Corresponding_Record_Type (Full_View (T)), Arg1);

               elsif Etype (First_Formal (Init)) /= Base_Type (T) then

                  declare
                     Ftyp : constant Entity_Id := Etype (First_Formal (Init));

                  begin
                     Arg1 := OK_Convert_To (Etype (Ftyp), Arg1);
                     Set_Etype (Arg1, Ftyp);
                  end;
               end if;

               Args := New_List (Arg1);

               --  For the task case, pass the Master_Id of the access type
               --  as the value of the _Master parameter, and _Chain as the
               --  value of the _Chain parameter (_Chain will be defined as
               --  part of the generated code for the allocator).

               if Has_Task (T) then

                  if No (Master_Id (Base_Type (PtrT))) then

                     --  The designated type was an incomplete type, and
                     --  the access type did not get expanded. Salvage
                     --  it now.

                     Expand_N_Full_Type_Declaration
                       (Parent (Base_Type (PtrT)));
                  end if;

                  --  If the context of the allocator is a declaration or
                  --  an assignment, we can generate a meaningful image for
                  --  it, even though subsequent assignments might remove
                  --  the connection between task and entity. We build this
                  --  image when the left-hand side is a simple variable,
                  --  a simple indexed assignment or a simple selected
                  --  component.

                  if Nkind (Parent (N)) = N_Assignment_Statement then
                     declare
                        Nam : constant Node_Id := Name (Parent (N));

                     begin
                        if Is_Entity_Name (Nam) then
                           Decls :=
                             Build_Task_Image_Decls (
                               Loc,
                                 New_Occurrence_Of
                                   (Entity (Nam), Sloc (Nam)), T);

                        elsif (Nkind (Nam) = N_Indexed_Component
                                or else Nkind (Nam) = N_Selected_Component)
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
                       Build_Task_Image_Decls (
                          Loc, Defining_Identifier (Parent (N)), T);

                  else
                     Decls := Build_Task_Image_Decls (Loc, T, T);
                  end if;

                  Append_To (Args,
                    New_Reference_To
                      (Master_Id (Base_Type (Root_Type (PtrT))), Loc));
                  Append_To (Args, Make_Identifier (Loc, Name_uChain));

                  Decl := Last (Decls);
                  Append_To (Args,
                    New_Occurrence_Of (Defining_Identifier (Decl), Loc));

               --  Has_Task is false, Decls not used

               else
                  Decls := No_List;
               end if;

               --  Add discriminants if discriminated type

               if Has_Discriminants (T) then
                  Discr := First_Elmt (Discriminant_Constraint (T));

                  while Present (Discr) loop
                     Append (New_Copy (Elists.Node (Discr)), Args);
                     Next_Elmt (Discr);
                  end loop;

               elsif Is_Private_Type (T)
                 and then Present (Full_View (T))
                 and then Has_Discriminants (Full_View (T))
               then
                  Discr :=
                    First_Elmt (Discriminant_Constraint (Full_View (T)));

                  while Present (Discr) loop
                     Append (New_Copy (Elists.Node (Discr)), Args);
                     Next_Elmt (Discr);
                  end loop;
               end if;

               --  We set the allocator as analyzed so that when we analyze the
               --  expression actions node, we do not get an unwanted recursive
               --  expansion of the allocator expression.

               Set_Analyzed (N, True);
               Node := Relocate_Node (N);

               --  Here is the transformation:
               --    input:  new T
               --    output: Temp : constant ptr_T := new T;
               --            Init (Temp.all, ...);
               --    <CTRL>  Attach_To_Final_List (Finalizable (Temp.all));
               --    <CTRL>  Initialize (Finalizable (Temp.all));

               --  Here ptr_T is the pointer type for the allocator, and T
               --  is the subtype of the allocator.

               Temp_Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Reference_To (Temp_Type, Loc),
                   Expression          => Node);

               Set_Assignment_OK (Temp_Decl);

               if Is_CPP_Class (T) then
                  Set_Aliased_Present (Temp_Decl);
               end if;

               Insert_Action (N, Temp_Decl, Suppress => All_Checks);

               --  Case of designated type is task or contains task
               --  Create block to activate created tasks, and insert
               --  declaration for Task_Image variable ahead of call.

               if Has_Task (T) then
                  declare
                     L   : List_Id := New_List;
                     Blk : Node_Id;

                  begin
                     Build_Task_Allocate_Block (L, Node, Args);
                     Blk := Last (L);

                     Insert_List_Before (First (Declarations (Blk)), Decls);
                     Insert_Actions (N, L);
                  end;

               else
                  Insert_Action (N,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (Init, Loc),
                      Parameter_Associations => Args));
               end if;

               if Controlled_Type (T) then

                  --  If the context is an access parameter, we need to create
                  --  a non-anonymous access type in order to have a usable
                  --  final list, because there is otherwise no pool to which
                  --  the allocated object can belong. We create both the type
                  --  and the finalization chain here, because freezing an
                  --  internal type does not create such a chain.

                  if Ekind (PtrT) = E_Anonymous_Access_Type then
                     declare
                        Acc : Entity_Id :=
                                Make_Defining_Identifier (Loc,
                                  New_Internal_Name ('I'));
                     begin
                        Insert_Action (N,
                          Make_Full_Type_Declaration (Loc,
                            Defining_Identifier => Acc,
                            Type_Definition =>
                               Make_Access_To_Object_Definition (Loc,
                                 Subtype_Indication =>
                                   New_Occurrence_Of (T, Loc))));

                        Build_Final_List (N, Acc);
                        Flist := Find_Final_List (Acc);
                     end;

                  else
                     Flist := Find_Final_List (PtrT);
                  end if;

                  Insert_Actions (N,
                    Make_Init_Call (
                      Ref          => New_Copy_Tree (Arg1),
                      Typ          => T,
                      Flist_Ref    => Flist,
                      With_Attach  => Make_Integer_Literal (Loc, 2)));
               end if;

               if Is_CPP_Class (T) then
                  Rewrite (N,
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Temp, Loc),
                      Attribute_Name => Name_Unchecked_Access));
               else
                  Rewrite (N, New_Reference_To (Temp, Loc));
               end if;

               Analyze_And_Resolve (N, PtrT);
            end if;
         end;
      end if;
   end Expand_N_Allocator;

   -----------------------
   -- Expand_N_And_Then --
   -----------------------

   --  Expand into conditional expression if Actions present, and also
   --  deal with optimizing case of arguments being True or False.

   procedure Expand_N_And_Then (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Left    : constant Node_Id    := Left_Opnd (N);
      Right   : constant Node_Id    := Right_Opnd (N);
      Actlist : List_Id;

   begin
      --  Deal with non-standard booleans

      if Is_Boolean_Type (Typ) then
         Adjust_Condition (Left);
         Adjust_Condition (Right);
         Set_Etype (N, Standard_Boolean);
      end if;

      --  Check for cases of left argument is True or False

      if Nkind (Left) = N_Identifier then

         --  If left argument is True, change (True and then Right) to Right.
         --  Any actions associated with Right will be executed unconditionally
         --  and can thus be inserted into the tree unconditionally.

         if Entity (Left) = Standard_True then
            if Present (Actions (N)) then
               Insert_Actions (N, Actions (N));
            end if;

            Rewrite (N, Right);
            Adjust_Result_Type (N, Typ);
            return;

         --  If left argument is False, change (False and then Right) to
         --  False. In this case we can forget the actions associated with
         --  Right, since they will never be executed.

         elsif Entity (Left) = Standard_False then
            Kill_Dead_Code (Right);
            Kill_Dead_Code (Actions (N));
            Rewrite (N, New_Occurrence_Of (Standard_False, Loc));
            Adjust_Result_Type (N, Typ);
            return;
         end if;
      end if;

      --  If Actions are present, we expand

      --     left and then right

      --  into

      --     if left then right else false end

      --  with the actions becoming the Then_Actions of the conditional
      --  expression. This conditional expression is then further expanded
      --  (and will eventually disappear)

      if Present (Actions (N)) then
         Actlist := Actions (N);
         Rewrite (N,
            Make_Conditional_Expression (Loc,
              Expressions => New_List (
                Left,
                Right,
                New_Occurrence_Of (Standard_False, Loc))));

         Set_Then_Actions (N, Actlist);
         Analyze_And_Resolve (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
         return;
      end if;

      --  No actions present, check for cases of right argument True/False

      if Nkind (Right) = N_Identifier then

         --  Change (Left and then True) to Left. Note that we know there
         --  are no actions associated with the True operand, since we
         --  just checked for this case above.

         if Entity (Right) = Standard_True then
            Rewrite (N, Left);

         --  Change (Left and then False) to False, making sure to preserve
         --  any side effects associated with the Left operand.

         elsif Entity (Right) = Standard_False then
            Remove_Side_Effects (Left);
            Rewrite
              (N, New_Occurrence_Of (Standard_False, Loc));
         end if;
      end if;

      Adjust_Result_Type (N, Typ);
   end Expand_N_And_Then;

   -------------------------------------
   -- Expand_N_Conditional_Expression --
   -------------------------------------

   --  Expand into expression actions if then/else actions present

   procedure Expand_N_Conditional_Expression (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Cond   : constant Node_Id    := First (Expressions (N));
      Thenx  : constant Node_Id    := Next (Cond);
      Elsex  : constant Node_Id    := Next (Thenx);
      Typ    : constant Entity_Id  := Etype (N);
      Cnn    : Entity_Id;
      New_If : Node_Id;

   begin
      --  If either then or else actions are present, then given:

      --     if cond then then-expr else else-expr end

      --  we insert the following sequence of actions (using Insert_Actions):

      --      Cnn : typ;
      --      if cond then
      --         <<then actions>>
      --         Cnn := then-expr;
      --      else
      --         <<else actions>>
      --         Cnn := else-expr
      --      end if;

      --  and replace the conditional expression by a reference to Cnn.

      if Present (Then_Actions (N)) or else Present (Else_Actions (N)) then
         Cnn := Make_Defining_Identifier (Loc, New_Internal_Name ('C'));

         New_If :=
           Make_Implicit_If_Statement (N,
             Condition => Relocate_Node (Cond),

             Then_Statements => New_List (
               Make_Assignment_Statement (Sloc (Thenx),
                 Name => New_Occurrence_Of (Cnn, Sloc (Thenx)),
                 Expression => Relocate_Node (Thenx))),

             Else_Statements => New_List (
               Make_Assignment_Statement (Sloc (Elsex),
                 Name => New_Occurrence_Of (Cnn, Sloc (Elsex)),
                 Expression => Relocate_Node (Elsex))));

         if Present (Then_Actions (N)) then
            Insert_List_Before
              (First (Then_Statements (New_If)), Then_Actions (N));
         end if;

         if Present (Else_Actions (N)) then
            Insert_List_Before
              (First (Else_Statements (New_If)), Else_Actions (N));
         end if;

         Rewrite (N, New_Occurrence_Of (Cnn, Loc));

         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Cnn,
             Object_Definition   => New_Occurrence_Of (Typ, Loc)));

         Insert_Action (N, New_If);
         Analyze_And_Resolve (N, Typ);
      end if;
   end Expand_N_Conditional_Expression;

   -----------------------------------
   -- Expand_N_Explicit_Dereference --
   -----------------------------------

   procedure Expand_N_Explicit_Dereference (N : Node_Id) is
   begin
      --  The only processing required is an insertion of an explicit
      --  dereference call for the checked storage pool case.

      Insert_Dereference_Action (Prefix (N));
   end Expand_N_Explicit_Dereference;

   -----------------
   -- Expand_N_In --
   -----------------

   procedure Expand_N_In (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Rtyp : constant Entity_Id  := Etype (N);

   begin
      --  No expansion is required if we have an explicit range

      if Nkind (Right_Opnd (N)) = N_Range then
         return;

      --  Here right operand is a subtype mark

      else
         declare
            Typ    : Entity_Id := Etype (Right_Opnd (N));
            Obj    : Node_Id   := Left_Opnd (N);
            Cond   : Node_Id := Empty;
            Is_Acc : Boolean := Is_Access_Type (Typ);

         begin
            Remove_Side_Effects (Obj);

            --  For tagged type, do tagged membership operation

            if Is_Tagged_Type (Typ) then
               --  No expansion will be performed when Java_VM, as the
               --  JVM back end will handle the membership tests directly
               --  (tags are not explicitly represented in Java objects,
               --  so the normal tagged membership expansion is not what
               --  we want).

               if not Java_VM then
                  Rewrite (N, Tagged_Membership (N));
                  Analyze_And_Resolve (N, Rtyp);
               end if;

               return;

            --  If type is scalar type, rewrite as x in t'first .. t'last
            --  This reason we do this is that the bounds may have the wrong
            --  type if they come from the original type definition.

            elsif Is_Scalar_Type (Typ) then
               Rewrite (Right_Opnd (N),
                 Make_Range (Loc,
                   Low_Bound =>
                     Make_Attribute_Reference (Loc,
                       Attribute_Name => Name_First,
                       Prefix => New_Reference_To (Typ, Loc)),

                   High_Bound =>
                     Make_Attribute_Reference (Loc,
                       Attribute_Name => Name_Last,
                       Prefix => New_Reference_To (Typ, Loc))));
               Analyze_And_Resolve (N, Rtyp);
               return;
            end if;

            if Is_Acc then
               Typ := Designated_Type (Typ);
            end if;

            if not Is_Constrained (Typ) then
               Rewrite (N,
                 New_Reference_To (Standard_True, Loc));
               Analyze_And_Resolve (N, Rtyp);

            --  For the constrained array case, we have to check the
            --  subscripts for an exact match if the lengths are
            --  non-zero (the lengths must match in any case).

            elsif Is_Array_Type (Typ) then

               declare
                  function Construct_Attribute_Reference
                    (E    : Node_Id;
                     Nam  : Name_Id;
                     Dim  : Nat)
                     return Node_Id;
                  --  Build attribute reference E'Nam(Dim)

                  function Construct_Attribute_Reference
                    (E    : Node_Id;
                     Nam  : Name_Id;
                     Dim  : Nat)
                     return Node_Id
                  is
                  begin
                     return
                       Make_Attribute_Reference (Loc,
                         Prefix => E,
                         Attribute_Name => Nam,
                         Expressions => New_List (
                           Make_Integer_Literal (Loc, Dim)));
                  end Construct_Attribute_Reference;

               begin
                  for J in 1 .. Number_Dimensions (Typ) loop
                     Evolve_And_Then (Cond,
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Construct_Attribute_Reference
                             (Duplicate_Subexpr (Obj), Name_First, J),
                         Right_Opnd =>
                           Construct_Attribute_Reference
                             (New_Occurrence_Of (Typ, Loc), Name_First, J)));

                     Evolve_And_Then (Cond,
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Construct_Attribute_Reference
                             (Duplicate_Subexpr (Obj), Name_Last, J),
                         Right_Opnd =>
                           Construct_Attribute_Reference
                             (New_Occurrence_Of (Typ, Loc), Name_Last, J)));
                  end loop;

                  if Is_Acc then
                     Cond := Make_Or_Else (Loc,
                       Left_Opnd =>
                         Make_Op_Eq (Loc,
                           Left_Opnd  => Obj,
                           Right_Opnd => Make_Null (Loc)),
                       Right_Opnd => Cond);
                  end if;

                  Rewrite (N, Cond);
                  Analyze_And_Resolve (N, Rtyp);
               end;

            --  These are the cases where constraint checks may be
            --  required, e.g. records with possible discriminants

            else
               --  Expand the test into a series of discriminant comparisons.
               --  The expression that is built is the negation of the one
               --  that is used for checking discriminant constraints.

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
               Analyze_And_Resolve (N, Rtyp);
            end if;
         end;
      end if;
   end Expand_N_In;

   --------------------------------
   -- Expand_N_Indexed_Component --
   --------------------------------

   procedure Expand_N_Indexed_Component (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      P   : constant Node_Id    := Prefix (N);
      T   : constant Entity_Id  := Etype (P);

   begin
      --  A special optimization, if we have an indexed component that
      --  is selecting from a slice, then we can eliminate the slice,
      --  since, for example, x (i .. j)(k) is identical to x(k). The
      --  only difference is the range check required by the slice. The
      --  range check for the slice itself has already been generated.
      --  The range check for the subscripting operation is ensured
      --  by converting the subject to the subtype of the slice.

      --  This optimization not only generates better code, avoiding
      --  slice messing especially in the packed case, but more importantly
      --  bypasses some problems in handling this peculiar case, for
      --  example, the issue of dealing specially with object renamings.

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

      --  If the prefix is an access type, then we unconditionally rewrite
      --  if as an explicit deference. This simplifies processing for several
      --  cases, including packed array cases and certain cases in which
      --  checks must be generated. We used to try to do this only when it
      --  was necessary, but it cleans up the code to do it all the time.

      if Is_Access_Type (T) then
         Rewrite (P,
           Make_Explicit_Dereference (Sloc (N),
             Prefix => Relocate_Node (P)));
         Analyze_And_Resolve (P, Designated_Type (T));
      end if;

      if Validity_Checks_On and then Validity_Check_Subscripts then
         Apply_Subscript_Validity_Checks (N);
      end if;

      --  All done for the non-packed case

      if not Is_Packed (Etype (Prefix (N))) then
         return;
      end if;

      --  For packed arrays that are not bit-packed (i.e. the case of an array
      --  with one or more index types with a non-coniguous enumeration type),
      --  we can always use the normal packed element get circuit.

      if not Is_Bit_Packed_Array (Etype (Prefix (N))) then
         Expand_Packed_Element_Reference (N);
         return;
      end if;

      --  For a reference to a component of a bit packed array, we have to
      --  convert it to a reference to the corresponding Packed_Array_Type.
      --  We only want to do this for simple references, and not for:

      --    Left side of assignment (or prefix of left side of assignment)
      --      This case is handled in Exp_Ch5.Expand_N_Assignment_Statement

      --    Renaming objects in renaming associations
      --      This case is handled when a use of the renamed variable occurs

      --    Actual parameters for a procedure call
      --      This case is handled in Exp_Ch6.Expand_Actuals

      --    The second expression in a 'Read attribute reference

      --    The prefix of an address or size attribute reference

      --  The following circuit detects these exceptions

      declare
         Child : Node_Id := N;
         Parnt : Node_Id := Parent (N);

      begin
         loop
            if Nkind (Parnt) = N_Unchecked_Expression then
               null;

            elsif Nkind (Parnt) = N_Object_Renaming_Declaration
              or else Nkind (Parnt) = N_Procedure_Call_Statement
              or else (Nkind (Parnt) = N_Parameter_Association
                        and then
                          Nkind (Parent (Parnt)) =  N_Procedure_Call_Statement)
            then
               return;

            elsif Nkind (Parnt) = N_Attribute_Reference
              and then (Attribute_Name (Parnt) = Name_Address
                         or else
                        Attribute_Name (Parnt) = Name_Size)
              and then Prefix (Parnt) = Child
            then
               return;

            elsif Nkind (Parnt) = N_Assignment_Statement
              and then Name (Parnt) = Child
            then
               return;

            elsif Nkind (Parnt) = N_Attribute_Reference
              and then Attribute_Name (Parnt) = Name_Read
              and then Next (First (Expressions (Parnt))) = Child
            then
               return;

            elsif (Nkind (Parnt) = N_Indexed_Component
                    or else Nkind (Parnt) = N_Selected_Component)
               and then Prefix (Parnt) = Child
            then
               null;

            else
               Expand_Packed_Element_Reference (N);
               return;
            end if;

            --  Keep looking up tree for unchecked expression, or if we are
            --  the prefix of a possible assignment left side.

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
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);

   begin
      Rewrite (N,
        Make_Op_Not (Loc,
          Right_Opnd =>
            Make_In (Loc,
              Left_Opnd  => Left_Opnd (N),
              Right_Opnd => Right_Opnd (N))));
      Analyze_And_Resolve (N, Typ);
   end Expand_N_Not_In;

   -------------------
   -- Expand_N_Null --
   -------------------

   --  The only replacement required is for the case of a null of type
   --  that is an access to protected subprogram. We represent such
   --  access values as a record, and so we must replace the occurrence
   --  of null by the equivalent record (with a null address and a null
   --  pointer in it), so that the backend creates the proper value.

   procedure Expand_N_Null (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Agg : Node_Id;

   begin
      if Ekind (Typ) = E_Access_Protected_Subprogram_Type then
         Agg :=
           Make_Aggregate (Loc,
             Expressions => New_List (
               New_Occurrence_Of (RTE (RE_Null_Address), Loc),
               Make_Null (Loc)));

         Rewrite (N, Agg);
         Analyze_And_Resolve (N, Equivalent_Type (Typ));

         --  For subsequent semantic analysis, the node must retain its
         --  type. Gigi in any case replaces this type by the corresponding
         --  record type before processing the node.

         Set_Etype (N, Typ);
      end if;
   end Expand_N_Null;

   ---------------------
   -- Expand_N_Op_Abs --
   ---------------------

   procedure Expand_N_Op_Abs (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Expr : constant Node_Id := Right_Opnd (N);

   begin
      Unary_Op_Validity_Checks (N);

      --  Deal with software overflow checking

      if not Backend_Overflow_Checks_On_Target
         and then Is_Signed_Integer_Type (Etype (N))
         and then Do_Overflow_Check (N)
      then
         --  Software overflow checking expands abs (expr) into

         --    (if expr >= 0 then expr else -expr)

         --  with the usual Duplicate_Subexpr use coding for expr

         Rewrite (N,
           Make_Conditional_Expression (Loc,
             Expressions => New_List (
               Make_Op_Ge (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Expr),
                 Right_Opnd => Make_Integer_Literal (Loc, 0)),

               Duplicate_Subexpr (Expr),

               Make_Op_Minus (Loc,
                 Right_Opnd  => Duplicate_Subexpr (Expr)))));

         Analyze_And_Resolve (N);

      --  Vax floating-point types case

      elsif Vax_Float (Etype (N)) then
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

      --  Arithemtic overflow checks for signed integer/fixed point types

      if Is_Signed_Integer_Type (Typ)
        or else Is_Fixed_Point_Type (Typ)
      then
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
      end if;
   end Expand_N_Op_And;

   ------------------------
   -- Expand_N_Op_Concat --
   ------------------------

   procedure Expand_N_Op_Concat (N : Node_Id) is

      Opnds : List_Id;
      --  List of operands to be concatenated

      Opnd  : Node_Id;
      --  Single operand for concatenation

      Cnode : Node_Id;
      --  Node which is to be replaced by the result of concatenating
      --  the nodes in the list Opnds.

      Atyp : Entity_Id;
      --  Array type of concatenation result type

      Ctyp : Entity_Id;
      --  Component type of concatenation represented by Cnode

   begin
      Binary_Op_Validity_Checks (N);

      --  If we are the left operand of a concatenation higher up the
      --  tree, then do nothing for now, since we want to deal with a
      --  series of concatenations as a unit.

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

      --  Now Opnd is the deepest Opnd, and its parents are the concatenation
      --  nodes above, so now we process bottom up, doing the operations. We
      --  gather a string that is as long as possible up to five operands

      --  The outer loop runs more than once if there are more than five
      --  concatenations of type Standard.String, the most we handle for
      --  this case, or if more than one concatenation type is involved.

      Outer : loop
         Opnds := New_List (Left_Opnd (Cnode), Right_Opnd (Cnode));
         Set_Parent (Opnds, N);

         --  The inner loop gathers concatenation operands

         Inner : while Cnode /= N
                   and then (Base_Type (Etype (Cnode)) /= Standard_String
                               or else
                             List_Length (Opnds) < 5)
                   and then Base_Type (Etype (Cnode)) =
                            Base_Type (Etype (Parent (Cnode)))
         loop
            Cnode := Parent (Cnode);
            Append (Right_Opnd (Cnode), Opnds);
         end loop Inner;

         --  Here we process the collected operands. First we convert
         --  singleton operands to singleton aggregates. This is skipped
         --  however for the case of two operands of type String, since
         --  we have special routines for these cases.

         Atyp := Base_Type (Etype (Cnode));
         Ctyp := Base_Type (Component_Type (Etype (Cnode)));

         if List_Length (Opnds) > 2 or else Atyp /= Standard_String then
            Opnd := First (Opnds);
            loop
               if Base_Type (Etype (Opnd)) = Ctyp then
                  Rewrite (Opnd,
                    Make_Aggregate (Sloc (Cnode),
                      Expressions => New_List (Relocate_Node (Opnd))));
                  Analyze_And_Resolve (Opnd, Atyp);
               end if;

               Next (Opnd);
               exit when No (Opnd);
            end loop;
         end if;

         --  Now call appropriate continuation routine

         if Atyp = Standard_String then
            Expand_Concatenate_String (Cnode, Opnds);
         else
            Expand_Concatenate_Other (Cnode, Opnds);
         end if;

         exit Outer when Cnode = N;
         Cnode := Parent (Cnode);
      end loop Outer;
   end Expand_N_Op_Concat;

   ------------------------
   -- Expand_N_Op_Divide --
   ------------------------

   procedure Expand_N_Op_Divide (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Ltyp : constant Entity_Id  := Etype (Left_Opnd (N));
      Rtyp : constant Entity_Id  := Etype (Right_Opnd (N));
      Typ  : Entity_Id           := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      --  Vax_Float is a special case

      if Vax_Float (Typ) then
         Expand_Vax_Arith (N);
         return;
      end if;

      --  N / 1 = N for integer types

      if Is_Integer_Type (Typ)
        and then Compile_Time_Known_Value (Right_Opnd (N))
        and then Expr_Value (Right_Opnd (N)) = Uint_1
      then
         Rewrite (N, Left_Opnd (N));
         return;
      end if;

      --  Convert x / 2 ** y to Shift_Right (x, y). Note that the fact that
      --  Is_Power_Of_2_For_Shift is set means that we know that our left
      --  operand is an unsigned integer, as required for this to work.

      if Nkind (Right_Opnd (N)) = N_Op_Expon
        and then Is_Power_Of_2_For_Shift (Right_Opnd (N))
      then
         Rewrite (N,
           Make_Op_Shift_Right (Loc,
             Left_Opnd  => Left_Opnd (N),
             Right_Opnd =>
               Convert_To (Standard_Natural, Right_Opnd (Right_Opnd (N)))));
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

         --  No special processing if Treat_Fixed_As_Integer is set,
         --  since from a semantic point of view such operations are
         --  simply integer operations and will be treated that way.

         if not Treat_Fixed_As_Integer (N) then
            if Is_Integer_Type (Rtyp) then
               Expand_Divide_Fixed_By_Integer_Giving_Fixed (N);
            else
               Expand_Divide_Fixed_By_Fixed_Giving_Fixed (N);
            end if;
         end if;

      --  Other cases of division of fixed-point operands. Again we
      --  exclude the case where Treat_Fixed_As_Integer is set.

      elsif (Is_Fixed_Point_Type (Ltyp) or else
             Is_Fixed_Point_Type (Rtyp))
        and then not Treat_Fixed_As_Integer (N)
      then
         if Is_Integer_Type (Typ) then
            Expand_Divide_Fixed_By_Fixed_Giving_Integer (N);
         else
            pragma Assert (Is_Floating_Point_Type (Typ));
            Expand_Divide_Fixed_By_Fixed_Giving_Float (N);
         end if;

      --  Mixed-mode operations can appear in a non-static universal
      --  context, in  which case the integer argument must be converted
      --  explicitly.

      elsif Typ = Universal_Real
        and then Is_Integer_Type (Rtyp)
      then
         Rewrite (Right_Opnd (N),
           Convert_To (Universal_Real, Relocate_Node (Right_Opnd (N))));

         Analyze_And_Resolve (Right_Opnd (N), Universal_Real);

      elsif Typ = Universal_Real
        and then Is_Integer_Type (Ltyp)
      then
         Rewrite (Left_Opnd (N),
           Convert_To (Universal_Real, Relocate_Node (Left_Opnd (N))));

         Analyze_And_Resolve (Left_Opnd (N), Universal_Real);

      --  Non-fixed point cases, do zero divide and overflow checks

      elsif Is_Integer_Type (Typ) then
         Apply_Divide_Check (N);
      end if;
   end Expand_N_Op_Divide;

   --------------------
   -- Expand_N_Op_Eq --
   --------------------

   procedure Expand_N_Op_Eq (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Lhs     : constant Node_Id    := Left_Opnd (N);
      Rhs     : constant Node_Id    := Right_Opnd (N);
      A_Typ   : Entity_Id           := Etype (Lhs);
      Typl    : Entity_Id := A_Typ;
      Op_Name : Entity_Id;
      Prim    : Elmt_Id;
      Bodies  : List_Id := New_List;

      procedure Build_Equality_Call (Eq : Entity_Id);
      --  If a constructed equality exists for the type or for its parent,
      --  build and analyze call, adding conversions if the operation is
      --  inherited.

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

         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (Eq, Loc),
             Parameter_Associations => New_List (L_Exp, R_Exp)));

         Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
      end Build_Equality_Call;

   --  Start of processing for Expand_N_Op_Eq

   begin
      Binary_Op_Validity_Checks (N);

      if Ekind (Typl) = E_Private_Type then
         Typl := Underlying_Type (Typl);

      elsif Ekind (Typl) = E_Private_Subtype then
         Typl := Underlying_Type (Base_Type (Typl));
      end if;

      --  It may happen in error situations that the underlying type is not
      --  set. The error will be detected later, here we just defend the
      --  expander code.

      if No (Typl) then
         return;
      end if;

      Typl := Base_Type (Typl);

      --  Vax float types

      if Vax_Float (Typl) then
         Expand_Vax_Comparison (N);
         return;

      --  Boolean types (requiring handling of non-standard case)

      elsif Is_Boolean_Type (Typl) then
         Adjust_Condition (Left_Opnd (N));
         Adjust_Condition (Right_Opnd (N));
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);

      --  Array types

      elsif Is_Array_Type (Typl) then

         --  Packed case

         if Is_Bit_Packed_Array (Typl) then
            Expand_Packed_Eq (N);

         --  For non-floating-point elementary types, the primitive equality
         --  always applies, and block-bit comparison is fine. Floating-point
         --  is an exception because of negative zeroes.

         --  However, we never use block bit comparison in No_Run_Time mode,
         --  since this may result in a call to a run time routine

         elsif Is_Elementary_Type (Component_Type (Typl))
           and then not Is_Floating_Point_Type (Component_Type (Typl))
           and then not No_Run_Time
         then
            null;

         --  For composite and floating-point cases, expand equality loop
         --  to make sure of using proper comparisons for tagged types,
         --  and correctly handling the floating-point case.

         else
            Rewrite (N,
              Expand_Array_Equality (N, Typl, A_Typ,
                Relocate_Node (Lhs), Relocate_Node (Rhs), Bodies));

            Insert_Actions      (N, Bodies,           Suppress => All_Checks);
            Analyze_And_Resolve (N, Standard_Boolean, Suppress => All_Checks);
         end if;

      --  Record Types

      elsif Is_Record_Type (Typl) then

         --  For tagged types, use the primitive "="

         if Is_Tagged_Type (Typl) then

            --  If this is derived from an untagged private type completed
            --  with a tagged type, it does not have a full view, so we
            --  use the primitive operations of the private type.
            --  This check should no longer be necessary when these
            --  types receive their full views ???

            if Is_Private_Type (A_Typ)
              and then not Is_Tagged_Type (A_Typ)
              and then Is_Derived_Type (A_Typ)
              and then No (Full_View (A_Typ))
            then
               Prim := First_Elmt (Collect_Primitive_Operations (A_Typ));

               while Chars (Node (Prim)) /= Name_Op_Eq loop
                  Next_Elmt (Prim);
                  pragma Assert (Present (Prim));
               end loop;

               Op_Name := Node (Prim);
            else
               Op_Name := Find_Prim_Op (Typl, Name_Op_Eq);
            end if;

            Build_Equality_Call (Op_Name);

         --  If a type support function is present (for complex cases), use it

         elsif Present (TSS (Root_Type (Typl), Name_uEquality)) then
            Build_Equality_Call (TSS (Root_Type (Typl), Name_uEquality));

         --  Otherwise expand the component by component equality. Note that
         --  we never use block-bit coparisons for records, because of the
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

      --  If we still have an equality comparison (i.e. it was not rewritten
      --  in some way), then we can test if result is needed at compile time).

      if Nkind (N) = N_Op_Eq then
         Rewrite_Comparison (N);
      end if;
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
      Xnode  : Node_Id;
      Temp   : Node_Id;
      Rent   : RE_Id;
      Ent    : Entity_Id;

   begin
      Binary_Op_Validity_Checks (N);

      --  If either operand is of a private type, then we have the use of
      --  an intrinsic operator, and we get rid of the privateness, by using
      --  root types of underlying types for the actual operation. Otherwise
      --  the private types will cause trouble if we expand multiplications
      --  or shifts etc. We also do this transformation if the result type
      --  is different from the base type.

      if Is_Private_Type (Etype (Base))
           or else
         Is_Private_Type (Typ)
           or else
         Is_Private_Type (Exptyp)
           or else
         Rtyp /= Root_Type (Bastyp)
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

      --  At this point the exponentiation must be dynamic since the static
      --  case has already been folded after Resolve by Eval_Op_Expon.

      --  Test for case of literal right argument

      if Compile_Time_Known_Value (Exp) then
         Expv := Expr_Value (Exp);

         --  We only fold small non-negative exponents. You might think we
         --  could fold small negative exponents for the real case, but we
         --  can't because we are required to raise Constraint_Error for
         --  the case of 0.0 ** (negative) even if Machine_Overflows = False.
         --  See ACVC test C4A012B.

         if Expv >= 0 and then Expv <= 4 then

            --  X ** 0 = 1 (or 1.0)

            if Expv = 0 then
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
                   Right_Opnd => Duplicate_Subexpr (Base));

            --  X ** 3 = X * X * X

            elsif Expv = 3 then
               Xnode :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd =>
                     Make_Op_Multiply (Loc,
                       Left_Opnd  => Duplicate_Subexpr (Base),
                       Right_Opnd => Duplicate_Subexpr (Base)),
                   Right_Opnd  => Duplicate_Subexpr (Base));

            --  X ** 4  ->
            --    En : constant base'type := base * base;
            --    ...
            --    En * En

            else -- Expv = 4
               Temp :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

               Insert_Actions (N, New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Reference_To (Typ, Loc),
                   Expression =>
                     Make_Op_Multiply (Loc,
                       Left_Opnd  => Duplicate_Subexpr (Base),
                       Right_Opnd => Duplicate_Subexpr (Base)))));

               Xnode :=
                 Make_Op_Multiply (Loc,
                   Left_Opnd  => New_Reference_To (Temp, Loc),
                   Right_Opnd => New_Reference_To (Temp, Loc));
            end if;

            Rewrite (N, Xnode);
            Analyze_And_Resolve (N, Typ);
            return;
         end if;
      end if;

      --  Case of (2 ** expression) appearing as an argument of an integer
      --  multiplication, or as the right argument of a division of a non-
      --  negative integer. In such cases we lave the node untouched, setting
      --  the flag Is_Natural_Power_Of_2_for_Shift set, then the expansion
      --  of the higher level node converts it into a shift.

      if Nkind (Base) = N_Integer_Literal
        and then Intval (Base) = 2
        and then Is_Integer_Type (Root_Type (Exptyp))
        and then Esize (Root_Type (Exptyp)) <= Esize (Standard_Integer)
        and then Is_Unsigned_Type (Exptyp)
        and then not Ovflo
        and then Nkind (Parent (N)) in N_Binary_Op
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
      end if;

      --  Fall through if exponentiation must be done using a runtime routine

      if No_Run_Time then
         Disallow_In_No_Run_Time_Mode (N);
         return;
      end if;

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
                  Name => New_Reference_To (RTE (RE_Exp_Modular), Loc),
                  Parameter_Associations => New_List (
                    Convert_To (Standard_Integer, Base),
                    Make_Integer_Literal (Loc, Modulus (Rtyp)),
                    Exp))));

         --  Binary case, in this case, we call one of two routines, either
         --  the unsigned integer case, or the unsigned long long integer
         --  case, with a final "and" operation to do the required mod.

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
                      Name => New_Reference_To (Ent, Loc),
                      Parameter_Associations => New_List (
                        Convert_To (Etype (First_Formal (Ent)), Base),
                        Exp)),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Modulus (Rtyp) - 1))));

         end if;

         --  Common exit point for modular type case

         Analyze_And_Resolve (N, Typ);
         return;

      --  Signed integer cases

      elsif Rtyp = Base_Type (Standard_Integer) then
         if Ovflo then
            Rent := RE_Exp_Integer;
         else
            Rent := RE_Exn_Integer;
         end if;

      elsif Rtyp = Base_Type (Standard_Short_Integer) then
         if Ovflo then
            Rent := RE_Exp_Short_Integer;
         else
            Rent := RE_Exn_Short_Integer;
         end if;

      elsif Rtyp = Base_Type (Standard_Short_Short_Integer) then
         if Ovflo then
            Rent := RE_Exp_Short_Short_Integer;
         else
            Rent := RE_Exn_Short_Short_Integer;
         end if;

      elsif Rtyp = Base_Type (Standard_Long_Integer) then
         if Ovflo then
            Rent := RE_Exp_Long_Integer;
         else
            Rent := RE_Exn_Long_Integer;
         end if;

      elsif (Rtyp = Base_Type (Standard_Long_Long_Integer)
        or else Rtyp = Universal_Integer)
      then
         if Ovflo then
            Rent := RE_Exp_Long_Long_Integer;
         else
            Rent := RE_Exn_Long_Long_Integer;
         end if;

      --  Floating-point cases

      elsif Rtyp = Standard_Float then
         if Ovflo then
            Rent := RE_Exp_Float;
         else
            Rent := RE_Exn_Float;
         end if;

      elsif Rtyp = Standard_Short_Float then
         if Ovflo then
            Rent := RE_Exp_Short_Float;
         else
            Rent := RE_Exn_Short_Float;
         end if;

      elsif Rtyp = Standard_Long_Float then
         if Ovflo then
            Rent := RE_Exp_Long_Float;
         else
            Rent := RE_Exn_Long_Float;
         end if;

      else
         pragma Assert
           (Rtyp = Standard_Long_Long_Float or else Rtyp = Universal_Real);

         if Ovflo then
            Rent := RE_Exp_Long_Long_Float;
         else
            Rent := RE_Exn_Long_Long_Float;
         end if;
      end if;

      --  Common processing for integer cases and floating-point cases.
      --  If we are in the base type, we can call runtime routine directly

      if Typ = Rtyp
        and then Rtyp /= Universal_Integer
        and then Rtyp /= Universal_Real
      then
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (RTE (Rent), Loc),
             Parameter_Associations => New_List (Base, Exp)));

      --  Otherwise we have to introduce conversions (conversions are also
      --  required in the universal cases, since the runtime routine was
      --  typed using the largest integer or real case.

      else
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (Rent), Loc),
               Parameter_Associations => New_List (
                 Convert_To (Rtyp, Base),
                 Exp))));
      end if;

      Analyze_And_Resolve (N, Typ);
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

      if Vax_Float (Typ1) then
         Expand_Vax_Comparison (N);
         return;

      elsif Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);
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

      if Vax_Float (Typ1) then
         Expand_Vax_Comparison (N);
         return;

      elsif Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);
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

      if Vax_Float (Typ1) then
         Expand_Vax_Comparison (N);
         return;

      elsif Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);
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

      if Vax_Float (Typ1) then
         Expand_Vax_Comparison (N);
         return;

      elsif Is_Array_Type (Typ1) then
         Expand_Array_Comparison (N);
         return;
      end if;

      if Is_Boolean_Type (Typ1) then
         Adjust_Condition (Op1);
         Adjust_Condition (Op2);
         Set_Etype (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
      end if;

      Rewrite_Comparison (N);
   end Expand_N_Op_Lt;

   -----------------------
   -- Expand_N_Op_Minus --
   -----------------------

   procedure Expand_N_Op_Minus (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      Unary_Op_Validity_Checks (N);

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
      T     : constant Entity_Id  := Etype (N);
      Left  : constant Node_Id    := Left_Opnd (N);
      Right : constant Node_Id    := Right_Opnd (N);
      DOC   : constant Boolean    := Do_Overflow_Check (N);
      DDC   : constant Boolean    := Do_Division_Check (N);

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;

   begin
      Binary_Op_Validity_Checks (N);

      Determine_Range (Right, ROK, Rlo, Rhi);
      Determine_Range (Left,  LOK, Llo, Lhi);

      --  Convert mod to rem if operands are known non-negative. We do this
      --  since it is quite likely that this will improve the quality of code,
      --  (the operation now corresponds to the hardware remainder), and it
      --  does not seem likely that it could be harmful.

      if LOK and then Llo >= 0
           and then
         ROK and then Rlo >= 0
      then
         Rewrite (N,
           Make_Op_Rem (Sloc (N),
             Left_Opnd  => Left_Opnd (N),
             Right_Opnd => Right_Opnd (N)));

         --  Instead of reanalyzing the node we do the analysis manually.
         --  This avoids anomalies when the replacement is done in an
         --  instance and is epsilon more efficient.

         Set_Entity            (N, Standard_Entity (S_Op_Rem));
         Set_Etype             (N, T);
         Set_Do_Overflow_Check (N, DOC);
         Set_Do_Division_Check (N, DDC);
         Expand_N_Op_Rem (N);
         Set_Analyzed (N);

      --  Otherwise, normal mod processing

      else
         if Is_Integer_Type (Etype (N)) then
            Apply_Divide_Check (N);
         end if;

         --  Deal with annoying case of largest negative number remainder
         --  minus one. Gigi does not handle this case correctly, because
         --  it generates a divide instruction which may trap in this case.

         --  In fact the check is quite easy, if the right operand is -1,
         --  then the mod value is always 0, and we can just ignore the
         --  left operand completely in this case.

         LLB := Expr_Value (Type_Low_Bound (Base_Type (Etype (Left))));

         if ((not ROK) or else (Rlo <= (-1) and then (-1) <= Rhi))
           and then
            ((not LOK) or else (Llo = LLB))
         then
            Rewrite (N,
              Make_Conditional_Expression (Loc,
                Expressions => New_List (
                  Make_Op_Eq (Loc,
                    Left_Opnd => Duplicate_Subexpr (Right),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc, -1)),
                  Make_Integer_Literal (Loc, Uint_0),
                  Relocate_Node (N))));

            Set_Analyzed (Next (Next (First (Expressions (N)))));
            Analyze_And_Resolve (N, T);
         end if;
      end if;
   end Expand_N_Op_Mod;

   --------------------------
   -- Expand_N_Op_Multiply --
   --------------------------

   procedure Expand_N_Op_Multiply (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Lop  : constant Node_Id    := Left_Opnd (N);
      Rop  : constant Node_Id    := Right_Opnd (N);
      Ltyp : constant Entity_Id  := Etype (Lop);
      Rtyp : constant Entity_Id  := Etype (Rop);
      Typ  : Entity_Id           := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      --  Special optimizations for integer types

      if Is_Integer_Type (Typ) then

         --  N * 0 = 0 * N = 0 for integer types

         if (Compile_Time_Known_Value (Right_Opnd (N))
              and then Expr_Value (Right_Opnd (N)) = Uint_0)
           or else
            (Compile_Time_Known_Value (Left_Opnd (N))
              and then Expr_Value (Left_Opnd (N)) = Uint_0)
         then
            Rewrite (N, Make_Integer_Literal (Loc, Uint_0));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

         --  N * 1 = 1 * N = N for integer types

         if Compile_Time_Known_Value (Right_Opnd (N))
           and then Expr_Value (Right_Opnd (N)) = Uint_1
         then
            Rewrite (N, Left_Opnd (N));
            return;

         elsif Compile_Time_Known_Value (Left_Opnd (N))
           and then Expr_Value (Left_Opnd (N)) = Uint_1
         then
            Rewrite (N, Right_Opnd (N));
            return;
         end if;
      end if;

      --  Deal with VAX float case

      if Vax_Float (Typ) then
         Expand_Vax_Arith (N);
         return;
      end if;

      --  Convert x * 2 ** y to Shift_Left (x, y). Note that the fact that
      --  Is_Power_Of_2_For_Shift is set means that we know that our left
      --  operand is an integer, as required for this to work.

      if Nkind (Rop) = N_Op_Expon
        and then Is_Power_Of_2_For_Shift (Rop)
      then
         if Nkind (Lop) = N_Op_Expon
           and then Is_Power_Of_2_For_Shift (Lop)
         then

            --  convert 2 ** A * 2 ** B into  2 ** (A + B)

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
            Rewrite (N,
              Make_Op_Shift_Left (Loc,
                Left_Opnd  => Lop,
                Right_Opnd =>
                  Convert_To (Standard_Natural, Right_Opnd (Rop))));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

      --  Same processing for the operands the other way round

      elsif Nkind (Lop) = N_Op_Expon
        and then Is_Power_Of_2_For_Shift (Lop)
      then
         Rewrite (N,
           Make_Op_Shift_Left (Loc,
             Left_Opnd  => Rop,
             Right_Opnd =>
               Convert_To (Standard_Natural, Right_Opnd (Lop))));
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

         --  No special processing if Treat_Fixed_As_Integer is set,
         --  since from a semantic point of view such operations are
         --  simply integer operations and will be treated that way.

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

      --  Other cases of multiplication of fixed-point operands. Again
      --  we exclude the cases where Treat_Fixed_As_Integer flag is set.

      elsif (Is_Fixed_Point_Type (Ltyp) or else Is_Fixed_Point_Type (Rtyp))
        and then not Treat_Fixed_As_Integer (N)
      then
         if Is_Integer_Type (Typ) then
            Expand_Multiply_Fixed_By_Fixed_Giving_Integer (N);
         else
            pragma Assert (Is_Floating_Point_Type (Typ));
            Expand_Multiply_Fixed_By_Fixed_Giving_Float (N);
         end if;

      --  Mixed-mode operations can appear in a non-static universal
      --  context, in  which case the integer argument must be converted
      --  explicitly.

      elsif Typ = Universal_Real
        and then Is_Integer_Type (Rtyp)
      then
         Rewrite (Rop, Convert_To (Universal_Real, Relocate_Node (Rop)));

         Analyze_And_Resolve (Rop, Universal_Real);

      elsif Typ = Universal_Real
        and then Is_Integer_Type (Ltyp)
      then
         Rewrite (Lop, Convert_To (Universal_Real, Relocate_Node (Lop)));

         Analyze_And_Resolve (Lop, Universal_Real);

      --  Non-fixed point cases, check software overflow checking required

      elsif Is_Signed_Integer_Type (Etype (N)) then
         Apply_Arithmetic_Overflow_Check (N);
      end if;
   end Expand_N_Op_Multiply;

   --------------------
   -- Expand_N_Op_Ne --
   --------------------

   --  Rewrite node as the negation of an equality operation, and reanalyze.
   --  The equality to be used is defined in the same scope and has the same
   --  signature. It must be set explicitly because in an instance it may not
   --  have the same visibility as in the generic unit.

   procedure Expand_N_Op_Ne (N : Node_Id) is
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

      Rewrite (N, Neg);
      Analyze_And_Resolve (N, Standard_Boolean);
   end Expand_N_Op_Ne;

   ---------------------
   -- Expand_N_Op_Not --
   ---------------------

   --  If the argument is other than a Boolean array type, there is no
   --  special expansion required.

   --  For the packed case, we call the special routine in Exp_Pakd, except
   --  that if the component size is greater than one, we use the standard
   --  routine generating a gruesome loop (it is so peculiar to have packed
   --  arrays with non-standard Boolean representations anyway, so it does
   --  not matter that we do not handle this case efficiently).

   --  For the unpacked case (and for the special packed case where we have
   --  non standard Booleans, as discussed above), we generate and insert
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

      --  Only array types need any other processing

      if not Is_Array_Type (Typ) then
         return;
      end if;

      --  Case of array operand. If bit packed, handle it in Exp_Pakd

      if Is_Bit_Packed_Array (Typ) and then Component_Size (Typ) = 1 then
         Expand_Packed_Not (N);
         return;
      end if;

      --  Case of array operand which is not bit-packed

      Opnd := Relocate_Node (Right_Opnd (N));
      Convert_To_Actual_Subtype (Opnd);
      Arr := Etype (Opnd);
      Ensure_Defined (Arr, N);

      A := Make_Defining_Identifier (Loc, Name_uA);
      B := Make_Defining_Identifier (Loc, Name_uB);
      J := Make_Defining_Identifier (Loc, Name_uJ);

      A_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Reference_To (A, Loc),
          Expressions => New_List (New_Reference_To (J, Loc)));

      B_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Reference_To (B, Loc),
          Expressions => New_List (New_Reference_To (J, Loc)));

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
                      Prefix => Make_Identifier (Loc, Chars (A)),
                      Attribute_Name => Name_Range))),

          Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => B_J,
              Expression => Make_Op_Not (Loc, A_J))));

      Func_Name := Make_Defining_Identifier (Loc, New_Internal_Name ('N'));
      Set_Is_Inlined (Func_Name);

      Insert_Action (N,
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name => Func_Name,
              Parameter_Specifications => New_List (
                Make_Parameter_Specification (Loc,
                  Defining_Identifier => A,
                  Parameter_Type      => New_Reference_To (Typ, Loc))),
              Subtype_Mark => New_Reference_To (Typ, Loc)),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => B,
              Object_Definition   => New_Reference_To (Arr, Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Loop_Statement,
                Make_Return_Statement (Loc,
                  Expression =>
                    Make_Identifier (Loc, Chars (B)))))));

      Rewrite (N,
        Make_Function_Call (Loc,
          Name => New_Reference_To (Func_Name, Loc),
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
      end if;
   end Expand_N_Op_Or;

   ----------------------
   -- Expand_N_Op_Plus --
   ----------------------

   procedure Expand_N_Op_Plus (N : Node_Id) is
   begin
      Unary_Op_Validity_Checks (N);
   end Expand_N_Op_Plus;

   ---------------------
   -- Expand_N_Op_Rem --
   ---------------------

   procedure Expand_N_Op_Rem (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;
      Typ : Entity_Id;

   begin
      Binary_Op_Validity_Checks (N);

      if Is_Integer_Type (Etype (N)) then
         Apply_Divide_Check (N);
      end if;

      --  Deal with annoying case of largest negative number remainder
      --  minus one. Gigi does not handle this case correctly, because
      --  it generates a divide instruction which may trap in this case.

      --  In fact the check is quite easy, if the right operand is -1,
      --  then the remainder is always 0, and we can just ignore the
      --  left operand completely in this case.

      Determine_Range (Right, ROK, Rlo, Rhi);
      Determine_Range (Left, LOK, Llo, Lhi);
      LLB := Expr_Value (Type_Low_Bound (Base_Type (Etype (Left))));
      Typ := Etype (N);

      if ((not ROK) or else (Rlo <= (-1) and then (-1) <= Rhi))
        and then
         ((not LOK) or else (Llo = LLB))
      then
         Rewrite (N,
           Make_Conditional_Expression (Loc,
             Expressions => New_List (
               Make_Op_Eq (Loc,
                 Left_Opnd => Duplicate_Subexpr (Right),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc, -1)),

               Make_Integer_Literal (Loc, Uint_0),

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

   procedure Expand_N_Op_Shift_Left (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);
   end Expand_N_Op_Shift_Left;

   -----------------------------
   -- Expand_N_Op_Shift_Right --
   -----------------------------

   procedure Expand_N_Op_Shift_Right (N : Node_Id) is
   begin
      Binary_Op_Validity_Checks (N);
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

      --  N - 0 = N for integer types

      if Is_Integer_Type (Typ)
        and then Compile_Time_Known_Value (Right_Opnd (N))
        and then Expr_Value (Right_Opnd (N)) = 0
      then
         Rewrite (N, Left_Opnd (N));
         return;
      end if;

      --  Arithemtic overflow checks for signed integer/fixed point types

      if Is_Signed_Integer_Type (Typ)
        or else Is_Fixed_Point_Type (Typ)
      then
         Apply_Arithmetic_Overflow_Check (N);

      --  Vax floating-point types case

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
      end if;
   end Expand_N_Op_Xor;

   ----------------------
   -- Expand_N_Or_Else --
   ----------------------

   --  Expand into conditional expression if Actions present, and also
   --  deal with optimizing case of arguments being True or False.

   procedure Expand_N_Or_Else (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Left    : constant Node_Id    := Left_Opnd (N);
      Right   : constant Node_Id    := Right_Opnd (N);
      Actlist : List_Id;

   begin
      --  Deal with non-standard booleans

      if Is_Boolean_Type (Typ) then
         Adjust_Condition (Left);
         Adjust_Condition (Right);
         Set_Etype (N, Standard_Boolean);

      --  Check for cases of left argument is True or False

      elsif Nkind (Left) = N_Identifier then

         --  If left argument is False, change (False or else Right) to Right.
         --  Any actions associated with Right will be executed unconditionally
         --  and can thus be inserted into the tree unconditionally.

         if Entity (Left) = Standard_False then
            if Present (Actions (N)) then
               Insert_Actions (N, Actions (N));
            end if;

            Rewrite (N, Right);
            Adjust_Result_Type (N, Typ);
            return;

         --  If left argument is True, change (True and then Right) to
         --  True. In this case we can forget the actions associated with
         --  Right, since they will never be executed.

         elsif Entity (Left) = Standard_True then
            Kill_Dead_Code (Right);
            Kill_Dead_Code (Actions (N));
            Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
            Adjust_Result_Type (N, Typ);
            return;
         end if;
      end if;

      --  If Actions are present, we expand

      --     left or else right

      --  into

      --     if left then True else right end

      --  with the actions becoming the Else_Actions of the conditional
      --  expression. This conditional expression is then further expanded
      --  (and will eventually disappear)

      if Present (Actions (N)) then
         Actlist := Actions (N);
         Rewrite (N,
            Make_Conditional_Expression (Loc,
              Expressions => New_List (
                Left,
                New_Occurrence_Of (Standard_True, Loc),
                Right)));

         Set_Else_Actions (N, Actlist);
         Analyze_And_Resolve (N, Standard_Boolean);
         Adjust_Result_Type (N, Typ);
         return;
      end if;

      --  No actions present, check for cases of right argument True/False

      if Nkind (Right) = N_Identifier then

         --  Change (Left or else False) to Left. Note that we know there
         --  are no actions associated with the True operand, since we
         --  just checked for this case above.

         if Entity (Right) = Standard_False then
            Rewrite (N, Left);

         --  Change (Left or else True) to True, making sure to preserve
         --  any side effects associated with the Left operand.

         elsif Entity (Right) = Standard_True then
            Remove_Side_Effects (Left);
            Rewrite
              (N, New_Occurrence_Of (Standard_True, Loc));
         end if;
      end if;

      Adjust_Result_Type (N, Typ);
   end Expand_N_Or_Else;

   -----------------------------------
   -- Expand_N_Qualified_Expression --
   -----------------------------------

   procedure Expand_N_Qualified_Expression (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Target_Type : constant Entity_Id := Entity (Subtype_Mark (N));

   begin
      Apply_Constraint_Check (Operand, Target_Type, No_Sliding => True);
   end Expand_N_Qualified_Expression;

   ---------------------------------
   -- Expand_N_Selected_Component --
   ---------------------------------

   --  If the selector is a discriminant of a concurrent object, rewrite the
   --  prefix to denote the corresponding record type.

   procedure Expand_N_Selected_Component (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Par   : constant Node_Id    := Parent (N);
      P     : constant Node_Id    := Prefix (N);
      Disc  : Entity_Id;
      Ptyp  : Entity_Id := Underlying_Type (Etype (P));
      New_N : Node_Id;

      function In_Left_Hand_Side (Comp : Node_Id) return Boolean;
      --  Gigi needs a temporary for prefixes that depend on a discriminant,
      --  unless the context of an assignment can provide size information.

      function In_Left_Hand_Side (Comp : Node_Id) return Boolean is
      begin
         return
             (Nkind (Parent (Comp)) = N_Assignment_Statement
               and then Comp = Name (Parent (Comp)))
           or else
             (Present (Parent (Comp))
                and then Nkind (Parent (Comp)) in N_Subexpr
                and then In_Left_Hand_Side (Parent (Comp)));
      end In_Left_Hand_Side;

   begin
      if Do_Discriminant_Check (N) then

         --  Present the discrminant checking function to the backend,
         --  so that it can inline the call to the function.

         Add_Inlined_Body
           (Discriminant_Checking_Func
             (Original_Record_Component (Entity (Selector_Name (N)))));
      end if;

      --  Insert explicit dereference call for the checked storage pool case

      if Is_Access_Type (Ptyp) then
         Insert_Dereference_Action (P);
         return;
      end if;

   --  Gigi cannot handle unchecked conversions that are the prefix of
   --  a selected component with discriminants. This must be checked
   --  during expansion, because during analysis the type of the selector
   --  is not known at the point the prefix is analyzed. If the conversion
   --  is the target of an assignment, we cannot force the evaluation, of
   --  course.

      if Nkind (Prefix (N)) = N_Unchecked_Type_Conversion
        and then Has_Discriminants (Etype (N))
        and then not In_Left_Hand_Side (N)
      then
         Force_Evaluation (Prefix (N));
      end if;

      --  Remaining processing applies only if selector is a discriminant

      if Ekind (Entity (Selector_Name (N))) = E_Discriminant then

         --  If the selector is a discriminant of a constrained record type,
         --  rewrite the expression with the actual value of the discriminant.
         --  Don't do this on the left hand of an assignment statement (this
         --  happens in generated code, and means we really want to set it!)
         --  We also only do this optimization for discrete types, and not
         --  for access types (access discriminants get us into trouble!)
         --  We also do not expand the prefix of an attribute or the
         --  operand of an object renaming declaration.

         if Is_Record_Type (Ptyp)
           and then Has_Discriminants (Ptyp)
           and then Is_Constrained (Ptyp)
           and then Is_Discrete_Type (Etype (N))
           and then (Nkind (Par) /= N_Assignment_Statement
                       or else Name (Par) /= N)
           and then (Nkind (Par) /= N_Attribute_Reference
                       or else Prefix (Par) /= N)
           and then not Is_Renamed_Object (N)
         then
            declare
               D : Entity_Id;
               E : Elmt_Id;

            begin
               D := First_Discriminant (Ptyp);
               E := First_Elmt (Discriminant_Constraint (Ptyp));

               while Present (E) loop
                  if D = Entity (Selector_Name (N)) then

                     --  In the context of a case statement, the expression
                     --  may have the base type of the discriminant, and we
                     --  need to preserve the constraint to avoid spurious
                     --  errors on missing cases.

                     if Nkind (Parent (N)) = N_Case_Statement
                       and then Etype (Node (E)) /= Etype (D)
                     then
                        Rewrite (N,
                          Make_Qualified_Expression (Loc,
                            Subtype_Mark => New_Occurrence_Of (Etype (D), Loc),
                            Expression   => New_Copy (Node (E))));
                        Analyze (N);
                     else
                        Rewrite (N, New_Copy (Node (E)));
                     end if;

                     Set_Is_Static_Expression (N, False);
                     return;
                  end if;

                  Next_Elmt (E);
                  Next_Discriminant (D);
               end loop;

               --  Note: the above loop should always terminate, but if
               --  it does not, we just missed an optimization due to
               --  some glitch (perhaps a previous error), so ignore!
            end;
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

   end Expand_N_Selected_Component;

   --------------------
   -- Expand_N_Slice --
   --------------------

   procedure Expand_N_Slice (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Pfx  : constant Node_Id    := Prefix (N);
      Ptp  : Entity_Id           := Etype (Pfx);
      Ent  : Entity_Id;
      Decl : Node_Id;

   begin
      --  Special handling for access types

      if Is_Access_Type (Ptp) then

         --  Check for explicit dereference required for checked pool

         Insert_Dereference_Action (Pfx);

         --  If we have an access to a packed array type, then put in an
         --  explicit dereference. We do this in case the slice must be
         --  expanded, and we want to make sure we get an access check.

         Ptp := Designated_Type (Ptp);

         if Is_Array_Type (Ptp) and then Is_Packed (Ptp) then
            Rewrite (Pfx,
              Make_Explicit_Dereference (Sloc (N),
                Prefix => Relocate_Node (Pfx)));

            Analyze_And_Resolve (Pfx, Ptp);

            --  The prefix will now carry the Access_Check flag for the back
            --  end, remove it from slice itself.

            Set_Do_Access_Check (N, False);
         end if;
      end if;

      --  Range checks are potentially also needed for cases involving
      --  a slice indexed by a subtype indication, but Do_Range_Check
      --  can currently only be set for expressions ???

      if not Index_Checks_Suppressed (Ptp)
        and then (not Is_Entity_Name (Pfx)
                   or else not Index_Checks_Suppressed (Entity (Pfx)))
        and then Nkind (Discrete_Range (N)) /= N_Subtype_Indication
      then
         Enable_Range_Check (Discrete_Range (N));
      end if;

      --  The remaining case to be handled is packed slices. We can leave
      --  packed slices as they are in the following situations:

      --    1. Right or left side of an assignment (we can handle this
      --       situation correctly in the assignment statement expansion).

      --    2. Prefix of indexed component (the slide is optimized away
      --       in this case, see the start of Expand_N_Slice.

      --    3. Object renaming declaration, since we want the name of
      --       the slice, not the value.

      --    4. Argument to procedure call, since copy-in/copy-out handling
      --       may be required, and this is handled in the expansion of
      --       call itself.

      --    5. Prefix of an address attribute (this is an error which
      --       is caught elsewhere, and the expansion would intefere
      --       with generating the error message).

      if Is_Packed (Typ)
        and then Nkind (Parent (N)) /= N_Assignment_Statement
        and then Nkind (Parent (N)) /= N_Indexed_Component
        and then not Is_Renamed_Object (N)
        and then Nkind (Parent (N)) /= N_Procedure_Call_Statement
        and then (Nkind (Parent (N)) /= N_Attribute_Reference
                    or else
                  Attribute_Name (Parent (N)) /= Name_Address)
      then
         Ent :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Object_Definition   => New_Occurrence_Of (Typ, Loc));

         Set_No_Initialization (Decl);

         Insert_Actions (N, New_List (
           Decl,
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Ent, Loc),
             Expression => Relocate_Node (N))));

         Rewrite (N, New_Occurrence_Of (Ent, Loc));
         Analyze_And_Resolve (N, Typ);
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
      --  This is called in the case of record and array type conversions
      --  to see if there is a change of representation to be handled.
      --  Change of representation is actually handled at the assignment
      --  statement level, and what this procedure does is rewrite node N
      --  conversion as an assignment to temporary. If there is no change
      --  of representation, then the conversion node is unchanged.

      procedure Real_Range_Check;
      --  Handles generation of range check for real target value

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
         --  Nothing to do if no change of representation

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

            --  If type is unconstrained we have to add a constraint,
            --  copied from the actual value of the left hand side.

            if not Is_Constrained (Target_Type) then
               if Has_Discriminants (Operand_Type) then
                  Disc := First_Discriminant (Operand_Type);
                  Cons := New_List;
                  while Present (Disc) loop
                     Append_To (Cons,
                       Make_Selected_Component (Loc,
                         Prefix => Duplicate_Subexpr (Operand),
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
                                 Duplicate_Subexpr
                                   (Operand, Name_Req => True),
                               Attribute_Name => Name_First,
                               Expressions    => New_List (
                                 Make_Integer_Literal (Loc, J)))),

                         High_Bound =>
                           Unchecked_Convert_To (Etype (N_Ix),
                             Make_Attribute_Reference (Loc,
                               Prefix =>
                                 Duplicate_Subexpr
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

            Temp := Make_Defining_Identifier (Loc, New_Internal_Name ('C'));
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

      ----------------------
      -- Real_Range_Check --
      ----------------------

      --  Case of conversions to floating-point or fixed-point. If range
      --  checks are enabled and the target type has a range constraint,
      --  we convert:

      --     typ (x)

      --       to

      --     Tnn : typ'Base := typ'Base (x);
      --     [constraint_error when Tnn < typ'First or else Tnn > typ'Last]
      --     Tnn

      procedure Real_Range_Check is
         Btyp : constant Entity_Id := Base_Type (Target_Type);
         Lo   : constant Node_Id   := Type_Low_Bound  (Target_Type);
         Hi   : constant Node_Id   := Type_High_Bound (Target_Type);
         Conv : Node_Id;
         Tnn  : Entity_Id;

      begin
         --  Nothing to do if conversion was rewritten

         if Nkind (N) /= N_Type_Conversion then
            return;
         end if;

         --  Nothing to do if range checks suppressed, or target has the
         --  same range as the base type (or is the base type).

         if Range_Checks_Suppressed (Target_Type)
           or else (Lo = Type_Low_Bound (Btyp)
                      and then
                    Hi = Type_High_Bound (Btyp))
         then
            return;
         end if;

         --  Nothing to do if expression is an entity on which checks
         --  have been suppressed.

         if Is_Entity_Name (Expression (N))
           and then Range_Checks_Suppressed (Entity (Expression (N)))
         then
            return;
         end if;

         --  Here we rewrite the conversion as described above

         Conv := Relocate_Node (N);
         Rewrite
           (Subtype_Mark (Conv), New_Occurrence_Of (Btyp, Loc));
         Set_Etype (Conv, Btyp);

         --  Skip overflow check for integer to float conversions,
         --  since it is not needed, and in any case gigi generates
         --  incorrect code for such overflow checks ???

         if not Is_Integer_Type (Etype (Expression (N))) then
            Set_Do_Overflow_Check (Conv, True);
         end if;

         Tnn :=
           Make_Defining_Identifier (Loc,
             Chars => New_Internal_Name ('T'));

         Insert_Actions (N, New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tnn,
             Object_Definition   => New_Occurrence_Of (Btyp, Loc),
             Expression => Conv),

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

   --  Start of processing for Expand_N_Type_Conversion

   begin
      --  Nothing at all to do if conversion is to the identical type
      --  so remove the conversion completely, it is useless.

      if Operand_Type = Target_Type then
         Rewrite (N, Relocate_Node (Expression (N)));
         return;
      end if;

      --  Deal with Vax floating-point cases

      if Vax_Float (Operand_Type) or else Vax_Float (Target_Type) then
         Expand_Vax_Conversion (N);
         return;
      end if;

      --  Nothing to do if this is the second argument of read. This
      --  is a "backwards" conversion that will be handled by the
      --  specialized code in attribute processing.

      if Nkind (Parent (N)) = N_Attribute_Reference
        and then Attribute_Name (Parent (N)) = Name_Read
        and then Next (First (Expressions (Parent (N)))) = N
      then
         return;
      end if;

      --  Here if we may need to expand conversion

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

         --  Apply an accessibility check if the operand is an
         --  access parameter. Note that other checks may still
         --  need to be applied below (such as tagged type checks).

         if Is_Entity_Name (Operand)
           and then Ekind (Entity (Operand)) in Formal_Kind
           and then Ekind (Etype (Operand)) = E_Anonymous_Access_Type
         then
            Apply_Accessibility_Check (Operand, Target_Type);

         --  If the level of the operand type is statically deeper
         --  then the level of the target type, then force Program_Error.
         --  Note that this can only occur for cases where the attribute
         --  is within the body of an instantiation (otherwise the
         --  conversion will already have been rejected as illegal).
         --  Note: warnings are issued by the analyzer for the instance
         --  cases.

         elsif In_Instance_Body
           and then Type_Access_Level (Operand_Type) >
                    Type_Access_Level (Target_Type)
         then
            Rewrite (N,
              Make_Raise_Program_Error (Sloc (N),
                Reason => PE_Accessibility_Check_Failed));
            Set_Etype (N, Target_Type);

         --  When the operand is a selected access discriminant
         --  the check needs to be made against the level of the
         --  object denoted by the prefix of the selected name.
         --  Force Program_Error for this case as well (this
         --  accessibility violation can only happen if within
         --  the body of an instantiation).

         elsif In_Instance_Body
           and then Ekind (Operand_Type) = E_Anonymous_Access_Type
           and then Nkind (Operand) = N_Selected_Component
           and then Object_Access_Level (Operand) >
                      Type_Access_Level (Target_Type)
         then
            Rewrite (N,
              Make_Raise_Program_Error (Sloc (N),
                Reason => PE_Accessibility_Check_Failed));
            Set_Etype (N, Target_Type);
         end if;
      end if;

      --  Case of conversions of tagged types and access to tagged types

      --  When needed, that is to say when the expression is class-wide,
      --  Add runtime a tag check for (strict) downward conversion by using
      --  the membership test, generating:

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
         --  Do not do any expansion in the access type case if the
         --  parent is a renaming, since this is an error situation
         --  which will be caught by Sem_Ch8, and the expansion can
         --  intefere with this error check.

         if Is_Access_Type (Target_Type)
           and then Is_Renamed_Object (N)
         then
            return;
         end if;

         --  Oherwise, proceed with processing tagged conversion

         declare
            Actual_Operand_Type : Entity_Id;
            Actual_Target_Type  : Entity_Id;

            Cond : Node_Id;

         begin
            if Is_Access_Type (Target_Type) then
               Actual_Operand_Type := Designated_Type (Operand_Type);
               Actual_Target_Type  := Designated_Type (Target_Type);

            else
               Actual_Operand_Type := Operand_Type;
               Actual_Target_Type  := Target_Type;
            end if;

            if Is_Class_Wide_Type (Actual_Operand_Type)
              and then Root_Type (Actual_Operand_Type) /=  Actual_Target_Type
              and then Is_Ancestor
                         (Root_Type (Actual_Operand_Type),
                          Actual_Target_Type)
              and then not Tag_Checks_Suppressed (Actual_Target_Type)
            then
               --  The conversion is valid for any descendant of the
               --  target type

               Actual_Target_Type := Class_Wide_Type (Actual_Target_Type);

               if Is_Access_Type (Target_Type) then
                  Cond :=
                     Make_And_Then (Loc,
                       Left_Opnd =>
                         Make_Op_Ne (Loc,
                           Left_Opnd  => Duplicate_Subexpr (Operand),
                           Right_Opnd => Make_Null (Loc)),

                       Right_Opnd =>
                         Make_Not_In (Loc,
                           Left_Opnd  =>
                             Make_Explicit_Dereference (Loc,
                               Prefix => Duplicate_Subexpr (Operand)),
                           Right_Opnd =>
                             New_Reference_To (Actual_Target_Type, Loc)));

               else
                  Cond :=
                    Make_Not_In (Loc,
                      Left_Opnd  => Duplicate_Subexpr (Operand),
                      Right_Opnd =>
                        New_Reference_To (Actual_Target_Type, Loc));
               end if;

               Insert_Action (N,
                 Make_Raise_Constraint_Error (Loc,
                   Condition => Cond,
                   Reason    => CE_Tag_Check_Failed));

               Change_Conversion_To_Unchecked (N);
               Analyze_And_Resolve (N, Target_Type);
            end if;
         end;

      --  Case of other access type conversions

      elsif Is_Access_Type (Target_Type) then
         Apply_Constraint_Check (Operand, Target_Type);

      --  Case of conversions from a fixed-point type

      --  These conversions require special expansion and processing, found
      --  in the Exp_Fixd package. We ignore cases where Conversion_OK is
      --  set, since from a semantic point of view, these are simple integer
      --  conversions, which do not need further processing.

      elsif Is_Fixed_Point_Type (Operand_Type)
        and then not Conversion_OK (N)
      then
         --  We should never see universal fixed at this case, since the
         --  expansion of the constituent divide or multiply should have
         --  eliminated the explicit mention of universal fixed.

         pragma Assert (Operand_Type /= Universal_Fixed);

         --  Check for special case of the conversion to universal real
         --  that occurs as a result of the use of a round attribute.
         --  In this case, the real type for the conversion is taken
         --  from the target type of the Round attribute and the
         --  result must be marked as rounded.

         if Target_Type = Universal_Real
           and then Nkind (Parent (N)) = N_Attribute_Reference
           and then Attribute_Name (Parent (N)) = Name_Round
         then
            Set_Rounded_Result (N);
            Set_Etype (N, Etype (Parent (N)));
         end if;

         --  Otherwise do correct fixed-conversion, but skip these if the
         --  Conversion_OK flag is set, because from a semantic point of
         --  view these are simple integer conversions needing no further
         --  processing (the backend will simply treat them as integers)

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

      --  These conversions require special expansion and processing, found
      --  in the Exp_Fixd package. Again, ignore cases where Conversion_OK
      --  is set, since from a semantic point of view, these are simple
      --  integer conversions, which do not need further processing.

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
         --  Special processing required if the conversion is the expression
         --  of a Truncation attribute reference. In this case we replace:

         --     ityp (ftyp'Truncation (x))

         --  by

         --     ityp (x)

         --  with the Float_Truncate flag set. This is clearly more efficient.

         if Nkind (Operand) = N_Attribute_Reference
           and then Attribute_Name (Operand) = Name_Truncation
         then
            Rewrite (Operand,
              Relocate_Node (First (Expressions (Operand))));
            Set_Float_Truncate (N, True);
         end if;

         --  One more check here, gcc is still not able to do conversions of
         --  this type with proper overflow checking, and so gigi is doing an
         --  approximation of what is required by doing floating-point compares
         --  with the end-point. But that can lose precision in some cases, and
         --  give a wrong result. Converting the operand to Long_Long_Float is
         --  helpful, but still does not catch all cases with 64-bit integers
         --  on targets with only 64-bit floats ???

         if Do_Range_Check (Expression (N)) then
            Rewrite (Expression (N),
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_Long_Long_Float, Loc),
                Expression =>
                  Relocate_Node (Expression (N))));

            Set_Etype (Expression (N), Standard_Long_Long_Float);
            Enable_Range_Check (Expression (N));
            Set_Do_Range_Check (Expression (Expression (N)), False);
         end if;

      --  Case of array conversions

      --  Expansion of array conversions, add required length/range checks
      --  but only do this if there is no change of representation. For
      --  handling of this case, see Handle_Changed_Representation.

      elsif Is_Array_Type (Target_Type) then

         if Is_Constrained (Target_Type) then
            Apply_Length_Check (Operand, Target_Type);
         else
            Apply_Range_Check (Operand, Target_Type);
         end if;

         Handle_Changed_Representation;

      --  Case of conversions of discriminated types

      --  Add required discriminant checks if target is constrained. Again
      --  this change is skipped if we have a change of representation.

      elsif Has_Discriminants (Target_Type)
        and then Is_Constrained (Target_Type)
      then
         Apply_Discriminant_Check (Operand, Target_Type);
         Handle_Changed_Representation;

      --  Case of all other record conversions. The only processing required
      --  is to check for a change of representation requiring the special
      --  assignment processing.

      elsif Is_Record_Type (Target_Type) then
         Handle_Changed_Representation;

      --  Case of conversions of enumeration types

      elsif Is_Enumeration_Type (Target_Type) then

         --  Special processing is required if there is a change of
         --  representation (from enumeration representation clauses)

         if not Same_Representation (Target_Type, Operand_Type) then

            --  Convert: x(y) to x'val (ytyp'val (y))

            Rewrite (N,
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Target_Type, Loc),
                 Attribute_Name => Name_Val,
                 Expressions => New_List (
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Operand_Type, Loc),
                     Attribute_Name => Name_Pos,
                     Expressions => New_List (Operand)))));

            Analyze_And_Resolve (N, Target_Type);
         end if;

      --  Case of conversions to floating-point

      elsif Is_Floating_Point_Type (Target_Type) then
         Real_Range_Check;

      --  The remaining cases require no front end processing

      else
         null;
      end if;

      --  At this stage, either the conversion node has been transformed
      --  into some other equivalent expression, or left as a conversion
      --  that can be handled by Gigi. The conversions that Gigi can handle
      --  are the following:

      --    Conversions with no change of representation or type

      --    Numeric conversions involving integer values, floating-point
      --    values, and fixed-point values. Fixed-point values are allowed
      --    only if Conversion_OK is set, i.e. if the fixed-point values
      --    are to be treated as integers.

      --  No other conversions should be passed to Gigi.

   end Expand_N_Type_Conversion;

   -----------------------------------
   -- Expand_N_Unchecked_Expression --
   -----------------------------------

   --  Remove the unchecked expression node from the tree. It's job was simply
   --  to make sure that its constituent expression was handled with checks
   --  off, and now that that is done, we can remove it from the tree, and
   --  indeed must, since gigi does not expect to see these nodes.

   procedure Expand_N_Unchecked_Expression (N : Node_Id) is
      Exp : constant Node_Id := Expression (N);

   begin
      Set_Assignment_OK (Exp, Assignment_OK (N) or Assignment_OK (Exp));
      Rewrite (N, Exp);
   end Expand_N_Unchecked_Expression;

   ----------------------------------------
   -- Expand_N_Unchecked_Type_Conversion --
   ----------------------------------------

   --  If this cannot be handled by Gigi and we haven't already made
   --  a temporary for it, do it now.

   procedure Expand_N_Unchecked_Type_Conversion (N : Node_Id) is
      Target_Type  : constant Entity_Id := Etype (N);
      Operand      : constant Node_Id   := Expression (N);
      Operand_Type : constant Entity_Id := Etype (Operand);

   begin
      --  If we have a conversion of a compile time known value to a target
      --  type and the value is in range of the target type, then we can simply
      --  replace the construct by an integer literal of the correct type. We
      --  only apply this to integer types being converted. Possibly it may
      --  apply in other cases, but it is too much trouble to worry about.

      --  Note that we do not do this transformation if the Kill_Range_Check
      --  flag is set, since then the value may be outside the expected range.
      --  This happens in the Normalize_Scalars case.

      if Is_Integer_Type (Target_Type)
        and then Is_Integer_Type (Operand_Type)
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
               Analyze_And_Resolve (N, Target_Type);
               return;
            end if;
         end;
      end if;

      --  Nothing to do if conversion is safe

      if Safe_Unchecked_Type_Conversion (N) then
         return;
      end if;

      --  Otherwise force evaluation unless Assignment_OK flag is set (this
      --  flag indicates ??? -- more comments needed here)

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
      Bodies : List_Id)
      return   Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      function Suitable_Element (C : Entity_Id) return Entity_Id;
      --  Return the first field to compare beginning with C, skipping the
      --  inherited components

      function Suitable_Element (C : Entity_Id) return Entity_Id is
      begin
         if No (C) then
            return Empty;

         elsif Ekind (C) /= E_Discriminant
           and then Ekind (C) /= E_Component
         then
            return Suitable_Element (Next_Entity (C));

         elsif Is_Tagged_Type (Typ)
           and then C /= Original_Record_Component (C)
         then
            return Suitable_Element (Next_Entity (C));

         elsif Chars (C) = Name_uController
           or else Chars (C) = Name_uTag
         then
            return Suitable_Element (Next_Entity (C));

         else
            return C;
         end if;
      end Suitable_Element;

      Result : Node_Id;
      C      : Entity_Id;

      First_Time : Boolean := True;

   --  Start of processing for Expand_Record_Equality

   begin
      --  Special processing for the unchecked union case, which will occur
      --  only in the context of tagged types and dynamic dispatching, since
      --  other cases are handled statically. We return True, but insert a
      --  raise Program_Error statement.

      if Is_Unchecked_Union (Typ) then

         --  If this is a component of an enclosing record, return the Raise
         --  statement directly.

         if No (Parent (Lhs)) then
            Result :=
              Make_Raise_Program_Error (Loc,
                Reason => PE_Unchecked_Union_Restriction);
            Set_Etype (Result, Standard_Boolean);
            return Result;

         else
            Insert_Action (Lhs,
              Make_Raise_Program_Error (Loc,
                Reason => PE_Unchecked_Union_Restriction));
            return New_Occurrence_Of (Standard_True, Loc);
         end if;
      end if;

      --  Generates the following code: (assuming that Typ has one Discr and
      --  component C2 is also a record)

      --   True
      --     and then Lhs.Discr1 = Rhs.Discr1
      --     and then Lhs.C1 = Rhs.C1
      --     and then Lhs.C2.C1=Rhs.C2.C1 and then ... Lhs.C2.Cn=Rhs.C2.Cn
      --     and then ...
      --     and then Lhs.Cmpn = Rhs.Cmpn

      Result := New_Reference_To (Standard_True, Loc);
      C := Suitable_Element (First_Entity (Typ));

      while Present (C) loop

         declare
            New_Lhs : Node_Id;
            New_Rhs : Node_Id;

         begin
            if First_Time then
               First_Time := False;
               New_Lhs := Lhs;
               New_Rhs := Rhs;

            else
               New_Lhs := New_Copy_Tree (Lhs);
               New_Rhs := New_Copy_Tree (Rhs);
            end if;

            Result :=
              Make_And_Then (Loc,
                Left_Opnd  => Result,
                Right_Opnd =>
                  Expand_Composite_Equality (Nod, Etype (C),
                    Lhs =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Lhs,
                        Selector_Name => New_Reference_To (C, Loc)),
                    Rhs =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Rhs,
                        Selector_Name => New_Reference_To (C, Loc)),
                    Bodies => Bodies));
         end;

         C := Suitable_Element (Next_Entity (C));
      end loop;

      return Result;
   end Expand_Record_Equality;

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

   -------------------------------
   -- Insert_Dereference_Action --
   -------------------------------

   procedure Insert_Dereference_Action (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Pool : constant Entity_Id  := Associated_Storage_Pool (Typ);

      function Is_Checked_Storage_Pool (P : Entity_Id) return Boolean;
      --  return true if type of P is derived from Checked_Pool;

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

   --  Start of processing for Insert_Dereference_Action

   begin
      if not Comes_From_Source (Parent (N)) then
         return;

      elsif not Is_Checked_Storage_Pool (Pool) then
         return;
      end if;

      Insert_Action (N,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (
            Find_Prim_Op (Etype (Pool), Name_Dereference), Loc),

          Parameter_Associations => New_List (

            --  Pool

             New_Reference_To (Pool, Loc),

            --  Storage_Address

             Make_Attribute_Reference (Loc,
               Prefix         =>
                 Make_Explicit_Dereference (Loc, Duplicate_Subexpr (N)),
               Attribute_Name => Name_Address),

            --  Size_In_Storage_Elements

             Make_Op_Divide (Loc,
               Left_Opnd  =>
                Make_Attribute_Reference (Loc,
                  Prefix         =>
                    Make_Explicit_Dereference (Loc, Duplicate_Subexpr (N)),
                  Attribute_Name => Name_Size),
               Right_Opnd =>
                 Make_Integer_Literal (Loc, System_Storage_Unit)),

            --  Alignment

             Make_Attribute_Reference (Loc,
               Prefix         =>
                 Make_Explicit_Dereference (Loc, Duplicate_Subexpr (N)),
               Attribute_Name => Name_Alignment))));

   end Insert_Dereference_Action;

   ------------------------------
   -- Make_Array_Comparison_Op --
   ------------------------------

   --  This is a hand-coded expansion of the following generic function:

   --  generic
   --    type elem is  (<>);
   --    type index is (<>);
   --    type a is array (index range <>) of elem;
   --
   --  function Gnnn (X : a; Y: a) return boolean is
   --    J : index := Y'first;
   --
   --  begin
   --    if X'length = 0 then
   --       return false;
   --
   --    elsif Y'length = 0 then
   --       return true;
   --
   --    else
   --      for I in X'range loop
   --        if X (I) = Y (J) then
   --          if J = Y'last then
   --            exit;
   --          else
   --            J := index'succ (J);
   --          end if;
   --
   --        else
   --           return X (I) > Y (J);
   --        end if;
   --      end loop;
   --
   --      return X'length > Y'length;
   --    end if;
   --  end Gnnn;

   --  Note that since we are essentially doing this expansion by hand, we
   --  do not need to generate an actual or formal generic part, just the
   --  instantiated function itself.

   function Make_Array_Comparison_Op
     (Typ   : Entity_Id;
      Nod   : Node_Id)
      return  Node_Id
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
              Left_Opnd => New_Reference_To (J, Loc),
              Right_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix => New_Reference_To (Y, Loc),
                  Attribute_Name => Name_Last)),

          Then_Statements => New_List (
                Make_Exit_Statement (Loc)),

          Else_Statements =>
            New_List (
              Make_Assignment_Statement (Loc,
                Name => New_Reference_To (J, Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Reference_To (Index, Loc),
                    Attribute_Name => Name_Succ,
                    Expressions => New_List (New_Reference_To (J, Loc))))));

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
                  Prefix      => New_Reference_To (X, Loc),
                  Expressions => New_List (New_Reference_To (I, Loc))),

              Right_Opnd =>
                Make_Indexed_Component (Loc,
                  Prefix      => New_Reference_To (Y, Loc),
                  Expressions => New_List (New_Reference_To (J, Loc)))),

          Then_Statements => New_List (Inner_If),

          Else_Statements => New_List (
            Make_Return_Statement (Loc,
              Expression =>
                Make_Op_Gt (Loc,
                  Left_Opnd =>
                    Make_Indexed_Component (Loc,
                      Prefix      => New_Reference_To (X, Loc),
                      Expressions => New_List (New_Reference_To (I, Loc))),

                  Right_Opnd =>
                    Make_Indexed_Component (Loc,
                      Prefix      => New_Reference_To (Y, Loc),
                      Expressions => New_List (
                        New_Reference_To (J, Loc)))))));

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
                      Prefix => New_Reference_To (X, Loc),
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
          Prefix => New_Reference_To (X, Loc),
          Attribute_Name => Name_Length);

      Length2 :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Reference_To (Y, Loc),
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
                  Prefix => New_Reference_To (X, Loc),
                  Attribute_Name => Name_Length),
              Right_Opnd =>
                Make_Integer_Literal (Loc, 0)),

          Then_Statements =>
            New_List (
              Make_Return_Statement (Loc,
                Expression => New_Reference_To (Standard_False, Loc))),

          Elsif_Parts => New_List (
            Make_Elsif_Part (Loc,
              Condition =>
                Make_Op_Eq (Loc,
                  Left_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Reference_To (Y, Loc),
                      Attribute_Name => Name_Length),
                  Right_Opnd =>
                    Make_Integer_Literal (Loc, 0)),

              Then_Statements =>
                New_List (
                  Make_Return_Statement (Loc,
                     Expression => New_Reference_To (Standard_True, Loc))))),

          Else_Statements => New_List (
            Loop_Statement,
            Make_Return_Statement (Loc,
              Expression => Final_Expr)));

      --  (X : a; Y: a)

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => X,
          Parameter_Type      => New_Reference_To (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => Y,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      --  function Gnnn (...) return boolean is
      --    J : index := Y'first;
      --  begin
      --    if ... end if;
      --  end Gnnn;

      Func_Name := Make_Defining_Identifier (Loc, New_Internal_Name ('G'));

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean, Loc)),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => J,
              Object_Definition   => New_Reference_To (Index, Loc),
              Expression =>
                Make_Attribute_Reference (Loc,
                  Prefix => New_Reference_To (Y, Loc),
                  Attribute_Name => Name_First))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (If_Stat)));

      return Func_Body;

   end Make_Array_Comparison_Op;

   ---------------------------
   -- Make_Boolean_Array_Op --
   ---------------------------

   --  For logical operations on boolean arrays, expand in line the
   --  following, replacing 'and' with 'or' or 'xor' where needed:

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
     (Typ  : Entity_Id;
      N    : Node_Id)
      return Node_Id
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
          Prefix      => New_Reference_To (A, Loc),
          Expressions => New_List (New_Reference_To (J, Loc)));

      B_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Reference_To (B, Loc),
          Expressions => New_List (New_Reference_To (J, Loc)));

      C_J :=
        Make_Indexed_Component (Loc,
          Prefix      => New_Reference_To (C, Loc),
          Expressions => New_List (New_Reference_To (J, Loc)));

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
                      Prefix => New_Reference_To (A, Loc),
                      Attribute_Name => Name_Range))),

          Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => C_J,
              Expression => Op)));

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type      => New_Reference_To (Typ, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      Func_Name :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      Set_Is_Inlined (Func_Name);

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark             => New_Reference_To (Typ, Loc)),

          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => C,
              Object_Definition   => New_Reference_To (Typ, Loc))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Loop_Statement,
                Make_Return_Statement (Loc,
                  Expression => New_Reference_To (C, Loc)))));

      return Func_Body;
   end Make_Boolean_Array_Op;

   ------------------------
   -- Rewrite_Comparison --
   ------------------------

   procedure Rewrite_Comparison (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);
      Op1 : constant Node_Id   := Left_Opnd (N);
      Op2 : constant Node_Id   := Right_Opnd (N);

      Res : constant Compare_Result := Compile_Time_Compare (Op1, Op2);
      --  Res indicates if compare outcome can be determined at compile time

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

         when N_Op_Gt =>
            True_Result  := Res = GT;
            False_Result := Res in Compare_LE;

         when N_Op_Lt =>
            True_Result  := Res = LT;
            False_Result := Res in Compare_GE;

         when N_Op_Le =>
            True_Result  := Res in Compare_LE;
            False_Result := Res = GT;

         when N_Op_Ne =>
            True_Result  := Res = NE;
            False_Result := Res = LT or else Res = GT or else Res = EQ;
      end case;

      if True_Result then
         Rewrite (N,
           Convert_To (Typ, New_Occurrence_Of (Standard_True, Sloc (N))));
         Analyze_And_Resolve (N, Typ);
         Warn_On_Known_Condition (N);

      elsif False_Result then
         Rewrite (N,
           Convert_To (Typ, New_Occurrence_Of (Standard_False, Sloc (N))));
         Analyze_And_Resolve (N, Typ);
         Warn_On_Known_Condition (N);
      end if;
   end Rewrite_Comparison;

   -----------------------
   -- Tagged_Membership --
   -----------------------

   --  There are two different cases to consider depending on whether
   --  the right operand is a class-wide type or not. If not we just
   --  compare the actual tag of the left expr to the target type tag:
   --
   --     Left_Expr.Tag = Right_Type'Tag;
   --
   --  If it is a class-wide type we use the RT function CW_Membership which
   --  is usually implemented by looking in the ancestor tables contained in
   --  the dispatch table pointed by Left_Expr.Tag for Typ'Tag

   function Tagged_Membership (N : Node_Id) return Node_Id is
      Left  : constant Node_Id    := Left_Opnd  (N);
      Right : constant Node_Id    := Right_Opnd (N);
      Loc   : constant Source_Ptr := Sloc (N);

      Left_Type  : Entity_Id;
      Right_Type : Entity_Id;
      Obj_Tag    : Node_Id;

   begin
      Left_Type  := Etype (Left);
      Right_Type := Etype (Right);

      if Is_Class_Wide_Type (Left_Type) then
         Left_Type := Root_Type (Left_Type);
      end if;

      Obj_Tag :=
        Make_Selected_Component (Loc,
          Prefix        => Relocate_Node (Left),
          Selector_Name => New_Reference_To (Tag_Component (Left_Type), Loc));

      if Is_Class_Wide_Type (Right_Type) then
         return
           Make_DT_Access_Action (Left_Type,
             Action => CW_Membership,
             Args   => New_List (
               Obj_Tag,
               New_Reference_To (
                 Access_Disp_Table (Root_Type (Right_Type)), Loc)));
      else
         return
           Make_Op_Eq (Loc,
           Left_Opnd  => Obj_Tag,
           Right_Opnd =>
             New_Reference_To (Access_Disp_Table (Right_Type), Loc));
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
