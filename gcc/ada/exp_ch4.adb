------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 4                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
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
   --  If an boolean array assignment can be done in place, build call to
   --  corresponding library procedure.

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
   --  equality, and a call to it. Loc is the location for the generated
   --  nodes. Lhs and Rhs are the array expressions to be compared.
   --  Bodies is a list on which to attach bodies of local functions that
   --  are created in the process. It is the responsibility of the
   --  caller to insert those bodies at the right place. Nod provides
   --  the Sloc value for the generated code. Normally the types used
   --  for the generated equality routine are taken from Lhs and Rhs.
   --  However, in some situations of generated code, the Etype fields
   --  of Lhs and Rhs are not set yet. In such cases, Typ supplies the
   --  type to be used for the formal parameters.

   procedure Expand_Boolean_Operator (N : Node_Id);
   --  Common expansion processing for Boolean operators (And, Or, Xor)
   --  for the case of array type arguments.

   function Expand_Composite_Equality
     (Nod    : Node_Id;
      Typ    : Entity_Id;
      Lhs    : Node_Id;
      Rhs    : Node_Id;
      Bodies : List_Id) return Node_Id;
   --  Local recursive function used to expand equality for nested
   --  composite types. Used by Expand_Record/Array_Equality, Bodies
   --  is a list on which to attach bodies of local functions that are
   --  created in the process. This is the responsability of the caller
   --  to insert those bodies at the right place. Nod provides the Sloc
   --  value for generated code. Lhs and Rhs are the left and right sides
   --  for the comparison, and Typ is the type of the arrays to compare.

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

   function Get_Allocator_Final_List
     (N    : Node_Id;
      T    : Entity_Id;
      PtrT : Entity_Id) return Entity_Id;
   --  If the designated type is controlled, build final_list expression
   --  for created object. If context is an access parameter, create a
   --  local access type to have a usable finalization list.

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
   --  Comparisons between arrays are expanded in line. This function
   --  produces the body of the implementation of (a > b), where a and b
   --  are one-dimensional arrays of some discrete type. The original
   --  node is then expanded into the appropriate call to this function.
   --  Nod provides the Sloc value for the generated code.

   function Make_Boolean_Array_Op
     (Typ : Entity_Id;
      N   : Node_Id) return Node_Id;
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

   function Safe_In_Place_Array_Op
     (Lhs : Node_Id;
      Op1 : Node_Id;
      Op2 : Node_Id) return Boolean;
   --  In the context of an assignment, where the right-hand side is a
   --  boolean operation on arrays, check whether operation can be performed
   --  in place.

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

      Arg1      : constant Node_Id := Op1;
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
                    Prefix => Arg1,
                    Attribute_Name => Name_Address),
                  Make_Attribute_Reference (Loc,
                    Prefix => Arg2,
                    Attribute_Name => Name_Address),
                 Make_Attribute_Reference (Loc,
                   Prefix => Op1,
                    Attribute_Name => Name_Length)));
      end if;

      Rewrite (N, Call_Node);
      Analyze (N);

   exception
      when RE_Not_Available =>
         return;
   end Build_Boolean_Array_Proc_Call;

   ---------------------------------
   -- Expand_Allocator_Expression --
   ---------------------------------

   procedure Expand_Allocator_Expression (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Exp   : constant Node_Id    := Expression (Expression (N));
      Indic : constant Node_Id    := Subtype_Mark (Expression (N));
      PtrT  : constant Entity_Id  := Etype (N);
      T     : constant Entity_Id  := Entity (Indic);
      Flist : Node_Id;
      Node  : Node_Id;
      Temp  : Entity_Id;

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
                Expression          =>
                  Make_Allocator (Loc,
                    New_Reference_To (Etype (Exp), Loc)));

            Set_Comes_From_Source
              (Expression (Tmp_Node), Comes_From_Source (N));

            Set_No_Initialization (Expression (Tmp_Node));
            Insert_Action (N, Tmp_Node);

            if Controlled_Type (T)
              and then Ekind (PtrT) = E_Anonymous_Access_Type
            then
               --  Create local finalization list for access parameter

               Flist := Get_Allocator_Final_List (N, Base_Type (T), PtrT);
            end if;

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
                      New_Reference_To (First_Tag_Component (T), Loc)),

                Expression =>
                  Unchecked_Convert_To (RTE (RE_Tag),
                    New_Reference_To
                      (Elists.Node (First_Elmt (Access_Disp_Table (T))),
                       Loc)));

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
                         New_Reference_To (First_Tag_Component (Utyp), Loc)),

                   Expression =>
                     Unchecked_Convert_To (RTE (RE_Tag),
                       New_Reference_To (
                         Elists.Node (First_Elmt (Access_Disp_Table (Utyp))),
                         Loc)));

               Set_Assignment_OK (Name (Tag_Assign));
               Insert_Action (N, Tag_Assign);
            end;
         end if;

         if Controlled_Type (Designated_Type (PtrT))
            and then Controlled_Type (T)
         then
            declare
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
                  if Controlled_Type (T)
                    and then Ekind (PtrT) = E_Anonymous_Access_Type
                  then
                     --  Create local finalization list for access parameter

                     Flist :=
                       Get_Allocator_Final_List (N, Base_Type (T), PtrT);
                  else
                     Flist := Find_Final_List (PtrT);
                  end if;

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

         Set_Comes_From_Source
           (Expression (Tmp_Node), Comes_From_Source (N));

         Set_No_Initialization (Expression (Tmp_Node));
         Insert_Action (N, Tmp_Node);
         Convert_Aggr_In_Allocator (Tmp_Node, Exp);
         Rewrite (N, New_Reference_To (Temp, Loc));
         Analyze_And_Resolve (N, PtrT);

      elsif Is_Access_Type (Designated_Type (PtrT))
        and then Nkind (Exp) = N_Allocator
        and then Nkind (Expression (Exp)) /= N_Qualified_Expression
      then
         --  Apply constraint to designated subtype indication

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

   exception
      when RE_Not_Available =>
         return;
   end Expand_Allocator_Expression;

   -----------------------------
   -- Expand_Array_Comparison --
   -----------------------------

   --  Expansion is only required in the case of array types. For the
   --  unpacked case, an appropriate runtime routine is called. For
   --  packed cases, and also in some other cases where a runtime
   --  routine cannot be called, the form of the expansion is:

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
      --  Returns True if the length of the given operand is known to be
      --  less than 4. Returns False if this length is known to be four
      --  or greater or is not known at compile time.

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
      --  by bytes, and for the JVM, since the JVM does not support direct
      --  addressing of array components.

      if not Is_Bit_Packed_Array (Typ1)
        and then Byte_Addressable
        and then not Java_VM
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
          Name => New_Reference_To (Func_Name, Loc),
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

   --  Expand an equality function for multi-dimensional arrays. Here is
   --  an example of such a function for Nb_Dimension = 2

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

   --  Note on the formal types used (atyp and btyp). If either of the
   --  arrays is of a private type, we use the underlying type, and
   --  do an unchecked conversion of the actual. If either of the arrays
   --  has a bound depending on a discriminant, then we use the base type
   --  since otherwise we have an escaped discriminant in the function.

   --  If both arrays are constrained and have the same bounds, we can
   --  generate a loop with an explicit iteration scheme using a 'Range
   --  attribute over the first array.

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
      --  Create one statement to compare corresponding components,
      --  designated by a full set of indices.

      function Get_Arg_Type (N : Node_Id) return Entity_Id;
      --  Given one of the arguments, computes the appropriate type to
      --  be used for that argument in the corresponding function formal

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
      --  If both indices are constrained and identical, the procedure
      --  returns a simpler loop:
      --
      --      for An in A'Range (N) loop
      --         xxx
      --      end loop
      --
      --  N is the dimension for which we are generating a loop. Index is the
      --  N'th index node, whose Etype is Index_Type_n in the above code.
      --  The xxx statement is either the loop or declare for the next
      --  dimension or if this is the last dimension the comparison
      --  of corresponding components of the arrays.
      --
      --  The actual way the code works is to return the comparison
      --  of corresponding components for the N+1 call. That's neater!

      function Test_Empty_Arrays return Node_Id;
      --  This function constructs the test for both arrays being empty
      --    (A'length (1) = 0 or else A'length (2) = 0 or else ...)
      --      and then
      --    (B'length (1) = 0 or else B'length (2) = 0 or else ...)

      function Test_Lengths_Correspond return Node_Id;
      --  This function constructs the test for arrays having different
      --  lengths in at least one index position, in which case resull

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
            Prefix => New_Reference_To (Arr, Loc),
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
             Prefix => Make_Identifier (Loc, Chars (A)),
             Expressions => Index_List1);

         R :=
           Make_Indexed_Component (Loc,
             Prefix => Make_Identifier (Loc, Chars (B)),
             Expressions => Index_List2);

         Test := Expand_Composite_Equality
                   (Nod, Component_Type (Typ), L, R, Decls);

         --  If some (sub)component is an unchecked_union, the whole operation
         --  will raise program error.

         if Nkind (Test) = N_Raise_Program_Error then

            --  This node is going to be inserted at a location where a
            --  statement is expected: clear its Etype so analysis will
            --  set it to the expected Standard_Void_Type.

            Set_Etype (Test, Empty);
            return Test;

         else
            return
              Make_Implicit_If_Statement (Nod,
                Condition => Make_Op_Not (Loc, Right_Opnd => Test),
                Then_Statements => New_List (
                  Make_Return_Statement (Loc,
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
               if Denotes_Discriminant (Type_Low_Bound (Etype (X)))
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
                                   Ltyp /= Rtyp
                                     or else not Is_Constrained (Ltyp);
         --  If the index types are identical, and we are working with
         --  constrained types, then we can use the same index for both of
         --  the arrays.

         An : constant Entity_Id := Make_Defining_Identifier (Loc,
                                      Chars => New_Internal_Name ('A'));

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
            Bn :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('B'));
         else
            Bn := An;
         end if;

         Append (New_Reference_To (An, Loc), Index_List1);
         Append (New_Reference_To (Bn, Loc), Index_List2);

         Stm_List := New_List (
           Handle_One_Dimension (N + 1, Next_Index (Index)));

         if Need_Separate_Indexes then

            --  Generate guard for loop, followed by increments of indices

            Append_To (Stm_List,
               Make_Exit_Statement (Loc,
                 Condition =>
                   Make_Op_Eq (Loc,
                      Left_Opnd => New_Reference_To (An, Loc),
                      Right_Opnd => Arr_Attr (A, Name_Last, N))));

            Append_To (Stm_List,
              Make_Assignment_Statement (Loc,
                Name       => New_Reference_To (An, Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Reference_To (Index_T, Loc),
                    Attribute_Name => Name_Succ,
                    Expressions    => New_List (New_Reference_To (An, Loc)))));

            Append_To (Stm_List,
              Make_Assignment_Statement (Loc,
                Name       => New_Reference_To (Bn, Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Reference_To (Index_T, Loc),
                    Attribute_Name => Name_Succ,
                    Expressions    => New_List (New_Reference_To (Bn, Loc)))));
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
                    Object_Definition   => New_Reference_To (Index_T, Loc),
                    Expression          => Arr_Attr (A, Name_First, N)),

                  Make_Object_Declaration (Loc,
                    Defining_Identifier => Bn,
                    Object_Definition   => New_Reference_To (Index_T, Loc),
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

      --  For now, if the argument types are not the same, go to the
      --  base type, since the code assumes that the formals have the
      --  same type. This is fixable in future ???

      if Ltyp /= Rtyp then
         Ltyp := Base_Type (Ltyp);
         Rtyp := Base_Type (Rtyp);
         pragma Assert (Ltyp = Rtyp);
      end if;

      --  Build list of formals for function

      Formals := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier => A,
          Parameter_Type      => New_Reference_To (Ltyp, Loc)),

        Make_Parameter_Specification (Loc,
          Defining_Identifier => B,
          Parameter_Type      => New_Reference_To (Rtyp, Loc)));

      Func_Name := Make_Defining_Identifier (Loc,  New_Internal_Name ('E'));

      --  Build statement sequence for function

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Function_Specification (Loc,
              Defining_Unit_Name       => Func_Name,
              Parameter_Specifications => Formals,
              Subtype_Mark => New_Reference_To (Standard_Boolean, Loc)),

          Declarations =>  Decls,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (

                Make_Implicit_If_Statement (Nod,
                  Condition => Test_Empty_Arrays,
                  Then_Statements => New_List (
                    Make_Return_Statement (Loc,
                      Expression =>
                        New_Occurrence_Of (Standard_True, Loc)))),

                Make_Implicit_If_Statement (Nod,
                  Condition => Test_Lengths_Correspond,
                  Then_Statements => New_List (
                    Make_Return_Statement (Loc,
                      Expression =>
                        New_Occurrence_Of (Standard_False, Loc)))),

                Handle_One_Dimension (1, First_Index (Ltyp)),

                Make_Return_Statement (Loc,
                  Expression => New_Occurrence_Of (Standard_True, Loc)))));

         Set_Has_Completion (Func_Name, True);
         Set_Is_Inlined (Func_Name);

         --  If the array type is distinct from the type of the arguments,
         --  it is the full view of a private type. Apply an unchecked
         --  conversion to insure that analysis of the call succeeds.

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
             Name                   => New_Reference_To (Func_Name, Loc),
             Parameter_Associations => Actuals);
   end Expand_Array_Equality;

   -----------------------------
   -- Expand_Boolean_Operator --
   -----------------------------

   --  Note that we first get the actual subtypes of the operands,
   --  since we always want to deal with types that have bounds.

   procedure Expand_Boolean_Operator (N : Node_Id) is
      Typ : constant Entity_Id  := Etype (N);

   begin
      --  Special case of bit packed array where both operands are known
      --  to be properly aligned. In this case we use an efficient run time
      --  routine to carry out the operation (see System.Bit_Ops).

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
                Name                   => New_Reference_To (Func_Name, Loc),
                Parameter_Associations =>
                  New_List (
                    L,
                    Make_Type_Conversion
                      (Loc, New_Reference_To (Etype (L), Loc), R))));

            Analyze_And_Resolve (N, Typ);
         end if;
      end;
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
      Bodies : List_Id) return Node_Id
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
            return Expand_Array_Equality (Nod, Lhs, Rhs, Bodies, Full_Type);
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
                       Etype (Next_Formal (First_Formal (Eq_Op)))
              and then Base_Type (Etype (Eq_Op)) = Standard_Boolean;
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
         Eq_Op := TSS (Full_Type, TSS_Composite_Equality);

         if Present (Eq_Op) then
            if Etype (First_Formal (Eq_Op)) /= Full_Type then

               --  Inherited equality from parent type. Convert the actuals
               --  to match signature of operation.

               declare
                  T : constant Entity_Id := Etype (First_Formal (Eq_Op));

               begin
                  return
                    Make_Function_Call (Loc,
                      Name => New_Reference_To (Eq_Op, Loc),
                      Parameter_Associations =>
                        New_List (OK_Convert_To (T, Lhs),
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

                        --  Since the enclosing record can never be an
                        --  Unchecked_Union (this code is executed for records
                        --  that do not have variants), we may reference its
                        --  discriminant(s).

                        if Nkind (Lhs) = N_Selected_Component
                          and then Has_Per_Object_Constraint (
                                     Entity (Selector_Name (Lhs)))
                        then
                           Lhs_Discr_Val :=
                             Make_Selected_Component (Loc,
                               Prefix => Prefix (Lhs),
                               Selector_Name =>
                                 New_Copy (
                                   Get_Discriminant_Value (
                                     First_Discriminant (Lhs_Type),
                                     Lhs_Type,
                                     Stored_Constraint (Lhs_Type))));

                        else
                           Lhs_Discr_Val := New_Copy (
                             Get_Discriminant_Value (
                               First_Discriminant (Lhs_Type),
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
                          and then Has_Per_Object_Constraint (
                                     Entity (Selector_Name (Rhs)))
                        then
                           Rhs_Discr_Val :=
                             Make_Selected_Component (Loc,
                               Prefix => Prefix (Rhs),
                               Selector_Name =>
                                 New_Copy (
                                   Get_Discriminant_Value (
                                     First_Discriminant (Rhs_Type),
                                     Rhs_Type,
                                     Stored_Constraint (Rhs_Type))));

                        else
                           Rhs_Discr_Val := New_Copy (
                             Get_Discriminant_Value (
                               First_Discriminant (Rhs_Type),
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
                         Name => New_Reference_To (Eq_Op, Loc),
                         Parameter_Associations => New_List (
                           Lhs,
                           Rhs,
                           Lhs_Discr_Val,
                           Rhs_Discr_Val));
                  end;
               end if;

               --  Shouldn't this be an else, we can't fall through
               --  the above IF, right???

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

   --         . . .

   --         if Sn'Length /= 0 then
   --            P := Sn'First;
   --            loop
   --               R (L) := Sn (P);
   --               L := Ind_Typ'Succ (L);
   --               exit when P = Sn'Last;
   --               P := Ind_Typ'Succ (P);
   --            end loop;
   --         end if;

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

      function Copy_Into_R_S (I : Nat; Last : Boolean) return List_Id;
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
      --  If the flag Last is true, the exit statement is emitted before
      --  incrementing the lower bound, to prevent the creation out of
      --  bound values.

      function Init_L (I : Nat) return Node_Id;
      --  Builds the statement:
      --    L := Arr_Typ'First;  If Arr_Typ is constrained
      --    L := Si'First;       otherwise (where I is the input param given)

      function H return Node_Id;
      --  Builds reference to identifier H

      function Ind_Val (E : Node_Id) return Node_Id;
      --  Builds expression Ind_Typ'Val (E);

      function L return Node_Id;
      --  Builds reference to identifier L

      function L_Pos return Node_Id;
      --  Builds expression Integer_Type'(Ind_Typ'Pos (L)). We qualify the
      --  expression to avoid universal_integer computations whenever possible,
      --  in the expression for the upper bound H.

      function L_Succ return Node_Id;
      --  Builds expression Ind_Typ'Succ (L)

      function One return Node_Id;
      --  Builds integer literal one

      function P return Node_Id;
      --  Builds reference to identifier P

      function P_Succ return Node_Id;
      --  Builds expression Ind_Typ'Succ (P)

      function R return Node_Id;
      --  Builds reference to identifier R

      function S (I : Nat) return Node_Id;
      --  Builds reference to identifier Si, where I is the value given

      function S_First (I : Nat) return Node_Id;
      --  Builds expression Si'First, where I is the value given

      function S_Last (I : Nat) return Node_Id;
      --  Builds expression Si'Last, where I is the value given

      function S_Length (I : Nat) return Node_Id;
      --  Builds expression Si'Length, where I is the value given

      function S_Length_Test (I : Nat) return Node_Id;
      --  Builds expression Si'Length /= 0, where I is the value given

      -------------------
      -- Copy_Into_R_S --
      -------------------

      function Copy_Into_R_S (I : Nat; Last : Boolean) return List_Id is
         Stmts     : constant List_Id := New_List;
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

         if Last then
            Loop_Stmt :=
              Make_Implicit_Loop_Statement (Cnode,
                Statements => New_List (R_Copy, Exit_Stmt, L_Inc, P_Inc));
         else
            Loop_Stmt :=
              Make_Implicit_Loop_Statement (Cnode,
                Statements => New_List (R_Copy, L_Inc, Exit_Stmt, P_Inc));
         end if;

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
         Target_Type : Entity_Id;

      begin
         --  If the index type is an enumeration type, the computation
         --  can be done in standard integer. Otherwise, choose a large
         --  enough integer type.

         if Is_Enumeration_Type (Ind_Typ)
           or else Root_Type (Ind_Typ) = Standard_Integer
           or else Root_Type (Ind_Typ) = Standard_Short_Integer
           or else Root_Type (Ind_Typ) = Standard_Short_Short_Integer
         then
            Target_Type := Standard_Integer;
         else
            Target_Type := Root_Type (Ind_Typ);
         end if;

         return
           Make_Qualified_Expression (Loc,
              Subtype_Mark => New_Reference_To (Target_Type, Loc),
              Expression   =>
                Make_Attribute_Reference (Loc,
                  Prefix         => New_Reference_To (Ind_Typ, Loc),
                  Attribute_Name => Name_Pos,
                  Expressions    => New_List (L)));
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
                      Then_Statements => Copy_Into_R_S (I, I = Nb_Opnds)));
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

   exception
      when RE_Not_Available =>
         return;
   end Expand_Concatenate_String;

   ------------------------
   -- Expand_N_Allocator --
   ------------------------

   procedure Expand_N_Allocator (N : Node_Id) is
      PtrT  : constant Entity_Id  := Etype (N);
      Dtyp  : constant Entity_Id  := Designated_Type (PtrT);
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

         elsif Is_Class_Wide_Type (Etype (Storage_Pool (N))) then
            Set_Procedure_To_Call (N, RTE (RE_Allocate_Any));

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

      --  The allocator is not part of a constraint on a record component,
      --  because in that case the inserted actions are delayed until the
      --  record declaration is fully analyzed, which is too late for the
      --  analysis of the rewritten allocator.

      if Is_Access_Constant (PtrT)
        and then Nkind (Expression (N)) = N_Qualified_Expression
        and then Compile_Time_Known_Value (Expression (Expression (N)))
        and then Size_Known_At_Compile_Time (Etype (Expression
                                                    (Expression (N))))
        and then not Is_Record_Type (Current_Scope)
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
             Prefix => New_Occurrence_Of (Temp, Loc),
             Attribute_Name => Name_Unrestricted_Access));

         Analyze_And_Resolve (N, PtrT);

         --  We set the variable as statically allocated, since we don't
         --  want it going on the stack of the current procedure!

         Set_Is_Statically_Allocated (Temp);
         return;
      end if;

      --  Handle case of qualified expression (other than optimization above)

      if Nkind (Expression (N)) = N_Qualified_Expression then
         Expand_Allocator_Expression (N);

         --  If the allocator is for a type which requires initialization, and
         --  there is no initial value (i.e. operand is a subtype indication
         --  rather than a qualifed expression), then we must generate a call
         --  to the initialization routine. This is done using an expression
         --  actions node:
         --
         --     [Pnnn : constant ptr_T := new (T); Init (Pnnn.all,...); Pnnn]
         --
         --  Here ptr_T is the pointer type for the allocator, and T is the
         --  subtype of the allocator. A special case arises if the designated
         --  type of the access type is a task or contains tasks. In this case
         --  the call to Init (Temp.all ...) is replaced by code that ensures
         --  that tasks get activated (see Exp_Ch9.Build_Task_Allocate_Block
         --  for details). In addition, if the type T is a task T, then the
         --  first argument to Init must be converted to the task record type.

      else
         declare
            T            : constant Entity_Id  := Entity (Expression (N));
            Init         : Entity_Id;
            Arg1         : Node_Id;
            Args         : List_Id;
            Decls        : List_Id;
            Decl         : Node_Id;
            Discr        : Elmt_Id;
            Flist        : Node_Id;
            Temp_Decl    : Node_Id;
            Temp_Type    : Entity_Id;
            Attach_Level : Uint;

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

                  if Is_Class_Wide_Type (Dtyp) then
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
                     Append (New_Copy_Tree (Elists.Node (Discr)), Args);
                     Next_Elmt (Discr);
                  end loop;

               elsif Is_Private_Type (T)
                 and then Present (Full_View (T))
                 and then Has_Discriminants (Full_View (T))
               then
                  Discr :=
                    First_Elmt (Discriminant_Constraint (Full_View (T)));

                  while Present (Discr) loop
                     Append (New_Copy_Tree (Elists.Node (Discr)), Args);
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

               --  If the designated type is task type or contains tasks,
               --  Create block to activate created tasks, and insert
               --  declaration for Task_Image variable ahead of call.

               if Has_Task (T) then
                  declare
                     L   : constant List_Id := New_List;
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
                  Flist := Get_Allocator_Final_List (N, Base_Type (T), PtrT);
                  if Ekind (PtrT) = E_Anonymous_Access_Type then
                     Attach_Level := Uint_1;
                  else
                     Attach_Level := Uint_2;
                  end if;
                  Insert_Actions (N,
                    Make_Init_Call (
                      Ref          => New_Copy_Tree (Arg1),
                      Typ          => T,
                      Flist_Ref    => Flist,
                      With_Attach  => Make_Integer_Literal (Loc,
                        Attach_Level)));
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

   exception
      when RE_Not_Available =>
         return;
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

      --  and replace the conditional expression by a reference to Cnn

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

         Set_Assignment_OK (Name (First (Then_Statements (New_If))));
         Set_Assignment_OK (Name (First (Else_Statements (New_If))));

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
      Loc    : constant Source_Ptr := Sloc (N);
      Rtyp   : constant Entity_Id  := Etype (N);
      Lop    : constant Node_Id    := Left_Opnd (N);
      Rop    : constant Node_Id    := Right_Opnd (N);
      Static : constant Boolean    := Is_OK_Static_Expression (N);

   begin
      --  If we have an explicit range, do a bit of optimization based
      --  on range analysis (we may be able to kill one or both checks).

      if Nkind (Rop) = N_Range then
         declare
            Lcheck : constant Compare_Result :=
                       Compile_Time_Compare (Lop, Low_Bound (Rop));
            Ucheck : constant Compare_Result :=
                       Compile_Time_Compare (Lop, High_Bound (Rop));

         begin
            --  If either check is known to fail, replace result
            --  by False, since the other check does not matter.
            --  Preserve the static flag for legality checks, because
            --  we are constant-folding beyond RM 4.9.

            if Lcheck = LT or else Ucheck = GT then
               Rewrite (N,
                 New_Reference_To (Standard_False, Loc));
               Analyze_And_Resolve (N, Rtyp);
               Set_Is_Static_Expression (N, Static);
               return;

            --  If both checks are known to succeed, replace result
            --  by True, since we know we are in range.

            elsif Lcheck in Compare_GE and then Ucheck in Compare_LE then
               Rewrite (N,
                 New_Reference_To (Standard_True, Loc));
               Analyze_And_Resolve (N, Rtyp);
               Set_Is_Static_Expression (N, Static);
               return;

            --  If lower bound check succeeds and upper bound check is
            --  not known to succeed or fail, then replace the range check
            --  with a comparison against the upper bound.

            elsif Lcheck in Compare_GE then
               Rewrite (N,
                 Make_Op_Le (Loc,
                   Left_Opnd  => Lop,
                   Right_Opnd => High_Bound (Rop)));
               Analyze_And_Resolve (N, Rtyp);
               return;

            --  If upper bound check succeeds and lower bound check is
            --  not known to succeed or fail, then replace the range check
            --  with a comparison against the lower bound.

            elsif Ucheck in Compare_LE then
               Rewrite (N,
                 Make_Op_Ge (Loc,
                   Left_Opnd  => Lop,
                   Right_Opnd => Low_Bound (Rop)));
               Analyze_And_Resolve (N, Rtyp);
               return;
            end if;
         end;

         --  For all other cases of an explicit range, nothing to be done

         return;

      --  Here right operand is a subtype mark

      else
         declare
            Typ    : Entity_Id        := Etype (Rop);
            Is_Acc : constant Boolean := Is_Access_Type (Typ);
            Obj    : Node_Id          := Lop;
            Cond   : Node_Id          := Empty;

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
               Rewrite (Rop,
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

               --  Prevent Gigi from generating incorrect code by rewriting
               --  the test as a standard False.

               Rewrite (N,
                 New_Occurrence_Of (Standard_False, Loc));

               return;
            end if;

            --  Here we have a non-scalar type

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

               Check_Subscripts : declare
                  function Construct_Attribute_Reference
                    (E   : Node_Id;
                     Nam : Name_Id;
                     Dim : Nat) return Node_Id;
                  --  Build attribute reference E'Nam(Dim)

                  -----------------------------------
                  -- Construct_Attribute_Reference --
                  -----------------------------------

                  function Construct_Attribute_Reference
                    (E   : Node_Id;
                     Nam : Name_Id;
                     Dim : Nat) return Node_Id
                  is
                  begin
                     return
                       Make_Attribute_Reference (Loc,
                         Prefix => E,
                         Attribute_Name => Nam,
                         Expressions => New_List (
                           Make_Integer_Literal (Loc, Dim)));
                  end Construct_Attribute_Reference;

               --  Start processing for Check_Subscripts

               begin
                  for J in 1 .. Number_Dimensions (Typ) loop
                     Evolve_And_Then (Cond,
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Construct_Attribute_Reference
                             (Duplicate_Subexpr_No_Checks (Obj),
                              Name_First, J),
                         Right_Opnd =>
                           Construct_Attribute_Reference
                             (New_Occurrence_Of (Typ, Loc), Name_First, J)));

                     Evolve_And_Then (Cond,
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Construct_Attribute_Reference
                             (Duplicate_Subexpr_No_Checks (Obj),
                              Name_Last, J),
                         Right_Opnd =>
                           Construct_Attribute_Reference
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
                  Analyze_And_Resolve (N, Rtyp);
               end Check_Subscripts;

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
         Insert_Explicit_Dereference (P);
         Analyze_And_Resolve (P, Designated_Type (T));
      end if;

      --  Generate index and validity checks

      Generate_Index_Checks (N);

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

      --    Left side of assignment, or prefix of left side of assignment,
      --    or prefix of the prefix, to handle packed arrays of packed arrays,
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

            --  If the expression is an index of an indexed component,
            --  it must be expanded regardless of context.

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

      --  Deal with software overflow checking

      if not Backend_Overflow_Checks_On_Target
         and then Is_Signed_Integer_Type (Etype (N))
         and then Do_Overflow_Check (N)
      then
         --  The only case to worry about is when the argument is
         --  equal to the largest negative number, so what we do is
         --  to insert the check:

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

   Max_Available_String_Operands : Int := -1;
   --  This is initialized the first time this routine is called. It records
   --  a value of 0,2,3,4,5 depending on what Str_Concat_n procedures are
   --  available in the run-time:
   --
   --    0  None available
   --    2  RE_Str_Concat available, RE_Str_Concat_3 not available
   --    3  RE_Str_Concat/Concat_2 available, RE_Str_Concat_4 not available
   --    4  RE_Str_Concat/Concat_2/3 available, RE_Str_Concat_5 not available
   --    5  All routines including RE_Str_Concat_5 available

   Char_Concat_Available : Boolean;
   --  Records if the routines RE_Str_Concat_CC/CS/SC are available. True if
   --  all three are available, False if any one of these is unavailable.

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
      --  Initialize global variables showing run-time status

      if Max_Available_String_Operands < 1 then
         if not RTE_Available (RE_Str_Concat) then
            Max_Available_String_Operands := 0;
         elsif not RTE_Available (RE_Str_Concat_3) then
            Max_Available_String_Operands := 2;
         elsif not RTE_Available (RE_Str_Concat_4) then
            Max_Available_String_Operands := 3;
         elsif not RTE_Available (RE_Str_Concat_5) then
            Max_Available_String_Operands := 4;
         else
            Max_Available_String_Operands := 5;
         end if;

         Char_Concat_Available :=
           RTE_Available (RE_Str_Concat_CC)
             and then
           RTE_Available (RE_Str_Concat_CS)
             and then
           RTE_Available (RE_Str_Concat_SC);
      end if;

      --  Ensure validity of both operands

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

         --  The inner loop gathers concatenation operands. We gather any
         --  number of these in the non-string case, or if no concatenation
         --  routines are available for string (since in that case we will
         --  treat string like any other non-string case). Otherwise we only
         --  gather as many operands as can be handled by the available
         --  procedures in the run-time library (normally 5, but may be
         --  less for the configurable run-time case).

         Inner : while Cnode /= N
                   and then (Base_Type (Etype (Cnode)) /= Standard_String
                               or else
                             Max_Available_String_Operands = 0
                               or else
                             List_Length (Opnds) <
                                               Max_Available_String_Operands)
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

         if (List_Length (Opnds) > 2 or else Atyp /= Standard_String)
           or else not Char_Concat_Available
         then
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

         if Atyp = Standard_String
           and then Max_Available_String_Operands > 0
         then
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

      --  We cannot do this transformation in configurable run time mode if we
      --  have 64-bit --  integers and long shifts are not available.

        and then
          (Esize (Ltyp) <= 32
             or else Support_Long_Shifts_On_Target)
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

         --  Check for 64-bit division available

         if Esize (Ltyp) > 32
           and then not Support_64_Bit_Divides_On_Target
         then
            Error_Msg_CRT ("64-bit division", N);
         end if;
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
      --  Determines whether a type has a subcompoment of an unconstrained
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
               Lhs_Type      : constant Node_Id := Etype (L_Exp);
               Rhs_Type      : constant Node_Id := Etype (R_Exp);
               Lhs_Discr_Val : Node_Id;
               Rhs_Discr_Val : Node_Id;

            begin
               --  Per-object constrained selected components require special
               --  attention. If the enclosing scope of the component is an
               --  Unchecked_Union, we can not reference its discriminants
               --  directly. This is why we use the two extra parameters of
               --  the equality function of the enclosing Unchecked_Union.

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
               --  formals to capture the inferred discriminant values.

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

               --     . . . Obj1 = Obj2 . . .

               --     Generated code:

               --     if not (uu_typeEQ (obj1.comp, obj2.comp,
               --                        obj1.discr, obj2.discr)) then

               --  In this case we can directly reference the discriminants of
               --  the enclosing record.

               --  Lhs of equality

               if Nkind (Lhs) = N_Selected_Component
                 and then Has_Per_Object_Constraint
                            (Entity (Selector_Name (Lhs)))
               then
                  --  Enclosing record is an Unchecked_Union, use formal A

                  if Is_Unchecked_Union (Scope
                       (Entity (Selector_Name (Lhs))))
                  then
                     Lhs_Discr_Val :=
                       Make_Identifier (Loc,
                         Chars => Name_A);

                  --  Enclosing record is of a non-Unchecked_Union type, it is
                  --  possible to reference the discriminant.

                  else
                     Lhs_Discr_Val :=
                       Make_Selected_Component (Loc,
                         Prefix => Prefix (Lhs),
                         Selector_Name =>
                           New_Copy
                             (Get_Discriminant_Value
                                (First_Discriminant (Lhs_Type),
                                 Lhs_Type,
                                 Stored_Constraint (Lhs_Type))));
                  end if;

               --  Comment needed here ???

               else
                  --  Infer the discriminant value

                  Lhs_Discr_Val :=
                    New_Copy
                      (Get_Discriminant_Value
                         (First_Discriminant (Lhs_Type),
                          Lhs_Type,
                          Stored_Constraint (Lhs_Type)));
               end if;

               --  Rhs of equality

               if Nkind (Rhs) = N_Selected_Component
                 and then Has_Per_Object_Constraint
                            (Entity (Selector_Name (Rhs)))
               then
                  if Is_Unchecked_Union
                       (Scope (Entity (Selector_Name (Rhs))))
                  then
                     Rhs_Discr_Val :=
                       Make_Identifier (Loc,
                         Chars => Name_B);

                  else
                     Rhs_Discr_Val :=
                       Make_Selected_Component (Loc,
                         Prefix => Prefix (Rhs),
                         Selector_Name =>
                           New_Copy (Get_Discriminant_Value (
                             First_Discriminant (Rhs_Type),
                             Rhs_Type,
                             Stored_Constraint (Rhs_Type))));

                  end if;
               else
                  Rhs_Discr_Val :=
                    New_Copy (Get_Discriminant_Value (
                      First_Discriminant (Rhs_Type),
                      Rhs_Type,
                      Stored_Constraint (Rhs_Type)));

               end if;

               Rewrite (N,
                 Make_Function_Call (Loc,
                   Name => New_Reference_To (Eq, Loc),
                   Parameter_Associations => New_List (
                     L_Exp,
                     R_Exp,
                     Lhs_Discr_Val,
                     Rhs_Discr_Val)));
            end;

         --  Normal case, not an unchecked union

         else
            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (Eq, Loc),
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

            declare
               Comp : Node_Id := First (Component_Items (Clist));

            begin
               while Present (Comp) loop

                  --  One component is sufficent

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

                  --  One component is sufficent

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

                  --  One component within a variant is sufficent

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

         --  If we are doing full validity checking, then expand out array
         --  comparisons to make sure that we check the array elements.

         if Validity_Check_Operands then
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

         --  For composite and floating-point cases, expand equality loop
         --  to make sure of using proper comparisons for tagged types,
         --  and correctly handling the floating-point case.

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
               --  Search for equality operation, checking that the
               --  operands have the same type. Note that we must find
               --  a matching entry, or something is very wrong!

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
            --  user-defined equality. The reason for not simply calling
            --  Find_Prim_Op here is that there may be a user-defined
            --  overloaded equality op that precedes the equality that
            --  we want, so we have to explicitly search (e.g., there
            --  could be an equality with two different parameter types).

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
            --  equality as a standard False.

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
               --  the equality as a standard False.

               Rewrite (N,
                 New_Occurrence_Of (Standard_False, Loc));

            end if;

         --  If a type support function is present (for complex cases), use it

         elsif Present (TSS (Root_Type (Typl), TSS_Composite_Equality)) then
            Build_Equality_Call
              (TSS (Root_Type (Typl), TSS_Composite_Equality));

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
      Etyp   : Entity_Id;

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

      --  Test for case of known right argument

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
                       Right_Opnd => Duplicate_Subexpr_No_Checks (Base)))));

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
      --  negative integer. In such cases we leave the node untouched, setting
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

      --  Signed integer cases, done using either Integer or Long_Long_Integer.
      --  It is not worth having routines for Short_[Short_]Integer, since for
      --  most machines it would not help, and it would generate more code that
      --  might need certification in the HI-E case.

      --  In the integer cases, we have two routines, one for when overflow
      --  checks are required, and one when they are not required, since
      --  there is a real gain in ommitting checks on many machines.

      elsif Rtyp = Base_Type (Standard_Long_Long_Integer)
        or else (Rtyp = Base_Type (Standard_Long_Integer)
                   and then
                     Esize (Standard_Long_Integer) > Esize (Standard_Integer))
        or else (Rtyp = Universal_Integer)
      then
         Etyp := Standard_Long_Long_Integer;

         if Ovflo then
            Rent := RE_Exp_Long_Long_Integer;
         else
            Rent := RE_Exn_Long_Long_Integer;
         end if;

      elsif Is_Signed_Integer_Type (Rtyp) then
         Etyp := Standard_Integer;

         if Ovflo then
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
             Name => New_Reference_To (RTE (Rent), Loc),
             Parameter_Associations => New_List (Base, Exp)));

      --  Otherwise we have to introduce conversions (conversions are also
      --  required in the universal cases, since the runtime routine is
      --  typed using one of the standard types.

      else
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (Rent), Loc),
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
      Typ   : constant Entity_Id  := Etype (N);
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
         Set_Etype             (N, Typ);
         Set_Do_Overflow_Check (N, DOC);
         Set_Do_Division_Check (N, DDC);
         Expand_N_Op_Rem (N);
         Set_Analyzed (N);

      --  Otherwise, normal mod processing

      else
         if Is_Integer_Type (Etype (N)) then
            Apply_Divide_Check (N);
         end if;

         --  Apply optimization x mod 1 = 0. We don't really need that with
         --  gcc, but it is useful with other back ends (e.g. AAMP), and is
         --  certainly harmless.

         if Is_Integer_Type (Etype (N))
           and then Compile_Time_Known_Value (Right)
           and then Expr_Value (Right) = Uint_1
         then
            Rewrite (N, Make_Integer_Literal (Loc, 0));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

         --  Deal with annoying case of largest negative number remainder
         --  minus one. Gigi does not handle this case correctly, because
         --  it generates a divide instruction which may trap in this case.

         --  In fact the check is quite easy, if the right operand is -1,
         --  then the mod value is always 0, and we can just ignore the
         --  left operand completely in this case.

         --  The operand type may be private (e.g. in the expansion of an
         --  an intrinsic operation) so we must use the underlying type to
         --  get the bounds, and convert the literals explicitly.

         LLB :=
           Expr_Value
             (Type_Low_Bound (Base_Type (Underlying_Type (Etype (Left)))));

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
                      Unchecked_Convert_To (Typ,
                        Make_Integer_Literal (Loc, -1))),
                  Unchecked_Convert_To (Typ,
                    Make_Integer_Literal (Loc, Uint_0)),
                  Relocate_Node (N))));

            Set_Analyzed (Next (Next (First (Expressions (N)))));
            Analyze_And_Resolve (N, Typ);
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

      Lp2  : constant Boolean :=
               Nkind (Lop) = N_Op_Expon
                 and then Is_Power_Of_2_For_Shift (Lop);

      Rp2  : constant Boolean :=
               Nkind (Rop) = N_Op_Expon
                 and then Is_Power_Of_2_For_Shift (Rop);

      Ltyp : constant Entity_Id  := Etype (Lop);
      Rtyp : constant Entity_Id  := Etype (Rop);
      Typ  : Entity_Id           := Etype (N);

   begin
      Binary_Op_Validity_Checks (N);

      --  Special optimizations for integer types

      if Is_Integer_Type (Typ) then

         --  N * 0 = 0 * N = 0 for integer types

         if (Compile_Time_Known_Value (Rop)
              and then Expr_Value (Rop) = Uint_0)
           or else
            (Compile_Time_Known_Value (Lop)
              and then Expr_Value (Lop) = Uint_0)
         then
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

      --  Deal with VAX float case

      if Vax_Float (Typ) then
         Expand_Vax_Arith (N);
         return;
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
            Rewrite (N,
              Make_Op_Shift_Left (Loc,
                Left_Opnd  => Lop,
                Right_Opnd =>
                  Convert_To (Standard_Natural, Right_Opnd (Rop))));
            Analyze_And_Resolve (N, Typ);
            return;
         end if;

      --  Same processing for the operands the other way round

      elsif Lp2 then
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

      --  For navigation purposes, the inequality is treated as an implicit
      --  reference to the corresponding equality. Preserve the Comes_From_
      --  source flag so that the proper Xref entry is generated.

      Preserve_Comes_From_Source (Neg, N);
      Preserve_Comes_From_Source (Right_Opnd (Neg), N);
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

      if Nkind (Parent (N)) = N_Assignment_Statement then
         if Safe_In_Place_Array_Op (Name (Parent (N)), N, Empty) then
            Build_Boolean_Array_Proc_Call (Parent (N), Opnd, Empty);
            return;

         --  Special case the negation of a binary operation

         elsif (Nkind (Opnd) = N_Op_And
                 or else Nkind (Opnd) = N_Op_Or
                 or else Nkind (Opnd) = N_Op_Xor)
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
               if N = Op1
                 and then Nkind (Op2) = N_Op_Not
               then
                  --  (not A) op (not B) can be reduced to a single call

                  return;

               elsif N = Op2
                 and then Nkind (Parent (N)) = N_Op_Xor
               then
                  --  A xor (not B) can also be special-cased

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
      Typ : constant Entity_Id  := Etype (N);

      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;

   begin
      Binary_Op_Validity_Checks (N);

      if Is_Integer_Type (Etype (N)) then
         Apply_Divide_Check (N);
      end if;

      --  Apply optimization x rem 1 = 0. We don't really need that with
      --  gcc, but it is useful with other back ends (e.g. AAMP), and is
      --  certainly harmless.

      if Is_Integer_Type (Etype (N))
        and then Compile_Time_Known_Value (Right)
        and then Expr_Value (Right) = Uint_1
      then
         Rewrite (N, Make_Integer_Literal (Loc, 0));
         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  Deal with annoying case of largest negative number remainder
      --  minus one. Gigi does not handle this case correctly, because
      --  it generates a divide instruction which may trap in this case.

      --  In fact the check is quite easy, if the right operand is -1,
      --  then the remainder is always 0, and we can just ignore the
      --  left operand completely in this case.

      Determine_Range (Right, ROK, Rlo, Rhi);
      Determine_Range (Left, LOK, Llo, Lhi);

      --  The operand type may be private (e.g. in the expansion of an
      --  an intrinsic operation) so we must use the underlying type to
      --  get the bounds, and convert the literals explicitly.

      LLB :=
        Expr_Value
          (Type_Low_Bound (Base_Type (Underlying_Type (Etype (Left)))));

      --  Now perform the test, generating code only if needed

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
                   Unchecked_Convert_To (Typ,
                     Make_Integer_Literal (Loc, -1))),

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
      end if;

      --  Check for cases of left argument is True or False

      if Nkind (Left) = N_Identifier then

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
      Ptyp  : Entity_Id           := Underlying_Type (Etype (P));
      Disc  : Entity_Id;
      New_N : Node_Id;
      Dcon  : Elmt_Id;

      function In_Left_Hand_Side (Comp : Node_Id) return Boolean;
      --  Gigi needs a temporary for prefixes that depend on a discriminant,
      --  unless the context of an assignment can provide size information.
      --  Don't we have a general routine that does this???

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

   --  Start of processing for Expand_N_Selected_Component

   begin
      --  Insert explicit dereference if required

      if Is_Access_Type (Ptyp) then
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

         --  Present the discrminant checking function to the backend,
         --  so that it can inline the call to the function.

         Add_Inlined_Body
           (Discriminant_Checking_Func
             (Original_Record_Component (Entity (Selector_Name (N)))));

         --  Now reset the flag and generate the call

         Set_Do_Discriminant_Check (N, False);
         Generate_Discriminant_Check (N);
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
            --  access types (access discriminants get us into trouble!)

            if not Is_Discrete_Type (Etype (N)) then
               null;

            --  Don't do this on the left hand of an assignment statement.
            --  Normally one would think that references like this would
            --  not occur, but they do in generated code, and mean that
            --  we really do want to assign the discriminant!

            elsif Nkind (Par) = N_Assignment_Statement
              and then Name (Par) = N
            then
               null;

            --  Don't do this optimization for the prefix of an attribute
            --  or the operand of an object renaming declaration since these
            --  are contexts where we do not want the value anyway.

            elsif (Nkind (Par) = N_Attribute_Reference
                     and then Prefix (Par) = N)
              or else Is_Renamed_Object (N)
            then
               null;

            --  Don't do this optimization if we are within the code for a
            --  discriminant check, since the whole point of such a check may
            --  be to verify the condition on which the code below depends!

            elsif Is_In_Discriminant_Check (N) then
               null;

            --  Green light to see if we can do the optimization. There is
            --  still one condition that inhibits the optimization below
            --  but now is the time to check the particular discriminant.

            else
               --  Loop through discriminants to find the matching
               --  discriminant constraint to see if we can copy it.

               Disc := First_Discriminant (Ptyp);
               Dcon := First_Elmt (Discriminant_Constraint (Ptyp));
               Discr_Loop : while Present (Dcon) loop

                  --  Check if this is the matching discriminant

                  if Disc = Entity (Selector_Name (N)) then

                     --  Here we have the matching discriminant. Check for
                     --  the case of a discriminant of a component that is
                     --  constrained by an outer discriminant, which cannot
                     --  be optimized away.

                     if
                       Denotes_Discriminant
                        (Node (Dcon), Check_Protected => True)
                     then
                        exit Discr_Loop;

                     --  In the context of a case statement, the expression
                     --  may have the base type of the discriminant, and we
                     --  need to preserve the constraint to avoid spurious
                     --  errors on missing cases.

                     elsif Nkind (Parent (N)) = N_Case_Statement
                       and then Etype (Node (Dcon)) /= Etype (Disc)
                     then
                        Rewrite (N,
                          Make_Qualified_Expression (Loc,
                            Subtype_Mark =>
                              New_Occurrence_Of (Etype (Disc), Loc),
                            Expression   =>
                              New_Copy_Tree (Node (Dcon))));
                        Analyze_And_Resolve (N, Etype (Disc));

                        --  In case that comes out as a static expression,
                        --  reset it (a selected component is never static).

                        Set_Is_Static_Expression (N, False);
                        return;

                     --  Otherwise we can just copy the constraint, but the
                     --  result is certainly not static! In some cases the
                     --  discriminant constraint has been analyzed in the
                     --  context of the original subtype indication, but for
                     --  itypes the constraint might not have been analyzed
                     --  yet, and this must be done now.

                     else
                        Rewrite (N, New_Copy_Tree (Node (Dcon)));
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
   end Expand_N_Selected_Component;

   --------------------
   -- Expand_N_Slice --
   --------------------

   procedure Expand_N_Slice (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Pfx  : constant Node_Id    := Prefix (N);
      Ptp  : Entity_Id           := Etype (Pfx);

      function Is_Procedure_Actual (N : Node_Id) return Boolean;
      --  Check whether the argument is an actual for a procedure call,
      --  in which case the expansion of a bit-packed slice is deferred
      --  until the call itself is expanded. The reason this is required
      --  is that we might have an IN OUT or OUT parameter, and the copy out
      --  is essential, and that copy out would be missed if we created a
      --  temporary here in Expand_N_Slice. Note that we don't bother
      --  to test specifically for an IN OUT or OUT mode parameter, since it
      --  is a bit tricky to do, and it is harmless to defer expansion
      --  in the IN case, since the call processing will still generate the
      --  appropriate copy in operation, which will take care of the slice.

      procedure Make_Temporary;
      --  Create a named variable for the value of the slice, in
      --  cases where the back-end cannot handle it properly, e.g.
      --  when packed types or unaligned slices are involved.

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

            --  If our parent is a type conversion, keep climbing the
            --  tree, since a type conversion can be a procedure actual.
            --  Also keep climbing if parameter association or a qualified
            --  expression, since these are additional cases that do can
            --  appear on procedure actuals.

            elsif Nkind (Par) = N_Type_Conversion
              or else Nkind (Par) = N_Parameter_Association
              or else Nkind (Par) = N_Qualified_Expression
            then
               Par := Parent (Par);

               --  Any other case is not what we are looking for

            else
               return False;
            end if;
         end loop;
      end Is_Procedure_Actual;

      --------------------
      -- Make_Temporary --
      --------------------

      procedure Make_Temporary is
         Decl : Node_Id;
         Ent  : constant Entity_Id :=
                  Make_Defining_Identifier (Loc, New_Internal_Name ('T'));
      begin
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
      end Make_Temporary;

   --  Start of processing for Expand_N_Slice

   begin
      --  Special handling for access types

      if Is_Access_Type (Ptp) then

         Ptp := Designated_Type (Ptp);

         Rewrite (Pfx,
           Make_Explicit_Dereference (Sloc (N),
            Prefix => Relocate_Node (Pfx)));

         Analyze_And_Resolve (Pfx, Ptp);
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

      if not Is_Packed (Typ) then

         --  Apply transformation for actuals of a function call,
         --  where Expand_Actuals is not used.

         if Nkind (Parent (N)) = N_Function_Call
           and then Is_Possibly_Unaligned_Slice (N)
         then
            Make_Temporary;
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
         Make_Temporary;
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

                  if Disc /= First_Stored_Discriminant (Operand_Type) then
                     Disc := First_Stored_Discriminant (Operand_Type);
                  end if;

                  Cons := New_List;
                  while Present (Disc) loop
                     Append_To (Cons,
                       Make_Selected_Component (Loc,
                         Prefix => Duplicate_Subexpr_Move_Checks (Operand),
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

      --  This is necessary when there is a conversion of integer to float
      --  or to fixed-point to ensure that the correct checks are made. It
      --  is not necessary for float to float where it is enough to simply
      --  set the Do_Range_Check flag.

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

         if Is_Entity_Name (Operand)
           and then Range_Checks_Suppressed (Entity (Operand))
         then
            return;
         end if;

         --  Nothing to do if bounds are all static and we can tell that
         --  the expression is within the bounds of the target. Note that
         --  if the operand is of an unconstrained floating-point type,
         --  then we do not trust it to be in range (might be infinite)

         declare
            S_Lo : constant Node_Id   := Type_Low_Bound (Xtyp);
            S_Hi : constant Node_Id   := Type_High_Bound (Xtyp);

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
         Rewrite
           (Subtype_Mark (Conv), New_Occurrence_Of (Btyp, Loc));
         Set_Etype (Conv, Btyp);

         --  Enable overflow except in the case of integer to float
         --  conversions, where it is never required, since we can
         --  never have overflow in this case.

         if not Is_Integer_Type (Etype (Operand)) then
            Enable_Overflow_Check (Conv);
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
         Rewrite (N, Relocate_Node (Operand));
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
                           Left_Opnd  => Duplicate_Subexpr_No_Checks (Operand),
                           Right_Opnd => Make_Null (Loc)),

                       Right_Opnd =>
                         Make_Not_In (Loc,
                           Left_Opnd  =>
                             Make_Explicit_Dereference (Loc,
                               Prefix =>
                                 Duplicate_Subexpr_No_Checks (Operand)),
                           Right_Opnd =>
                             New_Reference_To (Actual_Target_Type, Loc)));

               else
                  Cond :=
                    Make_Not_In (Loc,
                      Left_Opnd  => Duplicate_Subexpr_No_Checks (Operand),
                      Right_Opnd =>
                        New_Reference_To (Actual_Target_Type, Loc));
               end if;

               Insert_Action (N,
                 Make_Raise_Constraint_Error (Loc,
                   Condition => Cond,
                   Reason    => CE_Tag_Check_Failed));

               declare
                  Conv : Node_Id;
               begin
                  Conv :=
                    Make_Unchecked_Type_Conversion (Loc,
                      Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                      Expression => Relocate_Node (Expression (N)));
                  Rewrite (N, Conv);
                  Analyze_And_Resolve (N, Target_Type);
               end;
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

         --  with the Float_Truncate flag set. This is clearly more efficient

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

         if Do_Range_Check (Operand) then
            Rewrite (Operand,
              Make_Type_Conversion (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_Long_Long_Float, Loc),
                Expression =>
                  Relocate_Node (Operand)));

            Set_Etype (Operand, Standard_Long_Long_Float);
            Enable_Range_Check (Operand);
            Set_Do_Range_Check (Expression (Operand), False);
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

         --  Ada 2005 (AI-216): Program_Error is raised when converting from
         --  a derived Unchecked_Union type to an unconstrained non-Unchecked_
         --  Union type if the operand lacks inferable discriminants.

         if Is_Derived_Type (Operand_Type)
           and then Is_Unchecked_Union (Base_Type (Operand_Type))
           and then not Is_Constrained (Target_Type)
           and then not Is_Unchecked_Union (Base_Type (Target_Type))
           and then not Has_Inferable_Discriminants (Operand)
         then
            --  To prevent Gigi from generating illegal code, we make a
            --  Program_Error node, but we give it the target type of the
            --  conversion.

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

      --  No other conversions should be passed to Gigi

      --  Check: are these rules stated in sinfo??? if so, why restate here???

      --  The only remaining step is to generate a range check if we still
      --  have a type conversion at this stage and Do_Range_Check is set.
      --  For now we do this only for conversions of discrete types.

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

               --  Before we do a range check, we have to deal with treating
               --  a fixed-point operand as an integer. The way we do this
               --  is simply to do an unchecked conversion to an appropriate
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
               --  dealing with possible overflow, and generate the check
               --  If Address is either source or target type, suppress
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

               --  If Address is the target type, just set the type
               --  to avoid a spurious type error on the literal when
               --  Address is a visible integer type.

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
      Bodies : List_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Result : Node_Id;
      C      : Entity_Id;

      First_Time : Boolean := True;

      function Suitable_Element (C : Entity_Id) return Entity_Id;
      --  Return the first field to compare beginning with C, skipping the
      --  inherited components.

      ----------------------
      -- Suitable_Element --
      ----------------------

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

      Result := New_Reference_To (Standard_True, Loc);
      C := Suitable_Element (First_Entity (Typ));

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
                   Prefix => New_Lhs,
                   Selector_Name => New_Reference_To (C, Loc)),
               Rhs =>
                 Make_Selected_Component (Loc,
                   Prefix => New_Rhs,
                   Selector_Name => New_Reference_To (C, Loc)),
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

   ------------------------------
   -- Get_Allocator_Final_List --
   ------------------------------

   function Get_Allocator_Final_List
     (N    : Node_Id;
      T    : Entity_Id;
      PtrT : Entity_Id) return Entity_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

      Owner : Entity_Id := PtrT;
      --  The entity whose finalisation list must be used to attach the
      --  allocated object.

   begin
      if Ekind (PtrT) = E_Anonymous_Access_Type then
         if Nkind (Associated_Node_For_Itype (PtrT))
              in N_Subprogram_Specification
         then
            --  If the context is an access parameter, we need to create
            --  a non-anonymous access type in order to have a usable
            --  final list, because there is otherwise no pool to which
            --  the allocated object can belong. We create both the type
            --  and the finalization chain here, because freezing an
            --  internal type does not create such a chain. The Final_Chain
            --  that is thus created is shared by the access parameter.

            Owner := Make_Defining_Identifier (Loc, New_Internal_Name ('J'));
            Insert_Action (N,
              Make_Full_Type_Declaration (Loc,
                Defining_Identifier => Owner,
                Type_Definition =>
                   Make_Access_To_Object_Definition (Loc,
                     Subtype_Indication =>
                       New_Occurrence_Of (T, Loc))));

            Build_Final_List (N, Owner);
            Set_Associated_Final_Chain (PtrT, Associated_Final_Chain (Owner));

         else
            --  Case of an access discriminant, or (Ada 2005) of
            --  an anonymous access component: find the final list
            --  associated with the scope of the type.

            Owner := Scope (PtrT);
         end if;
      end if;

      return Find_Final_List (Owner);
   end Get_Allocator_Final_List;

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
         Sel_Comp : Node_Id := N;

      begin
         --  Move to the left-most prefix by climbing up the tree

         while Present (Parent (Sel_Comp))
           and then Nkind (Parent (Sel_Comp)) = N_Selected_Component
         loop
            Sel_Comp := Parent (Sel_Comp);
         end loop;

         return Ekind (Entity (Prefix (Sel_Comp))) in Formal_Kind;
      end Prefix_Is_Formal_Parameter;

   --  Start of processing for Has_Inferable_Discriminants

   begin
      --  For identifiers and indexed components, it is sufficent to have a
      --  constrained Unchecked_Union nominal subtype.

      if Nkind (N) = N_Identifier
           or else
         Nkind (N) = N_Indexed_Component
      then
         return Is_Unchecked_Union (Base_Type (Etype (N)))
                  and then
                Is_Constrained (Etype (N));

      --  For selected components, the subtype of the selector must be a
      --  constrained Unchecked_Union. If the component is subject to a
      --  per-object constraint, then the enclosing object must have inferable
      --  discriminants.

      elsif Nkind (N) = N_Selected_Component then
         if Has_Per_Object_Constraint (Entity (Selector_Name (N))) then

            --  A small hack. If we have a per-object constrained selected
            --  component of a formal parameter, return True since we do not
            --  know the actual parameter association yet.

            if Prefix_Is_Formal_Parameter (N) then
               return True;
            end if;

            --  Otherwise, check the enclosing object and the selector

            return Has_Inferable_Discriminants (Prefix (N))
                     and then
                   Has_Inferable_Discriminants (Selector_Name (N));
         end if;

         --  The call to Has_Inferable_Discriminants will determine whether
         --  the selector has a constrained Unchecked_Union nominal type.

         return Has_Inferable_Discriminants (Selector_Name (N));

      --  A qualified expression has inferable discriminants if its subtype
      --  mark is a constrained Unchecked_Union subtype.

      elsif Nkind (N) = N_Qualified_Expression then
         return Is_Unchecked_Union (Subtype_Mark (N))
                  and then
                Is_Constrained (Subtype_Mark (N));

      end if;

      return False;
   end Has_Inferable_Discriminants;

   -------------------------------
   -- Insert_Dereference_Action --
   -------------------------------

   procedure Insert_Dereference_Action (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Pool : constant Entity_Id  := Associated_Storage_Pool (Typ);
      Pnod : constant Node_Id    := Parent (N);

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

   --  Start of processing for Insert_Dereference_Action

   begin
      pragma Assert (Nkind (Pnod) = N_Explicit_Dereference);

      if not (Is_Checked_Storage_Pool (Pool)
              and then Comes_From_Source (Original_Node (Pnod)))
      then
         return;
      end if;

      Insert_Action (N,
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (
            Find_Prim_Op (Etype (Pool), Name_Dereference), Loc),

          Parameter_Associations => New_List (

            --  Pool

             New_Reference_To (Pool, Loc),

            --  Storage_Address. We use the attribute Pool_Address,
            --  which uses the pointer itself to find the address of
            --  the object, and which handles unconstrained arrays
            --  properly by computing the address of the template.
            --  i.e. the correct address of the corresponding allocation.

             Make_Attribute_Reference (Loc,
               Prefix         => Duplicate_Subexpr_Move_Checks (N),
               Attribute_Name => Name_Pool_Address),

            --  Size_In_Storage_Elements

             Make_Op_Divide (Loc,
               Left_Opnd  =>
                Make_Attribute_Reference (Loc,
                  Prefix         =>
                    Make_Explicit_Dereference (Loc,
                      Duplicate_Subexpr_Move_Checks (N)),
                  Attribute_Name => Name_Size),
               Right_Opnd =>
                 Make_Integer_Literal (Loc, System_Storage_Unit)),

            --  Alignment

             Make_Attribute_Reference (Loc,
               Prefix         =>
                 Make_Explicit_Dereference (Loc,
                   Duplicate_Subexpr_Move_Checks (N)),
               Attribute_Name => Name_Alignment))));

   exception
      when RE_Not_Available =>
         return;
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

         elsif Nkind (Op) = N_Indexed_Component
           or else Nkind (Op) = N_Selected_Component
         then
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

      --  Start of processing for Is_Safe_In_Place_Array_Op

   begin
      --  We skip this processing if the component size is not the
      --  same as a system storage unit (since at least for NOT
      --  this would cause problems).

      if Component_Size (Etype (Lhs)) /= System_Storage_Unit then
         return False;

      --  Cannot do in place stuff on Java_VM since cannot pass addresses

      elsif Java_VM then
         return False;

      --  Cannot do in place stuff if non-standard Boolean representation

      elsif Has_Non_Standard_Rep (Component_Type (Etype (Lhs))) then
         return False;

      elsif not Is_Unaliased (Lhs) then
         return False;
      else
         Target := Entity (Lhs);

         return
           Is_Safe_Operand (Op1)
             and then Is_Safe_Operand (Op2);
      end if;
   end Safe_In_Place_Array_Op;

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
          Selector_Name =>
            New_Reference_To (First_Tag_Component (Left_Type), Loc));

      if Is_Class_Wide_Type (Right_Type) then
         return
           Make_DT_Access_Action (Left_Type,
             Action => CW_Membership,
             Args   => New_List (
               Obj_Tag,
               New_Reference_To
                 (Node (First_Elmt
                          (Access_Disp_Table (Root_Type (Right_Type)))),
                  Loc)));
      else
         return
           Make_Op_Eq (Loc,
           Left_Opnd  => Obj_Tag,
           Right_Opnd =>
             New_Reference_To
               (Node (First_Elmt (Access_Disp_Table (Right_Type))), Loc));
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
