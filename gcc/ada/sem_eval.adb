------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E V A L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Eval_Fat; use Eval_Fat;
with Exp_Util; use Exp_Util;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;

package body Sem_Eval is

   -----------------------------------------
   -- Handling of Compile Time Evaluation --
   -----------------------------------------

   --  The compile time evaluation of expressions is distributed over several
   --  Eval_xxx procedures. These procedures are called immediatedly after
   --  a subexpression is resolved and is therefore accomplished in a bottom
   --  up fashion. The flags are synthesized using the following approach.

   --    Is_Static_Expression is determined by following the detailed rules
   --    in RM 4.9(4-14). This involves testing the Is_Static_Expression
   --    flag of the operands in many cases.

   --    Raises_Constraint_Error is set if any of the operands have the flag
   --    set or if an attempt to compute the value of the current expression
   --    results in detection of a runtime constraint error.

   --  As described in the spec, the requirement is that Is_Static_Expression
   --  be accurately set, and in addition for nodes for which this flag is set,
   --  Raises_Constraint_Error must also be set. Furthermore a node which has
   --  Is_Static_Expression set, and Raises_Constraint_Error clear, then the
   --  requirement is that the expression value must be precomputed, and the
   --  node is either a literal, or the name of a constant entity whose value
   --  is a static expression.

   --  The general approach is as follows. First compute Is_Static_Expression.
   --  If the node is not static, then the flag is left off in the node and
   --  we are all done. Otherwise for a static node, we test if any of the
   --  operands will raise constraint error, and if so, propagate the flag
   --  Raises_Constraint_Error to the result node and we are done (since the
   --  error was already posted at a lower level).

   --  For the case of a static node whose operands do not raise constraint
   --  error, we attempt to evaluate the node. If this evaluation succeeds,
   --  then the node is replaced by the result of this computation. If the
   --  evaluation raises constraint error, then we rewrite the node with
   --  Apply_Compile_Time_Constraint_Error to raise the exception and also
   --  to post appropriate error messages.

   ----------------
   -- Local Data --
   ----------------

   type Bits is array (Nat range <>) of Boolean;
   --  Used to convert unsigned (modular) values for folding logical ops

   --  The following definitions are used to maintain a cache of nodes that
   --  have compile time known values. The cache is maintained only for
   --  discrete types (the most common case), and is populated by calls to
   --  Compile_Time_Known_Value and Expr_Value, but only used by Expr_Value
   --  since it is possible for the status to change (in particular it is
   --  possible for a node to get replaced by a constraint error node).

   CV_Bits : constant := 5;
   --  Number of low order bits of Node_Id value used to reference entries
   --  in the cache table.

   CV_Cache_Size : constant Nat := 2 ** CV_Bits;
   --  Size of cache for compile time values

   subtype CV_Range is Nat range 0 .. CV_Cache_Size;

   type CV_Entry is record
      N : Node_Id;
      V : Uint;
   end record;

   type CV_Cache_Array is array (CV_Range) of CV_Entry;

   CV_Cache : CV_Cache_Array := (others => (Node_High_Bound, Uint_0));
   --  This is the actual cache, with entries consisting of node/value pairs,
   --  and the impossible value Node_High_Bound used for unset entries.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function From_Bits (B : Bits; T : Entity_Id) return Uint;
   --  Converts a bit string of length B'Length to a Uint value to be used
   --  for a target of type T, which is a modular type. This procedure
   --  includes the necessary reduction by the modulus in the case of a
   --  non-binary modulus (for a binary modulus, the bit string is the
   --  right length any way so all is well).

   function Get_String_Val (N : Node_Id) return Node_Id;
   --  Given a tree node for a folded string or character value, returns
   --  the corresponding string literal or character literal (one of the
   --  two must be available, or the operand would not have been marked
   --  as foldable in the earlier analysis of the operation).

   function OK_Bits (N : Node_Id; Bits : Uint) return Boolean;
   --  Bits represents the number of bits in an integer value to be computed
   --  (but the value has not been computed yet). If this value in Bits is
   --  reasonable, a result of True is returned, with the implication that
   --  the caller should go ahead and complete the calculation. If the value
   --  in Bits is unreasonably large, then an error is posted on node N, and
   --  False is returned (and the caller skips the proposed calculation).

   procedure Out_Of_Range (N : Node_Id);
   --  This procedure is called if it is determined that node N, which
   --  appears in a non-static context, is a compile time known value
   --  which is outside its range, i.e. the range of Etype. This is used
   --  in contexts where this is an illegality if N is static, and should
   --  generate a warning otherwise.

   procedure Rewrite_In_Raise_CE (N : Node_Id; Exp : Node_Id);
   --  N and Exp are nodes representing an expression, Exp is known
   --  to raise CE. N is rewritten in term of Exp in the optimal way.

   function String_Type_Len (Stype : Entity_Id) return Uint;
   --  Given a string type, determines the length of the index type, or,
   --  if this index type is non-static, the length of the base type of
   --  this index type. Note that if the string type is itself static,
   --  then the index type is static, so the second case applies only
   --  if the string type passed is non-static.

   function Test (Cond : Boolean) return Uint;
   pragma Inline (Test);
   --  This function simply returns the appropriate Boolean'Pos value
   --  corresponding to the value of Cond as a universal integer. It is
   --  used for producing the result of the static evaluation of the
   --  logical operators

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean);
   --  Tests to see if expression N whose single operand is Op1 is foldable,
   --  i.e. the operand value is known at compile time. If the operation is
   --  foldable, then Fold is True on return, and Stat indicates whether
   --  the result is static (i.e. both operands were static). Note that it
   --  is quite possible for Fold to be True, and Stat to be False, since
   --  there are cases in which we know the value of an operand even though
   --  it is not technically static (e.g. the static lower bound of a range
   --  whose upper bound is non-static).
   --
   --  If Stat is set False on return, then Expression_Is_Foldable makes a
   --  call to Check_Non_Static_Context on the operand. If Fold is False on
   --  return, then all processing is complete, and the caller should
   --  return, since there is nothing else to do.

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Op2  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean);
   --  Same processing, except applies to an expression N with two operands
   --  Op1 and Op2.

   procedure To_Bits (U : Uint; B : out Bits);
   --  Converts a Uint value to a bit string of length B'Length

   ------------------------------
   -- Check_Non_Static_Context --
   ------------------------------

   procedure Check_Non_Static_Context (N : Node_Id) is
      T         : constant Entity_Id := Etype (N);
      Checks_On : constant Boolean   :=
                    not Index_Checks_Suppressed (T)
                      and not Range_Checks_Suppressed (T);

   begin
      --  Ignore cases of non-scalar types or error types

      if T = Any_Type or else not Is_Scalar_Type (T) then
         return;
      end if;

      --  At this stage we have a scalar type. If we have an expression
      --  that raises CE, then we already issued a warning or error msg
      --  so there is nothing more to be done in this routine.

      if Raises_Constraint_Error (N) then
         return;
      end if;

      --  Now we have a scalar type which is not marked as raising a
      --  constraint error exception. The main purpose of this routine
      --  is to deal with static expressions appearing in a non-static
      --  context. That means that if we do not have a static expression
      --  then there is not much to do. The one case that we deal with
      --  here is that if we have a floating-point value that is out of
      --  range, then we post a warning that an infinity will result.

      if not Is_Static_Expression (N) then
         if Is_Floating_Point_Type (T)
           and then Is_Out_Of_Range (N, Base_Type (T))
         then
            Error_Msg_N
              ("?float value out of range, infinity will be generated", N);
         end if;

         return;
      end if;

      --  Here we have the case of outer level static expression of
      --  scalar type, where the processing of this procedure is needed.

      --  For real types, this is where we convert the value to a machine
      --  number (see RM 4.9(38)). Also see ACVC test C490001. We should
      --  only need to do this if the parent is a constant declaration,
      --  since in other cases, gigi should do the necessary conversion
      --  correctly, but experimentation shows that this is not the case
      --  on all machines, in particular if we do not convert all literals
      --  to machine values in non-static contexts, then ACVC test C490001
      --  fails on Sparc/Solaris and SGI/Irix.

      if Nkind (N) = N_Real_Literal
        and then not Is_Machine_Number (N)
        and then not Is_Generic_Type (Etype (N))
        and then Etype (N) /= Universal_Real
      then
         --  Check that value is in bounds before converting to machine
         --  number, so as not to lose case where value overflows in the
         --  least significant bit or less. See B490001.

         if Is_Out_Of_Range (N, Base_Type (T)) then
            Out_Of_Range (N);
            return;
         end if;

         --  Note: we have to copy the node, to avoid problems with conformance
         --  of very similar numbers (see ACVC tests B4A010C and B63103A).

         Rewrite (N, New_Copy (N));

         if not Is_Floating_Point_Type (T) then
            Set_Realval
              (N, Corresponding_Integer_Value (N) * Small_Value (T));

         elsif not UR_Is_Zero (Realval (N)) then

            --  Note: even though RM 4.9(38) specifies biased rounding,
            --  this has been modified by AI-100 in order to prevent
            --  confusing differences in rounding between static and
            --  non-static expressions. AI-100 specifies that the effect
            --  of such rounding is implementation dependent, and in GNAT
            --  we round to nearest even to match the run-time behavior.

            Set_Realval
              (N, Machine (Base_Type (T), Realval (N), Round_Even, N));
         end if;

         Set_Is_Machine_Number (N);
      end if;

      --  Check for out of range universal integer. This is a non-static
      --  context, so the integer value must be in range of the runtime
      --  representation of universal integers.

      --  We do this only within an expression, because that is the only
      --  case in which non-static universal integer values can occur, and
      --  furthermore, Check_Non_Static_Context is currently (incorrectly???)
      --  called in contexts like the expression of a number declaration where
      --  we certainly want to allow out of range values.

      if Etype (N) = Universal_Integer
        and then Nkind (N) = N_Integer_Literal
        and then Nkind (Parent (N)) in N_Subexpr
        and then
          (Intval (N) < Expr_Value (Type_Low_Bound (Universal_Integer))
            or else
           Intval (N) > Expr_Value (Type_High_Bound (Universal_Integer)))
      then
         Apply_Compile_Time_Constraint_Error
           (N, "non-static universal integer value out of range?",
            CE_Range_Check_Failed);

      --  Check out of range of base type

      elsif Is_Out_Of_Range (N, Base_Type (T)) then
         Out_Of_Range (N);

      --  Give warning if outside subtype (where one or both of the
      --  bounds of the subtype is static). This warning is omitted
      --  if the expression appears in a range that could be null
      --  (warnings are handled elsewhere for this case).

      elsif T /= Base_Type (T)
        and then Nkind (Parent (N)) /= N_Range
      then
         if Is_In_Range (N, T) then
            null;

         elsif Is_Out_Of_Range (N, T) then
            Apply_Compile_Time_Constraint_Error
              (N, "value not in range of}?", CE_Range_Check_Failed);

         elsif Checks_On then
            Enable_Range_Check (N);

         else
            Set_Do_Range_Check (N, False);
         end if;
      end if;
   end Check_Non_Static_Context;

   ---------------------------------
   -- Check_String_Literal_Length --
   ---------------------------------

   procedure Check_String_Literal_Length (N : Node_Id; Ttype : Entity_Id) is
   begin
      if not Raises_Constraint_Error (N)
        and then Is_Constrained (Ttype)
      then
         if
           UI_From_Int (String_Length (Strval (N))) /= String_Type_Len (Ttype)
         then
            Apply_Compile_Time_Constraint_Error
              (N, "string length wrong for}?",
               CE_Length_Check_Failed,
               Ent => Ttype,
               Typ => Ttype);
         end if;
      end if;
   end Check_String_Literal_Length;

   --------------------------
   -- Compile_Time_Compare --
   --------------------------

   function Compile_Time_Compare
     (L, R : Node_Id;
      Rec  : Boolean := False)
      return Compare_Result
   is
      Ltyp : constant Entity_Id := Etype (L);
      Rtyp : constant Entity_Id := Etype (R);

      procedure Compare_Decompose
        (N : Node_Id;
         R : out Node_Id;
         V : out Uint);
      --  This procedure decomposes the node N into an expression node
      --  and a signed offset, so that the value of N is equal to the
      --  value of R plus the value V (which may be negative). If no
      --  such decomposition is possible, then on return R is a copy
      --  of N, and V is set to zero.

      function Compare_Fixup (N : Node_Id) return Node_Id;
      --  This function deals with replacing 'Last and 'First references
      --  with their corresponding type bounds, which we then can compare.
      --  The argument is the original node, the result is the identity,
      --  unless we have a 'Last/'First reference in which case the value
      --  returned is the appropriate type bound.

      function Is_Same_Value (L, R : Node_Id) return Boolean;
      --  Returns True iff L and R represent expressions that definitely
      --  have identical (but not necessarily compile time known) values
      --  Indeed the caller is expected to have already dealt with the
      --  cases of compile time known values, so these are not tested here.

      -----------------------
      -- Compare_Decompose --
      -----------------------

      procedure Compare_Decompose
        (N : Node_Id;
         R : out Node_Id;
         V : out Uint)
      is
      begin
         if Nkind (N) = N_Op_Add
           and then Nkind (Right_Opnd (N)) = N_Integer_Literal
         then
            R := Left_Opnd (N);
            V := Intval (Right_Opnd (N));
            return;

         elsif Nkind (N) = N_Op_Subtract
           and then Nkind (Right_Opnd (N)) = N_Integer_Literal
         then
            R := Left_Opnd (N);
            V := UI_Negate (Intval (Right_Opnd (N)));
            return;

         elsif Nkind (N) = N_Attribute_Reference  then

            if Attribute_Name (N) = Name_Succ then
               R := First (Expressions (N));
               V := Uint_1;
               return;

            elsif Attribute_Name (N) = Name_Pred then
               R := First (Expressions (N));
               V := Uint_Minus_1;
               return;
            end if;
         end if;

         R := N;
         V := Uint_0;
      end Compare_Decompose;

      -------------------
      -- Compare_Fixup --
      -------------------

      function Compare_Fixup (N : Node_Id) return Node_Id is
         Indx : Node_Id;
         Xtyp : Entity_Id;
         Subs : Nat;

      begin
         if Nkind (N) = N_Attribute_Reference
           and then (Attribute_Name (N) = Name_First
                       or else
                     Attribute_Name (N) = Name_Last)
         then
            Xtyp := Etype (Prefix (N));

            --  If we have no type, then just abandon the attempt to do
            --  a fixup, this is probably the result of some other error.

            if No (Xtyp) then
               return N;
            end if;

            --  Dereference an access type

            if Is_Access_Type (Xtyp) then
               Xtyp := Designated_Type (Xtyp);
            end if;

            --  If we don't have an array type at this stage, something
            --  is peculiar, e.g. another error, and we abandon the attempt
            --  at a fixup.

            if not Is_Array_Type (Xtyp) then
               return N;
            end if;

            --  Ignore unconstrained array, since bounds are not meaningful

            if not Is_Constrained (Xtyp) then
               return N;
            end if;

            if Ekind (Xtyp) = E_String_Literal_Subtype then
               if Attribute_Name (N) = Name_First then
                  return String_Literal_Low_Bound (Xtyp);

               else         -- Attribute_Name (N) = Name_Last
                  return Make_Integer_Literal (Sloc (N),
                    Intval => Intval (String_Literal_Low_Bound (Xtyp))
                       + String_Literal_Length (Xtyp));
               end if;
            end if;

            --  Find correct index type

            Indx := First_Index (Xtyp);

            if Present (Expressions (N)) then
               Subs := UI_To_Int (Expr_Value (First (Expressions (N))));

               for J in 2 .. Subs loop
                  Indx := Next_Index (Indx);
               end loop;
            end if;

            Xtyp := Etype (Indx);

            if Attribute_Name (N) = Name_First then
               return Type_Low_Bound (Xtyp);

            else -- Attribute_Name (N) = Name_Last
               return Type_High_Bound (Xtyp);
            end if;
         end if;

         return N;
      end Compare_Fixup;

      -------------------
      -- Is_Same_Value --
      -------------------

      function Is_Same_Value (L, R : Node_Id) return Boolean is
         Lf : constant Node_Id := Compare_Fixup (L);
         Rf : constant Node_Id := Compare_Fixup (R);

         function Is_Same_Subscript (L, R : List_Id) return Boolean;
         --  L, R are the Expressions values from two attribute nodes
         --  for First or Last attributes. Either may be set to No_List
         --  if no expressions are present (indicating subscript 1).
         --  The result is True if both expressions represent the same
         --  subscript (note that one case is where one subscript is
         --  missing and the other is explicitly set to 1).

         -----------------------
         -- Is_Same_Subscript --
         -----------------------

         function Is_Same_Subscript (L, R : List_Id) return Boolean is
         begin
            if L = No_List then
               if R = No_List then
                  return True;
               else
                  return Expr_Value (First (R)) = Uint_1;
               end if;

            else
               if R = No_List then
                  return Expr_Value (First (L)) = Uint_1;
               else
                  return Expr_Value (First (L)) = Expr_Value (First (R));
               end if;
            end if;
         end Is_Same_Subscript;

      --  Start of processing for Is_Same_Value

      begin
         --  Values are the same if they are the same identifier and the
         --  identifier refers to a constant object (E_Constant). This
         --  does not however apply to Float types, since we may have two
         --  NaN values and they should never compare equal.

         if Nkind (Lf) = N_Identifier and then Nkind (Rf) = N_Identifier
           and then Entity (Lf) = Entity (Rf)
           and then not Is_Floating_Point_Type (Etype (L))
           and then (Ekind (Entity (Lf)) = E_Constant     or else
                     Ekind (Entity (Lf)) = E_In_Parameter or else
                     Ekind (Entity (Lf)) = E_Loop_Parameter)
         then
            return True;

         --  Or if they are compile time known and identical

         elsif Compile_Time_Known_Value (Lf)
                 and then
               Compile_Time_Known_Value (Rf)
           and then Expr_Value (Lf) = Expr_Value (Rf)
         then
            return True;

         --  Or if they are both 'First or 'Last values applying to the
         --  same entity (first and last don't change even if value does)

         elsif Nkind (Lf) = N_Attribute_Reference
                 and then
               Nkind (Rf) = N_Attribute_Reference
           and then Attribute_Name (Lf) = Attribute_Name (Rf)
           and then (Attribute_Name (Lf) = Name_First
                       or else
                     Attribute_Name (Lf) = Name_Last)
           and then Is_Entity_Name (Prefix (Lf))
           and then Is_Entity_Name (Prefix (Rf))
           and then Entity (Prefix (Lf)) = Entity (Prefix (Rf))
           and then Is_Same_Subscript (Expressions (Lf), Expressions (Rf))
         then
            return True;

         --  All other cases, we can't tell

         else
            return False;
         end if;
      end Is_Same_Value;

   --  Start of processing for Compile_Time_Compare

   begin
      --  If either operand could raise constraint error, then we cannot
      --  know the result at compile time (since CE may be raised!)

      if not (Cannot_Raise_Constraint_Error (L)
                and then
              Cannot_Raise_Constraint_Error (R))
      then
         return Unknown;
      end if;

      --  Identical operands are most certainly equal

      if L = R then
         return EQ;

      --  If expressions have no types, then do not attempt to determine
      --  if they are the same, since something funny is going on. One
      --  case in which this happens is during generic template analysis,
      --  when bounds are not fully analyzed.

      elsif No (Ltyp) or else No (Rtyp) then
         return Unknown;

      --  We only attempt compile time analysis for scalar values, and
      --  not for packed arrays represented as modular types, where the
      --  semantics of comparison is quite different.

      elsif not Is_Scalar_Type (Ltyp)
        or else Is_Packed_Array_Type (Ltyp)
      then
         return Unknown;

      --  Case where comparison involves two compile time known values

      elsif Compile_Time_Known_Value (L)
        and then Compile_Time_Known_Value (R)
      then
         --  For the floating-point case, we have to be a little careful, since
         --  at compile time we are dealing with universal exact values, but at
         --  runtime, these will be in non-exact target form. That's why the
         --  returned results are LE and GE below instead of LT and GT.

         if Is_Floating_Point_Type (Ltyp)
              or else
            Is_Floating_Point_Type (Rtyp)
         then
            declare
               Lo : constant Ureal := Expr_Value_R (L);
               Hi : constant Ureal := Expr_Value_R (R);

            begin
               if Lo < Hi then
                  return LE;
               elsif Lo = Hi then
                  return EQ;
               else
                  return GE;
               end if;
            end;

         --  For the integer case we know exactly (note that this includes the
         --  fixed-point case, where we know the run time integer values now)

         else
            declare
               Lo : constant Uint := Expr_Value (L);
               Hi : constant Uint := Expr_Value (R);

            begin
               if Lo < Hi then
                  return LT;
               elsif Lo = Hi then
                  return EQ;
               else
                  return GT;
               end if;
            end;
         end if;

      --  Cases where at least one operand is not known at compile time

      else
         --  Here is where we check for comparisons against maximum bounds of
         --  types, where we know that no value can be outside the bounds of
         --  the subtype. Note that this routine is allowed to assume that all
         --  expressions are within their subtype bounds. Callers wishing to
         --  deal with possibly invalid values must in any case take special
         --  steps (e.g. conversions to larger types) to avoid this kind of
         --  optimization, which is always considered to be valid. We do not
         --  attempt this optimization with generic types, since the type
         --  bounds may not be meaningful in this case.

         --  We are in danger of an infinite recursion here. It does not seem
         --  useful to go more than one level deep, so the parameter Rec is
         --  used to protect ourselves against this infinite recursion.

         if not Rec
           and then Is_Discrete_Type (Ltyp)
           and then Is_Discrete_Type (Rtyp)
           and then not Is_Generic_Type (Ltyp)
           and then not Is_Generic_Type (Rtyp)
         then
            --  See if we can get a decisive check against one operand and
            --  a bound of the other operand (four possible tests here).

            case Compile_Time_Compare (L, Type_Low_Bound (Rtyp), True) is
               when LT => return LT;
               when LE => return LE;
               when EQ => return LE;
               when others => null;
            end case;

            case Compile_Time_Compare (L, Type_High_Bound (Rtyp), True) is
               when GT => return GT;
               when GE => return GE;
               when EQ => return GE;
               when others => null;
            end case;

            case Compile_Time_Compare (Type_Low_Bound (Ltyp), R, True) is
               when GT => return GT;
               when GE => return GE;
               when EQ => return GE;
               when others => null;
            end case;

            case Compile_Time_Compare (Type_High_Bound (Ltyp), R, True) is
               when LT => return LT;
               when LE => return LE;
               when EQ => return LE;
               when others => null;
            end case;
         end if;

         --  Next attempt is to decompose the expressions to extract
         --  a constant offset resulting from the use of any of the forms:

         --     expr + literal
         --     expr - literal
         --     typ'Succ (expr)
         --     typ'Pred (expr)

         --  Then we see if the two expressions are the same value, and if so
         --  the result is obtained by comparing the offsets.

         declare
            Lnode : Node_Id;
            Loffs : Uint;
            Rnode : Node_Id;
            Roffs : Uint;

         begin
            Compare_Decompose (L, Lnode, Loffs);
            Compare_Decompose (R, Rnode, Roffs);

            if Is_Same_Value (Lnode, Rnode) then
               if Loffs = Roffs then
                  return EQ;

               elsif Loffs < Roffs then
                  return LT;

               else
                  return GT;
               end if;

            --  If the expressions are different, we cannot say at compile
            --  time how they compare, so we return the Unknown indication.

            else
               return Unknown;
            end if;
         end;
      end if;
   end Compile_Time_Compare;

   ------------------------------
   -- Compile_Time_Known_Value --
   ------------------------------

   function Compile_Time_Known_Value (Op : Node_Id) return Boolean is
      K      : constant Node_Kind := Nkind (Op);
      CV_Ent : CV_Entry renames CV_Cache (Nat (Op) mod CV_Cache_Size);

   begin
      --  Never known at compile time if bad type or raises constraint error
      --  or empty (latter case occurs only as a result of a previous error)

      if No (Op)
        or else Op = Error
        or else Etype (Op) = Any_Type
        or else Raises_Constraint_Error (Op)
      then
         return False;
      end if;

      --  If this is not a static expression and we are in configurable run
      --  time mode, then we consider it not known at compile time. This
      --  avoids anomalies where whether something is permitted with a given
      --  configurable run-time library depends on how good the compiler is
      --  at optimizing and knowing that things are constant when they
      --  are non-static.

      if Configurable_Run_Time_Mode and then not Is_Static_Expression (Op) then
         return False;
      end if;

      --  If we have an entity name, then see if it is the name of a constant
      --  and if so, test the corresponding constant value, or the name of
      --  an enumeration literal, which is always a constant.

      if Present (Etype (Op)) and then Is_Entity_Name (Op) then
         declare
            E : constant Entity_Id := Entity (Op);
            V : Node_Id;

         begin
            --  Never known at compile time if it is a packed array value.
            --  We might want to try to evaluate these at compile time one
            --  day, but we do not make that attempt now.

            if Is_Packed_Array_Type (Etype (Op)) then
               return False;
            end if;

            if Ekind (E) = E_Enumeration_Literal then
               return True;

            elsif Ekind (E) = E_Constant then
               V := Constant_Value (E);
               return Present (V) and then Compile_Time_Known_Value (V);
            end if;
         end;

      --  We have a value, see if it is compile time known

      else
         --  Integer literals are worth storing in the cache

         if K = N_Integer_Literal then
            CV_Ent.N := Op;
            CV_Ent.V := Intval (Op);
            return True;

         --  Other literals and NULL are known at compile time

         elsif
            K = N_Character_Literal
              or else
            K = N_Real_Literal
              or else
            K = N_String_Literal
              or else
            K = N_Null
         then
            return True;

         --  Any reference to Null_Parameter is known at compile time. No
         --  other attribute references (that have not already been folded)
         --  are known at compile time.

         elsif K = N_Attribute_Reference then
            return Attribute_Name (Op) = Name_Null_Parameter;
         end if;
      end if;

      --  If we fall through, not known at compile time

      return False;

   --  If we get an exception while trying to do this test, then some error
   --  has occurred, and we simply say that the value is not known after all

   exception
      when others =>
         return False;
   end Compile_Time_Known_Value;

   --------------------------------------
   -- Compile_Time_Known_Value_Or_Aggr --
   --------------------------------------

   function Compile_Time_Known_Value_Or_Aggr (Op : Node_Id) return Boolean is
   begin
      --  If we have an entity name, then see if it is the name of a constant
      --  and if so, test the corresponding constant value, or the name of
      --  an enumeration literal, which is always a constant.

      if Is_Entity_Name (Op) then
         declare
            E : constant Entity_Id := Entity (Op);
            V : Node_Id;

         begin
            if Ekind (E) = E_Enumeration_Literal then
               return True;

            elsif Ekind (E) /= E_Constant then
               return False;

            else
               V := Constant_Value (E);
               return Present (V)
                 and then Compile_Time_Known_Value_Or_Aggr (V);
            end if;
         end;

      --  We have a value, see if it is compile time known

      else
         if Compile_Time_Known_Value (Op) then
            return True;

         elsif Nkind (Op) = N_Aggregate then

            if Present (Expressions (Op)) then
               declare
                  Expr : Node_Id;

               begin
                  Expr := First (Expressions (Op));
                  while Present (Expr) loop
                     if not Compile_Time_Known_Value_Or_Aggr (Expr) then
                        return False;
                     end if;

                     Next (Expr);
                  end loop;
               end;
            end if;

            if Present (Component_Associations (Op)) then
               declare
                  Cass : Node_Id;

               begin
                  Cass := First (Component_Associations (Op));
                  while Present (Cass) loop
                     if not
                       Compile_Time_Known_Value_Or_Aggr (Expression (Cass))
                     then
                        return False;
                     end if;

                     Next (Cass);
                  end loop;
               end;
            end if;

            return True;

         --  All other types of values are not known at compile time

         else
            return False;
         end if;

      end if;
   end Compile_Time_Known_Value_Or_Aggr;

   -----------------
   -- Eval_Actual --
   -----------------

   --  This is only called for actuals of functions that are not predefined
   --  operators (which have already been rewritten as operators at this
   --  stage), so the call can never be folded, and all that needs doing for
   --  the actual is to do the check for a non-static context.

   procedure Eval_Actual (N : Node_Id) is
   begin
      Check_Non_Static_Context (N);
   end Eval_Actual;

   --------------------
   -- Eval_Allocator --
   --------------------

   --  Allocators are never static, so all we have to do is to do the
   --  check for a non-static context if an expression is present.

   procedure Eval_Allocator (N : Node_Id) is
      Expr : constant Node_Id := Expression (N);

   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Check_Non_Static_Context (Expression (Expr));
      end if;
   end Eval_Allocator;

   ------------------------
   -- Eval_Arithmetic_Op --
   ------------------------

   --  Arithmetic operations are static functions, so the result is static
   --  if both operands are static (RM 4.9(7), 4.9(20)).

   procedure Eval_Arithmetic_Op (N : Node_Id) is
      Left  : constant Node_Id   := Left_Opnd (N);
      Right : constant Node_Id   := Right_Opnd (N);
      Ltype : constant Entity_Id := Etype (Left);
      Rtype : constant Entity_Id := Etype (Right);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold for cases where both operands are of integer type

      if Is_Integer_Type (Ltype) and then Is_Integer_Type (Rtype) then
         declare
            Left_Int  : constant Uint := Expr_Value (Left);
            Right_Int : constant Uint := Expr_Value (Right);
            Result    : Uint;

         begin
            case Nkind (N) is

               when N_Op_Add =>
                  Result := Left_Int + Right_Int;

               when N_Op_Subtract =>
                  Result := Left_Int - Right_Int;

               when N_Op_Multiply =>
                  if OK_Bits
                       (N, UI_From_Int
                             (Num_Bits (Left_Int) + Num_Bits (Right_Int)))
                  then
                     Result := Left_Int * Right_Int;
                  else
                     Result := Left_Int;
                  end if;

               when N_Op_Divide =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "division by zero",
                        CE_Divide_By_Zero,
                        Warn => not Stat);
                     return;

                  else
                     Result := Left_Int / Right_Int;
                  end if;

               when N_Op_Mod =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "mod with zero divisor",
                        CE_Divide_By_Zero,
                        Warn => not Stat);
                     return;
                  else
                     Result := Left_Int mod Right_Int;
                  end if;

               when N_Op_Rem =>

                  --  The exception Constraint_Error is raised by integer
                  --  division, rem and mod if the right operand is zero.

                  if Right_Int = 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "rem with zero divisor",
                        CE_Divide_By_Zero,
                        Warn => not Stat);
                     return;

                  else
                     Result := Left_Int rem Right_Int;
                  end if;

               when others =>
                  raise Program_Error;
            end case;

            --  Adjust the result by the modulus if the type is a modular type

            if Is_Modular_Integer_Type (Ltype) then
               Result := Result mod Modulus (Ltype);

               --  For a signed integer type, check non-static overflow

            elsif (not Stat) and then Is_Signed_Integer_Type (Ltype) then
               declare
                  BT : constant Entity_Id := Base_Type (Ltype);
                  Lo : constant Uint := Expr_Value (Type_Low_Bound (BT));
                  Hi : constant Uint := Expr_Value (Type_High_Bound (BT));
               begin
                  if Result < Lo or else Result > Hi then
                     Apply_Compile_Time_Constraint_Error
                       (N, "value not in range of }?",
                        CE_Overflow_Check_Failed,
                        Ent => BT);
                     return;
                  end if;
               end;
            end if;

            --  If we get here we can fold the result

            Fold_Uint (N, Result, Stat);
         end;

      --  Cases where at least one operand is a real. We handle the cases
      --  of both reals, or mixed/real integer cases (the latter happen
      --  only for divide and multiply, and the result is always real).

      elsif Is_Real_Type (Ltype) or else Is_Real_Type (Rtype) then
         declare
            Left_Real  : Ureal;
            Right_Real : Ureal;
            Result     : Ureal;

         begin
            if Is_Real_Type (Ltype) then
               Left_Real := Expr_Value_R (Left);
            else
               Left_Real := UR_From_Uint (Expr_Value (Left));
            end if;

            if Is_Real_Type (Rtype) then
               Right_Real := Expr_Value_R (Right);
            else
               Right_Real := UR_From_Uint (Expr_Value (Right));
            end if;

            if Nkind (N) = N_Op_Add then
               Result := Left_Real + Right_Real;

            elsif Nkind (N) = N_Op_Subtract then
               Result := Left_Real - Right_Real;

            elsif Nkind (N) = N_Op_Multiply then
               Result := Left_Real * Right_Real;

            else pragma Assert (Nkind (N) = N_Op_Divide);
               if UR_Is_Zero (Right_Real) then
                  Apply_Compile_Time_Constraint_Error
                    (N, "division by zero", CE_Divide_By_Zero);
                  return;
               end if;

               Result := Left_Real / Right_Real;
            end if;

            Fold_Ureal (N, Result, Stat);
         end;
      end if;
   end Eval_Arithmetic_Op;

   ----------------------------
   -- Eval_Character_Literal --
   ----------------------------

   --  Nothing to be done!

   procedure Eval_Character_Literal (N : Node_Id) is
      pragma Warnings (Off, N);
   begin
      null;
   end Eval_Character_Literal;

   ---------------
   -- Eval_Call --
   ---------------

   --  Static function calls are either calls to predefined operators
   --  with static arguments, or calls to functions that rename a literal.
   --  Only the latter case is handled here, predefined operators are
   --  constant-folded elsewhere.
   --  If the function is itself inherited (see 7423-001) the literal of
   --  the parent type must be explicitly converted to the return type
   --  of the function.

   procedure Eval_Call (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Lit : Entity_Id;

   begin
      if Nkind (N) = N_Function_Call
        and then No (Parameter_Associations (N))
        and then Is_Entity_Name (Name (N))
        and then Present (Alias (Entity (Name (N))))
        and then Is_Enumeration_Type (Base_Type (Typ))
      then
         Lit := Alias (Entity (Name (N)));

         while Present (Alias (Lit)) loop
            Lit := Alias (Lit);
         end loop;

         if Ekind (Lit) = E_Enumeration_Literal then
            if Base_Type (Etype (Lit)) /= Base_Type (Typ) then
               Rewrite
                 (N, Convert_To (Typ, New_Occurrence_Of (Lit, Loc)));
            else
               Rewrite (N, New_Occurrence_Of (Lit, Loc));
            end if;

            Resolve (N, Typ);
         end if;
      end if;
   end Eval_Call;

   ------------------------
   -- Eval_Concatenation --
   ------------------------

   --  Concatenation is a static function, so the result is static if
   --  both operands are static (RM 4.9(7), 4.9(21)).

   procedure Eval_Concatenation (N : Node_Id) is
      Left  : constant Node_Id   := Left_Opnd (N);
      Right : constant Node_Id   := Right_Opnd (N);
      C_Typ : constant Entity_Id := Root_Type (Component_Type (Etype (N)));
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  Concatenation is never static in Ada 83, so if Ada 83
      --  check operand non-static context

      if Ada_Version = Ada_83
        and then Comes_From_Source (N)
      then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  If not foldable we are done. In principle concatenation that yields
      --  any string type is static (i.e. an array type of character types).
      --  However, character types can include enumeration literals, and
      --  concatenation in that case cannot be described by a literal, so we
      --  only consider the operation static if the result is an array of
      --  (a descendant of) a predefined character type.

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if (C_Typ = Standard_Character
            or else C_Typ = Standard_Wide_Character
            or else C_Typ = Standard_Wide_Wide_Character)
        and then Fold
      then
         null;
      else
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  Compile time string concatenation

      --  ??? Note that operands that are aggregates can be marked as
      --  static, so we should attempt at a later stage to fold
      --  concatenations with such aggregates.

      declare
         Left_Str  : constant Node_Id := Get_String_Val (Left);
         Left_Len  : Nat;
         Right_Str : constant Node_Id := Get_String_Val (Right);

      begin
         --  Establish new string literal, and store left operand. We make
         --  sure to use the special Start_String that takes an operand if
         --  the left operand is a string literal. Since this is optimized
         --  in the case where that is the most recently created string
         --  literal, we ensure efficient time/space behavior for the
         --  case of a concatenation of a series of string literals.

         if Nkind (Left_Str) = N_String_Literal then
            Left_Len :=  String_Length (Strval (Left_Str));
            Start_String (Strval (Left_Str));
         else
            Start_String;
            Store_String_Char (UI_To_CC (Char_Literal_Value (Left_Str)));
            Left_Len := 1;
         end if;

         --  Now append the characters of the right operand

         if Nkind (Right_Str) = N_String_Literal then
            declare
               S : constant String_Id := Strval (Right_Str);

            begin
               for J in 1 .. String_Length (S) loop
                  Store_String_Char (Get_String_Char (S, J));
               end loop;
            end;
         else
            Store_String_Char (UI_To_CC (Char_Literal_Value (Right_Str)));
         end if;

         Set_Is_Static_Expression (N, Stat);

         if Stat then

            --  If left operand is the empty string, the result is the
            --  right operand, including its bounds if anomalous.

            if Left_Len = 0
              and then Is_Array_Type (Etype (Right))
              and then Etype (Right) /= Any_String
            then
               Set_Etype (N, Etype (Right));
            end if;

            Fold_Str (N, End_String, True);
         end if;
      end;
   end Eval_Concatenation;

   ---------------------------------
   -- Eval_Conditional_Expression --
   ---------------------------------

   --  This GNAT internal construct can never be statically folded, so the
   --  only required processing is to do the check for non-static context
   --  for the two expression operands.

   procedure Eval_Conditional_Expression (N : Node_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : constant Node_Id := Next (Then_Expr);

   begin
      Check_Non_Static_Context (Then_Expr);
      Check_Non_Static_Context (Else_Expr);
   end Eval_Conditional_Expression;

   ----------------------
   -- Eval_Entity_Name --
   ----------------------

   --  This procedure is used for identifiers and expanded names other than
   --  named numbers (see Eval_Named_Integer, Eval_Named_Real. These are
   --  static if they denote a static constant (RM 4.9(6)) or if the name
   --  denotes an enumeration literal (RM 4.9(22)).

   procedure Eval_Entity_Name (N : Node_Id) is
      Def_Id : constant Entity_Id := Entity (N);
      Val    : Node_Id;

   begin
      --  Enumeration literals are always considered to be constants
      --  and cannot raise constraint error (RM 4.9(22)).

      if Ekind (Def_Id) = E_Enumeration_Literal then
         Set_Is_Static_Expression (N);
         return;

      --  A name is static if it denotes a static constant (RM 4.9(5)), and
      --  we also copy Raise_Constraint_Error. Notice that even if non-static,
      --  it does not violate 10.2.1(8) here, since this is not a variable.

      elsif Ekind (Def_Id) = E_Constant then

         --  Deferred constants must always be treated as nonstatic
         --  outside the scope of their full view.

         if Present (Full_View (Def_Id))
           and then not In_Open_Scopes (Scope (Def_Id))
         then
            Val := Empty;
         else
            Val := Constant_Value (Def_Id);
         end if;

         if Present (Val) then
            Set_Is_Static_Expression
              (N, Is_Static_Expression (Val)
                    and then Is_Static_Subtype (Etype (Def_Id)));
            Set_Raises_Constraint_Error (N, Raises_Constraint_Error (Val));

            if not Is_Static_Expression (N)
              and then not Is_Generic_Type (Etype (N))
            then
               Validate_Static_Object_Name (N);
            end if;

            return;
         end if;
      end if;

      --  Fall through if the name is not static

      Validate_Static_Object_Name (N);
   end Eval_Entity_Name;

   ----------------------------
   -- Eval_Indexed_Component --
   ----------------------------

   --  Indexed components are never static, so we need to perform the check
   --  for non-static context on the index values. Then, we check if the
   --  value can be obtained at compile time, even though it is non-static.

   procedure Eval_Indexed_Component (N : Node_Id) is
      Expr : Node_Id;

   begin
      --  Check for non-static context on index values

      Expr := First (Expressions (N));
      while Present (Expr) loop
         Check_Non_Static_Context (Expr);
         Next (Expr);
      end loop;

      --  If the indexed component appears in an object renaming declaration
      --  then we do not want to try to evaluate it, since in this case we
      --  need the identity of the array element.

      if Nkind (Parent (N)) = N_Object_Renaming_Declaration then
         return;

      --  Similarly if the indexed component appears as the prefix of an
      --  attribute we don't want to evaluate it, because at least for
      --  some cases of attributes we need the identify (e.g. Access, Size)

      elsif Nkind (Parent (N)) = N_Attribute_Reference then
         return;
      end if;

      --  Note: there are other cases, such as the left side of an assignment,
      --  or an OUT parameter for a call, where the replacement results in the
      --  illegal use of a constant, But these cases are illegal in the first
      --  place, so the replacement, though silly, is harmless.

      --  Now see if this is a constant array reference

      if List_Length (Expressions (N)) = 1
        and then Is_Entity_Name (Prefix (N))
        and then Ekind (Entity (Prefix (N))) = E_Constant
        and then Present (Constant_Value (Entity (Prefix (N))))
      then
         declare
            Loc : constant Source_Ptr := Sloc (N);
            Arr : constant Node_Id    := Constant_Value (Entity (Prefix (N)));
            Sub : constant Node_Id    := First (Expressions (N));

            Atyp : Entity_Id;
            --  Type of array

            Lin : Nat;
            --  Linear one's origin subscript value for array reference

            Lbd : Node_Id;
            --  Lower bound of the first array index

            Elm : Node_Id;
            --  Value from constant array

         begin
            Atyp := Etype (Arr);

            if Is_Access_Type (Atyp) then
               Atyp := Designated_Type (Atyp);
            end if;

            --  If we have an array type (we should have but perhaps there
            --  are error cases where this is not the case), then see if we
            --  can do a constant evaluation of the array reference.

            if Is_Array_Type (Atyp) then
               if Ekind (Atyp) = E_String_Literal_Subtype then
                  Lbd := String_Literal_Low_Bound (Atyp);
               else
                  Lbd := Type_Low_Bound (Etype (First_Index (Atyp)));
               end if;

               if Compile_Time_Known_Value (Sub)
                 and then Nkind (Arr) = N_Aggregate
                 and then Compile_Time_Known_Value (Lbd)
                 and then Is_Discrete_Type (Component_Type (Atyp))
               then
                  Lin := UI_To_Int (Expr_Value (Sub) - Expr_Value (Lbd)) + 1;

                  if List_Length (Expressions (Arr)) >= Lin then
                     Elm := Pick (Expressions (Arr), Lin);

                     --  If the resulting expression is compile time known,
                     --  then we can rewrite the indexed component with this
                     --  value, being sure to mark the result as non-static.
                     --  We also reset the Sloc, in case this generates an
                     --  error later on (e.g. 136'Access).

                     if Compile_Time_Known_Value (Elm) then
                        Rewrite (N, Duplicate_Subexpr_No_Checks (Elm));
                        Set_Is_Static_Expression (N, False);
                        Set_Sloc (N, Loc);
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;
   end Eval_Indexed_Component;

   --------------------------
   -- Eval_Integer_Literal --
   --------------------------

   --  Numeric literals are static (RM 4.9(1)), and have already been marked
   --  as static by the analyzer. The reason we did it that early is to allow
   --  the possibility of turning off the Is_Static_Expression flag after
   --  analysis, but before resolution, when integer literals are generated
   --  in the expander that do not correspond to static expressions.

   procedure Eval_Integer_Literal (N : Node_Id) is
      T : constant Entity_Id := Etype (N);

      function In_Any_Integer_Context return Boolean;
      --  If the literal is resolved with a specific type in a context
      --  where the expected type is Any_Integer, there are no range checks
      --  on the literal. By the time the literal is evaluated, it carries
      --  the type imposed by the enclosing expression, and we must recover
      --  the context to determine that Any_Integer is meant.

      ----------------------------
      -- To_Any_Integer_Context --
      ----------------------------

      function In_Any_Integer_Context return Boolean is
         Par : constant Node_Id   := Parent (N);
         K   : constant Node_Kind := Nkind (Par);

      begin
         --  Any_Integer also appears in digits specifications for real types,
         --  but those have bounds smaller that those of any integer base
         --  type, so we can safely ignore these cases.

         return    K = N_Number_Declaration
           or else K = N_Attribute_Reference
           or else K = N_Attribute_Definition_Clause
           or else K = N_Modular_Type_Definition
           or else K = N_Signed_Integer_Type_Definition;
      end In_Any_Integer_Context;

   --  Start of processing for Eval_Integer_Literal

   begin

      --  If the literal appears in a non-expression context, then it is
      --  certainly appearing in a non-static context, so check it. This
      --  is actually a redundant check, since Check_Non_Static_Context
      --  would check it, but it seems worth while avoiding the call.

      if Nkind (Parent (N)) not in N_Subexpr
        and then not In_Any_Integer_Context
      then
         Check_Non_Static_Context (N);
      end if;

      --  Modular integer literals must be in their base range

      if Is_Modular_Integer_Type (T)
        and then Is_Out_Of_Range (N, Base_Type (T))
      then
         Out_Of_Range (N);
      end if;
   end Eval_Integer_Literal;

   ---------------------
   -- Eval_Logical_Op --
   ---------------------

   --  Logical operations are static functions, so the result is potentially
   --  static if both operands are potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Logical_Op (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Compile time evaluation of logical operation

      declare
         Left_Int  : constant Uint := Expr_Value (Left);
         Right_Int : constant Uint := Expr_Value (Right);

      begin
         if Is_Modular_Integer_Type (Etype (N)) then
            declare
               Left_Bits  : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);
               Right_Bits : Bits (0 .. UI_To_Int (Esize (Etype (N))) - 1);

            begin
               To_Bits (Left_Int, Left_Bits);
               To_Bits (Right_Int, Right_Bits);

               --  Note: should really be able to use array ops instead of
               --  these loops, but they weren't working at the time ???

               if Nkind (N) = N_Op_And then
                  for J in Left_Bits'Range loop
                     Left_Bits (J) := Left_Bits (J) and Right_Bits (J);
                  end loop;

               elsif Nkind (N) = N_Op_Or then
                  for J in Left_Bits'Range loop
                     Left_Bits (J) := Left_Bits (J) or Right_Bits (J);
                  end loop;

               else
                  pragma Assert (Nkind (N) = N_Op_Xor);

                  for J in Left_Bits'Range loop
                     Left_Bits (J) := Left_Bits (J) xor Right_Bits (J);
                  end loop;
               end if;

               Fold_Uint (N, From_Bits (Left_Bits, Etype (N)), Stat);
            end;

         else
            pragma Assert (Is_Boolean_Type (Etype (N)));

            if Nkind (N) = N_Op_And then
               Fold_Uint (N,
                 Test (Is_True (Left_Int) and then Is_True (Right_Int)), Stat);

            elsif Nkind (N) = N_Op_Or then
               Fold_Uint (N,
                 Test (Is_True (Left_Int) or else Is_True (Right_Int)), Stat);

            else
               pragma Assert (Nkind (N) = N_Op_Xor);
               Fold_Uint (N,
                 Test (Is_True (Left_Int) xor Is_True (Right_Int)), Stat);
            end if;
         end if;
      end;
   end Eval_Logical_Op;

   ------------------------
   -- Eval_Membership_Op --
   ------------------------

   --  A membership test is potentially static if the expression is static,
   --  and the range is a potentially static range, or is a subtype mark
   --  denoting a static subtype (RM 4.9(12)).

   procedure Eval_Membership_Op (N : Node_Id) is
      Left   : constant Node_Id := Left_Opnd (N);
      Right  : constant Node_Id := Right_Opnd (N);
      Def_Id : Entity_Id;
      Lo     : Node_Id;
      Hi     : Node_Id;
      Result : Boolean;
      Stat   : Boolean;
      Fold   : Boolean;

   begin
      --  Ignore if error in either operand, except to make sure that
      --  Any_Type is properly propagated to avoid junk cascaded errors.

      if Etype (Left) = Any_Type
        or else Etype (Right) = Any_Type
      then
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  Case of right operand is a subtype name

      if Is_Entity_Name (Right) then
         Def_Id := Entity (Right);

         if (Is_Scalar_Type (Def_Id) or else Is_String_Type (Def_Id))
           and then Is_OK_Static_Subtype (Def_Id)
         then
            Test_Expression_Is_Foldable (N, Left, Stat, Fold);

            if not Fold or else not Stat then
               return;
            end if;
         else
            Check_Non_Static_Context (Left);
            return;
         end if;

         --  For string membership tests we will check the length
         --  further below.

         if not Is_String_Type (Def_Id) then
            Lo := Type_Low_Bound (Def_Id);
            Hi := Type_High_Bound (Def_Id);

         else
            Lo := Empty;
            Hi := Empty;
         end if;

      --  Case of right operand is a range

      else
         if Is_Static_Range (Right) then
            Test_Expression_Is_Foldable (N, Left, Stat, Fold);

            if not Fold or else not Stat then
               return;

            --  If one bound of range raises CE, then don't try to fold

            elsif not Is_OK_Static_Range (Right) then
               Check_Non_Static_Context (Left);
               return;
            end if;

         else
            Check_Non_Static_Context (Left);
            return;
         end if;

         --  Here we know range is an OK static range

         Lo := Low_Bound (Right);
         Hi := High_Bound (Right);
      end if;

      --  For strings we check that the length of the string expression is
      --  compatible with the string subtype if the subtype is constrained,
      --  or if unconstrained then the test is always true.

      if Is_String_Type (Etype (Right)) then
         if not Is_Constrained (Etype (Right)) then
            Result := True;

         else
            declare
               Typlen : constant Uint := String_Type_Len (Etype (Right));
               Strlen : constant Uint :=
                 UI_From_Int (String_Length (Strval (Get_String_Val (Left))));
            begin
               Result := (Typlen = Strlen);
            end;
         end if;

      --  Fold the membership test. We know we have a static range and Lo
      --  and Hi are set to the expressions for the end points of this range.

      elsif Is_Real_Type (Etype (Right)) then
         declare
            Leftval : constant Ureal := Expr_Value_R (Left);

         begin
            Result := Expr_Value_R (Lo) <= Leftval
                        and then Leftval <= Expr_Value_R (Hi);
         end;

      else
         declare
            Leftval : constant Uint := Expr_Value (Left);

         begin
            Result := Expr_Value (Lo) <= Leftval
                        and then Leftval <= Expr_Value (Hi);
         end;
      end if;

      if Nkind (N) = N_Not_In then
         Result := not Result;
      end if;

      Fold_Uint (N, Test (Result), True);
      Warn_On_Known_Condition (N);
   end Eval_Membership_Op;

   ------------------------
   -- Eval_Named_Integer --
   ------------------------

   procedure Eval_Named_Integer (N : Node_Id) is
   begin
      Fold_Uint (N,
        Expr_Value (Expression (Declaration_Node (Entity (N)))), True);
   end Eval_Named_Integer;

   ---------------------
   -- Eval_Named_Real --
   ---------------------

   procedure Eval_Named_Real (N : Node_Id) is
   begin
      Fold_Ureal (N,
        Expr_Value_R (Expression (Declaration_Node (Entity (N)))), True);
   end Eval_Named_Real;

   -------------------
   -- Eval_Op_Expon --
   -------------------

   --  Exponentiation is a static functions, so the result is potentially
   --  static if both operands are potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Op_Expon (N : Node_Id) is
      Left  : constant Node_Id := Left_Opnd (N);
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold exponentiation operation

      declare
         Right_Int : constant Uint := Expr_Value (Right);

      begin
         --  Integer case

         if Is_Integer_Type (Etype (Left)) then
            declare
               Left_Int : constant Uint := Expr_Value (Left);
               Result   : Uint;

            begin
               --  Exponentiation of an integer raises the exception
               --  Constraint_Error for a negative exponent (RM 4.5.6)

               if Right_Int < 0 then
                  Apply_Compile_Time_Constraint_Error
                    (N, "integer exponent negative",
                     CE_Range_Check_Failed,
                     Warn => not Stat);
                  return;

               else
                  if OK_Bits (N, Num_Bits (Left_Int) * Right_Int) then
                     Result := Left_Int ** Right_Int;
                  else
                     Result := Left_Int;
                  end if;

                  if Is_Modular_Integer_Type (Etype (N)) then
                     Result := Result mod Modulus (Etype (N));
                  end if;

                  Fold_Uint (N, Result, Stat);
               end if;
            end;

         --  Real case

         else
            declare
               Left_Real : constant Ureal := Expr_Value_R (Left);

            begin
               --  Cannot have a zero base with a negative exponent

               if UR_Is_Zero (Left_Real) then

                  if Right_Int < 0 then
                     Apply_Compile_Time_Constraint_Error
                       (N, "zero ** negative integer",
                        CE_Range_Check_Failed,
                        Warn => not Stat);
                     return;
                  else
                     Fold_Ureal (N, Ureal_0, Stat);
                  end if;

               else
                  Fold_Ureal (N, Left_Real ** Right_Int, Stat);
               end if;
            end;
         end if;
      end;
   end Eval_Op_Expon;

   -----------------
   -- Eval_Op_Not --
   -----------------

   --  The not operation is a  static functions, so the result is potentially
   --  static if the operand is potentially static (RM 4.9(7), 4.9(20)).

   procedure Eval_Op_Not (N : Node_Id) is
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold not operation

      declare
         Rint : constant Uint      := Expr_Value (Right);
         Typ  : constant Entity_Id := Etype (N);

      begin
         --  Negation is equivalent to subtracting from the modulus minus
         --  one. For a binary modulus this is equivalent to the ones-
         --  component of the original value. For non-binary modulus this
         --  is an arbitrary but consistent definition.

         if Is_Modular_Integer_Type (Typ) then
            Fold_Uint (N, Modulus (Typ) - 1 - Rint, Stat);

         else
            pragma Assert (Is_Boolean_Type (Typ));
            Fold_Uint (N, Test (not Is_True (Rint)), Stat);
         end if;

         Set_Is_Static_Expression (N, Stat);
      end;
   end Eval_Op_Not;

   -------------------------------
   -- Eval_Qualified_Expression --
   -------------------------------

   --  A qualified expression is potentially static if its subtype mark denotes
   --  a static subtype and its expression is potentially static (RM 4.9 (11)).

   procedure Eval_Qualified_Expression (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Target_Type : constant Entity_Id := Entity (Subtype_Mark (N));

      Stat : Boolean;
      Fold : Boolean;
      Hex  : Boolean;

   begin
      --  Can only fold if target is string or scalar and subtype is static
      --  Also, do not fold if our parent is an allocator (this is because
      --  the qualified expression is really part of the syntactic structure
      --  of an allocator, and we do not want to end up with something that
      --  corresponds to "new 1" where the 1 is the result of folding a
      --  qualified expression).

      if not Is_Static_Subtype (Target_Type)
        or else Nkind (Parent (N)) = N_Allocator
      then
         Check_Non_Static_Context (Operand);

         --  If operand is known to raise constraint_error, set the
         --  flag on the expression so it does not get optimized away.

         if Nkind (Operand) = N_Raise_Constraint_Error then
            Set_Raises_Constraint_Error (N);
         end if;

         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Operand, Stat, Fold);

      if not Fold then
         return;

      --  Don't try fold if target type has constraint error bounds

      elsif not Is_OK_Static_Subtype (Target_Type) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Here we will fold, save Print_In_Hex indication

      Hex := Nkind (Operand) = N_Integer_Literal
               and then Print_In_Hex (Operand);

      --  Fold the result of qualification

      if Is_Discrete_Type (Target_Type) then
         Fold_Uint (N, Expr_Value (Operand), Stat);

         --  Preserve Print_In_Hex indication

         if Hex and then Nkind (N) = N_Integer_Literal then
            Set_Print_In_Hex (N);
         end if;

      elsif Is_Real_Type (Target_Type) then
         Fold_Ureal (N, Expr_Value_R (Operand), Stat);

      else
         Fold_Str (N, Strval (Get_String_Val (Operand)), Stat);

         if not Stat then
            Set_Is_Static_Expression (N, False);
         else
            Check_String_Literal_Length (N, Target_Type);
         end if;

         return;
      end if;

      --  The expression may be foldable but not static

      Set_Is_Static_Expression (N, Stat);

      if Is_Out_Of_Range (N, Etype (N)) then
         Out_Of_Range (N);
      end if;
   end Eval_Qualified_Expression;

   -----------------------
   -- Eval_Real_Literal --
   -----------------------

   --  Numeric literals are static (RM 4.9(1)), and have already been marked
   --  as static by the analyzer. The reason we did it that early is to allow
   --  the possibility of turning off the Is_Static_Expression flag after
   --  analysis, but before resolution, when integer literals are generated
   --  in the expander that do not correspond to static expressions.

   procedure Eval_Real_Literal (N : Node_Id) is
   begin
      --  If the literal appears in a non-expression context, then it is
      --  certainly appearing in a non-static context, so check it.

      if Nkind (Parent (N)) not in N_Subexpr then
         Check_Non_Static_Context (N);
      end if;

   end Eval_Real_Literal;

   ------------------------
   -- Eval_Relational_Op --
   ------------------------

   --  Relational operations are static functions, so the result is static
   --  if both operands are static (RM 4.9(7), 4.9(20)).

   procedure Eval_Relational_Op (N : Node_Id) is
      Left   : constant Node_Id   := Left_Opnd (N);
      Right  : constant Node_Id   := Right_Opnd (N);
      Typ    : constant Entity_Id := Etype (Left);
      Result : Boolean;
      Stat   : Boolean;
      Fold   : Boolean;

   begin
      --  One special case to deal with first. If we can tell that
      --  the result will be false because the lengths of one or
      --  more index subtypes are compile time known and different,
      --  then we can replace the entire result by False. We only
      --  do this for one dimensional arrays, because the case of
      --  multi-dimensional arrays is rare and too much trouble!

      if Is_Array_Type (Typ)
        and then Number_Dimensions (Typ) = 1
        and then (Nkind (N) = N_Op_Eq
                    or else Nkind (N) = N_Op_Ne)
      then
         if Raises_Constraint_Error (Left)
           or else Raises_Constraint_Error (Right)
         then
            return;
         end if;

         declare
            procedure Get_Static_Length (Op : Node_Id; Len : out Uint);
            --  If Op is an expression for a constrained array with a
            --  known at compile time length, then Len is set to this
            --  (non-negative length). Otherwise Len is set to minus 1.

            -----------------------
            -- Get_Static_Length --
            -----------------------

            procedure Get_Static_Length (Op : Node_Id; Len : out Uint) is
               T : Entity_Id;

            begin
               if Nkind (Op) = N_String_Literal then
                  Len := UI_From_Int (String_Length (Strval (Op)));

               elsif not Is_Constrained (Etype (Op)) then
                  Len := Uint_Minus_1;

               else
                  T := Etype (First_Index (Etype (Op)));

                  if Is_Discrete_Type (T)
                    and then
                      Compile_Time_Known_Value (Type_Low_Bound (T))
                    and then
                      Compile_Time_Known_Value (Type_High_Bound (T))
                  then
                     Len := UI_Max (Uint_0,
                                     Expr_Value (Type_High_Bound (T)) -
                                     Expr_Value (Type_Low_Bound  (T)) + 1);
                  else
                     Len := Uint_Minus_1;
                  end if;
               end if;
            end Get_Static_Length;

            Len_L : Uint;
            Len_R : Uint;

         begin
            Get_Static_Length (Left,  Len_L);
            Get_Static_Length (Right, Len_R);

            if Len_L /= Uint_Minus_1
              and then Len_R /= Uint_Minus_1
              and then Len_L /= Len_R
            then
               Fold_Uint (N, Test (Nkind (N) = N_Op_Ne), False);
               Warn_On_Known_Condition (N);
               return;
            end if;
         end;
      end if;

      --  Can only fold if type is scalar (don't fold string ops)

      if not Is_Scalar_Type (Typ) then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Left, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Integer and Enumeration (discrete) type cases

      if Is_Discrete_Type (Typ) then
         declare
            Left_Int  : constant Uint := Expr_Value (Left);
            Right_Int : constant Uint := Expr_Value (Right);

         begin
            case Nkind (N) is
               when N_Op_Eq => Result := Left_Int =  Right_Int;
               when N_Op_Ne => Result := Left_Int /= Right_Int;
               when N_Op_Lt => Result := Left_Int <  Right_Int;
               when N_Op_Le => Result := Left_Int <= Right_Int;
               when N_Op_Gt => Result := Left_Int >  Right_Int;
               when N_Op_Ge => Result := Left_Int >= Right_Int;

               when others =>
                  raise Program_Error;
            end case;

            Fold_Uint (N, Test (Result), Stat);
         end;

      --  Real type case

      else
         pragma Assert (Is_Real_Type (Typ));

         declare
            Left_Real  : constant Ureal := Expr_Value_R (Left);
            Right_Real : constant Ureal := Expr_Value_R (Right);

         begin
            case Nkind (N) is
               when N_Op_Eq => Result := (Left_Real =  Right_Real);
               when N_Op_Ne => Result := (Left_Real /= Right_Real);
               when N_Op_Lt => Result := (Left_Real <  Right_Real);
               when N_Op_Le => Result := (Left_Real <= Right_Real);
               when N_Op_Gt => Result := (Left_Real >  Right_Real);
               when N_Op_Ge => Result := (Left_Real >= Right_Real);

               when others =>
                  raise Program_Error;
            end case;

            Fold_Uint (N, Test (Result), Stat);
         end;
      end if;

      Warn_On_Known_Condition (N);
   end Eval_Relational_Op;

   ----------------
   -- Eval_Shift --
   ----------------

   --  Shift operations are intrinsic operations that can never be static,
   --  so the only processing required is to perform the required check for
   --  a non static context for the two operands.

   --  Actually we could do some compile time evaluation here some time ???

   procedure Eval_Shift (N : Node_Id) is
   begin
      Check_Non_Static_Context (Left_Opnd (N));
      Check_Non_Static_Context (Right_Opnd (N));
   end Eval_Shift;

   ------------------------
   -- Eval_Short_Circuit --
   ------------------------

   --  A short circuit operation is potentially static if both operands
   --  are potentially static (RM 4.9 (13))

   procedure Eval_Short_Circuit (N : Node_Id) is
      Kind     : constant Node_Kind := Nkind (N);
      Left     : constant Node_Id   := Left_Opnd (N);
      Right    : constant Node_Id   := Right_Opnd (N);
      Left_Int : Uint;
      Rstat    : constant Boolean   :=
                   Is_Static_Expression (Left)
                     and then Is_Static_Expression (Right);

   begin
      --  Short circuit operations are never static in Ada 83

      if Ada_Version = Ada_83
        and then Comes_From_Source (N)
      then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  Now look at the operands, we can't quite use the normal call to
      --  Test_Expression_Is_Foldable here because short circuit operations
      --  are a special case, they can still be foldable, even if the right
      --  operand raises constraint error.

      --  If either operand is Any_Type, just propagate to result and
      --  do not try to fold, this prevents cascaded errors.

      if Etype (Left) = Any_Type or else Etype (Right) = Any_Type then
         Set_Etype (N, Any_Type);
         return;

      --  If left operand raises constraint error, then replace node N with
      --  the raise constraint error node, and we are obviously not foldable.
      --  Is_Static_Expression is set from the two operands in the normal way,
      --  and we check the right operand if it is in a non-static context.

      elsif Raises_Constraint_Error (Left) then
         if not Rstat then
            Check_Non_Static_Context (Right);
         end if;

         Rewrite_In_Raise_CE (N, Left);
         Set_Is_Static_Expression (N, Rstat);
         return;

      --  If the result is not static, then we won't in any case fold

      elsif not Rstat then
         Check_Non_Static_Context (Left);
         Check_Non_Static_Context (Right);
         return;
      end if;

      --  Here the result is static, note that, unlike the normal processing
      --  in Test_Expression_Is_Foldable, we did *not* check above to see if
      --  the right operand raises constraint error, that's because it is not
      --  significant if the left operand is decisive.

      Set_Is_Static_Expression (N);

      --  It does not matter if the right operand raises constraint error if
      --  it will not be evaluated. So deal specially with the cases where
      --  the right operand is not evaluated. Note that we will fold these
      --  cases even if the right operand is non-static, which is fine, but
      --  of course in these cases the result is not potentially static.

      Left_Int := Expr_Value (Left);

      if (Kind = N_And_Then and then Is_False (Left_Int))
        or else (Kind = N_Or_Else and Is_True (Left_Int))
      then
         Fold_Uint (N, Left_Int, Rstat);
         return;
      end if;

      --  If first operand not decisive, then it does matter if the right
      --  operand raises constraint error, since it will be evaluated, so
      --  we simply replace the node with the right operand. Note that this
      --  properly propagates Is_Static_Expression and Raises_Constraint_Error
      --  (both are set to True in Right).

      if Raises_Constraint_Error (Right) then
         Rewrite_In_Raise_CE (N, Right);
         Check_Non_Static_Context (Left);
         return;
      end if;

      --  Otherwise the result depends on the right operand

      Fold_Uint (N, Expr_Value (Right), Rstat);
      return;
   end Eval_Short_Circuit;

   ----------------
   -- Eval_Slice --
   ----------------

   --  Slices can never be static, so the only processing required is to
   --  check for non-static context if an explicit range is given.

   procedure Eval_Slice (N : Node_Id) is
      Drange : constant Node_Id := Discrete_Range (N);

   begin
      if Nkind (Drange) = N_Range then
         Check_Non_Static_Context (Low_Bound (Drange));
         Check_Non_Static_Context (High_Bound (Drange));
      end if;
   end Eval_Slice;

   -------------------------
   -- Eval_String_Literal --
   -------------------------

   procedure Eval_String_Literal (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);
      Bas : constant Entity_Id := Base_Type (Typ);
      Xtp : Entity_Id;
      Len : Nat;
      Lo  : Node_Id;

   begin
      --  Nothing to do if error type (handles cases like default expressions
      --  or generics where we have not yet fully resolved the type)

      if Bas = Any_Type or else Bas = Any_String then
         return;
      end if;

      --  String literals are static if the subtype is static (RM 4.9(2)), so
      --  reset the static expression flag (it was set unconditionally in
      --  Analyze_String_Literal) if the subtype is non-static. We tell if
      --  the subtype is static by looking at the lower bound.

      if Ekind (Typ) = E_String_Literal_Subtype then
         if not Is_OK_Static_Expression (String_Literal_Low_Bound (Typ)) then
            Set_Is_Static_Expression (N, False);
            return;
         end if;

      --  Here if Etype of string literal is normal Etype (not yet possible,
      --  but may be possible in future!)

      elsif not Is_OK_Static_Expression
                    (Type_Low_Bound (Etype (First_Index (Typ))))
      then
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  If original node was a type conversion, then result if non-static

      if Nkind (Original_Node (N)) = N_Type_Conversion then
         Set_Is_Static_Expression (N, False);
         return;
      end if;

      --  Test for illegal Ada 95 cases. A string literal is illegal in
      --  Ada 95 if its bounds are outside the index base type and this
      --  index type is static. This can happen in only two ways. Either
      --  the string literal is too long, or it is null, and the lower
      --  bound is type'First. In either case it is the upper bound that
      --  is out of range of the index type.

      if Ada_Version >= Ada_95 then
         if Root_Type (Bas) = Standard_String
              or else
            Root_Type (Bas) = Standard_Wide_String
         then
            Xtp := Standard_Positive;
         else
            Xtp := Etype (First_Index (Bas));
         end if;

         if Ekind (Typ) = E_String_Literal_Subtype then
            Lo := String_Literal_Low_Bound (Typ);
         else
            Lo := Type_Low_Bound (Etype (First_Index (Typ)));
         end if;

         Len := String_Length (Strval (N));

         if UI_From_Int (Len) > String_Type_Len (Bas) then
            Apply_Compile_Time_Constraint_Error
              (N, "string literal too long for}", CE_Length_Check_Failed,
               Ent => Bas,
               Typ => First_Subtype (Bas));

         elsif Len = 0
           and then not Is_Generic_Type (Xtp)
           and then
             Expr_Value (Lo) = Expr_Value (Type_Low_Bound (Base_Type (Xtp)))
         then
            Apply_Compile_Time_Constraint_Error
              (N, "null string literal not allowed for}",
               CE_Length_Check_Failed,
               Ent => Bas,
               Typ => First_Subtype (Bas));
         end if;
      end if;
   end Eval_String_Literal;

   --------------------------
   -- Eval_Type_Conversion --
   --------------------------

   --  A type conversion is potentially static if its subtype mark is for a
   --  static scalar subtype, and its operand expression is potentially static
   --  (RM 4.9 (10))

   procedure Eval_Type_Conversion (N : Node_Id) is
      Operand     : constant Node_Id   := Expression (N);
      Source_Type : constant Entity_Id := Etype (Operand);
      Target_Type : constant Entity_Id := Etype (N);

      Stat   : Boolean;
      Fold   : Boolean;

      function To_Be_Treated_As_Integer (T : Entity_Id) return Boolean;
      --  Returns true if type T is an integer type, or if it is a
      --  fixed-point type to be treated as an integer (i.e. the flag
      --  Conversion_OK is set on the conversion node).

      function To_Be_Treated_As_Real (T : Entity_Id) return Boolean;
      --  Returns true if type T is a floating-point type, or if it is a
      --  fixed-point type that is not to be treated as an integer (i.e. the
      --  flag Conversion_OK is not set on the conversion node).

      ------------------------------
      -- To_Be_Treated_As_Integer --
      ------------------------------

      function To_Be_Treated_As_Integer (T : Entity_Id) return Boolean is
      begin
         return
           Is_Integer_Type (T)
             or else (Is_Fixed_Point_Type (T) and then Conversion_OK (N));
      end To_Be_Treated_As_Integer;

      ---------------------------
      -- To_Be_Treated_As_Real --
      ---------------------------

      function To_Be_Treated_As_Real (T : Entity_Id) return Boolean is
      begin
         return
           Is_Floating_Point_Type (T)
             or else (Is_Fixed_Point_Type (T) and then not Conversion_OK (N));
      end To_Be_Treated_As_Real;

   --  Start of processing for Eval_Type_Conversion

   begin
      --  Cannot fold if target type is non-static or if semantic error

      if not Is_Static_Subtype (Target_Type) then
         Check_Non_Static_Context (Operand);
         return;

      elsif Error_Posted (N) then
         return;
      end if;

      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Operand, Stat, Fold);

      if not Fold then
         return;

      --  Don't try fold if target type has constraint error bounds

      elsif not Is_OK_Static_Subtype (Target_Type) then
         Set_Raises_Constraint_Error (N);
         return;
      end if;

      --  Remaining processing depends on operand types. Note that in the
      --  following type test, fixed-point counts as real unless the flag
      --  Conversion_OK is set, in which case it counts as integer.

      --  Fold conversion, case of string type. The result is not static

      if Is_String_Type (Target_Type) then
         Fold_Str (N, Strval (Get_String_Val (Operand)), False);

         return;

      --  Fold conversion, case of integer target type

      elsif To_Be_Treated_As_Integer (Target_Type) then
         declare
            Result : Uint;

         begin
            --  Integer to integer conversion

            if To_Be_Treated_As_Integer (Source_Type) then
               Result := Expr_Value (Operand);

            --  Real to integer conversion

            else
               Result := UR_To_Uint (Expr_Value_R (Operand));
            end if;

            --  If fixed-point type (Conversion_OK must be set), then the
            --  result is logically an integer, but we must replace the
            --  conversion with the corresponding real literal, since the
            --  type from a semantic point of view is still fixed-point.

            if Is_Fixed_Point_Type (Target_Type) then
               Fold_Ureal
                 (N, UR_From_Uint (Result) * Small_Value (Target_Type), Stat);

            --  Otherwise result is integer literal

            else
               Fold_Uint (N, Result, Stat);
            end if;
         end;

      --  Fold conversion, case of real target type

      elsif To_Be_Treated_As_Real (Target_Type) then
         declare
            Result : Ureal;

         begin
            if To_Be_Treated_As_Real (Source_Type) then
               Result := Expr_Value_R (Operand);
            else
               Result := UR_From_Uint (Expr_Value (Operand));
            end if;

            Fold_Ureal (N, Result, Stat);
         end;

      --  Enumeration types

      else
         Fold_Uint (N, Expr_Value (Operand), Stat);
      end if;

      if Is_Out_Of_Range (N, Etype (N)) then
         Out_Of_Range (N);
      end if;

   end Eval_Type_Conversion;

   -------------------
   -- Eval_Unary_Op --
   -------------------

   --  Predefined unary operators are static functions (RM 4.9(20)) and thus
   --  are potentially static if the operand is potentially static (RM 4.9(7))

   procedure Eval_Unary_Op (N : Node_Id) is
      Right : constant Node_Id := Right_Opnd (N);
      Stat  : Boolean;
      Fold  : Boolean;

   begin
      --  If not foldable we are done

      Test_Expression_Is_Foldable (N, Right, Stat, Fold);

      if not Fold then
         return;
      end if;

      --  Fold for integer case

      if Is_Integer_Type (Etype (N)) then
         declare
            Rint   : constant Uint := Expr_Value (Right);
            Result : Uint;

         begin
            --  In the case of modular unary plus and abs there is no need
            --  to adjust the result of the operation since if the original
            --  operand was in bounds the result will be in the bounds of the
            --  modular type. However, in the case of modular unary minus the
            --  result may go out of the bounds of the modular type and needs
            --  adjustment.

            if Nkind (N) = N_Op_Plus then
               Result := Rint;

            elsif Nkind (N) = N_Op_Minus then
               if Is_Modular_Integer_Type (Etype (N)) then
                  Result := (-Rint) mod Modulus (Etype (N));
               else
                  Result := (-Rint);
               end if;

            else
               pragma Assert (Nkind (N) = N_Op_Abs);
               Result := abs Rint;
            end if;

            Fold_Uint (N, Result, Stat);
         end;

      --  Fold for real case

      elsif Is_Real_Type (Etype (N)) then
         declare
            Rreal  : constant Ureal := Expr_Value_R (Right);
            Result : Ureal;

         begin
            if Nkind (N) = N_Op_Plus then
               Result := Rreal;

            elsif Nkind (N) = N_Op_Minus then
               Result := UR_Negate (Rreal);

            else
               pragma Assert (Nkind (N) = N_Op_Abs);
               Result := abs Rreal;
            end if;

            Fold_Ureal (N, Result, Stat);
         end;
      end if;
   end Eval_Unary_Op;

   -------------------------------
   -- Eval_Unchecked_Conversion --
   -------------------------------

   --  Unchecked conversions can never be static, so the only required
   --  processing is to check for a non-static context for the operand.

   procedure Eval_Unchecked_Conversion (N : Node_Id) is
   begin
      Check_Non_Static_Context (Expression (N));
   end Eval_Unchecked_Conversion;

   --------------------
   -- Expr_Rep_Value --
   --------------------

   function Expr_Rep_Value (N : Node_Id) return Uint is
      Kind : constant Node_Kind := Nkind (N);
      Ent  : Entity_Id;

   begin
      if Is_Entity_Name (N) then
         Ent := Entity (N);

         --  An enumeration literal that was either in the source or
         --  created as a result of static evaluation.

         if Ekind (Ent) = E_Enumeration_Literal then
            return Enumeration_Rep (Ent);

         --  A user defined static constant

         else
            pragma Assert (Ekind (Ent) = E_Constant);
            return Expr_Rep_Value (Constant_Value (Ent));
         end if;

      --  An integer literal that was either in the source or created
      --  as a result of static evaluation.

      elsif Kind = N_Integer_Literal then
         return Intval (N);

      --  A real literal for a fixed-point type. This must be the fixed-point
      --  case, either the literal is of a fixed-point type, or it is a bound
      --  of a fixed-point type, with type universal real. In either case we
      --  obtain the desired value from Corresponding_Integer_Value.

      elsif Kind = N_Real_Literal then
         pragma Assert (Is_Fixed_Point_Type (Underlying_Type (Etype (N))));
         return Corresponding_Integer_Value (N);

      --  Peculiar VMS case, if we have xxx'Null_Parameter, return zero

      elsif Kind = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Null_Parameter
      then
         return Uint_0;

      --  Otherwise must be character literal

      else
         pragma Assert (Kind = N_Character_Literal);
         Ent := Entity (N);

         --  Since Character literals of type Standard.Character don't
         --  have any defining character literals built for them, they
         --  do not have their Entity set, so just use their Char
         --  code. Otherwise for user-defined character literals use
         --  their Pos value as usual which is the same as the Rep value.

         if No (Ent) then
            return Char_Literal_Value (N);
         else
            return Enumeration_Rep (Ent);
         end if;
      end if;
   end Expr_Rep_Value;

   ----------------
   -- Expr_Value --
   ----------------

   function Expr_Value (N : Node_Id) return Uint is
      Kind   : constant Node_Kind := Nkind (N);
      CV_Ent : CV_Entry renames CV_Cache (Nat (N) mod CV_Cache_Size);
      Ent    : Entity_Id;
      Val    : Uint;

   begin
      --  If already in cache, then we know it's compile time known and
      --  we can return the value that was previously stored in the cache
      --  since compile time known values cannot change :-)

      if CV_Ent.N = N then
         return CV_Ent.V;
      end if;

      --  Otherwise proceed to test value

      if Is_Entity_Name (N) then
         Ent := Entity (N);

         --  An enumeration literal that was either in the source or
         --  created as a result of static evaluation.

         if Ekind (Ent) = E_Enumeration_Literal then
            Val := Enumeration_Pos (Ent);

         --  A user defined static constant

         else
            pragma Assert (Ekind (Ent) = E_Constant);
            Val := Expr_Value (Constant_Value (Ent));
         end if;

      --  An integer literal that was either in the source or created
      --  as a result of static evaluation.

      elsif Kind = N_Integer_Literal then
         Val := Intval (N);

      --  A real literal for a fixed-point type. This must be the fixed-point
      --  case, either the literal is of a fixed-point type, or it is a bound
      --  of a fixed-point type, with type universal real. In either case we
      --  obtain the desired value from Corresponding_Integer_Value.

      elsif Kind = N_Real_Literal then

         pragma Assert (Is_Fixed_Point_Type (Underlying_Type (Etype (N))));
         Val := Corresponding_Integer_Value (N);

      --  Peculiar VMS case, if we have xxx'Null_Parameter, return zero

      elsif Kind = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Null_Parameter
      then
         Val := Uint_0;

      --  Otherwise must be character literal

      else
         pragma Assert (Kind = N_Character_Literal);
         Ent := Entity (N);

         --  Since Character literals of type Standard.Character don't
         --  have any defining character literals built for them, they
         --  do not have their Entity set, so just use their Char
         --  code. Otherwise for user-defined character literals use
         --  their Pos value as usual.

         if No (Ent) then
            Val := Char_Literal_Value (N);
         else
            Val := Enumeration_Pos (Ent);
         end if;
      end if;

      --  Come here with Val set to value to be returned, set cache

      CV_Ent.N := N;
      CV_Ent.V := Val;
      return Val;
   end Expr_Value;

   ------------------
   -- Expr_Value_E --
   ------------------

   function Expr_Value_E (N : Node_Id) return Entity_Id is
      Ent  : constant Entity_Id := Entity (N);

   begin
      if Ekind (Ent) = E_Enumeration_Literal then
         return Ent;
      else
         pragma Assert (Ekind (Ent) = E_Constant);
         return Expr_Value_E (Constant_Value (Ent));
      end if;
   end Expr_Value_E;

   ------------------
   -- Expr_Value_R --
   ------------------

   function Expr_Value_R (N : Node_Id) return Ureal is
      Kind : constant Node_Kind := Nkind (N);
      Ent  : Entity_Id;
      Expr : Node_Id;

   begin
      if Kind = N_Real_Literal then
         return Realval (N);

      elsif Kind = N_Identifier or else Kind = N_Expanded_Name then
         Ent := Entity (N);
         pragma Assert (Ekind (Ent) = E_Constant);
         return Expr_Value_R (Constant_Value (Ent));

      elsif Kind = N_Integer_Literal then
         return UR_From_Uint (Expr_Value (N));

      --  Strange case of VAX literals, which are at this stage transformed
      --  into Vax_Type!x_To_y(IEEE_Literal). See Expand_N_Real_Literal in
      --  Exp_Vfpt for further details.

      elsif Vax_Float (Etype (N))
        and then Nkind (N) = N_Unchecked_Type_Conversion
      then
         Expr := Expression (N);

         if Nkind (Expr) = N_Function_Call
           and then Present (Parameter_Associations (Expr))
         then
            Expr := First (Parameter_Associations (Expr));

            if Nkind (Expr) = N_Real_Literal then
               return Realval (Expr);
            end if;
         end if;

      --  Peculiar VMS case, if we have xxx'Null_Parameter, return 0.0

      elsif Kind = N_Attribute_Reference
        and then Attribute_Name (N) = Name_Null_Parameter
      then
         return Ureal_0;
      end if;

      --  If we fall through, we have a node that cannot be interepreted
      --  as a compile time constant. That is definitely an error.

      raise Program_Error;
   end Expr_Value_R;

   ------------------
   -- Expr_Value_S --
   ------------------

   function Expr_Value_S (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_String_Literal then
         return N;
      else
         pragma Assert (Ekind (Entity (N)) = E_Constant);
         return Expr_Value_S (Constant_Value (Entity (N)));
      end if;
   end Expr_Value_S;

   --------------------------
   -- Flag_Non_Static_Expr --
   --------------------------

   procedure Flag_Non_Static_Expr (Msg : String; Expr : Node_Id) is
   begin
      if Error_Posted (Expr) and then not All_Errors_Mode then
         return;
      else
         Error_Msg_F (Msg, Expr);
         Why_Not_Static (Expr);
      end if;
   end Flag_Non_Static_Expr;

   --------------
   -- Fold_Str --
   --------------

   procedure Fold_Str (N : Node_Id; Val : String_Id; Static : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      Rewrite (N, Make_String_Literal (Loc, Strval => Val));

      --  We now have the literal with the right value, both the actual type
      --  and the expected type of this literal are taken from the expression
      --  that was evaluated.

      Analyze (N);
      Set_Is_Static_Expression (N, Static);
      Set_Etype (N, Typ);
      Resolve (N);
   end Fold_Str;

   ---------------
   -- Fold_Uint --
   ---------------

   procedure Fold_Uint (N : Node_Id; Val : Uint; Static : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : Entity_Id  := Etype (N);
      Ent : Entity_Id;

   begin
      --  If we are folding a named number, retain the entity in the
      --  literal, for ASIS use.

      if Is_Entity_Name (N)
        and then Ekind (Entity (N)) = E_Named_Integer
      then
         Ent := Entity (N);
      else
         Ent := Empty;
      end if;

      if Is_Private_Type (Typ) then
         Typ := Full_View (Typ);
      end if;

      --  For a result of type integer, subsitute an N_Integer_Literal node
      --  for the result of the compile time evaluation of the expression.

      if Is_Integer_Type (Typ) then
         Rewrite (N, Make_Integer_Literal (Loc, Val));
         Set_Original_Entity (N, Ent);

      --  Otherwise we have an enumeration type, and we substitute either
      --  an N_Identifier or N_Character_Literal to represent the enumeration
      --  literal corresponding to the given value, which must always be in
      --  range, because appropriate tests have already been made for this.

      else pragma Assert (Is_Enumeration_Type (Typ));
         Rewrite (N, Get_Enum_Lit_From_Pos (Etype (N), Val, Loc));
      end if;

      --  We now have the literal with the right value, both the actual type
      --  and the expected type of this literal are taken from the expression
      --  that was evaluated.

      Analyze (N);
      Set_Is_Static_Expression (N, Static);
      Set_Etype (N, Typ);
      Resolve (N);
   end Fold_Uint;

   ----------------
   -- Fold_Ureal --
   ----------------

   procedure Fold_Ureal (N : Node_Id; Val : Ureal; Static : Boolean) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Ent : Entity_Id;

   begin
      --  If we are folding a named number, retain the entity in the
      --  literal, for ASIS use.

      if Is_Entity_Name (N)
        and then Ekind (Entity (N)) = E_Named_Real
      then
         Ent := Entity (N);
      else
         Ent := Empty;
      end if;

      Rewrite (N, Make_Real_Literal (Loc, Realval => Val));
      Set_Original_Entity (N, Ent);

      --  Both the actual and expected type comes from the original expression

      Analyze (N);
      Set_Is_Static_Expression (N, Static);
      Set_Etype (N, Typ);
      Resolve (N);
   end Fold_Ureal;

   ---------------
   -- From_Bits --
   ---------------

   function From_Bits (B : Bits; T : Entity_Id) return Uint is
      V : Uint := Uint_0;

   begin
      for J in 0 .. B'Last loop
         if B (J) then
            V := V + 2 ** J;
         end if;
      end loop;

      if Non_Binary_Modulus (T) then
         V := V mod Modulus (T);
      end if;

      return V;
   end From_Bits;

   --------------------
   -- Get_String_Val --
   --------------------

   function Get_String_Val (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_String_Literal then
         return N;

      elsif Nkind (N) = N_Character_Literal then
         return N;

      else
         pragma Assert (Is_Entity_Name (N));
         return Get_String_Val (Constant_Value (Entity (N)));
      end if;
   end Get_String_Val;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      CV_Cache := (others => (Node_High_Bound, Uint_0));
   end Initialize;

   --------------------
   -- In_Subrange_Of --
   --------------------

   function In_Subrange_Of
     (T1        : Entity_Id;
      T2        : Entity_Id;
      Fixed_Int : Boolean := False)
      return      Boolean
   is
      L1 : Node_Id;
      H1 : Node_Id;

      L2 : Node_Id;
      H2 : Node_Id;

   begin
      if T1 = T2 or else Is_Subtype_Of (T1, T2) then
         return True;

      --  Never in range if both types are not scalar. Don't know if this can
      --  actually happen, but just in case.

      elsif not Is_Scalar_Type (T1) or else not Is_Scalar_Type (T1) then
         return False;

      else
         L1 := Type_Low_Bound  (T1);
         H1 := Type_High_Bound (T1);

         L2 := Type_Low_Bound  (T2);
         H2 := Type_High_Bound (T2);

         --  Check bounds to see if comparison possible at compile time

         if Compile_Time_Compare (L1, L2) in Compare_GE
              and then
            Compile_Time_Compare (H1, H2) in Compare_LE
         then
            return True;
         end if;

         --  If bounds not comparable at compile time, then the bounds of T2
         --  must be compile time known or we cannot answer the query.

         if not Compile_Time_Known_Value (L2)
           or else not Compile_Time_Known_Value (H2)
         then
            return False;
         end if;

         --  If the bounds of T1 are know at compile time then use these
         --  ones, otherwise use the bounds of the base type (which are of
         --  course always static).

         if not Compile_Time_Known_Value (L1) then
            L1 := Type_Low_Bound (Base_Type (T1));
         end if;

         if not Compile_Time_Known_Value (H1) then
            H1 := Type_High_Bound (Base_Type (T1));
         end if;

         --  Fixed point types should be considered as such only if
         --  flag Fixed_Int is set to False.

         if Is_Floating_Point_Type (T1) or else Is_Floating_Point_Type (T2)
           or else (Is_Fixed_Point_Type (T1) and then not Fixed_Int)
           or else (Is_Fixed_Point_Type (T2) and then not Fixed_Int)
         then
            return
              Expr_Value_R (L2) <= Expr_Value_R (L1)
                and then
              Expr_Value_R (H2) >= Expr_Value_R (H1);

         else
            return
              Expr_Value (L2) <= Expr_Value (L1)
                and then
              Expr_Value (H2) >= Expr_Value (H1);

         end if;
      end if;

   --  If any exception occurs, it means that we have some bug in the compiler
   --  possibly triggered by a previous error, or by some unforseen peculiar
   --  occurrence. However, this is only an optimization attempt, so there is
   --  really no point in crashing the compiler. Instead we just decide, too
   --  bad, we can't figure out the answer in this case after all.

   exception
      when others =>

         --  Debug flag K disables this behavior (useful for debugging)

         if Debug_Flag_K then
            raise;
         else
            return False;
         end if;
   end In_Subrange_Of;

   -----------------
   -- Is_In_Range --
   -----------------

   function Is_In_Range
     (N         : Node_Id;
      Typ       : Entity_Id;
      Fixed_Int : Boolean := False;
      Int_Real  : Boolean := False)
      return      Boolean
   is
      Val  : Uint;
      Valr : Ureal;

   begin
      --  Universal types have no range limits, so always in range

      if Typ = Universal_Integer or else Typ = Universal_Real then
         return True;

      --  Never in range if not scalar type. Don't know if this can
      --  actually happen, but our spec allows it, so we must check!

      elsif not Is_Scalar_Type (Typ) then
         return False;

      --  Never in range unless we have a compile time known value

      elsif not Compile_Time_Known_Value (N) then
         return False;

      else
         declare
            Lo       : constant Node_Id := Type_Low_Bound  (Typ);
            Hi       : constant Node_Id := Type_High_Bound (Typ);
            LB_Known : constant Boolean := Compile_Time_Known_Value (Lo);
            UB_Known : constant Boolean := Compile_Time_Known_Value (Hi);

         begin
            --  Fixed point types should be considered as such only in
            --  flag Fixed_Int is set to False.

            if Is_Floating_Point_Type (Typ)
              or else (Is_Fixed_Point_Type (Typ) and then not Fixed_Int)
              or else Int_Real
            then
               Valr := Expr_Value_R (N);

               if LB_Known and then Valr >= Expr_Value_R (Lo)
                 and then UB_Known and then Valr <= Expr_Value_R (Hi)
               then
                  return True;
               else
                  return False;
               end if;

            else
               Val := Expr_Value (N);

               if         LB_Known and then Val >= Expr_Value (Lo)
                 and then UB_Known and then Val <= Expr_Value (Hi)
               then
                  return True;
               else
                  return False;
               end if;
            end if;
         end;
      end if;
   end Is_In_Range;

   -------------------
   -- Is_Null_Range --
   -------------------

   function Is_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Lo);

   begin
      if not Compile_Time_Known_Value (Lo)
        or else not Compile_Time_Known_Value (Hi)
      then
         return False;
      end if;

      if Is_Discrete_Type (Typ) then
         return Expr_Value (Lo) > Expr_Value (Hi);

      else
         pragma Assert (Is_Real_Type (Typ));
         return Expr_Value_R (Lo) > Expr_Value_R (Hi);
      end if;
   end Is_Null_Range;

   -----------------------------
   -- Is_OK_Static_Expression --
   -----------------------------

   function Is_OK_Static_Expression (N : Node_Id) return Boolean is
   begin
      return Is_Static_Expression (N)
        and then not Raises_Constraint_Error (N);
   end Is_OK_Static_Expression;

   ------------------------
   -- Is_OK_Static_Range --
   ------------------------

   --  A static range is a range whose bounds are static expressions, or a
   --  Range_Attribute_Reference equivalent to such a range (RM 4.9(26)).
   --  We have already converted range attribute references, so we get the
   --  "or" part of this rule without needing a special test.

   function Is_OK_Static_Range (N : Node_Id) return Boolean is
   begin
      return Is_OK_Static_Expression (Low_Bound (N))
        and then Is_OK_Static_Expression (High_Bound (N));
   end Is_OK_Static_Range;

   --------------------------
   -- Is_OK_Static_Subtype --
   --------------------------

   --  Determines if Typ is a static subtype as defined in (RM 4.9(26))
   --  where neither bound raises constraint error when evaluated.

   function Is_OK_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T   : constant Entity_Id := Base_Type (Typ);
      Anc_Subt : Entity_Id;

   begin
      --  First a quick check on the non static subtype flag. As described
      --  in further detail in Einfo, this flag is not decisive in all cases,
      --  but if it is set, then the subtype is definitely non-static.

      if Is_Non_Static_Subtype (Typ) then
         return False;
      end if;

      Anc_Subt := Ancestor_Subtype (Typ);

      if Anc_Subt = Empty then
         Anc_Subt := Base_T;
      end if;

      if Is_Generic_Type (Root_Type (Base_T))
        or else Is_Generic_Actual_Type (Base_T)
      then
         return False;

      --  String types

      elsif Is_String_Type (Typ) then
         return
           Ekind (Typ) = E_String_Literal_Subtype
             or else
           (Is_OK_Static_Subtype (Component_Type (Typ))
              and then Is_OK_Static_Subtype (Etype (First_Index (Typ))));

      --  Scalar types

      elsif Is_Scalar_Type (Typ) then
         if Base_T = Typ then
            return True;

         else
            --  Scalar_Range (Typ) might be an N_Subtype_Indication, so
            --  use Get_Type_Low,High_Bound.

            return     Is_OK_Static_Subtype (Anc_Subt)
              and then Is_OK_Static_Expression (Type_Low_Bound (Typ))
              and then Is_OK_Static_Expression (Type_High_Bound (Typ));
         end if;

      --  Types other than string and scalar types are never static

      else
         return False;
      end if;
   end Is_OK_Static_Subtype;

   ---------------------
   -- Is_Out_Of_Range --
   ---------------------

   function Is_Out_Of_Range
     (N         : Node_Id;
      Typ       : Entity_Id;
      Fixed_Int : Boolean := False;
      Int_Real  : Boolean := False)
      return      Boolean
   is
      Val  : Uint;
      Valr : Ureal;

   begin
      --  Universal types have no range limits, so always in range

      if Typ = Universal_Integer or else Typ = Universal_Real then
         return False;

      --  Never out of range if not scalar type. Don't know if this can
      --  actually happen, but our spec allows it, so we must check!

      elsif not Is_Scalar_Type (Typ) then
         return False;

      --  Never out of range if this is a generic type, since the bounds
      --  of generic types are junk. Note that if we only checked for
      --  static expressions (instead of compile time known values) below,
      --  we would not need this check, because values of a generic type
      --  can never be static, but they can be known at compile time.

      elsif Is_Generic_Type (Typ) then
         return False;

      --  Never out of range unless we have a compile time known value

      elsif not Compile_Time_Known_Value (N) then
         return False;

      else
         declare
            Lo       : constant Node_Id := Type_Low_Bound  (Typ);
            Hi       : constant Node_Id := Type_High_Bound (Typ);
            LB_Known : constant Boolean := Compile_Time_Known_Value (Lo);
            UB_Known : constant Boolean := Compile_Time_Known_Value (Hi);

         begin
            --  Real types (note that fixed-point types are not treated
            --  as being of a real type if the flag Fixed_Int is set,
            --  since in that case they are regarded as integer types).

            if Is_Floating_Point_Type (Typ)
              or else (Is_Fixed_Point_Type (Typ) and then not Fixed_Int)
              or else Int_Real
            then
               Valr := Expr_Value_R (N);

               if LB_Known and then Valr < Expr_Value_R (Lo) then
                  return True;

               elsif UB_Known and then Expr_Value_R (Hi) < Valr then
                  return True;

               else
                  return False;
               end if;

            else
               Val := Expr_Value (N);

               if LB_Known and then Val < Expr_Value (Lo) then
                  return True;

               elsif UB_Known and then Expr_Value (Hi) < Val then
                  return True;

               else
                  return False;
               end if;
            end if;
         end;
      end if;
   end Is_Out_Of_Range;

   ---------------------
   -- Is_Static_Range --
   ---------------------

   --  A static range is a range whose bounds are static expressions, or a
   --  Range_Attribute_Reference equivalent to such a range (RM 4.9(26)).
   --  We have already converted range attribute references, so we get the
   --  "or" part of this rule without needing a special test.

   function Is_Static_Range (N : Node_Id) return Boolean is
   begin
      return Is_Static_Expression (Low_Bound (N))
        and then Is_Static_Expression (High_Bound (N));
   end Is_Static_Range;

   -----------------------
   -- Is_Static_Subtype --
   -----------------------

   --  Determines if Typ is a static subtype as defined in (RM 4.9(26))

   function Is_Static_Subtype (Typ : Entity_Id) return Boolean is
      Base_T   : constant Entity_Id := Base_Type (Typ);
      Anc_Subt : Entity_Id;

   begin
      --  First a quick check on the non static subtype flag. As described
      --  in further detail in Einfo, this flag is not decisive in all cases,
      --  but if it is set, then the subtype is definitely non-static.

      if Is_Non_Static_Subtype (Typ) then
         return False;
      end if;

      Anc_Subt := Ancestor_Subtype (Typ);

      if Anc_Subt = Empty then
         Anc_Subt := Base_T;
      end if;

      if Is_Generic_Type (Root_Type (Base_T))
        or else Is_Generic_Actual_Type (Base_T)
      then
         return False;

      --  String types

      elsif Is_String_Type (Typ) then
         return
           Ekind (Typ) = E_String_Literal_Subtype
             or else
           (Is_Static_Subtype (Component_Type (Typ))
              and then Is_Static_Subtype (Etype (First_Index (Typ))));

      --  Scalar types

      elsif Is_Scalar_Type (Typ) then
         if Base_T = Typ then
            return True;

         else
            return     Is_Static_Subtype (Anc_Subt)
              and then Is_Static_Expression (Type_Low_Bound (Typ))
              and then Is_Static_Expression (Type_High_Bound (Typ));
         end if;

      --  Types other than string and scalar types are never static

      else
         return False;
      end if;
   end Is_Static_Subtype;

   --------------------
   -- Not_Null_Range --
   --------------------

   function Not_Null_Range (Lo : Node_Id; Hi : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Lo);

   begin
      if not Compile_Time_Known_Value (Lo)
        or else not Compile_Time_Known_Value (Hi)
      then
         return False;
      end if;

      if Is_Discrete_Type (Typ) then
         return Expr_Value (Lo) <= Expr_Value (Hi);

      else
         pragma Assert (Is_Real_Type (Typ));

         return Expr_Value_R (Lo) <= Expr_Value_R (Hi);
      end if;
   end Not_Null_Range;

   -------------
   -- OK_Bits --
   -------------

   function OK_Bits (N : Node_Id; Bits : Uint) return Boolean is
   begin
      --  We allow a maximum of 500,000 bits which seems a reasonable limit

      if Bits < 500_000 then
         return True;

      else
         Error_Msg_N ("static value too large, capacity exceeded", N);
         return False;
      end if;
   end OK_Bits;

   ------------------
   -- Out_Of_Range --
   ------------------

   procedure Out_Of_Range (N : Node_Id) is
   begin
      --  If we have the static expression case, then this is an illegality
      --  in Ada 95 mode, except that in an instance, we never generate an
      --  error (if the error is legitimate, it was already diagnosed in
      --  the template). The expression to compute the length of a packed
      --  array is attached to the array type itself, and deserves a separate
      --  message.

      if Is_Static_Expression (N)
        and then not In_Instance
        and then not In_Inlined_Body
        and then Ada_Version >= Ada_95
      then
         if Nkind (Parent (N)) = N_Defining_Identifier
           and then Is_Array_Type (Parent (N))
           and then Present (Packed_Array_Type (Parent (N)))
           and then Present (First_Rep_Item (Parent (N)))
         then
            Error_Msg_N
             ("length of packed array must not exceed Integer''Last",
              First_Rep_Item (Parent (N)));
            Rewrite (N, Make_Integer_Literal (Sloc (N), Uint_1));

         else
            Apply_Compile_Time_Constraint_Error
              (N, "value not in range of}", CE_Range_Check_Failed);
         end if;

      --  Here we generate a warning for the Ada 83 case, or when we are
      --  in an instance, or when we have a non-static expression case.

      else
         Apply_Compile_Time_Constraint_Error
           (N, "value not in range of}?", CE_Range_Check_Failed);
      end if;
   end Out_Of_Range;

   -------------------------
   -- Rewrite_In_Raise_CE --
   -------------------------

   procedure Rewrite_In_Raise_CE (N : Node_Id; Exp : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      --  If we want to raise CE in the condition of a raise_CE node
      --  we may as well get rid of the condition

      if Present (Parent (N))
        and then Nkind (Parent (N)) = N_Raise_Constraint_Error
      then
         Set_Condition (Parent (N), Empty);

      --  If the expression raising CE is a N_Raise_CE node, we can use
      --  that one. We just preserve the type of the context

      elsif Nkind (Exp) = N_Raise_Constraint_Error then
         Rewrite (N, Exp);
         Set_Etype (N, Typ);

      --  We have to build an explicit raise_ce node

      else
         Rewrite (N,
           Make_Raise_Constraint_Error (Sloc (Exp),
             Reason => CE_Range_Check_Failed));
         Set_Raises_Constraint_Error (N);
         Set_Etype (N, Typ);
      end if;
   end Rewrite_In_Raise_CE;

   ---------------------
   -- String_Type_Len --
   ---------------------

   function String_Type_Len (Stype : Entity_Id) return Uint is
      NT : constant Entity_Id := Etype (First_Index (Stype));
      T  : Entity_Id;

   begin
      if Is_OK_Static_Subtype (NT) then
         T := NT;
      else
         T := Base_Type (NT);
      end if;

      return Expr_Value (Type_High_Bound (T)) -
             Expr_Value (Type_Low_Bound (T)) + 1;
   end String_Type_Len;

   ------------------------------------
   -- Subtypes_Statically_Compatible --
   ------------------------------------

   function Subtypes_Statically_Compatible
     (T1   : Entity_Id;
      T2   : Entity_Id)
      return Boolean
   is
   begin
      if Is_Scalar_Type (T1) then

         --  Definitely compatible if we match

         if Subtypes_Statically_Match (T1, T2) then
            return True;

         --  If either subtype is nonstatic then they're not compatible

         elsif not Is_Static_Subtype (T1)
           or else not Is_Static_Subtype (T2)
         then
            return False;

         --  If either type has constraint error bounds, then consider that
         --  they match to avoid junk cascaded errors here.

         elsif not Is_OK_Static_Subtype (T1)
           or else not Is_OK_Static_Subtype (T2)
         then
            return True;

         --  Base types must match, but we don't check that (should
         --  we???) but we do at least check that both types are
         --  real, or both types are not real.

         elsif Is_Real_Type (T1) /= Is_Real_Type (T2) then
            return False;

         --  Here we check the bounds

         else
            declare
               LB1 : constant Node_Id := Type_Low_Bound  (T1);
               HB1 : constant Node_Id := Type_High_Bound (T1);
               LB2 : constant Node_Id := Type_Low_Bound  (T2);
               HB2 : constant Node_Id := Type_High_Bound (T2);

            begin
               if Is_Real_Type (T1) then
                  return
                    (Expr_Value_R (LB1) > Expr_Value_R (HB1))
                      or else
                    (Expr_Value_R (LB2) <= Expr_Value_R (LB1)
                       and then
                     Expr_Value_R (HB1) <= Expr_Value_R (HB2));

               else
                  return
                    (Expr_Value (LB1) > Expr_Value (HB1))
                      or else
                    (Expr_Value (LB2) <= Expr_Value (LB1)
                       and then
                     Expr_Value (HB1) <= Expr_Value (HB2));
               end if;
            end;
         end if;

      elsif Is_Access_Type (T1) then
         return not Is_Constrained (T2)
           or else Subtypes_Statically_Match
                     (Designated_Type (T1), Designated_Type (T2));

      else
         return (Is_Composite_Type (T1) and then not Is_Constrained (T2))
           or else Subtypes_Statically_Match (T1, T2);
      end if;
   end Subtypes_Statically_Compatible;

   -------------------------------
   -- Subtypes_Statically_Match --
   -------------------------------

   --  Subtypes statically match if they have statically matching constraints
   --  (RM 4.9.1(2)). Constraints statically match if there are none, or if
   --  they are the same identical constraint, or if they are static and the
   --  values match (RM 4.9.1(1)).

   function Subtypes_Statically_Match (T1, T2 : Entity_Id) return Boolean is
   begin
      --  A type always statically matches itself

      if T1 = T2 then
         return True;

      --  Scalar types

      elsif Is_Scalar_Type (T1) then

         --  Base types must be the same

         if Base_Type (T1) /= Base_Type (T2) then
            return False;
         end if;

         --  A constrained numeric subtype never matches an unconstrained
         --  subtype, i.e. both types must be constrained or unconstrained.

         --  To understand the requirement for this test, see RM 4.9.1(1).
         --  As is made clear in RM 3.5.4(11), type Integer, for example
         --  is a constrained subtype with constraint bounds matching the
         --  bounds of its corresponding uncontrained base type. In this
         --  situation, Integer and Integer'Base do not statically match,
         --  even though they have the same bounds.

         --  We only apply this test to types in Standard and types that
         --  appear in user programs. That way, we do not have to be
         --  too careful about setting Is_Constrained right for itypes.

         if Is_Numeric_Type (T1)
           and then (Is_Constrained (T1) /= Is_Constrained (T2))
           and then (Scope (T1) = Standard_Standard
                      or else Comes_From_Source (T1))
           and then (Scope (T2) = Standard_Standard
                      or else Comes_From_Source (T2))
         then
            return False;

         --  A generic scalar type does not statically match its base
         --  type (AI-311). In this case we make sure that the formals,
         --  which are first subtypes of their bases, are constrained.

         elsif Is_Generic_Type (T1)
           and then Is_Generic_Type (T2)
           and then (Is_Constrained (T1) /= Is_Constrained (T2))
         then
            return False;
         end if;

         --  If there was an error in either range, then just assume
         --  the types statically match to avoid further junk errors

         if Error_Posted (Scalar_Range (T1))
              or else
            Error_Posted (Scalar_Range (T2))
         then
            return True;
         end if;

         --  Otherwise both types have bound that can be compared

         declare
            LB1 : constant Node_Id := Type_Low_Bound  (T1);
            HB1 : constant Node_Id := Type_High_Bound (T1);
            LB2 : constant Node_Id := Type_Low_Bound  (T2);
            HB2 : constant Node_Id := Type_High_Bound (T2);

         begin
            --  If the bounds are the same tree node, then match

            if LB1 = LB2 and then HB1 = HB2 then
               return True;

            --  Otherwise bounds must be static and identical value

            else
               if not Is_Static_Subtype (T1)
                 or else not Is_Static_Subtype (T2)
               then
                  return False;

               --  If either type has constraint error bounds, then say
               --  that they match to avoid junk cascaded errors here.

               elsif not Is_OK_Static_Subtype (T1)
                 or else not Is_OK_Static_Subtype (T2)
               then
                  return True;

               elsif Is_Real_Type (T1) then
                  return
                    (Expr_Value_R (LB1) = Expr_Value_R (LB2))
                      and then
                    (Expr_Value_R (HB1) = Expr_Value_R (HB2));

               else
                  return
                    Expr_Value (LB1) = Expr_Value (LB2)
                      and then
                    Expr_Value (HB1) = Expr_Value (HB2);
               end if;
            end if;
         end;

      --  Type with discriminants

      elsif Has_Discriminants (T1) or else Has_Discriminants (T2) then
         if Has_Discriminants (T1) /= Has_Discriminants (T2) then
            return False;
         end if;

         declare
            DL1 : constant Elist_Id := Discriminant_Constraint (T1);
            DL2 : constant Elist_Id := Discriminant_Constraint (T2);

            DA1 : Elmt_Id := First_Elmt (DL1);
            DA2 : Elmt_Id := First_Elmt (DL2);

         begin
            if DL1 = DL2 then
               return True;

            elsif Is_Constrained (T1) /= Is_Constrained (T2) then
               return False;
            end if;

            while Present (DA1) loop
               declare
                  Expr1 : constant Node_Id := Node (DA1);
                  Expr2 : constant Node_Id := Node (DA2);

               begin
                  if not Is_Static_Expression (Expr1)
                    or else not Is_Static_Expression (Expr2)
                  then
                     return False;

                  --  If either expression raised a constraint error,
                  --  consider the expressions as matching, since this
                  --  helps to prevent cascading errors.

                  elsif Raises_Constraint_Error (Expr1)
                    or else Raises_Constraint_Error (Expr2)
                  then
                     null;

                  elsif Expr_Value (Expr1) /= Expr_Value (Expr2) then
                     return False;
                  end if;
               end;

               Next_Elmt (DA1);
               Next_Elmt (DA2);
            end loop;
         end;

         return True;

      --  A definite type does not match an indefinite or classwide type

      elsif
         Has_Unknown_Discriminants (T1) /= Has_Unknown_Discriminants (T2)
      then
         return False;

      --  Array type

      elsif Is_Array_Type (T1) then

         --  If either subtype is unconstrained then both must be,
         --  and if both are unconstrained then no further checking
         --  is needed.

         if not Is_Constrained (T1) or else not Is_Constrained (T2) then
            return not (Is_Constrained (T1) or else Is_Constrained (T2));
         end if;

         --  Both subtypes are constrained, so check that the index
         --  subtypes statically match.

         declare
            Index1 : Node_Id := First_Index (T1);
            Index2 : Node_Id := First_Index (T2);

         begin
            while Present (Index1) loop
               if not
                 Subtypes_Statically_Match (Etype (Index1), Etype (Index2))
               then
                  return False;
               end if;

               Next_Index (Index1);
               Next_Index (Index2);
            end loop;

            return True;
         end;

      elsif Is_Access_Type (T1) then
         return Subtypes_Statically_Match
                  (Designated_Type (T1),
                   Designated_Type (T2));

      --  All other types definitely match

      else
         return True;
      end if;
   end Subtypes_Statically_Match;

   ----------
   -- Test --
   ----------

   function Test (Cond : Boolean) return Uint is
   begin
      if Cond then
         return Uint_1;
      else
         return Uint_0;
      end if;
   end Test;

   ---------------------------------
   -- Test_Expression_Is_Foldable --
   ---------------------------------

   --  One operand case

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean)
   is
   begin
      Stat := False;

      --  If operand is Any_Type, just propagate to result and do not
      --  try to fold, this prevents cascaded errors.

      if Etype (Op1) = Any_Type then
         Set_Etype (N, Any_Type);
         Fold := False;
         return;

      --  If operand raises constraint error, then replace node N with the
      --  raise constraint error node, and we are obviously not foldable.
      --  Note that this replacement inherits the Is_Static_Expression flag
      --  from the operand.

      elsif Raises_Constraint_Error (Op1) then
         Rewrite_In_Raise_CE (N, Op1);
         Fold := False;
         return;

      --  If the operand is not static, then the result is not static, and
      --  all we have to do is to check the operand since it is now known
      --  to appear in a non-static context.

      elsif not Is_Static_Expression (Op1) then
         Check_Non_Static_Context (Op1);
         Fold := Compile_Time_Known_Value (Op1);
         return;

      --   An expression of a formal modular type is not foldable because
      --   the modulus is unknown.

      elsif Is_Modular_Integer_Type (Etype (Op1))
        and then Is_Generic_Type (Etype (Op1))
      then
         Check_Non_Static_Context (Op1);
         Fold := False;
         return;

      --  Here we have the case of an operand whose type is OK, which is
      --  static, and which does not raise constraint error, we can fold.

      else
         Set_Is_Static_Expression (N);
         Fold := True;
         Stat := True;
      end if;
   end Test_Expression_Is_Foldable;

   --  Two operand case

   procedure Test_Expression_Is_Foldable
     (N    : Node_Id;
      Op1  : Node_Id;
      Op2  : Node_Id;
      Stat : out Boolean;
      Fold : out Boolean)
   is
      Rstat : constant Boolean := Is_Static_Expression (Op1)
                                    and then Is_Static_Expression (Op2);

   begin
      Stat := False;

      --  If either operand is Any_Type, just propagate to result and
      --  do not try to fold, this prevents cascaded errors.

      if Etype (Op1) = Any_Type or else Etype (Op2) = Any_Type then
         Set_Etype (N, Any_Type);
         Fold := False;
         return;

      --  If left operand raises constraint error, then replace node N with
      --  the raise constraint error node, and we are obviously not foldable.
      --  Is_Static_Expression is set from the two operands in the normal way,
      --  and we check the right operand if it is in a non-static context.

      elsif Raises_Constraint_Error (Op1) then
         if not Rstat then
            Check_Non_Static_Context (Op2);
         end if;

         Rewrite_In_Raise_CE (N, Op1);
         Set_Is_Static_Expression (N, Rstat);
         Fold := False;
         return;

      --  Similar processing for the case of the right operand. Note that
      --  we don't use this routine for the short-circuit case, so we do
      --  not have to worry about that special case here.

      elsif Raises_Constraint_Error (Op2) then
         if not Rstat then
            Check_Non_Static_Context (Op1);
         end if;

         Rewrite_In_Raise_CE (N, Op2);
         Set_Is_Static_Expression (N, Rstat);
         Fold := False;
         return;

      --  Exclude expressions of a generic modular type, as above

      elsif Is_Modular_Integer_Type (Etype (Op1))
        and then Is_Generic_Type (Etype (Op1))
      then
         Check_Non_Static_Context (Op1);
         Fold := False;
         return;

      --  If result is not static, then check non-static contexts on operands
      --  since one of them may be static and the other one may not be static

      elsif not Rstat then
         Check_Non_Static_Context (Op1);
         Check_Non_Static_Context (Op2);
         Fold := Compile_Time_Known_Value (Op1)
                   and then Compile_Time_Known_Value (Op2);
         return;

      --  Else result is static and foldable. Both operands are static,
      --  and neither raises constraint error, so we can definitely fold.

      else
         Set_Is_Static_Expression (N);
         Fold := True;
         Stat := True;
         return;
      end if;
   end Test_Expression_Is_Foldable;

   --------------
   -- To_Bits --
   --------------

   procedure To_Bits (U : Uint; B : out Bits) is
   begin
      for J in 0 .. B'Last loop
         B (J) := (U / (2 ** J)) mod 2 /= 0;
      end loop;
   end To_Bits;

   --------------------
   -- Why_Not_Static --
   --------------------

   procedure Why_Not_Static (Expr : Node_Id) is
      N   : constant Node_Id   := Original_Node (Expr);
      Typ : Entity_Id;
      E   : Entity_Id;

      procedure Why_Not_Static_List (L : List_Id);
      --  A version that can be called on a list of expressions. Finds
      --  all non-static violations in any element of the list.

      -------------------------
      -- Why_Not_Static_List --
      -------------------------

      procedure Why_Not_Static_List (L : List_Id) is
         N : Node_Id;

      begin
         if Is_Non_Empty_List (L) then
            N := First (L);
            while Present (N) loop
               Why_Not_Static (N);
               Next (N);
            end loop;
         end if;
      end Why_Not_Static_List;

   --  Start of processing for Why_Not_Static

   begin
      --  If in ACATS mode (debug flag 2), then suppress all these
      --  messages, this avoids massive updates to the ACATS base line.

      if Debug_Flag_2 then
         return;
      end if;

      --  Ignore call on error or empty node

      if No (Expr) or else Nkind (Expr) = N_Error then
         return;
      end if;

      --  Preprocessing for sub expressions

      if Nkind (Expr) in N_Subexpr then

         --  Nothing to do if expression is static

         if Is_OK_Static_Expression (Expr) then
            return;
         end if;

         --  Test for constraint error raised

         if Raises_Constraint_Error (Expr) then
            Error_Msg_N
              ("expression raises exception, cannot be static " &
               "('R'M 4.9(34))!", N);
            return;
         end if;

         --  If no type, then something is pretty wrong, so ignore

         Typ := Etype (Expr);

         if No (Typ) then
            return;
         end if;

         --  Type must be scalar or string type

         if not Is_Scalar_Type (Typ)
           and then not Is_String_Type (Typ)
         then
            Error_Msg_N
              ("static expression must have scalar or string type " &
               "('R'M 4.9(2))!", N);
            return;
         end if;
      end if;

      --  If we got through those checks, test particular node kind

      case Nkind (N) is
         when N_Expanded_Name | N_Identifier | N_Operator_Symbol =>
            E := Entity (N);

            if Is_Named_Number (E) then
               null;

            elsif Ekind (E) = E_Constant then
               if not Is_Static_Expression (Constant_Value (E)) then
                  Error_Msg_NE
                    ("& is not a static constant ('R'M 4.9(5))!", N, E);
               end if;

            else
               Error_Msg_NE
                 ("& is not static constant or named number " &
                  "('R'M 4.9(5))!", N, E);
            end if;

         when N_Binary_Op | N_And_Then | N_Or_Else | N_In | N_Not_In =>
            if Nkind (N) in N_Op_Shift then
               Error_Msg_N
                ("shift functions are never static ('R'M 4.9(6,18))!", N);

            else
               Why_Not_Static (Left_Opnd (N));
               Why_Not_Static (Right_Opnd (N));
            end if;

         when N_Unary_Op =>
            Why_Not_Static (Right_Opnd (N));

         when N_Attribute_Reference =>
            Why_Not_Static_List (Expressions (N));

            E := Etype (Prefix (N));

            if E = Standard_Void_Type then
               return;
            end if;

            --  Special case non-scalar'Size since this is a common error

            if Attribute_Name (N) = Name_Size then
               Error_Msg_N
                 ("size attribute is only static for scalar type " &
                  "('R'M 4.9(7,8))", N);

            --  Flag array cases

            elsif Is_Array_Type (E) then
               if Attribute_Name (N) /= Name_First
                    and then
                  Attribute_Name (N) /= Name_Last
                    and then
                  Attribute_Name (N) /= Name_Length
               then
                  Error_Msg_N
                    ("static array attribute must be Length, First, or Last " &
                     "('R'M 4.9(8))!", N);

               --  Since we know the expression is not-static (we already
               --  tested for this, must mean array is not static).

               else
                  Error_Msg_N
                    ("prefix is non-static array ('R'M 4.9(8))!", Prefix (N));
               end if;

               return;

            --  Special case generic types, since again this is a common
            --  source of confusion.

            elsif Is_Generic_Actual_Type (E)
                    or else
                  Is_Generic_Type (E)
            then
               Error_Msg_N
                 ("attribute of generic type is never static " &
                  "('R'M 4.9(7,8))!", N);

            elsif Is_Static_Subtype (E) then
               null;

            elsif Is_Scalar_Type (E) then
               Error_Msg_N
                 ("prefix type for attribute is not static scalar subtype " &
                  "('R'M 4.9(7))!", N);

            else
               Error_Msg_N
                 ("static attribute must apply to array/scalar type " &
                  "('R'M 4.9(7,8))!", N);
            end if;

         when N_String_Literal =>
            Error_Msg_N
              ("subtype of string literal is non-static ('R'M 4.9(4))!", N);

         when N_Explicit_Dereference =>
            Error_Msg_N
              ("explicit dereference is never static ('R'M 4.9)!", N);

         when N_Function_Call =>
            Why_Not_Static_List (Parameter_Associations (N));
            Error_Msg_N ("non-static function call ('R'M 4.9(6,18))!", N);

         when N_Parameter_Association =>
            Why_Not_Static (Explicit_Actual_Parameter (N));

         when N_Indexed_Component =>
            Error_Msg_N
              ("indexed component is never static ('R'M 4.9)!", N);

         when N_Procedure_Call_Statement =>
            Error_Msg_N
              ("procedure call is never static ('R'M 4.9)!", N);

         when N_Qualified_Expression =>
            Why_Not_Static (Expression (N));

         when N_Aggregate | N_Extension_Aggregate =>
            Error_Msg_N
              ("an aggregate is never static ('R'M 4.9)!", N);

         when N_Range =>
            Why_Not_Static (Low_Bound (N));
            Why_Not_Static (High_Bound (N));

         when N_Range_Constraint =>
            Why_Not_Static (Range_Expression (N));

         when N_Subtype_Indication =>
            Why_Not_Static (Constraint (N));

         when N_Selected_Component =>
            Error_Msg_N
              ("selected component is never static ('R'M 4.9)!", N);

         when N_Slice =>
            Error_Msg_N
              ("slice is never static ('R'M 4.9)!", N);

         when N_Type_Conversion =>
            Why_Not_Static (Expression (N));

            if not Is_Scalar_Type (Etype (Prefix (N)))
              or else not Is_Static_Subtype (Etype (Prefix (N)))
            then
               Error_Msg_N
                 ("static conversion requires static scalar subtype result " &
                  "('R'M 4.9(9))!", N);
            end if;

         when N_Unchecked_Type_Conversion =>
            Error_Msg_N
              ("unchecked type conversion is never static ('R'M 4.9)!", N);

         when others =>
            null;

      end case;
   end Why_Not_Static;

end Sem_Eval;
