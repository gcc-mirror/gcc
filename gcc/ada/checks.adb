------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
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
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Pakd; use Exp_Pakd;
with Exp_Util; use Exp_Util;
with Elists;   use Elists;
with Eval_Fat; use Eval_Fat;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Sprint;   use Sprint;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Urealp;   use Urealp;
with Validsw;  use Validsw;

package body Checks is

   --  General note: many of these routines are concerned with generating
   --  checking code to make sure that constraint error is raised at runtime.
   --  Clearly this code is only needed if the expander is active, since
   --  otherwise we will not be generating code or going into the runtime
   --  execution anyway.

   --  We therefore disconnect most of these checks if the expander is
   --  inactive. This has the additional benefit that we do not need to
   --  worry about the tree being messed up by previous errors (since errors
   --  turn off expansion anyway).

   --  There are a few exceptions to the above rule. For instance routines
   --  such as Apply_Scalar_Range_Check that do not insert any code can be
   --  safely called even when the Expander is inactive (but Errors_Detected
   --  is 0). The benefit of executing this code when expansion is off, is
   --  the ability to emit constraint error warning for static expressions
   --  even when we are not generating code.

   -------------------------------------
   -- Suppression of Redundant Checks --
   -------------------------------------

   --  This unit implements a limited circuit for removal of redundant
   --  checks. The processing is based on a tracing of simple sequential
   --  flow. For any sequence of statements, we save expressions that are
   --  marked to be checked, and then if the same expression appears later
   --  with the same check, then under certain circumstances, the second
   --  check can be suppressed.

   --  Basically, we can suppress the check if we know for certain that
   --  the previous expression has been elaborated (together with its
   --  check), and we know that the exception frame is the same, and that
   --  nothing has happened to change the result of the exception.

   --  Let us examine each of these three conditions in turn to describe
   --  how we ensure that this condition is met.

   --  First, we need to know for certain that the previous expression has
   --  been executed. This is done principly by the mechanism of calling
   --  Conditional_Statements_Begin at the start of any statement sequence
   --  and Conditional_Statements_End at the end. The End call causes all
   --  checks remembered since the Begin call to be discarded. This does
   --  miss a few cases, notably the case of a nested BEGIN-END block with
   --  no exception handlers. But the important thing is to be conservative.
   --  The other protection is that all checks are discarded if a label
   --  is encountered, since then the assumption of sequential execution
   --  is violated, and we don't know enough about the flow.

   --  Second, we need to know that the exception frame is the same. We
   --  do this by killing all remembered checks when we enter a new frame.
   --  Again, that's over-conservative, but generally the cases we can help
   --  with are pretty local anyway (like the body of a loop for example).

   --  Third, we must be sure to forget any checks which are no longer valid.
   --  This is done by two mechanisms, first the Kill_Checks_Variable call is
   --  used to note any changes to local variables. We only attempt to deal
   --  with checks involving local variables, so we do not need to worry
   --  about global variables. Second, a call to any non-global procedure
   --  causes us to abandon all stored checks, since such a all may affect
   --  the values of any local variables.

   --  The following define the data structures used to deal with remembering
   --  checks so that redundant checks can be eliminated as described above.

   --  Right now, the only expressions that we deal with are of the form of
   --  simple local objects (either declared locally, or IN parameters) or
   --  such objects plus/minus a compile time known constant. We can do
   --  more later on if it seems worthwhile, but this catches many simple
   --  cases in practice.

   --  The following record type reflects a single saved check. An entry
   --  is made in the stack of saved checks if and only if the expression
   --  has been elaborated with the indicated checks.

   type Saved_Check is record
      Killed : Boolean;
      --  Set True if entry is killed by Kill_Checks

      Entity : Entity_Id;
      --  The entity involved in the expression that is checked

      Offset : Uint;
      --  A compile time value indicating the result of adding or
      --  subtracting a compile time value. This value is to be
      --  added to the value of the Entity. A value of zero is
      --  used for the case of a simple entity reference.

      Check_Type : Character;
      --  This is set to 'R' for a range check (in which case Target_Type
      --  is set to the target type for the range check) or to 'O' for an
      --  overflow check (in which case Target_Type is set to Empty).

      Target_Type : Entity_Id;
      --  Used only if Do_Range_Check is set. Records the target type for
      --  the check. We need this, because a check is a duplicate only if
      --  it has a the same target type (or more accurately one with a
      --  range that is smaller or equal to the stored target type of a
      --  saved check).
   end record;

   --  The following table keeps track of saved checks. Rather than use an
   --  extensible table. We just use a table of fixed size, and we discard
   --  any saved checks that do not fit. That's very unlikely to happen and
   --  this is only an optimization in any case.

   Saved_Checks : array (Int range 1 .. 200) of Saved_Check;
   --  Array of saved checks

   Num_Saved_Checks : Nat := 0;
   --  Number of saved checks

   --  The following stack keeps track of statement ranges. It is treated
   --  as a stack. When Conditional_Statements_Begin is called, an entry
   --  is pushed onto this stack containing the value of Num_Saved_Checks
   --  at the time of the call. Then when Conditional_Statements_End is
   --  called, this value is popped off and used to reset Num_Saved_Checks.

   --  Note: again, this is a fixed length stack with a size that should
   --  always be fine. If the value of the stack pointer goes above the
   --  limit, then we just forget all saved checks.

   Saved_Checks_Stack : array (Int range 1 .. 100) of Nat;
   Saved_Checks_TOS : Nat := 0;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Apply_Float_Conversion_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id);
   --  The checks on a conversion from a floating-point type to an integer
   --  type are delicate. They have to be performed before conversion, they
   --  have to raise an exception when the operand is a NaN, and rounding must
   --  be taken into account to determine the safe bounds of the operand.

   procedure Apply_Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean);
   --  This is the subprogram that does all the work for Apply_Length_Check
   --  and Apply_Static_Length_Check. Expr, Target_Typ and Source_Typ are as
   --  described for the above routines. The Do_Static flag indicates that
   --  only a static check is to be done.

   procedure Apply_Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean);
   --  This is the subprogram that does all the work for Apply_Range_Check.
   --  Expr, Target_Typ and Source_Typ are as described for the above
   --  routine. The Do_Static flag indicates that only a static check is
   --  to be done.

   type Check_Type is (Access_Check, Division_Check);
   function Check_Needed (Nod : Node_Id; Check : Check_Type) return Boolean;
   --  This function is used to see if an access or division by zero check is
   --  needed. The check is to be applied to a single variable appearing in the
   --  source, and N is the node for the reference. If N is not of this form,
   --  True is returned with no further processing. If N is of the right form,
   --  then further processing determines if the given Check is needed.
   --
   --  The particular circuit is to see if we have the case of a check that is
   --  not needed because it appears in the right operand of a short circuited
   --  conditional where the left operand guards the check. For example:
   --
   --    if Var = 0 or else Q / Var > 12 then
   --       ...
   --    end if;
   --
   --  In this example, the division check is not required. At the same time
   --  we can issue warnings for suspicious use of non-short-circuited forms,
   --  such as:
   --
   --    if Var = 0 or Q / Var > 12 then
   --       ...
   --    end if;

   procedure Find_Check
     (Expr        : Node_Id;
      Check_Type  : Character;
      Target_Type : Entity_Id;
      Entry_OK    : out Boolean;
      Check_Num   : out Nat;
      Ent         : out Entity_Id;
      Ofs         : out Uint);
   --  This routine is used by Enable_Range_Check and Enable_Overflow_Check
   --  to see if a check is of the form for optimization, and if so, to see
   --  if it has already been performed. Expr is the expression to check,
   --  and Check_Type is 'R' for a range check, 'O' for an overflow check.
   --  Target_Type is the target type for a range check, and Empty for an
   --  overflow check. If the entry is not of the form for optimization,
   --  then Entry_OK is set to False, and the remaining out parameters
   --  are undefined. If the entry is OK, then Ent/Ofs are set to the
   --  entity and offset from the expression. Check_Num is the number of
   --  a matching saved entry in Saved_Checks, or zero if no such entry
   --  is located.

   function Get_Discriminal (E : Entity_Id; Bound : Node_Id) return Node_Id;
   --  If a discriminal is used in constraining a prival, Return reference
   --  to the discriminal of the protected body (which renames the parameter
   --  of the enclosing protected operation). This clumsy transformation is
   --  needed because privals are created too late and their actual subtypes
   --  are not available when analysing the bodies of the protected operations.
   --  To be cleaned up???

   function Guard_Access
     (Cond    : Node_Id;
      Loc     : Source_Ptr;
      Ck_Node : Node_Id) return Node_Id;
   --  In the access type case, guard the test with a test to ensure
   --  that the access value is non-null, since the checks do not
   --  not apply to null access values.

   procedure Install_Static_Check (R_Cno : Node_Id; Loc : Source_Ptr);
   --  Called by Apply_{Length,Range}_Checks to rewrite the tree with the
   --  Constraint_Error node.

   function Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id) return Check_Result;
   --  Like Apply_Selected_Length_Checks, except it doesn't modify
   --  anything, just returns a list of nodes as described in the spec of
   --  this package for the Range_Check function.

   function Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id) return Check_Result;
   --  Like Apply_Selected_Range_Checks, except it doesn't modify anything,
   --  just returns a list of nodes as described in the spec of this package
   --  for the Range_Check function.

   ------------------------------
   -- Access_Checks_Suppressed --
   ------------------------------

   function Access_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Access_Check);
      else
         return Scope_Suppress (Access_Check);
      end if;
   end Access_Checks_Suppressed;

   -------------------------------------
   -- Accessibility_Checks_Suppressed --
   -------------------------------------

   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Accessibility_Check);
      else
         return Scope_Suppress (Accessibility_Check);
      end if;
   end Accessibility_Checks_Suppressed;

   -------------------------
   -- Append_Range_Checks --
   -------------------------

   procedure Append_Range_Checks
     (Checks       : Check_Result;
      Stmts        : List_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr;
      Flag_Node    : Node_Id)
   is
      Internal_Flag_Node   : constant Node_Id    := Flag_Node;
      Internal_Static_Sloc : constant Source_Ptr := Static_Sloc;

      Checks_On : constant Boolean :=
                    (not Index_Checks_Suppressed (Suppress_Typ))
                       or else
                    (not Range_Checks_Suppressed (Suppress_Typ));

   begin
      --  For now we just return if Checks_On is false, however this should
      --  be enhanced to check for an always True value in the condition
      --  and to generate a compilation warning???

      if not Checks_On then
         return;
      end if;

      for J in 1 .. 2 loop
         exit when No (Checks (J));

         if Nkind (Checks (J)) = N_Raise_Constraint_Error
           and then Present (Condition (Checks (J)))
         then
            if not Has_Dynamic_Range_Check (Internal_Flag_Node) then
               Append_To (Stmts, Checks (J));
               Set_Has_Dynamic_Range_Check (Internal_Flag_Node);
            end if;

         else
            Append_To
              (Stmts,
                Make_Raise_Constraint_Error (Internal_Static_Sloc,
                  Reason => CE_Range_Check_Failed));
         end if;
      end loop;
   end Append_Range_Checks;

   ------------------------
   -- Apply_Access_Check --
   ------------------------

   procedure Apply_Access_Check (N : Node_Id) is
      P : constant Node_Id := Prefix (N);

   begin
      if Inside_A_Generic then
         return;
      end if;

      if Is_Entity_Name (P) then
         Check_Unset_Reference (P);
      end if;

      --  We do not need access checks if prefix is known to be non-null

      if Known_Non_Null (P) then
         return;

      --  We do not need access checks if they are suppressed on the type

      elsif Access_Checks_Suppressed (Etype (P)) then
         return;

      --  We do not need checks if we are not generating code (i.e. the
      --  expander is not active). This is not just an optimization, there
      --  are cases (e.g. with pragma Debug) where generating the checks
      --  can cause real trouble).

      elsif not Expander_Active then
         return;

      --  We do not need checks if not needed because of short circuiting

      elsif not Check_Needed (P, Access_Check) then
         return;
      end if;

      --  Case where P is an entity name

      if Is_Entity_Name (P) then
         declare
            Ent : constant Entity_Id := Entity (P);

         begin
            if Access_Checks_Suppressed (Ent) then
               return;
            end if;

            --  Otherwise we are going to generate an access check, and
            --  are we have done it, the entity will now be known non null
            --  But we have to check for safe sequential semantics here!

            if Safe_To_Capture_Value (N, Ent) then
               Set_Is_Known_Non_Null (Ent);
            end if;
         end;
      end if;

      --  Access check is required

      Install_Null_Excluding_Check (P);
   end Apply_Access_Check;

   -------------------------------
   -- Apply_Accessibility_Check --
   -------------------------------

   procedure Apply_Accessibility_Check (N : Node_Id; Typ : Entity_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Param_Ent   : constant Entity_Id  := Param_Entity (N);
      Param_Level : Node_Id;
      Type_Level  : Node_Id;

   begin
      if Inside_A_Generic then
         return;

      --  Only apply the run-time check if the access parameter
      --  has an associated extra access level parameter and
      --  when the level of the type is less deep than the level
      --  of the access parameter.

      elsif Present (Param_Ent)
         and then Present (Extra_Accessibility (Param_Ent))
         and then UI_Gt (Object_Access_Level (N),
                         Type_Access_Level (Typ))
         and then not Accessibility_Checks_Suppressed (Param_Ent)
         and then not Accessibility_Checks_Suppressed (Typ)
      then
         Param_Level :=
           New_Occurrence_Of (Extra_Accessibility (Param_Ent), Loc);

         Type_Level :=
           Make_Integer_Literal (Loc, Type_Access_Level (Typ));

         --  Raise Program_Error if the accessibility level of the
         --  the access parameter is deeper than the level of the
         --  target access type.

         Insert_Action (N,
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => Param_Level,
                 Right_Opnd => Type_Level),
             Reason => PE_Accessibility_Check_Failed));

         Analyze_And_Resolve (N);
      end if;
   end Apply_Accessibility_Check;

   ---------------------------
   -- Apply_Alignment_Check --
   ---------------------------

   procedure Apply_Alignment_Check (E : Entity_Id; N : Node_Id) is
      AC   : constant Node_Id   := Address_Clause (E);
      Typ  : constant Entity_Id := Etype (E);
      Expr : Node_Id;
      Loc  : Source_Ptr;

      Alignment_Required : constant Boolean := Maximum_Alignment > 1;
      --  Constant to show whether target requires alignment checks

   begin
      --  See if check needed. Note that we never need a check if the
      --  maximum alignment is one, since the check will always succeed

      if No (AC)
        or else not Check_Address_Alignment (AC)
        or else not Alignment_Required
      then
         return;
      end if;

      Loc  := Sloc (AC);
      Expr := Expression (AC);

      if Nkind (Expr) = N_Unchecked_Type_Conversion then
         Expr := Expression (Expr);

      elsif Nkind (Expr) = N_Function_Call
        and then Is_Entity_Name (Name (Expr))
        and then Is_RTE (Entity (Name (Expr)), RE_To_Address)
      then
         Expr := First (Parameter_Associations (Expr));

         if Nkind (Expr) = N_Parameter_Association then
            Expr := Explicit_Actual_Parameter (Expr);
         end if;
      end if;

      --  Here Expr is the address value. See if we know that the
      --  value is unacceptable at compile time.

      if Compile_Time_Known_Value (Expr)
        and then (Known_Alignment (E) or else Known_Alignment (Typ))
      then
         declare
            AL : Uint := Alignment (Typ);

         begin
            --  The object alignment might be more restrictive than the
            --  type alignment.

            if Known_Alignment (E) then
               AL := Alignment (E);
            end if;

            if Expr_Value (Expr) mod AL /= 0 then
               Insert_Action (N,
                  Make_Raise_Program_Error (Loc,
                    Reason => PE_Misaligned_Address_Value));
               Error_Msg_NE
                 ("?specified address for& not " &
                  "consistent with alignment ('R'M 13.3(27))", Expr, E);
            end if;
         end;

      --  Here we do not know if the value is acceptable, generate
      --  code to raise PE if alignment is inappropriate.

      else
         --  Skip generation of this code if we don't want elab code

         if not Restriction_Active (No_Elaboration_Code) then
            Insert_After_And_Analyze (N,
              Make_Raise_Program_Error (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd =>
                      Make_Op_Mod (Loc,
                        Left_Opnd =>
                          Unchecked_Convert_To
                           (RTE (RE_Integer_Address),
                            Duplicate_Subexpr_No_Checks (Expr)),
                        Right_Opnd =>
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Occurrence_Of (E, Loc),
                            Attribute_Name => Name_Alignment)),
                    Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),
                Reason => PE_Misaligned_Address_Value),
              Suppress => All_Checks);
         end if;
      end if;

      return;

   exception
      when RE_Not_Available =>
         return;
   end Apply_Alignment_Check;

   -------------------------------------
   -- Apply_Arithmetic_Overflow_Check --
   -------------------------------------

   --  This routine is called only if the type is an integer type, and
   --  a software arithmetic overflow check must be performed for op
   --  (add, subtract, multiply). The check is performed only if
   --  Software_Overflow_Checking is enabled and Do_Overflow_Check
   --  is set. In this case we expand the operation into a more complex
   --  sequence of tests that ensures that overflow is properly caught.

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Rtyp  : constant Entity_Id  := Root_Type (Typ);
      Siz   : constant Int        := UI_To_Int (Esize (Rtyp));
      Dsiz  : constant Int        := Siz * 2;
      Opnod : Node_Id;
      Ctyp  : Entity_Id;
      Opnd  : Node_Id;
      Cent  : RE_Id;

   begin
      --  Skip this if overflow checks are done in back end, or the overflow
      --  flag is not set anyway, or we are not doing code expansion.

      if Backend_Overflow_Checks_On_Target
        or else not Do_Overflow_Check (N)
        or else not Expander_Active
      then
         return;
      end if;

      --  Otherwise, we generate the full general code for front end overflow
      --  detection, which works by doing arithmetic in a larger type:

      --    x op y

      --  is expanded into

      --    Typ (Checktyp (x) op Checktyp (y));

      --  where Typ is the type of the original expression, and Checktyp is
      --  an integer type of sufficient length to hold the largest possible
      --  result.

      --  In the case where check type exceeds the size of Long_Long_Integer,
      --  we use a different approach, expanding to:

      --    typ (xxx_With_Ovflo_Check (Integer_64 (x), Integer (y)))

      --  where xxx is Add, Multiply or Subtract as appropriate

      --  Find check type if one exists

      if Dsiz <= Standard_Integer_Size then
         Ctyp := Standard_Integer;

      elsif Dsiz <= Standard_Long_Long_Integer_Size then
         Ctyp := Standard_Long_Long_Integer;

      --  No check type exists, use runtime call

      else
         if Nkind (N) = N_Op_Add then
            Cent := RE_Add_With_Ovflo_Check;

         elsif Nkind (N) = N_Op_Multiply then
            Cent := RE_Multiply_With_Ovflo_Check;

         else
            pragma Assert (Nkind (N) = N_Op_Subtract);
            Cent := RE_Subtract_With_Ovflo_Check;
         end if;

         Rewrite (N,
           OK_Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (Cent), Loc),
               Parameter_Associations => New_List (
                 OK_Convert_To (RTE (RE_Integer_64), Left_Opnd  (N)),
                 OK_Convert_To (RTE (RE_Integer_64), Right_Opnd (N))))));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  If we fall through, we have the case where we do the arithmetic in
      --  the next higher type and get the check by conversion. In these cases
      --  Ctyp is set to the type to be used as the check type.

      Opnod := Relocate_Node (N);

      Opnd := OK_Convert_To (Ctyp, Left_Opnd (Opnod));

      Analyze (Opnd);
      Set_Etype (Opnd, Ctyp);
      Set_Analyzed (Opnd, True);
      Set_Left_Opnd (Opnod, Opnd);

      Opnd := OK_Convert_To (Ctyp, Right_Opnd (Opnod));

      Analyze (Opnd);
      Set_Etype (Opnd, Ctyp);
      Set_Analyzed (Opnd, True);
      Set_Right_Opnd (Opnod, Opnd);

      --  The type of the operation changes to the base type of the check
      --  type, and we reset the overflow check indication, since clearly
      --  no overflow is possible now that we are using a double length
      --  type. We also set the Analyzed flag to avoid a recursive attempt
      --  to expand the node.

      Set_Etype             (Opnod, Base_Type (Ctyp));
      Set_Do_Overflow_Check (Opnod, False);
      Set_Analyzed          (Opnod, True);

      --  Now build the outer conversion

      Opnd := OK_Convert_To (Typ, Opnod);
      Analyze (Opnd);
      Set_Etype (Opnd, Typ);

      --  In the discrete type case, we directly generate the range check
      --  for the outer operand. This range check will implement the required
      --  overflow check.

      if Is_Discrete_Type (Typ) then
         Rewrite (N, Opnd);
         Generate_Range_Check (Expression (N), Typ, CE_Overflow_Check_Failed);

      --  For other types, we enable overflow checking on the conversion,
      --  after setting the node as analyzed to prevent recursive attempts
      --  to expand the conversion node.

      else
         Set_Analyzed (Opnd, True);
         Enable_Overflow_Check (Opnd);
         Rewrite (N, Opnd);
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Apply_Arithmetic_Overflow_Check;

   ----------------------------
   -- Apply_Array_Size_Check --
   ----------------------------

   --  The situation is as follows. In GNAT 3 (GCC 2.x), the size in bits
   --  is computed in 32 bits without an overflow check. That's a real
   --  problem for Ada. So what we do in GNAT 3 is to approximate the
   --  size of an array by manually multiplying the element size by the
   --  number of elements, and comparing that against the allowed limits.

   --  In GNAT 5, the size in byte is still computed in 32 bits without
   --  an overflow check in the dynamic case, but the size in bits is
   --  computed in 64 bits. We assume that's good enough, and we do not
   --  bother to generate any front end test.

   procedure Apply_Array_Size_Check (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Ctyp : constant Entity_Id  := Component_Type (Typ);
      Ent  : constant Entity_Id  := Defining_Identifier (N);
      Decl : Node_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      Lob  : Uint;
      Hib  : Uint;
      Siz  : Uint;
      Xtyp : Entity_Id;
      Indx : Node_Id;
      Sizx : Node_Id;
      Code : Node_Id;

      Static : Boolean := True;
      --  Set false if any index subtye bound is non-static

      Umark : constant Uintp.Save_Mark := Uintp.Mark;
      --  We can throw away all the Uint computations here, since they are
      --  done only to generate boolean test results.

      Check_Siz : Uint;
      --  Size to check against

      function Is_Address_Or_Import (Decl : Node_Id) return Boolean;
      --  Determines if Decl is an address clause or Import/Interface pragma
      --  that references the defining identifier of the current declaration.

      --------------------------
      -- Is_Address_Or_Import --
      --------------------------

      function Is_Address_Or_Import (Decl : Node_Id) return Boolean is
      begin
         if Nkind (Decl) = N_At_Clause then
            return Chars (Identifier (Decl)) = Chars (Ent);

         elsif Nkind (Decl) = N_Attribute_Definition_Clause then
            return
              Chars (Decl) = Name_Address
                and then
              Nkind (Name (Decl)) = N_Identifier
                and then
              Chars (Name (Decl)) = Chars (Ent);

         elsif Nkind (Decl) = N_Pragma then
            if (Chars (Decl) = Name_Import
                 or else
                Chars (Decl) = Name_Interface)
              and then Present (Pragma_Argument_Associations (Decl))
            then
               declare
                  F : constant Node_Id :=
                        First (Pragma_Argument_Associations (Decl));

               begin
                  return
                    Present (F)
                      and then
                    Present (Next (F))
                      and then
                    Nkind (Expression (Next (F))) = N_Identifier
                      and then
                    Chars (Expression (Next (F))) = Chars (Ent);
               end;

            else
               return False;
            end if;

         else
            return False;
         end if;
      end Is_Address_Or_Import;

   --  Start of processing for Apply_Array_Size_Check

   begin
      --  Do size check on local arrays. We only need this in the GCC 2
      --  case, since in GCC 3, we expect the back end to properly handle
      --  things. This routine can be removed when we baseline GNAT 3.

      if Opt.GCC_Version >= 3 then
         return;
      end if;

      --  No need for a check if not expanding

      if not Expander_Active then
         return;
      end if;

      --  No need for a check if checks are suppressed

      if Storage_Checks_Suppressed (Typ) then
         return;
      end if;

      --  It is pointless to insert this check inside an init proc, because
      --  that's too late, we have already built the object to be the right
      --  size, and if it's too large, too bad!

      if Inside_Init_Proc then
         return;
      end if;

      --  Look head for pragma interface/import or address clause applying
      --  to this entity. If found, we suppress the check entirely. For now
      --  we only look ahead 20 declarations to stop this becoming too slow
      --  Note that eventually this whole routine gets moved to gigi.

      Decl := N;
      for Ctr in 1 .. 20 loop
         Next (Decl);
         exit when No (Decl);

         if Is_Address_Or_Import (Decl) then
            return;
         end if;
      end loop;

      --  First step is to calculate the maximum number of elements. For
      --  this calculation, we use the actual size of the subtype if it is
      --  static, and if a bound of a subtype is non-static, we go to the
      --  bound of the base type.

      Siz := Uint_1;
      Indx := First_Index (Typ);
      while Present (Indx) loop
         Xtyp := Etype (Indx);
         Lo := Type_Low_Bound (Xtyp);
         Hi := Type_High_Bound (Xtyp);

         --  If any bound raises constraint error, we will never get this
         --  far, so there is no need to generate any kind of check.

         if Raises_Constraint_Error (Lo)
           or else
             Raises_Constraint_Error (Hi)
         then
            Uintp.Release (Umark);
            return;
         end if;

         --  Otherwise get bounds values

         if Is_Static_Expression (Lo) then
            Lob := Expr_Value (Lo);
         else
            Lob := Expr_Value (Type_Low_Bound (Base_Type (Xtyp)));
            Static := False;
         end if;

         if Is_Static_Expression (Hi) then
            Hib := Expr_Value (Hi);
         else
            Hib := Expr_Value (Type_High_Bound (Base_Type (Xtyp)));
            Static := False;
         end if;

         Siz := Siz *  UI_Max (Hib - Lob + 1, Uint_0);
         Next_Index (Indx);
      end loop;

      --  Compute the limit against which we want to check. For subprograms,
      --  where the array will go on the stack, we use 8*2**24, which (in
      --  bits) is the size of a 16 megabyte array.

      if Is_Subprogram (Scope (Ent)) then
         Check_Siz := Uint_2 ** 27;
      else
         Check_Siz := Uint_2 ** 31;
      end if;

      --  If we have all static bounds and Siz is too large, then we know
      --  we know we have a storage error right now, so generate message

      if Static and then Siz >= Check_Siz then
         Insert_Action (N,
           Make_Raise_Storage_Error (Loc,
             Reason => SE_Object_Too_Large));
         Error_Msg_N ("?Storage_Error will be raised at run-time", N);
         Uintp.Release (Umark);
         return;
      end if;

      --  Case of component size known at compile time. If the array
      --  size is definitely in range, then we do not need a check.

      if Known_Esize (Ctyp)
        and then Siz * Esize (Ctyp) < Check_Siz
      then
         Uintp.Release (Umark);
         return;
      end if;

      --  Here if a dynamic check is required

      --  What we do is to build an expression for the size of the array,
      --  which is computed as the 'Size of the array component, times
      --  the size of each dimension.

      Uintp.Release (Umark);

      Sizx :=
        Make_Attribute_Reference (Loc,
          Prefix =>         New_Occurrence_Of (Ctyp, Loc),
          Attribute_Name => Name_Size);

      Indx := First_Index (Typ);
      for J in 1 .. Number_Dimensions (Typ) loop
         if Sloc (Etype (Indx)) = Sloc (N) then
            Ensure_Defined (Etype (Indx), N);
         end if;

         Sizx :=
           Make_Op_Multiply (Loc,
             Left_Opnd  => Sizx,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix         => New_Occurrence_Of (Typ, Loc),
                 Attribute_Name => Name_Length,
                 Expressions    => New_List (
                   Make_Integer_Literal (Loc, J))));
         Next_Index (Indx);
      end loop;

      --  Emit the check

      Code :=
        Make_Raise_Storage_Error (Loc,
          Condition =>
            Make_Op_Ge (Loc,
              Left_Opnd  => Sizx,
              Right_Opnd =>
                Make_Integer_Literal (Loc,
                  Intval    => Check_Siz)),
          Reason => SE_Object_Too_Large);

      Set_Size_Check_Code (Defining_Identifier (N), Code);
      Insert_Action (N, Code, Suppress => All_Checks);
   end Apply_Array_Size_Check;

   ----------------------------
   -- Apply_Constraint_Check --
   ----------------------------

   procedure Apply_Constraint_Check
     (N          : Node_Id;
      Typ        : Entity_Id;
      No_Sliding : Boolean := False)
   is
      Desig_Typ : Entity_Id;

   begin
      if Inside_A_Generic then
         return;

      elsif Is_Scalar_Type (Typ) then
         Apply_Scalar_Range_Check (N, Typ);

      elsif Is_Array_Type (Typ) then

         --  A useful optimization: an aggregate with only an others clause
         --  always has the right bounds.

         if Nkind (N) = N_Aggregate
           and then No (Expressions (N))
           and then Nkind
            (First (Choices (First (Component_Associations (N)))))
              = N_Others_Choice
         then
            return;
         end if;

         if Is_Constrained (Typ) then
            Apply_Length_Check (N, Typ);

            if No_Sliding then
               Apply_Range_Check (N, Typ);
            end if;
         else
            Apply_Range_Check (N, Typ);
         end if;

      elsif (Is_Record_Type (Typ)
               or else Is_Private_Type (Typ))
        and then Has_Discriminants (Base_Type (Typ))
        and then Is_Constrained (Typ)
      then
         Apply_Discriminant_Check (N, Typ);

      elsif Is_Access_Type (Typ) then

         Desig_Typ := Designated_Type (Typ);

         --  No checks necessary if expression statically null

         if Nkind (N) = N_Null then
            null;

         --  No sliding possible on access to arrays

         elsif Is_Array_Type (Desig_Typ) then
            if Is_Constrained (Desig_Typ) then
               Apply_Length_Check (N, Typ);
            end if;

            Apply_Range_Check (N, Typ);

         elsif Has_Discriminants (Base_Type (Desig_Typ))
            and then Is_Constrained (Desig_Typ)
         then
            Apply_Discriminant_Check (N, Typ);
         end if;

         if Can_Never_Be_Null (Typ)
           and then not Can_Never_Be_Null (Etype (N))
         then
            Install_Null_Excluding_Check (N);
         end if;
      end if;
   end Apply_Constraint_Check;

   ------------------------------
   -- Apply_Discriminant_Check --
   ------------------------------

   procedure Apply_Discriminant_Check
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id := Empty)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Do_Access : constant Boolean    := Is_Access_Type (Typ);
      S_Typ     : Entity_Id  := Etype (N);
      Cond      : Node_Id;
      T_Typ     : Entity_Id;

      function Is_Aliased_Unconstrained_Component return Boolean;
      --  It is possible for an aliased component to have a nominal
      --  unconstrained subtype (through instantiation). If this is a
      --  discriminated component assigned in the expansion of an aggregate
      --  in an initialization, the check must be suppressed. This unusual
      --  situation requires a predicate of its own (see 7503-008).

      ----------------------------------------
      -- Is_Aliased_Unconstrained_Component --
      ----------------------------------------

      function Is_Aliased_Unconstrained_Component return Boolean is
         Comp : Entity_Id;
         Pref : Node_Id;

      begin
         if Nkind (Lhs) /= N_Selected_Component then
            return False;
         else
            Comp := Entity (Selector_Name (Lhs));
            Pref := Prefix (Lhs);
         end if;

         if Ekind (Comp) /= E_Component
           or else not Is_Aliased (Comp)
         then
            return False;
         end if;

         return not Comes_From_Source (Pref)
           and then In_Instance
           and then not Is_Constrained (Etype (Comp));
      end Is_Aliased_Unconstrained_Component;

   --  Start of processing for Apply_Discriminant_Check

   begin
      if Do_Access then
         T_Typ := Designated_Type (Typ);
      else
         T_Typ := Typ;
      end if;

      --  Nothing to do if discriminant checks are suppressed or else no code
      --  is to be generated

      if not Expander_Active
        or else Discriminant_Checks_Suppressed (T_Typ)
      then
         return;
      end if;

      --  No discriminant checks necessary for an access when expression
      --  is statically Null. This is not only an optimization, this is
      --  fundamental because otherwise discriminant checks may be generated
      --  in init procs for types containing an access to a not-yet-frozen
      --  record, causing a deadly forward reference.

      --  Also, if the expression is of an access type whose designated
      --  type is incomplete, then the access value must be null and
      --  we suppress the check.

      if Nkind (N) = N_Null then
         return;

      elsif Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);

         if Ekind (S_Typ) = E_Incomplete_Type then
            return;
         end if;
      end if;

      --  If an assignment target is present, then we need to generate
      --  the actual subtype if the target is a parameter or aliased
      --  object with an unconstrained nominal subtype.

      if Present (Lhs)
        and then (Present (Param_Entity (Lhs))
                   or else (not Is_Constrained (T_Typ)
                             and then Is_Aliased_View (Lhs)
                             and then not Is_Aliased_Unconstrained_Component))
      then
         T_Typ := Get_Actual_Subtype (Lhs);
      end if;

      --  Nothing to do if the type is unconstrained (this is the case
      --  where the actual subtype in the RM sense of N is unconstrained
      --  and no check is required).

      if not Is_Constrained (T_Typ) then
         return;

      --  Ada 2005: nothing to do if the type is one for which there is a
      --  partial view that is constrained.

      elsif Ada_Version >= Ada_05
        and then Has_Constrained_Partial_View (Base_Type (T_Typ))
      then
         return;
      end if;

      --  Nothing to do if the type is an Unchecked_Union

      if Is_Unchecked_Union (Base_Type (T_Typ)) then
         return;
      end if;

      --  Suppress checks if the subtypes are the same.
      --  the check must be preserved in an assignment to a formal, because
      --  the constraint is given by the actual.

      if Nkind (Original_Node (N)) /= N_Allocator
        and then (No (Lhs)
          or else not Is_Entity_Name (Lhs)
          or else No (Param_Entity (Lhs)))
      then
         if (Etype (N) = Typ
              or else (Do_Access and then Designated_Type (Typ) = S_Typ))
           and then not Is_Aliased_View (Lhs)
         then
            return;
         end if;

      --  We can also eliminate checks on allocators with a subtype mark
      --  that coincides with the context type. The context type may be a
      --  subtype without a constraint (common case, a generic actual).

      elsif Nkind (Original_Node (N)) = N_Allocator
        and then Is_Entity_Name (Expression (Original_Node (N)))
      then
         declare
            Alloc_Typ : constant Entity_Id :=
                          Entity (Expression (Original_Node (N)));

         begin
            if Alloc_Typ = T_Typ
              or else (Nkind (Parent (T_Typ)) = N_Subtype_Declaration
                        and then Is_Entity_Name (
                          Subtype_Indication (Parent (T_Typ)))
                        and then Alloc_Typ = Base_Type (T_Typ))

            then
               return;
            end if;
         end;
      end if;

      --  See if we have a case where the types are both constrained, and
      --  all the constraints are constants. In this case, we can do the
      --  check successfully at compile time.

      --  We skip this check for the case where the node is a rewritten`
      --  allocator, because it already carries the context subtype, and
      --  extracting the discriminants from the aggregate is messy.

      if Is_Constrained (S_Typ)
        and then Nkind (Original_Node (N)) /= N_Allocator
      then
         declare
            DconT : Elmt_Id;
            Discr : Entity_Id;
            DconS : Elmt_Id;
            ItemS : Node_Id;
            ItemT : Node_Id;

         begin
            --  S_Typ may not have discriminants in the case where it is a
            --  private type completed by a default discriminated type. In
            --  that case, we need to get the constraints from the
            --  underlying_type. If the underlying type is unconstrained (i.e.
            --  has no default discriminants) no check is needed.

            if Has_Discriminants (S_Typ) then
               Discr := First_Discriminant (S_Typ);
               DconS := First_Elmt (Discriminant_Constraint (S_Typ));

            else
               Discr := First_Discriminant (Underlying_Type (S_Typ));
               DconS :=
                 First_Elmt
                   (Discriminant_Constraint (Underlying_Type (S_Typ)));

               if No (DconS) then
                  return;
               end if;

               --  A further optimization: if T_Typ is derived from S_Typ
               --  without imposing a constraint, no check is needed.

               if Nkind (Original_Node (Parent (T_Typ))) =
                 N_Full_Type_Declaration
               then
                  declare
                     Type_Def : constant Node_Id :=
                                 Type_Definition
                                   (Original_Node (Parent (T_Typ)));
                  begin
                     if Nkind (Type_Def) = N_Derived_Type_Definition
                       and then Is_Entity_Name (Subtype_Indication (Type_Def))
                       and then Entity (Subtype_Indication (Type_Def)) = S_Typ
                     then
                        return;
                     end if;
                  end;
               end if;
            end if;

            DconT  := First_Elmt (Discriminant_Constraint (T_Typ));

            while Present (Discr) loop
               ItemS := Node (DconS);
               ItemT := Node (DconT);

               exit when
                 not Is_OK_Static_Expression (ItemS)
                   or else
                 not Is_OK_Static_Expression (ItemT);

               if Expr_Value (ItemS) /= Expr_Value (ItemT) then
                  if Do_Access then   --  needs run-time check.
                     exit;
                  else
                     Apply_Compile_Time_Constraint_Error
                       (N, "incorrect value for discriminant&?",
                        CE_Discriminant_Check_Failed, Ent => Discr);
                     return;
                  end if;
               end if;

               Next_Elmt (DconS);
               Next_Elmt (DconT);
               Next_Discriminant (Discr);
            end loop;

            if No (Discr) then
               return;
            end if;
         end;
      end if;

      --  Here we need a discriminant check. First build the expression
      --  for the comparisons of the discriminants:

      --    (n.disc1 /= typ.disc1) or else
      --    (n.disc2 /= typ.disc2) or else
      --     ...
      --    (n.discn /= typ.discn)

      Cond := Build_Discriminant_Checks (N, T_Typ);

      --  If Lhs is set and is a parameter, then the condition is
      --  guarded by: lhs'constrained and then (condition built above)

      if Present (Param_Entity (Lhs)) then
         Cond :=
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Param_Entity (Lhs), Loc),
                 Attribute_Name => Name_Constrained),
             Right_Opnd => Cond);
      end if;

      if Do_Access then
         Cond := Guard_Access (Cond, Loc, N);
      end if;

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition => Cond,
          Reason    => CE_Discriminant_Check_Failed));
   end Apply_Discriminant_Check;

   ------------------------
   -- Apply_Divide_Check --
   ------------------------

   procedure Apply_Divide_Check (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Left  : constant Node_Id    := Left_Opnd (N);
      Right : constant Node_Id    := Right_Opnd (N);

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;

   begin
      if Expander_Active
        and then not Backend_Divide_Checks_On_Target
        and then Check_Needed (Right, Division_Check)
      then
         Determine_Range (Right, ROK, Rlo, Rhi);

         --  See if division by zero possible, and if so generate test. This
         --  part of the test is not controlled by the -gnato switch.

         if Do_Division_Check (N) then
            if (not ROK) or else (Rlo <= 0 and then 0 <= Rhi) then
               Insert_Action (N,
                 Make_Raise_Constraint_Error (Loc,
                   Condition =>
                     Make_Op_Eq (Loc,
                       Left_Opnd => Duplicate_Subexpr_Move_Checks (Right),
                       Right_Opnd => Make_Integer_Literal (Loc, 0)),
                   Reason => CE_Divide_By_Zero));
            end if;
         end if;

         --  Test for extremely annoying case of xxx'First divided by -1

         if Do_Overflow_Check (N) then
            if Nkind (N) = N_Op_Divide
              and then Is_Signed_Integer_Type (Typ)
            then
               Determine_Range (Left, LOK, Llo, Lhi);
               LLB := Expr_Value (Type_Low_Bound (Base_Type (Typ)));

               if ((not ROK) or else (Rlo <= (-1) and then (-1) <= Rhi))
                 and then
                 ((not LOK) or else (Llo = LLB))
               then
                  Insert_Action (N,
                    Make_Raise_Constraint_Error (Loc,
                      Condition =>
                        Make_And_Then (Loc,

                           Make_Op_Eq (Loc,
                             Left_Opnd  =>
                               Duplicate_Subexpr_Move_Checks (Left),
                             Right_Opnd => Make_Integer_Literal (Loc, LLB)),

                           Make_Op_Eq (Loc,
                             Left_Opnd =>
                               Duplicate_Subexpr (Right),
                             Right_Opnd =>
                               Make_Integer_Literal (Loc, -1))),
                      Reason => CE_Overflow_Check_Failed));
               end if;
            end if;
         end if;
      end if;
   end Apply_Divide_Check;

   ----------------------------------
   -- Apply_Float_Conversion_Check --
   ----------------------------------

   --  Let F and I be the source and target types of the conversion.
   --  The Ada standard specifies that a floating-point value X is rounded
   --  to the nearest integer, with halfway cases being rounded away from
   --  zero. The rounded value of X is checked against I'Range.

   --  The catch in the above paragraph is that there is no good way
   --  to know whether the round-to-integer operation resulted in
   --  overflow. A remedy is to perform a range check in the floating-point
   --  domain instead, however:
   --      (1)  The bounds may not be known at compile time
   --      (2)  The check must take into account possible rounding.
   --      (3)  The range of type I may not be exactly representable in F.
   --      (4)  The end-points I'First - 0.5 and I'Last + 0.5 may or may
   --           not be in range, depending on the sign of  I'First and I'Last.
   --      (5)  X may be a NaN, which will fail any comparison

   --  The following steps take care of these issues converting X:
   --      (1) If either I'First or I'Last is not known at compile time, use
   --          I'Base instead of I in the next three steps and perform a
   --          regular range check against I'Range after conversion.
   --      (2) If I'First - 0.5 is representable in F then let Lo be that
   --          value and define Lo_OK as (I'First > 0). Otherwise, let Lo be
   --          F'Machine (T) and let Lo_OK be (Lo >= I'First). In other words,
   --          take one of the closest floating-point numbers to T, and see if
   --          it is in range or not.
   --      (3) If I'Last + 0.5 is representable in F then let Hi be that value
   --          and define Hi_OK as (I'Last < 0). Otherwise, let Hi be
   --          F'Rounding (T) and let Hi_OK be (Hi <= I'Last).
   --      (4) Raise CE when (Lo_OK and X < Lo) or (not Lo_OK and X <= Lo)
   --                     or (Hi_OK and X > Hi) or (not Hi_OK and X >= Hi)

   procedure Apply_Float_Conversion_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id)
   is
      LB          : constant Node_Id := Type_Low_Bound (Target_Typ);
      HB          : constant Node_Id := Type_High_Bound (Target_Typ);
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      Expr_Type   : constant Entity_Id  := Base_Type (Etype (Ck_Node));
      Target_Base : constant Entity_Id  := Implementation_Base_Type
                                             (Target_Typ);
      Max_Bound   : constant Uint := UI_Expon
                                       (Machine_Radix (Expr_Type),
                                        Machine_Mantissa (Expr_Type) - 1) - 1;
      --  Largest bound, so bound plus or minus half is a machine number of F

      Ifirst,
      Ilast     : Uint;         --  Bounds of integer type
      Lo, Hi    : Ureal;        --  Bounds to check in floating-point domain
      Lo_OK,
      Hi_OK     : Boolean;      --  True iff Lo resp. Hi belongs to I'Range

      Lo_Chk,
      Hi_Chk    : Node_Id;      --  Expressions that are False iff check fails

      Reason    : RT_Exception_Code;

   begin
      if not Compile_Time_Known_Value (LB)
          or not Compile_Time_Known_Value (HB)
      then
         declare
            --  First check that the value falls in the range of the base
            --  type, to prevent overflow during conversion and then
            --  perform a regular range check against the (dynamic) bounds.

            Par : constant Node_Id := Parent (Ck_Node);

            pragma Assert (Target_Base /= Target_Typ);
            pragma Assert (Nkind (Par) = N_Type_Conversion);

            Temp : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_Internal_Name ('T'));

         begin
            Apply_Float_Conversion_Check (Ck_Node, Target_Base);
            Set_Etype (Temp, Target_Base);

            Insert_Action (Parent (Par),
              Make_Object_Declaration (Loc,
                Defining_Identifier => Temp,
                Object_Definition => New_Occurrence_Of (Target_Typ, Loc),
                Expression => New_Copy_Tree (Par)),
                Suppress => All_Checks);

            Insert_Action (Par,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Not_In (Loc,
                    Left_Opnd  => New_Occurrence_Of (Temp, Loc),
                    Right_Opnd => New_Occurrence_Of (Target_Typ, Loc)),
                Reason => CE_Range_Check_Failed));
            Rewrite (Par, New_Occurrence_Of (Temp, Loc));

            return;
         end;
      end if;

      --  Get the bounds of the target type

      Ifirst := Expr_Value (LB);
      Ilast  := Expr_Value (HB);

      --  Check against lower bound

      if abs (Ifirst) < Max_Bound then
         Lo := UR_From_Uint (Ifirst) - Ureal_Half;
         Lo_OK := (Ifirst > 0);
      else
         Lo := Machine (Expr_Type, UR_From_Uint (Ifirst), Round_Even, Ck_Node);
         Lo_OK := (Lo >= UR_From_Uint (Ifirst));
      end if;

      if Lo_OK then

         --  Lo_Chk := (X >= Lo)

         Lo_Chk := Make_Op_Ge (Loc,
                     Left_Opnd => Duplicate_Subexpr_No_Checks (Ck_Node),
                     Right_Opnd => Make_Real_Literal (Loc, Lo));

      else
         --  Lo_Chk := (X > Lo)

         Lo_Chk := Make_Op_Gt (Loc,
                     Left_Opnd => Duplicate_Subexpr_No_Checks (Ck_Node),
                     Right_Opnd => Make_Real_Literal (Loc, Lo));
      end if;

      --  Check against higher bound

      if abs (Ilast) < Max_Bound then
         Hi := UR_From_Uint (Ilast) + Ureal_Half;
         Hi_OK := (Ilast < 0);
      else
         Hi := Machine (Expr_Type, UR_From_Uint (Ilast), Round_Even, Ck_Node);
         Hi_OK := (Hi <= UR_From_Uint (Ilast));
      end if;

      if Hi_OK then

         --  Hi_Chk := (X <= Hi)

         Hi_Chk := Make_Op_Le (Loc,
                     Left_Opnd => Duplicate_Subexpr_No_Checks (Ck_Node),
                     Right_Opnd => Make_Real_Literal (Loc, Hi));

      else
         --  Hi_Chk := (X < Hi)

         Hi_Chk := Make_Op_Lt (Loc,
                     Left_Opnd => Duplicate_Subexpr_No_Checks (Ck_Node),
                     Right_Opnd => Make_Real_Literal (Loc, Hi));
      end if;

      --  If the bounds of the target type are the same as those of the
      --  base type, the check is an overflow check as a range check is
      --  not performed in these cases.

      if Expr_Value (Type_Low_Bound (Target_Base)) = Ifirst
        and then Expr_Value (Type_High_Bound (Target_Base)) = Ilast
      then
         Reason := CE_Overflow_Check_Failed;
      else
         Reason := CE_Range_Check_Failed;
      end if;

      --  Raise CE if either conditions does not hold

      Insert_Action (Ck_Node,
        Make_Raise_Constraint_Error (Loc,
          Condition => Make_Op_Not (Loc, Make_And_Then (Loc, Lo_Chk, Hi_Chk)),
          Reason    => Reason));
   end Apply_Float_Conversion_Check;

   ------------------------
   -- Apply_Length_Check --
   ------------------------

   procedure Apply_Length_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Length_Checks
        (Ck_Node, Target_Typ, Source_Typ, Do_Static => False);
   end Apply_Length_Check;

   -----------------------
   -- Apply_Range_Check --
   -----------------------

   procedure Apply_Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Range_Checks
        (Ck_Node, Target_Typ, Source_Typ, Do_Static => False);
   end Apply_Range_Check;

   ------------------------------
   -- Apply_Scalar_Range_Check --
   ------------------------------

   --  Note that Apply_Scalar_Range_Check never turns the Do_Range_Check
   --  flag off if it is already set on.

   procedure Apply_Scalar_Range_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Fixed_Int  : Boolean   := False)
   is
      Parnt   : constant Node_Id := Parent (Expr);
      S_Typ   : Entity_Id;
      Arr     : Node_Id   := Empty;  -- initialize to prevent warning
      Arr_Typ : Entity_Id := Empty;  -- initialize to prevent warning
      OK      : Boolean;

      Is_Subscr_Ref : Boolean;
      --  Set true if Expr is a subscript

      Is_Unconstrained_Subscr_Ref : Boolean;
      --  Set true if Expr is a subscript of an unconstrained array. In this
      --  case we do not attempt to do an analysis of the value against the
      --  range of the subscript, since we don't know the actual subtype.

      Int_Real : Boolean;
      --  Set to True if Expr should be regarded as a real value
      --  even though the type of Expr might be discrete.

      procedure Bad_Value;
      --  Procedure called if value is determined to be out of range

      ---------------
      -- Bad_Value --
      ---------------

      procedure Bad_Value is
      begin
         Apply_Compile_Time_Constraint_Error
           (Expr, "value not in range of}?", CE_Range_Check_Failed,
            Ent => Target_Typ,
            Typ => Target_Typ);
      end Bad_Value;

   --  Start of processing for Apply_Scalar_Range_Check

   begin
      if Inside_A_Generic then
         return;

      --  Return if check obviously not needed. Note that we do not check
      --  for the expander being inactive, since this routine does not
      --  insert any code, but it does generate useful warnings sometimes,
      --  which we would like even if we are in semantics only mode.

      elsif Target_Typ = Any_Type
        or else not Is_Scalar_Type (Target_Typ)
        or else Raises_Constraint_Error (Expr)
      then
         return;
      end if;

      --  Now, see if checks are suppressed

      Is_Subscr_Ref :=
        Is_List_Member (Expr) and then Nkind (Parnt) = N_Indexed_Component;

      if Is_Subscr_Ref then
         Arr := Prefix (Parnt);
         Arr_Typ := Get_Actual_Subtype_If_Available (Arr);
      end if;

      if not Do_Range_Check (Expr) then

         --  Subscript reference. Check for Index_Checks suppressed

         if Is_Subscr_Ref then

            --  Check array type and its base type

            if Index_Checks_Suppressed (Arr_Typ)
              or else Index_Checks_Suppressed (Base_Type (Arr_Typ))
            then
               return;

            --  Check array itself if it is an entity name

            elsif Is_Entity_Name (Arr)
              and then Index_Checks_Suppressed (Entity (Arr))
            then
               return;

            --  Check expression itself if it is an entity name

            elsif Is_Entity_Name (Expr)
              and then Index_Checks_Suppressed (Entity (Expr))
            then
               return;
            end if;

         --  All other cases, check for Range_Checks suppressed

         else
            --  Check target type and its base type

            if Range_Checks_Suppressed (Target_Typ)
              or else Range_Checks_Suppressed (Base_Type (Target_Typ))
            then
               return;

            --  Check expression itself if it is an entity name

            elsif Is_Entity_Name (Expr)
              and then Range_Checks_Suppressed (Entity (Expr))
            then
               return;

            --  If Expr is part of an assignment statement, then check
            --  left side of assignment if it is an entity name.

            elsif Nkind (Parnt) = N_Assignment_Statement
              and then Is_Entity_Name (Name (Parnt))
              and then Range_Checks_Suppressed (Entity (Name (Parnt)))
            then
               return;
            end if;
         end if;
      end if;

      --  Do not set range checks if they are killed

      if Nkind (Expr) = N_Unchecked_Type_Conversion
        and then Kill_Range_Check (Expr)
      then
         return;
      end if;

      --  Do not set range checks for any values from System.Scalar_Values
      --  since the whole idea of such values is to avoid checking them!

      if Is_Entity_Name (Expr)
        and then Is_RTU (Scope (Entity (Expr)), System_Scalar_Values)
      then
         return;
      end if;

      --  Now see if we need a check

      if No (Source_Typ) then
         S_Typ := Etype (Expr);
      else
         S_Typ := Source_Typ;
      end if;

      if not Is_Scalar_Type (S_Typ) or else S_Typ = Any_Type then
         return;
      end if;

      Is_Unconstrained_Subscr_Ref :=
        Is_Subscr_Ref and then not Is_Constrained (Arr_Typ);

      --  Always do a range check if the source type includes infinities
      --  and the target type does not include infinities. We do not do
      --  this if range checks are killed.

      if Is_Floating_Point_Type (S_Typ)
        and then Has_Infinities (S_Typ)
        and then not Has_Infinities (Target_Typ)
      then
         Enable_Range_Check (Expr);
      end if;

      --  Return if we know expression is definitely in the range of
      --  the target type as determined by Determine_Range. Right now
      --  we only do this for discrete types, and not fixed-point or
      --  floating-point types.

      --  The additional less-precise tests below catch these cases

      --  Note: skip this if we are given a source_typ, since the point
      --  of supplying a Source_Typ is to stop us looking at the expression.
      --  could sharpen this test to be out parameters only ???

      if Is_Discrete_Type (Target_Typ)
        and then Is_Discrete_Type (Etype (Expr))
        and then not Is_Unconstrained_Subscr_Ref
        and then No (Source_Typ)
      then
         declare
            Tlo : constant Node_Id := Type_Low_Bound  (Target_Typ);
            Thi : constant Node_Id := Type_High_Bound (Target_Typ);
            Lo  : Uint;
            Hi  : Uint;

         begin
            if Compile_Time_Known_Value (Tlo)
              and then Compile_Time_Known_Value (Thi)
            then
               declare
                  Lov : constant Uint := Expr_Value (Tlo);
                  Hiv : constant Uint := Expr_Value (Thi);

               begin
                  --  If range is null, we for sure have a constraint error
                  --  (we don't even need to look at the value involved,
                  --  since all possible values will raise CE).

                  if Lov > Hiv then
                     Bad_Value;
                     return;
                  end if;

                  --  Otherwise determine range of value

                  Determine_Range (Expr, OK, Lo, Hi);

                  if OK then

                     --  If definitely in range, all OK

                     if Lo >= Lov and then Hi <= Hiv then
                        return;

                     --  If definitely not in range, warn

                     elsif Lov > Hi or else Hiv < Lo then
                        Bad_Value;
                        return;

                     --  Otherwise we don't know

                     else
                        null;
                     end if;
                  end if;
               end;
            end if;
         end;
      end if;

      Int_Real :=
        Is_Floating_Point_Type (S_Typ)
          or else (Is_Fixed_Point_Type (S_Typ) and then not Fixed_Int);

      --  Check if we can determine at compile time whether Expr is in the
      --  range of the target type. Note that if S_Typ is within the bounds
      --  of Target_Typ then this must be the case. This check is meaningful
      --  only if this is not a conversion between integer and real types.

      if not Is_Unconstrained_Subscr_Ref
        and then
           Is_Discrete_Type (S_Typ) = Is_Discrete_Type (Target_Typ)
        and then
          (In_Subrange_Of (S_Typ, Target_Typ, Fixed_Int)
             or else
           Is_In_Range (Expr, Target_Typ, Fixed_Int, Int_Real))
      then
         return;

      elsif Is_Out_Of_Range (Expr, Target_Typ, Fixed_Int, Int_Real) then
         Bad_Value;
         return;

      --  In the floating-point case, we only do range checks if the
      --  type is constrained. We definitely do NOT want range checks
      --  for unconstrained types, since we want to have infinities

      elsif Is_Floating_Point_Type (S_Typ) then
         if Is_Constrained (S_Typ) then
            Enable_Range_Check (Expr);
         end if;

      --  For all other cases we enable a range check unconditionally

      else
         Enable_Range_Check (Expr);
         return;
      end if;
   end Apply_Scalar_Range_Check;

   ----------------------------------
   -- Apply_Selected_Length_Checks --
   ----------------------------------

   procedure Apply_Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean)
   is
      Cond     : Node_Id;
      R_Result : Check_Result;
      R_Cno    : Node_Id;

      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On   : constant Boolean :=
                      (not Index_Checks_Suppressed (Target_Typ))
                        or else
                      (not Length_Checks_Suppressed (Target_Typ));

   begin
      if not Expander_Active then
         return;
      end if;

      R_Result :=
        Selected_Length_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop
         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  A length check may mention an Itype which is attached to a
         --  subsequent node. At the top level in a package this can cause
         --  an order-of-elaboration problem, so we make sure that the itype
         --  is referenced now.

         if Ekind (Current_Scope) = E_Package
           and then Is_Compilation_Unit (Current_Scope)
         then
            Ensure_Defined (Target_Typ, Ck_Node);

            if Present (Source_Typ) then
               Ensure_Defined (Source_Typ, Ck_Node);

            elsif Is_Itype (Etype (Ck_Node)) then
               Ensure_Defined (Etype (Ck_Node), Ck_Node);
            end if;
         end if;

         --  If the item is a conditional raise of constraint error,
         --  then have a look at what check is being performed and
         --  ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            if not Has_Dynamic_Length_Check (Ck_Node)
              and then Checks_On
            then
               Insert_Action (Ck_Node, R_Cno);

               if not Do_Static then
                  Set_Has_Dynamic_Length_Check (Ck_Node);
               end if;
            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               Apply_Compile_Time_Constraint_Error
                 (Ck_Node, "wrong length for array of}?",
                  CE_Length_Check_Failed,
                  Ent => Target_Typ,
                  Typ => Target_Typ);

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Rewrite (R_Cno, Make_Null_Statement (Loc));
            end if;

         else
            Install_Static_Check (R_Cno, Loc);
         end if;

      end loop;

   end Apply_Selected_Length_Checks;

   ---------------------------------
   -- Apply_Selected_Range_Checks --
   ---------------------------------

   procedure Apply_Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean)
   is
      Cond     : Node_Id;
      R_Result : Check_Result;
      R_Cno    : Node_Id;

      Loc       : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On : constant Boolean :=
                    (not Index_Checks_Suppressed (Target_Typ))
                      or else
                    (not Range_Checks_Suppressed (Target_Typ));

   begin
      if not Expander_Active or else not Checks_On then
         return;
      end if;

      R_Result :=
        Selected_Range_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop

         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  If the item is a conditional raise of constraint error,
         --  then have a look at what check is being performed and
         --  ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            if not Has_Dynamic_Range_Check (Ck_Node) then
               Insert_Action (Ck_Node, R_Cno);

               if not Do_Static then
                  Set_Has_Dynamic_Range_Check (Ck_Node);
               end if;
            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               --  Since an N_Range is technically not an expression, we
               --  have to set one of the bounds to C_E and then just flag
               --  the N_Range. The warning message will point to the
               --  lower bound and complain about a range, which seems OK.

               if Nkind (Ck_Node) = N_Range then
                  Apply_Compile_Time_Constraint_Error
                    (Low_Bound (Ck_Node), "static range out of bounds of}?",
                     CE_Range_Check_Failed,
                     Ent => Target_Typ,
                     Typ => Target_Typ);

                  Set_Raises_Constraint_Error (Ck_Node);

               else
                  Apply_Compile_Time_Constraint_Error
                    (Ck_Node, "static value out of range of}?",
                     CE_Range_Check_Failed,
                     Ent => Target_Typ,
                     Typ => Target_Typ);
               end if;

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Rewrite (R_Cno, Make_Null_Statement (Loc));
            end if;

         else
            Install_Static_Check (R_Cno, Loc);
         end if;
      end loop;
   end Apply_Selected_Range_Checks;

   -------------------------------
   -- Apply_Static_Length_Check --
   -------------------------------

   procedure Apply_Static_Length_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Length_Checks
        (Expr, Target_Typ, Source_Typ, Do_Static => True);
   end Apply_Static_Length_Check;

   -------------------------------------
   -- Apply_Subscript_Validity_Checks --
   -------------------------------------

   procedure Apply_Subscript_Validity_Checks (Expr : Node_Id) is
      Sub : Node_Id;

   begin
      pragma Assert (Nkind (Expr) = N_Indexed_Component);

      --  Loop through subscripts

      Sub := First (Expressions (Expr));
      while Present (Sub) loop

         --  Check one subscript. Note that we do not worry about
         --  enumeration type with holes, since we will convert the
         --  value to a Pos value for the subscript, and that convert
         --  will do the necessary validity check.

         Ensure_Valid (Sub, Holes_OK => True);

         --  Move to next subscript

         Sub := Next (Sub);
      end loop;
   end Apply_Subscript_Validity_Checks;

   ----------------------------------
   -- Apply_Type_Conversion_Checks --
   ----------------------------------

   procedure Apply_Type_Conversion_Checks (N : Node_Id) is
      Target_Type : constant Entity_Id := Etype (N);
      Target_Base : constant Entity_Id := Base_Type (Target_Type);
      Expr        : constant Node_Id   := Expression (N);
      Expr_Type   : constant Entity_Id := Etype (Expr);

   begin
      if Inside_A_Generic then
         return;

      --  Skip these checks if serious errors detected, there are some nasty
      --  situations of incomplete trees that blow things up.

      elsif Serious_Errors_Detected > 0 then
         return;

      --  Scalar type conversions of the form Target_Type (Expr) require
      --  a range check if we cannot be sure that Expr is in the base type
      --  of Target_Typ and also that Expr is in the range of Target_Typ.
      --  These are not quite the same condition from an implementation
      --  point of view, but clearly the second includes the first.

      elsif Is_Scalar_Type (Target_Type) then
         declare
            Conv_OK  : constant Boolean := Conversion_OK (N);
            --  If the Conversion_OK flag on the type conversion is set
            --  and no floating point type is involved in the type conversion
            --  then fixed point values must be read as integral values.

            Float_To_Int : constant Boolean :=
                             Is_Floating_Point_Type (Expr_Type)
                               and then Is_Integer_Type (Target_Type);

         begin
            if not Overflow_Checks_Suppressed (Target_Base)
              and then not In_Subrange_Of (Expr_Type, Target_Base, Conv_OK)
              and then not Float_To_Int
            then
               Set_Do_Overflow_Check (N);
            end if;

            if not Range_Checks_Suppressed (Target_Type)
              and then not Range_Checks_Suppressed (Expr_Type)
            then
               if Float_To_Int then
                  Apply_Float_Conversion_Check (Expr, Target_Type);
               else
                  Apply_Scalar_Range_Check
                    (Expr, Target_Type, Fixed_Int => Conv_OK);
               end if;
            end if;
         end;

      elsif Comes_From_Source (N)
        and then Is_Record_Type (Target_Type)
        and then Is_Derived_Type (Target_Type)
        and then not Is_Tagged_Type (Target_Type)
        and then not Is_Constrained (Target_Type)
        and then Present (Stored_Constraint (Target_Type))
      then
         --  An unconstrained derived type may have inherited discriminant
         --  Build an actual discriminant constraint list using the stored
         --  constraint, to verify that the expression of the parent type
         --  satisfies the constraints imposed by the (unconstrained!)
         --  derived type. This applies to value conversions, not to view
         --  conversions of tagged types.

         declare
            Loc         : constant Source_Ptr := Sloc (N);
            Cond        : Node_Id;
            Constraint  : Elmt_Id;
            Discr_Value : Node_Id;
            Discr       : Entity_Id;

            New_Constraints : constant Elist_Id := New_Elmt_List;
            Old_Constraints : constant Elist_Id :=
                                Discriminant_Constraint (Expr_Type);

         begin
            Constraint := First_Elmt (Stored_Constraint (Target_Type));

            while Present (Constraint) loop
               Discr_Value := Node (Constraint);

               if Is_Entity_Name (Discr_Value)
                 and then Ekind (Entity (Discr_Value)) = E_Discriminant
               then
                  Discr := Corresponding_Discriminant (Entity (Discr_Value));

                  if Present (Discr)
                    and then Scope (Discr) = Base_Type (Expr_Type)
                  then
                     --  Parent is constrained by new discriminant. Obtain
                     --  Value of original discriminant in expression. If
                     --  the new discriminant has been used to constrain more
                     --  than one of the stored discriminants, this will
                     --  provide the required consistency check.

                     Append_Elmt (
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Duplicate_Subexpr_No_Checks
                              (Expr, Name_Req => True),
                          Selector_Name =>
                            Make_Identifier (Loc, Chars (Discr))),
                                New_Constraints);

                  else
                     --  Discriminant of more remote ancestor ???

                     return;
                  end if;

               --  Derived type definition has an explicit value for
               --  this stored discriminant.

               else
                  Append_Elmt
                    (Duplicate_Subexpr_No_Checks (Discr_Value),
                     New_Constraints);
               end if;

               Next_Elmt (Constraint);
            end loop;

            --  Use the unconstrained expression type to retrieve the
            --  discriminants of the parent, and apply momentarily the
            --  discriminant constraint synthesized above.

            Set_Discriminant_Constraint (Expr_Type, New_Constraints);
            Cond := Build_Discriminant_Checks (Expr, Expr_Type);
            Set_Discriminant_Constraint (Expr_Type, Old_Constraints);

            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition => Cond,
                Reason    => CE_Discriminant_Check_Failed));
         end;

      --  For arrays, conversions are applied during expansion, to take
      --  into accounts changes of representation.  The checks become range
      --  checks on the base type or length checks on the subtype, depending
      --  on whether the target type is unconstrained or constrained.

      else
         null;
      end if;
   end Apply_Type_Conversion_Checks;

   ----------------------------------------------
   -- Apply_Universal_Integer_Attribute_Checks --
   ----------------------------------------------

   procedure Apply_Universal_Integer_Attribute_Checks (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      if Inside_A_Generic then
         return;

      --  Nothing to do if checks are suppressed

      elsif Range_Checks_Suppressed (Typ)
        and then Overflow_Checks_Suppressed (Typ)
      then
         return;

      --  Nothing to do if the attribute does not come from source. The
      --  internal attributes we generate of this type do not need checks,
      --  and furthermore the attempt to check them causes some circular
      --  elaboration orders when dealing with packed types.

      elsif not Comes_From_Source (N) then
         return;

      --  If the prefix is a selected component that depends on a discriminant
      --  the check may improperly expose a discriminant instead of using
      --  the bounds of the object itself. Set the type of the attribute to
      --  the base type of the context, so that a check will be imposed when
      --  needed (e.g. if the node appears as an index).

      elsif Nkind (Prefix (N)) = N_Selected_Component
        and then Ekind (Typ) = E_Signed_Integer_Subtype
        and then Depends_On_Discriminant (Scalar_Range (Typ))
      then
         Set_Etype (N, Base_Type (Typ));

      --  Otherwise, replace the attribute node with a type conversion
      --  node whose expression is the attribute, retyped to universal
      --  integer, and whose subtype mark is the target type. The call
      --  to analyze this conversion will set range and overflow checks
      --  as required for proper detection of an out of range value.

      else
         Set_Etype    (N, Universal_Integer);
         Set_Analyzed (N, True);

         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Typ, Loc),
             Expression   => Relocate_Node (N)));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

   end Apply_Universal_Integer_Attribute_Checks;

   -------------------------------
   -- Build_Discriminant_Checks --
   -------------------------------

   function Build_Discriminant_Checks
     (N     : Node_Id;
      T_Typ : Entity_Id) return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (N);
      Cond     : Node_Id;
      Disc     : Elmt_Id;
      Disc_Ent : Entity_Id;
      Dref     : Node_Id;
      Dval     : Node_Id;

   begin
      Cond := Empty;
      Disc := First_Elmt (Discriminant_Constraint (T_Typ));

      --  For a fully private type, use the discriminants of the parent type

      if Is_Private_Type (T_Typ)
        and then No (Full_View (T_Typ))
      then
         Disc_Ent := First_Discriminant (Etype (Base_Type (T_Typ)));
      else
         Disc_Ent := First_Discriminant (T_Typ);
      end if;

      while Present (Disc) loop
         Dval := Node (Disc);

         if Nkind (Dval) = N_Identifier
           and then Ekind (Entity (Dval)) = E_Discriminant
         then
            Dval := New_Occurrence_Of (Discriminal (Entity (Dval)), Loc);
         else
            Dval := Duplicate_Subexpr_No_Checks (Dval);
         end if;

         --  If we have an Unchecked_Union node, we can infer the discriminants
         --  of the node.

         if Is_Unchecked_Union (Base_Type (T_Typ)) then
            Dref := New_Copy (
              Get_Discriminant_Value (
                First_Discriminant (T_Typ),
                T_Typ,
                Stored_Constraint (T_Typ)));

         else
            Dref :=
              Make_Selected_Component (Loc,
                Prefix =>
                  Duplicate_Subexpr_No_Checks (N, Name_Req => True),
                Selector_Name =>
                  Make_Identifier (Loc, Chars (Disc_Ent)));

            Set_Is_In_Discriminant_Check (Dref);
         end if;

         Evolve_Or_Else (Cond,
           Make_Op_Ne (Loc,
             Left_Opnd => Dref,
             Right_Opnd => Dval));

         Next_Elmt (Disc);
         Next_Discriminant (Disc_Ent);
      end loop;

      return Cond;
   end Build_Discriminant_Checks;

   ------------------
   -- Check_Needed --
   ------------------

   function Check_Needed (Nod : Node_Id; Check : Check_Type) return Boolean is
      N : Node_Id;
      P : Node_Id;
      K : Node_Kind;
      L : Node_Id;
      R : Node_Id;

   begin
      --  Always check if not simple entity

      if Nkind (Nod) not in N_Has_Entity
        or else not Comes_From_Source (Nod)
      then
         return True;
      end if;

      --  Look up tree for short circuit

      N := Nod;
      loop
         P := Parent (N);
         K := Nkind (P);

         if K not in N_Subexpr then
            return True;

         --  Or/Or Else case, left operand must be equality test

         elsif K = N_Op_Or or else K = N_Or_Else then
            exit when N = Right_Opnd (P)
              and then Nkind (Left_Opnd (P)) = N_Op_Eq;

         --  And/And then case, left operand must be inequality test

         elsif K = N_Op_And or else K = N_And_Then then
            exit when N = Right_Opnd (P)
              and then Nkind (Left_Opnd (P)) = N_Op_Ne;
         end if;

         N := P;
      end loop;

      --  If we fall through the loop, then we have a conditional with an
      --  appropriate test as its left operand. So test further.

      L := Left_Opnd (P);

      if Nkind (L) = N_Op_Not then
         L := Right_Opnd (L);
      end if;

      R := Right_Opnd (L);
      L := Left_Opnd (L);

      --  Left operand of test must match original variable

      if Nkind (L) not in N_Has_Entity
        or else Entity (L) /= Entity (Nod)
      then
         return True;
      end if;

      --  Right operand of test mus be key value (zero or null)

      case Check is
         when Access_Check =>
            if Nkind (R) /= N_Null then
               return True;
            end if;

         when Division_Check =>
            if not Compile_Time_Known_Value (R)
              or else Expr_Value (R) /= Uint_0
            then
               return True;
            end if;
      end case;

      --  Here we have the optimizable case, warn if not short-circuited

      if K = N_Op_And or else K = N_Op_Or then
         case Check is
            when Access_Check =>
               Error_Msg_N
                 ("Constraint_Error may be raised (access check)?",
                  Parent (Nod));
            when Division_Check =>
               Error_Msg_N
                 ("Constraint_Error may be raised (zero divide)?",
                  Parent (Nod));
         end case;

         if K = N_Op_And then
            Error_Msg_N ("use `AND THEN` instead of AND?", P);
         else
            Error_Msg_N ("use `OR ELSE` instead of OR?", P);
         end if;

         --  If not short-circuited, we need the ckeck

         return True;

      --  If short-circuited, we can omit the check

      else
         return False;
      end if;
   end Check_Needed;

   -----------------------------------
   -- Check_Valid_Lvalue_Subscripts --
   -----------------------------------

   procedure Check_Valid_Lvalue_Subscripts (Expr : Node_Id) is
   begin
      --  Skip this if range checks are suppressed

      if Range_Checks_Suppressed (Etype (Expr)) then
         return;

      --  Only do this check for expressions that come from source. We
      --  assume that expander generated assignments explicitly include
      --  any necessary checks. Note that this is not just an optimization,
      --  it avoids infinite recursions!

      elsif not Comes_From_Source (Expr) then
         return;

      --  For a selected component, check the prefix

      elsif Nkind (Expr) = N_Selected_Component then
         Check_Valid_Lvalue_Subscripts (Prefix (Expr));
         return;

      --  Case of indexed component

      elsif Nkind (Expr) = N_Indexed_Component then
         Apply_Subscript_Validity_Checks (Expr);

         --  Prefix may itself be or contain an indexed component, and
         --  these subscripts need checking as well

         Check_Valid_Lvalue_Subscripts (Prefix (Expr));
      end if;
   end Check_Valid_Lvalue_Subscripts;

   ----------------------------------
   -- Null_Exclusion_Static_Checks --
   ----------------------------------

   procedure Null_Exclusion_Static_Checks (N : Node_Id) is
      K                  : constant Node_Kind := Nkind (N);
      Typ                : Entity_Id;
      Related_Nod        : Node_Id;
      Has_Null_Exclusion : Boolean := False;

   begin
      pragma Assert (K = N_Parameter_Specification
                       or else K = N_Object_Declaration
                       or else K = N_Discriminant_Specification
                       or else K = N_Component_Declaration);

      Typ := Etype (Defining_Identifier (N));

      pragma Assert (Is_Access_Type (Typ)
        or else (K = N_Object_Declaration and then Is_Array_Type (Typ)));

      case K is
         when N_Parameter_Specification =>
            Related_Nod        := Parameter_Type (N);
            Has_Null_Exclusion := Null_Exclusion_Present (N);

         when N_Object_Declaration =>
            Related_Nod        := Object_Definition (N);
            Has_Null_Exclusion := Null_Exclusion_Present (N);

         when N_Discriminant_Specification =>
            Related_Nod        := Discriminant_Type (N);
            Has_Null_Exclusion := Null_Exclusion_Present (N);

         when N_Component_Declaration =>
            if Present (Access_Definition (Component_Definition (N))) then
               Related_Nod := Component_Definition (N);
               Has_Null_Exclusion :=
                 Null_Exclusion_Present
                   (Access_Definition (Component_Definition (N)));
            else
               Related_Nod :=
                 Subtype_Indication (Component_Definition (N));
               Has_Null_Exclusion :=
                 Null_Exclusion_Present (Component_Definition (N));
            end if;

         when others =>
            raise Program_Error;
      end case;

      --  Enforce legality rule 3.10 (14/1): A null_exclusion is only allowed
      --  of the access subtype does not exclude null.

      if Has_Null_Exclusion
        and then Can_Never_Be_Null (Typ)

         --  No need to check itypes that have the null-excluding attribute
         --  because they were checked at their point of creation

        and then not Is_Itype (Typ)
      then
         Error_Msg_N
           ("(Ada 2005) already a null-excluding type", Related_Nod);
      end if;

      --  Check that null-excluding objects are always initialized

      if K = N_Object_Declaration
        and then not Present (Expression (N))
      then
         --  Add a an expression that assignates null. This node is needed
         --  by Apply_Compile_Time_Constraint_Error, that will replace this
         --  node by a Constraint_Error node.

         Set_Expression (N, Make_Null (Sloc (N)));
         Set_Etype (Expression (N), Etype (Defining_Identifier (N)));

         Apply_Compile_Time_Constraint_Error
           (N      => Expression (N),
            Msg    => "(Ada 2005) null-excluding objects must be initialized?",
            Reason => CE_Null_Not_Allowed);
      end if;

      --  Check that the null value is not used as a single expression to
      --  assignate a value to a null-excluding component, formal or object;
      --  otherwise generate a warning message at the sloc of Related_Nod and
      --  replace Expression (N) by an N_Contraint_Error node.

      declare
         Expr : constant Node_Id := Expression (N);

      begin
         if Present (Expr)
           and then Nkind (Expr) = N_Null
         then
            case K is
               when N_Discriminant_Specification  |
                    N_Component_Declaration      =>
                  Apply_Compile_Time_Constraint_Error
                     (N      => Expr,
                      Msg    => "(Ada 2005) NULL not allowed in"
                                  & " null-excluding components?",
                      Reason => CE_Null_Not_Allowed);

               when N_Parameter_Specification =>
                  Apply_Compile_Time_Constraint_Error
                     (N      => Expr,
                      Msg    => "(Ada 2005) NULL not allowed in"
                                  & " null-excluding formals?",
                      Reason => CE_Null_Not_Allowed);

               when N_Object_Declaration =>
                  Apply_Compile_Time_Constraint_Error
                     (N      => Expr,
                      Msg    => "(Ada 2005) NULL not allowed in"
                                  & " null-excluding objects?",
                      Reason => CE_Null_Not_Allowed);

               when others =>
                  null;
            end case;
         end if;
      end;
   end Null_Exclusion_Static_Checks;

   ----------------------------------
   -- Conditional_Statements_Begin --
   ----------------------------------

   procedure Conditional_Statements_Begin is
   begin
      Saved_Checks_TOS := Saved_Checks_TOS + 1;

      --  If stack overflows, kill all checks, that way we know to
      --  simply reset the number of saved checks to zero on return.
      --  This should never occur in practice.

      if Saved_Checks_TOS > Saved_Checks_Stack'Last then
         Kill_All_Checks;

      --  In the normal case, we just make a new stack entry saving
      --  the current number of saved checks for a later restore.

      else
         Saved_Checks_Stack (Saved_Checks_TOS) := Num_Saved_Checks;

         if Debug_Flag_CC then
            w ("Conditional_Statements_Begin: Num_Saved_Checks = ",
               Num_Saved_Checks);
         end if;
      end if;
   end Conditional_Statements_Begin;

   --------------------------------
   -- Conditional_Statements_End --
   --------------------------------

   procedure Conditional_Statements_End is
   begin
      pragma Assert (Saved_Checks_TOS > 0);

      --  If the saved checks stack overflowed, then we killed all
      --  checks, so setting the number of saved checks back to
      --  zero is correct. This should never occur in practice.

      if Saved_Checks_TOS > Saved_Checks_Stack'Last then
         Num_Saved_Checks := 0;

      --  In the normal case, restore the number of saved checks
      --  from the top stack entry.

      else
         Num_Saved_Checks := Saved_Checks_Stack (Saved_Checks_TOS);
         if Debug_Flag_CC then
            w ("Conditional_Statements_End: Num_Saved_Checks = ",
               Num_Saved_Checks);
         end if;
      end if;

      Saved_Checks_TOS := Saved_Checks_TOS - 1;
   end Conditional_Statements_End;

   ---------------------
   -- Determine_Range --
   ---------------------

   Cache_Size : constant := 2 ** 10;
   type Cache_Index is range 0 .. Cache_Size - 1;
   --  Determine size of below cache (power of 2 is more efficient!)

   Determine_Range_Cache_N  : array (Cache_Index) of Node_Id;
   Determine_Range_Cache_Lo : array (Cache_Index) of Uint;
   Determine_Range_Cache_Hi : array (Cache_Index) of Uint;
   --  The above arrays are used to implement a small direct cache
   --  for Determine_Range calls. Because of the way Determine_Range
   --  recursively traces subexpressions, and because overflow checking
   --  calls the routine on the way up the tree, a quadratic behavior
   --  can otherwise be encountered in large expressions. The cache
   --  entry for node N is stored in the (N mod Cache_Size) entry, and
   --  can be validated by checking the actual node value stored there.

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint)
   is
      Typ : constant Entity_Id := Etype (N);

      Lo_Left : Uint;
      Hi_Left : Uint;
      --  Lo and Hi bounds of left operand

      Lo_Right : Uint;
      Hi_Right : Uint;
      --  Lo and Hi bounds of right (or only) operand

      Bound : Node_Id;
      --  Temp variable used to hold a bound node

      Hbound : Uint;
      --  High bound of base type of expression

      Lor : Uint;
      Hir : Uint;
      --  Refined values for low and high bounds, after tightening

      OK1 : Boolean;
      --  Used in lower level calls to indicate if call succeeded

      Cindex : Cache_Index;
      --  Used to search cache

      function OK_Operands return Boolean;
      --  Used for binary operators. Determines the ranges of the left and
      --  right operands, and if they are both OK, returns True, and puts
      --  the results in Lo_Right, Hi_Right, Lo_Left, Hi_Left

      -----------------
      -- OK_Operands --
      -----------------

      function OK_Operands return Boolean is
      begin
         Determine_Range (Left_Opnd  (N), OK1, Lo_Left,  Hi_Left);

         if not OK1 then
            return False;
         end if;

         Determine_Range (Right_Opnd (N), OK1, Lo_Right, Hi_Right);
         return OK1;
      end OK_Operands;

   --  Start of processing for Determine_Range

   begin
      --  Prevent junk warnings by initializing range variables

      Lo  := No_Uint;
      Hi  := No_Uint;
      Lor := No_Uint;
      Hir := No_Uint;

      --  If the type is not discrete, or is undefined, then we can't
      --  do anything about determining the range.

      if No (Typ) or else not Is_Discrete_Type (Typ)
        or else Error_Posted (N)
      then
         OK := False;
         return;
      end if;

      --  For all other cases, we can determine the range

      OK := True;

      --  If value is compile time known, then the possible range is the
      --  one value that we know this expression definitely has!

      if Compile_Time_Known_Value (N) then
         Lo := Expr_Value (N);
         Hi := Lo;
         return;
      end if;

      --  Return if already in the cache

      Cindex := Cache_Index (N mod Cache_Size);

      if Determine_Range_Cache_N (Cindex) = N then
         Lo := Determine_Range_Cache_Lo (Cindex);
         Hi := Determine_Range_Cache_Hi (Cindex);
         return;
      end if;

      --  Otherwise, start by finding the bounds of the type of the
      --  expression, the value cannot be outside this range (if it
      --  is, then we have an overflow situation, which is a separate
      --  check, we are talking here only about the expression value).

      --  We use the actual bound unless it is dynamic, in which case
      --  use the corresponding base type bound if possible. If we can't
      --  get a bound then we figure we can't determine the range (a
      --  peculiar case, that perhaps cannot happen, but there is no
      --  point in bombing in this optimization circuit.

      --  First the low bound

      Bound := Type_Low_Bound (Typ);

      if Compile_Time_Known_Value (Bound) then
         Lo := Expr_Value (Bound);

      elsif Compile_Time_Known_Value (Type_Low_Bound (Base_Type (Typ))) then
         Lo := Expr_Value (Type_Low_Bound (Base_Type (Typ)));

      else
         OK := False;
         return;
      end if;

      --  Now the high bound

      Bound := Type_High_Bound (Typ);

      --  We need the high bound of the base type later on, and this should
      --  always be compile time known. Again, it is not clear that this
      --  can ever be false, but no point in bombing.

      if Compile_Time_Known_Value (Type_High_Bound (Base_Type (Typ))) then
         Hbound := Expr_Value (Type_High_Bound (Base_Type (Typ)));
         Hi := Hbound;

      else
         OK := False;
         return;
      end if;

      --  If we have a static subtype, then that may have a tighter bound
      --  so use the upper bound of the subtype instead in this case.

      if Compile_Time_Known_Value (Bound) then
         Hi := Expr_Value (Bound);
      end if;

      --  We may be able to refine this value in certain situations. If
      --  refinement is possible, then Lor and Hir are set to possibly
      --  tighter bounds, and OK1 is set to True.

      case Nkind (N) is

         --  For unary plus, result is limited by range of operand

         when N_Op_Plus =>
            Determine_Range (Right_Opnd (N), OK1, Lor, Hir);

         --  For unary minus, determine range of operand, and negate it

         when N_Op_Minus =>
            Determine_Range (Right_Opnd (N), OK1, Lo_Right, Hi_Right);

            if OK1 then
               Lor := -Hi_Right;
               Hir := -Lo_Right;
            end if;

         --  For binary addition, get range of each operand and do the
         --  addition to get the result range.

         when N_Op_Add =>
            if OK_Operands then
               Lor := Lo_Left + Lo_Right;
               Hir := Hi_Left + Hi_Right;
            end if;

         --  Division is tricky. The only case we consider is where the
         --  right operand is a positive constant, and in this case we
         --  simply divide the bounds of the left operand

         when N_Op_Divide =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right > 0
               then
                  Lor := Lo_Left / Lo_Right;
                  Hir := Hi_Left / Lo_Right;

               else
                  OK1 := False;
               end if;
            end if;

         --  For binary subtraction, get range of each operand and do
         --  the worst case subtraction to get the result range.

         when N_Op_Subtract =>
            if OK_Operands then
               Lor := Lo_Left - Hi_Right;
               Hir := Hi_Left - Lo_Right;
            end if;

         --  For MOD, if right operand is a positive constant, then
         --  result must be in the allowable range of mod results.

         when N_Op_Mod =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right /= 0
               then
                  if Lo_Right > 0 then
                     Lor := Uint_0;
                     Hir := Lo_Right - 1;

                  else -- Lo_Right < 0
                     Lor := Lo_Right + 1;
                     Hir := Uint_0;
                  end if;

               else
                  OK1 := False;
               end if;
            end if;

         --  For REM, if right operand is a positive constant, then
         --  result must be in the allowable range of mod results.

         when N_Op_Rem =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right /= 0
               then
                  declare
                     Dval : constant Uint := (abs Lo_Right) - 1;

                  begin
                     --  The sign of the result depends on the sign of the
                     --  dividend (but not on the sign of the divisor, hence
                     --  the abs operation above).

                     if Lo_Left < 0 then
                        Lor := -Dval;
                     else
                        Lor := Uint_0;
                     end if;

                     if Hi_Left < 0 then
                        Hir := Uint_0;
                     else
                        Hir := Dval;
                     end if;
                  end;

               else
                  OK1 := False;
               end if;
            end if;

         --  Attribute reference cases

         when N_Attribute_Reference =>
            case Attribute_Name (N) is

               --  For Pos/Val attributes, we can refine the range using the
               --  possible range of values of the attribute expression

               when Name_Pos | Name_Val =>
                  Determine_Range (First (Expressions (N)), OK1, Lor, Hir);

               --  For Length attribute, use the bounds of the corresponding
               --  index type to refine the range.

               when Name_Length =>
                  declare
                     Atyp : Entity_Id := Etype (Prefix (N));
                     Inum : Nat;
                     Indx : Node_Id;

                     LL, LU : Uint;
                     UL, UU : Uint;

                  begin
                     if Is_Access_Type (Atyp) then
                        Atyp := Designated_Type (Atyp);
                     end if;

                     --  For string literal, we know exact value

                     if Ekind (Atyp) = E_String_Literal_Subtype then
                        OK := True;
                        Lo := String_Literal_Length (Atyp);
                        Hi := String_Literal_Length (Atyp);
                        return;
                     end if;

                     --  Otherwise check for expression given

                     if No (Expressions (N)) then
                        Inum := 1;
                     else
                        Inum :=
                          UI_To_Int (Expr_Value (First (Expressions (N))));
                     end if;

                     Indx := First_Index (Atyp);
                     for J in 2 .. Inum loop
                        Indx := Next_Index (Indx);
                     end loop;

                     Determine_Range
                       (Type_Low_Bound (Etype (Indx)), OK1, LL, LU);

                     if OK1 then
                        Determine_Range
                          (Type_High_Bound (Etype (Indx)), OK1, UL, UU);

                        if OK1 then

                           --  The maximum value for Length is the biggest
                           --  possible gap between the values of the bounds.
                           --  But of course, this value cannot be negative.

                           Hir := UI_Max (Uint_0, UU - LL);

                           --  For constrained arrays, the minimum value for
                           --  Length is taken from the actual value of the
                           --  bounds, since the index will be exactly of
                           --  this subtype.

                           if Is_Constrained (Atyp) then
                              Lor := UI_Max (Uint_0, UL - LU);

                           --  For an unconstrained array, the minimum value
                           --  for length is always zero.

                           else
                              Lor := Uint_0;
                           end if;
                        end if;
                     end if;
                  end;

               --  No special handling for other attributes
               --  Probably more opportunities exist here ???

               when others =>
                  OK1 := False;

            end case;

         --  For type conversion from one discrete type to another, we
         --  can refine the range using the converted value.

         when N_Type_Conversion =>
            Determine_Range (Expression (N), OK1, Lor, Hir);

         --  Nothing special to do for all other expression kinds

         when others =>
            OK1 := False;
            Lor := No_Uint;
            Hir := No_Uint;
      end case;

      --  At this stage, if OK1 is true, then we know that the actual
      --  result of the computed expression is in the range Lor .. Hir.
      --  We can use this to restrict the possible range of results.

      if OK1 then

         --  If the refined value of the low bound is greater than the
         --  type high bound, then reset it to the more restrictive
         --  value. However, we do NOT do this for the case of a modular
         --  type where the possible upper bound on the value is above the
         --  base type high bound, because that means the result could wrap.

         if Lor > Lo
           and then not (Is_Modular_Integer_Type (Typ)
                           and then Hir > Hbound)
         then
            Lo := Lor;
         end if;

         --  Similarly, if the refined value of the high bound is less
         --  than the value so far, then reset it to the more restrictive
         --  value. Again, we do not do this if the refined low bound is
         --  negative for a modular type, since this would wrap.

         if Hir < Hi
           and then not (Is_Modular_Integer_Type (Typ)
                          and then Lor < Uint_0)
         then
            Hi := Hir;
         end if;
      end if;

      --  Set cache entry for future call and we are all done

      Determine_Range_Cache_N  (Cindex) := N;
      Determine_Range_Cache_Lo (Cindex) := Lo;
      Determine_Range_Cache_Hi (Cindex) := Hi;
      return;

   --  If any exception occurs, it means that we have some bug in the compiler
   --  possibly triggered by a previous error, or by some unforseen peculiar
   --  occurrence. However, this is only an optimization attempt, so there is
   --  really no point in crashing the compiler. Instead we just decide, too
   --  bad, we can't figure out a range in this case after all.

   exception
      when others =>

         --  Debug flag K disables this behavior (useful for debugging)

         if Debug_Flag_K then
            raise;
         else
            OK := False;
            Lo := No_Uint;
            Hi := No_Uint;
            return;
         end if;
   end Determine_Range;

   ------------------------------------
   -- Discriminant_Checks_Suppressed --
   ------------------------------------

   function Discriminant_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) then
         if Is_Unchecked_Union (E) then
            return True;
         elsif Checks_May_Be_Suppressed (E) then
            return Is_Check_Suppressed (E, Discriminant_Check);
         end if;
      end if;

      return Scope_Suppress (Discriminant_Check);
   end Discriminant_Checks_Suppressed;

   --------------------------------
   -- Division_Checks_Suppressed --
   --------------------------------

   function Division_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Division_Check);
      else
         return Scope_Suppress (Division_Check);
      end if;
   end Division_Checks_Suppressed;

   -----------------------------------
   -- Elaboration_Checks_Suppressed --
   -----------------------------------

   function Elaboration_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      --  The complication in this routine is that if we are in the dynamic
      --  model of elaboration, we also check All_Checks, since All_Checks
      --  does not set Elaboration_Check explicitly.

      if Present (E) then
         if Kill_Elaboration_Checks (E) then
            return True;

         elsif Checks_May_Be_Suppressed (E) then
            if Is_Check_Suppressed (E, Elaboration_Check) then
               return True;
            elsif Dynamic_Elaboration_Checks then
               return Is_Check_Suppressed (E, All_Checks);
            else
               return False;
            end if;
         end if;
      end if;

      if Scope_Suppress (Elaboration_Check) then
         return True;
      elsif Dynamic_Elaboration_Checks then
         return Scope_Suppress (All_Checks);
      else
         return False;
      end if;
   end Elaboration_Checks_Suppressed;

   ---------------------------
   -- Enable_Overflow_Check --
   ---------------------------

   procedure Enable_Overflow_Check (N : Node_Id) is
      Typ : constant Entity_Id  := Base_Type (Etype (N));
      Chk : Nat;
      OK  : Boolean;
      Ent : Entity_Id;
      Ofs : Uint;
      Lo  : Uint;
      Hi  : Uint;

   begin
      if Debug_Flag_CC then
         w ("Enable_Overflow_Check for node ", Int (N));
         Write_Str ("  Source location = ");
         wl (Sloc (N));
         pg (N);
      end if;

      --  Nothing to do if the range of the result is known OK. We skip
      --  this for conversions, since the caller already did the check,
      --  and in any case the condition for deleting the check for a
      --  type conversion is different in any case.

      if Nkind (N) /= N_Type_Conversion then
         Determine_Range (N, OK, Lo, Hi);

         --  Note in the test below that we assume that if a bound of the
         --  range is equal to that of the type. That's not quite accurate
         --  but we do this for the following reasons:

         --   a) The way that Determine_Range works, it will typically report
         --      the bounds of the value as being equal to the bounds of the
         --      type, because it either can't tell anything more precise, or
         --      does not think it is worth the effort to be more precise.

         --   b) It is very unusual to have a situation in which this would
         --      generate an unnecessary overflow check (an example would be
         --      a subtype with a range 0 .. Integer'Last - 1 to which the
         --      literal value one is added.

         --   c) The alternative is a lot of special casing in this routine
         --      which would partially duplicate Determine_Range processing.

         if OK
           and then Lo > Expr_Value (Type_Low_Bound  (Typ))
           and then Hi < Expr_Value (Type_High_Bound (Typ))
         then
            if Debug_Flag_CC then
               w ("No overflow check required");
            end if;

            return;
         end if;
      end if;

      --  If not in optimizing mode, set flag and we are done. We are also
      --  done (and just set the flag) if the type is not a discrete type,
      --  since it is not worth the effort to eliminate checks for other
      --  than discrete types. In addition, we take this same path if we
      --  have stored the maximum number of checks possible already (a
      --  very unlikely situation, but we do not want to blow up!)

      if Optimization_Level = 0
        or else not Is_Discrete_Type (Etype (N))
        or else Num_Saved_Checks = Saved_Checks'Last
      then
         Set_Do_Overflow_Check (N, True);

         if Debug_Flag_CC then
            w ("Optimization off");
         end if;

         return;
      end if;

      --  Otherwise evaluate and check the expression

      Find_Check
        (Expr        => N,
         Check_Type  => 'O',
         Target_Type => Empty,
         Entry_OK    => OK,
         Check_Num   => Chk,
         Ent         => Ent,
         Ofs         => Ofs);

      if Debug_Flag_CC then
         w ("Called Find_Check");
         w ("  OK = ", OK);

         if OK then
            w ("  Check_Num = ", Chk);
            w ("  Ent       = ", Int (Ent));
            Write_Str ("  Ofs       = ");
            pid (Ofs);
         end if;
      end if;

      --  If check is not of form to optimize, then set flag and we are done

      if not OK then
         Set_Do_Overflow_Check (N, True);
         return;
      end if;

      --  If check is already performed, then return without setting flag

      if Chk /= 0 then
         if Debug_Flag_CC then
            w ("Check suppressed!");
         end if;

         return;
      end if;

      --  Here we will make a new entry for the new check

      Set_Do_Overflow_Check (N, True);
      Num_Saved_Checks := Num_Saved_Checks + 1;
      Saved_Checks (Num_Saved_Checks) :=
        (Killed      => False,
         Entity      => Ent,
         Offset      => Ofs,
         Check_Type  => 'O',
         Target_Type => Empty);

      if Debug_Flag_CC then
         w ("Make new entry, check number = ", Num_Saved_Checks);
         w ("  Entity = ", Int (Ent));
         Write_Str ("  Offset = ");
         pid (Ofs);
         w ("  Check_Type = O");
         w ("  Target_Type = Empty");
      end if;

   --  If we get an exception, then something went wrong, probably because
   --  of an error in the structure of the tree due to an incorrect program.
   --  Or it may be a bug in the optimization circuit. In either case the
   --  safest thing is simply to set the check flag unconditionally.

   exception
      when others =>
         Set_Do_Overflow_Check (N, True);

         if Debug_Flag_CC then
            w ("  exception occurred, overflow flag set");
         end if;

         return;
   end Enable_Overflow_Check;

   ------------------------
   -- Enable_Range_Check --
   ------------------------

   procedure Enable_Range_Check (N : Node_Id) is
      Chk  : Nat;
      OK   : Boolean;
      Ent  : Entity_Id;
      Ofs  : Uint;
      Ttyp : Entity_Id;
      P    : Node_Id;

   begin
      --  Return if unchecked type conversion with range check killed.
      --  In this case we never set the flag (that's what Kill_Range_Check
      --  is all about!)

      if Nkind (N) = N_Unchecked_Type_Conversion
        and then Kill_Range_Check (N)
      then
         return;
      end if;

      --  Debug trace output

      if Debug_Flag_CC then
         w ("Enable_Range_Check for node ", Int (N));
         Write_Str ("  Source location = ");
         wl (Sloc (N));
         pg (N);
      end if;

      --  If not in optimizing mode, set flag and we are done. We are also
      --  done (and just set the flag) if the type is not a discrete type,
      --  since it is not worth the effort to eliminate checks for other
      --  than discrete types. In addition, we take this same path if we
      --  have stored the maximum number of checks possible already (a
      --  very unlikely situation, but we do not want to blow up!)

      if Optimization_Level = 0
        or else No (Etype (N))
        or else not Is_Discrete_Type (Etype (N))
        or else Num_Saved_Checks = Saved_Checks'Last
      then
         Set_Do_Range_Check (N, True);

         if Debug_Flag_CC then
            w ("Optimization off");
         end if;

         return;
      end if;

      --  Otherwise find out the target type

      P := Parent (N);

      --  For assignment, use left side subtype

      if Nkind (P) = N_Assignment_Statement
        and then Expression (P) = N
      then
         Ttyp := Etype (Name (P));

      --  For indexed component, use subscript subtype

      elsif Nkind (P) = N_Indexed_Component then
         declare
            Atyp : Entity_Id;
            Indx : Node_Id;
            Subs : Node_Id;

         begin
            Atyp := Etype (Prefix (P));

            if Is_Access_Type (Atyp) then
               Atyp := Designated_Type (Atyp);

               --  If the prefix is an access to an unconstrained array,
               --  perform check unconditionally: it depends on the bounds
               --  of an object and we cannot currently recognize whether
               --  the test may be redundant.

               if not Is_Constrained (Atyp) then
                  Set_Do_Range_Check (N, True);
                  return;
               end if;

            --  Ditto if the prefix is an explicit dereference whose
            --  designated type is unconstrained.

            elsif Nkind (Prefix (P)) = N_Explicit_Dereference
              and then not Is_Constrained (Atyp)
            then
               Set_Do_Range_Check (N, True);
               return;
            end if;

            Indx := First_Index (Atyp);
            Subs := First (Expressions (P));
            loop
               if Subs = N then
                  Ttyp := Etype (Indx);
                  exit;
               end if;

               Next_Index (Indx);
               Next (Subs);
            end loop;
         end;

      --  For now, ignore all other cases, they are not so interesting

      else
         if Debug_Flag_CC then
            w ("  target type not found, flag set");
         end if;

         Set_Do_Range_Check (N, True);
         return;
      end if;

      --  Evaluate and check the expression

      Find_Check
        (Expr        => N,
         Check_Type  => 'R',
         Target_Type => Ttyp,
         Entry_OK    => OK,
         Check_Num   => Chk,
         Ent         => Ent,
         Ofs         => Ofs);

      if Debug_Flag_CC then
         w ("Called Find_Check");
         w ("Target_Typ = ", Int (Ttyp));
         w ("  OK = ", OK);

         if OK then
            w ("  Check_Num = ", Chk);
            w ("  Ent       = ", Int (Ent));
            Write_Str ("  Ofs       = ");
            pid (Ofs);
         end if;
      end if;

      --  If check is not of form to optimize, then set flag and we are done

      if not OK then
         if Debug_Flag_CC then
            w ("  expression not of optimizable type, flag set");
         end if;

         Set_Do_Range_Check (N, True);
         return;
      end if;

      --  If check is already performed, then return without setting flag

      if Chk /= 0 then
         if Debug_Flag_CC then
            w ("Check suppressed!");
         end if;

         return;
      end if;

      --  Here we will make a new entry for the new check

      Set_Do_Range_Check (N, True);
      Num_Saved_Checks := Num_Saved_Checks + 1;
      Saved_Checks (Num_Saved_Checks) :=
        (Killed      => False,
         Entity      => Ent,
         Offset      => Ofs,
         Check_Type  => 'R',
         Target_Type => Ttyp);

      if Debug_Flag_CC then
         w ("Make new entry, check number = ", Num_Saved_Checks);
         w ("  Entity = ", Int (Ent));
         Write_Str ("  Offset = ");
         pid (Ofs);
         w ("  Check_Type = R");
         w ("  Target_Type = ", Int (Ttyp));
         pg (Ttyp);
      end if;

   --  If we get an exception, then something went wrong, probably because
   --  of an error in the structure of the tree due to an incorrect program.
   --  Or it may be a bug in the optimization circuit. In either case the
   --  safest thing is simply to set the check flag unconditionally.

   exception
      when others =>
         Set_Do_Range_Check (N, True);

         if Debug_Flag_CC then
            w ("  exception occurred, range flag set");
         end if;

         return;
   end Enable_Range_Check;

   ------------------
   -- Ensure_Valid --
   ------------------

   procedure Ensure_Valid (Expr : Node_Id; Holes_OK : Boolean := False) is
      Typ : constant Entity_Id  := Etype (Expr);

   begin
      --  Ignore call if we are not doing any validity checking

      if not Validity_Checks_On then
         return;

      --  Ignore call if range checks suppressed on entity in question

      elsif Is_Entity_Name (Expr)
        and then Range_Checks_Suppressed (Entity (Expr))
      then
         return;

      --  No check required if expression is from the expander, we assume
      --  the expander will generate whatever checks are needed. Note that
      --  this is not just an optimization, it avoids infinite recursions!

      --  Unchecked conversions must be checked, unless they are initialized
      --  scalar values, as in a component assignment in an init proc.

      --  In addition, we force a check if Force_Validity_Checks is set

      elsif not Comes_From_Source (Expr)
        and then not Force_Validity_Checks
        and then (Nkind (Expr) /= N_Unchecked_Type_Conversion
                    or else Kill_Range_Check (Expr))
      then
         return;

      --  No check required if expression is known to have valid value

      elsif Expr_Known_Valid (Expr) then
         return;

      --  No check required if checks off

      elsif Range_Checks_Suppressed (Typ) then
         return;

      --  Ignore case of enumeration with holes where the flag is set not
      --  to worry about holes, since no special validity check is needed

      elsif Is_Enumeration_Type (Typ)
        and then Has_Non_Standard_Rep (Typ)
        and then Holes_OK
      then
         return;

      --  No check required on the left-hand side of an assignment

      elsif Nkind (Parent (Expr)) = N_Assignment_Statement
        and then Expr = Name (Parent (Expr))
      then
         return;

      --  No check on a univeral real constant. The context will eventually
      --  convert it to a machine number for some target type, or report an
      --  illegality.

      elsif Nkind (Expr) = N_Real_Literal
        and then Etype (Expr) = Universal_Real
      then
         return;

      --  An annoying special case. If this is an out parameter of a scalar
      --  type, then the value is not going to be accessed, therefore it is
      --  inappropriate to do any validity check at the call site.

      else
         --  Only need to worry about scalar types

         if Is_Scalar_Type (Typ) then
            declare
               P : Node_Id;
               N : Node_Id;
               E : Entity_Id;
               F : Entity_Id;
               A : Node_Id;
               L : List_Id;

            begin
               --  Find actual argument (which may be a parameter association)
               --  and the parent of the actual argument (the call statement)

               N := Expr;
               P := Parent (Expr);

               if Nkind (P) = N_Parameter_Association then
                  N := P;
                  P := Parent (N);
               end if;

               --  Only need to worry if we are argument of a procedure
               --  call since functions don't have out parameters. If this
               --  is an indirect or dispatching call, get signature from
               --  the subprogram type.

               if Nkind (P) = N_Procedure_Call_Statement then
                  L := Parameter_Associations (P);

                  if Is_Entity_Name (Name (P)) then
                     E := Entity (Name (P));
                  else
                     pragma Assert (Nkind (Name (P)) = N_Explicit_Dereference);
                     E := Etype (Name (P));
                  end if;

                  --  Only need to worry if there are indeed actuals, and
                  --  if this could be a procedure call, otherwise we cannot
                  --  get a match (either we are not an argument, or the
                  --  mode of the formal is not OUT). This test also filters
                  --  out the generic case.

                  if Is_Non_Empty_List (L)
                    and then Is_Subprogram (E)
                  then
                     --  This is the loop through parameters, looking to
                     --  see if there is an OUT parameter for which we are
                     --  the argument.

                     F := First_Formal (E);
                     A := First (L);

                     while Present (F) loop
                        if Ekind (F) = E_Out_Parameter and then A = N then
                           return;
                        end if;

                        Next_Formal (F);
                        Next (A);
                     end loop;
                  end if;
               end if;
            end;
         end if;
      end if;

      --  If we fall through, a validity check is required. Note that it would
      --  not be good to set Do_Range_Check, even in contexts where this is
      --  permissible, since this flag causes checking against the target type,
      --  not the source type in contexts such as assignments

      Insert_Valid_Check (Expr);
   end Ensure_Valid;

   ----------------------
   -- Expr_Known_Valid --
   ----------------------

   function Expr_Known_Valid (Expr : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Expr);

   begin
      --  Non-scalar types are always considered valid, since they never
      --  give rise to the issues of erroneous or bounded error behavior
      --  that are the concern. In formal reference manual terms the
      --  notion of validity only applies to scalar types. Note that
      --  even when packed arrays are represented using modular types,
      --  they are still arrays semantically, so they are also always
      --  valid (in particular, the unused bits can be random rubbish
      --  without affecting the validity of the array value).

      if not Is_Scalar_Type (Typ) or else Is_Packed_Array_Type (Typ) then
         return True;

      --  If no validity checking, then everything is considered valid

      elsif not Validity_Checks_On then
         return True;

      --  Floating-point types are considered valid unless floating-point
      --  validity checks have been specifically turned on.

      elsif Is_Floating_Point_Type (Typ)
        and then not Validity_Check_Floating_Point
      then
         return True;

      --  If the expression is the value of an object that is known to
      --  be valid, then clearly the expression value itself is valid.

      elsif Is_Entity_Name (Expr)
        and then Is_Known_Valid (Entity (Expr))
      then
         return True;

      --  If the type is one for which all values are known valid, then
      --  we are sure that the value is valid except in the slightly odd
      --  case where the expression is a reference to a variable whose size
      --  has been explicitly set to a value greater than the object size.

      elsif Is_Known_Valid (Typ) then
         if Is_Entity_Name (Expr)
           and then Ekind (Entity (Expr)) = E_Variable
           and then Esize (Entity (Expr)) > Esize (Typ)
         then
            return False;
         else
            return True;
         end if;

      --  Integer and character literals always have valid values, where
      --  appropriate these will be range checked in any case.

      elsif Nkind (Expr) = N_Integer_Literal
              or else
            Nkind (Expr) = N_Character_Literal
      then
         return True;

      --  If we have a type conversion or a qualification of a known valid
      --  value, then the result will always be valid.

      elsif Nkind (Expr) = N_Type_Conversion
              or else
            Nkind (Expr) = N_Qualified_Expression
      then
         return Expr_Known_Valid (Expression (Expr));

      --  The result of any operator is always considered valid, since we
      --  assume the necessary checks are done by the operator. For operators
      --  on floating-point operations, we must also check when the operation
      --  is the right-hand side of an assignment, or is an actual in a call.

      elsif
        Nkind (Expr) in N_Binary_Op or else Nkind (Expr) in N_Unary_Op
      then
         if Is_Floating_Point_Type (Typ)
            and then Validity_Check_Floating_Point
            and then
              (Nkind (Parent (Expr)) = N_Assignment_Statement
                or else Nkind (Parent (Expr)) = N_Function_Call
                or else Nkind (Parent (Expr)) = N_Parameter_Association)
         then
            return False;
         else
            return True;
         end if;

      --  For all other cases, we do not know the expression is valid

      else
         return False;
      end if;
   end Expr_Known_Valid;

   ----------------
   -- Find_Check --
   ----------------

   procedure Find_Check
     (Expr        : Node_Id;
      Check_Type  : Character;
      Target_Type : Entity_Id;
      Entry_OK    : out Boolean;
      Check_Num   : out Nat;
      Ent         : out Entity_Id;
      Ofs         : out Uint)
   is
      function Within_Range_Of
        (Target_Type : Entity_Id;
         Check_Type  : Entity_Id) return Boolean;
      --  Given a requirement for checking a range against Target_Type, and
      --  and a range Check_Type against which a check has already been made,
      --  determines if the check against check type is sufficient to ensure
      --  that no check against Target_Type is required.

      ---------------------
      -- Within_Range_Of --
      ---------------------

      function Within_Range_Of
        (Target_Type : Entity_Id;
         Check_Type  : Entity_Id) return Boolean
      is
      begin
         if Target_Type = Check_Type then
            return True;

         else
            declare
               Tlo : constant Node_Id := Type_Low_Bound  (Target_Type);
               Thi : constant Node_Id := Type_High_Bound (Target_Type);
               Clo : constant Node_Id := Type_Low_Bound  (Check_Type);
               Chi : constant Node_Id := Type_High_Bound (Check_Type);

            begin
               if (Tlo = Clo
                     or else (Compile_Time_Known_Value (Tlo)
                                and then
                              Compile_Time_Known_Value (Clo)
                                and then
                              Expr_Value (Clo) >= Expr_Value (Tlo)))
                 and then
                  (Thi = Chi
                     or else (Compile_Time_Known_Value (Thi)
                                and then
                              Compile_Time_Known_Value (Chi)
                                and then
                              Expr_Value (Chi) <= Expr_Value (Clo)))
               then
                  return True;
               else
                  return False;
               end if;
            end;
         end if;
      end Within_Range_Of;

   --  Start of processing for Find_Check

   begin
      --  Establish default, to avoid warnings from GCC

      Check_Num := 0;

      --  Case of expression is simple entity reference

      if Is_Entity_Name (Expr) then
         Ent := Entity (Expr);
         Ofs := Uint_0;

      --  Case of expression is entity + known constant

      elsif Nkind (Expr) = N_Op_Add
        and then Compile_Time_Known_Value (Right_Opnd (Expr))
        and then Is_Entity_Name (Left_Opnd (Expr))
      then
         Ent := Entity (Left_Opnd (Expr));
         Ofs := Expr_Value (Right_Opnd (Expr));

      --  Case of expression is entity - known constant

      elsif Nkind (Expr) = N_Op_Subtract
        and then Compile_Time_Known_Value (Right_Opnd (Expr))
        and then Is_Entity_Name (Left_Opnd (Expr))
      then
         Ent := Entity (Left_Opnd (Expr));
         Ofs := UI_Negate (Expr_Value (Right_Opnd (Expr)));

      --  Any other expression is not of the right form

      else
         Ent := Empty;
         Ofs := Uint_0;
         Entry_OK := False;
         return;
      end if;

      --  Come here with expression of appropriate form, check if
      --  entity is an appropriate one for our purposes.

      if (Ekind (Ent) = E_Variable
            or else
          Ekind (Ent) = E_Constant
            or else
          Ekind (Ent) = E_Loop_Parameter
            or else
          Ekind (Ent) = E_In_Parameter)
        and then not Is_Library_Level_Entity (Ent)
      then
         Entry_OK := True;
      else
         Entry_OK := False;
         return;
      end if;

      --  See if there is matching check already

      for J in reverse 1 .. Num_Saved_Checks loop
         declare
            SC : Saved_Check renames Saved_Checks (J);

         begin
            if SC.Killed = False
              and then SC.Entity = Ent
              and then SC.Offset = Ofs
              and then SC.Check_Type = Check_Type
              and then Within_Range_Of (Target_Type, SC.Target_Type)
            then
               Check_Num := J;
               return;
            end if;
         end;
      end loop;

      --  If we fall through entry was not found

      Check_Num := 0;
      return;
   end Find_Check;

   ---------------------------------
   -- Generate_Discriminant_Check --
   ---------------------------------

   --  Note: the code for this procedure is derived from the
   --  emit_discriminant_check routine a-trans.c v1.659.

   procedure Generate_Discriminant_Check (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Pref : constant Node_Id    := Prefix (N);
      Sel  : constant Node_Id    := Selector_Name (N);

      Orig_Comp : constant Entity_Id :=
                    Original_Record_Component (Entity (Sel));
      --  The original component to be checked

      Discr_Fct : constant Entity_Id :=
                    Discriminant_Checking_Func (Orig_Comp);
      --  The discriminant checking function

      Discr : Entity_Id;
      --  One discriminant to be checked in the type

      Real_Discr : Entity_Id;
      --  Actual discriminant in the call

      Pref_Type : Entity_Id;
      --  Type of relevant prefix (ignoring private/access stuff)

      Args : List_Id;
      --  List of arguments for function call

      Formal : Entity_Id;
      --  Keep track of the formal corresponding to the actual we build
      --  for each discriminant, in order to be able to perform the
      --  necessary type conversions.

      Scomp : Node_Id;
      --  Selected component reference for checking function argument

   begin
      Pref_Type := Etype (Pref);

      --  Force evaluation of the prefix, so that it does not get evaluated
      --  twice (once for the check, once for the actual reference). Such a
      --  double evaluation is always a potential source of inefficiency,
      --  and is functionally incorrect in the volatile case, or when the
      --  prefix may have side-effects. An entity or a component of an
      --  entity requires no evaluation.

      if Is_Entity_Name (Pref) then
         if Treat_As_Volatile (Entity (Pref)) then
            Force_Evaluation (Pref, Name_Req => True);
         end if;

      elsif Treat_As_Volatile (Etype (Pref)) then
            Force_Evaluation (Pref, Name_Req => True);

      elsif Nkind (Pref) = N_Selected_Component
        and then Is_Entity_Name (Prefix (Pref))
      then
         null;

      else
         Force_Evaluation (Pref, Name_Req => True);
      end if;

      --  For a tagged type, use the scope of the original component to
      --  obtain the type, because ???

      if Is_Tagged_Type (Scope (Orig_Comp)) then
         Pref_Type := Scope (Orig_Comp);

      --  For an untagged derived type, use the discriminants of the
      --  parent which have been renamed in the derivation, possibly
      --  by a one-to-many discriminant constraint.
      --  For non-tagged type, initially get the Etype of the prefix

      else
         if Is_Derived_Type (Pref_Type)
           and then Number_Discriminants (Pref_Type) /=
                    Number_Discriminants (Etype (Base_Type (Pref_Type)))
         then
            Pref_Type := Etype (Base_Type (Pref_Type));
         end if;
      end if;

      --  We definitely should have a checking function, This routine should
      --  not be called if no discriminant checking function is present.

      pragma Assert (Present (Discr_Fct));

      --  Create the list of the actual parameters for the call. This list
      --  is the list of the discriminant fields of the record expression to
      --  be discriminant checked.

      Args   := New_List;
      Formal := First_Formal (Discr_Fct);
      Discr  := First_Discriminant (Pref_Type);
      while Present (Discr) loop

         --  If we have a corresponding discriminant field, and a parent
         --  subtype is present, then we want to use the corresponding
         --  discriminant since this is the one with the useful value.

         if Present (Corresponding_Discriminant (Discr))
           and then Ekind (Pref_Type) = E_Record_Type
           and then Present (Parent_Subtype (Pref_Type))
         then
            Real_Discr := Corresponding_Discriminant (Discr);
         else
            Real_Discr := Discr;
         end if;

         --  Construct the reference to the discriminant

         Scomp :=
           Make_Selected_Component (Loc,
             Prefix =>
               Unchecked_Convert_To (Pref_Type,
                 Duplicate_Subexpr (Pref)),
             Selector_Name => New_Occurrence_Of (Real_Discr, Loc));

         --  Manually analyze and resolve this selected component. We really
         --  want it just as it appears above, and do not want the expander
         --  playing discriminal games etc with this reference. Then we
         --  append the argument to the list we are gathering.

         Set_Etype (Scomp, Etype (Real_Discr));
         Set_Analyzed (Scomp, True);
         Append_To (Args, Convert_To (Etype (Formal), Scomp));

         Next_Formal_With_Extras (Formal);
         Next_Discriminant (Discr);
      end loop;

      --  Now build and insert the call

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (Discr_Fct, Loc),
              Parameter_Associations => Args),
          Reason => CE_Discriminant_Check_Failed));
   end Generate_Discriminant_Check;

   ---------------------------
   -- Generate_Index_Checks --
   ---------------------------

   procedure Generate_Index_Checks (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      A   : constant Node_Id    := Prefix (N);
      Sub : Node_Id;
      Ind : Nat;
      Num : List_Id;

   begin
      Sub := First (Expressions (N));
      Ind := 1;
      while Present (Sub) loop
         if Do_Range_Check (Sub) then
            Set_Do_Range_Check (Sub, False);

            --  Force evaluation except for the case of a simple name of
            --  a non-volatile entity.

            if not Is_Entity_Name (Sub)
              or else Treat_As_Volatile (Entity (Sub))
            then
               Force_Evaluation (Sub);
            end if;

            --  Generate a raise of constraint error with the appropriate
            --  reason and a condition of the form:

            --    Base_Type(Sub) not in array'range (subscript)

            --  Note that the reason we generate the conversion to the
            --  base type here is that we definitely want the range check
            --  to take place, even if it looks like the subtype is OK.
            --  Optimization considerations that allow us to omit the
            --  check have already been taken into account in the setting
            --  of the Do_Range_Check flag earlier on.

            if Ind = 1 then
               Num := No_List;
            else
               Num :=  New_List (Make_Integer_Literal (Loc, Ind));
            end if;

            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Not_In (Loc,
                    Left_Opnd  =>
                      Convert_To (Base_Type (Etype (Sub)),
                        Duplicate_Subexpr_Move_Checks (Sub)),
                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Prefix         => Duplicate_Subexpr_Move_Checks (A),
                        Attribute_Name => Name_Range,
                        Expressions    => Num)),
                Reason => CE_Index_Check_Failed));
         end if;

         Ind := Ind + 1;
         Next (Sub);
      end loop;
   end Generate_Index_Checks;

   --------------------------
   -- Generate_Range_Check --
   --------------------------

   procedure Generate_Range_Check
     (N           : Node_Id;
      Target_Type : Entity_Id;
      Reason      : RT_Exception_Code)
   is
      Loc              : constant Source_Ptr := Sloc (N);
      Source_Type      : constant Entity_Id  := Etype (N);
      Source_Base_Type : constant Entity_Id  := Base_Type (Source_Type);
      Target_Base_Type : constant Entity_Id  := Base_Type (Target_Type);

   begin
      --  First special case, if the source type is already within the
      --  range of the target type, then no check is needed (probably we
      --  should have stopped Do_Range_Check from being set in the first
      --  place, but better late than later in preventing junk code!

      --  We do NOT apply this if the source node is a literal, since in
      --  this case the literal has already been labeled as having the
      --  subtype of the target.

      if In_Subrange_Of (Source_Type, Target_Type)
        and then not
          (Nkind (N) = N_Integer_Literal
             or else
           Nkind (N) = N_Real_Literal
             or else
           Nkind (N) = N_Character_Literal
             or else
           (Is_Entity_Name (N)
              and then Ekind (Entity (N)) = E_Enumeration_Literal))
      then
         return;
      end if;

      --  We need a check, so force evaluation of the node, so that it does
      --  not get evaluated twice (once for the check, once for the actual
      --  reference). Such a double evaluation is always a potential source
      --  of inefficiency, and is functionally incorrect in the volatile case.

      if not Is_Entity_Name (N)
        or else Treat_As_Volatile (Entity (N))
      then
         Force_Evaluation (N);
      end if;

      --  The easiest case is when Source_Base_Type and Target_Base_Type
      --  are the same since in this case we can simply do a direct
      --  check of the value of N against the bounds of Target_Type.

      --    [constraint_error when N not in Target_Type]

      --  Note: this is by far the most common case, for example all cases of
      --  checks on the RHS of assignments are in this category, but not all
      --  cases are like this. Notably conversions can involve two types.

      if Source_Base_Type = Target_Base_Type then
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Not_In (Loc,
                 Left_Opnd  => Duplicate_Subexpr (N),
                 Right_Opnd => New_Occurrence_Of (Target_Type, Loc)),
             Reason => Reason));

      --  Next test for the case where the target type is within the bounds
      --  of the base type of the source type, since in this case we can
      --  simply convert these bounds to the base type of T to do the test.

      --    [constraint_error when N not in
      --       Source_Base_Type (Target_Type'First)
      --         ..
      --       Source_Base_Type(Target_Type'Last))]

      --  The conversions will always work and need no check

      elsif In_Subrange_Of (Target_Type, Source_Base_Type) then
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Not_In (Loc,
                 Left_Opnd  => Duplicate_Subexpr (N),

                 Right_Opnd =>
                   Make_Range (Loc,
                     Low_Bound =>
                       Convert_To (Source_Base_Type,
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (Target_Type, Loc),
                           Attribute_Name => Name_First)),

                     High_Bound =>
                       Convert_To (Source_Base_Type,
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (Target_Type, Loc),
                           Attribute_Name => Name_Last)))),
             Reason => Reason));

      --  Note that at this stage we now that the Target_Base_Type is
      --  not in the range of the Source_Base_Type (since even the
      --  Target_Type itself is not in this range). It could still be
      --  the case that the Source_Type is in range of the target base
      --  type, since we have not checked that case.

      --  If that is the case, we can freely convert the source to the
      --  target, and then test the target result against the bounds.

      elsif In_Subrange_Of (Source_Type, Target_Base_Type) then

         --  We make a temporary to hold the value of the converted
         --  value (converted to the base type), and then we will
         --  do the test against this temporary.

         --     Tnn : constant Target_Base_Type := Target_Base_Type (N);
         --     [constraint_error when Tnn not in Target_Type]

         --  Then the conversion itself is replaced by an occurrence of Tnn

         declare
            Tnn : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_Internal_Name ('T'));

         begin
            Insert_Actions (N, New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Tnn,
                Object_Definition   =>
                  New_Occurrence_Of (Target_Base_Type, Loc),
                Constant_Present    => True,
                Expression          =>
                  Make_Type_Conversion (Loc,
                    Subtype_Mark => New_Occurrence_Of (Target_Base_Type, Loc),
                    Expression   => Duplicate_Subexpr (N))),

              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Not_In (Loc,
                    Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                    Right_Opnd => New_Occurrence_Of (Target_Type, Loc)),

                Reason => Reason)));

            Rewrite (N, New_Occurrence_Of (Tnn, Loc));
         end;

      --  At this stage, we know that we have two scalar types, which are
      --  directly convertible, and where neither scalar type has a base
      --  range that is in the range of the other scalar type.

      --  The only way this can happen is with a signed and unsigned type.
      --  So test for these two cases:

      else
         --  Case of the source is unsigned and the target is signed

         if Is_Unsigned_Type (Source_Base_Type)
           and then not Is_Unsigned_Type (Target_Base_Type)
         then
            --  If the source is unsigned and the target is signed, then we
            --  know that the source is not shorter than the target (otherwise
            --  the source base type would be in the target base type range).

            --  In other words, the unsigned type is either the same size
            --  as the target, or it is larger. It cannot be smaller.

            pragma Assert
              (Esize (Source_Base_Type) >= Esize (Target_Base_Type));

            --  We only need to check the low bound if the low bound of the
            --  target type is non-negative. If the low bound of the target
            --  type is negative, then we know that we will fit fine.

            --  If the high bound of the target type is negative, then we
            --  know we have a constraint error, since we can't possibly
            --  have a negative source.

            --  With these two checks out of the way, we can do the check
            --  using the source type safely

            --  This is definitely the most annoying case!

            --    [constraint_error
            --       when (Target_Type'First >= 0
            --               and then
            --                 N < Source_Base_Type (Target_Type'First))
            --         or else Target_Type'Last < 0
            --         or else N > Source_Base_Type (Target_Type'Last)];

            --  We turn off all checks since we know that the conversions
            --  will work fine, given the guards for negative values.

            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Or_Else (Loc,
                    Make_Or_Else (Loc,
                      Left_Opnd =>
                        Make_And_Then (Loc,
                          Left_Opnd => Make_Op_Ge (Loc,
                            Left_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix =>
                                  New_Occurrence_Of (Target_Type, Loc),
                                Attribute_Name => Name_First),
                            Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),

                          Right_Opnd =>
                            Make_Op_Lt (Loc,
                              Left_Opnd => Duplicate_Subexpr (N),
                              Right_Opnd =>
                                Convert_To (Source_Base_Type,
                                  Make_Attribute_Reference (Loc,
                                    Prefix =>
                                      New_Occurrence_Of (Target_Type, Loc),
                                    Attribute_Name => Name_First)))),

                      Right_Opnd =>
                        Make_Op_Lt (Loc,
                          Left_Opnd =>
                            Make_Attribute_Reference (Loc,
                              Prefix => New_Occurrence_Of (Target_Type, Loc),
                              Attribute_Name => Name_Last),
                            Right_Opnd => Make_Integer_Literal (Loc, Uint_0))),

                    Right_Opnd =>
                      Make_Op_Gt (Loc,
                        Left_Opnd => Duplicate_Subexpr (N),
                        Right_Opnd =>
                          Convert_To (Source_Base_Type,
                            Make_Attribute_Reference (Loc,
                              Prefix => New_Occurrence_Of (Target_Type, Loc),
                              Attribute_Name => Name_Last)))),

                Reason => Reason),
              Suppress  => All_Checks);

         --  Only remaining possibility is that the source is signed and
         --  the target is unsigned

         else
            pragma Assert (not Is_Unsigned_Type (Source_Base_Type)
                             and then Is_Unsigned_Type (Target_Base_Type));

            --  If the source is signed and the target is unsigned, then
            --  we know that the target is not shorter than the source
            --  (otherwise the target base type would be in the source
            --  base type range).

            --  In other words, the unsigned type is either the same size
            --  as the target, or it is larger. It cannot be smaller.

            --  Clearly we have an error if the source value is negative
            --  since no unsigned type can have negative values. If the
            --  source type is non-negative, then the check can be done
            --  using the target type.

            --    Tnn : constant Target_Base_Type (N) := Target_Type;

            --    [constraint_error
            --       when N < 0 or else Tnn not in Target_Type];

            --  We turn off all checks for the conversion of N to the
            --  target base type, since we generate the explicit check
            --  to ensure that the value is non-negative

            declare
               Tnn : constant Entity_Id :=
                       Make_Defining_Identifier (Loc,
                         Chars => New_Internal_Name ('T'));

            begin
               Insert_Actions (N, New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Tnn,
                   Object_Definition   =>
                     New_Occurrence_Of (Target_Base_Type, Loc),
                   Constant_Present    => True,
                   Expression          =>
                     Make_Type_Conversion (Loc,
                       Subtype_Mark =>
                         New_Occurrence_Of (Target_Base_Type, Loc),
                       Expression   => Duplicate_Subexpr (N))),

                 Make_Raise_Constraint_Error (Loc,
                   Condition =>
                     Make_Or_Else (Loc,
                       Left_Opnd =>
                         Make_Op_Lt (Loc,
                           Left_Opnd  => Duplicate_Subexpr (N),
                           Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),

                       Right_Opnd =>
                         Make_Not_In (Loc,
                           Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                           Right_Opnd =>
                             New_Occurrence_Of (Target_Type, Loc))),

                   Reason => Reason)),
                 Suppress => All_Checks);

               --  Set the Etype explicitly, because Insert_Actions may
               --  have placed the declaration in the freeze list for an
               --  enclosing construct, and thus it is not analyzed yet.

               Set_Etype (Tnn, Target_Base_Type);
               Rewrite (N, New_Occurrence_Of (Tnn, Loc));
            end;
         end if;
      end if;
   end Generate_Range_Check;

   ---------------------
   -- Get_Discriminal --
   ---------------------

   function Get_Discriminal (E : Entity_Id; Bound : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (E);
      D   : Entity_Id;
      Sc  : Entity_Id;

   begin
      --  The entity E is the type of a private component of the protected
      --  type, or the type of a renaming of that component within a protected
      --  operation of that type.

      Sc := Scope (E);

      if Ekind (Sc) /= E_Protected_Type then
         Sc := Scope (Sc);

         if Ekind (Sc) /= E_Protected_Type then
            return Bound;
         end if;
      end if;

      D := First_Discriminant (Sc);

      while Present (D)
        and then Chars (D) /= Chars (Bound)
      loop
         Next_Discriminant (D);
      end loop;

      return New_Occurrence_Of (Discriminal (D), Loc);
   end Get_Discriminal;

   ------------------
   -- Guard_Access --
   ------------------

   function Guard_Access
     (Cond    : Node_Id;
      Loc     : Source_Ptr;
      Ck_Node : Node_Id) return Node_Id
   is
   begin
      if Nkind (Cond) = N_Or_Else then
         Set_Paren_Count (Cond, 1);
      end if;

      if Nkind (Ck_Node) = N_Allocator then
         return Cond;
      else
         return
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => Duplicate_Subexpr_No_Checks (Ck_Node),
                 Right_Opnd => Make_Null (Loc)),
             Right_Opnd => Cond);
      end if;
   end Guard_Access;

   -----------------------------
   -- Index_Checks_Suppressed --
   -----------------------------

   function Index_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Index_Check);
      else
         return Scope_Suppress (Index_Check);
      end if;
   end Index_Checks_Suppressed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      for J in Determine_Range_Cache_N'Range loop
         Determine_Range_Cache_N (J) := Empty;
      end loop;
   end Initialize;

   -------------------------
   -- Insert_Range_Checks --
   -------------------------

   procedure Insert_Range_Checks
     (Checks       : Check_Result;
      Node         : Node_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr := No_Location;
      Flag_Node    : Node_Id    := Empty;
      Do_Before    : Boolean    := False)
   is
      Internal_Flag_Node   : Node_Id    := Flag_Node;
      Internal_Static_Sloc : Source_Ptr := Static_Sloc;

      Check_Node : Node_Id;
      Checks_On  : constant Boolean :=
                     (not Index_Checks_Suppressed (Suppress_Typ))
                       or else
                     (not Range_Checks_Suppressed (Suppress_Typ));

   begin
      --  For now we just return if Checks_On is false, however this should
      --  be enhanced to check for an always True value in the condition
      --  and to generate a compilation warning???

      if not Expander_Active or else not Checks_On then
         return;
      end if;

      if Static_Sloc = No_Location then
         Internal_Static_Sloc := Sloc (Node);
      end if;

      if No (Flag_Node) then
         Internal_Flag_Node := Node;
      end if;

      for J in 1 .. 2 loop
         exit when No (Checks (J));

         if Nkind (Checks (J)) = N_Raise_Constraint_Error
           and then Present (Condition (Checks (J)))
         then
            if not Has_Dynamic_Range_Check (Internal_Flag_Node) then
               Check_Node := Checks (J);
               Mark_Rewrite_Insertion (Check_Node);

               if Do_Before then
                  Insert_Before_And_Analyze (Node, Check_Node);
               else
                  Insert_After_And_Analyze (Node, Check_Node);
               end if;

               Set_Has_Dynamic_Range_Check (Internal_Flag_Node);
            end if;

         else
            Check_Node :=
              Make_Raise_Constraint_Error (Internal_Static_Sloc,
                Reason => CE_Range_Check_Failed);
            Mark_Rewrite_Insertion (Check_Node);

            if Do_Before then
               Insert_Before_And_Analyze (Node, Check_Node);
            else
               Insert_After_And_Analyze (Node, Check_Node);
            end if;
         end if;
      end loop;
   end Insert_Range_Checks;

   ------------------------
   -- Insert_Valid_Check --
   ------------------------

   procedure Insert_Valid_Check (Expr : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Expr);
      Exp : Node_Id;

   begin
      --  Do not insert if checks off, or if not checking validity

      if Range_Checks_Suppressed (Etype (Expr))
        or else (not Validity_Checks_On)
      then
         return;
      end if;

      --  If we have a checked conversion, then validity check applies to
      --  the expression inside the conversion, not the result, since if
      --  the expression inside is valid, then so is the conversion result.

      Exp := Expr;
      while Nkind (Exp) = N_Type_Conversion loop
         Exp := Expression (Exp);
      end loop;

      --  Insert the validity check. Note that we do this with validity
      --  checks turned off, to avoid recursion, we do not want validity
      --  checks on the validity checking code itself!

      Validity_Checks_On := False;
      Insert_Action
        (Expr,
         Make_Raise_Constraint_Error (Loc,
           Condition =>
             Make_Op_Not (Loc,
               Right_Opnd =>
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     Duplicate_Subexpr_No_Checks (Exp, Name_Req => True),
                   Attribute_Name => Name_Valid)),
           Reason => CE_Invalid_Data),
         Suppress => All_Checks);

      --  If the expression is a a reference to an element of a bit-packed
      --  array, it is rewritten as a renaming declaration. If the expression
      --  is an actual in a call, it has not been expanded, waiting for the
      --  proper point at which to do it. The same happens with renamings, so
      --  that we have to force the expansion now. This non-local complication
      --  is due to code in exp_ch2,adb, exp_ch4.adb and exp_ch6.adb.

      if Is_Entity_Name (Exp)
        and then Nkind (Parent (Entity (Exp))) = N_Object_Renaming_Declaration
      then
         declare
            Old_Exp : constant Node_Id := Name (Parent (Entity (Exp)));
         begin
            if Nkind (Old_Exp) = N_Indexed_Component
              and then Is_Bit_Packed_Array (Etype (Prefix (Old_Exp)))
            then
               Expand_Packed_Element_Reference (Old_Exp);
            end if;
         end;
      end if;

      Validity_Checks_On := True;
   end Insert_Valid_Check;

   ----------------------------------
   -- Install_Null_Excluding_Check --
   ----------------------------------

   procedure Install_Null_Excluding_Check (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Etyp : constant Entity_Id  := Etype (N);

   begin
      pragma Assert (Is_Access_Type (Etyp));

      --  Don't need access check if:
      --   1) we are analyzing a generic
      --   2) it is known to be non-null
      --   3) the check was suppressed on the type
      --   4) This is an attribute reference that returns an access type.

      if Inside_A_Generic
        or else Access_Checks_Suppressed (Etyp)
      then
         return;
      elsif Nkind (N) = N_Attribute_Reference
        and then
         (Attribute_Name (N) = Name_Access
            or else
          Attribute_Name (N) = Name_Unchecked_Access
            or else
          Attribute_Name (N) = Name_Unrestricted_Access)
      then
         return;
         --  Otherwise install access check

      else
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Op_Eq (Loc,
                 Left_Opnd  => Duplicate_Subexpr_Move_Checks (N),
                 Right_Opnd => Make_Null (Loc)),
             Reason    => CE_Access_Check_Failed));
      end if;
   end Install_Null_Excluding_Check;

   --------------------------
   -- Install_Static_Check --
   --------------------------

   procedure Install_Static_Check (R_Cno : Node_Id; Loc : Source_Ptr) is
      Stat : constant Boolean   := Is_Static_Expression (R_Cno);
      Typ  : constant Entity_Id := Etype (R_Cno);

   begin
      Rewrite (R_Cno,
        Make_Raise_Constraint_Error (Loc,
          Reason => CE_Range_Check_Failed));
      Set_Analyzed (R_Cno);
      Set_Etype (R_Cno, Typ);
      Set_Raises_Constraint_Error (R_Cno);
      Set_Is_Static_Expression (R_Cno, Stat);
   end Install_Static_Check;

   ---------------------
   -- Kill_All_Checks --
   ---------------------

   procedure Kill_All_Checks is
   begin
      if Debug_Flag_CC then
         w ("Kill_All_Checks");
      end if;

      --  We reset the number of saved checks to zero, and also modify
      --  all stack entries for statement ranges to indicate that the
      --  number of checks at each level is now zero.

      Num_Saved_Checks := 0;

      for J in 1 .. Saved_Checks_TOS loop
         Saved_Checks_Stack (J) := 0;
      end loop;
   end Kill_All_Checks;

   -----------------
   -- Kill_Checks --
   -----------------

   procedure Kill_Checks (V : Entity_Id) is
   begin
      if Debug_Flag_CC then
         w ("Kill_Checks for entity", Int (V));
      end if;

      for J in 1 .. Num_Saved_Checks loop
         if Saved_Checks (J).Entity = V then
            if Debug_Flag_CC then
               w ("   Checks killed for saved check ", J);
            end if;

            Saved_Checks (J).Killed := True;
         end if;
      end loop;
   end Kill_Checks;

   ------------------------------
   -- Length_Checks_Suppressed --
   ------------------------------

   function Length_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Length_Check);
      else
         return Scope_Suppress (Length_Check);
      end if;
   end Length_Checks_Suppressed;

   --------------------------------
   -- Overflow_Checks_Suppressed --
   --------------------------------

   function Overflow_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Overflow_Check);
      else
         return Scope_Suppress (Overflow_Check);
      end if;
   end Overflow_Checks_Suppressed;

   -----------------
   -- Range_Check --
   -----------------

   function Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty) return Check_Result
   is
   begin
      return Selected_Range_Checks
        (Ck_Node, Target_Typ, Source_Typ, Warn_Node);
   end Range_Check;

   -----------------------------
   -- Range_Checks_Suppressed --
   -----------------------------

   function Range_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) then

         --  Note: for now we always suppress range checks on Vax float types,
         --  since Gigi does not know how to generate these checks.

         if Vax_Float (E) then
            return True;
         elsif Kill_Range_Checks (E) then
            return True;
         elsif Checks_May_Be_Suppressed (E) then
            return Is_Check_Suppressed (E, Range_Check);
         end if;
      end if;

      return Scope_Suppress (Range_Check);
   end Range_Checks_Suppressed;

   -------------------
   -- Remove_Checks --
   -------------------

   procedure Remove_Checks (Expr : Node_Id) is
      Discard : Traverse_Result;
      pragma Warnings (Off, Discard);

      function Process (N : Node_Id) return Traverse_Result;
      --  Process a single node during the traversal

      function Traverse is new Traverse_Func (Process);
      --  The traversal function itself

      -------------
      -- Process --
      -------------

      function Process (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) not in N_Subexpr then
            return Skip;
         end if;

         Set_Do_Range_Check (N, False);

         case Nkind (N) is
            when N_And_Then =>
               Discard := Traverse (Left_Opnd (N));
               return Skip;

            when N_Attribute_Reference =>
               Set_Do_Overflow_Check (N, False);

            when N_Function_Call =>
               Set_Do_Tag_Check (N, False);

            when N_Op =>
               Set_Do_Overflow_Check (N, False);

               case Nkind (N) is
                  when N_Op_Divide =>
                     Set_Do_Division_Check (N, False);

                  when N_Op_And =>
                     Set_Do_Length_Check (N, False);

                  when N_Op_Mod =>
                     Set_Do_Division_Check (N, False);

                  when N_Op_Or =>
                     Set_Do_Length_Check (N, False);

                  when N_Op_Rem =>
                     Set_Do_Division_Check (N, False);

                  when N_Op_Xor =>
                     Set_Do_Length_Check (N, False);

                  when others =>
                     null;
               end case;

            when N_Or_Else =>
               Discard := Traverse (Left_Opnd (N));
               return Skip;

            when N_Selected_Component =>
               Set_Do_Discriminant_Check (N, False);

            when N_Type_Conversion =>
               Set_Do_Length_Check   (N, False);
               Set_Do_Tag_Check      (N, False);
               Set_Do_Overflow_Check (N, False);

            when others =>
               null;
         end case;

         return OK;
      end Process;

   --  Start of processing for Remove_Checks

   begin
      Discard := Traverse (Expr);
   end Remove_Checks;

   ----------------------------
   -- Selected_Length_Checks --
   ----------------------------

   function Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id) return Check_Result
   is
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      S_Typ       : Entity_Id;
      T_Typ       : Entity_Id;
      Expr_Actual : Node_Id;
      Exptyp      : Entity_Id;
      Cond        : Node_Id := Empty;
      Do_Access   : Boolean := False;
      Wnode       : Node_Id := Warn_Node;
      Ret_Result  : Check_Result := (Empty, Empty);
      Num_Checks  : Natural := 0;

      procedure Add_Check (N : Node_Id);
      --  Adds the action given to Ret_Result if N is non-Empty

      function Get_E_Length (E : Entity_Id; Indx : Nat) return Node_Id;
      function Get_N_Length (N : Node_Id; Indx : Nat) return Node_Id;
      --  Comments required ???

      function Same_Bounds (L : Node_Id; R : Node_Id) return Boolean;
      --  True for equal literals and for nodes that denote the same constant
      --  entity, even if its value is not a static constant. This includes the
      --  case of a discriminal reference within an init proc. Removes some
      --  obviously superfluous checks.

      function Length_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat) return Node_Id;
      --  Returns expression to compute:
      --    Typ'Length /= Exptyp'Length

      function Length_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat) return Node_Id;
      --  Returns expression to compute:
      --    Typ'Length /= Expr'Length

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check (N : Node_Id) is
      begin
         if Present (N) then

            --  For now, ignore attempt to place more than 2 checks ???

            if Num_Checks = 2 then
               return;
            end if;

            pragma Assert (Num_Checks <= 1);
            Num_Checks := Num_Checks + 1;
            Ret_Result (Num_Checks) := N;
         end if;
      end Add_Check;

      ------------------
      -- Get_E_Length --
      ------------------

      function Get_E_Length (E : Entity_Id; Indx : Nat) return Node_Id is
         Pt : constant Entity_Id := Scope (Scope (E));
         N  : Node_Id;
         E1 : Entity_Id := E;

      begin
         if Ekind (Scope (E)) = E_Record_Type
           and then Has_Discriminants (Scope (E))
         then
            N := Build_Discriminal_Subtype_Of_Component (E);

            if Present (N) then
               Insert_Action (Ck_Node, N);
               E1 := Defining_Identifier (N);
            end if;
         end if;

         if Ekind (E1) = E_String_Literal_Subtype then
            return
              Make_Integer_Literal (Loc,
                Intval => String_Literal_Length (E1));

         elsif Ekind (Pt) = E_Protected_Type
           and then Has_Discriminants (Pt)
           and then Has_Completion (Pt)
           and then not Inside_Init_Proc
         then

            --  If the type whose length is needed is a private component
            --  constrained by a discriminant, we must expand the 'Length
            --  attribute into an explicit computation, using the discriminal
            --  of the current protected operation. This is because the actual
            --  type of the prival is constructed after the protected opera-
            --  tion has been fully expanded.

            declare
               Indx_Type : Node_Id;
               Lo        : Node_Id;
               Hi        : Node_Id;
               Do_Expand : Boolean := False;

            begin
               Indx_Type := First_Index (E);

               for J in 1 .. Indx - 1 loop
                  Next_Index (Indx_Type);
               end loop;

               Get_Index_Bounds  (Indx_Type, Lo, Hi);

               if Nkind (Lo) = N_Identifier
                 and then Ekind (Entity (Lo)) = E_In_Parameter
               then
                  Lo := Get_Discriminal (E, Lo);
                  Do_Expand := True;
               end if;

               if Nkind (Hi) = N_Identifier
                 and then Ekind (Entity (Hi)) = E_In_Parameter
               then
                  Hi := Get_Discriminal (E, Hi);
                  Do_Expand := True;
               end if;

               if Do_Expand then
                  if not Is_Entity_Name (Lo) then
                     Lo := Duplicate_Subexpr_No_Checks (Lo);
                  end if;

                  if not Is_Entity_Name (Hi) then
                     Lo := Duplicate_Subexpr_No_Checks (Hi);
                  end if;

                  N :=
                    Make_Op_Add (Loc,
                      Left_Opnd =>
                        Make_Op_Subtract (Loc,
                          Left_Opnd  => Hi,
                          Right_Opnd => Lo),

                      Right_Opnd => Make_Integer_Literal (Loc, 1));
                  return N;

               else
                  N :=
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Length,
                      Prefix =>
                        New_Occurrence_Of (E1, Loc));

                  if Indx > 1 then
                     Set_Expressions (N, New_List (
                       Make_Integer_Literal (Loc, Indx)));
                  end if;

                  return N;
               end if;
            end;

         else
            N :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Length,
                Prefix =>
                  New_Occurrence_Of (E1, Loc));

            if Indx > 1 then
               Set_Expressions (N, New_List (
                 Make_Integer_Literal (Loc, Indx)));
            end if;

            return N;

         end if;
      end Get_E_Length;

      ------------------
      -- Get_N_Length --
      ------------------

      function Get_N_Length (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Length,
             Prefix =>
               Duplicate_Subexpr_No_Checks (N, Name_Req => True),
             Expressions => New_List (
               Make_Integer_Literal (Loc, Indx)));

      end Get_N_Length;

      -------------------
      -- Length_E_Cond --
      -------------------

      function Length_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat) return Node_Id
      is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => Get_E_Length (Typ, Indx),
             Right_Opnd => Get_E_Length (Exptyp, Indx));

      end Length_E_Cond;

      -------------------
      -- Length_N_Cond --
      -------------------

      function Length_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat) return Node_Id
      is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => Get_E_Length (Typ, Indx),
             Right_Opnd => Get_N_Length (Expr, Indx));

      end Length_N_Cond;

      function Same_Bounds (L : Node_Id; R : Node_Id) return Boolean is
      begin
         return
           (Nkind (L) = N_Integer_Literal
             and then Nkind (R) = N_Integer_Literal
             and then Intval (L) = Intval (R))

          or else
            (Is_Entity_Name (L)
              and then Ekind (Entity (L)) = E_Constant
              and then ((Is_Entity_Name (R)
                         and then Entity (L) = Entity (R))
                        or else
                       (Nkind (R) = N_Type_Conversion
                         and then Is_Entity_Name (Expression (R))
                         and then Entity (L) = Entity (Expression (R)))))

          or else
            (Is_Entity_Name (R)
              and then Ekind (Entity (R)) = E_Constant
              and then Nkind (L) = N_Type_Conversion
              and then Is_Entity_Name (Expression (L))
              and then Entity (R) = Entity (Expression (L)))

         or else
            (Is_Entity_Name (L)
              and then Is_Entity_Name (R)
              and then Entity (L) = Entity (R)
              and then Ekind (Entity (L)) = E_In_Parameter
              and then Inside_Init_Proc);
      end Same_Bounds;

   --  Start of processing for Selected_Length_Checks

   begin
      if not Expander_Active then
         return Ret_Result;
      end if;

      if Target_Typ = Any_Type
        or else Target_Typ = Any_Composite
        or else Raises_Constraint_Error (Ck_Node)
      then
         return Ret_Result;
      end if;

      if No (Wnode) then
         Wnode := Ck_Node;
      end if;

      T_Typ := Target_Typ;

      if No (Source_Typ) then
         S_Typ := Etype (Ck_Node);
      else
         S_Typ := Source_Typ;
      end if;

      if S_Typ = Any_Type or else S_Typ = Any_Composite then
         return Ret_Result;
      end if;

      if Is_Access_Type (T_Typ) and then Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);
         T_Typ := Designated_Type (T_Typ);
         Do_Access := True;

         --  A simple optimization

         if Nkind (Ck_Node) = N_Null then
            return Ret_Result;
         end if;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            --  The checking code to be generated will freeze the
            --  corresponding array type. However, we must freeze the
            --  type now, so that the freeze node does not appear within
            --  the generated condional expression, but ahead of it.

            Freeze_Before (Ck_Node, T_Typ);

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Expr_Actual);

            if Is_Access_Type (Exptyp) then
               Exptyp := Designated_Type (Exptyp);
            end if;

            --  String_Literal case. This needs to be handled specially be-
            --  cause no index types are available for string literals. The
            --  condition is simply:

            --    T_Typ'Length = string-literal-length

            if Nkind (Expr_Actual) = N_String_Literal
              and then Ekind (Etype (Expr_Actual)) = E_String_Literal_Subtype
            then
               Cond :=
                 Make_Op_Ne (Loc,
                   Left_Opnd  => Get_E_Length (T_Typ, 1),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                       Intval =>
                         String_Literal_Length (Etype (Expr_Actual))));

            --  General array case. Here we have a usable actual subtype for
            --  the expression, and the condition is built from the two types
            --  (Do_Length):

            --     T_Typ'Length     /= Exptyp'Length     or else
            --     T_Typ'Length (2) /= Exptyp'Length (2) or else
            --     T_Typ'Length (3) /= Exptyp'Length (3) or else
            --     ...

            elsif Is_Constrained (Exptyp) then
               declare
                  Ndims : constant Nat := Number_Dimensions (T_Typ);

                  L_Index  : Node_Id;
                  R_Index  : Node_Id;
                  L_Low    : Node_Id;
                  L_High   : Node_Id;
                  R_Low    : Node_Id;
                  R_High   : Node_Id;
                  L_Length : Uint;
                  R_Length : Uint;
                  Ref_Node : Node_Id;

               begin

                  --  At the library level, we need to ensure that the
                  --  type of the object is elaborated before the check
                  --  itself is emitted. This is only done if the object
                  --  is in the current compilation unit, otherwise the
                  --  type is frozen and elaborated in its unit.

                  if Is_Itype (Exptyp)
                    and then
                      Ekind (Cunit_Entity (Current_Sem_Unit)) = E_Package
                    and then
                      not In_Package_Body (Cunit_Entity (Current_Sem_Unit))
                    and then In_Open_Scopes (Scope (Exptyp))
                  then
                     Ref_Node := Make_Itype_Reference (Sloc (Ck_Node));
                     Set_Itype (Ref_Node, Exptyp);
                     Insert_Action (Ck_Node, Ref_Node);
                  end if;

                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                               or else
                             Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
                        Get_Index_Bounds (L_Index, L_Low, L_High);
                        Get_Index_Bounds (R_Index, R_Low, R_High);

                        --  Deal with compile time length check. Note that we
                        --  skip this in the access case, because the access
                        --  value may be null, so we cannot know statically.

                        if not Do_Access
                          and then Compile_Time_Known_Value (L_Low)
                          and then Compile_Time_Known_Value (L_High)
                          and then Compile_Time_Known_Value (R_Low)
                          and then Compile_Time_Known_Value (R_High)
                        then
                           if Expr_Value (L_High) >= Expr_Value (L_Low) then
                              L_Length := Expr_Value (L_High) -
                                          Expr_Value (L_Low) + 1;
                           else
                              L_Length := UI_From_Int (0);
                           end if;

                           if Expr_Value (R_High) >= Expr_Value (R_Low) then
                              R_Length := Expr_Value (R_High) -
                                          Expr_Value (R_Low) + 1;
                           else
                              R_Length := UI_From_Int (0);
                           end if;

                           if L_Length > R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too few elements for}?", T_Typ));

                           elsif  L_Length < R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too many elements for}?", T_Typ));
                           end if;

                        --  The comparison for an individual index subtype
                        --  is omitted if the corresponding index subtypes
                        --  statically match, since the result is known to
                        --  be true. Note that this test is worth while even
                        --  though we do static evaluation, because non-static
                        --  subtypes can statically match.

                        elsif not
                          Subtypes_Statically_Match
                            (Etype (L_Index), Etype (R_Index))

                          and then not
                            (Same_Bounds (L_Low, R_Low)
                              and then Same_Bounds (L_High, R_High))
                        then
                           Evolve_Or_Else
                             (Cond, Length_E_Cond (Exptyp, T_Typ, Indx));
                        end if;

                        Next (L_Index);
                        Next (R_Index);
                     end if;
                  end loop;
               end;

            --  Handle cases where we do not get a usable actual subtype that
            --  is constrained. This happens for example in the function call
            --  and explicit dereference cases. In these cases, we have to get
            --  the length or range from the expression itself, making sure we
            --  do not evaluate it more than once.

            --  Here Ck_Node is the original expression, or more properly the
            --  result of applying Duplicate_Expr to the original tree,
            --  forcing the result to be a name.

            else
               declare
                  Ndims : constant Nat := Number_Dimensions (T_Typ);

               begin
                  --  Build the condition for the explicit dereference case

                  for Indx in 1 .. Ndims loop
                     Evolve_Or_Else
                       (Cond, Length_N_Cond (Ck_Node, T_Typ, Indx));
                  end loop;
               end;
            end if;
         end if;
      end if;

      --  Construct the test and insert into the tree

      if Present (Cond) then
         if Do_Access then
            Cond := Guard_Access (Cond, Loc, Ck_Node);
         end if;

         Add_Check
           (Make_Raise_Constraint_Error (Loc,
              Condition => Cond,
              Reason => CE_Length_Check_Failed));
      end if;

      return Ret_Result;
   end Selected_Length_Checks;

   ---------------------------
   -- Selected_Range_Checks --
   ---------------------------

   function Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id) return Check_Result
   is
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      S_Typ       : Entity_Id;
      T_Typ       : Entity_Id;
      Expr_Actual : Node_Id;
      Exptyp      : Entity_Id;
      Cond        : Node_Id := Empty;
      Do_Access   : Boolean := False;
      Wnode       : Node_Id  := Warn_Node;
      Ret_Result  : Check_Result := (Empty, Empty);
      Num_Checks  : Integer := 0;

      procedure Add_Check (N : Node_Id);
      --  Adds the action given to Ret_Result if N is non-Empty

      function Discrete_Range_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id) return Node_Id;
      --  Returns expression to compute:
      --    Low_Bound (Expr) < Typ'First
      --      or else
      --    High_Bound (Expr) > Typ'Last

      function Discrete_Expr_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id) return Node_Id;
      --  Returns expression to compute:
      --    Expr < Typ'First
      --      or else
      --    Expr > Typ'Last

      function Get_E_First_Or_Last
        (E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id) return Node_Id;
      --  Returns expression to compute:
      --    E'First or E'Last

      function Get_N_First (N : Node_Id; Indx : Nat) return Node_Id;
      function Get_N_Last  (N : Node_Id; Indx : Nat) return Node_Id;
      --  Returns expression to compute:
      --    N'First or N'Last using Duplicate_Subexpr_No_Checks

      function Range_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id;
      --  Returns expression to compute:
      --    Exptyp'First < Typ'First or else Exptyp'Last > Typ'Last

      function Range_Equal_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat) return Node_Id;
      --  Returns expression to compute:
      --    Exptyp'First /= Typ'First or else Exptyp'Last /= Typ'Last

      function Range_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat) return Node_Id;
      --  Return expression to compute:
      --    Expr'First < Typ'First or else Expr'Last > Typ'Last

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check (N : Node_Id) is
      begin
         if Present (N) then

            --  For now, ignore attempt to place more than 2 checks ???

            if Num_Checks = 2 then
               return;
            end if;

            pragma Assert (Num_Checks <= 1);
            Num_Checks := Num_Checks + 1;
            Ret_Result (Num_Checks) := N;
         end if;
      end Add_Check;

      -------------------------
      -- Discrete_Expr_Cond --
      -------------------------

      function Discrete_Expr_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id) return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ),
                     Duplicate_Subexpr_No_Checks (Expr)),
                 Right_Opnd =>
                   Convert_To (Base_Type (Typ),
                               Get_E_First_Or_Last (Typ, 0, Name_First))),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ),
                     Duplicate_Subexpr_No_Checks (Expr)),
                 Right_Opnd =>
                   Convert_To
                     (Base_Type (Typ),
                      Get_E_First_Or_Last (Typ, 0, Name_Last))));
      end Discrete_Expr_Cond;

      -------------------------
      -- Discrete_Range_Cond --
      -------------------------

      function Discrete_Range_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id) return Node_Id
      is
         LB : Node_Id := Low_Bound (Expr);
         HB : Node_Id := High_Bound (Expr);

         Left_Opnd  : Node_Id;
         Right_Opnd : Node_Id;

      begin
         if Nkind (LB) = N_Identifier
           and then Ekind (Entity (LB)) = E_Discriminant then
            LB := New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
         end if;

         if Nkind (HB) = N_Identifier
           and then Ekind (Entity (HB)) = E_Discriminant then
            HB := New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
         end if;

         Left_Opnd :=
           Make_Op_Lt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr_No_Checks (LB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ), Get_E_First_Or_Last (Typ, 0, Name_First)));

         if Base_Type (Typ) = Typ then
            return Left_Opnd;

         elsif Compile_Time_Known_Value (High_Bound (Scalar_Range (Typ)))
            and then
               Compile_Time_Known_Value (High_Bound (Scalar_Range
                                                     (Base_Type (Typ))))
         then
            if Is_Floating_Point_Type (Typ) then
               if Expr_Value_R (High_Bound (Scalar_Range (Typ))) =
                  Expr_Value_R (High_Bound (Scalar_Range (Base_Type (Typ))))
               then
                  return Left_Opnd;
               end if;

            else
               if Expr_Value (High_Bound (Scalar_Range (Typ))) =
                  Expr_Value (High_Bound (Scalar_Range (Base_Type (Typ))))
               then
                  return Left_Opnd;
               end if;
            end if;
         end if;

         Right_Opnd :=
           Make_Op_Gt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr_No_Checks (HB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ),
                  Get_E_First_Or_Last (Typ, 0, Name_Last)));

         return Make_Or_Else (Loc, Left_Opnd, Right_Opnd);
      end Discrete_Range_Cond;

      -------------------------
      -- Get_E_First_Or_Last --
      -------------------------

      function Get_E_First_Or_Last
        (E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id) return Node_Id
      is
         N     : Node_Id;
         LB    : Node_Id;
         HB    : Node_Id;
         Bound : Node_Id;

      begin
         if Is_Array_Type (E) then
            N := First_Index (E);

            for J in 2 .. Indx loop
               Next_Index (N);
            end loop;

         else
            N := Scalar_Range (E);
         end if;

         if Nkind (N) = N_Subtype_Indication then
            LB := Low_Bound (Range_Expression (Constraint (N)));
            HB := High_Bound (Range_Expression (Constraint (N)));

         elsif Is_Entity_Name (N) then
            LB := Type_Low_Bound  (Etype (N));
            HB := Type_High_Bound (Etype (N));

         else
            LB := Low_Bound  (N);
            HB := High_Bound (N);
         end if;

         if Nam = Name_First then
            Bound := LB;
         else
            Bound := HB;
         end if;

         if Nkind (Bound) = N_Identifier
           and then Ekind (Entity (Bound)) = E_Discriminant
         then
            --  If this is a task discriminant, and we are the body, we must
            --  retrieve the corresponding body discriminal. This is another
            --  consequence of the early creation of discriminals, and the
            --  need to generate constraint checks before their declarations
            --  are made visible.

            if Is_Concurrent_Record_Type (Scope (Entity (Bound)))  then
               declare
                  Tsk : constant Entity_Id :=
                          Corresponding_Concurrent_Type
                           (Scope (Entity (Bound)));
                  Disc : Entity_Id;

               begin
                  if In_Open_Scopes (Tsk)
                    and then Has_Completion (Tsk)
                  then
                     --  Find discriminant of original task, and use its
                     --  current discriminal, which is the renaming within
                     --  the task body.

                     Disc :=  First_Discriminant (Tsk);
                     while Present (Disc) loop
                        if Chars (Disc) = Chars (Entity (Bound)) then
                           Set_Scope (Discriminal (Disc), Tsk);
                           return New_Occurrence_Of (Discriminal (Disc), Loc);
                        end if;

                        Next_Discriminant (Disc);
                     end loop;

                     --  That loop should always succeed in finding a matching
                     --  entry and returning. Fatal error if not.

                     raise Program_Error;

                  else
                     return
                       New_Occurrence_Of (Discriminal (Entity (Bound)), Loc);
                  end if;
               end;
            else
               return New_Occurrence_Of (Discriminal (Entity (Bound)), Loc);
            end if;

         elsif Nkind (Bound) = N_Identifier
           and then Ekind (Entity (Bound)) = E_In_Parameter
           and then not Inside_Init_Proc
         then
            return Get_Discriminal (E, Bound);

         elsif Nkind (Bound) = N_Integer_Literal then
            return Make_Integer_Literal (Loc, Intval (Bound));

         --  Case of a bound that has been rewritten to an
         --  N_Raise_Constraint_Error node because it is an out-of-range
         --  value. We may not call Duplicate_Subexpr on this node because
         --  an N_Raise_Constraint_Error is not side effect free, and we may
         --  not assume that we are in the proper context to remove side
         --  effects on it at the point of reference.

         elsif Nkind (Bound) = N_Raise_Constraint_Error then
            return New_Copy_Tree (Bound);

         else
            return Duplicate_Subexpr_No_Checks (Bound);
         end if;
      end Get_E_First_Or_Last;

      -----------------
      -- Get_N_First --
      -----------------

      function Get_N_First (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_First,
             Prefix =>
               Duplicate_Subexpr_No_Checks (N, Name_Req => True),
             Expressions => New_List (
               Make_Integer_Literal (Loc, Indx)));
      end Get_N_First;

      ----------------
      -- Get_N_Last --
      ----------------

      function Get_N_Last (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Last,
             Prefix =>
               Duplicate_Subexpr_No_Checks (N, Name_Req => True),
             Expressions => New_List (
              Make_Integer_Literal (Loc, Indx)));
      end Get_N_Last;

      ------------------
      -- Range_E_Cond --
      ------------------

      function Range_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat) return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_First),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_Last),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));

      end Range_E_Cond;

      ------------------------
      -- Range_Equal_E_Cond --
      ------------------------

      function Range_Equal_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat) return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_First),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),
             Right_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_Last),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));
      end Range_Equal_E_Cond;

      ------------------
      -- Range_N_Cond --
      ------------------

      function Range_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat) return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd => Get_N_First (Expr, Indx),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd => Get_N_Last (Expr, Indx),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));
      end Range_N_Cond;

   --  Start of processing for Selected_Range_Checks

   begin
      if not Expander_Active then
         return Ret_Result;
      end if;

      if Target_Typ = Any_Type
        or else Target_Typ = Any_Composite
        or else Raises_Constraint_Error (Ck_Node)
      then
         return Ret_Result;
      end if;

      if No (Wnode) then
         Wnode := Ck_Node;
      end if;

      T_Typ := Target_Typ;

      if No (Source_Typ) then
         S_Typ := Etype (Ck_Node);
      else
         S_Typ := Source_Typ;
      end if;

      if S_Typ = Any_Type or else S_Typ = Any_Composite then
         return Ret_Result;
      end if;

      --  The order of evaluating T_Typ before S_Typ seems to be critical
      --  because S_Typ can be derived from Etype (Ck_Node), if it's not passed
      --  in, and since Node can be an N_Range node, it might be invalid.
      --  Should there be an assert check somewhere for taking the Etype of
      --  an N_Range node ???

      if Is_Access_Type (T_Typ) and then Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);
         T_Typ := Designated_Type (T_Typ);
         Do_Access := True;

         --  A simple optimization

         if Nkind (Ck_Node) = N_Null then
            return Ret_Result;
         end if;
      end if;

      --  For an N_Range Node, check for a null range and then if not
      --  null generate a range check action.

      if Nkind (Ck_Node) = N_Range then

         --  There's no point in checking a range against itself

         if Ck_Node = Scalar_Range (T_Typ) then
            return Ret_Result;
         end if;

         declare
            T_LB       : constant Node_Id := Type_Low_Bound  (T_Typ);
            T_HB       : constant Node_Id := Type_High_Bound (T_Typ);
            LB         : constant Node_Id := Low_Bound (Ck_Node);
            HB         : constant Node_Id := High_Bound (Ck_Node);
            Null_Range : Boolean;

            Out_Of_Range_L : Boolean;
            Out_Of_Range_H : Boolean;

         begin
            --  Check for case where everything is static and we can
            --  do the check at compile time. This is skipped if we
            --  have an access type, since the access value may be null.

            --  ??? This code can be improved since you only need to know
            --  that the two respective bounds (LB & T_LB or HB & T_HB)
            --  are known at compile time to emit pertinent messages.

            if Compile_Time_Known_Value (LB)
              and then Compile_Time_Known_Value (HB)
              and then Compile_Time_Known_Value (T_LB)
              and then Compile_Time_Known_Value (T_HB)
              and then not Do_Access
            then
               --  Floating-point case

               if Is_Floating_Point_Type (S_Typ) then
                  Null_Range := Expr_Value_R (HB) < Expr_Value_R (LB);
                  Out_Of_Range_L :=
                    (Expr_Value_R (LB) < Expr_Value_R (T_LB))
                       or else
                    (Expr_Value_R (LB) > Expr_Value_R (T_HB));

                  Out_Of_Range_H :=
                    (Expr_Value_R (HB) > Expr_Value_R (T_HB))
                       or else
                    (Expr_Value_R (HB) < Expr_Value_R (T_LB));

               --  Fixed or discrete type case

               else
                  Null_Range := Expr_Value (HB) < Expr_Value (LB);
                  Out_Of_Range_L :=
                    (Expr_Value (LB) < Expr_Value (T_LB))
                    or else
                    (Expr_Value (LB) > Expr_Value (T_HB));

                  Out_Of_Range_H :=
                    (Expr_Value (HB) > Expr_Value (T_HB))
                    or else
                    (Expr_Value (HB) < Expr_Value (T_LB));
               end if;

               if not Null_Range then
                  if Out_Of_Range_L then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Low_Bound (Ck_Node),
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                            (Wnode,
                             "static range out of bounds of}?", T_Typ));
                     end if;
                  end if;

                  if Out_Of_Range_H then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (High_Bound (Ck_Node),
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static range out of bounds of}?", T_Typ));
                     end if;
                  end if;

               end if;

            else
               declare
                  LB : Node_Id := Low_Bound (Ck_Node);
                  HB : Node_Id := High_Bound (Ck_Node);

               begin

                  --  If either bound is a discriminant and we are within
                  --  the record declaration, it is a use of the discriminant
                  --  in a constraint of a component, and nothing can be
                  --  checked here. The check will be emitted within the
                  --  init proc. Before then, the discriminal has no real
                  --  meaning.

                  if Nkind (LB) = N_Identifier
                    and then Ekind (Entity (LB)) = E_Discriminant
                  then
                     if Current_Scope = Scope (Entity (LB)) then
                        return Ret_Result;
                     else
                        LB :=
                          New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
                     end if;
                  end if;

                  if Nkind (HB) = N_Identifier
                    and then Ekind (Entity (HB)) = E_Discriminant
                  then
                     if Current_Scope = Scope (Entity (HB)) then
                        return Ret_Result;
                     else
                        HB :=
                          New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
                     end if;
                  end if;

                  Cond := Discrete_Range_Cond (Ck_Node, T_Typ);
                  Set_Paren_Count (Cond, 1);

                  Cond :=
                    Make_And_Then (Loc,
                      Left_Opnd =>
                        Make_Op_Ge (Loc,
                          Left_Opnd  => Duplicate_Subexpr_No_Checks (HB),
                          Right_Opnd => Duplicate_Subexpr_No_Checks (LB)),
                      Right_Opnd => Cond);
               end;

            end if;
         end;

      elsif Is_Scalar_Type (S_Typ) then

         --  This somewhat duplicates what Apply_Scalar_Range_Check does,
         --  except the above simply sets a flag in the node and lets
         --  gigi generate the check base on the Etype of the expression.
         --  Sometimes, however we want to do a dynamic check against an
         --  arbitrary target type, so we do that here.

         if Ekind (Base_Type (S_Typ)) /= Ekind (Base_Type (T_Typ)) then
            Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);

         --  For literals, we can tell if the constraint error will be
         --  raised at compile time, so we never need a dynamic check, but
         --  if the exception will be raised, then post the usual warning,
         --  and replace the literal with a raise constraint error
         --  expression. As usual, skip this for access types

         elsif Compile_Time_Known_Value (Ck_Node)
           and then not Do_Access
         then
            declare
               LB : constant Node_Id := Type_Low_Bound (T_Typ);
               UB : constant Node_Id := Type_High_Bound (T_Typ);

               Out_Of_Range  : Boolean;
               Static_Bounds : constant Boolean :=
                                 Compile_Time_Known_Value (LB)
                                   and Compile_Time_Known_Value (UB);

            begin
               --  Following range tests should use Sem_Eval routine ???

               if Static_Bounds then
                  if Is_Floating_Point_Type (S_Typ) then
                     Out_Of_Range :=
                       (Expr_Value_R (Ck_Node) < Expr_Value_R (LB))
                         or else
                       (Expr_Value_R (Ck_Node) > Expr_Value_R (UB));

                  else -- fixed or discrete type
                     Out_Of_Range :=
                       Expr_Value (Ck_Node) < Expr_Value (LB)
                         or else
                       Expr_Value (Ck_Node) > Expr_Value (UB);
                  end if;

                  --  Bounds of the type are static and the literal is
                  --  out of range so make a warning message.

                  if Out_Of_Range then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Ck_Node,
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static value out of range of}?", T_Typ));
                     end if;
                  end if;

               else
                  Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);
               end if;
            end;

         --  Here for the case of a non-static expression, we need a runtime
         --  check unless the source type range is guaranteed to be in the
         --  range of the target type.

         else
            if not In_Subrange_Of (S_Typ, T_Typ) then
               Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);
            end if;
         end if;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Expr_Actual);

            if Is_Access_Type (Exptyp) then
               Exptyp := Designated_Type (Exptyp);
            end if;

            --  String_Literal case. This needs to be handled specially be-
            --  cause no index types are available for string literals. The
            --  condition is simply:

            --    T_Typ'Length = string-literal-length

            if Nkind (Expr_Actual) = N_String_Literal then
               null;

            --  General array case. Here we have a usable actual subtype for
            --  the expression, and the condition is built from the two types

            --     T_Typ'First     < Exptyp'First     or else
            --     T_Typ'Last      > Exptyp'Last      or else
            --     T_Typ'First(1)  < Exptyp'First(1)  or else
            --     T_Typ'Last(1)   > Exptyp'Last(1)   or else
            --     ...

            elsif Is_Constrained (Exptyp) then
               declare
                  Ndims : constant Nat := Number_Dimensions (T_Typ);

                  L_Index : Node_Id;
                  R_Index : Node_Id;
                  L_Low   : Node_Id;
                  L_High  : Node_Id;
                  R_Low   : Node_Id;
                  R_High  : Node_Id;

               begin
                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                               or else
                             Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
                        Get_Index_Bounds (L_Index, L_Low, L_High);
                        Get_Index_Bounds (R_Index, R_Low, R_High);

                        --  Deal with compile time length check. Note that we
                        --  skip this in the access case, because the access
                        --  value may be null, so we cannot know statically.

                        if not
                          Subtypes_Statically_Match
                            (Etype (L_Index), Etype (R_Index))
                        then
                           --  If the target type is constrained then we
                           --  have to check for exact equality of bounds
                           --  (required for qualified expressions).

                           if Is_Constrained (T_Typ) then
                              Evolve_Or_Else
                                (Cond,
                                 Range_Equal_E_Cond (Exptyp, T_Typ, Indx));

                           else
                              Evolve_Or_Else
                                (Cond, Range_E_Cond (Exptyp, T_Typ, Indx));
                           end if;
                        end if;

                        Next (L_Index);
                        Next (R_Index);

                     end if;
                  end loop;
               end;

            --  Handle cases where we do not get a usable actual subtype that
            --  is constrained. This happens for example in the function call
            --  and explicit dereference cases. In these cases, we have to get
            --  the length or range from the expression itself, making sure we
            --  do not evaluate it more than once.

            --  Here Ck_Node is the original expression, or more properly the
            --  result of applying Duplicate_Expr to the original tree,
            --  forcing the result to be a name.

            else
               declare
                  Ndims : constant Nat := Number_Dimensions (T_Typ);

               begin
                  --  Build the condition for the explicit dereference case

                  for Indx in 1 .. Ndims loop
                     Evolve_Or_Else
                       (Cond, Range_N_Cond (Ck_Node, T_Typ, Indx));
                  end loop;
               end;

            end if;

         else
            --  Generate an Action to check that the bounds of the
            --  source value are within the constraints imposed by the
            --  target type for a conversion to an unconstrained type.
            --  Rule is 4.6(38).

            if Nkind (Parent (Ck_Node)) = N_Type_Conversion then
               declare
                  Opnd_Index : Node_Id;
                  Targ_Index : Node_Id;

               begin
                  Opnd_Index
                    := First_Index (Get_Actual_Subtype (Ck_Node));
                  Targ_Index := First_Index (T_Typ);

                  while Opnd_Index /= Empty loop
                     if Nkind (Opnd_Index) = N_Range then
                        if Is_In_Range
                             (Low_Bound (Opnd_Index), Etype (Targ_Index))
                          and then
                            Is_In_Range
                             (High_Bound (Opnd_Index), Etype (Targ_Index))
                        then
                           null;

                           --  If null range, no check needed

                        elsif
                          Compile_Time_Known_Value (High_Bound (Opnd_Index))
                            and then
                          Compile_Time_Known_Value (Low_Bound (Opnd_Index))
                            and then
                              Expr_Value (High_Bound (Opnd_Index)) <
                                  Expr_Value (Low_Bound (Opnd_Index))
                        then
                           null;

                        elsif Is_Out_Of_Range
                                (Low_Bound (Opnd_Index), Etype (Targ_Index))
                          or else
                              Is_Out_Of_Range
                                (High_Bound (Opnd_Index), Etype (Targ_Index))
                        then
                           Add_Check
                             (Compile_Time_Constraint_Error
                               (Wnode, "value out of range of}?", T_Typ));

                        else
                           Evolve_Or_Else
                             (Cond,
                              Discrete_Range_Cond
                                (Opnd_Index, Etype (Targ_Index)));
                        end if;
                     end if;

                     Next_Index (Opnd_Index);
                     Next_Index (Targ_Index);
                  end loop;
               end;
            end if;
         end if;
      end if;

      --  Construct the test and insert into the tree

      if Present (Cond) then
         if Do_Access then
            Cond := Guard_Access (Cond, Loc, Ck_Node);
         end if;

         Add_Check
           (Make_Raise_Constraint_Error (Loc,
              Condition => Cond,
              Reason    => CE_Range_Check_Failed));
      end if;

      return Ret_Result;
   end Selected_Range_Checks;

   -------------------------------
   -- Storage_Checks_Suppressed --
   -------------------------------

   function Storage_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Storage_Check);
      else
         return Scope_Suppress (Storage_Check);
      end if;
   end Storage_Checks_Suppressed;

   ---------------------------
   -- Tag_Checks_Suppressed --
   ---------------------------

   function Tag_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) then
         if Kill_Tag_Checks (E) then
            return True;
         elsif Checks_May_Be_Suppressed (E) then
            return Is_Check_Suppressed (E, Tag_Check);
         end if;
      end if;

      return Scope_Suppress (Tag_Check);
   end Tag_Checks_Suppressed;

end Checks;
