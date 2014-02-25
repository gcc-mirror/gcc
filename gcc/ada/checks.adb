------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
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
with Casing;   use Casing;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch4;  use Exp_Ch4;
with Exp_Ch11; use Exp_Ch11;
with Exp_Pakd; use Exp_Pakd;
with Exp_Util; use Exp_Util;
with Elists;   use Elists;
with Expander; use Expander;
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
with Sem_Aux;  use Sem_Aux;
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
with Stringt;  use Stringt;
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

   --  The above is modified in gnatprove mode to ensure that proper check
   --  flags are always placed, even if expansion is off.

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
   --  been executed. This is done principally by the mechanism of calling
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
      --  it has the same target type (or more accurately one with a
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

   procedure Apply_Arithmetic_Overflow_Strict (N : Node_Id);
   --  Used to apply arithmetic overflow checks for all cases except operators
   --  on signed arithmetic types in MINIMIZED/ELIMINATED case (for which we
   --  call Apply_Arithmetic_Overflow_Minimized_Eliminated below). N can be a
   --  signed integer arithmetic operator (but not an if or case expression).
   --  It is also called for types other than signed integers.

   procedure Apply_Arithmetic_Overflow_Minimized_Eliminated (Op : Node_Id);
   --  Used to apply arithmetic overflow checks for the case where the overflow
   --  checking mode is MINIMIZED or ELIMINATED and we have a signed integer
   --  arithmetic op (which includes the case of if and case expressions). Note
   --  that Do_Overflow_Check may or may not be set for node Op. In these modes
   --  we have work to do even if overflow checking is suppressed.

   procedure Apply_Division_Check
     (N   : Node_Id;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean);
   --  N is an N_Op_Div, N_Op_Rem, or N_Op_Mod node. This routine applies
   --  division checks as required if the Do_Division_Check flag is set.
   --  Rlo and Rhi give the possible range of the right operand, these values
   --  can be referenced and trusted only if ROK is set True.

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

   type Check_Type is new Check_Id range Access_Check .. Division_Check;
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
   --  This function is called whenever the bound is an entity and the scope
   --  indicates a protected operation. If the bound is an in-parameter of
   --  a protected operation that is not a prival, the function returns the
   --  bound itself.
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

   function Is_Signed_Integer_Arithmetic_Op (N : Node_Id) return Boolean;
   --  Returns True if node N is for an arithmetic operation with signed
   --  integer operands. This includes unary and binary operators, and also
   --  if and case expression nodes where the dependent expressions are of
   --  a signed integer type. These are the kinds of nodes for which special
   --  handling applies in MINIMIZED or ELIMINATED overflow checking mode.

   function Range_Or_Validity_Checks_Suppressed
     (Expr : Node_Id) return Boolean;
   --  Returns True if either range or validity checks or both are suppressed
   --  for the type of the given expression, or, if the expression is the name
   --  of an entity, if these checks are suppressed for the entity.

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
         return Scope_Suppress.Suppress (Access_Check);
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
         return Scope_Suppress.Suppress (Accessibility_Check);
      end if;
   end Accessibility_Checks_Suppressed;

   -----------------------------
   -- Activate_Division_Check --
   -----------------------------

   procedure Activate_Division_Check (N : Node_Id) is
   begin
      Set_Do_Division_Check (N, True);
      Possible_Local_Raise (N, Standard_Constraint_Error);
   end Activate_Division_Check;

   -----------------------------
   -- Activate_Overflow_Check --
   -----------------------------

   procedure Activate_Overflow_Check (N : Node_Id) is
   begin
      if not Nkind_In (N, N_Op_Rem, N_Op_Mod, N_Op_Plus) then
         Set_Do_Overflow_Check (N, True);
         Possible_Local_Raise (N, Standard_Constraint_Error);
      end if;
   end Activate_Overflow_Check;

   --------------------------
   -- Activate_Range_Check --
   --------------------------

   procedure Activate_Range_Check (N : Node_Id) is
   begin
      Set_Do_Range_Check (N, True);
      Possible_Local_Raise (N, Standard_Constraint_Error);
   end Activate_Range_Check;

   ---------------------------------
   -- Alignment_Checks_Suppressed --
   ---------------------------------

   function Alignment_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Alignment_Check);
      else
         return Scope_Suppress.Suppress (Alignment_Check);
      end if;
   end Alignment_Checks_Suppressed;

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
         or else (not Range_Checks_Suppressed (Suppress_Typ));

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
      --  We do not need checks if we are not generating code (i.e. the
      --  expander is not active). This is not just an optimization, there
      --  are cases (e.g. with pragma Debug) where generating the checks
      --  can cause real trouble).

      if not Expander_Active then
         return;
      end if;

      --  No check if short circuiting makes check unnecessary

      if not Check_Needed (P, Access_Check) then
         return;
      end if;

      --  No check if accessing the Offset_To_Top component of a dispatch
      --  table. They are safe by construction.

      if Tagged_Type_Expansion
        and then Present (Etype (P))
        and then RTU_Loaded (Ada_Tags)
        and then RTE_Available (RE_Offset_To_Top_Ptr)
        and then Etype (P) = RTE (RE_Offset_To_Top_Ptr)
      then
         return;
      end if;

      --  Otherwise go ahead and install the check

      Install_Null_Excluding_Check (P);
   end Apply_Access_Check;

   -------------------------------
   -- Apply_Accessibility_Check --
   -------------------------------

   procedure Apply_Accessibility_Check
     (N           : Node_Id;
      Typ         : Entity_Id;
      Insert_Node : Node_Id)
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Param_Ent   : Entity_Id           := Param_Entity (N);
      Param_Level : Node_Id;
      Type_Level  : Node_Id;

   begin
      if Ada_Version >= Ada_2012
         and then not Present (Param_Ent)
         and then Is_Entity_Name (N)
         and then Ekind_In (Entity (N), E_Constant, E_Variable)
         and then Present (Effective_Extra_Accessibility (Entity (N)))
      then
         Param_Ent := Entity (N);
         while Present (Renamed_Object (Param_Ent)) loop

            --  Renamed_Object must return an Entity_Name here
            --  because of preceding "Present (E_E_A (...))" test.

            Param_Ent := Entity (Renamed_Object (Param_Ent));
         end loop;
      end if;

      if Inside_A_Generic then
         return;

      --  Only apply the run-time check if the access parameter has an
      --  associated extra access level parameter and when the level of the
      --  type is less deep than the level of the access parameter, and
      --  accessibility checks are not suppressed.

      elsif Present (Param_Ent)
         and then Present (Extra_Accessibility (Param_Ent))
         and then UI_Gt (Object_Access_Level (N),
                         Deepest_Type_Access_Level (Typ))
         and then not Accessibility_Checks_Suppressed (Param_Ent)
         and then not Accessibility_Checks_Suppressed (Typ)
      then
         Param_Level :=
           New_Occurrence_Of (Extra_Accessibility (Param_Ent), Loc);

         Type_Level :=
           Make_Integer_Literal (Loc, Deepest_Type_Access_Level (Typ));

         --  Raise Program_Error if the accessibility level of the access
         --  parameter is deeper than the level of the target access type.

         Insert_Action (Insert_Node,
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => Param_Level,
                 Right_Opnd => Type_Level),
             Reason => PE_Accessibility_Check_Failed));

         Analyze_And_Resolve (N);
      end if;
   end Apply_Accessibility_Check;

   --------------------------------
   -- Apply_Address_Clause_Check --
   --------------------------------

   procedure Apply_Address_Clause_Check (E : Entity_Id; N : Node_Id) is
      pragma Assert (Nkind (N) = N_Freeze_Entity);

      AC   : constant Node_Id    := Address_Clause (E);
      Loc  : constant Source_Ptr := Sloc (AC);
      Typ  : constant Entity_Id  := Etype (E);
      Aexp : constant Node_Id    := Expression (AC);

      Expr : Node_Id;
      --  Address expression (not necessarily the same as Aexp, for example
      --  when Aexp is a reference to a constant, in which case Expr gets
      --  reset to reference the value expression of the constant.

      procedure Compile_Time_Bad_Alignment;
      --  Post error warnings when alignment is known to be incompatible. Note
      --  that we do not go as far as inserting a raise of Program_Error since
      --  this is an erroneous case, and it may happen that we are lucky and an
      --  underaligned address turns out to be OK after all.

      --------------------------------
      -- Compile_Time_Bad_Alignment --
      --------------------------------

      procedure Compile_Time_Bad_Alignment is
      begin
         if Address_Clause_Overlay_Warnings then
            Error_Msg_FE
              ("?o?specified address for& may be inconsistent with alignment",
               Aexp, E);
            Error_Msg_FE
              ("\?o?program execution may be erroneous (RM 13.3(27))",
               Aexp, E);
            Set_Address_Warning_Posted (AC);
         end if;
      end Compile_Time_Bad_Alignment;

   --  Start of processing for Apply_Address_Clause_Check

   begin
      --  See if alignment check needed. Note that we never need a check if the
      --  maximum alignment is one, since the check will always succeed.

      --  Note: we do not check for checks suppressed here, since that check
      --  was done in Sem_Ch13 when the address clause was processed. We are
      --  only called if checks were not suppressed. The reason for this is
      --  that we have to delay the call to Apply_Alignment_Check till freeze
      --  time (so that all types etc are elaborated), but we have to check
      --  the status of check suppressing at the point of the address clause.

      if No (AC)
        or else not Check_Address_Alignment (AC)
        or else Maximum_Alignment = 1
      then
         return;
      end if;

      --  Obtain expression from address clause

      Expr := Expression (AC);

      --  The following loop digs for the real expression to use in the check

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

      --  See if we know that Expr has a bad alignment at compile time

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
               Compile_Time_Bad_Alignment;
            else
               return;
            end if;
         end;

      --  If the expression has the form X'Address, then we can find out if
      --  the object X has an alignment that is compatible with the object E.
      --  If it hasn't or we don't know, we defer issuing the warning until
      --  the end of the compilation to take into account back end annotations.

      elsif Nkind (Expr) = N_Attribute_Reference
        and then Attribute_Name (Expr) = Name_Address
        and then Has_Compatible_Alignment (E, Prefix (Expr)) = Known_Compatible
      then
         return;
      end if;

      --  Here we do not know if the value is acceptable. Strictly we don't
      --  have to do anything, since if the alignment is bad, we have an
      --  erroneous program. However we are allowed to check for erroneous
      --  conditions and we decide to do this by default if the check is not
      --  suppressed.

      --  However, don't do the check if elaboration code is unwanted

      if Restriction_Active (No_Elaboration_Code) then
         return;

      --  Generate a check to raise PE if alignment may be inappropriate

      else
         --  If the original expression is a non-static constant, use the
         --  name of the constant itself rather than duplicating its
         --  defining expression, which was extracted above.

         --  Note: Expr is empty if the address-clause is applied to in-mode
         --  actuals (allowed by 13.1(22)).

         if not Present (Expr)
           or else
             (Is_Entity_Name (Expression (AC))
               and then Ekind (Entity (Expression (AC))) = E_Constant
               and then Nkind (Parent (Entity (Expression (AC))))
                                 = N_Object_Declaration)
         then
            Expr := New_Copy_Tree (Expression (AC));
         else
            Remove_Side_Effects (Expr);
         end if;

         if No (Actions (N)) then
            Set_Actions (N, New_List);
         end if;

         Prepend_To (Actions (N),
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Ne (Loc,
                 Left_Opnd =>
                   Make_Op_Mod (Loc,
                     Left_Opnd =>
                       Unchecked_Convert_To
                         (RTE (RE_Integer_Address), Expr),
                     Right_Opnd =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (E, Loc),
                         Attribute_Name => Name_Alignment)),
                 Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),
             Reason => PE_Misaligned_Address_Value));
         Analyze (First (Actions (N)), Suppress => All_Checks);

         --  If the address clause generates an alignment check and we are
         --  in ZPF or some restricted run-time, add a warning to explain
         --  the propagation warning that is generated by the check.

         if Nkind (First (Actions (N))) = N_Raise_Program_Error
           and then not Warnings_Off (E)
           and then Restriction_Active (No_Exception_Propagation)
         then
            Error_Msg_N
              ("address value may be incompatible with alignment of object?",
               N);
         end if;

         return;
      end if;

   exception
      --  If we have some missing run time component in configurable run time
      --  mode then just skip the check (it is not required in any case).

      when RE_Not_Available =>
         return;
   end Apply_Address_Clause_Check;

   -------------------------------------
   -- Apply_Arithmetic_Overflow_Check --
   -------------------------------------

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id) is
   begin
      --  Use old routine in almost all cases (the only case we are treating
      --  specially is the case of a signed integer arithmetic op with the
      --  overflow checking mode set to MINIMIZED or ELIMINATED).

      if Overflow_Check_Mode = Strict
        or else not Is_Signed_Integer_Arithmetic_Op (N)
      then
         Apply_Arithmetic_Overflow_Strict (N);

      --  Otherwise use the new routine for the case of a signed integer
      --  arithmetic op, with Do_Overflow_Check set to True, and the checking
      --  mode is MINIMIZED or ELIMINATED.

      else
         Apply_Arithmetic_Overflow_Minimized_Eliminated (N);
      end if;
   end Apply_Arithmetic_Overflow_Check;

   --------------------------------------
   -- Apply_Arithmetic_Overflow_Strict --
   --------------------------------------

   --  This routine is called only if the type is an integer type, and a
   --  software arithmetic overflow check may be needed for op (add, subtract,
   --  or multiply). This check is performed only if Software_Overflow_Checking
   --  is enabled and Do_Overflow_Check is set. In this case we expand the
   --  operation into a more complex sequence of tests that ensures that
   --  overflow is properly caught.

   --  This is used in CHECKED modes. It is identical to the code for this
   --  cases before the big overflow earthquake, thus ensuring that in this
   --  modes we have compatible behavior (and reliability) to what was there
   --  before. It is also called for types other than signed integers, and if
   --  the Do_Overflow_Check flag is off.

   --  Note: we also call this routine if we decide in the MINIMIZED case
   --  to give up and just generate an overflow check without any fuss.

   procedure Apply_Arithmetic_Overflow_Strict (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Typ  : constant Entity_Id  := Etype (N);
      Rtyp : constant Entity_Id  := Root_Type (Typ);

   begin
      --  Nothing to do if Do_Overflow_Check not set or overflow checks
      --  suppressed.

      if not Do_Overflow_Check (N) then
         return;
      end if;

      --  An interesting special case. If the arithmetic operation appears as
      --  the operand of a type conversion:

      --    type1 (x op y)

      --  and all the following conditions apply:

      --    arithmetic operation is for a signed integer type
      --    target type type1 is a static integer subtype
      --    range of x and y are both included in the range of type1
      --    range of x op y is included in the range of type1
      --    size of type1 is at least twice the result size of op

      --  then we don't do an overflow check in any case, instead we transform
      --  the operation so that we end up with:

      --    type1 (type1 (x) op type1 (y))

      --  This avoids intermediate overflow before the conversion. It is
      --  explicitly permitted by RM 3.5.4(24):

      --    For the execution of a predefined operation of a signed integer
      --    type, the implementation need not raise Constraint_Error if the
      --    result is outside the base range of the type, so long as the
      --    correct result is produced.

      --  It's hard to imagine that any programmer counts on the exception
      --  being raised in this case, and in any case it's wrong coding to
      --  have this expectation, given the RM permission. Furthermore, other
      --  Ada compilers do allow such out of range results.

      --  Note that we do this transformation even if overflow checking is
      --  off, since this is precisely about giving the "right" result and
      --  avoiding the need for an overflow check.

      --  Note: this circuit is partially redundant with respect to the similar
      --  processing in Exp_Ch4.Expand_N_Type_Conversion, but the latter deals
      --  with cases that do not come through here. We still need the following
      --  processing even with the Exp_Ch4 code in place, since we want to be
      --  sure not to generate the arithmetic overflow check in these cases
      --  (Exp_Ch4 would have a hard time removing them once generated).

      if Is_Signed_Integer_Type (Typ)
        and then Nkind (Parent (N)) = N_Type_Conversion
      then
         Conversion_Optimization : declare
            Target_Type : constant Entity_Id :=
              Base_Type (Entity (Subtype_Mark (Parent (N))));

            Llo, Lhi : Uint;
            Rlo, Rhi : Uint;
            LOK, ROK : Boolean;

            Vlo : Uint;
            Vhi : Uint;
            VOK : Boolean;

            Tlo : Uint;
            Thi : Uint;

         begin
            if Is_Integer_Type (Target_Type)
              and then RM_Size (Root_Type (Target_Type)) >= 2 * RM_Size (Rtyp)
            then
               Tlo := Expr_Value (Type_Low_Bound  (Target_Type));
               Thi := Expr_Value (Type_High_Bound (Target_Type));

               Determine_Range
                 (Left_Opnd  (N), LOK, Llo, Lhi, Assume_Valid => True);
               Determine_Range
                 (Right_Opnd (N), ROK, Rlo, Rhi, Assume_Valid => True);

               if (LOK and ROK)
                 and then Tlo <= Llo and then Lhi <= Thi
                 and then Tlo <= Rlo and then Rhi <= Thi
               then
                  Determine_Range (N, VOK, Vlo, Vhi, Assume_Valid => True);

                  if VOK and then Tlo <= Vlo and then Vhi <= Thi then
                     Rewrite (Left_Opnd (N),
                       Make_Type_Conversion (Loc,
                         Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                         Expression   => Relocate_Node (Left_Opnd (N))));

                     Rewrite (Right_Opnd (N),
                       Make_Type_Conversion (Loc,
                        Subtype_Mark => New_Occurrence_Of (Target_Type, Loc),
                        Expression   => Relocate_Node (Right_Opnd (N))));

                     --  Rewrite the conversion operand so that the original
                     --  node is retained, in order to avoid the warning for
                     --  redundant conversions in Resolve_Type_Conversion.

                     Rewrite (N, Relocate_Node (N));

                     Set_Etype (N, Target_Type);

                     Analyze_And_Resolve (Left_Opnd  (N), Target_Type);
                     Analyze_And_Resolve (Right_Opnd (N), Target_Type);

                     --  Given that the target type is twice the size of the
                     --  source type, overflow is now impossible, so we can
                     --  safely kill the overflow check and return.

                     Set_Do_Overflow_Check (N, False);
                     return;
                  end if;
               end if;
            end if;
         end Conversion_Optimization;
      end if;

      --  Now see if an overflow check is required

      declare
         Siz   : constant Int := UI_To_Int (Esize (Rtyp));
         Dsiz  : constant Int := Siz * 2;
         Opnod : Node_Id;
         Ctyp  : Entity_Id;
         Opnd  : Node_Id;
         Cent  : RE_Id;

      begin
         --  Skip check if back end does overflow checks, or the overflow flag
         --  is not set anyway, or we are not doing code expansion, or the
         --  parent node is a type conversion whose operand is an arithmetic
         --  operation on signed integers on which the expander can promote
         --  later the operands to type Integer (see Expand_N_Type_Conversion).

         --  Special case CLI target, where arithmetic overflow checks can be
         --  performed for integer and long_integer

         if Backend_Overflow_Checks_On_Target
           or else not Do_Overflow_Check (N)
           or else not Expander_Active
           or else (Present (Parent (N))
                     and then Nkind (Parent (N)) = N_Type_Conversion
                     and then Integer_Promotion_Possible (Parent (N)))
           or else
             (VM_Target = CLI_Target and then Siz >= Standard_Integer_Size)
         then
            return;
         end if;

         --  Otherwise, generate the full general code for front end overflow
         --  detection, which works by doing arithmetic in a larger type:

         --    x op y

         --  is expanded into

         --    Typ (Checktyp (x) op Checktyp (y));

         --  where Typ is the type of the original expression, and Checktyp is
         --  an integer type of sufficient length to hold the largest possible
         --  result.

         --  If the size of check type exceeds the size of Long_Long_Integer,
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
                  Name => New_Occurrence_Of (RTE (Cent), Loc),
                  Parameter_Associations => New_List (
                    OK_Convert_To (RTE (RE_Integer_64), Left_Opnd  (N)),
                    OK_Convert_To (RTE (RE_Integer_64), Right_Opnd (N))))));

            Analyze_And_Resolve (N, Typ);
            return;
         end if;

         --  If we fall through, we have the case where we do the arithmetic
         --  in the next higher type and get the check by conversion. In these
         --  cases Ctyp is set to the type to be used as the check type.

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
         --  type, and we reset the overflow check indication, since clearly no
         --  overflow is possible now that we are using a double length type.
         --  We also set the Analyzed flag to avoid a recursive attempt to
         --  expand the node.

         Set_Etype             (Opnod, Base_Type (Ctyp));
         Set_Do_Overflow_Check (Opnod, False);
         Set_Analyzed          (Opnod, True);

         --  Now build the outer conversion

         Opnd := OK_Convert_To (Typ, Opnod);
         Analyze (Opnd);
         Set_Etype (Opnd, Typ);

         --  In the discrete type case, we directly generate the range check
         --  for the outer operand. This range check will implement the
         --  required overflow check.

         if Is_Discrete_Type (Typ) then
            Rewrite (N, Opnd);
            Generate_Range_Check
              (Expression (N), Typ, CE_Overflow_Check_Failed);

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
      end;
   end Apply_Arithmetic_Overflow_Strict;

   ----------------------------------------------------
   -- Apply_Arithmetic_Overflow_Minimized_Eliminated --
   ----------------------------------------------------

   procedure Apply_Arithmetic_Overflow_Minimized_Eliminated (Op : Node_Id) is
      pragma Assert (Is_Signed_Integer_Arithmetic_Op (Op));

      Loc : constant Source_Ptr := Sloc (Op);
      P   : constant Node_Id    := Parent (Op);

      LLIB : constant Entity_Id := Base_Type (Standard_Long_Long_Integer);
      --  Operands and results are of this type when we convert

      Result_Type : constant Entity_Id := Etype (Op);
      --  Original result type

      Check_Mode : constant Overflow_Mode_Type := Overflow_Check_Mode;
      pragma Assert (Check_Mode in Minimized_Or_Eliminated);

      Lo, Hi : Uint;
      --  Ranges of values for result

   begin
      --  Nothing to do if our parent is one of the following:

      --    Another signed integer arithmetic op
      --    A membership operation
      --    A comparison operation

      --  In all these cases, we will process at the higher level (and then
      --  this node will be processed during the downwards recursion that
      --  is part of the processing in Minimize_Eliminate_Overflows).

      if Is_Signed_Integer_Arithmetic_Op (P)
        or else Nkind (P) in N_Membership_Test
        or else Nkind (P) in N_Op_Compare

        --  This is also true for an alternative in a case expression

        or else Nkind (P) = N_Case_Expression_Alternative

        --  This is also true for a range operand in a membership test

        or else (Nkind (P) = N_Range
                  and then Nkind (Parent (P)) in N_Membership_Test)
      then
         return;
      end if;

      --  Otherwise, we have a top level arithmetic operation node, and this
      --  is where we commence the special processing for MINIMIZED/ELIMINATED
      --  modes. This is the case where we tell the machinery not to move into
      --  Bignum mode at this top level (of course the top level operation
      --  will still be in Bignum mode if either of its operands are of type
      --  Bignum).

      Minimize_Eliminate_Overflows (Op, Lo, Hi, Top_Level => True);

      --  That call may but does not necessarily change the result type of Op.
      --  It is the job of this routine to undo such changes, so that at the
      --  top level, we have the proper type. This "undoing" is a point at
      --  which a final overflow check may be applied.

      --  If the result type was not fiddled we are all set. We go to base
      --  types here because things may have been rewritten to generate the
      --  base type of the operand types.

      if Base_Type (Etype (Op)) = Base_Type (Result_Type) then
         return;

      --  Bignum case

      elsif Is_RTE (Etype (Op), RE_Bignum) then

         --  We need a sequence that looks like:

         --    Rnn : Result_Type;

         --    declare
         --       M : Mark_Id := SS_Mark;
         --    begin
         --       Rnn := Long_Long_Integer'Base (From_Bignum (Op));
         --       SS_Release (M);
         --    end;

         --  This block is inserted (using Insert_Actions), and then the node
         --  is replaced with a reference to Rnn.

         --  A special case arises if our parent is a conversion node. In this
         --  case no point in generating a conversion to Result_Type, we will
         --  let the parent handle this. Note that this special case is not
         --  just about optimization. Consider

         --      A,B,C : Integer;
         --      ...
         --      X := Long_Long_Integer'Base (A * (B ** C));

         --  Now the product may fit in Long_Long_Integer but not in Integer.
         --  In MINIMIZED/ELIMINATED mode, we don't want to introduce an
         --  overflow exception for this intermediate value.

         declare
            Blk : constant Node_Id  := Make_Bignum_Block (Loc);
            Rnn : constant Entity_Id := Make_Temporary (Loc, 'R', Op);
            RHS : Node_Id;

            Rtype : Entity_Id;

         begin
            RHS := Convert_From_Bignum (Op);

            if Nkind (P) /= N_Type_Conversion then
               Convert_To_And_Rewrite (Result_Type, RHS);
               Rtype := Result_Type;

               --  Interesting question, do we need a check on that conversion
               --  operation. Answer, not if we know the result is in range.
               --  At the moment we are not taking advantage of this. To be
               --  looked at later ???

            else
               Rtype := LLIB;
            end if;

            Insert_Before
              (First (Statements (Handled_Statement_Sequence (Blk))),
               Make_Assignment_Statement (Loc,
                 Name       => New_Occurrence_Of (Rnn, Loc),
                 Expression => RHS));

            Insert_Actions (Op, New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Rnn,
                Object_Definition   => New_Occurrence_Of (Rtype, Loc)),
              Blk));

            Rewrite (Op, New_Occurrence_Of (Rnn, Loc));
            Analyze_And_Resolve (Op);
         end;

      --  Here we know the result is Long_Long_Integer'Base, of that it has
      --  been rewritten because the parent operation is a conversion. See
      --  Apply_Arithmetic_Overflow_Strict.Conversion_Optimization.

      else
         pragma Assert
           (Etype (Op) = LLIB or else Nkind (Parent (Op)) = N_Type_Conversion);

         --  All we need to do here is to convert the result to the proper
         --  result type. As explained above for the Bignum case, we can
         --  omit this if our parent is a type conversion.

         if Nkind (P) /= N_Type_Conversion then
            Convert_To_And_Rewrite (Result_Type, Op);
         end if;

         Analyze_And_Resolve (Op);
      end if;
   end Apply_Arithmetic_Overflow_Minimized_Eliminated;

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
      --  No checks inside a generic (check the instantiations)

      if Inside_A_Generic then
         return;
      end if;

      --  Apply required constraint checks

      if Is_Scalar_Type (Typ) then
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

      elsif (Is_Record_Type (Typ) or else Is_Private_Type (Typ))
        and then Has_Discriminants (Base_Type (Typ))
        and then Is_Constrained (Typ)
      then
         Apply_Discriminant_Check (N, Typ);

      elsif Is_Access_Type (Typ) then

         Desig_Typ := Designated_Type (Typ);

         --  No checks necessary if expression statically null

         if Known_Null (N) then
            if Can_Never_Be_Null (Typ) then
               Install_Null_Excluding_Check (N);
            end if;

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

         --  Apply the 2005 Null_Excluding check. Note that we do not apply
         --  this check if the constraint node is illegal, as shown by having
         --  an error posted. This additional guard prevents cascaded errors
         --  and compiler aborts on illegal programs involving Ada 2005 checks.

         if Can_Never_Be_Null (Typ)
           and then not Can_Never_Be_Null (Etype (N))
           and then not Error_Posted (N)
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

      function Denotes_Explicit_Dereference (Obj : Node_Id) return Boolean;
      --  A heap object with an indefinite subtype is constrained by its
      --  initial value, and assigning to it requires a constraint_check.
      --  The target may be an explicit dereference, or a renaming of one.

      function Is_Aliased_Unconstrained_Component return Boolean;
      --  It is possible for an aliased component to have a nominal
      --  unconstrained subtype (through instantiation). If this is a
      --  discriminated component assigned in the expansion of an aggregate
      --  in an initialization, the check must be suppressed. This unusual
      --  situation requires a predicate of its own.

      ----------------------------------
      -- Denotes_Explicit_Dereference --
      ----------------------------------

      function Denotes_Explicit_Dereference (Obj : Node_Id) return Boolean is
      begin
         return
           Nkind (Obj) = N_Explicit_Dereference
             or else
               (Is_Entity_Name (Obj)
                 and then Present (Renamed_Object (Entity (Obj)))
                 and then Nkind (Renamed_Object (Entity (Obj))) =
                                              N_Explicit_Dereference);
      end Denotes_Explicit_Dereference;

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

      --  No discriminant checks necessary for an access when expression is
      --  statically Null. This is not only an optimization, it is fundamental
      --  because otherwise discriminant checks may be generated in init procs
      --  for types containing an access to a not-yet-frozen record, causing a
      --  deadly forward reference.

      --  Also, if the expression is of an access type whose designated type is
      --  incomplete, then the access value must be null and we suppress the
      --  check.

      if Known_Null (N) then
         return;

      elsif Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);

         if Ekind (S_Typ) = E_Incomplete_Type then
            return;
         end if;
      end if;

      --  If an assignment target is present, then we need to generate the
      --  actual subtype if the target is a parameter or aliased object with
      --  an unconstrained nominal subtype.

      --  Ada 2005 (AI-363): For Ada 2005, we limit the building of the actual
      --  subtype to the parameter and dereference cases, since other aliased
      --  objects are unconstrained (unless the nominal subtype is explicitly
      --  constrained).

      if Present (Lhs)
        and then (Present (Param_Entity (Lhs))
                   or else (Ada_Version < Ada_2005
                             and then not Is_Constrained (T_Typ)
                             and then Is_Aliased_View (Lhs)
                             and then not Is_Aliased_Unconstrained_Component)
                   or else (Ada_Version >= Ada_2005
                             and then not Is_Constrained (T_Typ)
                             and then Denotes_Explicit_Dereference (Lhs)
                             and then Nkind (Original_Node (Lhs)) /=
                                        N_Function_Call))
      then
         T_Typ := Get_Actual_Subtype (Lhs);
      end if;

      --  Nothing to do if the type is unconstrained (this is the case where
      --  the actual subtype in the RM sense of N is unconstrained and no check
      --  is required).

      if not Is_Constrained (T_Typ) then
         return;

      --  Ada 2005: nothing to do if the type is one for which there is a
      --  partial view that is constrained.

      elsif Ada_Version >= Ada_2005
        and then Object_Type_Has_Constrained_Partial_View
                   (Typ  => Base_Type (T_Typ),
                    Scop => Current_Scope)
      then
         return;
      end if;

      --  Nothing to do if the type is an Unchecked_Union

      if Is_Unchecked_Union (Base_Type (T_Typ)) then
         return;
      end if;

      --  Suppress checks if the subtypes are the same. The check must be
      --  preserved in an assignment to a formal, because the constraint is
      --  given by the actual.

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

      --  We can also eliminate checks on allocators with a subtype mark that
      --  coincides with the context type. The context type may be a subtype
      --  without a constraint (common case, a generic actual).

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

      --  See if we have a case where the types are both constrained, and all
      --  the constraints are constants. In this case, we can do the check
      --  successfully at compile time.

      --  We skip this check for the case where the node is rewritten as
      --  an allocator, because it already carries the context subtype,
      --  and extracting the discriminants from the aggregate is messy.

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
            --  private type completed by a default discriminated type. In that
            --  case, we need to get the constraints from the underlying type.
            --  If the underlying type is unconstrained (i.e. has no default
            --  discriminants) no check is needed.

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
                       Type_Definition (Original_Node (Parent (T_Typ)));
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

            --  Constraint may appear in full view of type

            if Ekind (T_Typ) = E_Private_Subtype
              and then Present (Full_View (T_Typ))
            then
               DconT :=
                 First_Elmt (Discriminant_Constraint (Full_View (T_Typ)));
            else
               DconT :=
                 First_Elmt (Discriminant_Constraint (T_Typ));
            end if;

            while Present (Discr) loop
               ItemS := Node (DconS);
               ItemT := Node (DconT);

               --  For a discriminated component type constrained by the
               --  current instance of an enclosing type, there is no
               --  applicable discriminant check.

               if Nkind (ItemT) = N_Attribute_Reference
                 and then Is_Access_Type (Etype (ItemT))
                 and then Is_Entity_Name (Prefix (ItemT))
                 and then Is_Type (Entity (Prefix (ItemT)))
               then
                  return;
               end if;

               --  If the expressions for the discriminants are identical
               --  and it is side-effect free (for now just an entity),
               --  this may be a shared constraint, e.g. from a subtype
               --  without a constraint introduced as a generic actual.
               --  Examine other discriminants if any.

               if ItemS = ItemT
                 and then Is_Entity_Name (ItemS)
               then
                  null;

               elsif not Is_OK_Static_Expression (ItemS)
                 or else not Is_OK_Static_Expression (ItemT)
               then
                  exit;

               elsif Expr_Value (ItemS) /= Expr_Value (ItemT) then
                  if Do_Access then   --  needs run-time check.
                     exit;
                  else
                     Apply_Compile_Time_Constraint_Error
                       (N, "incorrect value for discriminant&??",
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

      --  If Lhs is set and is a parameter, then the condition is guarded by:
      --  lhs'constrained and then (condition built above)

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

   -------------------------
   -- Apply_Divide_Checks --
   -------------------------

   procedure Apply_Divide_Checks (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Left  : constant Node_Id    := Left_Opnd (N);
      Right : constant Node_Id    := Right_Opnd (N);

      Mode : constant Overflow_Mode_Type := Overflow_Check_Mode;
      --  Current overflow checking mode

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;

      pragma Warnings (Off, Lhi);
      --  Don't actually use this value

   begin
      --  If we are operating in MINIMIZED or ELIMINATED mode, and we are
      --  operating on signed integer types, then the only thing this routine
      --  does is to call Apply_Arithmetic_Overflow_Minimized_Eliminated. That
      --  procedure will (possibly later on during recursive downward calls),
      --  ensure that any needed overflow/division checks are properly applied.

      if Mode in Minimized_Or_Eliminated
        and then Is_Signed_Integer_Type (Typ)
      then
         Apply_Arithmetic_Overflow_Minimized_Eliminated (N);
         return;
      end if;

      --  Proceed here in SUPPRESSED or CHECKED modes

      if Expander_Active
        and then not Backend_Divide_Checks_On_Target
        and then Check_Needed (Right, Division_Check)
      then
         Determine_Range (Right, ROK, Rlo, Rhi, Assume_Valid => True);

         --  Deal with division check

         if Do_Division_Check (N)
           and then not Division_Checks_Suppressed (Typ)
         then
            Apply_Division_Check (N, Rlo, Rhi, ROK);
         end if;

         --  Deal with overflow check

         if Do_Overflow_Check (N)
           and then not Overflow_Checks_Suppressed (Etype (N))
         then

            --  Test for extremely annoying case of xxx'First divided by -1
            --  for division of signed integer types (only overflow case).

            if Nkind (N) = N_Op_Divide
              and then Is_Signed_Integer_Type (Typ)
            then
               Determine_Range (Left, LOK, Llo, Lhi, Assume_Valid => True);
               LLB := Expr_Value (Type_Low_Bound (Base_Type (Typ)));

               if ((not ROK) or else (Rlo <= (-1) and then (-1) <= Rhi))
                     and then
                  ((not LOK) or else (Llo = LLB))
               then
                  Insert_Action (N,
                    Make_Raise_Constraint_Error (Loc,
                      Condition =>
                        Make_And_Then (Loc,
                          Left_Opnd  =>
                            Make_Op_Eq (Loc,
                              Left_Opnd  =>
                                Duplicate_Subexpr_Move_Checks (Left),
                              Right_Opnd => Make_Integer_Literal (Loc, LLB)),

                          Right_Opnd =>
                            Make_Op_Eq (Loc,
                              Left_Opnd  => Duplicate_Subexpr (Right),
                              Right_Opnd => Make_Integer_Literal (Loc, -1))),

                      Reason => CE_Overflow_Check_Failed));
               end if;
            end if;
         end if;
      end if;
   end Apply_Divide_Checks;

   --------------------------
   -- Apply_Division_Check --
   --------------------------

   procedure Apply_Division_Check
     (N   : Node_Id;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean)
   is
      pragma Assert (Do_Division_Check (N));

      Loc   : constant Source_Ptr := Sloc (N);
      Right : constant Node_Id    := Right_Opnd (N);

   begin
      if Expander_Active
        and then not Backend_Divide_Checks_On_Target
        and then Check_Needed (Right, Division_Check)
      then
         --  See if division by zero possible, and if so generate test. This
         --  part of the test is not controlled by the -gnato switch, since
         --  it is a Division_Check and not an Overflow_Check.

         if Do_Division_Check (N) then
            if (not ROK) or else (Rlo <= 0 and then 0 <= Rhi) then
               Insert_Action (N,
                 Make_Raise_Constraint_Error (Loc,
                   Condition =>
                     Make_Op_Eq (Loc,
                       Left_Opnd  => Duplicate_Subexpr_Move_Checks (Right),
                       Right_Opnd => Make_Integer_Literal (Loc, 0)),
                   Reason => CE_Divide_By_Zero));
            end if;
         end if;
      end if;
   end Apply_Division_Check;

   ----------------------------------
   -- Apply_Float_Conversion_Check --
   ----------------------------------

   --  Let F and I be the source and target types of the conversion. The RM
   --  specifies that a floating-point value X is rounded to the nearest
   --  integer, with halfway cases being rounded away from zero. The rounded
   --  value of X is checked against I'Range.

   --  The catch in the above paragraph is that there is no good way to know
   --  whether the round-to-integer operation resulted in overflow. A remedy is
   --  to perform a range check in the floating-point domain instead, however:

   --      (1)  The bounds may not be known at compile time
   --      (2)  The check must take into account rounding or truncation.
   --      (3)  The range of type I may not be exactly representable in F.
   --      (4)  For the rounding case, The end-points I'First - 0.5 and
   --           I'Last + 0.5 may or may not be in range, depending on the
   --           sign of  I'First and I'Last.
   --      (5)  X may be a NaN, which will fail any comparison

   --  The following steps correctly convert X with rounding:

   --      (1) If either I'First or I'Last is not known at compile time, use
   --          I'Base instead of I in the next three steps and perform a
   --          regular range check against I'Range after conversion.
   --      (2) If I'First - 0.5 is representable in F then let Lo be that
   --          value and define Lo_OK as (I'First > 0). Otherwise, let Lo be
   --          F'Machine (I'First) and let Lo_OK be (Lo >= I'First).
   --          In other words, take one of the closest floating-point numbers
   --          (which is an integer value) to I'First, and see if it is in
   --          range or not.
   --      (3) If I'Last + 0.5 is representable in F then let Hi be that value
   --          and define Hi_OK as (I'Last < 0). Otherwise, let Hi be
   --          F'Machine (I'Last) and let Hi_OK be (Hi <= I'Last).
   --      (4) Raise CE when (Lo_OK and X < Lo) or (not Lo_OK and X <= Lo)
   --                     or (Hi_OK and X > Hi) or (not Hi_OK and X >= Hi)

   --  For the truncating case, replace steps (2) and (3) as follows:
   --      (2) If I'First > 0, then let Lo be F'Pred (I'First) and let Lo_OK
   --          be False. Otherwise, let Lo be F'Succ (I'First - 1) and let
   --          Lo_OK be True.
   --      (3) If I'Last < 0, then let Hi be F'Succ (I'Last) and let Hi_OK
   --          be False. Otherwise let Hi be F'Pred (I'Last + 1) and let
   --          Hi_OK be True.

   procedure Apply_Float_Conversion_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id)
   is
      LB          : constant Node_Id    := Type_Low_Bound (Target_Typ);
      HB          : constant Node_Id    := Type_High_Bound (Target_Typ);
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      Expr_Type   : constant Entity_Id  := Base_Type (Etype (Ck_Node));
      Target_Base : constant Entity_Id  :=
        Implementation_Base_Type (Target_Typ);

      Par : constant Node_Id := Parent (Ck_Node);
      pragma Assert (Nkind (Par) = N_Type_Conversion);
      --  Parent of check node, must be a type conversion

      Truncate  : constant Boolean := Float_Truncate (Par);
      Max_Bound : constant Uint :=
        UI_Expon
          (Machine_Radix_Value (Expr_Type),
           Machine_Mantissa_Value (Expr_Type) - 1) - 1;

      --  Largest bound, so bound plus or minus half is a machine number of F

      Ifirst, Ilast : Uint;
      --  Bounds of integer type

      Lo, Hi : Ureal;
      --  Bounds to check in floating-point domain

      Lo_OK, Hi_OK : Boolean;
      --  True iff Lo resp. Hi belongs to I'Range

      Lo_Chk, Hi_Chk : Node_Id;
      --  Expressions that are False iff check fails

      Reason : RT_Exception_Code;

   begin
      --  We do not need checks if we are not generating code (i.e. the full
      --  expander is not active). In SPARK mode, we specifically don't want
      --  the frontend to expand these checks, which are dealt with directly
      --  in the formal verification backend.

      if not Expander_Active then
         return;
      end if;

      if not Compile_Time_Known_Value (LB)
          or not Compile_Time_Known_Value (HB)
      then
         declare
            --  First check that the value falls in the range of the base type,
            --  to prevent overflow during conversion and then perform a
            --  regular range check against the (dynamic) bounds.

            pragma Assert (Target_Base /= Target_Typ);

            Temp : constant Entity_Id := Make_Temporary (Loc, 'T', Par);

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

      --  Get the (static) bounds of the target type

      Ifirst := Expr_Value (LB);
      Ilast  := Expr_Value (HB);

      --  A simple optimization: if the expression is a universal literal,
      --  we can do the comparison with the bounds and the conversion to
      --  an integer type statically. The range checks are unchanged.

      if Nkind (Ck_Node) = N_Real_Literal
        and then Etype (Ck_Node) = Universal_Real
        and then Is_Integer_Type (Target_Typ)
        and then Nkind (Parent (Ck_Node)) = N_Type_Conversion
      then
         declare
            Int_Val : constant Uint := UR_To_Uint (Realval (Ck_Node));

         begin
            if Int_Val <= Ilast and then Int_Val >= Ifirst then

               --  Conversion is safe

               Rewrite (Parent (Ck_Node),
                 Make_Integer_Literal (Loc, UI_To_Int (Int_Val)));
               Analyze_And_Resolve (Parent (Ck_Node), Target_Typ);
               return;
            end if;
         end;
      end if;

      --  Check against lower bound

      if Truncate and then Ifirst > 0 then
         Lo := Pred (Expr_Type, UR_From_Uint (Ifirst));
         Lo_OK := False;

      elsif Truncate then
         Lo := Succ (Expr_Type, UR_From_Uint (Ifirst - 1));
         Lo_OK := True;

      elsif abs (Ifirst) < Max_Bound then
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

      if Truncate and then Ilast < 0 then
         Hi := Succ (Expr_Type, UR_From_Uint (Ilast));
         Hi_OK := False;

      elsif Truncate then
         Hi := Pred (Expr_Type, UR_From_Uint (Ilast + 1));
         Hi_OK := True;

      elsif abs (Ilast) < Max_Bound then
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

      --  If the bounds of the target type are the same as those of the base
      --  type, the check is an overflow check as a range check is not
      --  performed in these cases.

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

   -------------------------------------
   -- Apply_Parameter_Aliasing_Checks --
   -------------------------------------

   procedure Apply_Parameter_Aliasing_Checks
     (Call : Node_Id;
      Subp : Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Call);

      function May_Cause_Aliasing
        (Formal_1 : Entity_Id;
         Formal_2 : Entity_Id) return Boolean;
      --  Determine whether two formal parameters can alias each other
      --  depending on their modes.

      function Original_Actual (N : Node_Id) return Node_Id;
      --  The expander may replace an actual with a temporary for the sake of
      --  side effect removal. The temporary may hide a potential aliasing as
      --  it does not share the address of the actual. This routine attempts
      --  to retrieve the original actual.

      procedure Overlap_Check
        (Actual_1 : Node_Id;
         Actual_2 : Node_Id;
         Formal_1 : Entity_Id;
         Formal_2 : Entity_Id;
         Check    : in out Node_Id);
      --  Create a check to determine whether Actual_1 overlaps with Actual_2.
      --  If detailed exception messages are enabled, the check is augmented to
      --  provide information about the names of the corresponding formals. See
      --  the body for details. Actual_1 and Actual_2 denote the two actuals to
      --  be tested. Formal_1 and Formal_2 denote the corresponding formals.
      --  Check contains all and-ed simple tests generated so far or remains
      --  unchanged in the case of detailed exception messaged.

      ------------------------
      -- May_Cause_Aliasing --
      ------------------------

      function May_Cause_Aliasing
        (Formal_1 : Entity_Id;
         Formal_2 : Entity_Id) return Boolean
      is
      begin
         --  The following combination cannot lead to aliasing

         --     Formal 1    Formal 2
         --     IN          IN

         if Ekind (Formal_1) = E_In_Parameter
              and then
            Ekind (Formal_2) = E_In_Parameter
         then
            return False;

         --  The following combinations may lead to aliasing

         --     Formal 1    Formal 2
         --     IN          OUT
         --     IN          IN OUT
         --     OUT         IN
         --     OUT         IN OUT
         --     OUT         OUT

         else
            return True;
         end if;
      end May_Cause_Aliasing;

      ---------------------
      -- Original_Actual --
      ---------------------

      function Original_Actual (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Type_Conversion then
            return Expression (N);

         --  The expander created a temporary to capture the result of a type
         --  conversion where the expression is the real actual.

         elsif Nkind (N) = N_Identifier
           and then Present (Original_Node (N))
           and then Nkind (Original_Node (N)) = N_Type_Conversion
         then
            return Expression (Original_Node (N));
         end if;

         return N;
      end Original_Actual;

      -------------------
      -- Overlap_Check --
      -------------------

      procedure Overlap_Check
        (Actual_1 : Node_Id;
         Actual_2 : Node_Id;
         Formal_1 : Entity_Id;
         Formal_2 : Entity_Id;
         Check    : in out Node_Id)
      is
         Cond      : Node_Id;
         ID_Casing : constant Casing_Type :=
                       Identifier_Casing (Source_Index (Current_Sem_Unit));

      begin
         --  Generate:
         --    Actual_1'Overlaps_Storage (Actual_2)

         Cond :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Copy_Tree (Original_Actual (Actual_1)),
             Attribute_Name => Name_Overlaps_Storage,
             Expressions    =>
               New_List (New_Copy_Tree (Original_Actual (Actual_2))));

         --  Generate the following check when detailed exception messages are
         --  enabled:

         --    if Actual_1'Overlaps_Storage (Actual_2) then
         --       raise Program_Error with <detailed message>;
         --    end if;

         if Exception_Extra_Info then
            Start_String;

            --  Do not generate location information for internal calls

            if Comes_From_Source (Call) then
               Store_String_Chars (Build_Location_String (Loc));
               Store_String_Char (' ');
            end if;

            Store_String_Chars ("aliased parameters, actuals for """);

            Get_Name_String (Chars (Formal_1));
            Set_Casing (ID_Casing);
            Store_String_Chars (Name_Buffer (1 .. Name_Len));

            Store_String_Chars (""" and """);

            Get_Name_String (Chars (Formal_2));
            Set_Casing (ID_Casing);
            Store_String_Chars (Name_Buffer (1 .. Name_Len));

            Store_String_Chars (""" overlap");

            Insert_Action (Call,
              Make_If_Statement (Loc,
                Condition       => Cond,
                Then_Statements => New_List (
                  Make_Raise_Statement (Loc,
                    Name       =>
                      New_Occurrence_Of (Standard_Program_Error, Loc),
                    Expression => Make_String_Literal (Loc, End_String)))));

         --  Create a sequence of overlapping checks by and-ing them all
         --  together.

         else
            if No (Check) then
               Check := Cond;
            else
               Check :=
                 Make_And_Then (Loc,
                   Left_Opnd  => Check,
                   Right_Opnd => Cond);
            end if;
         end if;
      end Overlap_Check;

      --  Local variables

      Actual_1 : Node_Id;
      Actual_2 : Node_Id;
      Check    : Node_Id;
      Formal_1 : Entity_Id;
      Formal_2 : Entity_Id;

   --  Start of processing for Apply_Parameter_Aliasing_Checks

   begin
      Check := Empty;

      Actual_1 := First_Actual (Call);
      Formal_1 := First_Formal (Subp);
      while Present (Actual_1) and then Present (Formal_1) loop

         --  Ensure that the actual is an object that is not passed by value.
         --  Elementary types are always passed by value, therefore actuals of
         --  such types cannot lead to aliasing.

         if Is_Object_Reference (Original_Actual (Actual_1))
           and then not Is_Elementary_Type (Etype (Original_Actual (Actual_1)))
         then
            Actual_2 := Next_Actual (Actual_1);
            Formal_2 := Next_Formal (Formal_1);
            while Present (Actual_2) and then Present (Formal_2) loop

               --  The other actual we are testing against must also denote
               --  a non pass-by-value object. Generate the check only when
               --  the mode of the two formals may lead to aliasing.

               if Is_Object_Reference (Original_Actual (Actual_2))
                 and then not
                   Is_Elementary_Type (Etype (Original_Actual (Actual_2)))
                 and then May_Cause_Aliasing (Formal_1, Formal_2)
               then
                  Overlap_Check
                    (Actual_1 => Actual_1,
                     Actual_2 => Actual_2,
                     Formal_1 => Formal_1,
                     Formal_2 => Formal_2,
                     Check    => Check);
               end if;

               Next_Actual (Actual_2);
               Next_Formal (Formal_2);
            end loop;
         end if;

         Next_Actual (Actual_1);
         Next_Formal (Formal_1);
      end loop;

      --  Place a simple check right before the call

      if Present (Check) and then not Exception_Extra_Info then
         Insert_Action (Call,
           Make_Raise_Program_Error (Loc,
             Condition => Check,
             Reason    => PE_Aliased_Parameters));
      end if;
   end Apply_Parameter_Aliasing_Checks;

   -------------------------------------
   -- Apply_Parameter_Validity_Checks --
   -------------------------------------

   procedure Apply_Parameter_Validity_Checks (Subp : Entity_Id) is
      Subp_Decl : Node_Id;

      procedure Add_Validity_Check
        (Context    : Entity_Id;
         PPC_Nam    : Name_Id;
         For_Result : Boolean := False);
      --  Add a single 'Valid[_Scalar] check which verifies the initialization
      --  of Context. PPC_Nam denotes the pre or post condition pragma name.
      --  Set flag For_Result when to verify the result of a function.

      procedure Build_PPC_Pragma (PPC_Nam : Name_Id; Check : Node_Id);
      --  Create a pre or post condition pragma with name PPC_Nam which
      --  tests expression Check.

      ------------------------
      -- Add_Validity_Check --
      ------------------------

      procedure Add_Validity_Check
        (Context    : Entity_Id;
         PPC_Nam    : Name_Id;
         For_Result : Boolean := False)
      is
         Loc   : constant Source_Ptr := Sloc (Subp);
         Typ   : constant Entity_Id  := Etype (Context);
         Check : Node_Id;
         Nam   : Name_Id;

      begin
         --  Pick the proper version of 'Valid depending on the type of the
         --  context. If the context is not eligible for such a check, return.

         if Is_Scalar_Type (Typ) then
            Nam := Name_Valid;
         elsif not No_Scalar_Parts (Typ) then
            Nam := Name_Valid_Scalars;
         else
            return;
         end if;

         --  Step 1: Create the expression to verify the validity of the
         --  context.

         Check := New_Occurrence_Of (Context, Loc);

         --  When processing a function result, use 'Result. Generate
         --    Context'Result

         if For_Result then
            Check :=
              Make_Attribute_Reference (Loc,
                Prefix         => Check,
                Attribute_Name => Name_Result);
         end if;

         --  Generate:
         --    Context['Result]'Valid[_Scalars]

         Check :=
           Make_Attribute_Reference (Loc,
             Prefix         => Check,
             Attribute_Name => Nam);

         --  Step 2: Create a pre or post condition pragma

         Build_PPC_Pragma (PPC_Nam, Check);
      end Add_Validity_Check;

      ----------------------
      -- Build_PPC_Pragma --
      ----------------------

      procedure Build_PPC_Pragma (PPC_Nam : Name_Id; Check : Node_Id) is
         Loc   : constant Source_Ptr := Sloc (Subp);
         Decls : List_Id;
         Prag  : Node_Id;

      begin
         Prag :=
           Make_Pragma (Loc,
             Pragma_Identifier            => Make_Identifier (Loc, PPC_Nam),
             Pragma_Argument_Associations => New_List (
               Make_Pragma_Argument_Association (Loc,
                 Chars      => Name_Check,
                 Expression => Check)));

         --  Add a message unless exception messages are suppressed

         if not Exception_Locations_Suppressed then
            Append_To (Pragma_Argument_Associations (Prag),
              Make_Pragma_Argument_Association (Loc,
                Chars      => Name_Message,
                Expression =>
                  Make_String_Literal (Loc,
                    Strval => "failed " & Get_Name_String (PPC_Nam) &
                               " from " & Build_Location_String (Loc))));
         end if;

         --  Insert the pragma in the tree

         if Nkind (Parent (Subp_Decl)) = N_Compilation_Unit then
            Add_Global_Declaration (Prag);
            Analyze (Prag);

         --  PPC pragmas associated with subprogram bodies must be inserted in
         --  the declarative part of the body.

         elsif Nkind (Subp_Decl) = N_Subprogram_Body then
            Decls := Declarations (Subp_Decl);

            if No (Decls) then
               Decls := New_List;
               Set_Declarations (Subp_Decl, Decls);
            end if;

            Prepend_To (Decls, Prag);

            --  Ensure the proper visibility of the subprogram body and its
            --  parameters.

            Push_Scope (Subp);
            Analyze (Prag);
            Pop_Scope;

         --  For subprogram declarations insert the PPC pragma right after the
         --  declarative node.

         else
            Insert_After_And_Analyze (Subp_Decl, Prag);
         end if;
      end Build_PPC_Pragma;

      --  Local variables

      Formal    : Entity_Id;
      Subp_Spec : Node_Id;

   --  Start of processing for Apply_Parameter_Validity_Checks

   begin
      --  Extract the subprogram specification and declaration nodes

      Subp_Spec := Parent (Subp);

      if Nkind (Subp_Spec) = N_Defining_Program_Unit_Name then
         Subp_Spec := Parent (Subp_Spec);
      end if;

      Subp_Decl := Parent (Subp_Spec);

      if not Comes_From_Source (Subp)

         --  Do not process formal subprograms because the corresponding actual
         --  will receive the proper checks when the instance is analyzed.

        or else Is_Formal_Subprogram (Subp)

        --  Do not process imported subprograms since pre and post conditions
        --  are never verified on routines coming from a different language.

        or else Is_Imported (Subp)
        or else Is_Intrinsic_Subprogram (Subp)

        --  The PPC pragmas generated by this routine do not correspond to
        --  source aspects, therefore they cannot be applied to abstract
        --  subprograms.

        or else Nkind (Subp_Decl) = N_Abstract_Subprogram_Declaration

        --  Do not consider subprogram renaminds because the renamed entity
        --  already has the proper PPC pragmas.

        or else Nkind (Subp_Decl) = N_Subprogram_Renaming_Declaration

        --  Do not process null procedures because there is no benefit of
        --  adding the checks to a no action routine.

        or else (Nkind (Subp_Spec) = N_Procedure_Specification
                  and then Null_Present (Subp_Spec))
      then
         return;
      end if;

      --  Inspect all the formals applying aliasing and scalar initialization
      --  checks where applicable.

      Formal := First_Formal (Subp);
      while Present (Formal) loop

         --  Generate the following scalar initialization checks for each
         --  formal parameter:

         --    mode IN     - Pre       => Formal'Valid[_Scalars]
         --    mode IN OUT - Pre, Post => Formal'Valid[_Scalars]
         --    mode    OUT -      Post => Formal'Valid[_Scalars]

         if Check_Validity_Of_Parameters then
            if Ekind_In (Formal, E_In_Parameter, E_In_Out_Parameter) then
               Add_Validity_Check (Formal, Name_Precondition, False);
            end if;

            if Ekind_In (Formal, E_In_Out_Parameter, E_Out_Parameter) then
               Add_Validity_Check (Formal, Name_Postcondition, False);
            end if;
         end if;

         Next_Formal (Formal);
      end loop;

      --  Generate following scalar initialization check for function result:

      --    Post => Subp'Result'Valid[_Scalars]

      if Check_Validity_Of_Parameters and then Ekind (Subp) = E_Function then
         Add_Validity_Check (Subp, Name_Postcondition, True);
      end if;
   end Apply_Parameter_Validity_Checks;

   ---------------------------
   -- Apply_Predicate_Check --
   ---------------------------

   procedure Apply_Predicate_Check (N : Node_Id; Typ : Entity_Id) is
      S : Entity_Id;

   begin
      if Present (Predicate_Function (Typ)) then

         S := Current_Scope;
         while Present (S) and then not Is_Subprogram (S) loop
            S := Scope (S);
         end loop;

         --  A predicate check does not apply within internally generated
         --  subprograms, such as TSS functions.

         if Within_Internal_Subprogram then
            return;

         --  If the check appears within the predicate function itself, it
         --  means that the user specified a check whose formal is the
         --  predicated subtype itself, rather than some covering type. This
         --  is likely to be a common error, and thus deserves a warning.

         elsif Present (S) and then S = Predicate_Function (Typ) then
            Error_Msg_N
              ("predicate check includes a function call that "
               & "requires a predicate check??", Parent (N));
            Error_Msg_N
              ("\this will result in infinite recursion??", Parent (N));
            Insert_Action (N,
              Make_Raise_Storage_Error (Sloc (N),
                Reason => SE_Infinite_Recursion));

         --  Here for normal case of predicate active

         else
            --  If the type has a static predicate and the expression is known
            --  at compile time, see if the expression satisfies the predicate.

            Check_Expression_Against_Static_Predicate (N, Typ);

            Insert_Action (N,
              Make_Predicate_Check (Typ, Duplicate_Subexpr (N)));
         end if;
      end if;
   end Apply_Predicate_Check;

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

   --  Note that Apply_Scalar_Range_Check never turns the Do_Range_Check flag
   --  off if it is already set on.

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
      --  Set to True if Expr should be regarded as a real value even though
      --  the type of Expr might be discrete.

      procedure Bad_Value;
      --  Procedure called if value is determined to be out of range

      ---------------
      -- Bad_Value --
      ---------------

      procedure Bad_Value is
      begin
         Apply_Compile_Time_Constraint_Error
           (Expr, "value not in range of}??", CE_Range_Check_Failed,
            Ent => Target_Typ,
            Typ => Target_Typ);
      end Bad_Value;

   --  Start of processing for Apply_Scalar_Range_Check

   begin
      --  Return if check obviously not needed

      if
         --  Not needed inside generic

         Inside_A_Generic

         --  Not needed if previous error

         or else Target_Typ = Any_Type
         or else Nkind (Expr) = N_Error

         --  Not needed for non-scalar type

         or else not Is_Scalar_Type (Target_Typ)

         --  Not needed if we know node raises CE already

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

         if Is_Access_Type (Arr_Typ) then
            Arr_Typ := Designated_Type (Arr_Typ);
         end if;
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

            --  If Expr is part of an assignment statement, then check left
            --  side of assignment if it is an entity name.

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
      --  since the whole idea of such values is to avoid checking them.

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

      --  Special checks for floating-point type

      if Is_Floating_Point_Type (S_Typ) then

         --  Always do a range check if the source type includes infinities and
         --  the target type does not include infinities. We do not do this if
         --  range checks are killed.

         if Has_Infinities (S_Typ)
           and then not Has_Infinities (Target_Typ)
         then
            Enable_Range_Check (Expr);

         --  Always do a range check for operators if option set

         elsif Check_Float_Overflow and then Nkind (Expr) in N_Op then
            Enable_Range_Check (Expr);
         end if;
      end if;

      --  Return if we know expression is definitely in the range of the target
      --  type as determined by Determine_Range. Right now we only do this for
      --  discrete types, and not fixed-point or floating-point types.

      --  The additional less-precise tests below catch these cases

      --  Note: skip this if we are given a source_typ, since the point of
      --  supplying a Source_Typ is to stop us looking at the expression.
      --  We could sharpen this test to be out parameters only ???

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

                  Determine_Range (Expr, OK, Lo, Hi, Assume_Valid => True);

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
        and then Is_Discrete_Type (S_Typ) = Is_Discrete_Type (Target_Typ)
        and then
          (In_Subrange_Of (S_Typ, Target_Typ, Fixed_Int)
             or else
               Is_In_Range (Expr, Target_Typ,
                            Assume_Valid => True,
                            Fixed_Int    => Fixed_Int,
                            Int_Real     => Int_Real))
      then
         return;

      elsif Is_Out_Of_Range (Expr, Target_Typ,
                             Assume_Valid => True,
                             Fixed_Int    => Fixed_Int,
                             Int_Real     => Int_Real)
      then
         Bad_Value;
         return;

      --  Floating-point case
      --  In the floating-point case, we only do range checks if the type is
      --  constrained. We definitely do NOT want range checks for unconstrained
      --  types, since we want to have infinities

      elsif Is_Floating_Point_Type (S_Typ) then

      --  Normally, we only do range checks if the type is constrained. We do
      --  NOT want range checks for unconstrained types, since we want to have
      --  infinities. Override this decision in Check_Float_Overflow mode.

         if Is_Constrained (S_Typ) or else Check_Float_Overflow then
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
          or else (not Length_Checks_Suppressed (Target_Typ));

   begin
      --  Note: this means that we lose some useful warnings if the expander
      --  is not active, and we also lose these warnings in SPARK mode ???

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

         --  If the item is a conditional raise of constraint error, then have
         --  a look at what check is being performed and ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            --  Case where node does not now have a dynamic check

            if not Has_Dynamic_Length_Check (Ck_Node) then

               --  If checks are on, just insert the check

               if Checks_On then
                  Insert_Action (Ck_Node, R_Cno);

                  if not Do_Static then
                     Set_Has_Dynamic_Length_Check (Ck_Node);
                  end if;

               --  If checks are off, then analyze the length check after
               --  temporarily attaching it to the tree in case the relevant
               --  condition can be evaluated at compile time. We still want a
               --  compile time warning in this case.

               else
                  Set_Parent (R_Cno, Ck_Node);
                  Analyze (R_Cno);
               end if;
            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               Apply_Compile_Time_Constraint_Error
                 (Ck_Node, "wrong length for array of}??",
                  CE_Length_Check_Failed,
                  Ent => Target_Typ,
                  Typ => Target_Typ);

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Remove_Warning_Messages (R_Cno);
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
      Loc       : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On : constant Boolean :=
                    not Index_Checks_Suppressed (Target_Typ)
                      or else
                    not Range_Checks_Suppressed (Target_Typ);

      Cond     : Node_Id;
      R_Cno    : Node_Id;
      R_Result : Check_Result;

   begin
      if not Expander_Active or not Checks_On then
         return;
      end if;

      R_Result :=
        Selected_Range_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop
         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  The range check requires runtime evaluation. Depending on what its
         --  triggering condition is, the check may be converted into a compile
         --  time constraint check.

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            --  Insert the range check before the related context. Note that
            --  this action analyses the triggering condition.

            Insert_Action (Ck_Node, R_Cno);

            --  This old code doesn't make sense, why is the context flagged as
            --  requiring dynamic range checks now in the middle of generating
            --  them ???

            if not Do_Static then
               Set_Has_Dynamic_Range_Check (Ck_Node);
            end if;

            --  The triggering condition evaluates to True, the range check
            --  can be converted into a compile time constraint check.

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               --  Since an N_Range is technically not an expression, we have
               --  to set one of the bounds to C_E and then just flag the
               --  N_Range. The warning message will point to the lower bound
               --  and complain about a range, which seems OK.

               if Nkind (Ck_Node) = N_Range then
                  Apply_Compile_Time_Constraint_Error
                    (Low_Bound (Ck_Node),
                     "static range out of bounds of}??",
                     CE_Range_Check_Failed,
                     Ent => Target_Typ,
                     Typ => Target_Typ);

                  Set_Raises_Constraint_Error (Ck_Node);

               else
                  Apply_Compile_Time_Constraint_Error
                    (Ck_Node,
                     "static value out of range of}?",
                     CE_Range_Check_Failed,
                     Ent => Target_Typ,
                     Typ => Target_Typ);
               end if;

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            --  Why are we even generating checks if checks are turned off ???

            elsif Do_Static or else not Checks_On then
               Remove_Warning_Messages (R_Cno);
               Rewrite (R_Cno, Make_Null_Statement (Loc));
            end if;

         --  The range check raises Constrant_Error explicitly

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

         --  Check one subscript. Note that we do not worry about enumeration
         --  type with holes, since we will convert the value to a Pos value
         --  for the subscript, and that convert will do the necessary validity
         --  check.

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

      Expr_Type : constant Entity_Id := Underlying_Type (Etype (Expr));
      --  Note: if Etype (Expr) is a private type without discriminants, its
      --  full view might have discriminants with defaults, so we need the
      --  full view here to retrieve the constraints.

   begin
      if Inside_A_Generic then
         return;

      --  Skip these checks if serious errors detected, there are some nasty
      --  situations of incomplete trees that blow things up.

      elsif Serious_Errors_Detected > 0 then
         return;

      --  Never generate discriminant checks for Unchecked_Union types

      elsif Present (Expr_Type)
        and then Is_Unchecked_Union (Expr_Type)
      then
         return;

      --  Scalar type conversions of the form Target_Type (Expr) require a
      --  range check if we cannot be sure that Expr is in the base type of
      --  Target_Typ and also that Expr is in the range of Target_Typ. These
      --  are not quite the same condition from an implementation point of
      --  view, but clearly the second includes the first.

      elsif Is_Scalar_Type (Target_Type) then
         declare
            Conv_OK  : constant Boolean := Conversion_OK (N);
            --  If the Conversion_OK flag on the type conversion is set and no
            --  floating-point type is involved in the type conversion then
            --  fixed-point values must be read as integral values.

            Float_To_Int : constant Boolean :=
              Is_Floating_Point_Type (Expr_Type)
              and then Is_Integer_Type (Target_Type);

         begin
            if not Overflow_Checks_Suppressed (Target_Base)
              and then not Overflow_Checks_Suppressed (Target_Type)
              and then not
                In_Subrange_Of (Expr_Type, Target_Base, Fixed_Int => Conv_OK)
              and then not Float_To_Int
            then
               Activate_Overflow_Check (N);
            end if;

            if not Range_Checks_Suppressed (Target_Type)
              and then not Range_Checks_Suppressed (Expr_Type)
            then
               if Float_To_Int then
                  Apply_Float_Conversion_Check (Expr, Target_Type);
               else
                  Apply_Scalar_Range_Check
                    (Expr, Target_Type, Fixed_Int => Conv_OK);

                  --  If the target type has predicates, we need to indicate
                  --  the need for a check, even if Determine_Range finds that
                  --  the value is within bounds. This may be the case e.g for
                  --  a division with a constant denominator.

                  if Has_Predicates (Target_Type) then
                     Enable_Range_Check (Expr);
                  end if;
               end if;
            end if;
         end;

      elsif Comes_From_Source (N)
        and then not Discriminant_Checks_Suppressed (Target_Type)
        and then Is_Record_Type (Target_Type)
        and then Is_Derived_Type (Target_Type)
        and then not Is_Tagged_Type (Target_Type)
        and then not Is_Constrained (Target_Type)
        and then Present (Stored_Constraint (Target_Type))
      then
         --  An unconstrained derived type may have inherited discriminant.
         --  Build an actual discriminant constraint list using the stored
         --  constraint, to verify that the expression of the parent type
         --  satisfies the constraints imposed by the (unconstrained) derived
         --  type. This applies to value conversions, not to view conversions
         --  of tagged types.

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
                     --  Value of original discriminant in expression. If the
                     --  new discriminant has been used to constrain more than
                     --  one of the stored discriminants, this will provide the
                     --  required consistency check.

                     Append_Elmt
                       (Make_Selected_Component (Loc,
                          Prefix        =>
                            Duplicate_Subexpr_No_Checks
                              (Expr, Name_Req => True),
                          Selector_Name =>
                            Make_Identifier (Loc, Chars (Discr))),
                        New_Constraints);

                  else
                     --  Discriminant of more remote ancestor ???

                     return;
                  end if;

               --  Derived type definition has an explicit value for this
               --  stored discriminant.

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

      --  For arrays, checks are set now, but conversions are applied during
      --  expansion, to take into accounts changes of representation. The
      --  checks become range checks on the base type or length checks on the
      --  subtype, depending on whether the target type is unconstrained or
      --  constrained. Note that the range check is put on the expression of a
      --  type conversion, while the length check is put on the type conversion
      --  itself.

      elsif Is_Array_Type (Target_Type) then
         if Is_Constrained (Target_Type) then
            Set_Do_Length_Check (N);
         else
            Set_Do_Range_Check (Expr);
         end if;
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

      --  Otherwise, replace the attribute node with a type conversion node
      --  whose expression is the attribute, retyped to universal integer, and
      --  whose subtype mark is the target type. The call to analyze this
      --  conversion will set range and overflow checks as required for proper
      --  detection of an out of range value.

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

   -------------------------------------
   -- Atomic_Synchronization_Disabled --
   -------------------------------------

   --  Note: internally Disable/Enable_Atomic_Synchronization is implemented
   --  using a bogus check called Atomic_Synchronization. This is to make it
   --  more convenient to get exactly the same semantics as [Un]Suppress.

   function Atomic_Synchronization_Disabled (E : Entity_Id) return Boolean is
   begin
      --  If debug flag d.e is set, always return False, i.e. all atomic sync
      --  looks enabled, since it is never disabled.

      if Debug_Flag_Dot_E then
         return False;

      --  If debug flag d.d is set then always return True, i.e. all atomic
      --  sync looks disabled, since it always tests True.

      elsif Debug_Flag_Dot_D then
         return True;

      --  If entity present, then check result for that entity

      elsif Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Atomic_Synchronization);

      --  Otherwise result depends on current scope setting

      else
         return Scope_Suppress.Suppress (Atomic_Synchronization);
      end if;
   end Atomic_Synchronization_Disabled;

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

      function Aggregate_Discriminant_Val (Disc : Entity_Id) return Node_Id;

      ----------------------------------
      -- Aggregate_Discriminant_Value --
      ----------------------------------

      function Aggregate_Discriminant_Val (Disc : Entity_Id) return Node_Id is
         Assoc : Node_Id;

      begin
         --  The aggregate has been normalized with named associations. We use
         --  the Chars field to locate the discriminant to take into account
         --  discriminants in derived types, which carry the same name as those
         --  in the parent.

         Assoc := First (Component_Associations (N));
         while Present (Assoc) loop
            if Chars (First (Choices (Assoc))) = Chars (Disc) then
               return Expression (Assoc);
            else
               Next (Assoc);
            end if;
         end loop;

         --  Discriminant must have been found in the loop above

         raise Program_Error;
      end Aggregate_Discriminant_Val;

   --  Start of processing for Build_Discriminant_Checks

   begin
      --  Loop through discriminants evolving the condition

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

         elsif Nkind (N) = N_Aggregate then
            Dref :=
               Duplicate_Subexpr_No_Checks
                 (Aggregate_Discriminant_Val (Disc_Ent));

         else
            Dref :=
              Make_Selected_Component (Loc,
                Prefix        =>
                  Duplicate_Subexpr_No_Checks (N, Name_Req => True),
                Selector_Name => Make_Identifier (Loc, Chars (Disc_Ent)));

            Set_Is_In_Discriminant_Check (Dref);
         end if;

         Evolve_Or_Else (Cond,
           Make_Op_Ne (Loc,
             Left_Opnd  => Dref,
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

      function Left_Expression (Op : Node_Id) return Node_Id;
      --  Return the relevant expression from the left operand of the given
      --  short circuit form: this is LO itself, except if LO is a qualified
      --  expression, a type conversion, or an expression with actions, in
      --  which case this is Left_Expression (Expression (LO)).

      ---------------------
      -- Left_Expression --
      ---------------------

      function Left_Expression (Op : Node_Id) return Node_Id is
         LE : Node_Id := Left_Opnd (Op);
      begin
         while Nkind_In (LE, N_Qualified_Expression,
                             N_Type_Conversion,
                             N_Expression_With_Actions)
         loop
            LE := Expression (LE);
         end loop;

         return LE;
      end Left_Expression;

   --  Start of processing for Check_Needed

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

         --  Done if out of subexpression (note that we allow generated stuff
         --  such as itype declarations in this context, to keep the loop going
         --  since we may well have generated such stuff in complex situations.
         --  Also done if no parent (probably an error condition, but no point
         --  in behaving nasty if we find it).

         if No (P)
           or else (K not in N_Subexpr and then Comes_From_Source (P))
         then
            return True;

         --  Or/Or Else case, where test is part of the right operand, or is
         --  part of one of the actions associated with the right operand, and
         --  the left operand is an equality test.

         elsif K = N_Op_Or then
            exit when N = Right_Opnd (P)
              and then Nkind (Left_Expression (P)) = N_Op_Eq;

         elsif K = N_Or_Else then
            exit when (N = Right_Opnd (P)
                        or else
                          (Is_List_Member (N)
                             and then List_Containing (N) = Actions (P)))
              and then Nkind (Left_Expression (P)) = N_Op_Eq;

         --  Similar test for the And/And then case, where the left operand
         --  is an inequality test.

         elsif K = N_Op_And then
            exit when N = Right_Opnd (P)
              and then Nkind (Left_Expression (P)) = N_Op_Ne;

         elsif K = N_And_Then then
            exit when (N = Right_Opnd (P)
                        or else
                          (Is_List_Member (N)
                            and then List_Containing (N) = Actions (P)))
              and then Nkind (Left_Expression (P)) = N_Op_Ne;
         end if;

         N := P;
      end loop;

      --  If we fall through the loop, then we have a conditional with an
      --  appropriate test as its left operand, so look further.

      L := Left_Expression (P);

      --  L is an "=" or "/=" operator: extract its operands

      R := Right_Opnd (L);
      L := Left_Opnd (L);

      --  Left operand of test must match original variable

      if Nkind (L) not in N_Has_Entity or else Entity (L) /= Entity (Nod) then
         return True;
      end if;

      --  Right operand of test must be key value (zero or null)

      case Check is
         when Access_Check =>
            if not Known_Null (R) then
               return True;
            end if;

         when Division_Check =>
            if not Compile_Time_Known_Value (R)
              or else Expr_Value (R) /= Uint_0
            then
               return True;
            end if;

         when others =>
            raise Program_Error;
      end case;

      --  Here we have the optimizable case, warn if not short-circuited

      if K = N_Op_And or else K = N_Op_Or then
         Error_Msg_Warn := SPARK_Mode /= On;

         case Check is
            when Access_Check =>
               if GNATprove_Mode then
                  Error_Msg_N
                    ("Constraint_Error might have been raised (access check)",
                     Parent (Nod));
               else
                  Error_Msg_N
                    ("Constraint_Error may be raised (access check)??",
                     Parent (Nod));
               end if;

            when Division_Check =>
               if GNATprove_Mode then
                  Error_Msg_N
                    ("Constraint_Error might have been raised (zero divide)",
                     Parent (Nod));
               else
                  Error_Msg_N
                    ("Constraint_Error may be raised (zero divide)??",
                     Parent (Nod));
               end if;

            when others =>
               raise Program_Error;
         end case;

         if K = N_Op_And then
            Error_Msg_N -- CODEFIX
              ("use `AND THEN` instead of AND??", P);
         else
            Error_Msg_N -- CODEFIX
              ("use `OR ELSE` instead of OR??", P);
         end if;

         --  If not short-circuited, we need the check

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

      --  Only do this check for expressions that come from source. We assume
      --  that expander generated assignments explicitly include any necessary
      --  checks. Note that this is not just an optimization, it avoids
      --  infinite recursions.

      elsif not Comes_From_Source (Expr) then
         return;

      --  For a selected component, check the prefix

      elsif Nkind (Expr) = N_Selected_Component then
         Check_Valid_Lvalue_Subscripts (Prefix (Expr));
         return;

      --  Case of indexed component

      elsif Nkind (Expr) = N_Indexed_Component then
         Apply_Subscript_Validity_Checks (Expr);

         --  Prefix may itself be or contain an indexed component, and these
         --  subscripts need checking as well.

         Check_Valid_Lvalue_Subscripts (Prefix (Expr));
      end if;
   end Check_Valid_Lvalue_Subscripts;

   ----------------------------------
   -- Null_Exclusion_Static_Checks --
   ----------------------------------

   procedure Null_Exclusion_Static_Checks (N : Node_Id) is
      Error_Node : Node_Id;
      Expr       : Node_Id;
      Has_Null   : constant Boolean := Has_Null_Exclusion (N);
      K          : constant Node_Kind := Nkind (N);
      Typ        : Entity_Id;

   begin
      pragma Assert
        (Nkind_In (K, N_Component_Declaration,
                      N_Discriminant_Specification,
                      N_Function_Specification,
                      N_Object_Declaration,
                      N_Parameter_Specification));

      if K = N_Function_Specification then
         Typ := Etype (Defining_Entity (N));
      else
         Typ := Etype (Defining_Identifier (N));
      end if;

      case K is
         when N_Component_Declaration =>
            if Present (Access_Definition (Component_Definition (N))) then
               Error_Node := Component_Definition (N);
            else
               Error_Node := Subtype_Indication (Component_Definition (N));
            end if;

         when N_Discriminant_Specification =>
            Error_Node    := Discriminant_Type (N);

         when N_Function_Specification =>
            Error_Node    := Result_Definition (N);

         when N_Object_Declaration =>
            Error_Node    := Object_Definition (N);

         when N_Parameter_Specification =>
            Error_Node    := Parameter_Type (N);

         when others =>
            raise Program_Error;
      end case;

      if Has_Null then

         --  Enforce legality rule 3.10 (13): A null exclusion can only be
         --  applied to an access [sub]type.

         if not Is_Access_Type (Typ) then
            Error_Msg_N
              ("`NOT NULL` allowed only for an access type", Error_Node);

         --  Enforce legality rule RM 3.10(14/1): A null exclusion can only
         --  be applied to a [sub]type that does not exclude null already.

         elsif Can_Never_Be_Null (Typ)
           and then Comes_From_Source (Typ)
         then
            Error_Msg_NE
              ("`NOT NULL` not allowed (& already excludes null)",
               Error_Node, Typ);
         end if;
      end if;

      --  Check that null-excluding objects are always initialized, except for
      --  deferred constants, for which the expression will appear in the full
      --  declaration.

      if K = N_Object_Declaration
        and then No (Expression (N))
        and then not Constant_Present (N)
        and then not No_Initialization (N)
      then
         --  Add an expression that assigns null. This node is needed by
         --  Apply_Compile_Time_Constraint_Error, which will replace this with
         --  a Constraint_Error node.

         Set_Expression (N, Make_Null (Sloc (N)));
         Set_Etype (Expression (N), Etype (Defining_Identifier (N)));

         Apply_Compile_Time_Constraint_Error
           (N      => Expression (N),
            Msg    =>
              "(Ada 2005) null-excluding objects must be initialized??",
            Reason => CE_Null_Not_Allowed);
      end if;

      --  Check that a null-excluding component, formal or object is not being
      --  assigned a null value. Otherwise generate a warning message and
      --  replace Expression (N) by an N_Constraint_Error node.

      if K /= N_Function_Specification then
         Expr := Expression (N);

         if Present (Expr) and then Known_Null (Expr) then
            case K is
               when N_Component_Declaration      |
                    N_Discriminant_Specification =>
                  Apply_Compile_Time_Constraint_Error
                    (N      => Expr,
                     Msg    => "(Ada 2005) null not allowed "
                               & "in null-excluding components??",
                     Reason => CE_Null_Not_Allowed);

               when N_Object_Declaration =>
                  Apply_Compile_Time_Constraint_Error
                    (N      => Expr,
                     Msg    => "(Ada 2005) null not allowed "
                               & "in null-excluding objects?",
                     Reason => CE_Null_Not_Allowed);

               when N_Parameter_Specification =>
                  Apply_Compile_Time_Constraint_Error
                    (N      => Expr,
                     Msg    => "(Ada 2005) null not allowed "
                               & "in null-excluding formals??",
                     Reason => CE_Null_Not_Allowed);

               when others =>
                  null;
            end case;
         end if;
      end if;
   end Null_Exclusion_Static_Checks;

   ----------------------------------
   -- Conditional_Statements_Begin --
   ----------------------------------

   procedure Conditional_Statements_Begin is
   begin
      Saved_Checks_TOS := Saved_Checks_TOS + 1;

      --  If stack overflows, kill all checks, that way we know to simply reset
      --  the number of saved checks to zero on return. This should never occur
      --  in practice.

      if Saved_Checks_TOS > Saved_Checks_Stack'Last then
         Kill_All_Checks;

      --  In the normal case, we just make a new stack entry saving the current
      --  number of saved checks for a later restore.

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

      --  If the saved checks stack overflowed, then we killed all checks, so
      --  setting the number of saved checks back to zero is correct. This
      --  should never occur in practice.

      if Saved_Checks_TOS > Saved_Checks_Stack'Last then
         Num_Saved_Checks := 0;

      --  In the normal case, restore the number of saved checks from the top
      --  stack entry.

      else
         Num_Saved_Checks := Saved_Checks_Stack (Saved_Checks_TOS);

         if Debug_Flag_CC then
            w ("Conditional_Statements_End: Num_Saved_Checks = ",
               Num_Saved_Checks);
         end if;
      end if;

      Saved_Checks_TOS := Saved_Checks_TOS - 1;
   end Conditional_Statements_End;

   -------------------------
   -- Convert_From_Bignum --
   -------------------------

   function Convert_From_Bignum (N : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      pragma Assert (Is_RTE (Etype (N), RE_Bignum));

      --  Construct call From Bignum

      return
        Make_Function_Call (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_From_Bignum), Loc),
          Parameter_Associations => New_List (Relocate_Node (N)));
   end Convert_From_Bignum;

   -----------------------
   -- Convert_To_Bignum --
   -----------------------

   function Convert_To_Bignum (N : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  Nothing to do if Bignum already except call Relocate_Node

      if Is_RTE (Etype (N), RE_Bignum) then
         return Relocate_Node (N);

      --  Otherwise construct call to To_Bignum, converting the operand to the
      --  required Long_Long_Integer form.

      else
         pragma Assert (Is_Signed_Integer_Type (Etype (N)));
         return
           Make_Function_Call (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_To_Bignum), Loc),
             Parameter_Associations => New_List (
               Convert_To (Standard_Long_Long_Integer, Relocate_Node (N))));
      end if;
   end Convert_To_Bignum;

   ---------------------
   -- Determine_Range --
   ---------------------

   Cache_Size : constant := 2 ** 10;
   type Cache_Index is range 0 .. Cache_Size - 1;
   --  Determine size of below cache (power of 2 is more efficient)

   Determine_Range_Cache_N  : array (Cache_Index) of Node_Id;
   Determine_Range_Cache_V  : array (Cache_Index) of Boolean;
   Determine_Range_Cache_Lo : array (Cache_Index) of Uint;
   Determine_Range_Cache_Hi : array (Cache_Index) of Uint;
   --  The above arrays are used to implement a small direct cache for
   --  Determine_Range calls. Because of the way Determine_Range recursively
   --  traces subexpressions, and because overflow checking calls the routine
   --  on the way up the tree, a quadratic behavior can otherwise be
   --  encountered in large expressions. The cache entry for node N is stored
   --  in the (N mod Cache_Size) entry, and can be validated by checking the
   --  actual node value stored there. The Range_Cache_V array records the
   --  setting of Assume_Valid for the cache entry.

   procedure Determine_Range
     (N            : Node_Id;
      OK           : out Boolean;
      Lo           : out Uint;
      Hi           : out Uint;
      Assume_Valid : Boolean := False)
   is
      Typ : Entity_Id := Etype (N);
      --  Type to use, may get reset to base type for possibly invalid entity

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

      Btyp : Entity_Id;
      --  Base type

      function OK_Operands return Boolean;
      --  Used for binary operators. Determines the ranges of the left and
      --  right operands, and if they are both OK, returns True, and puts
      --  the results in Lo_Right, Hi_Right, Lo_Left, Hi_Left.

      -----------------
      -- OK_Operands --
      -----------------

      function OK_Operands return Boolean is
      begin
         Determine_Range
           (Left_Opnd  (N), OK1, Lo_Left,  Hi_Left, Assume_Valid);

         if not OK1 then
            return False;
         end if;

         Determine_Range
           (Right_Opnd (N), OK1, Lo_Right, Hi_Right, Assume_Valid);
         return OK1;
      end OK_Operands;

   --  Start of processing for Determine_Range

   begin
      --  For temporary constants internally generated to remove side effects
      --  we must use the corresponding expression to determine the range of
      --  the expression.

      if Is_Entity_Name (N)
        and then Nkind (Parent (Entity (N))) = N_Object_Declaration
        and then Ekind (Entity (N)) = E_Constant
        and then Is_Internal_Name (Chars (Entity (N)))
      then
         Determine_Range
           (Expression (Parent (Entity (N))), OK, Lo, Hi, Assume_Valid);
         return;
      end if;

      --  Prevent junk warnings by initializing range variables

      Lo  := No_Uint;
      Hi  := No_Uint;
      Lor := No_Uint;
      Hir := No_Uint;

      --  If type is not defined, we can't determine its range

      if No (Typ)

        --  We don't deal with anything except discrete types

        or else not Is_Discrete_Type (Typ)

        --  Ignore type for which an error has been posted, since range in
        --  this case may well be a bogosity deriving from the error. Also
        --  ignore if error posted on the reference node.

        or else Error_Posted (N) or else Error_Posted (Typ)
      then
         OK := False;
         return;
      end if;

      --  For all other cases, we can determine the range

      OK := True;

      --  If value is compile time known, then the possible range is the one
      --  value that we know this expression definitely has.

      if Compile_Time_Known_Value (N) then
         Lo := Expr_Value (N);
         Hi := Lo;
         return;
      end if;

      --  Return if already in the cache

      Cindex := Cache_Index (N mod Cache_Size);

      if Determine_Range_Cache_N (Cindex) = N
           and then
         Determine_Range_Cache_V (Cindex) = Assume_Valid
      then
         Lo := Determine_Range_Cache_Lo (Cindex);
         Hi := Determine_Range_Cache_Hi (Cindex);
         return;
      end if;

      --  Otherwise, start by finding the bounds of the type of the expression,
      --  the value cannot be outside this range (if it is, then we have an
      --  overflow situation, which is a separate check, we are talking here
      --  only about the expression value).

      --  First a check, never try to find the bounds of a generic type, since
      --  these bounds are always junk values, and it is only valid to look at
      --  the bounds in an instance.

      if Is_Generic_Type (Typ) then
         OK := False;
         return;
      end if;

      --  First step, change to use base type unless we know the value is valid

      if (Is_Entity_Name (N) and then Is_Known_Valid (Entity (N)))
        or else Assume_No_Invalid_Values
        or else Assume_Valid
      then
         null;
      else
         Typ := Underlying_Type (Base_Type (Typ));
      end if;

      --  Retrieve the base type. Handle the case where the base type is a
      --  private enumeration type.

      Btyp := Base_Type (Typ);

      if Is_Private_Type (Btyp) and then Present (Full_View (Btyp)) then
         Btyp := Full_View (Btyp);
      end if;

      --  We use the actual bound unless it is dynamic, in which case use the
      --  corresponding base type bound if possible. If we can't get a bound
      --  then we figure we can't determine the range (a peculiar case, that
      --  perhaps cannot happen, but there is no point in bombing in this
      --  optimization circuit.

      --  First the low bound

      Bound := Type_Low_Bound (Typ);

      if Compile_Time_Known_Value (Bound) then
         Lo := Expr_Value (Bound);

      elsif Compile_Time_Known_Value (Type_Low_Bound (Btyp)) then
         Lo := Expr_Value (Type_Low_Bound (Btyp));

      else
         OK := False;
         return;
      end if;

      --  Now the high bound

      Bound := Type_High_Bound (Typ);

      --  We need the high bound of the base type later on, and this should
      --  always be compile time known. Again, it is not clear that this
      --  can ever be false, but no point in bombing.

      if Compile_Time_Known_Value (Type_High_Bound (Btyp)) then
         Hbound := Expr_Value (Type_High_Bound (Btyp));
         Hi := Hbound;

      else
         OK := False;
         return;
      end if;

      --  If we have a static subtype, then that may have a tighter bound so
      --  use the upper bound of the subtype instead in this case.

      if Compile_Time_Known_Value (Bound) then
         Hi := Expr_Value (Bound);
      end if;

      --  We may be able to refine this value in certain situations. If any
      --  refinement is possible, then Lor and Hir are set to possibly tighter
      --  bounds, and OK1 is set to True.

      case Nkind (N) is

         --  For unary plus, result is limited by range of operand

         when N_Op_Plus =>
            Determine_Range
              (Right_Opnd (N), OK1, Lor, Hir, Assume_Valid);

         --  For unary minus, determine range of operand, and negate it

         when N_Op_Minus =>
            Determine_Range
              (Right_Opnd (N), OK1, Lo_Right, Hi_Right, Assume_Valid);

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

         --  Division is tricky. The only case we consider is where the right
         --  operand is a positive constant, and in this case we simply divide
         --  the bounds of the left operand

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

         --  For binary subtraction, get range of each operand and do the worst
         --  case subtraction to get the result range.

         when N_Op_Subtract =>
            if OK_Operands then
               Lor := Lo_Left - Hi_Right;
               Hir := Hi_Left - Lo_Right;
            end if;

         --  For MOD, if right operand is a positive constant, then result must
         --  be in the allowable range of mod results.

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

         --  For REM, if right operand is a positive constant, then result must
         --  be in the allowable range of mod results.

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
               --  possible range of values of the attribute expression.

               when Name_Pos | Name_Val =>
                  Determine_Range
                    (First (Expressions (N)), OK1, Lor, Hir, Assume_Valid);

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

                     --  If the index type is a formal type or derived from
                     --  one, the bounds are not static.

                     if Is_Generic_Type (Root_Type (Etype (Indx))) then
                        OK := False;
                        return;
                     end if;

                     Determine_Range
                       (Type_Low_Bound (Etype (Indx)), OK1, LL, LU,
                        Assume_Valid);

                     if OK1 then
                        Determine_Range
                          (Type_High_Bound (Etype (Indx)), OK1, UL, UU,
                           Assume_Valid);

                        if OK1 then

                           --  The maximum value for Length is the biggest
                           --  possible gap between the values of the bounds.
                           --  But of course, this value cannot be negative.

                           Hir := UI_Max (Uint_0, UU - LL + 1);

                           --  For constrained arrays, the minimum value for
                           --  Length is taken from the actual value of the
                           --  bounds, since the index will be exactly of this
                           --  subtype.

                           if Is_Constrained (Atyp) then
                              Lor := UI_Max (Uint_0, UL - LU + 1);

                           --  For an unconstrained array, the minimum value
                           --  for length is always zero.

                           else
                              Lor := Uint_0;
                           end if;
                        end if;
                     end if;
                  end;

               --  No special handling for other attributes
               --  Probably more opportunities exist here???

               when others =>
                  OK1 := False;

            end case;

         --  For type conversion from one discrete type to another, we can
         --  refine the range using the converted value.

         when N_Type_Conversion =>
            Determine_Range (Expression (N), OK1, Lor, Hir, Assume_Valid);

         --  Nothing special to do for all other expression kinds

         when others =>
            OK1 := False;
            Lor := No_Uint;
            Hir := No_Uint;
      end case;

      --  At this stage, if OK1 is true, then we know that the actual result of
      --  the computed expression is in the range Lor .. Hir. We can use this
      --  to restrict the possible range of results.

      if OK1 then

         --  If the refined value of the low bound is greater than the type
         --  high bound, then reset it to the more restrictive value. However,
         --  we do NOT do this for the case of a modular type where the
         --  possible upper bound on the value is above the base type high
         --  bound, because that means the result could wrap.

         if Lor > Lo
           and then not (Is_Modular_Integer_Type (Typ) and then Hir > Hbound)
         then
            Lo := Lor;
         end if;

         --  Similarly, if the refined value of the high bound is less than the
         --  value so far, then reset it to the more restrictive value. Again,
         --  we do not do this if the refined low bound is negative for a
         --  modular type, since this would wrap.

         if Hir < Hi
           and then not (Is_Modular_Integer_Type (Typ) and then Lor < Uint_0)
         then
            Hi := Hir;
         end if;
      end if;

      --  Set cache entry for future call and we are all done

      Determine_Range_Cache_N  (Cindex) := N;
      Determine_Range_Cache_V  (Cindex) := Assume_Valid;
      Determine_Range_Cache_Lo (Cindex) := Lo;
      Determine_Range_Cache_Hi (Cindex) := Hi;
      return;

   --  If any exception occurs, it means that we have some bug in the compiler,
   --  possibly triggered by a previous error, or by some unforeseen peculiar
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

      return Scope_Suppress.Suppress (Discriminant_Check);
   end Discriminant_Checks_Suppressed;

   --------------------------------
   -- Division_Checks_Suppressed --
   --------------------------------

   function Division_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Division_Check);
      else
         return Scope_Suppress.Suppress (Division_Check);
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

      if Scope_Suppress.Suppress (Elaboration_Check) then
         return True;
      elsif Dynamic_Elaboration_Checks then
         return Scope_Suppress.Suppress (All_Checks);
      else
         return False;
      end if;
   end Elaboration_Checks_Suppressed;

   ---------------------------
   -- Enable_Overflow_Check --
   ---------------------------

   procedure Enable_Overflow_Check (N : Node_Id) is
      Typ  : constant Entity_Id           := Base_Type (Etype (N));
      Mode : constant Overflow_Mode_Type := Overflow_Check_Mode;
      Chk  : Nat;
      OK   : Boolean;
      Ent  : Entity_Id;
      Ofs  : Uint;
      Lo   : Uint;
      Hi   : Uint;

   begin
      if Debug_Flag_CC then
         w ("Enable_Overflow_Check for node ", Int (N));
         Write_Str ("  Source location = ");
         wl (Sloc (N));
         pg (Union_Id (N));
      end if;

      --  No check if overflow checks suppressed for type of node

      if Overflow_Checks_Suppressed (Etype (N)) then
         return;

      --  Nothing to do for unsigned integer types, which do not overflow

      elsif Is_Modular_Integer_Type (Typ) then
         return;
      end if;

      --  This is the point at which processing for STRICT mode diverges
      --  from processing for MINIMIZED/ELIMINATED modes. This divergence is
      --  probably more extreme that it needs to be, but what is going on here
      --  is that when we introduced MINIMIZED/ELIMINATED modes, we wanted
      --  to leave the processing for STRICT mode untouched. There were
      --  two reasons for this. First it avoided any incompatible change of
      --  behavior. Second, it guaranteed that STRICT mode continued to be
      --  legacy reliable.

      --  The big difference is that in STRICT mode there is a fair amount of
      --  circuitry to try to avoid setting the Do_Overflow_Check flag if we
      --  know that no check is needed. We skip all that in the two new modes,
      --  since really overflow checking happens over a whole subtree, and we
      --  do the corresponding optimizations later on when applying the checks.

      if Mode in Minimized_Or_Eliminated then
         if not (Overflow_Checks_Suppressed (Etype (N)))
           and then not (Is_Entity_Name (N)
                          and then Overflow_Checks_Suppressed (Entity (N)))
         then
            Activate_Overflow_Check (N);
         end if;

         if Debug_Flag_CC then
            w ("Minimized/Eliminated mode");
         end if;

         return;
      end if;

      --  Remainder of processing is for STRICT case, and is unchanged from
      --  earlier versions preceding the addition of MINIMIZED/ELIMINATED.

      --  Nothing to do if the range of the result is known OK. We skip this
      --  for conversions, since the caller already did the check, and in any
      --  case the condition for deleting the check for a type conversion is
      --  different.

      if Nkind (N) /= N_Type_Conversion then
         Determine_Range (N, OK, Lo, Hi, Assume_Valid => True);

         --  Note in the test below that we assume that the range is not OK
         --  if a bound of the range is equal to that of the type. That's not
         --  quite accurate but we do this for the following reasons:

         --   a) The way that Determine_Range works, it will typically report
         --      the bounds of the value as being equal to the bounds of the
         --      type, because it either can't tell anything more precise, or
         --      does not think it is worth the effort to be more precise.

         --   b) It is very unusual to have a situation in which this would
         --      generate an unnecessary overflow check (an example would be
         --      a subtype with a range 0 .. Integer'Last - 1 to which the
         --      literal value one is added).

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

      --  If not in optimizing mode, set flag and we are done. We are also done
      --  (and just set the flag) if the type is not a discrete type, since it
      --  is not worth the effort to eliminate checks for other than discrete
      --  types. In addition, we take this same path if we have stored the
      --  maximum number of checks possible already (a very unlikely situation,
      --  but we do not want to blow up).

      if Optimization_Level = 0
        or else not Is_Discrete_Type (Etype (N))
        or else Num_Saved_Checks = Saved_Checks'Last
      then
         Activate_Overflow_Check (N);

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
         Activate_Overflow_Check (N);
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

      Activate_Overflow_Check (N);
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

   --  If we get an exception, then something went wrong, probably because of
   --  an error in the structure of the tree due to an incorrect program. Or
   --  it may be a bug in the optimization circuit. In either case the safest
   --  thing is simply to set the check flag unconditionally.

   exception
      when others =>
         Activate_Overflow_Check (N);

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
      --  Return if unchecked type conversion with range check killed. In this
      --  case we never set the flag (that's what Kill_Range_Check is about).

      if Nkind (N) = N_Unchecked_Type_Conversion
        and then Kill_Range_Check (N)
      then
         return;
      end if;

      --  Do not set range check flag if parent is assignment statement or
      --  object declaration with Suppress_Assignment_Checks flag set

      if Nkind_In (Parent (N), N_Assignment_Statement, N_Object_Declaration)
        and then Suppress_Assignment_Checks (Parent (N))
      then
         return;
      end if;

      --  Check for various cases where we should suppress the range check

      --  No check if range checks suppressed for type of node

      if Present (Etype (N)) and then Range_Checks_Suppressed (Etype (N)) then
         return;

      --  No check if node is an entity name, and range checks are suppressed
      --  for this entity, or for the type of this entity.

      elsif Is_Entity_Name (N)
        and then (Range_Checks_Suppressed (Entity (N))
                   or else Range_Checks_Suppressed (Etype (Entity (N))))
      then
         return;

      --  No checks if index of array, and index checks are suppressed for
      --  the array object or the type of the array.

      elsif Nkind (Parent (N)) = N_Indexed_Component then
         declare
            Pref : constant Node_Id := Prefix (Parent (N));
         begin
            if Is_Entity_Name (Pref)
              and then Index_Checks_Suppressed (Entity (Pref))
            then
               return;
            elsif Index_Checks_Suppressed (Etype (Pref)) then
               return;
            end if;
         end;
      end if;

      --  Debug trace output

      if Debug_Flag_CC then
         w ("Enable_Range_Check for node ", Int (N));
         Write_Str ("  Source location = ");
         wl (Sloc (N));
         pg (Union_Id (N));
      end if;

      --  If not in optimizing mode, set flag and we are done. We are also done
      --  (and just set the flag) if the type is not a discrete type, since it
      --  is not worth the effort to eliminate checks for other than discrete
      --  types. In addition, we take this same path if we have stored the
      --  maximum number of checks possible already (a very unlikely situation,
      --  but we do not want to blow up).

      if Optimization_Level = 0
        or else No (Etype (N))
        or else not Is_Discrete_Type (Etype (N))
        or else Num_Saved_Checks = Saved_Checks'Last
      then
         Activate_Range_Check (N);

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
               --  perform check unconditionally: it depends on the bounds of
               --  an object and we cannot currently recognize whether the test
               --  may be redundant.

               if not Is_Constrained (Atyp) then
                  Activate_Range_Check (N);
                  return;
               end if;

            --  Ditto if the prefix is an explicit dereference whose designated
            --  type is unconstrained.

            elsif Nkind (Prefix (P)) = N_Explicit_Dereference
              and then not Is_Constrained (Atyp)
            then
               Activate_Range_Check (N);
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

         Activate_Range_Check (N);
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

         Activate_Range_Check (N);
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

      Activate_Range_Check (N);
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
         pg (Union_Id (Ttyp));
      end if;

   --  If we get an exception, then something went wrong, probably because of
   --  an error in the structure of the tree due to an incorrect program. Or
   --  it may be a bug in the optimization circuit. In either case the safest
   --  thing is simply to set the check flag unconditionally.

   exception
      when others =>
         Activate_Range_Check (N);

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

      --  Ignore call if range or validity checks suppressed on entity or type

      elsif Range_Or_Validity_Checks_Suppressed (Expr) then
         return;

      --  No check required if expression is from the expander, we assume the
      --  expander will generate whatever checks are needed. Note that this is
      --  not just an optimization, it avoids infinite recursions.

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

      --  Ignore case of enumeration with holes where the flag is set not to
      --  worry about holes, since no special validity check is needed

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

      --  No check on a universal real constant. The context will eventually
      --  convert it to a machine number for some target type, or report an
      --  illegality.

      elsif Nkind (Expr) = N_Real_Literal
        and then Etype (Expr) = Universal_Real
      then
         return;

      --  If the expression denotes a component of a packed boolean array,
      --  no possible check applies. We ignore the old ACATS chestnuts that
      --  involve Boolean range True..True.

      --  Note: validity checks are generated for expressions that yield a
      --  scalar type, when it is possible to create a value that is outside of
      --  the type. If this is a one-bit boolean no such value exists. This is
      --  an optimization, and it also prevents compiler blowing up during the
      --  elaboration of improperly expanded packed array references.

      elsif Nkind (Expr) = N_Indexed_Component
        and then Is_Bit_Packed_Array (Etype (Prefix (Expr)))
        and then Root_Type (Etype (Expr)) = Standard_Boolean
      then
         return;

      --  For an expression with actions, we want to insert the validity check
      --  on the final Expression.

      elsif Nkind (Expr) = N_Expression_With_Actions then
         Ensure_Valid (Expression (Expr));
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

               --  Only need to worry if we are argument of a procedure call
               --  since functions don't have out parameters. If this is an
               --  indirect or dispatching call, get signature from the
               --  subprogram type.

               if Nkind (P) = N_Procedure_Call_Statement then
                  L := Parameter_Associations (P);

                  if Is_Entity_Name (Name (P)) then
                     E := Entity (Name (P));
                  else
                     pragma Assert (Nkind (Name (P)) = N_Explicit_Dereference);
                     E := Etype (Name (P));
                  end if;

                  --  Only need to worry if there are indeed actuals, and if
                  --  this could be a procedure call, otherwise we cannot get a
                  --  match (either we are not an argument, or the mode of the
                  --  formal is not OUT). This test also filters out the
                  --  generic case.

                  if Is_Non_Empty_List (L) and then Is_Subprogram (E) then

                     --  This is the loop through parameters, looking for an
                     --  OUT parameter for which we are the argument.

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

      --  If this is a boolean expression, only its elementary operands need
      --  checking: if they are valid, a boolean or short-circuit operation
      --  with them will be valid as well.

      if Base_Type (Typ) = Standard_Boolean
        and then
         (Nkind (Expr) in N_Op or else Nkind (Expr) in N_Short_Circuit)
      then
         return;
      end if;

      --  If we fall through, a validity check is required

      Insert_Valid_Check (Expr);

      if Is_Entity_Name (Expr)
        and then Safe_To_Capture_Value (Expr, Entity (Expr))
      then
         Set_Is_Known_Valid (Entity (Expr));
      end if;
   end Ensure_Valid;

   ----------------------
   -- Expr_Known_Valid --
   ----------------------

   function Expr_Known_Valid (Expr : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Expr);

   begin
      --  Non-scalar types are always considered valid, since they never give
      --  rise to the issues of erroneous or bounded error behavior that are
      --  the concern. In formal reference manual terms the notion of validity
      --  only applies to scalar types. Note that even when packed arrays are
      --  represented using modular types, they are still arrays semantically,
      --  so they are also always valid (in particular, the unused bits can be
      --  random rubbish without affecting the validity of the array value).

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

      --  If the expression is the value of an object that is known to be
      --  valid, then clearly the expression value itself is valid.

      elsif Is_Entity_Name (Expr)
        and then Is_Known_Valid (Entity (Expr))

        --  Exclude volatile variables

        and then not Treat_As_Volatile (Entity (Expr))
      then
         return True;

      --  References to discriminants are always considered valid. The value
      --  of a discriminant gets checked when the object is built. Within the
      --  record, we consider it valid, and it is important to do so, since
      --  otherwise we can try to generate bogus validity checks which
      --  reference discriminants out of scope. Discriminants of concurrent
      --  types are excluded for the same reason.

      elsif Is_Entity_Name (Expr)
        and then Denotes_Discriminant (Expr, Check_Concurrent => True)
      then
         return True;

      --  If the type is one for which all values are known valid, then we are
      --  sure that the value is valid except in the slightly odd case where
      --  the expression is a reference to a variable whose size has been
      --  explicitly set to a value greater than the object size.

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

      elsif Nkind_In (Expr, N_Integer_Literal, N_Character_Literal) then
         return True;

      --  Real literals are assumed to be valid in VM targets

      elsif VM_Target /= No_VM and then Nkind (Expr) = N_Real_Literal then
         return True;

      --  If we have a type conversion or a qualification of a known valid
      --  value, then the result will always be valid.

      elsif Nkind_In (Expr, N_Type_Conversion, N_Qualified_Expression) then
         return Expr_Known_Valid (Expression (Expr));

      --  Case of expression is a non-floating-point operator. In this case we
      --  can assume the result is valid the generated code for the operator
      --  will include whatever checks are needed (e.g. range checks) to ensure
      --  validity. This assumption does not hold for the floating-point case,
      --  since floating-point operators can generate Infinite or NaN results
      --  which are considered invalid.

      --  Historical note: in older versions, the exemption of floating-point
      --  types from this assumption was done only in cases where the parent
      --  was an assignment, function call or parameter association. Presumably
      --  the idea was that in other contexts, the result would be checked
      --  elsewhere, but this list of cases was missing tests (at least the
      --  N_Object_Declaration case, as shown by a reported missing validity
      --  check), and it is not clear why function calls but not procedure
      --  calls were tested for. It really seems more accurate and much
      --  safer to recognize that expressions which are the result of a
      --  floating-point operator can never be assumed to be valid.

      elsif Nkind (Expr) in N_Op and then not Is_Floating_Point_Type (Typ) then
         return True;

      --  The result of a membership test is always valid, since it is true or
      --  false, there are no other possibilities.

      elsif Nkind (Expr) in N_Membership_Test then
         return True;

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
      --  Establish default, in case no entry is found

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

      --  Come here with expression of appropriate form, check if entity is an
      --  appropriate one for our purposes.

      if (Ekind (Ent) = E_Variable
            or else Is_Constant_Object (Ent))
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

      return;
   end Find_Check;

   ---------------------------------
   -- Generate_Discriminant_Check --
   ---------------------------------

   --  Note: the code for this procedure is derived from the
   --  Emit_Discriminant_Check Routine in trans.c.

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
      --  Keep track of the formal corresponding to the actual we build for
      --  each discriminant, in order to be able to perform the necessary type
      --  conversions.

      Scomp : Node_Id;
      --  Selected component reference for checking function argument

   begin
      Pref_Type := Etype (Pref);

      --  Force evaluation of the prefix, so that it does not get evaluated
      --  twice (once for the check, once for the actual reference). Such a
      --  double evaluation is always a potential source of inefficiency, and
      --  is functionally incorrect in the volatile case, or when the prefix
      --  may have side-effects. A non-volatile entity or a component of a
      --  non-volatile entity requires no evaluation.

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

      --  For an untagged derived type, use the discriminants of the parent
      --  which have been renamed in the derivation, possibly by a one-to-many
      --  discriminant constraint. For non-tagged type, initially get the Etype
      --  of the prefix

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
         --  playing discriminal games etc with this reference. Then we append
         --  the argument to the list we are gathering.

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
              Name                   => New_Occurrence_Of (Discr_Fct, Loc),
              Parameter_Associations => Args),
          Reason => CE_Discriminant_Check_Failed));
   end Generate_Discriminant_Check;

   ---------------------------
   -- Generate_Index_Checks --
   ---------------------------

   procedure Generate_Index_Checks (N : Node_Id) is

      function Entity_Of_Prefix return Entity_Id;
      --  Returns the entity of the prefix of N (or Empty if not found)

      ----------------------
      -- Entity_Of_Prefix --
      ----------------------

      function Entity_Of_Prefix return Entity_Id is
         P : Node_Id;

      begin
         P := Prefix (N);
         while not Is_Entity_Name (P) loop
            if not Nkind_In (P, N_Selected_Component,
                                N_Indexed_Component)
            then
               return Empty;
            end if;

            P := Prefix (P);
         end loop;

         return Entity (P);
      end Entity_Of_Prefix;

      --  Local variables

      Loc   : constant Source_Ptr := Sloc (N);
      A     : constant Node_Id    := Prefix (N);
      A_Ent : constant Entity_Id  := Entity_Of_Prefix;
      Sub   : Node_Id;

   --  Start of processing for Generate_Index_Checks

   begin
      --  Ignore call if the prefix is not an array since we have a serious
      --  error in the sources. Ignore it also if index checks are suppressed
      --  for array object or type.

      if not Is_Array_Type (Etype (A))
        or else (Present (A_Ent) and then Index_Checks_Suppressed (A_Ent))
        or else Index_Checks_Suppressed (Etype (A))
      then
         return;

      --  The indexed component we are dealing with contains 'Loop_Entry in its
      --  prefix. This case arises when analysis has determined that constructs
      --  such as

      --     Prefix'Loop_Entry (Expr)
      --     Prefix'Loop_Entry (Expr1, Expr2, ... ExprN)

      --  require rewriting for error detection purposes. A side effect of this
      --  action is the generation of index checks that mention 'Loop_Entry.
      --  Delay the generation of the check until 'Loop_Entry has been properly
      --  expanded. This is done in Expand_Loop_Entry_Attributes.

      elsif Nkind (Prefix (N)) = N_Attribute_Reference
        and then Attribute_Name (Prefix (N)) = Name_Loop_Entry
      then
         return;
      end if;

      --  Generate a raise of constraint error with the appropriate reason and
      --  a condition of the form:

      --    Base_Type (Sub) not in Array'Range (Subscript)

      --  Note that the reason we generate the conversion to the base type here
      --  is that we definitely want the range check to take place, even if it
      --  looks like the subtype is OK. Optimization considerations that allow
      --  us to omit the check have already been taken into account in the
      --  setting of the Do_Range_Check flag earlier on.

      Sub := First (Expressions (N));

      --  Handle string literals

      if Ekind (Etype (A)) = E_String_Literal_Subtype then
         if Do_Range_Check (Sub) then
            Set_Do_Range_Check (Sub, False);

            --  For string literals we obtain the bounds of the string from the
            --  associated subtype.

            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                   Make_Not_In (Loc,
                     Left_Opnd  =>
                       Convert_To (Base_Type (Etype (Sub)),
                         Duplicate_Subexpr_Move_Checks (Sub)),
                     Right_Opnd =>
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Etype (A), Loc),
                         Attribute_Name => Name_Range)),
                Reason => CE_Index_Check_Failed));
         end if;

      --  General case

      else
         declare
            A_Idx   : Node_Id := Empty;
            A_Range : Node_Id;
            Ind     : Nat;
            Num     : List_Id;
            Range_N : Node_Id;

         begin
            A_Idx := First_Index (Etype (A));
            Ind   := 1;
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

                  if Nkind (A_Idx) = N_Range then
                     A_Range := A_Idx;

                  elsif Nkind (A_Idx) = N_Identifier
                    or else Nkind (A_Idx) = N_Expanded_Name
                  then
                     A_Range := Scalar_Range (Entity (A_Idx));

                  else pragma Assert (Nkind (A_Idx) = N_Subtype_Indication);
                     A_Range := Range_Expression (Constraint (A_Idx));
                  end if;

                  --  For array objects with constant bounds we can generate
                  --  the index check using the bounds of the type of the index

                  if Present (A_Ent)
                    and then Ekind (A_Ent) = E_Variable
                    and then Is_Constant_Bound (Low_Bound (A_Range))
                    and then Is_Constant_Bound (High_Bound (A_Range))
                  then
                     Range_N :=
                       Make_Attribute_Reference (Loc,
                         Prefix         =>
                           New_Occurrence_Of (Etype (A_Idx), Loc),
                         Attribute_Name => Name_Range);

                  --  For arrays with non-constant bounds we cannot generate
                  --  the index check using the bounds of the type of the index
                  --  since it may reference discriminants of some enclosing
                  --  type. We obtain the bounds directly from the prefix
                  --  object.

                  else
                     if Ind = 1 then
                        Num := No_List;
                     else
                        Num := New_List (Make_Integer_Literal (Loc, Ind));
                     end if;

                     Range_N :=
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           Duplicate_Subexpr_Move_Checks (A, Name_Req => True),
                         Attribute_Name => Name_Range,
                         Expressions    => Num);
                  end if;

                  Insert_Action (N,
                    Make_Raise_Constraint_Error (Loc,
                      Condition =>
                         Make_Not_In (Loc,
                           Left_Opnd  =>
                             Convert_To (Base_Type (Etype (Sub)),
                               Duplicate_Subexpr_Move_Checks (Sub)),
                           Right_Opnd => Range_N),
                      Reason => CE_Index_Check_Failed));
               end if;

               A_Idx := Next_Index (A_Idx);
               Ind := Ind + 1;
               Next (Sub);
            end loop;
         end;
      end if;
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
      --  First special case, if the source type is already within the range
      --  of the target type, then no check is needed (probably we should have
      --  stopped Do_Range_Check from being set in the first place, but better
      --  late than never in preventing junk code.

      if In_Subrange_Of (Source_Type, Target_Type)

        --  We do NOT apply this if the source node is a literal, since in this
        --  case the literal has already been labeled as having the subtype of
        --  the target.

        and then not
          (Nkind_In (N, N_Integer_Literal, N_Real_Literal, N_Character_Literal)
             or else
               (Is_Entity_Name (N)
                 and then Ekind (Entity (N)) = E_Enumeration_Literal))

        --  Also do not apply this for floating-point if Check_Float_Overflow

        and then not
          (Is_Floating_Point_Type (Source_Type) and Check_Float_Overflow)
      then
         return;
      end if;

      --  We need a check, so force evaluation of the node, so that it does
      --  not get evaluated twice (once for the check, once for the actual
      --  reference). Such a double evaluation is always a potential source
      --  of inefficiency, and is functionally incorrect in the volatile case.

      if not Is_Entity_Name (N) or else Treat_As_Volatile (Entity (N)) then
         Force_Evaluation (N);
      end if;

      --  The easiest case is when Source_Base_Type and Target_Base_Type are
      --  the same since in this case we can simply do a direct check of the
      --  value of N against the bounds of Target_Type.

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

      --  Unchecked_Convert_To is used instead of Convert_To to handle the case
      --  of converting from an enumeration value to an integer type, such as
      --  occurs for the case of generating a range check on Enum'Val(Exp)
      --  (which used to be handled by gigi). This is OK, since the conversion
      --  itself does not require a check.

      elsif In_Subrange_Of (Target_Type, Source_Base_Type) then
         Insert_Action (N,
           Make_Raise_Constraint_Error (Loc,
             Condition =>
               Make_Not_In (Loc,
                 Left_Opnd  => Duplicate_Subexpr (N),

                 Right_Opnd =>
                   Make_Range (Loc,
                     Low_Bound =>
                       Unchecked_Convert_To (Source_Base_Type,
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (Target_Type, Loc),
                           Attribute_Name => Name_First)),

                     High_Bound =>
                       Unchecked_Convert_To (Source_Base_Type,
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (Target_Type, Loc),
                           Attribute_Name => Name_Last)))),
             Reason => Reason));

      --  Note that at this stage we now that the Target_Base_Type is not in
      --  the range of the Source_Base_Type (since even the Target_Type itself
      --  is not in this range). It could still be the case that Source_Type is
      --  in range of the target base type since we have not checked that case.

      --  If that is the case, we can freely convert the source to the target,
      --  and then test the target result against the bounds.

      elsif In_Subrange_Of (Source_Type, Target_Base_Type) then

         --  We make a temporary to hold the value of the converted value
         --  (converted to the base type), and then we will do the test against
         --  this temporary.

         --     Tnn : constant Target_Base_Type := Target_Base_Type (N);
         --     [constraint_error when Tnn not in Target_Type]

         --  Then the conversion itself is replaced by an occurrence of Tnn

         declare
            Tnn : constant Entity_Id := Make_Temporary (Loc, 'T', N);

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

            --  Set the type of N, because the declaration for Tnn might not
            --  be analyzed yet, as is the case if N appears within a record
            --  declaration, as a discriminant constraint or expression.

            Set_Etype (N, Target_Base_Type);
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

            --  In other words, the unsigned type is either the same size as
            --  the target, or it is larger. It cannot be smaller.

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

            --  This is definitely the most annoying case.

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
         --  the target is unsigned.

         else
            pragma Assert (not Is_Unsigned_Type (Source_Base_Type)
                            and then Is_Unsigned_Type (Target_Base_Type));

            --  If the source is signed and the target is unsigned, then we
            --  know that the target is not shorter than the source (otherwise
            --  the target base type would be in the source base type range).

            --  In other words, the unsigned type is either the same size as
            --  the target, or it is larger. It cannot be smaller.

            --  Clearly we have an error if the source value is negative since
            --  no unsigned type can have negative values. If the source type
            --  is non-negative, then the check can be done using the target
            --  type.

            --    Tnn : constant Target_Base_Type (N) := Target_Type;

            --    [constraint_error
            --       when N < 0 or else Tnn not in Target_Type];

            --  We turn off all checks for the conversion of N to the target
            --  base type, since we generate the explicit check to ensure that
            --  the value is non-negative

            declare
               Tnn : constant Entity_Id := Make_Temporary (Loc, 'T', N);

            begin
               Insert_Actions (N, New_List (
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Tnn,
                   Object_Definition   =>
                     New_Occurrence_Of (Target_Base_Type, Loc),
                   Constant_Present    => True,
                   Expression          =>
                     Make_Unchecked_Type_Conversion (Loc,
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

                   Reason     => Reason)),
                 Suppress => All_Checks);

               --  Set the Etype explicitly, because Insert_Actions may have
               --  placed the declaration in the freeze list for an enclosing
               --  construct, and thus it is not analyzed yet.

               Set_Etype (Tnn, Target_Base_Type);
               Rewrite (N, New_Occurrence_Of (Tnn, Loc));
            end;
         end if;
      end if;
   end Generate_Range_Check;

   ------------------
   -- Get_Check_Id --
   ------------------

   function Get_Check_Id (N : Name_Id) return Check_Id is
   begin
      --  For standard check name, we can do a direct computation

      if N in First_Check_Name .. Last_Check_Name then
         return Check_Id (N - (First_Check_Name - 1));

      --  For non-standard names added by pragma Check_Name, search table

      else
         for J in All_Checks + 1 .. Check_Names.Last loop
            if Check_Names.Table (J) = N then
               return J;
            end if;
         end loop;
      end if;

      --  No matching name found

      return No_Check_Id;
   end Get_Check_Id;

   ---------------------
   -- Get_Discriminal --
   ---------------------

   function Get_Discriminal (E : Entity_Id; Bound : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (E);
      D   : Entity_Id;
      Sc  : Entity_Id;

   begin
      --  The bound can be a bona fide parameter of a protected operation,
      --  rather than a prival encoded as an in-parameter.

      if No (Discriminal_Link (Entity (Bound))) then
         return Bound;
      end if;

      --  Climb the scope stack looking for an enclosing protected type. If
      --  we run out of scopes, return the bound itself.

      Sc := Scope (E);
      while Present (Sc) loop
         if Sc = Standard_Standard then
            return Bound;
         elsif Ekind (Sc) = E_Protected_Type then
            exit;
         end if;

         Sc := Scope (Sc);
      end loop;

      D := First_Discriminant (Sc);
      while Present (D) loop
         if Chars (D) = Chars (Bound) then
            return New_Occurrence_Of (Discriminal (D), Loc);
         end if;

         Next_Discriminant (D);
      end loop;

      return Bound;
   end Get_Discriminal;

   ----------------------
   -- Get_Range_Checks --
   ----------------------

   function Get_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty) return Check_Result
   is
   begin
      return
        Selected_Range_Checks (Ck_Node, Target_Typ, Source_Typ, Warn_Node);
   end Get_Range_Checks;

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
         return Scope_Suppress.Suppress (Index_Check);
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

      Check_Names.Init;

      for J in Int range 1 .. All_Checks loop
         Check_Names.Append (Name_Id (Int (First_Check_Name) + J - 1));
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
         or else (not Range_Checks_Suppressed (Suppress_Typ));

   begin
      --  For now we just return if Checks_On is false, however this should be
      --  enhanced to check for an always True value in the condition and to
      --  generate a compilation warning???

      if not Expander_Active or not Checks_On then
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
      Typ : constant Entity_Id  := Etype (Expr);
      Exp : Node_Id;

   begin
      --  Do not insert if checks off, or if not checking validity or
      --  if expression is known to be valid

      if not Validity_Checks_On
        or else Range_Or_Validity_Checks_Suppressed (Expr)
        or else Expr_Known_Valid (Expr)
      then
         return;
      end if;

      --  Do not insert checks within a predicate function. This will arise
      --  if the current unit and the predicate function are being compiled
      --  with validity checks enabled.

      if Present (Predicate_Function (Typ))
        and then Current_Scope = Predicate_Function (Typ)
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

      --  We are about to insert the validity check for Exp. We save and
      --  reset the Do_Range_Check flag over this validity check, and then
      --  put it back for the final original reference (Exp may be rewritten).

      declare
         DRC : constant Boolean := Do_Range_Check (Exp);
         PV  : Node_Id;
         CE  : Node_Id;

      begin
         Set_Do_Range_Check (Exp, False);

         --  Force evaluation to avoid multiple reads for atomic/volatile

         if Is_Entity_Name (Exp)
           and then Is_Volatile (Entity (Exp))
         then
            Force_Evaluation (Exp, Name_Req => True);
         end if;

         --  Build the prefix for the 'Valid call

         PV := Duplicate_Subexpr_No_Checks (Exp, Name_Req => True);

         --  A rather specialized kludge. If PV is an analyzed expression
         --  which is an indexed component of a packed array that has not
         --  been properly expanded, turn off its Analyzed flag to make sure
         --  it gets properly reexpanded.

         --  The reason this arises is that Duplicate_Subexpr_No_Checks did
         --  an analyze with the old parent pointer. This may point e.g. to
         --  a subprogram call, which deactivates this expansion.

         if Analyzed (PV)
           and then Nkind (PV) = N_Indexed_Component
           and then Present (Packed_Array_Type (Etype (Prefix (PV))))
         then
            Set_Analyzed (PV, False);
         end if;

         --  Build the raise CE node to check for validity

         CE :=
            Make_Raise_Constraint_Error (Loc,
              Condition =>
                Make_Op_Not (Loc,
                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => PV,
                      Attribute_Name => Name_Valid)),
              Reason => CE_Invalid_Data);

         --  Insert the validity check. Note that we do this with validity
         --  checks turned off, to avoid recursion, we do not want validity
         --  checks on the validity checking code itself.

         Insert_Action (Expr, CE, Suppress => Validity_Check);

         --  If the expression is a reference to an element of a bit-packed
         --  array, then it is rewritten as a renaming declaration. If the
         --  expression is an actual in a call, it has not been expanded,
         --  waiting for the proper point at which to do it. The same happens
         --  with renamings, so that we have to force the expansion now. This
         --  non-local complication is due to code in exp_ch2,adb, exp_ch4.adb
         --  and exp_ch6.adb.

         if Is_Entity_Name (Exp)
           and then Nkind (Parent (Entity (Exp))) =
                                                 N_Object_Renaming_Declaration
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

         --  Put back the Do_Range_Check flag on the resulting (possibly
         --  rewritten) expression.

         --  Note: it might be thought that a validity check is not required
         --  when a range check is present, but that's not the case, because
         --  the back end is allowed to assume for the range check that the
         --  operand is within its declared range (an assumption that validity
         --  checking is all about NOT assuming).

         --  Note: no need to worry about Possible_Local_Raise here, it will
         --  already have been called if original node has Do_Range_Check set.

         Set_Do_Range_Check (Exp, DRC);
      end;
   end Insert_Valid_Check;

   -------------------------------------
   -- Is_Signed_Integer_Arithmetic_Op --
   -------------------------------------

   function Is_Signed_Integer_Arithmetic_Op (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is
         when N_Op_Abs   | N_Op_Add      | N_Op_Divide   | N_Op_Expon |
              N_Op_Minus | N_Op_Mod      | N_Op_Multiply | N_Op_Plus  |
              N_Op_Rem   | N_Op_Subtract =>
            return Is_Signed_Integer_Type (Etype (N));

         when N_If_Expression | N_Case_Expression =>
            return Is_Signed_Integer_Type (Etype (N));

         when others =>
            return False;
      end case;
   end Is_Signed_Integer_Arithmetic_Op;

   ----------------------------------
   -- Install_Null_Excluding_Check --
   ----------------------------------

   procedure Install_Null_Excluding_Check (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Parent (N));
      Typ : constant Entity_Id  := Etype (N);

      function Safe_To_Capture_In_Parameter_Value return Boolean;
      --  Determines if it is safe to capture Known_Non_Null status for an
      --  the entity referenced by node N. The caller ensures that N is indeed
      --  an entity name. It is safe to capture the non-null status for an IN
      --  parameter when the reference occurs within a declaration that is sure
      --  to be executed as part of the declarative region.

      procedure Mark_Non_Null;
      --  After installation of check, if the node in question is an entity
      --  name, then mark this entity as non-null if possible.

      function Safe_To_Capture_In_Parameter_Value return Boolean is
         E     : constant Entity_Id := Entity (N);
         S     : constant Entity_Id := Current_Scope;
         S_Par : Node_Id;

      begin
         if Ekind (E) /= E_In_Parameter then
            return False;
         end if;

         --  Two initial context checks. We must be inside a subprogram body
         --  with declarations and reference must not appear in nested scopes.

         if (Ekind (S) /= E_Function and then Ekind (S) /= E_Procedure)
           or else Scope (E) /= S
         then
            return False;
         end if;

         S_Par := Parent (Parent (S));

         if Nkind (S_Par) /= N_Subprogram_Body
           or else No (Declarations (S_Par))
         then
            return False;
         end if;

         declare
            N_Decl : Node_Id;
            P      : Node_Id;

         begin
            --  Retrieve the declaration node of N (if any). Note that N
            --  may be a part of a complex initialization expression.

            P := Parent (N);
            N_Decl := Empty;
            while Present (P) loop

               --  If we have a short circuit form, and we are within the right
               --  hand expression, we return false, since the right hand side
               --  is not guaranteed to be elaborated.

               if Nkind (P) in N_Short_Circuit
                 and then N = Right_Opnd (P)
               then
                  return False;
               end if;

               --  Similarly, if we are in an if expression and not part of the
               --  condition, then we return False, since neither the THEN or
               --  ELSE dependent expressions will always be elaborated.

               if Nkind (P) = N_If_Expression
                 and then N /= First (Expressions (P))
               then
                  return False;
               end if;

               --  If within a case expression, and not part of the expression,
               --  then return False, since a particular dependent expression
               --  may not always be elaborated

               if Nkind (P) = N_Case_Expression
                 and then N /= Expression (P)
               then
                  return False;
               end if;

               --  While traversing the parent chain, if node N belongs to a
               --  statement, then it may never appear in a declarative region.

               if Nkind (P) in N_Statement_Other_Than_Procedure_Call
                 or else Nkind (P) = N_Procedure_Call_Statement
               then
                  return False;
               end if;

               --  If we are at a declaration, record it and exit

               if Nkind (P) in N_Declaration
                 and then Nkind (P) not in N_Subprogram_Specification
               then
                  N_Decl := P;
                  exit;
               end if;

               P := Parent (P);
            end loop;

            if No (N_Decl) then
               return False;
            end if;

            return List_Containing (N_Decl) = Declarations (S_Par);
         end;
      end Safe_To_Capture_In_Parameter_Value;

      -------------------
      -- Mark_Non_Null --
      -------------------

      procedure Mark_Non_Null is
      begin
         --  Only case of interest is if node N is an entity name

         if Is_Entity_Name (N) then

            --  For sure, we want to clear an indication that this is known to
            --  be null, since if we get past this check, it definitely is not.

            Set_Is_Known_Null (Entity (N), False);

            --  We can mark the entity as known to be non-null if either it is
            --  safe to capture the value, or in the case of an IN parameter,
            --  which is a constant, if the check we just installed is in the
            --  declarative region of the subprogram body. In this latter case,
            --  a check is decisive for the rest of the body if the expression
            --  is sure to be elaborated, since we know we have to elaborate
            --  all declarations before executing the body.

            --  Couldn't this always be part of Safe_To_Capture_Value ???

            if Safe_To_Capture_Value (N, Entity (N))
              or else Safe_To_Capture_In_Parameter_Value
            then
               Set_Is_Known_Non_Null (Entity (N));
            end if;
         end if;
      end Mark_Non_Null;

   --  Start of processing for Install_Null_Excluding_Check

   begin
      pragma Assert (Is_Access_Type (Typ));

      --  No check inside a generic, check will be emitted in instance

      if Inside_A_Generic then
         return;
      end if;

      --  No check needed if known to be non-null

      if Known_Non_Null (N) then
         return;
      end if;

      --  If known to be null, here is where we generate a compile time check

      if Known_Null (N) then

         --  Avoid generating warning message inside init procs. In SPARK mode
         --  we can go ahead and call Apply_Compile_Time_Constraint_Error
         --  since it will be turned into an error in any case.

         if (not Inside_Init_Proc or else SPARK_Mode = On)

           --  Do not emit the warning within a conditional expression,
           --  where the expression might not be evaluated, and the warning
           --  appear as extraneous noise.

           and then not Within_Case_Or_If_Expression (N)
         then
            Apply_Compile_Time_Constraint_Error
              (N, "null value not allowed here??", CE_Access_Check_Failed);

         --  Remaining cases, where we silently insert the raise

         else
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Reason => CE_Access_Check_Failed));
         end if;

         Mark_Non_Null;
         return;
      end if;

      --  If entity is never assigned, for sure a warning is appropriate

      if Is_Entity_Name (N) then
         Check_Unset_Reference (N);
      end if;

      --  No check needed if checks are suppressed on the range. Note that we
      --  don't set Is_Known_Non_Null in this case (we could legitimately do
      --  so, since the program is erroneous, but we don't like to casually
      --  propagate such conclusions from erroneosity).

      if Access_Checks_Suppressed (Typ) then
         return;
      end if;

      --  No check needed for access to concurrent record types generated by
      --  the expander. This is not just an optimization (though it does indeed
      --  remove junk checks). It also avoids generation of junk warnings.

      if Nkind (N) in N_Has_Chars
        and then Chars (N) = Name_uObject
        and then Is_Concurrent_Record_Type
                   (Directly_Designated_Type (Etype (N)))
      then
         return;
      end if;

      --  No check needed in interface thunks since the runtime check is
      --  already performed at the caller side.

      if Is_Thunk (Current_Scope) then
         return;
      end if;

      --  No check needed for the Get_Current_Excep.all.all idiom generated by
      --  the expander within exception handlers, since we know that the value
      --  can never be null.

      --  Is this really the right way to do this? Normally we generate such
      --  code in the expander with checks off, and that's how we suppress this
      --  kind of junk check ???

      if Nkind (N) = N_Function_Call
        and then Nkind (Name (N)) = N_Explicit_Dereference
        and then Nkind (Prefix (Name (N))) = N_Identifier
        and then Is_RTE (Entity (Prefix (Name (N))), RE_Get_Current_Excep)
      then
         return;
      end if;

      --  Otherwise install access check

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd  => Duplicate_Subexpr_Move_Checks (N),
              Right_Opnd => Make_Null (Loc)),
          Reason => CE_Access_Check_Failed));

      Mark_Non_Null;
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

      --  Now deal with possible local raise handling

      Possible_Local_Raise (R_Cno, Standard_Constraint_Error);
   end Install_Static_Check;

   -------------------------
   -- Is_Check_Suppressed --
   -------------------------

   function Is_Check_Suppressed (E : Entity_Id; C : Check_Id) return Boolean is
      Ptr : Suppress_Stack_Entry_Ptr;

   begin
      --  First search the local entity suppress stack. We search this from the
      --  top of the stack down so that we get the innermost entry that applies
      --  to this case if there are nested entries.

      Ptr := Local_Suppress_Stack_Top;
      while Ptr /= null loop
         if (Ptr.Entity = Empty or else Ptr.Entity = E)
           and then (Ptr.Check = All_Checks or else Ptr.Check = C)
         then
            return Ptr.Suppress;
         end if;

         Ptr := Ptr.Prev;
      end loop;

      --  Now search the global entity suppress table for a matching entry.
      --  We also search this from the top down so that if there are multiple
      --  pragmas for the same entity, the last one applies (not clear what
      --  or whether the RM specifies this handling, but it seems reasonable).

      Ptr := Global_Suppress_Stack_Top;
      while Ptr /= null loop
         if (Ptr.Entity = Empty or else Ptr.Entity = E)
           and then (Ptr.Check = All_Checks or else Ptr.Check = C)
         then
            return Ptr.Suppress;
         end if;

         Ptr := Ptr.Prev;
      end loop;

      --  If we did not find a matching entry, then use the normal scope
      --  suppress value after all (actually this will be the global setting
      --  since it clearly was not overridden at any point). For a predefined
      --  check, we test the specific flag. For a user defined check, we check
      --  the All_Checks flag. The Overflow flag requires special handling to
      --  deal with the General vs Assertion case

      if C = Overflow_Check then
         return Overflow_Checks_Suppressed (Empty);
      elsif C in Predefined_Check_Id then
         return Scope_Suppress.Suppress (C);
      else
         return Scope_Suppress.Suppress (All_Checks);
      end if;
   end Is_Check_Suppressed;

   ---------------------
   -- Kill_All_Checks --
   ---------------------

   procedure Kill_All_Checks is
   begin
      if Debug_Flag_CC then
         w ("Kill_All_Checks");
      end if;

      --  We reset the number of saved checks to zero, and also modify all
      --  stack entries for statement ranges to indicate that the number of
      --  checks at each level is now zero.

      Num_Saved_Checks := 0;

      --  Note: the Int'Min here avoids any possibility of J being out of
      --  range when called from e.g. Conditional_Statements_Begin.

      for J in 1 .. Int'Min (Saved_Checks_TOS, Saved_Checks_Stack'Last) loop
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
         return Scope_Suppress.Suppress (Length_Check);
      end if;
   end Length_Checks_Suppressed;

   -----------------------
   -- Make_Bignum_Block --
   -----------------------

   function Make_Bignum_Block (Loc : Source_Ptr) return Node_Id is
      M : constant Entity_Id := Make_Defining_Identifier (Loc, Name_uM);

   begin
      return
        Make_Block_Statement (Loc,
          Declarations => New_List (
            Make_Object_Declaration (Loc,
              Defining_Identifier => M,
              Object_Definition   =>
                New_Occurrence_Of (RTE (RE_Mark_Id), Loc),
              Expression          =>
                Make_Function_Call (Loc,
                  Name => New_Occurrence_Of (RTE (RE_SS_Mark), Loc)))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Procedure_Call_Statement (Loc,
                  Name => New_Occurrence_Of (RTE (RE_SS_Release), Loc),
                  Parameter_Associations => New_List (
                    New_Occurrence_Of (M, Loc))))));
   end Make_Bignum_Block;

   ----------------------------------
   -- Minimize_Eliminate_Overflows --
   ----------------------------------

   --  This is a recursive routine that is called at the top of an expression
   --  tree to properly process overflow checking for a whole subtree by making
   --  recursive calls to process operands. This processing may involve the use
   --  of bignum or long long integer arithmetic, which will change the types
   --  of operands and results. That's why we can't do this bottom up (since
   --  it would interfere with semantic analysis).

   --  What happens is that if MINIMIZED/ELIMINATED mode is in effect then
   --  the operator expansion routines, as well as the expansion routines for
   --  if/case expression, do nothing (for the moment) except call the routine
   --  to apply the overflow check (Apply_Arithmetic_Overflow_Check). That
   --  routine does nothing for non top-level nodes, so at the point where the
   --  call is made for the top level node, the entire expression subtree has
   --  not been expanded, or processed for overflow. All that has to happen as
   --  a result of the top level call to this routine.

   --  As noted above, the overflow processing works by making recursive calls
   --  for the operands, and figuring out what to do, based on the processing
   --  of these operands (e.g. if a bignum operand appears, the parent op has
   --  to be done in bignum mode), and the determined ranges of the operands.

   --  After possible rewriting of a constituent subexpression node, a call is
   --  made to either reexpand the node (if nothing has changed) or reanalyze
   --  the node (if it has been modified by the overflow check processing). The
   --  Analyzed_Flag is set to False before the reexpand/reanalyze. To avoid
   --  a recursive call into the whole overflow apparatus, an important rule
   --  for this call is that the overflow handling mode must be temporarily set
   --  to STRICT.

   procedure Minimize_Eliminate_Overflows
     (N         : Node_Id;
      Lo        : out Uint;
      Hi        : out Uint;
      Top_Level : Boolean)
   is
      Rtyp : constant Entity_Id := Etype (N);
      pragma Assert (Is_Signed_Integer_Type (Rtyp));
      --  Result type, must be a signed integer type

      Check_Mode : constant Overflow_Mode_Type := Overflow_Check_Mode;
      pragma Assert (Check_Mode in Minimized_Or_Eliminated);

      Loc : constant Source_Ptr := Sloc (N);

      Rlo, Rhi : Uint;
      --  Ranges of values for right operand (operator case)

      Llo, Lhi : Uint;
      --  Ranges of values for left operand (operator case)

      LLIB : constant Entity_Id := Base_Type (Standard_Long_Long_Integer);
      --  Operands and results are of this type when we convert

      LLLo : constant Uint := Intval (Type_Low_Bound  (LLIB));
      LLHi : constant Uint := Intval (Type_High_Bound (LLIB));
      --  Bounds of Long_Long_Integer

      Binary : constant Boolean := Nkind (N) in N_Binary_Op;
      --  Indicates binary operator case

      OK : Boolean;
      --  Used in call to Determine_Range

      Bignum_Operands : Boolean;
      --  Set True if one or more operands is already of type Bignum, meaning
      --  that for sure (regardless of Top_Level setting) we are committed to
      --  doing the operation in Bignum mode (or in the case of a case or if
      --  expression, converting all the dependent expressions to Bignum).

      Long_Long_Integer_Operands : Boolean;
      --  Set True if one or more operands is already of type Long_Long_Integer
      --  which means that if the result is known to be in the result type
      --  range, then we must convert such operands back to the result type.

      procedure Reanalyze (Typ : Entity_Id; Suppress : Boolean := False);
      --  This is called when we have modified the node and we therefore need
      --  to reanalyze it. It is important that we reset the mode to STRICT for
      --  this reanalysis, since if we leave it in MINIMIZED or ELIMINATED mode
      --  we would reenter this routine recursively which would not be good.
      --  The argument Suppress is set True if we also want to suppress
      --  overflow checking for the reexpansion (this is set when we know
      --  overflow is not possible). Typ is the type for the reanalysis.

      procedure Reexpand (Suppress : Boolean := False);
      --  This is like Reanalyze, but does not do the Analyze step, it only
      --  does a reexpansion. We do this reexpansion in STRICT mode, so that
      --  instead of reentering the MINIMIZED/ELIMINATED mode processing, we
      --  follow the normal expansion path (e.g. converting A**4 to A**2**2).
      --  Note that skipping reanalysis is not just an optimization, testing
      --  has showed up several complex cases in which reanalyzing an already
      --  analyzed node causes incorrect behavior.

      function In_Result_Range return Boolean;
      --  Returns True iff Lo .. Hi are within range of the result type

      procedure Max (A : in out Uint; B : Uint);
      --  If A is No_Uint, sets A to B, else to UI_Max (A, B)

      procedure Min (A : in out Uint; B : Uint);
      --  If A is No_Uint, sets A to B, else to UI_Min (A, B)

      ---------------------
      -- In_Result_Range --
      ---------------------

      function In_Result_Range return Boolean is
      begin
         if Lo = No_Uint or else Hi = No_Uint then
            return False;

         elsif Is_Static_Subtype (Etype (N)) then
            return Lo >= Expr_Value (Type_Low_Bound  (Rtyp))
                     and then
                   Hi <= Expr_Value (Type_High_Bound (Rtyp));

         else
            return Lo >= Expr_Value (Type_Low_Bound  (Base_Type (Rtyp)))
                     and then
                   Hi <= Expr_Value (Type_High_Bound (Base_Type (Rtyp)));
         end if;
      end In_Result_Range;

      ---------
      -- Max --
      ---------

      procedure Max (A : in out Uint; B : Uint) is
      begin
         if A = No_Uint or else B > A then
            A := B;
         end if;
      end Max;

      ---------
      -- Min --
      ---------

      procedure Min (A : in out Uint; B : Uint) is
      begin
         if A = No_Uint or else B < A then
            A := B;
         end if;
      end Min;

      ---------------
      -- Reanalyze --
      ---------------

      procedure Reanalyze (Typ : Entity_Id; Suppress : Boolean := False) is
         Svg : constant Overflow_Mode_Type :=
                 Scope_Suppress.Overflow_Mode_General;
         Sva : constant Overflow_Mode_Type :=
                 Scope_Suppress.Overflow_Mode_Assertions;
         Svo : constant Boolean             :=
                 Scope_Suppress.Suppress (Overflow_Check);

      begin
         Scope_Suppress.Overflow_Mode_General    := Strict;
         Scope_Suppress.Overflow_Mode_Assertions := Strict;

         if Suppress then
            Scope_Suppress.Suppress (Overflow_Check) := True;
         end if;

         Analyze_And_Resolve (N, Typ);

         Scope_Suppress.Suppress (Overflow_Check)  := Svo;
         Scope_Suppress.Overflow_Mode_General    := Svg;
         Scope_Suppress.Overflow_Mode_Assertions := Sva;
      end Reanalyze;

      --------------
      -- Reexpand --
      --------------

      procedure Reexpand (Suppress : Boolean := False) is
         Svg : constant Overflow_Mode_Type :=
                 Scope_Suppress.Overflow_Mode_General;
         Sva : constant Overflow_Mode_Type :=
                 Scope_Suppress.Overflow_Mode_Assertions;
         Svo : constant Boolean             :=
                 Scope_Suppress.Suppress (Overflow_Check);

      begin
         Scope_Suppress.Overflow_Mode_General    := Strict;
         Scope_Suppress.Overflow_Mode_Assertions := Strict;
         Set_Analyzed (N, False);

         if Suppress then
            Scope_Suppress.Suppress (Overflow_Check) := True;
         end if;

         Expand (N);

         Scope_Suppress.Suppress (Overflow_Check)  := Svo;
         Scope_Suppress.Overflow_Mode_General    := Svg;
         Scope_Suppress.Overflow_Mode_Assertions := Sva;
      end Reexpand;

   --  Start of processing for Minimize_Eliminate_Overflows

   begin
      --  Case where we do not have a signed integer arithmetic operation

      if not Is_Signed_Integer_Arithmetic_Op (N) then

         --  Use the normal Determine_Range routine to get the range. We
         --  don't require operands to be valid, invalid values may result in
         --  rubbish results where the result has not been properly checked for
         --  overflow, that's fine.

         Determine_Range (N, OK, Lo, Hi, Assume_Valid => False);

         --  If Determine_Range did not work (can this in fact happen? Not
         --  clear but might as well protect), use type bounds.

         if not OK then
            Lo := Intval (Type_Low_Bound  (Base_Type (Etype (N))));
            Hi := Intval (Type_High_Bound (Base_Type (Etype (N))));
         end if;

         --  If we don't have a binary operator, all we have to do is to set
         --  the Hi/Lo range, so we are done.

         return;

      --  Processing for if expression

      elsif Nkind (N) = N_If_Expression then
         declare
            Then_DE : constant Node_Id := Next (First (Expressions (N)));
            Else_DE : constant Node_Id := Next (Then_DE);

         begin
            Bignum_Operands := False;

            Minimize_Eliminate_Overflows
              (Then_DE, Lo, Hi, Top_Level => False);

            if Lo = No_Uint then
               Bignum_Operands := True;
            end if;

            Minimize_Eliminate_Overflows
              (Else_DE, Rlo, Rhi, Top_Level => False);

            if Rlo = No_Uint then
               Bignum_Operands := True;
            else
               Long_Long_Integer_Operands :=
                 Etype (Then_DE) = LLIB or else Etype (Else_DE) = LLIB;

               Min (Lo, Rlo);
               Max (Hi, Rhi);
            end if;

            --  If at least one of our operands is now Bignum, we must rebuild
            --  the if expression to use Bignum operands. We will analyze the
            --  rebuilt if expression with overflow checks off, since once we
            --  are in bignum mode, we are all done with overflow checks.

            if Bignum_Operands then
               Rewrite (N,
                 Make_If_Expression (Loc,
                   Expressions => New_List (
                     Remove_Head (Expressions (N)),
                     Convert_To_Bignum (Then_DE),
                     Convert_To_Bignum (Else_DE)),
                   Is_Elsif    => Is_Elsif (N)));

               Reanalyze (RTE (RE_Bignum), Suppress => True);

            --  If we have no Long_Long_Integer operands, then we are in result
            --  range, since it means that none of our operands felt the need
            --  to worry about overflow (otherwise it would have already been
            --  converted to long long integer or bignum). We reexpand to
            --  complete the expansion of the if expression (but we do not
            --  need to reanalyze).

            elsif not Long_Long_Integer_Operands then
               Set_Do_Overflow_Check (N, False);
               Reexpand;

            --  Otherwise convert us to long long integer mode. Note that we
            --  don't need any further overflow checking at this level.

            else
               Convert_To_And_Rewrite (LLIB, Then_DE);
               Convert_To_And_Rewrite (LLIB, Else_DE);
               Set_Etype (N, LLIB);

               --  Now reanalyze with overflow checks off

               Set_Do_Overflow_Check (N, False);
               Reanalyze (LLIB, Suppress => True);
            end if;
         end;

         return;

      --  Here for case expression

      elsif Nkind (N) = N_Case_Expression then
         Bignum_Operands := False;
         Long_Long_Integer_Operands := False;

         declare
            Alt : Node_Id;

         begin
            --  Loop through expressions applying recursive call

            Alt := First (Alternatives (N));
            while Present (Alt) loop
               declare
                  Aexp : constant Node_Id := Expression (Alt);

               begin
                  Minimize_Eliminate_Overflows
                    (Aexp, Lo, Hi, Top_Level => False);

                  if Lo = No_Uint then
                     Bignum_Operands := True;
                  elsif Etype (Aexp) = LLIB then
                     Long_Long_Integer_Operands := True;
                  end if;
               end;

               Next (Alt);
            end loop;

            --  If we have no bignum or long long integer operands, it means
            --  that none of our dependent expressions could raise overflow.
            --  In this case, we simply return with no changes except for
            --  resetting the overflow flag, since we are done with overflow
            --  checks for this node. We will reexpand to get the needed
            --  expansion for the case expression, but we do not need to
            --  reanalyze, since nothing has changed.

            if not (Bignum_Operands or Long_Long_Integer_Operands) then
               Set_Do_Overflow_Check (N, False);
               Reexpand (Suppress => True);

            --  Otherwise we are going to rebuild the case expression using
            --  either bignum or long long integer operands throughout.

            else
               declare
                  Rtype    : Entity_Id;
                  New_Alts : List_Id;
                  New_Exp  : Node_Id;

               begin
                  New_Alts := New_List;
                  Alt := First (Alternatives (N));
                  while Present (Alt) loop
                     if Bignum_Operands then
                        New_Exp := Convert_To_Bignum (Expression (Alt));
                        Rtype   := RTE (RE_Bignum);
                     else
                        New_Exp := Convert_To (LLIB, Expression (Alt));
                        Rtype   := LLIB;
                     end if;

                     Append_To (New_Alts,
                       Make_Case_Expression_Alternative (Sloc (Alt),
                         Actions          => No_List,
                         Discrete_Choices => Discrete_Choices (Alt),
                         Expression       => New_Exp));

                     Next (Alt);
                  end loop;

                  Rewrite (N,
                    Make_Case_Expression (Loc,
                      Expression   => Expression (N),
                      Alternatives => New_Alts));

                  Reanalyze (Rtype, Suppress => True);
               end;
            end if;
         end;

         return;
      end if;

      --  If we have an arithmetic operator we make recursive calls on the
      --  operands to get the ranges (and to properly process the subtree
      --  that lies below us).

      Minimize_Eliminate_Overflows
        (Right_Opnd (N), Rlo, Rhi, Top_Level => False);

      if Binary then
         Minimize_Eliminate_Overflows
           (Left_Opnd (N), Llo, Lhi, Top_Level => False);
      end if;

      --  Record if we have Long_Long_Integer operands

      Long_Long_Integer_Operands :=
        Etype (Right_Opnd (N)) = LLIB
          or else (Binary and then Etype (Left_Opnd (N)) = LLIB);

      --  If either operand is a bignum, then result will be a bignum and we
      --  don't need to do any range analysis. As previously discussed we could
      --  do range analysis in such cases, but it could mean working with giant
      --  numbers at compile time for very little gain (the number of cases
      --  in which we could slip back from bignum mode is small).

      if Rlo = No_Uint or else (Binary and then Llo = No_Uint) then
         Lo := No_Uint;
         Hi := No_Uint;
         Bignum_Operands := True;

      --  Otherwise compute result range

      else
         Bignum_Operands := False;

         case Nkind (N) is

            --  Absolute value

            when N_Op_Abs =>
               Lo := Uint_0;
               Hi := UI_Max (abs Rlo, abs Rhi);

            --  Addition

            when N_Op_Add =>
               Lo := Llo + Rlo;
               Hi := Lhi + Rhi;

            --  Division

            when N_Op_Divide =>

               --  If the right operand can only be zero, set 0..0

               if Rlo = 0 and then Rhi = 0 then
                  Lo := Uint_0;
                  Hi := Uint_0;

               --  Possible bounds of division must come from dividing end
               --  values of the input ranges (four possibilities), provided
               --  zero is not included in the possible values of the right
               --  operand.

               --  Otherwise, we just consider two intervals of values for
               --  the right operand: the interval of negative values (up to
               --  -1) and the interval of positive values (starting at 1).
               --  Since division by 1 is the identity, and division by -1
               --  is negation, we get all possible bounds of division in that
               --  case by considering:
               --    - all values from the division of end values of input
               --      ranges;
               --    - the end values of the left operand;
               --    - the negation of the end values of the left operand.

               else
                  declare
                     Mrk : constant Uintp.Save_Mark := Mark;
                     --  Mark so we can release the RR and Ev values

                     Ev1 : Uint;
                     Ev2 : Uint;
                     Ev3 : Uint;
                     Ev4 : Uint;

                  begin
                     --  Discard extreme values of zero for the divisor, since
                     --  they will simply result in an exception in any case.

                     if Rlo = 0 then
                        Rlo := Uint_1;
                     elsif Rhi = 0 then
                        Rhi := -Uint_1;
                     end if;

                     --  Compute possible bounds coming from dividing end
                     --  values of the input ranges.

                     Ev1 := Llo / Rlo;
                     Ev2 := Llo / Rhi;
                     Ev3 := Lhi / Rlo;
                     Ev4 := Lhi / Rhi;

                     Lo := UI_Min (UI_Min (Ev1, Ev2), UI_Min (Ev3, Ev4));
                     Hi := UI_Max (UI_Max (Ev1, Ev2), UI_Max (Ev3, Ev4));

                     --  If the right operand can be both negative or positive,
                     --  include the end values of the left operand in the
                     --  extreme values, as well as their negation.

                     if Rlo < 0 and then Rhi > 0 then
                        Ev1 := Llo;
                        Ev2 := -Llo;
                        Ev3 := Lhi;
                        Ev4 := -Lhi;

                        Min (Lo,
                             UI_Min (UI_Min (Ev1, Ev2), UI_Min (Ev3, Ev4)));
                        Max (Hi,
                             UI_Max (UI_Max (Ev1, Ev2), UI_Max (Ev3, Ev4)));
                     end if;

                     --  Release the RR and Ev values

                     Release_And_Save (Mrk, Lo, Hi);
                  end;
               end if;

            --  Exponentiation

            when N_Op_Expon =>

               --  Discard negative values for the exponent, since they will
               --  simply result in an exception in any case.

               if Rhi < 0 then
                  Rhi := Uint_0;
               elsif Rlo < 0 then
                  Rlo := Uint_0;
               end if;

               --  Estimate number of bits in result before we go computing
               --  giant useless bounds. Basically the number of bits in the
               --  result is the number of bits in the base multiplied by the
               --  value of the exponent. If this is big enough that the result
               --  definitely won't fit in Long_Long_Integer, switch to bignum
               --  mode immediately, and avoid computing giant bounds.

               --  The comparison here is approximate, but conservative, it
               --  only clicks on cases that are sure to exceed the bounds.

               if Num_Bits (UI_Max (abs Llo, abs Lhi)) * Rhi + 1 > 100 then
                  Lo := No_Uint;
                  Hi := No_Uint;

               --  If right operand is zero then result is 1

               elsif Rhi = 0 then
                  Lo := Uint_1;
                  Hi := Uint_1;

               else
                  --  High bound comes either from exponentiation of largest
                  --  positive value to largest exponent value, or from
                  --  the exponentiation of most negative value to an
                  --  even exponent.

                  declare
                     Hi1, Hi2 : Uint;

                  begin
                     if Lhi > 0 then
                        Hi1 := Lhi ** Rhi;
                     else
                        Hi1 := Uint_0;
                     end if;

                     if Llo < 0 then
                        if Rhi mod 2 = 0 then
                           Hi2 := Llo ** Rhi;
                        else
                           Hi2 := Llo ** (Rhi - 1);
                        end if;
                     else
                        Hi2 := Uint_0;
                     end if;

                     Hi := UI_Max (Hi1, Hi2);
                  end;

                  --  Result can only be negative if base can be negative

                  if Llo < 0 then
                     if Rhi mod 2 = 0 then
                        Lo := Llo ** (Rhi - 1);
                     else
                        Lo := Llo ** Rhi;
                     end if;

                  --  Otherwise low bound is minimum ** minimum

                  else
                     Lo := Llo ** Rlo;
                  end if;
               end if;

            --  Negation

            when N_Op_Minus =>
               Lo := -Rhi;
               Hi := -Rlo;

            --  Mod

            when N_Op_Mod =>
               declare
                  Maxabs : constant Uint := UI_Max (abs Rlo, abs Rhi) - 1;
                  --  This is the maximum absolute value of the result

               begin
                  Lo := Uint_0;
                  Hi := Uint_0;

                  --  The result depends only on the sign and magnitude of
                  --  the right operand, it does not depend on the sign or
                  --  magnitude of the left operand.

                  if Rlo < 0 then
                     Lo := -Maxabs;
                  end if;

                  if Rhi > 0 then
                     Hi := Maxabs;
                  end if;
               end;

            --  Multiplication

            when N_Op_Multiply =>

               --  Possible bounds of multiplication must come from multiplying
               --  end values of the input ranges (four possibilities).

               declare
                  Mrk : constant Uintp.Save_Mark := Mark;
                  --  Mark so we can release the Ev values

                  Ev1 : constant Uint := Llo * Rlo;
                  Ev2 : constant Uint := Llo * Rhi;
                  Ev3 : constant Uint := Lhi * Rlo;
                  Ev4 : constant Uint := Lhi * Rhi;

               begin
                  Lo := UI_Min (UI_Min (Ev1, Ev2), UI_Min (Ev3, Ev4));
                  Hi := UI_Max (UI_Max (Ev1, Ev2), UI_Max (Ev3, Ev4));

                  --  Release the Ev values

                  Release_And_Save (Mrk, Lo, Hi);
               end;

            --  Plus operator (affirmation)

            when N_Op_Plus =>
               Lo := Rlo;
               Hi := Rhi;

            --  Remainder

            when N_Op_Rem =>
               declare
                  Maxabs : constant Uint := UI_Max (abs Rlo, abs Rhi) - 1;
                  --  This is the maximum absolute value of the result. Note
                  --  that the result range does not depend on the sign of the
                  --  right operand.

               begin
                  Lo := Uint_0;
                  Hi := Uint_0;

                  --  Case of left operand negative, which results in a range
                  --  of -Maxabs .. 0 for those negative values. If there are
                  --  no negative values then Lo value of result is always 0.

                  if Llo < 0 then
                     Lo := -Maxabs;
                  end if;

                  --  Case of left operand positive

                  if Lhi > 0 then
                     Hi := Maxabs;
                  end if;
               end;

            --  Subtract

            when N_Op_Subtract =>
               Lo := Llo - Rhi;
               Hi := Lhi - Rlo;

            --  Nothing else should be possible

            when others =>
               raise Program_Error;
         end case;
      end if;

      --  Here for the case where we have not rewritten anything (no bignum
      --  operands or long long integer operands), and we know the result.
      --  If we know we are in the result range, and we do not have Bignum
      --  operands or Long_Long_Integer operands, we can just reexpand with
      --  overflow checks turned off (since we know we cannot have overflow).
      --  As always the reexpansion is required to complete expansion of the
      --  operator, but we do not need to reanalyze, and we prevent recursion
      --  by suppressing the check.

      if not (Bignum_Operands or Long_Long_Integer_Operands)
        and then In_Result_Range
      then
         Set_Do_Overflow_Check (N, False);
         Reexpand (Suppress => True);
         return;

      --  Here we know that we are not in the result range, and in the general
      --  case we will move into either the Bignum or Long_Long_Integer domain
      --  to compute the result. However, there is one exception. If we are
      --  at the top level, and we do not have Bignum or Long_Long_Integer
      --  operands, we will have to immediately convert the result back to
      --  the result type, so there is no point in Bignum/Long_Long_Integer
      --  fiddling.

      elsif Top_Level
        and then not (Bignum_Operands or Long_Long_Integer_Operands)

        --  One further refinement. If we are at the top level, but our parent
        --  is a type conversion, then go into bignum or long long integer node
        --  since the result will be converted to that type directly without
        --  going through the result type, and we may avoid an overflow. This
        --  is the case for example of Long_Long_Integer (A ** 4), where A is
        --  of type Integer, and the result A ** 4 fits in Long_Long_Integer
        --  but does not fit in Integer.

        and then Nkind (Parent (N)) /= N_Type_Conversion
      then
         --  Here keep original types, but we need to complete analysis

         --  One subtlety. We can't just go ahead and do an analyze operation
         --  here because it will cause recursion into the whole MINIMIZED/
         --  ELIMINATED overflow processing which is not what we want. Here
         --  we are at the top level, and we need a check against the result
         --  mode (i.e. we want to use STRICT mode). So do exactly that.
         --  Also, we have not modified the node, so this is a case where
         --  we need to reexpand, but not reanalyze.

         Reexpand;
         return;

      --  Cases where we do the operation in Bignum mode. This happens either
      --  because one of our operands is in Bignum mode already, or because
      --  the computed bounds are outside the bounds of Long_Long_Integer,
      --  which in some cases can be indicated by Hi and Lo being No_Uint.

      --  Note: we could do better here and in some cases switch back from
      --  Bignum mode to normal mode, e.g. big mod 2 must be in the range
      --  0 .. 1, but the cases are rare and it is not worth the effort.
      --  Failing to do this switching back is only an efficiency issue.

      elsif Lo = No_Uint or else Lo < LLLo or else Hi > LLHi then

         --  OK, we are definitely outside the range of Long_Long_Integer. The
         --  question is whether to move to Bignum mode, or stay in the domain
         --  of Long_Long_Integer, signalling that an overflow check is needed.

         --  Obviously in MINIMIZED mode we stay with LLI, since we are not in
         --  the Bignum business. In ELIMINATED mode, we will normally move
         --  into Bignum mode, but there is an exception if neither of our
         --  operands is Bignum now, and we are at the top level (Top_Level
         --  set True). In this case, there is no point in moving into Bignum
         --  mode to prevent overflow if the caller will immediately convert
         --  the Bignum value back to LLI with an overflow check. It's more
         --  efficient to stay in LLI mode with an overflow check (if needed)

         if Check_Mode = Minimized
           or else (Top_Level and not Bignum_Operands)
         then
            if Do_Overflow_Check (N) then
               Enable_Overflow_Check (N);
            end if;

            --  The result now has to be in Long_Long_Integer mode, so adjust
            --  the possible range to reflect this. Note these calls also
            --  change No_Uint values from the top level case to LLI bounds.

            Max (Lo, LLLo);
            Min (Hi, LLHi);

         --  Otherwise we are in ELIMINATED mode and we switch to Bignum mode

         else
            pragma Assert (Check_Mode = Eliminated);

            declare
               Fent : Entity_Id;
               Args : List_Id;

            begin
               case Nkind (N) is
                  when N_Op_Abs      =>
                     Fent := RTE (RE_Big_Abs);

                  when N_Op_Add      =>
                     Fent := RTE (RE_Big_Add);

                  when N_Op_Divide   =>
                     Fent := RTE (RE_Big_Div);

                  when N_Op_Expon    =>
                     Fent := RTE (RE_Big_Exp);

                  when N_Op_Minus    =>
                     Fent := RTE (RE_Big_Neg);

                  when N_Op_Mod      =>
                     Fent := RTE (RE_Big_Mod);

                  when N_Op_Multiply =>
                     Fent := RTE (RE_Big_Mul);

                  when N_Op_Rem      =>
                     Fent := RTE (RE_Big_Rem);

                  when N_Op_Subtract =>
                     Fent := RTE (RE_Big_Sub);

                  --  Anything else is an internal error, this includes the
                  --  N_Op_Plus case, since how can plus cause the result
                  --  to be out of range if the operand is in range?

                  when others =>
                     raise Program_Error;
               end case;

               --  Construct argument list for Bignum call, converting our
               --  operands to Bignum form if they are not already there.

               Args := New_List;

               if Binary then
                  Append_To (Args, Convert_To_Bignum (Left_Opnd (N)));
               end if;

               Append_To (Args, Convert_To_Bignum (Right_Opnd (N)));

               --  Now rewrite the arithmetic operator with a call to the
               --  corresponding bignum function.

               Rewrite (N,
                 Make_Function_Call (Loc,
                   Name                   => New_Occurrence_Of (Fent, Loc),
                   Parameter_Associations => Args));
               Reanalyze (RTE (RE_Bignum), Suppress => True);

               --  Indicate result is Bignum mode

               Lo := No_Uint;
               Hi := No_Uint;
               return;
            end;
         end if;

      --  Otherwise we are in range of Long_Long_Integer, so no overflow
      --  check is required, at least not yet.

      else
         Set_Do_Overflow_Check (N, False);
      end if;

      --  Here we are not in Bignum territory, but we may have long long
      --  integer operands that need special handling. First a special check:
      --  If an exponentiation operator exponent is of type Long_Long_Integer,
      --  it means we converted it to prevent overflow, but exponentiation
      --  requires a Natural right operand, so convert it back to Natural.
      --  This conversion may raise an exception which is fine.

      if Nkind (N) = N_Op_Expon and then Etype (Right_Opnd (N)) = LLIB then
         Convert_To_And_Rewrite (Standard_Natural, Right_Opnd (N));
      end if;

      --  Here we will do the operation in Long_Long_Integer. We do this even
      --  if we know an overflow check is required, better to do this in long
      --  long integer mode, since we are less likely to overflow.

      --  Convert right or only operand to Long_Long_Integer, except that
      --  we do not touch the exponentiation right operand.

      if Nkind (N) /= N_Op_Expon then
         Convert_To_And_Rewrite (LLIB, Right_Opnd (N));
      end if;

      --  Convert left operand to Long_Long_Integer for binary case

      if Binary then
         Convert_To_And_Rewrite (LLIB, Left_Opnd (N));
      end if;

      --  Reset node to unanalyzed

      Set_Analyzed (N, False);
      Set_Etype (N, Empty);
      Set_Entity (N, Empty);

      --  Now analyze this new node. This reanalysis will complete processing
      --  for the node. In particular we will complete the expansion of an
      --  exponentiation operator (e.g. changing A ** 2 to A * A), and also
      --  we will complete any division checks (since we have not changed the
      --  setting of the Do_Division_Check flag).

      --  We do this reanalysis in STRICT mode to avoid recursion into the
      --  MINIMIZED/ELIMINATED handling, since we are now done with that.

      declare
         SG : constant Overflow_Mode_Type :=
                Scope_Suppress.Overflow_Mode_General;
         SA : constant Overflow_Mode_Type :=
                Scope_Suppress.Overflow_Mode_Assertions;

      begin
         Scope_Suppress.Overflow_Mode_General    := Strict;
         Scope_Suppress.Overflow_Mode_Assertions := Strict;

         if not Do_Overflow_Check (N) then
            Reanalyze (LLIB, Suppress => True);
         else
            Reanalyze (LLIB);
         end if;

         Scope_Suppress.Overflow_Mode_General    := SG;
         Scope_Suppress.Overflow_Mode_Assertions := SA;
      end;
   end Minimize_Eliminate_Overflows;

   -------------------------
   -- Overflow_Check_Mode --
   -------------------------

   function Overflow_Check_Mode return Overflow_Mode_Type is
   begin
      if In_Assertion_Expr = 0 then
         return Scope_Suppress.Overflow_Mode_General;
      else
         return Scope_Suppress.Overflow_Mode_Assertions;
      end if;
   end Overflow_Check_Mode;

   --------------------------------
   -- Overflow_Checks_Suppressed --
   --------------------------------

   function Overflow_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Overflow_Check);
      else
         return Scope_Suppress.Suppress (Overflow_Check);
      end if;
   end Overflow_Checks_Suppressed;

   ---------------------------------
   -- Predicate_Checks_Suppressed --
   ---------------------------------

   function Predicate_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Predicate_Check);
      else
         return Scope_Suppress.Suppress (Predicate_Check);
      end if;
   end Predicate_Checks_Suppressed;

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

      return Scope_Suppress.Suppress (Range_Check);
   end Range_Checks_Suppressed;

   -----------------------------------------
   -- Range_Or_Validity_Checks_Suppressed --
   -----------------------------------------

   --  Note: the coding would be simpler here if we simply made appropriate
   --  calls to Range/Validity_Checks_Suppressed, but that would result in
   --  duplicated checks which we prefer to avoid.

   function Range_Or_Validity_Checks_Suppressed
     (Expr : Node_Id) return Boolean
   is
   begin
      --  Immediate return if scope checks suppressed for either check

      if Scope_Suppress.Suppress (Range_Check)
           or
         Scope_Suppress.Suppress (Validity_Check)
      then
         return True;
      end if;

      --  If no expression, that's odd, decide that checks are suppressed,
      --  since we don't want anyone trying to do checks in this case, which
      --  is most likely the result of some other error.

      if No (Expr) then
         return True;
      end if;

      --  Expression is present, so perform suppress checks on type

      declare
         Typ : constant Entity_Id := Etype (Expr);
      begin
         if Vax_Float (Typ) then
            return True;
         elsif Checks_May_Be_Suppressed (Typ)
           and then (Is_Check_Suppressed (Typ, Range_Check)
                       or else
                     Is_Check_Suppressed (Typ, Validity_Check))
         then
            return True;
         end if;
      end;

      --  If expression is an entity name, perform checks on this entity

      if Is_Entity_Name (Expr) then
         declare
            Ent : constant Entity_Id := Entity (Expr);
         begin
            if Checks_May_Be_Suppressed (Ent) then
               return Is_Check_Suppressed (Ent, Range_Check)
                 or else Is_Check_Suppressed (Ent, Validity_Check);
            end if;
         end;
      end if;

      --  If we fall through, no checks suppressed

      return False;
   end Range_Or_Validity_Checks_Suppressed;

   -------------------
   -- Remove_Checks --
   -------------------

   procedure Remove_Checks (Expr : Node_Id) is
      function Process (N : Node_Id) return Traverse_Result;
      --  Process a single node during the traversal

      procedure Traverse is new Traverse_Proc (Process);
      --  The traversal procedure itself

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
               Traverse (Left_Opnd (N));
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
               Traverse (Left_Opnd (N));
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
      Traverse (Expr);
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

            --  For now, ignore attempt to place more than two checks ???
            --  This is really worrisome, are we really discarding checks ???

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
         SE : constant Entity_Id := Scope (E);
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

         elsif SE /= Standard_Standard
           and then Ekind (Scope (SE)) = E_Protected_Type
           and then Has_Discriminants (Scope (SE))
           and then Has_Completion (Scope (SE))
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

               Get_Index_Bounds (Indx_Type, Lo, Hi);

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

      -----------------
      -- Same_Bounds --
      -----------------

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

         --  A simple optimization for the null case

         if Known_Null (Ck_Node) then
            return Ret_Result;
         end if;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            --  The checking code to be generated will freeze the corresponding
            --  array type. However, we must freeze the type now, so that the
            --  freeze node does not appear within the generated if expression,
            --  but ahead of it.

            Freeze_Before (Ck_Node, T_Typ);

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Ck_Node);

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
                  --  At the library level, we need to ensure that the type of
                  --  the object is elaborated before the check itself is
                  --  emitted. This is only done if the object is in the
                  --  current compilation unit, otherwise the type is frozen
                  --  and elaborated in its unit.

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
                                  (Wnode, "too few elements for}??", T_Typ));

                           elsif  L_Length < R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too many elements for}??", T_Typ));
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
            --  result of applying Duplicate_Expr to the original tree, forcing
            --  the result to be a name.

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
        (Loc  : Source_Ptr;
         E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id) return Node_Id;
      --  Returns an attribute reference
      --    E'First or E'Last
      --  with a source location of Loc.
      --
      --  Nam is Name_First or Name_Last, according to which attribute is
      --  desired. If Indx is non-zero, it is passed as a literal in the
      --  Expressions of the attribute reference (identifying the desired
      --  array dimension).

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
                               Get_E_First_Or_Last (Loc, Typ, 0, Name_First))),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ),
                     Duplicate_Subexpr_No_Checks (Expr)),
                 Right_Opnd =>
                   Convert_To
                     (Base_Type (Typ),
                      Get_E_First_Or_Last (Loc, Typ, 0, Name_Last))));
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
           and then Ekind (Entity (LB)) = E_Discriminant
         then
            LB := New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
         end if;

         Left_Opnd :=
           Make_Op_Lt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr_No_Checks (LB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ),
                  Get_E_First_Or_Last (Loc, Typ, 0, Name_First)));

         if Nkind (HB) = N_Identifier
           and then Ekind (Entity (HB)) = E_Discriminant
         then
            HB := New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
         end if;

         Right_Opnd :=
           Make_Op_Gt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr_No_Checks (HB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ),
                  Get_E_First_Or_Last (Loc, Typ, 0, Name_Last)));

         return Make_Or_Else (Loc, Left_Opnd, Right_Opnd);
      end Discrete_Range_Cond;

      -------------------------
      -- Get_E_First_Or_Last --
      -------------------------

      function Get_E_First_Or_Last
        (Loc  : Source_Ptr;
         E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id) return Node_Id
      is
         Exprs : List_Id;
      begin
         if Indx > 0 then
            Exprs := New_List (Make_Integer_Literal (Loc, UI_From_Int (Indx)));
         else
            Exprs := No_List;
         end if;

         return Make_Attribute_Reference (Loc,
                  Prefix         => New_Occurrence_Of (E, Loc),
                  Attribute_Name => Nam,
                  Expressions    => Exprs);
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
                 Left_Opnd   =>
                   Get_E_First_Or_Last (Loc, Exptyp, Indx, Name_First),
                 Right_Opnd  =>
                   Get_E_First_Or_Last (Loc, Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd   =>
                   Get_E_First_Or_Last (Loc, Exptyp, Indx, Name_Last),
                 Right_Opnd  =>
                   Get_E_First_Or_Last (Loc, Typ, Indx, Name_Last)));
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
                 Left_Opnd   =>
                   Get_E_First_Or_Last (Loc, Exptyp, Indx, Name_First),
                 Right_Opnd  =>
                   Get_E_First_Or_Last (Loc, Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd   =>
                   Get_E_First_Or_Last (Loc, Exptyp, Indx, Name_Last),
                 Right_Opnd  =>
                   Get_E_First_Or_Last (Loc, Typ, Indx, Name_Last)));
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
                 Left_Opnd  =>
                   Get_N_First (Expr, Indx),
                 Right_Opnd =>
                   Get_E_First_Or_Last (Loc, Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd  =>
                   Get_N_Last (Expr, Indx),
                 Right_Opnd =>
                   Get_E_First_Or_Last (Loc, Typ, Indx, Name_Last)));
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

         --  A simple optimization for the null case

         if Known_Null (Ck_Node) then
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
            Known_T_LB : constant Boolean := Compile_Time_Known_Value (T_LB);
            Known_T_HB : constant Boolean := Compile_Time_Known_Value (T_HB);

            LB         : Node_Id := Low_Bound (Ck_Node);
            HB         : Node_Id := High_Bound (Ck_Node);
            Known_LB   : Boolean;
            Known_HB   : Boolean;

            Null_Range     : Boolean;
            Out_Of_Range_L : Boolean;
            Out_Of_Range_H : Boolean;

         begin
            --  Compute what is known at compile time

            if Known_T_LB and Known_T_HB then
               if Compile_Time_Known_Value (LB) then
                  Known_LB := True;

               --  There's no point in checking that a bound is within its
               --  own range so pretend that it is known in this case. First
               --  deal with low bound.

               elsif Ekind (Etype (LB)) = E_Signed_Integer_Subtype
                 and then Scalar_Range (Etype (LB)) = Scalar_Range (T_Typ)
               then
                  LB := T_LB;
                  Known_LB := True;

               else
                  Known_LB := False;
               end if;

               --  Likewise for the high bound

               if Compile_Time_Known_Value (HB) then
                  Known_HB := True;

               elsif Ekind (Etype (HB)) = E_Signed_Integer_Subtype
                 and then Scalar_Range (Etype (HB)) = Scalar_Range (T_Typ)
               then
                  HB := T_HB;
                  Known_HB := True;
               else
                  Known_HB := False;
               end if;
            end if;

            --  Check for case where everything is static and we can do the
            --  check at compile time. This is skipped if we have an access
            --  type, since the access value may be null.

            --  ??? This code can be improved since you only need to know that
            --  the two respective bounds (LB & T_LB or HB & T_HB) are known at
            --  compile time to emit pertinent messages.

            if Known_T_LB and Known_T_HB and Known_LB and Known_HB
              and not Do_Access
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
                              "static value out of range of}??", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                            (Wnode,
                             "static range out of bounds of}??", T_Typ));
                     end if;
                  end if;

                  if Out_Of_Range_H then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (High_Bound (Ck_Node),
                              "static value out of range of}??", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static range out of bounds of}??", T_Typ));
                     end if;
                  end if;
               end if;

            else
               declare
                  LB : Node_Id := Low_Bound (Ck_Node);
                  HB : Node_Id := High_Bound (Ck_Node);

               begin
                  --  If either bound is a discriminant and we are within the
                  --  record declaration, it is a use of the discriminant in a
                  --  constraint of a component, and nothing can be checked
                  --  here. The check will be emitted within the init proc.
                  --  Before then, the discriminal has no real meaning.
                  --  Similarly, if the entity is a discriminal, there is no
                  --  check to perform yet.

                  --  The same holds within a discriminated synchronized type,
                  --  where the discriminant may constrain a component or an
                  --  entry family.

                  if Nkind (LB) = N_Identifier
                    and then Denotes_Discriminant (LB, True)
                  then
                     if Current_Scope = Scope (Entity (LB))
                       or else Is_Concurrent_Type (Current_Scope)
                       or else Ekind (Entity (LB)) /= E_Discriminant
                     then
                        return Ret_Result;
                     else
                        LB :=
                          New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
                     end if;
                  end if;

                  if Nkind (HB) = N_Identifier
                    and then Denotes_Discriminant (HB, True)
                  then
                     if Current_Scope = Scope (Entity (HB))
                       or else Is_Concurrent_Type (Current_Scope)
                       or else Ekind (Entity (HB)) /= E_Discriminant
                     then
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
                          Left_Opnd  =>
                            Convert_To (Base_Type (Etype (HB)),
                              Duplicate_Subexpr_No_Checks (HB)),
                          Right_Opnd =>
                            Convert_To (Base_Type (Etype (LB)),
                              Duplicate_Subexpr_No_Checks (LB))),
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

         elsif Compile_Time_Known_Value (Ck_Node) and then not Do_Access then
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

                  --  Fixed or discrete type

                  else
                     Out_Of_Range :=
                       Expr_Value (Ck_Node) < Expr_Value (LB)
                         or else
                       Expr_Value (Ck_Node) > Expr_Value (UB);
                  end if;

                  --  Bounds of the type are static and the literal is out of
                  --  range so output a warning message.

                  if Out_Of_Range then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Ck_Node,
                              "static value out of range of}??", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static value out of range of}??", T_Typ));
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

               begin
                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                               or else
                             Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
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
            --  For a conversion to an unconstrained array type, generate an
            --  Action to check that the bounds of the source value are within
            --  the constraints imposed by the target type (RM 4.6(38)). No
            --  check is needed for a conversion to an access to unconstrained
            --  array type, as 4.6(24.15/2) requires the designated subtypes
            --  of the two access types to statically match.

            if Nkind (Parent (Ck_Node)) = N_Type_Conversion
              and then not Do_Access
            then
               declare
                  Opnd_Index : Node_Id;
                  Targ_Index : Node_Id;
                  Opnd_Range : Node_Id;

               begin
                  Opnd_Index := First_Index (Get_Actual_Subtype (Ck_Node));
                  Targ_Index := First_Index (T_Typ);
                  while Present (Opnd_Index) loop

                     --  If the index is a range, use its bounds. If it is an
                     --  entity (as will be the case if it is a named subtype
                     --  or an itype created for a slice) retrieve its range.

                     if Is_Entity_Name (Opnd_Index)
                       and then Is_Type (Entity (Opnd_Index))
                     then
                        Opnd_Range := Scalar_Range (Entity (Opnd_Index));
                     else
                        Opnd_Range := Opnd_Index;
                     end if;

                     if Nkind (Opnd_Range) = N_Range then
                        if  Is_In_Range
                             (Low_Bound (Opnd_Range), Etype (Targ_Index),
                              Assume_Valid => True)
                          and then
                            Is_In_Range
                             (High_Bound (Opnd_Range), Etype (Targ_Index),
                              Assume_Valid => True)
                        then
                           null;

                        --  If null range, no check needed

                        elsif
                          Compile_Time_Known_Value (High_Bound (Opnd_Range))
                            and then
                          Compile_Time_Known_Value (Low_Bound (Opnd_Range))
                            and then
                              Expr_Value (High_Bound (Opnd_Range)) <
                                  Expr_Value (Low_Bound (Opnd_Range))
                        then
                           null;

                        elsif Is_Out_Of_Range
                                (Low_Bound (Opnd_Range), Etype (Targ_Index),
                                 Assume_Valid => True)
                          or else
                              Is_Out_Of_Range
                                (High_Bound (Opnd_Range), Etype (Targ_Index),
                                 Assume_Valid => True)
                        then
                           Add_Check
                             (Compile_Time_Constraint_Error
                               (Wnode, "value out of range of}??", T_Typ));

                        else
                           Evolve_Or_Else
                             (Cond,
                              Discrete_Range_Cond
                                (Opnd_Range, Etype (Targ_Index)));
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
         return Scope_Suppress.Suppress (Storage_Check);
      end if;
   end Storage_Checks_Suppressed;

   ---------------------------
   -- Tag_Checks_Suppressed --
   ---------------------------

   function Tag_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E)
        and then Checks_May_Be_Suppressed (E)
      then
         return Is_Check_Suppressed (E, Tag_Check);
      else
         return Scope_Suppress.Suppress (Tag_Check);
      end if;
   end Tag_Checks_Suppressed;

   --------------------------
   -- Validity_Check_Range --
   --------------------------

   procedure Validity_Check_Range (N : Node_Id) is
   begin
      if Validity_Checks_On and Validity_Check_Operands then
         if Nkind (N) = N_Range then
            Ensure_Valid (Low_Bound (N));
            Ensure_Valid (High_Bound (N));
         end if;
      end if;
   end Validity_Check_Range;

   --------------------------------
   -- Validity_Checks_Suppressed --
   --------------------------------

   function Validity_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      if Present (E) and then Checks_May_Be_Suppressed (E) then
         return Is_Check_Suppressed (E, Validity_Check);
      else
         return Scope_Suppress.Suppress (Validity_Check);
      end if;
   end Validity_Checks_Suppressed;

end Checks;
