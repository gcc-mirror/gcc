------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
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

with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch11; use Exp_Ch11;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Inline;   use Inline;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Validsw;  use Validsw;

package body Exp_Prag is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 (N : Node_Id) return Node_Id;
   function Arg2 (N : Node_Id) return Node_Id;
   function Arg3 (N : Node_Id) return Node_Id;
   --  Obtain specified pragma argument expression

   procedure Expand_Pragma_Abort_Defer             (N : Node_Id);
   procedure Expand_Pragma_Check                   (N : Node_Id);
   procedure Expand_Pragma_Common_Object           (N : Node_Id);
   procedure Expand_Pragma_Import_Or_Interface     (N : Node_Id);
   procedure Expand_Pragma_Inspection_Point        (N : Node_Id);
   procedure Expand_Pragma_Interrupt_Priority      (N : Node_Id);
   procedure Expand_Pragma_Loop_Variant            (N : Node_Id);
   procedure Expand_Pragma_Psect_Object            (N : Node_Id);
   procedure Expand_Pragma_Relative_Deadline       (N : Node_Id);
   procedure Expand_Pragma_Suppress_Initialization (N : Node_Id);

   procedure Undo_Initialization (Def_Id : Entity_Id; N : Node_Id);
   --  This procedure is used to undo initialization already done for Def_Id,
   --  which is always an E_Variable, in response to the occurrence of the
   --  pragma N, a pragma Interface, Import, or Suppress_Initialization. In all
   --  these cases we want no initialization to occur, but we have already done
   --  the initialization by the time we see the pragma, so we have to undo it.

   ----------
   -- Arg1 --
   ----------

   function Arg1 (N : Node_Id) return Node_Id is
      Arg : constant Node_Id := First (Pragma_Argument_Associations (N));
   begin
      if Present (Arg)
        and then Nkind (Arg) = N_Pragma_Argument_Association
      then
         return Expression (Arg);
      else
         return Arg;
      end if;
   end Arg1;

   ----------
   -- Arg2 --
   ----------

   function Arg2 (N : Node_Id) return Node_Id is
      Arg1 : constant Node_Id := First (Pragma_Argument_Associations (N));

   begin
      if No (Arg1) then
         return Empty;

      else
         declare
            Arg : constant Node_Id := Next (Arg1);
         begin
            if Present (Arg)
              and then Nkind (Arg) = N_Pragma_Argument_Association
            then
               return Expression (Arg);
            else
               return Arg;
            end if;
         end;
      end if;
   end Arg2;

   ----------
   -- Arg3 --
   ----------

   function Arg3 (N : Node_Id) return Node_Id is
      Arg1 : constant Node_Id := First (Pragma_Argument_Associations (N));

   begin
      if No (Arg1) then
         return Empty;

      else
         declare
            Arg : Node_Id := Next (Arg1);
         begin
            if No (Arg) then
               return Empty;

            else
               Next (Arg);

               if Present (Arg)
                 and then Nkind (Arg) = N_Pragma_Argument_Association
               then
                  return Expression (Arg);
               else
                  return Arg;
               end if;
            end if;
         end;
      end if;
   end Arg3;

   ---------------------
   -- Expand_N_Pragma --
   ---------------------

   procedure Expand_N_Pragma (N : Node_Id) is
      Pname   : constant Name_Id   := Pragma_Name (N);
      Prag_Id : constant Pragma_Id := Get_Pragma_Id (Pname);

   begin
      --  Rewrite pragma ignored by Ignore_Pragma to null statement, so that
      --  the back end doesn't see it. The same goes for pragma
      --  Default_Scalar_Storage_Order if the -gnatI switch was given.

      if Should_Ignore_Pragma_Sem (N)
        or else (Prag_Id = Pragma_Default_Scalar_Storage_Order
                  and then Ignore_Rep_Clauses)
      then
         Rewrite (N, Make_Null_Statement (Sloc (N)));
         return;
      end if;

      case Prag_Id is

         --  Pragmas requiring special expander action

         when Pragma_Abort_Defer =>
            Expand_Pragma_Abort_Defer (N);

         when Pragma_Check =>
            Expand_Pragma_Check (N);

         when Pragma_Common_Object =>
            Expand_Pragma_Common_Object (N);

         when Pragma_Import =>
            Expand_Pragma_Import_Or_Interface (N);

         when Pragma_Inspection_Point =>
            Expand_Pragma_Inspection_Point (N);

         when Pragma_Interface =>
            Expand_Pragma_Import_Or_Interface (N);

         when Pragma_Interrupt_Priority =>
            Expand_Pragma_Interrupt_Priority (N);

         when Pragma_Loop_Variant =>
            Expand_Pragma_Loop_Variant (N);

         when Pragma_Psect_Object =>
            Expand_Pragma_Psect_Object (N);

         when Pragma_Relative_Deadline =>
            Expand_Pragma_Relative_Deadline (N);

         when Pragma_Suppress_Initialization =>
            Expand_Pragma_Suppress_Initialization (N);

         --  All other pragmas need no expander action (includes
         --  Unknown_Pragma).

         when others => null;
      end case;
   end Expand_N_Pragma;

   -------------------------------
   -- Expand_Pragma_Abort_Defer --
   -------------------------------

   --  An Abort_Defer pragma appears as the first statement in a handled
   --  statement sequence (right after the begin). It defers aborts for
   --  the entire statement sequence, but not for any declarations or
   --  handlers (if any) associated with this statement sequence.

   --  The transformation is to transform

   --    pragma Abort_Defer;
   --    statements;

   --  into

   --    begin
   --       Abort_Defer.all;
   --       statements
   --    exception
   --       when all others =>
   --          Abort_Undefer.all;
   --          raise;
   --    at end
   --       Abort_Undefer_Direct;
   --    end;

   procedure Expand_Pragma_Abort_Defer (N : Node_Id) is
   begin
      --  Abort_Defer has no useful effect if Abort's are not allowed

      if not Abort_Allowed then
         return;
      end if;

      --  Normal case where abort is possible

      declare
         Loc  : constant Source_Ptr := Sloc (N);
         Stm  : Node_Id;
         Stms : List_Id;
         HSS  : Node_Id;
         Blk  : constant Entity_Id :=
                  New_Internal_Entity (E_Block, Current_Scope, Sloc (N), 'B');
         AUD  : constant Entity_Id := RTE (RE_Abort_Undefer_Direct);

      begin
         Stms := New_List (Build_Runtime_Call (Loc, RE_Abort_Defer));
         loop
            Stm := Remove_Next (N);
            exit when No (Stm);
            Append (Stm, Stms);
         end loop;

         HSS :=
           Make_Handled_Sequence_Of_Statements (Loc,
             Statements  => Stms,
             At_End_Proc => New_Occurrence_Of (AUD, Loc));

         --  Present the Abort_Undefer_Direct function to the backend so that
         --  it can inline the call to the function.

         Add_Inlined_Body (AUD, N);

         Rewrite (N,
           Make_Block_Statement (Loc, Handled_Statement_Sequence => HSS));

         Set_Scope (Blk, Current_Scope);
         Set_Etype (Blk, Standard_Void_Type);
         Set_Identifier (N, New_Occurrence_Of (Blk, Sloc (N)));
         Expand_At_End_Handler (HSS, Blk);
         Analyze (N);
      end;
   end Expand_Pragma_Abort_Defer;

   --------------------------
   -- Expand_Pragma_Check --
   --------------------------

   procedure Expand_Pragma_Check (N : Node_Id) is
      Cond : constant Node_Id := Arg2 (N);
      Nam  : constant Name_Id := Chars (Arg1 (N));
      Msg  : Node_Id;

      Loc : constant Source_Ptr := Sloc (First_Node (Cond));
      --  Source location used in the case of a failed assertion: point to the
      --  failing condition, not Loc. Note that the source location of the
      --  expression is not usually the best choice here, because it points to
      --  the location of the topmost tree node, which may be an operator in
      --  the middle of the source text of the expression. For example, it gets
      --  located on the last AND keyword in a chain of boolean expressiond
      --  AND'ed together. It is best to put the message on the first character
      --  of the condition, which is the effect of the First_Node call here.
      --  This source location is used to build the default exception message,
      --  and also as the sloc of the call to the runtime subprogram raising
      --  Assert_Failure, so that coverage analysis tools can relate the
      --  call to the failed check.

      procedure Replace_Discriminals_Of_Protected_Op (Expr : Node_Id);
      --  Discriminants of the enclosing protected object may be referenced
      --  in the expression of a precondition of a protected operation.
      --  In the body of the operation these references must be replaced by
      --  the discriminal created for them, which area renamings of the
      --  discriminants of the object that is the target of the operation.
      --  This replacement is done by visibility when the references appear
      --  in the subprogram body, but in the case of a condition which appears
      --  on the specification of the subprogram it has be done separately
      --  because the condition has been replaced by a Check pragma and
      --  analyzed earlier, before the creation of the discriminal renaming
      --  declarations that are added to the subprogram body.

      ------------------------------------------
      -- Replace_Discriminals_Of_Protected_Op --
      ------------------------------------------

      procedure Replace_Discriminals_Of_Protected_Op (Expr : Node_Id) is
         function Find_Corresponding_Discriminal (E : Entity_Id)
           return Entity_Id;
         --  Find the local entity that renames a discriminant of the
         --  enclosing protected type, and has a matching name.

         ------------------------------------
         -- find_Corresponding_Discriminal --
         ------------------------------------

         function Find_Corresponding_Discriminal (E : Entity_Id)
           return Entity_Id
         is
            R : Entity_Id;

         begin
            R := First_Entity (Current_Scope);

            while Present (R) loop
               if Nkind (Parent (R)) = N_Object_Renaming_Declaration
                 and then Present (Discriminal_Link (R))
                 and then Chars (Discriminal_Link (R)) = Chars (E)
               then
                  return R;
               end if;

               Next_Entity (R);
            end loop;

            return Empty;
         end Find_Corresponding_Discriminal;

         function  Replace_Discr_Ref (N : Node_Id) return Traverse_Result;
         --  Replace a reference to a discriminant of the original protected
         --  type by the local renaming declaration of the discriminant of
         --  the target object.

         -----------------------
         -- Replace_Discr_Ref --
         -----------------------

         function  Replace_Discr_Ref (N : Node_Id) return Traverse_Result is
            R : Entity_Id;

         begin
            if Is_Entity_Name (N)
               and then Present (Discriminal_Link (Entity (N)))
            then
               R := Find_Corresponding_Discriminal (Entity (N));
               Rewrite (N, New_Occurrence_Of (R, Sloc (N)));
            end if;
            return OK;
         end Replace_Discr_Ref;

         procedure Replace_Discriminant_References is
           new Traverse_Proc (Replace_Discr_Ref);

      begin
         Replace_Discriminant_References (Expr);
      end Replace_Discriminals_Of_Protected_Op;

   begin
      --  Nothing to do if pragma is ignored

      if Is_Ignored (N) then
         return;
      end if;

      --  Since this check is active, rewrite the pragma into a corresponding
      --  if statement, and then analyze the statement.

      --  The normal case expansion transforms:

      --    pragma Check (name, condition [,message]);

      --  into

      --    if not condition then
      --       System.Assertions.Raise_Assert_Failure (Str);
      --    end if;

      --  where Str is the message if one is present, or the default of
      --  name failed at file:line if no message is given (the "name failed
      --  at" is omitted for name = Assertion, since it is redundant, given
      --  that the name of the exception is Assert_Failure.)

      --  Also, instead of "XXX failed at", we generate slightly
      --  different messages for some of the contract assertions (see
      --  code below for details).

      --  An alternative expansion is used when the No_Exception_Propagation
      --  restriction is active and there is a local Assert_Failure handler.
      --  This is not a common combination of circumstances, but it occurs in
      --  the context of Aunit and the zero footprint profile. In this case we
      --  generate:

      --    if not condition then
      --       raise Assert_Failure;
      --    end if;

      --  This will then be transformed into a goto, and the local handler will
      --  be able to handle the assert error (which would not be the case if a
      --  call is made to the Raise_Assert_Failure procedure).

      --  We also generate the direct raise if the Suppress_Exception_Locations
      --  is active, since we don't want to generate messages in this case.

      --  Note that the reason we do not always generate a direct raise is that
      --  the form in which the procedure is called allows for more efficient
      --  breakpointing of assertion errors.

      --  Generate the appropriate if statement. Note that we consider this to
      --  be an explicit conditional in the source, not an implicit if, so we
      --  do not call Make_Implicit_If_Statement.

      --  Case where we generate a direct raise

      if ((Debug_Flag_Dot_G
            or else Restriction_Active (No_Exception_Propagation))
           and then Present (Find_Local_Handler (RTE (RE_Assert_Failure), N)))
        or else (Opt.Exception_Locations_Suppressed and then No (Arg3 (N)))
      then
         Rewrite (N,
           Make_If_Statement (Loc,
             Condition       => Make_Op_Not (Loc, Right_Opnd => Cond),
             Then_Statements => New_List (
               Make_Raise_Statement (Loc,
                 Name => New_Occurrence_Of (RTE (RE_Assert_Failure), Loc)))));

      --  Case where we call the procedure

      else
         --  If we have a message given, use it

         if Present (Arg3 (N)) then
            Msg := Get_Pragma_Arg (Arg3 (N));

         --  Here we have no string, so prepare one

         else
            declare
               Loc_Str : constant String := Build_Location_String (Loc);

            begin
               Name_Len := 0;

               --  For Assert, we just use the location

               if Nam = Name_Assert then
                  null;

               --  For predicate, we generate the string "predicate failed at
               --  yyy". We prefer all lower case for predicate.

               elsif Nam = Name_Predicate then
                  Add_Str_To_Name_Buffer ("predicate failed at ");

               --  For special case of Precondition/Postcondition the string is
               --  "failed xx from yy" where xx is precondition/postcondition
               --  in all lower case. The reason for this different wording is
               --  that the failure is not at the point of occurrence of the
               --  pragma, unlike the other Check cases.

               elsif Nam_In (Nam, Name_Precondition, Name_Postcondition) then
                  Get_Name_String (Nam);
                  Insert_Str_In_Name_Buffer ("failed ", 1);
                  Add_Str_To_Name_Buffer (" from ");

               --  For special case of Invariant, the string is "failed
               --  invariant from yy", to be consistent with the string that is
               --  generated for the aspect case (the code later on checks for
               --  this specific string to modify it in some cases, so this is
               --  functionally important).

               elsif Nam = Name_Invariant then
                  Add_Str_To_Name_Buffer ("failed invariant from ");

               --  For all other checks, the string is "xxx failed at yyy"
               --  where xxx is the check name with appropriate casing.

               else
                  Get_Name_String (Nam);
                  Set_Casing
                    (Identifier_Casing (Source_Index (Current_Sem_Unit)));
                  Add_Str_To_Name_Buffer (" failed at ");
               end if;

               --  In all cases, add location string

               Add_Str_To_Name_Buffer (Loc_Str);

               --  Build the message

               Msg := Make_String_Literal (Loc, Name_Buffer (1 .. Name_Len));
            end;
         end if;

         --  For a precondition, replace references to discriminants of a
         --  protected type with the local discriminals.

         if Is_Protected_Type (Scope (Current_Scope))
           and then Has_Discriminants (Scope (Current_Scope))
           and then From_Aspect_Specification (N)
         then
            Replace_Discriminals_Of_Protected_Op (Cond);
         end if;

         --  Now rewrite as an if statement

         Rewrite (N,
           Make_If_Statement (Loc,
             Condition       => Make_Op_Not (Loc, Right_Opnd => Cond),
             Then_Statements => New_List (
               Make_Procedure_Call_Statement (Loc,
                 Name                   =>
                   New_Occurrence_Of (RTE (RE_Raise_Assert_Failure), Loc),
                 Parameter_Associations => New_List (Relocate_Node (Msg))))));
      end if;

      Analyze (N);

      --  If new condition is always false, give a warning

      if Warn_On_Assertion_Failure
        and then Nkind (N) = N_Procedure_Call_Statement
        and then Is_RTE (Entity (Name (N)), RE_Raise_Assert_Failure)
      then
         --  If original condition was a Standard.False, we assume that this is
         --  indeed intended to raise assert error and no warning is required.

         if Is_Entity_Name (Original_Node (Cond))
           and then Entity (Original_Node (Cond)) = Standard_False
         then
            null;

         elsif Nam = Name_Assert then
            Error_Msg_N ("?A?assertion will fail at run time", N);
         else
            Error_Msg_N ("?A?check will fail at run time", N);
         end if;
      end if;
   end Expand_Pragma_Check;

   ---------------------------------
   -- Expand_Pragma_Common_Object --
   ---------------------------------

   --  Use a machine attribute to replicate semantic effect in DEC Ada

   --    pragma Machine_Attribute (intern_name, "common_object", extern_name);

   --  For now we do nothing with the size attribute ???

   --  Note: Psect_Object shares this processing

   procedure Expand_Pragma_Common_Object (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Internal : constant Node_Id := Arg1 (N);
      External : constant Node_Id := Arg2 (N);

      Psect : Node_Id;
      --  Psect value upper cased as string literal

      Iloc : constant Source_Ptr := Sloc (Internal);
      Eloc : constant Source_Ptr := Sloc (External);
      Ploc : Source_Ptr;

   begin
      --  Acquire Psect value and fold to upper case

      if Present (External) then
         if Nkind (External) = N_String_Literal then
            String_To_Name_Buffer (Strval (External));
         else
            Get_Name_String (Chars (External));
         end if;

         Set_All_Upper_Case;

         Psect :=
           Make_String_Literal (Eloc, Strval => String_From_Name_Buffer);

      else
         Get_Name_String (Chars (Internal));
         Set_All_Upper_Case;
         Psect :=
           Make_String_Literal (Iloc, Strval => String_From_Name_Buffer);
      end if;

      Ploc := Sloc (Psect);

      --  Insert the pragma

      Insert_After_And_Analyze (N,
        Make_Pragma (Loc,
          Chars                        => Name_Machine_Attribute,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Iloc,
              Expression => New_Copy_Tree (Internal)),
            Make_Pragma_Argument_Association (Eloc,
              Expression =>
                Make_String_Literal (Sloc => Ploc, Strval => "common_object")),
            Make_Pragma_Argument_Association (Ploc,
              Expression => New_Copy_Tree (Psect)))));
   end Expand_Pragma_Common_Object;

   ----------------------------------
   -- Expand_Pragma_Contract_Cases --
   ----------------------------------

   --  Pragma Contract_Cases is expanded in the following manner:

   --    subprogram S is
   --       Count    : Natural := 0;
   --       Flag_1   : Boolean := False;
   --       . . .
   --       Flag_N   : Boolean := False;
   --       Flag_N+1 : Boolean := False;  --  when "others" present
   --       Pref_1   : ...;
   --       . . .
   --       Pref_M   : ...;

   --       <preconditions (if any)>

   --       --  Evaluate all case guards

   --       if Case_Guard_1 then
   --          Flag_1 := True;
   --          Count  := Count + 1;
   --       end if;
   --       . . .
   --       if Case_Guard_N then
   --          Flag_N := True;
   --          Count  := Count + 1;
   --       end if;

   --       --  Emit errors depending on the number of case guards that
   --       --  evaluated to True.

   --       if Count = 0 then
   --          raise Assertion_Error with "xxx contract cases incomplete";
   --            <or>
   --          Flag_N+1 := True;  --  when "others" present

   --       elsif Count > 1 then
   --          declare
   --             Str0 : constant String :=
   --                      "contract cases overlap for subprogram ABC";
   --             Str1 : constant String :=
   --                      (if Flag_1 then
   --                         Str0 & "case guard at xxx evaluates to True"
   --                       else Str0);
   --             StrN : constant String :=
   --                      (if Flag_N then
   --                         StrN-1 & "case guard at xxx evaluates to True"
   --                       else StrN-1);
   --          begin
   --             raise Assertion_Error with StrN;
   --          end;
   --       end if;

   --       --  Evaluate all attribute 'Old prefixes found in the selected
   --       --  consequence.

   --       if Flag_1 then
   --          Pref_1 := <prefix of 'Old found in Consequence_1>
   --       . . .
   --       elsif Flag_N then
   --          Pref_M := <prefix of 'Old found in Consequence_N>
   --       end if;

   --       procedure _Postconditions is
   --       begin
   --          <postconditions (if any)>

   --          if Flag_1 and then not Consequence_1 then
   --             raise Assertion_Error with "failed contract case at xxx";
   --          end if;
   --          . . .
   --          if Flag_N[+1] and then not Consequence_N[+1] then
   --             raise Assertion_Error with "failed contract case at xxx";
   --          end if;
   --       end _Postconditions;
   --    begin
   --       . . .
   --    end S;

   procedure Expand_Pragma_Contract_Cases
     (CCs     : Node_Id;
      Subp_Id : Entity_Id;
      Decls   : List_Id;
      Stmts   : in out List_Id)
   is
      Loc : constant Source_Ptr := Sloc (CCs);

      procedure Case_Guard_Error
        (Decls     : List_Id;
         Flag      : Entity_Id;
         Error_Loc : Source_Ptr;
         Msg       : in out Entity_Id);
      --  Given a declarative list Decls, status flag Flag, the location of the
      --  error and a string Msg, construct the following check:
      --    Msg : constant String :=
      --            (if Flag then
      --                Msg & "case guard at Error_Loc evaluates to True"
      --             else Msg);
      --  The resulting code is added to Decls

      procedure Consequence_Error
        (Checks : in out Node_Id;
         Flag   : Entity_Id;
         Conseq : Node_Id);
      --  Given an if statement Checks, status flag Flag and a consequence
      --  Conseq, construct the following check:
      --    [els]if Flag and then not Conseq then
      --       raise Assertion_Error
      --         with "failed contract case at Sloc (Conseq)";
      --    [end if;]
      --  The resulting code is added to Checks

      function Declaration_Of (Id : Entity_Id) return Node_Id;
      --  Given the entity Id of a boolean flag, generate:
      --    Id : Boolean := False;

      procedure Expand_Attributes_In_Consequence
        (Decls  : List_Id;
         Evals  : in out Node_Id;
         Flag   : Entity_Id;
         Conseq : Node_Id);
      --  Perform specialized expansion of all attribute 'Old references found
      --  in consequence Conseq such that at runtime only prefixes coming from
      --  the selected consequence are evaluated. Similarly expand attribute
      --  'Result references by replacing them with identifier _result which
      --  resolves to the sole formal parameter of procedure _Postconditions.
      --  Any temporaries generated in the process are added to declarations
      --  Decls. Evals is a complex if statement tasked with the evaluation of
      --  all prefixes coming from a single selected consequence. Flag is the
      --  corresponding case guard flag. Conseq is the consequence expression.

      function Increment (Id : Entity_Id) return Node_Id;
      --  Given the entity Id of a numerical variable, generate:
      --    Id := Id + 1;

      function Set (Id : Entity_Id) return Node_Id;
      --  Given the entity Id of a boolean variable, generate:
      --    Id := True;

      ----------------------
      -- Case_Guard_Error --
      ----------------------

      procedure Case_Guard_Error
        (Decls     : List_Id;
         Flag      : Entity_Id;
         Error_Loc : Source_Ptr;
         Msg       : in out Entity_Id)
      is
         New_Line : constant Character := Character'Val (10);
         New_Msg  : constant Entity_Id := Make_Temporary (Loc, 'S');

      begin
         Start_String;
         Store_String_Char  (New_Line);
         Store_String_Chars ("  case guard at ");
         Store_String_Chars (Build_Location_String (Error_Loc));
         Store_String_Chars (" evaluates to True");

         --  Generate:
         --    New_Msg : constant String :=
         --      (if Flag then
         --          Msg & "case guard at Error_Loc evaluates to True"
         --       else Msg);

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => New_Msg,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
             Expression          =>
               Make_If_Expression (Loc,
                 Expressions => New_List (
                   New_Occurrence_Of (Flag, Loc),

                   Make_Op_Concat (Loc,
                     Left_Opnd  => New_Occurrence_Of (Msg, Loc),
                     Right_Opnd => Make_String_Literal (Loc, End_String)),

                   New_Occurrence_Of (Msg, Loc)))));

         Msg := New_Msg;
      end Case_Guard_Error;

      -----------------------
      -- Consequence_Error --
      -----------------------

      procedure Consequence_Error
        (Checks : in out Node_Id;
         Flag   : Entity_Id;
         Conseq : Node_Id)
      is
         Cond  : Node_Id;
         Error : Node_Id;

      begin
         --  Generate:
         --    Flag and then not Conseq

         Cond :=
           Make_And_Then (Loc,
             Left_Opnd  => New_Occurrence_Of (Flag, Loc),
             Right_Opnd =>
               Make_Op_Not (Loc,
                 Right_Opnd => Relocate_Node (Conseq)));

         --  Generate:
         --    raise Assertion_Error
         --      with "failed contract case at Sloc (Conseq)";

         Start_String;
         Store_String_Chars ("failed contract case at ");
         Store_String_Chars (Build_Location_String (Sloc (Conseq)));

         Error :=
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Raise_Assert_Failure), Loc),
             Parameter_Associations => New_List (
               Make_String_Literal (Loc, End_String)));

         if No (Checks) then
            Checks :=
              Make_Implicit_If_Statement (CCs,
                Condition       => Cond,
                Then_Statements => New_List (Error));

         else
            if No (Elsif_Parts (Checks)) then
               Set_Elsif_Parts (Checks, New_List);
            end if;

            Append_To (Elsif_Parts (Checks),
              Make_Elsif_Part (Loc,
                Condition       => Cond,
                Then_Statements => New_List (Error)));
         end if;
      end Consequence_Error;

      --------------------
      -- Declaration_Of --
      --------------------

      function Declaration_Of (Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Object_Declaration (Loc,
             Defining_Identifier => Id,
             Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
             Expression          => New_Occurrence_Of (Standard_False, Loc));
      end Declaration_Of;

      --------------------------------------
      -- Expand_Attributes_In_Consequence --
      --------------------------------------

      procedure Expand_Attributes_In_Consequence
        (Decls  : List_Id;
         Evals  : in out Node_Id;
         Flag   : Entity_Id;
         Conseq : Node_Id)
      is
         Eval_Stmts : List_Id := No_List;
         --  The evaluation sequence expressed as assignment statements of all
         --  prefixes of attribute 'Old found in the current consequence.

         function Expand_Attributes (N : Node_Id) return Traverse_Result;
         --  Determine whether an arbitrary node denotes attribute 'Old or
         --  'Result and if it does, perform all expansion-related actions.

         -----------------------
         -- Expand_Attributes --
         -----------------------

         function Expand_Attributes (N : Node_Id) return Traverse_Result is
            Decl : Node_Id;
            Pref : Node_Id;
            Temp : Entity_Id;

         begin
            --  Attribute 'Old

            if Nkind (N) = N_Attribute_Reference
              and then Attribute_Name (N) = Name_Old
            then
               Pref := Prefix (N);
               Temp := Make_Temporary (Loc, 'T', Pref);
               Set_Etype (Temp, Etype (Pref));

               --  Generate a temporary to capture the value of the prefix:
               --    Temp : <Pref type>;

               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   =>
                     New_Occurrence_Of (Etype (Pref), Loc));

               --  Place that temporary at the beginning of declarations, to
               --  prevent anomalies in the GNATprove flow-analysis pass in
               --  the precondition procedure that follows.

               Prepend_To (Decls, Decl);

               --  If the type is unconstrained, the prefix provides its
               --  value and constraint, so add it to declaration.

               if not Is_Constrained (Etype (Pref))
                 and then Is_Entity_Name (Pref)
               then
                  Set_Expression (Decl, Pref);
                  Analyze (Decl);

               --  Otherwise add an assignment statement to temporary using
               --  prefix as RHS.

               else
                  Analyze (Decl);

                  if No (Eval_Stmts) then
                     Eval_Stmts := New_List;
                  end if;

                  Append_To (Eval_Stmts,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Temp, Loc),
                      Expression => Pref));

               end if;

               --  Ensure that the prefix is valid

               if Validity_Checks_On and then Validity_Check_Operands then
                  Ensure_Valid (Pref);
               end if;

               --  Replace the original attribute 'Old by a reference to the
               --  generated temporary.

               Rewrite (N, New_Occurrence_Of (Temp, Loc));

            --  Attribute 'Result

            elsif Is_Attribute_Result (N) then
               Rewrite (N, Make_Identifier (Loc, Name_uResult));
            end if;

            return OK;
         end Expand_Attributes;

         procedure Expand_Attributes_In is
           new Traverse_Proc (Expand_Attributes);

      --  Start of processing for Expand_Attributes_In_Consequence

      begin
         --  Inspect the consequence and expand any attribute 'Old and 'Result
         --  references found within.

         Expand_Attributes_In (Conseq);

         --  The consequence does not contain any attribute 'Old references

         if No (Eval_Stmts) then
            return;
         end if;

         --  Augment the machinery to trigger the evaluation of all prefixes
         --  found in the step above. If Eval is empty, then this is the first
         --  consequence to yield expansion of 'Old. Generate:

         --    if Flag then
         --       <evaluation statements>
         --    end if;

         if No (Evals) then
            Evals :=
              Make_Implicit_If_Statement (CCs,
                Condition       => New_Occurrence_Of (Flag, Loc),
                Then_Statements => Eval_Stmts);

         --  Otherwise generate:
         --    elsif Flag then
         --       <evaluation statements>
         --    end if;

         else
            if No (Elsif_Parts (Evals)) then
               Set_Elsif_Parts (Evals, New_List);
            end if;

            Append_To (Elsif_Parts (Evals),
              Make_Elsif_Part (Loc,
                Condition       => New_Occurrence_Of (Flag, Loc),
                Then_Statements => Eval_Stmts));
         end if;
      end Expand_Attributes_In_Consequence;

      ---------------
      -- Increment --
      ---------------

      function Increment (Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Id, Loc),
             Expression =>
               Make_Op_Add (Loc,
                 Left_Opnd  => New_Occurrence_Of (Id, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, 1)));
      end Increment;

      ---------
      -- Set --
      ---------

      function Set (Id : Entity_Id) return Node_Id is
      begin
         return
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Id, Loc),
             Expression => New_Occurrence_Of (Standard_True, Loc));
      end Set;

      --  Local variables

      Aggr : constant Node_Id :=
               Expression (First (Pragma_Argument_Associations (CCs)));

      Case_Guard    : Node_Id;
      CG_Checks     : Node_Id;
      CG_Stmts      : List_Id;
      Conseq        : Node_Id;
      Conseq_Checks : Node_Id   := Empty;
      Count         : Entity_Id;
      Count_Decl    : Node_Id;
      Error_Decls   : List_Id;
      Flag          : Entity_Id;
      Flag_Decl     : Node_Id;
      If_Stmt       : Node_Id;
      Msg_Str       : Entity_Id := Empty;
      Multiple_PCs  : Boolean;
      Old_Evals     : Node_Id   := Empty;
      Others_Decl   : Node_Id;
      Others_Flag   : Entity_Id := Empty;
      Post_Case     : Node_Id;

   --  Start of processing for Expand_Pragma_Contract_Cases

   begin
      --  Do nothing if pragma is not enabled. If pragma is disabled, it has
      --  already been rewritten as a Null statement.

      if Is_Ignored (CCs) then
         return;

      --  Guard against malformed contract cases

      elsif Nkind (Aggr) /= N_Aggregate then
         return;
      end if;

      --  The expansion of contract cases is quite distributed as it produces
      --  various statements to evaluate the case guards and consequences. To
      --  preserve the original context, set the Is_Assertion_Expr flag. This
      --  aids the Ghost legality checks when verifying the placement of a
      --  reference to a Ghost entity.

      In_Assertion_Expr := In_Assertion_Expr + 1;

      Multiple_PCs := List_Length (Component_Associations (Aggr)) > 1;

      --  Create the counter which tracks the number of case guards that
      --  evaluate to True.

      --    Count : Natural := 0;

      Count := Make_Temporary (Loc, 'C');
      Count_Decl :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Count,
          Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc),
          Expression          => Make_Integer_Literal (Loc, 0));

      Prepend_To (Decls, Count_Decl);
      Analyze (Count_Decl);

      --  Create the base error message for multiple overlapping case guards

      --    Msg_Str : constant String :=
      --                "contract cases overlap for subprogram Subp_Id";

      if Multiple_PCs then
         Msg_Str := Make_Temporary (Loc, 'S');

         Start_String;
         Store_String_Chars ("contract cases overlap for subprogram ");
         Store_String_Chars (Get_Name_String (Chars (Subp_Id)));

         Error_Decls := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => Msg_Str,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
             Expression          => Make_String_Literal (Loc, End_String)));
      end if;

      --  Process individual post cases

      Post_Case := First (Component_Associations (Aggr));
      while Present (Post_Case) loop
         Case_Guard := First (Choices (Post_Case));
         Conseq     := Expression (Post_Case);

         --  The "others" choice requires special processing

         if Nkind (Case_Guard) = N_Others_Choice then
            Others_Flag := Make_Temporary (Loc, 'F');
            Others_Decl := Declaration_Of (Others_Flag);

            Prepend_To (Decls, Others_Decl);
            Analyze (Others_Decl);

            --  Check possible overlap between a case guard and "others"

            if Multiple_PCs and Exception_Extra_Info then
               Case_Guard_Error
                 (Decls     => Error_Decls,
                  Flag      => Others_Flag,
                  Error_Loc => Sloc (Case_Guard),
                  Msg       => Msg_Str);
            end if;

            --  Inspect the consequence and perform special expansion of any
            --  attribute 'Old and 'Result references found within.

            Expand_Attributes_In_Consequence
              (Decls  => Decls,
               Evals  => Old_Evals,
               Flag   => Others_Flag,
               Conseq => Conseq);

            --  Check the corresponding consequence of "others"

            Consequence_Error
              (Checks => Conseq_Checks,
               Flag   => Others_Flag,
               Conseq => Conseq);

         --  Regular post case

         else
            --  Create the flag which tracks the state of its associated case
            --  guard.

            Flag := Make_Temporary (Loc, 'F');
            Flag_Decl := Declaration_Of (Flag);

            Prepend_To (Decls, Flag_Decl);
            Analyze (Flag_Decl);

            --  The flag is set when the case guard is evaluated to True
            --    if Case_Guard then
            --       Flag  := True;
            --       Count := Count + 1;
            --    end if;

            If_Stmt :=
              Make_Implicit_If_Statement (CCs,
                Condition       => Relocate_Node (Case_Guard),
                Then_Statements => New_List (
                  Set (Flag),
                  Increment (Count)));

            Append_To (Decls, If_Stmt);
            Analyze (If_Stmt);

            --  Check whether this case guard overlaps with another one

            if Multiple_PCs and Exception_Extra_Info then
               Case_Guard_Error
                 (Decls     => Error_Decls,
                  Flag      => Flag,
                  Error_Loc => Sloc (Case_Guard),
                  Msg       => Msg_Str);
            end if;

            --  Inspect the consequence and perform special expansion of any
            --  attribute 'Old and 'Result references found within.

            Expand_Attributes_In_Consequence
              (Decls  => Decls,
               Evals  => Old_Evals,
               Flag   => Flag,
               Conseq => Conseq);

            --  The corresponding consequence of the case guard which evaluated
            --  to True must hold on exit from the subprogram.

            Consequence_Error
              (Checks => Conseq_Checks,
               Flag   => Flag,
               Conseq => Conseq);
         end if;

         Next (Post_Case);
      end loop;

      --  Raise Assertion_Error when none of the case guards evaluate to True.
      --  The only exception is when we have "others", in which case there is
      --  no error because "others" acts as a default True.

      --  Generate:
      --    Flag := True;

      if Present (Others_Flag) then
         CG_Stmts := New_List (Set (Others_Flag));

      --  Generate:
      --    raise Assertion_Error with "xxx contract cases incomplete";

      else
         Start_String;
         Store_String_Chars (Build_Location_String (Loc));
         Store_String_Chars (" contract cases incomplete");

         CG_Stmts := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_Raise_Assert_Failure), Loc),
             Parameter_Associations => New_List (
               Make_String_Literal (Loc, End_String))));
      end if;

      CG_Checks :=
        Make_Implicit_If_Statement (CCs,
          Condition       =>
            Make_Op_Eq (Loc,
              Left_Opnd  => New_Occurrence_Of (Count, Loc),
              Right_Opnd => Make_Integer_Literal (Loc, 0)),
          Then_Statements => CG_Stmts);

      --  Detect a possible failure due to several case guards evaluating to
      --  True.

      --  Generate:
      --    elsif Count > 0 then
      --       declare
      --          <Error_Decls>
      --       begin
      --          raise Assertion_Error with <Msg_Str>;
      --    end if;

      if Multiple_PCs then
         Set_Elsif_Parts (CG_Checks, New_List (
           Make_Elsif_Part (Loc,
             Condition       =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => New_Occurrence_Of (Count, Loc),
                 Right_Opnd => Make_Integer_Literal (Loc, 1)),

             Then_Statements => New_List (
               Make_Block_Statement (Loc,
                 Declarations               => Error_Decls,
                 Handled_Statement_Sequence =>
                   Make_Handled_Sequence_Of_Statements (Loc,
                     Statements => New_List (
                       Make_Procedure_Call_Statement (Loc,
                         Name                   =>
                           New_Occurrence_Of
                             (RTE (RE_Raise_Assert_Failure), Loc),
                         Parameter_Associations => New_List (
                           New_Occurrence_Of (Msg_Str, Loc))))))))));
      end if;

      Append_To (Decls, CG_Checks);
      Analyze (CG_Checks);

      --  Once all case guards are evaluated and checked, evaluate any prefixes
      --  of attribute 'Old founds in the selected consequence.

      if Present (Old_Evals) then
         Append_To (Decls, Old_Evals);
         Analyze (Old_Evals);
      end if;

      --  Raise Assertion_Error when the corresponding consequence of a case
      --  guard that evaluated to True fails.

      if No (Stmts) then
         Stmts := New_List;
      end if;

      Append_To (Stmts, Conseq_Checks);

      In_Assertion_Expr := In_Assertion_Expr - 1;
   end Expand_Pragma_Contract_Cases;

   ---------------------------------------
   -- Expand_Pragma_Import_Or_Interface --
   ---------------------------------------

   procedure Expand_Pragma_Import_Or_Interface (N : Node_Id) is
      Def_Id : Entity_Id;

   begin
      --  In Relaxed_RM_Semantics, support old Ada 83 style:
      --  pragma Import (Entity, "external name");

      if Relaxed_RM_Semantics
        and then List_Length (Pragma_Argument_Associations (N)) = 2
        and then Pragma_Name (N) = Name_Import
        and then Nkind (Arg2 (N)) = N_String_Literal
      then
         Def_Id := Entity (Arg1 (N));
      else
         Def_Id := Entity (Arg2 (N));
      end if;

      --  Variable case (we have to undo any initialization already done)

      if Ekind (Def_Id) = E_Variable then
         Undo_Initialization (Def_Id, N);

      --  Case of exception with convention C++

      elsif Ekind (Def_Id) = E_Exception
        and then Convention (Def_Id) = Convention_CPP
      then
         --  Import a C++ convention

         declare
            Loc          : constant Source_Ptr := Sloc (N);
            Rtti_Name    : constant Node_Id    := Arg3 (N);
            Dum          : constant Entity_Id  := Make_Temporary (Loc, 'D');
            Exdata       : List_Id;
            Lang_Char    : Node_Id;
            Foreign_Data : Node_Id;

         begin
            Exdata := Component_Associations (Expression (Parent (Def_Id)));

            Lang_Char := Next (First (Exdata));

            --  Change the one-character language designator to 'C'

            Rewrite (Expression (Lang_Char),
              Make_Character_Literal (Loc,
                Chars              => Name_uC,
                Char_Literal_Value => UI_From_Int (Character'Pos ('C'))));
            Analyze (Expression (Lang_Char));

            --  Change the value of Foreign_Data

            Foreign_Data := Next (Next (Next (Next (Lang_Char))));

            Insert_Actions (Def_Id, New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Dum,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Character, Loc)),

              Make_Pragma (Loc,
                Chars                        => Name_Import,
                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Loc,
                    Expression => Make_Identifier (Loc, Name_Ada)),

                  Make_Pragma_Argument_Association (Loc,
                    Expression => Make_Identifier (Loc, Chars (Dum))),

                  Make_Pragma_Argument_Association (Loc,
                    Chars      => Name_External_Name,
                    Expression => Relocate_Node (Rtti_Name))))));

            Rewrite (Expression (Foreign_Data),
              Unchecked_Convert_To (Standard_A_Char,
                Make_Attribute_Reference (Loc,
                  Prefix         => Make_Identifier (Loc, Chars (Dum)),
                  Attribute_Name => Name_Address)));
            Analyze (Expression (Foreign_Data));
         end;

      --  No special expansion required for any other case

      else
         null;
      end if;
   end Expand_Pragma_Import_Or_Interface;

   -------------------------------------
   -- Expand_Pragma_Initial_Condition --
   -------------------------------------

   procedure Expand_Pragma_Initial_Condition (Spec_Or_Body : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Spec_Or_Body);

      Check     : Node_Id;
      Expr      : Node_Id;
      Init_Cond : Node_Id;
      List      : List_Id;
      Pack_Id   : Entity_Id;

   begin
      if Nkind (Spec_Or_Body) = N_Package_Body then
         Pack_Id := Corresponding_Spec (Spec_Or_Body);

         if Present (Handled_Statement_Sequence (Spec_Or_Body)) then
            List := Statements (Handled_Statement_Sequence (Spec_Or_Body));

         --  The package body lacks statements, create an empty list

         else
            List := New_List;

            Set_Handled_Statement_Sequence (Spec_Or_Body,
              Make_Handled_Sequence_Of_Statements (Loc, Statements => List));
         end if;

      elsif Nkind (Spec_Or_Body) = N_Package_Declaration then
         Pack_Id := Defining_Entity (Spec_Or_Body);

         if Present (Visible_Declarations (Specification (Spec_Or_Body))) then
            List := Visible_Declarations (Specification (Spec_Or_Body));

         --  The package lacks visible declarations, create an empty list

         else
            List := New_List;

            Set_Visible_Declarations (Specification (Spec_Or_Body), List);
         end if;

      --  This routine should not be used on anything other than packages

      else
         raise Program_Error;
      end if;

      Init_Cond := Get_Pragma (Pack_Id, Pragma_Initial_Condition);

      --  The caller should check whether the package is subject to pragma
      --  Initial_Condition.

      pragma Assert (Present (Init_Cond));

      Expr :=
        Get_Pragma_Arg (First (Pragma_Argument_Associations (Init_Cond)));

      --  The assertion expression was found to be illegal, do not generate the
      --  runtime check as it will repeat the illegality.

      if Error_Posted (Init_Cond) or else Error_Posted (Expr) then
         return;
      end if;

      --  Generate:
      --    pragma Check (Initial_Condition, <Expr>);

      Check :=
        Make_Pragma (Loc,
          Chars                        => Name_Check,
          Pragma_Argument_Associations => New_List (
            Make_Pragma_Argument_Association (Loc,
              Expression => Make_Identifier (Loc, Name_Initial_Condition)),
            Make_Pragma_Argument_Association (Loc,
              Expression => New_Copy_Tree (Expr))));

      Append_To (List, Check);
      Analyze (Check);
   end Expand_Pragma_Initial_Condition;

   ------------------------------------
   -- Expand_Pragma_Inspection_Point --
   ------------------------------------

   --  If no argument is given, then we supply a default argument list that
   --  includes all objects declared at the source level in all subprograms
   --  that enclose the inspection point pragma.

   procedure Expand_Pragma_Inspection_Point (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      A     : List_Id;
      Assoc : Node_Id;
      S     : Entity_Id;
      E     : Entity_Id;

   begin
      if No (Pragma_Argument_Associations (N)) then
         A := New_List;
         S := Current_Scope;

         while S /= Standard_Standard loop
            E := First_Entity (S);
            while Present (E) loop
               if Comes_From_Source (E)
                 and then Is_Object (E)
                 and then not Is_Entry_Formal (E)
                 and then Ekind (E) /= E_Component
                 and then Ekind (E) /= E_Discriminant
                 and then Ekind (E) /= E_Generic_In_Parameter
                 and then Ekind (E) /= E_Generic_In_Out_Parameter
               then
                  Append_To (A,
                    Make_Pragma_Argument_Association (Loc,
                      Expression => New_Occurrence_Of (E, Loc)));
               end if;

               Next_Entity (E);
            end loop;

            S := Scope (S);
         end loop;

         Set_Pragma_Argument_Associations (N, A);
      end if;

      --  Expand the arguments of the pragma. Expanding an entity reference
      --  is a noop, except in a protected operation, where a reference may
      --  have to be transformed into a reference to the corresponding prival.
      --  Are there other pragmas that may require this ???

      Assoc := First (Pragma_Argument_Associations (N));
      while Present (Assoc) loop
         Expand (Expression (Assoc));
         Next (Assoc);
      end loop;
   end Expand_Pragma_Inspection_Point;

   --------------------------------------
   -- Expand_Pragma_Interrupt_Priority --
   --------------------------------------

   --  Supply default argument if none exists (System.Interrupt_Priority'Last)

   procedure Expand_Pragma_Interrupt_Priority (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
   begin
      if No (Pragma_Argument_Associations (N)) then
         Set_Pragma_Argument_Associations (N, New_List (
           Make_Pragma_Argument_Association (Loc,
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix         =>
                   New_Occurrence_Of (RTE (RE_Interrupt_Priority), Loc),
                 Attribute_Name => Name_Last))));
      end if;
   end Expand_Pragma_Interrupt_Priority;

   --------------------------------
   -- Expand_Pragma_Loop_Variant --
   --------------------------------

   --  Pragma Loop_Variant is expanded in the following manner:

   --  Original code

   --     for | while ... loop
   --        <preceding source statements>
   --        pragma Loop_Variant
   --                 (Increases => Incr_Expr,
   --                  Decreases => Decr_Expr);
   --        <succeeding source statements>
   --     end loop;

   --  Expanded code

   --     Curr_1 : <type of Incr_Expr>;
   --     Curr_2 : <type of Decr_Expr>;
   --     Old_1  : <type of Incr_Expr>;
   --     Old_2  : <type of Decr_Expr>;
   --     Flag   : Boolean := False;

   --     for | while ... loop
   --        <preceding source statements>

   --        if Flag then
   --           Old_1 := Curr_1;
   --           Old_2 := Curr_2;
   --        end if;

   --        Curr_1 := <Incr_Expr>;
   --        Curr_2 := <Decr_Expr>;

   --        if Flag then
   --           if Curr_1 /= Old_1 then
   --              pragma Check (Loop_Variant, Curr_1 > Old_1);
   --           else
   --              pragma Check (Loop_Variant, Curr_2 < Old_2);
   --           end if;
   --        else
   --           Flag := True;
   --        end if;

   --        <succeeding source statements>
   --     end loop;

   procedure Expand_Pragma_Loop_Variant (N : Node_Id) is
      Loc      : constant Source_Ptr := Sloc (N);
      Last_Var : constant Node_Id    :=
                   Last (Pragma_Argument_Associations (N));

      Curr_Assign : List_Id   := No_List;
      Flag_Id     : Entity_Id := Empty;
      If_Stmt     : Node_Id   := Empty;
      Old_Assign  : List_Id   := No_List;
      Loop_Scop   : Entity_Id;
      Loop_Stmt   : Node_Id;
      Variant     : Node_Id;

      procedure Process_Variant (Variant : Node_Id; Is_Last : Boolean);
      --  Process a single increasing / decreasing termination variant. Flag
      --  Is_Last should be set when processing the last variant.

      ---------------------
      -- Process_Variant --
      ---------------------

      procedure Process_Variant (Variant : Node_Id; Is_Last : Boolean) is
         function Make_Op
           (Loc      : Source_Ptr;
            Curr_Val : Node_Id;
            Old_Val  : Node_Id) return Node_Id;
         --  Generate a comparison between Curr_Val and Old_Val depending on
         --  the change mode (Increases / Decreases) of the variant.

         -------------
         -- Make_Op --
         -------------

         function Make_Op
           (Loc      : Source_Ptr;
            Curr_Val : Node_Id;
            Old_Val  : Node_Id) return Node_Id
         is
         begin
            if Chars (Variant) = Name_Increases then
               return Make_Op_Gt (Loc, Curr_Val, Old_Val);
            else pragma Assert (Chars (Variant) = Name_Decreases);
               return Make_Op_Lt (Loc, Curr_Val, Old_Val);
            end if;
         end Make_Op;

         --  Local variables

         Expr     : constant Node_Id    := Expression (Variant);
         Expr_Typ : constant Entity_Id  := Etype (Expr);
         Loc      : constant Source_Ptr := Sloc (Expr);
         Loop_Loc : constant Source_Ptr := Sloc (Loop_Stmt);
         Curr_Id  : Entity_Id;
         Old_Id   : Entity_Id;
         Prag     : Node_Id;

      --  Start of processing for Process_Variant

      begin
         --  All temporaries generated in this routine must be inserted before
         --  the related loop statement. Ensure that the proper scope is on the
         --  stack when analyzing the temporaries. Note that we also use the
         --  Sloc of the related loop.

         Push_Scope (Scope (Loop_Scop));

         --  Step 1: Create the declaration of the flag which controls the
         --  behavior of the assertion on the first iteration of the loop.

         if No (Flag_Id) then

            --  Generate:
            --    Flag : Boolean := False;

            Flag_Id := Make_Temporary (Loop_Loc, 'F');

            Insert_Action (Loop_Stmt,
              Make_Object_Declaration (Loop_Loc,
                Defining_Identifier => Flag_Id,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Boolean, Loop_Loc),
                Expression          =>
                  New_Occurrence_Of (Standard_False, Loop_Loc)));

            --  Prevent an unwanted optimization where the Current_Value of
            --  the flag eliminates the if statement which stores the variant
            --  values coming from the previous iteration.

            --     Flag : Boolean := False;
            --     loop
            --        if Flag then         --  condition rewritten to False
            --           Old_N := Curr_N;  --  and if statement eliminated
            --        end if;
            --        . . .
            --        Flag := True;
            --     end loop;

            Set_Current_Value (Flag_Id, Empty);
         end if;

         --  Step 2: Create the temporaries which store the old and current
         --  values of the associated expression.

         --  Generate:
         --    Curr : <type of Expr>;

         Curr_Id := Make_Temporary (Loc, 'C');

         Insert_Action (Loop_Stmt,
           Make_Object_Declaration (Loop_Loc,
             Defining_Identifier => Curr_Id,
             Object_Definition   => New_Occurrence_Of (Expr_Typ, Loop_Loc)));

         --  Generate:
         --    Old : <type of Expr>;

         Old_Id := Make_Temporary (Loc, 'P');

         Insert_Action (Loop_Stmt,
           Make_Object_Declaration (Loop_Loc,
             Defining_Identifier => Old_Id,
             Object_Definition   => New_Occurrence_Of (Expr_Typ, Loop_Loc)));

         --  Restore original scope after all temporaries have been analyzed

         Pop_Scope;

         --  Step 3: Store value of the expression from the previous iteration

         if No (Old_Assign) then
            Old_Assign := New_List;
         end if;

         --  Generate:
         --    Old := Curr;

         Append_To (Old_Assign,
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Old_Id, Loc),
             Expression => New_Occurrence_Of (Curr_Id, Loc)));

         --  Step 4: Store the current value of the expression

         if No (Curr_Assign) then
            Curr_Assign := New_List;
         end if;

         --  Generate:
         --    Curr := <Expr>;

         Append_To (Curr_Assign,
           Make_Assignment_Statement (Loc,
             Name       => New_Occurrence_Of (Curr_Id, Loc),
             Expression => Relocate_Node (Expr)));

         --  Step 5: Create corresponding assertion to verify change of value

         --  Generate:
         --    pragma Check (Loop_Variant, Curr <|> Old);

         Prag :=
           Make_Pragma (Loc,
             Chars                        => Name_Check,
             Pragma_Argument_Associations => New_List (
               Make_Pragma_Argument_Association (Loc,
                 Expression => Make_Identifier (Loc, Name_Loop_Variant)),
               Make_Pragma_Argument_Association (Loc,
                 Expression =>
                   Make_Op (Loc,
                     Curr_Val => New_Occurrence_Of (Curr_Id, Loc),
                     Old_Val  => New_Occurrence_Of (Old_Id, Loc)))));

         --  Generate:
         --    if Curr /= Old then
         --       <Prag>;

         if No (If_Stmt) then

            --  When there is just one termination variant, do not compare the
            --  old and current value for equality, just check the pragma.

            if Is_Last then
               If_Stmt := Prag;
            else
               If_Stmt :=
                 Make_If_Statement (Loc,
                   Condition       =>
                     Make_Op_Ne (Loc,
                       Left_Opnd  => New_Occurrence_Of (Curr_Id, Loc),
                       Right_Opnd => New_Occurrence_Of (Old_Id, Loc)),
                   Then_Statements => New_List (Prag));
            end if;

         --  Generate:
         --    else
         --       <Prag>;
         --    end if;

         elsif Is_Last then
            Set_Else_Statements (If_Stmt, New_List (Prag));

         --  Generate:
         --    elsif Curr /= Old then
         --       <Prag>;

         else
            if Elsif_Parts (If_Stmt) = No_List then
               Set_Elsif_Parts (If_Stmt, New_List);
            end if;

            Append_To (Elsif_Parts (If_Stmt),
              Make_Elsif_Part (Loc,
                Condition       =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => New_Occurrence_Of (Curr_Id, Loc),
                    Right_Opnd => New_Occurrence_Of (Old_Id, Loc)),
                Then_Statements => New_List (Prag)));
         end if;
      end Process_Variant;

   --  Start of processing for Expand_Pragma_Loop_Variant

   begin
      --  If pragma is not enabled, rewrite as Null statement. If pragma is
      --  disabled, it has already been rewritten as a Null statement.

      if Is_Ignored (N) then
         Rewrite (N, Make_Null_Statement (Loc));
         Analyze (N);
         return;
      end if;

      --  The expansion of Loop_Variant is quite distributed as it produces
      --  various statements to capture and compare the arguments. To preserve
      --  the original context, set the Is_Assertion_Expr flag. This aids the
      --  Ghost legality checks when verifying the placement of a reference to
      --  a Ghost entity.

      In_Assertion_Expr := In_Assertion_Expr + 1;

      --  Locate the enclosing loop for which this assertion applies. In the
      --  case of Ada 2012 array iteration, we might be dealing with nested
      --  loops. Only the outermost loop has an identifier.

      Loop_Stmt := N;
      while Present (Loop_Stmt) loop
         if Nkind (Loop_Stmt) = N_Loop_Statement
           and then Present (Identifier (Loop_Stmt))
         then
            exit;
         end if;

         Loop_Stmt := Parent (Loop_Stmt);
      end loop;

      Loop_Scop := Entity (Identifier (Loop_Stmt));

      --  Create the circuitry which verifies individual variants

      Variant := First (Pragma_Argument_Associations (N));
      while Present (Variant) loop
         Process_Variant (Variant, Is_Last => Variant = Last_Var);
         Next (Variant);
      end loop;

      --  Construct the segment which stores the old values of all expressions.
      --  Generate:
      --    if Flag then
      --       <Old_Assign>
      --    end if;

      Insert_Action (N,
        Make_If_Statement (Loc,
          Condition       => New_Occurrence_Of (Flag_Id, Loc),
          Then_Statements => Old_Assign));

      --  Update the values of all expressions

      Insert_Actions (N, Curr_Assign);

      --  Add the assertion circuitry to test all changes in expressions.
      --  Generate:
      --    if Flag then
      --       <If_Stmt>
      --    else
      --       Flag := True;
      --    end if;

      Insert_Action (N,
        Make_If_Statement (Loc,
          Condition       => New_Occurrence_Of (Flag_Id, Loc),
          Then_Statements => New_List (If_Stmt),
          Else_Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => New_Occurrence_Of (Flag_Id, Loc),
              Expression => New_Occurrence_Of (Standard_True, Loc)))));

      --  Note: the pragma has been completely transformed into a sequence of
      --  corresponding declarations and statements. We leave it in the tree
      --  for documentation purposes. It will be ignored by the backend.

      In_Assertion_Expr := In_Assertion_Expr - 1;
   end Expand_Pragma_Loop_Variant;

   --------------------------------
   -- Expand_Pragma_Psect_Object --
   --------------------------------

   --  Convert to Common_Object, and expand the resulting pragma

   procedure Expand_Pragma_Psect_Object (N : Node_Id)
     renames Expand_Pragma_Common_Object;

   -------------------------------------
   -- Expand_Pragma_Relative_Deadline --
   -------------------------------------

   procedure Expand_Pragma_Relative_Deadline (N : Node_Id) is
      P    : constant Node_Id    := Parent (N);
      Loc  : constant Source_Ptr := Sloc (N);

   begin
      --  Expand the pragma only in the case of the main subprogram. For tasks
      --  the expansion is done in exp_ch9. Generate a call to Set_Deadline
      --  at Clock plus the relative deadline specified in the pragma. Time
      --  values are translated into Duration to allow for non-private
      --  addition operation.

      if Nkind (P) = N_Subprogram_Body then
         Rewrite
           (N,
            Make_Procedure_Call_Statement (Loc,
              Name => New_Occurrence_Of (RTE (RE_Set_Deadline), Loc),
              Parameter_Associations => New_List (
                Unchecked_Convert_To (RTE (RO_RT_Time),
                  Make_Op_Add (Loc,
                    Left_Opnd  =>
                      Make_Function_Call (Loc,
                        New_Occurrence_Of (RTE (RO_RT_To_Duration), Loc),
                        New_List
                          (Make_Function_Call
                             (Loc, New_Occurrence_Of (RTE (RE_Clock), Loc)))),
                    Right_Opnd  =>
                      Unchecked_Convert_To (Standard_Duration, Arg1 (N)))))));

         Analyze (N);
      end if;
   end Expand_Pragma_Relative_Deadline;

   -------------------------------------------
   -- Expand_Pragma_Suppress_Initialization --
   -------------------------------------------

   procedure Expand_Pragma_Suppress_Initialization (N : Node_Id) is
      Def_Id : constant Entity_Id  := Entity (Arg1 (N));

   begin
      --  Variable case (we have to undo any initialization already done)

      if Ekind (Def_Id) = E_Variable then
         Undo_Initialization (Def_Id, N);
      end if;
   end Expand_Pragma_Suppress_Initialization;

   -------------------------
   -- Undo_Initialization --
   -------------------------

   procedure Undo_Initialization (Def_Id : Entity_Id; N : Node_Id) is
      Init_Call : Node_Id;

   begin
      --  When applied to a variable, the default initialization must not be
      --  done. As it is already done when the pragma is found, we just get rid
      --  of the call the initialization procedure which followed the object
      --  declaration. The call is inserted after the declaration, but validity
      --  checks may also have been inserted and thus the initialization call
      --  does not necessarily appear immediately after the object declaration.

      --  We can't use the freezing mechanism for this purpose, since we have
      --  to elaborate the initialization expression when it is first seen (so
      --  this elaboration cannot be deferred to the freeze point).

      --  Find and remove generated initialization call for object, if any

      Init_Call := Remove_Init_Call (Def_Id, Rep_Clause => N);

      --  Any default initialization expression should be removed (e.g.
      --  null defaults for access objects, zero initialization of packed
      --  bit arrays). Imported objects aren't allowed to have explicit
      --  initialization, so the expression must have been generated by
      --  the compiler.

      if No (Init_Call) and then Present (Expression (Parent (Def_Id))) then
         Set_Expression (Parent (Def_Id), Empty);
      end if;

      --  The object may not have any initialization, but in the presence of
      --  Initialize_Scalars code is inserted after then declaration, which
      --  must now be removed as well. The code carries the same source
      --  location as the declaration itself.

      if Initialize_Scalars and then Is_Array_Type (Etype (Def_Id)) then
         declare
            Init : Node_Id;
            Nxt  : Node_Id;
         begin
            Init := Next (Parent (Def_Id));
            while not Comes_From_Source (Init)
              and then Sloc (Init) = Sloc (Def_Id)
            loop
               Nxt := Next (Init);
               Remove (Init);
               Init := Nxt;
            end loop;
         end;
      end if;
   end Undo_Initialization;

end Exp_Prag;
