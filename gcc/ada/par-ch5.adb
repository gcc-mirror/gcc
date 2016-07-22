------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P A R . C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

pragma Style_Checks (All_Checks);
--  Turn off subprogram body ordering check. Subprograms are in order by RM
--  section rather than alphabetical.

with Sinfo.CN; use Sinfo.CN;

separate (Par)
package body Ch5 is

   --  Local functions, used only in this chapter

   function P_Case_Statement                     return Node_Id;
   function P_Case_Statement_Alternative         return Node_Id;
   function P_Exit_Statement                     return Node_Id;
   function P_Goto_Statement                     return Node_Id;
   function P_If_Statement                       return Node_Id;
   function P_Label                              return Node_Id;
   function P_Null_Statement                     return Node_Id;

   function P_Assignment_Statement (LHS : Node_Id) return Node_Id;
   --  Parse assignment statement. On entry, the caller has scanned the left
   --  hand side (passed in as Lhs), and the colon-equal (or some symbol
   --  taken to be an error equivalent such as equal).

   function P_Begin_Statement (Block_Name : Node_Id := Empty) return Node_Id;
   --  Parse begin-end statement. If Block_Name is non-Empty on entry, it is
   --  the N_Identifier node for the label on the block. If Block_Name is
   --  Empty on entry (the default), then the block statement is unlabeled.

   function P_Declare_Statement (Block_Name : Node_Id := Empty) return Node_Id;
   --  Parse declare block. If Block_Name is non-Empty on entry, it is
   --  the N_Identifier node for the label on the block. If Block_Name is
   --  Empty on entry (the default), then the block statement is unlabeled.

   function P_For_Statement (Loop_Name : Node_Id := Empty) return Node_Id;
   --  Parse for statement. If Loop_Name is non-Empty on entry, it is
   --  the N_Identifier node for the label on the loop. If Loop_Name is
   --  Empty on entry (the default), then the for statement is unlabeled.

   function P_Iterator_Specification (Def_Id : Node_Id) return Node_Id;
   --  Parse an iterator specification. The defining identifier has already
   --  been scanned, as it is the common prefix between loop and iterator
   --  specification.

   function P_Loop_Statement (Loop_Name : Node_Id := Empty) return Node_Id;
   --  Parse loop statement. If Loop_Name is non-Empty on entry, it is
   --  the N_Identifier node for the label on the loop. If Loop_Name is
   --  Empty on entry (the default), then the loop statement is unlabeled.

   function P_While_Statement (Loop_Name : Node_Id := Empty) return Node_Id;
   --  Parse while statement. If Loop_Name is non-Empty on entry, it is
   --  the N_Identifier node for the label on the loop. If Loop_Name is
   --  Empty on entry (the default), then the while statement is unlabeled.

   function Set_Loop_Block_Name (L : Character) return Name_Id;
   --  Given a letter 'L' for a loop or 'B' for a block, returns a name
   --  of the form L_nn or B_nn where nn is a serial number obtained by
   --  incrementing the variable Loop_Block_Count.

   procedure Then_Scan;
   --  Scan past THEN token, testing for illegal junk after it

   ---------------------------------
   -- 5.1  Sequence of Statements --
   ---------------------------------

   --  SEQUENCE_OF_STATEMENTS ::= STATEMENT {STATEMENT} {LABEL}
   --  Note: the final label is an Ada 2012 addition.

   --  STATEMENT ::=
   --    {LABEL} SIMPLE_STATEMENT | {LABEL} COMPOUND_STATEMENT

   --  SIMPLE_STATEMENT ::=      NULL_STATEMENT
   --  | ASSIGNMENT_STATEMENT  | EXIT_STATEMENT
   --  | GOTO_STATEMENT        | PROCEDURE_CALL_STATEMENT
   --  | RETURN_STATEMENT      | ENTRY_CALL_STATEMENT
   --  | REQUEUE_STATEMENT     | DELAY_STATEMENT
   --  | ABORT_STATEMENT       | RAISE_STATEMENT
   --  | CODE_STATEMENT

   --  COMPOUND_STATEMENT ::=
   --    IF_STATEMENT         | CASE_STATEMENT
   --  | LOOP_STATEMENT       | BLOCK_STATEMENT
   --  | ACCEPT_STATEMENT     | SELECT_STATEMENT

   --  This procedure scans a sequence of statements. The caller sets SS_Flags
   --  to indicate acceptable termination conditions for the sequence:

   --    SS_Flags.Eftm Terminate on ELSIF
   --    SS_Flags.Eltm Terminate on ELSE
   --    SS_Flags.Extm Terminate on EXCEPTION
   --    SS_Flags.Ortm Terminate on OR
   --    SS_Flags.Tatm Terminate on THEN ABORT (Token = ABORT on return)
   --    SS_Flags.Whtm Terminate on WHEN
   --    SS_Flags.Unco Unconditional terminate after scanning one statement

   --  In addition, the scan is always terminated by encountering END or the
   --  end of file (EOF) condition. If one of the six above terminators is
   --  encountered with the corresponding SS_Flags flag not set, then the
   --  action taken is as follows:

   --    If the keyword occurs to the left of the expected column of the end
   --    for the current sequence (as recorded in the current end context),
   --    then it is assumed to belong to an outer context, and is considered
   --    to terminate the sequence of statements.

   --    If the keyword occurs to the right of, or in the expected column of
   --    the end for the current sequence, then an error message is output,
   --    the keyword together with its associated context is skipped, and
   --    the statement scan continues until another terminator is found.

   --  Note that the first action means that control can return to the caller
   --  with Token set to a terminator other than one of those specified by the
   --  SS parameter. The caller should treat such a case as equivalent to END.

   --  In addition, the flag SS_Flags.Sreq is set to True to indicate that at
   --  least one real statement (other than a pragma) is required in the
   --  statement sequence. During the processing of the sequence, this
   --  flag is manipulated to indicate the current status of the requirement
   --  for a statement. For example, it is turned off by the occurrence of a
   --  statement, and back on by a label (which requires a following statement)

   --  Error recovery: cannot raise Error_Resync. If an error occurs during
   --  parsing a statement, then the scan pointer is advanced past the next
   --  semicolon and the parse continues.

   function P_Sequence_Of_Statements (SS_Flags : SS_Rec) return List_Id is

      Statement_Required : Boolean;
      --  This flag indicates if a subsequent statement (other than a pragma)
      --  is required. It is initialized from the Sreq flag, and modified as
      --  statements are scanned (a statement turns it off, and a label turns
      --  it back on again since a statement must follow a label).
      --  Note : this final requirement is lifted in Ada 2012.

      Statement_Seen : Boolean;
      --  In Ada 2012, a label can end a sequence of statements, but the
      --  sequence cannot contain only labels. This flag is set whenever a
      --  label is encountered, to enforce this rule at the end of a sequence.

      Declaration_Found : Boolean := False;
      --  This flag is set True if a declaration is encountered, so that the
      --  error message about declarations in the statement part is only
      --  given once for a given sequence of statements.

      Scan_State_Label : Saved_Scan_State;
      Scan_State       : Saved_Scan_State;

      Statement_List : List_Id;
      Block_Label    : Name_Id;
      Id_Node        : Node_Id;
      Name_Node      : Node_Id;

      procedure Junk_Declaration;
      --  Procedure called to handle error of declaration encountered in
      --  statement sequence.

      procedure Test_Statement_Required;
      --  Flag error if Statement_Required flag set

      ----------------------
      -- Junk_Declaration --
      ----------------------

      procedure Junk_Declaration is
      begin
         if (not Declaration_Found) or All_Errors_Mode then
            Error_Msg_SC -- CODEFIX
              ("declarations must come before BEGIN");
            Declaration_Found := True;
         end if;

         Skip_Declaration (Statement_List);
      end Junk_Declaration;

      -----------------------------
      -- Test_Statement_Required --
      -----------------------------

      procedure Test_Statement_Required is
         function All_Pragmas return Boolean;
         --  Return True if statement list is all pragmas

         -----------------
         -- All_Pragmas --
         -----------------

         function All_Pragmas return Boolean is
            S : Node_Id;
         begin
            S := First (Statement_List);
            while Present (S) loop
               if Nkind (S) /= N_Pragma then
                  return False;
               else
                  Next (S);
               end if;
            end loop;

            return True;
         end All_Pragmas;

      --  Start of processing for Test_Statement_Required

      begin
         if Statement_Required then

            --  Check no statement required after label in Ada 2012, and that
            --  it is OK to have nothing but pragmas in a statement sequence.

            if Ada_Version >= Ada_2012
              and then not Is_Empty_List (Statement_List)
              and then
                ((Nkind (Last (Statement_List)) = N_Label
                   and then Statement_Seen)
                or else All_Pragmas)
            then
               --  This Ada 2012 construct not allowed in a compiler unit

               Check_Compiler_Unit ("null statement list", Token_Ptr);

               declare
                  Null_Stm : constant Node_Id :=
                               Make_Null_Statement (Token_Ptr);
               begin
                  Set_Comes_From_Source (Null_Stm, False);
                  Append_To (Statement_List, Null_Stm);
               end;

            --  If not Ada 2012, or not special case above, give error message

            else
               Error_Msg_BC -- CODEFIX
                 ("statement expected");
            end if;
         end if;
      end Test_Statement_Required;

   --  Start of processing for P_Sequence_Of_Statements

   begin
      Statement_List := New_List;
      Statement_Required := SS_Flags.Sreq;
      Statement_Seen     := False;

      loop
         Ignore (Tok_Semicolon);

         begin
            if Style_Check then
               Style.Check_Indentation;
            end if;

            --  Deal with reserved identifier (in assignment or call)

            if Is_Reserved_Identifier then
               Save_Scan_State (Scan_State); -- at possible bad identifier
               Scan; -- and scan past it

               --  We have an reserved word which is spelled in identifier
               --  style, so the question is whether it really is intended
               --  to be an identifier.

               if
                  --  If followed by a semicolon, then it is an identifier,
                  --  with the exception of the cases tested for below.

                  (Token = Tok_Semicolon
                    and then Prev_Token /= Tok_Return
                    and then Prev_Token /= Tok_Null
                    and then Prev_Token /= Tok_Raise
                    and then Prev_Token /= Tok_End
                    and then Prev_Token /= Tok_Exit)

                  --  If followed by colon, colon-equal, or dot, then we
                  --  definitely  have an identifier (could not be reserved)

                  or else Token = Tok_Colon
                  or else Token = Tok_Colon_Equal
                  or else Token = Tok_Dot

                  --  Left paren means we have an identifier except for those
                  --  reserved words that can legitimately be followed by a
                  --  left paren.

                  or else
                    (Token = Tok_Left_Paren
                      and then Prev_Token /= Tok_Case
                      and then Prev_Token /= Tok_Delay
                      and then Prev_Token /= Tok_If
                      and then Prev_Token /= Tok_Elsif
                      and then Prev_Token /= Tok_Return
                      and then Prev_Token /= Tok_When
                      and then Prev_Token /= Tok_While
                      and then Prev_Token /= Tok_Separate)
               then
                  --  Here we have an apparent reserved identifier and the
                  --  token past it is appropriate to this usage (and would
                  --  be a definite error if this is not an identifier). What
                  --  we do is to use P_Identifier to fix up the identifier,
                  --  and then fall into the normal processing.

                  Restore_Scan_State (Scan_State); -- back to the ID
                  Scan_Reserved_Identifier (Force_Msg => False);

                  --  Not a reserved identifier after all (or at least we can't
                  --  be sure that it is), so reset the scan and continue.

               else
                  Restore_Scan_State (Scan_State); -- back to the reserved word
               end if;
            end if;

            --  Now look to see what kind of statement we have

            case Token is

               --  Case of end or EOF

               when Tok_End | Tok_EOF =>

                  --  These tokens always terminate the statement sequence

                  Test_Statement_Required;
                  exit;

               --  Case of ELSIF

               when Tok_Elsif =>

                  --  Terminate if Eftm set or if the ELSIF is to the left
                  --  of the expected column of the end for this sequence

                  if SS_Flags.Eftm
                     or else Start_Column < Scope.Table (Scope.Last).Ecol
                  then
                     Test_Statement_Required;
                     exit;

                  --  Otherwise complain and skip past ELSIF Condition then

                  else
                     Error_Msg_SC ("ELSIF not allowed here");
                     Scan; -- past ELSIF
                     Discard_Junk_Node (P_Expression_No_Right_Paren);
                     Then_Scan;
                     Statement_Required := False;
                  end if;

               --  Case of ELSE

               when Tok_Else =>

                  --  Terminate if Eltm set or if the else is to the left
                  --  of the expected column of the end for this sequence

                  if SS_Flags.Eltm
                     or else Start_Column < Scope.Table (Scope.Last).Ecol
                  then
                     Test_Statement_Required;
                     exit;

                  --  Otherwise complain and skip past else

                  else
                     Error_Msg_SC ("ELSE not allowed here");
                     Scan; -- past ELSE
                     Statement_Required := False;
                  end if;

               --  Case of exception

               when Tok_Exception =>
                  Test_Statement_Required;

                  --  If Extm not set and the exception is not to the left of
                  --  the expected column of the end for this sequence, then we
                  --  assume it belongs to the current sequence, even though it
                  --  is not permitted.

                  if not SS_Flags.Extm and then
                     Start_Column >= Scope.Table (Scope.Last).Ecol

                  then
                     Error_Msg_SC ("exception handler not permitted here");
                     Scan; -- past EXCEPTION
                     Discard_Junk_List (Parse_Exception_Handlers);
                  end if;

                  --  Always return, in the case where we scanned out handlers
                  --  that we did not expect, Parse_Exception_Handlers returned
                  --  with Token being either end or EOF, so we are OK.

                  exit;

               --  Case of OR

               when Tok_Or =>

                  --  Terminate if Ortm set or if the or is to the left of the
                  --  expected column of the end for this sequence.

                  if SS_Flags.Ortm
                     or else Start_Column < Scope.Table (Scope.Last).Ecol
                  then
                     Test_Statement_Required;
                     exit;

                  --  Otherwise complain and skip past or

                  else
                     Error_Msg_SC ("OR not allowed here");
                     Scan; -- past or
                     Statement_Required := False;
                  end if;

               --  Case of THEN (deal also with THEN ABORT)

               when Tok_Then =>
                  Save_Scan_State (Scan_State); -- at THEN
                  Scan; -- past THEN

                  --  Terminate if THEN ABORT allowed (ATC case)

                  exit when SS_Flags.Tatm and then Token = Tok_Abort;

                  --  Otherwise we treat THEN as some kind of mess where we did
                  --  not see the associated IF, but we pick up assuming it had
                  --  been there.

                  Restore_Scan_State (Scan_State); -- to THEN
                  Append_To (Statement_List, P_If_Statement);
                  Statement_Required := False;

               --  Case of WHEN (error because we are not in a case)

               when Tok_When | Tok_Others =>

                  --  Terminate if Whtm set or if the WHEN is to the left of
                  --  the expected column of the end for this sequence.

                  if SS_Flags.Whtm
                     or else Start_Column < Scope.Table (Scope.Last).Ecol
                  then
                     Test_Statement_Required;
                     exit;

                  --  Otherwise complain and skip when Choice {| Choice} =>

                  else
                     Error_Msg_SC ("WHEN not allowed here");
                     Scan; -- past when
                     Discard_Junk_List (P_Discrete_Choice_List);
                     TF_Arrow;
                     Statement_Required := False;
                  end if;

               --  Cases of statements starting with an identifier

               when Tok_Identifier =>
                  Check_Bad_Layout;

                  --  Save scan pointers and line number in case block label

                  Id_Node := Token_Node;
                  Block_Label := Token_Name;
                  Save_Scan_State (Scan_State_Label); -- at possible label
                  Scan; -- past Id

                  --  Check for common case of assignment, since it occurs
                  --  frequently, and we want to process it efficiently.

                  if Token = Tok_Colon_Equal then
                     Scan; -- past the colon-equal
                     Append_To (Statement_List,
                       P_Assignment_Statement (Id_Node));
                     Statement_Required := False;

                  --  Check common case of procedure call, another case that
                  --  we want to speed up as much as possible.

                  elsif Token = Tok_Semicolon then
                     Change_Name_To_Procedure_Call_Statement (Id_Node);
                     Append_To (Statement_List, Id_Node);
                     Scan; -- past semicolon
                     Statement_Required := False;

                     --  Here is the special test for a suspicious label, more
                     --  accurately a suspicious name, which we think perhaps
                     --  should have been a label. If next token is one of
                     --  LOOP, FOR, WHILE, DECLARE, BEGIN, then make an entry
                     --  in the suspicious label table.

                     if Token = Tok_Loop    or else
                        Token = Tok_For     or else
                        Token = Tok_While   or else
                        Token = Tok_Declare or else
                        Token = Tok_Begin
                     then
                        Suspicious_Labels.Append
                          ((Proc_Call     => Id_Node,
                            Semicolon_Loc => Prev_Token_Ptr,
                            Start_Token   => Token_Ptr));
                     end if;

                  --  Check for case of "go to" in place of "goto"

                  elsif Token = Tok_Identifier
                    and then Block_Label = Name_Go
                    and then Token_Name = Name_To
                  then
                     Error_Msg_SP -- CODEFIX
                       ("goto is one word");
                     Append_To (Statement_List, P_Goto_Statement);
                     Statement_Required := False;

                  --  Check common case of = used instead of :=, just so we
                  --  give a better error message for this special misuse.

                  elsif Token = Tok_Equal then
                     T_Colon_Equal; -- give := expected message
                     Append_To (Statement_List,
                       P_Assignment_Statement (Id_Node));
                     Statement_Required := False;

                  --  Check case of loop label or block label

                  elsif Token = Tok_Colon
                    or else (Token in Token_Class_Labeled_Stmt
                              and then not Token_Is_At_Start_Of_Line)
                  then
                     T_Colon; -- past colon (if there, or msg for missing one)

                     --  Test for more than one label

                     loop
                        exit when Token /= Tok_Identifier;
                        Save_Scan_State (Scan_State); -- at second Id
                        Scan; -- past Id

                        if Token = Tok_Colon then
                           Error_Msg_SP
                              ("only one label allowed on block or loop");
                           Scan; -- past colon on extra label

                           --  Use the second label as the "real" label

                           Scan_State_Label := Scan_State;

                           --  We will set Error_name as the Block_Label since
                           --  we really don't know which of the labels might
                           --  be used at the end of the loop or block.

                           Block_Label := Error_Name;

                        --  If Id with no colon, then backup to point to the
                        --  Id and we will issue the message below when we try
                        --  to scan out the statement as some other form.

                        else
                           Restore_Scan_State (Scan_State); -- to second Id
                           exit;
                        end if;
                     end loop;

                     --  Loop_Statement (labeled Loop_Statement)

                     if Token = Tok_Loop then
                        Append_To (Statement_List,
                          P_Loop_Statement (Id_Node));

                     --  While statement (labeled loop statement with WHILE)

                     elsif Token = Tok_While then
                        Append_To (Statement_List,
                          P_While_Statement (Id_Node));

                     --  Declare statement (labeled block statement with
                     --  DECLARE part)

                     elsif Token = Tok_Declare then
                        Append_To (Statement_List,
                          P_Declare_Statement (Id_Node));

                     --  Begin statement (labeled block statement with no
                     --  DECLARE part)

                     elsif Token = Tok_Begin then
                        Append_To (Statement_List,
                          P_Begin_Statement (Id_Node));

                     --  For statement (labeled loop statement with FOR)

                     elsif Token = Tok_For then
                        Append_To (Statement_List,
                          P_For_Statement (Id_Node));

                     --  Improper statement follows label. If we have an
                     --  expression token, then assume the colon was part
                     --  of a misplaced declaration.

                     elsif Token not in Token_Class_Eterm then
                        Restore_Scan_State (Scan_State_Label);
                        Junk_Declaration;

                     --  Otherwise complain we have inappropriate statement

                     else
                        Error_Msg_AP
                          ("loop or block statement must follow label");
                     end if;

                     Statement_Required := False;

                  --  Here we have an identifier followed by something
                  --  other than a colon, semicolon or assignment symbol.
                  --  The only valid possibility is a name extension symbol

                  elsif Token in Token_Class_Namext then
                     Restore_Scan_State (Scan_State_Label); -- to Id
                     Name_Node := P_Name;

                     --  Skip junk right parens in this context

                     Ignore (Tok_Right_Paren);

                     --  Check context following call

                     if Token = Tok_Colon_Equal then
                        Scan; -- past colon equal
                        Append_To (Statement_List,
                          P_Assignment_Statement (Name_Node));
                        Statement_Required := False;

                     --  Check common case of = used instead of :=

                     elsif Token = Tok_Equal then
                        T_Colon_Equal; -- give := expected message
                        Append_To (Statement_List,
                          P_Assignment_Statement (Name_Node));
                        Statement_Required := False;

                     --  Check apostrophe cases

                     elsif Token = Tok_Apostrophe then
                        Append_To (Statement_List,
                          P_Code_Statement (Name_Node));
                        Statement_Required := False;

                     --  The only other valid item after a name is ; which
                     --  means that the item we just scanned was a call.

                     elsif Token = Tok_Semicolon then
                        Change_Name_To_Procedure_Call_Statement (Name_Node);
                        Append_To (Statement_List, Name_Node);
                        Scan; -- past semicolon
                        Statement_Required := False;

                     --  A slash following an identifier or a selected
                     --  component in this situation is most likely a period
                     --  (see location of keys on keyboard).

                     elsif Token = Tok_Slash
                       and then (Nkind (Name_Node) = N_Identifier
                                   or else
                                 Nkind (Name_Node) = N_Selected_Component)
                     then
                        Error_Msg_SC -- CODEFIX
                          ("""/"" should be "".""");
                        Statement_Required := False;
                        raise Error_Resync;

                     --  Else we have a missing semicolon

                     else
                        TF_Semicolon;

                        --  Normal processing as though semicolon were present

                        Change_Name_To_Procedure_Call_Statement (Name_Node);
                        Append_To (Statement_List, Name_Node);
                        Statement_Required := False;
                     end if;

                  --  If junk after identifier, check if identifier is an
                  --  instance of an incorrectly spelled keyword. If so, we
                  --  do nothing. The Bad_Spelling_Of will have reset Token
                  --  to the appropriate keyword, so the next time round the
                  --  loop we will process the modified token. Note that we
                  --  check for ELSIF before ELSE here. That's not accidental.
                  --  We don't want to identify a misspelling of ELSE as
                  --  ELSIF, and in particular we do not want to treat ELSEIF
                  --  as ELSE IF.

                  else
                     Restore_Scan_State (Scan_State_Label); -- to identifier

                     if Bad_Spelling_Of (Tok_Abort)
                       or else Bad_Spelling_Of (Tok_Accept)
                       or else Bad_Spelling_Of (Tok_Case)
                       or else Bad_Spelling_Of (Tok_Declare)
                       or else Bad_Spelling_Of (Tok_Delay)
                       or else Bad_Spelling_Of (Tok_Elsif)
                       or else Bad_Spelling_Of (Tok_Else)
                       or else Bad_Spelling_Of (Tok_End)
                       or else Bad_Spelling_Of (Tok_Exception)
                       or else Bad_Spelling_Of (Tok_Exit)
                       or else Bad_Spelling_Of (Tok_For)
                       or else Bad_Spelling_Of (Tok_Goto)
                       or else Bad_Spelling_Of (Tok_If)
                       or else Bad_Spelling_Of (Tok_Loop)
                       or else Bad_Spelling_Of (Tok_Or)
                       or else Bad_Spelling_Of (Tok_Pragma)
                       or else Bad_Spelling_Of (Tok_Raise)
                       or else Bad_Spelling_Of (Tok_Requeue)
                       or else Bad_Spelling_Of (Tok_Return)
                       or else Bad_Spelling_Of (Tok_Select)
                       or else Bad_Spelling_Of (Tok_When)
                       or else Bad_Spelling_Of (Tok_While)
                     then
                        null;

                     --  If not a bad spelling, then we really have junk

                     else
                        Scan; -- past identifier again

                        --  If next token is first token on line, then we
                        --  consider that we were missing a semicolon after
                        --  the identifier, and process it as a procedure
                        --  call with no parameters.

                        if Token_Is_At_Start_Of_Line then
                           Change_Name_To_Procedure_Call_Statement (Id_Node);
                           Append_To (Statement_List, Id_Node);
                           T_Semicolon; -- to give error message
                           Statement_Required := False;

                        --  Otherwise we give a missing := message and
                        --  simply abandon the junk that is there now.

                        else
                           T_Colon_Equal; -- give := expected message
                           raise Error_Resync;
                        end if;

                     end if;
                  end if;

               --  Statement starting with operator symbol. This could be
               --  a call, a name starting an assignment, or a qualified
               --  expression.

               when Tok_Operator_Symbol =>
                  Check_Bad_Layout;
                  Name_Node := P_Name;

                  --  An attempt at a range attribute or a qualified expression
                  --  must be illegal here (a code statement cannot possibly
                  --  allow qualification by a function name).

                  if Token = Tok_Apostrophe then
                     Error_Msg_SC ("apostrophe illegal here");
                     raise Error_Resync;
                  end if;

                  --  Scan possible assignment if we have a name

                  if Expr_Form = EF_Name
                    and then Token = Tok_Colon_Equal
                  then
                     Scan; -- past colon equal
                     Append_To (Statement_List,
                       P_Assignment_Statement (Name_Node));
                  else
                     Change_Name_To_Procedure_Call_Statement (Name_Node);
                     Append_To (Statement_List, Name_Node);
                  end if;

                  TF_Semicolon;
                  Statement_Required := False;

               --  Label starting with << which must precede real statement
               --  Note: in Ada 2012, the label may end the sequence.

               when Tok_Less_Less =>
                  if Present (Last (Statement_List))
                    and then Nkind (Last (Statement_List)) /= N_Label
                  then
                     Statement_Seen := True;
                  end if;

                  Append_To (Statement_List, P_Label);
                  Statement_Required := True;

               --  Pragma appearing as a statement in a statement sequence

               when Tok_Pragma =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Pragma);

               --  Abort_Statement

               when Tok_Abort =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Abort_Statement);
                  Statement_Required := False;

               --  Accept_Statement

               when Tok_Accept =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Accept_Statement);
                  Statement_Required := False;

               --  Begin_Statement (Block_Statement with no declare, no label)

               when Tok_Begin =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Begin_Statement);
                  Statement_Required := False;

               --  Case_Statement

               when Tok_Case =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Case_Statement);
                  Statement_Required := False;

               --  Block_Statement with DECLARE and no label

               when Tok_Declare =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Declare_Statement);
                  Statement_Required := False;

               --  Delay_Statement

               when Tok_Delay =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Delay_Statement);
                  Statement_Required := False;

               --  Exit_Statement

               when Tok_Exit =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Exit_Statement);
                  Statement_Required := False;

               --  Loop_Statement with FOR and no label

               when Tok_For =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_For_Statement);
                  Statement_Required := False;

               --  Goto_Statement

               when Tok_Goto =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Goto_Statement);
                  Statement_Required := False;

               --  If_Statement

               when Tok_If =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_If_Statement);
                  Statement_Required := False;

               --  Loop_Statement

               when Tok_Loop =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Loop_Statement);
                  Statement_Required := False;

               --  Null_Statement

               when Tok_Null =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Null_Statement);
                  Statement_Required := False;

               --  Raise_Statement

               when Tok_Raise =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Raise_Statement);
                  Statement_Required := False;

               --  Requeue_Statement

               when Tok_Requeue =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Requeue_Statement);
                  Statement_Required := False;

               --  Return_Statement

               when Tok_Return =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Return_Statement);
                  Statement_Required := False;

               --  Select_Statement

               when Tok_Select =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_Select_Statement);
                  Statement_Required := False;

               --  While_Statement (Block_Statement with while and no loop)

               when Tok_While =>
                  Check_Bad_Layout;
                  Append_To (Statement_List, P_While_Statement);
                  Statement_Required := False;

               --  Anything else is some kind of junk, signal an error message
               --  and then raise Error_Resync, to merge with the normal
               --  handling of a bad statement.

               when others =>

                  if Token in Token_Class_Declk then
                     Junk_Declaration;

                  else
                     Error_Msg_BC -- CODEFIX
                       ("statement expected");
                     raise Error_Resync;
                  end if;
            end case;

         --  On error resynchronization, skip past next semicolon, and, since
         --  we are still in the statement loop, look for next statement. We
         --  set Statement_Required False to avoid an unnecessary error message
         --  complaining that no statement was found (i.e. we consider the
         --  junk to satisfy the requirement for a statement being present).

         exception
            when Error_Resync =>
               Resync_Past_Semicolon_Or_To_Loop_Or_Then;
               Statement_Required := False;
         end;

         exit when SS_Flags.Unco;

      end loop;

      return Statement_List;

   end P_Sequence_Of_Statements;

   --------------------
   -- 5.1  Statement --
   --------------------

   ---------------------------
   -- 5.1  Simple Statement --
   ---------------------------

   --  Parsed by P_Sequence_Of_Statements (5.1)

   -----------------------------
   -- 5.1  Compound Statement --
   -----------------------------

   --  Parsed by P_Sequence_Of_Statements (5.1)

   -------------------------
   -- 5.1  Null Statement --
   -------------------------

   --  NULL_STATEMENT ::= null;

   --  The caller has already checked that the current token is null

   --  Error recovery: cannot raise Error_Resync

   function P_Null_Statement return Node_Id is
      Null_Stmt_Node : Node_Id;

   begin
      Null_Stmt_Node := New_Node (N_Null_Statement, Token_Ptr);
      Scan; -- past NULL
      TF_Semicolon;
      return Null_Stmt_Node;
   end P_Null_Statement;

   ----------------
   -- 5.1  Label --
   ----------------

   --  LABEL ::= <<label_STATEMENT_IDENTIFIER>>

   --  STATEMENT_IDENTIFIER ::= DIRECT_NAME

   --  The IDENTIFIER of a STATEMENT_IDENTIFIER shall be an identifier
   --  (not an OPERATOR_SYMBOL)

   --  The caller has already checked that the current token is <<

   --  Error recovery: can raise Error_Resync

   function P_Label return Node_Id is
      Label_Node : Node_Id;

   begin
      Label_Node := New_Node (N_Label, Token_Ptr);
      Scan; -- past <<
      Set_Identifier (Label_Node, P_Identifier (C_Greater_Greater));
      T_Greater_Greater;
      Append_Elmt (Label_Node, Label_List);
      return Label_Node;
   end P_Label;

   -------------------------------
   -- 5.1  Statement Identifier --
   -------------------------------

   --  Statement label is parsed by P_Label (5.1)

   --  Loop label is parsed by P_Loop_Statement (5.5), P_For_Statement (5.5)
   --   or P_While_Statement (5.5)

   --  Block label is parsed by P_Begin_Statement (5.6) or
   --   P_Declare_Statement (5.6)

   -------------------------------
   -- 5.2  Assignment Statement --
   -------------------------------

   --  ASSIGNMENT_STATEMENT ::=
   --    variable_NAME := EXPRESSION;

   --  Error recovery: can raise Error_Resync

   function P_Assignment_Statement (LHS : Node_Id) return Node_Id is
      Assign_Node : Node_Id;

   begin
      Assign_Node := New_Node (N_Assignment_Statement, Prev_Token_Ptr);
      Set_Name (Assign_Node, LHS);
      Set_Expression (Assign_Node, P_Expression_No_Right_Paren);
      TF_Semicolon;
      return Assign_Node;
   end P_Assignment_Statement;

   -----------------------
   -- 5.3  If Statement --
   -----------------------

   --  IF_STATEMENT ::=
   --    if CONDITION then
   --      SEQUENCE_OF_STATEMENTS
   --    {elsif CONDITION then
   --      SEQUENCE_OF_STATEMENTS}
   --    [else
   --      SEQUENCE_OF_STATEMENTS]
   --    end if;

   --  The caller has checked that the initial token is IF (or in the error
   --  case of a mysterious THEN, the initial token may simply be THEN, in
   --  which case, no condition (or IF) was scanned).

   --  Error recovery: can raise Error_Resync

   function P_If_Statement return Node_Id is
      If_Node    : Node_Id;
      Elsif_Node : Node_Id;
      Loc        : Source_Ptr;

      procedure Add_Elsif_Part;
      --  An internal procedure used to scan out a single ELSIF part. On entry
      --  the ELSIF (or an ELSE which has been determined should be ELSIF) is
      --  scanned out and is in Prev_Token.

      procedure Check_If_Column;
      --  An internal procedure used to check that THEN, ELSE, or ELSIF
      --  appear in the right place if column checking is enabled (i.e. if
      --  they are the first token on the line, then they must appear in
      --  the same column as the opening IF).

      procedure Check_Then_Column;
      --  This procedure carries out the style checks for a THEN token
      --  Note that the caller has set Loc to the Source_Ptr value for
      --  the previous IF or ELSIF token.

      function Else_Should_Be_Elsif return Boolean;
      --  An internal routine used to do a special error recovery check when
      --  an ELSE is encountered. It determines if the ELSE should be treated
      --  as an ELSIF. A positive decision (TRUE returned, is made if the ELSE
      --  is followed by a sequence of tokens, starting on the same line as
      --  the ELSE, which are not expression terminators, followed by a THEN.
      --  On entry, the ELSE has been scanned out.

      procedure Add_Elsif_Part is
      begin
         if No (Elsif_Parts (If_Node)) then
            Set_Elsif_Parts (If_Node, New_List);
         end if;

         Elsif_Node := New_Node (N_Elsif_Part, Prev_Token_Ptr);
         Loc := Prev_Token_Ptr;
         Set_Condition (Elsif_Node, P_Condition);
         Check_Then_Column;
         Then_Scan;
         Set_Then_Statements
           (Elsif_Node, P_Sequence_Of_Statements (SS_Eftm_Eltm_Sreq));
         Append (Elsif_Node, Elsif_Parts (If_Node));
      end Add_Elsif_Part;

      procedure Check_If_Column is
      begin
         if RM_Column_Check and then Token_Is_At_Start_Of_Line
           and then Start_Column /= Scope.Table (Scope.Last).Ecol
         then
            Error_Msg_Col := Scope.Table (Scope.Last).Ecol;
            Error_Msg_SC ("(style) this token should be@");
         end if;
      end Check_If_Column;

      procedure Check_Then_Column is
      begin
         if Token = Tok_Then then
            Check_If_Column;

            if Style_Check then
               Style.Check_Then (Loc);
            end if;
         end if;
      end Check_Then_Column;

      function Else_Should_Be_Elsif return Boolean is
         Scan_State : Saved_Scan_State;

      begin
         if Token_Is_At_Start_Of_Line then
            return False;

         else
            Save_Scan_State (Scan_State);

            loop
               if Token in Token_Class_Eterm then
                  Restore_Scan_State (Scan_State);
                  return False;
               else
                  Scan; -- past non-expression terminating token

                  if Token = Tok_Then then
                     Restore_Scan_State (Scan_State);
                     return True;
                  end if;
               end if;
            end loop;
         end if;
      end Else_Should_Be_Elsif;

   --  Start of processing for P_If_Statement

   begin
      If_Node := New_Node (N_If_Statement, Token_Ptr);

      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_If;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Labl := Error;
      Scope.Table (Scope.Last).Node := If_Node;

      if Token = Tok_If then
         Loc := Token_Ptr;
         Scan; -- past IF
         Set_Condition (If_Node, P_Condition);

         --  Deal with misuse of IF expression => used instead
         --  of WHEN expression =>

         if Token = Tok_Arrow then
            Error_Msg_SC -- CODEFIX
              ("THEN expected");
            Scan; -- past the arrow
            Pop_Scope_Stack; -- remove unneeded entry
            raise Error_Resync;
         end if;

         Check_Then_Column;

      else
         Error_Msg_SC ("no IF for this THEN");
         Set_Condition (If_Node, Error);
      end if;

      Then_Scan;

      Set_Then_Statements
        (If_Node, P_Sequence_Of_Statements (SS_Eftm_Eltm_Sreq));

      --  This loop scans out else and elsif parts

      loop
         if Token = Tok_Elsif then
            Check_If_Column;

            if Present (Else_Statements (If_Node)) then
               Error_Msg_SP ("ELSIF cannot appear after ELSE");
            end if;

            Scan; -- past ELSIF
            Add_Elsif_Part;

         elsif Token = Tok_Else then
            Check_If_Column;
            Scan; -- past ELSE

            if Else_Should_Be_Elsif then
               Error_Msg_SP -- CODEFIX
                 ("ELSE should be ELSIF");
               Add_Elsif_Part;

            else
               --  Here we have an else that really is an else

               if Present (Else_Statements (If_Node)) then
                  Error_Msg_SP ("only one ELSE part allowed");
                  Append_List
                    (P_Sequence_Of_Statements (SS_Eftm_Eltm_Sreq),
                     Else_Statements (If_Node));
               else
                  Set_Else_Statements
                    (If_Node, P_Sequence_Of_Statements (SS_Eftm_Eltm_Sreq));
               end if;
            end if;

         --  If anything other than ELSE or ELSIF, exit the loop. The token
         --  had better be END (and in fact it had better be END IF), but
         --  we will let End_Statements take care of checking that.

         else
            exit;
         end if;
      end loop;

      End_Statements;
      return If_Node;

   end P_If_Statement;

   --------------------
   -- 5.3  Condition --
   --------------------

   --  CONDITION ::= boolean_EXPRESSION

   function P_Condition return Node_Id is
   begin
      return P_Condition (P_Expression_No_Right_Paren);
   end P_Condition;

   function P_Condition (Cond : Node_Id) return Node_Id is
   begin
      --  It is never possible for := to follow a condition, so if we get
      --  a := we assume it is a mistyped equality. Note that we do not try
      --  to reconstruct the tree correctly in this case, but we do at least
      --  give an accurate error message.

      if Token = Tok_Colon_Equal then
         while Token = Tok_Colon_Equal loop
            Error_Msg_SC -- CODEFIX
              (""":="" should be ""=""");
            Scan; -- past junk :=
            Discard_Junk_Node (P_Expression_No_Right_Paren);
         end loop;

         return Cond;

      --  Otherwise check for redundant parentheses

      --  If the condition is a conditional or a quantified expression, it is
      --  parenthesized in the context of a condition, because of a separate
      --  syntax rule.

      else
         if Style_Check and then Paren_Count (Cond) > 0 then
            if not Nkind_In (Cond, N_If_Expression,
                                   N_Case_Expression,
                                   N_Quantified_Expression)
              or else Paren_Count (Cond) > 1
            then
               Style.Check_Xtra_Parens (First_Sloc (Cond));
            end if;
         end if;

         --  And return the result

         return Cond;
      end if;
   end P_Condition;

   -------------------------
   -- 5.4  Case Statement --
   -------------------------

   --  CASE_STATEMENT ::=
   --    case EXPRESSION is
   --      CASE_STATEMENT_ALTERNATIVE
   --      {CASE_STATEMENT_ALTERNATIVE}
   --    end case;

   --  The caller has checked that the first token is CASE

   --  Can raise Error_Resync

   function P_Case_Statement return Node_Id is
      Case_Node         : Node_Id;
      Alternatives_List : List_Id;
      First_When_Loc    : Source_Ptr;

   begin
      Case_Node := New_Node (N_Case_Statement, Token_Ptr);

      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Case;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Labl := Error;
      Scope.Table (Scope.Last).Node := Case_Node;

      Scan; -- past CASE
      Set_Expression (Case_Node, P_Expression_No_Right_Paren);
      TF_Is;

      --  Prepare to parse case statement alternatives

      Alternatives_List := New_List;
      P_Pragmas_Opt (Alternatives_List);
      First_When_Loc := Token_Ptr;

      --  Loop through case statement alternatives

      loop
         --  If we have a WHEN or OTHERS, then that's fine keep going. Note
         --  that it is a semantic check to ensure the proper use of OTHERS

         if Token = Tok_When or else Token = Tok_Others then
            Append (P_Case_Statement_Alternative, Alternatives_List);

         --  If we have an END, then probably we are at the end of the case
         --  but we only exit if Check_End thinks the END was reasonable.

         elsif Token = Tok_End then
            exit when Check_End;

         --  Here if token is other than WHEN, OTHERS or END. We definitely
         --  have an error, but the question is whether or not to get out of
         --  the case statement. We don't want to get out early, or we will
         --  get a slew of junk error messages for subsequent when tokens.

         --  If the token is not at the start of the line, or if it is indented
         --  with respect to the current case statement, then the best guess is
         --  that we are still supposed to be inside the case statement. We
         --  complain about the missing WHEN, and discard the junk statements.

         elsif not Token_Is_At_Start_Of_Line
           or else Start_Column > Scope.Table (Scope.Last).Ecol
         then
            Error_Msg_BC ("WHEN (case statement alternative) expected");

            --  Here is a possibility for infinite looping if we don't make
            --  progress. So try to process statements, otherwise exit

            declare
               Error_Ptr : constant Source_Ptr := Scan_Ptr;
            begin
               Discard_Junk_List (P_Sequence_Of_Statements (SS_Whtm));
               exit when Scan_Ptr = Error_Ptr and then Check_End;
            end;

         --  Here we have a junk token at the start of the line and it is
         --  not indented. If Check_End thinks there is a missing END, then
         --  we will get out of the case, otherwise we keep going.

         else
            exit when Check_End;
         end if;
      end loop;

      --  Make sure we have at least one alternative

      if No (First_Non_Pragma (Alternatives_List)) then
         Error_Msg
            ("WHEN expected, must have at least one alternative in case",
             First_When_Loc);
         return Error;

      else
         Set_Alternatives (Case_Node, Alternatives_List);
         return Case_Node;
      end if;
   end P_Case_Statement;

   -------------------------------------
   -- 5.4  Case Statement Alternative --
   -------------------------------------

   --  CASE_STATEMENT_ALTERNATIVE ::=
   --    when DISCRETE_CHOICE_LIST =>
   --      SEQUENCE_OF_STATEMENTS

   --  The caller has checked that the initial token is WHEN or OTHERS
   --  Error recovery: can raise Error_Resync

   function P_Case_Statement_Alternative return Node_Id is
      Case_Alt_Node : Node_Id;

   begin
      if Style_Check then
         Style.Check_Indentation;
      end if;

      Case_Alt_Node := New_Node (N_Case_Statement_Alternative, Token_Ptr);
      T_When; -- past WHEN (or give error in OTHERS case)
      Set_Discrete_Choices (Case_Alt_Node, P_Discrete_Choice_List);
      TF_Arrow;
      Set_Statements (Case_Alt_Node, P_Sequence_Of_Statements (SS_Sreq_Whtm));
      return Case_Alt_Node;
   end P_Case_Statement_Alternative;

   -------------------------
   -- 5.5  Loop Statement --
   -------------------------

   --  LOOP_STATEMENT ::=
   --    [LOOP_STATEMENT_IDENTIFIER:]
   --      [ITERATION_SCHEME] loop
   --        SEQUENCE_OF_STATEMENTS
   --      end loop [loop_IDENTIFIER];

   --  ITERATION_SCHEME ::=
   --    while CONDITION
   --  | for LOOP_PARAMETER_SPECIFICATION

   --  The parsing of loop statements is handled by one of three functions
   --  P_Loop_Statement, P_For_Statement or P_While_Statement depending
   --  on the initial keyword in the construct (excluding the identifier)

   --  P_Loop_Statement

   --  This function parses the case where no iteration scheme is present

   --  The caller has checked that the initial token is LOOP. The parameter
   --  is the node identifiers for the loop label if any (or is set to Empty
   --  if there is no loop label).

   --  Error recovery : cannot raise Error_Resync

   function P_Loop_Statement (Loop_Name : Node_Id := Empty) return Node_Id is
      Loop_Node    : Node_Id;
      Created_Name : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Labl := Loop_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Etyp := E_Loop;

      Loop_Node := New_Node (N_Loop_Statement, Token_Ptr);
      TF_Loop;

      if No (Loop_Name) then
         Created_Name :=
           Make_Identifier (Sloc (Loop_Node), Set_Loop_Block_Name ('L'));
         Set_Comes_From_Source (Created_Name, False);
         Set_Has_Created_Identifier (Loop_Node, True);
         Set_Identifier (Loop_Node, Created_Name);
         Scope.Table (Scope.Last).Labl := Created_Name;
      else
         Set_Identifier (Loop_Node, Loop_Name);
      end if;

      Append_Elmt (Loop_Node, Label_List);
      Set_Statements (Loop_Node, P_Sequence_Of_Statements (SS_Sreq));
      End_Statements (Loop_Node);
      return Loop_Node;
   end P_Loop_Statement;

   --  P_For_Statement

   --  This function parses a loop statement with a FOR iteration scheme

   --  The caller has checked that the initial token is FOR. The parameter
   --  is the node identifier for the block label if any (or is set to Empty
   --  if there is no block label).

   --  Note: the caller fills in the Identifier field if a label was present

   --  Error recovery: can raise Error_Resync

   function P_For_Statement (Loop_Name : Node_Id := Empty) return Node_Id is
      Loop_Node        : Node_Id;
      Iter_Scheme_Node : Node_Id;
      Loop_For_Flag    : Boolean;
      Created_Name     : Node_Id;
      Spec             : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Labl := Loop_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Etyp := E_Loop;

      Loop_For_Flag := (Prev_Token = Tok_Loop);
      Scan; -- past FOR
      Iter_Scheme_Node := New_Node (N_Iteration_Scheme, Token_Ptr);
      Spec := P_Loop_Parameter_Specification;

      if Nkind (Spec) = N_Loop_Parameter_Specification then
         Set_Loop_Parameter_Specification (Iter_Scheme_Node, Spec);
      else
         Set_Iterator_Specification (Iter_Scheme_Node, Spec);
      end if;

      --  The following is a special test so that a miswritten for loop such
      --  as "loop for I in 1..10;" is handled nicely, without making an extra
      --  entry in the scope stack. We don't bother to actually fix up the
      --  tree in this case since it's not worth the effort. Instead we just
      --  eat up the loop junk, leaving the entry for what now looks like an
      --  unmodified loop intact.

      if Loop_For_Flag and then Token = Tok_Semicolon then
         Error_Msg_SC ("LOOP belongs here, not before FOR");
         Pop_Scope_Stack;
         return Error;

      --  Normal case

      else
         Loop_Node := New_Node (N_Loop_Statement, Token_Ptr);

         if No (Loop_Name) then
            Created_Name :=
              Make_Identifier (Sloc (Loop_Node), Set_Loop_Block_Name ('L'));
            Set_Comes_From_Source (Created_Name, False);
            Set_Has_Created_Identifier (Loop_Node, True);
            Set_Identifier (Loop_Node, Created_Name);
            Scope.Table (Scope.Last).Labl := Created_Name;
         else
            Set_Identifier (Loop_Node, Loop_Name);
         end if;

         TF_Loop;
         Set_Statements (Loop_Node, P_Sequence_Of_Statements (SS_Sreq));
         End_Statements (Loop_Node);
         Set_Iteration_Scheme (Loop_Node, Iter_Scheme_Node);
         Append_Elmt (Loop_Node, Label_List);
         return Loop_Node;
      end if;
   end P_For_Statement;

   --  P_While_Statement

   --  This procedure scans a loop statement with a WHILE iteration scheme

   --  The caller has checked that the initial token is WHILE. The parameter
   --  is the node identifier for the block label if any (or is set to Empty
   --  if there is no block label).

   --  Error recovery: cannot raise Error_Resync

   function P_While_Statement (Loop_Name : Node_Id := Empty) return Node_Id is
      Loop_Node        : Node_Id;
      Iter_Scheme_Node : Node_Id;
      Loop_While_Flag  : Boolean;
      Created_Name     : Node_Id;

   begin
      Push_Scope_Stack;
      Scope.Table (Scope.Last).Labl := Loop_Name;
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scope.Table (Scope.Last).Etyp := E_Loop;

      Loop_While_Flag := (Prev_Token = Tok_Loop);
      Iter_Scheme_Node := New_Node (N_Iteration_Scheme, Token_Ptr);
      Scan; -- past WHILE
      Set_Condition (Iter_Scheme_Node, P_Condition);

      --  The following is a special test so that a miswritten for loop such
      --  as "loop while I > 10;" is handled nicely, without making an extra
      --  entry in the scope stack. We don't bother to actually fix up the
      --  tree in this case since it's not worth the effort. Instead we just
      --  eat up the loop junk, leaving the entry for what now looks like an
      --  unmodified loop intact.

      if Loop_While_Flag and then Token = Tok_Semicolon then
         Error_Msg_SC ("LOOP belongs here, not before WHILE");
         Pop_Scope_Stack;
         return Error;

      --  Normal case

      else
         Loop_Node := New_Node (N_Loop_Statement, Token_Ptr);
         TF_Loop;

         if No (Loop_Name) then
            Created_Name :=
              Make_Identifier (Sloc (Loop_Node), Set_Loop_Block_Name ('L'));
            Set_Comes_From_Source (Created_Name, False);
            Set_Has_Created_Identifier (Loop_Node, True);
            Set_Identifier (Loop_Node, Created_Name);
            Scope.Table (Scope.Last).Labl := Created_Name;
         else
            Set_Identifier (Loop_Node, Loop_Name);
         end if;

         Set_Statements (Loop_Node, P_Sequence_Of_Statements (SS_Sreq));
         End_Statements (Loop_Node);
         Set_Iteration_Scheme (Loop_Node, Iter_Scheme_Node);
         Append_Elmt (Loop_Node, Label_List);
         return Loop_Node;
      end if;
   end P_While_Statement;

   ---------------------------------------
   -- 5.5  Loop Parameter Specification --
   ---------------------------------------

   --  LOOP_PARAMETER_SPECIFICATION ::=
   --    DEFINING_IDENTIFIER in [reverse] DISCRETE_SUBTYPE_DEFINITION

   --  Error recovery: cannot raise Error_Resync

   function P_Loop_Parameter_Specification return Node_Id is
      Loop_Param_Specification_Node : Node_Id;

      ID_Node    : Node_Id;
      Scan_State : Saved_Scan_State;

   begin

      Save_Scan_State (Scan_State);
      ID_Node := P_Defining_Identifier (C_In);

      --  If the next token is OF, it indicates an Ada 2012 iterator. If the
      --  next token is a colon, this is also an Ada 2012 iterator, including
      --  a subtype indication for the loop parameter. Otherwise we parse the
      --  construct as a loop parameter specification. Note that the form
      --  "for A in B" is ambiguous, and must be resolved semantically: if B
      --  is a discrete subtype this is a loop specification, but if it is an
      --  expression it is an iterator specification. Ambiguity is resolved
      --  during analysis of the loop parameter specification.

      if Token = Tok_Of or else Token = Tok_Colon then
         Error_Msg_Ada_2012_Feature ("iterator", Token_Ptr);
         return P_Iterator_Specification (ID_Node);
      end if;

      --  The span of the Loop_Parameter_Specification starts at the
      --  defining identifier.

      Loop_Param_Specification_Node :=
        New_Node (N_Loop_Parameter_Specification, Sloc (ID_Node));
      Set_Defining_Identifier (Loop_Param_Specification_Node, ID_Node);

      if Token = Tok_Left_Paren then
         Error_Msg_SC ("subscripted loop parameter not allowed");
         Restore_Scan_State (Scan_State);
         Discard_Junk_Node (P_Name);

      elsif Token = Tok_Dot then
         Error_Msg_SC ("selected loop parameter not allowed");
         Restore_Scan_State (Scan_State);
         Discard_Junk_Node (P_Name);
      end if;

      T_In;

      if Token = Tok_Reverse then
         Scan; -- past REVERSE
         Set_Reverse_Present (Loop_Param_Specification_Node, True);
      end if;

      Set_Discrete_Subtype_Definition
        (Loop_Param_Specification_Node, P_Discrete_Subtype_Definition);
      return Loop_Param_Specification_Node;

   exception
      when Error_Resync =>
         return Error;
   end P_Loop_Parameter_Specification;

   ----------------------------------
   -- 5.5.1 Iterator_Specification --
   ----------------------------------

   function P_Iterator_Specification (Def_Id : Node_Id) return Node_Id is
      Node1 : Node_Id;

   begin
      Node1 := New_Node (N_Iterator_Specification, Sloc (Def_Id));
      Set_Defining_Identifier (Node1, Def_Id);

      if Token = Tok_Colon then
         Scan;  --  past :
         Set_Subtype_Indication (Node1, P_Subtype_Indication);
      end if;

      if Token = Tok_Of then
         Set_Of_Present (Node1);
         Scan;  --  past OF

      elsif Token = Tok_In then
         Scan;  --  past IN

      elsif Prev_Token = Tok_In
        and then Present (Subtype_Indication (Node1))
      then
         --  Simplest recovery is to transform it into an element iterator.
         --  Error message on 'in" has already been emitted when parsing the
         --  optional constraint.

         Set_Of_Present (Node1);
         Error_Msg_N
           ("subtype indication is only legal on an element iterator",
              Subtype_Indication (Node1));

      else
         return Error;
      end if;

      if Token = Tok_Reverse then
         Scan; -- past REVERSE
         Set_Reverse_Present (Node1, True);
      end if;

      Set_Name (Node1, P_Name);
      return Node1;
   end P_Iterator_Specification;

   --------------------------
   -- 5.6  Block Statement --
   --------------------------

   --  BLOCK_STATEMENT ::=
   --    [block_STATEMENT_IDENTIFIER:]
   --      [declare
   --        DECLARATIVE_PART]
   --      begin
   --        HANDLED_SEQUENCE_OF_STATEMENTS
   --      end [block_IDENTIFIER];

   --  The parsing of block statements is handled by one of the two functions
   --  P_Declare_Statement or P_Begin_Statement depending on whether or not
   --  a declare section is present

   --  P_Declare_Statement

   --  This function parses a block statement with DECLARE present

   --  The caller has checked that the initial token is DECLARE

   --  Error recovery: cannot raise Error_Resync

   function P_Declare_Statement
     (Block_Name : Node_Id := Empty)
      return       Node_Id
   is
      Block_Node   : Node_Id;
      Created_Name : Node_Id;

   begin
      Block_Node := New_Node (N_Block_Statement, Token_Ptr);

      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Lreq := Present (Block_Name);
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Labl := Block_Name;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;

      Scan; -- past DECLARE

      if No (Block_Name) then
         Created_Name :=
           Make_Identifier (Sloc (Block_Node), Set_Loop_Block_Name ('B'));
         Set_Comes_From_Source (Created_Name, False);
         Set_Has_Created_Identifier (Block_Node, True);
         Set_Identifier (Block_Node, Created_Name);
         Scope.Table (Scope.Last).Labl := Created_Name;
      else
         Set_Identifier (Block_Node, Block_Name);
      end if;

      Append_Elmt (Block_Node, Label_List);
      Parse_Decls_Begin_End (Block_Node);
      return Block_Node;
   end P_Declare_Statement;

   --  P_Begin_Statement

   --  This function parses a block statement with no DECLARE present

   --  The caller has checked that the initial token is BEGIN

   --  Error recovery: cannot raise Error_Resync

   function P_Begin_Statement
     (Block_Name : Node_Id := Empty)
      return       Node_Id
   is
      Block_Node   : Node_Id;
      Created_Name : Node_Id;

   begin
      Block_Node := New_Node (N_Block_Statement, Token_Ptr);

      Push_Scope_Stack;
      Scope.Table (Scope.Last).Etyp := E_Name;
      Scope.Table (Scope.Last).Lreq := Present (Block_Name);
      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Labl := Block_Name;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;

      if No (Block_Name) then
         Created_Name :=
           Make_Identifier (Sloc (Block_Node), Set_Loop_Block_Name ('B'));
         Set_Comes_From_Source (Created_Name, False);
         Set_Has_Created_Identifier (Block_Node, True);
         Set_Identifier (Block_Node, Created_Name);
         Scope.Table (Scope.Last).Labl := Created_Name;
      else
         Set_Identifier (Block_Node, Block_Name);
      end if;

      Append_Elmt (Block_Node, Label_List);

      Scope.Table (Scope.Last).Ecol := Start_Column;
      Scope.Table (Scope.Last).Sloc := Token_Ptr;
      Scan; -- past BEGIN
      Set_Handled_Statement_Sequence
        (Block_Node, P_Handled_Sequence_Of_Statements);
      End_Statements (Handled_Statement_Sequence (Block_Node));
      return Block_Node;
   end P_Begin_Statement;

   -------------------------
   -- 5.7  Exit Statement --
   -------------------------

   --  EXIT_STATEMENT ::=
   --    exit [loop_NAME] [when CONDITION];

   --  The caller has checked that the initial token is EXIT

   --  Error recovery: can raise Error_Resync

   function P_Exit_Statement return Node_Id is
      Exit_Node : Node_Id;

      function Missing_Semicolon_On_Exit return Boolean;
      --  This function deals with the following specialized situation
      --
      --    when 'x' =>
      --       exit [identifier]
      --    when 'y' =>
      --
      --  This looks like a messed up EXIT WHEN, when in fact the problem
      --  is a missing semicolon. It is called with Token pointing to the
      --  WHEN token, and returns True if a semicolon is missing before
      --  the WHEN as in the above example.

      -------------------------------
      -- Missing_Semicolon_On_Exit --
      -------------------------------

      function Missing_Semicolon_On_Exit return Boolean is
         State : Saved_Scan_State;

      begin
         if not Token_Is_At_Start_Of_Line then
            return False;

         elsif Scope.Table (Scope.Last).Etyp /= E_Case then
            return False;

         else
            Save_Scan_State (State);
            Scan; -- past WHEN
            Scan; -- past token after WHEN

            if Token = Tok_Arrow then
               Restore_Scan_State (State);
               return True;
            else
               Restore_Scan_State (State);
               return False;
            end if;
         end if;
      end Missing_Semicolon_On_Exit;

   --  Start of processing for P_Exit_Statement

   begin
      Exit_Node := New_Node (N_Exit_Statement, Token_Ptr);
      Scan; -- past EXIT

      if Token = Tok_Identifier then
         Set_Name (Exit_Node, P_Qualified_Simple_Name);

      elsif Style_Check then
         --  This EXIT has no name, so check that
         --  the innermost loop is unnamed too.

         Check_No_Exit_Name :
         for J in reverse 1 .. Scope.Last loop
            if Scope.Table (J).Etyp = E_Loop then
               if Present (Scope.Table (J).Labl)
                 and then Comes_From_Source (Scope.Table (J).Labl)
               then
                  --  Innermost loop in fact had a name, style check fails

                  Style.No_Exit_Name (Scope.Table (J).Labl);
               end if;

               exit Check_No_Exit_Name;
            end if;
         end loop Check_No_Exit_Name;
      end if;

      if Token = Tok_When and then not Missing_Semicolon_On_Exit then
         Scan; -- past WHEN
         Set_Condition (Exit_Node, P_Condition);

      --  Allow IF instead of WHEN, giving error message

      elsif Token = Tok_If then
         T_When;
         Scan; -- past IF used in place of WHEN
         Set_Condition (Exit_Node, P_Expression_No_Right_Paren);
      end if;

      TF_Semicolon;
      return Exit_Node;
   end P_Exit_Statement;

   -------------------------
   -- 5.8  Goto Statement --
   -------------------------

   --  GOTO_STATEMENT ::= goto label_NAME;

   --  The caller has checked that the initial token is GOTO  (or TO in the
   --  error case where GO and TO were incorrectly separated).

   --  Error recovery: can raise Error_Resync

   function P_Goto_Statement return Node_Id is
      Goto_Node : Node_Id;

   begin
      Goto_Node := New_Node (N_Goto_Statement, Token_Ptr);
      Scan; -- past GOTO (or TO)
      Set_Name (Goto_Node, P_Qualified_Simple_Name_Resync);
      Append_Elmt (Goto_Node, Goto_List);
      No_Constraint;
      TF_Semicolon;
      return Goto_Node;
   end P_Goto_Statement;

   ---------------------------
   -- Parse_Decls_Begin_End --
   ---------------------------

   --  This function parses the construct:

   --      DECLARATIVE_PART
   --    begin
   --      HANDLED_SEQUENCE_OF_STATEMENTS
   --    end [NAME];

   --  The caller has built the scope stack entry, and created the node to
   --  whose Declarations and Handled_Statement_Sequence fields are to be
   --  set. On return these fields are filled in (except in the case of a
   --  task body, where the handled statement sequence is optional, and may
   --  thus be Empty), and the scan is positioned past the End sequence.

   --  If the BEGIN is missing, then the parent node is used to help construct
   --  an appropriate missing BEGIN message. Possibilities for the parent are:

   --    N_Block_Statement     declare block
   --    N_Entry_Body          entry body
   --    N_Package_Body        package body (begin part optional)
   --    N_Subprogram_Body     procedure or function body
   --    N_Task_Body           task body

   --  Note: in the case of a block statement, there is definitely a DECLARE
   --  present (because a Begin statement without a DECLARE is handled by the
   --  P_Begin_Statement procedure, which does not call Parse_Decls_Begin_End.

   --  Error recovery: cannot raise Error_Resync

   procedure Parse_Decls_Begin_End (Parent : Node_Id) is
      Body_Decl    : Node_Id;
      Decls        : List_Id;
      Parent_Nkind : Node_Kind;
      Spec_Node    : Node_Id;
      HSS          : Node_Id;

      procedure Missing_Begin (Msg : String);
      --  Called to post a missing begin message. In the normal case this is
      --  posted at the start of the current token. A special case arises when
      --  P_Declarative_Items has previously found a missing begin, in which
      --  case we replace the original error message.

      procedure Set_Null_HSS (Parent : Node_Id);
      --  Construct an empty handled statement sequence and install in Parent
      --  Leaves HSS set to reference the newly constructed statement sequence.

      -------------------
      -- Missing_Begin --
      -------------------

      procedure Missing_Begin (Msg : String) is
      begin
         if Missing_Begin_Msg = No_Error_Msg then
            Error_Msg_BC (Msg);
         else
            Change_Error_Text (Missing_Begin_Msg, Msg);

            --  Purge any messages issued after than, since a missing begin
            --  can cause a lot of havoc, and it is better not to dump these
            --  cascaded messages on the user.

            Purge_Messages (Get_Location (Missing_Begin_Msg), Prev_Token_Ptr);
         end if;
      end Missing_Begin;

      ------------------
      -- Set_Null_HSS --
      ------------------

      procedure Set_Null_HSS (Parent : Node_Id) is
         Null_Stm : Node_Id;

      begin
         Null_Stm :=
           Make_Null_Statement (Token_Ptr);
         Set_Comes_From_Source (Null_Stm, False);

         HSS :=
           Make_Handled_Sequence_Of_Statements (Token_Ptr,
             Statements => New_List (Null_Stm));
         Set_Comes_From_Source (HSS, False);

         Set_Handled_Statement_Sequence (Parent, HSS);
      end Set_Null_HSS;

   --  Start of processing for Parse_Decls_Begin_End

   begin
      Decls := P_Declarative_Part;

      if Ada_Version = Ada_83 then
         Check_Later_Vs_Basic_Declarations (Decls, During_Parsing => True);
      end if;

      --  Here is where we deal with the case of IS used instead of semicolon.
      --  Specifically, if the last declaration in the declarative part is a
      --  subprogram body still marked as having a bad IS, then this is where
      --  we decide that the IS should really have been a semicolon and that
      --  the body should have been a declaration. Note that if the bad IS
      --  had turned out to be OK (i.e. a decent begin/end was found for it),
      --  then the Bad_Is_Detected flag would have been reset by now.

      Body_Decl := Last (Decls);

      if Present (Body_Decl)
        and then Nkind (Body_Decl) = N_Subprogram_Body
        and then Bad_Is_Detected (Body_Decl)
      then
         --  OK, we have the case of a bad IS, so we need to fix up the tree.
         --  What we have now is a subprogram body with attached declarations
         --  and a possible statement sequence.

         --  First step is to take the declarations that were part of the bogus
         --  subprogram body and append them to the outer declaration chain.
         --  In other words we append them past the body (which we will later
         --  convert into a declaration).

         Append_List (Declarations (Body_Decl), Decls);

         --  Now take the handled statement sequence of the bogus body and
         --  set it as the statement sequence for the outer construct. Note
         --  that it may be empty (we specially allowed a missing BEGIN for
         --  a subprogram body marked as having a bad IS -- see below).

         Set_Handled_Statement_Sequence (Parent,
           Handled_Statement_Sequence (Body_Decl));

         --  Next step is to convert the old body node to a declaration node

         Spec_Node := Specification (Body_Decl);
         Change_Node (Body_Decl, N_Subprogram_Declaration);
         Set_Specification (Body_Decl, Spec_Node);

         --  Final step is to put the declarations for the parent where
         --  they belong, and then fall through the IF to scan out the
         --  END statements.

         Set_Declarations (Parent, Decls);

      --  This is the normal case (i.e. any case except the bad IS case)
      --  If we have a BEGIN, then scan out the sequence of statements, and
      --  also reset the expected column for the END to match the BEGIN.

      else
         Set_Declarations (Parent, Decls);

         if Token = Tok_Begin then
            if Style_Check then
               Style.Check_Indentation;
            end if;

            Error_Msg_Col := Scope.Table (Scope.Last).Ecol;

            if RM_Column_Check
              and then Token_Is_At_Start_Of_Line
              and then Start_Column /= Error_Msg_Col
            then
               Error_Msg_SC ("(style) BEGIN in wrong column, should be@");

            else
               Scope.Table (Scope.Last).Ecol := Start_Column;
            end if;

            Scope.Table (Scope.Last).Sloc := Token_Ptr;
            Scan; -- past BEGIN
            Set_Handled_Statement_Sequence (Parent,
              P_Handled_Sequence_Of_Statements);

         --  No BEGIN present

         else
            Parent_Nkind := Nkind (Parent);

            --  A special check for the missing IS case. If we have a
            --  subprogram body that was marked as having a suspicious
            --  IS, and the current token is END, then we simply confirm
            --  the suspicion, and do not require a BEGIN to be present

            if Parent_Nkind = N_Subprogram_Body
              and then Token  = Tok_End
              and then Scope.Table (Scope.Last).Etyp = E_Suspicious_Is
            then
               Scope.Table (Scope.Last).Etyp := E_Bad_Is;

            --  Otherwise BEGIN is not required for a package body, so we
            --  don't mind if it is missing, but we do construct a dummy
            --  one (so that we have somewhere to set End_Label).

            --  However if we have something other than a BEGIN which
            --  looks like it might be statements, then we signal a missing
            --  BEGIN for these cases as well. We define "something which
            --  looks like it might be statements" as a token other than
            --  END, EOF, or a token which starts declarations.

            elsif Parent_Nkind = N_Package_Body
              and then (Token = Tok_End
                          or else Token = Tok_EOF
                          or else Token in Token_Class_Declk)
            then
               Set_Null_HSS (Parent);

            --  These are cases in which a BEGIN is required and not present

            else
               Set_Null_HSS (Parent);

               --  Prepare to issue error message

               Error_Msg_Sloc := Scope.Table (Scope.Last).Sloc;
               Error_Msg_Node_1 := Scope.Table (Scope.Last).Labl;

               --  Now issue appropriate message

               if Parent_Nkind = N_Block_Statement then
                  Missing_Begin ("missing BEGIN for DECLARE#!");

               elsif Parent_Nkind = N_Entry_Body then
                  Missing_Begin ("missing BEGIN for ENTRY#!");

               elsif Parent_Nkind = N_Subprogram_Body then
                  if Nkind (Specification (Parent))
                               = N_Function_Specification
                  then
                     Missing_Begin ("missing BEGIN for function&#!");
                  else
                     Missing_Begin ("missing BEGIN for procedure&#!");
                  end if;

               --  The case for package body arises only when
               --  we have possible statement junk present.

               elsif Parent_Nkind = N_Package_Body then
                  Missing_Begin ("missing BEGIN for package body&#!");

               else
                  pragma Assert (Parent_Nkind = N_Task_Body);
                  Missing_Begin ("missing BEGIN for task body&#!");
               end if;

               --  Here we pick up the statements after the BEGIN that
               --  should have been present but was not. We don't insist
               --  on statements being present if P_Declarative_Part had
               --  already found a missing BEGIN, since it might have
               --  swallowed a lone statement into the declarative part.

               if Missing_Begin_Msg /= No_Error_Msg
                 and then Token = Tok_End
               then
                  null;
               else
                  Set_Handled_Statement_Sequence (Parent,
                    P_Handled_Sequence_Of_Statements);
               end if;
            end if;
         end if;
      end if;

      --  Here with declarations and handled statement sequence scanned

      if Present (Handled_Statement_Sequence (Parent)) then
         End_Statements (Handled_Statement_Sequence (Parent));
      else
         End_Statements;
      end if;

      --  We know that End_Statements removed an entry from the scope stack
      --  (because it is required to do so under all circumstances). We can
      --  therefore reference the entry it removed one past the stack top.
      --  What we are interested in is whether it was a case of a bad IS.

      if Scope.Table (Scope.Last + 1).Etyp = E_Bad_Is then
         Error_Msg -- CODEFIX
           ("|IS should be "";""", Scope.Table (Scope.Last + 1).S_Is);
         Set_Bad_Is_Detected (Parent, True);
      end if;

   end Parse_Decls_Begin_End;

   -------------------------
   -- Set_Loop_Block_Name --
   -------------------------

   function Set_Loop_Block_Name (L : Character) return Name_Id is
   begin
      Name_Buffer (1) := L;
      Name_Buffer (2) := '_';
      Name_Len := 2;
      Loop_Block_Count := Loop_Block_Count + 1;
      Add_Nat_To_Name_Buffer (Loop_Block_Count);
      return Name_Find;
   end Set_Loop_Block_Name;

   ---------------
   -- Then_Scan --
   ---------------

   procedure Then_Scan is
   begin
      TF_Then;

      while Token = Tok_Then loop
         Error_Msg_SC -- CODEFIX
           ("redundant THEN");
         TF_Then;
      end loop;

      if Token = Tok_And or else Token = Tok_Or then
         Error_Msg_SC ("unexpected logical operator");
         Scan; -- past logical operator

         if (Prev_Token = Tok_And and then Token = Tok_Then)
              or else
            (Prev_Token = Tok_Or  and then Token = Tok_Else)
         then
            Scan;
         end if;

         Discard_Junk_Node (P_Expression);
      end if;

      if Token = Tok_Then then
         Scan;
      end if;
   end Then_Scan;

end Ch5;
