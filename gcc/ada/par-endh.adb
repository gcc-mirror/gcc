------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . E N D H                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

with Stringt; use Stringt;
with Uintp;   use Uintp;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

separate (Par)
package body Endh is

   ----------------
   -- Local Data --
   ----------------

   type End_Action_Type is (
   --  Type used to describe the result of the Pop_End_Context call

      Accept_As_Scanned,
      --  Current end sequence is entirely c correct. In this case Token and
      --  the scan pointer are left pointing past the end sequence (i.e. they
      --  are unchanged from the values set on entry to Pop_End_Context).

      Insert_And_Accept,
      --  Current end sequence is to be left in place to satisfy some outer
      --  scope. Token and the scan pointer are set to point to the end
      --  token, and should be left there. A message has been generated
      --  indicating a missing end sequence. This status is also used for
      --  the case when no end token is present.

      Skip_And_Accept,
      --  The end sequence is incorrect (and an error message has been
      --  posted), but it will still be accepted. In this case Token and
      --  the scan pointer point back to the end token, and the caller
      --  should skip past the end sequence before proceeding.

      Skip_And_Reject);
      --  The end sequence is judged to belong to an unrecognized inner
      --  scope. An appropriate message has been issued and the caller
      --  should skip past the end sequence and then proceed as though
      --  no end sequence had been encountered.

   End_Action : End_Action_Type;
   --  The variable set by Pop_End_Context call showing which of the four
   --  decisions described above is judged the best.

   End_Sloc : Source_Ptr;
   --  Source location of END token

   End_OK : Boolean;
   --  Set False if error is found in END line

   End_Column : Column_Number;
   --  Column of END line

   End_Type : SS_End_Type;
   --  Type of END expected. The special value E_Dummy is set to indicate that
   --  no END token was present (so a missing END inserted message is needed)

   End_Labl : Node_Id;
   --  Node_Id value for explicit name on END line, or for compiler supplied
   --  name in the case where an optional name is not given. Empty if no name
   --  appears. If non-empty, then it is either an N_Designator node for a
   --  child unit or a node with a Chars field identifying the actual label.

   End_Labl_Present : Boolean;
   --  Indicates that the value in End_Labl was for an explicit label.

   Syntax_OK : Boolean;
   --  Set True if the entry is syntactically correct

   Token_OK : Boolean;
   --  Set True if the keyword in the END sequence matches, or if neither
   --  the END sequence nor the END stack entry has a keyword.

   Label_OK : Boolean;
   --  Set True if both the END sequence and the END stack entry contained
   --  labels (other than No_Name or Error_Name) and the labels matched.
   --  This is a stronger condition than SYNTAX_OK, since it means that a
   --  label was present, even in a case where it was optional. Note that
   --  the case of no label required, and no label present does NOT set
   --  Label_OK to True, it is True only if a positive label match is found.

   Column_OK : Boolean;
   --  Column_OK is set True if the END sequence appears in the expected column

   Scan_State : Saved_Scan_State;
   --  Save state at start of END sequence, in case we decide not to eat it up

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Evaluate_End_Entry (SS_Index : Int);
   --  Compare scanned END entry (as recorded by a prior call to P_End_Scan)
   --  with a specified entry in the scope stack (the single parameter is the
   --  entry index in the scope stack). Note that Scan is not called. The above
   --  variables xxx_OK are set to indicate the result of the evaluation.

   procedure Output_End_Deleted;
   --  Output a message complaining that the current END structure does not
   --  match anything and is being deleted.

   procedure Output_End_Expected (Ins : Boolean);
   --  Output a message at the start of the current token which is always an
   --  END, complaining that the END is not of the right form. The message
   --  indicates the expected form. The information for the message is taken
   --  from the top entry in the scope stack. The Ins parameter is True if
   --  an end is being inserted, and false if an existing end is being
   --  replaced. Note that in the case of a suspicious IS for the Ins case,
   --  we do not output the message, but instead simply mark the scope stack
   --  entry as being a case of a bad IS.

   procedure Output_End_Missing;
   --  Output a message just before the current token, complaining that the
   --  END is not of the right form. The message indicates the expected form.
   --  The information for the message is taken from the top entry in the
   --  scope stack. Note that in the case of a suspicious IS, we do not output
   --  the message, but instead simply mark the scope stack entry as a bad IS.

   procedure Pop_End_Context;
   --  Pop_End_Context is called after processing a construct, to pop the
   --  top entry off the end stack. It decides on the appropriate action to
   --  to take, signalling the result by setting End_Action as described in
   --  the global variable section.

   function Same_Label (Label1, Label2 : Node_Id) return Boolean;
   --  This function compares the two names associated with the given nodes.
   --  If they are both simple (i.e. have Chars fields), then they have to
   --  be the same name. Otherwise they must both be N_Selected_Component
   --  nodes, referring to the same set of names, or Label1 is an N_Designator
   --  referring to the same set of names as the N_Defining_Program_Unit_Name
   --  in Label2. Any other combination returns False. This routine is used
   --  to compare the End_Labl scanned from the End line with the saved label
   --  value in the scope stack.

   ---------------
   -- Check_End --
   ---------------

   function Check_End return Boolean is
      Name_On_Separate_Line : Boolean;
      --  Set True if the name on an END line is on a separate source line
      --  from the END. This is highly suspicious, but is allowed. The point
      --  is that we want to make sure that we don't just have a missing
      --  semicolon misleading us into swallowing an identifier from the
      --  following line.

      Name_Scan_State : Saved_Scan_State;
      --  Save state at start of name if Name_On_Separate_Line is TRUE

      Span_Node : constant Node_Id := Scope.Table (Scope.Last).Node;

   begin
      End_Labl_Present := False;
      End_Labl := Empty;

      --  Our first task is to scan out the END sequence if one is present.
      --  If none is present, signal by setting End_Type to E_Dummy.

      if Token /= Tok_End then
         End_Type := E_Dummy;

      else
         Save_Scan_State (Scan_State); -- at END
         End_Sloc := Token_Ptr;
         End_Column := Start_Column;
         End_OK := True;
         Scan; -- past END

         --  Set End_Span if expected. note that this will be useless
         --  if we do not have the right ending keyword, but in this
         --  case we have a malformed program anyway, and the setting
         --  of End_Span will simply be unreliable in this case anyway.

         if Present (Span_Node) then
            Set_End_Location (Span_Node, Token_Ptr);
         end if;

         --  Cases of keywords where no label is allowed

         if Token = Tok_Case then
            End_Type := E_Case;
            Scan; -- past CASE

         elsif Token = Tok_If then
            End_Type := E_If;
            Scan; -- past IF

         elsif Token = Tok_Record then
            End_Type := E_Record;
            Scan; -- past RECORD

         elsif Token = Tok_Select then
            End_Type := E_Select;
            Scan; -- past SELECT

         --  Cases which do allow labels

         else
            --  LOOP

            if Token = Tok_Loop then
               Scan; -- past LOOP
               End_Type := E_Loop;

            --  FOR or WHILE allowed (signalling error) to substitute for LOOP
            --  if on the same line as the END

            elsif (Token = Tok_For or else Token = Tok_While)
              and then not Token_Is_At_Start_Of_Line
            then
               Scan; -- past FOR or WHILE
               End_Type := E_Loop;
               End_OK := False;

            --  Cases with no keyword

            else
               End_Type := E_Name;
            end if;

            --  Now see if a name is present

            if Token = Tok_Identifier or else
               Token = Tok_String_Literal or else
               Token = Tok_Operator_Symbol
            then
               if Token_Is_At_Start_Of_Line then
                  Name_On_Separate_Line := True;
                  Save_Scan_State (Name_Scan_State);
               else
                  Name_On_Separate_Line := False;
               end if;

               End_Labl := P_Designator;
               End_Labl_Present := True;

               --  We have now scanned out a name. Here is where we do a check
               --  to catch the cases like:
               --
               --    end loop
               --    X := 3;
               --
               --  where the missing semicolon might make us swallow up the X
               --  as a bogus end label. In a situation like this, where the
               --  apparent name is on a separate line, we accept it only if
               --  it matches the label and is followed by a semicolon.

               if Name_On_Separate_Line then
                  if Token /= Tok_Semicolon or else
                    not Same_Label (End_Labl, Scope.Table (Scope.Last).Labl)
                  then
                     Restore_Scan_State (Name_Scan_State);
                     End_Labl := Empty;
                     End_Labl_Present := False;
                  end if;
               end if;

            --  Here for case of name allowed, but no name present. We will
            --  supply an implicit matching name, with source location set
            --  to the scan location past the END token.

            else
               End_Labl := Scope.Table (Scope.Last).Labl;

               if End_Labl > Empty_Or_Error then

                  --  The task here is to construct a designator from the
                  --  opening label, with the components all marked as not
                  --  from source, and Is_End_Label set in the identifier
                  --  or operator symbol. The location for all components
                  --  is the curent token location.

                  --  Case of child unit name

                  if Nkind (End_Labl) = N_Defining_Program_Unit_Name then
                     declare
                        Eref : constant Node_Id :=
                                 Make_Identifier (Token_Ptr,
                                   Chars =>
                                     Chars (Defining_Identifier (End_Labl)));

                        function Copy_Name (N : Node_Id) return Node_Id;
                        --  Copies a selected component or identifier

                        function Copy_Name (N : Node_Id) return Node_Id is
                           R : Node_Id;

                        begin
                           if Nkind (N) = N_Selected_Component then
                              return
                                Make_Selected_Component (Token_Ptr,
                                  Prefix        =>
                                    Copy_Name (Prefix (N)),
                                  Selector_Name =>
                                    Copy_Name (Selector_Name (N)));

                           else
                              R :=
                                Make_Identifier (Token_Ptr,
                                  Chars => Chars (N));
                              Set_Comes_From_Source (N, False);
                              return R;
                           end if;
                        end Copy_Name;

                     begin
                        Set_Comes_From_Source (Eref, False);

                        End_Labl :=
                          Make_Designator (Token_Ptr,
                            Name       => Copy_Name (Name (End_Labl)),
                            Identifier => Eref);
                     end;

                  --  Simple identifier case

                  elsif Nkind (End_Labl) = N_Defining_Identifier
                    or else Nkind (End_Labl) = N_Identifier
                  then
                     End_Labl :=
                       Make_Identifier (Token_Ptr,
                         Chars => Chars (End_Labl));

                  elsif Nkind (End_Labl) = N_Defining_Operator_Symbol
                    or else Nkind (End_Labl) = N_Operator_Symbol
                  then
                     Get_Decoded_Name_String (Chars (End_Labl));

                     End_Labl :=
                       Make_Operator_Symbol (Token_Ptr,
                         Chars  => Chars (End_Labl),
                         Strval => String_From_Name_Buffer);
                  end if;

                  Set_Comes_From_Source (End_Labl, False);
                  End_Labl_Present := False;

                  --  Do style check for missing label

                  if Style_Check
                    and then End_Type = E_Name
                    and then Present (Scope.Table (Scope.Last).Labl)
                  then
                     Style.No_End_Name (Scope.Table (Scope.Last).Labl);
                  end if;
               end if;
            end if;
         end if;

         --  Except in case of END RECORD, semicolon must follow. For END
         --  RECORD, a semicolon does follow, but it is part of a higher level
         --  construct. In any case, a missing semicolon is not serious enough
         --  to consider the END statement to be bad in the sense that we
         --  are dealing with (i.e. to be suspicious that it is not in fact
         --  the END statement we are looking for!)

         if End_Type /= E_Record then
            if Token = Tok_Semicolon then
               T_Semicolon;

            --  Semicolon is missing. If the missing semicolon is at the end
            --  of the line, i.e. we are at the start of the line now, then
            --  a missing semicolon gets flagged, but is not serious enough
            --  to consider the END statement to be bad in the sense that we
            --  are dealing with (i.e. to be suspicious that this END is not
            --  the END statement we are looking for).

            --  Similarly, if we are at a colon, we flag it but a colon for
            --  a semicolon is not serious enough to consider the END to be
            --  incorrect. Same thing for a period in place of a semicolon.

            elsif Token_Is_At_Start_Of_Line
              or else Token = Tok_Colon
              or else Token = Tok_Dot
            then
               T_Semicolon;

            --  If the missing semicolon is not at the start of the line,
            --  then we do consider the END line to be dubious in this sense.

            else
               End_OK := False;
            end if;
         end if;
      end if;

      --  Now we call the Pop_End_Context routine to get a recommendation
      --  as to what should be done with the END sequence we have scanned.

      Pop_End_Context;

      --  Remaining action depends on End_Action set by Pop_End_Context

      case End_Action is

         --  Accept_As_Scanned. In this case, Pop_End_Context left Token
         --  pointing past the last token of a syntactically correct END

         when Accept_As_Scanned =>

            --  Syntactically correct included the possibility of a missing
            --  semicolon. If we do have a missing semicolon, then we have
            --  already given a message, but now we scan out possible rubbish
            --  on the same line as the END

            while not Token_Is_At_Start_Of_Line
              and then Prev_Token /= Tok_Record
              and then Prev_Token /= Tok_Semicolon
              and then Token /= Tok_End
              and then Token /= Tok_EOF
            loop
               Scan; -- past junk
            end loop;

            return True;

         --  Insert_And_Accept. In this case, Pop_End_Context has reset Token
         --  to point to the start of the END sequence, and recommends that it
         --  be left in place to satisfy an outer scope level END. This means
         --  that we proceed as though an END were present, and leave the scan
         --  pointer unchanged.

         when Insert_And_Accept =>
            return True;

         --  Skip_And_Accept. In this case, Pop_End_Context has reset Token
         --  to point to the start of the END sequence. This END sequence is
         --  syntactically incorrect, and an appropriate error message has
         --  already been posted. Pop_End_Context recommends accepting the
         --  END sequence as the one we want, so we skip past it and then
         --  proceed as though an END were present.

         when Skip_And_Accept =>
            End_Skip;
            return True;

         --  Skip_And_Reject. In this case, Pop_End_Context has reset Token
         --  to point to the start of the END sequence. This END sequence is
         --  syntactically incorrect, and an appropriate error message has
         --  already been posted. Pop_End_Context recommends entirely ignoring
         --  this END sequence, so we skip past it and then return False, since
         --  as far as the caller is concerned, no END sequence is present.

         when Skip_And_Reject =>
            End_Skip;
            return False;
      end case;
   end Check_End;

   --------------
   -- End Skip --
   --------------

   --  This procedure skips past an END sequence. On entry Token contains
   --  Tok_End, and we know that the END sequence is syntactically incorrect,
   --  and that an appropriate error message has already been posted. The
   --  mission is simply to position the scan pointer to be the best guess of
   --  the position after the END sequence. We do not issue any additional
   --  error messages while carrying this out.

   --  Error recovery: does not raise Error_Resync

   procedure End_Skip is
   begin
      Scan; -- past END

      --  If the scan past the END leaves us on the next line, that's probably
      --  where we should quit the scan, since it is likely that what we have
      --  is a missing semicolon. Consider the following:

      --       END
      --       Process_Input;

      --  This will have looked like a syntactically valid END sequence to the
      --  initial scan of the END, but subsequent checking will have determined
      --  that the label Process_Input is not an appropriate label. The real
      --  error is a missing semicolon after the END, and by leaving the scan
      --  pointer just past the END, we will improve the error recovery.

      if Token_Is_At_Start_Of_Line then
         return;
      end if;

      --  If there is a semicolon after the END, scan it out and we are done

      if Token = Tok_Semicolon then
         T_Semicolon;
         return;
      end if;

      --  Otherwise skip past a token after the END on the same line. Note
      --  that we do not eat a token on the following line since it seems
      --  very unlikely in any case that the END gets separated from its
      --  token, and we do not want to swallow up a keyword that starts a
      --  legitimate construct following the bad END.

      if not Token_Is_At_Start_Of_Line
        and then

         --  Cases of normal tokens following an END

          (Token = Tok_Case   or else
           Token = Tok_For    or else
           Token = Tok_If     or else
           Token = Tok_Loop   or else
           Token = Tok_Record or else
           Token = Tok_Select or else

         --  Cases of bogus keywords ending loops

           Token = Tok_For    or else
           Token = Tok_While  or else

         --  Cases of operator symbol names without quotes

           Token = Tok_Abs    or else
           Token = Tok_And    or else
           Token = Tok_Mod    or else
           Token = Tok_Not    or else
           Token = Tok_Or     or else
           Token = Tok_Xor)

      then
         Scan; -- past token after END

         --  If that leaves us on the next line, then we are done. This is the
         --  same principle described above for the case of END at line end

         if Token_Is_At_Start_Of_Line then
            return;

         --  If we just scanned out record, then we are done, since the
         --  semicolon after END RECORD is not part of the END sequence

         elsif Prev_Token = Tok_Record then
            return;

         --  If we have a semicolon, scan it out and we are done

         elsif Token = Tok_Semicolon then
            T_Semicolon;
            return;
         end if;
      end if;

      --  Check for a label present on the same line

      loop
         if Token_Is_At_Start_Of_Line then
            return;
         end if;

         if Token /= Tok_Identifier
           and then Token /= Tok_Operator_Symbol
           and then Token /= Tok_String_Literal
         then
            exit;
         end if;

         Scan; -- past identifier, operator symbol or string literal

         if Token_Is_At_Start_Of_Line then
            return;
         elsif Token = Tok_Dot then
            Scan; -- past dot
         end if;
      end loop;

      --  Skip final semicolon

      if Token = Tok_Semicolon then
         T_Semicolon;

      --  If we don't have a final semicolon, skip until we either encounter
      --  an END token, or a semicolon or the start of the next line. This
      --  allows general junk to follow the end line (normally it is hard to
      --  think that anyone will put anything deliberate here, and remember
      --  that we know there is a missing semicolon in any case). We also
      --  quite on an EOF (or else we would get stuck in an infinite loop
      --  if there is no line end at the end of the last line of the file)

      else
         while Token /= Tok_End
           and then Token /= Tok_EOF
           and then Token /= Tok_Semicolon
           and then not Token_Is_At_Start_Of_Line
         loop
            Scan; -- past junk token on same line
         end loop;
      end if;

      return;
   end End_Skip;

   --------------------
   -- End Statements --
   --------------------

   --  This procedure is called when END is required or expected to terminate
   --  a sequence of statements. The caller has already made an appropriate
   --  entry on the scope stack to describe the expected form of the END.
   --  End_Statements should only be used in cases where the only appropriate
   --  terminator is END.

   --  Error recovery: cannot raise Error_Resync;

   procedure End_Statements (Parent : Node_Id := Empty) is
   begin
      --  This loop runs more than once in the case where Check_End rejects
      --  the END sequence, as indicated by Check_End returning False.

      loop
         if Check_End then
            if Present (Parent) then
               Set_End_Label (Parent, End_Labl);
            end if;

            return;
         end if;

         --  Extra statements past the bogus END are discarded. This is not
         --  ideal for maximum error recovery, but it's too much trouble to
         --  find an appropriate place to put them!

         Discard_Junk_List (P_Sequence_Of_Statements (SS_None));
      end loop;
   end End_Statements;

   ------------------------
   -- Evaluate End Entry --
   ------------------------

   procedure Evaluate_End_Entry (SS_Index : Int) is
   begin
      Column_OK := (End_Column = Scope.Table (SS_Index).Ecol);

      Token_OK  := (End_Type = Scope.Table (SS_Index).Etyp or else
                     (End_Type = E_Name and then
                       Scope.Table (SS_Index).Etyp >= E_Name));

      Label_OK := End_Labl_Present
                    and then
                      (Same_Label (End_Labl, Scope.Table (SS_Index).Labl)
                        or else Scope.Table (SS_Index).Labl = Error);

      --  Compute setting of Syntax_OK. We definitely have a syntax error
      --  if the Token does not match properly or if P_End_Scan detected
      --  a syntax error such as a missing semicolon.

      if not Token_OK or not End_OK then
         Syntax_OK := False;

      --  Final check is that label is OK. Certainly it is OK if there
      --  was an exact match on the label (the END label = the stack label)

      elsif Label_OK then
         Syntax_OK := True;

      --  Case of label present

      elsif End_Labl_Present then

         --  If probably misspelling, then complain, and pretend it is OK

         declare
            Nam : constant Node_Or_Entity_Id := Scope.Table (SS_Index).Labl;

         begin
            if Nkind (End_Labl) in N_Has_Chars
              and then Nkind (Nam) in N_Has_Chars
              and then Chars (End_Labl) > Error_Name
              and then Chars (Nam) > Error_Name
            then
               Get_Name_String (Chars (End_Labl));
               Error_Msg_Name_1 := Chars (Nam);

               if Error_Msg_Name_1 > Error_Name then
                  declare
                     S : String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

                  begin
                     Get_Name_String (Error_Msg_Name_1);

                     if Is_Bad_Spelling_Of
                         (Name_Buffer (1 .. Name_Len), S)
                     then
                        Error_Msg_N ("misspelling of %", End_Labl);
                        Syntax_OK := True;
                        return;
                     end if;
                  end;
               end if;
            end if;
         end;

         Syntax_OK := False;

      --  Otherwise we have cases of no label on the END line. For the loop
      --  case, this is acceptable only if the loop is unlabeled.

      elsif End_Type = E_Loop then
         Syntax_OK := (Scope.Table (SS_Index).Labl = Empty);

      --  Cases where a label is definitely allowed on the END line

      elsif End_Type = E_Name then
         Syntax_OK := (Scope.Table (SS_Index).Labl = Empty or else
                         not Scope.Table (SS_Index).Lreq);

      --  Otherwise we have cases which don't allow labels anyway, so we
      --  certainly accept an END which does not have a label.

      else
         Syntax_OK := True;
      end if;
   end Evaluate_End_Entry;

   ------------------------
   -- Output End Deleted --
   ------------------------

   procedure Output_End_Deleted is
   begin

      if End_Type = E_Loop then
         Error_Msg_SC ("no LOOP for this `END LOOP`!");

      elsif End_Type = E_Case then
         Error_Msg_SC ("no CASE for this `END CASE`");

      elsif End_Type = E_If then
         Error_Msg_SC ("no IF for this `END IF`!");

      elsif End_Type = E_Record then
         Error_Msg_SC ("no RECORD for this `END RECORD`!");

      elsif End_Type = E_Select then
         Error_Msg_SC ("no SELECT for this `END SELECT`!");

      else
         Error_Msg_SC ("no BEGIN for this END!");
      end if;
   end Output_End_Deleted;

   -------------------------
   -- Output End Expected --
   -------------------------

   procedure Output_End_Expected (Ins : Boolean) is
      End_Type : SS_End_Type;

   begin
      --  Suppress message if this was a potentially junk entry (e.g. a
      --  record entry where no record keyword was present.

      if Scope.Table (Scope.Last).Junk then
         return;
      end if;

      End_Type := Scope.Table (Scope.Last).Etyp;
      Error_Msg_Col    := Scope.Table (Scope.Last).Ecol;
      Error_Msg_Node_1 := Scope.Table (Scope.Last).Labl;
      Error_Msg_Sloc   := Scope.Table (Scope.Last).Sloc;

      --  Suppress message if error was posted on opening label

      if Error_Msg_Node_1 > Empty_Or_Error
        and then Error_Posted (Error_Msg_Node_1)
      then
         return;
      end if;

      if End_Type = E_Case then
         Error_Msg_SC ("`END CASE;` expected@ for CASE#!");

      elsif End_Type = E_If then
         Error_Msg_SC ("`END IF;` expected@ for IF#!");

      elsif End_Type = E_Loop then
         if Error_Msg_Node_1 = Empty then
            Error_Msg_SC
              ("`END LOOP;` expected@ for LOOP#!");
         else
            Error_Msg_SC ("`END LOOP &;` expected@!");
         end if;

      elsif End_Type = E_Record then
         Error_Msg_SC
           ("`END RECORD;` expected@ for RECORD#!");

      elsif End_Type = E_Select then
         Error_Msg_SC
           ("`END SELECT;` expected@ for SELECT#!");

      --  All remaining cases are cases with a name (we do not treat
      --  the suspicious is cases specially for a replaced end, only
      --  for an inserted end).

      elsif End_Type = E_Name or else (not Ins) then
         if Error_Msg_Node_1 = Empty then
            Error_Msg_SC ("`END;` expected@ for BEGIN#!");
         else
            Error_Msg_SC ("`END &;` expected@!");
         end if;

      --  The other possibility is a missing END for a subprogram with a
      --  suspicious IS (that probably should have been a semicolon). The
      --  Missing IS confirms the suspicion!

      else -- End_Type = E_Suspicious_Is or E_Bad_Is
         Scope.Table (Scope.Last).Etyp := E_Bad_Is;
      end if;
   end Output_End_Expected;

   ------------------------
   -- Output End Missing --
   ------------------------

   procedure Output_End_Missing is
      End_Type : SS_End_Type;

   begin
      --  Suppress message if this was a potentially junk entry (e.g. a
      --  record entry where no record keyword was present.

      if Scope.Table (Scope.Last).Junk then
         return;
      end if;

      End_Type := Scope.Table (Scope.Last).Etyp;
      Error_Msg_Node_1 := Scope.Table (Scope.Last).Labl;
      Error_Msg_Sloc   := Scope.Table (Scope.Last).Sloc;

      if End_Type = E_Case then
         Error_Msg_BC ("missing `END CASE;` for CASE#!");

      elsif End_Type = E_If then
         Error_Msg_BC ("missing `END IF;` for IF#!");

      elsif End_Type = E_Loop then
         if Error_Msg_Node_1 = Empty then
            Error_Msg_BC ("missing `END LOOP;` for LOOP#!");
         else
            Error_Msg_BC ("missing `END LOOP &;`!");
         end if;

      elsif End_Type = E_Record then
         Error_Msg_SC
           ("missing `END RECORD;` for RECORD#!");

      elsif End_Type = E_Select then
         Error_Msg_BC
           ("missing `END SELECT;` for SELECT#!");

      elsif End_Type = E_Name then
         if Error_Msg_Node_1 = Empty then
            Error_Msg_BC ("missing `END;` for BEGIN#!");
         else
            Error_Msg_BC ("missing `END &;`!");
         end if;

      else -- End_Type = E_Suspicious_Is or E_Bad_Is
         Scope.Table (Scope.Last).Etyp := E_Bad_Is;
      end if;
   end Output_End_Missing;

   ---------------------
   -- Pop End Context --
   ---------------------

   procedure Pop_End_Context is

      Pretty_Good : Boolean;
      --  This flag is set True if the END sequence is syntactically incorrect,
      --  but is (from a heuristic point of view), pretty likely to be simply
      --  a misspelling of the intended END.

      Outer_Match : Boolean;
      --  This flag is set True if we decide that the current END sequence
      --  belongs to some outer level entry in the scope stack, and thus
      --  we will NOT eat it up in matching the current expected END.

   begin
      --  If not at END, then output END expected message

      if End_Type = E_Dummy then
         Output_End_Missing;
         Pop_Scope_Stack;
         End_Action := Insert_And_Accept;
         return;

      --  Otherwise we do have an END present

      else
         --  A special check. If we have END; followed by an end of file,
         --  WITH or SEPARATE, then if we are not at the outer level, then
         --  we have a sytax error. Consider the example:

         --   ...
         --      declare
         --         X : Integer;
         --      begin
         --         X := Father (A);
         --         Process (X, X);
         --   end;
         --   with Package1;
         --   ...

         --  Now the END; here is a syntactically correct closer for the
         --  declare block, but if we eat it up, then we obviously have
         --  a missing END for the outer context (since WITH can only appear
         --  at the outer level.

         --  In this situation, we always reserve the END; for the outer level,
         --  even if it is in the wrong column. This is because it's much more
         --  useful to have the error message point to the DECLARE than to the
         --  package header in this case.

         --  We also reserve an end with a name before the end of file if the
         --  name is the one we expect at the outer level.

         if (Token = Tok_EOF or else
             Token = Tok_With or else
             Token = Tok_Separate)
           and then End_Type >= E_Name
           and then (not End_Labl_Present
                      or else Same_Label (End_Labl, Scope.Table (1).Labl))
           and then Scope.Last > 1
         then
            Restore_Scan_State (Scan_State); -- to END
            Output_End_Expected (Ins => True);
            Pop_Scope_Stack;
            End_Action := Insert_And_Accept;
            return;
         end if;

         --  Otherwise we go through the normal END evaluation procedure

         Evaluate_End_Entry (Scope.Last);

         --  If top entry in stack is syntactically correct, then we have
         --  scanned it out and everything is fine. This is the required
         --  action to properly process correct Ada programs.

         if Syntax_OK then

            --  Complain if checking columns and END is not in right column.
            --  Right in this context means exactly right, or on the same
            --  line as the opener.

            if Style.RM_Column_Check then
               if End_Column /= Scope.Table (Scope.Last).Ecol
                 and then Current_Line_Start > Scope.Table (Scope.Last).Sloc
               then
                  Error_Msg_Col := Scope.Table (Scope.Last).Ecol;
                  Error_Msg
                    ("(style) END in wrong column, should be@", End_Sloc);
               end if;
            end if;

            --  One final check. If the end had a label, check for an exact
            --  duplicate of this end sequence, and if so, skip it with an
            --  appropriate message.

            if End_Labl_Present and then Token = Tok_End then
               declare
                  Scan_State : Saved_Scan_State;
                  End_Loc    : constant Source_Ptr := Token_Ptr;
                  Nxt_Labl   : Node_Id;
                  Dup_Found  : Boolean := False;

               begin
                  Save_Scan_State (Scan_State);

                  Scan; -- past END

                  if Token = Tok_Identifier
                    or else Token = Tok_Operator_Symbol
                  then
                     Nxt_Labl := P_Designator;

                     --  We only consider it an error if the label is a match
                     --  and would be wrong for the level one above us, and
                     --  the indentation is the same.

                     if Token = Tok_Semicolon
                       and then Same_Label (End_Labl, Nxt_Labl)
                       and then End_Column = Start_Column
                       and then
                         (Scope.Last = 1
                            or else
                              (No (Scope.Table (Scope.Last - 1).Labl)
                                or else
                               not Same_Label
                                     (End_Labl,
                                      Scope.Table (Scope.Last - 1).Labl)))
                     then
                        T_Semicolon;
                        Error_Msg ("duplicate end line ignored", End_Loc);
                        Dup_Found := True;
                     end if;
                  end if;

                  if not Dup_Found then
                     Restore_Scan_State (Scan_State);
                  end if;
               end;
            end if;

            --  All OK, so return to caller indicating END is OK

            Pop_Scope_Stack;
            End_Action := Accept_As_Scanned;
            return;
         end if;

         --  If that check failed, then we definitely have an error. The issue
         --  is how to choose among three possible courses of action:

         --   1. Ignore the current END text completely, scanning past it,
         --      deciding that it belongs neither to the current context,
         --      nor to any outer context.

         --   2. Accept the current END text, scanning past it, and issuing
         --      an error message that it does not have the right form.

         --   3. Leave the current END text in place, NOT scanning past it,
         --      issuing an error message indicating the END expected for the
         --      current context. In this case, the END is available to match
         --      some outer END context.

         --  From a correct functioning point of view, it does not make any
         --  difference which of these three approaches we take, the program
         --  will work correctly in any case. However, making an accurate
         --  choice among these alternatives, i.e. choosing the one that
         --  corresponds to what the programmer had in mind, does make a
         --  significant difference in the quality of error recovery.

         Restore_Scan_State (Scan_State); -- to END

         --  First we see how good the current END entry is with respect to
         --  what we expect. It is considered pretty good if the token is OK,
         --  and either the label or the column matches. an END for RECORD is
         --  always considered to be pretty good in the record case. This is
         --  because not only does a record disallow a nested structure, but
         --  also it is unlikely that such nesting could occur by accident.

         Pretty_Good := (Token_OK and (Column_OK or Label_OK))
                          or else Scope.Table (Scope.Last).Etyp = E_Record;

         --  Next check, if there is a deeper entry in the stack which
         --  has a very high probability of being acceptable, then insert
         --  the END entry we want, leaving the higher level entry for later

         for J in reverse 1 .. Scope.Last - 1 loop
            Evaluate_End_Entry (J);

            --  To even consider the deeper entry to be immediately acceptable,
            --  it must be syntactically correct. Furthermore it must either
            --  have a correct label, or the correct column. If the current
            --  entry was a close match (Pretty_Good set), then we are even
            --  more strict in accepting the outer level one: even if it has
            --  the right label, it must have the right column as well.

            if Syntax_OK then
               if Pretty_Good then
                  Outer_Match := Label_OK and Column_OK;
               else
                  Outer_Match := Label_OK or Column_OK;
               end if;
            else
               Outer_Match := False;
            end if;

            --  If the outer entry does convincingly match the END text, then
            --  back up the scan to the start of the END sequence, issue an
            --  error message indicating the END we expected, and return with
            --  Token pointing to the END (case 3 from above discussion).

            if Outer_Match then
               Output_End_Missing;
               Pop_Scope_Stack;
               End_Action := Insert_And_Accept;
               return;
            end if;
         end loop;

         --  Here we have a situation in which the current END entry is
         --  syntactically incorrect, but there is no deeper entry in the
         --  END stack which convincingly matches it.

         --  If the END text was judged to be a Pretty_Good match for the
         --  expected token or if it appears left of the expected column,
         --  then we will accept it as the one we want, scanning past it, even
         --  though it is not completely right (we issue a message showing what
         --  we expected it to be). This is action 2 from the discussion above.
         --  There is one other special case to consider: the LOOP case.
         --  Consider the example:

         --     Lbl: loop
         --             null;
         --          end loop;

         --  Here the column lines up with Lbl, so END LOOP is to the right,
         --  but it is still acceptable. LOOP is the one case where alignment
         --  practices vary substantially in practice.

         if Pretty_Good
            or else End_Column <= Scope.Table (Scope.Last).Ecol
            or else (End_Type = Scope.Table (Scope.Last).Etyp
                        and then End_Type = E_Loop)
         then
            Output_End_Expected (Ins => False);
            Pop_Scope_Stack;
            End_Action := Skip_And_Accept;
            return;

         --  Here we have the case where the END is to the right of the
         --  expected column and does not have a correct label to convince
         --  us that it nevertheless belongs to the current scope. For this
         --  we consider that it probably belongs not to the current context,
         --  but to some inner context that was not properly recognized (due to
         --  other syntax errors), and for which no proper scope stack entry
         --  was made. The proper action in this case is to delete the END text
         --  and return False to the caller as a signal to keep on looking for
         --  an acceptable END. This is action 1 from the discussion above.

         else
            Output_End_Deleted;
            End_Action := Skip_And_Reject;
            return;
         end if;
      end if;
   end Pop_End_Context;

   ----------------
   -- Same_Label --
   ----------------

   function Same_Label (Label1, Label2 : Node_Id) return Boolean is
   begin
      if Nkind (Label1) in N_Has_Chars
        and then Nkind (Label2) in N_Has_Chars
      then
         return Chars (Label1) = Chars (Label2);

      elsif Nkind (Label1) = N_Selected_Component
        and then Nkind (Label2) = N_Selected_Component
      then
         return Same_Label (Prefix (Label1), Prefix (Label2)) and then
           Same_Label (Selector_Name (Label1), Selector_Name (Label2));

      elsif Nkind (Label1) = N_Designator
        and then Nkind (Label2) = N_Defining_Program_Unit_Name
      then
         return Same_Label (Name (Label1), Name (Label2)) and then
           Same_Label (Identifier (Label1), Defining_Identifier (Label2));

      else
         return False;
      end if;
   end Same_Label;

end Endh;
