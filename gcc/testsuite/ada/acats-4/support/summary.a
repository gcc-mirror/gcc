-- SUMMARY.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- PURPOSE:
--      This tool appends a test file summary for an ACATS test file to
--      a summary .CSV file. (See the ACATS documentation for details).
--
--      Command line:
--         Summary <ACATS_Test_File_Name> <Summary_of_Tests_File_Name>
--
-- CHANGE HISTORY:
--     16 May 2016   RLB  Created tool.
--     19 May 2016   RLB  Added unit parser.
--     30 May 2016   RLB  Fixed bug with pragma units.
--     24 Jun 2016   RLB  Eliminated recursive tag checking for comments that
--                        start a line (to fix tests like CA1010A1 without
--                        breaking tests like C611006 and CD30008).
--     28 Jun 2016   RLB  Added recognition of "OPTIONAL ERR MESSAGE:" as
--                        some legacy tests use this (incorrect) form.
--     25 Jan 2017   RLB  Corrected bug in the handling of main subprogram
--                        names.
--     27 Jan 2017   RLB  Changed the unit_name for a subunit to include the
--                        parent information.

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Special_Handling;
with Trace, Test_Summary;
procedure Summary is

   DEBUG : constant Boolean := False;

   Kill_Tool : exception;

   Source_File : Ada.Text_IO.File_Type;

   Simple_Source_Name : Trace.Name_Subtype;

   Summary_File : Ada.Text_IO.File_Type;


   -- Text buffer:

   subtype Line_Buffer_Subtype is String(1..260);

   type Line_Buffer_Rec is record
      Line_Num  : Trace.Line_Number_Type := 0; -- So it is safe to check
                                               -- unused buffers.
      Line_Last : Trace.Line_Position_Type;
      Last_Sig  : Trace.Line_Position_Type;
      Line      : Line_Buffer_Subtype;
   end record;

   -- A circular buffer of lines. Except for the current line, all of the
   -- lines in the buffer have text other than whitespace and comments.
   -- We use this buffer to back the tokens so the values can be extracted
   -- without having to store the identifiers in the token values. (Token_Error
   -- should be raised if the token is needed and isn't in the buffer anymore.)

   type Buffer_Count is range 0 .. 10;
   subtype Buffer_Index is Buffer_Count range 1 .. Buffer_Count'Last;

   Buffers : array (Buffer_Index) of Line_Buffer_Rec;


   procedure Process_Comment (The_Comment   : in String;
                              The_Line      : in Trace.Line_Number_Type;
                              First_Sig_Pos : in Trace.Line_Position_Type;
                              Last_Sig_Pos  : in Trace.Line_Position_Type;
                              Check_for_Inner_Tags : in Boolean) is
      -- Process a comment (if it is one of the special tags, we need
      -- to record that and their positional information). The "--" and any
      -- leading whitespace in the comment should have been removed.
      -- First_Sig_Pos and Last_Sig_Pos are the first and last significant
      -- characters on the line with the comment (0 if there are no
      -- significant characters on the line).
      Result : Test_Summary.Info_Record;

      procedure Check_for_Range_Indicators_then_Write_Result is
         -- Check the comment to see if it has range indicators; if so,
         -- update the result record to use them. Then (whether or not
         -- there are any range indicators), write the result.
         --
         -- A range indicator (sometimes known as a location indicator) has the
         -- following format ({} are literal; [] indicates optionality):
         --    {[sl:]sp[;[el:]ep]}
         -- Each of these values is relative, so it is one or two digits, with
         -- an optional minus sign for el. Omitted values are assumed to be
         -- zero. Specifically:
         --   sl - Start Line - offset *before* the current line for the
         --        start of the error range.
         --   sp - Start Position - position offset in the line indicated by
         --        Start Line (relative to the start of the line).
         --   el - End_Line - offset *before* the current line for the end of
         --        the error range. Can be negative if the end of the error
         --        range follows the error tag. But almost always should be
         --        zero.
         --   ep - End Position - position offset from the last significant
         --        character in the line indication by End_Line. (The last
         --        significant character is the last non-white-space character
         --        not including any comment.) Assumed zero if not present.
         SLoc, ELoc : Natural;
         SemiLoc, SColonLoc, EColonLoc : Natural;
      begin
         -- See if there is a range indicator following the test tag:
         SLoc := Ada.Strings.Fixed.Index (The_Comment, "{");
         ELoc := Ada.Strings.Fixed.Index (The_Comment, "}");
         if SLoc = 0 or else ELoc = 0 or else ELoc <= SLoc then
            -- No range indicator. Just write the result.
            Test_Summary.Write_Summary_Record (Summary_File, Result);
            return;
         end if;
         SemiLoc := Ada.Strings.Fixed.Index (The_Comment(SLoc..ELoc), ";");
         if SemiLoc = 0 then
            SColonLoc :=
               Ada.Strings.Fixed.Index (The_Comment(SLoc..ELoc), ":");
            EColonLoc := 0;
         else
            SColonLoc :=
               Ada.Strings.Fixed.Index (The_Comment(SLoc..SemiLoc), ":");
            EColonLoc :=
               Ada.Strings.Fixed.Index (The_Comment(SemiLoc..ELoc), ":");
         end if;
         if SColonLoc /= 0 then
             begin
                Result.Start_Line := The_Line - Trace.Line_Number_Type'Value
                   (The_Comment(SLoc+1..SColonLoc-1));
             exception
                when Constraint_Error =>
                   raise Constraint_Error with "bad locator SL " &
                      The_Comment(SLoc..ELoc) & " on line" &
                      Trace.Line_Number_Type'Image (The_Line);
             end;
         else -- no special start line.
             Result.Start_Line := The_Line;
         end if;
         begin
            if SemiLoc /= 0 then
                if SColonLoc /= 0 then -- {sl:sp;...
                   Result.Start_Position := Trace.Line_Position_Type'Value
                      (The_Comment(SColonLoc+1..SemiLoc-1));
                else -- {sp;...
                   Result.Start_Position := Trace.Line_Position_Type'Value
                      (The_Comment(SLoc+1..SemiLoc-1));
                end if;
            else
                if SColonLoc /= 0 then -- {sl:sp}
                   Result.Start_Position := Trace.Line_Position_Type'Value
                      (The_Comment(SColonLoc+1..ELoc-1));
                else -- {sp}
                   Result.Start_Position := Trace.Line_Position_Type'Value
                      (The_Comment(SLoc+1..ELoc-1));
                end if;
            end if;
         exception
            when Constraint_Error =>
               raise Constraint_Error with "bad locator SP " &
                  The_Comment(SLoc..ELoc) & " on line" &
                  Trace.Line_Number_Type'Image (The_Line);
         end;
         if SemiLoc /= 0 and then EColonLoc /= 0 then
             begin
                Result.End_Line := The_Line - Trace.Line_Number_Type'Value
                   (The_Comment(SemiLoc+1..EColonLoc-1));
             exception
                when Constraint_Error =>
                   raise Constraint_Error with "bad locator EL " &
                      The_Comment(SLoc..ELoc) & " on line" &
                      Trace.Line_Number_Type'Image (The_Line);
             end;
         else -- No special end line.
             Result.End_Line := The_Line;
         end if;
         if SemiLoc /= 0 then
             begin
                if EColonLoc /= 0 then
                    Result.End_Position := Trace.Line_Position_Type'Value
                       (The_Comment(EColonLoc+1..ELoc-1));
                else
                    Result.End_Position := Trace.Line_Position_Type'Value
                       (The_Comment(SemiLoc+1..ELoc-1));
                end if;
             exception
                when Constraint_Error =>
                   raise Constraint_Error with "bad locator EL " &
                      The_Comment(SLoc..ELoc) & " on line" &
                      Trace.Line_Number_Type'Image (The_Line);
             end;
         else -- No special end position.
             Result.End_Position := 0;
         end if;
         -- Now, find the ending significant character:
         if Result.End_Line = The_Line then
             if Last_Sig_Pos < Result.End_Position then
                raise Constraint_Error with "bad locator EP (range) " &
                   The_Comment(SLoc..ELoc) & " on line" &
                   Trace.Line_Number_Type'Image (The_Line);
             end if;
             Result.End_Position := Last_Sig_Pos - Result.End_Position;
         elsif Result.End_Line > The_Line then
             -- %%% This is a forward reference, we don't know where the
             -- %%% significant end is. So just use 79, the maximum line end.
             -- %%% We should try to fix this to accurately determine this,
             -- %%% but that seems hard (delaying writing these results would
             -- %%% be needed), and this hack will not cause anyone to fail
             -- %%% a test (most likely, it will include an area beyond the
             -- %%% end of the line).
             Result.End_Position := 79 - Result.End_Position;
         else -- Look in the buffers:
             for I in Buffers'range loop
                if Result.End_Line = Buffers(I).Line_Num then
                   if Buffers(I).Last_Sig < Result.End_Position then
                      raise Constraint_Error with "bad locator EP (brange) " &
                         The_Comment(SLoc..ELoc) & " on line" &
                         Trace.Line_Number_Type'Image (The_Line) &
                         "; Last_Sig=" & Trace.Line_Position_Type'Image (
                            Buffers(I).Last_Sig) & " for line" &
                         Trace.Line_Number_Type'Image (Result.End_Line);
                   end if;
                   Result.End_Position :=
                                Buffers(I).Last_Sig - Result.End_Position;
                   exit;
                elsif I = Buffers'Last then
                   raise Constraint_Error with "bad locator EP (no buffer) " &
                      The_Comment(SLoc..ELoc) & " on line" &
                      Trace.Line_Number_Type'Image (The_Line);
                -- else keep looking.
                end if;
             end loop;
         end if;
         -- Don't want positions of zero, so change them to 1.
         if Result.Start_Position = 0 then
            Result.Start_Position := 1;
         end if;
         if Result.End_Position = 0 then
            Result.End_Position := 1;
         end if;

         Test_Summary.Write_Summary_Record (Summary_File, Result);
      end Check_for_Range_Indicators_then_Write_Result;

   begin
      -- Note: ACATS test tags have to be in all upper case; if they're not
      -- the test is wrong (and we have a tool to check that). So we can
      -- just use straight string compares here.
      if The_Comment'Length = 0 then
         return; -- Nothing to do.
      elsif The_Comment(The_Comment'First) = ' ' then
         raise Program_Error with "forgot to remove whitespace from comment";
      end if;
      if DEBUG then
         Ada.Text_Io.Put_Line ("-- Comment Line " &
                               Trace.Line_Number_Type'Image(The_Line) &
                               ':' & The_Comment);
      end if;

      if The_Comment'Length >= 6 and then
         The_Comment(The_Comment'First..The_Comment'First+5) = "ERROR:" then
         -- If there is no Range indicator, then we just use the first
         -- significant position on this line.
         Result := (Kind => Test_Summary.Error,
                    Source_Name => Simple_Source_Name,
                    Start_Line | End_Line => The_Line,
                    Start_Position => First_Sig_Pos,
                    End_Position => Last_Sig_Pos);
         Check_for_Range_Indicators_then_Write_Result;

      elsif The_Comment'Length >= 2 and then
         The_Comment(The_Comment'First..The_Comment'First+1) = "OK" then
         -- If there is no Range indicator, then we just use the first
         -- significant position on this line.
         Result := (Kind => Test_Summary.OK,
                    Source_Name => Simple_Source_Name,
                    Start_Line | End_Line => The_Line,
                    Start_Position => First_Sig_Pos,
                    End_Position => Last_Sig_Pos);
         Check_for_Range_Indicators_then_Write_Result;

      elsif The_Comment'Length >= 15 and then
         The_Comment(The_Comment'First..The_Comment'First+14) =
            "POSSIBLE ERROR:" then
         declare
            Label : Test_Summary.PE_Label;
            SLoc, ELoc : Natural;
         begin
            -- Get the test label; we assume it is surrounded by square
            -- brackets.
            SLoc := Ada.Strings.Fixed.Index (The_Comment, "[");
            ELoc := Ada.Strings.Fixed.Index (The_Comment, "]");
            if SLoc = 0 or else ELoc = 0 or else ELoc <= SLoc then
               raise Program_Error with "unable to find PE Label in " &
                     "comment:" & The_Comment;
            end if;
            Ada.Strings.Fixed.Move (Target => Label,
                                    Source => The_Comment(SLoc+1..ELoc-1));

            -- If there is no Range indicator, then we just use the first
            -- significant position on this line.
            Result := (Kind => Test_Summary.Possible_Error,
                       Source_Name => Simple_Source_Name,
                       Start_Line | End_Line => The_Line,
                       Start_Position => First_Sig_Pos,
                       End_Position => Last_Sig_Pos,
                       Set_Label => Label);
            Check_for_Range_Indicators_then_Write_Result;
         end;

      elsif The_Comment'Length >= 15 and then
         The_Comment(The_Comment'First..The_Comment'First+14) =
            "OPTIONAL ERROR:" then
         -- If there is no Range indicator, then we just use the first
         -- significant position on this line.
         Result := (Kind => Test_Summary.Optional_Error,
                    Source_Name => Simple_Source_Name,
                    Start_Line | End_Line => The_Line,
                    Start_Position => First_Sig_Pos,
                    End_Position => Last_Sig_Pos);
         Check_for_Range_Indicators_then_Write_Result;

      elsif The_Comment'Length >= 21 and then
         The_Comment(The_Comment'First..The_Comment'First+19) =
            "OPTIONAL ERR MESSAGE" and then
            (The_Comment(The_Comment'First+20) = ':' or else
             The_Comment(The_Comment'First+20) = '.') then
         -- Note: Some ancient tests (esp. chapter 8, for example B83001A.ADA)
         -- use this form (followed by a period or colon).
         -- If there is no Range indicator, then we just use the first
         -- significant position on this line.
         Result := (Kind => Test_Summary.Optional_Error,
                    Source_Name => Simple_Source_Name,
                    Start_Line | End_Line => The_Line,
                    Start_Position => First_Sig_Pos,
                    End_Position => Last_Sig_Pos);
         Check_for_Range_Indicators_then_Write_Result;

      elsif The_Comment'Length >= 13 and then
         The_Comment(The_Comment'First..The_Comment'First+12) =
            "N/A => ERROR." then
         -- If there is no Range indicator, then we just use the first
         -- significant position on this line.
         Result := (Kind => Test_Summary.NA_Error,
                    Source_Name => Simple_Source_Name,
                    Start_Line | End_Line => The_Line,
                    Start_Position => First_Sig_Pos,
                    End_Position => Last_Sig_Pos);
         Check_for_Range_Indicators_then_Write_Result;

      elsif The_Comment'Length >= 10 and then
         The_Comment(The_Comment'First..The_Comment'First+9) =
            "ANX-C RQMT" then
         -- If there is no Range indicator, then we just use the first
         -- significant position on this line.
         Result := (Kind => Test_Summary.Annex_C_Requirement,
                    Source_Name => Simple_Source_Name,
                    Start_Line | End_Line => The_Line,
                    Start_Position => First_Sig_Pos,
                    End_Position => Last_Sig_Pos);
         Check_for_Range_Indicators_then_Write_Result;

      elsif Check_for_Inner_Tags and then
            Ada.Strings.Fixed.Index (The_Comment, "--") /= 0 then
         -- There is a nested comment, which might be a test tag (as in
         -- test B611006). Recurse to process it.
         declare
            SLoc : Natural := Ada.Strings.Fixed.Index (The_Comment, "--");
         begin
            SLoc := SLoc + 2; -- Skip the "--".
            while SLoc <= The_Comment'Last loop
               exit when The_Comment(SLoc) /= ' ';
               SLoc := SLoc + 1;
            end loop;
            Process_Comment (
               The_Comment   => The_Comment (SLoc .. The_Comment'Last),
               The_Line      => The_Line,
               First_Sig_Pos => First_Sig_Pos,
               Last_Sig_Pos  => Last_Sig_Pos,
               Check_for_Inner_Tags => False); -- Only do one level of this.
            return; -- We're done here.
         end;

      -- else not a recognized test tag, nothing to do.
      end if;
   end Process_Comment;

   -- Token information:

   type Token_Kind is (Unknown, -- Anything that we didn't try to identify.
                       EOF,
                       Identifier, -- Actually an expanded name.
                       Numeric_Literal,
                       String_Literal,
                       Dot,
                       LParen,
                       RParen,
                       Semicolon,
                       Generic_Word,
                       Body_Word,
                       Package_Word,
                       Procedure_Word,
                       Function_Word,
                       Protected_Word,
                       Task_Word,
                       Entry_Word,
                       Return_Word,
                       Is_Word,
                       New_Word,
                       Null_Word,
                       Abstract_Word,
                       Declare_Word,
                       Begin_Word,
                       End_Word,
                       Separate_Word,
                       Renames_Word,
                       With_Word,
                       Type_Word,
                       Record_Word,
                       Access_Word,
                       Interface_Word,
                       Loop_Word,
                       Case_Word,
                       If_Word,
                       When_Word,
                       Accept_Word,
                       Select_Word,
                       Do_Word,
                       Pragma_Word);
      -- This is a partial listing of kinds; we've only included ones that
      -- we need for parsing.

   type Token is record
      Line_Num   : Trace.Line_Number_Type;
      Buffer     : Buffer_Count := 0;
      First,Last : Natural;
      Kind       : Token_Kind := Unknown;
   end record;

   NO_TOKEN : constant Token  := (Line_Num => 0, Buffer => 0,
                                 First | Last => 0, Kind => Unknown);
      -- Represents no token.
   EOF_TOKEN : constant Token := (Line_Num => Trace.Line_Number_Type'Last,
                                 Buffer => 0,
                                 First | Last => 0, Kind => EOF);
      -- Represents the end of file.

   Token_Error : exception;

   -- The following Ada tokenizer is simplistic and not complete. We only
   -- tokenize enough to get the answers we need for this purpose.
   -- In particular, we do not try to handle character literals (other than
   -- '%' and '"' to avoid misdetecting string literals) nor the
   -- alternative base character ':'. In both cases, we'll get a bunch
   -- of tokens where we correctly would only get one, but this is OK as
   -- no reserved words can appear inside of a character literal or based
   -- number, and we're only parsing program units and things that fake those
   -- (like blocks); we skip as much as possible of expressions, statements,
   -- and expressions. This is necessary as some ACATS tests are not
   -- syntactically or even lexically correct. We want to be able to handle
   -- the maximum number of tests with minimal exceptions (as those will
   -- always pose a maintenance hazard).
   -- Whitespace is just space (' ') here, as most tests should not contain
   -- tab characters. But we do allow tabs in some cases as there are more
   -- tests than one would expect that include them unintentionally.

   -- The following declarations should be in the package body if
   -- we ever split the tokenizer into a separate package.
   Lookahead_Token : Token := NO_TOKEN;
      -- Holds the lookahead token if Line_Num /= 0. Note: Do not access this
      -- directly. (If we refactor this tool into multiple packages, this
      -- variable will be placed into a package body.)
   Name_Set : constant Ada.Strings.Maps.Character_Set :=
      Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('.'),
         Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('_'),
            Ada.Strings.Maps."or" (Ada.Strings.Maps.To_Set ('#'),
               Ada.Strings.Maps.Constants.Alphanumeric_Set)));
        -- Tokenize reserved words, expanded names, numbers (including
        -- based numbers).
   Current_Buffer : Buffer_Index := 1;
   EMPTY : constant Natural := Natural'Last;
   Next_Char : Natural := EMPTY;
      -- The next character to read.
   First_Sig_Char : Natural := EMPTY;
      -- The first significant character in the current buffer.
   Current_Line : Trace.Line_Number_Type := 0;
      -- The line in the current buffer.

   -- End package body declarations.

   function Peek_Next_Token return Token is
      -- Return (but do not consume) the next token in the ACATS test file.

      procedure Handle_Comment (Whole_Line : in Boolean) is
         -- Next_Char points at the start of a comment in the current
         -- buffer. Deal with it.
         First_Char : Natural := Next_Char;
      begin
         -- Skip the "--":
         Next_Char := Next_Char + 2;
         -- Skip any leading white space in the comment:
         while Next_Char <= Buffers(Current_Buffer).Line_Last loop
            exit when Buffers(Current_Buffer).Line(Next_Char) /= ' ';
            Next_Char := Next_Char + 1;
         end loop;
         Process_Comment (
            The_Comment   => Buffers(Current_Buffer).Line(Next_Char ..
                                          Buffers(Current_Buffer).Line_Last),
            The_Line      => Buffers(Current_Buffer).Line_Num,
            First_Sig_Pos => First_Sig_Char,
            Last_Sig_Pos  => Buffers(Current_Buffer).Last_Sig,
            Check_for_Inner_Tags => not Whole_Line);
               -- Do not check for inner tags in header comments (to fix
               -- the many tests like CA1010A1) and in commented out code
               -- (so test splits can be graded by the tools).

         -- We've eaten the entire line now:
         Next_Char := Buffers(Current_Buffer).Line_Last+1;
      end Handle_Comment;

   begin
      if Lookahead_Token.Line_Num /= 0 then
         return Lookahead_Token;
      end if;
      if Next_Char = EMPTY or else
         Next_Char > Buffers(Current_Buffer).Line_Last then
          -- The buffer is empty, or we've used all the characters. Read a
          -- new bufferful:
          begin
             Ada.Text_IO.Get_Line (Source_File, Buffers(Current_Buffer).Line,
                                   Buffers(Current_Buffer).Line_Last);
             Current_Line := Current_Line + 1;
             Buffers(Current_Buffer).Line_Num := Current_Line;
             Buffers(Current_Buffer).Last_Sig := 1;
          exception
             when Ada.Text_IO.End_Error =>
                -- Reached the end of the file, return EOF_TOKEN.
                Lookahead_Token := EOF_TOKEN;
                if DEBUG then
                   Ada.Text_IO.Put_Line ("Reached end of source file at line" &
                                  Trace.Line_Number_Type'Image(Current_Line));
                end if;
                return Lookahead_Token;
          end;
          Next_Char := 1;
          -- Special case: We repeat these checks here so we can discard any
          -- buffer that doesn't contain anything significant.
          -- Skip any leading whitespace:
          while Next_Char <= Buffers(Current_Buffer).Line_Last loop
             exit when Buffers(Current_Buffer).Line(Next_Char) /= ' ' and then
                       Buffers(Current_Buffer).Line(Next_Char) /= Ascii.HT;
             Next_Char := Next_Char + 1;
          end loop;
          if Next_Char+1 <= Buffers(Current_Buffer).Line_Last and then
             Buffers(Current_Buffer).Line(Next_Char .. Next_Char+1) = "--" then
             -- Comment.
             First_Sig_Char := 1; -- There is none, but we need something in
                                  -- range here.
             Handle_Comment (Whole_Line => True);
          end if;
          if Next_Char > Buffers(Current_Buffer).Line_Last then
             -- The line is only whitespace and a comment, do not save this
             -- buffer. Make a recursive call to get a token.
             Next_Char := EMPTY;
             return Peek_Next_Token;
          else
             First_Sig_Char := Next_Char;
             -- Drop out to the normal code.
          end if;
      end if;

      -- We have a buffer with remaining characters.
      -- Skip any leading whitespace:
      while Next_Char <= Buffers(Current_Buffer).Line_Last loop
         exit when Buffers(Current_Buffer).Line(Next_Char) /= ' ' and then
                   Buffers(Current_Buffer).Line(Next_Char) /= Ascii.HT;
         Next_Char := Next_Char + 1;
      end loop;
      if Next_Char+1 <= Buffers(Current_Buffer).Line_Last and then
         Buffers(Current_Buffer).Line(Next_Char .. Next_Char+1) = "--" then
         -- Comment.
         Handle_Comment (Whole_Line => False);
      end if;

      -- Check that we haven't reached the end of the line:
      if Next_Char > Buffers(Current_Buffer).Line_Last then
         -- Nothing significant left on the line, advance the buffer and get
         -- another line:
         Next_Char := EMPTY;
         if Current_Buffer = Buffers'Last then
            Current_Buffer := Buffers'First;
         else
            Current_Buffer := Current_Buffer + 1;
         end if;
         return Peek_Next_Token; -- Recursive call to get a token.
      end if;

      -- If we get here, Next_Char is the start of a token.
      Lookahead_Token.Line_Num := Current_Line;
      Lookahead_Token.Buffer := Current_Buffer;
      Lookahead_Token.Kind := Unknown; -- Unlike we know better.

      -- Try reserved words/identifiers/numbers.
      Ada.Strings.Fixed.Find_Token (
         Buffers(Current_Buffer).Line(Next_Char ..
                 Buffers(Current_Buffer).Line_Last),
                 Set   => Name_Set,
                 Test  => Ada.Strings.Inside,
                 First => Lookahead_Token.First,
                 Last  => Lookahead_Token.Last);
      if Lookahead_Token.First /= Next_Char or else
         Lookahead_Token.Last = 0 then
         -- Next token is not a reserved word/identifier/number.
         Lookahead_Token.First := Next_Char;
         -- Check the text directly for interesting cases:
         case Buffers(Current_Buffer).Line(Next_Char) is
            when '"' => -- String literal character.
               if Next_Char /= 1 and then
                  Buffers(Current_Buffer).Line(Next_Char-1) = ''' then
                  -- Actually, this appears to be part of a
                  -- character literal. Just return this single character.
                  Lookahead_Token.Last  := Next_Char;
                  Next_Char := Next_Char + 1;
               else -- String literal.
                  Next_Char := Next_Char + 1;
                  while Next_Char <= Buffers(Current_Buffer).Line_Last loop
                     if Buffers(Current_Buffer).Line(Next_Char) = '"' then
                        -- This is either the end of the string or a doubled
                        -- quote character.
                        if Next_Char = Buffers(Current_Buffer).Line_Last
                           or else
                           Buffers(Current_Buffer).Line(Next_Char+1) /= '"'
                           then
                           -- Normal end of string:
                           Lookahead_Token.Kind  := String_Literal;
                           Lookahead_Token.Last  := Next_Char;
                           Next_Char := Next_Char + 1;
                           exit;
                        else -- Skip the extra quote as well, then continue.
                           Next_Char := Next_Char + 2;
                        end if;
                     else -- Normal character.
                        Next_Char := Next_Char + 1;
                     end if;
                  end loop;
                  if Lookahead_Token.Kind /= String_Literal then
                     -- Didn't find the normal end of string.
                     if Simple_Source_Name(1..2) /= "B2" then
                        raise Token_Error with "no string end on " &
                           "line" &
                           Trace.Line_Number_Type'Image(Current_Line);
                     -- else B2 files include lexical tests and do include
                     -- some unclosed strings. So allow those.
                     end if;
                  -- else OK.
                  end if;
               end if;

            when '%' => -- Alternative string character.
               if Next_Char /= 1 and then
                  Buffers(Current_Buffer).Line(Next_Char-1) = ''' then
                  -- Actually, this appears to be part of a
                  -- character literal. Just return this single character.
                  Lookahead_Token.Last  := Next_Char;
                  Next_Char := Next_Char + 1;
               else -- String literal.
                  Next_Char := Next_Char + 1;
                  while Next_Char <= Buffers(Current_Buffer).Line_Last loop
                     if Buffers(Current_Buffer).Line(Next_Char) = '%' then
                        -- This is either the end of the string or a doubled
                        -- quote character.
                        if Next_Char = Buffers(Current_Buffer).Line_Last
                           or else
                           Buffers(Current_Buffer).Line(Next_Char+1) /= '%'
                           then
                           -- Normal end of string:
                           Lookahead_Token.Kind  := String_Literal;
                           Lookahead_Token.Last  := Next_Char;
                           Next_Char := Next_Char + 1;
                           exit;
                        else -- Skip the extra quote as well, then continue.
                           Next_Char := Next_Char + 2;
                        end if;
                     else -- Normal character.
                        Next_Char := Next_Char + 1;
                     end if;
                  end loop;
                  if Lookahead_Token.Kind /= String_Literal then
                     -- Didn't find the normal end of string.
                     if Simple_Source_Name(1..2) /= "B2" then
                        raise Token_Error with "no string end on " &
                           "line" &
                           Trace.Line_Number_Type'Image(Current_Line);
                     -- else B2 files include lexical tests and do include
                     -- some unclosed strings. So allow those.
                     end if;
                  -- else OK.
                  end if;
               end if;

            when '(' => -- LParam
               Lookahead_Token.Kind  := LParen;
               Lookahead_Token.Last  := Next_Char;
               Next_Char := Next_Char + 1;

            when ')' => -- RParam
               Lookahead_Token.Kind  := RParen;
               Lookahead_Token.Last  := Next_Char;
               Next_Char := Next_Char + 1;

            when ';' => -- Semicolon
               Lookahead_Token.Kind  := Semicolon;
               Lookahead_Token.Last  := Next_Char;
               Next_Char := Next_Char + 1;

            when Ascii.HT =>
               -- Tab; should not be any tests outside of lexical tests.
               Ada.Text_IO.Put_Line ("== Tab in text on " &
                    "line" & Trace.Line_Number_Type'Image(Current_Line));
               Lookahead_Token.Last  := Next_Char;
               Next_Char := Next_Char + 1;
               -- Note: Since these are first-class tokens, they could
               -- mess up parsing. That's the reason for the message above.
            when others =>
               -- Just return the first character as a token and continue.
               -- Note: We identify all of the tokens, including the two
               -- character tokens here, but that's more work and it
               -- doesn't appear that we care about those here so we won't
               -- bother for now.
               Lookahead_Token.Last  := Next_Char;
               Next_Char := Next_Char + 1;
         end case;
      else
         -- Next token is a reserved word/identifier/number, skip over
         -- its characters and return it.
         Next_Char := Lookahead_Token.Last + 1;
         if Buffers(Current_Buffer).Line(Lookahead_Token.First) in
            '0' .. '9' then
            Lookahead_Token.Kind  := Numeric_Literal;
         elsif Buffers(Current_Buffer).Line(Lookahead_Token.First) = '.' then
            Next_Char := Lookahead_Token.First + 1;
            Lookahead_Token.Last  := Lookahead_Token.First;
            Lookahead_Token.Kind  := Dot;
         else
            declare
               UPPER : constant String :=  Ada.Strings.Fixed.Translate (
                   Buffers(Current_Buffer).Line(Lookahead_Token.First ..
                                                Lookahead_Token.Last),
                                   Ada.Strings.Maps.Constants.Upper_Case_Map);
            begin
               if UPPER = "BEGIN" then
                  Lookahead_Token.Kind  := Begin_Word;
               elsif UPPER = "END" then
                  Lookahead_Token.Kind  := End_Word;
               elsif UPPER = "DECLARE" then
                  Lookahead_Token.Kind  := Declare_Word;
               elsif UPPER = "GENERIC" then
                  Lookahead_Token.Kind  := Generic_Word;
               elsif UPPER = "PACKAGE" then
                  Lookahead_Token.Kind  := Package_Word;
               elsif UPPER = "PROCEDURE" then
                  Lookahead_Token.Kind  := Procedure_Word;
               elsif UPPER = "FUNCTION" then
                  Lookahead_Token.Kind  := Function_Word;
               elsif UPPER = "PROTECTED" then
                  Lookahead_Token.Kind  := Protected_Word;
               elsif UPPER = "TASK" then
                  Lookahead_Token.Kind  := Task_Word;
               elsif UPPER = "ENTRY" then
                  Lookahead_Token.Kind  := Entry_Word;
               elsif UPPER = "SEPARATE" then
                  Lookahead_Token.Kind  := Separate_Word;
               elsif UPPER = "RENAMES" then
                  Lookahead_Token.Kind  := Renames_Word;
               elsif UPPER = "BODY" then
                  Lookahead_Token.Kind  := Body_Word;
               elsif UPPER = "RETURN" then
                  Lookahead_Token.Kind  := Return_Word;
               elsif UPPER = "IS" then
                  Lookahead_Token.Kind  := Is_Word;
               elsif UPPER = "NEW" then
                  Lookahead_Token.Kind  := New_Word;
               elsif UPPER = "NULL" then
                  Lookahead_Token.Kind  := Null_Word;
               elsif UPPER = "ABSTRACT" then
                  Lookahead_Token.Kind  := Abstract_Word;
               elsif UPPER = "WITH" then
                  Lookahead_Token.Kind  := With_Word;
               elsif UPPER = "TYPE" then
                  Lookahead_Token.Kind  := Type_Word;
               elsif UPPER = "RECORD" then
                  Lookahead_Token.Kind  := Record_Word;
               elsif UPPER = "ACCESS" then
                  Lookahead_Token.Kind  := Access_Word;
               elsif UPPER = "INTERFACE" then
                  Lookahead_Token.Kind  := Interface_Word;
               elsif UPPER = "LOOP" then
                  Lookahead_Token.Kind  := Loop_Word;
               elsif UPPER = "CASE" then
                  Lookahead_Token.Kind  := Case_Word;
               elsif UPPER = "IF" then
                  Lookahead_Token.Kind  := If_Word;
               elsif UPPER = "WHEN" then
                  Lookahead_Token.Kind  := When_Word;
               elsif UPPER = "ACCEPT" then
                  Lookahead_Token.Kind  := Accept_Word;
               elsif UPPER = "SELECT" then
                  Lookahead_Token.Kind  := Select_Word;
               elsif UPPER = "DO" then
                  Lookahead_Token.Kind  := Do_Word;
               elsif UPPER = "PRAGMA" then
                  Lookahead_Token.Kind  := Pragma_Word;
               else
                  Lookahead_Token.Kind  := Identifier;
               end if;
            end;
         end if;
      end if;
      Buffers(Current_Buffer).Last_Sig := Lookahead_Token.Last;
         -- Save for error range indicators (else we'd have to reparse, yuck).
      return Lookahead_Token;
   end Peek_Next_Token;


   function Get_Next_Token return Token is
      -- Consume and return the next token in the ACATS test file.
   begin
      if Lookahead_Token.Line_Num = 0 then
         declare
            Temp : Token := Peek_Next_Token; -- Load the Lookahead_Token.
         begin
            -- We have to read Temp so that it is not a dead object,
            -- else some compilers might optimize the Peek_Next_Token call
            -- away.
            if Temp.Line_Num /= Lookahead_Token.Line_Num then
                raise Token_Error with
                          " Lookahead not loaded by Peek_Next_Token";
            end if;
         end;
      end if;

      if Lookahead_Token.Line_Num = Trace.Line_Number_Type'Last then
         -- End of file token. We just keep returning that until the
         --- parser stops asking. :-)
         return Lookahead_Token;
      elsif Lookahead_Token.Line_Num /= 0 then
         declare
            Temp : Token := Lookahead_Token;
         begin
            Lookahead_Token := NO_TOKEN;
            return Temp;
         end;
      else
         raise Token_Error with
         " no token to return in Get_Next_Token after calling Peek_Next_Token";
      end if;
   end Get_Next_Token;


   procedure Get_Identifier (Id_Tok    : in     Token;
                             Id_String :    out String) is
      -- Get an identifier (or expanded name) represented by Id_Tok
      -- into Id_String in upper case. Id_String is blank-padded if necessary.
   begin
      if Buffers(Id_Tok.Buffer).Line_Num /= Id_Tok.Line_Num then
         raise Token_Error with "identifier not available in buffer" &
            " at line" & Trace.Line_Number_Type'Image(Id_Tok.Line_Num);
      end if;
      Ada.Strings.Fixed.Move (Target => Id_String,
                              Source =>
         Ada.Strings.Fixed.Translate (
              Buffers(Id_Tok.Buffer).Line(Id_Tok.First .. Id_Tok.Last),
                                   Ada.Strings.Maps.Constants.Upper_Case_Map));
   end Get_Identifier;


   procedure Put_Token (Tok : in Token) is
   begin
      if Tok.Kind = EOF then
         Ada.Text_Io.Put_Line ("Kind=" & Token_Kind'Image(Tok.Kind));
      elsif Tok = NO_TOKEN then
         Ada.Text_Io.Put_Line ("Kind=NO_TOKEN");
      else
         Ada.Text_Io.Put_Line ("Kind=" & Token_Kind'Image(Tok.Kind) &
            "; line=" & Trace.Line_Number_Type'Image(Tok.Line_Num) &
            "; contents=" & Buffers(Tok.Buffer).Line(Tok.First..Tok.Last));
      end if;
   end Put_Token;


   procedure Debug_Info (Text : in String;
                         Unit_Name : in Test_Summary.Comp_Unit_Name :=
                            (others => ' ');
                         Unit_Kind : in String := "";
                         Tok : in Token) is
   begin
      if DEBUG then
         Ada.Text_Io.Put ("&& " & Text & " "); Put_Token(Tok);
         if Unit_Name /= (Test_Summary.Comp_Unit_Name'range => ' ') then
            Ada.Text_Io.Put_Line ("   For " & Unit_Kind & " " &
               Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right));
         -- else not Unit to display.
         end if;
      end if;
   end Debug_Info;

   -- The following routines define a recursive recent parser for a superset
   -- of Ada; we try to parse as little as we have to in order to identify
   -- compilation unit boundaries (so as to allow the maximum number of
   -- syntax errors), but of course allowing all syntax errors is impossible.
   -- In particular, we try to allow extra syntax where that doesn't compromise
   -- our ability to find compilation units. (For instance, we treat
   -- "return subtype_mark" as optional for functions, where it is required
   -- or not allowed.)

   Parse_Error : exception;

   procedure Skip_to_Closing_Paren is
      -- Skip to a closing paren, ignoring all that occurs in between
      -- (except, of course, nested parens). The opening paren has already
      -- been consumed upon entry; the closing paren will have been consumed
      -- on return.
      Cnt : Natural := 1;
      Tok : Token;
   begin
      loop
         Tok := Get_Next_Token;
         if Tok.Kind = LParen then
            Cnt := Cnt + 1;
         elsif Tok.Kind = RParen then
            Cnt := Cnt - 1;
            if Cnt = 0 then
               return; -- We're done.
            end if;
         elsif Tok.Kind = EOF then
            return; -- Avoid an infinite loop in an error case.
         -- else continue consuming tokens.
         end if;
      end loop;
   end Skip_to_Closing_Paren;


   type Aspect_Spec_Ending_Kind is (When_or_Semicolon,
                                    Is_or_Semicolon, Only_Is);

   procedure Skip_Aspect_Spec (Ending_Kind : Aspect_Spec_Ending_Kind) is
      -- Skip an aspect specification, ignoring all that occurs in between.
      -- The Is or semicolon will not have been consumed on return.
      -- This is more complicated than one would think, as "is" can
      -- appear in a case expression. Thus, we have to count parens,
      -- and only exit if the count is zero.
      Tok : Token;
      Cnt : Natural := 0;
   begin
      loop
         Tok := Peek_Next_Token;
         if Tok.Kind = LParen then
            Cnt := Cnt + 1;
         elsif Tok.Kind = RParen then
            Cnt := Cnt - 1;
         elsif Tok.Kind = Is_Word then
            if Cnt = 0 and then Ending_Kind /= When_or_Semicolon then
               return;
            -- else in a case expression, skip and continue.
            end if;
         elsif Tok.Kind = Semicolon and then Ending_Kind /= Only_Is then
            if Cnt = 0 then
               return;
            -- else in some expression, skip and continue. (This case probably
            -- can't happen, but better safe than sorry.)
            end if;
         elsif Tok.Kind = When_Word and then
               Ending_Kind = When_or_Semicolon then
            if Cnt = 0 then
               return;
            -- else in some expression, skip and continue. (This case probably
            -- can't happen, but better safe than sorry.)
            end if;
         elsif Tok.Kind = EOF then
            return; -- Avoid an infinite loop in an error case.
         -- else continue consuming tokens.
         end if;
         Tok := Get_Next_Token;
      end loop;
   end Skip_Aspect_Spec;


   procedure Skip_Block (Has_Declarative_Part : in Boolean);
      -- See below for details.
   procedure Parse_Package_Unit (Semi_Tok  : out Token;
                                 Unit_Kind : out
                                       Test_Summary.Compilation_Unit_Kinds;
                                 Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.
   procedure Parse_Procedure_Unit (
       Semi_Tok  : out Token;
       Unit_Kind : out Test_Summary.Compilation_Unit_Kinds;
       Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.
   procedure Parse_Function_Unit (
       Semi_Tok  : out Token;
       Unit_Kind : out Test_Summary.Compilation_Unit_Kinds;
       Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.
   procedure Parse_Protected_Unit (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.
   procedure Parse_Task_Unit (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.
   procedure Parse_Entry (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.
   procedure Parse_Accept (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name);
      -- See below for details.

   type Decl_Part_Ending_Kind is (Begin_or_End, Only_End, Only_Begin);

   procedure Skip_Declarative_Part (End_Kind : Decl_Part_Ending_Kind;
                                    Was_End  : out Boolean) is
      -- Skip a declarative part that ends as specified.
      -- This requires care, as we have to properly skip over any nested
      -- constructs that might include a Begin or End. Consumes the Begin
      -- or End.
      -- This should be called with the first tok of the declarative part
      -- ready to be read.
      Tok : Token;
      Junk : Test_Summary.Compilation_Unit_Kinds;
      More_Junk : Test_Summary.Comp_Unit_Name;
   begin
      --if DEBUG then
      --   Tok := Peek_Next_Token;
      --   Ada.Text_IO.Put_Line ("Start Skip_Decl_Part - " &
      --      "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
      --end if;
      loop
         Tok := Peek_Next_Token;
         if Tok.Kind = With_Word then
            -- This could be many things. Here, we only care if it precedes
            -- package, procedure, or function, meaning these are formal
            -- packages or subprograms. These do not include a begin or
            -- end so they can be skipped outright.
--if DEBUG then
--   Ada.Text_IO.Put_Line ("^^ found With_Word " &
--      "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
--end if;
            Tok := Get_Next_Token; -- Junk the "with"
            Tok := Peek_Next_Token;
            if Tok.Kind = Package_Word then -- Formal package.
               null; -- Skip "package".
            elsif Tok.Kind = Procedure_Word then -- Formal procedure.
               null; -- Skip "procedure".
            elsif Tok.Kind = Function_Word then -- Formal function.
               null; -- Skip "function".
            else
               goto Continue_without_Consumption;
               -- Process this new token normally.
            end if;
         -- Note: We don't specially process generics here; the normal
         -- package/subprogram cases take care of those, and the formals
         -- don't cause trouble.
         elsif Tok.Kind = Access_Word then
            -- This could be many things. Here, we only care if it precedes
            -- protected, procedure, or function, meaning these are
            -- protected or normal access-to-subprogram. These do not include
            -- a begin or end so they can be skipped outright.
--if DEBUG then
--   Ada.Text_IO.Put_Line ("^^ found Access_Word " &
--      "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
--end if;
            Tok := Get_Next_Token; -- Junk the "access".
            Tok := Peek_Next_Token;
            if Tok.Kind = Protected_Word then -- protected access
               Tok := Get_Next_Token; -- Skip "protected"
               Tok := Peek_Next_Token;
               if Tok.Kind = Procedure_Word then -- prot access-to-procedure.
                  null; -- Skip "procedure".
               elsif Tok.Kind = Function_Word then -- prot access-to-function.
                  null; -- Skip "function".
               else
                  Debug_Info ("access protected not proc/func?", Tok => Tok);
                  raise Parse_Error with "access protected not subprog, " &
                     "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
               end if;
            elsif Tok.Kind = Procedure_Word then -- access-to-procedure.
               null; -- Skip "procedure".
            elsif Tok.Kind = Function_Word then -- access-to-function.
               null; -- Skip "function".
            else
               goto Continue_without_Consumption;
               -- Process this new token normally.
            end if;
         -- Note: We don't specially process generics here; the normal
         -- package/subprogram cases take care of those, and the formals
         -- don't cause trouble.
         elsif Tok.Kind = Package_Word then
            -- Some nested package.
            Tok := Get_Next_Token; -- Discard "Package".
            Parse_Package_Unit (Tok, Junk, More_Junk);
               -- We don't need the semicolon or unit information here.
               -- But the semicolon was consumed, so we need to process
               -- the next token.
--Ada.Text_IO.Put ("!! SDP - Semi after Pack"); Put_Token (Tok);
            goto Continue_without_Consumption;

         elsif Tok.Kind = Procedure_Word then
            -- Some nested procedure. (We really only care about bodies, but
            -- we can't easily tell them from the others).
            Tok := Get_Next_Token; -- Discard "procedure".
            Parse_Procedure_Unit (Tok, Junk, More_Junk);
               -- We don't need the semicolon or unit information here.
               -- But the semicolon was consumed, so we need to process
               -- the next token.
--Ada.Text_IO.Put ("!! SDP - Semi after Proc"); Put_Token (Tok);
           goto Continue_without_Consumption;

         elsif Tok.Kind = Function_Word then
            -- Some nested function. (We really only care about bodies, but
            -- we can't easily tell them from the others).
            Tok := Get_Next_Token; -- Discard "function".
            Parse_Function_Unit (Tok, Junk, More_Junk);
               -- We don't need the semicolon or unit information here.
               -- But the semicolon was consumed, so we need to process
               -- the next token.
--Ada.Text_IO.Put ("!! SDP - Semi after Func"); Put_Token (Tok);
           goto Continue_without_Consumption;

         elsif Tok.Kind = Protected_Word then
            -- Some nested protected unit.
            Tok := Get_Next_Token; -- Discard "protected".
            Parse_Protected_Unit (Tok, More_Junk);
               -- We don't need the semicolon or unit name here.
               -- But the semicolon was consumed, so we need to process
               -- the next token.
--Ada.Text_IO.Put ("!! SDP - Semi after Prot"); Put_Token (Tok);
           goto Continue_without_Consumption;

         elsif Tok.Kind = Task_Word then
            -- Some nested task (these all contain "end").
            Tok := Get_Next_Token; -- Discard "protected".
            Parse_Task_Unit (Tok, More_Junk);
               -- We don't need the semicolon or unit name here.
               -- But the semicolon was consumed, so we need to process
               -- the next token.
--Ada.Text_IO.Put ("!! SDP - Semi after Task"); Put_Token (Tok);
           goto Continue_without_Consumption;

         elsif Tok.Kind = Entry_Word then
            -- Some entry (we only care about entry bodies, but we have to
            -- look at all of them). [Entry bodies only occur in protected
            -- bodies, but we allow them everywhere to simply this code
            -- and to allow some possible errors.]
            Tok := Get_Next_Token; -- Discard "entry".
            Parse_Entry (Tok, More_Junk);
               -- We don't need the semicolon or unit name here.
               -- But the semicolon was consumed, so we need to process
               -- the next token.
--Ada.Text_IO.Put ("!! SDP - Semi after Entry"); Put_Token (Tok);
           goto Continue_without_Consumption;

         elsif Tok.Kind = Begin_Word and End_Kind /= Only_End then
            Tok := Get_Next_Token; -- Discard "Begin".
            Was_End := False;
            return; -- We're done.

         elsif Tok.Kind = End_Word then
--Ada.Text_IO.Put ("!! SDP - Saw End"); Put_Token (Tok);
            Tok := Get_Next_Token; -- Discard "End".
            Tok := Peek_Next_Token;
            if Tok.Kind = Record_Word then -- "end record", not the end of this
                                           -- declarative part.
               null; -- Continue, skipping "Record" as we don't do anything
                     -- special for it.
            elsif Tok.Kind = Case_Word then -- "end case" (closing a variant
                                            -- part), not the end of this
                                            -- declarative part.
               null; -- Continue, skipping "Case" as we don't do anything
                     -- special for it.
            elsif End_Kind /= Only_Begin then
--Ada.Text_IO.Put ("!! SDP - Normal exit - peek "); Put_Token (Tok);
               Was_End := True;
               return; -- We're done.
            else
--Ada.Text_IO.Put ("!! SDP - Normal continue - peek "); Put_Token (Tok);
               goto Continue_without_Consumption;
               -- Process this new token normally.
            end if;

         elsif Tok.Kind = EOF then
            Was_End := False;
            return; -- Oops, quit in the case of major problems.

         -- else continue consuming the declarative part.
--else Ada.Text_IO.Put ("!! SDP- Eat"); Put_Token (Tok);
         end if;
         Tok := Get_Next_Token;
      <<Continue_without_Consumption>> null;
      end loop;
   end Skip_Declarative_Part;


   procedure Skip_Stmts_and_Handlers is
      -- Skip statements and exception handlers. End with an End word.
      -- This requires care, as we have to properly skip over any nested
      -- constructs that might include an End. Consumes the End word.
      -- This should be called with the first tok of the statements
      -- ready to be read.
      Tok : Token;
      More_Junk : Test_Summary.Comp_Unit_Name;
   begin
      loop
         Tok := Get_Next_Token;
         if Tok.Kind = End_Word then
            -- Check if this is the end of a statement or of the unit.
            Tok := Peek_Next_Token;
            if Tok.Kind = If_Word then
               null; -- end of an if statement.
            elsif Tok.Kind = Case_Word then
               null; -- end of a case statement.
            elsif Tok.Kind = Loop_Word then
               null; -- end of a loop statement.
            elsif Tok.Kind = Return_Word then
               null; -- end of an extended return statement.
            elsif Tok.Kind = Select_Word then
               null; -- end of a select statement.
            elsif Tok.Kind = Identifier then
               return; -- Found the end.
            elsif Tok.Kind = Semicolon then
               return; -- Found the end.
            else -- ???
               return;
            end if;
         elsif Tok.Kind = Accept_Word then
            -- Some accept statement (we only care when they have statements,
            -- but we have to look at all of them).
            Parse_Accept (Tok, More_Junk);
               -- We don't need the semicolon or unit name here.
         elsif Tok.Kind = Declare_Word then
            Skip_Block (Has_Declarative_Part => True);
         elsif Tok.Kind = Begin_Word then
            Skip_Block (Has_Declarative_Part => False);
         elsif Tok.Kind = EOF then
            return; -- Oops, quit in case of a major problem.
         -- else continue.
         end if;
      end loop;
   end Skip_Stmts_and_Handlers;


   procedure Skip_Block (Has_Declarative_Part : in Boolean) is
      -- Skip a block, which has a declare part if Has_Declarative_Part is
      -- True. The initial keyword (declare or begin) has been consumed.
      -- Consumes the entire block, including the closing semicolon.
      Tok : Token; Junk : Boolean;
   begin
      if DEBUG then
         if Has_Declarative_Part then
            Ada.Text_IO.Put_Line ("Saw start of block (with declare)");
         else
            Ada.Text_IO.Put_Line ("Saw start of block (no declare)");
         end if;
      end if;
      if Has_Declarative_Part then
         Skip_Declarative_Part (Only_Begin, Junk); -- Skips the Begin.
      -- else no declarative part.
      end if;
      Skip_Stmts_and_Handlers;
      Tok := Get_Next_Token;
      if Tok.Kind = Identifier then -- Skip the optional identifier.
         Tok := Get_Next_Token;
      end if;
      if Tok.Kind /= Semicolon then
         Debug_Info ("Not semi?", Tok => Tok);
         raise Parse_Error with "semicolon at end of block " &
            "missing, on line" &
            Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw end of block at line" &
            Trace.Line_Number_Type'Image(Tok.Line_Num));
      end if;
   end Skip_Block;


   procedure Parse_Package_Unit (
                    Semi_Tok  : out Token;
                    Unit_Kind : out Test_Summary.Compilation_Unit_Kinds;
                    Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse a package unit; we've seen (and discarded) the "package".
      -- This is one of five possible things: package specification,
      -- package body, package instance, package renaming, or package stub.
      -- (Stub is not a library unit, so it gets identified as a package
      -- specification.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token; Was_End : Boolean;
   begin
      -- Spec ::= package ID [aspect_spec] is
      --             decl_part -- includes the private part. We use
      --                       -- decl_part to allow illegal syntax
      --                       -- (and save work).
      --          end [ID];
      -- Body ::= package body ID [aspect_spec] is
      --             decl_part
      --          [begin statements]
      --          end [ID];
      -- In the below, <stuff> can't include a ;
      -- Renames ::= package ID renames <stuff>;
      -- Inst ::= package ID is new <stuff>;
      -- Stub ::= package body ID is separate <stuff>;

      Tok := Get_Next_Token;
      if Tok.Kind = Body_Word then -- This is a package body.
         Unit_Kind := Test_Summary.Package_Body;
         -- Discard "body":
         Tok := Get_Next_Token;
      else
         Unit_Kind := Test_Summary.Package_Specification;
            -- We might still have to change this.
      end if;

      -- Tok should contain the defining identifier/expanded name for this
      -- package unit. Save it.
      Get_Identifier (Tok, Unit_Name);
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw start of package unit " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
      end if;

      Tok := Get_Next_Token;
      if Tok.Kind = Renames_Word then
         -- Renames ::= package ID renames <stuff>; --<stuff> can't include a ;
         Unit_Kind := Test_Summary.Package_Renaming;
         -- Skip to the next semicolon:
         loop
            Tok := Get_Next_Token;
            exit when Tok.Kind = Semicolon;
         end loop;
         -- Tok is the ending semicolon of the unit.
      elsif Tok.Kind = With_Word or else Tok.Kind = Is_Word then
         if Tok.Kind = With_Word then -- Skip an aspect spec.
            Skip_Aspect_Spec (Only_Is);
            Tok := Get_Next_Token; -- Get the "is".
         end if;
         if Tok.Kind /= Is_Word then
            Debug_Info ("Not is?", Unit_Name, "package", Tok);
            raise Parse_Error with "after aspect spec, not at is," &
              "on line" &
                Trace.Line_Number_Type'Image(Tok.Line_Num);
         end if;
         Tok := Peek_Next_Token;
         if Tok.Kind = New_Word then
            -- Inst ::= package ID is new <stuff>; -- <stuff> can't include a ;
            Unit_Kind := Test_Summary.Package_Instantiation;
            -- Skip to the next semicolon:
            loop
               Tok := Get_Next_Token;
               exit when Tok.Kind = Semicolon;
            end loop;
            -- Tok is the ending semicolon of the unit.
         elsif Tok.Kind = Separate_Word then
            -- Stub ::= package ID is separate <stuff>;
            Unit_Kind := Test_Summary.Package_Specification;
            -- Skip to the next semicolon:
            loop
               Tok := Get_Next_Token;
               exit when Tok.Kind = Semicolon;
            end loop;
            -- Tok is the ending semicolon of the unit.
         elsif Test_Summary."=" (Unit_Kind,
                                 Test_Summary.Package_Specification) then
            -- Spec ::= package ID is
            --             decl_part -- includes the private part. We use
            --                       -- decl_part to allow illegal bodies.
            --          end [ID];
            Skip_Declarative_Part (Only_End, Was_End); -- Skips the End.
            Tok := Get_Next_Token;
            if Tok.Kind = Identifier then -- Skip the optional identifier.
               Tok := Get_Next_Token;
            end if;
            if Tok.Kind /= Semicolon then
                Debug_Info ("Not semi?", Unit_Name, "package", Tok);
               raise Parse_Error with "semicolon at end of package spec " &
                  "missing, on line" &
                   Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
         else -- package body.
            -- Body ::= package body ID is
            --             decl_part
            --          [begin statements]
            --          end [ID];
            Skip_Declarative_Part (Begin_or_End, Was_End);
            if not Was_End then -- Begin, already consumed.
               Skip_Stmts_and_Handlers; -- Eats the End.
            -- else end already consumed.
            end if;
            Tok := Get_Next_Token;
            if Tok.Kind = Identifier then -- Skip the optional identifier.
               Tok := Get_Next_Token;
            end if;
            if Tok.Kind /= Semicolon then
                Debug_Info ("Not semi?", Unit_Name, "package", Tok);
               raise Parse_Error with "semicolon at end of package body " &
                  "missing, on line" &
                   Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
         end if;
      else -- ???
         Debug_Info ("Not is/renames?", Unit_Name, "package", Tok);
         raise Parse_Error with "package unit without is or renames " &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw end of function " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Semi_Tok.Line_Num) &
            "; is a " & Test_Summary.Compilation_Unit_Kinds'Image(Unit_Kind));
      end if;
   end Parse_Package_Unit;


   procedure Parse_Package_Library_Unit (First_Tok : Token) is
      -- Parse a package library unit; we've seen (and discarded) the
      -- "package".
      -- This is one of four possible things: package specification,
      -- package body, package instance, or package renaming.
      -- This routine writes a Compilation_Unit record when done.
      -- First_Tok is the first token of the compilation unit; this typically
      -- will be part of the context clause (don't assume anything about what
      -- it is).
      Semi   : Token;
      Result : Test_Summary.Info_Record(Test_Summary.Compilation_Unit);
   begin
      -- Set up the parts of the Result that we know:
      Result.Source_Name := Simple_Source_Name;
      Result.Start_Line  := First_Tok.Line_Num;
      Result.Start_Position := First_Tok.First;
      Result.Is_Main := False; -- No main packages
      -- End_Line and End_Position TBD.
      -- Unit_Kind and Unit_Name TBD.
      -- Optional TBD.

      Parse_Package_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);

      -- Here, Semi contains the ending semicolon of the unit.
      Result.End_Line  := Semi.Line_Num;
      Result.End_Position := Semi.Last;
      Result.Optional :=
         Special_Handling.Optional_Unit_for_Test (Simple_Source_Name,
                                  Result.Unit_Name, Result.Unit_Kind);
      Test_Summary.Write_Summary_Record (Summary_File, Result);
   end Parse_Package_Library_Unit;


   procedure Parse_Procedure_Unit (
       Semi_Tok  : out Token;
       Unit_Kind : out Test_Summary.Compilation_Unit_Kinds;
       Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse a Procedure unit; we've seen (and discarded) the "Procedure".
      -- This is one of seven possible things: procedure specification,
      -- abstract procedure, null procedure, procedure body,
      -- procedure instance, procedure renaming, or procedure stub.
      -- (Note: Three of these aren't library units, and thus we use
      -- procedure specification as the unit kind for them.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token;
      Was_End : Boolean;
   begin
      -- Spec ::= procedure ID [params] [aspect_spec];
      -- Body ::= procedure ID [params] [aspect_spec] is
      --             decl_part
      --          begin
      --             statements
      --          end [ID];
      -- Null ::= procedure ID [params] is null [aspect_spec];
      -- Abst ::= procedure ID [params] is abstract [aspect_spec];
      -- Ren  ::= procedure ID [params] renames <stuff>;
      --           -- <stuff> cannot contain a semicolon.
      -- Inst ::= procedure ID is new <stuff>;
      --           -- <stuff> cannot contain a semicolon.
      -- Stub ::= procedure ID [params] is separate [aspect_spec];
      --
      -- Note: We'll also come here for an access-to-procedure type; that
      -- looks like
      --      ::= access procedure [params] [aspect_spec];
      -- and we can process that as a specification with sufficient care.

      Tok := Get_Next_Token;
      -- Tok should contain the defining identifier/expanded name for this
      -- procedure unit unless this is an access-to-procedure type. Save it.
      if Tok.Kind = Identifier then
         Get_Identifier (Tok, Unit_Name);
         if DEBUG then
            Ada.Text_IO.Put_Line ("Saw start of procedure unit " &
               Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
               " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
         end if;
         Tok := Get_Next_Token; -- Get next token.
      elsif Tok.Kind /= LParen and then
            Tok.Kind /= With_Word and then
            Tok.Kind /= Semicolon then
         Ada.Strings.Fixed.Move (Target => Unit_Name, Source => "BAD");
         Debug_Info ("Not ID/(/with/;?", Unit_Name, "procedure", Tok);
         raise Parse_Error with "header, not at Id, (, ;, or with," &
           "on line" &
            Trace.Line_Number_Type'Image(Tok.Line_Num);
      else -- just leave the tok where it is.
         Ada.Strings.Fixed.Move (Target => Unit_Name, Source => "ACCESS");
            -- Hope no one is using this for anything important.
      end if;
      if Tok.Kind = LParen then
         -- There is a formal part; it has semicolons that will confuse us,
         -- so we skip the entire formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;

      if Tok.Kind = Renames_Word then
         -- Ren  ::= procedure ID [params] renames <stuff>;
         --           -- <stuff> cannot contain a semicolon.
         Unit_Kind := Test_Summary.Procedure_Renaming;
         -- Skip to the next semicolon:
         loop
            Tok := Get_Next_Token;
            exit when Tok.Kind = Semicolon;
         end loop;
         -- Tok is the ending semicolon of the unit.
      elsif Tok.Kind = With_Word or else
            Tok.Kind = Is_Word or else
            Tok.Kind = Semicolon then
         if Tok.Kind = With_Word then -- Skip an aspect spec.
            Skip_Aspect_Spec (Is_or_Semicolon);
            Tok := Get_Next_Token; -- Get the Is or semicolon.
         end if;

         if Tok.Kind = Semicolon then
            -- Spec ::= procedure ID [params] [aspect_spec];
            Unit_Kind := Test_Summary.Procedure_Specification;
            -- Tok is the ending semicolon of the unit.
         elsif Tok.Kind /= Is_Word then
            Debug_Info ("Not is/;?", Unit_Name, "procedure", Tok);
            raise Parse_Error with "after aspect spec, not at is or ;," &
              "on line" &
                Trace.Line_Number_Type'Image(Tok.Line_Num);
         else -- IS
            Tok := Peek_Next_Token;
            if Tok.Kind = New_Word then
               -- Inst ::= procedure ID is new <stuff>;
               --           -- <stuff> cannot contain a semicolon.
               Unit_Kind := Test_Summary.Procedure_Instantiation;
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            elsif Tok.Kind = Null_Word then
               -- Null ::= procedure ID [params] is null [aspect_spec];
               --           -- [aspect_spec] cannot contain a semicolon.
               Unit_Kind := Test_Summary.Procedure_Specification;
                  -- Not a kind of library unit, so we have to use the closest.
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            elsif Tok.Kind = Abstract_Word then
               -- Abst ::= procedure ID [params] is abstract [aspect_spec];
               --           -- [aspect_spec] cannot contain a semicolon.
               Unit_Kind := Test_Summary.Procedure_Specification;
                  -- Not a kind of library unit, so we have to use the closest.
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            elsif Tok.Kind = Separate_Word then
               -- Stub ::= procedure ID [params] is separate [aspect_spec];
               --           -- [aspect_spec] cannot contain a semicolon.
               Unit_Kind := Test_Summary.Procedure_Specification;
                  -- Not a kind of library unit, so we have to use the closest.
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            else
               -- Body ::= procedure ID [params] [aspect_spec] is
               --             decl_part
               --          begin
               --             statements
               --          end [ID];
               Unit_Kind := Test_Summary.Procedure_Body;
               Skip_Declarative_Part (Only_Begin, Was_End); -- Skips the Begin.
               Skip_Stmts_and_Handlers; -- Eats the End.
               Tok := Get_Next_Token;
               if Tok.Kind = Identifier then -- Skip the optional identifier.
                  Tok := Get_Next_Token;
               end if;
               if Tok.Kind /= Semicolon then
                  Debug_Info ("Not semi?", Unit_Name, "procedure", Tok);
                  raise Parse_Error with "semicolon at end of procedure " &
                  "body missing, on line" &
                   Trace.Line_Number_Type'Image(Tok.Line_Num);
               end if;
            end if;
         end if;
      else -- ???
         Debug_Info ("Not is/;/renames?", Unit_Name, "procedure", Tok);
         raise Parse_Error with "procedure unit without is, ;, or renames " &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw end of procedure " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Semi_Tok.Line_Num) &
            "; is a " & Test_Summary.Compilation_Unit_Kinds'Image(Unit_Kind));
      end if;
   end Parse_Procedure_Unit;


   procedure Parse_Procedure_Library_Unit (First_Tok : Token) is
      -- Parse a Procedure library unit; we've seen (and discarded) the
      -- "Procedure".
      -- This is one of four possible things: procedure specification,
      -- procedure body, procedure instance, or procedure renaming.
      -- This routine writes a Compilation_Unit record when done.
      -- First_Tok is the first token of the compilation unit; this typically
      -- will be part of the context clause (don't assume anything about what
      -- it is).
      Semi   : Token;
      Result : Test_Summary.Info_Record(Test_Summary.Compilation_Unit);
   begin
      -- Set up the parts of the Result that we know:
      Result.Source_Name := Simple_Source_Name;
      Result.Start_Line  := First_Tok.Line_Num;
      Result.Start_Position := First_Tok.First;
      -- End_Line and End_Position TBD.
      -- Unit_Kind and Unit_Name TBD.
      -- Optional and Main TBD.

      Parse_Procedure_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);

      -- Here, Semi contains the ending semicolon of the unit.
      Result.End_Line  := Semi.Line_Num;
      Result.End_Position := Semi.Last;
      Result.Optional :=
         Special_Handling.Optional_Unit_for_Test (Simple_Source_Name,
                                  Result.Unit_Name, Result.Unit_Kind);
      -- Main subprogram naming rules are given in 4.3.3 of the Users Guide.
      if Simple_Source_Name (8..12) = ".A   " then
         -- A modern test in one part. The main is a procedure that
         -- matches the test name.
         Result.Is_Main := Simple_Source_Name (1..7) & "   " =
            Result.Unit_Name (1..10);
      elsif Simple_Source_Name (8..12) = ".AU  " then
         -- A modern test in one part using UTF-8 encoding. The main is a
         -- procedure that matches the test name.
         Result.Is_Main := Simple_Source_Name (1..7) & "   " =
            Result.Unit_Name (1..10);
      elsif Simple_Source_Name (9..12) = ".A  " then
         -- A modern test in multiple parts. The main is not in this file.
         Result.Is_Main := False;
      elsif Simple_Source_Name (9..12) = ".AM " then
         -- A modern test in multiple parts. The main is a procedure that
         -- matches the file name (sans extension).
         Result.Is_Main := Simple_Source_Name (1..8) & "  " =
            Result.Unit_Name (1..10);
         if not Result.Is_Main then
             Ada.Text_IO.Put_Line ("?? Not Main in main file with SSN=" &
                Simple_Source_Name (1..8) & "  ; Unit_Name=" &
                Result.Unit_Name (1..10) & "; Is_Main=" &
                Boolean'Image(Result.Is_Main));
             -- This can be OK if there are multiple units in this .AM
             -- file, but usually it means that the main subprogram has
             -- the wrong name.
         end if;
      elsif Simple_Source_Name (8..12) = ".ADA " or else
            Simple_Source_Name (8..12) = ".DEP " or else
            Simple_Source_Name (8..12) = ".ADT " then
         -- A legacy test in one part. The main is a procedure that
         -- matches the test name.
         Result.Is_Main := Simple_Source_Name (1..7) & "   " =
            Result.Unit_Name (1..10);
      elsif Simple_Source_Name (9..12) = ".ADA" or else
            Simple_Source_Name (9..12) = ".DEP" or else
            Simple_Source_Name (9..12) = ".ADT" then
         -- A legacy test in multiple parts. The main is a procedure that
         -- matches the file name (sans extension) followed by M.
         Result.Is_Main := Simple_Source_Name (1..8) & "M " =
            Result.Unit_Name (1..10);
      else -- Something weird that isn't an ACATS test (or was not processed).
         Result.Is_Main := False;
      end if;
      Test_Summary.Write_Summary_Record (Summary_File, Result);
   end Parse_Procedure_Library_Unit;


   procedure Parse_Function_Unit (
       Semi_Tok  : out Token;
       Unit_Kind : out Test_Summary.Compilation_Unit_Kinds;
       Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse a function unit; we've seen (and discarded) the "function".
      -- This is one of seven possible things: function specification,
      -- abstract function, expression function, function body,
      -- function instance, function renaming, or function stub.
      -- (Note: Three of these aren't library units, and thus we use
      -- function specification as the unit kind for them.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token;
      Was_End : Boolean;
   begin
     -- Spec ::= function ID [params] return subtype [aspect_spec];
     -- Body ::= function ID [params] return subtype [aspect_spec] is
     --             decl_part
     --          begin
     --             statements
     --          end [ID];
     -- Expr ::= function ID [params] return subtype is (expr) [aspect_spec];
     -- Abst ::= function ID [params] return subtype is abstract [aspect_spec];
     -- Ren  ::= function ID [params] return subtype renames <stuff>;
     --           -- <stuff> cannot contain a semicolon.
     -- Inst ::= function ID is new <stuff>;
     --           -- <stuff> cannot contain a semicolon.
     -- Stub ::= function ID [params] return subtype is separate [aspect_spec];
     -- Notes: when renaming a generic function, the syntax does not allow
     -- parameters or "return".
     -- For all of these, "ID" can also be a string literal (operator symbol).
     --
     -- We'll also come here for an access-to-function type; that
     -- looks like
     --      ::= access function [params] return subtype [aspect_spec];
     -- and we can process that as a specification with sufficient care.

      Tok := Get_Next_Token;
      -- Tok should contain the defining identifier/expanded name for this
      -- function unit unless this is an access-to-function type. Save it.
      if Tok.Kind = Identifier or else
         Tok.Kind = String_Literal then
         Get_Identifier (Tok, Unit_Name);
         if DEBUG then
            Ada.Text_IO.Put_Line ("Saw start of function unit " &
               Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
               " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
         end if;
         Tok := Get_Next_Token; -- Get next token.
--if Tok.Kind /= Identifier then -- Better be a string literal for an Opsym.
--   Ada.Text_Io.Put      ("&& Opsym! "); Put_Token(Tok);
--end if;
      elsif Tok.Kind /= LParen and then
            Tok.Kind /= Return_Word and then
            Tok.Kind /= With_Word and then
            Tok.Kind /= Semicolon then
         Ada.Strings.Fixed.Move (Target => Unit_Name, Source => "BAD");
         Debug_Info ("Not ID/opsym/(/return/with/;?", Unit_Name,
             "function", Tok);
         raise Parse_Error with "header, not at Id, Opsym, (, ;, return, " &
            "or with, on line" &
            Trace.Line_Number_Type'Image(Tok.Line_Num);
      else -- just leave the tok where it is.
         Ada.Strings.Fixed.Move (Target => Unit_Name, Source => "ACCESS");
            -- Hope no one is using this for anything important.
      end if;
      if Tok.Kind = LParen then
         -- There is a formal part; it has semicolons that will confuse us,
         -- so we skip the entire formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;
      if Tok.Kind = Return_Word then
         -- There is a return subtype. This is required except for
         -- instances and renames of generic functions, for which it is
         -- not allowed. We split the differencee and treat it as optional.
         Tok := Get_Next_Token; -- Toss the return.
         loop
            -- One of the following should follow the return:
            exit when Tok.Kind = Is_Word;
            exit when Tok.Kind = Semicolon;
            exit when Tok.Kind = Renames_Word;
            exit when Tok.Kind = With_Word;
            exit when Tok.Kind = EOF; -- In case of disaster.
            Tok := Get_Next_Token; -- Skip the token.
         end loop;
      end if;
      if Tok.Kind = Renames_Word then
         -- Ren  ::= function ID [params] return subtype renames <stuff>;
         --           -- <stuff> cannot contain a semicolon.
         Unit_Kind := Test_Summary.Function_Renaming;
         -- Skip to the next semicolon:
         loop
            Tok := Get_Next_Token;
            exit when Tok.Kind = Semicolon;
         end loop;
         -- Tok is the ending semicolon of the unit.

      elsif Tok.Kind = With_Word or else
            Tok.Kind = Is_Word or else
            Tok.Kind = Semicolon then
         if Tok.Kind = With_Word then -- Skip an aspect spec.
            Skip_Aspect_Spec (Is_or_Semicolon);
            Tok := Get_Next_Token; -- Get the Is or semicolon.
         end if;
         if Tok.Kind = Semicolon then
            -- Spec ::= function ID [params] return subtype [aspect_spec];
            Unit_Kind := Test_Summary.Function_Specification;
            -- Tok is the ending semicolon of the unit.
         elsif Tok.Kind /= Is_Word then
            Debug_Info ("Not is/;?", Unit_Name, "function", Tok);
            raise Parse_Error with "after func aspect spec, not at is or ;," &
              "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
         else -- IS
            Tok := Peek_Next_Token;
            if Tok.Kind = New_Word then
               -- Inst ::= function ID is new <stuff>;
               --           -- <stuff> cannot contain a semicolon.
               Unit_Kind := Test_Summary.Function_Instantiation;
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            elsif Tok.Kind = LParen then
               -- Expr ::= function ID [params] return subtype
               --                           is (expr) [aspect_spec];
               --           -- [aspect_spec] cannot contain a semicolon.
               Unit_Kind := Test_Summary.Function_Specification;
                  -- Not a kind of library unit, so we have to use the closest.
               Tok := Get_Next_Token; -- Eat the LParen.
               Skip_to_Closing_Paren; -- RParen is consumed.
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            elsif Tok.Kind = Abstract_Word then
               -- Abst ::= function ID [params] return subtype
               --                         is abstract [aspect_spec];
               --           -- [aspect_spec] cannot contain a semicolon.
               Unit_Kind := Test_Summary.Function_Specification;
                  -- Not a kind of library unit, so we have to use the closest.
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            elsif Tok.Kind = Separate_Word then
               -- Stub ::= function ID [params] return subtype
               --                         is separate [aspect_spec];
               --           -- [aspect_spec] cannot contain a semicolon.
               Unit_Kind := Test_Summary.Function_Specification;
                  -- Not a kind of library unit, so we have to use the closest.
               -- Skip to the next semicolon:
               loop
                  Tok := Get_Next_Token;
                  exit when Tok.Kind = Semicolon;
               end loop;
               -- Tok is the ending semicolon of the unit.
            else
               -- Body ::= function ID [params] return subtype [aspect_spec] is
               --            decl_part
               --         begin
               --            statements
               --         end [ID];
               Unit_Kind := Test_Summary.Function_Body;
               Skip_Declarative_Part (Only_Begin, Was_End); -- Skips the Begin.
               Skip_Stmts_and_Handlers; -- Eats the End.
               Tok := Get_Next_Token;
               if (Tok.Kind = Identifier or else
                   Tok.Kind = String_Literal) then
                  -- Skip the optional identifier or operator symbol.
                  Tok := Get_Next_Token;
               end if;
               if Tok.Kind /= Semicolon then
                  Debug_Info ("Not semi?", Unit_Name, "function", Tok);
                  raise Parse_Error with "semicolon at end of function " &
                  "body missing, on line" &
                   Trace.Line_Number_Type'Image(Tok.Line_Num);
               end if;
            end if;
         end if;
      else -- ???
         Debug_Info ("Not is/;/renames?", Unit_Name, "function", Tok);
         raise Parse_Error with "function unit without is, ;, or renames " &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw end of function " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Semi_Tok.Line_Num) &
            "; is a " & Test_Summary.Compilation_Unit_Kinds'Image(Unit_Kind));
      end if;
   end Parse_Function_Unit;


   procedure Parse_Function_Library_Unit (First_Tok : Token) is
      -- Parse a Function library unit; we've seen (and discarded) the
      -- "Function".
      -- This is one of four possible things: function specification,
      -- function body, function instance, or function renaming.
      -- This routine writes a Compilation_Unit record when done.
      -- First_Tok is the first token of the compilation unit; this typically
      -- will be part of the context clause (don't assume anything about what
      -- it is).
      Semi   : Token;
      Result : Test_Summary.Info_Record(Test_Summary.Compilation_Unit);
   begin
      -- Set up the parts of the Result that we know:
      Result.Source_Name := Simple_Source_Name;
      Result.Start_Line  := First_Tok.Line_Num;
      Result.Start_Position := First_Tok.First;
      -- End_Line and End_Position TBD.
      -- Unit_Kind and Unit_Name TBD.
      -- Optional and Main TBD.

      Parse_Function_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);

      -- Here, Semi contains the ending semicolon of the unit.
      Result.End_Line  := Semi.Line_Num;
      Result.End_Position := Semi.Last;
      Result.Optional :=
         Special_Handling.Optional_Unit_for_Test (Simple_Source_Name,
                                  Result.Unit_Name, Result.Unit_Kind);
      Result.Is_Main := False; -- No function main subprograms in the ACATS.

      Test_Summary.Write_Summary_Record (Summary_File, Result);
   end Parse_Function_Library_Unit;


   procedure Parse_Protected_Unit (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse a protected unit; we've seen (and discarded) the "protected".
      -- This is one of four possible things: single protected specification,
      -- protected type, protected body, or protected stub.
      -- (Note: None of these things are library units, so we don't have a
      -- unit kind.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token;
      Was_End : Boolean;
      Saw_Body : Boolean := False;
      Junk : Test_Summary.Compilation_Unit_Kinds;
      More_Junk : Test_Summary.Comp_Unit_Name;
   begin
      -- single ::= protected ID [aspect_spec] is
      --              [new interface_list with]
      --              {protected_operation} -- No bodies
      --            private
      --              {protected_element}
      --            end [ID];
      -- type   ::= protected type ID [discrims] [aspect_spec] is
      --              [new interface_list with]
      --              {protected_operation} -- No bodies
      --            private
      --              {protected_element}
      --            end [ID];
      -- body   ::= protected body ID [aspect_spec] is
      --              {protected_operation_item} -- Bodies
      --            end [ID];
      -- stub   ::= protected body ID is separate [aspect_spec];
      -- In addition, protected interface types will come here, this will look
      -- like:
      --    ::= protected interface {and ID} [aspect_spec];
      -- We'll also get access to protected subprogram here:
      --    ::= access protected procedure ...;
      --    ::= access protected function ...;

      Tok := Peek_Next_Token;
      if Tok.Kind = Type_Word then
         Tok := Get_Next_Token; -- Discard "type".
      elsif Tok.Kind = Body_Word then
         Tok := Get_Next_Token; -- Discard "body".
         Saw_Body := True;
      elsif Tok.Kind = Interface_Word then
         -- An interface type declaration, not a protected unit at all.
         Ada.Strings.Fixed.Move (Target => Unit_Name, Source => "INTERFACE");
            -- Hope no one is using this for anything important.
         -- We can just skip to the following semicolon.
         loop
            Tok := Get_Next_Token;
            exit when Tok.Kind = Semicolon;
         end loop;
         -- Tok is the ending semicolon of the unit.
         goto Completed;
      elsif Tok.Kind = Procedure_Word then
         -- An access-to-protected procedure declaration, not a protected unit
         -- at all.
         Ada.Strings.Fixed.Move (Target => Unit_Name,
            Source => "PROT_ACCESS_PROC");
            -- Hope no one is using this for anything important.
         -- Use the procedure parser for this:
         Tok := Get_Next_Token; -- Discard "procedure".
         Parse_Procedure_Unit (Tok, Junk, More_Junk);
         -- Tok is the ending semicolon of the unit.
         goto Completed;
      elsif Tok.Kind = Function_Word then
         -- An access-to-protected function declaration, not a protected unit
         -- at all.
         Ada.Strings.Fixed.Move (Target => Unit_Name,
            Source => "PROT_ACCESS_FUNC");
            -- Hope no one is using this for anything important.
         -- Use the function parser for this:
         Tok := Get_Next_Token; -- Discard "function".
         Parse_Function_Unit (Tok, Junk, More_Junk);
         -- Tok is the ending semicolon of the unit.
         goto Completed;
      -- else an ID, hopefully.
      end if;

      Tok := Get_Next_Token;
      -- Tok should contain the defining identifier/expanded name for this
      -- protected unit. Save it.
      Get_Identifier (Tok, Unit_Name);
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw start of protected unit " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
      end if;

      Tok := Get_Next_Token; -- Get next token.
      if Tok.Kind = LParen then
         -- There is a formal part; it has semicolons that will confuse us,
         -- so we skip the entire formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;

      if Tok.Kind = With_Word then -- Skip an aspect spec.
         Skip_Aspect_Spec (Is_or_Semicolon);
         Tok := Get_Next_Token; -- Get the Is.
      end if;

      if Tok.Kind /= Is_Word then -- ???
         Debug_Info ("Not is?", Unit_Name, "protected", Tok);
         raise Parse_Error with "protected unit without is " &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;

      Tok := Peek_Next_Token; -- See what follows the IS.

      if Tok.Kind = Separate_Word then
         -- Skip to the next semicolon:
         loop
            Tok := Get_Next_Token;
            exit when Tok.Kind = Semicolon;
         end loop;
         -- Tok is the ending semicolon of the unit.
      else
         if not Saw_Body then
            -- A protected type or single protected object.
            -- The contents of these cannot include an "end", so we can
            -- just skip to the end without all of the other complications.
            Tok := Get_Next_Token; -- Get, rather than Peek, the token in
                                   -- case this is already the End.
            while Tok.Kind /= End_Word loop
               exit when Tok.Kind = EOF; -- In case of disaster.
               Tok := Get_Next_Token;
            end loop;
            -- Here, Tok = End.
            if Tok.Kind /= End_Word then -- ???
               Debug_Info ("Not end?", Unit_Name, "protected", Tok);
               raise Parse_Error with "protected unit without end " &
                  "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
         else -- Body.
            -- A protected body.
            Skip_Declarative_Part (Only_End, Was_End); -- Skips the End.
         end if;

         Tok := Get_Next_Token; -- Get the token after the "end".

         if Tok.Kind = Identifier then -- Skip the optional identifier.
            Tok := Get_Next_Token;
         end if;
         if Tok.Kind /= Semicolon then
            Debug_Info ("Not semi?", Unit_Name, "protected", Tok);
            raise Parse_Error with "semicolon at end of protected " &
              "unit missing, on line" &
              Trace.Line_Number_Type'Image(Tok.Line_Num);
         end if;
      end if;

   <<Completed>>
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw end of protected unit " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Semi_Tok.Line_Num));
      end if;
   end Parse_Protected_Unit;


   procedure Parse_Task_Unit (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse a task unit; we've seen (and discarded) the "Task".
      -- This is one of four possible things: single task specification,
      -- task type, task body, or task stub.
      -- (Note: None of these things are library units, so we don't have a
      -- unit kind.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token;
      Was_End : Boolean;
      Saw_Body : Boolean := False;
   begin
      -- single ::= task ID [aspect_spec] [is
      --              [new interface_list with]
      --              {task_item} -- No bodies
      --            private
      --              {task_item}
      --            end [ID]];
      -- type   ::= task type ID [discrims] [aspect_spec] [is
      --              [new interface_list with]
      --              {task_item} -- No bodies
      --            private
      --              {task_item}
      --            end [ID]];
      -- body   ::= task body ID [aspect_spec] is
      --              decl_part
      --            begin
      --              statements
      --            end [ID];
      -- stub   ::= task body ID is separate [aspect_spec];
      -- In addition, task interface types will come here, this will look
      -- like:
      --    ::= task interface {and ID} [aspect_spec];

      Tok := Peek_Next_Token;
      if Tok.Kind = Type_Word then
         Tok := Get_Next_Token; -- Discard "type".
      elsif Tok.Kind = Body_Word then
         Tok := Get_Next_Token; -- Discard "body".
         Saw_Body := True;
      elsif Tok.Kind = Interface_Word then
         -- An interface type declaration, not a task unit at all.
         Ada.Strings.Fixed.Move (Target => Unit_Name, Source => "INTERFACE");
            -- Hope no one is using this for anything important.
         -- We can just skip to the following semicolon.
         loop
            Tok := Get_Next_Token;
            exit when Tok.Kind = Semicolon;
         end loop;
         -- Tok is the ending semicolon of the unit.
         goto Completed;
      -- else an ID, hopefully.
      end if;

      Tok := Get_Next_Token;
      -- Tok should contain the defining identifier/expanded name for this
      -- task unit. Save it.
      Get_Identifier (Tok, Unit_Name);
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw start of task unit " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
      end if;

      Tok := Get_Next_Token; -- Get next token.
      if Tok.Kind = LParen then
         -- There is a formal part; it has semicolons that will confuse us,
         -- so we skip the entire formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;

      if Tok.Kind = With_Word then -- Skip an aspect spec.
         Skip_Aspect_Spec (Is_or_Semicolon);
         Tok := Get_Next_Token; -- Get the Is.
      end if;

      if Tok.Kind = Semicolon then
         -- A task declaration with no entries. Weird but legal.
         null; -- We're done here.
      elsif Tok.Kind /= Is_Word then -- ???
         Debug_Info ("Not is/;?", Unit_Name, "task", Tok);
         raise Parse_Error with "task unit without is " &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      else -- Saw is, parse rest of task

         Tok := Peek_Next_Token; -- See what's after the "is".

         if Tok.Kind = Separate_Word then
            -- Skip to the next semicolon:
            loop
               Tok := Get_Next_Token;
               exit when Tok.Kind = Semicolon;
            end loop;
            -- Tok is the ending semicolon of the unit.
         else
            if not Saw_Body then
               -- A task type or single task object.
               -- The contents of these cannot include an "end", so we can
               -- just skip to the end without all of the other complications.
               Tok := Get_Next_Token; -- Get the token, in case this is
                                      -- already the "End".
               while Tok.Kind /= End_Word loop
                  exit when Tok.Kind = EOF; -- In case of disaster.
                  Tok := Get_Next_Token;
               end loop;
               -- Here, Tok = End.
               if Tok.Kind /= End_Word then -- ???
                  Debug_Info ("Not end?", Unit_Name, "task", Tok);
                  raise Parse_Error with "task unit without end " &
                     "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
               end if;
            else -- Body.
               -- A task body.
               Skip_Declarative_Part (Only_Begin, Was_End); -- Skips the Begin.
               Skip_Stmts_and_Handlers; -- Eats the End.
            end if;

            Tok := Get_Next_Token; -- Get the token after the "end".

            if Tok.Kind = Identifier then -- Skip the optional identifier.
               Tok := Get_Next_Token;
            end if;
            if Tok.Kind /= Semicolon then
               Debug_Info ("Not semi?", Unit_Name, "task", Tok);
               raise Parse_Error with "semicolon at end of task " &
                 "unit missing, on line" &
                 Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
         end if;
      end if;
  <<Completed>>
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw end of task unit " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Semi_Tok.Line_Num));
      end if;
   end Parse_Task_Unit;


   procedure Parse_Entry (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse an entry; we've seen (and discarded) the "Entry".
      -- This is one of two possible things: entry declaration or
      -- entry body.
      -- (Note: None of these things are library units, so we don't have a
      -- unit kind.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token;
      Was_End : Boolean;
   begin
      -- decl ::= entry ID [(family)] [(parameters)] [aspect_spec];
      -- body ::= entry ID [(family)] [(parameters)] [aspect_spec] when expr is
      --             decl_part
      --          begin
      --             statements
      --          end [ID];
      Tok := Get_Next_Token;
      -- Tok should contain the defining identifier/expanded name for this
      -- entry. Save it.
      Get_Identifier (Tok, Unit_Name);
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw start of entry " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
      end if;
      Tok := Get_Next_Token; -- Get next token.
      if Tok.Kind = LParen then
         -- There is a family or formal part; it has semicolons that will
         -- confuse us, so we skip the entire family or formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;
      if Tok.Kind = LParen then
         -- There is a family and formal part; it has semicolons that will
         -- confuse us, so we skip the entire formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;
      if Tok.Kind = With_Word then
         -- There is an aspect spec.
         Skip_Aspect_Spec (When_or_Semicolon);
         Tok := Get_Next_Token; -- Get the When or semicolon.
      end if;
      if Tok.Kind = Semicolon then
         -- decl ::= entry ID [(family)] [(parameters)] [aspect_spec];
         -- Entry declaration.
         null; -- Tok is the ending semicolon of the entry, nothing else to do.
      elsif Tok.Kind = When_Word then
         -- body ::=
         --       entry ID [(family)] [(parameters)] [aspect_spec] when expr is
         --          decl_part
         --       begin
         --          statements
         --       end [ID];
         -- Skip to and including "Is":
         Skip_Aspect_Spec (Only_Is);
            -- This isn't an aspect, but this routine does what we need. We
            -- need to use this routine in case (pun intended) of a case
            -- expression in the barrier, which contains an "is".
         Tok := Get_Next_Token; -- Get the Is.

         Skip_Declarative_Part (Only_Begin, Was_End); -- Skips the Begin.
         Skip_Stmts_and_Handlers; -- Eats the End.
         Tok := Get_Next_Token;
         if Tok.Kind = Identifier then -- Skip the optional identifier.
            Tok := Get_Next_Token;
         end if;
         if Tok.Kind /= Semicolon then
            Debug_Info ("Not semi?", Unit_Name, "entry body", Tok);
            raise Parse_Error with "semicolon at end of entry " &
               "body missing, on line" &
                   Trace.Line_Number_Type'Image(Tok.Line_Num);
         end if;
      else -- ??
         Debug_Info ("Not when/;?", Unit_Name, "entry", Tok);
         raise Parse_Error with "after aspect spec, not at when or ;," &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
   end Parse_Entry;


   procedure Parse_Accept (
       Semi_Tok  : out Token;
       Unit_Name : out Test_Summary.Comp_Unit_Name) is
      -- Parse an entry; we've seen (and discarded) the "Accept".
      -- This is an accept statement, with or without statements.
      -- (Note: This is not a library unit, so we don't have a unit kind.)
      -- Semi_Tok will have the details of the closing semicolon.
      Tok : Token;
      Was_End : Boolean;
   begin
      -- accp ::= accept ID [(family)] [(parameters)] [do
      --             statements
      --          end [ID]];
      Tok := Get_Next_Token;
      -- Tok should contain the defining identifier/expanded name for this
      -- accept. Save it.
      Get_Identifier (Tok, Unit_Name);
      if DEBUG then
         Ada.Text_IO.Put_Line ("Saw start of accept stmt " &
            Ada.Strings.Fixed.Trim (Unit_Name, Ada.Strings.Right) &
            " at line" & Trace.Line_Number_Type'Image(Tok.Line_Num));
      end if;
      Tok := Get_Next_Token; -- Get next token.
      if Tok.Kind = LParen then
         -- There is a family or formal part; it has semicolons that will
         -- confuse us, so we skip the entire family or formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;
      if Tok.Kind = LParen then
         -- There is a family and formal part; it has semicolons that will
         -- confuse us, so we skip the entire formal part.
         Skip_to_Closing_Paren; -- RParen is consumed.
         Tok := Get_Next_Token;
      end if;
      if Tok.Kind = Do_Word then -- Has statements.
         -- accp ::= accept ID [(family)] [(parameters)] do
         --             statements
         --          end [ID];
         Skip_Stmts_and_Handlers; -- Eats the End.
         Tok := Get_Next_Token;
         if Tok.Kind = Identifier then -- Skip the optional identifier.
            Tok := Get_Next_Token;
         end if;
         if Tok.Kind /= Semicolon then
            Debug_Info ("Not semi?", Unit_Name, "entry body", Tok);
            raise Parse_Error with "semicolon at end of entry " &
               "body missing, on line" &
                   Trace.Line_Number_Type'Image(Tok.Line_Num);
         end if;
      elsif Tok.Kind = Semicolon then -- No statements.
         -- accp ::= accept ID [(family)] [(parameters)];
         null; -- Tok is the ending semicolon of the accept,
               -- nothing else to do.
      else -- ??
         Debug_Info ("Not do/;?", Unit_Name, "accept", Tok);
         raise Parse_Error with "after params, not at do or ;," &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      -- Here, Tok contains the closing semicolon. Save it:
      Semi_Tok := Tok;
   end Parse_Accept;


   procedure Parse_Generic_Library_Unit (First_Tok : Token) is
      -- Parse a generic library unit; we've seen (and discarded) the
      -- "generic".
      -- This is one of six possible things: generic package specification,
      -- generic procedure specification, generic function specification,
      -- generic package renaming, generic procedure renaming, or
      -- generic function renaming.
      -- ** This routine should write a Compilation_Unit record when done **.
      -- First_Tok is the first token of the compilation unit; this typically
      -- will be part of the context clause (don't assume anything about what
      -- it is).
      Tok : Token;
      Semi   : Token;
      Result : Test_Summary.Info_Record(Test_Summary.Compilation_Unit);
   begin
      -- func ::= generic [formals] function <normal spec>
      -- proc ::= generic [formals] procedure <normal spec>
      -- pack ::= generic [formals] package <normal spec>
      -- funcren ::= generic function ID renames name;
      -- procren ::= generic procedure ID renames name;
      -- packren ::= generic package ID renames name;

      -- Set up the parts of the Result that we know:
      Result.Source_Name    := Simple_Source_Name;
      Result.Start_Line     := First_Tok.Line_Num;
      Result.Start_Position := First_Tok.First;
      Result.Is_Main        := False; -- No main generics!
      -- End_Line and End_Position TBD.
      -- Unit_Kind and Unit_Name TBD.
      -- Optional TBD.

      -- First, discard any generic formals, stopping on the
      -- function/procedure/package of the specification or renames.
      -- Then, use the Parse_xxx routines to process the unit.

      loop
         Tok := Get_Next_Token;
         if Tok.Kind = With_Word then
            -- This could be many things. Here, we only care if it precedes
            -- package, procedure, or function, meaning these are formal
            -- packages or subprograms. These do not include a begin or
            -- end so they can be skipped outright.
            Tok := Peek_Next_Token;
            if Tok.Kind = Package_Word then -- Formal package.
               Tok := Get_Next_Token; -- Skip "package".
            elsif Tok.Kind = Procedure_Word then -- Formal procedure.
               Tok := Get_Next_Token; -- Skip "procedure".
            elsif Tok.Kind = Function_Word then -- Formal function.
               Tok := Get_Next_Token; -- Skip "function".
            else
               null; -- Process this new token normally.
            end if;
         elsif Tok.Kind = Access_Word then
            -- This could be many things. Here, we only care if it precedes
            -- protected, procedure, or function, meaning these are formal
            -- access-to-subprograms. These do not include a begin or
            -- end so they can be skipped outright.
            Tok := Peek_Next_Token;
            if Tok.Kind = Protected_Word then -- Some formal access type.
               Tok := Get_Next_Token; -- Skip "protected".
               Tok := Peek_Next_Token;
               if Tok.Kind = Procedure_Word then -- prot access-to-procedure.
                  Tok := Get_Next_Token; -- Skip "procedure".
               elsif Tok.Kind = Function_Word then -- prot access-to-function.
                  Tok := Get_Next_Token; -- Skip "function".
               else
                  Debug_Info ("formal access protected not proc/func?",
                      Tok => Tok);
                  raise Parse_Error with
                     "formal access protected not subprog, " &
                     "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
               end if;
            elsif Tok.Kind = Procedure_Word then -- Formal access-to-procedure.
               Tok := Get_Next_Token; -- Skip "procedure".
            elsif Tok.Kind = Function_Word then -- Formal access-to-function.
               Tok := Get_Next_Token; -- Skip "function".
            else
               null; -- Process this new token normally.
            end if;
         elsif Tok.Kind = Package_Word then
            -- Generic package spec or renames.
            Parse_Package_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);
            if Test_Summary."=" (Result.Unit_Kind,
                                 Test_Summary.Package_Specification) then
               Result.Unit_Kind := Test_Summary.Generic_Package;
            elsif Test_Summary."=" (Result.Unit_Kind,
                                 Test_Summary.Package_Renaming) then
               Result.Unit_Kind := Test_Summary.Generic_Package_Renaming;
            else
               Debug_Info ("generic " &
                 Test_Summary.Compilation_Unit_Kinds'Image (Result.Unit_Kind) &
                 "?", Result.Unit_Name, "package", Semi);
               raise Parse_Error with "unusual generic package kind," &
                  "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
            exit; -- Done with this unit.

         elsif Tok.Kind = Procedure_Word then
            -- Generic procedure spec or renames.
            Parse_Procedure_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);
            if Test_Summary."=" (Result.Unit_Kind,
                                 Test_Summary.Procedure_Specification) then
               Result.Unit_Kind := Test_Summary.Generic_Procedure;
            elsif Test_Summary."=" (Result.Unit_Kind,
                                 Test_Summary.Procedure_Renaming) then
               Result.Unit_Kind := Test_Summary.Generic_Procedure_Renaming;
            else
               Debug_Info ("generic " &
                 Test_Summary.Compilation_Unit_Kinds'Image (Result.Unit_Kind) &
                 "?", Result.Unit_Name, "procedure", Semi);
               raise Parse_Error with "unusual generic procedure kind," &
                  "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
            exit; -- Done with this unit.

         elsif Tok.Kind = Function_Word then
            -- Generic Function spec or renames.
            Parse_Function_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);
            if Test_Summary."=" (Result.Unit_Kind,
                                 Test_Summary.Function_Specification) then
               Result.Unit_Kind := Test_Summary.Generic_Function;
            elsif Test_Summary."=" (Result.Unit_Kind,
                                 Test_Summary.Function_Renaming) then
               Result.Unit_Kind := Test_Summary.Generic_Function_Renaming;
            else
               Debug_Info ("generic " &
                 Test_Summary.Compilation_Unit_Kinds'Image (Result.Unit_Kind) &
                 "?", Result.Unit_Name, "function", Semi);
               raise Parse_Error with "unusual generic function kind," &
                  "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
            end if;
            exit; -- Done with this unit.

         elsif Tok.Kind = EOF then
            raise Parse_Error with "no spec for generic, reached EOF," &
               "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);

         -- else some uninteresting part of a generic formal part, skip it.
         end if;
      end loop;

      -- Here, Semi contains the ending semicolon of the unit.
      Result.End_Line := Semi.Line_Num;
      Result.End_Position := Semi.Last;
      Result.Optional :=
         Special_Handling.Optional_Unit_for_Test (Simple_Source_Name,
                                  Result.Unit_Name, Result.Unit_Kind);

      Test_Summary.Write_Summary_Record (Summary_File, Result);
   end Parse_Generic_Library_Unit;


   procedure Parse_Subunit (First_Tok : Token) is
      -- Parse a subunit; we've seen (and discarded) the "separate".
      -- This is one of five possible things: package subunit,
      -- procedure subunit, function subunit, task subunit, or protected
      -- subunit.
      -- This routine writes a Compilation_Unit record when done.
      -- First_Tok is the first token of the compilation unit; this typically
      -- will be part of the context clause (don't assume anything about what
      -- it is).
      Tok    : Token;
      Semi   : Token;
      Result : Test_Summary.Info_Record(Test_Summary.Compilation_Unit);
      Parent_Name : Test_Summary.Comp_Unit_Name;
   begin
      -- Set up the parts of the Result that we know:
      Result.Source_Name    := Simple_Source_Name;
      Result.Start_Line     := First_Tok.Line_Num;
      Result.Start_Position := First_Tok.First;
      Result.Is_Main        := False; -- No main subunits!
      -- End_Line and End_Position TBD.
      -- Unit_Kind and Unit_Name TBD.
      -- Optional TBD.

      -- subunit ::= separate (parent_name) proper_body
      --    (all 5 kinds of bodies: function, procedure, package, protected,
      --          and task are allowed.)
      Tok := Get_Next_Token; -- Get next token.
      if Tok.Kind = LParen then
         Tok := Get_Next_Token; -- Get next token.
         Get_Identifier (Tok, Parent_Name); -- Identifier or expanded name.
         Tok := Get_Next_Token; -- Get next token.
         if Tok.Kind = RParen then
             Tok := Get_Next_Token; -- Get next token.
         else
             Debug_Info ("Not rparen?", Tok => Tok);
             raise Parse_Error with "missing closing for parent name of " &
                "subunit, on line" &
                Trace.Line_Number_Type'Image(Tok.Line_Num);
         end if;
      else
         Debug_Info ("Not lparen?", Tok => Tok);
         raise Parse_Error with "missing parent name for subunit" &
            ", on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;
      if Tok.Kind = Function_Word then
         Parse_Function_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);
         -- Ignore problems with the wrong kind of unit (might allow some
         -- possible illegal compilations).
         Result.Unit_Kind := Test_Summary.Function_Subunit;

      elsif Tok.Kind = Procedure_Word then
         Parse_Procedure_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);
         -- Ignore problems with the wrong kind of unit (might allow some
         -- possible illegal compilations).
         Result.Unit_Kind := Test_Summary.Procedure_Subunit;

      elsif Tok.Kind = Package_Word then
         Parse_Package_Unit (Semi, Result.Unit_Kind, Result.Unit_Name);
         -- Ignore problems with the wrong kind of unit (might allow some
         -- possible illegal compilations).
         Result.Unit_Kind := Test_Summary.Package_Subunit;

      elsif Tok.Kind = Protected_Word then
         Parse_Protected_Unit (Semi, Result.Unit_Name);
         Result.Unit_Kind := Test_Summary.Protected_Subunit;

      elsif Tok.Kind = Task_Word then
         Parse_Task_Unit (Semi, Result.Unit_Name);
         Result.Unit_Kind := Test_Summary.Task_Subunit;

      else --??
         Debug_Info ("Not body kind for subunit?", Tok => Tok);
         raise Parse_Error with "not a kind of body for subunit," &
            "on line" & Trace.Line_Number_Type'Image(Tok.Line_Num);
      end if;

      -- Combine the Parent_Name and Unit_Name into a single expanded name.
      -- The name is not unique without that, and we may need to know the
      -- parent in usage. It's easy to just get the unit name if we need
      -- it from the expanded name, the reverse isn't possible.
      Ada.Strings.Fixed.Move (Target => Result.Unit_Name,
                              Source =>
         Ada.Strings.Fixed.Trim (Parent_Name, Ada.Strings.Both) & '.' &
         Ada.Strings.Fixed.Trim (Result.Unit_Name, Ada.Strings.Both));

      -- Here, Semi contains the ending semicolon of the unit.
      Result.End_Line := Semi.Line_Num;
      Result.End_Position := Semi.Last;
      Result.Optional :=
         Special_Handling.Optional_Unit_for_Test (Simple_Source_Name,
                                  Result.Unit_Name, Result.Unit_Kind);

      Test_Summary.Write_Summary_Record (Summary_File, Result);
   end Parse_Subunit;


   procedure Parse_Pragma (First_Tok : Token) is
      -- Parse a configuration pragma; we've seen (and discarded) the
      -- "pragma".
      -- This routine writes a Compilation_Unit record when done.
      -- First_Tok is the first token of the compilation unit; this typically
      -- will be part of the context clause (don't assume anything about what
      -- it is).
      Tok    : Token;
      Semi   : Token;
      Result : Test_Summary.Info_Record(Test_Summary.Compilation_Unit);
   begin
      -- Set up the parts of the Result that we know:
      Result.Source_Name    := Simple_Source_Name;
      Result.Start_Line     := First_Tok.Line_Num;
      Result.Start_Position := First_Tok.First;
      Result.Is_Main        := False; -- No main pragmas!
      Result.Unit_Kind      := Test_Summary.Configuration_Pragma;
      Result.Optional       := False;
      -- End_Line and End_Position TBD.
      -- Unit_Name TBD.

      Tok := Get_Next_Token;
      -- Tok should contain the pragme name. Save it.
      Get_Identifier (Tok, Result.Unit_Name);

      -- Now, just skip to the semicolon, as no semicolon can appear inside
      -- of the pragma.
      while Tok.Kind /= Semicolon loop
         exit when Tok.Kind = EOF; -- In event of disaster, no infinite loop.
         Tok := Get_Next_Token;
      end loop;

      -- Here, Tok contains the ending semicolon of the unit.
      Result.End_Line := Tok.Line_Num;
      Result.End_Position := Tok.Last;

      Test_Summary.Write_Summary_Record (Summary_File, Result);
   end Parse_Pragma;


   procedure Parse_Compilation_Unit is
      First_Tok : Token := Peek_Next_Token;
                                  -- The first token in the compilation unit.
      Tok : Token;
      Started_Context_Clause : Boolean := False;
   begin
      -- Start by skipping any preliminary context clause and
      -- "private":
      loop
         Tok := Get_Next_Token;
         if Tok.Kind = With_Word then
            Started_Context_Clause := True;
         -- else not the start of a context clause.
         end if;

         -- Amazingly, the 23 kinds of compilation unit have to start with
         -- one of the following 6 reserved words:
         exit when Tok.Kind = Generic_Word;
         exit when Tok.Kind = Package_Word;
         exit when Tok.Kind = Procedure_Word;
         exit when Tok.Kind = Function_Word;
         exit when Tok.Kind = Separate_Word;
         exit when Tok.Kind = Pragma_Word and then
                   (not Started_Context_Clause);
            -- A pragma inside of a context clause (like Elaborate) does
            -- not start a compilation unit.
         if Tok.Kind = EOF then
            raise Parse_Error with "reached end of file without finding a " &
               "compilation unit; line of first token is" &
               Trace.Line_Number_Type'Image(First_Tok.Line_Num);
         end if;
      end loop;
      -- Now, handle the unit:
      if Tok.Kind = Separate_Word then
         Parse_Subunit (First_Tok); -- Pack, Proc, Func, Task, Prot.
      elsif Tok.Kind = Package_Word then
         Parse_Package_Library_Unit (First_Tok);
                   -- Decl, Body, Renames, Instance
      elsif Tok.Kind = Procedure_Word then
         Parse_Procedure_Library_Unit (First_Tok);
                   -- Decl, Body, Renames, Instance
      elsif Tok.Kind = Function_Word then
         Parse_Function_Library_Unit (First_Tok);
                   -- Decl, Body, Renames, Instance
      elsif Tok.Kind = Generic_Word then
         Parse_Generic_Library_Unit (First_Tok);
                   -- Pack, Proc, Func, and renames thereof
      elsif Tok.Kind = Pragma_Word then
         -- A configuration pragma.
         Parse_Pragma (First_Tok);
      else
         raise Parse_Error with "Dropped out of loop with wrong token " &
            Token_Kind'Image (Tok.Kind);
      end if;
   end Parse_Compilation_Unit;


   procedure Parse_Compilation is
      -- Parse a Compilation (10.1.1(2)).
      Tok : Token;
   begin
      loop
         Tok := Peek_Next_Token;
         exit when Tok.Kind = EOF;
         Parse_Compilation_Unit;
      end loop;
   end Parse_Compilation;

begin
   Ada.Text_IO.Put_Line ("ACATS Test Summary Tool - version 1.0");

   if Ada.Command_Line.Argument_Count < 2 then
      Ada.Text_IO.Put_Line ("*ERROR* Insufficient command line arguments - " &
         "ACATS test source file and ");
      Ada.Text_IO.Put_Line ("        test summary file are required - " &
         "quitting");
      raise Kill_Tool;
   end if;

   begin
      Ada.Text_IO.Open (Source_File, Ada.Text_IO.In_File,
                        Ada.Command_Line.Argument(1));
   exception
      when Exc1:Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("*ERROR* ACATS test source file not found - " &
            "message: " & Ada.Exceptions.Exception_Message (Exc1));
         raise Kill_Tool;
      when Exc2:others =>
         Ada.Text_IO.Put_Line ("*ERROR* Exception opening ACATS test " &
            "source file - " & Ada.Exceptions.Exception_Name (Exc2) & "; " &
            Ada.Exceptions.Exception_Message (Exc2));
         raise Kill_Tool;
   end;

   begin
      Ada.Text_IO.Open (Summary_File, Ada.Text_IO.Append_File,
                        Ada.Command_Line.Argument(2));
   exception
      when Exc1:Ada.Text_IO.Name_Error =>
         -- File note found, try to create it.
         begin
            Ada.Text_IO.Create (Summary_File, Ada.Text_IO.Out_File,
                                Ada.Command_Line.Argument(2));
            Ada.Text_IO.Put_Line (Summary_File,
                                  "Kind,Source_Name,Start Line,Start Pos," &
                                  "End Line,End Pos,Name_Label,Flag");
               -- Write column headers, to make it easier to use these
               -- files in a spreadsheet.
         exception
            when Exc1:others =>
               Ada.Text_IO.Put_Line ("*ERROR* Exception creating Test summary "
               & "file - " & Ada.Exceptions.Exception_Name (Exc1) & "; " &
               Ada.Exceptions.Exception_Message (Exc1));
            raise Kill_Tool;
         end;
      when Exc2:others =>
         Ada.Text_IO.Put_Line ("*ERROR* Exception opening Test summary " &
            "file - " & Ada.Exceptions.Exception_Name (Exc2) & "; " &
            Ada.Exceptions.Exception_Message (Exc2));
         raise Kill_Tool;
   end;

   -- The files are open if we get here.

   -- Figure out the simple test file name:

   Ada.Strings.Fixed.Move (Target => Simple_Source_Name,
                           Source => Ada.Strings.Fixed.Translate (
                                   Ada.Directories.Simple_Name (
                                      Ada.Text_IO.Name(Source_File)),
                                   Ada.Strings.Maps.Constants.Upper_Case_Map));

   Ada.Text_IO.Put_Line ("Process ACATS Test " & Simple_Source_Name);

   -- The source file is in UTF-8, but for now we'll ignore that (it only
   -- will matter to us if a compilation unit name is outside of Ascii;
   -- that will happen someday, but isn't true yet).

   -- Check if the unit need special handling:
   case Special_Handling.Do_Special_for_Test_File (
          Source_Name  => Simple_Source_Name,
          Summary_File => Summary_File) is
      when Special_Handling.Not_Special =>
         -- Parse the test file, looking at program units so we can write the
         -- appropriate unit records with the beginning and ending tokens.
         Parse_Compilation;
      when Special_Handling.No_Parse =>
         -- Just process the unit lexically, handling all of the error tags.
         -- Do_Special already wrote the compilation unit records.
         if DEBUG then
            Ada.Text_IO.Put_Line ("Special handling for " & Simple_Source_Name
               & " lexical processing only");
         end if;
         declare
            Current_Token : Token := Get_Next_Token;
         begin
            while Current_Token /= EOF_TOKEN loop
               Current_Token := Get_Next_Token;
            end loop;
         end;
      when Special_Handling.Fully_Manual =>
         -- Do_Special already wrote the entire summary for the test file.
         -- Nothing to do here.
         null;
         if DEBUG then
            Ada.Text_IO.Put_Line ("Special handling for " & Simple_Source_Name
               & " no normal processing");
         end if;
   end case;

   Ada.Text_IO.Close (Source_File);

   Ada.Text_IO.Close (Summary_File);

exception
   when Kill_Tool => null; -- A fatal error happened.
end Summary;
