------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S T Y L E G                                --
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

--  This version of the Style package implements the standard GNAT style
--  checking rules. For documentation of these rules, see comments on the
--  individual procedures.

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Einfo;    use Einfo;
with Err_Vars; use Err_Vars;
with Opt;      use Opt;
with Scans;    use Scans;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stylesw;  use Stylesw;

package body Styleg is

   use ASCII;

   Blank_Lines : Nat := 0;
   --  Counts number of empty lines seen. Reset to zero if a non-empty line
   --  is encountered. Used to check for trailing blank lines in Check_EOF,
   --  and for multiple blank lines.

   Blank_Line_Location : Source_Ptr;
   --  Remembers location of first blank line in a series. Used to issue an
   --  appropriate diagnostic if subsequent blank lines or the end of file
   --  is encountered.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Check_No_Space_After;
   --  Checks that there is a non-white space character after the current
   --  token, or white space followed by a comment, or the end of line.
   --  Issue error message if not.

   procedure Check_No_Space_Before;
   --  Check that token is first token on line, or else is not preceded
   --  by white space. Signal error of space not allowed if not.

   procedure Check_Separate_Stmt_Lines_Cont;
   --  Non-inlined continuation of Check_Separate_Stmt_Lines

   function Determine_Token_Casing return Casing_Type;
   --  Determine casing of current token

   procedure Error_Space_Not_Allowed (S : Source_Ptr);
   --  Posts an error message indicating that a space is not allowed
   --  at the given source location.

   procedure Error_Space_Required (S : Source_Ptr);
   --  Posts an error message indicating that a space is required at
   --  the given source location.

   function Is_White_Space (C : Character) return Boolean;
   pragma Inline (Is_White_Space);
   --  Returns True for space or HT, False otherwise
   --  What about VT and FF, should they return True ???

   procedure Require_Following_Space;
   pragma Inline (Require_Following_Space);
   --  Require token to be followed by white space. Used only if in GNAT
   --  style checking mode.

   procedure Require_Preceding_Space;
   pragma Inline (Require_Preceding_Space);
   --  Require token to be preceded by white space. Used only if in GNAT
   --  style checking mode.

   ----------------------
   -- Check_Abs_Or_Not --
   ----------------------

   --  In check token mode (-gnatyt), ABS/NOT must be followed by a space

   procedure Check_Abs_Not is
   begin
      if Style_Check_Tokens then
         if Source (Scan_Ptr) > ' ' then -- ???
            Error_Space_Required (Scan_Ptr);
         end if;
      end if;
   end Check_Abs_Not;

   ----------------------
   -- Check_Apostrophe --
   ----------------------

   --  Do not allow space before or after apostrophe -- OR AFTER???

   procedure Check_Apostrophe is
   begin
      if Style_Check_Tokens then
         Check_No_Space_After;
      end if;
   end Check_Apostrophe;

   -----------------
   -- Check_Arrow --
   -----------------

   --  In check tokens mode (-gnatys), arrow must be surrounded by spaces,
   --  except that within the argument of a Depends or Refined_Depends aspect
   --  or pragma the required format is "=>+ " rather than "=> +").

   procedure Check_Arrow (Inside_Depends : Boolean := False) is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;

         --  Special handling for Depends and Refined_Depends

         if Inside_Depends then
            if Source (Scan_Ptr) = ' '
              and then Source (Scan_Ptr + 1) = '+'
            then
               Error_Space_Not_Allowed (Scan_Ptr);

            elsif Source (Scan_Ptr) /= ' '
              and then Source (Scan_Ptr) /= '+'
            then
               Require_Following_Space;
            end if;

         --  Normal case

         else
            Require_Following_Space;
         end if;
      end if;
   end Check_Arrow;

   --------------------------
   -- Check_Attribute_Name --
   --------------------------

   --  In check attribute casing mode (-gnatya), attribute names must be
   --  mixed case, i.e. start with an upper case letter, and otherwise
   --  lower case, except after an underline character.

   procedure Check_Attribute_Name (Reserved : Boolean) is
      pragma Warnings (Off, Reserved);
   begin
      if Style_Check_Attribute_Casing then
         if Determine_Token_Casing /= Mixed_Case then
            Error_Msg_SC -- CODEFIX
              ("(style) bad capitalization, mixed case required");
         end if;
      end if;
   end Check_Attribute_Name;

   ---------------------------
   -- Check_Binary_Operator --
   ---------------------------

   --  In check token mode (-gnatyt), binary operators other than the special
   --  case of exponentiation require surrounding space characters.

   procedure Check_Binary_Operator is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
      end if;
   end Check_Binary_Operator;

   ----------------------------
   -- Check_Boolean_Operator --
   ----------------------------

   procedure Check_Boolean_Operator (Node : Node_Id) is

      function OK_Boolean_Operand (N : Node_Id) return Boolean;
      --  Returns True for simple variable, or "not X1" or "X1 and X2" or
      --  "X1 or X2" where X1, X2 are recursively OK_Boolean_Operand's.

      ------------------------
      -- OK_Boolean_Operand --
      ------------------------

      function OK_Boolean_Operand (N : Node_Id) return Boolean is
      begin
         if Nkind_In (N, N_Identifier, N_Expanded_Name) then
            return True;

         elsif Nkind (N) = N_Op_Not then
            return OK_Boolean_Operand (Original_Node (Right_Opnd (N)));

         elsif Nkind_In (N, N_Op_And, N_Op_Or) then
            return OK_Boolean_Operand (Original_Node (Left_Opnd (N)))
                     and then
                   OK_Boolean_Operand (Original_Node (Right_Opnd (N)));

         else
            return False;
         end if;
      end OK_Boolean_Operand;

   --  Start of processing for Check_Boolean_Operator

   begin
      if Style_Check_Boolean_And_Or
        and then Comes_From_Source (Node)
      then
         declare
            Orig : constant Node_Id := Original_Node (Node);

         begin
            if Nkind_In (Orig, N_Op_And, N_Op_Or) then
               declare
                  L : constant Node_Id := Original_Node (Left_Opnd  (Orig));
                  R : constant Node_Id := Original_Node (Right_Opnd (Orig));

               begin
                  --  First OK case, simple boolean constants/identifiers

                  if OK_Boolean_Operand (L)
                       and then
                     OK_Boolean_Operand (R)
                  then
                     return;

                  --  Second OK case, modular types

                  elsif Is_Modular_Integer_Type (Etype (Node)) then
                     return;

                  --  Third OK case, array types

                  elsif Is_Array_Type (Etype (Node)) then
                     return;

                  --  Otherwise we have an error

                  elsif Nkind (Orig) = N_Op_And then
                     Error_Msg -- CODEFIX
                       ("(style) `AND THEN` required", Sloc (Orig));
                  else
                     Error_Msg -- CODEFIX
                       ("(style) `OR ELSE` required", Sloc (Orig));
                  end if;
               end;
            end if;
         end;
      end if;
   end Check_Boolean_Operator;

   ---------------
   -- Check_Box --
   ---------------

   --  In check token mode (-gnatyt), box must be preceded by a space or by
   --  a left parenthesis. Spacing checking on the surrounding tokens takes
   --  care of the remaining checks.

   procedure Check_Box is
   begin
      if Style_Check_Tokens then
         if Prev_Token /= Tok_Left_Paren then
            Require_Preceding_Space;
         end if;
      end if;
   end Check_Box;

   -----------------
   -- Check_Colon --
   -----------------

   --  In check token mode (-gnatyt), colon must be surrounded by spaces

   procedure Check_Colon is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
      end if;
   end Check_Colon;

   -----------------------
   -- Check_Colon_Equal --
   -----------------------

   --  In check token mode (-gnatyt), := must be surrounded by spaces

   procedure Check_Colon_Equal is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
      end if;
   end Check_Colon_Equal;

   -----------------
   -- Check_Comma --
   -----------------

   --  In check token mode (-gnatyt), comma must be either the first
   --  token on a line, or be preceded by a non-blank character.
   --  It must also always be followed by a blank.

   procedure Check_Comma is
   begin
      if Style_Check_Tokens then
         Check_No_Space_Before;

         if Source (Scan_Ptr) > ' ' then
            Error_Space_Required (Scan_Ptr);
         end if;
      end if;
   end Check_Comma;

   -------------------
   -- Check_Comment --
   -------------------

   --  In check comment mode (-gnatyc) there are several requirements on the
   --  format of comments. The following are permissible comment formats:

   --    1. Any comment that is not at the start of a line, i.e. where the
   --       initial minuses are not the first non-blank characters on the
   --       line must have at least one blank after the second minus or a
   --       special character as defined in rule 5.

   --    2. A row of all minuses of any length is permitted (see procedure
   --       box above in the source of this routine).

   --    3. A comment line starting with two minuses and a space, and ending
   --       with a space and two minuses. Again see the procedure title box
   --       immediately above in the source.

   --    4. A full line comment where two spaces follow the two minus signs.
   --       This is the normal comment format in GNAT style, as typified by
   --       the comments you are reading now.

   --    5. A full line comment where the first character after the second
   --       minus is a special character, i.e. a character in the ASCII
   --       range 16#21#..16#2F# or 16#3A#..16#3F#. This allows special
   --       comments, such as those generated by gnatprep, or those that
   --       appear in the SPARK annotation language to be accepted.

   --       Note: for GNAT internal files (-gnatg switch set on for the
   --       compilation), the only special sequence recognized and allowed
   --       is --! as generated by gnatprep.

   --    6. In addition, the comment must be properly indented if comment
   --       indentation checking is active (Style_Check_Indentation non-zero).
   --       Either the start column must be a multiple of this indentation,
   --       or the indentation must match that of the next non-blank line,
   --       or must match the indentation of the immediately preciding line
   --       if it is non-blank.

   procedure Check_Comment is
      S : Source_Ptr;
      C : Character;

      function Is_Box_Comment return Boolean;
      --  Returns True if the last two characters on the line are -- which
      --  characterizes a box comment (as for example follows this spec).

      function Is_Special_Character (C : Character) return Boolean;
      --  Determines if C is a special character (see rule 5 above)

      function Same_Column_As_Next_Non_Blank_Line return Boolean;
      --  Called for a full line comment. If the indentation of this comment
      --  matches that of the next non-blank line in the source, then True is
      --  returned, otherwise False.

      function Same_Column_As_Previous_Line return Boolean;
      --  Called for a full line comment. If the previous line is blank, then
      --  returns False. Otherwise, if the indentation of this comment matches
      --  that of the previous line in the source, then True is returned,
      --  otherwise False.

      --------------------
      -- Is_Box_Comment --
      --------------------

      function Is_Box_Comment return Boolean is
         S : Source_Ptr;

      begin
         --  Do we need to worry about UTF_32 line terminators here ???

         S := Scan_Ptr + 3;
         while Source (S) not in Line_Terminator loop
            S := S + 1;
         end loop;

         return Source (S - 1) = '-' and then Source (S - 2) = '-';
      end Is_Box_Comment;

      --------------------------
      -- Is_Special_Character --
      --------------------------

      function Is_Special_Character (C : Character) return Boolean is
      begin
         if GNAT_Mode then
            return C = '!';
         else
            return
              Character'Pos (C) in 16#21# .. 16#2F#
                or else
              Character'Pos (C) in 16#3A# .. 16#3F#;
         end if;
      end Is_Special_Character;

      ----------------------------------------
      -- Same_Column_As_Next_Non_Blank_Line --
      ----------------------------------------

      function Same_Column_As_Next_Non_Blank_Line return Boolean is
         P : Source_Ptr;

      begin
         --  Step to end of line

         P := Scan_Ptr + 2;
         while Source (P) not in Line_Terminator loop
            P := P + 1;
         end loop;

         --  Step past blanks, and line terminators (UTF_32 case???)

         while Source (P) <= ' ' and then Source (P) /= EOF loop
            P := P + 1;
         end loop;

         --  Compare columns

         return Get_Column_Number (Scan_Ptr) = Get_Column_Number (P);
      end Same_Column_As_Next_Non_Blank_Line;

      ----------------------------------
      -- Same_Column_As_Previous_Line --
      ----------------------------------

      function Same_Column_As_Previous_Line return Boolean is
         S, P : Source_Ptr;

      begin
         --  Point S to start of this line, and P to start of previous line

         S := Line_Start (Scan_Ptr);
         P := S;
         Backup_Line (P);

         --  Step P to first non-blank character on line

         loop
            --  If we get back to start of current line, then the previous line
            --  was blank, and we always return False in that situation.

            if P = S then
               return False;
            end if;

            exit when Source (P) /= ' ' and then Source (P) /= ASCII.HT;
            P := P + 1;
         end loop;

         --  Compare columns

         return Get_Column_Number (Scan_Ptr) = Get_Column_Number (P);
      end Same_Column_As_Previous_Line;

   --  Start of processing for Check_Comment

   begin
      --  Can never have a non-blank character preceding the first minus.
      --  The "+ 3" is to leave room for a possible byte order mark (BOM);
      --  we want to avoid a warning for a comment at the start of the
      --  file just after the BOM.

      if Style_Check_Comments then
         if Scan_Ptr > Source_First (Current_Source_File) + 3
           and then Source (Scan_Ptr - 1) > ' '
         then
            Error_Msg_S -- CODEFIX
              ("(style) space required");
         end if;
      end if;

      --  For a comment that is not at the start of the line, the only
      --  requirement is that we cannot have a non-blank character after
      --  the second minus sign or a special character.

      if Scan_Ptr /= First_Non_Blank_Location then
         if Style_Check_Comments then
            if Source (Scan_Ptr + 2) > ' '
              and then not Is_Special_Character (Source (Scan_Ptr + 2))
            then
               Error_Msg -- CODEFIX
                 ("(style) space required", Scan_Ptr + 2);
            end if;
         end if;

         return;

      --  Case of a comment that is at the start of a line

      else
         --  First check, must be in appropriately indented column

         if Style_Check_Indentation /= 0 then
            if Start_Column rem Style_Check_Indentation /= 0 then
               if not Same_Column_As_Next_Non_Blank_Line
                 and then not Same_Column_As_Previous_Line
               then
                  Error_Msg_S -- CODEFIX
                    ("(style) bad column");
               end if;

               return;
            end if;
         end if;

         --  If we are not checking comments, nothing more to do

         if not Style_Check_Comments then
            return;
         end if;

         --  Case of not followed by a blank. Usually wrong, but there are
         --  some exceptions that we permit.

         if Source (Scan_Ptr + 2) /= ' ' then
            C := Source (Scan_Ptr + 2);

            --  Case of -- all on its own on a line is OK

            if C < ' ' then
               return;
            end if;

            --  Case of --x, x special character is OK (gnatprep/SPARK/etc.)
            --  This is not permitted in internal GNAT implementation units
            --  except for the case of --! as used by gnatprep output.

            if Is_Special_Character (C) then
               return;
            end if;

            --  The only other case in which we allow a character after
            --  the -- other than a space is when we have a row of minus
            --  signs (case of header lines for a box comment for example).

            S := Scan_Ptr + 2;
            while Source (S) >= ' ' loop
               if Source (S) /= '-' then
                  if Is_Box_Comment
                    or else Style_Check_Comments_Spacing = 1
                  then
                     Error_Space_Required (Scan_Ptr + 2);
                  else
                     Error_Msg -- CODEFIX
                       ("(style) two spaces required", Scan_Ptr + 2);
                  end if;

                  return;
               end if;

               S := S + 1;
            end loop;

         --  If we are followed by a blank, then the comment is OK if the
         --  character following this blank is another blank or a format
         --  effector, or if the required comment spacing is 1.

         elsif Source (Scan_Ptr + 3) <= ' '
           or else Style_Check_Comments_Spacing = 1
         then
            return;

         --  Here is the case where we only have one blank after the two minus
         --  signs, with Style_Check_Comments_Spacing set to 2, which is an
         --  error unless the line ends with two minus signs, the case of a
         --  box comment.

         elsif not Is_Box_Comment then
            Error_Space_Required (Scan_Ptr + 3);
         end if;
      end if;
   end Check_Comment;

   -------------------
   -- Check_Dot_Dot --
   -------------------

   --  In check token mode (-gnatyt), ".." must be surrounded by spaces

   procedure Check_Dot_Dot is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
      end if;
   end Check_Dot_Dot;

   ---------------
   -- Check_EOF --
   ---------------

   --  In check blanks at end mode, check no blank lines precede the EOF

   procedure Check_EOF is
   begin
      if Style_Check_Blank_Lines then

         --  We expect one blank line, from the EOF, but no more than one

         if Blank_Lines = 2 then
            Error_Msg -- CODEFIX
              ("(style) blank line not allowed at end of file",
               Blank_Line_Location);

         elsif Blank_Lines >= 3 then
            Error_Msg -- CODEFIX
              ("(style) blank lines not allowed at end of file",
               Blank_Line_Location);
         end if;
      end if;
   end Check_EOF;

   -----------------------------------
   -- Check_Exponentiation_Operator --
   -----------------------------------

   --  No spaces are required for the ** operator in GNAT style check mode

   procedure Check_Exponentiation_Operator is
   begin
      null;
   end Check_Exponentiation_Operator;

   --------------
   -- Check_HT --
   --------------

   --  In check horizontal tab mode (-gnatyh), tab characters are not allowed

   procedure Check_HT is
   begin
      if Style_Check_Horizontal_Tabs then
         Error_Msg_S -- CODEFIX
           ("(style) horizontal tab not allowed");
      end if;
   end Check_HT;

   -----------------------
   -- Check_Indentation --
   -----------------------

   --  In check indentation mode (-gnaty? for ? a digit), a new statement or
   --  declaration is required to start in a column that is a multiple of the
   --  indentation amount.

   procedure Check_Indentation is
   begin
      if Style_Check_Indentation /= 0 then
         if Token_Ptr = First_Non_Blank_Location
           and then Start_Column rem Style_Check_Indentation /= 0
         then
            Error_Msg_SC -- CODEFIX
              ("(style) bad indentation");
         end if;
      end if;
   end Check_Indentation;

   ----------------------
   -- Check_Left_Paren --
   ----------------------

   --  In check token mode (-gnatyt), left paren must not be preceded by an
   --  identifier character or digit (a separating space is required) and may
   --  never be followed by a space.

   procedure Check_Left_Paren is
   begin
      if Style_Check_Tokens then
         if Token_Ptr > Source_First (Current_Source_File)
           and then Identifier_Char (Source (Token_Ptr - 1))
         then
            Error_Space_Required (Token_Ptr);
         end if;

         Check_No_Space_After;
      end if;
   end Check_Left_Paren;

   ---------------------------
   -- Check_Line_Max_Length --
   ---------------------------

   --  In check max line length mode (-gnatym), the line length must
   --  not exceed the permitted maximum value.

   procedure Check_Line_Max_Length (Len : Nat) is
   begin
      if Style_Check_Max_Line_Length then
         if Len > Style_Max_Line_Length then
            Error_Msg
              ("(style) this line is too long",
               Current_Line_Start + Source_Ptr (Style_Max_Line_Length));
         end if;
      end if;
   end Check_Line_Max_Length;

   ---------------------------
   -- Check_Line_Terminator --
   ---------------------------

   --  In check blanks at end mode (-gnatyb), lines may not end with a
   --  trailing space.

   --  In check form feeds mode (-gnatyf), the line terminator may not
   --  be either of the characters FF or VT.

   --  In check DOS line terminators node (-gnatyd), the line terminator
   --  must be a single LF, without a following CR.

   procedure Check_Line_Terminator (Len : Nat) is
      S : Source_Ptr;

      L : Nat := Len;
      --  Length of line (adjusted down for blanks at end of line)

   begin
      --  Reset count of blank lines if first line

      if Get_Logical_Line_Number (Scan_Ptr) = 1 then
         Blank_Lines := 0;
      end if;

      --  Check FF/VT terminators

      if Style_Check_Form_Feeds then
         if Source (Scan_Ptr) = ASCII.FF then
            Error_Msg_S -- CODEFIX
              ("(style) form feed not allowed");
         elsif Source (Scan_Ptr) = ASCII.VT then
            Error_Msg_S -- CODEFIX
              ("(style) vertical tab not allowed");
         end if;
      end if;

      --  Check DOS line terminator

      if Style_Check_DOS_Line_Terminator then

         --  Ignore EOF, since we only get called with an EOF if it is the last
         --  character in the buffer (and was therefore not in the source
         --  file), since the terminating EOF is added to stop the scan.

         if Source (Scan_Ptr) = EOF then
            null;

         --  Bad terminator if we don't have an LF

         elsif Source (Scan_Ptr) /= LF then
            Error_Msg_S ("(style) incorrect line terminator");
         end if;
      end if;

      --  Remove trailing spaces

      S := Scan_Ptr;
      while L > 0 and then Is_White_Space (Source (S - 1)) loop
         S := S - 1;
         L := L - 1;
      end loop;

      --  Issue message for blanks at end of line if option enabled

      if Style_Check_Blanks_At_End and then L < Len then
         Error_Msg -- CODEFIX
           ("(style) trailing spaces not permitted", S);
      end if;

      --  Deal with empty (blank) line

      if L = 0 then

         --  Increment blank line count

         Blank_Lines := Blank_Lines + 1;

         --  If first blank line, record location for later error message

         if Blank_Lines = 1 then
            Blank_Line_Location := Scan_Ptr;
         end if;

      --  Non-blank line, check for previous multiple blank lines

      else
         if Style_Check_Blank_Lines and then Blank_Lines > 1 then
            Error_Msg -- CODEFIX
              ("(style) multiple blank lines", Blank_Line_Location);
         end if;

         --  And reset blank line count

         Blank_Lines := 0;
      end if;
   end Check_Line_Terminator;

   ------------------
   -- Check_Not_In --
   ------------------

   --  In check tokens mode, only one space between NOT and IN

   procedure Check_Not_In is
   begin
      if Style_Check_Tokens then
         if Source (Token_Ptr - 1) /= ' '
           or else Token_Ptr - Prev_Token_Ptr /= 4
         then -- CODEFIX?
            Error_Msg
              ("(style) single space must separate NOT and IN", Token_Ptr - 1);
         end if;
      end if;
   end Check_Not_In;

   --------------------------
   -- Check_No_Space_After --
   --------------------------

   procedure Check_No_Space_After is
      S : Source_Ptr;

   begin
      if Is_White_Space (Source (Scan_Ptr)) then

         --  Allow one or more spaces if followed by comment

         S := Scan_Ptr + 1;
         loop
            if Source (S) = '-' and then Source (S + 1) = '-' then
               return;

            elsif Is_White_Space (Source (S)) then
               S := S + 1;

            else
               exit;
            end if;
         end loop;

         Error_Space_Not_Allowed (Scan_Ptr);
      end if;
   end Check_No_Space_After;

   ---------------------------
   -- Check_No_Space_Before --
   ---------------------------

   procedure Check_No_Space_Before is
   begin
      if Token_Ptr > First_Non_Blank_Location
         and then Source (Token_Ptr - 1) <= ' '
      then
         Error_Space_Not_Allowed (Token_Ptr - 1);
      end if;
   end Check_No_Space_Before;

   -----------------------
   -- Check_Pragma_Name --
   -----------------------

   --  In check pragma casing mode (-gnatyp), pragma names must be mixed
   --  case, i.e. start with an upper case letter, and otherwise lower case,
   --  except after an underline character.

   procedure Check_Pragma_Name is
   begin
      if Style_Check_Pragma_Casing then
         if Determine_Token_Casing /= Mixed_Case then
            Error_Msg_SC -- CODEFIX
              ("(style) bad capitalization, mixed case required");
         end if;
      end if;
   end Check_Pragma_Name;

   -----------------------
   -- Check_Right_Paren --
   -----------------------

   --  In check token mode (-gnatyt), right paren must not be immediately
   --  followed by an identifier character, and must never be preceded by
   --  a space unless it is the initial non-blank character on the line.

   procedure Check_Right_Paren is
   begin
      if Style_Check_Tokens then
         if Identifier_Char (Source (Token_Ptr + 1)) then
            Error_Space_Required (Token_Ptr + 1);
         end if;

         Check_No_Space_Before;
      end if;
   end Check_Right_Paren;

   ---------------------
   -- Check_Semicolon --
   ---------------------

   --  In check token mode (-gnatyt), semicolon does not permit a preceding
   --  space and a following space is required.

   procedure Check_Semicolon is
   begin
      if Style_Check_Tokens then
         Check_No_Space_Before;

         if Source (Scan_Ptr) > ' ' then
            Error_Space_Required (Scan_Ptr);
         end if;
      end if;
   end Check_Semicolon;

   -------------------------------
   -- Check_Separate_Stmt_Lines --
   -------------------------------

   procedure Check_Separate_Stmt_Lines is
   begin
      if Style_Check_Separate_Stmt_Lines then
         Check_Separate_Stmt_Lines_Cont;
      end if;
   end Check_Separate_Stmt_Lines;

   ------------------------------------
   -- Check_Separate_Stmt_Lines_Cont --
   ------------------------------------

   procedure Check_Separate_Stmt_Lines_Cont is
      S : Source_Ptr;

   begin
      --  Skip past white space

      S := Scan_Ptr;
      while Is_White_Space (Source (S)) loop
         S := S + 1;
      end loop;

      --  Line terminator is OK

      if Source (S) in Line_Terminator then
         return;

      --  Comment is OK

      elsif Source (S) = '-' and then Source (S + 1) = '-' then
         return;

      --  ABORT keyword is OK after THEN (THEN ABORT case)

      elsif Token = Tok_Then
        and then (Source (S + 0) = 'a' or else Source (S + 0) = 'A')
        and then (Source (S + 1) = 'b' or else Source (S + 1) = 'B')
        and then (Source (S + 2) = 'o' or else Source (S + 2) = 'O')
        and then (Source (S + 3) = 'r' or else Source (S + 3) = 'R')
        and then (Source (S + 4) = 't' or else Source (S + 4) = 'T')
        and then (Source (S + 5) in Line_Terminator
                   or else Is_White_Space (Source (S + 5)))
      then
         return;

      --  PRAGMA keyword is OK after ELSE

      elsif Token = Tok_Else
        and then (Source (S + 0) = 'p' or else Source (S + 0) = 'P')
        and then (Source (S + 1) = 'r' or else Source (S + 1) = 'R')
        and then (Source (S + 2) = 'a' or else Source (S + 2) = 'A')
        and then (Source (S + 3) = 'g' or else Source (S + 3) = 'G')
        and then (Source (S + 4) = 'm' or else Source (S + 4) = 'M')
        and then (Source (S + 5) = 'a' or else Source (S + 5) = 'A')
        and then (Source (S + 6) in Line_Terminator
                   or else Is_White_Space (Source (S + 6)))
      then
         return;

         --  Otherwise we have the style violation we are looking for

      else
         if Token = Tok_Then then
            Error_Msg -- CODEFIX
              ("(style) no statements may follow THEN on same line", S);
         else
            Error_Msg
              ("(style) no statements may follow ELSE on same line", S);
         end if;
      end if;
   end Check_Separate_Stmt_Lines_Cont;

   ----------------
   -- Check_Then --
   ----------------

   --  In check if then layout mode (-gnatyi), we expect a THEN keyword to
   --  appear either on the same line as the IF, or on a separate line if
   --  the IF statement extends for more than one line.

   procedure Check_Then (If_Loc : Source_Ptr) is
   begin
      if Style_Check_If_Then_Layout then
         declare
            If_Line   : constant Physical_Line_Number :=
              Get_Physical_Line_Number (If_Loc);
            Then_Line : constant Physical_Line_Number :=
              Get_Physical_Line_Number (Token_Ptr);
         begin
            if If_Line = Then_Line then
               null;
            elsif Token_Ptr /= First_Non_Blank_Location then
               Error_Msg_SC ("(style) misplaced THEN");
            end if;
         end;
      end if;
   end Check_Then;

   -------------------------------
   -- Check_Unary_Plus_Or_Minus --
   -------------------------------

   --  In check token mode (-gnatyt), unary plus or minus must not be
   --  followed by a space.

   --  Annoying exception: if we have the sequence =>+ within a Depends or
   --  Refined_Depends pragma or aspect, then we insist on a space rather
   --  than forbidding it.

   procedure Check_Unary_Plus_Or_Minus (Inside_Depends : Boolean := False) is
   begin
      if Style_Check_Tokens then
         if Inside_Depends then
            Require_Following_Space;
         else
            Check_No_Space_After;
         end if;
      end if;
   end Check_Unary_Plus_Or_Minus;

   ------------------------
   -- Check_Vertical_Bar --
   ------------------------

   --  In check token mode (-gnatyt), vertical bar must be surrounded by spaces

   procedure Check_Vertical_Bar is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
      end if;
   end Check_Vertical_Bar;

   -----------------------
   -- Check_Xtra_Parens --
   -----------------------

   procedure Check_Xtra_Parens (Loc : Source_Ptr) is
   begin
      if Style_Check_Xtra_Parens then
         Error_Msg -- CODEFIX
           ("(style) redundant parentheses", Loc);
      end if;
   end Check_Xtra_Parens;

   ----------------------------
   -- Determine_Token_Casing --
   ----------------------------

   function Determine_Token_Casing return Casing_Type is
   begin
      return Determine_Casing (Source (Token_Ptr .. Scan_Ptr - 1));
   end Determine_Token_Casing;

   -----------------------------
   -- Error_Space_Not_Allowed --
   -----------------------------

   procedure Error_Space_Not_Allowed (S : Source_Ptr) is
   begin
      Error_Msg -- CODEFIX
        ("(style) space not allowed", S);
   end Error_Space_Not_Allowed;

   --------------------------
   -- Error_Space_Required --
   --------------------------

   procedure Error_Space_Required (S : Source_Ptr) is
   begin
      Error_Msg -- CODEFIX
        ("(style) space required", S);
   end Error_Space_Required;

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space (C : Character) return Boolean is
   begin
      return C = ' ' or else C = HT;
   end Is_White_Space;

   -------------------
   -- Mode_In_Check --
   -------------------

   function Mode_In_Check return Boolean is
   begin
      return Style_Check and Style_Check_Mode_In;
   end Mode_In_Check;

   -----------------
   -- No_End_Name --
   -----------------

   --  In check end/exit labels mode (-gnatye), always require the name of
   --  a subprogram or package to be present on the END, so this is an error.

   procedure No_End_Name (Name : Node_Id) is
   begin
      if Style_Check_End_Labels then
         Error_Msg_Node_1 := Name;
         Error_Msg_SP -- CODEFIX
           ("(style) `END &` required");
      end if;
   end No_End_Name;

   ------------------
   -- No_Exit_Name --
   ------------------

   --  In check end/exit labels mode (-gnatye), always require the name of
   --  the loop to be present on the EXIT when exiting a named loop.

   procedure No_Exit_Name (Name : Node_Id) is
   begin
      if Style_Check_End_Labels then
         Error_Msg_Node_1 := Name;
         Error_Msg_SP -- CODEFIX
           ("(style) `EXIT &` required");
      end if;
   end No_Exit_Name;

   ----------------------------
   -- Non_Lower_Case_Keyword --
   ----------------------------

   --  In check casing mode (-gnatyk), reserved keywords must be spelled
   --  in all lower case (excluding keywords range, access, delta and digits
   --  used as attribute designators).

   procedure Non_Lower_Case_Keyword is
   begin
      if Style_Check_Keyword_Casing then
         Error_Msg_SC -- CODEFIX
           ("(style) reserved words must be all lower case");
      end if;
   end Non_Lower_Case_Keyword;

   -----------------------------
   -- Require_Following_Space --
   -----------------------------

   procedure Require_Following_Space is
   begin
      if Source (Scan_Ptr) > ' ' then
         Error_Space_Required (Scan_Ptr);
      end if;
   end Require_Following_Space;

   -----------------------------
   -- Require_Preceding_Space --
   -----------------------------

   procedure Require_Preceding_Space is
   begin
      if Token_Ptr > Source_First (Current_Source_File)
        and then Source (Token_Ptr - 1) > ' '
      then
         Error_Space_Required (Token_Ptr);
      end if;
   end Require_Preceding_Space;

end Styleg;
