------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S T Y L E G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

--  This version of the Style package implements the standard GNAT style
--  checking rules. For documentation of these rules, see comments on the
--  individual procedures.

with Casing;   use Casing;
with Csets;    use Csets;
with Err_Vars; use Err_Vars;
with Opt;      use Opt;
with Scans;    use Scans;
with Sinput;   use Sinput;
with Stylesw;  use Stylesw;

package body Styleg is

   use ASCII;

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

   function Determine_Token_Casing return Casing_Type;

   procedure Error_Space_Not_Allowed (S : Source_Ptr);
   --  Posts an error message indicating that a space is not allowed
   --  at the given source location.

   procedure Error_Space_Required (S : Source_Ptr);
   --  Posts an error message indicating that a space is required at
   --  the given source location.

   function Is_White_Space (C : Character) return Boolean;
   pragma Inline (Is_White_Space);
   --  Returns True for space, HT, VT or FF, False otherwise

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

   --  In check tokens mode (-gnatyt), ABS/NOT must be followed by a space

   procedure Check_Abs_Not is
   begin
      if Style_Check_Tokens then
         if Source (Scan_Ptr) > ' ' then
            Error_Space_Required (Scan_Ptr);
         end if;
      end if;
   end Check_Abs_Not;

   ----------------------
   -- Check_Apostrophe --
   ----------------------

   --  Do not allow space before or after apostrophe

   procedure Check_Apostrophe is
   begin
      if Style_Check_Tokens then
         Check_No_Space_After;
      end if;
   end Check_Apostrophe;

   -----------------
   -- Check_Arrow --
   -----------------

   --  In check tokens mode (-gnatys), arrow must be surrounded by spaces

   procedure Check_Arrow is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
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
            Error_Msg_SC ("(style) bad capitalization, mixed case required");
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
   --       line must have at least one blank after the second minus.

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
   --
   --       Note: for GNAT internal files (-gnatg switch set on for the
   --       compilation), the only special sequence recognized and allowed
   --       is --! as generated by gnatprep.

   procedure Check_Comment is
      S : Source_Ptr;
      C : Character;

      function Is_Box_Comment return Boolean;
      --  Returns True if the last two characters on the line are -- which
      --  characterizes a box comment (as for example follows this spec).

      --------------------
      -- Is_Box_Comment --
      --------------------

      function Is_Box_Comment return Boolean is
         S : Source_Ptr;

      begin
         S := Scan_Ptr + 3;
         while Source (S) not in Line_Terminator loop
            S := S + 1;
         end loop;

         return Source (S - 1) = '-' and then Source (S - 2) = '-';
      end Is_Box_Comment;

   --  Start of processing for Check_Comment

   begin
      --  Can never have a non-blank character preceding the first minus

      if Style_Check_Comments then
         if Scan_Ptr > Source_First (Current_Source_File)
           and then Source (Scan_Ptr - 1) > ' '
         then
            Error_Msg_S ("(style) space required");
         end if;
      end if;

      --  For a comment that is not at the start of the line, the only
      --  requirement is that we cannot have a non-blank character after
      --  the second minus sign.

      if Scan_Ptr /= First_Non_Blank_Location then
         if Style_Check_Comments then
            if Source (Scan_Ptr + 2) > ' ' then
               Error_Msg ("(style) space required", Scan_Ptr + 2);
            end if;
         end if;

         return;

      --  Case of a comment that is at the start of a line

      else
         --  First check, must be in appropriately indented column

         if Style_Check_Indentation /= 0 then
            if Start_Column rem Style_Check_Indentation /= 0 then
               Error_Msg_S ("(style) bad column");
               return;
            end if;
         end if;

         --  If we are not checking comments, nothing to do

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

            if GNAT_Mode then
               if C = '!' then
                  return;
               end if;

            else
               if Character'Pos (C) in 16#21# .. 16#2F#
                    or else
                  Character'Pos (C) in 16#3A# .. 16#3F#
               then
                  return;
               end if;
            end if;

            --  The only other case in which we allow a character after
            --  the -- other than a space is when we have a row of minus
            --  signs (case of header lines for a box comment for example).

            S := Scan_Ptr + 2;
            while Source (S) >= ' ' loop
               if Source (S) /= '-' then
                  if Is_Box_Comment then
                     Error_Space_Required (Scan_Ptr + 2);
                  else
                     Error_Msg ("(style) two spaces required", Scan_Ptr + 2);
                  end if;

                  return;
               end if;

               S := S + 1;
            end loop;

         --  If we are followed by a blank, then the comment is OK if the
         --  character following this blank is another blank or a format
         --  effector.

         elsif Source (Scan_Ptr + 3) <= ' ' then
            return;

         --  Here is the case where we only have one blank after the two
         --  minus signs, which is an error unless the line ends with two
         --  minus signs, the case of a box comment.

         elsif not Is_Box_Comment then
            Error_Space_Required (Scan_Ptr + 3);
         end if;
      end if;
   end Check_Comment;

   -------------------
   -- Check_Dot_Dot --
   -------------------

   --  In check token mode (-gnatyt), colon must be surrounded by spaces

   procedure Check_Dot_Dot is
   begin
      if Style_Check_Tokens then
         Require_Preceding_Space;
         Require_Following_Space;
      end if;
   end Check_Dot_Dot;

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
         Error_Msg_S ("(style) horizontal tab not allowed");
      end if;
   end Check_HT;

   -----------------------
   -- Check_Indentation --
   -----------------------

   --  In check indentation mode (-gnatyn for n a digit), a new statement or
   --  declaration is required to start in a column that is a multiple of the
   --  indentiation amount.

   procedure Check_Indentation is
   begin
      if Style_Check_Indentation /= 0 then
         if Token_Ptr = First_Non_Blank_Location
           and then Start_Column rem Style_Check_Indentation /= 0
         then
            Error_Msg_SC ("(style) bad indentation");
         end if;
      end if;
   end Check_Indentation;

   ----------------------
   -- Check_Left_Paren --
   ----------------------

   --  In tone check mode (-gnatyt), left paren must not be preceded by an
   --  identifier character or digit (a separating space is required) and
   --  may never be followed by a space.

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
   -- Check_Line_Terminator --
   ---------------------------

   --  In check blanks at end mode (-gnatyb), lines may not end with a
   --  trailing space.

   --  In check max line length mode (-gnatym), the line length must
   --  not exceed the permitted maximum value.

   --  In check form feeds mode (-gnatyf), the line terminator may not
   --  be either of the characters FF or VT.

   procedure Check_Line_Terminator (Len : Int) is
      S : Source_Ptr;

   begin
      --  Check FF/VT terminators

      if Style_Check_Form_Feeds then
         if Source (Scan_Ptr) = ASCII.FF then
            Error_Msg_S ("(style) form feed not allowed");

         elsif Source (Scan_Ptr) = ASCII.VT then
            Error_Msg_S ("(style) vertical tab not allowed");
         end if;
      end if;

      --  We are now possibly going to check for trailing spaces and maximum
      --  line length. There is no point in doing this if the current line is
      --  empty. It is actually wrong in the case of trailing spaces, because
      --  we scan backwards for this purpose, so we would end up looking at a
      --  different line, or even at invalid buffer locations if we have the
      --  first source line at hand.

      if Len = 0 then
         return;
      end if;

      --  Check trailing space

      if Style_Check_Blanks_At_End then
         if Scan_Ptr >= First_Non_Blank_Location then
            if Is_White_Space (Source (Scan_Ptr - 1)) then
               S := Scan_Ptr - 1;

               while Is_White_Space (Source (S - 1)) loop
                  S := S - 1;
               end loop;

               Error_Msg ("(style) trailing spaces not permitted", S);
            end if;
         end if;
      end if;

      --  Check max line length

      if Style_Check_Max_Line_Length then
         if Len > Style_Max_Line_Length then
            Error_Msg
              ("(style) this line is too long",
               Current_Line_Start + Source_Ptr (Style_Max_Line_Length));
         end if;
      end if;

   end Check_Line_Terminator;

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
            Error_Msg_SC ("(style) bad capitalization, mixed case required");
         end if;
      end if;
   end Check_Pragma_Name;

   -----------------------
   -- Check_Right_Paren --
   -----------------------

   --  In check tokens mode (-gnatyt), right paren must never be preceded by
   --  a space unless it is the initial non-blank character on the line.

   procedure Check_Right_Paren is
   begin
      if Style_Check_Tokens then
         Check_No_Space_Before;
      end if;
   end Check_Right_Paren;

   ---------------------
   -- Check_Semicolon --
   ---------------------

   --  In check tokens mode (-gnatyt), semicolon does not permit a preceding
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

   ----------------
   -- Check_Then --
   ----------------

   --  In check if then layout mode (-gnatyi), we expect a THEN keyword
   --  to appear either on the same line as the IF, or on a separate line
   --  after multiple conditions. In any case, it may not appear on the
   --  line immediately following the line with the IF.

   procedure Check_Then (If_Loc : Source_Ptr) is
   begin
      if Style_Check_If_Then_Layout then
         if Get_Physical_Line_Number (Token_Ptr) =
            Get_Physical_Line_Number (If_Loc) + 1
         then
            Error_Msg_SC ("(style) misplaced THEN");
         end if;
      end if;
   end Check_Then;

   -------------------------------
   -- Check_Unary_Plus_Or_Minus --
   -------------------------------

   --  In check tokem mode (-gnatyt), unary plus or minus must not be
   --  followed by a space.

   procedure Check_Unary_Plus_Or_Minus is
   begin
      if Style_Check_Tokens then
         Check_No_Space_After;
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
         Error_Msg ("redundant parentheses?", Loc);
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
      Error_Msg ("(style) space not allowed", S);
   end Error_Space_Not_Allowed;

   --------------------------
   -- Error_Space_Required --
   --------------------------

   procedure Error_Space_Required (S : Source_Ptr) is
   begin
      Error_Msg ("(style) space required", S);
   end Error_Space_Required;

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space (C : Character) return Boolean is
   begin
      return C = ' ' or else C = HT;
   end Is_White_Space;

   -----------------
   -- No_End_Name --
   -----------------

   --  In check end/exit labels mode (-gnatye), always require the name of
   --  a subprogram or package to be present on the END, so this is an error.

   procedure No_End_Name (Name : Node_Id) is
   begin
      if Style_Check_End_Labels then
         Error_Msg_Node_1 := Name;
         Error_Msg_SP ("(style) `END &` required");
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
         Error_Msg_SP ("(style) `EXIT &` required");
      end if;
   end No_Exit_Name;

   ----------------------------
   -- Non_Lower_Case_Keyword --
   ----------------------------

   --  In check casing mode (-gnatyk), reserved keywords must be be spelled
   --  in all lower case (excluding keywords range, access, delta and digits
   --  used as attribute designators).

   procedure Non_Lower_Case_Keyword is
   begin
      if Style_Check_Keyword_Casing then
         Error_Msg_SC ("(style) reserved words must be all lower case");
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

   ---------------------
   -- RM_Column_Check --
   ---------------------

   function RM_Column_Check return Boolean is
   begin
      return Style_Check and Style_Check_Layout;
   end RM_Column_Check;

end Styleg;
