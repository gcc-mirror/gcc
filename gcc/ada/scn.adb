------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  S C N                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
with Csets;    use Csets;
with Errout;   use Errout;
with Hostparm; use Hostparm;
with Namet;    use Namet;
with Opt;      use Opt;
with Scans;    use Scans;
with Sinput;   use Sinput;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Style;
with Widechar; use Widechar;

with System.CRC32;
with System.WCh_Con; use System.WCh_Con;

package body Scn is

   use ASCII;
   --  Make control characters visible

   Used_As_Identifier : array (Token_Type) of Boolean;
   --  Flags set True if a given keyword is used as an identifier (used to
   --  make sure that we only post an error message for incorrect use of a
   --  keyword as an identifier once for a given keyword).

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Accumulate_Checksum (C : Character);
   pragma Inline (Accumulate_Checksum);
   --  This routine accumulates the checksum given character C. During the
   --  scanning of a source file, this routine is called with every character
   --  in the source, excluding blanks, and all control characters (except
   --  that ESC is included in the checksum). Upper case letters not in string
   --  literals are folded by the caller. See Sinput spec for the documentation
   --  of the checksum algorithm. Note: checksum values are only used if we
   --  generate code, so it is not necessary to worry about making the right
   --  sequence of calls in any error situation.

   procedure Accumulate_Checksum (C : Char_Code);
   pragma Inline (Accumulate_Checksum);
   --  This version is identical, except that the argument, C, is a character
   --  code value instead of a character. This is used when wide characters
   --  are scanned. We use the character code rather than the ASCII characters
   --  so that the checksum is independent of wide character encoding method.

   procedure Initialize_Checksum;
   pragma Inline (Initialize_Checksum);
   --  Initialize checksum value

   procedure Check_End_Of_Line;
   --  Called when end of line encountered. Checks that line is not
   --  too long, and that other style checks for the end of line are met.

   function Determine_License return License_Type;
   --  Scan header of file and check that it has an appropriate GNAT-style
   --  header with a proper license statement. Returns GPL, Unrestricted,
   --  or Modified_GPL depending on header. If none of these, returns Unknown.

   function Double_Char_Token (C : Character) return Boolean;
   --  This function is used for double character tokens like := or <>. It
   --  checks if the character following Source (Scan_Ptr) is C, and if so
   --  bumps Scan_Ptr past the pair of characters and returns True. A space
   --  between the two characters is also recognized with an appropriate
   --  error message being issued. If C is not present, False is returned.
   --  Note that Double_Char_Token can only be used for tokens defined in
   --  the Ada syntax (it's use for error cases like && is not appropriate
   --  since we do not want a junk message for a case like &-space-&).

   procedure Error_Illegal_Character;
   --  Give illegal character error, Scan_Ptr points to character. On return,
   --  Scan_Ptr is bumped past the illegal character.

   procedure Error_Illegal_Wide_Character;
   --  Give illegal wide character message. On return, Scan_Ptr is bumped
   --  past the illegal character, which may still leave us pointing to
   --  junk, not much we can do if the escape sequence is messed up!

   procedure Error_Long_Line;
   --  Signal error of excessively long line

   procedure Error_No_Double_Underline;
   --  Signal error of double underline character

   procedure Nlit;
   --  This is the procedure for scanning out numeric literals. On entry,
   --  Scan_Ptr points to the digit that starts the numeric literal (the
   --  checksum for this character has not been accumulated yet). On return
   --  Scan_Ptr points past the last character of the numeric literal, Token
   --  and Token_Node are set appropriately, and the checksum is updated.

   function Set_Start_Column return Column_Number;
   --  This routine is called with Scan_Ptr pointing to the first character
   --  of a line. On exit, Scan_Ptr is advanced to the first non-blank
   --  character of this line (or to the terminating format effector if the
   --  line contains no non-blank characters), and the returned result is the
   --  column number of this non-blank character (zero origin), which is the
   --  value to be stored in the Start_Column scan variable.

   procedure Slit;
   --  This is the procedure for scanning out string literals. On entry,
   --  Scan_Ptr points to the opening string quote (the checksum for this
   --  character has not been accumulated yet). On return Scan_Ptr points
   --  past the closing quote of the string literal, Token and Token_Node
   --  are set appropriately, and the checksum is upated.

   -------------------------
   -- Accumulate_Checksum --
   -------------------------

   procedure Accumulate_Checksum (C : Character) is
   begin
      System.CRC32.Update (System.CRC32.CRC32 (Checksum), C);
   end Accumulate_Checksum;

   procedure Accumulate_Checksum (C : Char_Code) is
   begin
      Accumulate_Checksum (Character'Val (C / 256));
      Accumulate_Checksum (Character'Val (C mod 256));
   end Accumulate_Checksum;

   -----------------------
   -- Check_End_Of_Line --
   -----------------------

   procedure Check_End_Of_Line is
      Len : constant Int := Int (Scan_Ptr) - Int (Current_Line_Start);

   begin
      if Len > Hostparm.Max_Line_Length then
         Error_Long_Line;

      elsif Style_Check then
         Style.Check_Line_Terminator (Len);
      end if;
   end Check_End_Of_Line;

   -----------------------
   -- Determine_License --
   -----------------------

   function Determine_License return License_Type is
      GPL_Found : Boolean := False;

      function Contains (S : String) return Boolean;
      --  See if current comment contains successive non-blank characters
      --  matching the contents of S. If so leave Scan_Ptr unchanged and
      --  return True, otherwise leave Scan_Ptr unchanged and return False.

      procedure Skip_EOL;
      --  Skip to line terminator character

      --------------
      -- Contains --
      --------------

      function Contains (S : String) return Boolean is
         CP : Natural;
         SP : Source_Ptr;
         SS : Source_Ptr;

      begin
         SP := Scan_Ptr;
         while Source (SP) /= CR and then Source (SP) /= LF loop
            if Source (SP) = S (S'First) then
               SS := SP;
               CP := S'First;

               loop
                  SS := SS + 1;
                  CP := CP + 1;

                  if CP > S'Last then
                     return True;
                  end if;

                  while Source (SS) = ' ' loop
                     SS := SS + 1;
                  end loop;

                  exit when Source (SS) /= S (CP);
               end loop;
            end if;

            SP := SP + 1;
         end loop;

         return False;
      end Contains;

      --------------
      -- Skip_EOL --
      --------------

      procedure Skip_EOL is
      begin
         while Source (Scan_Ptr) /= CR
           and then Source (Scan_Ptr) /= LF
         loop
            Scan_Ptr := Scan_Ptr + 1;
         end loop;
      end Skip_EOL;

   --  Start of processing for Determine_License

   begin
      loop
         if Source (Scan_Ptr) /= '-'
           or else Source (Scan_Ptr + 1) /= '-'
         then
            if GPL_Found then
               return GPL;
            else
               return Unknown;
            end if;

         elsif Contains ("Asaspecialexception") then
            if GPL_Found then
               return Modified_GPL;
            end if;

         elsif Contains ("GNUGeneralPublicLicense") then
            GPL_Found := True;

         elsif
             Contains
               ("ThisspecificationisadaptedfromtheAdaSemanticInterface")
           or else
             Contains
              ("ThisspecificationisderivedfromtheAdaReferenceManual")
         then
            return Unrestricted;
         end if;

         Skip_EOL;

         Check_End_Of_Line;

         declare
            Physical : Boolean;

         begin
            Skip_Line_Terminators (Scan_Ptr, Physical);

            --  If we are at start of physical line, update scan pointers
            --  to reflect the start of the new line.

            if Physical then
               Current_Line_Start       := Scan_Ptr;
               Start_Column             := Set_Start_Column;
               First_Non_Blank_Location := Scan_Ptr;
            end if;
         end;
      end loop;
   end Determine_License;

   ----------------------------
   -- Determine_Token_Casing --
   ----------------------------

   function Determine_Token_Casing return Casing_Type is
   begin
      return Determine_Casing (Source (Token_Ptr .. Scan_Ptr - 1));
   end Determine_Token_Casing;

   -----------------------
   -- Double_Char_Token --
   -----------------------

   function Double_Char_Token (C : Character) return Boolean is
   begin
      if Source (Scan_Ptr + 1) = C then
         Accumulate_Checksum (C);
         Scan_Ptr := Scan_Ptr + 2;
         return True;

      elsif Source (Scan_Ptr + 1) = ' '
        and then Source (Scan_Ptr + 2) = C
      then
         Scan_Ptr := Scan_Ptr + 1;
         Error_Msg_S ("no space allowed here");
         Scan_Ptr := Scan_Ptr + 2;
         return True;

      else
         return False;
      end if;
   end Double_Char_Token;

   -----------------------------
   -- Error_Illegal_Character --
   -----------------------------

   procedure Error_Illegal_Character is
   begin
      Error_Msg_S ("illegal character");
      Scan_Ptr := Scan_Ptr + 1;
   end Error_Illegal_Character;

   ----------------------------------
   -- Error_Illegal_Wide_Character --
   ----------------------------------

   procedure Error_Illegal_Wide_Character is
   begin
      if OpenVMS then
         Error_Msg_S
           ("illegal wide character, check " &
            "'/'W'I'D'E'_'C'H'A'R'A'C'T'E'R'_'E'N'C'O'D'I'N'G qualifer");
      else
         Error_Msg_S
           ("illegal wide character, check -gnatW switch");
      end if;

      Scan_Ptr := Scan_Ptr + 1;
   end Error_Illegal_Wide_Character;

   ---------------------
   -- Error_Long_Line --
   ---------------------

   procedure Error_Long_Line is
   begin
      Error_Msg
        ("this line is too long",
         Current_Line_Start + Hostparm.Max_Line_Length);
   end Error_Long_Line;

   -------------------------------
   -- Error_No_Double_Underline --
   -------------------------------

   procedure Error_No_Double_Underline is
   begin
      Error_Msg_S ("two consecutive underlines not permitted");
   end Error_No_Double_Underline;

   -------------------------
   -- Initialize_Checksum --
   -------------------------

   procedure Initialize_Checksum is
   begin
      System.CRC32.Initialize (System.CRC32.CRC32 (Checksum));
   end Initialize_Checksum;

   ------------------------
   -- Initialize_Scanner --
   ------------------------

   procedure Initialize_Scanner
     (Unit  : Unit_Number_Type;
      Index : Source_File_Index)
   is
      GNAT_Hedr : constant Text_Buffer (1 .. 78) := (others => '-');

   begin
      --  Set up Token_Type values in Names Table entries for reserved keywords
      --  We use the Pos value of the Token_Type value. Note we are relying on
      --  the fact that Token_Type'Val (0) is not a reserved word!

      Set_Name_Table_Byte (Name_Abort,      Token_Type'Pos (Tok_Abort));
      Set_Name_Table_Byte (Name_Abs,        Token_Type'Pos (Tok_Abs));
      Set_Name_Table_Byte (Name_Abstract,   Token_Type'Pos (Tok_Abstract));
      Set_Name_Table_Byte (Name_Accept,     Token_Type'Pos (Tok_Accept));
      Set_Name_Table_Byte (Name_Access,     Token_Type'Pos (Tok_Access));
      Set_Name_Table_Byte (Name_And,        Token_Type'Pos (Tok_And));
      Set_Name_Table_Byte (Name_Aliased,    Token_Type'Pos (Tok_Aliased));
      Set_Name_Table_Byte (Name_All,        Token_Type'Pos (Tok_All));
      Set_Name_Table_Byte (Name_Array,      Token_Type'Pos (Tok_Array));
      Set_Name_Table_Byte (Name_At,         Token_Type'Pos (Tok_At));
      Set_Name_Table_Byte (Name_Begin,      Token_Type'Pos (Tok_Begin));
      Set_Name_Table_Byte (Name_Body,       Token_Type'Pos (Tok_Body));
      Set_Name_Table_Byte (Name_Case,       Token_Type'Pos (Tok_Case));
      Set_Name_Table_Byte (Name_Constant,   Token_Type'Pos (Tok_Constant));
      Set_Name_Table_Byte (Name_Declare,    Token_Type'Pos (Tok_Declare));
      Set_Name_Table_Byte (Name_Delay,      Token_Type'Pos (Tok_Delay));
      Set_Name_Table_Byte (Name_Delta,      Token_Type'Pos (Tok_Delta));
      Set_Name_Table_Byte (Name_Digits,     Token_Type'Pos (Tok_Digits));
      Set_Name_Table_Byte (Name_Do,         Token_Type'Pos (Tok_Do));
      Set_Name_Table_Byte (Name_Else,       Token_Type'Pos (Tok_Else));
      Set_Name_Table_Byte (Name_Elsif,      Token_Type'Pos (Tok_Elsif));
      Set_Name_Table_Byte (Name_End,        Token_Type'Pos (Tok_End));
      Set_Name_Table_Byte (Name_Entry,      Token_Type'Pos (Tok_Entry));
      Set_Name_Table_Byte (Name_Exception,  Token_Type'Pos (Tok_Exception));
      Set_Name_Table_Byte (Name_Exit,       Token_Type'Pos (Tok_Exit));
      Set_Name_Table_Byte (Name_For,        Token_Type'Pos (Tok_For));
      Set_Name_Table_Byte (Name_Function,   Token_Type'Pos (Tok_Function));
      Set_Name_Table_Byte (Name_Generic,    Token_Type'Pos (Tok_Generic));
      Set_Name_Table_Byte (Name_Goto,       Token_Type'Pos (Tok_Goto));
      Set_Name_Table_Byte (Name_If,         Token_Type'Pos (Tok_If));
      Set_Name_Table_Byte (Name_In,         Token_Type'Pos (Tok_In));
      Set_Name_Table_Byte (Name_Is,         Token_Type'Pos (Tok_Is));
      Set_Name_Table_Byte (Name_Limited,    Token_Type'Pos (Tok_Limited));
      Set_Name_Table_Byte (Name_Loop,       Token_Type'Pos (Tok_Loop));
      Set_Name_Table_Byte (Name_Mod,        Token_Type'Pos (Tok_Mod));
      Set_Name_Table_Byte (Name_New,        Token_Type'Pos (Tok_New));
      Set_Name_Table_Byte (Name_Not,        Token_Type'Pos (Tok_Not));
      Set_Name_Table_Byte (Name_Null,       Token_Type'Pos (Tok_Null));
      Set_Name_Table_Byte (Name_Of,         Token_Type'Pos (Tok_Of));
      Set_Name_Table_Byte (Name_Or,         Token_Type'Pos (Tok_Or));
      Set_Name_Table_Byte (Name_Others,     Token_Type'Pos (Tok_Others));
      Set_Name_Table_Byte (Name_Out,        Token_Type'Pos (Tok_Out));
      Set_Name_Table_Byte (Name_Package,    Token_Type'Pos (Tok_Package));
      Set_Name_Table_Byte (Name_Pragma,     Token_Type'Pos (Tok_Pragma));
      Set_Name_Table_Byte (Name_Private,    Token_Type'Pos (Tok_Private));
      Set_Name_Table_Byte (Name_Procedure,  Token_Type'Pos (Tok_Procedure));
      Set_Name_Table_Byte (Name_Protected,  Token_Type'Pos (Tok_Protected));
      Set_Name_Table_Byte (Name_Raise,      Token_Type'Pos (Tok_Raise));
      Set_Name_Table_Byte (Name_Range,      Token_Type'Pos (Tok_Range));
      Set_Name_Table_Byte (Name_Record,     Token_Type'Pos (Tok_Record));
      Set_Name_Table_Byte (Name_Rem,        Token_Type'Pos (Tok_Rem));
      Set_Name_Table_Byte (Name_Renames,    Token_Type'Pos (Tok_Renames));
      Set_Name_Table_Byte (Name_Requeue,    Token_Type'Pos (Tok_Requeue));
      Set_Name_Table_Byte (Name_Return,     Token_Type'Pos (Tok_Return));
      Set_Name_Table_Byte (Name_Reverse,    Token_Type'Pos (Tok_Reverse));
      Set_Name_Table_Byte (Name_Select,     Token_Type'Pos (Tok_Select));
      Set_Name_Table_Byte (Name_Separate,   Token_Type'Pos (Tok_Separate));
      Set_Name_Table_Byte (Name_Subtype,    Token_Type'Pos (Tok_Subtype));
      Set_Name_Table_Byte (Name_Tagged,     Token_Type'Pos (Tok_Tagged));
      Set_Name_Table_Byte (Name_Task,       Token_Type'Pos (Tok_Task));
      Set_Name_Table_Byte (Name_Terminate,  Token_Type'Pos (Tok_Terminate));
      Set_Name_Table_Byte (Name_Then,       Token_Type'Pos (Tok_Then));
      Set_Name_Table_Byte (Name_Type,       Token_Type'Pos (Tok_Type));
      Set_Name_Table_Byte (Name_Until,      Token_Type'Pos (Tok_Until));
      Set_Name_Table_Byte (Name_Use,        Token_Type'Pos (Tok_Use));
      Set_Name_Table_Byte (Name_When,       Token_Type'Pos (Tok_When));
      Set_Name_Table_Byte (Name_While,      Token_Type'Pos (Tok_While));
      Set_Name_Table_Byte (Name_With,       Token_Type'Pos (Tok_With));
      Set_Name_Table_Byte (Name_Xor,        Token_Type'Pos (Tok_Xor));

      --  Initialize scan control variables

      Current_Source_File       := Index;
      Source                    := Source_Text (Current_Source_File);
      Current_Source_Unit       := Unit;
      Scan_Ptr                  := Source_First (Current_Source_File);
      Token                     := No_Token;
      Token_Ptr                 := Scan_Ptr;
      Current_Line_Start        := Scan_Ptr;
      Token_Node                := Empty;
      Token_Name                := No_Name;
      Start_Column              := Set_Start_Column;
      First_Non_Blank_Location  := Scan_Ptr;

      Initialize_Checksum;

      --  Set default for Comes_From_Source. All nodes built now until we
      --  reenter the analyzer will have Comes_From_Source set to True

      Set_Comes_From_Source_Default (True);

      --  Check license if GNAT type header possibly present

      if Source_Last (Index) - Scan_Ptr > 80
        and then Source (Scan_Ptr .. Scan_Ptr + 77) = GNAT_Hedr
      then
         Set_License (Current_Source_File, Determine_License);
      end if;

      --  Scan initial token (note this initializes Prev_Token, Prev_Token_Ptr)

      Scan;

      --  Clear flags for reserved words used as identifiers

      for J in Token_Type loop
         Used_As_Identifier (J) := False;
      end loop;

   end Initialize_Scanner;

   ----------
   -- Nlit --
   ----------

   procedure Nlit is separate;

   ----------
   -- Scan --
   ----------

   procedure Scan is
   begin
      Prev_Token := Token;
      Prev_Token_Ptr := Token_Ptr;
      Token_Name := Error_Name;

      --  The following loop runs more than once only if a format effector
      --  (tab, vertical tab, form  feed, line feed, carriage return) is
      --  encountered and skipped, or some error situation, such as an
      --  illegal character, is encountered.

      loop
         --  Skip past blanks, loop is opened up for speed

         while Source (Scan_Ptr) = ' ' loop

            if Source (Scan_Ptr + 1) /= ' ' then
               Scan_Ptr := Scan_Ptr + 1;
               exit;
            end if;

            if Source (Scan_Ptr + 2) /= ' ' then
               Scan_Ptr := Scan_Ptr + 2;
               exit;
            end if;

            if Source (Scan_Ptr + 3) /= ' ' then
               Scan_Ptr := Scan_Ptr + 3;
               exit;
            end if;

            if Source (Scan_Ptr + 4) /= ' ' then
               Scan_Ptr := Scan_Ptr + 4;
               exit;
            end if;

            if Source (Scan_Ptr + 5) /= ' ' then
               Scan_Ptr := Scan_Ptr + 5;
               exit;
            end if;

            if Source (Scan_Ptr + 6) /= ' ' then
               Scan_Ptr := Scan_Ptr + 6;
               exit;
            end if;

            if Source (Scan_Ptr + 7) /= ' ' then
               Scan_Ptr := Scan_Ptr + 7;
               exit;
            end if;

            Scan_Ptr := Scan_Ptr + 8;
         end loop;

         --  We are now at a non-blank character, which is the first character
         --  of the token we will scan, and hence the value of Token_Ptr.

         Token_Ptr := Scan_Ptr;

         --  Here begins the main case statement which transfers control on
         --  the basis of the non-blank character we have encountered.

         case Source (Scan_Ptr) is

         --  Line terminator characters

         when CR | LF | FF | VT => Line_Terminator_Case : begin

            --  Check line too long

            Check_End_Of_Line;

            declare
               Physical : Boolean;

            begin
               Skip_Line_Terminators (Scan_Ptr, Physical);

               --  If we are at start of physical line, update scan pointers
               --  to reflect the start of the new line.

               if Physical then
                  Current_Line_Start       := Scan_Ptr;
                  Start_Column             := Set_Start_Column;
                  First_Non_Blank_Location := Scan_Ptr;
               end if;
            end;
         end Line_Terminator_Case;

         --  Horizontal tab, just skip past it

         when HT =>
            if Style_Check then Style.Check_HT; end if;
            Scan_Ptr := Scan_Ptr + 1;

         --  End of file character, treated as an end of file only if it
         --  is the last character in the buffer, otherwise it is ignored.

         when EOF =>
            if Scan_Ptr = Source_Last (Current_Source_File) then
               Check_End_Of_Line;
               Token := Tok_EOF;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
            end if;

         --  Ampersand

         when '&' =>
            Accumulate_Checksum ('&');

            if Source (Scan_Ptr + 1) = '&' then
               Error_Msg_S ("'&'& should be `AND THEN`");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_And;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Ampersand;
               return;
            end if;

         --  Asterisk (can be multiplication operator or double asterisk
         --  which is the exponentiation compound delimtier).

         when '*' =>
            Accumulate_Checksum ('*');

            if Source (Scan_Ptr + 1) = '*' then
               Accumulate_Checksum ('*');
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Double_Asterisk;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Asterisk;
               return;
            end if;

         --  Colon, which can either be an isolated colon, or part of an
         --  assignment compound delimiter.

         when ':' =>
            Accumulate_Checksum (':');

            if Double_Char_Token ('=') then
               Token := Tok_Colon_Equal;
               if Style_Check then Style.Check_Colon_Equal; end if;
               return;

            elsif Source (Scan_Ptr + 1) = '-'
              and then Source (Scan_Ptr + 2) /= '-'
            then
               Token := Tok_Colon_Equal;
               Error_Msg (":- should be :=", Scan_Ptr);
               Scan_Ptr := Scan_Ptr + 2;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Colon;
               if Style_Check then Style.Check_Colon; end if;
               return;
            end if;

         --  Left parenthesis

         when '(' =>
            Accumulate_Checksum ('(');
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Left_Paren;
            if Style_Check then Style.Check_Left_Paren; end if;
            return;

         --  Left bracket

         when '[' =>
            if Source (Scan_Ptr + 1) = '"' then
               Name_Len := 0;
               goto Scan_Identifier;

            else
               Error_Msg_S ("illegal character, replaced by ""(""");
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Left_Paren;
               return;
            end if;

         --  Left brace

         when '{' =>
            Error_Msg_S ("illegal character, replaced by ""(""");
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Left_Paren;
            return;

         --  Comma

         when ',' =>
            Accumulate_Checksum (',');
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Comma;
            if Style_Check then Style.Check_Comma; end if;
            return;

         --  Dot, which is either an isolated period, or part of a double
         --  dot compound delimiter sequence. We also check for the case of
         --  a digit following the period, to give a better error message.

         when '.' =>
            Accumulate_Checksum ('.');

            if Double_Char_Token ('.') then
               Token := Tok_Dot_Dot;
               if Style_Check then Style.Check_Dot_Dot; end if;
               return;

            elsif Source (Scan_Ptr + 1) in '0' .. '9' then
               Error_Msg_S ("numeric literal cannot start with point");
               Scan_Ptr := Scan_Ptr + 1;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Dot;
               return;
            end if;

         --  Equal, which can either be an equality operator, or part of the
         --  arrow (=>) compound delimiter.

         when '=' =>
            Accumulate_Checksum ('=');

            if Double_Char_Token ('>') then
               Token := Tok_Arrow;
               if Style_Check then Style.Check_Arrow; end if;
               return;

            elsif Source (Scan_Ptr + 1) = '=' then
               Error_Msg_S ("== should be =");
               Scan_Ptr := Scan_Ptr + 1;
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Equal;
            return;

         --  Greater than, which can be a greater than operator, greater than
         --  or equal operator, or first character of a right label bracket.

         when '>' =>
            Accumulate_Checksum ('>');

            if Double_Char_Token ('=') then
               Token := Tok_Greater_Equal;
               return;

            elsif Double_Char_Token ('>') then
               Token := Tok_Greater_Greater;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Greater;
               return;
            end if;

         --  Less than, which can be a less than operator, less than or equal
         --  operator, or the first character of a left label bracket, or the
         --  first character of a box (<>) compound delimiter.

         when '<' =>
            Accumulate_Checksum ('<');

            if Double_Char_Token ('=') then
               Token := Tok_Less_Equal;
               return;

            elsif Double_Char_Token ('>') then
               Token := Tok_Box;
               if Style_Check then Style.Check_Box; end if;
               return;

            elsif Double_Char_Token ('<') then
               Token := Tok_Less_Less;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Less;
               return;
            end if;

         --  Minus, which is either a subtraction operator, or the first
         --  character of double minus starting a comment

         when '-' => Minus_Case : begin
            if Source (Scan_Ptr + 1) = '>' then
               Error_Msg_S ("invalid token");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Arrow;
               return;

            elsif Source (Scan_Ptr + 1) /= '-' then
               Accumulate_Checksum ('-');
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Minus;
               return;

            --  Comment

            else -- Source (Scan_Ptr + 1) = '-' then
               if Style_Check then Style.Check_Comment; end if;
               Scan_Ptr := Scan_Ptr + 2;

               --  Loop to scan comment (this loop runs more than once only if
               --  a horizontal tab or other non-graphic character is scanned)

               loop
                  --  Scan to non graphic character (opened up for speed)

                  loop
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                     exit when Source (Scan_Ptr) not in Graphic_Character;
                     Scan_Ptr := Scan_Ptr + 1;
                  end loop;

                  --  Keep going if horizontal tab

                  if Source (Scan_Ptr) = HT then
                     if Style_Check then Style.Check_HT; end if;
                     Scan_Ptr := Scan_Ptr + 1;

                  --  Terminate scan of comment if line terminator

                  elsif Source (Scan_Ptr) in Line_Terminator then
                     exit;

                  --  Terminate scan of comment if end of file encountered
                  --  (embedded EOF character or real last character in file)

                  elsif Source (Scan_Ptr) = EOF then
                     exit;

                  --  Keep going if character in 80-FF range, or is ESC. These
                  --  characters are allowed in comments by RM-2.1(1), 2.7(2).
                  --  They are allowed even in Ada 83 mode according to the
                  --  approved AI. ESC was added to the AI in June 93.

                  elsif Source (Scan_Ptr) in Upper_Half_Character
                    or else Source (Scan_Ptr) = ESC
                  then
                     Scan_Ptr := Scan_Ptr + 1;

                  --  Otherwise we have an illegal comment character

                  else
                     Error_Illegal_Character;
                  end if;

               end loop;

               --  Note that we do NOT execute a return here, instead we fall
               --  through to reexecute the scan loop to look for a token.

            end if;
         end Minus_Case;

         --  Double quote or percent starting a string literal

         when '"' | '%' =>
            Slit;
            return;

         --  Apostrophe. This can either be the start of a character literal,
         --  or an isolated apostrophe used in a qualified expression or an
         --  attribute. We treat it as a character literal if it does not
         --  follow a right parenthesis, identifier, the keyword ALL or
         --  a literal. This means that we correctly treat constructs like:

         --    A := CHARACTER'('A');

         --  Note that RM-2.2(7) does not require a separator between
         --  "CHARACTER" and "'" in the above.

         when ''' => Char_Literal_Case : declare
            Code : Char_Code;
            Err  : Boolean;

         begin
            Accumulate_Checksum (''');
            Scan_Ptr := Scan_Ptr + 1;

            --  Here is where we make the test to distinguish the cases. Treat
            --  as apostrophe if previous token is an identifier, right paren
            --  or the reserved word "all" (latter case as in A.all'Address)
            --  Also treat it as apostrophe after a literal (this catches
            --  some legitimate cases, like A."abs"'Address, and also gives
            --  better error behavior for impossible cases like 123'xxx).

            if Prev_Token = Tok_Identifier
               or else Prev_Token = Tok_Right_Paren
               or else Prev_Token = Tok_All
               or else Prev_Token in Token_Class_Literal
            then
               Token := Tok_Apostrophe;
               return;

            --  Otherwise the apostrophe starts a character literal

            else
               --  Case of wide character literal with ESC or [ encoding

               if (Source (Scan_Ptr) = ESC
                     and then
                    Wide_Character_Encoding_Method in WC_ESC_Encoding_Method)
                 or else
                   (Source (Scan_Ptr) in Upper_Half_Character
                     and then
                    Upper_Half_Encoding)
                 or else
                   (Source (Scan_Ptr) = '['
                     and then
                    Source (Scan_Ptr + 1) = '"')
               then
                  Scan_Wide (Source, Scan_Ptr, Code, Err);
                  Accumulate_Checksum (Code);

                  if Err then
                     Error_Illegal_Wide_Character;
                  end if;

                  if Source (Scan_Ptr) /= ''' then
                     Error_Msg_S ("missing apostrophe");
                  else
                     Scan_Ptr := Scan_Ptr + 1;
                  end if;

               --  If we do not find a closing quote in the expected place then
               --  assume that we have a misguided attempt at a string literal.

               --  However, if previous token is RANGE, then we return an
               --  apostrophe instead since this gives better error recovery

               elsif Source (Scan_Ptr + 1) /= ''' then

                  if Prev_Token = Tok_Range then
                     Token := Tok_Apostrophe;
                     return;

                  else
                     Scan_Ptr := Scan_Ptr - 1;
                     Error_Msg_S
                       ("strings are delimited by double quote character");
                     Scn.Slit;
                     return;
                  end if;

               --  Otherwise we have a (non-wide) character literal

               else
                  Accumulate_Checksum (Source (Scan_Ptr));

                  if Source (Scan_Ptr) not in Graphic_Character then
                     if Source (Scan_Ptr) in Upper_Half_Character then
                        if Ada_83 then
                           Error_Illegal_Character;
                        end if;

                     else
                        Error_Illegal_Character;
                     end if;
                  end if;

                  Code := Get_Char_Code (Source (Scan_Ptr));
                  Scan_Ptr := Scan_Ptr + 2;
               end if;

               --  Fall through here with Scan_Ptr updated past the closing
               --  quote, and Code set to the Char_Code value for the literal

               Accumulate_Checksum (''');
               Token := Tok_Char_Literal;
               Token_Node := New_Node (N_Character_Literal, Token_Ptr);
               Set_Char_Literal_Value (Token_Node, Code);
               Set_Character_Literal_Name (Code);
               Token_Name := Name_Find;
               Set_Chars (Token_Node, Token_Name);
               return;
            end if;
         end Char_Literal_Case;

         --  Right parenthesis

         when ')' =>
            Accumulate_Checksum (')');
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Right_Paren;
            if Style_Check then Style.Check_Right_Paren; end if;
            return;

         --  Right bracket or right brace, treated as right paren

         when ']' | '}' =>
            Error_Msg_S ("illegal character, replaced by "")""");
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Right_Paren;
            return;

         --  Slash (can be division operator or first character of not equal)

         when '/' =>
            Accumulate_Checksum ('/');

            if Double_Char_Token ('=') then
               Token := Tok_Not_Equal;
               return;
            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Slash;
               return;
            end if;

         --  Semicolon

         when ';' =>
            Accumulate_Checksum (';');
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Semicolon;
            if Style_Check then Style.Check_Semicolon; end if;
            return;

         --  Vertical bar

         when '|' => Vertical_Bar_Case : begin
            Accumulate_Checksum ('|');

            --  Special check for || to give nice message

            if Source (Scan_Ptr + 1) = '|' then
               Error_Msg_S ("""||"" should be `OR ELSE`");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Or;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Vertical_Bar;
               if Style_Check then Style.Check_Vertical_Bar; end if;
               return;
            end if;
         end Vertical_Bar_Case;

         --  Exclamation, replacement character for vertical bar

         when '!' => Exclamation_Case : begin
            Accumulate_Checksum ('!');

            if Source (Scan_Ptr + 1) = '=' then
               Error_Msg_S ("'!= should be /=");
               Scan_Ptr := Scan_Ptr + 2;
               Token := Tok_Not_Equal;
               return;

            else
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Vertical_Bar;
               return;
            end if;

         end Exclamation_Case;

         --  Plus

         when '+' => Plus_Case : begin
            Accumulate_Checksum ('+');
            Scan_Ptr := Scan_Ptr + 1;
            Token := Tok_Plus;
            return;
         end Plus_Case;

         --  Digits starting a numeric literal

         when '0' .. '9' =>
            Nlit;

            if Identifier_Char (Source (Scan_Ptr)) then
               Error_Msg_S
                 ("delimiter required between literal and identifier");
            end if;

            return;

         --  Lower case letters

         when 'a' .. 'z' =>
            Name_Len := 1;
            Name_Buffer (1) := Source (Scan_Ptr);
            Accumulate_Checksum (Name_Buffer (1));
            Scan_Ptr := Scan_Ptr + 1;
            goto Scan_Identifier;

         --  Upper case letters

         when 'A' .. 'Z' =>
            Name_Len := 1;
            Name_Buffer (1) :=
              Character'Val (Character'Pos (Source (Scan_Ptr)) + 32);
            Accumulate_Checksum (Name_Buffer (1));
            Scan_Ptr := Scan_Ptr + 1;
            goto Scan_Identifier;

         --  Underline character

         when '_' =>
            Error_Msg_S ("identifier cannot start with underline");
            Name_Len := 1;
            Name_Buffer (1) := '_';
            Scan_Ptr := Scan_Ptr + 1;
            goto Scan_Identifier;

         --  Space (not possible, because we scanned past blanks)

         when ' ' =>
            raise Program_Error;

         --  Characters in top half of ASCII 8-bit chart

         when Upper_Half_Character =>

            --  Wide character case. Note that Scan_Identifier will issue
            --  an appropriate message if wide characters are not allowed
            --  in identifiers.

            if Upper_Half_Encoding then
               Name_Len := 0;
               goto Scan_Identifier;

            --  Otherwise we have OK Latin-1 character

            else
               --  Upper half characters may possibly be identifier letters
               --  but can never be digits, so Identifier_Character can be
               --  used to test for a valid start of identifier character.

               if Identifier_Char (Source (Scan_Ptr)) then
                  Name_Len := 0;
                  goto Scan_Identifier;
               else
                  Error_Illegal_Character;
               end if;
            end if;

         when ESC =>

            --  ESC character, possible start of identifier if wide characters
            --  using ESC encoding are allowed in identifiers, which we can
            --  tell by looking at the Identifier_Char flag for ESC, which is
            --  only true if these conditions are met.

            if Identifier_Char (ESC) then
               Name_Len := 0;
               goto Scan_Identifier;
            else
               Error_Illegal_Wide_Character;
            end if;

         --  Invalid control characters

         when NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | SO  |
              SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN |
              EM  | FS  | GS  | RS  | US  | DEL
         =>
            Error_Illegal_Character;

         --  Invalid graphic characters

         when '#' | '$' | '?' | '@' | '`' | '\' | '^' | '~' =>
            Error_Illegal_Character;

         --  End switch on non-blank character

         end case;

      --  End loop past format effectors. The exit from this loop is by
      --  executing a return statement following completion of token scan
      --  (control never falls out of this loop to the code which follows)

      end loop;

      --  Identifier scanning routine. On entry, some initial characters
      --  of the identifier may have already been stored in Name_Buffer.
      --  If so, Name_Len has the number of characters stored. otherwise
      --  Name_Len is set to zero on entry.

      <<Scan_Identifier>>

         --  This loop scans as fast as possible past lower half letters
         --  and digits, which we expect to be the most common characters.

         loop
            if Source (Scan_Ptr) in 'a' .. 'z'
              or else Source (Scan_Ptr) in '0' .. '9'
            then
               Name_Buffer (Name_Len + 1) := Source (Scan_Ptr);
               Accumulate_Checksum (Source (Scan_Ptr));

            elsif Source (Scan_Ptr) in 'A' .. 'Z' then
               Name_Buffer (Name_Len + 1) :=
                 Character'Val (Character'Pos (Source (Scan_Ptr)) + 32);
               Accumulate_Checksum (Name_Buffer (Name_Len + 1));
            else
               exit;
            end if;

            --  Open out the loop a couple of times for speed

            if Source (Scan_Ptr + 1) in 'a' .. 'z'
              or else Source (Scan_Ptr + 1) in '0' .. '9'
            then
               Name_Buffer (Name_Len + 2) := Source (Scan_Ptr + 1);
               Accumulate_Checksum (Source (Scan_Ptr + 1));

            elsif Source (Scan_Ptr + 1) in 'A' .. 'Z' then
               Name_Buffer (Name_Len + 2) :=
                 Character'Val (Character'Pos (Source (Scan_Ptr + 1)) + 32);
               Accumulate_Checksum (Name_Buffer (Name_Len + 2));

            else
               Scan_Ptr := Scan_Ptr + 1;
               Name_Len := Name_Len + 1;
               exit;
            end if;

            if Source (Scan_Ptr + 2) in 'a' .. 'z'
              or else Source (Scan_Ptr + 2) in '0' .. '9'
            then
               Name_Buffer (Name_Len + 3) := Source (Scan_Ptr + 2);
               Accumulate_Checksum (Source (Scan_Ptr + 2));

            elsif Source (Scan_Ptr + 2) in 'A' .. 'Z' then
               Name_Buffer (Name_Len + 3) :=
                 Character'Val (Character'Pos (Source (Scan_Ptr + 2)) + 32);
               Accumulate_Checksum (Name_Buffer (Name_Len + 3));
            else
               Scan_Ptr := Scan_Ptr + 2;
               Name_Len := Name_Len + 2;
               exit;
            end if;

            if Source (Scan_Ptr + 3) in 'a' .. 'z'
              or else Source (Scan_Ptr + 3) in '0' .. '9'
            then
               Name_Buffer (Name_Len + 4) := Source (Scan_Ptr + 3);
               Accumulate_Checksum (Source (Scan_Ptr + 3));

            elsif Source (Scan_Ptr + 3) in 'A' .. 'Z' then
               Name_Buffer (Name_Len + 4) :=
                 Character'Val (Character'Pos (Source (Scan_Ptr + 3)) + 32);
               Accumulate_Checksum (Name_Buffer (Name_Len + 4));

            else
               Scan_Ptr := Scan_Ptr + 3;
               Name_Len := Name_Len + 3;
               exit;
            end if;

            Scan_Ptr := Scan_Ptr + 4;
            Name_Len := Name_Len + 4;
         end loop;

         --  If we fall through, then we have encountered either an underline
         --  character, or an extended identifier character (i.e. one from the
         --  upper half), or a wide character, or an identifier terminator.
         --  The initial test speeds us up in the most common case where we
         --  have an identifier terminator. Note that ESC is an identifier
         --  character only if a wide character encoding method that uses
         --  ESC encoding is active, so if we find an ESC character we know
         --  that we have a wide character.

         if Identifier_Char (Source (Scan_Ptr)) then

            --  Case of underline, check for error cases of double underline,
            --  and for a trailing underline character

            if Source (Scan_Ptr) = '_' then
               Accumulate_Checksum ('_');
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := '_';

               if Identifier_Char (Source (Scan_Ptr + 1)) then
                  Scan_Ptr := Scan_Ptr + 1;

                  if Source (Scan_Ptr) = '_' then
                     Error_No_Double_Underline;
                  end if;

               else
                  Error_Msg_S ("identifier cannot end with underline");
                  Scan_Ptr := Scan_Ptr + 1;
               end if;

               goto Scan_Identifier;

            --  Upper half character

            elsif Source (Scan_Ptr) in Upper_Half_Character
              and then not Upper_Half_Encoding
            then
               Accumulate_Checksum (Source (Scan_Ptr));
               Store_Encoded_Character
                 (Get_Char_Code (Fold_Lower (Source (Scan_Ptr))));
               Scan_Ptr := Scan_Ptr + 1;
               goto Scan_Identifier;

            --  Left bracket not followed by a quote terminates an identifier.
            --  This is an error, but we don't want to give a junk error msg
            --  about wide characters in this case!

            elsif Source (Scan_Ptr) = '['
              and then Source (Scan_Ptr + 1) /= '"'
            then
               null;

            --  We know we have a wide character encoding here (the current
            --  character is either ESC, left bracket, or an upper half
            --  character depending on the encoding method).

            else
               --  Scan out the wide character and insert the appropriate
               --  encoding into the name table entry for the identifier.

               declare
                  Sptr : constant Source_Ptr := Scan_Ptr;
                  Code : Char_Code;
                  Err  : Boolean;

               begin
                  Scan_Wide (Source, Scan_Ptr, Code, Err);
                  Accumulate_Checksum (Code);

                  if Err then
                     Error_Illegal_Wide_Character;
                  else
                     Store_Encoded_Character (Code);
                  end if;

                  --  Make sure we are allowing wide characters in identifiers.
                  --  Note that we allow wide character notation for an OK
                  --  identifier character. This in particular allows bracket
                  --  or other notation to be used for upper half letters.

                  if Identifier_Character_Set /= 'w'
                    and then
                      (not In_Character_Range (Code)
                         or else
                       not Identifier_Char (Get_Character (Code)))
                  then
                     Error_Msg
                       ("wide character not allowed in identifier", Sptr);
                  end if;
               end;

               goto Scan_Identifier;
            end if;
         end if;

         --  Scan of identifier is complete. The identifier is stored in
         --  Name_Buffer, and Scan_Ptr points past the last character.

         Token_Name := Name_Find;

         --  Here is where we check if it was a keyword

         if Get_Name_Table_Byte (Token_Name) /= 0
           and then (Ada_95 or else Token_Name not in Ada_95_Reserved_Words)
         then
            Token := Token_Type'Val (Get_Name_Table_Byte (Token_Name));

            --  Deal with possible style check for non-lower case keyword,
            --  but we don't treat ACCESS, DELTA, DIGITS, RANGE as keywords
            --  for this purpose if they appear as attribute designators.
            --  Actually we only check the first character for speed.

            if Style_Check
              and then Source (Token_Ptr) <= 'Z'
              and then (Prev_Token /= Tok_Apostrophe
                          or else
                            (Token /= Tok_Access
                               and then Token /= Tok_Delta
                               and then Token /= Tok_Digits
                               and then Token /= Tok_Range))
            then
               Style.Non_Lower_Case_Keyword;
            end if;

            --  We must reset Token_Name since this is not an identifier
            --  and if we leave Token_Name set, the parser gets confused
            --  because it thinks it is dealing with an identifier instead
            --  of the corresponding keyword.

            Token_Name := No_Name;
            return;

         --  It is an identifier after all

         else
            Token_Node := New_Node (N_Identifier, Token_Ptr);
            Set_Chars (Token_Node, Token_Name);
            Token := Tok_Identifier;
            return;
         end if;
   end Scan;

   ---------------------
   -- Scan_First_Char --
   ---------------------

   function Scan_First_Char return Source_Ptr is
      Ptr : Source_Ptr := Current_Line_Start;

   begin
      loop
         if Source (Ptr) = ' ' then
            Ptr := Ptr + 1;

         elsif Source (Ptr) = HT then
            if Style_Check then Style.Check_HT; end if;
            Ptr := Ptr + 1;

         else
            return Ptr;
         end if;
      end loop;
   end Scan_First_Char;

   ------------------------------
   -- Scan_Reserved_Identifier --
   ------------------------------

   procedure Scan_Reserved_Identifier (Force_Msg : Boolean) is
      Token_Chars : constant String := Token_Type'Image (Token);

   begin
      --  We have in Token_Chars the image of the Token name, i.e. Tok_xxx.
      --  This code extracts the xxx and makes an identifier out of it.

      Name_Len := 0;

      for J in 5 .. Token_Chars'Length loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Fold_Lower (Token_Chars (J));
      end loop;

      Token_Name := Name_Find;

      if not Used_As_Identifier (Token) or else Force_Msg then
         Error_Msg_Name_1 := Token_Name;
         Error_Msg_SC ("reserved word* cannot be used as identifier!");
         Used_As_Identifier (Token) := True;
      end if;

      Token := Tok_Identifier;
      Token_Node := New_Node (N_Identifier, Token_Ptr);
      Set_Chars (Token_Node, Token_Name);
   end Scan_Reserved_Identifier;

   ----------------------
   -- Set_Start_Column --
   ----------------------

   --  Note: it seems at first glance a little expensive to compute this value
   --  for every source line (since it is certainly not used for all source
   --  lines). On the other hand, it doesn't take much more work to skip past
   --  the initial white space on the line counting the columns than it would
   --  to scan past the white space using the standard scanning circuits.

   function Set_Start_Column return Column_Number is
      Start_Column : Column_Number := 0;

   begin
      --  Outer loop scans past horizontal tab characters

      Tabs_Loop : loop

         --  Inner loop scans past blanks as fast as possible, bumping Scan_Ptr
         --  past the blanks and adjusting Start_Column to account for them.

         Blanks_Loop : loop
            if Source (Scan_Ptr) = ' ' then
               if Source (Scan_Ptr + 1) = ' ' then
                  if Source (Scan_Ptr + 2) = ' ' then
                     if Source (Scan_Ptr + 3) = ' ' then
                        if Source (Scan_Ptr + 4) = ' ' then
                           if Source (Scan_Ptr + 5) = ' ' then
                              if Source (Scan_Ptr + 6) = ' ' then
                                 Scan_Ptr := Scan_Ptr + 7;
                                 Start_Column := Start_Column + 7;
                              else
                                 Scan_Ptr := Scan_Ptr + 6;
                                 Start_Column := Start_Column + 6;
                                 exit Blanks_Loop;
                              end if;
                           else
                              Scan_Ptr := Scan_Ptr + 5;
                              Start_Column := Start_Column + 5;
                              exit Blanks_Loop;
                           end if;
                        else
                           Scan_Ptr := Scan_Ptr + 4;
                           Start_Column := Start_Column + 4;
                           exit Blanks_Loop;
                        end if;
                     else
                        Scan_Ptr := Scan_Ptr + 3;
                        Start_Column := Start_Column + 3;
                        exit Blanks_Loop;
                     end if;
                  else
                     Scan_Ptr := Scan_Ptr + 2;
                     Start_Column := Start_Column + 2;
                     exit Blanks_Loop;
                  end if;
               else
                  Scan_Ptr := Scan_Ptr + 1;
                  Start_Column := Start_Column + 1;
                  exit Blanks_Loop;
               end if;
            else
               exit Blanks_Loop;
            end if;
         end loop Blanks_Loop;

         --  Outer loop keeps going only if a horizontal tab follows

         if Source (Scan_Ptr) = HT then
            if Style_Check then Style.Check_HT; end if;
            Scan_Ptr := Scan_Ptr + 1;
            Start_Column := (Start_Column / 8) * 8 + 8;
         else
            exit Tabs_Loop;
         end if;

      end loop Tabs_Loop;

      return Start_Column;
   end Set_Start_Column;

   ----------
   -- Slit --
   ----------

   procedure Slit is separate;

end Scn;
