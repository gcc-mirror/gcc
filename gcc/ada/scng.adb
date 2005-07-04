------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 S C N G                                  --
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
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Csets;    use Csets;
with Err_Vars; use Err_Vars;
with Namet;    use Namet;
with Opt;      use Opt;
with Scans;    use Scans;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stylesw;  use Stylesw;
with Uintp;    use Uintp;
with Urealp;   use Urealp;
with Widechar; use Widechar;

with System.CRC32;
with System.WCh_Con; use System.WCh_Con;

with GNAT.UTF_32; use GNAT.UTF_32;

package body Scng is

   use ASCII;
   --  Make control characters visible

   Special_Characters : array (Character) of Boolean := (others => False);
   --  For characters that are Special token, the value is True

   Comment_Is_Token : Boolean := False;
   --  True if comments are tokens

   End_Of_Line_Is_Token : Boolean := False;
   --  True if End_Of_Line is a token

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Accumulate_Token_Checksum;
   pragma Inline (Accumulate_Token_Checksum);

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

   -------------------------
   -- Accumulate_Checksum --
   -------------------------

   procedure Accumulate_Checksum (C : Character) is
   begin
      System.CRC32.Update (System.CRC32.CRC32 (Checksum), C);
   end Accumulate_Checksum;

   procedure Accumulate_Checksum (C : Char_Code) is
   begin
      if C > 16#FFFF# then
         Accumulate_Checksum (Character'Val (C / 2 ** 24));
         Accumulate_Checksum (Character'Val ((C / 2 ** 16) mod 256));
         Accumulate_Checksum (Character'Val ((C / 256) mod 256));
      else
         Accumulate_Checksum (Character'Val (C / 256));
      end if;

      Accumulate_Checksum (Character'Val (C mod 256));
   end Accumulate_Checksum;

   -------------------------------
   -- Accumulate_Token_Checksum --
   -------------------------------

   procedure Accumulate_Token_Checksum is
   begin
      System.CRC32.Update
        (System.CRC32.CRC32 (Checksum),
         Character'Val (Token_Type'Pos (Token)));
   end Accumulate_Token_Checksum;

   ----------------------------
   -- Determine_Token_Casing --
   ----------------------------

   function Determine_Token_Casing return Casing_Type is
   begin
      return Determine_Casing (Source (Token_Ptr .. Scan_Ptr - 1));
   end Determine_Token_Casing;

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
      procedure Set_Reserved (N : Name_Id; T : Token_Type);
      pragma Inline (Set_Reserved);
      --  Set given name as a reserved keyword (T is the corresponding token)

      -------------
      -- Set_NTB --
      -------------

      procedure Set_Reserved (N : Name_Id; T : Token_Type) is
      begin
         --  Set up Token_Type values in Names Table entries for reserved
         --  keywords We use the Pos value of the Token_Type value. Note we
         --  rely on the fact that Token_Type'Val (0) is not a reserved word!

         Set_Name_Table_Byte (N, Token_Type'Pos (T));
      end Set_Reserved;

   --  Start of processing for Initialize_Scanner

   begin
      --  Establish reserved words

      Set_Reserved (Name_Abort,     Tok_Abort);
      Set_Reserved (Name_Abs,       Tok_Abs);
      Set_Reserved (Name_Abstract,  Tok_Abstract);
      Set_Reserved (Name_Accept,    Tok_Accept);
      Set_Reserved (Name_Access,    Tok_Access);
      Set_Reserved (Name_And,       Tok_And);
      Set_Reserved (Name_Aliased,   Tok_Aliased);
      Set_Reserved (Name_All,       Tok_All);
      Set_Reserved (Name_Array,     Tok_Array);
      Set_Reserved (Name_At,        Tok_At);
      Set_Reserved (Name_Begin,     Tok_Begin);
      Set_Reserved (Name_Body,      Tok_Body);
      Set_Reserved (Name_Case,      Tok_Case);
      Set_Reserved (Name_Constant,  Tok_Constant);
      Set_Reserved (Name_Declare,   Tok_Declare);
      Set_Reserved (Name_Delay,     Tok_Delay);
      Set_Reserved (Name_Delta,     Tok_Delta);
      Set_Reserved (Name_Digits,    Tok_Digits);
      Set_Reserved (Name_Do,        Tok_Do);
      Set_Reserved (Name_Else,      Tok_Else);
      Set_Reserved (Name_Elsif,     Tok_Elsif);
      Set_Reserved (Name_End,       Tok_End);
      Set_Reserved (Name_Entry,     Tok_Entry);
      Set_Reserved (Name_Exception, Tok_Exception);
      Set_Reserved (Name_Exit,      Tok_Exit);
      Set_Reserved (Name_For,       Tok_For);
      Set_Reserved (Name_Function,  Tok_Function);
      Set_Reserved (Name_Generic,   Tok_Generic);
      Set_Reserved (Name_Goto,      Tok_Goto);
      Set_Reserved (Name_If,        Tok_If);
      Set_Reserved (Name_In,        Tok_In);
      Set_Reserved (Name_Is,        Tok_Is);
      Set_Reserved (Name_Limited,   Tok_Limited);
      Set_Reserved (Name_Loop,      Tok_Loop);
      Set_Reserved (Name_Mod,       Tok_Mod);
      Set_Reserved (Name_New,       Tok_New);
      Set_Reserved (Name_Not,       Tok_Not);
      Set_Reserved (Name_Null,      Tok_Null);
      Set_Reserved (Name_Of,        Tok_Of);
      Set_Reserved (Name_Or,        Tok_Or);
      Set_Reserved (Name_Others,    Tok_Others);
      Set_Reserved (Name_Out,       Tok_Out);
      Set_Reserved (Name_Package,   Tok_Package);
      Set_Reserved (Name_Pragma,    Tok_Pragma);
      Set_Reserved (Name_Private,   Tok_Private);
      Set_Reserved (Name_Procedure, Tok_Procedure);
      Set_Reserved (Name_Protected, Tok_Protected);
      Set_Reserved (Name_Raise,     Tok_Raise);
      Set_Reserved (Name_Range,     Tok_Range);
      Set_Reserved (Name_Record,    Tok_Record);
      Set_Reserved (Name_Rem,       Tok_Rem);
      Set_Reserved (Name_Renames,   Tok_Renames);
      Set_Reserved (Name_Requeue,   Tok_Requeue);
      Set_Reserved (Name_Return,    Tok_Return);
      Set_Reserved (Name_Reverse,   Tok_Reverse);
      Set_Reserved (Name_Select,    Tok_Select);
      Set_Reserved (Name_Separate,  Tok_Separate);
      Set_Reserved (Name_Subtype,   Tok_Subtype);
      Set_Reserved (Name_Tagged,    Tok_Tagged);
      Set_Reserved (Name_Task,      Tok_Task);
      Set_Reserved (Name_Terminate, Tok_Terminate);
      Set_Reserved (Name_Then,      Tok_Then);
      Set_Reserved (Name_Type,      Tok_Type);
      Set_Reserved (Name_Until,     Tok_Until);
      Set_Reserved (Name_Use,       Tok_Use);
      Set_Reserved (Name_When,      Tok_When);
      Set_Reserved (Name_While,     Tok_While);
      Set_Reserved (Name_With,      Tok_With);
      Set_Reserved (Name_Xor,       Tok_Xor);

      --  Ada 2005 reserved words

      Set_Reserved (Name_Interface,     Tok_Interface);
      Set_Reserved (Name_Overriding,    Tok_Overriding);
      Set_Reserved (Name_Synchronized,  Tok_Synchronized);

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

      --  Do not call Scan, otherwise the License stuff does not work in Scn

   end Initialize_Scanner;

   ------------------------------
   -- Reset_Special_Characters --
   ------------------------------

   procedure Reset_Special_Characters is
   begin
      Special_Characters := (others => False);
   end Reset_Special_Characters;

   ----------
   -- Scan --
   ----------

   procedure Scan is

      Start_Of_Comment : Source_Ptr;
      --  Record start of comment position

      Underline_Found : Boolean;
      --  During scanning of an identifier, set to True if last character
      --  scanned was an underline or other punctuation character. This
      --  is used to flag the error of two underlines/punctuations in a
      --  row or ending an identifier with a underline/punctuation. Here
      --  punctuation means any UTF_32 character in the Unicode category
      --  Punctuation,Connector.

      Wptr : Source_Ptr;
      --  Used to remember start of last wide character scanned

      procedure Check_End_Of_Line;
      --  Called when end of line encountered. Checks that line is not too
      --  long, and that other style checks for the end of line are met.

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
      --  Give illegal character error, Scan_Ptr points to character. On
      --  return, Scan_Ptr is bumped past the illegal character.

      procedure Error_Illegal_Wide_Character;
      --  Give illegal wide character message. On return, Scan_Ptr is bumped
      --  past the illegal character, which may still leave us pointing to
      --  junk, not much we can do if the escape sequence is messed up!

      procedure Error_Long_Line;
      --  Signal error of excessively long line

      procedure Error_No_Double_Underline;
      --  Signal error of two underline or punctuation characters in a row.
      --  Called with Scan_Ptr pointing to second underline/punctuation char.

      procedure Nlit;
      --  This is the procedure for scanning out numeric literals. On entry,
      --  Scan_Ptr points to the digit that starts the numeric literal (the
      --  checksum for this character has not been accumulated yet). On return
      --  Scan_Ptr points past the last character of the numeric literal, Token
      --  and Token_Node are set appropriately, and the checksum is updated.

      procedure Slit;
      --  This is the procedure for scanning out string literals. On entry,
      --  Scan_Ptr points to the opening string quote (the checksum for this
      --  character has not been accumulated yet). On return Scan_Ptr points
      --  past the closing quote of the string literal, Token and Token_Node
      --  are set appropriately, and the checksum is upated.

      -----------------------
      -- Check_End_Of_Line --
      -----------------------

      procedure Check_End_Of_Line is
         Len : constant Int := Int (Scan_Ptr) - Int (Current_Line_Start);

      begin
         if Style_Check then
            Style.Check_Line_Terminator (Len);
         end if;

         --  Deal with checking maximum line length

         if Style_Check and Style_Check_Max_Line_Length then
            Style.Check_Line_Max_Length (Len);

         --  If style checking is inactive, check maximum line length against
         --  standard value. Note that we take this from Opt.Max_Line_Length
         --  rather than Hostparm.Max_Line_Length because we do not want to
         --  impose any limit during scanning of configuration pragma files,
         --  and Opt.Max_Line_Length (normally set to Hostparm.Max_Line_Length)
         --  is reset to Column_Number'Max during scanning of such files.

         elsif Len > Opt.Max_Line_Length then
            Error_Long_Line;
         end if;
      end Check_End_Of_Line;

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
         Error_Msg ("illegal wide character", Wptr);
      end Error_Illegal_Wide_Character;

      ---------------------
      -- Error_Long_Line --
      ---------------------

      procedure Error_Long_Line is
      begin
         Error_Msg
           ("this line is too long",
            Current_Line_Start + Source_Ptr (Opt.Max_Line_Length));
      end Error_Long_Line;

      -------------------------------
      -- Error_No_Double_Underline --
      -------------------------------

      procedure Error_No_Double_Underline is
      begin
         Underline_Found := False;

         --  There are four cases, and we special case the messages

         if Source (Scan_Ptr) = '_' then
            if Source (Scan_Ptr - 1) = '_' then
               Error_Msg_S
                 ("two consecutive underlines not permitted");
            else
               Error_Msg_S
                 ("underline cannot follow punctuation character");
            end if;

         else
            if Source (Scan_Ptr - 1) = '_' then
               Error_Msg_S
                 ("punctuation character cannot follow underline");
            else
               Error_Msg_S
                 ("two consecutive punctuation characters not permitted");
            end if;
         end if;
      end Error_No_Double_Underline;

      ----------
      -- Nlit --
      ----------

      procedure Nlit is

         C : Character;
         --  Current source program character

         Base_Char : Character;
         --  Either # or : (character at start of based number)

         Base : Int;
         --  Value of base

         UI_Base : Uint;
         --  Value of base in Uint format

         UI_Int_Value : Uint;
         --  Value of integer scanned by Scan_Integer in Uint format

         UI_Num_Value : Uint;
         --  Value of integer in numeric value being scanned

         Scale : Int;
         --  Scale value for real literal

         UI_Scale : Uint;
         --  Scale in Uint format

         Exponent_Is_Negative : Boolean;
         --  Set true for negative exponent

         Extended_Digit_Value : Int;
         --  Extended digit value

         Point_Scanned : Boolean;
         --  Flag for decimal point scanned in numeric literal

         -----------------------
         -- Local Subprograms --
         -----------------------

         procedure Error_Digit_Expected;
         --  Signal error of bad digit, Scan_Ptr points to the location at
         --  which the digit was expected on input, and is unchanged on return.

         procedure Scan_Integer;
         --  Procedure to scan integer literal. On entry, Scan_Ptr points to a
         --  digit, on exit Scan_Ptr points past the last character of the
         --  integer.
         --
         --  For each digit encountered, UI_Int_Value is multiplied by 10, and
         --  the value of the digit added to the result. In addition, the
         --  value in Scale is decremented by one for each actual digit
         --  scanned.

         --------------------------
         -- Error_Digit_Expected --
         --------------------------

         procedure Error_Digit_Expected is
         begin
            Error_Msg_S ("digit expected");
         end Error_Digit_Expected;

         ------------------
         -- Scan_Integer --
         ------------------

         procedure Scan_Integer is
            C : Character;
            --  Next character scanned

         begin
            C := Source (Scan_Ptr);

            --  Loop through digits (allowing underlines)

            loop
               Accumulate_Checksum (C);
               UI_Int_Value :=
                 UI_Int_Value * 10 + (Character'Pos (C) - Character'Pos ('0'));
               Scan_Ptr := Scan_Ptr + 1;
               Scale := Scale - 1;
               C := Source (Scan_Ptr);

               --  Case of underline encountered

               if C = '_' then

                  --  We do not accumulate the '_' in the checksum, so that
                  --  1_234 is equivalent to 1234, and does not trigger
                  --  compilation for "minimal recompilation" (gnatmake -m).

                  loop
                     Scan_Ptr := Scan_Ptr + 1;
                     C := Source (Scan_Ptr);
                     exit when C /= '_';
                     Error_No_Double_Underline;
                  end loop;

                  if C not in '0' .. '9' then
                     Error_Digit_Expected;
                     exit;
                  end if;

               else
                  exit when C not in '0' .. '9';
               end if;
            end loop;
         end Scan_Integer;

      --  Start of Processing for Nlit

      begin
         Base := 10;
         UI_Base := Uint_10;
         UI_Int_Value := Uint_0;
         Scale := 0;
         Scan_Integer;
         Scale := 0;
         Point_Scanned := False;
         UI_Num_Value := UI_Int_Value;

         --  Various possibilities now for continuing the literal are period,
         --  E/e (for exponent), or :/# (for based literal).

         Scale := 0;
         C := Source (Scan_Ptr);

         if C = '.' then

            --  Scan out point, but do not scan past .. which is a range
            --  sequence, and must not be eaten up scanning a numeric literal.

            while C = '.' and then Source (Scan_Ptr + 1) /= '.' loop
               Accumulate_Checksum ('.');

               if Point_Scanned then
                  Error_Msg_S ("duplicate point ignored");
               end if;

               Point_Scanned := True;
               Scan_Ptr := Scan_Ptr + 1;
               C := Source (Scan_Ptr);

               if C not in '0' .. '9' then
                  Error_Msg
                    ("real literal cannot end with point", Scan_Ptr - 1);
               else
                  Scan_Integer;
                  UI_Num_Value := UI_Int_Value;
               end if;
            end loop;

         --  Based literal case. The base is the value we already scanned.
         --  In the case of colon, we insist that the following character
         --  is indeed an extended digit or a period. This catches a number
         --  of common errors, as well as catching the well known tricky
         --  bug otherwise arising from "x : integer range 1 .. 10:= 6;"

         elsif C = '#'
           or else (C = ':' and then
                      (Source (Scan_Ptr + 1) = '.'
                         or else
                       Source (Scan_Ptr + 1) in '0' .. '9'
                         or else
                       Source (Scan_Ptr + 1) in 'A' .. 'Z'
                         or else
                       Source (Scan_Ptr + 1) in 'a' .. 'z'))
         then
            if C = ':' then
               Obsolescent_Check (Scan_Ptr);

               if Warn_On_Obsolescent_Feature then
                  Error_Msg_S
                    ("use of "":"" is an obsolescent feature ('R'M 'J.2(3))?");
                  Error_Msg_S
                    ("\use ""'#"" instead?");
               end if;
            end if;

            Accumulate_Checksum (C);
            Base_Char := C;
            UI_Base := UI_Int_Value;

            if UI_Base < 2 or else UI_Base > 16 then
               Error_Msg_SC ("base not 2-16");
               UI_Base := Uint_16;
            end if;

            Base := UI_To_Int (UI_Base);
            Scan_Ptr := Scan_Ptr + 1;

            --  Scan out extended integer [. integer]

            C := Source (Scan_Ptr);
            UI_Int_Value := Uint_0;
            Scale := 0;

            loop
               if C in '0' .. '9' then
                  Accumulate_Checksum (C);
                  Extended_Digit_Value :=
                    Int'(Character'Pos (C)) - Int'(Character'Pos ('0'));

               elsif C in 'A' .. 'F' then
                  Accumulate_Checksum (Character'Val (Character'Pos (C) + 32));
                  Extended_Digit_Value :=
                    Int'(Character'Pos (C)) - Int'(Character'Pos ('A')) + 10;

               elsif C in 'a' .. 'f' then
                  Accumulate_Checksum (C);
                  Extended_Digit_Value :=
                    Int'(Character'Pos (C)) - Int'(Character'Pos ('a')) + 10;

               else
                  Error_Msg_S ("extended digit expected");
                  exit;
               end if;

               if Extended_Digit_Value >= Base then
                  Error_Msg_S ("digit '>= base");
               end if;

               UI_Int_Value := UI_Int_Value * UI_Base + Extended_Digit_Value;
               Scale := Scale - 1;
               Scan_Ptr := Scan_Ptr + 1;
               C := Source (Scan_Ptr);

               if C = '_' then
                  loop
                     Accumulate_Checksum ('_');
                     Scan_Ptr := Scan_Ptr + 1;
                     C := Source (Scan_Ptr);
                     exit when C /= '_';
                     Error_No_Double_Underline;
                  end loop;

               elsif C = '.' then
                  Accumulate_Checksum ('.');

                  if Point_Scanned then
                     Error_Msg_S ("duplicate point ignored");
                  end if;

                  Scan_Ptr := Scan_Ptr + 1;
                  C := Source (Scan_Ptr);
                  Point_Scanned := True;
                  Scale := 0;

               elsif C = Base_Char then
                  Accumulate_Checksum (C);
                  Scan_Ptr := Scan_Ptr + 1;
                  exit;

               elsif C = '#' or else C = ':' then
                  Error_Msg_S ("based number delimiters must match");
                  Scan_Ptr := Scan_Ptr + 1;
                  exit;

               elsif not Identifier_Char (C) then
                  if Base_Char = '#' then
                     Error_Msg_S ("missing '#");
                  else
                     Error_Msg_S ("missing ':");
                  end if;

                  exit;
               end if;

            end loop;

            UI_Num_Value := UI_Int_Value;
         end if;

         --  Scan out exponent

         if not Point_Scanned then
            Scale := 0;
            UI_Scale := Uint_0;
         else
            UI_Scale := UI_From_Int (Scale);
         end if;

         if Source (Scan_Ptr) = 'e' or else Source (Scan_Ptr) = 'E' then
            Accumulate_Checksum ('e');
            Scan_Ptr := Scan_Ptr + 1;
            Exponent_Is_Negative := False;

            if Source (Scan_Ptr) = '+' then
               Accumulate_Checksum ('+');
               Scan_Ptr := Scan_Ptr + 1;

            elsif Source (Scan_Ptr) = '-' then
               Accumulate_Checksum ('-');

               if not Point_Scanned then
                  Error_Msg_S
                    ("negative exponent not allowed for integer literal");
               else
                  Exponent_Is_Negative := True;
               end if;

               Scan_Ptr := Scan_Ptr + 1;
            end if;

            UI_Int_Value := Uint_0;

            if Source (Scan_Ptr) in '0' .. '9' then
               Scan_Integer;
            else
               Error_Digit_Expected;
            end if;

            if Exponent_Is_Negative then
               UI_Scale := UI_Scale - UI_Int_Value;
            else
               UI_Scale := UI_Scale + UI_Int_Value;
            end if;
         end if;

         --  Case of real literal to be returned

         if Point_Scanned then
            Token := Tok_Real_Literal;
            Real_Literal_Value :=
              UR_From_Components (
                                  Num   => UI_Num_Value,
                                  Den   => -UI_Scale,
                                  Rbase => Base);

         --  Case of integer literal to be returned

         else
            Token := Tok_Integer_Literal;

            if UI_Scale = 0 then
               Int_Literal_Value := UI_Num_Value;

            --  Avoid doing possibly expensive calculations in cases like
            --  parsing 163E800_000# when semantics will not be done anyway.
            --  This is especially useful when parsing garbled input.

            elsif Operating_Mode /= Check_Syntax
              and then (Serious_Errors_Detected = 0 or else Try_Semantics)
            then
               Int_Literal_Value := UI_Num_Value * UI_Base ** UI_Scale;

            else
               Int_Literal_Value := No_Uint;
            end if;
         end if;

         Accumulate_Token_Checksum;

         return;
      end Nlit;

      ----------
      -- Slit --
      ----------

      procedure Slit is

         Delimiter : Character;
         --  Delimiter (first character of string)

         C : Character;
         --  Current source program character

         Code : Char_Code;
         --  Current character code value

         Err : Boolean;
         --  Error flag for Scan_Wide call

         procedure Error_Bad_String_Char;
         --  Signal bad character in string/character literal. On entry
         --  Scan_Ptr points to the improper character encountered during the
         --  scan. Scan_Ptr is not modified, so it still points to the bad
         --  character on return.

         procedure Error_Unterminated_String;
         --  Procedure called if a line terminator character is encountered
         --  during scanning a string, meaning that the string is not properly
         --  terminated.

         procedure Set_String;
         --  Procedure used to distinguish between string and operator symbol.
         --  On entry the string has been scanned out, and its characters
         --  start at Token_Ptr and end one character before Scan_Ptr. On exit
         --  Token is set to Tok_String_Literal or Tok_Operator_Symbol as
         --  appropriate, and Token_Node is appropriately initialized. In
         --  addition, in the operator symbol case, Token_Name is
         --  appropriately set.

         ---------------------------
         -- Error_Bad_String_Char --
         ---------------------------

         procedure Error_Bad_String_Char is
            C : constant Character := Source (Scan_Ptr);

         begin
            if C = HT then
               Error_Msg_S ("horizontal tab not allowed in string");

            elsif C = VT or else C = FF then
               Error_Msg_S ("format effector not allowed in string");

            elsif C in Upper_Half_Character then
               Error_Msg_S ("(Ada 83) upper half character not allowed");

            else
               Error_Msg_S ("control character not allowed in string");
            end if;
         end Error_Bad_String_Char;

         -------------------------------
         -- Error_Unterminated_String --
         -------------------------------

         procedure Error_Unterminated_String is
         begin
            --  An interesting little refinement. Consider the following
            --  examples:

            --     A := "this is an unterminated string;
            --     A := "this is an unterminated string &
            --     P(A, "this is a parameter that didn't get terminated);

            --  We fiddle a little to do slightly better placement in these
            --  cases also if there is white space at the end of the line we
            --  place the flag at the start of this white space, not at the
            --  end. Note that we only have to test for blanks, since tabs
            --  aren't allowed in strings in the first place and would have
            --  caused an error message.

            --  Two more cases that we treat specially are:

            --     A := "this string uses the wrong terminator'
            --     A := "this string uses the wrong terminator' &

            --  In these cases we give a different error message as well

            --  We actually reposition the scan pointer to the point where we
            --  place the flag in these cases, since it seems a better bet on
            --  the original intention.

            while Source (Scan_Ptr - 1) = ' '
              or else Source (Scan_Ptr - 1) = '&'
            loop
               Scan_Ptr := Scan_Ptr - 1;
               Unstore_String_Char;
            end loop;

            --  Check for case of incorrect string terminator, but single quote
            --  is not considered incorrect if the opening terminator misused
            --  a single quote (error message already given).

            if Delimiter /= '''
              and then Source (Scan_Ptr - 1) = '''
            then
               Unstore_String_Char;
               Error_Msg
                 ("incorrect string terminator character", Scan_Ptr - 1);
               return;
            end if;

            if Source (Scan_Ptr - 1) = ';' then
               Scan_Ptr := Scan_Ptr - 1;
               Unstore_String_Char;

               if Source (Scan_Ptr - 1) = ')' then
                  Scan_Ptr := Scan_Ptr - 1;
                  Unstore_String_Char;
               end if;
            end if;

            Error_Msg_S ("missing string quote");
         end Error_Unterminated_String;

         ----------------
         -- Set_String --
         ----------------

         procedure Set_String is
            Slen : constant Int := Int (Scan_Ptr - Token_Ptr - 2);
            C1   : Character;
            C2   : Character;
            C3   : Character;

         begin
            --  Token_Name is currently set to Error_Name. The following
            --  section of code resets Token_Name to the proper Name_Op_xx
            --  value if the string is a valid operator symbol, otherwise it is
            --  left set to Error_Name.

            if Slen = 1 then
               C1 := Source (Token_Ptr + 1);

               case C1 is
                  when '=' =>
                     Token_Name := Name_Op_Eq;

                  when '>' =>
                     Token_Name := Name_Op_Gt;

                  when '<' =>
                     Token_Name := Name_Op_Lt;

                  when '+' =>
                     Token_Name := Name_Op_Add;

                  when '-' =>
                     Token_Name := Name_Op_Subtract;

                  when '&' =>
                     Token_Name := Name_Op_Concat;

                  when '*' =>
                     Token_Name := Name_Op_Multiply;

                  when '/' =>
                     Token_Name := Name_Op_Divide;

                  when others =>
                     null;
               end case;

            elsif Slen = 2 then
               C1 := Source (Token_Ptr + 1);
               C2 := Source (Token_Ptr + 2);

               if C1 = '*' and then C2 = '*' then
                  Token_Name := Name_Op_Expon;

               elsif C2 = '=' then

                  if C1 = '/' then
                     Token_Name := Name_Op_Ne;
                  elsif C1 = '<' then
                     Token_Name := Name_Op_Le;
                  elsif C1 = '>' then
                     Token_Name := Name_Op_Ge;
                  end if;

               elsif (C1 = 'O' or else C1 = 'o') and then    -- OR
                 (C2 = 'R' or else C2 = 'r')
               then
                  Token_Name := Name_Op_Or;
               end if;

            elsif Slen = 3 then
               C1 := Source (Token_Ptr + 1);
               C2 := Source (Token_Ptr + 2);
               C3 := Source (Token_Ptr + 3);

               if (C1 = 'A' or else C1 = 'a') and then       -- AND
                 (C2 = 'N' or else C2 = 'n') and then
                 (C3 = 'D' or else C3 = 'd')
               then
                  Token_Name := Name_Op_And;

               elsif (C1 = 'A' or else C1 = 'a') and then    -- ABS
                 (C2 = 'B' or else C2 = 'b') and then
                 (C3 = 'S' or else C3 = 's')
               then
                  Token_Name := Name_Op_Abs;

               elsif (C1 = 'M' or else C1 = 'm') and then    -- MOD
                 (C2 = 'O' or else C2 = 'o') and then
                 (C3 = 'D' or else C3 = 'd')
               then
                  Token_Name := Name_Op_Mod;

               elsif (C1 = 'N' or else C1 = 'n') and then    -- NOT
                 (C2 = 'O' or else C2 = 'o') and then
                 (C3 = 'T' or else C3 = 't')
               then
                  Token_Name := Name_Op_Not;

               elsif (C1 = 'R' or else C1 = 'r') and then    -- REM
                 (C2 = 'E' or else C2 = 'e') and then
                 (C3 = 'M' or else C3 = 'm')
               then
                  Token_Name := Name_Op_Rem;

               elsif (C1 = 'X' or else C1 = 'x') and then    -- XOR
                 (C2 = 'O' or else C2 = 'o') and then
                 (C3 = 'R' or else C3 = 'r')
               then
                  Token_Name := Name_Op_Xor;
               end if;

            end if;

            --  If it is an operator symbol, then Token_Name is set. If it is
            --  some other string value, then Token_Name still contains
            --  Error_Name.

            if Token_Name = Error_Name then
               Token := Tok_String_Literal;

            else
               Token := Tok_Operator_Symbol;
            end if;
         end Set_String;

      --  Start of processing for Slit

      begin
         --  On entry, Scan_Ptr points to the opening character of the string
         --  which is either a percent, double quote, or apostrophe (single
         --  quote). The latter case is an error detected by the character
         --  literal circuit.

         Delimiter := Source (Scan_Ptr);
         Accumulate_Checksum (Delimiter);
         Start_String;
         Scan_Ptr := Scan_Ptr + 1;

         --  Loop to scan out characters of string literal

         loop
            C := Source (Scan_Ptr);

            if C = Delimiter then
               Accumulate_Checksum (C);
               Scan_Ptr := Scan_Ptr + 1;
               exit when Source (Scan_Ptr) /= Delimiter;
               Code := Get_Char_Code (C);
               Accumulate_Checksum (C);
               Scan_Ptr := Scan_Ptr + 1;

            else
               if C = '"' and then Delimiter = '%' then
                  Error_Msg_S
                    ("quote not allowed in percent delimited string");
                  Code := Get_Char_Code (C);
                  Scan_Ptr := Scan_Ptr + 1;

               elsif (C = ESC
                        and then Wide_Character_Encoding_Method
                                   in WC_ESC_Encoding_Method)
                 or else (C in Upper_Half_Character
                            and then Upper_Half_Encoding)
                 or else (C = '['
                            and then Source (Scan_Ptr + 1) = '"'
                            and then Identifier_Char (Source (Scan_Ptr + 2)))
               then
                  Wptr := Scan_Ptr;
                  Scan_Wide (Source, Scan_Ptr, Code, Err);

                  if Err then
                     Error_Illegal_Wide_Character;
                     Code := Get_Char_Code (' ');
                  end if;

                  Accumulate_Checksum (Code);

                  --  In Ada 95 mode we allow any wide characters in a string
                  --  but in Ada 2005, the set of characters allowed has been
                  --  restricted to graphic characters.

                  if Ada_Version >= Ada_05
                    and then Is_UTF_32_Non_Graphic (UTF_32 (Code))
                  then
                     Error_Msg
                       ("(Ada 2005) non-graphic character not permitted " &
                        "in string literal", Wptr);
                  end if;

               else
                  Accumulate_Checksum (C);

                  if C not in Graphic_Character then
                     if C in Line_Terminator then
                        Error_Unterminated_String;
                        exit;

                     elsif C in Upper_Half_Character then
                        if Ada_Version = Ada_83 then
                           Error_Bad_String_Char;
                        end if;

                     else
                        Error_Bad_String_Char;
                     end if;
                  end if;

                  Code := Get_Char_Code (C);
                  Scan_Ptr := Scan_Ptr + 1;
               end if;
            end if;

            Store_String_Char (Code);

            if not In_Character_Range (Code) then
               Wide_Character_Found := True;
            end if;
         end loop;

         String_Literal_Id := End_String;
         Set_String;
         return;
      end Slit;

   --  Start of processing for Scan

   begin
      Prev_Token := Token;
      Prev_Token_Ptr := Token_Ptr;
      Token_Name := Error_Name;

      --  The following loop runs more than once only if a format effector
      --  (tab, vertical tab, form  feed, line feed, carriage return) is
      --  encountered and skipped, or some error situation, such as an
      --  illegal character, is encountered.

      <<Scan_Next_Character>>

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

         --  Here begins the main case statement which transfers control on the
         --  basis of the non-blank character we have encountered.

         case Source (Scan_Ptr) is

         --  Line terminator characters

         when CR | LF | FF | VT =>
            goto Scan_Line_Terminator;

         --  Horizontal tab, just skip past it

         when HT =>
            if Style_Check then Style.Check_HT; end if;
            Scan_Ptr := Scan_Ptr + 1;

         --  End of file character, treated as an end of file only if it is
         --  the last character in the buffer, otherwise it is ignored.

         when EOF =>
            if Scan_Ptr = Source_Last (Current_Source_File) then
               Check_End_Of_Line;
               if Style_Check then Style.Check_EOF; end if;
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

         --  Asterisk (can be multiplication operator or double asterisk which
         --  is the exponentiation compound delimiter).

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
               goto Scan_Wide_Character;

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

         --  Dot, which is either an isolated period, or part of a double dot
         --  compound delimiter sequence. We also check for the case of a
         --  digit following the period, to give a better error message.

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
               Start_Of_Comment := Scan_Ptr;

               --  Loop to scan comment (this loop runs more than once only if
               --  a horizontal tab or other non-graphic character is scanned)

               loop
                  --  Scan to non graphic character (opened up for speed)

                  --  Note that we just eat left brackets, which means that
                  --  bracket notation cannot be used for end of line
                  --  characters in comments. This seems a reasonable choice,
                  --  since no one would ever use brackets notation in a real
                  --  program in this situation, and if we allow brackets
                  --  notation, we forbid some valid comments which contain a
                  --  brackets sequence that happens to match an end of line
                  --  character.

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

                  --  If we have a wide character, we have to scan it out,
                  --  because it might be a legitimate line terminator

                  elsif (Source (Scan_Ptr) = ESC
                           and then Identifier_Char (ESC))
                    or else
                         (Source (Scan_Ptr) in Upper_Half_Character
                            and then Upper_Half_Encoding)
                  then
                     declare
                        Wptr : constant Source_Ptr := Scan_Ptr;
                        Code : Char_Code;
                        Err  : Boolean;

                     begin
                        Scan_Wide (Source, Scan_Ptr, Code, Err);

                        --  If not well formed wide character, then just skip
                        --  past it and ignore it.

                        if Err then
                           Scan_Ptr := Wptr + 1;

                        --  If UTF_32 terminator, terminate comment scan

                        elsif Is_UTF_32_Line_Terminator (UTF_32 (Code)) then
                           Scan_Ptr := Wptr;
                           exit;
                        end if;
                     end;

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

               --  Note that, except when comments are tokens, we do NOT
               --  execute a return here, instead we fall through to reexecute
               --  the scan loop to look for a token.

               if Comment_Is_Token then
                  Name_Len := Integer (Scan_Ptr - Start_Of_Comment);
                  Name_Buffer (1 .. Name_Len) :=
                    String (Source (Start_Of_Comment .. Scan_Ptr - 1));
                  Comment_Id := Name_Find;
                  Token := Tok_Comment;
                  return;
               end if;
            end if;
         end Minus_Case;

         --  Double quote starting a string literal

         when '"' =>
            Slit;
            Post_Scan;
            return;

         --  Percent starting a string literal

         when '%' =>
            Obsolescent_Check (Token_Ptr);

            if Warn_On_Obsolescent_Feature then
               Error_Msg_S
                 ("use of ""'%"" is an obsolescent feature ('R'M 'J.2(4))?");
               Error_Msg_S
                 ("\use """""" instead?");
            end if;

            Slit;
            Post_Scan;
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
            --  (or the reserved word "project" in project files). Also treat
            --  it as apostrophe after a literal (this catches some legitimate
            --  cases, like A."abs"'Address, and also gives better error
            --  behavior for impossible cases like 123'xxx).

            if Prev_Token = Tok_Identifier
               or else Prev_Token = Tok_Right_Paren
               or else Prev_Token = Tok_All
               or else Prev_Token = Tok_Project
               or else Prev_Token in Token_Class_Literal
            then
               Token := Tok_Apostrophe;
               if Style_Check then Style.Check_Apostrophe; end if;
               return;

            --  Otherwise the apostrophe starts a character literal

            else
               --  Case of wide character literal

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
                  Wptr := Scan_Ptr;
                  Scan_Wide (Source, Scan_Ptr, Code, Err);
                  Accumulate_Checksum (Code);

                  if Err then
                     Error_Illegal_Wide_Character;
                        Code := Character'Pos (' ');

                  --  In Ada 95 mode we allow any wide character in a character
                  --  literal, but in Ada 2005, the set of characters allowed
                  --  is restricted to graphic characters.

                  elsif Ada_Version >= Ada_05
                    and then Is_UTF_32_Non_Graphic (UTF_32 (Code))
                  then
                     Error_Msg
                       ("(Ada 2005) non-graphic character not permitted " &
                        "in character literal", Wptr);
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
                     Slit;
                     Post_Scan;
                     return;
                  end if;

               --  Otherwise we have a (non-wide) character literal

               else
                  Accumulate_Checksum (Source (Scan_Ptr));

                  if Source (Scan_Ptr) not in Graphic_Character then
                     if Source (Scan_Ptr) in Upper_Half_Character then
                        if Ada_Version = Ada_83 then
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
               Set_Character_Literal_Name (Code);
               Token_Name := Name_Find;
               Character_Code := Code;
               Post_Scan;
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
               Error_Msg_S ("""'|'|"" should be `OR ELSE`");
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
            Obsolescent_Check (Token_Ptr);

            if Warn_On_Obsolescent_Feature then
               Error_Msg_S
                 ("use of ""'!"" is an obsolescent feature ('R'M 'J.2(2))?");
               Error_Msg_S
                 ("\use ""'|"" instead?");
            end if;

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
            Post_Scan;
            return;

         --  Lower case letters

         when 'a' .. 'z' =>
            Name_Len := 1;
            Underline_Found := False;
            Name_Buffer (1) := Source (Scan_Ptr);
            Accumulate_Checksum (Name_Buffer (1));
            Scan_Ptr := Scan_Ptr + 1;
            goto Scan_Identifier;

         --  Upper case letters

         when 'A' .. 'Z' =>
            Name_Len := 1;
            Underline_Found := False;
            Name_Buffer (1) :=
              Character'Val (Character'Pos (Source (Scan_Ptr)) + 32);
            Accumulate_Checksum (Name_Buffer (1));
            Scan_Ptr := Scan_Ptr + 1;
            goto Scan_Identifier;

         --  Underline character

         when '_' =>
            if Special_Characters ('_') then
               Token_Ptr := Scan_Ptr;
               Scan_Ptr := Scan_Ptr + 1;
               Token := Tok_Special;
               Special_Character := '_';
               return;
            end if;

            Error_Msg_S ("identifier cannot start with underline");
            Name_Len := 1;
            Name_Buffer (1) := '_';
            Scan_Ptr := Scan_Ptr + 1;
            Underline_Found := False;
            goto Scan_Identifier;

         --  Space (not possible, because we scanned past blanks)

         when ' ' =>
            raise Program_Error;

         --  Characters in top half of ASCII 8-bit chart

         when Upper_Half_Character =>

            --  Wide character case

            if Upper_Half_Encoding then
               goto Scan_Wide_Character;

            --  Otherwise we have OK Latin-1 character

            else
               --  Upper half characters may possibly be identifier letters
               --  but can never be digits, so Identifier_Char can be used to
               --  test for a valid start of identifier character.

               if Identifier_Char (Source (Scan_Ptr)) then
                  Name_Len := 0;
                  Underline_Found := False;
                  goto Scan_Identifier;
               else
                  Error_Illegal_Character;
               end if;
            end if;

         when ESC =>

            --  ESC character, possible start of identifier if wide characters
            --  using ESC encoding are allowed in identifiers, which we can
            --  tell by looking at the Identifier_Char flag for ESC, which is
            --  only true if these conditions are met. In Ada 2005 mode, may
            --  also be valid UTF_32 space or line terminator character.

            if Identifier_Char (ESC) then
               Name_Len := 0;
               goto Scan_Wide_Character;
            else
               Error_Illegal_Character;
            end if;

         --  Invalid control characters

         when NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL | BS  | ASCII.SO |
              SI  | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN |
              EM  | FS  | GS  | RS  | US  | DEL
         =>
            Error_Illegal_Character;

         --  Invalid graphic characters

         when '#' | '$' | '?' | '@' | '`' | '\' | '^' | '~' =>

            --  If Set_Special_Character has been called for this character,
            --  set Scans.Special_Character and return a Special token.

            if Special_Characters (Source (Scan_Ptr)) then
               Token_Ptr := Scan_Ptr;
               Token := Tok_Special;
               Special_Character := Source (Scan_Ptr);
               Scan_Ptr := Scan_Ptr + 1;
               return;

            --  Otherwise, this is an illegal character

            else
               Error_Illegal_Character;
            end if;

         --  End switch on non-blank character

         end case;

      --  End loop past format effectors. The exit from this loop is by
      --  executing a return statement following completion of token scan
      --  (control never falls out of this loop to the code which follows)

      end loop;

      --  Wide_Character scanning routine. On entry we have encountered the
      --  initial character of a wide character sequence.

      <<Scan_Wide_Character>>

         declare
            Code : Char_Code;
            Cat  : Category;
            Err  : Boolean;

         begin
            Wptr := Scan_Ptr;
            Scan_Wide (Source, Scan_Ptr, Code, Err);

            --  If bad wide character, signal error and continue scan

            if Err then
               Error_Illegal_Wide_Character;
               goto Scan_Next_Character;
            end if;

            Cat := Get_Category (UTF_32 (Code));

            --  If OK letter, reset scan ptr and go scan identifier

            if Is_UTF_32_Letter (Cat) then
               Scan_Ptr := Wptr;
               Name_Len := 0;
               Underline_Found := False;
               goto Scan_Identifier;

            --  If OK wide space, ignore and keep scanning (we do not include
            --  any ignored spaces in checksum)

            elsif Is_UTF_32_Space (Cat) then
               goto Scan_Next_Character;

            --  If OK wide line terminator, terminate current line

            elsif Is_UTF_32_Line_Terminator (UTF_32 (Code)) then
               Scan_Ptr := Wptr;
               goto Scan_Line_Terminator;

            --  Punctuation is an error (at start of identifier)

            elsif Is_UTF_32_Punctuation (Cat) then
               Error_Msg
                 ("identifier cannot start with punctuation", Wptr);
               Scan_Ptr := Wptr;
               Name_Len := 0;
               Underline_Found := False;
               goto Scan_Identifier;

            --  Mark character is an error (at start of identifer)

            elsif Is_UTF_32_Mark (Cat) then
               Error_Msg
                 ("identifier cannot start with mark character", Wptr);
               Scan_Ptr := Wptr;
               Name_Len := 0;
               Underline_Found := False;
               goto Scan_Identifier;

            --  Other format character is an error (at start of identifer)

            elsif Is_UTF_32_Other (Cat) then
               Error_Msg
                 ("identifier cannot start with other format character", Wptr);
               Scan_Ptr := Wptr;
               Name_Len := 0;
               Underline_Found := False;
               goto Scan_Identifier;

            --  Extended digit character is an error. Could be bad start of
            --  identifier or bad literal. Not worth doing too much to try to
            --  distinguish these cases, but we will do a little bit.

            elsif Is_UTF_32_Digit (Cat) then
               Error_Msg
                 ("identifier cannot start with digit character", Wptr);
               Scan_Ptr := Wptr;
               Name_Len := 0;
               Underline_Found := False;
               goto Scan_Identifier;

            --  All other wide characters are illegal here

            else
               Error_Illegal_Wide_Character;
               goto Scan_Next_Character;
            end if;
         end;

      --  Routine to scan line terminator. On entry Scan_Ptr points to a
      --  character which is one of FF,LR,CR,VT, or one of the wide characters
      --  that is treated as a line termiantor.

      <<Scan_Line_Terminator>>

         --  Check line too long

         Check_End_Of_Line;

         --  Set Token_Ptr, if End_Of_Line is a token, for the case when it is
         --  a physical line.

         if End_Of_Line_Is_Token then
            Token_Ptr := Scan_Ptr;
         end if;

         declare
            Physical : Boolean;

         begin
            Skip_Line_Terminators (Scan_Ptr, Physical);

            --  If we are at start of physical line, update scan pointers to
            --  reflect the start of the new line.

            if Physical then
               Current_Line_Start       := Scan_Ptr;
               Start_Column             := Set_Start_Column;
               First_Non_Blank_Location := Scan_Ptr;

               --  If End_Of_Line is a token, we return it as it is a
               --  physical line.

               if End_Of_Line_Is_Token then
                  Token := Tok_End_Of_Line;
                  return;
               end if;
            end if;
         end;

         goto Scan_Next_Character;

      --  Identifier scanning routine. On entry, some initial characters of
      --  the identifier may have already been stored in Name_Buffer. If so,
      --  Name_Len has the number of characters stored. otherwise Name_Len is
      --  set to zero on entry. Underline_Found is also set False on entry.

      <<Scan_Identifier>>

         --  This loop scans as fast as possible past lower half letters and
         --  digits, which we expect to be the most common characters.

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

            Underline_Found := False;
            Scan_Ptr := Scan_Ptr + 1;
            Name_Len := Name_Len + 1;
         end loop;

         --  If we fall through, then we have encountered either an underline
         --  character, or an extended identifier character (i.e. one from the
         --  upper half), or a wide character, or an identifier terminator. The
         --  initial test speeds us up in the most common case where we have
         --  an identifier terminator. Note that ESC is an identifier character
         --  only if a wide character encoding method that uses ESC encoding
         --  is active, so if we find an ESC character we know that we have a
         --  wide character.

         if Identifier_Char (Source (Scan_Ptr)) then

            --  Case of underline

            if Source (Scan_Ptr) = '_' then
               Accumulate_Checksum ('_');

               if Underline_Found then
                  Error_No_Double_Underline;
               else
                  Underline_Found := True;
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := '_';
               end if;

               Scan_Ptr := Scan_Ptr + 1;
               goto Scan_Identifier;

            --  Upper half character

            elsif Source (Scan_Ptr) in Upper_Half_Character
              and then not Upper_Half_Encoding
            then
               Accumulate_Checksum (Source (Scan_Ptr));
               Store_Encoded_Character
                 (Get_Char_Code (Fold_Lower (Source (Scan_Ptr))));
               Scan_Ptr := Scan_Ptr + 1;
               Underline_Found := False;
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
                  Code : Char_Code;
                  Err  : Boolean;
                  Chr  : Character;
                  Cat  : Category;

               begin
                  Wptr := Scan_Ptr;
                  Scan_Wide (Source, Scan_Ptr, Code, Err);

                  --  If error, signal error

                  if Err then
                     Error_Illegal_Wide_Character;

                  --  If the character scanned is a normal identifier
                  --  character, then we treat it that way.

                  elsif In_Character_Range (Code)
                    and then Identifier_Char (Get_Character (Code))
                  then
                     Chr := Get_Character (Code);
                     Accumulate_Checksum (Chr);
                     Store_Encoded_Character
                       (Get_Char_Code (Fold_Lower (Chr)));
                     Underline_Found := False;

                  --  Here if not a normal identifier character

                  else
                     --  Make sure we are allowing wide characters in
                     --  identifiers. Note that we allow wide character
                     --  notation for an OK identifier character. This in
                     --  particular allows bracket or other notation to be
                     --  used for upper half letters.

                     --  Wide characters are always allowed in Ada 2005

                     if Identifier_Character_Set /= 'w'
                       and then Ada_Version < Ada_05
                     then
                        Error_Msg
                       ("wide character not allowed in identifier", Wptr);
                     end if;

                     Cat := Get_Category (UTF_32 (Code));

                     --  If OK letter, store it folding to upper case. Note
                     --  that we include the folded letter in the checksum.

                     if Is_UTF_32_Letter (Cat) then
                        Code :=
                          Char_Code (UTF_32_To_Upper_Case (UTF_32 (Code)));
                        Accumulate_Checksum (Code);
                        Store_Encoded_Character (Code);
                        Underline_Found := False;

                     --  If OK extended digit or mark, then store it

                     elsif Is_UTF_32_Digit (Cat)
                       or else Is_UTF_32_Mark (Cat)
                     then
                        Accumulate_Checksum (Code);
                        Store_Encoded_Character (Code);
                        Underline_Found := False;

                     --  Wide punctuation is also stored, but counts as an
                     --  underline character for error checking purposes.

                     elsif Is_UTF_32_Punctuation (Cat) then
                        Accumulate_Checksum (Code);

                        if Underline_Found then
                           declare
                              Cend : constant Source_Ptr := Scan_Ptr;
                           begin
                              Scan_Ptr := Wptr;
                              Error_No_Double_Underline;
                              Scan_Ptr := Cend;
                           end;

                        else
                           Store_Encoded_Character (Code);
                           Underline_Found := True;
                        end if;

                     --  Wide character in Unicode cateogory "Other, Format"
                     --  is accepted in an identifier, but is ignored and not
                     --  stored. It seems reasonable to exclude it from the
                     --  checksum.

                     --  Note that it is correct (see AI-395) to simply strip
                     --  other format characters, before testing for double
                     --  underlines, or for reserved words).

                     elsif Is_UTF_32_Other (Cat) then
                        null;

                     --  Wide character in category Separator,Space terminates

                     elsif Is_UTF_32_Space (Cat) then
                        goto Scan_Identifier_Complete;

                     --  Any other wide character is not acceptable

                     else
                        Error_Msg
                          ("invalid wide character in identifier", Wptr);
                     end if;
                  end if;

                  goto Scan_Identifier;
               end;
            end if;
         end if;

      --  Scan of identifier is complete. The identifier is stored in
      --  Name_Buffer, and Scan_Ptr points past the last character.

      <<Scan_Identifier_Complete>>
         Token_Name := Name_Find;

         --  Check for identifier ending with underline or punctuation char

         if Underline_Found then
            Underline_Found := False;

            if Source (Scan_Ptr - 1) = '_' then
               Error_Msg
                 ("identifier cannot end with underline", Scan_Ptr - 1);
            else
               Error_Msg
                 ("identifier cannot end with punctuation character", Wptr);
            end if;
         end if;

         --  Here is where we check if it was a keyword

         if Get_Name_Table_Byte (Token_Name) /= 0
           and then (Ada_Version >= Ada_95
                       or else Token_Name not in Ada_95_Reserved_Words)
           and then (Ada_Version >= Ada_05
                       or else Token_Name not in Ada_2005_Reserved_Words)
         then
            Token := Token_Type'Val (Get_Name_Table_Byte (Token_Name));

            --  Deal with possible style check for non-lower case keyword, but
            --  we don't treat ACCESS, DELTA, DIGITS, RANGE as keywords for
            --  this purpose if they appear as attribute designators. Actually
            --  we only check the first character for speed.

            --  Ada 2005 (AI-284): Do not apply the style check in case of
            --  "pragma Interface"

            --  Ada 2005 (AI-340): Do not apply the style check in case of
            --  MOD attribute.

            if Style_Check
              and then Source (Token_Ptr) <= 'Z'
              and then (Prev_Token /= Tok_Apostrophe
                          or else
                            (Token /= Tok_Access and then
                             Token /= Tok_Delta  and then
                             Token /= Tok_Digits and then
                             Token /= Tok_Mod    and then
                             Token /= Tok_Range))
              and then (Token /= Tok_Interface
                          or else
                            (Token = Tok_Interface
                               and then Prev_Token /= Tok_Pragma))
            then
               Style.Non_Lower_Case_Keyword;
            end if;

            --  We must reset Token_Name since this is not an identifier and
            --  if we leave Token_Name set, the parser gets confused because
            --  it thinks it is dealing with an identifier instead of the
            --  corresponding keyword.

            Token_Name := No_Name;
            Accumulate_Token_Checksum;
            return;

         --  It is an identifier after all

         else
            Token := Tok_Identifier;
            Accumulate_Token_Checksum;
            Post_Scan;
            return;
         end if;
   end Scan;

   --------------------------
   -- Set_Comment_As_Token --
   --------------------------

   procedure Set_Comment_As_Token (Value : Boolean) is
   begin
      Comment_Is_Token := Value;
   end Set_Comment_As_Token;

   ------------------------------
   -- Set_End_Of_Line_As_Token --
   ------------------------------

   procedure Set_End_Of_Line_As_Token (Value : Boolean) is
   begin
      End_Of_Line_Is_Token := Value;
   end Set_End_Of_Line_As_Token;

   ---------------------------
   -- Set_Special_Character --
   ---------------------------

   procedure Set_Special_Character (C : Character) is
   begin
      case C is
         when '#' | '$' | '_' | '?' | '@' | '`' | '\' | '^' | '~' =>
            Special_Characters (C) := True;

         when others =>
            null;
      end case;
   end Set_Special_Character;

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

end Scng;
