------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C H A R A C T E R S . H A N D L I N G               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Latin_1;     use Ada.Characters.Latin_1;
with Ada.Strings.Maps;           use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;

package body Ada.Characters.Handling is

   ------------------------------------
   -- Character Classification Table --
   ------------------------------------

   type Character_Flags is mod 256;
   for Character_Flags'Size use 8;

   Control    : constant Character_Flags := 1;
   Lower      : constant Character_Flags := 2;
   Upper      : constant Character_Flags := 4;
   Basic      : constant Character_Flags := 8;
   Hex_Digit  : constant Character_Flags := 16;
   Digit      : constant Character_Flags := 32;
   Special    : constant Character_Flags := 64;
   Line_Term  : constant Character_Flags := 128;

   Letter     : constant Character_Flags := Lower or Upper;
   Alphanum   : constant Character_Flags := Letter or Digit;
   Graphic    : constant Character_Flags := Alphanum or Special;

   Char_Map : constant array (Character) of Character_Flags :=
   (
     NUL                         => Control,
     SOH                         => Control,
     STX                         => Control,
     ETX                         => Control,
     EOT                         => Control,
     ENQ                         => Control,
     ACK                         => Control,
     BEL                         => Control,
     BS                          => Control,
     HT                          => Control,
     LF                          => Control + Line_Term,
     VT                          => Control + Line_Term,
     FF                          => Control + Line_Term,
     CR                          => Control + Line_Term,
     SO                          => Control,
     SI                          => Control,

     DLE                         => Control,
     DC1                         => Control,
     DC2                         => Control,
     DC3                         => Control,
     DC4                         => Control,
     NAK                         => Control,
     SYN                         => Control,
     ETB                         => Control,
     CAN                         => Control,
     EM                          => Control,
     SUB                         => Control,
     ESC                         => Control,
     FS                          => Control,
     GS                          => Control,
     RS                          => Control,
     US                          => Control,

     Space                       => Special,
     Exclamation                 => Special,
     Quotation                   => Special,
     Number_Sign                 => Special,
     Dollar_Sign                 => Special,
     Percent_Sign                => Special,
     Ampersand                   => Special,
     Apostrophe                  => Special,
     Left_Parenthesis            => Special,
     Right_Parenthesis           => Special,
     Asterisk                    => Special,
     Plus_Sign                   => Special,
     Comma                       => Special,
     Hyphen                      => Special,
     Full_Stop                   => Special,
     Solidus                     => Special,

     '0' .. '9'                  => Digit + Hex_Digit,

     Colon                       => Special,
     Semicolon                   => Special,
     Less_Than_Sign              => Special,
     Equals_Sign                 => Special,
     Greater_Than_Sign           => Special,
     Question                    => Special,
     Commercial_At               => Special,

     'A' .. 'F'                  => Upper + Basic + Hex_Digit,
     'G' .. 'Z'                  => Upper + Basic,

     Left_Square_Bracket         => Special,
     Reverse_Solidus             => Special,
     Right_Square_Bracket        => Special,
     Circumflex                  => Special,
     Low_Line                    => Special,
     Grave                       => Special,

     'a' .. 'f'                  => Lower + Basic + Hex_Digit,
     'g' .. 'z'                  => Lower + Basic,

     Left_Curly_Bracket          => Special,
     Vertical_Line               => Special,
     Right_Curly_Bracket         => Special,
     Tilde                       => Special,

     DEL                         => Control,
     Reserved_128                => Control,
     Reserved_129                => Control,
     BPH                         => Control,
     NBH                         => Control,
     Reserved_132                => Control,
     NEL                         => Control + Line_Term,
     SSA                         => Control,
     ESA                         => Control,
     HTS                         => Control,
     HTJ                         => Control,
     VTS                         => Control,
     PLD                         => Control,
     PLU                         => Control,
     RI                          => Control,
     SS2                         => Control,
     SS3                         => Control,

     DCS                         => Control,
     PU1                         => Control,
     PU2                         => Control,
     STS                         => Control,
     CCH                         => Control,
     MW                          => Control,
     SPA                         => Control,
     EPA                         => Control,

     SOS                         => Control,
     Reserved_153                => Control,
     SCI                         => Control,
     CSI                         => Control,
     ST                          => Control,
     OSC                         => Control,
     PM                          => Control,
     APC                         => Control,

     No_Break_Space              => Special,
     Inverted_Exclamation        => Special,
     Cent_Sign                   => Special,
     Pound_Sign                  => Special,
     Currency_Sign               => Special,
     Yen_Sign                    => Special,
     Broken_Bar                  => Special,
     Section_Sign                => Special,
     Diaeresis                   => Special,
     Copyright_Sign              => Special,
     Feminine_Ordinal_Indicator  => Special,
     Left_Angle_Quotation        => Special,
     Not_Sign                    => Special,
     Soft_Hyphen                 => Special,
     Registered_Trade_Mark_Sign  => Special,
     Macron                      => Special,
     Degree_Sign                 => Special,
     Plus_Minus_Sign             => Special,
     Superscript_Two             => Special,
     Superscript_Three           => Special,
     Acute                       => Special,
     Micro_Sign                  => Special,
     Pilcrow_Sign                => Special,
     Middle_Dot                  => Special,
     Cedilla                     => Special,
     Superscript_One             => Special,
     Masculine_Ordinal_Indicator => Special,
     Right_Angle_Quotation       => Special,
     Fraction_One_Quarter        => Special,
     Fraction_One_Half           => Special,
     Fraction_Three_Quarters     => Special,
     Inverted_Question           => Special,

     UC_A_Grave                  => Upper,
     UC_A_Acute                  => Upper,
     UC_A_Circumflex             => Upper,
     UC_A_Tilde                  => Upper,
     UC_A_Diaeresis              => Upper,
     UC_A_Ring                   => Upper,
     UC_AE_Diphthong             => Upper + Basic,
     UC_C_Cedilla                => Upper,
     UC_E_Grave                  => Upper,
     UC_E_Acute                  => Upper,
     UC_E_Circumflex             => Upper,
     UC_E_Diaeresis              => Upper,
     UC_I_Grave                  => Upper,
     UC_I_Acute                  => Upper,
     UC_I_Circumflex             => Upper,
     UC_I_Diaeresis              => Upper,
     UC_Icelandic_Eth            => Upper + Basic,
     UC_N_Tilde                  => Upper,
     UC_O_Grave                  => Upper,
     UC_O_Acute                  => Upper,
     UC_O_Circumflex             => Upper,
     UC_O_Tilde                  => Upper,
     UC_O_Diaeresis              => Upper,

     Multiplication_Sign         => Special,

     UC_O_Oblique_Stroke         => Upper,
     UC_U_Grave                  => Upper,
     UC_U_Acute                  => Upper,
     UC_U_Circumflex             => Upper,
     UC_U_Diaeresis              => Upper,
     UC_Y_Acute                  => Upper,
     UC_Icelandic_Thorn          => Upper + Basic,

     LC_German_Sharp_S           => Lower + Basic,
     LC_A_Grave                  => Lower,
     LC_A_Acute                  => Lower,
     LC_A_Circumflex             => Lower,
     LC_A_Tilde                  => Lower,
     LC_A_Diaeresis              => Lower,
     LC_A_Ring                   => Lower,
     LC_AE_Diphthong             => Lower + Basic,
     LC_C_Cedilla                => Lower,
     LC_E_Grave                  => Lower,
     LC_E_Acute                  => Lower,
     LC_E_Circumflex             => Lower,
     LC_E_Diaeresis              => Lower,
     LC_I_Grave                  => Lower,
     LC_I_Acute                  => Lower,
     LC_I_Circumflex             => Lower,
     LC_I_Diaeresis              => Lower,
     LC_Icelandic_Eth            => Lower + Basic,
     LC_N_Tilde                  => Lower,
     LC_O_Grave                  => Lower,
     LC_O_Acute                  => Lower,
     LC_O_Circumflex             => Lower,
     LC_O_Tilde                  => Lower,
     LC_O_Diaeresis              => Lower,

     Division_Sign               => Special,

     LC_O_Oblique_Stroke         => Lower,
     LC_U_Grave                  => Lower,
     LC_U_Acute                  => Lower,
     LC_U_Circumflex             => Lower,
     LC_U_Diaeresis              => Lower,
     LC_Y_Acute                  => Lower,
     LC_Icelandic_Thorn          => Lower + Basic,
     LC_Y_Diaeresis              => Lower
   );

   ---------------------
   -- Is_Alphanumeric --
   ---------------------

   function Is_Alphanumeric (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Alphanum) /= 0;
   end Is_Alphanumeric;

   --------------
   -- Is_Basic --
   --------------

   function Is_Basic (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Basic) /= 0;
   end Is_Basic;

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Item : Wide_Character) return Boolean is
   begin
      return Wide_Character'Pos (Item) < 256;
   end Is_Character;

   ----------------
   -- Is_Control --
   ----------------

   function Is_Control (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Control) /= 0;
   end Is_Control;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (Item : Character) return Boolean is
   begin
      return Item in '0' .. '9';
   end Is_Digit;

   ----------------
   -- Is_Graphic --
   ----------------

   function Is_Graphic (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Graphic) /= 0;
   end Is_Graphic;

   --------------------------
   -- Is_Hexadecimal_Digit --
   --------------------------

   function Is_Hexadecimal_Digit (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Hex_Digit) /= 0;
   end Is_Hexadecimal_Digit;

   ----------------
   -- Is_ISO_646 --
   ----------------

   function Is_ISO_646 (Item : Character) return Boolean is
   begin
      return Item in ISO_646;
   end Is_ISO_646;

   --  Note: much more efficient coding of the following function is possible
   --  by testing several 16#80# bits in a complete word in a single operation

   function Is_ISO_646 (Item : String) return Boolean is
   begin
      for J in Item'Range loop
         if Item (J) not in ISO_646 then
            return False;
         end if;
      end loop;

      return True;
   end Is_ISO_646;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Letter) /= 0;
   end Is_Letter;

   ------------------------
   -- Is_Line_Terminator --
   ------------------------

   function Is_Line_Terminator (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Line_Term) /= 0;
   end Is_Line_Terminator;

   --------------
   -- Is_Lower --
   --------------

   function Is_Lower (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Lower) /= 0;
   end Is_Lower;

   -------------
   -- Is_Mark --
   -------------

   function Is_Mark (Item : Character) return Boolean is
      pragma Unreferenced (Item);
   begin
      return False;
   end Is_Mark;

   ---------------------
   -- Is_Other_Format --
   ---------------------

   function Is_Other_Format (Item : Character) return Boolean is
   begin
      return Item = Soft_Hyphen;
   end Is_Other_Format;

   ------------------------------
   -- Is_Punctuation_Connector --
   ------------------------------

   function Is_Punctuation_Connector (Item : Character) return Boolean is
   begin
      return Item = '_';
   end Is_Punctuation_Connector;

   --------------
   -- Is_Space --
   --------------

   function Is_Space (Item : Character) return Boolean is
   begin
      return Item = ' ' or else Item = No_Break_Space;
   end Is_Space;

   ----------------
   -- Is_Special --
   ----------------

   function Is_Special (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Special) /= 0;
   end Is_Special;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (Item : Wide_String) return Boolean is
   begin
      for J in Item'Range loop
         if Wide_Character'Pos (Item (J)) >= 256 then
            return False;
         end if;
      end loop;

      return True;
   end Is_String;

   --------------
   -- Is_Upper --
   --------------

   function Is_Upper (Item : Character) return Boolean is
   begin
      return (Char_Map (Item) and Upper) /= 0;
   end Is_Upper;

   --------------
   -- To_Basic --
   --------------

   function To_Basic (Item : Character) return Character is
   begin
      return Value (Basic_Map, Item);
   end To_Basic;

   function To_Basic (Item : String) return String is
   begin
      return Result : String (1 .. Item'Length) do
         for J in Item'Range loop
            Result (J - (Item'First - 1)) := Value (Basic_Map, Item (J));
         end loop;
      end return;
   end To_Basic;

   ------------------
   -- To_Character --
   ------------------

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character
   is
   begin
      if Is_Character (Item) then
         return Character'Val (Wide_Character'Pos (Item));
      else
         return Substitute;
      end if;
   end To_Character;

   ----------------
   -- To_ISO_646 --
   ----------------

   function To_ISO_646
     (Item       : Character;
      Substitute : ISO_646 := ' ') return ISO_646
   is
   begin
      return (if Item in ISO_646 then Item else Substitute);
   end To_ISO_646;

   function To_ISO_646
     (Item       : String;
      Substitute : ISO_646 := ' ') return String
   is
      Result : String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) :=
           (if Item (J) in ISO_646 then Item (J) else Substitute);
      end loop;

      return Result;
   end To_ISO_646;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : Character) return Character is
   begin
      return Value (Lower_Case_Map, Item);
   end To_Lower;

   function To_Lower (Item : String) return String is
   begin
      return Result : String (1 .. Item'Length) do
         for J in Item'Range loop
            Result (J - (Item'First - 1)) := Value (Lower_Case_Map, Item (J));
         end loop;
      end return;
   end To_Lower;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String
   is
      Result : String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Character (Item (J), Substitute);
      end loop;

      return Result;
   end To_String;

   --------------
   -- To_Upper --
   --------------

   function To_Upper
     (Item : Character) return Character
   is
   begin
      return Value (Upper_Case_Map, Item);
   end To_Upper;

   function To_Upper
     (Item : String) return String
   is
   begin
      return Result : String (1 .. Item'Length) do
         for J in Item'Range loop
            Result (J - (Item'First - 1)) := Value (Upper_Case_Map, Item (J));
         end loop;
      end return;
   end To_Upper;

   -----------------------
   -- To_Wide_Character --
   -----------------------

   function To_Wide_Character
     (Item : Character) return Wide_Character
   is
   begin
      return Wide_Character'Val (Character'Pos (Item));
   end To_Wide_Character;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String
     (Item : String) return Wide_String
   is
      Result : Wide_String (1 .. Item'Length);

   begin
      for J in Item'Range loop
         Result (J - (Item'First - 1)) := To_Wide_Character (Item (J));
      end loop;

      return Result;
   end To_Wide_String;

end Ada.Characters.Handling;
