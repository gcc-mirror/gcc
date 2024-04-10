------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . C H A R A C T E R S . H A N D L I N G               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  Postconditions in this unit are meant for analysis only, not for run-time
--  checking, in order not to slow down the execution of these functions.

pragma Assertion_Policy (Post => Ignore);

with Ada.Characters.Latin_1;

package Ada.Characters.Handling with
  SPARK_Mode,
  Always_Terminates
is
   pragma Pure;
   --  In accordance with Ada 2005 AI-362

   ----------------------------------------
   -- Character Classification Functions --
   ----------------------------------------

   --  In the description below for each function that returns a Boolean
   --  result, the effect is described in terms of the conditions under which
   --  the value True is returned. If these conditions are not met, then the
   --  function returns False.
   --
   --  Each of the following classification functions has a formal Character
   --  parameter, Item, and returns a Boolean result.

   function Is_Control               (Item : Character) return Boolean
   with
     Post => Is_Control'Result =
       (Character'Pos (Item) in 0 .. 31 | 127 .. 159);
   --  True if Item is a control character. A control character is a character
   --  whose position is in one of the ranges 0..31 or 127..159.

   function Is_Graphic               (Item : Character) return Boolean
   with
     Post => Is_Graphic'Result =
       (Character'Pos (Item) in 32 .. 126 | 160 .. 255);
   --  True if Item is a graphic character. A graphic character is a character
   --  whose position is in one of the ranges 32..126 or 160..255.

   function Is_Letter                (Item : Character) return Boolean
   with
     Post => Is_Letter'Result =
       (Item in 'A' .. 'Z' | 'a' .. 'z'
         or else Character'Pos (Item) in 192 .. 214 | 216 .. 246 | 248 .. 255);
   --  True if Item is a letter. A letter is a character that is in one of the
   --  ranges 'A'..'Z' or 'a'..'z', or whose position is in one of the ranges
   --  192..214, 216..246, or 248..255.

   function Is_Lower                 (Item : Character) return Boolean
   with
     Post => Is_Lower'Result =
       (Item in 'a' .. 'z'
         or else Character'Pos (Item) in 223 .. 246 | 248 .. 255);
   --  True if Item is a lower-case letter. A lower-case letter is a character
   --  that is in the range 'a'..'z', or whose position is in one of the ranges
   --  223..246 or 248..255.

   function Is_Upper                 (Item : Character) return Boolean
   with
     Post => Is_Upper'Result =
       (Item in 'A' .. 'Z'
         or else Character'Pos (Item) in 192 .. 214 | 216 .. 222);
   --  True if Item is an upper-case letter. An upper-case letter is a
   --  character that is in the range 'A'..'Z' or whose position is in one
   --  of the ranges 192..214 or 216..222.

   function Is_Basic                 (Item : Character) return Boolean
   with
     Post => Is_Basic'Result =
       (Item in 'A' .. 'Z'
          | 'a' .. 'z'
          | Latin_1.UC_AE_Diphthong
          | Latin_1.LC_AE_Diphthong
          | Latin_1.UC_Icelandic_Eth
          | Latin_1.LC_Icelandic_Eth
          | Latin_1.UC_Icelandic_Thorn
          | Latin_1.LC_Icelandic_Thorn
          | Latin_1.LC_German_Sharp_S);
   --  True if Item is a basic letter. A basic letter is a character that
   --  is in one of the ranges 'A'..'Z' and 'a'..'z', or that is one of
   --  the following: UC_AE_Diphthong, LC_AE_Diphthong, UC_Icelandic_Eth,
   --  LC_Icelandic_Eth, UC_Icelandic_Thorn, LC_Icelandic_Thorn, or
   --  LC_German_Sharp_S.

   function Is_Digit                 (Item : Character) return Boolean
   with
     Post => Is_Digit'Result = (Item in '0' .. '9');
   --  True if Item is a decimal digit. A decimal digit is a character in the
   --  range '0'..'9'.

   function Is_Decimal_Digit         (Item : Character) return Boolean
     renames Is_Digit;

   function Is_Hexadecimal_Digit     (Item : Character) return Boolean
   with
     Post => Is_Hexadecimal_Digit'Result =
       (Is_Decimal_Digit (Item) or Item in 'A' .. 'F' | 'a' .. 'f');
   --  True if Item is a hexadecimal digit. A hexadecimal digit is a character
   --  that is either a decimal digit or that is in one of the ranges 'A'..'F'
   --  or 'a'..'f'.

   function Is_Alphanumeric          (Item : Character) return Boolean
   with
     Post => Is_Alphanumeric'Result =
       (Is_Letter (Item) or Is_Decimal_Digit (Item));
   --  True if Item is an alphanumeric character. An alphanumeric character is
   --  a character that is either a letter or a decimal digit.

   function Is_Special               (Item : Character) return Boolean
   with
     Post => Is_Special'Result =
       (Is_Graphic (Item) and not Is_Alphanumeric (Item));
   --  True if Item is a special graphic character. A special graphic character
   --  is a graphic character that is not alphanumeric.

   function Is_Line_Terminator       (Item : Character) return Boolean
   with
     Post => Is_Line_Terminator'Result =
       (Character'Pos (Item) in 10 .. 13 | 133);
   --  True if Item is a character with position 10..13 (Line_Feed,
   --  Line_Tabulation, Form_Feed, Carriage_Return) or 133 (Next_Line).

   function Is_Mark                  (Item : Character) return Boolean
   with
     Post => Is_Mark'Result = False;
   --  Never True (no value of type Character has categories Mark, Non-Spacing
   --  or Mark, Spacing Combining).

   function Is_Other_Format          (Item : Character) return Boolean
   with
     Post => Is_Other_Format'Result = (Character'Pos (Item) = 173);
   --  True if Item is a character with position 173 (Soft_Hyphen).

   function Is_Punctuation_Connector (Item : Character) return Boolean
   with
     Post => Is_Punctuation_Connector'Result =
       (Character'Pos (Item) = 95);
   --  True if Item is a character with position 95 ('_', known as Low_Line or
   --  Underscore).

   function Is_Space                 (Item : Character) return Boolean
   with
     Post => Is_Space'Result = (Character'Pos (Item) in 32 | 160);
   --  True if Item is a character with position 32 (' ') or 160
   --  (No_Break_Space).

   function Is_NFKC                  (Item : Character) return Boolean
   with
     Post => Is_NFKC'Result =
       (Character'Pos (Item) not in
            160 | 168 | 170 | 175 | 178 | 179 | 180
          | 181 | 184 | 185 | 186 | 188 | 189 | 190);
   --  True if Item could be present in a string normalized to Normalization
   --  Form KC (as defined by Clause 21 of ISO/IEC 10646:2017); this includes
   --  all characters except those with positions 160, 168, 170, 175, 178, 179,
   --  180, 181, 184, 185, 186, 188, 189, and 190.

   ---------------------------------------------------
   -- Conversion Functions for Character and String --
   ---------------------------------------------------

   --  Each of the names To_Lower, To_Upper, and To_Basic refers to two
   --  functions: one that converts from Character to Character, and
   --  the other that converts from String to String. The result of each
   --  Character-to-Character function is described below, in terms of
   --  the conversion applied to Item, its formal Character parameter. The
   --  result of each String-to-String conversion is obtained by applying
   --  to each element of the function's String parameter the corresponding
   --  Character-to-Character conversion; the result is the null String if the
   --  value of the formal parameter is the null String. The lower bound of the
   --  result String is 1.

   function To_Lower (Item : Character) return Character
   with
     Post => To_Lower'Result =
       (if Is_Upper (Item) then
          Character'Val (Character'Pos (Item) +
            (if Item in 'A' .. 'Z' then
               Character'Pos ('a') - Character'Pos ('A')
             else
               Character'Pos (Latin_1.LC_A_Grave)
                 - Character'Pos (Latin_1.UC_A_Grave)))
        else
          Item);
   --  Returns the corresponding lower-case value for Item if Is_Upper(Item),
   --  and returns Item otherwise.

   function To_Upper (Item : Character) return Character
   with
     Post => To_Upper'Result =
       (if Is_Lower (Item)
          and then Item not in Latin_1.LC_German_Sharp_S
                             | Latin_1.LC_Y_Diaeresis
        then
          Character'Val (Character'Pos (Item) +
            (if Item in 'A' .. 'Z' then
               Character'Pos ('A') - Character'Pos ('a')
             else
               Character'Pos (Latin_1.UC_A_Grave)
                 - Character'Pos (Latin_1.LC_A_Grave)))
        else
          Item);
   --  Returns the corresponding upper-case value for Item if Is_Lower(Item)
   --  and Item has an upper-case form, and returns Item otherwise. The lower
   --  case letters LC_German_Sharp_S and LC_Y_Diaeresis do not have upper case
   --  forms.

   function To_Basic (Item : Character) return Character
   with
     Post => To_Basic'Result =
       (if not Is_Letter (Item) or else Is_Basic (Item) then
          Item
        else
          (case Item is
             when Latin_1.UC_A_Grave .. Latin_1.UC_A_Ring      => 'A',
             when Latin_1.UC_C_Cedilla                         => 'C',
             when Latin_1.UC_E_Grave .. Latin_1.UC_E_Diaeresis => 'E',
             when Latin_1.UC_I_Grave .. Latin_1.UC_I_Diaeresis => 'I',
             when Latin_1.UC_N_Tilde                           => 'N',
             when Latin_1.UC_O_Grave .. Latin_1.UC_O_Diaeresis => 'O',
             when Latin_1.UC_O_Oblique_Stroke                  => 'O',
             when Latin_1.UC_U_Grave .. Latin_1.UC_U_Diaeresis => 'U',
             when Latin_1.UC_Y_Acute                           => 'Y',
             when Latin_1.LC_A_Grave .. Latin_1.LC_A_Ring      => 'a',
             when Latin_1.LC_C_Cedilla                         => 'c',
             when Latin_1.LC_E_Grave .. Latin_1.LC_E_Diaeresis => 'e',
             when Latin_1.LC_I_Grave .. Latin_1.LC_I_Diaeresis => 'i',
             when Latin_1.LC_N_Tilde                           => 'n',
             when Latin_1.LC_O_Grave .. Latin_1.LC_O_Diaeresis => 'o',
             when Latin_1.LC_O_Oblique_Stroke                  => 'o',
             when Latin_1.LC_U_Grave .. Latin_1.LC_U_Diaeresis => 'u',
             when Latin_1.LC_Y_Acute                           => 'y',
             when Latin_1.LC_Y_Diaeresis                       => 'y',
             when others => raise Program_Error));
   --  Returns the letter corresponding to Item but with no diacritical mark,
   --  if Item is a letter but not a basic letter; returns Item otherwise.

   function To_Lower (Item : String) return String
   with
     Post => To_Lower'Result'First = 1
       and then To_Lower'Result'Length = Item'Length
       and then
         (for all J in To_Lower'Result'Range =>
            To_Lower'Result (J) = To_Lower (Item (Item'First + (J - 1))));

   function To_Upper (Item : String) return String
   with
     Post => To_Upper'Result'First = 1
       and then To_Upper'Result'Length = Item'Length
       and then
         (for all J in To_Upper'Result'Range =>
            To_Upper'Result (J) = To_Upper (Item (Item'First + (J - 1))));

   function To_Basic (Item : String) return String
   with
     Post => To_Basic'Result'First = 1
       and then To_Basic'Result'Length = Item'Length
       and then
         (for all J in To_Basic'Result'Range =>
            To_Basic'Result (J) = To_Basic (Item (Item'First + (J - 1))));

   ----------------------------------------------------------------------
   -- Classifications of and Conversions Between Character and ISO 646 --
   ----------------------------------------------------------------------

   --  The following set of functions test for membership in the ISO 646
   --  character range, or convert between ISO 646 and Character.

   subtype ISO_646 is
     Character range Character'Val (0) .. Character'Val (127);

   function Is_ISO_646 (Item : Character) return Boolean
   with
     Post => Is_ISO_646'Result = (Item in ISO_646);
   --  The function whose formal parameter, Item, is of type Character returns
   --  True if Item is in the subtype ISO_646.

   function Is_ISO_646 (Item : String)    return Boolean
   with
     Post => Is_ISO_646'Result =
       (for all J in Item'Range => Is_ISO_646 (Item (J)));
   --  The function whose formal parameter, Item, is of type String returns
   --  True if Is_ISO_646(Item(I)) is True for each I in Item'Range.

   function To_ISO_646
     (Item       : Character;
      Substitute : ISO_646 := ' ') return ISO_646
   with
     Post => To_ISO_646'Result =
       (if Is_ISO_646 (Item) then Item else Substitute);
   --  The function whose first formal parameter, Item, is of type Character
   --  returns Item if Is_ISO_646(Item), and returns the Substitute ISO_646
   --  character otherwise.

   function To_ISO_646
     (Item       : String;
      Substitute : ISO_646 := ' ') return String
   with
     Post => To_ISO_646'Result'First = 1
       and then To_ISO_646'Result'Length = Item'Length
       and then
         (for all J in To_ISO_646'Result'Range =>
            To_ISO_646'Result (J) =
              To_ISO_646 (Item (Item'First + (J - 1)), Substitute));
   --  The function whose first formal parameter, Item, is of type String
   --  returns the String whose Range is 1..Item'Length and each of whose
   --  elements is given by To_ISO_646 of the corresponding element in Item.

   ------------------------------------------------------
   -- Classifications of Wide_Character and Characters --
   ------------------------------------------------------

   --  Ada 2005 AI 395: these functions are moved to Ada.Characters.Conversions
   --  and are considered obsolete in Ada.Characters.Handling. However we do
   --  not complain about this obsolescence, since in practice it is necessary
   --  to use these routines when creating code that is intended to run in
   --  either Ada 95 or Ada 2005 mode.

   --  We do however have to flag these if the pragma No_Obsolescent_Features
   --  restriction is active (see Restrict.Check_Obsolescent_2005_Entity).

   function Is_Character (Item : Wide_Character) return Boolean
   with
     Post => Is_Character'Result =
       (Wide_Character'Pos (Item) <= Character'Pos (Character'Last));
   --  Returns True if Wide_Character'Pos(Item) <=
   --  Character'Pos(Character'Last).

   function Is_String    (Item : Wide_String)    return Boolean
   with
     Post => Is_String'Result =
       (for all I in Item'Range => Is_Character (Item (I)));
   --  Returns True if Is_Character(Item(I)) is True for each I in Item'Range.

   ------------------------------------------------------
   -- Conversions between Wide_Character and Character --
   ------------------------------------------------------

   --  Ada 2005 AI 395: these functions are moved to Ada.Characters.Conversions
   --  and are considered obsolete in Ada.Characters.Handling. However we do
   --  not complain about this obsolescence, since in practice it is necessary
   --  to use these routines when creating code that is intended to run in
   --  either Ada 95 or Ada 2005 mode.

   --  We do however have to flag these if the pragma No_Obsolescent_Features
   --  restriction is active (see Restrict.Check_Obsolescent_2005_Entity).

   function To_Character
     (Item       : Wide_Character;
      Substitute : Character := ' ') return Character
   with
     Post => To_Character'Result =
       (if Is_Character (Item) then
          Character'Val (Wide_Character'Pos (Item))
        else
          Substitute);
   --  Returns the Character corresponding to Item if Is_Character(Item), and
   --  returns the Substitute Character otherwise.

   function To_String
     (Item       : Wide_String;
      Substitute : Character := ' ') return String
   with
     Post => To_String'Result'First = 1
       and then To_String'Result'Length = Item'Length
       and then
         (for all J in To_String'Result'Range =>
            To_String'Result (J) =
              To_Character (Item (Item'First + (J - 1)), Substitute));
   --  Returns the String whose range is 1..Item'Length and each of whose
   --  elements is given by To_Character of the corresponding element in Item.

   function To_Wide_Character
     (Item : Character) return Wide_Character
   with
     Post => To_Wide_Character'Result =
       Wide_Character'Val (Character'Pos (Item));
   --  Returns the Wide_Character X such that Character'Pos(Item) =
   --  Wide_Character'Pos (X).

   function To_Wide_String
     (Item : String) return Wide_String
   with
     Post => To_Wide_String'Result'First = 1
       and then To_Wide_String'Result'Length = Item'Length
       and then
         (for all J in To_Wide_String'Result'Range =>
            To_Wide_String'Result (J) =
              To_Wide_Character (Item (Item'First + (J - 1))));
   --  Returns the Wide_String whose range is 1..Item'Length and each of whose
   --  elements is given by To_Wide_Character of the corresponding element in
   --  Item.

private
   pragma Inline (Is_Alphanumeric);
   pragma Inline (Is_Basic);
   pragma Inline (Is_Character);
   pragma Inline (Is_Control);
   pragma Inline (Is_Digit);
   pragma Inline (Is_Graphic);
   pragma Inline (Is_Hexadecimal_Digit);
   pragma Inline (Is_ISO_646);
   pragma Inline (Is_Letter);
   pragma Inline (Is_Line_Terminator);
   pragma Inline (Is_Lower);
   pragma Inline (Is_Mark);
   pragma Inline (Is_Other_Format);
   pragma Inline (Is_Punctuation_Connector);
   pragma Inline (Is_Space);
   pragma Inline (Is_Special);
   pragma Inline (Is_Upper);
   pragma Inline (To_Basic);
   pragma Inline (To_Character);
   pragma Inline (To_Lower);
   pragma Inline (To_Upper);
   pragma Inline (To_Wide_Character);

end Ada.Characters.Handling;
