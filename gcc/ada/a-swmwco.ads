------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
-- A D A . S T R I N G S . W I D E _ M A P S . W I D E _ C O N S T A N T S  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Wide_Latin_1;

package Ada.Strings.Wide_Maps.Wide_Constants is
pragma Preelaborate (Wide_Constants);

   Control_Set           : constant Wide_Maps.Wide_Character_Set;
   Graphic_Set           : constant Wide_Maps.Wide_Character_Set;
   Letter_Set            : constant Wide_Maps.Wide_Character_Set;
   Lower_Set             : constant Wide_Maps.Wide_Character_Set;
   Upper_Set             : constant Wide_Maps.Wide_Character_Set;
   Basic_Set             : constant Wide_Maps.Wide_Character_Set;
   Decimal_Digit_Set     : constant Wide_Maps.Wide_Character_Set;
   Hexadecimal_Digit_Set : constant Wide_Maps.Wide_Character_Set;
   Alphanumeric_Set      : constant Wide_Maps.Wide_Character_Set;
   Special_Graphic_Set   : constant Wide_Maps.Wide_Character_Set;
   ISO_646_Set           : constant Wide_Maps.Wide_Character_Set;
   Character_Set         : constant Wide_Maps.Wide_Character_Set;

   Lower_Case_Map        : constant Wide_Maps.Wide_Character_Mapping;
   --  Maps to lower case for letters, else identity

   Upper_Case_Map        : constant Wide_Maps.Wide_Character_Mapping;
   --  Maps to upper case for letters, else identity

   Basic_Map             : constant Wide_Maps.Wide_Character_Mapping;
   --  Maps to basic letter for letters, else identity

private
   package W renames Ada.Characters.Wide_Latin_1;

   subtype WC is Wide_Character;

   Control_Ranges           : aliased constant Wide_Character_Ranges :=
     ((W.NUL, W.US),
      (W.DEL, W.APC));

   Control_Set              : constant Wide_Character_Set :=
     (AF.Controlled with
      Control_Ranges'Unrestricted_Access);

   Graphic_Ranges           : aliased constant Wide_Character_Ranges :=
     ((W.Space,       W.Tilde),
      (WC'Val (256), WC'Last));

   Graphic_Set              : constant Wide_Character_Set :=
     (AF.Controlled with
      Graphic_Ranges'Unrestricted_Access);

   Letter_Ranges            : aliased constant Wide_Character_Ranges :=
     (('A',                   'Z'),
      (W.LC_A,                W.LC_Z),
      (W.UC_A_Grave,          W.UC_O_Diaeresis),
      (W.UC_O_Oblique_Stroke, W.LC_O_Diaeresis),
      (W.LC_O_Oblique_Stroke, W.LC_Y_Diaeresis));

   Letter_Set               : constant Wide_Character_Set :=
     (AF.Controlled with
      Letter_Ranges'Unrestricted_Access);

   Lower_Ranges             : aliased constant Wide_Character_Ranges :=
     (1 => (W.LC_A,                 W.LC_Z),
      2 => (W.LC_German_Sharp_S,   W.LC_O_Diaeresis),
      3 => (W.LC_O_Oblique_Stroke, W.LC_Y_Diaeresis));

   Lower_Set                : constant Wide_Character_Set :=
     (AF.Controlled with
      Lower_Ranges'Unrestricted_Access);

   Upper_Ranges             : aliased constant Wide_Character_Ranges :=
     (1 => ('A',                   'Z'),
      2 => (W.UC_A_Grave,          W.UC_O_Diaeresis),
      3 => (W.UC_O_Oblique_Stroke, W.UC_Icelandic_Thorn));

   Upper_Set                : constant Wide_Character_Set :=
     (AF.Controlled with
      Upper_Ranges'Unrestricted_Access);

   Basic_Ranges             : aliased constant Wide_Character_Ranges :=
     (1 => ('A',                   'Z'),
      2 => (W.LC_A,                W.LC_Z),
      3 => (W.UC_AE_Diphthong,     W.UC_AE_Diphthong),
      4 => (W.LC_AE_Diphthong,     W.LC_AE_Diphthong),
      5 => (W.LC_German_Sharp_S,   W.LC_German_Sharp_S),
      6 => (W.UC_Icelandic_Thorn,  W.UC_Icelandic_Thorn),
      7 => (W.LC_Icelandic_Thorn,  W.LC_Icelandic_Thorn),
      8 => (W.UC_Icelandic_Eth,    W.UC_Icelandic_Eth),
      9 => (W.LC_Icelandic_Eth,    W.LC_Icelandic_Eth));

   Basic_Set                : constant Wide_Character_Set :=
     (AF.Controlled with
      Basic_Ranges'Unrestricted_Access);

   Decimal_Digit_Ranges     : aliased constant Wide_Character_Ranges :=
     (1 => ('0', '9'));

   Decimal_Digit_Set        : constant Wide_Character_Set :=
     (AF.Controlled with
      Decimal_Digit_Ranges'Unrestricted_Access);

   Hexadecimal_Digit_Ranges : aliased constant Wide_Character_Ranges :=
     (1 => ('0', '9'),
      2 => ('A', 'F'),
      3 => (W.LC_A, W.LC_F));

   Hexadecimal_Digit_Set    : constant Wide_Character_Set :=
     (AF.Controlled with
      Hexadecimal_Digit_Ranges'Unrestricted_Access);

   Alphanumeric_Ranges      : aliased constant Wide_Character_Ranges :=
     (1 => ('0',                   '9'),
      2 => ('A',                   'Z'),
      3 => (W.LC_A,                W.LC_Z),
      4 => (W.UC_A_Grave,          W.UC_O_Diaeresis),
      5 => (W.UC_O_Oblique_Stroke, W.LC_O_Diaeresis),
      6 => (W.LC_O_Oblique_Stroke, W.LC_Y_Diaeresis));

   Alphanumeric_Set         : constant Wide_Character_Set :=
     (AF.Controlled with
      Alphanumeric_Ranges'Unrestricted_Access);

   Special_Graphic_Ranges   : aliased constant Wide_Character_Ranges :=
     (1 => (Wide_Space,            W.Solidus),
      2 => (W.Colon,               W.Commercial_At),
      3 => (W.Left_Square_Bracket, W.Grave),
      4 => (W.Left_Curly_Bracket,  W.Tilde),
      5 => (W.No_Break_Space,      W.Inverted_Question),
      6 => (W.Multiplication_Sign, W.Multiplication_Sign),
      7 => (W.Division_Sign,       W.Division_Sign));

   Special_Graphic_Set      : constant Wide_Character_Set :=
     (AF.Controlled with
      Special_Graphic_Ranges'Unrestricted_Access);

   ISO_646_Ranges           : aliased constant Wide_Character_Ranges :=
     (1 => (W.NUL, W.DEL));

   ISO_646_Set              : constant Wide_Character_Set :=
     (AF.Controlled with
      ISO_646_Ranges'Unrestricted_Access);

   Character_Ranges         : aliased constant Wide_Character_Ranges :=
     (1 => (W.NUL, WC'Val (255)));

   Character_Set            : constant Wide_Character_Set :=
     (AF.Controlled with
      Character_Ranges'Unrestricted_Access);


   Lower_Case_Mapping : aliased constant Wide_Character_Mapping_Values :=
     (Length => 56,

      Domain =>
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
         W.UC_A_Grave                &
         W.UC_A_Acute                &
         W.UC_A_Circumflex           &
         W.UC_A_Tilde                &
         W.UC_A_Diaeresis            &
         W.UC_A_Ring                 &
         W.UC_AE_Diphthong           &
         W.UC_C_Cedilla              &
         W.UC_E_Grave                &
         W.UC_E_Acute                &
         W.UC_E_Circumflex           &
         W.UC_E_Diaeresis            &
         W.UC_I_Grave                &
         W.UC_I_Acute                &
         W.UC_I_Circumflex           &
         W.UC_I_Diaeresis            &
         W.UC_Icelandic_Eth          &
         W.UC_N_Tilde                &
         W.UC_O_Grave                &
         W.UC_O_Acute                &
         W.UC_O_Circumflex           &
         W.UC_O_Tilde                &
         W.UC_O_Diaeresis            &
         W.UC_O_Oblique_Stroke       &
         W.UC_U_Grave                &
         W.UC_U_Acute                &
         W.UC_U_Circumflex           &
         W.UC_U_Diaeresis            &
         W.UC_Y_Acute                &
         W.UC_Icelandic_Thorn,

      Rangev =>
        "abcdefghijklmnopqrstuvwxyz" &
         W.LC_A_Grave                &
         W.LC_A_Acute                &
         W.LC_A_Circumflex           &
         W.LC_A_Tilde                &
         W.LC_A_Diaeresis            &
         W.LC_A_Ring                 &
         W.LC_AE_Diphthong           &
         W.LC_C_Cedilla              &
         W.LC_E_Grave                &
         W.LC_E_Acute                &
         W.LC_E_Circumflex           &
         W.LC_E_Diaeresis            &
         W.LC_I_Grave                &
         W.LC_I_Acute                &
         W.LC_I_Circumflex           &
         W.LC_I_Diaeresis            &
         W.LC_Icelandic_Eth          &
         W.LC_N_Tilde                &
         W.LC_O_Grave                &
         W.LC_O_Acute                &
         W.LC_O_Circumflex           &
         W.LC_O_Tilde                &
         W.LC_O_Diaeresis            &
         W.LC_O_Oblique_Stroke       &
         W.LC_U_Grave                &
         W.LC_U_Acute                &
         W.LC_U_Circumflex           &
         W.LC_U_Diaeresis            &
         W.LC_Y_Acute                &
         W.LC_Icelandic_Thorn);

   Lower_Case_Map : constant Wide_Character_Mapping :=
     (AF.Controlled with
      Map => Lower_Case_Mapping'Unrestricted_Access);

   Upper_Case_Mapping : aliased constant Wide_Character_Mapping_Values :=
     (Length => 56,

      Domain =>
        "abcdefghijklmnopqrstuvwxyz" &
         W.LC_A_Grave                &
         W.LC_A_Acute                &
         W.LC_A_Circumflex           &
         W.LC_A_Tilde                &
         W.LC_A_Diaeresis            &
         W.LC_A_Ring                 &
         W.LC_AE_Diphthong           &
         W.LC_C_Cedilla              &
         W.LC_E_Grave                &
         W.LC_E_Acute                &
         W.LC_E_Circumflex           &
         W.LC_E_Diaeresis            &
         W.LC_I_Grave                &
         W.LC_I_Acute                &
         W.LC_I_Circumflex           &
         W.LC_I_Diaeresis            &
         W.LC_Icelandic_Eth          &
         W.LC_N_Tilde                &
         W.LC_O_Grave                &
         W.LC_O_Acute                &
         W.LC_O_Circumflex           &
         W.LC_O_Tilde                &
         W.LC_O_Diaeresis            &
         W.LC_O_Oblique_Stroke       &
         W.LC_U_Grave                &
         W.LC_U_Acute                &
         W.LC_U_Circumflex           &
         W.LC_U_Diaeresis            &
         W.LC_Y_Acute                &
         W.LC_Icelandic_Thorn,

      Rangev =>
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
         W.UC_A_Grave                &
         W.UC_A_Acute                &
         W.UC_A_Circumflex           &
         W.UC_A_Tilde                &
         W.UC_A_Diaeresis            &
         W.UC_A_Ring                 &
         W.UC_AE_Diphthong           &
         W.UC_C_Cedilla              &
         W.UC_E_Grave                &
         W.UC_E_Acute                &
         W.UC_E_Circumflex           &
         W.UC_E_Diaeresis            &
         W.UC_I_Grave                &
         W.UC_I_Acute                &
         W.UC_I_Circumflex           &
         W.UC_I_Diaeresis            &
         W.UC_Icelandic_Eth          &
         W.UC_N_Tilde                &
         W.UC_O_Grave                &
         W.UC_O_Acute                &
         W.UC_O_Circumflex           &
         W.UC_O_Tilde                &
         W.UC_O_Diaeresis            &
         W.UC_O_Oblique_Stroke       &
         W.UC_U_Grave                &
         W.UC_U_Acute                &
         W.UC_U_Circumflex           &
         W.UC_U_Diaeresis            &
         W.UC_Y_Acute                &
         W.UC_Icelandic_Thorn);

   Upper_Case_Map : constant Wide_Character_Mapping :=
     (AF.Controlled with
      Upper_Case_Mapping'Unrestricted_Access);

   Basic_Mapping : aliased constant Wide_Character_Mapping_Values :=
     (Length => 55,

      Domain =>
        W.UC_A_Grave          &
        W.UC_A_Acute          &
        W.UC_A_Circumflex     &
        W.UC_A_Tilde          &
        W.UC_A_Diaeresis      &
        W.UC_A_Ring           &
        W.UC_C_Cedilla        &
        W.UC_E_Grave          &
        W.UC_E_Acute          &
        W.UC_E_Circumflex     &
        W.UC_E_Diaeresis      &
        W.UC_I_Grave          &
        W.UC_I_Acute          &
        W.UC_I_Circumflex     &
        W.UC_I_Diaeresis      &
        W.UC_N_Tilde          &
        W.UC_O_Grave          &
        W.UC_O_Acute          &
        W.UC_O_Circumflex     &
        W.UC_O_Tilde          &
        W.UC_O_Diaeresis      &
        W.UC_O_Oblique_Stroke &
        W.UC_U_Grave          &
        W.UC_U_Acute          &
        W.UC_U_Circumflex     &
        W.UC_U_Diaeresis      &
        W.UC_Y_Acute          &
        W.LC_A_Grave          &
        W.LC_A_Acute          &
        W.LC_A_Circumflex     &
        W.LC_A_Tilde          &
        W.LC_A_Diaeresis      &
        W.LC_A_Ring           &
        W.LC_C_Cedilla        &
        W.LC_E_Grave          &
        W.LC_E_Acute          &
        W.LC_E_Circumflex     &
        W.LC_E_Diaeresis      &
        W.LC_I_Grave          &
        W.LC_I_Acute          &
        W.LC_I_Circumflex     &
        W.LC_I_Diaeresis      &
        W.LC_N_Tilde          &
        W.LC_O_Grave          &
        W.LC_O_Acute          &
        W.LC_O_Circumflex     &
        W.LC_O_Tilde          &
        W.LC_O_Diaeresis      &
        W.LC_O_Oblique_Stroke &
        W.LC_U_Grave          &
        W.LC_U_Acute          &
        W.LC_U_Circumflex     &
        W.LC_U_Diaeresis      &
        W.LC_Y_Acute          &
        W.LC_Y_Diaeresis,

      Rangev =>
        'A'        &  -- UC_A_Grave
        'A'        &  -- UC_A_Acute
        'A'        &  -- UC_A_Circumflex
        'A'        &  -- UC_A_Tilde
        'A'        &  -- UC_A_Diaeresis
        'A'        &  -- UC_A_Ring
        'C'        &  -- UC_C_Cedilla
        'E'        &  -- UC_E_Grave
        'E'        &  -- UC_E_Acute
        'E'        &  -- UC_E_Circumflex
        'E'        &  -- UC_E_Diaeresis
        'I'        &  -- UC_I_Grave
        'I'        &  -- UC_I_Acute
        'I'        &  -- UC_I_Circumflex
        'I'        &  -- UC_I_Diaeresis
        'N'        &  -- UC_N_Tilde
        'O'        &  -- UC_O_Grave
        'O'        &  -- UC_O_Acute
        'O'        &  -- UC_O_Circumflex
        'O'        &  -- UC_O_Tilde
        'O'        &  -- UC_O_Diaeresis
        'O'        &  -- UC_O_Oblique_Stroke
        'U'        &  -- UC_U_Grave
        'U'        &  -- UC_U_Acute
        'U'        &  -- UC_U_Circumflex
        'U'        &  -- UC_U_Diaeresis
        'Y'        &  -- UC_Y_Acute
        'a'        &  -- LC_A_Grave
        'a'        &  -- LC_A_Acute
        'a'        &  -- LC_A_Circumflex
        'a'        &  -- LC_A_Tilde
        'a'        &  -- LC_A_Diaeresis
        'a'        &  -- LC_A_Ring
        'c'        &  -- LC_C_Cedilla
        'e'        &  -- LC_E_Grave
        'e'        &  -- LC_E_Acute
        'e'        &  -- LC_E_Circumflex
        'e'        &  -- LC_E_Diaeresis
        'i'        &  -- LC_I_Grave
        'i'        &  -- LC_I_Acute
        'i'        &  -- LC_I_Circumflex
        'i'        &  -- LC_I_Diaeresis
        'n'        &  -- LC_N_Tilde
        'o'        &  -- LC_O_Grave
        'o'        &  -- LC_O_Acute
        'o'        &  -- LC_O_Circumflex
        'o'        &  -- LC_O_Tilde
        'o'        &  -- LC_O_Diaeresis
        'o'        &  -- LC_O_Oblique_Stroke
        'u'        &  -- LC_U_Grave
        'u'        &  -- LC_U_Acute
        'u'        &  -- LC_U_Circumflex
        'u'        &  -- LC_U_Diaeresis
        'y'        &  -- LC_Y_Acute
        'y');         -- LC_Y_Diaeresis

   Basic_Map : constant Wide_Character_Mapping :=
     (AF.Controlled with
      Basic_Mapping'Unrestricted_Access);

end Ada.Strings.Wide_Maps.Wide_Constants;
