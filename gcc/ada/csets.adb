------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C S E T S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
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

with Opt;   use Opt;

with System.WCh_Con; use System.WCh_Con;

package body Csets is

   X_80 : constant Character := Character'Val (16#80#);
   X_81 : constant Character := Character'Val (16#81#);
   X_82 : constant Character := Character'Val (16#82#);
   X_83 : constant Character := Character'Val (16#83#);
   X_84 : constant Character := Character'Val (16#84#);
   X_85 : constant Character := Character'Val (16#85#);
   X_86 : constant Character := Character'Val (16#86#);
   X_87 : constant Character := Character'Val (16#87#);
   X_88 : constant Character := Character'Val (16#88#);
   X_89 : constant Character := Character'Val (16#89#);
   X_8A : constant Character := Character'Val (16#8A#);
   X_8B : constant Character := Character'Val (16#8B#);
   X_8C : constant Character := Character'Val (16#8C#);
   X_8D : constant Character := Character'Val (16#8D#);
   X_8E : constant Character := Character'Val (16#8E#);
   X_8F : constant Character := Character'Val (16#8F#);
   X_90 : constant Character := Character'Val (16#90#);
   X_91 : constant Character := Character'Val (16#91#);
   X_92 : constant Character := Character'Val (16#92#);
   X_93 : constant Character := Character'Val (16#93#);
   X_94 : constant Character := Character'Val (16#94#);
   X_95 : constant Character := Character'Val (16#95#);
   X_96 : constant Character := Character'Val (16#96#);
   X_97 : constant Character := Character'Val (16#97#);
   X_98 : constant Character := Character'Val (16#98#);
   X_99 : constant Character := Character'Val (16#99#);
   X_9A : constant Character := Character'Val (16#9A#);
   X_9B : constant Character := Character'Val (16#9B#);
   X_9C : constant Character := Character'Val (16#9C#);
   X_9D : constant Character := Character'Val (16#9D#);
   X_9E : constant Character := Character'Val (16#9E#);
   X_9F : constant Character := Character'Val (16#9F#);
   X_A0 : constant Character := Character'Val (16#A0#);
   X_A1 : constant Character := Character'Val (16#A1#);
   X_A2 : constant Character := Character'Val (16#A2#);
   X_A3 : constant Character := Character'Val (16#A3#);
   X_A4 : constant Character := Character'Val (16#A4#);
   X_A5 : constant Character := Character'Val (16#A5#);
   X_A6 : constant Character := Character'Val (16#A6#);
   X_A7 : constant Character := Character'Val (16#A7#);
   X_A8 : constant Character := Character'Val (16#A8#);
   X_A9 : constant Character := Character'Val (16#A9#);
   X_AA : constant Character := Character'Val (16#AA#);
   X_AB : constant Character := Character'Val (16#AB#);
   X_AC : constant Character := Character'Val (16#AC#);
   X_AD : constant Character := Character'Val (16#AD#);
   X_AE : constant Character := Character'Val (16#AE#);
   X_AF : constant Character := Character'Val (16#AF#);
   X_B0 : constant Character := Character'Val (16#B0#);
   X_B1 : constant Character := Character'Val (16#B1#);
   X_B2 : constant Character := Character'Val (16#B2#);
   X_B3 : constant Character := Character'Val (16#B3#);
   X_B4 : constant Character := Character'Val (16#B4#);
   X_B5 : constant Character := Character'Val (16#B5#);
   X_B6 : constant Character := Character'Val (16#B6#);
   X_B7 : constant Character := Character'Val (16#B7#);
   X_B8 : constant Character := Character'Val (16#B8#);
   X_B9 : constant Character := Character'Val (16#B9#);
   X_BA : constant Character := Character'Val (16#BA#);
   X_BB : constant Character := Character'Val (16#BB#);
   X_BC : constant Character := Character'Val (16#BC#);
   X_BD : constant Character := Character'Val (16#BD#);
   X_BE : constant Character := Character'Val (16#BE#);
   X_BF : constant Character := Character'Val (16#BF#);
   X_C0 : constant Character := Character'Val (16#C0#);
   X_C1 : constant Character := Character'Val (16#C1#);
   X_C2 : constant Character := Character'Val (16#C2#);
   X_C3 : constant Character := Character'Val (16#C3#);
   X_C4 : constant Character := Character'Val (16#C4#);
   X_C5 : constant Character := Character'Val (16#C5#);
   X_C6 : constant Character := Character'Val (16#C6#);
   X_C7 : constant Character := Character'Val (16#C7#);
   X_C8 : constant Character := Character'Val (16#C8#);
   X_C9 : constant Character := Character'Val (16#C9#);
   X_CA : constant Character := Character'Val (16#CA#);
   X_CB : constant Character := Character'Val (16#CB#);
   X_CC : constant Character := Character'Val (16#CC#);
   X_CD : constant Character := Character'Val (16#CD#);
   X_CE : constant Character := Character'Val (16#CE#);
   X_CF : constant Character := Character'Val (16#CF#);
   X_D0 : constant Character := Character'Val (16#D0#);
   X_D1 : constant Character := Character'Val (16#D1#);
   X_D2 : constant Character := Character'Val (16#D2#);
   X_D3 : constant Character := Character'Val (16#D3#);
   X_D4 : constant Character := Character'Val (16#D4#);
   X_D5 : constant Character := Character'Val (16#D5#);
   X_D6 : constant Character := Character'Val (16#D6#);
   X_D7 : constant Character := Character'Val (16#D7#);
   X_D8 : constant Character := Character'Val (16#D8#);
   X_D9 : constant Character := Character'Val (16#D9#);
   X_DA : constant Character := Character'Val (16#DA#);
   X_DB : constant Character := Character'Val (16#DB#);
   X_DC : constant Character := Character'Val (16#DC#);
   X_DD : constant Character := Character'Val (16#DD#);
   X_DE : constant Character := Character'Val (16#DE#);
   X_DF : constant Character := Character'Val (16#DF#);
   X_E0 : constant Character := Character'Val (16#E0#);
   X_E1 : constant Character := Character'Val (16#E1#);
   X_E2 : constant Character := Character'Val (16#E2#);
   X_E3 : constant Character := Character'Val (16#E3#);
   X_E4 : constant Character := Character'Val (16#E4#);
   X_E5 : constant Character := Character'Val (16#E5#);
   X_E6 : constant Character := Character'Val (16#E6#);
   X_E7 : constant Character := Character'Val (16#E7#);
   X_E8 : constant Character := Character'Val (16#E8#);
   X_E9 : constant Character := Character'Val (16#E9#);
   X_EA : constant Character := Character'Val (16#EA#);
   X_EB : constant Character := Character'Val (16#EB#);
   X_EC : constant Character := Character'Val (16#EC#);
   X_ED : constant Character := Character'Val (16#ED#);
   X_EE : constant Character := Character'Val (16#EE#);
   X_EF : constant Character := Character'Val (16#EF#);
   X_F0 : constant Character := Character'Val (16#F0#);
   X_F1 : constant Character := Character'Val (16#F1#);
   X_F2 : constant Character := Character'Val (16#F2#);
   X_F3 : constant Character := Character'Val (16#F3#);
   X_F4 : constant Character := Character'Val (16#F4#);
   X_F5 : constant Character := Character'Val (16#F5#);
   X_F6 : constant Character := Character'Val (16#F6#);
   X_F7 : constant Character := Character'Val (16#F7#);
   X_F8 : constant Character := Character'Val (16#F8#);
   X_F9 : constant Character := Character'Val (16#F9#);
   X_FA : constant Character := Character'Val (16#FA#);
   X_FB : constant Character := Character'Val (16#FB#);
   X_FC : constant Character := Character'Val (16#FC#);
   X_FD : constant Character := Character'Val (16#FD#);
   X_FE : constant Character := Character'Val (16#FE#);
   X_FF : constant Character := Character'Val (16#FF#);

   ------------------------------------------
   -- Definitions for Latin-1 (ISO 8859-1) --
   ------------------------------------------

   Fold_Latin_1 : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,
      'p' => 'P',  X_EF => X_CF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,
      'P' => 'P',  X_CF => X_CF,  X_DF => X_DF,  X_FF => X_FF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ------------------------------------------
   -- Definitions for Latin-2 (ISO 8859-2) --
   ------------------------------------------

   Fold_Latin_2 : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,  X_B1 => X_A1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,  X_B3 => X_A3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,  X_B5 => X_A5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,  X_B6 => X_A6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,  X_B9 => X_A9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,  X_BA => X_AA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,  X_BB => X_AB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,  X_BC => X_AC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,  X_BE => X_AE,
      'p' => 'P',  X_EF => X_CF,  X_FF => X_DF,  X_BF => X_AF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,  X_A1 => X_A1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,  X_A3 => X_A3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,  X_A5 => X_A5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,  X_A6 => X_A6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,  X_A9 => X_A9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,  X_AA => X_AA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,  X_AB => X_AB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,  X_AC => X_AC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,  X_AE => X_AE,
      'P' => 'P',  X_CF => X_CF,  X_DF => X_DF,  X_AF => X_AF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ------------------------------------------
   -- Definitions for Latin-3 (ISO 8859-3) --
   ------------------------------------------

   Fold_Latin_3 : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,  X_B1 => X_A1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',                 X_F3 => X_D3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,  X_B5 => X_A5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,  X_B6 => X_A6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,  X_B9 => X_A9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,  X_BA => X_AA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,  X_BB => X_AB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,  X_BC => X_AC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,
      'p' => 'P',  X_EF => X_CF,                 X_BF => X_AF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,  X_A1 => X_A1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',                 X_D3 => X_D3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,  X_A5 => X_A5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,  X_A6 => X_A6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,  X_A9 => X_A9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,  X_AA => X_AA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,  X_AB => X_AB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,  X_AC => X_AC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,
      'P' => 'P',  X_CF => X_CF,                 X_AF => X_AF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ------------------------------------------
   -- Definitions for Latin-4 (ISO 8859-4) --
   ------------------------------------------

   Fold_Latin_4 : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,  X_B1 => X_A1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,  X_B3 => X_A3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,  X_B5 => X_A5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,  X_B6 => X_A6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,  X_B9 => X_A9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,  X_BA => X_AA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,  X_BB => X_AB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,  X_BC => X_AC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,  X_BE => X_AE,
      'p' => 'P',  X_EF => X_CF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,  X_A1 => X_A1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,  X_A3 => X_A3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,  X_A5 => X_A5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,  X_A6 => X_A6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,  X_A9 => X_A9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,  X_AA => X_AA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,  X_AB => X_AB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,  X_AC => X_AC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,  X_AE => X_AE,
      'P' => 'P',  X_CF => X_CF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ---------------------------------------------------
   -- Definitions for Latin-5 (Cyrillic ISO-8859-5) --
   ---------------------------------------------------

   Fold_Latin_5 : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_D0 => X_B0,  X_E0 => X_C0,
      'b' => 'B',  X_D1 => X_B1,  X_E1 => X_C1,  X_F1 => X_A1,
      'c' => 'C',  X_D2 => X_B2,  X_E2 => X_C2,  X_F2 => X_A2,
      'd' => 'D',  X_D3 => X_B3,  X_E3 => X_C3,  X_F3 => X_A3,
      'e' => 'E',  X_D4 => X_B4,  X_E4 => X_C4,  X_F4 => X_A4,
      'f' => 'F',  X_D5 => X_B5,  X_E5 => X_C5,  X_F5 => X_A5,
      'g' => 'G',  X_D6 => X_B6,  X_E6 => X_C6,  X_F6 => X_A6,
      'h' => 'H',  X_D7 => X_B7,  X_E7 => X_C7,  X_F7 => X_A7,
      'i' => 'I',  X_D8 => X_B8,  X_E8 => X_C8,  X_F8 => X_A8,
      'j' => 'J',  X_D9 => X_B9,  X_E9 => X_C9,  X_F9 => X_A9,
      'k' => 'K',  X_DA => X_BA,  X_EA => X_CA,  X_FA => X_AA,
      'l' => 'L',  X_DB => X_BB,  X_EB => X_CB,  X_FB => X_AB,
      'm' => 'M',  X_DC => X_BC,  X_EC => X_CC,  X_FC => X_AC,
      'n' => 'N',  X_DD => X_BD,  X_ED => X_CD,
      'o' => 'O',  X_DE => X_BE,  X_EE => X_CE,  X_FE => X_AE,
      'p' => 'P',  X_DF => X_BF,  X_EF => X_CF,  X_FF => X_AF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_B0 => X_B0,  X_C0 => X_C0,
      'B' => 'B',  X_B1 => X_B1,  X_C1 => X_C1,  X_A1 => X_A1,
      'C' => 'C',  X_B2 => X_B2,  X_C2 => X_C2,  X_A2 => X_A2,
      'D' => 'D',  X_B3 => X_B3,  X_C3 => X_C3,  X_A3 => X_A3,
      'E' => 'E',  X_B4 => X_B4,  X_C4 => X_C4,  X_A4 => X_A4,
      'F' => 'F',  X_B5 => X_B5,  X_C5 => X_C5,  X_A5 => X_A5,
      'G' => 'G',  X_B6 => X_B6,  X_C6 => X_C6,  X_A6 => X_A6,
      'H' => 'H',  X_B7 => X_B7,  X_C7 => X_C7,  X_A7 => X_A7,
      'I' => 'I',  X_B8 => X_B8,  X_C8 => X_C8,  X_A8 => X_A8,
      'J' => 'J',  X_B9 => X_B9,  X_C9 => X_C9,  X_A9 => X_A9,
      'K' => 'K',  X_BA => X_BA,  X_CA => X_CA,  X_AA => X_AA,
      'L' => 'L',  X_BB => X_BB,  X_CB => X_CB,  X_AB => X_AB,
      'M' => 'M',  X_BC => X_BC,  X_CC => X_CC,  X_AC => X_AC,
      'N' => 'N',  X_BD => X_BD,  X_CD => X_CD,
      'O' => 'O',  X_BE => X_BE,  X_CE => X_CE,  X_AE => X_AE,
      'P' => 'P',  X_BF => X_BF,  X_CF => X_CF,  X_AF => X_AF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ------------------------------------------
   -- Definitions for Latin-9 (ISO 8859-9) --
   ------------------------------------------

   Fold_Latin_9 : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,
      'p' => 'P',  X_EF => X_CF,
      'q' => 'Q',  X_A8 => X_A6,
      'r' => 'R',  X_B8 => X_B4,
      's' => 'S',  X_BD => X_BC,
      't' => 'T',  X_BE => X_FF,
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,
      'P' => 'P',  X_CF => X_CF,  X_DF => X_DF,  X_FF => X_FF,
      'Q' => 'Q',  X_A6 => X_A6,
      'R' => 'R',  X_B4 => X_B4,
      'S' => 'S',  X_BC => X_BC,
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   --------------------------------------------
   -- Definitions for IBM PC (Code Page 437) --
   --------------------------------------------

   --  Note: Code page 437 is the typical default in DOS, Windows and OS/2
   --  for PC's in the US, it corresponds to the original PC character set.
   --  See also the definitions for code page 850.

   Fold_IBM_PC_437 : constant Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      X_80 => X_80,  -- C cedilla
      X_81 => X_9A,  -- u umlaut
      X_82 => X_90,  -- e acute
      X_83 => X_83,  -- a circumflex
      X_84 => X_8E,  -- a umlaut
      X_85 => X_85,  -- a grave
      X_86 => X_8F,  -- a ring
      X_87 => X_80,  -- c cedilla
      X_88 => X_88,  -- e circumflex
      X_89 => X_89,  -- e umlaut
      X_8A => X_8A,  -- e grave
      X_8B => X_8B,  -- i umlaut
      X_8C => X_8C,  -- i circumflex
      X_8D => X_8D,  -- i grave
      X_8E => X_8E,  -- A umlaut
      X_8F => X_8F,  -- A ring

      X_90 => X_90,  -- E acute
      X_91 => X_92,  -- ae
      X_92 => X_92,  -- AE
      X_93 => X_93,  -- o circumflex
      X_94 => X_99,  -- o umlaut
      X_95 => X_95,  -- o grave
      X_96 => X_96,  -- u circumflex
      X_97 => X_97,  -- u grave
      X_98 => X_98,  -- y umlaut
      X_99 => X_99,  -- O umlaut
      X_9A => X_9A,  -- U umlaut

      X_A0 => X_A0,  -- a acute
      X_A1 => X_A1,  -- i acute
      X_A2 => X_A2,  -- o acute
      X_A3 => X_A3,  -- u acute
      X_A4 => X_A5,  -- n tilde
      X_A5 => X_A5,  -- N tilde
      X_A6 => X_A6,  -- a underline
      X_A7 => X_A7,  -- o underline

      X_E0 => X_E0,  -- lower case alpha
      X_E1 => X_E1,  -- lower case beta
      X_E2 => X_E2,  -- upper case gamma
      X_E3 => X_E3,  -- lower case pi
      X_E4 => X_E4,  -- upper case sigma (lower/upper sigma not equivalent)
      X_E5 => X_E5,  -- lower case sigma (lower/upper sigma not equivalent)
      X_E6 => X_E6,  -- lower case mu
      X_E7 => X_E7,  -- lower case tau
      X_E8 => X_E8,  -- upper case phi   (lower/upper phi not equivalent)
      X_E9 => X_E9,  -- lower case theta
      X_EA => X_EA,  -- upper case omega
      X_EB => X_EB,  -- lower case delta
      X_ED => X_ED,  -- lower case phi   (lower/upper phi not equivalent)
      X_EE => X_EE,  -- lower case epsilon

      X_FC => X_FC,  -- lower case eta

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   --------------------------------------------
   -- Definitions for IBM PC (Code Page 850) --
   --------------------------------------------

   --  Note: Code page 850 is the typical default in DOS, Windows and OS/2
   --  for PC's in Europe, it is an extension of the original PC character
   --  set to include the additional characters defined in ISO Latin-1.
   --  See also the definitions for code page 437.

   Fold_IBM_PC_850 : constant Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      X_80 => X_80,  -- C cedilla
      X_81 => X_9A,  -- u umlaut
      X_82 => X_90,  -- e acute
      X_83 => X_B6,  -- a circumflex
      X_84 => X_8E,  -- a umlaut
      X_85 => X_B7,  -- a grave
      X_86 => X_8F,  -- a ring
      X_87 => X_80,  -- c cedilla
      X_88 => X_D2,  -- e circumflex
      X_89 => X_D3,  -- e umlaut
      X_8A => X_D4,  -- e grave
      X_8B => X_D8,  -- i umlaut
      X_8C => X_D7,  -- i circumflex
      X_8D => X_DE,  -- i grave
      X_8E => X_8E,  -- A umlaut
      X_8F => X_8F,  -- A ring

      X_90 => X_90,  -- E acute
      X_91 => X_92,  -- ae
      X_92 => X_92,  -- AE
      X_93 => X_E2,  -- o circumflex
      X_94 => X_99,  -- o umlaut
      X_95 => X_E3,  -- o grave
      X_96 => X_EA,  -- u circumflex
      X_97 => X_EB,  -- u grave
      X_98 => X_98,  -- y umlaut
      X_99 => X_99,  -- O umlaut
      X_9A => X_9A,  -- U umlaut

      X_A0 => X_B5,  -- a acute
      X_A1 => X_D6,  -- i acute
      X_A2 => X_E0,  -- o acute
      X_A3 => X_E9,  -- u acute
      X_A4 => X_A5,  -- n tilde
      X_A5 => X_A5,  -- N tilde
      X_A6 => X_A6,  -- a underline
      X_A7 => X_A7,  -- o underline

      X_B5 => X_B5,  -- A acute
      X_B6 => X_B6,  -- A circumflex
      X_B7 => X_B7,  -- A grave

      X_C6 => X_C7,  -- a tilde
      X_C7 => X_C7,  -- A tilde

      X_D0 => X_D1,  -- eth
      X_D1 => X_D1,  -- Eth
      X_D2 => X_D2,  -- E circumflex
      X_D3 => X_D3,  -- E umlaut
      X_D4 => X_D4,  -- E grave
      X_D5 => X_D5,  -- dotless i, no uppercase
      X_D6 => X_D6,  -- I acute
      X_D7 => X_D7,  -- I circumflex
      X_D8 => X_D8,  -- I umlaut
      X_DE => X_DE,  -- I grave

      X_E0 => X_E0,  -- O acute
      X_E1 => X_E1,  -- german dbl s, no uppercase
      X_E2 => X_E2,  -- O circumflex
      X_E3 => X_E3,  -- O grave
      X_E4 => X_E4,  -- o tilde
      X_E5 => X_E5,  -- O tilde
      X_E7 => X_E8,  -- thorn
      X_E8 => X_E8,  -- Thorn
      X_E9 => X_E9,  -- U acute
      X_EA => X_EA,  -- U circumflex
      X_EB => X_EB,  -- U grave
      X_EC => X_ED,  -- y acute
      X_ED => X_ED,  -- Y acute

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   -----------------------------------------
   -- Definitions for Full Upper Half Set --
   -----------------------------------------

   --  The full upper half set allows all upper half characters as letters,
   --  and does not recognize any upper/lower case equivalences in this half.

   Fold_Full_Upper_Half : constant Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      X_80 => X_80,  X_90 => X_90,  X_A0 => X_A0,  X_B0 => X_B0,
      X_81 => X_81,  X_91 => X_91,  X_A1 => X_A1,  X_B1 => X_B1,
      X_82 => X_82,  X_92 => X_92,  X_A2 => X_A2,  X_B2 => X_B2,
      X_83 => X_83,  X_93 => X_93,  X_A3 => X_A3,  X_B3 => X_B3,
      X_84 => X_84,  X_94 => X_94,  X_A4 => X_A4,  X_B4 => X_B4,
      X_85 => X_85,  X_95 => X_95,  X_A5 => X_A5,  X_B5 => X_B5,
      X_86 => X_86,  X_96 => X_96,  X_A6 => X_A6,  X_B6 => X_B6,
      X_87 => X_87,  X_97 => X_97,  X_A7 => X_A7,  X_B7 => X_B7,
      X_88 => X_88,  X_98 => X_98,  X_A8 => X_A8,  X_B8 => X_B8,
      X_89 => X_89,  X_99 => X_99,  X_A9 => X_A9,  X_B9 => X_B9,
      X_8A => X_8A,  X_9A => X_9A,  X_AA => X_AA,  X_BA => X_BA,
      X_8B => X_8B,  X_9B => X_9B,  X_AB => X_AB,  X_BB => X_BB,
      X_8C => X_8C,  X_9C => X_9C,  X_AC => X_AC,  X_BC => X_BC,
      X_8D => X_8D,  X_9D => X_9D,  X_AD => X_AD,  X_BD => X_BD,
      X_8E => X_8E,  X_9E => X_9E,  X_AE => X_AE,  X_BE => X_BE,
      X_8F => X_8F,  X_9F => X_9F,  X_AF => X_AF,  X_BF => X_BF,

      X_C0 => X_C0,  X_D0 => X_D0,  X_E0 => X_E0,  X_F0 => X_F0,
      X_C1 => X_C1,  X_D1 => X_D1,  X_E1 => X_E1,  X_F1 => X_F1,
      X_C2 => X_C2,  X_D2 => X_D2,  X_E2 => X_E2,  X_F2 => X_F2,
      X_C3 => X_C3,  X_D3 => X_D3,  X_E3 => X_E3,  X_F3 => X_F3,
      X_C4 => X_C4,  X_D4 => X_D4,  X_E4 => X_E4,  X_F4 => X_F4,
      X_C5 => X_C5,  X_D5 => X_D5,  X_E5 => X_E5,  X_F5 => X_F5,
      X_C6 => X_C6,  X_D6 => X_D6,  X_E6 => X_E6,  X_F6 => X_F6,
      X_C7 => X_C7,  X_D7 => X_D7,  X_E7 => X_E7,  X_F7 => X_F7,
      X_C8 => X_C8,  X_D8 => X_D8,  X_E8 => X_E8,  X_F8 => X_F8,
      X_C9 => X_C9,  X_D9 => X_D9,  X_E9 => X_E9,  X_F9 => X_F9,
      X_CA => X_CA,  X_DA => X_DA,  X_EA => X_EA,  X_FA => X_FA,
      X_CB => X_CB,  X_DB => X_DB,  X_EB => X_EB,  X_FB => X_FB,
      X_CC => X_CC,  X_DC => X_DC,  X_EC => X_EC,  X_FC => X_FC,
      X_CD => X_CD,  X_DD => X_DD,  X_ED => X_ED,  X_FD => X_FD,
      X_CE => X_CE,  X_DE => X_DE,  X_EE => X_EE,  X_FE => X_FE,
      X_CF => X_CF,  X_DF => X_DF,  X_EF => X_EF,  X_FF => X_FF,

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ---------------------------------------
   -- Definitions for No Upper Half Set --
   ---------------------------------------

   --  The no upper half set allows no upper half characters as letters, and
   --  thus there are no upper/lower case equivalences in this half. This set
   --  corresponds to the Ada 83 rules.

   Fold_No_Upper_Half : constant Translate_Table := Translate_Table'(

      'a' => 'A',
      'b' => 'B',
      'c' => 'C',
      'd' => 'D',
      'e' => 'E',
      'f' => 'F',
      'g' => 'G',
      'h' => 'H',
      'i' => 'I',
      'j' => 'J',
      'k' => 'K',
      'l' => 'L',
      'm' => 'M',
      'n' => 'N',
      'o' => 'O',
      'p' => 'P',
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',
      'B' => 'B',
      'C' => 'C',
      'D' => 'D',
      'E' => 'E',
      'F' => 'F',
      'G' => 'G',
      'H' => 'H',
      'I' => 'I',
      'J' => 'J',
      'K' => 'K',
      'L' => 'L',
      'M' => 'M',
      'N' => 'N',
      'O' => 'O',
      'P' => 'P',
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Set Fold_Upper table from source code indication

      if Identifier_Character_Set = '1'
        or else Identifier_Character_Set = 'w'
      then
         Fold_Upper := Fold_Latin_1;

      elsif Identifier_Character_Set = '2' then
         Fold_Upper := Fold_Latin_2;

      elsif Identifier_Character_Set = '3' then
         Fold_Upper := Fold_Latin_3;

      elsif Identifier_Character_Set = '4' then
         Fold_Upper := Fold_Latin_4;

      elsif Identifier_Character_Set = '5' then
         Fold_Upper := Fold_Latin_5;

      elsif Identifier_Character_Set = 'p' then
         Fold_Upper := Fold_IBM_PC_437;

      elsif Identifier_Character_Set = '8' then
         Fold_Upper := Fold_IBM_PC_850;

      elsif Identifier_Character_Set = '9' then
         Fold_Upper := Fold_Latin_9;

      elsif Identifier_Character_Set = 'f' then
         Fold_Upper := Fold_Full_Upper_Half;

      else -- Identifier_Character_Set = 'n'
         Fold_Upper := Fold_No_Upper_Half;
      end if;

      --  Use Fold_Upper table to compute Fold_Lower table

      Fold_Lower := Fold_Upper;

      for J in Character loop
         if J /= Fold_Upper (J) then
            Fold_Lower (Fold_Upper (J)) := J;
            Fold_Lower (J) := J;
         end if;
      end loop;

      Fold_Lower (' ') := ' ';

      --  Build Identifier_Char table from used entries of Fold_Upper

      for J in Character loop
         Identifier_Char (J) := (Fold_Upper (J) /= ' ');
      end loop;

      --  Always add [ as an identifier character to deal with the brackets
      --  notation for wide characters used in identifiers. Note that if
      --  we are not allowing wide characters in identifiers, then any use
      --  of this notation will be flagged as an error in Scan_Identifier.

      Identifier_Char ('[') := True;

      --  Add entry for ESC if wide characters in use with a wide character
      --  encoding method active that uses the ESC code for encoding. Also
      --  add entry for left bracket to capture use of brackets notation.

      if Identifier_Character_Set = 'w'
        and then Wide_Character_Encoding_Method in WC_ESC_Encoding_Method
      then
         Identifier_Char (ASCII.ESC) := True;
      end if;
   end Initialize;

   --------------------------
   -- Is_Lower_Case_Letter --
   --------------------------

   function Is_Lower_Case_Letter (C : Character) return Boolean is
   begin
      return C /= Fold_Upper (C);
   end Is_Lower_Case_Letter;

   --------------------------
   -- Is_Upper_Case_Letter --
   --------------------------

   function Is_Upper_Case_Letter (C : Character) return Boolean is
   begin
      return C /= Fold_Lower (C);
   end Is_Upper_Case_Letter;

end Csets;
