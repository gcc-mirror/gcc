------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . S T R I N G S . M A P S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2014, Free Software Foundation, Inc.         --
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

with Ada.Characters.Latin_1;

package Ada.Strings.Maps is
   pragma Pure;
   --  In accordance with Ada 2005 AI-362

   --------------------------------
   -- Character Set Declarations --
   --------------------------------

   type Character_Set is private;
   pragma Preelaborable_Initialization (Character_Set);
   --  Representation for a set of character values:

   Null_Set : constant Character_Set;

   ---------------------------
   -- Constructors for Sets --
   ---------------------------

   type Character_Range is record
      Low  : Character;
      High : Character;
   end record;
   --  Represents Character range Low .. High

   type Character_Ranges is array (Positive range <>) of Character_Range;

   function To_Set    (Ranges : Character_Ranges) return Character_Set;

   function To_Set    (Span   : Character_Range)  return Character_Set;

   function To_Ranges (Set    : Character_Set)    return Character_Ranges;

   ----------------------------------
   -- Operations on Character Sets --
   ----------------------------------

   function "="   (Left, Right : Character_Set) return Boolean;

   function "not" (Right       : Character_Set) return Character_Set;
   function "and" (Left, Right : Character_Set) return Character_Set;
   function "or"  (Left, Right : Character_Set) return Character_Set;
   function "xor" (Left, Right : Character_Set) return Character_Set;
   function "-"   (Left, Right : Character_Set) return Character_Set;

   function Is_In
     (Element : Character;
      Set     : Character_Set) return Boolean;

   function Is_Subset
     (Elements : Character_Set;
      Set      : Character_Set) return     Boolean;

   function "<="
     (Left  : Character_Set;
      Right : Character_Set) return  Boolean
   renames Is_Subset;

   subtype Character_Sequence is String;
   --  Alternative representation for a set of character values

   function To_Set (Sequence  : Character_Sequence) return Character_Set;
   function To_Set (Singleton : Character)          return Character_Set;

   function To_Sequence (Set : Character_Set) return Character_Sequence;

   ------------------------------------
   -- Character Mapping Declarations --
   ------------------------------------

   type Character_Mapping is private;
   pragma Preelaborable_Initialization (Character_Mapping);
   --  Representation for a character to character mapping:

   function Value
     (Map     : Character_Mapping;
      Element : Character) return Character;

   Identity : constant Character_Mapping;

   ----------------------------
   -- Operations on Mappings --
   ----------------------------

   function To_Mapping
     (From, To : Character_Sequence) return Character_Mapping;

   function To_Domain
     (Map : Character_Mapping) return Character_Sequence;

   function To_Range
     (Map : Character_Mapping) return Character_Sequence;

   type Character_Mapping_Function is
      access function (From : Character) return Character;

private
   pragma Inline (Is_In);
   pragma Inline (Value);

   type Character_Set_Internal is array (Character) of Boolean;
   pragma Pack (Character_Set_Internal);

   type Character_Set is new Character_Set_Internal;
   --  Note: the reason for this level of derivation is to make sure
   --  that the predefined logical operations on this type remain
   --  accessible. The operations on Character_Set are overridden by
   --  the defined operations in the spec, but the operations defined
   --  on Character_Set_Internal remain visible.

   Null_Set : constant Character_Set := (others => False);

   type Character_Mapping is array (Character) of Character;

   package L renames Ada.Characters.Latin_1;

   Identity : constant Character_Mapping :=
     (L.NUL                         &  -- NUL                             0
      L.SOH                         &  -- SOH                             1
      L.STX                         &  -- STX                             2
      L.ETX                         &  -- ETX                             3
      L.EOT                         &  -- EOT                             4
      L.ENQ                         &  -- ENQ                             5
      L.ACK                         &  -- ACK                             6
      L.BEL                         &  -- BEL                             7
      L.BS                          &  -- BS                              8
      L.HT                          &  -- HT                              9
      L.LF                          &  -- LF                             10
      L.VT                          &  -- VT                             11
      L.FF                          &  -- FF                             12
      L.CR                          &  -- CR                             13
      L.SO                          &  -- SO                             14
      L.SI                          &  -- SI                             15
      L.DLE                         &  -- DLE                            16
      L.DC1                         &  -- DC1                            17
      L.DC2                         &  -- DC2                            18
      L.DC3                         &  -- DC3                            19
      L.DC4                         &  -- DC4                            20
      L.NAK                         &  -- NAK                            21
      L.SYN                         &  -- SYN                            22
      L.ETB                         &  -- ETB                            23
      L.CAN                         &  -- CAN                            24
      L.EM                          &  -- EM                             25
      L.SUB                         &  -- SUB                            26
      L.ESC                         &  -- ESC                            27
      L.FS                          &  -- FS                             28
      L.GS                          &  -- GS                             29
      L.RS                          &  -- RS                             30
      L.US                          &  -- US                             31
      L.Space                       &  -- ' '                            32
      L.Exclamation                 &  -- '!'                            33
      L.Quotation                   &  -- '"'                            34
      L.Number_Sign                 &  -- '#'                            35
      L.Dollar_Sign                 &  -- '$'                            36
      L.Percent_Sign                &  -- '%'                            37
      L.Ampersand                   &  -- '&'                            38
      L.Apostrophe                  &  -- '''                            39
      L.Left_Parenthesis            &  -- '('                            40
      L.Right_Parenthesis           &  -- ')'                            41
      L.Asterisk                    &  -- '*'                            42
      L.Plus_Sign                   &  -- '+'                            43
      L.Comma                       &  -- ','                            44
      L.Hyphen                      &  -- '-'                            45
      L.Full_Stop                   &  -- '.'                            46
      L.Solidus                     &  -- '/'                            47
      '0'                           &  -- '0'                            48
      '1'                           &  -- '1'                            49
      '2'                           &  -- '2'                            50
      '3'                           &  -- '3'                            51
      '4'                           &  -- '4'                            52
      '5'                           &  -- '5'                            53
      '6'                           &  -- '6'                            54
      '7'                           &  -- '7'                            55
      '8'                           &  -- '8'                            56
      '9'                           &  -- '9'                            57
      L.Colon                       &  -- ':'                            58
      L.Semicolon                   &  -- ';'                            59
      L.Less_Than_Sign              &  -- '<'                            60
      L.Equals_Sign                 &  -- '='                            61
      L.Greater_Than_Sign           &  -- '>'                            62
      L.Question                    &  -- '?'                            63
      L.Commercial_At               &  -- '@'                            64
      'A'                           &  -- 'A'                            65
      'B'                           &  -- 'B'                            66
      'C'                           &  -- 'C'                            67
      'D'                           &  -- 'D'                            68
      'E'                           &  -- 'E'                            69
      'F'                           &  -- 'F'                            70
      'G'                           &  -- 'G'                            71
      'H'                           &  -- 'H'                            72
      'I'                           &  -- 'I'                            73
      'J'                           &  -- 'J'                            74
      'K'                           &  -- 'K'                            75
      'L'                           &  -- 'L'                            76
      'M'                           &  -- 'M'                            77
      'N'                           &  -- 'N'                            78
      'O'                           &  -- 'O'                            79
      'P'                           &  -- 'P'                            80
      'Q'                           &  -- 'Q'                            81
      'R'                           &  -- 'R'                            82
      'S'                           &  -- 'S'                            83
      'T'                           &  -- 'T'                            84
      'U'                           &  -- 'U'                            85
      'V'                           &  -- 'V'                            86
      'W'                           &  -- 'W'                            87
      'X'                           &  -- 'X'                            88
      'Y'                           &  -- 'Y'                            89
      'Z'                           &  -- 'Z'                            90
      L.Left_Square_Bracket         &  -- '['                            91
      L.Reverse_Solidus             &  -- '\'                            92
      L.Right_Square_Bracket        &  -- ']'                            93
      L.Circumflex                  &  -- '^'                            94
      L.Low_Line                    &  -- '_'                            95
      L.Grave                       &  -- '`'                            96
      L.LC_A                        &  -- 'a'                            97
      L.LC_B                        &  -- 'b'                            98
      L.LC_C                        &  -- 'c'                            99
      L.LC_D                        &  -- 'd'                           100
      L.LC_E                        &  -- 'e'                           101
      L.LC_F                        &  -- 'f'                           102
      L.LC_G                        &  -- 'g'                           103
      L.LC_H                        &  -- 'h'                           104
      L.LC_I                        &  -- 'i'                           105
      L.LC_J                        &  -- 'j'                           106
      L.LC_K                        &  -- 'k'                           107
      L.LC_L                        &  -- 'l'                           108
      L.LC_M                        &  -- 'm'                           109
      L.LC_N                        &  -- 'n'                           110
      L.LC_O                        &  -- 'o'                           111
      L.LC_P                        &  -- 'p'                           112
      L.LC_Q                        &  -- 'q'                           113
      L.LC_R                        &  -- 'r'                           114
      L.LC_S                        &  -- 's'                           115
      L.LC_T                        &  -- 't'                           116
      L.LC_U                        &  -- 'u'                           117
      L.LC_V                        &  -- 'v'                           118
      L.LC_W                        &  -- 'w'                           119
      L.LC_X                        &  -- 'x'                           120
      L.LC_Y                        &  -- 'y'                           121
      L.LC_Z                        &  -- 'z'                           122
      L.Left_Curly_Bracket          &  -- '{'                           123
      L.Vertical_Line               &  -- '|'                           124
      L.Right_Curly_Bracket         &  -- '}'                           125
      L.Tilde                       &  -- '~'                           126
      L.DEL                         &  -- DEL                           127
      L.Reserved_128                &  -- Reserved_128                  128
      L.Reserved_129                &  -- Reserved_129                  129
      L.BPH                         &  -- BPH                           130
      L.NBH                         &  -- NBH                           131
      L.Reserved_132                &  -- Reserved_132                  132
      L.NEL                         &  -- NEL                           133
      L.SSA                         &  -- SSA                           134
      L.ESA                         &  -- ESA                           135
      L.HTS                         &  -- HTS                           136
      L.HTJ                         &  -- HTJ                           137
      L.VTS                         &  -- VTS                           138
      L.PLD                         &  -- PLD                           139
      L.PLU                         &  -- PLU                           140
      L.RI                          &  -- RI                            141
      L.SS2                         &  -- SS2                           142
      L.SS3                         &  -- SS3                           143
      L.DCS                         &  -- DCS                           144
      L.PU1                         &  -- PU1                           145
      L.PU2                         &  -- PU2                           146
      L.STS                         &  -- STS                           147
      L.CCH                         &  -- CCH                           148
      L.MW                          &  -- MW                            149
      L.SPA                         &  -- SPA                           150
      L.EPA                         &  -- EPA                           151
      L.SOS                         &  -- SOS                           152
      L.Reserved_153                &  -- Reserved_153                  153
      L.SCI                         &  -- SCI                           154
      L.CSI                         &  -- CSI                           155
      L.ST                          &  -- ST                            156
      L.OSC                         &  -- OSC                           157
      L.PM                          &  -- PM                            158
      L.APC                         &  -- APC                           159
      L.No_Break_Space              &  -- No_Break_Space                160
      L.Inverted_Exclamation        &  -- Inverted_Exclamation          161
      L.Cent_Sign                   &  -- Cent_Sign                     162
      L.Pound_Sign                  &  -- Pound_Sign                    163
      L.Currency_Sign               &  -- Currency_Sign                 164
      L.Yen_Sign                    &  -- Yen_Sign                      165
      L.Broken_Bar                  &  -- Broken_Bar                    166
      L.Section_Sign                &  -- Section_Sign                  167
      L.Diaeresis                   &  -- Diaeresis                     168
      L.Copyright_Sign              &  -- Copyright_Sign                169
      L.Feminine_Ordinal_Indicator  &  -- Feminine_Ordinal_Indicator    170
      L.Left_Angle_Quotation        &  -- Left_Angle_Quotation          171
      L.Not_Sign                    &  -- Not_Sign                      172
      L.Soft_Hyphen                 &  -- Soft_Hyphen                   173
      L.Registered_Trade_Mark_Sign  &  -- Registered_Trade_Mark_Sign    174
      L.Macron                      &  -- Macron                        175
      L.Degree_Sign                 &  -- Degree_Sign                   176
      L.Plus_Minus_Sign             &  -- Plus_Minus_Sign               177
      L.Superscript_Two             &  -- Superscript_Two               178
      L.Superscript_Three           &  -- Superscript_Three             179
      L.Acute                       &  -- Acute                         180
      L.Micro_Sign                  &  -- Micro_Sign                    181
      L.Pilcrow_Sign                &  -- Pilcrow_Sign                  182
      L.Middle_Dot                  &  -- Middle_Dot                    183
      L.Cedilla                     &  -- Cedilla                       184
      L.Superscript_One             &  -- Superscript_One               185
      L.Masculine_Ordinal_Indicator &  -- Masculine_Ordinal_Indicator   186
      L.Right_Angle_Quotation       &  -- Right_Angle_Quotation         187
      L.Fraction_One_Quarter        &  -- Fraction_One_Quarter          188
      L.Fraction_One_Half           &  -- Fraction_One_Half             189
      L.Fraction_Three_Quarters     &  -- Fraction_Three_Quarters       190
      L.Inverted_Question           &  -- Inverted_Question             191
      L.UC_A_Grave                  &  -- UC_A_Grave                    192
      L.UC_A_Acute                  &  -- UC_A_Acute                    193
      L.UC_A_Circumflex             &  -- UC_A_Circumflex               194
      L.UC_A_Tilde                  &  -- UC_A_Tilde                    195
      L.UC_A_Diaeresis              &  -- UC_A_Diaeresis                196
      L.UC_A_Ring                   &  -- UC_A_Ring                     197
      L.UC_AE_Diphthong             &  -- UC_AE_Diphthong               198
      L.UC_C_Cedilla                &  -- UC_C_Cedilla                  199
      L.UC_E_Grave                  &  -- UC_E_Grave                    200
      L.UC_E_Acute                  &  -- UC_E_Acute                    201
      L.UC_E_Circumflex             &  -- UC_E_Circumflex               202
      L.UC_E_Diaeresis              &  -- UC_E_Diaeresis                203
      L.UC_I_Grave                  &  -- UC_I_Grave                    204
      L.UC_I_Acute                  &  -- UC_I_Acute                    205
      L.UC_I_Circumflex             &  -- UC_I_Circumflex               206
      L.UC_I_Diaeresis              &  -- UC_I_Diaeresis                207
      L.UC_Icelandic_Eth            &  -- UC_Icelandic_Eth              208
      L.UC_N_Tilde                  &  -- UC_N_Tilde                    209
      L.UC_O_Grave                  &  -- UC_O_Grave                    210
      L.UC_O_Acute                  &  -- UC_O_Acute                    211
      L.UC_O_Circumflex             &  -- UC_O_Circumflex               212
      L.UC_O_Tilde                  &  -- UC_O_Tilde                    213
      L.UC_O_Diaeresis              &  -- UC_O_Diaeresis                214
      L.Multiplication_Sign         &  -- Multiplication_Sign           215
      L.UC_O_Oblique_Stroke         &  -- UC_O_Oblique_Stroke           216
      L.UC_U_Grave                  &  -- UC_U_Grave                    217
      L.UC_U_Acute                  &  -- UC_U_Acute                    218
      L.UC_U_Circumflex             &  -- UC_U_Circumflex               219
      L.UC_U_Diaeresis              &  -- UC_U_Diaeresis                220
      L.UC_Y_Acute                  &  -- UC_Y_Acute                    221
      L.UC_Icelandic_Thorn          &  -- UC_Icelandic_Thorn            222
      L.LC_German_Sharp_S           &  -- LC_German_Sharp_S             223
      L.LC_A_Grave                  &  -- LC_A_Grave                    224
      L.LC_A_Acute                  &  -- LC_A_Acute                    225
      L.LC_A_Circumflex             &  -- LC_A_Circumflex               226
      L.LC_A_Tilde                  &  -- LC_A_Tilde                    227
      L.LC_A_Diaeresis              &  -- LC_A_Diaeresis                228
      L.LC_A_Ring                   &  -- LC_A_Ring                     229
      L.LC_AE_Diphthong             &  -- LC_AE_Diphthong               230
      L.LC_C_Cedilla                &  -- LC_C_Cedilla                  231
      L.LC_E_Grave                  &  -- LC_E_Grave                    232
      L.LC_E_Acute                  &  -- LC_E_Acute                    233
      L.LC_E_Circumflex             &  -- LC_E_Circumflex               234
      L.LC_E_Diaeresis              &  -- LC_E_Diaeresis                235
      L.LC_I_Grave                  &  -- LC_I_Grave                    236
      L.LC_I_Acute                  &  -- LC_I_Acute                    237
      L.LC_I_Circumflex             &  -- LC_I_Circumflex               238
      L.LC_I_Diaeresis              &  -- LC_I_Diaeresis                239
      L.LC_Icelandic_Eth            &  -- LC_Icelandic_Eth              240
      L.LC_N_Tilde                  &  -- LC_N_Tilde                    241
      L.LC_O_Grave                  &  -- LC_O_Grave                    242
      L.LC_O_Acute                  &  -- LC_O_Acute                    243
      L.LC_O_Circumflex             &  -- LC_O_Circumflex               244
      L.LC_O_Tilde                  &  -- LC_O_Tilde                    245
      L.LC_O_Diaeresis              &  -- LC_O_Diaeresis                246
      L.Division_Sign               &  -- Division_Sign                 247
      L.LC_O_Oblique_Stroke         &  -- LC_O_Oblique_Stroke           248
      L.LC_U_Grave                  &  -- LC_U_Grave                    249
      L.LC_U_Acute                  &  -- LC_U_Acute                    250
      L.LC_U_Circumflex             &  -- LC_U_Circumflex               251
      L.LC_U_Diaeresis              &  -- LC_U_Diaeresis                252
      L.LC_Y_Acute                  &  -- LC_Y_Acute                    253
      L.LC_Icelandic_Thorn          &  -- LC_Icelandic_Thorn            254
      L.LC_Y_Diaeresis);               -- LC_Y_Diaeresis                255

end Ada.Strings.Maps;
