------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . S T R I N G S . M A P S . C O N S T A N T S            --
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

package Ada.Strings.Maps.Constants is
   pragma Pure;
   --  In accordance with Ada 2005 AI-362

   Control_Set           : constant Character_Set;
   Graphic_Set           : constant Character_Set;
   Letter_Set            : constant Character_Set;
   Lower_Set             : constant Character_Set;
   Upper_Set             : constant Character_Set;
   Basic_Set             : constant Character_Set;
   Decimal_Digit_Set     : constant Character_Set;
   Hexadecimal_Digit_Set : constant Character_Set;
   Alphanumeric_Set      : constant Character_Set;
   Special_Set           : constant Character_Set;
   ISO_646_Set           : constant Character_Set;

   Lower_Case_Map        : constant Character_Mapping;
   --  Maps to lower case for letters, else identity

   Upper_Case_Map        : constant Character_Mapping;
   --  Maps to upper case for letters, else identity

   Basic_Map             : constant Character_Mapping;
   --  Maps to basic letters for letters, else identity

private
   package L renames Ada.Characters.Latin_1;

   Control_Set               : constant Character_Set :=
     (L.NUL                  ..  L.US                  => True,
      L.DEL                  ..  L.APC                 => True,
      others                                           => False);

   Graphic_Set               : constant Character_Set :=
     (L.Space                ..  L.Tilde               => True,
      L.No_Break_Space       ..  L.LC_Y_Diaeresis      => True,
      others                                           => False);

   Letter_Set                : constant Character_Set :=
     ('A'                    .. 'Z'                    => True,
      L.LC_A                 ..  L.LC_Z                => True,
      L.UC_A_Grave           ..  L.UC_O_Diaeresis      => True,
      L.UC_O_Oblique_Stroke  ..  L.LC_O_Diaeresis      => True,
      L.LC_O_Oblique_Stroke  ..  L.LC_Y_Diaeresis      => True,
      others                                           => False);

   Lower_Set                 : constant Character_Set :=
     (L.LC_A                 ..  L.LC_Z                => True,
      L.LC_German_Sharp_S    ..  L.LC_O_Diaeresis      => True,
      L.LC_O_Oblique_Stroke  ..  L.LC_Y_Diaeresis      => True,
      others                                           => False);

   Upper_Set                 : constant Character_Set :=
     ('A'                    ..  'Z'                   => True,
      L.UC_A_Grave           ..  L.UC_O_Diaeresis      => True,
      L.UC_O_Oblique_Stroke  ..  L.UC_Icelandic_Thorn  => True,
      others                                           => False);

   Basic_Set                 : constant Character_Set :=
     ('A'                    .. 'Z'                    => True,
      L.LC_A                 ..  L.LC_Z                => True,
      L.UC_AE_Diphthong      ..  L.UC_AE_Diphthong     => True,
      L.LC_AE_Diphthong      ..  L.LC_AE_Diphthong     => True,
      L.LC_German_Sharp_S    ..  L.LC_German_Sharp_S   => True,
      L.UC_Icelandic_Thorn   ..  L.UC_Icelandic_Thorn  => True,
      L.LC_Icelandic_Thorn   ..  L.LC_Icelandic_Thorn  => True,
      L.UC_Icelandic_Eth     ..  L.UC_Icelandic_Eth    => True,
      L.LC_Icelandic_Eth     ..  L.LC_Icelandic_Eth    => True,
      others                                           => False);

   Decimal_Digit_Set         : constant Character_Set :=
     ('0'                    ..  '9'                   => True,
      others                                           => False);

   Hexadecimal_Digit_Set     : constant Character_Set :=
     ('0'                    ..  '9'                   => True,
      'A'                    ..  'F'                   => True,
      L.LC_A                 ..  L.LC_F                => True,
      others                                           => False);

   Alphanumeric_Set          : constant Character_Set :=
     ('0'                    ..  '9'                   => True,
      'A'                    ..  'Z'                   => True,
      L.LC_A                 ..  L.LC_Z                => True,
      L.UC_A_Grave           ..  L.UC_O_Diaeresis      => True,
      L.UC_O_Oblique_Stroke  ..  L.LC_O_Diaeresis      => True,
      L.LC_O_Oblique_Stroke  ..  L.LC_Y_Diaeresis      => True,
      others                                           => False);

   Special_Set               : constant Character_Set :=
     (L.Space                ..  L.Solidus             => True,
      L.Colon                ..  L.Commercial_At       => True,
      L.Left_Square_Bracket  ..  L.Grave               => True,
      L.Left_Curly_Bracket   ..  L.Tilde               => True,
      L.No_Break_Space       ..  L.Inverted_Question   => True,
      L.Multiplication_Sign  ..  L.Multiplication_Sign => True,
      L.Division_Sign        ..  L.Division_Sign       => True,
      others                                           => False);

   ISO_646_Set               : constant Character_Set :=
     (L.NUL                  ..  L.DEL                 => True,
      others                                           => False);

   Lower_Case_Map : constant Character_Mapping :=
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
      L.LC_A                        &  -- 'a'                            65
      L.LC_B                        &  -- 'b'                            66
      L.LC_C                        &  -- 'c'                            67
      L.LC_D                        &  -- 'd'                            68
      L.LC_E                        &  -- 'e'                            69
      L.LC_F                        &  -- 'f'                            70
      L.LC_G                        &  -- 'g'                            71
      L.LC_H                        &  -- 'h'                            72
      L.LC_I                        &  -- 'i'                            73
      L.LC_J                        &  -- 'j'                            74
      L.LC_K                        &  -- 'k'                            75
      L.LC_L                        &  -- 'l'                            76
      L.LC_M                        &  -- 'm'                            77
      L.LC_N                        &  -- 'n'                            78
      L.LC_O                        &  -- 'o'                            79
      L.LC_P                        &  -- 'p'                            80
      L.LC_Q                        &  -- 'q'                            81
      L.LC_R                        &  -- 'r'                            82
      L.LC_S                        &  -- 's'                            83
      L.LC_T                        &  -- 't'                            84
      L.LC_U                        &  -- 'u'                            85
      L.LC_V                        &  -- 'v'                            86
      L.LC_W                        &  -- 'w'                            87
      L.LC_X                        &  -- 'x'                            88
      L.LC_Y                        &  -- 'y'                            89
      L.LC_Z                        &  -- 'z'                            90
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
      L.LC_A_Grave                  &  -- UC_A_Grave                    192
      L.LC_A_Acute                  &  -- UC_A_Acute                    193
      L.LC_A_Circumflex             &  -- UC_A_Circumflex               194
      L.LC_A_Tilde                  &  -- UC_A_Tilde                    195
      L.LC_A_Diaeresis              &  -- UC_A_Diaeresis                196
      L.LC_A_Ring                   &  -- UC_A_Ring                     197
      L.LC_AE_Diphthong             &  -- UC_AE_Diphthong               198
      L.LC_C_Cedilla                &  -- UC_C_Cedilla                  199
      L.LC_E_Grave                  &  -- UC_E_Grave                    200
      L.LC_E_Acute                  &  -- UC_E_Acute                    201
      L.LC_E_Circumflex             &  -- UC_E_Circumflex               202
      L.LC_E_Diaeresis              &  -- UC_E_Diaeresis                203
      L.LC_I_Grave                  &  -- UC_I_Grave                    204
      L.LC_I_Acute                  &  -- UC_I_Acute                    205
      L.LC_I_Circumflex             &  -- UC_I_Circumflex               206
      L.LC_I_Diaeresis              &  -- UC_I_Diaeresis                207
      L.LC_Icelandic_Eth            &  -- UC_Icelandic_Eth              208
      L.LC_N_Tilde                  &  -- UC_N_Tilde                    209
      L.LC_O_Grave                  &  -- UC_O_Grave                    210
      L.LC_O_Acute                  &  -- UC_O_Acute                    211
      L.LC_O_Circumflex             &  -- UC_O_Circumflex               212
      L.LC_O_Tilde                  &  -- UC_O_Tilde                    213
      L.LC_O_Diaeresis              &  -- UC_O_Diaeresis                214
      L.Multiplication_Sign         &  -- Multiplication_Sign           215
      L.LC_O_Oblique_Stroke         &  -- UC_O_Oblique_Stroke           216
      L.LC_U_Grave                  &  -- UC_U_Grave                    217
      L.LC_U_Acute                  &  -- UC_U_Acute                    218
      L.LC_U_Circumflex             &  -- UC_U_Circumflex               219
      L.LC_U_Diaeresis              &  -- UC_U_Diaeresis                220
      L.LC_Y_Acute                  &  -- UC_Y_Acute                    221
      L.LC_Icelandic_Thorn          &  -- UC_Icelandic_Thorn            222
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

   Upper_Case_Map : constant Character_Mapping :=
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
      'A'                           &  -- 'a'                            97
      'B'                           &  -- 'b'                            98
      'C'                           &  -- 'c'                            99
      'D'                           &  -- 'd'                           100
      'E'                           &  -- 'e'                           101
      'F'                           &  -- 'f'                           102
      'G'                           &  -- 'g'                           103
      'H'                           &  -- 'h'                           104
      'I'                           &  -- 'i'                           105
      'J'                           &  -- 'j'                           106
      'K'                           &  -- 'k'                           107
      'L'                           &  -- 'l'                           108
      'M'                           &  -- 'm'                           109
      'N'                           &  -- 'n'                           110
      'O'                           &  -- 'o'                           111
      'P'                           &  -- 'p'                           112
      'Q'                           &  -- 'q'                           113
      'R'                           &  -- 'r'                           114
      'S'                           &  -- 's'                           115
      'T'                           &  -- 't'                           116
      'U'                           &  -- 'u'                           117
      'V'                           &  -- 'v'                           118
      'W'                           &  -- 'w'                           119
      'X'                           &  -- 'x'                           120
      'Y'                           &  -- 'y'                           121
      'Z'                           &  -- 'z'                           122
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
      L.UC_A_Grave                  &  -- LC_A_Grave                    224
      L.UC_A_Acute                  &  -- LC_A_Acute                    225
      L.UC_A_Circumflex             &  -- LC_A_Circumflex               226
      L.UC_A_Tilde                  &  -- LC_A_Tilde                    227
      L.UC_A_Diaeresis              &  -- LC_A_Diaeresis                228
      L.UC_A_Ring                   &  -- LC_A_Ring                     229
      L.UC_AE_Diphthong             &  -- LC_AE_Diphthong               230
      L.UC_C_Cedilla                &  -- LC_C_Cedilla                  231
      L.UC_E_Grave                  &  -- LC_E_Grave                    232
      L.UC_E_Acute                  &  -- LC_E_Acute                    233
      L.UC_E_Circumflex             &  -- LC_E_Circumflex               234
      L.UC_E_Diaeresis              &  -- LC_E_Diaeresis                235
      L.UC_I_Grave                  &  -- LC_I_Grave                    236
      L.UC_I_Acute                  &  -- LC_I_Acute                    237
      L.UC_I_Circumflex             &  -- LC_I_Circumflex               238
      L.UC_I_Diaeresis              &  -- LC_I_Diaeresis                239
      L.UC_Icelandic_Eth            &  -- LC_Icelandic_Eth              240
      L.UC_N_Tilde                  &  -- LC_N_Tilde                    241
      L.UC_O_Grave                  &  -- LC_O_Grave                    242
      L.UC_O_Acute                  &  -- LC_O_Acute                    243
      L.UC_O_Circumflex             &  -- LC_O_Circumflex               244
      L.UC_O_Tilde                  &  -- LC_O_Tilde                    245
      L.UC_O_Diaeresis              &  -- LC_O_Diaeresis                246
      L.Division_Sign               &  -- Division_Sign                 247
      L.UC_O_Oblique_Stroke         &  -- LC_O_Oblique_Stroke           248
      L.UC_U_Grave                  &  -- LC_U_Grave                    249
      L.UC_U_Acute                  &  -- LC_U_Acute                    250
      L.UC_U_Circumflex             &  -- LC_U_Circumflex               251
      L.UC_U_Diaeresis              &  -- LC_U_Diaeresis                252
      L.UC_Y_Acute                  &  -- LC_Y_Acute                    253
      L.UC_Icelandic_Thorn          &  -- LC_Icelandic_Thorn            254
      L.LC_Y_Diaeresis);               -- LC_Y_Diaeresis                255

   Basic_Map : constant Character_Mapping :=
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
      'A'                           &  -- UC_A_Grave                    192
      'A'                           &  -- UC_A_Acute                    193
      'A'                           &  -- UC_A_Circumflex               194
      'A'                           &  -- UC_A_Tilde                    195
      'A'                           &  -- UC_A_Diaeresis                196
      'A'                           &  -- UC_A_Ring                     197
      L.UC_AE_Diphthong             &  -- UC_AE_Diphthong               198
      'C'                           &  -- UC_C_Cedilla                  199
      'E'                           &  -- UC_E_Grave                    200
      'E'                           &  -- UC_E_Acute                    201
      'E'                           &  -- UC_E_Circumflex               202
      'E'                           &  -- UC_E_Diaeresis                203
      'I'                           &  -- UC_I_Grave                    204
      'I'                           &  -- UC_I_Acute                    205
      'I'                           &  -- UC_I_Circumflex               206
      'I'                           &  -- UC_I_Diaeresis                207
      L.UC_Icelandic_Eth            &  -- UC_Icelandic_Eth              208
      'N'                           &  -- UC_N_Tilde                    209
      'O'                           &  -- UC_O_Grave                    210
      'O'                           &  -- UC_O_Acute                    211
      'O'                           &  -- UC_O_Circumflex               212
      'O'                           &  -- UC_O_Tilde                    213
      'O'                           &  -- UC_O_Diaeresis                214
      L.Multiplication_Sign         &  -- Multiplication_Sign           215
      'O'                           &  -- UC_O_Oblique_Stroke           216
      'U'                           &  -- UC_U_Grave                    217
      'U'                           &  -- UC_U_Acute                    218
      'U'                           &  -- UC_U_Circumflex               219
      'U'                           &  -- UC_U_Diaeresis                220
      'Y'                           &  -- UC_Y_Acute                    221
      L.UC_Icelandic_Thorn          &  -- UC_Icelandic_Thorn            222
      L.LC_German_Sharp_S           &  -- LC_German_Sharp_S             223
      L.LC_A                        &  -- LC_A_Grave                    224
      L.LC_A                        &  -- LC_A_Acute                    225
      L.LC_A                        &  -- LC_A_Circumflex               226
      L.LC_A                        &  -- LC_A_Tilde                    227
      L.LC_A                        &  -- LC_A_Diaeresis                228
      L.LC_A                        &  -- LC_A_Ring                     229
      L.LC_AE_Diphthong             &  -- LC_AE_Diphthong               230
      L.LC_C                        &  -- LC_C_Cedilla                  231
      L.LC_E                        &  -- LC_E_Grave                    232
      L.LC_E                        &  -- LC_E_Acute                    233
      L.LC_E                        &  -- LC_E_Circumflex               234
      L.LC_E                        &  -- LC_E_Diaeresis                235
      L.LC_I                        &  -- LC_I_Grave                    236
      L.LC_I                        &  -- LC_I_Acute                    237
      L.LC_I                        &  -- LC_I_Circumflex               238
      L.LC_I                        &  -- LC_I_Diaeresis                239
      L.LC_Icelandic_Eth            &  -- LC_Icelandic_Eth              240
      L.LC_N                        &  -- LC_N_Tilde                    241
      L.LC_O                        &  -- LC_O_Grave                    242
      L.LC_O                        &  -- LC_O_Acute                    243
      L.LC_O                        &  -- LC_O_Circumflex               244
      L.LC_O                        &  -- LC_O_Tilde                    245
      L.LC_O                        &  -- LC_O_Diaeresis                246
      L.Division_Sign               &  -- Division_Sign                 247
      L.LC_O                        &  -- LC_O_Oblique_Stroke           248
      L.LC_U                        &  -- LC_U_Grave                    249
      L.LC_U                        &  -- LC_U_Acute                    250
      L.LC_U                        &  -- LC_U_Circumflex               251
      L.LC_U                        &  -- LC_U_Diaeresis                252
      L.LC_Y                        &  -- LC_Y_Acute                    253
      L.LC_Icelandic_Thorn          &  -- LC_Icelandic_Thorn            254
      L.LC_Y);                         -- LC_Y_Diaeresis                255

end Ada.Strings.Maps.Constants;
