------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              I N T E R F A C E S . C . E X T E N S I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

--  This package contains additional C-related definitions, intended for use
--  with either manually or automatically generated bindings to C libraries.

with System;

package Interfaces.C.Extensions is
   pragma Pure;

   --  Definitions for C "void" and "void *" types

   subtype void     is System.Address;
   subtype void_ptr is System.Address;

   --  Definitions for C incomplete/unknown structs

   subtype opaque_structure_def is System.Address;
   type opaque_structure_def_ptr is access opaque_structure_def;
   for opaque_structure_def_ptr'Storage_Size use 0;

   --  Definitions for C++ incomplete/unknown classes

   subtype incomplete_class_def is System.Address;
   type incomplete_class_def_ptr is access incomplete_class_def;
   for incomplete_class_def_ptr'Storage_Size use 0;

   --  C bool

   subtype bool is Interfaces.C.C_bool;

   --  64-bit integer types

   subtype long_long is Interfaces.C.long_long;
   subtype unsigned_long_long is Interfaces.C.unsigned_long_long;

   --  128-bit floating-point type available on x86:
   --  typedef float float_128 __attribute__ ((mode (TF)));

   type Float_128 is record
      low, high : unsigned_long_long;
   end record;
   pragma Convention (C_Pass_By_Copy, Float_128);
   for Float_128'Alignment use unsigned_long_long'Alignment * 2;

   --  128-bit complex floating-point type available on x86:
   --  typedef _Complex float cfloat_128 __attribute__ ((mode (TC)));

   type CFloat_128 is record
      re, im : Float_128;
   end record;
   pragma Convention (C_Pass_By_Copy, CFloat_128);

   --  Types for bitfields

   type Unsigned_1 is mod 2 ** 1;
   for Unsigned_1'Size use 1;

   type Unsigned_2 is mod 2 ** 2;
   for Unsigned_2'Size use 2;

   type Unsigned_3 is mod 2 ** 3;
   for Unsigned_3'Size use 3;

   type Unsigned_4 is mod 2 ** 4;
   for Unsigned_4'Size use 4;

   type Unsigned_5 is mod 2 ** 5;
   for Unsigned_5'Size use 5;

   type Unsigned_6 is mod 2 ** 6;
   for Unsigned_6'Size use 6;

   type Unsigned_7 is mod 2 ** 7;
   for Unsigned_7'Size use 7;

   type Unsigned_8 is mod 2 ** 8;
   for Unsigned_8'Size use 8;

   type Unsigned_9 is mod 2 ** 9;
   for Unsigned_9'Size use 9;

   type Unsigned_10 is mod 2 ** 10;
   for Unsigned_10'Size use 10;

   type Unsigned_11 is mod 2 ** 11;
   for Unsigned_11'Size use 11;

   type Unsigned_12 is mod 2 ** 12;
   for Unsigned_12'Size use 12;

   type Unsigned_13 is mod 2 ** 13;
   for Unsigned_13'Size use 13;

   type Unsigned_14 is mod 2 ** 14;
   for Unsigned_14'Size use 14;

   type Unsigned_15 is mod 2 ** 15;
   for Unsigned_15'Size use 15;

   type Unsigned_16 is mod 2 ** 16;
   for Unsigned_16'Size use 16;

   type Unsigned_17 is mod 2 ** 17;
   for Unsigned_17'Size use 17;

   type Unsigned_18 is mod 2 ** 18;
   for Unsigned_18'Size use 18;

   type Unsigned_19 is mod 2 ** 19;
   for Unsigned_19'Size use 19;

   type Unsigned_20 is mod 2 ** 20;
   for Unsigned_20'Size use 20;

   type Unsigned_21 is mod 2 ** 21;
   for Unsigned_21'Size use 21;

   type Unsigned_22 is mod 2 ** 22;
   for Unsigned_22'Size use 22;

   type Unsigned_23 is mod 2 ** 23;
   for Unsigned_23'Size use 23;

   type Unsigned_24 is mod 2 ** 24;
   for Unsigned_24'Size use 24;

   type Unsigned_25 is mod 2 ** 25;
   for Unsigned_25'Size use 25;

   type Unsigned_26 is mod 2 ** 26;
   for Unsigned_26'Size use 26;

   type Unsigned_27 is mod 2 ** 27;
   for Unsigned_27'Size use 27;

   type Unsigned_28 is mod 2 ** 28;
   for Unsigned_28'Size use 28;

   type Unsigned_29 is mod 2 ** 29;
   for Unsigned_29'Size use 29;

   type Unsigned_30 is mod 2 ** 30;
   for Unsigned_30'Size use 30;

   type Unsigned_31 is mod 2 ** 31;
   for Unsigned_31'Size use 31;

   type Unsigned_32 is mod 2 ** 32;
   for Unsigned_32'Size use 32;

   type Unsigned_33 is mod 2 ** 33;
   for Unsigned_33'Size use 33;

   type Unsigned_34 is mod 2 ** 34;
   for Unsigned_34'Size use 34;

   type Unsigned_35 is mod 2 ** 35;
   for Unsigned_35'Size use 35;

   type Unsigned_36 is mod 2 ** 36;
   for Unsigned_36'Size use 36;

   type Unsigned_37 is mod 2 ** 37;
   for Unsigned_37'Size use 37;

   type Unsigned_38 is mod 2 ** 38;
   for Unsigned_38'Size use 38;

   type Unsigned_39 is mod 2 ** 39;
   for Unsigned_39'Size use 39;

   type Unsigned_40 is mod 2 ** 40;
   for Unsigned_40'Size use 40;

   type Unsigned_41 is mod 2 ** 41;
   for Unsigned_41'Size use 41;

   type Unsigned_42 is mod 2 ** 42;
   for Unsigned_42'Size use 42;

   type Unsigned_43 is mod 2 ** 43;
   for Unsigned_43'Size use 43;

   type Unsigned_44 is mod 2 ** 44;
   for Unsigned_44'Size use 44;

   type Unsigned_45 is mod 2 ** 45;
   for Unsigned_45'Size use 45;

   type Unsigned_46 is mod 2 ** 46;
   for Unsigned_46'Size use 46;

   type Unsigned_47 is mod 2 ** 47;
   for Unsigned_47'Size use 47;

   type Unsigned_48 is mod 2 ** 48;
   for Unsigned_48'Size use 48;

   type Unsigned_49 is mod 2 ** 49;
   for Unsigned_49'Size use 49;

   type Unsigned_50 is mod 2 ** 50;
   for Unsigned_50'Size use 50;

   type Unsigned_51 is mod 2 ** 51;
   for Unsigned_51'Size use 51;

   type Unsigned_52 is mod 2 ** 52;
   for Unsigned_52'Size use 52;

   type Unsigned_53 is mod 2 ** 53;
   for Unsigned_53'Size use 53;

   type Unsigned_54 is mod 2 ** 54;
   for Unsigned_54'Size use 54;

   type Unsigned_55 is mod 2 ** 55;
   for Unsigned_55'Size use 55;

   type Unsigned_56 is mod 2 ** 56;
   for Unsigned_56'Size use 56;

   type Unsigned_57 is mod 2 ** 57;
   for Unsigned_57'Size use 57;

   type Unsigned_58 is mod 2 ** 58;
   for Unsigned_58'Size use 58;

   type Unsigned_59 is mod 2 ** 59;
   for Unsigned_59'Size use 59;

   type Unsigned_60 is mod 2 ** 60;
   for Unsigned_60'Size use 60;

   type Unsigned_61 is mod 2 ** 61;
   for Unsigned_61'Size use 61;

   type Unsigned_62 is mod 2 ** 62;
   for Unsigned_62'Size use 62;

   type Unsigned_63 is mod 2 ** 63;
   for Unsigned_63'Size use 63;

   type Unsigned_64 is mod 2 ** 64;
   for Unsigned_64'Size use 64;

   type Unsigned_65 is mod 2 ** 65;
   for Unsigned_65'Size use 65;

   type Unsigned_66 is mod 2 ** 66;
   for Unsigned_66'Size use 66;

   type Unsigned_67 is mod 2 ** 67;
   for Unsigned_67'Size use 67;

   type Unsigned_68 is mod 2 ** 68;
   for Unsigned_68'Size use 68;

   type Unsigned_69 is mod 2 ** 69;
   for Unsigned_69'Size use 69;

   type Unsigned_70 is mod 2 ** 70;
   for Unsigned_70'Size use 70;

   type Unsigned_71 is mod 2 ** 71;
   for Unsigned_71'Size use 71;

   type Unsigned_72 is mod 2 ** 72;
   for Unsigned_72'Size use 72;

   type Unsigned_73 is mod 2 ** 73;
   for Unsigned_73'Size use 73;

   type Unsigned_74 is mod 2 ** 74;
   for Unsigned_74'Size use 74;

   type Unsigned_75 is mod 2 ** 75;
   for Unsigned_75'Size use 75;

   type Unsigned_76 is mod 2 ** 76;
   for Unsigned_76'Size use 76;

   type Unsigned_77 is mod 2 ** 77;
   for Unsigned_77'Size use 77;

   type Unsigned_78 is mod 2 ** 78;
   for Unsigned_78'Size use 78;

   type Unsigned_79 is mod 2 ** 79;
   for Unsigned_79'Size use 79;

   type Unsigned_80 is mod 2 ** 80;
   for Unsigned_80'Size use 80;

   type Unsigned_81 is mod 2 ** 81;
   for Unsigned_81'Size use 81;

   type Unsigned_82 is mod 2 ** 82;
   for Unsigned_82'Size use 82;

   type Unsigned_83 is mod 2 ** 83;
   for Unsigned_83'Size use 83;

   type Unsigned_84 is mod 2 ** 84;
   for Unsigned_84'Size use 84;

   type Unsigned_85 is mod 2 ** 85;
   for Unsigned_85'Size use 85;

   type Unsigned_86 is mod 2 ** 86;
   for Unsigned_86'Size use 86;

   type Unsigned_87 is mod 2 ** 87;
   for Unsigned_87'Size use 87;

   type Unsigned_88 is mod 2 ** 88;
   for Unsigned_88'Size use 88;

   type Unsigned_89 is mod 2 ** 89;
   for Unsigned_89'Size use 89;

   type Unsigned_90 is mod 2 ** 90;
   for Unsigned_90'Size use 90;

   type Unsigned_91 is mod 2 ** 91;
   for Unsigned_91'Size use 91;

   type Unsigned_92 is mod 2 ** 92;
   for Unsigned_92'Size use 92;

   type Unsigned_93 is mod 2 ** 93;
   for Unsigned_93'Size use 93;

   type Unsigned_94 is mod 2 ** 94;
   for Unsigned_94'Size use 94;

   type Unsigned_95 is mod 2 ** 95;
   for Unsigned_95'Size use 95;

   type Unsigned_96 is mod 2 ** 96;
   for Unsigned_96'Size use 96;

   type Unsigned_97 is mod 2 ** 97;
   for Unsigned_97'Size use 97;

   type Unsigned_98 is mod 2 ** 98;
   for Unsigned_98'Size use 98;

   type Unsigned_99 is mod 2 ** 99;
   for Unsigned_99'Size use 99;

   type Unsigned_100 is mod 2 ** 100;
   for Unsigned_100'Size use 100;

   type Unsigned_101 is mod 2 ** 101;
   for Unsigned_101'Size use 101;

   type Unsigned_102 is mod 2 ** 102;
   for Unsigned_102'Size use 102;

   type Unsigned_103 is mod 2 ** 103;
   for Unsigned_103'Size use 103;

   type Unsigned_104 is mod 2 ** 104;
   for Unsigned_104'Size use 104;

   type Unsigned_105 is mod 2 ** 105;
   for Unsigned_105'Size use 105;

   type Unsigned_106 is mod 2 ** 106;
   for Unsigned_106'Size use 106;

   type Unsigned_107 is mod 2 ** 107;
   for Unsigned_107'Size use 107;

   type Unsigned_108 is mod 2 ** 108;
   for Unsigned_108'Size use 108;

   type Unsigned_109 is mod 2 ** 109;
   for Unsigned_109'Size use 109;

   type Unsigned_110 is mod 2 ** 110;
   for Unsigned_110'Size use 110;

   type Unsigned_111 is mod 2 ** 111;
   for Unsigned_111'Size use 111;

   type Unsigned_112 is mod 2 ** 112;
   for Unsigned_112'Size use 112;

   type Unsigned_113 is mod 2 ** 113;
   for Unsigned_113'Size use 113;

   type Unsigned_114 is mod 2 ** 114;
   for Unsigned_114'Size use 114;

   type Unsigned_115 is mod 2 ** 115;
   for Unsigned_115'Size use 115;

   type Unsigned_116 is mod 2 ** 116;
   for Unsigned_116'Size use 116;

   type Unsigned_117 is mod 2 ** 117;
   for Unsigned_117'Size use 117;

   type Unsigned_118 is mod 2 ** 118;
   for Unsigned_118'Size use 118;

   type Unsigned_119 is mod 2 ** 119;
   for Unsigned_119'Size use 119;

   type Unsigned_120 is mod 2 ** 120;
   for Unsigned_120'Size use 120;

   type Unsigned_121 is mod 2 ** 121;
   for Unsigned_121'Size use 121;

   type Unsigned_122 is mod 2 ** 122;
   for Unsigned_122'Size use 122;

   type Unsigned_123 is mod 2 ** 123;
   for Unsigned_123'Size use 123;

   type Unsigned_124 is mod 2 ** 124;
   for Unsigned_124'Size use 124;

   type Unsigned_125 is mod 2 ** 125;
   for Unsigned_125'Size use 125;

   type Unsigned_126 is mod 2 ** 126;
   for Unsigned_126'Size use 126;

   type Unsigned_127 is mod 2 ** 127;
   for Unsigned_127'Size use 127;

   type Unsigned_128 is mod 2 ** 128;
   for Unsigned_128'Size use 128;

   type Signed_2 is range -2 ** 1 .. 2 ** 1 - 1;
   for Signed_2'Size use 2;

   type Signed_3 is range -2 ** 2 .. 2 ** 2 - 1;
   for Signed_3'Size use 3;

   type Signed_4 is range -2 ** 3 .. 2 ** 3 - 1;
   for Signed_4'Size use 4;

   type Signed_5 is range -2 ** 4 .. 2 ** 4 - 1;
   for Signed_5'Size use 5;

   type Signed_6 is range -2 ** 5 .. 2 ** 5 - 1;
   for Signed_6'Size use 6;

   type Signed_7 is range -2 ** 6 .. 2 ** 6 - 1;
   for Signed_7'Size use 7;

   type Signed_8 is range -2 ** 7 .. 2 ** 7 - 1;
   for Signed_8'Size use 8;

   type Signed_9 is range -2 ** 8 .. 2 ** 8 - 1;
   for Signed_9'Size use 9;

   type Signed_10 is range -2 ** 9 .. 2 ** 9 - 1;
   for Signed_10'Size use 10;

   type Signed_11 is range -2 ** 10 .. 2 ** 10 - 1;
   for Signed_11'Size use 11;

   type Signed_12 is range -2 ** 11 .. 2 ** 11 - 1;
   for Signed_12'Size use 12;

   type Signed_13 is range -2 ** 12 .. 2 ** 12 - 1;
   for Signed_13'Size use 13;

   type Signed_14 is range -2 ** 13 .. 2 ** 13 - 1;
   for Signed_14'Size use 14;

   type Signed_15 is range -2 ** 14 .. 2 ** 14 - 1;
   for Signed_15'Size use 15;

   type Signed_16 is range -2 ** 15 .. 2 ** 15 - 1;
   for Signed_16'Size use 16;

   type Signed_17 is range -2 ** 16 .. 2 ** 16 - 1;
   for Signed_17'Size use 17;

   type Signed_18 is range -2 ** 17 .. 2 ** 17 - 1;
   for Signed_18'Size use 18;

   type Signed_19 is range -2 ** 18 .. 2 ** 18 - 1;
   for Signed_19'Size use 19;

   type Signed_20 is range -2 ** 19 .. 2 ** 19 - 1;
   for Signed_20'Size use 20;

   type Signed_21 is range -2 ** 20 .. 2 ** 20 - 1;
   for Signed_21'Size use 21;

   type Signed_22 is range -2 ** 21 .. 2 ** 21 - 1;
   for Signed_22'Size use 22;

   type Signed_23 is range -2 ** 22 .. 2 ** 22 - 1;
   for Signed_23'Size use 23;

   type Signed_24 is range -2 ** 23 .. 2 ** 23 - 1;
   for Signed_24'Size use 24;

   type Signed_25 is range -2 ** 24 .. 2 ** 24 - 1;
   for Signed_25'Size use 25;

   type Signed_26 is range -2 ** 25 .. 2 ** 25 - 1;
   for Signed_26'Size use 26;

   type Signed_27 is range -2 ** 26 .. 2 ** 26 - 1;
   for Signed_27'Size use 27;

   type Signed_28 is range -2 ** 27 .. 2 ** 27 - 1;
   for Signed_28'Size use 28;

   type Signed_29 is range -2 ** 28 .. 2 ** 28 - 1;
   for Signed_29'Size use 29;

   type Signed_30 is range -2 ** 29 .. 2 ** 29 - 1;
   for Signed_30'Size use 30;

   type Signed_31 is range -2 ** 30 .. 2 ** 30 - 1;
   for Signed_31'Size use 31;

   type Signed_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Signed_32'Size use 32;

   type Signed_33 is range -2 ** 32 .. 2 ** 32 - 1;
   for Signed_33'Size use 33;

   type Signed_34 is range -2 ** 33 .. 2 ** 33 - 1;
   for Signed_34'Size use 34;

   type Signed_35 is range -2 ** 34 .. 2 ** 34 - 1;
   for Signed_35'Size use 35;

   type Signed_36 is range -2 ** 35 .. 2 ** 35 - 1;
   for Signed_36'Size use 36;

   type Signed_37 is range -2 ** 36 .. 2 ** 36 - 1;
   for Signed_37'Size use 37;

   type Signed_38 is range -2 ** 37 .. 2 ** 37 - 1;
   for Signed_38'Size use 38;

   type Signed_39 is range -2 ** 38 .. 2 ** 38 - 1;
   for Signed_39'Size use 39;

   type Signed_40 is range -2 ** 39 .. 2 ** 39 - 1;
   for Signed_40'Size use 40;

   type Signed_41 is range -2 ** 40 .. 2 ** 40 - 1;
   for Signed_41'Size use 41;

   type Signed_42 is range -2 ** 41 .. 2 ** 41 - 1;
   for Signed_42'Size use 42;

   type Signed_43 is range -2 ** 42 .. 2 ** 42 - 1;
   for Signed_43'Size use 43;

   type Signed_44 is range -2 ** 43 .. 2 ** 43 - 1;
   for Signed_44'Size use 44;

   type Signed_45 is range -2 ** 44 .. 2 ** 44 - 1;
   for Signed_45'Size use 45;

   type Signed_46 is range -2 ** 45 .. 2 ** 45 - 1;
   for Signed_46'Size use 46;

   type Signed_47 is range -2 ** 46 .. 2 ** 46 - 1;
   for Signed_47'Size use 47;

   type Signed_48 is range -2 ** 47 .. 2 ** 47 - 1;
   for Signed_48'Size use 48;

   type Signed_49 is range -2 ** 48 .. 2 ** 48 - 1;
   for Signed_49'Size use 49;

   type Signed_50 is range -2 ** 49 .. 2 ** 49 - 1;
   for Signed_50'Size use 50;

   type Signed_51 is range -2 ** 50 .. 2 ** 50 - 1;
   for Signed_51'Size use 51;

   type Signed_52 is range -2 ** 51 .. 2 ** 51 - 1;
   for Signed_52'Size use 52;

   type Signed_53 is range -2 ** 52 .. 2 ** 52 - 1;
   for Signed_53'Size use 53;

   type Signed_54 is range -2 ** 53 .. 2 ** 53 - 1;
   for Signed_54'Size use 54;

   type Signed_55 is range -2 ** 54 .. 2 ** 54 - 1;
   for Signed_55'Size use 55;

   type Signed_56 is range -2 ** 55 .. 2 ** 55 - 1;
   for Signed_56'Size use 56;

   type Signed_57 is range -2 ** 56 .. 2 ** 56 - 1;
   for Signed_57'Size use 57;

   type Signed_58 is range -2 ** 57 .. 2 ** 57 - 1;
   for Signed_58'Size use 58;

   type Signed_59 is range -2 ** 58 .. 2 ** 58 - 1;
   for Signed_59'Size use 59;

   type Signed_60 is range -2 ** 59 .. 2 ** 59 - 1;
   for Signed_60'Size use 60;

   type Signed_61 is range -2 ** 60 .. 2 ** 60 - 1;
   for Signed_61'Size use 61;

   type Signed_62 is range -2 ** 61 .. 2 ** 61 - 1;
   for Signed_62'Size use 62;

   type Signed_63 is range -2 ** 62 .. 2 ** 62 - 1;
   for Signed_63'Size use 63;

   type Signed_64 is range -2 ** 63 .. 2 ** 63 - 1;
   for Signed_64'Size use 64;

   type Signed_65 is range -2 ** 64 .. 2 ** 64 - 1;
   for Signed_65'Size use 65;

   type Signed_66 is range -2 ** 65 .. 2 ** 65 - 1;
   for Signed_66'Size use 66;

   type Signed_67 is range -2 ** 66 .. 2 ** 66 - 1;
   for Signed_67'Size use 67;

   type Signed_68 is range -2 ** 67 .. 2 ** 67 - 1;
   for Signed_68'Size use 68;

   type Signed_69 is range -2 ** 68 .. 2 ** 68 - 1;
   for Signed_69'Size use 69;

   type Signed_70 is range -2 ** 69 .. 2 ** 69 - 1;
   for Signed_70'Size use 70;

   type Signed_71 is range -2 ** 70 .. 2 ** 70 - 1;
   for Signed_71'Size use 71;

   type Signed_72 is range -2 ** 71 .. 2 ** 71 - 1;
   for Signed_72'Size use 72;

   type Signed_73 is range -2 ** 72 .. 2 ** 72 - 1;
   for Signed_73'Size use 73;

   type Signed_74 is range -2 ** 73 .. 2 ** 73 - 1;
   for Signed_74'Size use 74;

   type Signed_75 is range -2 ** 74 .. 2 ** 74 - 1;
   for Signed_75'Size use 75;

   type Signed_76 is range -2 ** 75 .. 2 ** 75 - 1;
   for Signed_76'Size use 76;

   type Signed_77 is range -2 ** 76 .. 2 ** 76 - 1;
   for Signed_77'Size use 77;

   type Signed_78 is range -2 ** 77 .. 2 ** 77 - 1;
   for Signed_78'Size use 78;

   type Signed_79 is range -2 ** 78 .. 2 ** 78 - 1;
   for Signed_79'Size use 79;

   type Signed_80 is range -2 ** 79 .. 2 ** 79 - 1;
   for Signed_80'Size use 80;

   type Signed_81 is range -2 ** 80 .. 2 ** 80 - 1;
   for Signed_81'Size use 81;

   type Signed_82 is range -2 ** 81 .. 2 ** 81 - 1;
   for Signed_82'Size use 82;

   type Signed_83 is range -2 ** 82 .. 2 ** 82 - 1;
   for Signed_83'Size use 83;

   type Signed_84 is range -2 ** 83 .. 2 ** 83 - 1;
   for Signed_84'Size use 84;

   type Signed_85 is range -2 ** 84 .. 2 ** 84 - 1;
   for Signed_85'Size use 85;

   type Signed_86 is range -2 ** 85 .. 2 ** 85 - 1;
   for Signed_86'Size use 86;

   type Signed_87 is range -2 ** 86 .. 2 ** 86 - 1;
   for Signed_87'Size use 87;

   type Signed_88 is range -2 ** 87 .. 2 ** 87 - 1;
   for Signed_88'Size use 88;

   type Signed_89 is range -2 ** 88 .. 2 ** 88 - 1;
   for Signed_89'Size use 89;

   type Signed_90 is range -2 ** 89 .. 2 ** 89 - 1;
   for Signed_90'Size use 90;

   type Signed_91 is range -2 ** 90 .. 2 ** 90 - 1;
   for Signed_91'Size use 91;

   type Signed_92 is range -2 ** 91 .. 2 ** 91 - 1;
   for Signed_92'Size use 92;

   type Signed_93 is range -2 ** 92 .. 2 ** 92 - 1;
   for Signed_93'Size use 93;

   type Signed_94 is range -2 ** 93 .. 2 ** 93 - 1;
   for Signed_94'Size use 94;

   type Signed_95 is range -2 ** 94 .. 2 ** 94 - 1;
   for Signed_95'Size use 95;

   type Signed_96 is range -2 ** 95 .. 2 ** 95 - 1;
   for Signed_96'Size use 96;

   type Signed_97 is range -2 ** 96 .. 2 ** 96 - 1;
   for Signed_97'Size use 97;

   type Signed_98 is range -2 ** 97 .. 2 ** 97 - 1;
   for Signed_98'Size use 98;

   type Signed_99 is range -2 ** 98 .. 2 ** 98 - 1;
   for Signed_99'Size use 99;

   type Signed_100 is range -2 ** 99 .. 2 ** 99 - 1;
   for Signed_100'Size use 100;

   type Signed_101 is range -2 ** 100 .. 2 ** 100 - 1;
   for Signed_101'Size use 101;

   type Signed_102 is range -2 ** 101 .. 2 ** 101 - 1;
   for Signed_102'Size use 102;

   type Signed_103 is range -2 ** 102 .. 2 ** 102 - 1;
   for Signed_103'Size use 103;

   type Signed_104 is range -2 ** 103 .. 2 ** 103 - 1;
   for Signed_104'Size use 104;

   type Signed_105 is range -2 ** 104 .. 2 ** 104 - 1;
   for Signed_105'Size use 105;

   type Signed_106 is range -2 ** 105 .. 2 ** 105 - 1;
   for Signed_106'Size use 106;

   type Signed_107 is range -2 ** 106 .. 2 ** 106 - 1;
   for Signed_107'Size use 107;

   type Signed_108 is range -2 ** 107 .. 2 ** 107 - 1;
   for Signed_108'Size use 108;

   type Signed_109 is range -2 ** 108 .. 2 ** 108 - 1;
   for Signed_109'Size use 109;

   type Signed_110 is range -2 ** 109 .. 2 ** 109 - 1;
   for Signed_110'Size use 110;

   type Signed_111 is range -2 ** 110 .. 2 ** 110 - 1;
   for Signed_111'Size use 111;

   type Signed_112 is range -2 ** 111 .. 2 ** 111 - 1;
   for Signed_112'Size use 112;

   type Signed_113 is range -2 ** 112 .. 2 ** 112 - 1;
   for Signed_113'Size use 113;

   type Signed_114 is range -2 ** 113 .. 2 ** 113 - 1;
   for Signed_114'Size use 114;

   type Signed_115 is range -2 ** 114 .. 2 ** 114 - 1;
   for Signed_115'Size use 115;

   type Signed_116 is range -2 ** 115 .. 2 ** 115 - 1;
   for Signed_116'Size use 116;

   type Signed_117 is range -2 ** 116 .. 2 ** 116 - 1;
   for Signed_117'Size use 117;

   type Signed_118 is range -2 ** 117 .. 2 ** 117 - 1;
   for Signed_118'Size use 118;

   type Signed_119 is range -2 ** 118 .. 2 ** 118 - 1;
   for Signed_119'Size use 119;

   type Signed_120 is range -2 ** 119 .. 2 ** 119 - 1;
   for Signed_120'Size use 120;

   type Signed_121 is range -2 ** 120 .. 2 ** 120 - 1;
   for Signed_121'Size use 121;

   type Signed_122 is range -2 ** 121 .. 2 ** 121 - 1;
   for Signed_122'Size use 122;

   type Signed_123 is range -2 ** 122 .. 2 ** 122 - 1;
   for Signed_123'Size use 123;

   type Signed_124 is range -2 ** 123 .. 2 ** 123 - 1;
   for Signed_124'Size use 124;

   type Signed_125 is range -2 ** 124 .. 2 ** 124 - 1;
   for Signed_125'Size use 125;

   type Signed_126 is range -2 ** 125 .. 2 ** 125 - 1;
   for Signed_126'Size use 126;

   type Signed_127 is range -2 ** 126 .. 2 ** 126 - 1;
   for Signed_127'Size use 127;

   type Signed_128 is range -2 ** 127 .. 2 ** 127 - 1;
   for Signed_128'Size use 128;

end Interfaces.C.Extensions;
