------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P A K D                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Expand routines for manipulation of packed arrays

with Rtsfind; use Rtsfind;
with Types;   use Types;

package Exp_Pakd is

   -------------------------------------
   -- Implementation of Packed Arrays --
   -------------------------------------

   --  When a packed array (sub)type is frozen, we create a corresponding
   --  type that will be used to hold the bits of the packed value, and store
   --  the entity for this type in the Packed_Array_Impl_Type field of the
   --  E_Array_Type or E_Array_Subtype entity for the packed array.

   --  This packed array type has the name xxxPn, where xxx is the name
   --  of the packed type, and n is the component size. The expanded
   --  declaration declares a type that is one of the following (sizes
   --  below are in bytes):

   --    For an unconstrained array with component size 1,2,4 or any other
   --    odd component size. These are the cases in which we do not need
   --    to align the underlying array.

   --      type xxxPn is new Packed_Bytes1;

   --    For an unconstrained array with component size greater than 2, that is
   --    divisible by 2, but not divisible by 4. These are the cases in which
   --    we can generate better code if the underlying array is 2-byte aligned
   --    (see System.Pack_14 in file s-pack14 for example).

   --      type xxxPn is new Packed_Bytes2;

   --    For an unconstrained array with component size that is divisible
   --    by 4, other than powers of 2 (which either come under the 1,2,4
   --    exception above, or are not packed at all). These are cases where
   --    we can generate better code if the underlying array is 4-byte
   --    aligned (see System.Pack_20 in file s-pack20 for example).

   --      type xxxPn is new Packed_Bytes4;

   --    For a constrained array with a static index type where the number
   --    of bits does not exceed the size of Unsigned:

   --      type xxxPn is new Unsigned range 0 .. 2 ** nbits - 1;

   --    For a constrained array with a static index type where the number
   --    of bits is greater than the size of Unsigned, but does not exceed
   --    the size of Long_Long_Unsigned:

   --       type xxxPn is new Long_Long_Unsigned range 0 .. 2 ** nbits - 1;

   --    For all other constrained arrays, we use one of

   --       type xxxPn is new Packed_Bytes1 (0 .. m);
   --       type xxxPn is new Packed_Bytes2 (0 .. m);
   --       type xxxPn is new Packed_Bytes4 (0 .. m);

   --    where m is calculated (from the length of the original packed array)
   --    to hold the required number of bits, and the choice of the particular
   --    Packed_Bytes{1,2,4} type is made on the basis of alignment needs as
   --    described above for the unconstrained case.

   --  When the packed array (sub)type is specified to have the reverse scalar
   --  storage order, the Packed_Bytes{1,2,4} references above are replaced
   --  with Rev_Packed_Bytes{1,2,4}. This is necessary because, although the
   --  component type is Packed_Byte and therefore endian neutral, the scalar
   --  storage order of the new type must be compatible with that of an outer
   --  composite type, if this composite type contains a component whose type
   --  is the packed array (sub)type and which does not start or does not end
   --  on a storage unit boundary.

   --  When a variable of packed array type is allocated, gigi will allocate
   --  the amount of space indicated by the corresponding packed array type.
   --  However, we do NOT attempt to rewrite the types of any references or
   --  to retype the variable itself, since this would cause all kinds of
   --  semantic problems in the front end (remember that expansion proceeds
   --  at the same time as analysis).

   --  For an indexed reference to a packed array, we simply convert the
   --  reference to the appropriate equivalent reference to the object
   --  of the packed array type (using unchecked conversion).

   --  In some cases (for internally generated types, and for the subtypes
   --  for record fields that depend on a discriminant), the corresponding
   --  packed type cannot be easily generated in advance. In these cases,
   --  we generate the required subtype on the fly at the reference point.

   --  For the modular case, any unused bits are initialized to zero, and
   --  all operations maintain these bits as zero (where necessary all
   --  unchecked conversions from corresponding array values require
   --  these bits to be clear, which is done automatically by gigi).

   --  For the array cases, there can be unused bits in the last byte, and
   --  these are neither initialized, nor treated specially in operations
   --  (i.e. it is allowable for these bits to be clobbered, e.g. by not).

   ---------------------------
   -- Endian Considerations --
   ---------------------------

   --  The standard does not specify the way in which bits are numbered in
   --  a packed array. There are two reasonable rules for deciding this:

   --    Store the first bit at right end (low order) word. This means
   --    that the scaled subscript can be used directly as a left shift
   --    count (if we put bit 0 at the left end, then we need an extra
   --    subtract to compute the shift count).

   --    Layout the bits so that if the packed boolean array is overlaid on
   --    a record, using unchecked conversion, then bit 0 of the array is
   --    the same as the bit numbered bit 0 in a record representation
   --    clause applying to the record. For example:

   --       type Rec is record
   --          C : Bits4;
   --          D : Bits7;
   --          E : Bits5;
   --       end record;

   --       for Rec use record
   --          C at 0 range  0  .. 3;
   --          D at 0 range  4 .. 10;
   --          E at 0 range 11 .. 15;
   --       end record;

   --       type P16 is array (0 .. 15) of Boolean;
   --       pragma Pack (P16);

   --    Now if we use unchecked conversion to convert a value of the record
   --    type to the packed array type, according to this second criterion,
   --    we would expect field D to occupy bits 4..10 of the Boolean array.

   --  Although not required, this correspondence seems a highly desirable
   --  property, and is one that GNAT decides to guarantee. For a little
   --  endian machine, we can also meet the first requirement, but for a
   --  big endian machine, it will be necessary to store the first bit of
   --  a Boolean array in the left end (most significant) bit of the word.
   --  This may cost an extra instruction on some machines, but we consider
   --  that a worthwhile price to pay for the consistency.

   --  One more important point arises in the case where we have a constrained
   --  subtype of an unconstrained array. Take the case of 20 bits. For the
   --  unconstrained representation, we would use an array of bytes:

   --     Little-endian case
   --       8-7-6-5-4-3-2-1  16-15-14-13-12-11-10-9  x-x-x-x-20-19-18-17

   --     Big-endian case
   --       1-2-3-4-5-6-7-8  9-10-11-12-13-14-15-16  17-18-19-20-x-x-x-x

   --   For the constrained case, we use a 20-bit modular value, but in
   --   general this value may well be stored in 32 bits. Let's look at
   --   what it looks like:

   --     Little-endian case

   --       x-x-x-x-x-x-x-x-x-x-x-x-20-19-18-17-...-10-9-8-7-6-5-4-3-2-1

   --         which stored in memory looks like

   --       8-7-...-2-1  16-15-...-10-9  x-x-x-x-20-19-18-17  x-x-x-x-x-x-x

   --   An important rule is that the constrained and unconstrained cases
   --   must have the same bit representation in memory, since we will often
   --   convert from one to the other (e.g. when calling a procedure whose
   --   formal is unconstrained). As we see, that criterion is met for the
   --   little-endian case above. Now let's look at the big-endian case:

   --     Big-endian case

   --       x-x-x-x-x-x-x-x-x-x-x-x-1-2-3-4-5-6-7-8-9-10-...-17-18-19-20

   --         which stored in memory looks like

   --       x-x-x-x-x-x-x-x  x-x-x-x-1-2-3-4  5-6-...11-12  13-14-...-19-20

   --   That won't do, the representation value in memory is NOT the same in
   --   the constrained and unconstrained case. The solution is to store the
   --   modular value left-justified:

   --       1-2-3-4-5-6-7-8-9-10-...-17-18-19-20-x-x-x-x-x-x-x-x-x-x-x

   --         which stored in memory looks like

   --       1-2-...-7-8  9-10-...15-16  17-18-19-20-x-x-x-x  x-x-x-x-x-x-x-x

   --   and now, we do indeed have the same representation for the memory
   --   version in the constrained and unconstrained cases.

   ----------------------------------------------
   -- Entity Tables for Packed Access Routines --
   ----------------------------------------------

   --  For the cases of component size = 3,5-7,9-15,17-31,33-63,65-127 we call
   --  library routines. These tables provide the entity for the right routine.
   --  They are exposed in the spec to allow checking for the presence of the
   --  needed routine when an array is subject to pragma Pack.

   type E_Array is array (Int range 1 .. 127) of RE_Id;

   --  Array of Bits_nn entities. Note that we do not use library routines
   --  for the 8-bit and 16-bit cases, but we still fill in the table, using
   --  entries from System.Unsigned, because we also use this table for
   --  certain special unchecked conversions in the big-endian case.

   Bits_Id : constant E_Array :=
     (01 => RE_Bits_1,
      02 => RE_Bits_2,
      03 => RE_Bits_03,
      04 => RE_Bits_4,
      05 => RE_Bits_05,
      06 => RE_Bits_06,
      07 => RE_Bits_07,
      08 => RE_Unsigned_8,
      09 => RE_Bits_09,
      10 => RE_Bits_10,
      11 => RE_Bits_11,
      12 => RE_Bits_12,
      13 => RE_Bits_13,
      14 => RE_Bits_14,
      15 => RE_Bits_15,
      16 => RE_Unsigned_16,
      17 => RE_Bits_17,
      18 => RE_Bits_18,
      19 => RE_Bits_19,
      20 => RE_Bits_20,
      21 => RE_Bits_21,
      22 => RE_Bits_22,
      23 => RE_Bits_23,
      24 => RE_Bits_24,
      25 => RE_Bits_25,
      26 => RE_Bits_26,
      27 => RE_Bits_27,
      28 => RE_Bits_28,
      29 => RE_Bits_29,
      30 => RE_Bits_30,
      31 => RE_Bits_31,
      32 => RE_Unsigned_32,
      33 => RE_Bits_33,
      34 => RE_Bits_34,
      35 => RE_Bits_35,
      36 => RE_Bits_36,
      37 => RE_Bits_37,
      38 => RE_Bits_38,
      39 => RE_Bits_39,
      40 => RE_Bits_40,
      41 => RE_Bits_41,
      42 => RE_Bits_42,
      43 => RE_Bits_43,
      44 => RE_Bits_44,
      45 => RE_Bits_45,
      46 => RE_Bits_46,
      47 => RE_Bits_47,
      48 => RE_Bits_48,
      49 => RE_Bits_49,
      50 => RE_Bits_50,
      51 => RE_Bits_51,
      52 => RE_Bits_52,
      53 => RE_Bits_53,
      54 => RE_Bits_54,
      55 => RE_Bits_55,
      56 => RE_Bits_56,
      57 => RE_Bits_57,
      58 => RE_Bits_58,
      59 => RE_Bits_59,
      60 => RE_Bits_60,
      61 => RE_Bits_61,
      62 => RE_Bits_62,
      63 => RE_Bits_63,
      64 => RE_Unsigned_64,
      65 => RE_Bits_65,
      66 => RE_Bits_66,
      67 => RE_Bits_67,
      68 => RE_Bits_68,
      69 => RE_Bits_69,
      70 => RE_Bits_70,
      71 => RE_Bits_71,
      72 => RE_Bits_72,
      73 => RE_Bits_73,
      74 => RE_Bits_74,
      75 => RE_Bits_75,
      76 => RE_Bits_76,
      77 => RE_Bits_77,
      78 => RE_Bits_78,
      79 => RE_Bits_79,
      80 => RE_Bits_80,
      81 => RE_Bits_81,
      82 => RE_Bits_82,
      83 => RE_Bits_83,
      84 => RE_Bits_84,
      85 => RE_Bits_85,
      86 => RE_Bits_86,
      87 => RE_Bits_87,
      88 => RE_Bits_88,
      89 => RE_Bits_89,
      90 => RE_Bits_90,
      91 => RE_Bits_91,
      92 => RE_Bits_92,
      93 => RE_Bits_93,
      94 => RE_Bits_94,
      95 => RE_Bits_95,
      96 => RE_Bits_96,
      97 => RE_Bits_97,
      98 => RE_Bits_98,
      99 => RE_Bits_99,
      100 => RE_Bits_100,
      101 => RE_Bits_101,
      102 => RE_Bits_102,
      103 => RE_Bits_103,
      104 => RE_Bits_104,
      105 => RE_Bits_105,
      106 => RE_Bits_106,
      107 => RE_Bits_107,
      108 => RE_Bits_108,
      109 => RE_Bits_109,
      110 => RE_Bits_110,
      111 => RE_Bits_111,
      112 => RE_Bits_112,
      113 => RE_Bits_113,
      114 => RE_Bits_114,
      115 => RE_Bits_115,
      116 => RE_Bits_116,
      117 => RE_Bits_117,
      118 => RE_Bits_118,
      119 => RE_Bits_119,
      120 => RE_Bits_120,
      121 => RE_Bits_121,
      122 => RE_Bits_122,
      123 => RE_Bits_123,
      124 => RE_Bits_124,
      125 => RE_Bits_125,
      126 => RE_Bits_126,
      127 => RE_Bits_127);

   --  Array of Get routine entities. These are used to obtain an element from
   --  a packed array. The N'th entry is used to obtain elements from a packed
   --  array whose component size is N. RE_Null is used as a null entry, for
   --  the cases where a library routine is not used.

   Get_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Get_03,
      04 => RE_Null,
      05 => RE_Get_05,
      06 => RE_Get_06,
      07 => RE_Get_07,
      08 => RE_Null,
      09 => RE_Get_09,
      10 => RE_Get_10,
      11 => RE_Get_11,
      12 => RE_Get_12,
      13 => RE_Get_13,
      14 => RE_Get_14,
      15 => RE_Get_15,
      16 => RE_Null,
      17 => RE_Get_17,
      18 => RE_Get_18,
      19 => RE_Get_19,
      20 => RE_Get_20,
      21 => RE_Get_21,
      22 => RE_Get_22,
      23 => RE_Get_23,
      24 => RE_Get_24,
      25 => RE_Get_25,
      26 => RE_Get_26,
      27 => RE_Get_27,
      28 => RE_Get_28,
      29 => RE_Get_29,
      30 => RE_Get_30,
      31 => RE_Get_31,
      32 => RE_Null,
      33 => RE_Get_33,
      34 => RE_Get_34,
      35 => RE_Get_35,
      36 => RE_Get_36,
      37 => RE_Get_37,
      38 => RE_Get_38,
      39 => RE_Get_39,
      40 => RE_Get_40,
      41 => RE_Get_41,
      42 => RE_Get_42,
      43 => RE_Get_43,
      44 => RE_Get_44,
      45 => RE_Get_45,
      46 => RE_Get_46,
      47 => RE_Get_47,
      48 => RE_Get_48,
      49 => RE_Get_49,
      50 => RE_Get_50,
      51 => RE_Get_51,
      52 => RE_Get_52,
      53 => RE_Get_53,
      54 => RE_Get_54,
      55 => RE_Get_55,
      56 => RE_Get_56,
      57 => RE_Get_57,
      58 => RE_Get_58,
      59 => RE_Get_59,
      60 => RE_Get_60,
      61 => RE_Get_61,
      62 => RE_Get_62,
      63 => RE_Get_63,
      64 => RE_Null,
      65 => RE_Get_65,
      66 => RE_Get_66,
      67 => RE_Get_67,
      68 => RE_Get_68,
      69 => RE_Get_69,
      70 => RE_Get_70,
      71 => RE_Get_71,
      72 => RE_Get_72,
      73 => RE_Get_73,
      74 => RE_Get_74,
      75 => RE_Get_75,
      76 => RE_Get_76,
      77 => RE_Get_77,
      78 => RE_Get_78,
      79 => RE_Get_79,
      80 => RE_Get_80,
      81 => RE_Get_81,
      82 => RE_Get_82,
      83 => RE_Get_83,
      84 => RE_Get_84,
      85 => RE_Get_85,
      86 => RE_Get_86,
      87 => RE_Get_87,
      88 => RE_Get_88,
      89 => RE_Get_89,
      90 => RE_Get_90,
      91 => RE_Get_91,
      92 => RE_Get_92,
      93 => RE_Get_93,
      94 => RE_Get_94,
      95 => RE_Get_95,
      96 => RE_Get_96,
      97 => RE_Get_97,
      98 => RE_Get_98,
      99 => RE_Get_99,
      100 => RE_Get_100,
      101 => RE_Get_101,
      102 => RE_Get_102,
      103 => RE_Get_103,
      104 => RE_Get_104,
      105 => RE_Get_105,
      106 => RE_Get_106,
      107 => RE_Get_107,
      108 => RE_Get_108,
      109 => RE_Get_109,
      110 => RE_Get_110,
      111 => RE_Get_111,
      112 => RE_Get_112,
      113 => RE_Get_113,
      114 => RE_Get_114,
      115 => RE_Get_115,
      116 => RE_Get_116,
      117 => RE_Get_117,
      118 => RE_Get_118,
      119 => RE_Get_119,
      120 => RE_Get_120,
      121 => RE_Get_121,
      122 => RE_Get_122,
      123 => RE_Get_123,
      124 => RE_Get_124,
      125 => RE_Get_125,
      126 => RE_Get_126,
      127 => RE_Get_127);

   --  Array of Get routine entities to be used in the case where the packed
   --  array is itself a component of a packed structure, and therefore may not
   --  be fully aligned. This only affects the even sizes, since for the odd
   --  sizes, we do not get any fixed alignment in any case.

   GetU_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Get_03,
      04 => RE_Null,
      05 => RE_Get_05,
      06 => RE_GetU_06,
      07 => RE_Get_07,
      08 => RE_Null,
      09 => RE_Get_09,
      10 => RE_GetU_10,
      11 => RE_Get_11,
      12 => RE_GetU_12,
      13 => RE_Get_13,
      14 => RE_GetU_14,
      15 => RE_Get_15,
      16 => RE_Null,
      17 => RE_Get_17,
      18 => RE_GetU_18,
      19 => RE_Get_19,
      20 => RE_GetU_20,
      21 => RE_Get_21,
      22 => RE_GetU_22,
      23 => RE_Get_23,
      24 => RE_GetU_24,
      25 => RE_Get_25,
      26 => RE_GetU_26,
      27 => RE_Get_27,
      28 => RE_GetU_28,
      29 => RE_Get_29,
      30 => RE_GetU_30,
      31 => RE_Get_31,
      32 => RE_Null,
      33 => RE_Get_33,
      34 => RE_GetU_34,
      35 => RE_Get_35,
      36 => RE_GetU_36,
      37 => RE_Get_37,
      38 => RE_GetU_38,
      39 => RE_Get_39,
      40 => RE_GetU_40,
      41 => RE_Get_41,
      42 => RE_GetU_42,
      43 => RE_Get_43,
      44 => RE_GetU_44,
      45 => RE_Get_45,
      46 => RE_GetU_46,
      47 => RE_Get_47,
      48 => RE_GetU_48,
      49 => RE_Get_49,
      50 => RE_GetU_50,
      51 => RE_Get_51,
      52 => RE_GetU_52,
      53 => RE_Get_53,
      54 => RE_GetU_54,
      55 => RE_Get_55,
      56 => RE_GetU_56,
      57 => RE_Get_57,
      58 => RE_GetU_58,
      59 => RE_Get_59,
      60 => RE_GetU_60,
      61 => RE_Get_61,
      62 => RE_GetU_62,
      63 => RE_Get_63,
      64 => RE_Null,
      65 => RE_Get_65,
      66 => RE_GetU_66,
      67 => RE_Get_67,
      68 => RE_GetU_68,
      69 => RE_Get_69,
      70 => RE_GetU_70,
      71 => RE_Get_71,
      72 => RE_GetU_72,
      73 => RE_Get_73,
      74 => RE_GetU_74,
      75 => RE_Get_75,
      76 => RE_GetU_76,
      77 => RE_Get_77,
      78 => RE_GetU_78,
      79 => RE_Get_79,
      80 => RE_GetU_80,
      81 => RE_Get_81,
      82 => RE_GetU_82,
      83 => RE_Get_83,
      84 => RE_GetU_84,
      85 => RE_Get_85,
      86 => RE_GetU_86,
      87 => RE_Get_87,
      88 => RE_GetU_88,
      89 => RE_Get_89,
      90 => RE_GetU_90,
      91 => RE_Get_91,
      92 => RE_GetU_92,
      93 => RE_Get_93,
      94 => RE_GetU_94,
      95 => RE_Get_95,
      96 => RE_GetU_96,
      97 => RE_Get_97,
      98 => RE_GetU_98,
      99 => RE_Get_99,
      100 => RE_GetU_100,
      101 => RE_Get_101,
      102 => RE_GetU_102,
      103 => RE_Get_103,
      104 => RE_GetU_104,
      105 => RE_Get_105,
      106 => RE_GetU_106,
      107 => RE_Get_107,
      108 => RE_GetU_108,
      109 => RE_Get_109,
      110 => RE_GetU_110,
      111 => RE_Get_111,
      112 => RE_GetU_112,
      113 => RE_Get_113,
      114 => RE_GetU_114,
      115 => RE_Get_115,
      116 => RE_GetU_116,
      117 => RE_Get_117,
      118 => RE_GetU_118,
      119 => RE_Get_119,
      120 => RE_GetU_120,
      121 => RE_Get_121,
      122 => RE_GetU_122,
      123 => RE_Get_123,
      124 => RE_GetU_124,
      125 => RE_Get_125,
      126 => RE_GetU_126,
      127 => RE_Get_127);

   --  Array of Set routine entities. These are used to assign an element of a
   --  packed array. The N'th entry is used to assign elements for a packed
   --  array whose component size is N. RE_Null is used as a null entry, for
   --  the cases where a library routine is not used.

   Set_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Set_03,
      04 => RE_Null,
      05 => RE_Set_05,
      06 => RE_Set_06,
      07 => RE_Set_07,
      08 => RE_Null,
      09 => RE_Set_09,
      10 => RE_Set_10,
      11 => RE_Set_11,
      12 => RE_Set_12,
      13 => RE_Set_13,
      14 => RE_Set_14,
      15 => RE_Set_15,
      16 => RE_Null,
      17 => RE_Set_17,
      18 => RE_Set_18,
      19 => RE_Set_19,
      20 => RE_Set_20,
      21 => RE_Set_21,
      22 => RE_Set_22,
      23 => RE_Set_23,
      24 => RE_Set_24,
      25 => RE_Set_25,
      26 => RE_Set_26,
      27 => RE_Set_27,
      28 => RE_Set_28,
      29 => RE_Set_29,
      30 => RE_Set_30,
      31 => RE_Set_31,
      32 => RE_Null,
      33 => RE_Set_33,
      34 => RE_Set_34,
      35 => RE_Set_35,
      36 => RE_Set_36,
      37 => RE_Set_37,
      38 => RE_Set_38,
      39 => RE_Set_39,
      40 => RE_Set_40,
      41 => RE_Set_41,
      42 => RE_Set_42,
      43 => RE_Set_43,
      44 => RE_Set_44,
      45 => RE_Set_45,
      46 => RE_Set_46,
      47 => RE_Set_47,
      48 => RE_Set_48,
      49 => RE_Set_49,
      50 => RE_Set_50,
      51 => RE_Set_51,
      52 => RE_Set_52,
      53 => RE_Set_53,
      54 => RE_Set_54,
      55 => RE_Set_55,
      56 => RE_Set_56,
      57 => RE_Set_57,
      58 => RE_Set_58,
      59 => RE_Set_59,
      60 => RE_Set_60,
      61 => RE_Set_61,
      62 => RE_Set_62,
      63 => RE_Set_63,
      64 => RE_Null,
      65 => RE_Set_65,
      66 => RE_Set_66,
      67 => RE_Set_67,
      68 => RE_Set_68,
      69 => RE_Set_69,
      70 => RE_Set_70,
      71 => RE_Set_71,
      72 => RE_Set_72,
      73 => RE_Set_73,
      74 => RE_Set_74,
      75 => RE_Set_75,
      76 => RE_Set_76,
      77 => RE_Set_77,
      78 => RE_Set_78,
      79 => RE_Set_79,
      80 => RE_Set_80,
      81 => RE_Set_81,
      82 => RE_Set_82,
      83 => RE_Set_83,
      84 => RE_Set_84,
      85 => RE_Set_85,
      86 => RE_Set_86,
      87 => RE_Set_87,
      88 => RE_Set_88,
      89 => RE_Set_89,
      90 => RE_Set_90,
      91 => RE_Set_91,
      92 => RE_Set_92,
      93 => RE_Set_93,
      94 => RE_Set_94,
      95 => RE_Set_95,
      96 => RE_Set_96,
      97 => RE_Set_97,
      98 => RE_Set_98,
      99 => RE_Set_99,
      100 => RE_Set_100,
      101 => RE_Set_101,
      102 => RE_Set_102,
      103 => RE_Set_103,
      104 => RE_Set_104,
      105 => RE_Set_105,
      106 => RE_Set_106,
      107 => RE_Set_107,
      108 => RE_Set_108,
      109 => RE_Set_109,
      110 => RE_Set_110,
      111 => RE_Set_111,
      112 => RE_Set_112,
      113 => RE_Set_113,
      114 => RE_Set_114,
      115 => RE_Set_115,
      116 => RE_Set_116,
      117 => RE_Set_117,
      118 => RE_Set_118,
      119 => RE_Set_119,
      120 => RE_Set_120,
      121 => RE_Set_121,
      122 => RE_Set_122,
      123 => RE_Set_123,
      124 => RE_Set_124,
      125 => RE_Set_125,
      126 => RE_Set_126,
      127 => RE_Set_127);

   --  Array of Set routine entities to be used in the case where the packed
   --  array is itself a component of a packed structure, and therefore may not
   --  be fully aligned. This only affects the even sizes, since for the odd
   --  sizes, we do not get any fixed alignment in any case.

   SetU_Id : constant E_Array :=
     (01 => RE_Null,
      02 => RE_Null,
      03 => RE_Set_03,
      04 => RE_Null,
      05 => RE_Set_05,
      06 => RE_SetU_06,
      07 => RE_Set_07,
      08 => RE_Null,
      09 => RE_Set_09,
      10 => RE_SetU_10,
      11 => RE_Set_11,
      12 => RE_SetU_12,
      13 => RE_Set_13,
      14 => RE_SetU_14,
      15 => RE_Set_15,
      16 => RE_Null,
      17 => RE_Set_17,
      18 => RE_SetU_18,
      19 => RE_Set_19,
      20 => RE_SetU_20,
      21 => RE_Set_21,
      22 => RE_SetU_22,
      23 => RE_Set_23,
      24 => RE_SetU_24,
      25 => RE_Set_25,
      26 => RE_SetU_26,
      27 => RE_Set_27,
      28 => RE_SetU_28,
      29 => RE_Set_29,
      30 => RE_SetU_30,
      31 => RE_Set_31,
      32 => RE_Null,
      33 => RE_Set_33,
      34 => RE_SetU_34,
      35 => RE_Set_35,
      36 => RE_SetU_36,
      37 => RE_Set_37,
      38 => RE_SetU_38,
      39 => RE_Set_39,
      40 => RE_SetU_40,
      41 => RE_Set_41,
      42 => RE_SetU_42,
      43 => RE_Set_43,
      44 => RE_SetU_44,
      45 => RE_Set_45,
      46 => RE_SetU_46,
      47 => RE_Set_47,
      48 => RE_SetU_48,
      49 => RE_Set_49,
      50 => RE_SetU_50,
      51 => RE_Set_51,
      52 => RE_SetU_52,
      53 => RE_Set_53,
      54 => RE_SetU_54,
      55 => RE_Set_55,
      56 => RE_SetU_56,
      57 => RE_Set_57,
      58 => RE_SetU_58,
      59 => RE_Set_59,
      60 => RE_SetU_60,
      61 => RE_Set_61,
      62 => RE_SetU_62,
      63 => RE_Set_63,
      64 => RE_Null,
      65 => RE_Set_65,
      66 => RE_SetU_66,
      67 => RE_Set_67,
      68 => RE_SetU_68,
      69 => RE_Set_69,
      70 => RE_SetU_70,
      71 => RE_Set_71,
      72 => RE_SetU_72,
      73 => RE_Set_73,
      74 => RE_SetU_74,
      75 => RE_Set_75,
      76 => RE_SetU_76,
      77 => RE_Set_77,
      78 => RE_SetU_78,
      79 => RE_Set_79,
      80 => RE_SetU_80,
      81 => RE_Set_81,
      82 => RE_SetU_82,
      83 => RE_Set_83,
      84 => RE_SetU_84,
      85 => RE_Set_85,
      86 => RE_SetU_86,
      87 => RE_Set_87,
      88 => RE_SetU_88,
      89 => RE_Set_89,
      90 => RE_SetU_90,
      91 => RE_Set_91,
      92 => RE_SetU_92,
      93 => RE_Set_93,
      94 => RE_SetU_94,
      95 => RE_Set_95,
      96 => RE_SetU_96,
      97 => RE_Set_97,
      98 => RE_SetU_98,
      99 => RE_Set_99,
      100 => RE_SetU_100,
      101 => RE_Set_101,
      102 => RE_SetU_102,
      103 => RE_Set_103,
      104 => RE_SetU_104,
      105 => RE_Set_105,
      106 => RE_SetU_106,
      107 => RE_Set_107,
      108 => RE_SetU_108,
      109 => RE_Set_109,
      110 => RE_SetU_110,
      111 => RE_Set_111,
      112 => RE_SetU_112,
      113 => RE_Set_113,
      114 => RE_SetU_114,
      115 => RE_Set_115,
      116 => RE_SetU_116,
      117 => RE_Set_117,
      118 => RE_SetU_118,
      119 => RE_Set_119,
      120 => RE_SetU_120,
      121 => RE_Set_121,
      122 => RE_SetU_122,
      123 => RE_Set_123,
      124 => RE_SetU_124,
      125 => RE_Set_125,
      126 => RE_SetU_126,
      127 => RE_Set_127);

   -----------------
   -- Subprograms --
   -----------------

   procedure Create_Packed_Array_Impl_Type (Typ  : Entity_Id);
   --  Typ is a array type or subtype to which pragma Pack applies. If the
   --  Packed_Array_Impl_Type field of Typ is already set, then the call has
   --  no effect, otherwise a suitable type or subtype is created and stored in
   --  the Packed_Array_Impl_Type field of Typ. This created type is an Itype
   --  so that Gigi will simply elaborate and freeze the type on first use
   --  (which is typically the definition of the corresponding array type).
   --
   --  Note: although this routine is included in the expander package for
   --  packed types, it is actually called unconditionally from Freeze,
   --  whether or not expansion (and code generation) is enabled. We do this
   --  since we want gigi to be able to properly compute type characteristics
   --  (for the Data Decomposition Annex of ASIS, and possible other future
   --  uses) even if code generation is not active. Strictly this means that
   --  this procedure is not part of the expander, but it seems appropriate
   --  to keep it together with the other expansion routines that have to do
   --  with packed array types.

   procedure Expand_Packed_Boolean_Operator (N : Node_Id);
   --  N is an N_Op_And, N_Op_Or or N_Op_Xor node whose operand type is a
   --  packed boolean array. This routine expands the appropriate operations
   --  to carry out the logical operation on the packed arrays. It handles
   --  both the modular and array representation cases.

   procedure Expand_Packed_Element_Reference (N : Node_Id);
   --  N is an N_Indexed_Component node whose prefix is a packed array. In
   --  the bit packed case, this routine can only be used for the expression
   --  evaluation case, not the assignment case, since the result is not a
   --  variable. See Expand_Bit_Packed_Element_Set for how the assignment case
   --  is handled in the bit packed case. For the enumeration case, the result
   --  of this call is always a variable, so the call can be used for both the
   --  expression evaluation and assignment cases.

   procedure Expand_Bit_Packed_Element_Set (N : Node_Id);
   --  N is an N_Assignment_Statement node whose name is an indexed
   --  component of a bit-packed array. This procedure rewrites the entire
   --  assignment statement with appropriate code to set the referenced
   --  bits of the packed array type object. Note that this procedure is
   --  used only for the bit-packed case, not for the enumeration case.

   procedure Expand_Packed_Eq (N : Node_Id);
   --  N is an N_Op_Eq node where the operands are packed arrays whose
   --  representation is an array-of-bytes type (the case where a modular
   --  type is used for the representation does not require any special
   --  handling, because in the modular case, unused bits are zeroes).

   procedure Expand_Packed_Not (N : Node_Id);
   --  N is an N_Op_Not node where the operand is packed array of Boolean
   --  in standard representation (i.e. component size is one bit). This
   --  procedure expands the corresponding not operation. Note that the
   --  non-standard representation case is handled by using a loop through
   --  elements generated by the normal non-packed circuitry.

   function Involves_Packed_Array_Reference (N : Node_Id) return Boolean;
   --  N is the node for a name. This function returns true if the name
   --  involves a packed array reference. A node involves a packed array
   --  reference if it is itself an indexed component referring to a bit-
   --  packed array, or it is a selected component whose prefix involves
   --  a packed array reference.

   procedure Expand_Packed_Address_Reference (N : Node_Id);
   --  The node N is an attribute reference for the 'Address reference, where
   --  the prefix involves a packed array reference. This routine expands the
   --  necessary code for performing the address reference in this case.

   procedure Expand_Packed_Bit_Reference (N : Node_Id);
   --  The node N is an attribute reference for the 'Bit reference, where the
   --  prefix involves a packed array reference. This routine expands the
   --  necessary code for performing the bit reference in this case.

end Exp_Pakd;
