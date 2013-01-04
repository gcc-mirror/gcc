------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              I N T E R F A C E S . C . E X T E N S I O N S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

   subtype bool is plain_char;

   --  64-bit integer types

   subtype long_long is Long_Long_Integer;
   type unsigned_long_long is mod 2 ** 64;

   --  128-bit integer type available on 64-bit platforms:
   --  typedef int signed_128 __attribute__ ((mode (TI)));

   type Signed_128 is record
      low, high : unsigned_long_long;
   end record;
   pragma Convention (C_Pass_By_Copy, Signed_128);
   for Signed_128'Alignment use unsigned_long_long'Alignment * 2;

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

end Interfaces.C.Extensions;
