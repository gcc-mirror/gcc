------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 G N A T . S S E . V E C T O R _ T Y P E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
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

--  This unit exposes the Ada __m128 like data types to represent the contents
--  of SSE registers, for use by the SSE intrinsics.

package GNAT.SSE.Vector_Types is

   --  The reference guide states a few usage guidelines for the C types :

   --  << Since these new data types are not basic ANSI C data types, you
   --     must observe the following usage restrictions:
   --
   --     * Use new data types only on either side of an assignment, as a
   --       return value, or as a parameter. You cannot use it with other
   --       arithmetic expressions ("+", "-", and so on).
   --
   --     * Use new data types as objects in aggregates, such as unions to
   --       access the byte elements and structures.
   --
   --     * Use new data types only with the respective intrinsics described
   --       in this documentation. >>

   type M128 is private;   --  SSE >= 1
   type M128d is private;  --  SSE >= 2
   type M128i is private;  --  SSE >= 2

private
   --  GCC'wise, vector operations operate on objects of vector modes,
   --  conveyed through vector types obtained by setting an attribute on what
   --  looks like a component typedef.  For example, in C (xmmintrin.h):
   --
   --    typedef float __v4sf __attribute__ ((__vector_size__ (16)));

   --  We can obtain the same low level GCC effect in Ada with
   --  Machine_Attribute pragmas, as in
   --
   --    type Vf is new Float;
   --    pragma Machine_Attribute (Vf,  "vector_size", 16);
   --
   --  which makes Vf a 16bytes long V4SFmode type for GCC. The effect on the
   --  type layout is not conveyed to the front-end, however, so the latter
   --  still sees "Vf" as a 4bytes long single float. This leads to numerous
   --  potential pitfalls if this type is directly exposed user land, so we
   --  add wrapper records with rep clauses to compensate.

   --  The wrapper records all have a single component of the twisted low
   --  level type, so they inherit the mode while the rep clauses convey the
   --  size and alignment information to the front-end.

   ------------
   --  M128  --
   ------------

   --  << The __m128 data type can hold four 32-bit floating-point values. >>

   type V4sf is new Float32;
   pragma Machine_Attribute (V4sf, "vector_size", VECTOR_BYTES);

   type M128 is record
      Value : V4sf;
   end record;
   for M128'Size use VECTOR_BYTES * 8;
   for M128'Alignment use VECTOR_ALIGN;

   -------------
   --  M128d  --
   -------------

   --  << The __m128d data type can hold two 64-bit floating-point values. >>

   type V2df is new Float64;
   pragma Machine_Attribute (V2df, "vector_size", VECTOR_BYTES);

   type M128d is record
      Value : V2df;
   end record;
   for M128d'Size use VECTOR_BYTES * 8;
   for M128d'Alignment use VECTOR_ALIGN;

   -------------
   --  M128i  --
   -------------

   --  << The __m128i data type can hold sixteen 8-bit, eight 16-bit, four
   --     32-bit, or two 64-bit integer values. >>

   type V2di is new Integer64;
   pragma Machine_Attribute (V2di, "vector_size", VECTOR_BYTES);

   type M128i is record
      Value : V2di;
   end record;
   for M128i'Size use VECTOR_BYTES * 8;
   for M128i'Alignment use VECTOR_ALIGN;

end GNAT.SSE.Vector_Types;
