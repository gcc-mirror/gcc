------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 G N A T . S S E . V E C T O R _ T Y P E S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2009-2024, Free Software Foundation, Inc.      --
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
--  of SSE registers, for use by bindings to the SSE intrinsic operations.

--  See GNAT.SSE for the list of targets where this facility is supported

package GNAT.SSE.Vector_Types is

   --  The reference guide states a few usage guidelines for the C types:

   --    Since these new data types are not basic ANSI C data types, you
   --    must observe the following usage restrictions:
   --
   --     * Use new data types only on either side of an assignment, as a
   --       return value, or as a parameter. You cannot use it with other
   --       arithmetic expressions ("+", "-", and so on).
   --
   --     * Use new data types as objects in aggregates, such as unions to
   --       access the byte elements and structures.
   --
   --     * Use new data types only with the respective intrinsics described
   --       in this documentation.

   type m128  is private;  --  SSE >= 1
   type m128d is private;  --  SSE >= 2
   type m128i is private;  --  SSE >= 2

private
   --  Each of the m128 types maps to a specific vector_type with an extra
   --  "may_alias" attribute as in GCC's definitions for C, for instance in
   --  xmmintrin.h:

   --  /* The Intel API is flexible enough that we must allow aliasing
   --     with other vector types, and their scalar components.  */
   --  typedef float __m128
   --    __attribute__ ((__vector_size__ (16), __may_alias__));

   --  /* Internal data types for implementing the intrinsics.  */
   --  typedef float __v4sf __attribute__ ((__vector_size__ (16)));

   ------------
   --  m128  --
   ------------

   --  The __m128 data type can hold four 32-bit floating-point values

   type m128 is array (1 .. 4) of Float32;
   for m128'Alignment use VECTOR_ALIGN;
   pragma Machine_Attribute (m128, "vector_type");
   pragma Machine_Attribute (m128, "may_alias");

   -------------
   --  m128d  --
   -------------

   --  The __m128d data type can hold two 64-bit floating-point values

   type m128d is array (1 .. 2) of Float64;
   for m128d'Alignment use VECTOR_ALIGN;
   pragma Machine_Attribute (m128d, "vector_type");
   pragma Machine_Attribute (m128d, "may_alias");

   -------------
   --  m128i  --
   -------------

   --  The __m128i data type can hold sixteen 8-bit, eight 16-bit, four 32-bit,
   --  or two 64-bit integer values.

   type m128i is array (1 .. 2) of Integer64;
   for m128i'Alignment use VECTOR_ALIGN;
   pragma Machine_Attribute (m128i, "vector_type");
   pragma Machine_Attribute (m128i, "may_alias");

end GNAT.SSE.Vector_Types;
