------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . A L T I V E C . V E C T O R _ V I E W S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2018, Free Software Foundation, Inc.          --
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

--  This unit provides public 'View' data types from/to which private vector
--  representations can be converted via Altivec.Conversions. This allows
--  convenient access to individual vector elements and provides a simple way
--  to initialize vector objects.

--  Accessing vector contents with direct memory overlays should be avoided
--  because actual vector representations may vary across configurations, for
--  instance to accommodate different target endianness.

--  The natural representation of a vector is an array indexed by vector
--  component number, which is materialized by the Varray type definitions
--  below. The 16byte alignment constraint is unfortunately sometimes not
--  properly honored for constant array aggregates, so the View types are
--  actually records enclosing such arrays.

package GNAT.Altivec.Vector_Views is

   ---------------------
   -- char components --
   ---------------------

   type Vchar_Range is range 1 .. 16;

   type Varray_unsigned_char is array (Vchar_Range) of unsigned_char;
   for Varray_unsigned_char'Alignment use VECTOR_ALIGNMENT;

   type VUC_View is record
      Values : Varray_unsigned_char;
   end record;

   type Varray_signed_char is array (Vchar_Range) of signed_char;
   for Varray_signed_char'Alignment use VECTOR_ALIGNMENT;

   type VSC_View is record
      Values : Varray_signed_char;
   end record;

   type Varray_bool_char is array (Vchar_Range) of bool_char;
   for Varray_bool_char'Alignment use VECTOR_ALIGNMENT;

   type VBC_View is record
      Values : Varray_bool_char;
   end record;

   ----------------------
   -- short components --
   ----------------------

   type Vshort_Range is range 1 .. 8;

   type Varray_unsigned_short is array (Vshort_Range) of unsigned_short;
   for Varray_unsigned_short'Alignment use VECTOR_ALIGNMENT;

   type VUS_View is record
      Values : Varray_unsigned_short;
   end record;

   type Varray_signed_short is array (Vshort_Range) of signed_short;
   for Varray_signed_short'Alignment use VECTOR_ALIGNMENT;

   type VSS_View is record
      Values : Varray_signed_short;
   end record;

   type Varray_bool_short is array (Vshort_Range) of bool_short;
   for Varray_bool_short'Alignment use VECTOR_ALIGNMENT;

   type VBS_View is record
      Values : Varray_bool_short;
   end record;

   --------------------
   -- int components --
   --------------------

   type Vint_Range is range 1 .. 4;

   type Varray_unsigned_int is array (Vint_Range) of unsigned_int;
   for Varray_unsigned_int'Alignment use VECTOR_ALIGNMENT;

   type VUI_View is record
      Values : Varray_unsigned_int;
   end record;

   type Varray_signed_int is array (Vint_Range) of signed_int;
   for Varray_signed_int'Alignment use VECTOR_ALIGNMENT;

   type VSI_View is record
      Values : Varray_signed_int;
   end record;

   type Varray_bool_int is array (Vint_Range) of bool_int;
   for Varray_bool_int'Alignment use VECTOR_ALIGNMENT;

   type VBI_View is record
      Values : Varray_bool_int;
   end record;

   ----------------------
   -- float components --
   ----------------------

   type Vfloat_Range is range 1 .. 4;

   type Varray_float is array (Vfloat_Range) of C_float;
   for Varray_float'Alignment use VECTOR_ALIGNMENT;

   type VF_View is record
      Values : Varray_float;
   end record;

   ----------------------
   -- pixel components --
   ----------------------

   type Vpixel_Range is range 1 .. 8;

   type Varray_pixel is array (Vpixel_Range) of pixel;
   for Varray_pixel'Alignment use VECTOR_ALIGNMENT;

   type VP_View is record
      Values : Varray_pixel;
   end record;

end GNAT.Altivec.Vector_Views;
