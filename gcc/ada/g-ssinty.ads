------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               G N A T . S S E . I N T E R N A L _ T Y P E S              --
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

--  This unit exposes low level types to interface with the GCC vector
--  builtins directly. These are useful for the development of higher level
--  bindings to the reference Intel intrinsic operations.

--  See GNAT.SSE for the list of targets where this facility is supported.

package GNAT.SSE.Internal_Types is

   type v4sf is private;
   type v2df is private;
   type v2di is private;

private

   --  GCC'wise, vector operations operate on objects of vector modes,
   --  conveyed through vector types obtained in C by setting an attribute on
   --  what looks like a component typedef.  For example, in xmmintrin.h:
   --
   --    typedef float __v4sf __attribute__ ((__vector_size__ (16)));

   --  Applying a 'vector_size' machine attribute in Ada, as in
   --
   --    type Vf is new Float;
   --    pragma Machine_Attribute (Vf,  "vector_size", 16);
   --
   --  makes Vf a 16bytes long V4SFmode GCC type but the effect on the type
   --  layout is not conveyed to the front-end.  The latter still sees "Vf"
   --  as a 4bytes long single float, with numerous potential pitfalls.

   --  We devised a 'vector_type' alternate machine attribute, which applies
   --  to array types of the proper size and alignment from the front-end
   --  perspective:

   type v4sf is array (1 .. 4) of GNAT.SSE.Float32;
   for v4sf'Alignment use GNAT.SSE.VECTOR_ALIGN;
   pragma Machine_Attribute (v4sf, "vector_type");

   type v2di is array (1 .. 2) of GNAT.SSE.Integer64;
   for v2di'Alignment use GNAT.SSE.VECTOR_ALIGN;
   pragma Machine_Attribute (v2di, "vector_type");

   type v2df is array (1 .. 2) of GNAT.SSE.Float64;
   for v2df'Alignment use GNAT.SSE.VECTOR_ALIGN;
   pragma Machine_Attribute (v2df, "vector_type");

end GNAT.SSE.Internal_Types;
