------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . A L T I V E C . V E C T O R _ T Y P E S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2005, Free Software Foundation, Inc.         --
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

--  This unit exposes the various vector types part of the Ada binding to
--  Altivec facilities.

with GNAT.Altivec.Low_Level_Vectors;

package GNAT.Altivec.Vector_Types is

   use GNAT.Altivec.Low_Level_Vectors;

   ---------------------------------------------------
   -- Vector type declarations [PIM-2.1 Data Types] --
   ---------------------------------------------------

   --  Except for assignments and pointer creation/dereference, operations
   --  on vectors are only performed via subprograms. The vector types are
   --  then private, and non-limited since assignments are allowed.

   --  The Hard/Soft binding type-structure differentiation is achieved in
   --  Low_Level_Vectors. Each version only exposes private vector types, that
   --  we just sub-type here. This is fine from the design standpoint and
   --  reduces the amount of explicit conversion required in various places
   --  internally.

   subtype vector_unsigned_char is Low_Level_Vectors.LL_VUC;
   subtype vector_signed_char is Low_Level_Vectors.LL_VSC;
   subtype vector_bool_char is Low_Level_Vectors.LL_VBC;

   subtype vector_unsigned_short is Low_Level_Vectors.LL_VUS;
   subtype vector_signed_short is Low_Level_Vectors.LL_VSS;
   subtype vector_bool_short is Low_Level_Vectors.LL_VBS;

   subtype vector_unsigned_int is Low_Level_Vectors.LL_VUI;
   subtype vector_signed_int is Low_Level_Vectors.LL_VSI;
   subtype vector_bool_int is Low_Level_Vectors.LL_VBI;

   subtype vector_float is Low_Level_Vectors.LL_VF;
   subtype vector_pixel is Low_Level_Vectors.LL_VP;

   --  [PIM-2.1] shows groups of declarations with exact same component types,
   --  e.g. vector unsigned short together with vector unsigned short int. It
   --  so appears tempting to define subtypes for those matches here.
   --
   --  [PIM-2.1] does not qualify items in those groups as "the same types",
   --  though, and [PIM-2.4.2 Assignments] reads: "if either the left hand
   --  side or the right hand side of an expression has a vector type, then
   --  both sides of the expression must be of the same vector type".
   --
   --  Not so clear what is exactly right, then. We go with subtypes for now
   --  and can adjust later if need be.

   subtype vector_unsigned_short_int is vector_unsigned_short;
   subtype vector_signed_short_int is vector_signed_short;

   subtype vector_char is vector_signed_char;
   subtype vector_short is vector_signed_short;
   subtype vector_int is vector_signed_int;

   --------------------------------
   -- Corresponding access types --
   --------------------------------

   type vector_unsigned_char_ptr is access all vector_unsigned_char;
   type vector_signed_char_ptr is access all vector_signed_char;
   type vector_bool_char_ptr is access all vector_bool_char;

   type vector_unsigned_short_ptr is access all vector_unsigned_short;
   type vector_signed_short_ptr is access all vector_signed_short;
   type vector_bool_short_ptr is access all vector_bool_short;

   type vector_unsigned_int_ptr is access all vector_unsigned_int;
   type vector_signed_int_ptr is access all vector_signed_int;
   type vector_bool_int_ptr is access all vector_bool_int;

   type vector_float_ptr is access all vector_float;
   type vector_pixel_ptr is access all vector_pixel;

   --------------------------------------------------------------------
   -- Additional access types, for the sake of some argument passing --
   --------------------------------------------------------------------

   --  ... because some of the operations expect pointers to possibly
   --  constant objects.

   type const_vector_bool_char_ptr     is access constant vector_bool_char;
   type const_vector_signed_char_ptr   is access constant vector_signed_char;
   type const_vector_unsigned_char_ptr is access constant vector_unsigned_char;

   type const_vector_bool_short_ptr     is access constant vector_bool_short;
   type const_vector_signed_short_ptr   is access constant vector_signed_short;
   type const_vector_unsigned_short_ptr is access
     constant vector_unsigned_short;

   type const_vector_bool_int_ptr     is access constant vector_bool_int;
   type const_vector_signed_int_ptr   is access constant vector_signed_int;
   type const_vector_unsigned_int_ptr is access constant vector_unsigned_int;

   type const_vector_float_ptr is access constant vector_float;
   type const_vector_pixel_ptr is access constant vector_pixel;

   ----------------------
   -- Useful shortcuts --
   ----------------------

   subtype VUC is vector_unsigned_char;
   subtype VSC is vector_signed_char;
   subtype VBC is vector_bool_char;

   subtype VUS is vector_unsigned_short;
   subtype VSS is vector_signed_short;
   subtype VBS is vector_bool_short;

   subtype VUI is vector_unsigned_int;
   subtype VSI is vector_signed_int;
   subtype VBI is vector_bool_int;

   subtype VP is vector_pixel;
   subtype VF is vector_float;

end GNAT.Altivec.Vector_Types;
