------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     G N A T . A L T I V E C . L O W _ L E V E L _ I N T E R F A C E      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2021, Free Software Foundation, Inc.         --
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

--  This unit provides entities to be used internally by the units common to
--  both bindings (Hard or Soft), and relevant to the interfacing with the
--  underlying Low Level support.

with GNAT.Altivec.Vector_Types;      use GNAT.Altivec.Vector_Types;
with GNAT.Altivec.Low_Level_Vectors; use GNAT.Altivec.Low_Level_Vectors;

with Ada.Unchecked_Conversion;

package GNAT.Altivec.Low_Level_Interface is

   -----------------------------------------
   -- Conversions between low level types --
   -----------------------------------------

   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VBC, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VUC, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VSC, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VBS, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VUS, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VSS, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VBI, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VUI, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VSI, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VF,  LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VP,  LL_VBC);

   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VBC, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VUC, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VSC, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VBS, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VUS, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VSS, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VBI, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VUI, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VSI, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VF,  LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VP,  LL_VUC);

   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VBC, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VUC, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VSC, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VBS, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VUS, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VSS, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VBI, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VUI, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VSI, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VF,  LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VP,  LL_VSC);

   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VBC, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VUC, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VSC, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VBS, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VUS, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VSS, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VBI, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VUI, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VSI, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VF, LL_VBS);
   function To_LL_VBS is new Ada.Unchecked_Conversion (LL_VP, LL_VBS);

   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VBC, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VUC, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VSC, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VBS, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VUS, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VSS, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VBI, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VUI, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VSI, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VF,  LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VP,  LL_VUS);

   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VBC, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VUC, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VSC, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VBS, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VUS, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VSS, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VBI, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VUI, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VSI, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VF,  LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VP,  LL_VSS);

   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VBC, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VUC, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VSC, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VBS, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VUS, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VSS, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VBI, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VUI, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VSI, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VF,  LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VP,  LL_VBI);

   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VBC, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VUC, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VSC, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VBS, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VUS, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VSS, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VBI, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VUI, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VSI, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VF,  LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VP,  LL_VUI);

   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VBC, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VUC, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VSC, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VBS, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VUS, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VSS, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VBI, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VUI, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VSI, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VF,  LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VP,  LL_VSI);

   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VBC, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VUC, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VSC, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VBS, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VUS, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VSS, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VBI, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VUI, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VSI, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VF,  LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VP,  LL_VF);

   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VBC, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VUC, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VSC, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VBS, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VUS, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VSS, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VBI, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VUI, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VSI, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VF,  LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VP,  LL_VP);

   ----------------------------------------------
   -- Conversions Between Pointer/Access Types --
   ----------------------------------------------

   function To_PTR is
      new Ada.Unchecked_Conversion (vector_unsigned_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_signed_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_bool_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_unsigned_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_signed_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_bool_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_unsigned_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_signed_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_bool_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_float_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (vector_pixel_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_bool_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_signed_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_unsigned_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_bool_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_signed_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_unsigned_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_bool_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_signed_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_unsigned_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_float_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_vector_pixel_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (c_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (signed_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (unsigned_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (signed_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (unsigned_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (signed_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (unsigned_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (signed_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (unsigned_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (float_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_signed_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_unsigned_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_signed_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_unsigned_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_signed_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_unsigned_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_signed_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_unsigned_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (const_float_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_signed_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_unsigned_char_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_signed_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_unsigned_short_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_signed_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_unsigned_int_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_signed_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_unsigned_long_ptr, c_ptr);
   function To_PTR is
      new Ada.Unchecked_Conversion (constv_float_ptr, c_ptr);

end GNAT.Altivec.Low_Level_Interface;
