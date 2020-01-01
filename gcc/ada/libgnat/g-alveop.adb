------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--       G N A T . A L T I V E C . V E C T O R _ O P E R A T I O N S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2004-2020, Free Software Foundation, Inc.        --
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

with GNAT.Altivec.Low_Level_Interface; use GNAT.Altivec.Low_Level_Interface;

package body GNAT.Altivec.Vector_Operations is

   --------------------------------------------------------
   -- Bodies for generic and specific Altivec operations --
   --------------------------------------------------------

   -------------
   -- vec_abs --
   -------------

   function vec_abs
     (A : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (abs_v16qi (A));
   end vec_abs;

   function vec_abs
     (A : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (abs_v8hi (A));
   end vec_abs;

   function vec_abs
     (A : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (abs_v4si (A));
   end vec_abs;

   function vec_abs
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (abs_v4sf (A));
   end vec_abs;

   --------------
   -- vec_abss --
   --------------

   function vec_abss
     (A : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (abss_v16qi (A));
   end vec_abss;

   function vec_abss
     (A : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (abss_v8hi (A));
   end vec_abss;

   function vec_abss
     (A : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (abss_v4si (A));
   end vec_abss;

   -------------
   -- vec_add --
   -------------

   function vec_add
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_add;

   function vec_add
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_add;

   function vec_add
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_add;

   function vec_add
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_add;

   function vec_add
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_add;

   function vec_add
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_add;

   function vec_add
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_add;

   function vec_add
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_add;

   function vec_add
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_add;

   function vec_add
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_add;

   function vec_add
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_add;

   function vec_add
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_add;

   function vec_add
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_add;

   function vec_add
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_add;

   function vec_add
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_add;

   function vec_add
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_add;

   function vec_add
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_add;

   function vec_add
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_add;

   function vec_add
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vaddfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_add;

   ----------------
   -- vec_vaddfp --
   ----------------

   function vec_vaddfp
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vaddfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_vaddfp;

   -----------------
   -- vec_vadduwm --
   -----------------

   function vec_vadduwm
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduwm;

   function vec_vadduwm
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduwm;

   function vec_vadduwm
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduwm;

   function vec_vadduwm
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduwm;

   function vec_vadduwm
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduwm;

   function vec_vadduwm
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduwm;

   -----------------
   -- vec_vadduhm --
   -----------------

   function vec_vadduhm
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhm;

   function vec_vadduhm
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhm;

   function vec_vadduhm
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhm;

   function vec_vadduhm
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhm;

   function vec_vadduhm
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhm;

   function vec_vadduhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhm;

   -----------------
   -- vec_vaddubm --
   -----------------

   function vec_vaddubm
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubm;

   function vec_vaddubm
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubm;

   function vec_vaddubm
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubm;

   function vec_vaddubm
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubm;

   function vec_vaddubm
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubm;

   function vec_vaddubm
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubm;

   --------------
   -- vec_addc --
   --------------

   function vec_addc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vaddcuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_addc;

   --------------
   -- vec_adds --
   --------------

   function vec_adds
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_adds;

   function vec_adds
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_adds;

   function vec_adds
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_adds;

   function vec_adds
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_adds;

   function vec_adds
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_adds;

   function vec_adds
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_adds;

   function vec_adds
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_adds;

   function vec_adds
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_adds;

   function vec_adds
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_adds;

   function vec_adds
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vaddshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_adds;

   function vec_adds
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vaddshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_adds;

   function vec_adds
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vaddshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_adds;

   function vec_adds
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_adds;

   function vec_adds
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_adds;

   function vec_adds
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_adds;

   function vec_adds
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vaddsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_adds;

   function vec_adds
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vaddsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_adds;

   function vec_adds
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vaddsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_adds;

   -----------------
   -- vec_vaddsws --
   -----------------

   function vec_vaddsws
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vaddsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vaddsws;

   function vec_vaddsws
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vaddsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vaddsws;

   function vec_vaddsws
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vaddsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vaddsws;

   -----------------
   -- vec_vadduws --
   -----------------

   function vec_vadduws
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduws;

   function vec_vadduws
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduws;

   function vec_vadduws
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vadduws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vadduws;

   -----------------
   -- vec_vaddshs --
   -----------------

   function vec_vaddshs
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vaddshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vaddshs;

   function vec_vaddshs
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vaddshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vaddshs;

   function vec_vaddshs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vaddshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vaddshs;

   -----------------
   -- vec_vadduhs --
   -----------------

   function vec_vadduhs
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhs;

   function vec_vadduhs
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhs;

   function vec_vadduhs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vadduhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vadduhs;

   -----------------
   -- vec_vaddsbs --
   -----------------

   function vec_vaddsbs
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddsbs;

   function vec_vaddsbs
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddsbs;

   function vec_vaddsbs
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vaddsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddsbs;

   -----------------
   -- vec_vaddubs --
   -----------------

   function vec_vaddubs
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubs;

   function vec_vaddubs
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubs;

   function vec_vaddubs
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vaddubs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vaddubs;

   -------------
   -- vec_and --
   -------------

   function vec_and
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_float;
      B : vector_bool_int) return vector_float
   is
   begin
      return To_LL_VF  (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_int;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   function vec_and
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vand (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_and;

   --------------
   -- vec_andc --
   --------------

   function vec_andc
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_float;
      B : vector_bool_int) return vector_float
   is
   begin
      return To_LL_VF  (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_int;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   function vec_andc
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vandc (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_andc;

   -------------
   -- vec_avg --
   -------------

   function vec_avg
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vavgub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_avg;

   function vec_avg
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vavgsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_avg;

   function vec_avg
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vavguh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_avg;

   function vec_avg
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vavgsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_avg;

   function vec_avg
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vavguw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_avg;

   function vec_avg
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vavgsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_avg;

   ----------------
   -- vec_vavgsw --
   ----------------

   function vec_vavgsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vavgsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vavgsw;

   ----------------
   -- vec_vavguw --
   ----------------

   function vec_vavguw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vavguw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vavguw;

   ----------------
   -- vec_vavgsh --
   ----------------

   function vec_vavgsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vavgsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vavgsh;

   ----------------
   -- vec_vavguh --
   ----------------

   function vec_vavguh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vavguh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vavguh;

   ----------------
   -- vec_vavgsb --
   ----------------

   function vec_vavgsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vavgsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vavgsb;

   ----------------
   -- vec_vavgub --
   ----------------

   function vec_vavgub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vavgub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vavgub;

   --------------
   -- vec_ceil --
   --------------

   function vec_ceil
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF  (vrfip (To_LL_VF  (A)));
   end vec_ceil;

   --------------
   -- vec_cmpb --
   --------------

   function vec_cmpb
     (A : vector_float;
      B : vector_float) return vector_signed_int
   is
   begin
      return To_LL_VSI (vcmpbfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_cmpb;

   ---------------
   -- vec_cmpeq --
   ---------------

   function vec_cmpeq
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpequb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_cmpeq;

   function vec_cmpeq
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpequb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_cmpeq;

   function vec_cmpeq
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpequh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_cmpeq;

   function vec_cmpeq
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpequh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_cmpeq;

   function vec_cmpeq
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpequw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_cmpeq;

   function vec_cmpeq
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpequw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_cmpeq;

   function vec_cmpeq
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpeqfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_cmpeq;

   ------------------
   -- vec_vcmpeqfp --
   ------------------

   function vec_vcmpeqfp
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpeqfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_vcmpeqfp;

   ------------------
   -- vec_vcmpequw --
   ------------------

   function vec_vcmpequw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpequw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vcmpequw;

   function vec_vcmpequw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpequw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vcmpequw;

   ------------------
   -- vec_vcmpequh --
   ------------------

   function vec_vcmpequh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpequh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vcmpequh;

   function vec_vcmpequh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpequh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vcmpequh;

   ------------------
   -- vec_vcmpequb --
   ------------------

   function vec_vcmpequb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpequb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vcmpequb;

   function vec_vcmpequb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpequb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vcmpequb;

   ---------------
   -- vec_cmpge --
   ---------------

   function vec_cmpge
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgefp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_cmpge;

   ---------------
   -- vec_cmpgt --
   ---------------

   function vec_cmpgt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpgtub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_cmpgt;

   function vec_cmpgt
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpgtsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_cmpgt;

   function vec_cmpgt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpgtuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_cmpgt;

   function vec_cmpgt
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpgtsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_cmpgt;

   function vec_cmpgt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_cmpgt;

   function vec_cmpgt
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_cmpgt;

   function vec_cmpgt
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_cmpgt;

   ------------------
   -- vec_vcmpgtfp --
   ------------------

   function vec_vcmpgtfp
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtfp (To_LL_VF  (A), To_LL_VF  (B)));
   end vec_vcmpgtfp;

   ------------------
   -- vec_vcmpgtsw --
   ------------------

   function vec_vcmpgtsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vcmpgtsw;

   ------------------
   -- vec_vcmpgtuw --
   ------------------

   function vec_vcmpgtuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vcmpgtuw;

   ------------------
   -- vec_vcmpgtsh --
   ------------------

   function vec_vcmpgtsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpgtsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vcmpgtsh;

   ------------------
   -- vec_vcmpgtuh --
   ------------------

   function vec_vcmpgtuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpgtuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vcmpgtuh;

   ------------------
   -- vec_vcmpgtsb --
   ------------------

   function vec_vcmpgtsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpgtsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vcmpgtsb;

   ------------------
   -- vec_vcmpgtub --
   ------------------

   function vec_vcmpgtub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpgtub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vcmpgtub;

   ---------------
   -- vec_cmple --
   ---------------

   function vec_cmple
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgefp (To_LL_VF  (B), To_LL_VF  (A)));
   end vec_cmple;

   ---------------
   -- vec_cmplt --
   ---------------

   function vec_cmplt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpgtub (To_LL_VSC (B), To_LL_VSC (A)));
   end vec_cmplt;

   function vec_cmplt
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vcmpgtsb (To_LL_VSC (B), To_LL_VSC (A)));
   end vec_cmplt;

   function vec_cmplt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpgtuh (To_LL_VSS (B), To_LL_VSS (A)));
   end vec_cmplt;

   function vec_cmplt
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vcmpgtsh (To_LL_VSS (B), To_LL_VSS (A)));
   end vec_cmplt;

   function vec_cmplt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtuw (To_LL_VSI (B), To_LL_VSI (A)));
   end vec_cmplt;

   function vec_cmplt
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtsw (To_LL_VSI (B), To_LL_VSI (A)));
   end vec_cmplt;

   function vec_cmplt
     (A : vector_float;
      B : vector_float) return vector_bool_int
   is
   begin
      return To_LL_VBI (vcmpgtfp (To_LL_VF  (B), To_LL_VF  (A)));
   end vec_cmplt;

   ---------------
   -- vec_expte --
   ---------------

   function vec_expte
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vexptefp (To_LL_VF (A)));
   end vec_expte;

   ---------------
   -- vec_floor --
   ---------------

   function vec_floor
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vrfim (To_LL_VF (A)));
   end vec_floor;

   ------------
   -- vec_ld --
   ------------

   function vec_ld
     (A : c_long;
      B : const_vector_float_ptr) return vector_float
   is
   begin
      return To_LL_VF (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_float_ptr) return vector_float
   is
   begin
      return To_LL_VF (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_bool_int_ptr) return vector_bool_int
   is
   begin
      return To_LL_VBI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_signed_int_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_int_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_long_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_unsigned_int_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_bool_short_ptr) return vector_bool_short
   is
   begin
      return To_LL_VBS (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_pixel_ptr) return vector_pixel
   is
   begin
      return To_LL_VP (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_signed_short_ptr) return vector_signed_short
   is
   begin
      return To_LL_VSS (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_short_ptr) return vector_signed_short
   is
   begin
      return To_LL_VSS (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_unsigned_short_ptr) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_bool_char_ptr) return vector_bool_char
   is
   begin
      return To_LL_VBC (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_signed_char_ptr) return vector_signed_char
   is
   begin
      return To_LL_VSC (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char
   is
   begin
      return To_LL_VSC (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_vector_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvx (A, To_PTR (B)));
   end vec_ld;

   function vec_ld
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvx (A, To_PTR (B)));
   end vec_ld;

   -------------
   -- vec_lde --
   -------------

   function vec_lde
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char
   is
   begin
      return To_LL_VSC (lvebx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvebx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_short_ptr) return vector_signed_short
   is
   begin
      return To_LL_VSS (lvehx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (lvehx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_float_ptr) return vector_float
   is
   begin
      return To_LL_VF (lvewx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_int_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvewx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvewx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_long_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvewx (A, To_PTR (B)));
   end vec_lde;

   function vec_lde
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvewx (A, To_PTR (B)));
   end vec_lde;

   ---------------
   -- vec_lvewx --
   ---------------

   function vec_lvewx
     (A : c_long;
      B : float_ptr) return vector_float
   is
   begin
      return To_LL_VF (lvewx (A, To_PTR (B)));
   end vec_lvewx;

   function vec_lvewx
     (A : c_long;
      B : int_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvewx (A, To_PTR (B)));
   end vec_lvewx;

   function vec_lvewx
     (A : c_long;
      B : unsigned_int_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvewx (A, To_PTR (B)));
   end vec_lvewx;

   function vec_lvewx
     (A : c_long;
      B : long_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvewx (A, To_PTR (B)));
   end vec_lvewx;

   function vec_lvewx
     (A : c_long;
      B : unsigned_long_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvewx (A, To_PTR (B)));
   end vec_lvewx;

   ---------------
   -- vec_lvehx --
   ---------------

   function vec_lvehx
     (A : c_long;
      B : short_ptr) return vector_signed_short
   is
   begin
      return To_LL_VSS (lvehx (A, To_PTR (B)));
   end vec_lvehx;

   function vec_lvehx
     (A : c_long;
      B : unsigned_short_ptr) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (lvehx (A, To_PTR (B)));
   end vec_lvehx;

   ---------------
   -- vec_lvebx --
   ---------------

   function vec_lvebx
     (A : c_long;
      B : signed_char_ptr) return vector_signed_char
   is
   begin
      return To_LL_VSC (lvebx (A, To_PTR (B)));
   end vec_lvebx;

   function vec_lvebx
     (A : c_long;
      B : unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvebx (A, To_PTR (B)));
   end vec_lvebx;

   -------------
   -- vec_ldl --
   -------------

   function vec_ldl
     (A : c_long;
      B : const_vector_float_ptr) return vector_float
   is
   begin
      return To_LL_VF (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_float_ptr) return vector_float
   is
   begin
      return To_LL_VF (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_bool_int_ptr) return vector_bool_int
   is
   begin
      return To_LL_VBI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_signed_int_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_int_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_long_ptr) return vector_signed_int
   is
   begin
      return To_LL_VSI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_unsigned_int_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_bool_short_ptr) return vector_bool_short
   is
   begin
      return To_LL_VBS (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_pixel_ptr) return vector_pixel
   is
   begin
      return To_LL_VP (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_signed_short_ptr) return vector_signed_short
   is
   begin
      return To_LL_VSS (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_short_ptr) return vector_signed_short
   is
   begin
      return To_LL_VSS (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_unsigned_short_ptr) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_bool_char_ptr) return vector_bool_char
   is
   begin
      return To_LL_VBC (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_signed_char_ptr) return vector_signed_char
   is
   begin
      return To_LL_VSC (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char
   is
   begin
      return To_LL_VSC (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_vector_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvxl (A, To_PTR (B)));
   end vec_ldl;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvxl (A, To_PTR (B)));
   end vec_ldl;

   --------------
   -- vec_loge --
   --------------

   function vec_loge
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vlogefp (To_LL_VF (A)));
   end vec_loge;

   --------------
   -- vec_lvsl --
   --------------

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_signed_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_short_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_short_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_int_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_int_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_long_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_long_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   function vec_lvsl
     (A : c_long;
      B : constv_float_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsl (A, To_PTR (B)));
   end vec_lvsl;

   --------------
   -- vec_lvsr --
   --------------

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_signed_char_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_short_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_short_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_int_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_int_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_long_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_long_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   function vec_lvsr
     (A : c_long;
      B : constv_float_ptr) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (lvsr (A, To_PTR (B)));
   end vec_lvsr;

   --------------
   -- vec_madd --
   --------------

   function vec_madd
     (A : vector_float;
      B : vector_float;
      C : vector_float) return vector_float
   is
   begin
      return vmaddfp (A, B, C);
   end vec_madd;

   ---------------
   -- vec_madds --
   ---------------

   function vec_madds
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   is
   begin
      return vmhaddshs (A, B, C);
   end vec_madds;

   -------------
   -- vec_max --
   -------------

   function vec_max
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmaxub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_max;

   function vec_max
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmaxub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_max;

   function vec_max
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmaxub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_max;

   function vec_max
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmaxsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_max;

   function vec_max
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmaxsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_max;

   function vec_max
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmaxsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_max;

   function vec_max
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmaxuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_max;

   function vec_max
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmaxuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_max;

   function vec_max
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmaxuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_max;

   function vec_max
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmaxsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_max;

   function vec_max
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmaxsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_max;

   function vec_max
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmaxsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_max;

   function vec_max
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmaxuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_max;

   function vec_max
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmaxuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_max;

   function vec_max
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmaxuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_max;

   function vec_max
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmaxsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_max;

   function vec_max
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmaxsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_max;

   function vec_max
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmaxsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_max;

   function vec_max
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vmaxfp (To_LL_VF (A), To_LL_VF (B)));
   end vec_max;

   ----------------
   -- vec_vmaxfp --
   ----------------

   function vec_vmaxfp
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vmaxfp (To_LL_VF (A), To_LL_VF (B)));
   end vec_vmaxfp;

   ----------------
   -- vec_vmaxsw --
   ----------------

   function vec_vmaxsw
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmaxsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmaxsw;

   function vec_vmaxsw
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmaxsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmaxsw;

   function vec_vmaxsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmaxsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmaxsw;

   ----------------
   -- vec_vmaxuw --
   ----------------

   function vec_vmaxuw
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmaxuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmaxuw;

   function vec_vmaxuw
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmaxuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmaxuw;

   function vec_vmaxuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmaxuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmaxuw;

   ----------------
   -- vec_vmaxsh --
   ----------------

   function vec_vmaxsh
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmaxsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmaxsh;

   function vec_vmaxsh
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmaxsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmaxsh;

   function vec_vmaxsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmaxsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmaxsh;

   ----------------
   -- vec_vmaxuh --
   ----------------

   function vec_vmaxuh
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmaxuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmaxuh;

   function vec_vmaxuh
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmaxuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmaxuh;

   function vec_vmaxuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmaxuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmaxuh;

   ----------------
   -- vec_vmaxsb --
   ----------------

   function vec_vmaxsb
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmaxsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmaxsb;

   function vec_vmaxsb
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmaxsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmaxsb;

   function vec_vmaxsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmaxsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmaxsb;

   ----------------
   -- vec_vmaxub --
   ----------------

   function vec_vmaxub
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmaxub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmaxub;

   function vec_vmaxub
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmaxub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmaxub;

   function vec_vmaxub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmaxub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmaxub;

   ----------------
   -- vec_mergeh --
   ----------------

   function vec_mergeh
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vmrghb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmrghb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmrghb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel
   is
   begin
      return To_LL_VP (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergeh;

   function vec_mergeh
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergeh;

   ----------------
   -- vec_vmrghw --
   ----------------

   function vec_vmrghw
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrghw;

   function vec_vmrghw
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrghw;

   function vec_vmrghw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrghw;

   function vec_vmrghw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmrghw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrghw;

   ----------------
   -- vec_vmrghh --
   ----------------

   function vec_vmrghh
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrghh;

   function vec_vmrghh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrghh;

   function vec_vmrghh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrghh;

   function vec_vmrghh
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel
   is
   begin
      return To_LL_VP (vmrghh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrghh;

   ----------------
   -- vec_vmrghb --
   ----------------

   function vec_vmrghb
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vmrghb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmrghb;

   function vec_vmrghb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmrghb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmrghb;

   function vec_vmrghb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmrghb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmrghb;

   ----------------
   -- vec_mergel --
   ----------------

   function vec_mergel
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vmrglb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmrglb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmrglb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel
   is
   begin
      return To_LL_VP (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergel;

   function vec_mergel
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_mergel;

   ----------------
   -- vec_vmrglw --
   ----------------

   function vec_vmrglw
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrglw;

   function vec_vmrglw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrglw;

   function vec_vmrglw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrglw;

   function vec_vmrglw
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vmrglw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vmrglw;

   ----------------
   -- vec_vmrglh --
   ----------------

   function vec_vmrglh
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrglh;

   function vec_vmrglh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrglh;

   function vec_vmrglh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrglh;

   function vec_vmrglh
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel
   is
   begin
      return To_LL_VP (vmrglh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmrglh;

   ----------------
   -- vec_vmrglb --
   ----------------

   function vec_vmrglb
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vmrglb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmrglb;

   function vec_vmrglb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vmrglb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmrglb;

   function vec_vmrglb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vmrglb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmrglb;

   ----------------
   -- vec_mfvscr --
   ----------------

   function vec_mfvscr return vector_unsigned_short
   is
   begin
      return To_LL_VUS (mfvscr);
   end vec_mfvscr;

   -------------
   -- vec_min --
   -------------

   function vec_min
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vminub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_min;

   function vec_min
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vminub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_min;

   function vec_min
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vminub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_min;

   function vec_min
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vminsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_min;

   function vec_min
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vminsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_min;

   function vec_min
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vminsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_min;

   function vec_min
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vminuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_min;

   function vec_min
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vminuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_min;

   function vec_min
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vminuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_min;

   function vec_min
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vminsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_min;

   function vec_min
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vminsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_min;

   function vec_min
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vminsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_min;

   function vec_min
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vminuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_min;

   function vec_min
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vminuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_min;

   function vec_min
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vminuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_min;

   function vec_min
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vminsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_min;

   function vec_min
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vminsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_min;

   function vec_min
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vminsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_min;

   function vec_min
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vminfp (To_LL_VF (A), To_LL_VF (B)));
   end vec_min;

   -- vec_vminfp --

   function vec_vminfp
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vminfp (To_LL_VF (A), To_LL_VF (B)));
   end vec_vminfp;

   -- vec_vminsw --

   function vec_vminsw
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vminsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vminsw;

   function vec_vminsw
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vminsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vminsw;

   function vec_vminsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vminsw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vminsw;

   -- vec_vminuw --

   function vec_vminuw
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vminuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vminuw;

   function vec_vminuw
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vminuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vminuw;

   function vec_vminuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vminuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vminuw;

   -- vec_vminsh --

   function vec_vminsh
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vminsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vminsh;

   function vec_vminsh
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vminsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vminsh;

   function vec_vminsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vminsh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vminsh;

   ----------------
   -- vec_vminuh --
   ----------------

   function vec_vminuh
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vminuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vminuh;

   function vec_vminuh
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vminuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vminuh;

   function vec_vminuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vminuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vminuh;

   ----------------
   -- vec_vminsb --
   ----------------

   function vec_vminsb
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vminsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vminsb;

   function vec_vminsb
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vminsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vminsb;

   function vec_vminsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vminsb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vminsb;

   ----------------
   -- vec_vminub --
   ----------------

   function vec_vminub
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vminub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vminub;

   function vec_vminub
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vminub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vminub;

   function vec_vminub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vminub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vminub;

   ---------------
   -- vec_mladd --
   ---------------

   function vec_mladd
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   is
   begin
      return vmladduhm (A, B, C);
   end vec_mladd;

   function vec_mladd
     (A : vector_signed_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_signed_short
   is
   begin
      return vmladduhm (A, To_LL_VSS (B), To_LL_VSS (C));
   end vec_mladd;

   function vec_mladd
     (A : vector_unsigned_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   is
   begin
      return vmladduhm (To_LL_VSS (A), B, C);
   end vec_mladd;

   function vec_mladd
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return
        To_LL_VUS (vmladduhm (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSS (C)));
   end vec_mladd;

   ----------------
   -- vec_mradds --
   ----------------

   function vec_mradds
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   is
   begin
      return vmhraddshs (A, B, C);
   end vec_mradds;

   --------------
   -- vec_msum --
   --------------

   function vec_msum
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vmsumubm (To_LL_VSC (A), To_LL_VSC (B), To_LL_VSI (C)));
   end vec_msum;

   function vec_msum
     (A : vector_signed_char;
      B : vector_unsigned_char;
      C : vector_signed_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vmsummbm (To_LL_VSC (A), To_LL_VSC (B), To_LL_VSI (C)));
   end vec_msum;

   function vec_msum
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vmsumuhm (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_msum;

   function vec_msum
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vmsumshm (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_msum;

   ------------------
   -- vec_vmsumshm --
   ------------------

   function vec_vmsumshm
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vmsumshm (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_vmsumshm;

   ------------------
   -- vec_vmsumuhm --
   ------------------

   function vec_vmsumuhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vmsumuhm (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_vmsumuhm;

   ------------------
   -- vec_vmsummbm --
   ------------------

   function vec_vmsummbm
     (A : vector_signed_char;
      B : vector_unsigned_char;
      C : vector_signed_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vmsummbm (To_LL_VSC (A), To_LL_VSC (B), To_LL_VSI (C)));
   end vec_vmsummbm;

   ------------------
   -- vec_vmsumubm --
   ------------------

   function vec_vmsumubm
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vmsumubm (To_LL_VSC (A), To_LL_VSC (B), To_LL_VSI (C)));
   end vec_vmsumubm;

   ---------------
   -- vec_msums --
   ---------------

   function vec_msums
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vmsumuhs (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_msums;

   function vec_msums
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vmsumshs (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_msums;

   ------------------
   -- vec_vmsumshs --
   ------------------

   function vec_vmsumshs
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vmsumshs (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_vmsumshs;

   ------------------
   -- vec_vmsumuhs --
   ------------------

   function vec_vmsumuhs
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vmsumuhs (To_LL_VSS (A), To_LL_VSS (B), To_LL_VSI (C)));
   end vec_vmsumuhs;

   ----------------
   -- vec_mtvscr --
   ----------------

   procedure vec_mtvscr
     (A : vector_signed_int)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_unsigned_int)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_bool_int)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_signed_short)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_unsigned_short)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_bool_short)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_pixel)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_signed_char)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_unsigned_char)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   procedure vec_mtvscr
     (A : vector_bool_char)
   is
   begin
      mtvscr (To_LL_VSI (A));
   end vec_mtvscr;

   --------------
   -- vec_mule --
   --------------

   function vec_mule
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmuleub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mule;

   function vec_mule
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmulesb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mule;

   function vec_mule
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmuleuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mule;

   function vec_mule
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmulesh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mule;

   -----------------
   -- vec_vmulesh --
   -----------------

   function vec_vmulesh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmulesh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmulesh;

   -----------------
   -- vec_vmuleuh --
   -----------------

   function vec_vmuleuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmuleuh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmuleuh;

   -----------------
   -- vec_vmulesb --
   -----------------

   function vec_vmulesb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmuleub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmulesb;

   -----------------
   -- vec_vmuleub --
   -----------------

   function vec_vmuleub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmuleub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmuleub;

   --------------
   -- vec_mulo --
   --------------

   function vec_mulo
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmuloub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mulo;

   function vec_mulo
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmulosb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_mulo;

   function vec_mulo
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmulouh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mulo;

   function vec_mulo
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmulosh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_mulo;

   -----------------
   -- vec_vmulosh --
   -----------------

   function vec_vmulosh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vmulosh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmulosh;

   -----------------
   -- vec_vmulouh --
   -----------------

   function vec_vmulouh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vmulouh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vmulouh;

   -----------------
   -- vec_vmulosb --
   -----------------

   function vec_vmulosb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vmulosb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmulosb;

   -----------------
   -- vec_vmuloub --
   -----------------

   function vec_vmuloub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vmuloub (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vmuloub;

   ---------------
   -- vec_nmsub --
   ---------------

   function vec_nmsub
     (A : vector_float;
      B : vector_float;
      C : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vnmsubfp (To_LL_VF (A), To_LL_VF (B), To_LL_VF (C)));
   end vec_nmsub;

   -------------
   -- vec_nor --
   -------------

   function vec_nor
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   function vec_nor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vnor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_nor;

   ------------
   -- vec_or --
   ------------

   function vec_or
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_float;
      B : vector_bool_int) return vector_float
   is
   begin
      return To_LL_VF (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_int;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   function vec_or
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_or;

   --------------
   -- vec_pack --
   --------------

   function vec_pack
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char
   is
   begin
      return To_LL_VSC (vpkuhum (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_pack;

   function vec_pack
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkuhum (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_pack;

   function vec_pack
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_char
   is
   begin
      return To_LL_VBC (vpkuhum (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_pack;

   function vec_pack
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short
   is
   begin
      return To_LL_VSS (vpkuwum (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_pack;

   function vec_pack
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkuwum (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_pack;

   function vec_pack
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_short
   is
   begin
      return To_LL_VBS (vpkuwum (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_pack;

   -----------------
   -- vec_vpkuwum --
   -----------------

   function vec_vpkuwum
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_short
   is
   begin
      return To_LL_VBS (vpkuwum (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vpkuwum;

   function vec_vpkuwum
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short
   is
   begin
      return To_LL_VSS (vpkuwum (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vpkuwum;

   function vec_vpkuwum
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkuwum (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vpkuwum;

   -----------------
   -- vec_vpkuhum --
   -----------------

   function vec_vpkuhum
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_char
   is
   begin
      return To_LL_VBC (vpkuhum (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vpkuhum;

   function vec_vpkuhum
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char
   is
   begin
      return To_LL_VSC (vpkuhum (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vpkuhum;

   function vec_vpkuhum
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkuhum (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vpkuhum;

   ----------------
   -- vec_packpx --
   ----------------

   function vec_packpx
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_pixel
   is
   begin
      return To_LL_VP (vpkpx (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_packpx;

   ---------------
   -- vec_packs --
   ---------------

   function vec_packs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkuhus (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_packs;

   function vec_packs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char
   is
   begin
      return To_LL_VSC (vpkshss (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_packs;

   function vec_packs
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkuwus (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_packs;

   function vec_packs
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short
   is
   begin
      return To_LL_VSS (vpkswss (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_packs;

   -----------------
   -- vec_vpkswss --
   -----------------

   function vec_vpkswss
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short
   is
   begin
      return To_LL_VSS (vpkswss (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vpkswss;

   -----------------
   -- vec_vpkuwus --
   -----------------

   function vec_vpkuwus
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkuwus (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vpkuwus;

   -----------------
   -- vec_vpkshss --
   -----------------

   function vec_vpkshss
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char
   is
   begin
      return To_LL_VSC (vpkshss (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vpkshss;

   -----------------
   -- vec_vpkuhus --
   -----------------

   function vec_vpkuhus
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkuhus (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vpkuhus;

   ----------------
   -- vec_packsu --
   ----------------

   function vec_packsu
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkuhus (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_packsu;

   function vec_packsu
     (A : vector_signed_short;
      B : vector_signed_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkshus (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_packsu;

   function vec_packsu
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkuwus (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_packsu;

   function vec_packsu
     (A : vector_signed_int;
      B : vector_signed_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkswus (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_packsu;

   -----------------
   -- vec_vpkswus --
   -----------------

   function vec_vpkswus
     (A : vector_signed_int;
      B : vector_signed_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vpkswus (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vpkswus;

   -----------------
   -- vec_vpkshus --
   -----------------

   function vec_vpkshus
     (A : vector_signed_short;
      B : vector_signed_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vpkshus (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vpkshus;

   --------------
   -- vec_perm --
   --------------

   function vec_perm
     (A : vector_float;
      B : vector_float;
      C : vector_unsigned_char) return vector_float
   is
   begin
      return
        To_LL_VF (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_unsigned_char) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_unsigned_char) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_unsigned_char) return vector_bool_int
   is
   begin
      return
        To_LL_VBI (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_unsigned_char) return vector_signed_short
   is
   begin
      return
        To_LL_VSS (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return
        To_LL_VUS (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_unsigned_char) return vector_bool_short
   is
   begin
      return
        To_LL_VBS (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_pixel;
      B : vector_pixel;
      C : vector_unsigned_char) return vector_pixel
   is
   begin
      return To_LL_VP
        (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC
        (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return
        To_LL_VUC (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   function vec_perm
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_unsigned_char) return vector_bool_char
   is
   begin
      return
        To_LL_VBC (vperm_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSC (C)));
   end vec_perm;

   ------------
   -- vec_re --
   ------------

   function vec_re
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vrefp (To_LL_VF (A)));
   end vec_re;

   ------------
   -- vec_rl --
   ------------

   function vec_rl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vrlb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_rl;

   function vec_rl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vrlb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_rl;

   function vec_rl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vrlh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_rl;

   function vec_rl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vrlh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_rl;

   function vec_rl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vrlw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_rl;

   function vec_rl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vrlw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_rl;

   --------------
   -- vec_vrlw --
   --------------

   function vec_vrlw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vrlw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vrlw;

   function vec_vrlw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vrlw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vrlw;

   --------------
   -- vec_vrlh --
   --------------

   function vec_vrlh
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vrlh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vrlh;

   function vec_vrlh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vrlh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vrlh;

   --------------
   -- vec_vrlb --
   --------------

   function vec_vrlb
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vrlb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vrlb;

   function vec_vrlb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vrlb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vrlb;

   ---------------
   -- vec_round --
   ---------------

   function vec_round
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vrfin (To_LL_VF (A)));
   end vec_round;

   ----------------
   -- vec_rsqrte --
   ----------------

   function vec_rsqrte
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vrsqrtefp (To_LL_VF (A)));
   end vec_rsqrte;

   -------------
   -- vec_sel --
   -------------

   function vec_sel
     (A : vector_float;
      B : vector_float;
      C : vector_bool_int) return vector_float
   is
   begin
      return To_LL_VF (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_float;
      B : vector_float;
      C : vector_unsigned_int) return vector_float
   is
   begin
      return To_LL_VF (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_bool_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_unsigned_int) return vector_signed_int
   is
   begin
      return
        To_LL_VSI (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_bool_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return
        To_LL_VUI (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_bool_int) return vector_bool_int
   is
   begin
      return
        To_LL_VBI (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_unsigned_int) return vector_bool_int
   is
   begin
      return
        To_LL_VBI (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_bool_short) return vector_signed_short
   is
   begin
      return
        To_LL_VSS (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_unsigned_short) return vector_signed_short
   is
   begin
      return
        To_LL_VSS (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_bool_short) return vector_unsigned_short
   is
   begin
      return
        To_LL_VUS (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return
        To_LL_VUS (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_bool_short) return vector_bool_short
   is
   begin
      return
        To_LL_VBS (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_unsigned_short) return vector_bool_short
   is
   begin
      return
        To_LL_VBS (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_bool_char) return vector_signed_char
   is
   begin
      return
        To_LL_VSC (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_unsigned_char) return vector_signed_char
   is
   begin
      return
        To_LL_VSC (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_bool_char) return vector_unsigned_char
   is
   begin
      return
        To_LL_VUC (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return
        To_LL_VUC (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_bool_char) return vector_bool_char
   is
   begin
      return
        To_LL_VBC (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   function vec_sel
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_unsigned_char) return vector_bool_char
   is
   begin
      return
        To_LL_VBC (vsel_4si (To_LL_VSI (A), To_LL_VSI (B), To_LL_VSI (C)));
   end vec_sel;

   ------------
   -- vec_sl --
   ------------

   function vec_sl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vslb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sl;

   function vec_sl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vslb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sl;

   function vec_sl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vslh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sl;

   function vec_sl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vslh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sl;

   function vec_sl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vslw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sl;

   function vec_sl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vslw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sl;

   --------------
   -- vec_vslw --
   --------------

   function vec_vslw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vslw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vslw;

   function vec_vslw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vslw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vslw;

   --------------
   -- vec_vslh --
   --------------

   function vec_vslh
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vslh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vslh;

   function vec_vslh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vslh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vslh;

   --------------
   -- vec_vslb --
   --------------

   function vec_vslb
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vslb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vslb;

   function vec_vslb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vslb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vslb;

   -------------
   -- vec_sll --
   -------------

   function vec_sll
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_int;
      B : vector_unsigned_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_int;
      B : vector_unsigned_short) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_int;
      B : vector_unsigned_short) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_int;
      B : vector_unsigned_char) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_short;
      B : vector_unsigned_int) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_short;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_short;
      B : vector_unsigned_int) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_short;
      B : vector_unsigned_char) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_pixel;
      B : vector_unsigned_int) return vector_pixel
   is
   begin
      return To_LL_VP (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_pixel;
      B : vector_unsigned_short) return vector_pixel
   is
   begin
      return To_LL_VP (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   is
   begin
      return To_LL_VP (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_char;
      B : vector_unsigned_int) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_char;
      B : vector_unsigned_short) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_char;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_char;
      B : vector_unsigned_int) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_char;
      B : vector_unsigned_short) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   function vec_sll
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsl (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sll;

   -------------
   -- vec_slo --
   -------------

   function vec_slo
     (A : vector_float;
      B : vector_signed_char) return vector_float
   is
   begin
      return To_LL_VF (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_float;
      B : vector_unsigned_char) return vector_float
   is
   begin
      return To_LL_VF (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_signed_int;
      B : vector_signed_char) return vector_signed_int
   is
   begin
      return To_LL_VSI (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   is
   begin
      return To_LL_VSI (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_unsigned_int;
      B : vector_signed_char) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_signed_short;
      B : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_unsigned_short;
      B : vector_signed_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_pixel;
      B : vector_signed_char) return vector_pixel
   is
   begin
      return To_LL_VP (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   is
   begin
      return To_LL_VP (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_unsigned_char;
      B : vector_signed_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   function vec_slo
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vslo (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_slo;

   ------------
   -- vec_sr --
   ------------

   function vec_sr
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsrb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sr;

   function vec_sr
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsrb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sr;

   function vec_sr
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsrh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sr;

   function vec_sr
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsrh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sr;

   function vec_sr
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsrw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sr;

   function vec_sr
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsrw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sr;

   --------------
   -- vec_vsrw --
   --------------

   function vec_vsrw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsrw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsrw;

   function vec_vsrw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsrw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsrw;

   --------------
   -- vec_vsrh --
   --------------

   function vec_vsrh
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsrh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsrh;

   function vec_vsrh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsrh (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsrh;

   --------------
   -- vec_vsrb --
   --------------

   function vec_vsrb
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsrb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsrb;

   function vec_vsrb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsrb (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsrb;

   -------------
   -- vec_sra --
   -------------

   function vec_sra
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsrab (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sra;

   function vec_sra
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsrab (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sra;

   function vec_sra
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsrah (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sra;

   function vec_sra
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsrah (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sra;

   function vec_sra
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsraw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sra;

   function vec_sra
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsraw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sra;

   ---------------
   -- vec_vsraw --
   ---------------

   function vec_vsraw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsraw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsraw;

   function vec_vsraw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsraw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsraw;

   ---------------
   -- vec_vsrah --
   ---------------

   function vec_vsrah
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsrah (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsrah;

   function vec_vsrah
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsrah (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsrah;

   ---------------
   -- vec_vsrab --
   ---------------

   function vec_vsrab
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsrab (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsrab;

   function vec_vsrab
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsrab (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsrab;

   -------------
   -- vec_srl --
   -------------

   function vec_srl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_int;
      B : vector_unsigned_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_int;
      B : vector_unsigned_short) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_int;
      B : vector_unsigned_short) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_int;
      B : vector_unsigned_char) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_short;
      B : vector_unsigned_int) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_short;
      B : vector_unsigned_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_short;
      B : vector_unsigned_int) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_short;
      B : vector_unsigned_char) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_pixel;
      B : vector_unsigned_int) return vector_pixel
   is
   begin
      return To_LL_VP (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_pixel;
      B : vector_unsigned_short) return vector_pixel
   is
   begin
      return To_LL_VP (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   is
   begin
      return To_LL_VP (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_char;
      B : vector_unsigned_int) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_char;
      B : vector_unsigned_short) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_char;
      B : vector_unsigned_short) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_char;
      B : vector_unsigned_int) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_char;
      B : vector_unsigned_short) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   function vec_srl
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsr (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_srl;

   -------------
   -- vec_sro --
   -------------

   function vec_sro
     (A : vector_float;
      B : vector_signed_char) return vector_float
   is
   begin
      return To_LL_VF (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_float;
      B : vector_unsigned_char) return vector_float
   is
   begin
      return To_LL_VF (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_signed_int;
      B : vector_signed_char) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_unsigned_int;
      B : vector_signed_char) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_signed_short;
      B : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_unsigned_short;
      B : vector_signed_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_pixel;
      B : vector_signed_char) return vector_pixel
   is
   begin
      return To_LL_VP (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   is
   begin
      return To_LL_VP (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_unsigned_char;
      B : vector_signed_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   function vec_sro
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsro (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sro;

   ------------
   -- vec_st --
   ------------

   procedure vec_st
     (A : vector_float;
      B : c_int;
      C : vector_float_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_float;
      B : c_int;
      C : float_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_signed_int;
      B : c_int;
      C : vector_signed_int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_unsigned_int;
      B : c_int;
      C : vector_unsigned_int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_int;
      B : c_int;
      C : vector_bool_int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_signed_short;
      B : c_int;
      C : vector_signed_short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_unsigned_short;
      B : c_int;
      C : vector_unsigned_short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_short;
      B : c_int;
      C : vector_bool_short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_pixel;
      B : c_int;
      C : vector_pixel_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_pixel;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_signed_char;
      B : c_int;
      C : vector_signed_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_unsigned_char;
      B : c_int;
      C : vector_unsigned_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_char;
      B : c_int;
      C : vector_bool_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   procedure vec_st
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvx (To_LL_VSI (A), B, To_PTR (C));
   end vec_st;

   -------------
   -- vec_ste --
   -------------

   procedure vec_ste
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_pixel;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_float;
      B : c_int;
      C : float_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_ste;

   procedure vec_ste
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_ste;

   ----------------
   -- vec_stvewx --
   ----------------

   procedure vec_stvewx
     (A : vector_float;
      B : c_int;
      C : float_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_stvewx;

   procedure vec_stvewx
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_stvewx;

   procedure vec_stvewx
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_stvewx;

   procedure vec_stvewx
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_stvewx;

   procedure vec_stvewx
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvewx (To_LL_VSI (A), B, To_PTR (C));
   end vec_stvewx;

   ----------------
   -- vec_stvehx --
   ----------------

   procedure vec_stvehx
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_stvehx;

   procedure vec_stvehx
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_stvehx;

   procedure vec_stvehx
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_stvehx;

   procedure vec_stvehx
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_stvehx;

   procedure vec_stvehx
     (A : vector_pixel;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_stvehx;

   procedure vec_stvehx
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvehx (To_LL_VSS (A), B, To_PTR (C));
   end vec_stvehx;

   ----------------
   -- vec_stvebx --
   ----------------

   procedure vec_stvebx
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_stvebx;

   procedure vec_stvebx
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_stvebx;

   procedure vec_stvebx
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_stvebx;

   procedure vec_stvebx
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvebx (To_LL_VSC (A), B, To_PTR (C));
   end vec_stvebx;

   -------------
   -- vec_stl --
   -------------

   procedure vec_stl
     (A : vector_float;
      B : c_int;
      C : vector_float_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_float;
      B : c_int;
      C : float_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_signed_int;
      B : c_int;
      C : vector_signed_int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_unsigned_int;
      B : c_int;
      C : vector_unsigned_int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_int;
      B : c_int;
      C : vector_bool_int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_signed_short;
      B : c_int;
      C : vector_signed_short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_unsigned_short;
      B : c_int;
      C : vector_unsigned_short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_short;
      B : c_int;
      C : vector_bool_short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_pixel;
      B : c_int;
      C : vector_pixel_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_pixel;
      B : c_int;
      C : short_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_signed_char;
      B : c_int;
      C : vector_signed_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_unsigned_char;
      B : c_int;
      C : vector_unsigned_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_char;
      B : c_int;
      C : vector_bool_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   procedure vec_stl
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr)
   is
   begin
      stvxl (To_LL_VSI (A), B, To_PTR (C));
   end vec_stl;

   -------------
   -- vec_sub --
   -------------

   function vec_sub
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sub;

   function vec_sub
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sub;

   function vec_sub
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sub;

   function vec_sub
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sub;

   function vec_sub
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sub;

   function vec_sub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_sub;

   function vec_sub
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sub;

   function vec_sub
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sub;

   function vec_sub
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sub;

   function vec_sub
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sub;

   function vec_sub
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sub;

   function vec_sub
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_sub;

   function vec_sub
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sub;

   function vec_sub
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sub;

   function vec_sub
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sub;

   function vec_sub
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sub;

   function vec_sub
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sub;

   function vec_sub
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sub;

   function vec_sub
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vsubfp (To_LL_VF (A), To_LL_VF (B)));
   end vec_sub;

   ----------------
   -- vec_vsubfp --
   ----------------

   function vec_vsubfp
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vsubfp (To_LL_VF (A), To_LL_VF (B)));
   end vec_vsubfp;

   -----------------
   -- vec_vsubuwm --
   -----------------

   function vec_vsubuwm
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuwm;

   function vec_vsubuwm
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuwm;

   function vec_vsubuwm
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuwm;

   function vec_vsubuwm
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuwm;

   function vec_vsubuwm
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuwm;

   function vec_vsubuwm
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuwm (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuwm;

   -----------------
   -- vec_vsubuhm --
   -----------------

   function vec_vsubuhm
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhm;

   function vec_vsubuhm
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhm;

   function vec_vsubuhm
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhm;

   function vec_vsubuhm
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhm;

   function vec_vsubuhm
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhm;

   function vec_vsubuhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhm (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhm;

   -----------------
   -- vec_vsububm --
   -----------------

   function vec_vsububm
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububm;

   function vec_vsububm
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububm;

   function vec_vsububm
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububm;

   function vec_vsububm
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububm;

   function vec_vsububm
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububm;

   function vec_vsububm
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububm (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububm;

   --------------
   -- vec_subc --
   --------------

   function vec_subc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubcuw (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subc;

   --------------
   -- vec_subs --
   --------------

   function vec_subs
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_subs;

   function vec_subs
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_subs;

   function vec_subs
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_subs;

   function vec_subs
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsubsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_subs;

   function vec_subs
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsubsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_subs;

   function vec_subs
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsubsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_subs;

   function vec_subs
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_subs;

   function vec_subs
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_subs;

   function vec_subs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_subs;

   function vec_subs
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_subs;

   function vec_subs
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_subs;

   function vec_subs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_subs;

   function vec_subs
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subs;

   function vec_subs
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subs;

   function vec_subs
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subs;

   function vec_subs
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subs;

   function vec_subs
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subs;

   function vec_subs
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_subs;

   -----------------
   -- vec_vsubsws --
   -----------------

   function vec_vsubsws
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubsws;

   function vec_vsubsws
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubsws;

   function vec_vsubsws
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsubsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubsws;

   -----------------
   -- vec_vsubuws --
   -----------------

   function vec_vsubuws
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuws;

   function vec_vsubuws
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuws;

   function vec_vsubuws
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsubuws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_vsubuws;

   -----------------
   -- vec_vsubshs --
   -----------------

   function vec_vsubshs
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubshs;

   function vec_vsubshs
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubshs;

   function vec_vsubshs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vsubshs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubshs;

   -----------------
   -- vec_vsubuhs --
   -----------------

   function vec_vsubuhs
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhs;

   function vec_vsubuhs
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhs;

   function vec_vsubuhs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsubuhs (To_LL_VSS (A), To_LL_VSS (B)));
   end vec_vsubuhs;

   -----------------
   -- vec_vsubsbs --
   -----------------

   function vec_vsubsbs
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsubsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsubsbs;

   function vec_vsubsbs
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsubsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsubsbs;

   function vec_vsubsbs
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vsubsbs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsubsbs;

   -----------------
   -- vec_vsububs --
   -----------------

   function vec_vsububs
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububs;

   function vec_vsububs
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububs;

   function vec_vsububs
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsububs (To_LL_VSC (A), To_LL_VSC (B)));
   end vec_vsububs;

   ---------------
   -- vec_sum4s --
   ---------------

   function vec_sum4s
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsum4ubs (To_LL_VSC (A), To_LL_VSI (B)));
   end vec_sum4s;

   function vec_sum4s
     (A : vector_signed_char;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsum4sbs (To_LL_VSC (A), To_LL_VSI (B)));
   end vec_sum4s;

   function vec_sum4s
     (A : vector_signed_short;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsum4shs (To_LL_VSS (A), To_LL_VSI (B)));
   end vec_sum4s;

   ------------------
   -- vec_vsum4shs --
   ------------------

   function vec_vsum4shs
     (A : vector_signed_short;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsum4shs (To_LL_VSS (A), To_LL_VSI (B)));
   end vec_vsum4shs;

   ------------------
   -- vec_vsum4sbs --
   ------------------

   function vec_vsum4sbs
     (A : vector_signed_char;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsum4sbs (To_LL_VSC (A), To_LL_VSI (B)));
   end vec_vsum4sbs;

   ------------------
   -- vec_vsum4ubs --
   ------------------

   function vec_vsum4ubs
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsum4ubs (To_LL_VSC (A), To_LL_VSI (B)));
   end vec_vsum4ubs;

   ---------------
   -- vec_sum2s --
   ---------------

   function vec_sum2s
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsum2sws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sum2s;

   --------------
   -- vec_sums --
   --------------

   function vec_sums
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vsumsws (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_sums;

   ---------------
   -- vec_trunc --
   ---------------

   function vec_trunc
     (A : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vrfiz (To_LL_VF (A)));
   end vec_trunc;

   -----------------
   -- vec_unpackh --
   -----------------

   function vec_unpackh
     (A : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vupkhsb (To_LL_VSC (A)));
   end vec_unpackh;

   function vec_unpackh
     (A : vector_bool_char) return vector_bool_short
   is
   begin
      return To_LL_VBS (vupkhsb (To_LL_VSC (A)));
   end vec_unpackh;

   function vec_unpackh
     (A : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vupkhsh (To_LL_VSS (A)));
   end vec_unpackh;

   function vec_unpackh
     (A : vector_bool_short) return vector_bool_int
   is
   begin
      return To_LL_VBI (vupkhsh (To_LL_VSS (A)));
   end vec_unpackh;

   function vec_unpackh
     (A : vector_pixel) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vupkhpx (To_LL_VSS (A)));
   end vec_unpackh;

   -----------------
   -- vec_vupkhsh --
   -----------------

   function vec_vupkhsh
     (A : vector_bool_short) return vector_bool_int
   is
   begin
      return To_LL_VBI (vupkhsh (To_LL_VSS (A)));
   end vec_vupkhsh;

   function vec_vupkhsh
     (A : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vupkhsh (To_LL_VSS (A)));
   end vec_vupkhsh;

   -----------------
   -- vec_vupkhpx --
   -----------------

   function vec_vupkhpx
     (A : vector_pixel) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vupkhpx (To_LL_VSS (A)));
   end vec_vupkhpx;

   -----------------
   -- vec_vupkhsb --
   -----------------

   function vec_vupkhsb
     (A : vector_bool_char) return vector_bool_short
   is
   begin
      return To_LL_VBS (vupkhsb (To_LL_VSC (A)));
   end vec_vupkhsb;

   function vec_vupkhsb
     (A : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vupkhsb (To_LL_VSC (A)));
   end vec_vupkhsb;

   -----------------
   -- vec_unpackl --
   -----------------

   function vec_unpackl
     (A : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vupklsb (To_LL_VSC (A)));
   end vec_unpackl;

   function vec_unpackl
     (A : vector_bool_char) return vector_bool_short
   is
   begin
      return To_LL_VBS (vupklsb (To_LL_VSC (A)));
   end vec_unpackl;

   function vec_unpackl
     (A : vector_pixel) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vupklpx (To_LL_VSS (A)));
   end vec_unpackl;

   function vec_unpackl
     (A : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vupklsh (To_LL_VSS (A)));
   end vec_unpackl;

   function vec_unpackl
     (A : vector_bool_short) return vector_bool_int
   is
   begin
      return To_LL_VBI (vupklsh (To_LL_VSS (A)));
   end vec_unpackl;

   -----------------
   -- vec_vupklpx --
   -----------------

   function vec_vupklpx
     (A : vector_pixel) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vupklpx (To_LL_VSS (A)));
   end vec_vupklpx;

   -----------------
   -- vec_vupklsh --
   -----------------

   function vec_vupklsh
     (A : vector_bool_short) return vector_bool_int
   is
   begin
      return To_LL_VBI (vupklsh (To_LL_VSS (A)));
   end vec_vupklsh;

   function vec_vupklsh
     (A : vector_signed_short) return vector_signed_int
   is
   begin
      return To_LL_VSI (vupklsh (To_LL_VSS (A)));
   end vec_vupklsh;

   -----------------
   -- vec_vupklsb --
   -----------------

   function vec_vupklsb
     (A : vector_bool_char) return vector_bool_short
   is
   begin
      return To_LL_VBS (vupklsb (To_LL_VSC (A)));
   end vec_vupklsb;

   function vec_vupklsb
     (A : vector_signed_char) return vector_signed_short
   is
   begin
      return To_LL_VSS (vupklsb (To_LL_VSC (A)));
   end vec_vupklsb;

   -------------
   -- vec_xor --
   -------------

   function vec_xor
     (A : vector_float;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_float;
      B : vector_bool_int) return vector_float
   is
   begin
      return To_LL_VF (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_int;
      B : vector_float) return vector_float
   is
   begin
      return To_LL_VF (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   is
   begin
      return To_LL_VSI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   is
   begin
      return To_LL_VBS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   is
   begin
      return To_LL_VSS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   is
   begin
      return To_LL_VBC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   is
   begin
      return To_LL_VSC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   function vec_xor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vxor (To_LL_VSI (A), To_LL_VSI (B)));
   end vec_xor;

   -------------
   -- vec_dst --
   -------------

   procedure vec_dst
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   procedure vec_dst
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dst (To_PTR (A), B, C);
   end vec_dst;

   --------------
   -- vec_dstt --
   --------------

   procedure vec_dstt
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   procedure vec_dstt
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstt (To_PTR (A), B, C);
   end vec_dstt;

   ---------------
   -- vec_dstst --
   ---------------

   procedure vec_dstst
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   procedure vec_dstst
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dstst (To_PTR (A), B, C);
   end vec_dstst;

   ----------------
   -- vec_dststt --
   ----------------

   procedure vec_dststt
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   procedure vec_dststt
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   is
   begin
      dststt (To_PTR (A), B, C);
   end vec_dststt;

   ----------------
   -- vec_vspltw --
   ----------------

   function vec_vspltw
     (A : vector_float;
      B : c_int) return vector_float
   is
   begin
      return To_LL_VF (vspltw (To_LL_VSI (A), B));
   end vec_vspltw;

   function vec_vspltw
     (A : vector_unsigned_int;
      B : c_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vspltw (To_LL_VSI (A), B));
   end vec_vspltw;

   function vec_vspltw
     (A : vector_bool_int;
      B : c_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vspltw (To_LL_VSI (A), B));
   end vec_vspltw;

   ----------------
   -- vec_vsplth --
   ----------------

   function vec_vsplth
     (A : vector_bool_short;
      B : c_int) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsplth (To_LL_VSS (A), B));
   end vec_vsplth;

   function vec_vsplth
     (A : vector_unsigned_short;
      B : c_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsplth (To_LL_VSS (A), B));
   end vec_vsplth;

   function vec_vsplth
     (A : vector_pixel;
      B : c_int) return vector_pixel
   is
   begin
      return To_LL_VP (vsplth (To_LL_VSS (A), B));
   end vec_vsplth;

   ----------------
   -- vec_vspltb --
   ----------------

   function vec_vspltb
     (A : vector_unsigned_char;
      B : c_int) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vspltb (To_LL_VSC (A), B));
   end vec_vspltb;

   function vec_vspltb
     (A : vector_bool_char;
      B : c_int) return vector_bool_char
   is
   begin
      return To_LL_VBC (vspltb (To_LL_VSC (A), B));
   end vec_vspltb;

   ------------------
   -- vec_splat_u8 --
   ------------------

   function vec_splat_u8
     (A : c_int) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vspltisb (A));
   end vec_splat_u8;

   -------------------
   -- vec_splat_u16 --
   -------------------

   function vec_splat_u16
     (A : c_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vspltish (A));
   end vec_splat_u16;

   -------------------
   -- vec_splat_u32 --
   -------------------

   function vec_splat_u32
     (A : c_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vspltisw (A));
   end vec_splat_u32;

   -------------
   -- vec_sld --
   -------------

   function vec_sld
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : c_int) return vector_unsigned_int
   is
   begin
      return To_LL_VUI (vsldoi_4si (To_LL_VSI (A), To_LL_VSI (B), C));
   end vec_sld;

   function vec_sld
     (A : vector_bool_int;
      B : vector_bool_int;
      C : c_int) return vector_bool_int
   is
   begin
      return To_LL_VBI (vsldoi_4si (To_LL_VSI (A), To_LL_VSI (B), C));
   end vec_sld;

   function vec_sld
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : c_int) return vector_unsigned_short
   is
   begin
      return To_LL_VUS (vsldoi_8hi (To_LL_VSS (A), To_LL_VSS (B), C));
   end vec_sld;

   function vec_sld
     (A : vector_bool_short;
      B : vector_bool_short;
      C : c_int) return vector_bool_short
   is
   begin
      return To_LL_VBS (vsldoi_8hi (To_LL_VSS (A), To_LL_VSS (B), C));
   end vec_sld;

   function vec_sld
     (A : vector_pixel;
      B : vector_pixel;
      C : c_int) return vector_pixel
   is
   begin
      return To_LL_VP (vsldoi_8hi (To_LL_VSS (A), To_LL_VSS (B), C));
   end vec_sld;

   function vec_sld
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : c_int) return vector_unsigned_char
   is
   begin
      return To_LL_VUC (vsldoi_16qi (To_LL_VSC (A), To_LL_VSC (B), C));
   end vec_sld;

   function vec_sld
     (A : vector_bool_char;
      B : vector_bool_char;
      C : c_int) return vector_bool_char
   is
   begin
      return To_LL_VBC (vsldoi_16qi (To_LL_VSC (A), To_LL_VSC (B), C));
   end vec_sld;

   ----------------
   -- vec_all_eq --
   ----------------

   function vec_all_eq
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_pixel;
      B : vector_pixel) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_eq;

   function vec_all_eq
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_LT, To_LL_VF (A), To_LL_VF (B));
   end vec_all_eq;

   ----------------
   -- vec_all_ge --
   ----------------

   function vec_all_ge
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_ge;

   function vec_all_ge
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_LT, To_LL_VF (A), To_LL_VF (B));
   end vec_all_ge;

   ----------------
   -- vec_all_gt --
   ----------------

   function vec_all_gt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_gt;

   function vec_all_gt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_LT, To_LL_VF (A), To_LL_VF (B));
   end vec_all_gt;

   ----------------
   -- vec_all_in --
   ----------------

   function vec_all_in
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpbfp_p (CR6_EQ, To_LL_VF (A), To_LL_VF (B));
   end vec_all_in;

   ----------------
   -- vec_all_le --
   ----------------

   function vec_all_le
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_le;

   function vec_all_le
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_LT, To_LL_VF (B), To_LL_VF (A));
   end vec_all_le;

   ----------------
   -- vec_all_lt --
   ----------------

   function vec_all_lt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT, To_LL_VSC (B), To_LL_VSC (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT, To_LL_VSS (B), To_LL_VSS (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT, To_LL_VSI (B), To_LL_VSI (A));
   end vec_all_lt;

   function vec_all_lt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_LT, To_LL_VF (B), To_LL_VF (A));
   end vec_all_lt;

   -----------------
   -- vec_all_nan --
   -----------------

   function vec_all_nan
     (A : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_EQ, To_LL_VF (A), To_LL_VF (A));
   end vec_all_nan;

   ----------------
   -- vec_all_ne --
   ----------------

   function vec_all_ne
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ, To_LL_VSC (A), To_LL_VSC (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_pixel;
      B : vector_pixel) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ, To_LL_VSS (A), To_LL_VSS (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ, To_LL_VSI (A), To_LL_VSI (B));
   end vec_all_ne;

   function vec_all_ne
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_EQ, To_LL_VF (A), To_LL_VF (B));
   end vec_all_ne;

   -----------------
   -- vec_all_nge --
   -----------------

   function vec_all_nge
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_EQ, To_LL_VF (A), To_LL_VF (B));
   end vec_all_nge;

   -----------------
   -- vec_all_ngt --
   -----------------

   function vec_all_ngt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_EQ, To_LL_VF (A), To_LL_VF (B));
   end vec_all_ngt;

   -----------------
   -- vec_all_nle --
   -----------------

   function vec_all_nle
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_EQ, To_LL_VF (B), To_LL_VF (A));
   end vec_all_nle;

   -----------------
   -- vec_all_nlt --
   -----------------

   function vec_all_nlt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_EQ, To_LL_VF (B), To_LL_VF (A));
   end vec_all_nlt;

   ---------------------
   -- vec_all_numeric --
   ---------------------

   function vec_all_numeric
     (A : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_LT, To_LL_VF (A), To_LL_VF (A));
   end vec_all_numeric;

   ----------------
   -- vec_any_eq --
   ----------------

   function vec_any_eq
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_pixel;
      B : vector_pixel) return c_int
   is
   begin
      return vcmpequh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_eq;

   function vec_any_eq
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_EQ_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_eq;

   ----------------
   -- vec_any_ge --
   ----------------

   function vec_any_ge
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_ge;

   function vec_any_ge
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_EQ_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_ge;

   ----------------
   -- vec_any_gt --
   ----------------

   function vec_any_gt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_gt;

   function vec_any_gt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_EQ_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_gt;

   ----------------
   -- vec_any_le --
   ----------------

   function vec_any_le
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_le;

   function vec_any_le
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_EQ_REV, To_LL_VF (B), To_LL_VF (A));
   end vec_any_le;

   ----------------
   -- vec_any_lt --
   ----------------

   function vec_any_lt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpgtub_p (CR6_EQ_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpgtsb_p (CR6_EQ_REV, To_LL_VSC (B), To_LL_VSC (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpgtuh_p (CR6_EQ_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpgtsh_p (CR6_EQ_REV, To_LL_VSS (B), To_LL_VSS (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpgtuw_p (CR6_EQ_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpgtsw_p (CR6_EQ_REV, To_LL_VSI (B), To_LL_VSI (A));
   end vec_any_lt;

   function vec_any_lt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_EQ_REV, To_LL_VF (B), To_LL_VF (A));
   end vec_any_lt;

   -----------------
   -- vec_any_nan --
   -----------------

   function vec_any_nan
     (A : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_LT_REV, To_LL_VF (A), To_LL_VF (A));
   end vec_any_nan;

   ----------------
   -- vec_any_ne --
   ----------------

   function vec_any_ne
     (A : vector_signed_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_signed_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_char;
      B : vector_bool_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_char;
      B : vector_signed_char) return c_int
   is
   begin
      return vcmpequb_p (CR6_LT_REV, To_LL_VSC (A), To_LL_VSC (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_signed_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_signed_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_short;
      B : vector_bool_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_short;
      B : vector_signed_short) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_pixel;
      B : vector_pixel) return c_int
   is
   begin
      return vcmpequh_p (CR6_LT_REV, To_LL_VSS (A), To_LL_VSS (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_signed_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_signed_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_int;
      B : vector_bool_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_bool_int;
      B : vector_signed_int) return c_int
   is
   begin
      return vcmpequw_p (CR6_LT_REV, To_LL_VSI (A), To_LL_VSI (B));
   end vec_any_ne;

   function vec_any_ne
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_LT_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_ne;

   -----------------
   -- vec_any_nge --
   -----------------

   function vec_any_nge
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_LT_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_nge;

   -----------------
   -- vec_any_ngt --
   -----------------

   function vec_any_ngt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_LT_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_ngt;

   -----------------
   -- vec_any_nle --
   -----------------

   function vec_any_nle
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgefp_p (CR6_LT_REV, To_LL_VF (B), To_LL_VF (A));
   end vec_any_nle;

   -----------------
   -- vec_any_nlt --
   -----------------

   function vec_any_nlt
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpgtfp_p (CR6_LT_REV, To_LL_VF (B), To_LL_VF (A));
   end vec_any_nlt;

   ---------------------
   -- vec_any_numeric --
   ---------------------

   function vec_any_numeric
     (A : vector_float) return c_int
   is
   begin
      return vcmpeqfp_p (CR6_EQ_REV, To_LL_VF (A), To_LL_VF (A));
   end vec_any_numeric;

   -----------------
   -- vec_any_out --
   -----------------

   function vec_any_out
     (A : vector_float;
      B : vector_float) return c_int
   is
   begin
      return vcmpbfp_p (CR6_EQ_REV, To_LL_VF (A), To_LL_VF (B));
   end vec_any_out;

   --------------
   -- vec_step --
   --------------

   function vec_step
     (V : vector_unsigned_char) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 16;
   end vec_step;

   function vec_step
     (V : vector_signed_char) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 16;
   end vec_step;

   function vec_step
     (V : vector_bool_char) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 16;
   end vec_step;

   function vec_step
     (V : vector_unsigned_short) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 8;
   end vec_step;

   function vec_step
     (V : vector_signed_short) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 8;
   end vec_step;

   function vec_step
     (V : vector_bool_short) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 8;
   end vec_step;

   function vec_step
     (V : vector_unsigned_int) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 4;
   end vec_step;

   function vec_step
     (V : vector_signed_int) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 4;
   end vec_step;

   function vec_step
     (V : vector_bool_int) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 4;
   end vec_step;

   function vec_step
     (V : vector_float) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 4;
   end vec_step;

   function vec_step
     (V : vector_pixel) return Integer
   is
      pragma Unreferenced (V);
   begin
      return 4;
   end vec_step;

end GNAT.Altivec.Vector_Operations;
