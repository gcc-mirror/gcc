------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     G N A T . A L T I V E C . L O W _ L E V E L _ I N T E R F A C E      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2009, Free Software Foundation, Inc.         --
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

--  The set of "services" includes:
--
--  o Imports to the low level routines for which a direct binding is
--    mandatory (or just possible when analyzed as such).
--
--  o Conversion routines (unchecked) between low level types, or between
--    various pointer representations.

with GNAT.Altivec.Vector_Types;
with GNAT.Altivec.Low_Level_Vectors;

with Ada.Unchecked_Conversion;

package GNAT.Altivec.Low_Level_Interface is

   ----------------------------------------------------------------------------
   -- Imports for "argument must be literal" constraints in the Hard binding --
   ----------------------------------------------------------------------------

   use GNAT.Altivec.Vector_Types;

   -- vec_ctf --

   function vec_ctf_vui_cint_r_vf
     (A : vector_unsigned_int;
      B : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_ctf_vui_cint_r_vf, "__builtin_altivec_vcfux");

   function vec_ctf_vsi_cint_r_vf
     (A : vector_signed_int;
      B : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_ctf_vsi_cint_r_vf, "__builtin_altivec_vcfsx");

   -- vec_vcfsx --

   function vec_vcfsx_vsi_cint_r_vf
     (A : vector_signed_int;
      B : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_vcfsx_vsi_cint_r_vf, "__builtin_altivec_vcfsx");

   -- vec_vcfux --

   function vec_vcfux_vui_cint_r_vf
     (A : vector_unsigned_int;
      B : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_vcfux_vui_cint_r_vf, "__builtin_altivec_vcfux");

   -- vec_cts --

   function vec_cts_vf_cint_r_vsi
     (A : vector_float;
      B : c_int) return vector_signed_int;

   pragma Import
     (LL_Altivec, vec_cts_vf_cint_r_vsi, "__builtin_altivec_vctsxs");

   -- vec_ctu --

   function vec_ctu_vf_cint_r_vui
     (A : vector_float;
      B : c_int) return vector_unsigned_int;

   pragma Import
     (LL_Altivec, vec_ctu_vf_cint_r_vui, "__builtin_altivec_vctuxs");

   -- vec_dss --

   procedure vec_dss_cint
     (A : c_int);

   pragma Import
     (LL_Altivec, vec_dss_cint, "__builtin_altivec_dss");

   -- vec_dst --

   procedure vec_dst_kvucp_cint_cint
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvucp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvscp_cint_cint
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvscp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvbcp_cint_cint
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvbcp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvusp_cint_cint
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvusp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvssp_cint_cint
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvssp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvbsp_cint_cint
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvbsp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvxp_cint_cint
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvxp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvuip_cint_cint
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvuip_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvsip_cint_cint
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvsip_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvbip_cint_cint
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvbip_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kvfp_cint_cint
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kvfp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kucp_cint_cint
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kucp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kscp_cint_cint
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kscp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kusp_cint_cint
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kusp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_ksp_cint_cint
     (A : const_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_ksp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kuip_cint_cint
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kuip_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kip_cint_cint
     (A : const_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kip_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kulongp_cint_cint
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kulongp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_klongp_cint_cint
     (A : const_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_klongp_cint_cint, "__builtin_altivec_dst");

   procedure vec_dst_kfp_cint_cint
     (A : const_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dst_kfp_cint_cint, "__builtin_altivec_dst");

   -- vec_dstst --

   procedure vec_dstst_kvucp_cint_cint
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvucp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvscp_cint_cint
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvscp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvbcp_cint_cint
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvbcp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvusp_cint_cint
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvusp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvssp_cint_cint
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvssp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvbsp_cint_cint
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvbsp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvxp_cint_cint
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvxp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvuip_cint_cint
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvuip_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvsip_cint_cint
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvsip_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvbip_cint_cint
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvbip_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kvfp_cint_cint
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kvfp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kucp_cint_cint
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kucp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kscp_cint_cint
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kscp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kusp_cint_cint
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kusp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_ksp_cint_cint
     (A : const_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_ksp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kuip_cint_cint
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kuip_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kip_cint_cint
     (A : const_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kip_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kulongp_cint_cint
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kulongp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_klongp_cint_cint
     (A : const_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_klongp_cint_cint, "__builtin_altivec_dstst");

   procedure vec_dstst_kfp_cint_cint
     (A : const_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstst_kfp_cint_cint, "__builtin_altivec_dstst");

   -- vec_dststt --

   procedure vec_dststt_kvucp_cint_cint
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvucp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvscp_cint_cint
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvscp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvbcp_cint_cint
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvbcp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvusp_cint_cint
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvusp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvssp_cint_cint
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvssp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvbsp_cint_cint
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvbsp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvxp_cint_cint
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvxp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvuip_cint_cint
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvuip_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvsip_cint_cint
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvsip_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvbip_cint_cint
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvbip_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kvfp_cint_cint
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kvfp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kucp_cint_cint
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kucp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kscp_cint_cint
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kscp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kusp_cint_cint
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kusp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_ksp_cint_cint
     (A : const_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_ksp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kuip_cint_cint
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kuip_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kip_cint_cint
     (A : const_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kip_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kulongp_cint_cint
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kulongp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_klongp_cint_cint
     (A : const_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_klongp_cint_cint, "__builtin_altivec_dststt");

   procedure vec_dststt_kfp_cint_cint
     (A : const_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dststt_kfp_cint_cint, "__builtin_altivec_dststt");

   -- vec_dstt --

   procedure vec_dstt_kvucp_cint_cint
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvucp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvscp_cint_cint
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvscp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvbcp_cint_cint
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvbcp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvusp_cint_cint
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvusp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvssp_cint_cint
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvssp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvbsp_cint_cint
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvbsp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvxp_cint_cint
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvxp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvuip_cint_cint
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvuip_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvsip_cint_cint
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvsip_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvbip_cint_cint
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvbip_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kvfp_cint_cint
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kvfp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kucp_cint_cint
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kucp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kscp_cint_cint
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kscp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kusp_cint_cint
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kusp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_ksp_cint_cint
     (A : const_short_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_ksp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kuip_cint_cint
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kuip_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kip_cint_cint
     (A : const_int_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kip_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kulongp_cint_cint
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kulongp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_klongp_cint_cint
     (A : const_long_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_klongp_cint_cint, "__builtin_altivec_dstt");

   procedure vec_dstt_kfp_cint_cint
     (A : const_float_ptr;
      B : c_int;
      C : c_int);

   pragma Import
     (LL_Altivec, vec_dstt_kfp_cint_cint, "__builtin_altivec_dstt");

   -- vec_sld --

   --  ??? The base GCC implementation maps everything to vsldoi_4si, while
   --  it defines builtin variants for all the modes. Adjust here, to avoid
   --  the infamous argument mode mismatch.

   function vec_sld_vf_vf_cint_r_vf
     (A : vector_float;
      B : vector_float;
      C : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_sld_vf_vf_cint_r_vf, "__builtin_altivec_vsldoi_4sf");

   function vec_sld_vsi_vsi_cint_r_vsi
     (A : vector_signed_int;
      B : vector_signed_int;
      C : c_int) return vector_signed_int;

   pragma Import
     (LL_Altivec, vec_sld_vsi_vsi_cint_r_vsi, "__builtin_altivec_vsldoi_4si");

   function vec_sld_vui_vui_cint_r_vui
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : c_int) return vector_unsigned_int;

   pragma Import
     (LL_Altivec, vec_sld_vui_vui_cint_r_vui, "__builtin_altivec_vsldoi_4si");

   function vec_sld_vbi_vbi_cint_r_vbi
     (A : vector_bool_int;
      B : vector_bool_int;
      C : c_int) return vector_bool_int;

   pragma Import
     (LL_Altivec, vec_sld_vbi_vbi_cint_r_vbi, "__builtin_altivec_vsldoi_4si");

   function vec_sld_vss_vss_cint_r_vss
     (A : vector_signed_short;
      B : vector_signed_short;
      C : c_int) return vector_signed_short;

   pragma Import
     (LL_Altivec, vec_sld_vss_vss_cint_r_vss, "__builtin_altivec_vsldoi_8hi");

   function vec_sld_vus_vus_cint_r_vus
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : c_int) return vector_unsigned_short;

   pragma Import
     (LL_Altivec, vec_sld_vus_vus_cint_r_vus, "__builtin_altivec_vsldoi_8hi");

   function vec_sld_vbs_vbs_cint_r_vbs
     (A : vector_bool_short;
      B : vector_bool_short;
      C : c_int) return vector_bool_short;

   pragma Import
     (LL_Altivec, vec_sld_vbs_vbs_cint_r_vbs, "__builtin_altivec_vsldoi_8hi");

   function vec_sld_vx_vx_cint_r_vx
     (A : vector_pixel;
      B : vector_pixel;
      C : c_int) return vector_pixel;

   pragma Import
     (LL_Altivec, vec_sld_vx_vx_cint_r_vx, "__builtin_altivec_vsldoi_4si");

   function vec_sld_vsc_vsc_cint_r_vsc
     (A : vector_signed_char;
      B : vector_signed_char;
      C : c_int) return vector_signed_char;

   pragma Import
     (LL_Altivec, vec_sld_vsc_vsc_cint_r_vsc, "__builtin_altivec_vsldoi_16qi");

   function vec_sld_vuc_vuc_cint_r_vuc
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : c_int) return vector_unsigned_char;

   pragma Import
     (LL_Altivec, vec_sld_vuc_vuc_cint_r_vuc, "__builtin_altivec_vsldoi_16qi");

   function vec_sld_vbc_vbc_cint_r_vbc
     (A : vector_bool_char;
      B : vector_bool_char;
      C : c_int) return vector_bool_char;

   pragma Import
     (LL_Altivec, vec_sld_vbc_vbc_cint_r_vbc, "__builtin_altivec_vsldoi_16qi");

   -- vec_splat --

   function vec_splat_vsc_cint_r_vsc
     (A : vector_signed_char;
      B : c_int) return vector_signed_char;

   pragma Import
     (LL_Altivec, vec_splat_vsc_cint_r_vsc, "__builtin_altivec_vspltb");

   function vec_splat_vuc_cint_r_vuc
     (A : vector_unsigned_char;
      B : c_int) return vector_unsigned_char;

   pragma Import
     (LL_Altivec, vec_splat_vuc_cint_r_vuc, "__builtin_altivec_vspltb");

   function vec_splat_vbc_cint_r_vbc
     (A : vector_bool_char;
      B : c_int) return vector_bool_char;

   pragma Import
     (LL_Altivec, vec_splat_vbc_cint_r_vbc, "__builtin_altivec_vspltb");

   function vec_splat_vss_cint_r_vss
     (A : vector_signed_short;
      B : c_int) return vector_signed_short;

   pragma Import
     (LL_Altivec, vec_splat_vss_cint_r_vss, "__builtin_altivec_vsplth");

   function vec_splat_vus_cint_r_vus
     (A : vector_unsigned_short;
      B : c_int) return vector_unsigned_short;

   pragma Import
     (LL_Altivec, vec_splat_vus_cint_r_vus, "__builtin_altivec_vsplth");

   function vec_splat_vbs_cint_r_vbs
     (A : vector_bool_short;
      B : c_int) return vector_bool_short;

   pragma Import
     (LL_Altivec, vec_splat_vbs_cint_r_vbs, "__builtin_altivec_vsplth");

   function vec_splat_vx_cint_r_vx
     (A : vector_pixel;
      B : c_int) return vector_pixel;

   pragma Import
     (LL_Altivec, vec_splat_vx_cint_r_vx, "__builtin_altivec_vsplth");

   function vec_splat_vf_cint_r_vf
     (A : vector_float;
      B : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_splat_vf_cint_r_vf, "__builtin_altivec_vspltw");

   function vec_splat_vsi_cint_r_vsi
     (A : vector_signed_int;
      B : c_int) return vector_signed_int;

   pragma Import
     (LL_Altivec, vec_splat_vsi_cint_r_vsi, "__builtin_altivec_vspltw");

   function vec_splat_vui_cint_r_vui
     (A : vector_unsigned_int;
      B : c_int) return vector_unsigned_int;

   pragma Import
     (LL_Altivec, vec_splat_vui_cint_r_vui, "__builtin_altivec_vspltw");

   function vec_splat_vbi_cint_r_vbi
     (A : vector_bool_int;
      B : c_int) return vector_bool_int;

   pragma Import
     (LL_Altivec, vec_splat_vbi_cint_r_vbi, "__builtin_altivec_vspltw");

   -- vec_vspltw --

   function vec_vspltw_vf_cint_r_vf
     (A : vector_float;
      B : c_int) return vector_float;

   pragma Import
     (LL_Altivec, vec_vspltw_vf_cint_r_vf, "__builtin_altivec_vspltw");

   function vec_vspltw_vsi_cint_r_vsi
     (A : vector_signed_int;
      B : c_int) return vector_signed_int;

   pragma Import
     (LL_Altivec, vec_vspltw_vsi_cint_r_vsi, "__builtin_altivec_vspltw");

   function vec_vspltw_vui_cint_r_vui
     (A : vector_unsigned_int;
      B : c_int) return vector_unsigned_int;

   pragma Import
     (LL_Altivec, vec_vspltw_vui_cint_r_vui, "__builtin_altivec_vspltw");

   function vec_vspltw_vbi_cint_r_vbi
     (A : vector_bool_int;
      B : c_int) return vector_bool_int;

   pragma Import
     (LL_Altivec, vec_vspltw_vbi_cint_r_vbi, "__builtin_altivec_vspltw");

   -- vec_vsplth --

   function vec_vsplth_vbs_cint_r_vbs
     (A : vector_bool_short;
      B : c_int) return vector_bool_short;

   pragma Import
     (LL_Altivec, vec_vsplth_vbs_cint_r_vbs, "__builtin_altivec_vsplth");

   function vec_vsplth_vss_cint_r_vss
     (A : vector_signed_short;
      B : c_int) return vector_signed_short;

   pragma Import
     (LL_Altivec, vec_vsplth_vss_cint_r_vss, "__builtin_altivec_vsplth");

   function vec_vsplth_vus_cint_r_vus
     (A : vector_unsigned_short;
      B : c_int) return vector_unsigned_short;

   pragma Import
     (LL_Altivec, vec_vsplth_vus_cint_r_vus, "__builtin_altivec_vsplth");

   function vec_vsplth_vx_cint_r_vx
     (A : vector_pixel;
      B : c_int) return vector_pixel;

   pragma Import
     (LL_Altivec, vec_vsplth_vx_cint_r_vx, "__builtin_altivec_vsplth");

   -- vec_vspltb --

   function vec_vspltb_vsc_cint_r_vsc
     (A : vector_signed_char;
      B : c_int) return vector_signed_char;

   pragma Import
     (LL_Altivec, vec_vspltb_vsc_cint_r_vsc, "__builtin_altivec_vspltb");

   function vec_vspltb_vuc_cint_r_vuc
     (A : vector_unsigned_char;
      B : c_int) return vector_unsigned_char;

   pragma Import
     (LL_Altivec, vec_vspltb_vuc_cint_r_vuc, "__builtin_altivec_vspltb");

   function vec_vspltb_vbc_cint_r_vbc
     (A : vector_bool_char;
      B : c_int) return vector_bool_char;

   pragma Import
     (LL_Altivec, vec_vspltb_vbc_cint_r_vbc, "__builtin_altivec_vspltb");

   -- vec_splat_s8 --

   function vec_splat_s8_cint_r_vsc
     (A : c_int) return vector_signed_char;

   pragma Import
     (LL_Altivec, vec_splat_s8_cint_r_vsc, "__builtin_altivec_vspltisb");

   -- vec_splat_s16 --

   function vec_splat_s16_cint_r_vss
     (A : c_int) return vector_signed_short;

   pragma Import
     (LL_Altivec, vec_splat_s16_cint_r_vss, "__builtin_altivec_vspltish");

   -- vec_splat_s32 --

   function vec_splat_s32_cint_r_vsi
     (A : c_int) return vector_signed_int;

   pragma Import
     (LL_Altivec, vec_splat_s32_cint_r_vsi, "__builtin_altivec_vspltisw");

   -- vec_splat_u8 --

   function vec_splat_u8_cint_r_vuc
     (A : c_int) return vector_unsigned_char;

   pragma Import
     (LL_Altivec, vec_splat_u8_cint_r_vuc, "__builtin_altivec_vspltisb");

   -- vec_splat_u16 --

   function vec_splat_u16_cint_r_vus
     (A : c_int) return vector_unsigned_short;

   pragma Import
     (LL_Altivec, vec_splat_u16_cint_r_vus, "__builtin_altivec_vspltish");

   -- vec_splat_u32 --

   function vec_splat_u32_cint_r_vui
     (A : c_int) return vector_unsigned_int;

   pragma Import
     (LL_Altivec, vec_splat_u32_cint_r_vui, "__builtin_altivec_vspltisw");

   ------------------------------------------------------------
   -- Imports for low-level signature consistent subprograms --
   ------------------------------------------------------------

   -- vec_dssall --

   procedure vec_dssall;

   pragma Import
     (LL_Altivec, vec_dssall, "__builtin_altivec_dssall");

   -----------------------------------------
   -- Conversions between low level types --
   -----------------------------------------

   use GNAT.Altivec.Low_Level_Vectors;

   --  Something like...
   --
   --  TYPES="LL_VBC LL_VUC LL_VSC LL_VBS LL_VUS LL_VSS \
   --         LL_VBI LL_VUI LL_VSI LL_VF LL_VP"
   --  for TT in `echo $TYPES`; do
   --  for ST in `echo $TYPES`; do
   --  echo "function To_$TT is new Ada.Unchecked_Conversion ($ST, $TT);"
   --  done
   --  echo ""
   --  done

   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VBC, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VUC, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VSC, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VBS, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VUS, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VSS, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VBI, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VUI, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VSI, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VF, LL_VBC);
   function To_LL_VBC is new Ada.Unchecked_Conversion (LL_VP, LL_VBC);

   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VBC, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VUC, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VSC, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VBS, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VUS, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VSS, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VBI, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VUI, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VSI, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VF, LL_VUC);
   function To_LL_VUC is new Ada.Unchecked_Conversion (LL_VP, LL_VUC);

   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VBC, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VUC, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VSC, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VBS, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VUS, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VSS, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VBI, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VUI, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VSI, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VF, LL_VSC);
   function To_LL_VSC is new Ada.Unchecked_Conversion (LL_VP, LL_VSC);

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
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VF, LL_VUS);
   function To_LL_VUS is new Ada.Unchecked_Conversion (LL_VP, LL_VUS);

   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VBC, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VUC, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VSC, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VBS, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VUS, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VSS, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VBI, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VUI, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VSI, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VF, LL_VSS);
   function To_LL_VSS is new Ada.Unchecked_Conversion (LL_VP, LL_VSS);

   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VBC, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VUC, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VSC, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VBS, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VUS, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VSS, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VBI, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VUI, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VSI, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VF, LL_VBI);
   function To_LL_VBI is new Ada.Unchecked_Conversion (LL_VP, LL_VBI);

   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VBC, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VUC, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VSC, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VBS, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VUS, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VSS, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VBI, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VUI, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VSI, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VF, LL_VUI);
   function To_LL_VUI is new Ada.Unchecked_Conversion (LL_VP, LL_VUI);

   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VBC, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VUC, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VSC, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VBS, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VUS, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VSS, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VBI, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VUI, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VSI, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VF, LL_VSI);
   function To_LL_VSI is new Ada.Unchecked_Conversion (LL_VP, LL_VSI);

   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VBC, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VUC, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VSC, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VBS, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VUS, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VSS, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VBI, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VUI, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VSI, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VF, LL_VF);
   function To_LL_VF is new Ada.Unchecked_Conversion (LL_VP, LL_VF);

   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VBC, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VUC, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VSC, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VBS, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VUS, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VSS, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VBI, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VUI, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VSI, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VF, LL_VP);
   function To_LL_VP is new Ada.Unchecked_Conversion (LL_VP, LL_VP);

   ----------------------------------------------
   -- Conversions between pointer/access types --
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
