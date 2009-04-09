------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--       G N A T . A L T I V E C . V E C T O R _ O P E R A T I O N S        --
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

--  This unit is the user-level Ada interface to AltiVec operations on vector
--  objects. It is common to both the Soft and the Hard bindings.

with GNAT.Altivec.Vector_Types;  use GNAT.Altivec.Vector_Types;
with GNAT.Altivec.Low_Level_Interface;  use GNAT.Altivec.Low_Level_Interface;

package GNAT.Altivec.Vector_Operations is

   --  The vast majority of the operations exposed here are overloads over a
   --  much smaller set of low level primitives with type conversions around.
   --
   --  In some cases, a direct binding without any intermediate body is
   --  possible or even mandatory for technical reasons. What we provide
   --  here for such cases are renamings of straight imports exposed by
   --  Altivec.Low_Level_Interface.  See the comments in the private part for
   --  additional details.

   -------------------------------------------------------
   -- [PIM-4.4 Generic and Specific AltiVec operations] --
   -------------------------------------------------------

   -- vec_abs --

   function vec_abs
     (A : vector_signed_char) return vector_signed_char;

   function vec_abs
     (A : vector_signed_short) return vector_signed_short;

   function vec_abs
     (A : vector_signed_int) return vector_signed_int;

   function vec_abs
     (A : vector_float) return vector_float;

   -- vec_abss --

   function vec_abss
     (A : vector_signed_char) return vector_signed_char;

   function vec_abss
     (A : vector_signed_short) return vector_signed_short;

   function vec_abss
     (A : vector_signed_int) return vector_signed_int;

   -- vec_add --

   function vec_add
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_add
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_add
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_add
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_add
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_add
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_add
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_add
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_add
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_add
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_add
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_add
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_add
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_add
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_add
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_add
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_add
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_add
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_add
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vaddfp --

   function vec_vaddfp
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vadduwm --

   function vec_vadduwm
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vadduwm
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_vadduwm
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vadduwm
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vadduwm
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_vadduwm
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vadduhm --

   function vec_vadduhm
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vadduhm
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_vadduhm
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vadduhm
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vadduhm
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_vadduhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vaddubm --

   function vec_vaddubm
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vaddubm
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_vaddubm
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vaddubm
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_vaddubm
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_vaddubm
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_addc --

   function vec_addc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_adds --

   function vec_adds
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_adds
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_adds
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_adds
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_adds
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_adds
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_adds
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_adds
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_adds
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_adds
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_adds
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_adds
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_adds
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_adds
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_adds
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_adds
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_adds
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_adds
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vaddsws --

   function vec_vaddsws
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vaddsws
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_vaddsws
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vadduws --

   function vec_vadduws
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vadduws
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_vadduws
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vaddshs --

   function vec_vaddshs
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vaddshs
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_vaddshs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   -- vec_vadduhs --

   function vec_vadduhs
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vadduhs
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_vadduhs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vaddsbs --

   function vec_vaddsbs
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vaddsbs
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_vaddsbs
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   -- vec_vaddubs --

   function vec_vaddubs
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_vaddubs
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_vaddubs
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_and --

   function vec_and
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_and
     (A : vector_float;
      B : vector_bool_int) return vector_float;

   function vec_and
     (A : vector_bool_int;
      B : vector_float) return vector_float;

   function vec_and
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_and
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_and
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_and
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_and
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_and
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_and
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_and
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_and
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_and
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_and
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_and
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_and
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_and
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_and
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_and
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_and
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_and
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_and
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_and
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_and
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_andc --

   function vec_andc
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_andc
     (A : vector_float;
      B : vector_bool_int) return vector_float;

   function vec_andc
     (A : vector_bool_int;
      B : vector_float) return vector_float;

   function vec_andc
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_andc
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_andc
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_andc
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_andc
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_andc
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_andc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_andc
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_andc
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_andc
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_andc
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_andc
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_andc
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_andc
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_andc
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_andc
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_andc
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_andc
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_andc
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_andc
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_andc
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_avg --

   function vec_avg
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_avg
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_avg
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_avg
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_avg
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_avg
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vavgsw --

   function vec_vavgsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vavguw --

   function vec_vavguw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vavgsh --

   function vec_vavgsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   -- vec_vavguh --

   function vec_vavguh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vavgsb --

   function vec_vavgsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   -- vec_vavgub --

   function vec_vavgub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_ceil --

   function vec_ceil
     (A : vector_float) return vector_float;

   -- vec_cmpb --

   function vec_cmpb
     (A : vector_float;
      B : vector_float) return vector_signed_int;

   -- vec_cmpeq --

   function vec_cmpeq
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char;

   function vec_cmpeq
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char;

   function vec_cmpeq
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short;

   function vec_cmpeq
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short;

   function vec_cmpeq
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int;

   function vec_cmpeq
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int;

   function vec_cmpeq
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_vcmpeqfp --

   function vec_vcmpeqfp
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_vcmpequw --

   function vec_vcmpequw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int;

   function vec_vcmpequw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int;

   -- vec_vcmpequh --

   function vec_vcmpequh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short;

   function vec_vcmpequh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short;

   -- vec_vcmpequb --

   function vec_vcmpequb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char;

   function vec_vcmpequb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char;

   -- vec_cmpge --

   function vec_cmpge
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_cmpgt --

   function vec_cmpgt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char;

   function vec_cmpgt
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char;

   function vec_cmpgt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short;

   function vec_cmpgt
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short;

   function vec_cmpgt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int;

   function vec_cmpgt
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int;

   function vec_cmpgt
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_vcmpgtfp --

   function vec_vcmpgtfp
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_vcmpgtsw --

   function vec_vcmpgtsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int;

   -- vec_vcmpgtuw --

   function vec_vcmpgtuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int;

   -- vec_vcmpgtsh --

   function vec_vcmpgtsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short;

   -- vec_vcmpgtuh --

   function vec_vcmpgtuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short;

   -- vec_vcmpgtsb --

   function vec_vcmpgtsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char;

   -- vec_vcmpgtub --

   function vec_vcmpgtub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char;

   -- vec_cmple --

   function vec_cmple
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_cmplt --

   function vec_cmplt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_bool_char;

   function vec_cmplt
     (A : vector_signed_char;
      B : vector_signed_char) return vector_bool_char;

   function vec_cmplt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_bool_short;

   function vec_cmplt
     (A : vector_signed_short;
      B : vector_signed_short) return vector_bool_short;

   function vec_cmplt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_bool_int;

   function vec_cmplt
     (A : vector_signed_int;
      B : vector_signed_int) return vector_bool_int;

   function vec_cmplt
     (A : vector_float;
      B : vector_float) return vector_bool_int;

   -- vec_ctf --

   function vec_ctf
     (A : vector_unsigned_int;
      B : c_int) return vector_float
   renames Low_Level_Interface.vec_ctf_vui_cint_r_vf;

   function vec_ctf
     (A : vector_signed_int;
      B : c_int) return vector_float
   renames Low_Level_Interface.vec_ctf_vsi_cint_r_vf;

   -- vec_vcfsx --

   function vec_vcfsx
     (A : vector_signed_int;
      B : c_int) return vector_float
   renames Low_Level_Interface.vec_vcfsx_vsi_cint_r_vf;

   -- vec_vcfux --

   function vec_vcfux
     (A : vector_unsigned_int;
      B : c_int) return vector_float
   renames Low_Level_Interface.vec_vcfux_vui_cint_r_vf;

   -- vec_cts --

   function vec_cts
     (A : vector_float;
      B : c_int) return vector_signed_int
   renames Low_Level_Interface.vec_cts_vf_cint_r_vsi;

   -- vec_ctu --

   function vec_ctu
     (A : vector_float;
      B : c_int) return vector_unsigned_int
   renames Low_Level_Interface.vec_ctu_vf_cint_r_vui;

   -- vec_dss --

   procedure vec_dss
     (A : c_int)
   renames Low_Level_Interface.vec_dss_cint;

   -- vec_dssall --

   procedure vec_dssall
   renames Low_Level_Interface.vec_dssall;

   -- vec_dst --

   procedure vec_dst
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvucp_cint_cint;

   procedure vec_dst
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvscp_cint_cint;

   procedure vec_dst
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvbcp_cint_cint;

   procedure vec_dst
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvusp_cint_cint;

   procedure vec_dst
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvssp_cint_cint;

   procedure vec_dst
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvbsp_cint_cint;

   procedure vec_dst
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvxp_cint_cint;

   procedure vec_dst
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvuip_cint_cint;

   procedure vec_dst
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvsip_cint_cint;

   procedure vec_dst
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvbip_cint_cint;

   procedure vec_dst
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kvfp_cint_cint;

   procedure vec_dst
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kucp_cint_cint;

   procedure vec_dst
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kscp_cint_cint;

   procedure vec_dst
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kusp_cint_cint;

   procedure vec_dst
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_ksp_cint_cint;

   procedure vec_dst
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kuip_cint_cint;

   procedure vec_dst
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kip_cint_cint;

   procedure vec_dst
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kulongp_cint_cint;

   procedure vec_dst
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_klongp_cint_cint;

   procedure vec_dst
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dst_kfp_cint_cint;

   -- vec_dstst --

   procedure vec_dstst
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvucp_cint_cint;

   procedure vec_dstst
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvscp_cint_cint;

   procedure vec_dstst
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvbcp_cint_cint;

   procedure vec_dstst
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvusp_cint_cint;

   procedure vec_dstst
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvssp_cint_cint;

   procedure vec_dstst
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvbsp_cint_cint;

   procedure vec_dstst
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvxp_cint_cint;

   procedure vec_dstst
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvuip_cint_cint;

   procedure vec_dstst
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvsip_cint_cint;

   procedure vec_dstst
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvbip_cint_cint;

   procedure vec_dstst
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kvfp_cint_cint;

   procedure vec_dstst
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kucp_cint_cint;

   procedure vec_dstst
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kscp_cint_cint;

   procedure vec_dstst
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kusp_cint_cint;

   procedure vec_dstst
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_ksp_cint_cint;

   procedure vec_dstst
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kuip_cint_cint;

   procedure vec_dstst
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kip_cint_cint;

   procedure vec_dstst
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kulongp_cint_cint;

   procedure vec_dstst
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_klongp_cint_cint;

   procedure vec_dstst
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstst_kfp_cint_cint;

   -- vec_dststt --

   procedure vec_dststt
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvucp_cint_cint;

   procedure vec_dststt
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvscp_cint_cint;

   procedure vec_dststt
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvbcp_cint_cint;

   procedure vec_dststt
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvusp_cint_cint;

   procedure vec_dststt
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvssp_cint_cint;

   procedure vec_dststt
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvbsp_cint_cint;

   procedure vec_dststt
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvxp_cint_cint;

   procedure vec_dststt
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvuip_cint_cint;

   procedure vec_dststt
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvsip_cint_cint;

   procedure vec_dststt
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvbip_cint_cint;

   procedure vec_dststt
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kvfp_cint_cint;

   procedure vec_dststt
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kucp_cint_cint;

   procedure vec_dststt
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kscp_cint_cint;

   procedure vec_dststt
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kusp_cint_cint;

   procedure vec_dststt
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_ksp_cint_cint;

   procedure vec_dststt
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kuip_cint_cint;

   procedure vec_dststt
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kip_cint_cint;

   procedure vec_dststt
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kulongp_cint_cint;

   procedure vec_dststt
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_klongp_cint_cint;

   procedure vec_dststt
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dststt_kfp_cint_cint;

   -- vec_dstt --

   procedure vec_dstt
     (A : const_vector_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvucp_cint_cint;

   procedure vec_dstt
     (A : const_vector_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvscp_cint_cint;

   procedure vec_dstt
     (A : const_vector_bool_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvbcp_cint_cint;

   procedure vec_dstt
     (A : const_vector_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvusp_cint_cint;

   procedure vec_dstt
     (A : const_vector_signed_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvssp_cint_cint;

   procedure vec_dstt
     (A : const_vector_bool_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvbsp_cint_cint;

   procedure vec_dstt
     (A : const_vector_pixel_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvxp_cint_cint;

   procedure vec_dstt
     (A : const_vector_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvuip_cint_cint;

   procedure vec_dstt
     (A : const_vector_signed_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvsip_cint_cint;

   procedure vec_dstt
     (A : const_vector_bool_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvbip_cint_cint;

   procedure vec_dstt
     (A : const_vector_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kvfp_cint_cint;

   procedure vec_dstt
     (A : const_unsigned_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kucp_cint_cint;

   procedure vec_dstt
     (A : const_signed_char_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kscp_cint_cint;

   procedure vec_dstt
     (A : const_unsigned_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kusp_cint_cint;

   procedure vec_dstt
     (A : const_short_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_ksp_cint_cint;

   procedure vec_dstt
     (A : const_unsigned_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kuip_cint_cint;

   procedure vec_dstt
     (A : const_int_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kip_cint_cint;

   procedure vec_dstt
     (A : const_unsigned_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kulongp_cint_cint;

   procedure vec_dstt
     (A : const_long_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_klongp_cint_cint;

   procedure vec_dstt
     (A : const_float_ptr;
      B : c_int;
      C : c_int)
   renames Low_Level_Interface.vec_dstt_kfp_cint_cint;

   -- vec_expte --

   function vec_expte
     (A : vector_float) return vector_float;

   -- vec_floor --

   function vec_floor
     (A : vector_float) return vector_float;

   -- vec_ld --

   function vec_ld
     (A : c_long;
      B : const_vector_float_ptr) return vector_float;

   function vec_ld
     (A : c_long;
      B : const_float_ptr) return vector_float;

   function vec_ld
     (A : c_long;
      B : const_vector_bool_int_ptr) return vector_bool_int;

   function vec_ld
     (A : c_long;
      B : const_vector_signed_int_ptr) return vector_signed_int;

   function vec_ld
     (A : c_long;
      B : const_int_ptr) return vector_signed_int;

   function vec_ld
     (A : c_long;
      B : const_long_ptr) return vector_signed_int;

   function vec_ld
     (A : c_long;
      B : const_vector_unsigned_int_ptr) return vector_unsigned_int;

   function vec_ld
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int;

   function vec_ld
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int;

   function vec_ld
     (A : c_long;
      B : const_vector_bool_short_ptr) return vector_bool_short;

   function vec_ld
     (A : c_long;
      B : const_vector_pixel_ptr) return vector_pixel;

   function vec_ld
     (A : c_long;
      B : const_vector_signed_short_ptr) return vector_signed_short;

   function vec_ld
     (A : c_long;
      B : const_short_ptr) return vector_signed_short;

   function vec_ld
     (A : c_long;
      B : const_vector_unsigned_short_ptr) return vector_unsigned_short;

   function vec_ld
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short;

   function vec_ld
     (A : c_long;
      B : const_vector_bool_char_ptr) return vector_bool_char;

   function vec_ld
     (A : c_long;
      B : const_vector_signed_char_ptr) return vector_signed_char;

   function vec_ld
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char;

   function vec_ld
     (A : c_long;
      B : const_vector_unsigned_char_ptr) return vector_unsigned_char;

   function vec_ld
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char;

   -- vec_lde --

   function vec_lde
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char;

   function vec_lde
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char;

   function vec_lde
     (A : c_long;
      B : const_short_ptr) return vector_signed_short;

   function vec_lde
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short;

   function vec_lde
     (A : c_long;
      B : const_float_ptr) return vector_float;

   function vec_lde
     (A : c_long;
      B : const_int_ptr) return vector_signed_int;

   function vec_lde
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int;

   function vec_lde
     (A : c_long;
      B : const_long_ptr) return vector_signed_int;

   function vec_lde
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int;

   -- vec_lvewx --

   function vec_lvewx
     (A : c_long;
      B : float_ptr) return vector_float;

   function vec_lvewx
     (A : c_long;
      B : int_ptr) return vector_signed_int;

   function vec_lvewx
     (A : c_long;
      B : unsigned_int_ptr) return vector_unsigned_int;

   function vec_lvewx
     (A : c_long;
      B : long_ptr) return vector_signed_int;

   function vec_lvewx
     (A : c_long;
      B : unsigned_long_ptr) return vector_unsigned_int;

   -- vec_lvehx --

   function vec_lvehx
     (A : c_long;
      B : short_ptr) return vector_signed_short;

   function vec_lvehx
     (A : c_long;
      B : unsigned_short_ptr) return vector_unsigned_short;

   -- vec_lvebx --

   function vec_lvebx
     (A : c_long;
      B : signed_char_ptr) return vector_signed_char;

   function vec_lvebx
     (A : c_long;
      B : unsigned_char_ptr) return vector_unsigned_char;

   -- vec_ldl --

   function vec_ldl
     (A : c_long;
      B : const_vector_float_ptr) return vector_float;

   function vec_ldl
     (A : c_long;
      B : const_float_ptr) return vector_float;

   function vec_ldl
     (A : c_long;
      B : const_vector_bool_int_ptr) return vector_bool_int;

   function vec_ldl
     (A : c_long;
      B : const_vector_signed_int_ptr) return vector_signed_int;

   function vec_ldl
     (A : c_long;
      B : const_int_ptr) return vector_signed_int;

   function vec_ldl
     (A : c_long;
      B : const_long_ptr) return vector_signed_int;

   function vec_ldl
     (A : c_long;
      B : const_vector_unsigned_int_ptr) return vector_unsigned_int;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int;

   function vec_ldl
     (A : c_long;
      B : const_vector_bool_short_ptr) return vector_bool_short;

   function vec_ldl
     (A : c_long;
      B : const_vector_pixel_ptr) return vector_pixel;

   function vec_ldl
     (A : c_long;
      B : const_vector_signed_short_ptr) return vector_signed_short;

   function vec_ldl
     (A : c_long;
      B : const_short_ptr) return vector_signed_short;

   function vec_ldl
     (A : c_long;
      B : const_vector_unsigned_short_ptr) return vector_unsigned_short;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short;

   function vec_ldl
     (A : c_long;
      B : const_vector_bool_char_ptr) return vector_bool_char;

   function vec_ldl
     (A : c_long;
      B : const_vector_signed_char_ptr) return vector_signed_char;

   function vec_ldl
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char;

   function vec_ldl
     (A : c_long;
      B : const_vector_unsigned_char_ptr) return vector_unsigned_char;

   function vec_ldl
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char;

   -- vec_loge --

   function vec_loge
     (A : vector_float) return vector_float;

   -- vec_lvsl --

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_char_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_signed_char_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_short_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_short_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_int_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_int_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_unsigned_long_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_long_ptr) return vector_unsigned_char;

   function vec_lvsl
     (A : c_long;
      B : constv_float_ptr) return vector_unsigned_char;

   -- vec_lvsr --

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_char_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_signed_char_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_short_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_short_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_int_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_int_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_unsigned_long_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_long_ptr) return vector_unsigned_char;

   function vec_lvsr
     (A : c_long;
      B : constv_float_ptr) return vector_unsigned_char;

   -- vec_madd --

   function vec_madd
     (A : vector_float;
      B : vector_float;
      C : vector_float) return vector_float;

   -- vec_madds --

   function vec_madds
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short;

   -- vec_max --

   function vec_max
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_max
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_max
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_max
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_max
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_max
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_max
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_max
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_max
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_max
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_max
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_max
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_max
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_max
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_max
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_max
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_max
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_max
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_max
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vmaxfp --

   function vec_vmaxfp
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vmaxsw --

   function vec_vmaxsw
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vmaxsw
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_vmaxsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vmaxuw --

   function vec_vmaxuw
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vmaxuw
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_vmaxuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vmaxsh --

   function vec_vmaxsh
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vmaxsh
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_vmaxsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   -- vec_vmaxuh --

   function vec_vmaxuh
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vmaxuh
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_vmaxuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vmaxsb --

   function vec_vmaxsb
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vmaxsb
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_vmaxsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   -- vec_vmaxub --

   function vec_vmaxub
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_vmaxub
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_vmaxub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_mergeh --

   function vec_mergeh
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_mergeh
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_mergeh
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_mergeh
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_mergeh
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel;

   function vec_mergeh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_mergeh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_mergeh
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_mergeh
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_mergeh
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_mergeh
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vmrghw --

   function vec_vmrghw
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_vmrghw
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_vmrghw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vmrghw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vmrghh --

   function vec_vmrghh
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_vmrghh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vmrghh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vmrghh
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel;

   -- vec_vmrghb --

   function vec_vmrghb
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_vmrghb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vmrghb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_mergel --

   function vec_mergel
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_mergel
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_mergel
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_mergel
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_mergel
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel;

   function vec_mergel
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_mergel
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_mergel
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_mergel
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_mergel
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_mergel
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vmrglw --

   function vec_vmrglw
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_vmrglw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vmrglw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vmrglw
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   -- vec_vmrglh --

   function vec_vmrglh
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_vmrglh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vmrglh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vmrglh
     (A : vector_pixel;
      B : vector_pixel) return vector_pixel;

   -- vec_vmrglb --

   function vec_vmrglb
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_vmrglb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vmrglb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_mfvscr --

   function vec_mfvscr return vector_unsigned_short;

   -- vec_min --

   function vec_min
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_min
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_min
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_min
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_min
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_min
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_min
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_min
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_min
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_min
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_min
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_min
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_min
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_min
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_min
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_min
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_min
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_min
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_min
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vminfp --

   function vec_vminfp
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vminsw --

   function vec_vminsw
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vminsw
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_vminsw
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vminuw --

   function vec_vminuw
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vminuw
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_vminuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vminsh --

   function vec_vminsh
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vminsh
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_vminsh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   -- vec_vminuh --

   function vec_vminuh
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vminuh
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_vminuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vminsb --

   function vec_vminsb
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vminsb
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_vminsb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   -- vec_vminub --

   function vec_vminub
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_vminub
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_vminub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_mladd --

   function vec_mladd
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short;

   function vec_mladd
     (A : vector_signed_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_signed_short;

   function vec_mladd
     (A : vector_unsigned_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short;

   function vec_mladd
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_unsigned_short;

   -- vec_mradds --

   function vec_mradds
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short;

   -- vec_msum --

   function vec_msum
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_int) return vector_unsigned_int;

   function vec_msum
     (A : vector_signed_char;
      B : vector_unsigned_char;
      C : vector_signed_int) return vector_signed_int;

   function vec_msum
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int;

   function vec_msum
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int;

   -- vec_vmsumshm --

   function vec_vmsumshm
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int;

   -- vec_vmsumuhm --

   function vec_vmsumuhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vmsummbm --

   function vec_vmsummbm
     (A : vector_signed_char;
      B : vector_unsigned_char;
      C : vector_signed_int) return vector_signed_int;

   -- vec_vmsumubm --

   function vec_vmsumubm
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_int) return vector_unsigned_int;

   -- vec_msums --

   function vec_msums
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int;

   function vec_msums
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int;

   -- vec_vmsumshs --

   function vec_vmsumshs
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_int) return vector_signed_int;

   -- vec_vmsumuhs --

   function vec_vmsumuhs
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_int) return vector_unsigned_int;

   -- vec_mtvscr --

   procedure vec_mtvscr
     (A : vector_signed_int);

   procedure vec_mtvscr
     (A : vector_unsigned_int);

   procedure vec_mtvscr
     (A : vector_bool_int);

   procedure vec_mtvscr
     (A : vector_signed_short);

   procedure vec_mtvscr
     (A : vector_unsigned_short);

   procedure vec_mtvscr
     (A : vector_bool_short);

   procedure vec_mtvscr
     (A : vector_pixel);

   procedure vec_mtvscr
     (A : vector_signed_char);

   procedure vec_mtvscr
     (A : vector_unsigned_char);

   procedure vec_mtvscr
     (A : vector_bool_char);

   -- vec_mule --

   function vec_mule
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short;

   function vec_mule
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short;

   function vec_mule
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int;

   function vec_mule
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int;

   -- vec_vmulesh --

   function vec_vmulesh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int;

   -- vec_vmuleuh --

   function vec_vmuleuh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int;

   -- vec_vmulesb --

   function vec_vmulesb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short;

   -- vec_vmuleub --

   function vec_vmuleub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short;

   -- vec_mulo --

   function vec_mulo
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short;

   function vec_mulo
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short;

   function vec_mulo
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int;

   function vec_mulo
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int;

   -- vec_vmulosh --

   function vec_vmulosh
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_int;

   -- vec_vmulouh --

   function vec_vmulouh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_int;

   -- vec_vmulosb --

   function vec_vmulosb
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_short;

   -- vec_vmuloub --

   function vec_vmuloub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_short;

   -- vec_nmsub --

   function vec_nmsub
     (A : vector_float;
      B : vector_float;
      C : vector_float) return vector_float;

   -- vec_nor --

   function vec_nor
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_nor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_nor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_nor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_nor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_nor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_nor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_nor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_nor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_nor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   -- vec_or --

   function vec_or
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_or
     (A : vector_float;
      B : vector_bool_int) return vector_float;

   function vec_or
     (A : vector_bool_int;
      B : vector_float) return vector_float;

   function vec_or
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_or
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_or
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_or
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_or
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_or
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_or
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_or
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_or
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_or
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_or
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_or
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_or
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_or
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_or
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_or
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_or
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_or
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_or
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_or
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_or
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_pack --

   function vec_pack
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char;

   function vec_pack
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char;

   function vec_pack
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_char;

   function vec_pack
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short;

   function vec_pack
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short;

   function vec_pack
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_short;

   -- vec_vpkuwum --

   function vec_vpkuwum
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_short;

   function vec_vpkuwum
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short;

   function vec_vpkuwum
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short;

   -- vec_vpkuhum --

   function vec_vpkuhum
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_char;

   function vec_vpkuhum
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char;

   function vec_vpkuhum
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char;

   -- vec_packpx --

   function vec_packpx
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_pixel;

   -- vec_packs --

   function vec_packs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char;

   function vec_packs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char;

   function vec_packs
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short;

   function vec_packs
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short;

   -- vec_vpkswss --

   function vec_vpkswss
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_short;

   -- vec_vpkuwus --

   function vec_vpkuwus
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short;

   -- vec_vpkshss --

   function vec_vpkshss
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_char;

   -- vec_vpkuhus --

   function vec_vpkuhus
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char;

   -- vec_packsu --

   function vec_packsu
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_char;

   function vec_packsu
     (A : vector_signed_short;
      B : vector_signed_short) return vector_unsigned_char;

   function vec_packsu
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_short;

   function vec_packsu
     (A : vector_signed_int;
      B : vector_signed_int) return vector_unsigned_short;

   -- vec_vpkswus --

   function vec_vpkswus
     (A : vector_signed_int;
      B : vector_signed_int) return vector_unsigned_short;

   -- vec_vpkshus --

   function vec_vpkshus
     (A : vector_signed_short;
      B : vector_signed_short) return vector_unsigned_char;

   -- vec_perm --

   function vec_perm
     (A : vector_float;
      B : vector_float;
      C : vector_unsigned_char) return vector_float;

   function vec_perm
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_unsigned_char) return vector_signed_int;

   function vec_perm
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_unsigned_char) return vector_unsigned_int;

   function vec_perm
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_unsigned_char) return vector_bool_int;

   function vec_perm
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_unsigned_char) return vector_signed_short;

   function vec_perm
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_char) return vector_unsigned_short;

   function vec_perm
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_unsigned_char) return vector_bool_short;

   function vec_perm
     (A : vector_pixel;
      B : vector_pixel;
      C : vector_unsigned_char) return vector_pixel;

   function vec_perm
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_unsigned_char) return vector_signed_char;

   function vec_perm
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_char) return vector_unsigned_char;

   function vec_perm
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_unsigned_char) return vector_bool_char;

   -- vec_re --

   function vec_re
     (A : vector_float) return vector_float;

   -- vec_rl --

   function vec_rl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_rl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_rl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_rl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_rl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_rl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vrlw --

   function vec_vrlw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_vrlw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vrlh --

   function vec_vrlh
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_vrlh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vrlb --

   function vec_vrlb
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_vrlb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_round --

   function vec_round
     (A : vector_float) return vector_float;

   -- vec_rsqrte --

   function vec_rsqrte
     (A : vector_float) return vector_float;

   -- vec_sel --

   function vec_sel
     (A : vector_float;
      B : vector_float;
      C : vector_bool_int) return vector_float;

   function vec_sel
     (A : vector_float;
      B : vector_float;
      C : vector_unsigned_int) return vector_float;

   function vec_sel
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_bool_int) return vector_signed_int;

   function vec_sel
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_unsigned_int) return vector_signed_int;

   function vec_sel
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_bool_int) return vector_unsigned_int;

   function vec_sel
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_unsigned_int) return vector_unsigned_int;

   function vec_sel
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_bool_int) return vector_bool_int;

   function vec_sel
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_unsigned_int) return vector_bool_int;

   function vec_sel
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_bool_short) return vector_signed_short;

   function vec_sel
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_unsigned_short) return vector_signed_short;

   function vec_sel
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_bool_short) return vector_unsigned_short;

   function vec_sel
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_unsigned_short;

   function vec_sel
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_bool_short) return vector_bool_short;

   function vec_sel
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_unsigned_short) return vector_bool_short;

   function vec_sel
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_bool_char) return vector_signed_char;

   function vec_sel
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_unsigned_char) return vector_signed_char;

   function vec_sel
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_bool_char) return vector_unsigned_char;

   function vec_sel
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_char) return vector_unsigned_char;

   function vec_sel
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_bool_char) return vector_bool_char;

   function vec_sel
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_unsigned_char) return vector_bool_char;

   -- vec_sl --

   function vec_sl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_sl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_sl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_sl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_sl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_sl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vslw --

   function vec_vslw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_vslw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vslh --

   function vec_vslh
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_vslh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vslb --

   function vec_vslb
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_vslb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_sld --

   function vec_sld
     (A : vector_float;
      B : vector_float;
      C : c_int) return vector_float
   renames Low_Level_Interface.vec_sld_vf_vf_cint_r_vf;

   function vec_sld
     (A : vector_signed_int;
      B : vector_signed_int;
      C : c_int) return vector_signed_int
   renames Low_Level_Interface.vec_sld_vsi_vsi_cint_r_vsi;

   function vec_sld
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : c_int) return vector_unsigned_int
   renames Low_Level_Interface.vec_sld_vui_vui_cint_r_vui;

   function vec_sld
     (A : vector_bool_int;
      B : vector_bool_int;
      C : c_int) return vector_bool_int
   renames Low_Level_Interface.vec_sld_vbi_vbi_cint_r_vbi;

   function vec_sld
     (A : vector_signed_short;
      B : vector_signed_short;
      C : c_int) return vector_signed_short
   renames Low_Level_Interface.vec_sld_vss_vss_cint_r_vss;

   function vec_sld
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : c_int) return vector_unsigned_short
   renames Low_Level_Interface.vec_sld_vus_vus_cint_r_vus;

   function vec_sld
     (A : vector_bool_short;
      B : vector_bool_short;
      C : c_int) return vector_bool_short
   renames Low_Level_Interface.vec_sld_vbs_vbs_cint_r_vbs;

   function vec_sld
     (A : vector_pixel;
      B : vector_pixel;
      C : c_int) return vector_pixel
   renames Low_Level_Interface.vec_sld_vx_vx_cint_r_vx;

   function vec_sld
     (A : vector_signed_char;
      B : vector_signed_char;
      C : c_int) return vector_signed_char
   renames Low_Level_Interface.vec_sld_vsc_vsc_cint_r_vsc;

   function vec_sld
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : c_int) return vector_unsigned_char
   renames Low_Level_Interface.vec_sld_vuc_vuc_cint_r_vuc;

   function vec_sld
     (A : vector_bool_char;
      B : vector_bool_char;
      C : c_int) return vector_bool_char
   renames Low_Level_Interface.vec_sld_vbc_vbc_cint_r_vbc;

   -- vec_sll --

   function vec_sll
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_sll
     (A : vector_signed_int;
      B : vector_unsigned_short) return vector_signed_int;

   function vec_sll
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int;

   function vec_sll
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_sll
     (A : vector_unsigned_int;
      B : vector_unsigned_short) return vector_unsigned_int;

   function vec_sll
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int;

   function vec_sll
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_bool_int;

   function vec_sll
     (A : vector_bool_int;
      B : vector_unsigned_short) return vector_bool_int;

   function vec_sll
     (A : vector_bool_int;
      B : vector_unsigned_char) return vector_bool_int;

   function vec_sll
     (A : vector_signed_short;
      B : vector_unsigned_int) return vector_signed_short;

   function vec_sll
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_sll
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short;

   function vec_sll
     (A : vector_unsigned_short;
      B : vector_unsigned_int) return vector_unsigned_short;

   function vec_sll
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_sll
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short;

   function vec_sll
     (A : vector_bool_short;
      B : vector_unsigned_int) return vector_bool_short;

   function vec_sll
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_bool_short;

   function vec_sll
     (A : vector_bool_short;
      B : vector_unsigned_char) return vector_bool_short;

   function vec_sll
     (A : vector_pixel;
      B : vector_unsigned_int) return vector_pixel;

   function vec_sll
     (A : vector_pixel;
      B : vector_unsigned_short) return vector_pixel;

   function vec_sll
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel;

   function vec_sll
     (A : vector_signed_char;
      B : vector_unsigned_int) return vector_signed_char;

   function vec_sll
     (A : vector_signed_char;
      B : vector_unsigned_short) return vector_signed_char;

   function vec_sll
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_sll
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_char;

   function vec_sll
     (A : vector_unsigned_char;
      B : vector_unsigned_short) return vector_unsigned_char;

   function vec_sll
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_sll
     (A : vector_bool_char;
      B : vector_unsigned_int) return vector_bool_char;

   function vec_sll
     (A : vector_bool_char;
      B : vector_unsigned_short) return vector_bool_char;

   function vec_sll
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_bool_char;

   -- vec_slo --

   function vec_slo
     (A : vector_float;
      B : vector_signed_char) return vector_float;

   function vec_slo
     (A : vector_float;
      B : vector_unsigned_char) return vector_float;

   function vec_slo
     (A : vector_signed_int;
      B : vector_signed_char) return vector_signed_int;

   function vec_slo
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int;

   function vec_slo
     (A : vector_unsigned_int;
      B : vector_signed_char) return vector_unsigned_int;

   function vec_slo
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int;

   function vec_slo
     (A : vector_signed_short;
      B : vector_signed_char) return vector_signed_short;

   function vec_slo
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short;

   function vec_slo
     (A : vector_unsigned_short;
      B : vector_signed_char) return vector_unsigned_short;

   function vec_slo
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short;

   function vec_slo
     (A : vector_pixel;
      B : vector_signed_char) return vector_pixel;

   function vec_slo
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel;

   function vec_slo
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_slo
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_slo
     (A : vector_unsigned_char;
      B : vector_signed_char) return vector_unsigned_char;

   function vec_slo
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_splat --

   function vec_splat
     (A : vector_signed_char;
      B : c_int) return vector_signed_char
   renames Low_Level_Interface.vec_splat_vsc_cint_r_vsc;

   function vec_splat
     (A : vector_unsigned_char;
      B : c_int) return vector_unsigned_char
   renames Low_Level_Interface.vec_splat_vuc_cint_r_vuc;

   function vec_splat
     (A : vector_bool_char;
      B : c_int) return vector_bool_char
   renames Low_Level_Interface.vec_splat_vbc_cint_r_vbc;

   function vec_splat
     (A : vector_signed_short;
      B : c_int) return vector_signed_short
   renames Low_Level_Interface.vec_splat_vss_cint_r_vss;

   function vec_splat
     (A : vector_unsigned_short;
      B : c_int) return vector_unsigned_short
   renames Low_Level_Interface.vec_splat_vus_cint_r_vus;

   function vec_splat
     (A : vector_bool_short;
      B : c_int) return vector_bool_short
   renames Low_Level_Interface.vec_splat_vbs_cint_r_vbs;

   function vec_splat
     (A : vector_pixel;
      B : c_int) return vector_pixel
   renames Low_Level_Interface.vec_splat_vx_cint_r_vx;

   function vec_splat
     (A : vector_float;
      B : c_int) return vector_float
   renames Low_Level_Interface.vec_splat_vf_cint_r_vf;

   function vec_splat
     (A : vector_signed_int;
      B : c_int) return vector_signed_int
   renames Low_Level_Interface.vec_splat_vsi_cint_r_vsi;

   function vec_splat
     (A : vector_unsigned_int;
      B : c_int) return vector_unsigned_int
   renames Low_Level_Interface.vec_splat_vui_cint_r_vui;

   function vec_splat
     (A : vector_bool_int;
      B : c_int) return vector_bool_int
   renames Low_Level_Interface.vec_splat_vbi_cint_r_vbi;

   -- vec_vspltw --

   function vec_vspltw
     (A : vector_float;
      B : c_int) return vector_float
   renames Low_Level_Interface.vec_vspltw_vf_cint_r_vf;

   function vec_vspltw
     (A : vector_signed_int;
      B : c_int) return vector_signed_int
   renames Low_Level_Interface.vec_vspltw_vsi_cint_r_vsi;

   function vec_vspltw
     (A : vector_unsigned_int;
      B : c_int) return vector_unsigned_int
   renames Low_Level_Interface.vec_vspltw_vui_cint_r_vui;

   function vec_vspltw
     (A : vector_bool_int;
      B : c_int) return vector_bool_int
   renames Low_Level_Interface.vec_vspltw_vbi_cint_r_vbi;

   -- vec_vsplth --

   function vec_vsplth
     (A : vector_bool_short;
      B : c_int) return vector_bool_short
   renames Low_Level_Interface.vec_vsplth_vbs_cint_r_vbs;

   function vec_vsplth
     (A : vector_signed_short;
      B : c_int) return vector_signed_short
   renames Low_Level_Interface.vec_vsplth_vss_cint_r_vss;

   function vec_vsplth
     (A : vector_unsigned_short;
      B : c_int) return vector_unsigned_short
   renames Low_Level_Interface.vec_vsplth_vus_cint_r_vus;

   function vec_vsplth
     (A : vector_pixel;
      B : c_int) return vector_pixel
   renames Low_Level_Interface.vec_vsplth_vx_cint_r_vx;

   -- vec_vspltb --

   function vec_vspltb
     (A : vector_signed_char;
      B : c_int) return vector_signed_char
   renames Low_Level_Interface.vec_vspltb_vsc_cint_r_vsc;

   function vec_vspltb
     (A : vector_unsigned_char;
      B : c_int) return vector_unsigned_char
   renames Low_Level_Interface.vec_vspltb_vuc_cint_r_vuc;

   function vec_vspltb
     (A : vector_bool_char;
      B : c_int) return vector_bool_char
   renames Low_Level_Interface.vec_vspltb_vbc_cint_r_vbc;

   -- vec_splat_s8 --

   function vec_splat_s8
     (A : c_int) return vector_signed_char
   renames Low_Level_Interface.vec_splat_s8_cint_r_vsc;

   -- vec_splat_s16 --

   function vec_splat_s16
     (A : c_int) return vector_signed_short
   renames Low_Level_Interface.vec_splat_s16_cint_r_vss;

   -- vec_splat_s32 --

   function vec_splat_s32
     (A : c_int) return vector_signed_int
   renames Low_Level_Interface.vec_splat_s32_cint_r_vsi;

   -- vec_splat_u8 --

   function vec_splat_u8
     (A : c_int) return vector_unsigned_char
   renames Low_Level_Interface.vec_splat_u8_cint_r_vuc;

   -- vec_splat_u16 --

   function vec_splat_u16
     (A : c_int) return vector_unsigned_short
   renames Low_Level_Interface.vec_splat_u16_cint_r_vus;

   -- vec_splat_u32 --

   function vec_splat_u32
     (A : c_int) return vector_unsigned_int
   renames Low_Level_Interface.vec_splat_u32_cint_r_vui;

   -- vec_sr --

   function vec_sr
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_sr
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_sr
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_sr
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_sr
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_sr
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vsrw --

   function vec_vsrw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_vsrw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vsrh --

   function vec_vsrh
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_vsrh
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vsrb --

   function vec_vsrb
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_vsrb
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_sra --

   function vec_sra
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_sra
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_sra
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_sra
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_sra
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_sra
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vsraw --

   function vec_vsraw
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_vsraw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vsrah --

   function vec_vsrah
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_vsrah
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vsrab --

   function vec_vsrab
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_vsrab
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_srl --

   function vec_srl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int;

   function vec_srl
     (A : vector_signed_int;
      B : vector_unsigned_short) return vector_signed_int;

   function vec_srl
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int;

   function vec_srl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_srl
     (A : vector_unsigned_int;
      B : vector_unsigned_short) return vector_unsigned_int;

   function vec_srl
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int;

   function vec_srl
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_bool_int;

   function vec_srl
     (A : vector_bool_int;
      B : vector_unsigned_short) return vector_bool_int;

   function vec_srl
     (A : vector_bool_int;
      B : vector_unsigned_char) return vector_bool_int;

   function vec_srl
     (A : vector_signed_short;
      B : vector_unsigned_int) return vector_signed_short;

   function vec_srl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short;

   function vec_srl
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short;

   function vec_srl
     (A : vector_unsigned_short;
      B : vector_unsigned_int) return vector_unsigned_short;

   function vec_srl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_srl
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short;

   function vec_srl
     (A : vector_bool_short;
      B : vector_unsigned_int) return vector_bool_short;

   function vec_srl
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_bool_short;

   function vec_srl
     (A : vector_bool_short;
      B : vector_unsigned_char) return vector_bool_short;

   function vec_srl
     (A : vector_pixel;
      B : vector_unsigned_int) return vector_pixel;

   function vec_srl
     (A : vector_pixel;
      B : vector_unsigned_short) return vector_pixel;

   function vec_srl
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel;

   function vec_srl
     (A : vector_signed_char;
      B : vector_unsigned_int) return vector_signed_char;

   function vec_srl
     (A : vector_signed_char;
      B : vector_unsigned_short) return vector_signed_char;

   function vec_srl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_srl
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_char;

   function vec_srl
     (A : vector_unsigned_char;
      B : vector_unsigned_short) return vector_unsigned_char;

   function vec_srl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_srl
     (A : vector_bool_char;
      B : vector_unsigned_int) return vector_bool_char;

   function vec_srl
     (A : vector_bool_char;
      B : vector_unsigned_short) return vector_bool_char;

   function vec_srl
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_bool_char;

   -- vec_sro --

   function vec_sro
     (A : vector_float;
      B : vector_signed_char) return vector_float;

   function vec_sro
     (A : vector_float;
      B : vector_unsigned_char) return vector_float;

   function vec_sro
     (A : vector_signed_int;
      B : vector_signed_char) return vector_signed_int;

   function vec_sro
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int;

   function vec_sro
     (A : vector_unsigned_int;
      B : vector_signed_char) return vector_unsigned_int;

   function vec_sro
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int;

   function vec_sro
     (A : vector_signed_short;
      B : vector_signed_char) return vector_signed_short;

   function vec_sro
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short;

   function vec_sro
     (A : vector_unsigned_short;
      B : vector_signed_char) return vector_unsigned_short;

   function vec_sro
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short;

   function vec_sro
     (A : vector_pixel;
      B : vector_signed_char) return vector_pixel;

   function vec_sro
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel;

   function vec_sro
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_sro
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char;

   function vec_sro
     (A : vector_unsigned_char;
      B : vector_signed_char) return vector_unsigned_char;

   function vec_sro
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_st --

   procedure vec_st
     (A : vector_float;
      B : c_int;
      C : vector_float_ptr);

   procedure vec_st
     (A : vector_float;
      B : c_int;
      C : float_ptr);

   procedure vec_st
     (A : vector_signed_int;
      B : c_int;
      C : vector_signed_int_ptr);

   procedure vec_st
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr);

   procedure vec_st
     (A : vector_unsigned_int;
      B : c_int;
      C : vector_unsigned_int_ptr);

   procedure vec_st
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr);

   procedure vec_st
     (A : vector_bool_int;
      B : c_int;
      C : vector_bool_int_ptr);

   procedure vec_st
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr);

   procedure vec_st
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr);

   procedure vec_st
     (A : vector_signed_short;
      B : c_int;
      C : vector_signed_short_ptr);

   procedure vec_st
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr);

   procedure vec_st
     (A : vector_unsigned_short;
      B : c_int;
      C : vector_unsigned_short_ptr);

   procedure vec_st
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_st
     (A : vector_bool_short;
      B : c_int;
      C : vector_bool_short_ptr);

   procedure vec_st
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_st
     (A : vector_pixel;
      B : c_int;
      C : vector_pixel_ptr);

   procedure vec_st
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_st
     (A : vector_pixel;
      B : c_int;
      C : short_ptr);

   procedure vec_st
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr);

   procedure vec_st
     (A : vector_signed_char;
      B : c_int;
      C : vector_signed_char_ptr);

   procedure vec_st
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr);

   procedure vec_st
     (A : vector_unsigned_char;
      B : c_int;
      C : vector_unsigned_char_ptr);

   procedure vec_st
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_st
     (A : vector_bool_char;
      B : c_int;
      C : vector_bool_char_ptr);

   procedure vec_st
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_st
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr);

   -- vec_ste --

   procedure vec_ste
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr);

   procedure vec_ste
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_ste
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr);

   procedure vec_ste
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_ste
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr);

   procedure vec_ste
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_ste
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr);

   procedure vec_ste
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_ste
     (A : vector_pixel;
      B : c_int;
      C : short_ptr);

   procedure vec_ste
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_ste
     (A : vector_float;
      B : c_int;
      C : float_ptr);

   procedure vec_ste
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr);

   procedure vec_ste
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr);

   procedure vec_ste
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr);

   procedure vec_ste
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr);

   -- vec_stvewx --

   procedure vec_stvewx
     (A : vector_float;
      B : c_int;
      C : float_ptr);

   procedure vec_stvewx
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr);

   procedure vec_stvewx
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr);

   procedure vec_stvewx
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr);

   procedure vec_stvewx
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr);

   -- vec_stvehx --

   procedure vec_stvehx
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr);

   procedure vec_stvehx
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_stvehx
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr);

   procedure vec_stvehx
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_stvehx
     (A : vector_pixel;
      B : c_int;
      C : short_ptr);

   procedure vec_stvehx
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr);

   -- vec_stvebx --

   procedure vec_stvebx
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr);

   procedure vec_stvebx
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_stvebx
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr);

   procedure vec_stvebx
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr);

   -- vec_stl --

   procedure vec_stl
     (A : vector_float;
      B : c_int;
      C : vector_float_ptr);

   procedure vec_stl
     (A : vector_float;
      B : c_int;
      C : float_ptr);

   procedure vec_stl
     (A : vector_signed_int;
      B : c_int;
      C : vector_signed_int_ptr);

   procedure vec_stl
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr);

   procedure vec_stl
     (A : vector_unsigned_int;
      B : c_int;
      C : vector_unsigned_int_ptr);

   procedure vec_stl
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr);

   procedure vec_stl
     (A : vector_bool_int;
      B : c_int;
      C : vector_bool_int_ptr);

   procedure vec_stl
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr);

   procedure vec_stl
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr);

   procedure vec_stl
     (A : vector_signed_short;
      B : c_int;
      C : vector_signed_short_ptr);

   procedure vec_stl
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr);

   procedure vec_stl
     (A : vector_unsigned_short;
      B : c_int;
      C : vector_unsigned_short_ptr);

   procedure vec_stl
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_stl
     (A : vector_bool_short;
      B : c_int;
      C : vector_bool_short_ptr);

   procedure vec_stl
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_stl
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr);

   procedure vec_stl
     (A : vector_pixel;
      B : c_int;
      C : vector_pixel_ptr);

   procedure vec_stl
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr);

   procedure vec_stl
     (A : vector_pixel;
      B : c_int;
      C : short_ptr);

   procedure vec_stl
     (A : vector_signed_char;
      B : c_int;
      C : vector_signed_char_ptr);

   procedure vec_stl
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr);

   procedure vec_stl
     (A : vector_unsigned_char;
      B : c_int;
      C : vector_unsigned_char_ptr);

   procedure vec_stl
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_stl
     (A : vector_bool_char;
      B : c_int;
      C : vector_bool_char_ptr);

   procedure vec_stl
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr);

   procedure vec_stl
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr);

   -- vec_sub --

   function vec_sub
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_sub
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_sub
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_sub
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_sub
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_sub
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_sub
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_sub
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_sub
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_sub
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_sub
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_sub
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_sub
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_sub
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_sub
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_sub
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_sub
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_sub
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_sub
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vsubfp --

   function vec_vsubfp
     (A : vector_float;
      B : vector_float) return vector_float;

   -- vec_vsubuwm --

   function vec_vsubuwm
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vsubuwm
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_vsubuwm
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vsubuwm
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vsubuwm
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_vsubuwm
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vsubuhm --

   function vec_vsubuhm
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vsubuhm
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_vsubuhm
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vsubuhm
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vsubuhm
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_vsubuhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vsububm --

   function vec_vsububm
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vsububm
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_vsububm
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vsububm
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_vsububm
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_vsububm
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_subc --

   function vec_subc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_subs --

   function vec_subs
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_subs
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_subs
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_subs
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_subs
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_subs
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_subs
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_subs
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_subs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_subs
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_subs
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_subs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_subs
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_subs
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_subs
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_subs
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_subs
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_subs
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vsubsws --

   function vec_vsubsws
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_vsubsws
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_vsubsws
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vsubuws --

   function vec_vsubuws
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_vsubuws
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_vsubuws
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_vsubshs --

   function vec_vsubshs
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_vsubshs
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_vsubshs
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   -- vec_vsubuhs --

   function vec_vsubuhs
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_vsubuhs
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_vsubuhs
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   -- vec_vsubsbs --

   function vec_vsubsbs
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_vsubsbs
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_vsubsbs
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   -- vec_vsububs --

   function vec_vsububs
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_vsububs
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_vsububs
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   -- vec_sum4s --

   function vec_sum4s
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_sum4s
     (A : vector_signed_char;
      B : vector_signed_int) return vector_signed_int;

   function vec_sum4s
     (A : vector_signed_short;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vsum4shs --

   function vec_vsum4shs
     (A : vector_signed_short;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vsum4sbs --

   function vec_vsum4sbs
     (A : vector_signed_char;
      B : vector_signed_int) return vector_signed_int;

   -- vec_vsum4ubs --

   function vec_vsum4ubs
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_int;

   -- vec_sum2s --

   function vec_sum2s
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_sums --

   function vec_sums
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   -- vec_trunc --

   function vec_trunc
     (A : vector_float) return vector_float;

   -- vec_unpackh --

   function vec_unpackh
     (A : vector_signed_char) return vector_signed_short;

   function vec_unpackh
     (A : vector_bool_char) return vector_bool_short;

   function vec_unpackh
     (A : vector_signed_short) return vector_signed_int;

   function vec_unpackh
     (A : vector_bool_short) return vector_bool_int;

   function vec_unpackh
     (A : vector_pixel) return vector_unsigned_int;

   -- vec_vupkhsh --

   function vec_vupkhsh
     (A : vector_bool_short) return vector_bool_int;

   function vec_vupkhsh
     (A : vector_signed_short) return vector_signed_int;

   -- vec_vupkhpx --

   function vec_vupkhpx
     (A : vector_pixel) return vector_unsigned_int;

   -- vec_vupkhsb --

   function vec_vupkhsb
     (A : vector_bool_char) return vector_bool_short;

   function vec_vupkhsb
     (A : vector_signed_char) return vector_signed_short;

   -- vec_unpackl --

   function vec_unpackl
     (A : vector_signed_char) return vector_signed_short;

   function vec_unpackl
     (A : vector_bool_char) return vector_bool_short;

   function vec_unpackl
     (A : vector_pixel) return vector_unsigned_int;

   function vec_unpackl
     (A : vector_signed_short) return vector_signed_int;

   function vec_unpackl
     (A : vector_bool_short) return vector_bool_int;

   -- vec_vupklpx --

   function vec_vupklpx
     (A : vector_pixel) return vector_unsigned_int;

   -- vec_upklsh --

   function vec_vupklsh
     (A : vector_bool_short) return vector_bool_int;

   function vec_vupklsh
     (A : vector_signed_short) return vector_signed_int;

   -- vec_vupklsb --

   function vec_vupklsb
     (A : vector_bool_char) return vector_bool_short;

   function vec_vupklsb
     (A : vector_signed_char) return vector_signed_short;

   -- vec_xor --

   function vec_xor
     (A : vector_float;
      B : vector_float) return vector_float;

   function vec_xor
     (A : vector_float;
      B : vector_bool_int) return vector_float;

   function vec_xor
     (A : vector_bool_int;
      B : vector_float) return vector_float;

   function vec_xor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int;

   function vec_xor
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_xor
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int;

   function vec_xor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int;

   function vec_xor
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_xor
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int;

   function vec_xor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int;

   function vec_xor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short;

   function vec_xor
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_xor
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short;

   function vec_xor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short;

   function vec_xor
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_xor
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short;

   function vec_xor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short;

   function vec_xor
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_xor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char;

   function vec_xor
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char;

   function vec_xor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char;

   function vec_xor
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   function vec_xor
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char;

   function vec_xor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char;

   ----------------------------------
   -- [PIM-4.5 AltiVec predicates] --
   ----------------------------------

   -- vec_all_eq --

   function vec_all_eq
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_all_eq
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_all_eq
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_all_eq
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_eq
     (A : vector_bool_char;
      B : vector_bool_char) return c_int;

   function vec_all_eq
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_eq
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_all_eq
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_all_eq
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_all_eq
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_all_eq
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_eq
     (A : vector_bool_short;
      B : vector_bool_short) return c_int;

   function vec_all_eq
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_eq
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_all_eq
     (A : vector_pixel;
      B : vector_pixel) return c_int;

   function vec_all_eq
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_all_eq
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_all_eq
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_all_eq
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_eq
     (A : vector_bool_int;
      B : vector_bool_int) return c_int;

   function vec_all_eq
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_eq
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_all_eq
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_ge --

   function vec_all_ge
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_ge
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_all_ge
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_ge
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_all_ge
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_all_ge
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_all_ge
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_ge
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_all_ge
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_ge
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_all_ge
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_all_ge
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_all_ge
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_ge
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_all_ge
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_ge
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_all_ge
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_all_ge
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_all_ge
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_gt --

   function vec_all_gt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_gt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_all_gt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_gt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_all_gt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_all_gt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_all_gt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_gt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_all_gt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_gt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_all_gt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_all_gt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_all_gt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_gt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_all_gt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_gt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_all_gt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_all_gt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_all_gt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_in --

   function vec_all_in
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_le --

   function vec_all_le
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_le
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_all_le
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_le
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_all_le
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_all_le
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_all_le
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_le
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_all_le
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_le
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_all_le
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_all_le
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_all_le
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_le
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_all_le
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_le
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_all_le
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_all_le
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_all_le
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_lt --

   function vec_all_lt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_lt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_all_lt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_lt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_all_lt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_all_lt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_all_lt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_lt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_all_lt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_lt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_all_lt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_all_lt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_all_lt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_lt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_all_lt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_lt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_all_lt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_all_lt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_all_lt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_nan --

   function vec_all_nan
     (A : vector_float) return c_int;

   -- vec_all_ne --

   function vec_all_ne
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_all_ne
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_all_ne
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_all_ne
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_ne
     (A : vector_bool_char;
      B : vector_bool_char) return c_int;

   function vec_all_ne
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_all_ne
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_all_ne
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_all_ne
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_all_ne
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_all_ne
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_ne
     (A : vector_bool_short;
      B : vector_bool_short) return c_int;

   function vec_all_ne
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_all_ne
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_all_ne
     (A : vector_pixel;
      B : vector_pixel) return c_int;

   function vec_all_ne
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_all_ne
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_all_ne
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_all_ne
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_ne
     (A : vector_bool_int;
      B : vector_bool_int) return c_int;

   function vec_all_ne
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_all_ne
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_all_ne
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_nge --

   function vec_all_nge
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_ngt --

   function vec_all_ngt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_nle --

   function vec_all_nle
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_nlt --

   function vec_all_nlt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_all_numeric --

   function vec_all_numeric
     (A : vector_float) return c_int;

   -- vec_any_eq --

   function vec_any_eq
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_any_eq
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_any_eq
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_any_eq
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_eq
     (A : vector_bool_char;
      B : vector_bool_char) return c_int;

   function vec_any_eq
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_eq
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_any_eq
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_any_eq
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_any_eq
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_any_eq
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_eq
     (A : vector_bool_short;
      B : vector_bool_short) return c_int;

   function vec_any_eq
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_eq
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_any_eq
     (A : vector_pixel;
      B : vector_pixel) return c_int;

   function vec_any_eq
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_any_eq
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_any_eq
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_any_eq
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_eq
     (A : vector_bool_int;
      B : vector_bool_int) return c_int;

   function vec_any_eq
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_eq
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_any_eq
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_ge --

   function vec_any_ge
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_any_ge
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_any_ge
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_ge
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_any_ge
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_ge
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_any_ge
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_any_ge
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_ge
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_any_ge
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_any_ge
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_ge
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_any_ge
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_any_ge
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_any_ge
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_ge
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_any_ge
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_ge
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_any_ge
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_gt --

   function vec_any_gt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_gt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_any_gt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_gt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_any_gt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_any_gt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_any_gt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_gt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_any_gt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_gt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_any_gt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_any_gt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_any_gt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_gt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_any_gt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_gt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_any_gt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_any_gt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_any_gt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_le --

   function vec_any_le
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_le
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_any_le
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_le
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_any_le
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_any_le
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_any_le
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_le
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_any_le
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_le
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_any_le
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_any_le
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_any_le
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_le
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_any_le
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_le
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_any_le
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_any_le
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_any_le
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_lt --

   function vec_any_lt
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_lt
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_any_lt
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_lt
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_any_lt
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_any_lt
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_any_lt
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_lt
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_any_lt
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_lt
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_any_lt
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_any_lt
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_any_lt
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_lt
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_any_lt
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_lt
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_any_lt
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_any_lt
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_any_lt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_nan --

   function vec_any_nan
     (A : vector_float) return c_int;

   -- vec_any_ne --

   function vec_any_ne
     (A : vector_signed_char;
      B : vector_bool_char) return c_int;

   function vec_any_ne
     (A : vector_signed_char;
      B : vector_signed_char) return c_int;

   function vec_any_ne
     (A : vector_unsigned_char;
      B : vector_bool_char) return c_int;

   function vec_any_ne
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_ne
     (A : vector_bool_char;
      B : vector_bool_char) return c_int;

   function vec_any_ne
     (A : vector_bool_char;
      B : vector_unsigned_char) return c_int;

   function vec_any_ne
     (A : vector_bool_char;
      B : vector_signed_char) return c_int;

   function vec_any_ne
     (A : vector_signed_short;
      B : vector_bool_short) return c_int;

   function vec_any_ne
     (A : vector_signed_short;
      B : vector_signed_short) return c_int;

   function vec_any_ne
     (A : vector_unsigned_short;
      B : vector_bool_short) return c_int;

   function vec_any_ne
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_ne
     (A : vector_bool_short;
      B : vector_bool_short) return c_int;

   function vec_any_ne
     (A : vector_bool_short;
      B : vector_unsigned_short) return c_int;

   function vec_any_ne
     (A : vector_bool_short;
      B : vector_signed_short) return c_int;

   function vec_any_ne
     (A : vector_pixel;
      B : vector_pixel) return c_int;

   function vec_any_ne
     (A : vector_signed_int;
      B : vector_bool_int) return c_int;

   function vec_any_ne
     (A : vector_signed_int;
      B : vector_signed_int) return c_int;

   function vec_any_ne
     (A : vector_unsigned_int;
      B : vector_bool_int) return c_int;

   function vec_any_ne
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_ne
     (A : vector_bool_int;
      B : vector_bool_int) return c_int;

   function vec_any_ne
     (A : vector_bool_int;
      B : vector_unsigned_int) return c_int;

   function vec_any_ne
     (A : vector_bool_int;
      B : vector_signed_int) return c_int;

   function vec_any_ne
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_nge --

   function vec_any_nge
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_ngt --

   function vec_any_ngt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_nle --

   function vec_any_nle
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_nlt --

   function vec_any_nlt
     (A : vector_float;
      B : vector_float) return c_int;

   -- vec_any_numeric --

   function vec_any_numeric
     (A : vector_float) return c_int;

   -- vec_any_out --

   function vec_any_out
     (A : vector_float;
      B : vector_float) return c_int;

   -------------------------------------------
   -- Straight overloads of routines aboves --
   -------------------------------------------

   -- vec_vaddcuw --

   function vec_vaddcuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_addc;

   -- vec_vand --

   function vec_vand
     (A : vector_float;
      B : vector_float) return vector_float
   renames vec_and;

   function vec_vand
     (A : vector_float;
      B : vector_bool_int) return vector_float
   renames vec_and;

   function vec_vand
     (A : vector_bool_int;
      B : vector_float) return vector_float
   renames vec_and;

   function vec_vand
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   renames vec_and;

   function vec_vand
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_and;

   function vec_vand
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   renames vec_and;

   function vec_vand
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_and;

   function vec_vand
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_and;

   function vec_vand
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   renames vec_and;

   function vec_vand
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_and;

   function vec_vand
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   renames vec_and;

   function vec_vand
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_and;

   function vec_vand
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   renames vec_and;

   function vec_vand
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_and;

   function vec_vand
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_and;

   function vec_vand
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   renames vec_and;

   function vec_vand
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_and;

   function vec_vand
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_and;

   function vec_vand
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   renames vec_and;

   function vec_vand
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   renames vec_and;

   function vec_vand
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_and;

   function vec_vand
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_and;

   function vec_vand
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   renames vec_and;

   function vec_vand
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_and;

   -- vec_vandc --

   function vec_vandc
     (A : vector_float;
      B : vector_float) return vector_float
   renames vec_andc;

   function vec_vandc
     (A : vector_float;
      B : vector_bool_int) return vector_float
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_int;
      B : vector_float) return vector_float
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_andc;

   function vec_vandc
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   renames vec_andc;

   function vec_vandc
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_andc;

   function vec_vandc
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   renames vec_andc;

   function vec_vandc
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_andc;

   function vec_vandc
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   renames vec_andc;

   function vec_vandc
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_andc;

   function vec_vandc
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   renames vec_andc;

   function vec_vandc
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   renames vec_andc;

   function vec_vandc
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   renames vec_andc;

   function vec_vandc
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_andc;

   function vec_vandc
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_andc;

   function vec_vandc
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   renames vec_andc;

   function vec_vandc
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_andc;

   -- vec_vrfip --

   function vec_vrfip
     (A : vector_float) return vector_float
   renames vec_ceil;

   -- vec_vcmpbfp --

   function vec_vcmpbfp
     (A : vector_float;
      B : vector_float) return vector_signed_int
   renames vec_cmpb;

   -- vec_vcmpgefp --

   function vec_vcmpgefp
     (A : vector_float;
      B : vector_float) return vector_bool_int
   renames vec_cmpge;

   -- vec_vctsxs --

   function vec_vctsxs
     (A : vector_float;
      B : c_int) return vector_signed_int
   renames vec_cts;

   -- vec_vctuxs --

   function vec_vctuxs
     (A : vector_float;
      B : c_int) return vector_unsigned_int
   renames vec_ctu;

   -- vec_vexptefp --

   function vec_vexptefp
     (A : vector_float) return vector_float
   renames vec_expte;

   -- vec_vrfim --

   function vec_vrfim
     (A : vector_float) return vector_float
   renames vec_floor;

   -- vec_lvx --

   function vec_lvx
     (A : c_long;
      B : const_vector_float_ptr) return vector_float
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_float_ptr) return vector_float
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_bool_int_ptr) return vector_bool_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_signed_int_ptr) return vector_signed_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_int_ptr) return vector_signed_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_long_ptr) return vector_signed_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_unsigned_int_ptr) return vector_unsigned_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_bool_short_ptr) return vector_bool_short
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_pixel_ptr) return vector_pixel
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_signed_short_ptr) return vector_signed_short
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_short_ptr) return vector_signed_short
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_unsigned_short_ptr) return vector_unsigned_short
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_bool_char_ptr) return vector_bool_char
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_signed_char_ptr) return vector_signed_char
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_vector_unsigned_char_ptr) return vector_unsigned_char
   renames vec_ld;

   function vec_lvx
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char
   renames vec_ld;

   -- vec_lvxl --

   function vec_lvxl
     (A : c_long;
      B : const_vector_float_ptr) return vector_float
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_float_ptr) return vector_float
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_bool_int_ptr) return vector_bool_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_signed_int_ptr) return vector_signed_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_int_ptr) return vector_signed_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_long_ptr) return vector_signed_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_unsigned_int_ptr) return vector_unsigned_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_unsigned_int_ptr) return vector_unsigned_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_unsigned_long_ptr) return vector_unsigned_int
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_bool_short_ptr) return vector_bool_short
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_pixel_ptr) return vector_pixel
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_signed_short_ptr) return vector_signed_short
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_short_ptr) return vector_signed_short
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_unsigned_short_ptr) return vector_unsigned_short
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_unsigned_short_ptr) return vector_unsigned_short
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_bool_char_ptr) return vector_bool_char
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_signed_char_ptr) return vector_signed_char
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_signed_char_ptr) return vector_signed_char
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_vector_unsigned_char_ptr) return vector_unsigned_char
   renames vec_ldl;

   function vec_lvxl
     (A : c_long;
      B : const_unsigned_char_ptr) return vector_unsigned_char
   renames vec_ldl;

   -- vec_vlogefp --

   function vec_vlogefp
     (A : vector_float) return vector_float
   renames vec_loge;

   -- vec_vmaddfp --

   function vec_vmaddfp
     (A : vector_float;
      B : vector_float;
      C : vector_float) return vector_float
   renames vec_madd;

   -- vec_vmhaddshs --

   function vec_vmhaddshs
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   renames vec_madds;

   -- vec_vmladduhm --

   function vec_vmladduhm
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   renames vec_mladd;

   function vec_vmladduhm
     (A : vector_signed_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_signed_short
   renames vec_mladd;

   function vec_vmladduhm
     (A : vector_unsigned_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   renames vec_mladd;

   function vec_vmladduhm
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_unsigned_short
   renames vec_mladd;

   -- vec_vmhraddshs --

   function vec_vmhraddshs
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_signed_short) return vector_signed_short
   renames vec_mradds;

   -- vec_vnmsubfp --

   function vec_vnmsubfp
     (A : vector_float;
      B : vector_float;
      C : vector_float) return vector_float
   renames vec_nmsub;

   -- vec_vnor --

   function vec_vnor
     (A : vector_float;
      B : vector_float) return vector_float
   renames vec_nor;

   function vec_vnor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_nor;

   function vec_vnor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_nor;

   function vec_vnor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   renames vec_nor;

   function vec_vnor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_nor;

   function vec_vnor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_nor;

   function vec_vnor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   renames vec_nor;

   function vec_vnor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_nor;

   function vec_vnor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_nor;

   function vec_vnor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   renames vec_nor;

   -- vec_vor --

   function vec_vor
     (A : vector_float;
      B : vector_float) return vector_float
   renames vec_or;

   function vec_vor
     (A : vector_float;
      B : vector_bool_int) return vector_float
   renames vec_or;

   function vec_vor
     (A : vector_bool_int;
      B : vector_float) return vector_float
   renames vec_or;

   function vec_vor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   renames vec_or;

   function vec_vor
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_or;

   function vec_vor
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   renames vec_or;

   function vec_vor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_or;

   function vec_vor
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_or;

   function vec_vor
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   renames vec_or;

   function vec_vor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_or;

   function vec_vor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   renames vec_or;

   function vec_vor
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_or;

   function vec_vor
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   renames vec_or;

   function vec_vor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_or;

   function vec_vor
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_or;

   function vec_vor
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   renames vec_or;

   function vec_vor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_or;

   function vec_vor
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_or;

   function vec_vor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   renames vec_or;

   function vec_vor
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   renames vec_or;

   function vec_vor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_or;

   function vec_vor
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_or;

   function vec_vor
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   renames vec_or;

   function vec_vor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_or;

   -- vec_vpkpx --

   function vec_vpkpx
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_pixel
   renames vec_packpx;

   -- vec_vperm --

   function vec_vperm
     (A : vector_float;
      B : vector_float;
      C : vector_unsigned_char) return vector_float
   renames vec_perm;

   function vec_vperm
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_unsigned_char) return vector_signed_int
   renames vec_perm;

   function vec_vperm
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_unsigned_char) return vector_unsigned_int
   renames vec_perm;

   function vec_vperm
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_unsigned_char) return vector_bool_int
   renames vec_perm;

   function vec_vperm
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_unsigned_char) return vector_signed_short
   renames vec_perm;

   function vec_vperm
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_char) return vector_unsigned_short
   renames vec_perm;

   function vec_vperm
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_unsigned_char) return vector_bool_short
   renames vec_perm;

   function vec_vperm
     (A : vector_pixel;
      B : vector_pixel;
      C : vector_unsigned_char) return vector_pixel
   renames vec_perm;

   function vec_vperm
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_unsigned_char) return vector_signed_char
   renames vec_perm;

   function vec_vperm
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_char) return vector_unsigned_char
   renames vec_perm;

   function vec_vperm
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_unsigned_char) return vector_bool_char
   renames vec_perm;

   -- vec_vrefp --

   function vec_vrefp
     (A : vector_float) return vector_float
   renames vec_re;

   -- vec_vrfin --

   function vec_vrfin
     (A : vector_float) return vector_float
   renames vec_round;

   -- vec_vrsqrtefp --

   function vec_vrsqrtefp
     (A : vector_float) return vector_float
   renames vec_rsqrte;

   -- vec_vsel --

   function vec_vsel
     (A : vector_float;
      B : vector_float;
      C : vector_bool_int) return vector_float
   renames vec_sel;

   function vec_vsel
     (A : vector_float;
      B : vector_float;
      C : vector_unsigned_int) return vector_float
   renames vec_sel;

   function vec_vsel
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_bool_int) return vector_signed_int
   renames vec_sel;

   function vec_vsel
     (A : vector_signed_int;
      B : vector_signed_int;
      C : vector_unsigned_int) return vector_signed_int
   renames vec_sel;

   function vec_vsel
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_bool_int) return vector_unsigned_int
   renames vec_sel;

   function vec_vsel
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : vector_unsigned_int) return vector_unsigned_int
   renames vec_sel;

   function vec_vsel
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_bool_int) return vector_bool_int
   renames vec_sel;

   function vec_vsel
     (A : vector_bool_int;
      B : vector_bool_int;
      C : vector_unsigned_int) return vector_bool_int
   renames vec_sel;

   function vec_vsel
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_bool_short) return vector_signed_short
   renames vec_sel;

   function vec_vsel
     (A : vector_signed_short;
      B : vector_signed_short;
      C : vector_unsigned_short) return vector_signed_short
   renames vec_sel;

   function vec_vsel
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_bool_short) return vector_unsigned_short
   renames vec_sel;

   function vec_vsel
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : vector_unsigned_short) return vector_unsigned_short
   renames vec_sel;

   function vec_vsel
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_bool_short) return vector_bool_short
   renames vec_sel;

   function vec_vsel
     (A : vector_bool_short;
      B : vector_bool_short;
      C : vector_unsigned_short) return vector_bool_short
   renames vec_sel;

   function vec_vsel
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_bool_char) return vector_signed_char
   renames vec_sel;

   function vec_vsel
     (A : vector_signed_char;
      B : vector_signed_char;
      C : vector_unsigned_char) return vector_signed_char
   renames vec_sel;

   function vec_vsel
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_bool_char) return vector_unsigned_char
   renames vec_sel;

   function vec_vsel
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : vector_unsigned_char) return vector_unsigned_char
   renames vec_sel;

   function vec_vsel
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_bool_char) return vector_bool_char
   renames vec_sel;

   function vec_vsel
     (A : vector_bool_char;
      B : vector_bool_char;
      C : vector_unsigned_char) return vector_bool_char
   renames vec_sel;

   -- vec_vsldoi --

   function vec_vsldoi
     (A : vector_float;
      B : vector_float;
      C : c_int) return vector_float
   renames vec_sld;

   function vec_vsldoi
     (A : vector_signed_int;
      B : vector_signed_int;
      C : c_int) return vector_signed_int
   renames vec_sld;

   function vec_vsldoi
     (A : vector_unsigned_int;
      B : vector_unsigned_int;
      C : c_int) return vector_unsigned_int
   renames vec_sld;

   function vec_vsldoi
     (A : vector_bool_int;
      B : vector_bool_int;
      C : c_int) return vector_bool_int
   renames vec_sld;

   function vec_vsldoi
     (A : vector_signed_short;
      B : vector_signed_short;
      C : c_int) return vector_signed_short
   renames vec_sld;

   function vec_vsldoi
     (A : vector_unsigned_short;
      B : vector_unsigned_short;
      C : c_int) return vector_unsigned_short
   renames vec_sld;

   function vec_vsldoi
     (A : vector_bool_short;
      B : vector_bool_short;
      C : c_int) return vector_bool_short
   renames vec_sld;

   function vec_vsldoi
     (A : vector_pixel;
      B : vector_pixel;
      C : c_int) return vector_pixel
   renames vec_sld;

   function vec_vsldoi
     (A : vector_signed_char;
      B : vector_signed_char;
      C : c_int) return vector_signed_char
   renames vec_sld;

   function vec_vsldoi
     (A : vector_unsigned_char;
      B : vector_unsigned_char;
      C : c_int) return vector_unsigned_char
   renames vec_sld;

   function vec_vsldoi
     (A : vector_bool_char;
      B : vector_bool_char;
      C : c_int) return vector_bool_char
   renames vec_sld;

   -- vec_vsl --

   function vec_vsl
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_int;
      B : vector_unsigned_short) return vector_signed_int
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_int;
      B : vector_unsigned_short) return vector_unsigned_int
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_bool_int
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_int;
      B : vector_unsigned_short) return vector_bool_int
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_int;
      B : vector_unsigned_char) return vector_bool_int
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_short;
      B : vector_unsigned_int) return vector_signed_short
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_short;
      B : vector_unsigned_int) return vector_unsigned_short
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_short;
      B : vector_unsigned_int) return vector_bool_short
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_bool_short
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_short;
      B : vector_unsigned_char) return vector_bool_short
   renames vec_sll;

   function vec_vsl
     (A : vector_pixel;
      B : vector_unsigned_int) return vector_pixel
   renames vec_sll;

   function vec_vsl
     (A : vector_pixel;
      B : vector_unsigned_short) return vector_pixel
   renames vec_sll;

   function vec_vsl
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_char;
      B : vector_unsigned_int) return vector_signed_char
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_char;
      B : vector_unsigned_short) return vector_signed_char
   renames vec_sll;

   function vec_vsl
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_char
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_char;
      B : vector_unsigned_short) return vector_unsigned_char
   renames vec_sll;

   function vec_vsl
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_char;
      B : vector_unsigned_int) return vector_bool_char
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_char;
      B : vector_unsigned_short) return vector_bool_char
   renames vec_sll;

   function vec_vsl
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_bool_char
   renames vec_sll;

   -- vec_vslo --

   function vec_vslo
     (A : vector_float;
      B : vector_signed_char) return vector_float
   renames vec_slo;

   function vec_vslo
     (A : vector_float;
      B : vector_unsigned_char) return vector_float
   renames vec_slo;

   function vec_vslo
     (A : vector_signed_int;
      B : vector_signed_char) return vector_signed_int
   renames vec_slo;

   function vec_vslo
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   renames vec_slo;

   function vec_vslo
     (A : vector_unsigned_int;
      B : vector_signed_char) return vector_unsigned_int
   renames vec_slo;

   function vec_vslo
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   renames vec_slo;

   function vec_vslo
     (A : vector_signed_short;
      B : vector_signed_char) return vector_signed_short
   renames vec_slo;

   function vec_vslo
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   renames vec_slo;

   function vec_vslo
     (A : vector_unsigned_short;
      B : vector_signed_char) return vector_unsigned_short
   renames vec_slo;

   function vec_vslo
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   renames vec_slo;

   function vec_vslo
     (A : vector_pixel;
      B : vector_signed_char) return vector_pixel
   renames vec_slo;

   function vec_vslo
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   renames vec_slo;

   function vec_vslo
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_slo;

   function vec_vslo
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   renames vec_slo;

   function vec_vslo
     (A : vector_unsigned_char;
      B : vector_signed_char) return vector_unsigned_char
   renames vec_slo;

   function vec_vslo
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_slo;

   -- vec_vspltisb --

   function vec_vspltisb
     (A : c_int) return vector_signed_char
   renames vec_splat_s8;

   -- vec_vspltish --

   function vec_vspltish
     (A : c_int) return vector_signed_short
   renames vec_splat_s16;

   -- vec_vspltisw --

   function vec_vspltisw
     (A : c_int) return vector_signed_int
   renames vec_splat_s32;

   -- vec_vsr --

   function vec_vsr
     (A : vector_signed_int;
      B : vector_unsigned_int) return vector_signed_int
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_int;
      B : vector_unsigned_short) return vector_signed_int
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_int;
      B : vector_unsigned_short) return vector_unsigned_int
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_bool_int
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_int;
      B : vector_unsigned_short) return vector_bool_int
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_int;
      B : vector_unsigned_char) return vector_bool_int
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_short;
      B : vector_unsigned_int) return vector_signed_short
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_short;
      B : vector_unsigned_short) return vector_signed_short
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_short;
      B : vector_unsigned_int) return vector_unsigned_short
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_short;
      B : vector_unsigned_int) return vector_bool_short
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_bool_short
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_short;
      B : vector_unsigned_char) return vector_bool_short
   renames vec_srl;

   function vec_vsr
     (A : vector_pixel;
      B : vector_unsigned_int) return vector_pixel
   renames vec_srl;

   function vec_vsr
     (A : vector_pixel;
      B : vector_unsigned_short) return vector_pixel
   renames vec_srl;

   function vec_vsr
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_char;
      B : vector_unsigned_int) return vector_signed_char
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_char;
      B : vector_unsigned_short) return vector_signed_char
   renames vec_srl;

   function vec_vsr
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_char;
      B : vector_unsigned_int) return vector_unsigned_char
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_char;
      B : vector_unsigned_short) return vector_unsigned_char
   renames vec_srl;

   function vec_vsr
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_char;
      B : vector_unsigned_int) return vector_bool_char
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_char;
      B : vector_unsigned_short) return vector_bool_char
   renames vec_srl;

   function vec_vsr
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_bool_char
   renames vec_srl;

   -- vec_vsro --

   function vec_vsro
     (A : vector_float;
      B : vector_signed_char) return vector_float
   renames vec_sro;

   function vec_vsro
     (A : vector_float;
      B : vector_unsigned_char) return vector_float
   renames vec_sro;

   function vec_vsro
     (A : vector_signed_int;
      B : vector_signed_char) return vector_signed_int
   renames vec_sro;

   function vec_vsro
     (A : vector_signed_int;
      B : vector_unsigned_char) return vector_signed_int
   renames vec_sro;

   function vec_vsro
     (A : vector_unsigned_int;
      B : vector_signed_char) return vector_unsigned_int
   renames vec_sro;

   function vec_vsro
     (A : vector_unsigned_int;
      B : vector_unsigned_char) return vector_unsigned_int
   renames vec_sro;

   function vec_vsro
     (A : vector_signed_short;
      B : vector_signed_char) return vector_signed_short
   renames vec_sro;

   function vec_vsro
     (A : vector_signed_short;
      B : vector_unsigned_char) return vector_signed_short
   renames vec_sro;

   function vec_vsro
     (A : vector_unsigned_short;
      B : vector_signed_char) return vector_unsigned_short
   renames vec_sro;

   function vec_vsro
     (A : vector_unsigned_short;
      B : vector_unsigned_char) return vector_unsigned_short
   renames vec_sro;

   function vec_vsro
     (A : vector_pixel;
      B : vector_signed_char) return vector_pixel
   renames vec_sro;

   function vec_vsro
     (A : vector_pixel;
      B : vector_unsigned_char) return vector_pixel
   renames vec_sro;

   function vec_vsro
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_sro;

   function vec_vsro
     (A : vector_signed_char;
      B : vector_unsigned_char) return vector_signed_char
   renames vec_sro;

   function vec_vsro
     (A : vector_unsigned_char;
      B : vector_signed_char) return vector_unsigned_char
   renames vec_sro;

   function vec_vsro
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_sro;

   -- vec_stvx --

   procedure vec_stvx
     (A : vector_float;
      B : c_int;
      C : vector_float_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_float;
      B : c_int;
      C : float_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_signed_int;
      B : c_int;
      C : vector_signed_int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_unsigned_int;
      B : c_int;
      C : vector_unsigned_int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_int;
      B : c_int;
      C : vector_bool_int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_signed_short;
      B : c_int;
      C : vector_signed_short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_unsigned_short;
      B : c_int;
      C : vector_unsigned_short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_short;
      B : c_int;
      C : vector_bool_short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_pixel;
      B : c_int;
      C : vector_pixel_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_pixel;
      B : c_int;
      C : short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_signed_char;
      B : c_int;
      C : vector_signed_char_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_unsigned_char;
      B : c_int;
      C : vector_unsigned_char_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_char;
      B : c_int;
      C : vector_bool_char_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr)
   renames vec_st;

   procedure vec_stvx
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr)
   renames vec_st;

   -- vec_stvxl --

   procedure vec_stvxl
     (A : vector_float;
      B : c_int;
      C : vector_float_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_float;
      B : c_int;
      C : float_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_signed_int;
      B : c_int;
      C : vector_signed_int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_signed_int;
      B : c_int;
      C : int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_unsigned_int;
      B : c_int;
      C : vector_unsigned_int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_unsigned_int;
      B : c_int;
      C : unsigned_int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_int;
      B : c_int;
      C : vector_bool_int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_int;
      B : c_int;
      C : unsigned_int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_int;
      B : c_int;
      C : int_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_signed_short;
      B : c_int;
      C : vector_signed_short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_signed_short;
      B : c_int;
      C : short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_unsigned_short;
      B : c_int;
      C : vector_unsigned_short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_unsigned_short;
      B : c_int;
      C : unsigned_short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_short;
      B : c_int;
      C : vector_bool_short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_short;
      B : c_int;
      C : unsigned_short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_short;
      B : c_int;
      C : short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_pixel;
      B : c_int;
      C : vector_pixel_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_pixel;
      B : c_int;
      C : unsigned_short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_pixel;
      B : c_int;
      C : short_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_signed_char;
      B : c_int;
      C : vector_signed_char_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_signed_char;
      B : c_int;
      C : signed_char_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_unsigned_char;
      B : c_int;
      C : vector_unsigned_char_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_unsigned_char;
      B : c_int;
      C : unsigned_char_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_char;
      B : c_int;
      C : vector_bool_char_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_char;
      B : c_int;
      C : unsigned_char_ptr)
   renames vec_stl;

   procedure vec_stvxl
     (A : vector_bool_char;
      B : c_int;
      C : signed_char_ptr)
   renames vec_stl;

   -- vec_vsubcuw --

   function vec_vsubcuw
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_subc;

   -- vec_vsum2sws --

   function vec_vsum2sws
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_sum2s;

   -- vec_vsumsws --

   function vec_vsumsws
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_sums;

   -- vec_vrfiz --

   function vec_vrfiz
     (A : vector_float) return vector_float
   renames vec_trunc;

   -- vec_vxor --

   function vec_vxor
     (A : vector_float;
      B : vector_float) return vector_float
   renames vec_xor;

   function vec_vxor
     (A : vector_float;
      B : vector_bool_int) return vector_float
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_int;
      B : vector_float) return vector_float
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_int;
      B : vector_bool_int) return vector_bool_int
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_xor;

   function vec_vxor
     (A : vector_signed_int;
      B : vector_bool_int) return vector_signed_int
   renames vec_xor;

   function vec_vxor
     (A : vector_signed_int;
      B : vector_signed_int) return vector_signed_int
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_xor;

   function vec_vxor
     (A : vector_unsigned_int;
      B : vector_bool_int) return vector_unsigned_int
   renames vec_xor;

   function vec_vxor
     (A : vector_unsigned_int;
      B : vector_unsigned_int) return vector_unsigned_int
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_short;
      B : vector_bool_short) return vector_bool_short
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_xor;

   function vec_vxor
     (A : vector_signed_short;
      B : vector_bool_short) return vector_signed_short
   renames vec_xor;

   function vec_vxor
     (A : vector_signed_short;
      B : vector_signed_short) return vector_signed_short
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_xor;

   function vec_vxor
     (A : vector_unsigned_short;
      B : vector_bool_short) return vector_unsigned_short
   renames vec_xor;

   function vec_vxor
     (A : vector_unsigned_short;
      B : vector_unsigned_short) return vector_unsigned_short
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_char;
      B : vector_bool_char) return vector_bool_char
   renames vec_xor;

   function vec_vxor
     (A : vector_signed_char;
      B : vector_bool_char) return vector_signed_char
   renames vec_xor;

   function vec_vxor
     (A : vector_signed_char;
      B : vector_signed_char) return vector_signed_char
   renames vec_xor;

   function vec_vxor
     (A : vector_bool_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_xor;

   function vec_vxor
     (A : vector_unsigned_char;
      B : vector_bool_char) return vector_unsigned_char
   renames vec_xor;

   function vec_vxor
     (A : vector_unsigned_char;
      B : vector_unsigned_char) return vector_unsigned_char
   renames vec_xor;

   ----------------------------------------------
   -- [PIM 2.5.3 Value for adjusting pointers] --
   ----------------------------------------------

   --  "At compile time, vec_step (vec_data) produces the integer value
   --  representing the amount by which a pointer to a component of an AltiVec
   --  data type should increment to cause a pointer increment to increment by
   --  16 bytes".

   function vec_step (V : vector_unsigned_char) return Integer;
   function vec_step (V : vector_signed_char) return Integer;
   function vec_step (V : vector_bool_char) return Integer;

   function vec_step (V : vector_unsigned_short) return Integer;
   function vec_step (V : vector_signed_short) return Integer;
   function vec_step (V : vector_bool_short) return Integer;

   function vec_step (V : vector_unsigned_int) return Integer;
   function vec_step (V : vector_signed_int) return Integer;
   function vec_step (V : vector_bool_int) return Integer;

   function vec_step (V : vector_float) return Integer;
   function vec_step (V : vector_pixel) return Integer;

private

   -------------------------------------
   -- Different flavors of interfaces --
   -------------------------------------

   --  The vast majority of the user visible functions are just neutral type
   --  conversion wrappers around calls to low level primitives. For instance:
   --
   --        function vec_sll
   --          (A : vector_signed_int;
   --           B : vector_unsigned_char) return vector_signed_int is
   --        begin
   --          return To_VSI (vsl (To_VSI (A), To_VSI (B)));
   --        end vec_sll;
   --
   --  We actually don't always need an explicit wrapper and can bind directly
   --  with a straight Import of the low level routine, or a renaming of such
   --  instead.
   --
   --  A direct binding is not possible (that is, a wrapper is mandatory) in
   --  a number of cases:
   --
   --  o When the high-level/low-level types don't match, in which case a
   --  straight import would risk wrong code generation or compiler blowups in
   --  the Hard binding case. This is the case for 'B' in the example above.
   --
   --  o When the high-level/low-level argument lists differ, as is the case
   --  for most of the AltiVec predicates, relying on a low-level primitive
   --  which expects a control code argument, like:
   --
   --        function vec_any_ne
   --           (A : vector_signed_int;
   --            B : vector_signed_int) return c_int is
   --        begin
   --          return vcmpequw_p (CR6_LT_REV, To_VSI (A), To_VSI (B));
   --        end vec_any_ne;
   --
   --  o When the high-level/low-level arguments order don't match, as in:
   --
   --        function vec_cmplt
   --           (A : vector_unsigned_char;
   --            B : vector_unsigned_char) return vector_bool_char is
   --         begin
   --           return To_VBC (vcmpgtub (To_VSC (B), To_VSC (A)));
   --         end vec_cmplt;
   --
   --  Conversely, a direct (without wrapper) binding is sometimes mandatory
   --  in the Hard binding case, because the corresponding low level code
   --  accept only literal values for some arguments. Inlined calls to the
   --  wrapper with proper arguments would be fine, but the wrapper body
   --  itself would not be compilable. These can of course also be used in the
   --  Soft binding, and so are naturally in this common unit.
   --
   --  Fortunately, the sets of operations for which a wrapper is required
   --  and the set of operations for which a wrapper would not be compilable
   --  do not intersect.

   -----------------------------
   -- Inlining considerations --
   -----------------------------

   --  The intent in the Hard binding case is to eventually map operations
   --  to hardware instructions. Needless to say, intermediate function calls
   --  do not fit this purpose, so all the user visible subprograms shall be
   --  inlined. In the soft case, the bulk of the work is performed by the
   --  low level routines, and those exported by this unit are short enough
   --  for the inlining to make sense and even be beneficial, so...

   pragma Inline_Always (vec_abs);
   pragma Inline_Always (vec_abss);
   pragma Inline_Always (vec_add);
   pragma Inline_Always (vec_vaddfp);
   pragma Inline_Always (vec_vadduwm);
   pragma Inline_Always (vec_vadduhm);
   pragma Inline_Always (vec_vaddubm);
   pragma Inline_Always (vec_addc);
   pragma Inline_Always (vec_adds);
   pragma Inline_Always (vec_vaddsws);
   pragma Inline_Always (vec_vadduws);
   pragma Inline_Always (vec_vaddshs);
   pragma Inline_Always (vec_vadduhs);
   pragma Inline_Always (vec_vaddsbs);
   pragma Inline_Always (vec_vaddubs);
   pragma Inline_Always (vec_and);
   pragma Inline_Always (vec_andc);
   pragma Inline_Always (vec_avg);
   pragma Inline_Always (vec_vavgsw);
   pragma Inline_Always (vec_vavguw);
   pragma Inline_Always (vec_vavgsh);
   pragma Inline_Always (vec_vavguh);
   pragma Inline_Always (vec_vavgsb);
   pragma Inline_Always (vec_vavgub);
   pragma Inline_Always (vec_ceil);
   pragma Inline_Always (vec_cmpb);
   pragma Inline_Always (vec_cmpeq);
   pragma Inline_Always (vec_vcmpeqfp);
   pragma Inline_Always (vec_vcmpequw);
   pragma Inline_Always (vec_vcmpequh);
   pragma Inline_Always (vec_vcmpequb);
   pragma Inline_Always (vec_cmpge);
   pragma Inline_Always (vec_cmpgt);
   pragma Inline_Always (vec_vcmpgtfp);
   pragma Inline_Always (vec_vcmpgtsw);
   pragma Inline_Always (vec_vcmpgtuw);
   pragma Inline_Always (vec_vcmpgtsh);
   pragma Inline_Always (vec_vcmpgtuh);
   pragma Inline_Always (vec_vcmpgtsb);
   pragma Inline_Always (vec_vcmpgtub);
   pragma Inline_Always (vec_cmple);
   pragma Inline_Always (vec_cmplt);
   pragma Inline_Always (vec_expte);
   pragma Inline_Always (vec_floor);
   pragma Inline_Always (vec_ld);
   pragma Inline_Always (vec_lde);
   pragma Inline_Always (vec_lvewx);
   pragma Inline_Always (vec_lvehx);
   pragma Inline_Always (vec_lvebx);
   pragma Inline_Always (vec_ldl);
   pragma Inline_Always (vec_loge);
   pragma Inline_Always (vec_lvsl);
   pragma Inline_Always (vec_lvsr);
   pragma Inline_Always (vec_madd);
   pragma Inline_Always (vec_madds);
   pragma Inline_Always (vec_max);
   pragma Inline_Always (vec_vmaxfp);
   pragma Inline_Always (vec_vmaxsw);
   pragma Inline_Always (vec_vmaxuw);
   pragma Inline_Always (vec_vmaxsh);
   pragma Inline_Always (vec_vmaxuh);
   pragma Inline_Always (vec_vmaxsb);
   pragma Inline_Always (vec_vmaxub);
   pragma Inline_Always (vec_mergeh);
   pragma Inline_Always (vec_vmrghw);
   pragma Inline_Always (vec_vmrghh);
   pragma Inline_Always (vec_vmrghb);
   pragma Inline_Always (vec_mergel);
   pragma Inline_Always (vec_vmrglw);
   pragma Inline_Always (vec_vmrglh);
   pragma Inline_Always (vec_vmrglb);
   pragma Inline_Always (vec_mfvscr);
   pragma Inline_Always (vec_min);
   pragma Inline_Always (vec_vminfp);
   pragma Inline_Always (vec_vminsw);
   pragma Inline_Always (vec_vminuw);
   pragma Inline_Always (vec_vminsh);
   pragma Inline_Always (vec_vminuh);
   pragma Inline_Always (vec_vminsb);
   pragma Inline_Always (vec_vminub);
   pragma Inline_Always (vec_mladd);
   pragma Inline_Always (vec_mradds);
   pragma Inline_Always (vec_msum);
   pragma Inline_Always (vec_vmsumshm);
   pragma Inline_Always (vec_vmsumuhm);
   pragma Inline_Always (vec_vmsummbm);
   pragma Inline_Always (vec_vmsumubm);
   pragma Inline_Always (vec_msums);
   pragma Inline_Always (vec_vmsumshs);
   pragma Inline_Always (vec_vmsumuhs);
   pragma Inline_Always (vec_mtvscr);
   pragma Inline_Always (vec_mule);
   pragma Inline_Always (vec_vmulesh);
   pragma Inline_Always (vec_vmuleuh);
   pragma Inline_Always (vec_vmulesb);
   pragma Inline_Always (vec_vmuleub);
   pragma Inline_Always (vec_mulo);
   pragma Inline_Always (vec_vmulosh);
   pragma Inline_Always (vec_vmulouh);
   pragma Inline_Always (vec_vmulosb);
   pragma Inline_Always (vec_vmuloub);
   pragma Inline_Always (vec_nmsub);
   pragma Inline_Always (vec_nor);
   pragma Inline_Always (vec_or);
   pragma Inline_Always (vec_pack);
   pragma Inline_Always (vec_vpkuwum);
   pragma Inline_Always (vec_vpkuhum);
   pragma Inline_Always (vec_packpx);
   pragma Inline_Always (vec_packs);
   pragma Inline_Always (vec_vpkswss);
   pragma Inline_Always (vec_vpkuwus);
   pragma Inline_Always (vec_vpkshss);
   pragma Inline_Always (vec_vpkuhus);
   pragma Inline_Always (vec_packsu);
   pragma Inline_Always (vec_vpkswus);
   pragma Inline_Always (vec_vpkshus);
   pragma Inline_Always (vec_perm);
   pragma Inline_Always (vec_re);
   pragma Inline_Always (vec_rl);
   pragma Inline_Always (vec_vrlw);
   pragma Inline_Always (vec_vrlh);
   pragma Inline_Always (vec_vrlb);
   pragma Inline_Always (vec_round);
   pragma Inline_Always (vec_rsqrte);
   pragma Inline_Always (vec_sel);
   pragma Inline_Always (vec_sl);
   pragma Inline_Always (vec_vslw);
   pragma Inline_Always (vec_vslh);
   pragma Inline_Always (vec_vslb);
   pragma Inline_Always (vec_sll);
   pragma Inline_Always (vec_slo);
   pragma Inline_Always (vec_sr);
   pragma Inline_Always (vec_vsrw);
   pragma Inline_Always (vec_vsrh);
   pragma Inline_Always (vec_vsrb);
   pragma Inline_Always (vec_sra);
   pragma Inline_Always (vec_vsraw);
   pragma Inline_Always (vec_vsrah);
   pragma Inline_Always (vec_vsrab);
   pragma Inline_Always (vec_srl);
   pragma Inline_Always (vec_sro);
   pragma Inline_Always (vec_st);
   pragma Inline_Always (vec_ste);
   pragma Inline_Always (vec_stvewx);
   pragma Inline_Always (vec_stvehx);
   pragma Inline_Always (vec_stvebx);
   pragma Inline_Always (vec_stl);
   pragma Inline_Always (vec_sub);
   pragma Inline_Always (vec_vsubfp);
   pragma Inline_Always (vec_vsubuwm);
   pragma Inline_Always (vec_vsubuhm);
   pragma Inline_Always (vec_vsububm);
   pragma Inline_Always (vec_subc);
   pragma Inline_Always (vec_subs);
   pragma Inline_Always (vec_vsubsws);
   pragma Inline_Always (vec_vsubuws);
   pragma Inline_Always (vec_vsubshs);
   pragma Inline_Always (vec_vsubuhs);
   pragma Inline_Always (vec_vsubsbs);
   pragma Inline_Always (vec_vsububs);
   pragma Inline_Always (vec_sum4s);
   pragma Inline_Always (vec_vsum4shs);
   pragma Inline_Always (vec_vsum4sbs);
   pragma Inline_Always (vec_vsum4ubs);
   pragma Inline_Always (vec_sum2s);
   pragma Inline_Always (vec_sums);
   pragma Inline_Always (vec_trunc);
   pragma Inline_Always (vec_unpackh);
   pragma Inline_Always (vec_vupkhsh);
   pragma Inline_Always (vec_vupkhpx);
   pragma Inline_Always (vec_vupkhsb);
   pragma Inline_Always (vec_unpackl);
   pragma Inline_Always (vec_vupklpx);
   pragma Inline_Always (vec_vupklsh);
   pragma Inline_Always (vec_vupklsb);
   pragma Inline_Always (vec_xor);

   pragma Inline_Always (vec_all_eq);
   pragma Inline_Always (vec_all_ge);
   pragma Inline_Always (vec_all_gt);
   pragma Inline_Always (vec_all_in);
   pragma Inline_Always (vec_all_le);
   pragma Inline_Always (vec_all_lt);
   pragma Inline_Always (vec_all_nan);
   pragma Inline_Always (vec_all_ne);
   pragma Inline_Always (vec_all_nge);
   pragma Inline_Always (vec_all_ngt);
   pragma Inline_Always (vec_all_nle);
   pragma Inline_Always (vec_all_nlt);
   pragma Inline_Always (vec_all_numeric);
   pragma Inline_Always (vec_any_eq);
   pragma Inline_Always (vec_any_ge);
   pragma Inline_Always (vec_any_gt);
   pragma Inline_Always (vec_any_le);
   pragma Inline_Always (vec_any_lt);
   pragma Inline_Always (vec_any_nan);
   pragma Inline_Always (vec_any_ne);
   pragma Inline_Always (vec_any_nge);
   pragma Inline_Always (vec_any_ngt);
   pragma Inline_Always (vec_any_nle);
   pragma Inline_Always (vec_any_nlt);
   pragma Inline_Always (vec_any_numeric);
   pragma Inline_Always (vec_any_out);

   --  Similarly, vec_step is expected to be turned into a compile time
   --  constant, so ...

   pragma Inline_Always (vec_step);

end GNAT.Altivec.Vector_Operations;
