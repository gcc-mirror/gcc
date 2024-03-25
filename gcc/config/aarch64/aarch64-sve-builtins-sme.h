/* ACLE support for AArch64 SME.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_SVE_BUILTINS_SME_H
#define GCC_AARCH64_SVE_BUILTINS_SME_H

namespace aarch64_sve
{
  namespace functions
  {
    extern const function_base *const arm_has_sme;
    extern const function_base *const arm_in_streaming_mode;
    extern const function_base *const svadd_za;
    extern const function_base *const svadd_write_za;
    extern const function_base *const svaddha_za;
    extern const function_base *const svaddva_za;
    extern const function_base *const svbmopa_za;
    extern const function_base *const svbmops_za;
    extern const function_base *const svcntsb;
    extern const function_base *const svcntsd;
    extern const function_base *const svcntsh;
    extern const function_base *const svcntsw;
    extern const function_base *const svdot_za;
    extern const function_base *const svdot_lane_za;
    extern const function_base *const svld1_hor_za;
    extern const function_base *const svld1_ver_za;
    extern const function_base *const svldr_za;
    extern const function_base *const svldr_zt;
    extern const function_base *const svluti2_lane_zt;
    extern const function_base *const svluti4_lane_zt;
    extern const function_base *const svmla_za;
    extern const function_base *const svmla_lane_za;
    extern const function_base *const svmls_za;
    extern const function_base *const svmls_lane_za;
    extern const function_base *const svmopa_za;
    extern const function_base *const svmops_za;
    extern const function_base *const svread_za;
    extern const function_base *const svread_hor_za;
    extern const function_base *const svread_ver_za;
    extern const function_base *const svst1_hor_za;
    extern const function_base *const svst1_ver_za;
    extern const function_base *const svstr_za;
    extern const function_base *const svstr_zt;
    extern const function_base *const svsub_za;
    extern const function_base *const svsub_write_za;
    extern const function_base *const svsudot_za;
    extern const function_base *const svsudot_lane_za;
    extern const function_base *const svsuvdot_lane_za;
    extern const function_base *const svsumopa_za;
    extern const function_base *const svsumops_za;
    extern const function_base *const svusdot_za;
    extern const function_base *const svusdot_lane_za;
    extern const function_base *const svusvdot_lane_za;
    extern const function_base *const svusmopa_za;
    extern const function_base *const svusmops_za;
    extern const function_base *const svwrite_za;
    extern const function_base *const svwrite_hor_za;
    extern const function_base *const svwrite_ver_za;
    extern const function_base *const svundef_za;
    extern const function_base *const svvdot_lane_za;
    extern const function_base *const svzero_mask_za;
    extern const function_base *const svzero_za;
    extern const function_base *const svzero_zt;
  }
}

#endif
