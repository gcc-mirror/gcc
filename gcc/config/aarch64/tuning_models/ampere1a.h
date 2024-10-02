/* Tuning model description for AArch64 architecture.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_H_AMPERE1A
#define GCC_AARCH64_H_AMPERE1A

#include "generic.h"

static const struct tune_params ampere1a_tunings =
{
  &ampere1a_extra_costs,
  &generic_addrcost_table,
  &generic_regmove_cost,
  &ampere1_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  { 4, /* load_int.  */
    4, /* store_int.  */
    4, /* load_fp.  */
    4, /* store_fp.  */
    4, /* load_pred.  */
    4 /* store_pred.  */
  }, /* memmov_cost.  */
  4, /* issue_rate  */
  (AARCH64_FUSE_BASE | AARCH64_FUSE_ADRP_ADD | AARCH64_FUSE_MOVK
   | AARCH64_FUSE_ALU_BRANCH | AARCH64_FUSE_ALU_CBZ
   | AARCH64_FUSE_ADDSUB_2REG_CONST1), /* fusible_ops  */
  "32",		/* function_align.  */
  "4",		/* jump_align.  */
  "32:16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  4,	/* fma_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_AVOID_CROSS_LOOP_FMA
   | AARCH64_EXTRA_TUNE_FULLY_PIPELINED_FMA), /* tune_flags.  */
  &ampere1_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALIGNED,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALIGNED    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_AMPERE1A.  */
