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

#ifndef GCC_AARCH64_H_EMAG
#define GCC_AARCH64_H_EMAG

#include "generic.h"

static const struct tune_params emag_tunings =
{
  &xgene1_extra_costs,
  &xgene1_addrcost_table,
  &xgene1_regmove_cost,
  &xgene1_vector_cost,
  &generic_branch_cost,
  &xgene1_approx_modes,
  SVE_NOT_IMPLEMENTED,
  { 6, /* load_int.  */
    6, /* store_int.  */
    6, /* load_fp.  */
    6, /* store_fp.  */
    6, /* load_pred.  */
    6 /* store_pred.  */
  }, /* memmov_cost.  */
  4, /* issue_rate  */
  AARCH64_FUSE_NOTHING, /* fusible_ops  */
  "16",	/* function_align.  */
  "16",	/* jump_align.  */
  "16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* fma_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  17,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_OFF,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NO_LDP_STP_QREGS),	/* tune_flags.  */
  &xgene1_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_EMAG.  */
