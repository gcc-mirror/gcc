/* Tuning model description for FUJITSU-MONAKA.
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

#ifndef GCC_AARCH64_H_FUJITSU_MONAKA
#define GCC_AARCH64_H_FUJITSU_MONAKA

#include "generic.h"
#include "generic_armv9_a.h"

/* Tuning parameters for FUJITSU-MONAKA processor.  It is copied from the
   generic one except for the vector width for now.  */
static const struct tune_params fujitsu_monaka_tunings =
{
  &cortexa76_extra_costs,
  &generic_armv9_a_addrcost_table,
  &generic_armv9_a_regmove_cost,
  &generic_armv9_a_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_256, /* sve_width.  */
  { 4, /* load_int.  */
    1, /* store_int.  */
    6, /* load_fp.  */
    2, /* store_fp.  */
    6, /* load_pred.  */
    1 /* store_pred.  */
  }, /* memmov_cost.  */
  3, /* issue_rate.  */
  AARCH64_FUSE_BASE, /* fusible_ops.  */
  "32:16",	/* function_align.  */
  "4",		/* jump_align.  */
  "32:16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  2,	/* fma_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_BASE
   | AARCH64_EXTRA_TUNE_USE_NEW_VECTOR_COSTS
   | AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT),	/* tune_flags.  */
  &generic_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS	   /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_FUJITSU_MONAKA.  */
