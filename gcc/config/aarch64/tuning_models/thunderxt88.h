/* Tuning model description for AArch64 architecture.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_H_THUNDERXT88
#define GCC_AARCH64_H_THUNDERXT88

#include "generic.h"
#include "thunderx.h"

static const cpu_prefetch_tune thunderxt88_prefetch_tune =
{
  8,			/* num_slots  */
  32,			/* l1_cache_size  */
  128,			/* l1_cache_line_size  */
  16*1024,		/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  3			/* default_opt_level  */
};

static const struct tune_params thunderxt88_tunings =
{
  &thunderx_extra_costs,
  &generic_addrcost_table,
  &thunderx_regmove_cost,
  &thunderx_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  { 6, /* load_int.  */
    6, /* store_int.  */
    6, /* load_fp.  */
    6, /* store_fp.  */
    6, /* load_pred.  */
    6 /* store_pred.  */
  }, /* memmov_cost.  */
  2, /* issue_rate  */
  AARCH64_FUSE_ALU_BRANCH, /* fusible_ops  */
  "8",	/* function_align.  */
  "8",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* fma_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_OFF,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &thunderxt88_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALIGNED,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALIGNED    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_THUNDERXT88.  */
