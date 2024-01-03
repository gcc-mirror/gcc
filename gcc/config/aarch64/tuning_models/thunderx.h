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

#ifndef GCC_AARCH64_H_THUNDERX
#define GCC_AARCH64_H_THUNDERX

#include "generic.h"

static const struct cpu_regmove_cost thunderx_regmove_cost =
{
  2, /* GP2GP  */
  2, /* GP2FP  */
  6, /* FP2GP  */
  4 /* FP2FP  */
};

static const advsimd_vec_cost thunderx_advsimd_vector_cost =
{
  4, /* int_stmt_cost  */
  1, /* fp_stmt_cost  */
  0, /* ld2_st2_permute_cost  */
  0, /* ld3_st3_permute_cost  */
  0, /* ld4_st4_permute_cost  */
  4, /* permute_cost  */
  2, /* reduc_i8_cost  */
  2, /* reduc_i16_cost  */
  2, /* reduc_i32_cost  */
  2, /* reduc_i64_cost  */
  2, /* reduc_f16_cost  */
  2, /* reduc_f32_cost  */
  2, /* reduc_f64_cost  */
  2, /* store_elt_extra_cost  */
  2, /* vec_to_scalar_cost  */
  2, /* scalar_to_vec_cost  */
  3, /* align_load_cost  */
  5, /* unalign_load_cost  */
  5, /* unalign_store_cost  */
  1  /* store_cost  */
};

/* ThunderX costs for vector insn classes.  */
static const struct cpu_vector_cost thunderx_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  3, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  3, /* cond_taken_branch_cost  */
  3, /* cond_not_taken_branch_cost  */
  &thunderx_advsimd_vector_cost, /* advsimd  */
  nullptr, /* sve  */
  nullptr /* issue_info  */
};

static const cpu_prefetch_tune thunderx_prefetch_tune =
{
  8,			/* num_slots  */
  32,			/* l1_cache_size  */
  128,			/* l1_cache_line_size  */
  -1,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static const struct tune_params thunderx_tunings =
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
  (AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND),	/* tune_flags.  */
  &thunderx_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALIGNED,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALIGNED    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_THUNDERX.  */
