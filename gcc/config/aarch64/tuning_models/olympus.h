/* Tuning model description for the NVIDIA Olympus core.
   Copyright The GNU Toolchain Authors.

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

#ifndef GCC_AARCH64_H_OLYMPUS
#define GCC_AARCH64_H_OLYMPUS

#include "generic.h"

static struct cpu_regmove_cost olympus_regmove_cost =
{
  1, /* GP2GP  */
  /* Spilling to int<->fp instead of memory is recommended so set
     realistic costs compared to memmov_cost.  */
  3, /* GP2FP  */
  3, /* FP2GP  */
  2 /* FP2FP  */
};

static advsimd_vec_cost olympus_advsimd_vector_cost =
{
  2, /* int_stmt_cost  */
  2, /* fp_stmt_cost  */
  2, /* ld2_st2_permute_cost */
  2, /* ld3_st3_permute_cost  */
  3, /* ld4_st4_permute_cost  */
  2, /* permute_cost  */
  5, /* reduc_i8_cost  */
  3, /* reduc_i16_cost  */
  3, /* reduc_i32_cost  */
  2, /* reduc_i64_cost  */
  4, /* reduc_f16_cost  */
  4, /* reduc_f32_cost  */
  4, /* reduc_f64_cost  */
  2, /* store_elt_extra_cost  */
  8, /* vec_to_scalar_cost  */
  4, /* scalar_to_vec_cost  */
  6, /* align_load_cost  */
  6, /* unalign_load_cost  */
  1, /* unalign_store_cost  */
  1  /* store_cost  */
};

static sve_vec_cost olympus_sve_vector_cost =
{
  {
    2, /* int_stmt_cost  */
    2, /* fp_stmt_cost  */
    2, /* ld2_st2_permute_cost  */
    3, /* ld3_st3_permute_cost  */
    3, /* ld4_st4_permute_cost  */
    2, /* permute_cost  */
    9, /* reduc_i8_cost  */
    8, /* reduc_i16_cost  */
    6, /* reduc_i32_cost  */
    2, /* reduc_i64_cost  */
    8, /* reduc_f16_cost  */
    6, /* reduc_f32_cost  */
    4, /* reduc_f64_cost  */
    2, /* store_elt_extra_cost  */
    8, /* vec_to_scalar_cost  */
    4, /* scalar_to_vec_cost  */
    4, /* align_load_cost  */
    6, /* unalign_load_cost  */
    1, /* unalign_store_cost  */
    1  /* store_cost  */
  },
  3, /* clast_cost  */
  10, /* fadda_f16_cost  */
  6, /* fadda_f32_cost  */
  4, /* fadda_f64_cost  */
  14, /* gather_load_x32_cost  */
  12, /* gather_load_x64_cost  */
  42, /* gather_load_x32_init_cost  */
  24, /* gather_load_x64_init_cost  */
  1 /* scatter_store_elt_cost  */
};

static aarch64_scalar_vec_issue_info olympus_scalar_issue_info =
{
  4, /* loads_stores_per_cycle  */
  2, /* stores_per_cycle  */
  8, /* general_ops_per_cycle  */
  0, /* fp_simd_load_general_ops  */
  1 /* fp_simd_store_general_ops  */
};

static aarch64_advsimd_vec_issue_info olympus_advsimd_issue_info =
{
  {
    3, /* loads_stores_per_cycle  */
    2, /* stores_per_cycle  */
    6, /* general_ops_per_cycle  */
    0, /* fp_simd_load_general_ops  */
    1 /* fp_simd_store_general_ops  */
  },
  2, /* ld2_st2_general_ops  */
  2, /* ld3_st3_general_ops  */
  3 /* ld4_st4_general_ops  */
};

static aarch64_sve_vec_issue_info olympus_sve_issue_info =
{
  {
    {
      3, /* loads_stores_per_cycle  */
      2, /* stores_per_cycle  */
      6, /* general_ops_per_cycle  */
      0, /* fp_simd_load_general_ops  */
      1 /* fp_simd_store_general_ops  */
    },
    2, /* ld2_st2_general_ops  */
    2, /* ld3_st3_general_ops  */
    3 /* ld4_st4_general_ops  */
  },
  2, /* pred_ops_per_cycle  */
  1, /* while_pred_ops  */
  0, /* int_cmp_pred_ops  */
  0, /* fp_cmp_pred_ops  */
  1, /* gather_scatter_pair_general_ops  */
  1 /* gather_scatter_pair_pred_ops  */
};

static aarch64_vec_issue_info olympus_vec_issue_info =
{
  &olympus_scalar_issue_info,
  &olympus_advsimd_issue_info,
  &olympus_sve_issue_info
};

/* Olympus costs for vector insn classes.  */
static struct cpu_vector_cost olympus_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  2, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &olympus_advsimd_vector_cost, /* advsimd  */
  &olympus_sve_vector_cost, /* sve  */
  &olympus_vec_issue_info /* issue_info  */
};

/* Olympus prefetch settings (which disable prefetch).  */
static cpu_prefetch_tune olympus_prefetch_tune =
{
  0,			/* num_slots  */
  -1,			/* l1_cache_size  */
  64,			/* l1_cache_line_size  */
  -1,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static struct tune_params olympus_tunings =
{
  &cortexa76_extra_costs,
  &generic_armv9_a_addrcost_table,
  &olympus_regmove_cost,
  &olympus_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_128, /* sve_width  */
  { 4, /* load_int.  */
    1, /* store_int.  */
    6, /* load_fp.  */
    3, /* store_fp.  */
    5, /* load_pred.  */
    1 /* store_pred.  */
  }, /* memmov_cost.  */
  10, /* issue_rate  */
  AARCH64_FUSE_NEOVERSE_BASE, /* fusible_ops  */
  "32:16",	/* function_align.  */
  "4",		/* jump_align.  */
  "32:16",	/* loop_align.  */
  8,	/* int_reassoc_width.  */
  6,	/* fp_reassoc_width.  */
  4,	/* fma_reassoc_width.  */
  6,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_BASE
   | AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS
   | AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT
   | AARCH64_EXTRA_TUNE_AVOID_PRED_RMW),	/* tune_flags.  */
  &olympus_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS	   /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_OLYMPUS.  */
