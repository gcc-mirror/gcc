/* Tuning model description for AArch64 architecture.
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

#ifndef GCC_AARCH64_H_GENERIC_ARMV8_A
#define GCC_AARCH64_H_GENERIC_ARMV8_A

#include "generic.h"

static const struct cpu_addrcost_table generic_armv8_a_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  0, /* post_modify_ld3_st3  */
  0, /* post_modify_ld4_st4  */
  0, /* register_offset  */
  0, /* register_sextend  */
  0, /* register_zextend  */
  0 /* imm_offset  */
};

static const struct cpu_regmove_cost generic_armv8_a_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

/* Generic costs for Advanced SIMD vector operations.   */
static const advsimd_vec_cost generic_armv8_a_advsimd_vector_cost =
{
  1, /* int_stmt_cost  */
  1, /* fp_stmt_cost  */
  0, /* ld2_st2_permute_cost  */
  0, /* ld3_st3_permute_cost  */
  0, /* ld4_st4_permute_cost  */
  2, /* permute_cost  */
  2, /* reduc_i8_cost  */
  2, /* reduc_i16_cost  */
  2, /* reduc_i32_cost  */
  2, /* reduc_i64_cost  */
  2, /* reduc_f16_cost  */
  2, /* reduc_f32_cost  */
  2, /* reduc_f64_cost  */
  2, /* store_elt_extra_cost  */
  2, /* vec_to_scalar_cost  */
  1, /* scalar_to_vec_cost  */
  1, /* align_load_cost  */
  1, /* unalign_load_cost  */
  1, /* unalign_store_cost  */
  1  /* store_cost  */
};

/* Generic costs for SVE vector operations.  */
static const sve_vec_cost generic_armv8_a_sve_vector_cost =
{
  {
    1, /* int_stmt_cost  */
    1, /* fp_stmt_cost  */
    0, /* ld2_st2_permute_cost  */
    0, /* ld3_st3_permute_cost  */
    0, /* ld4_st4_permute_cost  */
    2, /* permute_cost  */
    2, /* reduc_i8_cost  */
    2, /* reduc_i16_cost  */
    2, /* reduc_i32_cost  */
    2, /* reduc_i64_cost  */
    2, /* reduc_f16_cost  */
    2, /* reduc_f32_cost  */
    2, /* reduc_f64_cost  */
    2, /* store_elt_extra_cost  */
    2, /* vec_to_scalar_cost  */
    1, /* scalar_to_vec_cost  */
    1, /* align_load_cost  */
    1, /* unalign_load_cost  */
    1, /* unalign_store_cost  */
    1  /* store_cost  */
  },
  2, /* clast_cost  */
  2, /* fadda_f16_cost  */
  2, /* fadda_f32_cost  */
  2, /* fadda_f64_cost  */
  4, /* gather_load_x32_cost  */
  2, /* gather_load_x64_cost  */
  12, /* gather_load_x32_init_cost  */
  4, /* gather_load_x64_init_cost  */
  1 /* scatter_store_elt_cost  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost generic_armv8_a_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  1, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  3, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &generic_armv8_a_advsimd_vector_cost, /* advsimd  */
  &generic_armv8_a_sve_vector_cost, /* sve */
  nullptr /* issue_info  */
};

/* Generic costs for branch instructions.  */
static const struct cpu_branch_cost generic_armv8_a_branch_cost =
{
  1,  /* Predictable.  */
  3   /* Unpredictable.  */
};

/* Generic approximation modes.  */
static const cpu_approx_modes generic_armv8_a_approx_modes =
{
  AARCH64_APPROX_NONE,	/* division  */
  AARCH64_APPROX_NONE,	/* sqrt  */
  AARCH64_APPROX_NONE	/* recip_sqrt  */
};

/* Generic prefetch settings (which disable prefetch).  */
static const cpu_prefetch_tune generic_armv8_a_prefetch_tune =
{
  0,			/* num_slots  */
  -1,			/* l1_cache_size  */
  -1,			/* l1_cache_line_size  */
  -1,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static const struct tune_params generic_armv8_a_tunings =
{
  &cortexa76_extra_costs,
  &generic_armv8_a_addrcost_table,
  &generic_armv8_a_regmove_cost,
  &generic_armv8_a_vector_cost,
  &generic_armv8_a_branch_cost,
  &generic_armv8_a_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  { 4, /* load_int.  */
    2, /* store_int.  */
    5, /* load_fp.  */
    2, /* store_fp.  */
    4, /* load_pred.  */
    4 /* store_pred.  */
  }, /* memmov_cost.  */
  3, /* issue_rate  */
  AARCH64_FUSE_BASE, /* fusible_ops  */
  "32:16",	/* function_align.  */
  "4",		/* jump_align.  */
  "32:16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* fma_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_BASE
   | AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS
   | AARCH64_EXTRA_TUNE_USE_NEW_VECTOR_COSTS
   | AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT),	/* tune_flags.  */
  &generic_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_GENERIC_ARMV8_A.  */
