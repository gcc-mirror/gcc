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

#ifndef GCC_AARCH64_H_XGENE1
#define GCC_AARCH64_H_XGENE1

#include "generic.h"

static const struct cpu_addrcost_table xgene1_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  1, /* pre_modify  */
  1, /* post_modify  */
  1, /* post_modify_ld3_st3  */
  1, /* post_modify_ld4_st4  */
  0, /* register_offset  */
  1, /* register_sextend  */
  1, /* register_zextend  */
  0, /* imm_offset  */
};

static const struct cpu_regmove_cost xgene1_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  8, /* GP2FP  */
  8, /* FP2GP  */
  2 /* FP2FP  */
};

static const advsimd_vec_cost xgene1_advsimd_vector_cost =
{
  2, /* int_stmt_cost  */
  2, /* fp_stmt_cost  */
  0, /* ld2_st2_permute_cost  */
  0, /* ld3_st3_permute_cost  */
  0, /* ld4_st4_permute_cost  */
  2, /* permute_cost  */
  4, /* reduc_i8_cost  */
  4, /* reduc_i16_cost  */
  4, /* reduc_i32_cost  */
  4, /* reduc_i64_cost  */
  4, /* reduc_f16_cost  */
  4, /* reduc_f32_cost  */
  4, /* reduc_f64_cost  */
  4, /* store_elt_extra_cost  */
  4, /* vec_to_scalar_cost  */
  4, /* scalar_to_vec_cost  */
  10, /* align_load_cost  */
  10, /* unalign_load_cost  */
  2, /* unalign_store_cost  */
  2  /* store_cost  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost xgene1_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  5, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  2, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &xgene1_advsimd_vector_cost, /* advsimd  */
  nullptr, /* sve  */
  nullptr /* issue_info  */
};

/* Approximation modes for X-Gene 1.  */
static const cpu_approx_modes xgene1_approx_modes =
{
  AARCH64_APPROX_NONE,	/* division  */
  AARCH64_APPROX_NONE,	/* sqrt  */
  AARCH64_APPROX_ALL	/* recip_sqrt  */
};

static const cpu_prefetch_tune xgene1_prefetch_tune =
{
  8,			/* num_slots  */
  32,			/* l1_cache_size  */
  64,			/* l1_cache_line_size  */
  256,			/* l2_cache_size  */
  true,                 /* prefetch_dynamic_strides */
  -1,                   /* minimum_stride */
  -1			/* default_opt_level  */
};

static const struct tune_params xgene1_tunings =
{
  &xgene1_extra_costs,
  &xgene1_addrcost_table,
  &xgene1_regmove_cost,
  &xgene1_vector_cost,
  &generic_branch_cost,
  &xgene1_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
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
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &xgene1_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_XGENE1.  */
