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

#ifndef GCC_AARCH64_H_THUNDERX2T99
#define GCC_AARCH64_H_THUNDERX2T99

#include "generic.h"

static const struct cpu_addrcost_table thunderx2t99_addrcost_table =
{
    {
      1, /* hi  */
      1, /* si  */
      1, /* di  */
      2, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  0, /* post_modify_ld3_st3  */
  0, /* post_modify_ld4_st4  */
  2, /* register_offset  */
  3, /* register_sextend  */
  3, /* register_zextend  */
  0, /* imm_offset  */
};

static const struct cpu_regmove_cost thunderx2t99_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of int<->fp moves for spilling.  */
  5, /* GP2FP  */
  6, /* FP2GP  */
  3, /* FP2FP  */
};

static const advsimd_vec_cost thunderx2t99_advsimd_vector_cost =
{
  4, /* int_stmt_cost  */
  5, /* fp_stmt_cost  */
  0, /* ld2_st2_permute_cost  */
  0, /* ld3_st3_permute_cost  */
  0, /* ld4_st4_permute_cost  */
  10, /* permute_cost  */
  6, /* reduc_i8_cost  */
  6, /* reduc_i16_cost  */
  6, /* reduc_i32_cost  */
  6, /* reduc_i64_cost  */
  6, /* reduc_f16_cost  */
  6, /* reduc_f32_cost  */
  6, /* reduc_f64_cost  */
  6, /* store_elt_extra_cost  */
  6, /* vec_to_scalar_cost  */
  5, /* scalar_to_vec_cost  */
  4, /* align_load_cost  */
  4, /* unalign_load_cost  */
  1, /* unalign_store_cost  */
  1  /* store_cost  */
};

/* Costs for vector insn classes for Vulcan.  */
static const struct cpu_vector_cost thunderx2t99_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  6, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  2, /* cond_taken_branch_cost  */
  1,  /* cond_not_taken_branch_cost  */
  &thunderx2t99_advsimd_vector_cost, /* advsimd  */
  nullptr, /* sve  */
  nullptr /* issue_info  */
};

static const cpu_prefetch_tune thunderx2t99_prefetch_tune =
{
  8,			/* num_slots  */
  32,			/* l1_cache_size  */
  64,			/* l1_cache_line_size  */
  256,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static const struct tune_params thunderx2t99_tunings =
{
  &thunderx2t99_extra_costs,
  &thunderx2t99_addrcost_table,
  &thunderx2t99_regmove_cost,
  &thunderx2t99_vector_cost,
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
  4, /* issue_rate.  */
  (AARCH64_FUSE_BASE | AARCH64_FUSE_ALU_BRANCH
   | AARCH64_FUSE_ALU_CBZ), /* fusible_ops  */
  "16",	/* function_align.  */
  "8",	/* jump_align.  */
  "16",	/* loop_align.  */
  3,	/* int_reassoc_width.  */
  2,	/* fp_reassoc_width.  */
  1,	/* fma_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &thunderx2t99_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS    /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_THUNDERX2T99.  */
