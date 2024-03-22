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

#ifndef GCC_AARCH64_H_AMPERE1
#define GCC_AARCH64_H_AMPERE1

#include "generic.h"

static const advsimd_vec_cost ampere1_advsimd_vector_cost =
{
  1, /* int_stmt_cost  */
  3, /* fp_stmt_cost  */
  0, /* ld2_st2_permute_cost  */
  0, /* ld3_st3_permute_cost  */
  0, /* ld4_st4_permute_cost  */
  2, /* permute_cost  */
  12, /* reduc_i8_cost  */
  9, /* reduc_i16_cost  */
  6, /* reduc_i32_cost  */
  5, /* reduc_i64_cost  */
  9, /* reduc_f16_cost  */
  6, /* reduc_f32_cost  */
  5, /* reduc_f64_cost  */
  8, /* store_elt_extra_cost  */
  6, /* vec_to_scalar_cost  */
  7, /* scalar_to_vec_cost  */
  4, /* align_load_cost  */
  4, /* unalign_load_cost  */
  1, /* unalign_store_cost  */
  1  /* store_cost  */
};

/* Ampere-1 costs for vector insn classes.  */
static const struct cpu_vector_cost ampere1_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  3, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &ampere1_advsimd_vector_cost, /* advsimd  */
  nullptr, /* sve  */
  nullptr  /* issue_info  */
};

static const cpu_prefetch_tune ampere1_prefetch_tune =
{
  0,			/* num_slots  */
  64,			/* l1_cache_size  */
  64,			/* l1_cache_line_size  */
  2048,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static const struct tune_params ampere1_tunings =
{
  &ampere1_extra_costs,
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
  (AARCH64_FUSE_ADRP_ADD | AARCH64_FUSE_AES_AESMC |
   AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_MOVK_MOVK |
   AARCH64_FUSE_ALU_BRANCH /* adds, ands, bics, ccmp, ccmn */ |
   AARCH64_FUSE_CMP_BRANCH),
  /* fusible_ops  */
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

#endif /* GCC_AARCH64_H_AMPERE1.  */
