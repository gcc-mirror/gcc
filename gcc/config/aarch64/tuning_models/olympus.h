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
#include "../aarch64-sched-dispatch.h"
#include "vec.h"

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

/* Olympus dispatch constraint types.  */
enum olympus_dispatch_constraint_type
{
  OLYMPUS_TOTAL_SLOTS, /* total slots  */
  OLYMPUS_M_PIPE,      /* m pipelines  */
  OLYMPUS_M0_PIPE,     /* m0 pipeline  */
  OLYMPUS_BRANCH_PIPE, /* branch pipelines  */
  OLYMPUS_L_SA_PIPE,   /* l, sa pipelines  */
  OLYMPUS_SA_PIPE,     /* sa pipelines  */
  OLYMPUS_V_PIPE,      /* v pipelines  */
  OLYMPUS_V0123_PIPE,  /* v0, v1, v2, v3 pipelines  */
  OLYMPUS_V03_PIPE,    /* v0, v3 pipelines  */
  OLYMPUS_V12_PIPE,    /* v1, v2 pipelines  */
  OLYMPUS_V45_PIPE,    /* v4, v5 pipelines  */
  OLYMPUS_V0_PIPE      /* v0 pipeline  */
};

/* Olympus dispatch constraints for instruction scheduling.
   The numbers are based on the Olympus CPU Core SWOG, section 4.1.  */
static const int olympus_dispatch_max_slots[] = {
  10, /* total slots  */
  6,  /* m pipelines  */
  3,  /* m0 pipeline  */
  3,  /* branch pipelines  */
  8,  /* l, sa pipelines  */
  4,  /* sa pipelines  */
  6,  /* v pipelines  */
  4,  /* v0, v1, v2, v3 pipelines  */
  4,  /* v0, v3 pipelines  */
  4,  /* v1, v2 pipelines  */
  2,  /* v4, v5 pipelines  */
  2   /* v0 pipeline  */
};

/* Olympus dispatch constraint callback function.
   Determines which constraints apply to an instruction and how many slots
   it requires.  Returns a vec of (constraint_index, slots_required) pairs.  */
static vec<std::pair<int, int>>
olympus_dispatch_constraint_callback (rtx_insn *insn)
{
  auto dispatch_group = get_attr_olympus_dispatch (insn);
  vec<std::pair<int, int>> constraints = vNULL;

  /* In addition to deducting slots from the specific pipeline types required
     by an instruction, we keep track of the total number of slots required.
     There are different cases how total_slots is derived from the specific
     pipeline slots:
     Case 1: Single top-level pipeline type used
       Example groups: OLYMPUS_DISPATCH_B, OLYMPUS_DISPATCH_V_V0123
       Total_slots is equal to the number of slots for the top-level
       pipeline type.
       Example: Assume an instruction in the OLYMPUS_DISPATCH_V_V0123
       dispatch group is executed as 2 MOps: 1 utilizing any V pipeline and
       1 utilizing a V0123 pipeline.  It requires 1 slot in the
       OLYMPUS_V0123_PIPE constraint and a total of 2 slots in the
       OLYMPUS_V_PIPE constraint, because the V0123 pipelines are a subset of
       the V pipelines. Total_slots is 2.
     Case 2: Multiple top-level pipeline types used
       Example groups: OLYMPUS_DISPATCH_M_V, OLYMPUS_DISPATCH_SA_V_V0123
       Total_slots is equal to the sum of the slots for the top-level
       pipeline types.  */
  int total_slots = 1;

  switch (dispatch_group)
    {
    case OLYMPUS_DISPATCH_NONE:
    case OLYMPUS_DISPATCH_I:
      break;

    case OLYMPUS_DISPATCH_B:
      constraints.safe_push ({OLYMPUS_BRANCH_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_M:
      constraints.safe_push ({OLYMPUS_M_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_M0:
      constraints.safe_push ({OLYMPUS_M_PIPE, 1});
      constraints.safe_push ({OLYMPUS_M0_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_L:
      {
	auto type_attr = get_attr_type (insn);
	int l_slots = 1;
	if (type_attr == TYPE_NEON_LDP_Q
	    || type_attr == TYPE_NEON_LOAD1_2REG_Q
	    || type_attr == TYPE_NEON_LOAD1_3REG
	    || type_attr == TYPE_NEON_LOAD1_4REG)
	  l_slots = 2;
	else if (type_attr == TYPE_NEON_LOAD1_3REG_Q)
	  l_slots = 3;
	else if (type_attr == TYPE_NEON_LOAD1_4REG_Q)
	  l_slots = 4;
	constraints.safe_push ({OLYMPUS_L_SA_PIPE, l_slots});
	total_slots = l_slots;
      }
      break;

    case OLYMPUS_DISPATCH_V:
      {
	auto type_attr = get_attr_type (insn);
	int v_slots = 1;
	if (type_attr == TYPE_NEON_TBL3
	    || type_attr == TYPE_NEON_FP_REDUC_MINMAX_D
	    || type_attr == TYPE_NEON_FP_REDUC_MINMAX_S
	    || get_attr_sve_type (insn) == SVE_TYPE_SVE_FP_REDUC)
	  v_slots = 2;
	else if (type_attr == TYPE_NEON_TBL4
		 || type_attr == TYPE_NEON_FP_REDUC_MINMAX_D_Q
		 || type_attr == TYPE_NEON_FP_REDUC_MINMAX_S_Q)
	  v_slots = 3;
	constraints.safe_push ({OLYMPUS_V_PIPE, v_slots});
	total_slots = v_slots;
      }
      break;

    case OLYMPUS_DISPATCH_V0:
      constraints.safe_push ({OLYMPUS_V_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V03_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V0_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_V03:
      constraints.safe_push ({OLYMPUS_V_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V03_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_V12:
      {
	auto sve_type_attr = get_attr_sve_type (insn);
	int slots = (sve_type_attr == SVE_TYPE_SVE_INT_BIT_PERM) ? 2 : 1;
	constraints.safe_push ({OLYMPUS_V_PIPE, slots});
	constraints.safe_push ({OLYMPUS_V0123_PIPE, slots});
	constraints.safe_push ({OLYMPUS_V12_PIPE, slots});
	total_slots = slots;
      }
      break;

    case OLYMPUS_DISPATCH_V45:
      constraints.safe_push ({OLYMPUS_V_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V45_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_V0123:
      constraints.safe_push ({OLYMPUS_V_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_M_V:
      constraints.safe_push ({OLYMPUS_M_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V_PIPE, 1});
      total_slots = 2;
      break;

    case OLYMPUS_DISPATCH_L_V:
      {
	auto type_attr = get_attr_type (insn);
	auto sve_type_attr = get_attr_sve_type (insn);
	int l_sa_slots = 1;
	int v_slots = 1;
	if (type_attr == TYPE_NEON_LOAD2_2REG
	    || type_attr == TYPE_NEON_LOAD2_ALL_LANES
	    || type_attr == TYPE_NEON_LOAD2_ALL_LANES_Q
	    || type_attr == TYPE_NEON_LOAD2_ONE_LANE)
	  v_slots = 2;
	else if (type_attr == TYPE_NEON_LOAD2_2REG_Q
		 || sve_type_attr == SVE_TYPE_SVE_LOAD_2REG)
	  {
	    l_sa_slots = 2;
	    v_slots = 2;
	  }
	else if (type_attr == TYPE_NEON_LOAD3_3REG
		 || type_attr == TYPE_NEON_LOAD3_ALL_LANES
		 || type_attr == TYPE_NEON_LOAD3_ONE_LANE
		 || sve_type_attr == SVE_TYPE_SVE_LOAD_3REG)
	  {
	    l_sa_slots = 2;
	    v_slots = 3;
	  }
	else if (type_attr == TYPE_NEON_LOAD3_3REG_Q)
	  {
	    l_sa_slots = 3;
	    v_slots = 3;
	  }
	else if (type_attr == TYPE_NEON_LOAD4_4REG
		 || type_attr == TYPE_NEON_LOAD4_ALL_LANES
		 || type_attr == TYPE_NEON_LOAD4_ONE_LANE)
	  {
	    l_sa_slots = 2;
	    v_slots = 4;
	  }
	else if (type_attr == TYPE_NEON_LOAD4_4REG_Q
		 || sve_type_attr == SVE_TYPE_SVE_LOAD_4REG)
	  {
	    l_sa_slots = 4;
	    v_slots = 6;
	  }
	constraints.safe_push ({OLYMPUS_L_SA_PIPE, l_sa_slots});
	constraints.safe_push ({OLYMPUS_V_PIPE, v_slots});
	total_slots = l_sa_slots + v_slots;
      }
      break;

    case OLYMPUS_DISPATCH_M_L:
      constraints.safe_push ({OLYMPUS_M_PIPE, 1});
      constraints.safe_push ({OLYMPUS_L_SA_PIPE, 1});
      total_slots = 2;
      break;

    case OLYMPUS_DISPATCH_M_V0123:
      constraints.safe_push ({OLYMPUS_M_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V_PIPE, 1});
      constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
      total_slots = 2;
      break;

    case OLYMPUS_DISPATCH_V_V0123:
      constraints.safe_push ({OLYMPUS_V_PIPE, 2});
      constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
      total_slots = 2;
      break;

    case OLYMPUS_DISPATCH_L_V03:
      {
	auto sve_type_attr = get_attr_sve_type (insn);
	int l_slots = 1;
	if (sve_type_attr == SVE_TYPE_SVE_GATHERLOAD_32)
	  l_slots = 4;
	else if (sve_type_attr == SVE_TYPE_SVE_GATHERLOAD_64)
	  l_slots = 2;
	constraints.safe_push ({OLYMPUS_L_SA_PIPE, l_slots});
	constraints.safe_push ({OLYMPUS_V_PIPE, 1});
	constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
	constraints.safe_push ({OLYMPUS_V03_PIPE, 1});
	total_slots = l_slots + 1;
      }
      break;

    case OLYMPUS_DISPATCH_SA_D:
      constraints.safe_push ({OLYMPUS_SA_PIPE, 1});
      break;

    case OLYMPUS_DISPATCH_SA_V0123:
      {
	/* According to the note in section 4.1 of the SWOG, MOps using the
	   V0123 pipeline do not count towards the limits, when those MOps
	   are in the same instruction as a MOp in the SA pipeline.  That is
	   why total_slots is set to the number of slots for the SA pipelines,
	   disregarding the slots for the V0123 pipelines.  */
	auto type_attr = get_attr_type (insn);
	if (type_attr == TYPE_NEON_STORE1_3REG
	    || type_attr == TYPE_NEON_STORE1_3REG_Q
	    || type_attr == TYPE_NEON_STORE1_4REG
	    || type_attr == TYPE_NEON_STORE1_4REG_Q)
	  {
	    constraints.safe_push ({OLYMPUS_SA_PIPE, 2});
	    constraints.safe_push ({OLYMPUS_V_PIPE, 2});
	    constraints.safe_push ({OLYMPUS_V0123_PIPE, 2});
	    total_slots = 2;
	  }
	else
	  {
	    constraints.safe_push ({OLYMPUS_SA_PIPE, 1});
	    constraints.safe_push ({OLYMPUS_V_PIPE, 1});
	    constraints.safe_push ({OLYMPUS_V0123_PIPE, 1});
	    total_slots = 1;
	  }
      }
      break;

    case OLYMPUS_DISPATCH_SA_V_V0123:
      {
	auto type_attr = get_attr_type (insn);
	auto sve_type_attr = get_attr_sve_type (insn);
	int sa_slots = 1;
	int v_slots = 2;
	int v0123_slots = 1;
	if (type_attr == TYPE_NEON_STORE2_2REG_Q
	    || type_attr == TYPE_NEON_STORE4_ONE_LANE
	    || type_attr == TYPE_NEON_STORE4_ONE_LANE_Q)
	  v_slots = 3;
	else if (type_attr == TYPE_NEON_STORE3_3REG
		 || type_attr == TYPE_NEON_STORE3_ONE_LANE
		 || type_attr == TYPE_NEON_STORE3_ONE_LANE_Q
		 || sve_type_attr == SVE_TYPE_SVE_STORE_2REG)
	  {
	    sa_slots = 2;
	    v_slots = 4;
	    v0123_slots = 2;
	  }
	else if (type_attr == TYPE_NEON_STORE3_3REG_Q)
	  {
	    sa_slots = 2;
	    v_slots = 5;
	    v0123_slots = 2;
	  }
	else if (type_attr == TYPE_NEON_STORE4_4REG)
	  v_slots = 5;
	else if (type_attr == TYPE_NEON_STORE4_4REG_Q)
	  {
	    sa_slots = 2;
	    v_slots = 6;
	    v0123_slots = 2;
	  }
	else if (sve_type_attr == SVE_TYPE_SVE_STORE_3REG)
	  {
	    sa_slots = 3;
	    v_slots = 6;
	    v0123_slots = 3;
	  }
	else if (sve_type_attr == SVE_TYPE_SVE_STORE_4REG)
	  {
	    sa_slots = 4;
	    v_slots = 6;
	    v0123_slots = 4;
	  }
	else if (sve_type_attr == SVE_TYPE_SVE_SCATTERSTORE_32)
	  {
	    sa_slots = 4;
	    v_slots = 5;
	    v0123_slots = 4;
	  }
	else if (sve_type_attr == SVE_TYPE_SVE_SCATTERSTORE_64)
	  {
	    sa_slots = 2;
	    v_slots = 4;
	    v0123_slots = 3;
	  }
	constraints.safe_push ({OLYMPUS_SA_PIPE, sa_slots});
	constraints.safe_push ({OLYMPUS_V_PIPE, v_slots});
	constraints.safe_push ({OLYMPUS_V0123_PIPE, v0123_slots});
	/* We disregard slots from the V0123 pipelines in total_slots when
	   the instruction also uses the SA pipelines, see comment in
	   OLYMPUS_DISPATCH_SA_V0123.  */
	total_slots = sa_slots + (v_slots - v0123_slots);
      }
      break;
    }

  /* Add total slots constraint  */
  constraints.safe_push ({OLYMPUS_TOTAL_SLOTS, total_slots});

  return constraints;
}

/* Olympus dispatch constraints configuration. */
static const struct dispatch_constraint_info olympus_dispatch_constraint_info = {
  olympus_dispatch_max_slots,  /* max_slots */
  ARRAY_SIZE (olympus_dispatch_max_slots),  /* num_constraints */
  olympus_dispatch_constraint_callback  /* callback */
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
   | AARCH64_EXTRA_TUNE_AVOID_PRED_RMW
   | AARCH64_EXTRA_TUNE_DISPATCH_SCHED),	/* tune_flags.  */
  &olympus_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* stp_policy_model.  */
  &olympus_dispatch_constraint_info	/* dispatch_constraints.  */
};

#endif /* GCC_AARCH64_H_OLYMPUS.  */
