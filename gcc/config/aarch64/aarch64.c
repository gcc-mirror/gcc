/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#define IN_TARGET_CODE 1

#include "config.h"
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "insn-attr.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "output.h"
#include "flags.h"
#include "explow.h"
#include "expr.h"
#include "reload.h"
#include "langhooks.h"
#include "opts.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "gimple-iterator.h"
#include "tree-vectorizer.h"
#include "aarch64-cost-tables.h"
#include "dumpfile.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tm-constrs.h"
#include "sched-int.h"
#include "target-globals.h"
#include "common/common-target.h"
#include "cfgrtl.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "rtx-vector-builder.h"
#include "intl.h"
#include "expmed.h"
#include "function-abi.h"

/* This file should be included last.  */
#include "target-def.h"

/* Defined for convenience.  */
#define POINTER_BYTES (POINTER_SIZE / BITS_PER_UNIT)

/* Information about a legitimate vector immediate operand.  */
struct simd_immediate_info
{
  enum insn_type { MOV, MVN, INDEX, PTRUE };
  enum modifier_type { LSL, MSL };

  simd_immediate_info () {}
  simd_immediate_info (scalar_float_mode, rtx);
  simd_immediate_info (scalar_int_mode, unsigned HOST_WIDE_INT,
		       insn_type = MOV, modifier_type = LSL,
		       unsigned int = 0);
  simd_immediate_info (scalar_mode, rtx, rtx);
  simd_immediate_info (scalar_int_mode, aarch64_svpattern);

  /* The mode of the elements.  */
  scalar_mode elt_mode;

  /* The instruction to use to move the immediate into a vector.  */
  insn_type insn;

  union
  {
    /* For MOV and MVN.  */
    struct
    {
      /* The value of each element.  */
      rtx value;

      /* The kind of shift modifier to use, and the number of bits to shift.
	 This is (LSL, 0) if no shift is needed.  */
      modifier_type modifier;
      unsigned int shift;
    } mov;

    /* For INDEX.  */
    struct
    {
      /* The value of the first element and the step to be added for each
	 subsequent element.  */
      rtx base, step;
    } index;

    /* For PTRUE.  */
    aarch64_svpattern pattern;
  } u;
};

/* Construct a floating-point immediate in which each element has mode
   ELT_MODE_IN and value VALUE_IN.  */
inline simd_immediate_info
::simd_immediate_info (scalar_float_mode elt_mode_in, rtx value_in)
  : elt_mode (elt_mode_in), insn (MOV)
{
  u.mov.value = value_in;
  u.mov.modifier = LSL;
  u.mov.shift = 0;
}

/* Construct an integer immediate in which each element has mode ELT_MODE_IN
   and value VALUE_IN.  The other parameters are as for the structure
   fields.  */
inline simd_immediate_info
::simd_immediate_info (scalar_int_mode elt_mode_in,
		       unsigned HOST_WIDE_INT value_in,
		       insn_type insn_in, modifier_type modifier_in,
		       unsigned int shift_in)
  : elt_mode (elt_mode_in), insn (insn_in)
{
  u.mov.value = gen_int_mode (value_in, elt_mode_in);
  u.mov.modifier = modifier_in;
  u.mov.shift = shift_in;
}

/* Construct an integer immediate in which each element has mode ELT_MODE_IN
   and where element I is equal to BASE_IN + I * STEP_IN.  */
inline simd_immediate_info
::simd_immediate_info (scalar_mode elt_mode_in, rtx base_in, rtx step_in)
  : elt_mode (elt_mode_in), insn (INDEX)
{
  u.index.base = base_in;
  u.index.step = step_in;
}

/* Construct a predicate that controls elements of mode ELT_MODE_IN
   and has PTRUE pattern PATTERN_IN.  */
inline simd_immediate_info
::simd_immediate_info (scalar_int_mode elt_mode_in,
		       aarch64_svpattern pattern_in)
  : elt_mode (elt_mode_in), insn (PTRUE)
{
  u.pattern = pattern_in;
}

/* The current code model.  */
enum aarch64_code_model aarch64_cmodel;

/* The number of 64-bit elements in an SVE vector.  */
poly_uint16 aarch64_sve_vg;

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS 1
#endif

static bool aarch64_composite_type_p (const_tree, machine_mode);
static bool aarch64_vfp_is_call_or_return_candidate (machine_mode,
						     const_tree,
						     machine_mode *, int *,
						     bool *);
static void aarch64_elf_asm_constructor (rtx, int) ATTRIBUTE_UNUSED;
static void aarch64_elf_asm_destructor (rtx, int) ATTRIBUTE_UNUSED;
static void aarch64_override_options_after_change (void);
static bool aarch64_vector_mode_supported_p (machine_mode);
static int aarch64_address_cost (rtx, machine_mode, addr_space_t, bool);
static bool aarch64_builtin_support_vector_misalignment (machine_mode mode,
							 const_tree type,
							 int misalignment,
							 bool is_packed);
static machine_mode aarch64_simd_container_mode (scalar_mode, poly_int64);
static bool aarch64_print_address_internal (FILE*, machine_mode, rtx,
					    aarch64_addr_query_type);
static HOST_WIDE_INT aarch64_clamp_to_uimm12_shift (HOST_WIDE_INT val);

/* Major revision number of the ARM Architecture implemented by the target.  */
unsigned aarch64_architecture_version;

/* The processor for which instructions should be scheduled.  */
enum aarch64_processor aarch64_tune = cortexa53;

/* Mask to specify which instruction scheduling options should be used.  */
uint64_t aarch64_tune_flags = 0;

/* Global flag for PC relative loads.  */
bool aarch64_pcrelative_literal_loads;

/* Global flag for whether frame pointer is enabled.  */
bool aarch64_use_frame_pointer;

#define BRANCH_PROTECT_STR_MAX 255
char *accepted_branch_protection_string = NULL;

static enum aarch64_parse_opt_result
aarch64_parse_branch_protection (const char*, char**);

/* Support for command line parsing of boolean flags in the tuning
   structures.  */
struct aarch64_flag_desc
{
  const char* name;
  unsigned int flag;
};

#define AARCH64_FUSION_PAIR(name, internal_name) \
  { name, AARCH64_FUSE_##internal_name },
static const struct aarch64_flag_desc aarch64_fusible_pairs[] =
{
  { "none", AARCH64_FUSE_NOTHING },
#include "aarch64-fusion-pairs.def"
  { "all", AARCH64_FUSE_ALL },
  { NULL, AARCH64_FUSE_NOTHING }
};

#define AARCH64_EXTRA_TUNING_OPTION(name, internal_name) \
  { name, AARCH64_EXTRA_TUNE_##internal_name },
static const struct aarch64_flag_desc aarch64_tuning_flags[] =
{
  { "none", AARCH64_EXTRA_TUNE_NONE },
#include "aarch64-tuning-flags.def"
  { "all", AARCH64_EXTRA_TUNE_ALL },
  { NULL, AARCH64_EXTRA_TUNE_NONE }
};

/* Tuning parameters.  */

static const struct cpu_addrcost_table generic_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  0, /* register_offset  */
  0, /* register_sextend  */
  0, /* register_zextend  */
  0 /* imm_offset  */
};

static const struct cpu_addrcost_table exynosm1_addrcost_table =
{
    {
      0, /* hi  */
      0, /* si  */
      0, /* di  */
      2, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  1, /* register_offset  */
  1, /* register_sextend  */
  2, /* register_zextend  */
  0, /* imm_offset  */
};

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
  0, /* register_offset  */
  1, /* register_sextend  */
  1, /* register_zextend  */
  0, /* imm_offset  */
};

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
  2, /* register_offset  */
  3, /* register_sextend  */
  3, /* register_zextend  */
  0, /* imm_offset  */
};

static const struct cpu_addrcost_table tsv110_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  0, /* register_offset  */
  1, /* register_sextend  */
  1, /* register_zextend  */
  0, /* imm_offset  */
};

static const struct cpu_addrcost_table qdf24xx_addrcost_table =
{
    {
      1, /* hi  */
      1, /* si  */
      1, /* di  */
      2, /* ti  */
    },
  1, /* pre_modify  */
  1, /* post_modify  */
  3, /* register_offset  */
  3, /* register_sextend  */
  3, /* register_zextend  */
  2, /* imm_offset  */
};

static const struct cpu_regmove_cost generic_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const struct cpu_regmove_cost cortexa57_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const struct cpu_regmove_cost cortexa53_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const struct cpu_regmove_cost exynosm1_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost (actual, 4 and 9).  */
  9, /* GP2FP  */
  9, /* FP2GP  */
  1 /* FP2FP  */
};

static const struct cpu_regmove_cost thunderx_regmove_cost =
{
  2, /* GP2GP  */
  2, /* GP2FP  */
  6, /* FP2GP  */
  4 /* FP2FP  */
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

static const struct cpu_regmove_cost qdf24xx_regmove_cost =
{
  2, /* GP2GP  */
  /* Avoid the use of int<->fp moves for spilling.  */
  6, /* GP2FP  */
  6, /* FP2GP  */
  4 /* FP2FP  */
};

static const struct cpu_regmove_cost thunderx2t99_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of int<->fp moves for spilling.  */
  8, /* GP2FP  */
  8, /* FP2GP  */
  4  /* FP2FP  */
};

static const struct cpu_regmove_cost tsv110_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  2, /* GP2FP  */
  3, /* FP2GP  */
  2  /* FP2FP  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost generic_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  1, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* vec_int_stmt_cost  */
  1, /* vec_fp_stmt_cost  */
  2, /* vec_permute_cost  */
  2, /* vec_to_scalar_cost  */
  1, /* scalar_to_vec_cost  */
  1, /* vec_align_load_cost  */
  1, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  3, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* QDF24XX costs for vector insn classes.  */
static const struct cpu_vector_cost qdf24xx_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  1, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* vec_int_stmt_cost  */
  3, /* vec_fp_stmt_cost  */
  2, /* vec_permute_cost  */
  1, /* vec_to_scalar_cost  */
  1, /* scalar_to_vec_cost  */
  1, /* vec_align_load_cost  */
  1, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  3, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* ThunderX costs for vector insn classes.  */
static const struct cpu_vector_cost thunderx_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  3, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  4, /* vec_int_stmt_cost  */
  1, /* vec_fp_stmt_cost  */
  4, /* vec_permute_cost  */
  2, /* vec_to_scalar_cost  */
  2, /* scalar_to_vec_cost  */
  3, /* vec_align_load_cost  */
  5, /* vec_unalign_load_cost  */
  5, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  3, /* cond_taken_branch_cost  */
  3 /* cond_not_taken_branch_cost  */
};

static const struct cpu_vector_cost tsv110_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  5, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  2, /* vec_int_stmt_cost  */
  2, /* vec_fp_stmt_cost  */
  2, /* vec_permute_cost  */
  3, /* vec_to_scalar_cost  */
  2, /* scalar_to_vec_cost  */
  5, /* vec_align_load_cost  */
  5, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  1, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost cortexa57_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  2, /* vec_int_stmt_cost  */
  2, /* vec_fp_stmt_cost  */
  3, /* vec_permute_cost  */
  8, /* vec_to_scalar_cost  */
  8, /* scalar_to_vec_cost  */
  4, /* vec_align_load_cost  */
  4, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  1, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

static const struct cpu_vector_cost exynosm1_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  5, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  3, /* vec_int_stmt_cost  */
  3, /* vec_fp_stmt_cost  */
  3, /* vec_permute_cost  */
  3, /* vec_to_scalar_cost  */
  3, /* scalar_to_vec_cost  */
  5, /* vec_align_load_cost  */
  5, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  1, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost xgene1_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  1, /* scalar_fp_stmt_cost  */
  5, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  2, /* vec_int_stmt_cost  */
  2, /* vec_fp_stmt_cost  */
  2, /* vec_permute_cost  */
  4, /* vec_to_scalar_cost  */
  4, /* scalar_to_vec_cost  */
  10, /* vec_align_load_cost  */
  10, /* vec_unalign_load_cost  */
  2, /* vec_unalign_store_cost  */
  2, /* vec_store_cost  */
  2, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* Costs for vector insn classes for Vulcan.  */
static const struct cpu_vector_cost thunderx2t99_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  6, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  5, /* vec_int_stmt_cost  */
  6, /* vec_fp_stmt_cost  */
  10, /* vec_permute_cost  */
  6, /* vec_to_scalar_cost  */
  5, /* scalar_to_vec_cost  */
  8, /* vec_align_load_cost  */
  8, /* vec_unalign_load_cost  */
  4, /* vec_unalign_store_cost  */
  4, /* vec_store_cost  */
  2, /* cond_taken_branch_cost  */
  1  /* cond_not_taken_branch_cost  */
};

/* Generic costs for branch instructions.  */
static const struct cpu_branch_cost generic_branch_cost =
{
  1,  /* Predictable.  */
  3   /* Unpredictable.  */
};

/* Generic approximation modes.  */
static const cpu_approx_modes generic_approx_modes =
{
  AARCH64_APPROX_NONE,	/* division  */
  AARCH64_APPROX_NONE,	/* sqrt  */
  AARCH64_APPROX_NONE	/* recip_sqrt  */
};

/* Approximation modes for Exynos M1.  */
static const cpu_approx_modes exynosm1_approx_modes =
{
  AARCH64_APPROX_NONE,	/* division  */
  AARCH64_APPROX_ALL,	/* sqrt  */
  AARCH64_APPROX_ALL	/* recip_sqrt  */
};

/* Approximation modes for X-Gene 1.  */
static const cpu_approx_modes xgene1_approx_modes =
{
  AARCH64_APPROX_NONE,	/* division  */
  AARCH64_APPROX_NONE,	/* sqrt  */
  AARCH64_APPROX_ALL	/* recip_sqrt  */
};

/* Generic prefetch settings (which disable prefetch).  */
static const cpu_prefetch_tune generic_prefetch_tune =
{
  0,			/* num_slots  */
  -1,			/* l1_cache_size  */
  -1,			/* l1_cache_line_size  */
  -1,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static const cpu_prefetch_tune exynosm1_prefetch_tune =
{
  0,			/* num_slots  */
  -1,			/* l1_cache_size  */
  64,			/* l1_cache_line_size  */
  -1,			/* l2_cache_size  */
  true,			/* prefetch_dynamic_strides */
  -1,			/* minimum_stride */
  -1			/* default_opt_level  */
};

static const cpu_prefetch_tune qdf24xx_prefetch_tune =
{
  4,			/* num_slots  */
  32,			/* l1_cache_size  */
  64,			/* l1_cache_line_size  */
  512,			/* l2_cache_size  */
  false,		/* prefetch_dynamic_strides */
  2048,			/* minimum_stride */
  3			/* default_opt_level  */
};

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

static const cpu_prefetch_tune tsv110_prefetch_tune =
{
  0,                    /* num_slots  */
  64,                   /* l1_cache_size  */
  64,                   /* l1_cache_line_size  */
  512,                  /* l2_cache_size  */
  true,                 /* prefetch_dynamic_strides */
  -1,                   /* minimum_stride */
  -1                    /* default_opt_level  */
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

static const struct tune_params generic_tunings =
{
  &cortexa57_extra_costs,
  &generic_addrcost_table,
  &generic_regmove_cost,
  &generic_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  2, /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC), /* fusible_ops  */
  "16:12",	/* function_align.  */
  "4",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &generic_prefetch_tune
};

static const struct tune_params cortexa35_tunings =
{
  &cortexa53_extra_costs,
  &generic_addrcost_table,
  &cortexa53_regmove_cost,
  &generic_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  1, /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK | AARCH64_FUSE_ADRP_LDR), /* fusible_ops  */
  "16",	/* function_align.  */
  "4",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &generic_prefetch_tune
};

static const struct tune_params cortexa53_tunings =
{
  &cortexa53_extra_costs,
  &generic_addrcost_table,
  &cortexa53_regmove_cost,
  &generic_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  2, /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK | AARCH64_FUSE_ADRP_LDR), /* fusible_ops  */
  "16",	/* function_align.  */
  "4",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &generic_prefetch_tune
};

static const struct tune_params cortexa57_tunings =
{
  &cortexa57_extra_costs,
  &generic_addrcost_table,
  &cortexa57_regmove_cost,
  &cortexa57_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  3, /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK), /* fusible_ops  */
  "16",	/* function_align.  */
  "4",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_RENAME_FMA_REGS),	/* tune_flags.  */
  &generic_prefetch_tune
};

static const struct tune_params cortexa72_tunings =
{
  &cortexa57_extra_costs,
  &generic_addrcost_table,
  &cortexa57_regmove_cost,
  &cortexa57_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  3, /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK), /* fusible_ops  */
  "16",	/* function_align.  */
  "4",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &generic_prefetch_tune
};

static const struct tune_params cortexa73_tunings =
{
  &cortexa57_extra_costs,
  &generic_addrcost_table,
  &cortexa57_regmove_cost,
  &cortexa57_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost.  */
  2, /* issue_rate.  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK | AARCH64_FUSE_ADRP_LDR), /* fusible_ops  */
  "16",	/* function_align.  */
  "4",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &generic_prefetch_tune
};



static const struct tune_params exynosm1_tunings =
{
  &exynosm1_extra_costs,
  &exynosm1_addrcost_table,
  &exynosm1_regmove_cost,
  &exynosm1_vector_cost,
  &generic_branch_cost,
  &exynosm1_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4,	/* memmov_cost  */
  3,	/* issue_rate  */
  (AARCH64_FUSE_AES_AESMC), /* fusible_ops  */
  "4",	/* function_align.  */
  "4",	/* jump_align.  */
  "4",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  48,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK, /* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE), /* tune_flags.  */
  &exynosm1_prefetch_tune
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
  6, /* memmov_cost  */
  2, /* issue_rate  */
  AARCH64_FUSE_ALU_BRANCH, /* fusible_ops  */
  "8",	/* function_align.  */
  "8",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_OFF,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_SLOW_UNALIGNED_LDPW),	/* tune_flags.  */
  &thunderxt88_prefetch_tune
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
  6, /* memmov_cost  */
  2, /* issue_rate  */
  AARCH64_FUSE_ALU_BRANCH, /* fusible_ops  */
  "8",	/* function_align.  */
  "8",	/* jump_align.  */
  "8",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_OFF,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_SLOW_UNALIGNED_LDPW
   | AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND),	/* tune_flags.  */
  &thunderx_prefetch_tune
};

static const struct tune_params tsv110_tunings =
{
  &tsv110_extra_costs,
  &tsv110_addrcost_table,
  &tsv110_regmove_cost,
  &tsv110_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4,    /* memmov_cost  */
  4,    /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_ALU_BRANCH
   | AARCH64_FUSE_ALU_CBZ), /* fusible_ops  */
  "16", /* function_align.  */
  "4",  /* jump_align.  */
  "8",  /* loop_align.  */
  2,    /* int_reassoc_width.  */
  4,    /* fp_reassoc_width.  */
  1,    /* vec_reassoc_width.  */
  2,    /* min_div_recip_mul_sf.  */
  2,    /* min_div_recip_mul_df.  */
  0,    /* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,     /* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),     /* tune_flags.  */
  &tsv110_prefetch_tune
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
  6, /* memmov_cost  */
  4, /* issue_rate  */
  AARCH64_FUSE_NOTHING, /* fusible_ops  */
  "16",	/* function_align.  */
  "16",	/* jump_align.  */
  "16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  17,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_OFF,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NO_LDP_STP_QREGS),	/* tune_flags.  */
  &xgene1_prefetch_tune
};

static const struct tune_params emag_tunings =
{
  &xgene1_extra_costs,
  &xgene1_addrcost_table,
  &xgene1_regmove_cost,
  &xgene1_vector_cost,
  &generic_branch_cost,
  &xgene1_approx_modes,
  SVE_NOT_IMPLEMENTED,
  6, /* memmov_cost  */
  4, /* issue_rate  */
  AARCH64_FUSE_NOTHING, /* fusible_ops  */
  "16",	/* function_align.  */
  "16",	/* jump_align.  */
  "16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  17,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_OFF,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NO_LDP_STP_QREGS),	/* tune_flags.  */
  &xgene1_prefetch_tune
};

static const struct tune_params qdf24xx_tunings =
{
  &qdf24xx_extra_costs,
  &qdf24xx_addrcost_table,
  &qdf24xx_regmove_cost,
  &qdf24xx_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  4, /* issue_rate  */
  (AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK), /* fuseable_ops  */
  "16",	/* function_align.  */
  "8",	/* jump_align.  */
  "16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  AARCH64_EXTRA_TUNE_RENAME_LOAD_REGS, /* tune_flags.  */
  &qdf24xx_prefetch_tune
};

/* Tuning structure for the Qualcomm Saphira core.  Default to falkor values
   for now.  */
static const struct tune_params saphira_tunings =
{
  &generic_extra_costs,
  &generic_addrcost_table,
  &generic_regmove_cost,
  &generic_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  4, /* issue_rate  */
  (AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK), /* fuseable_ops  */
  "16",	/* function_align.  */
  "8",	/* jump_align.  */
  "16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),		/* tune_flags.  */
  &generic_prefetch_tune
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
  4, /* memmov_cost.  */
  4, /* issue_rate.  */
  (AARCH64_FUSE_ALU_BRANCH | AARCH64_FUSE_AES_AESMC
   | AARCH64_FUSE_ALU_CBZ), /* fusible_ops  */
  "16",	/* function_align.  */
  "8",	/* jump_align.  */
  "16",	/* loop_align.  */
  3,	/* int_reassoc_width.  */
  2,	/* fp_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &thunderx2t99_prefetch_tune
};

static const struct tune_params neoversen1_tunings =
{
  &cortexa57_extra_costs,
  &generic_addrcost_table,
  &generic_regmove_cost,
  &cortexa57_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_NOT_IMPLEMENTED, /* sve_width  */
  4, /* memmov_cost  */
  3, /* issue_rate  */
  AARCH64_FUSE_AES_AESMC, /* fusible_ops  */
  "32:16",	/* function_align.  */
  "32:16",	/* jump_align.  */
  "32:16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_NONE),	/* tune_flags.  */
  &generic_prefetch_tune
};

/* Support for fine-grained override of the tuning structures.  */
struct aarch64_tuning_override_function
{
  const char* name;
  void (*parse_override)(const char*, struct tune_params*);
};

static void aarch64_parse_fuse_string (const char*, struct tune_params*);
static void aarch64_parse_tune_string (const char*, struct tune_params*);
static void aarch64_parse_sve_width_string (const char*, struct tune_params*);

static const struct aarch64_tuning_override_function
aarch64_tuning_override_functions[] =
{
  { "fuse", aarch64_parse_fuse_string },
  { "tune", aarch64_parse_tune_string },
  { "sve_width", aarch64_parse_sve_width_string },
  { NULL, NULL }
};

/* A processor implementing AArch64.  */
struct processor
{
  const char *const name;
  enum aarch64_processor ident;
  enum aarch64_processor sched_core;
  enum aarch64_arch arch;
  unsigned architecture_version;
  const uint64_t flags;
  const struct tune_params *const tune;
};

/* Architectures implementing AArch64.  */
static const struct processor all_architectures[] =
{
#define AARCH64_ARCH(NAME, CORE, ARCH_IDENT, ARCH_REV, FLAGS) \
  {NAME, CORE, CORE, AARCH64_ARCH_##ARCH_IDENT, ARCH_REV, FLAGS, NULL},
#include "aarch64-arches.def"
  {NULL, aarch64_none, aarch64_none, aarch64_no_arch, 0, 0, NULL}
};

/* Processor cores implementing AArch64.  */
static const struct processor all_cores[] =
{
#define AARCH64_CORE(NAME, IDENT, SCHED, ARCH, FLAGS, COSTS, IMP, PART, VARIANT) \
  {NAME, IDENT, SCHED, AARCH64_ARCH_##ARCH,				\
  all_architectures[AARCH64_ARCH_##ARCH].architecture_version,	\
  FLAGS, &COSTS##_tunings},
#include "aarch64-cores.def"
  {"generic", generic, cortexa53, AARCH64_ARCH_8A, 8,
    AARCH64_FL_FOR_ARCH8, &generic_tunings},
  {NULL, aarch64_none, aarch64_none, aarch64_no_arch, 0, 0, NULL}
};


/* Target specification.  These are populated by the -march, -mtune, -mcpu
   handling code or by target attributes.  */
static const struct processor *selected_arch;
static const struct processor *selected_cpu;
static const struct processor *selected_tune;

enum aarch64_key_type aarch64_ra_sign_key = AARCH64_KEY_A;

/* The current tuning set.  */
struct tune_params aarch64_tune_params = generic_tunings;

/* Check whether an 'aarch64_vector_pcs' attribute is valid.  */

static tree
handle_aarch64_vector_pcs_attribute (tree *node, tree name, tree,
				     int, bool *no_add_attrs)
{
  /* Since we set fn_type_req to true, the caller should have checked
     this for us.  */
  gcc_assert (FUNC_OR_METHOD_TYPE_P (*node));
  switch ((arm_pcs) fntype_abi (*node).id ())
    {
    case ARM_PCS_AAPCS64:
    case ARM_PCS_SIMD:
      return NULL_TREE;

    case ARM_PCS_SVE:
      error ("the %qE attribute cannot be applied to an SVE function type",
	     name);
      *no_add_attrs = true;
      return NULL_TREE;

    case ARM_PCS_TLSDESC:
    case ARM_PCS_UNKNOWN:
      break;
    }
  gcc_unreachable ();
}

/* Table of machine attributes.  */
static const struct attribute_spec aarch64_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "aarch64_vector_pcs", 0, 0, false, true,  true,  true,
			  handle_aarch64_vector_pcs_attribute, NULL },
  { NULL,                 0, 0, false, false, false, false, NULL, NULL }
};

#define AARCH64_CPU_DEFAULT_FLAGS ((selected_cpu) ? selected_cpu->flags : 0)

/* An ISA extension in the co-processor and main instruction set space.  */
struct aarch64_option_extension
{
  const char *const name;
  const unsigned long flags_on;
  const unsigned long flags_off;
};

typedef enum aarch64_cond_code
{
  AARCH64_EQ = 0, AARCH64_NE, AARCH64_CS, AARCH64_CC, AARCH64_MI, AARCH64_PL,
  AARCH64_VS, AARCH64_VC, AARCH64_HI, AARCH64_LS, AARCH64_GE, AARCH64_LT,
  AARCH64_GT, AARCH64_LE, AARCH64_AL, AARCH64_NV
}
aarch64_cc;

#define AARCH64_INVERSE_CONDITION_CODE(X) ((aarch64_cc) (((int) X) ^ 1))

struct aarch64_branch_protect_type
{
  /* The type's name that the user passes to the branch-protection option
    string.  */
  const char* name;
  /* Function to handle the protection type and set global variables.
    First argument is the string token corresponding with this type and the
    second argument is the next token in the option string.
    Return values:
    * AARCH64_PARSE_OK: Handling was sucessful.
    * AARCH64_INVALID_ARG: The type is invalid in this context and the caller
      should print an error.
    * AARCH64_INVALID_FEATURE: The type is invalid and the handler prints its
      own error.  */
  enum aarch64_parse_opt_result (*handler)(char*, char*);
  /* A list of types that can follow this type in the option string.  */
  const aarch64_branch_protect_type* subtypes;
  unsigned int num_subtypes;
};

static enum aarch64_parse_opt_result
aarch64_handle_no_branch_protection (char* str, char* rest)
{
  aarch64_ra_sign_scope = AARCH64_FUNCTION_NONE;
  aarch64_enable_bti = 0;
  if (rest)
    {
      error ("unexpected %<%s%> after %<%s%>", rest, str);
      return AARCH64_PARSE_INVALID_FEATURE;
    }
  return AARCH64_PARSE_OK;
}

static enum aarch64_parse_opt_result
aarch64_handle_standard_branch_protection (char* str, char* rest)
{
  aarch64_ra_sign_scope = AARCH64_FUNCTION_NON_LEAF;
  aarch64_ra_sign_key = AARCH64_KEY_A;
  aarch64_enable_bti = 1;
  if (rest)
    {
      error ("unexpected %<%s%> after %<%s%>", rest, str);
      return AARCH64_PARSE_INVALID_FEATURE;
    }
  return AARCH64_PARSE_OK;
}

static enum aarch64_parse_opt_result
aarch64_handle_pac_ret_protection (char* str ATTRIBUTE_UNUSED,
				    char* rest ATTRIBUTE_UNUSED)
{
  aarch64_ra_sign_scope = AARCH64_FUNCTION_NON_LEAF;
  aarch64_ra_sign_key = AARCH64_KEY_A;
  return AARCH64_PARSE_OK;
}

static enum aarch64_parse_opt_result
aarch64_handle_pac_ret_leaf (char* str ATTRIBUTE_UNUSED,
			      char* rest ATTRIBUTE_UNUSED)
{
  aarch64_ra_sign_scope = AARCH64_FUNCTION_ALL;
  return AARCH64_PARSE_OK;
}

static enum aarch64_parse_opt_result
aarch64_handle_pac_ret_b_key (char* str ATTRIBUTE_UNUSED,
			      char* rest ATTRIBUTE_UNUSED)
{
  aarch64_ra_sign_key = AARCH64_KEY_B;
  return AARCH64_PARSE_OK;
}

static enum aarch64_parse_opt_result
aarch64_handle_bti_protection (char* str ATTRIBUTE_UNUSED,
				    char* rest ATTRIBUTE_UNUSED)
{
  aarch64_enable_bti = 1;
  return AARCH64_PARSE_OK;
}

static const struct aarch64_branch_protect_type aarch64_pac_ret_subtypes[] = {
  { "leaf", aarch64_handle_pac_ret_leaf, NULL, 0 },
  { "b-key", aarch64_handle_pac_ret_b_key, NULL, 0 },
  { NULL, NULL, NULL, 0 }
};

static const struct aarch64_branch_protect_type aarch64_branch_protect_types[] = {
  { "none", aarch64_handle_no_branch_protection, NULL, 0 },
  { "standard", aarch64_handle_standard_branch_protection, NULL, 0 },
  { "pac-ret", aarch64_handle_pac_ret_protection, aarch64_pac_ret_subtypes,
    ARRAY_SIZE (aarch64_pac_ret_subtypes) },
  { "bti", aarch64_handle_bti_protection, NULL, 0 },
  { NULL, NULL, NULL, 0 }
};

/* The condition codes of the processor, and the inverse function.  */
static const char * const aarch64_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

/* The preferred condition codes for SVE conditions.  */
static const char *const aarch64_sve_condition_codes[] =
{
  "none", "any", "nlast", "last", "first", "nfrst", "vs", "vc",
  "pmore", "plast", "tcont", "tstop", "gt", "le", "al", "nv"
};

/* Return the assembly token for svpattern value VALUE.  */

static const char *
svpattern_token (enum aarch64_svpattern pattern)
{
  switch (pattern)
    {
#define CASE(UPPER, LOWER, VALUE) case AARCH64_SV_##UPPER: return #LOWER;
    AARCH64_FOR_SVPATTERN (CASE)
#undef CASE
    case AARCH64_NUM_SVPATTERNS:
      break;
    }
  gcc_unreachable ();
}

/* Return the descriptor of the SIMD ABI.  */

static const predefined_function_abi &
aarch64_simd_abi (void)
{
  predefined_function_abi &simd_abi = function_abis[ARM_PCS_SIMD];
  if (!simd_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers
	= default_function_abi.full_reg_clobbers ();
      for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FP_SIMD_SAVED_REGNUM_P (regno))
	  CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      simd_abi.initialize (ARM_PCS_SIMD, full_reg_clobbers);
    }
  return simd_abi;
}

/* Return the descriptor of the SVE PCS.  */

static const predefined_function_abi &
aarch64_sve_abi (void)
{
  predefined_function_abi &sve_abi = function_abis[ARM_PCS_SVE];
  if (!sve_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers
	= default_function_abi.full_reg_clobbers ();
      for (int regno = V8_REGNUM; regno <= V23_REGNUM; ++regno)
	CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      for (int regno = P4_REGNUM; regno <= P11_REGNUM; ++regno)
	CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      sve_abi.initialize (ARM_PCS_SVE, full_reg_clobbers);
    }
  return sve_abi;
}

/* Generate code to enable conditional branches in functions over 1 MiB.  */
const char *
aarch64_gen_far_branch (rtx * operands, int pos_label, const char * dest,
			const char * branch_format)
{
    rtx_code_label * tmp_label = gen_label_rtx ();
    char label_buf[256];
    char buffer[128];
    ASM_GENERATE_INTERNAL_LABEL (label_buf, dest,
				 CODE_LABEL_NUMBER (tmp_label));
    const char *label_ptr = targetm.strip_name_encoding (label_buf);
    rtx dest_label = operands[pos_label];
    operands[pos_label] = tmp_label;

    snprintf (buffer, sizeof (buffer), "%s%s", branch_format, label_ptr);
    output_asm_insn (buffer, operands);

    snprintf (buffer, sizeof (buffer), "b\t%%l%d\n%s:", pos_label, label_ptr);
    operands[pos_label] = dest_label;
    output_asm_insn (buffer, operands);
    return "";
}

void
aarch64_err_no_fpadvsimd (machine_mode mode)
{
  if (TARGET_GENERAL_REGS_ONLY)
    if (FLOAT_MODE_P (mode))
      error ("%qs is incompatible with the use of floating-point types",
	     "-mgeneral-regs-only");
    else
      error ("%qs is incompatible with the use of vector types",
	     "-mgeneral-regs-only");
  else
    if (FLOAT_MODE_P (mode))
      error ("%qs feature modifier is incompatible with the use of"
	     " floating-point types", "+nofp");
    else
      error ("%qs feature modifier is incompatible with the use of"
	     " vector types", "+nofp");
}

/* Report when we try to do something that requires SVE when SVE is disabled.
   This is an error of last resort and isn't very high-quality.  It usually
   involves attempts to measure the vector length in some way.  */
static void
aarch64_report_sve_required (void)
{
  static bool reported_p = false;

  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_p)
    return;

  error ("this operation requires the SVE ISA extension");
  inform (input_location, "you can enable SVE using the command-line"
	  " option %<-march%>, or by using the %<target%>"
	  " attribute or pragma");
  reported_p = true;
}

/* Return true if REGNO is P0-P15 or one of the special FFR-related
   registers.  */
inline bool
pr_or_ffr_regnum_p (unsigned int regno)
{
  return PR_REGNUM_P (regno) || regno == FFR_REGNUM || regno == FFRT_REGNUM;
}

/* Implement TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS.
   The register allocator chooses POINTER_AND_FP_REGS if FP_REGS and
   GENERAL_REGS have the same cost - even if POINTER_AND_FP_REGS has a much
   higher cost.  POINTER_AND_FP_REGS is also used if the cost of both FP_REGS
   and GENERAL_REGS is lower than the memory cost (in this case the best class
   is the lowest cost one).  Using POINTER_AND_FP_REGS irrespectively of its
   cost results in bad allocations with many redundant int<->FP moves which
   are expensive on various cores.
   To avoid this we don't allow POINTER_AND_FP_REGS as the allocno class, but
   force a decision between FP_REGS and GENERAL_REGS.  We use the allocno class
   if it isn't POINTER_AND_FP_REGS.  Similarly, use the best class if it isn't
   POINTER_AND_FP_REGS.  Otherwise set the allocno class depending on the mode.
   The result of this is that it is no longer inefficient to have a higher
   memory move cost than the register move cost.
*/

static reg_class_t
aarch64_ira_change_pseudo_allocno_class (int regno, reg_class_t allocno_class,
					 reg_class_t best_class)
{
  machine_mode mode;

  if (!reg_class_subset_p (GENERAL_REGS, allocno_class)
      || !reg_class_subset_p (FP_REGS, allocno_class))
    return allocno_class;

  if (!reg_class_subset_p (GENERAL_REGS, best_class)
      || !reg_class_subset_p (FP_REGS, best_class))
    return best_class;

  mode = PSEUDO_REGNO_MODE (regno);
  return FLOAT_MODE_P (mode) || VECTOR_MODE_P (mode) ? FP_REGS : GENERAL_REGS;
}

static unsigned int
aarch64_min_divisions_for_recip_mul (machine_mode mode)
{
  if (GET_MODE_UNIT_SIZE (mode) == 4)
    return aarch64_tune_params.min_div_recip_mul_sf;
  return aarch64_tune_params.min_div_recip_mul_df;
}

/* Return the reassociation width of treeop OPC with mode MODE.  */
static int
aarch64_reassociation_width (unsigned opc, machine_mode mode)
{
  if (VECTOR_MODE_P (mode))
    return aarch64_tune_params.vec_reassoc_width;
  if (INTEGRAL_MODE_P (mode))
    return aarch64_tune_params.int_reassoc_width;
  /* Avoid reassociating floating point addition so we emit more FMAs.  */
  if (FLOAT_MODE_P (mode) && opc != PLUS_EXPR)
    return aarch64_tune_params.fp_reassoc_width;
  return 1;
}

/* Provide a mapping from gcc register numbers to dwarf register numbers.  */
unsigned
aarch64_dbx_register_number (unsigned regno)
{
   if (GP_REGNUM_P (regno))
     return AARCH64_DWARF_R0 + regno - R0_REGNUM;
   else if (regno == SP_REGNUM)
     return AARCH64_DWARF_SP;
   else if (FP_REGNUM_P (regno))
     return AARCH64_DWARF_V0 + regno - V0_REGNUM;
   else if (PR_REGNUM_P (regno))
     return AARCH64_DWARF_P0 + regno - P0_REGNUM;
   else if (regno == VG_REGNUM)
     return AARCH64_DWARF_VG;

   /* Return values >= DWARF_FRAME_REGISTERS indicate that there is no
      equivalent DWARF register.  */
   return DWARF_FRAME_REGISTERS;
}

/* If X is a CONST_DOUBLE, return its bit representation as a constant
   integer, otherwise return X unmodified.  */
static rtx
aarch64_bit_representation (rtx x)
{
  if (CONST_DOUBLE_P (x))
    x = gen_lowpart (int_mode_for_mode (GET_MODE (x)).require (), x);
  return x;
}

/* Return true if MODE is any of the Advanced SIMD structure modes.  */
static bool
aarch64_advsimd_struct_mode_p (machine_mode mode)
{
  return (TARGET_SIMD
	  && (mode == OImode || mode == CImode || mode == XImode));
}

/* Return true if MODE is an SVE predicate mode.  */
static bool
aarch64_sve_pred_mode_p (machine_mode mode)
{
  return (TARGET_SVE
	  && (mode == VNx16BImode
	      || mode == VNx8BImode
	      || mode == VNx4BImode
	      || mode == VNx2BImode));
}

/* Three mutually-exclusive flags describing a vector or predicate type.  */
const unsigned int VEC_ADVSIMD  = 1;
const unsigned int VEC_SVE_DATA = 2;
const unsigned int VEC_SVE_PRED = 4;
/* Can be used in combination with VEC_ADVSIMD or VEC_SVE_DATA to indicate
   a structure of 2, 3 or 4 vectors.  */
const unsigned int VEC_STRUCT   = 8;
/* Can be used in combination with VEC_SVE_DATA to indicate that the
   vector has fewer significant bytes than a full SVE vector.  */
const unsigned int VEC_PARTIAL  = 16;
/* Useful combinations of the above.  */
const unsigned int VEC_ANY_SVE  = VEC_SVE_DATA | VEC_SVE_PRED;
const unsigned int VEC_ANY_DATA = VEC_ADVSIMD | VEC_SVE_DATA;

/* Return a set of flags describing the vector properties of mode MODE.
   Ignore modes that are not supported by the current target.  */
static unsigned int
aarch64_classify_vector_mode (machine_mode mode)
{
  if (aarch64_advsimd_struct_mode_p (mode))
    return VEC_ADVSIMD | VEC_STRUCT;

  if (aarch64_sve_pred_mode_p (mode))
    return VEC_SVE_PRED;

  /* Make the decision based on the mode's enum value rather than its
     properties, so that we keep the correct classification regardless
     of -msve-vector-bits.  */
  switch (mode)
    {
    /* Partial SVE QI vectors.  */
    case E_VNx2QImode:
    case E_VNx4QImode:
    case E_VNx8QImode:
    /* Partial SVE HI vectors.  */
    case E_VNx2HImode:
    case E_VNx4HImode:
    /* Partial SVE SI vector.  */
    case E_VNx2SImode:
    /* Partial SVE HF vectors.  */
    case E_VNx2HFmode:
    case E_VNx4HFmode:
    /* Partial SVE SF vector.  */
    case E_VNx2SFmode:
      return TARGET_SVE ? VEC_SVE_DATA | VEC_PARTIAL : 0;

    case E_VNx16QImode:
    case E_VNx8HImode:
    case E_VNx4SImode:
    case E_VNx2DImode:
    case E_VNx8HFmode:
    case E_VNx4SFmode:
    case E_VNx2DFmode:
      return TARGET_SVE ? VEC_SVE_DATA : 0;

    /* x2 SVE vectors.  */
    case E_VNx32QImode:
    case E_VNx16HImode:
    case E_VNx8SImode:
    case E_VNx4DImode:
    case E_VNx16HFmode:
    case E_VNx8SFmode:
    case E_VNx4DFmode:
    /* x3 SVE vectors.  */
    case E_VNx48QImode:
    case E_VNx24HImode:
    case E_VNx12SImode:
    case E_VNx6DImode:
    case E_VNx24HFmode:
    case E_VNx12SFmode:
    case E_VNx6DFmode:
    /* x4 SVE vectors.  */
    case E_VNx64QImode:
    case E_VNx32HImode:
    case E_VNx16SImode:
    case E_VNx8DImode:
    case E_VNx32HFmode:
    case E_VNx16SFmode:
    case E_VNx8DFmode:
      return TARGET_SVE ? VEC_SVE_DATA | VEC_STRUCT : 0;

    /* 64-bit Advanced SIMD vectors.  */
    case E_V8QImode:
    case E_V4HImode:
    case E_V2SImode:
    /* ...E_V1DImode doesn't exist.  */
    case E_V4HFmode:
    case E_V2SFmode:
    case E_V1DFmode:
    /* 128-bit Advanced SIMD vectors.  */
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V8HFmode:
    case E_V4SFmode:
    case E_V2DFmode:
      return TARGET_SIMD ? VEC_ADVSIMD : 0;

    default:
      return 0;
    }
}

/* Return true if MODE is any of the data vector modes, including
   structure modes.  */
static bool
aarch64_vector_data_mode_p (machine_mode mode)
{
  return aarch64_classify_vector_mode (mode) & VEC_ANY_DATA;
}

/* Return true if MODE is any form of SVE mode, including predicates,
   vectors and structures.  */
bool
aarch64_sve_mode_p (machine_mode mode)
{
  return aarch64_classify_vector_mode (mode) & VEC_ANY_SVE;
}

/* Return true if MODE is an SVE data vector mode; either a single vector
   or a structure of vectors.  */
static bool
aarch64_sve_data_mode_p (machine_mode mode)
{
  return aarch64_classify_vector_mode (mode) & VEC_SVE_DATA;
}

/* Return the number of defined bytes in one constituent vector of
   SVE mode MODE, which has vector flags VEC_FLAGS.  */
static poly_int64
aarch64_vl_bytes (machine_mode mode, unsigned int vec_flags)
{
  if (vec_flags & VEC_PARTIAL)
    /* A single partial vector.  */
    return GET_MODE_SIZE (mode);

  if (vec_flags & VEC_SVE_DATA)
    /* A single vector or a tuple.  */
    return BYTES_PER_SVE_VECTOR;

  /* A single predicate.  */
  gcc_assert (vec_flags & VEC_SVE_PRED);
  return BYTES_PER_SVE_PRED;
}

/* Implement target hook TARGET_ARRAY_MODE.  */
static opt_machine_mode
aarch64_array_mode (machine_mode mode, unsigned HOST_WIDE_INT nelems)
{
  if (aarch64_classify_vector_mode (mode) == VEC_SVE_DATA
      && IN_RANGE (nelems, 2, 4))
    return mode_for_vector (GET_MODE_INNER (mode),
			    GET_MODE_NUNITS (mode) * nelems);

  return opt_machine_mode ();
}

/* Implement target hook TARGET_ARRAY_MODE_SUPPORTED_P.  */
static bool
aarch64_array_mode_supported_p (machine_mode mode,
				unsigned HOST_WIDE_INT nelems)
{
  if (TARGET_SIMD
      && (AARCH64_VALID_SIMD_QREG_MODE (mode)
	  || AARCH64_VALID_SIMD_DREG_MODE (mode))
      && (nelems >= 2 && nelems <= 4))
    return true;

  return false;
}

/* MODE is some form of SVE vector mode.  For data modes, return the number
   of vector register bits that each element of MODE occupies, such as 64
   for both VNx2DImode and VNx2SImode (where each 32-bit value is stored
   in a 64-bit container).  For predicate modes, return the number of
   data bits controlled by each significant predicate bit.  */

static unsigned int
aarch64_sve_container_bits (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  poly_uint64 vector_bits = (vec_flags & (VEC_PARTIAL | VEC_SVE_PRED)
			     ? BITS_PER_SVE_VECTOR
			     : GET_MODE_BITSIZE (mode));
  return vector_element_size (vector_bits, GET_MODE_NUNITS (mode));
}

/* Return the SVE predicate mode to use for elements that have
   ELEM_NBYTES bytes, if such a mode exists.  */

opt_machine_mode
aarch64_sve_pred_mode (unsigned int elem_nbytes)
{
  if (TARGET_SVE)
    {
      if (elem_nbytes == 1)
	return VNx16BImode;
      if (elem_nbytes == 2)
	return VNx8BImode;
      if (elem_nbytes == 4)
	return VNx4BImode;
      if (elem_nbytes == 8)
	return VNx2BImode;
    }
  return opt_machine_mode ();
}

/* Return the SVE predicate mode that should be used to control
   SVE mode MODE.  */

machine_mode
aarch64_sve_pred_mode (machine_mode mode)
{
  unsigned int bits = aarch64_sve_container_bits (mode);
  return aarch64_sve_pred_mode (bits / BITS_PER_UNIT).require ();
}

/* Implement TARGET_VECTORIZE_GET_MASK_MODE.  */

static opt_machine_mode
aarch64_get_mask_mode (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags & VEC_SVE_DATA)
    return aarch64_sve_pred_mode (mode);

  return default_get_mask_mode (mode);
}

/* Return the SVE vector mode that has NUNITS elements of mode INNER_MODE.  */

opt_machine_mode
aarch64_sve_data_mode (scalar_mode inner_mode, poly_uint64 nunits)
{
  enum mode_class mclass = (is_a <scalar_float_mode> (inner_mode)
			    ? MODE_VECTOR_FLOAT : MODE_VECTOR_INT);
  machine_mode mode;
  FOR_EACH_MODE_IN_CLASS (mode, mclass)
    if (inner_mode == GET_MODE_INNER (mode)
	&& known_eq (nunits, GET_MODE_NUNITS (mode))
	&& aarch64_sve_data_mode_p (mode))
      return mode;
  return opt_machine_mode ();
}

/* Return the integer element mode associated with SVE mode MODE.  */

static scalar_int_mode
aarch64_sve_element_int_mode (machine_mode mode)
{
  poly_uint64 vector_bits = (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
			     ? BITS_PER_SVE_VECTOR
			     : GET_MODE_BITSIZE (mode));
  unsigned int elt_bits = vector_element_size (vector_bits,
					       GET_MODE_NUNITS (mode));
  return int_mode_for_size (elt_bits, 0).require ();
}

/* Return an integer element mode that contains exactly
   aarch64_sve_container_bits (MODE) bits.  This is wider than
   aarch64_sve_element_int_mode if MODE is a partial vector,
   otherwise it's the same.  */

static scalar_int_mode
aarch64_sve_container_int_mode (machine_mode mode)
{
  return int_mode_for_size (aarch64_sve_container_bits (mode), 0).require ();
}

/* Return the integer vector mode associated with SVE mode MODE.
   Unlike related_int_vector_mode, this can handle the case in which
   MODE is a predicate (and thus has a different total size).  */

machine_mode
aarch64_sve_int_mode (machine_mode mode)
{
  scalar_int_mode int_mode = aarch64_sve_element_int_mode (mode);
  return aarch64_sve_data_mode (int_mode, GET_MODE_NUNITS (mode)).require ();
}

/* Implement TARGET_VECTORIZE_RELATED_MODE.  */

static opt_machine_mode
aarch64_vectorize_related_mode (machine_mode vector_mode,
				scalar_mode element_mode,
				poly_uint64 nunits)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (vector_mode);

  /* If we're operating on SVE vectors, try to return an SVE mode.  */
  poly_uint64 sve_nunits;
  if ((vec_flags & VEC_SVE_DATA)
      && multiple_p (BYTES_PER_SVE_VECTOR,
		     GET_MODE_SIZE (element_mode), &sve_nunits))
    {
      machine_mode sve_mode;
      if (maybe_ne (nunits, 0U))
	{
	  /* Try to find a full or partial SVE mode with exactly
	     NUNITS units.  */
	  if (multiple_p (sve_nunits, nunits)
	      && aarch64_sve_data_mode (element_mode,
					nunits).exists (&sve_mode))
	    return sve_mode;
	}
      else
	{
	  /* Take the preferred number of units from the number of bytes
	     that fit in VECTOR_MODE.  We always start by "autodetecting"
	     a full vector mode with preferred_simd_mode, so vectors
	     chosen here will also be full vector modes.  Then
	     autovectorize_vector_modes tries smaller starting modes
	     and thus smaller preferred numbers of units.  */
	  sve_nunits = ordered_min (sve_nunits, GET_MODE_SIZE (vector_mode));
	  if (aarch64_sve_data_mode (element_mode,
				     sve_nunits).exists (&sve_mode))
	    return sve_mode;
	}
    }

  /* Prefer to use 1 128-bit vector instead of 2 64-bit vectors.  */
  if ((vec_flags & VEC_ADVSIMD)
      && known_eq (nunits, 0U)
      && known_eq (GET_MODE_BITSIZE (vector_mode), 64U)
      && maybe_ge (GET_MODE_BITSIZE (element_mode)
		   * GET_MODE_NUNITS (vector_mode), 128U))
    {
      machine_mode res = aarch64_simd_container_mode (element_mode, 128);
      if (VECTOR_MODE_P (res))
	return res;
    }

  return default_vectorize_related_mode (vector_mode, element_mode, nunits);
}

/* Implement TARGET_PREFERRED_ELSE_VALUE.  For binary operations,
   prefer to use the first arithmetic operand as the else value if
   the else value doesn't matter, since that exactly matches the SVE
   destructive merging form.  For ternary operations we could either
   pick the first operand and use FMAD-like instructions or the last
   operand and use FMLA-like instructions; the latter seems more
   natural.  */

static tree
aarch64_preferred_else_value (unsigned, tree, unsigned int nops, tree *ops)
{
  return nops == 3 ? ops[2] : ops[0];
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
aarch64_hard_regno_nregs (unsigned regno, machine_mode mode)
{
  /* ??? Logically we should only need to provide a value when
     HARD_REGNO_MODE_OK says that the combination is valid,
     but at the moment we need to handle all modes.  Just ignore
     any runtime parts for registers that can't store them.  */
  HOST_WIDE_INT lowest_size = constant_lower_bound (GET_MODE_SIZE (mode));
  switch (aarch64_regno_regclass (regno))
    {
    case FP_REGS:
    case FP_LO_REGS:
    case FP_LO8_REGS:
      {
	unsigned int vec_flags = aarch64_classify_vector_mode (mode);
	if (vec_flags & VEC_SVE_DATA)
	  return exact_div (GET_MODE_SIZE (mode),
			    aarch64_vl_bytes (mode, vec_flags)).to_constant ();
	return CEIL (lowest_size, UNITS_PER_VREG);
      }
    case PR_REGS:
    case PR_LO_REGS:
    case PR_HI_REGS:
    case FFR_REGS:
    case PR_AND_FFR_REGS:
      return 1;
    default:
      return CEIL (lowest_size, UNITS_PER_WORD);
    }
  gcc_unreachable ();
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
aarch64_hard_regno_mode_ok (unsigned regno, machine_mode mode)
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;

  if (regno == VG_REGNUM)
    /* This must have the same size as _Unwind_Word.  */
    return mode == DImode;

  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags & VEC_SVE_PRED)
    return pr_or_ffr_regnum_p (regno);

  if (pr_or_ffr_regnum_p (regno))
    return false;

  if (regno == SP_REGNUM)
    /* The purpose of comparing with ptr_mode is to support the
       global register variable associated with the stack pointer
       register via the syntax of asm ("wsp") in ILP32.  */
    return mode == Pmode || mode == ptr_mode;

  if (regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    return mode == Pmode;

  if (GP_REGNUM_P (regno))
    {
      if (known_le (GET_MODE_SIZE (mode), 8))
	return true;
      else if (known_le (GET_MODE_SIZE (mode), 16))
	return (regno & 1) == 0;
    }
  else if (FP_REGNUM_P (regno))
    {
      if (vec_flags & VEC_STRUCT)
	return end_hard_regno (mode, regno) - 1 <= V31_REGNUM;
      else
	return !VECTOR_MODE_P (mode) || vec_flags != 0;
    }

  return false;
}

/* Return true if TYPE is a type that should be passed or returned in
   SVE registers, assuming enough registers are available.  When returning
   true, set *NUM_ZR and *NUM_PR to the number of required Z and P registers
   respectively.  */

static bool
aarch64_sve_argument_p (const_tree type, unsigned int *num_zr,
			unsigned int *num_pr)
{
  if (aarch64_sve::svbool_type_p (type))
    {
      *num_pr = 1;
      *num_zr = 0;
      return true;
    }

  if (unsigned int nvectors = aarch64_sve::nvectors_if_data_type (type))
    {
      *num_pr = 0;
      *num_zr = nvectors;
      return true;
    }

  return false;
}

/* Return true if a function with type FNTYPE returns its value in
   SVE vector or predicate registers.  */

static bool
aarch64_returns_value_in_sve_regs_p (const_tree fntype)
{
  unsigned int num_zr, num_pr;
  tree return_type = TREE_TYPE (fntype);
  return (return_type != error_mark_node
	  && aarch64_sve_argument_p (return_type, &num_zr, &num_pr));
}

/* Return true if a function with type FNTYPE takes arguments in
   SVE vector or predicate registers.  */

static bool
aarch64_takes_arguments_in_sve_regs_p (const_tree fntype)
{
  CUMULATIVE_ARGS args_so_far_v;
  aarch64_init_cumulative_args (&args_so_far_v, NULL_TREE, NULL_RTX,
				NULL_TREE, 0, true);
  cumulative_args_t args_so_far = pack_cumulative_args (&args_so_far_v);

  for (tree chain = TYPE_ARG_TYPES (fntype);
       chain && chain != void_list_node;
       chain = TREE_CHAIN (chain))
    {
      tree arg_type = TREE_VALUE (chain);
      if (arg_type == error_mark_node)
	return false;

      function_arg_info arg (arg_type, /*named=*/true);
      apply_pass_by_reference_rules (&args_so_far_v, arg);
      unsigned int num_zr, num_pr;
      if (aarch64_sve_argument_p (arg.type, &num_zr, &num_pr))
	return true;

      targetm.calls.function_arg_advance (args_so_far, arg);
    }
  return false;
}

/* Implement TARGET_FNTYPE_ABI.  */

static const predefined_function_abi &
aarch64_fntype_abi (const_tree fntype)
{
  if (lookup_attribute ("aarch64_vector_pcs", TYPE_ATTRIBUTES (fntype)))
    return aarch64_simd_abi ();

  if (aarch64_returns_value_in_sve_regs_p (fntype)
      || aarch64_takes_arguments_in_sve_regs_p (fntype))
    return aarch64_sve_abi ();

  return default_function_abi;
}

/* Return true if we should emit CFI for register REGNO.  */

static bool
aarch64_emit_cfi_for_reg_p (unsigned int regno)
{
  return (GP_REGNUM_P (regno)
	  || !default_function_abi.clobbers_full_reg_p (regno));
}

/* Return the mode we should use to save and restore register REGNO.  */

static machine_mode
aarch64_reg_save_mode (unsigned int regno)
{
  if (GP_REGNUM_P (regno))
    return DImode;

  if (FP_REGNUM_P (regno))
    switch (crtl->abi->id ())
      {
      case ARM_PCS_AAPCS64:
	/* Only the low 64 bits are saved by the base PCS.  */
	return DFmode;

      case ARM_PCS_SIMD:
	/* The vector PCS saves the low 128 bits (which is the full
	   register on non-SVE targets).  */
	return TFmode;

      case ARM_PCS_SVE:
	/* Use vectors of DImode for registers that need frame
	   information, so that the first 64 bytes of the save slot
	   are always the equivalent of what storing D<n> would give.  */
	if (aarch64_emit_cfi_for_reg_p (regno))
	  return VNx2DImode;

	/* Use vectors of bytes otherwise, so that the layout is
	   endian-agnostic, and so that we can use LDR and STR for
	   big-endian targets.  */
	return VNx16QImode;

      case ARM_PCS_TLSDESC:
      case ARM_PCS_UNKNOWN:
	break;
      }

  if (PR_REGNUM_P (regno))
    /* Save the full predicate register.  */
    return VNx16BImode;

  gcc_unreachable ();
}

/* Implement TARGET_INSN_CALLEE_ABI.  */

const predefined_function_abi &
aarch64_insn_callee_abi (const rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  gcc_assert (GET_CODE (pat) == PARALLEL);
  rtx unspec = XVECEXP (pat, 0, 1);
  gcc_assert (GET_CODE (unspec) == UNSPEC
	      && XINT (unspec, 1) == UNSPEC_CALLEE_ABI);
  return function_abis[INTVAL (XVECEXP (unspec, 0, 0))];
}

/* Implement TARGET_HARD_REGNO_CALL_PART_CLOBBERED.  The callee only saves
   the lower 64 bits of a 128-bit register.  Tell the compiler the callee
   clobbers the top 64 bits when restoring the bottom 64 bits.  */

static bool
aarch64_hard_regno_call_part_clobbered (unsigned int abi_id,
					unsigned int regno,
					machine_mode mode)
{
  if (FP_REGNUM_P (regno) && abi_id != ARM_PCS_SVE)
    {
      poly_int64 per_register_size = GET_MODE_SIZE (mode);
      unsigned int nregs = hard_regno_nregs (regno, mode);
      if (nregs > 1)
	per_register_size = exact_div (per_register_size, nregs);
      if (abi_id == ARM_PCS_SIMD || abi_id == ARM_PCS_TLSDESC)
	return maybe_gt (per_register_size, 16);
      return maybe_gt (per_register_size, 8);
    }
  return false;
}

/* Implement REGMODE_NATURAL_SIZE.  */
poly_uint64
aarch64_regmode_natural_size (machine_mode mode)
{
  /* The natural size for SVE data modes is one SVE data vector,
     and similarly for predicates.  We can't independently modify
     anything smaller than that.  */
  /* ??? For now, only do this for variable-width SVE registers.
     Doing it for constant-sized registers breaks lower-subreg.c.  */
  /* ??? And once that's fixed, we should probably have similar
     code for Advanced SIMD.  */
  if (!aarch64_sve_vg.is_constant ())
    {
      unsigned int vec_flags = aarch64_classify_vector_mode (mode);
      if (vec_flags & VEC_SVE_PRED)
	return BYTES_PER_SVE_PRED;
      if (vec_flags & VEC_SVE_DATA)
	return BYTES_PER_SVE_VECTOR;
    }
  return UNITS_PER_WORD;
}

/* Implement HARD_REGNO_CALLER_SAVE_MODE.  */
machine_mode
aarch64_hard_regno_caller_save_mode (unsigned regno, unsigned,
				     machine_mode mode)
{
  /* The predicate mode determines which bits are significant and
     which are "don't care".  Decreasing the number of lanes would
     lose data while increasing the number of lanes would make bits
     unnecessarily significant.  */
  if (PR_REGNUM_P (regno))
    return mode;
  if (known_ge (GET_MODE_SIZE (mode), 4))
    return mode;
  else
    return SImode;
}

/* Return true if I's bits are consecutive ones from the MSB.  */
bool
aarch64_high_bits_all_ones_p (HOST_WIDE_INT i)
{
  return exact_log2 (-i) != HOST_WIDE_INT_M1;
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  Make strings word-aligned so
   that strcpy from constants will be faster.  */

static HOST_WIDE_INT
aarch64_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST && !optimize_size)
    return MAX (align, BITS_PER_WORD);
  return align;
}

/* Return true if calls to DECL should be treated as
   long-calls (ie called via a register).  */
static bool
aarch64_decl_is_long_call_p (const_tree decl ATTRIBUTE_UNUSED)
{
  return false;
}

/* Return true if calls to symbol-ref SYM should be treated as
   long-calls (ie called via a register).  */
bool
aarch64_is_long_call_p (rtx sym)
{
  return aarch64_decl_is_long_call_p (SYMBOL_REF_DECL (sym));
}

/* Return true if calls to symbol-ref SYM should not go through
   plt stubs.  */

bool
aarch64_is_noplt_call_p (rtx sym)
{
  const_tree decl = SYMBOL_REF_DECL (sym);

  if (flag_pic
      && decl
      && (!flag_plt
	  || lookup_attribute ("noplt", DECL_ATTRIBUTES (decl)))
      && !targetm.binds_local_p (decl))
    return true;

  return false;
}

/* Return true if the offsets to a zero/sign-extract operation
   represent an expression that matches an extend operation.  The
   operands represent the paramters from

   (extract:MODE (mult (reg) (MULT_IMM)) (EXTRACT_IMM) (const_int 0)).  */
bool
aarch64_is_extend_from_extract (scalar_int_mode mode, rtx mult_imm,
				rtx extract_imm)
{
  HOST_WIDE_INT mult_val, extract_val;

  if (! CONST_INT_P (mult_imm) || ! CONST_INT_P (extract_imm))
    return false;

  mult_val = INTVAL (mult_imm);
  extract_val = INTVAL (extract_imm);

  if (extract_val > 8
      && extract_val < GET_MODE_BITSIZE (mode)
      && exact_log2 (extract_val & ~7) > 0
      && (extract_val & 7) <= 4
      && mult_val == (1 << (extract_val & 7)))
    return true;

  return false;
}

/* Emit an insn that's a simple single-set.  Both the operands must be
   known to be valid.  */
inline static rtx_insn *
emit_set_insn (rtx x, rtx y)
{
  return emit_insn (gen_rtx_SET (x, y));
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  */
rtx
aarch64_gen_compare_reg (RTX_CODE code, rtx x, rtx y)
{
  machine_mode cmp_mode = GET_MODE (x);
  machine_mode cc_mode;
  rtx cc_reg;

  if (cmp_mode == TImode)
    {
      gcc_assert (code == NE);

      cc_mode = CCmode;
      cc_reg = gen_rtx_REG (cc_mode, CC_REGNUM);

      rtx x_lo = operand_subword (x, 0, 0, TImode);
      rtx y_lo = operand_subword (y, 0, 0, TImode);
      emit_set_insn (cc_reg, gen_rtx_COMPARE (cc_mode, x_lo, y_lo));

      rtx x_hi = operand_subword (x, 1, 0, TImode);
      rtx y_hi = operand_subword (y, 1, 0, TImode);
      emit_insn (gen_ccmpdi (cc_reg, cc_reg, x_hi, y_hi,
			     gen_rtx_EQ (cc_mode, cc_reg, const0_rtx),
			     GEN_INT (AARCH64_EQ)));
    }
  else
    {
      cc_mode = SELECT_CC_MODE (code, x, y);
      cc_reg = gen_rtx_REG (cc_mode, CC_REGNUM);
      emit_set_insn (cc_reg, gen_rtx_COMPARE (cc_mode, x, y));
    }
  return cc_reg;
}

/* Similarly, but maybe zero-extend Y if Y_MODE < SImode.  */

static rtx
aarch64_gen_compare_reg_maybe_ze (RTX_CODE code, rtx x, rtx y,
                                  machine_mode y_mode)
{
  if (y_mode == E_QImode || y_mode == E_HImode)
    {
      if (CONST_INT_P (y))
	y = GEN_INT (INTVAL (y) & GET_MODE_MASK (y_mode));
      else
	{
	  rtx t, cc_reg;
	  machine_mode cc_mode;

	  t = gen_rtx_ZERO_EXTEND (SImode, y);
	  t = gen_rtx_COMPARE (CC_SWPmode, t, x);
	  cc_mode = CC_SWPmode;
	  cc_reg = gen_rtx_REG (cc_mode, CC_REGNUM);
	  emit_set_insn (cc_reg, t);
	  return cc_reg;
	}
    }

  if (!aarch64_plus_operand (y, y_mode))
    y = force_reg (y_mode, y);

  return aarch64_gen_compare_reg (code, x, y);
}

/* Build the SYMBOL_REF for __tls_get_addr.  */

static GTY(()) rtx tls_get_addr_libfunc;

rtx
aarch64_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}

/* Return the TLS model to use for ADDR.  */

static enum tls_model
tls_symbolic_operand_type (rtx addr)
{
  enum tls_model tls_kind = TLS_MODEL_NONE;
  if (GET_CODE (addr) == CONST)
    {
      poly_int64 addend;
      rtx sym = strip_offset (addr, &addend);
      if (GET_CODE (sym) == SYMBOL_REF)
	tls_kind = SYMBOL_REF_TLS_MODEL (sym);
    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    tls_kind = SYMBOL_REF_TLS_MODEL (addr);

  return tls_kind;
}

/* We'll allow lo_sum's in addresses in our legitimate addresses
   so that combine would take care of combining addresses where
   necessary, but for generation purposes, we'll generate the address
   as :
   RTL                               Absolute
   tmp = hi (symbol_ref);            adrp  x1, foo
   dest = lo_sum (tmp, symbol_ref);  add dest, x1, :lo_12:foo
                                     nop

   PIC                               TLS
   adrp x1, :got:foo                 adrp tmp, :tlsgd:foo
   ldr  x1, [:got_lo12:foo]          add  dest, tmp, :tlsgd_lo12:foo
                                     bl   __tls_get_addr
                                     nop

   Load TLS symbol, depending on TLS mechanism and TLS access model.

   Global Dynamic - Traditional TLS:
   adrp tmp, :tlsgd:imm
   add  dest, tmp, #:tlsgd_lo12:imm
   bl   __tls_get_addr

   Global Dynamic - TLS Descriptors:
   adrp dest, :tlsdesc:imm
   ldr  tmp, [dest, #:tlsdesc_lo12:imm]
   add  dest, dest, #:tlsdesc_lo12:imm
   blr  tmp
   mrs  tp, tpidr_el0
   add  dest, dest, tp

   Initial Exec:
   mrs  tp, tpidr_el0
   adrp tmp, :gottprel:imm
   ldr  dest, [tmp, #:gottprel_lo12:imm]
   add  dest, dest, tp

   Local Exec:
   mrs  tp, tpidr_el0
   add  t0, tp, #:tprel_hi12:imm, lsl #12
   add  t0, t0, #:tprel_lo12_nc:imm
*/

static void
aarch64_load_symref_appropriately (rtx dest, rtx imm,
				   enum aarch64_symbol_type type)
{
  switch (type)
    {
    case SYMBOL_SMALL_ABSOLUTE:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode.  */
	rtx tmp_reg = dest;
	machine_mode mode = GET_MODE (dest);

	gcc_assert (mode == Pmode || mode == ptr_mode);

	if (can_create_pseudo_p ())
	  tmp_reg = gen_reg_rtx (mode);

	emit_move_insn (tmp_reg, gen_rtx_HIGH (mode, imm));
	emit_insn (gen_add_losym (dest, tmp_reg, imm));
	return;
      }

    case SYMBOL_TINY_ABSOLUTE:
      emit_insn (gen_rtx_SET (dest, imm));
      return;

    case SYMBOL_SMALL_GOT_28K:
      {
	machine_mode mode = GET_MODE (dest);
	rtx gp_rtx = pic_offset_table_rtx;
	rtx insn;
	rtx mem;

	/* NOTE: pic_offset_table_rtx can be NULL_RTX, because we can reach
	   here before rtl expand.  Tree IVOPT will generate rtl pattern to
	   decide rtx costs, in which case pic_offset_table_rtx is not
	   initialized.  For that case no need to generate the first adrp
	   instruction as the final cost for global variable access is
	   one instruction.  */
	if (gp_rtx != NULL)
	  {
	    /* -fpic for -mcmodel=small allow 32K GOT table size (but we are
	       using the page base as GOT base, the first page may be wasted,
	       in the worst scenario, there is only 28K space for GOT).

	       The generate instruction sequence for accessing global variable
	       is:

		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym]

	       Only one instruction needed. But we must initialize
	       pic_offset_table_rtx properly.  We generate initialize insn for
	       every global access, and allow CSE to remove all redundant.

	       The final instruction sequences will look like the following
	       for multiply global variables access.

		 adrp pic_offset_table_rtx, _GLOBAL_OFFSET_TABLE_

		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym1]
		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym2]
		 ldr reg, [pic_offset_table_rtx, #:gotpage_lo15:sym3]
		 ...  */

	    rtx s = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
	    crtl->uses_pic_offset_table = 1;
	    emit_move_insn (gp_rtx, gen_rtx_HIGH (Pmode, s));

	    if (mode != GET_MODE (gp_rtx))
             gp_rtx = gen_lowpart (mode, gp_rtx);

	  }

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      insn = gen_ldr_got_small_28k_di (dest, gp_rtx, imm);
	    else
	      insn = gen_ldr_got_small_28k_si (dest, gp_rtx, imm);

	    mem = XVECEXP (SET_SRC (insn), 0, 0);
	  }
	else
	  {
	    gcc_assert (mode == Pmode);

	    insn = gen_ldr_got_small_28k_sidi (dest, gp_rtx, imm);
	    mem = XVECEXP (XEXP (SET_SRC (insn), 0), 0, 0);
	  }

	/* The operand is expected to be MEM.  Whenever the related insn
	   pattern changed, above code which calculate mem should be
	   updated.  */
	gcc_assert (GET_CODE (mem) == MEM);
	MEM_READONLY_P (mem) = 1;
	MEM_NOTRAP_P (mem) = 1;
	emit_insn (insn);
	return;
      }

    case SYMBOL_SMALL_GOT_4G:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode,
	   while the got entry is always of SImode size.  The mode of
	   dest depends on how dest is used: if dest is assigned to a
	   pointer (e.g. in the memory), it has SImode; it may have
	   DImode if dest is dereferenced to access the memeory.
	   This is why we have to handle three different ldr_got_small
	   patterns here (two patterns for ILP32).  */

	rtx insn;
	rtx mem;
	rtx tmp_reg = dest;
	machine_mode mode = GET_MODE (dest);

	if (can_create_pseudo_p ())
	  tmp_reg = gen_reg_rtx (mode);

	emit_move_insn (tmp_reg, gen_rtx_HIGH (mode, imm));
	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      insn = gen_ldr_got_small_di (dest, tmp_reg, imm);
	    else
	      insn = gen_ldr_got_small_si (dest, tmp_reg, imm);

	    mem = XVECEXP (SET_SRC (insn), 0, 0);
	  }
	else
	  {
	    gcc_assert (mode == Pmode);

	    insn = gen_ldr_got_small_sidi (dest, tmp_reg, imm);
	    mem = XVECEXP (XEXP (SET_SRC (insn), 0), 0, 0);
	  }

	gcc_assert (GET_CODE (mem) == MEM);
	MEM_READONLY_P (mem) = 1;
	MEM_NOTRAP_P (mem) = 1;
	emit_insn (insn);
	return;
      }

    case SYMBOL_SMALL_TLSGD:
      {
	rtx_insn *insns;
	machine_mode mode = GET_MODE (dest);
	rtx result = gen_rtx_REG (mode, R0_REGNUM);

	start_sequence ();
	if (TARGET_ILP32)
	  aarch64_emit_call_insn (gen_tlsgd_small_si (result, imm));
	else
	  aarch64_emit_call_insn (gen_tlsgd_small_di (result, imm));
	insns = get_insns ();
	end_sequence ();

	RTL_CONST_CALL_P (insns) = 1;
	emit_libcall_block (insns, dest, result, imm);
	return;
      }

    case SYMBOL_SMALL_TLSDESC:
      {
	machine_mode mode = GET_MODE (dest);
	rtx x0 = gen_rtx_REG (mode, R0_REGNUM);
	rtx tp;

	gcc_assert (mode == Pmode || mode == ptr_mode);

	/* In ILP32, the got entry is always of SImode size.  Unlike
	   small GOT, the dest is fixed at reg 0.  */
	if (TARGET_ILP32)
	  emit_insn (gen_tlsdesc_small_si (imm));
	else
	  emit_insn (gen_tlsdesc_small_di (imm));
	tp = aarch64_load_tp (NULL);

	if (mode != Pmode)
	  tp = gen_lowpart (mode, tp);

	emit_insn (gen_rtx_SET (dest, gen_rtx_PLUS (mode, tp, x0)));
	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_SMALL_TLSIE:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode,
	   while the got entry is always of SImode size.  The mode of
	   dest depends on how dest is used: if dest is assigned to a
	   pointer (e.g. in the memory), it has SImode; it may have
	   DImode if dest is dereferenced to access the memeory.
	   This is why we have to handle three different tlsie_small
	   patterns here (two patterns for ILP32).  */
	machine_mode mode = GET_MODE (dest);
	rtx tmp_reg = gen_reg_rtx (mode);
	rtx tp = aarch64_load_tp (NULL);

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      emit_insn (gen_tlsie_small_di (tmp_reg, imm));
	    else
	      {
		emit_insn (gen_tlsie_small_si (tmp_reg, imm));
		tp = gen_lowpart (mode, tp);
	      }
	  }
	else
	  {
	    gcc_assert (mode == Pmode);
	    emit_insn (gen_tlsie_small_sidi (tmp_reg, imm));
	  }

	emit_insn (gen_rtx_SET (dest, gen_rtx_PLUS (mode, tp, tmp_reg)));
	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_TLSLE12:
    case SYMBOL_TLSLE24:
    case SYMBOL_TLSLE32:
    case SYMBOL_TLSLE48:
      {
	machine_mode mode = GET_MODE (dest);
	rtx tp = aarch64_load_tp (NULL);

	if (mode != Pmode)
	  tp = gen_lowpart (mode, tp);

	switch (type)
	  {
	  case SYMBOL_TLSLE12:
	    emit_insn ((mode == DImode ? gen_tlsle12_di : gen_tlsle12_si)
			(dest, tp, imm));
	    break;
	  case SYMBOL_TLSLE24:
	    emit_insn ((mode == DImode ? gen_tlsle24_di : gen_tlsle24_si)
			(dest, tp, imm));
	  break;
	  case SYMBOL_TLSLE32:
	    emit_insn ((mode == DImode ? gen_tlsle32_di : gen_tlsle32_si)
			(dest, imm));
	    emit_insn ((mode == DImode ? gen_adddi3 : gen_addsi3)
			(dest, dest, tp));
	  break;
	  case SYMBOL_TLSLE48:
	    emit_insn ((mode == DImode ? gen_tlsle48_di : gen_tlsle48_si)
			(dest, imm));
	    emit_insn ((mode == DImode ? gen_adddi3 : gen_addsi3)
			(dest, dest, tp));
	    break;
	  default:
	    gcc_unreachable ();
	  }

	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_TINY_GOT:
      emit_insn (gen_ldr_got_tiny (dest, imm));
      return;

    case SYMBOL_TINY_TLSIE:
      {
	machine_mode mode = GET_MODE (dest);
	rtx tp = aarch64_load_tp (NULL);

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      emit_insn (gen_tlsie_tiny_di (dest, imm, tp));
	    else
	      {
		tp = gen_lowpart (mode, tp);
		emit_insn (gen_tlsie_tiny_si (dest, imm, tp));
	      }
	  }
	else
	  {
	    gcc_assert (mode == Pmode);
	    emit_insn (gen_tlsie_tiny_sidi (dest, imm, tp));
	  }

	if (REG_P (dest))
	  set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    default:
      gcc_unreachable ();
    }
}

/* Emit a move from SRC to DEST.  Assume that the move expanders can
   handle all moves if !can_create_pseudo_p ().  The distinction is
   important because, unlike emit_move_insn, the move expanders know
   how to force Pmode objects into the constant pool even when the
   constant pool address is not itself legitimate.  */
static rtx
aarch64_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p ()
	  ? emit_move_insn (dest, src)
	  : emit_move_insn_1 (dest, src));
}

/* Apply UNOPTAB to OP and store the result in DEST.  */

static void
aarch64_emit_unop (rtx dest, optab unoptab, rtx op)
{
  rtx tmp = expand_unop (GET_MODE (dest), unoptab, op, dest, 0);
  if (dest != tmp)
    emit_move_insn (dest, tmp);
}

/* Apply BINOPTAB to OP0 and OP1 and store the result in DEST.  */

static void
aarch64_emit_binop (rtx dest, optab binoptab, rtx op0, rtx op1)
{
  rtx tmp = expand_binop (GET_MODE (dest), binoptab, op0, op1, dest, 0,
			  OPTAB_DIRECT);
  if (dest != tmp)
    emit_move_insn (dest, tmp);
}

/* Split a 128-bit move operation into two 64-bit move operations,
   taking care to handle partial overlap of register to register
   copies.  Special cases are needed when moving between GP regs and
   FP regs.  SRC can be a register, constant or memory; DST a register
   or memory.  If either operand is memory it must not have any side
   effects.  */
void
aarch64_split_128bit_move (rtx dst, rtx src)
{
  rtx dst_lo, dst_hi;
  rtx src_lo, src_hi;

  machine_mode mode = GET_MODE (dst);

  gcc_assert (mode == TImode || mode == TFmode);
  gcc_assert (!(side_effects_p (src) || side_effects_p (dst)));
  gcc_assert (mode == GET_MODE (src) || GET_MODE (src) == VOIDmode);

  if (REG_P (dst) && REG_P (src))
    {
      int src_regno = REGNO (src);
      int dst_regno = REGNO (dst);

      /* Handle FP <-> GP regs.  */
      if (FP_REGNUM_P (dst_regno) && GP_REGNUM_P (src_regno))
	{
	  src_lo = gen_lowpart (word_mode, src);
	  src_hi = gen_highpart (word_mode, src);

	  emit_insn (gen_aarch64_movlow_di (mode, dst, src_lo));
	  emit_insn (gen_aarch64_movhigh_di (mode, dst, src_hi));
	  return;
	}
      else if (GP_REGNUM_P (dst_regno) && FP_REGNUM_P (src_regno))
	{
	  dst_lo = gen_lowpart (word_mode, dst);
	  dst_hi = gen_highpart (word_mode, dst);

	  emit_insn (gen_aarch64_movdi_low (mode, dst_lo, src));
	  emit_insn (gen_aarch64_movdi_high (mode, dst_hi, src));
	  return;
	}
    }

  dst_lo = gen_lowpart (word_mode, dst);
  dst_hi = gen_highpart (word_mode, dst);
  src_lo = gen_lowpart (word_mode, src);
  src_hi = gen_highpart_mode (word_mode, mode, src);

  /* At most one pairing may overlap.  */
  if (reg_overlap_mentioned_p (dst_lo, src_hi))
    {
      aarch64_emit_move (dst_hi, src_hi);
      aarch64_emit_move (dst_lo, src_lo);
    }
  else
    {
      aarch64_emit_move (dst_lo, src_lo);
      aarch64_emit_move (dst_hi, src_hi);
    }
}

bool
aarch64_split_128bit_move_p (rtx dst, rtx src)
{
  return (! REG_P (src)
	  || ! (FP_REGNUM_P (REGNO (dst)) && FP_REGNUM_P (REGNO (src))));
}

/* Split a complex SIMD combine.  */

void
aarch64_split_simd_combine (rtx dst, rtx src1, rtx src2)
{
  machine_mode src_mode = GET_MODE (src1);
  machine_mode dst_mode = GET_MODE (dst);

  gcc_assert (VECTOR_MODE_P (dst_mode));
  gcc_assert (register_operand (dst, dst_mode)
	      && register_operand (src1, src_mode)
	      && register_operand (src2, src_mode));

  emit_insn (gen_aarch64_simd_combine (src_mode, dst, src1, src2));
  return;
}

/* Split a complex SIMD move.  */

void
aarch64_split_simd_move (rtx dst, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  machine_mode dst_mode = GET_MODE (dst);

  gcc_assert (VECTOR_MODE_P (dst_mode));

  if (REG_P (dst) && REG_P (src))
    {
      gcc_assert (VECTOR_MODE_P (src_mode));
      emit_insn (gen_aarch64_split_simd_mov (src_mode, dst, src));
    }
}

bool
aarch64_zero_extend_const_eq (machine_mode xmode, rtx x,
			      machine_mode ymode, rtx y)
{
  rtx r = simplify_const_unary_operation (ZERO_EXTEND, xmode, y, ymode);
  gcc_assert (r != NULL);
  return rtx_equal_p (x, r);
}

/* Return TARGET if it is nonnull and a register of mode MODE.
   Otherwise, return a fresh register of mode MODE if we can,
   or TARGET reinterpreted as MODE if we can't.  */

static rtx
aarch64_target_reg (rtx target, machine_mode mode)
{
  if (target && REG_P (target) && GET_MODE (target) == mode)
    return target;
  if (!can_create_pseudo_p ())
    {
      gcc_assert (target);
      return gen_lowpart (mode, target);
    }
  return gen_reg_rtx (mode);
}

/* Return a register that contains the constant in BUILDER, given that
   the constant is a legitimate move operand.  Use TARGET as the register
   if it is nonnull and convenient.  */

static rtx
aarch64_emit_set_immediate (rtx target, rtx_vector_builder &builder)
{
  rtx src = builder.build ();
  target = aarch64_target_reg (target, GET_MODE (src));
  emit_insn (gen_rtx_SET (target, src));
  return target;
}

static rtx
aarch64_force_temporary (machine_mode mode, rtx x, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (mode, value);
  else
    {
      gcc_assert (x);
      aarch64_emit_move (x, value);
      return x;
    }
}

/* Return true if predicate value X is a constant in which every element
   is a CONST_INT.  When returning true, describe X in BUILDER as a VNx16BI
   value, i.e. as a predicate in which all bits are significant.  */

static bool
aarch64_get_sve_pred_bits (rtx_vector_builder &builder, rtx x)
{
  if (GET_CODE (x) != CONST_VECTOR)
    return false;

  unsigned int factor = vector_element_size (GET_MODE_NUNITS (VNx16BImode),
					     GET_MODE_NUNITS (GET_MODE (x)));
  unsigned int npatterns = CONST_VECTOR_NPATTERNS (x) * factor;
  unsigned int nelts_per_pattern = CONST_VECTOR_NELTS_PER_PATTERN (x);
  builder.new_vector (VNx16BImode, npatterns, nelts_per_pattern);

  unsigned int nelts = const_vector_encoded_nelts (x);
  for (unsigned int i = 0; i < nelts; ++i)
    {
      rtx elt = CONST_VECTOR_ENCODED_ELT (x, i);
      if (!CONST_INT_P (elt))
	return false;

      builder.quick_push (elt);
      for (unsigned int j = 1; j < factor; ++j)
	builder.quick_push (const0_rtx);
    }
  builder.finalize ();
  return true;
}

/* BUILDER contains a predicate constant of mode VNx16BI.  Return the
   widest predicate element size it can have (that is, the largest size
   for which each element would still be 0 or 1).  */

unsigned int
aarch64_widest_sve_pred_elt_size (rtx_vector_builder &builder)
{
  /* Start with the most optimistic assumption: that we only need
     one bit per pattern.  This is what we will use if only the first
     bit in each pattern is ever set.  */
  unsigned int mask = GET_MODE_SIZE (DImode);
  mask |= builder.npatterns ();

  /* Look for set bits.  */
  unsigned int nelts = builder.encoded_nelts ();
  for (unsigned int i = 1; i < nelts; ++i)
    if (INTVAL (builder.elt (i)) != 0)
      {
	if (i & 1)
	  return 1;
	mask |= i;
      }
  return mask & -mask;
}

/* If VNx16BImode rtx X is a canonical PTRUE for a predicate mode,
   return that predicate mode, otherwise return opt_machine_mode ().  */

opt_machine_mode
aarch64_ptrue_all_mode (rtx x)
{
  gcc_assert (GET_MODE (x) == VNx16BImode);
  if (GET_CODE (x) != CONST_VECTOR
      || !CONST_VECTOR_DUPLICATE_P (x)
      || !CONST_INT_P (CONST_VECTOR_ENCODED_ELT (x, 0))
      || INTVAL (CONST_VECTOR_ENCODED_ELT (x, 0)) == 0)
    return opt_machine_mode ();

  unsigned int nelts = const_vector_encoded_nelts (x);
  for (unsigned int i = 1; i < nelts; ++i)
    if (CONST_VECTOR_ENCODED_ELT (x, i) != const0_rtx)
      return opt_machine_mode ();

  return aarch64_sve_pred_mode (nelts);
}

/* BUILDER is a predicate constant of mode VNx16BI.  Consider the value
   that the constant would have with predicate element size ELT_SIZE
   (ignoring the upper bits in each element) and return:

   * -1 if all bits are set
   * N if the predicate has N leading set bits followed by all clear bits
   * 0 if the predicate does not have any of these forms.  */

int
aarch64_partial_ptrue_length (rtx_vector_builder &builder,
			      unsigned int elt_size)
{
  /* If nelts_per_pattern is 3, we have set bits followed by clear bits
     followed by set bits.  */
  if (builder.nelts_per_pattern () == 3)
    return 0;

  /* Skip over leading set bits.  */
  unsigned int nelts = builder.encoded_nelts ();
  unsigned int i = 0;
  for (; i < nelts; i += elt_size)
    if (INTVAL (builder.elt (i)) == 0)
      break;
  unsigned int vl = i / elt_size;

  /* Check for the all-true case.  */
  if (i == nelts)
    return -1;

  /* If nelts_per_pattern is 1, then either VL is zero, or we have a
     repeating pattern of set bits followed by clear bits.  */
  if (builder.nelts_per_pattern () != 2)
    return 0;

  /* We have a "foreground" value and a duplicated "background" value.
     If the background might repeat and the last set bit belongs to it,
     we might have set bits followed by clear bits followed by set bits.  */
  if (i > builder.npatterns () && maybe_ne (nelts, builder.full_nelts ()))
    return 0;

  /* Make sure that the rest are all clear.  */
  for (; i < nelts; i += elt_size)
    if (INTVAL (builder.elt (i)) != 0)
      return 0;

  return vl;
}

/* See if there is an svpattern that encodes an SVE predicate of mode
   PRED_MODE in which the first VL bits are set and the rest are clear.
   Return the pattern if so, otherwise return AARCH64_NUM_SVPATTERNS.
   A VL of -1 indicates an all-true vector.  */

aarch64_svpattern
aarch64_svpattern_for_vl (machine_mode pred_mode, int vl)
{
  if (vl < 0)
    return AARCH64_SV_ALL;

  if (maybe_gt (vl, GET_MODE_NUNITS (pred_mode)))
    return AARCH64_NUM_SVPATTERNS;

  if (vl >= 1 && vl <= 8)
    return aarch64_svpattern (AARCH64_SV_VL1 + (vl - 1));

  if (vl >= 16 && vl <= 256 && pow2p_hwi (vl))
    return aarch64_svpattern (AARCH64_SV_VL16 + (exact_log2 (vl) - 4));

  int max_vl;
  if (GET_MODE_NUNITS (pred_mode).is_constant (&max_vl))
    {
      if (vl == (max_vl / 3) * 3)
	return AARCH64_SV_MUL3;
      /* These would only trigger for non-power-of-2 lengths.  */
      if (vl == (max_vl & -4))
	return AARCH64_SV_MUL4;
      if (vl == (1 << floor_log2 (max_vl)))
	return AARCH64_SV_POW2;
      if (vl == max_vl)
	return AARCH64_SV_ALL;
    }
  return AARCH64_NUM_SVPATTERNS;
}

/* Return a VNx16BImode constant in which every sequence of ELT_SIZE
   bits has the lowest bit set and the upper bits clear.  This is the
   VNx16BImode equivalent of a PTRUE for controlling elements of
   ELT_SIZE bytes.  However, because the constant is VNx16BImode,
   all bits are significant, even the upper zeros.  */

rtx
aarch64_ptrue_all (unsigned int elt_size)
{
  rtx_vector_builder builder (VNx16BImode, elt_size, 1);
  builder.quick_push (const1_rtx);
  for (unsigned int i = 1; i < elt_size; ++i)
    builder.quick_push (const0_rtx);
  return builder.build ();
}

/* Return an all-true predicate register of mode MODE.  */

rtx
aarch64_ptrue_reg (machine_mode mode)
{
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL);
  rtx reg = force_reg (VNx16BImode, CONSTM1_RTX (VNx16BImode));
  return gen_lowpart (mode, reg);
}

/* Return an all-false predicate register of mode MODE.  */

rtx
aarch64_pfalse_reg (machine_mode mode)
{
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL);
  rtx reg = force_reg (VNx16BImode, CONST0_RTX (VNx16BImode));
  return gen_lowpart (mode, reg);
}

/* Return true if predicate PRED1[0] is true whenever predicate PRED2 is
   true, or alternatively if we know that the operation predicated by
   PRED1[0] is safe to perform whenever PRED2 is true.  PRED1[1] is a
   aarch64_sve_gp_strictness operand that describes the operation
   predicated by PRED1[0].  */

bool
aarch64_sve_pred_dominates_p (rtx *pred1, rtx pred2)
{
  machine_mode mode = GET_MODE (pred2);
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
	      && mode == GET_MODE (pred1[0])
	      && aarch64_sve_gp_strictness (pred1[1], SImode));
  return (pred1[0] == CONSTM1_RTX (mode)
	  || INTVAL (pred1[1]) == SVE_RELAXED_GP
	  || rtx_equal_p (pred1[0], pred2));
}

/* PRED1[0] is a PTEST predicate and PRED1[1] is an aarch64_sve_ptrue_flag
   for it.  PRED2[0] is the predicate for the instruction whose result
   is tested by the PTEST and PRED2[1] is again an aarch64_sve_ptrue_flag
   for it.  Return true if we can prove that the two predicates are
   equivalent for PTEST purposes; that is, if we can replace PRED2[0]
   with PRED1[0] without changing behavior.  */

bool
aarch64_sve_same_pred_for_ptest_p (rtx *pred1, rtx *pred2)
{
  machine_mode mode = GET_MODE (pred1[0]);
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
	      && mode == GET_MODE (pred2[0])
	      && aarch64_sve_ptrue_flag (pred1[1], SImode)
	      && aarch64_sve_ptrue_flag (pred2[1], SImode));

  bool ptrue1_p = (pred1[0] == CONSTM1_RTX (mode)
		   || INTVAL (pred1[1]) == SVE_KNOWN_PTRUE);
  bool ptrue2_p = (pred2[0] == CONSTM1_RTX (mode)
		   || INTVAL (pred2[1]) == SVE_KNOWN_PTRUE);
  return (ptrue1_p && ptrue2_p) || rtx_equal_p (pred1[0], pred2[0]);
}

/* Emit a comparison CMP between OP0 and OP1, both of which have mode
   DATA_MODE, and return the result in a predicate of mode PRED_MODE.
   Use TARGET as the target register if nonnull and convenient.  */

static rtx
aarch64_sve_emit_int_cmp (rtx target, machine_mode pred_mode, rtx_code cmp,
			  machine_mode data_mode, rtx op1, rtx op2)
{
  insn_code icode = code_for_aarch64_pred_cmp (cmp, data_mode);
  expand_operand ops[5];
  create_output_operand (&ops[0], target, pred_mode);
  create_input_operand (&ops[1], CONSTM1_RTX (pred_mode), pred_mode);
  create_integer_operand (&ops[2], SVE_KNOWN_PTRUE);
  create_input_operand (&ops[3], op1, data_mode);
  create_input_operand (&ops[4], op2, data_mode);
  expand_insn (icode, 5, ops);
  return ops[0].value;
}

/* Use a comparison to convert integer vector SRC into MODE, which is
   the corresponding SVE predicate mode.  Use TARGET for the result
   if it's nonnull and convenient.  */

rtx
aarch64_convert_sve_data_to_pred (rtx target, machine_mode mode, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  return aarch64_sve_emit_int_cmp (target, mode, NE, src_mode,
				   src, CONST0_RTX (src_mode));
}

/* Return the assembly token for svprfop value PRFOP.  */

static const char *
svprfop_token (enum aarch64_svprfop prfop)
{
  switch (prfop)
    {
#define CASE(UPPER, LOWER, VALUE) case AARCH64_SV_##UPPER: return #LOWER;
    AARCH64_FOR_SVPRFOP (CASE)
#undef CASE
    case AARCH64_NUM_SVPRFOPS:
      break;
    }
  gcc_unreachable ();
}

/* Return the assembly string for an SVE prefetch operation with
   mnemonic MNEMONIC, given that PRFOP_RTX is the prefetch operation
   and that SUFFIX is the format for the remaining operands.  */

char *
aarch64_output_sve_prefetch (const char *mnemonic, rtx prfop_rtx,
			     const char *suffix)
{
  static char buffer[128];
  aarch64_svprfop prfop = (aarch64_svprfop) INTVAL (prfop_rtx);
  unsigned int written = snprintf (buffer, sizeof (buffer), "%s\t%s, %s",
				   mnemonic, svprfop_token (prfop), suffix);
  gcc_assert (written < sizeof (buffer));
  return buffer;
}

/* Check whether we can calculate the number of elements in PATTERN
   at compile time, given that there are NELTS_PER_VQ elements per
   128-bit block.  Return the value if so, otherwise return -1.  */

HOST_WIDE_INT
aarch64_fold_sve_cnt_pat (aarch64_svpattern pattern, unsigned int nelts_per_vq)
{
  unsigned int vl, const_vg;
  if (pattern >= AARCH64_SV_VL1 && pattern <= AARCH64_SV_VL8)
    vl = 1 + (pattern - AARCH64_SV_VL1);
  else if (pattern >= AARCH64_SV_VL16 && pattern <= AARCH64_SV_VL256)
    vl = 16 << (pattern - AARCH64_SV_VL16);
  else if (aarch64_sve_vg.is_constant (&const_vg))
    {
      /* There are two vector granules per quadword.  */
      unsigned int nelts = (const_vg / 2) * nelts_per_vq;
      switch (pattern)
	{
	case AARCH64_SV_POW2: return 1 << floor_log2 (nelts);
	case AARCH64_SV_MUL4: return nelts & -4;
	case AARCH64_SV_MUL3: return (nelts / 3) * 3;
	case AARCH64_SV_ALL: return nelts;
	default: gcc_unreachable ();
	}
    }
  else
    return -1;

  /* There are two vector granules per quadword.  */
  poly_uint64 nelts_all = exact_div (aarch64_sve_vg, 2) * nelts_per_vq;
  if (known_le (vl, nelts_all))
    return vl;

  /* Requesting more elements than are available results in a PFALSE.  */
  if (known_gt (vl, nelts_all))
    return 0;

  return -1;
}

/* Return true if we can move VALUE into a register using a single
   CNT[BHWD] instruction.  */

static bool
aarch64_sve_cnt_immediate_p (poly_int64 value)
{
  HOST_WIDE_INT factor = value.coeffs[0];
  /* The coefficient must be [1, 16] * {2, 4, 8, 16}.  */
  return (value.coeffs[1] == factor
	  && IN_RANGE (factor, 2, 16 * 16)
	  && (factor & 1) == 0
	  && factor <= 16 * (factor & -factor));
}

/* Likewise for rtx X.  */

bool
aarch64_sve_cnt_immediate_p (rtx x)
{
  poly_int64 value;
  return poly_int_rtx_p (x, &value) && aarch64_sve_cnt_immediate_p (value);
}

/* Return the asm string for an instruction with a CNT-like vector size
   operand (a vector pattern followed by a multiplier in the range [1, 16]).
   PREFIX is the mnemonic without the size suffix and OPERANDS is the
   first part of the operands template (the part that comes before the
   vector size itself).  PATTERN is the pattern to use.  FACTOR is the
   number of quadwords.  NELTS_PER_VQ, if nonzero, is the number of elements
   in each quadword.  If it is zero, we can use any element size.  */

static char *
aarch64_output_sve_cnt_immediate (const char *prefix, const char *operands,
				  aarch64_svpattern pattern,
				  unsigned int factor,
				  unsigned int nelts_per_vq)
{
  static char buffer[sizeof ("sqincd\t%x0, %w0, vl256, mul #16")];

  if (nelts_per_vq == 0)
    /* There is some overlap in the ranges of the four CNT instructions.
       Here we always use the smallest possible element size, so that the
       multiplier is 1 whereever possible.  */
    nelts_per_vq = factor & -factor;
  int shift = std::min (exact_log2 (nelts_per_vq), 4);
  gcc_assert (IN_RANGE (shift, 1, 4));
  char suffix = "dwhb"[shift - 1];

  factor >>= shift;
  unsigned int written;
  if (pattern == AARCH64_SV_ALL && factor == 1)
    written = snprintf (buffer, sizeof (buffer), "%s%c\t%s",
			prefix, suffix, operands);
  else if (factor == 1)
    written = snprintf (buffer, sizeof (buffer), "%s%c\t%s, %s",
			prefix, suffix, operands, svpattern_token (pattern));
  else
    written = snprintf (buffer, sizeof (buffer), "%s%c\t%s, %s, mul #%d",
			prefix, suffix, operands, svpattern_token (pattern),
			factor);
  gcc_assert (written < sizeof (buffer));
  return buffer;
}

/* Return the asm string for an instruction with a CNT-like vector size
   operand (a vector pattern followed by a multiplier in the range [1, 16]).
   PREFIX is the mnemonic without the size suffix and OPERANDS is the
   first part of the operands template (the part that comes before the
   vector size itself).  X is the value of the vector size operand,
   as a polynomial integer rtx; we need to convert this into an "all"
   pattern with a multiplier.  */

char *
aarch64_output_sve_cnt_immediate (const char *prefix, const char *operands,
				  rtx x)
{
  poly_int64 value = rtx_to_poly_int64 (x);
  gcc_assert (aarch64_sve_cnt_immediate_p (value));
  return aarch64_output_sve_cnt_immediate (prefix, operands, AARCH64_SV_ALL,
					   value.coeffs[1], 0);
}

/* Return the asm string for an instruction with a CNT-like vector size
   operand (a vector pattern followed by a multiplier in the range [1, 16]).
   PREFIX is the mnemonic without the size suffix and OPERANDS is the
   first part of the operands template (the part that comes before the
   vector size itself).  CNT_PAT[0..2] are the operands of the
   UNSPEC_SVE_CNT_PAT; see aarch64_sve_cnt_pat for details.  */

char *
aarch64_output_sve_cnt_pat_immediate (const char *prefix,
				      const char *operands, rtx *cnt_pat)
{
  aarch64_svpattern pattern = (aarch64_svpattern) INTVAL (cnt_pat[0]);
  unsigned int nelts_per_vq = INTVAL (cnt_pat[1]);
  unsigned int factor = INTVAL (cnt_pat[2]) * nelts_per_vq;
  return aarch64_output_sve_cnt_immediate (prefix, operands, pattern,
					   factor, nelts_per_vq);
}

/* Return true if we can add X using a single SVE INC or DEC instruction.  */

bool
aarch64_sve_scalar_inc_dec_immediate_p (rtx x)
{
  poly_int64 value;
  return (poly_int_rtx_p (x, &value)
	  && (aarch64_sve_cnt_immediate_p (value)
	      || aarch64_sve_cnt_immediate_p (-value)));
}

/* Return the asm string for adding SVE INC/DEC immediate OFFSET to
   operand 0.  */

char *
aarch64_output_sve_scalar_inc_dec (rtx offset)
{
  poly_int64 offset_value = rtx_to_poly_int64 (offset);
  gcc_assert (offset_value.coeffs[0] == offset_value.coeffs[1]);
  if (offset_value.coeffs[1] > 0)
    return aarch64_output_sve_cnt_immediate ("inc", "%x0", AARCH64_SV_ALL,
					     offset_value.coeffs[1], 0);
  else
    return aarch64_output_sve_cnt_immediate ("dec", "%x0", AARCH64_SV_ALL,
					     -offset_value.coeffs[1], 0);
}

/* Return true if we can add VALUE to a register using a single ADDVL
   or ADDPL instruction.  */

static bool
aarch64_sve_addvl_addpl_immediate_p (poly_int64 value)
{
  HOST_WIDE_INT factor = value.coeffs[0];
  if (factor == 0 || value.coeffs[1] != factor)
    return false;
  /* FACTOR counts VG / 2, so a value of 2 is one predicate width
     and a value of 16 is one vector width.  */
  return (((factor & 15) == 0 && IN_RANGE (factor, -32 * 16, 31 * 16))
	  || ((factor & 1) == 0 && IN_RANGE (factor, -32 * 2, 31 * 2)));
}

/* Likewise for rtx X.  */

bool
aarch64_sve_addvl_addpl_immediate_p (rtx x)
{
  poly_int64 value;
  return (poly_int_rtx_p (x, &value)
	  && aarch64_sve_addvl_addpl_immediate_p (value));
}

/* Return the asm string for adding ADDVL or ADDPL immediate OFFSET
   to operand 1 and storing the result in operand 0.  */

char *
aarch64_output_sve_addvl_addpl (rtx offset)
{
  static char buffer[sizeof ("addpl\t%x0, %x1, #-") + 3 * sizeof (int)];
  poly_int64 offset_value = rtx_to_poly_int64 (offset);
  gcc_assert (aarch64_sve_addvl_addpl_immediate_p (offset_value));

  int factor = offset_value.coeffs[1];
  if ((factor & 15) == 0)
    snprintf (buffer, sizeof (buffer), "addvl\t%%x0, %%x1, #%d", factor / 16);
  else
    snprintf (buffer, sizeof (buffer), "addpl\t%%x0, %%x1, #%d", factor / 2);
  return buffer;
}

/* Return true if X is a valid immediate for an SVE vector INC or DEC
   instruction.  If it is, store the number of elements in each vector
   quadword in *NELTS_PER_VQ_OUT (if nonnull) and store the multiplication
   factor in *FACTOR_OUT (if nonnull).  */

bool
aarch64_sve_vector_inc_dec_immediate_p (rtx x, int *factor_out,
					unsigned int *nelts_per_vq_out)
{
  rtx elt;
  poly_int64 value;

  if (!const_vec_duplicate_p (x, &elt)
      || !poly_int_rtx_p (elt, &value))
    return false;

  unsigned int nelts_per_vq = 128 / GET_MODE_UNIT_BITSIZE (GET_MODE (x));
  if (nelts_per_vq != 8 && nelts_per_vq != 4 && nelts_per_vq != 2)
    /* There's no vector INCB.  */
    return false;

  HOST_WIDE_INT factor = value.coeffs[0];
  if (value.coeffs[1] != factor)
    return false;

  /* The coefficient must be [1, 16] * NELTS_PER_VQ.  */
  if ((factor % nelts_per_vq) != 0
      || !IN_RANGE (abs (factor), nelts_per_vq, 16 * nelts_per_vq))
    return false;

  if (factor_out)
    *factor_out = factor;
  if (nelts_per_vq_out)
    *nelts_per_vq_out = nelts_per_vq;
  return true;
}

/* Return true if X is a valid immediate for an SVE vector INC or DEC
   instruction.  */

bool
aarch64_sve_vector_inc_dec_immediate_p (rtx x)
{
  return aarch64_sve_vector_inc_dec_immediate_p (x, NULL, NULL);
}

/* Return the asm template for an SVE vector INC or DEC instruction.
   OPERANDS gives the operands before the vector count and X is the
   value of the vector count operand itself.  */

char *
aarch64_output_sve_vector_inc_dec (const char *operands, rtx x)
{
  int factor;
  unsigned int nelts_per_vq;
  if (!aarch64_sve_vector_inc_dec_immediate_p (x, &factor, &nelts_per_vq))
    gcc_unreachable ();
  if (factor < 0)
    return aarch64_output_sve_cnt_immediate ("dec", operands, AARCH64_SV_ALL,
					     -factor, nelts_per_vq);
  else
    return aarch64_output_sve_cnt_immediate ("inc", operands, AARCH64_SV_ALL,
					     factor, nelts_per_vq);
}

static int
aarch64_internal_mov_immediate (rtx dest, rtx imm, bool generate,
				scalar_int_mode mode)
{
  int i;
  unsigned HOST_WIDE_INT val, val2, mask;
  int one_match, zero_match;
  int num_insns;

  val = INTVAL (imm);

  if (aarch64_move_imm (val, mode))
    {
      if (generate)
	emit_insn (gen_rtx_SET (dest, imm));
      return 1;
    }

  /* Check to see if the low 32 bits are either 0xffffXXXX or 0xXXXXffff
     (with XXXX non-zero). In that case check to see if the move can be done in
     a smaller mode.  */
  val2 = val & 0xffffffff;
  if (mode == DImode
      && aarch64_move_imm (val2, SImode)
      && (((val >> 32) & 0xffff) == 0 || (val >> 48) == 0))
    {
      if (generate)
	emit_insn (gen_rtx_SET (dest, GEN_INT (val2)));

      /* Check if we have to emit a second instruction by checking to see
         if any of the upper 32 bits of the original DI mode value is set.  */
      if (val == val2)
	return 1;

      i = (val >> 48) ? 48 : 32;

      if (generate)
	 emit_insn (gen_insv_immdi (dest, GEN_INT (i),
				    GEN_INT ((val >> i) & 0xffff)));

      return 2;
    }

  if ((val >> 32) == 0 || mode == SImode)
    {
      if (generate)
	{
	  emit_insn (gen_rtx_SET (dest, GEN_INT (val & 0xffff)));
	  if (mode == SImode)
	    emit_insn (gen_insv_immsi (dest, GEN_INT (16),
				       GEN_INT ((val >> 16) & 0xffff)));
	  else
	    emit_insn (gen_insv_immdi (dest, GEN_INT (16),
				       GEN_INT ((val >> 16) & 0xffff)));
	}
      return 2;
    }

  /* Remaining cases are all for DImode.  */

  mask = 0xffff;
  zero_match = ((val & mask) == 0) + ((val & (mask << 16)) == 0) +
    ((val & (mask << 32)) == 0) + ((val & (mask << 48)) == 0);
  one_match = ((~val & mask) == 0) + ((~val & (mask << 16)) == 0) +
    ((~val & (mask << 32)) == 0) + ((~val & (mask << 48)) == 0);

  if (zero_match != 2 && one_match != 2)
    {
      /* Try emitting a bitmask immediate with a movk replacing 16 bits.
	 For a 64-bit bitmask try whether changing 16 bits to all ones or
	 zeroes creates a valid bitmask.  To check any repeated bitmask,
	 try using 16 bits from the other 32-bit half of val.  */

      for (i = 0; i < 64; i += 16, mask <<= 16)
	{
	  val2 = val & ~mask;
	  if (val2 != val && aarch64_bitmask_imm (val2, mode))
	    break;
	  val2 = val | mask;
	  if (val2 != val && aarch64_bitmask_imm (val2, mode))
	    break;
	  val2 = val2 & ~mask;
	  val2 = val2 | (((val2 >> 32) | (val2 << 32)) & mask);
	  if (val2 != val && aarch64_bitmask_imm (val2, mode))
	    break;
	}
      if (i != 64)
	{
	  if (generate)
	    {
	      emit_insn (gen_rtx_SET (dest, GEN_INT (val2)));
	      emit_insn (gen_insv_immdi (dest, GEN_INT (i),
					 GEN_INT ((val >> i) & 0xffff)));
	    }
	  return 2;
	}
    }

  /* Generate 2-4 instructions, skipping 16 bits of all zeroes or ones which
     are emitted by the initial mov.  If one_match > zero_match, skip set bits,
     otherwise skip zero bits.  */

  num_insns = 1;
  mask = 0xffff;
  val2 = one_match > zero_match ? ~val : val;
  i = (val2 & mask) != 0 ? 0 : (val2 & (mask << 16)) != 0 ? 16 : 32;

  if (generate)
    emit_insn (gen_rtx_SET (dest, GEN_INT (one_match > zero_match
					   ? (val | ~(mask << i))
					   : (val & (mask << i)))));
  for (i += 16; i < 64; i += 16)
    {
      if ((val2 & (mask << i)) == 0)
	continue;
      if (generate)
	emit_insn (gen_insv_immdi (dest, GEN_INT (i),
				   GEN_INT ((val >> i) & 0xffff)));
      num_insns ++;
    }

  return num_insns;
}

/* Return whether imm is a 128-bit immediate which is simple enough to
   expand inline.  */
bool
aarch64_mov128_immediate (rtx imm)
{
  if (GET_CODE (imm) == CONST_INT)
    return true;

  gcc_assert (CONST_WIDE_INT_NUNITS (imm) == 2);

  rtx lo = GEN_INT (CONST_WIDE_INT_ELT (imm, 0));
  rtx hi = GEN_INT (CONST_WIDE_INT_ELT (imm, 1));

  return aarch64_internal_mov_immediate (NULL_RTX, lo, false, DImode)
	 + aarch64_internal_mov_immediate (NULL_RTX, hi, false, DImode) <= 4;
}


/* Return the number of temporary registers that aarch64_add_offset_1
   would need to add OFFSET to a register.  */

static unsigned int
aarch64_add_offset_1_temporaries (HOST_WIDE_INT offset)
{
  return abs_hwi (offset) < 0x1000000 ? 0 : 1;
}

/* A subroutine of aarch64_add_offset.  Set DEST to SRC + OFFSET for
   a non-polynomial OFFSET.  MODE is the mode of the addition.
   FRAME_RELATED_P is true if the RTX_FRAME_RELATED flag should
   be set and CFA adjustments added to the generated instructions.

   TEMP1, if nonnull, is a register of mode MODE that can be used as a
   temporary if register allocation is already complete.  This temporary
   register may overlap DEST but must not overlap SRC.  If TEMP1 is known
   to hold abs (OFFSET), EMIT_MOVE_IMM can be set to false to avoid emitting
   the immediate again.

   Since this function may be used to adjust the stack pointer, we must
   ensure that it cannot cause transient stack deallocation (for example
   by first incrementing SP and then decrementing when adjusting by a
   large immediate).  */

static void
aarch64_add_offset_1 (scalar_int_mode mode, rtx dest,
		      rtx src, HOST_WIDE_INT offset, rtx temp1,
		      bool frame_related_p, bool emit_move_imm)
{
  gcc_assert (emit_move_imm || temp1 != NULL_RTX);
  gcc_assert (temp1 == NULL_RTX || !reg_overlap_mentioned_p (temp1, src));

  HOST_WIDE_INT moffset = abs_hwi (offset);
  rtx_insn *insn;

  if (!moffset)
    {
      if (!rtx_equal_p (dest, src))
	{
	  insn = emit_insn (gen_rtx_SET (dest, src));
	  RTX_FRAME_RELATED_P (insn) = frame_related_p;
	}
      return;
    }

  /* Single instruction adjustment.  */
  if (aarch64_uimm12_shift (moffset))
    {
      insn = emit_insn (gen_add3_insn (dest, src, GEN_INT (offset)));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      return;
    }

  /* Emit 2 additions/subtractions if the adjustment is less than 24 bits
     and either:

     a) the offset cannot be loaded by a 16-bit move or
     b) there is no spare register into which we can move it.  */
  if (moffset < 0x1000000
      && ((!temp1 && !can_create_pseudo_p ())
	  || !aarch64_move_imm (moffset, mode)))
    {
      HOST_WIDE_INT low_off = moffset & 0xfff;

      low_off = offset < 0 ? -low_off : low_off;
      insn = emit_insn (gen_add3_insn (dest, src, GEN_INT (low_off)));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      insn = emit_insn (gen_add2_insn (dest, GEN_INT (offset - low_off)));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      return;
    }

  /* Emit a move immediate if required and an addition/subtraction.  */
  if (emit_move_imm)
    {
      gcc_assert (temp1 != NULL_RTX || can_create_pseudo_p ());
      temp1 = aarch64_force_temporary (mode, temp1, GEN_INT (moffset));
    }
  insn = emit_insn (offset < 0
		    ? gen_sub3_insn (dest, src, temp1)
		    : gen_add3_insn (dest, src, temp1));
  if (frame_related_p)
    {
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      rtx adj = plus_constant (mode, src, offset);
      add_reg_note (insn, REG_CFA_ADJUST_CFA, gen_rtx_SET (dest, adj));
    }
}

/* Return the number of temporary registers that aarch64_add_offset
   would need to move OFFSET into a register or add OFFSET to a register;
   ADD_P is true if we want the latter rather than the former.  */

static unsigned int
aarch64_offset_temporaries (bool add_p, poly_int64 offset)
{
  /* This follows the same structure as aarch64_add_offset.  */
  if (add_p && aarch64_sve_addvl_addpl_immediate_p (offset))
    return 0;

  unsigned int count = 0;
  HOST_WIDE_INT factor = offset.coeffs[1];
  HOST_WIDE_INT constant = offset.coeffs[0] - factor;
  poly_int64 poly_offset (factor, factor);
  if (add_p && aarch64_sve_addvl_addpl_immediate_p (poly_offset))
    /* Need one register for the ADDVL/ADDPL result.  */
    count += 1;
  else if (factor != 0)
    {
      factor = abs (factor);
      if (factor > 16 * (factor & -factor))
	/* Need one register for the CNT result and one for the multiplication
	   factor.  If necessary, the second temporary can be reused for the
	   constant part of the offset.  */
	return 2;
      /* Need one register for the CNT result (which might then
	 be shifted).  */
      count += 1;
    }
  return count + aarch64_add_offset_1_temporaries (constant);
}

/* If X can be represented as a poly_int64, return the number
   of temporaries that are required to add it to a register.
   Return -1 otherwise.  */

int
aarch64_add_offset_temporaries (rtx x)
{
  poly_int64 offset;
  if (!poly_int_rtx_p (x, &offset))
    return -1;
  return aarch64_offset_temporaries (true, offset);
}

/* Set DEST to SRC + OFFSET.  MODE is the mode of the addition.
   FRAME_RELATED_P is true if the RTX_FRAME_RELATED flag should
   be set and CFA adjustments added to the generated instructions.

   TEMP1, if nonnull, is a register of mode MODE that can be used as a
   temporary if register allocation is already complete.  This temporary
   register may overlap DEST if !FRAME_RELATED_P but must not overlap SRC.
   If TEMP1 is known to hold abs (OFFSET), EMIT_MOVE_IMM can be set to
   false to avoid emitting the immediate again.

   TEMP2, if nonnull, is a second temporary register that doesn't
   overlap either DEST or REG.

   Since this function may be used to adjust the stack pointer, we must
   ensure that it cannot cause transient stack deallocation (for example
   by first incrementing SP and then decrementing when adjusting by a
   large immediate).  */

static void
aarch64_add_offset (scalar_int_mode mode, rtx dest, rtx src,
		    poly_int64 offset, rtx temp1, rtx temp2,
		    bool frame_related_p, bool emit_move_imm = true)
{
  gcc_assert (emit_move_imm || temp1 != NULL_RTX);
  gcc_assert (temp1 == NULL_RTX || !reg_overlap_mentioned_p (temp1, src));
  gcc_assert (temp1 == NULL_RTX
	      || !frame_related_p
	      || !reg_overlap_mentioned_p (temp1, dest));
  gcc_assert (temp2 == NULL_RTX || !reg_overlap_mentioned_p (dest, temp2));

  /* Try using ADDVL or ADDPL to add the whole value.  */
  if (src != const0_rtx && aarch64_sve_addvl_addpl_immediate_p (offset))
    {
      rtx offset_rtx = gen_int_mode (offset, mode);
      rtx_insn *insn = emit_insn (gen_add3_insn (dest, src, offset_rtx));
      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      return;
    }

  /* Coefficient 1 is multiplied by the number of 128-bit blocks in an
     SVE vector register, over and above the minimum size of 128 bits.
     This is equivalent to half the value returned by CNTD with a
     vector shape of ALL.  */
  HOST_WIDE_INT factor = offset.coeffs[1];
  HOST_WIDE_INT constant = offset.coeffs[0] - factor;

  /* Try using ADDVL or ADDPL to add the VG-based part.  */
  poly_int64 poly_offset (factor, factor);
  if (src != const0_rtx
      && aarch64_sve_addvl_addpl_immediate_p (poly_offset))
    {
      rtx offset_rtx = gen_int_mode (poly_offset, mode);
      if (frame_related_p)
	{
	  rtx_insn *insn = emit_insn (gen_add3_insn (dest, src, offset_rtx));
	  RTX_FRAME_RELATED_P (insn) = true;
	  src = dest;
	}
      else
	{
	  rtx addr = gen_rtx_PLUS (mode, src, offset_rtx);
	  src = aarch64_force_temporary (mode, temp1, addr);
	  temp1 = temp2;
	  temp2 = NULL_RTX;
	}
    }
  /* Otherwise use a CNT-based sequence.  */
  else if (factor != 0)
    {
      /* Use a subtraction if we have a negative factor.  */
      rtx_code code = PLUS;
      if (factor < 0)
	{
	  factor = -factor;
	  code = MINUS;
	}

      /* Calculate CNTD * FACTOR / 2.  First try to fold the division
	 into the multiplication.  */
      rtx val;
      int shift = 0;
      if (factor & 1)
	/* Use a right shift by 1.  */
	shift = -1;
      else
	factor /= 2;
      HOST_WIDE_INT low_bit = factor & -factor;
      if (factor <= 16 * low_bit)
	{
	  if (factor > 16 * 8)
	    {
	      /* "CNTB Xn, ALL, MUL #FACTOR" is out of range, so calculate
		 the value with the minimum multiplier and shift it into
		 position.  */
	      int extra_shift = exact_log2 (low_bit);
	      shift += extra_shift;
	      factor >>= extra_shift;
	    }
	  val = gen_int_mode (poly_int64 (factor * 2, factor * 2), mode);
	}
      else
	{
	  /* Base the factor on LOW_BIT if we can calculate LOW_BIT
	     directly, since that should increase the chances of being
	     able to use a shift and add sequence.  If LOW_BIT itself
	     is out of range, just use CNTD.  */
	  if (low_bit <= 16 * 8)
	    factor /= low_bit;
	  else
	    low_bit = 1;

	  val = gen_int_mode (poly_int64 (low_bit * 2, low_bit * 2), mode);
	  val = aarch64_force_temporary (mode, temp1, val);

	  if (can_create_pseudo_p ())
	    {
	      rtx coeff1 = gen_int_mode (factor, mode);
	      val = expand_mult (mode, val, coeff1, NULL_RTX, false, true);
	    }
	  else
	    {
	      /* Go back to using a negative multiplication factor if we have
		 no register from which to subtract.  */
	      if (code == MINUS && src == const0_rtx)
		{
		  factor = -factor;
		  code = PLUS;
		}
	      rtx coeff1 = gen_int_mode (factor, mode);
	      coeff1 = aarch64_force_temporary (mode, temp2, coeff1);
	      val = gen_rtx_MULT (mode, val, coeff1);
	    }
	}

      if (shift > 0)
	{
	  /* Multiply by 1 << SHIFT.  */
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_ASHIFT (mode, val, GEN_INT (shift));
	}
      else if (shift == -1)
	{
	  /* Divide by 2.  */
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_ASHIFTRT (mode, val, const1_rtx);
	}

      /* Calculate SRC +/- CNTD * FACTOR / 2.  */
      if (src != const0_rtx)
	{
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_fmt_ee (code, mode, src, val);
	}
      else if (code == MINUS)
	{
	  val = aarch64_force_temporary (mode, temp1, val);
	  val = gen_rtx_NEG (mode, val);
	}

      if (constant == 0 || frame_related_p)
	{
	  rtx_insn *insn = emit_insn (gen_rtx_SET (dest, val));
	  if (frame_related_p)
	    {
	      RTX_FRAME_RELATED_P (insn) = true;
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (dest, plus_constant (Pmode, src,
							      poly_offset)));
	    }
	  src = dest;
	  if (constant == 0)
	    return;
	}
      else
	{
	  src = aarch64_force_temporary (mode, temp1, val);
	  temp1 = temp2;
	  temp2 = NULL_RTX;
	}

      emit_move_imm = true;
    }

  aarch64_add_offset_1 (mode, dest, src, constant, temp1,
			frame_related_p, emit_move_imm);
}

/* Like aarch64_add_offset, but the offset is given as an rtx rather
   than a poly_int64.  */

void
aarch64_split_add_offset (scalar_int_mode mode, rtx dest, rtx src,
			  rtx offset_rtx, rtx temp1, rtx temp2)
{
  aarch64_add_offset (mode, dest, src, rtx_to_poly_int64 (offset_rtx),
		      temp1, temp2, false);
}

/* Add DELTA to the stack pointer, marking the instructions frame-related.
   TEMP1 is available as a temporary if nonnull.  EMIT_MOVE_IMM is false
   if TEMP1 already contains abs (DELTA).  */

static inline void
aarch64_add_sp (rtx temp1, rtx temp2, poly_int64 delta, bool emit_move_imm)
{
  aarch64_add_offset (Pmode, stack_pointer_rtx, stack_pointer_rtx, delta,
		      temp1, temp2, true, emit_move_imm);
}

/* Subtract DELTA from the stack pointer, marking the instructions
   frame-related if FRAME_RELATED_P.  TEMP1 is available as a temporary
   if nonnull.  */

static inline void
aarch64_sub_sp (rtx temp1, rtx temp2, poly_int64 delta, bool frame_related_p,
		bool emit_move_imm = true)
{
  aarch64_add_offset (Pmode, stack_pointer_rtx, stack_pointer_rtx, -delta,
		      temp1, temp2, frame_related_p, emit_move_imm);
}

/* Set DEST to (vec_series BASE STEP).  */

static void
aarch64_expand_vec_series (rtx dest, rtx base, rtx step)
{
  machine_mode mode = GET_MODE (dest);
  scalar_mode inner = GET_MODE_INNER (mode);

  /* Each operand can be a register or an immediate in the range [-16, 15].  */
  if (!aarch64_sve_index_immediate_p (base))
    base = force_reg (inner, base);
  if (!aarch64_sve_index_immediate_p (step))
    step = force_reg (inner, step);

  emit_set_insn (dest, gen_rtx_VEC_SERIES (mode, base, step));
}

/* Duplicate 128-bit Advanced SIMD vector SRC so that it fills an SVE
   register of mode MODE.  Use TARGET for the result if it's nonnull
   and convenient.

   The two vector modes must have the same element mode.  The behavior
   is to duplicate architectural lane N of SRC into architectural lanes
   N + I * STEP of the result.  On big-endian targets, architectural
   lane 0 of an Advanced SIMD vector is the last element of the vector
   in memory layout, so for big-endian targets this operation has the
   effect of reversing SRC before duplicating it.  Callers need to
   account for this.  */

rtx
aarch64_expand_sve_dupq (rtx target, machine_mode mode, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  gcc_assert (GET_MODE_INNER (mode) == GET_MODE_INNER (src_mode));
  insn_code icode = (BYTES_BIG_ENDIAN
		     ? code_for_aarch64_vec_duplicate_vq_be (mode)
		     : code_for_aarch64_vec_duplicate_vq_le (mode));

  unsigned int i = 0;
  expand_operand ops[3];
  create_output_operand (&ops[i++], target, mode);
  create_output_operand (&ops[i++], src, src_mode);
  if (BYTES_BIG_ENDIAN)
    {
      /* Create a PARALLEL describing the reversal of SRC.  */
      unsigned int nelts_per_vq = 128 / GET_MODE_UNIT_BITSIZE (mode);
      rtx sel = aarch64_gen_stepped_int_parallel (nelts_per_vq,
						  nelts_per_vq - 1, -1);
      create_fixed_operand (&ops[i++], sel);
    }
  expand_insn (icode, i, ops);
  return ops[0].value;
}

/* Try to force 128-bit vector value SRC into memory and use LD1RQ to fetch
   the memory image into DEST.  Return true on success.  */

static bool
aarch64_expand_sve_ld1rq (rtx dest, rtx src)
{
  src = force_const_mem (GET_MODE (src), src);
  if (!src)
    return false;

  /* Make sure that the address is legitimate.  */
  if (!aarch64_sve_ld1rq_operand_p (src))
    {
      rtx addr = force_reg (Pmode, XEXP (src, 0));
      src = replace_equiv_address (src, addr);
    }

  machine_mode mode = GET_MODE (dest);
  machine_mode pred_mode = aarch64_sve_pred_mode (mode);
  rtx ptrue = aarch64_ptrue_reg (pred_mode);
  emit_insn (gen_aarch64_sve_ld1rq (mode, dest, src, ptrue));
  return true;
}

/* Return a register containing CONST_VECTOR SRC, given that SRC has an
   SVE data mode and isn't a legitimate constant.  Use TARGET for the
   result if convenient.

   The returned register can have whatever mode seems most natural
   given the contents of SRC.  */

static rtx
aarch64_expand_sve_const_vector (rtx target, rtx src)
{
  machine_mode mode = GET_MODE (src);
  unsigned int npatterns = CONST_VECTOR_NPATTERNS (src);
  unsigned int nelts_per_pattern = CONST_VECTOR_NELTS_PER_PATTERN (src);
  scalar_mode elt_mode = GET_MODE_INNER (mode);
  unsigned int elt_bits = GET_MODE_BITSIZE (elt_mode);
  unsigned int container_bits = aarch64_sve_container_bits (mode);
  unsigned int encoded_bits = npatterns * nelts_per_pattern * container_bits;

  if (nelts_per_pattern == 1
      && encoded_bits <= 128
      && container_bits != elt_bits)
    {
      /* We have a partial vector mode and a constant whose full-vector
	 equivalent would occupy a repeating 128-bit sequence.  Build that
	 full-vector equivalent instead, so that we have the option of
	 using LD1RQ and Advanced SIMD operations.  */
      unsigned int repeat = container_bits / elt_bits;
      machine_mode full_mode = aarch64_full_sve_mode (elt_mode).require ();
      rtx_vector_builder builder (full_mode, npatterns * repeat, 1);
      for (unsigned int i = 0; i < npatterns; ++i)
	for (unsigned int j = 0; j < repeat; ++j)
	  builder.quick_push (CONST_VECTOR_ENCODED_ELT (src, i));
      target = aarch64_target_reg (target, full_mode);
      return aarch64_expand_sve_const_vector (target, builder.build ());
    }

  if (nelts_per_pattern == 1 && encoded_bits == 128)
    {
      /* The constant is a duplicated quadword but can't be narrowed
	 beyond a quadword.  Get the memory image of the first quadword
	 as a 128-bit vector and try using LD1RQ to load it from memory.

	 The effect for both endiannesses is to load memory lane N into
	 architectural lanes N + I * STEP of the result.  On big-endian
	 targets, the layout of the 128-bit vector in an Advanced SIMD
	 register would be different from its layout in an SVE register,
	 but this 128-bit vector is a memory value only.  */
      machine_mode vq_mode = aarch64_vq_mode (elt_mode).require ();
      rtx vq_value = simplify_gen_subreg (vq_mode, src, mode, 0);
      if (vq_value && aarch64_expand_sve_ld1rq (target, vq_value))
	return target;
    }

  if (nelts_per_pattern == 1 && encoded_bits < 128)
    {
      /* The vector is a repeating sequence of 64 bits or fewer.
	 See if we can load them using an Advanced SIMD move and then
	 duplicate it to fill a vector.  This is better than using a GPR
	 move because it keeps everything in the same register file.  */
      machine_mode vq_mode = aarch64_vq_mode (elt_mode).require ();
      rtx_vector_builder builder (vq_mode, npatterns, 1);
      for (unsigned int i = 0; i < npatterns; ++i)
	{
	  /* We want memory lane N to go into architectural lane N,
	     so reverse for big-endian targets.  The DUP .Q pattern
	     has a compensating reverse built-in.  */
	  unsigned int srci = BYTES_BIG_ENDIAN ? npatterns - i - 1 : i;
	  builder.quick_push (CONST_VECTOR_ENCODED_ELT (src, srci));
	}
      rtx vq_src = builder.build ();
      if (aarch64_simd_valid_immediate (vq_src, NULL))
	{
	  vq_src = force_reg (vq_mode, vq_src);
	  return aarch64_expand_sve_dupq (target, mode, vq_src);
	}

      /* Get an integer representation of the repeating part of Advanced
	 SIMD vector VQ_SRC.  This preserves the endianness of VQ_SRC,
	 which for big-endian targets is lane-swapped wrt a normal
	 Advanced SIMD vector.  This means that for both endiannesses,
	 memory lane N of SVE vector SRC corresponds to architectural
	 lane N of a register holding VQ_SRC.  This in turn means that
	 memory lane 0 of SVE vector SRC is in the lsb of VQ_SRC (viewed
	 as a single 128-bit value) and thus that memory lane 0 of SRC is
	 in the lsb of the integer.  Duplicating the integer therefore
	 ensures that memory lane N of SRC goes into architectural lane
	 N + I * INDEX of the SVE register.  */
      scalar_mode int_mode = int_mode_for_size (encoded_bits, 0).require ();
      rtx elt_value = simplify_gen_subreg (int_mode, vq_src, vq_mode, 0);
      if (elt_value)
	{
	  /* Pretend that we had a vector of INT_MODE to start with.  */
	  elt_mode = int_mode;
	  mode = aarch64_full_sve_mode (int_mode).require ();

	  /* If the integer can be moved into a general register by a
	     single instruction, do that and duplicate the result.  */
	  if (CONST_INT_P (elt_value)
	      && aarch64_move_imm (INTVAL (elt_value), elt_mode))
	    {
	      elt_value = force_reg (elt_mode, elt_value);
	      return expand_vector_broadcast (mode, elt_value);
	    }
	}
      else if (npatterns == 1)
	/* We're duplicating a single value, but can't do better than
	   force it to memory and load from there.  This handles things
	   like symbolic constants.  */
	elt_value = CONST_VECTOR_ENCODED_ELT (src, 0);

      if (elt_value)
	{
	  /* Load the element from memory if we can, otherwise move it into
	     a register and use a DUP.  */
	  rtx op = force_const_mem (elt_mode, elt_value);
	  if (!op)
	    op = force_reg (elt_mode, elt_value);
	  return expand_vector_broadcast (mode, op);
	}
    }

  /* Try using INDEX.  */
  rtx base, step;
  if (const_vec_series_p (src, &base, &step))
    {
      aarch64_expand_vec_series (target, base, step);
      return target;
    }

  /* From here on, it's better to force the whole constant to memory
     if we can.  */
  if (GET_MODE_NUNITS (mode).is_constant ())
    return NULL_RTX;

  /* Expand each pattern individually.  */
  gcc_assert (npatterns > 1);
  rtx_vector_builder builder;
  auto_vec<rtx, 16> vectors (npatterns);
  for (unsigned int i = 0; i < npatterns; ++i)
    {
      builder.new_vector (mode, 1, nelts_per_pattern);
      for (unsigned int j = 0; j < nelts_per_pattern; ++j)
	builder.quick_push (CONST_VECTOR_ELT (src, i + j * npatterns));
      vectors.quick_push (force_reg (mode, builder.build ()));
    }

  /* Use permutes to interleave the separate vectors.  */
  while (npatterns > 1)
    {
      npatterns /= 2;
      for (unsigned int i = 0; i < npatterns; ++i)
	{
	  rtx tmp = (npatterns == 1 ? target : gen_reg_rtx (mode));
	  rtvec v = gen_rtvec (2, vectors[i], vectors[i + npatterns]);
	  emit_set_insn (tmp, gen_rtx_UNSPEC (mode, v, UNSPEC_ZIP1));
	  vectors[i] = tmp;
	}
    }
  gcc_assert (vectors[0] == target);
  return target;
}

/* Use WHILE to set a predicate register of mode MODE in which the first
   VL bits are set and the rest are clear.  Use TARGET for the register
   if it's nonnull and convenient.  */

static rtx
aarch64_sve_move_pred_via_while (rtx target, machine_mode mode,
				 unsigned int vl)
{
  rtx limit = force_reg (DImode, gen_int_mode (vl, DImode));
  target = aarch64_target_reg (target, mode);
  emit_insn (gen_while (UNSPEC_WHILE_LO, DImode, mode,
			target, const0_rtx, limit));
  return target;
}

static rtx
aarch64_expand_sve_const_pred_1 (rtx, rtx_vector_builder &, bool);

/* BUILDER is a constant predicate in which the index of every set bit
   is a multiple of ELT_SIZE (which is <= 8).  Try to load the constant
   by inverting every element at a multiple of ELT_SIZE and EORing the
   result with an ELT_SIZE PTRUE.

   Return a register that contains the constant on success, otherwise
   return null.  Use TARGET as the register if it is nonnull and
   convenient.  */

static rtx
aarch64_expand_sve_const_pred_eor (rtx target, rtx_vector_builder &builder,
				   unsigned int elt_size)
{
  /* Invert every element at a multiple of ELT_SIZE, keeping the
     other bits zero.  */
  rtx_vector_builder inv_builder (VNx16BImode, builder.npatterns (),
				  builder.nelts_per_pattern ());
  for (unsigned int i = 0; i < builder.encoded_nelts (); ++i)
    if ((i & (elt_size - 1)) == 0 && INTVAL (builder.elt (i)) == 0)
      inv_builder.quick_push (const1_rtx);
    else
      inv_builder.quick_push (const0_rtx);
  inv_builder.finalize ();

  /* See if we can load the constant cheaply.  */
  rtx inv = aarch64_expand_sve_const_pred_1 (NULL_RTX, inv_builder, false);
  if (!inv)
    return NULL_RTX;

  /* EOR the result with an ELT_SIZE PTRUE.  */
  rtx mask = aarch64_ptrue_all (elt_size);
  mask = force_reg (VNx16BImode, mask);
  target = aarch64_target_reg (target, VNx16BImode);
  emit_insn (gen_aarch64_pred_z (XOR, VNx16BImode, target, mask, inv, mask));
  return target;
}

/* BUILDER is a constant predicate in which the index of every set bit
   is a multiple of ELT_SIZE (which is <= 8).  Try to load the constant
   using a TRN1 of size PERMUTE_SIZE, which is >= ELT_SIZE.  Return the
   register on success, otherwise return null.  Use TARGET as the register
   if nonnull and convenient.  */

static rtx
aarch64_expand_sve_const_pred_trn (rtx target, rtx_vector_builder &builder,
				   unsigned int elt_size,
				   unsigned int permute_size)
{
  /* We're going to split the constant into two new constants A and B,
     with element I of BUILDER going into A if (I & PERMUTE_SIZE) == 0
     and into B otherwise.  E.g. for PERMUTE_SIZE == 4 && ELT_SIZE == 1:

     A: { 0, 1, 2, 3, _, _, _, _, 8, 9, 10, 11, _, _, _, _ }
     B: { 4, 5, 6, 7, _, _, _, _, 12, 13, 14, 15, _, _, _, _ }

     where _ indicates elements that will be discarded by the permute.

     First calculate the ELT_SIZEs for A and B.  */
  unsigned int a_elt_size = GET_MODE_SIZE (DImode);
  unsigned int b_elt_size = GET_MODE_SIZE (DImode);
  for (unsigned int i = 0; i < builder.encoded_nelts (); i += elt_size)
    if (INTVAL (builder.elt (i)) != 0)
      {
	if (i & permute_size)
	  b_elt_size |= i - permute_size;
	else
	  a_elt_size |= i;
      }
  a_elt_size &= -a_elt_size;
  b_elt_size &= -b_elt_size;

  /* Now construct the vectors themselves.  */
  rtx_vector_builder a_builder (VNx16BImode, builder.npatterns (),
				builder.nelts_per_pattern ());
  rtx_vector_builder b_builder (VNx16BImode, builder.npatterns (),
				builder.nelts_per_pattern ());
  unsigned int nelts = builder.encoded_nelts ();
  for (unsigned int i = 0; i < nelts; ++i)
    if (i & (elt_size - 1))
      {
	a_builder.quick_push (const0_rtx);
	b_builder.quick_push (const0_rtx);
      }
    else if ((i & permute_size) == 0)
      {
	/* The A and B elements are significant.  */
	a_builder.quick_push (builder.elt (i));
	b_builder.quick_push (builder.elt (i + permute_size));
      }
    else
      {
	/* The A and B elements are going to be discarded, so pick whatever
	   is likely to give a nice constant.  We are targeting element
	   sizes A_ELT_SIZE and B_ELT_SIZE for A and B respectively,
	   with the aim of each being a sequence of ones followed by
	   a sequence of zeros.  So:

	   * if X_ELT_SIZE <= PERMUTE_SIZE, the best approach is to
	     duplicate the last X_ELT_SIZE element, to extend the
	     current sequence of ones or zeros.

	   * if X_ELT_SIZE > PERMUTE_SIZE, the best approach is to add a
	     zero, so that the constant really does have X_ELT_SIZE and
	     not a smaller size.  */
	if (a_elt_size > permute_size)
	  a_builder.quick_push (const0_rtx);
	else
	  a_builder.quick_push (a_builder.elt (i - a_elt_size));
	if (b_elt_size > permute_size)
	  b_builder.quick_push (const0_rtx);
	else
	  b_builder.quick_push (b_builder.elt (i - b_elt_size));
      }
  a_builder.finalize ();
  b_builder.finalize ();

  /* Try loading A into a register.  */
  rtx_insn *last = get_last_insn ();
  rtx a = aarch64_expand_sve_const_pred_1 (NULL_RTX, a_builder, false);
  if (!a)
    return NULL_RTX;

  /* Try loading B into a register.  */
  rtx b = a;
  if (a_builder != b_builder)
    {
      b = aarch64_expand_sve_const_pred_1 (NULL_RTX, b_builder, false);
      if (!b)
	{
	  delete_insns_since (last);
	  return NULL_RTX;
	}
    }

  /* Emit the TRN1 itself.  */
  machine_mode mode = aarch64_sve_pred_mode (permute_size).require ();
  target = aarch64_target_reg (target, mode);
  emit_insn (gen_aarch64_sve (UNSPEC_TRN1, mode, target,
			      gen_lowpart (mode, a),
			      gen_lowpart (mode, b)));
  return target;
}

/* Subroutine of aarch64_expand_sve_const_pred.  Try to load the VNx16BI
   constant in BUILDER into an SVE predicate register.  Return the register
   on success, otherwise return null.  Use TARGET for the register if
   nonnull and convenient.

   ALLOW_RECURSE_P is true if we can use methods that would call this
   function recursively.  */

static rtx
aarch64_expand_sve_const_pred_1 (rtx target, rtx_vector_builder &builder,
				 bool allow_recurse_p)
{
  if (builder.encoded_nelts () == 1)
    /* A PFALSE or a PTRUE .B ALL.  */
    return aarch64_emit_set_immediate (target, builder);

  unsigned int elt_size = aarch64_widest_sve_pred_elt_size (builder);
  if (int vl = aarch64_partial_ptrue_length (builder, elt_size))
    {
      /* If we can load the constant using PTRUE, use it as-is.  */
      machine_mode mode = aarch64_sve_pred_mode (elt_size).require ();
      if (aarch64_svpattern_for_vl (mode, vl) != AARCH64_NUM_SVPATTERNS)
	return aarch64_emit_set_immediate (target, builder);

      /* Otherwise use WHILE to set the first VL bits.  */
      return aarch64_sve_move_pred_via_while (target, mode, vl);
    }

  if (!allow_recurse_p)
    return NULL_RTX;

  /* Try inverting the vector in element size ELT_SIZE and then EORing
     the result with an ELT_SIZE PTRUE.  */
  if (INTVAL (builder.elt (0)) == 0)
    if (rtx res = aarch64_expand_sve_const_pred_eor (target, builder,
						     elt_size))
      return res;

  /* Try using TRN1 to permute two simpler constants.  */
  for (unsigned int i = elt_size; i <= 8; i *= 2)
    if (rtx res = aarch64_expand_sve_const_pred_trn (target, builder,
						     elt_size, i))
      return res;

  return NULL_RTX;
}

/* Return an SVE predicate register that contains the VNx16BImode
   constant in BUILDER, without going through the move expanders.

   The returned register can have whatever mode seems most natural
   given the contents of BUILDER.  Use TARGET for the result if
   convenient.  */

static rtx
aarch64_expand_sve_const_pred (rtx target, rtx_vector_builder &builder)
{
  /* Try loading the constant using pure predicate operations.  */
  if (rtx res = aarch64_expand_sve_const_pred_1 (target, builder, true))
    return res;

  /* Try forcing the constant to memory.  */
  if (builder.full_nelts ().is_constant ())
    if (rtx mem = force_const_mem (VNx16BImode, builder.build ()))
      {
	target = aarch64_target_reg (target, VNx16BImode);
	emit_move_insn (target, mem);
	return target;
      }

  /* The last resort is to load the constant as an integer and then
     compare it against zero.  Use -1 for set bits in order to increase
     the changes of using SVE DUPM or an Advanced SIMD byte mask.  */
  rtx_vector_builder int_builder (VNx16QImode, builder.npatterns (),
				  builder.nelts_per_pattern ());
  for (unsigned int i = 0; i < builder.encoded_nelts (); ++i)
    int_builder.quick_push (INTVAL (builder.elt (i))
			    ? constm1_rtx : const0_rtx);
  return aarch64_convert_sve_data_to_pred (target, VNx16BImode,
					   int_builder.build ());
}

/* Set DEST to immediate IMM.  */

void
aarch64_expand_mov_immediate (rtx dest, rtx imm)
{
  machine_mode mode = GET_MODE (dest);

  /* Check on what type of symbol it is.  */
  scalar_int_mode int_mode;
  if ((GET_CODE (imm) == SYMBOL_REF
       || GET_CODE (imm) == LABEL_REF
       || GET_CODE (imm) == CONST
       || GET_CODE (imm) == CONST_POLY_INT)
      && is_a <scalar_int_mode> (mode, &int_mode))
    {
      rtx mem;
      poly_int64 offset;
      HOST_WIDE_INT const_offset;
      enum aarch64_symbol_type sty;

      /* If we have (const (plus symbol offset)), separate out the offset
	 before we start classifying the symbol.  */
      rtx base = strip_offset (imm, &offset);

      /* We must always add an offset involving VL separately, rather than
	 folding it into the relocation.  */
      if (!offset.is_constant (&const_offset))
	{
	  if (!TARGET_SVE)
	    {
	      aarch64_report_sve_required ();
	      return;
	    }
	  if (base == const0_rtx && aarch64_sve_cnt_immediate_p (offset))
	    emit_insn (gen_rtx_SET (dest, imm));
	  else
	    {
	      /* Do arithmetic on 32-bit values if the result is smaller
		 than that.  */
	      if (partial_subreg_p (int_mode, SImode))
		{
		  /* It is invalid to do symbol calculations in modes
		     narrower than SImode.  */
		  gcc_assert (base == const0_rtx);
		  dest = gen_lowpart (SImode, dest);
		  int_mode = SImode;
		}
	      if (base != const0_rtx)
		{
		  base = aarch64_force_temporary (int_mode, dest, base);
		  aarch64_add_offset (int_mode, dest, base, offset,
				      NULL_RTX, NULL_RTX, false);
		}
	      else
		aarch64_add_offset (int_mode, dest, base, offset,
				    dest, NULL_RTX, false);
	    }
	  return;
	}

      sty = aarch64_classify_symbol (base, const_offset);
      switch (sty)
	{
	case SYMBOL_FORCE_TO_MEM:
	  if (const_offset != 0
	      && targetm.cannot_force_const_mem (int_mode, imm))
	    {
	      gcc_assert (can_create_pseudo_p ());
	      base = aarch64_force_temporary (int_mode, dest, base);
	      aarch64_add_offset (int_mode, dest, base, const_offset,
				  NULL_RTX, NULL_RTX, false);
	      return;
	    }

	  mem = force_const_mem (ptr_mode, imm);
	  gcc_assert (mem);

	  /* If we aren't generating PC relative literals, then
	     we need to expand the literal pool access carefully.
	     This is something that needs to be done in a number
	     of places, so could well live as a separate function.  */
	  if (!aarch64_pcrelative_literal_loads)
	    {
	      gcc_assert (can_create_pseudo_p ());
	      base = gen_reg_rtx (ptr_mode);
	      aarch64_expand_mov_immediate (base, XEXP (mem, 0));
	      if (ptr_mode != Pmode)
		base = convert_memory_address (Pmode, base);
	      mem = gen_rtx_MEM (ptr_mode, base);
	    }

	  if (int_mode != ptr_mode)
	    mem = gen_rtx_ZERO_EXTEND (int_mode, mem);

	  emit_insn (gen_rtx_SET (dest, mem));

	  return;

        case SYMBOL_SMALL_TLSGD:
        case SYMBOL_SMALL_TLSDESC:
	case SYMBOL_SMALL_TLSIE:
	case SYMBOL_SMALL_GOT_28K:
	case SYMBOL_SMALL_GOT_4G:
	case SYMBOL_TINY_GOT:
	case SYMBOL_TINY_TLSIE:
	  if (const_offset != 0)
	    {
	      gcc_assert(can_create_pseudo_p ());
	      base = aarch64_force_temporary (int_mode, dest, base);
	      aarch64_add_offset (int_mode, dest, base, const_offset,
				  NULL_RTX, NULL_RTX, false);
	      return;
	    }
	  /* FALLTHRU */

	case SYMBOL_SMALL_ABSOLUTE:
	case SYMBOL_TINY_ABSOLUTE:
	case SYMBOL_TLSLE12:
	case SYMBOL_TLSLE24:
	case SYMBOL_TLSLE32:
	case SYMBOL_TLSLE48:
	  aarch64_load_symref_appropriately (dest, imm, sty);
	  return;

	default:
	  gcc_unreachable ();
	}
    }

  if (!CONST_INT_P (imm))
    {
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
	{
	  /* Only the low bit of each .H, .S and .D element is defined,
	     so we can set the upper bits to whatever we like.  If the
	     predicate is all-true in MODE, prefer to set all the undefined
	     bits as well, so that we can share a single .B predicate for
	     all modes.  */
	  if (imm == CONSTM1_RTX (mode))
	    imm = CONSTM1_RTX (VNx16BImode);

	  /* All methods for constructing predicate modes wider than VNx16BI
	     will set the upper bits of each element to zero.  Expose this
	     by moving such constants as a VNx16BI, so that all bits are
	     significant and so that constants for different modes can be
	     shared.  The wider constant will still be available as a
	     REG_EQUAL note.  */
	  rtx_vector_builder builder;
	  if (aarch64_get_sve_pred_bits (builder, imm))
	    {
	      rtx res = aarch64_expand_sve_const_pred (dest, builder);
	      if (dest != res)
		emit_move_insn (dest, gen_lowpart (mode, res));
	      return;
	    }
	}

      if (GET_CODE (imm) == HIGH
	  || aarch64_simd_valid_immediate (imm, NULL))
	{
	  emit_insn (gen_rtx_SET (dest, imm));
	  return;
	}

      if (GET_CODE (imm) == CONST_VECTOR && aarch64_sve_data_mode_p (mode))
	if (rtx res = aarch64_expand_sve_const_vector (dest, imm))
	  {
	    if (dest != res)
	      emit_insn (gen_aarch64_sve_reinterpret (mode, dest, res));
	    return;
	  }

      rtx mem = force_const_mem (mode, imm);
      gcc_assert (mem);
      emit_move_insn (dest, mem);
      return;
    }

  aarch64_internal_mov_immediate (dest, imm, true,
				  as_a <scalar_int_mode> (mode));
}

/* Emit an SVE predicated move from SRC to DEST.  PRED is a predicate
   that is known to contain PTRUE.  */

void
aarch64_emit_sve_pred_move (rtx dest, rtx pred, rtx src)
{
  expand_operand ops[3];
  machine_mode mode = GET_MODE (dest);
  create_output_operand (&ops[0], dest, mode);
  create_input_operand (&ops[1], pred, GET_MODE(pred));
  create_input_operand (&ops[2], src, mode);
  temporary_volatile_ok v (true);
  expand_insn (code_for_aarch64_pred_mov (mode), 3, ops);
}

/* Expand a pre-RA SVE data move from SRC to DEST in which at least one
   operand is in memory.  In this case we need to use the predicated LD1
   and ST1 instead of LDR and STR, both for correctness on big-endian
   targets and because LD1 and ST1 support a wider range of addressing modes.
   PRED_MODE is the mode of the predicate.

   See the comment at the head of aarch64-sve.md for details about the
   big-endian handling.  */

void
aarch64_expand_sve_mem_move (rtx dest, rtx src, machine_mode pred_mode)
{
  machine_mode mode = GET_MODE (dest);
  rtx ptrue = aarch64_ptrue_reg (pred_mode);
  if (!register_operand (src, mode)
      && !register_operand (dest, mode))
    {
      rtx tmp = gen_reg_rtx (mode);
      if (MEM_P (src))
	aarch64_emit_sve_pred_move (tmp, ptrue, src);
      else
	emit_move_insn (tmp, src);
      src = tmp;
    }
  aarch64_emit_sve_pred_move (dest, ptrue, src);
}

/* Called only on big-endian targets.  See whether an SVE vector move
   from SRC to DEST is effectively a REV[BHW] instruction, because at
   least one operand is a subreg of an SVE vector that has wider or
   narrower elements.  Return true and emit the instruction if so.

   For example:

     (set (reg:VNx8HI R1) (subreg:VNx8HI (reg:VNx16QI R2) 0))

   represents a VIEW_CONVERT between the following vectors, viewed
   in memory order:

     R2: { [0].high, [0].low,  [1].high, [1].low, ... }
     R1: { [0],      [1],      [2],      [3],     ... }

   The high part of lane X in R2 should therefore correspond to lane X*2
   of R1, but the register representations are:

         msb                                      lsb
     R2: ...... [1].high  [1].low   [0].high  [0].low
     R1: ...... [3]       [2]       [1]       [0]

   where the low part of lane X in R2 corresponds to lane X*2 in R1.
   We therefore need a reverse operation to swap the high and low values
   around.

   This is purely an optimization.  Without it we would spill the
   subreg operand to the stack in one mode and reload it in the
   other mode, which has the same effect as the REV.  */

bool
aarch64_maybe_expand_sve_subreg_move (rtx dest, rtx src)
{
  gcc_assert (BYTES_BIG_ENDIAN);
  if (GET_CODE (dest) == SUBREG)
    dest = SUBREG_REG (dest);
  if (GET_CODE (src) == SUBREG)
    src = SUBREG_REG (src);

  /* The optimization handles two single SVE REGs with different element
     sizes.  */
  if (!REG_P (dest)
      || !REG_P (src)
      || aarch64_classify_vector_mode (GET_MODE (dest)) != VEC_SVE_DATA
      || aarch64_classify_vector_mode (GET_MODE (src)) != VEC_SVE_DATA
      || (GET_MODE_UNIT_SIZE (GET_MODE (dest))
	  == GET_MODE_UNIT_SIZE (GET_MODE (src))))
    return false;

  /* Generate *aarch64_sve_mov<mode>_subreg_be.  */
  rtx ptrue = aarch64_ptrue_reg (VNx16BImode);
  rtx unspec = gen_rtx_UNSPEC (GET_MODE (dest), gen_rtvec (2, ptrue, src),
			       UNSPEC_REV_SUBREG);
  emit_insn (gen_rtx_SET (dest, unspec));
  return true;
}

/* Return a copy of X with mode MODE, without changing its other
   attributes.  Unlike gen_lowpart, this doesn't care whether the
   mode change is valid.  */

rtx
aarch64_replace_reg_mode (rtx x, machine_mode mode)
{
  if (GET_MODE (x) == mode)
    return x;

  x = shallow_copy_rtx (x);
  set_mode_and_regno (x, mode, REGNO (x));
  return x;
}

/* Return the SVE REV[BHW] unspec for reversing quantites of mode MODE
   stored in wider integer containers.  */

static unsigned int
aarch64_sve_rev_unspec (machine_mode mode)
{
  switch (GET_MODE_UNIT_SIZE (mode))
    {
    case 1: return UNSPEC_REVB;
    case 2: return UNSPEC_REVH;
    case 4: return UNSPEC_REVW;
    }
  gcc_unreachable ();
}

/* Split a *aarch64_sve_mov<mode>_subreg_be pattern with the given
   operands.  */

void
aarch64_split_sve_subreg_move (rtx dest, rtx ptrue, rtx src)
{
  /* Decide which REV operation we need.  The mode with wider elements
     determines the mode of the operands and the mode with the narrower
     elements determines the reverse width.  */
  machine_mode mode_with_wider_elts = GET_MODE (dest);
  machine_mode mode_with_narrower_elts = GET_MODE (src);
  if (GET_MODE_UNIT_SIZE (mode_with_wider_elts)
      < GET_MODE_UNIT_SIZE (mode_with_narrower_elts))
    std::swap (mode_with_wider_elts, mode_with_narrower_elts);

  unsigned int unspec = aarch64_sve_rev_unspec (mode_with_narrower_elts);
  machine_mode pred_mode = aarch64_sve_pred_mode (mode_with_wider_elts);

  /* Get the operands in the appropriate modes and emit the instruction.  */
  ptrue = gen_lowpart (pred_mode, ptrue);
  dest = aarch64_replace_reg_mode (dest, mode_with_wider_elts);
  src = aarch64_replace_reg_mode (src, mode_with_wider_elts);
  emit_insn (gen_aarch64_pred (unspec, mode_with_wider_elts,
			       dest, ptrue, src));
}

static bool
aarch64_function_ok_for_sibcall (tree, tree exp)
{
  if (crtl->abi->id () != expr_callee_abi (exp).id ())
    return false;

  return true;
}

/* Implement TARGET_PASS_BY_REFERENCE.  */

static bool
aarch64_pass_by_reference (cumulative_args_t pcum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  HOST_WIDE_INT size;
  machine_mode dummymode;
  int nregs;

  unsigned int num_zr, num_pr;
  if (arg.type && aarch64_sve_argument_p (arg.type, &num_zr, &num_pr))
    {
      if (pcum && !pcum->silent_p && !TARGET_SVE)
	/* We can't gracefully recover at this point, so make this a
	   fatal error.  */
	fatal_error (input_location, "arguments of type %qT require"
		     " the SVE ISA extension", arg.type);

      /* Variadic SVE types are passed by reference.  Normal non-variadic
	 arguments are too if we've run out of registers.  */
      return (!arg.named
	      || pcum->aapcs_nvrn + num_zr > NUM_FP_ARG_REGS
	      || pcum->aapcs_nprn + num_pr > NUM_PR_ARG_REGS);
    }

  /* GET_MODE_SIZE (BLKmode) is useless since it is 0.  */
  if (arg.mode == BLKmode && arg.type)
    size = int_size_in_bytes (arg.type);
  else
    /* No frontends can create types with variable-sized modes, so we
       shouldn't be asked to pass or return them.  */
    size = GET_MODE_SIZE (arg.mode).to_constant ();

  /* Aggregates are passed by reference based on their size.  */
  if (arg.aggregate_type_p ())
    size = int_size_in_bytes (arg.type);

  /* Variable sized arguments are always returned by reference.  */
  if (size < 0)
    return true;

  /* Can this be a candidate to be passed in fp/simd register(s)?  */
  if (aarch64_vfp_is_call_or_return_candidate (arg.mode, arg.type,
					       &dummymode, &nregs,
					       NULL))
    return false;

  /* Arguments which are variable sized or larger than 2 registers are
     passed by reference unless they are a homogenous floating point
     aggregate.  */
  return size > 2 * UNITS_PER_WORD;
}

/* Return TRUE if VALTYPE is padded to its least significant bits.  */
static bool
aarch64_return_in_msb (const_tree valtype)
{
  machine_mode dummy_mode;
  int dummy_int;

  /* Never happens in little-endian mode.  */
  if (!BYTES_BIG_ENDIAN)
    return false;

  /* Only composite types smaller than or equal to 16 bytes can
     be potentially returned in registers.  */
  if (!aarch64_composite_type_p (valtype, TYPE_MODE (valtype))
      || int_size_in_bytes (valtype) <= 0
      || int_size_in_bytes (valtype) > 16)
    return false;

  /* But not a composite that is an HFA (Homogeneous Floating-point Aggregate)
     or an HVA (Homogeneous Short-Vector Aggregate); such a special composite
     is always passed/returned in the least significant bits of fp/simd
     register(s).  */
  if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (valtype), valtype,
					       &dummy_mode, &dummy_int, NULL))
    return false;

  return true;
}

/* Implement TARGET_FUNCTION_VALUE.
   Define how to find the value returned by a function.  */

static rtx
aarch64_function_value (const_tree type, const_tree func,
			bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsignedp;
  int count;
  machine_mode ag_mode;

  mode = TYPE_MODE (type);
  if (INTEGRAL_TYPE_P (type))
    mode = promote_function_mode (type, mode, &unsignedp, func, 1);

  unsigned int num_zr, num_pr;
  if (type && aarch64_sve_argument_p (type, &num_zr, &num_pr))
    {
      /* Don't raise an error here if we're called when SVE is disabled,
	 since this is really just a query function.  Other code must
	 do that where appropriate.  */
      mode = TYPE_MODE_RAW (type);
      gcc_assert (VECTOR_MODE_P (mode)
		  && (!TARGET_SVE || aarch64_sve_mode_p (mode)));

      if (num_zr > 0 && num_pr == 0)
	return gen_rtx_REG (mode, V0_REGNUM);

      if (num_zr == 0 && num_pr == 1)
	return gen_rtx_REG (mode, P0_REGNUM);

      gcc_unreachable ();
    }

  /* Generic vectors that map to SVE modes with -msve-vector-bits=N are
     returned in memory, not by value.  */
  gcc_assert (!aarch64_sve_mode_p (mode));

  if (aarch64_return_in_msb (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);

      if (size % UNITS_PER_WORD != 0)
	{
	  size += UNITS_PER_WORD - size % UNITS_PER_WORD;
	  mode = int_mode_for_size (size * BITS_PER_UNIT, 0).require ();
	}
    }

  if (aarch64_vfp_is_call_or_return_candidate (mode, type,
					       &ag_mode, &count, NULL))
    {
      if (!aarch64_composite_type_p (type, mode))
	{
	  gcc_assert (count == 1 && mode == ag_mode);
	  return gen_rtx_REG (mode, V0_REGNUM);
	}
      else
	{
	  int i;
	  rtx par;

	  par = gen_rtx_PARALLEL (mode, rtvec_alloc (count));
	  for (i = 0; i < count; i++)
	    {
	      rtx tmp = gen_rtx_REG (ag_mode, V0_REGNUM + i);
	      rtx offset = gen_int_mode (i * GET_MODE_SIZE (ag_mode), Pmode);
	      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp, offset);
	      XVECEXP (par, 0, i) = tmp;
	    }
	  return par;
	}
    }
  else
    return gen_rtx_REG (mode, R0_REGNUM);
}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.
   Return true if REGNO is the number of a hard register in which the values
   of called function may come back.  */

static bool
aarch64_function_value_regno_p (const unsigned int regno)
{
  /* Maximum of 16 bytes can be returned in the general registers.  Examples
     of 16-byte return values are: 128-bit integers and 16-byte small
     structures (excluding homogeneous floating-point aggregates).  */
  if (regno == R0_REGNUM || regno == R1_REGNUM)
    return true;

  /* Up to four fp/simd registers can return a function value, e.g. a
     homogeneous floating-point aggregate having four members.  */
  if (regno >= V0_REGNUM && regno < V0_REGNUM + HA_MAX_NUM_FLDS)
    return TARGET_FLOAT;

  return false;
}

/* Implement TARGET_RETURN_IN_MEMORY.

   If the type T of the result of a function is such that
     void func (T arg)
   would require that arg be passed as a value in a register (or set of
   registers) according to the parameter passing rules, then the result
   is returned in the same registers as would be used for such an
   argument.  */

static bool
aarch64_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size;
  machine_mode ag_mode;
  int count;

  if (!AGGREGATE_TYPE_P (type)
      && TREE_CODE (type) != COMPLEX_TYPE
      && TREE_CODE (type) != VECTOR_TYPE)
    /* Simple scalar types always returned in registers.  */
    return false;

  unsigned int num_zr, num_pr;
  if (type && aarch64_sve_argument_p (type, &num_zr, &num_pr))
    {
      /* All SVE types we support fit in registers.  For example, it isn't
	 yet possible to define an aggregate of 9+ SVE vectors or 5+ SVE
	 predicates.  */
      gcc_assert (num_zr <= NUM_FP_ARG_REGS && num_pr <= NUM_PR_ARG_REGS);
      return false;
    }

  if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (type),
					       type,
					       &ag_mode,
					       &count,
					       NULL))
    return false;

  /* Types larger than 2 registers returned in memory.  */
  size = int_size_in_bytes (type);
  return (size < 0 || size > 2 * UNITS_PER_WORD);
}

static bool
aarch64_vfp_is_call_candidate (cumulative_args_t pcum_v, machine_mode mode,
			       const_tree type, int *nregs)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  return aarch64_vfp_is_call_or_return_candidate (mode,
						  type,
						  &pcum->aapcs_vfp_rmode,
						  nregs,
						  NULL);
}

/* Given MODE and TYPE of a function argument, return the alignment in
   bits.  The idea is to suppress any stronger alignment requested by
   the user and opt for the natural alignment (specified in AAPCS64 \S
   4.1).  ABI_BREAK is set to true if the alignment was incorrectly
   calculated in versions of GCC prior to GCC-9.  This is a helper
   function for local use only.  */

static unsigned int
aarch64_function_arg_alignment (machine_mode mode, const_tree type,
				bool *abi_break)
{
  *abi_break = false;
  if (!type)
    return GET_MODE_ALIGNMENT (mode);

  if (integer_zerop (TYPE_SIZE (type)))
    return 0;

  gcc_assert (TYPE_MODE (type) == mode);

  if (!AGGREGATE_TYPE_P (type))
    return TYPE_ALIGN (TYPE_MAIN_VARIANT (type));

  if (TREE_CODE (type) == ARRAY_TYPE)
    return TYPE_ALIGN (TREE_TYPE (type));

  unsigned int alignment = 0;
  unsigned int bitfield_alignment = 0;
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      {
	alignment = std::max (alignment, DECL_ALIGN (field));
	if (DECL_BIT_FIELD_TYPE (field))
	  bitfield_alignment
	    = std::max (bitfield_alignment,
			TYPE_ALIGN (DECL_BIT_FIELD_TYPE (field)));
      }

  if (bitfield_alignment > alignment)
    {
      *abi_break = true;
      return bitfield_alignment;
    }

  return alignment;
}

/* Layout a function argument according to the AAPCS64 rules.  The rule
   numbers refer to the rule numbers in the AAPCS64.  */

static void
aarch64_layout_arg (cumulative_args_t pcum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  tree type = arg.type;
  machine_mode mode = arg.mode;
  int ncrn, nvrn, nregs;
  bool allocate_ncrn, allocate_nvrn;
  HOST_WIDE_INT size;
  bool abi_break;

  /* We need to do this once per argument.  */
  if (pcum->aapcs_arg_processed)
    return;

  pcum->aapcs_arg_processed = true;

  unsigned int num_zr, num_pr;
  if (type && aarch64_sve_argument_p (type, &num_zr, &num_pr))
    {
      /* The PCS says that it is invalid to pass an SVE value to an
	 unprototyped function.  There is no ABI-defined location we
	 can return in this case, so we have no real choice but to raise
	 an error immediately, even though this is only a query function.  */
      if (arg.named && pcum->pcs_variant != ARM_PCS_SVE)
	{
	  gcc_assert (!pcum->silent_p);
	  error ("SVE type %qT cannot be passed to an unprototyped function",
		 arg.type);
	  /* Avoid repeating the message, and avoid tripping the assert
	     below.  */
	  pcum->pcs_variant = ARM_PCS_SVE;
	}

      /* We would have converted the argument into pass-by-reference
	 form if it didn't fit in registers.  */
      pcum->aapcs_nextnvrn = pcum->aapcs_nvrn + num_zr;
      pcum->aapcs_nextnprn = pcum->aapcs_nprn + num_pr;
      gcc_assert (arg.named
		  && pcum->pcs_variant == ARM_PCS_SVE
		  && aarch64_sve_mode_p (mode)
		  && pcum->aapcs_nextnvrn <= NUM_FP_ARG_REGS
		  && pcum->aapcs_nextnprn <= NUM_PR_ARG_REGS);

      if (num_zr > 0 && num_pr == 0)
	pcum->aapcs_reg = gen_rtx_REG (mode, V0_REGNUM + pcum->aapcs_nvrn);
      else if (num_zr == 0 && num_pr == 1)
	pcum->aapcs_reg = gen_rtx_REG (mode, P0_REGNUM + pcum->aapcs_nprn);
      else
	gcc_unreachable ();
      return;
    }

  /* Generic vectors that map to SVE modes with -msve-vector-bits=N are
     passed by reference, not by value.  */
  gcc_assert (!aarch64_sve_mode_p (mode));

  /* Size in bytes, rounded to the nearest multiple of 8 bytes.  */
  if (type)
    size = int_size_in_bytes (type);
  else
    /* No frontends can create types with variable-sized modes, so we
       shouldn't be asked to pass or return them.  */
    size = GET_MODE_SIZE (mode).to_constant ();
  size = ROUND_UP (size, UNITS_PER_WORD);

  allocate_ncrn = (type) ? !(FLOAT_TYPE_P (type)) : !FLOAT_MODE_P (mode);
  allocate_nvrn = aarch64_vfp_is_call_candidate (pcum_v,
						 mode,
						 type,
						 &nregs);

  /* allocate_ncrn may be false-positive, but allocate_nvrn is quite reliable.
     The following code thus handles passing by SIMD/FP registers first.  */

  nvrn = pcum->aapcs_nvrn;

  /* C1 - C5 for floating point, homogenous floating point aggregates (HFA)
     and homogenous short-vector aggregates (HVA).  */
  if (allocate_nvrn)
    {
      if (!pcum->silent_p && !TARGET_FLOAT)
	aarch64_err_no_fpadvsimd (mode);

      if (nvrn + nregs <= NUM_FP_ARG_REGS)
	{
	  pcum->aapcs_nextnvrn = nvrn + nregs;
	  if (!aarch64_composite_type_p (type, mode))
	    {
	      gcc_assert (nregs == 1);
	      pcum->aapcs_reg = gen_rtx_REG (mode, V0_REGNUM + nvrn);
	    }
	  else
	    {
	      rtx par;
	      int i;
	      par = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));
	      for (i = 0; i < nregs; i++)
		{
		  rtx tmp = gen_rtx_REG (pcum->aapcs_vfp_rmode,
					 V0_REGNUM + nvrn + i);
		  rtx offset = gen_int_mode
		    (i * GET_MODE_SIZE (pcum->aapcs_vfp_rmode), Pmode);
		  tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp, offset);
		  XVECEXP (par, 0, i) = tmp;
		}
	      pcum->aapcs_reg = par;
	    }
	  return;
	}
      else
	{
	  /* C.3 NSRN is set to 8.  */
	  pcum->aapcs_nextnvrn = NUM_FP_ARG_REGS;
	  goto on_stack;
	}
    }

  ncrn = pcum->aapcs_ncrn;
  nregs = size / UNITS_PER_WORD;

  /* C6 - C9.  though the sign and zero extension semantics are
     handled elsewhere.  This is the case where the argument fits
     entirely general registers.  */
  if (allocate_ncrn && (ncrn + nregs <= NUM_ARG_REGS))
    {
      gcc_assert (nregs == 0 || nregs == 1 || nregs == 2);

      /* C.8 if the argument has an alignment of 16 then the NGRN is
	 rounded up to the next even number.  */
      if (nregs == 2
	  && ncrn % 2
	  /* The == 16 * BITS_PER_UNIT instead of >= 16 * BITS_PER_UNIT
	     comparison is there because for > 16 * BITS_PER_UNIT
	     alignment nregs should be > 2 and therefore it should be
	     passed by reference rather than value.  */
	  && (aarch64_function_arg_alignment (mode, type, &abi_break)
	      == 16 * BITS_PER_UNIT))
	{
	  if (abi_break && warn_psabi && currently_expanding_gimple_stmt)
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 9.1", type);
	  ++ncrn;
	  gcc_assert (ncrn + nregs <= NUM_ARG_REGS);
	}

      /* NREGS can be 0 when e.g. an empty structure is to be passed.
	 A reg is still generated for it, but the caller should be smart
	 enough not to use it.  */
      if (nregs == 0 || nregs == 1 || GET_MODE_CLASS (mode) == MODE_INT)
	pcum->aapcs_reg = gen_rtx_REG (mode, R0_REGNUM + ncrn);
      else
	{
	  rtx par;
	  int i;

	  par = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));
	  for (i = 0; i < nregs; i++)
	    {
	      rtx tmp = gen_rtx_REG (word_mode, R0_REGNUM + ncrn + i);
	      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp,
				       GEN_INT (i * UNITS_PER_WORD));
	      XVECEXP (par, 0, i) = tmp;
	    }
	  pcum->aapcs_reg = par;
	}

      pcum->aapcs_nextncrn = ncrn + nregs;
      return;
    }

  /* C.11  */
  pcum->aapcs_nextncrn = NUM_ARG_REGS;

  /* The argument is passed on stack; record the needed number of words for
     this argument and align the total size if necessary.  */
on_stack:
  pcum->aapcs_stack_words = size / UNITS_PER_WORD;

  if (aarch64_function_arg_alignment (mode, type, &abi_break)
      == 16 * BITS_PER_UNIT)
    {
      int new_size = ROUND_UP (pcum->aapcs_stack_size, 16 / UNITS_PER_WORD);
      if (pcum->aapcs_stack_size != new_size)
	{
	  if (abi_break && warn_psabi && currently_expanding_gimple_stmt)
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 9.1", type);
	  pcum->aapcs_stack_size = new_size;
	}
    }
  return;
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
aarch64_function_arg (cumulative_args_t pcum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  gcc_assert (pcum->pcs_variant == ARM_PCS_AAPCS64
	      || pcum->pcs_variant == ARM_PCS_SIMD
	      || pcum->pcs_variant == ARM_PCS_SVE);

  if (arg.end_marker_p ())
    return gen_int_mode (pcum->pcs_variant, DImode);

  aarch64_layout_arg (pcum_v, arg);
  return pcum->aapcs_reg;
}

void
aarch64_init_cumulative_args (CUMULATIVE_ARGS *pcum,
			      const_tree fntype,
			      rtx libname ATTRIBUTE_UNUSED,
			      const_tree fndecl ATTRIBUTE_UNUSED,
			      unsigned n_named ATTRIBUTE_UNUSED,
			      bool silent_p)
{
  pcum->aapcs_ncrn = 0;
  pcum->aapcs_nvrn = 0;
  pcum->aapcs_nprn = 0;
  pcum->aapcs_nextncrn = 0;
  pcum->aapcs_nextnvrn = 0;
  pcum->aapcs_nextnprn = 0;
  if (fntype)
    pcum->pcs_variant = (arm_pcs) fntype_abi (fntype).id ();
  else
    pcum->pcs_variant = ARM_PCS_AAPCS64;
  pcum->aapcs_reg = NULL_RTX;
  pcum->aapcs_arg_processed = false;
  pcum->aapcs_stack_words = 0;
  pcum->aapcs_stack_size = 0;
  pcum->silent_p = silent_p;

  if (!silent_p
      && !TARGET_FLOAT
      && fndecl && TREE_PUBLIC (fndecl)
      && fntype && fntype != error_mark_node)
    {
      const_tree type = TREE_TYPE (fntype);
      machine_mode mode ATTRIBUTE_UNUSED; /* To pass pointer as argument.  */
      int nregs ATTRIBUTE_UNUSED; /* Likewise.  */
      if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (type), type,
						   &mode, &nregs, NULL))
	aarch64_err_no_fpadvsimd (TYPE_MODE (type));
    }

  if (!silent_p
      && !TARGET_SVE
      && pcum->pcs_variant == ARM_PCS_SVE)
    {
      /* We can't gracefully recover at this point, so make this a
	 fatal error.  */
      if (fndecl)
	fatal_error (input_location, "%qE requires the SVE ISA extension",
		     fndecl);
      else
	fatal_error (input_location, "calls to functions of type %qT require"
		     " the SVE ISA extension", fntype);
    }
}

static void
aarch64_function_arg_advance (cumulative_args_t pcum_v,
			      const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  if (pcum->pcs_variant == ARM_PCS_AAPCS64
      || pcum->pcs_variant == ARM_PCS_SIMD
      || pcum->pcs_variant == ARM_PCS_SVE)
    {
      aarch64_layout_arg (pcum_v, arg);
      gcc_assert ((pcum->aapcs_reg != NULL_RTX)
		  != (pcum->aapcs_stack_words != 0));
      pcum->aapcs_arg_processed = false;
      pcum->aapcs_ncrn = pcum->aapcs_nextncrn;
      pcum->aapcs_nvrn = pcum->aapcs_nextnvrn;
      pcum->aapcs_nprn = pcum->aapcs_nextnprn;
      pcum->aapcs_stack_size += pcum->aapcs_stack_words;
      pcum->aapcs_stack_words = 0;
      pcum->aapcs_reg = NULL_RTX;
    }
}

bool
aarch64_function_arg_regno_p (unsigned regno)
{
  return ((GP_REGNUM_P (regno) && regno < R0_REGNUM + NUM_ARG_REGS)
	  || (FP_REGNUM_P (regno) && regno < V0_REGNUM + NUM_FP_ARG_REGS));
}

/* Implement FUNCTION_ARG_BOUNDARY.  Every parameter gets at least
   PARM_BOUNDARY bits of alignment, but will be given anything up
   to STACK_BOUNDARY bits if the type requires it.  This makes sure
   that both before and after the layout of each argument, the Next
   Stacked Argument Address (NSAA) will have a minimum alignment of
   8 bytes.  */

static unsigned int
aarch64_function_arg_boundary (machine_mode mode, const_tree type)
{
  bool abi_break;
  unsigned int alignment = aarch64_function_arg_alignment (mode, type,
							   &abi_break);
  if (abi_break & warn_psabi)
    inform (input_location, "parameter passing for argument of type "
	    "%qT changed in GCC 9.1", type);

  return MIN (MAX (alignment, PARM_BOUNDARY), STACK_BOUNDARY);
}

/* Implement TARGET_GET_RAW_RESULT_MODE and TARGET_GET_RAW_ARG_MODE.  */

static fixed_size_mode
aarch64_get_reg_raw_mode (int regno)
{
  if (TARGET_SVE && FP_REGNUM_P (regno))
    /* Don't use the SVE part of the register for __builtin_apply and
       __builtin_return.  The SVE registers aren't used by the normal PCS,
       so using them there would be a waste of time.  The PCS extensions
       for SVE types are fundamentally incompatible with the
       __builtin_return/__builtin_apply interface.  */
    return as_a <fixed_size_mode> (V16QImode);
  return default_get_reg_raw_mode (regno);
}

/* Implement TARGET_FUNCTION_ARG_PADDING.

   Small aggregate types are placed in the lowest memory address.

   The related parameter passing rules are B.4, C.3, C.5 and C.14.  */

static pad_direction
aarch64_function_arg_padding (machine_mode mode, const_tree type)
{
  /* On little-endian targets, the least significant byte of every stack
     argument is passed at the lowest byte address of the stack slot.  */
  if (!BYTES_BIG_ENDIAN)
    return PAD_UPWARD;

  /* Otherwise, integral, floating-point and pointer types are padded downward:
     the least significant byte of a stack argument is passed at the highest
     byte address of the stack slot.  */
  if (type
      ? (INTEGRAL_TYPE_P (type) || SCALAR_FLOAT_TYPE_P (type)
	 || POINTER_TYPE_P (type))
      : (SCALAR_INT_MODE_P (mode) || SCALAR_FLOAT_MODE_P (mode)))
    return PAD_DOWNWARD;

  /* Everything else padded upward, i.e. data in first byte of stack slot.  */
  return PAD_UPWARD;
}

/* Similarly, for use by BLOCK_REG_PADDING (MODE, TYPE, FIRST).

   It specifies padding for the last (may also be the only)
   element of a block move between registers and memory.  If
   assuming the block is in the memory, padding upward means that
   the last element is padded after its highest significant byte,
   while in downward padding, the last element is padded at the
   its least significant byte side.

   Small aggregates and small complex types are always padded
   upwards.

   We don't need to worry about homogeneous floating-point or
   short-vector aggregates; their move is not affected by the
   padding direction determined here.  Regardless of endianness,
   each element of such an aggregate is put in the least
   significant bits of a fp/simd register.

   Return !BYTES_BIG_ENDIAN if the least significant byte of the
   register has useful data, and return the opposite if the most
   significant byte does.  */

bool
aarch64_pad_reg_upward (machine_mode mode, const_tree type,
		     bool first ATTRIBUTE_UNUSED)
{

  /* Small composite types are always padded upward.  */
  if (BYTES_BIG_ENDIAN && aarch64_composite_type_p (type, mode))
    {
      HOST_WIDE_INT size;
      if (type)
	size = int_size_in_bytes (type);
      else
	/* No frontends can create types with variable-sized modes, so we
	   shouldn't be asked to pass or return them.  */
	size = GET_MODE_SIZE (mode).to_constant ();
      if (size < 2 * UNITS_PER_WORD)
	return true;
    }

  /* Otherwise, use the default padding.  */
  return !BYTES_BIG_ENDIAN;
}

static scalar_int_mode
aarch64_libgcc_cmp_return_mode (void)
{
  return SImode;
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

/* We use the 12-bit shifted immediate arithmetic instructions so values
   must be multiple of (1 << 12), i.e. 4096.  */
#define ARITH_FACTOR 4096

#if (PROBE_INTERVAL % ARITH_FACTOR) != 0
#error Cannot use simple address calculation for stack probing
#endif

/* The pair of scratch registers used for stack probing.  */
#define PROBE_STACK_FIRST_REG  R9_REGNUM
#define PROBE_STACK_SECOND_REG R10_REGNUM

/* Emit code to probe a range of stack addresses from FIRST to FIRST+POLY_SIZE,
   inclusive.  These are offsets from the current stack pointer.  */

static void
aarch64_emit_probe_stack_range (HOST_WIDE_INT first, poly_int64 poly_size)
{
  HOST_WIDE_INT size;
  if (!poly_size.is_constant (&size))
    {
      sorry ("stack probes for SVE frames");
      return;
    }

  rtx reg1 = gen_rtx_REG (Pmode, PROBE_STACK_FIRST_REG);

  /* See the same assertion on PROBE_INTERVAL above.  */
  gcc_assert ((first % ARITH_FACTOR) == 0);

  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  */
  if (size <= PROBE_INTERVAL)
    {
      const HOST_WIDE_INT base = ROUND_UP (size, ARITH_FACTOR);

      emit_set_insn (reg1,
		     plus_constant (Pmode,
				    stack_pointer_rtx, -(first + base)));
      emit_stack_probe (plus_constant (Pmode, reg1, base - size));
    }

  /* The run-time loop is made up of 8 insns in the generic case while the
     compile-time loop is made up of 4+2*(n-2) insns for n # of intervals.  */
  else if (size <= 4 * PROBE_INTERVAL)
    {
      HOST_WIDE_INT i, rem;

      emit_set_insn (reg1,
		     plus_constant (Pmode,
				    stack_pointer_rtx,
				    -(first + PROBE_INTERVAL)));
      emit_stack_probe (reg1);

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 2 until
	 it exceeds SIZE.  If only two probes are needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = 2 * PROBE_INTERVAL; i < size; i += PROBE_INTERVAL)
	{
	  emit_set_insn (reg1,
			 plus_constant (Pmode, reg1, -PROBE_INTERVAL));
	  emit_stack_probe (reg1);
	}

      rem = size - (i - PROBE_INTERVAL);
      if (rem > 256)
	{
	  const HOST_WIDE_INT base = ROUND_UP (rem, ARITH_FACTOR);

	  emit_set_insn (reg1, plus_constant (Pmode, reg1, -base));
	  emit_stack_probe (plus_constant (Pmode, reg1, base - rem));
	}
      else
	emit_stack_probe (plus_constant (Pmode, reg1, -rem));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      rtx reg2 = gen_rtx_REG (Pmode, PROBE_STACK_SECOND_REG);

      /* Step 1: round SIZE to the previous multiple of the interval.  */

      HOST_WIDE_INT rounded_size = size & -PROBE_INTERVAL;


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      emit_set_insn (reg1,
		     plus_constant (Pmode, stack_pointer_rtx, -first));

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      HOST_WIDE_INT adjustment = - (first + rounded_size);
      if (! aarch64_uimm12_shift (adjustment))
	{
	  aarch64_internal_mov_immediate (reg2, GEN_INT (adjustment),
					  true, Pmode);
	  emit_set_insn (reg2, gen_rtx_PLUS (Pmode, stack_pointer_rtx, reg2));
	}
      else
	emit_set_insn (reg2,
		       plus_constant (Pmode, stack_pointer_rtx, adjustment));

      /* Step 3: the loop

	 do
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }
	 while (TEST_ADDR != LAST_ADDR)

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      emit_insn (gen_probe_stack_range (reg1, reg1, reg2));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	{
	  HOST_WIDE_INT rem = size - rounded_size;

	  if (rem > 256)
	    {
	      const HOST_WIDE_INT base = ROUND_UP (rem, ARITH_FACTOR);

	      emit_set_insn (reg2, plus_constant (Pmode, reg2, -base));
	      emit_stack_probe (plus_constant (Pmode, reg2, base - rem));
	    }
	  else
	    emit_stack_probe (plus_constant (Pmode, reg2, -rem));
	}
    }

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   absolute addresses.  */

const char *
aarch64_output_probe_stack_range (rtx reg1, rtx reg2)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  HOST_WIDE_INT stack_clash_probe_interval
    = 1 << param_stack_clash_protection_guard_size;

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg1;
  HOST_WIDE_INT interval;
  if (flag_stack_clash_protection)
    interval = stack_clash_probe_interval;
  else
    interval = PROBE_INTERVAL;

  gcc_assert (aarch64_uimm12_shift (interval));
  xops[1] = GEN_INT (interval);

  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* If doing stack clash protection then we probe up by the ABI specified
     amount.  We do this because we're dropping full pages at a time in the
     loop.  But if we're doing non-stack clash probing, probe at SP 0.  */
  if (flag_stack_clash_protection)
    xops[1] = GEN_INT (STACK_CLASH_CALLER_GUARD);
  else
    xops[1] = CONST0_RTX (GET_MODE (xops[1]));

  /* Probe at TEST_ADDR.  If we're inside the loop it is always safe to probe
     by this amount for each iteration.  */
  output_asm_insn ("str\txzr, [%0, %1]", xops);

  /* Test if TEST_ADDR == LAST_ADDR.  */
  xops[1] = reg2;
  output_asm_insn ("cmp\t%0, %1", xops);

  /* Branch.  */
  fputs ("\tb.ne\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* Emit the probe loop for doing stack clash probes and stack adjustments for
   SVE.  This emits probes from BASE to BASE - ADJUSTMENT based on a guard size
   of GUARD_SIZE.  When a probe is emitted it is done at most
   MIN_PROBE_THRESHOLD bytes from the current BASE at an interval of
   at most MIN_PROBE_THRESHOLD.  By the end of this function
   BASE = BASE - ADJUSTMENT.  */

const char *
aarch64_output_probe_sve_stack_clash (rtx base, rtx adjustment,
				      rtx min_probe_threshold, rtx guard_size)
{
  /* This function is not allowed to use any instruction generation function
     like gen_ and friends.  If you do you'll likely ICE during CFG validation,
     so instead emit the code you want using output_asm_insn.  */
  gcc_assert (flag_stack_clash_protection);
  gcc_assert (CONST_INT_P (min_probe_threshold) && CONST_INT_P (guard_size));
  gcc_assert (INTVAL (guard_size) > INTVAL (min_probe_threshold));

  /* The minimum required allocation before the residual requires probing.  */
  HOST_WIDE_INT residual_probe_guard = INTVAL (min_probe_threshold);

  /* Clamp the value down to the nearest value that can be used with a cmp.  */
  residual_probe_guard = aarch64_clamp_to_uimm12_shift (residual_probe_guard);
  rtx probe_offset_value_rtx = gen_int_mode (residual_probe_guard, Pmode);

  gcc_assert (INTVAL (min_probe_threshold) >= residual_probe_guard);
  gcc_assert (aarch64_uimm12_shift (residual_probe_guard));

  static int labelno = 0;
  char loop_start_lab[32];
  char loop_end_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_start_lab, "SVLPSPL", labelno);
  ASM_GENERATE_INTERNAL_LABEL (loop_end_lab, "SVLPEND", labelno++);

  /* Emit loop start label.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_start_lab);

  /* ADJUSTMENT < RESIDUAL_PROBE_GUARD.  */
  xops[0] = adjustment;
  xops[1] = probe_offset_value_rtx;
  output_asm_insn ("cmp\t%0, %1", xops);

  /* Branch to end if not enough adjustment to probe.  */
  fputs ("\tb.lt\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_end_lab);
  fputc ('\n', asm_out_file);

  /* BASE = BASE - RESIDUAL_PROBE_GUARD.  */
  xops[0] = base;
  xops[1] = probe_offset_value_rtx;
  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* Probe at BASE.  */
  xops[1] = const0_rtx;
  output_asm_insn ("str\txzr, [%0, %1]", xops);

  /* ADJUSTMENT = ADJUSTMENT - RESIDUAL_PROBE_GUARD.  */
  xops[0] = adjustment;
  xops[1] = probe_offset_value_rtx;
  output_asm_insn ("sub\t%0, %0, %1", xops);

  /* Branch to start if still more bytes to allocate.  */
  fputs ("\tb\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_start_lab);
  fputc ('\n', asm_out_file);

  /* No probe leave.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_end_lab);

  /* BASE = BASE - ADJUSTMENT.  */
  xops[0] = base;
  xops[1] = adjustment;
  output_asm_insn ("sub\t%0, %0, %1", xops);
  return "";
}

/* Determine whether a frame chain needs to be generated.  */
static bool
aarch64_needs_frame_chain (void)
{
  /* Force a frame chain for EH returns so the return address is at FP+8.  */
  if (frame_pointer_needed || crtl->calls_eh_return)
    return true;

  /* A leaf function cannot have calls or write LR.  */
  bool is_leaf = crtl->is_leaf && !df_regs_ever_live_p (LR_REGNUM);

  /* Don't use a frame chain in leaf functions if leaf frame pointers
     are disabled.  */
  if (flag_omit_leaf_frame_pointer && is_leaf)
    return false;

  return aarch64_use_frame_pointer;
}

/* Mark the registers that need to be saved by the callee and calculate
   the size of the callee-saved registers area and frame record (both FP
   and LR may be omitted).  */
static void
aarch64_layout_frame (void)
{
  poly_int64 offset = 0;
  int regno, last_fp_reg = INVALID_REGNUM;
  machine_mode vector_save_mode = aarch64_reg_save_mode (V8_REGNUM);
  poly_int64 vector_save_size = GET_MODE_SIZE (vector_save_mode);
  bool frame_related_fp_reg_p = false;
  aarch64_frame &frame = cfun->machine->frame;

  frame.emit_frame_chain = aarch64_needs_frame_chain ();

  /* Adjust the outgoing arguments size if required.  Keep it in sync with what
     the mid-end is doing.  */
  crtl->outgoing_args_size = STACK_DYNAMIC_OFFSET (cfun);

#define SLOT_NOT_REQUIRED (-2)
#define SLOT_REQUIRED     (-1)

  frame.wb_candidate1 = INVALID_REGNUM;
  frame.wb_candidate2 = INVALID_REGNUM;
  frame.spare_pred_reg = INVALID_REGNUM;

  /* First mark all the registers that really need to be saved...  */
  for (regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    frame.reg_offset[regno] = SLOT_NOT_REQUIRED;

  /* ... that includes the eh data registers (if needed)...  */
  if (crtl->calls_eh_return)
    for (regno = 0; EH_RETURN_DATA_REGNO (regno) != INVALID_REGNUM; regno++)
      frame.reg_offset[EH_RETURN_DATA_REGNO (regno)] = SLOT_REQUIRED;

  /* ... and any callee saved register that dataflow says is live.  */
  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    if (df_regs_ever_live_p (regno)
	&& !fixed_regs[regno]
	&& (regno == R30_REGNUM
	    || !crtl->abi->clobbers_full_reg_p (regno)))
      frame.reg_offset[regno] = SLOT_REQUIRED;

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    if (df_regs_ever_live_p (regno)
	&& !fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno))
      {
	frame.reg_offset[regno] = SLOT_REQUIRED;
	last_fp_reg = regno;
	if (aarch64_emit_cfi_for_reg_p (regno))
	  frame_related_fp_reg_p = true;
      }

  /* Big-endian SVE frames need a spare predicate register in order
     to save Z8-Z15.  Decide which register they should use.  Prefer
     an unused argument register if possible, so that we don't force P4
     to be saved unnecessarily.  */
  if (frame_related_fp_reg_p
      && crtl->abi->id () == ARM_PCS_SVE
      && BYTES_BIG_ENDIAN)
    {
      bitmap live1 = df_get_live_out (ENTRY_BLOCK_PTR_FOR_FN (cfun));
      bitmap live2 = df_get_live_in (EXIT_BLOCK_PTR_FOR_FN (cfun));
      for (regno = P0_REGNUM; regno <= P7_REGNUM; regno++)
	if (!bitmap_bit_p (live1, regno) && !bitmap_bit_p (live2, regno))
	  break;
      gcc_assert (regno <= P7_REGNUM);
      frame.spare_pred_reg = regno;
      df_set_regs_ever_live (regno, true);
    }

  for (regno = P0_REGNUM; regno <= P15_REGNUM; regno++)
    if (df_regs_ever_live_p (regno)
	&& !fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno))
      frame.reg_offset[regno] = SLOT_REQUIRED;

  /* With stack-clash, LR must be saved in non-leaf functions.  */
  gcc_assert (crtl->is_leaf
	      || maybe_ne (frame.reg_offset[R30_REGNUM], SLOT_NOT_REQUIRED));

  /* Now assign stack slots for the registers.  Start with the predicate
     registers, since predicate LDR and STR have a relatively small
     offset range.  These saves happen below the hard frame pointer.  */
  for (regno = P0_REGNUM; regno <= P15_REGNUM; regno++)
    if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
      {
	frame.reg_offset[regno] = offset;
	offset += BYTES_PER_SVE_PRED;
      }

  /* We save a maximum of 8 predicate registers, and since vector
     registers are 8 times the size of a predicate register, all the
     saved predicates fit within a single vector.  Doing this also
     rounds the offset to a 128-bit boundary.  */
  if (maybe_ne (offset, 0))
    {
      gcc_assert (known_le (offset, vector_save_size));
      offset = vector_save_size;
    }

  /* If we need to save any SVE vector registers, add them next.  */
  if (last_fp_reg != (int) INVALID_REGNUM && crtl->abi->id () == ARM_PCS_SVE)
    for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
      if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
	{
	  frame.reg_offset[regno] = offset;
	  offset += vector_save_size;
	}

  /* OFFSET is now the offset of the hard frame pointer from the bottom
     of the callee save area.  */
  bool saves_below_hard_fp_p = maybe_ne (offset, 0);
  frame.below_hard_fp_saved_regs_size = offset;
  if (frame.emit_frame_chain)
    {
      /* FP and LR are placed in the linkage record.  */
      frame.reg_offset[R29_REGNUM] = offset;
      frame.wb_candidate1 = R29_REGNUM;
      frame.reg_offset[R30_REGNUM] = offset + UNITS_PER_WORD;
      frame.wb_candidate2 = R30_REGNUM;
      offset += 2 * UNITS_PER_WORD;
    }

  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
      {
	frame.reg_offset[regno] = offset;
	if (frame.wb_candidate1 == INVALID_REGNUM)
	  frame.wb_candidate1 = regno;
	else if (frame.wb_candidate2 == INVALID_REGNUM)
	  frame.wb_candidate2 = regno;
	offset += UNITS_PER_WORD;
      }

  poly_int64 max_int_offset = offset;
  offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);
  bool has_align_gap = maybe_ne (offset, max_int_offset);

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    if (known_eq (frame.reg_offset[regno], SLOT_REQUIRED))
      {
	/* If there is an alignment gap between integer and fp callee-saves,
	   allocate the last fp register to it if possible.  */
	if (regno == last_fp_reg
	    && has_align_gap
	    && known_eq (vector_save_size, 8)
	    && multiple_p (offset, 16))
	  {
	    frame.reg_offset[regno] = max_int_offset;
	    break;
	  }

	frame.reg_offset[regno] = offset;
	if (frame.wb_candidate1 == INVALID_REGNUM)
	  frame.wb_candidate1 = regno;
	else if (frame.wb_candidate2 == INVALID_REGNUM
		 && frame.wb_candidate1 >= V0_REGNUM)
	  frame.wb_candidate2 = regno;
	offset += vector_save_size;
      }

  offset = aligned_upper_bound (offset, STACK_BOUNDARY / BITS_PER_UNIT);

  frame.saved_regs_size = offset;

  poly_int64 varargs_and_saved_regs_size = offset + frame.saved_varargs_size;

  poly_int64 above_outgoing_args
    = aligned_upper_bound (varargs_and_saved_regs_size
			   + get_frame_size (),
			   STACK_BOUNDARY / BITS_PER_UNIT);

  frame.hard_fp_offset
    = above_outgoing_args - frame.below_hard_fp_saved_regs_size;

  /* Both these values are already aligned.  */
  gcc_assert (multiple_p (crtl->outgoing_args_size,
			  STACK_BOUNDARY / BITS_PER_UNIT));
  frame.frame_size = above_outgoing_args + crtl->outgoing_args_size;

  frame.locals_offset = frame.saved_varargs_size;

  frame.initial_adjust = 0;
  frame.final_adjust = 0;
  frame.callee_adjust = 0;
  frame.sve_callee_adjust = 0;
  frame.callee_offset = 0;

  HOST_WIDE_INT max_push_offset = 0;
  if (frame.wb_candidate2 != INVALID_REGNUM)
    max_push_offset = 512;
  else if (frame.wb_candidate1 != INVALID_REGNUM)
    max_push_offset = 256;

  HOST_WIDE_INT const_size, const_outgoing_args_size, const_fp_offset;
  HOST_WIDE_INT const_saved_regs_size;
  if (frame.frame_size.is_constant (&const_size)
      && const_size < max_push_offset
      && known_eq (frame.hard_fp_offset, const_size))
    {
      /* Simple, small frame with no outgoing arguments:

	 stp reg1, reg2, [sp, -frame_size]!
	 stp reg3, reg4, [sp, 16]  */
      frame.callee_adjust = const_size;
    }
  else if (crtl->outgoing_args_size.is_constant (&const_outgoing_args_size)
	   && frame.saved_regs_size.is_constant (&const_saved_regs_size)
	   && const_outgoing_args_size + const_saved_regs_size < 512
	   /* We could handle this case even with outgoing args, provided
	      that the number of args left us with valid offsets for all
	      predicate and vector save slots.  It's such a rare case that
	      it hardly seems worth the effort though.  */
	   && (!saves_below_hard_fp_p || const_outgoing_args_size == 0)
	   && !(cfun->calls_alloca
		&& frame.hard_fp_offset.is_constant (&const_fp_offset)
		&& const_fp_offset < max_push_offset))
    {
      /* Frame with small outgoing arguments:

	 sub sp, sp, frame_size
	 stp reg1, reg2, [sp, outgoing_args_size]
	 stp reg3, reg4, [sp, outgoing_args_size + 16]  */
      frame.initial_adjust = frame.frame_size;
      frame.callee_offset = const_outgoing_args_size;
    }
  else if (saves_below_hard_fp_p
	   && known_eq (frame.saved_regs_size,
			frame.below_hard_fp_saved_regs_size))
    {
      /* Frame in which all saves are SVE saves:

	 sub sp, sp, hard_fp_offset + below_hard_fp_saved_regs_size
	 save SVE registers relative to SP
	 sub sp, sp, outgoing_args_size  */
      frame.initial_adjust = (frame.hard_fp_offset
			      + frame.below_hard_fp_saved_regs_size);
      frame.final_adjust = crtl->outgoing_args_size;
    }
  else if (frame.hard_fp_offset.is_constant (&const_fp_offset)
	   && const_fp_offset < max_push_offset)
    {
      /* Frame with large outgoing arguments or SVE saves, but with
	 a small local area:

	 stp reg1, reg2, [sp, -hard_fp_offset]!
	 stp reg3, reg4, [sp, 16]
	 [sub sp, sp, below_hard_fp_saved_regs_size]
	 [save SVE registers relative to SP]
	 sub sp, sp, outgoing_args_size  */
      frame.callee_adjust = const_fp_offset;
      frame.sve_callee_adjust = frame.below_hard_fp_saved_regs_size;
      frame.final_adjust = crtl->outgoing_args_size;
    }
  else
    {
      /* Frame with large local area and outgoing arguments or SVE saves,
	 using frame pointer:

	 sub sp, sp, hard_fp_offset
	 stp x29, x30, [sp, 0]
	 add x29, sp, 0
	 stp reg3, reg4, [sp, 16]
	 [sub sp, sp, below_hard_fp_saved_regs_size]
	 [save SVE registers relative to SP]
	 sub sp, sp, outgoing_args_size  */
      frame.initial_adjust = frame.hard_fp_offset;
      frame.sve_callee_adjust = frame.below_hard_fp_saved_regs_size;
      frame.final_adjust = crtl->outgoing_args_size;
    }

  /* Make sure the individual adjustments add up to the full frame size.  */
  gcc_assert (known_eq (frame.initial_adjust
			+ frame.callee_adjust
			+ frame.sve_callee_adjust
			+ frame.final_adjust, frame.frame_size));

  frame.laid_out = true;
}

/* Return true if the register REGNO is saved on entry to
   the current function.  */

static bool
aarch64_register_saved_on_entry (int regno)
{
  return known_ge (cfun->machine->frame.reg_offset[regno], 0);
}

/* Return the next register up from REGNO up to LIMIT for the callee
   to save.  */

static unsigned
aarch64_next_callee_save (unsigned regno, unsigned limit)
{
  while (regno <= limit && !aarch64_register_saved_on_entry (regno))
    regno ++;
  return regno;
}

/* Push the register number REGNO of mode MODE to the stack with write-back
   adjusting the stack by ADJUSTMENT.  */

static void
aarch64_pushwb_single_reg (machine_mode mode, unsigned regno,
			   HOST_WIDE_INT adjustment)
 {
  rtx base_rtx = stack_pointer_rtx;
  rtx insn, reg, mem;

  reg = gen_rtx_REG (mode, regno);
  mem = gen_rtx_PRE_MODIFY (Pmode, base_rtx,
			    plus_constant (Pmode, base_rtx, -adjustment));
  mem = gen_frame_mem (mode, mem);

  insn = emit_move_insn (mem, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Generate and return an instruction to store the pair of registers
   REG and REG2 of mode MODE to location BASE with write-back adjusting
   the stack location BASE by ADJUSTMENT.  */

static rtx
aarch64_gen_storewb_pair (machine_mode mode, rtx base, rtx reg, rtx reg2,
			  HOST_WIDE_INT adjustment)
{
  switch (mode)
    {
    case E_DImode:
      return gen_storewb_pairdi_di (base, base, reg, reg2,
				    GEN_INT (-adjustment),
				    GEN_INT (UNITS_PER_WORD - adjustment));
    case E_DFmode:
      return gen_storewb_pairdf_di (base, base, reg, reg2,
				    GEN_INT (-adjustment),
				    GEN_INT (UNITS_PER_WORD - adjustment));
    case E_TFmode:
      return gen_storewb_pairtf_di (base, base, reg, reg2,
				    GEN_INT (-adjustment),
				    GEN_INT (UNITS_PER_VREG - adjustment));
    default:
      gcc_unreachable ();
    }
}

/* Push registers numbered REGNO1 and REGNO2 to the stack, adjusting the
   stack pointer by ADJUSTMENT.  */

static void
aarch64_push_regs (unsigned regno1, unsigned regno2, HOST_WIDE_INT adjustment)
{
  rtx_insn *insn;
  machine_mode mode = aarch64_reg_save_mode (regno1);

  if (regno2 == INVALID_REGNUM)
    return aarch64_pushwb_single_reg (mode, regno1, adjustment);

  rtx reg1 = gen_rtx_REG (mode, regno1);
  rtx reg2 = gen_rtx_REG (mode, regno2);

  insn = emit_insn (aarch64_gen_storewb_pair (mode, stack_pointer_rtx, reg1,
					      reg2, adjustment));
  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 2)) = 1;
  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 1)) = 1;
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Load the pair of register REG, REG2 of mode MODE from stack location BASE,
   adjusting it by ADJUSTMENT afterwards.  */

static rtx
aarch64_gen_loadwb_pair (machine_mode mode, rtx base, rtx reg, rtx reg2,
			 HOST_WIDE_INT adjustment)
{
  switch (mode)
    {
    case E_DImode:
      return gen_loadwb_pairdi_di (base, base, reg, reg2, GEN_INT (adjustment),
				   GEN_INT (UNITS_PER_WORD));
    case E_DFmode:
      return gen_loadwb_pairdf_di (base, base, reg, reg2, GEN_INT (adjustment),
				   GEN_INT (UNITS_PER_WORD));
    case E_TFmode:
      return gen_loadwb_pairtf_di (base, base, reg, reg2, GEN_INT (adjustment),
				   GEN_INT (UNITS_PER_VREG));
    default:
      gcc_unreachable ();
    }
}

/* Pop the two registers numbered REGNO1, REGNO2 from the stack, adjusting it
   afterwards by ADJUSTMENT and writing the appropriate REG_CFA_RESTORE notes
   into CFI_OPS.  */

static void
aarch64_pop_regs (unsigned regno1, unsigned regno2, HOST_WIDE_INT adjustment,
		  rtx *cfi_ops)
{
  machine_mode mode = aarch64_reg_save_mode (regno1);
  rtx reg1 = gen_rtx_REG (mode, regno1);

  *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg1, *cfi_ops);

  if (regno2 == INVALID_REGNUM)
    {
      rtx mem = plus_constant (Pmode, stack_pointer_rtx, adjustment);
      mem = gen_rtx_POST_MODIFY (Pmode, stack_pointer_rtx, mem);
      emit_move_insn (reg1, gen_frame_mem (mode, mem));
    }
  else
    {
      rtx reg2 = gen_rtx_REG (mode, regno2);
      *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg2, *cfi_ops);
      emit_insn (aarch64_gen_loadwb_pair (mode, stack_pointer_rtx, reg1,
					  reg2, adjustment));
    }
}

/* Generate and return a store pair instruction of mode MODE to store
   register REG1 to MEM1 and register REG2 to MEM2.  */

static rtx
aarch64_gen_store_pair (machine_mode mode, rtx mem1, rtx reg1, rtx mem2,
			rtx reg2)
{
  switch (mode)
    {
    case E_DImode:
      return gen_store_pair_dw_didi (mem1, reg1, mem2, reg2);

    case E_DFmode:
      return gen_store_pair_dw_dfdf (mem1, reg1, mem2, reg2);

    case E_TFmode:
      return gen_store_pair_dw_tftf (mem1, reg1, mem2, reg2);

    default:
      gcc_unreachable ();
    }
}

/* Generate and regurn a load pair isntruction of mode MODE to load register
   REG1 from MEM1 and register REG2 from MEM2.  */

static rtx
aarch64_gen_load_pair (machine_mode mode, rtx reg1, rtx mem1, rtx reg2,
		       rtx mem2)
{
  switch (mode)
    {
    case E_DImode:
      return gen_load_pair_dw_didi (reg1, mem1, reg2, mem2);

    case E_DFmode:
      return gen_load_pair_dw_dfdf (reg1, mem1, reg2, mem2);

    case E_TFmode:
      return gen_load_pair_dw_tftf (reg1, mem1, reg2, mem2);

    default:
      gcc_unreachable ();
    }
}

/* Return TRUE if return address signing should be enabled for the current
   function, otherwise return FALSE.  */

bool
aarch64_return_address_signing_enabled (void)
{
  /* This function should only be called after frame laid out.   */
  gcc_assert (cfun->machine->frame.laid_out);

  /* If signing scope is AARCH64_FUNCTION_NON_LEAF, we only sign a leaf function
     if its LR is pushed onto stack.  */
  return (aarch64_ra_sign_scope == AARCH64_FUNCTION_ALL
	  || (aarch64_ra_sign_scope == AARCH64_FUNCTION_NON_LEAF
	      && known_ge (cfun->machine->frame.reg_offset[LR_REGNUM], 0)));
}

/* Return TRUE if Branch Target Identification Mechanism is enabled.  */
bool
aarch64_bti_enabled (void)
{
  return (aarch64_enable_bti == 1);
}

/* The caller is going to use ST1D or LD1D to save or restore an SVE
   register in mode MODE at BASE_RTX + OFFSET, where OFFSET is in
   the range [1, 16] * GET_MODE_SIZE (MODE).  Prepare for this by:

     (1) updating BASE_RTX + OFFSET so that it is a legitimate ST1D
	 or LD1D address

     (2) setting PRED to a valid predicate register for the ST1D or LD1D,
	 if the variable isn't already nonnull

   (1) is needed when OFFSET is in the range [8, 16] * GET_MODE_SIZE (MODE).
   Handle this case using a temporary base register that is suitable for
   all offsets in that range.  Use ANCHOR_REG as this base register if it
   is nonnull, otherwise create a new register and store it in ANCHOR_REG.  */

static inline void
aarch64_adjust_sve_callee_save_base (machine_mode mode, rtx &base_rtx,
				     rtx &anchor_reg, poly_int64 &offset,
				     rtx &ptrue)
{
  if (maybe_ge (offset, 8 * GET_MODE_SIZE (mode)))
    {
      /* This is the maximum valid offset of the anchor from the base.
	 Lower values would be valid too.  */
      poly_int64 anchor_offset = 16 * GET_MODE_SIZE (mode);
      if (!anchor_reg)
	{
	  anchor_reg = gen_rtx_REG (Pmode, STACK_CLASH_SVE_CFA_REGNUM);
	  emit_insn (gen_add3_insn (anchor_reg, base_rtx,
				    gen_int_mode (anchor_offset, Pmode)));
	}
      base_rtx = anchor_reg;
      offset -= anchor_offset;
    }
  if (!ptrue)
    {
      int pred_reg = cfun->machine->frame.spare_pred_reg;
      emit_move_insn (gen_rtx_REG (VNx16BImode, pred_reg),
		      CONSTM1_RTX (VNx16BImode));
      ptrue = gen_rtx_REG (VNx2BImode, pred_reg);
    }
}

/* Add a REG_CFA_EXPRESSION note to INSN to say that register REG
   is saved at BASE + OFFSET.  */

static void
aarch64_add_cfa_expression (rtx_insn *insn, rtx reg,
			    rtx base, poly_int64 offset)
{
  rtx mem = gen_frame_mem (GET_MODE (reg),
			   plus_constant (Pmode, base, offset));
  add_reg_note (insn, REG_CFA_EXPRESSION, gen_rtx_SET (mem, reg));
}

/* Emit code to save the callee-saved registers from register number START
   to LIMIT to the stack at the location starting at offset START_OFFSET,
   skipping any write-back candidates if SKIP_WB is true.  HARD_FP_VALID_P
   is true if the hard frame pointer has been set up.  */

static void
aarch64_save_callee_saves (poly_int64 start_offset,
			   unsigned start, unsigned limit, bool skip_wb,
			   bool hard_fp_valid_p)
{
  rtx_insn *insn;
  unsigned regno;
  unsigned regno2;
  rtx anchor_reg = NULL_RTX, ptrue = NULL_RTX;

  for (regno = aarch64_next_callee_save (start, limit);
       regno <= limit;
       regno = aarch64_next_callee_save (regno + 1, limit))
    {
      rtx reg, mem;
      poly_int64 offset;
      bool frame_related_p = aarch64_emit_cfi_for_reg_p (regno);

      if (skip_wb
	  && (regno == cfun->machine->frame.wb_candidate1
	      || regno == cfun->machine->frame.wb_candidate2))
	continue;

      if (cfun->machine->reg_is_wrapped_separately[regno])
	continue;

      machine_mode mode = aarch64_reg_save_mode (regno);
      reg = gen_rtx_REG (mode, regno);
      offset = start_offset + cfun->machine->frame.reg_offset[regno];
      rtx base_rtx = stack_pointer_rtx;
      poly_int64 sp_offset = offset;

      HOST_WIDE_INT const_offset;
      if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	aarch64_adjust_sve_callee_save_base (mode, base_rtx, anchor_reg,
					     offset, ptrue);
      else if (GP_REGNUM_P (regno)
	       && (!offset.is_constant (&const_offset) || const_offset >= 512))
	{
	  gcc_assert (known_eq (start_offset, 0));
	  poly_int64 fp_offset
	    = cfun->machine->frame.below_hard_fp_saved_regs_size;
	  if (hard_fp_valid_p)
	    base_rtx = hard_frame_pointer_rtx;
	  else
	    {
	      if (!anchor_reg)
		{
		  anchor_reg = gen_rtx_REG (Pmode, STACK_CLASH_SVE_CFA_REGNUM);
		  emit_insn (gen_add3_insn (anchor_reg, base_rtx,
					    gen_int_mode (fp_offset, Pmode)));
		}
	      base_rtx = anchor_reg;
	    }
	  offset -= fp_offset;
	}
      mem = gen_frame_mem (mode, plus_constant (Pmode, base_rtx, offset));
      bool need_cfa_note_p = (base_rtx != stack_pointer_rtx);

      if (!aarch64_sve_mode_p (mode)
	  && (regno2 = aarch64_next_callee_save (regno + 1, limit)) <= limit
	  && !cfun->machine->reg_is_wrapped_separately[regno2]
	  && known_eq (GET_MODE_SIZE (mode),
		       cfun->machine->frame.reg_offset[regno2]
		       - cfun->machine->frame.reg_offset[regno]))
	{
	  rtx reg2 = gen_rtx_REG (mode, regno2);
	  rtx mem2;

	  offset += GET_MODE_SIZE (mode);
	  mem2 = gen_frame_mem (mode, plus_constant (Pmode, base_rtx, offset));
	  insn = emit_insn (aarch64_gen_store_pair (mode, mem, reg, mem2,
						    reg2));

	  /* The first part of a frame-related parallel insn is
	     always assumed to be relevant to the frame
	     calculations; subsequent parts, are only
	     frame-related if explicitly marked.  */
	  if (aarch64_emit_cfi_for_reg_p (regno2))
	    {
	      if (need_cfa_note_p)
		aarch64_add_cfa_expression (insn, reg2, stack_pointer_rtx,
					    sp_offset + GET_MODE_SIZE (mode));
	      else
		RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 1)) = 1;
	    }

	  regno = regno2;
	}
      else if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	{
	  insn = emit_insn (gen_aarch64_pred_mov (mode, mem, ptrue, reg));
	  need_cfa_note_p = true;
	}
      else if (aarch64_sve_mode_p (mode))
	insn = emit_insn (gen_rtx_SET (mem, reg));
      else
	insn = emit_move_insn (mem, reg);

      RTX_FRAME_RELATED_P (insn) = frame_related_p;
      if (frame_related_p && need_cfa_note_p)
	aarch64_add_cfa_expression (insn, reg, stack_pointer_rtx, sp_offset);
    }
}

/* Emit code to restore the callee registers from register number START
   up to and including LIMIT.  Restore from the stack offset START_OFFSET,
   skipping any write-back candidates if SKIP_WB is true.  Write the
   appropriate REG_CFA_RESTORE notes into CFI_OPS.  */

static void
aarch64_restore_callee_saves (poly_int64 start_offset, unsigned start,
			      unsigned limit, bool skip_wb, rtx *cfi_ops)
{
  unsigned regno;
  unsigned regno2;
  poly_int64 offset;
  rtx anchor_reg = NULL_RTX, ptrue = NULL_RTX;

  for (regno = aarch64_next_callee_save (start, limit);
       regno <= limit;
       regno = aarch64_next_callee_save (regno + 1, limit))
    {
      bool frame_related_p = aarch64_emit_cfi_for_reg_p (regno);
      if (cfun->machine->reg_is_wrapped_separately[regno])
	continue;

      rtx reg, mem;

      if (skip_wb
	  && (regno == cfun->machine->frame.wb_candidate1
	      || regno == cfun->machine->frame.wb_candidate2))
	continue;

      machine_mode mode = aarch64_reg_save_mode (regno);
      reg = gen_rtx_REG (mode, regno);
      offset = start_offset + cfun->machine->frame.reg_offset[regno];
      rtx base_rtx = stack_pointer_rtx;
      if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	aarch64_adjust_sve_callee_save_base (mode, base_rtx, anchor_reg,
					     offset, ptrue);
      mem = gen_frame_mem (mode, plus_constant (Pmode, base_rtx, offset));

      if (!aarch64_sve_mode_p (mode)
	  && (regno2 = aarch64_next_callee_save (regno + 1, limit)) <= limit
	  && !cfun->machine->reg_is_wrapped_separately[regno2]
	  && known_eq (GET_MODE_SIZE (mode),
		       cfun->machine->frame.reg_offset[regno2]
		       - cfun->machine->frame.reg_offset[regno]))
	{
	  rtx reg2 = gen_rtx_REG (mode, regno2);
	  rtx mem2;

	  offset += GET_MODE_SIZE (mode);
	  mem2 = gen_frame_mem (mode, plus_constant (Pmode, base_rtx, offset));
	  emit_insn (aarch64_gen_load_pair (mode, reg, mem, reg2, mem2));

	  *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg2, *cfi_ops);
	  regno = regno2;
	}
      else if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	emit_insn (gen_aarch64_pred_mov (mode, reg, ptrue, mem));
      else if (aarch64_sve_mode_p (mode))
	emit_insn (gen_rtx_SET (reg, mem));
      else
	emit_move_insn (reg, mem);
      if (frame_related_p)
	*cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg, *cfi_ops);
    }
}

/* Return true if OFFSET is a signed 4-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_4bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -8, 7));
}

/* Return true if OFFSET is a unsigned 6-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_6bit_unsigned_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, 0, 63));
}

/* Return true if OFFSET is a signed 7-bit value multiplied by the size
   of MODE.  */

bool
aarch64_offset_7bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -64, 63));
}

/* Return true if OFFSET is a signed 9-bit value.  */

bool
aarch64_offset_9bit_signed_unscaled_p (machine_mode mode ATTRIBUTE_UNUSED,
				       poly_int64 offset)
{
  HOST_WIDE_INT const_offset;
  return (offset.is_constant (&const_offset)
	  && IN_RANGE (const_offset, -256, 255));
}

/* Return true if OFFSET is a signed 9-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_9bit_signed_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, -256, 255));
}

/* Return true if OFFSET is an unsigned 12-bit value multiplied by the size
   of MODE.  */

static inline bool
offset_12bit_unsigned_scaled_p (machine_mode mode, poly_int64 offset)
{
  HOST_WIDE_INT multiple;
  return (constant_multiple_p (offset, GET_MODE_SIZE (mode), &multiple)
	  && IN_RANGE (multiple, 0, 4095));
}

/* Implement TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS.  */

static sbitmap
aarch64_get_separate_components (void)
{
  sbitmap components = sbitmap_alloc (LAST_SAVED_REGNUM + 1);
  bitmap_clear (components);

  /* The registers we need saved to the frame.  */
  for (unsigned regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    if (aarch64_register_saved_on_entry (regno))
      {
	/* Punt on saves and restores that use ST1D and LD1D.  We could
	   try to be smarter, but it would involve making sure that the
	   spare predicate register itself is safe to use at the save
	   and restore points.  Also, when a frame pointer is being used,
	   the slots are often out of reach of ST1D and LD1D anyway.  */
	machine_mode mode = aarch64_reg_save_mode (regno);
	if (mode == VNx2DImode && BYTES_BIG_ENDIAN)
	  continue;

	poly_int64 offset = cfun->machine->frame.reg_offset[regno];

	/* If the register is saved in the first SVE save slot, we use
	   it as a stack probe for -fstack-clash-protection.  */
	if (flag_stack_clash_protection
	    && maybe_ne (cfun->machine->frame.below_hard_fp_saved_regs_size, 0)
	    && known_eq (offset, 0))
	  continue;

	/* Get the offset relative to the register we'll use.  */
	if (frame_pointer_needed)
	  offset -= cfun->machine->frame.below_hard_fp_saved_regs_size;
	else
	  offset += crtl->outgoing_args_size;

	/* Check that we can access the stack slot of the register with one
	   direct load with no adjustments needed.  */
	if (aarch64_sve_mode_p (mode)
	    ? offset_9bit_signed_scaled_p (mode, offset)
	    : offset_12bit_unsigned_scaled_p (mode, offset))
	  bitmap_set_bit (components, regno);
      }

  /* Don't mess with the hard frame pointer.  */
  if (frame_pointer_needed)
    bitmap_clear_bit (components, HARD_FRAME_POINTER_REGNUM);

  /* If the spare predicate register used by big-endian SVE code
     is call-preserved, it must be saved in the main prologue
     before any saves that use it.  */
  if (cfun->machine->frame.spare_pred_reg != INVALID_REGNUM)
    bitmap_clear_bit (components, cfun->machine->frame.spare_pred_reg);

  unsigned reg1 = cfun->machine->frame.wb_candidate1;
  unsigned reg2 = cfun->machine->frame.wb_candidate2;
  /* If registers have been chosen to be stored/restored with
     writeback don't interfere with them to avoid having to output explicit
     stack adjustment instructions.  */
  if (reg2 != INVALID_REGNUM)
    bitmap_clear_bit (components, reg2);
  if (reg1 != INVALID_REGNUM)
    bitmap_clear_bit (components, reg1);

  bitmap_clear_bit (components, LR_REGNUM);
  bitmap_clear_bit (components, SP_REGNUM);

  return components;
}

/* Implement TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB.  */

static sbitmap
aarch64_components_for_bb (basic_block bb)
{
  bitmap in = DF_LIVE_IN (bb);
  bitmap gen = &DF_LIVE_BB_INFO (bb)->gen;
  bitmap kill = &DF_LIVE_BB_INFO (bb)->kill;

  sbitmap components = sbitmap_alloc (LAST_SAVED_REGNUM + 1);
  bitmap_clear (components);

  /* Clobbered registers don't generate values in any meaningful sense,
     since nothing after the clobber can rely on their value.  And we can't
     say that partially-clobbered registers are unconditionally killed,
     because whether they're killed or not depends on the mode of the
     value they're holding.  Thus partially call-clobbered registers
     appear in neither the kill set nor the gen set.

     Check manually for any calls that clobber more of a register than the
     current function can.  */
  function_abi_aggregator callee_abis;
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (CALL_P (insn))
      callee_abis.note_callee_abi (insn_callee_abi (insn));
  HARD_REG_SET extra_caller_saves = callee_abis.caller_save_regs (*crtl->abi);

  /* GPRs are used in a bb if they are in the IN, GEN, or KILL sets.  */
  for (unsigned regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    if (!fixed_regs[regno]
	&& !crtl->abi->clobbers_full_reg_p (regno)
	&& (TEST_HARD_REG_BIT (extra_caller_saves, regno)
	    || bitmap_bit_p (in, regno)
	    || bitmap_bit_p (gen, regno)
	    || bitmap_bit_p (kill, regno)))
      {
	bitmap_set_bit (components, regno);

	/* If there is a callee-save at an adjacent offset, add it too
	   to increase the use of LDP/STP.  */
	poly_int64 offset = cfun->machine->frame.reg_offset[regno];
	unsigned regno2 = multiple_p (offset, 16) ? regno + 1 : regno - 1;

	if (regno2 <= LAST_SAVED_REGNUM)
	  {
	    poly_int64 offset2 = cfun->machine->frame.reg_offset[regno2];
	    if (regno < regno2
		? known_eq (offset + 8, offset2)
		: multiple_p (offset2, 16) && known_eq (offset2 + 8, offset))
	      bitmap_set_bit (components, regno2);
	  }
      }

  return components;
}

/* Implement TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS.
   Nothing to do for aarch64.  */

static void
aarch64_disqualify_components (sbitmap, edge, sbitmap, bool)
{
}

/* Return the next set bit in BMP from START onwards.  Return the total number
   of bits in BMP if no set bit is found at or after START.  */

static unsigned int
aarch64_get_next_set_bit (sbitmap bmp, unsigned int start)
{
  unsigned int nbits = SBITMAP_SIZE (bmp);
  if (start == nbits)
    return start;

  gcc_assert (start < nbits);
  for (unsigned int i = start; i < nbits; i++)
    if (bitmap_bit_p (bmp, i))
      return i;

  return nbits;
}

/* Do the work for aarch64_emit_prologue_components and
   aarch64_emit_epilogue_components.  COMPONENTS is the bitmap of registers
   to save/restore, PROLOGUE_P indicates whether to emit the prologue sequence
   for these components or the epilogue sequence.  That is, it determines
   whether we should emit stores or loads and what kind of CFA notes to attach
   to the insns.  Otherwise the logic for the two sequences is very
   similar.  */

static void
aarch64_process_components (sbitmap components, bool prologue_p)
{
  rtx ptr_reg = gen_rtx_REG (Pmode, frame_pointer_needed
			     ? HARD_FRAME_POINTER_REGNUM
			     : STACK_POINTER_REGNUM);

  unsigned last_regno = SBITMAP_SIZE (components);
  unsigned regno = aarch64_get_next_set_bit (components, R0_REGNUM);
  rtx_insn *insn = NULL;

  while (regno != last_regno)
    {
      bool frame_related_p = aarch64_emit_cfi_for_reg_p (regno);
      machine_mode mode = aarch64_reg_save_mode (regno);
      
      rtx reg = gen_rtx_REG (mode, regno);
      poly_int64 offset = cfun->machine->frame.reg_offset[regno];
      if (frame_pointer_needed)
	offset -= cfun->machine->frame.below_hard_fp_saved_regs_size;
      else
	offset += crtl->outgoing_args_size;

      rtx addr = plus_constant (Pmode, ptr_reg, offset);
      rtx mem = gen_frame_mem (mode, addr);

      rtx set = prologue_p ? gen_rtx_SET (mem, reg) : gen_rtx_SET (reg, mem);
      unsigned regno2 = aarch64_get_next_set_bit (components, regno + 1);
      /* No more registers to handle after REGNO.
	 Emit a single save/restore and exit.  */
      if (regno2 == last_regno)
	{
	  insn = emit_insn (set);
	  if (frame_related_p)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      if (prologue_p)
		add_reg_note (insn, REG_CFA_OFFSET, copy_rtx (set));
	      else
		add_reg_note (insn, REG_CFA_RESTORE, reg);
	    }
	  break;
	}

      poly_int64 offset2 = cfun->machine->frame.reg_offset[regno2];
      /* The next register is not of the same class or its offset is not
	 mergeable with the current one into a pair.  */
      if (aarch64_sve_mode_p (mode)
	  || !satisfies_constraint_Ump (mem)
	  || GP_REGNUM_P (regno) != GP_REGNUM_P (regno2)
	  || (crtl->abi->id () == ARM_PCS_SIMD && FP_REGNUM_P (regno))
	  || maybe_ne ((offset2 - cfun->machine->frame.reg_offset[regno]),
		       GET_MODE_SIZE (mode)))
	{
	  insn = emit_insn (set);
	  if (frame_related_p)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      if (prologue_p)
		add_reg_note (insn, REG_CFA_OFFSET, copy_rtx (set));
	      else
		add_reg_note (insn, REG_CFA_RESTORE, reg);
	    }

	  regno = regno2;
	  continue;
	}

      bool frame_related2_p = aarch64_emit_cfi_for_reg_p (regno2);

      /* REGNO2 can be saved/restored in a pair with REGNO.  */
      rtx reg2 = gen_rtx_REG (mode, regno2);
      if (frame_pointer_needed)
	offset2 -= cfun->machine->frame.below_hard_fp_saved_regs_size;
      else
	offset2 += crtl->outgoing_args_size;
      rtx addr2 = plus_constant (Pmode, ptr_reg, offset2);
      rtx mem2 = gen_frame_mem (mode, addr2);
      rtx set2 = prologue_p ? gen_rtx_SET (mem2, reg2)
			     : gen_rtx_SET (reg2, mem2);

      if (prologue_p)
	insn = emit_insn (aarch64_gen_store_pair (mode, mem, reg, mem2, reg2));
      else
	insn = emit_insn (aarch64_gen_load_pair (mode, reg, mem, reg2, mem2));

      if (frame_related_p || frame_related2_p)
	{
	  RTX_FRAME_RELATED_P (insn) = 1;
	  if (prologue_p)
	    {
	      if (frame_related_p)
		add_reg_note (insn, REG_CFA_OFFSET, set);
	      if (frame_related2_p)
		add_reg_note (insn, REG_CFA_OFFSET, set2);
	    }
	  else
	    {
	      if (frame_related_p)
		add_reg_note (insn, REG_CFA_RESTORE, reg);
	      if (frame_related2_p)
		add_reg_note (insn, REG_CFA_RESTORE, reg2);
	    }
	}

      regno = aarch64_get_next_set_bit (components, regno2 + 1);
    }
}

/* Implement TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS.  */

static void
aarch64_emit_prologue_components (sbitmap components)
{
  aarch64_process_components (components, true);
}

/* Implement TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS.  */

static void
aarch64_emit_epilogue_components (sbitmap components)
{
  aarch64_process_components (components, false);
}

/* Implement TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS.  */

static void
aarch64_set_handled_components (sbitmap components)
{
  for (unsigned regno = 0; regno <= LAST_SAVED_REGNUM; regno++)
    if (bitmap_bit_p (components, regno))
      cfun->machine->reg_is_wrapped_separately[regno] = true;
}

/* On AArch64 we have an ABI defined safe buffer.  This constant is used to
   determining the probe offset for alloca.  */

static HOST_WIDE_INT
aarch64_stack_clash_protection_alloca_probe_range (void)
{
  return STACK_CLASH_CALLER_GUARD;
}


/* Allocate POLY_SIZE bytes of stack space using TEMP1 and TEMP2 as scratch
   registers.  If POLY_SIZE is not large enough to require a probe this function
   will only adjust the stack.  When allocating the stack space
   FRAME_RELATED_P is then used to indicate if the allocation is frame related.
   FINAL_ADJUSTMENT_P indicates whether we are allocating the outgoing
   arguments.  If we are then we ensure that any allocation larger than the ABI
   defined buffer needs a probe so that the invariant of having a 1KB buffer is
   maintained.

   We emit barriers after each stack adjustment to prevent optimizations from
   breaking the invariant that we never drop the stack more than a page.  This
   invariant is needed to make it easier to correctly handle asynchronous
   events, e.g. if we were to allow the stack to be dropped by more than a page
   and then have multiple probes up and we take a signal somewhere in between
   then the signal handler doesn't know the state of the stack and can make no
   assumptions about which pages have been probed.  */

static void
aarch64_allocate_and_probe_stack_space (rtx temp1, rtx temp2,
					poly_int64 poly_size,
					bool frame_related_p,
					bool final_adjustment_p)
{
  HOST_WIDE_INT guard_size
    = 1 << param_stack_clash_protection_guard_size;
  HOST_WIDE_INT guard_used_by_caller = STACK_CLASH_CALLER_GUARD;
  HOST_WIDE_INT min_probe_threshold
    = (final_adjustment_p
       ? guard_used_by_caller
       : guard_size - guard_used_by_caller);
  /* When doing the final adjustment for the outgoing arguments, take into
     account any unprobed space there is above the current SP.  There are
     two cases:

     - When saving SVE registers below the hard frame pointer, we force
       the lowest save to take place in the prologue before doing the final
       adjustment (i.e. we don't allow the save to be shrink-wrapped).
       This acts as a probe at SP, so there is no unprobed space.

     - When there are no SVE register saves, we use the store of the link
       register as a probe.  We can't assume that LR was saved at position 0
       though, so treat any space below it as unprobed.  */
  if (final_adjustment_p
      && known_eq (cfun->machine->frame.below_hard_fp_saved_regs_size, 0))
    {
      poly_int64 lr_offset = cfun->machine->frame.reg_offset[LR_REGNUM];
      if (known_ge (lr_offset, 0))
	min_probe_threshold -= lr_offset.to_constant ();
      else
	gcc_assert (!flag_stack_clash_protection || known_eq (poly_size, 0));
    }

  poly_int64 frame_size = cfun->machine->frame.frame_size;

  /* We should always have a positive probe threshold.  */
  gcc_assert (min_probe_threshold > 0);

  if (flag_stack_clash_protection && !final_adjustment_p)
    {
      poly_int64 initial_adjust = cfun->machine->frame.initial_adjust;
      poly_int64 sve_callee_adjust = cfun->machine->frame.sve_callee_adjust;
      poly_int64 final_adjust = cfun->machine->frame.final_adjust;

      if (known_eq (frame_size, 0))
	{
	  dump_stack_clash_frame_info (NO_PROBE_NO_FRAME, false);
	}
      else if (known_lt (initial_adjust + sve_callee_adjust,
			 guard_size - guard_used_by_caller)
	       && known_lt (final_adjust, guard_used_by_caller))
	{
	  dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);
	}
    }

  /* If SIZE is not large enough to require probing, just adjust the stack and
     exit.  */
  if (known_lt (poly_size, min_probe_threshold)
      || !flag_stack_clash_protection)
    {
      aarch64_sub_sp (temp1, temp2, poly_size, frame_related_p);
      return;
    }

  HOST_WIDE_INT size;
  /* Handle the SVE non-constant case first.  */
  if (!poly_size.is_constant (&size))
    {
     if (dump_file)
      {
	fprintf (dump_file, "Stack clash SVE prologue: ");
	print_dec (poly_size, dump_file);
	fprintf (dump_file, " bytes, dynamic probing will be required.\n");
      }

      /* First calculate the amount of bytes we're actually spilling.  */
      aarch64_add_offset (Pmode, temp1, CONST0_RTX (Pmode),
			  poly_size, temp1, temp2, false, true);

      rtx_insn *insn = get_last_insn ();

      if (frame_related_p)
	{
	  /* This is done to provide unwinding information for the stack
	     adjustments we're about to do, however to prevent the optimizers
	     from removing the R11 move and leaving the CFA note (which would be
	     very wrong) we tie the old and new stack pointer together.
	     The tie will expand to nothing but the optimizers will not touch
	     the instruction.  */
	  rtx stack_ptr_copy = gen_rtx_REG (Pmode, STACK_CLASH_SVE_CFA_REGNUM);
	  emit_move_insn (stack_ptr_copy, stack_pointer_rtx);
	  emit_insn (gen_stack_tie (stack_ptr_copy, stack_pointer_rtx));

	  /* We want the CFA independent of the stack pointer for the
	     duration of the loop.  */
	  add_reg_note (insn, REG_CFA_DEF_CFA, stack_ptr_copy);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      rtx probe_const = gen_int_mode (min_probe_threshold, Pmode);
      rtx guard_const = gen_int_mode (guard_size, Pmode);

      insn = emit_insn (gen_probe_sve_stack_clash (Pmode, stack_pointer_rtx,
						   stack_pointer_rtx, temp1,
						   probe_const, guard_const));

      /* Now reset the CFA register if needed.  */
      if (frame_related_p)
	{
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				      gen_int_mode (poly_size, Pmode)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      return;
    }

  if (dump_file)
    fprintf (dump_file,
	     "Stack clash AArch64 prologue: " HOST_WIDE_INT_PRINT_DEC
	     " bytes, probing will be required.\n", size);

  /* Round size to the nearest multiple of guard_size, and calculate the
     residual as the difference between the original size and the rounded
     size.  */
  HOST_WIDE_INT rounded_size = ROUND_DOWN (size, guard_size);
  HOST_WIDE_INT residual = size - rounded_size;

  /* We can handle a small number of allocations/probes inline.  Otherwise
     punt to a loop.  */
  if (rounded_size <= STACK_CLASH_MAX_UNROLL_PAGES * guard_size)
    {
      for (HOST_WIDE_INT i = 0; i < rounded_size; i += guard_size)
	{
	  aarch64_sub_sp (NULL, temp2, guard_size, true);
	  emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					   guard_used_by_caller));
	  emit_insn (gen_blockage ());
	}
      dump_stack_clash_frame_info (PROBE_INLINE, size != rounded_size);
    }
  else
    {
      /* Compute the ending address.  */
      aarch64_add_offset (Pmode, temp1, stack_pointer_rtx, -rounded_size,
			  temp1, NULL, false, true);
      rtx_insn *insn = get_last_insn ();

      /* For the initial allocation, we don't have a frame pointer
	 set up, so we always need CFI notes.  If we're doing the
	 final allocation, then we may have a frame pointer, in which
	 case it is the CFA, otherwise we need CFI notes.

	 We can determine which allocation we are doing by looking at
	 the value of FRAME_RELATED_P since the final allocations are not
	 frame related.  */
      if (frame_related_p)
	{
	  /* We want the CFA independent of the stack pointer for the
	     duration of the loop.  */
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, temp1, rounded_size));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* This allocates and probes the stack.  Note that this re-uses some of
	 the existing Ada stack protection code.  However we are guaranteed not
	 to enter the non loop or residual branches of that code.

	 The non-loop part won't be entered because if our allocation amount
	 doesn't require a loop, the case above would handle it.

	 The residual amount won't be entered because TEMP1 is a mutliple of
	 the allocation size.  The residual will always be 0.  As such, the only
	 part we are actually using from that code is the loop setup.  The
	 actual probing is done in aarch64_output_probe_stack_range.  */
      insn = emit_insn (gen_probe_stack_range (stack_pointer_rtx,
					       stack_pointer_rtx, temp1));

      /* Now reset the CFA register if needed.  */
      if (frame_related_p)
	{
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx, rounded_size));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      emit_insn (gen_blockage ());
      dump_stack_clash_frame_info (PROBE_LOOP, size != rounded_size);
    }

  /* Handle any residuals.  Residuals of at least MIN_PROBE_THRESHOLD have to
     be probed.  This maintains the requirement that each page is probed at
     least once.  For initial probing we probe only if the allocation is
     more than GUARD_SIZE - buffer, and for the outgoing arguments we probe
     if the amount is larger than buffer.  GUARD_SIZE - buffer + buffer ==
     GUARD_SIZE.  This works that for any allocation that is large enough to
     trigger a probe here, we'll have at least one, and if they're not large
     enough for this code to emit anything for them, The page would have been
     probed by the saving of FP/LR either by this function or any callees.  If
     we don't have any callees then we won't have more stack adjustments and so
     are still safe.  */
  if (residual)
    {
      HOST_WIDE_INT residual_probe_offset = guard_used_by_caller;
      /* If we're doing final adjustments, and we've done any full page
	 allocations then any residual needs to be probed.  */
      if (final_adjustment_p && rounded_size != 0)
	min_probe_threshold = 0;
      /* If doing a small final adjustment, we always probe at offset 0.
	 This is done to avoid issues when LR is not at position 0 or when
	 the final adjustment is smaller than the probing offset.  */
      else if (final_adjustment_p && rounded_size == 0)
	residual_probe_offset = 0;

      aarch64_sub_sp (temp1, temp2, residual, frame_related_p);
      if (residual >= min_probe_threshold)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Stack clash AArch64 prologue residuals: "
		     HOST_WIDE_INT_PRINT_DEC " bytes, probing will be required."
		     "\n", residual);

	    emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					     residual_probe_offset));
	  emit_insn (gen_blockage ());
	}
    }
}

/* Return 1 if the register is used by the epilogue.  We need to say the
   return register is used, but only after epilogue generation is complete.
   Note that in the case of sibcalls, the values "used by the epilogue" are
   considered live at the start of the called function.

   For SIMD functions we need to return 1 for FP registers that are saved and
   restored by a function but are not zero in call_used_regs.  If we do not do 
   this optimizations may remove the restore of the register.  */

int
aarch64_epilogue_uses (int regno)
{
  if (epilogue_completed)
    {
      if (regno == LR_REGNUM)
	return 1;
    }
  return 0;
}

/* AArch64 stack frames generated by this compiler look like:

	+-------------------------------+
	|                               |
	|  incoming stack arguments     |
	|                               |
	+-------------------------------+
	|                               | <-- incoming stack pointer (aligned)
	|  callee-allocated save area   |
	|  for register varargs         |
	|                               |
	+-------------------------------+
	|  local variables              | <-- frame_pointer_rtx
	|                               |
	+-------------------------------+
	|  padding                      | \
	+-------------------------------+  |
	|  callee-saved registers       |  | frame.saved_regs_size
	+-------------------------------+  |
	|  LR'                          |  |
	+-------------------------------+  |
	|  FP'                          |  |
	+-------------------------------+  |<- hard_frame_pointer_rtx (aligned)
	|  SVE vector registers         |  | \
	+-------------------------------+  |  | below_hard_fp_saved_regs_size
	|  SVE predicate registers      | /  /
	+-------------------------------+
	|  dynamic allocation           |
	+-------------------------------+
	|  padding                      |
	+-------------------------------+
	|  outgoing stack arguments     | <-- arg_pointer
        |                               |
	+-------------------------------+
	|                               | <-- stack_pointer_rtx (aligned)

   Dynamic stack allocations via alloca() decrease stack_pointer_rtx
   but leave frame_pointer_rtx and hard_frame_pointer_rtx
   unchanged.

   By default for stack-clash we assume the guard is at least 64KB, but this
   value is configurable to either 4KB or 64KB.  We also force the guard size to
   be the same as the probing interval and both values are kept in sync.

   With those assumptions the callee can allocate up to 63KB (or 3KB depending
   on the guard size) of stack space without probing.

   When probing is needed, we emit a probe at the start of the prologue
   and every PARAM_STACK_CLASH_PROTECTION_GUARD_SIZE bytes thereafter.

   We have to track how much space has been allocated and the only stores
   to the stack we track as implicit probes are the FP/LR stores.

   For outgoing arguments we probe if the size is larger than 1KB, such that
   the ABI specified buffer is maintained for the next callee.

   The following registers are reserved during frame layout and should not be
   used for any other purpose:

   - r11: Used by stack clash protection when SVE is enabled, and also
	  as an anchor register when saving and restoring registers
   - r12(EP0) and r13(EP1): Used as temporaries for stack adjustment.
   - r14 and r15: Used for speculation tracking.
   - r16(IP0), r17(IP1): Used by indirect tailcalls.
   - r30(LR), r29(FP): Used by standard frame layout.

   These registers must be avoided in frame layout related code unless the
   explicit intention is to interact with one of the features listed above.  */

/* Generate the prologue instructions for entry into a function.
   Establish the stack frame by decreasing the stack pointer with a
   properly calculated size and, if necessary, create a frame record
   filled with the values of LR and previous frame pointer.  The
   current FP is also set up if it is in use.  */

void
aarch64_expand_prologue (void)
{
  poly_int64 frame_size = cfun->machine->frame.frame_size;
  poly_int64 initial_adjust = cfun->machine->frame.initial_adjust;
  HOST_WIDE_INT callee_adjust = cfun->machine->frame.callee_adjust;
  poly_int64 final_adjust = cfun->machine->frame.final_adjust;
  poly_int64 callee_offset = cfun->machine->frame.callee_offset;
  poly_int64 sve_callee_adjust = cfun->machine->frame.sve_callee_adjust;
  poly_int64 below_hard_fp_saved_regs_size
    = cfun->machine->frame.below_hard_fp_saved_regs_size;
  unsigned reg1 = cfun->machine->frame.wb_candidate1;
  unsigned reg2 = cfun->machine->frame.wb_candidate2;
  bool emit_frame_chain = cfun->machine->frame.emit_frame_chain;
  rtx_insn *insn;

  if (flag_stack_clash_protection && known_eq (callee_adjust, 0))
    {
      /* Fold the SVE allocation into the initial allocation.
	 We don't do this in aarch64_layout_arg to avoid pessimizing
	 the epilogue code.  */
      initial_adjust += sve_callee_adjust;
      sve_callee_adjust = 0;
    }

  /* Sign return address for functions.  */
  if (aarch64_return_address_signing_enabled ())
    {
      switch (aarch64_ra_sign_key)
	{
	  case AARCH64_KEY_A:
	    insn = emit_insn (gen_paciasp ());
	    break;
	  case AARCH64_KEY_B:
	    insn = emit_insn (gen_pacibsp ());
	    break;
	  default:
	    gcc_unreachable ();
	}
      add_reg_note (insn, REG_CFA_TOGGLE_RA_MANGLE, const0_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (flag_stack_usage_info)
    current_function_static_stack_size = constant_lower_bound (frame_size);

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (maybe_gt (frame_size, PROBE_INTERVAL)
	      && maybe_gt (frame_size, get_stack_check_protect ()))
	    aarch64_emit_probe_stack_range (get_stack_check_protect (),
					    (frame_size
					     - get_stack_check_protect ()));
	}
      else if (maybe_gt (frame_size, 0))
	aarch64_emit_probe_stack_range (get_stack_check_protect (), frame_size);
    }

  rtx tmp0_rtx = gen_rtx_REG (Pmode, EP0_REGNUM);
  rtx tmp1_rtx = gen_rtx_REG (Pmode, EP1_REGNUM);

  /* In theory we should never have both an initial adjustment
     and a callee save adjustment.  Verify that is the case since the
     code below does not handle it for -fstack-clash-protection.  */
  gcc_assert (known_eq (initial_adjust, 0) || callee_adjust == 0);

  /* Will only probe if the initial adjustment is larger than the guard
     less the amount of the guard reserved for use by the caller's
     outgoing args.  */
  aarch64_allocate_and_probe_stack_space (tmp0_rtx, tmp1_rtx, initial_adjust,
					  true, false);

  if (callee_adjust != 0)
    aarch64_push_regs (reg1, reg2, callee_adjust);

  /* The offset of the frame chain record (if any) from the current SP.  */
  poly_int64 chain_offset = (initial_adjust + callee_adjust
			     - cfun->machine->frame.hard_fp_offset);
  gcc_assert (known_ge (chain_offset, 0));

  /* The offset of the bottom of the save area from the current SP.  */
  poly_int64 saved_regs_offset = chain_offset - below_hard_fp_saved_regs_size;

  if (emit_frame_chain)
    {
      if (callee_adjust == 0)
	{
	  reg1 = R29_REGNUM;
	  reg2 = R30_REGNUM;
	  aarch64_save_callee_saves (saved_regs_offset, reg1, reg2,
				     false, false);
	}
      else
	gcc_assert (known_eq (chain_offset, 0));
      aarch64_add_offset (Pmode, hard_frame_pointer_rtx,
			  stack_pointer_rtx, chain_offset,
			  tmp1_rtx, tmp0_rtx, frame_pointer_needed);
      if (frame_pointer_needed && !frame_size.is_constant ())
	{
	  /* Variable-sized frames need to describe the save slot
	     address using DW_CFA_expression rather than DW_CFA_offset.
	     This means that, without taking further action, the
	     locations of the registers that we've already saved would
	     remain based on the stack pointer even after we redefine
	     the CFA based on the frame pointer.  We therefore need new
	     DW_CFA_expressions to re-express the save slots with addresses
	     based on the frame pointer.  */
	  rtx_insn *insn = get_last_insn ();
	  gcc_assert (RTX_FRAME_RELATED_P (insn));

	  /* Add an explicit CFA definition if this was previously
	     implicit.  */
	  if (!find_reg_note (insn, REG_CFA_ADJUST_CFA, NULL_RTX))
	    {
	      rtx src = plus_constant (Pmode, stack_pointer_rtx,
				       callee_offset);
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (hard_frame_pointer_rtx, src));
	    }

	  /* Change the save slot expressions for the registers that
	     we've already saved.  */
	  aarch64_add_cfa_expression (insn, regno_reg_rtx[reg2],
				      hard_frame_pointer_rtx, UNITS_PER_WORD);
	  aarch64_add_cfa_expression (insn, regno_reg_rtx[reg1],
				      hard_frame_pointer_rtx, 0);
	}
      emit_insn (gen_stack_tie (stack_pointer_rtx, hard_frame_pointer_rtx));
    }

  aarch64_save_callee_saves (saved_regs_offset, R0_REGNUM, R30_REGNUM,
			     callee_adjust != 0 || emit_frame_chain,
			     emit_frame_chain);
  if (maybe_ne (sve_callee_adjust, 0))
    {
      gcc_assert (!flag_stack_clash_protection
		  || known_eq (initial_adjust, 0));
      aarch64_allocate_and_probe_stack_space (tmp1_rtx, tmp0_rtx,
					      sve_callee_adjust,
					      !frame_pointer_needed, false);
      saved_regs_offset += sve_callee_adjust;
    }
  aarch64_save_callee_saves (saved_regs_offset, P0_REGNUM, P15_REGNUM,
			     false, emit_frame_chain);
  aarch64_save_callee_saves (saved_regs_offset, V0_REGNUM, V31_REGNUM,
			     callee_adjust != 0 || emit_frame_chain,
			     emit_frame_chain);

  /* We may need to probe the final adjustment if it is larger than the guard
     that is assumed by the called.  */
  aarch64_allocate_and_probe_stack_space (tmp1_rtx, tmp0_rtx, final_adjust,
					  !frame_pointer_needed, true);
}

/* Return TRUE if we can use a simple_return insn.

   This function checks whether the callee saved stack is empty, which
   means no restore actions are need. The pro_and_epilogue will use
   this to check whether shrink-wrapping opt is feasible.  */

bool
aarch64_use_return_insn_p (void)
{
  if (!reload_completed)
    return false;

  if (crtl->profile)
    return false;

  return known_eq (cfun->machine->frame.frame_size, 0);
}

/* Generate the epilogue instructions for returning from a function.
   This is almost exactly the reverse of the prolog sequence, except
   that we need to insert barriers to avoid scheduling loads that read
   from a deallocated stack, and we optimize the unwind records by
   emitting them all together if possible.  */
void
aarch64_expand_epilogue (bool for_sibcall)
{
  poly_int64 initial_adjust = cfun->machine->frame.initial_adjust;
  HOST_WIDE_INT callee_adjust = cfun->machine->frame.callee_adjust;
  poly_int64 final_adjust = cfun->machine->frame.final_adjust;
  poly_int64 callee_offset = cfun->machine->frame.callee_offset;
  poly_int64 sve_callee_adjust = cfun->machine->frame.sve_callee_adjust;
  poly_int64 below_hard_fp_saved_regs_size
    = cfun->machine->frame.below_hard_fp_saved_regs_size;
  unsigned reg1 = cfun->machine->frame.wb_candidate1;
  unsigned reg2 = cfun->machine->frame.wb_candidate2;
  rtx cfi_ops = NULL;
  rtx_insn *insn;
  /* A stack clash protection prologue may not have left EP0_REGNUM or
     EP1_REGNUM in a usable state.  The same is true for allocations
     with an SVE component, since we then need both temporary registers
     for each allocation.  For stack clash we are in a usable state if
     the adjustment is less than GUARD_SIZE - GUARD_USED_BY_CALLER.  */
  HOST_WIDE_INT guard_size
    = 1 << param_stack_clash_protection_guard_size;
  HOST_WIDE_INT guard_used_by_caller = STACK_CLASH_CALLER_GUARD;

  /* We can re-use the registers when:

     (a) the deallocation amount is the same as the corresponding
	 allocation amount (which is false if we combine the initial
	 and SVE callee save allocations in the prologue); and

     (b) the allocation amount doesn't need a probe (which is false
	 if the amount is guard_size - guard_used_by_caller or greater).

     In such situations the register should remain live with the correct
     value.  */
  bool can_inherit_p = (initial_adjust.is_constant ()
			&& final_adjust.is_constant ()
			&& (!flag_stack_clash_protection
			    || (known_lt (initial_adjust,
					  guard_size - guard_used_by_caller)
				&& known_eq (sve_callee_adjust, 0))));

  /* We need to add memory barrier to prevent read from deallocated stack.  */
  bool need_barrier_p
    = maybe_ne (get_frame_size ()
		+ cfun->machine->frame.saved_varargs_size, 0);

  /* Emit a barrier to prevent loads from a deallocated stack.  */
  if (maybe_gt (final_adjust, crtl->outgoing_args_size)
      || cfun->calls_alloca
      || crtl->calls_eh_return)
    {
      emit_insn (gen_stack_tie (stack_pointer_rtx, stack_pointer_rtx));
      need_barrier_p = false;
    }

  /* Restore the stack pointer from the frame pointer if it may not
     be the same as the stack pointer.  */
  rtx tmp0_rtx = gen_rtx_REG (Pmode, EP0_REGNUM);
  rtx tmp1_rtx = gen_rtx_REG (Pmode, EP1_REGNUM);
  if (frame_pointer_needed
      && (maybe_ne (final_adjust, 0) || cfun->calls_alloca))
    /* If writeback is used when restoring callee-saves, the CFA
       is restored on the instruction doing the writeback.  */
    aarch64_add_offset (Pmode, stack_pointer_rtx,
			hard_frame_pointer_rtx,
			-callee_offset - below_hard_fp_saved_regs_size,
			tmp1_rtx, tmp0_rtx, callee_adjust == 0);
  else
     /* The case where we need to re-use the register here is very rare, so
	avoid the complicated condition and just always emit a move if the
	immediate doesn't fit.  */
     aarch64_add_sp (tmp1_rtx, tmp0_rtx, final_adjust, true);

  /* Restore the vector registers before the predicate registers,
     so that we can use P4 as a temporary for big-endian SVE frames.  */
  aarch64_restore_callee_saves (callee_offset, V0_REGNUM, V31_REGNUM,
				callee_adjust != 0, &cfi_ops);
  aarch64_restore_callee_saves (callee_offset, P0_REGNUM, P15_REGNUM,
				false, &cfi_ops);
  if (maybe_ne (sve_callee_adjust, 0))
    aarch64_add_sp (NULL_RTX, NULL_RTX, sve_callee_adjust, true);
  aarch64_restore_callee_saves (callee_offset - sve_callee_adjust,
				R0_REGNUM, R30_REGNUM,
				callee_adjust != 0, &cfi_ops);

  if (need_barrier_p)
    emit_insn (gen_stack_tie (stack_pointer_rtx, stack_pointer_rtx));

  if (callee_adjust != 0)
    aarch64_pop_regs (reg1, reg2, callee_adjust, &cfi_ops);

  if (callee_adjust != 0 || maybe_gt (initial_adjust, 65536))
    {
      /* Emit delayed restores and set the CFA to be SP + initial_adjust.  */
      insn = get_last_insn ();
      rtx new_cfa = plus_constant (Pmode, stack_pointer_rtx, initial_adjust);
      REG_NOTES (insn) = alloc_reg_note (REG_CFA_DEF_CFA, new_cfa, cfi_ops);
      RTX_FRAME_RELATED_P (insn) = 1;
      cfi_ops = NULL;
    }

  /* Liveness of EP0_REGNUM can not be trusted across function calls either, so
     add restriction on emit_move optimization to leaf functions.  */
  aarch64_add_sp (tmp0_rtx, tmp1_rtx, initial_adjust,
		  (!can_inherit_p || !crtl->is_leaf
		   || df_regs_ever_live_p (EP0_REGNUM)));

  if (cfi_ops)
    {
      /* Emit delayed restores and reset the CFA to be SP.  */
      insn = get_last_insn ();
      cfi_ops = alloc_reg_note (REG_CFA_DEF_CFA, stack_pointer_rtx, cfi_ops);
      REG_NOTES (insn) = cfi_ops;
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* We prefer to emit the combined return/authenticate instruction RETAA,
     however there are three cases in which we must instead emit an explicit
     authentication instruction.

	1) Sibcalls don't return in a normal way, so if we're about to call one
	   we must authenticate.

	2) The RETAA instruction is not available before ARMv8.3-A, so if we are
	   generating code for !TARGET_ARMV8_3 we can't use it and must
	   explicitly authenticate.

	3) On an eh_return path we make extra stack adjustments to update the
	   canonical frame address to be the exception handler's CFA.  We want
	   to authenticate using the CFA of the function which calls eh_return.
    */
  if (aarch64_return_address_signing_enabled ()
      && (for_sibcall || !TARGET_ARMV8_3 || crtl->calls_eh_return))
    {
      switch (aarch64_ra_sign_key)
	{
	  case AARCH64_KEY_A:
	    insn = emit_insn (gen_autiasp ());
	    break;
	  case AARCH64_KEY_B:
	    insn = emit_insn (gen_autibsp ());
	    break;
	  default:
	    gcc_unreachable ();
	}
      add_reg_note (insn, REG_CFA_TOGGLE_RA_MANGLE, const0_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Stack adjustment for exception handler.  */
  if (crtl->calls_eh_return && !for_sibcall)
    {
      /* We need to unwind the stack by the offset computed by
	 EH_RETURN_STACKADJ_RTX.  We have already reset the CFA
	 to be SP; letting the CFA move during this adjustment
	 is just as correct as retaining the CFA from the body
	 of the function.  Therefore, do nothing special.  */
      emit_insn (gen_add2_insn (stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));
    }

  emit_use (gen_rtx_REG (DImode, LR_REGNUM));
  if (!for_sibcall)
    emit_jump_insn (ret_rtx);
}

/* Implement EH_RETURN_HANDLER_RTX.  EH returns need to either return
   normally or return to a previous frame after unwinding.

   An EH return uses a single shared return sequence.  The epilogue is
   exactly like a normal epilogue except that it has an extra input
   register (EH_RETURN_STACKADJ_RTX) which contains the stack adjustment
   that must be applied after the frame has been destroyed.  An extra label
   is inserted before the epilogue which initializes this register to zero,
   and this is the entry point for a normal return.

   An actual EH return updates the return address, initializes the stack
   adjustment and jumps directly into the epilogue (bypassing the zeroing
   of the adjustment).  Since the return address is typically saved on the
   stack when a function makes a call, the saved LR must be updated outside
   the epilogue.

   This poses problems as the store is generated well before the epilogue,
   so the offset of LR is not known yet.  Also optimizations will remove the
   store as it appears dead, even after the epilogue is generated (as the
   base or offset for loading LR is different in many cases).

   To avoid these problems this implementation forces the frame pointer
   in eh_return functions so that the location of LR is fixed and known early.
   It also marks the store volatile, so no optimization is permitted to
   remove the store.  */
rtx
aarch64_eh_return_handler_rtx (void)
{
  rtx tmp = gen_frame_mem (Pmode,
    plus_constant (Pmode, hard_frame_pointer_rtx, UNITS_PER_WORD));

  /* Mark the store volatile, so no optimization is permitted to remove it.  */
  MEM_VOLATILE_P (tmp) = true;
  return tmp;
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */
static void
aarch64_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT delta,
			 HOST_WIDE_INT vcall_offset,
			 tree function)
{
  /* The this pointer is always in x0.  Note that this differs from
     Arm where the this pointer maybe bumped to r1 if r0 is required
     to return a pointer to an aggregate.  On AArch64 a result value
     pointer will be in x8.  */
  int this_regno = R0_REGNUM;
  rtx this_rtx, temp0, temp1, addr, funexp;
  rtx_insn *insn;
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk));

  if (aarch64_bti_enabled ())
    emit_insn (gen_bti_c());

  reload_completed = 1;
  emit_note (NOTE_INSN_PROLOGUE_END);

  this_rtx = gen_rtx_REG (Pmode, this_regno);
  temp0 = gen_rtx_REG (Pmode, EP0_REGNUM);
  temp1 = gen_rtx_REG (Pmode, EP1_REGNUM);

  if (vcall_offset == 0)
    aarch64_add_offset (Pmode, this_rtx, this_rtx, delta, temp1, temp0, false);
  else
    {
      gcc_assert ((vcall_offset & (POINTER_BYTES - 1)) == 0);

      addr = this_rtx;
      if (delta != 0)
	{
	  if (delta >= -256 && delta < 256)
	    addr = gen_rtx_PRE_MODIFY (Pmode, this_rtx,
				       plus_constant (Pmode, this_rtx, delta));
	  else
	    aarch64_add_offset (Pmode, this_rtx, this_rtx, delta,
				temp1, temp0, false);
	}

      if (Pmode == ptr_mode)
	aarch64_emit_move (temp0, gen_rtx_MEM (ptr_mode, addr));
      else
	aarch64_emit_move (temp0,
			   gen_rtx_ZERO_EXTEND (Pmode,
						gen_rtx_MEM (ptr_mode, addr)));

      if (vcall_offset >= -256 && vcall_offset < 4096 * POINTER_BYTES)
	  addr = plus_constant (Pmode, temp0, vcall_offset);
      else
	{
	  aarch64_internal_mov_immediate (temp1, GEN_INT (vcall_offset), true,
					  Pmode);
	  addr = gen_rtx_PLUS (Pmode, temp0, temp1);
	}

      if (Pmode == ptr_mode)
	aarch64_emit_move (temp1, gen_rtx_MEM (ptr_mode,addr));
      else
	aarch64_emit_move (temp1,
			   gen_rtx_SIGN_EXTEND (Pmode,
						gen_rtx_MEM (ptr_mode, addr)));

      emit_insn (gen_add2_insn (this_rtx, temp1));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  rtx callee_abi = gen_int_mode (fndecl_abi (function).id (), DImode);
  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx, callee_abi));
  SIBLING_CALL_P (insn) = 1;

  insn = get_insns ();
  shorten_branches (insn);

  assemble_start_function (thunk, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk, fnname);

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}

static bool
aarch64_tls_referenced_p (rtx x)
{
  if (!TARGET_HAVE_TLS)
    return false;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x) != 0)
	return true;
      /* Don't recurse into UNSPEC_TLS looking for TLS symbols; these are
	 TLS offsets, not real symbol references.  */
      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
	iter.skip_subrtxes ();
    }
  return false;
}


/* Return true if val can be encoded as a 12-bit unsigned immediate with
   a left shift of 0 or 12 bits.  */
bool
aarch64_uimm12_shift (HOST_WIDE_INT val)
{
  return ((val & (((HOST_WIDE_INT) 0xfff) << 0)) == val
	  || (val & (((HOST_WIDE_INT) 0xfff) << 12)) == val
	  );
}

/* Returns the nearest value to VAL that will fit as a 12-bit unsigned immediate
   that can be created with a left shift of 0 or 12.  */
static HOST_WIDE_INT
aarch64_clamp_to_uimm12_shift (HOST_WIDE_INT val)
{
  /* Check to see if the value fits in 24 bits, as that is the maximum we can
     handle correctly.  */
  gcc_assert ((val & 0xffffff) == val);

  if (((val & 0xfff) << 0) == val)
    return val;

  return val & (0xfff << 12);
}

/* Return true if val is an immediate that can be loaded into a
   register by a MOVZ instruction.  */
static bool
aarch64_movw_imm (HOST_WIDE_INT val, scalar_int_mode mode)
{
  if (GET_MODE_SIZE (mode) > 4)
    {
      if ((val & (((HOST_WIDE_INT) 0xffff) << 32)) == val
	  || (val & (((HOST_WIDE_INT) 0xffff) << 48)) == val)
	return 1;
    }
  else
    {
      /* Ignore sign extension.  */
      val &= (HOST_WIDE_INT) 0xffffffff;
    }
  return ((val & (((HOST_WIDE_INT) 0xffff) << 0)) == val
	  || (val & (((HOST_WIDE_INT) 0xffff) << 16)) == val);
}

/* VAL is a value with the inner mode of MODE.  Replicate it to fill a
   64-bit (DImode) integer.  */

static unsigned HOST_WIDE_INT
aarch64_replicate_bitmask_imm (unsigned HOST_WIDE_INT val, machine_mode mode)
{
  unsigned int size = GET_MODE_UNIT_PRECISION (mode);
  while (size < 64)
    {
      val &= (HOST_WIDE_INT_1U << size) - 1;
      val |= val << size;
      size *= 2;
    }
  return val;
}

/* Multipliers for repeating bitmasks of width 32, 16, 8, 4, and 2.  */

static const unsigned HOST_WIDE_INT bitmask_imm_mul[] =
  {
    0x0000000100000001ull,
    0x0001000100010001ull,
    0x0101010101010101ull,
    0x1111111111111111ull,
    0x5555555555555555ull,
  };


/* Return true if val is a valid bitmask immediate.  */

bool
aarch64_bitmask_imm (HOST_WIDE_INT val_in, machine_mode mode)
{
  unsigned HOST_WIDE_INT val, tmp, mask, first_one, next_one;
  int bits;

  /* Check for a single sequence of one bits and return quickly if so.
     The special cases of all ones and all zeroes returns false.  */
  val = aarch64_replicate_bitmask_imm (val_in, mode);
  tmp = val + (val & -val);

  if (tmp == (tmp & -tmp))
    return (val + 1) > 1;

  /* Replicate 32-bit immediates so we can treat them as 64-bit.  */
  if (mode == SImode)
    val = (val << 32) | (val & 0xffffffff);

  /* Invert if the immediate doesn't start with a zero bit - this means we
     only need to search for sequences of one bits.  */
  if (val & 1)
    val = ~val;

  /* Find the first set bit and set tmp to val with the first sequence of one
     bits removed.  Return success if there is a single sequence of ones.  */
  first_one = val & -val;
  tmp = val & (val + first_one);

  if (tmp == 0)
    return true;

  /* Find the next set bit and compute the difference in bit position.  */
  next_one = tmp & -tmp;
  bits = clz_hwi (first_one) - clz_hwi (next_one);
  mask = val ^ tmp;

  /* Check the bit position difference is a power of 2, and that the first
     sequence of one bits fits within 'bits' bits.  */
  if ((mask >> bits) != 0 || bits != (bits & -bits))
    return false;

  /* Check the sequence of one bits is repeated 64/bits times.  */
  return val == mask * bitmask_imm_mul[__builtin_clz (bits) - 26];
}

/* Create mask of ones, covering the lowest to highest bits set in VAL_IN.  
   Assumed precondition: VAL_IN Is not zero.  */

unsigned HOST_WIDE_INT
aarch64_and_split_imm1 (HOST_WIDE_INT val_in)
{
  int lowest_bit_set = ctz_hwi (val_in);
  int highest_bit_set = floor_log2 (val_in);
  gcc_assert (val_in != 0);

  return ((HOST_WIDE_INT_UC (2) << highest_bit_set) -
	  (HOST_WIDE_INT_1U << lowest_bit_set));
}

/* Create constant where bits outside of lowest bit set to highest bit set
   are set to 1.  */

unsigned HOST_WIDE_INT
aarch64_and_split_imm2 (HOST_WIDE_INT val_in)
{
  return val_in | ~aarch64_and_split_imm1 (val_in);
}

/* Return true if VAL_IN is a valid 'and' bitmask immediate.  */

bool
aarch64_and_bitmask_imm (unsigned HOST_WIDE_INT val_in, machine_mode mode)
{
  scalar_int_mode int_mode;
  if (!is_a <scalar_int_mode> (mode, &int_mode))
    return false;

  if (aarch64_bitmask_imm (val_in, int_mode))
    return false;

  if (aarch64_move_imm (val_in, int_mode))
    return false;

  unsigned HOST_WIDE_INT imm2 = aarch64_and_split_imm2 (val_in);

  return aarch64_bitmask_imm (imm2, int_mode);
}

/* Return true if val is an immediate that can be loaded into a
   register in a single instruction.  */
bool
aarch64_move_imm (HOST_WIDE_INT val, machine_mode mode)
{
  scalar_int_mode int_mode;
  if (!is_a <scalar_int_mode> (mode, &int_mode))
    return false;

  if (aarch64_movw_imm (val, int_mode) || aarch64_movw_imm (~val, int_mode))
    return 1;
  return aarch64_bitmask_imm (val, int_mode);
}

static bool
aarch64_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  rtx base, offset;

  if (GET_CODE (x) == HIGH)
    return true;

  /* There's no way to calculate VL-based values using relocations.  */
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (GET_CODE (*iter) == CONST_POLY_INT)
      return true;

  split_const (x, &base, &offset);
  if (GET_CODE (base) == SYMBOL_REF || GET_CODE (base) == LABEL_REF)
    {
      if (aarch64_classify_symbol (base, INTVAL (offset))
	  != SYMBOL_FORCE_TO_MEM)
	return true;
      else
	/* Avoid generating a 64-bit relocation in ILP32; leave
	   to aarch64_expand_mov_immediate to handle it properly.  */
	return mode != ptr_mode;
    }

  return aarch64_tls_referenced_p (x);
}

/* Implement TARGET_CASE_VALUES_THRESHOLD.
   The expansion for a table switch is quite expensive due to the number
   of instructions, the table lookup and hard to predict indirect jump.
   When optimizing for speed, and -O3 enabled, use the per-core tuning if 
   set, otherwise use tables for > 16 cases as a tradeoff between size and
   performance.  When optimizing for size, use the default setting.  */

static unsigned int
aarch64_case_values_threshold (void)
{
  /* Use the specified limit for the number of cases before using jump
     tables at higher optimization levels.  */
  if (optimize > 2
      && selected_cpu->tune->max_case_values != 0)
    return selected_cpu->tune->max_case_values;
  else
    return optimize_size ? default_case_values_threshold () : 17;
}

/* Return true if register REGNO is a valid index register.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
aarch64_regno_ok_for_index_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }
  return GP_REGNUM_P (regno);
}

/* Return true if register REGNO is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
aarch64_regno_ok_for_base_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  /* The fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  return (GP_REGNUM_P (regno)
	  || regno == SP_REGNUM
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if X is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
aarch64_base_register_rtx_p (rtx x, bool strict_p)
{
  if (!strict_p
      && GET_CODE (x) == SUBREG
      && contains_reg_of_mode[GENERAL_REGS][GET_MODE (SUBREG_REG (x))])
    x = SUBREG_REG (x);

  return (REG_P (x) && aarch64_regno_ok_for_base_p (REGNO (x), strict_p));
}

/* Return true if address offset is a valid index.  If it is, fill in INFO
   appropriately.  STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
aarch64_classify_index (struct aarch64_address_info *info, rtx x,
			machine_mode mode, bool strict_p)
{
  enum aarch64_address_type type;
  rtx index;
  int shift;

  /* (reg:P) */
  if ((REG_P (x) || GET_CODE (x) == SUBREG)
      && GET_MODE (x) == Pmode)
    {
      type = ADDRESS_REG_REG;
      index = x;
      shift = 0;
    }
  /* (sign_extend:DI (reg:SI)) */
  else if ((GET_CODE (x) == SIGN_EXTEND
	    || GET_CODE (x) == ZERO_EXTEND)
	   && GET_MODE (x) == DImode
	   && GET_MODE (XEXP (x, 0)) == SImode)
    {
      type = (GET_CODE (x) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (x, 0);
      shift = 0;
    }
  /* (mult:DI (sign_extend:DI (reg:SI)) (const_int scale)) */
  else if (GET_CODE (x) == MULT
	   && (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	       || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	   && GET_MODE (XEXP (x, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (x, 1)));
    }
  /* (ashift:DI (sign_extend:DI (reg:SI)) (const_int shift)) */
  else if (GET_CODE (x) == ASHIFT
	   && (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	       || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	   && GET_MODE (XEXP (x, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (x, 1));
    }
  /* (sign_extract:DI (mult:DI (reg:DI) (const_int scale)) 32+shift 0) */
  else if ((GET_CODE (x) == SIGN_EXTRACT
	    || GET_CODE (x) == ZERO_EXTRACT)
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == MULT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    {
      type = (GET_CODE (x) == SIGN_EXTRACT)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)));
      if (INTVAL (XEXP (x, 1)) != 32 + shift
	  || INTVAL (XEXP (x, 2)) != 0)
	shift = -1;
    }
  /* (and:DI (mult:DI (reg:DI) (const_int scale))
     (const_int 0xffffffff<<shift)) */
  else if (GET_CODE (x) == AND
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == MULT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)));
      if (INTVAL (XEXP (x, 1)) != (HOST_WIDE_INT)0xffffffff << shift)
	shift = -1;
    }
  /* (sign_extract:DI (ashift:DI (reg:DI) (const_int shift)) 32+shift 0) */
  else if ((GET_CODE (x) == SIGN_EXTRACT
	    || GET_CODE (x) == ZERO_EXTRACT)
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == ASHIFT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    {
      type = (GET_CODE (x) == SIGN_EXTRACT)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (XEXP (x, 0), 1));
      if (INTVAL (XEXP (x, 1)) != 32 + shift
	  || INTVAL (XEXP (x, 2)) != 0)
	shift = -1;
    }
  /* (and:DI (ashift:DI (reg:DI) (const_int shift))
     (const_int 0xffffffff<<shift)) */
  else if (GET_CODE (x) == AND
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == ASHIFT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (XEXP (x, 0), 1));
      if (INTVAL (XEXP (x, 1)) != (HOST_WIDE_INT)0xffffffff << shift)
	shift = -1;
    }
  /* (mult:P (reg:P) (const_int scale)) */
  else if (GET_CODE (x) == MULT
	   && GET_MODE (x) == Pmode
	   && GET_MODE (XEXP (x, 0)) == Pmode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_REG;
      index = XEXP (x, 0);
      shift = exact_log2 (INTVAL (XEXP (x, 1)));
    }
  /* (ashift:P (reg:P) (const_int shift)) */
  else if (GET_CODE (x) == ASHIFT
	   && GET_MODE (x) == Pmode
	   && GET_MODE (XEXP (x, 0)) == Pmode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_REG;
      index = XEXP (x, 0);
      shift = INTVAL (XEXP (x, 1));
    }
  else
    return false;

  if (!strict_p
      && GET_CODE (index) == SUBREG
      && contains_reg_of_mode[GENERAL_REGS][GET_MODE (SUBREG_REG (index))])
    index = SUBREG_REG (index);

  if (aarch64_sve_data_mode_p (mode))
    {
      if (type != ADDRESS_REG_REG
	  || (1 << shift) != GET_MODE_UNIT_SIZE (mode))
	return false;
    }
  else
    {
      if (shift != 0
	  && !(IN_RANGE (shift, 1, 3)
	       && known_eq (1 << shift, GET_MODE_SIZE (mode))))
	return false;
    }

  if (REG_P (index)
      && aarch64_regno_ok_for_index_p (REGNO (index), strict_p))
    {
      info->type = type;
      info->offset = index;
      info->shift = shift;
      return true;
    }

  return false;
}

/* Return true if MODE is one of the modes for which we
   support LDP/STP operations.  */

static bool
aarch64_mode_valid_for_sched_fusion_p (machine_mode mode)
{
  return mode == SImode || mode == DImode
	 || mode == SFmode || mode == DFmode
	 || (aarch64_vector_mode_supported_p (mode)
	     && (known_eq (GET_MODE_SIZE (mode), 8)
		 || (known_eq (GET_MODE_SIZE (mode), 16)
		    && (aarch64_tune_params.extra_tuning_flags
			& AARCH64_EXTRA_TUNE_NO_LDP_STP_QREGS) == 0)));
}

/* Return true if REGNO is a virtual pointer register, or an eliminable
   "soft" frame register.  Like REGNO_PTR_FRAME_P except that we don't
   include stack_pointer or hard_frame_pointer.  */
static bool
virt_or_elim_regno_p (unsigned regno)
{
  return ((regno >= FIRST_VIRTUAL_REGISTER
	   && regno <= LAST_VIRTUAL_POINTER_REGISTER)
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if X is a valid address of type TYPE for machine mode MODE.
   If it is, fill in INFO appropriately.  STRICT_P is true if
   REG_OK_STRICT is in effect.  */

bool
aarch64_classify_address (struct aarch64_address_info *info,
			  rtx x, machine_mode mode, bool strict_p,
			  aarch64_addr_query_type type)
{
  enum rtx_code code = GET_CODE (x);
  rtx op0, op1;
  poly_int64 offset;

  HOST_WIDE_INT const_size;

  /* Whether a vector mode is partial doesn't affect address legitimacy.
     Partial vectors like VNx8QImode allow the same indexed addressing
     mode and MUL VL addressing mode as full vectors like VNx16QImode;
     in both cases, MUL VL counts multiples of GET_MODE_SIZE.  */
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  vec_flags &= ~VEC_PARTIAL;

  /* On BE, we use load/store pair for all large int mode load/stores.
     TI/TFmode may also use a load/store pair.  */
  bool advsimd_struct_p = (vec_flags == (VEC_ADVSIMD | VEC_STRUCT));
  bool load_store_pair_p = (type == ADDR_QUERY_LDP_STP
			    || type == ADDR_QUERY_LDP_STP_N
			    || mode == TImode
			    || mode == TFmode
			    || (BYTES_BIG_ENDIAN && advsimd_struct_p));

  /* If we are dealing with ADDR_QUERY_LDP_STP_N that means the incoming mode
     corresponds to the actual size of the memory being loaded/stored and the
     mode of the corresponding addressing mode is half of that.  */
  if (type == ADDR_QUERY_LDP_STP_N
      && known_eq (GET_MODE_SIZE (mode), 16))
    mode = DFmode;

  bool allow_reg_index_p = (!load_store_pair_p
			    && (known_lt (GET_MODE_SIZE (mode), 16)
				|| vec_flags == VEC_ADVSIMD
				|| vec_flags & VEC_SVE_DATA));

  /* For SVE, only accept [Rn], [Rn, Rm, LSL #shift] and
     [Rn, #offset, MUL VL].  */
  if ((vec_flags & (VEC_SVE_DATA | VEC_SVE_PRED)) != 0
      && (code != REG && code != PLUS))
    return false;

  /* On LE, for AdvSIMD, don't support anything other than POST_INC or
     REG addressing.  */
  if (advsimd_struct_p
      && !BYTES_BIG_ENDIAN
      && (code != POST_INC && code != REG))
    return false;

  gcc_checking_assert (GET_MODE (x) == VOIDmode
		       || SCALAR_INT_MODE_P (GET_MODE (x)));

  switch (code)
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG_IMM;
      info->base = x;
      info->offset = const0_rtx;
      info->const_offset = 0;
      return aarch64_base_register_rtx_p (x, strict_p);

    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (! strict_p
	  && REG_P (op0)
	  && virt_or_elim_regno_p (REGNO (op0))
	  && poly_int_rtx_p (op1, &offset))
	{
	  info->type = ADDRESS_REG_IMM;
	  info->base = op0;
	  info->offset = op1;
	  info->const_offset = offset;

	  return true;
	}

      if (maybe_ne (GET_MODE_SIZE (mode), 0)
	  && aarch64_base_register_rtx_p (op0, strict_p)
	  && poly_int_rtx_p (op1, &offset))
	{
	  info->type = ADDRESS_REG_IMM;
	  info->base = op0;
	  info->offset = op1;
	  info->const_offset = offset;

	  /* TImode and TFmode values are allowed in both pairs of X
	     registers and individual Q registers.  The available
	     address modes are:
	     X,X: 7-bit signed scaled offset
	     Q:   9-bit signed offset
	     We conservatively require an offset representable in either mode.
	     When performing the check for pairs of X registers i.e.  LDP/STP
	     pass down DImode since that is the natural size of the LDP/STP
	     instruction memory accesses.  */
	  if (mode == TImode || mode == TFmode)
	    return (aarch64_offset_7bit_signed_scaled_p (DImode, offset)
		    && (aarch64_offset_9bit_signed_unscaled_p (mode, offset)
			|| offset_12bit_unsigned_scaled_p (mode, offset)));

	  /* A 7bit offset check because OImode will emit a ldp/stp
	     instruction (only big endian will get here).
	     For ldp/stp instructions, the offset is scaled for the size of a
	     single element of the pair.  */
	  if (mode == OImode)
	    return aarch64_offset_7bit_signed_scaled_p (TImode, offset);

	  /* Three 9/12 bit offsets checks because CImode will emit three
	     ldr/str instructions (only big endian will get here).  */
	  if (mode == CImode)
	    return (aarch64_offset_7bit_signed_scaled_p (TImode, offset)
		    && (aarch64_offset_9bit_signed_unscaled_p (V16QImode,
							       offset + 32)
			|| offset_12bit_unsigned_scaled_p (V16QImode,
							   offset + 32)));

	  /* Two 7bit offsets checks because XImode will emit two ldp/stp
	     instructions (only big endian will get here).  */
	  if (mode == XImode)
	    return (aarch64_offset_7bit_signed_scaled_p (TImode, offset)
		    && aarch64_offset_7bit_signed_scaled_p (TImode,
							    offset + 32));

	  /* Make "m" use the LD1 offset range for SVE data modes, so
	     that pre-RTL optimizers like ivopts will work to that
	     instead of the wider LDR/STR range.  */
	  if (vec_flags == VEC_SVE_DATA)
	    return (type == ADDR_QUERY_M
		    ? offset_4bit_signed_scaled_p (mode, offset)
		    : offset_9bit_signed_scaled_p (mode, offset));

	  if (vec_flags == (VEC_SVE_DATA | VEC_STRUCT))
	    {
	      poly_int64 end_offset = (offset
				       + GET_MODE_SIZE (mode)
				       - BYTES_PER_SVE_VECTOR);
	      return (type == ADDR_QUERY_M
		      ? offset_4bit_signed_scaled_p (mode, offset)
		      : (offset_9bit_signed_scaled_p (SVE_BYTE_MODE, offset)
			 && offset_9bit_signed_scaled_p (SVE_BYTE_MODE,
							 end_offset)));
	    }

	  if (vec_flags == VEC_SVE_PRED)
	    return offset_9bit_signed_scaled_p (mode, offset);

	  if (load_store_pair_p)
	    return ((known_eq (GET_MODE_SIZE (mode), 4)
		     || known_eq (GET_MODE_SIZE (mode), 8)
		     || known_eq (GET_MODE_SIZE (mode), 16))
		    && aarch64_offset_7bit_signed_scaled_p (mode, offset));
	  else
	    return (aarch64_offset_9bit_signed_unscaled_p (mode, offset)
		    || offset_12bit_unsigned_scaled_p (mode, offset));
	}

      if (allow_reg_index_p)
	{
	  /* Look for base + (scaled/extended) index register.  */
	  if (aarch64_base_register_rtx_p (op0, strict_p)
	      && aarch64_classify_index (info, op1, mode, strict_p))
	    {
	      info->base = op0;
	      return true;
	    }
	  if (aarch64_base_register_rtx_p (op1, strict_p)
	      && aarch64_classify_index (info, op0, mode, strict_p))
	    {
	      info->base = op1;
	      return true;
	    }
	}

      return false;

    case POST_INC:
    case POST_DEC:
    case PRE_INC:
    case PRE_DEC:
      info->type = ADDRESS_REG_WB;
      info->base = XEXP (x, 0);
      info->offset = NULL_RTX;
      return aarch64_base_register_rtx_p (info->base, strict_p);

    case POST_MODIFY:
    case PRE_MODIFY:
      info->type = ADDRESS_REG_WB;
      info->base = XEXP (x, 0);
      if (GET_CODE (XEXP (x, 1)) == PLUS
	  && poly_int_rtx_p (XEXP (XEXP (x, 1), 1), &offset)
	  && rtx_equal_p (XEXP (XEXP (x, 1), 0), info->base)
	  && aarch64_base_register_rtx_p (info->base, strict_p))
	{
	  info->offset = XEXP (XEXP (x, 1), 1);
	  info->const_offset = offset;

	  /* TImode and TFmode values are allowed in both pairs of X
	     registers and individual Q registers.  The available
	     address modes are:
	     X,X: 7-bit signed scaled offset
	     Q:   9-bit signed offset
	     We conservatively require an offset representable in either mode.
	   */
	  if (mode == TImode || mode == TFmode)
	    return (aarch64_offset_7bit_signed_scaled_p (mode, offset)
		    && aarch64_offset_9bit_signed_unscaled_p (mode, offset));

	  if (load_store_pair_p)
	    return ((known_eq (GET_MODE_SIZE (mode), 4)
		     || known_eq (GET_MODE_SIZE (mode), 8)
		     || known_eq (GET_MODE_SIZE (mode), 16))
		    && aarch64_offset_7bit_signed_scaled_p (mode, offset));
	  else
	    return aarch64_offset_9bit_signed_unscaled_p (mode, offset);
	}
      return false;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* load literal: pc-relative constant pool entry.  Only supported
         for SI mode or larger.  */
      info->type = ADDRESS_SYMBOLIC;

      if (!load_store_pair_p
	  && GET_MODE_SIZE (mode).is_constant (&const_size)
	  && const_size >= 4)
	{
	  rtx sym, addend;

	  split_const (x, &sym, &addend);
	  return ((GET_CODE (sym) == LABEL_REF
		   || (GET_CODE (sym) == SYMBOL_REF
		       && CONSTANT_POOL_ADDRESS_P (sym)
		       && aarch64_pcrelative_literal_loads)));
	}
      return false;

    case LO_SUM:
      info->type = ADDRESS_LO_SUM;
      info->base = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      if (allow_reg_index_p
	  && aarch64_base_register_rtx_p (info->base, strict_p))
	{
	  rtx sym, offs;
	  split_const (info->offset, &sym, &offs);
	  if (GET_CODE (sym) == SYMBOL_REF
	      && (aarch64_classify_symbol (sym, INTVAL (offs))
		  == SYMBOL_SMALL_ABSOLUTE))
	    {
	      /* The symbol and offset must be aligned to the access size.  */
	      unsigned int align;

	      if (CONSTANT_POOL_ADDRESS_P (sym))
		align = GET_MODE_ALIGNMENT (get_pool_mode (sym));
	      else if (TREE_CONSTANT_POOL_ADDRESS_P (sym))
		{
		  tree exp = SYMBOL_REF_DECL (sym);
		  align = TYPE_ALIGN (TREE_TYPE (exp));
		  align = aarch64_constant_alignment (exp, align);
		}
	      else if (SYMBOL_REF_DECL (sym))
		align = DECL_ALIGN (SYMBOL_REF_DECL (sym));
	      else if (SYMBOL_REF_HAS_BLOCK_INFO_P (sym)
		       && SYMBOL_REF_BLOCK (sym) != NULL)
		align = SYMBOL_REF_BLOCK (sym)->alignment;
	      else
		align = BITS_PER_UNIT;

	      poly_int64 ref_size = GET_MODE_SIZE (mode);
	      if (known_eq (ref_size, 0))
		ref_size = GET_MODE_SIZE (DImode);

	      return (multiple_p (INTVAL (offs), ref_size)
		      && multiple_p (align / BITS_PER_UNIT, ref_size));
	    }
	}
      return false;

    default:
      return false;
    }
}

/* Return true if the address X is valid for a PRFM instruction.
   STRICT_P is true if we should do strict checking with
   aarch64_classify_address.  */

bool
aarch64_address_valid_for_prefetch_p (rtx x, bool strict_p)
{
  struct aarch64_address_info addr;

  /* PRFM accepts the same addresses as DImode...  */
  bool res = aarch64_classify_address (&addr, x, DImode, strict_p);
  if (!res)
    return false;

  /* ... except writeback forms.  */
  return addr.type != ADDRESS_REG_WB;
}

bool
aarch64_symbolic_address_p (rtx x)
{
  rtx offset;

  split_const (x, &x, &offset);
  return GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF;
}

/* Classify the base of symbolic expression X.  */

enum aarch64_symbol_type
aarch64_classify_symbolic_expression (rtx x)
{
  rtx offset;

  split_const (x, &x, &offset);
  return aarch64_classify_symbol (x, INTVAL (offset));
}


/* Return TRUE if X is a legitimate address for accessing memory in
   mode MODE.  */
static bool
aarch64_legitimate_address_hook_p (machine_mode mode, rtx x, bool strict_p)
{
  struct aarch64_address_info addr;

  return aarch64_classify_address (&addr, x, mode, strict_p);
}

/* Return TRUE if X is a legitimate address of type TYPE for accessing
   memory in mode MODE.  STRICT_P is true if REG_OK_STRICT is in effect.  */
bool
aarch64_legitimate_address_p (machine_mode mode, rtx x, bool strict_p,
			      aarch64_addr_query_type type)
{
  struct aarch64_address_info addr;

  return aarch64_classify_address (&addr, x, mode, strict_p, type);
}

/* Implement TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT.  */

static bool
aarch64_legitimize_address_displacement (rtx *offset1, rtx *offset2,
					 poly_int64 orig_offset,
					 machine_mode mode)
{
  HOST_WIDE_INT size;
  if (GET_MODE_SIZE (mode).is_constant (&size))
    {
      HOST_WIDE_INT const_offset, second_offset;

      /* A general SVE offset is A * VQ + B.  Remove the A component from
	 coefficient 0 in order to get the constant B.  */
      const_offset = orig_offset.coeffs[0] - orig_offset.coeffs[1];

      /* Split an out-of-range address displacement into a base and
	 offset.  Use 4KB range for 1- and 2-byte accesses and a 16KB
	 range otherwise to increase opportunities for sharing the base
	 address of different sizes.  Unaligned accesses use the signed
	 9-bit range, TImode/TFmode use the intersection of signed
	 scaled 7-bit and signed 9-bit offset.  */
      if (mode == TImode || mode == TFmode)
	second_offset = ((const_offset + 0x100) & 0x1f8) - 0x100;
      else if ((const_offset & (size - 1)) != 0)
	second_offset = ((const_offset + 0x100) & 0x1ff) - 0x100;
      else
	second_offset = const_offset & (size < 4 ? 0xfff : 0x3ffc);

      if (second_offset == 0 || known_eq (orig_offset, second_offset))
	return false;

      /* Split the offset into second_offset and the rest.  */
      *offset1 = gen_int_mode (orig_offset - second_offset, Pmode);
      *offset2 = gen_int_mode (second_offset, Pmode);
      return true;
    }
  else
    {
      /* Get the mode we should use as the basis of the range.  For structure
	 modes this is the mode of one vector.  */
      unsigned int vec_flags = aarch64_classify_vector_mode (mode);
      machine_mode step_mode
	= (vec_flags & VEC_STRUCT) != 0 ? SVE_BYTE_MODE : mode;

      /* Get the "mul vl" multiplier we'd like to use.  */
      HOST_WIDE_INT factor = GET_MODE_SIZE (step_mode).coeffs[1];
      HOST_WIDE_INT vnum = orig_offset.coeffs[1] / factor;
      if (vec_flags & VEC_SVE_DATA)
	/* LDR supports a 9-bit range, but the move patterns for
	   structure modes require all vectors to be in range of the
	   same base.  The simplest way of accomodating that while still
	   promoting reuse of anchor points between different modes is
	   to use an 8-bit range unconditionally.  */
	vnum = ((vnum + 128) & 255) - 128;
      else
	/* Predicates are only handled singly, so we might as well use
	   the full range.  */
	vnum = ((vnum + 256) & 511) - 256;
      if (vnum == 0)
	return false;

      /* Convert the "mul vl" multiplier into a byte offset.  */
      poly_int64 second_offset = GET_MODE_SIZE (step_mode) * vnum;
      if (known_eq (second_offset, orig_offset))
	return false;

      /* Split the offset into second_offset and the rest.  */
      *offset1 = gen_int_mode (orig_offset - second_offset, Pmode);
      *offset2 = gen_int_mode (second_offset, Pmode);
      return true;
    }
}

/* Return the binary representation of floating point constant VALUE in INTVAL.
   If the value cannot be converted, return false without setting INTVAL.
   The conversion is done in the given MODE.  */
bool
aarch64_reinterpret_float_as_int (rtx value, unsigned HOST_WIDE_INT *intval)
{

  /* We make a general exception for 0.  */
  if (aarch64_float_const_zero_rtx_p (value))
    {
      *intval = 0;
      return true;
    }

  scalar_float_mode mode;
  if (GET_CODE (value) != CONST_DOUBLE
      || !is_a <scalar_float_mode> (GET_MODE (value), &mode)
      || GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT
      /* Only support up to DF mode.  */
      || GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (DFmode))
    return false;

  unsigned HOST_WIDE_INT ival = 0;

  long res[2];
  real_to_target (res,
		  CONST_DOUBLE_REAL_VALUE (value),
		  REAL_MODE_FORMAT (mode));

  if (mode == DFmode)
    {
      int order = BYTES_BIG_ENDIAN ? 1 : 0;
      ival = zext_hwi (res[order], 32);
      ival |= (zext_hwi (res[1 - order], 32) << 32);
    }
  else
      ival = zext_hwi (res[0], 32);

  *intval = ival;
  return true;
}

/* Return TRUE if rtx X is an immediate constant that can be moved using a
   single MOV(+MOVK) followed by an FMOV.  */
bool
aarch64_float_const_rtx_p (rtx x)
{
  machine_mode mode = GET_MODE (x);
  if (mode == VOIDmode)
    return false;

  /* Determine whether it's cheaper to write float constants as
     mov/movk pairs over ldr/adrp pairs.  */
  unsigned HOST_WIDE_INT ival;

  if (GET_CODE (x) == CONST_DOUBLE
      && SCALAR_FLOAT_MODE_P (mode)
      && aarch64_reinterpret_float_as_int (x, &ival))
    {
      scalar_int_mode imode = (mode == HFmode
			       ? SImode
			       : int_mode_for_mode (mode).require ());
      int num_instr = aarch64_internal_mov_immediate
			(NULL_RTX, gen_int_mode (ival, imode), false, imode);
      return num_instr < 3;
    }

  return false;
}

/* Return TRUE if rtx X is immediate constant 0.0 */
bool
aarch64_float_const_zero_rtx_p (rtx x)
{
  if (GET_MODE (x) == VOIDmode)
    return false;

  if (REAL_VALUE_MINUS_ZERO (*CONST_DOUBLE_REAL_VALUE (x)))
    return !HONOR_SIGNED_ZEROS (GET_MODE (x));
  return real_equal (CONST_DOUBLE_REAL_VALUE (x), &dconst0);
}

/* Return TRUE if rtx X is immediate constant that fits in a single
   MOVI immediate operation.  */
bool
aarch64_can_const_movi_rtx_p (rtx x, machine_mode mode)
{
  if (!TARGET_SIMD)
     return false;

  machine_mode vmode;
  scalar_int_mode imode;
  unsigned HOST_WIDE_INT ival;

  if (GET_CODE (x) == CONST_DOUBLE
      && SCALAR_FLOAT_MODE_P (mode))
    {
      if (!aarch64_reinterpret_float_as_int (x, &ival))
	return false;

      /* We make a general exception for 0.  */
      if (aarch64_float_const_zero_rtx_p (x))
	return true;

      imode = int_mode_for_mode (mode).require ();
    }
  else if (GET_CODE (x) == CONST_INT
	   && is_a <scalar_int_mode> (mode, &imode))
    ival = INTVAL (x);
  else
    return false;

   /* use a 64 bit mode for everything except for DI/DF mode, where we use
     a 128 bit vector mode.  */
  int width = GET_MODE_BITSIZE (imode) == 64 ? 128 : 64;

  vmode = aarch64_simd_container_mode (imode, width);
  rtx v_op = aarch64_simd_gen_const_vector_dup (vmode, ival);

  return aarch64_simd_valid_immediate (v_op, NULL);
}


/* Return the fixed registers used for condition codes.  */

static bool
aarch64_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REGNUM;
  *p2 = INVALID_REGNUM;
  return true;
}

/* This function is used by the call expanders of the machine description.
   RESULT is the register in which the result is returned.  It's NULL for
   "call" and "sibcall".
   MEM is the location of the function call.
   CALLEE_ABI is a const_int that gives the arm_pcs of the callee.
   SIBCALL indicates whether this function call is normal call or sibling call.
   It will generate different pattern accordingly.  */

void
aarch64_expand_call (rtx result, rtx mem, rtx callee_abi, bool sibcall)
{
  rtx call, callee, tmp;
  rtvec vec;
  machine_mode mode;

  gcc_assert (MEM_P (mem));
  callee = XEXP (mem, 0);
  mode = GET_MODE (callee);
  gcc_assert (mode == Pmode);

  /* Decide if we should generate indirect calls by loading the
     address of the callee into a register before performing
     the branch-and-link.  */
  if (SYMBOL_REF_P (callee)
      ? (aarch64_is_long_call_p (callee)
	 || aarch64_is_noplt_call_p (callee))
      : !REG_P (callee))
    XEXP (mem, 0) = force_reg (mode, callee);

  call = gen_rtx_CALL (VOIDmode, mem, const0_rtx);

  if (result != NULL_RTX)
    call = gen_rtx_SET (result, call);

  if (sibcall)
    tmp = ret_rtx;
  else
    tmp = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, LR_REGNUM));

  gcc_assert (CONST_INT_P (callee_abi));
  callee_abi = gen_rtx_UNSPEC (DImode, gen_rtvec (1, callee_abi),
			       UNSPEC_CALLEE_ABI);

  vec = gen_rtvec (3, call, callee_abi, tmp);
  call = gen_rtx_PARALLEL (VOIDmode, vec);

  aarch64_emit_call_insn (call);
}

/* Emit call insn with PAT and do aarch64-specific handling.  */

void
aarch64_emit_call_insn (rtx pat)
{
  rtx insn = emit_call_insn (pat);

  rtx *fusage = &CALL_INSN_FUNCTION_USAGE (insn);
  clobber_reg (fusage, gen_rtx_REG (word_mode, IP0_REGNUM));
  clobber_reg (fusage, gen_rtx_REG (word_mode, IP1_REGNUM));
}

machine_mode
aarch64_select_cc_mode (RTX_CODE code, rtx x, rtx y)
{
  machine_mode mode_x = GET_MODE (x);
  rtx_code code_x = GET_CODE (x);

  /* All floating point compares return CCFP if it is an equality
     comparison, and CCFPE otherwise.  */
  if (GET_MODE_CLASS (mode_x) == MODE_FLOAT)
    {
      switch (code)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	case LTGT:
	  return CCFPEmode;

	default:
	  gcc_unreachable ();
	}
    }

  /* Equality comparisons of short modes against zero can be performed
     using the TST instruction with the appropriate bitmask.  */
  if (y == const0_rtx && (REG_P (x) || SUBREG_P (x))
      && (code == EQ || code == NE)
      && (mode_x == HImode || mode_x == QImode))
    return CC_NZmode;

  /* Similarly, comparisons of zero_extends from shorter modes can
     be performed using an ANDS with an immediate mask.  */
  if (y == const0_rtx && code_x == ZERO_EXTEND
      && (mode_x == SImode || mode_x == DImode)
      && (GET_MODE (XEXP (x, 0)) == HImode || GET_MODE (XEXP (x, 0)) == QImode)
      && (code == EQ || code == NE))
    return CC_NZmode;

  if ((mode_x == SImode || mode_x == DImode)
      && y == const0_rtx
      && (code == EQ || code == NE || code == LT || code == GE)
      && (code_x == PLUS || code_x == MINUS || code_x == AND
	  || code_x == NEG
	  || (code_x == ZERO_EXTRACT && CONST_INT_P (XEXP (x, 1))
	      && CONST_INT_P (XEXP (x, 2)))))
    return CC_NZmode;

  /* A compare with a shifted operand.  Because of canonicalization,
     the comparison will have to be swapped when we emit the assembly
     code.  */
  if ((mode_x == SImode || mode_x == DImode)
      && (REG_P (y) || GET_CODE (y) == SUBREG || y == const0_rtx)
      && (code_x == ASHIFT || code_x == ASHIFTRT
	  || code_x == LSHIFTRT
	  || code_x == ZERO_EXTEND || code_x == SIGN_EXTEND))
    return CC_SWPmode;

  /* Similarly for a negated operand, but we can only do this for
     equalities.  */
  if ((mode_x == SImode || mode_x == DImode)
      && (REG_P (y) || GET_CODE (y) == SUBREG)
      && (code == EQ || code == NE)
      && code_x == NEG)
    return CC_Zmode;

  /* A test for unsigned overflow from an addition.  */
  if ((mode_x == DImode || mode_x == TImode)
      && (code == LTU || code == GEU)
      && code_x == PLUS
      && rtx_equal_p (XEXP (x, 0), y))
    return CC_Cmode;

  /* A test for unsigned overflow from an add with carry.  */
  if ((mode_x == DImode || mode_x == TImode)
      && (code == LTU || code == GEU)
      && code_x == PLUS
      && CONST_SCALAR_INT_P (y)
      && (rtx_mode_t (y, mode_x)
	  == (wi::shwi (1, mode_x)
	      << (GET_MODE_BITSIZE (mode_x).to_constant () / 2))))
    return CC_ADCmode;

  /* A test for signed overflow.  */
  if ((mode_x == DImode || mode_x == TImode)
      && code == NE
      && code_x == PLUS
      && GET_CODE (y) == SIGN_EXTEND)
    return CC_Vmode;

  /* For everything else, return CCmode.  */
  return CCmode;
}

static int
aarch64_get_condition_code_1 (machine_mode, enum rtx_code);

int
aarch64_get_condition_code (rtx x)
{
  machine_mode mode = GET_MODE (XEXP (x, 0));
  enum rtx_code comp_code = GET_CODE (x);

  if (GET_MODE_CLASS (mode) != MODE_CC)
    mode = SELECT_CC_MODE (comp_code, XEXP (x, 0), XEXP (x, 1));
  return aarch64_get_condition_code_1 (mode, comp_code);
}

static int
aarch64_get_condition_code_1 (machine_mode mode, enum rtx_code comp_code)
{
  switch (mode)
    {
    case E_CCFPmode:
    case E_CCFPEmode:
      switch (comp_code)
	{
	case GE: return AARCH64_GE;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LS;
	case LT: return AARCH64_MI;
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case ORDERED: return AARCH64_VC;
	case UNORDERED: return AARCH64_VS;
	case UNLT: return AARCH64_LT;
	case UNLE: return AARCH64_LE;
	case UNGT: return AARCH64_HI;
	case UNGE: return AARCH64_PL;
	default: return -1;
	}
      break;

    case E_CCmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_GE;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LE;
	case LT: return AARCH64_LT;
	case GEU: return AARCH64_CS;
	case GTU: return AARCH64_HI;
	case LEU: return AARCH64_LS;
	case LTU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case E_CC_SWPmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_LE;
	case GT: return AARCH64_LT;
	case LE: return AARCH64_GE;
	case LT: return AARCH64_GT;
	case GEU: return AARCH64_LS;
	case GTU: return AARCH64_CC;
	case LEU: return AARCH64_CS;
	case LTU: return AARCH64_HI;
	default: return -1;
	}
      break;

    case E_CC_NZCmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE; /* = any */
	case EQ: return AARCH64_EQ; /* = none */
	case GE: return AARCH64_PL; /* = nfrst */
	case LT: return AARCH64_MI; /* = first */
	case GEU: return AARCH64_CS; /* = nlast */
	case GTU: return AARCH64_HI; /* = pmore */
	case LEU: return AARCH64_LS; /* = plast */
	case LTU: return AARCH64_CC; /* = last */
	default: return -1;
	}
      break;

    case E_CC_NZmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_PL;
	case LT: return AARCH64_MI;
	default: return -1;
	}
      break;

    case E_CC_Zmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	default: return -1;
	}
      break;

    case E_CC_Cmode:
      switch (comp_code)
	{
	case LTU: return AARCH64_CS;
	case GEU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case E_CC_ADCmode:
      switch (comp_code)
	{
	case GEU: return AARCH64_CS;
	case LTU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case E_CC_Vmode:
      switch (comp_code)
	{
	case NE: return AARCH64_VS;
	case EQ: return AARCH64_VC;
	default: return -1;
	}
      break;

    default:
      return -1;
    }

  return -1;
}

bool
aarch64_const_vec_all_same_in_range_p (rtx x,
				       HOST_WIDE_INT minval,
				       HOST_WIDE_INT maxval)
{
  rtx elt;
  return (const_vec_duplicate_p (x, &elt)
	  && CONST_INT_P (elt)
	  && IN_RANGE (INTVAL (elt), minval, maxval));
}

bool
aarch64_const_vec_all_same_int_p (rtx x, HOST_WIDE_INT val)
{
  return aarch64_const_vec_all_same_in_range_p (x, val, val);
}

/* Return true if VEC is a constant in which every element is in the range
   [MINVAL, MAXVAL].  The elements do not need to have the same value.  */

static bool
aarch64_const_vec_all_in_range_p (rtx vec,
				  HOST_WIDE_INT minval,
				  HOST_WIDE_INT maxval)
{
  if (GET_CODE (vec) != CONST_VECTOR
      || GET_MODE_CLASS (GET_MODE (vec)) != MODE_VECTOR_INT)
    return false;

  int nunits;
  if (!CONST_VECTOR_STEPPED_P (vec))
    nunits = const_vector_encoded_nelts (vec);
  else if (!CONST_VECTOR_NUNITS (vec).is_constant (&nunits))
    return false;

  for (int i = 0; i < nunits; i++)
    {
      rtx vec_elem = CONST_VECTOR_ELT (vec, i);
      if (!CONST_INT_P (vec_elem)
	  || !IN_RANGE (INTVAL (vec_elem), minval, maxval))
	return false;
    }
  return true;
}

/* N Z C V.  */
#define AARCH64_CC_V 1
#define AARCH64_CC_C (1 << 1)
#define AARCH64_CC_Z (1 << 2)
#define AARCH64_CC_N (1 << 3)

/* N Z C V flags for ccmp.  Indexed by AARCH64_COND_CODE.  */
static const int aarch64_nzcv_codes[] =
{
  0,		/* EQ, Z == 1.  */
  AARCH64_CC_Z,	/* NE, Z == 0.  */
  0,		/* CS, C == 1.  */
  AARCH64_CC_C,	/* CC, C == 0.  */
  0,		/* MI, N == 1.  */
  AARCH64_CC_N, /* PL, N == 0.  */
  0,		/* VS, V == 1.  */
  AARCH64_CC_V, /* VC, V == 0.  */
  0,		/* HI, C ==1 && Z == 0.  */
  AARCH64_CC_C,	/* LS, !(C == 1 && Z == 0).  */
  AARCH64_CC_V,	/* GE, N == V.  */
  0,		/* LT, N != V.  */
  AARCH64_CC_Z, /* GT, Z == 0 && N == V.  */
  0,		/* LE, !(Z == 0 && N == V).  */
  0,		/* AL, Any.  */
  0		/* NV, Any.  */
};

/* Print floating-point vector immediate operand X to F, negating it
   first if NEGATE is true.  Return true on success, false if it isn't
   a constant we can handle.  */

static bool
aarch64_print_vector_float_operand (FILE *f, rtx x, bool negate)
{
  rtx elt;

  if (!const_vec_duplicate_p (x, &elt))
    return false;

  REAL_VALUE_TYPE r = *CONST_DOUBLE_REAL_VALUE (elt);
  if (negate)
    r = real_value_negate (&r);

  /* Handle the SVE single-bit immediates specially, since they have a
     fixed form in the assembly syntax.  */
  if (real_equal (&r, &dconst0))
    asm_fprintf (f, "0.0");
  else if (real_equal (&r, &dconst2))
    asm_fprintf (f, "2.0");
  else if (real_equal (&r, &dconst1))
    asm_fprintf (f, "1.0");
  else if (real_equal (&r, &dconsthalf))
    asm_fprintf (f, "0.5");
  else
    {
      const int buf_size = 20;
      char float_buf[buf_size] = {'\0'};
      real_to_decimal_for_mode (float_buf, &r, buf_size, buf_size,
				1, GET_MODE (elt));
      asm_fprintf (f, "%s", float_buf);
    }

  return true;
}

/* Return the equivalent letter for size.  */
static char
sizetochar (int size)
{
  switch (size)
    {
    case 64: return 'd';
    case 32: return 's';
    case 16: return 'h';
    case 8 : return 'b';
    default: gcc_unreachable ();
    }
}

/* Print operand X to file F in a target specific manner according to CODE.
   The acceptable formatting commands given by CODE are:
     'c':		An integer or symbol address without a preceding #
			sign.
     'C':		Take the duplicated element in a vector constant
			and print it in hex.
     'D':		Take the duplicated element in a vector constant
			and print it as an unsigned integer, in decimal.
     'e':		Print the sign/zero-extend size as a character 8->b,
			16->h, 32->w.  Can also be used for masks:
			0xff->b, 0xffff->h, 0xffffffff->w.
     'I':		If the operand is a duplicated vector constant,
			replace it with the duplicated scalar.  If the
			operand is then a floating-point constant, replace
			it with the integer bit representation.  Print the
			transformed constant as a signed decimal number.
     'p':		Prints N such that 2^N == X (X must be power of 2 and
			const int).
     'P':		Print the number of non-zero bits in X (a const_int).
     'H':		Print the higher numbered register of a pair (TImode)
			of regs.
     'm':		Print a condition (eq, ne, etc).
     'M':		Same as 'm', but invert condition.
     'N':		Take the duplicated element in a vector constant
			and print the negative of it in decimal.
     'b/h/s/d/q':	Print a scalar FP/SIMD register name.
     'S/T/U/V':		Print a FP/SIMD register name for a register list.
			The register printed is the FP/SIMD register name
			of X + 0/1/2/3 for S/T/U/V.
     'R':		Print a scalar Integer/FP/SIMD register name + 1.
     'X':		Print bottom 16 bits of integer constant in hex.
     'w/x':		Print a general register name or the zero register
			(32-bit or 64-bit).
     '0':		Print a normal operand, if it's a general register,
			then we assume DImode.
     'k':		Print NZCV for conditional compare instructions.
     'A':		Output address constant representing the first
			argument of X, specifying a relocation offset
			if appropriate.
     'L':		Output constant address specified by X
			with a relocation offset if appropriate.
     'G':		Prints address of X, specifying a PC relative
			relocation mode if appropriate.
     'y':		Output address of LDP or STP - this is used for
			some LDP/STPs which don't use a PARALLEL in their
			pattern (so the mode needs to be adjusted).
     'z':		Output address of a typical LDP or STP.  */

static void
aarch64_print_operand (FILE *f, rtx x, int code)
{
  rtx elt;
  switch (code)
    {
    case 'c':
      switch (GET_CODE (x))
	{
	case CONST_INT:
	  fprintf (f, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
	  break;

	case SYMBOL_REF:
	  output_addr_const (f, x);
	  break;

	case CONST:
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	    {
	      output_addr_const (f, x);
	      break;
	    }
	  /* Fall through.  */

	default:
	  output_operand_lossage ("unsupported operand for code '%c'", code);
	}
      break;

    case 'e':
      {
	x = unwrap_const_vec_duplicate (x);
	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	HOST_WIDE_INT val = INTVAL (x);
	if ((val & ~7) == 8 || val == 0xff)
	  fputc ('b', f);
	else if ((val & ~7) == 16 || val == 0xffff)
	  fputc ('h', f);
	else if ((val & ~7) == 32 || val == 0xffffffff)
	  fputc ('w', f);
	else
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
      }
      break;

    case 'p':
      {
	int n;

	if (!CONST_INT_P (x) || (n = exact_log2 (INTVAL (x))) < 0)
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	asm_fprintf (f, "%d", n);
      }
      break;

    case 'P':
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}

      asm_fprintf (f, "%u", popcount_hwi (INTVAL (x)));
      break;

    case 'H':
      if (x == const0_rtx)
	{
	  asm_fprintf (f, "xzr");
	  break;
	}

      if (!REG_P (x) || !GP_REGNUM_P (REGNO (x) + 1))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}

      asm_fprintf (f, "%s", reg_names [REGNO (x) + 1]);
      break;

    case 'I':
      {
	x = aarch64_bit_representation (unwrap_const_vec_duplicate (x));
	if (CONST_INT_P (x))
	  asm_fprintf (f, "%wd", INTVAL (x));
	else
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
	break;
      }

    case 'M':
    case 'm':
      {
        int cond_code;
	/* CONST_TRUE_RTX means al/nv (al is the default, don't print it).  */
	if (x == const_true_rtx)
	  {
	    if (code == 'M')
	      fputs ("nv", f);
	    return;
	  }

        if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

        cond_code = aarch64_get_condition_code (x);
        gcc_assert (cond_code >= 0);
	if (code == 'M')
	  cond_code = AARCH64_INVERSE_CONDITION_CODE (cond_code);
	if (GET_MODE (XEXP (x, 0)) == CC_NZCmode)
	  fputs (aarch64_sve_condition_codes[cond_code], f);
	else
	  fputs (aarch64_condition_codes[cond_code], f);
      }
      break;

    case 'N':
      if (!const_vec_duplicate_p (x, &elt))
	{
	  output_operand_lossage ("invalid vector constant");
	  return;
	}

      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_INT)
	asm_fprintf (f, "%wd", -INTVAL (elt));
      else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_FLOAT
	       && aarch64_print_vector_float_operand (f, x, true))
	;
      else
	{
	  output_operand_lossage ("invalid vector constant");
	  return;
	}
      break;

    case 'b':
    case 'h':
    case 's':
    case 'd':
    case 'q':
      if (!REG_P (x) || !FP_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("incompatible floating point / vector register operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "%c%d", code, REGNO (x) - V0_REGNUM);
      break;

    case 'S':
    case 'T':
    case 'U':
    case 'V':
      if (!REG_P (x) || !FP_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("incompatible floating point / vector register operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "%c%d",
		   aarch64_sve_data_mode_p (GET_MODE (x)) ? 'z' : 'v',
		   REGNO (x) - V0_REGNUM + (code - 'S'));
      break;

    case 'R':
      if (REG_P (x) && FP_REGNUM_P (REGNO (x)))
	asm_fprintf (f, "q%d", REGNO (x) - V0_REGNUM + 1);
      else if (REG_P (x) && GP_REGNUM_P (REGNO (x)))
	asm_fprintf (f, "x%d", REGNO (x) - R0_REGNUM + 1);
      else
	output_operand_lossage ("incompatible register operand for '%%%c'",
				code);
      break;

    case 'X':
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "0x%wx", UINTVAL (x) & 0xffff);
      break;

    case 'C':
      {
	/* Print a replicated constant in hex.  */
	if (!const_vec_duplicate_p (x, &elt) || !CONST_INT_P (elt))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
	scalar_mode inner_mode = GET_MODE_INNER (GET_MODE (x));
	asm_fprintf (f, "0x%wx", UINTVAL (elt) & GET_MODE_MASK (inner_mode));
      }
      break;

    case 'D':
      {
	/* Print a replicated constant in decimal, treating it as
	   unsigned.  */
	if (!const_vec_duplicate_p (x, &elt) || !CONST_INT_P (elt))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
	scalar_mode inner_mode = GET_MODE_INNER (GET_MODE (x));
	asm_fprintf (f, "%wd", UINTVAL (elt) & GET_MODE_MASK (inner_mode));
      }
      break;

    case 'w':
    case 'x':
      if (x == const0_rtx
	  || (CONST_DOUBLE_P (x) && aarch64_float_const_zero_rtx_p (x)))
	{
	  asm_fprintf (f, "%czr", code);
	  break;
	}

      if (REG_P (x) && GP_REGNUM_P (REGNO (x)))
	{
	  asm_fprintf (f, "%c%d", code, REGNO (x) - R0_REGNUM);
	  break;
	}

      if (REG_P (x) && REGNO (x) == SP_REGNUM)
	{
	  asm_fprintf (f, "%ssp", code == 'w' ? "w" : "");
	  break;
	}

      /* Fall through */

    case 0:
      if (x == NULL)
	{
	  output_operand_lossage ("missing operand");
	  return;
	}

      switch (GET_CODE (x))
	{
	case REG:
	  if (aarch64_sve_data_mode_p (GET_MODE (x)))
	    {
	      if (REG_NREGS (x) == 1)
		asm_fprintf (f, "z%d", REGNO (x) - V0_REGNUM);
	      else
		{
		  char suffix
		    = sizetochar (GET_MODE_UNIT_BITSIZE (GET_MODE (x)));
		  asm_fprintf (f, "{z%d.%c - z%d.%c}",
			       REGNO (x) - V0_REGNUM, suffix,
			       END_REGNO (x) - V0_REGNUM - 1, suffix);
		}
	    }
	  else
	    asm_fprintf (f, "%s", reg_names [REGNO (x)]);
	  break;

	case MEM:
	  output_address (GET_MODE (x), XEXP (x, 0));
	  break;

	case LABEL_REF:
	case SYMBOL_REF:
	  output_addr_const (asm_out_file, x);
	  break;

	case CONST_INT:
	  asm_fprintf (f, "%wd", INTVAL (x));
	  break;

	case CONST:
	  if (!VECTOR_MODE_P (GET_MODE (x)))
	    {
	      output_addr_const (asm_out_file, x);
	      break;
	    }
	  /* fall through */

	case CONST_VECTOR:
	  if (!const_vec_duplicate_p (x, &elt))
	    {
	      output_operand_lossage ("invalid vector constant");
	      return;
	    }

	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_INT)
	    asm_fprintf (f, "%wd", INTVAL (elt));
	  else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_FLOAT
		   && aarch64_print_vector_float_operand (f, x, false))
	    ;
	  else
	    {
	      output_operand_lossage ("invalid vector constant");
	      return;
	    }
	  break;

	case CONST_DOUBLE:
	  /* Since we define TARGET_SUPPORTS_WIDE_INT we shouldn't ever
	     be getting CONST_DOUBLEs holding integers.  */
	  gcc_assert (GET_MODE (x) != VOIDmode);
	  if (aarch64_float_const_zero_rtx_p (x))
	    {
	      fputc ('0', f);
	      break;
	    }
	  else if (aarch64_float_const_representable_p (x))
	    {
#define buf_size 20
	      char float_buf[buf_size] = {'\0'};
	      real_to_decimal_for_mode (float_buf,
					CONST_DOUBLE_REAL_VALUE (x),
					buf_size, buf_size,
					1, GET_MODE (x));
	      asm_fprintf (asm_out_file, "%s", float_buf);
	      break;
#undef buf_size
	    }
	  output_operand_lossage ("invalid constant");
	  return;
	default:
	  output_operand_lossage ("invalid operand");
	  return;
	}
      break;

    case 'A':
      if (GET_CODE (x) == HIGH)
	x = XEXP (x, 0);

      switch (aarch64_classify_symbolic_expression (x))
	{
	case SYMBOL_SMALL_GOT_4G:
	  asm_fprintf (asm_out_file, ":got:");
	  break;

	case SYMBOL_SMALL_TLSGD:
	  asm_fprintf (asm_out_file, ":tlsgd:");
	  break;

	case SYMBOL_SMALL_TLSDESC:
	  asm_fprintf (asm_out_file, ":tlsdesc:");
	  break;

	case SYMBOL_SMALL_TLSIE:
	  asm_fprintf (asm_out_file, ":gottprel:");
	  break;

	case SYMBOL_TLSLE24:
	  asm_fprintf (asm_out_file, ":tprel:");
	  break;

	case SYMBOL_TINY_GOT:
	  gcc_unreachable ();
	  break;

	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'L':
      switch (aarch64_classify_symbolic_expression (x))
	{
	case SYMBOL_SMALL_GOT_4G:
	  asm_fprintf (asm_out_file, ":lo12:");
	  break;

	case SYMBOL_SMALL_TLSGD:
	  asm_fprintf (asm_out_file, ":tlsgd_lo12:");
	  break;

	case SYMBOL_SMALL_TLSDESC:
	  asm_fprintf (asm_out_file, ":tlsdesc_lo12:");
	  break;

	case SYMBOL_SMALL_TLSIE:
	  asm_fprintf (asm_out_file, ":gottprel_lo12:");
	  break;

	case SYMBOL_TLSLE12:
	  asm_fprintf (asm_out_file, ":tprel_lo12:");
	  break;

	case SYMBOL_TLSLE24:
	  asm_fprintf (asm_out_file, ":tprel_lo12_nc:");
	  break;

	case SYMBOL_TINY_GOT:
	  asm_fprintf (asm_out_file, ":got:");
	  break;

	case SYMBOL_TINY_TLSIE:
	  asm_fprintf (asm_out_file, ":gottprel:");
	  break;

	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'G':
      switch (aarch64_classify_symbolic_expression (x))
	{
	case SYMBOL_TLSLE24:
	  asm_fprintf (asm_out_file, ":tprel_hi12:");
	  break;
	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'k':
      {
	HOST_WIDE_INT cond_code;

	if (!CONST_INT_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	cond_code = INTVAL (x);
	gcc_assert (cond_code >= 0 && cond_code <= AARCH64_NV);
	asm_fprintf (f, "%d", aarch64_nzcv_codes[cond_code]);
      }
      break;

    case 'y':
    case 'z':
      {
	machine_mode mode = GET_MODE (x);

	if (GET_CODE (x) != MEM
	    || (code == 'y' && maybe_ne (GET_MODE_SIZE (mode), 16)))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	if (!aarch64_print_address_internal (f, mode, XEXP (x, 0),
					    code == 'y'
					    ? ADDR_QUERY_LDP_STP_N
					    : ADDR_QUERY_LDP_STP))
	  output_operand_lossage ("invalid operand prefix '%%%c'", code);
      }
      break;

    default:
      output_operand_lossage ("invalid operand prefix '%%%c'", code);
      return;
    }
}

/* Print address 'x' of a memory access with mode 'mode'.
   'op' is the context required by aarch64_classify_address.  It can either be
   MEM for a normal memory access or PARALLEL for LDP/STP.  */
static bool
aarch64_print_address_internal (FILE *f, machine_mode mode, rtx x,
				aarch64_addr_query_type type)
{
  struct aarch64_address_info addr;
  unsigned int size, vec_flags;

  /* Check all addresses are Pmode - including ILP32.  */
  if (GET_MODE (x) != Pmode
      && (!CONST_INT_P (x)
	  || trunc_int_for_mode (INTVAL (x), Pmode) != INTVAL (x)))
    {
      output_operand_lossage ("invalid address mode");
      return false;
    }

  if (aarch64_classify_address (&addr, x, mode, true, type))
    switch (addr.type)
      {
      case ADDRESS_REG_IMM:
	if (known_eq (addr.const_offset, 0))
	  {
	    asm_fprintf (f, "[%s]", reg_names[REGNO (addr.base)]);
	    return true;
	  }

	vec_flags = aarch64_classify_vector_mode (mode);
	if (vec_flags & VEC_ANY_SVE)
	  {
	    HOST_WIDE_INT vnum
	      = exact_div (addr.const_offset,
			   aarch64_vl_bytes (mode, vec_flags)).to_constant ();
	    asm_fprintf (f, "[%s, #%wd, mul vl]",
			 reg_names[REGNO (addr.base)], vnum);
	    return true;
	  }

	asm_fprintf (f, "[%s, %wd]", reg_names[REGNO (addr.base)],
		     INTVAL (addr.offset));
	return true;

      case ADDRESS_REG_REG:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, %s]", reg_names [REGNO (addr.base)],
		       reg_names [REGNO (addr.offset)]);
	else
	  asm_fprintf (f, "[%s, %s, lsl %u]", reg_names [REGNO (addr.base)],
		       reg_names [REGNO (addr.offset)], addr.shift);
	return true;

      case ADDRESS_REG_UXTW:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, w%d, uxtw]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM);
	else
	  asm_fprintf (f, "[%s, w%d, uxtw %u]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM, addr.shift);
	return true;

      case ADDRESS_REG_SXTW:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, w%d, sxtw]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM);
	else
	  asm_fprintf (f, "[%s, w%d, sxtw %u]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM, addr.shift);
	return true;

      case ADDRESS_REG_WB:
	/* Writeback is only supported for fixed-width modes.  */
	size = GET_MODE_SIZE (mode).to_constant ();
	switch (GET_CODE (x))
	  {
	  case PRE_INC:
	    asm_fprintf (f, "[%s, %d]!", reg_names [REGNO (addr.base)], size);
	    return true;
	  case POST_INC:
	    asm_fprintf (f, "[%s], %d", reg_names [REGNO (addr.base)], size);
	    return true;
	  case PRE_DEC:
	    asm_fprintf (f, "[%s, -%d]!", reg_names [REGNO (addr.base)], size);
	    return true;
	  case POST_DEC:
	    asm_fprintf (f, "[%s], -%d", reg_names [REGNO (addr.base)], size);
	    return true;
	  case PRE_MODIFY:
	    asm_fprintf (f, "[%s, %wd]!", reg_names[REGNO (addr.base)],
			 INTVAL (addr.offset));
	    return true;
	  case POST_MODIFY:
	    asm_fprintf (f, "[%s], %wd", reg_names[REGNO (addr.base)],
			 INTVAL (addr.offset));
	    return true;
	  default:
	    break;
	  }
	break;

      case ADDRESS_LO_SUM:
	asm_fprintf (f, "[%s, #:lo12:", reg_names [REGNO (addr.base)]);
	output_addr_const (f, addr.offset);
	asm_fprintf (f, "]");
	return true;

      case ADDRESS_SYMBOLIC:
	output_addr_const (f, x);
	return true;
      }

  return false;
}

/* Print address 'x' of a memory access with mode 'mode'.  */
static void
aarch64_print_operand_address (FILE *f, machine_mode mode, rtx x)
{
  if (!aarch64_print_address_internal (f, mode, x, ADDR_QUERY_ANY))
    output_addr_const (f, x);
}

bool
aarch64_label_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return true;

  /* UNSPEC_TLS entries for a symbol include a LABEL_REF for the
     referencing instruction, but they are constant offsets, not
     symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return false;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (aarch64_label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && aarch64_label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Implement REGNO_REG_CLASS.  */

enum reg_class
aarch64_regno_regclass (unsigned regno)
{
  if (GP_REGNUM_P (regno))
    return GENERAL_REGS;

  if (regno == SP_REGNUM)
    return STACK_REG;

  if (regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return POINTER_REGS;

  if (FP_REGNUM_P (regno))
    return (FP_LO8_REGNUM_P (regno) ? FP_LO8_REGS
	    : FP_LO_REGNUM_P (regno) ? FP_LO_REGS : FP_REGS);

  if (PR_REGNUM_P (regno))
    return PR_LO_REGNUM_P (regno) ? PR_LO_REGS : PR_HI_REGS;

  if (regno == FFR_REGNUM || regno == FFRT_REGNUM)
    return FFR_REGS;

  return NO_REGS;
}

/* OFFSET is an address offset for mode MODE, which has SIZE bytes.
   If OFFSET is out of range, return an offset of an anchor point
   that is in range.  Return 0 otherwise.  */

static HOST_WIDE_INT
aarch64_anchor_offset (HOST_WIDE_INT offset, HOST_WIDE_INT size,
		       machine_mode mode)
{
  /* Does it look like we'll need a 16-byte load/store-pair operation?  */
  if (size > 16)
    return (offset + 0x400) & ~0x7f0;

  /* For offsets that aren't a multiple of the access size, the limit is
     -256...255.  */
  if (offset & (size - 1))
    {
      /* BLKmode typically uses LDP of X-registers.  */
      if (mode == BLKmode)
	return (offset + 512) & ~0x3ff;
      return (offset + 0x100) & ~0x1ff;
    }

  /* Small negative offsets are supported.  */
  if (IN_RANGE (offset, -256, 0))
    return 0;

  if (mode == TImode || mode == TFmode)
    return (offset + 0x100) & ~0x1ff;

  /* Use 12-bit offset by access size.  */
  return offset & (~0xfff * size);
}

static rtx
aarch64_legitimize_address (rtx x, rtx /* orig_x  */, machine_mode mode)
{
  /* Try to split X+CONST into Y=X+(CONST & ~mask), Y+(CONST&mask),
     where mask is selected by alignment and size of the offset.
     We try to pick as large a range for the offset as possible to
     maximize the chance of a CSE.  However, for aligned addresses
     we limit the range to 4k so that structures with different sized
     elements are likely to use the same base.  We need to be careful
     not to split a CONST for some forms of address expression, otherwise
     it will generate sub-optimal code.  */

  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    {
      rtx base = XEXP (x, 0);
      rtx offset_rtx = XEXP (x, 1);
      HOST_WIDE_INT offset = INTVAL (offset_rtx);

      if (GET_CODE (base) == PLUS)
	{
	  rtx op0 = XEXP (base, 0);
	  rtx op1 = XEXP (base, 1);

	  /* Force any scaling into a temp for CSE.  */
	  op0 = force_reg (Pmode, op0);
	  op1 = force_reg (Pmode, op1);

	  /* Let the pointer register be in op0.  */
	  if (REG_POINTER (op1))
	    std::swap (op0, op1);

	  /* If the pointer is virtual or frame related, then we know that
	     virtual register instantiation or register elimination is going
	     to apply a second constant.  We want the two constants folded
	     together easily.  Therefore, emit as (OP0 + CONST) + OP1.  */
	  if (virt_or_elim_regno_p (REGNO (op0)))
	    {
	      base = expand_binop (Pmode, add_optab, op0, offset_rtx,
				   NULL_RTX, true, OPTAB_DIRECT);
	      return gen_rtx_PLUS (Pmode, base, op1);
	    }

	  /* Otherwise, in order to encourage CSE (and thence loop strength
	     reduce) scaled addresses, emit as (OP0 + OP1) + CONST.  */
	  base = expand_binop (Pmode, add_optab, op0, op1,
			       NULL_RTX, true, OPTAB_DIRECT);
	  x = gen_rtx_PLUS (Pmode, base, offset_rtx);
	}

      HOST_WIDE_INT size;
      if (GET_MODE_SIZE (mode).is_constant (&size))
	{
	  HOST_WIDE_INT base_offset = aarch64_anchor_offset (offset, size,
							     mode);
	  if (base_offset != 0)
	    {
	      base = plus_constant (Pmode, base, base_offset);
	      base = force_operand (base, NULL_RTX);
	      return plus_constant (Pmode, base, offset - base_offset);
	    }
	}
    }

  return x;
}

static reg_class_t
aarch64_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x,
			  reg_class_t rclass,
			  machine_mode mode,
			  secondary_reload_info *sri)
{
  /* Use aarch64_sve_reload_mem for SVE memory reloads that cannot use
     LDR and STR.  See the comment at the head of aarch64-sve.md for
     more details about the big-endian handling.  */
  if (reg_class_subset_p (rclass, FP_REGS)
      && !((REG_P (x) && HARD_REGISTER_P (x))
	   || aarch64_simd_valid_immediate (x, NULL))
      && mode != VNx16QImode)
    {
      unsigned int vec_flags = aarch64_classify_vector_mode (mode);
      if ((vec_flags & VEC_SVE_DATA)
	  && ((vec_flags & VEC_PARTIAL) || BYTES_BIG_ENDIAN))
	{
	  sri->icode = CODE_FOR_aarch64_sve_reload_mem;
	  return NO_REGS;
	}
    }

  /* If we have to disable direct literal pool loads and stores because the
     function is too big, then we need a scratch register.  */
  if (MEM_P (x) && GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x)
      && (SCALAR_FLOAT_MODE_P (GET_MODE (x))
	  || targetm.vector_mode_supported_p (GET_MODE (x)))
      && !aarch64_pcrelative_literal_loads)
    {
      sri->icode = code_for_aarch64_reload_movcp (mode, DImode);
      return NO_REGS;
    }

  /* Without the TARGET_SIMD instructions we cannot move a Q register
     to a Q register directly.  We need a scratch.  */
  if (REG_P (x) && (mode == TFmode || mode == TImode) && mode == GET_MODE (x)
      && FP_REGNUM_P (REGNO (x)) && !TARGET_SIMD
      && reg_class_subset_p (rclass, FP_REGS))
    {
      sri->icode = code_for_aarch64_reload_mov (mode);
      return NO_REGS;
    }

  /* A TFmode or TImode memory access should be handled via an FP_REGS
     because AArch64 has richer addressing modes for LDR/STR instructions
     than LDP/STP instructions.  */
  if (TARGET_FLOAT && rclass == GENERAL_REGS
      && known_eq (GET_MODE_SIZE (mode), 16) && MEM_P (x))
    return FP_REGS;

  if (rclass == FP_REGS && (mode == TImode || mode == TFmode) && CONSTANT_P(x))
      return GENERAL_REGS;

  return NO_REGS;
}

static bool
aarch64_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  gcc_assert (from == ARG_POINTER_REGNUM || from == FRAME_POINTER_REGNUM);

  /* If we need a frame pointer, ARG_POINTER_REGNUM and FRAME_POINTER_REGNUM
     can only eliminate to HARD_FRAME_POINTER_REGNUM.  */
  if (frame_pointer_needed)
    return to == HARD_FRAME_POINTER_REGNUM;
  return true;
}

poly_int64
aarch64_initial_elimination_offset (unsigned from, unsigned to)
{
  if (to == HARD_FRAME_POINTER_REGNUM)
    {
      if (from == ARG_POINTER_REGNUM)
	return cfun->machine->frame.hard_fp_offset;

      if (from == FRAME_POINTER_REGNUM)
	return cfun->machine->frame.hard_fp_offset
	       - cfun->machine->frame.locals_offset;
    }

  if (to == STACK_POINTER_REGNUM)
    {
      if (from == FRAME_POINTER_REGNUM)
	  return cfun->machine->frame.frame_size
		 - cfun->machine->frame.locals_offset;
    }

  return cfun->machine->frame.frame_size;
}

/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
aarch64_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;
  return get_hard_reg_initial_val (Pmode, LR_REGNUM);
}


static void
aarch64_asm_trampoline_template (FILE *f)
{
  int offset1 = 16;
  int offset2 = 20;

  if (aarch64_bti_enabled ())
    {
      asm_fprintf (f, "\thint\t34 // bti c\n");
      offset1 -= 4;
      offset2 -= 4;
    }

  if (TARGET_ILP32)
    {
      asm_fprintf (f, "\tldr\tw%d, .+%d\n", IP1_REGNUM - R0_REGNUM, offset1);
      asm_fprintf (f, "\tldr\tw%d, .+%d\n", STATIC_CHAIN_REGNUM - R0_REGNUM,
		   offset1);
    }
  else
    {
      asm_fprintf (f, "\tldr\t%s, .+%d\n", reg_names [IP1_REGNUM], offset1);
      asm_fprintf (f, "\tldr\t%s, .+%d\n", reg_names [STATIC_CHAIN_REGNUM],
		   offset2);
    }
  asm_fprintf (f, "\tbr\t%s\n", reg_names [IP1_REGNUM]);

  /* The trampoline needs an extra padding instruction.  In case if BTI is
     enabled the padding instruction is replaced by the BTI instruction at
     the beginning.  */
  if (!aarch64_bti_enabled ())
    assemble_aligned_integer (4, const0_rtx);

  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
}

static void
aarch64_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr, mem, a_tramp;
  const int tramp_code_sz = 16;

  /* Don't need to copy the trailing D-words, we fill those in below.  */
  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (tramp_code_sz), BLOCK_OP_NORMAL);
  mem = adjust_address (m_tramp, ptr_mode, tramp_code_sz);
  fnaddr = XEXP (DECL_RTL (fndecl), 0);
  if (GET_MODE (fnaddr) != ptr_mode)
    fnaddr = convert_memory_address (ptr_mode, fnaddr);
  emit_move_insn (mem, fnaddr);

  mem = adjust_address (m_tramp, ptr_mode, tramp_code_sz + POINTER_BYTES);
  emit_move_insn (mem, chain_value);

  /* XXX We should really define a "clear_cache" pattern and use
     gen_clear_cache().  */
  a_tramp = XEXP (m_tramp, 0);
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),
		     LCT_NORMAL, VOIDmode, a_tramp, ptr_mode,
		     plus_constant (ptr_mode, a_tramp, TRAMPOLINE_SIZE),
		     ptr_mode);
}

static unsigned char
aarch64_class_max_nregs (reg_class_t regclass, machine_mode mode)
{
  /* ??? Logically we should only need to provide a value when
     HARD_REGNO_MODE_OK says that at least one register in REGCLASS
     can hold MODE, but at the moment we need to handle all modes.
     Just ignore any runtime parts for registers that can't store them.  */
  HOST_WIDE_INT lowest_size = constant_lower_bound (GET_MODE_SIZE (mode));
  unsigned int nregs, vec_flags;
  switch (regclass)
    {
    case TAILCALL_ADDR_REGS:
    case POINTER_REGS:
    case GENERAL_REGS:
    case ALL_REGS:
    case POINTER_AND_FP_REGS:
    case FP_REGS:
    case FP_LO_REGS:
    case FP_LO8_REGS:
      vec_flags = aarch64_classify_vector_mode (mode);
      if ((vec_flags & VEC_SVE_DATA)
	  && constant_multiple_p (GET_MODE_SIZE (mode),
				  aarch64_vl_bytes (mode, vec_flags), &nregs))
	return nregs;
      return (vec_flags & VEC_ADVSIMD
	      ? CEIL (lowest_size, UNITS_PER_VREG)
	      : CEIL (lowest_size, UNITS_PER_WORD));
    case STACK_REG:
    case PR_REGS:
    case PR_LO_REGS:
    case PR_HI_REGS:
    case FFR_REGS:
    case PR_AND_FFR_REGS:
      return 1;

    case NO_REGS:
      return 0;

    default:
      break;
    }
  gcc_unreachable ();
}

static reg_class_t
aarch64_preferred_reload_class (rtx x, reg_class_t regclass)
{
  if (regclass == POINTER_REGS)
    return GENERAL_REGS;

  if (regclass == STACK_REG)
    {
      if (REG_P(x)
	  && reg_class_subset_p (REGNO_REG_CLASS (REGNO (x)), POINTER_REGS))
	  return regclass;

      return NO_REGS;
    }

  /* Register eliminiation can result in a request for
     SP+constant->FP_REGS.  We cannot support such operations which
     use SP as source and an FP_REG as destination, so reject out
     right now.  */
  if (! reg_class_subset_p (regclass, GENERAL_REGS) && GET_CODE (x) == PLUS)
    {
      rtx lhs = XEXP (x, 0);

      /* Look through a possible SUBREG introduced by ILP32.  */
      if (GET_CODE (lhs) == SUBREG)
	lhs = SUBREG_REG (lhs);

      gcc_assert (REG_P (lhs));
      gcc_assert (reg_class_subset_p (REGNO_REG_CLASS (REGNO (lhs)),
				      POINTER_REGS));
      return NO_REGS;
    }

  return regclass;
}

void
aarch64_asm_output_labelref (FILE* f, const char *name)
{
  asm_fprintf (f, "%U%s", name);
}

static void
aarch64_elf_asm_constructor (rtx symbol, int priority)
{
  if (priority == DEFAULT_INIT_PRIORITY)
    default_ctor_section_asm_out_constructor (symbol, priority);
  else
    {
      section *s;
      /* While priority is known to be in range [0, 65535], so 18 bytes
         would be enough, the compiler might not know that.  To avoid
         -Wformat-truncation false positive, use a larger size.  */
      char buf[23];
      snprintf (buf, sizeof (buf), ".init_array.%.5u", priority);
      s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL);
      switch_to_section (s);
      assemble_align (POINTER_SIZE);
      assemble_aligned_integer (POINTER_BYTES, symbol);
    }
}

static void
aarch64_elf_asm_destructor (rtx symbol, int priority)
{
  if (priority == DEFAULT_INIT_PRIORITY)
    default_dtor_section_asm_out_destructor (symbol, priority);
  else
    {
      section *s;
      /* While priority is known to be in range [0, 65535], so 18 bytes
         would be enough, the compiler might not know that.  To avoid
         -Wformat-truncation false positive, use a larger size.  */
      char buf[23];
      snprintf (buf, sizeof (buf), ".fini_array.%.5u", priority);
      s = get_section (buf, SECTION_WRITE | SECTION_NOTYPE, NULL);
      switch_to_section (s);
      assemble_align (POINTER_SIZE);
      assemble_aligned_integer (POINTER_BYTES, symbol);
    }
}

const char*
aarch64_output_casesi (rtx *operands)
{
  char buf[100];
  char label[100];
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[2])));
  int index;
  static const char *const patterns[4][2] =
  {
    {
      "ldrb\t%w3, [%0,%w1,uxtw]",
      "add\t%3, %4, %w3, sxtb #2"
    },
    {
      "ldrh\t%w3, [%0,%w1,uxtw #1]",
      "add\t%3, %4, %w3, sxth #2"
    },
    {
      "ldr\t%w3, [%0,%w1,uxtw #2]",
      "add\t%3, %4, %w3, sxtw #2"
    },
    /* We assume that DImode is only generated when not optimizing and
       that we don't really need 64-bit address offsets.  That would
       imply an object file with 8GB of code in a single function!  */
    {
      "ldr\t%w3, [%0,%w1,uxtw #2]",
      "add\t%3, %4, %w3, sxtw #2"
    }
  };

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  scalar_int_mode mode = as_a <scalar_int_mode> (GET_MODE (diff_vec));
  index = exact_log2 (GET_MODE_SIZE (mode));

  gcc_assert (index >= 0 && index <= 3);

  /* Need to implement table size reduction, by chaning the code below.  */
  output_asm_insn (patterns[index][0], operands);
  ASM_GENERATE_INTERNAL_LABEL (label, "Lrtx", CODE_LABEL_NUMBER (operands[2]));
  snprintf (buf, sizeof (buf),
	    "adr\t%%4, %s", targetm.strip_name_encoding (label));
  output_asm_insn (buf, operands);
  output_asm_insn (patterns[index][1], operands);
  output_asm_insn ("br\t%3", operands);
  assemble_label (asm_out_file, label);
  return "";
}


/* Return size in bits of an arithmetic operand which is shifted/scaled and
   masked such that it is suitable for a UXTB, UXTH, or UXTW extend
   operator.  */

int
aarch64_uxt_size (int shift, HOST_WIDE_INT mask)
{
  if (shift >= 0 && shift <= 3)
    {
      int size;
      for (size = 8; size <= 32; size *= 2)
	{
	  HOST_WIDE_INT bits = ((HOST_WIDE_INT)1U << size) - 1;
	  if (mask == bits << shift)
	    return size;
	}
    }
  return 0;
}

/* Constant pools are per function only when PC relative
   literal loads are true or we are in the large memory
   model.  */

static inline bool
aarch64_can_use_per_function_literal_pools_p (void)
{
  return (aarch64_pcrelative_literal_loads
	  || aarch64_cmodel == AARCH64_CMODEL_LARGE);
}

static bool
aarch64_use_blocks_for_constant_p (machine_mode, const_rtx)
{
  /* We can't use blocks for constants when we're using a per-function
     constant pool.  */
  return !aarch64_can_use_per_function_literal_pools_p ();
}

/* Select appropriate section for constants depending
   on where we place literal pools.  */

static section *
aarch64_select_rtx_section (machine_mode mode,
			    rtx x,
			    unsigned HOST_WIDE_INT align)
{
  if (aarch64_can_use_per_function_literal_pools_p ())
    return function_section (current_function_decl);

  return default_elf_select_rtx_section (mode, x, align);
}

/* Implement ASM_OUTPUT_POOL_EPILOGUE.  */
void
aarch64_asm_output_pool_epilogue (FILE *f, const char *, tree,
				  HOST_WIDE_INT offset)
{
  /* When using per-function literal pools, we must ensure that any code
     section is aligned to the minimal instruction length, lest we get
     errors from the assembler re "unaligned instructions".  */
  if ((offset & 3) && aarch64_can_use_per_function_literal_pools_p ())
    ASM_OUTPUT_ALIGN (f, 2);
}

/* Costs.  */

/* Helper function for rtx cost calculation.  Strip a shift expression
   from X.  Returns the inner operand if successful, or the original
   expression on failure.  */
static rtx
aarch64_strip_shift (rtx x)
{
  rtx op = x;

  /* We accept both ROTATERT and ROTATE: since the RHS must be a constant
     we can convert both to ROR during final output.  */
  if ((GET_CODE (op) == ASHIFT
       || GET_CODE (op) == ASHIFTRT
       || GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ROTATERT
       || GET_CODE (op) == ROTATE)
      && CONST_INT_P (XEXP (op, 1)))
    return XEXP (op, 0);

  if (GET_CODE (op) == MULT
      && CONST_INT_P (XEXP (op, 1))
      && ((unsigned) exact_log2 (INTVAL (XEXP (op, 1)))) < 64)
    return XEXP (op, 0);

  return x;
}

/* Helper function for rtx cost calculation.  Strip an extend
   expression from X.  Returns the inner operand if successful, or the
   original expression on failure.  We deal with a number of possible
   canonicalization variations here. If STRIP_SHIFT is true, then
   we can strip off a shift also.  */
static rtx
aarch64_strip_extend (rtx x, bool strip_shift)
{
  scalar_int_mode mode;
  rtx op = x;

  if (!is_a <scalar_int_mode> (GET_MODE (op), &mode))
    return op;

  /* Zero and sign extraction of a widened value.  */
  if ((GET_CODE (op) == ZERO_EXTRACT || GET_CODE (op) == SIGN_EXTRACT)
      && XEXP (op, 2) == const0_rtx
      && GET_CODE (XEXP (op, 0)) == MULT
      && aarch64_is_extend_from_extract (mode, XEXP (XEXP (op, 0), 1),
					 XEXP (op, 1)))
    return XEXP (XEXP (op, 0), 0);

  /* It can also be represented (for zero-extend) as an AND with an
     immediate.  */
  if (GET_CODE (op) == AND
      && GET_CODE (XEXP (op, 0)) == MULT
      && CONST_INT_P (XEXP (XEXP (op, 0), 1))
      && CONST_INT_P (XEXP (op, 1))
      && aarch64_uxt_size (exact_log2 (INTVAL (XEXP (XEXP (op, 0), 1))),
			   INTVAL (XEXP (op, 1))) != 0)
    return XEXP (XEXP (op, 0), 0);

  /* Now handle extended register, as this may also have an optional
     left shift by 1..4.  */
  if (strip_shift
      && GET_CODE (op) == ASHIFT
      && CONST_INT_P (XEXP (op, 1))
      && ((unsigned HOST_WIDE_INT) INTVAL (XEXP (op, 1))) <= 4)
    op = XEXP (op, 0);

  if (GET_CODE (op) == ZERO_EXTEND
      || GET_CODE (op) == SIGN_EXTEND)
    op = XEXP (op, 0);

  if (op != x)
    return op;

  return x;
}

/* Return true iff CODE is a shift supported in combination
   with arithmetic instructions.  */

static bool
aarch64_shift_p (enum rtx_code code)
{
  return code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT;
}


/* Return true iff X is a cheap shift without a sign extend. */

static bool
aarch64_cheap_mult_shift_p (rtx x)
{
  rtx op0, op1;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if (!(aarch64_tune_params.extra_tuning_flags
                      & AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND))
    return false;

  if (GET_CODE (op0) == SIGN_EXTEND)
    return false;

  if (GET_CODE (x) == ASHIFT && CONST_INT_P (op1)
      && UINTVAL (op1) <= 4)
    return true;

  if (GET_CODE (x) != MULT || !CONST_INT_P (op1))
    return false;

  HOST_WIDE_INT l2 = exact_log2 (INTVAL (op1));

  if (l2 > 0 && l2 <= 4)
    return true;

  return false;
}

/* Helper function for rtx cost calculation.  Calculate the cost of
   a MULT or ASHIFT, which may be part of a compound PLUS/MINUS rtx.
   Return the calculated cost of the expression, recursing manually in to
   operands where needed.  */

static int
aarch64_rtx_mult_cost (rtx x, enum rtx_code code, int outer, bool speed)
{
  rtx op0, op1;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params.insn_extra_cost;
  int cost = 0;
  bool compound_p = (outer == PLUS || outer == MINUS);
  machine_mode mode = GET_MODE (x);

  gcc_checking_assert (code == MULT);

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if (VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  /* Integer multiply/fma.  */
  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      /* The multiply will be canonicalized as a shift, cost it as such.  */
      if (aarch64_shift_p (GET_CODE (x))
	  || (CONST_INT_P (op1)
	      && exact_log2 (INTVAL (op1)) > 0))
	{
	  bool is_extend = GET_CODE (op0) == ZERO_EXTEND
	                   || GET_CODE (op0) == SIGN_EXTEND;
	  if (speed)
	    {
	      if (compound_p)
	        {
		  /* If the shift is considered cheap,
		     then don't add any cost. */
		  if (aarch64_cheap_mult_shift_p (x))
		    ;
	          else if (REG_P (op1))
		    /* ARITH + shift-by-register.  */
		    cost += extra_cost->alu.arith_shift_reg;
		  else if (is_extend)
		    /* ARITH + extended register.  We don't have a cost field
		       for ARITH+EXTEND+SHIFT, so use extend_arith here.  */
		    cost += extra_cost->alu.extend_arith;
		  else
		    /* ARITH + shift-by-immediate.  */
		    cost += extra_cost->alu.arith_shift;
		}
	      else
		/* LSL (immediate).  */
	        cost += extra_cost->alu.shift;

	    }
	  /* Strip extends as we will have costed them in the case above.  */
	  if (is_extend)
	    op0 = aarch64_strip_extend (op0, true);

	  cost += rtx_cost (op0, VOIDmode, code, 0, speed);

	  return cost;
	}

      /* MNEG or [US]MNEGL.  Extract the NEG operand and indicate that it's a
	 compound and let the below cases handle it.  After all, MNEG is a
	 special-case alias of MSUB.  */
      if (GET_CODE (op0) == NEG)
	{
	  op0 = XEXP (op0, 0);
	  compound_p = true;
	}

      /* Integer multiplies or FMAs have zero/sign extending variants.  */
      if ((GET_CODE (op0) == ZERO_EXTEND
	   && GET_CODE (op1) == ZERO_EXTEND)
	  || (GET_CODE (op0) == SIGN_EXTEND
	      && GET_CODE (op1) == SIGN_EXTEND))
	{
	  cost += rtx_cost (XEXP (op0, 0), VOIDmode, MULT, 0, speed);
	  cost += rtx_cost (XEXP (op1, 0), VOIDmode, MULT, 1, speed);

	  if (speed)
	    {
	      if (compound_p)
		/* SMADDL/UMADDL/UMSUBL/SMSUBL.  */
		cost += extra_cost->mult[0].extend_add;
	      else
		/* MUL/SMULL/UMULL.  */
		cost += extra_cost->mult[0].extend;
	    }

	  return cost;
	}

      /* This is either an integer multiply or a MADD.  In both cases
	 we want to recurse and cost the operands.  */
      cost += rtx_cost (op0, mode, MULT, 0, speed);
      cost += rtx_cost (op1, mode, MULT, 1, speed);

      if (speed)
	{
	  if (compound_p)
	    /* MADD/MSUB.  */
	    cost += extra_cost->mult[mode == DImode].add;
	  else
	    /* MUL.  */
	    cost += extra_cost->mult[mode == DImode].simple;
	}

      return cost;
    }
  else
    {
      if (speed)
	{
	  /* Floating-point FMA/FMUL can also support negations of the
	     operands, unless the rounding mode is upward or downward in
	     which case FNMUL is different than FMUL with operand negation.  */
	  bool neg0 = GET_CODE (op0) == NEG;
	  bool neg1 = GET_CODE (op1) == NEG;
	  if (compound_p || !flag_rounding_math || (neg0 && neg1))
	    {
	      if (neg0)
		op0 = XEXP (op0, 0);
	      if (neg1)
		op1 = XEXP (op1, 0);
	    }

	  if (compound_p)
	    /* FMADD/FNMADD/FNMSUB/FMSUB.  */
	    cost += extra_cost->fp[mode == DFmode].fma;
	  else
	    /* FMUL/FNMUL.  */
	    cost += extra_cost->fp[mode == DFmode].mult;
	}

      cost += rtx_cost (op0, mode, MULT, 0, speed);
      cost += rtx_cost (op1, mode, MULT, 1, speed);
      return cost;
    }
}

static int
aarch64_address_cost (rtx x,
		      machine_mode mode,
		      addr_space_t as ATTRIBUTE_UNUSED,
		      bool speed)
{
  enum rtx_code c = GET_CODE (x);
  const struct cpu_addrcost_table *addr_cost = aarch64_tune_params.addr_cost;
  struct aarch64_address_info info;
  int cost = 0;
  info.shift = 0;

  if (!aarch64_classify_address (&info, x, mode, false))
    {
      if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF)
	{
	  /* This is a CONST or SYMBOL ref which will be split
	     in a different way depending on the code model in use.
	     Cost it through the generic infrastructure.  */
	  int cost_symbol_ref = rtx_cost (x, Pmode, MEM, 1, speed);
	  /* Divide through by the cost of one instruction to
	     bring it to the same units as the address costs.  */
	  cost_symbol_ref /= COSTS_N_INSNS (1);
	  /* The cost is then the cost of preparing the address,
	     followed by an immediate (possibly 0) offset.  */
	  return cost_symbol_ref + addr_cost->imm_offset;
	}
      else
	{
	  /* This is most likely a jump table from a case
	     statement.  */
	  return addr_cost->register_offset;
	}
    }

  switch (info.type)
    {
      case ADDRESS_LO_SUM:
      case ADDRESS_SYMBOLIC:
      case ADDRESS_REG_IMM:
	cost += addr_cost->imm_offset;
	break;

      case ADDRESS_REG_WB:
	if (c == PRE_INC || c == PRE_DEC || c == PRE_MODIFY)
	  cost += addr_cost->pre_modify;
	else if (c == POST_INC || c == POST_DEC || c == POST_MODIFY)
	  cost += addr_cost->post_modify;
	else
	  gcc_unreachable ();

	break;

      case ADDRESS_REG_REG:
	cost += addr_cost->register_offset;
	break;

      case ADDRESS_REG_SXTW:
	cost += addr_cost->register_sextend;
	break;

      case ADDRESS_REG_UXTW:
	cost += addr_cost->register_zextend;
	break;

      default:
	gcc_unreachable ();
    }


  if (info.shift > 0)
    {
      /* For the sake of calculating the cost of the shifted register
	 component, we can treat same sized modes in the same way.  */
      if (known_eq (GET_MODE_BITSIZE (mode), 16))
	cost += addr_cost->addr_scale_costs.hi;
      else if (known_eq (GET_MODE_BITSIZE (mode), 32))
	cost += addr_cost->addr_scale_costs.si;
      else if (known_eq (GET_MODE_BITSIZE (mode), 64))
	cost += addr_cost->addr_scale_costs.di;
      else
	/* We can't tell, or this is a 128-bit vector.  */
	cost += addr_cost->addr_scale_costs.ti;
    }

  return cost;
}

/* Return the cost of a branch.  If SPEED_P is true then the compiler is
   optimizing for speed.  If PREDICTABLE_P is true then the branch is predicted
   to be taken.  */

int
aarch64_branch_cost (bool speed_p, bool predictable_p)
{
  /* When optimizing for speed, use the cost of unpredictable branches.  */
  const struct cpu_branch_cost *branch_costs =
    aarch64_tune_params.branch_costs;

  if (!speed_p || predictable_p)
    return branch_costs->predictable;
  else
    return branch_costs->unpredictable;
}

/* Return true if the RTX X in mode MODE is a zero or sign extract
   usable in an ADD or SUB (extended register) instruction.  */
static bool
aarch64_rtx_arith_op_extract_p (rtx x, scalar_int_mode mode)
{
  /* Catch add with a sign extract.
     This is add_<optab><mode>_multp2.  */
  if (GET_CODE (x) == SIGN_EXTRACT
      || GET_CODE (x) == ZERO_EXTRACT)
    {
      rtx op0 = XEXP (x, 0);
      rtx op1 = XEXP (x, 1);
      rtx op2 = XEXP (x, 2);

      if (GET_CODE (op0) == MULT
	  && CONST_INT_P (op1)
	  && op2 == const0_rtx
	  && CONST_INT_P (XEXP (op0, 1))
	  && aarch64_is_extend_from_extract (mode,
					     XEXP (op0, 1),
					     op1))
	{
	  return true;
	}
    }
  /* The simple case <ARITH>, XD, XN, XM, [us]xt.
     No shift.  */
  else if (GET_CODE (x) == SIGN_EXTEND
	   || GET_CODE (x) == ZERO_EXTEND)
    return REG_P (XEXP (x, 0));

  return false;
}

static bool
aarch64_frint_unspec_p (unsigned int u)
{
  switch (u)
    {
      case UNSPEC_FRINTZ:
      case UNSPEC_FRINTP:
      case UNSPEC_FRINTM:
      case UNSPEC_FRINTA:
      case UNSPEC_FRINTN:
      case UNSPEC_FRINTX:
      case UNSPEC_FRINTI:
        return true;

      default:
        return false;
    }
}

/* Return true iff X is an rtx that will match an extr instruction
   i.e. as described in the *extr<mode>5_insn family of patterns.
   OP0 and OP1 will be set to the operands of the shifts involved
   on success and will be NULL_RTX otherwise.  */

static bool
aarch64_extr_rtx_p (rtx x, rtx *res_op0, rtx *res_op1)
{
  rtx op0, op1;
  scalar_int_mode mode;
  if (!is_a <scalar_int_mode> (GET_MODE (x), &mode))
    return false;

  *res_op0 = NULL_RTX;
  *res_op1 = NULL_RTX;

  if (GET_CODE (x) != IOR)
    return false;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if ((GET_CODE (op0) == ASHIFT && GET_CODE (op1) == LSHIFTRT)
      || (GET_CODE (op1) == ASHIFT && GET_CODE (op0) == LSHIFTRT))
    {
     /* Canonicalise locally to ashift in op0, lshiftrt in op1.  */
      if (GET_CODE (op1) == ASHIFT)
        std::swap (op0, op1);

      if (!CONST_INT_P (XEXP (op0, 1)) || !CONST_INT_P (XEXP (op1, 1)))
        return false;

      unsigned HOST_WIDE_INT shft_amnt_0 = UINTVAL (XEXP (op0, 1));
      unsigned HOST_WIDE_INT shft_amnt_1 = UINTVAL (XEXP (op1, 1));

      if (shft_amnt_0 < GET_MODE_BITSIZE (mode)
          && shft_amnt_0 + shft_amnt_1 == GET_MODE_BITSIZE (mode))
        {
          *res_op0 = XEXP (op0, 0);
          *res_op1 = XEXP (op1, 0);
          return true;
        }
    }

  return false;
}

/* Calculate the cost of calculating (if_then_else (OP0) (OP1) (OP2)),
   storing it in *COST.  Result is true if the total cost of the operation
   has now been calculated.  */
static bool
aarch64_if_then_else_costs (rtx op0, rtx op1, rtx op2, int *cost, bool speed)
{
  rtx inner;
  rtx comparator;
  enum rtx_code cmpcode;

  if (COMPARISON_P (op0))
    {
      inner = XEXP (op0, 0);
      comparator = XEXP (op0, 1);
      cmpcode = GET_CODE (op0);
    }
  else
    {
      inner = op0;
      comparator = const0_rtx;
      cmpcode = NE;
    }

  if (GET_CODE (op1) == PC || GET_CODE (op2) == PC)
    {
      /* Conditional branch.  */
      if (GET_MODE_CLASS (GET_MODE (inner)) == MODE_CC)
	return true;
      else
	{
	  if (cmpcode == NE || cmpcode == EQ)
	    {
	      if (comparator == const0_rtx)
		{
		  /* TBZ/TBNZ/CBZ/CBNZ.  */
		  if (GET_CODE (inner) == ZERO_EXTRACT)
		    /* TBZ/TBNZ.  */
		    *cost += rtx_cost (XEXP (inner, 0), VOIDmode,
				       ZERO_EXTRACT, 0, speed);
		  else
		    /* CBZ/CBNZ.  */
		    *cost += rtx_cost (inner, VOIDmode, cmpcode, 0, speed);

	        return true;
	      }
	    }
	  else if (cmpcode == LT || cmpcode == GE)
	    {
	      /* TBZ/TBNZ.  */
	      if (comparator == const0_rtx)
		return true;
	    }
	}
    }
  else if (GET_MODE_CLASS (GET_MODE (inner)) == MODE_CC)
    {
      /* CCMP.  */
      if (GET_CODE (op1) == COMPARE)
	{
	  /* Increase cost of CCMP reg, 0, imm, CC to prefer CMP reg, 0.  */
	  if (XEXP (op1, 1) == const0_rtx)
	    *cost += 1;
	  if (speed)
	    {
	      machine_mode mode = GET_MODE (XEXP (op1, 0));
	      const struct cpu_cost_table *extra_cost
		= aarch64_tune_params.insn_extra_cost;

	      if (GET_MODE_CLASS (mode) == MODE_INT)
		*cost += extra_cost->alu.arith;
	      else
		*cost += extra_cost->fp[mode == DFmode].compare;
	    }
	  return true;
	}

      /* It's a conditional operation based on the status flags,
	 so it must be some flavor of CSEL.  */

      /* CSNEG, CSINV, and CSINC are handled for free as part of CSEL.  */
      if (GET_CODE (op1) == NEG
          || GET_CODE (op1) == NOT
          || (GET_CODE (op1) == PLUS && XEXP (op1, 1) == const1_rtx))
	op1 = XEXP (op1, 0);
      else if (GET_CODE (op1) == ZERO_EXTEND && GET_CODE (op2) == ZERO_EXTEND)
	{
	  /* CSEL with zero-extension (*cmovdi_insn_uxtw).  */
	  op1 = XEXP (op1, 0);
	  op2 = XEXP (op2, 0);
	}

      *cost += rtx_cost (op1, VOIDmode, IF_THEN_ELSE, 1, speed);
      *cost += rtx_cost (op2, VOIDmode, IF_THEN_ELSE, 2, speed);
      return true;
    }

  /* We don't know what this is, cost all operands.  */
  return false;
}

/* Check whether X is a bitfield operation of the form shift + extend that
   maps down to a UBFIZ/SBFIZ/UBFX/SBFX instruction.  If so, return the
   operand to which the bitfield operation is applied.  Otherwise return
   NULL_RTX.  */

static rtx
aarch64_extend_bitfield_pattern_p (rtx x)
{
  rtx_code outer_code = GET_CODE (x);
  machine_mode outer_mode = GET_MODE (x);

  if (outer_code != ZERO_EXTEND && outer_code != SIGN_EXTEND
      && outer_mode != SImode && outer_mode != DImode)
    return NULL_RTX;

  rtx inner = XEXP (x, 0);
  rtx_code inner_code = GET_CODE (inner);
  machine_mode inner_mode = GET_MODE (inner);
  rtx op = NULL_RTX;

  switch (inner_code)
    {
      case ASHIFT:
	if (CONST_INT_P (XEXP (inner, 1))
	    && (inner_mode == QImode || inner_mode == HImode))
	  op = XEXP (inner, 0);
	break;
      case LSHIFTRT:
	if (outer_code == ZERO_EXTEND && CONST_INT_P (XEXP (inner, 1))
	    && (inner_mode == QImode || inner_mode == HImode))
	  op = XEXP (inner, 0);
	break;
      case ASHIFTRT:
	if (outer_code == SIGN_EXTEND && CONST_INT_P (XEXP (inner, 1))
	    && (inner_mode == QImode || inner_mode == HImode))
	  op = XEXP (inner, 0);
	break;
      default:
	break;
    }

  return op;
}

/* Return true if the mask and a shift amount from an RTX of the form
   (x << SHFT_AMNT) & MASK are valid to combine into a UBFIZ instruction of
   mode MODE.  See the *andim_ashift<mode>_bfiz pattern.  */

bool
aarch64_mask_and_shift_for_ubfiz_p (scalar_int_mode mode, rtx mask,
				    rtx shft_amnt)
{
  return CONST_INT_P (mask) && CONST_INT_P (shft_amnt)
	 && INTVAL (shft_amnt) < GET_MODE_BITSIZE (mode)
	 && exact_log2 ((INTVAL (mask) >> INTVAL (shft_amnt)) + 1) >= 0
	 && (INTVAL (mask)
	     & ((HOST_WIDE_INT_1U << INTVAL (shft_amnt)) - 1)) == 0;
}

/* Return true if the masks and a shift amount from an RTX of the form
   ((x & MASK1) | ((y << SHIFT_AMNT) & MASK2)) are valid to combine into
   a BFI instruction of mode MODE.  See *arch64_bfi patterns.  */

bool
aarch64_masks_and_shift_for_bfi_p (scalar_int_mode mode,
				   unsigned HOST_WIDE_INT mask1,
				   unsigned HOST_WIDE_INT shft_amnt,
				   unsigned HOST_WIDE_INT mask2)
{
  unsigned HOST_WIDE_INT t;

  /* Verify that there is no overlap in what bits are set in the two masks.  */
  if (mask1 != ~mask2)
    return false;

  /* Verify that mask2 is not all zeros or ones.  */
  if (mask2 == 0 || mask2 == HOST_WIDE_INT_M1U)
    return false;

  /* The shift amount should always be less than the mode size.  */
  gcc_assert (shft_amnt < GET_MODE_BITSIZE (mode));

  /* Verify that the mask being shifted is contiguous and would be in the
     least significant bits after shifting by shft_amnt.  */
  t = mask2 + (HOST_WIDE_INT_1U << shft_amnt);
  return (t == (t & -t));
}

/* Calculate the cost of calculating X, storing it in *COST.  Result
   is true if the total cost of the operation has now been calculated.  */
static bool
aarch64_rtx_costs (rtx x, machine_mode mode, int outer ATTRIBUTE_UNUSED,
		   int param ATTRIBUTE_UNUSED, int *cost, bool speed)
{
  rtx op0, op1, op2;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params.insn_extra_cost;
  int code = GET_CODE (x);
  scalar_int_mode int_mode;

  /* By default, assume that everything has equivalent cost to the
     cheapest instruction.  Any additional costs are applied as a delta
     above this default.  */
  *cost = COSTS_N_INSNS (1);

  switch (code)
    {
    case SET:
      /* The cost depends entirely on the operands to SET.  */
      *cost = 0;
      op0 = SET_DEST (x);
      op1 = SET_SRC (x);

      switch (GET_CODE (op0))
	{
	case MEM:
	  if (speed)
	    {
	      rtx address = XEXP (op0, 0);
	      if (VECTOR_MODE_P (mode))
		*cost += extra_cost->ldst.storev;
	      else if (GET_MODE_CLASS (mode) == MODE_INT)
		*cost += extra_cost->ldst.store;
	      else if (mode == SFmode)
		*cost += extra_cost->ldst.storef;
	      else if (mode == DFmode)
		*cost += extra_cost->ldst.stored;

	      *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	    }

	  *cost += rtx_cost (op1, mode, SET, 1, speed);
	  return true;

	case SUBREG:
	  if (! REG_P (SUBREG_REG (op0)))
	    *cost += rtx_cost (SUBREG_REG (op0), VOIDmode, SET, 0, speed);

	  /* Fall through.  */
	case REG:
	  /* The cost is one per vector-register copied.  */
	  if (VECTOR_MODE_P (GET_MODE (op0)) && REG_P (op1))
	    {
	      int nregs = aarch64_hard_regno_nregs (V0_REGNUM, GET_MODE (op0));
	      *cost = COSTS_N_INSNS (nregs);
	    }
	  /* const0_rtx is in general free, but we will use an
	     instruction to set a register to 0.  */
	  else if (REG_P (op1) || op1 == const0_rtx)
	    {
	      /* The cost is 1 per register copied.  */
	      int nregs = aarch64_hard_regno_nregs (R0_REGNUM, GET_MODE (op0));
	      *cost = COSTS_N_INSNS (nregs);
	    }
          else
	    /* Cost is just the cost of the RHS of the set.  */
	    *cost += rtx_cost (op1, mode, SET, 1, speed);
	  return true;

	case ZERO_EXTRACT:
	case SIGN_EXTRACT:
	  /* Bit-field insertion.  Strip any redundant widening of
	     the RHS to meet the width of the target.  */
	  if (GET_CODE (op1) == SUBREG)
	    op1 = SUBREG_REG (op1);
	  if ((GET_CODE (op1) == ZERO_EXTEND
	       || GET_CODE (op1) == SIGN_EXTEND)
	      && CONST_INT_P (XEXP (op0, 1))
	      && is_a <scalar_int_mode> (GET_MODE (XEXP (op1, 0)), &int_mode)
	      && GET_MODE_BITSIZE (int_mode) >= INTVAL (XEXP (op0, 1)))
	    op1 = XEXP (op1, 0);

          if (CONST_INT_P (op1))
            {
              /* MOV immediate is assumed to always be cheap.  */
              *cost = COSTS_N_INSNS (1);
            }
          else
            {
              /* BFM.  */
	      if (speed)
		*cost += extra_cost->alu.bfi;
	      *cost += rtx_cost (op1, VOIDmode, (enum rtx_code) code, 1, speed);
            }

	  return true;

	default:
	  /* We can't make sense of this, assume default cost.  */
          *cost = COSTS_N_INSNS (1);
	  return false;
	}
      return false;

    case CONST_INT:
      /* If an instruction can incorporate a constant within the
	 instruction, the instruction's expression avoids calling
	 rtx_cost() on the constant.  If rtx_cost() is called on a
	 constant, then it is usually because the constant must be
	 moved into a register by one or more instructions.

	 The exception is constant 0, which can be expressed
	 as XZR/WZR and is therefore free.  The exception to this is
	 if we have (set (reg) (const0_rtx)) in which case we must cost
	 the move.  However, we can catch that when we cost the SET, so
	 we don't need to consider that here.  */
      if (x == const0_rtx)
	*cost = 0;
      else
	{
	  /* To an approximation, building any other constant is
	     proportionally expensive to the number of instructions
	     required to build that constant.  This is true whether we
	     are compiling for SPEED or otherwise.  */
	  if (!is_a <scalar_int_mode> (mode, &int_mode))
	    int_mode = word_mode;
	  *cost = COSTS_N_INSNS (aarch64_internal_mov_immediate
				 (NULL_RTX, x, false, int_mode));
	}
      return true;

    case CONST_DOUBLE:

      /* First determine number of instructions to do the move
	  as an integer constant.  */
      if (!aarch64_float_const_representable_p (x)
	   && !aarch64_can_const_movi_rtx_p (x, mode)
	   && aarch64_float_const_rtx_p (x))
	{
	  unsigned HOST_WIDE_INT ival;
	  bool succeed = aarch64_reinterpret_float_as_int (x, &ival);
	  gcc_assert (succeed);

	  scalar_int_mode imode = (mode == HFmode
				   ? SImode
				   : int_mode_for_mode (mode).require ());
	  int ncost = aarch64_internal_mov_immediate
		(NULL_RTX, gen_int_mode (ival, imode), false, imode);
	  *cost += COSTS_N_INSNS (ncost);
	  return true;
	}

      if (speed)
	{
	  /* mov[df,sf]_aarch64.  */
	  if (aarch64_float_const_representable_p (x))
	    /* FMOV (scalar immediate).  */
	    *cost += extra_cost->fp[mode == DFmode].fpconst;
	  else if (!aarch64_float_const_zero_rtx_p (x))
	    {
	      /* This will be a load from memory.  */
	      if (mode == DFmode)
		*cost += extra_cost->ldst.loadd;
	      else
		*cost += extra_cost->ldst.loadf;
	    }
	  else
	    /* Otherwise this is +0.0.  We get this using MOVI d0, #0
	       or MOV v0.s[0], wzr - neither of which are modeled by the
	       cost tables.  Just use the default cost.  */
	    {
	    }
	}

      return true;

    case MEM:
      if (speed)
	{
	  /* For loads we want the base cost of a load, plus an
	     approximation for the additional cost of the addressing
	     mode.  */
	  rtx address = XEXP (x, 0);
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->ldst.loadv;
	  else if (GET_MODE_CLASS (mode) == MODE_INT)
	    *cost += extra_cost->ldst.load;
	  else if (mode == SFmode)
	    *cost += extra_cost->ldst.loadf;
	  else if (mode == DFmode)
	    *cost += extra_cost->ldst.loadd;

	  *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	}

      return true;

    case NEG:
      op0 = XEXP (x, 0);

      if (VECTOR_MODE_P (mode))
	{
	  if (speed)
	    {
	      /* FNEG.  */
	      *cost += extra_cost->vect.alu;
	    }
	  return false;
	}

      if (GET_MODE_CLASS (mode) == MODE_INT)
	{
          if (GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMPARE
              || GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMM_COMPARE)
            {
              /* CSETM.  */
	      *cost += rtx_cost (XEXP (op0, 0), VOIDmode, NEG, 0, speed);
              return true;
            }

	  /* Cost this as SUB wzr, X.  */
          op0 = CONST0_RTX (mode);
          op1 = XEXP (x, 0);
          goto cost_minus;
        }

      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
        {
          /* Support (neg(fma...)) as a single instruction only if
             sign of zeros is unimportant.  This matches the decision
             making in aarch64.md.  */
          if (GET_CODE (op0) == FMA && !HONOR_SIGNED_ZEROS (GET_MODE (op0)))
            {
	      /* FNMADD.  */
	      *cost = rtx_cost (op0, mode, NEG, 0, speed);
              return true;
            }
	  if (GET_CODE (op0) == MULT)
	    {
	      /* FNMUL.  */
	      *cost = rtx_cost (op0, mode, NEG, 0, speed);
	      return true;
	    }
	  if (speed)
	    /* FNEG.  */
	    *cost += extra_cost->fp[mode == DFmode].neg;
          return false;
        }

      return false;

    case CLRSB:
    case CLZ:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.clz;
	}

      return false;

    case COMPARE:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (op1 == const0_rtx
	  && GET_CODE (op0) == AND)
	{
	  x = op0;
	  mode = GET_MODE (op0);
	  goto cost_logic;
	}

      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
        {
          /* TODO: A write to the CC flags possibly costs extra, this
	     needs encoding in the cost tables.  */

	  mode = GET_MODE (op0);
          /* ANDS.  */
          if (GET_CODE (op0) == AND)
            {
              x = op0;
              goto cost_logic;
            }

          if (GET_CODE (op0) == PLUS)
            {
	      /* ADDS (and CMN alias).  */
              x = op0;
              goto cost_plus;
            }

          if (GET_CODE (op0) == MINUS)
            {
	      /* SUBS.  */
              x = op0;
              goto cost_minus;
            }

	  if (GET_CODE (op0) == ZERO_EXTRACT && op1 == const0_rtx
	      && GET_MODE (x) == CC_NZmode && CONST_INT_P (XEXP (op0, 1))
	      && CONST_INT_P (XEXP (op0, 2)))
	    {
	      /* COMPARE of ZERO_EXTRACT form of TST-immediate.
		 Handle it here directly rather than going to cost_logic
		 since we know the immediate generated for the TST is valid
		 so we can avoid creating an intermediate rtx for it only
		 for costing purposes.  */
	      if (speed)
		*cost += extra_cost->alu.logical;

	      *cost += rtx_cost (XEXP (op0, 0), GET_MODE (op0),
				 ZERO_EXTRACT, 0, speed);
	      return true;
	    }

          if (GET_CODE (op1) == NEG)
            {
	      /* CMN.  */
	      if (speed)
		*cost += extra_cost->alu.arith;

	      *cost += rtx_cost (op0, mode, COMPARE, 0, speed);
	      *cost += rtx_cost (XEXP (op1, 0), mode, NEG, 1, speed);
              return true;
            }

          /* CMP.

	     Compare can freely swap the order of operands, and
             canonicalization puts the more complex operation first.
             But the integer MINUS logic expects the shift/extend
             operation in op1.  */
          if (! (REG_P (op0)
                 || (GET_CODE (op0) == SUBREG && REG_P (SUBREG_REG (op0)))))
          {
            op0 = XEXP (x, 1);
            op1 = XEXP (x, 0);
          }
          goto cost_minus;
        }

      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
        {
	  /* FCMP.  */
	  if (speed)
	    *cost += extra_cost->fp[mode == DFmode].compare;

          if (CONST_DOUBLE_P (op1) && aarch64_float_const_zero_rtx_p (op1))
            {
	      *cost += rtx_cost (op0, VOIDmode, COMPARE, 0, speed);
              /* FCMP supports constant 0.0 for no extra cost. */
              return true;
            }
          return false;
        }

      if (VECTOR_MODE_P (mode))
	{
	  /* Vector compare.  */
	  if (speed)
	    *cost += extra_cost->vect.alu;

	  if (aarch64_float_const_zero_rtx_p (op1))
	    {
	      /* Vector cm (eq|ge|gt|lt|le) supports constant 0.0 for no extra
		 cost.  */
	      return true;
	    }
	  return false;
	}
      return false;

    case MINUS:
      {
	op0 = XEXP (x, 0);
	op1 = XEXP (x, 1);

cost_minus:
	*cost += rtx_cost (op0, mode, MINUS, 0, speed);

	/* Detect valid immediates.  */
	if ((GET_MODE_CLASS (mode) == MODE_INT
	     || (GET_MODE_CLASS (mode) == MODE_CC
		 && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT))
	    && CONST_INT_P (op1)
	    && aarch64_uimm12_shift (INTVAL (op1)))
	  {
	    if (speed)
	      /* SUB(S) (immediate).  */
	      *cost += extra_cost->alu.arith;
	    return true;
	  }

	/* Look for SUB (extended register).  */
	if (is_a <scalar_int_mode> (mode, &int_mode)
	    && aarch64_rtx_arith_op_extract_p (op1, int_mode))
	  {
	    if (speed)
	      *cost += extra_cost->alu.extend_arith;

	    op1 = aarch64_strip_extend (op1, true);
	    *cost += rtx_cost (op1, VOIDmode,
			       (enum rtx_code) GET_CODE (op1), 0, speed);
	    return true;
	  }

	rtx new_op1 = aarch64_strip_extend (op1, false);

	/* Cost this as an FMA-alike operation.  */
	if ((GET_CODE (new_op1) == MULT
	     || aarch64_shift_p (GET_CODE (new_op1)))
	    && code != COMPARE)
	  {
	    *cost += aarch64_rtx_mult_cost (new_op1, MULT,
					    (enum rtx_code) code,
					    speed);
	    return true;
	  }

	*cost += rtx_cost (new_op1, VOIDmode, MINUS, 1, speed);

	if (speed)
	  {
	    if (VECTOR_MODE_P (mode))
	      {
		/* Vector SUB.  */
		*cost += extra_cost->vect.alu;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_INT)
	      {
		/* SUB(S).  */
		*cost += extra_cost->alu.arith;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	      {
		/* FSUB.  */
		*cost += extra_cost->fp[mode == DFmode].addsub;
	      }
	  }
	return true;
      }

    case PLUS:
      {
	rtx new_op0;

	op0 = XEXP (x, 0);
	op1 = XEXP (x, 1);

cost_plus:
	if (GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMPARE
	    || GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMM_COMPARE)
	  {
	    /* CSINC.  */
	    *cost += rtx_cost (XEXP (op0, 0), mode, PLUS, 0, speed);
	    *cost += rtx_cost (op1, mode, PLUS, 1, speed);
	    return true;
	  }

	if (GET_MODE_CLASS (mode) == MODE_INT
	    && (aarch64_plus_immediate (op1, mode)
		|| aarch64_sve_addvl_addpl_immediate (op1, mode)))
	  {
	    *cost += rtx_cost (op0, mode, PLUS, 0, speed);

	    if (speed)
	      /* ADD (immediate).  */
	      *cost += extra_cost->alu.arith;
	    return true;
	  }

	*cost += rtx_cost (op1, mode, PLUS, 1, speed);

	/* Look for ADD (extended register).  */
	if (is_a <scalar_int_mode> (mode, &int_mode)
	    && aarch64_rtx_arith_op_extract_p (op0, int_mode))
	  {
	    if (speed)
	      *cost += extra_cost->alu.extend_arith;

	    op0 = aarch64_strip_extend (op0, true);
	    *cost += rtx_cost (op0, VOIDmode,
			       (enum rtx_code) GET_CODE (op0), 0, speed);
	    return true;
	  }

	/* Strip any extend, leave shifts behind as we will
	   cost them through mult_cost.  */
	new_op0 = aarch64_strip_extend (op0, false);

	if (GET_CODE (new_op0) == MULT
	    || aarch64_shift_p (GET_CODE (new_op0)))
	  {
	    *cost += aarch64_rtx_mult_cost (new_op0, MULT, PLUS,
					    speed);
	    return true;
	  }

	*cost += rtx_cost (new_op0, VOIDmode, PLUS, 0, speed);

	if (speed)
	  {
	    if (VECTOR_MODE_P (mode))
	      {
		/* Vector ADD.  */
		*cost += extra_cost->vect.alu;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_INT)
	      {
		/* ADD.  */
		*cost += extra_cost->alu.arith;
	      }
	    else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	      {
		/* FADD.  */
		*cost += extra_cost->fp[mode == DFmode].addsub;
	      }
	  }
	return true;
      }

    case BSWAP:
      *cost = COSTS_N_INSNS (1);

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.rev;
	}
      return false;

    case IOR:
      if (aarch_rev16_p (x))
        {
          *cost = COSTS_N_INSNS (1);

	  if (speed)
	    {
	      if (VECTOR_MODE_P (mode))
		*cost += extra_cost->vect.alu;
	      else
		*cost += extra_cost->alu.rev;
	    }
	  return true;
        }

      if (aarch64_extr_rtx_p (x, &op0, &op1))
        {
	  *cost += rtx_cost (op0, mode, IOR, 0, speed);
	  *cost += rtx_cost (op1, mode, IOR, 1, speed);
          if (speed)
            *cost += extra_cost->alu.shift;

          return true;
        }
    /* Fall through.  */
    case XOR:
    case AND:
    cost_logic:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (VECTOR_MODE_P (mode))
	{
	  if (speed)
	    *cost += extra_cost->vect.alu;
	  return true;
	}

      if (code == AND
          && GET_CODE (op0) == MULT
          && CONST_INT_P (XEXP (op0, 1))
          && CONST_INT_P (op1)
          && aarch64_uxt_size (exact_log2 (INTVAL (XEXP (op0, 1))),
                               INTVAL (op1)) != 0)
        {
          /* This is a UBFM/SBFM.  */
	  *cost += rtx_cost (XEXP (op0, 0), mode, ZERO_EXTRACT, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
          return true;
        }

      if (is_int_mode (mode, &int_mode))
	{
	  if (CONST_INT_P (op1))
	    {
	      /* We have a mask + shift version of a UBFIZ
		 i.e. the *andim_ashift<mode>_bfiz pattern.  */
	      if (GET_CODE (op0) == ASHIFT
		  && aarch64_mask_and_shift_for_ubfiz_p (int_mode, op1,
							 XEXP (op0, 1)))
		{
		  *cost += rtx_cost (XEXP (op0, 0), int_mode,
				     (enum rtx_code) code, 0, speed);
		  if (speed)
		    *cost += extra_cost->alu.bfx;

		  return true;
		}
	      else if (aarch64_bitmask_imm (INTVAL (op1), int_mode))
		{
		/* We possibly get the immediate for free, this is not
		   modelled.  */
		  *cost += rtx_cost (op0, int_mode,
				     (enum rtx_code) code, 0, speed);
		  if (speed)
		    *cost += extra_cost->alu.logical;

		  return true;
		}
	    }
	  else
	    {
	      rtx new_op0 = op0;

	      /* Handle ORN, EON, or BIC.  */
	      if (GET_CODE (op0) == NOT)
		op0 = XEXP (op0, 0);

	      new_op0 = aarch64_strip_shift (op0);

	      /* If we had a shift on op0 then this is a logical-shift-
		 by-register/immediate operation.  Otherwise, this is just
		 a logical operation.  */
	      if (speed)
		{
		  if (new_op0 != op0)
		    {
		      /* Shift by immediate.  */
		      if (CONST_INT_P (XEXP (op0, 1)))
			*cost += extra_cost->alu.log_shift;
		      else
			*cost += extra_cost->alu.log_shift_reg;
		    }
		  else
		    *cost += extra_cost->alu.logical;
		}

	      /* In both cases we want to cost both operands.  */
	      *cost += rtx_cost (new_op0, int_mode, (enum rtx_code) code,
				 0, speed);
	      *cost += rtx_cost (op1, int_mode, (enum rtx_code) code,
				 1, speed);

	      return true;
	    }
	}
      return false;

    case NOT:
      x = XEXP (x, 0);
      op0 = aarch64_strip_shift (x);

      if (VECTOR_MODE_P (mode))
	{
	  /* Vector NOT.  */
	  *cost += extra_cost->vect.alu;
	  return false;
	}

      /* MVN-shifted-reg.  */
      if (op0 != x)
        {
	  *cost += rtx_cost (op0, mode, (enum rtx_code) code, 0, speed);

          if (speed)
            *cost += extra_cost->alu.log_shift;

          return true;
        }
      /* EON can have two forms: (xor (not a) b) but also (not (xor a b)).
         Handle the second form here taking care that 'a' in the above can
         be a shift.  */
      else if (GET_CODE (op0) == XOR)
        {
          rtx newop0 = XEXP (op0, 0);
          rtx newop1 = XEXP (op0, 1);
          rtx op0_stripped = aarch64_strip_shift (newop0);

	  *cost += rtx_cost (newop1, mode, (enum rtx_code) code, 1, speed);
	  *cost += rtx_cost (op0_stripped, mode, XOR, 0, speed);

          if (speed)
            {
              if (op0_stripped != newop0)
                *cost += extra_cost->alu.log_shift;
              else
                *cost += extra_cost->alu.logical;
            }

          return true;
        }
      /* MVN.  */
      if (speed)
	*cost += extra_cost->alu.logical;

      return false;

    case ZERO_EXTEND:

      op0 = XEXP (x, 0);
      /* If a value is written in SI mode, then zero extended to DI
	 mode, the operation will in general be free as a write to
	 a 'w' register implicitly zeroes the upper bits of an 'x'
	 register.  However, if this is

	   (set (reg) (zero_extend (reg)))

	 we must cost the explicit register move.  */
      if (mode == DImode
	  && GET_MODE (op0) == SImode
	  && outer == SET)
	{
	  int op_cost = rtx_cost (op0, VOIDmode, ZERO_EXTEND, 0, speed);

	/* If OP_COST is non-zero, then the cost of the zero extend
	   is effectively the cost of the inner operation.  Otherwise
	   we have a MOV instruction and we take the cost from the MOV
	   itself.  This is true independently of whether we are
	   optimizing for space or time.  */
	  if (op_cost)
	    *cost = op_cost;

	  return true;
	}
      else if (MEM_P (op0))
	{
	  /* All loads can zero extend to any size for free.  */
	  *cost = rtx_cost (op0, VOIDmode, ZERO_EXTEND, param, speed);
	  return true;
	}

      op0 = aarch64_extend_bitfield_pattern_p (x);
      if (op0)
	{
	  *cost += rtx_cost (op0, mode, ZERO_EXTEND, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
	  return true;
	}

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      /* UMOV.  */
	      *cost += extra_cost->vect.alu;
	    }
	  else
	    {
	      /* We generate an AND instead of UXTB/UXTH.  */
	      *cost += extra_cost->alu.logical;
	    }
	}
      return false;

    case SIGN_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	{
	  /* LDRSH.  */
	  if (speed)
	    {
	      rtx address = XEXP (XEXP (x, 0), 0);
	      *cost += extra_cost->ldst.load_sign_extend;

	      *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	    }
	  return true;
	}

      op0 = aarch64_extend_bitfield_pattern_p (x);
      if (op0)
	{
	  *cost += rtx_cost (op0, mode, SIGN_EXTEND, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
	  return true;
	}

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.extend;
	}
      return false;

    case ASHIFT:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (CONST_INT_P (op1))
        {
	  if (speed)
	    {
	      if (VECTOR_MODE_P (mode))
		{
		  /* Vector shift (immediate).  */
		  *cost += extra_cost->vect.alu;
		}
	      else
		{
		  /* LSL (immediate), UBMF, UBFIZ and friends.  These are all
		     aliases.  */
		  *cost += extra_cost->alu.shift;
		}
	    }

          /* We can incorporate zero/sign extend for free.  */
          if (GET_CODE (op0) == ZERO_EXTEND
              || GET_CODE (op0) == SIGN_EXTEND)
            op0 = XEXP (op0, 0);

	  *cost += rtx_cost (op0, VOIDmode, ASHIFT, 0, speed);
          return true;
        }
      else
        {
	  if (VECTOR_MODE_P (mode))
	    {
	      if (speed)
		/* Vector shift (register).  */
		*cost += extra_cost->vect.alu;
	    }
	  else
	    {
	      if (speed)
		/* LSLV.  */
		*cost += extra_cost->alu.shift_reg;

	      if (GET_CODE (op1) == AND && REG_P (XEXP (op1, 0))
		  && CONST_INT_P (XEXP (op1, 1))
		  && known_eq (INTVAL (XEXP (op1, 1)),
			       GET_MODE_BITSIZE (mode) - 1))
		{
		  *cost += rtx_cost (op0, mode, (rtx_code) code, 0, speed);
		  /* We already demanded XEXP (op1, 0) to be REG_P, so
		     don't recurse into it.  */
		  return true;
		}
	    }
	  return false;  /* All arguments need to be in registers.  */
        }

    case ROTATE:
    case ROTATERT:
    case LSHIFTRT:
    case ASHIFTRT:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (CONST_INT_P (op1))
	{
	  /* ASR (immediate) and friends.  */
	  if (speed)
	    {
	      if (VECTOR_MODE_P (mode))
		*cost += extra_cost->vect.alu;
	      else
		*cost += extra_cost->alu.shift;
	    }

	  *cost += rtx_cost (op0, mode, (enum rtx_code) code, 0, speed);
	  return true;
	}
      else
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      if (speed)
		/* Vector shift (register).  */
		*cost += extra_cost->vect.alu;
	    }
	  else
	    {
	      if (speed)
		/* ASR (register) and friends.  */
		*cost += extra_cost->alu.shift_reg;

	      if (GET_CODE (op1) == AND && REG_P (XEXP (op1, 0))
		  && CONST_INT_P (XEXP (op1, 1))
		  && known_eq (INTVAL (XEXP (op1, 1)),
			       GET_MODE_BITSIZE (mode) - 1))
		{
		  *cost += rtx_cost (op0, mode, (rtx_code) code, 0, speed);
		  /* We already demanded XEXP (op1, 0) to be REG_P, so
		     don't recurse into it.  */
		  return true;
		}
	    }
	  return false;  /* All arguments need to be in registers.  */
	}

    case SYMBOL_REF:

      if (aarch64_cmodel == AARCH64_CMODEL_LARGE
	  || aarch64_cmodel == AARCH64_CMODEL_SMALL_SPIC)
	{
	  /* LDR.  */
	  if (speed)
	    *cost += extra_cost->ldst.load;
	}
      else if (aarch64_cmodel == AARCH64_CMODEL_SMALL
	       || aarch64_cmodel == AARCH64_CMODEL_SMALL_PIC)
	{
	  /* ADRP, followed by ADD.  */
	  *cost += COSTS_N_INSNS (1);
	  if (speed)
	    *cost += 2 * extra_cost->alu.arith;
	}
      else if (aarch64_cmodel == AARCH64_CMODEL_TINY
	       || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)
	{
	  /* ADR.  */
	  if (speed)
	    *cost += extra_cost->alu.arith;
	}

      if (flag_pic)
	{
	  /* One extra load instruction, after accessing the GOT.  */
	  *cost += COSTS_N_INSNS (1);
	  if (speed)
	    *cost += extra_cost->ldst.load;
	}
      return true;

    case HIGH:
    case LO_SUM:
      /* ADRP/ADD (immediate).  */
      if (speed)
	*cost += extra_cost->alu.arith;
      return true;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      /* UBFX/SBFX.  */
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->alu.bfx;
	}

      /* We can trust that the immediates used will be correct (there
	 are no by-register forms), so we need only cost op0.  */
      *cost += rtx_cost (XEXP (x, 0), VOIDmode, (enum rtx_code) code, 0, speed);
      return true;

    case MULT:
      *cost += aarch64_rtx_mult_cost (x, MULT, 0, speed);
      /* aarch64_rtx_mult_cost always handles recursion to its
	 operands.  */
      return true;

    case MOD:
    /* We can expand signed mod by power of 2 using a NEGS, two parallel
       ANDs and a CSNEG.  Assume here that CSNEG is the same as the cost of
       an unconditional negate.  This case should only ever be reached through
       the set_smod_pow2_cheap check in expmed.c.  */
      if (CONST_INT_P (XEXP (x, 1))
	  && exact_log2 (INTVAL (XEXP (x, 1))) > 0
	  && (mode == SImode || mode == DImode))
	{
	  /* We expand to 4 instructions.  Reset the baseline.  */
	  *cost = COSTS_N_INSNS (4);

	  if (speed)
	    *cost += 2 * extra_cost->alu.logical
		     + 2 * extra_cost->alu.arith;

	  return true;
	}

    /* Fall-through.  */
    case UMOD:
      if (speed)
	{
	  /* Slighly prefer UMOD over SMOD.  */
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else if (GET_MODE_CLASS (mode) == MODE_INT)
	    *cost += (extra_cost->mult[mode == DImode].add
		      + extra_cost->mult[mode == DImode].idiv
		      + (code == MOD ? 1 : 0));
	}
      return false;  /* All arguments need to be in registers.  */

    case DIV:
    case UDIV:
    case SQRT:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else if (GET_MODE_CLASS (mode) == MODE_INT)
	    /* There is no integer SQRT, so only DIV and UDIV can get
	       here.  */
	    *cost += (extra_cost->mult[mode == DImode].idiv
		     /* Slighly prefer UDIV over SDIV.  */
		     + (code == DIV ? 1 : 0));
	  else
	    *cost += extra_cost->fp[mode == DFmode].div;
	}
      return false;  /* All arguments need to be in registers.  */

    case IF_THEN_ELSE:
      return aarch64_if_then_else_costs (XEXP (x, 0), XEXP (x, 1),
					 XEXP (x, 2), cost, speed);

    case EQ:
    case NE:
    case GT:
    case GTU:
    case LT:
    case LTU:
    case GE:
    case GEU:
    case LE:
    case LEU:

      return false; /* All arguments must be in registers.  */

    case FMA:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      op2 = XEXP (x, 2);

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->fp[mode == DFmode].fma;
	}

      /* FMSUB, FNMADD, and FNMSUB are free.  */
      if (GET_CODE (op0) == NEG)
        op0 = XEXP (op0, 0);

      if (GET_CODE (op2) == NEG)
        op2 = XEXP (op2, 0);

      /* aarch64_fnma4_elt_to_64v2df has the NEG as operand 1,
	 and the by-element operand as operand 0.  */
      if (GET_CODE (op1) == NEG)
        op1 = XEXP (op1, 0);

      /* Catch vector-by-element operations.  The by-element operand can
	 either be (vec_duplicate (vec_select (x))) or just
	 (vec_select (x)), depending on whether we are multiplying by
	 a vector or a scalar.

	 Canonicalization is not very good in these cases, FMA4 will put the
	 by-element operand as operand 0, FNMA4 will have it as operand 1.  */
      if (GET_CODE (op0) == VEC_DUPLICATE)
	op0 = XEXP (op0, 0);
      else if (GET_CODE (op1) == VEC_DUPLICATE)
	op1 = XEXP (op1, 0);

      if (GET_CODE (op0) == VEC_SELECT)
	op0 = XEXP (op0, 0);
      else if (GET_CODE (op1) == VEC_SELECT)
	op1 = XEXP (op1, 0);

      /* If the remaining parameters are not registers,
         get the cost to put them into registers.  */
      *cost += rtx_cost (op0, mode, FMA, 0, speed);
      *cost += rtx_cost (op1, mode, FMA, 1, speed);
      *cost += rtx_cost (op2, mode, FMA, 2, speed);
      return true;

    case FLOAT:
    case UNSIGNED_FLOAT:
      if (speed)
	*cost += extra_cost->fp[mode == DFmode].fromint;
      return false;

    case FLOAT_EXTEND:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      /*Vector truncate.  */
	      *cost += extra_cost->vect.alu;
	    }
	  else
	    *cost += extra_cost->fp[mode == DFmode].widen;
	}
      return false;

    case FLOAT_TRUNCATE:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    {
	      /*Vector conversion.  */
	      *cost += extra_cost->vect.alu;
	    }
	  else
	    *cost += extra_cost->fp[mode == DFmode].narrow;
	}
      return false;

    case FIX:
    case UNSIGNED_FIX:
      x = XEXP (x, 0);
      /* Strip the rounding part.  They will all be implemented
         by the fcvt* family of instructions anyway.  */
      if (GET_CODE (x) == UNSPEC)
        {
          unsigned int uns_code = XINT (x, 1);

          if (uns_code == UNSPEC_FRINTA
              || uns_code == UNSPEC_FRINTM
              || uns_code == UNSPEC_FRINTN
              || uns_code == UNSPEC_FRINTP
              || uns_code == UNSPEC_FRINTZ)
            x = XVECEXP (x, 0, 0);
        }

      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    *cost += extra_cost->fp[GET_MODE (x) == DFmode].toint;
	}

      /* We can combine fmul by a power of 2 followed by a fcvt into a single
	 fixed-point fcvt.  */
      if (GET_CODE (x) == MULT
	  && ((VECTOR_MODE_P (mode)
	       && aarch64_vec_fpconst_pow_of_2 (XEXP (x, 1)) > 0)
	      || aarch64_fpconst_pow_of_2 (XEXP (x, 1)) > 0))
	{
	  *cost += rtx_cost (XEXP (x, 0), VOIDmode, (rtx_code) code,
			     0, speed);
	  return true;
	}

      *cost += rtx_cost (x, VOIDmode, (enum rtx_code) code, 0, speed);
      return true;

    case ABS:
      if (VECTOR_MODE_P (mode))
	{
	  /* ABS (vector).  */
	  if (speed)
	    *cost += extra_cost->vect.alu;
	}
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  op0 = XEXP (x, 0);

	  /* FABD, which is analogous to FADD.  */
	  if (GET_CODE (op0) == MINUS)
	    {
	      *cost += rtx_cost (XEXP (op0, 0), mode, MINUS, 0, speed);
	      *cost += rtx_cost (XEXP (op0, 1), mode, MINUS, 1, speed);
	      if (speed)
		*cost += extra_cost->fp[mode == DFmode].addsub;

	      return true;
	    }
	  /* Simple FABS is analogous to FNEG.  */
	  if (speed)
	    *cost += extra_cost->fp[mode == DFmode].neg;
	}
      else
	{
	  /* Integer ABS will either be split to
	     two arithmetic instructions, or will be an ABS
	     (scalar), which we don't model.  */
	  *cost = COSTS_N_INSNS (2);
	  if (speed)
	    *cost += 2 * extra_cost->alu.arith;
	}
      return false;

    case SMAX:
    case SMIN:
      if (speed)
	{
	  if (VECTOR_MODE_P (mode))
	    *cost += extra_cost->vect.alu;
	  else
	    {
	      /* FMAXNM/FMINNM/FMAX/FMIN.
	         TODO: This may not be accurate for all implementations, but
	         we do not model this in the cost tables.  */
	      *cost += extra_cost->fp[mode == DFmode].addsub;
	    }
	}
      return false;

    case UNSPEC:
      /* The floating point round to integer frint* instructions.  */
      if (aarch64_frint_unspec_p (XINT (x, 1)))
        {
          if (speed)
            *cost += extra_cost->fp[mode == DFmode].roundint;

          return false;
        }

      if (XINT (x, 1) == UNSPEC_RBIT)
        {
          if (speed)
            *cost += extra_cost->alu.rev;

          return false;
        }
      break;

    case TRUNCATE:

      /* Decompose <su>muldi3_highpart.  */
      if (/* (truncate:DI  */
	  mode == DImode
	  /*   (lshiftrt:TI  */
          && GET_MODE (XEXP (x, 0)) == TImode
          && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  /*      (mult:TI  */
          && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  /*        (ANY_EXTEND:TI (reg:DI))
	            (ANY_EXTEND:TI (reg:DI)))  */
          && ((GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
               && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == ZERO_EXTEND)
              || (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND
                  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == SIGN_EXTEND))
          && GET_MODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0)) == DImode
          && GET_MODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 1), 0)) == DImode
	  /*     (const_int 64)  */
          && CONST_INT_P (XEXP (XEXP (x, 0), 1))
          && UINTVAL (XEXP (XEXP (x, 0), 1)) == 64)
        {
          /* UMULH/SMULH.  */
	  if (speed)
	    *cost += extra_cost->mult[mode == DImode].extend;
	  *cost += rtx_cost (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0),
			     mode, MULT, 0, speed);
	  *cost += rtx_cost (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 1), 0),
			     mode, MULT, 1, speed);
          return true;
        }

      /* Fall through.  */
    default:
      break;
    }

  if (dump_file
      && flag_aarch64_verbose_cost)
    fprintf (dump_file,
      "\nFailed to cost RTX.  Assuming default cost.\n");

  return true;
}

/* Wrapper around aarch64_rtx_costs, dumps the partial, or total cost
   calculated for X.  This cost is stored in *COST.  Returns true
   if the total cost of X was calculated.  */
static bool
aarch64_rtx_costs_wrapper (rtx x, machine_mode mode, int outer,
		   int param, int *cost, bool speed)
{
  bool result = aarch64_rtx_costs (x, mode, outer, param, cost, speed);

  if (dump_file
      && flag_aarch64_verbose_cost)
    {
      print_rtl_single (dump_file, x);
      fprintf (dump_file, "\n%s cost: %d (%s)\n",
	       speed ? "Hot" : "Cold",
	       *cost, result ? "final" : "partial");
    }

  return result;
}

static int
aarch64_register_move_cost (machine_mode mode,
			    reg_class_t from_i, reg_class_t to_i)
{
  enum reg_class from = (enum reg_class) from_i;
  enum reg_class to = (enum reg_class) to_i;
  const struct cpu_regmove_cost *regmove_cost
    = aarch64_tune_params.regmove_cost;

  /* Caller save and pointer regs are equivalent to GENERAL_REGS.  */
  if (to == TAILCALL_ADDR_REGS || to == POINTER_REGS)
    to = GENERAL_REGS;

  if (from == TAILCALL_ADDR_REGS || from == POINTER_REGS)
    from = GENERAL_REGS;

  /* Make RDFFR very expensive.  In particular, if we know that the FFR
     contains a PTRUE (e.g. after a SETFFR), we must never use RDFFR
     as a way of obtaining a PTRUE.  */
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
      && hard_reg_set_subset_p (reg_class_contents[from_i],
				reg_class_contents[FFR_REGS]))
    return 80;

  /* Moving between GPR and stack cost is the same as GP2GP.  */
  if ((from == GENERAL_REGS && to == STACK_REG)
      || (to == GENERAL_REGS && from == STACK_REG))
    return regmove_cost->GP2GP;

  /* To/From the stack register, we move via the gprs.  */
  if (to == STACK_REG || from == STACK_REG)
    return aarch64_register_move_cost (mode, from, GENERAL_REGS)
            + aarch64_register_move_cost (mode, GENERAL_REGS, to);

  if (known_eq (GET_MODE_SIZE (mode), 16))
    {
      /* 128-bit operations on general registers require 2 instructions.  */
      if (from == GENERAL_REGS && to == GENERAL_REGS)
	return regmove_cost->GP2GP * 2;
      else if (from == GENERAL_REGS)
	return regmove_cost->GP2FP * 2;
      else if (to == GENERAL_REGS)
	return regmove_cost->FP2GP * 2;

      /* When AdvSIMD instructions are disabled it is not possible to move
	 a 128-bit value directly between Q registers.  This is handled in
	 secondary reload.  A general register is used as a scratch to move
	 the upper DI value and the lower DI value is moved directly,
	 hence the cost is the sum of three moves. */
      if (! TARGET_SIMD)
	return regmove_cost->GP2FP + regmove_cost->FP2GP + regmove_cost->FP2FP;

      return regmove_cost->FP2FP;
    }

  if (from == GENERAL_REGS && to == GENERAL_REGS)
    return regmove_cost->GP2GP;
  else if (from == GENERAL_REGS)
    return regmove_cost->GP2FP;
  else if (to == GENERAL_REGS)
    return regmove_cost->FP2GP;

  return regmove_cost->FP2FP;
}

static int
aarch64_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			  reg_class_t rclass ATTRIBUTE_UNUSED,
			  bool in ATTRIBUTE_UNUSED)
{
  return aarch64_tune_params.memmov_cost;
}

/* Implement TARGET_INIT_BUILTINS.  */
static void
aarch64_init_builtins ()
{
  aarch64_general_init_builtins ();
  aarch64_sve::init_builtins ();
}

/* Implement TARGET_FOLD_BUILTIN.  */
static tree
aarch64_fold_builtin (tree fndecl, int nargs, tree *args, bool)
{
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_fold_builtin (subcode, type, nargs, args);

    case AARCH64_BUILTIN_SVE:
      return NULL_TREE;
    }
  gcc_unreachable ();
}

/* Implement TARGET_GIMPLE_FOLD_BUILTIN.  */
static bool
aarch64_gimple_fold_builtin (gimple_stmt_iterator *gsi)
{
  gcall *stmt = as_a <gcall *> (gsi_stmt (*gsi));
  tree fndecl = gimple_call_fndecl (stmt);
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  gimple *new_stmt = NULL;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      new_stmt = aarch64_general_gimple_fold_builtin (subcode, stmt);
      break;

    case AARCH64_BUILTIN_SVE:
      new_stmt = aarch64_sve::gimple_fold_builtin (subcode, gsi, stmt);
      break;
    }

  if (!new_stmt)
    return false;

  gsi_replace (gsi, new_stmt, true);
  return true;
}

/* Implement TARGET_EXPAND_BUILTIN.  */
static rtx
aarch64_expand_builtin (tree exp, rtx target, rtx, machine_mode, int ignore)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_expand_builtin (subcode, exp, target, ignore);

    case AARCH64_BUILTIN_SVE:
      return aarch64_sve::expand_builtin (subcode, exp, target);
    }
  gcc_unreachable ();
}

/* Implement TARGET_BUILTIN_DECL.  */
static tree
aarch64_builtin_decl (unsigned int code, bool initialize_p)
{
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_builtin_decl (subcode, initialize_p);

    case AARCH64_BUILTIN_SVE:
      return aarch64_sve::builtin_decl (subcode, initialize_p);
    }
  gcc_unreachable ();
}

/* Return true if it is safe and beneficial to use the approximate rsqrt optabs
   to optimize 1.0/sqrt.  */

static bool
use_rsqrt_p (machine_mode mode)
{
  return (!flag_trapping_math
	  && flag_unsafe_math_optimizations
	  && ((aarch64_tune_params.approx_modes->recip_sqrt
	       & AARCH64_APPROX_MODE (mode))
	      || flag_mrecip_low_precision_sqrt));
}

/* Function to decide when to use the approximate reciprocal square root
   builtin.  */

static tree
aarch64_builtin_reciprocal (tree fndecl)
{
  machine_mode mode = TYPE_MODE (TREE_TYPE (fndecl));

  if (!use_rsqrt_p (mode))
    return NULL_TREE;
  unsigned int code = DECL_MD_FUNCTION_CODE (fndecl);
  unsigned int subcode = code >> AARCH64_BUILTIN_SHIFT;
  switch (code & AARCH64_BUILTIN_CLASS)
    {
    case AARCH64_BUILTIN_GENERAL:
      return aarch64_general_builtin_rsqrt (subcode);

    case AARCH64_BUILTIN_SVE:
      return NULL_TREE;
    }
  gcc_unreachable ();
}

/* Emit instruction sequence to compute either the approximate square root
   or its approximate reciprocal, depending on the flag RECP, and return
   whether the sequence was emitted or not.  */

bool
aarch64_emit_approx_sqrt (rtx dst, rtx src, bool recp)
{
  machine_mode mode = GET_MODE (dst);

  if (GET_MODE_INNER (mode) == HFmode)
    {
      gcc_assert (!recp);
      return false;
    }

  if (!recp)
    {
      if (!(flag_mlow_precision_sqrt
	    || (aarch64_tune_params.approx_modes->sqrt
		& AARCH64_APPROX_MODE (mode))))
	return false;

      if (flag_finite_math_only
	  || flag_trapping_math
	  || !flag_unsafe_math_optimizations
	  || optimize_function_for_size_p (cfun))
	return false;
    }
  else
    /* Caller assumes we cannot fail.  */
    gcc_assert (use_rsqrt_p (mode));

  machine_mode mmsk = (VECTOR_MODE_P (mode)
		       ? related_int_vector_mode (mode).require ()
		       : int_mode_for_mode (mode).require ());
  rtx xmsk = gen_reg_rtx (mmsk);
  if (!recp)
    /* When calculating the approximate square root, compare the
       argument with 0.0 and create a mask.  */
    emit_insn (gen_rtx_SET (xmsk,
			    gen_rtx_NEG (mmsk,
					 gen_rtx_EQ (mmsk, src,
						     CONST0_RTX (mode)))));

  /* Estimate the approximate reciprocal square root.  */
  rtx xdst = gen_reg_rtx (mode);
  emit_insn (gen_aarch64_rsqrte (mode, xdst, src));

  /* Iterate over the series twice for SF and thrice for DF.  */
  int iterations = (GET_MODE_INNER (mode) == DFmode) ? 3 : 2;

  /* Optionally iterate over the series once less for faster performance
     while sacrificing the accuracy.  */
  if ((recp && flag_mrecip_low_precision_sqrt)
      || (!recp && flag_mlow_precision_sqrt))
    iterations--;

  /* Iterate over the series to calculate the approximate reciprocal square
     root.  */
  rtx x1 = gen_reg_rtx (mode);
  while (iterations--)
    {
      rtx x2 = gen_reg_rtx (mode);
      emit_set_insn (x2, gen_rtx_MULT (mode, xdst, xdst));

      emit_insn (gen_aarch64_rsqrts (mode, x1, src, x2));

      if (iterations > 0)
	emit_set_insn (xdst, gen_rtx_MULT (mode, xdst, x1));
    }

  if (!recp)
    {
      /* Qualify the approximate reciprocal square root when the argument is
	 0.0 by squashing the intermediary result to 0.0.  */
      rtx xtmp = gen_reg_rtx (mmsk);
      emit_set_insn (xtmp, gen_rtx_AND (mmsk, gen_rtx_NOT (mmsk, xmsk),
					      gen_rtx_SUBREG (mmsk, xdst, 0)));
      emit_move_insn (xdst, gen_rtx_SUBREG (mode, xtmp, 0));

      /* Calculate the approximate square root.  */
      emit_set_insn (xdst, gen_rtx_MULT (mode, xdst, src));
    }

  /* Finalize the approximation.  */
  emit_set_insn (dst, gen_rtx_MULT (mode, xdst, x1));

  return true;
}

/* Emit the instruction sequence to compute the approximation for the division
   of NUM by DEN in QUO and return whether the sequence was emitted or not.  */

bool
aarch64_emit_approx_div (rtx quo, rtx num, rtx den)
{
  machine_mode mode = GET_MODE (quo);

  if (GET_MODE_INNER (mode) == HFmode)
    return false;

  bool use_approx_division_p = (flag_mlow_precision_div
			        || (aarch64_tune_params.approx_modes->division
				    & AARCH64_APPROX_MODE (mode)));

  if (!flag_finite_math_only
      || flag_trapping_math
      || !flag_unsafe_math_optimizations
      || optimize_function_for_size_p (cfun)
      || !use_approx_division_p)
    return false;

  if (!TARGET_SIMD && VECTOR_MODE_P (mode))
    return false;

  /* Estimate the approximate reciprocal.  */
  rtx xrcp = gen_reg_rtx (mode);
  emit_insn (gen_aarch64_frecpe (mode, xrcp, den));

  /* Iterate over the series twice for SF and thrice for DF.  */
  int iterations = (GET_MODE_INNER (mode) == DFmode) ? 3 : 2;

  /* Optionally iterate over the series once less for faster performance,
     while sacrificing the accuracy.  */
  if (flag_mlow_precision_div)
    iterations--;

  /* Iterate over the series to calculate the approximate reciprocal.  */
  rtx xtmp = gen_reg_rtx (mode);
  while (iterations--)
    {
      emit_insn (gen_aarch64_frecps (mode, xtmp, xrcp, den));

      if (iterations > 0)
	emit_set_insn (xrcp, gen_rtx_MULT (mode, xrcp, xtmp));
    }

  if (num != CONST1_RTX (mode))
    {
      /* As the approximate reciprocal of DEN is already calculated, only
	 calculate the approximate division when NUM is not 1.0.  */
      rtx xnum = force_reg (mode, num);
      emit_set_insn (xrcp, gen_rtx_MULT (mode, xrcp, xnum));
    }

  /* Finalize the approximation.  */
  emit_set_insn (quo, gen_rtx_MULT (mode, xrcp, xtmp));
  return true;
}

/* Return the number of instructions that can be issued per cycle.  */
static int
aarch64_sched_issue_rate (void)
{
  return aarch64_tune_params.issue_rate;
}

/* Implement TARGET_SCHED_VARIABLE_ISSUE.  */
static int
aarch64_sched_variable_issue (FILE *, int, rtx_insn *insn, int more)
{
  if (DEBUG_INSN_P (insn))
    return more;

  rtx_code code = GET_CODE (PATTERN (insn));
  if (code == USE || code == CLOBBER)
    return more;

  if (get_attr_type (insn) == TYPE_NO_INSN)
    return more;

  return more - 1;
}

static int
aarch64_sched_first_cycle_multipass_dfa_lookahead (void)
{
  int issue_rate = aarch64_sched_issue_rate ();

  return issue_rate > 1 && !sched_fusion ? issue_rate : 0;
}


/* Implement TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD as
   autopref_multipass_dfa_lookahead_guard from haifa-sched.c.  It only
   has an effect if PARAM_SCHED_AUTOPREF_QUEUE_DEPTH > 0.  */

static int
aarch64_first_cycle_multipass_dfa_lookahead_guard (rtx_insn *insn,
						    int ready_index)
{
  return autopref_multipass_dfa_lookahead_guard (insn, ready_index);
}


/* Vectorizer cost model target hooks.  */

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
aarch64_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				    tree vectype,
				    int misalign ATTRIBUTE_UNUSED)
{
  unsigned elements;
  const cpu_vector_cost *costs = aarch64_tune_params.vec_costs;
  bool fp = false;

  if (vectype != NULL)
    fp = FLOAT_TYPE_P (vectype);

  switch (type_of_cost)
    {
      case scalar_stmt:
	return fp ? costs->scalar_fp_stmt_cost : costs->scalar_int_stmt_cost;

      case scalar_load:
	return costs->scalar_load_cost;

      case scalar_store:
	return costs->scalar_store_cost;

      case vector_stmt:
	return fp ? costs->vec_fp_stmt_cost : costs->vec_int_stmt_cost;

      case vector_load:
	return costs->vec_align_load_cost;

      case vector_store:
	return costs->vec_store_cost;

      case vec_to_scalar:
	return costs->vec_to_scalar_cost;

      case scalar_to_vec:
	return costs->scalar_to_vec_cost;

      case unaligned_load:
      case vector_gather_load:
	return costs->vec_unalign_load_cost;

      case unaligned_store:
      case vector_scatter_store:
	return costs->vec_unalign_store_cost;

      case cond_branch_taken:
	return costs->cond_taken_branch_cost;

      case cond_branch_not_taken:
	return costs->cond_not_taken_branch_cost;

      case vec_perm:
	return costs->vec_permute_cost;

      case vec_promote_demote:
	return fp ? costs->vec_fp_stmt_cost : costs->vec_int_stmt_cost;

      case vec_construct:
	elements = estimated_poly_value (TYPE_VECTOR_SUBPARTS (vectype));
	return elements / 2 + 1;

      default:
	gcc_unreachable ();
    }
}

/* Return true if STMT_INFO extends the result of a load.  */
static bool
aarch64_extending_load_p (stmt_vec_info stmt_info)
{
  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (!assign || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (assign)))
    return false;

  tree rhs = gimple_assign_rhs1 (stmt_info->stmt);
  tree lhs_type = TREE_TYPE (gimple_assign_lhs (assign));
  tree rhs_type = TREE_TYPE (rhs);
  if (!INTEGRAL_TYPE_P (lhs_type)
      || !INTEGRAL_TYPE_P (rhs_type)
      || TYPE_PRECISION (lhs_type) <= TYPE_PRECISION (rhs_type))
    return false;

  stmt_vec_info def_stmt_info = stmt_info->vinfo->lookup_def (rhs);
  return (def_stmt_info
	  && STMT_VINFO_DATA_REF (def_stmt_info)
	  && DR_IS_READ (STMT_VINFO_DATA_REF (def_stmt_info)));
}

/* Return true if STMT_INFO is an integer truncation.  */
static bool
aarch64_integer_truncation_p (stmt_vec_info stmt_info)
{
  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (!assign || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (assign)))
    return false;

  tree lhs_type = TREE_TYPE (gimple_assign_lhs (assign));
  tree rhs_type = TREE_TYPE (gimple_assign_rhs1 (assign));
  return (INTEGRAL_TYPE_P (lhs_type)
	  && INTEGRAL_TYPE_P (rhs_type)
	  && TYPE_PRECISION (lhs_type) < TYPE_PRECISION (rhs_type));
}

/* STMT_COST is the cost calculated by aarch64_builtin_vectorization_cost
   for STMT_INFO, which has cost kind KIND.  Adjust the cost as necessary
   for SVE targets.  */
static unsigned int
aarch64_sve_adjust_stmt_cost (vect_cost_for_stmt kind, stmt_vec_info stmt_info,
			      unsigned int stmt_cost)
{
  /* Unlike vec_promote_demote, vector_stmt conversions do not change the
     vector register size or number of units.  Integer promotions of this
     type therefore map to SXT[BHW] or UXT[BHW].

     Most loads have extending forms that can do the sign or zero extension
     on the fly.  Optimistically assume that a load followed by an extension
     will fold to this form during combine, and that the extension therefore
     comes for free.  */
  if (kind == vector_stmt && aarch64_extending_load_p (stmt_info))
    stmt_cost = 0;

  /* For similar reasons, vector_stmt integer truncations are a no-op,
     because we can just ignore the unused upper bits of the source.  */
  if (kind == vector_stmt && aarch64_integer_truncation_p (stmt_info))
    stmt_cost = 0;

  return stmt_cost;
}

/* Implement targetm.vectorize.add_stmt_cost.  */
static unsigned
aarch64_add_stmt_cost (void *data, int count, enum vect_cost_for_stmt kind,
		       struct _stmt_vec_info *stmt_info, int misalign,
		       enum vect_cost_model_location where)
{
  unsigned *cost = (unsigned *) data;
  unsigned retval = 0;

  if (flag_vect_cost_model)
    {
      tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
      int stmt_cost =
	    aarch64_builtin_vectorization_cost (kind, vectype, misalign);

      if (stmt_info && vectype && aarch64_sve_mode_p (TYPE_MODE (vectype)))
	stmt_cost = aarch64_sve_adjust_stmt_cost (kind, stmt_info, stmt_cost);

      /* Statements in an inner loop relative to the loop being
	 vectorized are weighted more heavily.  The value here is
	 arbitrary and could potentially be improved with analysis.  */
      if (where == vect_body && stmt_info && stmt_in_inner_loop_p (stmt_info))
	count *= 50; /*  FIXME  */

      retval = (unsigned) (count * stmt_cost);
      cost[where] += retval;
    }

  return retval;
}

static void initialize_aarch64_code_model (struct gcc_options *);

/* Parse the TO_PARSE string and put the architecture struct that it
   selects into RES and the architectural features into ISA_FLAGS.
   Return an aarch64_parse_opt_result describing the parse result.
   If there is an error parsing, RES and ISA_FLAGS are left unchanged.
   When the TO_PARSE string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

static enum aarch64_parse_opt_result
aarch64_parse_arch (const char *to_parse, const struct processor **res,
		    uint64_t *isa_flags, std::string *invalid_extension)
{
  const char *ext;
  const struct processor *arch;
  size_t len;

  ext = strchr (to_parse, '+');

  if (ext != NULL)
    len = ext - to_parse;
  else
    len = strlen (to_parse);

  if (len == 0)
    return AARCH64_PARSE_MISSING_ARG;


  /* Loop through the list of supported ARCHes to find a match.  */
  for (arch = all_architectures; arch->name != NULL; arch++)
    {
      if (strlen (arch->name) == len
	  && strncmp (arch->name, to_parse, len) == 0)
	{
	  uint64_t isa_temp = arch->flags;

	  if (ext != NULL)
	    {
	      /* TO_PARSE string contains at least one extension.  */
	      enum aarch64_parse_opt_result ext_res
		= aarch64_parse_extension (ext, &isa_temp, invalid_extension);

	      if (ext_res != AARCH64_PARSE_OK)
		return ext_res;
	    }
	  /* Extension parsing was successful.  Confirm the result
	     arch and ISA flags.  */
	  *res = arch;
	  *isa_flags = isa_temp;
	  return AARCH64_PARSE_OK;
	}
    }

  /* ARCH name not found in list.  */
  return AARCH64_PARSE_INVALID_ARG;
}

/* Parse the TO_PARSE string and put the result tuning in RES and the
   architecture flags in ISA_FLAGS.  Return an aarch64_parse_opt_result
   describing the parse result.  If there is an error parsing, RES and
   ISA_FLAGS are left unchanged.
   When the TO_PARSE string contains an invalid extension,
   a copy of the string is created and stored to INVALID_EXTENSION.  */

static enum aarch64_parse_opt_result
aarch64_parse_cpu (const char *to_parse, const struct processor **res,
		   uint64_t *isa_flags, std::string *invalid_extension)
{
  const char *ext;
  const struct processor *cpu;
  size_t len;

  ext = strchr (to_parse, '+');

  if (ext != NULL)
    len = ext - to_parse;
  else
    len = strlen (to_parse);

  if (len == 0)
    return AARCH64_PARSE_MISSING_ARG;


  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strlen (cpu->name) == len && strncmp (cpu->name, to_parse, len) == 0)
	{
	  uint64_t isa_temp = cpu->flags;


	  if (ext != NULL)
	    {
	      /* TO_PARSE string contains at least one extension.  */
	      enum aarch64_parse_opt_result ext_res
		= aarch64_parse_extension (ext, &isa_temp, invalid_extension);

	      if (ext_res != AARCH64_PARSE_OK)
		return ext_res;
	    }
	  /* Extension parsing was successfull.  Confirm the result
	     cpu and ISA flags.  */
	  *res = cpu;
	  *isa_flags = isa_temp;
	  return AARCH64_PARSE_OK;
	}
    }

  /* CPU name not found in list.  */
  return AARCH64_PARSE_INVALID_ARG;
}

/* Parse the TO_PARSE string and put the cpu it selects into RES.
   Return an aarch64_parse_opt_result describing the parse result.
   If the parsing fails the RES does not change.  */

static enum aarch64_parse_opt_result
aarch64_parse_tune (const char *to_parse, const struct processor **res)
{
  const struct processor *cpu;

  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strcmp (cpu->name, to_parse) == 0)
	{
	  *res = cpu;
	  return AARCH64_PARSE_OK;
	}
    }

  /* CPU name not found in list.  */
  return AARCH64_PARSE_INVALID_ARG;
}

/* Parse TOKEN, which has length LENGTH to see if it is an option
   described in FLAG.  If it is, return the index bit for that fusion type.
   If not, error (printing OPTION_NAME) and return zero.  */

static unsigned int
aarch64_parse_one_option_token (const char *token,
				size_t length,
				const struct aarch64_flag_desc *flag,
				const char *option_name)
{
  for (; flag->name != NULL; flag++)
    {
      if (length == strlen (flag->name)
	  && !strncmp (flag->name, token, length))
	return flag->flag;
    }

  error ("unknown flag passed in %<-moverride=%s%> (%s)", option_name, token);
  return 0;
}

/* Parse OPTION which is a comma-separated list of flags to enable.
   FLAGS gives the list of flags we understand, INITIAL_STATE gives any
   default state we inherit from the CPU tuning structures.  OPTION_NAME
   gives the top-level option we are parsing in the -moverride string,
   for use in error messages.  */

static unsigned int
aarch64_parse_boolean_options (const char *option,
			       const struct aarch64_flag_desc *flags,
			       unsigned int initial_state,
			       const char *option_name)
{
  const char separator = '.';
  const char* specs = option;
  const char* ntoken = option;
  unsigned int found_flags = initial_state;

  while ((ntoken = strchr (specs, separator)))
    {
      size_t token_length = ntoken - specs;
      unsigned token_ops = aarch64_parse_one_option_token (specs,
							   token_length,
							   flags,
							   option_name);
      /* If we find "none" (or, for simplicity's sake, an error) anywhere
	 in the token stream, reset the supported operations.  So:

	   adrp+add.cmp+branch.none.adrp+add

	   would have the result of turning on only adrp+add fusion.  */
      if (!token_ops)
	found_flags = 0;

      found_flags |= token_ops;
      specs = ++ntoken;
    }

  /* We ended with a comma, print something.  */
  if (!(*specs))
    {
      error ("%s string ill-formed\n", option_name);
      return 0;
    }

  /* We still have one more token to parse.  */
  size_t token_length = strlen (specs);
  unsigned token_ops = aarch64_parse_one_option_token (specs,
						       token_length,
						       flags,
						       option_name);
   if (!token_ops)
     found_flags = 0;

  found_flags |= token_ops;
  return found_flags;
}

/* Support for overriding instruction fusion.  */

static void
aarch64_parse_fuse_string (const char *fuse_string,
			    struct tune_params *tune)
{
  tune->fusible_ops = aarch64_parse_boolean_options (fuse_string,
						     aarch64_fusible_pairs,
						     tune->fusible_ops,
						     "fuse=");
}

/* Support for overriding other tuning flags.  */

static void
aarch64_parse_tune_string (const char *tune_string,
			    struct tune_params *tune)
{
  tune->extra_tuning_flags
    = aarch64_parse_boolean_options (tune_string,
				     aarch64_tuning_flags,
				     tune->extra_tuning_flags,
				     "tune=");
}

/* Parse the sve_width tuning moverride string in TUNE_STRING.
   Accept the valid SVE vector widths allowed by
   aarch64_sve_vector_bits_enum and use it to override sve_width
   in TUNE.  */

static void
aarch64_parse_sve_width_string (const char *tune_string,
				struct tune_params *tune)
{
  int width = -1;

  int n = sscanf (tune_string, "%d", &width);
  if (n == EOF)
    {
      error ("invalid format for sve_width");
      return;
    }
  switch (width)
    {
    case SVE_128:
    case SVE_256:
    case SVE_512:
    case SVE_1024:
    case SVE_2048:
      break;
    default:
      error ("invalid sve_width value: %d", width);
    }
  tune->sve_width = (enum aarch64_sve_vector_bits_enum) width;
}

/* Parse TOKEN, which has length LENGTH to see if it is a tuning option
   we understand.  If it is, extract the option string and handoff to
   the appropriate function.  */

void
aarch64_parse_one_override_token (const char* token,
				  size_t length,
				  struct tune_params *tune)
{
  const struct aarch64_tuning_override_function *fn
    = aarch64_tuning_override_functions;

  const char *option_part = strchr (token, '=');
  if (!option_part)
    {
      error ("tuning string missing in option (%s)", token);
      return;
    }

  /* Get the length of the option name.  */
  length = option_part - token;
  /* Skip the '=' to get to the option string.  */
  option_part++;

  for (; fn->name != NULL; fn++)
    {
      if (!strncmp (fn->name, token, length))
	{
	  fn->parse_override (option_part, tune);
	  return;
	}
    }

  error ("unknown tuning option (%s)",token);
  return;
}

/* A checking mechanism for the implementation of the tls size.  */

static void
initialize_aarch64_tls_size (struct gcc_options *opts)
{
  if (aarch64_tls_size == 0)
    aarch64_tls_size = 24;

  switch (opts->x_aarch64_cmodel_var)
    {
    case AARCH64_CMODEL_TINY:
      /* Both the default and maximum TLS size allowed under tiny is 1M which
	 needs two instructions to address, so we clamp the size to 24.  */
      if (aarch64_tls_size > 24)
	aarch64_tls_size = 24;
      break;
    case AARCH64_CMODEL_SMALL:
      /* The maximum TLS size allowed under small is 4G.  */
      if (aarch64_tls_size > 32)
	aarch64_tls_size = 32;
      break;
    case AARCH64_CMODEL_LARGE:
      /* The maximum TLS size allowed under large is 16E.
	 FIXME: 16E should be 64bit, we only support 48bit offset now.  */
      if (aarch64_tls_size > 48)
	aarch64_tls_size = 48;
      break;
    default:
      gcc_unreachable ();
    }

  return;
}

/* Parse STRING looking for options in the format:
     string	:: option:string
     option	:: name=substring
     name	:: {a-z}
     substring	:: defined by option.  */

static void
aarch64_parse_override_string (const char* input_string,
			       struct tune_params* tune)
{
  const char separator = ':';
  size_t string_length = strlen (input_string) + 1;
  char *string_root = (char *) xmalloc (sizeof (*string_root) * string_length);
  char *string = string_root;
  strncpy (string, input_string, string_length);
  string[string_length - 1] = '\0';

  char* ntoken = string;

  while ((ntoken = strchr (string, separator)))
    {
      size_t token_length = ntoken - string;
      /* Make this substring look like a string.  */
      *ntoken = '\0';
      aarch64_parse_one_override_token (string, token_length, tune);
      string = ++ntoken;
    }

  /* One last option to parse.  */
  aarch64_parse_one_override_token (string, strlen (string), tune);
  free (string_root);
}


static void
aarch64_override_options_after_change_1 (struct gcc_options *opts)
{
  if (accepted_branch_protection_string)
    {
      opts->x_aarch64_branch_protection_string
	= xstrdup (accepted_branch_protection_string);
    }

  /* PR 70044: We have to be careful about being called multiple times for the
     same function.  This means all changes should be repeatable.  */

  /* Set aarch64_use_frame_pointer based on -fno-omit-frame-pointer.
     Disable the frame pointer flag so the mid-end will not use a frame
     pointer in leaf functions in order to support -fomit-leaf-frame-pointer.
     Set x_flag_omit_frame_pointer to the special value 2 to differentiate
     between -fomit-frame-pointer (1) and -fno-omit-frame-pointer (2).  */
  aarch64_use_frame_pointer = opts->x_flag_omit_frame_pointer != 1;
  if (opts->x_flag_omit_frame_pointer == 0)
    opts->x_flag_omit_frame_pointer = 2;

  /* If not optimizing for size, set the default
     alignment to what the target wants.  */
  if (!opts->x_optimize_size)
    {
      if (opts->x_flag_align_loops && !opts->x_str_align_loops)
	opts->x_str_align_loops = aarch64_tune_params.loop_align;
      if (opts->x_flag_align_jumps && !opts->x_str_align_jumps)
	opts->x_str_align_jumps = aarch64_tune_params.jump_align;
      if (opts->x_flag_align_functions && !opts->x_str_align_functions)
	opts->x_str_align_functions = aarch64_tune_params.function_align;
    }

  /* We default to no pc-relative literal loads.  */

  aarch64_pcrelative_literal_loads = false;

  /* If -mpc-relative-literal-loads is set on the command line, this
     implies that the user asked for PC relative literal loads.  */
  if (opts->x_pcrelative_literal_loads == 1)
    aarch64_pcrelative_literal_loads = true;

  /* In the tiny memory model it makes no sense to disallow PC relative
     literal pool loads.  */
  if (aarch64_cmodel == AARCH64_CMODEL_TINY
      || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)
    aarch64_pcrelative_literal_loads = true;

  /* When enabling the lower precision Newton series for the square root, also
     enable it for the reciprocal square root, since the latter is an
     intermediary step for the former.  */
  if (flag_mlow_precision_sqrt)
    flag_mrecip_low_precision_sqrt = true;
}

/* 'Unpack' up the internal tuning structs and update the options
    in OPTS.  The caller must have set up selected_tune and selected_arch
    as all the other target-specific codegen decisions are
    derived from them.  */

void
aarch64_override_options_internal (struct gcc_options *opts)
{
  aarch64_tune_flags = selected_tune->flags;
  aarch64_tune = selected_tune->sched_core;
  /* Make a copy of the tuning parameters attached to the core, which
     we may later overwrite.  */
  aarch64_tune_params = *(selected_tune->tune);
  aarch64_architecture_version = selected_arch->architecture_version;

  if (opts->x_aarch64_override_tune_string)
    aarch64_parse_override_string (opts->x_aarch64_override_tune_string,
				  &aarch64_tune_params);

  /* This target defaults to strict volatile bitfields.  */
  if (opts->x_flag_strict_volatile_bitfields < 0 && abi_version_at_least (2))
    opts->x_flag_strict_volatile_bitfields = 1;

  if (aarch64_stack_protector_guard == SSP_GLOBAL
      && opts->x_aarch64_stack_protector_guard_offset_str)
    {
      error ("incompatible options %<-mstack-protector-guard=global%> and "
	     "%<-mstack-protector-guard-offset=%s%>",
	     aarch64_stack_protector_guard_offset_str);
    }

  if (aarch64_stack_protector_guard == SSP_SYSREG
      && !(opts->x_aarch64_stack_protector_guard_offset_str
	   && opts->x_aarch64_stack_protector_guard_reg_str))
    {
      error ("both %<-mstack-protector-guard-offset%> and "
	     "%<-mstack-protector-guard-reg%> must be used "
	     "with %<-mstack-protector-guard=sysreg%>");
    }

  if (opts->x_aarch64_stack_protector_guard_reg_str)
    {
      if (strlen (opts->x_aarch64_stack_protector_guard_reg_str) > 100)
	  error ("specify a system register with a small string length.");
    }

  if (opts->x_aarch64_stack_protector_guard_offset_str)
    {
      char *end;
      const char *str = aarch64_stack_protector_guard_offset_str;
      errno = 0;
      long offs = strtol (aarch64_stack_protector_guard_offset_str, &end, 0);
      if (!*str || *end || errno)
	error ("%qs is not a valid offset in %qs", str,
	       "-mstack-protector-guard-offset=");
      aarch64_stack_protector_guard_offset = offs;
    }

  initialize_aarch64_code_model (opts);
  initialize_aarch64_tls_size (opts);

  int queue_depth = 0;
  switch (aarch64_tune_params.autoprefetcher_model)
    {
      case tune_params::AUTOPREFETCHER_OFF:
	queue_depth = -1;
	break;
      case tune_params::AUTOPREFETCHER_WEAK:
	queue_depth = 0;
	break;
      case tune_params::AUTOPREFETCHER_STRONG:
	queue_depth = max_insn_queue_index + 1;
	break;
      default:
	gcc_unreachable ();
    }

  /* We don't mind passing in global_options_set here as we don't use
     the *options_set structs anyway.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_sched_autopref_queue_depth, queue_depth);

  /* Set up parameters to be used in prefetching algorithm.  Do not
     override the defaults unless we are tuning for a core we have
     researched values for.  */
  if (aarch64_tune_params.prefetch->num_slots > 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_simultaneous_prefetches,
			 aarch64_tune_params.prefetch->num_slots);
  if (aarch64_tune_params.prefetch->l1_cache_size >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_l1_cache_size,
			 aarch64_tune_params.prefetch->l1_cache_size);
  if (aarch64_tune_params.prefetch->l1_cache_line_size >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_l1_cache_line_size,
			 aarch64_tune_params.prefetch->l1_cache_line_size);
  if (aarch64_tune_params.prefetch->l2_cache_size >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_l2_cache_size,
			 aarch64_tune_params.prefetch->l2_cache_size);
  if (!aarch64_tune_params.prefetch->prefetch_dynamic_strides)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_prefetch_dynamic_strides, 0);
  if (aarch64_tune_params.prefetch->minimum_stride >= 0)
    SET_OPTION_IF_UNSET (opts, &global_options_set,
			 param_prefetch_minimum_stride,
			 aarch64_tune_params.prefetch->minimum_stride);

  /* Use the alternative scheduling-pressure algorithm by default.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_sched_pressure_algorithm,
		       SCHED_PRESSURE_MODEL);

  /* Validate the guard size.  */
  int guard_size = param_stack_clash_protection_guard_size;

  if (guard_size != 12 && guard_size != 16)
    error ("only values 12 (4 KB) and 16 (64 KB) are supported for guard "
	   "size.  Given value %d (%llu KB) is out of range",
	   guard_size, (1ULL << guard_size) / 1024ULL);

  /* Enforce that interval is the same size as size so the mid-end does the
     right thing.  */
  SET_OPTION_IF_UNSET (opts, &global_options_set,
		       param_stack_clash_protection_probe_interval,
		       guard_size);

  /* The maybe_set calls won't update the value if the user has explicitly set
     one.  Which means we need to validate that probing interval and guard size
     are equal.  */
  int probe_interval
    = param_stack_clash_protection_probe_interval;
  if (guard_size != probe_interval)
    error ("stack clash guard size %<%d%> must be equal to probing interval "
	   "%<%d%>", guard_size, probe_interval);

  /* Enable sw prefetching at specified optimization level for
     CPUS that have prefetch.  Lower optimization level threshold by 1
     when profiling is enabled.  */
  if (opts->x_flag_prefetch_loop_arrays < 0
      && !opts->x_optimize_size
      && aarch64_tune_params.prefetch->default_opt_level >= 0
      && opts->x_optimize >= aarch64_tune_params.prefetch->default_opt_level)
    opts->x_flag_prefetch_loop_arrays = 1;

  if (opts->x_aarch64_arch_string == NULL)
    opts->x_aarch64_arch_string = selected_arch->name;
  if (opts->x_aarch64_cpu_string == NULL)
    opts->x_aarch64_cpu_string = selected_cpu->name;
  if (opts->x_aarch64_tune_string == NULL)
    opts->x_aarch64_tune_string = selected_tune->name;

  aarch64_override_options_after_change_1 (opts);
}

/* Print a hint with a suggestion for a core or architecture name that
   most closely resembles what the user passed in STR.  ARCH is true if
   the user is asking for an architecture name.  ARCH is false if the user
   is asking for a core name.  */

static void
aarch64_print_hint_for_core_or_arch (const char *str, bool arch)
{
  auto_vec<const char *> candidates;
  const struct processor *entry = arch ? all_architectures : all_cores;
  for (; entry->name != NULL; entry++)
    candidates.safe_push (entry->name);

#ifdef HAVE_LOCAL_CPU_DETECT
  /* Add also "native" as possible value.  */
  if (arch)
    candidates.safe_push ("native");
#endif

  char *s;
  const char *hint = candidates_list_and_hint (str, s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s;"
			     " did you mean %qs?", s, hint);
  else
    inform (input_location, "valid arguments are: %s", s);

  XDELETEVEC (s);
}

/* Print a hint with a suggestion for a core name that most closely resembles
   what the user passed in STR.  */

inline static void
aarch64_print_hint_for_core (const char *str)
{
  aarch64_print_hint_for_core_or_arch (str, false);
}

/* Print a hint with a suggestion for an architecture name that most closely
   resembles what the user passed in STR.  */

inline static void
aarch64_print_hint_for_arch (const char *str)
{
  aarch64_print_hint_for_core_or_arch (str, true);
}


/* Print a hint with a suggestion for an extension name
   that most closely resembles what the user passed in STR.  */

void
aarch64_print_hint_for_extensions (const std::string &str)
{
  auto_vec<const char *> candidates;
  aarch64_get_all_extension_candidates (&candidates);
  char *s;
  const char *hint = candidates_list_and_hint (str.c_str (), s, candidates);
  if (hint)
    inform (input_location, "valid arguments are: %s;"
			     " did you mean %qs?", s, hint);
  else
    inform (input_location, "valid arguments are: %s;", s);

  XDELETEVEC (s);
}

/* Validate a command-line -mcpu option.  Parse the cpu and extensions (if any)
   specified in STR and throw errors if appropriate.  Put the results if
   they are valid in RES and ISA_FLAGS.  Return whether the option is
   valid.  */

static bool
aarch64_validate_mcpu (const char *str, const struct processor **res,
		       uint64_t *isa_flags)
{
  std::string invalid_extension;
  enum aarch64_parse_opt_result parse_res
    = aarch64_parse_cpu (str, res, isa_flags, &invalid_extension);

  if (parse_res == AARCH64_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH64_PARSE_MISSING_ARG:
	error ("missing cpu name in %<-mcpu=%s%>", str);
	break;
      case AARCH64_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-mcpu%>", str);
	aarch64_print_hint_for_core (str);
	break;
      case AARCH64_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs in %<-mcpu=%s%>",
	       invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Parses CONST_STR for branch protection features specified in
   aarch64_branch_protect_types, and set any global variables required.  Returns
   the parsing result and assigns LAST_STR to the last processed token from
   CONST_STR so that it can be used for error reporting.  */

static enum
aarch64_parse_opt_result aarch64_parse_branch_protection (const char *const_str,
							  char** last_str)
{
  char *str_root = xstrdup (const_str);
  char* token_save = NULL;
  char *str = strtok_r (str_root, "+", &token_save);
  enum aarch64_parse_opt_result res = AARCH64_PARSE_OK;
  if (!str)
    res = AARCH64_PARSE_MISSING_ARG;
  else
    {
      char *next_str = strtok_r (NULL, "+", &token_save);
      /* Reset the branch protection features to their defaults.  */
      aarch64_handle_no_branch_protection (NULL, NULL);

      while (str && res == AARCH64_PARSE_OK)
	{
	  const aarch64_branch_protect_type* type = aarch64_branch_protect_types;
	  bool found = false;
	  /* Search for this type.  */
	  while (type && type->name && !found && res == AARCH64_PARSE_OK)
	    {
	      if (strcmp (str, type->name) == 0)
		{
		  found = true;
		  res = type->handler (str, next_str);
		  str = next_str;
		  next_str = strtok_r (NULL, "+", &token_save);
		}
	      else
		type++;
	    }
	  if (found && res == AARCH64_PARSE_OK)
	    {
	      bool found_subtype = true;
	      /* Loop through each token until we find one that isn't a
		 subtype.  */
	      while (found_subtype)
		{
		  found_subtype = false;
		  const aarch64_branch_protect_type *subtype = type->subtypes;
		  /* Search for the subtype.  */
		  while (str && subtype && subtype->name && !found_subtype
			  && res == AARCH64_PARSE_OK)
		    {
		      if (strcmp (str, subtype->name) == 0)
			{
			  found_subtype = true;
			  res = subtype->handler (str, next_str);
			  str = next_str;
			  next_str = strtok_r (NULL, "+", &token_save);
			}
		      else
			subtype++;
		    }
		}
	    }
	  else if (!found)
	    res = AARCH64_PARSE_INVALID_ARG;
	}
    }
  /* Copy the last processed token into the argument to pass it back.
    Used by option and attribute validation to print the offending token.  */
  if (last_str)
    {
      if (str) strcpy (*last_str, str);
      else *last_str = NULL;
    }
  if (res == AARCH64_PARSE_OK)
    {
      /* If needed, alloc the accepted string then copy in const_str.
	Used by override_option_after_change_1.  */
      if (!accepted_branch_protection_string)
	accepted_branch_protection_string = (char *) xmalloc (
						      BRANCH_PROTECT_STR_MAX
							+ 1);
      strncpy (accepted_branch_protection_string, const_str,
		BRANCH_PROTECT_STR_MAX + 1);
      /* Forcibly null-terminate.  */
      accepted_branch_protection_string[BRANCH_PROTECT_STR_MAX] = '\0';
    }
  return res;
}

static bool
aarch64_validate_mbranch_protection (const char *const_str)
{
  char *str = (char *) xmalloc (strlen (const_str));
  enum aarch64_parse_opt_result res =
    aarch64_parse_branch_protection (const_str, &str);
  if (res == AARCH64_PARSE_INVALID_ARG)
    error ("invalid argument %<%s%> for %<-mbranch-protection=%>", str);
  else if (res == AARCH64_PARSE_MISSING_ARG)
    error ("missing argument for %<-mbranch-protection=%>");
  free (str);
  return res == AARCH64_PARSE_OK;
}

/* Validate a command-line -march option.  Parse the arch and extensions
   (if any) specified in STR and throw errors if appropriate.  Put the
   results, if they are valid, in RES and ISA_FLAGS.  Return whether the
   option is valid.  */

static bool
aarch64_validate_march (const char *str, const struct processor **res,
			 uint64_t *isa_flags)
{
  std::string invalid_extension;
  enum aarch64_parse_opt_result parse_res
    = aarch64_parse_arch (str, res, isa_flags, &invalid_extension);

  if (parse_res == AARCH64_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH64_PARSE_MISSING_ARG:
	error ("missing arch name in %<-march=%s%>", str);
	break;
      case AARCH64_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-march%>", str);
	aarch64_print_hint_for_arch (str);
	break;
      case AARCH64_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %qs in %<-march=%s%>",
	       invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Validate a command-line -mtune option.  Parse the cpu
   specified in STR and throw errors if appropriate.  Put the
   result, if it is valid, in RES.  Return whether the option is
   valid.  */

static bool
aarch64_validate_mtune (const char *str, const struct processor **res)
{
  enum aarch64_parse_opt_result parse_res
    = aarch64_parse_tune (str, res);

  if (parse_res == AARCH64_PARSE_OK)
    return true;

  switch (parse_res)
    {
      case AARCH64_PARSE_MISSING_ARG:
	error ("missing cpu name in %<-mtune=%s%>", str);
	break;
      case AARCH64_PARSE_INVALID_ARG:
	error ("unknown value %qs for %<-mtune%>", str);
	aarch64_print_hint_for_core (str);
	break;
      default:
	gcc_unreachable ();
    }
  return false;
}

/* Return the CPU corresponding to the enum CPU.
   If it doesn't specify a cpu, return the default.  */

static const struct processor *
aarch64_get_tune_cpu (enum aarch64_processor cpu)
{
  if (cpu != aarch64_none)
    return &all_cores[cpu];

  /* The & 0x3f is to extract the bottom 6 bits that encode the
     default cpu as selected by the --with-cpu GCC configure option
     in config.gcc.
     ???: The whole TARGET_CPU_DEFAULT and AARCH64_CPU_DEFAULT_FLAGS
     flags mechanism should be reworked to make it more sane.  */
  return &all_cores[TARGET_CPU_DEFAULT & 0x3f];
}

/* Return the architecture corresponding to the enum ARCH.
   If it doesn't specify a valid architecture, return the default.  */

static const struct processor *
aarch64_get_arch (enum aarch64_arch arch)
{
  if (arch != aarch64_no_arch)
    return &all_architectures[arch];

  const struct processor *cpu = &all_cores[TARGET_CPU_DEFAULT & 0x3f];

  return &all_architectures[cpu->arch];
}

/* Return the VG value associated with -msve-vector-bits= value VALUE.  */

static poly_uint16
aarch64_convert_sve_vector_bits (aarch64_sve_vector_bits_enum value)
{
  /* For now generate vector-length agnostic code for -msve-vector-bits=128.
     This ensures we can clearly distinguish SVE and Advanced SIMD modes when
     deciding which .md file patterns to use and when deciding whether
     something is a legitimate address or constant.  */
  if (value == SVE_SCALABLE || value == SVE_128)
    return poly_uint16 (2, 2);
  else
    return (int) value / 64;
}

/* Implement TARGET_OPTION_OVERRIDE.  This is called once in the beginning
   and is used to parse the -m{cpu,tune,arch} strings and setup the initial
   tuning structs.  In particular it must set selected_tune and
   aarch64_isa_flags that define the available ISA features and tuning
   decisions.  It must also set selected_arch as this will be used to
   output the .arch asm tags for each function.  */

static void
aarch64_override_options (void)
{
  uint64_t cpu_isa = 0;
  uint64_t arch_isa = 0;
  aarch64_isa_flags = 0;

  bool valid_cpu = true;
  bool valid_tune = true;
  bool valid_arch = true;

  selected_cpu = NULL;
  selected_arch = NULL;
  selected_tune = NULL;

  if (aarch64_branch_protection_string)
    aarch64_validate_mbranch_protection (aarch64_branch_protection_string);

  /* -mcpu=CPU is shorthand for -march=ARCH_FOR_CPU, -mtune=CPU.
     If either of -march or -mtune is given, they override their
     respective component of -mcpu.  */
  if (aarch64_cpu_string)
    valid_cpu = aarch64_validate_mcpu (aarch64_cpu_string, &selected_cpu,
					&cpu_isa);

  if (aarch64_arch_string)
    valid_arch = aarch64_validate_march (aarch64_arch_string, &selected_arch,
					  &arch_isa);

  if (aarch64_tune_string)
    valid_tune = aarch64_validate_mtune (aarch64_tune_string, &selected_tune);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* If the user did not specify a processor, choose the default
     one for them.  This will be the CPU set during configuration using
     --with-cpu, otherwise it is "generic".  */
  if (!selected_cpu)
    {
      if (selected_arch)
	{
	  selected_cpu = &all_cores[selected_arch->ident];
	  aarch64_isa_flags = arch_isa;
	  explicit_arch = selected_arch->arch;
	}
      else
	{
	  /* Get default configure-time CPU.  */
	  selected_cpu = aarch64_get_tune_cpu (aarch64_none);
	  aarch64_isa_flags = TARGET_CPU_DEFAULT >> 6;
	}

      if (selected_tune)
	explicit_tune_core = selected_tune->ident;
    }
  /* If both -mcpu and -march are specified check that they are architecturally
     compatible, warn if they're not and prefer the -march ISA flags.  */
  else if (selected_arch)
    {
      if (selected_arch->arch != selected_cpu->arch)
	{
	  warning (0, "switch %<-mcpu=%s%> conflicts with %<-march=%s%> switch",
		       all_architectures[selected_cpu->arch].name,
		       selected_arch->name);
	}
      aarch64_isa_flags = arch_isa;
      explicit_arch = selected_arch->arch;
      explicit_tune_core = selected_tune ? selected_tune->ident
					  : selected_cpu->ident;
    }
  else
    {
      /* -mcpu but no -march.  */
      aarch64_isa_flags = cpu_isa;
      explicit_tune_core = selected_tune ? selected_tune->ident
					  : selected_cpu->ident;
      gcc_assert (selected_cpu);
      selected_arch = &all_architectures[selected_cpu->arch];
      explicit_arch = selected_arch->arch;
    }

  /* Set the arch as well as we will need it when outputing
     the .arch directive in assembly.  */
  if (!selected_arch)
    {
      gcc_assert (selected_cpu);
      selected_arch = &all_architectures[selected_cpu->arch];
    }

  if (!selected_tune)
    selected_tune = selected_cpu;

  if (aarch64_enable_bti == 2)
    {
#ifdef TARGET_ENABLE_BTI
      aarch64_enable_bti = 1;
#else
      aarch64_enable_bti = 0;
#endif
    }

  /* Return address signing is currently not supported for ILP32 targets.  For
     LP64 targets use the configured option in the absence of a command-line
     option for -mbranch-protection.  */
  if (!TARGET_ILP32 && accepted_branch_protection_string == NULL)
    {
#ifdef TARGET_ENABLE_PAC_RET
      aarch64_ra_sign_scope = AARCH64_FUNCTION_NON_LEAF;
#else
      aarch64_ra_sign_scope = AARCH64_FUNCTION_NONE;
#endif
    }

#ifndef HAVE_AS_MABI_OPTION
  /* The compiler may have been configured with 2.23.* binutils, which does
     not have support for ILP32.  */
  if (TARGET_ILP32)
    error ("assembler does not support %<-mabi=ilp32%>");
#endif

  /* Convert -msve-vector-bits to a VG count.  */
  aarch64_sve_vg = aarch64_convert_sve_vector_bits (aarch64_sve_vector_bits);

  if (aarch64_ra_sign_scope != AARCH64_FUNCTION_NONE && TARGET_ILP32)
    sorry ("return address signing is only supported for %<-mabi=lp64%>");

  /* Make sure we properly set up the explicit options.  */
  if ((aarch64_cpu_string && valid_cpu)
       || (aarch64_tune_string && valid_tune))
    gcc_assert (explicit_tune_core != aarch64_none);

  if ((aarch64_cpu_string && valid_cpu)
       || (aarch64_arch_string && valid_arch))
    gcc_assert (explicit_arch != aarch64_no_arch);

  /* The pass to insert speculation tracking runs before
     shrink-wrapping and the latter does not know how to update the
     tracking status.  So disable it in this case.  */
  if (aarch64_track_speculation)
    flag_shrink_wrap = 0;

  aarch64_override_options_internal (&global_options);

  /* Save these options as the default ones in case we push and pop them later
     while processing functions with potential target attributes.  */
  target_option_default_node = target_option_current_node
      = build_target_option_node (&global_options);
}

/* Implement targetm.override_options_after_change.  */

static void
aarch64_override_options_after_change (void)
{
  aarch64_override_options_after_change_1 (&global_options);
}

static struct machine_function *
aarch64_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();
  return machine;
}

void
aarch64_init_expanders (void)
{
  init_machine_status = aarch64_init_machine_status;
}

/* A checking mechanism for the implementation of the various code models.  */
static void
initialize_aarch64_code_model (struct gcc_options *opts)
{
   if (opts->x_flag_pic)
     {
       switch (opts->x_aarch64_cmodel_var)
	 {
	 case AARCH64_CMODEL_TINY:
	   aarch64_cmodel = AARCH64_CMODEL_TINY_PIC;
	   break;
	 case AARCH64_CMODEL_SMALL:
#ifdef HAVE_AS_SMALL_PIC_RELOCS
	   aarch64_cmodel = (flag_pic == 2
			     ? AARCH64_CMODEL_SMALL_PIC
			     : AARCH64_CMODEL_SMALL_SPIC);
#else
	   aarch64_cmodel = AARCH64_CMODEL_SMALL_PIC;
#endif
	   break;
	 case AARCH64_CMODEL_LARGE:
	   sorry ("code model %qs with %<-f%s%>", "large",
		  opts->x_flag_pic > 1 ? "PIC" : "pic");
	   break;
	 default:
	   gcc_unreachable ();
	 }
     }
   else
     aarch64_cmodel = opts->x_aarch64_cmodel_var;
}

/* Implement TARGET_OPTION_SAVE.  */

static void
aarch64_option_save (struct cl_target_option *ptr, struct gcc_options *opts)
{
  ptr->x_aarch64_override_tune_string = opts->x_aarch64_override_tune_string;
  ptr->x_aarch64_branch_protection_string
    = opts->x_aarch64_branch_protection_string;
}

/* Implements TARGET_OPTION_RESTORE.  Restore the backend codegen decisions
   using the information saved in PTR.  */

static void
aarch64_option_restore (struct gcc_options *opts, struct cl_target_option *ptr)
{
  opts->x_explicit_tune_core = ptr->x_explicit_tune_core;
  selected_tune = aarch64_get_tune_cpu (ptr->x_explicit_tune_core);
  opts->x_explicit_arch = ptr->x_explicit_arch;
  selected_arch = aarch64_get_arch (ptr->x_explicit_arch);
  opts->x_aarch64_override_tune_string = ptr->x_aarch64_override_tune_string;
  opts->x_aarch64_branch_protection_string
    = ptr->x_aarch64_branch_protection_string;
  if (opts->x_aarch64_branch_protection_string)
    {
      aarch64_parse_branch_protection (opts->x_aarch64_branch_protection_string,
					NULL);
    }

  aarch64_override_options_internal (opts);
}

/* Implement TARGET_OPTION_PRINT.  */

static void
aarch64_option_print (FILE *file, int indent, struct cl_target_option *ptr)
{
  const struct processor *cpu
    = aarch64_get_tune_cpu (ptr->x_explicit_tune_core);
  uint64_t isa_flags = ptr->x_aarch64_isa_flags;
  const struct processor *arch = aarch64_get_arch (ptr->x_explicit_arch);
  std::string extension
    = aarch64_get_extension_string_for_isa_flags (isa_flags, arch->flags);

  fprintf (file, "%*sselected tune = %s\n", indent, "", cpu->name);
  fprintf (file, "%*sselected arch = %s%s\n", indent, "",
	   arch->name, extension.c_str ());
}

static GTY(()) tree aarch64_previous_fndecl;

void
aarch64_reset_previous_fndecl (void)
{
  aarch64_previous_fndecl = NULL;
}

/* Restore or save the TREE_TARGET_GLOBALS from or to NEW_TREE.
   Used by aarch64_set_current_function and aarch64_pragma_target_parse to
   make sure optab availability predicates are recomputed when necessary.  */

void
aarch64_save_restore_target_globals (tree new_tree)
{
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
}

/* Implement TARGET_SET_CURRENT_FUNCTION.  Unpack the codegen decisions
   like tuning and ISA features from the DECL_FUNCTION_SPECIFIC_TARGET
   of the function, if such exists.  This function may be called multiple
   times on a single function so use aarch64_previous_fndecl to avoid
   setting up identical state.  */

static void
aarch64_set_current_function (tree fndecl)
{
  if (!fndecl || fndecl == aarch64_previous_fndecl)
    return;

  tree old_tree = (aarch64_previous_fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (aarch64_previous_fndecl)
		   : NULL_TREE);

  tree new_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  /* If current function has no attributes but the previous one did,
     use the default node.  */
  if (!new_tree && old_tree)
    new_tree = target_option_default_node;

  /* If nothing to do, return.  #pragma GCC reset or #pragma GCC pop to
     the default have been handled by aarch64_save_restore_target_globals from
     aarch64_pragma_target_parse.  */
  if (old_tree == new_tree)
    return;

  aarch64_previous_fndecl = fndecl;

  /* First set the target options.  */
  cl_target_option_restore (&global_options, TREE_TARGET_OPTION (new_tree));

  aarch64_save_restore_target_globals (new_tree);
}

/* Enum describing the various ways we can handle attributes.
   In many cases we can reuse the generic option handling machinery.  */

enum aarch64_attr_opt_type
{
  aarch64_attr_mask,	/* Attribute should set a bit in target_flags.  */
  aarch64_attr_bool,	/* Attribute sets or unsets a boolean variable.  */
  aarch64_attr_enum,	/* Attribute sets an enum variable.  */
  aarch64_attr_custom	/* Attribute requires a custom handling function.  */
};

/* All the information needed to handle a target attribute.
   NAME is the name of the attribute.
   ATTR_TYPE specifies the type of behavior of the attribute as described
   in the definition of enum aarch64_attr_opt_type.
   ALLOW_NEG is true if the attribute supports a "no-" form.
   HANDLER is the function that takes the attribute string as an argument
   It is needed only when the ATTR_TYPE is aarch64_attr_custom.
   OPT_NUM is the enum specifying the option that the attribute modifies.
   This is needed for attributes that mirror the behavior of a command-line
   option, that is it has ATTR_TYPE aarch64_attr_mask, aarch64_attr_bool or
   aarch64_attr_enum.  */

struct aarch64_attribute_info
{
  const char *name;
  enum aarch64_attr_opt_type attr_type;
  bool allow_neg;
  bool (*handler) (const char *);
  enum opt_code opt_num;
};

/* Handle the ARCH_STR argument to the arch= target attribute.  */

static bool
aarch64_handle_attr_arch (const char *str)
{
  const struct processor *tmp_arch = NULL;
  std::string invalid_extension;
  enum aarch64_parse_opt_result parse_res
    = aarch64_parse_arch (str, &tmp_arch, &aarch64_isa_flags, &invalid_extension);

  if (parse_res == AARCH64_PARSE_OK)
    {
      gcc_assert (tmp_arch);
      selected_arch = tmp_arch;
      explicit_arch = selected_arch->arch;
      return true;
    }

  switch (parse_res)
    {
      case AARCH64_PARSE_MISSING_ARG:
	error ("missing name in %<target(\"arch=\")%> pragma or attribute");
	break;
      case AARCH64_PARSE_INVALID_ARG:
	error ("invalid name (\"%s\") in %<target(\"arch=\")%> pragma or attribute", str);
	aarch64_print_hint_for_arch (str);
	break;
      case AARCH64_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %s of value (\"%s\") in "
	       "%<target()%> pragma or attribute", invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Handle the argument CPU_STR to the cpu= target attribute.  */

static bool
aarch64_handle_attr_cpu (const char *str)
{
  const struct processor *tmp_cpu = NULL;
  std::string invalid_extension;
  enum aarch64_parse_opt_result parse_res
    = aarch64_parse_cpu (str, &tmp_cpu, &aarch64_isa_flags, &invalid_extension);

  if (parse_res == AARCH64_PARSE_OK)
    {
      gcc_assert (tmp_cpu);
      selected_tune = tmp_cpu;
      explicit_tune_core = selected_tune->ident;

      selected_arch = &all_architectures[tmp_cpu->arch];
      explicit_arch = selected_arch->arch;
      return true;
    }

  switch (parse_res)
    {
      case AARCH64_PARSE_MISSING_ARG:
	error ("missing name in %<target(\"cpu=\")%> pragma or attribute");
	break;
      case AARCH64_PARSE_INVALID_ARG:
	error ("invalid name (\"%s\") in %<target(\"cpu=\")%> pragma or attribute", str);
	aarch64_print_hint_for_core (str);
	break;
      case AARCH64_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %s of value (\"%s\") in "
	       "%<target()%> pragma or attribute", invalid_extension.c_str (), str);
	aarch64_print_hint_for_extensions (invalid_extension);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Handle the argument STR to the branch-protection= attribute.  */

 static bool
 aarch64_handle_attr_branch_protection (const char* str)
 {
  char *err_str = (char *) xmalloc (strlen (str) + 1);
  enum aarch64_parse_opt_result res = aarch64_parse_branch_protection (str,
								      &err_str);
  bool success = false;
  switch (res)
    {
     case AARCH64_PARSE_MISSING_ARG:
       error ("missing argument to %<target(\"branch-protection=\")%> pragma or"
	      " attribute");
       break;
     case AARCH64_PARSE_INVALID_ARG:
       error ("invalid protection type (\"%s\") in %<target(\"branch-protection"
	      "=\")%> pragma or attribute", err_str);
       break;
     case AARCH64_PARSE_OK:
       success = true;
      /* Fall through.  */
     case AARCH64_PARSE_INVALID_FEATURE:
       break;
     default:
       gcc_unreachable ();
    }
  free (err_str);
  return success;
 }

/* Handle the argument STR to the tune= target attribute.  */

static bool
aarch64_handle_attr_tune (const char *str)
{
  const struct processor *tmp_tune = NULL;
  enum aarch64_parse_opt_result parse_res
    = aarch64_parse_tune (str, &tmp_tune);

  if (parse_res == AARCH64_PARSE_OK)
    {
      gcc_assert (tmp_tune);
      selected_tune = tmp_tune;
      explicit_tune_core = selected_tune->ident;
      return true;
    }

  switch (parse_res)
    {
      case AARCH64_PARSE_INVALID_ARG:
	error ("invalid name (\"%s\") in %<target(\"tune=\")%> pragma or attribute", str);
	aarch64_print_hint_for_core (str);
	break;
      default:
	gcc_unreachable ();
    }

  return false;
}

/* Parse an architecture extensions target attribute string specified in STR.
   For example "+fp+nosimd".  Show any errors if needed.  Return TRUE
   if successful.  Update aarch64_isa_flags to reflect the ISA features
   modified.  */

static bool
aarch64_handle_attr_isa_flags (char *str)
{
  enum aarch64_parse_opt_result parse_res;
  uint64_t isa_flags = aarch64_isa_flags;

  /* We allow "+nothing" in the beginning to clear out all architectural
     features if the user wants to handpick specific features.  */
  if (strncmp ("+nothing", str, 8) == 0)
    {
      isa_flags = 0;
      str += 8;
    }

  std::string invalid_extension;
  parse_res = aarch64_parse_extension (str, &isa_flags, &invalid_extension);

  if (parse_res == AARCH64_PARSE_OK)
    {
      aarch64_isa_flags = isa_flags;
      return true;
    }

  switch (parse_res)
    {
      case AARCH64_PARSE_MISSING_ARG:
	error ("missing value in %<target()%> pragma or attribute");
	break;

      case AARCH64_PARSE_INVALID_FEATURE:
	error ("invalid feature modifier %s of value (\"%s\") in "
	       "%<target()%> pragma or attribute", invalid_extension.c_str (), str);
	break;

      default:
	gcc_unreachable ();
    }

 return false;
}

/* The target attributes that we support.  On top of these we also support just
   ISA extensions, like  __attribute__ ((target ("+crc"))), but that case is
   handled explicitly in aarch64_process_one_target_attr.  */

static const struct aarch64_attribute_info aarch64_attributes[] =
{
  { "general-regs-only", aarch64_attr_mask, false, NULL,
     OPT_mgeneral_regs_only },
  { "fix-cortex-a53-835769", aarch64_attr_bool, true, NULL,
     OPT_mfix_cortex_a53_835769 },
  { "fix-cortex-a53-843419", aarch64_attr_bool, true, NULL,
     OPT_mfix_cortex_a53_843419 },
  { "cmodel", aarch64_attr_enum, false, NULL, OPT_mcmodel_ },
  { "strict-align", aarch64_attr_mask, true, NULL, OPT_mstrict_align },
  { "omit-leaf-frame-pointer", aarch64_attr_bool, true, NULL,
     OPT_momit_leaf_frame_pointer },
  { "tls-dialect", aarch64_attr_enum, false, NULL, OPT_mtls_dialect_ },
  { "arch", aarch64_attr_custom, false, aarch64_handle_attr_arch,
     OPT_march_ },
  { "cpu", aarch64_attr_custom, false, aarch64_handle_attr_cpu, OPT_mcpu_ },
  { "tune", aarch64_attr_custom, false, aarch64_handle_attr_tune,
     OPT_mtune_ },
  { "branch-protection", aarch64_attr_custom, false,
     aarch64_handle_attr_branch_protection, OPT_mbranch_protection_ },
  { "sign-return-address", aarch64_attr_enum, false, NULL,
     OPT_msign_return_address_ },
  { NULL, aarch64_attr_custom, false, NULL, OPT____ }
};

/* Parse ARG_STR which contains the definition of one target attribute.
   Show appropriate errors if any or return true if the attribute is valid.  */

static bool
aarch64_process_one_target_attr (char *arg_str)
{
  bool invert = false;

  size_t len = strlen (arg_str);

  if (len == 0)
    {
      error ("malformed %<target()%> pragma or attribute");
      return false;
    }

  char *str_to_check = (char *) alloca (len + 1);
  strcpy (str_to_check, arg_str);

  /* We have something like __attribute__ ((target ("+fp+nosimd"))).
     It is easier to detect and handle it explicitly here rather than going
     through the machinery for the rest of the target attributes in this
     function.  */
  if (*str_to_check == '+')
    return aarch64_handle_attr_isa_flags (str_to_check);

  if (len > 3 && strncmp (str_to_check, "no-", 3) == 0)
    {
      invert = true;
      str_to_check += 3;
    }
  char *arg = strchr (str_to_check, '=');

  /* If we found opt=foo then terminate STR_TO_CHECK at the '='
     and point ARG to "foo".  */
  if (arg)
    {
      *arg = '\0';
      arg++;
    }
  const struct aarch64_attribute_info *p_attr;
  bool found = false;
  for (p_attr = aarch64_attributes; p_attr->name; p_attr++)
    {
      /* If the names don't match up, or the user has given an argument
	 to an attribute that doesn't accept one, or didn't give an argument
	 to an attribute that expects one, fail to match.  */
      if (strcmp (str_to_check, p_attr->name) != 0)
	continue;

      found = true;
      bool attr_need_arg_p = p_attr->attr_type == aarch64_attr_custom
			      || p_attr->attr_type == aarch64_attr_enum;

      if (attr_need_arg_p ^ (arg != NULL))
	{
	  error ("pragma or attribute %<target(\"%s\")%> does not accept an argument", str_to_check);
	  return false;
	}

      /* If the name matches but the attribute does not allow "no-" versions
	 then we can't match.  */
      if (invert && !p_attr->allow_neg)
	{
	  error ("pragma or attribute %<target(\"%s\")%> does not allow a negated form", str_to_check);
	  return false;
	}

      switch (p_attr->attr_type)
	{
	/* Has a custom handler registered.
	   For example, cpu=, arch=, tune=.  */
	  case aarch64_attr_custom:
	    gcc_assert (p_attr->handler);
	    if (!p_attr->handler (arg))
	      return false;
	    break;

	  /* Either set or unset a boolean option.  */
	  case aarch64_attr_bool:
	    {
	      struct cl_decoded_option decoded;

	      generate_option (p_attr->opt_num, NULL, !invert,
			       CL_TARGET, &decoded);
	      aarch64_handle_option (&global_options, &global_options_set,
				      &decoded, input_location);
	      break;
	    }
	  /* Set or unset a bit in the target_flags.  aarch64_handle_option
	     should know what mask to apply given the option number.  */
	  case aarch64_attr_mask:
	    {
	      struct cl_decoded_option decoded;
	      /* We only need to specify the option number.
		 aarch64_handle_option will know which mask to apply.  */
	      decoded.opt_index = p_attr->opt_num;
	      decoded.value = !invert;
	      aarch64_handle_option (&global_options, &global_options_set,
				      &decoded, input_location);
	      break;
	    }
	  /* Use the option setting machinery to set an option to an enum.  */
	  case aarch64_attr_enum:
	    {
	      gcc_assert (arg);
	      bool valid;
	      int value;
	      valid = opt_enum_arg_to_value (p_attr->opt_num, arg,
					      &value, CL_TARGET);
	      if (valid)
		{
		  set_option (&global_options, NULL, p_attr->opt_num, value,
			      NULL, DK_UNSPECIFIED, input_location,
			      global_dc);
		}
	      else
		{
		  error ("pragma or attribute %<target(\"%s=%s\")%> is not valid", str_to_check, arg);
		}
	      break;
	    }
	  default:
	    gcc_unreachable ();
	}
    }

  /* If we reached here we either have found an attribute and validated
     it or didn't match any.  If we matched an attribute but its arguments
     were malformed we will have returned false already.  */
  return found;
}

/* Count how many times the character C appears in
   NULL-terminated string STR.  */

static unsigned int
num_occurences_in_str (char c, char *str)
{
  unsigned int res = 0;
  while (*str != '\0')
    {
      if (*str == c)
	res++;

      str++;
    }

  return res;
}

/* Parse the tree in ARGS that contains the target attribute information
   and update the global target options space.  */

bool
aarch64_process_target_attr (tree args)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree head = TREE_VALUE (args);
	  if (head)
	    {
	      if (!aarch64_process_target_attr (head))
		return false;
	    }
	  args = TREE_CHAIN (args);
	} while (args);

      return true;
    }

  if (TREE_CODE (args) != STRING_CST)
    {
      error ("attribute %<target%> argument not a string");
      return false;
    }

  size_t len = strlen (TREE_STRING_POINTER (args));
  char *str_to_check = (char *) alloca (len + 1);
  strcpy (str_to_check, TREE_STRING_POINTER (args));

  if (len == 0)
    {
      error ("malformed %<target()%> pragma or attribute");
      return false;
    }

  /* Used to catch empty spaces between commas i.e.
     attribute ((target ("attr1,,attr2"))).  */
  unsigned int num_commas = num_occurences_in_str (',', str_to_check);

  /* Handle multiple target attributes separated by ','.  */
  char *token = strtok_r (str_to_check, ",", &str_to_check);

  unsigned int num_attrs = 0;
  while (token)
    {
      num_attrs++;
      if (!aarch64_process_one_target_attr (token))
	{
	  error ("pragma or attribute %<target(\"%s\")%> is not valid", token);
	  return false;
	}

      token = strtok_r (NULL, ",", &str_to_check);
    }

  if (num_attrs != num_commas + 1)
    {
      error ("malformed %<target(\"%s\")%> pragma or attribute", TREE_STRING_POINTER (args));
      return false;
    }

  return true;
}

/* Implement TARGET_OPTION_VALID_ATTRIBUTE_P.  This is used to
   process attribute ((target ("..."))).  */

static bool
aarch64_option_valid_attribute_p (tree fndecl, tree, tree args, int)
{
  struct cl_target_option cur_target;
  bool ret;
  tree old_optimize;
  tree new_target, new_optimize;
  tree existing_target = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  /* If what we're processing is the current pragma string then the
     target option node is already stored in target_option_current_node
     by aarch64_pragma_target_parse in aarch64-c.c.  Use that to avoid
     having to re-parse the string.  This is especially useful to keep
     arm_neon.h compile times down since that header contains a lot
     of intrinsics enclosed in pragmas.  */
  if (!existing_target && args == current_target_pragma)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = target_option_current_node;
      return true;
    }
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  old_optimize = build_optimization_node (&global_options);
  func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  /* If the function changed the optimization levels as well as setting
     target options, start with the optimizations specified.  */
  if (func_optimize && func_optimize != old_optimize)
    cl_optimization_restore (&global_options,
			     TREE_OPTIMIZATION (func_optimize));

  /* Save the current target options to restore at the end.  */
  cl_target_option_save (&cur_target, &global_options);

  /* If fndecl already has some target attributes applied to it, unpack
     them so that we add this attribute on top of them, rather than
     overwriting them.  */
  if (existing_target)
    {
      struct cl_target_option *existing_options
	= TREE_TARGET_OPTION (existing_target);

      if (existing_options)
	cl_target_option_restore (&global_options, existing_options);
    }
  else
    cl_target_option_restore (&global_options,
			TREE_TARGET_OPTION (target_option_current_node));

  ret = aarch64_process_target_attr (args);

  /* Set up any additional state.  */
  if (ret)
    {
      aarch64_override_options_internal (&global_options);
      /* Initialize SIMD builtins if we haven't already.
	 Set current_target_pragma to NULL for the duration so that
	 the builtin initialization code doesn't try to tag the functions
	 being built with the attributes specified by any current pragma, thus
	 going into an infinite recursion.  */
      if (TARGET_SIMD)
	{
	  tree saved_current_target_pragma = current_target_pragma;
	  current_target_pragma = NULL;
	  aarch64_init_simd_builtins ();
	  current_target_pragma = saved_current_target_pragma;
	}
      new_target = build_target_option_node (&global_options);
    }
  else
    new_target = NULL;

  new_optimize = build_optimization_node (&global_options);

  if (fndecl && ret)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  cl_target_option_restore (&global_options, &cur_target);

  if (old_optimize != new_optimize)
    cl_optimization_restore (&global_options,
			     TREE_OPTIMIZATION (old_optimize));
  return ret;
}

/* Helper for aarch64_can_inline_p.  In the case where CALLER and CALLEE are
   tri-bool options (yes, no, don't care) and the default value is
   DEF, determine whether to reject inlining.  */

static bool
aarch64_tribools_ok_for_inlining_p (int caller, int callee,
				     int dont_care, int def)
{
  /* If the callee doesn't care, always allow inlining.  */
  if (callee == dont_care)
    return true;

  /* If the caller doesn't care, always allow inlining.  */
  if (caller == dont_care)
    return true;

  /* Otherwise, allow inlining if either the callee and caller values
     agree, or if the callee is using the default value.  */
  return (callee == caller || callee == def);
}

/* Implement TARGET_CAN_INLINE_P.  Decide whether it is valid
   to inline CALLEE into CALLER based on target-specific info.
   Make sure that the caller and callee have compatible architectural
   features.  Then go through the other possible target attributes
   and see if they can block inlining.  Try not to reject always_inline
   callees unless they are incompatible architecturally.  */

static bool
aarch64_can_inline_p (tree caller, tree callee)
{
  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);

  struct cl_target_option *caller_opts
	= TREE_TARGET_OPTION (caller_tree ? caller_tree
					   : target_option_default_node);

  struct cl_target_option *callee_opts
	= TREE_TARGET_OPTION (callee_tree ? callee_tree
					   : target_option_default_node);

  /* Callee's ISA flags should be a subset of the caller's.  */
  if ((caller_opts->x_aarch64_isa_flags & callee_opts->x_aarch64_isa_flags)
       != callee_opts->x_aarch64_isa_flags)
    return false;

  /* Allow non-strict aligned functions inlining into strict
     aligned ones.  */
  if ((TARGET_STRICT_ALIGN_P (caller_opts->x_target_flags)
       != TARGET_STRICT_ALIGN_P (callee_opts->x_target_flags))
      && !(!TARGET_STRICT_ALIGN_P (callee_opts->x_target_flags)
	   && TARGET_STRICT_ALIGN_P (caller_opts->x_target_flags)))
    return false;

  bool always_inline = lookup_attribute ("always_inline",
					  DECL_ATTRIBUTES (callee));

  /* If the architectural features match up and the callee is always_inline
     then the other attributes don't matter.  */
  if (always_inline)
    return true;

  if (caller_opts->x_aarch64_cmodel_var
      != callee_opts->x_aarch64_cmodel_var)
    return false;

  if (caller_opts->x_aarch64_tls_dialect
      != callee_opts->x_aarch64_tls_dialect)
    return false;

  /* Honour explicit requests to workaround errata.  */
  if (!aarch64_tribools_ok_for_inlining_p (
	  caller_opts->x_aarch64_fix_a53_err835769,
	  callee_opts->x_aarch64_fix_a53_err835769,
	  2, TARGET_FIX_ERR_A53_835769_DEFAULT))
    return false;

  if (!aarch64_tribools_ok_for_inlining_p (
	  caller_opts->x_aarch64_fix_a53_err843419,
	  callee_opts->x_aarch64_fix_a53_err843419,
	  2, TARGET_FIX_ERR_A53_843419))
    return false;

  /* If the user explicitly specified -momit-leaf-frame-pointer for the
     caller and calle and they don't match up, reject inlining.  */
  if (!aarch64_tribools_ok_for_inlining_p (
	  caller_opts->x_flag_omit_leaf_frame_pointer,
	  callee_opts->x_flag_omit_leaf_frame_pointer,
	  2, 1))
    return false;

  /* If the callee has specific tuning overrides, respect them.  */
  if (callee_opts->x_aarch64_override_tune_string != NULL
      && caller_opts->x_aarch64_override_tune_string == NULL)
    return false;

  /* If the user specified tuning override strings for the
     caller and callee and they don't match up, reject inlining.
     We just do a string compare here, we don't analyze the meaning
     of the string, as it would be too costly for little gain.  */
  if (callee_opts->x_aarch64_override_tune_string
      && caller_opts->x_aarch64_override_tune_string
      && (strcmp (callee_opts->x_aarch64_override_tune_string,
		  caller_opts->x_aarch64_override_tune_string) != 0))
    return false;

  return true;
}

/* Return the ID of the TLDESC ABI, initializing the descriptor if hasn't
   been already.  */

unsigned int
aarch64_tlsdesc_abi_id ()
{
  predefined_function_abi &tlsdesc_abi = function_abis[ARM_PCS_TLSDESC];
  if (!tlsdesc_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers;
      CLEAR_HARD_REG_SET (full_reg_clobbers);
      SET_HARD_REG_BIT (full_reg_clobbers, R0_REGNUM);
      SET_HARD_REG_BIT (full_reg_clobbers, CC_REGNUM);
      for (int regno = P0_REGNUM; regno <= P15_REGNUM; ++regno)
	SET_HARD_REG_BIT (full_reg_clobbers, regno);
      tlsdesc_abi.initialize (ARM_PCS_TLSDESC, full_reg_clobbers);
    }
  return tlsdesc_abi.id ();
}

/* Return true if SYMBOL_REF X binds locally.  */

static bool
aarch64_symbol_binds_local_p (const_rtx x)
{
  return (SYMBOL_REF_DECL (x)
	  ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	  : SYMBOL_REF_LOCAL_P (x));
}

/* Return true if SYMBOL_REF X is thread local */
static bool
aarch64_tls_symbol_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  if (GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Classify a TLS symbol into one of the TLS kinds.  */
enum aarch64_symbol_type
aarch64_classify_tls_symbol (rtx x)
{
  enum tls_model tls_kind = tls_symbolic_operand_type (x);

  switch (tls_kind)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
    case TLS_MODEL_LOCAL_DYNAMIC:
      return TARGET_TLS_DESC ? SYMBOL_SMALL_TLSDESC : SYMBOL_SMALL_TLSGD;

    case TLS_MODEL_INITIAL_EXEC:
      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_TINY:
	case AARCH64_CMODEL_TINY_PIC:
	  return SYMBOL_TINY_TLSIE;
	default:
	  return SYMBOL_SMALL_TLSIE;
	}

    case TLS_MODEL_LOCAL_EXEC:
      if (aarch64_tls_size == 12)
	return SYMBOL_TLSLE12;
      else if (aarch64_tls_size == 24)
	return SYMBOL_TLSLE24;
      else if (aarch64_tls_size == 32)
	return SYMBOL_TLSLE32;
      else if (aarch64_tls_size == 48)
	return SYMBOL_TLSLE48;
      else
	gcc_unreachable ();

    case TLS_MODEL_EMULATED:
    case TLS_MODEL_NONE:
      return SYMBOL_FORCE_TO_MEM;

    default:
      gcc_unreachable ();
    }
}

/* Return the correct method for accessing X + OFFSET, where X is either
   a SYMBOL_REF or LABEL_REF.  */

enum aarch64_symbol_type
aarch64_classify_symbol (rtx x, HOST_WIDE_INT offset)
{
  if (GET_CODE (x) == LABEL_REF)
    {
      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_LARGE:
	  return SYMBOL_FORCE_TO_MEM;

	case AARCH64_CMODEL_TINY_PIC:
	case AARCH64_CMODEL_TINY:
	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL_SPIC:
	case AARCH64_CMODEL_SMALL_PIC:
	case AARCH64_CMODEL_SMALL:
	  return SYMBOL_SMALL_ABSOLUTE;

	default:
	  gcc_unreachable ();
	}
    }

  if (GET_CODE (x) == SYMBOL_REF)
    {
      if (aarch64_tls_symbol_p (x))
	return aarch64_classify_tls_symbol (x);

      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_TINY:
	  /* When we retrieve symbol + offset address, we have to make sure
	     the offset does not cause overflow of the final address.  But
	     we have no way of knowing the address of symbol at compile time
	     so we can't accurately say if the distance between the PC and
	     symbol + offset is outside the addressible range of +/-1MB in the
	     TINY code model.  So we limit the maximum offset to +/-64KB and
	     assume the offset to the symbol is not larger than +/-(1MB - 64KB).
	     If offset_within_block_p is true we allow larger offsets.
	     Furthermore force to memory if the symbol is a weak reference to
	     something that doesn't resolve to a symbol in this module.  */

	  if (SYMBOL_REF_WEAK (x) && !aarch64_symbol_binds_local_p (x))
	    return SYMBOL_FORCE_TO_MEM;
	  if (!(IN_RANGE (offset, -0x10000, 0x10000)
		|| offset_within_block_p (x, offset)))
	    return SYMBOL_FORCE_TO_MEM;

	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL:
	  /* Same reasoning as the tiny code model, but the offset cap here is
	     1MB, allowing +/-3.9GB for the offset to the symbol.  */

	  if (SYMBOL_REF_WEAK (x) && !aarch64_symbol_binds_local_p (x))
	    return SYMBOL_FORCE_TO_MEM;
	  if (!(IN_RANGE (offset, -0x100000, 0x100000)
		|| offset_within_block_p (x, offset)))
	    return SYMBOL_FORCE_TO_MEM;

	  return SYMBOL_SMALL_ABSOLUTE;

	case AARCH64_CMODEL_TINY_PIC:
	  if (!aarch64_symbol_binds_local_p (x))
	    return SYMBOL_TINY_GOT;
	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL_SPIC:
	case AARCH64_CMODEL_SMALL_PIC:
	  if (!aarch64_symbol_binds_local_p (x))
	    return (aarch64_cmodel == AARCH64_CMODEL_SMALL_SPIC
		    ?  SYMBOL_SMALL_GOT_28K : SYMBOL_SMALL_GOT_4G);
	  return SYMBOL_SMALL_ABSOLUTE;

	case AARCH64_CMODEL_LARGE:
	  /* This is alright even in PIC code as the constant
	     pool reference is always PC relative and within
	     the same translation unit.  */
	  if (!aarch64_pcrelative_literal_loads && CONSTANT_POOL_ADDRESS_P (x))
	    return SYMBOL_SMALL_ABSOLUTE;
	  else
	    return SYMBOL_FORCE_TO_MEM;

	default:
	  gcc_unreachable ();
	}
    }

  /* By default push everything into the constant pool.  */
  return SYMBOL_FORCE_TO_MEM;
}

bool
aarch64_constant_address_p (rtx x)
{
  return (CONSTANT_P (x) && memory_address_p (DImode, x));
}

bool
aarch64_legitimate_pic_operand_p (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF
      || (GET_CODE (x) == CONST
	  && GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
     return false;

  return true;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P hook.  Return true for constants
   that should be rematerialized rather than spilled.  */

static bool
aarch64_legitimate_constant_p (machine_mode mode, rtx x)
{
  /* Support CSE and rematerialization of common constants.  */
  if (CONST_INT_P (x)
      || (CONST_DOUBLE_P (x) && GET_MODE_CLASS (mode) == MODE_FLOAT)
      || GET_CODE (x) == CONST_VECTOR)
    return true;

  /* Do not allow vector struct mode constants for Advanced SIMD.
     We could support 0 and -1 easily, but they need support in
     aarch64-simd.md.  */
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
    return false;

  /* Only accept variable-length vector constants if they can be
     handled directly.

     ??? It would be possible to handle rematerialization of other
     constants via secondary reloads.  */
  if (vec_flags & VEC_ANY_SVE)
    return aarch64_simd_valid_immediate (x, NULL);

  if (GET_CODE (x) == HIGH)
    x = XEXP (x, 0);

  /* Accept polynomial constants that can be calculated by using the
     destination of a move as the sole temporary.  Constants that
     require a second temporary cannot be rematerialized (they can't be
     forced to memory and also aren't legitimate constants).  */
  poly_int64 offset;
  if (poly_int_rtx_p (x, &offset))
    return aarch64_offset_temporaries (false, offset) <= 1;

  /* If an offset is being added to something else, we need to allow the
     base to be moved into the destination register, meaning that there
     are no free temporaries for the offset.  */
  x = strip_offset (x, &offset);
  if (!offset.is_constant () && aarch64_offset_temporaries (true, offset) > 0)
    return false;

  /* Do not allow const (plus (anchor_symbol, const_int)).  */
  if (maybe_ne (offset, 0) && SYMBOL_REF_P (x) && SYMBOL_REF_ANCHOR_P (x))
    return false;

  /* Treat symbols as constants.  Avoid TLS symbols as they are complex,
     so spilling them is better than rematerialization.  */
  if (SYMBOL_REF_P (x) && !SYMBOL_REF_TLS_MODEL (x))
    return true;

  /* Label references are always constant.  */
  if (GET_CODE (x) == LABEL_REF)
    return true;

  return false;
}

rtx
aarch64_load_tp (rtx target)
{
  if (!target
      || GET_MODE (target) != Pmode
      || !register_operand (target, Pmode))
    target = gen_reg_rtx (Pmode);

  /* Can return in any reg.  */
  emit_insn (gen_aarch64_load_tp_hard (target));
  return target;
}

/* On AAPCS systems, this is the "struct __va_list".  */
static GTY(()) tree va_list_type;

/* Implement TARGET_BUILD_BUILTIN_VA_LIST.
   Return the type to use as __builtin_va_list.

   AAPCS64 \S 7.1.4 requires that va_list be a typedef for a type defined as:

   struct __va_list
   {
     void *__stack;
     void *__gr_top;
     void *__vr_top;
     int   __gr_offs;
     int   __vr_offs;
   };  */

static tree
aarch64_build_builtin_va_list (void)
{
  tree va_list_name;
  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;

  /* Create the type.  */
  va_list_type = lang_hooks.types.make_type (RECORD_TYPE);
  /* Give it the required name.  */
  va_list_name = build_decl (BUILTINS_LOCATION,
			     TYPE_DECL,
			     get_identifier ("__va_list"),
			     va_list_type);
  DECL_ARTIFICIAL (va_list_name) = 1;
  TYPE_NAME (va_list_type) = va_list_name;
  TYPE_STUB_DECL (va_list_type) = va_list_name;

  /* Create the fields.  */
  f_stack = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__stack"),
			ptr_type_node);
  f_grtop = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__gr_top"),
			ptr_type_node);
  f_vrtop = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__vr_top"),
			ptr_type_node);
  f_groff = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__gr_offs"),
			integer_type_node);
  f_vroff = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__vr_offs"),
			integer_type_node);

  /* Tell tree-stdarg pass about our internal offset fields.
     NOTE: va_list_gpr/fpr_counter_field are only used for tree comparision
     purpose to identify whether the code is updating va_list internal
     offset fields through irregular way.  */
  va_list_gpr_counter_field = f_groff;
  va_list_fpr_counter_field = f_vroff;

  DECL_ARTIFICIAL (f_stack) = 1;
  DECL_ARTIFICIAL (f_grtop) = 1;
  DECL_ARTIFICIAL (f_vrtop) = 1;
  DECL_ARTIFICIAL (f_groff) = 1;
  DECL_ARTIFICIAL (f_vroff) = 1;

  DECL_FIELD_CONTEXT (f_stack) = va_list_type;
  DECL_FIELD_CONTEXT (f_grtop) = va_list_type;
  DECL_FIELD_CONTEXT (f_vrtop) = va_list_type;
  DECL_FIELD_CONTEXT (f_groff) = va_list_type;
  DECL_FIELD_CONTEXT (f_vroff) = va_list_type;

  TYPE_FIELDS (va_list_type) = f_stack;
  DECL_CHAIN (f_stack) = f_grtop;
  DECL_CHAIN (f_grtop) = f_vrtop;
  DECL_CHAIN (f_vrtop) = f_groff;
  DECL_CHAIN (f_groff) = f_vroff;

  /* Compute its layout.  */
  layout_type (va_list_type);

  return va_list_type;
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */
static void
aarch64_expand_builtin_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  const CUMULATIVE_ARGS *cum;
  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;
  tree stack, grtop, vrtop, groff, vroff;
  tree t;
  int gr_save_area_size = cfun->va_list_gpr_size;
  int vr_save_area_size = cfun->va_list_fpr_size;
  int vr_offset;

  cum = &crtl->args.info;
  if (cfun->va_list_gpr_size)
    gr_save_area_size = MIN ((NUM_ARG_REGS - cum->aapcs_ncrn) * UNITS_PER_WORD,
			     cfun->va_list_gpr_size);
  if (cfun->va_list_fpr_size)
    vr_save_area_size = MIN ((NUM_FP_ARG_REGS - cum->aapcs_nvrn)
			     * UNITS_PER_VREG, cfun->va_list_fpr_size);

  if (!TARGET_FLOAT)
    {
      gcc_assert (cum->aapcs_nvrn == 0);
      vr_save_area_size = 0;
    }

  f_stack = TYPE_FIELDS (va_list_type_node);
  f_grtop = DECL_CHAIN (f_stack);
  f_vrtop = DECL_CHAIN (f_grtop);
  f_groff = DECL_CHAIN (f_vrtop);
  f_vroff = DECL_CHAIN (f_groff);

  stack = build3 (COMPONENT_REF, TREE_TYPE (f_stack), valist, f_stack,
		  NULL_TREE);
  grtop = build3 (COMPONENT_REF, TREE_TYPE (f_grtop), valist, f_grtop,
		  NULL_TREE);
  vrtop = build3 (COMPONENT_REF, TREE_TYPE (f_vrtop), valist, f_vrtop,
		  NULL_TREE);
  groff = build3 (COMPONENT_REF, TREE_TYPE (f_groff), valist, f_groff,
		  NULL_TREE);
  vroff = build3 (COMPONENT_REF, TREE_TYPE (f_vroff), valist, f_vroff,
		  NULL_TREE);

  /* Emit code to initialize STACK, which points to the next varargs stack
     argument.  CUM->AAPCS_STACK_SIZE gives the number of stack words used
     by named arguments.  STACK is 8-byte aligned.  */
  t = make_tree (TREE_TYPE (stack), virtual_incoming_args_rtx);
  if (cum->aapcs_stack_size > 0)
    t = fold_build_pointer_plus_hwi (t, cum->aapcs_stack_size * UNITS_PER_WORD);
  t = build2 (MODIFY_EXPR, TREE_TYPE (stack), stack, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize GRTOP, the top of the GR save area.
     virtual_incoming_args_rtx should have been 16 byte aligned.  */
  t = make_tree (TREE_TYPE (grtop), virtual_incoming_args_rtx);
  t = build2 (MODIFY_EXPR, TREE_TYPE (grtop), grtop, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize VRTOP, the top of the VR save area.
     This address is gr_save_area_bytes below GRTOP, rounded
     down to the next 16-byte boundary.  */
  t = make_tree (TREE_TYPE (vrtop), virtual_incoming_args_rtx);
  vr_offset = ROUND_UP (gr_save_area_size,
			STACK_BOUNDARY / BITS_PER_UNIT);

  if (vr_offset)
    t = fold_build_pointer_plus_hwi (t, -vr_offset);
  t = build2 (MODIFY_EXPR, TREE_TYPE (vrtop), vrtop, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize GROFF, the offset from GRTOP of the
     next GPR argument.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (groff), groff,
	      build_int_cst (TREE_TYPE (groff), -gr_save_area_size));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Likewise emit code to initialize VROFF, the offset from FTOP
     of the next VR argument.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (vroff), vroff,
	      build_int_cst (TREE_TYPE (vroff), -vr_save_area_size));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  */

static tree
aarch64_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			      gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree addr;
  bool indirect_p;
  bool is_ha;		/* is HFA or HVA.  */
  bool dw_align;	/* double-word align.  */
  machine_mode ag_mode = VOIDmode;
  int nregs;
  machine_mode mode;

  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;
  tree stack, f_top, f_off, off, arg, roundup, on_stack;
  HOST_WIDE_INT size, rsize, adjust, align;
  tree t, u, cond1, cond2;

  indirect_p = pass_va_arg_by_reference (type);
  if (indirect_p)
    type = build_pointer_type (type);

  mode = TYPE_MODE (type);

  f_stack = TYPE_FIELDS (va_list_type_node);
  f_grtop = DECL_CHAIN (f_stack);
  f_vrtop = DECL_CHAIN (f_grtop);
  f_groff = DECL_CHAIN (f_vrtop);
  f_vroff = DECL_CHAIN (f_groff);

  stack = build3 (COMPONENT_REF, TREE_TYPE (f_stack), unshare_expr (valist),
		  f_stack, NULL_TREE);
  size = int_size_in_bytes (type);

  bool abi_break;
  align
    = aarch64_function_arg_alignment (mode, type, &abi_break) / BITS_PER_UNIT;

  dw_align = false;
  adjust = 0;
  if (aarch64_vfp_is_call_or_return_candidate (mode,
					       type,
					       &ag_mode,
					       &nregs,
					       &is_ha))
    {
      /* No frontends can create types with variable-sized modes, so we
	 shouldn't be asked to pass or return them.  */
      unsigned int ag_size = GET_MODE_SIZE (ag_mode).to_constant ();

      /* TYPE passed in fp/simd registers.  */
      if (!TARGET_FLOAT)
	aarch64_err_no_fpadvsimd (mode);

      f_top = build3 (COMPONENT_REF, TREE_TYPE (f_vrtop),
		      unshare_expr (valist), f_vrtop, NULL_TREE);
      f_off = build3 (COMPONENT_REF, TREE_TYPE (f_vroff),
		      unshare_expr (valist), f_vroff, NULL_TREE);

      rsize = nregs * UNITS_PER_VREG;

      if (is_ha)
	{
	  if (BYTES_BIG_ENDIAN && ag_size < UNITS_PER_VREG)
	    adjust = UNITS_PER_VREG - ag_size;
	}
      else if (BLOCK_REG_PADDING (mode, type, 1) == PAD_DOWNWARD
	       && size < UNITS_PER_VREG)
	{
	  adjust = UNITS_PER_VREG - size;
	}
    }
  else
    {
      /* TYPE passed in general registers.  */
      f_top = build3 (COMPONENT_REF, TREE_TYPE (f_grtop),
		      unshare_expr (valist), f_grtop, NULL_TREE);
      f_off = build3 (COMPONENT_REF, TREE_TYPE (f_groff),
		      unshare_expr (valist), f_groff, NULL_TREE);
      rsize = ROUND_UP (size, UNITS_PER_WORD);
      nregs = rsize / UNITS_PER_WORD;

      if (align > 8)
	{
	  if (abi_break && warn_psabi)
	    inform (input_location, "parameter passing for argument of type "
		    "%qT changed in GCC 9.1", type);
	  dw_align = true;
	}

      if (BLOCK_REG_PADDING (mode, type, 1) == PAD_DOWNWARD
	  && size < UNITS_PER_WORD)
	{
	  adjust = UNITS_PER_WORD  - size;
	}
    }

  /* Get a local temporary for the field value.  */
  off = get_initialized_tmp_var (f_off, pre_p, NULL);

  /* Emit code to branch if off >= 0.  */
  t = build2 (GE_EXPR, boolean_type_node, off,
	      build_int_cst (TREE_TYPE (off), 0));
  cond1 = build3 (COND_EXPR, ptr_type_node, t, NULL_TREE, NULL_TREE);

  if (dw_align)
    {
      /* Emit: offs = (offs + 15) & -16.  */
      t = build2 (PLUS_EXPR, TREE_TYPE (off), off,
		  build_int_cst (TREE_TYPE (off), 15));
      t = build2 (BIT_AND_EXPR, TREE_TYPE (off), t,
		  build_int_cst (TREE_TYPE (off), -16));
      roundup = build2 (MODIFY_EXPR, TREE_TYPE (off), off, t);
    }
  else
    roundup = NULL;

  /* Update ap.__[g|v]r_offs  */
  t = build2 (PLUS_EXPR, TREE_TYPE (off), off,
	      build_int_cst (TREE_TYPE (off), rsize));
  t = build2 (MODIFY_EXPR, TREE_TYPE (f_off), unshare_expr (f_off), t);

  /* String up.  */
  if (roundup)
    t = build2 (COMPOUND_EXPR, TREE_TYPE (t), roundup, t);

  /* [cond2] if (ap.__[g|v]r_offs > 0)  */
  u = build2 (GT_EXPR, boolean_type_node, unshare_expr (f_off),
	      build_int_cst (TREE_TYPE (f_off), 0));
  cond2 = build3 (COND_EXPR, ptr_type_node, u, NULL_TREE, NULL_TREE);

  /* String up: make sure the assignment happens before the use.  */
  t = build2 (COMPOUND_EXPR, TREE_TYPE (cond2), t, cond2);
  COND_EXPR_ELSE (cond1) = t;

  /* Prepare the trees handling the argument that is passed on the stack;
     the top level node will store in ON_STACK.  */
  arg = get_initialized_tmp_var (stack, pre_p, NULL);
  if (align > 8)
    {
      /* if (alignof(type) > 8) (arg = arg + 15) & -16;  */
      t = fold_build_pointer_plus_hwi (arg, 15);
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), -16));
      roundup = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg, t);
    }
  else
    roundup = NULL;
  /* Advance ap.__stack  */
  t = fold_build_pointer_plus_hwi (arg, size + 7);
  t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
	      build_int_cst (TREE_TYPE (t), -8));
  t = build2 (MODIFY_EXPR, TREE_TYPE (stack), unshare_expr (stack), t);
  /* String up roundup and advance.  */
  if (roundup)
    t = build2 (COMPOUND_EXPR, TREE_TYPE (t), roundup, t);
  /* String up with arg */
  on_stack = build2 (COMPOUND_EXPR, TREE_TYPE (arg), t, arg);
  /* Big-endianness related address adjustment.  */
  if (BLOCK_REG_PADDING (mode, type, 1) == PAD_DOWNWARD
      && size < UNITS_PER_WORD)
  {
    t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (arg), arg,
		size_int (UNITS_PER_WORD - size));
    on_stack = build2 (COMPOUND_EXPR, TREE_TYPE (arg), on_stack, t);
  }

  COND_EXPR_THEN (cond1) = unshare_expr (on_stack);
  COND_EXPR_THEN (cond2) = unshare_expr (on_stack);

  /* Adjustment to OFFSET in the case of BIG_ENDIAN.  */
  t = off;
  if (adjust)
    t = build2 (PREINCREMENT_EXPR, TREE_TYPE (off), off,
		build_int_cst (TREE_TYPE (off), adjust));

  t = fold_convert (sizetype, t);
  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (f_top), f_top, t);

  if (is_ha)
    {
      /* type ha; // treat as "struct {ftype field[n];}"
         ... [computing offs]
         for (i = 0; i <nregs; ++i, offs += 16)
	   ha.field[i] = *((ftype *)(ap.__vr_top + offs));
	 return ha;  */
      int i;
      tree tmp_ha, field_t, field_ptr_t;

      /* Declare a local variable.  */
      tmp_ha = create_tmp_var_raw (type, "ha");
      gimple_add_tmp_var (tmp_ha);

      /* Establish the base type.  */
      switch (ag_mode)
	{
	case E_SFmode:
	  field_t = float_type_node;
	  field_ptr_t = float_ptr_type_node;
	  break;
	case E_DFmode:
	  field_t = double_type_node;
	  field_ptr_t = double_ptr_type_node;
	  break;
	case E_TFmode:
	  field_t = long_double_type_node;
	  field_ptr_t = long_double_ptr_type_node;
	  break;
	case E_HFmode:
	  field_t = aarch64_fp16_type_node;
	  field_ptr_t = aarch64_fp16_ptr_type_node;
	  break;
	case E_V2SImode:
	case E_V4SImode:
	    {
	      tree innertype = make_signed_type (GET_MODE_PRECISION (SImode));
	      field_t = build_vector_type_for_mode (innertype, ag_mode);
	      field_ptr_t = build_pointer_type (field_t);
	    }
	  break;
	default:
	  gcc_assert (0);
	}

      /* *(field_ptr_t)&ha = *((field_ptr_t)vr_saved_area  */
      tmp_ha = build1 (ADDR_EXPR, field_ptr_t, tmp_ha);
      addr = t;
      t = fold_convert (field_ptr_t, addr);
      t = build2 (MODIFY_EXPR, field_t,
		  build1 (INDIRECT_REF, field_t, tmp_ha),
		  build1 (INDIRECT_REF, field_t, t));

      /* ha.field[i] = *((field_ptr_t)vr_saved_area + i)  */
      for (i = 1; i < nregs; ++i)
	{
	  addr = fold_build_pointer_plus_hwi (addr, UNITS_PER_VREG);
	  u = fold_convert (field_ptr_t, addr);
	  u = build2 (MODIFY_EXPR, field_t,
		      build2 (MEM_REF, field_t, tmp_ha,
			      build_int_cst (field_ptr_t,
					     (i *
					      int_size_in_bytes (field_t)))),
		      build1 (INDIRECT_REF, field_t, u));
	  t = build2 (COMPOUND_EXPR, TREE_TYPE (t), t, u);
	}

      u = fold_convert (TREE_TYPE (f_top), tmp_ha);
      t = build2 (COMPOUND_EXPR, TREE_TYPE (f_top), t, u);
    }

  COND_EXPR_ELSE (cond2) = t;
  addr = fold_convert (build_pointer_type (type), cond1);
  addr = build_va_arg_indirect_ref (addr);

  if (indirect_p)
    addr = build_va_arg_indirect_ref (addr);

  return addr;
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
aarch64_setup_incoming_varargs (cumulative_args_t cum_v,
				const function_arg_info &arg,
				int *pretend_size ATTRIBUTE_UNUSED, int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  CUMULATIVE_ARGS local_cum;
  int gr_saved = cfun->va_list_gpr_size;
  int vr_saved = cfun->va_list_fpr_size;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  local_cum = *cum;
  aarch64_function_arg_advance (pack_cumulative_args(&local_cum), arg);

  /* Found out how many registers we need to save.
     Honor tree-stdvar analysis results.  */
  if (cfun->va_list_gpr_size)
    gr_saved = MIN (NUM_ARG_REGS - local_cum.aapcs_ncrn,
		    cfun->va_list_gpr_size / UNITS_PER_WORD);
  if (cfun->va_list_fpr_size)
    vr_saved = MIN (NUM_FP_ARG_REGS - local_cum.aapcs_nvrn,
		    cfun->va_list_fpr_size / UNITS_PER_VREG);

  if (!TARGET_FLOAT)
    {
      gcc_assert (local_cum.aapcs_nvrn == 0);
      vr_saved = 0;
    }

  if (!no_rtl)
    {
      if (gr_saved > 0)
	{
	  rtx ptr, mem;

	  /* virtual_incoming_args_rtx should have been 16-byte aligned.  */
	  ptr = plus_constant (Pmode, virtual_incoming_args_rtx,
			       - gr_saved * UNITS_PER_WORD);
	  mem = gen_frame_mem (BLKmode, ptr);
	  set_mem_alias_set (mem, get_varargs_alias_set ());

	  move_block_from_reg (local_cum.aapcs_ncrn + R0_REGNUM,
			       mem, gr_saved);
	}
      if (vr_saved > 0)
	{
	  /* We can't use move_block_from_reg, because it will use
	     the wrong mode, storing D regs only.  */
	  machine_mode mode = TImode;
	  int off, i, vr_start;

	  /* Set OFF to the offset from virtual_incoming_args_rtx of
	     the first vector register.  The VR save area lies below
	     the GR one, and is aligned to 16 bytes.  */
	  off = -ROUND_UP (gr_saved * UNITS_PER_WORD,
			   STACK_BOUNDARY / BITS_PER_UNIT);
	  off -= vr_saved * UNITS_PER_VREG;

	  vr_start = V0_REGNUM + local_cum.aapcs_nvrn;
	  for (i = 0; i < vr_saved; ++i)
	    {
	      rtx ptr, mem;

	      ptr = plus_constant (Pmode, virtual_incoming_args_rtx, off);
	      mem = gen_frame_mem (mode, ptr);
	      set_mem_alias_set (mem, get_varargs_alias_set ());
	      aarch64_emit_move (mem, gen_rtx_REG (mode, vr_start + i));
	      off += UNITS_PER_VREG;
	    }
	}
    }

  /* We don't save the size into *PRETEND_SIZE because we want to avoid
     any complication of having crtl->args.pretend_args_size changed.  */
  cfun->machine->frame.saved_varargs_size
    = (ROUND_UP (gr_saved * UNITS_PER_WORD,
		 STACK_BOUNDARY / BITS_PER_UNIT)
       + vr_saved * UNITS_PER_VREG);
}

static void
aarch64_conditional_register_usage (void)
{
  int i;
  if (!TARGET_FLOAT)
    {
      for (i = V0_REGNUM; i <= V31_REGNUM; i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	}
    }
  if (!TARGET_SVE)
    for (i = P0_REGNUM; i <= P15_REGNUM; i++)
      {
	fixed_regs[i] = 1;
	call_used_regs[i] = 1;
      }

  /* Only allow the FFR and FFRT to be accessed via special patterns.  */
  CLEAR_HARD_REG_BIT (operand_reg_set, FFR_REGNUM);
  CLEAR_HARD_REG_BIT (operand_reg_set, FFRT_REGNUM);

  /* When tracking speculation, we need a couple of call-clobbered registers
     to track the speculation state.  It would be nice to just use
     IP0 and IP1, but currently there are numerous places that just
     assume these registers are free for other uses (eg pointer
     authentication).  */
  if (aarch64_track_speculation)
    {
      fixed_regs[SPECULATION_TRACKER_REGNUM] = 1;
      call_used_regs[SPECULATION_TRACKER_REGNUM] = 1;
      fixed_regs[SPECULATION_SCRATCH_REGNUM] = 1;
      call_used_regs[SPECULATION_SCRATCH_REGNUM] = 1;
    }
}

/* Walk down the type tree of TYPE counting consecutive base elements.
   If *MODEP is VOIDmode, then set it to the first valid floating point
   type.  If a non-floating point type is found, or if a floating point
   type that doesn't match a non-VOIDmode *MODEP is found, then return -1,
   otherwise return the count in the sub-tree.  */
static int
aapcs_vfp_sub_candidate (const_tree type, machine_mode *modep)
{
  machine_mode mode;
  HOST_WIDE_INT size;

  /* SVE types (and types containing SVE types) must be handled
     before calling this function.  */
  gcc_assert (!aarch64_sve::builtin_type_p (type));

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      mode = TYPE_MODE (type);
      if (mode != DFmode && mode != SFmode
	  && mode != TFmode && mode != HFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 1;

      break;

    case COMPLEX_TYPE:
      mode = TYPE_MODE (TREE_TYPE (type));
      if (mode != DFmode && mode != SFmode
	  && mode != TFmode && mode != HFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 2;

      break;

    case VECTOR_TYPE:
      /* Use V2SImode and V4SImode as representatives of all 64-bit
	 and 128-bit vector types.  */
      size = int_size_in_bytes (type);
      switch (size)
	{
	case 8:
	  mode = V2SImode;
	  break;
	case 16:
	  mode = V4SImode;
	  break;
	default:
	  return -1;
	}

      if (*modep == VOIDmode)
	*modep = mode;

      /* Vector modes are considered to be opaque: two vectors are
	 equivalent for the purposes of being homogeneous aggregates
	 if they are the same size.  */
      if (*modep == mode)
	return 1;

      break;

    case ARRAY_TYPE:
      {
	int count;
	tree index = TYPE_DOMAIN (type);

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	count = aapcs_vfp_sub_candidate (TREE_TYPE (type), modep);
	if (count == -1
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || count < 0)
	  return -1;

	count *= (1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		      - tree_to_uhwi (TYPE_MIN_VALUE (index)));

	/* There must be no padding.  */
	if (maybe_ne (wi::to_poly_wide (TYPE_SIZE (type)),
		      count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case RECORD_TYPE:
      {
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count += sub_count;
	  }

	/* There must be no padding.  */
	if (maybe_ne (wi::to_poly_wide (TYPE_SIZE (type)),
		      count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* These aren't very interesting except in a degenerate case.  */
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count = count > sub_count ? count : sub_count;
	  }

	/* There must be no padding.  */
	if (maybe_ne (wi::to_poly_wide (TYPE_SIZE (type)),
		      count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    default:
      break;
    }

  return -1;
}

/* Return TRUE if the type, as described by TYPE and MODE, is a short vector
   type as described in AAPCS64 \S 4.1.2.

   See the comment above aarch64_composite_type_p for the notes on MODE.  */

static bool
aarch64_short_vector_p (const_tree type,
			machine_mode mode)
{
  poly_int64 size = -1;

  if (type && aarch64_sve::builtin_type_p (type))
    return false;

  if (type && TREE_CODE (type) == VECTOR_TYPE)
    size = int_size_in_bytes (type);
  else if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	    || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
    size = GET_MODE_SIZE (mode);

  return known_eq (size, 8) || known_eq (size, 16);
}

/* Return TRUE if the type, as described by TYPE and MODE, is a composite
   type as described in AAPCS64 \S 4.3.  This includes aggregate, union and
   array types.  The C99 floating-point complex types are also considered
   as composite types, according to AAPCS64 \S 7.1.1.  The complex integer
   types, which are GCC extensions and out of the scope of AAPCS64, are
   treated as composite types here as well.

   Note that MODE itself is not sufficient in determining whether a type
   is such a composite type or not.  This is because
   stor-layout.c:compute_record_mode may have already changed the MODE
   (BLKmode) of a RECORD_TYPE TYPE to some other mode.  For example, a
   structure with only one field may have its MODE set to the mode of the
   field.  Also an integer mode whose size matches the size of the
   RECORD_TYPE type may be used to substitute the original mode
   (i.e. BLKmode) in certain circumstances.  In other words, MODE cannot be
   solely relied on.  */

static bool
aarch64_composite_type_p (const_tree type,
			  machine_mode mode)
{
  if (aarch64_short_vector_p (type, mode))
    return false;

  if (type && (AGGREGATE_TYPE_P (type) || TREE_CODE (type) == COMPLEX_TYPE))
    return true;

  if (mode == BLKmode
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    return true;

  return false;
}

/* Return TRUE if an argument, whose type is described by TYPE and MODE,
   shall be passed or returned in simd/fp register(s) (providing these
   parameter passing registers are available).

   Upon successful return, *COUNT returns the number of needed registers,
   *BASE_MODE returns the mode of the individual register and when IS_HAF
   is not NULL, *IS_HA indicates whether or not the argument is a homogeneous
   floating-point aggregate or a homogeneous short-vector aggregate.  */

static bool
aarch64_vfp_is_call_or_return_candidate (machine_mode mode,
					 const_tree type,
					 machine_mode *base_mode,
					 int *count,
					 bool *is_ha)
{
  if (is_ha != NULL) *is_ha = false;

  if (type && aarch64_sve::builtin_type_p (type))
    return false;

  machine_mode new_mode = VOIDmode;
  bool composite_p = aarch64_composite_type_p (type, mode);

  if ((!composite_p && GET_MODE_CLASS (mode) == MODE_FLOAT)
      || aarch64_short_vector_p (type, mode))
    {
      *count = 1;
      new_mode = mode;
    }
  else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    {
      if (is_ha != NULL) *is_ha = true;
      *count = 2;
      new_mode = GET_MODE_INNER (mode);
    }
  else if (type && composite_p)
    {
      int ag_count = aapcs_vfp_sub_candidate (type, &new_mode);

      if (ag_count > 0 && ag_count <= HA_MAX_NUM_FLDS)
	{
	  if (is_ha != NULL) *is_ha = true;
	  *count = ag_count;
	}
      else
	return false;
    }
  else
    return false;

  *base_mode = new_mode;
  return true;
}

/* Implement TARGET_STRUCT_VALUE_RTX.  */

static rtx
aarch64_struct_value_rtx (tree fndecl ATTRIBUTE_UNUSED,
			  int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, AARCH64_STRUCT_VALUE_REGNUM);
}

/* Implements target hook vector_mode_supported_p.  */
static bool
aarch64_vector_mode_supported_p (machine_mode mode)
{
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  return vec_flags != 0 && (vec_flags & VEC_STRUCT) == 0;
}

/* Return the full-width SVE vector mode for element mode MODE, if one
   exists.  */
opt_machine_mode
aarch64_full_sve_mode (scalar_mode mode)
{
  switch (mode)
    {
    case E_DFmode:
      return VNx2DFmode;
    case E_SFmode:
      return VNx4SFmode;
    case E_HFmode:
      return VNx8HFmode;
    case E_DImode:
	return VNx2DImode;
    case E_SImode:
      return VNx4SImode;
    case E_HImode:
      return VNx8HImode;
    case E_QImode:
      return VNx16QImode;
    default:
      return opt_machine_mode ();
    }
}

/* Return the 128-bit Advanced SIMD vector mode for element mode MODE,
   if it exists.  */
opt_machine_mode
aarch64_vq_mode (scalar_mode mode)
{
  switch (mode)
    {
    case E_DFmode:
      return V2DFmode;
    case E_SFmode:
      return V4SFmode;
    case E_HFmode:
      return V8HFmode;
    case E_SImode:
      return V4SImode;
    case E_HImode:
      return V8HImode;
    case E_QImode:
      return V16QImode;
    case E_DImode:
      return V2DImode;
    default:
      return opt_machine_mode ();
    }
}

/* Return appropriate SIMD container
   for MODE within a vector of WIDTH bits.  */
static machine_mode
aarch64_simd_container_mode (scalar_mode mode, poly_int64 width)
{
  if (TARGET_SVE && known_eq (width, BITS_PER_SVE_VECTOR))
    return aarch64_full_sve_mode (mode).else_mode (word_mode);

  gcc_assert (known_eq (width, 64) || known_eq (width, 128));
  if (TARGET_SIMD)
    {
      if (known_eq (width, 128))
	return aarch64_vq_mode (mode).else_mode (word_mode);
      else
	switch (mode)
	  {
	  case E_SFmode:
	    return V2SFmode;
	  case E_HFmode:
	    return V4HFmode;
	  case E_SImode:
	    return V2SImode;
	  case E_HImode:
	    return V4HImode;
	  case E_QImode:
	    return V8QImode;
	  default:
	    break;
	  }
    }
  return word_mode;
}

/* Return 128-bit container as the preferred SIMD mode for MODE.  */
static machine_mode
aarch64_preferred_simd_mode (scalar_mode mode)
{
  poly_int64 bits = TARGET_SVE ? BITS_PER_SVE_VECTOR : 128;
  return aarch64_simd_container_mode (mode, bits);
}

/* Return a list of possible vector sizes for the vectorizer
   to iterate over.  */
static unsigned int
aarch64_autovectorize_vector_modes (vector_modes *modes, bool)
{
  static const machine_mode sve_modes[] = {
    /* Try using full vectors for all element types.  */
    VNx16QImode,

    /* Try using 16-bit containers for 8-bit elements and full vectors
       for wider elements.  */
    VNx8QImode,

    /* Try using 32-bit containers for 8-bit and 16-bit elements and
       full vectors for wider elements.  */
    VNx4QImode,

    /* Try using 64-bit containers for all element types.  */
    VNx2QImode
  };

  static const machine_mode advsimd_modes[] = {
    /* Try using 128-bit vectors for all element types.  */
    V16QImode,

    /* Try using 64-bit vectors for 8-bit elements and 128-bit vectors
       for wider elements.  */
    V8QImode,

    /* Try using 64-bit vectors for 16-bit elements and 128-bit vectors
       for wider elements.

       TODO: We could support a limited form of V4QImode too, so that
       we use 32-bit vectors for 8-bit elements.  */
    V4HImode,

    /* Try using 64-bit vectors for 32-bit elements and 128-bit vectors
       for 64-bit elements.

       TODO: We could similarly support limited forms of V2QImode and V2HImode
       for this case.  */
    V2SImode
  };

  /* Try using N-byte SVE modes only after trying N-byte Advanced SIMD mode.
     This is because:

     - If we can't use N-byte Advanced SIMD vectors then the placement
       doesn't matter; we'll just continue as though the Advanced SIMD
       entry didn't exist.

     - If an SVE main loop with N bytes ends up being cheaper than an
       Advanced SIMD main loop with N bytes then by default we'll replace
       the Advanced SIMD version with the SVE one.

     - If an Advanced SIMD main loop with N bytes ends up being cheaper
       than an SVE main loop with N bytes then by default we'll try to
       use the SVE loop to vectorize the epilogue instead.  */
  unsigned int sve_i = TARGET_SVE ? 0 : ARRAY_SIZE (sve_modes);
  unsigned int advsimd_i = 0;
  while (advsimd_i < ARRAY_SIZE (advsimd_modes))
    {
      if (sve_i < ARRAY_SIZE (sve_modes)
	  && maybe_gt (GET_MODE_NUNITS (sve_modes[sve_i]),
		       GET_MODE_NUNITS (advsimd_modes[advsimd_i])))
	modes->safe_push (sve_modes[sve_i++]);
      else
	modes->safe_push (advsimd_modes[advsimd_i++]);
    }
  while (sve_i < ARRAY_SIZE (sve_modes))
    modes->safe_push (sve_modes[sve_i++]);

  unsigned int flags = 0;
  /* Consider enabling VECT_COMPARE_COSTS for SVE, both so that we
     can compare SVE against Advanced SIMD and so that we can compare
     multiple SVE vectorization approaches against each other.  There's
     not really any point doing this for Advanced SIMD only, since the
     first mode that works should always be the best.  */
  if (TARGET_SVE && aarch64_sve_compare_costs)
    flags |= VECT_COMPARE_COSTS;
  return flags;
}

/* Implement TARGET_MANGLE_TYPE.  */

static const char *
aarch64_mangle_type (const_tree type)
{
  /* The AArch64 ABI documents say that "__va_list" has to be
     mangled as if it is in the "std" namespace.  */
  if (lang_hooks.types_compatible_p (CONST_CAST_TREE (type), va_list_type))
    return "St9__va_list";

  /* Half-precision float.  */
  if (TREE_CODE (type) == REAL_TYPE && TYPE_PRECISION (type) == 16)
    return "Dh";

  /* Mangle AArch64-specific internal types.  TYPE_NAME is non-NULL_TREE for
     builtin types.  */
  if (TYPE_NAME (type) != NULL)
    {
      const char *res;
      if ((res = aarch64_general_mangle_builtin_type (type))
	  || (res = aarch64_sve::mangle_builtin_type (type)))
	return res;
    }

  /* Use the default mangling.  */
  return NULL;
}

/* Implement TARGET_VERIFY_TYPE_CONTEXT.  */

static bool
aarch64_verify_type_context (location_t loc, type_context_kind context,
			     const_tree type, bool silent_p)
{
  return aarch64_sve::verify_type_context (loc, context, type, silent_p);
}

/* Find the first rtx_insn before insn that will generate an assembly
   instruction.  */

static rtx_insn *
aarch64_prev_real_insn (rtx_insn *insn)
{
  if (!insn)
    return NULL;

  do
    {
      insn = prev_real_insn (insn);
    }
  while (insn && recog_memoized (insn) < 0);

  return insn;
}

static bool
is_madd_op (enum attr_type t1)
{
  unsigned int i;
  /* A number of these may be AArch32 only.  */
  enum attr_type mlatypes[] = {
    TYPE_MLA, TYPE_MLAS, TYPE_SMLAD, TYPE_SMLADX, TYPE_SMLAL, TYPE_SMLALD,
    TYPE_SMLALS, TYPE_SMLALXY, TYPE_SMLAWX, TYPE_SMLAWY, TYPE_SMLAXY,
    TYPE_SMMLA, TYPE_UMLAL, TYPE_UMLALS,TYPE_SMLSD, TYPE_SMLSDX, TYPE_SMLSLD
  };

  for (i = 0; i < sizeof (mlatypes) / sizeof (enum attr_type); i++)
    {
      if (t1 == mlatypes[i])
	return true;
    }

  return false;
}

/* Check if there is a register dependency between a load and the insn
   for which we hold recog_data.  */

static bool
dep_between_memop_and_curr (rtx memop)
{
  rtx load_reg;
  int opno;

  gcc_assert (GET_CODE (memop) == SET);

  if (!REG_P (SET_DEST (memop)))
    return false;

  load_reg = SET_DEST (memop);
  for (opno = 1; opno < recog_data.n_operands; opno++)
    {
      rtx operand = recog_data.operand[opno];
      if (REG_P (operand)
          && reg_overlap_mentioned_p (load_reg, operand))
        return true;

    }
  return false;
}


/* When working around the Cortex-A53 erratum 835769,
   given rtx_insn INSN, return true if it is a 64-bit multiply-accumulate
   instruction and has a preceding memory instruction such that a NOP
   should be inserted between them.  */

bool
aarch64_madd_needs_nop (rtx_insn* insn)
{
  enum attr_type attr_type;
  rtx_insn *prev;
  rtx body;

  if (!TARGET_FIX_ERR_A53_835769)
    return false;

  if (!INSN_P (insn) || recog_memoized (insn) < 0)
    return false;

  attr_type = get_attr_type (insn);
  if (!is_madd_op (attr_type))
    return false;

  prev = aarch64_prev_real_insn (insn);
  /* aarch64_prev_real_insn can call recog_memoized on insns other than INSN.
     Restore recog state to INSN to avoid state corruption.  */
  extract_constrain_insn_cached (insn);

  if (!prev || !contains_mem_rtx_p (PATTERN (prev)))
    return false;

  body = single_set (prev);

  /* If the previous insn is a memory op and there is no dependency between
     it and the DImode madd, emit a NOP between them.  If body is NULL then we
     have a complex memory operation, probably a load/store pair.
     Be conservative for now and emit a NOP.  */
  if (GET_MODE (recog_data.operand[0]) == DImode
      && (!body || !dep_between_memop_and_curr (body)))
    return true;

  return false;

}


/* Implement FINAL_PRESCAN_INSN.  */

void
aarch64_final_prescan_insn (rtx_insn *insn)
{
  if (aarch64_madd_needs_nop (insn))
    fprintf (asm_out_file, "\tnop // between mem op and mult-accumulate\n");
}


/* Return true if BASE_OR_STEP is a valid immediate operand for an SVE INDEX
   instruction.  */

bool
aarch64_sve_index_immediate_p (rtx base_or_step)
{
  return (CONST_INT_P (base_or_step)
	  && IN_RANGE (INTVAL (base_or_step), -16, 15));
}

/* Return true if X is a valid immediate for the SVE ADD and SUB
   instructions.  Negate X first if NEGATE_P is true.  */

bool
aarch64_sve_arith_immediate_p (rtx x, bool negate_p)
{
  rtx elt;

  if (!const_vec_duplicate_p (x, &elt)
      || !CONST_INT_P (elt))
    return false;

  HOST_WIDE_INT val = INTVAL (elt);
  if (negate_p)
    val = -val;
  val &= GET_MODE_MASK (GET_MODE_INNER (GET_MODE (x)));

  if (val & 0xff)
    return IN_RANGE (val, 0, 0xff);
  return IN_RANGE (val, 0, 0xff00);
}

/* Return true if X is a valid immediate for the SVE SQADD and SQSUB
   instructions.  Negate X first if NEGATE_P is true.  */

bool
aarch64_sve_sqadd_sqsub_immediate_p (rtx x, bool negate_p)
{
  rtx elt;

  if (!const_vec_duplicate_p (x, &elt)
      || !CONST_INT_P (elt))
    return false;

  if (!aarch64_sve_arith_immediate_p (x, negate_p))
    return false;

  /* After the optional negation, the immediate must be nonnegative.
     E.g. a saturating add of -127 must be done via SQSUB Zn.B, Zn.B, #127
     instead of SQADD Zn.B, Zn.B, #129.  */
  return negate_p == (INTVAL (elt) < 0);
}

/* Return true if X is a valid immediate operand for an SVE logical
   instruction such as AND.  */

bool
aarch64_sve_bitmask_immediate_p (rtx x)
{
  rtx elt;

  return (const_vec_duplicate_p (x, &elt)
	  && CONST_INT_P (elt)
	  && aarch64_bitmask_imm (INTVAL (elt),
				  GET_MODE_INNER (GET_MODE (x))));
}

/* Return true if X is a valid immediate for the SVE DUP and CPY
   instructions.  */

bool
aarch64_sve_dup_immediate_p (rtx x)
{
  x = aarch64_bit_representation (unwrap_const_vec_duplicate (x));
  if (!CONST_INT_P (x))
    return false;

  HOST_WIDE_INT val = INTVAL (x);
  if (val & 0xff)
    return IN_RANGE (val, -0x80, 0x7f);
  return IN_RANGE (val, -0x8000, 0x7f00);
}

/* Return true if X is a valid immediate operand for an SVE CMP instruction.
   SIGNED_P says whether the operand is signed rather than unsigned.  */

bool
aarch64_sve_cmp_immediate_p (rtx x, bool signed_p)
{
  x = unwrap_const_vec_duplicate (x);
  return (CONST_INT_P (x)
	  && (signed_p
	      ? IN_RANGE (INTVAL (x), -16, 15)
	      : IN_RANGE (INTVAL (x), 0, 127)));
}

/* Return true if X is a valid immediate operand for an SVE FADD or FSUB
   instruction.  Negate X first if NEGATE_P is true.  */

bool
aarch64_sve_float_arith_immediate_p (rtx x, bool negate_p)
{
  rtx elt;
  REAL_VALUE_TYPE r;

  if (!const_vec_duplicate_p (x, &elt)
      || GET_CODE (elt) != CONST_DOUBLE)
    return false;

  r = *CONST_DOUBLE_REAL_VALUE (elt);

  if (negate_p)
    r = real_value_negate (&r);

  if (real_equal (&r, &dconst1))
    return true;
  if (real_equal (&r, &dconsthalf))
    return true;
  return false;
}

/* Return true if X is a valid immediate operand for an SVE FMUL
   instruction.  */

bool
aarch64_sve_float_mul_immediate_p (rtx x)
{
  rtx elt;

  return (const_vec_duplicate_p (x, &elt)
	  && GET_CODE (elt) == CONST_DOUBLE
	  && (real_equal (CONST_DOUBLE_REAL_VALUE (elt), &dconsthalf)
	      || real_equal (CONST_DOUBLE_REAL_VALUE (elt), &dconst2)));
}

/* Return true if replicating VAL32 is a valid 2-byte or 4-byte immediate
   for the Advanced SIMD operation described by WHICH and INSN.  If INFO
   is nonnull, use it to describe valid immediates.  */
static bool
aarch64_advsimd_valid_immediate_hs (unsigned int val32,
				    simd_immediate_info *info,
				    enum simd_immediate_check which,
				    simd_immediate_info::insn_type insn)
{
  /* Try a 4-byte immediate with LSL.  */
  for (unsigned int shift = 0; shift < 32; shift += 8)
    if ((val32 & (0xff << shift)) == val32)
      {
	if (info)
	  *info = simd_immediate_info (SImode, val32 >> shift, insn,
				       simd_immediate_info::LSL, shift);
	return true;
      }

  /* Try a 2-byte immediate with LSL.  */
  unsigned int imm16 = val32 & 0xffff;
  if (imm16 == (val32 >> 16))
    for (unsigned int shift = 0; shift < 16; shift += 8)
      if ((imm16 & (0xff << shift)) == imm16)
	{
	  if (info)
	    *info = simd_immediate_info (HImode, imm16 >> shift, insn,
					 simd_immediate_info::LSL, shift);
	  return true;
	}

  /* Try a 4-byte immediate with MSL, except for cases that MVN
     can handle.  */
  if (which == AARCH64_CHECK_MOV)
    for (unsigned int shift = 8; shift < 24; shift += 8)
      {
	unsigned int low = (1 << shift) - 1;
	if (((val32 & (0xff << shift)) | low) == val32)
	  {
	    if (info)
	      *info = simd_immediate_info (SImode, val32 >> shift, insn,
					   simd_immediate_info::MSL, shift);
	    return true;
	  }
      }

  return false;
}

/* Return true if replicating VAL64 is a valid immediate for the
   Advanced SIMD operation described by WHICH.  If INFO is nonnull,
   use it to describe valid immediates.  */
static bool
aarch64_advsimd_valid_immediate (unsigned HOST_WIDE_INT val64,
				 simd_immediate_info *info,
				 enum simd_immediate_check which)
{
  unsigned int val32 = val64 & 0xffffffff;
  unsigned int val16 = val64 & 0xffff;
  unsigned int val8 = val64 & 0xff;

  if (val32 == (val64 >> 32))
    {
      if ((which & AARCH64_CHECK_ORR) != 0
	  && aarch64_advsimd_valid_immediate_hs (val32, info, which,
						 simd_immediate_info::MOV))
	return true;

      if ((which & AARCH64_CHECK_BIC) != 0
	  && aarch64_advsimd_valid_immediate_hs (~val32, info, which,
						 simd_immediate_info::MVN))
	return true;

      /* Try using a replicated byte.  */
      if (which == AARCH64_CHECK_MOV
	  && val16 == (val32 >> 16)
	  && val8 == (val16 >> 8))
	{
	  if (info)
	    *info = simd_immediate_info (QImode, val8);
	  return true;
	}
    }

  /* Try using a bit-to-bytemask.  */
  if (which == AARCH64_CHECK_MOV)
    {
      unsigned int i;
      for (i = 0; i < 64; i += 8)
	{
	  unsigned char byte = (val64 >> i) & 0xff;
	  if (byte != 0 && byte != 0xff)
	    break;
	}
      if (i == 64)
	{
	  if (info)
	    *info = simd_immediate_info (DImode, val64);
	  return true;
	}
    }
  return false;
}

/* Return true if replicating VAL64 gives a valid immediate for an SVE MOV
   instruction.  If INFO is nonnull, use it to describe valid immediates.  */

static bool
aarch64_sve_valid_immediate (unsigned HOST_WIDE_INT val64,
			     simd_immediate_info *info)
{
  scalar_int_mode mode = DImode;
  unsigned int val32 = val64 & 0xffffffff;
  if (val32 == (val64 >> 32))
    {
      mode = SImode;
      unsigned int val16 = val32 & 0xffff;
      if (val16 == (val32 >> 16))
	{
	  mode = HImode;
	  unsigned int val8 = val16 & 0xff;
	  if (val8 == (val16 >> 8))
	    mode = QImode;
	}
    }
  HOST_WIDE_INT val = trunc_int_for_mode (val64, mode);
  if (IN_RANGE (val, -0x80, 0x7f))
    {
      /* DUP with no shift.  */
      if (info)
	*info = simd_immediate_info (mode, val);
      return true;
    }
  if ((val & 0xff) == 0 && IN_RANGE (val, -0x8000, 0x7f00))
    {
      /* DUP with LSL #8.  */
      if (info)
	*info = simd_immediate_info (mode, val);
      return true;
    }
  if (aarch64_bitmask_imm (val64, mode))
    {
      /* DUPM.  */
      if (info)
	*info = simd_immediate_info (mode, val);
      return true;
    }
  return false;
}

/* Return true if X is an UNSPEC_PTRUE constant of the form:

       (const (unspec [PATTERN ZERO] UNSPEC_PTRUE))

   where PATTERN is the svpattern as a CONST_INT and where ZERO
   is a zero constant of the required PTRUE mode (which can have
   fewer elements than X's mode, if zero bits are significant).

   If so, and if INFO is nonnull, describe the immediate in INFO.  */
bool
aarch64_sve_ptrue_svpattern_p (rtx x, struct simd_immediate_info *info)
{
  if (GET_CODE (x) != CONST)
    return false;

  x = XEXP (x, 0);
  if (GET_CODE (x) != UNSPEC || XINT (x, 1) != UNSPEC_PTRUE)
    return false;

  if (info)
    {
      aarch64_svpattern pattern
	= (aarch64_svpattern) INTVAL (XVECEXP (x, 0, 0));
      machine_mode pred_mode = GET_MODE (XVECEXP (x, 0, 1));
      scalar_int_mode int_mode = aarch64_sve_element_int_mode (pred_mode);
      *info = simd_immediate_info (int_mode, pattern);
    }
  return true;
}

/* Return true if X is a valid SVE predicate.  If INFO is nonnull, use
   it to describe valid immediates.  */

static bool
aarch64_sve_pred_valid_immediate (rtx x, simd_immediate_info *info)
{
  if (aarch64_sve_ptrue_svpattern_p (x, info))
    return true;

  if (x == CONST0_RTX (GET_MODE (x)))
    {
      if (info)
	*info = simd_immediate_info (DImode, 0);
      return true;
    }

  /* Analyze the value as a VNx16BImode.  This should be relatively
     efficient, since rtx_vector_builder has enough built-in capacity
     to store all VLA predicate constants without needing the heap.  */
  rtx_vector_builder builder;
  if (!aarch64_get_sve_pred_bits (builder, x))
    return false;

  unsigned int elt_size = aarch64_widest_sve_pred_elt_size (builder);
  if (int vl = aarch64_partial_ptrue_length (builder, elt_size))
    {
      machine_mode mode = aarch64_sve_pred_mode (elt_size).require ();
      aarch64_svpattern pattern = aarch64_svpattern_for_vl (mode, vl);
      if (pattern != AARCH64_NUM_SVPATTERNS)
	{
	  if (info)
	    {
	      scalar_int_mode int_mode = aarch64_sve_element_int_mode (mode);
	      *info = simd_immediate_info (int_mode, pattern);
	    }
	  return true;
	}
    }
  return false;
}

/* Return true if OP is a valid SIMD immediate for the operation
   described by WHICH.  If INFO is nonnull, use it to describe valid
   immediates.  */
bool
aarch64_simd_valid_immediate (rtx op, simd_immediate_info *info,
			      enum simd_immediate_check which)
{
  machine_mode mode = GET_MODE (op);
  unsigned int vec_flags = aarch64_classify_vector_mode (mode);
  if (vec_flags == 0 || vec_flags == (VEC_ADVSIMD | VEC_STRUCT))
    return false;

  if (vec_flags & VEC_SVE_PRED)
    return aarch64_sve_pred_valid_immediate (op, info);

  scalar_mode elt_mode = GET_MODE_INNER (mode);
  rtx base, step;
  unsigned int n_elts;
  if (GET_CODE (op) == CONST_VECTOR
      && CONST_VECTOR_DUPLICATE_P (op))
    n_elts = CONST_VECTOR_NPATTERNS (op);
  else if ((vec_flags & VEC_SVE_DATA)
	   && const_vec_series_p (op, &base, &step))
    {
      gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
      if (!aarch64_sve_index_immediate_p (base)
	  || !aarch64_sve_index_immediate_p (step))
	return false;

      if (info)
	{
	  /* Get the corresponding container mode.  E.g. an INDEX on V2SI
	     should yield two integer values per 128-bit block, meaning
	     that we need to treat it in the same way as V2DI and then
	     ignore the upper 32 bits of each element.  */
	  elt_mode = aarch64_sve_container_int_mode (mode);
	  *info = simd_immediate_info (elt_mode, base, step);
	}
      return true;
    }
  else if (GET_CODE (op) == CONST_VECTOR
	   && CONST_VECTOR_NUNITS (op).is_constant (&n_elts))
    /* N_ELTS set above.  */;
  else
    return false;

  scalar_float_mode elt_float_mode;
  if (n_elts == 1
      && is_a <scalar_float_mode> (elt_mode, &elt_float_mode))
    {
      rtx elt = CONST_VECTOR_ENCODED_ELT (op, 0);
      if (aarch64_float_const_zero_rtx_p (elt)
	  || aarch64_float_const_representable_p (elt))
	{
	  if (info)
	    *info = simd_immediate_info (elt_float_mode, elt);
	  return true;
	}
    }

  unsigned int elt_size = GET_MODE_SIZE (elt_mode);
  if (elt_size > 8)
    return false;

  scalar_int_mode elt_int_mode = int_mode_for_mode (elt_mode).require ();

  /* Expand the vector constant out into a byte vector, with the least
     significant byte of the register first.  */
  auto_vec<unsigned char, 16> bytes;
  bytes.reserve (n_elts * elt_size);
  for (unsigned int i = 0; i < n_elts; i++)
    {
      /* The vector is provided in gcc endian-neutral fashion.
	 For aarch64_be Advanced SIMD, it must be laid out in the vector
	 register in reverse order.  */
      bool swap_p = ((vec_flags & VEC_ADVSIMD) != 0 && BYTES_BIG_ENDIAN);
      rtx elt = CONST_VECTOR_ELT (op, swap_p ? (n_elts - 1 - i) : i);

      if (elt_mode != elt_int_mode)
	elt = gen_lowpart (elt_int_mode, elt);

      if (!CONST_INT_P (elt))
	return false;

      unsigned HOST_WIDE_INT elt_val = INTVAL (elt);
      for (unsigned int byte = 0; byte < elt_size; byte++)
	{
	  bytes.quick_push (elt_val & 0xff);
	  elt_val >>= BITS_PER_UNIT;
	}
    }

  /* The immediate must repeat every eight bytes.  */
  unsigned int nbytes = bytes.length ();
  for (unsigned i = 8; i < nbytes; ++i)
    if (bytes[i] != bytes[i - 8])
      return false;

  /* Get the repeating 8-byte value as an integer.  No endian correction
     is needed here because bytes is already in lsb-first order.  */
  unsigned HOST_WIDE_INT val64 = 0;
  for (unsigned int i = 0; i < 8; i++)
    val64 |= ((unsigned HOST_WIDE_INT) bytes[i % nbytes]
	      << (i * BITS_PER_UNIT));

  if (vec_flags & VEC_SVE_DATA)
    return aarch64_sve_valid_immediate (val64, info);
  else
    return aarch64_advsimd_valid_immediate (val64, info, which);
}

/* Check whether X is a VEC_SERIES-like constant that starts at 0 and
   has a step in the range of INDEX.  Return the index expression if so,
   otherwise return null.  */
rtx
aarch64_check_zero_based_sve_index_immediate (rtx x)
{
  rtx base, step;
  if (const_vec_series_p (x, &base, &step)
      && base == const0_rtx
      && aarch64_sve_index_immediate_p (step))
    return step;
  return NULL_RTX;
}

/* Check of immediate shift constants are within range.  */
bool
aarch64_simd_shift_imm_p (rtx x, machine_mode mode, bool left)
{
  x = unwrap_const_vec_duplicate (x);
  if (!CONST_INT_P (x))
    return false;
  int bit_width = GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT;
  if (left)
    return IN_RANGE (INTVAL (x), 0, bit_width - 1);
  else
    return IN_RANGE (INTVAL (x), 1, bit_width);
}

/* Return the bitmask CONST_INT to select the bits required by a zero extract
   operation of width WIDTH at bit position POS.  */

rtx
aarch64_mask_from_zextract_ops (rtx width, rtx pos)
{
  gcc_assert (CONST_INT_P (width));
  gcc_assert (CONST_INT_P (pos));

  unsigned HOST_WIDE_INT mask
    = ((unsigned HOST_WIDE_INT) 1 << UINTVAL (width)) - 1;
  return GEN_INT (mask << UINTVAL (pos));
}

bool
aarch64_mov_operand_p (rtx x, machine_mode mode)
{
  if (GET_CODE (x) == HIGH
      && aarch64_valid_symref (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
    return true;

  if (CONST_INT_P (x))
    return true;

  if (VECTOR_MODE_P (GET_MODE (x)))
    {
      /* Require predicate constants to be VNx16BI before RA, so that we
	 force everything to have a canonical form.  */
      if (!lra_in_progress
	  && !reload_completed
	  && GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_BOOL
	  && GET_MODE (x) != VNx16BImode)
	return false;

      return aarch64_simd_valid_immediate (x, NULL);
    }

  if (GET_CODE (x) == SYMBOL_REF && mode == DImode && CONSTANT_ADDRESS_P (x))
    return true;

  if (TARGET_SVE && aarch64_sve_cnt_immediate_p (x))
    return true;

  return aarch64_classify_symbolic_expression (x)
    == SYMBOL_TINY_ABSOLUTE;
}

/* Return a const_int vector of VAL.  */
rtx
aarch64_simd_gen_const_vector_dup (machine_mode mode, HOST_WIDE_INT val)
{
  rtx c = gen_int_mode (val, GET_MODE_INNER (mode));
  return gen_const_vec_duplicate (mode, c);
}

/* Check OP is a legal scalar immediate for the MOVI instruction.  */

bool
aarch64_simd_scalar_immediate_valid_for_move (rtx op, scalar_int_mode mode)
{
  machine_mode vmode;

  vmode = aarch64_simd_container_mode (mode, 64);
  rtx op_v = aarch64_simd_gen_const_vector_dup (vmode, INTVAL (op));
  return aarch64_simd_valid_immediate (op_v, NULL);
}

/* Construct and return a PARALLEL RTX vector with elements numbering the
   lanes of either the high (HIGH == TRUE) or low (HIGH == FALSE) half of
   the vector - from the perspective of the architecture.  This does not
   line up with GCC's perspective on lane numbers, so we end up with
   different masks depending on our target endian-ness.  The diagram
   below may help.  We must draw the distinction when building masks
   which select one half of the vector.  An instruction selecting
   architectural low-lanes for a big-endian target, must be described using
   a mask selecting GCC high-lanes.

                 Big-Endian             Little-Endian

GCC             0   1   2   3           3   2   1   0
              | x | x | x | x |       | x | x | x | x |
Architecture    3   2   1   0           3   2   1   0

Low Mask:         { 2, 3 }                { 0, 1 }
High Mask:        { 0, 1 }                { 2, 3 }

   MODE Is the mode of the vector and NUNITS is the number of units in it.  */

rtx
aarch64_simd_vect_par_cnst_half (machine_mode mode, int nunits, bool high)
{
  rtvec v = rtvec_alloc (nunits / 2);
  int high_base = nunits / 2;
  int low_base = 0;
  int base;
  rtx t1;
  int i;

  if (BYTES_BIG_ENDIAN)
    base = high ? low_base : high_base;
  else
    base = high ? high_base : low_base;

  for (i = 0; i < nunits / 2; i++)
    RTVEC_ELT (v, i) = GEN_INT (base + i);

  t1 = gen_rtx_PARALLEL (mode, v);
  return t1;
}

/* Check OP for validity as a PARALLEL RTX vector with elements
   numbering the lanes of either the high (HIGH == TRUE) or low lanes,
   from the perspective of the architecture.  See the diagram above
   aarch64_simd_vect_par_cnst_half for more details.  */

bool
aarch64_simd_check_vect_par_cnst_half (rtx op, machine_mode mode,
				       bool high)
{
  int nelts;
  if (!VECTOR_MODE_P (mode) || !GET_MODE_NUNITS (mode).is_constant (&nelts))
    return false;

  rtx ideal = aarch64_simd_vect_par_cnst_half (mode, nelts, high);
  HOST_WIDE_INT count_op = XVECLEN (op, 0);
  HOST_WIDE_INT count_ideal = XVECLEN (ideal, 0);
  int i = 0;

  if (count_op != count_ideal)
    return false;

  for (i = 0; i < count_ideal; i++)
    {
      rtx elt_op = XVECEXP (op, 0, i);
      rtx elt_ideal = XVECEXP (ideal, 0, i);

      if (!CONST_INT_P (elt_op)
	  || INTVAL (elt_ideal) != INTVAL (elt_op))
	return false;
    }
  return true;
}

/* Return a PARALLEL containing NELTS elements, with element I equal
   to BASE + I * STEP.  */

rtx
aarch64_gen_stepped_int_parallel (unsigned int nelts, int base, int step)
{
  rtvec vec = rtvec_alloc (nelts);
  for (unsigned int i = 0; i < nelts; ++i)
    RTVEC_ELT (vec, i) = gen_int_mode (base + i * step, DImode);
  return gen_rtx_PARALLEL (VOIDmode, vec);
}

/* Return true if OP is a PARALLEL of CONST_INTs that form a linear
   series with step STEP.  */

bool
aarch64_stepped_int_parallel_p (rtx op, int step)
{
  if (GET_CODE (op) != PARALLEL || !CONST_INT_P (XVECEXP (op, 0, 0)))
    return false;

  unsigned HOST_WIDE_INT base = UINTVAL (XVECEXP (op, 0, 0));
  for (int i = 1; i < XVECLEN (op, 0); ++i)
    if (!CONST_INT_P (XVECEXP (op, 0, i))
	|| UINTVAL (XVECEXP (op, 0, i)) != base + i * step)
      return false;

  return true;
}

/* Bounds-check lanes.  Ensure OPERAND lies between LOW (inclusive) and
   HIGH (exclusive).  */
void
aarch64_simd_lane_bounds (rtx operand, HOST_WIDE_INT low, HOST_WIDE_INT high,
			  const_tree exp)
{
  HOST_WIDE_INT lane;
  gcc_assert (CONST_INT_P (operand));
  lane = INTVAL (operand);

  if (lane < low || lane >= high)
  {
    if (exp)
      error ("%Klane %wd out of range %wd - %wd", exp, lane, low, high - 1);
    else
      error ("lane %wd out of range %wd - %wd", lane, low, high - 1);
  }
}

/* Peform endian correction on lane number N, which indexes a vector
   of mode MODE, and return the result as an SImode rtx.  */

rtx
aarch64_endian_lane_rtx (machine_mode mode, unsigned int n)
{
  return gen_int_mode (ENDIAN_LANE_N (GET_MODE_NUNITS (mode), n), SImode);
}

/* Return TRUE if OP is a valid vector addressing mode.  */

bool
aarch64_simd_mem_operand_p (rtx op)
{
  return MEM_P (op) && (GET_CODE (XEXP (op, 0)) == POST_INC
			|| REG_P (XEXP (op, 0)));
}

/* Return true if OP is a valid MEM operand for an SVE LD1R instruction.  */

bool
aarch64_sve_ld1r_operand_p (rtx op)
{
  struct aarch64_address_info addr;
  scalar_mode mode;

  return (MEM_P (op)
	  && is_a <scalar_mode> (GET_MODE (op), &mode)
	  && aarch64_classify_address (&addr, XEXP (op, 0), mode, false)
	  && addr.type == ADDRESS_REG_IMM
	  && offset_6bit_unsigned_scaled_p (mode, addr.const_offset));
}

/* Return true if OP is a valid MEM operand for an SVE LD1RQ instruction.  */
bool
aarch64_sve_ld1rq_operand_p (rtx op)
{
  struct aarch64_address_info addr;
  scalar_mode elem_mode = GET_MODE_INNER (GET_MODE (op));
  if (!MEM_P (op)
      || !aarch64_classify_address (&addr, XEXP (op, 0), elem_mode, false))
    return false;

  if (addr.type == ADDRESS_REG_IMM)
    return offset_4bit_signed_scaled_p (TImode, addr.const_offset);

  if (addr.type == ADDRESS_REG_REG)
    return (1U << addr.shift) == GET_MODE_SIZE (elem_mode);

  return false;
}

/* Return true if OP is a valid MEM operand for an SVE LDFF1 instruction.  */
bool
aarch64_sve_ldff1_operand_p (rtx op)
{
  if (!MEM_P (op))
    return false;

  struct aarch64_address_info addr;
  if (!aarch64_classify_address (&addr, XEXP (op, 0), GET_MODE (op), false))
    return false;

  if (addr.type == ADDRESS_REG_IMM)
    return known_eq (addr.const_offset, 0);

  return addr.type == ADDRESS_REG_REG;
}

/* Return true if OP is a valid MEM operand for an SVE LDNF1 instruction.  */
bool
aarch64_sve_ldnf1_operand_p (rtx op)
{
  struct aarch64_address_info addr;

  return (MEM_P (op)
	  && aarch64_classify_address (&addr, XEXP (op, 0),
				       GET_MODE (op), false)
	  && addr.type == ADDRESS_REG_IMM);
}

/* Return true if OP is a valid MEM operand for an SVE LDR instruction.
   The conditions for STR are the same.  */
bool
aarch64_sve_ldr_operand_p (rtx op)
{
  struct aarch64_address_info addr;

  return (MEM_P (op)
	  && aarch64_classify_address (&addr, XEXP (op, 0), GET_MODE (op),
				       false, ADDR_QUERY_ANY)
	  && addr.type == ADDRESS_REG_IMM);
}

/* Return true if OP is a valid address for an SVE PRF[BHWD] instruction,
   addressing memory of mode MODE.  */
bool
aarch64_sve_prefetch_operand_p (rtx op, machine_mode mode)
{
  struct aarch64_address_info addr;
  if (!aarch64_classify_address (&addr, op, mode, false))
    return false;

  if (addr.type == ADDRESS_REG_IMM)
    return known_eq (addr.const_offset, 0);

  return addr.type == ADDRESS_REG_REG;
}

/* Return true if OP is a valid MEM operand for an SVE_STRUCT mode.
   We need to be able to access the individual pieces, so the range
   is different from LD[234] and ST[234].  */
bool
aarch64_sve_struct_memory_operand_p (rtx op)
{
  if (!MEM_P (op))
    return false;

  machine_mode mode = GET_MODE (op);
  struct aarch64_address_info addr;
  if (!aarch64_classify_address (&addr, XEXP (op, 0), SVE_BYTE_MODE, false,
				 ADDR_QUERY_ANY)
      || addr.type != ADDRESS_REG_IMM)
    return false;

  poly_int64 first = addr.const_offset;
  poly_int64 last = first + GET_MODE_SIZE (mode) - BYTES_PER_SVE_VECTOR;
  return (offset_4bit_signed_scaled_p (SVE_BYTE_MODE, first)
	  && offset_4bit_signed_scaled_p (SVE_BYTE_MODE, last));
}

/* Emit a register copy from operand to operand, taking care not to
   early-clobber source registers in the process.

   COUNT is the number of components into which the copy needs to be
   decomposed.  */
void
aarch64_simd_emit_reg_reg_move (rtx *operands, machine_mode mode,
				unsigned int count)
{
  unsigned int i;
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);

  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      || rdest < rsrc)
    for (i = 0; i < count; i++)
      emit_move_insn (gen_rtx_REG (mode, rdest + i),
		      gen_rtx_REG (mode, rsrc + i));
  else
    for (i = 0; i < count; i++)
      emit_move_insn (gen_rtx_REG (mode, rdest + count - i - 1),
		      gen_rtx_REG (mode, rsrc + count - i - 1));
}

/* Compute and return the length of aarch64_simd_reglist<mode>, where <mode> is
   one of VSTRUCT modes: OI, CI, or XI.  */
int
aarch64_simd_attr_length_rglist (machine_mode mode)
{
  /* This is only used (and only meaningful) for Advanced SIMD, not SVE.  */
  return (GET_MODE_SIZE (mode).to_constant () / UNITS_PER_VREG) * 4;
}

/* Implement target hook TARGET_VECTOR_ALIGNMENT.  The AAPCS64 sets the maximum
   alignment of a vector to 128 bits.  SVE predicates have an alignment of
   16 bits.  */
static HOST_WIDE_INT
aarch64_simd_vector_alignment (const_tree type)
{
  /* ??? Checking the mode isn't ideal, but VECTOR_BOOLEAN_TYPE_P can
     be set for non-predicate vectors of booleans.  Modes are the most
     direct way we have of identifying real SVE predicate types.  */
  if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_VECTOR_BOOL)
    return 16;
  widest_int min_size
    = constant_lower_bound (wi::to_poly_widest (TYPE_SIZE (type)));
  return wi::umin (min_size, 128).to_uhwi ();
}

/* Implement target hook TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT.  */
static poly_uint64
aarch64_vectorize_preferred_vector_alignment (const_tree type)
{
  if (aarch64_sve_data_mode_p (TYPE_MODE (type)))
    {
      /* If the length of the vector is fixed, try to align to that length,
	 otherwise don't try to align at all.  */
      HOST_WIDE_INT result;
      if (!BITS_PER_SVE_VECTOR.is_constant (&result))
	result = TYPE_ALIGN (TREE_TYPE (type));
      return result;
    }
  return TYPE_ALIGN (type);
}

/* Implement target hook TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE.  */
static bool
aarch64_simd_vector_alignment_reachable (const_tree type, bool is_packed)
{
  if (is_packed)
    return false;

  /* For fixed-length vectors, check that the vectorizer will aim for
     full-vector alignment.  This isn't true for generic GCC vectors
     that are wider than the ABI maximum of 128 bits.  */
  poly_uint64 preferred_alignment =
    aarch64_vectorize_preferred_vector_alignment (type);
  if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
      && maybe_ne (wi::to_widest (TYPE_SIZE (type)),
		   preferred_alignment))
    return false;

  /* Vectors whose size is <= BIGGEST_ALIGNMENT are naturally aligned.  */
  return true;
}

/* Return true if the vector misalignment factor is supported by the
   target.  */
static bool
aarch64_builtin_support_vector_misalignment (machine_mode mode,
					     const_tree type, int misalignment,
					     bool is_packed)
{
  if (TARGET_SIMD && STRICT_ALIGNMENT)
    {
      /* Return if movmisalign pattern is not supported for this mode.  */
      if (optab_handler (movmisalign_optab, mode) == CODE_FOR_nothing)
        return false;

      /* Misalignment factor is unknown at compile time.  */
      if (misalignment == -1)
	return false;
    }
  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

/* If VALS is a vector constant that can be loaded into a register
   using DUP, generate instructions to do so and return an RTX to
   assign to the register.  Otherwise return NULL_RTX.  */
static rtx
aarch64_simd_dup_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx x;

  if (!const_vec_duplicate_p (vals, &x))
    return NULL_RTX;

  /* We can load this constant by using DUP and a constant in a
     single ARM register.  This will be cheaper than a vector
     load.  */
  x = copy_to_mode_reg (inner_mode, x);
  return gen_vec_duplicate (mode, x);
}


/* Generate code to load VALS, which is a PARALLEL containing only
   constants (for vec_init) or CONST_VECTOR, efficiently into a
   register.  Returns an RTX to copy into the register, or NULL_RTX
   for a PARALLEL that cannot be converted into a CONST_VECTOR.  */
static rtx
aarch64_simd_make_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  rtx const_dup;
  rtx const_vec = NULL_RTX;
  int n_const = 0;
  int i;

  if (GET_CODE (vals) == CONST_VECTOR)
    const_vec = vals;
  else if (GET_CODE (vals) == PARALLEL)
    {
      /* A CONST_VECTOR must contain only CONST_INTs and
	 CONST_DOUBLEs, but CONSTANT_P allows more (e.g. SYMBOL_REF).
	 Only store valid constants in a CONST_VECTOR.  */
      int n_elts = XVECLEN (vals, 0);
      for (i = 0; i < n_elts; ++i)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	    n_const++;
	}
      if (n_const == n_elts)
	const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
    }
  else
    gcc_unreachable ();

  if (const_vec != NULL_RTX
      && aarch64_simd_valid_immediate (const_vec, NULL))
    /* Load using MOVI/MVNI.  */
    return const_vec;
  else if ((const_dup = aarch64_simd_dup_constant (vals)) != NULL_RTX)
    /* Loaded using DUP.  */
    return const_dup;
  else if (const_vec != NULL_RTX)
    /* Load from constant pool. We cannot take advantage of single-cycle
       LD1 because we need a PC-relative addressing mode.  */
    return const_vec;
  else
    /* A PARALLEL containing something not valid inside CONST_VECTOR.
       We cannot construct an initializer.  */
    return NULL_RTX;
}

/* Expand a vector initialisation sequence, such that TARGET is
   initialised to contain VALS.  */

void
aarch64_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode inner_mode = GET_MODE_INNER (mode);
  /* The number of vector elements.  */
  int n_elts = XVECLEN (vals, 0);
  /* The number of vector elements which are not constant.  */
  int n_var = 0;
  rtx any_const = NULL_RTX;
  /* The first element of vals.  */
  rtx v0 = XVECEXP (vals, 0, 0);
  bool all_same = true;

  /* This is a special vec_init<M><N> where N is not an element mode but a
     vector mode with half the elements of M.  We expect to find two entries
     of mode N in VALS and we must put their concatentation into TARGET.  */
  if (XVECLEN (vals, 0) == 2 && VECTOR_MODE_P (GET_MODE (XVECEXP (vals, 0, 0))))
    {
      gcc_assert (known_eq (GET_MODE_SIZE (mode),
		  2 * GET_MODE_SIZE (GET_MODE (XVECEXP (vals, 0, 0)))));
      rtx lo = XVECEXP (vals, 0, 0);
      rtx hi = XVECEXP (vals, 0, 1);
      machine_mode narrow_mode = GET_MODE (lo);
      gcc_assert (GET_MODE_INNER (narrow_mode) == inner_mode);
      gcc_assert (narrow_mode == GET_MODE (hi));

      /* When we want to concatenate a half-width vector with zeroes we can
	 use the aarch64_combinez[_be] patterns.  Just make sure that the
	 zeroes are in the right half.  */
      if (BYTES_BIG_ENDIAN
	  && aarch64_simd_imm_zero (lo, narrow_mode)
	  && general_operand (hi, narrow_mode))
	emit_insn (gen_aarch64_combinez_be (narrow_mode, target, hi, lo));
      else if (!BYTES_BIG_ENDIAN
	       && aarch64_simd_imm_zero (hi, narrow_mode)
	       && general_operand (lo, narrow_mode))
	emit_insn (gen_aarch64_combinez (narrow_mode, target, lo, hi));
      else
	{
	  /* Else create the two half-width registers and combine them.  */
	  if (!REG_P (lo))
	    lo = force_reg (GET_MODE (lo), lo);
	  if (!REG_P (hi))
	    hi = force_reg (GET_MODE (hi), hi);

	  if (BYTES_BIG_ENDIAN)
	    std::swap (lo, hi);
	  emit_insn (gen_aarch64_simd_combine (narrow_mode, target, lo, hi));
	}
     return;
   }

  /* Count the number of variable elements to initialise.  */
  for (int i = 0; i < n_elts; ++i)
    {
      rtx x = XVECEXP (vals, 0, i);
      if (!(CONST_INT_P (x) || CONST_DOUBLE_P (x)))
	++n_var;
      else
	any_const = x;

      all_same &= rtx_equal_p (x, v0);
    }

  /* No variable elements, hand off to aarch64_simd_make_constant which knows
     how best to handle this.  */
  if (n_var == 0)
    {
      rtx constant = aarch64_simd_make_constant (vals);
      if (constant != NULL_RTX)
	{
	  emit_move_insn (target, constant);
	  return;
	}
    }

  /* Splat a single non-constant element if we can.  */
  if (all_same)
    {
      rtx x = copy_to_mode_reg (inner_mode, v0);
      aarch64_emit_move (target, gen_vec_duplicate (mode, x));
      return;
    }

  enum insn_code icode = optab_handler (vec_set_optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);

  /* If there are only variable elements, try to optimize
     the insertion using dup for the most common element
     followed by insertions.  */

  /* The algorithm will fill matches[*][0] with the earliest matching element,
     and matches[X][1] with the count of duplicate elements (if X is the
     earliest element which has duplicates).  */

  if (n_var == n_elts && n_elts <= 16)
    {
      int matches[16][2] = {0};
      for (int i = 0; i < n_elts; i++)
	{
	  for (int j = 0; j <= i; j++)
	    {
	      if (rtx_equal_p (XVECEXP (vals, 0, i), XVECEXP (vals, 0, j)))
		{
		  matches[i][0] = j;
		  matches[j][1]++;
		  break;
		}
	    }
	}
      int maxelement = 0;
      int maxv = 0;
      for (int i = 0; i < n_elts; i++)
	if (matches[i][1] > maxv)
	  {
	    maxelement = i;
	    maxv = matches[i][1];
	  }

      /* Create a duplicate of the most common element, unless all elements
	 are equally useless to us, in which case just immediately set the
	 vector register using the first element.  */

      if (maxv == 1)
	{
	  /* For vectors of two 64-bit elements, we can do even better.  */
	  if (n_elts == 2
	      && (inner_mode == E_DImode
		  || inner_mode == E_DFmode))

	    {
	      rtx x0 = XVECEXP (vals, 0, 0);
	      rtx x1 = XVECEXP (vals, 0, 1);
	      /* Combine can pick up this case, but handling it directly
		 here leaves clearer RTL.

		 This is load_pair_lanes<mode>, and also gives us a clean-up
		 for store_pair_lanes<mode>.  */
	      if (memory_operand (x0, inner_mode)
		  && memory_operand (x1, inner_mode)
		  && !STRICT_ALIGNMENT
		  && rtx_equal_p (XEXP (x1, 0),
				  plus_constant (Pmode,
						 XEXP (x0, 0),
						 GET_MODE_SIZE (inner_mode))))
		{
		  rtx t;
		  if (inner_mode == DFmode)
		    t = gen_load_pair_lanesdf (target, x0, x1);
		  else
		    t = gen_load_pair_lanesdi (target, x0, x1);
		  emit_insn (t);
		  return;
		}
	    }
	  /* The subreg-move sequence below will move into lane zero of the
	     vector register.  For big-endian we want that position to hold
	     the last element of VALS.  */
	  maxelement = BYTES_BIG_ENDIAN ? n_elts - 1 : 0;
	  rtx x = copy_to_mode_reg (inner_mode, XVECEXP (vals, 0, maxelement));
	  aarch64_emit_move (target, lowpart_subreg (mode, x, inner_mode));
	}
      else
	{
	  rtx x = copy_to_mode_reg (inner_mode, XVECEXP (vals, 0, maxelement));
	  aarch64_emit_move (target, gen_vec_duplicate (mode, x));
	}

      /* Insert the rest.  */
      for (int i = 0; i < n_elts; i++)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (matches[i][0] == maxelement)
	    continue;
	  x = copy_to_mode_reg (inner_mode, x);
	  emit_insn (GEN_FCN (icode) (target, x, GEN_INT (i)));
	}
      return;
    }

  /* Initialise a vector which is part-variable.  We want to first try
     to build those lanes which are constant in the most efficient way we
     can.  */
  if (n_var != n_elts)
    {
      rtx copy = copy_rtx (vals);

      /* Load constant part of vector.  We really don't care what goes into the
	 parts we will overwrite, but we're more likely to be able to load the
	 constant efficiently if it has fewer, larger, repeating parts
	 (see aarch64_simd_valid_immediate).  */
      for (int i = 0; i < n_elts; i++)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	    continue;
	  rtx subst = any_const;
	  for (int bit = n_elts / 2; bit > 0; bit /= 2)
	    {
	      /* Look in the copied vector, as more elements are const.  */
	      rtx test = XVECEXP (copy, 0, i ^ bit);
	      if (CONST_INT_P (test) || CONST_DOUBLE_P (test))
		{
		  subst = test;
		  break;
		}
	    }
	  XVECEXP (copy, 0, i) = subst;
	}
      aarch64_expand_vector_init (target, copy);
    }

  /* Insert the variable lanes directly.  */
  for (int i = 0; i < n_elts; i++)
    {
      rtx x = XVECEXP (vals, 0, i);
      if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	continue;
      x = copy_to_mode_reg (inner_mode, x);
      emit_insn (GEN_FCN (icode) (target, x, GEN_INT (i)));
    }
}

/* Emit RTL corresponding to:
   insr TARGET, ELEM.  */

static void
emit_insr (rtx target, rtx elem)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);
  elem = force_reg (elem_mode, elem);

  insn_code icode = optab_handler (vec_shl_insert_optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);
  emit_insn (GEN_FCN (icode) (target, target, elem));
}

/* Subroutine of aarch64_sve_expand_vector_init for handling
   trailing constants.
   This function works as follows:
   (a) Create a new vector consisting of trailing constants.
   (b) Initialize TARGET with the constant vector using emit_move_insn.
   (c) Insert remaining elements in TARGET using insr.
   NELTS is the total number of elements in original vector while
   while NELTS_REQD is the number of elements that are actually
   significant.

   ??? The heuristic used is to do above only if number of constants
   is at least half the total number of elements.  May need fine tuning.  */

static bool
aarch64_sve_expand_vector_init_handle_trailing_constants
 (rtx target, const rtx_vector_builder &builder, int nelts, int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);
  int n_trailing_constants = 0;

  for (int i = nelts_reqd - 1;
       i >= 0 && aarch64_legitimate_constant_p (elem_mode, builder.elt (i));
       i--)
    n_trailing_constants++;

  if (n_trailing_constants >= nelts_reqd / 2)
    {
      rtx_vector_builder v (mode, 1, nelts);
      for (int i = 0; i < nelts; i++)
	v.quick_push (builder.elt (i + nelts_reqd - n_trailing_constants));
      rtx const_vec = v.build ();
      emit_move_insn (target, const_vec);

      for (int i = nelts_reqd - n_trailing_constants - 1; i >= 0; i--)
	emit_insr (target, builder.elt (i));

      return true;
    }

  return false;
}

/* Subroutine of aarch64_sve_expand_vector_init.
   Works as follows:
   (a) Initialize TARGET by broadcasting element NELTS_REQD - 1 of BUILDER.
   (b) Skip trailing elements from BUILDER, which are the same as
       element NELTS_REQD - 1.
   (c) Insert earlier elements in reverse order in TARGET using insr.  */

static void
aarch64_sve_expand_vector_init_insert_elems (rtx target,
					     const rtx_vector_builder &builder,
					     int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);

  struct expand_operand ops[2];
  enum insn_code icode = optab_handler (vec_duplicate_optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);

  create_output_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], builder.elt (nelts_reqd - 1), elem_mode);
  expand_insn (icode, 2, ops);

  int ndups = builder.count_dups (nelts_reqd - 1, -1, -1);
  for (int i = nelts_reqd - ndups - 1; i >= 0; i--)
    emit_insr (target, builder.elt (i));
}

/* Subroutine of aarch64_sve_expand_vector_init to handle case
   when all trailing elements of builder are same.
   This works as follows:
   (a) Use expand_insn interface to broadcast last vector element in TARGET.
   (b) Insert remaining elements in TARGET using insr.

   ??? The heuristic used is to do above if number of same trailing elements
   is at least 3/4 of total number of elements, loosely based on
   heuristic from mostly_zeros_p.  May need fine-tuning.  */

static bool
aarch64_sve_expand_vector_init_handle_trailing_same_elem
 (rtx target, const rtx_vector_builder &builder, int nelts_reqd)
{
  int ndups = builder.count_dups (nelts_reqd - 1, -1, -1);
  if (ndups >= (3 * nelts_reqd) / 4)
    {
      aarch64_sve_expand_vector_init_insert_elems (target, builder,
						   nelts_reqd - ndups + 1);
      return true;
    }

  return false;
}

/* Initialize register TARGET from BUILDER. NELTS is the constant number
   of elements in BUILDER.

   The function tries to initialize TARGET from BUILDER if it fits one
   of the special cases outlined below.

   Failing that, the function divides BUILDER into two sub-vectors:
   v_even = even elements of BUILDER;
   v_odd = odd elements of BUILDER;

   and recursively calls itself with v_even and v_odd.

   if (recursive call succeeded for v_even or v_odd)
     TARGET = zip (v_even, v_odd)

   The function returns true if it managed to build TARGET from BUILDER
   with one of the special cases, false otherwise.

   Example: {a, 1, b, 2, c, 3, d, 4}

   The vector gets divided into:
   v_even = {a, b, c, d}
   v_odd = {1, 2, 3, 4}

   aarch64_sve_expand_vector_init(v_odd) hits case 1 and
   initialize tmp2 from constant vector v_odd using emit_move_insn.

   aarch64_sve_expand_vector_init(v_even) fails since v_even contains
   4 elements, so we construct tmp1 from v_even using insr:
   tmp1 = dup(d)
   insr tmp1, c
   insr tmp1, b
   insr tmp1, a

   And finally:
   TARGET = zip (tmp1, tmp2)
   which sets TARGET to {a, 1, b, 2, c, 3, d, 4}.  */

static bool
aarch64_sve_expand_vector_init (rtx target, const rtx_vector_builder &builder,
				int nelts, int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);

  /* Case 1: Vector contains trailing constants.  */

  if (aarch64_sve_expand_vector_init_handle_trailing_constants
       (target, builder, nelts, nelts_reqd))
    return true;

  /* Case 2: Vector contains leading constants.  */

  rtx_vector_builder rev_builder (mode, 1, nelts_reqd);
  for (int i = 0; i < nelts_reqd; i++)
    rev_builder.quick_push (builder.elt (nelts_reqd - i - 1));
  rev_builder.finalize ();

  if (aarch64_sve_expand_vector_init_handle_trailing_constants
       (target, rev_builder, nelts, nelts_reqd))
    {
      emit_insn (gen_aarch64_sve_rev (mode, target, target));
      return true;
    }

  /* Case 3: Vector contains trailing same element.  */

  if (aarch64_sve_expand_vector_init_handle_trailing_same_elem
       (target, builder, nelts_reqd))
    return true;

  /* Case 4: Vector contains leading same element.  */

  if (aarch64_sve_expand_vector_init_handle_trailing_same_elem
       (target, rev_builder, nelts_reqd) && nelts_reqd == nelts)
    {
      emit_insn (gen_aarch64_sve_rev (mode, target, target));
      return true;
    }

  /* Avoid recursing below 4-elements.
     ??? The threshold 4 may need fine-tuning.  */

  if (nelts_reqd <= 4)
    return false;

  rtx_vector_builder v_even (mode, 1, nelts);
  rtx_vector_builder v_odd (mode, 1, nelts);

  for (int i = 0; i < nelts * 2; i += 2)
    {
      v_even.quick_push (builder.elt (i));
      v_odd.quick_push (builder.elt (i + 1));
    }

  v_even.finalize ();
  v_odd.finalize ();

  rtx tmp1 = gen_reg_rtx (mode);
  bool did_even_p = aarch64_sve_expand_vector_init (tmp1, v_even,
						    nelts, nelts_reqd / 2);

  rtx tmp2 = gen_reg_rtx (mode);
  bool did_odd_p = aarch64_sve_expand_vector_init (tmp2, v_odd,
						   nelts, nelts_reqd / 2);

  if (!did_even_p && !did_odd_p)
    return false;

  /* Initialize v_even and v_odd using INSR if it didn't match any of the
     special cases and zip v_even, v_odd.  */

  if (!did_even_p)
    aarch64_sve_expand_vector_init_insert_elems (tmp1, v_even, nelts_reqd / 2);

  if (!did_odd_p)
    aarch64_sve_expand_vector_init_insert_elems (tmp2, v_odd, nelts_reqd / 2);

  rtvec v = gen_rtvec (2, tmp1, tmp2);
  emit_set_insn (target, gen_rtx_UNSPEC (mode, v, UNSPEC_ZIP1));
  return true;
}

/* Initialize register TARGET from the elements in PARALLEL rtx VALS.  */

void
aarch64_sve_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  int nelts = XVECLEN (vals, 0);

  rtx_vector_builder v (mode, 1, nelts);
  for (int i = 0; i < nelts; i++)
    v.quick_push (XVECEXP (vals, 0, i));
  v.finalize ();

  /* If neither sub-vectors of v could be initialized specially,
     then use INSR to insert all elements from v into TARGET.
     ??? This might not be optimal for vectors with large
     initializers like 16-element or above.
     For nelts < 4, it probably isn't useful to handle specially.  */

  if (nelts < 4
      || !aarch64_sve_expand_vector_init (target, v, nelts, nelts))
    aarch64_sve_expand_vector_init_insert_elems (target, v, nelts);
}

/* Check whether VALUE is a vector constant in which every element
   is either a power of 2 or a negated power of 2.  If so, return
   a constant vector of log2s, and flip CODE between PLUS and MINUS
   if VALUE contains negated powers of 2.  Return NULL_RTX otherwise.  */

static rtx
aarch64_convert_mult_to_shift (rtx value, rtx_code &code)
{
  if (GET_CODE (value) != CONST_VECTOR)
    return NULL_RTX;

  rtx_vector_builder builder;
  if (!builder.new_unary_operation (GET_MODE (value), value, false))
    return NULL_RTX;

  scalar_mode int_mode = GET_MODE_INNER (GET_MODE (value));
  /* 1 if the result of the multiplication must be negated,
     0 if it mustn't, or -1 if we don't yet care.  */
  int negate = -1;
  unsigned int encoded_nelts = const_vector_encoded_nelts (value);
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    {
      rtx elt = CONST_VECTOR_ENCODED_ELT (value, i);
      if (!CONST_SCALAR_INT_P (elt))
	return NULL_RTX;
      rtx_mode_t val (elt, int_mode);
      wide_int pow2 = wi::neg (val);
      if (val != pow2)
	{
	  /* It matters whether we negate or not.  Make that choice,
	     and make sure that it's consistent with previous elements.  */
	  if (negate == !wi::neg_p (val))
	    return NULL_RTX;
	  negate = wi::neg_p (val);
	  if (!negate)
	    pow2 = val;
	}
      /* POW2 is now the value that we want to be a power of 2.  */
      int shift = wi::exact_log2 (pow2);
      if (shift < 0)
	return NULL_RTX;
      builder.quick_push (gen_int_mode (shift, int_mode));
    }
  if (negate == -1)
    /* PLUS and MINUS are equivalent; canonicalize on PLUS.  */
    code = PLUS;
  else if (negate == 1)
    code = code == PLUS ? MINUS : PLUS;
  return builder.build ();
}

/* Prepare for an integer SVE multiply-add or multiply-subtract pattern;
   CODE is PLUS for the former and MINUS for the latter.  OPERANDS is the
   operands array, in the same order as for fma_optab.  Return true if
   the function emitted all the necessary instructions, false if the caller
   should generate the pattern normally with the new OPERANDS array.  */

bool
aarch64_prepare_sve_int_fma (rtx *operands, rtx_code code)
{
  machine_mode mode = GET_MODE (operands[0]);
  if (rtx shifts = aarch64_convert_mult_to_shift (operands[2], code))
    {
      rtx product = expand_binop (mode, vashl_optab, operands[1], shifts,
				  NULL_RTX, true, OPTAB_DIRECT);
      force_expand_binop (mode, code == PLUS ? add_optab : sub_optab,
			  operands[3], product, operands[0], true,
			  OPTAB_DIRECT);
      return true;
    }
  operands[2] = force_reg (mode, operands[2]);
  return false;
}

/* Likewise, but for a conditional pattern.  */

bool
aarch64_prepare_sve_cond_int_fma (rtx *operands, rtx_code code)
{
  machine_mode mode = GET_MODE (operands[0]);
  if (rtx shifts = aarch64_convert_mult_to_shift (operands[3], code))
    {
      rtx product = expand_binop (mode, vashl_optab, operands[2], shifts,
				  NULL_RTX, true, OPTAB_DIRECT);
      emit_insn (gen_cond (code, mode, operands[0], operands[1],
			   operands[4], product, operands[5]));
      return true;
    }
  operands[3] = force_reg (mode, operands[3]);
  return false;
}

static unsigned HOST_WIDE_INT
aarch64_shift_truncation_mask (machine_mode mode)
{
  if (!SHIFT_COUNT_TRUNCATED || aarch64_vector_data_mode_p (mode))
    return 0;
  return GET_MODE_UNIT_BITSIZE (mode) - 1;
}

/* Select a format to encode pointers in exception handling data.  */
int
aarch64_asm_preferred_eh_data_format (int code ATTRIBUTE_UNUSED, int global)
{
   int type;
   switch (aarch64_cmodel)
     {
     case AARCH64_CMODEL_TINY:
     case AARCH64_CMODEL_TINY_PIC:
     case AARCH64_CMODEL_SMALL:
     case AARCH64_CMODEL_SMALL_PIC:
     case AARCH64_CMODEL_SMALL_SPIC:
       /* text+got+data < 4Gb.  4-byte signed relocs are sufficient
	  for everything.  */
       type = DW_EH_PE_sdata4;
       break;
     default:
       /* No assumptions here.  8-byte relocs required.  */
       type = DW_EH_PE_sdata8;
       break;
     }
   return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
}

/* Output .variant_pcs for aarch64_vector_pcs function symbols.  */

static void
aarch64_asm_output_variant_pcs (FILE *stream, const tree decl, const char* name)
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      arm_pcs pcs = (arm_pcs) fndecl_abi (decl).id ();
      if (pcs == ARM_PCS_SIMD || pcs == ARM_PCS_SVE)
	{
	  fprintf (stream, "\t.variant_pcs\t");
	  assemble_name (stream, name);
	  fprintf (stream, "\n");
	}
    }
}

/* The last .arch and .tune assembly strings that we printed.  */
static std::string aarch64_last_printed_arch_string;
static std::string aarch64_last_printed_tune_string;

/* Implement ASM_DECLARE_FUNCTION_NAME.  Output the ISA features used
   by the function fndecl.  */

void
aarch64_declare_function_name (FILE *stream, const char* name,
				tree fndecl)
{
  tree target_parts = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);

  struct cl_target_option *targ_options;
  if (target_parts)
    targ_options = TREE_TARGET_OPTION (target_parts);
  else
    targ_options = TREE_TARGET_OPTION (target_option_current_node);
  gcc_assert (targ_options);

  const struct processor *this_arch
    = aarch64_get_arch (targ_options->x_explicit_arch);

  uint64_t isa_flags = targ_options->x_aarch64_isa_flags;
  std::string extension
    = aarch64_get_extension_string_for_isa_flags (isa_flags,
						  this_arch->flags);
  /* Only update the assembler .arch string if it is distinct from the last
     such string we printed.  */
  std::string to_print = this_arch->name + extension;
  if (to_print != aarch64_last_printed_arch_string)
    {
      asm_fprintf (asm_out_file, "\t.arch %s\n", to_print.c_str ());
      aarch64_last_printed_arch_string = to_print;
    }

  /* Print the cpu name we're tuning for in the comments, might be
     useful to readers of the generated asm.  Do it only when it changes
     from function to function and verbose assembly is requested.  */
  const struct processor *this_tune
    = aarch64_get_tune_cpu (targ_options->x_explicit_tune_core);

  if (flag_debug_asm && aarch64_last_printed_tune_string != this_tune->name)
    {
      asm_fprintf (asm_out_file, "\t" ASM_COMMENT_START ".tune %s\n",
		   this_tune->name);
      aarch64_last_printed_tune_string = this_tune->name;
    }

  aarch64_asm_output_variant_pcs (stream, fndecl, name);

  /* Don't forget the type directive for ELF.  */
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "function");
  ASM_OUTPUT_LABEL (stream, name);
}

/* Implement ASM_OUTPUT_DEF_FROM_DECLS.  Output .variant_pcs for aliases.  */

void
aarch64_asm_output_alias (FILE *stream, const tree decl, const tree target)
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  const char *value = IDENTIFIER_POINTER (target);
  aarch64_asm_output_variant_pcs (stream, decl, name);
  ASM_OUTPUT_DEF (stream, name, value);
}

/* Implement ASM_OUTPUT_EXTERNAL.  Output .variant_pcs for undefined
   function symbol references.  */

void
aarch64_asm_output_external (FILE *stream, tree decl, const char* name)
{
  default_elf_asm_output_external (stream, decl, name);
  aarch64_asm_output_variant_pcs (stream, decl, name);
}

/* Triggered after a .cfi_startproc directive is emitted into the assembly file.
   Used to output the .cfi_b_key_frame directive when signing the current
   function with the B key.  */

void
aarch64_post_cfi_startproc (FILE *f, tree ignored ATTRIBUTE_UNUSED)
{
  if (cfun->machine->frame.laid_out && aarch64_return_address_signing_enabled ()
      && aarch64_ra_sign_key == AARCH64_KEY_B)
	asm_fprintf (f, "\t.cfi_b_key_frame\n");
}

/* Implements TARGET_ASM_FILE_START.  Output the assembly header.  */

static void
aarch64_start_file (void)
{
  struct cl_target_option *default_options
    = TREE_TARGET_OPTION (target_option_default_node);

  const struct processor *default_arch
    = aarch64_get_arch (default_options->x_explicit_arch);
  uint64_t default_isa_flags = default_options->x_aarch64_isa_flags;
  std::string extension
    = aarch64_get_extension_string_for_isa_flags (default_isa_flags,
						  default_arch->flags);

   aarch64_last_printed_arch_string = default_arch->name + extension;
   aarch64_last_printed_tune_string = "";
   asm_fprintf (asm_out_file, "\t.arch %s\n",
		aarch64_last_printed_arch_string.c_str ());

   default_file_start ();
}

/* Emit load exclusive.  */

static void
aarch64_emit_load_exclusive (machine_mode mode, rtx rval,
			     rtx mem, rtx model_rtx)
{
  if (mode == TImode)
    emit_insn (gen_aarch64_load_exclusive_pair (gen_lowpart (DImode, rval),
						gen_highpart (DImode, rval),
						mem, model_rtx));
  else
    emit_insn (gen_aarch64_load_exclusive (mode, rval, mem, model_rtx));
}

/* Emit store exclusive.  */

static void
aarch64_emit_store_exclusive (machine_mode mode, rtx bval,
			      rtx mem, rtx rval, rtx model_rtx)
{
  if (mode == TImode)
    emit_insn (gen_aarch64_store_exclusive_pair
	       (bval, mem, operand_subword (rval, 0, 0, TImode),
		operand_subword (rval, 1, 0, TImode), model_rtx));
  else
    emit_insn (gen_aarch64_store_exclusive (mode, bval, mem, rval, model_rtx));
}

/* Mark the previous jump instruction as unlikely.  */

static void
aarch64_emit_unlikely_jump (rtx insn)
{
  rtx_insn *jump = emit_jump_insn (insn);
  add_reg_br_prob_note (jump, profile_probability::very_unlikely ());
}

/* We store the names of the various atomic helpers in a 5x4 array.
   Return the libcall function given MODE, MODEL and NAMES.  */

rtx
aarch64_atomic_ool_func(machine_mode mode, rtx model_rtx,
			const atomic_ool_names *names)
{
  memmodel model = memmodel_base (INTVAL (model_rtx));
  int mode_idx, model_idx;

  switch (mode)
    {
    case E_QImode:
      mode_idx = 0;
      break;
    case E_HImode:
      mode_idx = 1;
      break;
    case E_SImode:
      mode_idx = 2;
      break;
    case E_DImode:
      mode_idx = 3;
      break;
    case E_TImode:
      mode_idx = 4;
      break;
    default:
      gcc_unreachable ();
    }

  switch (model)
    {
    case MEMMODEL_RELAXED:
      model_idx = 0;
      break;
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
      model_idx = 1;
      break;
    case MEMMODEL_RELEASE:
      model_idx = 2;
      break;
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      model_idx = 3;
      break;
    default:
      gcc_unreachable ();
    }

  return init_one_libfunc_visibility (names->str[mode_idx][model_idx],
				      VISIBILITY_HIDDEN);
}

#define DEF0(B, N) \
  { "__aarch64_" #B #N "_relax", \
    "__aarch64_" #B #N "_acq", \
    "__aarch64_" #B #N "_rel", \
    "__aarch64_" #B #N "_acq_rel" }

#define DEF4(B)  DEF0(B, 1), DEF0(B, 2), DEF0(B, 4), DEF0(B, 8), \
		 { NULL, NULL, NULL, NULL }
#define DEF5(B)  DEF0(B, 1), DEF0(B, 2), DEF0(B, 4), DEF0(B, 8), DEF0(B, 16)

static const atomic_ool_names aarch64_ool_cas_names = { { DEF5(cas) } };
const atomic_ool_names aarch64_ool_swp_names = { { DEF4(swp) } };
const atomic_ool_names aarch64_ool_ldadd_names = { { DEF4(ldadd) } };
const atomic_ool_names aarch64_ool_ldset_names = { { DEF4(ldset) } };
const atomic_ool_names aarch64_ool_ldclr_names = { { DEF4(ldclr) } };
const atomic_ool_names aarch64_ool_ldeor_names = { { DEF4(ldeor) } };

#undef DEF0
#undef DEF4
#undef DEF5

/* Expand a compare and swap pattern.  */

void
aarch64_expand_compare_and_swap (rtx operands[])
{
  rtx bval, rval, mem, oldval, newval, is_weak, mod_s, mod_f, x, cc_reg;
  machine_mode mode, r_mode;

  bval = operands[0];
  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);

  /* Normally the succ memory model must be stronger than fail, but in the
     unlikely event of fail being ACQUIRE and succ being RELEASE we need to
     promote succ to ACQ_REL so that we don't lose the acquire semantics.  */
  if (is_mm_acquire (memmodel_from_int (INTVAL (mod_f)))
      && is_mm_release (memmodel_from_int (INTVAL (mod_s))))
    mod_s = GEN_INT (MEMMODEL_ACQ_REL);

  r_mode = mode;
  if (mode == QImode || mode == HImode)
    {
      r_mode = SImode;
      rval = gen_reg_rtx (r_mode);
    }

  if (TARGET_LSE)
    {
      /* The CAS insn requires oldval and rval overlap, but we need to
	 have a copy of oldval saved across the operation to tell if
	 the operation is successful.  */
      if (reg_overlap_mentioned_p (rval, oldval))
        rval = copy_to_mode_reg (r_mode, oldval);
      else
	emit_move_insn (rval, gen_lowpart (r_mode, oldval));

      emit_insn (gen_aarch64_compare_and_swap_lse (mode, rval, mem,
						   newval, mod_s));
      cc_reg = aarch64_gen_compare_reg_maybe_ze (NE, rval, oldval, mode);
    }
  else if (TARGET_OUTLINE_ATOMICS)
    {
      /* Oldval must satisfy compare afterward.  */
      if (!aarch64_plus_operand (oldval, mode))
	oldval = force_reg (mode, oldval);
      rtx func = aarch64_atomic_ool_func (mode, mod_s, &aarch64_ool_cas_names);
      rval = emit_library_call_value (func, NULL_RTX, LCT_NORMAL, r_mode,
				      oldval, mode, newval, mode,
				      XEXP (mem, 0), Pmode);
      cc_reg = aarch64_gen_compare_reg_maybe_ze (NE, rval, oldval, mode);
    }
  else
    {
      /* The oldval predicate varies by mode.  Test it and force to reg.  */
      insn_code code = code_for_aarch64_compare_and_swap (mode);
      if (!insn_data[code].operand[2].predicate (oldval, mode))
	oldval = force_reg (mode, oldval);

      emit_insn (GEN_FCN (code) (rval, mem, oldval, newval,
				 is_weak, mod_s, mod_f));
      cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);
    }

  if (r_mode != mode)
    rval = gen_lowpart (mode, rval);
  emit_move_insn (operands[1], rval);

  x = gen_rtx_EQ (SImode, cc_reg, const0_rtx);
  emit_insn (gen_rtx_SET (bval, x));
}

/* Emit a barrier, that is appropriate for memory model MODEL, at the end of a
   sequence implementing an atomic operation.  */

static void
aarch64_emit_post_barrier (enum memmodel model)
{
  const enum memmodel base_model = memmodel_base (model);

  if (is_mm_sync (model)
      && (base_model == MEMMODEL_ACQUIRE
	  || base_model == MEMMODEL_ACQ_REL
	  || base_model == MEMMODEL_SEQ_CST))
    {
      emit_insn (gen_mem_thread_fence (GEN_INT (MEMMODEL_SEQ_CST)));
    }
}

/* Split a compare and swap pattern.  */

void
aarch64_split_compare_and_swap (rtx operands[])
{
  rtx rval, mem, oldval, newval, scratch, x, model_rtx;
  machine_mode mode;
  bool is_weak;
  rtx_code_label *label1, *label2;
  enum memmodel model;

  rval = operands[0];
  mem = operands[1];
  oldval = operands[2];
  newval = operands[3];
  is_weak = (operands[4] != const0_rtx);
  model_rtx = operands[5];
  scratch = operands[7];
  mode = GET_MODE (mem);
  model = memmodel_from_int (INTVAL (model_rtx));

  /* When OLDVAL is zero and we want the strong version we can emit a tighter
    loop:
    .label1:
	LD[A]XR	rval, [mem]
	CBNZ	rval, .label2
	ST[L]XR	scratch, newval, [mem]
	CBNZ	scratch, .label1
    .label2:
	CMP	rval, 0.  */
  bool strong_zero_p = (!is_weak && !aarch64_track_speculation &&
			oldval == const0_rtx && mode != TImode);

  label1 = NULL;
  if (!is_weak)
    {
      label1 = gen_label_rtx ();
      emit_label (label1);
    }
  label2 = gen_label_rtx ();

  /* The initial load can be relaxed for a __sync operation since a final
     barrier will be emitted to stop code hoisting.  */
  if (is_mm_sync (model))
    aarch64_emit_load_exclusive (mode, rval, mem, GEN_INT (MEMMODEL_RELAXED));
  else
    aarch64_emit_load_exclusive (mode, rval, mem, model_rtx);

  if (strong_zero_p)
    x = gen_rtx_NE (VOIDmode, rval, const0_rtx);
  else
    {
      rtx cc_reg = aarch64_gen_compare_reg_maybe_ze (NE, rval, oldval, mode);
      x = gen_rtx_NE (VOIDmode, cc_reg, const0_rtx);
    }
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label2), pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  aarch64_emit_store_exclusive (mode, scratch, mem, newval, model_rtx);

  if (!is_weak)
    {
      if (aarch64_track_speculation)
	{
	  /* Emit an explicit compare instruction, so that we can correctly
	     track the condition codes.  */
	  rtx cc_reg = aarch64_gen_compare_reg (NE, scratch, const0_rtx);
	  x = gen_rtx_NE (GET_MODE (cc_reg), cc_reg, const0_rtx);
	}
      else
	x = gen_rtx_NE (VOIDmode, scratch, const0_rtx);

      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, label1), pc_rtx);
      aarch64_emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
    }
  else
    aarch64_gen_compare_reg (NE, scratch, const0_rtx);

  emit_label (label2);

  /* If we used a CBNZ in the exchange loop emit an explicit compare with RVAL
     to set the condition flags.  If this is not used it will be removed by
     later passes.  */
  if (strong_zero_p)
    aarch64_gen_compare_reg (NE, rval, const0_rtx);

  /* Emit any final barrier needed for a __sync operation.  */
  if (is_mm_sync (model))
    aarch64_emit_post_barrier (model);
}

/* Split an atomic operation.  */

void
aarch64_split_atomic_op (enum rtx_code code, rtx old_out, rtx new_out, rtx mem,
			 rtx value, rtx model_rtx, rtx cond)
{
  machine_mode mode = GET_MODE (mem);
  machine_mode wmode = (mode == DImode ? DImode : SImode);
  const enum memmodel model = memmodel_from_int (INTVAL (model_rtx));
  const bool is_sync = is_mm_sync (model);
  rtx_code_label *label;
  rtx x;

  /* Split the atomic operation into a sequence.  */
  label = gen_label_rtx ();
  emit_label (label);

  if (new_out)
    new_out = gen_lowpart (wmode, new_out);
  if (old_out)
    old_out = gen_lowpart (wmode, old_out);
  else
    old_out = new_out;
  value = simplify_gen_subreg (wmode, value, mode, 0);

  /* The initial load can be relaxed for a __sync operation since a final
     barrier will be emitted to stop code hoisting.  */
 if (is_sync)
    aarch64_emit_load_exclusive (mode, old_out, mem,
				 GEN_INT (MEMMODEL_RELAXED));
  else
    aarch64_emit_load_exclusive (mode, old_out, mem, model_rtx);

  switch (code)
    {
    case SET:
      new_out = value;
      break;

    case NOT:
      x = gen_rtx_AND (wmode, old_out, value);
      emit_insn (gen_rtx_SET (new_out, x));
      x = gen_rtx_NOT (wmode, new_out);
      emit_insn (gen_rtx_SET (new_out, x));
      break;

    case MINUS:
      if (CONST_INT_P (value))
	{
	  value = GEN_INT (-INTVAL (value));
	  code = PLUS;
	}
      /* Fall through.  */

    default:
      x = gen_rtx_fmt_ee (code, wmode, old_out, value);
      emit_insn (gen_rtx_SET (new_out, x));
      break;
    }

  aarch64_emit_store_exclusive (mode, cond, mem,
				gen_lowpart (mode, new_out), model_rtx);

  if (aarch64_track_speculation)
    {
      /* Emit an explicit compare instruction, so that we can correctly
	 track the condition codes.  */
      rtx cc_reg = aarch64_gen_compare_reg (NE, cond, const0_rtx);
      x = gen_rtx_NE (GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
  else
    x = gen_rtx_NE (VOIDmode, cond, const0_rtx);

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label), pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  /* Emit any final barrier needed for a __sync operation.  */
  if (is_sync)
    aarch64_emit_post_barrier (model);
}

static void
aarch64_init_libfuncs (void)
{
   /* Half-precision float operations.  The compiler handles all operations
     with NULL libfuncs by converting to SFmode.  */

  /* Conversions.  */
  set_conv_libfunc (trunc_optab, HFmode, SFmode, "__gnu_f2h_ieee");
  set_conv_libfunc (sext_optab, SFmode, HFmode, "__gnu_h2f_ieee");

  /* Arithmetic.  */
  set_optab_libfunc (add_optab, HFmode, NULL);
  set_optab_libfunc (sdiv_optab, HFmode, NULL);
  set_optab_libfunc (smul_optab, HFmode, NULL);
  set_optab_libfunc (neg_optab, HFmode, NULL);
  set_optab_libfunc (sub_optab, HFmode, NULL);

  /* Comparisons.  */
  set_optab_libfunc (eq_optab, HFmode, NULL);
  set_optab_libfunc (ne_optab, HFmode, NULL);
  set_optab_libfunc (lt_optab, HFmode, NULL);
  set_optab_libfunc (le_optab, HFmode, NULL);
  set_optab_libfunc (ge_optab, HFmode, NULL);
  set_optab_libfunc (gt_optab, HFmode, NULL);
  set_optab_libfunc (unord_optab, HFmode, NULL);
}

/* Target hook for c_mode_for_suffix.  */
static machine_mode
aarch64_c_mode_for_suffix (char suffix)
{
  if (suffix == 'q')
    return TFmode;

  return VOIDmode;
}

/* We can only represent floating point constants which will fit in
   "quarter-precision" values.  These values are characterised by
   a sign bit, a 4-bit mantissa and a 3-bit exponent.  And are given
   by:

   (-1)^s * (n/16) * 2^r

   Where:
     's' is the sign bit.
     'n' is an integer in the range 16 <= n <= 31.
     'r' is an integer in the range -3 <= r <= 4.  */

/* Return true iff X can be represented by a quarter-precision
   floating point immediate operand X.  Note, we cannot represent 0.0.  */
bool
aarch64_float_const_representable_p (rtx x)
{
  /* This represents our current view of how many bits
     make up the mantissa.  */
  int point_pos = 2 * HOST_BITS_PER_WIDE_INT - 1;
  int exponent;
  unsigned HOST_WIDE_INT mantissa, mask;
  REAL_VALUE_TYPE r, m;
  bool fail;

  x = unwrap_const_vec_duplicate (x);
  if (!CONST_DOUBLE_P (x))
    return false;

  if (GET_MODE (x) == VOIDmode
      || (GET_MODE (x) == HFmode && !TARGET_FP_F16INST))
    return false;

  r = *CONST_DOUBLE_REAL_VALUE (x);

  /* We cannot represent infinities, NaNs or +/-zero.  We won't
     know if we have +zero until we analyse the mantissa, but we
     can reject the other invalid values.  */
  if (REAL_VALUE_ISINF (r) || REAL_VALUE_ISNAN (r)
      || REAL_VALUE_MINUS_ZERO (r))
    return false;

  /* Extract exponent.  */
  r = real_value_abs (&r);
  exponent = REAL_EXP (&r);

  /* For the mantissa, we expand into two HOST_WIDE_INTS, apart from the
     highest (sign) bit, with a fixed binary point at bit point_pos.
     m1 holds the low part of the mantissa, m2 the high part.
     WARNING: If we ever have a representation using more than 2 * H_W_I - 1
     bits for the mantissa, this can fail (low bits will be lost).  */
  real_ldexp (&m, &r, point_pos - exponent);
  wide_int w = real_to_integer (&m, &fail, HOST_BITS_PER_WIDE_INT * 2);

  /* If the low part of the mantissa has bits set we cannot represent
     the value.  */
  if (w.ulow () != 0)
    return false;
  /* We have rejected the lower HOST_WIDE_INT, so update our
     understanding of how many bits lie in the mantissa and
     look only at the high HOST_WIDE_INT.  */
  mantissa = w.elt (1);
  point_pos -= HOST_BITS_PER_WIDE_INT;

  /* We can only represent values with a mantissa of the form 1.xxxx.  */
  mask = ((unsigned HOST_WIDE_INT)1 << (point_pos - 5)) - 1;
  if ((mantissa & mask) != 0)
    return false;

  /* Having filtered unrepresentable values, we may now remove all
     but the highest 5 bits.  */
  mantissa >>= point_pos - 5;

  /* We cannot represent the value 0.0, so reject it.  This is handled
     elsewhere.  */
  if (mantissa == 0)
    return false;

  /* Then, as bit 4 is always set, we can mask it off, leaving
     the mantissa in the range [0, 15].  */
  mantissa &= ~(1 << 4);
  gcc_assert (mantissa <= 15);

  /* GCC internally does not use IEEE754-like encoding (where normalized
     significands are in the range [1, 2).  GCC uses [0.5, 1) (see real.c).
     Our mantissa values are shifted 4 places to the left relative to
     normalized IEEE754 so we must modify the exponent returned by REAL_EXP
     by 5 places to correct for GCC's representation.  */
  exponent = 5 - exponent;

  return (exponent >= 0 && exponent <= 7);
}

/* Returns the string with the instruction for AdvSIMD MOVI, MVNI, ORR or BIC
   immediate with a CONST_VECTOR of MODE and WIDTH.  WHICH selects whether to
   output MOVI/MVNI, ORR or BIC immediate.  */
char*
aarch64_output_simd_mov_immediate (rtx const_vector, unsigned width,
				   enum simd_immediate_check which)
{
  bool is_valid;
  static char templ[40];
  const char *mnemonic;
  const char *shift_op;
  unsigned int lane_count = 0;
  char element_char;

  struct simd_immediate_info info;

  /* This will return true to show const_vector is legal for use as either
     a AdvSIMD MOVI instruction (or, implicitly, MVNI), ORR or BIC immediate.
     It will also update INFO to show how the immediate should be generated.
     WHICH selects whether to check for MOVI/MVNI, ORR or BIC.  */
  is_valid = aarch64_simd_valid_immediate (const_vector, &info, which);
  gcc_assert (is_valid);

  element_char = sizetochar (GET_MODE_BITSIZE (info.elt_mode));
  lane_count = width / GET_MODE_BITSIZE (info.elt_mode);

  if (GET_MODE_CLASS (info.elt_mode) == MODE_FLOAT)
    {
      gcc_assert (info.insn == simd_immediate_info::MOV
		  && info.u.mov.shift == 0);
      /* For FP zero change it to a CONST_INT 0 and use the integer SIMD
	 move immediate path.  */
      if (aarch64_float_const_zero_rtx_p (info.u.mov.value))
        info.u.mov.value = GEN_INT (0);
      else
	{
	  const unsigned int buf_size = 20;
	  char float_buf[buf_size] = {'\0'};
	  real_to_decimal_for_mode (float_buf,
				    CONST_DOUBLE_REAL_VALUE (info.u.mov.value),
				    buf_size, buf_size, 1, info.elt_mode);

	  if (lane_count == 1)
	    snprintf (templ, sizeof (templ), "fmov\t%%d0, %s", float_buf);
	  else
	    snprintf (templ, sizeof (templ), "fmov\t%%0.%d%c, %s",
		      lane_count, element_char, float_buf);
	  return templ;
	}
    }

  gcc_assert (CONST_INT_P (info.u.mov.value));

  if (which == AARCH64_CHECK_MOV)
    {
      mnemonic = info.insn == simd_immediate_info::MVN ? "mvni" : "movi";
      shift_op = (info.u.mov.modifier == simd_immediate_info::MSL
		  ? "msl" : "lsl");
      if (lane_count == 1)
	snprintf (templ, sizeof (templ), "%s\t%%d0, " HOST_WIDE_INT_PRINT_HEX,
		  mnemonic, UINTVAL (info.u.mov.value));
      else if (info.u.mov.shift)
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, "
		  HOST_WIDE_INT_PRINT_HEX ", %s %d", mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value), shift_op,
		  info.u.mov.shift);
      else
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, "
		  HOST_WIDE_INT_PRINT_HEX, mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value));
    }
  else
    {
      /* For AARCH64_CHECK_BIC and AARCH64_CHECK_ORR.  */
      mnemonic = info.insn == simd_immediate_info::MVN ? "bic" : "orr";
      if (info.u.mov.shift)
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, #"
		  HOST_WIDE_INT_PRINT_DEC ", %s #%d", mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value), "lsl",
		  info.u.mov.shift);
      else
	snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, #"
		  HOST_WIDE_INT_PRINT_DEC, mnemonic, lane_count,
		  element_char, UINTVAL (info.u.mov.value));
    }
  return templ;
}

char*
aarch64_output_scalar_simd_mov_immediate (rtx immediate, scalar_int_mode mode)
{

  /* If a floating point number was passed and we desire to use it in an
     integer mode do the conversion to integer.  */
  if (CONST_DOUBLE_P (immediate) && GET_MODE_CLASS (mode) == MODE_INT)
    {
      unsigned HOST_WIDE_INT ival;
      if (!aarch64_reinterpret_float_as_int (immediate, &ival))
	  gcc_unreachable ();
      immediate = gen_int_mode (ival, mode);
    }

  machine_mode vmode;
  /* use a 64 bit mode for everything except for DI/DF mode, where we use
     a 128 bit vector mode.  */
  int width = GET_MODE_BITSIZE (mode) == 64 ? 128 : 64;

  vmode = aarch64_simd_container_mode (mode, width);
  rtx v_op = aarch64_simd_gen_const_vector_dup (vmode, INTVAL (immediate));
  return aarch64_output_simd_mov_immediate (v_op, width);
}

/* Return the output string to use for moving immediate CONST_VECTOR
   into an SVE register.  */

char *
aarch64_output_sve_mov_immediate (rtx const_vector)
{
  static char templ[40];
  struct simd_immediate_info info;
  char element_char;

  bool is_valid = aarch64_simd_valid_immediate (const_vector, &info);
  gcc_assert (is_valid);

  element_char = sizetochar (GET_MODE_BITSIZE (info.elt_mode));

  machine_mode vec_mode = GET_MODE (const_vector);
  if (aarch64_sve_pred_mode_p (vec_mode))
    {
      static char buf[sizeof ("ptrue\t%0.N, vlNNNNN")];
      if (info.insn == simd_immediate_info::MOV)
	{
	  gcc_assert (info.u.mov.value == const0_rtx);
	  snprintf (buf, sizeof (buf), "pfalse\t%%0.b");
	}
      else
	{
	  gcc_assert (info.insn == simd_immediate_info::PTRUE);
	  unsigned int total_bytes;
	  if (info.u.pattern == AARCH64_SV_ALL
	      && BYTES_PER_SVE_VECTOR.is_constant (&total_bytes))
	    snprintf (buf, sizeof (buf), "ptrue\t%%0.%c, vl%d", element_char,
		      total_bytes / GET_MODE_SIZE (info.elt_mode));
	  else
	    snprintf (buf, sizeof (buf), "ptrue\t%%0.%c, %s", element_char,
		      svpattern_token (info.u.pattern));
	}
      return buf;
    }

  if (info.insn == simd_immediate_info::INDEX)
    {
      snprintf (templ, sizeof (templ), "index\t%%0.%c, #"
		HOST_WIDE_INT_PRINT_DEC ", #" HOST_WIDE_INT_PRINT_DEC,
		element_char, INTVAL (info.u.index.base),
		INTVAL (info.u.index.step));
      return templ;
    }

  if (GET_MODE_CLASS (info.elt_mode) == MODE_FLOAT)
    {
      if (aarch64_float_const_zero_rtx_p (info.u.mov.value))
	info.u.mov.value = GEN_INT (0);
      else
	{
	  const int buf_size = 20;
	  char float_buf[buf_size] = {};
	  real_to_decimal_for_mode (float_buf,
				    CONST_DOUBLE_REAL_VALUE (info.u.mov.value),
				    buf_size, buf_size, 1, info.elt_mode);

	  snprintf (templ, sizeof (templ), "fmov\t%%0.%c, #%s",
		    element_char, float_buf);
	  return templ;
	}
    }

  snprintf (templ, sizeof (templ), "mov\t%%0.%c, #" HOST_WIDE_INT_PRINT_DEC,
	    element_char, INTVAL (info.u.mov.value));
  return templ;
}

/* Return the asm template for a PTRUES.  CONST_UNSPEC is the
   aarch64_sve_ptrue_svpattern_immediate that describes the predicate
   pattern.  */

char *
aarch64_output_sve_ptrues (rtx const_unspec)
{
  static char templ[40];

  struct simd_immediate_info info;
  bool is_valid = aarch64_simd_valid_immediate (const_unspec, &info);
  gcc_assert (is_valid && info.insn == simd_immediate_info::PTRUE);

  char element_char = sizetochar (GET_MODE_BITSIZE (info.elt_mode));
  snprintf (templ, sizeof (templ), "ptrues\t%%0.%c, %s", element_char,
	    svpattern_token (info.u.pattern));
  return templ;
}

/* Split operands into moves from op[1] + op[2] into op[0].  */

void
aarch64_split_combinev16qi (rtx operands[3])
{
  unsigned int dest = REGNO (operands[0]);
  unsigned int src1 = REGNO (operands[1]);
  unsigned int src2 = REGNO (operands[2]);
  machine_mode halfmode = GET_MODE (operands[1]);
  unsigned int halfregs = REG_NREGS (operands[1]);
  rtx destlo, desthi;

  gcc_assert (halfmode == V16QImode);

  if (src1 == dest && src2 == dest + halfregs)
    {
      /* No-op move.  Can't split to nothing; emit something.  */
      emit_note (NOTE_INSN_DELETED);
      return;
    }

  /* Preserve register attributes for variable tracking.  */
  destlo = gen_rtx_REG_offset (operands[0], halfmode, dest, 0);
  desthi = gen_rtx_REG_offset (operands[0], halfmode, dest + halfregs,
			       GET_MODE_SIZE (halfmode));

  /* Special case of reversed high/low parts.  */
  if (reg_overlap_mentioned_p (operands[2], destlo)
      && reg_overlap_mentioned_p (operands[1], desthi))
    {
      emit_insn (gen_xorv16qi3 (operands[1], operands[1], operands[2]));
      emit_insn (gen_xorv16qi3 (operands[2], operands[1], operands[2]));
      emit_insn (gen_xorv16qi3 (operands[1], operands[1], operands[2]));
    }
  else if (!reg_overlap_mentioned_p (operands[2], destlo))
    {
      /* Try to avoid unnecessary moves if part of the result
	 is in the right place already.  */
      if (src1 != dest)
	emit_move_insn (destlo, operands[1]);
      if (src2 != dest + halfregs)
	emit_move_insn (desthi, operands[2]);
    }
  else
    {
      if (src2 != dest + halfregs)
	emit_move_insn (desthi, operands[2]);
      if (src1 != dest)
	emit_move_insn (destlo, operands[1]);
    }
}

/* vec_perm support.  */

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  vec_perm_indices perm;
  machine_mode vmode;
  unsigned int vec_flags;
  bool one_vector_p;
  bool testing_p;
};

/* Generate a variable permutation.  */

static void
aarch64_expand_vec_perm_1 (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  bool one_vector_p = rtx_equal_p (op0, op1);

  gcc_checking_assert (vmode == V8QImode || vmode == V16QImode);
  gcc_checking_assert (GET_MODE (op0) == vmode);
  gcc_checking_assert (GET_MODE (op1) == vmode);
  gcc_checking_assert (GET_MODE (sel) == vmode);
  gcc_checking_assert (TARGET_SIMD);

  if (one_vector_p)
    {
      if (vmode == V8QImode)
	{
	  /* Expand the argument to a V16QI mode by duplicating it.  */
	  rtx pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_aarch64_combinev8qi (pair, op0, op0));
	  emit_insn (gen_aarch64_tbl1v8qi (target, pair, sel));
	}
      else
	{
	  emit_insn (gen_aarch64_tbl1v16qi (target, op0, sel));
	}
    }
  else
    {
      rtx pair;

      if (vmode == V8QImode)
	{
	  pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_aarch64_combinev8qi (pair, op0, op1));
	  emit_insn (gen_aarch64_tbl1v8qi (target, pair, sel));
	}
      else
	{
	  pair = gen_reg_rtx (OImode);
	  emit_insn (gen_aarch64_combinev16qi (pair, op0, op1));
	  emit_insn (gen_aarch64_tbl2v16qi (target, pair, sel));
	}
    }
}

/* Expand a vec_perm with the operands given by TARGET, OP0, OP1 and SEL.
   NELT is the number of elements in the vector.  */

void
aarch64_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel,
			 unsigned int nelt)
{
  machine_mode vmode = GET_MODE (target);
  bool one_vector_p = rtx_equal_p (op0, op1);
  rtx mask;

  /* The TBL instruction does not use a modulo index, so we must take care
     of that ourselves.  */
  mask = aarch64_simd_gen_const_vector_dup (vmode,
      one_vector_p ? nelt - 1 : 2 * nelt - 1);
  sel = expand_simple_binop (vmode, AND, sel, mask, NULL, 0, OPTAB_LIB_WIDEN);

  /* For big-endian, we also need to reverse the index within the vector
     (but not which vector).  */
  if (BYTES_BIG_ENDIAN)
    {
      /* If one_vector_p, mask is a vector of (nelt - 1)'s already.  */
      if (!one_vector_p)
        mask = aarch64_simd_gen_const_vector_dup (vmode, nelt - 1);
      sel = expand_simple_binop (vmode, XOR, sel, mask,
				 NULL, 0, OPTAB_LIB_WIDEN);
    }
  aarch64_expand_vec_perm_1 (target, op0, op1, sel);
}

/* Generate (set TARGET (unspec [OP0 OP1] CODE)).  */

static void
emit_unspec2 (rtx target, int code, rtx op0, rtx op1)
{
  emit_insn (gen_rtx_SET (target,
			  gen_rtx_UNSPEC (GET_MODE (target),
					  gen_rtvec (2, op0, op1), code)));
}

/* Expand an SVE vec_perm with the given operands.  */

void
aarch64_expand_sve_vec_perm (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = GET_MODE (sel);
  /* Enforced by the pattern condition.  */
  int nunits = GET_MODE_NUNITS (sel_mode).to_constant ();

  /* Note: vec_perm indices are supposed to wrap when they go beyond the
     size of the two value vectors, i.e. the upper bits of the indices
     are effectively ignored.  SVE TBL instead produces 0 for any
     out-of-range indices, so we need to modulo all the vec_perm indices
     to ensure they are all in range.  */
  rtx sel_reg = force_reg (sel_mode, sel);

  /* Check if the sel only references the first values vector.  */
  if (GET_CODE (sel) == CONST_VECTOR
      && aarch64_const_vec_all_in_range_p (sel, 0, nunits - 1))
    {
      emit_unspec2 (target, UNSPEC_TBL, op0, sel_reg);
      return;
    }

  /* Check if the two values vectors are the same.  */
  if (rtx_equal_p (op0, op1))
    {
      rtx max_sel = aarch64_simd_gen_const_vector_dup (sel_mode, nunits - 1);
      rtx sel_mod = expand_simple_binop (sel_mode, AND, sel_reg, max_sel,
					 NULL, 0, OPTAB_DIRECT);
      emit_unspec2 (target, UNSPEC_TBL, op0, sel_mod);
      return;
    }

  /* Run TBL on for each value vector and combine the results.  */

  rtx res0 = gen_reg_rtx (data_mode);
  rtx res1 = gen_reg_rtx (data_mode);
  rtx neg_num_elems = aarch64_simd_gen_const_vector_dup (sel_mode, -nunits);
  if (GET_CODE (sel) != CONST_VECTOR
      || !aarch64_const_vec_all_in_range_p (sel, 0, 2 * nunits - 1))
    {
      rtx max_sel = aarch64_simd_gen_const_vector_dup (sel_mode,
						       2 * nunits - 1);
      sel_reg = expand_simple_binop (sel_mode, AND, sel_reg, max_sel,
				     NULL, 0, OPTAB_DIRECT);
    }
  emit_unspec2 (res0, UNSPEC_TBL, op0, sel_reg);
  rtx sel_sub = expand_simple_binop (sel_mode, PLUS, sel_reg, neg_num_elems,
				     NULL, 0, OPTAB_DIRECT);
  emit_unspec2 (res1, UNSPEC_TBL, op1, sel_sub);
  if (GET_MODE_CLASS (data_mode) == MODE_VECTOR_INT)
    emit_insn (gen_rtx_SET (target, gen_rtx_IOR (data_mode, res0, res1)));
  else
    emit_unspec2 (target, UNSPEC_IORF, res0, res1);
}

/* Recognize patterns suitable for the TRN instructions.  */
static bool
aarch64_evpc_trn (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  poly_uint64 nelt = d->perm.length ();
  rtx out, in0, in1, x;
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  if (!d->perm[0].is_constant (&odd)
      || (odd != 0 && odd != 1)
      || !d->perm.series_p (0, 2, odd, 2)
      || !d->perm.series_p (1, 2, nelt + odd, 2))
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  /* We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && d->vec_flags == VEC_ADVSIMD)
    {
      x = in0, in0 = in1, in1 = x;
      odd = !odd;
    }
  out = d->target;

  emit_set_insn (out, gen_rtx_UNSPEC (vmode, gen_rtvec (2, in0, in1),
				      odd ? UNSPEC_TRN2 : UNSPEC_TRN1));
  return true;
}

/* Recognize patterns suitable for the UZP instructions.  */
static bool
aarch64_evpc_uzp (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  rtx out, in0, in1, x;
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  if (!d->perm[0].is_constant (&odd)
      || (odd != 0 && odd != 1)
      || !d->perm.series_p (0, 1, odd, 2))
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  /* We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && d->vec_flags == VEC_ADVSIMD)
    {
      x = in0, in0 = in1, in1 = x;
      odd = !odd;
    }
  out = d->target;

  emit_set_insn (out, gen_rtx_UNSPEC (vmode, gen_rtvec (2, in0, in1),
				      odd ? UNSPEC_UZP2 : UNSPEC_UZP1));
  return true;
}

/* Recognize patterns suitable for the ZIP instructions.  */
static bool
aarch64_evpc_zip (struct expand_vec_perm_d *d)
{
  unsigned int high;
  poly_uint64 nelt = d->perm.length ();
  rtx out, in0, in1, x;
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  poly_uint64 first = d->perm[0];
  if ((maybe_ne (first, 0U) && maybe_ne (first * 2, nelt))
      || !d->perm.series_p (0, 2, first, 1)
      || !d->perm.series_p (1, 2, first + nelt, 1))
    return false;
  high = maybe_ne (first, 0U);

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  /* We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && d->vec_flags == VEC_ADVSIMD)
    {
      x = in0, in0 = in1, in1 = x;
      high = !high;
    }
  out = d->target;

  emit_set_insn (out, gen_rtx_UNSPEC (vmode, gen_rtvec (2, in0, in1),
				      high ? UNSPEC_ZIP2 : UNSPEC_ZIP1));
  return true;
}

/* Recognize patterns for the EXT insn.  */

static bool
aarch64_evpc_ext (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT location;
  rtx offset;

  /* The first element always refers to the first vector.
     Check if the extracted indices are increasing by one.  */
  if (d->vec_flags == VEC_SVE_PRED
      || !d->perm[0].is_constant (&location)
      || !d->perm.series_p (0, 1, location, 1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  /* The case where (location == 0) is a no-op for both big- and little-endian,
     and is removed by the mid-end at optimization levels -O1 and higher.

     We don't need a big-endian lane correction for SVE; see the comment
     at the head of aarch64-sve.md for details.  */
  if (BYTES_BIG_ENDIAN && location != 0 && d->vec_flags == VEC_ADVSIMD)
    {
      /* After setup, we want the high elements of the first vector (stored
         at the LSB end of the register), and the low elements of the second
         vector (stored at the MSB end of the register). So swap.  */
      std::swap (d->op0, d->op1);
      /* location != 0 (above), so safe to assume (nelt - location) < nelt.
	 to_constant () is safe since this is restricted to Advanced SIMD
	 vectors.  */
      location = d->perm.length ().to_constant () - location;
    }

  offset = GEN_INT (location);
  emit_set_insn (d->target,
		 gen_rtx_UNSPEC (d->vmode,
				 gen_rtvec (3, d->op0, d->op1, offset),
				 UNSPEC_EXT));
  return true;
}

/* Recognize patterns for the REV{64,32,16} insns, which reverse elements
   within each 64-bit, 32-bit or 16-bit granule.  */

static bool
aarch64_evpc_rev_local (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT diff;
  unsigned int i, size, unspec;
  machine_mode pred_mode;

  if (d->vec_flags == VEC_SVE_PRED
      || !d->one_vector_p
      || !d->perm[0].is_constant (&diff))
    return false;

  size = (diff + 1) * GET_MODE_UNIT_SIZE (d->vmode);
  if (size == 8)
    {
      unspec = UNSPEC_REV64;
      pred_mode = VNx2BImode;
    }
  else if (size == 4)
    {
      unspec = UNSPEC_REV32;
      pred_mode = VNx4BImode;
    }
  else if (size == 2)
    {
      unspec = UNSPEC_REV16;
      pred_mode = VNx8BImode;
    }
  else
    return false;

  unsigned int step = diff + 1;
  for (i = 0; i < step; ++i)
    if (!d->perm.series_p (i, step, diff - i, step))
      return false;

  /* Success! */
  if (d->testing_p)
    return true;

  if (d->vec_flags == VEC_SVE_DATA)
    {
      machine_mode int_mode = aarch64_sve_int_mode (pred_mode);
      rtx target = gen_reg_rtx (int_mode);
      if (BYTES_BIG_ENDIAN)
	/* The act of taking a subreg between INT_MODE and d->vmode
	   is itself a reversing operation on big-endian targets;
	   see the comment at the head of aarch64-sve.md for details.
	   First reinterpret OP0 as INT_MODE without using a subreg
	   and without changing the contents.  */
	emit_insn (gen_aarch64_sve_reinterpret (int_mode, target, d->op0));
      else
	{
	  /* For SVE we use REV[BHW] unspecs derived from the element size
	     of v->mode and vector modes whose elements have SIZE bytes.
	     This ensures that the vector modes match the predicate modes.  */
	  int unspec = aarch64_sve_rev_unspec (d->vmode);
	  rtx pred = aarch64_ptrue_reg (pred_mode);
	  emit_insn (gen_aarch64_pred (unspec, int_mode, target, pred,
				       gen_lowpart (int_mode, d->op0)));
	}
      emit_move_insn (d->target, gen_lowpart (d->vmode, target));
      return true;
    }
  rtx src = gen_rtx_UNSPEC (d->vmode, gen_rtvec (1, d->op0), unspec);
  emit_set_insn (d->target, src);
  return true;
}

/* Recognize patterns for the REV insn, which reverses elements within
   a full vector.  */

static bool
aarch64_evpc_rev_global (struct expand_vec_perm_d *d)
{
  poly_uint64 nelt = d->perm.length ();

  if (!d->one_vector_p || d->vec_flags == VEC_ADVSIMD)
    return false;

  if (!d->perm.series_p (0, 1, nelt - 1, -1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  rtx src = gen_rtx_UNSPEC (d->vmode, gen_rtvec (1, d->op0), UNSPEC_REV);
  emit_set_insn (d->target, src);
  return true;
}

static bool
aarch64_evpc_dup (struct expand_vec_perm_d *d)
{
  rtx out = d->target;
  rtx in0;
  HOST_WIDE_INT elt;
  machine_mode vmode = d->vmode;
  rtx lane;

  if (d->vec_flags == VEC_SVE_PRED
      || d->perm.encoding ().encoded_nelts () != 1
      || !d->perm[0].is_constant (&elt))
    return false;

  if (d->vec_flags == VEC_SVE_DATA && elt >= 64 * GET_MODE_UNIT_SIZE (vmode))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  /* The generic preparation in aarch64_expand_vec_perm_const_1
     swaps the operand order and the permute indices if it finds
     d->perm[0] to be in the second operand.  Thus, we can always
     use d->op0 and need not do any extra arithmetic to get the
     correct lane number.  */
  in0 = d->op0;
  lane = GEN_INT (elt); /* The pattern corrects for big-endian.  */

  rtx parallel = gen_rtx_PARALLEL (vmode, gen_rtvec (1, lane));
  rtx select = gen_rtx_VEC_SELECT (GET_MODE_INNER (vmode), in0, parallel);
  emit_set_insn (out, gen_rtx_VEC_DUPLICATE (vmode, select));
  return true;
}

static bool
aarch64_evpc_tbl (struct expand_vec_perm_d *d)
{
  rtx rperm[MAX_COMPILE_TIME_VEC_BYTES], sel;
  machine_mode vmode = d->vmode;

  /* Make sure that the indices are constant.  */
  unsigned int encoded_nelts = d->perm.encoding ().encoded_nelts ();
  for (unsigned int i = 0; i < encoded_nelts; ++i)
    if (!d->perm[i].is_constant ())
      return false;

  if (d->testing_p)
    return true;

  /* Generic code will try constant permutation twice.  Once with the
     original mode and again with the elements lowered to QImode.
     So wait and don't do the selector expansion ourselves.  */
  if (vmode != V8QImode && vmode != V16QImode)
    return false;

  /* to_constant is safe since this routine is specific to Advanced SIMD
     vectors.  */
  unsigned int nelt = d->perm.length ().to_constant ();
  for (unsigned int i = 0; i < nelt; ++i)
    /* If big-endian and two vectors we end up with a weird mixed-endian
       mode on NEON.  Reverse the index within each word but not the word
       itself.  to_constant is safe because we checked is_constant above.  */
    rperm[i] = GEN_INT (BYTES_BIG_ENDIAN
			? d->perm[i].to_constant () ^ (nelt - 1)
			: d->perm[i].to_constant ());

  sel = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (nelt, rperm));
  sel = force_reg (vmode, sel);

  aarch64_expand_vec_perm_1 (d->target, d->op0, d->op1, sel);
  return true;
}

/* Try to implement D using an SVE TBL instruction.  */

static bool
aarch64_evpc_sve_tbl (struct expand_vec_perm_d *d)
{
  unsigned HOST_WIDE_INT nelt;

  /* Permuting two variable-length vectors could overflow the
     index range.  */
  if (!d->one_vector_p && !d->perm.length ().is_constant (&nelt))
    return false;

  if (d->testing_p)
    return true;

  machine_mode sel_mode = related_int_vector_mode (d->vmode).require ();
  rtx sel = vec_perm_indices_to_rtx (sel_mode, d->perm);
  if (d->one_vector_p)
    emit_unspec2 (d->target, UNSPEC_TBL, d->op0, force_reg (sel_mode, sel));
  else
    aarch64_expand_sve_vec_perm (d->target, d->op0, d->op1, sel);
  return true;
}

/* Try to implement D using SVE SEL instruction.  */

static bool
aarch64_evpc_sel (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  int unit_size = GET_MODE_UNIT_SIZE (vmode);

  if (d->vec_flags != VEC_SVE_DATA
      || unit_size > 8)
    return false;

  int n_patterns = d->perm.encoding ().npatterns ();
  poly_int64 vec_len = d->perm.length ();

  for (int i = 0; i < n_patterns; ++i)
    if (!known_eq (d->perm[i], i)
	&& !known_eq (d->perm[i], vec_len + i))
      return false;

  for (int i = n_patterns; i < n_patterns * 2; i++)
    if (!d->perm.series_p (i, n_patterns, i, n_patterns)
	&& !d->perm.series_p (i, n_patterns, vec_len + i, n_patterns))
      return false;

  if (d->testing_p)
    return true;

  machine_mode pred_mode = aarch64_sve_pred_mode (vmode);

  rtx_vector_builder builder (pred_mode, n_patterns, 2);
  for (int i = 0; i < n_patterns * 2; i++)
    {
      rtx elem = known_eq (d->perm[i], i) ? CONST1_RTX (BImode)
					  : CONST0_RTX (BImode);
      builder.quick_push (elem);
    }

  rtx const_vec = builder.build ();
  rtx pred = force_reg (pred_mode, const_vec);
  emit_insn (gen_vcond_mask (vmode, vmode, d->target, d->op1, d->op0, pred));
  return true;
}

static bool
aarch64_expand_vec_perm_const_1 (struct expand_vec_perm_d *d)
{
  /* The pattern matching functions above are written to look for a small
     number to begin the sequence (0, 1, N/2).  If we begin with an index
     from the second operand, we can swap the operands.  */
  poly_int64 nelt = d->perm.length ();
  if (known_ge (d->perm[0], nelt))
    {
      d->perm.rotate_inputs (1);
      std::swap (d->op0, d->op1);
    }

  if ((d->vec_flags == VEC_ADVSIMD
       || d->vec_flags == VEC_SVE_DATA
       || d->vec_flags == VEC_SVE_PRED)
      && known_gt (nelt, 1))
    {
      if (aarch64_evpc_rev_local (d))
	return true;
      else if (aarch64_evpc_rev_global (d))
	return true;
      else if (aarch64_evpc_ext (d))
	return true;
      else if (aarch64_evpc_dup (d))
	return true;
      else if (aarch64_evpc_zip (d))
	return true;
      else if (aarch64_evpc_uzp (d))
	return true;
      else if (aarch64_evpc_trn (d))
	return true;
      else if (aarch64_evpc_sel (d))
	return true;
      if (d->vec_flags == VEC_SVE_DATA)
	return aarch64_evpc_sve_tbl (d);
      else if (d->vec_flags == VEC_ADVSIMD)
	return aarch64_evpc_tbl (d);
    }
  return false;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
aarch64_vectorize_vec_perm_const (machine_mode vmode, rtx target, rtx op0,
				  rtx op1, const vec_perm_indices &sel)
{
  struct expand_vec_perm_d d;

  /* Check whether the mask can be applied to a single vector.  */
  if (sel.ninputs () == 1
      || (op0 && rtx_equal_p (op0, op1)))
    d.one_vector_p = true;
  else if (sel.all_from_input_p (0))
    {
      d.one_vector_p = true;
      op1 = op0;
    }
  else if (sel.all_from_input_p (1))
    {
      d.one_vector_p = true;
      op0 = op1;
    }
  else
    d.one_vector_p = false;

  d.perm.new_vector (sel.encoding (), d.one_vector_p ? 1 : 2,
		     sel.nelts_per_input ());
  d.vmode = vmode;
  d.vec_flags = aarch64_classify_vector_mode (d.vmode);
  d.target = target;
  d.op0 = op0;
  d.op1 = op1;
  d.testing_p = !target;

  if (!d.testing_p)
    return aarch64_expand_vec_perm_const_1 (&d);

  rtx_insn *last = get_last_insn ();
  bool ret = aarch64_expand_vec_perm_const_1 (&d);
  gcc_assert (last == get_last_insn ());

  return ret;
}

/* Generate a byte permute mask for a register of mode MODE,
   which has NUNITS units.  */

rtx
aarch64_reverse_mask (machine_mode mode, unsigned int nunits)
{
  /* We have to reverse each vector because we dont have
     a permuted load that can reverse-load according to ABI rules.  */
  rtx mask;
  rtvec v = rtvec_alloc (16);
  unsigned int i, j;
  unsigned int usize = GET_MODE_UNIT_SIZE (mode);

  gcc_assert (BYTES_BIG_ENDIAN);
  gcc_assert (AARCH64_VALID_SIMD_QREG_MODE (mode));

  for (i = 0; i < nunits; i++)
    for (j = 0; j < usize; j++)
      RTVEC_ELT (v, i * usize + j) = GEN_INT ((i + 1) * usize - 1 - j);
  mask = gen_rtx_CONST_VECTOR (V16QImode, v);
  return force_reg (V16QImode, mask);
}

/* Expand an SVE integer comparison using the SVE equivalent of:

     (set TARGET (CODE OP0 OP1)).  */

void
aarch64_expand_sve_vec_cmp_int (rtx target, rtx_code code, rtx op0, rtx op1)
{
  machine_mode pred_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);
  rtx res = aarch64_sve_emit_int_cmp (target, pred_mode, code, data_mode,
				      op0, op1);
  if (!rtx_equal_p (target, res))
    emit_move_insn (target, res);
}

/* Return the UNSPEC_COND_* code for comparison CODE.  */

static unsigned int
aarch64_unspec_cond_code (rtx_code code)
{
  switch (code)
    {
    case NE:
      return UNSPEC_COND_FCMNE;
    case EQ:
      return UNSPEC_COND_FCMEQ;
    case LT:
      return UNSPEC_COND_FCMLT;
    case GT:
      return UNSPEC_COND_FCMGT;
    case LE:
      return UNSPEC_COND_FCMLE;
    case GE:
      return UNSPEC_COND_FCMGE;
    case UNORDERED:
      return UNSPEC_COND_FCMUO;
    default:
      gcc_unreachable ();
    }
}

/* Emit:

      (set TARGET (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X>))

   where <X> is the operation associated with comparison CODE.
   KNOWN_PTRUE_P is true if PRED is known to be a PTRUE.  */

static void
aarch64_emit_sve_fp_cond (rtx target, rtx_code code, rtx pred,
			  bool known_ptrue_p, rtx op0, rtx op1)
{
  rtx flag = gen_int_mode (known_ptrue_p, SImode);
  rtx unspec = gen_rtx_UNSPEC (GET_MODE (pred),
			       gen_rtvec (4, pred, flag, op0, op1),
			       aarch64_unspec_cond_code (code));
  emit_set_insn (target, unspec);
}

/* Emit the SVE equivalent of:

      (set TMP1 (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X1>))
      (set TMP2 (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X2>))
      (set TARGET (ior:PRED_MODE TMP1 TMP2))

   where <Xi> is the operation associated with comparison CODEi.
   KNOWN_PTRUE_P is true if PRED is known to be a PTRUE.  */

static void
aarch64_emit_sve_or_fp_conds (rtx target, rtx_code code1, rtx_code code2,
			      rtx pred, bool known_ptrue_p, rtx op0, rtx op1)
{
  machine_mode pred_mode = GET_MODE (pred);
  rtx tmp1 = gen_reg_rtx (pred_mode);
  aarch64_emit_sve_fp_cond (tmp1, code1, pred, known_ptrue_p, op0, op1);
  rtx tmp2 = gen_reg_rtx (pred_mode);
  aarch64_emit_sve_fp_cond (tmp2, code2, pred, known_ptrue_p, op0, op1);
  aarch64_emit_binop (target, ior_optab, tmp1, tmp2);
}

/* Emit the SVE equivalent of:

      (set TMP (unspec [PRED KNOWN_PTRUE_P OP0 OP1] UNSPEC_COND_<X>))
      (set TARGET (not TMP))

   where <X> is the operation associated with comparison CODE.
   KNOWN_PTRUE_P is true if PRED is known to be a PTRUE.  */

static void
aarch64_emit_sve_invert_fp_cond (rtx target, rtx_code code, rtx pred,
				 bool known_ptrue_p, rtx op0, rtx op1)
{
  machine_mode pred_mode = GET_MODE (pred);
  rtx tmp = gen_reg_rtx (pred_mode);
  aarch64_emit_sve_fp_cond (tmp, code, pred, known_ptrue_p, op0, op1);
  aarch64_emit_unop (target, one_cmpl_optab, tmp);
}

/* Expand an SVE floating-point comparison using the SVE equivalent of:

     (set TARGET (CODE OP0 OP1))

   If CAN_INVERT_P is true, the caller can also handle inverted results;
   return true if the result is in fact inverted.  */

bool
aarch64_expand_sve_vec_cmp_float (rtx target, rtx_code code,
				  rtx op0, rtx op1, bool can_invert_p)
{
  machine_mode pred_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);

  rtx ptrue = aarch64_ptrue_reg (pred_mode);
  switch (code)
    {
    case UNORDERED:
      /* UNORDERED has no immediate form.  */
      op1 = force_reg (data_mode, op1);
      /* fall through */
    case LT:
    case LE:
    case GT:
    case GE:
    case EQ:
    case NE:
      {
	/* There is native support for the comparison.  */
	aarch64_emit_sve_fp_cond (target, code, ptrue, true, op0, op1);
	return false;
      }

    case LTGT:
      /* This is a trapping operation (LT or GT).  */
      aarch64_emit_sve_or_fp_conds (target, LT, GT, ptrue, true, op0, op1);
      return false;

    case UNEQ:
      if (!flag_trapping_math)
	{
	  /* This would trap for signaling NaNs.  */
	  op1 = force_reg (data_mode, op1);
	  aarch64_emit_sve_or_fp_conds (target, UNORDERED, EQ,
					ptrue, true, op0, op1);
	  return false;
	}
      /* fall through */
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
      if (flag_trapping_math)
	{
	  /* Work out which elements are ordered.  */
	  rtx ordered = gen_reg_rtx (pred_mode);
	  op1 = force_reg (data_mode, op1);
	  aarch64_emit_sve_invert_fp_cond (ordered, UNORDERED,
					   ptrue, true, op0, op1);

	  /* Test the opposite condition for the ordered elements,
	     then invert the result.  */
	  if (code == UNEQ)
	    code = NE;
	  else
	    code = reverse_condition_maybe_unordered (code);
	  if (can_invert_p)
	    {
	      aarch64_emit_sve_fp_cond (target, code,
					ordered, false, op0, op1);
	      return true;
	    }
	  aarch64_emit_sve_invert_fp_cond (target, code,
					   ordered, false, op0, op1);
	  return false;
	}
      break;

    case ORDERED:
      /* ORDERED has no immediate form.  */
      op1 = force_reg (data_mode, op1);
      break;

    default:
      gcc_unreachable ();
    }

  /* There is native support for the inverse comparison.  */
  code = reverse_condition_maybe_unordered (code);
  if (can_invert_p)
    {
      aarch64_emit_sve_fp_cond (target, code, ptrue, true, op0, op1);
      return true;
    }
  aarch64_emit_sve_invert_fp_cond (target, code, ptrue, true, op0, op1);
  return false;
}

/* Expand an SVE vcond pattern with operands OPS.  DATA_MODE is the mode
   of the data being selected and CMP_MODE is the mode of the values being
   compared.  */

void
aarch64_expand_sve_vcond (machine_mode data_mode, machine_mode cmp_mode,
			  rtx *ops)
{
  machine_mode pred_mode = aarch64_get_mask_mode (cmp_mode).require ();
  rtx pred = gen_reg_rtx (pred_mode);
  if (FLOAT_MODE_P (cmp_mode))
    {
      if (aarch64_expand_sve_vec_cmp_float (pred, GET_CODE (ops[3]),
					    ops[4], ops[5], true))
	std::swap (ops[1], ops[2]);
    }
  else
    aarch64_expand_sve_vec_cmp_int (pred, GET_CODE (ops[3]), ops[4], ops[5]);

  if (!aarch64_sve_reg_or_dup_imm (ops[1], data_mode))
    ops[1] = force_reg (data_mode, ops[1]);
  /* The "false" value can only be zero if the "true" value is a constant.  */
  if (register_operand (ops[1], data_mode)
      || !aarch64_simd_reg_or_zero (ops[2], data_mode))
    ops[2] = force_reg (data_mode, ops[2]);

  rtvec vec = gen_rtvec (3, pred, ops[1], ops[2]);
  emit_set_insn (ops[0], gen_rtx_UNSPEC (data_mode, vec, UNSPEC_SEL));
}

/* Implement TARGET_MODES_TIEABLE_P.  In principle we should always return
   true.  However due to issues with register allocation it is preferable
   to avoid tieing integer scalar and FP scalar modes.  Executing integer
   operations in general registers is better than treating them as scalar
   vector operations.  This reduces latency and avoids redundant int<->FP
   moves.  So tie modes if they are either the same class, or vector modes
   with other vector modes, vector structs or any scalar mode.  */

static bool
aarch64_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (GET_MODE_CLASS (mode1) == GET_MODE_CLASS (mode2))
    return true;

  /* We specifically want to allow elements of "structure" modes to
     be tieable to the structure.  This more general condition allows
     other rarer situations too.  The reason we don't extend this to
     predicate modes is that there are no predicate structure modes
     nor any specific instructions for extracting part of a predicate
     register.  */
  if (aarch64_vector_data_mode_p (mode1)
      && aarch64_vector_data_mode_p (mode2))
    return true;

  /* Also allow any scalar modes with vectors.  */
  if (aarch64_vector_mode_supported_p (mode1)
      || aarch64_vector_mode_supported_p (mode2))
    return true;

  return false;
}

/* Return a new RTX holding the result of moving POINTER forward by
   AMOUNT bytes.  */

static rtx
aarch64_move_pointer (rtx pointer, poly_int64 amount)
{
  rtx next = plus_constant (Pmode, XEXP (pointer, 0), amount);

  return adjust_automodify_address (pointer, GET_MODE (pointer),
				    next, amount);
}

/* Return a new RTX holding the result of moving POINTER forward by the
   size of the mode it points to.  */

static rtx
aarch64_progress_pointer (rtx pointer)
{
  return aarch64_move_pointer (pointer, GET_MODE_SIZE (GET_MODE (pointer)));
}

/* Copy one MODE sized block from SRC to DST, then progress SRC and DST by
   MODE bytes.  */

static void
aarch64_copy_one_block_and_progress_pointers (rtx *src, rtx *dst,
					      machine_mode mode)
{
  rtx reg = gen_reg_rtx (mode);

  /* "Cast" the pointers to the correct mode.  */
  *src = adjust_address (*src, mode, 0);
  *dst = adjust_address (*dst, mode, 0);
  /* Emit the memcpy.  */
  emit_move_insn (reg, *src);
  emit_move_insn (*dst, reg);
  /* Move the pointers forward.  */
  *src = aarch64_progress_pointer (*src);
  *dst = aarch64_progress_pointer (*dst);
}

/* Expand cpymem, as if from a __builtin_memcpy.  Return true if
   we succeed, otherwise return false.  */

bool
aarch64_expand_cpymem (rtx *operands)
{
  int n, mode_bits;
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx base;
  machine_mode cur_mode = BLKmode, next_mode;
  bool speed_p = !optimize_function_for_size_p (cfun);

  /* When optimizing for size, give a better estimate of the length of a
     memcpy call, but use the default otherwise.  Moves larger than 8 bytes
     will always require an even number of instructions to do now.  And each
     operation requires both a load+store, so devide the max number by 2.  */
  int max_num_moves = (speed_p ? 16 : AARCH64_CALL_RATIO) / 2;

  /* We can't do anything smart if the amount to copy is not constant.  */
  if (!CONST_INT_P (operands[2]))
    return false;

  n = INTVAL (operands[2]);

  /* Try to keep the number of instructions low.  For all cases we will do at
     most two moves for the residual amount, since we'll always overlap the
     remainder.  */
  if (((n / 16) + (n % 16 ? 2 : 0)) > max_num_moves)
    return false;

  base = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  dst = adjust_automodify_address (dst, VOIDmode, base, 0);

  base = copy_to_mode_reg (Pmode, XEXP (src, 0));
  src = adjust_automodify_address (src, VOIDmode, base, 0);

  /* Convert n to bits to make the rest of the code simpler.  */
  n = n * BITS_PER_UNIT;

  /* Maximum amount to copy in one go.  The AArch64 back-end has integer modes
     larger than TImode, but we should not use them for loads/stores here.  */
  const int copy_limit = GET_MODE_BITSIZE (TImode);

  while (n > 0)
    {
      /* Find the largest mode in which to do the copy in without over reading
	 or writing.  */
      opt_scalar_int_mode mode_iter;
      FOR_EACH_MODE_IN_CLASS (mode_iter, MODE_INT)
	if (GET_MODE_BITSIZE (mode_iter.require ()) <= MIN (n, copy_limit))
	  cur_mode = mode_iter.require ();

      gcc_assert (cur_mode != BLKmode);

      mode_bits = GET_MODE_BITSIZE (cur_mode).to_constant ();
      aarch64_copy_one_block_and_progress_pointers (&src, &dst, cur_mode);

      n -= mode_bits;

      /* Do certain trailing copies as overlapping if it's going to be
	 cheaper.  i.e. less instructions to do so.  For instance doing a 15
	 byte copy it's more efficient to do two overlapping 8 byte copies than
	 8 + 6 + 1.  */
      if (n > 0 && n <= 8 * BITS_PER_UNIT)
	{
	  next_mode = smallest_mode_for_size (n, MODE_INT);
	  int n_bits = GET_MODE_BITSIZE (next_mode).to_constant ();
	  src = aarch64_move_pointer (src, (n - n_bits) / BITS_PER_UNIT);
	  dst = aarch64_move_pointer (dst, (n - n_bits) / BITS_PER_UNIT);
	  n = n_bits;
	}
    }

  return true;
}

/* Split a DImode store of a CONST_INT SRC to MEM DST as two
   SImode stores.  Handle the case when the constant has identical
   bottom and top halves.  This is beneficial when the two stores can be
   merged into an STP and we avoid synthesising potentially expensive
   immediates twice.  Return true if such a split is possible.  */

bool
aarch64_split_dimode_const_store (rtx dst, rtx src)
{
  rtx lo = gen_lowpart (SImode, src);
  rtx hi = gen_highpart_mode (SImode, DImode, src);

  bool size_p = optimize_function_for_size_p (cfun);

  if (!rtx_equal_p (lo, hi))
    return false;

  unsigned int orig_cost
    = aarch64_internal_mov_immediate (NULL_RTX, src, false, DImode);
  unsigned int lo_cost
    = aarch64_internal_mov_immediate (NULL_RTX, lo, false, SImode);

  /* We want to transform:
     MOV	x1, 49370
     MOVK	x1, 0x140, lsl 16
     MOVK	x1, 0xc0da, lsl 32
     MOVK	x1, 0x140, lsl 48
     STR	x1, [x0]
   into:
     MOV	w1, 49370
     MOVK	w1, 0x140, lsl 16
     STP	w1, w1, [x0]
   So we want to perform this only when we save two instructions
   or more.  When optimizing for size, however, accept any code size
   savings we can.  */
  if (size_p && orig_cost <= lo_cost)
    return false;

  if (!size_p
      && (orig_cost <= lo_cost + 1))
    return false;

  rtx mem_lo = adjust_address (dst, SImode, 0);
  if (!aarch64_mem_pair_operand (mem_lo, SImode))
    return false;

  rtx tmp_reg = gen_reg_rtx (SImode);
  aarch64_expand_mov_immediate (tmp_reg, lo);
  rtx mem_hi = aarch64_move_pointer (mem_lo, GET_MODE_SIZE (SImode));
  /* Don't emit an explicit store pair as this may not be always profitable.
     Let the sched-fusion logic decide whether to merge them.  */
  emit_move_insn (mem_lo, tmp_reg);
  emit_move_insn (mem_hi, tmp_reg);

  return true;
}

/* Generate RTL for a conditional branch with rtx comparison CODE in
   mode CC_MODE.  The destination of the unlikely conditional branch
   is LABEL_REF.  */

void
aarch64_gen_unlikely_cbranch (enum rtx_code code, machine_mode cc_mode,
			      rtx label_ref)
{
  rtx x;
  x = gen_rtx_fmt_ee (code, VOIDmode,
		      gen_rtx_REG (cc_mode, CC_REGNUM),
		      const0_rtx);

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (VOIDmode, label_ref),
			    pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
}

/* Generate DImode scratch registers for 128-bit (TImode) addition.

   OP1 represents the TImode destination operand 1
   OP2 represents the TImode destination operand 2
   LOW_DEST represents the low half (DImode) of TImode operand 0
   LOW_IN1 represents the low half (DImode) of TImode operand 1
   LOW_IN2 represents the low half (DImode) of TImode operand 2
   HIGH_DEST represents the high half (DImode) of TImode operand 0
   HIGH_IN1 represents the high half (DImode) of TImode operand 1
   HIGH_IN2 represents the high half (DImode) of TImode operand 2.  */

void
aarch64_addti_scratch_regs (rtx op1, rtx op2, rtx *low_dest,
			    rtx *low_in1, rtx *low_in2,
			    rtx *high_dest, rtx *high_in1,
			    rtx *high_in2)
{
  *low_dest = gen_reg_rtx (DImode);
  *low_in1 = gen_lowpart (DImode, op1);
  *low_in2 = simplify_gen_subreg (DImode, op2, TImode,
				  subreg_lowpart_offset (DImode, TImode));
  *high_dest = gen_reg_rtx (DImode);
  *high_in1 = gen_highpart (DImode, op1);
  *high_in2 = simplify_gen_subreg (DImode, op2, TImode,
				   subreg_highpart_offset (DImode, TImode));
}

/* Generate DImode scratch registers for 128-bit (TImode) subtraction.

   This function differs from 'arch64_addti_scratch_regs' in that
   OP1 can be an immediate constant (zero). We must call
   subreg_highpart_offset with DImode and TImode arguments, otherwise
   VOIDmode will be used for the const_int which generates an internal
   error from subreg_size_highpart_offset which does not expect a size of zero.

   OP1 represents the TImode destination operand 1
   OP2 represents the TImode destination operand 2
   LOW_DEST represents the low half (DImode) of TImode operand 0
   LOW_IN1 represents the low half (DImode) of TImode operand 1
   LOW_IN2 represents the low half (DImode) of TImode operand 2
   HIGH_DEST represents the high half (DImode) of TImode operand 0
   HIGH_IN1 represents the high half (DImode) of TImode operand 1
   HIGH_IN2 represents the high half (DImode) of TImode operand 2.  */


void
aarch64_subvti_scratch_regs (rtx op1, rtx op2, rtx *low_dest,
			     rtx *low_in1, rtx *low_in2,
			     rtx *high_dest, rtx *high_in1,
			     rtx *high_in2)
{
  *low_dest = gen_reg_rtx (DImode);
  *low_in1 = simplify_gen_subreg (DImode, op1, TImode,
				  subreg_lowpart_offset (DImode, TImode));

  *low_in2 = simplify_gen_subreg (DImode, op2, TImode,
				  subreg_lowpart_offset (DImode, TImode));
  *high_dest = gen_reg_rtx (DImode);

  *high_in1 = simplify_gen_subreg (DImode, op1, TImode,
				   subreg_highpart_offset (DImode, TImode));
  *high_in2 = simplify_gen_subreg (DImode, op2, TImode,
				   subreg_highpart_offset (DImode, TImode));
}

/* Generate RTL for 128-bit (TImode) subtraction with overflow.

   OP0 represents the TImode destination operand 0
   LOW_DEST represents the low half (DImode) of TImode operand 0
   LOW_IN1 represents the low half (DImode) of TImode operand 1
   LOW_IN2 represents the low half (DImode) of TImode operand 2
   HIGH_DEST represents the high half (DImode) of TImode operand 0
   HIGH_IN1 represents the high half (DImode) of TImode operand 1
   HIGH_IN2 represents the high half (DImode) of TImode operand 2
   UNSIGNED_P is true if the operation is being performed on unsigned
   values.  */
void
aarch64_expand_subvti (rtx op0, rtx low_dest, rtx low_in1,
		       rtx low_in2, rtx high_dest, rtx high_in1,
		       rtx high_in2, bool unsigned_p)
{
  if (low_in2 == const0_rtx)
    {
      low_dest = low_in1;
      high_in2 = force_reg (DImode, high_in2);
      if (unsigned_p)
	emit_insn (gen_subdi3_compare1 (high_dest, high_in1, high_in2));
      else
	emit_insn (gen_subvdi_insn (high_dest, high_in1, high_in2));
    }
  else
    {
      if (CONST_INT_P (low_in2))
	{
	  high_in2 = force_reg (DImode, high_in2);
	  emit_insn (gen_subdi3_compare1_imm (low_dest, low_in1, low_in2,
					      GEN_INT (-INTVAL (low_in2))));
	}
      else
	emit_insn (gen_subdi3_compare1 (low_dest, low_in1, low_in2));

      if (unsigned_p)
	emit_insn (gen_usubdi3_carryinC (high_dest, high_in1, high_in2));
      else
	emit_insn (gen_subdi3_carryinV (high_dest, high_in1, high_in2));
    }

  emit_move_insn (gen_lowpart (DImode, op0), low_dest);
  emit_move_insn (gen_highpart (DImode, op0), high_dest);

}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
aarch64_asan_shadow_offset (void)
{
  if (TARGET_ILP32)
    return (HOST_WIDE_INT_1 << 29);
  else
    return (HOST_WIDE_INT_1 << 36);
}

static rtx
aarch64_gen_ccmp_first (rtx_insn **prep_seq, rtx_insn **gen_seq,
			int code, tree treeop0, tree treeop1)
{
  machine_mode op_mode, cmp_mode, cc_mode = CCmode;
  rtx op0, op1;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));
  insn_code icode;
  struct expand_operand ops[4];

  start_sequence ();
  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);

  op_mode = GET_MODE (op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (op1);

  switch (op_mode)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
      cmp_mode = SImode;
      icode = CODE_FOR_cmpsi;
      break;

    case E_DImode:
      cmp_mode = DImode;
      icode = CODE_FOR_cmpdi;
      break;

    case E_SFmode:
      cmp_mode = SFmode;
      cc_mode = aarch64_select_cc_mode ((rtx_code) code, op0, op1);
      icode = cc_mode == CCFPEmode ? CODE_FOR_fcmpesf : CODE_FOR_fcmpsf;
      break;

    case E_DFmode:
      cmp_mode = DFmode;
      cc_mode = aarch64_select_cc_mode ((rtx_code) code, op0, op1);
      icode = cc_mode == CCFPEmode ? CODE_FOR_fcmpedf : CODE_FOR_fcmpdf;
      break;

    default:
      end_sequence ();
      return NULL_RTX;
    }

  op0 = prepare_operand (icode, op0, 0, op_mode, cmp_mode, unsignedp);
  op1 = prepare_operand (icode, op1, 1, op_mode, cmp_mode, unsignedp);
  if (!op0 || !op1)
    {
      end_sequence ();
      return NULL_RTX;
    }
  *prep_seq = get_insns ();
  end_sequence ();

  create_fixed_operand (&ops[0], op0);
  create_fixed_operand (&ops[1], op1);

  start_sequence ();
  if (!maybe_expand_insn (icode, 2, ops))
    {
      end_sequence ();
      return NULL_RTX;
    }
  *gen_seq = get_insns ();
  end_sequence ();

  return gen_rtx_fmt_ee ((rtx_code) code, cc_mode,
			 gen_rtx_REG (cc_mode, CC_REGNUM), const0_rtx);
}

static rtx
aarch64_gen_ccmp_next (rtx_insn **prep_seq, rtx_insn **gen_seq, rtx prev,
		       int cmp_code, tree treeop0, tree treeop1, int bit_code)
{
  rtx op0, op1, target;
  machine_mode op_mode, cmp_mode, cc_mode = CCmode;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));
  insn_code icode;
  struct expand_operand ops[6];
  int aarch64_cond;

  push_to_sequence (*prep_seq);
  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);

  op_mode = GET_MODE (op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (op1);

  switch (op_mode)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
      cmp_mode = SImode;
      icode = CODE_FOR_ccmpsi;
      break;

    case E_DImode:
      cmp_mode = DImode;
      icode = CODE_FOR_ccmpdi;
      break;

    case E_SFmode:
      cmp_mode = SFmode;
      cc_mode = aarch64_select_cc_mode ((rtx_code) cmp_code, op0, op1);
      icode = cc_mode == CCFPEmode ? CODE_FOR_fccmpesf : CODE_FOR_fccmpsf;
      break;

    case E_DFmode:
      cmp_mode = DFmode;
      cc_mode = aarch64_select_cc_mode ((rtx_code) cmp_code, op0, op1);
      icode = cc_mode == CCFPEmode ? CODE_FOR_fccmpedf : CODE_FOR_fccmpdf;
      break;

    default:
      end_sequence ();
      return NULL_RTX;
    }

  op0 = prepare_operand (icode, op0, 2, op_mode, cmp_mode, unsignedp);
  op1 = prepare_operand (icode, op1, 3, op_mode, cmp_mode, unsignedp);
  if (!op0 || !op1)
    {
      end_sequence ();
      return NULL_RTX;
    }
  *prep_seq = get_insns ();
  end_sequence ();

  target = gen_rtx_REG (cc_mode, CC_REGNUM);
  aarch64_cond = aarch64_get_condition_code_1 (cc_mode, (rtx_code) cmp_code);

  if (bit_code != AND)
    {
      prev = gen_rtx_fmt_ee (REVERSE_CONDITION (GET_CODE (prev),
						GET_MODE (XEXP (prev, 0))),
			     VOIDmode, XEXP (prev, 0), const0_rtx);
      aarch64_cond = AARCH64_INVERSE_CONDITION_CODE (aarch64_cond);
    }

  create_fixed_operand (&ops[0], XEXP (prev, 0));
  create_fixed_operand (&ops[1], target);
  create_fixed_operand (&ops[2], op0);
  create_fixed_operand (&ops[3], op1);
  create_fixed_operand (&ops[4], prev);
  create_fixed_operand (&ops[5], GEN_INT (aarch64_cond));

  push_to_sequence (*gen_seq);
  if (!maybe_expand_insn (icode, 6, ops))
    {
      end_sequence ();
      return NULL_RTX;
    }

  *gen_seq = get_insns ();
  end_sequence ();

  return gen_rtx_fmt_ee ((rtx_code) cmp_code, VOIDmode, target, const0_rtx);
}

#undef TARGET_GEN_CCMP_FIRST
#define TARGET_GEN_CCMP_FIRST aarch64_gen_ccmp_first

#undef TARGET_GEN_CCMP_NEXT
#define TARGET_GEN_CCMP_NEXT aarch64_gen_ccmp_next

/* Implement TARGET_SCHED_MACRO_FUSION_P.  Return true if target supports
   instruction fusion of some sort.  */

static bool
aarch64_macro_fusion_p (void)
{
  return aarch64_tune_params.fusible_ops != AARCH64_FUSE_NOTHING;
}


/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

static bool
aarch_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  rtx set_dest;
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  /* prev and curr are simple SET insns i.e. no flag setting or branching.  */
  bool simple_sets_p = prev_set && curr_set && !any_condjump_p (curr);

  if (!aarch64_macro_fusion_p ())
    return false;

  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_MOV_MOVK))
    {
      /* We are trying to match:
         prev (mov)  == (set (reg r0) (const_int imm16))
         curr (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 16))
                             (const_int imm16_1))  */

      set_dest = SET_DEST (curr_set);

      if (GET_CODE (set_dest) == ZERO_EXTRACT
          && CONST_INT_P (SET_SRC (curr_set))
          && CONST_INT_P (SET_SRC (prev_set))
          && CONST_INT_P (XEXP (set_dest, 2))
          && INTVAL (XEXP (set_dest, 2)) == 16
          && REG_P (XEXP (set_dest, 0))
          && REG_P (SET_DEST (prev_set))
          && REGNO (XEXP (set_dest, 0)) == REGNO (SET_DEST (prev_set)))
        {
          return true;
        }
    }

  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_ADRP_ADD))
    {

      /*  We're trying to match:
          prev (adrp) == (set (reg r1)
                              (high (symbol_ref ("SYM"))))
          curr (add) == (set (reg r0)
                             (lo_sum (reg r1)
                                     (symbol_ref ("SYM"))))
          Note that r0 need not necessarily be the same as r1, especially
          during pre-regalloc scheduling.  */

      if (satisfies_constraint_Ush (SET_SRC (prev_set))
          && REG_P (SET_DEST (prev_set)) && REG_P (SET_DEST (curr_set)))
        {
          if (GET_CODE (SET_SRC (curr_set)) == LO_SUM
              && REG_P (XEXP (SET_SRC (curr_set), 0))
              && REGNO (XEXP (SET_SRC (curr_set), 0))
                 == REGNO (SET_DEST (prev_set))
              && rtx_equal_p (XEXP (SET_SRC (prev_set), 0),
                              XEXP (SET_SRC (curr_set), 1)))
            return true;
        }
    }

  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_MOVK_MOVK))
    {

      /* We're trying to match:
         prev (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 32))
                             (const_int imm16_1))
         curr (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 48))
                             (const_int imm16_2))  */

      if (GET_CODE (SET_DEST (prev_set)) == ZERO_EXTRACT
          && GET_CODE (SET_DEST (curr_set)) == ZERO_EXTRACT
          && REG_P (XEXP (SET_DEST (prev_set), 0))
          && REG_P (XEXP (SET_DEST (curr_set), 0))
          && REGNO (XEXP (SET_DEST (prev_set), 0))
             == REGNO (XEXP (SET_DEST (curr_set), 0))
          && CONST_INT_P (XEXP (SET_DEST (prev_set), 2))
          && CONST_INT_P (XEXP (SET_DEST (curr_set), 2))
          && INTVAL (XEXP (SET_DEST (prev_set), 2)) == 32
          && INTVAL (XEXP (SET_DEST (curr_set), 2)) == 48
          && CONST_INT_P (SET_SRC (prev_set))
          && CONST_INT_P (SET_SRC (curr_set)))
        return true;

    }
  if (simple_sets_p && aarch64_fusion_enabled_p (AARCH64_FUSE_ADRP_LDR))
    {
      /* We're trying to match:
          prev (adrp) == (set (reg r0)
                              (high (symbol_ref ("SYM"))))
          curr (ldr) == (set (reg r1)
                             (mem (lo_sum (reg r0)
                                             (symbol_ref ("SYM")))))
                 or
          curr (ldr) == (set (reg r1)
                             (zero_extend (mem
                                           (lo_sum (reg r0)
                                                   (symbol_ref ("SYM"))))))  */
      if (satisfies_constraint_Ush (SET_SRC (prev_set))
          && REG_P (SET_DEST (prev_set)) && REG_P (SET_DEST (curr_set)))
        {
          rtx curr_src = SET_SRC (curr_set);

          if (GET_CODE (curr_src) == ZERO_EXTEND)
            curr_src = XEXP (curr_src, 0);

          if (MEM_P (curr_src) && GET_CODE (XEXP (curr_src, 0)) == LO_SUM
              && REG_P (XEXP (XEXP (curr_src, 0), 0))
              && REGNO (XEXP (XEXP (curr_src, 0), 0))
                 == REGNO (SET_DEST (prev_set))
              && rtx_equal_p (XEXP (XEXP (curr_src, 0), 1),
                              XEXP (SET_SRC (prev_set), 0)))
              return true;
        }
    }

  /* Fuse compare (CMP/CMN/TST/BICS) and conditional branch.  */
  if (aarch64_fusion_enabled_p (AARCH64_FUSE_CMP_BRANCH)
      && prev_set && curr_set && any_condjump_p (curr)
      && GET_CODE (SET_SRC (prev_set)) == COMPARE
      && SCALAR_INT_MODE_P (GET_MODE (XEXP (SET_SRC (prev_set), 0)))
      && reg_referenced_p (SET_DEST (prev_set), PATTERN (curr)))
    return true;

  /* Fuse flag-setting ALU instructions and conditional branch.  */
  if (aarch64_fusion_enabled_p (AARCH64_FUSE_ALU_BRANCH)
      && any_condjump_p (curr))
    {
      unsigned int condreg1, condreg2;
      rtx cc_reg_1;
      aarch64_fixed_condition_code_regs (&condreg1, &condreg2);
      cc_reg_1 = gen_rtx_REG (CCmode, condreg1);

      if (reg_referenced_p (cc_reg_1, PATTERN (curr))
	  && prev
	  && modified_in_p (cc_reg_1, prev))
	{
	  enum attr_type prev_type = get_attr_type (prev);

	  /* FIXME: this misses some which is considered simple arthematic
	     instructions for ThunderX.  Simple shifts are missed here.  */
	  if (prev_type == TYPE_ALUS_SREG
	      || prev_type == TYPE_ALUS_IMM
	      || prev_type == TYPE_LOGICS_REG
	      || prev_type == TYPE_LOGICS_IMM)
	    return true;
	}
    }

  /* Fuse ALU instructions and CBZ/CBNZ.  */
  if (prev_set
      && curr_set
      && aarch64_fusion_enabled_p (AARCH64_FUSE_ALU_CBZ)
      && any_condjump_p (curr))
    {
      /* We're trying to match:
	  prev (alu_insn) == (set (r0) plus ((r0) (r1/imm)))
	  curr (cbz) ==  (set (pc) (if_then_else (eq/ne) (r0)
							 (const_int 0))
						 (label_ref ("SYM"))
						 (pc))  */
      if (SET_DEST (curr_set) == (pc_rtx)
	  && GET_CODE (SET_SRC (curr_set)) == IF_THEN_ELSE
	  && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  && REG_P (SET_DEST (prev_set))
	  && REGNO (SET_DEST (prev_set))
	     == REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0)))
	{
	  /* Fuse ALU operations followed by conditional branch instruction.  */
	  switch (get_attr_type (prev))
	    {
	    case TYPE_ALU_IMM:
	    case TYPE_ALU_SREG:
	    case TYPE_ADC_REG:
	    case TYPE_ADC_IMM:
	    case TYPE_ADCS_REG:
	    case TYPE_ADCS_IMM:
	    case TYPE_LOGIC_REG:
	    case TYPE_LOGIC_IMM:
	    case TYPE_CSEL:
	    case TYPE_ADR:
	    case TYPE_MOV_IMM:
	    case TYPE_SHIFT_REG:
	    case TYPE_SHIFT_IMM:
	    case TYPE_BFM:
	    case TYPE_RBIT:
	    case TYPE_REV:
	    case TYPE_EXTEND:
	      return true;

	    default:;
	    }
	}
    }

  return false;
}

/* Return true iff the instruction fusion described by OP is enabled.  */

bool
aarch64_fusion_enabled_p (enum aarch64_fusion_pairs op)
{
  return (aarch64_tune_params.fusible_ops & op) != 0;
}

/* If MEM is in the form of [base+offset], extract the two parts
   of address and set to BASE and OFFSET, otherwise return false
   after clearing BASE and OFFSET.  */

bool
extract_base_offset_in_addr (rtx mem, rtx *base, rtx *offset)
{
  rtx addr;

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  if (REG_P (addr))
    {
      *base = addr;
      *offset = const0_rtx;
      return true;
    }

  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && CONST_INT_P (XEXP (addr, 1)))
    {
      *base = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return true;
    }

  *base = NULL_RTX;
  *offset = NULL_RTX;

  return false;
}

/* Types for scheduling fusion.  */
enum sched_fusion_type
{
  SCHED_FUSION_NONE = 0,
  SCHED_FUSION_LD_SIGN_EXTEND,
  SCHED_FUSION_LD_ZERO_EXTEND,
  SCHED_FUSION_LD,
  SCHED_FUSION_ST,
  SCHED_FUSION_NUM
};

/* If INSN is a load or store of address in the form of [base+offset],
   extract the two parts and set to BASE and OFFSET.  Return scheduling
   fusion type this INSN is.  */

static enum sched_fusion_type
fusion_load_store (rtx_insn *insn, rtx *base, rtx *offset)
{
  rtx x, dest, src;
  enum sched_fusion_type fusion = SCHED_FUSION_LD;

  gcc_assert (INSN_P (insn));
  x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return SCHED_FUSION_NONE;

  src = SET_SRC (x);
  dest = SET_DEST (x);

  machine_mode dest_mode = GET_MODE (dest);

  if (!aarch64_mode_valid_for_sched_fusion_p (dest_mode))
    return SCHED_FUSION_NONE;

  if (GET_CODE (src) == SIGN_EXTEND)
    {
      fusion = SCHED_FUSION_LD_SIGN_EXTEND;
      src = XEXP (src, 0);
      if (GET_CODE (src) != MEM || GET_MODE (src) != SImode)
	return SCHED_FUSION_NONE;
    }
  else if (GET_CODE (src) == ZERO_EXTEND)
    {
      fusion = SCHED_FUSION_LD_ZERO_EXTEND;
      src = XEXP (src, 0);
      if (GET_CODE (src) != MEM || GET_MODE (src) != SImode)
	return SCHED_FUSION_NONE;
    }

  if (GET_CODE (src) == MEM && REG_P (dest))
    extract_base_offset_in_addr (src, base, offset);
  else if (GET_CODE (dest) == MEM && (REG_P (src) || src == const0_rtx))
    {
      fusion = SCHED_FUSION_ST;
      extract_base_offset_in_addr (dest, base, offset);
    }
  else
    return SCHED_FUSION_NONE;

  if (*base == NULL_RTX || *offset == NULL_RTX)
    fusion = SCHED_FUSION_NONE;

  return fusion;
}

/* Implement the TARGET_SCHED_FUSION_PRIORITY hook.

   Currently we only support to fuse ldr or str instructions, so FUSION_PRI
   and PRI are only calculated for these instructions.  For other instruction,
   FUSION_PRI and PRI are simply set to MAX_PRI - 1.  In the future, other
   type instruction fusion can be added by returning different priorities.

   It's important that irrelevant instructions get the largest FUSION_PRI.  */

static void
aarch64_sched_fusion_priority (rtx_insn *insn, int max_pri,
			       int *fusion_pri, int *pri)
{
  int tmp, off_val;
  rtx base, offset;
  enum sched_fusion_type fusion;

  gcc_assert (INSN_P (insn));

  tmp = max_pri - 1;
  fusion = fusion_load_store (insn, &base, &offset);
  if (fusion == SCHED_FUSION_NONE)
    {
      *pri = tmp;
      *fusion_pri = tmp;
      return;
    }

  /* Set FUSION_PRI according to fusion type and base register.  */
  *fusion_pri = tmp - fusion * FIRST_PSEUDO_REGISTER - REGNO (base);

  /* Calculate PRI.  */
  tmp /= 2;

  /* INSN with smaller offset goes first.  */
  off_val = (int)(INTVAL (offset));
  if (off_val >= 0)
    tmp -= (off_val & 0xfffff);
  else
    tmp += ((- off_val) & 0xfffff);

  *pri = tmp;
  return;
}

/* Implement the TARGET_SCHED_ADJUST_PRIORITY hook.
   Adjust priority of sha1h instructions so they are scheduled before
   other SHA1 instructions.  */

static int
aarch64_sched_adjust_priority (rtx_insn *insn, int priority)
{
  rtx x = PATTERN (insn);

  if (GET_CODE (x) == SET)
    {
      x = SET_SRC (x);

      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_SHA1H)
	return priority + 10;
    }

  return priority;
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into ldp/stp.  LOAD is true if they are load instructions.
   MODE is the mode of memory operands.  */

bool
aarch64_operands_ok_for_ldpstp (rtx *operands, bool load,
				machine_mode mode)
{
  HOST_WIDE_INT offval_1, offval_2, msize;
  enum reg_class rclass_1, rclass_2;
  rtx mem_1, mem_2, reg_1, reg_2, base_1, base_2, offset_1, offset_2;

  if (load)
    {
      mem_1 = operands[1];
      mem_2 = operands[3];
      reg_1 = operands[0];
      reg_2 = operands[2];
      gcc_assert (REG_P (reg_1) && REG_P (reg_2));
      if (REGNO (reg_1) == REGNO (reg_2))
	return false;
    }
  else
    {
      mem_1 = operands[0];
      mem_2 = operands[2];
      reg_1 = operands[1];
      reg_2 = operands[3];
    }

  /* The mems cannot be volatile.  */
  if (MEM_VOLATILE_P (mem_1) || MEM_VOLATILE_P (mem_2))
    return false;

  /* If we have SImode and slow unaligned ldp,
     check the alignment to be at least 8 byte. */
  if (mode == SImode
      && (aarch64_tune_params.extra_tuning_flags
          & AARCH64_EXTRA_TUNE_SLOW_UNALIGNED_LDPW)
      && !optimize_size
      && MEM_ALIGN (mem_1) < 8 * BITS_PER_UNIT)
    return false;

  /* Check if the addresses are in the form of [base+offset].  */
  extract_base_offset_in_addr (mem_1, &base_1, &offset_1);
  if (base_1 == NULL_RTX || offset_1 == NULL_RTX)
    return false;
  extract_base_offset_in_addr (mem_2, &base_2, &offset_2);
  if (base_2 == NULL_RTX || offset_2 == NULL_RTX)
    return false;

  /* Check if the bases are same.  */
  if (!rtx_equal_p (base_1, base_2))
    return false;

  /* The operands must be of the same size.  */
  gcc_assert (known_eq (GET_MODE_SIZE (GET_MODE (mem_1)),
			 GET_MODE_SIZE (GET_MODE (mem_2))));

  offval_1 = INTVAL (offset_1);
  offval_2 = INTVAL (offset_2);
  /* We should only be trying this for fixed-sized modes.  There is no
     SVE LDP/STP instruction.  */
  msize = GET_MODE_SIZE (mode).to_constant ();
  /* Check if the offsets are consecutive.  */
  if (offval_1 != (offval_2 + msize) && offval_2 != (offval_1 + msize))
    return false;

  /* Check if the addresses are clobbered by load.  */
  if (load)
    {
      if (reg_mentioned_p (reg_1, mem_1))
	return false;

      /* In increasing order, the last load can clobber the address.  */
      if (offval_1 > offval_2 && reg_mentioned_p (reg_2, mem_2))
	return false;
    }

  /* One of the memory accesses must be a mempair operand.
     If it is not the first one, they need to be swapped by the
     peephole.  */
  if (!aarch64_mem_pair_operand (mem_1, GET_MODE (mem_1))
       && !aarch64_mem_pair_operand (mem_2, GET_MODE (mem_2)))
    return false;

  if (REG_P (reg_1) && FP_REGNUM_P (REGNO (reg_1)))
    rclass_1 = FP_REGS;
  else
    rclass_1 = GENERAL_REGS;

  if (REG_P (reg_2) && FP_REGNUM_P (REGNO (reg_2)))
    rclass_2 = FP_REGS;
  else
    rclass_2 = GENERAL_REGS;

  /* Check if the registers are of same class.  */
  if (rclass_1 != rclass_2)
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store that can be merged,
   swap them if they are not in ascending order.  */
void
aarch64_swap_ldrstr_operands (rtx* operands, bool load)
{
  rtx mem_1, mem_2, base_1, base_2, offset_1, offset_2;
  HOST_WIDE_INT offval_1, offval_2;

  if (load)
    {
      mem_1 = operands[1];
      mem_2 = operands[3];
    }
  else
    {
      mem_1 = operands[0];
      mem_2 = operands[2];
    }

  extract_base_offset_in_addr (mem_1, &base_1, &offset_1);
  extract_base_offset_in_addr (mem_2, &base_2, &offset_2);

  offval_1 = INTVAL (offset_1);
  offval_2 = INTVAL (offset_2);

  if (offval_1 > offval_2)
    {
      /* Irrespective of whether this is a load or a store,
	 we do the same swap.  */
      std::swap (operands[0], operands[2]);
      std::swap (operands[1], operands[3]);
    }
}

/* Taking X and Y to be HOST_WIDE_INT pointers, return the result of a
   comparison between the two.  */
int
aarch64_host_wide_int_compare (const void *x, const void *y)
{
  return wi::cmps (* ((const HOST_WIDE_INT *) x),
		   * ((const HOST_WIDE_INT *) y));
}

/* Taking X and Y to be pairs of RTX, one pointing to a MEM rtx and the
   other pointing to a REG rtx containing an offset, compare the offsets
   of the two pairs.

   Return:

	1 iff offset (X) > offset (Y)
	0 iff offset (X) == offset (Y)
	-1 iff offset (X) < offset (Y)  */
int
aarch64_ldrstr_offset_compare (const void *x, const void *y)
{
  const rtx * operands_1 = (const rtx *) x;
  const rtx * operands_2 = (const rtx *) y;
  rtx mem_1, mem_2, base, offset_1, offset_2;

  if (MEM_P (operands_1[0]))
    mem_1 = operands_1[0];
  else
    mem_1 = operands_1[1];

  if (MEM_P (operands_2[0]))
    mem_2 = operands_2[0];
  else
    mem_2 = operands_2[1];

  /* Extract the offsets.  */
  extract_base_offset_in_addr (mem_1, &base, &offset_1);
  extract_base_offset_in_addr (mem_2, &base, &offset_2);

  gcc_assert (offset_1 != NULL_RTX && offset_2 != NULL_RTX);

  return wi::cmps (INTVAL (offset_1), INTVAL (offset_2));
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into ldp/stp by adjusting the offset.  LOAD is true if they
   are load instructions.  MODE is the mode of memory operands.

   Given below consecutive stores:

     str  w1, [xb, 0x100]
     str  w1, [xb, 0x104]
     str  w1, [xb, 0x108]
     str  w1, [xb, 0x10c]

   Though the offsets are out of the range supported by stp, we can
   still pair them after adjusting the offset, like:

     add  scratch, xb, 0x100
     stp  w1, w1, [scratch]
     stp  w1, w1, [scratch, 0x8]

   The peephole patterns detecting this opportunity should guarantee
   the scratch register is avaliable.  */

bool
aarch64_operands_adjust_ok_for_ldpstp (rtx *operands, bool load,
				       scalar_mode mode)
{
  const int num_insns = 4;
  enum reg_class rclass;
  HOST_WIDE_INT offvals[num_insns], msize;
  rtx mem[num_insns], reg[num_insns], base[num_insns], offset[num_insns];

  if (load)
    {
      for (int i = 0; i < num_insns; i++)
	{
	  reg[i] = operands[2 * i];
	  mem[i] = operands[2 * i + 1];

	  gcc_assert (REG_P (reg[i]));
	}

      /* Do not attempt to merge the loads if the loads clobber each other.  */
      for (int i = 0; i < 8; i += 2)
	for (int j = i + 2; j < 8; j += 2)
	  if (reg_overlap_mentioned_p (operands[i], operands[j]))
	    return false;
    }
  else
    for (int i = 0; i < num_insns; i++)
      {
	mem[i] = operands[2 * i];
	reg[i] = operands[2 * i + 1];
      }

  /* Skip if memory operand is by itself valid for ldp/stp.  */
  if (!MEM_P (mem[0]) || aarch64_mem_pair_operand (mem[0], mode))
    return false;

  for (int i = 0; i < num_insns; i++)
    {
      /* The mems cannot be volatile.  */
      if (MEM_VOLATILE_P (mem[i]))
	return false;

      /* Check if the addresses are in the form of [base+offset].  */
      extract_base_offset_in_addr (mem[i], base + i, offset + i);
      if (base[i] == NULL_RTX || offset[i] == NULL_RTX)
	return false;
    }

  /* Check if the registers are of same class.  */
  rclass = REG_P (reg[0]) && FP_REGNUM_P (REGNO (reg[0]))
    ? FP_REGS : GENERAL_REGS;

  for (int i = 1; i < num_insns; i++)
    if (REG_P (reg[i]) && FP_REGNUM_P (REGNO (reg[i])))
      {
	if (rclass != FP_REGS)
	  return false;
      }
    else
      {
	if (rclass != GENERAL_REGS)
	  return false;
      }

  /* Only the last register in the order in which they occur
     may be clobbered by the load.  */
  if (rclass == GENERAL_REGS && load)
    for (int i = 0; i < num_insns - 1; i++)
      if (reg_mentioned_p (reg[i], mem[i]))
	return false;

  /* Check if the bases are same.  */
  for (int i = 0; i < num_insns - 1; i++)
    if (!rtx_equal_p (base[i], base[i + 1]))
      return false;

  for (int i = 0; i < num_insns; i++)
    offvals[i] = INTVAL (offset[i]);

  msize = GET_MODE_SIZE (mode);

  /* Check if the offsets can be put in the right order to do a ldp/stp.  */
  qsort (offvals, num_insns, sizeof (HOST_WIDE_INT),
	 aarch64_host_wide_int_compare);

  if (!(offvals[1] == offvals[0] + msize
	&& offvals[3] == offvals[2] + msize))
    return false;

  /* Check that offsets are within range of each other.  The ldp/stp
     instructions have 7 bit immediate offsets, so use 0x80.  */
  if (offvals[2] - offvals[0] >= msize * 0x80)
    return false;

  /* The offsets must be aligned with respect to each other.  */
  if (offvals[0] % msize != offvals[2] % msize)
    return false;

  /* If we have SImode and slow unaligned ldp,
     check the alignment to be at least 8 byte. */
  if (mode == SImode
      && (aarch64_tune_params.extra_tuning_flags
	  & AARCH64_EXTRA_TUNE_SLOW_UNALIGNED_LDPW)
      && !optimize_size
      && MEM_ALIGN (mem[0]) < 8 * BITS_PER_UNIT)
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store, this function pairs them
   into LDP/STP after adjusting the offset.  It depends on the fact
   that the operands can be sorted so the offsets are correct for STP.
   MODE is the mode of memory operands.  CODE is the rtl operator
   which should be applied to all memory operands, it's SIGN_EXTEND,
   ZERO_EXTEND or UNKNOWN.  */

bool
aarch64_gen_adjusted_ldpstp (rtx *operands, bool load,
			     scalar_mode mode, RTX_CODE code)
{
  rtx base, offset_1, offset_3, t1, t2;
  rtx mem_1, mem_2, mem_3, mem_4;
  rtx temp_operands[8];
  HOST_WIDE_INT off_val_1, off_val_3, base_off, new_off_1, new_off_3,
		stp_off_upper_limit, stp_off_lower_limit, msize;

  /* We make changes on a copy as we may still bail out.  */
  for (int i = 0; i < 8; i ++)
    temp_operands[i] = operands[i];

  /* Sort the operands.  */
  qsort (temp_operands, 4, 2 * sizeof (rtx *), aarch64_ldrstr_offset_compare);

  /* Copy the memory operands so that if we have to bail for some
     reason the original addresses are unchanged.  */
  if (load)
    {
      mem_1 = copy_rtx (temp_operands[1]);
      mem_2 = copy_rtx (temp_operands[3]);
      mem_3 = copy_rtx (temp_operands[5]);
      mem_4 = copy_rtx (temp_operands[7]);
    }
  else
    {
      mem_1 = copy_rtx (temp_operands[0]);
      mem_2 = copy_rtx (temp_operands[2]);
      mem_3 = copy_rtx (temp_operands[4]);
      mem_4 = copy_rtx (temp_operands[6]);
      gcc_assert (code == UNKNOWN);
    }

  extract_base_offset_in_addr (mem_1, &base, &offset_1);
  extract_base_offset_in_addr (mem_3, &base, &offset_3);
  gcc_assert (base != NULL_RTX && offset_1 != NULL_RTX
	      && offset_3 != NULL_RTX);

  /* Adjust offset so it can fit in LDP/STP instruction.  */
  msize = GET_MODE_SIZE (mode);
  stp_off_upper_limit = msize * (0x40 - 1);
  stp_off_lower_limit = - msize * 0x40;

  off_val_1 = INTVAL (offset_1);
  off_val_3 = INTVAL (offset_3);

  /* The base offset is optimally half way between the two STP/LDP offsets.  */
  if (msize <= 4)
    base_off = (off_val_1 + off_val_3) / 2;
  else
    /* However, due to issues with negative LDP/STP offset generation for
       larger modes, for DF, DI and vector modes. we must not use negative
       addresses smaller than 9 signed unadjusted bits can store.  This
       provides the most range in this case.  */
    base_off = off_val_1;

  /* Adjust the base so that it is aligned with the addresses but still
     optimal.  */
  if (base_off % msize != off_val_1 % msize)
    /* Fix the offset, bearing in mind we want to make it bigger not
       smaller.  */
    base_off += (((base_off % msize) - (off_val_1 % msize)) + msize) % msize;
  else if (msize <= 4)
    /* The negative range of LDP/STP is one larger than the positive range.  */
    base_off += msize;

  /* Check if base offset is too big or too small.  We can attempt to resolve
     this issue by setting it to the maximum value and seeing if the offsets
     still fit.  */
  if (base_off >= 0x1000)
    {
      base_off = 0x1000 - 1;
      /* We must still make sure that the base offset is aligned with respect
	 to the address.  But it may may not be made any bigger.  */
      base_off -= (((base_off % msize) - (off_val_1 % msize)) + msize) % msize;
    }

  /* Likewise for the case where the base is too small.  */
  if (base_off <= -0x1000)
    {
      base_off = -0x1000 + 1;
      base_off += (((base_off % msize) - (off_val_1 % msize)) + msize) % msize;
    }

  /* Offset of the first STP/LDP.  */
  new_off_1 = off_val_1 - base_off;

  /* Offset of the second STP/LDP.  */
  new_off_3 = off_val_3 - base_off;

  /* The offsets must be within the range of the LDP/STP instructions.  */
  if (new_off_1 > stp_off_upper_limit || new_off_1 < stp_off_lower_limit
      || new_off_3 > stp_off_upper_limit || new_off_3 < stp_off_lower_limit)
    return false;

  replace_equiv_address_nv (mem_1, plus_constant (Pmode, operands[8],
						  new_off_1), true);
  replace_equiv_address_nv (mem_2, plus_constant (Pmode, operands[8],
						  new_off_1 + msize), true);
  replace_equiv_address_nv (mem_3, plus_constant (Pmode, operands[8],
						  new_off_3), true);
  replace_equiv_address_nv (mem_4, plus_constant (Pmode, operands[8],
						  new_off_3 + msize), true);

  if (!aarch64_mem_pair_operand (mem_1, mode)
      || !aarch64_mem_pair_operand (mem_3, mode))
    return false;

  if (code == ZERO_EXTEND)
    {
      mem_1 = gen_rtx_ZERO_EXTEND (DImode, mem_1);
      mem_2 = gen_rtx_ZERO_EXTEND (DImode, mem_2);
      mem_3 = gen_rtx_ZERO_EXTEND (DImode, mem_3);
      mem_4 = gen_rtx_ZERO_EXTEND (DImode, mem_4);
    }
  else if (code == SIGN_EXTEND)
    {
      mem_1 = gen_rtx_SIGN_EXTEND (DImode, mem_1);
      mem_2 = gen_rtx_SIGN_EXTEND (DImode, mem_2);
      mem_3 = gen_rtx_SIGN_EXTEND (DImode, mem_3);
      mem_4 = gen_rtx_SIGN_EXTEND (DImode, mem_4);
    }

  if (load)
    {
      operands[0] = temp_operands[0];
      operands[1] = mem_1;
      operands[2] = temp_operands[2];
      operands[3] = mem_2;
      operands[4] = temp_operands[4];
      operands[5] = mem_3;
      operands[6] = temp_operands[6];
      operands[7] = mem_4;
    }
  else
    {
      operands[0] = mem_1;
      operands[1] = temp_operands[1];
      operands[2] = mem_2;
      operands[3] = temp_operands[3];
      operands[4] = mem_3;
      operands[5] = temp_operands[5];
      operands[6] = mem_4;
      operands[7] = temp_operands[7];
    }

  /* Emit adjusting instruction.  */
  emit_insn (gen_rtx_SET (operands[8], plus_constant (DImode, base, base_off)));
  /* Emit ldp/stp instructions.  */
  t1 = gen_rtx_SET (operands[0], operands[1]);
  t2 = gen_rtx_SET (operands[2], operands[3]);
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, t1, t2)));
  t1 = gen_rtx_SET (operands[4], operands[5]);
  t2 = gen_rtx_SET (operands[6], operands[7]);
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, t1, t2)));
  return true;
}

/* Implement TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE.  Assume for now that
   it isn't worth branching around empty masked ops (including masked
   stores).  */

static bool
aarch64_empty_mask_is_expensive (unsigned)
{
  return false;
}

/* Return 1 if pseudo register should be created and used to hold
   GOT address for PIC code.  */

bool
aarch64_use_pseudo_pic_reg (void)
{
  return aarch64_cmodel == AARCH64_CMODEL_SMALL_SPIC;
}

/* Implement TARGET_UNSPEC_MAY_TRAP_P.  */

static int
aarch64_unspec_may_trap_p (const_rtx x, unsigned flags)
{
  switch (XINT (x, 1))
    {
    case UNSPEC_GOTSMALLPIC:
    case UNSPEC_GOTSMALLPIC28K:
    case UNSPEC_GOTTINYPIC:
      return 0;
    default:
      break;
    }

  return default_unspec_may_trap_p (x, flags);
}


/* If X is a positive CONST_DOUBLE with a value that is a power of 2
   return the log2 of that value.  Otherwise return -1.  */

int
aarch64_fpconst_pow_of_2 (rtx x)
{
  const REAL_VALUE_TYPE *r;

  if (!CONST_DOUBLE_P (x))
    return -1;

  r = CONST_DOUBLE_REAL_VALUE (x);

  if (REAL_VALUE_NEGATIVE (*r)
      || REAL_VALUE_ISNAN (*r)
      || REAL_VALUE_ISINF (*r)
      || !real_isinteger (r, DFmode))
    return -1;

  return exact_log2 (real_to_integer (r));
}

/* If X is a positive CONST_DOUBLE with a value that is the reciprocal of a
   power of 2 (i.e 1/2^n) return the number of float bits. e.g. for x==(1/2^n)
   return n. Otherwise return -1.  */

int
aarch64_fpconst_pow2_recip (rtx x)
{
  REAL_VALUE_TYPE r0;

  if (!CONST_DOUBLE_P (x))
    return -1;

  r0 = *CONST_DOUBLE_REAL_VALUE (x);
  if (exact_real_inverse (DFmode, &r0)
      && !REAL_VALUE_NEGATIVE (r0))
    {
	int ret = exact_log2 (real_to_integer (&r0));
	if (ret >= 1 && ret <= 32)
	    return ret;
    }
  return -1;
}

/* If X is a vector of equal CONST_DOUBLE values and that value is
   Y, return the aarch64_fpconst_pow_of_2 of Y.  Otherwise return -1.  */

int
aarch64_vec_fpconst_pow_of_2 (rtx x)
{
  int nelts;
  if (GET_CODE (x) != CONST_VECTOR
      || !CONST_VECTOR_NUNITS (x).is_constant (&nelts))
    return -1;

  if (GET_MODE_CLASS (GET_MODE (x)) != MODE_VECTOR_FLOAT)
    return -1;

  int firstval = aarch64_fpconst_pow_of_2 (CONST_VECTOR_ELT (x, 0));
  if (firstval <= 0)
    return -1;

  for (int i = 1; i < nelts; i++)
    if (aarch64_fpconst_pow_of_2 (CONST_VECTOR_ELT (x, i)) != firstval)
      return -1;

  return firstval;
}

/* Implement TARGET_PROMOTED_TYPE to promote 16-bit floating point types
   to float.

   __fp16 always promotes through this hook.
   _Float16 may promote if TARGET_FLT_EVAL_METHOD is 16, but we do that
   through the generic excess precision logic rather than here.  */

static tree
aarch64_promoted_type (const_tree t)
{
  if (SCALAR_FLOAT_TYPE_P (t)
      && TYPE_MAIN_VARIANT (t) == aarch64_fp16_type_node)
    return float_type_node;

  return NULL_TREE;
}

/* Implement the TARGET_OPTAB_SUPPORTED_P hook.  */

static bool
aarch64_optab_supported_p (int op, machine_mode mode1, machine_mode,
			   optimization_type opt_type)
{
  switch (op)
    {
    case rsqrt_optab:
      return opt_type == OPTIMIZE_FOR_SPEED && use_rsqrt_p (mode1);

    default:
      return true;
    }
}

/* Implement the TARGET_DWARF_POLY_INDETERMINATE_VALUE hook.  */

static unsigned int
aarch64_dwarf_poly_indeterminate_value (unsigned int i, unsigned int *factor,
					int *offset)
{
  /* Polynomial invariant 1 == (VG / 2) - 1.  */
  gcc_assert (i == 1);
  *factor = 2;
  *offset = 1;
  return AARCH64_DWARF_VG;
}

/* Implement TARGET_LIBGCC_FLOATING_POINT_MODE_SUPPORTED_P - return TRUE
   if MODE is HFmode, and punt to the generic implementation otherwise.  */

static bool
aarch64_libgcc_floating_mode_supported_p (scalar_float_mode mode)
{
  return (mode == HFmode
	  ? true
	  : default_libgcc_floating_mode_supported_p (mode));
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P - return TRUE
   if MODE is HFmode, and punt to the generic implementation otherwise.  */

static bool
aarch64_scalar_mode_supported_p (scalar_mode mode)
{
  return (mode == HFmode
	  ? true
	  : default_scalar_mode_supported_p (mode));
}

/* Set the value of FLT_EVAL_METHOD.
   ISO/IEC TS 18661-3 defines two values that we'd like to make use of:

    0: evaluate all operations and constants, whose semantic type has at
       most the range and precision of type float, to the range and
       precision of float; evaluate all other operations and constants to
       the range and precision of the semantic type;

    N, where _FloatN is a supported interchange floating type
       evaluate all operations and constants, whose semantic type has at
       most the range and precision of _FloatN type, to the range and
       precision of the _FloatN type; evaluate all other operations and
       constants to the range and precision of the semantic type;

   If we have the ARMv8.2-A extensions then we support _Float16 in native
   precision, so we should set this to 16.  Otherwise, we support the type,
   but want to evaluate expressions in float precision, so set this to
   0.  */

static enum flt_eval_method
aarch64_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
      case EXCESS_PRECISION_TYPE_FAST:
      case EXCESS_PRECISION_TYPE_STANDARD:
	/* We can calculate either in 16-bit range and precision or
	   32-bit range and precision.  Make that decision based on whether
	   we have native support for the ARMv8.2-A 16-bit floating-point
	   instructions or not.  */
	return (TARGET_FP_F16INST
		? FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16
		: FLT_EVAL_METHOD_PROMOTE_TO_FLOAT);
      case EXCESS_PRECISION_TYPE_IMPLICIT:
	return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16;
      default:
	gcc_unreachable ();
    }
  return FLT_EVAL_METHOD_UNPREDICTABLE;
}

/* Implement TARGET_SCHED_CAN_SPECULATE_INSN.  Return true if INSN can be
   scheduled for speculative execution.  Reject the long-running division
   and square-root instructions.  */

static bool
aarch64_sched_can_speculate_insn (rtx_insn *insn)
{
  switch (get_attr_type (insn))
    {
      case TYPE_SDIV:
      case TYPE_UDIV:
      case TYPE_FDIVS:
      case TYPE_FDIVD:
      case TYPE_FSQRTS:
      case TYPE_FSQRTD:
      case TYPE_NEON_FP_SQRT_S:
      case TYPE_NEON_FP_SQRT_D:
      case TYPE_NEON_FP_SQRT_S_Q:
      case TYPE_NEON_FP_SQRT_D_Q:
      case TYPE_NEON_FP_DIV_S:
      case TYPE_NEON_FP_DIV_D:
      case TYPE_NEON_FP_DIV_S_Q:
      case TYPE_NEON_FP_DIV_D_Q:
	return false;
      default:
	return true;
    }
}

/* Implement TARGET_COMPUTE_PRESSURE_CLASSES.  */

static int
aarch64_compute_pressure_classes (reg_class *classes)
{
  int i = 0;
  classes[i++] = GENERAL_REGS;
  classes[i++] = FP_REGS;
  /* PR_REGS isn't a useful pressure class because many predicate pseudo
     registers need to go in PR_LO_REGS at some point during their
     lifetime.  Splitting it into two halves has the effect of making
     all predicates count against PR_LO_REGS, so that we try whenever
     possible to restrict the number of live predicates to 8.  This
     greatly reduces the amount of spilling in certain loops.  */
  classes[i++] = PR_LO_REGS;
  classes[i++] = PR_HI_REGS;
  return i;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
aarch64_can_change_mode_class (machine_mode from,
			       machine_mode to, reg_class_t)
{
  if (BYTES_BIG_ENDIAN)
    {
      bool from_sve_p = aarch64_sve_data_mode_p (from);
      bool to_sve_p = aarch64_sve_data_mode_p (to);

      /* Don't allow changes between SVE data modes and non-SVE modes.
	 See the comment at the head of aarch64-sve.md for details.  */
      if (from_sve_p != to_sve_p)
	return false;

      /* Don't allow changes in element size: lane 0 of the new vector
	 would not then be lane 0 of the old vector.  See the comment
	 above aarch64_maybe_expand_sve_subreg_move for a more detailed
	 description.

	 In the worst case, this forces a register to be spilled in
	 one mode and reloaded in the other, which handles the
	 endianness correctly.  */
      if (from_sve_p && GET_MODE_UNIT_SIZE (from) != GET_MODE_UNIT_SIZE (to))
	return false;
    }
  return true;
}

/* Implement TARGET_EARLY_REMAT_MODES.  */

static void
aarch64_select_early_remat_modes (sbitmap modes)
{
  /* SVE values are not normally live across a call, so it should be
     worth doing early rematerialization even in VL-specific mode.  */
  for (int i = 0; i < NUM_MACHINE_MODES; ++i)
    if (aarch64_sve_mode_p ((machine_mode) i))
      bitmap_set_bit (modes, i);
}

/* Override the default target speculation_safe_value.  */
static rtx
aarch64_speculation_safe_value (machine_mode mode,
				rtx result, rtx val, rtx failval)
{
  /* Maybe we should warn if falling back to hard barriers.  They are
     likely to be noticably more expensive than the alternative below.  */
  if (!aarch64_track_speculation)
    return default_speculation_safe_value (mode, result, val, failval);

  if (!REG_P (val))
    val = copy_to_mode_reg (mode, val);

  if (!aarch64_reg_or_zero (failval, mode))
    failval = copy_to_mode_reg (mode, failval);

  emit_insn (gen_despeculate_copy (mode, result, val, failval));
  return result;
}

/* Implement TARGET_ESTIMATED_POLY_VALUE.
   Look into the tuning structure for an estimate.
   VAL.coeffs[1] is multiplied by the number of VQ chunks over the initial
   Advanced SIMD 128 bits.  */

static HOST_WIDE_INT
aarch64_estimated_poly_value (poly_int64 val)
{
  enum aarch64_sve_vector_bits_enum width_source
    = aarch64_tune_params.sve_width;

  /* If we still don't have an estimate, use the default.  */
  if (width_source == SVE_SCALABLE)
    return default_estimated_poly_value (val);

  HOST_WIDE_INT over_128 = width_source - 128;
  return val.coeffs[0] + val.coeffs[1] * over_128 / 128;
}


/* Return true for types that could be supported as SIMD return or
   argument types.  */

static bool
supported_simd_type (tree t)
{
  if (SCALAR_FLOAT_TYPE_P (t) || INTEGRAL_TYPE_P (t) || POINTER_TYPE_P (t))
    {
      HOST_WIDE_INT s = tree_to_shwi (TYPE_SIZE_UNIT (t));
      return s == 1 || s == 2 || s == 4 || s == 8;
    }
  return false;
}

/* Return true for types that currently are supported as SIMD return
   or argument types.  */

static bool
currently_supported_simd_type (tree t, tree b)
{
  if (COMPLEX_FLOAT_TYPE_P (t))
    return false;

  if (TYPE_SIZE (t) != TYPE_SIZE (b))
    return false;

  return supported_simd_type (t);
}

/* Implement TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN.  */

static int
aarch64_simd_clone_compute_vecsize_and_simdlen (struct cgraph_node *node,
					struct cgraph_simd_clone *clonei,
					tree base_type, int num)
{
  tree t, ret_type, arg_type;
  unsigned int elt_bits, vec_bits, count;

  if (!TARGET_SIMD)
    return 0;

  if (clonei->simdlen
      && (clonei->simdlen < 2
	  || clonei->simdlen > 1024
	  || (clonei->simdlen & (clonei->simdlen - 1)) != 0))
    {
      warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		  "unsupported simdlen %d", clonei->simdlen);
      return 0;
    }

  ret_type = TREE_TYPE (TREE_TYPE (node->decl));
  if (TREE_CODE (ret_type) != VOID_TYPE
      && !currently_supported_simd_type (ret_type, base_type))
    {
      if (TYPE_SIZE (ret_type) != TYPE_SIZE (base_type))
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "GCC does not currently support mixed size types "
		    "for %<simd%> functions");
      else if (supported_simd_type (ret_type))
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "GCC does not currently support return type %qT "
		    "for %<simd%> functions", ret_type);
      else
	warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		    "unsupported return type %qT for %<simd%> functions",
		    ret_type);
      return 0;
    }

  for (t = DECL_ARGUMENTS (node->decl); t; t = DECL_CHAIN (t))
    {
      arg_type = TREE_TYPE (t);

      if (!currently_supported_simd_type (arg_type, base_type))
	{
	  if (TYPE_SIZE (arg_type) != TYPE_SIZE (base_type))
	    warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			"GCC does not currently support mixed size types "
			"for %<simd%> functions");
	  else
	    warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
			"GCC does not currently support argument type %qT "
			"for %<simd%> functions", arg_type);
	  return 0;
	}
    }

  clonei->vecsize_mangle = 'n';
  clonei->mask_mode = VOIDmode;
  elt_bits = GET_MODE_BITSIZE (SCALAR_TYPE_MODE (base_type));
  if (clonei->simdlen == 0)
    {
      count = 2;
      vec_bits = (num == 0 ? 64 : 128);
      clonei->simdlen = vec_bits / elt_bits;
    }
  else
    {
      count = 1;
      vec_bits = clonei->simdlen * elt_bits;
      if (vec_bits != 64 && vec_bits != 128)
	{
	  warning_at (DECL_SOURCE_LOCATION (node->decl), 0,
		      "GCC does not currently support simdlen %d for type %qT",
		      clonei->simdlen, base_type);
	  return 0;
	}
    }
  clonei->vecsize_int = vec_bits;
  clonei->vecsize_float = vec_bits;
  return count;
}

/* Implement TARGET_SIMD_CLONE_ADJUST.  */

static void
aarch64_simd_clone_adjust (struct cgraph_node *node)
{
  /* Add aarch64_vector_pcs target attribute to SIMD clones so they
     use the correct ABI.  */

  tree t = TREE_TYPE (node->decl);
  TYPE_ATTRIBUTES (t) = make_attribute ("aarch64_vector_pcs", "default",
					TYPE_ATTRIBUTES (t));
}

/* Implement TARGET_SIMD_CLONE_USABLE.  */

static int
aarch64_simd_clone_usable (struct cgraph_node *node)
{
  switch (node->simdclone->vecsize_mangle)
    {
    case 'n':
      if (!TARGET_SIMD)
	return -1;
      return 0;
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_COMP_TYPE_ATTRIBUTES */

static int
aarch64_comp_type_attributes (const_tree type1, const_tree type2)
{
  if (lookup_attribute ("aarch64_vector_pcs", TYPE_ATTRIBUTES (type1))
      != lookup_attribute ("aarch64_vector_pcs", TYPE_ATTRIBUTES (type2)))
    return 0;
  return 1;
}

/* Implement TARGET_GET_MULTILIB_ABI_NAME */

static const char *
aarch64_get_multilib_abi_name (void)
{
  if (TARGET_BIG_END)
    return TARGET_ILP32 ? "aarch64_be_ilp32" : "aarch64_be";
  return TARGET_ILP32 ? "aarch64_ilp32" : "aarch64";
}

/* Implement TARGET_STACK_PROTECT_GUARD. In case of a
   global variable based guard use the default else
   return a null tree.  */
static tree
aarch64_stack_protect_guard (void)
{
  if (aarch64_stack_protector_guard == SSP_GLOBAL)
    return default_stack_protect_guard ();

  return NULL_TREE;
}

/* Implement TARGET_ASM_FILE_END for AArch64.  This adds the AArch64 GNU NOTE
   section at the end if needed.  */
#define GNU_PROPERTY_AARCH64_FEATURE_1_AND	0xc0000000
#define GNU_PROPERTY_AARCH64_FEATURE_1_BTI	(1U << 0)
#define GNU_PROPERTY_AARCH64_FEATURE_1_PAC	(1U << 1)
void
aarch64_file_end_indicate_exec_stack ()
{
  file_end_indicate_exec_stack ();

  unsigned feature_1_and = 0;
  if (aarch64_bti_enabled ())
    feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_BTI;

  if (aarch64_ra_sign_scope != AARCH64_FUNCTION_NONE)
    feature_1_and |= GNU_PROPERTY_AARCH64_FEATURE_1_PAC;

  if (feature_1_and)
    {
      /* Generate .note.gnu.property section.  */
      switch_to_section (get_section (".note.gnu.property",
				      SECTION_NOTYPE, NULL));

      /* PT_NOTE header: namesz, descsz, type.
	 namesz = 4 ("GNU\0")
	 descsz = 16 (Size of the program property array)
		  [(12 + padding) * Number of array elements]
	 type   = 5 (NT_GNU_PROPERTY_TYPE_0).  */
      assemble_align (POINTER_SIZE);
      assemble_integer (GEN_INT (4), 4, 32, 1);
      assemble_integer (GEN_INT (ROUND_UP (12, POINTER_BYTES)), 4, 32, 1);
      assemble_integer (GEN_INT (5), 4, 32, 1);

      /* PT_NOTE name.  */
      assemble_string ("GNU", 4);

      /* PT_NOTE contents for NT_GNU_PROPERTY_TYPE_0:
	 type   = GNU_PROPERTY_AARCH64_FEATURE_1_AND
	 datasz = 4
	 data   = feature_1_and.  */
      assemble_integer (GEN_INT (GNU_PROPERTY_AARCH64_FEATURE_1_AND), 4, 32, 1);
      assemble_integer (GEN_INT (4), 4, 32, 1);
      assemble_integer (GEN_INT (feature_1_and), 4, 32, 1);

      /* Pad the size of the note to the required alignment.  */
      assemble_align (POINTER_SIZE);
    }
}
#undef GNU_PROPERTY_AARCH64_FEATURE_1_PAC
#undef GNU_PROPERTY_AARCH64_FEATURE_1_BTI
#undef GNU_PROPERTY_AARCH64_FEATURE_1_AND

/* Target-specific selftests.  */

#if CHECKING_P

namespace selftest {

/* Selftest for the RTL loader.
   Verify that the RTL loader copes with a dump from
   print_rtx_function.  This is essentially just a test that class
   function_reader can handle a real dump, but it also verifies
   that lookup_reg_by_dump_name correctly handles hard regs.
   The presence of hard reg names in the dump means that the test is
   target-specific, hence it is in this file.  */

static void
aarch64_test_loading_full_dump ()
{
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("aarch64/times-two.rtl"));

  ASSERT_STREQ ("times_two", IDENTIFIER_POINTER (DECL_NAME (cfun->decl)));

  rtx_insn *insn_1 = get_insn_by_uid (1);
  ASSERT_EQ (NOTE, GET_CODE (insn_1));

  rtx_insn *insn_15 = get_insn_by_uid (15);
  ASSERT_EQ (INSN, GET_CODE (insn_15));
  ASSERT_EQ (USE, GET_CODE (PATTERN (insn_15)));

  /* Verify crtl->return_rtx.  */
  ASSERT_EQ (REG, GET_CODE (crtl->return_rtx));
  ASSERT_EQ (0, REGNO (crtl->return_rtx));
  ASSERT_EQ (SImode, GET_MODE (crtl->return_rtx));
}

/* Run all target-specific selftests.  */

static void
aarch64_run_selftests (void)
{
  aarch64_test_loading_full_dump ();
}

} // namespace selftest

#endif /* #if CHECKING_P */

#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD aarch64_stack_protect_guard

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST aarch64_address_cost

/* This hook will determines whether unnamed bitfields affect the alignment
   of the containing structure.  The hook returns true if the structure
   should inherit the alignment requirements of an unnamed bitfield's
   type.  */
#undef TARGET_ALIGN_ANON_BITFIELD
#define TARGET_ALIGN_ANON_BITFIELD hook_bool_void_true

#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.xword\t"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START aarch64_start_file

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK aarch64_output_mi_thunk

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION aarch64_select_rtx_section

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE aarch64_asm_trampoline_template

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST aarch64_build_builtin_va_list

#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_arg_info_false

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE aarch64_can_eliminate

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P aarch64_can_inline_p

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM aarch64_cannot_force_const_mem

#undef TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD aarch64_case_values_threshold

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE aarch64_conditional_register_usage

/* Only the least significant bit is used for initialization guard
   variables.  */
#undef TARGET_CXX_GUARD_MASK_BIT
#define TARGET_CXX_GUARD_MASK_BIT hook_bool_void_true

#undef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX aarch64_c_mode_for_suffix

#ifdef TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_END)
#endif

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS aarch64_class_max_nregs

#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL aarch64_builtin_decl

#undef TARGET_BUILTIN_RECIPROCAL
#define TARGET_BUILTIN_RECIPROCAL aarch64_builtin_reciprocal

#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION aarch64_excess_precision

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN aarch64_expand_builtin

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START aarch64_expand_builtin_va_start

#undef TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN aarch64_fold_builtin

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG aarch64_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE aarch64_function_arg_advance

#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY aarch64_function_arg_boundary

#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING aarch64_function_arg_padding

#undef TARGET_GET_RAW_RESULT_MODE
#define TARGET_GET_RAW_RESULT_MODE aarch64_get_reg_raw_mode
#undef TARGET_GET_RAW_ARG_MODE
#define TARGET_GET_RAW_ARG_MODE aarch64_get_reg_raw_mode

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL aarch64_function_ok_for_sibcall

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE aarch64_function_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P aarch64_function_value_regno_p

#undef TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN aarch64_gimple_fold_builtin

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR aarch64_gimplify_va_arg_expr

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  aarch64_init_builtins

#undef TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS
#define TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS \
  aarch64_ira_change_pseudo_allocno_class

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P aarch64_legitimate_address_hook_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P aarch64_legitimate_constant_p

#undef TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT
#define TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT \
  aarch64_legitimize_address_displacement

#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE aarch64_libgcc_cmp_return_mode

#undef TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P
#define TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P \
aarch64_libgcc_floating_mode_supported_p

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE aarch64_mangle_type

#undef TARGET_VERIFY_TYPE_CONTEXT
#define TARGET_VERIFY_TYPE_CONTEXT aarch64_verify_type_context

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST aarch64_memory_move_cost

#undef TARGET_MIN_DIVISIONS_FOR_RECIP_MUL
#define TARGET_MIN_DIVISIONS_FOR_RECIP_MUL aarch64_min_divisions_for_recip_mul

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

/* This target hook should return true if accesses to volatile bitfields
   should use the narrowest mode possible.  It should return false if these
   accesses should use the bitfield container type.  */
#undef TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE aarch64_override_options

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE \
  aarch64_override_options_after_change

#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE aarch64_option_save

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE aarch64_option_restore

#undef TARGET_OPTION_PRINT
#define TARGET_OPTION_PRINT aarch64_option_print

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P aarch64_option_valid_attribute_p

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION aarch64_set_current_function

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE aarch64_pass_by_reference

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS aarch64_preferred_reload_class

#undef TARGET_SCHED_REASSOCIATION_WIDTH
#define TARGET_SCHED_REASSOCIATION_WIDTH aarch64_reassociation_width

#undef TARGET_PROMOTED_TYPE
#define TARGET_PROMOTED_TYPE aarch64_promoted_type

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD aarch64_secondary_reload

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK aarch64_shift_truncation_mask

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS aarch64_setup_incoming_varargs

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX   aarch64_struct_value_rtx

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST aarch64_register_move_cost

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY aarch64_return_in_memory

#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB aarch64_return_in_msb

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS aarch64_rtx_costs_wrapper

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P aarch64_scalar_mode_supported_p

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE aarch64_sched_issue_rate

#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE aarch64_sched_variable_issue

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  aarch64_sched_first_cycle_multipass_dfa_lookahead

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD \
  aarch64_first_cycle_multipass_dfa_lookahead_guard

#undef TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS
#define TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS \
  aarch64_get_separate_components

#undef TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB
#define TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB \
  aarch64_components_for_bb

#undef TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS
#define TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS \
  aarch64_disqualify_components

#undef TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS \
  aarch64_emit_prologue_components

#undef TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS \
  aarch64_emit_epilogue_components

#undef TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS
#define TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS \
  aarch64_set_handled_components

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT aarch64_trampoline_init

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P aarch64_use_blocks_for_constant_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P aarch64_vector_mode_supported_p

#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT \
  aarch64_builtin_support_vector_misalignment

#undef TARGET_ARRAY_MODE
#define TARGET_ARRAY_MODE aarch64_array_mode

#undef TARGET_ARRAY_MODE_SUPPORTED_P
#define TARGET_ARRAY_MODE_SUPPORTED_P aarch64_array_mode_supported_p

#undef TARGET_VECTORIZE_ADD_STMT_COST
#define TARGET_VECTORIZE_ADD_STMT_COST aarch64_add_stmt_cost

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  aarch64_builtin_vectorization_cost

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE aarch64_preferred_simd_mode

#undef TARGET_VECTORIZE_BUILTINS
#define TARGET_VECTORIZE_BUILTINS

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  aarch64_builtin_vectorized_function

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES \
  aarch64_autovectorize_vector_modes

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV \
  aarch64_atomic_assign_expand_fenv

/* Section anchor support.  */

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -256

/* Limit the maximum anchor offset to 4k-1, since that's the limit for a
   byte offset; we can do much more for larger data types, but have no way
   to determine the size of the access.  We assume accesses are aligned.  */
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 4095

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT aarch64_simd_vector_alignment

#undef TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT
#define TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT \
  aarch64_vectorize_preferred_vector_alignment
#undef TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE \
  aarch64_simd_vector_alignment_reachable

/* vec_perm support.  */

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST \
  aarch64_vectorize_vec_perm_const

#undef TARGET_VECTORIZE_RELATED_MODE
#define TARGET_VECTORIZE_RELATED_MODE aarch64_vectorize_related_mode
#undef TARGET_VECTORIZE_GET_MASK_MODE
#define TARGET_VECTORIZE_GET_MASK_MODE aarch64_get_mask_mode
#undef TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE
#define TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE \
  aarch64_empty_mask_is_expensive
#undef TARGET_PREFERRED_ELSE_VALUE
#define TARGET_PREFERRED_ELSE_VALUE \
  aarch64_preferred_else_value

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS aarch64_init_libfuncs

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS aarch64_fixed_condition_code_regs

#undef TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM CC_REGNUM

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET aarch64_asan_shadow_offset

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS aarch64_legitimize_address

#undef TARGET_SCHED_CAN_SPECULATE_INSN
#define TARGET_SCHED_CAN_SPECULATE_INSN aarch64_sched_can_speculate_insn

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost

#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY aarch64_sched_adjust_priority

#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P aarch64_macro_fusion_p

#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P aarch_macro_fusion_pair_p

#undef TARGET_SCHED_FUSION_PRIORITY
#define TARGET_SCHED_FUSION_PRIORITY aarch64_sched_fusion_priority

#undef TARGET_UNSPEC_MAY_TRAP_P
#define TARGET_UNSPEC_MAY_TRAP_P aarch64_unspec_may_trap_p

#undef TARGET_USE_PSEUDO_PIC_REG
#define TARGET_USE_PSEUDO_PIC_REG aarch64_use_pseudo_pic_reg

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND aarch64_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS aarch64_print_operand_address

#undef TARGET_OPTAB_SUPPORTED_P
#define TARGET_OPTAB_SUPPORTED_P aarch64_optab_supported_p

#undef TARGET_OMIT_STRUCT_RETURN_REG
#define TARGET_OMIT_STRUCT_RETURN_REG true

#undef TARGET_DWARF_POLY_INDETERMINATE_VALUE
#define TARGET_DWARF_POLY_INDETERMINATE_VALUE \
  aarch64_dwarf_poly_indeterminate_value

/* The architecture reserves bits 0 and 1 so use bit 2 for descriptors.  */
#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 4

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS aarch64_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK aarch64_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P aarch64_modes_tieable_p

#undef TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  aarch64_hard_regno_call_part_clobbered

#undef TARGET_INSN_CALLEE_ABI
#define TARGET_INSN_CALLEE_ABI aarch64_insn_callee_abi

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT aarch64_constant_alignment

#undef TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE
#define TARGET_STACK_CLASH_PROTECTION_ALLOCA_PROBE_RANGE \
  aarch64_stack_clash_protection_alloca_probe_range

#undef TARGET_COMPUTE_PRESSURE_CLASSES
#define TARGET_COMPUTE_PRESSURE_CLASSES aarch64_compute_pressure_classes

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS aarch64_can_change_mode_class

#undef TARGET_SELECT_EARLY_REMAT_MODES
#define TARGET_SELECT_EARLY_REMAT_MODES aarch64_select_early_remat_modes

#undef TARGET_SPECULATION_SAFE_VALUE
#define TARGET_SPECULATION_SAFE_VALUE aarch64_speculation_safe_value

#undef TARGET_ESTIMATED_POLY_VALUE
#define TARGET_ESTIMATED_POLY_VALUE aarch64_estimated_poly_value

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE aarch64_attribute_table

#undef TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN
#define TARGET_SIMD_CLONE_COMPUTE_VECSIZE_AND_SIMDLEN \
  aarch64_simd_clone_compute_vecsize_and_simdlen

#undef TARGET_SIMD_CLONE_ADJUST
#define TARGET_SIMD_CLONE_ADJUST aarch64_simd_clone_adjust

#undef TARGET_SIMD_CLONE_USABLE
#define TARGET_SIMD_CLONE_USABLE aarch64_simd_clone_usable

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES aarch64_comp_type_attributes

#undef TARGET_GET_MULTILIB_ABI_NAME
#define TARGET_GET_MULTILIB_ABI_NAME aarch64_get_multilib_abi_name

#undef TARGET_FNTYPE_ABI
#define TARGET_FNTYPE_ABI aarch64_fntype_abi

#if CHECKING_P
#undef TARGET_RUN_TARGET_SELFTESTS
#define TARGET_RUN_TARGET_SELFTESTS selftest::aarch64_run_selftests
#endif /* #if CHECKING_P */

#undef TARGET_ASM_POST_CFI_STARTPROC
#define TARGET_ASM_POST_CFI_STARTPROC aarch64_post_cfi_startproc

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST arm_md_asm_adjust

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-aarch64.h"
