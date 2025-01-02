/* GCC backend functions for C-SKY targets.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "c-family/c-common.h"
#include "cpplib.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "sched-int.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "intl.h"
#include "libfuncs.h"
#include "opts.h"
#include "dumpfile.h"
#include "target-globals.h"
#include "builtins.h"
#include "tm-constrs.h"
#include "rtl-iter.h"
#include "pass_manager.h"
#include "tree-pass.h"
#include "context.h"
#include "opts.h"

/* This file should be included last.  */
#include "target-def.h"

/* Stack and register size macros.  */

#define CSKY_NUM_WORDS(SIZE) \
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
#define CSKY_NUM_REGS(MODE) \
  CSKY_NUM_WORDS (GET_MODE_SIZE (MODE))
#define CSKY_STACK_ALIGN(SIZE) \
  (CSKY_NUM_WORDS (SIZE) * UNITS_PER_WORD)

/* Offsets and range macros.  */

#define CSKY_LD16_MAX_OFFSET(MODE)		\
  (31 * GET_MODE_SIZE (MODE))
#define CSKY_LD32_MAX_OFFSET(MODE) \
  (4095 * GET_MODE_SIZE (MODE))
#define CSKY_LD16_OFFSET_MASK(MODE) \
  (CSKY_LD16_MAX_OFFSET (MODE) + GET_MODE_SIZE (MODE) - 1)

#define CSKY_ADDI16_MAX_IMM	      256
#define CSKY_SUBI16_MAX_IMM	      256

#define CSKY_CONSTPOOL_LABEL_PREFIX   "LCP"

/* Array of the smallest class containing reg number REGNO, indexed by
   REGNO.  Used by REGNO_REG_CLASS.  */
enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER] =
{
  /* Registers r0-r7.  */
  MINI_REGS,	 MINI_REGS,	MINI_REGS,     MINI_REGS,
  MINI_REGS,	 MINI_REGS,	MINI_REGS,     MINI_REGS,
  /* Registers r8-r15.  */
  LOW_REGS,	 LOW_REGS,	LOW_REGS,      LOW_REGS,
  LOW_REGS,	 LOW_REGS,	SP_REGS,       LOW_REGS,
  /* Registers r16-r31.  */
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  GENERAL_REGS, GENERAL_REGS, GENERAL_REGS, GENERAL_REGS,
  /* Reserved.  */
  RESERVE_REGS,
  /* CC,HI,LO registers.  */
  C_REGS,      HILO_REGS,     HILO_REGS,
  /* Reserved.  */
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  /* Vec registers.  */
  V_REGS,	V_REGS,	      V_REGS,	    V_REGS,
  V_REGS,	V_REGS,	      V_REGS,	    V_REGS,
  V_REGS,	V_REGS,	      V_REGS,	    V_REGS,
  V_REGS,	V_REGS,	      V_REGS,	    V_REGS,
  /* Reserved.  */
  RESERVE_REGS, RESERVE_REGS,
  /* Register epc.  */
  OTHER_REGS,
  /* Vec registers.  */
  V_REGS,       V_REGS,       V_REGS,       V_REGS,
  V_REGS,       V_REGS,       V_REGS,       V_REGS,
  V_REGS,       V_REGS,       V_REGS,       V_REGS,
  V_REGS,       V_REGS,       V_REGS,       V_REGS,
  /* Reserved.  */
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  /* Reserved.  */
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,

  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,

  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,
  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS, RESERVE_REGS,

  RESERVE_REGS, RESERVE_REGS, RESERVE_REGS
};

/* Arrays that map GCC register numbers to debugger register numbers,
   '-1' means that is INVALID_REGNUM.
   TODO: which rules according to here ?  */
const int csky_debugger_regno[FIRST_PSEUDO_REGISTER] =
{
  0,  1,  2,  3,  4,  5,  6,  7,
  8,  9,  10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22, 23,
  24, 25, 26, 27, 28, 29, 30, 31,
  -1, -1, 36, 37,
  75,  79,  83,  87,  91,  95,  99,  103,
  107, 111, 115, 119, 123, 127, 131, 135,
  74,  78,  82,  86,  90,  94,  98,  102,
  106, 110, 114, 118, 122, 126, 130, 134,
  -1, -1, 72,
  /* vr: 71 - 86 */
  139,  143,  147,  151,  155,  159,  163,  167,
  171,  175,  179,  183,  187,  191,  195,  199,
  138,  142,  146,  150,  154,  158,  162,  166,
  170,  174,  178,  182,  186,  190,  194,  198,
  /* resereved */
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,

  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,

  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,
  -1,   -1,   -1,   -1,   -1,   -1,   -1,  -1,

  -1,   -1,   -1
};

/* Table of machine attributes.  */
static tree csky_handle_fndecl_attribute (tree *, tree, tree, int, bool *);
static tree csky_handle_isr_attribute (tree *, tree, tree, int, bool *);
TARGET_GNU_ATTRIBUTES (csky_attribute_table,
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "naked",	 0, 0, true,  false, false, false, csky_handle_fndecl_attribute, NULL },
  /* Interrupt Service Routines have special prologue and epilogue requirements.  */
  { "interrupt", 0, 1, false, false, false, false, csky_handle_isr_attribute,	 NULL },
  { "isr",	 0, 1, false, false, false, false, csky_handle_isr_attribute,	 NULL }
});

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct GTY(()) machine_function
{
  /* Records if LR has to be saved for far jumps.  */
  int far_jump_used;
  /* Records the type of the current function.  */
  unsigned long func_type;
  /* Record if the function has a variable argument list.  */
  int uses_anonymous_args;

  /* Stack frame layout information.  If frame_init_p is true,
     these fields have been initialized and don't need to be
     recomputed.  */
  unsigned int reg_mask;	/* non-volatile reg saves */
  int arg_size;			/* stdarg spills (bytes) */
  int reg_size;			/* non-volatile reg saves (bytes) */
  int local_size;		/* locals */
  int outbound_size;		/* arg overflow on calls out */
  int frame_size;		/* total static size of stack frame */
  int local_offset;
  int reg_offset;
  int arg_offset;
  int frame_init_p;

} machine_function;

/* These macros are for the func_type values above.  */
#define CSKY_FT_TYPE_MASK   ((1 << 3) - 1)
#define CSKY_FT_UNKNOWN	    0		    /* Type not been determined */
#define CSKY_FT_NORMAL	    1		    /* Normal function */
#define CSKY_FT_ISR	    4		    /* Interrupt service routine */
#define CSKY_FT_FIQ	    5		    /* Fast interrupt service routine */
#define CSKY_FT_EXCEPTION   6		    /* Exception handler */
#define CSKY_FT_INTERRUPT   (1 << 2)	    /* overlap CSKY_FT_ISR */
#define CSKY_FT_NAKED	    (1 << 3)	    /* No prologue and epilogue */
#define CSKY_FUNCTION_TYPE(t)	      ((t) & CSKY_FT_TYPE_MASK)
#define CSKY_FUNCTION_IS_INTERRUPT(t) ((t) & CSKY_FT_INTERRUPT)
#define CSKY_FUNCTION_IS_NAKED(t)     ((t) & CSKY_FT_NAKED)

struct csky_processors
{
  const char *const name;
  enum csky_processor_type core;
  const char *arch;
  enum csky_base_architecture base_arch;
  enum csky_isa_feature isa_bits[CSKY_ISA_FEATURE_GET (max)];
};

static struct csky_processors all_cores[] =
{
#undef CSKY_CORE
#define CSKY_CORE(NAME, CORE, X, ARCH, ISA)  \
  {NAME, TARGET_CPU_##CORE, #ARCH, CSKY_BASE_ARCH_##ARCH, \
  {ISA CSKY_ISA_FEATURE_GET (none)}},
#include "csky_cores.def"
#undef CSKY_CORE
  {NULL, TARGET_CPU_csky_none, NULL, CSKY_BASE_ARCH_NONE, \
  {CSKY_ISA_FEATURE_GET (none)}}
};

static struct csky_processors all_architectures[] =
{
#undef CSKY_ARCH
#define CSKY_ARCH(NAME, CORE, ARCH, ISA)     \
  {NAME, TARGET_CPU_##CORE, #ARCH, CSKY_BASE_ARCH_##ARCH,  \
  {ISA CSKY_ISA_FEATURE_GET (none)}},
#include "csky_cores.def"
#undef CSKY_ARCH
  {NULL, TARGET_CPU_csky_none, NULL, CSKY_BASE_ARCH_NONE, \
  {CSKY_ISA_FEATURE_GET (none)}}
};

struct csky_fpu_desc
{
  const char *name;
  enum csky_isa_feature isa_bits[CSKY_ISA_FEATURE_GET (max)];
};

static const struct csky_fpu_desc all_fpus[] =
{
#undef CSKY_FPU
#define CSKY_FPU(NAME, CNAME, ISA) \
  {NAME, {ISA CSKY_ISA_FEATURE_GET (none)}},
#include "csky_cores.def"
#undef CSKY_FPU
};

/* Active target architecture.  */
struct csky_build_target
{
  /* Name of the target CPU, if known, or NULL if the target CPU was not
     specified by the user (and inferred from the -march option).  */
  const char *core_name;
  /* Name of the target ARCH.  NULL if there is a selected CPU.  */
  const char *arch_name;
  /* Preprocessor substring (never NULL).  */
  const char *arch_pp_name;
  /* CPU identifier for the core we're compiling for (architecturally).  */
  enum csky_processor_type arch_core;
  /* The base architecture value.  */
  enum csky_base_architecture base_arch;
  /* Bitmap encapsulating the isa_bits for the target environment.  */
  sbitmap isa;
};

struct csky_build_target csky_active_target;

/* The following are used in the .md file as equivalents to bits.  */
int csky_arch_isa_features[CSKY_ISA_FEATURE_GET (max)] = {0};

/* The highest CSKY architecture version supported by the target.  */
enum csky_base_architecture csky_base_arch = CSKY_TARGET_ARCH_GET (NONE);

/* Forward definitions of types.  */
typedef struct minipool_node	Mnode;
typedef struct minipool_fixup	Mfix;

static GTY(()) int tls_labelno;


/* Maximum constant offset that can be added/subtracted from SP in a
   single instruction.  For ck801, this is for addsp/subsp, otherwise
   it is the range of addi/subi.  */
#define CSKY_MAX_SP_ADJUST \
  (CSKY_TARGET_ARCH (CK801) ? 508 : 4096)


/* Implement TARGET_CPU_CPP_BUILTINS.  */

#define builtin_define(MACRO) cpp_define (pfile, MACRO)

void
csky_cpu_cpp_builtins (cpp_reader *pfile)
{
  const char *arch_name = csky_active_target.arch_pp_name;
  char *pp_name = (char *) alloca (1 + strlen (arch_name) + 4);
  sprintf (pp_name, "__%s__", arch_name);
  builtin_define (pp_name);

  builtin_define ("__csky__=2");
  builtin_define ("__CSKY__=2");
  builtin_define ("__ckcore__=2");
  builtin_define ("__CKCORE__=2");

  builtin_define ("__CSKYABIV2__");
  builtin_define ("__cskyabiv2__");
  builtin_define ("__CSKYABI__=2");
  builtin_define ("__cskyabi__=2");

  if (TARGET_BIG_ENDIAN)
    {
      builtin_define ("__ckcoreBE__");
      builtin_define ("__cskyBE__");
      builtin_define ("__cskybe__");
      builtin_define ("__CSKYBE__");
    }
  else
    {
      builtin_define ("__ckcoreLE__");
      builtin_define ("__cskyLE__");
      builtin_define ("__cskyle__");
      builtin_define ("__CSKYLE__");
    }

  if (TARGET_HARD_FLOAT)
    {
      builtin_define ("__csky_hard_float__");
      builtin_define ("__CSKY_HARD_FLOAT__");
      if (TARGET_HARD_FLOAT_ABI)
	{
	  builtin_define ("__csky_hard_float_abi__");
	  builtin_define ("__CSKY_HARD_FLOAT_ABI__");
	}
      else
	{
	  builtin_define ("__csky_soft_float_abi__");
	  builtin_define ("__CSKY_SOFT_FLOAT_ABI__");
	}
      if (TARGET_SINGLE_FPU)
	{
	  builtin_define ("__csky_hard_float_fpu_sf__");
	  builtin_define ("__CSKY_HARD_FLOAT_FPU_SF__");
	}
    }
  else
    {
      builtin_define ("__csky_soft_float__");
      builtin_define ("__CSKY_SOFT_FLOAT__");
    }

  if (CSKY_ISA_FEATURE (fpv2_sf))
    {
      builtin_define ("__csky_fpuv2__");
      builtin_define ("__CSKY_FPUV2__");
    }

  if (TARGET_SUPPORT_FPV3)
    {
      builtin_define ("__csky_fpuv3__");
      builtin_define ("__CSKY_FPUV3__");
    }

  if (TARGET_ELRW)
    {
      builtin_define ("__csky_elrw__");
      builtin_define ("__CSKY_ELRW__");
    }
  if (TARGET_ISTACK)
    {
      builtin_define ("__csky_istack__");
      builtin_define ("__CSKY_ISTACK__");
    }
  if (TARGET_MP)
    {
      builtin_define ("__csky_mp__");
      builtin_define ("__CSKY_MP__");
    }
  if (TARGET_CP)
    {
      builtin_define ("__csky_cp__");
      builtin_define ("__CSKY_CP__");
    }
  if (TARGET_CACHE)
    {
      builtin_define ("__csky_cache__");
      builtin_define ("__CSKY_CACHE__");
    }
  if (TARGET_SECURITY)
    {
      builtin_define ("__csky_security__");
      builtin_define ("__CSKY_SECURITY__");
    }
  if (TARGET_TRUST)
    {
      builtin_define ("__csky_trust__");
      builtin_define ("__CSKY_TRUST__");
    }
  if (TARGET_DSP)
    {
      builtin_define ("__csky_dsp__");
      builtin_define ("__CSKY_DSP__");
    }
  if (TARGET_EDSP)
    {
      builtin_define ("__csky_edsp__");
      builtin_define ("__CSKY_EDSP__");
    }
  if (TARGET_VDSP)
    {
      builtin_define ("__csky_vdsp__");
      builtin_define ("__CSKY_VDSP__");
    }
}


/******************************************************************
 *			   Storage Layout			  *
 ******************************************************************/

#undef	TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE \
  default_promote_function_mode_always_promote

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT csky_constant_alignment

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE csky_mangle_type


/******************************************************************
 *		Stack Layout and Calling Conventions		  *
 ******************************************************************/

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE csky_can_eliminate

#undef	TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG csky_function_arg

#undef	TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE csky_function_arg_advance

#undef	TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE csky_function_value

#undef	TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE csky_libcall_value

#undef	TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P csky_function_value_regno_p

#undef	TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG hook_bool_const_tree_true

#undef	TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef	TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES csky_arg_partial_bytes

#undef	TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE hook_pass_by_reference_must_pass_in_stack

#undef	TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK csky_output_mi_thunk

#undef	TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef	TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE csky_output_function_prologue

#undef	TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE csky_output_function_epilogue

#undef	TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN csky_warn_func_return

#undef	TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY csky_return_in_memory


/******************************************************************
 *		  Implementing the Varargs Macros		  *
 ******************************************************************/


#undef	TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS csky_setup_incoming_varargs


/******************************************************************
 *		 Implicit Calls to Library Routines		  *
 ******************************************************************/


#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS csky_init_libfuncs


/******************************************************************
 *    Dividing the Output into Sections (Texts, Data, . . . )	  *
 ******************************************************************/


#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS TARGET_CSKY_LINUX


/******************************************************************
 *	   Defining target-specific uses of __attribute__	  *
 ******************************************************************/


#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE csky_attribute_table

#undef	TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE csky_option_override


/* Implement the BRANCH_COST target macro.  */

int
csky_default_branch_cost (bool speed_p ATTRIBUTE_UNUSED,
			  bool predictable_p ATTRIBUTE_UNUSED)
{
  return csky_branch_cost;
}

bool
csky_default_logical_op_non_short_circuit (void)
{
  return BRANCH_COST (optimize_function_for_speed_p (cfun), false) >= 2;
}

/******************************************************************
 *			   Register Usage			  *
 ******************************************************************/

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS csky_hard_regno_nregs

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK csky_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P csky_modes_tieable_p

#undef	TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE csky_conditional_register_usage

#undef TARGET_CLASS_LIKELY_SPILLED_P
#define TARGET_CLASS_LIKELY_SPILLED_P csky_class_likely_spilled_p

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS csky_preferred_reload_class

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS csky_class_max_nregs

#undef	TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD	 csky_secondary_reload

#undef TARGET_SPILL_CLASS
#define TARGET_SPILL_CLASS csky_spill_class


/******************************************************************
 *			  Addressing Modes			  *
 ******************************************************************/


#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM csky_cannot_force_const_mem

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P csky_legitimate_constant_p

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS csky_legitimize_address

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P csky_legitimate_address_p


/******************************************************************
 *			       Others				  *
 ******************************************************************/


#undef	TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P csky_cannot_copy_insn_p


/******************************************************************
 *			Assembler Format			  *
 ******************************************************************/


#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND csky_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS csky_print_operand_address

#undef	TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.short\t"

#undef	TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"

#undef	TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN csky_dwarf_register_span


/******************************************************************
 *		      Miscellaneous Parameters			  *
 ******************************************************************/


#undef	TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG csky_reorg

#undef	TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS csky_allocate_stack_slots_for_args

#undef TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed


/******************************************************************
 *		  Trampolines for Nested Functions		  *
 ******************************************************************/


#undef	TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE	csky_asm_trampoline_template
#undef	TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT		csky_trampoline_init

/* The low bit is ignored by jsr and jmp instructions so is safe to use.  */
#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 1

/******************************************************************
 *	      Describing Relative Costs of Operations		  *
 ******************************************************************/


#undef	TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST csky_register_move_cost

#undef	TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST	  csky_memory_move_cost

#undef	TARGET_RTX_COSTS
#define TARGET_RTX_COSTS	  csky_rtx_costs

#undef	TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST	  csky_address_cost


/******************************************************************
 *			  Anchor address			  *
 ******************************************************************/


/* FIXME: the max offset is related to mode size, the following is
   defined according to SImode. How to deal with HImode and
   QImode, and should the min offset be defined?  */
#undef	TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET \
  ((TARGET_MINI_REGISTERS && optimize_size) ? 127 : 4095)


/******************************************************************
 *		       Condition Code Status			  *
 ******************************************************************/


#undef	TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS csky_fixed_condition_code_regs


/******************************************************************
 *	     Adjusting the Instruction Scheduler		  *
 ******************************************************************/


#undef	TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE csky_sched_issue_rate

#undef	TARGET_SCHED_ADJUST_COST
#define	 TARGET_SCHED_ADJUST_COST csky_sched_adjust_cost


/******************************************************************
 *			Builtin					  *
 ******************************************************************/


#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  csky_init_builtins


/* The declaration of functions.  */
static void push_csky_minipool_fix (rtx_insn *, HOST_WIDE_INT, rtx *,
				    machine_mode, rtx);
static void csky_print_operand (FILE *stream, rtx x, int code);


/* Define a table to map ISR attribute arguments onto function type
   modifiers.  */

typedef struct
{
  const char *const arg;
  const unsigned long return_value;
} isr_attribute_entry;

static const isr_attribute_entry isr_attribute_map[] =
{
  {"irq", CSKY_FT_ISR },
  {"IRQ", CSKY_FT_ISR },
  {"fiq", CSKY_FT_FIQ },
  {"FIQ", CSKY_FT_FIQ },
  {NULL, CSKY_FT_NORMAL }
};


/* Return the function type of the current function, if it has not been
   determined, return CSKY_FT_UNKNOWN.  */

static unsigned long
get_csky_isr_type (tree argument)
{
  const isr_attribute_entry *ptr;
  const char *arg;

  /* if argument is NULL, set default value ISR.  */
  if (argument == NULL_TREE)
    return CSKY_FT_ISR;

  if (TREE_VALUE (argument) == NULL_TREE
     || TREE_CODE (TREE_VALUE (argument)) != STRING_CST)
    return CSKY_FT_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (argument));

  for (ptr = isr_attribute_map; ptr->arg != NULL; ptr++)
    if (strcmp (arg, ptr->arg) == 0)
      return ptr->return_value;

  return CSKY_FT_UNKNOWN;
}

/* Classify cfun as a normal function or some sort of interrupt
   handler, and set the corresponding bits in cfun->machine->func_type.  */

static unsigned long
get_csky_current_func_type (void)
{
  if (CSKY_FUNCTION_TYPE (cfun->machine->func_type) == CSKY_FT_UNKNOWN)
    {
      unsigned long type = CSKY_FT_UNKNOWN;
      tree a;
      tree attr;

      gcc_assert (TREE_CODE (current_function_decl) == FUNCTION_DECL);

      attr = DECL_ATTRIBUTES (current_function_decl);
      a = lookup_attribute ("naked", attr);
      if (a != NULL_TREE)
	type |= CSKY_FT_NAKED;
      a = lookup_attribute ("isr", attr);
      if (a == NULL_TREE)
	a = lookup_attribute ("interrupt", attr);
      if (a == NULL_TREE)
	type |= CSKY_FT_NORMAL;
      else
	type |= get_csky_isr_type (TREE_VALUE (a));

      cfun->machine->func_type = type;
    }

  return cfun->machine->func_type;
}

/* These typedefs are located at the start of this file, so that
   they can be used in the prototypes there.  This comment is to
   remind readers of that fact so that the following structures
   can be understood more easily.

     typedef struct minipool_node    Mnode;
     typedef struct minipool_fixup   Mfix;  */

struct minipool_node
{
  /* Doubly linked chain of entries.  */
  Mnode *next;
  Mnode *prev;
  /* The maximum offset into the code that this entry can be placed.  While
     pushing fixes for forward references, all entries are sorted in order
     of increasing max_address.  */
  HOST_WIDE_INT max_address;
  /* Similarly for an entry inserted for a backwards ref.  */
  HOST_WIDE_INT min_address;
  /* The number of fixes referencing this entry.  This can become zero
     if we "unpush" an entry.  In this case we ignore the entry when we
     come to emit the code.  */
  int refcount;
  /* The offset from the start of the minipool.  */
  HOST_WIDE_INT offset;
  /* The value in table.  */
  rtx value;
  /* The mode of value.  */
  machine_mode mode;
  /* The size of the value.  */
  int fix_size;
};

struct minipool_fixup
{
  Mfix *next;
  rtx_insn *insn;
  HOST_WIDE_INT address;
  rtx *loc;
  machine_mode mode;
  int fix_size;
  rtx value;
  Mnode *minipool;
  HOST_WIDE_INT forwards;
  HOST_WIDE_INT backwards;
};

static Mnode *minipool_vector_head;
static Mnode *minipool_vector_tail;
static rtx  minipool_vector_label;
static HOST_WIDE_INT constpool_label_no = 0;

/* Obstack for minipool constant handling.  */
static struct obstack minipool_obstack;
static char *minipool_startobj;
/* The linked list of all minipool fixes required for this function.  */
Mfix *minipool_fix_head;
Mfix *minipool_fix_tail;
/* The fix entry for the current minipool, once it has been placed.  */
Mfix *minipool_barrier;

/* Allow GC scanning of the minipool obstack.  */

static void
csky_add_gc_roots (void)
{
  gcc_obstack_init (&minipool_obstack);
  minipool_startobj = (char *) obstack_alloc (&minipool_obstack, 0);
}

/* Implement TARGET_CONSTANT_ALIGNMENT.
   Make strings word-aligned so strcpy from constants will be faster.  */

static HOST_WIDE_INT
csky_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST
      && !optimize_size
      && align < BITS_PER_WORD)
    return BITS_PER_WORD;
  return align;
}

/* Record that there is a natural barrier in the insn stream at
   ADDRESS.  */

static void
push_csky_minipool_barrier (rtx_insn *insn, HOST_WIDE_INT address)
{
  Mfix *fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*fix));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Compute the size of a vector jump table.  */

static HOST_WIDE_INT
get_csky_jump_table_size (rtx insn)
{
  /* ADDR_VECs only take room if read-only data does into the text
     section.  */
  if (JUMP_TABLES_IN_TEXT_SECTION || readonly_data_section == text_section)
    {
      rtx body = PATTERN (insn);
      int elt = GET_CODE (body) == ADDR_DIFF_VEC ? 1 : 0;
      HOST_WIDE_INT size;
      HOST_WIDE_INT modesize;

      modesize = GET_MODE_SIZE (GET_MODE (body));
      size = modesize * XVECLEN (body, elt);
      switch (modesize)
	{
	case 1:
	  /* Round up size  of TBB table to a halfword boundary.  */
	  size = (size + 1) & ~(HOST_WIDE_INT)1;
	  break;
	case 2:
	  /* No padding necessary for TBH.  */
	  break;
	case 4:
	  break;
	default:
	  gcc_unreachable ();
	}
      return size;
    }

  return 0;
}


/* Scan INSN and note any of its operands that need fixing.
   If DO_PUSHES is false we do not actually push any of the fixups
   needed.  The function returns TRUE if any fixups were needed/pushed.  */

static bool
note_csky_invalid_constants (rtx_insn *insn, HOST_WIDE_INT address,
			     int do_pushes)
{
  bool result = false;
  int opno;

  extract_constrain_insn (insn);

  if (recog_data.n_alternatives == 0)
    return false;

  /* Fill in recog_op_alt with information about the constraints of
     this insn.  */
  preprocess_constraints (insn);

  const operand_alternative *op_alt = which_op_alt ();
  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      /* Things we need to fix can only occur in inputs.  */
      if (recog_data.operand_type[opno] != OP_IN)
	continue;

      /* If this alternative is a memory reference, then any mention
	 of constants in this alternative is really to fool reload
	 into allowing us to accept one there.  We need to fix them up
	 now so that we output the right code.  */
      if (op_alt[opno].memory_ok)
	{
	  rtx op = recog_data.operand[opno];

	  if (CONSTANT_P (op))
	    {
	      if (do_pushes)
		push_csky_minipool_fix (insn, address,
					recog_data.operand_loc[opno],
					recog_data.operand_mode[opno], op);
	      result = true;
	    }
	}
    }

  return result;
}


/* Add a constant to the minipool for a forward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  */

static Mnode *
add_csky_minipool_forward_ref (Mfix *fix)
{
  /* If set, max_mp is the first pool_entry that has a lower
     constraint than the one we are trying to add.  */
  Mnode *max_mp = NULL;
  HOST_WIDE_INT max_address = fix->address + fix->forwards;
  Mnode *mp;

  /* If the minipool starts before the end of FIX->INSN then this FIX
     cannot be placed into the current pool.  Furthermore, adding the
     new constant pool entry may cause the pool to start FIX_SIZE bytes
     earlier.  */
  if (minipool_vector_head
      && (fix->address + get_attr_length (fix->insn)
	  >= minipool_vector_head->max_address - fix->fix_size))
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  /* More than one fix references this entry.  */
	  mp->refcount++;
	  return mp;
	}

      /* Note the insertion point if necessary.  */
      if (max_mp == NULL && mp->max_address > max_address)
	max_mp = mp;
    }

  /* The value is not currently in the minipool, so we need to create
     a new entry for it.  If MAX_MP is NULL, the entry will be put on
     the end of the list since the placement is less constrained than
     any existing entry.  Otherwise, we insert the new fix before
     MAX_MP and, if necessary, adjust the constraints on the other
     entries.  */
  mp = XNEW (Mnode);
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  /* Not yet required for a backwards ref.  */
  mp->min_address = -65536;

  if (max_mp == NULL)
    {
      mp->max_address = max_address;
      mp->next = NULL;
      mp->prev = minipool_vector_tail;

      if (mp->prev == NULL)
	{
	  minipool_vector_head = mp;
	  minipool_vector_label
	    = gen_csky_constpool_label (gen_rtx_CONST_INT (VOIDmode,
							   constpool_label_no++));
	}
      else
	mp->prev->next = mp;

      minipool_vector_tail = mp;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;
      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}


/* Return the cost of forcibly inserting a barrier after INSN.  */

static int
get_csky_barrier_cost (rtx_insn *insn)
{
  /* Basing the location of the pool on the loop depth is preferable,
     but at the moment, the basic block information seems to be
     corrupt by this stage of the compilation.  */
  int base_cost = 50;
  rtx next = next_nonnote_insn (insn);

  if (next != NULL && GET_CODE (next) == CODE_LABEL)
    base_cost -= 20;

  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* It will always be better to place the table before the label, rather
     than after it.  */
      return 50;

    case INSN:
    case CALL_INSN:
      return base_cost;

    case JUMP_INSN:
      return base_cost - 10;

    default:
      return base_cost + 10;
    }
}


/* Find the best place in the insn stream in the range
   (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
   Create the barrier by inserting a jump and add a new fix entry for
   it.  */

static Mfix *
create_csky_fix_barrier (Mfix *fix, Mfix *fix_next,
			 HOST_WIDE_INT max_address)
{
  rtx_barrier *barrier;
  rtx_insn *from = (fix ? fix->insn : get_insns ());
  /* The instruction after which we will insert the jump.  */
  rtx_insn *selected = NULL;
  int selected_cost;
  /* The address at which the jump instruction will be placed.  */
  HOST_WIDE_INT selected_address = 0;
  Mfix *new_fix;
  HOST_WIDE_INT count = (fix ? fix->address : 0);
  HOST_WIDE_INT max_count = max_address;
  rtx_code_label *label = gen_label_rtx ();

  selected_cost = get_csky_barrier_cost (from);

  while (from && count < max_count)
    {
      int new_cost;
      rtx_jump_table_data *table;

      /* Count the length of this insn.  */
      count += get_attr_length (from);

      /* If there is a jump table, add its length.  */
      if (tablejump_p (from, NULL, &table))
	{
	  count += get_csky_jump_table_size (table);

	  /* Jump tables aren't in a basic block, so base the cost on
	     the dispatch insn.  If we select this location, we will
	     still put the pool after the table.  */
	  new_cost = get_csky_barrier_cost (from);

	  if (count < max_count
	      && (!selected || new_cost <= selected_cost))
	    {
	      selected = table;
	      selected_cost = new_cost;
	      selected_address = count;
	    }

	  /* Continue after the dispatch table.  */
	  from = NEXT_INSN (table);
	  continue;
	}

      new_cost = get_csky_barrier_cost (from);

      if (count < max_count
	  && (!selected || new_cost <= selected_cost))
	{
	  selected = from;
	  selected_cost = new_cost;
	  selected_address = count;
	}

      from = NEXT_INSN (from);
    }

  /* Make sure that we found a place to insert the jump.  */
  gcc_assert (selected);

  /* Create a new JUMP_INSN that branches around a barrier.  */
  from = emit_jump_insn_after (gen_jump (label), selected);
  JUMP_LABEL (from) = label;
  barrier = emit_barrier_after (from);
  emit_label_after (label, barrier);

  /* Create a minipool barrier entry for the new barrier.  */
  new_fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* new_fix));
  new_fix->insn = barrier;
  new_fix->address = selected_address;
  if (fix)
    {
      new_fix->next = fix->next;
      fix->next = new_fix;
    }
  else
    new_fix->next = fix_next;

  return new_fix;
}


/* Print a symbolic form of the constant X to the dump file F.
   This is used for dump output for -mconstpool in the target-dependent
   reorg pass.  */

static void
print_csky_value (FILE *f, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      fprintf (f, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      return;

    case CONST_DOUBLE:
      fprintf (f, "<0x%lx,0x%lx>", (long)XWINT (x, 2), (long)XWINT (x, 3));
      return;

    case CONST_VECTOR:
      {
	int i;

	fprintf (f, "<");
	for (i = 0; i < CONST_VECTOR_NUNITS (x); i++)
	  {
	    fprintf (f, HOST_WIDE_INT_PRINT_HEX,
		     INTVAL (CONST_VECTOR_ELT (x, i)));
	    if (i < (CONST_VECTOR_NUNITS (x) - 1))
	      fputc (',', f);
	  }
	fprintf (f, ">");
      }
      return;

    case CONST_STRING:
      fprintf (f, "\"%s\"", XSTR (x, 0));
      return;

    case SYMBOL_REF:
      fprintf (f, "`%s'", XSTR (x, 0));
      return;

    case LABEL_REF:
      fprintf (f, "L%d", INSN_UID (XEXP (x, 0)));
      return;

    case CONST:
      print_csky_value (f, XEXP (x, 0));
      return;

    case PLUS:
      print_csky_value (f, XEXP (x, 0));
      fprintf (f, "+");
      print_csky_value (f, XEXP (x, 1));
      return;

    case PC:
      fprintf (f, "pc");
      return;

    default:
      fprintf (f, "????");
      return;
    }
}


/* Record INSN, which will need fixing up to load a value from the
   minipool.  ADDRESS is the offset of the insn since the start of the
   function; LOC is a pointer to the part of the insn which requires
   fixing; VALUE is the constant that must be loaded, which is of type
   MODE.  */

static void
push_csky_minipool_fix (rtx_insn *insn, HOST_WIDE_INT address, rtx *loc,
			machine_mode mode, rtx value)
{
  #define CSKY_ELRW16_RANGE  1400
  #define CSKY_LRW16_RANGE   700
  #define CSKY_CONSTANT_POOL_RANGE (TARGET_ELRW ? CSKY_ELRW16_RANGE \
						: CSKY_LRW16_RANGE)

  /* Fixes less than a word need padding out to a word boundary.  */
  #define CSKY_MINIPOOL_FIX_SIZE(mode) \
    (GET_MODE_SIZE ((mode)) >= 4 ? GET_MODE_SIZE ((mode)) : 4)

  Mfix *fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*fix));

  fix->insn = insn;
  fix->address = address;
  fix->loc = loc;
  fix->mode = mode;
  fix->fix_size = CSKY_MINIPOOL_FIX_SIZE (mode);
  fix->value = value;
  fix->forwards = CSKY_CONSTANT_POOL_RANGE;
  fix->backwards = 0;
  fix->minipool = NULL;

  /* If an insn doesn't have a range defined for it, then it isn't
     expecting to be reworked by this code.  Better to stop now than
     to generate duff assembly code.  */
  gcc_assert (fix->forwards || fix->backwards);

  if (dump_file)
    {
      fprintf (dump_file,
	       ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
	       GET_MODE_NAME (mode),
	       INSN_UID (insn), (unsigned long) address,
	       -1 * (long)fix->backwards, (long)fix->forwards);
      print_csky_value (dump_file, fix->value);
      fprintf (dump_file, "\n");
    }

  /* Add it to the chain of fixes.  */
  fix->next = NULL;

  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}


/* Fill in the offsets for minipool entries.  */

static void
assign_csky_minipool_offsets (Mfix *barrier)
{
  HOST_WIDE_INT offset = 0;
  Mnode *mp;

  minipool_barrier = barrier;

  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;

      if (mp->refcount > 0)
	offset += mp->fix_size;
    }
}


/* Output the literal table.  */

static HOST_WIDE_INT
dump_csky_minipool (rtx_insn *scan)
{
  Mnode *mp;
  Mnode *nmp;
  HOST_WIDE_INT pool_length = 0;

  if (dump_file)
    fprintf (dump_file,
	     ";; Emitting minipool after insn %u;\
	      address %ld; align %d (bytes)\n",
	     INSN_UID (scan), (unsigned long) minipool_barrier->address, 4);

  scan = emit_insn_after (gen_align_4 (), scan);
  scan = emit_insn_after (minipool_vector_label, scan);

  for (mp = minipool_vector_head; mp != NULL; mp = nmp)
    {
      if (mp->refcount > 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, ";;  Offset %u, min %ld, max %ld ",
		       (unsigned) mp->offset, (unsigned long) mp->min_address,
		       (unsigned long) mp->max_address);
	      print_csky_value (dump_file, mp->value);
	      fputc ('\n', dump_file);
	    }

	  switch (mp->fix_size)
	    {
	    case 4:
	      scan = emit_insn_after (gen_consttable_4 (mp->value), scan);
	      pool_length += 4;
	      break;
	    case 8:
	      scan = emit_insn_after (gen_consttable_8 (mp->value), scan);
	      pool_length += 8;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}

      nmp = mp->next;
      free (mp);
    }

  minipool_vector_head = minipool_vector_tail = NULL;
  scan = emit_barrier_after (scan);

  return pool_length;
}

/* Return true if INSN is a minipool load or instruction that will be
   converted to one.  It is assumed that INSN has type attribute "load".  */

bool
csky_minipool_load_p (rtx_insn *insn)
{
  rtx op1, addr;

  extract_insn_cached (insn);

  op1 = recog_data.operand[1];

  /* This is a constant that has not yet been turned into
     a minipool load.  */
  if (CONSTANT_P (op1))
    return true;

  /* Constant pool loads are label_refs.  */
  if (GET_CODE (op1) == ZERO_EXTEND || GET_CODE (op1) == SIGN_EXTEND)
    op1 = XEXP (op1, 0);
  if (GET_CODE (op1) != MEM)
    return false;
  addr = XEXP (op1, 0);
  if (GET_CODE (addr) == PLUS && CONST_INT_P (XEXP (addr, 1)))
    addr = XEXP (addr, 0);
  return GET_CODE (addr) == LABEL_REF;
}


/* Compute the attribute "length" of push or pop insn, according to
   the registers it uses.  */

int
csky_compute_pushpop_length (rtx *operands)
{
  rtx parallel_op = operands[2];
  /* Initialize to elements number of PARALLEL.  */
  unsigned indx = XVECLEN (parallel_op, 0) - 1;
  unsigned first_indx = 0;
  unsigned regno = REGNO (operands[1]);

  if (regno > CSKY_LR_REGNUM)
    return 4;

  /* Check each register in the list.  */
  for (; indx > first_indx; indx--)
    {
      regno = REGNO (XEXP (XVECEXP (parallel_op, 0, indx), 0));
      /* If a register number higher than 15 is included, a 32-bit insn
	 is used.  */
      if (regno > CSKY_LR_REGNUM)
	return 4;
    }

  return 2;
}

/* Emit constant pools for -mconstpool.  */

static void
csky_emit_constant_pools (void)
{
    rtx_insn *insn;
    HOST_WIDE_INT address = 0;
    Mfix *fix;

    minipool_fix_head = minipool_fix_tail = NULL;

    /* The first insn must always be a note, or the code below won't
       scan it properly.  */
    insn = get_insns ();
    gcc_assert (NOTE_P (insn));

    /* Scan the insns and record the operands that need fixing.  */
    for (insn = next_nonnote_insn (insn); insn;
	 insn = next_nonnote_insn (insn))
      {
	if (BARRIER_P (insn))
	  push_csky_minipool_barrier (insn, address);
	else if (INSN_P (insn))
	  {
	    rtx_jump_table_data *table;

	    note_csky_invalid_constants (insn, address, true);
	    address += get_attr_length (insn);

	    /* If the insn is a vector jump, add the size of the table
	     and skip the table.  */
	    if (tablejump_p (insn, NULL, &table))
	      {
		address += get_csky_jump_table_size (table);
		insn = table;
	      }
	  }
      }

    fix = minipool_fix_head;

    /* Now scan the fixups and perform the required changes.  */
    while (fix)
      {
	Mfix *ftmp;
	Mfix *last_added_fix;
	Mfix *last_barrier = NULL;
	Mfix *this_fix;
	Mnode *mp;
	bool has_pending_const = false;

	/* Check if there is any pending constant not processed.  */
	for (mp = minipool_vector_head; mp; mp = mp->next)
	  if (mp->refcount > 0)
	    {
	      has_pending_const = true;
	      break;
	    }

	/* If no pending constant, skip over barrier insns.  */
	if (has_pending_const == false)
	  {
	    while (fix && BARRIER_P (fix->insn))
	      fix = fix->next;
	    if (fix == NULL)
	      break;
	  }

	last_added_fix = NULL;

	for (ftmp = fix; ftmp; ftmp = ftmp->next)
	  {
	    if (BARRIER_P (ftmp->insn))
	      {
		if (minipool_vector_head
		    && ftmp->address >= minipool_vector_head->max_address)
		  break;

		last_barrier = ftmp;
	      }
	    else
	      {
		ftmp->minipool = add_csky_minipool_forward_ref (ftmp);
		if (ftmp->minipool == NULL)
		  break;
	      }
	    last_added_fix = ftmp;  /* Keep track of the last fix added.  */
	  }

	/* If the last added fix is a barrier, dump minipool after it.  */
	if (last_added_fix && BARRIER_P (last_added_fix->insn))
	  ftmp = last_barrier;
	else
	  {
	    /* ftmp is first fix that we can't fit into this pool.
	       Insert a new barrier in the code somewhere between the previous
	       fix and this one, and arrange to jump around it.  */
	    HOST_WIDE_INT max_address;

	    /* The last item on the list of fixes must be a barrier, so
	       we can never run off the end of the list of fixes without
	       last_barrier being set.  */
	    gcc_assert (ftmp);

	    /* Check that there isn't another fix that is in range that
	       we couldn't fit into this pool because the pool was
	       already too large: we need to put the pool before such an
	       instruction.  The pool itself may come just after the
	       fix because create_csky_fix_barrier also allows space for a
	       jump instruction.  */
	    max_address = minipool_vector_head->max_address;
	    if (ftmp->address < max_address)
	      max_address = ftmp->address + 1;
	    last_barrier = create_csky_fix_barrier (last_added_fix, ftmp,
						    max_address);
	  }

	assign_csky_minipool_offsets (last_barrier);

	/* Scan over the fixes we have identified for this pool, fixing them
	   up and adding the constants to the pool itself.  */
	for (this_fix = fix; this_fix && ftmp != this_fix;
	     this_fix = this_fix->next)
	  {
	    if (GET_CODE (this_fix->insn) != BARRIER)
	      {
		rtx addr
		  = plus_constant (Pmode,
				   gen_rtx_LABEL_REF (VOIDmode,
						      minipool_vector_label),
				   this_fix->minipool->offset);
		rtx insn_body = PATTERN (this_fix->insn);
		rtx src = XEXP (insn_body, 1);
		*this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
		if (GET_CODE (this_fix->value) == SYMBOL_REF)
		  emit_insn_after (gen_rtx_UNSPEC_VOLATILE (VOIDmode,
							    gen_rtvec (1, src),
							    VUNSPEC_SYMBOL_REF),
				   this_fix->insn);
	      }
	  }
	dump_csky_minipool (last_barrier->insn);
	fix = ftmp;
	if (fix->next == NULL)
	  break;
      }

    /* Free the minipool memory.  */
    obstack_free (&minipool_obstack, minipool_startobj);
}


/* Implement TARGET_MACHINE_DEPENDENT_REORG.  This handles
   -mconstpool output.  */

static void
csky_reorg (void)
{
  if (TARGET_CONSTANT_POOL)
    csky_emit_constant_pools ();
}


/* Check to see if the current function contains a branch insn with the
   far jump attribute set.  Such a function uses the LR register.  */

static bool
csky_far_jump_used_p (void)
{
  rtx_insn *insn;
  if (cfun->machine->far_jump_used)
    return true;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == JUMP_INSN
	/* Ignore tablejump patterns.  */
	&& GET_CODE (PATTERN (insn)) != ADDR_VEC
	&& GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC
	&& get_attr_far_jump (insn) == FAR_JUMP_YES)
      {
	cfun->machine->far_jump_used = 1;
	return true;
      }
  return false;
}


/* Return the mask of registers used by the current function.  Set
   COUNT to the number of registers used.  */

static unsigned int
get_csky_live_regs (int *count)
{
  int reg;
  unsigned int live_regs_mask = 0;

  *count = 0;
  for (reg = 0; reg < CSKY_NGPR_REGS; reg++)
    {
      bool save = false;

      /* Ignore unsupported registers.  */
      if (CSKY_TARGET_ARCH (CK801) && reg > 8 && reg < 13)
	continue;
      if ((CSKY_TARGET_ARCH (CK801)
	   || CSKY_TARGET_ARCH (CK802)
	   || CSKY_TARGET_ARCH (CK803))
	  && reg > 15)
	break;

      /* Caller-saved registers marked as used.  */
      if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
	save = true;

      /* Frame pointer marked used.  */
      else if (frame_pointer_needed && reg == HARD_FRAME_POINTER_REGNUM)
	save = true;

      /* This is required for CK801/802 where FP is a fixed reg, otherwise
	 we end up with no FP value available to the DWARF-2 unwinder.  */
      else if (crtl->calls_eh_return && reg == HARD_FRAME_POINTER_REGNUM)
	save = true;

      /* CK801/802 also need special handling for LR because it's clobbered
	 by far jumps.  */
      else if ((CSKY_TARGET_ARCH (CK801) || CSKY_TARGET_ARCH (CK802))
	       && reg == CSKY_LR_REGNUM
	       && (!crtl->is_leaf || csky_far_jump_used_p ()))
	save = true;

      /* Register is used for EH data return.  */
      else if (crtl->calls_eh_return
	       && reg >= CSKY_FIRST_EH_RETDATA_REGNUM
	       && reg <= CSKY_LAST_EH_RETDATA_REGNUM)
	save = true;

      /* We need a temporary reg to hold the offset for adjusting the SP
	 for a large stack frame.  */
      if (reg == CSKY_STACKADJUST_REGNUM
	  && cfun->machine->reg_offset > CSKY_MAX_SP_ADJUST * 2)
	save = true;

      /* Add reg to the mask.  */
      if (save)
	{
	  (*count)++;
	  live_regs_mask |= (1 << reg);
	}
    }
  return live_regs_mask;
}

/* Compute the stack frame layout, storing sizes of the various pieces
   in cfun->machine.

   Stack frames constructed in the prologue look like:
			... caller's frame ...
	incoming SP ->	caller's outbound argument overflow
			argument spill
	optional FP ->	register save
			local variables
			alloca() space
	adjusted SP ->	outbound argument overflow

   with SP/FP pointing at the base (low address) of the respective area,
   and each area aligned to a word boundary.  */

static void
csky_layout_stack_frame (void)
{
  machine_function *infp = cfun->machine;
  int reg_count;

  if (infp->frame_init_p)
    return;

  /* Get sizes of local variables & outbound arguments.  */
  infp->outbound_size = CSKY_STACK_ALIGN (crtl->outgoing_args_size);
  infp->local_offset = infp->outbound_size;
  infp->local_size = CSKY_STACK_ALIGN (get_frame_size ());
  infp->reg_offset = infp->local_offset + infp->local_size;

  /* Now compute size of argument spill + saved regs.  These do not
     need explicit alignment since they are already word-sized.  */
  infp->reg_mask = get_csky_live_regs (&reg_count);
  infp->reg_size = reg_count * UNITS_PER_WORD;
  infp->arg_offset = infp->reg_offset + infp->reg_size;
  infp->arg_size = crtl->args.pretend_args_size;
  infp->frame_size = infp->arg_offset + infp->arg_size;
  infp->frame_init_p = reload_completed;
}

/* Implement TARGET_CAN_ELIMINATE.  */
static bool
csky_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  if (to == FRAME_POINTER_REGNUM)
    return from != ARG_POINTER_REGNUM;
  if (to == STACK_POINTER_REGNUM)
    return !frame_pointer_needed;
  return true;
}

/* Worker function for INITIAL_ELIMINATION_OFFSET macro.
   Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.  */

HOST_WIDE_INT
csky_initial_elimination_offset (int from, int to)
{
  int offset;

  csky_layout_stack_frame ();

  /* Set OFFSET to the offset to the initial stack pointer.  */
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
    case HARD_FRAME_POINTER_REGNUM:
      offset = cfun->machine->reg_offset;
      break;

    case ARG_POINTER_REGNUM:
      offset = cfun->machine->arg_offset;
      break;

    default:
      gcc_unreachable ();
    }

  /* If we are asked for the offset to the frame pointer instead,
     then subtract the difference between the frame pointer and stack
     pointer.  */
  if (to == FRAME_POINTER_REGNUM || to == HARD_FRAME_POINTER_REGNUM)
    offset -= cfun->machine->reg_offset;
  return offset;
}


/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   ARG is a description of the argument.  */

static rtx
csky_function_arg (cumulative_args_t pcum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int reg = pcum->reg;
  machine_mode mode = arg.mode;

  if (FUNCTION_VARG_MODE_P(mode)
      && !pcum->is_stdarg)
    {
      reg = pcum->freg;

      if (reg < CSKY_NPARM_FREGS)
	return gen_rtx_REG (mode, CSKY_FIRST_VFP_REGNUM + reg);
      else
	return NULL_RTX;
    }

  if (reg < CSKY_NPARM_REGS)
    return gen_rtx_REG (mode, CSKY_FIRST_PARM_REGNUM + reg);

  return NULL_RTX;
}


/* Return the number of registers (words) needed to pass an argument of
   MODE and TYPE.  */

static int
csky_num_arg_regs (machine_mode mode, const_tree type, bool is_stdarg)
{
  int size;

  if (type && mode == BLKmode)
    size = int_size_in_bytes (type);
  else
    size = GET_MODE_SIZE (mode);

  if (TARGET_HARD_FLOAT_ABI
      && !is_stdarg)
    {
      if (CSKY_VREG_MODE_P(mode)
	  && !TARGET_SINGLE_FPU)
	return ((CSKY_NUM_WORDS (size) + 1) / 2);
    }

  return CSKY_NUM_WORDS (size);
}


/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */

static void
csky_function_arg_advance (cumulative_args_t pcum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int *reg = &pcum->reg;
  machine_mode mode = arg.mode;

  int param_size = csky_num_arg_regs (mode, arg.type, pcum->is_stdarg);
  int param_regs_nums = CSKY_NPARM_REGS;

  if (FUNCTION_VARG_MODE_P(mode)
      && !pcum->is_stdarg)
    {
      reg = &pcum->freg;
      param_regs_nums = CSKY_NPARM_FREGS;
    }

  if (*reg + param_size > param_regs_nums)
    *reg = param_regs_nums;
  else
    *reg += param_size;
}


/* Implement TARGET_FUNCTION_VALUE.  */
static rtx
csky_function_value (const_tree type, const_tree func,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsignedp ATTRIBUTE_UNUSED;
  int size;

  mode = TYPE_MODE (type);
  size = int_size_in_bytes (type);

  if (FUNCTION_VARG_MODE_P(mode))
    {
      mode = promote_function_mode (type, mode, &unsignedp, func, 1);
      return gen_rtx_REG (mode, CSKY_FIRST_VFP_REGNUM);
    }

  /* Since we promote return types, we must promote the mode here too.  */
  if (INTEGRAL_TYPE_P (type))
    {
      mode = promote_function_mode (type, mode, &unsignedp, func, 1);
      return gen_rtx_REG (mode, CSKY_FIRST_RET_REGNUM);
    }

  if (mode == BLKmode && size > UNITS_PER_WORD
      && size <= UNITS_PER_WORD * 2)
    {
      rtx ret_regs[2];
      ret_regs[0] = gen_rtx_EXPR_LIST (SImode,
				       gen_rtx_REG (SImode,
						    CSKY_FIRST_RET_REGNUM),
				       GEN_INT (0 * UNITS_PER_WORD));
      ret_regs[1] = gen_rtx_EXPR_LIST (SImode,
				       gen_rtx_REG (SImode,
						    CSKY_FIRST_RET_REGNUM + 1),
				       GEN_INT (1 * UNITS_PER_WORD));

      rtvec vec = gen_rtvec (2, ret_regs[0], ret_regs[1]);

      return gen_rtx_PARALLEL (mode, vec);
    }

    return gen_rtx_REG (mode, CSKY_FIRST_RET_REGNUM);
}


/* Implement TARGET_LIBCALL_VALUE.  */

static rtx
csky_libcall_value (machine_mode mode,
		    const_rtx libcall ATTRIBUTE_UNUSED)
{
  if (FUNCTION_VARG_MODE_P(mode))
    {
      return gen_rtx_REG (mode, CSKY_FIRST_VFP_REGNUM);
    }
  return gen_rtx_REG (mode, CSKY_FIRST_RET_REGNUM);
}


/* Implement TARGET_FUNCTION_VALUE_REGNO_P.
   On C-SKY, only r0 can return results.  */

static bool
csky_function_value_regno_p (const unsigned int regno)
{
  if (regno == CSKY_FIRST_RET_REGNUM
      || (TARGET_HARD_FLOAT_ABI
	  && regno == CSKY_FIRST_VFP_REGNUM))
    return true;
  return false;
}


/* Return an RTX indicating where the return address to the
   calling function can be found.  */

rtx
csky_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return NULL_RTX;

  return get_hard_reg_initial_val (Pmode, CSKY_LR_REGNUM);
}


/* Implement TARGET_ARG_PARTIAL_BYTES.
   Return the number of bytes at the beginning of an argument
   that must be put in registers. The value must be zero for arguments
   that are passed entirely in registers or
   that are entirely pushed on the stack.  */

static int
csky_arg_partial_bytes (cumulative_args_t pcum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int param_size = csky_num_arg_regs (arg.mode, arg.type, pcum->is_stdarg);
  int reg = pcum->reg;

  if (FUNCTION_VARG_MODE_P(arg.mode)
      && !pcum->is_stdarg)
    return 0;

  if (reg < CSKY_NPARM_REGS
      && reg + param_size > CSKY_NPARM_REGS)
    return (CSKY_NPARM_REGS - reg) * UNITS_PER_WORD;

  return 0;
}


/* Implement TARGET_SETUP_INCOMING_VARARGS.
   On C-Sky the copy from the argument registers to the stack is emitted
   by the prologue hooks, so here we just have to note how much stack space
   to save.  */

static void
csky_setup_incoming_varargs (cumulative_args_t pcum_v,
			     const function_arg_info &arg,
			     int *pretend_size,
			     int second_time ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  CUMULATIVE_ARGS local_cum;
  cumulative_args_t local_cum_v = pack_cumulative_args (&local_cum);
  int regs_to_push;

  cfun->machine->uses_anonymous_args = 1;
  local_cum = *pcum;
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    csky_function_arg_advance (local_cum_v, arg);
  regs_to_push = CSKY_NPARM_REGS - local_cum.reg;
  if (regs_to_push)
    *pretend_size  = regs_to_push * UNITS_PER_WORD;
}


/* Implement TARGET_ASM_OUTPUT_MI_THUNK.
   Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */

static void
csky_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
		      HOST_WIDE_INT delta,
		      HOST_WIDE_INT vcall_offset,
		      tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk));
  const char *thiz = "a0";
  const char *reg0 = "t0";
  const char *reg1 = "t1";
  int maxoff = 4096;		/* Constant range for addi/subi.  */

  assemble_start_function (thunk, fnname);
  final_start_function (emit_barrier (), file, 1);

  rtx fnaddr = XEXP (DECL_RTL (function), 0);

  if (CSKY_TARGET_ARCH (CK801))
    {
      /* CK801 can't use t registers and has only 16-bit addi/subi.  */
      reg0 = "l0";
      reg1 = "l1";
      maxoff = 256;
      if (vcall_offset > maxoff || vcall_offset < -maxoff)
	fprintf (file, "\tpush\tl0, l1\n");
      else if (delta > maxoff || delta < -maxoff)
	fprintf (file, "\tpush\tl0\n");
    }

  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    thiz = "a1";

  /* Add delta to this_rtx.  */
  if (delta != 0)
    {
      if (delta > maxoff || delta < -maxoff)
	{
	  fprintf (file, "\tlrw\t%s, %ld\n", reg0, (long)delta);
	  fprintf (file, "\taddu\t%s, %s, %s\n", thiz, thiz, reg0);
	}
      else
	fprintf (file, "\t%s\t%s, %s, %ld\n",
		 (delta > 0 ? "addi" : "subi"), thiz, thiz,
		 (long)(delta > 0 ? delta : -delta));
    }

  /* If needed, add *(*this_rtx + vcall_offset) to this_rtx.  */
  if (vcall_offset != 0)
    {
      fprintf (file, "\tld.w\t%s, (%s, 0)\n", reg0, thiz);

      if (vcall_offset > maxoff || vcall_offset < -maxoff)
	{
	  fprintf (file, "\tlrw\t%s, %ld\n", reg1, (long)vcall_offset);
	  fprintf (file, "\taddu\t%s, %s, %s\n", reg0, reg0, reg1);
	}
      else
	fprintf (file, "\t%s\t%s, %s, %ld\n",
		 (vcall_offset > 0 ? "addi" : "subi"), reg0, reg0,
		 (long)(vcall_offset > 0 ? vcall_offset : -vcall_offset));

      /* Load the offset and add it to this_rtx	 */
      fprintf (file, "\tld.w\t%s, (%s, 0)\n", reg0, reg0);
      fprintf (file, "\taddu\t%s, %s, %s\n", thiz, thiz, reg0);
    }

  /* We must pop the scratch regs individually instead of using the
     "pop" insn, which also does a return.  */
  if (CSKY_TARGET_ARCH (CK801))
    {
      if (vcall_offset > maxoff || vcall_offset < -maxoff)
	{
	  fprintf (file, "\tld.w\tl0, (sp, 0)\n");
	  fprintf (file, "\tld.w\tl1, (sp, 4)\n");
	  fprintf (file, "\taddi\t sp, sp, 8\n");
	}
      else if (delta > maxoff || delta < -maxoff)
	{
	  fprintf (file, "\tld.w\tl0, (sp, 0)\n");
	  fprintf (file, "\taddi\tsp, sp, 4\n");
	}
    }

  fprintf (file, "\tjbr\t");
  output_addr_const (file, fnaddr);
  fprintf (file, "\n");

  final_end_function ();
  assemble_end_function (thunk, fnname);
}


/* Implement TARGET_CONDITIONAL_REGISTER_USAGE.
   Conditionally modify five variables fixed_regs, call_used_regs, global_regs,
   reg_names, and reg_class_contents, to take into account any dependence of
   these register sets on target flags.

   CK801 has registers r0-r8 and r13-r15.  CK802 and CK803 have registers
   r0-r15 (the "low" registers).  Other cpus use registers r0-r31 with
   -mhigh-registers, otherwise also only r0-r15.

   CK801 only has 16-bit instructions, most of which can only reference
   r0-r7 (the "mini" registers).  So we mark regs outside that range as
   fixed.  -msmart can be used on other arch variants to force the same
   behavior because it results in smaller code size.

   TODO: investigate whether it's beneficial to use r8-r13 as a spill
   class when TARGET_MINI_REGISTERS instead of making them unusable by
   the register allocator.  */

static void
csky_conditional_register_usage (void)
{
  /* Only use mini registers in smart mode or 801.  */
  if (TARGET_MINI_REGISTERS)
    {
      int i;

      for (i = (CSKY_LAST_MINI_REGNUM + 1); i < 32; i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	}
    }
  /* For some targets, the high registers are not supported.
     CPUs other than ck801/ck802/ck803 use high registers
     depending on -mhigh-registers option.  */
  else if (CSKY_TARGET_ARCH (CK802)
	   || CSKY_TARGET_ARCH (CK803)
	   || !TARGET_HIGH_REGISTERS)
   {
      int i;

      for (i = CSKY_FIRST_HIGH_REGNUM; i <= CSKY_LAST_HIGH_REGNUM; i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	}
   }

  /* On CK801/CK802 we must mark lr as a fixed register because it is
     used to implement far jumps.
     FIXME: perhaps there should be a command-line option controlling
     use of lr for far jumps on ck802 when !TARGET_MINI_REGS, when
     you really want lr to be available to the register allocator and
     you know there are no far jumps in the code.  */
  if (CSKY_TARGET_ARCH (CK801) || CSKY_TARGET_ARCH (CK802))
    {
      fixed_regs[CSKY_LR_REGNUM] = 1;
      call_used_regs[CSKY_LR_REGNUM] = 0;
    }

  /* The hi/lo registers are only supported in dsp mode.  */
  if (!TARGET_DSP)
    {
      fixed_regs[CSKY_HI_REGNUM] = 1;
      call_used_regs[CSKY_HI_REGNUM] = 1;

      fixed_regs[CSKY_LO_REGNUM] = 1;
      call_used_regs[CSKY_LO_REGNUM] = 1;
    }

  /* The V_REGS are only supported in hard float mode.  */
  if (!TARGET_HARD_FLOAT)
    {
      int regno;

      for (regno = CSKY_FIRST_VFP_REGNUM;
	   regno <= CSKY_LAST_VFP3_REGNUM; regno++)
	{
	  fixed_regs[regno] = 1;
	  call_used_regs[regno] = 1;
	}
    }

  if (!TARGET_SUPPORT_FPV3)
    {
      int regno;

      for (regno = CSKY_FIRST_VFP3_REGNUM;
	   regno <= CSKY_LAST_VFP3_REGNUM; regno++)
	{
	  fixed_regs[regno] = 1;
	  call_used_regs[regno] = 1;
	}
    }

  /* In pic mode, the gb register is not available for register
     allocation.  Since gb is not clobbered by function
     calls, set its call_used_regs to 0.  */
  if (flag_pic)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 0;
    }
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
csky_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (regno >= CSKY_FIRST_VFP_REGNUM && !CSKY_TARGET_ARCH (CK803))
    return 1;
  else
    return CSKY_NUM_REGS (mode);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  Return true if REGNO is a
   valid register for holding a quantity of type MODE.  */

static bool
csky_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  int nregs = CSKY_NUM_REGS (mode);

  /* We can't handle more than doubleword sizes for any register.  */
  if (nregs > 2)
    return false;

  /* For general registers, return true if mode is one word size.
     When the size is larger than one word size, there should
     be two successive hard registers to put the data.  */
  if (regno < CSKY_NGPR_REGS)
    {
      if (nregs < 2)
	return true;
      else if (TARGET_MINI_REGISTERS)
	return (regno < CSKY_LAST_MINI_REGNUM);
      else if (CSKY_TARGET_ARCH (CK802)
	       || CSKY_TARGET_ARCH (CK803)
	       || !TARGET_HIGH_REGISTERS)
	/* Without high register, r15 cannot hold doubleword data.  */
	return (regno < (CSKY_SP_REGNUM - 1));
      else
	return (regno < (CSKY_SP_REGNUM - 1)
		|| (regno >= CSKY_LR_REGNUM
		    && regno < CSKY_LAST_HIGH_UNFIXED_REGNUM));
    }
  else if (regno == CSKY_CC_REGNUM)
    return (mode == CCmode);
  else if (regno == CSKY_HI_REGNUM || regno == CSKY_LO_REGNUM)
    {
      /* Don't allocate hi,lo register for float data even
	 if in dsp mode, because it will cause high cost
	 to reload data from hi,lo register.  */
      if (!TARGET_DSP || mode == SFmode || mode == DFmode)
	return false;
      else if (nregs == 2)
	return (regno == CSKY_HI_REGNUM);
      else
	return true;
    }
  else if (CSKY_VREG_P (regno) && TARGET_HARD_FLOAT)
    return true;

  return false;
}

/* Implement TARGET_MODES_TIEABLE_P.  We can't tie DFmode with other modes
   when V_REGs might be in use because those registers mess with the stored
   bits.  */

static bool
csky_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  return !(TARGET_HARD_FLOAT
	   && mode1 != mode2
	   && (mode1 == DFmode || mode2 == DFmode));
}

/* Implement TARGET_CLASS_LIKELY_SPILLED_P.
   We need to define this for MINI_REGS when we only use r0 - r7.
   Otherwise we can end up using r0-r4 for function arguments, and don't
   have enough left over to do doubleword arithmetic.  */

static bool
csky_class_likely_spilled_p (reg_class_t rclass)
{
  if ((TARGET_MINI_REGISTERS && rclass == MINI_REGS)
      || rclass == C_REGS)
    return true;

  return false;
}


/* Implement TARGET_PREFERRED_RELOAD_CLASS.
   Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS.  */

static reg_class_t
csky_preferred_reload_class (rtx x, reg_class_t rclass)
{
  if (TARGET_HARD_FLOAT
      && CONST_DOUBLE_P (x)
      && (GET_MODE (x) == DFmode || GET_MODE (x) == SFmode)
      && rclass == NO_REGS)
    return GENERAL_REGS;
  return rclass;
}


/* Implement TARGET_CLASS_MAX_NREGS.
   Return the maximum number of consecutive registers of class rclass needed
   to hold a value of mode mode.
   On the csky, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */

static unsigned char
csky_class_max_nregs (reg_class_t rclass, machine_mode mode)
{
  if (rclass == V_REGS)
    return 1;
  else
    return CSKY_NUM_REGS (mode);
}


/* Implement TARGET_SECONDARY_RELOAD.
   If copying a register of RCLASS from/to X requires an intermediate
   register, the hook should return the REGISTER_CLASS required for this
   intermediate register.
   If no intermediate register is required, it should return NO_REGS.
   If more than one intermediate register is required, describe the one
   that is closest in the copy chain to the reload register.  */

reg_class_t
csky_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x,
		       reg_class_t rclass,
		       machine_mode mode,
		       secondary_reload_info *sri ATTRIBUTE_UNUSED)
{
  int regno = -1;

  /* Extract the real regno from X.  */
  if (GET_CODE (x) == SIGN_EXTEND)
    {
      int off = 0;

      x = XEXP (x, 0);

      if (reg_renumber)
	regno = true_regnum (x);
      else
	{
	  while (GET_CODE (x) == SUBREG)
	    {
	      off += subreg_regno_offset (REGNO (SUBREG_REG (x)),
	      GET_MODE (SUBREG_REG (x)),
	      SUBREG_BYTE (x), GET_MODE (x));
	      x = SUBREG_REG (x);
	    }

	    if (GET_CODE (x) == REG)
	      regno = REGNO (x) + off;
	}
    }
  else if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
    regno = true_regnum (x);

  /* We always require a general register when copying anything to
     HI/LO_REGNUM, except when copying an SImode value from HI/LO_REGNUM
     to a general register, or when copying from register 0.  */
  if (rclass == HILO_REGS && !CSKY_GENERAL_REGNO_P (regno))
    return GENERAL_REGS;

  if (rclass == V_REGS && !CSKY_GENERAL_REGNO_P (regno))
    {
      /* Reload between vector reg and memory does not need an
	 intermediate register.  */
      if (MEM_P (x) && (mode == SFmode || mode == DFmode))
	return NO_REGS;
      else
	return GENERAL_REGS;
    }

  return NO_REGS;
}

/* Implement TARGET_SPILL_CLASS.
   Try spilling to a larger register class before spilling to memory.  */

static reg_class_t
csky_spill_class (reg_class_t rclass, machine_mode mode ATTRIBUTE_UNUSED)
{
  if ((rclass == MINI_REGS && !TARGET_MINI_REGISTERS)
      || (rclass == LOW_REGS && TARGET_HIGH_REGISTERS))
    return GENERAL_REGS;
  return NO_REGS;
}

/* Convert a static initializer array of feature bits to sbitmap
   representation.  */

static void
csky_initialize_isa (sbitmap isa, const enum csky_isa_feature *isa_bits)
{
  bitmap_clear (isa);
  while (*isa_bits != CSKY_ISA_FEATURE_GET (none))
    bitmap_set_bit (isa, *(isa_bits++));
}


/* Configure a build target TARGET from the user-specified options OPTS and
   OPTS_SET.  */

static void
csky_configure_build_target (struct csky_build_target *target,
			     struct cl_target_option *opts,
			     struct gcc_options *opts_set)
{
  const struct csky_processors *csky_selected_tune = NULL;
  struct csky_processors *csky_selected_cpu = NULL;
  struct csky_processors *csky_selected_arch = NULL;
  sbitmap all_sbits = sbitmap_alloc (CSKY_ISA_FEATURE_GET (max));
  bitmap_clear (all_sbits);

  bitmap_clear (target->isa);
  target->core_name = NULL;
  target->arch_name = NULL;

  if (opts_set->x_csky_arch_option)
    csky_selected_arch = &all_architectures[opts->x_csky_arch_option];

  if (opts_set->x_csky_cpu_option)
    {
      csky_selected_cpu = &all_cores[opts->x_csky_cpu_option];
      csky_selected_tune = &all_cores[opts->x_csky_cpu_option];
    }

  if (csky_selected_cpu)
    {
      /* TODO: support combination of features
	 between different cpu & arch, should based on arch.  */
      if (csky_selected_arch
	  && (csky_selected_cpu->base_arch != csky_selected_arch->base_arch))
	warning (0, "cpu %qs is not based on arch %qs, ignoring the arch",
		 csky_selected_cpu->name, csky_selected_arch->name);
      if (!csky_selected_arch)
	csky_selected_arch = &all_architectures[csky_selected_cpu->base_arch];
      csky_initialize_isa (all_sbits, csky_selected_arch->isa_bits);
      target->core_name = csky_selected_cpu->name;
    }
  else if (csky_selected_arch)
    {
      csky_selected_cpu = csky_selected_arch;
      target->arch_name = csky_selected_arch->name;
    }
  else /* If the user did not specify a processor, choose one for them.  */
    {
      csky_selected_cpu = &all_cores[TARGET_CPU_DEFAULT];
      csky_selected_arch = &all_architectures[csky_selected_cpu->base_arch];
      csky_initialize_isa (all_sbits, csky_selected_arch->isa_bits);
      target->core_name = csky_selected_cpu->name;
    }

  /* The selected cpu may be an architecture, so lookup tuning by core ID.  */
  if (!csky_selected_tune)
    csky_selected_tune = &all_cores[csky_selected_cpu->core];
  gcc_assert (csky_selected_tune);

  gcc_assert (csky_selected_arch);
  gcc_assert (csky_selected_cpu);
  csky_initialize_isa (target->isa, csky_selected_cpu->isa_bits);
  bitmap_ior (target->isa, target->isa, all_sbits);

  /* Finish initializing the target structure.  */
  target->arch_pp_name = csky_selected_cpu->arch;
  target->base_arch = csky_selected_cpu->base_arch;
  target->arch_core = csky_selected_cpu->core;

  sbitmap_free (all_sbits);
}


/* Implement TARGET_OPTION_OVERRIDE.  */

static void
csky_option_override (void)
{
  csky_active_target.isa = sbitmap_alloc (CSKY_ISA_FEATURE_GET (max));

  /* Create the default target_options structure.  We need this early
     to configure the overall build target.  */
  target_option_default_node = target_option_current_node
    = build_target_option_node (&global_options, &global_options_set);

  csky_configure_build_target (&csky_active_target,
			      TREE_TARGET_OPTION (target_option_default_node),
			      &global_options_set);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  csky_base_arch = csky_active_target.base_arch;

  if (flag_pic && !(CSKY_TARGET_ARCH (CK807)
		   || CSKY_TARGET_ARCH (CK810)
		   || CSKY_TARGET_ARCH (CK860)))
    {
      flag_pic = 0;
      warning (0, "%qs is not supported by arch %qs",
	       "-fPIC", csky_active_target.arch_pp_name);
    }

  /* Check floating-point options for consistency.  */
  if (TARGET_HARD_FLOAT)
    {
      const struct csky_fpu_desc *csky_selected_fpu = NULL;

      if (csky_fpu_index == TARGET_FPU_auto)
	{
	  const char *target_fpu_name;
	  bool ok;
	  int fpu_index;

	  if (csky_active_target.core_name != NULL
	      && !strchr (csky_active_target.core_name, 'f'))
	    target_fpu_name = "auto";
	  else if (CSKY_TARGET_ARCH (CK803) || !TARGET_DOUBLE_FLOAT)
	    target_fpu_name = "fpv2_sf";
	  else if (CSKY_TARGET_ARCH (CK860))
	    target_fpu_name = "fpv3";
	  else if (TARGET_DOUBLE_FLOAT && TARGET_FDIVDU)
	    target_fpu_name = "fpv2_divd";
	  else
#ifdef CSKY_FPUTYPE_DEFAULT
	    target_fpu_name = CSKY_FPUTYPE_DEFAULT;
#else
	    target_fpu_name = "fpv2";
#endif

	  ok = opt_enum_arg_to_value (OPT_mfpu_, target_fpu_name, &fpu_index,
				      CL_TARGET);
	  gcc_assert (ok);
	  csky_fpu_index = (enum csky_fpu_type) fpu_index;
	}

      if (CSKY_TARGET_ARCH (CK801) || CSKY_TARGET_ARCH (CK802))
	error ("%qs is not supported by arch %qs",
	       "-mhard-float", csky_active_target.arch_pp_name);
      else if (csky_fpu_index == TARGET_FPU_auto)
	error ("%<-mhard-float%> is not supported by the selected CPU");
      else
	{
	  csky_selected_fpu = &all_fpus[csky_fpu_index];
	  sbitmap fpu_bits = sbitmap_alloc (CSKY_ISA_FEATURE_GET (max));
	  csky_initialize_isa (fpu_bits, csky_selected_fpu->isa_bits);

	  bitmap_ior (csky_active_target.isa, csky_active_target.isa,
		      fpu_bits);

	  sbitmap_free (fpu_bits);
	}
    }
  else
    {
      if (TARGET_DOUBLE_FLOAT > 0)
	warning (0, "%<-mdouble-float%> ignored without %<-mhard-float%>");
      TARGET_DOUBLE_FLOAT = 0;
      if (TARGET_FDIVDU > 0)
	warning (0, "%<-mfdivdu%> ignored without %<-mhard-float%>");
      TARGET_FDIVDU = 0;
    }

  /* Initialize boolean versions of the architectural flags, for use
     in the .md file.  */

#undef	CSKY_ISA
#define CSKY_ISA(IDENT, DESC)						  \
  {									  \
    csky_arch_isa_features[CSKY_ISA_FEATURE_GET (IDENT)] =		   \
      bitmap_bit_p (csky_active_target.isa, CSKY_ISA_FEATURE_GET (IDENT)); \
  }
#include "csky_isa.def"
#undef	CSKY_ISA

  /* Extended LRW instructions are enabled by default on CK801, disabled
     otherwise.  */
  if (TARGET_ELRW == -1)
    TARGET_ELRW = CSKY_TARGET_ARCH (CK801);

  /* DSP is enabled either by the processor feature or -mdsp
     command-line option.  There is no -mno-dsp option as the assembler
     doesn't take one.  */
  if (!TARGET_DSP)
    TARGET_DSP = CSKY_ISA_FEATURE (dsp);

  /* There's both -mdiv and -mno-div.  Take default from processor if
     neither is specified explicitly.  */
  if (TARGET_DIV == -1)
    TARGET_DIV = CSKY_ISA_FEATURE (div);

  /* TARGET_CONSTANT_POOL is mandatory for CK801 and CK802 and optional
     for other CPUs.
     The reason why the compiler has to generate constant pools for CK801/2
     instead of deferring to the assembler is that these cores don't have a
     long branch instruction other than jbsr, which clobbers lr.  So for
     the compiler to correctly save/restore lr it has to know whether there
     are long branches, which depends on having accurate branch length
     counts, which in turn depends on having control over where constant
     pools are placed.  */
  if ((CSKY_TARGET_ARCH (CK801) || CSKY_TARGET_ARCH (CK802))
      && !TARGET_CONSTANT_POOL)
    error ("%qs is not supported by arch %qs",
	   "-mno-constpool", csky_active_target.arch_pp_name);
  else if (TARGET_CONSTANT_POOL == -1)
    TARGET_CONSTANT_POOL = (CSKY_TARGET_ARCH (CK801)
			    || CSKY_TARGET_ARCH (CK802));

  /* TARGET_MINI_REGISTERS is mandatory for CK801, the default for CK802,
     and optional for other CPUs.  TARGET_HIGH_REGISTERS is incompatible
     with TARGET_MINI_REGISTERS, is not supported by CK801/802/803,
     and is the default for other processors.
     See csky_conditional_register_usage.  */
  if (TARGET_MINI_REGISTERS > 0 && TARGET_HIGH_REGISTERS > 0)
    error ("%<-msmart%> is incompatible with %<-mhigh-registers%>");
  else if (CSKY_TARGET_ARCH (CK801)
	   || CSKY_TARGET_ARCH (CK802)
	   || CSKY_TARGET_ARCH (CK803))
    {
      if (CSKY_TARGET_ARCH (CK801)
	  || (CSKY_TARGET_ARCH (CK802) && TARGET_MINI_REGISTERS == -1))
	TARGET_MINI_REGISTERS = 1;
      else if (TARGET_MINI_REGISTERS == -1)
	TARGET_MINI_REGISTERS = 0;
      if (TARGET_HIGH_REGISTERS > 0)
	warning (0, "%qs is not supported by arch %qs",
		 "-mhigh-registers", csky_active_target.arch_pp_name);
      TARGET_HIGH_REGISTERS = 0;
    }
  else
    {
      if (TARGET_MINI_REGISTERS == -1)
	TARGET_MINI_REGISTERS = 0;
      if (TARGET_HIGH_REGISTERS == -1)
	TARGET_HIGH_REGISTERS = !TARGET_MINI_REGISTERS;
    }

  /* -mmultiple-stld is the default for everything but CK801, which
     doesn't support it.  */
  if (CSKY_TARGET_ARCH (CK801))
    {
      if (TARGET_MULTIPLE_STLD > 0)
	warning (0, "%qs is not supported by arch %qs",
		 "-mmultiple-stld", csky_active_target.arch_pp_name);
      TARGET_MULTIPLE_STLD = 0;
    }

  /* TODO  */

  /* Resynchronize the saved target options.  */
  cl_target_option_save (TREE_TARGET_OPTION (target_option_default_node),
			 &global_options, &global_options_set);

#ifdef ENABLE_TPF_DEBUG
  /* Don't emit DWARF4 unless specifically selected.  The TPF
     debuggers do not yet support DWARF 3/4.  */
  if (!OPTION_SET_P (dwarf_strict))
    dwarf_strict = 1;
  if (!OPTION_SET_P (dwarf_version))
    dwarf_version = 3;
#endif

  /* Don't run the scheduler before reload by default,
     since it tends to increase register pressure.  */
  if (!OPTION_SET_P (flag_schedule_insns))
    flag_schedule_insns = 0;

  csky_add_gc_roots ();
}


/* Return TRUE if X contains any references to TLS symbols.  */

bool
csky_tls_referenced_p (rtx x)
{
  if (!TARGET_TLS)
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


/* Implement TARGET_CANNOT_FORCE_CONST_MEM.
   Determine if it's legal to put X into the constant pool.  This
   is not possible for the address of thread-local symbols, which
   is checked above.  */

static bool
csky_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED,
			     rtx x)
{
  return csky_tls_referenced_p (x);
}


/* Implement TARGET_LEGITIMATE_CONSTANT_P.  Returns nonzero if the
   constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

static bool
csky_legitimate_constant_p (machine_mode mode, rtx x)
{
  return (!csky_cannot_force_const_mem (mode, x)
	  && CONSTANT_P (x));
}


/* Return true if X is valid as an CSKY addressing register.  */

static bool
is_csky_address_register_rtx_p (rtx x, int strict_p)
{
  int regno;

  if (!x)
    return false;
  if (!REG_P (x))
    return false;

  regno = REGNO (x);

  if (strict_p)
    return (CSKY_GENERAL_REGNO_P (regno)
	    || CSKY_GENERAL_REGNO_P (reg_renumber[regno]));
  else
    return CSKY_GENERAL_REGNO_P (regno) || regno >= FIRST_PSEUDO_REGISTER;
}


/* Return TRUE if X is a thread-local symbol.  */

static bool
csky_tls_symbol_p (rtx x)
{
  if (!TARGET_TLS)
    return false;

  if (GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_TLS_MODEL (x) != 0;
}


/* Handle lazy initialization of __tls_get_addr libfunc.  */
static GTY(()) rtx tls_get_addr_libfunc;

static rtx
get_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}


/* Emit a call to __tls_get_addr.  */

static rtx_insn *
csky_call_tls_get_addr (rtx x, rtx reg, rtx *valuep, int reloc)
{
  rtx label, labelno, unspec, tmp;
  rtx_insn *insns;

  start_sequence ();

  labelno = GEN_INT (tls_labelno++);
  label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_TLS_LABEL);
  unspec = gen_rtx_UNSPEC (Pmode,
			   gen_rtvec (3, x, GEN_INT (reloc), label),
			   UNSPEC_TLS);
  tmp = gen_reg_rtx (SImode);
  emit_move_insn (reg, unspec);
  emit_move_insn (tmp, label);
  emit_insn (gen_addsi3 (reg, reg, tmp));
  *valuep = emit_library_call_value (get_tls_get_addr (),
				     NULL_RTX, LCT_PURE, /* LCT_CONST?	*/
				     Pmode, reg, Pmode);
  insns = get_insns ();
  end_sequence ();
  return insns;
}

/* Helper function for csky_legitimize_address, to handle the TLS cases.
   REG is a scratch register and may be null.  */

rtx
csky_legitimize_tls_address (rtx x, rtx reg)
{
  rtx dest, tp, label, labelno, unspec, ret, eqv, addend, tmp;
  rtx_insn *insns;
  unsigned int model = SYMBOL_REF_TLS_MODEL (x);

  if (!reg)
    reg = gen_reg_rtx (SImode);

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      insns = csky_call_tls_get_addr (x, reg, &ret, TLS_GD32);
      dest = gen_reg_rtx (Pmode);
      emit_libcall_block (insns, dest, ret, x);
      return dest;

    case TLS_MODEL_LOCAL_DYNAMIC:
      insns = csky_call_tls_get_addr (x, reg, &ret, TLS_LDM32);

      /* Attach a unique REG_EQUIV, to allow the RTL optimizers to
	 share the LDM result with other LD model accesses.  */
      eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const1_rtx), UNSPEC_TLS);
      dest = gen_reg_rtx (Pmode);
      emit_libcall_block (insns, dest, ret, eqv);

      /* Load the addend.  */
      addend = gen_rtx_UNSPEC (Pmode,
			       gen_rtvec (2, x, GEN_INT (TLS_LDO32)),
			       UNSPEC_TLS);
      addend = force_reg (SImode, addend);
      return gen_rtx_PLUS (Pmode, dest, addend);

    case TLS_MODEL_INITIAL_EXEC:
      labelno = GEN_INT (tls_labelno++);
      label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_TLS_LABEL);
      unspec = gen_rtx_UNSPEC (Pmode,
			       gen_rtvec (3, x, GEN_INT (TLS_IE32), label),
			       UNSPEC_TLS);
      tmp = gen_reg_rtx (SImode);
      emit_move_insn (reg, unspec);
      emit_move_insn (tmp, label);
      emit_insn (gen_addsi3 (reg, reg, tmp));
      emit_move_insn (reg, gen_const_mem (Pmode, reg));
      tp = gen_rtx_REG (SImode, CSKY_TLS_REGNUM);
      return gen_rtx_PLUS (Pmode, tp, reg);

    case TLS_MODEL_LOCAL_EXEC:
      unspec = gen_rtx_UNSPEC (Pmode,
			       gen_rtvec (2, x, GEN_INT (TLS_LE32)),
			       UNSPEC_TLS);
      emit_move_insn (reg, unspec);
      tp = gen_rtx_REG (SImode, CSKY_TLS_REGNUM);
      return gen_rtx_PLUS (Pmode, tp, reg);

    default:
      abort ();
    }
}


/* Implement TARGET_LEGITIMIZE_ADDRESS.  */

static rtx
csky_legitimize_address (rtx x, rtx orig_x ATTRIBUTE_UNUSED,
			 machine_mode mode)
{
  if (csky_tls_symbol_p (x))
    return csky_legitimize_tls_address (x, NULL_RTX);

  if (GET_CODE (x) == PLUS)
    {
      rtx xop0 = XEXP (x, 0);
      rtx xop1 = XEXP (x, 1);

      if (is_csky_address_register_rtx_p (xop0, 0)
	  && CONST_INT_P (xop1))
	{
	  HOST_WIDE_INT offset = INTVAL (xop1);

	  /* Try to replace ld32 rx,(ry, offset), to addi16 rz, oimm8
	     and ld16 rx,(rz, new_ld_offset) to avoid emitting a
	     32-bit ld, but this addi has a range limitation.  */
	  if (optimize_size
	      && offset > CSKY_LD16_MAX_OFFSET (mode)
	      && offset <= (CSKY_ADDI16_MAX_IMM
			   + CSKY_LD16_MAX_OFFSET (mode)))
	    {
	      HOST_WIDE_INT new_ld_offset
		= offset & CSKY_LD16_OFFSET_MASK (mode);

	      xop0 = force_operand (plus_constant (Pmode, xop0,
						   offset - new_ld_offset),
				    NULL_RTX);
	      x = plus_constant (Pmode, xop0, new_ld_offset);
	    }
	  else if (offset < 0 && offset >= (-CSKY_SUBI16_MAX_IMM))
	    x = force_operand (x, NULL_RTX);
	  else if (offset > CSKY_LD16_MAX_OFFSET (mode)
		   || offset < 0)
	    {
	      /* For the remaining cases, force the constant into a
		 register.  */
	      xop1 = force_reg (SImode, xop1);
	      x = gen_rtx_PLUS (SImode, xop0, xop1);
	    }
	}

      /* If the index is store in register, force the
	 base to register.  */
      if (is_csky_address_register_rtx_p (xop1, 0)
	  && !is_csky_address_register_rtx_p (xop0, 0))
	{
	  xop0 = force_operand (xop0, NULL_RTX);
	  x = gen_rtx_PLUS (SImode, xop0, xop1);
	}
    }
  /* Make sure to take full advantage of the pre-indexed addressing mode
     with absolute addresses which often allows for the base register to
     be factorized for multiple adjacent memory references, and it might
     even allows for the mini pool to be avoided entirely. */
  else if (CONST_INT_P (x)  && optimize > 0)
    {
      HOST_WIDE_INT mask, base, index;
      rtx base_reg;

      mask = CSKY_LD16_OFFSET_MASK (mode);
      base = INTVAL (x) & ~mask;
      index = INTVAL (x) & mask;
      base_reg = force_reg (SImode, GEN_INT (base));
      x = plus_constant (Pmode, base_reg, index);
    }

  return x;
}


/* Return nonzero if INDEX is valid for an address index operand.
   ck801 use 16 bits ld
   ck802 use 16 and 32 bits ld
   others use ld and ldr.  */

static int
ck801_legitimate_index_p (machine_mode mode, rtx index,
			  int strict_p ATTRIBUTE_UNUSED)
{
  enum rtx_code code = GET_CODE (index);

  /* When the mode size is larger than 4, we may use two ld instruction
     to get data, the index and (index+1) should be valid.  */
  if (GET_MODE_SIZE (mode) >= 8)
    return (code == CONST_INT
	    && INTVAL (index) <	 CSKY_LD16_MAX_OFFSET (SImode)
	    && INTVAL (index) >= 0 && (INTVAL (index) & 3) == 0);

  if (code == CONST_INT && GET_MODE_SIZE (mode) > 0
      && INTVAL (index) <= CSKY_LD16_MAX_OFFSET (mode)
      && INTVAL (index) >= 0)
    return ((INTVAL (index) % GET_MODE_SIZE (mode)) == 0);

  return 0;
}


static int
ck802_legitimate_index_p (machine_mode mode, rtx index,
			  int strict_p ATTRIBUTE_UNUSED)
{
  enum rtx_code code = GET_CODE (index);

  /* When the mode size is larger than 4, we may use two ld instruction
     to get data, the index and (index+1) should be valid.  */
  if (GET_MODE_SIZE (mode) >= 8)
    return (code == CONST_INT
	    && INTVAL (index) < CSKY_LD32_MAX_OFFSET (SImode)
	    && INTVAL (index) >= 0 && (INTVAL (index) & 3) == 0);

  if (code == CONST_INT && GET_MODE_SIZE (mode) > 0
      && INTVAL (index) <= CSKY_LD32_MAX_OFFSET (mode)
      && INTVAL (index) >= 0)
    return ((INTVAL (index) % GET_MODE_SIZE (mode)) == 0);

  return 0;
}


/* The instruction ldr rz, (rx, ry << i), i can be 0,1,2,3.
   Check that SHIFT is valid, that the code is MULT, and that
   the shift is a power of 2.  */

static bool
is_ldr_shift_p (HOST_WIDE_INT shift, enum rtx_code code)
{
  if (code == ASHIFT)
    return (shift >= 0 && shift <= 3);
  else if (code == MULT)
    return (shift == 1
	    || shift == 2
	    || shift == 4
	    || shift == 8);
  else
    return false;
}


static int
ck810_legitimate_index_p (machine_mode mode, rtx index, int strict_p)
{
  enum rtx_code code = GET_CODE (index);

  if (code == CONST_INT && TARGET_HARD_FLOAT && CSKY_VREG_MODE_P (mode))
    return (INTVAL (index) < 1024 && INTVAL (index) >= 0
	    && (INTVAL (index) & 3) == 0);

  if (code == CONST_INT)
    {
      /* When the mode size is larger than 4, we may use two ld instruction
	 to get data, the index and (index+1) should be valid.  */
      if (GET_MODE_SIZE (mode) >= 8)
	return (INTVAL (index) < CSKY_LD32_MAX_OFFSET (SImode)
		&& INTVAL (index) >= 0 && (INTVAL (index) & 3) == 0);

      if (GET_MODE_SIZE (mode) > 0
	  && INTVAL (index) <= CSKY_LD32_MAX_OFFSET (mode)
	  && INTVAL (index) >= 0)
	return ((INTVAL (index) % GET_MODE_SIZE (mode)) == 0);
    }
  /* Allow ld.w rx, (gb, sym@got) when -fpic specially.  */
  else if (code == UNSPEC)
    return (flag_pic == 1
	    && (XINT (index, 1) == UNSPEC_PIC_SYMBOL_PLT
		|| XINT (index, 1) == UNSPEC_PIC_SYMBOL_GOT));
  /* The follow index is for ldr instruction, the ldr cannot
     load dword data, so the mode size should not be larger than
     4.  */
  else if (GET_MODE_SIZE (mode) <= 4
	   || (TARGET_HARD_FLOAT && CSKY_VREG_MODE_P (mode)))
    {
      if (is_csky_address_register_rtx_p (index, strict_p))
	return 1;
      else if (code == MULT || code == ASHIFT)
	{
	  rtx xiop0 = XEXP (index, 0);
	  rtx xiop1 = XEXP (index, 1);

	  /* FIXME can the xiop1 be the reg and xiop0 be the int when mult?  */
	  return (is_csky_address_register_rtx_p (xiop0, strict_p)
		  && CONST_INT_P (xiop1)
		  && is_ldr_shift_p (INTVAL (xiop1), code));
	}
    }

  return 0;
}


static int
csky_legitimate_index_p (machine_mode mode, rtx index, int strict_p)
{
  if (CSKY_TARGET_ARCH (CK801))
    return ck801_legitimate_index_p (mode, index, strict_p);
  else if (CSKY_TARGET_ARCH (CK802))
    return ck802_legitimate_index_p (mode, index, strict_p);
  else
    return ck810_legitimate_index_p (mode, index, strict_p);
}


/* Implement TARGET_LEGITIMATE_ADDRESS_P.
   Recognizes RTL expressions that are valid memory addresses for an
   instruction.  The MODE argument is the machine mode for the MEM
   expression that wants to use this address.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

static bool
csky_legitimate_address_p (machine_mode mode, rtx addr, bool strict_p,
			   code_helper = ERROR_MARK)
{
  enum rtx_code code = GET_CODE (addr);

  /* Match the RTX form emitted for constant pool references.
     After reload constants split into minipools will have addresses
     from a LABEL_REF.  */
  if (reload_completed
      && ((code == LABEL_REF)
	   || (code == CONST
	       && GET_CODE (XEXP (addr, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (addr, 0), 0)) == LABEL_REF
	       && CONST_INT_P (XEXP (XEXP (addr, 0), 1)))))
    return 1;

  if (is_csky_address_register_rtx_p (addr, strict_p))
    return 1;
  /* It is a pc-relative load, may be generated for constpool.  */
  else if (GET_CODE (addr) == LABEL_REF)
    return 1;

  if (code == PLUS)
    {
      rtx xop0 = XEXP (addr, 0);
      rtx xop1 = XEXP (addr, 1);

      return ((is_csky_address_register_rtx_p (xop0, strict_p)
	       && csky_legitimate_index_p (mode, xop1, strict_p))
	      || (is_csky_address_register_rtx_p (xop1, strict_p)
		  && csky_legitimate_index_p (mode, xop0, strict_p)));
    }

  return 0;
}


/* Functions to save and restore machine-specific function data.  */

static struct machine_function *
csky_init_machine_status (void)
{
  struct machine_function *machine;

  machine = ggc_cleared_alloc<machine_function> ();

#if CSKY_FT_UNKNOWN != 0
  machine->func_type = CSKY_FT_UNKNOWN;
#endif
  return machine;
}


/* Implement INIT_EXPANDERS.  */

void
csky_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = csky_init_machine_status;
}


/* Implement TARGET_CANNOT_COPY_INSN_P.
   We must not copy any rtx that uses a pc-relative address.  */

static bool
csky_cannot_copy_insn_p (rtx_insn *insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == UNSPEC
	  && (XINT (x, 1) == UNSPEC_TLS_LABEL
	      || XINT (x, 1) == UNSPEC_PIC_SYMBOL_GOTPC_GRS))
	return true;
    }
  return false;
}


/* Extract the parts of an RTL expression that is a valid memory address
   for an instruction.  Return FALSE if it is a invalid memory address.  */

struct csky_address
{
  rtx base, index, symbol, label, disp;
  HOST_WIDE_INT scale;
};

static bool
decompose_csky_address (rtx addr, struct csky_address *out)
{
  rtx base = NULL_RTX, index = NULL_RTX, disp = NULL_RTX;
  HOST_WIDE_INT scale = 0;
  rtx scale_rtx = NULL_RTX;
  int i;

  out->base = out->index = out->symbol = out->label = out->disp = NULL_RTX;
  out->scale = 0;

  if (REG_P (addr))
    {
      out->base = addr;
      return true;
    }

  if (GET_CODE (addr) == LABEL_REF)
    {
      out->label = addr;
      return true;
    }

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS)
    {
      rtx addends[2], op;

      addends[0] = XEXP (addr, 0);
      addends[1] = XEXP (addr, 1);

      if (GET_CODE (addends[0]) == LABEL_REF && CONST_INT_P (addends[1]))
	{
	  out->label = addends[0];
	  out->disp = addends[1];
	  return true;
	}

      if (!REG_P (addends[0]))
	std::swap (addends[0], addends[1]);

      for (i = 0; i < 2; ++i)
	{
	  op = addends[i];
	  switch (GET_CODE (op))
	    {
	    case REG:
	      if (!base)
		base = op;
	      else if (!index)
		{
		  index = op;
		  scale = 1;
		}
	      else
		return false;
	      break;
	    case CONST_INT:
	    case UNSPEC:
	      if (disp)
		return false;
	      disp = op;
	      break;
	    case MULT:
	      if (index)
		return false;
	      index = XEXP (op, 0);
	      scale_rtx = XEXP (op, 1);
	      if (!CONST_INT_P (index) && !CONST_INT_P (scale_rtx))
		return false;
	      else if (CONST_INT_P (index))
		std::swap (index, scale_rtx);
	      scale = INTVAL (scale_rtx);
	      break;
	    case ASHIFT:
	      if (index)
		return false;
	      index = XEXP (op, 0);
	      scale_rtx = XEXP (op, 1);
	      if (!CONST_INT_P (scale_rtx))
		return false;
	      scale = 1 << INTVAL (scale_rtx);
	      break;
	    default:
	      return false;
	    }
	}
    }

  if (!base)
    return false;

  out->base = base;
  out->index = index;
  out->disp = disp;
  out->scale = scale;

  return true;
}

/* Helper function for the csky_simple_mem_operand predicate.  Returns
   true if OP is an address of the form reg + displacement.  */

bool
csky_simple_addr_operand_p (rtx op)
{
  struct csky_address addr;

  if (!decompose_csky_address (op, &addr))
    return false;

  /* FIXME The PIC related code.
     Check if load the symbol address from got table.  */
  if (addr.disp && GET_CODE (addr.disp) == UNSPEC)
    return false;
  if (!addr.index && !addr.symbol)
    return true;
  return false;
}


/* Print the UNSPEC operand in X to the STREAM.  */

static void
csky_output_pic_addr_const (FILE *stream, rtx x, int code)
{

  if (GET_CODE (x) != UNSPEC)
    return;

  if (UNSPEC_TLS == XINT (x, 1))
    {
      /* FIXME It is not reached */
      return;
    }

  csky_print_operand (stream, XVECEXP (x, 0, 0), code);

  switch (XINT (x, 1))
    {
    case UNSPEC_PIC_SYMBOL_GOTOFF:
      fputs ("@GOTOFF", stream);
      break;
    case UNSPEC_PIC_SYMBOL_PLT:
      fputs ("@PLT", stream);
      break;
    case UNSPEC_PIC_SYMBOL_GOT:
      fputs ("@GOT", stream);
      break;
    case UNSPEC_PIC_SYMBOL_GOTPC:
      fputs ("@GOTPC", stream);
      break;
    case UNSPEC_PIC_SYMBOL_BSR:
      break;
    default:
      break;
    }
}


/* Output the constpool label according to the rtx expression X.  */

static void
csky_output_constpool_label (FILE *stream, rtx x)
{
  char buf[15];

  gcc_assert (GET_CODE (x) == LABEL_REF);
  x = XEXP (x, 0);

  if (GET_CODE (x) == UNSPEC_VOLATILE && XINT (x, 1) == VUNSPEC_POOL_LABEL)
    {
      ASM_GENERATE_INTERNAL_LABEL (buf, CSKY_CONSTPOOL_LABEL_PREFIX,
				   INTVAL (XVECEXP (x, 0, 0)));
      assemble_name (stream, buf);
    }
}


/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */

static void
csky_print_operand_address (FILE *stream,
			    machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x)
{

  struct csky_address addr;

  decompose_csky_address (x, &addr);

  if (addr.label && addr.disp && GET_CODE (addr.disp) == CONST_INT)
    {
      fprintf (stream, "[");
      csky_output_constpool_label (stream, addr.label);
      fprintf (stream, "+%d]", (int) INTVAL (addr.disp));
    }
  else if (addr.label)
    {
      fprintf (stream, "[");
      csky_output_constpool_label (stream, addr.label);
      fprintf (stream, "]");
    }
  else if (addr.symbol && addr.disp && GET_CODE (addr.disp) == CONST_INT)
    {
      fprintf (stream, "[");
      output_addr_const (stream, addr.symbol);
      fprintf (stream, "+%d]", (int) INTVAL (addr.disp));
    }
  else if (addr.symbol)
    {
      fprintf (stream, "[");
      output_addr_const (stream, addr.symbol);
      fprintf (stream, "]");
    }
  else if (addr.disp && GET_CODE (addr.disp) == CONST_INT)
    fprintf (stream, "(%s, %d)",
	     reg_names[REGNO (addr.base)], (int) INTVAL (addr.disp));
  else if (addr.disp && GET_CODE (addr.disp) == UNSPEC)
    {
      if (REGNO (addr.base) != CSKY_GB_REGNUM)
	fprintf (stream, "(%s, ", reg_names[REGNO (addr.base)]);
      else
	fprintf (stream, "[");
      csky_output_pic_addr_const (stream, addr.disp, 0);
      fprintf (stream, "%s", (REGNO (addr.base) != CSKY_GB_REGNUM)
	       ? ")" : "]");
    }
  else if (addr.index)
    fprintf (stream, "(%s, %s << %d)",
	     reg_names[REGNO (addr.base)], reg_names[REGNO (addr.index)],
	     exact_log2 ((int) (addr.scale)));
  else
    fprintf (stream, "(%s, 0)", reg_names[REGNO (addr.base)]);
}


/* Implement TARGET_PRINT_OPERAND.
   Print operand X (an rtx) in assembler syntax to file STREAM
   according to modifier CODE.

   'N'	print the log2(X+1), mainly used for bmaski
   'P'	print the log2(X)
   'Q'	print the log2(~X)
   'O'	print a decimal number
   'M'	print a decimal number as its negative
   'R'	print the next register or memory location along, i.e. the lsw in
   a double word value
   'H'	print the high 16 bits of a constant.  */

static void
csky_print_operand (FILE *stream, rtx x, int code)
{
  switch (code)
    {
    case 'N':
      if ((INTVAL (x) & 0xffffffff) == 0xffffffff)
	fprintf (stream, "0");
      else
	fprintf (stream, "%d",
		 (int) exact_log2 ((INTVAL (x) & 0xffffffff) + 1) % 32);
      break;
    case 'P':
      fprintf (stream, "%d",
	       (int) exact_log2 (INTVAL (x) & 0xffffffff));
      break;
    case 'Q':
      fprintf (stream, "%d",
	       (int) exact_log2 (~INTVAL (x) & 0xffffffff));
      break;
    case 'O':
      fprintf (stream, "%d", (int) INTVAL (x));
      break;
    case 'M':
      fprintf (stream, "%d", (int) (-INTVAL (x)));
      break;
    case 'R':
      /* Next location along in memory or register.  */
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x) + 1], stream);
	  break;
	case MEM:
	  csky_print_operand_address
	    (stream, GET_MODE (x), XEXP (adjust_address (x, SImode, 4), 0));
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    case 'H':
      fprintf (stream, "%ld", (long)((INTVAL (x) & 0xFFFF0000) >> 16));
      break;
    default:
      switch (GET_CODE (x))
	{
	case REG:
	  fputs (reg_names[REGNO (x)], stream);
	  break;
	case MEM:
	  output_address (GET_MODE (x), XEXP (x, 0));
	  break;
	case UNSPEC:
	  csky_output_pic_addr_const (stream, x, code);
	  break;
	case CONST_DOUBLE:
	  {
	    char fpstr[20];
	    real_to_decimal ( fpstr, CONST_DOUBLE_REAL_VALUE (x),
			     sizeof (fpstr), 0, 1);
	    fprintf (stream, "%s", fpstr);
	  }
	  break;
	default:
	  output_addr_const (stream, x);
	  break;
	}
      break;
    }
}



/* Implement TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS.  */

static bool
csky_allocate_stack_slots_for_args (void)
{
  /* Naked functions should not allocate stack slots for arguments.  */
  return !CSKY_FUNCTION_IS_NAKED (get_csky_current_func_type ());
}


/* Can we generate a constant with a single instruction, without using
   lrw?	 */

static int
const_ok_for_cskyv2 (HOST_WIDE_INT value)
{
  /* Try exact power of two. It can be generated by bgeni.  */
  if (CSKY_CONST_OK_FOR_Ub (value))
    return 1;

  /* Try exact power of two - 1.  It can be generated by bmaski.  */
  if (CSKY_CONST_OK_FOR_Uc (value) && value != -1)
    return 1;

  /* Try if it can be generated by movi.  */
  if (CSKY_CONST_OK_FOR_I (value))
    return 1;

  /* The constant can be generated by movih.
     Notice that movih is a 32-bit instruction.  */
  if (CSKY_CONST_OK_FOR_MOVIH (value))
    return 1;

  return 0;
}


/* Tricks for synthesizing constants from values that can be directly
   manipulated by machine instructions.  */

enum csky_inline_const_type
{
  IC_UNINLINABLE = 0, /* Not inlineable */
  IC_SINGLE,	      /* Single instruction */
  IC_APPEND_NOT,      /* Single instruction followed by a not */
  IC_APPEND_ADDI,     /* Single insn followed by an addi */
  IC_APPEND_SUBI,     /* Single insn followed by a subi */
  IC_BGENI_ADDI,      /* Single insn(bgeni) followed by an addi */
  IC_BGENI_SUBI,      /* Single insn(bgeni) followed by a subi */
  IC_APPEND_BSETI,    /* Single insn followed by bseti */
  IC_APPEND_MOVI,     /* Single insn followed by movi */
  IC_APPEND_BCLRI,    /* Single insn followed by bclri */
  IC_APPEND_ROTLI,    /* Single insn followed by rotli */
  IC_APPEND_LSLI,     /* Single insn followed by lsli */
  IC_APPEND_IXH,      /* Single insn followed by ixh */
  IC_APPEND_IXW	      /* Single insn followed by ixw */
};


/* Try tricks to load a constant inline and return the trick number if
   success, or IC_UNINLINABLE.  */

static enum csky_inline_const_type
try_csky_constant_tricks (HOST_WIDE_INT value, HOST_WIDE_INT *x,
			  HOST_WIDE_INT *y)
{
  HOST_WIDE_INT i, value_invert;
  unsigned HOST_WIDE_INT bit, shf, rot, lobits, hibits;

  value &= 0xffffffff;
  value_invert = ~value & 0xffffffff;

  if (const_ok_for_cskyv2 (value))
    {
      *x = value;
      return IC_SINGLE;
    }

  /* Since movih is 32 bits, do not use it here, better code may
     be generated later.  */
  if (const_ok_for_cskyv2 (value_invert)
      && !CSKY_CONST_OK_FOR_MOVIH (value_invert))
    {
      *x = value_invert;
      return IC_APPEND_NOT;
    }

  /* One immediate generate instruction, and one 16-bit subi or addi.  */
  for (i = 1; i <= 32; i++)
    {
      if (const_ok_for_cskyv2 (value - i)
	  && !CSKY_CONST_OK_FOR_MOVIH (value - i))
	{
	  *x = value - i;
	  *y = i;
	  return IC_APPEND_ADDI;
	}

      if (const_ok_for_cskyv2 (value + i)
	  && !CSKY_CONST_OK_FOR_MOVIH (value - i))
	{
	  *x = value + i;
	  *y = i;
	  return IC_APPEND_SUBI;
	}
    }

  /* Generate bgeni + addi.  */
  if (CSKY_CONST_OK_FOR_Ub (value & 0xfffff000))
    {
      *x = (value & 0xfffff000);
      *y = (value & 0xfff);
      return IC_BGENI_ADDI;
    }

  /* Generate bgeni + subi.  */
  lobits = value & 0xfff;
  hibits = (unsigned HOST_WIDE_INT)(value & 0xfffff000) + (1 << 12);
  if (exact_log2 (hibits) >= 1
      && exact_log2 (hibits) <= 30
      && lobits != 0)
    {
      *x = hibits;
      *y = (0x1000 - lobits);
      return IC_BGENI_SUBI;
    }

  /* One immediate generate instruction, and one bseti or bclri.  */
  bit = 0x80000000ULL;
  for (i = 0; i <= 31; i++)
    {
      if (const_ok_for_cskyv2 (value & ~bit)
	  && !CSKY_CONST_OK_FOR_MOVIH (value & ~bit))
	{
	  *y = bit;
	  *x = (value & ~bit);
	  return IC_APPEND_BSETI;
	}

      if (const_ok_for_cskyv2 (value | bit)
	  && !CSKY_CONST_OK_FOR_MOVIH (value | bit))
	{
	  *y = ~bit & 0xffffffff;
	  *x = value | bit;
	  return IC_APPEND_BCLRI;
	}

      bit >>= 1;
    }

  /* One immediate generate instruction, and one rotli or lsli.  */
  shf = value;
  rot = value;
  for (i = 1; i < 31; i++)
    {
      int c;

      /* Rotate left.  */
      c = rot << 31;
      rot >>= 1;
      rot &= 0x7FFFFFFF;
      rot |= c;

      if (const_ok_for_cskyv2 (rot) && !CSKY_CONST_OK_FOR_MOVIH (rot))
	{
	  *y = i;
	  *x = rot;
	  return IC_APPEND_ROTLI;
	}

      /* Can't use logical shift when low order bit is one.  */
      if (shf & 1)
	shf = 0;
      else
	shf >>= 1;

      if (shf != 0 && const_ok_for_cskyv2 (shf)
	  && !CSKY_CONST_OK_FOR_MOVIH (shf))
	{
	  *y = i;
	  *x = shf;
	  return IC_APPEND_LSLI;
	}
    }

  /* One immediate generate instruction, and one ixh.  */
  if (CSKY_ISA_FEATURE (E2)
      && (value % 3) == 0
      && const_ok_for_cskyv2 (value / 3)
      && !CSKY_CONST_OK_FOR_MOVIH (value / 3))
    {
      *x = value / 3;
      return IC_APPEND_IXH;
    }

  /* One immediate generate instruction, and one ixw.  */
  if (CSKY_ISA_FEATURE (E2)
      && (value % 5) == 0
      && const_ok_for_cskyv2 (value / 5)
      && !CSKY_CONST_OK_FOR_MOVIH (value / 5))
    {
      *x = value / 5;
      return IC_APPEND_IXW;
    }

  /* Generate movih + bseti.  */
  if (CSKY_CONST_OK_FOR_Ub (value & 0xffff))
    {
      *x = value & 0xffff0000;
      *y = value & 0xffff;
      return IC_APPEND_BSETI;
    }

  /* Generate movih + not.  */
  if (CSKY_CONST_OK_FOR_MOVIH (value_invert))
    {
      *x = value_invert;
      return IC_APPEND_NOT;
    }

  /* One movih, and one 16bits addi or subi.  */
  for (i = 1; i <= 32; i++)
    {
      if (CSKY_CONST_OK_FOR_MOVIH (value - i))
	{
	  *x = value - i;
	  *y = i;
	  return IC_APPEND_ADDI;
	}

      if (CSKY_CONST_OK_FOR_MOVIH (value + i))
	{
	  *x = value + i;
	  *y = i;
	  return IC_APPEND_SUBI;
	}
    }

  /* One movih, and one bseti or bclri.  */
  bit = 0x80000000ULL;
  for (i = 0; i <= 31; i++)
    {
      if (CSKY_CONST_OK_FOR_MOVIH (value & ~bit))
	{
	  *y = bit;
	  *x = value & ~bit;
	  return IC_APPEND_BSETI;
	}

      if (CSKY_CONST_OK_FOR_MOVIH (value | bit))
	{
	  *y = ~bit & 0xffffffff;
	  *x = value | bit;
	  return IC_APPEND_BCLRI;
	}

       bit >>= 1;
    }

  /* One movih, and one rotli or lsli.  */
  shf = value;
  rot = value;
  for (i = 1; i < 31; i++)
    {
      int c;

      /* Rotate left.  */
      c = rot << 31;
      rot >>= 1;
      rot &= 0x7FFFFFFF;
      rot |= c;

      if (CSKY_CONST_OK_FOR_MOVIH (rot))
	{
	  *y = i;
	  *x = rot;
	  return IC_APPEND_ROTLI;
	}

      /* Can't use logical shift when low order bit is one.  */
      if (shf & 1)
	shf = 0;
      else
	shf >>= 1;

      if (shf != 0 && CSKY_CONST_OK_FOR_MOVIH (shf))
	{
	  *y = i;
	  *x = shf;
	  return IC_APPEND_LSLI;
	}
    }

  return IC_UNINLINABLE;
}


/* Actually output a constant using a trick.
   FIXME: I think this would be better handled by a splitter than at the
   asm output level.  */

static const char *
csky_output_inline_const (machine_mode mode, rtx operands[])
{
  HOST_WIDE_INT x = 0, y = 0;
  enum csky_inline_const_type trick_type;
  rtx out_operands[3];
  char buf[256];
  char load_op[128];
  const char *dst_fmt;
  HOST_WIDE_INT value = INTVAL (operands[1]);
  int ivalue = (int) value;
  unsigned int uvalue = (unsigned int) value;

  trick_type = try_csky_constant_tricks (value, &x, &y);
  /* lrw's are handled separately: Large inlinable constants never get
     turned into lrw's.  Our caller uses try_csky_constant_tricks to back
     off to an lrw rather than calling this routine.  */
  gcc_assert (trick_type != IC_UNINLINABLE);

  /* Operands: 0 = dst, 1 = load immedate., 2 = adjust immedate.  */
  out_operands[0] = operands[0];
  out_operands[1] = GEN_INT (x);
  if (trick_type != IC_SINGLE && trick_type != IC_APPEND_NOT)
    out_operands[2] = GEN_INT (y);

  /* Select dst format based on mode.  */
  if (mode == DImode && TARGET_BIG_ENDIAN)
    dst_fmt = "%R0";
  else
    dst_fmt = "%0";

  /* Try movi16: 0~31,movi32: 0~65535.  */
  if (CSKY_CONST_OK_FOR_I (x))
    sprintf (load_op, "movi\t%s, %%1", dst_fmt);
  /* Try exact power of two - 1.  */
  else if (CSKY_CONST_OK_FOR_Uc (x))
    sprintf (load_op, "bmaski\t%s, %%N1", dst_fmt);
  /* Try movih.  */
  else if (CSKY_CONST_OK_FOR_MOVIH (x))
    sprintf (load_op, "movih\t%s, %%H1", dst_fmt);
  else
    {
      sprintf (load_op, "BADMOVI-inline_const %s, %%1", dst_fmt);
      gcc_unreachable ();
    }

  switch (trick_type)
    {
    case IC_SINGLE:
      strcpy (buf, load_op);
      break;
    /* Add instruction 'not'.  */
    case IC_APPEND_NOT:
      sprintf (buf, "%s\n\tnot\t%s, %s\t// %d 0x%x", load_op, dst_fmt,
	       dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'addi'.  */
    case IC_APPEND_ADDI:
      sprintf (buf, "%s\n\taddi\t%s, %s, %%2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'subi'.  */
    case IC_APPEND_SUBI:
      sprintf (buf, "%s\n\tsubi\t%s, %s, %%2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'addi', the last instruction is bgeni.  */
    case IC_BGENI_ADDI:
      sprintf (buf, "%s\n\taddi\t%s, %s, %%2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'subi', the last instruction is bgeni.  */
    case IC_BGENI_SUBI:
      sprintf (buf, "%s\n\tsubi\t%s, %s, %%2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'bseti'.  */
    case IC_APPEND_BSETI:
      sprintf (buf, "%s\n\tbseti\t%s, %s, %%P2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'movi'.  */
    case IC_APPEND_MOVI:
      sprintf (buf, "%s\n\tmovi\t%s, %%2\t// %d 0x%x", load_op, dst_fmt,
	       ivalue, uvalue);
      break;
    /* Add instruction 'bclri'.  */
    case IC_APPEND_BCLRI:
      sprintf (buf, "%s\n\tbclri\t%s, %s, %%Q2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'rotli'.  */
    case IC_APPEND_ROTLI:
      sprintf (buf, "%s\n\trotli\t%s, %s, %%2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'lsli'.  */
    case IC_APPEND_LSLI:
      sprintf (buf, "%s\n\tlsli\t%s, %s, %%2\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'ixh'.  */
    case IC_APPEND_IXH:
      sprintf (buf, "%s\n\tixh\t%s, %s, %s\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    /* Add instruction 'ixw'.  */
    case IC_APPEND_IXW:
      sprintf (buf, "%s\n\tixw\t%s, %s, %s\t// %d 0x%x", load_op,
	       dst_fmt, dst_fmt, dst_fmt, ivalue, uvalue);
      break;
    default:
      return "";
    }

  output_asm_insn (buf, out_operands);

  return "";
}

/* This is a helper function for the Uo constraint for movsi patterns.  */

bool
csky_inlinable_constant (HOST_WIDE_INT value)
{
  HOST_WIDE_INT x, y;
  return (!(CSKY_TARGET_ARCH (CK802) || CSKY_TARGET_ARCH (CK801))
	  && try_csky_constant_tricks (value, &x, &y));
}


/* Return true if the constant VAL can be expressed by an 8-bit constant
   with a shift value, filling in *BASE and *SHIFT.  */

bool
csky_shifted_imm8_constant (unsigned HOST_WIDE_INT val,
			    unsigned int *base, unsigned int *shift)
{
  unsigned HOST_WIDE_INT mask = 0xff;
  int i;
  val = val & (unsigned HOST_WIDE_INT) 0xffffffffu;
  if (val == 0)
    return 0;

  for (i = 0; i < 25; i++)
    if ((val & (mask << i)) == val)
      {
	if (base)
	  *base = (unsigned int) (val >> i);
	if (shift)
	  *shift = (unsigned int) i;
	return true;
      }

  return false;
}


/* Output a move of a word or less value.  */

const char *
csky_output_move (rtx insn ATTRIBUTE_UNUSED, rtx operands[],
		  machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  struct csky_address op0, op1;

  if (REG_P (dst))
    {
      /* The situation mov reg to reg.  */
      if (REG_P (src))
	{
	  int dstreg = REGNO (dst);
	  int srcreg = REGNO (src);

	  /* hilo registers exchange their places,
	     and their order of Dimode as same as other
	     general registers in LITTLE_ENDIAN mode.  */
	  if (TARGET_BIG_ENDIAN)
	    {
	      if (dstreg == CSKY_HI_REGNUM)
		return "mthi\t%1";
	      else if (dstreg == CSKY_LO_REGNUM)
		return "mtlo\t%1";
	      else if (srcreg == CSKY_HI_REGNUM)
		return "mfhi\t%0";
	      else if (srcreg == CSKY_LO_REGNUM)
		return "mflo\t%0";
	    }
	  else
	    {
	      if (dstreg == CSKY_HI_REGNUM)
		return "mtlo\t%1";
	      else if (dstreg == CSKY_LO_REGNUM)
		return "mthi\t%1";
	      else if (srcreg == CSKY_HI_REGNUM)
		return "mflo\t%0";
	      else if (srcreg == CSKY_LO_REGNUM)
		return "mfhi\t%0";
	    }

	  if (CSKY_VREG_P (dstreg) && CSKY_VREG_P (srcreg))
	    {
	      if (CSKY_ISA_FEATURE (fpv2_sf))
		return "fmovs\t%0, %1";
	      else if (CSKY_ISA_FEATURE (fpv3_sf))
		return "fmov.32\t%0, %1";
	      else
		gcc_unreachable ();
	    }
	  if (CSKY_VREG_P (dstreg))
	    {
	      if (CSKY_ISA_FEATURE (fpv2_sf))
		return "fmtvrl\t%0, %1";
	      else if (CSKY_ISA_FEATURE (fpv3_sf))
		return "fmtvr.32.1\t%0, %1";
	      else
		gcc_unreachable ();
	    }
	  if (CSKY_VREG_P (srcreg))
	    {
	      if (CSKY_ISA_FEATURE (fpv2_sf))
		return "fmfvrl\t%0, %1";
	      else if (CSKY_ISA_FEATURE (fpv3_sf))
		return "fmfvr.32.1\t%0, %1";
	      else
		gcc_unreachable ();
	    }
	  if (REGNO (src) == CSKY_CC_REGNUM)
	    return "mvc\t%0";
	  else
	    return "mov\t%0, %1";
	}
      /* The situation mov memory to reg.  */
      else if (GET_CODE (src) == MEM)
	{
	  decompose_csky_address (XEXP (src, 0), &op1);

	  if (op1.index)
	    switch (GET_MODE (src))
	      {
	      case E_HImode:
	      case E_HFmode:
		return "ldr.h\t%0, %1";
	      case E_QImode:
		return "ldr.b\t%0, %1";
	      case E_SImode:
	      case E_SFmode:
		if (CSKY_VREG_P (REGNO (dst)))
		  {
		    if (CSKY_ISA_FEATURE(fpv2_sf))
		      return "fldrs\t%0, %1";
		    else if (CSKY_ISA_FEATURE(fpv3_sf))
		      return "fldr.32\t%0, %1";
		    else
		      gcc_unreachable ();
		    }
		else
		  return "ldr.w\t%0, %1";
	      default:
		gcc_unreachable ();
	      }
	  /* Generate lrw rx, [LABEL].  This happens when the compiler
	     generates constant pool references and uses lrw to get the
	     constant into memory.  */
	  else if (op1.label)
	    return "lrw\t%0, %1";
	  /* Generate lrs.w rx, [symbol@GOT/PLT].  */
	  else if (flag_pic == 1 && op1.disp && GET_CODE (op1.disp) == UNSPEC)
	    return "lrs.w\t%0, %1";
	  else
	    switch (GET_MODE (src))
	      {
	      case E_HImode:
	      case E_HFmode:
		return "ld.h\t%0, %1";
	      case E_QImode:
		return "ld.b\t%0, %1";
	      case E_SFmode:
	      case E_SImode:
		if (CSKY_VREG_P (REGNO (dst)))
		  {
		     if (CSKY_ISA_FEATURE(fpv2_sf))
		       return "flds\t%0, %1";
		     else if (CSKY_ISA_FEATURE(fpv3_sf))
		       return "fld.32\t%0, %1";
		     else
		       gcc_unreachable ();
		   }
		else
		  return "ld.w\t%0, %1";
	      default:
		gcc_unreachable ();
	      }
	}
      /* The situation mov integer to reg.  */
      else if (GET_CODE (src) == CONST_INT ||
	       (GET_CODE (src) == CONST_DOUBLE && GET_MODE (src) == SFmode))
	{
	  HOST_WIDE_INT x, y;
	  const REAL_VALUE_TYPE *d;
	  long l;

	  if (GET_CODE (src) == CONST_DOUBLE && GET_MODE (src) == SFmode)
	    {
	      d = CONST_DOUBLE_REAL_VALUE (src);
	      REAL_VALUE_TO_TARGET_SINGLE (*d, l);
	      operands[1] = GEN_INT (l);
	      src = operands[1];
	    }

	  if (try_csky_constant_tricks (INTVAL (src), &x, &y))
	    return csky_output_inline_const (SImode, operands);
	  /* Return '#' to split it.  */
	  else if (CSKY_CONST_OK_FOR_T (INTVAL (src)))
	    return "#";
	  else
	    return "lrw\t%0, %x1\t";
	}
      else if (TARGET_ANCHOR && GET_CODE (src) == SYMBOL_REF)
	{
	  if (SYMBOL_REF_FUNCTION_P (src))
	    return "lrw\t%0, %1@BTEXT";
	  else
	    return "lrw\t%0, %1@BDATA";
	}
      else if (GET_CODE (src) == UNSPEC
	       && XINT (src, 1) == UNSPEC_PIC_SYMBOL_GRS)
	return "grs\t%0, %1";
      else
	return "lrw\t%0, %1";
    }
  else if (GET_CODE (dst) == MEM)
    {
      decompose_csky_address (XEXP (dst, 0), &op0);

      if (op0.index)
	switch (GET_MODE (src))
	  {
	  case E_HImode:
	    return "str.h\t%1, %0";
	  case E_QImode:
	    return "str.b\t%1, %0";
	  case E_SFmode:
	  case E_SImode:
	    if (CSKY_VREG_P (REGNO (src)))
	      {
		if (CSKY_ISA_FEATURE(fpv2_sf))
		  return "fstrs\t%1, %0";
		else if (CSKY_ISA_FEATURE(fpv3_sf))
		  return "fstr.32\t%1, %0";
		else
		  gcc_unreachable ();
	      }
	    else
	      return "str.w\t%1, %0";
	  default:
	    gcc_unreachable ();
	  }
      else
	switch (GET_MODE (dst))
	  {
	  case E_HImode:
	    return "st.h\t%1, %0";
	  case E_QImode:
	    return "st.b\t%1, %0";
	  case E_SImode:
	  case E_SFmode:
	    if (CSKY_VREG_P (REGNO (src)))
	      {
		if (CSKY_ISA_FEATURE(fpv2_sf))
		  return "fsts\t%1, %0";
		else if (CSKY_ISA_FEATURE(fpv3_sf))
		  return "fst.32\t%1, %0";
		else
		  gcc_unreachable ();
	      }
	    else
	      return "st.w\t%1, %0";
	  default:
	    gcc_unreachable ();
	  }
    }

  gcc_unreachable ();
}


/* Output a move of a word or less value.  Specific for ck801.  */

const char *
csky_output_ck801_move (rtx insn ATTRIBUTE_UNUSED, rtx operands[],
			machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx dst = operands[0];
  rtx src = operands[1];
  struct csky_address op1;

  if (REG_P (dst))
    {
      if (REG_P (src))
	return "mov\t%0, %1";
      else if (GET_CODE (src) == MEM)
	{
	  decompose_csky_address (XEXP (src, 0), &op1);

	  /* Generate lrw rx, [LABEL].  This happens when the compiler
	     generates constant pool references and uses lrw to get the
	     constant in memory.  */
	  if (op1.label)
	    return "lrw\t%0, %1";
	  else
	    switch (GET_MODE (src))
	      {
	      case E_HImode:
		return "ld.h\t%0, %1";
	      case E_QImode:
		return "ld.b\t%0, %1";
	      case E_SFmode:
	      case E_SImode:
		return "ld.w\t%0, %1";
	      default:
		gcc_unreachable ();
	      }
	}
      else if (GET_CODE (src) == CONST_INT)
	{
	  if (REGNO (dst) > 7)
	    return "lrw\t%0, %x1\t";
	  else if (CSKY_CONST_OK_FOR_N (INTVAL (src) + 1))
	    return "movi\t%0, %1";
	  /* Return '#' to split it.  */
	  else if (CSKY_CONST_OK_FOR_T (INTVAL (src)))
	    return "#";
	  else if (csky_shifted_imm8_constant (INTVAL (src), NULL, NULL))
	    return "#";
	  else
	    return "lrw\t%0, %x1\t";
	}
      else if (GET_CODE (src) == CONST_DOUBLE && GET_MODE (src) == SFmode)
	{
	  const REAL_VALUE_TYPE *d;
	  long l;

	  d = CONST_DOUBLE_REAL_VALUE (src);
	  REAL_VALUE_TO_TARGET_SINGLE (*d, l);
	  operands[1] = GEN_INT (l);
	  src = operands[1];

	  if (CSKY_CONST_OK_FOR_N (INTVAL (src) + 1))
	    return "movi\t%0, %1";
	  else
	    return "lrw\t%0, %x1\t";
	}
      else if (TARGET_ANCHOR && GET_CODE (src) == SYMBOL_REF)
	{
	  if (SYMBOL_REF_FUNCTION_P (src))
	    return "lrw\t%0, %1@BTEXT";
	  else
	    return "lrw\t%0, %1@BDATA";
	}
      else
	return "lrw\t%0, %1";
    }
  else if (GET_CODE (dst) == MEM)
    switch (GET_MODE (dst))
      {
      case E_HImode:
	return "st.h\t%1, %0";
      case E_QImode:
	return "st.b\t%1, %0";
      case E_SImode:
      case E_SFmode:
	return "st.w\t%1, %0";
      default:
	gcc_unreachable ();
      }

  gcc_unreachable ();
}


/* Return a sequence of instructions to perform DI or DF move.
   Since the CSKY cannot move a DI or DF in one instruction, we have
   to take care when we see overlapping source and dest registers.  */

const char *
csky_output_movedouble (rtx operands[],
			machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx dst = operands[0];
  rtx src = operands[1];

  if (REG_P (dst))
    {
      if (REG_P (src))
	{
	  int dstreg = REGNO (dst);
	  int srcreg = REGNO (src);

	  if (CSKY_HILO_REG_P (srcreg))
	    {
	      if (TARGET_BIG_ENDIAN)
		return "mfhi\t%0\n\tmflo\t%R0";
	      else
		return "mfhi\t%R0\n\tmflo\t%0";
	    }
	  else if (CSKY_HILO_REG_P (dstreg))
	    {
	      if (TARGET_BIG_ENDIAN)
		return "mthi\t%1\n\tmtlo\t%R1";
	      else
		return "mthi\t%R1\n\tmtlo\t%1";
	    }
	  else if (CSKY_VREG_P (srcreg) && CSKY_VREG_P (dstreg))
	    {
	      if (CSKY_ISA_FEATURE(fpv2_df))
		return "fmovd\t%0, %1";
	      else if (CSKY_ISA_FEATURE(fpv3_df))
		return "fmov.64\t%0, %1";
	      else
		gcc_unreachable ();
	    }
	  else if (CSKY_VREG_P (srcreg))
	    {
	      /* Since the vector registers in fpuv2_soft processors
		 like ck803f are 32 bits wide, just one insn is needed
		 to complete the move operation.  */
	      if (TARGET_SOFT_FPU)
		  return "fmfvrl\t%0, %1";
	      else if (TARGET_BIG_ENDIAN)
		{
		  if (CSKY_ISA_FEATURE(fpv2_df))
		    return "fmfvrh\t%0, %1\n\tfmfvrl\t%R0, %1";
		  else if (CSKY_ISA_FEATURE(fpv3_df))
		    return "fmfvr.64\t%R0, %0, %1";
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  if (CSKY_ISA_FEATURE(fpv2_df))
		    return "fmfvrh\t%R0, %1\n\tfmfvrl\t%0, %1";
		  else if (CSKY_ISA_FEATURE(fpv3_df))
		    return "fmfvr.64\t%0, %R0, %1";
		  else
		    gcc_unreachable ();
		}
	    }
	  else if (CSKY_VREG_P (dstreg))
	    {
	      if (TARGET_SOFT_FPU)
		return "fmtvrl\t%0, %1";
	      else if (TARGET_BIG_ENDIAN)
		{
		  if (CSKY_ISA_FEATURE(fpv2_df))
		    return "fmtvrh\t%0, %1\n\tfmtvrl\t%0, %R1";
		  else if (CSKY_ISA_FEATURE(fpv3_df))
		    return "fmtvr.64\t%0, %R1, %1";
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  if (CSKY_ISA_FEATURE(fpv2_df))
		    return "fmtvrh\t%0, %R1\n\tfmtvrl\t%0, %1";
		  else if (CSKY_ISA_FEATURE(fpv3_df))
		    return "fmtvr.64\t%0, %1, %R1";
		  else
		    gcc_unreachable ();
		}
	    }

	  /* Ensure the second source not overwritten.  */
	  if (srcreg + 1 == dstreg)
	    return "mov\t%R0, %R1\n\tmov\t%0, %1";
	  else
	    return "mov\t%0, %1\n\tmov\t%R0, %R1";
	}
      else if (GET_CODE (src) == MEM)
	{
	  rtx memexp = XEXP (src, 0);
	  int dstreg = REGNO (dst);
	  int basereg = -1;
	  struct csky_address op0;

	  decompose_csky_address (XEXP (src, 0), &op0);

	  if (GET_CODE (memexp) == LABEL_REF
	      || (GET_CODE (memexp) == CONST
		  && GET_CODE (XEXP (memexp, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (memexp, 0), 0)) == LABEL_REF))
	    return "lrw\t%0, [%1]\n\tlrw\t%R0, [%R1]";
	  else if (GET_CODE (memexp) == REG)
	    basereg = REGNO (memexp);
	  else if (GET_CODE (memexp) == PLUS)
	    {
	      if (GET_CODE (XEXP (memexp, 0)) == REG)
		basereg = REGNO (XEXP (memexp, 0));
	      else if (GET_CODE (XEXP (memexp, 1)) == REG)
		basereg = REGNO (XEXP (memexp, 1));
	      else
		gcc_unreachable ();
	    }
	  else
	    gcc_unreachable ();


	  /* When FPUV2.  */
	  if (CSKY_VREG_P (dstreg))
	    {
	      if (op0.index)
		{
		  if (CSKY_ISA_FEATURE(fpv2_df))
		    return "fldrd\t%0, %1";
		  else if (CSKY_ISA_FEATURE(fpv3_df))
		    return "fldr.64\t%0, %1";
		  else
		    gcc_unreachable ();
		}
	      else
		{
		  if (CSKY_ISA_FEATURE(fpv2_df))
		    return "fldd\t%0, %1";
		  else if (CSKY_ISA_FEATURE(fpv3_df))
		    return "fld.64\t%0, %1";
		  else
		    gcc_unreachable ();
		}
	    }
	  /* FIXME length attribute is wrong here.  */
	  if (dstreg == basereg)
	    /* Just load them in reverse order.  */
	    return "ld.w\t%R0, %R1\n\tld.w\t%0, %1";
	  else
	    return "ld.w\t%0, %1\n\tld.w\t%R0, %R1";
	}
      else if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
	{
	  split_double (src, operands + 2, operands + 3);

	  if (CSKY_CONST_OK_FOR_I (INTVAL (operands[2])))
	    output_asm_insn ("movi\t%0, %2", operands);
	  else if (CSKY_CONST_OK_FOR_Uc (INTVAL (operands[2])))
	    output_asm_insn ("bmaski\t%0, %N2", operands);
	  else if (CSKY_CONST_OK_FOR_Ub (INTVAL (operands[2])))
	    output_asm_insn ("bgeni\t%0, %P2", operands);
	  else
	    output_asm_insn ("lrw\t%0, %2", operands);

	  if (CSKY_CONST_OK_FOR_I (INTVAL (operands[3])))
	    output_asm_insn ("movi\t%R0, %3", operands);
	  else if (CSKY_CONST_OK_FOR_Uc (INTVAL (operands[3])))
	    output_asm_insn ("bmaski\t%R0, %N3", operands);

	  else if (CSKY_CONST_OK_FOR_Ub (INTVAL (operands[3])))
	    output_asm_insn ("bgeni\t%R0, %P3", operands);
	  else
	    output_asm_insn ("lrw\t%R0, %3", operands);

	  return "";
	}
      else
	gcc_unreachable ();
    }
  else if (GET_CODE (dst) == MEM && GET_CODE (src) == REG)
    {
      rtx memexp = XEXP (dst, 0);
      int srcreg = REGNO (src);
      int basereg = -1;
      struct csky_address op0;

      decompose_csky_address (XEXP (dst, 0), &op0);

      if (GET_CODE (memexp) == REG)
	basereg = REGNO (memexp);
      else if (GET_CODE (memexp) == PLUS)
	{
	  if (GET_CODE (XEXP (memexp, 0)) == REG)
	    basereg = REGNO (XEXP (memexp, 0));
	  else if (GET_CODE (XEXP (memexp, 1)) == REG)
	    basereg = REGNO (XEXP (memexp, 1));
	  else
	    gcc_unreachable ();
	}
      else
	gcc_unreachable ();

      /* When FPUV2.  */
      if (CSKY_VREG_P (srcreg))
	{
	  if (op0.index)
	    {
	      if (CSKY_ISA_FEATURE(fpv2_df))
		return "fstrd\t%1, %0";
	      else if (CSKY_ISA_FEATURE(fpv3_df))
		return "fstr.64\t%1, %0";
	      else
		gcc_unreachable ();
	    }
	  else
	    {
	      if (CSKY_ISA_FEATURE(fpv2_df))
		return "fstd\t%1, %0";
	      else if (CSKY_ISA_FEATURE(fpv3_df))
		return "fst.64\t%1, %0";
	      else
		gcc_unreachable ();
	    }
	}
      /* FIXME length attribute is wrong here.  */
      if (srcreg == basereg)
	/* Just load them in reverse order.  */
	return "st.w\t%R1, %R0\n\tst.w\t%1, %0";
      else
	return "st.w\t%1, %0\n\tst.w\t%R1, %R0";
    }
  else
    gcc_unreachable ();
}


const char *
csky_output_ck801_movedouble (rtx operands[],
			      machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx dst = operands[0];
  rtx src = operands[1];

  if (REG_P (dst))
    {
      if (REG_P (src))
	{
	  int dstreg = REGNO (dst);
	  int srcreg = REGNO (src);

	  /* Ensure the second source not overwritten.  */
	  if (srcreg + 1 == dstreg)
	    return "mov\t%R0, %R1\n\tmov\t%0, %1";
	  else
	    return "mov\t%0, %1\n\tmov\t%R0, %R1";
	}
      else if (GET_CODE (src) == MEM)
	{
	  rtx memexp = XEXP (src, 0);
	  int dstreg = REGNO (dst);
	  int basereg = -1;
	  struct csky_address op0;

	  decompose_csky_address (XEXP (src, 0), &op0);

	  if (GET_CODE (memexp) == LABEL_REF
	      || (GET_CODE (memexp) == CONST
		  && GET_CODE (XEXP (memexp, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (memexp, 0), 0)) == LABEL_REF))
	    return "lrw\t%0, [%1]\n\tlrw\t%R0, [%R1]";
	  else if (GET_CODE (memexp) == REG)
	    basereg = REGNO (memexp);
	  else if (GET_CODE (memexp) == PLUS)
	    {
	      if (GET_CODE (XEXP (memexp, 0)) == REG)
		basereg = REGNO (XEXP (memexp, 0));
	      else if (GET_CODE (XEXP (memexp, 1)) == REG)
		basereg = REGNO (XEXP (memexp, 1));
	      else
		gcc_unreachable ();
	    }
	  else
	    gcc_unreachable ();

	  /* FIXME length attribute is wrong here.  */
	  if (dstreg == basereg)
	    /* Just load them in reverse order.  */
	    return "ld.w\t%R0, %R1\n\tld.w\t%0, %1";
	  else
	    return "ld.w\t%0, %1\n\tld.w\t%R0, %R1";
	}
      else if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
	{
	  split_double (src, operands + 2, operands + 3);

	  if (REGNO (dst) <= 7
	      && CSKY_CONST_OK_FOR_N (INTVAL (operands[2]) + 1))
	    output_asm_insn ("movi\t%0, %2", operands);
	  else
	    output_asm_insn ("lrw\t%0, %2", operands);


	  if (REGNO (dst) <= 6
	      && CSKY_CONST_OK_FOR_N (INTVAL (operands[3]) + 1))
	    output_asm_insn ("movi\t%R0, %3", operands);
	  else
	    output_asm_insn ("lrw\t%R0, %3", operands);

	  return "";


	}
      else
	gcc_unreachable ();
    }
  else if (GET_CODE (dst) == MEM && GET_CODE (src) == REG)
    {
      rtx memexp = XEXP (dst, 0);
      int srcreg = REGNO (src);
      int basereg = -1;
      struct csky_address op0;

      decompose_csky_address (XEXP (dst, 0), &op0);

      if (GET_CODE (memexp) == REG)
	basereg = REGNO (memexp);
      else if (GET_CODE (memexp) == PLUS)
	{
	  if (GET_CODE (XEXP (memexp, 0)) == REG)
	    basereg = REGNO (XEXP (memexp, 0));
	  else if (GET_CODE (XEXP (memexp, 1)) == REG)
	    basereg = REGNO (XEXP (memexp, 1));
	  else
	    gcc_unreachable ();
	}
      else
	gcc_unreachable ();

      /* FIXME length attribute is wrong here.  */
      if (srcreg == basereg)
	/* Just load them in reverse order.  */
	return "st.w\t%R1, %R0\n\tst.w\t%1, %0";
      else
	return "st.w\t%1, %0\n\tst.w\t%R1, %R0";
    }
  else
    gcc_unreachable ();
}

/* Calculate the instruction's length for moving double-word data.  */

int
csky_get_movedouble_length(rtx operands[])
{
  rtx dst = operands[0];
  rtx src = operands[1];

  if (REG_P (dst))
    {
      if (REG_P (src))
	{
	  int dstreg = REGNO (dst);
	  int srcreg = REGNO (src);

	  if (CSKY_VREG_P (srcreg) && CSKY_VREG_P (dstreg))
	    return 4;
	  else
	    return 8;
	}
      else if (GET_CODE (src) == MEM)
	{
	  rtx memexp = XEXP (src, 0);
	  int dstreg = REGNO (dst);
	  struct csky_address op0;
	  decompose_csky_address (XEXP (src, 0), &op0);

	  if (GET_CODE (memexp) == LABEL_REF)
	    return 8;
	  if (CSKY_VREG_P (dstreg))
	    return 4;
	  return 8;
	}
      else if (GET_CODE (src) == CONST_INT || GET_CODE (src) == CONST_DOUBLE)
	{
	  split_double (src, operands + 2, operands + 3);
	  if (CSKY_CONST_OK_FOR_N (INTVAL (operands[2]) + 1)
	      && CSKY_CONST_OK_FOR_N (INTVAL (operands[3]) + 1)
	      && REGNO (operands[0]) < 6)
	    return 4;
	  else
	    return 8;
	}
    }
  else if (GET_CODE (dst) == MEM && GET_CODE (src) == REG)
    {
      rtx memexp = XEXP (dst, 0);
      int srcreg = REGNO (src);
      int offset = -1;
      if (CSKY_VREG_P (srcreg))
	return 4;

      if (GET_CODE (memexp) == REG)
	offset = 0;
      else if (GET_CODE (memexp) == PLUS)
	{
	  if (GET_CODE (XEXP (memexp, 0)) == REG)
	    offset = INTVAL (XEXP (memexp, 1));
	  else if (GET_CODE (XEXP (memexp, 1)) == REG)
	    offset = INTVAL (XEXP (memexp, 0));
	  else
	    gcc_unreachable ();
	}
      else
	gcc_unreachable ();

      if (srcreg <= 6 && offset <= 1020)
	return 4;
      else if ((srcreg == 7 && offset <= 1024) || (srcreg <= 7 && offset == 1024))
	return 6;
      else
	return 8;
    }
  else
    gcc_unreachable ();

  return 0;
}

/* Output float point load/store instructions for fpuv3.  */

const char *
fpuv3_output_move (rtx *operands)
{
  rtx reg, mem, addr, ops[2];
  bool isload = REG_P (operands[0]);

  const char *templ = "f%s%s.%s\t%%0, %%1";
  char buff[50];
  machine_mode mode;

  reg = operands[isload ? 0 : 1];
  mem = operands[isload ? 1 : 0];

  gcc_assert (REG_P (reg));
  gcc_assert (CSKY_VREG_P (REGNO (reg)));
  gcc_assert (MEM_P (mem));

  mode = GET_MODE (reg);
  const char *type = mode == DFmode ? "64" :
		     mode == SFmode ? "32" :
		     mode == HFmode ? "16" :
		     NULL;
  gcc_assert(type != NULL);

  addr = XEXP (mem, 0);
  struct csky_address caddr;
  decompose_csky_address (addr, &caddr);

  ops[0] = reg;
  ops[1] = mem;
  sprintf (buff, templ,
	   isload ? "ld" : "st",
	   caddr.index ? "r" : "",
	   type);
  output_asm_insn (buff, ops);

  return "";
}

/* Check if a const_double can be used by a VFP fmovi instruction.  */

int
fpuv3_const_double_rtx (rtx x)
{
  REAL_VALUE_TYPE r, m;
  r = *CONST_DOUBLE_REAL_VALUE (x);

  /* Fpuv3 doesn't support the following values.  */
  if (REAL_VALUE_ISINF (r) || REAL_VALUE_ISNAN (r) || REAL_VALUE_MINUS_ZERO (r)
      || r.cl == rvc_zero)
    return 0;

  /* Extract sign, exponent and mantissa.  */
  int exponent;
  r = real_value_abs (&r);
  exponent = REAL_EXP (&r);

  bool fail;
  unsigned HOST_WIDE_INT mantissa, mant_hi;
  unsigned HOST_WIDE_INT mask;
  int point_pos = 2 * HOST_BITS_PER_WIDE_INT - 1;
  real_ldexp (&m, &r, point_pos - exponent);
  wide_int w = real_to_integer (&m, &fail, HOST_BITS_PER_WIDE_INT * 2);
  mantissa = w.elt (0);
  mant_hi = w.elt (1);

  exponent -= 1;

  if (!IN_RANGE (exponent, -4, 11))
    return 0;

  /* If there are bits set in the low part of the mantissa, these values are
     not supported.  */
  if (mantissa != 0)
    return 0;

  /* Now, make the mantissa contain the most-significant bits, and the
     point_pos indicates the number of these bits.  */
  point_pos -= HOST_BITS_PER_WIDE_INT;
  mantissa = mant_hi;

  /* We can only allow a mantissa of 9 significant digits, top of which is always 1.  */
  mask = ((unsigned HOST_WIDE_INT)1 << (point_pos - 9)) - 1;
  if ((mantissa & mask) != 0)
    return 0;

  return 1;
}


/* Split operands for an AND expression when OPERANDS[2] is a constant.
   Note operands[0] is marked earlyclobber in this case and can be
   overwritten.  Return true if "DONE", false otherwise.  */

bool
csky_split_and (rtx *operands)
{
  HOST_WIDE_INT mask = INTVAL (operands[2]);
  rtx not_value = GEN_INT (~mask);
  int i;

  /* All zeros or all ones can be handled by a move instruction.  */
  if (mask == 0)
    {
      emit_move_insn (operands[0], const0_rtx);
      return true;
    }
  if (mask == -1)
    {
      emit_move_insn (operands[0], operands[1]);
      return true;
    }

  /* Check for constants that can be handled directly by the 32-bit andi
     instruction.  */
  if (CSKY_ISA_FEATURE (E2) && csky_arith_O_operand (operands[2], SImode))
    return false;

  /* Try to transform to andni instruction.  */
  if (CSKY_ISA_FEATURE (E2) && csky_arith_O_operand (not_value, SImode))
    {
      emit_insn (gen_cskyv2_andnsi3 (operands[0], not_value, operands[1]));
      return true;
    }

  /* If there are only one or two 0 bits in the constant, we can
     replace the operation with bclri instructions on those bits.
     Note CK801 has only the 16-bit bclri that operates on a single
     register, so we must count a move if we are post-reload.  */
  if (popcount_hwi (~mask & 0xffffffff)
      <= (reload_completed && !CSKY_ISA_FEATURE (E2) ? 1 : 2))
    {
      rtx input = operands[1];

      if (!CSKY_ISA_FEATURE (E2))
	{
	  emit_move_insn (operands[0], input);
	  input = operands[0];
	}

      for (i = 0; i < 32; i++)
	if ((mask & (1 << i)) == 0x0)
	  {
	    emit_insn (gen_bclri (operands[0], input, GEN_INT (i)));
	    input = operands[0];
	  }
      return true;
    }

  /* If the constant mask is outside the [0, 4095] range for
     constraint O, or if constraint O is not allowed (ck801),
     maybe the constant is a contiguous bit range that we can
     handle by bit extract (low bits) or shifts (high bits).  */
  for (i = (CSKY_ISA_FEATURE (E2) ? 13 : 1); i < 32; i++)
    {
      if ((((HOST_WIDE_INT) 1) << i) - 1 == mask)
	{
	  if (CSKY_ISA_FEATURE (2E3))
	    emit_insn (gen_cskyv2_extzv (operands[0], operands[1],
					 GEN_INT (i), const0_rtx));
	  else
	    {
	      rtx shift = GEN_INT (32 - i);
	      rtx reg = (reload_completed
			 ? operands[0] : gen_reg_rtx (SImode));

	      emit_insn (gen_ashlsi3 (reg, operands[1], shift));
	      emit_insn (gen_lshrsi3 (operands[0], reg, shift));
	    }
	  return true;
	}
      else if ((((HOST_WIDE_INT) 1) << i) - 1 == ~mask)
	{
	  rtx shift = GEN_INT (i);
	  rtx reg = (reload_completed
		     ? operands[0] : gen_reg_rtx (SImode));

	  emit_insn (gen_lshrsi3 (reg, operands[1], shift));
	  emit_insn (gen_ashlsi3 (operands[0], reg, shift));
	  return true;
	}
    }

  /* If the constant is a negative number, it seems better to use
     andn and copy the NOT_VALUE to a register instead of the
     original value, since the NOT_VALUE is always smaller and thus
     more likely to be representable as a small constant.
     This transformation can only be done before reload because
     it requires a temporary.  Hopefully register allocation can get
     rid of the extra move required for CK801.  */
  if (!reload_completed && INTVAL (operands[2]) < 0)
    {
      rtx reg = copy_to_mode_reg (SImode, not_value);

      if (CSKY_ISA_FEATURE (E2))
	emit_insn (gen_cskyv2_andnsi3 (operands[0], reg, operands[1]));
      else
	{
	  emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_ck801_andnsi3 (operands[0], reg, operands[0]));
	}
      return true;
    }

  /* If the above ways are all not working, move the constant
     to a register.  We can clobber operands[0] as it is
     marked earlyclobber in the insn constraints, but then we have to
     swap operands 1 and 2 to match the constraints on the 2-operand
     16-bit and instruction.  */
  if (reload_completed)
    {
      emit_move_insn (operands[0], operands[2]);
      operands[2] = operands[1];
      operands[1] = operands[0];
    }
  else
    operands[2] = copy_to_mode_reg (SImode, operands[2]);
  return false;
}

/* Split operands for an IOR expression when OPERANDS[2] is a constant.
   Note operands[0] is marked earlyclobber in this case and can be
   overwritten.  Return true if "DONE", false otherwise.  */

bool
csky_split_ior (rtx *operands)
{
  HOST_WIDE_INT mask = INTVAL (operands[2]);
  int i;

  /* All zeros or all ones can be handled by a move instruction.  */
  if (mask == 0)
    {
      emit_move_insn (operands[0], operands[1]);
      return true;
    }
  if (mask == -1)
    {
      emit_move_insn (operands[0], gen_int_mode (-1, SImode));
      return true;
    }

  /* Check for constants that can be handled directly by the 32-bit ori
     instruction.  */
  if (CSKY_ISA_FEATURE (E2) && csky_literal_I_operand (operands[2], SImode))
    return false;

  /* If there are only one or two 1 bits in the value, we can replace
     the operation with bseti instructions to set those bits.
     Note CK801 has only the 16-bit bclri that operates on a single
     register, so we must count a move if we are post-reload.  */
  if (popcount_hwi (mask & 0xffffffff)
      <= (reload_completed && !CSKY_ISA_FEATURE (E2) ? 1 : 2))
    {
      rtx input = operands[1];

      if (!CSKY_ISA_FEATURE (E2))
	{
	  emit_move_insn (operands[0], input);
	  input = operands[0];
	}

      for (i = 0; i < 32; i++)
	if (mask & (1 << i))
	  {
	    emit_insn (gen_bseti (operands[0], input, GEN_INT (i)));
	    input = operands[0];
	  }
      return true;
    }

  /* If the above ways are all not working, move the constant
     to a register.  We can clobber operands[0] as it is
     marked earlyclobber in the insn constraints, but then we have to
     swap operands 1 and 2 to match the constraints on the 2-operand
     16-bit ior instruction.  */
  if (reload_completed)
    {
      emit_move_insn (operands[0], operands[2]);
      operands[2] = operands[1];
      operands[1] = operands[0];
    }
  else
    operands[2] = copy_to_mode_reg (SImode, operands[2]);
  return false;
}


/* Split operands for an XOR expression when OPERANDS[2] is a constant.
   Note operands[0] is marked earlyclobber in this case and can be
   overwritten.  Return true if "DONE", false otherwise.  */

bool
csky_split_xor (rtx *operands)
{
  HOST_WIDE_INT mask = INTVAL (operands[2]);

  /* All zeros can be turned into move instruction.  */
  if (mask == 0)
    {
      emit_move_insn (operands[0], operands[1]);
      return true;
    }

  /* All ones can be turned into a bitwise not.  */
  if (mask == -1)
    {
      if (CSKY_ISA_FEATURE (E2))
	emit_insn (gen_cskyv2_one_cmplsi2 (operands[0], operands[1]));
      else
	{
	  emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_ck801_one_cmplsi2 (operands[0], operands[0]));
	}
      return true;
    }

  /* Check for constants that can be handled directly by the 32-bit xori
     instruction.  */
  if (CSKY_ISA_FEATURE (E2) && csky_arith_O_operand (operands[2], SImode))
    return false;

  /* If the above ways are all not working, move the constant
     to a register.  We can clobber operands[0] as it is
     marked earlyclobber in the insn constraints, but then we have to
     swap operands 1 and 2 to match the constraints on the 2-operand
     16-bit ior instruction.  */
  if (reload_completed)
    {
      emit_move_insn (operands[0], operands[2]);
      operands[2] = operands[1];
      operands[1] = operands[0];
    }
  else
    operands[2] = copy_to_mode_reg (SImode, operands[2]);
  return false;
}


/* Return true if X is an address form involving a symbol or label ref.  */

bool
csky_symbolic_address_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    case CONST:
      x = XEXP (x, 0);
      return ((GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (x, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (x, 1)) == CONST_INT);
    default:
      return 0;
    }
}


/* Emit a comparison instruction.
   Return true if an inverted comparison is generated.  */

bool
csky_emit_compare (enum rtx_code code, rtx op0, rtx op1)
{
  bool invert;
  rtx cc_reg = gen_rtx_REG (CCmode, CSKY_CC_REGNUM);

  if (GET_MODE_CLASS(GET_MODE (op0)) == MODE_FLOAT)
    return csky_emit_compare_float(code, op0, op1);

  if (GET_CODE (op1) == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (op1);

      switch (code)
	{
	case GTU:
	  /* Unsigned (GTU 0) is the same as (NE 0); everything else is
	     converted below to LEU (reversed cmphs).  */
	  if (val == 0)
	    code = NE;
	  /* Check whether (GTU A imm) can become (GEU A  imm + 1).  */
	  else if (TARGET_MINI_REGISTERS
		   ? CSKY_CONST_OK_FOR_J (val + 1)
		   : CSKY_CONST_OK_FOR_Uk (val + 1))
	    {
	      op1 = GEN_INT (val + 1);
	      code = GEU;
	    }
	  break;
	/* Check whether (LE A imm) can become (LT A imm + 1),
	   or (GT A imm) can become (GE A imm + 1).  */
	case GT:
	case LE:
	  if (TARGET_MINI_REGISTERS
	      ? CSKY_CONST_OK_FOR_J (val + 1)
	      : CSKY_CONST_OK_FOR_Uk (val + 1))
	    {
	      op1 = GEN_INT (val + 1);
	      code = code == LE ? LT : GE;
	    }
	  break;

	default:
	  break;
	}
    }

  if (CONSTANT_P (op1) && GET_CODE (op1) != CONST_INT)
    op1 = force_reg (GET_MODE (op1), op1);

  /* cmpnei: 0-31 (K immediate)
     ti: 1-32 (J immediate, 0 using btsti x,31).  */
  invert = false;
  switch (code)
    {
      /* Use inverted condition, cmpne.  */
      case EQ:
	code = NE;
	invert = true;
      /* Fall through.  */
      /* Use normal condition, cmpne.  */
      case NE:
	if (GET_CODE (op1) == CONST_INT
	    && (TARGET_MINI_REGISTERS
		? !csky_literal_K_operand (op1, SImode)
		: !csky_literal_I_operand (op1, SImode)))
	  op1 = force_reg (SImode, op1);
      break;

      /* Use inverted condition, reversed cmplt.  */
      case LE:
	code = GT;
	invert = true;
      /* Fall through.  */
      /* Use normal condition, reversed cmplt.  */
      case GT:
	if (GET_CODE (op1) == CONST_INT)
	  op1 = force_reg (SImode, op1);
      break;

      /* Use inverted condition, cmplt.  */
      case GE:
	code = LT;
	invert = true;
      /* Fall through.  */
      /* Use normal condition, cmplt.  */
      case LT:
	/* covered by btsti x,31.  */
	if (GET_CODE (op1) == CONST_INT && INTVAL (op1) != 0
	    && (TARGET_MINI_REGISTERS
		? !csky_literal_J_operand (op1, SImode)
		: !csky_literal_Uk_operand (op1, SImode)))
	  op1 = force_reg (SImode, op1);
	break;

      /* Use inverted condition, cmple.  */
      case GTU:
	/* We coped with unsigned > 0 above.  */
	gcc_assert (GET_CODE (op1) != CONST_INT || INTVAL (op1) != 0);
	code = LEU;
	invert = true;
      /* Fall through.  */
      /* Use normal condition, reversed cmphs.  */
      case LEU:
	if (GET_CODE (op1) == CONST_INT && INTVAL (op1) != 0)
	  op1 = force_reg (SImode, op1);
	break;

      /* Use inverted condition, cmphs.  */
      case LTU:
	code = GEU;
	invert = true;
      /* Fall through.  */
      /* Use normal condition, cmphs.  */
      case GEU:
	if (GET_CODE (op1) == CONST_INT && INTVAL (op1) != 0
	    && (TARGET_MINI_REGISTERS
		? !csky_literal_J_operand (op1, SImode)
		: !csky_literal_Uk_operand (op1, SImode)))
	  op1 = force_reg (SImode, op1);
      break;

    default:
      break;
    }

  emit_insn (gen_rtx_SET (cc_reg,
			  gen_rtx_fmt_ee (code, CCmode, op0, op1)));
  return invert;
}

/* Return true if push/pop can be used to save/restore all the registers
   indicated by MASK.  We currently don't attempt to handle situations where
   some of the registers could be handled by push/pop and others saved and
   restored individually.  */

static bool
csky_can_use_pushpop (unsigned int mask)
{
  int i;
  int end_reg;

  if (!TARGET_PUSHPOP)
    return false;

  if (mask == 0)
    return false;

  /* Regs 0-3, 12-14, 18-27, 29-31 cannot be in the mask.  */
  if (mask & 0xeffc700f)
    return false;

  /* Regs in the range r4-r11 must be contiguous.  */
  for (end_reg = 0, i = 11; i >= 4; i--)
    {
      if (!end_reg && (mask & (1 << i)))
	end_reg = i;
      if (end_reg && !(mask & (1 << i)))
	return false;
    }

  /* Likewise for regs in the range r16-r17.  */
  for (end_reg = 0, i = 17; i >= 16; i--)
    {
      if (!end_reg && (mask & (1 << i)))
	end_reg = i;
      if (end_reg && !(mask & (1 << i)))
	return false;
    }

  return true;
}


/* Return true if store/load multiple instructions can be used to
   save/restore at least some of the registers indicated by MASK.
   Unlike the push/pop case, this does handle partial ranges.
   Set *BR and *ER to the beginning and end (respectively) of the
   register range that can be handled.  */

static bool
csky_can_use_ldstm (int mask, int *br, int *er)
{
  int regno;
  int begin_reg = 0, end_reg = 0;
  int count = 0;

  if (!TARGET_MULTIPLE_STLD)
    return false;

  /* We'll only handle registers in the range 4-11, the contiguous range
     of caller-saved registers.  Higher-numbered registers are handled
     individually in addition to this, but we'll give up on doing ldstm
     entirely if we need to save/restore the low-numbered EH registers.  */
  if (mask & 0xf)
    return false;

  for (regno = 4; regno <= 11; regno++)
    {
      if (mask & 1 << regno)
	{
	  if (!begin_reg)
	    begin_reg = regno;
	  end_reg = regno;
	  count++;
	}
      else if (begin_reg)
	break;
    }

  if (count >= CSKY_MIN_MULTIPLE_STLD && count <= CSKY_MAX_MULTIPLE_STLD)
    {
      if (br)
	*br = begin_reg;
      if (er)
	*er = end_reg;
      return true;
    }
  return false;
}


const char *
csky_output_return_instruction (void)
{
  unsigned long func_type = get_csky_current_func_type ();

  if (CSKY_FUNCTION_IS_NAKED (func_type))
    return "";
  if (CSKY_FUNCTION_IS_INTERRUPT (func_type))
    return "ipop\n\tnir\n";
  else
    return "rts\n";
}


/* Adjust the stack pointer by OFFSET bytes.  OFFSET is negative if this
   is in the prologue, positive if in the epilogue.  This may require
   multiple instructions and/or use of CSKY_STACKADJUST_REGNUM as
   a scratch register.  Emit CFA notes as appropriate.  */
static void
expand_csky_stack_adjust (int offset)
{
  rtx set;
  rtx_insn *insn;
  int size = (offset > 0 ? offset : -offset);

  if (offset == 0)
    return;

  /* If OFFSET is too large for addi/subi, load it into
     CSKY_STACKADJUST_REGNUM and use a register add/sub instead.
     This case is not mentioned in the ABI documentation, but it is
     supported by GDB prologue analysis provided that the instruction(s)
     to initialize CSKY_STACKADJUST_REGNUM appear directly before
     the sub.  Depending on the value of OFFSET, this might be a
     lrw instruction or the "tricks" used by csky_output_inline_const to
     encode special-case integer constants.  */
  if (size > CSKY_MAX_SP_ADJUST * 2)
    {
      rtx tmp, dwarf;

      /* We should have reserved the scratch register already in
	 csky_layout_stack_frame.  */
      gcc_assert (cfun->machine->reg_size != 0
		  && (cfun->machine->reg_mask
		      & (1 << CSKY_STACKADJUST_REGNUM)));

      /* Prevent the optimizer from reordering these instructions to
	 keep GDB happy.  */
      if (!flag_sched_prolog)
	emit_insn (gen_blockage ());

      tmp = gen_rtx_REG (SImode, CSKY_STACKADJUST_REGNUM);
      emit_move_insn (tmp, GEN_INT (size));

      if (offset > 0)
	set = gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, tmp);
      else
	set = gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx, tmp);
      insn = emit_insn (set);
      RTX_FRAME_RELATED_P (insn) = 1;
      dwarf = gen_rtx_SET (stack_pointer_rtx,
			   plus_constant (Pmode, stack_pointer_rtx, offset));
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, dwarf);

      /* More make GDB happy.  */
      if (!flag_sched_prolog)
	emit_insn (gen_blockage ());
    }

  /* Use one or two addi or subi insns to adjust stack.  */
  else
    while (size)
      {
	int delta = (size > CSKY_MAX_SP_ADJUST
		     ? CSKY_MAX_SP_ADJUST : size);

	if (offset > 0)
	  set = gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (delta));
	else
	  set = gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx,
			    GEN_INT (delta));
	insn = emit_insn (set);
	RTX_FRAME_RELATED_P (insn) = 1;
	size -= delta;
      }
}


/* Generate and emit an insn that we will recognize as a push_multi.
   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  DWARF_REGS_MASK is a subset of
   MASK for registers that should be annotated for DWARF2 frame unwind
   information.  */

static rtx
emit_csky_regs_push (unsigned long mask)
{
  int num_regs = 0;
  int i, j;
  rtx par;
  rtx dwarf;
  rtx tmp;
  int dwarf_par_index;

  for (i = 0; i < CSKY_NGPR_REGS; i++)
    {
      if (mask & (1 << i))
	num_regs++;
    }

  /* The reg range for push is:r4-r11,r15-r17,r28.  */
  gcc_assert (num_regs && num_regs <= 12);

  /* For the body of the insn we are going to generate an UNSPEC in
     parallel with several USEs.  This allows the insn to be recognized
     by the push_multi pattern in the csky.md file.

     The body of the insn looks something like this:

       (parallel [
	   (set (mem:BLK (pre_modify:SI (reg:SI sp)
					(const_int:SI <num>)))
		(unspec:BLK [(reg:SI r4)] UNSPEC_PUSHPOP_MULT))
	   (use (reg:SI XX))
	   (use (reg:SI YY))
	   ...
	])

     For the frame note however, we try to be more explicit and actually
     show each register being stored into the stack frame, plus a (single)
     decrement of the stack pointer.  We do it this way in order to be
     friendly to the stack unwinding code, which only wants to see a single
     stack decrement per instruction.  The RTL we generate for the note looks
     something like this:

      (sequence [
	   (set (reg:SI sp) (plus:SI (reg:SI sp) (const_int -20)))
	   (set (mem:SI (reg:SI sp)) (reg:SI r4))
	   (set (mem:SI (plus:SI (reg:SI sp) (const_int 4))) (reg:SI XX))
	   (set (mem:SI (plus:SI (reg:SI sp) (const_int 8))) (reg:SI YY))
	   ...
	])

     FIXME:: In an ideal world the PRE_MODIFY would not exist and
     instead we'd have a parallel expression detailing all
     the stores to the various memory addresses so that debug
     information is more up-to-date. Remember however while writing
     this to take care of the constraints with the push instruction.

     Note also that this has to be taken care of for the VFP registers.

     For more see PR43399.  */

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs));
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (num_regs + 1));
  dwarf_par_index = 1;

  for (i = 0; i < CSKY_NGPR_REGS; i++)
    if (mask & (1 << i))
      {
	rtx reg = gen_rtx_REG (SImode, i);
	rtx addr = plus_constant (Pmode, stack_pointer_rtx, -4 * num_regs);
	tmp = gen_frame_mem (BLKmode,
			     gen_rtx_PRE_MODIFY (Pmode,
						 stack_pointer_rtx, addr));
	XVECEXP (par, 0, 0)
	  = gen_rtx_SET (tmp,
			 gen_rtx_UNSPEC (BLKmode,
					 gen_rtvec (1, reg),
					 UNSPEC_PUSHPOP_MULT));
	tmp = gen_rtx_SET (gen_frame_mem (SImode, stack_pointer_rtx),
			   reg);
	RTX_FRAME_RELATED_P (tmp) = 1;
	XVECEXP (dwarf, 0, dwarf_par_index++) = tmp;

	break;
      }

  for (j = 1, i++; j < num_regs; i++)
    if (mask & (1 << i))
      {
	rtx reg = gen_rtx_REG (SImode, i);
	rtx addr = plus_constant (Pmode, stack_pointer_rtx, 4 * j);
	tmp = gen_rtx_SET (gen_frame_mem (SImode, addr), reg);
	RTX_FRAME_RELATED_P (tmp) = 1;
	XVECEXP (par, 0, j) = gen_rtx_USE (VOIDmode, reg);
	XVECEXP (dwarf, 0, dwarf_par_index++) = tmp;
	j++;
      }

  par = emit_insn (par);

  tmp = gen_rtx_SET (stack_pointer_rtx,
		     plus_constant (Pmode, stack_pointer_rtx, -4 * num_regs));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;

  add_reg_note (par, REG_FRAME_RELATED_EXPR, dwarf);
  RTX_FRAME_RELATED_P (par) = 1;

  return par;
}


/* Generate and emit an insn pattern that we will recognize as a pop_multi.
   SAVED_REGS_MASK shows which registers need to be restored.

   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  */

static void
emit_csky_regs_pop (unsigned long mask)
{
  int num_regs = 0;
  int i, j;
  rtx par;

  for (i = 0; i < CSKY_NGPR_REGS; i++)
    if (mask & (1 << i))
      num_regs++;

  /* The reg range for push is:r4-r11,r15-r17,r28.  */
  gcc_assert (num_regs && num_regs <= 12);

  /* The first element is (return),
     the second element is
       (set (reg:SI 'first reg number')
	    (unspec:SI [(mem)] UNSPEC_PUSHPOP_MULT),
     the rest elements is (use (reg:SI 'rest reg number')),
     so the length should be number of register to be poped
     plus one.  */
  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs + 1));

  XVECEXP (par, 0, 0) = ret_rtx;

  for (i = 0; i < CSKY_NGPR_REGS; i++)
    if (mask & (1 << i))
      {
	rtx reg = gen_rtx_REG (SImode, i);
	rtx addr = plus_constant (Pmode, stack_pointer_rtx, 4 * num_regs);
	rtx tmp = gen_frame_mem (SImode,
				 gen_rtx_POST_MODIFY (Pmode,
						      stack_pointer_rtx, addr));
	XVECEXP (par, 0, 1)
	  = gen_rtx_SET (reg,
			 gen_rtx_UNSPEC (SImode,
					 gen_rtvec (1, tmp),
					 UNSPEC_PUSHPOP_MULT));
	break;
      }

  for (j = 2, i++; j < (num_regs + 1); i++)
    if (mask & (1 << i))
      {
	rtx reg = gen_rtx_REG (SImode, i);
	XVECEXP (par, 0, j) = gen_rtx_USE (VOIDmode, reg);
	j++;
      }

  par = emit_jump_insn (par);
}


/* Generate the function prologue.  */

void
csky_expand_prologue (void)
{
  rtx_insn *insn;
  unsigned long func_type = get_csky_current_func_type ();
  unsigned int reg_mask;
  int reg_size;

  if (CSKY_FUNCTION_IS_NAKED (func_type))
    {
      if (flag_stack_usage_info)
	current_function_static_stack_size = 0;
      return;
    }

  csky_layout_stack_frame ();
  reg_mask = cfun->machine->reg_mask;
  reg_size = cfun->machine->reg_size;

  /* Adjust stack pointer past argument overflow area.  */
  if (cfun->machine->arg_size != 0)
    {
      int offset = cfun->machine->arg_size;
      expand_csky_stack_adjust (- offset);

      /* If we have a parameter passed partially in regs and partially
	 in memory, the registers will have been stored to memory already
	 in function.cc.  So we only need to copy varargs from registers
	 to stack.  */
      if (cfun->machine->uses_anonymous_args)
	{
	  int rn = CSKY_FIRST_PARM_REGNUM + CSKY_NPARM_REGS - 1;
	  for (offset -= 4; offset >= 0; offset -= 4, rn--)
	    {
	      rtx dst = gen_frame_mem (SImode,
				       plus_constant (Pmode,
						      stack_pointer_rtx,
						      offset));
	      insn = emit_move_insn (dst, gen_rtx_REG (SImode, rn));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }

  /* Push caller-saved registers to stack.  */
  if (csky_can_use_pushpop (reg_mask))
    emit_csky_regs_push (reg_mask);
  else if (reg_size)
    {
      int sreg = -1, ereg = -1;
      bool stm_p = csky_can_use_ldstm (reg_mask, &sreg, &ereg);
      int stm_regs = stm_p ? ereg - sreg + 1 : 0;
      int stm_size = stm_regs * 4;

      /* First adjust the SP to the low end of the register save area.  */
      expand_csky_stack_adjust (- reg_size);

      /* Emit individual register saves.  Even if we are going to emit an
	 stm, we may need to save individual registers above that too.  */
      if (reg_size > stm_size)
	{
	  int offset = reg_size - 4;
	  int regno = 31;
	  for ( ; regno > ereg; regno--)
	    if (reg_mask & (1 << regno))
	      {
		rtx dst = gen_rtx_MEM (SImode,
				       plus_constant (Pmode,
						      stack_pointer_rtx,
						      offset));
		rtx insn = emit_insn (gen_movsi (dst,
						 gen_rtx_REG (SImode, regno)));
		RTX_FRAME_RELATED_P (insn) = 1;
		if (offset == stm_size)
		  break;
		offset -= 4;
	      }
	}

      /* If possible, emit a stm to do a bulk store of sequential
	 registers to the stack.  Note that it is an error in the ABI
	 documentation that it doesn't list stm as a valid prologue
	 instruction.  */
      if (stm_p)
	{
	  rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (stm_regs));
	  int regno, slot;
	  for (regno = sreg, slot = 0; regno <= ereg; regno++, slot++)
	    {
	      rtx reg = gen_rtx_REG (SImode, regno);
	      rtx addr = plus_constant (Pmode, stack_pointer_rtx, slot * 4);
	      rtx set = gen_rtx_SET (gen_frame_mem (SImode, addr), reg);
	      RTX_FRAME_RELATED_P (set) = 1;
	      XVECEXP (par, 0, slot) = set;
	    }
	  insn = emit_insn (par);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Initialize hard frame pointer, if necessary.  It points at the base
     of the register save area.  */
  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_movsi (hard_frame_pointer_rtx, stack_pointer_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Reserve stack space for locals and outgoing args.  */
  expand_csky_stack_adjust (- cfun->machine->reg_offset);

  /* Put the GOT address in reg_gb for PIC, using R13 as a scratch.
     See section 4.7.1 in  the ABI documentation,
     "Function Prologue for PIC".  */
  if (flag_pic && (reg_mask & (1 << PIC_OFFSET_TABLE_REGNUM)))
    {
      rtx l1 = gen_label_rtx ();
      rtx grs_label = gen_rtx_LABEL_REF (SImode, l1);
      rtx reg_gb = gen_rtx_REG (SImode, PIC_OFFSET_TABLE_REGNUM);
      rtx reg_temp = gen_rtx_REG (SImode, 13);

      rtx tmp0_unspec = gen_rtx_UNSPEC (Pmode,
					gen_rtvec (1, grs_label),
					UNSPEC_PIC_SYMBOL_GOTPC_GRS);
      rtx tmp1_unspec = gen_rtx_UNSPEC (Pmode,
					gen_rtvec (1, grs_label),
					UNSPEC_PIC_SYMBOL_GOTPC);

      emit_insn (gen_prologue_get_pc (tmp0_unspec));
      emit_move_insn (reg_temp, tmp1_unspec);
      emit_insn (gen_addsi3 (reg_gb, reg_gb, reg_temp));
    }

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->frame_size;

  if (!flag_sched_prolog)
    emit_insn (gen_blockage ());
}

void
csky_expand_epilogue (void)
{
  unsigned long func_type = get_csky_current_func_type ();
  unsigned int reg_mask;
  int reg_size;
  int adjust;
  rtx_insn *insn;

  if (!flag_sched_prolog)
    emit_insn (gen_blockage ());

  if (CSKY_FUNCTION_IS_NAKED (func_type))
    {
      emit_jump_insn (gen_simple_return ());
      return;
    }

  /* Get the frame information.  */
  csky_layout_stack_frame ();
  reg_mask = cfun->machine->reg_mask;
  reg_size = cfun->machine->reg_size;
  adjust = reg_size + cfun->machine->arg_size;

  /* Restore the SP to the base of the register save area.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    expand_csky_stack_adjust (cfun->machine->reg_offset);

  /* Restore the callee-saved registers.  */
  if (csky_can_use_pushpop (reg_mask)
      && cfun->machine->arg_size == 0
      && !CSKY_FUNCTION_IS_INTERRUPT (func_type)
      && !crtl->calls_eh_return)
    {
      /* Pop includes an implicit return, so we are done.  */
      emit_csky_regs_pop (reg_mask);
      return;
    }
  else if (reg_size)
    {
      int sreg = -1, ereg = -1;
      bool ldm_p = csky_can_use_ldstm (reg_mask, &sreg, &ereg);
      int ldm_regs = ldm_p ? ereg - sreg + 1 : 0;
      int ldm_size = ldm_regs * 4;

      /* Emit individual register loads.  Even if we are going to emit an
	 ldm, we may need to load individual registers above that too.  */
      if (reg_size > ldm_size)
	{
	  int offset = reg_size - 4;
	  int regno = 31;
	  for ( ; regno > ereg; regno--)
	    if (reg_mask & (1 << regno))
	      {
		rtx src = gen_frame_mem (SImode,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							offset));
		rtx reg = gen_rtx_REG (SImode, regno);
		insn = emit_move_insn (reg, src);
		RTX_FRAME_RELATED_P (insn) = 1;
		add_reg_note (insn, REG_CFA_RESTORE, reg);
		if (offset == ldm_size)
		  break;
		offset -= 4;
	      }
	}

      /* If possible, emit a ldm to do a bulk load of sequential
	 registers from the stack.  */
      if (ldm_p)
	{
	  rtx par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (ldm_regs));
	  int regno, slot;
	  for (regno = sreg, slot = 0; regno <= ereg; regno++, slot++)
	    {
	      rtx reg = gen_rtx_REG (SImode, regno);
	      rtx addr = plus_constant (Pmode, stack_pointer_rtx, slot * 4);
	      rtx set = gen_rtx_SET (reg, gen_frame_mem (SImode, addr));
	      XVECEXP (par, 0, slot) = set;
	    }
	  insn = emit_insn (par);
	  RTX_FRAME_RELATED_P (insn) = 1;
	  for (regno = sreg; regno <= ereg; regno++)
	    {
	      rtx reg = gen_rtx_REG (SImode, regno);
	      add_reg_note (insn, REG_CFA_RESTORE, reg);
	    }
	}
    }

  /* Emit the final stack pointer adjustment to deallocate the saved
     registers and incoming argument area.  */
  expand_csky_stack_adjust (adjust);

  /* Extra stack adjustment for exception handler return.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   EH_RETURN_STACKADJ_RTX));

  /* Now we can return.  */
  emit_jump_insn (gen_simple_return ());
}


static void
csky_output_function_prologue (FILE *f)
{
  unsigned long func_type = get_csky_current_func_type ();

  switch ((int) CSKY_FUNCTION_TYPE (func_type))
    {
    default:
    case CSKY_FT_NORMAL:
      break;
    case CSKY_FT_INTERRUPT:
      {
	asm_fprintf (f, "\t# Interrupt Service Routine.\n");
	asm_fprintf (f, "\tnie\n\tipush\n");
	break;
      }
    case CSKY_FT_FIQ:
      asm_fprintf (f, "\t# Fast Interrupt Service Routine.\n");
      break;
    case CSKY_FT_EXCEPTION:
      asm_fprintf (f, "\t# CSKY Exception Handler.\n");
      break;
    case CSKY_FT_NAKED:
      asm_fprintf (f, "\t# Naked Function: prologue and epilogue \
		      provided by programmer.\n");
      return;
    }

  csky_layout_stack_frame ();

  /* Generate .stack_size function-name, size for callgraph;
     the default stack size is 0.  */
  if (TARGET_STACK_SIZE && cfun->machine->frame_size > 0)
    {
      gcc_assert (current_function_decl != NULL);
      const char *func_name =
	  IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (current_function_decl));
      if (func_name[0] == '*')
	asm_fprintf (f, "\t.stack_size %s, %d\n",
		     &func_name[1], cfun->machine->frame_size);
      else
	asm_fprintf (f, "\t.stack_size %s, %d\n",
		     func_name, cfun->machine->frame_size);
    }
}


static void
csky_output_function_epilogue (FILE *file ATTRIBUTE_UNUSED)
{

}


/* Helper for csky_eh_return splitter: store the call frame exception
   handler address in lr.  */
void
csky_set_eh_return_address (rtx source, rtx scratch)
{
  HOST_WIDE_INT delta = 0;
  rtx basereg, addr;
  unsigned int reg_mask;

  csky_layout_stack_frame ();
  reg_mask = cfun->machine->reg_mask;

  if (reg_mask & (1 << CSKY_LR_REGNUM))
    {
      /* Find LR in the stack frame.  */
      int i = 0;

      if (frame_pointer_needed)
	{
	  basereg = hard_frame_pointer_rtx;
	  delta = 0;
	}
      else
	{
	  basereg = stack_pointer_rtx;
	  delta = cfun->machine->reg_offset;
	}

      /* At this point, (basereg + delta) points at the low end of
	 the reg save area.  Regs are saved sequentially from low
	 to high from this address.  */
      for (i = 0; i < CSKY_LR_REGNUM; i++)
	if (reg_mask & (1 << i))
	  delta += 4;

      if ((CSKY_TARGET_ARCH (CK801) && delta >= CSKY_LD16_MAX_OFFSET (Pmode))
	  || delta >= CSKY_LD32_MAX_OFFSET (Pmode))
	{
	  emit_insn (gen_movsi (scratch, GEN_INT (delta)));
	  emit_insn (gen_addsi3 (scratch, scratch, basereg));
	  addr = scratch;
	}
      else
	addr = plus_constant (Pmode, basereg, delta);
      emit_move_insn (gen_frame_mem (Pmode, addr), source);
    }
  else
    emit_move_insn (gen_rtx_REG (Pmode, CSKY_LR_REGNUM), source);
}

/* Return TRUE if X references a SYMBOL_REF.  */

bool
csky_symbol_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF)
    return true;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (csky_symbol_mentioned_p (XVECEXP (x, i, j)))
	      return true;
	}
      else if (fmt[i] == 'e' && csky_symbol_mentioned_p (XEXP (x, i)))
	return true;
    }
  return false;
}


/* Return TRUE if X references a LABEL_REF.  */

bool
csky_label_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return true;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (csky_label_mentioned_p (XVECEXP (x, i, j)))
	      return true;
	}
      else if (fmt[i] == 'e' && csky_label_mentioned_p (XEXP (x, i)))
	return true;
    }

  return false;
}


static bool
tls_unspec_mentioned_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      return tls_unspec_mentioned_p (XEXP (x, 0));

    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_TLS)
	return true;

    /* Fall through.  */
    default:
      return false;
    }
}


/* Implement LEGITIMATE_PIC_OPERAND_P.  */

bool
csky_legitimate_pic_operand_p (rtx x)
{
  if (tls_unspec_mentioned_p (x))
    return true;
  if (csky_symbol_mentioned_p (x) || csky_label_mentioned_p (x))
    return false;
  return true;
}

rtx
csky_legitimize_pic_address (rtx orig, rtx reg, bool gotrel_p)
{
  rtx pic_reg = gen_rtx_REG (SImode, PIC_OFFSET_TABLE_REGNUM);
  bool optimize_p = false;

  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      rtx pic_ref, address, rtx_tmp;
      rtx insn;
      rtx pic_reg = gen_rtx_REG (SImode, PIC_OFFSET_TABLE_REGNUM);
      int subregs = 0;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	  subregs = 1;
	}

      if (subregs)
	address = gen_reg_rtx (Pmode);
      else
	address = reg;

      if (GET_CODE (orig) == SYMBOL_REF && !SYMBOL_REF_LOCAL_P (orig))
	{
	  /* When gotrel_p generate sym@GOT, otherwise generate sym@PLT.  */
	  rtx_tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, orig),
				    (gotrel_p
				     ? UNSPEC_PIC_SYMBOL_GOT
				     : UNSPEC_PIC_SYMBOL_PLT));
	  optimize_p = gotrel_p;
	  if (flag_pic != 1)
	    {
	      emit_move_insn (address, rtx_tmp);
	      rtx_tmp = gen_rtx_MULT (Pmode, address, GEN_INT (1));
	    }
	  pic_ref = gen_const_mem (Pmode,
				   gen_rtx_PLUS (Pmode, pic_reg, rtx_tmp));
	}
      else
	{
	  /* bsr symbol */
	  if (flag_pic == 1 && !gotrel_p)
	    {
	      pic_ref = gen_rtx_UNSPEC (Pmode,
					gen_rtvec (1, orig),
					UNSPEC_PIC_SYMBOL_BSR);
	      return pic_ref;
	    }
	  /* grs rx, symbol */
	  else if (flag_pic == 1 && (GET_CODE (orig) == SYMBOL_REF)
		   && SYMBOL_REF_FUNCTION_P (orig))
	    {
	      pic_ref = gen_rtx_UNSPEC (Pmode,
					gen_rtvec (1, orig),
					UNSPEC_PIC_SYMBOL_GRS);
	      return pic_ref;
	    }
	  /* lrw rx, symbol@GOTOFF; add rx, rx, gb */
	  else
	    {
	      rtx_tmp = gen_rtx_UNSPEC (Pmode,
					gen_rtvec (1, orig),
					UNSPEC_PIC_SYMBOL_GOTOFF);
	      emit_move_insn (address, rtx_tmp);
	      pic_ref = gen_rtx_PLUS (Pmode, address, pic_reg);
	      optimize_p = true;
	    }
	}

      insn = emit_move_insn (reg, pic_ref);
      /* Put a REG_EQUAL note on this insn,
	 so that it can be optimized by loop.  */
      if (optimize_p)
	set_unique_reg_note (insn, REG_EQUAL, orig);

      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 1) == pic_reg)
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);

      base = csky_legitimize_pic_address (XEXP (XEXP (orig, 0), 0),
					  reg, gotrel_p);
      offset = csky_legitimize_pic_address (XEXP (XEXP (orig, 0), 1),
					    base == reg ? 0 : reg, gotrel_p);

      if (GET_CODE (offset) == CONST_INT)
	return plus_constant (Pmode, base, INTVAL (offset));

      return gen_rtx_PLUS (Pmode, base, offset);
    }

  return orig;
}


/* Functions to output assembly code for a function call.  */

char *
csky_output_call (rtx *operands, int index)
{
  static char buffer[20];
  rtx addr = operands[index];

  if (REG_P (addr))
    sprintf (buffer, "jsr\t%%%d", index);
  else if (flag_pic && (GET_CODE (addr) == UNSPEC))
    sprintf (buffer, "bsr\t%%%d", index);
  else
    sprintf (buffer, "jbsr\t%%%d", index);

  return buffer;
}


/* Worker function for TARGET_ASM_TRAMPOLINE_TEMPLATE.
   Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.
   Note that STATIC_CHAIN_REGNUM is t1 (aka r12) on ck801 and
   t1 (r13) otherwise.  */

static void
csky_asm_trampoline_template (FILE *f)
{
  if (CSKY_ISA_FEATURE (2E3))
    {
      fprintf (f, "\tlrw\t%s, [.Lstatic_chain]\n",
	       reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (f, "\tjmpi\t[.Lfunc_address]\n");
      /* 2 32-bit insns = 8 bytes.  */
    }
  else if (CSKY_TARGET_ARCH (CK801))
    {
      /* It's hard to provide general support for trampolines on this
	 core.  We need a register other than the one holding the
	 static chain (r13) to hold the function pointer for the
	 indirect jump to it.  But ck801 has such a limited register set
	 there is no other call-clobbered scratch register available -- in
	 particular, this core does not have r12, which we use for the
	 ck802 case below.  If we use a callee-saved register like r4,
	 saving the old value on the stack screws up the stack frame
	 if there are overflow arguments pushed on the stack
	 by the caller.  In theory we could test for that and handle
	 limited cases with parameters that all fit in r0-r3 with no
	 stack overflow, but punt for now.  */
      sorry ("Nested function trampolines not supported on CK801.");
    }
  else
    {
      fprintf (f, "\tlrw\t%s, [.Lfunc_address]\n",
	       reg_names[CSKY_T1_REGNUM]);
      fprintf (f, "\tlrw\t%s, [.Lstatic_chain]\n",
	       reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (f, "\tjmp\t%s\n",
	       reg_names[CSKY_T1_REGNUM]);
      /* To align constant pool on a word boundary.  */
      fprintf (f, "\t.align 2\n");
      /* 2 32-bit lrw insns + 16-bit jump + 16-bit pad = 12 bytes.  */
    }

  fprintf (f, ".Lstatic_chain:\n");
  fprintf (f, "\t.long 0\n");
  fprintf (f, ".Lfunc_address:\n");
  fprintf (f, "\t.long 0\n");
  /* 2 words of constant pool = 8 bytes.  */
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
csky_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem, a_tramp;
  int pool = TRAMPOLINE_SIZE - 8;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, pool);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, pool + 4);
  emit_move_insn (mem, fnaddr);

  a_tramp = XEXP (m_tramp, 0);
  maybe_emit_call_builtin___clear_cache (a_tramp,
					 plus_constant (Pmode,
							a_tramp,
							TRAMPOLINE_SIZE));
}


/* Emit a comparison insn for float values.
   Return true if the comparison is inverted.  */

bool
csky_emit_compare_float (enum rtx_code code, rtx op0, rtx op1)
{
  rtx cc_reg = gen_rtx_REG (CCmode, CSKY_CC_REGNUM);
  bool invert;
  machine_mode mode = GET_MODE (op1);

  if (op1 != CONST0_RTX (mode))
    op1 = force_reg (mode, op1);

  invert = false;

  switch (code)
    {
    case EQ:
      code = NE;
      invert = true;
      break;
    case GT:
    case LT:
    case LE:
      if (op1 == CONST0_RTX (mode) && TARGET_SUPPORT_FPV2)
	op1 = force_reg (mode, op1);
      break;
    case ORDERED:
      code = UNORDERED;
      invert = true;
      break;

    default:
      break;
    }

  emit_insn (gen_rtx_SET (cc_reg, gen_rtx_fmt_ee (code, CCmode, op0, op1)));

  return invert;
}

/* Support for the Q or W memory constraint.  Returns true if OP is a MEM
   RTX with an address consisting of base + index or base + displacement.  */

bool
csky_valid_mem_constraint_operand (rtx op, const char *constraint)
{
  struct csky_address addr;

  if (GET_CODE (op) != MEM)
    return false;

  if (!decompose_csky_address (XEXP (op, 0), &addr))
    return false;

  /* Verify base register. */
  if (!is_csky_address_register_rtx_p (addr.base, 0))
    return false;

  /* Verify index operand. */
  if (addr.index && (constraint[0] == 'Q' || constraint[0] == 'W'))
    {
      if (!is_csky_address_register_rtx_p (addr.index, 0))
	return false;

      if (addr.scale == 1 || addr.scale == 2 || addr.scale == 4
	  || addr.scale == 8)
	return true;

      return false;
    }
  /* Verify disp operand.  */
  else if (addr.disp && constraint[0] == 'Q')
    {
      rtx disp = addr.disp;

      if (!CONST_INT_P (disp))
	return false;

      if (((unsigned) INTVAL (disp) % 4) == 0
	  && (unsigned) INTVAL (disp) <= (unsigned) 1020)
	return true;

       return false;
    }
  else if (constraint[0] == 'Q')
  /* Single reg is valid for 'Q'.  */
    return true;

  return false;
}


/* Returns the (interrupt) function type of the current
   function, or CSKY_FT_UNKNOWN if the type cannot be determined.  */

static unsigned long
csky_isr_value (tree argument)
{
  const isr_attribute_entry *ptr;
  const char *arg;

  /* No argument - default to IRQ.  */
  if (argument == NULL_TREE)
    return CSKY_FT_ISR;

  /* Get the value of the argument.  */
  if (TREE_VALUE (argument) == NULL_TREE
      || TREE_CODE (TREE_VALUE (argument)) != STRING_CST)
    return CSKY_FT_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (argument));

  /* Check it against the list of known arguments.  */
  for (ptr = isr_attribute_map; ptr->arg != NULL; ptr++)
    if (strcmp (arg, ptr->arg) == 0)
      return ptr->return_value;

  /* An unrecognized interrupt type.  */
  return CSKY_FT_UNKNOWN;
}

/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */

static tree
csky_handle_fndecl_attribute (tree *node, tree name, tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "interrupt" or "isr" attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
csky_handle_isr_attribute (tree *node, tree name, tree args, int flags,
			   bool *no_add_attrs)
{

  if (!TARGET_ISTACK)
    {
      warning (OPT_Wattributes, "%qE attribute ignored without %<-mistack%>",
	       name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) != FUNCTION_DECL)
	{
	  warning (OPT_Wattributes, "%qE attribute only applies to functions",
		   name);
	  *no_add_attrs = true;
	}
    }
  else
    {
      if (FUNC_OR_METHOD_TYPE_P (*node))
	{
	  if (csky_isr_value (args) == CSKY_FT_UNKNOWN)
	    {
	      warning (OPT_Wattributes, "%qE attribute ignored", name);
	      *no_add_attrs = true;
	    }
	}
      else if (TREE_CODE (*node) == POINTER_TYPE
	       && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (*node))
	       && csky_isr_value (args) != CSKY_FT_UNKNOWN)
	{
	  *node = build_variant_type_copy (*node);
	  TREE_TYPE (*node) = build_type_attribute_variant (TREE_TYPE (*node),
	    tree_cons (name, args, TYPE_ATTRIBUTES (TREE_TYPE (*node))));
	  *no_add_attrs = true;
	}
      else if (flags & ((int)ATTR_FLAG_DECL_NEXT
			| (int)ATTR_FLAG_FUNCTION_NEXT
			| (int)ATTR_FLAG_ARRAY_NEXT))
	{
	  *no_add_attrs = true;
	  return tree_cons (name, args, NULL_TREE);
	}
      else
	warning (OPT_Wattributes, "%qE attribute ignored", name);
    }
  return NULL_TREE;
}


/* Implement TARGET_REGISTER_MOVE_COST: compute extra cost of moving data
   between one register class and another.  */

int
csky_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			 reg_class_t from, reg_class_t to)
{
#define GR_REG_CLASS_P(CLASS) \
  ((CLASS) == GENERAL_REGS || (CLASS) == MINI_REGS || (CLASS) == SP_REGS \
   || (CLASS) == LOW_REGS)

#define HILO_REG_CLASS_P(CLASS) \
  ((CLASS) == HILO_REGS)

#define V_REG_CLASS_P(CLASS) \
  ((CLASS) == V_REGS)

  if (V_REG_CLASS_P (from) && V_REG_CLASS_P (to))
    return 2;

  if ((V_REG_CLASS_P (from) && GR_REG_CLASS_P (to))
      || (GR_REG_CLASS_P (from) && V_REG_CLASS_P (to)))
    return 6;

  if ((HILO_REG_CLASS_P (from) && GR_REG_CLASS_P (to))
      || (GR_REG_CLASS_P (from) && HILO_REG_CLASS_P (to)))
    return 16;

  if (HILO_REG_CLASS_P (from) && HILO_REG_CLASS_P (to))
    return 32;

  if ((HILO_REG_CLASS_P (from) && V_REG_CLASS_P (to))
      || (V_REG_CLASS_P (from) && HILO_REG_CLASS_P (to)))
    return 64;

  return 2;
}


/* Implement TARGET_MEMORY_MOVE_COST: compute the cost of moving data
   between registers and memory.  */

int
csky_memory_move_cost (machine_mode mode, reg_class_t rclass,
		       bool in)
{
  return (4 + memory_move_secondary_cost (mode, rclass, in));
}


/* TARGET_RTX_COSTS helper for ck801/ck802.  */

static bool
ck802_ck801_rtx_costs (rtx x, int code, int outer_code, int *total,
		       bool speed)
{
  machine_mode mode = GET_MODE (x);
  switch (code)
    {
      /* Accessing memory costs quite a lot for first word;  */
    case MEM:
      *total = COSTS_N_INSNS (1 + CSKY_NUM_REGS (mode));
      return false;
    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = 100;
      return true;

    case ROTATE:
    case ROTATERT:
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
      if (speed)
	*total = 2;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case MINUS:
    case PLUS:
      *total = COSTS_N_INSNS (CSKY_NUM_REGS (mode));
      return false;

    case AND:
      {
	enum rtx_code subcode = GET_CODE (XEXP (x, 1));

	/* If subcode is "not", we'll try to combine it into e.g. "andn"
	   instruction, so give AND itself zero cost. */
	if (subcode == NOT)
	  {
	    *total = 0;
	    return false;
	  }
      }
      /* Fall through.  */
    case XOR:
    case IOR:
      *total = COSTS_N_INSNS (CSKY_NUM_REGS (mode));
      return false;

    case MULT:
      /* FIXME:	 is ixw supported on ck801/ck802?  */
      /* We can use "ix.h/w" insn to replace multiply by 2 or 4.
	 "ix.h/w" is a 32-bit insn, so let its cost be a little less than
	 "mult" insn.  */
      if (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)))
	{
	  unsigned HOST_WIDE_INT m
	    = (unsigned HOST_WIDE_INT) (INTVAL (XEXP (x, 1)));
	  if ((m == 2 || m == 4) && outer_code == PLUS)
	    {
	      *total = 2;
	      return true;
	    }
	  else
	    {
	      /* Because mult is relatively slower than other operations,
		 we try to use other insns when optimizing for speed.
		 When optimizing for size, give it lower cost.  */
	      if (speed)
		{
		  *total = COSTS_N_INSNS (10 * CSKY_NUM_REGS (mode));
		  return true;
		}
	      int cycle = 0;
	      while (m)
		{
		  m >>= 2;
		  cycle++;
		}
	      *total = COSTS_N_INSNS (1) + cycle;
	      return false;
	    }
	}
      if (!speed)
	*total = COSTS_N_INSNS (1);
      return false;

    case NEG:
      /* Usually, we use subtract from 0 to substitute for neg, and
	 it costs 1 extra insn to move 0 to a register.  */
      *total = COSTS_N_INSNS (2 * CSKY_NUM_REGS (mode));
      return false;

    case NOT:
      *total = COSTS_N_INSNS (CSKY_NUM_REGS (mode));
      return false;

    case COMPARE:
      *total = COSTS_N_INSNS (1);
      return false;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      *total = COSTS_N_INSNS (CSKY_NUM_REGS (mode));
      return false;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      if (REG_P (XEXP (x, 0))
	  && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (x, 2))
	  && INTVAL (XEXP (x, 1)) == 8
	  && INTVAL (XEXP (x, 2)) % 8 == 0)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      *total = COSTS_N_INSNS (CSKY_NUM_REGS (mode));
      return false;

    case CONST_INT:
      {
	unsigned HOST_WIDE_INT t = (unsigned HOST_WIDE_INT) (INTVAL (x));

	if (outer_code == COMPARE)
	  {
	    if (t < 0x10000)
	      *total = 0;
	    else
	      *total = COSTS_N_INSNS (2);
	  }
	else if (outer_code == AND || outer_code == IOR || outer_code == XOR)
	  {
	    /* "andi,xori,ori" are 32-bit insns, so let it cost a
	       little more.  */
	    if (t < 0x1000)
	      {
		/* Try replacing "andi" by "sextb/h", so let it cost more.  */
		if (outer_code == AND && (t == 0xff || t == 0xffff))
		  {
		    *total = 8;
		    return true;
		  }
		*total = 2;
	      }
	    else if (t < 0x10000)
	      *total = COSTS_N_INSNS (1);
	    else
	      *total = COSTS_N_INSNS (2);
	  }
	else if (outer_code == PLUS || outer_code == MINUS)
	  {
	    /* "addi/subi rx,ry,imm", if imm<9, it is more often a
	       16-bit insn.  If imm>=9, use "movi" insn; it's probably
	       less than "addi/subi". */
	    if (t < 9)
	      *total = 0;
	    else if (t < 0x1000)
	      *total = 2;
	    else if (t < 0x10000)
	      *total = COSTS_N_INSNS (1);
	    else
	      *total = COSTS_N_INSNS (2);
	  }
	else if (outer_code == ROTATE || outer_code == ROTATERT
		 || outer_code == LSHIFTRT || outer_code == ASHIFTRT
		 || outer_code == ASHIFT)
	  {
	    if (t < 32)
	      *total = 0;
	    else
	      *total = COSTS_N_INSNS (2);
	  }
	else
	  {
	    if (t < 0x10000)
	      if (outer_code == SET && t < 256)
		*total = 0;
	      else
		*total = COSTS_N_INSNS (1);
	    else
	      *total = COSTS_N_INSNS (2);
	  }
      }
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (3);
      return true;
    default:
      return false;
    }
}


/* TARGET_RTX_COSTS helper for ck803.  */

static bool
ck803_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
		 int *total, bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case SET:
      if (MEM_P (XEXP (x, 1)))
	{
	  struct csky_address op1;
	  bool address_valid
	    = decompose_csky_address (XEXP (XEXP (x, 1), 0), &op1);
	  if (op1.index)
	    {
	      *total = COSTS_N_INSNS (3);
	      return true;
	    }
	  else if (address_valid)
	    {
	      *total = COSTS_N_INSNS (1);
	      return true;
	    }
	}
      if (REG_P (XEXP (x, 0)) && (GET_CODE (XEXP (x, 1)) == PLUS))
       {
	 rtx sub_exp = XEXP (x, 1);
	 if (REG_P (XEXP (sub_exp, 0)) && REG_P (XEXP (sub_exp, 1)))
	   {
	     *total = COSTS_N_INSNS (1);
	     return true;
	   }
       }
      return false;
    case MULT:
      if (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)))
	{
	  HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
	  if (val % 2 == 0 && val < 0xffffffff && val > 0)
	    {
	      *total = COSTS_N_INSNS (1);
	      return true;
	    }
	}
      return false;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (3);
      return true;
    default:
      return false;
    }
}

/* TARGET_RTX_COSTS helper for ck807/ck810 arches.  */

static bool
ck807_ck810_rtx_costs (rtx x, int code,
		       int outer_code ATTRIBUTE_UNUSED,
		       int *total, bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case MULT:
      if (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)))
	{
	  HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
	  if (val % 2 == 0 && val < 0xffffffff && val > 0)
	    {
	      *total = COSTS_N_INSNS (1);
	      return true;
	    }
	}
      return false;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (3);
      return true;
    default:
      return false;
    }
}

/* TARGET_RTX_COSTS helper for ck860 arches.  */

static bool
ck860_rtx_costs (rtx x, int code, machine_mode mode,
		 int outer_code ATTRIBUTE_UNUSED,
		 int *total, bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case PLUS:
      /* The costs of mula is 1 more than mult.  */
      if (GET_CODE (XEXP (x, 0)) == MULT && REG_P (XEXP (x, 1)) && speed)
	{
	  rtx mul_op0 = XEXP (XEXP (x, 0), 0);
	  rtx mul_op1 = XEXP (XEXP (x, 0), 1);
	  if (REG_P (mul_op0) && REG_P (mul_op1))
	    {
	      *total = COSTS_N_INSNS (1);
	      *total += rtx_cost (XEXP (x, 0), mode,
				  (enum rtx_code) code, 0, speed);
	      return true;
	    }
	}
      return false;
    case MULT:
      if (REG_P (XEXP (x, 0)) && CONST_INT_P (XEXP (x, 1)))
	{
	  HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
	  if (val % 2 == 0 && val < 0xffffffff && val > 0)
	    {
	      *total = COSTS_N_INSNS (1);
	      return true;
	    }
	}
      return false;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = COSTS_N_INSNS (3);
      return true;
    default:
      return false;
    }
}


/* Implement TARGET_RTX_COSTS, to compute a (partial) cost for rtx X.
   Return true if the complete cost has been computed, and false if
   subexpressions should be scanned.  In either case, *TOTAL contains
   the cost result.  */

static bool
csky_rtx_costs (rtx x, machine_mode mode ATTRIBUTE_UNUSED, int outer_code,
		int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  int code = GET_CODE (x);

  if (CSKY_TARGET_ARCH (CK802) || CSKY_TARGET_ARCH (CK801))
    return ck802_ck801_rtx_costs (x, code, outer_code, total, speed);
  else if (CSKY_TARGET_ARCH (CK803))
    return ck803_rtx_costs (x, code, outer_code, total, speed);
  else if (CSKY_TARGET_ARCH (CK807) || CSKY_TARGET_ARCH (CK810))
    return ck807_ck810_rtx_costs (x, code, outer_code, total, speed);
  else if (CSKY_TARGET_ARCH (CK860))
    return ck860_rtx_costs (x, code, mode, outer_code, total, speed);
  else
    gcc_unreachable ();
}

/* Emit assembly code for CASESI.  This is only used on CK801 and CK802
   when optimizing for size, and uses helper functions in libgcc instead
   of doing the control transfer inline.  */

const char *
csky_output_casesi (rtx *operands)
{
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[0])));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  switch (GET_MODE (diff_vec))
    {
    case E_QImode:
      return (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned
	      ? "jbsr\t___gnu_csky_case_uqi"
	      : "jbsr\t___gnu_csky_case_sqi");
    case E_HImode:
      return (ADDR_DIFF_VEC_FLAGS (diff_vec).offset_unsigned
	      ? "jbsr\t___gnu_csky_case_uhi"
	      : "jbsr\t___gnu_csky_case_shi");
    case E_SImode:
      return "jbsr\t___gnu_csky_case_si";
    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_SCHED_ISSUE_RATE.  Lookup the issue rate in the
   per-core tuning structs.  */
static int
csky_sched_issue_rate (void)
{
  if (CSKY_TARGET_ARCH (CK810))
    return 2;
  else
    return 1;
}


/* This function implements the target macro TARGET_SCHED_ADJUST_COST.
   It corrects the value of COST based on the relationship between
   INSN and DEP through the dependence DEP_TYPE.  It returns the new
   value.  */

static int
csky_sched_adjust_cost (rtx_insn *insn,
			int dep_type,
			rtx_insn *dep,
			int cost,
			unsigned int dw ATTRIBUTE_UNUSED)
{
  if (dep_type == REG_DEP_ANTI || dep_type == REG_DEP_OUTPUT)
    return 0;
  /* The REG_DEP_TRUE situation.  */
  else if (recog_memoized (insn) >= 0 && recog_memoized (dep) >= 0)
    {
      enum attr_type insn_type = get_attr_type (insn);
      if (CSKY_TARGET_ARCH (CK803))
	{
	  /* The ld or st's base reg depends on the pre insn,
	     it will delay 1 cycle.  */
	  if (insn_type == TYPE_LOAD || insn_type == TYPE_STORE)
	    {
	      rtx pattern = PATTERN (insn);

	      gcc_assert (GET_CODE (pattern) == SET);
	      rtx addr = (insn_type == TYPE_LOAD
			  ? SET_SRC (pattern) : SET_DEST (pattern));

	      enum rtx_code code = GET_CODE (addr);
	      if (code == ZERO_EXTEND || code == SIGN_EXTEND)
		addr = XEXP (addr, 0);
	      gcc_assert (GET_CODE (addr) == MEM);

	      rtx base =  XEXP (addr, 0);
	      rtx reg = NULL_RTX;
	      if (REG_P (base))
		reg = base;
	      if (GET_CODE (base) == PLUS
		  && GET_CODE (XEXP (base, 0)) == REG)
		reg = XEXP (base, 0);
	      if ((reg != NULL_RTX) && reg_set_p (reg, PATTERN (dep)))
		return 2;
	    }
	}
      else if (CSKY_TARGET_ARCH (CK802))
	{
	  if ((insn_type == TYPE_CALL_JSR || insn_type == TYPE_BRANCH_JMP)
	      && get_attr_type (dep) != TYPE_LOAD)
	    return 1;

	  if (insn_type == TYPE_LOAD || insn_type == TYPE_STORE)
	    {
	      rtx pattern = PATTERN (insn);

	      gcc_assert (GET_CODE (pattern) == SET);

	      rtx addr = (insn_type == TYPE_LOAD
			  ? SET_SRC (pattern) : SET_DEST (pattern));

	      enum rtx_code code = GET_CODE (addr);
	      if (code == ZERO_EXTEND || code == SIGN_EXTEND)
		addr = XEXP (addr, 0);
	      gcc_assert (GET_CODE (addr) == MEM);

	      rtx base =  XEXP (addr, 0);
	      rtx reg = NULL_RTX;
	      if (REG_P (base))
		reg = base;
	      if (GET_CODE (base) == PLUS
		  && GET_CODE (XEXP (base, 0)) == REG)
		reg = XEXP (base, 0);
	      if ((reg != NULL_RTX) && reg_set_p (reg, PATTERN (dep))
		  && get_attr_type (dep) != TYPE_LOAD)
		return 1;

	      if (insn_type == TYPE_STORE
		  && reg_referenced_p (SET_SRC (pattern), PATTERN (dep)))
		return 1;
	    }
	}
    }
  return cost;
}

static bool
csky_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */
  return lookup_attribute ("naked", DECL_ATTRIBUTES (decl)) == NULL_TREE;
}


/* Implement TARGET_RETURN_IN_MEMORY to decide whether TYPE should be
   returned in memory (true) or in a register (false).
   FNTYPE is the type of the function making the call.  */

static bool
csky_return_in_memory (const_tree type,
		       const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > 2 * UNITS_PER_WORD);
}


/* Implement TARGET_DWARF_REGISTER_SPAN.
   Dwarf models VFP registers as  64-bit or 128-bit registers default.
   GCC models tham as 32-bit registers, so we need to describe this to
   the DWARF generation code.  Other registers can use the default.  */

static rtx
csky_dwarf_register_span (rtx rtl)
{
  machine_mode mode;
  unsigned regno;
  rtx parts[16];
  int nregs;
  int i;

  regno = REGNO (rtl);
  if (!CSKY_VREG_P (regno))
    return NULL_RTX;

  if (CSKY_VREG_HI_P (regno))
    regno += 16;

  mode = GET_MODE (rtl);
  if (GET_MODE_SIZE (mode) < 8)
    return NULL_RTX;


  if (TARGET_SINGLE_FPU)
    {
      nregs = GET_MODE_SIZE (mode) / 4;
      for (i = 0; i < nregs; i += 2)
      if (TARGET_BIG_ENDIAN)
	{
	  parts[i] = gen_rtx_REG (SImode, regno + i + 1);
	  parts[i + 1] = gen_rtx_REG (SImode, regno + i);
	}
      else
	{
	  parts[i] = gen_rtx_REG (SImode, regno + i);
	  parts[i + 1] = gen_rtx_REG (SImode, regno + i + 1);
	}
    }
  else
    {
      /* FIXME: dwarf2 considers all general registers to be the same
	 as the CPU bit width. Transform the 64-bit FPU registers to
	 32 bits here, and we will modify the unwind processing to
	 fit CSKY architecture later.  */
      nregs = GET_MODE_SIZE (mode) / 4;
      for (i = 0; i < nregs; i += 2)
	if (TARGET_BIG_ENDIAN)
	  {
	    parts[i] = gen_rtx_REG (SImode, regno + i - 16);
	    parts[i + 1] = gen_rtx_REG (SImode, regno + i);
	  }
	else
	  {
	    parts[i] = gen_rtx_REG (SImode, regno + i);
	    parts[i + 1] = gen_rtx_REG (SImode, regno + i - 16);
	  }
    }

  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nregs , parts));
}

/* Implement TARGET_INIT_LIBFUNCS.  */

static void
csky_init_libfuncs (void)
{
  if (TARGET_CSKY_LINUX)
    init_sync_libfuncs (UNITS_PER_WORD);
  if (!TARGET_LIBCCRT)
    return;

  #define CSKY_GCC_SYM(sym) "__csky_ccrt_" # sym

  /* int */

  /* Arithmetic functions */
  set_optab_libfunc (ashl_optab,    DImode, CSKY_GCC_SYM (ashldi3));
  set_optab_libfunc (ashr_optab,    DImode, CSKY_GCC_SYM (ashrdi3));
  set_optab_libfunc (sdiv_optab,    SImode, CSKY_GCC_SYM (divsi3));
  set_optab_libfunc (sdiv_optab,    DImode, CSKY_GCC_SYM (divdi3));
  set_optab_libfunc (lshr_optab,    DImode, CSKY_GCC_SYM (lshrdi3));
  set_optab_libfunc (smod_optab,    SImode, CSKY_GCC_SYM (modsi3));
  set_optab_libfunc (smod_optab,    DImode, CSKY_GCC_SYM (moddi3));
  set_optab_libfunc (smul_optab,    DImode, CSKY_GCC_SYM (muldi3));
  set_optab_libfunc (neg_optab,	    DImode, CSKY_GCC_SYM (negdi2));
  set_optab_libfunc (udiv_optab,    SImode, CSKY_GCC_SYM (udivsi3));
  set_optab_libfunc (udiv_optab,    DImode, CSKY_GCC_SYM (udivdi3));
  set_optab_libfunc (udivmod_optab, DImode, CSKY_GCC_SYM (udivmoddi4));
  set_optab_libfunc (umod_optab,    SImode, CSKY_GCC_SYM (umodsi3));
  set_optab_libfunc (umod_optab,    DImode, CSKY_GCC_SYM (umoddi3));

  /* Comparison functions */
  set_optab_libfunc (cmp_optab,	    DImode, CSKY_GCC_SYM (cmpdi2));
  set_optab_libfunc (ucmp_optab,    DImode, CSKY_GCC_SYM (ucmpdi2));

  /* Trapping arithmetic functions */
  set_optab_libfunc (absv_optab,    SImode, CSKY_GCC_SYM (absvsi2));
  set_optab_libfunc (absv_optab,    DImode, CSKY_GCC_SYM (absvdi2));
  set_optab_libfunc (addv_optab,    SImode, CSKY_GCC_SYM (addvsi3));
  set_optab_libfunc (addv_optab,    DImode, CSKY_GCC_SYM (addvdi3));
  set_optab_libfunc (smulv_optab,   SImode, CSKY_GCC_SYM (mulvsi3));
  set_optab_libfunc (smulv_optab,   DImode, CSKY_GCC_SYM (mulvdi3));
  set_optab_libfunc (negv_optab,    SImode, CSKY_GCC_SYM (negvsi2));
  set_optab_libfunc (negv_optab,    DImode, CSKY_GCC_SYM (negvdi2));
  set_optab_libfunc (subv_optab,    SImode, CSKY_GCC_SYM (subvsi3));
  set_optab_libfunc (subv_optab,    DImode, CSKY_GCC_SYM (subvdi3));

  /* Bit operations */
  set_optab_libfunc (clz_optab,	    SImode, CSKY_GCC_SYM (clzsi2));
  set_optab_libfunc (clz_optab,	    DImode, CSKY_GCC_SYM (clzdi2));
  set_optab_libfunc (ctz_optab,	    SImode, CSKY_GCC_SYM (ctzsi2));
  set_optab_libfunc (ctz_optab,	    DImode, CSKY_GCC_SYM (ctzdi2));
  set_optab_libfunc (ffs_optab,	    DImode, CSKY_GCC_SYM (ffsdi2));
  set_optab_libfunc (parity_optab,  SImode, CSKY_GCC_SYM (paritysi2));
  set_optab_libfunc (parity_optab,  DImode, CSKY_GCC_SYM (paritydi2));
  set_optab_libfunc (popcount_optab,SImode, CSKY_GCC_SYM (popcountsi2));
  set_optab_libfunc (popcount_optab,DImode, CSKY_GCC_SYM (popcountdi2));
  set_optab_libfunc (bswap_optab,   SImode, CSKY_GCC_SYM (bswapsi2));
  set_optab_libfunc (bswap_optab,   DImode, CSKY_GCC_SYM (bswapdi2));

  /* float */

  /* Arithmetic functions */
  set_optab_libfunc (add_optab,	    SFmode, CSKY_GCC_SYM (addsf3));
  set_optab_libfunc (add_optab,	    DFmode, CSKY_GCC_SYM (adddf3));
  set_optab_libfunc (sub_optab,	    SFmode, CSKY_GCC_SYM (subsf3));
  set_optab_libfunc (sub_optab,	    DFmode, CSKY_GCC_SYM (subdf3));
  set_optab_libfunc (smul_optab,    SFmode, CSKY_GCC_SYM (mulsf3));
  set_optab_libfunc (smul_optab,    DFmode, CSKY_GCC_SYM (muldf3));
  set_optab_libfunc (sdiv_optab,    SFmode, CSKY_GCC_SYM (divsf3));
  set_optab_libfunc (sdiv_optab,    DFmode, CSKY_GCC_SYM (divdf3));
  set_optab_libfunc (neg_optab,	    SFmode, CSKY_GCC_SYM (negsf2));
  set_optab_libfunc (neg_optab,	    DFmode, CSKY_GCC_SYM (negdf2));

  /* Conversion functions */
  set_conv_libfunc (sext_optab,	   DFmode, SFmode, CSKY_GCC_SYM (extendsfdf2));
  set_conv_libfunc (trunc_optab,   SFmode, DFmode, CSKY_GCC_SYM (truncdfsf2));
  set_conv_libfunc (sfix_optab,	   SImode, SFmode, CSKY_GCC_SYM (fixsfsi));
  set_conv_libfunc (sfix_optab,	   SImode, DFmode, CSKY_GCC_SYM (fixdfsi));
  set_conv_libfunc (sfix_optab,	   DImode, SFmode, CSKY_GCC_SYM (fixsfdi));
  set_conv_libfunc (sfix_optab,	   DImode, DFmode, CSKY_GCC_SYM (fixdfdi));
  set_conv_libfunc (ufix_optab,	   SImode, SFmode, CSKY_GCC_SYM (fixunssfsi));
  set_conv_libfunc (ufix_optab,	   SImode, DFmode, CSKY_GCC_SYM (fixunsdfsi));
  set_conv_libfunc (ufix_optab,	   DImode, SFmode, CSKY_GCC_SYM (fixunssfdi));
  set_conv_libfunc (ufix_optab,	   DImode, DFmode, CSKY_GCC_SYM (fixunsdfdi));
  set_conv_libfunc (sfloat_optab,  SFmode, SImode, CSKY_GCC_SYM (floatsisf));
  set_conv_libfunc (sfloat_optab,  DFmode, SImode, CSKY_GCC_SYM (floatsidf));
  set_conv_libfunc (sfloat_optab,  SFmode, DImode, CSKY_GCC_SYM (floatdisf));
  set_conv_libfunc (sfloat_optab,  DFmode, DImode, CSKY_GCC_SYM (floatdidf));
  set_conv_libfunc (ufloat_optab,  SFmode, SImode, CSKY_GCC_SYM (floatunsisf));
  set_conv_libfunc (ufloat_optab,  DFmode, SImode, CSKY_GCC_SYM (floatunsidf));
  set_conv_libfunc (ufloat_optab,  SFmode, DImode, CSKY_GCC_SYM (floatundisf));
  set_conv_libfunc (ufloat_optab,  DFmode, DImode, CSKY_GCC_SYM (floatundidf));

  /* Comparison functions */
  set_optab_libfunc (cmp_optab,	   SFmode, CSKY_GCC_SYM (cmpsf2));
  set_optab_libfunc (cmp_optab,	   DFmode, CSKY_GCC_SYM (cmpdf2));
  set_optab_libfunc (unord_optab,  SFmode, CSKY_GCC_SYM (unordsf2));
  set_optab_libfunc (unord_optab,  DFmode, CSKY_GCC_SYM (unorddf2));
  set_optab_libfunc (eq_optab,	   SFmode, CSKY_GCC_SYM (eqsf2));
  set_optab_libfunc (eq_optab,	   DFmode, CSKY_GCC_SYM (eqdf2));
  set_optab_libfunc (ne_optab,	   SFmode, CSKY_GCC_SYM (nesf2));
  set_optab_libfunc (ne_optab,	   DFmode, CSKY_GCC_SYM (nedf2));
  set_optab_libfunc (ge_optab,	   SFmode, CSKY_GCC_SYM (gesf2));
  set_optab_libfunc (ge_optab,	   DFmode, CSKY_GCC_SYM (gedf2));
  set_optab_libfunc (lt_optab,	   SFmode, CSKY_GCC_SYM (ltsf2));
  set_optab_libfunc (lt_optab,	   DFmode, CSKY_GCC_SYM (ltdf2));
  set_optab_libfunc (le_optab,	   SFmode, CSKY_GCC_SYM (lesf2));
  set_optab_libfunc (le_optab,	   DFmode, CSKY_GCC_SYM (ledf2));
  set_optab_libfunc (gt_optab,	   SFmode, CSKY_GCC_SYM (gtsf2));
  set_optab_libfunc (gt_optab,	   DFmode, CSKY_GCC_SYM (gtdf2));
}


/* Implement TARGET_ADDRESS_COST to estimate cost of the memory address X.
   For C-SKY, (register) and (register + offset) have the same cost.
   Other situations cost more.  */

static int
csky_address_cost (rtx x, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
  enum rtx_code code = GET_CODE (x);

  if (code == REG)
    return COSTS_N_INSNS (1);
  if (code == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    return COSTS_N_INSNS (1);

  return COSTS_N_INSNS (3);
}


/* Implement TARGET_FIXED_CONDITION_CODE_REGS.  */

static bool
csky_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CSKY_CC_REGNUM;
  *p2 = INVALID_REGNUM;
  return true;
}

void
csky_init_cumulative_args (CUMULATIVE_ARGS *pcum, tree fntype,
			   rtx libname ATTRIBUTE_UNUSED,
			   tree fndecl ATTRIBUTE_UNUSED)
{
  memset(pcum, 0, sizeof(*pcum));
  if (stdarg_p (fntype))
    pcum->is_stdarg = true;
}


/* Implement the TARGET_INIT_BUILTINS target macro.  */

void
csky_init_builtins (void)
{
  /* Init fp16.  */
  static tree csky_floatHF_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (csky_floatHF_type_node) = GET_MODE_PRECISION (HFmode);
  layout_type (csky_floatHF_type_node);
  (*lang_hooks.types.register_builtin_type) (csky_floatHF_type_node, "__fp16");
}


/* Implement TARGET_MANGLE_TYPE.  */

static const char *
csky_mangle_type (const_tree type)
{
  if (SCALAR_FLOAT_TYPE_P (type)
      && TYPE_PRECISION (type) == 16
      && TYPE_MAIN_VARIANT (type) != float16_type_node)
    return "Dh";

  /* Use the default mangling.  */
  return NULL;
}

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-csky.h"
