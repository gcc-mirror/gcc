/* Subroutines for insn-output.c for Sun SPARC.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)
   64 bit SPARC V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "recog.h"
#include "toplev.h"
#include "ggc.h"
#include "tm_p.h"

/* 1 if the caller has placed an "unimp" insn immediately after the call.
   This is used in v8 code when calling a function that returns a structure.
   v9 doesn't have this.  Be careful to have this test be the same as that
   used on the call.  */

#define SKIP_CALLERS_UNIMP_P  \
(!TARGET_ARCH64 && current_function_returns_struct			\
 && ! integer_zerop (DECL_SIZE (DECL_RESULT (current_function_decl)))	\
 && (TREE_CODE (DECL_SIZE (DECL_RESULT (current_function_decl)))	\
     == INTEGER_CST))

/* Global variables for machine-dependent things.  */

/* Size of frame.  Need to know this to emit return insns from leaf procedures.
   ACTUAL_FSIZE is set by compute_frame_size() which is called during the
   reload pass.  This is important as the value is later used in insn
   scheduling (to see what can go in a delay slot).
   APPARENT_FSIZE is the size of the stack less the register save area and less
   the outgoing argument area.  It is used when saving call preserved regs.  */
static int apparent_fsize;
static int actual_fsize;

/* Number of live general or floating point registers needed to be saved
   (as 4-byte quantities).  This is only done if TARGET_EPILOGUE.  */
static int num_gfregs;

/* Save the operands last given to a compare for use when we
   generate a scc or bcc insn.  */

rtx sparc_compare_op0, sparc_compare_op1;

/* We may need an epilogue if we spill too many registers.
   If this is non-zero, then we branch here for the epilogue.  */
static rtx leaf_label;

#ifdef LEAF_REGISTERS

/* Vector to say how input registers are mapped to output
   registers.  FRAME_POINTER_REGNUM cannot be remapped by
   this function to eliminate it.  You must use -fomit-frame-pointer
   to get that.  */
char leaf_reg_remap[] =
{ 0, 1, 2, 3, 4, 5, 6, 7,
  -1, -1, -1, -1, -1, -1, 14, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  8, 9, 10, 11, 12, 13, -1, 15,

  32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71,
  72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87,
  88, 89, 90, 91, 92, 93, 94, 95,
  96, 97, 98, 99, 100};

#endif

/* Name of where we pretend to think the frame pointer points.
   Normally, this is "%fp", but if we are in a leaf procedure,
   this is "%sp+something".  We record "something" separately as it may be
   too big for reg+constant addressing.  */

static const char *frame_base_name;
static int frame_base_offset;

static rtx pic_setup_code	PARAMS ((void));
static void sparc_init_modes	PARAMS ((void));
static int save_regs		PARAMS ((FILE *, int, int, const char *,
				       int, int, int));
static int restore_regs		PARAMS ((FILE *, int, int, const char *, int, int));
static void build_big_number	PARAMS ((FILE *, int, const char *));
static int function_arg_slotno	PARAMS ((const CUMULATIVE_ARGS *,
				       enum machine_mode, tree, int, int,
				       int *, int *));

static int supersparc_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int hypersparc_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static int ultrasparc_adjust_cost PARAMS ((rtx, rtx, rtx, int));

static void sparc_output_addr_vec PARAMS ((rtx));
static void sparc_output_addr_diff_vec PARAMS ((rtx));
static void sparc_output_deferred_case_vectors PARAMS ((void));
static void sparc_add_gc_roots    PARAMS ((void));
static void mark_ultrasparc_pipeline_state PARAMS ((void *));
static int check_return_regs PARAMS ((rtx));
static int epilogue_renumber PARAMS ((rtx *, int));
static int ultra_cmove_results_ready_p PARAMS ((rtx));
static int ultra_fpmode_conflict_exists PARAMS ((enum machine_mode));
static rtx *ultra_find_type PARAMS ((int, rtx *, int));
static void ultra_build_types_avail PARAMS ((rtx *, int));
static void ultra_flush_pipeline PARAMS ((void));
static void ultra_rescan_pipeline_state PARAMS ((rtx *, int));
static int set_extends PARAMS ((rtx, rtx));

/* Option handling.  */

/* Code model option as passed by user.  */
const char *sparc_cmodel_string;
/* Parsed value.  */
enum cmodel sparc_cmodel;

char sparc_hard_reg_printed[8];

struct sparc_cpu_select sparc_select[] =
{
  /* switch	name,		tune	arch */
  { (char *)0,	"default",	1,	1 },
  { (char *)0,	"-mcpu=",	1,	1 },
  { (char *)0,	"-mtune=",	1,	0 },
  { 0, 0, 0, 0 }
};

/* CPU type.  This is set from TARGET_CPU_DEFAULT and -m{cpu,tune}=xxx.  */
enum processor_type sparc_cpu;

/* Validate and override various options, and do some machine dependent
   initialization.  */

void
sparc_override_options ()
{
  static struct code_model {
    const char *name;
    int value;
  } cmodels[] = {
    { "32", CM_32 },
    { "medlow", CM_MEDLOW },
    { "medmid", CM_MEDMID },
    { "medany", CM_MEDANY },
    { "embmedany", CM_EMBMEDANY },
    { 0, 0 }
  };
  struct code_model *cmodel;
  /* Map TARGET_CPU_DEFAULT to value for -m{arch,tune}=.  */
  static struct cpu_default {
    int cpu;
    const char *name;
  } cpu_default[] = {
    /* There must be one entry here for each TARGET_CPU value.  */
    { TARGET_CPU_sparc, "cypress" },
    { TARGET_CPU_sparclet, "tsc701" },
    { TARGET_CPU_sparclite, "f930" },
    { TARGET_CPU_v8, "v8" },
    { TARGET_CPU_hypersparc, "hypersparc" },
    { TARGET_CPU_sparclite86x, "sparclite86x" },
    { TARGET_CPU_supersparc, "supersparc" },
    { TARGET_CPU_v9, "v9" },
    { TARGET_CPU_ultrasparc, "ultrasparc" },
    { 0, 0 }
  };
  struct cpu_default *def;
  /* Table of values for -m{cpu,tune}=.  */
  static struct cpu_table {
    const char *name;
    enum processor_type processor;
    int disable;
    int enable;
  } cpu_table[] = {
    { "v7",         PROCESSOR_V7, MASK_ISA, 0 },
    { "cypress",    PROCESSOR_CYPRESS, MASK_ISA, 0 },
    { "v8",         PROCESSOR_V8, MASK_ISA, MASK_V8 },
    /* TI TMS390Z55 supersparc */
    { "supersparc", PROCESSOR_SUPERSPARC, MASK_ISA, MASK_V8 },
    { "sparclite",  PROCESSOR_SPARCLITE, MASK_ISA, MASK_SPARCLITE },
    /* The Fujitsu MB86930 is the original sparclite chip, with no fpu.
       The Fujitsu MB86934 is the recent sparclite chip, with an fpu.  */
    { "f930",       PROCESSOR_F930, MASK_ISA|MASK_FPU, MASK_SPARCLITE },
    { "f934",       PROCESSOR_F934, MASK_ISA, MASK_SPARCLITE|MASK_FPU },
    { "hypersparc", PROCESSOR_HYPERSPARC, MASK_ISA, MASK_V8|MASK_FPU },
    { "sparclite86x",  PROCESSOR_SPARCLITE86X, MASK_ISA|MASK_FPU,
      MASK_SPARCLITE },
    { "sparclet",   PROCESSOR_SPARCLET, MASK_ISA, MASK_SPARCLET },
    /* TEMIC sparclet */
    { "tsc701",     PROCESSOR_TSC701, MASK_ISA, MASK_SPARCLET },
    { "v9",         PROCESSOR_V9, MASK_ISA, MASK_V9 },
    /* TI ultrasparc I, II, IIi */
    { "ultrasparc", PROCESSOR_ULTRASPARC, MASK_ISA, MASK_V9
    /* Although insns using %y are deprecated, it is a clear win on current
       ultrasparcs. */
    						    |MASK_DEPRECATED_V8_INSNS },
    { 0, 0, 0, 0 }
  };
  struct cpu_table *cpu;
  struct sparc_cpu_select *sel;
  int fpu;
  
#ifndef SPARC_BI_ARCH
  /* Check for unsupported architecture size.  */
  if (! TARGET_64BIT != DEFAULT_ARCH32_P)
    {
      error ("%s is not supported by this configuration",
	     DEFAULT_ARCH32_P ? "-m64" : "-m32");
    }
#endif

  /* At the moment we don't allow different pointer size and architecture */
  if (! TARGET_64BIT != ! TARGET_PTR64)
    {
      error ("-mptr%d not allowed on -m%d",
      	     TARGET_PTR64 ? 64 : 32, TARGET_64BIT ? 64 : 32);
      if (TARGET_64BIT)
    	target_flags |= MASK_PTR64;
      else
        target_flags &= ~MASK_PTR64;
    }

  /* We force all 64bit archs to use 128 bit long double */
  if (TARGET_64BIT && ! TARGET_LONG_DOUBLE_128)
    {
      error ("-mlong-double-64 not allowed with -m64");
      target_flags |= MASK_LONG_DOUBLE_128;
    }

  /* Code model selection.  */
  sparc_cmodel = SPARC_DEFAULT_CMODEL;
  
#ifdef SPARC_BI_ARCH
  if (TARGET_ARCH32)
    sparc_cmodel = CM_32;
#endif

  if (sparc_cmodel_string != NULL)
    {
      if (TARGET_ARCH64)
	{
	  for (cmodel = &cmodels[0]; cmodel->name; cmodel++)
	    if (strcmp (sparc_cmodel_string, cmodel->name) == 0)
	      break;
	  if (cmodel->name == NULL)
	    error ("bad value (%s) for -mcmodel= switch", sparc_cmodel_string);
	  else
	    sparc_cmodel = cmodel->value;
	}
      else
	error ("-mcmodel= is not supported on 32 bit systems");
    }

  fpu = TARGET_FPU; /* save current -mfpu status */

  /* Set the default CPU.  */
  for (def = &cpu_default[0]; def->name; ++def)
    if (def->cpu == TARGET_CPU_DEFAULT)
      break;
  if (! def->name)
    abort ();
  sparc_select[0].string = def->name;

  for (sel = &sparc_select[0]; sel->name; ++sel)
    {
      if (sel->string)
	{
	  for (cpu = &cpu_table[0]; cpu->name; ++cpu)
	    if (! strcmp (sel->string, cpu->name))
	      {
		if (sel->set_tune_p)
		  sparc_cpu = cpu->processor;

		if (sel->set_arch_p)
		  {
		    target_flags &= ~cpu->disable;
		    target_flags |= cpu->enable;
		  }
		break;
	      }

	  if (! cpu->name)
	    error ("bad value (%s) for %s switch", sel->string, sel->name);
	}
    }

  /* If -mfpu or -mno-fpu was explicitly used, don't override with
     the processor default.  Clear MASK_FPU_SET to avoid confusing
     the reverse mapping from switch values to names.  */
  if (TARGET_FPU_SET)
    {
      target_flags = (target_flags & ~MASK_FPU) | fpu;
      target_flags &= ~MASK_FPU_SET;
    }

  /* Use the deprecated v8 insns for sparc64 in 32 bit mode.  */
  if (TARGET_V9 && TARGET_ARCH32)
    target_flags |= MASK_DEPRECATED_V8_INSNS;

  /* V8PLUS requires V9, makes no sense in 64 bit mode.  */
  if (! TARGET_V9 || TARGET_ARCH64)
    target_flags &= ~MASK_V8PLUS;

  /* Don't use stack biasing in 32 bit mode.  */
  if (TARGET_ARCH32)
    target_flags &= ~MASK_STACK_BIAS;
    
  /* Don't allow -mvis if FPU is disabled.  */
  if (! TARGET_FPU)
    target_flags &= ~MASK_VIS;

  /* Supply a default value for align_functions.  */
  if (align_functions == 0 && sparc_cpu == PROCESSOR_ULTRASPARC)
    align_functions = 32;

  /* Validate PCC_STRUCT_RETURN.  */
  if (flag_pcc_struct_return == DEFAULT_PCC_STRUCT_RETURN)
    flag_pcc_struct_return = (TARGET_ARCH64 ? 0 : 1);

  /* Do various machine dependent initializations.  */
  sparc_init_modes ();

  if ((profile_flag || profile_block_flag)
      && sparc_cmodel != CM_32 && sparc_cmodel != CM_MEDLOW)
    {
      error ("profiling does not support code models other than medlow");
    }

  /* Register global variables with the garbage collector.  */
  sparc_add_gc_roots ();
}

/* Miscellaneous utilities.  */

/* Nonzero if CODE, a comparison, is suitable for use in v9 conditional move
   or branch on register contents instructions.  */

int
v9_regcmp_p (code)
     enum rtx_code code;
{
  return (code == EQ || code == NE || code == GE || code == LT
	  || code == LE || code == GT);
}


/* Operand constraints.  */

/* Return non-zero only if OP is a register of mode MODE,
   or const0_rtx.  */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;
  if (op == const0_rtx)
    return 1;
  if (GET_MODE (op) == VOIDmode && GET_CODE (op) == CONST_DOUBLE
      && CONST_DOUBLE_HIGH (op) == 0
      && CONST_DOUBLE_LOW (op) == 0)
    return 1;
  if (fp_zero_operand (op, mode))
    return 1;
  return 0;
}

/* Nonzero if OP is a floating point value with value 0.0.  */

int
fp_zero_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE_CLASS (GET_MODE (op)) != MODE_FLOAT)
    return 0;
  return op == CONST0_RTX (mode);
}

/* Nonzero if OP is a floating point constant which can
   be loaded into an integer register using a single
   sethi instruction.  */

int
fp_sethi_p (op)
     rtx op;
{
  if (GET_CODE (op) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE r;
      long i;

      REAL_VALUE_FROM_CONST_DOUBLE (r, op);
      if (REAL_VALUES_EQUAL (r, dconst0) &&
	  ! REAL_VALUE_MINUS_ZERO (r))
	return 0;
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      if (SPARC_SETHI_P (i))
	return 1;
    }

  return 0;
}

/* Nonzero if OP is a floating point constant which can
   be loaded into an integer register using a single
   mov instruction.  */

int
fp_mov_p (op)
     rtx op;
{
  if (GET_CODE (op) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE r;
      long i;

      REAL_VALUE_FROM_CONST_DOUBLE (r, op);
      if (REAL_VALUES_EQUAL (r, dconst0) &&
	  ! REAL_VALUE_MINUS_ZERO (r))
	return 0;
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      if (SPARC_SIMM13_P (i))
	return 1;
    }

  return 0;
}

/* Nonzero if OP is a floating point constant which can
   be loaded into an integer register using a high/losum
   instruction sequence.  */

int
fp_high_losum_p (op)
     rtx op;
{
  /* The constraints calling this should only be in
     SFmode move insns, so any constant which cannot
     be moved using a single insn will do.  */
  if (GET_CODE (op) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE r;
      long i;

      REAL_VALUE_FROM_CONST_DOUBLE (r, op);
      if (REAL_VALUES_EQUAL (r, dconst0) &&
	  ! REAL_VALUE_MINUS_ZERO (r))
	return 0;
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      if (! SPARC_SETHI_P (i)
          && ! SPARC_SIMM13_P (i))
	return 1;
    }

  return 0;
}

/* Nonzero if OP is an integer register.  */

int
intreg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (register_operand (op, SImode)
	  || (TARGET_ARCH64 && register_operand (op, DImode)));
}

/* Nonzero if OP is a floating point condition code register.  */

int
fcc_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* This can happen when recog is called from combine.  Op may be a MEM.
     Fail instead of calling abort in this case.  */
  if (GET_CODE (op) != REG)
    return 0;

  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (mode == VOIDmode
      && (GET_MODE (op) != CCFPmode && GET_MODE (op) != CCFPEmode))
    return 0;

#if 0	/* ??? ==> 1 when %fcc0-3 are pseudos first.  See gen_compare_reg().  */
  if (reg_renumber == 0)
    return REGNO (op) >= FIRST_PSEUDO_REGISTER;
  return REGNO_OK_FOR_CCFP_P (REGNO (op));
#else
  return (unsigned) REGNO (op) - SPARC_FIRST_V9_FCC_REG < 4;
#endif
}

/* Nonzero if OP is an integer or floating point condition code register.  */

int
icc_or_fcc_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == REG && REGNO (op) == SPARC_ICC_REG)
    {
      if (mode != VOIDmode && mode != GET_MODE (op))
	return 0;
      if (mode == VOIDmode
	  && GET_MODE (op) != CCmode && GET_MODE (op) != CCXmode)
	return 0;
      return 1;
    }

  return fcc_reg_operand (op, mode);
}

/* Nonzero if OP can appear as the dest of a RESTORE insn.  */
int
restore_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == REG && GET_MODE (op) == mode
	  && (REGNO (op) < 8 || (REGNO (op) >= 24 && REGNO (op) < 32)));
}

/* Call insn on SPARC can take a PC-relative constant address, or any regular
   memory address.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    abort ();
  op = XEXP (op, 0);
  return (symbolic_operand (op, mode) || memory_address_p (Pmode, op));
}

int
call_operand_address (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (symbolic_operand (op, mode) || memory_address_p (Pmode, op));
}

/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum machine_mode omode = GET_MODE (op);

  if (omode != mode && omode != VOIDmode && mode != VOIDmode)
    return 0;

  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case CONST:
      op = XEXP (op, 0);
      return ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
	       || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);

    default:
      return 0;
    }
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST
	  || GET_CODE (op) == HIGH || GET_CODE (op) == LABEL_REF);
}

/* Return truth value of statement that OP is a LABEL_REF of mode MODE.  */

int
label_ref_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != LABEL_REF)
    return 0;
  if (GET_MODE (op) != mode)
    return 0;
  return 1;
}

/* Return 1 if the operand is an argument used in generating pic references
   in either the medium/low or medium/anywhere code models of sparc64.  */

int
sp64_medium_pic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* Check for (const (minus (symbol_ref:GOT)
                             (const (minus (label) (pc))))).  */
  if (GET_CODE (op) != CONST)
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) != MINUS)
    return 0;
  if (GET_CODE (XEXP (op, 0)) != SYMBOL_REF)
    return 0;
  /* ??? Ensure symbol is GOT.  */
  if (GET_CODE (XEXP (op, 1)) != CONST)
    return 0;
  if (GET_CODE (XEXP (XEXP (op, 1), 0)) != MINUS)
    return 0;
  return 1;
}

/* Return 1 if the operand is a data segment reference.  This includes
   the readonly data segment, or in other words anything but the text segment.
   This is needed in the medium/anywhere code model on v9.  These values
   are accessed with EMBMEDANY_BASE_REG.  */

int
data_segment_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
      return ! SYMBOL_REF_FLAG (op);
    case PLUS :
      /* Assume canonical format of symbol + constant.
	 Fall through.  */
    case CONST :
      return data_segment_operand (XEXP (op, 0), VOIDmode);
    default :
      return 0;
    }
}

/* Return 1 if the operand is a text segment reference.
   This is needed in the medium/anywhere code model on v9.  */

int
text_segment_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case LABEL_REF :
      return 1;
    case SYMBOL_REF :
      return SYMBOL_REF_FLAG (op);
    case PLUS :
      /* Assume canonical format of symbol + constant.
	 Fall through.  */
    case CONST :
      return text_segment_operand (XEXP (op, 0), VOIDmode);
    default :
      return 0;
    }
}

/* Return 1 if the operand is either a register or a memory operand that is
   not symbolic.  */

int
reg_or_nonsymb_mem_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;

  if (memory_operand (op, mode) && ! symbolic_memory_operand (op, mode))
    return 1;

  return 0;
}

int
splittable_symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != MEM)
    return 0;
  if (! symbolic_operand (XEXP (op, 0), Pmode))
    return 0;
  return 1;
}

int
splittable_immediate_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != MEM)
    return 0;
  if (! immediate_operand (XEXP (op, 0), Pmode))
    return 0;
  return 1;
}

/* Return truth value of whether OP is EQ or NE.  */

int
eq_or_neq (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return 1 if this is a comparison operator, but not an EQ, NE, GEU,
   or LTU for non-floating-point.  We handle those specially.  */

int
normal_comp_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  if (GET_MODE (XEXP (op, 0)) == CCFPmode
      || GET_MODE (XEXP (op, 0)) == CCFPEmode)
    return 1;

  return (code != NE && code != EQ && code != GEU && code != LTU);
}

/* Return 1 if this is a comparison operator.  This allows the use of
   MATCH_OPERATOR to recognize all the branch insns.  */

int
noov_compare_op (op, mode)
    register rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  if (GET_MODE (XEXP (op, 0)) == CC_NOOVmode)
    /* These are the only branches which work with CC_NOOVmode.  */
    return (code == EQ || code == NE || code == GE || code == LT);
  return 1;
}

/* Nonzero if OP is a comparison operator suitable for use in v9
   conditional move or branch on register contents instructions.  */

int
v9_regcmp_op (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  return v9_regcmp_p (code);
}

/* Return 1 if this is a SIGN_EXTEND or ZERO_EXTEND operation.  */

int
extend_op (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == SIGN_EXTEND || GET_CODE (op) == ZERO_EXTEND;
}

/* Return nonzero if OP is an operator of mode MODE which can set
   the condition codes explicitly.  We do not include PLUS and MINUS
   because these require CC_NOOVmode, which we handle explicitly.  */

int
cc_arithop (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == AND
      || GET_CODE (op) == IOR
      || GET_CODE (op) == XOR)
    return 1;

  return 0;
}

/* Return nonzero if OP is an operator of mode MODE which can bitwise
   complement its second operand and set the condition codes explicitly.  */

int
cc_arithopn (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* XOR is not here because combine canonicalizes (xor (not ...) ...)
     and (xor ... (not ...)) to (not (xor ...)).   */
  return (GET_CODE (op) == AND
	  || GET_CODE (op) == IOR);
}

/* Return true if OP is a register, or is a CONST_INT that can fit in a
   signed 13 bit immediate field.  This is an acceptable SImode operand for
   most 3 address instructions.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int val;
  if (register_operand (op, mode))
    return 1;
  if (GET_CODE (op) != CONST_INT)
    return 0;
  val = INTVAL (op) & 0xffffffff;
  return SPARC_SIMM13_P (val);
}

/* Return true if OP is a constant 4096  */

int
arith_4096_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int val;
  if (GET_CODE (op) != CONST_INT)
    return 0;
  val = INTVAL (op) & 0xffffffff;
  return val == 4096;
}

/* Return true if OP is suitable as second operand for add/sub */

int
arith_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return arith_operand (op, mode) || arith_4096_operand (op, mode);
}

/* Return true if OP is a CONST_INT or a CONST_DOUBLE which can fit in the
   immediate field of OR and XOR instructions.  Used for 64-bit
   constant formation patterns.  */
int
const64_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT
	   && SPARC_SIMM13_P (INTVAL (op)))
#if HOST_BITS_PER_WIDE_INT != 64
	  || (GET_CODE (op) == CONST_DOUBLE
	      && SPARC_SIMM13_P (CONST_DOUBLE_LOW (op))
	      && (CONST_DOUBLE_HIGH (op) ==
		  ((CONST_DOUBLE_LOW (op) & 0x80000000) != 0 ?
		   (HOST_WIDE_INT)0xffffffff : 0)))
#endif
	  );
}

/* The same, but only for sethi instructions.  */
int
const64_high_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT
	   && (INTVAL (op) & 0xfffffc00) != 0
	   && SPARC_SETHI_P (INTVAL (op))
#if HOST_BITS_PER_WIDE_INT != 64
	   /* Must be positive on non-64bit host else the
	      optimizer is fooled into thinking that sethi
	      sign extends, even though it does not.  */
	   && INTVAL (op) >= 0
#endif
	   )
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0
	      && (CONST_DOUBLE_LOW (op) & 0xfffffc00) != 0
	      && SPARC_SETHI_P (CONST_DOUBLE_LOW (op))));
}

/* Return true if OP is a register, or is a CONST_INT that can fit in a
   signed 11 bit immediate field.  This is an acceptable SImode operand for
   the movcc instructions.  */

int
arith11_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SPARC_SIMM11_P (INTVAL (op))));
}

/* Return true if OP is a register, or is a CONST_INT that can fit in a
   signed 10 bit immediate field.  This is an acceptable SImode operand for
   the movrcc instructions.  */

int
arith10_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SPARC_SIMM10_P (INTVAL (op))));
}

/* Return true if OP is a register, is a CONST_INT that fits in a 13 bit
   immediate field, or is a CONST_DOUBLE whose both parts fit in a 13 bit
   immediate field.
   v9: Return true if OP is a register, or is a CONST_INT or CONST_DOUBLE that
   can fit in a 13 bit immediate field.  This is an acceptable DImode operand
   for most 3 address instructions.  */

int
arith_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op))
	  || (! TARGET_ARCH64
	      && GET_CODE (op) == CONST_DOUBLE
	      && (unsigned HOST_WIDE_INT) (CONST_DOUBLE_LOW (op) + 0x1000) < 0x2000
	      && (unsigned HOST_WIDE_INT) (CONST_DOUBLE_HIGH (op) + 0x1000) < 0x2000)
	  || (TARGET_ARCH64
	      && GET_CODE (op) == CONST_DOUBLE
	      && (unsigned HOST_WIDE_INT) (CONST_DOUBLE_LOW (op) + 0x1000) < 0x2000
	      && ((CONST_DOUBLE_HIGH (op) == -1
		   && (CONST_DOUBLE_LOW (op) & 0x1000) == 0x1000)
		  || (CONST_DOUBLE_HIGH (op) == 0
		      && (CONST_DOUBLE_LOW (op) & 0x1000) == 0))));
}

/* Return true if OP is a constant 4096 for DImode on ARCH64 */

int
arith_double_4096_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (TARGET_ARCH64 &&
  	  ((GET_CODE (op) == CONST_INT && INTVAL (op) == 4096) ||
  	   (GET_CODE (op) == CONST_DOUBLE &&
  	    CONST_DOUBLE_LOW (op) == 4096 &&
  	    CONST_DOUBLE_HIGH (op) == 0)));
}

/* Return true if OP is suitable as second operand for add/sub in DImode */

int
arith_double_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return arith_double_operand (op, mode) || arith_double_4096_operand (op, mode);
}

/* Return true if OP is a register, or is a CONST_INT or CONST_DOUBLE that
   can fit in an 11 bit immediate field.  This is an acceptable DImode
   operand for the movcc instructions.  */
/* ??? Replace with arith11_operand?  */

int
arith11_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	      && (unsigned HOST_WIDE_INT) (CONST_DOUBLE_LOW (op) + 0x400) < 0x800
	      && ((CONST_DOUBLE_HIGH (op) == -1
		   && (CONST_DOUBLE_LOW (op) & 0x400) == 0x400)
		  || (CONST_DOUBLE_HIGH (op) == 0
		      && (CONST_DOUBLE_LOW (op) & 0x400) == 0)))
	  || (GET_CODE (op) == CONST_INT
	      && (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	      && (unsigned HOST_WIDE_INT) (INTVAL (op) + 0x400) < 0x800));
}

/* Return true if OP is a register, or is a CONST_INT or CONST_DOUBLE that
   can fit in an 10 bit immediate field.  This is an acceptable DImode
   operand for the movrcc instructions.  */
/* ??? Replace with arith10_operand?  */

int
arith10_double_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	      && (unsigned) (CONST_DOUBLE_LOW (op) + 0x200) < 0x400
	      && ((CONST_DOUBLE_HIGH (op) == -1
		   && (CONST_DOUBLE_LOW (op) & 0x200) == 0x200)
		  || (CONST_DOUBLE_HIGH (op) == 0
		      && (CONST_DOUBLE_LOW (op) & 0x200) == 0)))
	  || (GET_CODE (op) == CONST_INT
	      && (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	      && (unsigned HOST_WIDE_INT) (INTVAL (op) + 0x200) < 0x400));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in most three-address insns,
   which have a 13 bit immediate field.  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

int
small_int_or_double (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT && SMALL_INT (op))
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0
	      && SPARC_SIMM13_P (CONST_DOUBLE_LOW (op))));
}

/* Recognize operand values for the umul instruction.  That instruction sign
   extends immediate values just like all other sparc instructions, but
   interprets the extended result as an unsigned number.  */

int
uns_small_int (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && ((INTVAL (op) >= 0 && INTVAL (op) < 0x1000)
	      || (INTVAL (op) >= 0xFFFFF000
                  && INTVAL (op) < 0x100000000)));
#else
  return ((GET_CODE (op) == CONST_INT && (unsigned) INTVAL (op) < 0x1000)
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0
	      && (unsigned) CONST_DOUBLE_LOW (op) - 0xFFFFF000 < 0x1000));
#endif
}

int
uns_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) || uns_small_int (op, mode);
}

/* Return truth value of statement that OP is a call-clobbered register.  */
int
clobbered_register (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == REG && call_used_regs[REGNO (op)]);
}

/* Return 1 if OP is a valid operand for the source of a move insn.  */

int
input_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* If both modes are non-void they must be the same.  */
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return 0;

  /* Only a tiny bit of handling for CONSTANT_P_RTX is necessary.  */
  if (GET_CODE (op) == CONST && GET_CODE (XEXP (op, 0)) == CONSTANT_P_RTX)
    return 1;

  /* Allow any one instruction integer constant, and all CONST_INT
     variants when we are working in DImode and !arch64.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && ((GET_CODE (op) == CONST_INT
	   && ((SPARC_SETHI_P (INTVAL (op))
		&& (! TARGET_ARCH64
		    || (INTVAL (op) >= 0)
		    || mode == SImode
		    || mode == HImode
		    || mode == QImode))
	       || SPARC_SIMM13_P (INTVAL (op))
	       || (mode == DImode
		   && ! TARGET_ARCH64)))
	  || (TARGET_ARCH64
	      && GET_CODE (op) == CONST_DOUBLE
	      && ((CONST_DOUBLE_HIGH (op) == 0
		   && SPARC_SETHI_P (CONST_DOUBLE_LOW (op)))
		  ||
#if HOST_BITS_PER_WIDE_INT == 64
		  (CONST_DOUBLE_HIGH (op) == 0
		   && SPARC_SIMM13_P (CONST_DOUBLE_LOW (op)))
#else
		  (SPARC_SIMM13_P (CONST_DOUBLE_LOW (op))
		   && (((CONST_DOUBLE_LOW (op) & 0x80000000) == 0
			&& CONST_DOUBLE_HIGH (op) == 0)
		       || (CONST_DOUBLE_HIGH (op) == -1)))
#endif
		  ))))
    return 1;

  /* If !arch64 and this is a DImode const, allow it so that
     the splits can be generated.  */
  if (! TARGET_ARCH64
      && mode == DImode
      && GET_CODE (op) == CONST_DOUBLE)
    return 1;

  if (register_operand (op, mode))
    return 1;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_CODE (op) == CONST_DOUBLE)
    return 1;

  /* If this is a SUBREG, look inside so that we handle
     paradoxical ones.  */
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* Check for valid MEM forms.  */
  if (GET_CODE (op) == MEM)
    {
      rtx inside = XEXP (op, 0);

      if (GET_CODE (inside) == LO_SUM)
	{
	  /* We can't allow these because all of the splits
	     (eventually as they trickle down into DFmode
	     splits) require offsettable memory references.  */
	  if (! TARGET_V9
	      && GET_MODE (op) == TFmode)
	    return 0;

	  return (register_operand (XEXP (inside, 0), Pmode)
		  && CONSTANT_P (XEXP (inside, 1)));
	}
      return memory_address_p (mode, inside);
    }

  return 0;
}


/* We know it can't be done in one insn when we get here,
   the movsi expander guarentees this.  */
void
sparc_emit_set_const32 (op0, op1)
     rtx op0;
     rtx op1;
{
  enum machine_mode mode = GET_MODE (op0);
  rtx temp;

  if (GET_CODE (op1) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL (op1);

      if (SPARC_SETHI_P (value)
	  || SPARC_SIMM13_P (value))
	abort ();
    }

  /* Full 2-insn decomposition is needed.  */
  if (reload_in_progress || reload_completed)
    temp = op0;
  else
    temp = gen_reg_rtx (mode);

  if (GET_CODE (op1) == CONST_INT)
    {
      /* Emit them as real moves instead of a HIGH/LO_SUM,
	 this way CSE can see everything and reuse intermediate
	 values if it wants.  */
      if (TARGET_ARCH64
	  && HOST_BITS_PER_WIDE_INT != 64
	  && (INTVAL (op1) & 0x80000000) != 0)
	{
	  emit_insn (gen_rtx_SET (VOIDmode,
				  temp,
				  gen_rtx_CONST_DOUBLE (VOIDmode, const0_rtx,
							INTVAL (op1) & 0xfffffc00, 0)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (VOIDmode,
				  temp,
				  GEN_INT (INTVAL (op1) & 0xfffffc00)));
	}
      emit_insn (gen_rtx_SET (VOIDmode,
			      op0,
			      gen_rtx_IOR (mode,
					   temp,
					   GEN_INT (INTVAL (op1) & 0x3ff))));
    }
  else
    {
      /* A symbol, emit in the traditional way.  */
      emit_insn (gen_rtx_SET (VOIDmode,
			      temp,
			      gen_rtx_HIGH (mode,
					    op1)));
      emit_insn (gen_rtx_SET (VOIDmode,
			      op0,
			      gen_rtx_LO_SUM (mode,
					      temp,
					      op1)));

    }
}


/* Sparc-v9 code-model support. */
void
sparc_emit_set_symbolic_const64 (op0, op1, temp1)
     rtx op0;
     rtx op1;
     rtx temp1;
{
  switch (sparc_cmodel)
    {
    case CM_MEDLOW:
      /* The range spanned by all instructions in the object is less
	 than 2^31 bytes (2GB) and the distance from any instruction
	 to the location of the label _GLOBAL_OFFSET_TABLE_ is less
	 than 2^31 bytes (2GB).

	 The executable must be in the low 4TB of the virtual address
	 space.

	 sethi	%hi(symbol), %temp
	 or	%temp, %lo(symbol), %reg  */
      emit_insn (gen_rtx_SET (VOIDmode, temp1, gen_rtx_HIGH (DImode, op1)));
      emit_insn (gen_rtx_SET (VOIDmode, op0, gen_rtx_LO_SUM (DImode, temp1, op1)));
      break;

    case CM_MEDMID:
      /* The range spanned by all instructions in the object is less
	 than 2^31 bytes (2GB) and the distance from any instruction
	 to the location of the label _GLOBAL_OFFSET_TABLE_ is less
	 than 2^31 bytes (2GB).

	 The executable must be in the low 16TB of the virtual address
	 space.

	 sethi	%h44(symbol), %temp1
	 or	%temp1, %m44(symbol), %temp2
	 sllx	%temp2, 12, %temp3
	 or	%temp3, %l44(symbol), %reg  */
      emit_insn (gen_seth44 (op0, op1));
      emit_insn (gen_setm44 (op0, op0, op1));
      emit_insn (gen_rtx_SET (VOIDmode, temp1,
			      gen_rtx_ASHIFT (DImode, op0, GEN_INT (12))));
      emit_insn (gen_setl44 (op0, temp1, op1));
      break;

    case CM_MEDANY:
      /* The range spanned by all instructions in the object is less
	 than 2^31 bytes (2GB) and the distance from any instruction
	 to the location of the label _GLOBAL_OFFSET_TABLE_ is less
	 than 2^31 bytes (2GB).

	 The executable can be placed anywhere in the virtual address
	 space.

	 sethi	%hh(symbol), %temp1
	 sethi	%lm(symbol), %temp2
	 or	%temp1, %hm(symbol), %temp3
	 or	%temp2, %lo(symbol), %temp4
	 sllx	%temp3, 32, %temp5
	 or	%temp4, %temp5, %reg  */

      /* Getting this right wrt. reloading is really tricky.
	 We _MUST_ have a separate temporary at this point,
	 if we don't barf immediately instead of generating
	 incorrect code.  */
      if (temp1 == op0)
	abort ();

      emit_insn (gen_sethh (op0, op1));
      emit_insn (gen_setlm (temp1, op1));
      emit_insn (gen_sethm (op0, op0, op1));
      emit_insn (gen_rtx_SET (VOIDmode, op0,
			      gen_rtx_ASHIFT (DImode, op0, GEN_INT (32))));
      emit_insn (gen_rtx_SET (VOIDmode, op0,
			      gen_rtx_PLUS (DImode, op0, temp1)));
      emit_insn (gen_setlo (op0, op0, op1));
      break;

    case CM_EMBMEDANY:
      /* Old old old backwards compatibility kruft here.
	 Essentially it is MEDLOW with a fixed 64-bit
	 virtual base added to all data segment addresses.
	 Text-segment stuff is computed like MEDANY, we can't
	 reuse the code above because the relocation knobs
	 look different.

	 Data segment:	sethi	%hi(symbol), %temp1
			or	%temp1, %lo(symbol), %temp2
			add	%temp2, EMBMEDANY_BASE_REG, %reg

	 Text segment:	sethi	%uhi(symbol), %temp1
			sethi	%hi(symbol), %temp2
			or	%temp1, %ulo(symbol), %temp3
			or	%temp2, %lo(symbol), %temp4
			sllx	%temp3, 32, %temp5
			or	%temp4, %temp5, %reg  */
      if (data_segment_operand (op1, GET_MODE (op1)))
	{
	  emit_insn (gen_embmedany_sethi (temp1, op1));
	  emit_insn (gen_embmedany_brsum (op0, temp1));
	  emit_insn (gen_embmedany_losum (op0, op0, op1));
	}
      else
	{
	  /* Getting this right wrt. reloading is really tricky.
	     We _MUST_ have a separate temporary at this point,
	     so we barf immediately instead of generating
	     incorrect code.  */
	  if (temp1 == op0)
	    abort ();

	  emit_insn (gen_embmedany_textuhi (op0, op1));
	  emit_insn (gen_embmedany_texthi  (temp1, op1));
	  emit_insn (gen_embmedany_textulo (op0, op0, op1));
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_ASHIFT (DImode, op0, GEN_INT (32))));
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_PLUS (DImode, op0, temp1)));
	  emit_insn (gen_embmedany_textlo  (op0, op0, op1));
	}
      break;

    default:
      abort();
    }
}

/* These avoid problems when cross compiling.  If we do not
   go through all this hair then the optimizer will see
   invalid REG_EQUAL notes or in some cases none at all.  */
static void sparc_emit_set_safe_HIGH64 PARAMS ((rtx, HOST_WIDE_INT));
static rtx gen_safe_SET64 PARAMS ((rtx, HOST_WIDE_INT));
static rtx gen_safe_OR64 PARAMS ((rtx, HOST_WIDE_INT));
static rtx gen_safe_XOR64 PARAMS ((rtx, HOST_WIDE_INT));

#if HOST_BITS_PER_WIDE_INT == 64
#define GEN_HIGHINT64(__x)		GEN_INT ((__x) & 0xfffffc00)
#define GEN_INT64(__x)			GEN_INT (__x)
#else
#define GEN_HIGHINT64(__x) \
	gen_rtx_CONST_DOUBLE (VOIDmode, const0_rtx, \
			      (__x) & 0xfffffc00, 0)
#define GEN_INT64(__x) \
	gen_rtx_CONST_DOUBLE (VOIDmode, const0_rtx, \
			      (__x) & 0xffffffff, \
			      ((__x) & 0x80000000 \
			       ? 0xffffffff : 0))
#endif

/* The optimizer is not to assume anything about exactly
   which bits are set for a HIGH, they are unspecified.
   Unfortunately this leads to many missed optimizations
   during CSE.  We mask out the non-HIGH bits, and matches
   a plain movdi, to alleviate this problem.  */
static void
sparc_emit_set_safe_HIGH64 (dest, val)
     rtx dest;
     HOST_WIDE_INT val;
{
  emit_insn (gen_rtx_SET (VOIDmode, dest, GEN_HIGHINT64 (val)));
}

static rtx
gen_safe_SET64 (dest, val)
     rtx dest;
     HOST_WIDE_INT val;
{
  return gen_rtx_SET (VOIDmode, dest, GEN_INT64 (val));
}

static rtx
gen_safe_OR64 (src, val)
     rtx src;
     HOST_WIDE_INT val;
{
  return gen_rtx_IOR (DImode, src, GEN_INT64 (val));
}

static rtx
gen_safe_XOR64 (src, val)
     rtx src;
     HOST_WIDE_INT val;
{
  return gen_rtx_XOR (DImode, src, GEN_INT64 (val));
}

/* Worker routines for 64-bit constant formation on arch64.
   One of the key things to be doing in these emissions is
   to create as many temp REGs as possible.  This makes it
   possible for half-built constants to be used later when
   such values are similar to something required later on.
   Without doing this, the optimizer cannot see such
   opportunities.  */

static void sparc_emit_set_const64_quick1
	PARAMS ((rtx, rtx, unsigned HOST_WIDE_INT, int));

static void
sparc_emit_set_const64_quick1 (op0, temp, low_bits, is_neg)
  rtx op0;
  rtx temp;
  unsigned HOST_WIDE_INT low_bits;
  int is_neg;
{
  unsigned HOST_WIDE_INT high_bits;

  if (is_neg)
    high_bits = (~low_bits) & 0xffffffff;
  else
    high_bits = low_bits;

  sparc_emit_set_safe_HIGH64 (temp, high_bits);
  if (!is_neg)
    {
      emit_insn (gen_rtx_SET (VOIDmode, op0,
			      gen_safe_OR64 (temp, (high_bits & 0x3ff))));
    }
  else
    {
      /* If we are XOR'ing with -1, then we should emit a one's complement
	 instead.  This way the combiner will notice logical operations
	 such as ANDN later on and substitute.  */
      if ((low_bits & 0x3ff) == 0x3ff)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_NOT (DImode, temp)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_safe_XOR64 (temp,
						  (-0x400 | (low_bits & 0x3ff)))));
	}
    }
}

static void sparc_emit_set_const64_quick2
	PARAMS ((rtx, rtx, unsigned HOST_WIDE_INT,
	       unsigned HOST_WIDE_INT, int));

static void
sparc_emit_set_const64_quick2 (op0, temp, high_bits, low_immediate, shift_count)
  rtx op0;
  rtx temp;
  unsigned HOST_WIDE_INT high_bits;
  unsigned HOST_WIDE_INT low_immediate;
  int shift_count;
{
  rtx temp2 = op0;

  if ((high_bits & 0xfffffc00) != 0)
    {
      sparc_emit_set_safe_HIGH64 (temp, high_bits);
      if ((high_bits & ~0xfffffc00) != 0)
	emit_insn (gen_rtx_SET (VOIDmode, op0,
				gen_safe_OR64 (temp, (high_bits & 0x3ff))));
      else
	temp2 = temp;
    }
  else
    {
      emit_insn (gen_safe_SET64 (temp, high_bits));
      temp2 = temp;
    }

  /* Now shift it up into place. */
  emit_insn (gen_rtx_SET (VOIDmode, op0,
			  gen_rtx_ASHIFT (DImode, temp2,
					  GEN_INT (shift_count))));

  /* If there is a low immediate part piece, finish up by
     putting that in as well.  */
  if (low_immediate != 0)
    emit_insn (gen_rtx_SET (VOIDmode, op0,
			    gen_safe_OR64 (op0, low_immediate)));
}

static void sparc_emit_set_const64_longway
	PARAMS ((rtx, rtx, unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT));

/* Full 64-bit constant decomposition.  Even though this is the
   'worst' case, we still optimize a few things away.  */
static void
sparc_emit_set_const64_longway (op0, temp, high_bits, low_bits)
     rtx op0;
     rtx temp;
     unsigned HOST_WIDE_INT high_bits;
     unsigned HOST_WIDE_INT low_bits;
{
  rtx sub_temp;

  if (reload_in_progress || reload_completed)
    sub_temp = op0;
  else
    sub_temp = gen_reg_rtx (DImode);

  if ((high_bits & 0xfffffc00) != 0)
    {
      sparc_emit_set_safe_HIGH64 (temp, high_bits);
      if ((high_bits & ~0xfffffc00) != 0)
	emit_insn (gen_rtx_SET (VOIDmode,
				sub_temp,
				gen_safe_OR64 (temp, (high_bits & 0x3ff))));
      else
	sub_temp = temp;
    }
  else
    {
      emit_insn (gen_safe_SET64 (temp, high_bits));
      sub_temp = temp;
    }

  if (!reload_in_progress && !reload_completed)
    {
      rtx temp2 = gen_reg_rtx (DImode);
      rtx temp3 = gen_reg_rtx (DImode);
      rtx temp4 = gen_reg_rtx (DImode);

      emit_insn (gen_rtx_SET (VOIDmode, temp4,
			      gen_rtx_ASHIFT (DImode, sub_temp,
					      GEN_INT (32))));

      sparc_emit_set_safe_HIGH64 (temp2, low_bits);
      if ((low_bits & ~0xfffffc00) != 0)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, temp3,
				  gen_safe_OR64 (temp2, (low_bits & 0x3ff))));
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_PLUS (DImode, temp4, temp3)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_PLUS (DImode, temp4, temp2)));
	}
    }
  else
    {
      rtx low1 = GEN_INT ((low_bits >> (32 - 12))          & 0xfff);
      rtx low2 = GEN_INT ((low_bits >> (32 - 12 - 12))     & 0xfff);
      rtx low3 = GEN_INT ((low_bits >> (32 - 12 - 12 - 8)) & 0x0ff);
      int to_shift = 12;

      /* We are in the middle of reload, so this is really
	 painful.  However we do still make an attempt to
	 avoid emitting truly stupid code.  */
      if (low1 != const0_rtx)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_ASHIFT (DImode, sub_temp,
						  GEN_INT (to_shift))));
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_IOR (DImode, op0, low1)));
	  sub_temp = op0;
	  to_shift = 12;
	}
      else
	{
	  to_shift += 12;
	}
      if (low2 != const0_rtx)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_ASHIFT (DImode, sub_temp,
						  GEN_INT (to_shift))));
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_IOR (DImode, op0, low2)));
	  sub_temp = op0;
	  to_shift = 8;
	}
      else
	{
	  to_shift += 8;
	}
      emit_insn (gen_rtx_SET (VOIDmode, op0,
			      gen_rtx_ASHIFT (DImode, sub_temp,
					      GEN_INT (to_shift))));
      if (low3 != const0_rtx)
	emit_insn (gen_rtx_SET (VOIDmode, op0,
				gen_rtx_IOR (DImode, op0, low3)));
      /* phew... */
    }
}

/* Analyze a 64-bit constant for certain properties. */
static void analyze_64bit_constant
	PARAMS ((unsigned HOST_WIDE_INT,
	       unsigned HOST_WIDE_INT,
	       int *, int *, int *));

static void
analyze_64bit_constant (high_bits, low_bits, hbsp, lbsp, abbasp)
     unsigned HOST_WIDE_INT high_bits, low_bits;
     int *hbsp, *lbsp, *abbasp;
{
  int lowest_bit_set, highest_bit_set, all_bits_between_are_set;
  int i;

  lowest_bit_set = highest_bit_set = -1;
  i = 0;
  do
    {
      if ((lowest_bit_set == -1)
	  && ((low_bits >> i) & 1))
	lowest_bit_set = i;
      if ((highest_bit_set == -1)
	  && ((high_bits >> (32 - i - 1)) & 1))
	highest_bit_set = (64 - i - 1);
    }
  while (++i < 32
	 && ((highest_bit_set == -1)
	     || (lowest_bit_set == -1)));
  if (i == 32)
    {
      i = 0;
      do
	{
	  if ((lowest_bit_set == -1)
	      && ((high_bits >> i) & 1))
	    lowest_bit_set = i + 32;
	  if ((highest_bit_set == -1)
	      && ((low_bits >> (32 - i - 1)) & 1))
	    highest_bit_set = 32 - i - 1;
	}
      while (++i < 32
	     && ((highest_bit_set == -1)
		 || (lowest_bit_set == -1)));
    }
  /* If there are no bits set this should have gone out
     as one instruction!  */
  if (lowest_bit_set == -1
      || highest_bit_set == -1)
    abort ();
  all_bits_between_are_set = 1;
  for (i = lowest_bit_set; i <= highest_bit_set; i++)
    {
      if (i < 32)
	{
	  if ((low_bits & (1 << i)) != 0)
	    continue;
	}
      else
	{
	  if ((high_bits & (1 << (i - 32))) != 0)
	    continue;
	}
      all_bits_between_are_set = 0;
      break;
    }
  *hbsp = highest_bit_set;
  *lbsp = lowest_bit_set;
  *abbasp = all_bits_between_are_set;
}

static int const64_is_2insns
	PARAMS ((unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT));

static int
const64_is_2insns (high_bits, low_bits)
     unsigned HOST_WIDE_INT high_bits, low_bits;
{
  int highest_bit_set, lowest_bit_set, all_bits_between_are_set;

  if (high_bits == 0
      || high_bits == 0xffffffff)
    return 1;

  analyze_64bit_constant (high_bits, low_bits,
			  &highest_bit_set, &lowest_bit_set,
			  &all_bits_between_are_set);

  if ((highest_bit_set == 63
       || lowest_bit_set == 0)
      && all_bits_between_are_set != 0)
    return 1;

  if ((highest_bit_set - lowest_bit_set) < 21)
    return 1;

  return 0;
}

static unsigned HOST_WIDE_INT create_simple_focus_bits
	PARAMS ((unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
	       int, int));

static unsigned HOST_WIDE_INT
create_simple_focus_bits (high_bits, low_bits, lowest_bit_set, shift)
     unsigned HOST_WIDE_INT high_bits, low_bits;
     int lowest_bit_set, shift;
{
  HOST_WIDE_INT hi, lo;

  if (lowest_bit_set < 32)
    {
      lo = (low_bits >> lowest_bit_set) << shift;
      hi = ((high_bits << (32 - lowest_bit_set)) << shift);
    }
  else
    {
      lo = 0;
      hi = ((high_bits >> (lowest_bit_set - 32)) << shift);
    }
  if (hi & lo)
    abort ();
  return (hi | lo);
}

/* Here we are sure to be arch64 and this is an integer constant
   being loaded into a register.  Emit the most efficient
   insn sequence possible.  Detection of all the 1-insn cases
   has been done already.  */
void
sparc_emit_set_const64 (op0, op1)
     rtx op0;
     rtx op1;
{
  unsigned HOST_WIDE_INT high_bits, low_bits;
  int lowest_bit_set, highest_bit_set;
  int all_bits_between_are_set;
  rtx temp;

  /* Sanity check that we know what we are working with.  */
  if (! TARGET_ARCH64
      || GET_CODE (op0) != REG
      || (REGNO (op0) >= SPARC_FIRST_FP_REG
	  && REGNO (op0) <= SPARC_LAST_V9_FP_REG))
    abort ();

  if (reload_in_progress || reload_completed)
    temp = op0;
  else
    temp = gen_reg_rtx (DImode);

  if (GET_CODE (op1) != CONST_DOUBLE
      && GET_CODE (op1) != CONST_INT)
    {
      sparc_emit_set_symbolic_const64 (op0, op1, temp);
      return;
    }

  if (GET_CODE (op1) == CONST_DOUBLE)
    {
#if HOST_BITS_PER_WIDE_INT == 64
      high_bits = (CONST_DOUBLE_LOW (op1) >> 32) & 0xffffffff;
      low_bits  = CONST_DOUBLE_LOW (op1) & 0xffffffff;
#else
      high_bits = CONST_DOUBLE_HIGH (op1);
      low_bits = CONST_DOUBLE_LOW (op1);
#endif
    }
  else
    {
#if HOST_BITS_PER_WIDE_INT == 64
      high_bits = ((INTVAL (op1) >> 32) & 0xffffffff);
      low_bits = (INTVAL (op1) & 0xffffffff);
#else
      high_bits = ((INTVAL (op1) < 0) ?
		   0xffffffff :
		   0x00000000);
      low_bits = INTVAL (op1);
#endif
    }

  /* low_bits	bits 0  --> 31
     high_bits	bits 32 --> 63  */

  analyze_64bit_constant (high_bits, low_bits,
			  &highest_bit_set, &lowest_bit_set,
			  &all_bits_between_are_set);

  /* First try for a 2-insn sequence.  */

  /* These situations are preferred because the optimizer can
   * do more things with them:
   * 1) mov	-1, %reg
   *    sllx	%reg, shift, %reg
   * 2) mov	-1, %reg
   *    srlx	%reg, shift, %reg
   * 3) mov	some_small_const, %reg
   *    sllx	%reg, shift, %reg
   */
  if (((highest_bit_set == 63
	|| lowest_bit_set == 0)
       && all_bits_between_are_set != 0)
      || ((highest_bit_set - lowest_bit_set) < 12))
    {
      HOST_WIDE_INT the_const = -1;
      int shift = lowest_bit_set;

      if ((highest_bit_set != 63
	   && lowest_bit_set != 0)
	  || all_bits_between_are_set == 0)
	{
	  the_const =
	    create_simple_focus_bits (high_bits, low_bits,
				      lowest_bit_set, 0);
	}
      else if (lowest_bit_set == 0)
	shift = -(63 - highest_bit_set);

      if (! SPARC_SIMM13_P (the_const))
	abort ();

      emit_insn (gen_safe_SET64 (temp, the_const));
      if (shift > 0)
	emit_insn (gen_rtx_SET (VOIDmode,
				op0,
				gen_rtx_ASHIFT (DImode,
						temp,
						GEN_INT (shift))));
      else if (shift < 0)
	emit_insn (gen_rtx_SET (VOIDmode,
				op0,
				gen_rtx_LSHIFTRT (DImode,
						  temp,
						  GEN_INT (-shift))));
      else
	abort ();
      return;
    }

  /* Now a range of 22 or less bits set somewhere.
   * 1) sethi	%hi(focus_bits), %reg
   *    sllx	%reg, shift, %reg
   * 2) sethi	%hi(focus_bits), %reg
   *    srlx	%reg, shift, %reg
   */
  if ((highest_bit_set - lowest_bit_set) < 21)
    {
      unsigned HOST_WIDE_INT focus_bits =
	create_simple_focus_bits (high_bits, low_bits,
				  lowest_bit_set, 10);

      if (! SPARC_SETHI_P (focus_bits))
	 abort ();

      sparc_emit_set_safe_HIGH64 (temp, focus_bits);

      /* If lowest_bit_set == 10 then a sethi alone could have done it.  */
      if (lowest_bit_set < 10)
	emit_insn (gen_rtx_SET (VOIDmode,
				op0,
				gen_rtx_LSHIFTRT (DImode, temp,
						  GEN_INT (10 - lowest_bit_set))));
      else if (lowest_bit_set > 10)
	emit_insn (gen_rtx_SET (VOIDmode,
				op0,
				gen_rtx_ASHIFT (DImode, temp,
						GEN_INT (lowest_bit_set - 10))));
      else
	abort ();
      return;
    }

  /* 1) sethi	%hi(low_bits), %reg
   *    or	%reg, %lo(low_bits), %reg
   * 2) sethi	%hi(~low_bits), %reg
   *	xor	%reg, %lo(-0x400 | (low_bits & 0x3ff)), %reg
   */
  if (high_bits == 0
      || high_bits == 0xffffffff)
    {
      sparc_emit_set_const64_quick1 (op0, temp, low_bits,
				     (high_bits == 0xffffffff));
      return;
    }

  /* Now, try 3-insn sequences.  */

  /* 1) sethi	%hi(high_bits), %reg
   *    or	%reg, %lo(high_bits), %reg
   *    sllx	%reg, 32, %reg
   */
  if (low_bits == 0)
    {
      sparc_emit_set_const64_quick2 (op0, temp, high_bits, 0, 32);
      return;
    }

  /* We may be able to do something quick
     when the constant is negated, so try that.  */
  if (const64_is_2insns ((~high_bits) & 0xffffffff,
			 (~low_bits) & 0xfffffc00))
    {
      /* NOTE: The trailing bits get XOR'd so we need the
	 non-negated bits, not the negated ones.  */
      unsigned HOST_WIDE_INT trailing_bits = low_bits & 0x3ff;

      if ((((~high_bits) & 0xffffffff) == 0
	   && ((~low_bits) & 0x80000000) == 0)
	  || (((~high_bits) & 0xffffffff) == 0xffffffff
	      && ((~low_bits) & 0x80000000) != 0))
	{
	  int fast_int = (~low_bits & 0xffffffff);

	  if ((SPARC_SETHI_P (fast_int)
	       && (~high_bits & 0xffffffff) == 0)
	      || SPARC_SIMM13_P (fast_int))
	    emit_insn (gen_safe_SET64 (temp, fast_int));
	  else
	    sparc_emit_set_const64 (temp, GEN_INT64 (fast_int));
	}
      else
	{
	  rtx negated_const;
#if HOST_BITS_PER_WIDE_INT == 64
	  negated_const = GEN_INT (((~low_bits) & 0xfffffc00) |
				   (((HOST_WIDE_INT)((~high_bits) & 0xffffffff))<<32));
#else
	  negated_const = gen_rtx_CONST_DOUBLE (DImode, const0_rtx,
						(~low_bits) & 0xfffffc00,
						(~high_bits) & 0xffffffff);
#endif
	  sparc_emit_set_const64 (temp, negated_const);
	}

      /* If we are XOR'ing with -1, then we should emit a one's complement
	 instead.  This way the combiner will notice logical operations
	 such as ANDN later on and substitute.  */
      if (trailing_bits == 0x3ff)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, op0,
				  gen_rtx_NOT (DImode, temp)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (VOIDmode,
				  op0,
				  gen_safe_XOR64 (temp,
						  (-0x400 | trailing_bits))));
	}
      return;
    }

  /* 1) sethi	%hi(xxx), %reg
   *    or	%reg, %lo(xxx), %reg
   *	sllx	%reg, yyy, %reg
   *
   * ??? This is just a generalized version of the low_bits==0
   * thing above, FIXME...
   */
  if ((highest_bit_set - lowest_bit_set) < 32)
    {
      unsigned HOST_WIDE_INT focus_bits =
	create_simple_focus_bits (high_bits, low_bits,
				  lowest_bit_set, 0);

      /* We can't get here in this state.  */
      if (highest_bit_set < 32
	  || lowest_bit_set >= 32)
	abort ();

      /* So what we know is that the set bits straddle the
	 middle of the 64-bit word.  */
      sparc_emit_set_const64_quick2 (op0, temp,
				     focus_bits, 0,
				     lowest_bit_set);
      return;
    }

  /* 1) sethi	%hi(high_bits), %reg
   *    or	%reg, %lo(high_bits), %reg
   *    sllx	%reg, 32, %reg
   *	or	%reg, low_bits, %reg
   */
  if (SPARC_SIMM13_P(low_bits)
      && ((int)low_bits > 0))
    {
      sparc_emit_set_const64_quick2 (op0, temp, high_bits, low_bits, 32);
      return;
    }

  /* The easiest way when all else fails, is full decomposition. */
#if 0
  printf ("sparc_emit_set_const64: Hard constant [%08lx%08lx] neg[%08lx%08lx]\n",
	  high_bits, low_bits, ~high_bits, ~low_bits);
#endif
  sparc_emit_set_const64_longway (op0, temp, high_bits, low_bits);
}

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point,
   CCFP[E]mode is used.  CC_NOOVmode should be used when the first operand
   is a PLUS, MINUS, NEG, or ASHIFT.  CCmode should be used when no special
   processing is needed.  */

enum machine_mode
select_cc_mode (op, x, y)
     enum rtx_code op;
     rtx x;
     rtx y ATTRIBUTE_UNUSED;
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      switch (op)
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
	case LTGT:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	default:
	  abort ();
	}
    }
  else if (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	   || GET_CODE (x) == NEG || GET_CODE (x) == ASHIFT)
    {
      if (TARGET_ARCH64 && GET_MODE (x) == DImode)
	return CCX_NOOVmode;
      else
	return CC_NOOVmode;
    }
  else
    {
      if (TARGET_ARCH64 && GET_MODE (x) == DImode)
	return CCXmode;
      else
	return CCmode;
    }
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for the cc reg in the proper mode.  */

rtx
gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg;

  /* ??? We don't have movcc patterns so we cannot generate pseudo regs for the
     fcc regs (cse can't tell they're really call clobbered regs and will
     remove a duplicate comparison even if there is an intervening function
     call - it will then try to reload the cc reg via an int reg which is why
     we need the movcc patterns).  It is possible to provide the movcc
     patterns by using the ldxfsr/stxfsr v9 insns.  I tried it: you need two
     registers (say %g1,%g5) and it takes about 6 insns.  A better fix would be
     to tell cse that CCFPE mode registers (even pseudos) are call
     clobbered.  */

  /* ??? This is an experiment.  Rather than making changes to cse which may
     or may not be easy/clean, we do our own cse.  This is possible because
     we will generate hard registers.  Cse knows they're call clobbered (it
     doesn't know the same thing about pseudos). If we guess wrong, no big
     deal, but if we win, great!  */

  if (TARGET_V9 && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
#if 1 /* experiment */
    {
      int reg;
      /* We cycle through the registers to ensure they're all exercised.  */
      static int next_fcc_reg = 0;
      /* Previous x,y for each fcc reg.  */
      static rtx prev_args[4][2];

      /* Scan prev_args for x,y.  */
      for (reg = 0; reg < 4; reg++)
	if (prev_args[reg][0] == x && prev_args[reg][1] == y)
	  break;
      if (reg == 4)
	{
	  reg = next_fcc_reg;
	  prev_args[reg][0] = x;
	  prev_args[reg][1] = y;
	  next_fcc_reg = (next_fcc_reg + 1) & 3;
	}
      cc_reg = gen_rtx_REG (mode, reg + SPARC_FIRST_V9_FCC_REG);
    }
#else
    cc_reg = gen_reg_rtx (mode);
#endif /* ! experiment */
  else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    cc_reg = gen_rtx_REG (mode, SPARC_FCC_REG);
  else
    cc_reg = gen_rtx_REG (mode, SPARC_ICC_REG);

  emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
			  gen_rtx_COMPARE (mode, x, y)));

  return cc_reg;
}

/* This function is used for v9 only.
   CODE is the code for an Scc's comparison.
   OPERANDS[0] is the target of the Scc insn.
   OPERANDS[1] is the value we compare against const0_rtx (which hasn't
   been generated yet).

   This function is needed to turn

	   (set (reg:SI 110)
	       (gt (reg:CCX 100 %icc)
	           (const_int 0)))
   into
	   (set (reg:SI 110)
	       (gt:DI (reg:CCX 100 %icc)
	           (const_int 0)))

   IE: The instruction recognizer needs to see the mode of the comparison to
   find the right instruction. We could use "gt:DI" right in the
   define_expand, but leaving it out allows us to handle DI, SI, etc.

   We refer to the global sparc compare operands sparc_compare_op0 and
   sparc_compare_op1.  */

int
gen_v9_scc (compare_code, operands)
     enum rtx_code compare_code;
     register rtx *operands;
{
  rtx temp, op0, op1;

  if (! TARGET_ARCH64
      && (GET_MODE (sparc_compare_op0) == DImode
	  || GET_MODE (operands[0]) == DImode))
    return 0;

  /* Handle the case where operands[0] == sparc_compare_op0.
     We "early clobber" the result.  */
  if (REGNO (operands[0]) == REGNO (sparc_compare_op0))
    {
      op0 = gen_reg_rtx (GET_MODE (sparc_compare_op0));
      emit_move_insn (op0, sparc_compare_op0);
    }
  else
    op0 = sparc_compare_op0;
  /* For consistency in the following.  */
  op1 = sparc_compare_op1;

  /* Try to use the movrCC insns.  */
  if (TARGET_ARCH64
      && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
      && op1 == const0_rtx
      && v9_regcmp_p (compare_code))
    {
      /* Special case for op0 != 0.  This can be done with one instruction if
	 operands[0] == sparc_compare_op0.  We don't assume they are equal
	 now though.  */

      if (compare_code == NE
	  && GET_MODE (operands[0]) == DImode
	  && GET_MODE (op0) == DImode)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, operands[0], op0));
	  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			      gen_rtx_IF_THEN_ELSE (DImode,
				       gen_rtx_fmt_ee (compare_code, DImode,
						       op0, const0_rtx),
				       const1_rtx,
				       operands[0])));
	  return 1;
	}

      emit_insn (gen_rtx_SET (VOIDmode, operands[0], const0_rtx));
      if (GET_MODE (op0) != DImode)
	{
	  temp = gen_reg_rtx (DImode);
	  convert_move (temp, op0, 0);
	}
      else
	temp = op0;
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
				   gen_rtx_fmt_ee (compare_code, DImode,
						   temp, const0_rtx),
				   const1_rtx,
				   operands[0])));
      return 1;
    }
  else
    {
      operands[1] = gen_compare_reg (compare_code, op0, op1);

      switch (GET_MODE (operands[1]))
	{
	  case CCmode :
	  case CCXmode :
	  case CCFPEmode :
	  case CCFPmode :
	    break;
	  default :
	    abort ();
	}
      emit_insn (gen_rtx_SET (VOIDmode, operands[0], const0_rtx));
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
				   gen_rtx_fmt_ee (compare_code,
						   GET_MODE (operands[1]),
						   operands[1], const0_rtx),
				    const1_rtx, operands[0])));
      return 1;
    }
}

/* Emit a conditional jump insn for the v9 architecture using comparison code
   CODE and jump target LABEL.
   This function exists to take advantage of the v9 brxx insns.  */

void
emit_v9_brxx_insn (code, op0, label)
     enum rtx_code code;
     rtx op0, label;
{
  emit_jump_insn (gen_rtx_SET (VOIDmode,
			   pc_rtx,
			   gen_rtx_IF_THEN_ELSE (VOIDmode,
				    gen_rtx_fmt_ee (code, GET_MODE (op0),
						    op0, const0_rtx),
				    gen_rtx_LABEL_REF (VOIDmode, label),
				    pc_rtx)));
}

/* Generate a DFmode part of a hard TFmode register.
   REG is the TFmode hard register, LOW is 1 for the
   low 64bit of the register and 0 otherwise.
 */
rtx
gen_df_reg (reg, low)
     rtx reg;
     int low;
{
  int regno = REGNO (reg);

  if ((WORDS_BIG_ENDIAN == 0) ^ (low != 0))
    regno += (TARGET_ARCH64 && regno < 32) ? 1 : 2;
  return gen_rtx_REG (DFmode, regno);
}

/* Return nonzero if a return peephole merging return with
   setting of output register is ok.  */
int
leaf_return_peephole_ok ()
{
  return (actual_fsize == 0);
}

/* Return nonzero if TRIAL can go into the function epilogue's
   delay slot.  SLOT is the slot we are trying to fill.  */

int
eligible_for_epilogue_delay (trial, slot)
     rtx trial;
     int slot;
{
  rtx pat, src;

  if (slot >= 1)
    return 0;

  if (GET_CODE (trial) != INSN || GET_CODE (PATTERN (trial)) != SET)
    return 0;

  if (get_attr_length (trial) != 1)
    return 0;

  /* If there are any call-saved registers, we should scan TRIAL if it
     does not reference them.  For now just make it easy.  */
  if (num_gfregs)
    return 0;

  /* In the case of a true leaf function, anything can go into the delay slot.
     A delay slot only exists however if the frame size is zero, otherwise
     we will put an insn to adjust the stack after the return.  */
  if (current_function_uses_only_leaf_regs)
    {
      if (leaf_return_peephole_ok ())
	return ((get_attr_in_uncond_branch_delay (trial)
		 == IN_BRANCH_DELAY_TRUE));
      return 0;
    }

  pat = PATTERN (trial);

  /* Otherwise, only operations which can be done in tandem with
     a `restore' or `return' insn can go into the delay slot.  */
  if (GET_CODE (SET_DEST (pat)) != REG
      || REGNO (SET_DEST (pat)) < 24)
    return 0;

  /* If this instruction sets up floating point register and we have a return
     instruction, it can probably go in.  But restore will not work
     with FP_REGS.  */
  if (REGNO (SET_DEST (pat)) >= 32)
    {
      if (TARGET_V9 && ! epilogue_renumber (&pat, 1)
	  && (get_attr_in_uncond_branch_delay (trial) == IN_BRANCH_DELAY_TRUE))
	return 1;
      return 0;
    }

  /* The set of insns matched here must agree precisely with the set of
     patterns paired with a RETURN in sparc.md.  */

  src = SET_SRC (pat);

  /* This matches "*return_[qhs]i" or even "*return_di" on TARGET_ARCH64.  */
  if (arith_operand (src, GET_MODE (src)))
    {
      if (TARGET_ARCH64)
        return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (DImode);
      else
        return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (SImode);
    }

  /* This matches "*return_di".  */
  else if (arith_double_operand (src, GET_MODE (src)))
    return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (DImode);

  /* This matches "*return_sf_no_fpu".  */
  else if (! TARGET_FPU && restore_operand (SET_DEST (pat), SFmode)
	   && register_operand (src, SFmode))
    return 1;

  /* If we have return instruction, anything that does not use
     local or output registers and can go into a delay slot wins.  */
  else if (TARGET_V9 && ! epilogue_renumber (&pat, 1)
	   && (get_attr_in_uncond_branch_delay (trial) == IN_BRANCH_DELAY_TRUE))
    return 1;

  /* This matches "*return_addsi".  */
  else if (GET_CODE (src) == PLUS
	   && arith_operand (XEXP (src, 0), SImode)
	   && arith_operand (XEXP (src, 1), SImode)
	   && (register_operand (XEXP (src, 0), SImode)
	       || register_operand (XEXP (src, 1), SImode)))
    return 1;

  /* This matches "*return_adddi".  */
  else if (GET_CODE (src) == PLUS
	   && arith_double_operand (XEXP (src, 0), DImode)
	   && arith_double_operand (XEXP (src, 1), DImode)
	   && (register_operand (XEXP (src, 0), DImode)
	       || register_operand (XEXP (src, 1), DImode)))
    return 1;

  /* This can match "*return_losum_[sd]i".
     Catch only some cases, so that return_losum* don't have
     to be too big.  */
  else if (GET_CODE (src) == LO_SUM
	   && ! TARGET_CM_MEDMID
	   && ((register_operand (XEXP (src, 0), SImode)
	        && immediate_operand (XEXP (src, 1), SImode))
	       || (TARGET_ARCH64
		   && register_operand (XEXP (src, 0), DImode)
		   && immediate_operand (XEXP (src, 1), DImode))))
    return 1;

  /* sll{,x} reg,1,reg2 is add reg,reg,reg2 as well.  */
  else if (GET_CODE (src) == ASHIFT
	   && (register_operand (XEXP (src, 0), SImode)
	       || register_operand (XEXP (src, 0), DImode))
	   && XEXP (src, 1) == const1_rtx)
    return 1;

  return 0;
}

static int
check_return_regs (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case REG:
      return IN_OR_GLOBAL_P (x);

    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    return 1;

    case SET:
    case IOR:
    case AND:
    case XOR:
    case PLUS:
    case MINUS:
      if (check_return_regs (XEXP (x, 1)) == 0)
  return 0;
    case NOT:
    case NEG:
    case MEM:
      return check_return_regs (XEXP (x, 0));
      
    default:
      return 0;
    }

}

/* Return 1 if TRIAL references only in and global registers.  */
int
eligible_for_return_delay (trial)
     rtx trial;
{
  if (GET_CODE (PATTERN (trial)) != SET)
    return 0;

  return check_return_regs (PATTERN (trial));
}

int
short_branch (uid1, uid2)
     int uid1, uid2;
{
  unsigned int delta = insn_addresses[uid1] - insn_addresses[uid2];
  if (delta + 1024 < 2048)
    return 1;
  /* warning ("long branch, distance %d", delta); */
  return 0;
}

/* Return non-zero if REG is not used after INSN.
   We assume REG is a reload reg, and therefore does
   not live past labels or calls or jumps.  */
int
reg_unused_after (reg, insn)
     rtx reg;
     rtx insn;
{
  enum rtx_code code, prev_code = UNKNOWN;

  while ((insn = NEXT_INSN (insn)))
    {
      if (prev_code == CALL_INSN && call_used_regs[REGNO (reg)])
	return 1;

      code = GET_CODE (insn);
      if (GET_CODE (insn) == CODE_LABEL)
	return 1;

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx set = single_set (insn);
	  int in_src = set && reg_overlap_mentioned_p (reg, SET_SRC (set));
	  if (set && in_src)
	    return 0;
	  if (set && reg_overlap_mentioned_p (reg, SET_DEST (set)))
	    return 1;
	  if (set == 0 && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	    return 0;
	}
      prev_code = code;
    }
  return 1;
}

/* The table we use to reference PIC data.  */
static rtx global_offset_table;

/* The function we use to get at it.  */
static rtx get_pc_symbol;
static char get_pc_symbol_name[256];

/* Ensure that we are not using patterns that are not OK with PIC.  */

int
check_pic (i)
     int i;
{
  switch (flag_pic)
    {
    case 1:
      if (GET_CODE (recog_data.operand[i]) == SYMBOL_REF
	  || (GET_CODE (recog_data.operand[i]) == CONST
	      && ! (GET_CODE (XEXP (recog_data.operand[i], 0)) == MINUS
		    && (XEXP (XEXP (recog_data.operand[i], 0), 0)
			== global_offset_table)
		    && (GET_CODE (XEXP (XEXP (recog_data.operand[i], 0), 1))
			== CONST))))
	abort ();
    case 2:
    default:
      return 1;
    }
}

/* Return true if X is an address which needs a temporary register when 
   reloaded while generating PIC code.  */

int
pic_address_needs_scratch (x)
     rtx x;
{
  /* An address which is a symbolic plus a non SMALL_INT needs a temp reg.  */
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && ! SMALL_INT (XEXP (XEXP (x, 0), 1)))
    return 1;

  return 0;
}

/* Legitimize PIC addresses.  If the address is already position-independent,
   we return ORIG.  Newly generated position-independent addresses go into a
   reg.  This is REG if non zero, otherwise we allocate register(s) as
   necessary.  */

rtx
legitimize_pic_address (orig, mode, reg)
     rtx orig;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx reg;
{
  if (GET_CODE (orig) == SYMBOL_REF)
    {
      rtx pic_ref, address;
      rtx insn;

      if (reg == 0)
	{
	  if (reload_in_progress || reload_completed)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (flag_pic == 2)
	{
	  /* If not during reload, allocate another temp reg here for loading
	     in the address, so that these instructions can be optimized
	     properly.  */
	  rtx temp_reg = ((reload_in_progress || reload_completed)
			  ? reg : gen_reg_rtx (Pmode));

	  /* Must put the SYMBOL_REF inside an UNSPEC here so that cse
	     won't get confused into thinking that these two instructions
	     are loading in the true address of the symbol.  If in the
	     future a PIC rtx exists, that should be used instead.  */
	  if (Pmode == SImode)
	    {
	      emit_insn (gen_movsi_high_pic (temp_reg, orig));
	      emit_insn (gen_movsi_lo_sum_pic (temp_reg, temp_reg, orig));
	    }
	  else
	    {
	      emit_insn (gen_movdi_high_pic (temp_reg, orig));
	      emit_insn (gen_movdi_lo_sum_pic (temp_reg, temp_reg, orig));
	    }
	  address = temp_reg;
	}
      else
	address = orig;

      pic_ref = gen_rtx_MEM (Pmode,
			 gen_rtx_PLUS (Pmode,
				  pic_offset_table_rtx, address));
      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      insn = emit_move_insn (reg, pic_ref);
      /* Put a REG_EQUAL note on this insn, so that it can be optimized
	 by loop.  */
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL, orig,
				  REG_NOTES (insn));
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  if (reload_in_progress || reload_completed)
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base = legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  offset = legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
					 base == reg ? 0 : reg);
	}
      else
	abort ();

      if (GET_CODE (offset) == CONST_INT)
	{
	  if (SMALL_INT (offset))
	    return plus_constant_for_output (base, INTVAL (offset));
	  else if (! reload_in_progress && ! reload_completed)
	    offset = force_reg (Pmode, offset);
	  else
	    /* If we reach here, then something is seriously wrong.  */
	    abort ();
	}
      return gen_rtx_PLUS (Pmode, base, offset);
    }
  else if (GET_CODE (orig) == LABEL_REF)
    /* ??? Why do we do this?  */
    /* Now movsi_pic_label_ref uses it, but we ought to be checking that
       the register is live instead, in case it is eliminated.  */
    current_function_uses_pic_offset_table = 1;

  return orig;
}

/* Return the RTX for insns to set the PIC register.  */

static rtx
pic_setup_code ()
{
  rtx seq;

  start_sequence ();
  emit_insn (gen_get_pc (pic_offset_table_rtx, global_offset_table,
			 get_pc_symbol));
  seq = gen_sequence ();
  end_sequence ();

  return seq;
}

/* Emit special PIC prologues and epilogues.  */

void
finalize_pic ()
{
  /* Labels to get the PC in the prologue of this function.  */
  int orig_flag_pic = flag_pic;
  rtx insn;

  if (current_function_uses_pic_offset_table == 0)
    return;

  if (! flag_pic)
    abort ();

  /* If we havn't emitted the special get_pc helper function, do so now.  */
  if (get_pc_symbol_name[0] == 0)
    {
      int align;

      ASM_GENERATE_INTERNAL_LABEL (get_pc_symbol_name, "LGETPC", 0);
      text_section ();

      align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
      if (align > 0)
	ASM_OUTPUT_ALIGN (asm_out_file, align);
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LGETPC", 0);
      fputs ("\tretl\n\tadd %o7,%l7,%l7\n", asm_out_file);
    }

  /* Initialize every time through, since we can't easily
     know this to be permanent.  */
  global_offset_table = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  get_pc_symbol = gen_rtx_SYMBOL_REF (Pmode, get_pc_symbol_name);
  flag_pic = 0;

  emit_insn_after (pic_setup_code (), get_insns ());

  /* Insert the code in each nonlocal goto receiver.
     If you make changes here or to the nonlocal_goto_receiver
     pattern, make sure the unspec_volatile numbers still
     match.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == UNSPEC_VOLATILE
	&& XINT (PATTERN (insn), 1) == 5)
      emit_insn_after (pic_setup_code (), insn);

  flag_pic = orig_flag_pic;

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.
     ??? In the case where we don't obey regdecls, this is not sufficient
     since we may not fall out the bottom.  */
  emit_insn (gen_rtx_USE (VOIDmode, pic_offset_table_rtx));
}

/* Return 1 if RTX is a MEM which is known to be aligned to at
   least an 8 byte boundary.  */

int
mem_min_alignment (mem, desired)
     rtx mem;
     int desired;
{
  rtx addr, base, offset;

  /* If it's not a MEM we can't accept it.  */
  if (GET_CODE (mem) != MEM)
    return 0;

  addr = XEXP (mem, 0);
  base = offset = NULL_RTX;
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  base = XEXP (addr, 0);

	  /* What we are saying here is that if the base
	     REG is aligned properly, the compiler will make
	     sure any REG based index upon it will be so
	     as well.  */
	  if (GET_CODE (XEXP (addr, 1)) == CONST_INT)
	    offset = XEXP (addr, 1);
	  else
	    offset = const0_rtx;
	}
    }
  else if (GET_CODE (addr) == REG)
    {
      base = addr;
      offset = const0_rtx;
    }

  if (base != NULL_RTX)
    {
      int regno = REGNO (base);

      if (regno != FRAME_POINTER_REGNUM
	  && regno != STACK_POINTER_REGNUM)
	{
	  /* Check if the compiler has recorded some information
	     about the alignment of the base REG.  If reload has
	     completed, we already matched with proper alignments.  */
	  if (((cfun != 0 && REGNO_POINTER_ALIGN (regno) >= desired)
	       || reload_completed)
	      && ((INTVAL (offset) & (desired - 1)) == 0))
	    return 1;
	}
      else
	{
	  if (((INTVAL (offset) - SPARC_STACK_BIAS) & (desired - 1)) == 0)
	    return 1;
	}
    }
  else if (! TARGET_UNALIGNED_DOUBLES
	   || CONSTANT_P (addr)
	   || GET_CODE (addr) == LO_SUM)
    {
      /* Anything else we know is properly aligned unless TARGET_UNALIGNED_DOUBLES
	 is true, in which case we can only assume that an access is aligned if
	 it is to a constant address, or the address involves a LO_SUM.  */
      return 1;
    }
  
  /* An obviously unaligned address.  */
  return 0;
}


/* Vectors to keep interesting information about registers where it can easily
   be got.  We use to use the actual mode value as the bit number, but there
   are more than 32 modes now.  Instead we use two tables: one indexed by
   hard register number, and one indexed by mode.  */

/* The purpose of sparc_mode_class is to shrink the range of modes so that
   they all fit (as bit numbers) in a 32 bit word (again).  Each real mode is
   mapped into one sparc_mode_class mode.  */

enum sparc_mode_class {
  S_MODE, D_MODE, T_MODE, O_MODE,
  SF_MODE, DF_MODE, TF_MODE, OF_MODE,
  CC_MODE, CCFP_MODE
};

/* Modes for single-word and smaller quantities.  */
#define S_MODES ((1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-word and smaller quantities.  */
#define D_MODES (S_MODES | (1 << (int) D_MODE) | (1 << DF_MODE))

/* Modes for quad-word and smaller quantities.  */
#define T_MODES (D_MODES | (1 << (int) T_MODE) | (1 << (int) TF_MODE))

/* Modes for 8-word and smaller quantities.  */
#define O_MODES (T_MODES | (1 << (int) O_MODE) | (1 << (int) OF_MODE))

/* Modes for single-float quantities.  We must allow any single word or
   smaller quantity.  This is because the fix/float conversion instructions
   take integer inputs/outputs from the float registers.  */
#define SF_MODES (S_MODES)

/* Modes for double-float and smaller quantities.  */
#define DF_MODES (S_MODES | D_MODES)

/* Modes for double-float only quantities.  */
#define DF_MODES_NO_S (D_MODES)

/* Modes for quad-float only quantities.  */
#define TF_ONLY_MODES (1 << (int) TF_MODE)

/* Modes for quad-float and smaller quantities.  */
#define TF_MODES (DF_MODES | TF_ONLY_MODES)

/* Modes for quad-float and double-float quantities.  */
#define TF_MODES_NO_S (DF_MODES_NO_S | TF_ONLY_MODES)

/* Modes for quad-float pair only quantities.  */
#define OF_ONLY_MODES (1 << (int) OF_MODE)

/* Modes for quad-float pairs and smaller quantities.  */
#define OF_MODES (TF_MODES | OF_ONLY_MODES)

#define OF_MODES_NO_S (TF_MODES_NO_S | OF_ONLY_MODES)

/* Modes for condition codes.  */
#define CC_MODES (1 << (int) CC_MODE)
#define CCFP_MODES (1 << (int) CCFP_MODE)

/* Value is 1 if register/mode pair is acceptable on sparc.
   The funny mixture of D and T modes is because integer operations
   do not specially operate on tetra quantities, so non-quad-aligned
   registers can hold quadword quantities (except %o4 and %i4 because
   they cross fixed registers).  */

/* This points to either the 32 bit or the 64 bit version.  */
int *hard_regno_mode_classes;

static int hard_32bit_mode_classes[] = {
  S_MODES, S_MODES, T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES, D_MODES, S_MODES,

  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,

  /* FP regs f32 to f63.  Only the even numbered registers actually exist,
     and none can hold SFmode/SImode values.  */
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, TF_MODES_NO_S, 0, DF_MODES_NO_S, 0,

  /* %fcc[0123] */
  CCFP_MODES, CCFP_MODES, CCFP_MODES, CCFP_MODES,

  /* %icc */
  CC_MODES
};

static int hard_64bit_mode_classes[] = {
  D_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  O_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  O_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,

  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,

  /* FP regs f32 to f63.  Only the even numbered registers actually exist,
     and none can hold SFmode/SImode values.  */
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, TF_MODES_NO_S, 0, DF_MODES_NO_S, 0,

  /* %fcc[0123] */
  CCFP_MODES, CCFP_MODES, CCFP_MODES, CCFP_MODES,

  /* %icc */
  CC_MODES
};

int sparc_mode_class [NUM_MACHINE_MODES];

enum reg_class sparc_regno_reg_class[FIRST_PSEUDO_REGISTER];

static void
sparc_init_modes ()
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      switch (GET_MODE_CLASS (i))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_COMPLEX_INT:
	  if (GET_MODE_SIZE (i) <= 4)
	    sparc_mode_class[i] = 1 << (int) S_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    sparc_mode_class[i] = 1 << (int) D_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    sparc_mode_class[i] = 1 << (int) T_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    sparc_mode_class[i] = 1 << (int) O_MODE;
	  else 
	    sparc_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (GET_MODE_SIZE (i) <= 4)
	    sparc_mode_class[i] = 1 << (int) SF_MODE;
	  else if (GET_MODE_SIZE (i) == 8)
	    sparc_mode_class[i] = 1 << (int) DF_MODE;
	  else if (GET_MODE_SIZE (i) == 16)
	    sparc_mode_class[i] = 1 << (int) TF_MODE;
	  else if (GET_MODE_SIZE (i) == 32)
	    sparc_mode_class[i] = 1 << (int) OF_MODE;
	  else 
	    sparc_mode_class[i] = 0;
	  break;
	case MODE_CC:
	default:
	  /* mode_class hasn't been initialized yet for EXTRA_CC_MODES, so
	     we must explicitly check for them here.  */
	  if (i == (int) CCFPmode || i == (int) CCFPEmode)
	    sparc_mode_class[i] = 1 << (int) CCFP_MODE;
	  else if (i == (int) CCmode || i == (int) CC_NOOVmode
		   || i == (int) CCXmode || i == (int) CCX_NOOVmode)
	    sparc_mode_class[i] = 1 << (int) CC_MODE;
	  else
	    sparc_mode_class[i] = 0;
	  break;
	}
    }

  if (TARGET_ARCH64)
    hard_regno_mode_classes = hard_64bit_mode_classes;
  else
    hard_regno_mode_classes = hard_32bit_mode_classes;

  /* Initialize the array used by REGNO_REG_CLASS.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i < 16 && TARGET_V8PLUS)
	sparc_regno_reg_class[i] = I64_REGS;
      else if (i < 32)
	sparc_regno_reg_class[i] = GENERAL_REGS;
      else if (i < 64)
	sparc_regno_reg_class[i] = FP_REGS;
      else if (i < 96)
	sparc_regno_reg_class[i] = EXTRA_FP_REGS;
      else if (i < 100)
	sparc_regno_reg_class[i] = FPCC_REGS;
      else
	sparc_regno_reg_class[i] = NO_REGS;
    }
}

/* Save non call used registers from LOW to HIGH at BASE+OFFSET.
   N_REGS is the number of 4-byte regs saved thus far.  This applies even to
   v9 int regs as it simplifies the code.  */

static int
save_regs (file, low, high, base, offset, n_regs, real_offset)
     FILE *file;
     int low, high;
     const char *base;
     int offset;
     int n_regs;
     int real_offset;
{
  int i;

  if (TARGET_ARCH64 && high <= 32)
    {
      for (i = low; i < high; i++)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    {
	      fprintf (file, "\tstx\t%s, [%s+%d]\n",
		       reg_names[i], base, offset + 4 * n_regs);
	      if (dwarf2out_do_frame ())
		dwarf2out_reg_save ("", i, real_offset + 4 * n_regs);
	      n_regs += 2;
	    }
	}
    }
  else
    {
      for (i = low; i < high; i += 2)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    {
	      if (regs_ever_live[i+1] && ! call_used_regs[i+1])
		{
		  fprintf (file, "\tstd\t%s, [%s+%d]\n",
			   reg_names[i], base, offset + 4 * n_regs);
		  if (dwarf2out_do_frame ())
		    {
		      char *l = dwarf2out_cfi_label ();
		      dwarf2out_reg_save (l, i, real_offset + 4 * n_regs);
		      dwarf2out_reg_save (l, i+1, real_offset + 4 * n_regs + 4);
		    }
		  n_regs += 2;
		}
	      else
		{
		  fprintf (file, "\tst\t%s, [%s+%d]\n",
			   reg_names[i], base, offset + 4 * n_regs);
		  if (dwarf2out_do_frame ())
		    dwarf2out_reg_save ("", i, real_offset + 4 * n_regs);
		  n_regs += 2;
		}
	    }
	  else
	    {
	      if (regs_ever_live[i+1] && ! call_used_regs[i+1])
		{
		  fprintf (file, "\tst\t%s, [%s+%d]\n",
			   reg_names[i+1], base, offset + 4 * n_regs + 4);
		  if (dwarf2out_do_frame ())
		    dwarf2out_reg_save ("", i + 1, real_offset + 4 * n_regs + 4);
		  n_regs += 2;
		}
	    }
	}
    }
  return n_regs;
}

/* Restore non call used registers from LOW to HIGH at BASE+OFFSET.

   N_REGS is the number of 4-byte regs saved thus far.  This applies even to
   v9 int regs as it simplifies the code.  */

static int
restore_regs (file, low, high, base, offset, n_regs)
     FILE *file;
     int low, high;
     const char *base;
     int offset;
     int n_regs;
{
  int i;

  if (TARGET_ARCH64 && high <= 32)
    {
      for (i = low; i < high; i++)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    fprintf (file, "\tldx\t[%s+%d], %s\n",
	      base, offset + 4 * n_regs, reg_names[i]),
	    n_regs += 2;
	}
    }
  else
    {
      for (i = low; i < high; i += 2)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	      fprintf (file, "\tldd\t[%s+%d], %s\n",
		       base, offset + 4 * n_regs, reg_names[i]),
	      n_regs += 2;
	    else
	      fprintf (file, "\tld\t[%s+%d],%s\n",
		       base, offset + 4 * n_regs, reg_names[i]),
	      n_regs += 2;
	  else if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	    fprintf (file, "\tld\t[%s+%d],%s\n",
		     base, offset + 4 * n_regs + 4, reg_names[i+1]),
	    n_regs += 2;
	}
    }
  return n_regs;
}

/* Compute the frame size required by the function.  This function is called
   during the reload pass and also by output_function_prologue().  */

int
compute_frame_size (size, leaf_function)
     int size;
     int leaf_function;
{
  int n_regs = 0, i;
  int outgoing_args_size = (current_function_outgoing_args_size
			    + REG_PARM_STACK_SPACE (current_function_decl));

  if (TARGET_EPILOGUE)
    {
      /* N_REGS is the number of 4-byte regs saved thus far.  This applies
	 even to v9 int regs to be consistent with save_regs/restore_regs.  */

      if (TARGET_ARCH64)
	{
	  for (i = 0; i < 8; i++)
	    if (regs_ever_live[i] && ! call_used_regs[i])
	      n_regs += 2;
	}
      else
	{
	  for (i = 0; i < 8; i += 2)
	    if ((regs_ever_live[i] && ! call_used_regs[i])
		|| (regs_ever_live[i+1] && ! call_used_regs[i+1]))
	      n_regs += 2;
	}

      for (i = 32; i < (TARGET_V9 ? 96 : 64); i += 2)
	if ((regs_ever_live[i] && ! call_used_regs[i])
	    || (regs_ever_live[i+1] && ! call_used_regs[i+1]))
	  n_regs += 2;
    }

  /* Set up values for use in `function_epilogue'.  */
  num_gfregs = n_regs;

  if (leaf_function && n_regs == 0
      && size == 0 && current_function_outgoing_args_size == 0)
    {
      actual_fsize = apparent_fsize = 0;
    }
  else
    {
      /* We subtract STARTING_FRAME_OFFSET, remember it's negative.
         The stack bias (if any) is taken out to undo its effects.  */
      apparent_fsize = (size - STARTING_FRAME_OFFSET + SPARC_STACK_BIAS + 7) & -8;
      apparent_fsize += n_regs * 4;
      actual_fsize = apparent_fsize + ((outgoing_args_size + 7) & -8);
    }

  /* Make sure nothing can clobber our register windows.
     If a SAVE must be done, or there is a stack-local variable,
     the register window area must be allocated.
     ??? For v8 we apparently need an additional 8 bytes of reserved space.  */
  if (leaf_function == 0 || size > 0)
    actual_fsize += (16 * UNITS_PER_WORD) + (TARGET_ARCH64 ? 0 : 8);

  return SPARC_STACK_ALIGN (actual_fsize);
}

/* Build a (32 bit) big number in a register.  */
/* ??? We may be able to use the set macro here too.  */

static void
build_big_number (file, num, reg)
     FILE *file;
     int num;
     const char *reg;
{
  if (num >= 0 || ! TARGET_ARCH64)
    {
      fprintf (file, "\tsethi\t%%hi(%d), %s\n", num, reg);
      if ((num & 0x3ff) != 0)
	fprintf (file, "\tor\t%s, %%lo(%d), %s\n", reg, num, reg);
    }
  else /* num < 0 && TARGET_ARCH64 */
    {
      /* Sethi does not sign extend, so we must use a little trickery
	 to use it for negative numbers.  Invert the constant before
	 loading it in, then use xor immediate to invert the loaded bits
	 (along with the upper 32 bits) to the desired constant.  This
	 works because the sethi and immediate fields overlap.  */
      int asize = num;
      int inv = ~asize;
      int low = -0x400 + (asize & 0x3FF);
	  
      fprintf (file, "\tsethi\t%%hi(%d), %s\n\txor\t%s, %d, %s\n",
	       inv, reg, reg, low, reg);
    }
}

/* Output any necessary .register pseudo-ops.  */
void
sparc_output_scratch_registers (file)
     FILE *file ATTRIBUTE_UNUSED;
{
#ifdef HAVE_AS_REGISTER_PSEUDO_OP
  int i;

  if (TARGET_ARCH32)
    return;

  /* Check if %g[2367] were used without
     .register being printed for them already.  */
  for (i = 2; i < 8; i++)
    {
      if (regs_ever_live [i]
	  && ! sparc_hard_reg_printed [i])
	{
	  sparc_hard_reg_printed [i] = 1;
	  fprintf (file, "\t.register\t%%g%d, #scratch\n", i);
	}
      if (i == 3) i = 5;
    }
#endif
}

/* Output code for the function prologue.  */

void
output_function_prologue (file, size, leaf_function)
     FILE *file;
     int size;
     int leaf_function;
{
  sparc_output_scratch_registers (file);

  /* Need to use actual_fsize, since we are also allocating
     space for our callee (and our own register save area).  */
  actual_fsize = compute_frame_size (size, leaf_function);

  if (leaf_function)
    {
      frame_base_name = "%sp";
      frame_base_offset = actual_fsize + SPARC_STACK_BIAS;
    }
  else
    {
      frame_base_name = "%fp";
      frame_base_offset = SPARC_STACK_BIAS;
    }

  /* This is only for the human reader.  */
  fprintf (file, "\t%s#PROLOGUE# 0\n", ASM_COMMENT_START);

  if (actual_fsize == 0)
    /* do nothing.  */ ;
  else if (! leaf_function)
    {
      if (actual_fsize <= 4096)
	fprintf (file, "\tsave\t%%sp, -%d, %%sp\n", actual_fsize);
      else if (actual_fsize <= 8192)
	{
	  fprintf (file, "\tsave\t%%sp, -4096, %%sp\n");
	  fprintf (file, "\tadd\t%%sp, -%d, %%sp\n", actual_fsize - 4096);
	}
      else
	{
	  build_big_number (file, -actual_fsize, "%g1");
	  fprintf (file, "\tsave\t%%sp, %%g1, %%sp\n");
	}
    }
  else /* leaf function */
    {
      if (actual_fsize <= 4096)
	fprintf (file, "\tadd\t%%sp, -%d, %%sp\n", actual_fsize);
      else if (actual_fsize <= 8192)
	{
	  fprintf (file, "\tadd\t%%sp, -4096, %%sp\n");
	  fprintf (file, "\tadd\t%%sp, -%d, %%sp\n", actual_fsize - 4096);
	}
      else
	{
	  build_big_number (file, -actual_fsize, "%g1");
	  fprintf (file, "\tadd\t%%sp, %%g1, %%sp\n");
	}
    }

  if (dwarf2out_do_frame () && actual_fsize)
    {
      char *label = dwarf2out_cfi_label ();

      /* The canonical frame address refers to the top of the frame.  */
      dwarf2out_def_cfa (label, (leaf_function ? STACK_POINTER_REGNUM
				 : FRAME_POINTER_REGNUM),
			 frame_base_offset);

      if (! leaf_function)
	{
	  /* Note the register window save.  This tells the unwinder that
	     it needs to restore the window registers from the previous
	     frame's window save area at 0(cfa).  */
	  dwarf2out_window_save (label);

	  /* The return address (-8) is now in %i7.  */
	  dwarf2out_return_reg (label, 31);
	}
    }

  /* If doing anything with PIC, do it now.  */
  if (! flag_pic)
    fprintf (file, "\t%s#PROLOGUE# 1\n", ASM_COMMENT_START);

  /* Call saved registers are saved just above the outgoing argument area.  */
  if (num_gfregs)
    {
      int offset, real_offset, n_regs;
      const char *base;

      real_offset = -apparent_fsize;
      offset = -apparent_fsize + frame_base_offset;
      if (offset < -4096 || offset + num_gfregs * 4 > 4096)
	{
	  /* ??? This might be optimized a little as %g1 might already have a
	     value close enough that a single add insn will do.  */
	  /* ??? Although, all of this is probably only a temporary fix
	     because if %g1 can hold a function result, then
	     output_function_epilogue will lose (the result will get
	     clobbered).  */
	  build_big_number (file, offset, "%g1");
	  fprintf (file, "\tadd\t%s, %%g1, %%g1\n", frame_base_name);
	  base = "%g1";
	  offset = 0;
	}
      else
	{
	  base = frame_base_name;
	}

      n_regs = 0;
      if (TARGET_EPILOGUE && ! leaf_function)
	/* ??? Originally saved regs 0-15 here.  */
	n_regs = save_regs (file, 0, 8, base, offset, 0, real_offset);
      else if (leaf_function)
	/* ??? Originally saved regs 0-31 here.  */
	n_regs = save_regs (file, 0, 8, base, offset, 0, real_offset);
      if (TARGET_EPILOGUE)
	save_regs (file, 32, TARGET_V9 ? 96 : 64, base, offset, n_regs,
		   real_offset);
    }

  leaf_label = 0;
  if (leaf_function && actual_fsize != 0)
    {
      /* warning ("leaf procedure with frame size %d", actual_fsize); */
      if (! TARGET_EPILOGUE)
	leaf_label = gen_label_rtx ();
    }
}

/* Output code for the function epilogue.  */

void
output_function_epilogue (file, size, leaf_function)
     FILE *file;
     int size ATTRIBUTE_UNUSED;
     int leaf_function;
{
  const char *ret;

  if (leaf_label)
    {
      emit_label_after (leaf_label, get_last_insn ());
      final_scan_insn (get_last_insn (), file, 0, 0, 1);
    }

#ifdef FUNCTION_BLOCK_PROFILER_EXIT
  else if (profile_block_flag == 2)
    {
      FUNCTION_BLOCK_PROFILER_EXIT(file);
    }
#endif

  else if (current_function_epilogue_delay_list == 0)
    {
      /* If code does not drop into the epilogue, we need
	 do nothing except output pending case vectors.  */
      rtx insn = get_last_insn ();                               
      if (GET_CODE (insn) == NOTE)                               
      insn = prev_nonnote_insn (insn);                           
      if (insn && GET_CODE (insn) == BARRIER)                    
      goto output_vectors;                                                    
    }

  /* Restore any call saved registers.  */
  if (num_gfregs)
    {
      int offset, n_regs;
      const char *base;

      offset = -apparent_fsize + frame_base_offset;
      if (offset < -4096 || offset + num_gfregs * 4 > 4096 - 8 /*double*/)
	{
	  build_big_number (file, offset, "%g1");
	  fprintf (file, "\tadd\t%s, %%g1, %%g1\n", frame_base_name);
	  base = "%g1";
	  offset = 0;
	}
      else
	{
	  base = frame_base_name;
	}

      n_regs = 0;
      if (TARGET_EPILOGUE && ! leaf_function)
	/* ??? Originally saved regs 0-15 here.  */
	n_regs = restore_regs (file, 0, 8, base, offset, 0);
      else if (leaf_function)
	/* ??? Originally saved regs 0-31 here.  */
	n_regs = restore_regs (file, 0, 8, base, offset, 0);
      if (TARGET_EPILOGUE)
	restore_regs (file, 32, TARGET_V9 ? 96 : 64, base, offset, n_regs);
    }

  /* Work out how to skip the caller's unimp instruction if required.  */
  if (leaf_function)
    ret = (SKIP_CALLERS_UNIMP_P ? "jmp\t%o7+12" : "retl");
  else
    ret = (SKIP_CALLERS_UNIMP_P ? "jmp\t%i7+12" : "ret");

  if (TARGET_EPILOGUE || leaf_label)
    {
      int old_target_epilogue = TARGET_EPILOGUE;
      target_flags &= ~old_target_epilogue;

      if (! leaf_function)
	{
	  /* If we wound up with things in our delay slot, flush them here.  */
	  if (current_function_epilogue_delay_list)
	    {
	      rtx delay = PATTERN (XEXP (current_function_epilogue_delay_list, 0));

	      if (TARGET_V9 && ! epilogue_renumber (&delay, 1))
		{
		  epilogue_renumber (&delay, 0);
		  fputs (SKIP_CALLERS_UNIMP_P
			 ? "\treturn\t%i7+12\n"
			 : "\treturn\t%i7+8\n", file);
		  final_scan_insn (XEXP (current_function_epilogue_delay_list, 0), file, 1, 0, 0);
		}
	      else
		{
		  rtx insn = emit_jump_insn_after (gen_rtx_RETURN (VOIDmode),
						   get_last_insn ());
		  rtx src;

		  if (GET_CODE (delay) != SET)
		    abort();

		  src = SET_SRC (delay);
		  if (GET_CODE (src) == ASHIFT)
		    {
		      if (XEXP (src, 1) != const1_rtx)
			abort();
		      SET_SRC (delay) = gen_rtx_PLUS (GET_MODE (src), XEXP (src, 0),
						      XEXP (src, 0));
		    }

		  PATTERN (insn) = gen_rtx_PARALLEL (VOIDmode,
					gen_rtvec (2, delay, PATTERN (insn)));
		  final_scan_insn (insn, file, 1, 0, 1);
		}
	    }
	  else if (TARGET_V9 && ! SKIP_CALLERS_UNIMP_P)
	    fputs ("\treturn\t%i7+8\n\tnop\n", file);
	  else
	    fprintf (file, "\t%s\n\trestore\n", ret);
	}
      /* All of the following cases are for leaf functions.  */
      else if (current_function_epilogue_delay_list)
	{
	  /* eligible_for_epilogue_delay_slot ensures that if this is a
	     leaf function, then we will only have insn in the delay slot
	     if the frame size is zero, thus no adjust for the stack is
	     needed here.  */
	  if (actual_fsize != 0)
	    abort ();
	  fprintf (file, "\t%s\n", ret);
	  final_scan_insn (XEXP (current_function_epilogue_delay_list, 0),
			   file, 1, 0, 1);
	}
      /* Output 'nop' instead of 'sub %sp,-0,%sp' when no frame, so as to
	 avoid generating confusing assembly language output.  */
      else if (actual_fsize == 0)
	fprintf (file, "\t%s\n\tnop\n", ret);
      else if (actual_fsize <= 4096)
	fprintf (file, "\t%s\n\tsub\t%%sp, -%d, %%sp\n", ret, actual_fsize);
      else if (actual_fsize <= 8192)
	fprintf (file, "\tsub\t%%sp, -4096, %%sp\n\t%s\n\tsub\t%%sp, -%d, %%sp\n",
		 ret, actual_fsize - 4096);
      else if ((actual_fsize & 0x3ff) == 0)
	fprintf (file, "\tsethi\t%%hi(%d), %%g1\n\t%s\n\tadd\t%%sp, %%g1, %%sp\n",
		 actual_fsize, ret);
      else		 
	fprintf (file, "\tsethi\t%%hi(%d), %%g1\n\tor\t%%g1, %%lo(%d), %%g1\n\t%s\n\tadd\t%%sp, %%g1, %%sp\n",
		 actual_fsize, actual_fsize, ret);
      target_flags |= old_target_epilogue;
    }

 output_vectors:
  sparc_output_deferred_case_vectors ();
}

/* Functions for handling argument passing.

   For v8 the first six args are normally in registers and the rest are
   pushed.  Any arg that starts within the first 6 words is at least
   partially passed in a register unless its data type forbids.

   For v9, the argument registers are laid out as an array of 16 elements
   and arguments are added sequentially.  The first 6 int args and up to the
   first 16 fp args (depending on size) are passed in regs.

   Slot    Stack   Integral   Float   Float in structure   Double   Long Double
   ----    -----   --------   -----   ------------------   ------   -----------
    15   [SP+248]              %f31       %f30,%f31         %d30
    14   [SP+240]              %f29       %f28,%f29         %d28       %q28
    13   [SP+232]              %f27       %f26,%f27         %d26
    12   [SP+224]              %f25       %f24,%f25         %d24       %q24
    11   [SP+216]              %f23       %f22,%f23         %d22
    10   [SP+208]              %f21       %f20,%f21         %d20       %q20
     9   [SP+200]              %f19       %f18,%f19         %d18
     8   [SP+192]              %f17       %f16,%f17         %d16       %q16
     7   [SP+184]              %f15       %f14,%f15         %d14
     6   [SP+176]              %f13       %f12,%f13         %d12       %q12
     5   [SP+168]     %o5      %f11       %f10,%f11         %d10
     4   [SP+160]     %o4       %f9        %f8,%f9           %d8        %q8
     3   [SP+152]     %o3       %f7        %f6,%f7           %d6
     2   [SP+144]     %o2       %f5        %f4,%f5           %d4        %q4
     1   [SP+136]     %o1       %f3        %f2,%f3           %d2
     0   [SP+128]     %o0       %f1        %f0,%f1           %d0        %q0

   Here SP = %sp if -mno-stack-bias or %sp+stack_bias otherwise.

   Integral arguments are always passed as 64 bit quantities appropriately
   extended.

   Passing of floating point values is handled as follows.
   If a prototype is in scope:
     If the value is in a named argument (i.e. not a stdarg function or a
     value not part of the `...') then the value is passed in the appropriate
     fp reg.
     If the value is part of the `...' and is passed in one of the first 6
     slots then the value is passed in the appropriate int reg.
     If the value is part of the `...' and is not passed in one of the first 6
     slots then the value is passed in memory.
   If a prototype is not in scope:
     If the value is one of the first 6 arguments the value is passed in the
     appropriate integer reg and the appropriate fp reg.
     If the value is not one of the first 6 arguments the value is passed in
     the appropriate fp reg and in memory.
   */

/* Maximum number of int regs for args.  */
#define SPARC_INT_ARG_MAX 6
/* Maximum number of fp regs for args.  */
#define SPARC_FP_ARG_MAX 16

#define ROUND_ADVANCE(SIZE) (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Handle the INIT_CUMULATIVE_ARGS macro.
   Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
init_cumulative_args (cum, fntype, libname, indirect)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname ATTRIBUTE_UNUSED;
     int indirect ATTRIBUTE_UNUSED;
{
  cum->words = 0;
  cum->prototype_p = fntype && TYPE_ARG_TYPES (fntype);
  cum->libcall_p = fntype == 0;
}

/* Compute the slot number to pass an argument in.
   Returns the slot number or -1 if passing on the stack.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).
   INCOMING_P is zero for FUNCTION_ARG, nonzero for FUNCTION_INCOMING_ARG.
   *PREGNO records the register number to use if scalar type.
   *PPADDING records the amount of padding needed in words.  */

static int
function_arg_slotno (cum, mode, type, named, incoming_p, pregno, ppadding)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     int incoming_p;
     int *pregno;
     int *ppadding;
{
  int regbase = (incoming_p
		 ? SPARC_INCOMING_INT_ARG_FIRST
		 : SPARC_OUTGOING_INT_ARG_FIRST);
  int slotno = cum->words;
  int regno;

  *ppadding = 0;

  if (type != 0 && TREE_ADDRESSABLE (type))
    return -1;
  if (TARGET_ARCH32
      && type != 0 && mode == BLKmode
      && TYPE_ALIGN (type) % PARM_BOUNDARY != 0)
    return -1;

  switch (mode)
    {
    case VOIDmode :
      /* MODE is VOIDmode when generating the actual call.
	 See emit_call_1.  */
      return -1;

    case QImode : case CQImode :
    case HImode : case CHImode :
    case SImode : case CSImode :
    case DImode : case CDImode :
      if (slotno >= SPARC_INT_ARG_MAX)
	return -1;
      regno = regbase + slotno;
      break;

    case SFmode : case SCmode :
    case DFmode : case DCmode :
    case TFmode : case TCmode :
      if (TARGET_ARCH32)
	{
	  if (slotno >= SPARC_INT_ARG_MAX)
	    return -1;
	  regno = regbase + slotno;
	}
      else
	{
	  if ((mode == TFmode || mode == TCmode)
	      && (slotno & 1) != 0)
	    slotno++, *ppadding = 1;
	  if (TARGET_FPU && named)
	    {
	      if (slotno >= SPARC_FP_ARG_MAX)
		return -1;
	      regno = SPARC_FP_ARG_FIRST + slotno * 2;
	      if (mode == SFmode)
		regno++;
	    }
	  else
	    {
	      if (slotno >= SPARC_INT_ARG_MAX)
		return -1;
	      regno = regbase + slotno;
	    }
	}
      break;

    case BLKmode :
      /* For sparc64, objects requiring 16 byte alignment get it.  */
      if (TARGET_ARCH64)
	{
	  if (type && TYPE_ALIGN (type) == 128 && (slotno & 1) != 0)
	    slotno++, *ppadding = 1;
	}

      if (TARGET_ARCH32
	  || (type && TREE_CODE (type) == UNION_TYPE))
	{
	  if (slotno >= SPARC_INT_ARG_MAX)
	    return -1;
	  regno = regbase + slotno;
	}
      else
	{
	  tree field;
	  int intregs_p = 0, fpregs_p = 0;
	  /* The ABI obviously doesn't specify how packed
	     structures are passed.  These are defined to be passed
	     in int regs if possible, otherwise memory.  */
	  int packed_p = 0;

	  /* First see what kinds of registers we need.  */
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  if (TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
		      && TARGET_FPU)
		    fpregs_p = 1;
		  else
		    intregs_p = 1;
		  if (DECL_PACKED (field))
		    packed_p = 1;
		}
	    }
	  if (packed_p || !named)
	    fpregs_p = 0, intregs_p = 1;

	  /* If all arg slots are filled, then must pass on stack.  */
	  if (fpregs_p && slotno >= SPARC_FP_ARG_MAX)
	    return -1;
	  /* If there are only int args and all int arg slots are filled,
	     then must pass on stack.  */
	  if (!fpregs_p && intregs_p && slotno >= SPARC_INT_ARG_MAX)
	    return -1;
	  /* Note that even if all int arg slots are filled, fp members may
	     still be passed in regs if such regs are available.
	     *PREGNO isn't set because there may be more than one, it's up
	     to the caller to compute them.  */
	  return slotno;
	}
      break;

    default :
      abort ();
    }

  *pregno = regno;
  return slotno;
}

/* Handle recursive register counting for structure field layout.  */

struct function_arg_record_value_parms
{
  rtx ret;
  int slotno, named, regbase;
  int nregs, intoffset;
};

static void function_arg_record_value_3
	PARAMS ((int, struct function_arg_record_value_parms *));
static void function_arg_record_value_2
	PARAMS ((tree, int, struct function_arg_record_value_parms *));
static void function_arg_record_value_1
        PARAMS ((tree, int, struct function_arg_record_value_parms *));
static rtx function_arg_record_value
	PARAMS ((tree, enum machine_mode, int, int, int));

static void
function_arg_record_value_1 (type, startbitpos, parms)
     tree type;
     int startbitpos;
     struct function_arg_record_value_parms *parms;
{
  tree field;

  /* The ABI obviously doesn't specify how packed structures are
     passed.  These are defined to be passed in int regs if possible,
     otherwise memory.  */
  int packed_p = 0;

  /* We need to compute how many registers are needed so we can
     allocate the PARALLEL but before we can do that we need to know
     whether there are any packed fields.  If there are, int regs are
     used regardless of whether there are fp values present.  */
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL && DECL_PACKED (field))
	{
	  packed_p = 1;
	  break;
	}
    }

  /* Compute how many registers we need.  */
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
	{
	  int bitpos = startbitpos;
	  if (DECL_FIELD_BITPOS (field))
	    bitpos += TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));
	  /* ??? FIXME: else assume zero offset.  */

	  if (TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE)
	    {
	      function_arg_record_value_1 (TREE_TYPE (field), bitpos, parms);
	    }
	  else if (TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
	           && TARGET_FPU
	           && ! packed_p
	           && parms->named)
	    {
	      if (parms->intoffset != -1)
		{
		  int intslots, this_slotno;

		  intslots = (bitpos - parms->intoffset + BITS_PER_WORD - 1)
		    / BITS_PER_WORD;
		  this_slotno = parms->slotno + parms->intoffset
		    / BITS_PER_WORD;

		  intslots = MIN (intslots, SPARC_INT_ARG_MAX - this_slotno);
		  intslots = MAX (intslots, 0);
		  parms->nregs += intslots;
		  parms->intoffset = -1;
		}

	      /* There's no need to check this_slotno < SPARC_FP_ARG MAX.
		 If it wasn't true we wouldn't be here.  */
	      parms->nregs += 1;
	    }
	  else
	    {
	      if (parms->intoffset == -1)
		parms->intoffset = bitpos;
	    }
	}
    }
}

/* Handle recursive structure field register assignment.  */

static void 
function_arg_record_value_3 (bitpos, parms)
     int bitpos;
     struct function_arg_record_value_parms *parms;
{
  enum machine_mode mode;
  int regno, this_slotno, intslots, intoffset;
  rtx reg;

  if (parms->intoffset == -1)
    return;
  intoffset = parms->intoffset;
  parms->intoffset = -1;

  intslots = (bitpos - intoffset + BITS_PER_WORD - 1) / BITS_PER_WORD;
  this_slotno = parms->slotno + intoffset / BITS_PER_WORD;

  intslots = MIN (intslots, SPARC_INT_ARG_MAX - this_slotno);
  if (intslots <= 0)
    return;

  /* If this is the trailing part of a word, only load that much into
     the register.  Otherwise load the whole register.  Note that in
     the latter case we may pick up unwanted bits.  It's not a problem
     at the moment but may wish to revisit.  */

  if (intoffset % BITS_PER_WORD != 0)
    {
      mode = mode_for_size (BITS_PER_WORD - intoffset%BITS_PER_WORD,
			    MODE_INT, 0);
    }
  else
    mode = word_mode;

  intoffset /= BITS_PER_UNIT;
  do
    {
      regno = parms->regbase + this_slotno;
      reg = gen_rtx_REG (mode, regno);
      XVECEXP (parms->ret, 0, parms->nregs)
	= gen_rtx_EXPR_LIST (VOIDmode, reg, GEN_INT (intoffset));

      this_slotno += 1;
      intoffset = (intoffset | (UNITS_PER_WORD-1)) + 1;
      parms->nregs += 1;
      intslots -= 1;
    }
  while (intslots > 0);
}

static void
function_arg_record_value_2 (type, startbitpos, parms)
     tree type;
     int startbitpos;
     struct function_arg_record_value_parms *parms;
{
  tree field;
  int packed_p = 0;

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL && DECL_PACKED (field))
	{
	  packed_p = 1;
	  break;
	}
    }

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL)
	{
	  int bitpos = startbitpos;
	  if (DECL_FIELD_BITPOS (field))
	    bitpos += TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));
	  /* ??? FIXME: else assume zero offset.  */

	  if (TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE)
	    {
	      function_arg_record_value_2 (TREE_TYPE (field), bitpos, parms);
	    }
	  else if (TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
	           && TARGET_FPU
	           && ! packed_p
	           && parms->named)
	    {
	      int this_slotno = parms->slotno + bitpos / BITS_PER_WORD;
	      rtx reg;

	      function_arg_record_value_3 (bitpos, parms);

	      reg = gen_rtx_REG (DECL_MODE (field),
			         (SPARC_FP_ARG_FIRST + this_slotno * 2
			          + (DECL_MODE (field) == SFmode
				     && (bitpos & 32) != 0)));
	      XVECEXP (parms->ret, 0, parms->nregs)
		= gen_rtx_EXPR_LIST (VOIDmode, reg,
			   GEN_INT (bitpos / BITS_PER_UNIT));
	      parms->nregs += 1;
	    }
	  else
	    {
	      if (parms->intoffset == -1)
		parms->intoffset = bitpos;
	    }
	}
    }
}

static rtx
function_arg_record_value (type, mode, slotno, named, regbase)
     tree type;
     enum machine_mode mode;
     int slotno, named, regbase;
{
  HOST_WIDE_INT typesize = int_size_in_bytes (type);
  struct function_arg_record_value_parms parms;
  int nregs;

  parms.ret = NULL_RTX;
  parms.slotno = slotno;
  parms.named = named;
  parms.regbase = regbase;

  /* Compute how many registers we need.  */
  parms.nregs = 0;
  parms.intoffset = 0;
  function_arg_record_value_1 (type, 0, &parms);

  if (parms.intoffset != -1)
    {
      int intslots, this_slotno;

      intslots = (typesize*BITS_PER_UNIT - parms.intoffset + BITS_PER_WORD - 1)
	/ BITS_PER_WORD;
      this_slotno = slotno + parms.intoffset / BITS_PER_WORD;

      intslots = MIN (intslots, SPARC_INT_ARG_MAX - this_slotno);
      intslots = MAX (intslots, 0);

      parms.nregs += intslots;
    }
  nregs = parms.nregs;

  /* Allocate the vector and handle some annoying special cases.  */
  if (nregs == 0)
    {
      /* ??? Empty structure has no value?  Duh?  */
      if (typesize <= 0)
	{
	  /* Though there's nothing really to store, return a word register
	     anyway so the rest of gcc doesn't go nuts.  Returning a PARALLEL
	     leads to breakage due to the fact that there are zero bytes to
	     load.  */
	  return gen_rtx_REG (mode, regbase);
	}
      else
	{
	  /* ??? C++ has structures with no fields, and yet a size.  Give up
	     for now and pass everything back in integer registers.  */
	  nregs = (typesize + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
	}
      if (nregs + slotno > SPARC_INT_ARG_MAX)
	nregs = SPARC_INT_ARG_MAX - slotno;
    }
  if (nregs == 0)
    abort ();

  parms.ret = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));

  /* Fill in the entries.  */
  parms.nregs = 0;
  parms.intoffset = 0;
  function_arg_record_value_2 (type, 0, &parms);
  function_arg_record_value_3 (typesize * BITS_PER_UNIT, &parms);

  if (parms.nregs != nregs)
    abort ();

  return parms.ret;
}

/* Handle the FUNCTION_ARG macro.
   Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).
   INCOMING_P is zero for FUNCTION_ARG, nonzero for FUNCTION_INCOMING_ARG.  */

rtx
function_arg (cum, mode, type, named, incoming_p)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     int incoming_p;
{
  int regbase = (incoming_p
		 ? SPARC_INCOMING_INT_ARG_FIRST
		 : SPARC_OUTGOING_INT_ARG_FIRST);
  int slotno, regno, padding;
  rtx reg;

  slotno = function_arg_slotno (cum, mode, type, named, incoming_p,
				&regno, &padding);

  if (slotno == -1)
    return 0;

  if (TARGET_ARCH32)
    {
      reg = gen_rtx_REG (mode, regno);
      return reg;
    }

  /* v9 fp args in reg slots beyond the int reg slots get passed in regs
     but also have the slot allocated for them.
     If no prototype is in scope fp values in register slots get passed
     in two places, either fp regs and int regs or fp regs and memory.  */
  if ((GET_MODE_CLASS (mode) == MODE_FLOAT
       || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
      && SPARC_FP_REG_P (regno))
    {
      reg = gen_rtx_REG (mode, regno);
      if (cum->prototype_p || cum->libcall_p)
	{
	  /* "* 2" because fp reg numbers are recorded in 4 byte
	     quantities.  */
#if 0
	  /* ??? This will cause the value to be passed in the fp reg and
	     in the stack.  When a prototype exists we want to pass the
	     value in the reg but reserve space on the stack.  That's an
	     optimization, and is deferred [for a bit].  */
	  if ((regno - SPARC_FP_ARG_FIRST) >= SPARC_INT_ARG_MAX * 2)
	    return gen_rtx_PARALLEL (mode,
			    gen_rtvec (2,
				       gen_rtx_EXPR_LIST (VOIDmode,
						NULL_RTX, const0_rtx),
				       gen_rtx_EXPR_LIST (VOIDmode,
						reg, const0_rtx)));
	  else
#else
	  /* ??? It seems that passing back a register even when past
	     the area declared by REG_PARM_STACK_SPACE will allocate
	     space appropriately, and will not copy the data onto the
	     stack, exactly as we desire.

	     This is due to locate_and_pad_parm being called in
	     expand_call whenever reg_parm_stack_space > 0, which
	     while benefical to our example here, would seem to be
	     in error from what had been intended.  Ho hum...  -- r~ */
#endif
	    return reg;
	}
      else
	{
	  rtx v0, v1;

	  if ((regno - SPARC_FP_ARG_FIRST) < SPARC_INT_ARG_MAX * 2)
	    {
	      int intreg;

	      /* On incoming, we don't need to know that the value
		 is passed in %f0 and %i0, and it confuses other parts
		 causing needless spillage even on the simplest cases.  */
	      if (incoming_p)
		return reg;

	      intreg = (SPARC_OUTGOING_INT_ARG_FIRST
			+ (regno - SPARC_FP_ARG_FIRST) / 2);

	      v0 = gen_rtx_EXPR_LIST (VOIDmode, reg, const0_rtx);
	      v1 = gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_REG (mode, intreg),
				      const0_rtx);
	      return gen_rtx_PARALLEL (mode, gen_rtvec (2, v0, v1));
	    }
	  else
	    {
	      v0 = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
	      v1 = gen_rtx_EXPR_LIST (VOIDmode, reg, const0_rtx);
	      return gen_rtx_PARALLEL (mode, gen_rtvec (2, v0, v1));
	    }
	}
    }
  else if (type && TREE_CODE (type) == RECORD_TYPE)
    {
      /* Structures up to 16 bytes in size are passed in arg slots on the
	 stack and are promoted to registers where possible.  */

      if (int_size_in_bytes (type) > 16)
	abort (); /* shouldn't get here */

      return function_arg_record_value (type, mode, slotno, named, regbase);
    }
  else if (type && TREE_CODE (type) == UNION_TYPE)
    {
      enum machine_mode mode;
      int bytes = int_size_in_bytes (type);

      if (bytes > 16)
	abort ();

      mode = mode_for_size (bytes * BITS_PER_UNIT, MODE_INT, 0);
      reg = gen_rtx_REG (mode, regno);
    }
  else
    {
      /* Scalar or complex int.  */
      reg = gen_rtx_REG (mode, regno);
    }

  return reg;
}

/* Handle the FUNCTION_ARG_PARTIAL_NREGS macro.
   For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.

   Any arg that starts in the first 6 regs but won't entirely fit in them
   needs partial registers on v8.  On v9, structures with integer
   values in arg slots 5,6 will be passed in %o5 and SP+176, and complex fp
   values that begin in the last fp reg [where "last fp reg" varies with the
   mode] will be split between that reg and memory.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int slotno, regno, padding;

  /* We pass 0 for incoming_p here, it doesn't matter.  */
  slotno = function_arg_slotno (cum, mode, type, named, 0, &regno, &padding);

  if (slotno == -1)
    return 0;

  if (TARGET_ARCH32)
    {
      if ((slotno + (mode == BLKmode
		     ? ROUND_ADVANCE (int_size_in_bytes (type))
		     : ROUND_ADVANCE (GET_MODE_SIZE (mode))))
	  > NPARM_REGS (SImode))
	return NPARM_REGS (SImode) - slotno;
      return 0;
    }
  else
    {
      if (type && AGGREGATE_TYPE_P (type))
	{
	  int size = int_size_in_bytes (type);
	  int align = TYPE_ALIGN (type);

	  if (align == 16)
	    slotno += slotno & 1;
	  if (size > 8 && size <= 16
	      && slotno == SPARC_INT_ARG_MAX - 1)
	    return 1;
	}
      else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_INT
	       || (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
		   && ! TARGET_FPU))
	{
	  if (GET_MODE_ALIGNMENT (mode) == 128)
	    {
	      slotno += slotno & 1;
	      if (slotno == SPARC_INT_ARG_MAX - 2)
		return 1;
	    }
	  else
	    {
	      if (slotno == SPARC_INT_ARG_MAX - 1)
		return 1;
	    }
	}
      else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	{
	  if (GET_MODE_ALIGNMENT (mode) == 128)
	    slotno += slotno & 1;
	  if ((slotno + GET_MODE_SIZE (mode) / UNITS_PER_WORD)
	      > SPARC_FP_ARG_MAX)
	    return 1;
	}
      return 0;
    }
}

/* Handle the FUNCTION_ARG_PASS_BY_REFERENCE macro.
   !v9: The SPARC ABI stipulates passing struct arguments (of any size) and
   quad-precision floats by invisible reference.
   v9: Aggregates greater than 16 bytes are passed by reference.
   For Pascal, also pass arrays by reference.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     const CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if (TARGET_ARCH32)
    {
      return ((type && AGGREGATE_TYPE_P (type))
	      || mode == TFmode || mode == TCmode);
    }
  else
    {
      return ((type && TREE_CODE (type) == ARRAY_TYPE)
	      /* Consider complex values as aggregates, so care for TCmode. */
	      || GET_MODE_SIZE (mode) > 16
	      || (type && AGGREGATE_TYPE_P (type)
		  && int_size_in_bytes (type) > 16));
    }
}

/* Handle the FUNCTION_ARG_ADVANCE macro.
   Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   TYPE is null for libcalls where that information may not be available.  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int slotno, regno, padding;

  /* We pass 0 for incoming_p here, it doesn't matter.  */
  slotno = function_arg_slotno (cum, mode, type, named, 0, &regno, &padding);

  /* If register required leading padding, add it.  */
  if (slotno != -1)
    cum->words += padding;

  if (TARGET_ARCH32)
    {
      cum->words += (mode != BLKmode
		     ? ROUND_ADVANCE (GET_MODE_SIZE (mode))
		     : ROUND_ADVANCE (int_size_in_bytes (type)));
    }
  else
    {
      if (type && AGGREGATE_TYPE_P (type))
	{
	  int size = int_size_in_bytes (type);

	  if (size <= 8)
	    ++cum->words;
	  else if (size <= 16)
	    cum->words += 2;
	  else /* passed by reference */
	    ++cum->words;
	}
      else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
	{
	  cum->words += 2;
	}
      else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	{
	  cum->words += GET_MODE_SIZE (mode) / UNITS_PER_WORD;
	}
      else
	{
	  cum->words += (mode != BLKmode
			 ? ROUND_ADVANCE (GET_MODE_SIZE (mode))
			 : ROUND_ADVANCE (int_size_in_bytes (type)));
	}
    }
}

/* Handle the FUNCTION_ARG_PADDING macro.
   For the 64 bit ABI structs are always stored left shifted in their
   argument slot.  */

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  if (TARGET_ARCH64 && type != 0 && AGGREGATE_TYPE_P (type))
    return upward;

  /* This is the default definition.  */
  return (! BYTES_BIG_ENDIAN
	  ? upward
	  : ((mode == BLKmode
	      ? (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
		 && int_size_in_bytes (type) < (PARM_BOUNDARY / BITS_PER_UNIT))
	      : GET_MODE_BITSIZE (mode) < PARM_BOUNDARY)
	     ? downward : upward));
}

/* Handle FUNCTION_VALUE, FUNCTION_OUTGOING_VALUE, and LIBCALL_VALUE macros.
   For v9, function return values are subject to the same rules as arguments,
   except that up to 32-bytes may be returned in registers.  */

rtx
function_value (type, mode, incoming_p)
     tree type;
     enum machine_mode mode;
     int incoming_p;
{
  int regno;
  int regbase = (incoming_p
		 ? SPARC_OUTGOING_INT_ARG_FIRST
		 : SPARC_INCOMING_INT_ARG_FIRST);

  if (TARGET_ARCH64 && type)
    {
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  /* Structures up to 32 bytes in size are passed in registers,
	     promoted to fp registers where possible.  */

	  if (int_size_in_bytes (type) > 32)
	    abort (); /* shouldn't get here */

	  return function_arg_record_value (type, mode, 0, 1, regbase);
	}
      else if (TREE_CODE (type) == UNION_TYPE)
	{
	  int bytes = int_size_in_bytes (type);

	  if (bytes > 32)
	    abort ();

	  mode = mode_for_size (bytes * BITS_PER_UNIT, MODE_INT, 0);
	}
    }
    
  if (TARGET_ARCH64
      && GET_MODE_CLASS (mode) == MODE_INT 
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD
      && type && TREE_CODE (type) != UNION_TYPE)
    mode = DImode;

  if (incoming_p)
    regno = BASE_RETURN_VALUE_REG (mode);
  else
    regno = BASE_OUTGOING_VALUE_REG (mode);

  return gen_rtx_REG (mode, regno);
}

/* Do what is necessary for `va_start'.  We look at the current function
   to determine if stdarg or varargs is used and return the address of
   the first unnamed parameter.  */

rtx
sparc_builtin_saveregs ()
{
  int first_reg = current_function_args_info.words;
  rtx address;
  int regno;

  for (regno = first_reg; regno < NPARM_REGS (word_mode); regno++)
    emit_move_insn (gen_rtx_MEM (word_mode,
			     gen_rtx_PLUS (Pmode,
				      frame_pointer_rtx,
				      GEN_INT (STACK_POINTER_OFFSET
					       + UNITS_PER_WORD * regno))),
		    gen_rtx_REG (word_mode,
			     BASE_INCOMING_ARG_REG (word_mode) + regno));

  address = gen_rtx_PLUS (Pmode,
		     frame_pointer_rtx,
		     GEN_INT (STACK_POINTER_OFFSET
			      + UNITS_PER_WORD * first_reg));

  if (current_function_check_memory_usage
      && first_reg < NPARM_REGS (word_mode))
    emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
		       address, ptr_mode,
		       GEN_INT (UNITS_PER_WORD 
			 	* (NPARM_REGS (word_mode) - first_reg)),
		       TYPE_MODE (sizetype), GEN_INT (MEMORY_USE_RW),
		       TYPE_MODE (integer_type_node));

  return address;
}

/* Implement `va_start' for varargs and stdarg.  */

void
sparc_va_start (stdarg_p, valist, nextarg)
     int stdarg_p ATTRIBUTE_UNUSED;
     tree valist;
     rtx nextarg;
{
  nextarg = expand_builtin_saveregs ();
  std_expand_builtin_va_start (1, valist, nextarg);
}

/* Implement `va_arg'.  */

rtx
sparc_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT size, rsize, align;
  tree addr, incr;
  rtx addr_rtx;
  int indirect = 0;

  /* Round up sizeof(type) to a word.  */
  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;
  align = 0;

  if (TARGET_ARCH64)
    {
      if (TYPE_ALIGN (type) >= 2 * (unsigned) BITS_PER_WORD)
	align = 2 * UNITS_PER_WORD;

      if (AGGREGATE_TYPE_P (type))
	{
	  if (size > 16)
	    {
	      indirect = 1;
	      size = rsize = UNITS_PER_WORD;
	    }
	  else
	    size = rsize;
	}
    }
  else
    {
      if (AGGREGATE_TYPE_P (type)
	  || TYPE_MODE (type) == TFmode
	  || TYPE_MODE (type) == TCmode)
	{
	  indirect = 1;
	  size = rsize = UNITS_PER_WORD;
	}
      else
	{
	  /* ??? The old va-sparc.h implementation, for 8 byte objects
	     copied stuff to a temporary -- I don't see that that 
	     provides any more alignment than the stack slot did.  */
	}
    }

  incr = valist;
  if (align)
    {
      incr = fold (build (PLUS_EXPR, ptr_type_node, incr,
			 build_int_2 (align - 1, 0)));
      incr = fold (build (BIT_AND_EXPR, ptr_type_node, incr,
			  build_int_2 (-align, -1)));
    }

  addr = incr = save_expr (incr);
  if (BYTES_BIG_ENDIAN && size < rsize)
    {
      addr = fold (build (PLUS_EXPR, ptr_type_node, incr,
			  build_int_2 (rsize - size, 0)));
    }
  incr = fold (build (PLUS_EXPR, ptr_type_node, incr,
		      build_int_2 (rsize, 0)));

  incr = build (MODIFY_EXPR, ptr_type_node, valist, incr);
  TREE_SIDE_EFFECTS (incr) = 1;
  expand_expr (incr, const0_rtx, VOIDmode, EXPAND_NORMAL);

  addr_rtx = expand_expr (addr, NULL, Pmode, EXPAND_NORMAL);

  if (indirect)
    {
      addr_rtx = force_reg (Pmode, addr_rtx);
      addr_rtx = gen_rtx_MEM (Pmode, addr_rtx);
      MEM_ALIAS_SET (addr_rtx) = get_varargs_alias_set ();
    }

  return addr_rtx;
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label.  OP is the conditional expression.
   XEXP (OP, 0) is assumed to be a condition code register (integer or
   floating point) and its mode specifies what kind of comparison we made.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   ANNUL is non-zero if we should generate an annulling branch.

   NOOP is non-zero if we have to follow this branch by a noop.

   INSN, if set, is the insn.  */

char *
output_cbranch (op, label, reversed, annul, noop, insn)
     rtx op;
     int label;
     int reversed, annul, noop;
     rtx insn;
{
  static char string[32];
  enum rtx_code code = GET_CODE (op);
  rtx cc_reg = XEXP (op, 0);
  enum machine_mode mode = GET_MODE (cc_reg);
  static char v8_labelno[] = "%lX";
  static char v9_icc_labelno[] = "%%icc, %lX";
  static char v9_xcc_labelno[] = "%%xcc, %lX";
  static char v9_fcc_labelno[] = "%%fccX, %lY";
  char *labelno;
  const char *branch;
  int labeloff, spaces = 8;

  if (reversed)
    {
      /* Reversal of FP compares takes care -- an ordered compare
	 becomes an unordered compare and vice versa.  */
      if (mode == CCFPmode || mode == CCFPEmode)
	code = reverse_condition_maybe_unordered (code);
      else
	code = reverse_condition (code);
    }

  /* Start by writing the branch condition.  */
  if (mode == CCFPmode || mode == CCFPEmode)
    {
      switch (code)
	{
	case NE:
	  branch = "fbne";
	  break;
	case EQ:
	  branch = "fbe";
	  break;
	case GE:
	  branch = "fbge";
	  break;
	case GT:
	  branch = "fbg";
	  break;
	case LE:
	  branch = "fble";
	  break;
	case LT:
	  branch = "fbl";
	  break;
	case UNORDERED:
	  branch = "fbu";
	  break;
	case ORDERED:
	  branch = "fbo";
	  break;
	case UNGT:
	  branch = "fbug";
	  break;
	case UNLT:
	  branch = "fbul";
	  break;
	case UNEQ:
	  branch = "fbue";
	  break;
	case UNGE:
	  branch = "fbuge";
	  break;
	case UNLE:
	  branch = "fbule";
	  break;
	case LTGT:
	  branch = "fblg";
	  break;

	default:
	  abort ();
	}

      /* ??? !v9: FP branches cannot be preceded by another floating point
	 insn.  Because there is currently no concept of pre-delay slots,
	 we can fix this only by always emitting a nop before a floating
	 point branch.  */

      string[0] = '\0';
      if (! TARGET_V9)
	strcpy (string, "nop\n\t");
      strcat (string, branch);
    }
  else
    {
      switch (code)
	{
	case NE:
	  branch = "bne";
	  break;
	case EQ:
	  branch = "be";
	  break;
	case GE:
	  if (mode == CC_NOOVmode)
	    branch = "bpos";
	  else
	    branch = "bge";
	  break;
	case GT:
	  branch = "bg";
	  break;
	case LE:
	  branch = "ble";
	  break;
	case LT:
	  if (mode == CC_NOOVmode)
	    branch = "bneg";
	  else
	    branch = "bl";
	  break;
	case GEU:
	  branch = "bgeu";
	  break;
	case GTU:
	  branch = "bgu";
	  break;
	case LEU:
	  branch = "bleu";
	  break;
	case LTU:
	  branch = "blu";
	  break;

	default:
	  abort ();
	}
      strcpy (string, branch);
    }
  spaces -= strlen (branch);

  /* Now add the annulling, the label, and a possible noop.  */
  if (annul)
    {
      strcat (string, ",a");
      spaces -= 2;
    }

  if (! TARGET_V9)
    {
      labeloff = 2;
      labelno = v8_labelno;
    }
  else
    {
      rtx note;

      if (insn && (note = find_reg_note (insn, REG_BR_PRED, NULL_RTX)))
	{
	  strcat (string,
		  INTVAL (XEXP (note, 0)) & ATTR_FLAG_likely ? ",pt" : ",pn");
	  spaces -= 3;
	}

      labeloff = 9;
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  labeloff = 10;
	  labelno = v9_fcc_labelno;
	  /* Set the char indicating the number of the fcc reg to use.  */
	  labelno[5] = REGNO (cc_reg) - SPARC_FIRST_V9_FCC_REG + '0';
	}
      else if (mode == CCXmode || mode == CCX_NOOVmode)
	labelno = v9_xcc_labelno;
      else
	labelno = v9_icc_labelno;
    }
  /* Set the char indicating the number of the operand containing the
     label_ref.  */
  labelno[labeloff] = label + '0';
  if (spaces > 0)
    strcat (string, "\t");
  else
    strcat (string, " ");
  strcat (string, labelno);

  if (noop)
    strcat (string, "\n\tnop");

  return string;
}

/* Emit a library call comparison between floating point X and Y.
   COMPARISON is the rtl operator to compare with (EQ, NE, GT, etc.).
   TARGET_ARCH64 uses _Qp_* functions, which use pointers to TFmode
   values as arguments instead of the TFmode registers themselves,
   that's why we cannot call emit_float_lib_cmp.  */
void
sparc_emit_float_lib_cmp (x, y, comparison)
     rtx x, y;
     enum rtx_code comparison;
{
  char *qpfunc;
  rtx slot0, slot1, result, tem, tem2;
  enum machine_mode mode;

  switch (comparison)
    {
    case EQ:
      qpfunc = (TARGET_ARCH64) ? "_Qp_feq" : "_Q_feq";
      break;

    case NE:
      qpfunc = (TARGET_ARCH64) ? "_Qp_fne" : "_Q_fne";
      break;

    case GT:
      qpfunc = (TARGET_ARCH64) ? "_Qp_fgt" : "_Q_fgt";
      break;

    case GE:
      qpfunc = (TARGET_ARCH64) ? "_Qp_fge" : "_Q_fge";
      break;

    case LT:
      qpfunc = (TARGET_ARCH64) ? "_Qp_flt" : "_Q_flt";
      break;

    case LE:
      qpfunc = (TARGET_ARCH64) ? "_Qp_fle" : "_Q_fle";
      break;

    case ORDERED:
    case UNORDERED:
    case UNGT:
    case UNLT:
    case UNEQ:
    case UNGE:
    case UNLE:
    case LTGT:
      qpfunc = (TARGET_ARCH64) ? "_Qp_cmp" : "_Q_cmp";
      break;

    default:
      abort();
      break;
    }

  if (TARGET_ARCH64)
    {
      if (GET_CODE (x) != MEM)
	{
	  slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot0, x));
	}
      else
	slot0 = x;

      if (GET_CODE (y) != MEM)
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot1, y));
	}
      else
	slot1 = y;

      emit_library_call (gen_rtx_SYMBOL_REF (Pmode, qpfunc), 1,
			 DImode, 2,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode);

      mode = DImode;
    }
  else
    {
      emit_library_call (gen_rtx_SYMBOL_REF (Pmode, qpfunc), 1,
			 SImode, 2,
			 x, TFmode, y, TFmode);

      mode = SImode;
    }


  /* Immediately move the result of the libcall into a pseudo
     register so reload doesn't clobber the value if it needs
     the return register for a spill reg.  */
  result = gen_reg_rtx (mode);
  emit_move_insn (result, hard_libcall_value (mode));

  switch (comparison)
    {
    default:
      emit_cmp_insn (result, const0_rtx, NE,
		     NULL_RTX, mode, 0, 0);
      break;
    case ORDERED:
    case UNORDERED:
      emit_cmp_insn (result, GEN_INT(3),
		     (comparison == UNORDERED) ? EQ : NE,
		     NULL_RTX, mode, 0, 0);
      break;
    case UNGT:
    case UNGE:
      emit_cmp_insn (result, const1_rtx,
		     (comparison == UNGT) ? GT : NE,
		     NULL_RTX, mode, 0, 0);
      break;
    case UNLE:
      emit_cmp_insn (result, const2_rtx, NE,
		     NULL_RTX, mode, 0, 0);
      break;
    case UNLT:
      tem = gen_reg_rtx (mode);
      if (TARGET_ARCH32)
	emit_insn (gen_andsi3 (tem, result, const1_rtx));
      else
	emit_insn (gen_anddi3 (tem, result, const1_rtx));
      emit_cmp_insn (tem, const0_rtx, NE,
		     NULL_RTX, mode, 0, 0);
      break;
    case UNEQ:
    case LTGT:
      tem = gen_reg_rtx (mode);
      if (TARGET_ARCH32)
	emit_insn (gen_addsi3 (tem, result, const1_rtx));
      else
	emit_insn (gen_adddi3 (tem, result, const1_rtx));
      tem2 = gen_reg_rtx (mode);
      if (TARGET_ARCH32)
	emit_insn (gen_andsi3 (tem2, tem, const2_rtx));
      else
	emit_insn (gen_anddi3 (tem2, tem, const2_rtx));
      emit_cmp_insn (tem2, const0_rtx,
		     (comparison == UNEQ) ? EQ : NE,
		     NULL_RTX, mode, 0, 0);
      break;
    }
}

/* Return the string to output a conditional branch to LABEL, testing
   register REG.  LABEL is the operand number of the label; REG is the
   operand number of the reg.  OP is the conditional expression.  The mode
   of REG says what kind of comparison we made.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   ANNUL is non-zero if we should generate an annulling branch.

   NOOP is non-zero if we have to follow this branch by a noop.  */

char *
output_v9branch (op, reg, label, reversed, annul, noop, insn)
     rtx op;
     int reg, label;
     int reversed, annul, noop;
     rtx insn;
{
  static char string[20];
  enum rtx_code code = GET_CODE (op);
  enum machine_mode mode = GET_MODE (XEXP (op, 0));
  static char labelno[] = "%X, %lX";
  rtx note;
  int spaces = 8;

  /* If not floating-point or if EQ or NE, we can just reverse the code.  */
  if (reversed)
    code = reverse_condition (code), reversed = 0;

  /* Only 64 bit versions of these instructions exist.  */
  if (mode != DImode)
    abort ();

  /* Start by writing the branch condition.  */

  switch (code)
    {
    case NE:
      strcpy (string, "brnz");
      spaces -= 4;
      break;

    case EQ:
      strcpy (string, "brz");
      spaces -= 3;
      break;

    case GE:
      strcpy (string, "brgez");
      spaces -= 5;
      break;

    case LT:
      strcpy (string, "brlz");
      spaces -= 4;
      break;

    case LE:
      strcpy (string, "brlez");
      spaces -= 5;
      break;

    case GT:
      strcpy (string, "brgz");
      spaces -= 4;
      break;

    default:
      abort ();
    }

  /* Now add the annulling, reg, label, and nop.  */
  if (annul)
    {
      strcat (string, ",a");
      spaces -= 2;
    }

  if (insn && (note = find_reg_note (insn, REG_BR_PRED, NULL_RTX)))
    {
      strcat (string,
	      INTVAL (XEXP (note, 0)) & ATTR_FLAG_likely ? ",pt" : ",pn");
      spaces -= 3;
    }

  labelno[1] = reg + '0';
  labelno[6] = label + '0';
  if (spaces > 0)
    strcat (string, "\t");
  else
    strcat (string, " ");
  strcat (string, labelno);

  if (noop)
    strcat (string, "\n\tnop");

  return string;
}

/* Return 1, if any of the registers of the instruction are %l[0-7] or %o[0-7].
   Such instructions cannot be used in the delay slot of return insn on v9.
   If TEST is 0, also rename all %i[0-7] registers to their %o[0-7] counterparts.
 */

static int
epilogue_renumber (where, test)
     register rtx *where;
     int test;
{
  register const char *fmt;
  register int i;
  register enum rtx_code code;

  if (*where == 0)
    return 0;

  code = GET_CODE (*where);

  switch (code)
    {
    case REG:
      if (REGNO (*where) >= 8 && REGNO (*where) < 24)      /* oX or lX */
	return 1;
      if (! test && REGNO (*where) >= 24 && REGNO (*where) < 32)
	*where = gen_rtx (REG, GET_MODE (*where), OUTGOING_REGNO (REGNO(*where)));
    case SCRATCH:
    case CC0:
    case PC:
    case CONST_INT:
    case CONST_DOUBLE:
      return 0;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (*where, i) - 1; j >= 0; j--)
	    if (epilogue_renumber (&(XVECEXP (*where, i, j)), test))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && epilogue_renumber (&(XEXP (*where, i)), test))
	return 1;
    }
  return 0;
}

/* Output assembler code to return from a function.  */

const char *
output_return (operands)
     rtx *operands;
{
  rtx delay = final_sequence ? XVECEXP (final_sequence, 0, 1) : 0;

  if (leaf_label)
    {
      operands[0] = leaf_label;
      return "b%* %l0%(";
    }
  else if (current_function_uses_only_leaf_regs)
    {
      /* No delay slot in a leaf function.  */
      if (delay)
	abort ();

      /* If we didn't allocate a frame pointer for the current function,
	 the stack pointer might have been adjusted.  Output code to
	 restore it now.  */

      operands[0] = GEN_INT (actual_fsize);

      /* Use sub of negated value in first two cases instead of add to
	 allow actual_fsize == 4096.  */

      if (actual_fsize <= 4096)
	{
	  if (SKIP_CALLERS_UNIMP_P)
	    return "jmp\t%%o7+12\n\tsub\t%%sp, -%0, %%sp";
	  else
	    return "retl\n\tsub\t%%sp, -%0, %%sp";
	}
      else if (actual_fsize <= 8192)
	{
	  operands[0] = GEN_INT (actual_fsize - 4096);
	  if (SKIP_CALLERS_UNIMP_P)
	    return "sub\t%%sp, -4096, %%sp\n\tjmp\t%%o7+12\n\tsub\t%%sp, -%0, %%sp";
	  else
	    return "sub\t%%sp, -4096, %%sp\n\tretl\n\tsub\t%%sp, -%0, %%sp";
	}
      else if (SKIP_CALLERS_UNIMP_P)
	{
	  if ((actual_fsize & 0x3ff) != 0)
	    return "sethi\t%%hi(%a0), %%g1\n\tor\t%%g1, %%lo(%a0), %%g1\n\tjmp\t%%o7+12\n\tadd\t%%sp, %%g1, %%sp";
	  else
	    return "sethi\t%%hi(%a0), %%g1\n\tjmp\t%%o7+12\n\tadd\t%%sp, %%g1, %%sp";
	}
      else
	{
	  if ((actual_fsize & 0x3ff) != 0)
	    return "sethi %%hi(%a0),%%g1\n\tor %%g1,%%lo(%a0),%%g1\n\tretl\n\tadd %%sp,%%g1,%%sp";
	  else
	    return "sethi %%hi(%a0),%%g1\n\tretl\n\tadd %%sp,%%g1,%%sp";
	}
    }
  else if (TARGET_V9)
    {
      if (delay)
	{
	  epilogue_renumber (&SET_DEST (PATTERN (delay)), 0);
	  epilogue_renumber (&SET_SRC (PATTERN (delay)), 0);
	}
      if (SKIP_CALLERS_UNIMP_P)
	return "return\t%%i7+12%#";
      else
	return "return\t%%i7+8%#";
    }
  else
    {
      if (delay)
	abort ();
      if (SKIP_CALLERS_UNIMP_P)
	return "jmp\t%%i7+12\n\trestore";
      else
	return "ret\n\trestore";
    }
}

/* Leaf functions and non-leaf functions have different needs.  */

static int
reg_leaf_alloc_order[] = REG_LEAF_ALLOC_ORDER;

static int
reg_nonleaf_alloc_order[] = REG_ALLOC_ORDER;

static int *reg_alloc_orders[] = {
  reg_leaf_alloc_order,
  reg_nonleaf_alloc_order};

void
order_regs_for_local_alloc ()
{
  static int last_order_nonleaf = 1;

  if (regs_ever_live[15] != last_order_nonleaf)
    {
      last_order_nonleaf = !last_order_nonleaf;
      bcopy ((char *) reg_alloc_orders[last_order_nonleaf],
	     (char *) reg_alloc_order, FIRST_PSEUDO_REGISTER * sizeof (int));
    }
}

/* Return 1 if REG and MEM are legitimate enough to allow the various
   mem<-->reg splits to be run.  */

int
sparc_splitdi_legitimate (reg, mem)
     rtx reg;
     rtx mem;
{
  /* Punt if we are here by mistake.  */
  if (! reload_completed)
    abort ();

  /* We must have an offsettable memory reference.  */
  if (! offsettable_memref_p (mem))
    return 0;

  /* If we have legitimate args for ldd/std, we do not want
     the split to happen.  */
  if ((REGNO (reg) % 2) == 0
      && mem_min_alignment (mem, 8))
    return 0;

  /* Success.  */
  return 1;
}

/* Return 1 if x and y are some kind of REG and they refer to
   different hard registers.  This test is guarenteed to be
   run after reload.  */

int
sparc_absnegfloat_split_legitimate (x, y)
     rtx x, y;
{
  if (GET_CODE (x) == SUBREG)
    x = alter_subreg (x);
  if (GET_CODE (x) != REG)
    return 0;
  if (GET_CODE (y) == SUBREG)
    y = alter_subreg (y);
  if (GET_CODE (y) != REG)
    return 0;
  if (REGNO (x) == REGNO (y))
    return 0;
  return 1;
}

/* Return 1 if REGNO (reg1) is even and REGNO (reg1) == REGNO (reg2) - 1.
   This makes them candidates for using ldd and std insns. 

   Note reg1 and reg2 *must* be hard registers.  */

int
registers_ok_for_ldd_peep (reg1, reg2)
     rtx reg1, reg2;
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG) 
    return 0;

  if (REGNO (reg1) % 2 != 0)
    return 0;

  /* Integer ldd is deprecated in SPARC V9 */ 
  if (TARGET_V9 && REGNO (reg1) < 32)                  
    return 0;                             

  return (REGNO (reg1) == REGNO (reg2) - 1);
}

/* Return 1 if addr1 and addr2 are suitable for use in an ldd or 
   std insn.

   This can only happen when addr1 and addr2 are consecutive memory
   locations (addr1 + 4 == addr2).  addr1 must also be aligned on a 
   64 bit boundary (addr1 % 8 == 0).  

   We know %sp and %fp are kept aligned on a 64 bit boundary.  Other
   registers are assumed to *never* be properly aligned and are 
   rejected.

   Knowing %sp and %fp are kept aligned on a 64 bit boundary, we 
   need only check that the offset for addr1 % 8 == 0.  */

int
addrs_ok_for_ldd_peep (addr1, addr2)
      rtx addr1, addr2;
{
  int reg1, offset1;

  /* Extract a register number and offset (if used) from the first addr.  */
  if (GET_CODE (addr1) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (GET_CODE (XEXP (addr1, 0)) != REG)
	return 0;
      else
	{
          reg1 = REGNO (XEXP (addr1, 0));
	  /* The offset must be constant!  */
	  if (GET_CODE (XEXP (addr1, 1)) != CONST_INT)
            return 0;
          offset1 = INTVAL (XEXP (addr1, 1));
	}
    }
  else if (GET_CODE (addr1) != REG)
    return 0;
  else
    {
      reg1 = REGNO (addr1);
      /* This was a simple (mem (reg)) expression.  Offset is 0.  */
      offset1 = 0;
    }

  /* Make sure the second address is a (mem (plus (reg) (const_int).  */
  if (GET_CODE (addr2) != PLUS)
    return 0;

  if (GET_CODE (XEXP (addr2, 0)) != REG
      || GET_CODE (XEXP (addr2, 1)) != CONST_INT)
    return 0;

  /* Only %fp and %sp are allowed.  Additionally both addresses must
     use the same register.  */
  if (reg1 != FRAME_POINTER_REGNUM && reg1 != STACK_POINTER_REGNUM)
    return 0;

  if (reg1 != REGNO (XEXP (addr2, 0)))
    return 0;

  /* The first offset must be evenly divisible by 8 to ensure the 
     address is 64 bit aligned.  */
  if (offset1 % 8 != 0)
    return 0;

  /* The offset for the second addr must be 4 more than the first addr.  */
  if (INTVAL (XEXP (addr2, 1)) != offset1 + 4)
    return 0;

  /* All the tests passed.  addr1 and addr2 are valid for ldd and std
     instructions.  */
  return 1;
}

/* Return 1 if reg is a pseudo, or is the first register in 
   a hard register pair.  This makes it a candidate for use in
   ldd and std insns.  */

int
register_ok_for_ldd (reg)
     rtx reg;
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg) != REG) 
    return 0;

  if (REGNO (reg) < FIRST_PSEUDO_REGISTER)
    return (REGNO (reg) % 2 == 0);
  else 
    return 1;
}

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
    case '#':
      /* Output a 'nop' if there's nothing for the delay slot.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\t nop", file);
      return;
    case '*':
      /* Output an annul flag if there's nothing for the delay slot and we
	 are optimizing.  This is always used with '(' below.  */
      /* Sun OS 4.1.1 dbx can't handle an annulled unconditional branch;
	 this is a dbx bug.  So, we only do this when optimizing.  */
      /* On UltraSPARC, a branch in a delay slot causes a pipeline flush.
	 Always emit a nop in case the next instruction is a branch.  */
      if (dbr_sequence_length () == 0
	  && (optimize && (int)sparc_cpu < PROCESSOR_V9))
	fputs (",a", file);
      return;
    case '(':
      /* Output a 'nop' if there's nothing for the delay slot and we are
	 not optimizing.  This is always used with '*' above.  */
      if (dbr_sequence_length () == 0
	  && ! (optimize && (int)sparc_cpu < PROCESSOR_V9))
	fputs ("\n\t nop", file);
      return;
    case '_':
      /* Output the Embedded Medium/Anywhere code model base register.  */
      fputs (EMBMEDANY_BASE_REG, file);
      return;
    case '@':
      /* Print out what we are using as the frame pointer.  This might
	 be %fp, or might be %sp+offset.  */
      /* ??? What if offset is too big? Perhaps the caller knows it isn't? */
      fprintf (file, "%s+%d", frame_base_name, frame_base_offset);
      return;
    case 'Y':
      /* Adjust the operand to take into account a RESTORE operation.  */
      if (GET_CODE (x) == CONST_INT)
	break;
      else if (GET_CODE (x) != REG)
	output_operand_lossage ("Invalid %%Y operand");
      else if (REGNO (x) < 8)
	fputs (reg_names[REGNO (x)], file);
      else if (REGNO (x) >= 24 && REGNO (x) < 32)
	fputs (reg_names[REGNO (x)-16], file);
      else
	output_operand_lossage ("Invalid %%Y operand");
      return;
    case 'L':
      /* Print out the low order register name of a register pair.  */
      if (WORDS_BIG_ENDIAN)
	fputs (reg_names[REGNO (x)+1], file);
      else
	fputs (reg_names[REGNO (x)], file);
      return;
    case 'H':
      /* Print out the high order register name of a register pair.  */
      if (WORDS_BIG_ENDIAN)
	fputs (reg_names[REGNO (x)], file);
      else
	fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'R':
      /* Print out the second register name of a register pair or quad.
	 I.e., R (%o0) => %o1.  */
      fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'S':
      /* Print out the third register name of a register quad.
	 I.e., S (%o0) => %o2.  */
      fputs (reg_names[REGNO (x)+2], file);
      return;
    case 'T':
      /* Print out the fourth register name of a register quad.
	 I.e., T (%o0) => %o3.  */
      fputs (reg_names[REGNO (x)+3], file);
      return;
    case 'x':
      /* Print a condition code register.  */
      if (REGNO (x) == SPARC_ICC_REG)
	{
	  /* We don't handle CC[X]_NOOVmode because they're not supposed
	     to occur here.  */
	  if (GET_MODE (x) == CCmode)
	    fputs ("%icc", file);
	  else if (GET_MODE (x) == CCXmode)
	    fputs ("%xcc", file);
	  else
	    abort ();
	}
      else
	/* %fccN register */
	fputs (reg_names[REGNO (x)], file);
      return;
    case 'm':
      /* Print the operand's address only.  */
      output_address (XEXP (x, 0));
      return;
    case 'r':
      /* In this case we need a register.  Use %g0 if the
	 operand is const0_rtx.  */
      if (x == const0_rtx
	  || (GET_MODE (x) != VOIDmode && x == CONST0_RTX (GET_MODE (x))))
	{
	  fputs ("%g0", file);
	  return;
	}
      else
	break;

    case 'A':
      switch (GET_CODE (x))
	{
	case IOR: fputs ("or", file); break;
	case AND: fputs ("and", file); break;
	case XOR: fputs ("xor", file); break;
	default: output_operand_lossage ("Invalid %%A operand");
	}
      return;

    case 'B':
      switch (GET_CODE (x))
	{
	case IOR: fputs ("orn", file); break;
	case AND: fputs ("andn", file); break;
	case XOR: fputs ("xnor", file); break;
	default: output_operand_lossage ("Invalid %%B operand");
	}
      return;

      /* These are used by the conditional move instructions.  */
    case 'c' :
    case 'C':
      {
	enum rtx_code rc = GET_CODE (x);
	
	if (code == 'c')
	  {
	    enum machine_mode mode = GET_MODE (XEXP (x, 0));
	    if (mode == CCFPmode || mode == CCFPEmode)
	      rc = reverse_condition_maybe_unordered (GET_CODE (x));
	    else
	      rc = reverse_condition (GET_CODE (x));
	  }
	switch (rc)
	  {
	  case NE: fputs ("ne", file); break;
	  case EQ: fputs ("e", file); break;
	  case GE: fputs ("ge", file); break;
	  case GT: fputs ("g", file); break;
	  case LE: fputs ("le", file); break;
	  case LT: fputs ("l", file); break;
	  case GEU: fputs ("geu", file); break;
	  case GTU: fputs ("gu", file); break;
	  case LEU: fputs ("leu", file); break;
	  case LTU: fputs ("lu", file); break;
	  case LTGT: fputs ("lg", file); break;
	  case UNORDERED: fputs ("u", file); break;
	  case ORDERED: fputs ("o", file); break;
	  case UNLT: fputs ("ul", file); break;
	  case UNLE: fputs ("ule", file); break;
	  case UNGT: fputs ("ug", file); break;
	  case UNGE: fputs ("uge", file); break;
	  case UNEQ: fputs ("ue", file); break;
	  default: output_operand_lossage (code == 'c'
					   ? "Invalid %%c operand"
					   : "Invalid %%C operand");
	  }
	return;
      }

      /* These are used by the movr instruction pattern.  */
    case 'd':
    case 'D':
      {
	enum rtx_code rc = (code == 'd'
			    ? reverse_condition (GET_CODE (x))
			    : GET_CODE (x));
	switch (rc)
	  {
	  case NE: fputs ("ne", file); break;
	  case EQ: fputs ("e", file); break;
	  case GE: fputs ("gez", file); break;
	  case LT: fputs ("lz", file); break;
	  case LE: fputs ("lez", file); break;
	  case GT: fputs ("gz", file); break;
	  default: output_operand_lossage (code == 'd'
					   ? "Invalid %%d operand"
					   : "Invalid %%D operand");
	  }
	return;
      }

    case 'b':
      {
	/* Print a sign-extended character.  */
	int i = INTVAL (x) & 0xff;
	if (i & 0x80)
	  i |= 0xffffff00;
	fprintf (file, "%d", i);
	return;
      }

    case 'f':
      /* Operand must be a MEM; write its address.  */
      if (GET_CODE (x) != MEM)
	output_operand_lossage ("Invalid %%f operand");
      output_address (XEXP (x, 0));
      return;

    case 0:
      /* Do nothing special.  */
      break;

    default:
      /* Undocumented flag.  */
      output_operand_lossage ("invalid operand output code");
    }

  if (GET_CODE (x) == REG)
    fputs (reg_names[REGNO (x)], file);
  else if (GET_CODE (x) == MEM)
    {
      fputc ('[', file);
	/* Poor Sun assembler doesn't understand absolute addressing.  */
      if (CONSTANT_P (XEXP (x, 0)))
	fputs ("%g0+", file);
      output_address (XEXP (x, 0));
      fputc (']', file);
    }
  else if (GET_CODE (x) == HIGH)
    {
      fputs ("%hi(", file);
      output_addr_const (file, XEXP (x, 0));
      fputc (')', file);
    }
  else if (GET_CODE (x) == LO_SUM)
    {
      print_operand (file, XEXP (x, 0), 0);
      if (TARGET_CM_MEDMID)
	fputs ("+%l44(", file);
      else
	fputs ("+%lo(", file);
      output_addr_const (file, XEXP (x, 1));
      fputc (')', file);
    }
  else if (GET_CODE (x) == CONST_DOUBLE
	   && (GET_MODE (x) == VOIDmode
	       || GET_MODE_CLASS (GET_MODE (x)) == MODE_INT))
    {
      if (CONST_DOUBLE_HIGH (x) == 0)
	fprintf (file, "%u", (unsigned int) CONST_DOUBLE_LOW (x));
      else if (CONST_DOUBLE_HIGH (x) == -1
	       && CONST_DOUBLE_LOW (x) < 0)
	fprintf (file, "%d", (int) CONST_DOUBLE_LOW (x));
      else
	output_operand_lossage ("long long constant not a valid immediate operand");
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    output_operand_lossage ("floating point constant not a valid immediate operand");
  else { output_addr_const (file, x); }
}

/* This function outputs assembler code for VALUE to FILE, where VALUE is
   a 64 bit (DImode) value.  */

/* ??? If there is a 64 bit counterpart to .word that the assembler
   understands, then using that would simply this code greatly.  */
/* ??? We only output .xword's for symbols and only then in environments
   where the assembler can handle them.  */

void
output_double_int (file, value)
     FILE *file;
     rtx value;
{
  if (GET_CODE (value) == CONST_INT)
    {
      /* ??? This has endianness issues.  */
#if HOST_BITS_PER_WIDE_INT == 64
      HOST_WIDE_INT xword = INTVAL (value);
      HOST_WIDE_INT high, low;

      high = (xword >> 32) & 0xffffffff;
      low  = xword & 0xffffffff;
      ASM_OUTPUT_INT (file, GEN_INT (high));
      ASM_OUTPUT_INT (file, GEN_INT (low));
#else
      if (INTVAL (value) < 0)
	ASM_OUTPUT_INT (file, constm1_rtx);
      else
	ASM_OUTPUT_INT (file, const0_rtx);
      ASM_OUTPUT_INT (file, value);
#endif
    }
  else if (GET_CODE (value) == CONST_DOUBLE)
    {
      ASM_OUTPUT_INT (file, GEN_INT (CONST_DOUBLE_HIGH (value)));
      ASM_OUTPUT_INT (file, GEN_INT (CONST_DOUBLE_LOW (value)));
    }
  else if (GET_CODE (value) == SYMBOL_REF
	   || GET_CODE (value) == CONST
	   || GET_CODE (value) == PLUS
	   || (TARGET_ARCH64 &&
	       (GET_CODE (value) == LABEL_REF
		|| GET_CODE (value) == CODE_LABEL
		|| GET_CODE (value) == MINUS)))
    {
      if (! TARGET_V9)
	{
	  ASM_OUTPUT_INT (file, const0_rtx);
	  ASM_OUTPUT_INT (file, value);
	}
      else
	{
	  fprintf (file, "\t%s\t", ASM_LONGLONG);
	  output_addr_const (file, value);
	  fprintf (file, "\n");
	}
    }
  else
    abort ();
}

/* Return the value of a code used in the .proc pseudo-op that says
   what kind of result this function returns.  For non-C types, we pick
   the closest C type.  */

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * 2)
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

unsigned long
sparc_type_code (type)
     register tree type;
{
  register unsigned long qualifiers = 0;
  register unsigned shift;

  /* Only the first 30 bits of the qualifier are valid.  We must refrain from
     setting more, since some assemblers will give an error for this.  Also,
     we must be careful to avoid shifts of 32 bits or more to avoid getting
     unpredictable results.  */

  for (shift = 6; shift < 30; shift += 2, type = TREE_TYPE (type))
    {
      switch (TREE_CODE (type))
	{
	case ERROR_MARK:
	  return qualifiers;
  
	case ARRAY_TYPE:
	  qualifiers |= (3 << shift);
	  break;

	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  qualifiers |= (2 << shift);
	  break;

	case POINTER_TYPE:
	case REFERENCE_TYPE:
	case OFFSET_TYPE:
	  qualifiers |= (1 << shift);
	  break;

	case RECORD_TYPE:
	  return (qualifiers | 8);

	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  return (qualifiers | 9);

	case ENUMERAL_TYPE:
	  return (qualifiers | 10);

	case VOID_TYPE:
	  return (qualifiers | 16);

	case INTEGER_TYPE:
	  /* If this is a range type, consider it to be the underlying
	     type.  */
	  if (TREE_TYPE (type) != 0)
	    break;

	  /* Carefully distinguish all the standard types of C,
	     without messing up if the language is not C.  We do this by
	     testing TYPE_PRECISION and TREE_UNSIGNED.  The old code used to
	     look at both the names and the above fields, but that's redundant.
	     Any type whose size is between two C types will be considered
	     to be the wider of the two types.  Also, we do not have a
	     special code to use for "long long", so anything wider than
	     long is treated the same.  Note that we can't distinguish
	     between "int" and "long" in this code if they are the same
	     size, but that's fine, since neither can the assembler.  */

	  if (TYPE_PRECISION (type) <= CHAR_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 12 : 2));
  
	  else if (TYPE_PRECISION (type) <= SHORT_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 13 : 3));
  
	  else if (TYPE_PRECISION (type) <= INT_TYPE_SIZE)
	    return (qualifiers | (TREE_UNSIGNED (type) ? 14 : 4));
  
	  else
	    return (qualifiers | (TREE_UNSIGNED (type) ? 15 : 5));
  
	case REAL_TYPE:
	  /* If this is a range type, consider it to be the underlying
	     type.  */
	  if (TREE_TYPE (type) != 0)
	    break;

	  /* Carefully distinguish all the standard types of C,
	     without messing up if the language is not C.  */

	  if (TYPE_PRECISION (type) == FLOAT_TYPE_SIZE)
	    return (qualifiers | 6);

	  else 
	    return (qualifiers | 7);
  
	case COMPLEX_TYPE:	/* GNU Fortran COMPLEX type.  */
	  /* ??? We need to distinguish between double and float complex types,
	     but I don't know how yet because I can't reach this code from
	     existing front-ends.  */
	  return (qualifiers | 7);	/* Who knows? */

	case CHAR_TYPE:		/* GNU Pascal CHAR type.  Not used in C.  */
	case BOOLEAN_TYPE:	/* GNU Fortran BOOLEAN type.  */
	case FILE_TYPE:		/* GNU Pascal FILE type.  */
	case SET_TYPE:		/* GNU Pascal SET type.  */
	case LANG_TYPE:		/* ? */
	  return qualifiers;
  
	default:
	  abort ();		/* Not a type! */
        }
    }

  return qualifiers;
}

/* Nested function support.  */

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   This takes 16 insns: 2 shifts & 2 ands (to split up addresses), 4 sethi
   (to load in opcodes), 4 iors (to merge address and opcodes), and 4 writes
   (to store insns).  This is a bit excessive.  Perhaps a different
   mechanism would be better here.

   Emit enough FLUSH insns to synchronize the data and instruction caches.  */

void
sparc_initialize_trampoline (tramp, fnaddr, cxt)
     rtx tramp, fnaddr, cxt;
{
  /* SPARC 32 bit trampoline:

 	sethi	%hi(fn), %g1
 	sethi	%hi(static), %g2
 	jmp	%g1+%lo(fn)
 	or	%g2, %lo(static), %g2

    SETHI i,r  = 00rr rrr1 00ii iiii iiii iiii iiii iiii
    JMPL r+i,d = 10dd ddd1 1100 0rrr rr1i iiii iiii iiii
   */
#ifdef TRANSFER_FROM_TRAMPOLINE
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__enable_execute_stack"),
                     0, VOIDmode, 1, tramp, Pmode);
#endif

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 0)),
		  expand_binop (SImode, ior_optab,
				expand_shift (RSHIFT_EXPR, SImode, fnaddr,
					      size_int (10), 0, 1),
				GEN_INT (0x03000000),
				NULL_RTX, 1, OPTAB_DIRECT));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 4)),
		  expand_binop (SImode, ior_optab,
				expand_shift (RSHIFT_EXPR, SImode, cxt,
					      size_int (10), 0, 1),
				GEN_INT (0x05000000),
				NULL_RTX, 1, OPTAB_DIRECT));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 8)),
		  expand_binop (SImode, ior_optab,
				expand_and (fnaddr, GEN_INT (0x3ff), NULL_RTX),
				GEN_INT (0x81c06000),
				NULL_RTX, 1, OPTAB_DIRECT));

  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 12)),
		  expand_binop (SImode, ior_optab,
				expand_and (cxt, GEN_INT (0x3ff), NULL_RTX),
				GEN_INT (0x8410a000),
				NULL_RTX, 1, OPTAB_DIRECT));

  emit_insn (gen_flush (validize_mem (gen_rtx_MEM (SImode, tramp))));
  /* On UltraSPARC a flush flushes an entire cache line.  The trampoline is
     aligned on a 16 byte boundary so one flush clears it all.  */
  if (sparc_cpu != PROCESSOR_ULTRASPARC)
    emit_insn (gen_flush (validize_mem (gen_rtx_MEM (SImode,
						     plus_constant (tramp, 8)))));
}

/* The 64 bit version is simpler because it makes more sense to load the
   values as "immediate" data out of the trampoline.  It's also easier since
   we can read the PC without clobbering a register.  */

void
sparc64_initialize_trampoline (tramp, fnaddr, cxt)
     rtx tramp, fnaddr, cxt;
{
#ifdef TRANSFER_FROM_TRAMPOLINE
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__enable_execute_stack"),
                     0, VOIDmode, 1, tramp, Pmode);
#endif

  /*
	rd	%pc, %g1
	ldx	[%g1+24], %g5
	jmp	%g5
	ldx	[%g1+16], %g5
	+16 bytes data
   */

  emit_move_insn (gen_rtx_MEM (SImode, tramp),
		  GEN_INT (0x83414000));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 4)),
		  GEN_INT (0xca586018));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 8)),
		  GEN_INT (0x81c14000));
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 12)),
		  GEN_INT (0xca586010));
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, 16)), cxt);
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, 24)), fnaddr);
  emit_insn (gen_flushdi (validize_mem (gen_rtx_MEM (DImode, tramp))));

  if (sparc_cpu != PROCESSOR_ULTRASPARC)
    emit_insn (gen_flushdi (validize_mem (gen_rtx_MEM (DImode, plus_constant (tramp, 8)))));
}

/* Subroutines to support a flat (single) register window calling
   convention.  */

/* Single-register window sparc stack frames look like:

             Before call		        After call
        +-----------------------+	+-----------------------+
   high |		        |	|			|
   mem  |  caller's temps.    	|       |  caller's temps.    	|
	|       		|       |       	        |
        +-----------------------+	+-----------------------+
 	|       		|	|		        |
        |  arguments on stack.  |	|  arguments on stack.  |
	|       		|      	|			|
        +-----------------------+FP+92->+-----------------------+
 	|  6 words to save     	|	|  6 words to save	|
	|  arguments passed	|	|  arguments passed	|
	|  in registers, even	|	|  in registers, even	|
       	|  if not passed.       |      	|  if not passed.	|
 SP+68->+-----------------------+FP+68->+-----------------------+
        | 1 word struct addr	|      	| 1 word struct addr	|
        +-----------------------+FP+64->+-----------------------+
        |			|	|			|
        | 16 word reg save area	|	| 16 word reg save area |
       	|                       |      	|			|
    SP->+-----------------------+   FP->+-----------------------+
				        | 4 word area for	|
				       	| fp/alu reg moves	|
				 FP-16->+-----------------------+
				        |			|
				        |  local variables	|
				        |			|
				        +-----------------------+
				        |		        |
                                        |  fp register save     |
				        |			|
				        +-----------------------+
				        |		        |
                                        |  gp register save     |
                                        |       		|
				        +-----------------------+
				        |			|
                                        |  alloca allocations   |
        			        |			|
				        +-----------------------+
				        |			|
                                        |  arguments on stack   |
        			       	|		        |
				 SP+92->+-----------------------+
                                        |  6 words to save      |
				        |  arguments passed     |
                                        |  in registers, even   |
   low                                 	|  if not passed.       |
   memory        		 SP+68->+-----------------------+
				       	| 1 word struct addr	|
				 SP+64->+-----------------------+
				        |			|
				        I 16 word reg save area |
				       	|			|
				    SP->+-----------------------+  */

/* Structure to be filled in by sparc_flat_compute_frame_size with register
   save masks, and offsets for the current function.  */

struct sparc_frame_info
{
  unsigned long total_size;	/* # bytes that the entire frame takes up.  */
  unsigned long var_size;	/* # bytes that variables take up.  */
  unsigned long args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned long extra_size;	/* # bytes of extra gunk.  */
  unsigned int  gp_reg_size;	/* # bytes needed to store gp regs.  */
  unsigned int  fp_reg_size;	/* # bytes needed to store fp regs.  */
  unsigned long gmask;		/* Mask of saved gp registers.  */
  unsigned long fmask;		/* Mask of saved fp registers.  */
  unsigned long reg_offset;	/* Offset from new sp to store regs.  */
  int		initialized;	/* Nonzero if frame size already calculated.  */
};

/* Current frame information calculated by sparc_flat_compute_frame_size.  */
struct sparc_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
struct sparc_frame_info zero_frame_info;

/* Tell prologue and epilogue if register REGNO should be saved / restored.  */

#define RETURN_ADDR_REGNUM 15
#define FRAME_POINTER_MASK (1 << (FRAME_POINTER_REGNUM))
#define RETURN_ADDR_MASK (1 << (RETURN_ADDR_REGNUM))

#define MUST_SAVE_REGISTER(regno) \
 ((regs_ever_live[regno] && !call_used_regs[regno])		\
  || (regno == FRAME_POINTER_REGNUM && frame_pointer_needed)	\
  || (regno == RETURN_ADDR_REGNUM && regs_ever_live[RETURN_ADDR_REGNUM]))

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.  */

unsigned long
sparc_flat_compute_frame_size (size)
     int size;			/* # of var. bytes allocated.  */
{
  int regno;
  unsigned long total_size;	/* # bytes that the entire frame takes up.  */
  unsigned long var_size;	/* # bytes that variables take up.  */
  unsigned long args_size;	/* # bytes that outgoing arguments take up.  */
  unsigned long extra_size;	/* # extra bytes.  */
  unsigned int  gp_reg_size;	/* # bytes needed to store gp regs.  */
  unsigned int  fp_reg_size;	/* # bytes needed to store fp regs.  */
  unsigned long gmask;		/* Mask of saved gp registers.  */
  unsigned long fmask;		/* Mask of saved fp registers.  */
  unsigned long reg_offset;	/* Offset to register save area.  */
  int           need_aligned_p;	/* 1 if need the save area 8 byte aligned.  */

  /* This is the size of the 16 word reg save area, 1 word struct addr
     area, and 4 word fp/alu register copy area.  */
  extra_size = -STARTING_FRAME_OFFSET + FIRST_PARM_OFFSET(0);
  var_size = size;
  gp_reg_size = 0;
  fp_reg_size = 0;
  gmask = 0;
  fmask = 0;
  reg_offset = 0;
  need_aligned_p = 0;

  args_size = 0;
  if (!leaf_function_p ())
    {
      /* Also include the size needed for the 6 parameter registers.  */
      args_size = current_function_outgoing_args_size + 24;
    }
  total_size = var_size + args_size;

  /* Calculate space needed for gp registers.  */
  for (regno = 1; regno <= 31; regno++)
    {
      if (MUST_SAVE_REGISTER (regno))
	{
	  /* If we need to save two regs in a row, ensure there's room to bump
	     up the address to align it to a doubleword boundary.  */
	  if ((regno & 0x1) == 0 && MUST_SAVE_REGISTER (regno+1))
	    {
	      if (gp_reg_size % 8 != 0)
		gp_reg_size += 4;
	      gp_reg_size += 2 * UNITS_PER_WORD;
	      gmask |= 3 << regno;
	      regno++;
	      need_aligned_p = 1;
	    }
	  else
	    {
	      gp_reg_size += UNITS_PER_WORD;
	      gmask |= 1 << regno;
	    }
	}
    }

  /* Calculate space needed for fp registers.  */
  for (regno = 32; regno <= 63; regno++)
    {
      if (regs_ever_live[regno] && !call_used_regs[regno])
	{
	  fp_reg_size += UNITS_PER_WORD;
	  fmask |= 1 << (regno - 32);
	}
    }

  if (gmask || fmask)
    {
      int n;
      reg_offset = FIRST_PARM_OFFSET(0) + args_size;
      /* Ensure save area is 8 byte aligned if we need it.  */
      n = reg_offset % 8;
      if (need_aligned_p && n != 0)
	{
	  total_size += 8 - n;
	  reg_offset += 8 - n;
	}
      total_size += gp_reg_size + fp_reg_size;
    }

  /* If we must allocate a stack frame at all, we must also allocate 
     room for register window spillage, so as to be binary compatible
     with libraries and operating systems that do not use -mflat.  */
  if (total_size > 0)
    total_size += extra_size;
  else
    extra_size = 0;

  total_size = SPARC_STACK_ALIGN (total_size);

  /* Save other computed information.  */
  current_frame_info.total_size  = total_size;
  current_frame_info.var_size    = var_size;
  current_frame_info.args_size   = args_size;
  current_frame_info.extra_size  = extra_size;
  current_frame_info.gp_reg_size = gp_reg_size;
  current_frame_info.fp_reg_size = fp_reg_size;
  current_frame_info.gmask	 = gmask;
  current_frame_info.fmask	 = fmask;
  current_frame_info.reg_offset	 = reg_offset;
  current_frame_info.initialized = reload_completed;

  /* Ok, we're done.  */
  return total_size;
}

/* Save/restore registers in GMASK and FMASK at register BASE_REG plus offset
   OFFSET.

   BASE_REG must be 8 byte aligned.  This allows us to test OFFSET for
   appropriate alignment and use DOUBLEWORD_OP when we can.  We assume
   [BASE_REG+OFFSET] will always be a valid address.

   WORD_OP is either "st" for save, "ld" for restore.
   DOUBLEWORD_OP is either "std" for save, "ldd" for restore.  */

void
sparc_flat_save_restore (file, base_reg, offset, gmask, fmask, word_op,
			 doubleword_op, base_offset)
     FILE *file;
     const char *base_reg;
     unsigned int offset;
     unsigned long gmask;
     unsigned long fmask;
     const char *word_op;
     const char *doubleword_op;
     unsigned long base_offset;
{
  int regno;

  if (gmask == 0 && fmask == 0)
    return;

  /* Save registers starting from high to low.  We've already saved the
     previous frame pointer and previous return address for the debugger's
     sake.  The debugger allows us to not need a nop in the epilog if at least
     one register is reloaded in addition to return address.  */

  if (gmask)
    {
      for (regno = 1; regno <= 31; regno++)
	{
	  if ((gmask & (1L << regno)) != 0)
	    {
	      if ((regno & 0x1) == 0 && ((gmask & (1L << (regno+1))) != 0))
		{
		  /* We can save two registers in a row.  If we're not at a
		     double word boundary, move to one.
		     sparc_flat_compute_frame_size ensures there's room to do
		     this.  */
		  if (offset % 8 != 0)
		    offset += UNITS_PER_WORD;

		  if (word_op[0] == 's')
		    {
		      fprintf (file, "\t%s\t%s, [%s+%d]\n",
			       doubleword_op, reg_names[regno],
			       base_reg, offset);
		      if (dwarf2out_do_frame ())
			{
			  char *l = dwarf2out_cfi_label ();
			  dwarf2out_reg_save (l, regno, offset + base_offset);
			  dwarf2out_reg_save
			    (l, regno+1, offset+base_offset + UNITS_PER_WORD);
			}
		    }
		  else
		    fprintf (file, "\t%s\t[%s+%d], %s\n",
			     doubleword_op, base_reg, offset,
			     reg_names[regno]);

		  offset += 2 * UNITS_PER_WORD;
		  regno++;
		}
	      else
		{
		  if (word_op[0] == 's')
		    {
		      fprintf (file, "\t%s\t%s, [%s+%d]\n",
			       word_op, reg_names[regno],
			       base_reg, offset);
		      if (dwarf2out_do_frame ())
			dwarf2out_reg_save ("", regno, offset + base_offset);
		    }
		  else
		    fprintf (file, "\t%s\t[%s+%d], %s\n",
			     word_op, base_reg, offset, reg_names[regno]);

		  offset += UNITS_PER_WORD;
		}
	    }
	}
    }

  if (fmask)
    {
      for (regno = 32; regno <= 63; regno++)
	{
	  if ((fmask & (1L << (regno - 32))) != 0)
	    {
	      if (word_op[0] == 's')
		{
		  fprintf (file, "\t%s\t%s, [%s+%d]\n",
			   word_op, reg_names[regno],
			   base_reg, offset);
		  if (dwarf2out_do_frame ())
		    dwarf2out_reg_save ("", regno, offset + base_offset);
		}
	      else
		fprintf (file, "\t%s\t[%s+%d], %s\n",
			 word_op, base_reg, offset, reg_names[regno]);

	      offset += UNITS_PER_WORD;
	    }
	}
    }
}

/* Set up the stack and frame (if desired) for the function.  */

void
sparc_flat_output_function_prologue (file, size)
     FILE *file;
     int size;
{
  const char *sp_str = reg_names[STACK_POINTER_REGNUM];
  unsigned long gmask = current_frame_info.gmask;

  sparc_output_scratch_registers (file);

  /* This is only for the human reader.  */
  fprintf (file, "\t%s#PROLOGUE# 0\n", ASM_COMMENT_START);
  fprintf (file, "\t%s# vars= %ld, regs= %d/%d, args= %d, extra= %ld\n",
	   ASM_COMMENT_START,
	   current_frame_info.var_size,
	   current_frame_info.gp_reg_size / 4,
	   current_frame_info.fp_reg_size / 4,
	   current_function_outgoing_args_size,
	   current_frame_info.extra_size);

  size = SPARC_STACK_ALIGN (size);
  size = (! current_frame_info.initialized
	  ? sparc_flat_compute_frame_size (size)
	  : current_frame_info.total_size);

  /* These cases shouldn't happen.  Catch them now.  */
  if (size == 0 && (gmask || current_frame_info.fmask))
    abort ();

  /* Allocate our stack frame by decrementing %sp.
     At present, the only algorithm gdb can use to determine if this is a
     flat frame is if we always set %i7 if we set %sp.  This can be optimized
     in the future by putting in some sort of debugging information that says
     this is a `flat' function.  However, there is still the case of debugging
     code without such debugging information (including cases where most fns
     have such info, but there is one that doesn't).  So, always do this now
     so we don't get a lot of code out there that gdb can't handle.
     If the frame pointer isn't needn't then that's ok - gdb won't be able to
     distinguish us from a non-flat function but there won't (and shouldn't)
     be any differences anyway.  The return pc is saved (if necessary) right
     after %i7 so gdb won't have to look too far to find it.  */
  if (size > 0)
    {
      unsigned int reg_offset = current_frame_info.reg_offset;
      const char *fp_str = reg_names[FRAME_POINTER_REGNUM];
      const char *t1_str = "%g1";

      /* Things get a little tricky if local variables take up more than ~4096
	 bytes and outgoing arguments take up more than ~4096 bytes.  When that
	 happens, the register save area can't be accessed from either end of
	 the frame.  Handle this by decrementing %sp to the start of the gp
	 register save area, save the regs, update %i7, and then set %sp to its
	 final value.  Given that we only have one scratch register to play
	 with it is the cheapest solution, and it helps gdb out as it won't
	 slow down recognition of flat functions.
	 Don't change the order of insns emitted here without checking with
	 the gdb folk first.  */

      /* Is the entire register save area offsettable from %sp?  */
      if (reg_offset < 4096 - 64 * (unsigned) UNITS_PER_WORD)
	{
	  if (size <= 4096)
	    {
	      fprintf (file, "\tadd\t%s, %d, %s\n",
		       sp_str, -size, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst\t%s, [%s+%d]\n",
			   fp_str, sp_str, reg_offset);
		  fprintf (file, "\tsub\t%s, %d, %s\t%s# set up frame pointer\n",
			   sp_str, -size, fp_str, ASM_COMMENT_START);
		  reg_offset += 4;
		}
	    }
	  else
	    {
	      fprintf (file, "\tset\t%d, %s\n\tsub\t%s, %s, %s\n",
		       size, t1_str, sp_str, t1_str, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst\t%s, [%s+%d]\n",
			   fp_str, sp_str, reg_offset);
		  fprintf (file, "\tadd\t%s, %s, %s\t%s# set up frame pointer\n",
			   sp_str, t1_str, fp_str, ASM_COMMENT_START);
		  reg_offset += 4;
		}
	    }
	  if (dwarf2out_do_frame ())
	    {
	      char *l = dwarf2out_cfi_label ();
	      if (gmask & FRAME_POINTER_MASK)
		{
		  dwarf2out_reg_save (l, FRAME_POINTER_REGNUM,
				      reg_offset - 4 - size);
		  dwarf2out_def_cfa (l, FRAME_POINTER_REGNUM, 0);
		}
	      else
		dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, size);
	    }
	  if (gmask & RETURN_ADDR_MASK)
	    {
	      fprintf (file, "\tst\t%s, [%s+%d]\n",
		       reg_names[RETURN_ADDR_REGNUM], sp_str, reg_offset);
	      if (dwarf2out_do_frame ())
		dwarf2out_return_save ("", reg_offset - size);
	      reg_offset += 4;
	    }
	  sparc_flat_save_restore (file, sp_str, reg_offset,
				   gmask & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK),
				   current_frame_info.fmask,
				   "st", "std", -size);
	}
      else
	{
	  /* Subtract %sp in two steps, but make sure there is always a
	     64 byte register save area, and %sp is properly aligned.  */
	  /* Amount to decrement %sp by, the first time.  */
	  unsigned int size1 = ((size - reg_offset + 64) + 15) & -16;
	  /* Offset to register save area from %sp.  */
	  unsigned int offset = size1 - (size - reg_offset);
	  
	  if (size1 <= 4096)
	    {
	      fprintf (file, "\tadd\t%s, %d, %s\n",
		       sp_str, -size1, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst\t%s, [%s+%d]\n\tsub\t%s, %d, %s\t%s# set up frame pointer\n",
			   fp_str, sp_str, offset, sp_str, -size1, fp_str,
			   ASM_COMMENT_START);
		  offset += 4;
		}
	    }
	  else
	    {
	      fprintf (file, "\tset\t%d, %s\n\tsub\t%s, %s, %s\n",
		       size1, t1_str, sp_str, t1_str, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst\t%s, [%s+%d]\n\tadd\t%s, %s, %s\t%s# set up frame pointer\n",
			   fp_str, sp_str, offset, sp_str, t1_str, fp_str,
			   ASM_COMMENT_START);
		  offset += 4;
		}
	    }
	  if (dwarf2out_do_frame ())
	    {
	      char *l = dwarf2out_cfi_label ();
	      if (gmask & FRAME_POINTER_MASK)
		{
		  dwarf2out_reg_save (l, FRAME_POINTER_REGNUM,
				      offset - 4 - size1);
		  dwarf2out_def_cfa (l, FRAME_POINTER_REGNUM, 0);
		}
	      else
		dwarf2out_def_cfa (l, STACK_POINTER_REGNUM, size1);
	    }
	  if (gmask & RETURN_ADDR_MASK)
	    {
	      fprintf (file, "\tst\t%s, [%s+%d]\n",
		       reg_names[RETURN_ADDR_REGNUM], sp_str, offset);
	      if (dwarf2out_do_frame ())
		/* offset - size1 == reg_offset - size
		   if reg_offset were updated above like offset.  */
		dwarf2out_return_save ("", offset - size1);
	      offset += 4;
	    }
	  sparc_flat_save_restore (file, sp_str, offset,
				   gmask & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK),
				   current_frame_info.fmask,
				   "st", "std", -size1);
	  fprintf (file, "\tset\t%d, %s\n\tsub\t%s, %s, %s\n",
		   size - size1, t1_str, sp_str, t1_str, sp_str);
	  if (dwarf2out_do_frame ())
	    if (! (gmask & FRAME_POINTER_MASK))
	      dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, size);
	}
    }

  fprintf (file, "\t%s#PROLOGUE# 1\n", ASM_COMMENT_START);
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs. */

void
sparc_flat_output_function_epilogue (file, size)
     FILE *file;
     int size;
{
  rtx epilogue_delay = current_function_epilogue_delay_list;
  int noepilogue = FALSE;

  /* This is only for the human reader.  */
  fprintf (file, "\t%s#EPILOGUE#\n", ASM_COMMENT_START);

  /* The epilogue does not depend on any registers, but the stack
     registers, so we assume that if we have 1 pending nop, it can be
     ignored, and 2 it must be filled (2 nops occur for integer
     multiply and divide).  */

  size = SPARC_STACK_ALIGN (size);
  size = (!current_frame_info.initialized
	   ? sparc_flat_compute_frame_size (size)
	   : current_frame_info.total_size);

  if (size == 0 && epilogue_delay == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn && GET_CODE (insn) == BARRIER)
	noepilogue = TRUE;
    }

  if (!noepilogue)
    {
      unsigned int reg_offset = current_frame_info.reg_offset;
      unsigned int size1;
      const char *sp_str = reg_names[STACK_POINTER_REGNUM];
      const char *fp_str = reg_names[FRAME_POINTER_REGNUM];
      const char *t1_str = "%g1";

      /* In the reload sequence, we don't need to fill the load delay
	 slots for most of the loads, also see if we can fill the final
	 delay slot if not otherwise filled by the reload sequence.  */

      if (size > 4095)
	fprintf (file, "\tset\t%d, %s\n", size, t1_str);

      if (frame_pointer_needed)
	{
	  if (size > 4095)
	    fprintf (file,"\tsub\t%s, %s, %s\t\t%s# sp not trusted here\n",
		     fp_str, t1_str, sp_str, ASM_COMMENT_START);
	  else
	    fprintf (file,"\tsub\t%s, %d, %s\t\t%s# sp not trusted here\n",
		     fp_str, size, sp_str, ASM_COMMENT_START);
	}

      /* Is the entire register save area offsettable from %sp?  */
      if (reg_offset < 4096 - 64 * (unsigned) UNITS_PER_WORD)
	{
	  size1 = 0;
	}
      else
	{
	  /* Restore %sp in two steps, but make sure there is always a
	     64 byte register save area, and %sp is properly aligned.  */
	  /* Amount to increment %sp by, the first time.  */
	  size1 = ((reg_offset - 64 - 16) + 15) & -16;
	  /* Offset to register save area from %sp.  */
	  reg_offset = size1 - reg_offset;

	  fprintf (file, "\tset\t%d, %s\n\tadd\t%s, %s, %s\n",
		   size1, t1_str, sp_str, t1_str, sp_str);
	}

      /* We must restore the frame pointer and return address reg first
	 because they are treated specially by the prologue output code.  */
      if (current_frame_info.gmask & FRAME_POINTER_MASK)
	{
	  fprintf (file, "\tld\t[%s+%d], %s\n",
		   sp_str, reg_offset, fp_str);
	  reg_offset += 4;
	}
      if (current_frame_info.gmask & RETURN_ADDR_MASK)
	{
	  fprintf (file, "\tld\t[%s+%d], %s\n",
		   sp_str, reg_offset, reg_names[RETURN_ADDR_REGNUM]);
	  reg_offset += 4;
	}

      /* Restore any remaining saved registers.  */
      sparc_flat_save_restore (file, sp_str, reg_offset,
			       current_frame_info.gmask & ~(FRAME_POINTER_MASK | RETURN_ADDR_MASK),
			       current_frame_info.fmask,
			       "ld", "ldd", 0);

      /* If we had to increment %sp in two steps, record it so the second
	 restoration in the epilogue finishes up.  */
      if (size1 > 0)
	{
	  size -= size1;
	  if (size > 4095)
	    fprintf (file, "\tset\t%d, %s\n",
		     size, t1_str);
	}

      if (current_function_returns_struct)
	fprintf (file, "\tjmp\t%%o7+12\n");
      else
	fprintf (file, "\tretl\n");

      /* If the only register saved is the return address, we need a
	 nop, unless we have an instruction to put into it.  Otherwise
	 we don't since reloading multiple registers doesn't reference
	 the register being loaded.  */

      if (epilogue_delay)
	{
	  if (size)
	    abort ();
	  final_scan_insn (XEXP (epilogue_delay, 0), file, 1, -2, 1);
	}

      else if (size > 4095)
	fprintf (file, "\tadd\t%s, %s, %s\n", sp_str, t1_str, sp_str);

      else if (size > 0)
	fprintf (file, "\tadd\t%s, %d, %s\n", sp_str, size, sp_str);

      else
	fprintf (file, "\tnop\n");
    }

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;

  sparc_output_deferred_case_vectors ();
}

/* Define the number of delay slots needed for the function epilogue.

   On the sparc, we need a slot if either no stack has been allocated,
   or the only register saved is the return register.  */

int
sparc_flat_epilogue_delay_slots ()
{
  if (!current_frame_info.initialized)
    (void) sparc_flat_compute_frame_size (get_frame_size ());

  if (current_frame_info.total_size == 0)
    return 1;

  return 0;
}

/* Return true is TRIAL is a valid insn for the epilogue delay slot.
   Any single length instruction which doesn't reference the stack or frame
   pointer is OK.  */

int
sparc_flat_eligible_for_epilogue_delay (trial, slot)
     rtx trial;
     int slot ATTRIBUTE_UNUSED;
{
  rtx pat = PATTERN (trial);

  if (get_attr_length (trial) != 1)
    return 0;

  if (! reg_mentioned_p (stack_pointer_rtx, pat)
      && ! reg_mentioned_p (frame_pointer_rtx, pat))
    return 1;

  return 0;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
supersparc_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  enum attr_type insn_type;

  if (! recog_memoized (insn))
    return 0;

  insn_type = get_attr_type (insn);

  if (REG_NOTE_KIND (link) == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      /* if a load, then the dependence must be on the memory address;
	 add an extra "cycle".  Note that the cost could be two cycles
	 if the reg was written late in an instruction group; we ca not tell
	 here.  */
      if (insn_type == TYPE_LOAD || insn_type == TYPE_FPLOAD)
	return cost + 3;

      /* Get the delay only if the address of the store is the dependence.  */
      if (insn_type == TYPE_STORE || insn_type == TYPE_FPSTORE)
	{
	  rtx pat = PATTERN(insn);
	  rtx dep_pat = PATTERN (dep_insn);

	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    return cost;  /* This should not happen!  */

	  /* The dependency between the two instructions was on the data that
	     is being stored.  Assume that this implies that the address of the
	     store is not dependent.  */
	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    return cost;

	  return cost + 3;  /* An approximation.  */
	}

      /* A shift instruction cannot receive its data from an instruction
	 in the same cycle; add a one cycle penalty.  */
      if (insn_type == TYPE_SHIFT)
	return cost + 3;   /* Split before cascade into shift.  */
    }
  else
    {
      /* Anti- or output- dependency; DEP_INSN reads/writes a register that
	 INSN writes some cycles later.  */

      /* These are only significant for the fpu unit; writing a fp reg before
         the fpu has finished with it stalls the processor.  */

      /* Reusing an integer register causes no problems.  */
      if (insn_type == TYPE_IALU || insn_type == TYPE_SHIFT)
	return 0;
    }
	
  return cost;
}

static int
hypersparc_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  enum attr_type insn_type, dep_type;
  rtx pat = PATTERN(insn);
  rtx dep_pat = PATTERN (dep_insn);

  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_type = get_attr_type (dep_insn);

  switch (REG_NOTE_KIND (link))
    {
    case 0:
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      switch (insn_type)
	{
	case TYPE_STORE:
	case TYPE_FPSTORE:
	  /* Get the delay iff the address of the store is the dependence. */
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    return cost;

	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    return cost;
	  return cost + 3;

	case TYPE_LOAD:
	case TYPE_SLOAD:
	case TYPE_FPLOAD:
	  /* If a load, then the dependence must be on the memory address.  If
	     the addresses aren't equal, then it might be a false dependency */
	  if (dep_type == TYPE_STORE || dep_type == TYPE_FPSTORE)
	    {
	      if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET
		  || GET_CODE (SET_DEST (dep_pat)) != MEM        
		  || GET_CODE (SET_SRC (pat)) != MEM
		  || ! rtx_equal_p (XEXP (SET_DEST (dep_pat), 0),
				    XEXP (SET_SRC (pat), 0)))
		return cost + 2;

	      return cost + 8;        
	    }
	  break;

	case TYPE_BRANCH:
	  /* Compare to branch latency is 0.  There is no benefit from
	     separating compare and branch.  */
	  if (dep_type == TYPE_COMPARE)
	    return 0;
	  /* Floating point compare to branch latency is less than
	     compare to conditional move.  */
	  if (dep_type == TYPE_FPCMP)
	    return cost - 1;
	  break;
	default:
	  break;
	}
	break;

    case REG_DEP_ANTI:
      /* Anti-dependencies only penalize the fpu unit. */
      if (insn_type == TYPE_IALU || insn_type == TYPE_SHIFT)
        return 0;
      break;

    default:
      break;
    }    

  return cost;
}

static int
ultrasparc_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  enum attr_type insn_type, dep_type;
  rtx pat = PATTERN(insn);
  rtx dep_pat = PATTERN (dep_insn);

  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_type = get_attr_type (dep_insn);

  /* Nothing issues in parallel with integer multiplies, so
     mark as zero cost since the scheduler can not do anything
     about it.  */
  if (insn_type == TYPE_IMUL)
    return 0;

#define SLOW_FP(dep_type) \
(dep_type == TYPE_FPSQRTS || dep_type == TYPE_FPSQRTD || \
 dep_type == TYPE_FPDIVS || dep_type == TYPE_FPDIVD)

  switch (REG_NOTE_KIND (link))
    {
    case 0:
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      if (dep_type == TYPE_CMOVE)
	{
	  /* Instructions that read the result of conditional moves cannot
	     be in the same group or the following group.  */
	  return cost + 1;
	}

      switch (insn_type)
	{
	  /* UltraSPARC can dual issue a store and an instruction setting
	     the value stored, except for divide and square root.  */
	case TYPE_FPSTORE:
	  if (! SLOW_FP (dep_type))
	    return 0;
	  return cost;

	case TYPE_STORE:
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    return cost;

	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    /* The dependency between the two instructions is on the data
	       that is being stored.  Assume that the address of the store
	       is not also dependent.  */
	    return 0;
	  return cost;

	case TYPE_LOAD:
	case TYPE_SLOAD:
	case TYPE_FPLOAD:
	  /* A load does not return data until at least 11 cycles after
	     a store to the same location.  3 cycles are accounted for
	     in the load latency; add the other 8 here.  */
	  if (dep_type == TYPE_STORE || dep_type == TYPE_FPSTORE)
	    {
	      /* If the addresses are not equal this may be a false
		 dependency because pointer aliasing could not be
		 determined.  Add only 2 cycles in that case.  2 is
		 an arbitrary compromise between 8, which would cause
		 the scheduler to generate worse code elsewhere to
		 compensate for a dependency which might not really
		 exist, and 0.  */
	      if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET
		  || GET_CODE (SET_SRC (pat)) != MEM
		  || GET_CODE (SET_DEST (dep_pat)) != MEM
		  || ! rtx_equal_p (XEXP (SET_SRC (pat), 0),
				    XEXP (SET_DEST (dep_pat), 0)))
		return cost + 2;

	      return cost + 8;
	    }
	  return cost;

	case TYPE_BRANCH:
	  /* Compare to branch latency is 0.  There is no benefit from
	     separating compare and branch.  */
	  if (dep_type == TYPE_COMPARE)
	    return 0;
	  /* Floating point compare to branch latency is less than
	     compare to conditional move.  */
	  if (dep_type == TYPE_FPCMP)
	    return cost - 1;
	  return cost;

	case TYPE_FPCMOVE:
	  /* FMOVR class instructions can not issue in the same cycle
	     or the cycle after an instruction which writes any
	     integer register.  Model this as cost 2 for dependent
	     instructions.  */
	  if ((dep_type == TYPE_IALU || dep_type == TYPE_UNARY
	       || dep_type == TYPE_BINARY)
	      && cost < 2)
	    return 2;
	  /* Otherwise check as for integer conditional moves. */

	case TYPE_CMOVE:
	  /* Conditional moves involving integer registers wait until
	     3 cycles after loads return data.  The interlock applies
	     to all loads, not just dependent loads, but that is hard
	     to model.  */
	  if (dep_type == TYPE_LOAD || dep_type == TYPE_SLOAD)
	    return cost + 3;
	  return cost;

	default:
	  break;
	}
      break;

    case REG_DEP_ANTI:
      /* Divide and square root lock destination registers for full latency. */
      if (! SLOW_FP (dep_type))
	return 0;
      break;

    case REG_DEP_OUTPUT:
      /* IEU and FPU instruction that have the same destination
	 register cannot be grouped together.  */
      return cost + 1;

    default:
      break;
    }

  /* Other costs not accounted for:
     - Single precision floating point loads lock the other half of
       the even/odd register pair.
     - Several hazards associated with ldd/std are ignored because these
       instructions are rarely generated for V9.
     - The floating point pipeline can not have both a single and double
       precision operation active at the same time.  Format conversions
       and graphics instructions are given honorary double precision status.
     - call and jmpl are always the first instruction in a group.  */

  return cost;

#undef SLOW_FP
}

int
sparc_adjust_cost(insn, link, dep, cost)
     rtx insn;
     rtx link;
     rtx dep;
     int cost;
{
  switch (sparc_cpu)
    {
    case PROCESSOR_SUPERSPARC:
      cost = supersparc_adjust_cost (insn, link, dep, cost);
      break;
    case PROCESSOR_HYPERSPARC:
    case PROCESSOR_SPARCLITE86X:
      cost = hypersparc_adjust_cost (insn, link, dep, cost);
      break;
    case PROCESSOR_ULTRASPARC:
      cost = ultrasparc_adjust_cost (insn, link, dep, cost);
      break;
    default:
      break;
    }
  return cost;
}

/* This describes the state of the UltraSPARC pipeline during
   instruction scheduling.  */

#define TMASK(__x)	((unsigned)1 << ((int)(__x)))
#define UMASK(__x)	((unsigned)1 << ((int)(__x)))

enum ultra_code { NONE=0, /* no insn at all				*/
		  IEU0,   /* shifts and conditional moves		*/
		  IEU1,   /* condition code setting insns, calls+jumps	*/
		  IEUN,   /* all other single cycle ieu insns		*/
		  LSU,    /* loads and stores				*/
		  CTI,    /* branches					*/
		  FPM,    /* FPU pipeline 1, multiplies and divides	*/
		  FPA,    /* FPU pipeline 2, all other operations	*/
		  SINGLE, /* single issue instructions			*/
		  NUM_ULTRA_CODES };

static enum ultra_code ultra_code_from_mask PARAMS ((int));
static void ultra_schedule_insn PARAMS ((rtx *, rtx *, int, enum ultra_code));

static const char *ultra_code_names[NUM_ULTRA_CODES] = {
  "NONE", "IEU0", "IEU1", "IEUN", "LSU", "CTI",
  "FPM", "FPA", "SINGLE" };

struct ultrasparc_pipeline_state {
  /* The insns in this group.  */
  rtx group[4];

  /* The code for each insn.  */
  enum ultra_code codes[4];

  /* Which insns in this group have been committed by the
     scheduler.  This is how we determine how many more
     can issue this cycle.  */
  char commit[4];

  /* How many insns in this group.  */
  char group_size;

  /* Mask of free slots still in this group.  */
  char free_slot_mask;

  /* The slotter uses the following to determine what other
     insn types can still make their way into this group.  */
  char contents [NUM_ULTRA_CODES];
  char num_ieu_insns;
};

#define ULTRA_NUM_HIST	8
static struct ultrasparc_pipeline_state ultra_pipe_hist[ULTRA_NUM_HIST];
static int ultra_cur_hist;
static int ultra_cycles_elapsed;

#define ultra_pipe	(ultra_pipe_hist[ultra_cur_hist])

/* Given TYPE_MASK compute the ultra_code it has.  */
static enum ultra_code
ultra_code_from_mask (type_mask)
     int type_mask;
{
  if (type_mask & (TMASK (TYPE_SHIFT) | TMASK (TYPE_CMOVE)))
    return IEU0;
  else if (type_mask & (TMASK (TYPE_COMPARE) |
			TMASK (TYPE_CALL) |
			TMASK (TYPE_UNCOND_BRANCH)))
    return IEU1;
  else if (type_mask & (TMASK (TYPE_IALU) | TMASK (TYPE_BINARY) |
			TMASK (TYPE_MOVE) | TMASK (TYPE_UNARY)))
    return IEUN;
  else if (type_mask & (TMASK (TYPE_LOAD) | TMASK (TYPE_SLOAD) |
			TMASK (TYPE_STORE) | TMASK (TYPE_FPLOAD) |
			TMASK (TYPE_FPSTORE)))
    return LSU;
  else if (type_mask & (TMASK (TYPE_FPMUL) | TMASK (TYPE_FPDIVS) |
			TMASK (TYPE_FPDIVD) | TMASK (TYPE_FPSQRTS) |
			TMASK (TYPE_FPSQRTD)))
    return FPM;
  else if (type_mask & (TMASK (TYPE_FPMOVE) | TMASK (TYPE_FPCMOVE) |
			TMASK (TYPE_FP) | TMASK (TYPE_FPCMP)))
    return FPA;
  else if (type_mask & TMASK (TYPE_BRANCH))
    return CTI;

  return SINGLE;
}

/* Check INSN (a conditional move) and make sure that it's
   results are available at this cycle.  Return 1 if the
   results are in fact ready.  */
static int
ultra_cmove_results_ready_p (insn)
     rtx insn;
{
  struct ultrasparc_pipeline_state *up;
  int entry, slot;

  /* If this got dispatched in the previous
     group, the results are not ready.  */
  entry = (ultra_cur_hist - 1) & (ULTRA_NUM_HIST - 1);
  up = &ultra_pipe_hist[entry];
  slot = 4;
  while (--slot >= 0)
    if (up->group[slot] == insn)
      return 0;

  return 1;
}

/* Walk backwards in pipeline history looking for FPU
   operations which use a mode different than FPMODE and
   will create a stall if an insn using FPMODE were to be
   dispatched this cycle.  */
static int
ultra_fpmode_conflict_exists (fpmode)
     enum machine_mode fpmode;
{
  int hist_ent;
  int hist_lim;

  hist_ent = (ultra_cur_hist - 1) & (ULTRA_NUM_HIST - 1);
  if (ultra_cycles_elapsed < 4)
    hist_lim = ultra_cycles_elapsed;
  else
    hist_lim = 4;
  while (hist_lim > 0)
    {
      struct ultrasparc_pipeline_state *up = &ultra_pipe_hist[hist_ent];
      int slot = 4;

      while (--slot >= 0)
	{
	  rtx insn = up->group[slot];
	  enum machine_mode this_mode;
	  rtx pat;

	  if (! insn
	      || GET_CODE (insn) != INSN
	      || (pat = PATTERN (insn)) == 0
	      || GET_CODE (pat) != SET)
	    continue;

	  this_mode = GET_MODE (SET_DEST (pat));
	  if ((this_mode != SFmode
	       && this_mode != DFmode)
	      || this_mode == fpmode)
	    continue;

	  /* If it is not FMOV, FABS, FNEG, FDIV, or FSQRT then
	     we will get a stall.  Loads and stores are independant
	     of these rules.  */
	  if (GET_CODE (SET_SRC (pat)) != ABS
	      && GET_CODE (SET_SRC (pat)) != NEG
	      && ((TMASK (get_attr_type (insn)) &
		   (TMASK (TYPE_FPDIVS) | TMASK (TYPE_FPDIVD) |
		    TMASK (TYPE_FPMOVE) | TMASK (TYPE_FPSQRTS) |
		    TMASK (TYPE_FPSQRTD) |
                    TMASK (TYPE_LOAD) | TMASK (TYPE_STORE))) == 0))
	    return 1;
	}
      hist_lim--;
      hist_ent = (hist_ent - 1) & (ULTRA_NUM_HIST - 1);
    }

  /* No conflicts, safe to dispatch.  */
  return 0;
}

/* Find an instruction in LIST which has one of the
   type attributes enumerated in TYPE_MASK.  START
   says where to begin the search.

   NOTE: This scheme depends upon the fact that we
         have less than 32 distinct type attributes.  */

static int ultra_types_avail;

static rtx *
ultra_find_type (type_mask, list, start)
     int type_mask;
     rtx *list;
     int start;
{
  int i;

  /* Short circuit if no such insn exists in the ready
     at the moment.  */
  if ((type_mask & ultra_types_avail) == 0)
    return 0;

  for (i = start; i >= 0; i--)
    {
      rtx insn = list[i];

      if (recog_memoized (insn) >= 0
	  && (TMASK(get_attr_type (insn)) & type_mask))
	{
	  enum machine_mode fpmode = SFmode;
	  rtx pat = 0;
	  int slot;
	  int check_depend = 0;
	  int check_fpmode_conflict = 0;

	  if (GET_CODE (insn) == INSN
	      && (pat = PATTERN(insn)) != 0
	      && GET_CODE (pat) == SET
	      && !(type_mask & (TMASK (TYPE_STORE) |
				TMASK (TYPE_FPSTORE))))
	    {
	      check_depend = 1;
	      if (GET_MODE (SET_DEST (pat)) == SFmode
		  || GET_MODE (SET_DEST (pat)) == DFmode)
		{
		  fpmode = GET_MODE (SET_DEST (pat));
		  check_fpmode_conflict = 1;
		}
	    }

	  slot = 4;
	  while(--slot >= 0)
	    {
	      rtx slot_insn = ultra_pipe.group[slot];
	      rtx slot_pat;

	      /* Already issued, bad dependency, or FPU
		 mode conflict.  */
	      if (slot_insn != 0
		  && (slot_pat = PATTERN (slot_insn)) != 0
		  && ((insn == slot_insn)
		      || (check_depend == 1
			  && GET_CODE (slot_insn) == INSN
			  && GET_CODE (slot_pat) == SET
			  && ((GET_CODE (SET_DEST (slot_pat)) == REG
			       && GET_CODE (SET_SRC (pat)) == REG
			       && REGNO (SET_DEST (slot_pat)) ==
			            REGNO (SET_SRC (pat)))
			      || (GET_CODE (SET_DEST (slot_pat)) == SUBREG
				  && GET_CODE (SET_SRC (pat)) == SUBREG
				  && REGNO (SUBREG_REG (SET_DEST (slot_pat))) ==
				       REGNO (SUBREG_REG (SET_SRC (pat)))
				  && SUBREG_WORD (SET_DEST (slot_pat)) ==
				       SUBREG_WORD (SET_SRC (pat)))))
		      || (check_fpmode_conflict == 1
			  && GET_CODE (slot_insn) == INSN
			  && GET_CODE (slot_pat) == SET
			  && (GET_MODE (SET_DEST (slot_pat)) == SFmode
			      || GET_MODE (SET_DEST (slot_pat)) == DFmode)
			  && GET_MODE (SET_DEST (slot_pat)) != fpmode)))
		goto next;
	    }

	  /* Check for peculiar result availability and dispatch
	     interference situations.  */
	  if (pat != 0
	      && ultra_cycles_elapsed > 0)
	    {
	      rtx link;

	      for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
		{
		  rtx link_insn = XEXP (link, 0);
		  if (GET_CODE (link_insn) == INSN
		      && recog_memoized (link_insn) >= 0
		      && (TMASK (get_attr_type (link_insn)) &
			  (TMASK (TYPE_CMOVE) | TMASK (TYPE_FPCMOVE)))
		      && ! ultra_cmove_results_ready_p (link_insn))
		    goto next;
		}

	      if (check_fpmode_conflict
		  && ultra_fpmode_conflict_exists (fpmode))
		goto next;
	    }

	  return &list[i];
	}
    next:
      ;
    }
  return 0;
}

static void
ultra_build_types_avail (ready, n_ready)
  rtx *ready;
  int n_ready;
{
  int i = n_ready - 1;

  ultra_types_avail = 0;
  while(i >= 0)
    {
      rtx insn = ready[i];

      if (recog_memoized (insn) >= 0)
	ultra_types_avail |= TMASK (get_attr_type (insn));

      i -= 1;
    }
}

/* Place insn pointed to my IP into the pipeline.
   Make element THIS of READY be that insn if it
   is not already.  TYPE indicates the pipeline class
   this insn falls into.  */
static void
ultra_schedule_insn (ip, ready, this, type)
     rtx *ip;
     rtx *ready;
     int this;
     enum ultra_code type;
{
  int pipe_slot;
  char mask = ultra_pipe.free_slot_mask;
  rtx temp;

  /* Obtain free slot.  */
  for (pipe_slot = 0; pipe_slot < 4; pipe_slot++)
    if ((mask & (1 << pipe_slot)) != 0)
      break;
  if (pipe_slot == 4)
    abort ();

  /* In it goes, and it hasn't been committed yet.  */
  ultra_pipe.group[pipe_slot] = *ip;
  ultra_pipe.codes[pipe_slot] = type;
  ultra_pipe.contents[type] = 1;
  if (UMASK (type) &
      (UMASK (IEUN) | UMASK (IEU0) | UMASK (IEU1)))
    ultra_pipe.num_ieu_insns += 1;

  ultra_pipe.free_slot_mask = (mask & ~(1 << pipe_slot));
  ultra_pipe.group_size += 1;
  ultra_pipe.commit[pipe_slot] = 0;

  /* Update ready list.  */
  temp = *ip;
  while (ip != &ready[this])
    {
      ip[0] = ip[1];
      ++ip;
    }
  *ip = temp;
}

/* Advance to the next pipeline group.  */
static void
ultra_flush_pipeline ()
{
  ultra_cur_hist = (ultra_cur_hist + 1) & (ULTRA_NUM_HIST - 1);
  ultra_cycles_elapsed += 1;
  bzero ((char *) &ultra_pipe, sizeof ultra_pipe);
  ultra_pipe.free_slot_mask = 0xf;
}

/* Init our data structures for this current block.  */
void
ultrasparc_sched_init (dump, sched_verbose)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
{
  bzero ((char *) ultra_pipe_hist, sizeof ultra_pipe_hist);
  ultra_cur_hist = 0;
  ultra_cycles_elapsed = 0;
  ultra_pipe.free_slot_mask = 0xf;
}

/* INSN has been scheduled, update pipeline commit state
   and return how many instructions are still to be
   scheduled in this group.  */
int
ultrasparc_variable_issue (insn)
     rtx insn;
{
  struct ultrasparc_pipeline_state *up = &ultra_pipe;
  int i, left_to_fire;

  left_to_fire = 0;
  for (i = 0; i < 4; i++)
    {
      if (up->group[i] == 0)
	continue;

      if (up->group[i] == insn)
	{
	  up->commit[i] = 1;
	}
      else if (! up->commit[i])
	left_to_fire++;
    }

  return left_to_fire;
}

/* In actual_hazard_this_instance, we may have yanked some
   instructions from the ready list due to conflict cost
   adjustments.  If so, and such an insn was in our pipeline
   group, remove it and update state.  */
static void
ultra_rescan_pipeline_state (ready, n_ready)
     rtx *ready;
     int n_ready;
{
  struct ultrasparc_pipeline_state *up = &ultra_pipe;
  int i;

  for (i = 0; i < 4; i++)
    {
      rtx insn = up->group[i];
      int j;

      if (! insn)
	continue;

      /* If it has been committed, then it was removed from
	 the ready list because it was actually scheduled,
	 and that is not the case we are searching for here.  */
      if (up->commit[i] != 0)
	continue;

      for (j = n_ready - 1; j >= 0; j--)
	if (ready[j] == insn)
	  break;

      /* If we didn't find it, toss it.  */
      if (j < 0)
	{
	  enum ultra_code ucode = up->codes[i];

	  up->group[i] = 0;
	  up->codes[i] = NONE;
	  up->contents[ucode] = 0;
	  if (UMASK (ucode) &
	      (UMASK (IEUN) | UMASK (IEU0) | UMASK (IEU1)))
	    up->num_ieu_insns -= 1;

	  up->free_slot_mask |= (1 << i);
	  up->group_size -= 1;
	  up->commit[i] = 0;
	}
    }
}

void
ultrasparc_sched_reorder (dump, sched_verbose, ready, n_ready)
     FILE *dump;
     int sched_verbose;
     rtx *ready;
     int n_ready;
{
  struct ultrasparc_pipeline_state *up = &ultra_pipe;
  int i, this_insn;

  if (sched_verbose)
    {
      int n;

      fprintf (dump, "\n;;\tUltraSPARC Looking at [");
      for (n = n_ready - 1; n >= 0; n--)
	{
	  rtx insn = ready[n];
	  enum ultra_code ucode;

	  if (recog_memoized (insn) < 0)
	    continue;
	  ucode = ultra_code_from_mask (TMASK (get_attr_type (insn)));
	  if (n != 0)
	    fprintf (dump, "%s(%d) ",
		     ultra_code_names[ucode],
		     INSN_UID (insn));
	  else
	    fprintf (dump, "%s(%d)",
		     ultra_code_names[ucode],
		     INSN_UID (insn));
	}
      fprintf (dump, "]\n");
    }

  this_insn = n_ready - 1;

  /* Skip over junk we don't understand.  */
  while ((this_insn >= 0)
	 && recog_memoized (ready[this_insn]) < 0)
    this_insn--;

  ultra_build_types_avail (ready, this_insn + 1);

  while (this_insn >= 0) {
    int old_group_size = up->group_size;

    if (up->group_size != 0)
      {
	int num_committed;

	num_committed = (up->commit[0] + up->commit[1] +
			 up->commit[2] + up->commit[3]);
	/* If nothing has been commited from our group, or all of
	   them have.  Clear out the (current cycle's) pipeline
	   state and start afresh.  */
	if (num_committed == 0
	    || num_committed == up->group_size)
	  {
	    ultra_flush_pipeline ();
	    up = &ultra_pipe;
	    old_group_size = 0;
	  }
	else
	  {
	    /* OK, some ready list insns got requeued and thus removed
	       from the ready list.  Account for this fact.  */
	    ultra_rescan_pipeline_state (ready, n_ready);

	    /* Something "changed", make this look like a newly
	       formed group so the code at the end of the loop
	       knows that progress was in fact made.  */
	    if (up->group_size != old_group_size)
	      old_group_size = 0;
	  }
      }

    if (up->group_size == 0)
      {
	/* If the pipeline is (still) empty and we have any single
	   group insns, get them out now as this is a good time.  */
	rtx *ip = ultra_find_type ((TMASK (TYPE_RETURN) | TMASK (TYPE_ADDRESS) |
				    TMASK (TYPE_IMUL) | TMASK (TYPE_CMOVE) |
				    TMASK (TYPE_MULTI) | TMASK (TYPE_MISC)),
				   ready, this_insn);
	if (ip)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, SINGLE);
	    break;
	  }

	/* If we are not in the process of emptying out the pipe, try to
	   obtain an instruction which must be the first in it's group.  */
	ip = ultra_find_type ((TMASK (TYPE_CALL) |
			       TMASK (TYPE_CALL_NO_DELAY_SLOT) |
			       TMASK (TYPE_UNCOND_BRANCH)),
			      ready, this_insn);
	if (ip)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, IEU1);
	    this_insn--;
	  }
	else if ((ip = ultra_find_type ((TMASK (TYPE_FPDIVS) |
					 TMASK (TYPE_FPDIVD) |
					 TMASK (TYPE_FPSQRTS) |
					 TMASK (TYPE_FPSQRTD)),
					ready, this_insn)) != 0)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, FPM);
	    this_insn--;
	  }
      }

    /* Try to fill the integer pipeline.  First, look for an IEU0 specific
       operation.  We can't do more IEU operations if the first 3 slots are
       all full or we have dispatched two IEU insns already.  */
    if ((up->free_slot_mask & 0x7) != 0
	&& up->num_ieu_insns < 2
	&& up->contents[IEU0] == 0
	&& up->contents[IEUN] == 0)
      {
	rtx *ip = ultra_find_type (TMASK(TYPE_SHIFT), ready, this_insn);
	if (ip)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, IEU0);
	    this_insn--;
	  }
      }

    /* If we can, try to find an IEU1 specific or an unnamed
       IEU instruction.  */
    if ((up->free_slot_mask & 0x7) != 0
	&& up->num_ieu_insns < 2)
      {
	rtx *ip = ultra_find_type ((TMASK (TYPE_IALU) | TMASK (TYPE_BINARY) |
				    TMASK (TYPE_MOVE) | TMASK (TYPE_UNARY) |
				    (up->contents[IEU1] == 0 ? TMASK (TYPE_COMPARE) : 0)),
				   ready, this_insn);
	if (ip)
	  {
	    rtx insn = *ip;

	    ultra_schedule_insn (ip, ready, this_insn,
				 (!up->contents[IEU1]
				  && get_attr_type (insn) == TYPE_COMPARE)
				 ? IEU1 : IEUN);
	    this_insn--;
	  }
      }

    /* If only one IEU insn has been found, try to find another unnamed
       IEU operation or an IEU1 specific one.  */
    if ((up->free_slot_mask & 0x7) != 0
	&& up->num_ieu_insns < 2)
      {
	rtx *ip;
	int tmask = (TMASK (TYPE_IALU) | TMASK (TYPE_BINARY) |
		     TMASK (TYPE_MOVE) | TMASK (TYPE_UNARY));

	if (!up->contents[IEU1])
	  tmask |= TMASK (TYPE_COMPARE);
	ip = ultra_find_type (tmask, ready, this_insn);
	if (ip)
	  {
	    rtx insn = *ip;

	    ultra_schedule_insn (ip, ready, this_insn,
				 (!up->contents[IEU1]
				  && get_attr_type (insn) == TYPE_COMPARE)
				 ? IEU1 : IEUN);
	    this_insn--;
	  }
      }

    /* Try for a load or store, but such an insn can only be issued
       if it is within' one of the first 3 slots.  */
    if ((up->free_slot_mask & 0x7) != 0
        && up->contents[LSU] == 0)
      {
	rtx *ip = ultra_find_type ((TMASK (TYPE_LOAD) | TMASK (TYPE_SLOAD) |
				   TMASK (TYPE_STORE) | TMASK (TYPE_FPLOAD) |
				   TMASK (TYPE_FPSTORE)), ready, this_insn);
	if (ip)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, LSU);
	    this_insn--;
	  }
      }

    /* Now find FPU operations, first FPM class.  But not divisions or
       square-roots because those will break the group up.  Unlike all
       the previous types, these can go in any slot.  */
    if (up->free_slot_mask != 0
	&& up->contents[FPM] == 0)
      {
	rtx *ip = ultra_find_type (TMASK (TYPE_FPMUL), ready, this_insn);
	if (ip)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, FPM);
	    this_insn--;
	  }
      }
    
    /* Continue on with FPA class if we have not filled the group already.  */
    if (up->free_slot_mask != 0
	&& up->contents[FPA] == 0)
      {
	rtx *ip = ultra_find_type ((TMASK (TYPE_FPMOVE) | TMASK (TYPE_FPCMOVE) |
				    TMASK (TYPE_FP) | TMASK (TYPE_FPCMP)),
				   ready, this_insn);
	if (ip)
	  {
	    ultra_schedule_insn (ip, ready, this_insn, FPA);
	    this_insn--;
	  }
      }

    /* Finally, maybe stick a branch in here.  */
    if (up->free_slot_mask != 0
	&& up->contents[CTI] == 0)
      {
	rtx *ip = ultra_find_type (TMASK (TYPE_BRANCH), ready, this_insn);

	/* Try to slip in a branch only if it is one of the
	   next 2 in the ready list.  */
	if (ip && ((&ready[this_insn] - ip) < 2))
	  {
	    ultra_schedule_insn (ip, ready, this_insn, CTI);
	    this_insn--;
	  }
      }

    up->group_size = 0;
    for (i = 0; i < 4; i++)
      if ((up->free_slot_mask & (1 << i)) == 0)
	up->group_size++;

    /* See if we made any progress...  */
    if (old_group_size != up->group_size)
      break;

    /* Clean out the (current cycle's) pipeline state
       and try once more.  If we placed no instructions
       into the pipeline at all, it means a real hard
       conflict exists with some earlier issued instruction
       so we must advance to the next cycle to clear it up.  */
    if (up->group_size == 0)
      {
	ultra_flush_pipeline ();
	up = &ultra_pipe;
      }
    else
      {
	bzero ((char *) &ultra_pipe, sizeof ultra_pipe);
	ultra_pipe.free_slot_mask = 0xf;
      }
  }

  if (sched_verbose)
    {
      int n, gsize;

      fprintf (dump, ";;\tUltraSPARC Launched   [");
      gsize = up->group_size;
      for (n = 0; n < 4; n++)
	{
	  rtx insn = up->group[n];

	  if (! insn)
	    continue;

	  gsize -= 1;
	  if (gsize != 0)
	    fprintf (dump, "%s(%d) ",
		     ultra_code_names[up->codes[n]],
		     INSN_UID (insn));
	  else
	    fprintf (dump, "%s(%d)",
		     ultra_code_names[up->codes[n]],
		     INSN_UID (insn));
	}
      fprintf (dump, "]\n");
    }
}

int                                                           
sparc_issue_rate ()
{
  switch (sparc_cpu)
    {
    default:                                 
      return 1;                                                    
    case PROCESSOR_V9:                                                
      /* Assume V9 processors are capable of at least dual-issue.  */
      return 2;
    case PROCESSOR_SUPERSPARC:                                        
      return 3;                                                      
    case PROCESSOR_HYPERSPARC:
    case PROCESSOR_SPARCLITE86X:
      return 2;
    case PROCESSOR_ULTRASPARC:                                            
      return 4;                                                    
    }
}

static int
set_extends(x, insn)
     rtx x, insn;
{
  register rtx pat = PATTERN (insn);

  switch (GET_CODE (SET_SRC (pat)))
    {
      /* Load and some shift instructions zero extend. */
    case MEM:
    case ZERO_EXTEND:
      /* sethi clears the high bits */
    case HIGH:
      /* LO_SUM is used with sethi.  sethi cleared the high
	 bits and the values used with lo_sum are positive */
    case LO_SUM:
      /* Store flag stores 0 or 1 */
    case LT: case LTU:
    case GT: case GTU:
    case LE: case LEU:
    case GE: case GEU:
    case EQ:
    case NE:
      return 1;
    case AND:
      {
	rtx op1 = XEXP (SET_SRC (pat), 1);
	if (GET_CODE (op1) == CONST_INT)
	  return INTVAL (op1) >= 0;
	if (GET_CODE (XEXP (SET_SRC (pat), 0)) == REG
	    && sparc_check_64 (XEXP (SET_SRC (pat), 0), insn) == 1)
	  return 1;
	if (GET_CODE (op1) == REG
	    && sparc_check_64 ((op1), insn) == 1)
	  return 1;
      }
    case ASHIFT:
    case LSHIFTRT:
      return GET_MODE (SET_SRC (pat)) == SImode;
      /* Positive integers leave the high bits zero. */
    case CONST_DOUBLE:
      return ! (CONST_DOUBLE_LOW (x) & 0x80000000);
    case CONST_INT:
      return ! (INTVAL (x) & 0x80000000);
    case ASHIFTRT:
    case SIGN_EXTEND:
      return - (GET_MODE (SET_SRC (pat)) == SImode);
    default:
      return 0;
    }
}

/* We _ought_ to have only one kind per function, but... */
static rtx sparc_addr_diff_list;
static rtx sparc_addr_list;

void
sparc_defer_case_vector (lab, vec, diff)
     rtx lab, vec;
     int diff;
{
  vec = gen_rtx_EXPR_LIST (VOIDmode, lab, vec);
  if (diff)
    sparc_addr_diff_list
      = gen_rtx_EXPR_LIST (VOIDmode, vec, sparc_addr_diff_list);
  else
    sparc_addr_list = gen_rtx_EXPR_LIST (VOIDmode, vec, sparc_addr_list);
}

static void 
sparc_output_addr_vec (vec)
     rtx vec;
{
  rtx lab = XEXP (vec, 0), body = XEXP (vec, 1);
  int idx, vlen = XVECLEN (body, 0);

#ifdef ASM_OUTPUT_ADDR_VEC_START  
  ASM_OUTPUT_ADDR_VEC_START (asm_out_file);
#endif

#ifdef ASM_OUTPUT_CASE_LABEL
  ASM_OUTPUT_CASE_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (lab),
			 NEXT_INSN (lab));
#else
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (lab));
#endif

  for (idx = 0; idx < vlen; idx++)
    {
      ASM_OUTPUT_ADDR_VEC_ELT
	(asm_out_file, CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 0, idx), 0)));
    }
    
#ifdef ASM_OUTPUT_ADDR_VEC_END
  ASM_OUTPUT_ADDR_VEC_END (asm_out_file);
#endif
}

static void 
sparc_output_addr_diff_vec (vec)
     rtx vec;
{
  rtx lab = XEXP (vec, 0), body = XEXP (vec, 1);
  rtx base = XEXP (XEXP (body, 0), 0);
  int idx, vlen = XVECLEN (body, 1);

#ifdef ASM_OUTPUT_ADDR_VEC_START  
  ASM_OUTPUT_ADDR_VEC_START (asm_out_file);
#endif

#ifdef ASM_OUTPUT_CASE_LABEL
  ASM_OUTPUT_CASE_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (lab),
			 NEXT_INSN (lab));
#else
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (lab));
#endif

  for (idx = 0; idx < vlen; idx++)
    {
      ASM_OUTPUT_ADDR_DIFF_ELT
        (asm_out_file,
         body,
         CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 1, idx), 0)),
         CODE_LABEL_NUMBER (base));
    }
    
#ifdef ASM_OUTPUT_ADDR_VEC_END
  ASM_OUTPUT_ADDR_VEC_END (asm_out_file);
#endif
}

static void
sparc_output_deferred_case_vectors ()
{
  rtx t;
  int align;

  if (sparc_addr_list == NULL_RTX
      && sparc_addr_diff_list == NULL_RTX)
    return;

  /* Align to cache line in the function's code section.  */
  function_section (current_function_decl);

  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);
  
  for (t = sparc_addr_list; t ; t = XEXP (t, 1))
    sparc_output_addr_vec (XEXP (t, 0));
  for (t = sparc_addr_diff_list; t ; t = XEXP (t, 1))
    sparc_output_addr_diff_vec (XEXP (t, 0));

  sparc_addr_list = sparc_addr_diff_list = NULL_RTX;
}

/* Return 0 if the high 32 bits of X (the low word of X, if DImode) are
   unknown.  Return 1 if the high bits are zero, -1 if the register is
   sign extended.  */
int
sparc_check_64 (x, insn)
     rtx x, insn;
{
  /* If a register is set only once it is safe to ignore insns this
     code does not know how to handle.  The loop will either recognize
     the single set and return the correct value or fail to recognize
     it and return 0.  */
  int set_once = 0;

  if (GET_CODE (x) == REG
      && flag_expensive_optimizations
      && REG_N_SETS (REGNO (x)) == 1)
    set_once = 1;

  if (insn == 0)
    {
      if (set_once)
	insn = get_last_insn_anywhere ();
      else
	return 0;
    }

  while ((insn = PREV_INSN (insn)))
    {
      switch (GET_CODE (insn))
	{
	case JUMP_INSN:
	case NOTE:
	  break;
	case CODE_LABEL:
	case CALL_INSN:
	default:
	  if (! set_once)
	    return 0;
	  break;
	case INSN:
	  {
	    rtx pat = PATTERN (insn);
	    if (GET_CODE (pat) != SET)
	      return 0;
	    if (rtx_equal_p (x, SET_DEST (pat)))
	      return set_extends (x, insn);
	    if (reg_overlap_mentioned_p (SET_DEST (pat), x))
	      return 0;
	  }
	}
    }
  return 0;
}

char *
sparc_v8plus_shift (operands, insn, opcode)
     rtx *operands;
     rtx insn;
     const char *opcode;
{
  static char asm_code[60];

  if (GET_CODE (operands[3]) == SCRATCH)
    operands[3] = operands[0];
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      output_asm_insn ("mov %1,%3", operands);
    }
  else
    {
      output_asm_insn ("sllx %H1,32,%3", operands);
      if (sparc_check_64 (operands[1], insn) <= 0)
	output_asm_insn ("srl %L1,0,%L1", operands);
      output_asm_insn ("or %L1,%3,%3", operands);
    }

  strcpy(asm_code, opcode);
  if (which_alternative != 2)
    return strcat (asm_code, " %0,%2,%L0\n\tsrlx %L0,32,%H0");
  else
    return strcat (asm_code, " %3,%2,%3\n\tsrlx %3,32,%H0\n\tmov %3,%L0");
}


/* Return 1 if DEST and SRC reference only global and in registers. */

int
sparc_return_peephole_ok (dest, src)
     rtx dest, src;
{
  if (! TARGET_V9)
    return 0;
  if (current_function_uses_only_leaf_regs)
    return 0;
  if (GET_CODE (src) != CONST_INT
      && (GET_CODE (src) != REG || ! IN_OR_GLOBAL_P (src)))
    return 0;
  return IN_OR_GLOBAL_P (dest);
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.

   32 bit sparc uses %g2 as the STATIC_CHAIN_REGNUM which gets clobbered
   during profiling so we need to save/restore it around the call to mcount.
   We're guaranteed that a save has just been done, and we use the space
   allocated for intreg/fpreg value passing.  */

void
sparc_function_profiler (file, labelno)
     FILE *file;
     int labelno;
{
  char buf[32];
  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);

  if (! TARGET_ARCH64)
    fputs ("\tst\t%g2,[%fp-4]\n", file);

  fputs ("\tsethi\t%hi(", file);
  assemble_name (file, buf);
  fputs ("),%o0\n", file);

  fputs ("\tcall\t", file);
  assemble_name (file, MCOUNT_FUNCTION);
  putc ('\n', file);

  fputs ("\t or\t%o0,%lo(", file);
  assemble_name (file, buf);
  fputs ("),%o0\n", file);

  if (! TARGET_ARCH64)
    fputs ("\tld\t[%fp-4],%g2\n", file);
}


/* The following macro shall output assembler code to FILE
   to initialize basic-block profiling.

   If profile_block_flag == 2

	Output code to call the subroutine `__bb_init_trace_func'
	and pass two parameters to it. The first parameter is
	the address of a block allocated in the object module.
	The second parameter is the number of the first basic block
	of the function.

	The name of the block is a local symbol made with this statement:
	
	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result.

	The number of the first basic block of the function is
	passed to the macro in BLOCK_OR_LABEL.

	If described in a virtual assembler language the code to be
	output looks like:

		parameter1 <- LPBX0
		parameter2 <- BLOCK_OR_LABEL
		call __bb_init_trace_func

    else if profile_block_flag != 0

	Output code to call the subroutine `__bb_init_func'
	and pass one single parameter to it, which is the same
	as the first parameter to `__bb_init_trace_func'.

	The first word of this parameter is a flag which will be nonzero if
	the object module has already been initialized.  So test this word
	first, and do not call `__bb_init_func' if the flag is nonzero.
	Note: When profile_block_flag == 2 the test need not be done
	but `__bb_init_trace_func' *must* be called.

	BLOCK_OR_LABEL may be used to generate a label number as a
	branch destination in case `__bb_init_func' will not be called.

	If described in a virtual assembler language the code to be
	output looks like:

		cmp (LPBX0),0
		jne local_label
		parameter1 <- LPBX0
		call __bb_init_func
	    local_label:

*/

void
sparc_function_block_profiler(file, block_or_label)
     FILE *file;
     int block_or_label;
{
  char LPBX[32];
  ASM_GENERATE_INTERNAL_LABEL (LPBX, "LPBX", 0);

  if (profile_block_flag == 2)
    {
      fputs ("\tsethi\t%hi(", file);
      assemble_name (file, LPBX);
      fputs ("),%o0\n", file);
  
      fprintf (file, "\tsethi\t%%hi(%d),%%o1\n", block_or_label);

      fputs ("\tor\t%o0,%lo(", file);
      assemble_name (file, LPBX);
      fputs ("),%o0\n", file);
  
      fprintf (file, "\tcall\t%s__bb_init_trace_func\n", user_label_prefix);

      fprintf (file, "\t or\t%%o1,%%lo(%d),%%o1\n", block_or_label);
    }
  else if (profile_block_flag != 0)
    {
      char LPBY[32];
      ASM_GENERATE_INTERNAL_LABEL (LPBY, "LPBY", block_or_label);

      fputs ("\tsethi\t%hi(", file);
      assemble_name (file, LPBX);
      fputs ("),%o0\n", file);
      
      fputs ("\tld\t[%lo(", file);
      assemble_name (file, LPBX);
      fputs (")+%o0],%o1\n", file);

      fputs ("\ttst\t%o1\n", file);

      if (TARGET_V9)
	{
	  fputs ("\tbne,pn\t%icc,", file);
	  assemble_name (file, LPBY);
	  putc ('\n', file);
	}
      else
	{
	  fputs ("\tbne\t", file);
	  assemble_name (file, LPBY);
	  putc ('\n', file);
	}

      fputs ("\t or\t%o0,%lo(", file);
      assemble_name (file, LPBX);
      fputs ("),%o0\n", file);

      fprintf (file, "\tcall\t%s__bb_init_func\n\t nop\n", user_label_prefix);

      ASM_OUTPUT_INTERNAL_LABEL (file, "LPBY", block_or_label);
    }
}

/* The following macro shall output assembler code to FILE
   to increment a counter associated with basic block number BLOCKNO.

   If profile_block_flag == 2

	Output code to initialize the global structure `__bb' and
	call the function `__bb_trace_func' which will increment the
	counter.

	`__bb' consists of two words. In the first word the number
	of the basic block has to be stored. In the second word
	the address of a block allocated in the object module 
	has to be stored.

	The basic block number is given by BLOCKNO.

	The address of the block is given by the label created with 

	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

	by FUNCTION_BLOCK_PROFILER.

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result.

	If described in a virtual assembler language the code to be
	output looks like:

		move BLOCKNO -> (__bb)
		move LPBX0 -> (__bb+4)
		call __bb_trace_func

	Note that function `__bb_trace_func' must not change the
	machine state, especially the flag register. To grant
	this, you must output code to save and restore registers
	either in this macro or in the macros MACHINE_STATE_SAVE
	and MACHINE_STATE_RESTORE. The last two macros will be
	used in the function `__bb_trace_func', so you must make
	sure that the function prologue does not change any 
	register prior to saving it with MACHINE_STATE_SAVE.

   else if profile_block_flag != 0

	Output code to increment the counter directly.
	Basic blocks are numbered separately from zero within each
	compiled object module. The count associated with block number
	BLOCKNO is at index BLOCKNO in an array of words; the name of 
	this array is a local symbol made with this statement:

	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 2);

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result. 

	If described in a virtual assembler language, the code to be
	output looks like:

		inc (LPBX2+4*BLOCKNO)

*/

void
sparc_block_profiler(file, blockno)
     FILE *file;
     int blockno;
{
  char LPBX[32];
  int bbreg = TARGET_ARCH64 ? 4 : 2;

  if (profile_block_flag == 2)
    {
      ASM_GENERATE_INTERNAL_LABEL (LPBX, "LPBX", 0);

      fprintf (file, "\tsethi\t%%hi(%s__bb),%%g1\n", user_label_prefix);
      fprintf (file, "\tsethi\t%%hi(%d),%%g%d\n", blockno, bbreg);
      fprintf (file, "\tor\t%%g1,%%lo(%s__bb),%%g1\n", user_label_prefix);
      fprintf (file, "\tor\t%%g%d,%%lo(%d),%%g%d\n", bbreg, blockno, bbreg);

      fprintf (file, "\tst\t%%g%d,[%%g1]\n", bbreg);

      fputs ("\tsethi\t%hi(", file);
      assemble_name (file, LPBX);
      fprintf (file, "),%%g%d\n", bbreg);
  
      fputs ("\tor\t%o2,%lo(", file);
      assemble_name (file, LPBX);
      fprintf (file, "),%%g%d\n", bbreg);
  
      fprintf (file, "\tst\t%%g%d,[%%g1+4]\n", bbreg);
      fprintf (file, "\tmov\t%%o7,%%g%d\n", bbreg);

      fprintf (file, "\tcall\t%s__bb_trace_func\n\t nop\n", user_label_prefix);

      fprintf (file, "\tmov\t%%g%d,%%o7\n", bbreg);
    }
  else if (profile_block_flag != 0)
    {
      ASM_GENERATE_INTERNAL_LABEL (LPBX, "LPBX", 2);

      fputs ("\tsethi\t%hi(", file);
      assemble_name (file, LPBX);
      fprintf (file, "+%d),%%g1\n", blockno*4);

      fputs ("\tld\t[%g1+%lo(", file);
      assemble_name (file, LPBX);
      if (TARGET_ARCH64 && USE_AS_OFFSETABLE_LO10)
	fprintf (file, ")+%d],%%g%d\n", blockno*4, bbreg);
      else
	fprintf (file, "+%d)],%%g%d\n", blockno*4, bbreg);

      fprintf (file, "\tadd\t%%g%d,1,%%g%d\n", bbreg, bbreg);

      fprintf (file, "\tst\t%%g%d,[%%g1+%%lo(", bbreg);
      assemble_name (file, LPBX);
      if (TARGET_ARCH64 && USE_AS_OFFSETABLE_LO10)
	fprintf (file, ")+%d]\n", blockno*4);
      else
	fprintf (file, "+%d)]\n", blockno*4);
    }
}

/* The following macro shall output assembler code to FILE
   to indicate a return from function during basic-block profiling.

   If profile_block_flag == 2:

	Output assembler code to call function `__bb_trace_ret'.

	Note that function `__bb_trace_ret' must not change the
	machine state, especially the flag register. To grant
	this, you must output code to save and restore registers
	either in this macro or in the macros MACHINE_STATE_SAVE_RET
	and MACHINE_STATE_RESTORE_RET. The last two macros will be
	used in the function `__bb_trace_ret', so you must make
	sure that the function prologue does not change any 
	register prior to saving it with MACHINE_STATE_SAVE_RET.

   else if profile_block_flag != 0:

	The macro will not be used, so it need not distinguish
	these cases.
*/

void
sparc_function_block_profiler_exit(file)
     FILE *file;
{
  if (profile_block_flag == 2)
    fprintf (file, "\tcall\t%s__bb_trace_ret\n\t nop\n", user_label_prefix);
  else
    abort ();
}

/* Mark ARG, which is really a struct ultrasparc_pipline_state *, for
   GC.  */

static void
mark_ultrasparc_pipeline_state (arg)
     void *arg;
{
  struct ultrasparc_pipeline_state *ups;
  size_t i;

  ups = (struct ultrasparc_pipeline_state *) arg;
  for (i = 0; i < sizeof (ups->group) / sizeof (rtx); ++i)
    ggc_mark_rtx (ups->group[i]);
}

/* Called to register all of our global variables with the garbage
   collector.  */

static void
sparc_add_gc_roots ()
{
  ggc_add_rtx_root (&sparc_compare_op0, 1);
  ggc_add_rtx_root (&sparc_compare_op1, 1);
  ggc_add_rtx_root (&leaf_label, 1);
  ggc_add_rtx_root (&global_offset_table, 1);
  ggc_add_rtx_root (&get_pc_symbol, 1);
  ggc_add_rtx_root (&sparc_addr_diff_list, 1);
  ggc_add_rtx_root (&sparc_addr_list, 1);
  ggc_add_root (ultra_pipe_hist, 
		sizeof (ultra_pipe_hist) / sizeof (ultra_pipe_hist[0]),
		sizeof (ultra_pipe_hist[0]),
		&mark_ultrasparc_pipeline_state);
}
