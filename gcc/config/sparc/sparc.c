/* Subroutines for insn-output.c for Sun SPARC.
   Copyright (C) 1987, 88, 89, 92-97, 1998 Free Software Foundation, Inc.
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
#include <stdio.h>
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
#include "expr.h"
#include "recog.h"

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

static char *frame_base_name;
static int frame_base_offset;

static rtx pic_setup_code	PROTO((void));
static rtx find_addr_reg	PROTO((rtx));
static void sparc_init_modes	PROTO((void));
static int save_regs		PROTO((FILE *, int, int, char *,
				       int, int, int));
static int restore_regs		PROTO((FILE *, int, int, char *, int, int));
static void build_big_number	PROTO((FILE *, int, char *));
static function_arg_slotno	PROTO((const CUMULATIVE_ARGS *,
				       enum machine_mode, tree, int, int,
				       int *, int *));

#ifdef DWARF2_DEBUGGING_INFO
extern char *dwarf2out_cfi_label ();
#endif

/* Option handling.  */

/* Code model option as passed by user.  */
char *sparc_cmodel_string;
/* Parsed value.  */
enum cmodel sparc_cmodel;

/* Record alignment options as passed by user.  */
char *sparc_align_loops_string;
char *sparc_align_jumps_string;
char *sparc_align_funcs_string;

/* Parsed values, as a power of two.  */
int sparc_align_loops;
int sparc_align_jumps;
int sparc_align_funcs;

struct sparc_cpu_select sparc_select[] =
{
  /* switch	name,		tune	arch */
  { (char *)0,	"default",	1,	1 },
  { (char *)0,	"-mcpu=",	1,	1 },
  { (char *)0,	"-mtune=",	1,	0 },
  { 0, 0 }
};

/* CPU type.  This is set from TARGET_CPU_DEFAULT and -m{cpu,tune}=xxx.  */
enum processor_type sparc_cpu;

/* Validate and override various options, and do some machine dependent
   initialization.  */

void
sparc_override_options ()
{
  static struct code_model {
    char *name;
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
    char *name;
  } cpu_default[] = {
    /* There must be one entry here for each TARGET_CPU value.  */
    { TARGET_CPU_sparc, "cypress" },
    { TARGET_CPU_sparclet, "tsc701" },
    { TARGET_CPU_sparclite, "f930" },
    { TARGET_CPU_v8, "v8" },
    { TARGET_CPU_supersparc, "supersparc" },
    { TARGET_CPU_v9, "v9" },
    { TARGET_CPU_ultrasparc, "ultrasparc" },
    { 0 }
  };
  struct cpu_default *def;
  /* Table of values for -m{cpu,tune}=.  */
  static struct cpu_table {
    char *name;
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
    { "sparclet",   PROCESSOR_SPARCLET, MASK_ISA, MASK_SPARCLET },
    /* TEMIC sparclet */
    { "tsc701",     PROCESSOR_TSC701, MASK_ISA, MASK_SPARCLET },
    /* "v8plus" is what Sun calls Solaris2.5 running on UltraSPARC's.  */
    { "v8plus",     PROCESSOR_V8PLUS, MASK_ISA, MASK_V9 },
    { "v9",         PROCESSOR_V9, MASK_ISA, MASK_V9 },
    /* TI ultrasparc */
    { "ultrasparc", PROCESSOR_ULTRASPARC, MASK_ISA, MASK_V9 },
    { 0 }
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

  /* Code model selection.  */
  sparc_cmodel = SPARC_DEFAULT_CMODEL;
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
     the processor default.  */
  if (TARGET_FPU_SET)
    target_flags = (target_flags & ~MASK_FPU) | fpu;

  /* Use the deprecated v8 insns for sparc64 in 32 bit mode.  */
  if (TARGET_V9 && TARGET_ARCH32)
    target_flags |= MASK_DEPRECATED_V8_INSNS;

  /* Validate -malign-loops= value, or provide default.  */
  if (sparc_align_loops_string)
    {
      sparc_align_loops = exact_log2 (atoi (sparc_align_loops_string));
      if (sparc_align_loops < 2 || sparc_align_loops > 7)
	fatal ("-malign-loops=%s is not between 4 and 128 or is not a power of two",
	       sparc_align_loops_string);
    }
  else
    {
      /* ??? This relies on ASM_OUTPUT_ALIGN to not emit the alignment if
	 its 0.  This sounds a bit kludgey.  */
      sparc_align_loops = 0;
    }

  /* Validate -malign-jumps= value, or provide default.  */
  if (sparc_align_jumps_string)
    {
      sparc_align_jumps = exact_log2 (atoi (sparc_align_jumps_string));
      if (sparc_align_jumps < 2 || sparc_align_loops > 7)
	fatal ("-malign-jumps=%s is not between 4 and 128 or is not a power of two",
	       sparc_align_jumps_string);
    }
  else
    {
      /* ??? This relies on ASM_OUTPUT_ALIGN to not emit the alignment if
	 its 0.  This sounds a bit kludgey.  */
      sparc_align_jumps = 0;
    }

  /* Validate -malign-functions= value, or provide default. */
  if (sparc_align_funcs_string)
    {
      sparc_align_funcs = exact_log2 (atoi (sparc_align_funcs_string));
      if (sparc_align_funcs < 2 || sparc_align_loops > 7)
	fatal ("-malign-functions=%s is not between 4 and 128 or is not a power of two",
	       sparc_align_funcs_string);
    }
  else
    sparc_align_funcs = DEFAULT_SPARC_ALIGN_FUNCS;

  /* Do various machine dependent initializations.  */
  sparc_init_modes ();
}

/* Float conversions (v9 only).

   The floating point registers cannot hold DImode values because SUBREG's
   on them get the wrong register.   "(subreg:SI (reg:DI M int-reg) 0)" is the
   same as "(subreg:SI (reg:DI N float-reg) 1)", but gcc doesn't know how to
   turn the "0" to a "1".  Therefore, we must explicitly do the conversions
   to/from int/fp regs.  `sparc64_fpconv_stack_slot' is the address of an
   8 byte stack slot used during the transfer.
   ??? I could have used [%fp-16] but I didn't want to add yet another
   dependence on this.  */
/* ??? Can we use assign_stack_temp here?  */

static rtx fpconv_stack_temp;

/* Called once for each function.  */

void
sparc_init_expanders ()
{
  fpconv_stack_temp = NULL_RTX;
}

/* Assign a stack temp for fp/int DImode conversions.  */

rtx
sparc64_fpconv_stack_temp ()
{
  if (fpconv_stack_temp == NULL_RTX)
    fpconv_stack_temp =
      assign_stack_local (DImode, GET_MODE_SIZE (DImode), 0);

  return fpconv_stack_temp;
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
   or const0_rtx.  Don't allow const0_rtx if TARGET_LIVE_G0 because
   %g0 may contain anything.  */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;
  if (TARGET_LIVE_G0)
    return 0;
  if (op == const0_rtx)
    return 1;
  if (GET_MODE (op) == VOIDmode && GET_CODE (op) == CONST_DOUBLE
      && CONST_DOUBLE_HIGH (op) == 0
      && CONST_DOUBLE_LOW (op) == 0)
    return 1;
  if (GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT
      && GET_CODE (op) == CONST_DOUBLE
      && fp_zero_operand (op))
    return 1;
  return 0;
}

/* Nonzero if OP is a floating point value with value 0.0.  */

int
fp_zero_operand (op)
     rtx op;
{
  REAL_VALUE_TYPE r;

  REAL_VALUE_FROM_CONST_DOUBLE (r, op);
  return (REAL_VALUES_EQUAL (r, dconst0) && ! REAL_VALUE_MINUS_ZERO (r));
}

/* Nonzero if OP is an integer register.  */

int
intreg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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

      /* ??? This clause seems to be irrelevant.  */
    case CONST_DOUBLE:
      return GET_MODE (op) == mode;

    default:
      return 0;
    }
}

/* Return truth value of statement that OP is a symbolic memory
   operand of mode MODE.  */

int
symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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
     enum machine_mode mode;
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
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
      return ! SYMBOL_REF_FLAG (op);
    case PLUS :
      /* Assume canonical format of symbol + constant.
	 Fall through.  */
    case CONST :
      return data_segment_operand (XEXP (op, 0));
    default :
      return 0;
    }
}

/* Return 1 if the operand is a text segment reference.
   This is needed in the medium/anywhere code model on v9.  */

int
text_segment_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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
      return text_segment_operand (XEXP (op, 0));
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
sparc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (register_operand (op, mode))
    return 1;
  if (GET_CODE (op) == CONST_INT)
    return SMALL_INT (op);
  if (GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  if (GET_CODE (op) == LO_SUM)
    return (GET_CODE (XEXP (op, 0)) == REG
	    && symbolic_operand (XEXP (op, 1), Pmode));
  return memory_address_p (mode, op);
}

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == DImode && arith_double_operand (op, mode))
    return 1;
  if (register_operand (op, mode))
    return 1;
  if (GET_CODE (op) == CONST_INT)
    return SMALL_INT (op) || SPARC_SETHI_P (INTVAL (op));

  if (GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) == LO_SUM)
    return (register_operand (XEXP (op, 0), Pmode)
	    && CONSTANT_P (XEXP (op, 1)));
  return memory_address_p (mode, op);
}

int
splittable_symbolic_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
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
     enum machine_mode mode;
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
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return 1 if this is a comparison operator, but not an EQ, NE, GEU,
   or LTU for non-floating-point.  We handle those specially.  */

int
normal_comp_operator (op, mode)
     rtx op;
     enum machine_mode mode;
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
    enum machine_mode mode;
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
     enum machine_mode mode;
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
     enum machine_mode mode;
{
  return GET_CODE (op) == SIGN_EXTEND || GET_CODE (op) == ZERO_EXTEND;
}

/* Return nonzero if OP is an operator of mode MODE which can set
   the condition codes explicitly.  We do not include PLUS and MINUS
   because these require CC_NOOVmode, which we handle explicitly.  */

int
cc_arithop (op, mode)
     rtx op;
     enum machine_mode mode;
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
     enum machine_mode mode;
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
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op)));
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
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Recognize operand values for the umul instruction.  That instruction sign
   extends immediate values just like all other sparc instructions, but
   interprets the extended result as an unsigned number.  */

int
uns_small_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
#if HOST_BITS_PER_WIDE_INT > 32
  /* All allowed constants will fit a CONST_INT.  */
  return (GET_CODE (op) == CONST_INT
	  && ((INTVAL (op) >= 0 && INTVAL (op) < 0x1000)
	      || (INTVAL (op) >= 0xFFFFF000 && INTVAL (op) < 0x100000000L)));
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
     enum machine_mode mode;
{
  return (GET_CODE (op) == REG && call_used_regs[REGNO (op)]);
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
      cc_reg = gen_rtx (REG, mode, reg + SPARC_FIRST_V9_FCC_REG);
    }
#else
    cc_reg = gen_reg_rtx (mode);
#endif /* ! experiment */
  else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    cc_reg = gen_rtx (REG, mode, SPARC_FCC_REG);
  else
    cc_reg = gen_rtx (REG, mode, SPARC_ICC_REG);

  emit_insn (gen_rtx (SET, VOIDmode, cc_reg,
		      gen_rtx (COMPARE, mode, x, y)));

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
	  emit_insn (gen_rtx (SET, VOIDmode, operands[0], op0));
	  emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			      gen_rtx (IF_THEN_ELSE, DImode,
				       gen_rtx (compare_code, DImode,
						op0, const0_rtx),
				       const1_rtx,
				       operands[0])));
	  return 1;
	}

      emit_insn (gen_rtx (SET, VOIDmode, operands[0], const0_rtx));
      if (GET_MODE (op0) != DImode)
	{
	  temp = gen_reg_rtx (DImode);
	  convert_move (temp, op0, 0);
	}
      else
	temp = op0;
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (IF_THEN_ELSE, GET_MODE (operands[0]),
				   gen_rtx (compare_code, DImode,
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
      emit_insn (gen_rtx (SET, VOIDmode, operands[0], const0_rtx));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (IF_THEN_ELSE, GET_MODE (operands[0]),
				   gen_rtx (compare_code,
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
  emit_jump_insn (gen_rtx (SET, VOIDmode,
			   pc_rtx,
			   gen_rtx (IF_THEN_ELSE, VOIDmode,
				    gen_rtx (code, GET_MODE (op0),
					     op0, const0_rtx),
				    gen_rtx (LABEL_REF, VOIDmode, label),
				    pc_rtx)));
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

  /* If %g0 is live, there are lots of things we can't handle.
     Rather than trying to find them all now, let's punt and only
     optimize things as necessary.  */
  if (TARGET_LIVE_G0)
    return 0;

  /* In the case of a true leaf function, anything can go into the delay slot.
     A delay slot only exists however if the frame size is zero, otherwise
     we will put an insn to adjust the stack after the return.  */
  if (leaf_function)
    {
      if (leaf_return_peephole_ok ())
	return ((get_attr_in_uncond_branch_delay (trial)
		 == IN_BRANCH_DELAY_TRUE));
      return 0;
    }

  /* If only trivial `restore' insns work, nothing can go in the
     delay slot.  */
  else if (TARGET_BROKEN_SAVERESTORE)
    return 0;

  pat = PATTERN (trial);

  /* Otherwise, only operations which can be done in tandem with
     a `restore' insn can go into the delay slot.  */
  if (GET_CODE (SET_DEST (pat)) != REG
      || REGNO (SET_DEST (pat)) >= 32
      || REGNO (SET_DEST (pat)) < 24)
    return 0;

  /* The set of insns matched here must agree precisely with the set of
     patterns paired with a RETURN in sparc.md.  */

  src = SET_SRC (pat);

  /* This matches "*return_[qhs]".  */
  if (arith_operand (src, GET_MODE (src)))
    return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (SImode);
    
  /* This matches "*return_di".  */
  else if (arith_double_operand (src, GET_MODE (src)))
    return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (DImode);

  /* This matches "*return_sf_no_fpu".  */
  else if (! TARGET_FPU && restore_operand (SET_DEST (pat), SFmode)
	   && register_operand (src, SFmode))
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

  /* This matches "*return_subsi".  */
  else if (GET_CODE (src) == MINUS
      && register_operand (XEXP (src, 0), SImode)
      && small_int (XEXP (src, 1), VOIDmode)
      && INTVAL (XEXP (src, 1)) != -4096)
    return 1;

  return 0;
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

  while (insn = NEXT_INSN (insn))
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

/* Ensure that we are not using patterns that are not OK with PIC.  */

int
check_pic (i)
     int i;
{
  switch (flag_pic)
    {
    case 1:
      if (GET_CODE (recog_operand[i]) == SYMBOL_REF
	  || (GET_CODE (recog_operand[i]) == CONST
	      && ! (GET_CODE (XEXP (recog_operand[i], 0)) == MINUS
		    && (XEXP (XEXP (recog_operand[i], 0), 0)
			== global_offset_table)
		    && (GET_CODE (XEXP (XEXP (recog_operand[i], 0), 1))
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
     enum machine_mode mode;
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
	  emit_insn (gen_pic_sethi_si (temp_reg, orig));
	  emit_insn (gen_pic_lo_sum_si (temp_reg, temp_reg, orig));

	  address = temp_reg;
	}
      else
	address = orig;

      pic_ref = gen_rtx (MEM, Pmode,
			 gen_rtx (PLUS, Pmode,
				  pic_offset_table_rtx, address));
      current_function_uses_pic_offset_table = 1;
      RTX_UNCHANGING_P (pic_ref) = 1;
      insn = emit_move_insn (reg, pic_ref);
      /* Put a REG_EQUAL note on this insn, so that it can be optimized
	 by loop.  */
      REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_EQUAL, orig,
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
      return gen_rtx (PLUS, Pmode, base, offset);
    }
  else if (GET_CODE (orig) == LABEL_REF)
    /* ??? Why do we do this?  */
    current_function_uses_pic_offset_table = 1;

  return orig;
}

/* Set up PIC-specific rtl.  This should not cause any insns
   to be emitted.  */

void
initialize_pic ()
{
}

/* Return the RTX for insns to set the PIC register.  */

static rtx
pic_setup_code ()
{
  rtx pic_pc_rtx;
  rtx l1, l2;
  rtx seq;

  start_sequence ();

  /* If -O0, show the PIC register remains live before this.  */
  if (obey_regdecls)
    emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));
    
  l1 = gen_label_rtx ();

  pic_pc_rtx = gen_rtx (CONST, Pmode,
			gen_rtx (MINUS, Pmode,
				 global_offset_table,
				 gen_rtx (CONST, Pmode,
					  gen_rtx (MINUS, Pmode,
						   gen_rtx (LABEL_REF,
							    VOIDmode, l1),
						   pc_rtx))));

  /* sparc64: the RDPC instruction doesn't pair, and puts 4 bubbles in the
     pipe to boot.  So don't use it here, especially when we're
     doing a save anyway because of %l7.  */

  l2 = gen_label_rtx ();
  emit_label (l1);

  /* Iff we are doing delay branch optimization, slot the sethi up
     here so that it will fill the delay slot of the call.  */
  if (flag_delayed_branch)
    emit_insn (gen_rtx (SET, VOIDmode, pic_offset_table_rtx,
			gen_rtx (HIGH, Pmode, pic_pc_rtx)));

  /* Note that we pun calls and jumps here!  */
  emit_jump_insn (gen_get_pc_via_call (l2, l1));

  emit_label (l2);

  if (!flag_delayed_branch)
    emit_insn (gen_rtx (SET, VOIDmode, pic_offset_table_rtx,
			gen_rtx (HIGH, Pmode, pic_pc_rtx)));

  emit_insn (gen_rtx (SET, VOIDmode,
		      pic_offset_table_rtx,
		      gen_rtx (LO_SUM, Pmode,
			       pic_offset_table_rtx, pic_pc_rtx)));
  emit_insn (gen_rtx (SET, VOIDmode,
		      pic_offset_table_rtx,
		      gen_rtx (PLUS, Pmode,
			       pic_offset_table_rtx,
			       gen_rtx (REG, Pmode, 15))));

  /* emit_insn (gen_rtx (ASM_INPUT, VOIDmode, "!#PROLOGUE# 1")); */
  LABEL_PRESERVE_P (l1) = 1;
  LABEL_PRESERVE_P (l2) = 1;

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

  /* Initialize every time through, since we can't easily
     know this to be permanent.  */
  global_offset_table = gen_rtx (SYMBOL_REF, Pmode, "_GLOBAL_OFFSET_TABLE_");
  flag_pic = 0;

  emit_insn_after (pic_setup_code (), get_insns ());

  /* Insert the code in each nonlocal goto receiver.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == UNSPEC_VOLATILE
	&& XINT (PATTERN (insn), 1) == 4)
      emit_insn_after (pic_setup_code (), insn);

  flag_pic = orig_flag_pic;

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.
     ??? In the case where we don't obey regdecls, this is not sufficient
     since we may not fall out the bottom.  */
  emit_insn (gen_rtx (USE, VOIDmode, pic_offset_table_rtx));
}

/* Emit insns to move operands[1] into operands[0].

   Return 1 if we have written out everything that needs to be done to
   do the move.  Otherwise, return 0 and the caller will emit the move
   normally.  */

int
emit_move_sequence (operands, mode)
     rtx *operands;
     enum machine_mode mode;
{
  register rtx operand0 = operands[0];
  register rtx operand1 = operands[1];

  if (CONSTANT_P (operand1) && flag_pic
      && pic_address_needs_scratch (operand1))
    operands[1] = operand1 = legitimize_pic_address (operand1, mode, 0);

  /* Handle most common case first: storing into a register.  */
  if (register_operand (operand0, mode))
    {
      if (register_operand (operand1, mode)
	  || (GET_CODE (operand1) == CONST_INT && SMALL_INT (operand1))
	  || (GET_CODE (operand1) == CONST_DOUBLE
	      && arith_double_operand (operand1, DImode))
	  || (GET_CODE (operand1) == HIGH && GET_MODE (operand1) != DImode)
	  /* Only `general_operands' can come here, so MEM is ok.  */
	  || GET_CODE (operand1) == MEM)
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
    }
  else if (GET_CODE (operand0) == MEM)
    {
      if (register_operand (operand1, mode)
	  || (operand1 == const0_rtx && ! TARGET_LIVE_G0))
	{
	  /* Run this case quickly.  */
	  emit_insn (gen_rtx (SET, VOIDmode, operand0, operand1));
	  return 1;
	}
      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operand0);
	  operands[1] = operand1 = force_reg (mode, operand1);
	}
    }

  if (GET_CODE (operand1) == LABEL_REF
      && mode == SImode && flag_pic)
    {
      if (TARGET_ARCH64)
	abort ();
      emit_insn (gen_move_pic_label_si (operand0, operand1));
      return 1;
    }
  /* Non-pic LABEL_REF's in sparc64 are expensive to do the normal way,
     so always use special code.  */
  else if (GET_CODE (operand1) == LABEL_REF
	   && mode == DImode)
    {
      if (! TARGET_ARCH64)
	abort ();
      emit_insn (gen_move_label_di (operand0, operand1));
      return 1;
    }
  /* DImode HIGH values in sparc64 need a clobber added.  */
  else if (TARGET_ARCH64
      && GET_CODE (operand1) == HIGH && GET_MODE (operand1) == DImode)
    {
      emit_insn (gen_sethi_di_sp64 (operand0, XEXP (operand1, 0)));
      return 1;
    }
  /* Simplify the source if we need to.  */
  else if (GET_CODE (operand1) != HIGH && immediate_operand (operand1, mode))
    {
      if (flag_pic && symbolic_operand (operand1, mode))
	{
	  rtx temp_reg = reload_in_progress ? operand0 : 0;

	  operands[1] = legitimize_pic_address (operand1, mode, temp_reg);
	}
      else if (GET_CODE (operand1) == CONST_INT
	       ? (! SMALL_INT (operand1)
		  && ! SPARC_SETHI_P (INTVAL (operand1)))
	       : GET_CODE (operand1) == CONST_DOUBLE
	       ? ! arith_double_operand (operand1, DImode)
	       : 1)
	{
	  /* For DImode values, temp must be operand0 because of the way
	     HI and LO_SUM work.  The LO_SUM operator only copies half of
	     the LSW from the dest of the HI operator.  If the LO_SUM dest is
	     not the same as the HI dest, then the MSW of the LO_SUM dest will
	     never be set.

	     ??? The real problem here is that the ...(HI:DImode pattern emits
	     multiple instructions, and the ...(LO_SUM:DImode pattern emits
	     one instruction.  This fails, because the compiler assumes that
	     LO_SUM copies all bits of the first operand to its dest.  Better
	     would be to have the HI pattern emit one instruction and the
	     LO_SUM pattern multiple instructions.  Even better would be
	     to use four rtl insns.  */
	  rtx temp = ((reload_in_progress || mode == DImode)
		      ? operand0 : gen_reg_rtx (mode));

	  if (TARGET_ARCH64 && mode == DImode)
	    emit_insn (gen_sethi_di_sp64 (temp, operand1));
	  else
	    emit_insn (gen_rtx (SET, VOIDmode, temp,
				gen_rtx (HIGH, mode, operand1)));

	  if (GET_CODE (operand1) == CONST_INT)
	    operand1 = GEN_INT (INTVAL (operand1) & 0xffffffff);
	  else if (GET_CODE (operand1) == CONST_DOUBLE)
	    operand1 = GEN_INT (CONST_DOUBLE_LOW (operand1) & 0xffffffff);
	  operands[1] = gen_rtx (LO_SUM, mode, temp, operand1);
	}
    }

  /* Now have insn-emit do whatever it normally does.  */
  return 0;
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a 4 byte quantity.

   This isn't intended to be very smart.  It is up to the caller to
   choose the best way to do things.

   Note that OPERANDS may be modified to suit the returned string.  */

char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (GET_CODE (operands[1]) != MEM)
	return "st %r1,%0";
      else
	abort ();
    }
  else if (GET_CODE (operands[1]) == MEM)
    return "ld %1,%0";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE r;
      long i;

      /* Must be SFmode, otherwise this doesn't make sense.  */
      if (GET_MODE (operands[1]) != SFmode)
	abort ();

      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      operands[1] = GEN_INT (i);

      if (CONST_OK_FOR_LETTER_P (i, 'I'))
	return "mov %1,%0";
      else if ((i & 0x000003FF) != 0)
	return "sethi %%hi(%a1),%0\n\tor %0,%%lo(%a1),%0";
      else
	return "sethi %%hi(%a1),%0";
    }
  else if (GET_CODE (operands[1]) == CONST_INT
	   && ! CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'I'))
    {
      HOST_WIDE_INT i = INTVAL (operands[1]);

      /* If all low order 10 bits are clear, then we only need a single
	 sethi insn to load the constant.  */
      /* FIXME: Use SETHI_P.  */
      if ((i & 0x000003FF) != 0)
	return "sethi %%hi(%a1),%0\n\tor %0,%%lo(%a1),%0";
      else
	return "sethi %%hi(%a1),%0";
    }
  /* Operand 1 must be a register, or a 'I' type CONST_INT.  */
  return "mov %1,%0";
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as an 8 byte quantity.

   This isn't intended to be very smart.  It is up to the caller to
   choose the best way to do things.

   Note that OPERANDS may be modified to suit the returned string.  */

char *
doublemove_string (operands)
     rtx *operands;
{
  rtx op0 = operands[0], op1 = operands[1];

  if (GET_CODE (op0) == MEM)
    {
      if (GET_CODE (op1) == REG)
	{
	  if (FP_REG_P (op1))
	    return "std %1,%0";
	  return TARGET_ARCH64 ? "stx %1,%0" : "std %1,%0";
	}
      if (TARGET_ARCH64
	  && (op1 == const0_rtx
	      || (GET_MODE (op1) != VOIDmode
		  && op1 == CONST0_RTX (GET_MODE (op1)))))
	return "stx %r1,%0";
      abort ();
    }
  else if (GET_CODE (op1) == MEM)
    {
      if (GET_CODE (op0) != REG)
	abort ();
      if (FP_REG_P (op0))
	return "ldd %1,%0";
      return TARGET_ARCH64 ? "ldx %1,%0" : "ldd %1,%0";
    }
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      /* ??? Unfinished, and maybe not needed.  */
      abort ();
    }
  else if (GET_CODE (operands[1]) == CONST_INT
	   && ! CONST_OK_FOR_LETTER_P (INTVAL (operands[1]), 'I'))
    {
      /* ??? Unfinished, and maybe not needed.  */
      abort ();
    }
  /* Operand 1 must be a register, or a 'I' type CONST_INT.  */
  return "mov %1,%0";
}

/* Return non-zero if it is OK to assume that the given memory operand is
   aligned at least to a 8-byte boundary.  This should only be called
   for memory accesses whose size is 8 bytes or larger.  */

int
mem_aligned_8 (mem)
     register rtx mem;
{
  register rtx addr;
  register rtx base;
  register rtx offset;

  if (GET_CODE (mem) != MEM)
    return 0;	/* It's gotta be a MEM! */

  addr = XEXP (mem, 0);

  /* Now that all misaligned double parms are copied on function entry,
     we can assume any 64-bit object is 64-bit aligned except those which
     are at unaligned offsets from the stack or frame pointer.  If the
     TARGET_UNALIGNED_DOUBLES switch is given, we do not make this
     assumption.  */

  /* See what register we use in the address.  */
  base = 0;
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && GET_CODE (XEXP (addr, 1)) == CONST_INT)
	{
	  base = XEXP (addr, 0);
	  offset = XEXP (addr, 1);
	}
    }
  else if (GET_CODE (addr) == REG)
    {
      base = addr;
      offset = const0_rtx;
    }

  /* If it's the stack or frame pointer, check offset alignment.
     We can have improper alignment in the function entry code.  */
  if (base
      && (REGNO (base) == FRAME_POINTER_REGNUM
	  || REGNO (base) == STACK_POINTER_REGNUM))
    {
      if (((INTVAL (offset) - SPARC_STACK_BIAS) & 0x7) == 0)
	return 1;
    }
  /* Anything else we know is properly aligned unless TARGET_UNALIGNED_DOUBLES
     is true, in which case we can only assume that an access is aligned if
     it is to a constant address, or the address involves a LO_SUM.

     We used to assume an address was aligned if MEM_IN_STRUCT_P was true.
     That assumption was deleted so that gcc generated code can be used with
     memory allocators that only guarantee 4 byte alignment.  */
  else if (! TARGET_UNALIGNED_DOUBLES || CONSTANT_P (addr)
	   || GET_CODE (addr) == LO_SUM)
    return 1;

  /* An obviously unaligned address.  */
  return 0;
}

enum optype { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP };

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  This is very similar to the following
   output_move_quad function.  */

char *
output_move_double (operands)
     rtx *operands;
{
  register rtx op0 = operands[0];
  register rtx op1 = operands[1];
  register enum optype optype0;
  register enum optype optype1;
  rtx latehalf[2];
  rtx addreg0 = 0;
  rtx addreg1 = 0;
  int highest_first = 0;
  int no_addreg1_decrement = 0;

  /* First classify both operands.  */

  if (REG_P (op0))
    optype0 = REGOP;
  else if (offsettable_memref_p (op0))
    optype0 = OFFSOP;
  else if (GET_CODE (op0) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (op1))
    optype1 = REGOP;
  else if (CONSTANT_P (op1))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (op1))
    optype1 = OFFSOP;
  else if (GET_CODE (op1) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP
      || (optype0 == MEM && optype1 == MEM))
    abort ();

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (op0, 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (op1, 0));

  /* Ok, we can do one word at a time.
     Set up in LATEHALF the operands to use for the
     high-numbered (least significant) word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (op0) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (op0, 4);
  else
    latehalf[0] = op0;

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (op1) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (op1, 4);
  else if (optype1 == CNSTOP)
    {
      if (TARGET_ARCH64)
	{
	  if (arith_double_operand (op1, DImode))
	    {
	      operands[1] = gen_rtx (CONST_INT, VOIDmode,
				     CONST_DOUBLE_LOW (op1));
	      return "mov %1,%0";
	    }
	  else
	    {
	      /* The only way to handle CONST_DOUBLEs or other 64 bit
		 constants here is to use a temporary, such as is done
		 for the V9 DImode sethi insn pattern.  This is not
		 a practical solution, so abort if we reach here.
		 The md file should always force such constants to
		 memory.  */
	      abort ();
	    }
	}
      else
	split_double (op1, &operands[1], &latehalf[1]);
    }
  else
    latehalf[1] = op1;

  /* Easy case: try moving both words at once.  Check for moving between
     an even/odd register pair and a memory location.  */
  if ((optype0 == REGOP && optype1 != REGOP && optype1 != CNSTOP
       && (TARGET_ARCH64 || (REGNO (op0) & 1) == 0))
      || (optype0 != REGOP && optype0 != CNSTOP && optype1 == REGOP
	  && (TARGET_ARCH64 || (REGNO (op1) & 1) == 0)))
    {
      register rtx mem,reg;

      if (optype0 == REGOP)
	mem = op1, reg = op0;
      else
	mem = op0, reg = op1;

      /* In v9, ldd can be used for word aligned addresses, so technically
	 some of this logic is unneeded.  We still avoid ldd if the address
	 is obviously unaligned though.  */

      if (mem_aligned_8 (mem)
	  /* If this is a floating point register higher than %f31,
	     then we *must* use an aligned load, since `ld' will not accept
	     the register number.  */
	  || (TARGET_V9 && REGNO (reg) >= 64))
	{
	  if (FP_REG_P (reg) || ! TARGET_ARCH64)
	    return (mem == op1 ? "ldd %1,%0" : "std %1,%0");
	  else
	    return (mem == op1 ? "ldx %1,%0" : "stx %1,%0");
	}
    }

  if (TARGET_ARCH64)
    {
      if (optype0 == REGOP && optype1 == REGOP)
	{
	  if (FP_REG_P (op0))
	    return "fmovd %1,%0";
	  else
	    return "mov %1,%0";
	}
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers.  */
  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (op0) == REGNO (latehalf[1]))
    {
      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  /* Loading into a register which overlaps a register used in the address.  */
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (op0, op1))
    {
      /* If both halves of dest are used in the src memory address,
	 add the two regs and put them in the low reg (op0).
	 Then it works to load latehalf first.  */
      if (reg_mentioned_p (op0, XEXP (op1, 0))
	  && reg_mentioned_p (latehalf[0], XEXP (op1, 0)))
	{
	  rtx xops[2];
	  xops[0] = latehalf[0];
	  xops[1] = op0;
	  output_asm_insn ("add %1,%0,%1", xops);
	  operands[1] = gen_rtx (MEM, DImode, op0);
	  latehalf[1] = adj_offsettable_operand (operands[1], 4);
	  addreg1 = 0;
	  highest_first = 1;
	}
      /* Only one register in the dest is used in the src memory address,
	 and this is the first register of the dest, so we want to do
	 the late half first here also.  */
      else if (! reg_mentioned_p (latehalf[0], XEXP (op1, 0)))
	highest_first = 1;
      /* Only one register in the dest is used in the src memory address,
	 and this is the second register of the dest, so we want to do
	 the late half last.  If addreg1 is set, and addreg1 is the same
	 register as latehalf, then we must suppress the trailing decrement,
	 because it would clobber the value just loaded.  */
      else if (addreg1 && reg_mentioned_p (addreg1, latehalf[0]))
	no_addreg1_decrement = 1;
    }

  /* Normal case: do the two words, low-numbered first.
     Overlap case (highest_first set): do high-numbered word first.  */

  if (! highest_first)
    output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add %0,0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,0x4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("add %0,-0x4,%0", &addreg0);
  if (addreg1 && ! no_addreg1_decrement)
    output_asm_insn ("add %0,-0x4,%0", &addreg1);

  if (highest_first)
    output_asm_insn (singlemove_string (operands), operands);

  return "";
}

/* Output assembler code to perform a quadword move insn
   with operands OPERANDS.  This is very similar to the preceding
   output_move_double function.  */

char *
output_move_quad (operands)
     rtx *operands;
{
  register rtx op0 = operands[0];
  register rtx op1 = operands[1];
  register enum optype optype0;
  register enum optype optype1;
  rtx wordpart[4][2];
  rtx addreg0 = 0;
  rtx addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (op0))
    optype0 = REGOP;
  else if (offsettable_memref_p (op0))
    optype0 = OFFSOP;
  else if (GET_CODE (op0) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (op1))
    optype1 = REGOP;
  else if (CONSTANT_P (op1))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (op1))
    optype1 = OFFSOP;
  else if (GET_CODE (op1) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP
      || (optype0 == MEM && optype1 == MEM))
    abort ();

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the later words.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (op0, 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (op1, 0));

  /* Ok, we can do one word at a time.
     Set up in wordpart the operands to use for each word of the arguments.  */

  if (optype0 == REGOP)
    {
      wordpart[0][0] = gen_rtx (REG, word_mode, REGNO (op0) + 0);
      wordpart[1][0] = gen_rtx (REG, word_mode, REGNO (op0) + 1);
      if (TARGET_ARCH32)
	{
	  wordpart[2][0] = gen_rtx (REG, word_mode, REGNO (op0) + 2);
	  wordpart[3][0] = gen_rtx (REG, word_mode, REGNO (op0) + 3);
	}
    }
  else if (optype0 == OFFSOP)
    {
      wordpart[0][0] = adj_offsettable_operand (op0, 0);
      if (TARGET_ARCH32)
	{
	  wordpart[1][0] = adj_offsettable_operand (op0, 4);
	  wordpart[2][0] = adj_offsettable_operand (op0, 8);
	  wordpart[3][0] = adj_offsettable_operand (op0, 12);
	}
      else
	wordpart[1][0] = adj_offsettable_operand (op0, 8);
    }
  else
    {
      wordpart[0][0] = op0;
      wordpart[1][0] = op0;
      wordpart[2][0] = op0;
      wordpart[3][0] = op0;
    }

  if (optype1 == REGOP)
    {
      wordpart[0][1] = gen_rtx (REG, word_mode, REGNO (op1) + 0);
      wordpart[1][1] = gen_rtx (REG, word_mode, REGNO (op1) + 1);
      if (TARGET_ARCH32)
	{
	  wordpart[2][1] = gen_rtx (REG, word_mode, REGNO (op1) + 2);
	  wordpart[3][1] = gen_rtx (REG, word_mode, REGNO (op1) + 3);
	}
    }
  else if (optype1 == OFFSOP)
    {
      wordpart[0][1] = adj_offsettable_operand (op1, 0);
      if (TARGET_ARCH32)
	{
	  wordpart[1][1] = adj_offsettable_operand (op1, 4);
	  wordpart[2][1] = adj_offsettable_operand (op1, 8);
	  wordpart[3][1] = adj_offsettable_operand (op1, 12);
	}
      else
	wordpart[1][1] = adj_offsettable_operand (op1, 8);
    }
  else if (optype1 == CNSTOP)
    {
      REAL_VALUE_TYPE r;
      long l[4];

      /* This only works for TFmode floating point constants.  */
      if (GET_CODE (op1) != CONST_DOUBLE || GET_MODE (op1) != TFmode)
	abort ();

      REAL_VALUE_FROM_CONST_DOUBLE (r, op1);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
      
      wordpart[0][1] = GEN_INT (l[0]);
      wordpart[1][1] = GEN_INT (l[1]);
      wordpart[2][1] = GEN_INT (l[2]);
      wordpart[3][1] = GEN_INT (l[3]);
    }
  else
    {
      wordpart[0][1] = op1;
      wordpart[1][1] = op1;
      wordpart[2][1] = op1;
      wordpart[3][1] = op1;
    }

  /* Easy case: try moving the quad as two pairs.  Check for moving between
     an even/odd register pair and a memory location.
     Also handle new v9 fp regs here.  */
  /* ??? Should also handle the case of non-offsettable addresses here.
     We can at least do the first pair as a ldd/std, and then do the third
     and fourth words individually.  */
  if ((optype0 == REGOP && optype1 == OFFSOP && (REGNO (op0) & 1) == 0)
      || (optype0 == OFFSOP && optype1 == REGOP && (REGNO (op1) & 1) == 0))
    {
      rtx mem, reg;

      if (optype0 == REGOP)
	mem = op1, reg = op0;
      else
	mem = op0, reg = op1;

      if (mem_aligned_8 (mem)
	  /* If this is a floating point register higher than %f31,
	     then we *must* use an aligned load, since `ld' will not accept
	     the register number.  */
	  || (TARGET_V9 && REGNO (reg) >= SPARC_FIRST_V9_FP_REG))
	{
	  if (TARGET_V9 && FP_REG_P (reg) && TARGET_HARD_QUAD)
	    {
	      if ((REGNO (reg) & 3) != 0)
		abort ();
	      /* ??? Can `mem' have an inappropriate alignment here?  */
	      return (mem == op1 ? "ldq %1,%0" : "stq %1,%0");
	    }
	  operands[2] = adj_offsettable_operand (mem, 8);
	  /* ??? In arch64 case, shouldn't we use ldd/std for fp regs.  */
	  if (mem == op1)
	    return TARGET_ARCH64 ? "ldx %1,%0;ldx %2,%R0" : "ldd %1,%0;ldd %2,%S0";
	  else
	    return TARGET_ARCH64 ? "stx %1,%0;stx %R1,%2" : "std %1,%0;std %S1,%2";
	}
    }

  /* If the first move would clobber the source of the second one,
     do them in the other order.  */

  /* Overlapping registers?  */
  if (TARGET_ARCH32)
    {
      if (optype0 == REGOP && optype1 == REGOP
	  && (REGNO (op0) == REGNO (wordpart[1][3])
	      || REGNO (op0) == REGNO (wordpart[1][2])
	      || REGNO (op0) == REGNO (wordpart[1][1])))
	{
	  /* Do fourth word.  */
	  output_asm_insn (singlemove_string (wordpart[3]), wordpart[3]);
	  /* Do the third word.  */
	  output_asm_insn (singlemove_string (wordpart[2]), wordpart[2]);
	  /* Do the second word.  */
	  output_asm_insn (singlemove_string (wordpart[1]), wordpart[1]);
	  /* Do lowest-numbered word.  */
	  output_asm_insn (singlemove_string (wordpart[0]), wordpart[0]);
	  return "";
	}
    }
  else /* TARGET_ARCH64 */
    {
      if (optype0 == REGOP && optype1 == REGOP
	  && REGNO (op0) == REGNO (wordpart[1][1]))
	{
	  output_asm_insn ("mov %1,%0", wordpart[1]);
	  output_asm_insn ("mov %1,%0", wordpart[0]);
	  return "";
	}
    }

  /* Loading into a register which overlaps a register used in the address.  */
  if (optype0 == REGOP && optype1 != REGOP
      && reg_overlap_mentioned_p (op0, op1))
    {
      /* ??? Not implemented yet.  This is a bit complicated, because we
	 must load which ever part overlaps the address last.  If the address
	 is a double-reg address, then there are two parts which need to
	 be done last, which is impossible.  We would need a scratch register
	 in that case.  */
      abort ();
    }

  /* Normal case: move the words in lowest to highest address order.  */

  if (TARGET_ARCH32)
    {
      output_asm_insn (singlemove_string (wordpart[0]), wordpart[0]);

      /* Make any unoffsettable addresses point at the second word.  */
      if (addreg0)
	output_asm_insn ("add %0,0x4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,0x4,%0", &addreg1);

      /* Do the second word.  */
      output_asm_insn (singlemove_string (wordpart[1]), wordpart[1]);

      /* Make any unoffsettable addresses point at the third word.  */
      if (addreg0)
	output_asm_insn ("add %0,0x4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,0x4,%0", &addreg1);

      /* Do the third word.  */
      output_asm_insn (singlemove_string (wordpart[2]), wordpart[2]);

      /* Make any unoffsettable addresses point at the fourth word.  */
      if (addreg0)
	output_asm_insn ("add %0,0x4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,0x4,%0", &addreg1);

      /* Do the fourth word.  */
      output_asm_insn (singlemove_string (wordpart[3]), wordpart[3]);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("add %0,-0xc,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,-0xc,%0", &addreg1);
    }
  else /* TARGET_ARCH64 */
    {
      output_asm_insn (doublemove_string (wordpart[0]), wordpart[0]);

      /* Make any unoffsettable addresses point at the second word.  */
      if (addreg0)
	output_asm_insn ("add %0,0x8,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,0x8,%0", &addreg1);

      /* Do the second word.  */
      output_asm_insn (doublemove_string (wordpart[1]), wordpart[1]);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("add %0,-0x8,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,-0x8,%0", &addreg1);
    }

  return "";
}

/* Output assembler code to perform a doubleword move insn with operands
   OPERANDS, one of which must be a floating point register.  */

char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	{
	  if (TARGET_V9)
	    return "fmovd %1,%0";
	  else
	    return "fmovs %1,%0\n\tfmovs %R1,%R0";
	}
      else if (GET_CODE (operands[1]) == REG)
	abort ();
      else
	return output_move_double (operands);
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	abort ();
      else
	return output_move_double (operands);
    }
  else abort ();
}

/* Output assembler code to perform a quadword move insn with operands
   OPERANDS, one of which must be a floating point register.  */

char *
output_fp_move_quad (operands)
     rtx *operands;
{
  register rtx op0 = operands[0];
  register rtx op1 = operands[1];

  if (FP_REG_P (op0))
    {
      if (FP_REG_P (op1))
	{
	  if (TARGET_V9 && TARGET_HARD_QUAD)
	    return "fmovq %1,%0";
	  else if (TARGET_V9)
	    return "fmovd %1,%0\n\tfmovd %S1,%S0";
	  else
	    return "fmovs %1,%0\n\tfmovs %R1,%R0\n\tfmovs %S1,%S0\n\tfmovs %T1,%T0";
	}
      else if (GET_CODE (op1) == REG)
	abort ();
      else
	return output_move_quad (operands);
    }
  else if (FP_REG_P (op1))
    {
      if (GET_CODE (op0) == REG)
	abort ();
      else
	return output_move_quad (operands);
    }
  else
    abort ();
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      /* We absolutely can not fudge the frame pointer here, because the
	 frame pointer must always be 8 byte aligned.  It also confuses
	 debuggers.  */
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && REGNO (XEXP (addr, 0)) != FRAME_POINTER_REGNUM)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG
	       && REGNO (XEXP (addr, 1)) != FRAME_POINTER_REGNUM)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

#if 0 /* not currently used */

void
output_sized_memop (opname, mode, signedp)
     char *opname;
     enum machine_mode mode;
     int signedp;
{
  static char *ld_size_suffix_u[] = { "ub", "uh", "", "?", "d" };
  static char *ld_size_suffix_s[] = { "sb", "sh", "", "?", "d" };
  static char *st_size_suffix[] = { "b", "h", "", "?", "d" };
  char **opnametab, *modename;

  if (opname[0] == 'l')
    if (signedp)
      opnametab = ld_size_suffix_s;
    else
      opnametab = ld_size_suffix_u;
  else
    opnametab = st_size_suffix;
  modename = opnametab[GET_MODE_SIZE (mode) >> 1];

  fprintf (asm_out_file, "\t%s%s", opname, modename);
}

void
output_move_with_extension (operands)
     rtx *operands;
{
  if (GET_MODE (operands[2]) == HImode)
    output_asm_insn ("sll %2,0x10,%0", operands);
  else if (GET_MODE (operands[2]) == QImode)
    output_asm_insn ("sll %2,0x18,%0", operands);
  else
    abort ();
}
#endif /* not currently used */

#if 0
/* ??? These are only used by the movstrsi pattern, but we get better code
   in general without that, because emit_block_move can do just as good a
   job as this function does when alignment and size are known.  When they
   aren't known, a call to strcpy may be faster anyways, because it is
   likely to be carefully crafted assembly language code, and below we just
   do a byte-wise copy.

   Also, emit_block_move expands into multiple read/write RTL insns, which
   can then be optimized, whereas our movstrsi pattern can not be optimized
   at all.  */

/* Load the address specified by OPERANDS[3] into the register
   specified by OPERANDS[0].

   OPERANDS[3] may be the result of a sum, hence it could either be:

   (1) CONST
   (2) REG
   (2) REG + CONST_INT
   (3) REG + REG + CONST_INT
   (4) REG + REG  (special case of 3).

   Note that (3) is not a legitimate address.
   All cases are handled here.  */

void
output_load_address (operands)
     rtx *operands;
{
  rtx base, offset;

  if (CONSTANT_P (operands[3]))
    {
      output_asm_insn ("set %3,%0", operands);
      return;
    }

  if (REG_P (operands[3]))
    {
      if (REGNO (operands[0]) != REGNO (operands[3]))
	output_asm_insn ("mov %3,%0", operands);
      return;
    }

  if (GET_CODE (operands[3]) != PLUS)
    abort ();

  base = XEXP (operands[3], 0);
  offset = XEXP (operands[3], 1);

  if (GET_CODE (base) == CONST_INT)
    {
      rtx tmp = base;
      base = offset;
      offset = tmp;
    }

  if (GET_CODE (offset) != CONST_INT)
    {
      /* Operand is (PLUS (REG) (REG)).  */
      base = operands[3];
      offset = const0_rtx;
    }

  if (REG_P (base))
    {
      operands[6] = base;
      operands[7] = offset;
      if (SMALL_INT (offset))
	output_asm_insn ("add %6,%7,%0", operands);
      else
	output_asm_insn ("set %7,%0\n\tadd %0,%6,%0", operands);
    }
  else if (GET_CODE (base) == PLUS)
    {
      operands[6] = XEXP (base, 0);
      operands[7] = XEXP (base, 1);
      operands[8] = offset;

      if (SMALL_INT (offset))
	output_asm_insn ("add %6,%7,%0\n\tadd %0,%8,%0", operands);
      else
	output_asm_insn ("set %8,%0\n\tadd %0,%6,%0\n\tadd %0,%7,%0", operands);
    }
  else
    abort ();
}

/* Output code to place a size count SIZE in register REG.
   ALIGN is the size of the unit of transfer.

   Because block moves are pipelined, we don't include the
   first element in the transfer of SIZE to REG.  */

static void
output_size_for_block_move (size, reg, align)
     rtx size, reg;
     rtx align;
{
  rtx xoperands[3];

  xoperands[0] = reg;
  xoperands[1] = size;
  xoperands[2] = align;
  if (GET_CODE (size) == REG)
    output_asm_insn ("sub %1,%2,%0", xoperands);
  else
    {
      xoperands[1]
	= gen_rtx (CONST_INT, VOIDmode, INTVAL (size) - INTVAL (align));
      output_asm_insn ("set %1,%0", xoperands);
    }
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the alignment safe to use.
   OPERANDS[4] is a register we can safely clobber as a temp.  */

char *
output_block_move (operands)
     rtx *operands;
{
  /* A vector for our computed operands.  Note that load_output_address
     makes use of (and can clobber) up to the 8th element of this vector.  */
  rtx xoperands[10];
  rtx zoperands[10];
  static int movstrsi_label = 0;
  int i;
  rtx temp1 = operands[4];
  rtx sizertx = operands[2];
  rtx alignrtx = operands[3];
  int align = INTVAL (alignrtx);
  char label3[30], label5[30];

  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = temp1;

  /* We can't move more than this many bytes at a time because we have only
     one register, %g1, to move them through.  */
  if (align > UNITS_PER_WORD)
    {
      align = UNITS_PER_WORD;
      alignrtx = gen_rtx (CONST_INT, VOIDmode, UNITS_PER_WORD);
    }

  /* We consider 8 ld/st pairs, for a total of 16 inline insns to be
     reasonable here.  (Actually will emit a maximum of 18 inline insns for
     the case of size == 31 and align == 4).  */

  if (GET_CODE (sizertx) == CONST_INT && (INTVAL (sizertx) / align) <= 8
      && memory_address_p (QImode, plus_constant_for_output (xoperands[0],
							     INTVAL (sizertx)))
      && memory_address_p (QImode, plus_constant_for_output (xoperands[1],
							     INTVAL (sizertx))))
    {
      int size = INTVAL (sizertx);
      int offset = 0;

      /* We will store different integers into this particular RTX.  */
      xoperands[2] = rtx_alloc (CONST_INT);
      PUT_MODE (xoperands[2], VOIDmode);

      /* This case is currently not handled.  Abort instead of generating
	 bad code.  */
      if (align > UNITS_PER_WORD)
	abort ();

      if (TARGET_ARCH64 && align >= 8)
	{
	  for (i = (size >> 3) - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = (i << 3) + offset;
	      output_asm_insn ("ldx [%a1+%2],%%g1\n\tstx %%g1,[%a0+%2]",
			       xoperands);
	    }
	  offset += (size & ~0x7);
	  size = size & 0x7;
	  if (size == 0)
	    return "";
	}

      if (align >= 4)
	{
	  for (i = (size >> 2) - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = (i << 2) + offset;
	      output_asm_insn ("ld [%a1+%2],%%g1\n\tst %%g1,[%a0+%2]",
			       xoperands);
	    }
	  offset += (size & ~0x3);
	  size = size & 0x3;
	  if (size == 0)
	    return "";
	}

      if (align >= 2)
	{
	  for (i = (size >> 1) - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = (i << 1) + offset;
	      output_asm_insn ("lduh [%a1+%2],%%g1\n\tsth %%g1,[%a0+%2]",
			       xoperands);
	    }
	  offset += (size & ~0x1);
	  size = size & 0x1;
	  if (size == 0)
	    return "";
	}

      if (align >= 1)
	{
	  for (i = size - 1; i >= 0; i--)
	    {
	      INTVAL (xoperands[2]) = i + offset;
	      output_asm_insn ("ldub [%a1+%2],%%g1\n\tstb %%g1,[%a0+%2]",
			       xoperands);
	    }
	  return "";
	}

      /* We should never reach here.  */
      abort ();
    }

  /* If the size isn't known to be a multiple of the alignment,
     we have to do it in smaller pieces.  If we could determine that
     the size was a multiple of 2 (or whatever), we could be smarter
     about this.  */
  if (GET_CODE (sizertx) != CONST_INT)
    align = 1;
  else
    {
      int size = INTVAL (sizertx);
      while (size % align)
	align >>= 1;
    }

  if (align != INTVAL (alignrtx))
    alignrtx = gen_rtx (CONST_INT, VOIDmode, align);

  xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);
  xoperands[4] = gen_rtx (CONST_INT, VOIDmode, align);
  xoperands[5] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

  ASM_GENERATE_INTERNAL_LABEL (label3, "Lm", INTVAL (xoperands[3]));
  ASM_GENERATE_INTERNAL_LABEL (label5, "Lm", INTVAL (xoperands[5]));

  /* This is the size of the transfer.  Emit code to decrement the size
     value by ALIGN, and store the result in the temp1 register.  */
  output_size_for_block_move (sizertx, temp1, alignrtx);

  /* Must handle the case when the size is zero or negative, so the first thing
     we do is compare the size against zero, and only copy bytes if it is
     zero or greater.  Note that we have already subtracted off the alignment
     once, so we must copy 1 alignment worth of bytes if the size is zero
     here.

     The SUN assembler complains about labels in branch delay slots, so we
     do this before outputting the load address, so that there will always
     be a harmless insn between the branch here and the next label emitted
     below.  */

  {
    char pattern[100];

    sprintf (pattern, "cmp %%2,0\n\tbl %s", &label5[1]);
    output_asm_insn (pattern, xoperands);
  }

  zoperands[0] = operands[0];
  zoperands[3] = plus_constant_for_output (operands[0], align);
  output_load_address (zoperands);

  /* ??? This might be much faster if the loops below were preconditioned
     and unrolled.

     That is, at run time, copy enough bytes one at a time to ensure that the
     target and source addresses are aligned to the the largest possible
     alignment.  Then use a preconditioned unrolled loop to copy say 16
     bytes at a time.  Then copy bytes one at a time until finish the rest.  */

  /* Output the first label separately, so that it is spaced properly.  */

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "Lm", INTVAL (xoperands[3]));

  {
    char pattern[200];
    register char *ld_suffix = ((align == 1) ? "ub" : (align == 2) ? "uh"
				: (align == 8 && TARGET_ARCH64) ? "x" : "");
    register char *st_suffix = ((align == 1) ? "b" : (align == 2) ? "h"
				: (align == 8 && TARGET_ARCH64) ? "x" : "");

    sprintf (pattern, "ld%s [%%1+%%2],%%%%g1\n\tsubcc %%2,%%4,%%2\n\tbge %s\n\tst%s %%%%g1,[%%0+%%2]\n%s:", ld_suffix, &label3[1], st_suffix, &label5[1]);
    output_asm_insn (pattern, xoperands);
  }

  return "";
}
#endif

/* Output reasonable peephole for set-on-condition-code insns.
   Note that these insns assume a particular way of defining
   labels.  Therefore, *both* sparc.h and this function must
   be changed if a new syntax is needed.    */

char *
output_scc_insn (operands, insn)
     rtx operands[];
     rtx insn;
{
  static char string[100];
  rtx label = 0, next = insn;
  int need_label = 0;

  /* Try doing a jump optimization which jump.c can't do for us
     because we did not expose that setcc works by using branches.

     If this scc insn is followed by an unconditional branch, then have
     the jump insn emitted here jump to that location, instead of to
     the end of the scc sequence as usual.  */

  do
    {
      if (GET_CODE (next) == CODE_LABEL)
	label = next;
      next = NEXT_INSN (next);
      if (next == 0)
	break;
    }
  while (GET_CODE (next) == NOTE || GET_CODE (next) == CODE_LABEL);

  /* If we are in a sequence, and the following insn is a sequence also,
     then just following the current insn's next field will take us to the
     first insn of the next sequence, which is the wrong place.  We don't
     want to optimize with a branch that has had its delay slot filled.
     Avoid this by verifying that NEXT_INSN (PREV_INSN (next)) == next
     which fails only if NEXT is such a branch.  */

  if (next && GET_CODE (next) == JUMP_INSN && simplejump_p (next)
      && (! final_sequence || NEXT_INSN (PREV_INSN (next)) == next))
    label = JUMP_LABEL (next);
  /* If not optimizing, jump label fields are not set.  To be safe, always
     check here to whether label is still zero.  */
  if (label == 0)
    {
      label = gen_label_rtx ();
      need_label = 1;
    }

  LABEL_NUSES (label) += 1;

  /* operands[3] is an unused slot.  */
  operands[3] = label;

  /* If we are in a delay slot, assume it is the delay slot of an fpcc
     insn since our type isn't allowed anywhere else.  */

  /* ??? Fpcc instructions no longer have delay slots, so this code is
     probably obsolete.  */

  /* The fastest way to emit code for this is an annulled branch followed
     by two move insns.  This will take two cycles if the branch is taken,
     and three cycles if the branch is not taken.

     However, if we are in the delay slot of another branch, this won't work,
     because we can't put a branch in the delay slot of another branch.
     The above sequence would effectively take 3 or 4 cycles respectively
     since a no op would have be inserted between the two branches.
     In this case, we want to emit a move, annulled branch, and then the
     second move.  This sequence always takes 3 cycles, and hence is faster
     when we are in a branch delay slot.  */

  if (final_sequence)
    {
      strcpy (string, "mov 0,%0\n\t");
      strcat (string, output_cbranch (operands[2], 3, 0, 1, 0));
      strcat (string, "\n\tmov 1,%0");
    }
  else
    {
      strcpy (string, output_cbranch (operands[2], 3, 0, 1, 0));
      strcat (string, "\n\tmov 1,%0\n\tmov 0,%0");
    }

  if (need_label)
    strcat (string, "\n%l3:");

  return string;
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

/* Modes for single-float quantities.  We must allow any single word or
   smaller quantity.  This is because the fix/float conversion instructions
   take integer inputs/outputs from the float registers.  */
#define SF_MODES (S_MODES)

/* Modes for double-float and smaller quantities.  */
#define DF_MODES (S_MODES | D_MODES)

/* ??? Sparc64 fp regs cannot hold DImode values.  */
#define DF_MODES64 (SF_MODES | DF_MODE /* | D_MODE*/)

/* Modes for double-float only quantities.  */
/* ??? Sparc64 fp regs cannot hold DImode values.
   See fix_truncsfdi2.  */
#define DF_ONLY_MODES ((1 << (int) DF_MODE) /*| (1 << (int) D_MODE)*/)

/* Modes for double-float and larger quantities.  */
#define DF_UP_MODES (DF_ONLY_MODES | TF_ONLY_MODES)

/* Modes for quad-float only quantities.  */
#define TF_ONLY_MODES (1 << (int) TF_MODE)

/* Modes for quad-float and smaller quantities.  */
#define TF_MODES (DF_MODES | TF_ONLY_MODES)

/* ??? Sparc64 fp regs cannot hold DImode values.
   See fix_truncsfdi2.  */
#define TF_MODES64 (DF_MODES64 | TF_ONLY_MODES)

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

  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,
  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,
  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,
  TF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,

  /* FP regs f32 to f63.  Only the even numbered registers actually exist,
     and none can hold SFmode/SImode values.  */
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,

  /* %fcc[0123] */
  CCFP_MODES, CCFP_MODES, CCFP_MODES, CCFP_MODES,

  /* %icc */
  CC_MODES
};

static int hard_64bit_mode_classes[] = {
  D_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,

  TF_MODES64, SF_MODES, DF_MODES64, SF_MODES, TF_MODES64, SF_MODES, DF_MODES64, SF_MODES,
  TF_MODES64, SF_MODES, DF_MODES64, SF_MODES, TF_MODES64, SF_MODES, DF_MODES64, SF_MODES,
  TF_MODES64, SF_MODES, DF_MODES64, SF_MODES, TF_MODES64, SF_MODES, DF_MODES64, SF_MODES,
  TF_MODES64, SF_MODES, DF_MODES64, SF_MODES, TF_MODES64, SF_MODES, DF_MODES64, SF_MODES,

  /* FP regs f32 to f63.  Only the even numbered registers actually exist,
     and none can hold SFmode/SImode values.  */
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,
  DF_UP_MODES, 0, DF_ONLY_MODES, 0, DF_UP_MODES, 0, DF_ONLY_MODES, 0,

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
      if (i < 32)
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
     char *base;
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
	      fprintf (file, "\tstx %s,[%s+%d]\n",
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
	    if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	      {
		fprintf (file, "\tstd %s,[%s+%d]\n",
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
		fprintf (file, "\tst %s,[%s+%d]\n",
			 reg_names[i], base, offset + 4 * n_regs);
		if (dwarf2out_do_frame ())
		  dwarf2out_reg_save ("", i, real_offset + 4 * n_regs);
		n_regs += 2;
	      }
	  else if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	    {
	      fprintf (file, "\tst %s,[%s+%d]\n",
		       reg_names[i+1], base, offset + 4 * n_regs + 4);
	      if (dwarf2out_do_frame ())
		dwarf2out_reg_save ("", i + 1, real_offset + 4 * n_regs + 4);
	      n_regs += 2;
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
     char *base;
     int offset;
     int n_regs;
{
  int i;

  if (TARGET_ARCH64 && high <= 32)
    {
      for (i = low; i < high; i++)
	{
	  if (regs_ever_live[i] && ! call_used_regs[i])
	    fprintf (file, "\tldx [%s+%d], %s\n",
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
	      fprintf (file, "\tldd [%s+%d], %s\n",
		       base, offset + 4 * n_regs, reg_names[i]),
	      n_regs += 2;
	    else
	      fprintf (file, "\tld [%s+%d],%s\n",
		       base, offset + 4 * n_regs, reg_names[i]),
	      n_regs += 2;
	  else if (regs_ever_live[i+1] && ! call_used_regs[i+1])
	    fprintf (file, "\tld [%s+%d],%s\n",
		     base, offset + 4 * n_regs + 4, reg_names[i+1]),
	    n_regs += 2;
	}
    }
  return n_regs;
}

/* Static variables we want to share between prologue and epilogue.  */

/* Number of live general or floating point registers needed to be saved
   (as 4-byte quantities).  This is only done if TARGET_EPILOGUE.  */
static int num_gfregs;

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
     char *reg;
{
  if (num >= 0 || ! TARGET_ARCH64)
    {
      fprintf (file, "\tsethi %%hi(%d),%s\n", num, reg);
      if ((num & 0x3ff) != 0)
	fprintf (file, "\tor %s,%%lo(%d),%s\n", reg, num, reg);
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
	  
      fprintf (file, "\tsethi %%hi(%d),%s\n\txor %s,%d,%s\n",
	       inv, reg, reg, low, reg);
    }
}

/* Output code for the function prologue.  */

void
output_function_prologue (file, size, leaf_function)
     FILE *file;
     int size;
     int leaf_function;
{
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
  else if (! leaf_function && ! TARGET_BROKEN_SAVERESTORE)
    {
      if (actual_fsize <= 4096)
	fprintf (file, "\tsave %%sp,-%d,%%sp\n", actual_fsize);
      else if (actual_fsize <= 8192)
	{
	  fprintf (file, "\tsave %%sp,-4096,%%sp\n");
	  fprintf (file, "\tadd %%sp,-%d,%%sp\n", actual_fsize - 4096);
	}
      else
	{
	  build_big_number (file, -actual_fsize, "%g1");
	  fprintf (file, "\tsave %%sp,%%g1,%%sp\n");
	}
    }
  else if (! leaf_function && TARGET_BROKEN_SAVERESTORE)
    {
      /* We assume the environment will properly handle or otherwise avoid
	 trouble associated with an interrupt occurring after the `save' or
	 trap occurring during it.  */
      fprintf (file, "\tsave\n");

      if (actual_fsize <= 4096)
	fprintf (file, "\tadd %%fp,-%d,%%sp\n", actual_fsize);
      else if (actual_fsize <= 8192)
	{
	  fprintf (file, "\tadd %%fp,-4096,%%sp\n");
	  fprintf (file, "\tadd %%fp,-%d,%%sp\n", actual_fsize - 4096);
	}
      else
	{
	  build_big_number (file, -actual_fsize, "%g1");
	  fprintf (file, "\tadd %%fp,%%g1,%%sp\n");
	}
    }
  else /* leaf function */
    {
      if (actual_fsize <= 4096)
	fprintf (file, "\tadd %%sp,-%d,%%sp\n", actual_fsize);
      else if (actual_fsize <= 8192)
	{
	  fprintf (file, "\tadd %%sp,-4096,%%sp\n");
	  fprintf (file, "\tadd %%sp,-%d,%%sp\n", actual_fsize - 4096);
	}
      else
	{
	  build_big_number (file, -actual_fsize, "%g1");
	  fprintf (file, "\tadd %%sp,%%g1,%%sp\n");
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
      char *base;

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
	  fprintf (file, "\tadd %s,%%g1,%%g1\n", frame_base_name);
	  base = "%g1";
	  offset = 0;
	}
      else
	{
	  base = frame_base_name;
	}

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
     int size;
     int leaf_function;
{
  char *ret;

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

  /* Restore any call saved registers.  */
  if (num_gfregs)
    {
      int offset, n_regs;
      char *base;

      offset = -apparent_fsize + frame_base_offset;
      if (offset < -4096 || offset + num_gfregs * 4 > 4096 - 8 /*double*/)
	{
	  build_big_number (file, offset, "%g1");
	  fprintf (file, "\tadd %s,%%g1,%%g1\n", frame_base_name);
	  base = "%g1";
	  offset = 0;
	}
      else
	{
	  base = frame_base_name;
	}

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
    ret = (SKIP_CALLERS_UNIMP_P ? "jmp %o7+12" : "retl");
  else
    ret = (SKIP_CALLERS_UNIMP_P ? "jmp %i7+12" : "ret");

  if (TARGET_EPILOGUE || leaf_label)
    {
      int old_target_epilogue = TARGET_EPILOGUE;
      target_flags &= ~old_target_epilogue;

      if (! leaf_function)
	{
	  /* If we wound up with things in our delay slot, flush them here.  */
	  if (current_function_epilogue_delay_list)
	    {
	      rtx insn = emit_jump_insn_after (gen_rtx (RETURN, VOIDmode),
					       get_last_insn ());
	      PATTERN (insn) = gen_rtx (PARALLEL, VOIDmode,
					gen_rtvec (2,
						   PATTERN (XEXP (current_function_epilogue_delay_list, 0)),
						   PATTERN (insn)));
	      final_scan_insn (insn, file, 1, 0, 1);
	    }
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
	fprintf (file, "\t%s\n\tsub %%sp,-%d,%%sp\n", ret, actual_fsize);
      else if (actual_fsize <= 8192)
	fprintf (file, "\tsub %%sp,-4096,%%sp\n\t%s\n\tsub %%sp,-%d,%%sp\n",
		 ret, actual_fsize - 4096);
      else if ((actual_fsize & 0x3ff) == 0)
	fprintf (file, "\tsethi %%hi(%d),%%g1\n\t%s\n\tadd %%sp,%%g1,%%sp\n",
		 actual_fsize, ret);
      else		 
	fprintf (file, "\tsethi %%hi(%d),%%g1\n\tor %%g1,%%lo(%d),%%g1\n\t%s\n\tadd %%sp,%%g1,%%sp\n",
		 actual_fsize, actual_fsize, ret);
      target_flags |= old_target_epilogue;
    }
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
     tree fntype, libname;
     int indirect;
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
		return 0;
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
	  || type && TREE_CODE (type) == UNION_TYPE)
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
      reg = gen_rtx (REG, mode, regno);
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
      reg = gen_rtx (REG, mode, regno);
      if (cum->prototype_p || cum->libcall_p)
	{
	  /* "* 2" because fp reg numbers are recorded in 4 byte
	     quantities.  */
	  /* ??? This will cause the value to be passed in the fp reg and
	     in the stack.  When a prototype exists we want to pass the
	     value in the reg but reserve space on the stack.  That's an
	     optimization, and is deferred [for a bit].  */
	  if ((regno - SPARC_FP_ARG_FIRST) >= SPARC_INT_ARG_MAX * 2)
	    return gen_rtx (PARALLEL, mode,
			    gen_rtvec (2,
				       gen_rtx (EXPR_LIST, VOIDmode,
						NULL_RTX, const0_rtx),
				       gen_rtx (EXPR_LIST, VOIDmode,
						reg, const0_rtx)));
	  else
	    return reg;
	}
      else
	{
	  if ((regno - SPARC_FP_ARG_FIRST) < SPARC_INT_ARG_MAX * 2)
	    {
	      int regbase = (incoming_p
			     ? SPARC_INCOMING_INT_ARG_FIRST
			     : SPARC_OUTGOING_INT_ARG_FIRST);
	      int intreg = regbase + (regno - SPARC_FP_ARG_FIRST) / 2;
	      return gen_rtx (PARALLEL, mode,
			      gen_rtvec (2,
					 gen_rtx (EXPR_LIST, VOIDmode,
						  gen_rtx (REG, mode, intreg),
						  const0_rtx),
					 gen_rtx (EXPR_LIST, VOIDmode,
						  reg, const0_rtx)));
	    }
	  else
	    return gen_rtx (PARALLEL, mode,
			    gen_rtvec (2,
				       gen_rtx (EXPR_LIST, VOIDmode,
						NULL_RTX, const0_rtx),
				       gen_rtx (EXPR_LIST, VOIDmode,
						reg, const0_rtx)));
	}
    }
  else if (type && TREE_CODE (type) == RECORD_TYPE)
    {
      /* Structures up to 16 bytes in size are passed in arg slots on the
	 stack and are promoted to registers where possible.  */
      tree field;
      rtx ret;
      int i;
      int nregs;
      /* Starting bit position of a sequence of integer fields, counted from
	 msb of left most byte, -1 if last field wasn't an int.  */
      /* ??? This isn't entirely necessary, some simplification
	 may be possible.  */
      int start_int_bitpos;
      /* Current bitpos in struct, counted from msb of left most byte.  */
      int bitpos, this_slotno;
      /* The ABI obviously doesn't specify how packed
	 structures are passed.  These are defined to be passed
	 in int regs if possible, otherwise memory.  */
      int packed_p = 0;

      if (int_size_in_bytes (type) > 16)
	abort (); /* shouldn't get here */

      /* We need to compute how many registers are needed so we can allocate
	 the PARALLEL but before we can do that we need to know whether there
	 are any packed fields.  If there are, int regs are used regardless of
	 whether there are fp values present.  */
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) == FIELD_DECL
	      && DECL_PACKED (field))
	    {
	      packed_p = 1;
	      break;
	    }
	}

      /* Compute how many registers we need.  */
      nregs = 0;
      start_int_bitpos = -1;
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  bitpos = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));
	  this_slotno = slotno + bitpos / BITS_PER_WORD;
	  if (TREE_CODE (field) == FIELD_DECL)
	    {
	      if (TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
		  && TARGET_FPU
		  && ! packed_p
		  && named)
		{
		  /* There's no need to check this_slotno < SPARC_FP_ARG MAX.
		     If it wasn't true we wouldn't be here.  */
		  nregs++;
		  start_int_bitpos = -1;
		}
	      else if (this_slotno < SPARC_INT_ARG_MAX)
		{
		  if (start_int_bitpos == -1)
		    {
		      nregs++;
		      start_int_bitpos = bitpos;
		    }
		  else
		    {
		      if (bitpos % BITS_PER_WORD == 0)
			nregs++;
		    }
		}
	    }
	}
      if (nregs == 0)
	abort ();

      ret = gen_rtx (PARALLEL, BLKmode, rtvec_alloc (nregs + 1));

      /* ??? This causes the entire struct to be passed in memory.
	 This isn't necessary, but is left for later.  */
      XVECEXP (ret, 0, 0) = gen_rtx (EXPR_LIST, VOIDmode, NULL_RTX,
				     const0_rtx);

      /* Fill in the entries.  */
      start_int_bitpos = -1;
      for (i = 1, field = TYPE_FIELDS (type);
	   field;
	   field = TREE_CHAIN (field))
	{
	  bitpos = TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field));
	  this_slotno = slotno + bitpos / BITS_PER_WORD;
	  if (TREE_CODE (field) == FIELD_DECL)
	    {
	      if (TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
		  && TARGET_FPU
		  && ! packed_p
		  && named)
		{
		  reg = gen_rtx (REG, DECL_MODE (field),
				 (SPARC_FP_ARG_FIRST + this_slotno * 2
				  + (DECL_MODE (field) == SFmode
				     && (bitpos & 32) != 0)));
		  XVECEXP (ret, 0, i) = gen_rtx (EXPR_LIST, VOIDmode, reg,
						 GEN_INT (bitpos / BITS_PER_UNIT));
		  i++;
		  start_int_bitpos = -1;
		}
	      else
		{
		  if (this_slotno < SPARC_INT_ARG_MAX
		      && (start_int_bitpos == -1
			  || bitpos % BITS_PER_WORD == 0))
		    {
		      enum machine_mode mode;

		      /* If this is the trailing part of a word, only load
			 that much into the register.  Otherwise load the
			 whole register.  Note that in the latter case we may
			 pick up unwanted bits.  It's not a problem at the
			 moment but may wish to revisit.  */
		      if (bitpos % BITS_PER_WORD != 0)
			mode = mode_for_size (BITS_PER_WORD - bitpos % BITS_PER_WORD,
					      MODE_INT, 0);
		      else
			mode = word_mode;

		      regno = regbase + this_slotno;
		      reg = gen_rtx (REG, mode, regno);
		      XVECEXP (ret, 0, i) = gen_rtx (EXPR_LIST, VOIDmode, reg,
						     GEN_INT (bitpos / BITS_PER_UNIT));
		      i++;
		      if (start_int_bitpos == -1)
			start_int_bitpos = bitpos;
		    }
		}
	    }
	}
      if (i != nregs + 1)
	abort ();

      return ret;
    }
  else if (type && TREE_CODE (type) == UNION_TYPE)
    {
      enum machine_mode mode;
      int bytes = int_size_in_bytes (type);

      if (bytes > 16)
	abort ();

      mode = mode_for_size (bytes * BITS_PER_UNIT, MODE_INT, 0);
      reg = gen_rtx (REG, mode, regno);
    }
  else
    {
      /* Scalar or complex int.  */
      reg = gen_rtx (REG, mode, regno);
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
   v9: aggregates greater than 16 bytes are passed by reference.
   For Pascal, also pass arrays by reference.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  if (TARGET_ARCH32)
    {
      return (type && AGGREGATE_TYPE_P (type)
	      || mode == TFmode || mode == TCmode);
    }
  else
    {
      return ((type && TREE_CODE (type) == ARRAY_TYPE)
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
  if (TARGET_ARCH64 && type && TREE_CODE (type) == RECORD_TYPE)
    {
      return upward;
    }

  /* This is the default definition.  */
  return (! BYTES_BIG_ENDIAN
	  ? upward
	  : ((mode == BLKmode
	      ? (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
		 && int_size_in_bytes (type) < (PARM_BOUNDARY / BITS_PER_UNIT))
	      : GET_MODE_BITSIZE (mode) < PARM_BOUNDARY)
	     ? downward : upward));
}

/* Do what is necessary for `va_start'.  The argument is ignored.

   We look at the current function to determine if stdarg or varargs
   is used and return the address of the first unnamed parameter.  */

rtx
sparc_builtin_saveregs (arglist)
     tree arglist;
{
  tree fntype = TREE_TYPE (current_function_decl);
  int stdarg = (TYPE_ARG_TYPES (fntype) != 0
		&& (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		    != void_type_node));
  int first_reg = current_function_args_info.words;
  rtx address;
  int regno;

  for (regno = first_reg; regno < NPARM_REGS (word_mode); regno++)
    emit_move_insn (gen_rtx (MEM, word_mode,
			     gen_rtx (PLUS, Pmode,
				      frame_pointer_rtx,
				      GEN_INT (STACK_POINTER_OFFSET
					       + UNITS_PER_WORD * regno))),
		    gen_rtx (REG, word_mode,
			     BASE_INCOMING_ARG_REG (word_mode) + regno));

  address = gen_rtx (PLUS, Pmode,
		     frame_pointer_rtx,
		     GEN_INT (STACK_POINTER_OFFSET
			      + UNITS_PER_WORD * first_reg));

  if (flag_check_memory_usage
      && first_reg < NPARM_REGS (word_mode))
    emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
		       address, ptr_mode,
		       GEN_INT (UNITS_PER_WORD 
			 	* (NPARM_REGS (word_mode) - first_reg)),
		       TYPE_MODE (sizetype), GEN_INT (MEMORY_USE_RW),
		       TYPE_MODE (integer_type_node));

  return address;
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label.  OP is the conditional expression.
   XEXP (OP, 0) is assumed to be a condition code register (integer or
   floating point) and its mode specifies what kind of comparison we made.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   ANNUL is non-zero if we should generate an annulling branch.

   NOOP is non-zero if we have to follow this branch by a noop.  */

char *
output_cbranch (op, label, reversed, annul, noop)
     rtx op;
     int label;
     int reversed, annul, noop;
{
  static char string[20];
  enum rtx_code code = GET_CODE (op);
  rtx cc_reg = XEXP (op, 0);
  enum machine_mode mode = GET_MODE (cc_reg);
  static char v8_labelno[] = " %lX";
  static char v9_icc_labelno[] = " %%icc,%lX";
  static char v9_xcc_labelno[] = " %%xcc,%lX";
  static char v9_fcc_labelno[] = " %%fccX,%lY";
  char *labelno;
  int labeloff;

  /* ??? !v9: FP branches cannot be preceded by another floating point insn.
     Because there is currently no concept of pre-delay slots, we can fix
     this only by always emitting a nop before a floating point branch.  */

  if ((mode == CCFPmode || mode == CCFPEmode) && ! TARGET_V9)
    strcpy (string, "nop\n\t");
  else
    string[0] = '\0';

  /* If not floating-point or if EQ or NE, we can just reverse the code.  */
  if (reversed
      && ((mode != CCFPmode && mode != CCFPEmode) || code == EQ || code == NE))
    code = reverse_condition (code), reversed = 0;

  /* Start by writing the branch condition.  */
  switch (code)
    {
    case NE:
      if (mode == CCFPmode || mode == CCFPEmode)
	strcat (string, "fbne");
      else
	strcpy (string, "bne");
      break;

    case EQ:
      if (mode == CCFPmode || mode == CCFPEmode)
	strcat (string, "fbe");
      else
	strcpy (string, "be");
      break;

    case GE:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbul");
	  else
	    strcat (string, "fbge");
	}
      else if (mode == CC_NOOVmode)
	strcpy (string, "bpos");
      else
	strcpy (string, "bge");
      break;

    case GT:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbule");
	  else
	    strcat (string, "fbg");
	}
      else
	strcpy (string, "bg");
      break;

    case LE:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbug");
	  else
	    strcat (string, "fble");
	}
      else
	strcpy (string, "ble");
      break;

    case LT:
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  if (reversed)
	    strcat (string, "fbuge");
	  else
	    strcat (string, "fbl");
	}
      else if (mode == CC_NOOVmode)
	strcpy (string, "bneg");
      else
	strcpy (string, "bl");
      break;

    case GEU:
      strcpy (string, "bgeu");
      break;

    case GTU:
      strcpy (string, "bgu");
      break;

    case LEU:
      strcpy (string, "bleu");
      break;

    case LTU:
      strcpy (string, "blu");
      break;
    }

  /* Now add the annulling, the label, and a possible noop.  */
  if (annul)
    strcat (string, ",a");

  /* ??? If v9, optional prediction bit ",pt" or ",pf" goes here.  */

  if (! TARGET_V9)
    {
      labeloff = 3;
      labelno = v8_labelno;
    }
  else
    {
      labeloff = 9;
      if (mode == CCFPmode || mode == CCFPEmode)
	{
	  labeloff = 10;
	  labelno = v9_fcc_labelno;
	  /* Set the char indicating the number of the fcc reg to use.  */
	  labelno[6] = REGNO (cc_reg) - SPARC_FIRST_V9_FCC_REG + '0';
	}
      else if (mode == CCXmode || mode == CCX_NOOVmode)
	labelno = v9_xcc_labelno;
      else
	labelno = v9_icc_labelno;
    }
  /* Set the char indicating the number of the operand containing the
     label_ref.  */
  labelno[labeloff] = label + '0';
  strcat (string, labelno);

  if (noop)
    strcat (string, "\n\tnop");

  return string;
}

/* Return the string to output a conditional branch to LABEL, testing
   register REG.  LABEL is the operand number of the label; REG is the
   operand number of the reg.  OP is the conditional expression.  The mode
   of REG says what kind of comparison we made.

   REVERSED is non-zero if we should reverse the sense of the comparison.

   ANNUL is non-zero if we should generate an annulling branch.

   NOOP is non-zero if we have to follow this branch by a noop.  */

char *
output_v9branch (op, reg, label, reversed, annul, noop)
     rtx op;
     int reg, label;
     int reversed, annul, noop;
{
  static char string[20];
  enum rtx_code code = GET_CODE (op);
  enum machine_mode mode = GET_MODE (XEXP (op, 0));
  static char labelno[] = " %X,%lX";

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
      break;

    case EQ:
      strcpy (string, "brz");
      break;

    case GE:
      strcpy (string, "brgez");
      break;

    case LT:
      strcpy (string, "brlz");
      break;

    case LE:
      strcpy (string, "brlez");
      break;

    case GT:
      strcpy (string, "brgz");
      break;

    default:
      abort ();
    }

  /* Now add the annulling, reg, label, and nop.  */
  if (annul)
    strcat (string, ",a");

  /* ??? Optional prediction bit ",pt" or ",pf" goes here.  */

  labelno[2] = reg + '0';
  labelno[6] = label + '0';
  strcat (string, labelno);

  if (noop)
    strcat (string, "\n\tnop");

  return string;
}

/* Output assembler code to return from a function.  */

/* ??? v9: Update to use the new `return' instruction.  Also, add patterns to
   md file for the `return' instruction.  */

char *
output_return (operands)
     rtx *operands;
{
  if (leaf_label)
    {
      operands[0] = leaf_label;
      return "b,a %l0";
    }
  else if (leaf_function)
    {
      /* If we didn't allocate a frame pointer for the current function,
	 the stack pointer might have been adjusted.  Output code to
	 restore it now.  */

      operands[0] = gen_rtx (CONST_INT, VOIDmode, actual_fsize);

      /* Use sub of negated value in first two cases instead of add to
	 allow actual_fsize == 4096.  */

      if (actual_fsize <= 4096)
	{
	  if (SKIP_CALLERS_UNIMP_P)
	    return "jmp %%o7+12\n\tsub %%sp,-%0,%%sp";
	  else
	    return "retl\n\tsub %%sp,-%0,%%sp";
	}
      else if (actual_fsize <= 8192)
	{
	  operands[0] = gen_rtx (CONST_INT, VOIDmode, actual_fsize - 4096);
	  if (SKIP_CALLERS_UNIMP_P)
	    return "sub %%sp,-4096,%%sp\n\tjmp %%o7+12\n\tsub %%sp,-%0,%%sp";
	  else
	    return "sub %%sp,-4096,%%sp\n\tretl\n\tsub %%sp,-%0,%%sp";
	}
      else if (SKIP_CALLERS_UNIMP_P)
	{
	  if ((actual_fsize & 0x3ff) != 0)
	    return "sethi %%hi(%a0),%%g1\n\tor %%g1,%%lo(%a0),%%g1\n\tjmp %%o7+12\n\tadd %%sp,%%g1,%%sp";
	  else
	    return "sethi %%hi(%a0),%%g1\n\tjmp %%o7+12\n\tadd %%sp,%%g1,%%sp";
	}
      else
	{
	  if ((actual_fsize & 0x3ff) != 0)
	    return "sethi %%hi(%a0),%%g1\n\tor %%g1,%%lo(%a0),%%g1\n\tretl\n\tadd %%sp,%%g1,%%sp";
	  else
	    return "sethi %%hi(%a0),%%g1\n\tretl\n\tadd %%sp,%%g1,%%sp";
	}
    }
  else
    {
      if (SKIP_CALLERS_UNIMP_P)
	return "jmp %%i7+12\n\trestore";
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

/* Return 1 if REGNO (reg1) is even and REGNO (reg1) == REGNO (reg2) - 1.
   This makes them candidates for using ldd and std insns. 

   Note reg1 and reg2 *must* be hard registers.  To be sure we will
   abort if we are passed pseudo registers.  */

int
registers_ok_for_ldd_peep (reg1, reg2)
     rtx reg1, reg2;
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG) 
    return 0;

  if (REGNO (reg1) % 2 != 0)
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
	fputs ("\n\tnop", file);
      return;
    case '*':
      /* Output an annul flag if there's nothing for the delay slot and we
	 are optimizing.  This is always used with '(' below.  */
      /* Sun OS 4.1.1 dbx can't handle an annulled unconditional branch;
	 this is a dbx bug.  So, we only do this when optimizing.  */
      if (dbr_sequence_length () == 0 && optimize)
	fputs (",a", file);
      return;
    case '(':
      /* Output a 'nop' if there's nothing for the delay slot and we are
	 not optimizing.  This is always used with '*' above.  */
      if (dbr_sequence_length () == 0 && ! optimize)
	fputs ("\n\tnop", file);
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
      if (GET_CODE (x) != REG)
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
	enum rtx_code rc = (code == 'c'
			    ? reverse_condition (GET_CODE (x))
			    : GET_CODE (x));
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
      if (CONSTANT_P (XEXP (x, 0))
	  && ! TARGET_LIVE_G0)
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
      fputs ("+%lo(", file);
      output_addr_const (file, XEXP (x, 1));
      fputc (')', file);
    }
  else if (GET_CODE (x) == CONST_DOUBLE
	   && (GET_MODE (x) == VOIDmode
	       || GET_MODE_CLASS (GET_MODE (x)) == MODE_INT))
    {
      if (CONST_DOUBLE_HIGH (x) == 0)
	fprintf (file, "%u", CONST_DOUBLE_LOW (x));
      else if (CONST_DOUBLE_HIGH (x) == -1
	       && CONST_DOUBLE_LOW (x) < 0)
	fprintf (file, "%d", CONST_DOUBLE_LOW (x));
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
      ASM_OUTPUT_INT (file, gen_rtx (CONST_INT, VOIDmode, high));
      ASM_OUTPUT_INT (file, gen_rtx (CONST_INT, VOIDmode, low));
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
      ASM_OUTPUT_INT (file, gen_rtx (CONST_INT, VOIDmode,
				     CONST_DOUBLE_HIGH (value)));
      ASM_OUTPUT_INT (file, gen_rtx (CONST_INT, VOIDmode,
				     CONST_DOUBLE_LOW (value)));
    }
  else if (GET_CODE (value) == SYMBOL_REF
	   || GET_CODE (value) == CONST
	   || GET_CODE (value) == PLUS
	   || (TARGET_ARCH64 &&
	       (GET_CODE (value) == LABEL_REF
		|| GET_CODE (value) == CODE_LABEL
		|| GET_CODE (value) == MINUS)))
    {
      if (!TARGET_V9 || TARGET_CM_MEDLOW)
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
  register unsigned shift = 6;

  /* Only the first 30 bits of the qualifier are valid.  We must refrain from
     setting more, since some assemblers will give an error for this.  Also,
     we must be careful to avoid shifts of 32 bits or more to avoid getting
     unpredictable results.  */

  for (;;)
    {
      switch (TREE_CODE (type))
	{
	case ERROR_MARK:
	  return qualifiers;
  
	case ARRAY_TYPE:
	  if (shift < 30)
	    qualifiers |= (3 << shift);
	  shift += 2;
	  type = TREE_TYPE (type);
	  break;

	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  if (shift < 30)
	    qualifiers |= (2 << shift);
	  shift += 2;
	  type = TREE_TYPE (type);
	  break;

	case POINTER_TYPE:
	case REFERENCE_TYPE:
	case OFFSET_TYPE:
	  if (shift < 30)
	    qualifiers |= (1 << shift);
	  shift += 2;
	  type = TREE_TYPE (type);
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
	    {
	      type = TREE_TYPE (type);
	      break;
	    }

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
  rtx high_cxt = expand_shift (RSHIFT_EXPR, SImode, cxt,
			      size_int (10), 0, 1);
  rtx high_fn = expand_shift (RSHIFT_EXPR, SImode, fnaddr,
			     size_int (10), 0, 1);
  rtx low_cxt = expand_and (cxt, gen_rtx (CONST_INT, VOIDmode, 0x3ff), 0);
  rtx low_fn = expand_and (fnaddr, gen_rtx (CONST_INT, VOIDmode, 0x3ff), 0);
  rtx g1_sethi = gen_rtx (HIGH, SImode,
			  gen_rtx (CONST_INT, VOIDmode, 0x03000000));
  rtx g2_sethi = gen_rtx (HIGH, SImode,
			  gen_rtx (CONST_INT, VOIDmode, 0x05000000));
  rtx g1_ori = gen_rtx (HIGH, SImode,
			gen_rtx (CONST_INT, VOIDmode, 0x82106000));
  rtx g2_ori = gen_rtx (HIGH, SImode,
			gen_rtx (CONST_INT, VOIDmode, 0x8410A000));
  rtx tem = gen_reg_rtx (SImode);
  emit_move_insn (tem, g1_sethi);
  emit_insn (gen_iorsi3 (high_fn, high_fn, tem));
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (tramp, 0)), high_fn);
  emit_move_insn (tem, g1_ori);
  emit_insn (gen_iorsi3 (low_fn, low_fn, tem));
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (tramp, 4)), low_fn);
  emit_move_insn (tem, g2_sethi);
  emit_insn (gen_iorsi3 (high_cxt, high_cxt, tem));
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (tramp, 8)), high_cxt);
  emit_move_insn (tem, g2_ori);
  emit_insn (gen_iorsi3 (low_cxt, low_cxt, tem));
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (tramp, 16)), low_cxt);
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, SImode, tramp))));
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, SImode,
					       plus_constant (tramp, 8)))));
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, SImode,
					       plus_constant (tramp, 16)))));
}

/* The 64 bit version is simpler because it makes more sense to load the
   values as "immediate" data out of the trampoline.  It's also easier since
   we can read the PC without clobbering a register.  */

void
sparc64_initialize_trampoline (tramp, fnaddr, cxt)
     rtx tramp, fnaddr, cxt;
{
  emit_move_insn (gen_rtx (MEM, DImode, plus_constant (tramp, 24)), cxt);
  emit_move_insn (gen_rtx (MEM, DImode, plus_constant (tramp, 32)), fnaddr);
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, DImode, tramp))));
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, DImode,
					       plus_constant (tramp, 8)))));
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, DImode,
					       plus_constant (tramp, 16)))));
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, DImode,
					       plus_constant (tramp, 24)))));
  emit_insn (gen_flush (validize_mem (gen_rtx (MEM, DImode,
					       plus_constant (tramp, 32)))));
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
  extra_size	 = -STARTING_FRAME_OFFSET + FIRST_PARM_OFFSET(0);
  var_size	 = size;
  /* Also include the size needed for the 6 parameter registers.  */
  args_size	 = current_function_outgoing_args_size + 24;
  total_size	 = var_size + args_size + extra_size;
  gp_reg_size	 = 0;
  fp_reg_size	 = 0;
  gmask		 = 0;
  fmask		 = 0;
  reg_offset	 = 0;
  need_aligned_p = 0;

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

  /* ??? This looks a little suspicious.  Clarify.  */
  if (total_size == extra_size)
    total_size = extra_size = 0;

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
     char *base_reg;
     unsigned int offset;
     unsigned long gmask;
     unsigned long fmask;
     char *word_op;
     char *doubleword_op;
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
		      fprintf (file, "\t%s %s,[%s+%d]\n",
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
		    fprintf (file, "\t%s [%s+%d],%s\n",
			     doubleword_op, base_reg, offset,
			     reg_names[regno]);

		  offset += 2 * UNITS_PER_WORD;
		  regno++;
		}
	      else
		{
		  if (word_op[0] == 's')
		    {
		      fprintf (file, "\t%s %s,[%s+%d]\n",
			       word_op, reg_names[regno],
			       base_reg, offset);
		      if (dwarf2out_do_frame ())
			dwarf2out_reg_save ("", regno, offset + base_offset);
		    }
		  else
		    fprintf (file, "\t%s [%s+%d],%s\n",
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
		  fprintf (file, "\t%s %s,[%s+%d]\n",
			   word_op, reg_names[regno],
			   base_reg, offset);
		  if (dwarf2out_do_frame ())
		    dwarf2out_reg_save ("", regno, offset + base_offset);
		}
	      else
		fprintf (file, "\t%s [%s+%d],%s\n",
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
  char *sp_str = reg_names[STACK_POINTER_REGNUM];
  unsigned long gmask = current_frame_info.gmask;

  /* This is only for the human reader.  */
  fprintf (file, "\t%s#PROLOGUE# 0\n", ASM_COMMENT_START);
  fprintf (file, "\t%s# vars= %d, regs= %d/%d, args= %d, extra= %d\n",
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
      char *fp_str = reg_names[FRAME_POINTER_REGNUM];
      char *t1_str = "%g1";

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
      if (reg_offset < 4096 - 64 * UNITS_PER_WORD)
	{
	  if (size <= 4096)
	    {
	      fprintf (file, "\tadd %s,%d,%s\n",
		       sp_str, -size, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst %s,[%s+%d]\n",
			   fp_str, sp_str, reg_offset);
		  fprintf (file, "\tsub %s,%d,%s\t%s# set up frame pointer\n",
			   sp_str, -size, fp_str, ASM_COMMENT_START);
		  reg_offset += 4;
		}
	    }
	  else
	    {
	      fprintf (file, "\tset %d,%s\n\tsub %s,%s,%s\n",
		       size, t1_str, sp_str, t1_str, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst %s,[%s+%d]\n",
			   fp_str, sp_str, reg_offset);
		  fprintf (file, "\tadd %s,%s,%s\t%s# set up frame pointer\n",
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
	      fprintf (file, "\tst %s,[%s+%d]\n",
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
	      fprintf (file, "\tadd %s,%d,%s\n",
		       sp_str, -size1, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst %s,[%s+%d]\n\tsub %s,%d,%s\t%s# set up frame pointer\n",
			   fp_str, sp_str, offset, sp_str, -size1, fp_str,
			   ASM_COMMENT_START);
		  offset += 4;
		}
	    }
	  else
	    {
	      fprintf (file, "\tset %d,%s\n\tsub %s,%s,%s\n",
		       size1, t1_str, sp_str, t1_str, sp_str);
	      if (gmask & FRAME_POINTER_MASK)
		{
		  fprintf (file, "\tst %s,[%s+%d]\n\tadd %s,%s,%s\t%s# set up frame pointer\n",
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
	      fprintf (file, "\tst %s,[%s+%d]\n",
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
	  fprintf (file, "\tset %d,%s\n\tsub %s,%s,%s\n",
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
      char *sp_str = reg_names[STACK_POINTER_REGNUM];
      char *fp_str = reg_names[FRAME_POINTER_REGNUM];
      char *t1_str = "%g1";

      /* In the reload sequence, we don't need to fill the load delay
	 slots for most of the loads, also see if we can fill the final
	 delay slot if not otherwise filled by the reload sequence.  */

      if (size > 4095)
	fprintf (file, "\tset %d,%s\n", size, t1_str);

      if (frame_pointer_needed)
	{
	  if (size > 4095)
	    fprintf (file,"\tsub %s,%s,%s\t\t%s# sp not trusted here\n",
		     fp_str, t1_str, sp_str, ASM_COMMENT_START);
	  else
	    fprintf (file,"\tsub %s,%d,%s\t\t%s# sp not trusted here\n",
		     fp_str, size, sp_str, ASM_COMMENT_START);
	}

      /* Is the entire register save area offsettable from %sp?  */
      if (reg_offset < 4096 - 64 * UNITS_PER_WORD)
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

	  fprintf (file, "\tset %d,%s\n\tadd %s,%s,%s\n",
		   size1, t1_str, sp_str, t1_str, sp_str);
	}

      /* We must restore the frame pointer and return address reg first
	 because they are treated specially by the prologue output code.  */
      if (current_frame_info.gmask & FRAME_POINTER_MASK)
	{
	  fprintf (file, "\tld [%s+%d],%s\n",
		   sp_str, reg_offset, fp_str);
	  reg_offset += 4;
	}
      if (current_frame_info.gmask & RETURN_ADDR_MASK)
	{
	  fprintf (file, "\tld [%s+%d],%s\n",
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
	    fprintf (file, "\tset %d,%s\n",
		     size, t1_str);
	}

      if (current_function_returns_struct)
	fprintf (file, "\tjmp %%o7+12\n");
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
	fprintf (file, "\tadd %s,%s,%s\n", sp_str, t1_str, sp_str);

      else if (size > 0)
	fprintf (file, "\tadd %s,%d,%s\n", sp_str, size, sp_str);

      else
	fprintf (file, "\tnop\n");
    }

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;
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
     int slot;
{
  rtx pat = PATTERN (trial);

  if (get_attr_length (trial) != 1)
    return 0;

  /* If %g0 is live, there are lots of things we can't handle.
     Rather than trying to find them all now, let's punt and only
     optimize things as necessary.  */
  if (TARGET_LIVE_G0)
    return 0;

  if (! reg_mentioned_p (stack_pointer_rtx, pat)
      && ! reg_mentioned_p (frame_pointer_rtx, pat))
    return 1;

  return 0;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
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
	 add an extra 'cycle'.  Note that the cost could be two cycles
	 if the reg was written late in an instruction group; we can't tell
	 here.  */
      if (insn_type == TYPE_LOAD || insn_type == TYPE_FPLOAD)
	return cost + 3;

      /* Get the delay only if the address of the store is the dependence.  */
      if (insn_type == TYPE_STORE || insn_type == TYPE_FPSTORE)
	{
	  rtx pat = PATTERN(insn);
	  rtx dep_pat = PATTERN (dep_insn);

	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    return cost;  /* This shouldn't happen!  */

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
