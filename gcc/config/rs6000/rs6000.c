/* Subroutines used for code generation on IBM RS/6000.
   Copyright (C) 1991, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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

#include <stdio.h>
#include <ctype.h>
#include "config.h"
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
#include "recog.h"
#include "expr.h"
#include "obstack.h"
#include "tree.h"

extern char *language_string;
extern int profile_block_flag;

#define min(A,B)	((A) < (B) ? (A) : (B))
#define max(A,B)	((A) > (B) ? (A) : (B))

/* Target cpu type */

enum processor_type rs6000_cpu;
char *rs6000_cpu_string;

/* Set to non-zero by "fix" operation to indicate that itrunc and
   uitrunc must be defined.  */

int rs6000_trunc_used;

/* Set to non-zero once they have been defined.  */

static int trunc_defined;

/* Set to non-zero once AIX common-mode calls have been defined.  */
static int common_mode_defined;
/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */

rtx rs6000_compare_op0, rs6000_compare_op1;
int rs6000_compare_fp_p;

#ifdef USING_SVR4_H
/* Label number of label created for -mrelocatable, to call to so we can
   get the address of the GOT section */
int rs6000_pic_labelno;
#endif

/* Whether a System V.4 varargs area was created.  */
int rs6000_sysv_varargs_p;

/* Temporary memory used to convert integer -> float */
static rtx stack_temps[NUM_MACHINE_MODES];


/* Print the options used in the assembly file.  */

extern char *version_string, *language_string;

struct asm_option
{
  char *string;
  int *variable;
  int on_value;
};

#define MAX_LINE 79

static int
output_option (file, type, name, pos)
     FILE *file;
     char *type;
     char *name;
     int pos;
{
  int type_len = strlen (type);
  int name_len = strlen (name);

  if (1 + type_len + name_len + pos > MAX_LINE)
    {
      fprintf (file, "\n # %s%s", type, name);
      return 3 + type_len + name_len;
    }
  fprintf (file, " %s%s", type, name);
  return pos + 1 + type_len + name_len;
}

static struct { char *name; int value; } m_options[] = TARGET_SWITCHES;

void
output_options (file, f_options, f_len, W_options, W_len)
     FILE *file;
     struct asm_option *f_options;
     int f_len;
     struct asm_option *W_options;
     int W_len;
{
  int j;
  int flags = target_flags;
  int pos = 32767;

  fprintf (file, " # %s %s", language_string, version_string);

  if (optimize)
    {
      char opt_string[20];
      sprintf (opt_string, "%d", optimize);
      pos = output_option (file, "-O", opt_string, pos);
    }

  if (profile_flag)
    pos = output_option (file, "-p", "", pos);

  if (profile_block_flag)
    pos = output_option (file, "-a", "", pos);

  if (inhibit_warnings)
    pos = output_option (file, "-w", "", pos);

  for (j = 0; j < f_len; j++)
    {
      if (*f_options[j].variable == f_options[j].on_value)
	pos = output_option (file, "-f", f_options[j].string, pos);
    }

  for (j = 0; j < W_len; j++)
    {
      if (*W_options[j].variable == W_options[j].on_value)
	pos = output_option (file, "-W", W_options[j].string, pos);
    }

  for (j = 0; j < sizeof m_options / sizeof m_options[0]; j++)
    {
      if (m_options[j].name[0] != '\0'
	  && m_options[j].value > 0
	  && ((m_options[j].value & flags) == m_options[j].value))
	{
	  pos = output_option (file, "-m", m_options[j].name, pos);
	  flags &= ~ m_options[j].value;
	}
    }

  if (rs6000_cpu_string != (char *)0)
    pos = output_option (file, "-mcpu=", rs6000_cpu_string, pos);

  fputs ("\n\n", file);
}


/* Override command line options.  Mostly we process the processor
   type and sometimes adjust other TARGET_ options.  */

void
rs6000_override_options ()
{
  int i;

  /* Simplify the entries below by making a mask for any POWER
     variant and any PowerPC variant.  */

#define POWER_MASKS (MASK_POWER | MASK_POWER2 | MASK_MULTIPLE | MASK_STRING)
#define POWERPC_MASKS (MASK_POWERPC | MASK_PPC_GPOPT \
		       | MASK_PPC_GFXOPT | MASK_POWERPC64)
#define POWERPC_OPT_MASKS (MASK_PPC_GPOPT | MASK_PPC_GFXOPT)

  static struct ptt
    {
      char *name;		/* Canonical processor name.  */
      enum processor_type processor; /* Processor type enum value.  */
      int target_enable;	/* Target flags to enable.  */
      int target_disable;	/* Target flags to disable.  */
    } processor_target_table[]
      = {{"common", PROCESSOR_COMMON, 0, POWER_MASKS | POWERPC_MASKS},
	 {"power", PROCESSOR_POWER,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"powerpc", PROCESSOR_POWERPC,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"rios", PROCESSOR_RIOS1,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rios1", PROCESSOR_RIOS1,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rsc", PROCESSOR_PPC601,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rsc1", PROCESSOR_PPC601,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"rios2", PROCESSOR_RIOS2,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING | MASK_POWER2,
	    POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"403", PROCESSOR_PPC403,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"601", PROCESSOR_PPC601,
	    MASK_POWER | MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"603", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"604", PROCESSOR_PPC604,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64}};

  int ptt_size = sizeof (processor_target_table) / sizeof (struct ptt);

  int multiple = TARGET_MULTIPLE;	/* save current -mmultiple/-mno-multiple status */
  int string   = TARGET_STRING;		/* save current -mstring/-mno-string status */

  profile_block_flag = 0;

  /* Identify the processor type */
  if (rs6000_cpu_string == 0)
    rs6000_cpu = PROCESSOR_DEFAULT;
  else
    {
      for (i = 0; i < ptt_size; i++)
	if (! strcmp (rs6000_cpu_string, processor_target_table[i].name))
	  {
	    rs6000_cpu = processor_target_table[i].processor;
	    target_flags |= processor_target_table[i].target_enable;
	    target_flags &= ~processor_target_table[i].target_disable;
	    break;
	  }

      if (i == ptt_size)
	{
	  error ("bad value (%s) for -mcpu= switch", rs6000_cpu_string);
	  rs6000_cpu_string = "default";
	  rs6000_cpu = PROCESSOR_DEFAULT;
	}
    }

  /* If -mmultiple or -mno-multiple was explicitly used, don't
     override with the processor default */
  if (TARGET_MULTIPLE_SET)
    target_flags = (target_flags & ~MASK_MULTIPLE) | multiple;

  /* If -mstring or -mno-string was explicitly used, don't
     override with the processor default */
  if (TARGET_STRING_SET)
    target_flags = (target_flags & ~MASK_STRING) | string;

  /* Don't allow -mmultiple or -mstring on little endian systems, because the
     hardware doesn't support the instructions used in little endian mode */
  if (!BYTES_BIG_ENDIAN)
    {
      if (TARGET_MULTIPLE)
	{
	  target_flags &= ~MASK_MULTIPLE;
	  if (TARGET_MULTIPLE_SET)
	    warning ("-mmultiple is not supported on little endian systems");
	}

      if (TARGET_STRING)
	{
	  target_flags &= ~MASK_STRING;
	  if (TARGET_STRING_SET)
	    warning ("-mstring is not supported on little endian systems");
	}
    }

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif
}

/* Create a CONST_DOUBLE from a string.  */

struct rtx_def *
rs6000_float_const (string, mode)
     char *string;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE value = REAL_VALUE_ATOF (string, mode);
  return immed_real_const_1 (value, mode);
}


/* Create a CONST_DOUBLE like immed_double_const, except reverse the
   two parts of the constant if the target is little endian.  */

struct rtx_def *
rs6000_immed_double_const (i0, i1, mode)
     HOST_WIDE_INT i0, i1;
     enum machine_mode mode;
{
  if (! WORDS_BIG_ENDIAN)
    return immed_double_const (i1, i0, mode);

  return immed_double_const (i0, i1, mode);
}


/* Return non-zero if this function is known to have a null epilogue.  */

int
direct_return ()
{
  if (reload_completed)
    {
      rs6000_stack_t *info = rs6000_stack_info ();

      if (info->first_gp_reg_save == 32
	  && info->first_fp_reg_save == 64
	  && !info->lr_save_p
	  && !info->cr_save_p
	  && !info->push_p)
	return 1;
    }

  return 0;
}

/* Returns 1 always.  */

int
any_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return 1;
}

/* Return 1 if OP is a constant that can fit in a D field.  */

int
short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned) (INTVAL (op) + 0x8000) < 0x10000);
}

/* Similar for a unsigned D field.  */

int
u_short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffff0000) == 0);
}

/* Return 1 if OP is a CONST_INT that cannot fit in a signed D field.  */

int
non_short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned) (INTVAL (op) + 0x8000) >= 0x10000);
}

/* Returns 1 if OP is a register that is not special (i.e., not MQ,
   ctr, or lr).  */

int
gpc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG || REGNO (op) >= 67 || REGNO (op) < 64));
}

/* Returns 1 if OP is either a pseudo-register or a register denoting a
   CR field.  */

int
cc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || CR_REGNO_P (REGNO (op))));
}

/* Returns 1 if OP is either a constant integer valid for a D-field or a
   non-special register.  If a register, it must be in the proper mode unless
   MODE is VOIDmode.  */

int
reg_or_short_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  return short_cint_operand (op, mode) || gpc_reg_operand (op, mode);
}

/* Similar, except check if the negation of the constant would be valid for
   a D-field.  */

int
reg_or_neg_short_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return CONST_OK_FOR_LETTER_P (INTVAL (op), 'P');

  return gpc_reg_operand (op, mode);
}

/* Return 1 if the operand is either a register or an integer whose high-order
   16 bits are zero.  */

int
reg_or_u_short_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT
      && (INTVAL (op) & 0xffff0000) == 0)
    return 1;

  return gpc_reg_operand (op, mode);
}

/* Return 1 is the operand is either a non-special register or ANY
   constant integer.  */

int
reg_or_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return GET_CODE (op) == CONST_INT || gpc_reg_operand (op, mode);
}

/* Return 1 if the operand is a CONST_DOUBLE and it can be put into a register
   with one instruction per word.  We only do this if we can safely read
   CONST_DOUBLE_{LOW,HIGH}.  */

int
easy_fp_constant (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  rtx low, high;

  if (GET_CODE (op) != CONST_DOUBLE
      || GET_MODE (op) != mode
      || GET_MODE_CLASS (mode) != MODE_FLOAT)
    return 0;

  high = operand_subword (op, 0, 0, mode);
  low = operand_subword (op, 1, 0, mode);

  if (high == 0 || ! input_operand (high, word_mode))
    return 0;

  return (mode == SFmode
	  || (low != 0 && input_operand (low, word_mode)));
}

/* Return 1 if the operand is an offsettable memory address.  */

int
offsettable_addr_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return offsettable_address_p (reload_completed | reload_in_progress,
				mode, op);
}

/* Return 1 if the operand is either a floating-point register, a pseudo
   register, or memory.  */

int
fp_reg_or_mem_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (memory_operand (op, mode)
	  || (register_operand (op, mode)
	      && (GET_CODE (op) != REG
		  || REGNO (op) >= FIRST_PSEUDO_REGISTER
		  || FP_REGNO_P (REGNO (op)))));
}

/* Return 1 if the operand is either an easy FP constant (see above) or
   memory.  */

int
mem_or_easy_const_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return memory_operand (op, mode) || easy_fp_constant (op, mode);
}

/* Return 1 if the operand is either a non-special register or an item
   that can be used as the operand of an SI add insn.  */

int
add_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return (reg_or_short_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffff) == 0));
}

/* Return 1 if OP is a constant but not a valid add_operand.  */

int
non_add_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned) (INTVAL (op) + 0x8000) >= 0x10000
	  && (INTVAL (op) & 0xffff) != 0);
}

/* Return 1 if the operand is a non-special register or a constant that
   can be used as the operand of an OR or XOR insn on the RS/6000.  */

int
logical_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && ((INTVAL (op) & 0xffff0000) == 0
		  || (INTVAL (op) & 0xffff) == 0)));
}

/* Return 1 if C is a constant that is not a logical operand (as
   above).  */

int
non_logical_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) & 0xffff0000) != 0
	  && (INTVAL (op) & 0xffff) != 0);
}

/* Return 1 if C is a constant that can be encoded in a mask on the
   RS/6000.  It is if there are no more than two 1->0 or 0->1 transitions.
   Reject all ones and all zeros, since these should have been optimized
   away and confuse the making of MB and ME.  */

int
mask_constant (c)
     register int c;
{
  int i;
  int last_bit_value;
  int transitions = 0;

  if (c == 0 || c == ~0)
    return 0;

  last_bit_value = c & 1;

  for (i = 1; i < 32; i++)
    if (((c >>= 1) & 1) != last_bit_value)
      last_bit_value ^= 1, transitions++;

  return transitions <= 2;
}

/* Return 1 if the operand is a constant that is a mask on the RS/6000. */

int
mask_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_INT && mask_constant (INTVAL (op));
}

/* Return 1 if the operand is either a non-special register or a
   constant that can be used as the operand of an RS/6000 logical AND insn.  */

int
and_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return (reg_or_short_operand (op, mode)
	  || logical_operand (op, mode)
	  || mask_operand (op, mode));
}

/* Return 1 if the operand is a constant but not a valid operand for an AND
   insn.  */

int
non_and_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_INT && ! and_operand (op, mode);
}

/* Return 1 if the operand is a general register or memory operand.  */

int
reg_or_mem_operand (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  return gpc_reg_operand (op, mode) || memory_operand (op, mode);
}

/* Return 1 if the operand is a general register or memory operand without
   pre-inc or pre_dec which produces invalid form of PowerPC lwa
   instruction.  */

int
lwa_operand (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  rtx inner = op;

  if (reload_completed && GET_CODE (inner) == SUBREG)
    inner = SUBREG_REG (inner);
    
  return gpc_reg_operand (inner, mode)
    || (memory_operand (inner, mode)
	&& GET_CODE (XEXP (inner, 0)) != PRE_INC
	&& GET_CODE (XEXP (inner, 0)) != PRE_DEC);
}

/* Return 1 if the operand, used inside a MEM, is a valid first argument
   to CALL.  This is a SYMBOL_REF or a pseudo-register, which will be
   forced to lr.  */

int
call_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  return (GET_CODE (op) == SYMBOL_REF
	  || (GET_CODE (op) == REG && REGNO (op) >= FIRST_PSEUDO_REGISTER));
}


/* Return 1 if the operand is a SYMBOL_REF for a function known to be in
   this file.  */

int
current_file_function_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == SYMBOL_REF
	  && (SYMBOL_REF_FLAG (op)
	      || op == XEXP (DECL_RTL (current_function_decl), 0)));
}


/* Return 1 if this operand is a valid input for a move insn.  */

int
input_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  /* Memory is always valid.  */
  if (memory_operand (op, mode))
    return 1;

  /* For floating-point, easy constants are valid.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && CONSTANT_P (op)
      && easy_fp_constant (op, mode))
    return 1;

  /* For floating-point or multi-word mode, the only remaining valid type
     is a register.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      || GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return register_operand (op, mode);

  /* The only cases left are integral modes one word or smaller (we
     do not get called for MODE_CC values).  These can be in any
     register.  */
  if (register_operand (op, mode))
    return 1;

  /* For HImode and QImode, any constant is valid. */
  if ((mode == HImode || mode == QImode)
      && GET_CODE (op) == CONST_INT)
    return 1;

  /* A SYMBOL_REF referring to the TOC is valid.  */
  if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (op))
    return 1;

  /* Otherwise, we will be doing this SET with an add, so anything valid
     for an add will be valid.  */
  return add_operand (op, mode);
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   For incoming args we set the number of arguments in the prototype large
   so we never return an EXPR_LIST.  */

void
init_cumulative_args (cum, fntype, libname, incoming)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname;
     int incoming;
{
  static CUMULATIVE_ARGS zero_cumulative;

  *cum = zero_cumulative;
  cum->words = 0;
  cum->fregno = FP_ARG_MIN_REG;
  cum->prototype = (fntype && TYPE_ARG_TYPES (fntype));

  if (incoming)
    {
      cum->nargs_prototype = 1000;		/* don't return an EXPR_LIST */
#ifdef TARGET_V4_CALLS
      if (TARGET_V4_CALLS)
	cum->varargs_offset = RS6000_VARARGS_OFFSET;
#endif
    }

  else if (cum->prototype)
    cum->nargs_prototype = (list_length (TYPE_ARG_TYPES (fntype)) - 1
			    + (TYPE_MODE (TREE_TYPE (fntype)) == BLKmode
			       || RETURN_IN_MEMORY (TREE_TYPE (fntype))));

  else
    cum->nargs_prototype = 0;

  cum->orig_nargs = cum->nargs_prototype;
  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " ret code = %s,",
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}

#ifdef TARGET_V4_CALLS
      if (TARGET_V4_CALLS && incoming)
	fprintf (stderr, " varargs = %d, ", cum->varargs_offset);
#endif

      fprintf (stderr, " proto = %d, nargs = %d\n",
	       cum->prototype, cum->nargs_prototype);
    }
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  cum->nargs_prototype--;

#ifdef TARGET_V4_CALLS
  if (TARGET_V4_CALLS)
    {
      /* Long longs must not be split between registers and stack */
      if ((GET_MODE_CLASS (mode) != MODE_FLOAT || TARGET_SOFT_FLOAT)
	  && type && !AGGREGATE_TYPE_P (type)
	  && cum->words < GP_ARG_NUM_REG
	  && cum->words + RS6000_ARG_SIZE (mode, type, named) > GP_ARG_NUM_REG)
	{
	  cum->words = GP_ARG_NUM_REG;
	}

      /* Aggregates get passed as pointers */
      if (type && AGGREGATE_TYPE_P (type))
	cum->words++;

      /* Floats go in registers, & don't occupy space in the GP registers
	 like they do for AIX unless software floating point.  */
      else if (GET_MODE_CLASS (mode) == MODE_FLOAT
	       && TARGET_HARD_FLOAT
	       && cum->fregno <= FP_ARG_V4_MAX_REG)
	cum->fregno++;

      else
	cum->words += RS6000_ARG_SIZE (mode, type, 1);
    }
  else
#endif
    if (named)
      {
	cum->words += RS6000_ARG_SIZE (mode, type, named);
	if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_HARD_FLOAT)
	  cum->fregno++;
      }

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_adv: words = %2d, fregno = %2d, nargs = %4d, proto = %d, mode = %4s, named = %d\n",
	     cum->words, cum->fregno, cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode), named);
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On RS/6000 the first eight words of non-FP are normally in registers
   and the rest are pushed.  Under AIX, the first 13 FP args are in registers.
   Under V.4, the first 8 FP args are in registers.

   If this is floating-point and no prototype is specified, we use
   both an FP and integer register (or possibly FP reg and stack).  Library
   functions (when TYPE is zero) always have the proper types for args,
   so we can pass the FP value just in one register.  emit_library_function
   doesn't support EXPR_LIST anyway.  */

struct rtx_def *
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_arg: words = %2d, fregno = %2d, nargs = %4d, proto = %d, mode = %4s, named = %d\n",
	     cum->words, cum->fregno, cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode), named);

  /* Return a marker to indicate whether CR1 needs to set or clear the bit that V.4
     uses to say fp args were passed in registers.  Assume that we don't need the
     marker for software floating point, or compiler generated library calls.  */
  if (mode == VOIDmode)
    {
#ifdef TARGET_V4_CALLS
      if (TARGET_V4_CALLS && TARGET_HARD_FLOAT && cum->nargs_prototype < 0
	  && type && (cum->prototype || TARGET_NO_PROTOTYPE))
	return GEN_INT ((cum->fregno == FP_ARG_MIN_REG) ? -1 : 1);
#endif

      return GEN_INT (0);
    }

  if (!named)
    {
#ifdef TARGET_V4_CALLS
      if (!TARGET_V4_CALLS)
#endif
	return NULL_RTX;
    }

  if (type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return NULL_RTX;

  if (USE_FP_FOR_ARG_P (*cum, mode, type))
    {
      if ((cum->nargs_prototype > 0)
#ifdef TARGET_V4_CALLS
	  || TARGET_V4_CALLS	/* V.4 never passes FP values in GP registers */
#endif
	  || !type)
	return gen_rtx (REG, mode, cum->fregno);

      return gen_rtx (EXPR_LIST, VOIDmode,
		      ((cum->words < GP_ARG_NUM_REG)
		       ? gen_rtx (REG, mode, GP_ARG_MIN_REG + cum->words)
		       : NULL_RTX),
		      gen_rtx (REG, mode, cum->fregno));
    }

#ifdef TARGET_V4_CALLS
  /* Long longs won't be split between register and stack */
  else if (TARGET_V4_CALLS &&
	   cum->words + RS6000_ARG_SIZE (mode, type, named) > GP_ARG_NUM_REG)
    {
      return NULL_RTX;
    }
#endif

  else if (cum->words < GP_ARG_NUM_REG)
    return gen_rtx (REG, mode, GP_ARG_MIN_REG + cum->words);

  return NULL_RTX;
}

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  if (! named)
    return 0;

#ifdef TARGET_V4_CALLS
  if (TARGET_V4_CALLS)
    return 0;
#endif

  if (USE_FP_FOR_ARG_P (*cum, mode, type))
    {
      if (cum->nargs_prototype >= 0)
	return 0;
    }

  if (cum->words < GP_ARG_NUM_REG
      && GP_ARG_NUM_REG < (cum->words + RS6000_ARG_SIZE (mode, type, named)))
    {
      int ret = GP_ARG_NUM_REG - cum->words;
      if (ret && TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_partial_nregs: %d\n", ret);

      return ret;
    }

  return 0;
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.

   Under V.4, structures and unions are passed by reference.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
#ifdef TARGET_V4_CALLS
  if (TARGET_V4_CALLS && type && AGGREGATE_TYPE_P (type))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: aggregate\n");

      return 1;
    }
#endif

  return 0;
}


/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

void
setup_incoming_varargs (cum, mode, type, pretend_size, no_rtl)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int *pretend_size;
     int no_rtl;

{
  rtx save_area = virtual_incoming_args_rtx;
  int reg_size	= (TARGET_64BIT) ? 8 : 4;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "setup_vararg: words = %2d, fregno = %2d, nargs = %4d, proto = %d, mode = %4s, no_rtl= %d\n",
	     cum->words, cum->fregno, cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode), no_rtl);

#ifdef TARGET_V4_CALLS
  if (TARGET_V4_CALLS && !no_rtl)
    {
      rs6000_sysv_varargs_p = 1;
      save_area = plus_constant (frame_pointer_rtx, RS6000_VARARGS_OFFSET);
    }
#endif

  if (cum->words < 8)
    {
      int first_reg_offset = cum->words;

      if (MUST_PASS_IN_STACK (mode, type))
	first_reg_offset += RS6000_ARG_SIZE (TYPE_MODE (type), type, 1);

      if (first_reg_offset > GP_ARG_NUM_REG)
	first_reg_offset = GP_ARG_NUM_REG;

      if (!no_rtl && first_reg_offset != GP_ARG_NUM_REG)
	move_block_from_reg
	  (GP_ARG_MIN_REG + first_reg_offset,
	   gen_rtx (MEM, BLKmode,
		    plus_constant (save_area, first_reg_offset * reg_size)),
	   GP_ARG_NUM_REG - first_reg_offset,
	   (GP_ARG_NUM_REG - first_reg_offset) * UNITS_PER_WORD);

      *pretend_size = (GP_ARG_NUM_REG - first_reg_offset) * UNITS_PER_WORD;
    }

#ifdef TARGET_V4_CALLS
  /* Save FP registers if needed.  */
  if (TARGET_V4_CALLS && TARGET_HARD_FLOAT && !no_rtl)
    {
      int fregno     = cum->fregno;
      int num_fp_reg = FP_ARG_V4_MAX_REG + 1 - fregno;

      if (num_fp_reg >= 0)
	{
	  rtx cr1 = gen_rtx (REG, CCmode, 69);
	  rtx lab = gen_label_rtx ();
	  int off = (GP_ARG_NUM_REG * reg_size) + ((fregno - FP_ARG_MIN_REG) * 8);

	  emit_jump_insn (gen_rtx (SET, VOIDmode,
				   pc_rtx,
				   gen_rtx (IF_THEN_ELSE, VOIDmode,
					    gen_rtx (NE, VOIDmode, cr1, const0_rtx),
					    gen_rtx (LABEL_REF, VOIDmode, lab),
					    pc_rtx)));

	  while ( num_fp_reg-- >= 0)
	    {
	      emit_move_insn (gen_rtx (MEM, DFmode, plus_constant (save_area, off)),
			      gen_rtx (REG, DFmode, fregno++));
	      off += 8;
	    }

	  emit_label (lab);
	}
    }
#endif
}

/* If defined, is a C expression that produces the machine-specific
   code for a call to `__builtin_saveregs'.  This code will be moved
   to the very beginning of the function, before any parameter access
   are made.  The return value of this function should be an RTX that
   contains the value to use as the return of `__builtin_saveregs'.

   The argument ARGS is a `tree_list' containing the arguments that
   were passed to `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary
   call to the library function `__builtin_saveregs'.
   
   On the Power/PowerPC return the address of the area on the stack
   used to hold arguments.  Under AIX, this includes the 8 word register
   save area.  Under V.4 this does not.  */

struct rtx_def *
expand_builtin_saveregs (args)
     tree args;
{
  return virtual_incoming_args_rtx;
}


/* Allocate a stack temp.  Only allocate one stack temp per type for a
   function.  */

struct rtx_def *
rs6000_stack_temp (mode, size)
     enum machine_mode mode;
     int size;
{
  rtx temp = stack_temps[ (int)mode ];
  rtx addr;

  if (temp == NULL_RTX)
    {
      temp = assign_stack_local (mode, size, 0);
      addr = XEXP (temp, 0);

      if ((size > 4 && !offsettable_address_p (0, mode, addr))
	  || (size <= 4 && !memory_address_p (mode, addr)))
	{
	  XEXP (temp, 0) = copy_addr_to_reg (addr);
	}

      stack_temps[ (int)mode ] = temp;
    }

  return temp;
}


/* Generate a memory reference for expand_block_move, copying volatile,
   and other bits from an original memory reference.  */

static rtx
expand_block_move_mem (mode, addr, orig_mem)
     enum machine_mode mode;
     rtx addr;
     rtx orig_mem;
{
  rtx mem = gen_rtx (MEM, mode, addr);
  MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (orig_mem);
  MEM_IN_STRUCT_P (mem) = MEM_IN_STRUCT_P (orig_mem);
  return mem;
}

/* Expand a block move operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the source
   operands[2] is the length
   operands[3] is the alignment */

#define MAX_MOVE_REG 4

int
expand_block_move (operands)
     rtx operands[];
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align	= XINT (align_rtx, 0);
  int bytes;
  int offset;
  int num_reg;
  int i;
  rtx src_reg;
  rtx dest_reg;
  rtx src_addr;
  rtx dest_addr;
  rtx tmp_reg;
  rtx stores[MAX_MOVE_REG];
  int move_bytes;

  /* If this is not a fixed size move, just call memcpy */
  if (!constp)
    return 0;

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  /* Don't support real large moves.  If string instructions are not used,
     then don't generate more than 8 loads.  */
  if (TARGET_STRING)
    {
      if (bytes > 4*8)
	return 0;
    }
  else if (!STRICT_ALIGNMENT)
    {
      if (bytes > 4*8)
	return 0;
    }
  else if (bytes > 8*align)
    return 0;

  /* Move the address into scratch registers.  */
  dest_reg = copy_addr_to_reg (XEXP (operands[0], 0));
  src_reg  = copy_addr_to_reg (XEXP (operands[1], 0));

  if (TARGET_STRING)	/* string instructions are available */
    {
      for ( ; bytes > 0; bytes -= move_bytes)
	{
	  if (bytes > 24		/* move up to 32 bytes at a time */
	      && !fixed_regs[5]
	      && !fixed_regs[6]
	      && !fixed_regs[7]
	      && !fixed_regs[8]
	      && !fixed_regs[9]
	      && !fixed_regs[10]
	      && !fixed_regs[11]
	      && !fixed_regs[12])
	    {
	      move_bytes = (bytes > 32) ? 32 : bytes;
	      emit_insn (gen_movstrsi_8reg (dest_reg,
					    src_reg,
					    GEN_INT ((move_bytes == 32) ? 0 : move_bytes),
					    align_rtx));
	    }
	  else if (bytes > 16	/* move up to 24 bytes at a time */
		   && !fixed_regs[7]
		   && !fixed_regs[8]
		   && !fixed_regs[9]
		   && !fixed_regs[10]
		   && !fixed_regs[11]
		   && !fixed_regs[12])
	    {
	      move_bytes = (bytes > 24) ? 24 : bytes;
	      emit_insn (gen_movstrsi_6reg (dest_reg,
					    src_reg,
					    GEN_INT (move_bytes),
					    align_rtx));
	    }
	  else if (bytes > 8	/* move up to 16 bytes at a time */
		   && !fixed_regs[9]
		   && !fixed_regs[10]
		   && !fixed_regs[11]
		   && !fixed_regs[12])
	    {
	      move_bytes = (bytes > 16) ? 16 : bytes;
	      emit_insn (gen_movstrsi_4reg (dest_reg,
					    src_reg,
					    GEN_INT (move_bytes),
					    align_rtx));
	    }
	  else if (bytes > 4 && !TARGET_64BIT)
	    {			/* move up to 8 bytes at a time */
	      move_bytes = (bytes > 8) ? 8 : bytes;
	      emit_insn (gen_movstrsi_2reg (dest_reg,
					    src_reg,
					    GEN_INT (move_bytes),
					    align_rtx));
	    }
	  else if (bytes >= 4 && (align >= 4 || !STRICT_ALIGNMENT))
	    {			/* move 4 bytes */
	      move_bytes = 4;
	      tmp_reg = gen_reg_rtx (SImode);
	      emit_move_insn (tmp_reg, gen_rtx (MEM, SImode, src_reg));
	      emit_move_insn (gen_rtx (MEM, SImode, dest_reg), tmp_reg);
	    }
	  else if (bytes == 2 && (align >= 2 || !STRICT_ALIGNMENT))
	    {			/* move 2 bytes */
	      move_bytes = 2;
	      tmp_reg = gen_reg_rtx (HImode);
	      emit_move_insn (tmp_reg, gen_rtx (MEM, HImode, src_reg));
	      emit_move_insn (gen_rtx (MEM, HImode, dest_reg), tmp_reg);
	    }
	  else if (bytes == 1)	/* move 1 byte */
	    {
	      move_bytes = 1;
	      tmp_reg = gen_reg_rtx (QImode);
	      emit_move_insn (tmp_reg, gen_rtx (MEM, QImode, src_reg));
	      emit_move_insn (gen_rtx (MEM, QImode, dest_reg), tmp_reg);
	    }
	  else
	    {			/* move up to 4 bytes at a time */
	      move_bytes = (bytes > 4) ? 4 : bytes;
	      emit_insn (gen_movstrsi_1reg (dest_reg,
					    src_reg,
					    GEN_INT (move_bytes),
					    align_rtx));
	    }

	  if (bytes > move_bytes)
	    {
	      emit_insn (gen_addsi3 (src_reg, src_reg, GEN_INT (move_bytes)));
	      emit_insn (gen_addsi3 (dest_reg, dest_reg, GEN_INT (move_bytes)));
	    }
	}
    }

  else			/* string instructions not available */
    {
      num_reg = offset = 0;
      for ( ; bytes > 0; (bytes -= move_bytes), (offset += move_bytes))
	{
	  /* Calculate the correct offset for src/dest */
	  if (offset == 0)
	    {
	      src_addr  = src_reg;
	      dest_addr = dest_reg;
	    }
	  else
	    {
	      src_addr  = gen_rtx (PLUS, Pmode, src_reg,  GEN_INT (offset));
	      dest_addr = gen_rtx (PLUS, Pmode, dest_reg, GEN_INT (offset));
	    }

	  /* Generate the appropriate load and store, saving the stores for later */
	  if (bytes >= 4 && (align >= 4 || !STRICT_ALIGNMENT))
	    {
	      move_bytes = 4;
	      tmp_reg = gen_reg_rtx (SImode);
	      emit_insn (gen_movsi (tmp_reg, gen_rtx (MEM, SImode, src_addr)));
	      stores[ num_reg++ ] = gen_movsi (gen_rtx (MEM, SImode, dest_addr), tmp_reg);
	    }
	  else if (bytes >= 2 && (align >= 2 || !STRICT_ALIGNMENT))
	    {
	      move_bytes = 2;
	      tmp_reg = gen_reg_rtx (HImode);
	      emit_insn (gen_movhi (tmp_reg, gen_rtx (MEM, HImode, src_addr)));
	      stores[ num_reg++ ] = gen_movhi (gen_rtx (MEM, HImode, dest_addr), tmp_reg);
	    }
	  else
	    {
	      move_bytes = 1;
	      tmp_reg = gen_reg_rtx (QImode);
	      emit_insn (gen_movqi (tmp_reg, gen_rtx (MEM, QImode, src_addr)));
	      stores[ num_reg++ ] = gen_movqi (gen_rtx (MEM, QImode, dest_addr), tmp_reg);
	    }

	  if (num_reg >= MAX_MOVE_REG)
	    {
	      for (i = 0; i < num_reg; i++)
		emit_insn (stores[i]);
	      num_reg = 0;
	    }
	}

      if (num_reg > 0)
	{
	  for (i = 0; i < num_reg; i++)
	    emit_insn (stores[i]);
	}
    }

  return 1;
}


/* Return 1 if OP is a load multiple operation.  It is known to be a
   PARALLEL and the first section will be tested.  */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Similar, but tests for store multiple.  Here, the second vector element
   is a CLOBBER.  It will be tested later.  */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0) - 1;
  int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 1);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Return 1 if OP is a comparison operation that is valid for a branch insn.
   We only check the opcode against the mode of the CC value here.  */

int
branch_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  cc_mode = GET_MODE (XEXP (op, 0));
  if (GET_MODE_CLASS (cc_mode) != MODE_CC)
    return 0;

  if ((code == GT || code == LT || code == GE || code == LE)
      && cc_mode == CCUNSmode)
    return 0;

  if ((code == GTU || code == LTU || code == GEU || code == LEU)
      && (cc_mode != CCUNSmode))
    return 0;

  return 1;
}

/* Return 1 if OP is a comparison operation that is valid for an scc insn.
   We check the opcode against the mode of the CC value and disallow EQ or
   NE comparisons for integers.  */

int
scc_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  cc_mode = GET_MODE (XEXP (op, 0));
  if (GET_MODE_CLASS (cc_mode) != MODE_CC)
    return 0;

  if (code == NE && cc_mode != CCFPmode)
    return 0;

  if ((code == GT || code == LT || code == GE || code == LE)
      && cc_mode == CCUNSmode)
    return 0;

  if ((code == GTU || code == LTU || code == GEU || code == LEU)
      && (cc_mode != CCUNSmode))
    return 0;

  if (cc_mode == CCEQmode && code != EQ && code != NE)
    return 0;

  return 1;
}

/* Return 1 if ANDOP is a mask that has no bits on that are not in the
   mask required to convert the result of a rotate insn into a shift
   left insn of SHIFTOP bits.  Both are known to be CONST_INT.  */

int
includes_lshift_p (shiftop, andop)
     register rtx shiftop;
     register rtx andop;
{
  int shift_mask = (~0 << INTVAL (shiftop));

  return (INTVAL (andop) & ~shift_mask) == 0;
}

/* Similar, but for right shift.  */

int
includes_rshift_p (shiftop, andop)
     register rtx shiftop;
     register rtx andop;
{
  unsigned shift_mask = ~0;

  shift_mask >>= INTVAL (shiftop);

  return (INTVAL (andop) & ~ shift_mask) == 0;
}

/* Return 1 if REGNO (reg1) == REGNO (reg2) - 1 making them candidates
   for lfq and stfq insns.

   Note reg1 and reg2 *must* be hard registers.  To be sure we will
   abort if we are passed pseudo registers.  */

int
registers_ok_for_quad_peep (reg1, reg2)
     rtx reg1, reg2;
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG) 
    return 0;

  return (REGNO (reg1) == REGNO (reg2) - 1);
}

/* Return 1 if addr1 and addr2 are suitable for lfq or stfq insn.  addr1 and
   addr2 must be in consecutive memory locations (addr2 == addr1 + 8).  */

int
addrs_ok_for_quad_peep (addr1, addr2)
     register rtx addr1;
     register rtx addr2;
{
  int reg1;
  int offset1;

  /* Extract an offset (if used) from the first addr.  */
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

  if (reg1 != REGNO (XEXP (addr2, 0)))
    return 0;

  /* The offset for the second addr must be 8 more than the first addr.  */
  if (INTVAL (XEXP (addr2, 1)) != offset1 + 8)
    return 0;

  /* All the tests passed.  addr1 and addr2 are valid for lfq or stfq
     instructions.  */
  return 1;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = true_regnum (in);

  if (regno >= FIRST_PSEUDO_REGISTER)
    regno = -1;

  /* We can place anything into GENERAL_REGS and can put GENERAL_REGS
     into anything.  */
  if (class == GENERAL_REGS || class == BASE_REGS
      || (regno >= 0 && INT_REGNO_P (regno)))
    return NO_REGS;

  /* Constants, memory, and FP registers can go into FP registers.  */
  if ((regno == -1 || FP_REGNO_P (regno))
      && (class == FLOAT_REGS || class == NON_SPECIAL_REGS))
    return NO_REGS;

  /* We can copy among the CR registers.  */
  if ((class == CR_REGS || class == CR0_REGS)
      && regno >= 0 && CR_REGNO_P (regno))
    return NO_REGS;

  /* Otherwise, we need GENERAL_REGS.  */
  return GENERAL_REGS;
}

/* Given a comparison operation, return the bit number in CCR to test.  We
   know this is a valid comparison.  

   SCC_P is 1 if this is for an scc.  That means that %D will have been
   used instead of %C, so the bits will be in different places.

   Return -1 if OP isn't a valid comparison for some reason.  */

int
ccr_bit (op, scc_p)
     register rtx op;
     int scc_p;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;
  int cc_regnum;
  int base_bit;

  if (GET_RTX_CLASS (code) != '<')
    return -1;

  cc_mode = GET_MODE (XEXP (op, 0));
  cc_regnum = REGNO (XEXP (op, 0));
  base_bit = 4 * (cc_regnum - 68);

  /* In CCEQmode cases we have made sure that the result is always in the
     third bit of the CR field.  */

  if (cc_mode == CCEQmode)
    return base_bit + 3;

  switch (code)
    {
    case NE:
      return scc_p ? base_bit + 3 : base_bit + 2;
    case EQ:
      return base_bit + 2;
    case GT:  case GTU:
      return base_bit + 1;
    case LT:  case LTU:
      return base_bit;

    case GE:  case GEU:
      /* If floating-point, we will have done a cror to put the bit in the
	 unordered position.  So test that bit.  For integer, this is ! LT
	 unless this is an scc insn.  */
      return cc_mode == CCFPmode || scc_p ? base_bit + 3 : base_bit;

    case LE:  case LEU:
      return cc_mode == CCFPmode || scc_p ? base_bit + 3 : base_bit + 1;

    default:
      abort ();
    }
}

/* Print an operand.  Recognize special options, documented below.  */

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    char code;
{
  int i;
  int val;

  /* These macros test for integers and extract the low-order bits.  */
#define INT_P(X)  \
((GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE)	\
 && GET_MODE (X) == VOIDmode)

#define INT_LOWPART(X) \
  (GET_CODE (X) == CONST_INT ? INTVAL (X) : CONST_DOUBLE_LOW (X))

  switch (code)
    {
    case '.':
      /* Write out an instruction after the call which may be replaced
	 with glue code by the loader.  This depends on the AIX version.  */
      asm_fprintf (file, RS6000_CALL_GLUE);
      return;

    case '*':
      /* Write the register number of the TOC register.  */
      fputs (TARGET_MINIMAL_TOC ? reg_names[30] : reg_names[2], file);
      return;

    case 'A':
      /* If X is a constant integer whose low-order 5 bits are zero,
	 write 'l'.  Otherwise, write 'r'.  This is a kludge to fix a bug
	 in the AIX assembler where "sri" with a zero shift count
	 write a trash instruction.  */
      if (GET_CODE (x) == CONST_INT && (INTVAL (x) & 31) == 0)
	putc ('l', file);
      else
	putc ('r', file);
      return;

    case 'b':
      /* Low-order 16 bits of constant, unsigned.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%b value");

      fprintf (file, "%d", INT_LOWPART (x) & 0xffff);
      return;

    case 'C':
      /* This is an optional cror needed for LE or GE floating-point
	 comparisons.  Otherwise write nothing.  */
      if ((GET_CODE (x) == LE || GET_CODE (x) == GE)
	  && GET_MODE (XEXP (x, 0)) == CCFPmode)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - 68);

	  fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2, base_bit + (GET_CODE (x) == GE));
	}
      return;

    case 'D':
      /* Similar, except that this is for an scc, so we must be able to
	 encode the test in a single bit that is one.  We do the above
	 for any LE, GE, GEU, or LEU and invert the bit for NE.  */
      if (GET_CODE (x) == LE || GET_CODE (x) == GE
	  || GET_CODE (x) == LEU || GET_CODE (x) == GEU)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - 68);

	  fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2,
		   base_bit + (GET_CODE (x) == GE || GET_CODE (x) == GEU));
	}

      else if (GET_CODE (x) == NE)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - 68);

	  fprintf (file, "crnor %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2, base_bit + 2);
	}
      return;

    case 'E':
      /* X is a CR register.  Print the number of the third bit of the CR */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%E value");

      fprintf(file, "%d", 4 * (REGNO (x) - 68) + 3);
      return;

    case 'f':
      /* X is a CR register.  Print the shift count needed to move it
	 to the high-order four bits.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%f value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - 68));
      return;

    case 'F':
      /* Similar, but print the count for the rotate in the opposite
	 direction.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%F value");
      else
	fprintf (file, "%d", 32 - 4 * (REGNO (x) - 68));
      return;

    case 'G':
      /* X is a constant integer.  If it is negative, print "m",
	 otherwise print "z".  This is to make a aze or ame insn.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("invalid %%G value");
      else if (INTVAL (x) >= 0)
	putc ('z', file);
      else
	putc ('m', file);
      return;
	
    case 'h':
      /* If constant, output low-order five bits.  Otherwise,
	 write normally. */
      if (INT_P (x))
	fprintf (file, "%d", INT_LOWPART (x) & 31);
      else
	print_operand (file, x, 0);
      return;

    case 'I':
      /* Print `i' if this is a constant, else nothing.  */
      if (INT_P (x))
	putc ('i', file);
      return;

    case 'j':
      /* Write the bit number in CCR for jump.  */
      i = ccr_bit (x, 0);
      if (i == -1)
	output_operand_lossage ("invalid %%j code");
      else
	fprintf (file, "%d", i);
      return;

    case 'J':
      /* Similar, but add one for shift count in rlinm for scc and pass
	 scc flag to `ccr_bit'.  */
      i = ccr_bit (x, 1);
      if (i == -1)
	output_operand_lossage ("invalid %%J code");
      else
	/* If we want bit 31, write a shift count of zero, not 32.  */
	fprintf (file, "%d", i == 31 ? 0 : i + 1);
      return;

    case 'k':
      /* X must be a constant.  Write the 1's complement of the
	 constant.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%k value");

      fprintf (file, "%d", ~ INT_LOWPART (x));
      return;

    case 'L':
      /* Write second word of DImode or DFmode reference.  Works on register
	 or non-indexed memory only.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%d", REGNO (x) + 1);
      else if (GET_CODE (x) == MEM)
	{
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of four.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 4));
	  else
	    output_address (plus_constant (XEXP (x, 0), 4));
	}
      return;
			    
    case 'm':
      /* MB value for a mask operand.  */
      if (! mask_operand (x, VOIDmode))
	output_operand_lossage ("invalid %%m value");

      val = INT_LOWPART (x);

      /* If the high bit is set and the low bit is not, the value is zero.
	 If the high bit is zero, the value is the first 1 bit we find from
	 the left.  */
      if (val < 0 && (val & 1) == 0)
	{
	  fprintf (file, "0");
	  return;
	}
      else if (val >= 0)
	{
	  for (i = 1; i < 32; i++)
	    if ((val <<= 1) < 0)
	      break;
	  fprintf (file, "%d", i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the right.  The result is its
	 number plus 1. We know the low-order bit is one.  */
      for (i = 0; i < 32; i++)
	if (((val >>= 1) & 1) == 0)
	  break;

      /* If we ended in ...01, I would be 0.  The correct value is 31, so
	 we want 31 - i.  */
      fprintf (file, "%d", 31 - i);
      return;

    case 'M':
      /* ME value for a mask operand.  */
      if (! mask_operand (x, VOIDmode))
	output_operand_lossage ("invalid %%m value");

      val = INT_LOWPART (x);

      /* If the low bit is set and the high bit is not, the value is 31.
	 If the low bit is zero, the value is the first 1 bit we find from
	 the right.  */
      if ((val & 1) && val >= 0)
	{
	  fputs ("31", file);
	  return;
	}
      else if ((val & 1) == 0)
	{
	  for (i = 0; i < 32; i++)
	    if ((val >>= 1) & 1)
	      break;

	  /* If we had ....10, I would be 0.  The result should be
	     30, so we need 30 - i.  */
	  fprintf (file, "%d", 30 - i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the left.  The result is its
	 number minus 1. We know the high-order bit is one.  */
      for (i = 0; i < 32; i++)
	if ((val <<= 1) >= 0)
	  break;

      fprintf (file, "%d", i);
      return;

    case 'N':
      /* Write the number of elements in the vector times 4.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, "%d", XVECLEN (x, 0) * 4);
      return;

    case 'O':
      /* Similar, but subtract 1 first.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, "%d", (XVECLEN (x, 0) - 1) * 4);
      return;

    case 'p':
      /* X is a CONST_INT that is a power of two.  Output the logarithm.  */
      if (! INT_P (x)
	  || (i = exact_log2 (INT_LOWPART (x))) < 0)
	output_operand_lossage ("invalid %%p value");

      fprintf (file, "%d", i);
      return;

    case 'P':
      /* The operand must be an indirect memory reference.  The result
	 is the register number. */
      if (GET_CODE (x) != MEM || GET_CODE (XEXP (x, 0)) != REG
	  || REGNO (XEXP (x, 0)) >= 32)
	output_operand_lossage ("invalid %%P value");

      fprintf (file, "%d", REGNO (XEXP (x, 0)));
      return;

    case 'R':
      /* X is a CR register.  Print the mask for `mtcrf'.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%R value");
      else
	fprintf (file, "%d", 128 >> (REGNO (x) - 68));
      return;

    case 's':
      /* Low 5 bits of 32 - value */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%s value");

      fprintf (file, "%d", (32 - INT_LOWPART (x)) & 31);
      return;

    case 't':
      /* Write 12 if this jump operation will branch if true, 4 otherwise. 
	 All floating-point operations except NE branch true and integer
	 EQ, LT, GT, LTU and GTU also branch true.  */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%t value");

      else if ((GET_MODE (XEXP (x, 0)) == CCFPmode
		&& GET_CODE (x) != NE)
	       || GET_CODE (x) == EQ
	       || GET_CODE (x) == LT || GET_CODE (x) == GT
	       || GET_CODE (x) == LTU || GET_CODE (x) == GTU)
	fputs ("12", file);
      else
	putc ('4', file);
      return;
      
    case 'T':
      /* Opposite of 't': write 4 if this jump operation will branch if true,
	 12 otherwise.   */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%t value");

      else if ((GET_MODE (XEXP (x, 0)) == CCFPmode
		&& GET_CODE (x) != NE)
	       || GET_CODE (x) == EQ
	       || GET_CODE (x) == LT || GET_CODE (x) == GT
	       || GET_CODE (x) == LTU || GET_CODE (x) == GTU)
	putc ('4', file);
      else
	fputs ("12", file);
      return;
      
    case 'u':
      /* High-order 16 bits of constant.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%u value");

      fprintf (file, "0x%x", (INT_LOWPART (x) >> 16) & 0xffff);
      return;

    case 'U':
      /* Print `u' if this has an auto-increment or auto-decrement.  */
      if (GET_CODE (x) == MEM
	  && (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC))
	putc ('u', file);
      return;

    case 'w':
      /* If constant, low-order 16 bits of constant, signed.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, "%d",
		 (INT_LOWPART (x) & 0xffff) - 2 * (INT_LOWPART (x) & 0x8000));
      else
	print_operand (file, x, 0);
      return;

    case 'W':
      /* If constant, low-order 16 bits of constant, unsigned.
	 Otherwise, write normally.  */
      if (INT_P (x))
	fprintf (file, "%d", INT_LOWPART (x) & 0xffff);
      else
	print_operand (file, x, 0);
      return;

    case 'X':
      if (GET_CODE (x) == MEM
	  && LEGITIMATE_INDEXED_ADDRESS_P (XEXP (x, 0)))
	putc ('x', file);
      return;

    case 'Y':
      /* Like 'L', for third word of TImode  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%d", REGNO (x) + 2);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 8));
	  else
	    output_address (plus_constant (XEXP (x, 0), 8));
	}
      return;
			    
    case 'z':
      /* X is a SYMBOL_REF.  Write out the name preceded by a
	 period and without any trailing data in brackets.  Used for function
	 names.  If we are configured for System V (or the embedded ABI) on
	 the PowerPC, do not emit the period, since those systems do not use
	 TOCs and the like.  */
      if (GET_CODE (x) != SYMBOL_REF)
	abort ();

#ifndef USING_SVR4_H
      putc ('.', file);
#endif
      RS6000_OUTPUT_BASENAME (file, XSTR (x, 0));
      return;

    case 'Z':
      /* Like 'L', for last word of TImode.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%d", REGNO (x) + 3);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 12));
	  else
	    output_address (plus_constant (XEXP (x, 0), 12));
	}
      return;
			    
    case 0:
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (GET_CODE (x) == MEM)
	{
	  /* We need to handle PRE_INC and PRE_DEC here, since we need to
	     know the width from the mode.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC)
	    fprintf (file, "%d(%d)", GET_MODE_SIZE (GET_MODE (x)),
		     REGNO (XEXP (XEXP (x, 0), 0)));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fprintf (file, "%d(%d)", - GET_MODE_SIZE (GET_MODE (x)),
		     REGNO (XEXP (XEXP (x, 0), 0)));
	  else
	    output_address (XEXP (x, 0));
	}
      else
	output_addr_const (file, x);
      return;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* Print the address of an operand.  */

void
print_operand_address (file, x)
     FILE *file;
     register rtx x;
{
  if (GET_CODE (x) == REG)
    fprintf (file, "0(%s)", reg_names[ REGNO (x) ]);
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST)
    {
      output_addr_const (file, x);
      /* When TARGET_MINIMAL_TOC, use the indirected toc table pointer instead
	 of the toc pointer.  */
#ifdef TARGET_NO_TOC
      if (TARGET_NO_TOC)
	;
      else
#endif
	fprintf (file, "(%s)", reg_names[ TARGET_MINIMAL_TOC ? 30 : 2 ]);
    }
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == REG)
    {
      if (REGNO (XEXP (x, 0)) == 0)
	fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (x, 1)) ],
		 reg_names[ REGNO (XEXP (x, 0)) ]);
      else
	fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (x, 0)) ],
		 reg_names[ REGNO (XEXP (x, 1)) ]);
    }
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
    fprintf (file, "%d(%s)", INTVAL (XEXP (x, 1)), reg_names[ REGNO (XEXP (x, 0)) ]);
  else if (TARGET_ELF && !TARGET_64BIT && GET_CODE (x) == LO_SUM
	   && GET_CODE (XEXP (x, 0)) == REG && CONSTANT_P (XEXP (x, 1)))
    {
      output_addr_const (file, XEXP (x, 1));
      fprintf (file, "@l(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
  else
    abort ();
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/*  Return the first fixed-point register that is required to be saved. 32 if
    none.  */

int
first_reg_to_save ()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 13; first_reg <= 31; first_reg++)
    if (regs_ever_live[first_reg])
      break;

  /* If profiling, then we must save/restore every register that contains
     a parameter before/after the .mcount call.  Use registers from 30 down
     to 23 to do this.  Don't use the frame pointer in reg 31.

     For now, save enough room for all of the parameter registers.  */
#ifndef USING_SVR4_H
  if (profile_flag)
    if (first_reg > 23)
      first_reg = 23;
#endif

  return first_reg;
}

/* Similar, for FP regs.  */

int
first_fp_reg_to_save ()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 14 + 32; first_reg <= 63; first_reg++)
    if (regs_ever_live[first_reg])
      break;

  return first_reg;
}

/* Return non-zero if this function makes calls.  */

int
rs6000_makes_calls ()
{
  rtx insn;

  /* If we are profiling, we will be making a call to mcount.  */
  if (profile_flag)
    return 1;

  for (insn = get_insns (); insn; insn = next_insn (insn))
    if (GET_CODE (insn) == CALL_INSN)
      return 1;

  return 0;
}


/* Calculate the stack information for the current function.  This is
   complicated by having two separate calling sequences, the AIX calling
   sequence and the V.4 calling sequence.

   AIX stack frames look like:

	SP---->	+---------------------------------------+
		| back chain to caller			| 0
		+---------------------------------------+
		| saved CR				| 4
		+---------------------------------------+
		| saved LR				| 8
		+---------------------------------------+
		| reserved for compilers		| 12
		+---------------------------------------+
		| reserved for binders			| 16
		+---------------------------------------+
		| saved TOC pointer			| 20
		+---------------------------------------+
		| Parameter save area (P)		| 24
		+---------------------------------------+
		| Alloca space (A)			| 24+P
		+---------------------------------------+
		| Local variable space (L)		| 24+P+A
		+---------------------------------------+
		| Save area for GP registers (G)	| 24+P+A+L
		+---------------------------------------+
		| Save area for FP registers (F)	| 24+P+A+L+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   V.4 stack frames look like:

	SP---->	+---------------------------------------+
		| back chain to caller			| 0
		+---------------------------------------+
		| caller's saved LR			| 4
		+---------------------------------------+
		| Parameter save area (P)		| 8
		+---------------------------------------+
		| Alloca space (A)			| 8+P
		+---------------------------------------+
		| Varargs save area (V)			| 8+P+A
		+---------------------------------------+
		| Local variable space (L)		| 8+P+A+V
		+---------------------------------------+
		| saved CR (C)				| 8+P+A+V+L
		+---------------------------------------+
		| Save area for GP registers (G)	| 8+P+A+V+L+C
		+---------------------------------------+
		| Save area for FP registers (F)	| 8+P+A+V+L+C+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+
*/

rs6000_stack_t *
rs6000_stack_info ()
{
  static rs6000_stack_t info, zero_info;
  rs6000_stack_t *info_ptr = &info;
  int reg_size = TARGET_64BIT ? 8 : 4;
  enum rs6000_abi abi;

  /* Zero all fields portably */
  info = zero_info;

  /* Select which calling sequence */
#ifdef TARGET_V4_CALLS
  if (TARGET_V4_CALLS)
    abi = ABI_V4;
  else
#endif
    abi = ABI_AIX;

  info_ptr->abi = abi;

  /* Calculate which registers need to be saved & save area size */
  info_ptr->first_gp_reg_save = first_reg_to_save ();
  info_ptr->gp_size = reg_size * (32 - info_ptr->first_gp_reg_save);

  info_ptr->first_fp_reg_save = first_fp_reg_to_save ();
  info_ptr->fp_size = 8 * (64 - info_ptr->first_fp_reg_save);

  /* Does this function call anything? */
  info_ptr->calls_p = rs6000_makes_calls ();

  /* Determine if we need to save the link register */
  if (regs_ever_live[65] || profile_flag
#ifdef TARGET_RELOCATABLE
      || (TARGET_RELOCATABLE && (get_pool_size () != 0))
#endif
      || (info_ptr->first_fp_reg_save != 64
	  && !FP_SAVE_INLINE (info_ptr->first_fp_reg_save))
      || (abi == ABI_V4 && current_function_calls_alloca)
      || info_ptr->calls_p)
    {
      info_ptr->lr_save_p = 1;
      regs_ever_live[65] = 1;
    }

  /* Determine if we need to save the condition code registers */
  if (regs_ever_live[70] || regs_ever_live[71] || regs_ever_live[72])
    {
      info_ptr->cr_save_p = 1;
      if (abi == ABI_V4)
	info_ptr->cr_size = reg_size;
    }

  /* Determine various sizes */
  info_ptr->reg_size     = reg_size;
  info_ptr->fixed_size   = RS6000_SAVE_AREA;
  info_ptr->varargs_size = RS6000_VARARGS_AREA;
  info_ptr->vars_size    = ALIGN (get_frame_size (), 8);
  info_ptr->parm_size    = ALIGN (current_function_outgoing_args_size, 8);
  info_ptr->save_size    = ALIGN (info_ptr->fp_size + info_ptr->gp_size + info_ptr->cr_size, 8);
  info_ptr->total_size   = ALIGN (info_ptr->vars_size
				  + info_ptr->parm_size
				  + info_ptr->save_size
				  + info_ptr->varargs_size
				  + info_ptr->fixed_size, STACK_BOUNDARY / BITS_PER_UNIT);

  /* Determine if we need to allocate any stack frame.
     For AIX We need to push the stack if a frame pointer is needed (because
     the stack might be dynamically adjusted), if we are debugging, if the
     total stack size is more than 220 bytes, or if we make calls.

     For V.4 we don't have the stack cushion that AIX uses, but assume that
     the debugger can handle stackless frames.  */

  if (info_ptr->calls_p)
    info_ptr->push_p = 1;

  else if (abi == ABI_V4)
    info_ptr->push_p = (info_ptr->total_size > info_ptr->fixed_size
			|| info_ptr->lr_save_p);

  else
    info_ptr->push_p = (frame_pointer_needed
			|| write_symbols != NO_DEBUG
			|| info_ptr->total_size > 220);

  /* Calculate the offsets */
  info_ptr->fp_save_offset = - info_ptr->fp_size;
  info_ptr->gp_save_offset = info_ptr->fp_save_offset - info_ptr->gp_size;
  switch (abi)
    {
    default:
      info_ptr->cr_save_offset = 4;
      info_ptr->lr_save_offset = 8;
      break;

    case ABI_V4:
      info_ptr->cr_save_offset = info_ptr->gp_save_offset - reg_size;
      info_ptr->lr_save_offset = reg_size;
      break;
    }

  /* Zero offsets if we're not saving those registers */
  if (!info_ptr->fp_size)
    info_ptr->fp_save_offset = 0;

  if (!info_ptr->gp_size)
    info_ptr->gp_save_offset = 0;

  if (!info_ptr->lr_save_p)
    info_ptr->lr_save_offset = 0;

  if (!info_ptr->cr_save_p)
    info_ptr->cr_save_offset = 0;

  return info_ptr;
}

void
debug_stack_info (info)
     rs6000_stack_t *info;
{
  char *abi_string;

  if (!info)
    info = rs6000_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  switch (info->abi)
    {
    default:	   abi_string = "Unknown";	break;
    case ABI_NONE: abi_string = "NONE";		break;
    case ABI_AIX:  abi_string = "AIX";		break;
    case ABI_V4:   abi_string = "V.4";		break;
    }

  fprintf (stderr, "\tABI                 = %5s\n", abi_string);

  if (info->first_gp_reg_save != 32)
    fprintf (stderr, "\tfirst_gp_reg_save   = %5d\n", info->first_gp_reg_save);

  if (info->first_fp_reg_save != 64)
    fprintf (stderr, "\tfirst_fp_reg_save   = %5d\n", info->first_fp_reg_save);

  if (info->lr_save_p)
    fprintf (stderr, "\tlr_save_p           = %5d\n", info->lr_save_p);

  if (info->cr_save_p)
    fprintf (stderr, "\tcr_save_p           = %5d\n", info->cr_save_p);

  if (info->push_p)
    fprintf (stderr, "\tpush_p              = %5d\n", info->push_p);

  if (info->calls_p)
    fprintf (stderr, "\tcalls_p             = %5d\n", info->calls_p);

  if (info->gp_save_offset)
    fprintf (stderr, "\tgp_save_offset      = %5d\n", info->gp_save_offset);

  if (info->fp_save_offset)
    fprintf (stderr, "\tfp_save_offset      = %5d\n", info->fp_save_offset);

  if (info->lr_save_offset)
    fprintf (stderr, "\tlr_save_offset      = %5d\n", info->lr_save_offset);

  if (info->cr_save_offset)
    fprintf (stderr, "\tcr_save_offset      = %5d\n", info->cr_save_offset);

  if (info->varargs_save_offset)
    fprintf (stderr, "\tvarargs_save_offset = %5d\n", info->varargs_save_offset);

  if (info->total_size)
    fprintf (stderr, "\ttotal_size          = %5d\n", info->total_size);

  if (info->varargs_size)
    fprintf (stderr, "\tvarargs_size        = %5d\n", info->varargs_size);

  if (info->vars_size)
    fprintf (stderr, "\tvars_size           = %5d\n", info->vars_size);

  if (info->parm_size)
    fprintf (stderr, "\tparm_size           = %5d\n", info->parm_size);

  if (info->fixed_size)
    fprintf (stderr, "\tfixed_size          = %5d\n", info->fixed_size);

  if (info->gp_size)
    fprintf (stderr, "\tgp_size             = %5d\n", info->gp_size);

  if (info->fp_size)
    fprintf (stderr, "\tfp_size             = %5d\n", info->fp_size);

  if (info->cr_size)
    fprintf (stderr, "\tcr_size             = %5d\n", info->cr_size);

  if (info->save_size)
    fprintf (stderr, "\tsave_size           = %5d\n", info->save_size);

  if (info->reg_size != 4)
    fprintf (stderr, "\treg_size            = %5d\n", info->reg_size);

  fprintf (stderr, "\n");
}



#ifdef USING_SVR4_H
/* Write out a System V.4 style traceback table before the prologue

   At present, only emit the basic tag table (ie, do not emit tag_types other
   than 0, which might use more than 1 tag word).

   The first tag word looks like:

    0			1		    2			3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |         0 |ver| tag |e|s| alloca  | # fprs  | # gprs  |s|l|c|f|
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

*/

void
svr4_traceback (file, name, decl)
     FILE *file;
     tree name, decl;
{
  rs6000_stack_t *info = rs6000_stack_info ();
  long tag;
  long version		= 0;				/* version number */
  long tag_type		= 0;				/* function type */
  long extended_tag	= 0;				/* additional tag words needed */
  long spare		= 0;				/* reserved for future use */
  long fpscr_max	= 0;				/* 1 if the function has a FPSCR save word */
  long fpr_max		= 64 - info->first_fp_reg_save;	/* # of floating point registers saved */
  long gpr_max		= 32 - info->first_gp_reg_save;	/* # of general purpose registers saved */
  long alloca_reg;					/* stack/frame register */

  if (frame_pointer_needed)
    alloca_reg = 31;

  else if (info->push_p != 0)
    alloca_reg = 1;

  else
    alloca_reg = 0;

  tag = ((version << 24)
	 | (tag_type << 21)
	 | (extended_tag << 20)
	 | (spare << 19)
	 | (alloca_reg << 14)
	 | (fpr_max << 9)
	 | (gpr_max << 4)
	 | (info->push_p << 3)
	 | (info->lr_save_p << 2)
	 | (info->cr_save_p << 1)
	 | (fpscr_max << 0));
	   
  fprintf (file, "\t.long 0x%lx\n", tag);
}

#endif /* USING_SVR4_H */

/* Write function prologue.  */
void
output_prolog (file, size)
     FILE *file;
     int size;
{
  rs6000_stack_t *info = rs6000_stack_info ();
  char *store_reg = (TARGET_64BIT) ? "\tstd %s,%d(%s)" : "\t{st|stw} %s,%d(%s)\n";

  if (TARGET_DEBUG_STACK)
    debug_stack_info (info);

  /* Write .extern for any function we will call to save and restore fp
     values.  */
#ifndef USING_SVR4_H
  if (info->first_fp_reg_save < 62)
    fprintf (file, "\t.extern %s%d%s\n\t.extern %s%d%s\n",
	     SAVE_FP_PREFIX, info->first_fp_reg_save - 32, SAVE_FP_SUFFIX,
	     RESTORE_FP_PREFIX, info->first_fp_reg_save - 32, RESTORE_FP_SUFFIX);
#endif

  /* Write .extern for truncation routines, if needed.  */
  if (rs6000_trunc_used && ! trunc_defined)
    {
      fprintf (file, "\t.extern .%s\n\t.extern .%s\n",
	       RS6000_ITRUNC, RS6000_UITRUNC);
      trunc_defined = 1;
    }

  /* Write .extern for AIX common mode routines, if needed.  */
  if (! TARGET_POWER && ! TARGET_POWERPC && ! common_mode_defined)
    {
      fputs ("\t.extern __mulh\n", file);
      fputs ("\t.extern __mull\n", file);
      fputs ("\t.extern __divss\n", file);
      fputs ("\t.extern __divus\n", file);
      fputs ("\t.extern __quoss\n", file);
      fputs ("\t.extern __quous\n", file);
      common_mode_defined = 1;
    }

  /* If we use the link register, get it into r0.  */
  if (info->lr_save_p)
    asm_fprintf (file, "\tmflr %s\n", reg_names[0]);

  /* If we need to save CR, put it into r12.  */
  if (info->cr_save_p)
    asm_fprintf (file, "\tmfcr %s\n", reg_names[12]);

  /* Do any required saving of fpr's.  If only one or two to save, do it
     ourself.  Otherwise, call function.  Note that since they are statically
     linked, we do not need a nop following them.  */
  if (FP_SAVE_INLINE (info->first_fp_reg_save))
    {
      int regno = info->first_fp_reg_save;
      int loc   = info->fp_save_offset;

      for ( ; regno < 64; regno++, loc += 8)
	asm_fprintf (file, "\tstfd %s,%d(%s)\n", reg_names[regno], loc, reg_names[1]);
    }
  else if (info->first_fp_reg_save != 64)
    asm_fprintf (file, "\tbl %s%d%s\n", SAVE_FP_PREFIX,
		 info->first_fp_reg_save - 32, SAVE_FP_SUFFIX);

  /* Now save gpr's.  */
  if (! TARGET_MULTIPLE || info->first_gp_reg_save == 31 || TARGET_64BIT)
    {
      int regno    = info->first_gp_reg_save;
      int loc      = info->gp_save_offset;
      int reg_size = (TARGET_64BIT) ? 8 : 4;

      for ( ; regno < 32; regno++, loc += reg_size)
	asm_fprintf (file, store_reg, reg_names[regno], loc, reg_names[1]);
    }

  else if (info->first_gp_reg_save != 32)
    asm_fprintf (file, "\t{stm|stmw} %s,%d(%s)\n",
		 reg_names[info->first_gp_reg_save],
		 info->gp_save_offset,
		 reg_names[1]);

  /* Save lr if we used it.  */
  if (info->lr_save_p)
    asm_fprintf (file, store_reg, reg_names[0], info->lr_save_offset, reg_names[1]);

  /* Save CR if we use any that must be preserved.  */
  if (info->cr_save_p)
    asm_fprintf (file, store_reg, reg_names[12], info->cr_save_offset, reg_names[1]);

  /* Update stack and set back pointer.  */
  if (info->push_p)
    {
      if (info->total_size < 32767)
	asm_fprintf (file,
		     (TARGET_64BIT) ? "\tstdu %s,%d(%s)\n" : "\t{stu|stwu} %s,%d(%s)\n",
		     reg_names[1], - info->total_size, reg_names[1]);
      else
	{
	  int neg_size = - info->total_size;
	  asm_fprintf (file, "\t{liu|lis} %s,%d\n\t{oril|ori} %s,%s,%d\n",
		       reg_names[0], (neg_size >> 16) & 0xffff,
		       reg_names[0], reg_names[0], neg_size & 0xffff);
	  asm_fprintf (file,
		       (TARGET_64BIT) ? "\tstdux %s,%s,%s\n" : "\t{stux|stwux} %s,%s,%s\n",
		       reg_names[1], reg_names[1], reg_names[0]);
	}
    }

  /* Set frame pointer, if needed.  */
  if (frame_pointer_needed)
    asm_fprintf (file, "\tmr %s,%s\n", reg_names[31], reg_names[1]);

  /* If TARGET_MINIMAL_TOC, and the constant pool is needed, then load the
     TOC_TABLE address into register 30.  */
  if (TARGET_TOC && TARGET_MINIMAL_TOC && get_pool_size () != 0)
    {
      char buf[256];

#ifdef USING_SVR4_H
      if (TARGET_RELOCATABLE)
	{
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  fprintf (file, "\tbl ");
	  assemble_name (file, buf);
	  fprintf (file, "\n");

	  ASM_OUTPUT_INTERNAL_LABEL (file, "LCF", rs6000_pic_labelno);
	  fprintf (file, "\tmflr %s\n", reg_names[30]);

	  if (TARGET_POWERPC64)
	    fprintf (file, "\tld");
	  else if (TARGET_NEW_MNEMONICS)
	    fprintf (file, "\tlwz");
	  else
	    fprintf (file, "\tl");

	  fprintf (file, " %s,(", reg_names[0]);
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCL", rs6000_pic_labelno);
	  assemble_name (file, buf);
	  fprintf (file, "-");
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  assemble_name (file, buf);
	  fprintf (file, ")(%s)\n", reg_names[30]);
	  asm_fprintf (file, "\t{cax|add} %s,%s,%s\n",
		       reg_names[30], reg_names[0], reg_names[30]);
	  rs6000_pic_labelno++;
	}
      else if (!TARGET_64BIT)
	{
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
	  asm_fprintf (file, "\t{cau|addis} %s,%s,", reg_names[30], reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@ha\n");
	  if (TARGET_NEW_MNEMONICS)
	    {
	      asm_fprintf (file, "\taddi %s,%s,", reg_names[30], reg_names[30]);
	      assemble_name (file, buf);
	      asm_fprintf (file, "@l\n");
	    }
	  else
	    {
	      asm_fprintf (file, "\tcal %s,", reg_names[30]);
	      assemble_name (file, buf);
	      asm_fprintf (file, "@l(%s)\n", reg_names[30]);
	    }
	}
      else
	abort ();

#else	/* !USING_SVR4_H */
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 0);
      asm_fprintf (file, "\t{l|lwz} %s,", reg_names[30]);
      assemble_name (file, buf);
      asm_fprintf (file, "(%s)\n", reg_names[2]);
#endif /* USING_SVR4_H */
    }
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  rs6000_stack_t *info = rs6000_stack_info ();
  char *load_reg = (TARGET_64BIT) ? "\tld %s,%d(%s)" : "\t{l|lwz} %s,%d(%s)\n";
  rtx insn = get_last_insn ();
  int i;

  /* Forget about any temporaries created */
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    stack_temps[i] = NULL_RTX;

  /* If the last insn was a BARRIER, we don't have to write anything except
     the trace table.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn == 0 ||  GET_CODE (insn) != BARRIER)
    {
      /* If we have a frame pointer, a call to alloca,  or a large stack
	 frame, restore the old stack pointer using the backchain.  Otherwise,
	 we know what size to update it with.  */
      if (frame_pointer_needed || current_function_calls_alloca
	  || info->total_size > 32767)
	asm_fprintf (file, load_reg, reg_names[1], 0, reg_names[1]);
      else if (info->push_p)
	{
	  if (TARGET_NEW_MNEMONICS)
	    asm_fprintf (file, "\taddi %s,%s,%d\n", reg_names[1], reg_names[1], info->total_size);
	  else
	    asm_fprintf (file, "\tcal %s,%d(%s)\n", reg_names[1], info->total_size, reg_names[1]);
	}

      /* Get the old lr if we saved it.  */
      if (info->lr_save_p)
	asm_fprintf (file, load_reg, reg_names[0], info->lr_save_offset, reg_names[1]);

      /* Get the old cr if we saved it.  */
      if (info->cr_save_p)
	asm_fprintf (file, load_reg, reg_names[12], info->cr_save_offset, reg_names[1]);

      /* Set LR here to try to overlap restores below.  */
      if (info->lr_save_p)
	asm_fprintf (file, "\tmtlr %s\n", reg_names[0]);

      /* Restore gpr's.  */
      if (! TARGET_MULTIPLE || info->first_gp_reg_save == 31 || TARGET_64BIT)
	{
	  int regno    = info->first_gp_reg_save;
	  int loc      = info->gp_save_offset;
	  int reg_size = (TARGET_64BIT) ? 8 : 4;

	  for ( ; regno < 32; regno++, loc += reg_size)
	    asm_fprintf (file, load_reg, reg_names[regno], loc, reg_names[1]);
	}

      else if (info->first_gp_reg_save != 32)
	asm_fprintf (file, "\t{lm|lmw} %s,%d(%s)\n",
		     reg_names[info->first_gp_reg_save],
		     info->gp_save_offset,
		     reg_names[1]);

      /* Restore fpr's if we can do it without calling a function.  */
      if (FP_SAVE_INLINE (info->first_fp_reg_save))
	{
	  int regno = info->first_fp_reg_save;
	  int loc   = info->fp_save_offset;

	  for ( ; regno < 64; regno++, loc += 8)
	    asm_fprintf (file, "\tlfd %s,%d(%s)\n", reg_names[regno], loc, reg_names[1]);
	}

      /* If we saved cr, restore it here.  Just those of cr2, cr3, and cr4
	 that were used.  */
      if (info->cr_save_p)
	asm_fprintf (file, "\tmtcrf %d,%s\n",
		     (regs_ever_live[70] != 0) * 0x20
		     + (regs_ever_live[71] != 0) * 0x10
		     + (regs_ever_live[72] != 0) * 0x8, reg_names[12]);

      /* If we have to restore more than two FP registers, branch to the
	 restore function.  It will return to our caller.  */
      if (info->first_fp_reg_save != 64 && !FP_SAVE_INLINE (info->first_fp_reg_save))
	asm_fprintf (file, "\tb %s%d%s\n", RESTORE_FP_PREFIX,
		     info->first_fp_reg_save - 32, RESTORE_FP_SUFFIX);
      else
	asm_fprintf (file, "\t{br|blr}\n");
    }

  /* Output a traceback table here.  See /usr/include/sys/debug.h for info
     on its format.

     We don't output a traceback table if -finhibit-size-directive was
     used.  The documentation for -finhibit-size-directive reads
     ``don't output a @code{.size} assembler directive, or anything
     else that would cause trouble if the function is split in the
     middle, and the two halves are placed at locations far apart in
     memory.''  The traceback table has this property, since it
     includes the offset from the start of the function to the
     traceback table itself.

     System V.4 Powerpc's (and the embedded ABI derived from it) use a
     different traceback table located before the prologue.  */
#ifndef USING_SVR4_H
  if (! flag_inhibit_size_directive)
    {
      char *fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
      int fixed_parms, float_parms, parm_info;
      int i;

      /* Need label immediately before tbtab, so we can compute its offset
	 from the function start.  */
      if (*fname == '*')
	++fname;
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
      ASM_OUTPUT_LABEL (file, fname);

      /* The .tbtab pseudo-op can only be used for the first eight
	 expressions, since it can't handle the possibly variable
	 length fields that follow.  However, if you omit the optional
	 fields, the assembler outputs zeros for all optional fields
	 anyways, giving each variable length field is minimum length
	 (as defined in sys/debug.h).  Thus we can not use the .tbtab
	 pseudo-op at all.  */

      /* An all-zero word flags the start of the tbtab, for debuggers
	 that have to find it by searching forward from the entry
	 point or from the current pc.  */
      fprintf (file, "\t.long 0\n");

      /* Tbtab format type.  Use format type 0.  */
      fprintf (file, "\t.byte 0,");

      /* Language type.  Unfortunately, there doesn't seem to be any
	 official way to get this info, so we use language_string.  C
	 is 0.  C++ is 9.  No number defined for Obj-C, so use the
	 value for C for now.  */
      if (! strcmp (language_string, "GNU C")
	  || ! strcmp (language_string, "GNU Obj-C"))
	i = 0;
      else if (! strcmp (language_string, "GNU F77"))
	i = 1;
      else if (! strcmp (language_string, "GNU Ada"))
	i = 3;
      else if (! strcmp (language_string, "GNU PASCAL"))
	i = 2;
      else if (! strcmp (language_string, "GNU C++"))
	i = 9;
      else
	abort ();
      fprintf (file, "%d,", i);

      /* 8 single bit fields: global linkage (not set for C extern linkage,
	 apparently a PL/I convention?), out-of-line epilogue/prologue, offset
	 from start of procedure stored in tbtab, internal function, function
	 has controlled storage, function has no toc, function uses fp,
	 function logs/aborts fp operations.  */
      /* Assume that fp operations are used if any fp reg must be saved.  */
      fprintf (file, "%d,", (1 << 5) | ((info->first_fp_reg_save != 64) << 1));

      /* 6 bitfields: function is interrupt handler, name present in
	 proc table, function calls alloca, on condition directives
	 (controls stack walks, 3 bits), saves condition reg, saves
	 link reg.  */
      /* The `function calls alloca' bit seems to be set whenever reg 31 is
	 set up as a frame pointer, even when there is no alloca call.  */
      fprintf (file, "%d,",
	       ((1 << 6) | (frame_pointer_needed << 5)
		| (info->cr_save_p << 1) | (info->lr_save_p)));

      /* 3 bitfields: saves backchain, spare bit, number of fpr saved
	 (6 bits).  */
      fprintf (file, "%d,",
	       (info->push_p << 7) | (64 - info->first_fp_reg_save));

      /* 2 bitfields: spare bits (2 bits), number of gpr saved (6 bits).  */
      fprintf (file, "%d,", (32 - first_reg_to_save ()));

      {
	/* Compute the parameter info from the function decl argument
	   list.  */
	tree decl;
	int next_parm_info_bit;

	next_parm_info_bit = 31;
	parm_info = 0;
	fixed_parms = 0;
	float_parms = 0;

	for (decl = DECL_ARGUMENTS (current_function_decl);
	     decl; decl = TREE_CHAIN (decl))
	  {
	    rtx parameter = DECL_INCOMING_RTL (decl);
	    enum machine_mode mode = GET_MODE (parameter);

	    if (GET_CODE (parameter) == REG)
	      {
		if (GET_MODE_CLASS (mode) == MODE_FLOAT)
		  {
		    int bits;

		    float_parms++;

		    if (mode == SFmode)
		      bits = 0x2;
		    else if (mode == DFmode)
		      bits = 0x3;
		    else
		      abort ();

		    /* If only one bit will fit, don't or in this entry.  */
		    if (next_parm_info_bit > 0)
		      parm_info |= (bits << (next_parm_info_bit - 1));
		    next_parm_info_bit -= 2;
		  }
		else
		  {
		    fixed_parms += ((GET_MODE_SIZE (mode)
				     + (UNITS_PER_WORD - 1))
				    / UNITS_PER_WORD);
		    next_parm_info_bit -= 1;
		  }
	      }
	  }
      }

      /* Number of fixed point parameters.  */
      /* This is actually the number of words of fixed point parameters; thus
	 an 8 byte struct counts as 2; and thus the maximum value is 8.  */
      fprintf (file, "%d,", fixed_parms);

      /* 2 bitfields: number of floating point parameters (7 bits), parameters
	 all on stack.  */
      /* This is actually the number of fp registers that hold parameters;
	 and thus the maximum value is 13.  */
      /* Set parameters on stack bit if parameters are not in their original
	 registers, regardless of whether they are on the stack?  Xlc
	 seems to set the bit when not optimizing.  */
      fprintf (file, "%d\n", ((float_parms << 1) | (! optimize)));

      /* Optional fields follow.  Some are variable length.  */

      /* Parameter types, left adjusted bit fields: 0 fixed, 10 single float,
	 11 double float.  */
      /* There is an entry for each parameter in a register, in the order that
	 they occur in the parameter list.  Any intervening arguments on the
	 stack are ignored.  If the list overflows a long (max possible length
	 34 bits) then completely leave off all elements that don't fit.  */
      /* Only emit this long if there was at least one parameter.  */
      if (fixed_parms || float_parms)
	fprintf (file, "\t.long %d\n", parm_info);

      /* Offset from start of code to tb table.  */
      fprintf (file, "\t.long ");
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
      RS6000_OUTPUT_BASENAME (file, fname);
      fprintf (file, "-.");
      RS6000_OUTPUT_BASENAME (file, fname);
      fprintf (file, "\n");

      /* Interrupt handler mask.  */
      /* Omit this long, since we never set the interrupt handler bit
	 above.  */

      /* Number of CTL (controlled storage) anchors.  */
      /* Omit this long, since the has_ctl bit is never set above.  */

      /* Displacement into stack of each CTL anchor.  */
      /* Omit this list of longs, because there are no CTL anchors.  */

      /* Length of function name.  */
      fprintf (file, "\t.short %d\n", strlen (fname));

      /* Function name.  */
      assemble_string (fname, strlen (fname));

      /* Register for alloca automatic storage; this is always reg 31.
	 Only emit this if the alloca bit was set above.  */
      if (frame_pointer_needed)
	fprintf (file, "\t.byte 31\n");
    }
#endif /* !USING_SVR4_H */

  /* Reset varargs indicator */
  rs6000_sysv_varargs_p = 0;
}

/* Output a TOC entry.  We derive the entry name from what is
   being written.  */

void
output_toc (file, x, labelno)
     FILE *file;
     rtx x;
     int labelno;
{
  char buf[256];
  char *name = buf;
  rtx base = x;
  int offset = 0;

  if (TARGET_NO_TOC)
    abort ();

  /* if we're going to put a double constant in the TOC, make sure it's
     aligned properly when strict alignment is on. */
  if (GET_CODE (x) == CONST_DOUBLE
      && STRICT_ALIGNMENT
      && GET_MODE (x) == DFmode
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)) {
    ASM_OUTPUT_ALIGN (file, 3);
  }


#ifdef USING_SVR4_H
  if (TARGET_MINIMAL_TOC)
    {
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
      fprintf (file, "%d = .-", labelno);
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LCTOC");
      fprintf (file, "1\n");
    }
  else
#endif /* USING_SVR4_H */
    ASM_OUTPUT_INTERNAL_LABEL (file, "LC", labelno);

  /* Handle FP constants specially.  Note that if we have a minimal
     TOC, things we put here aren't actually in the TOC, so we can allow
     FP constants.  */
  if (GET_CODE (x) == CONST_DOUBLE
      && GET_MODE (x) == DFmode
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC))
    {
      REAL_VALUE_TYPE r;
      long l[2];

      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_TARGET_DOUBLE (r, l);
      if (TARGET_MINIMAL_TOC)
	fprintf (file, "\t.long %ld\n\t.long %ld\n", l[0], l[1]);
      else
	fprintf (file, "\t.tc FD_%lx_%lx[TC],%ld,%ld\n",
		 l[0], l[1], l[0], l[1]);
      return;
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode
	   && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC))
    {
      rtx val = operand_subword (x, 0, 0, SFmode);

      if (val == 0 || GET_CODE (val) != CONST_INT)
	abort ();

      if (TARGET_MINIMAL_TOC)
	fprintf (file, "\t.long %d\n", INTVAL (val));
      else
	fprintf (file, "\t.tc FS_%x[TC],%d\n", INTVAL (val), INTVAL (val));
      return;
    }

  if (GET_CODE (x) == CONST)
    {
      base = XEXP (XEXP (x, 0), 0);
      offset = INTVAL (XEXP (XEXP (x, 0), 1));
    }
  
  if (GET_CODE (base) == SYMBOL_REF)
    name = XSTR (base, 0);
  else if (GET_CODE (base) == LABEL_REF)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (base, 0)));
  else if (GET_CODE (base) == CODE_LABEL)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (base));
  else
    abort ();

  if (TARGET_MINIMAL_TOC)
    fprintf (file, "\t.long ");
  else
    {
      fprintf (file, "\t.tc ");
      RS6000_OUTPUT_BASENAME (file, name);

      if (offset < 0)
	fprintf (file, ".N%d", - offset);
      else if (offset)
	fprintf (file, ".P%d", offset);

      fprintf (file, "[TC],");
    }
  output_addr_const (file, x);
  fprintf (file, "\n");
}

/* Output an assembler pseudo-op to write an ASCII string of N characters
   starting at P to FILE.

   On the RS/6000, we have to do this using the .byte operation and
   write out special characters outside the quoted string.
   Also, the assembler is broken; very long strings are truncated,
   so we must artificially break them up early. */

void
output_ascii (file, p, n)
     FILE *file;
     char *p;
     int n;
{
  char c;
  int i, count_string;
  char *for_string = "\t.byte \"";
  char *for_decimal = "\t.byte ";
  char *to_close = NULL;

  count_string = 0;
  for (i = 0; i < n; i++)
    {
      c = *p++;
      if (c >= ' ' && c < 0177)
	{
	  if (for_string)
	    fputs (for_string, file);
	  putc (c, file);

	  /* Write two quotes to get one.  */
	  if (c == '"')
	    {
	      putc (c, file);
	      ++count_string;
	    }

	  for_string = NULL;
	  for_decimal = "\"\n\t.byte ";
	  to_close = "\"\n";
	  ++count_string;

	  if (count_string >= 512)
	    {
	      fputs (to_close, file);

	      for_string = "\t.byte \"";
	      for_decimal = "\t.byte ";
	      to_close = NULL;
	      count_string = 0;
	    }
	}
      else
	{
	  if (for_decimal)
	    fputs (for_decimal, file);
	  fprintf (file, "%d", c);

	  for_string = "\n\t.byte \"";
	  for_decimal = ", ";
	  to_close = "\n";
	  count_string = 0;
	}
    }

  /* Now close the string if we have written one.  Then end the line.  */
  if (to_close)
    fprintf (file, to_close);
}

/* Generate a unique section name for FILENAME for a section type
   represented by SECTION_DESC.  Output goes into BUF.

   SECTION_DESC can be any string, as long as it is different for each
   possible section type.

   We name the section in the same manner as xlc.  The name begins with an
   underscore followed by the filename (after stripping any leading directory
   names) with the last period replaced by the string SECTION_DESC.  If
   FILENAME does not contain a period, SECTION_DESC is appended to the end of
   the name.  */

void
rs6000_gen_section_name (buf, filename, section_desc)
     char **buf;
     char *filename;
     char *section_desc;
{
  char *q, *after_last_slash, *last_period;
  char *p;
  int len;

  after_last_slash = filename;
  for (q = filename; *q; q++)
    {
      if (*q == '/')
	after_last_slash = q + 1;
      else if (*q == '.')
	last_period = q;
    }

  len = strlen (after_last_slash) + strlen (section_desc) + 2;
  *buf = (char *) permalloc (len);

  p = *buf;
  *p++ = '_';

  for (q = after_last_slash; *q; q++)
    {
      if (q == last_period)
        {
	  strcpy (p, section_desc);
	  p += strlen (section_desc);
        }

      else if (isalnum (*q))
        *p++ = *q;
    }

  if (last_period == 0)
    strcpy (p, section_desc);
  else
    *p = '\0';
}

/* Write function profiler code. */

void
output_function_profiler (file, labelno)
  FILE *file;
  int labelno;
{
#ifdef USING_SVR4_H
  abort ();
#else
  /* The last used parameter register.  */
  int last_parm_reg;
  int i, j;
  char buf[100];

  /* Set up a TOC entry for the profiler label.  */
  toc_section ();
  ASM_OUTPUT_INTERNAL_LABEL (file, "LPC", labelno);
  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
  if (TARGET_MINIMAL_TOC)
    {
      fprintf (file, "\t.long ");
      assemble_name (file, buf);
      fprintf (file, "\n");
    }
  else
    {
      fprintf (file, "\t.tc\t");
      assemble_name (file, buf);
      fprintf (file, "[TC],");
      assemble_name (file, buf);
      fprintf (file, "\n");
    }
  text_section ();

  /* Figure out last used parameter register.  The proper thing to do is
     to walk incoming args of the function.  A function might have live
     parameter registers even if it has no incoming args.  */

  for (last_parm_reg = 10;
       last_parm_reg > 2 && ! regs_ever_live [last_parm_reg];
       last_parm_reg--)
    ;

  /* Save parameter registers in regs 23-30.  Don't overwrite reg 31, since
     it might be set up as the frame pointer.  */

  for (i = 3, j = 30; i <= last_parm_reg; i++, j--)
    fprintf (file, "\tai %d,%d,0\n", j, i);

  /* Load location address into r3, and call mcount.  */

  ASM_GENERATE_INTERNAL_LABEL (buf, "LPC", labelno);
  fprintf (file, "\tl 3,");
  assemble_name (file, buf);
  fprintf (file, "(2)\n\tbl .mcount\n");

  /* Restore parameter registers.  */

  for (i = 3, j = 30; i <= last_parm_reg; i++, j--)
    fprintf (file, "\tai %d,%d,0\n", i, j);
#endif
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
rs6000_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  if (! recog_memoized (insn))
    return 0;

  if (REG_NOTE_KIND (link) != 0)
    return 0;

  if (REG_NOTE_KIND (link) == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      /* Tell the first scheduling pass about the latency between a mtctr
	 and bctr (and mtlr and br/blr).  The first scheduling pass will not
	 know about this latency since the mtctr instruction, which has the
	 latency associated to it, will be generated by reload.  */
      if (get_attr_type (insn) == TYPE_JMPREG)
	return TARGET_POWER ? 5 : 4;

      /* Fall out to return default cost.  */
    }

  return cost;
}
