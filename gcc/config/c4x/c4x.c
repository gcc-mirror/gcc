/* Subroutines for assembler code output on the TMS320C[34]x
   Copyright (C) 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

   Contributed by Michael Hayes (m.hayes@elec.canterbury.ac.nz)
              and Herman Ten Brugge (Haj.Ten.Brugge@net.HCC.nl).

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

/* Some output-actions in c4x.md need these.  */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "config.h"
#include "toplev.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "real.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "tree.h"
#include "expr.h"
#include "flags.h"
#include "loop.h"
#include "recog.h"
#include "c-tree.h"

extern void iteration_info ();	/* in unroll.c */

static int c4x_leaf_function;

static char *float_reg_names[] = FLOAT_REGISTER_NAMES;

/* Array of the smallest class containing reg number REGNO, indexed by
   REGNO.  Used by REGNO_REG_CLASS in c4x.h.  We assume that all these
   registers are available and set the class to NO_REGS for registers 
   that the target switches say are unavailable.  */

enum reg_class c4x_regclass_map[FIRST_PSEUDO_REGISTER] =
{
                                /* Reg          Modes           Saved  */
  R0R1_REGS,			/* R0           QI, QF, HF      No  */
  R0R1_REGS,			/* R1           QI, QF, HF      No  */
  R2R3_REGS,			/* R2           QI, QF, HF      No  */
  R2R3_REGS,			/* R3           QI, QF, HF      No  */
  EXT_LOW_REGS,			/* R4           QI, QF, HF      QI  */
  EXT_LOW_REGS,			/* R5           QI, QF, HF      QI  */
  EXT_LOW_REGS,			/* R6           QI, QF, HF      QF  */
  EXT_LOW_REGS,			/* R7           QI, QF, HF      QF  */
  ADDR_REGS,			/* AR0          QI              No  */
  ADDR_REGS,			/* AR1          QI              No  */
  ADDR_REGS,			/* AR2          QI              No  */
  ADDR_REGS,			/* AR3          QI              QI  */
  ADDR_REGS,			/* AR4          QI              QI  */
  ADDR_REGS,			/* AR5          QI              QI  */
  ADDR_REGS,			/* AR6          QI              QI  */
  ADDR_REGS,			/* AR7          QI              QI  */
  DP_REG,			/* DP           QI              No  */
  INDEX_REGS,			/* IR0          QI              No  */
  INDEX_REGS,			/* IR1          QI              No  */
  BK_REG,			/* BK           QI              QI  */
  SP_REG,			/* SP           QI              No  */
  ST_REG,			/* ST           CC              No  */
  NO_REGS,			/* DIE/IE                       No  */
  NO_REGS,			/* IIE/IF                       No  */
  NO_REGS,			/* IIF/IOF                      No  */
  INT_REGS,			/* RS           QI              No  */
  INT_REGS,			/* RE           QI              No  */
  RC_REG,			/* RC           QI              No  */
  EXT_REGS,			/* R8           QI, QF, HF      QI  */
  EXT_REGS,			/* R9           QI, QF, HF      No  */
  EXT_REGS,			/* R10          QI, QF, HF      No  */
  EXT_REGS,			/* R11          QI, QF, HF      No  */
};

enum machine_mode c4x_caller_save_map[FIRST_PSEUDO_REGISTER] =
{
                                /* Reg          Modes           Saved  */
  HFmode,			/* R0           QI, QF, HF      No  */
  HFmode,			/* R1           QI, QF, HF      No  */
  HFmode,			/* R2           QI, QF, HF      No  */
  HFmode,			/* R3           QI, QF, HF      No  */
  QFmode,			/* R4           QI, QF, HF      QI  */
  QFmode,			/* R5           QI, QF, HF      QI  */
  QImode,			/* R6           QI, QF, HF      QF  */
  QImode,			/* R7           QI, QF, HF      QF  */
  QImode,			/* AR0          QI              No  */
  QImode,			/* AR1          QI              No  */
  QImode,			/* AR2          QI              No  */
  QImode,			/* AR3          QI              QI  */
  QImode,			/* AR4          QI              QI  */
  QImode,			/* AR5          QI              QI  */
  QImode,			/* AR6          QI              QI  */
  QImode,			/* AR7          QI              QI  */
  VOIDmode,			/* DP           QI              No  */
  QImode,			/* IR0          QI              No  */
  QImode,			/* IR1          QI              No  */
  QImode,			/* BK           QI              QI  */
  VOIDmode,			/* SP           QI              No  */
  VOIDmode,			/* ST           CC              No  */
  VOIDmode,			/* DIE/IE                       No  */
  VOIDmode,			/* IIE/IF                       No  */
  VOIDmode,			/* IIF/IOF                      No  */
  QImode,			/* RS           QI              No  */
  QImode,			/* RE           QI              No  */
  QImode,			/* RC           QI              No  */
  QFmode,			/* R8           QI, QF, HF      QI  */
  HFmode,			/* R9           QI, QF, HF      No  */
  HFmode,			/* R10          QI, QF, HF      No  */
  HFmode,			/* R11          QI, QF, HF      No  */
};


/* Test and compare insns in c4x.md store the information needed to
   generate branch and scc insns here.  */

struct rtx_def *c4x_compare_op0 = NULL_RTX;
struct rtx_def *c4x_compare_op1 = NULL_RTX;

char *c4x_rpts_cycles_string;
int c4x_rpts_cycles = 0;	/* Max. cycles for RPTS */
char *c4x_cpu_version_string;
int c4x_cpu_version = 40;	/* CPU version C30/31/32/40/44 */

/* Pragma definitions.  */

tree code_tree = NULL_TREE;
tree data_tree = NULL_TREE;
tree pure_tree = NULL_TREE;
tree noreturn_tree = NULL_TREE;
tree interrupt_tree = NULL_TREE;


/* Override command line options.
   Called once after all options have been parsed.
   Mostly we process the processor
   type and sometimes adjust other TARGET_ options.  */

void
c4x_override_options ()
{
  /* Convert foo / 8.0 into foo * 0.125, etc.  */
  flag_fast_math = 1;

  if (c4x_rpts_cycles_string)
    c4x_rpts_cycles = atoi (c4x_rpts_cycles_string);
  else
    c4x_rpts_cycles = 0;

  if (TARGET_C30)
    c4x_cpu_version = 30;
  else if (TARGET_C31)
    c4x_cpu_version = 31;
  else if (TARGET_C32)
    c4x_cpu_version = 32;
  else if (TARGET_C40)
    c4x_cpu_version = 40;
  else if (TARGET_C44)
    c4x_cpu_version = 44;
  else
    c4x_cpu_version = 40;	       

  /* -mcpu=xx overrides -m40 etc.  */
  if (c4x_cpu_version_string)
    c4x_cpu_version = atoi (c4x_cpu_version_string);

  target_flags &= ~(C30_FLAG | C31_FLAG | C32_FLAG | C40_FLAG | C44_FLAG);

  switch (c4x_cpu_version)
    {
    case 30: target_flags |= C30_FLAG; break;
    case 31: target_flags |= C31_FLAG; break;
    case 32: target_flags |= C32_FLAG; break;
    case 40: target_flags |= C40_FLAG; break;
    case 44: target_flags |= C44_FLAG; break;
    default:
      warning ("Unknown CPU version %d, using 40.\n", c4x_cpu_version);
      c4x_cpu_version = 40;
      target_flags |= C40_FLAG;
    }

  if (TARGET_C30 || TARGET_C31 || TARGET_C32)
    target_flags |= C3X_FLAG;
  else
    target_flags &= ~C3X_FLAG;

}

void
c4x_optimization_options (level, size)
     int level;
     int size ATTRIBUTE_UNUSED;
{
  /* When optimizing, enable use of RPTB instruction.  */
  if (level >= 1)
      flag_branch_on_count_reg = 1;
}

/* Write an ASCII string.  */

#define C4X_ASCII_LIMIT 40

void
c4x_output_ascii (stream, ptr, len)
     FILE *stream;
     unsigned char *ptr;
     int len;
{
  char sbuf[C4X_ASCII_LIMIT + 1];
  int s, first, onlys;

  if (len)
    {
      fprintf (stream, "\t.byte\t");
      first = 1;
    }

  for (s = 0; len > 0; --len, ++ptr)
    {
      onlys = 0;

      /* Escape " and \ with a \".  */
      if (*ptr == '\"' || *ptr == '\\')
	sbuf[s++] = '\\';

      /* If printable - add to buff.  */
      if (*ptr >= 0x20 && *ptr < 0x7f)
	{
	  sbuf[s++] = *ptr;
	  if (s < C4X_ASCII_LIMIT - 1)
	    continue;
	  onlys = 1;
	}
      if (s)
	{
	  if (first)
	    first = 0;
	  else
	    fputc (',', stream);

	  sbuf[s] = 0;
	  fprintf (stream, "\"%s\"", sbuf);
	  s = 0;
	}
      if (onlys)
	continue;

      if (first)
	first = 0;
      else
	fputc (',', stream);

      fprintf (stream, "%d", *ptr);
    }
  if (s)
    {
      if (!first)
	fputc (',', stream);

      sbuf[s] = 0;
      fprintf (stream, "\"%s\"", sbuf);
      s = 0;
    }
  fputc ('\n', stream);
}


int
c4x_hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  switch (mode)
    {
#if Pmode != QImode
    case Pmode:			/* Pointer (24/32 bits) */
#endif
    case QImode:		/* Integer (32 bits) */
      return IS_INT_REG (regno);

    case QFmode:		/* Float, Double (32 bits) */
    case HFmode:		/* Long Double (40 bits) */
      return IS_EXT_REG (regno);

    case CCmode:		/* Condition Codes */
    case CC_NOOVmode:		/* Condition Codes */
      return IS_ST_REG (regno);

    case HImode:		/* Long Long (64 bits) */
      /* We need two registers to store long longs.  Note that 
	 it is much easier to constrain the first register
	 to start on an even boundary.  */
      return IS_INT_REG (regno)
	&& IS_INT_REG (regno + 1)
	&& (regno & 1) == 0;

    default:
      return 0;			/* We don't support these modes */
    }

  return 0;
}


/* The TI C3x C compiler register argument runtime model uses 6 registers,
   AR2, R2, R3, RC, RS, RE.

   The first two floating point arguments (float, double, long double)
   that are found scanning from left to right are assigned to R2 and R3.

   The remaining integer (char, short, int, long) or pointer arguments
   are assigned to the remaining registers in the order AR2, R2, R3,
   RC, RS, RE when scanning left to right, except for the last named
   argument prior to an ellipsis denoting variable number of
   arguments.  We don't have to worry about the latter condition since
   function.c treats the last named argument as anonymous (unnamed).

   All arguments that cannot be passed in registers are pushed onto
   the stack in reverse order (right to left).  GCC handles that for us.

   c4x_init_cumulative_args() is called at the start, so we can parse
   the args to see how many floating point arguments and how many
   integer (or pointer) arguments there are.  c4x_function_arg() is
   then called (sometimes repeatedly) for each argument (parsed left
   to right) to obtain the register to pass the argument in, or zero
   if the argument is to be passed on the stack.  Once the compiler is
   happy, c4x_function_arg_advance() is called.

   Don't use R0 to pass arguments in, we use 0 to indicate a stack
   argument.  */

static int c4x_int_reglist[3][6] =
{
  {AR2_REGNO, R2_REGNO, R3_REGNO, RC_REGNO, RS_REGNO, RE_REGNO},
  {AR2_REGNO, R3_REGNO, RC_REGNO, RS_REGNO, RE_REGNO, 0},
  {AR2_REGNO, RC_REGNO, RS_REGNO, RE_REGNO, 0, 0}
};

static int c4x_fp_reglist[2] = {R2_REGNO, R3_REGNO};


/* Initialize a variable CUM of type CUMULATIVE_ARGS for a call to a
   function whose data type is FNTYPE.
   For a library call, FNTYPE is  0.  */

void
c4x_init_cumulative_args (cum, fntype, libname)
     CUMULATIVE_ARGS *cum;	/* argument info to initialize */
     tree fntype;		/* tree ptr for function decl */
     rtx libname;		/* SYMBOL_REF of library name or 0 */
{
  tree param, next_param;

  cum->floats = cum->ints = 0;
  cum->init = 0;
  cum->var = 0;
  cum->args = 0;

  if (TARGET_DEBUG)
    {
      fprintf (stderr, "\nc4x_init_cumulative_args (");
      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);

	  fprintf (stderr, "fntype code = %s, ret code = %s",
		   tree_code_name[(int) TREE_CODE (fntype)],
		   tree_code_name[(int) TREE_CODE (ret_type)]);
	}
      else
	fprintf (stderr, "no fntype");

      if (libname)
	fprintf (stderr, ", libname = %s", XSTR (libname, 0));
    }

  cum->prototype = (fntype && TYPE_ARG_TYPES (fntype));

  for (param = fntype ? TYPE_ARG_TYPES (fntype) : 0;
       param; param = next_param)
    {
      tree type;

      next_param = TREE_CHAIN (param);

      type = TREE_VALUE (param);
      if (type && type != void_type_node)
	{
	  enum machine_mode mode;

	  /* If the last arg doesn't have void type then we have
	     variable arguments.  */
	  if (!next_param)
	    cum->var = 1;

	  if ((mode = TYPE_MODE (type)))
	    {
	      if (!MUST_PASS_IN_STACK (mode, type))
		{
		  /* Look for float, double, or long double argument.  */
		  if (mode == QFmode || mode == HFmode)
		    cum->floats++;
		  /* Look for integer, enumeral, boolean, char, or pointer
		     argument.  */
		  else if (mode == QImode || mode == Pmode)
		    cum->ints++;
		}
	    }
	  cum->args++;
	}
    }

  if (TARGET_DEBUG)
    fprintf (stderr, "%s%s, args = %d)\n",
	     cum->prototype ? ", prototype" : "",
	     cum->var ? ", variable args" : "",
	     cum->args);
}


/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
c4x_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* whether or not the argument was named */
{
  if (TARGET_DEBUG)
    fprintf (stderr, "c4x_function_adv(mode=%s, named=%d)\n\n",
	     GET_MODE_NAME (mode), named);
  if (!TARGET_MEMPARM 
      && named
      && type
      && !MUST_PASS_IN_STACK (mode, type))
    {
      /* Look for float, double, or long double argument.  */
      if (mode == QFmode || mode == HFmode)
	cum->floats++;
      /* Look for integer, enumeral, boolean, char, or pointer argument.  */
      else if (mode == QImode || mode == Pmode)
	cum->ints++;
    }
  else if (!TARGET_MEMPARM && !type)
    {
      /* Handle libcall arguments.  */
      if (mode == QFmode || mode == HFmode)
	cum->floats++;
      else if (mode == QImode || mode == Pmode)
	cum->ints++;
    }
  return;
}


/* Define where to put the arguments to a function.  Value is zero to
   push the argument on the stack, or a hard register in which to
   store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
   This is null for libcalls where that information may
   not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
   the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
   (otherwise it is an extra parameter matching an ellipsis).  */

struct rtx_def *
c4x_function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  int reg = 0;			/* default to passing argument on stack */

  if (!cum->init)
    {
      /* We can handle at most 2 floats in R2, R3 */
      cum->maxfloats = (cum->floats > 2) ? 2 : cum->floats;

      /* We can handle at most 6 integers minus number of floats passed 
	 in registers.  */
      cum->maxints = (cum->ints > 6 - cum->maxfloats) ? 
	6 - cum->maxfloats : cum->ints;

      /* If there is no prototype, assume all the arguments are integers. */
      if (!cum->prototype)
	cum->maxints = 6;

      cum->ints = cum->floats = 0;
      cum->init = 1;
    }

  if (!TARGET_MEMPARM 
      && named 
      && type
      && !MUST_PASS_IN_STACK (mode, type))
    {
      /* Look for float, double, or long double argument.  */
      if (mode == QFmode || mode == HFmode)
	{
	  if (cum->floats < cum->maxfloats)
	    reg = c4x_fp_reglist[cum->floats];
	}
      /* Look for integer, enumeral, boolean, char, or pointer argument.  */
      else if (mode == QImode || mode == Pmode)
	{
	  if (cum->ints < cum->maxints)
	    reg = c4x_int_reglist[cum->maxfloats][cum->ints];
	}
    }
  else if (!TARGET_MEMPARM && !type)
    {
      /* We could use a different argument calling model for libcalls,
         since we're only calling functions in libgcc.  Thus we could
         pass arguments for long longs in registers rather than on the
         stack.  In the meantime, use the odd TI format.  We make the
         assumption that we won't have more than two floating point
         args, six integer args, and that all the arguments are of the
         same mode.  */
      if (mode == QFmode || mode == HFmode)
	reg = c4x_fp_reglist[cum->floats];
      else if (mode == QImode || mode == Pmode)
	reg = c4x_int_reglist[0][cum->ints];
    }

  if (TARGET_DEBUG)
    {
      fprintf (stderr, "c4x_function_arg(mode=%s, named=%d",
	       GET_MODE_NAME (mode), named);
      if (reg)
	fprintf (stderr, ", reg=%s", reg_names[reg]);
      else
	fprintf (stderr, ", stack");
      fprintf (stderr, ")\n");
    }
  if (reg)
    return gen_rtx_REG (mode, reg);
  else
    return NULL_RTX;
}


static int
c4x_isr_reg_used_p (regno)
     int regno;
{
  /* Don't save/restore FP or ST, we handle them separately.  */
  if (regno == FRAME_POINTER_REGNUM
      || IS_ST_REG (regno))
    return 0;

  /* We could be a little smarter abut saving/restoring DP.
     We'll only save if for the big memory model or if
     we're paranoid. ;-)  */
  if (IS_DP_REG (regno))
    return !TARGET_SMALL || TARGET_PARANOID;

  /* Only save/restore regs in leaf function that are used.  */
  if (c4x_leaf_function)
    return regs_ever_live[regno] && fixed_regs[regno] == 0;

  /* Only save/restore regs that are used by the ISR and regs
     that are likely to be used by functions the ISR calls
     if they are not fixed.  */
  return IS_EXT_REG (regno)
    || ((regs_ever_live[regno] || call_used_regs[regno]) 
	&& fixed_regs[regno] == 0);
}


static int
c4x_leaf_function_p ()
{
  /* A leaf function makes no calls, so we only need
     to save/restore the registers we actually use.
     For the global variable leaf_function to be set, we need
     to define LEAF_REGISTERS and all that it entails.
     Let's check ourselves...   */

  if (lookup_attribute ("leaf_pretend",
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    return 1;

  /* Use the leaf_pretend attribute at your own risk.  This is a hack
     to speed up ISRs that call a function infrequently where the
     overhead of saving and restoring the additional registers is not
     warranted.  You must save and restore the additional registers
     required by the called function.  Caveat emptor.  Here's enough
     rope...  */

  if (leaf_function_p ())
    return 1;

  return 0;
}


static int
c4x_assembler_function_p ()
{
  tree type;

  type = TREE_TYPE (current_function_decl);
  return lookup_attribute ("assembler", TYPE_ATTRIBUTES (type)) != NULL;
}


static int
c4x_interrupt_function_p ()
{
  if (lookup_attribute ("interrupt",
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    return 1;

  /* Look for TI style c_intnn  */
  return current_function_name[0] == 'c'
    && current_function_name[1] == '_'
    && current_function_name[2] == 'i'
    && current_function_name[3] == 'n' 
    && current_function_name[4] == 't'
    && isdigit (current_function_name[5])
    && isdigit (current_function_name[6]);
}


/* Write function prologue.  */

void
c4x_function_prologue (file, size)
     FILE *file;
     int size;
{
  int regno;

/* In functions where ar3 is not used but frame pointers are still
   specified, frame pointers are not adjusted (if >= -O2) and this is
   used so it won't be needlessly push the frame pointer.  */
  int dont_push_ar3;

  /* For __assembler__ function don't build a prologue.  */
  if (c4x_assembler_function_p ())
    {
      fprintf (file, "; *** Assembler Function ***\n");
      return;
    }

  /* For __interrupt__ function build specific prologue.  */
  if (c4x_interrupt_function_p ())
    {
      c4x_leaf_function = c4x_leaf_function_p ();
      fprintf (file, "; *** Interrupt Entry %s ***\n",
	       c4x_leaf_function ? "(leaf)" : "");

      fprintf (file, "\tpush\tst\n");
      if (size)
	{
	  fprintf (file, "\tpush\tar3\n\tldi\tsp,ar3\n");
	  /* FIXME: Assume ISR doesn't require more than 32767 words
	     of local variables.  */
	  if (size > 32767)
	    error ("ISR %s requires %d words of local variables, "
		   "maximum is 32767.", current_function_name, size);
	  fprintf (file, "\taddi\t%d,sp\n", size);
	}
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if (c4x_isr_reg_used_p (regno))
	    {
	      fprintf (file, "\tpush\t%s\n", reg_names[regno]);
	      if (IS_EXT_REG (regno))	/* save 32MSB of R0--R11 */
		fprintf (file, "\tpushf\t%s\n", float_reg_names[regno]);
	    }
	}
      /* We need to clear the repeat mode flag if the ISR is
         going to use a RPTB instruction or uses the RC, RS, or RE
         registers.  */
      if (regs_ever_live[RC_REGNO] 
	  || regs_ever_live[RS_REGNO] 
	  || regs_ever_live[RE_REGNO])
	fprintf (file, "\tandn\t0100h,st\n");

      /* Reload DP reg if we are paranoid about some turkey
         violating small memory model rules.  */
      if (TARGET_SMALL && TARGET_PARANOID)
	fprintf (file, TARGET_C3X ?
		 "\tldp\t@data_sec\n" :
		 "\tldpk\t@data_sec\n");
    }
  else
    {
      if (frame_pointer_needed)
	{
	  if ((size != 0)
	      || (current_function_args_size != 0)
	      || (optimize < 2))
	    {
	      fprintf (file, "\tpush\tar3\n");
	      fprintf (file, "\tldi\tsp,ar3\n");
	      dont_push_ar3 = 1;
	    }
	  else
	    {
	      /* Since ar3 is not used, we don't need to push it.  */
	      dont_push_ar3 = 1;
	    }
	}
      else
	{
	  /* If we use ar3, we need to push it.   */
	  dont_push_ar3 = 0;
	  if ((size != 0) || (current_function_args_size != 0))
	    {
	      /* If we are omitting the frame pointer, we still have
	         to make space for it so the offsets are correct
	         unless we don't use anything on the stack at all.  */
	      size += 1;
	    }
	}

      if (size > 32767)
	{
	  /* Local vars are too big, it will take multiple operations
	     to increment SP.  */
	  if (TARGET_C3X)
	    {
	      fprintf (file, "\tldi\t%d,r1\n", size >> 16);
	      fprintf (file, "\tlsh\t16,r1\n");
	    }
	  else
	    fprintf (file, "\tldhi\t%d,r1\n", size >> 16);
	  fprintf (file, "\tor\t%d,r1\n", size & 0xffff);
	  fprintf (file, "\taddi\tr1,sp\n");
	}
      else if (size != 0)
	{
	  /* Local vars take up less than 32767 words, so we can directly
	     add the number.  */
	  fprintf (file, "\taddi\t%d,sp\n", size);
	}

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  if (regs_ever_live[regno] && !call_used_regs[regno])
	    {
	      if ((regno == R6_REGNO) || (regno == R7_REGNO))
		{
		  /* R6 and R7 are saved as floating point */
		  if (TARGET_PRESERVE_FLOAT)
		    fprintf (file, "\tpush\t%s\n", reg_names[regno]);
		  fprintf (file, "\tpushf\t%s\n", float_reg_names[regno]);
		}
	      else if ((!dont_push_ar3) || (regno != AR3_REGNO))
		{
		  fprintf (file, "\tpush\t%s\n", reg_names[regno]);
		}
	    }
	}
    }
}


/* Write function epilogue.  */

void
c4x_function_epilogue (file, size)
     FILE *file;
     int size;
{
  int regno;
  int restore_count = 0;
  int delayed_jump = 0;
  int dont_pop_ar3;
  rtx insn;

  insn = get_last_insn ();
  if (insn && GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);

  if (insn && GET_CODE (insn) == BARRIER)
    return;

  /* For __assembler__ function build no epilogue.  */
  if (c4x_assembler_function_p ())
    {
      fprintf (file, "\trets\n");	/* Play it safe */
      return;
    }

#ifdef FUNCTION_BLOCK_PROFILER_EXIT
  if (profile_block_flag == 2)
    {
      FUNCTION_BLOCK_PROFILER_EXIT (file);
    }
#endif

  /* For __interrupt__ function build specific epilogue.  */
  if (c4x_interrupt_function_p ())
    {
      for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; --regno)
	{
	  if (!c4x_isr_reg_used_p (regno))
	    continue;
	  if (IS_EXT_REG (regno))
	    fprintf (file, "\tpopf\t%s\n", float_reg_names[regno]);
	  fprintf (file, "\tpop\t%s\n", reg_names[regno]);
	}
      if (size)
	{
	  fprintf (file, "\tsubi\t%d,sp\n", size);
	  fprintf (file, "\tpop\tar3\n");
	}
      fprintf (file, "\tpop\tst\n");
      fprintf (file, "\treti\n");
    }
  else
    {
      if (frame_pointer_needed)
	{
	  if ((size != 0) 
	      || (current_function_args_size != 0) 
	      || (optimize < 2))
	    {
	      /* R2 holds the return value.  */
	      fprintf (file, "\tldi\t*-ar3(1),r2\n");

	      /* We already have the return value and the fp,
	         so we need to add those to the stack.  */
	      size += 2;
	      delayed_jump = 1;
	      restore_count = 1;
	      dont_pop_ar3 = 1;
	    }
	  else
	    {
	      /* Since ar3 is not used for anything, we don't need to
	         pop it.  */
	      dont_pop_ar3 = 1;
	    }
	}
      else
	{
	  dont_pop_ar3 = 0;	/* If we use ar3, we need to pop it */
	  if (size || current_function_args_size)
	    {
	      /* If we are ommitting the frame pointer, we still have
	         to make space for it so the offsets are correct
	         unless we don't use anything on the stack at all.  */
	      size += 1;
	    }
	}

      /* Now get the number of instructions required to restore the
         registers.  */
      for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
	{
	  if ((regs_ever_live[regno] && !call_used_regs[regno])
	      && ((!dont_pop_ar3) || (regno != AR3_REGNO)))
	    {
	      restore_count++;
	      if (TARGET_PRESERVE_FLOAT
		  && ((regno == R6_REGNO) || (regno == R7_REGNO)))
	        restore_count++;
	    }
	}

      /* Get the number of instructions required to restore the stack.  */
      if (size > 32767)
	restore_count += (TARGET_C3X ? 4 : 3);
      else if (size != 0)
	restore_count += 1;

      if (delayed_jump && (restore_count < 3))
	{
	  /* We don't have enough instructions to account for the delayed
	     branch, so put some nops in.  */

	  fprintf (file, "\tbud\tr2\n");
	  while (restore_count < 3)
	    {
	      fprintf (file, "\tnop\n");
	      restore_count++;
	    }
	  restore_count = 0;
	}

      /* Now restore the saved registers, putting in the delayed branch
         where required.  */
      for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
	{
	  if (regs_ever_live[regno] && !call_used_regs[regno])
	    {
	      if (regno == AR3_REGNO && dont_pop_ar3)
		continue;

	      if (delayed_jump && (restore_count == 3))
		fprintf (file, "\tbud\tr2\n");

	      /* R6 and R7 are saved as floating point.  */
	      if ((regno == R6_REGNO) || (regno == R7_REGNO))
		{
		  fprintf (file, "\tpopf\t%s\n", float_reg_names[regno]);
		  if (TARGET_PRESERVE_FLOAT)
		    {
	              restore_count--;
	              if (delayed_jump && (restore_count == 3))
		        fprintf (file, "\tbud\tr2\n");
		      fprintf (file, "\tpop\t%s\n", reg_names[regno]);
		    }
		}
	      else
		fprintf (file, "\tpop\t%s\n", reg_names[regno]);
	      restore_count--;
	    }
	}

      if (delayed_jump && (restore_count == 3))
	fprintf (file, "\tbud\tr2\n");

      if (frame_pointer_needed)
	{
	  if ((size != 0)
	      || (current_function_args_size != 0)
	      || (optimize < 2))
	    {
	      /* Restore the old FP.  */
	      fprintf (file, "\tldi\t*ar3,ar3\n");
	      restore_count--;

	      if (delayed_jump && (restore_count == 3))
		fprintf (file, "\tbud\tr2\n");
	    }
	}

      if (size > 32767)
	{
	  /* Local vars are too big, it will take multiple operations
	     to decrement SP.  */
	  if (TARGET_C3X)
	    {
	      fprintf (file, "\tldi\t%d,r3\n", size >> 16);
	      if (delayed_jump)
		fprintf (file, "\tbud\tr2\n");
	      fprintf (file, "\tlsh\t16,r3\n");
	    }
	  else
	    fprintf (file, "\tldhi\t%d,r3\n", size >> 16);
	  fprintf (file, "\tor\t%d,r3\n", size & 0xffff);
	  fprintf (file, "\tsubi\tr3,sp\n");
	}
      else if (size != 0)
	{
	  /* Local vars take up less than 32768 words, so we can directly
	     subtract the number.  */
	  fprintf (file, "\tsubi\t%d,sp\n", size);
	}

      if (!delayed_jump)
	fprintf (file, "\trets\n");
    }
}

int
c4x_null_epilogue_p ()
{
  int regno;

  if (reload_completed
      && !c4x_assembler_function_p ()
      && !c4x_interrupt_function_p ()
      && !current_function_calls_alloca
      && !current_function_args_size
      && !(profile_block_flag == 2)
      && !(optimize < 2)
      && !get_frame_size ())
    {
      for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
	if (regs_ever_live[regno] && !call_used_regs[regno]
	    && (regno != AR3_REGNO))
	  return 0;
      return 1;
    }
  return 0;
}


void
c4x_emit_libcall (name, code, dmode, smode, noperands, operands)
     char *name;
     enum rtx_code code;
     enum machine_mode dmode;
     enum machine_mode smode;
     int noperands;
     rtx *operands;
{
  rtx ret;
  rtx insns;
  rtx libcall;
  rtx equiv;

  start_sequence ();
  libcall = gen_rtx_SYMBOL_REF (Pmode, name);
  switch (noperands)
    {
    case 2:
      ret = emit_library_call_value (libcall, NULL_RTX, 1, dmode, 1,
				     operands[1], smode);
      equiv = gen_rtx (code, dmode, operands[1]);
      break;

    case 3:
      ret = emit_library_call_value (libcall, NULL_RTX, 1, dmode, 2,
				     operands[1], smode, operands[2], smode);
      equiv = gen_rtx (code, dmode, operands[1], operands[2]);
      break;

    default:
      fatal ("c4x_emit_libcall: Bad number of operands");
    }

  insns = get_insns ();
  end_sequence ();
  emit_libcall_block (insns, operands[0], ret, equiv);
}


void
c4x_emit_libcall3 (name, code, mode, operands)
     const char *name;
     enum rtx_code code;
     enum machine_mode mode;
     rtx *operands;
{
  return c4x_emit_libcall (name, code, mode, mode, 3, operands);
}

void
c4x_emit_libcall_mulhi (name, code, mode, operands)
     char *name;
     enum rtx_code code;
     enum machine_mode mode;
     rtx *operands;
{
  rtx ret;
  rtx insns;
  rtx libcall;
  rtx equiv;

  start_sequence ();
  libcall = gen_rtx_SYMBOL_REF (Pmode, name);
  ret = emit_library_call_value (libcall, NULL_RTX, 1, mode, 2,
                                 operands[1], mode, operands[2], mode);
  equiv = gen_rtx_TRUNCATE (mode,
                   gen_rtx_LSHIFTRT (HImode,
                            gen_rtx_MULT (HImode,
                                     gen_rtx (code, HImode, operands[1]),
                                     gen_rtx (code, HImode, operands[2])),
                                     GEN_INT (32)));
  insns = get_insns ();
  end_sequence ();
  emit_libcall_block (insns, operands[0], ret, equiv);
}


enum reg_class
c4x_preferred_reload_class (x, class)
     rtx x;
     enum reg_class class;
{
  if (GET_CODE (x) == MEM && class > ADDR_REGS && class != INDEX_REGS)
    {
      x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS)
        {
          rtx op0 = XEXP (x, 0);
          rtx op1 = XEXP (x, 1);

          if (REG_P (op0) 
	      && IS_ADDR_REGNO (op0)
	      && GET_CODE (op1) == CONST_INT
	      && !IS_DISP8_CONST (INTVAL (op1)))
	    class = ADDR_REGS;
        }
    }
  return class;
}


enum reg_class
c4x_limit_reload_class (mode, class)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     enum reg_class class;
{
  return class;
}


enum reg_class
c4x_secondary_memory_needed (class1, class2, mode)
     enum reg_class class1 ATTRIBUTE_UNUSED;
     enum reg_class class2 ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return 0;
}


int
c4x_check_legit_addr (mode, addr, strict)
     enum machine_mode mode;
     rtx addr;
     int strict;
{
  rtx base = NULL_RTX;		/* Base register (AR0-AR7) */
  rtx indx = NULL_RTX;		/* Index register (IR0,IR1) */
  rtx disp = NULL_RTX;		/* Displacement */
  enum rtx_code code;

  code = GET_CODE (addr);
  switch (code)
    {
      /* Register indirect with auto increment/decrement.  We don't
	 allow SP here---push_operand should recognise an operand
	 being pushed on the stack.  */

    case PRE_DEC:
    case POST_DEC:
      if (mode != QImode && mode != QFmode)
	return 0;
    case PRE_INC:
    case POST_INC:
      base = XEXP (addr, 0);
      if (!REG_P (base))
	return 0;
      break;

    case PRE_MODIFY:
    case POST_MODIFY:
      {
	rtx op0 = XEXP (addr, 0);
	rtx op1 = XEXP (addr, 1);

	if (mode != QImode && mode != QFmode)
	  return 0;

	if (!REG_P (op0) 
	    || (GET_CODE (op1) != PLUS && GET_CODE (op1) != MINUS))
	  return 0;
	base = XEXP (op1, 0);
	if (base != op0)
	  return 0;
	if (REG_P (XEXP (op1, 1)))
	  indx = XEXP (op1, 1);
	else
	  disp = XEXP (op1, 1);
      }
      break;
	
      /* Register indirect.  */
    case REG:
      base = addr;
      break;

      /* Register indirect with displacement or index.  */
    case PLUS:
      {
	rtx op0 = XEXP (addr, 0);
	rtx op1 = XEXP (addr, 1);
	enum rtx_code code0 = GET_CODE (op0);

	switch (code0)
	  {
	  case USE:
	    /* The uses are put in to avoid problems
	       with referenced things disappearing.  */
	    return c4x_check_legit_addr (mode, op1, strict);

	  case PLUS:
	    /* This is another reference to keep things
	       from disappearing, but it contains a plus
	       of a use and DP.  */
	    if (GET_CODE (XEXP (op0, 0)) == USE)
	      return c4x_check_legit_addr (mode, op1, strict);
	    return 0;

	  case REG:
	    if (REG_P (op1))
	      {
		base = op0;	/* base + index */
		indx = op1;
		if (IS_INDEX_REGNO (base) || IS_ADDR_REGNO (indx))
		  {
		    base = op1;
		    indx = op0;
		  }
	      }
	    else
	      {
		base = op0;	/* base + displacement */
		disp = op1;
	      }
	    break;

	  default:
	    return 0;
	  }
      }
      break;

      /* Direct addressing with some work for the assembler...  */
    case CONST:
      if (GET_CODE (XEXP (addr, 0)) == PLUS
	  && (GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF
	      || GET_CODE (XEXP (XEXP (addr, 0), 0)) == LABEL_REF)
	  && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT)
	return 1;

      /* Direct addressing.  */
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

      /* Do not allow direct memory access to absolute addresses.
         This is more pain than its worth, especially for the
         small memory model where we can't guarantee that
         this address is within the data page---we don't want
         to modify the DP register in the small memory model,
         even temporarily, since an interrupt can sneak in....  */
    case CONST_INT:
      return 0;

      /* Indirect indirect addressing.  */
    case MEM:
      return 0;

    case CONST_DOUBLE:
      fatal_insn ("Using CONST_DOUBLE for address", addr);

    default:
      return 0;
    }

  /* Validate the base register.  */
  if (base)
    {
      /* Check that the address is offsettable for HImode and HFmode.  */
      if (indx && (mode == HImode || mode == HFmode))
	return 0;

      /* Handle DP based stuff.  */
      if (REGNO (base) == DP_REGNO)
	return 1;
      if (strict && !REGNO_OK_FOR_BASE_P (REGNO (base)))
	return 0;
      else if (!strict && !IS_ADDR_OR_PSEUDO_REGNO (base))
	return 0;
    }

  /* Now validate the index register.  */
  if (indx)
    {
      if (GET_CODE (indx) != REG)
	return 0;
      if (strict && !REGNO_OK_FOR_INDEX_P (REGNO (indx)))
	return 0;
      else if (!strict && !IS_INDEX_OR_PSEUDO_REGNO (indx))
	return 0;
    }

  /* Validate displacement.  */
  if (disp)
    {
      if (GET_CODE (disp) != CONST_INT)
	return 0;
      if (mode == HImode || mode == HFmode)
	{
	  /* The offset displacement must be legitimate.  */
	  if (!IS_DISP8_OFF_CONST (INTVAL (disp)))
	    return 0;
	}
      else
	{
	  if (!IS_DISP8_CONST (INTVAL (disp)))
	    return 0;
	}
      /* Can't add an index with a disp.  */
      if (indx)
	return 0;		
    }
  return 1;
}


rtx
c4x_legitimize_address (orig, mode)
     rtx orig ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return NULL_RTX;
}


/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  
   This is used in cse and loop optimisation to determine
   if it is worthwhile storing a common address into a register. 
   Unfortunately, the C4x address cost depends on other operands.  */

int 
c4x_address_cost (addr)
rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      return 1;

    case CONST:
      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (addr, &offset);
	
	if (GET_CODE (addr) == LABEL_REF)
	  return 3;
	
	if (GET_CODE (addr) != SYMBOL_REF)
	  return 4;

	if (INTVAL (offset) == 0)
	  return 3;
      }
      
      /* fall through */
      
    case POST_INC:
    case POST_DEC:
    case PRE_INC:
    case PRE_DEC:
      return 1;
      
    case SYMBOL_REF:
    case LABEL_REF:
      return TARGET_SMALL ? 3 : 4;
      
    case PLUS:
      {
	register rtx op0 = XEXP (addr, 0);
	register rtx op1 = XEXP (addr, 1);
	
	if (GET_CODE (op0) != REG)
	  break;
	
	switch (GET_CODE (op1))
	  {
	  default:
	    break;

	  case REG:
	    return 2;

	  case CONST_INT:
	    if (IS_DISP1_CONST (INTVAL (op1)))
	      return 1;

	    if (!TARGET_C3X && IS_UINT5_CONST (INTVAL (op1)))
	      return 2;

	    return 3;
	  }
      }
    default:
    }
  
  return 4;
}


rtx
c4x_gen_compare_reg (code, x, y)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg;

  if (mode == CC_NOOVmode
      && (code == LE || code == GE || code == LT || code == GT))
    return NULL_RTX;

  cc_reg = gen_rtx_REG (mode, ST_REGNO);
  emit_insn (gen_rtx_SET (VOIDmode, cc_reg,
			  gen_rtx_COMPARE (mode, x, y)));
  return cc_reg;
}

char *
c4x_output_cbranch (reversed, insn)
     int reversed;
     rtx insn;
{
  int delayed = 0;
  int annultrue = 0;
  int annulfalse = 0;
  rtx delay;
  char *cp;
  static char str[20];
  
  if (final_sequence)
    {
      delay = XVECEXP (final_sequence, 0, 1);
      delayed = !INSN_ANNULLED_BRANCH_P (insn);
      annultrue = INSN_ANNULLED_BRANCH_P (insn) && !INSN_FROM_TARGET_P (delay);
      annulfalse = INSN_ANNULLED_BRANCH_P (insn) && INSN_FROM_TARGET_P (delay);
    }
  cp = str;
  *cp++ = 'b';
  *cp++ = '%';
  if (reversed)
    *cp++ = 'I';
  *cp++ = '0';
  if (delayed)
    {
      *cp++ = '%';
      *cp++ = '#';
    }
  if (annultrue)
    {
      *cp++ = 'a';
      *cp++ = 't';
    }
  if (annulfalse)
    {
      *cp++ = 'a'; 
      *cp++ = 'f';
    }
  *cp++ = '\t';
  *cp++ = '%'; 
  *cp++ = 'l';
  *cp++ = '1';
  *cp = 0;
  return str;
}


void
c4x_print_operand (file, op, letter)
     FILE *file;		/* file to write to */
     rtx op;			/* operand to print */
     int letter;		/* %<letter> or 0 */
{
  rtx op1;
  enum rtx_code code;

  switch (letter)
    {
    case '#':			/* delayed */
      if (final_sequence)
	asm_fprintf (file, "d");
      return;
    }

  code = GET_CODE (op);
  switch (letter)
    {
    case 'A':			/* direct address */
      if (code == CONST_INT || code == SYMBOL_REF)
	asm_fprintf (file, "@");
      break;

    case 'C':			/* call */
      if (code != MEM)
	fatal_insn ("c4x_print_operand: %%C inconsistency", op);
      op1 = XEXP (op, 0);
      SYMBOL_REF_FLAG (op1) = 1;
      output_addr_const (file, op1);
      return;

    case 'H':			/* sethi */
      if (code == SYMBOL_REF)
	SYMBOL_REF_FLAG (op) = 1;
      break;

    case 'I':			/* reversed condition */
      code = reverse_condition (code);
      break;

    case 'L':			/* log 2 of constant */
      if (code != CONST_INT)
	fatal_insn ("c4x_print_operand: %%L inconsistency", op);
      fprintf (file, "%d", exact_log2 (INTVAL (op)));
      return;

    case 'N':			/* ones complement of small constant */
      if (code != CONST_INT)
	fatal_insn ("c4x_print_operand: %%N inconsistency", op);
      fprintf (file, "%d", ~INTVAL (op));
      return;

    case 'K':			/* generate ldp(k) if direct address */
      if (!TARGET_SMALL
	  && code == MEM
	  && GET_CODE (XEXP (op, 0)) == PLUS
	  && GET_CODE(XEXP (XEXP (op, 0), 0)) == REG
	  && REGNO(XEXP (XEXP (op, 0), 0)) == DP_REGNO)
	{
	  op1 = XEXP (XEXP (op, 0), 1);
          if (GET_CODE(op1) == CONST_INT || GET_CODE(op1) == SYMBOL_REF)
	    {
	      asm_fprintf (file, "\t%s\t", TARGET_C3X ? "ldp" : "ldpk");
	      output_address (XEXP (adj_offsettable_operand (op, 1), 0));
	      asm_fprintf (file, "\n");
	    }
	}
      return;

    case 'M':			/* generate ldp(k) if direct address */
      if (!TARGET_SMALL		/* only used in asm statements */
	  && code == MEM
	  && (GET_CODE (XEXP (op, 0)) == CONST
	      || GET_CODE (XEXP (op, 0)) == SYMBOL_REF))
	{
	  asm_fprintf (file, "%s\t", TARGET_C3X ? "ldp" : "ldpk");
          output_address (XEXP (op, 0));
	  asm_fprintf (file, "\n\t");
	}
      return;

    case 'O':			/* offset address */
      if (code == MEM && c4x_autoinc_operand (op, Pmode))
	break;
      else if (code == MEM)
	output_address (XEXP (adj_offsettable_operand (op, 1), 0));
      else if (code == REG)
	fprintf (file, "%s", reg_names[REGNO (op) + 1]);
      else
	fatal_insn ("c4x_print_operand: %%O inconsistency", op);
      return;

    case 'R':			/* call register */
      op1 = XEXP (op, 0);
      if (code != MEM || GET_CODE (op1) != REG)
	fatal_insn ("c4x_print_operand: %%R inconsistency", op);
      else
	fprintf (file, "%s", reg_names[REGNO (op1)]);
      return;

    default:
      break;
    }
  
  switch (code)
    {
    case REG:
      if (GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT)
	fprintf (file, "%s", float_reg_names[REGNO (op)]);
      else
	fprintf (file, "%s", reg_names[REGNO (op)]);
      break;
      
    case MEM:
      output_address (XEXP (op, 0));
      break;
      
    case CONST_DOUBLE:
      {
	char str[30];
	REAL_VALUE_TYPE r;
	
	REAL_VALUE_FROM_CONST_DOUBLE (r, op);
	REAL_VALUE_TO_DECIMAL (r, "%20f", str);
	fprintf (file, "%s", str);
      }
      break;
      
    case CONST_INT:
      fprintf (file, "%d", INTVAL (op));
      break;
      
    case NE:
      asm_fprintf (file, "ne");
      break;
      
    case EQ:
      asm_fprintf (file, "eq");
      break;
      
    case GE:
      asm_fprintf (file, "ge");
      break;

    case GT:
      asm_fprintf (file, "gt");
      break;

    case LE:
      asm_fprintf (file, "le");
      break;

    case LT:
      asm_fprintf (file, "lt");
      break;

    case GEU:
      asm_fprintf (file, "hs");
      break;

    case GTU:
      asm_fprintf (file, "hi");
      break;

    case LEU:
      asm_fprintf (file, "ls");
      break;

    case LTU:
      asm_fprintf (file, "lo");
      break;

    case SYMBOL_REF:
      output_addr_const (file, op);
      break;

    case CONST:
      output_addr_const (file, XEXP (op, 0));
      break;

    case CODE_LABEL:
      break;

    default:
      fatal_insn ("c4x_print_operand: Bad operand case", op);
      break;
    }
}


void
c4x_print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "*%s", reg_names[REGNO (addr)]);
      break;

    case PRE_DEC:
      fprintf (file, "*--%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_INC:
      fprintf (file, "*%s++", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_MODIFY:
      {
	rtx op0 = XEXP (XEXP (addr, 1), 0);
	rtx op1 = XEXP (XEXP (addr, 1), 1);
	
	if (GET_CODE (XEXP (addr, 1)) == PLUS && REG_P (op1))
	  fprintf (file, "*%s++(%s)", reg_names[REGNO (op0)],
		   reg_names[REGNO (op1)]);
	else if (GET_CODE (XEXP (addr, 1)) == PLUS && INTVAL (op1) > 0)
	  fprintf (file, "*%s++(%d)", reg_names[REGNO (op0)],
		   INTVAL (op1));
	else if (GET_CODE (XEXP (addr, 1)) == PLUS && INTVAL (op1) < 0)
	  fprintf (file, "*%s--(%d)", reg_names[REGNO (op0)],
		   -INTVAL (op1));
	else if (GET_CODE (XEXP (addr, 1)) == MINUS && REG_P (op1))
	  fprintf (file, "*%s--(%s)", reg_names[REGNO (op0)],
		   reg_names[REGNO (op1)]);
	else
	  fatal_insn ("c4x_print_operand_address: Bad post_modify", addr);
      }
      break;
      
    case PRE_MODIFY:
      {
	rtx op0 = XEXP (XEXP (addr, 1), 0);
	rtx op1 = XEXP (XEXP (addr, 1), 1);
	
	if (GET_CODE (XEXP (addr, 1)) == PLUS && REG_P (op1))
	  fprintf (file, "*++%s(%s)", reg_names[REGNO (op0)],
		   reg_names[REGNO (op1)]);
	else if (GET_CODE (XEXP (addr, 1)) == PLUS && INTVAL (op1) > 0)
	  fprintf (file, "*++%s(%d)", reg_names[REGNO (op0)],
		   INTVAL (op1));
	else if (GET_CODE (XEXP (addr, 1)) == PLUS && INTVAL (op1) < 0)
	  fprintf (file, "*--%s(%d)", reg_names[REGNO (op0)],
		   -INTVAL (op1));
	else if (GET_CODE (XEXP (addr, 1)) == MINUS && REG_P (op1))
	  fprintf (file, "*--%s(%s)", reg_names[REGNO (op0)],
		   reg_names[REGNO (op1)]);
	else
	  fatal_insn ("c4x_print_operand_address: Bad pre_modify", addr);
      }
      break;
      
    case PRE_INC:
      fprintf (file, "*++%s", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case POST_DEC:
      fprintf (file, "*%s--", reg_names[REGNO (XEXP (addr, 0))]);
      break;

    case PLUS:			/* Indirect with displacement.  */
      {
	rtx op0 = XEXP (addr, 0);
	rtx op1 = XEXP (addr, 1);
	enum rtx_code code0 = GET_CODE (op0);

	if (code0 == USE || code0 == PLUS)
	  {
	    asm_fprintf (file, "@");
	    output_addr_const (file, op1);
	  }
	else if (REG_P (op0))
	  {
	    if (REGNO (op0) == DP_REGNO)
	      {
		c4x_print_operand_address (file, op1);
	      }
	    else if (REG_P (op1))
	      {
		if (IS_INDEX_REGNO (op0))
		  {
		    fprintf (file, "*+%s(%s)",
			     reg_names[REGNO (op1)],
			     reg_names[REGNO (op0)]);	/* index + base */
		  }
		else
		  {
		    fprintf (file, "*+%s(%s)",
			     reg_names[REGNO (op0)],
			     reg_names[REGNO (op1)]);	/* base + index */
		  }
	      }
	    else if (INTVAL (op1) < 0)
	      {
		fprintf (file, "*-%s(%d)",
			 reg_names[REGNO (op0)],
			 -INTVAL (op1));	/* base - displacement */
	      }
	    else
	      {
		fprintf (file, "*+%s(%d)",
			 reg_names[REGNO (op0)],
			 INTVAL (op1));		/* base + displacement */
	      }
	  }
      }
      break;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (!SYMBOL_REF_FLAG (addr))
	fprintf (file, "@");
      output_addr_const (file, addr);
      SYMBOL_REF_FLAG (addr) = 0;
      break;

      /* We shouldn't access CONST_INT addresses.  */
    case CONST_INT:

    default:
      fatal_insn ("c4x_print_operand_address: Bad operand case", addr);
      break;
    }
}


static int
c4x_immed_float_p (operand)
     rtx operand;
{
  long convval[2];
  int exponent;
  REAL_VALUE_TYPE r;

  REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
  if (GET_MODE (operand) == HFmode)
    REAL_VALUE_TO_TARGET_DOUBLE (r, convval);
  else
    {
      REAL_VALUE_TO_TARGET_SINGLE (r, convval[0]);
      convval[1] = 0;
    }

  /* sign extend exponent */
  exponent = (((convval[0] >> 24) & 0xff) ^ 0x80) - 0x80;
  if (exponent == -128)
    return 1;			/* 0.0 */
  if ((convval[0] & 0x00000fff) != 0 || convval[1] != 0)
    return 0;			/* Precision doesn't fit */
  return (exponent <= 7)	/* Positive exp */
    && (exponent >= -7);	/* Negative exp */
}


/* This function checks for an insn operand that requires direct
   addressing and inserts a load of the DP register prior to the
   insn if the big memory model is being compiled for.  Immediate
   operands that do not fit within the opcode field get changed
   into memory references using direct addressing.  At this point
   all pseudos have been converted to hard registers.  */

int
c4x_scan_for_ldp (newop, insn, operand0)
     rtx *newop;
     rtx insn;
     rtx operand0;
{
  int i;
  char *format_ptr;
  rtx op0, op1, op2, addr;
  rtx operand = *newop;

  switch (GET_CODE (operand))
    {
    case MEM:
      op0 = XEXP (operand, 0);

      /* We have something we need to emit a load dp insn for.
         The first operand should hold the rtx for the instruction
         required.  */

      switch (GET_CODE (op0))
	{
	case CONST_INT:
	  fatal_insn ("c4x_scan_for_ldp: Direct memory access to const_int",
		     op0);
	  break;

	case CONST:
	case SYMBOL_REF:
	  if (!TARGET_C3X && !TARGET_SMALL
	      && recog_memoized (insn) == CODE_FOR_movqi_noclobber
	      && ((addr = find_reg_note (insn, REG_EQUAL, NULL_RTX))
		  || (addr = find_reg_note (insn, REG_EQUIV, NULL_RTX)))
	      && (IS_STD_OR_PSEUDO_REGNO (operand0)))
	    {
	      addr = XEXP (addr, 0);
	      if (GET_CODE (addr) == CONST_INT)
		{
		  op1 = GEN_INT (INTVAL (addr) & ~0xffff);
		  emit_insn_before (gen_movqi (operand0, op1), insn);
		  op1 = GEN_INT (INTVAL (addr) & 0xffff);
		  emit_insn_before (gen_iorqi3_noclobber (operand0,
						      operand0, op1), insn);
		  delete_insn (insn);
		  return 1;
		}
	      else if (GET_CODE (addr) == SYMBOL_REF)
		{
		  emit_insn_before (gen_set_high_use (operand0, addr, addr),
				    insn);
		  emit_insn_before (gen_set_ior_lo_use (operand0, addr, addr),
				    insn);
		  delete_insn (insn);
		  return 1;
		}
	      else if (GET_CODE (addr) == CONST
		       && GET_CODE (op1 = XEXP (addr, 0)) == PLUS
		       && GET_CODE (op2 = XEXP (op1, 0)) == SYMBOL_REF
		       && GET_CODE (XEXP (op1, 1)) == CONST_INT)
		{
		  emit_insn_before (gen_set_high_use (operand0, addr, op2),
				    insn);
		  emit_insn_before (gen_set_ior_lo_use (operand0, addr, op2),
				    insn);
		  delete_insn (insn);
		  return 1;
		}
	    }
	  if (!TARGET_SMALL)
	    emit_insn_before (gen_set_ldp (gen_rtx_REG (Pmode, DP_REGNO),
					   operand), insn);

	  /* Replace old memory reference with direct reference.  */
	  *newop = gen_rtx_MEM (GET_MODE (operand),
				gen_rtx_PLUS (Pmode,
					      gen_rtx_REG (Pmode, DP_REGNO),
					      op0));

	  /* Use change_address?  */
	  MEM_VOLATILE_P (*newop) = MEM_VOLATILE_P (operand);
	  RTX_UNCHANGING_P (*newop) = RTX_UNCHANGING_P (operand);
	  MEM_IN_STRUCT_P (*newop) = MEM_IN_STRUCT_P (operand);
	  break;

	default:
	  break;
	}

      return 0;

    case CONST_INT:
      if (SMALL_CONST (INTVAL (operand), insn))
	break;
      fatal_insn ("Immediate integer too large", insn);

    case CONST_DOUBLE:
      if (c4x_immed_float_p (operand))
	break;

      /* We'll come here if a CONST_DOUBLE integer has slipped
         though the net...  */
      fatal_insn ("Immediate CONST_DOUBLE integer too large", insn);

    case CONST:
      fatal_insn ("Immediate integer not known", insn);

      /* Symbol and label immediate addresses cannot be stored
         within a C[34]x instruction, so we store them in memory
         and use direct addressing instead.  */
    case LABEL_REF:
    case SYMBOL_REF:
      if (GET_CODE (operand0) != REG)
	break;

      op0 = XEXP (force_const_mem (Pmode, operand), 0);
      *newop = gen_rtx_MEM (GET_MODE (operand),
			    gen_rtx_PLUS (Pmode,
					  gen_rtx_PLUS (Pmode,
					      gen_rtx_USE (VOIDmode, operand),
					      gen_rtx_REG (Pmode, DP_REGNO)),
					  op0));
      
      if (!TARGET_SMALL)
	emit_insn_before (gen_set_ldp_use (gen_rtx_REG (Pmode, DP_REGNO),
					   *newop, operand), insn);
      return 0;

    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (operand));

  /* Recursively hunt for required loads of DP.  */
  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (operand)); i++)
    {
      if (*format_ptr++ == 'e')	/* rtx expression */
	if (c4x_scan_for_ldp (&XEXP (operand, i), insn, operand0))
	  break;
    }
  return 0;
}


/* The last instruction in a repeat block cannot be a Bcond, DBcound,
   CALL, CALLCond, TRAPcond, RETIcond, RETScond, IDLE, RPTB or RPTS.

   None of the last four instructions from the bottom of the block can
   be a BcondD, BRD, DBcondD, RPTBD, LAJ, LAJcond, LATcond, BcondAF,
   BcondAT or RETIcondD.

   This routine scans the four previous insns for a jump insn, and if
   one is found, returns 1 so that we bung in a nop instruction.
   This simple minded strategy will add a nop, when it may not
   be required.  Say when there is a JUMP_INSN near the end of the
   block that doesn't get converted into a delayed branch.

   Note that we cannot have a call insn, since we don't generate
   repeat loops with calls in them (although I suppose we could, but
   there's no benefit.)  

   !!! FIXME.  The rptb_top insn may be sucked into a SEQUENCE.  */

int
c4x_rptb_nop_p (insn)
     rtx insn;
{
  rtx start_label;
  int i;

  /* Extract the start label from the jump pattern (rptb_end).  */
  start_label = XEXP (XEXP (SET_SRC (XVECEXP (PATTERN (insn), 0, 0)), 1), 0);

  /* If there is a label at the end of the loop we must insert
     a NOP.  */
  insn = prev_nonnote_insn (insn);
  if (GET_CODE (insn) == CODE_LABEL)
    return 1;

  for (i = 0; i < 4; i++)
    {
      /* Search back for prev non-note and non-label insn.  */
      while (GET_CODE (insn) == NOTE || GET_CODE (insn) == CODE_LABEL
	     || GET_CODE (insn) == USE || GET_CODE (insn) == CLOBBER)
	{
	  if (insn == start_label)
	    return i == 0;

	  insn = PREV_INSN (insn);
	};

      /* If we have a jump instruction we should insert a NOP. If we
	 hit repeat block top we should only insert a NOP if the loop
	 is empty. */
      if (GET_CODE (insn) == JUMP_INSN)
	return 1;
      insn = PREV_INSN (insn);
    }
  return 0;
}


void
c4x_rptb_insert (insn)
     rtx insn;
{
  rtx end_label;
  rtx start_label;
  
  /* Extract the start label from the jump pattern (rptb_end).  */
  start_label = XEXP (XEXP (SET_SRC (XVECEXP (PATTERN (insn), 0, 0)), 1), 0);

  /* We'll have to update the basic blocks.  */
  end_label = gen_label_rtx ();
  emit_label_after (end_label, insn);

  for (; insn; insn = PREV_INSN (insn))
    if (insn == start_label)
      break;
  if (!insn)
    fatal_insn ("c4x_rptb_insert: Cannot find start label", start_label);

  /* We'll have to update the basic blocks.  */
  emit_insn_before (gen_rptb_top (start_label, end_label), insn);
}

/* This function is a C4x special. It scans through all the insn
   operands looking for places where the DP register needs to be
   reloaded and for large immediate operands that need to be converted
   to memory references.  The latter should be avoidable with proper
   definition of patterns in machine description.  We come here right
   near the end of things, immediately before delayed branch
   scheduling.  */

void
c4x_process_after_reload (first)
     rtx first;
{
  rtx insn;
  int i;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      /* Look for insn.  */
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  int insn_code_number;

	  insn_code_number = recog_memoized (insn);

	  if (insn_code_number < 0)
	    continue;

	  /* Insert the RTX for RPTB at the top of the loop
	     and a label at the end of the loop. */
	  if (insn_code_number == CODE_FOR_rptb_end)
	    c4x_rptb_insert(insn);

	  /* We split all insns here if they have a # for the output
	     template if we are using the big memory model since there
	     is a chance that we might be accessing memory across a
	     page boundary.  */

	  if (!TARGET_SMALL)
	    {
	      char *template;

	      template = insn_template[insn_code_number];
	      if (template && template[0] == '#' && template[1] == '\0')
		{
		  rtx new = try_split (PATTERN(insn), insn, 0);
		  
		  /* If we didn't split the insn, go away.  */
		  if (new == insn && PATTERN (new) == PATTERN(insn))
		    fatal_insn ("Couldn't split pattern", insn);

		  PUT_CODE (insn, NOTE);
		  NOTE_SOURCE_FILE (insn) = 0;
		  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;

		  /* Do we have to update the basic block info here? 
		     Maybe reorg wants it sorted out... */

		  /* Continue with the first of the new insns generated
                     by the split. */
		  insn = new;

		  insn_code_number = recog_memoized (insn);
		  
		  if (insn_code_number < 0)
		    continue;
		}
	    }

	  /* Ignore jumps and calls.  */
	  if (GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == JUMP_INSN)
	      continue;	

	  insn_extract (insn);
	  for (i = 0; i < insn_n_operands[insn_code_number]; i++)
	    if (c4x_scan_for_ldp (recog_operand_loc[i], insn, 
				  recog_operand[0]))
	      break;
	}
    }
}


static int
c4x_a_register (op)
     rtx op;
{
  return REG_P (op) && IS_ADDR_OR_PSEUDO_REGNO (op);
}


static int
c4x_x_register (op)
     rtx op;
{
  return REG_P (op) && IS_INDEX_OR_PSEUDO_REGNO (op);
}


static int
c4x_int_constant (op)
     rtx op;
{
  if (GET_CODE (op) != CONST_INT)
    return 0;
  return GET_MODE (op) == VOIDmode
    || GET_MODE_CLASS (op) == MODE_INT
    || GET_MODE_CLASS (op) == MODE_PARTIAL_INT;
}


static int
c4x_float_constant (op)
     rtx op;
{
  if (GET_CODE (op) != CONST_DOUBLE)
    return 0;
  return GET_MODE (op) == QFmode || GET_MODE (op) == HFmode;
}


int
c4x_H_constant (op)
     rtx op;
{
  return c4x_float_constant (op) && c4x_immed_float_p (op);
}


int
c4x_I_constant (op)
     rtx op;
{
  return c4x_int_constant (op) && IS_INT16_CONST (INTVAL (op));
}


int
c4x_J_constant (op)
     rtx op;
{
  if (TARGET_C3X)
    return 0;
  return c4x_int_constant (op) && IS_INT8_CONST (INTVAL (op));
}


static int
c4x_K_constant (op)
     rtx op;
{
  if (TARGET_C3X)
    return 0;
  return c4x_int_constant (op) && IS_INT5_CONST (INTVAL (op));
}


int
c4x_L_constant (op)
     rtx op;
{
  return c4x_int_constant (op) && IS_UINT16_CONST (INTVAL (op));
}


static int
c4x_N_constant (op)
     rtx op;
{
  return c4x_int_constant (op) && IS_NOT_UINT16_CONST (INTVAL (op));
}


static int
c4x_O_constant (op)
     rtx op;
{
  return c4x_int_constant (op) && IS_HIGH_CONST (INTVAL (op));
}


/* The constraints do not have to check the register class,
   except when needed to discriminate between the constraints.
   The operand has been checked by the predicates to be valid.  */

/* ARx + 9-bit signed const or IRn
   *ARx, *+ARx(n), *-ARx(n), *+ARx(IRn), *-Arx(IRn) for -256 < n < 256
   We don't include the pre/post inc/dec forms here since
   they are handled by the <> constraints.  */

int
c4x_Q_constraint (op)
     rtx op;
{
  enum machine_mode mode = GET_MODE (op);

  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case REG:
      return 1;

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (!REG_P (op0))
	  return 0;

	if (REG_P (op1))
	  return 1;

	if (GET_CODE (op1) != CONST_INT)
	  return 0;

	/* HImode and HFmode must be offsettable.  */
	if (mode == HImode || mode == HFmode)
	  return IS_DISP8_OFF_CONST (INTVAL (op1));
	
	return IS_DISP8_CONST (INTVAL (op1));
      }
      break;
    default:
      break;
    }
  return 0;
}


/* ARx + 5-bit unsigned const
   *ARx, *+ARx(n) for n < 32 */

int
c4x_R_constraint (op)
     rtx op;
{
  enum machine_mode mode = GET_MODE (op);

  if (TARGET_C3X)
    return 0;
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case REG:
      return 1;

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (!REG_P (op0))
	  return 0;

	if (GET_CODE (op1) != CONST_INT)
	  return 0;

	/* HImode and HFmode must be offsettable.  */
	if (mode == HImode || mode == HFmode)
	  return IS_UINT5_CONST (INTVAL (op1) + 1);
	
	return IS_UINT5_CONST (INTVAL (op1));
      }
      break;
    default:
      break;
    }
  return 0;
}


static int
c4x_R_indirect (op)
     rtx op;
{
  enum machine_mode mode = GET_MODE (op);

  if (TARGET_C3X || GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case REG:
      return IS_ADDR_OR_PSEUDO_REGNO (op);

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	/* HImode and HFmode must be offsettable.  */
	if (mode == HImode || mode == HFmode)
	  return IS_ADDR_OR_PSEUDO_REGNO (op0)
	    && GET_CODE (op1) == CONST_INT 
	    && IS_UINT5_CONST (INTVAL (op1) + 1);

	return REG_P (op0)
	  && IS_ADDR_OR_PSEUDO_REGNO (op0)
	  && GET_CODE (op1) == CONST_INT
	  && IS_UINT5_CONST (INTVAL (op1));
      }
      break;

    default:
      break;
    }
  return 0;
}


/* ARx + 1-bit unsigned const or IRn
   *ARx, *+ARx(1), *-ARx(1), *+ARx(IRn), *-Arx(IRn)
   We don't include the pre/post inc/dec forms here since
   they are handled by the <> constraints.  */

int
c4x_S_constraint (op)
     rtx op;
{
  enum machine_mode mode = GET_MODE (op);
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case REG:
      return 1;

    case PRE_MODIFY:
    case POST_MODIFY:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);
	
	if ((GET_CODE (op1) != PLUS && GET_CODE (op1) != MINUS)
	    || (op0 != XEXP (op1, 0)))
	  return 0;
	
	op0 = XEXP (op1, 0);
	op1 = XEXP (op1, 1);
	return REG_P (op0) && REG_P (op1);
	/* pre or post_modify with a displacement of 0 or 1 
	   should not be generated.  */
      }
      break;

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (!REG_P (op0))
	  return 0;

	if (REG_P (op1))
	  return 1;

	if (GET_CODE (op1) != CONST_INT)
	  return 0;
	
	/* HImode and HFmode must be offsettable.  */
	if (mode == HImode || mode == HFmode)
	  return IS_DISP1_OFF_CONST (INTVAL (op1));
	
	return IS_DISP1_CONST (INTVAL (op1));
      }
      break;
    default:
      break;
    }
  return 0;
}


static int
c4x_S_indirect (op)
     rtx op;
{
  enum machine_mode mode = GET_MODE (op);
  if (GET_CODE (op) != MEM)
    return 0;

  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case PRE_DEC:
    case POST_DEC:
      if (mode != QImode && mode != QFmode)
	return 0;
    case PRE_INC:
    case POST_INC:
      op = XEXP (op, 0);

    case REG:
      return IS_ADDR_OR_PSEUDO_REGNO (op);

    case PRE_MODIFY:
    case POST_MODIFY:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);
	
	if (mode != QImode && mode != QFmode)
	  return 0;

	if ((GET_CODE (op1) != PLUS && GET_CODE (op1) != MINUS)
	    || (op0 != XEXP (op1, 0)))
	  return 0;
	
	op0 = XEXP (op1, 0);
	op1 = XEXP (op1, 1);
	return REG_P (op0) && IS_ADDR_OR_PSEUDO_REGNO (op0)
	  && REG_P (op1) && IS_INDEX_OR_PSEUDO_REGNO (op1);
	/* pre or post_modify with a displacement of 0 or 1 
	   should not be generated.  */
      }

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (REG_P (op0))
	  {
	    /* HImode and HFmode must be offsettable.  */
	    if (mode == HImode || mode == HFmode)
	      return IS_ADDR_OR_PSEUDO_REGNO (op0)
		&& GET_CODE (op1) == CONST_INT 
		&& IS_DISP1_OFF_CONST (INTVAL (op1));

	    if (REG_P (op1))
	      return (IS_INDEX_OR_PSEUDO_REGNO (op1)
		      && IS_ADDR_OR_PSEUDO_REGNO (op0))
		|| (IS_ADDR_OR_PSEUDO_REGNO (op1)
		    && IS_INDEX_OR_PSEUDO_REGNO (op0));
	    
	    return IS_ADDR_OR_PSEUDO_REGNO (op0)
	      && GET_CODE (op1) == CONST_INT 
	      && IS_DISP1_CONST (INTVAL (op1));
	  }
      }
      break;

    default:
      break;
    }
  return 0;
}


/* Symbol ref.  */

int
c4x_T_constraint (op)
     rtx op;
{
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);

  if ((GET_CODE (op) == PLUS)
      && (GET_CODE (XEXP (op, 0)) == REG)
      && (REGNO (XEXP (op, 0)) == DP_REGNO))
    {
      op = XEXP (op, 1);
    }
  else if ((GET_CODE (op) == PLUS)
	   && (GET_CODE (XEXP (op, 0)) == PLUS)
	   && (GET_CODE (XEXP (XEXP (op, 0), 0)) == USE))
    {
      op = XEXP (op, 1);
    }
  else if ((GET_CODE (op) == PLUS) && (GET_CODE (XEXP (op, 0)) == USE))
    {
      op = XEXP (op, 1);
    }

  /* Don't allow direct addressing to an arbitrary constant.  */
  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT)
    return 1;

  return GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF;
}


int
c4x_autoinc_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == MEM)
    {
      enum rtx_code code = GET_CODE (XEXP (op, 0));
      
      if (code == PRE_INC
	  || code == PRE_DEC
	  || code == POST_INC
	  || code == POST_DEC
	  || code == PRE_MODIFY
	  || code == POST_MODIFY
	  )
	return 1;
    }
  return 0;
}


/* Match any operand.  */

int
any_operand (op, mode)
     register rtx op ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return 1;
}


/* Nonzero if OP is a floating point value with value 0.0.  */

int
fp_zero_operand (op)
     rtx op;
{
  REAL_VALUE_TYPE r;

  REAL_VALUE_FROM_CONST_DOUBLE (r, op);
  return REAL_VALUES_EQUAL (r, dconst0);
}


int
const_operand (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  switch (mode)
    {
    case QFmode:
    case HFmode:
      if (GET_CODE (op) != CONST_DOUBLE
	  || GET_MODE (op) != mode
	  || GET_MODE_CLASS (mode) != MODE_FLOAT)
	return 0;

      return c4x_immed_float_p (op);

#if Pmode != QImode
    case Pmode:
#endif
    case QImode:
      if (GET_CODE (op) != CONST_INT
	  || (GET_MODE (op) != VOIDmode && GET_MODE (op) != mode)
	  || GET_MODE_CLASS (mode) != MODE_INT)
	return 0;

      return IS_HIGH_CONST (INTVAL (op)) || IS_INT16_CONST (INTVAL (op));

    case HImode:
      return 0;

    default:
      return 0;
    }
}


int
stik_const_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return c4x_K_constant (op);
}


int
not_const_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return c4x_N_constant (op);
}


int
reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode);
}

int
reg_imm_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (REG_P (op) || CONSTANT_P (op))
    return 1;
  return 0;
}

int
not_modify_reg (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (REG_P (op) || CONSTANT_P (op))
    return 1;
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case REG:
      return 1;

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (!REG_P (op0))
	  return 0;
	
	if (REG_P (op1) || GET_CODE (op1) == CONST_INT)
	  return 1;
      }
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;
    default:
      break;
    }
  return 0;
}

int
not_rc_reg (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (REG_P (op) && REGNO (op) == RC_REGNO)
    return 0;
  return 1;
}

/* Extended precision register R0-R1.  */

int
r0r1_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && IS_R0R1_OR_PSEUDO_REGNO (op);
}


/* Extended precision register R2-R3.  */

int
r2r3_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && IS_R2R3_OR_PSEUDO_REGNO (op);
}


/* Low extended precision register R0-R7.  */

int
ext_low_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && IS_EXT_LOW_OR_PSEUDO_REGNO (op);
}


/* Extended precision register.  */

int
ext_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (!REG_P (op))
    return 0;
  return IS_EXT_OR_PSEUDO_REGNO (op);
}


/* Standard precision register.  */

int
std_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && IS_STD_OR_PSEUDO_REGNO (op);
}


/* Address register.  */

int
addr_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  return c4x_a_register (op);
}


/* Index register.  */

int
index_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return c4x_x_register (op);
}


/* DP register.  */

int
dp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return REG_P (op) && IS_DP_OR_PSEUDO_REGNO (op);
}


/* SP register.  */

int
sp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return REG_P (op) && IS_SP_OR_PSEUDO_REGNO (op);
}


/* ST register.  */

int
st_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return REG_P (op) && IS_ST_OR_PSEUDO_REGNO (op);
}


/* RC register.  */

int
rc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return REG_P (op) && IS_RC_OR_PSEUDO_REGNO (op);
}


int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != MEM)
    return 0;
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case REG:
      return 1;
    default:
    }
  return 0;
}


/* Check src operand of two operand arithmetic instructions.  */

int
src_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (REG_P (op))
    return reg_operand (op, mode);

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* We could allow certain CONST_INT values for HImode...  */
  if (GET_CODE (op) == CONST_INT)
    return (mode == QImode || mode == Pmode) && c4x_I_constant (op);

  /* We don't like CONST_DOUBLE integers.  */
  if (GET_CODE (op) == CONST_DOUBLE)
    return c4x_H_constant (op);

  return general_operand (op, mode);
}


int
src_hi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (c4x_O_constant (op))
    return 1;
  return src_operand (op, mode);
}


/* Check src operand of two operand logical instructions.  */

int
lsrc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  if (mode != QImode && mode != Pmode)
    fatal_insn ("Mode not QImode", op);

  if (REG_P (op))
    return reg_operand (op, mode);

  if (GET_CODE (op) == CONST_INT)
    return c4x_L_constant (op) || c4x_J_constant (op);

  return general_operand (op, mode);
}


/* Check src operand of two operand tricky instructions.  */

int
tsrc_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  if (mode != QImode && mode != Pmode)
    fatal_insn ("Mode not QImode", op);

  if (REG_P (op))
    return reg_operand (op, mode);

  if (GET_CODE (op) == CONST_INT)
    return c4x_L_constant (op) || c4x_N_constant (op) || c4x_J_constant (op);

  return general_operand (op, mode);
}


int
reg_or_const_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return reg_operand (op, mode) || const_operand (op, mode);
}


/* Check for indirect operands allowable in parallel instruction.  */

int
par_ind_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  return c4x_S_indirect (op);
}


/* Check for operands allowable in parallel instruction.  */

int
parallel_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ext_low_reg_operand (op, mode) || par_ind_operand (op, mode);
}


static void 
c4x_S_address_parse (op, base, incdec, index, disp)
     rtx op;
     int *base;
     int *incdec;
     int *index;
     int *disp;
{
  *base = 0;
  *incdec = 0;
  *index = 0;
  *disp = 0;
       
  if (GET_CODE (op) != MEM)
    fatal_insn ("Invalid indirect memory address", op);
  
  op = XEXP (op, 0);
  switch (GET_CODE (op))
    {
    case PRE_DEC:
      *base = REGNO (XEXP (op, 0));
      *incdec = 1;
      *disp = -1;
      return;

    case POST_DEC:
      *base = REGNO (XEXP (op, 0));
      *incdec = 1;
      *disp = 0;
      return;

    case PRE_INC:
      *base = REGNO (XEXP (op, 0));
      *incdec = 1;
      *disp = 1;
      return;

    case POST_INC:
      *base = REGNO (XEXP (op, 0));
      *incdec = 1;
      *disp = 0;
      return;

    case POST_MODIFY:
      *base = REGNO (XEXP (op, 0));
      if (REG_P (XEXP (XEXP (op, 1), 1)))
	{
	  *index = REGNO (XEXP (XEXP (op, 1), 1));
	  *disp = 0;		/* ??? */
	}
      else
	  *disp = INTVAL (XEXP (XEXP (op, 1), 1));
      *incdec = 1;
      return;

    case PRE_MODIFY:
      *base = REGNO (XEXP (op, 0));
      if (REG_P (XEXP (XEXP (op, 1), 1)))
	{
	  *index = REGNO (XEXP (XEXP (op, 1), 1));
	  *disp = 1;		/* ??? */
	}
      else
	  *disp = INTVAL (XEXP (XEXP (op, 1), 1));
      *incdec = 1;

      return;

    case REG:
      *base = REGNO (op);
      return;

    case PLUS:
      {
	rtx op0 = XEXP (op, 0);
	rtx op1 = XEXP (op, 1);

	if (c4x_a_register (op0))
	  {
	    if (c4x_x_register (op1))
	      {
		*base = REGNO (op0);
		*index = REGNO (op1);
		return;
	      }
	    else if ((GET_CODE (op1) == CONST_INT 
		      && IS_DISP1_CONST (INTVAL (op1))))
	      {
		*base = REGNO (op0);
		*disp = INTVAL (op1);
		return;
	      }
	  }
	else if (c4x_x_register (op0) && c4x_a_register (op1))
	  {
	    *base = REGNO (op1);
	    *index = REGNO (op0);
	    return;
	  }
      }
      /* Fallthrough  */

    default:
      fatal_insn ("Invalid indirect (S) memory address", op);
    }
}


int
c4x_address_conflict (op0, op1, store0, store1)
     rtx op0;
     rtx op1;
     int store0;
     int store1;
{
  int base0;
  int base1;
  int incdec0;
  int incdec1;
  int index0;
  int index1;
  int disp0;
  int disp1;
  
  c4x_S_address_parse (op0, &base0, &incdec0, &index0, &disp0);
  c4x_S_address_parse (op1, &base1, &incdec1, &index1, &disp1);

  if (store0 && store1)
    {
      /* If we have two stores in parallel to the same address, then
	 the C4x only executes one of the stores.  This is unlikely to
	 cause problems except when writing to a hardware device such
	 as a FIFO since the second write will be lost.  The user
	 should flag the hardware location as being volatile so that
	 we don't do this optimisation.  While it is unlikely that we
	 have an aliased address if both locations are not marked
	 volatile, it is probably safer to flag a potential conflict
	 if either location is volatile.  */
      if (!TARGET_ALIASES)
	{
	  if (MEM_VOLATILE_P (op0) && MEM_VOLATILE_P (op1))
	    return 1;
	}
      else
	{
	  if (MEM_VOLATILE_P (op0) || MEM_VOLATILE_P (op1))
	    return 1;
	}
    }

  /* If have a parallel load and a store to the same address, the load
     is performed first, so there is no conflict.  Similarly, there is
     no conflict if have parallel loads from the same address.  */

  /* Cannot use auto increment or auto decrement twice for same
     base register.  */
  if (base0 == base1 && incdec0 && incdec0)
    return 1;

  /* It might be too confusing for GCC if we have use a base register
     with a side effect and a memory reference using the same register
     in parallel.  */
  if (!TARGET_DEVEL && base0 == base1 && (incdec0 || incdec1))
    return 1;

  /* We can not optimize the case where op1 and op2 refer to the same
     address. */
  if (base0 == base1 && disp0 == disp1 && index0 == index1)
    return 1;

  /* No conflict.  */
  return 0;
}


/* Check for while loop inside a decrement and branch loop.  */

int
c4x_label_conflict (insn, jump, db)
     rtx insn;
     rtx jump;
     rtx db;
{
  while (insn)
    {
      if (GET_CODE (insn) == CODE_LABEL)
	{
          if (CODE_LABEL_NUMBER (jump) == CODE_LABEL_NUMBER (insn))
	    return 1;
          if (CODE_LABEL_NUMBER (db) == CODE_LABEL_NUMBER (insn))
	    return 0;
	}
      insn = PREV_INSN (insn);
    }
  return 1;
}


/* Validate combination of operands for parallel load/store instructions.  */

int
valid_parallel_operands_4 (operands, mode)
     rtx *operands;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx op3 = operands[3];

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);
  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);
  if (GET_CODE (op2) == SUBREG)
    op2 = SUBREG_REG (op2);
  if (GET_CODE (op3) == SUBREG)
    op3 = SUBREG_REG (op3);

  /* The patterns should only allow ext_low_reg_operand() or
     par_ind_operand() operands.  Thus of the 4 operands, only 2
     should be REGs and the other 2 should be MEMs.  */

  /* LDI||LDI  */
  if (GET_CODE (op0) == REG && GET_CODE (op2) == REG)
    return (REGNO (op0) != REGNO (op2))
      && GET_CODE (op1) == MEM && GET_CODE (op3) == MEM
      && !c4x_address_conflict (op1, op3, 0, 0);

  /* STI||STI  */
  if (GET_CODE (op1) == REG && GET_CODE (op3) == REG)
    return GET_CODE (op0) == MEM && GET_CODE (op2) == MEM
      && !c4x_address_conflict (op0, op2, 1, 1);

  /* LDI||STI  */
  if (GET_CODE (op0) == REG && GET_CODE (op3) == REG)
    return GET_CODE (op1) == MEM && GET_CODE (op2) == MEM
      && !c4x_address_conflict (op1, op2, 0, 1);

  /* STI||LDI  */
  if (GET_CODE (op1) == REG && GET_CODE (op2) == REG)
    return GET_CODE (op0) == MEM && GET_CODE (op3) == MEM
      && !c4x_address_conflict (op0, op3, 1, 0);

  return 0;
}

/* We only use this to check operands 1 and 2 since these may be
   commutative.  It will need extending for the C32 opcodes.  */
int
valid_parallel_operands_5 (operands, mode)
     rtx *operands;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int regs = 0;
  rtx op0 = operands[1];
  rtx op1 = operands[2];

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);
  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);

  /* The patterns should only allow ext_low_reg_operand() or
     par_ind_operand() operands. */

  if (GET_CODE (op0) == REG)
    regs++;
  if (GET_CODE (op1) == REG)
    regs++;

  return regs == 1;
}


int
valid_parallel_operands_6 (operands, mode)
     rtx *operands;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int regs = 0;
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx op2 = operands[4];
  rtx op3 = operands[5];

  if (GET_CODE (op0) == SUBREG)
    op0 = SUBREG_REG (op0);
  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);
  if (GET_CODE (op2) == SUBREG)
    op2 = SUBREG_REG (op2);
  if (GET_CODE (op3) == SUBREG)
    op3 = SUBREG_REG (op3);

  /* The patterns should only allow ext_low_reg_operand() or
     par_ind_operand() operands.  Thus of the 4 input operands, only 2
     should be REGs and the other 2 should be MEMs.  */

  if (GET_CODE (op0) == REG)
    regs++;
  if (GET_CODE (op1) == REG)
    regs++;
  if (GET_CODE (op2) == REG)
    regs++;
  if (GET_CODE (op3) == REG)
    regs++;

  /* The new C30/C40 silicon dies allow 3 regs of the 4 input operands. 
     Perhaps we should count the MEMs as well?  */
  return regs == 2;
}


int
legitimize_parallel_operands_6 (operands, mode)
     rtx *operands;
     enum machine_mode mode;
{
  /* It's gonna be hard to legitimize operands for a parallel
     instruction... TODO...  */
  return valid_parallel_operands_6 (operands, mode);
}


/* Validate combination of src operands.  Note that the operands have
   been screened by the src_operand predicate.  We just have to check
   that the combination of operands is valid.  If FORCE is set, ensure
   that the destination regno is valid if we have a 2 operand insn.  */

static int
c4x_valid_operands (code, operands, mode, force)
     enum rtx_code code;
     rtx *operands;
     enum machine_mode mode;
     int force;
{
  rtx op1;
  rtx op2;
  enum rtx_code code1;
  enum rtx_code code2;

  if (code == COMPARE)
    {
      op1 = operands[0];
      op2 = operands[1];
    }
  else
    {
      op1 = operands[1];
      op2 = operands[2];
    }

  if (GET_CODE (op1) == SUBREG)
    op1 = SUBREG_REG (op1);
  if (GET_CODE (op2) == SUBREG)
    op2 = SUBREG_REG (op2);

  code1 = GET_CODE (op1);
  code2 = GET_CODE (op2);

  if (code1 == REG && code2 == REG)
    return 1;

  if (code1 == MEM && code2 == MEM)
    {
      if (c4x_S_indirect (op1, mode) && c4x_S_indirect (op2, mode))
	return 1;
      return c4x_R_indirect (op1, mode) && c4x_R_indirect (op2, mode);
    }

  if (code1 == code2)
    return 0;

  if (code1 == REG)
    {
      switch (code2)
	{
	case CONST_INT:
	  if (c4x_J_constant (op2) && c4x_R_indirect (op1))
	    return 1;
	  break;
	  
	case CONST_DOUBLE:
	  if (!c4x_H_constant (op2))
	    return 0;
	  break;

	  /* Any valid memory operand screened by src_operand is OK.  */
  	case MEM:
	  
	  /* After CSE, any remaining (ADDRESSOF:P reg) gets converted
	     into a stack slot memory address comprising a PLUS and a
	     constant.  */
	case ADDRESSOF:
	  break;
	  
	default:
	  fatal ("c4x_valid_operands: Internal error");
	  break;
	}
      
      /* Check that we have a valid destination register for a two operand
	 instruction.  */
      return !force || code == COMPARE || REGNO (op1) == REGNO (operands[0]);
    }

  /* We assume MINUS is commutative since the subtract patterns
     also support the reverse subtract instructions.  Since op1
     is not a register, and op2 is a register, op1 can only
     be a restricted memory operand for a shift instruction.  */
  if (code == ASHIFTRT || code == LSHIFTRT
      || code == ASHIFT || code == COMPARE)
    return code2 == REG
      && (c4x_S_indirect (op1) || c4x_R_indirect (op1));
  
  switch (code1)
    {
    case CONST_INT:
      if (c4x_J_constant (op1) && c4x_R_indirect (op2))
	return 1;
      break;
      
    case CONST_DOUBLE:
      if (!c4x_H_constant (op1))
	return 0;
      break;

      /* Any valid memory operand screened by src_operand is OK. */      
    case MEM:

      /* After CSE, any remaining (ADDRESSOF:P reg) gets converted
	 into a stack slot memory address comprising a PLUS and a
	 constant.  */
    case ADDRESSOF:
      break;
      
    default:
      fatal ("c4x_valid_operands: Internal error");
      break;
    }
      
  /* Check that we have a valid destination register for a two operand
     instruction.  */
  return !force || REGNO (op1) == REGNO (operands[0]);
}


int valid_operands (code, operands, mode)
     enum rtx_code code;
     rtx *operands;
     enum machine_mode mode;
{

  /* If we are not optimizing then we have to let anything go and let
     reload fix things up.  instantiate_decl in function.c can produce
     invalid insns by changing the offset of a memory operand from a
     valid one into an invalid one, when the second operand is also a
     memory operand.  The alternative is not to allow two memory
     operands for an insn when not optimizing.  The problem only rarely
     occurs, for example with the C-torture program DFcmp.c  */

  return !optimize || c4x_valid_operands (code, operands, mode, 0);
}


int
legitimize_operands (code, operands, mode)
     enum rtx_code code;
     rtx *operands;
     enum machine_mode mode;
{
  /* Compare only has 2 operands.  */
  if (code == COMPARE)
    {
      /* During RTL generation, force constants into pseudos so that
	 they can get hoisted out of loops.  This will tie up an extra
	 register but can save an extra cycle.  Only do this if loop
	 optimisation enabled.  (We cannot pull this trick for add and
	 sub instructions since the flow pass won't find
	 autoincrements etc.)  This allows us to generate compare
	 instructions like CMPI R0, *AR0++ where R0 = 42, say, instead
	 of LDI *AR0++, R0; CMPI 42, R0. 

	 Note that expand_binops will try to load an expensive constant
	 into a register if it is used within a loop.  Unfortunately,
	 the cost mechanism doesn't allow us to look at the other
	 operand to decide whether the constant is expensive.  */
      
      if (!reload_in_progress
	  && TARGET_HOIST
	  && optimize > 0
	  && ((GET_CODE (operands[1]) == CONST_INT 
	       && !c4x_J_constant (operands[1])
	       && INTVAL (operands[1]) != 0)
	      || GET_CODE (operands[1]) == CONST_DOUBLE))
	operands[1] = force_reg (mode, operands[1]);
      
      if (!reload_in_progress
          && !c4x_valid_operands (code, operands, mode, 0))
	operands[0] = force_reg (mode, operands[0]);
      return 1;
    }
  
  /* We cannot do this for ADDI/SUBI insns since we will
     defeat the flow pass from finding autoincrement addressing
     opportunities.  */
  if (!reload_in_progress
      && !((code == PLUS || code == MINUS) && mode == Pmode)
      && (TARGET_HOIST && optimize > 1
       && ((GET_CODE (operands[2]) == CONST_INT 
	    && !c4x_J_constant (operands[2])
	    && INTVAL (operands[2]) != 0)
	   || GET_CODE (operands[2]) == CONST_DOUBLE)))
    operands[2] = force_reg (mode, operands[2]);

  /* We can get better code on a C30 if we force constant shift counts
     into a register.  This way they can get hoisted out of loops,
     tying up a register, but saving an instruction.  The downside is
     that they may get allocated to an address or index register, and
     thus we will get a pipeline conflict if there is a nearby
     indirect address using an address register. 

     Note that expand_binops will not try to load an expensive constant
     into a register if it is used within a loop for a shift insn.  */
  
  if (!reload_in_progress
      && !c4x_valid_operands (code, operands, mode, TARGET_FORCE))
    {
      /* If the operand combination is invalid, we force operand1 into a
         register, preventing reload from having doing to do this at a
         later stage.  */
      operands[1] = force_reg (mode, operands[1]);
      if (TARGET_FORCE)
	{
	  emit_move_insn (operands[0], operands[1]);
	  operands[1] = copy_rtx (operands[0]);
	}
      else
	{
	  /* Just in case...  */
	  if (!c4x_valid_operands (code, operands, mode, 0))
	    operands[2] = force_reg (mode, operands[2]);
	}
    }

  /* Right shifts require a negative shift count, but GCC expects
     a positive count, so we emit a NEG.  */
  if ((code == ASHIFTRT || code == LSHIFTRT)
      && (GET_CODE (operands[2]) != CONST_INT))
    operands[2] = gen_rtx_NEG (mode, negate_rtx (mode, operands[2]));
  
  return 1;
}


/* The following predicates are used for instruction scheduling.  */

int
group1_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && IS_GROUP1_REG (REGNO (op));
}


int
group1_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_CODE (op) == MEM)
    {
      op = XEXP (op, 0);
      if (GET_CODE (op) == PLUS)
	{
	  rtx op0 = XEXP (op, 0);
	  rtx op1 = XEXP (op, 1);

	  if (((GET_CODE (op0) == REG) && IS_GROUP1_REGNO (op0))
	      || ((GET_CODE (op1) == REG) && IS_GROUP1_REGNO (op1)))
	    return 1;
	}
      else if ((REG_P (op)) && IS_GROUP1_REGNO (op))
	return 1;
    }

  return 0;
}


/* Return true if any one of the address registers.  */

int
arx_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && IS_ADDR_REGNO (op);
}


static int
c4x_arn_reg_operand (op, mode, regno)
     rtx op;
     enum machine_mode mode;
     int regno;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return REG_P (op) && (REGNO (op) == regno);
}


static int
c4x_arn_mem_operand (op, mode, regno)
     rtx op;
     enum machine_mode mode;
     int regno;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  if (GET_CODE (op) == MEM)
    {
      op = XEXP (op, 0);
      switch (GET_CODE (op))
	{
	case PRE_DEC:
	case POST_DEC:
	case PRE_INC:
	case POST_INC:
	  op = XEXP (op, 0);

	case REG:
          if (REG_P (op) && (REGNO (op) == regno))
	    return 1;
	  break;

	case PRE_MODIFY:
	case POST_MODIFY:
          if (REG_P (XEXP (op, 0)) && (REGNO (XEXP (op, 0)) == regno))
	    return 1;
          if (REG_P (XEXP (XEXP (op, 1), 1))
	      && (REGNO (XEXP (XEXP (op, 1), 1)) == regno))
	    return 1;
	  break;

	case PLUS:
	  {
	    rtx op0 = XEXP (op, 0);
	    rtx op1 = XEXP (op, 1);

	    if (((GET_CODE (op0) == REG) && (REGNO (op0) == regno)) 
	        || ((GET_CODE (op1) == REG) && (REGNO (op1) == regno)))
	      return 1;
	  }
	  break;
	default:
	  break;
	}
    }
  return 0;
}


int
ar0_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR0_REGNO);
}


int
ar0_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR0_REGNO);
}


int
ar1_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR1_REGNO);
}


int
ar1_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR1_REGNO);
}


int
ar2_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR2_REGNO);
}


int
ar2_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR2_REGNO);
}


int
ar3_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR3_REGNO);
}


int
ar3_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR3_REGNO);
}


int
ar4_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR4_REGNO);
}


int
ar4_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR4_REGNO);
}


int
ar5_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR5_REGNO);
}


int
ar5_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR5_REGNO);
}


int
ar6_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR6_REGNO);
}


int
ar6_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR6_REGNO);
}


int
ar7_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, AR7_REGNO);
}


int
ar7_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, AR7_REGNO);
}


int
ir0_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, IR0_REGNO);
}


int
ir0_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, IR0_REGNO);
}


int
ir1_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_reg_operand (op, mode, IR1_REGNO);
}


int
ir1_mem_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return c4x_arn_mem_operand (op, mode, IR1_REGNO);
}


/* We allow autoincrement addressing.  */

rtx
c4x_operand_subword (op, i, validate_address, mode)
     rtx op;
     int i;
     int validate_address;
     enum machine_mode mode;
{
  if (mode != HImode && mode != HFmode)
    fatal_insn ("c4x_operand_subword: invalid mode", op);

  if (mode == HFmode && REG_P (op))
    fatal_insn ("c4x_operand_subword: invalid operand", op);

  if (GET_CODE (op) == MEM)
    {
      enum rtx_code code = GET_CODE (XEXP (op, 0));
      enum machine_mode mode = GET_MODE (XEXP (op, 0));

      switch (code)
	{
	case POST_INC:
	case PRE_INC:
	  if (mode == HImode)
	    mode = QImode;
	  else if (mode == HFmode)
	    mode = QFmode;
	  return gen_rtx_MEM (mode, XEXP (op, 0));
	  
	case POST_DEC:
	case PRE_DEC:
	case PRE_MODIFY:
	case POST_MODIFY:
	  /* We could handle these with some difficulty.
	     e.g., *p-- => *(p-=2); *(p+1).  */
	  fatal_insn ("c4x_operand_subword: invalid autoincrement", op);

	default:
	  break;
	}
    }
  
  return operand_subword (op, i, validate_address, mode);
}

/* Handle machine specific pragmas for compatibility with existing
   compilers for the C3x/C4x.

   pragma				   attribute
   ----------------------------------------------------------
   CODE_SECTION(symbol,"section")          section("section")
   DATA_SECTION(symbol,"section")          section("section")
   FUNC_CANNOT_INLINE(function)            
   FUNC_EXT_CALLED(function)               
   FUNC_IS_PURE(function)                  const
   FUNC_IS_SYSTEM(function)                
   FUNC_NEVER_RETURNS(function)            noreturn
   FUNC_NO_GLOBAL_ASG(function)            
   FUNC_NO_IND_ASG(function)               
   INTERRUPT(function)                     interrupt

   */

int
c4x_handle_pragma (p_getc, p_ungetc, pname)
     int (*  p_getc) PROTO ((void));
     void (* p_ungetc) PROTO ((int)) ATTRIBUTE_UNUSED;
     char *pname;
{
  int i;
  int c;
  int namesize;
  char *name;
  tree func;
  tree sect = NULL_TREE;
  tree new;

  c = p_getc ();
  while (c == ' ' || c == '\t') c = p_getc ();
  if (c != '(')
    return 0;

  c = p_getc ();
  while (c == ' ' || c == '\t') c = p_getc ();
  if (!(isalpha(c) || c == '_' || c == '$' || c == '@'))
    return 0;

  i = 0;
  namesize = 16;
  name = xmalloc (namesize);
  while (isalnum (c) || c == '_' || c == '$' || c == '@')
    {
      if (i >= namesize-1)
	{
	  namesize += 16;
	  name = xrealloc (name, namesize);
	}
      name[i++] = c;
      c = p_getc ();
    }
  name[i] = 0;
  func = get_identifier (name);
  free (name);
  
  if (strcmp (pname, "CODE_SECTION") == 0
      || strcmp (pname, "DATA_SECTION") == 0)
    {
      while (c == ' ' || c == '\t') c = p_getc ();
      if (c != ',')
        return 0;

      c = p_getc ();
      while (c == ' ' || c == '\t') c = p_getc ();
      if (c != '"')
        return 0;

      i = 0;
      namesize = 16;
      name = xmalloc (namesize);
      c = p_getc ();
      while (c != '"' && c != '\n' && c != '\r' && c != EOF)
        {
          if (i >= namesize-1)
	    {
	      namesize += 16;
	      name = xrealloc (name, namesize);
	    }
          name[i++] = c;
          c = p_getc ();
        }
      name[i] = 0;
      sect = build_string (i, name);
      free (name);
      sect = build_tree_list (NULL_TREE, sect);
      
      if (c != '"')
        return 0;
      c = p_getc ();
    }
  while (c == ' ' || c == '\t') c = p_getc ();
  if (c != ')')
    return 0;
  
  new = build_tree_list (func, sect);
  if (strcmp (pname, "CODE_SECTION") == 0)
    code_tree = chainon (code_tree, new);
  
  else if (strcmp (pname, "DATA_SECTION") == 0)
    data_tree = chainon (data_tree, new);
  
  else if (strcmp (pname, "FUNC_CANNOT_INLINE") == 0)
      ; /* ignore */
  
  else if (strcmp (pname, "FUNC_EXT_CALLED") == 0)
      ; /* ignore */
  
  else if (strcmp (pname, "FUNC_IS_PURE") == 0)
     pure_tree = chainon (pure_tree, new);
  
  else if (strcmp (pname, "FUNC_IS_SYSTEM") == 0)
      ; /* ignore */
  
  else if (strcmp (pname, "FUNC_NEVER_RETURNS") == 0)
    noreturn_tree = chainon (noreturn_tree, new);
  
  else if (strcmp (pname, "FUNC_NO_GLOBAL_ASG") == 0)
      ; /* ignore */
  
  else if (strcmp (pname, "FUNC_NO_IND_ASG") == 0)
      ; /* ignore */
  
  else if (strcmp (pname, "INTERRUPT") == 0)
    interrupt_tree = chainon (interrupt_tree, new);
  
  else
    return 0;
  
  return 1;
}


static void
c4x_check_attribute(attrib, list, decl, attributes)
     char *attrib;
     tree list, decl, *attributes;
{
  while (list != NULL_TREE
         && IDENTIFIER_POINTER (TREE_PURPOSE (list)) !=
	 IDENTIFIER_POINTER (DECL_NAME (decl)))
    list = TREE_CHAIN(list);
  if (list)
    *attributes = chainon (*attributes,
			   build_tree_list (get_identifier (attrib),
					    TREE_VALUE(list)));
}


void
c4x_set_default_attributes(decl, attributes)
     tree decl, *attributes;
{
  switch (TREE_CODE (decl))
    {
    case FUNCTION_DECL:
      c4x_check_attribute ("section", code_tree, decl, attributes);
      c4x_check_attribute ("const", pure_tree, decl, attributes);
      c4x_check_attribute ("noreturn", noreturn_tree, decl, attributes);
      c4x_check_attribute ("interrupt", interrupt_tree, decl, attributes);
      break;

    case VAR_DECL:
      c4x_check_attribute ("section", data_tree, decl, attributes);
      break;

    default:
      break;
    }
}


/* Return nonzero if IDENTIFIER with arguments ARGS is a valid machine
   specific attribute for TYPE.  The attributes in ATTRIBUTES have
   previously been assigned to TYPE.  */

int
c4x_valid_type_attribute_p (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args ATTRIBUTE_UNUSED;
{
  if (TREE_CODE (type) != FUNCTION_TYPE)
    return 0;
  
  if (is_attribute_p ("interrupt", identifier))
    return 1;
  
  if (is_attribute_p ("assembler", identifier))
    return 1;
  
  if (is_attribute_p ("leaf_pretend", identifier))
    return 1;
  
  return 0;
}


/* This is a modified version of modified_between_p that doesn't give
   up if a changing MEM is found.  It checks all insns between START
   and END to see if any registers mentioned in X are set. */
static int
c4x_modified_between_p (x, start, end)
     rtx x;
     rtx start, end;
{
  enum rtx_code code = GET_CODE (x);
  char *fmt;
  int i, j;

  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 0;

    case PC:
    case CC0:
      return 1;

    case MEM:
      break;

    case REG:
      return reg_set_between_p (x, start, end);
      
    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && c4x_modified_between_p (XEXP (x, i), start, end))
	return 1;

      if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (c4x_modified_between_p (XVECEXP (x, i, j), start, end))
	    return 1;
    }

  return 0;
}

/* Return 1 if rtx X references memory that is changing.  */
static int
c4x_mem_ref_p (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  char *fmt;
  int i, j;

  if (code == MEM && !RTX_UNCHANGING_P (x))
    return 1;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && c4x_mem_ref_p (XEXP (x, i)))
	return 1;

      if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (c4x_mem_ref_p (XVECEXP (x, i, j)))
	    return 1;
    }

  return 0;
}

/* Return 1 if rtx X sets or clobbers memory.  */
static int
c4x_mem_set_p (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  char *fmt;
  int i, j;

  if ((code == SET || code == CLOBBER)
      && (GET_CODE (SET_DEST (x)) == MEM))
    return 1;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && c4x_mem_set_p (XEXP (x, i)))
	return 1;

      if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if (c4x_mem_set_p (XVECEXP (x, i, j)))
	    return 1;
    }

  return 0;
}


/* Return 1 if any insns between START and END (exclusive) sets
   or clobbers memory.  */
static int
c4x_mem_modified_between_p (start, end)
     rtx start, end;
{
  rtx insn;

  if (start == end)
    return 0;

  for (insn = NEXT_INSN (start); insn != end; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& c4x_mem_set_p (PATTERN (insn)))
      return 1;
  return 0;
}


/* Returns 1 if INSN can be moved past all the insns between START and
   END exclusive.  If TARGET_ALIASES is not set and a memory store is
   detected, then 0 is returned.  */
static int
c4x_insn_moveable_p (insn, start, end)
     rtx insn;
     rtx start, end;
{
  if (start == end)
    return 1;

  /* We can't use modified_between_p since this will
     return 1 if set1 contains a MEM.  */
  if (c4x_modified_between_p (insn, start, end))
    return 0;

  return 1;
}


/* See if the insns INSN1 and INSN2 can be packed into a PARALLEL.
   Return 0 if the insns cannot be packed or the rtx of the packed
   insn (with clobbers added as necessary).  If DEPEND is non zero,
   then the destination register of INSN1 must be used by INSN2.  */
static rtx
c4x_parallel_pack (insn1, insn2, depend)
     rtx insn1;
     rtx insn2;
     int depend;
{
  rtx set1;
  rtx set2;
  rtx pack;
  enum machine_mode mode1;
  enum machine_mode mode2;
  int num_clobbers;
  int insn_code_number;

  /* We could generalise things to not just rely on single sets.  */
  if (!(set1 = single_set (insn1))
      || !(set2 = single_set (insn2)))
    return 0;

  mode1 = GET_MODE (SET_DEST (set1));
  mode2 = GET_MODE (SET_DEST (set2));
  if (mode1 != mode2)
    return 0;

  if (depend)
    {
      rtx dst1;

      /* Require insn2 to be dependent upon the result of insn1.  */
      dst1 = SET_DEST (set1);
      
      if (!REG_P (dst1))
	return 0;

      if (!reg_mentioned_p (dst1, set2))
	return 0;

      /* The dependent register must die in insn2 since a parallel
	 insn will generate a new value.  */
      if (!find_regno_note (insn2, REG_DEAD, REGNO (dst1)))
	return 0;
    }

  pack = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set1, set2));
  num_clobbers = 0;
  if ((insn_code_number = recog (pack, pack, &num_clobbers)) < 0)
    return 0;

  if (num_clobbers != 0)
    {
      rtx newpack;
      int i;

      newpack = gen_rtx_PARALLEL (VOIDmode,
				  gen_rtvec (GET_CODE (pack) == PARALLEL
				    ? XVECLEN (pack, 0) + num_clobbers
				    : num_clobbers + 1));

      if (GET_CODE (pack) == PARALLEL)
	for (i = 0; i < XVECLEN (pack, 0); i++)
	  XVECEXP (newpack, 0, i) = XVECEXP (pack, 0, i);
      else
	XVECEXP (newpack, 0, 0) = pack;

      add_clobbers (newpack, insn_code_number);
      pack = newpack;
    }

  return pack;
}


static rtx
c4x_parallel_find (insn1, loop_end, depend, insn2)
     rtx insn1;
     rtx loop_end;
     int depend;
     rtx *insn2;
{
  rtx insn;
  rtx pack;

  /* We could use the logical links if depend is non zero?  */

  for (insn = NEXT_INSN (insn1); insn != loop_end; insn = NEXT_INSN(insn))
    {
      switch (GET_CODE (insn))
	{
	default:
	case JUMP_INSN:
	case CALL_INSN:
	case NOTE:
	  break;

	case INSN:
	  if (!(pack = c4x_parallel_pack (insn1, insn, depend)))
	    break;

	  /* What if insn1 or insn2 sets cc and is required by another
	     insn?  */

#if 0
	  /* Check that nothing between insn1 and insn will spoil the
	     show.  */
	  if (NEXT_INSN (insn1) != insn 
	      && c4x_modified_between_p (insn, NEXT_INSN (insn1), insn))
	    return 0;
#else
	  /* This will do in the interim.  If the insns between
	     insn1 and insn are harmless, we can move things around
	     if we're careful.  */
	  if (next_nonnote_insn (insn1) != insn)
	    return 0;
#endif
	  
	  /* Do some checks here... */
	  *insn2 = insn;
	  return pack;
	}
    }
  return 0;
}


/* Update the register info for reg REG found in the basic block BB,
   where SET is 1 if the register is being set.  */
static void
c4x_update_info_reg (reg, set, bb)
     rtx reg;
     int set;
     int bb;
{
  int regno;

  if (!REG_P (reg))
    fatal_insn ("Expecting register rtx", reg);

  regno = REGNO (reg);

  /* REGNO_FIRST_UID and REGNO_LAST_UID don't need setting.  */

  SET_REGNO_REG_SET (basic_block_live_at_start[bb], regno);
  REG_BASIC_BLOCK (regno) = REG_BLOCK_GLOBAL;
  if (set)
    REG_N_SETS (regno)++;  
  else
    REG_N_REFS (regno)++;  
}


/* Update the register info for all the regs in X found in the basic
   block BB.  */
static void
c4x_update_info_regs(x, bb)
     rtx x;
     int bb;
{
  enum rtx_code code;
  char *fmt;
  int i, j;

  if (!x)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case CLOBBER:
#if 0
      if (REG_P (SET_DEST (x)))
	return;
      break;
#endif

    case SET:
      if (REG_P (SET_DEST (x)))
	c4x_update_info_reg (SET_DEST (x), 1, bb);
      else
	c4x_update_info_regs (SET_DEST (x), bb);

      if (code == SET)
	c4x_update_info_regs (SET_SRC (x), bb);
      return;

    case REG:
      c4x_update_info_reg (x, 0, bb);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	c4x_update_info_regs (XEXP (x, i), bb);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  c4x_update_info_regs (XVECEXP (x, i, j), bb);
    }
}


static void
c4x_copy_insn_after(insn, prev, bb)
     rtx insn;
     rtx prev;
     int bb;
{
  rtx note;
  rtx new;

  emit_insn_after (copy_rtx (PATTERN (insn)), prev);

  new = NEXT_INSN (prev);

  /* Copy the REG_NOTES from insn to the new insn.  */
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    REG_NOTES (new) = gen_rtx (GET_CODE (note),
			       REG_NOTE_KIND (note),
			       XEXP (note, 0),
			       REG_NOTES (new));

  /* Handle all the registers within insn and update the reg info.  */
  c4x_update_info_regs (PATTERN (insn), bb);
}


static void
c4x_copy_insns_after(start, end, pprev, bb)
     rtx start;
     rtx end;
     rtx *pprev;
     int bb;
{
  rtx insn;

  for (insn = start; insn != NEXT_INSN (end); insn = NEXT_INSN(insn))
    {
      switch (GET_CODE (insn))
	{
	case CALL_INSN:
	  /* We could allow a libcall with no side effects??? */
	  fatal_insn("Repeat block loop contains a call", insn);
	  break;
	  
	case INSN:
	  c4x_copy_insn_after(insn, *pprev, bb - 1);
	  *pprev = NEXT_INSN (*pprev);
	  break;

	default:
	  break;
	}
    }
}


/* Merge the notes of insn2 with the notes of insn.  */
static void
c4x_merge_notes(insn, insn2)
     rtx insn;
     rtx insn2;
{
  rtx note;
	   
  for (note = REG_NOTES (insn2); note; note = XEXP (note, 1))
    {
      rtx link;
      
      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	if (REG_NOTE_KIND (note) == REG_NOTE_KIND (link)
	    && XEXP (note, 0) == XEXP (link, 0))
	  remove_note (insn, note);
    }
  for (note = REG_NOTES (insn2); note; note = XEXP (note, 1))
    REG_NOTES (insn) = gen_rtx (GET_CODE (note),
				REG_NOTE_KIND (note),
				XEXP (note, 0),
				REG_NOTES (insn));
}


/* This pass must update information that subsequent passes expect to be
   correct.  Namely: reg_n_refs, reg_n_sets, reg_n_deaths,
   reg_n_calls_crossed, and reg_live_length.  Also, basic_block_head,
   basic_block_end.   */

static int
c4x_parallel_process (loop_start, loop_end)
     rtx loop_start;
     rtx loop_end;
{
  rtx insn;
  rtx insn2;
  rtx pack;
  rtx hoist_pos;
  rtx sink_pos;
  rtx loop_count;
  rtx loop_count_set;
  rtx loop_count_reg;
  rtx jump_insn;
  rtx end_label;
  int num_packs;
  int bb;

  jump_insn = PREV_INSN (loop_end);

  /* The loop must have a calculable number of iterations
     since we need to reduce the loop count by one.  

     For now, only process repeat block loops, since we can tell that
     these have a calculable number of iterations. 

     The loop count must be at least 2?  */

  loop_count = PREV_INSN (loop_start);
  
  if (!(loop_count_set = single_set (loop_count)))
    return 0;

#if 0
  /* Disable this optimisation until REG_LOOP_COUNT note
     added.  */
  if (!find_reg_note (loop_count, REG_LOOP_COUNT, NULL_RTX))
    return 0;
#else
  return 0;
#endif

  loop_count_reg = SET_DEST (loop_count_set);
		  
  /* Determine places to hoist and sink insns out of the loop. 
     We need to hoist insns before the label at the top of the loop. 
     We'll have to update basic_block_head.  */
  
  /* Place in the rtx where we hoist insns after.  */
  hoist_pos = loop_count;

  /* Place in the rtx where we sink insns after.  */
  sink_pos = loop_end;

  /* There must be an easier way to work out which basic block we are
     in.  */
  for (bb = 0; bb < n_basic_blocks; bb++)
    if (basic_block_head[bb] == NEXT_INSN (loop_end))
      break;

  if (bb >= n_basic_blocks)
    fatal_insn("Cannot find basic block for insn", NEXT_INSN (loop_end));

  /* Skip to label at top of loop.  */
  for (; GET_CODE (loop_start) != CODE_LABEL;
       loop_start = NEXT_INSN(loop_start));
  
  num_packs = 0;
  for (insn = loop_start; insn != loop_end; insn = NEXT_INSN(insn))
    {
      switch (GET_CODE (insn))
	{
	default:
	case JUMP_INSN:
	case CALL_INSN:
	case NOTE:
	  break;

	case INSN:

	  /* Look for potential insns to combine where the second one
	     is dependent upon the first.  We could have another pass
	     that tries combining independent insns but that is not so
	     important.  We could do this afterwards as a more generic
	     peepholer.  */
	     
	  if ((pack = c4x_parallel_find(insn, loop_end, 1, &insn2)))
	    {
	      rtx set1;
	      rtx set2;
	      rtx note;
	      rtx seq_start;

	      set1 = single_set (insn);
	      set2 = single_set (insn2);

	      /* We need to hoist a copy of insn1 out of the loop and
		 to sink a copy insn2 out of the loop.  We can avoid
		 the latter if the destination of insn2 is used
		 by a following insn within the loop.
		 
		 We cannot hoist insn1 out of the loop if any of the
		 preceeding insns within the loop modifies the destination
		 of insn1 or modifies any of the operands of insn1.  */

	      /* If the user has flagged that there are potential aliases,
		 then we can't move the insn if it references memory
		 past any insns that modify memory.  */
	      if (TARGET_ALIASES 
		  && c4x_mem_ref_p (PATTERN (insn))
		  && c4x_mem_modified_between_p (loop_start, loop_end))
		break;

	      /* None of the registers used in insn can be modified by
		 any of the insns from the start of the loop until insn.  */
	      if (!c4x_insn_moveable_p (set1, loop_start, insn))
		break;

	      /* None of the registers used in insn can be modified by
		 any of the insns after insn2 until the end of the
		 loop, especially the result which needs to be saved
		 for the next iteration. */
	      if (!c4x_insn_moveable_p (set1, insn2, loop_end))
		break;

	      /* We need to hoist all the insns from the loop top
		 to and including insn.  */
	      c4x_copy_insns_after (NEXT_INSN (loop_start), insn,
				    &hoist_pos, bb);

	      /* We need to sink all the insns after insn to 
		 loop_end.  */
	      c4x_copy_insns_after (NEXT_INSN (insn), PREV_INSN (jump_insn),
				    &sink_pos, bb + 1);

	      /* Change insn to the new parallel insn, retaining the notes
		 of the old insn.  */
	      if (!validate_change (insn, &PATTERN (insn), pack, 0))
		fatal_insn("Cannot replace insn with parallel insn", pack);

	      /* Copy the REG_NOTES from insn2 to the new insn
		 avoiding duplicates.  */
	      c4x_merge_notes (insn, insn2);

	      delete_insn (insn2);
	      
	      /* The destination register of insn1 no longer dies in
	      this composite insn.  Don't use remove_death since that
	      alters REG_N_DEATHS.  The REG_DEAD note has just been
	      moved.  */
	      note = find_regno_note (insn, REG_DEAD, REGNO (SET_DEST (set1)));
	      if (note)
		remove_note (insn, note);

	      /* ??? Do we have to modify the LOG_LINKS?  */

	      /* We need to decrement the loop count.  We probably
		 should test if the loop count is negative and branch
		 to end label if so.  */
	      if (GET_CODE (SET_SRC (loop_count_set)) == CONST_INT)
		{
		  /* The loop count must be more than 1 surely?  */
		  SET_SRC (loop_count_set) 
		    = GEN_INT (INTVAL (SET_SRC (loop_count_set)) - 1);
		}
	      else if (GET_CODE (SET_SRC (loop_count_set)) == PLUS
		       && GET_CODE (XEXP (SET_SRC (loop_count_set), 1))
		       == CONST_INT)
		{
		  XEXP (SET_SRC (loop_count_set), 1)
		    = GEN_INT (INTVAL (XEXP (SET_SRC (loop_count_set), 1))
			       - 1);
		}
	      else
		{
		  start_sequence ();
		  expand_binop (QImode, sub_optab, loop_count_reg,
				GEN_INT (1), loop_count_reg,		
				1, OPTAB_DIRECT);
		  seq_start = get_insns ();
		  end_sequence ();
		  emit_insns_after (seq_start, loop_count);

		  /* Check this.  What if we emit more than one insn?
		     Can we emit more than one insn? */
		  REG_NOTES (seq_start)
		    = gen_rtx_EXPR_LIST (REG_UNUSED,
			       loop_count_reg,
			       REG_NOTES (seq_start));
		}

	      if (GET_CODE (SET_SRC (loop_count_set)) != CONST_INT)
		{
		  end_label = gen_label_rtx();
		  start_sequence ();
		  emit_cmp_insn (loop_count_reg,
				 const0_rtx, LT, NULL_RTX, word_mode, 0, 0);
		  emit_jump_insn (gen_blt (end_label));
		  seq_start = get_insns ();
		  end_sequence ();
		  emit_insns_after (seq_start, hoist_pos);
		  emit_label_after (end_label, sink_pos);
		 
#if 0 
		  /* This is a bit of a hack...but why was it necessary?  */
		  REG_NOTES (NEXT_INSN (seq_start))
		    = gen_rtx_EXPR_LIST (REG_DEAD,
					 loop_count_reg,
					 REG_NOTES (NEXT_INSN (seq_start)));
#endif
		}

	      if (TARGET_DEVEL)
		debug_rtx(insn);

	      num_packs ++;
	      
#if 1
	      /* If we want to pack more than one parallel insn
		 we will have to tag which insns have been
		 hoisted/sunk/paired.  We might need a recursive approach. */
	      
	      return num_packs;
#endif
	    }
	  break;
	}
    }
  return num_packs;
}


static void
c4x_combine_parallel_independent (insns)
     rtx insns ATTRIBUTE_UNUSED;
{
  /* Combine independent insns like
     (set (mem (reg 0)) (reg 1))
     (set (reg 2) (mem (reg 3)))
     where (reg 1) != (reg 2) unless there is a REG_DEAD note
     on the first insn. */

}

static void
c4x_combine_parallel_dependent (insns)
     rtx insns;
{
  rtx insn;
  rtx loop_start;
  rtx loop_end;
  int num_jumps;
  int num_insns;

   /* Find the innermost loop and check that it is unjumped.  */
  loop_start = NULL_RTX;
  num_jumps = 0;
  for (insn = insns; insn; insn = NEXT_INSN(insn))
    {
      switch (GET_CODE (insn))
	{
	case INSN:
	  num_insns++;
	  break;

	case CALL_INSN:
	  /* We could allow a libcall with no side effects??? */
	case JUMP_INSN:
	  num_jumps++;
	  break;

	case NOTE:
	  switch (NOTE_LINE_NUMBER (insn))
	    {
	    case NOTE_INSN_LOOP_BEG:
	      loop_start = insn;
	      num_jumps = 0;
	      num_insns = 0;
	      break;

	    case NOTE_INSN_LOOP_CONT:
	      if (!loop_start)
		break;
	      /* We can't handle a loop with jumps or calls.
		 If there are too many insns, we are unlikely
		 to be able to find a suitable case for optimisation.
		 The maximum number of insns may require tweaking.  */
	      if (!num_jumps && num_insns < 20)
		{
		  /* Skip to end of loop.  */
		  loop_end = NULL_RTX;
		  for (; insn; insn = NEXT_INSN(insn))
		    if (GET_CODE (insn) == NOTE
			&& NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
		      break;
		  loop_end = insn;
		  if (!loop_end)
		    fatal_insn("Could not find note at end of loop", 
			       loop_start);
		  c4x_parallel_process(loop_start, loop_end);
		}
	      loop_start = NULL_RTX;
	      break;

	    default:
	      break;
	    }
	default:
	  break;
	}
    }
}


void
c4x_combine_parallel (insns)
     rtx insns;
{
 /* Only let people who know how to shoot themselves in the foot do so!  */
  if (!TARGET_PARALLEL_PACK)
    return;
  
  c4x_combine_parallel_dependent (insns);

  c4x_combine_parallel_independent (insns);
}


/* !!! FIXME to emit RPTS correctly.  */
int
c4x_rptb_rpts_p (insn, op)
     rtx insn, op;
{
  /* The next insn should be our label marking where the
     repeat block starts.  */
  insn = NEXT_INSN (insn);
  if (GET_CODE (insn) != CODE_LABEL)
    {
      /* Some insns may have been shifted between the RPTB insn
         and the top label... They were probably destined to
         be moved out of the loop.  For now, let's leave them
         where they are and print a warning.  We should
         probably move these insns before the repeat block insn.  */
      if (TARGET_DEBUG)
	fatal_insn("c4x_rptb_rpts_p: Repeat block top label moved\n",
		   insn);
      return 0;
    }

  /* Skip any notes.  */
  insn = next_nonnote_insn (insn);

  /* This should be our first insn in the loop.  */
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  /* Skip any notes.  */
  insn = next_nonnote_insn (insn);

  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  if (recog_memoized (insn) != CODE_FOR_rptb_end)
    return 0;

  if (TARGET_RPTS)
    return 1;

  return (GET_CODE (op) == CONST_INT) && TARGET_RPTS_CYCLES (INTVAL (op));
}


/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost. 
   A set of an address register followed by a use occurs a 2 cycle
   stall (reduced to a single cycle on the c40 using LDA), while
   a read of an address register followed by a use occurs a single cycle.  */
#define	SET_USE_COST	3
#define	SETLDA_USE_COST	2
#define	READ_USE_COST	2

int
c4x_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn;
     int cost;
{
  /* Don't worry about this until we know what registers have been
     assigned.  */
  if (!reload_completed)
    return 0;

  /* How do we handle dependencies where a read followed by another
     read causes a pipeline stall?  For example, a read of ar0 followed
     by the use of ar0 for a memory reference.  It looks like we
     need to extend the scheduler to handle this case.  */

  /* Reload sometimes generates a CLOBBER of a stack slot, e.g.,
     (clobber (mem:QI (plus:QI (reg:QI 11 ar3) (const_int 261)))),
     so only deal with insns we know about.  */
  if (recog_memoized (dep_insn) < 0)
    return 0;

  if (REG_NOTE_KIND (link) == 0)
    {
      int max = 0;

      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      if (TARGET_C3X)
	{
	  if (get_attr_setgroup1 (dep_insn) && get_attr_usegroup1 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_readarx (dep_insn) && get_attr_usegroup1 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;
	}
      else
	{
	  /* This could be significantly optimized. We should look
	     to see if dep_insn sets ar0-ar7 or ir0-ir1 and if
	     insn uses ar0-ar7.  We then test if the same register
	     is used.  The tricky bit is that some operands will
	     use several registers...  */

	  if (get_attr_setar0 (dep_insn) && get_attr_usear0 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar0 (dep_insn) && get_attr_usear0 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar0 (dep_insn) && get_attr_usear0 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar1 (dep_insn) && get_attr_usear1 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar1 (dep_insn) && get_attr_usear1 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar1 (dep_insn) && get_attr_usear1 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar2 (dep_insn) && get_attr_usear2 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar2 (dep_insn) && get_attr_usear2 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar2 (dep_insn) && get_attr_usear2 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar3 (dep_insn) && get_attr_usear3 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar3 (dep_insn) && get_attr_usear3 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar3 (dep_insn) && get_attr_usear3 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar4 (dep_insn) && get_attr_usear4 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar4 (dep_insn) && get_attr_usear4 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar4 (dep_insn) && get_attr_usear4 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar5 (dep_insn) && get_attr_usear5 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar5 (dep_insn) && get_attr_usear5 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar5 (dep_insn) && get_attr_usear5 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar6 (dep_insn) && get_attr_usear6 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar6 (dep_insn) && get_attr_usear6 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar6 (dep_insn) && get_attr_usear6 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setar7 (dep_insn) && get_attr_usear7 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ar7 (dep_insn) && get_attr_usear7 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	  if (get_attr_readar7 (dep_insn) && get_attr_usear7 (insn))
	    max = READ_USE_COST > max ? READ_USE_COST : max;

	  if (get_attr_setir0 (dep_insn) && get_attr_useir0 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ir0 (dep_insn) && get_attr_useir0 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;

	  if (get_attr_setir1 (dep_insn) && get_attr_useir1 (insn))
	    max = SET_USE_COST > max ? SET_USE_COST : max;
	  if (get_attr_setlda_ir1 (dep_insn) && get_attr_useir1 (insn))
	    max = SETLDA_USE_COST > max ? SETLDA_USE_COST : max;
	}

      if (max)
	cost = max;

      /* For other data dependencies, the default cost specified in the
	 md is correct.  */
      return cost;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_ANTI)
    {
      /* Anti dependency; DEP_INSN reads a register that INSN writes some
	 cycles later.  */

      /* For c4x anti dependencies, the cost is 0.  */
      return 0;
    }
  else if (REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
    {
      /* Output dependency; DEP_INSN writes a register that INSN writes some
	 cycles later.  */

      /* For c4x output dependencies, the cost is 0.  */
      return 0;
    }
  else
    abort ();
}
