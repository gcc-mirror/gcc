/* Subroutines used for code generation on IBM RS/6000.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 
   2000 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "obstack.h"
#include "tree.h"
#include "expr.h"
#include "except.h"
#include "function.h"
#include "output.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"
#include "tm_p.h"

#ifndef TARGET_NO_PROTOTYPE
#define TARGET_NO_PROTOTYPE 0
#endif

extern int profile_block_flag;

#define min(A,B)	((A) < (B) ? (A) : (B))
#define max(A,B)	((A) > (B) ? (A) : (B))

/* Target cpu type */

enum processor_type rs6000_cpu;
struct rs6000_cpu_select rs6000_select[3] =
{
  /* switch		name,			tune	arch */
  { (const char *)0,	"--with-cpu=",		1,	1 },
  { (const char *)0,	"-mcpu=",		1,	1 },
  { (const char *)0,	"-mtune=",		1,	0 },
};

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

/* Label number of label created for -mrelocatable, to call to so we can
   get the address of the GOT section */
int rs6000_pic_labelno;

#ifdef USING_SVR4_H
/* Which abi to adhere to */
const char *rs6000_abi_name = RS6000_ABI_NAME;

/* Semantics of the small data area */
enum rs6000_sdata_type rs6000_sdata = SDATA_DATA;

/* Which small data model to use */
const char *rs6000_sdata_name = (char *)0;

/* Counter for labels which are to be placed in .fixup.  */
int fixuplabelno = 0;
#endif

/* Whether a System V.4 varargs area was created.  */
int rs6000_sysv_varargs_p;

/* ABI enumeration available for subtarget to use.  */
enum rs6000_abi rs6000_current_abi;

/* Debug flags */
const char *rs6000_debug_name;
int rs6000_debug_stack;		/* debug stack applications */
int rs6000_debug_arg;		/* debug argument handling */

/* Flag to say the TOC is initialized */
int toc_initialized;
char toc_label_name[10];

/* Alias set for saves and restores from the rs6000 stack.  */
static int rs6000_sr_alias_set;

static void rs6000_add_gc_roots PARAMS ((void));
static int num_insns_constant_wide PARAMS ((HOST_WIDE_INT));
static rtx expand_block_move_mem PARAMS ((enum machine_mode, rtx, rtx));
static int ccr_bit_negated_p PARAMS((rtx));
static void rs6000_emit_stack_tie PARAMS ((void));
static void rs6000_frame_related PARAMS ((rtx, rtx, HOST_WIDE_INT, rtx, rtx));
static void rs6000_emit_allocate_stack PARAMS ((HOST_WIDE_INT, int));
static unsigned rs6000_hash_constant PARAMS ((rtx));
static unsigned toc_hash_function PARAMS ((const void *));
static int toc_hash_eq PARAMS ((const void *, const void *));
static int toc_hash_mark_entry PARAMS ((void *, void *));
static void toc_hash_mark_table PARAMS ((void *));
static int constant_pool_expr_1 PARAMS ((rtx, int *, int *));

/* Default register names.  */
char rs6000_reg_names[][8] =
{
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
     "mq", "lr", "ctr","ap",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "xer"
};

#ifdef TARGET_REGNAMES
static char alt_reg_names[][8] =
{
   "%r0",   "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
   "%r8",   "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
  "%r16",  "%r17", "%r18", "%r19", "%r20", "%r21", "%r22", "%r23",
  "%r24",  "%r25", "%r26", "%r27", "%r28", "%r29", "%r30", "%r31",
   "%f0",   "%f1",  "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7",
   "%f8",   "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",
  "%f16",  "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",
  "%f24",  "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31",
    "mq",    "lr",  "ctr",   "ap",
  "%cr0",  "%cr1", "%cr2", "%cr3", "%cr4", "%cr5", "%cr6", "%cr7",
  "xer"
};
#endif

#ifndef MASK_STRICT_ALIGN
#define MASK_STRICT_ALIGN 0
#endif

/* Override command line options.  Mostly we process the processor
   type and sometimes adjust other TARGET_ options.  */

void
rs6000_override_options (default_cpu)
     const char *default_cpu;
{
  size_t i, j;
  struct rs6000_cpu_select *ptr;

  /* Simplify the entries below by making a mask for any POWER
     variant and any PowerPC variant.  */

#define POWER_MASKS (MASK_POWER | MASK_POWER2 | MASK_MULTIPLE | MASK_STRING)
#define POWERPC_MASKS (MASK_POWERPC | MASK_PPC_GPOPT \
		       | MASK_PPC_GFXOPT | MASK_POWERPC64)
#define POWERPC_OPT_MASKS (MASK_PPC_GPOPT | MASK_PPC_GFXOPT)

  static struct ptt
    {
      const char *name;		/* Canonical processor name.  */
      enum processor_type processor; /* Processor type enum value.  */
      int target_enable;	/* Target flags to enable.  */
      int target_disable;	/* Target flags to disable.  */
    } processor_target_table[]
      = {{"common", PROCESSOR_COMMON, MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_MASKS},
	 {"power", PROCESSOR_POWER,
	    MASK_POWER | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"power2", PROCESSOR_POWER,
	    MASK_POWER | MASK_POWER2 | MASK_MULTIPLE | MASK_STRING,
	    POWERPC_MASKS | MASK_NEW_MNEMONICS},
	 {"power3", PROCESSOR_PPC630,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT},
	 {"powerpc", PROCESSOR_POWERPC,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"powerpc64", PROCESSOR_POWERPC64,
	    MASK_POWERPC | MASK_POWERPC64 | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS},
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
	 {"rs64a", PROCESSOR_RS64A,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS},
	 {"401", PROCESSOR_PPC403,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"403", PROCESSOR_PPC403,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS | MASK_STRICT_ALIGN,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"505", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"601", PROCESSOR_PPC601,
	    MASK_POWER | MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_MULTIPLE | MASK_STRING,
	    MASK_POWER2 | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"602", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"603", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"603e", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"ec603e", PROCESSOR_PPC603,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"604", PROCESSOR_PPC604,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"604e", PROCESSOR_PPC604e,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"620", PROCESSOR_PPC620,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT},
	 {"630", PROCESSOR_PPC630,
	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | MASK_PPC_GPOPT},
	 {"740", PROCESSOR_PPC750,
 	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
 	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"750", PROCESSOR_PPC750,
 	    MASK_POWERPC | MASK_PPC_GFXOPT | MASK_NEW_MNEMONICS,
 	    POWER_MASKS | MASK_PPC_GPOPT | MASK_POWERPC64},
	 {"801", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"821", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"823", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64},
	 {"860", PROCESSOR_MPCCORE,
	    MASK_POWERPC | MASK_SOFT_FLOAT | MASK_NEW_MNEMONICS,
	    POWER_MASKS | POWERPC_OPT_MASKS | MASK_POWERPC64}};

  size_t ptt_size = sizeof (processor_target_table) / sizeof (struct ptt);

  int multiple = TARGET_MULTIPLE;	/* save current -mmultiple/-mno-multiple status */
  int string   = TARGET_STRING;		/* save current -mstring/-mno-string status */

  profile_block_flag = 0;

  /* Identify the processor type */
  rs6000_select[0].string = default_cpu;
  rs6000_cpu = TARGET_POWERPC64 ? PROCESSOR_DEFAULT64 : PROCESSOR_DEFAULT;

  for (i = 0; i < sizeof (rs6000_select) / sizeof (rs6000_select[0]); i++)
    {
      ptr = &rs6000_select[i];
      if (ptr->string != (char *)0 && ptr->string[0] != '\0')
	{
	  for (j = 0; j < ptt_size; j++)
	    if (! strcmp (ptr->string, processor_target_table[j].name))
	      {
		if (ptr->set_tune_p)
		  rs6000_cpu = processor_target_table[j].processor;

		if (ptr->set_arch_p)
		  {
		    target_flags |= processor_target_table[j].target_enable;
		    target_flags &= ~processor_target_table[j].target_disable;
		  }
		break;
	      }

	  if (j == ptt_size)
	    error ("bad value (%s) for %s switch", ptr->string, ptr->name);
	}
    }

  /* If we are optimizing big endian systems for space, use the
     store multiple instructions.  */
  if (BYTES_BIG_ENDIAN && optimize_size)
    target_flags |= MASK_MULTIPLE;

  /* If -mmultiple or -mno-multiple was explicitly used, don't
     override with the processor default */
  if (TARGET_MULTIPLE_SET)
    target_flags = (target_flags & ~MASK_MULTIPLE) | multiple;

  /* If -mstring or -mno-string was explicitly used, don't
     override with the processor default */
  if (TARGET_STRING_SET)
    target_flags = (target_flags & ~MASK_STRING) | string;

  /* Don't allow -mmultiple or -mstring on little endian systems unless the cpu
     is a 750, because the hardware doesn't support the instructions used in
     little endian mode, and causes an alignment trap.  The 750 does not cause
     an alignment trap (except when the target is unaligned).  */

  if (! BYTES_BIG_ENDIAN && rs6000_cpu != PROCESSOR_PPC750)
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

  if (flag_pic && (DEFAULT_ABI == ABI_AIX))
    {
      warning ("-f%s ignored for AIX (all code is position independent)",
	       (flag_pic > 1) ? "PIC" : "pic");
      flag_pic = 0;
    }

  /* Set debug flags */
  if (rs6000_debug_name)
    {
      if (! strcmp (rs6000_debug_name, "all"))
	rs6000_debug_stack = rs6000_debug_arg = 1;
      else if (! strcmp (rs6000_debug_name, "stack"))
	rs6000_debug_stack = 1;
      else if (! strcmp (rs6000_debug_name, "arg"))
	rs6000_debug_arg = 1;
      else
	error ("Unknown -mdebug-%s switch", rs6000_debug_name);
    }

#ifdef TARGET_REGNAMES
  /* If the user desires alternate register names, copy in the alternate names
     now.  */
  if (TARGET_REGNAMES)
    bcopy ((char *)alt_reg_names, (char *)rs6000_reg_names,
	   sizeof (rs6000_reg_names));
#endif

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Register global variables with the garbage collector.  */
  rs6000_add_gc_roots ();

  /* Allocate an alias set for register saves & restores from stack.  */
  rs6000_sr_alias_set = new_alias_set ();

  if (TARGET_TOC) 
    ASM_GENERATE_INTERNAL_LABEL (toc_label_name, "LCTOC", 1);
}

void
optimization_options (level, size)
     int level;
     int size ATTRIBUTE_UNUSED;
{
#ifdef HAVE_decrement_and_branch_on_count
  /* When optimizing, enable use of BCT instruction.  */
  if (level >= 1)
      flag_branch_on_count_reg = 1;
#endif
}

/* Do anything needed at the start of the asm file.  */

void
rs6000_file_start (file, default_cpu)
     FILE *file;
     const char *default_cpu;
{
  size_t i;
  char buffer[80];
  const char *start = buffer;
  struct rs6000_cpu_select *ptr;

  if (flag_verbose_asm)
    {
      sprintf (buffer, "\n%s rs6000/powerpc options:", ASM_COMMENT_START);
      rs6000_select[0].string = default_cpu;

      for (i = 0; i < sizeof (rs6000_select) / sizeof (rs6000_select[0]); i++)
	{
	  ptr = &rs6000_select[i];
	  if (ptr->string != (char *)0 && ptr->string[0] != '\0')
	    {
	      fprintf (file, "%s %s%s", start, ptr->name, ptr->string);
	      start = "";
	    }
	}

#ifdef USING_SVR4_H
      switch (rs6000_sdata)
	{
	case SDATA_NONE: fprintf (file, "%s -msdata=none", start); start = ""; break;
	case SDATA_DATA: fprintf (file, "%s -msdata=data", start); start = ""; break;
	case SDATA_SYSV: fprintf (file, "%s -msdata=sysv", start); start = ""; break;
	case SDATA_EABI: fprintf (file, "%s -msdata=eabi", start); start = ""; break;
	}

      if (rs6000_sdata && g_switch_value)
	{
	  fprintf (file, "%s -G %d", start, g_switch_value);
	  start = "";
	}
#endif

      if (*start == '\0')
	putc ('\n', file);
    }
}


/* Create a CONST_DOUBLE from a string.  */

struct rtx_def *
rs6000_float_const (string, mode)
     const char *string;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE value = REAL_VALUE_ATOF (string, mode);
  return immed_real_const_1 (value, mode);
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
	  && ! info->lr_save_p
	  && ! info->cr_save_p
	  && ! info->push_p)
	return 1;
    }

  return 0;
}

/* Returns 1 always.  */

int
any_operand (op, mode)
     register rtx op ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return 1;
}

/* Returns 1 if op is the count register */
int
count_register_operand(op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != REG)
    return 0;

  if (REGNO (op) == COUNT_REGISTER_REGNUM)
    return 1;

  if (REGNO (op) > FIRST_PSEUDO_REGISTER)
    return 1;

  return 0;
}

int
xer_operand(op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != REG)
    return 0;

  if (XER_REGNO_P (REGNO (op)))
    return 1;

  return 0;
}

/* Return 1 if OP is a constant that can fit in a D field.  */

int
short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && CONST_OK_FOR_LETTER_P (INTVAL (op), 'I'));
}

/* Similar for a unsigned D field.  */

int
u_short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && CONST_OK_FOR_LETTER_P (INTVAL (op), 'K'));
}

/* Return 1 if OP is a CONST_INT that cannot fit in a signed D field.  */

int
non_short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned HOST_WIDE_INT) (INTVAL (op) + 0x8000) >= 0x10000);
}

/* Returns 1 if OP is a register that is not special (i.e., not MQ,
   ctr, or lr).  */

int
gpc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || (REGNO (op) >= ARG_POINTER_REGNUM 
		  && !XER_REGNO_P (REGNO (op)))
	      || REGNO (op) < MQ_REGNO));
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

/* Returns 1 if OP is either a pseudo-register or a register denoting a
   CR field that isn't CR0.  */

int
cc_reg_not_cr0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || CR_REGNO_NOT_CR0_P (REGNO (op))));
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
  return u_short_cint_operand (op, mode) || gpc_reg_operand (op, mode);
}

/* Return 1 is the operand is either a non-special register or ANY
   constant integer.  */

int
reg_or_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return (GET_CODE (op) == CONST_INT
	     || gpc_reg_operand (op, mode));
}

/* Return 1 is the operand is either a non-special register or ANY
   32-bit unsigned constant integer.  */

int
reg_or_u_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return (gpc_reg_operand (op, mode)
	     || (GET_CODE (op) == CONST_INT
#if HOST_BITS_PER_WIDE_INT != 32
		 && INTVAL (op) < ((HOST_WIDE_INT) 1 << 32)
#endif
		 && INTVAL (op) > 0)
#if HOST_BITS_PER_WIDE_INT == 32
	     || (GET_CODE (op) == CONST_DOUBLE
		 && CONST_DOUBLE_HIGH (op) == 0)
#endif
	 );
}

/* Return 1 if the operand is an operand that can be loaded via the GOT */

int
got_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == CONST
	  || GET_CODE (op) == LABEL_REF);
}

/* Return 1 if the operand is a simple references that can be loaded via
   the GOT (labels involving addition aren't allowed).  */

int
got_no_const_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF);
}

/* Return the number of instructions it takes to form a constant in an
   integer register.  */

static int
num_insns_constant_wide (value)
     HOST_WIDE_INT value;
{
  /* signed constant loadable with {cal|addi} */
  if (CONST_OK_FOR_LETTER_P (value, 'I'))
    return 1;

  /* constant loadable with {cau|addis} */
  else if (CONST_OK_FOR_LETTER_P (value, 'L'))
    return 1;

#if HOST_BITS_PER_WIDE_INT == 64
  else if (TARGET_POWERPC64)
    {
      HOST_WIDE_INT low  = value & 0xffffffff;
      HOST_WIDE_INT high = value >> 32;

      if (high == 0 && (low & 0x80000000u) == 0)
	return 2;

      else if (high == -1 && (low & 0x80000000u) != 0)
	return 2;

      else if (! low)
	return num_insns_constant_wide (high) + 1;

      else
	return (num_insns_constant_wide (high)
		+ num_insns_constant_wide (low) + 1);
    }
#endif

  else
    return 2;
}

int
num_insns_constant (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return num_insns_constant_wide (INTVAL (op));

  else if (GET_CODE (op) == CONST_DOUBLE && mode == SFmode)
    {
      long l;
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);
      return num_insns_constant_wide ((HOST_WIDE_INT)l);
    }

  else if (GET_CODE (op) == CONST_DOUBLE)
    {
      HOST_WIDE_INT low;
      HOST_WIDE_INT high;
      long l[2];
      REAL_VALUE_TYPE rv;
      int endian = (WORDS_BIG_ENDIAN == 0);

      if (mode == VOIDmode || mode == DImode)
	{
	  high = CONST_DOUBLE_HIGH (op);
	  low  = CONST_DOUBLE_LOW (op);
	}
      else
	{
	  REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
	  REAL_VALUE_TO_TARGET_DOUBLE (rv, l);
	  high = l[endian];
	  low  = l[1 - endian];
	}

      if (TARGET_32BIT)
	return (num_insns_constant_wide (low)
		+ num_insns_constant_wide (high));

      else
	{
	  if (high == 0 && (low & 0x80000000u) == 0)
	    return num_insns_constant_wide (low);

	  else if (high == -1 && (low & 0x80000000u) != 0)
	    return num_insns_constant_wide (low);

	  else if (mask64_operand (op, mode))
	    return 2;

	  else if (low == 0)
	    return num_insns_constant_wide (high) + 1;

	  else
	    return (num_insns_constant_wide (high)
		    + num_insns_constant_wide (low) + 1);
	}
    }

  else
    abort ();
}

/* Return 1 if the operand is a CONST_DOUBLE and it can be put into a register
   with one instruction per word.  We only do this if we can safely read
   CONST_DOUBLE_{LOW,HIGH}.  */

int
easy_fp_constant (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_DOUBLE
      || GET_MODE (op) != mode
      || (GET_MODE_CLASS (mode) != MODE_FLOAT && mode != DImode))
    return 0;

  /* Consider all constants with -msoft-float to be easy */
  if (TARGET_SOFT_FLOAT && mode != DImode)
    return 1;

  /* If we are using V.4 style PIC, consider all constants to be hard */
  if (flag_pic && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS))
    return 0;

#ifdef TARGET_RELOCATABLE
  /* Similarly if we are using -mrelocatable, consider all constants to be hard */
  if (TARGET_RELOCATABLE)
    return 0;
#endif

  if (mode == DFmode)
    {
      long k[2];
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_DOUBLE (rv, k);

      return (num_insns_constant_wide ((HOST_WIDE_INT)k[0]) == 1
	      && num_insns_constant_wide ((HOST_WIDE_INT)k[1]) == 1);
    }

  else if (mode == SFmode)
    {
      long l;
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);

      return num_insns_constant_wide (l) == 1;
    }

  else if (mode == DImode)
    return ((TARGET_POWERPC64
	     && GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_LOW (op) == 0)
	    || (num_insns_constant (op, DImode) <= 2));

  else
    abort ();
}

/* Return 1 if the operand is in volatile memory.  Note that during the
   RTL generation phase, memory_operand does not return TRUE for
   volatile memory references.  So this function allows us to
   recognize volatile references where its safe.  */

int
volatile_mem_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (!MEM_VOLATILE_P (op))
    return 0;

  if (mode != GET_MODE (op))
    return 0;

  if (reload_completed)
    return memory_operand (op, mode);

  if (reload_in_progress)
    return strict_memory_address_p (mode, XEXP (op, 0));

  return memory_address_p (mode, XEXP (op, 0));
}

/* Return 1 if the operand is an offsettable memory operand.  */

int
offsettable_mem_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == MEM)
	  && offsettable_address_p (reload_completed || reload_in_progress,
				    mode, XEXP (op, 0)));
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
   that can be used as the operand of a `mode' add insn.  */

int
add_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return (reg_or_short_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL(op), 'L')));
}

/* Return 1 if OP is a constant but not a valid add_operand.  */

int
non_add_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned HOST_WIDE_INT) (INTVAL (op) + 0x8000) >= 0x10000
	  && ! CONST_OK_FOR_LETTER_P (INTVAL(op), 'L'));
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
#if HOST_BITS_PER_WIDE_INT != 32
	      && INTVAL (op) > 0
	      && INTVAL (op) < ((HOST_WIDE_INT) 1 << 32)
#endif
	      && ((INTVAL (op) & GET_MODE_MASK (mode)
		   & (~ (HOST_WIDE_INT) 0xffff)) == 0
		  || (INTVAL (op) & GET_MODE_MASK (mode)
		      & (~ (unsigned HOST_WIDE_INT) 0xffff0000u)) == 0)));
}

/* Return 1 if the operand is a non-special register or a 32-bit constant
   that can be used as the operand of an OR or XOR insn on the RS/6000.  */

int
logical_u_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && INTVAL (op) > 0
#if HOST_BITS_PER_WIDE_INT != 32
	      && INTVAL (op) < ((HOST_WIDE_INT) 1 << 32)
#endif
	      && ((INTVAL (op) & GET_MODE_MASK (mode)
		   & (~ (HOST_WIDE_INT) 0xffff)) == 0
		  || (INTVAL (op) & GET_MODE_MASK (mode)
		      & (~ (unsigned HOST_WIDE_INT) 0xffff0000u)) == 0))
#if HOST_BITS_PER_WIDE_INT == 32
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0
	      && ((CONST_DOUBLE_LOW (op)
		   & (~ (unsigned HOST_WIDE_INT) 0xffff0000u)) == 0))
#endif
      );
}

/* Return 1 if C is a constant that is not a logical operand (as
   above).  */

int
non_logical_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
#if HOST_BITS_PER_WIDE_INT != 32
	  && INTVAL (op) < ((HOST_WIDE_INT) 1 << 32)
#endif
	  && (INTVAL (op) & GET_MODE_MASK (mode) &
	      (~ (HOST_WIDE_INT) 0xffff)) != 0
	  && (INTVAL (op) & GET_MODE_MASK (mode) &
	      (~ (unsigned HOST_WIDE_INT) 0xffff0000u)) != 0);
}

/* Return 1 if C is an unsigned 32-bit constant that is not a
   logical operand (as above).  */

int
non_logical_u_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ((GET_CODE (op) == CONST_INT
	   && INTVAL (op) > 0
#if HOST_BITS_PER_WIDE_INT != 32
	   && INTVAL (op) < ((HOST_WIDE_INT) 1 << 32)
#endif
	   && (INTVAL (op) & GET_MODE_MASK (mode)
	       & (~ (HOST_WIDE_INT) 0xffff)) != 0
	   && (INTVAL (op) & GET_MODE_MASK (mode)
	       & (~ (unsigned HOST_WIDE_INT) 0xffff0000u)) != 0)
#if HOST_BITS_PER_WIDE_INT == 32
	  || (GET_CODE (op) == CONST_DOUBLE
	      && CONST_DOUBLE_HIGH (op) == 0
	      && (CONST_DOUBLE_LOW (op) & (~ (HOST_WIDE_INT) 0xffff)) != 0
	      && (CONST_DOUBLE_LOW (op)
		  & (~ (unsigned HOST_WIDE_INT) 0xffff0000u)) != 0));
#endif
}

/* Return 1 if C is a constant that can be encoded in a 32-bit mask on the
   RS/6000.  It is if there are no more than two 1->0 or 0->1 transitions.
   Reject all ones and all zeros, since these should have been optimized
   away and confuse the making of MB and ME.  */

int
mask_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT c;
  int i;
  int last_bit_value;
  int transitions = 0;

  if (GET_CODE (op) != CONST_INT)
    return 0;

  c = INTVAL (op);

  if (c == 0 || c == ~0)
    return 0;

  last_bit_value = c & 1;

  for (i = 1; i < 32; i++)
    if (((c >>= 1) & 1) != last_bit_value)
      last_bit_value ^= 1, transitions++;

  return transitions <= 2;
}

/* Return 1 if the operand is a constant that is a PowerPC64 mask.
   It is if there are no more than one 1->0 or 0->1 transitions.
   Reject all ones and all zeros, since these should have been optimized
   away and confuse the making of MB and ME.  */

int
mask64_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      HOST_WIDE_INT c = INTVAL (op);
      int i;
      int last_bit_value;
      int transitions = 0;

      if (c == 0 || c == ~0)
	return 0;

      last_bit_value = c & 1;

      for (i = 1; i < HOST_BITS_PER_WIDE_INT; i++)
	if (((c >>= 1) & 1) != last_bit_value)
	  last_bit_value ^= 1, transitions++;

#if HOST_BITS_PER_WIDE_INT == 32
      /* Consider CONST_INT sign-extended.  */
      transitions += (last_bit_value != 1);
#endif

      return transitions <= 1;
    }
  else if (GET_CODE (op) == CONST_DOUBLE
	   && (mode == VOIDmode || mode == DImode))
    {
      HOST_WIDE_INT low = CONST_DOUBLE_LOW (op);
#if HOST_BITS_PER_WIDE_INT == 32
      HOST_WIDE_INT high = CONST_DOUBLE_HIGH (op);
#endif
      int i;
      int last_bit_value;
      int transitions = 0;

      if ((low == 0
#if HOST_BITS_PER_WIDE_INT == 32
	  && high == 0
#endif
	   )
	  || (low == ~0
#if HOST_BITS_PER_WIDE_INT == 32
	      && high == ~0
#endif
	      ))
	return 0;

      last_bit_value = low & 1;

      for (i = 1; i < HOST_BITS_PER_WIDE_INT; i++)
	if (((low >>= 1) & 1) != last_bit_value)
	  last_bit_value ^= 1, transitions++;

#if HOST_BITS_PER_WIDE_INT == 32
      if ((high & 1) != last_bit_value)
	last_bit_value ^= 1, transitions++;

      for (i = 1; i < HOST_BITS_PER_WIDE_INT; i++)
	if (((high >>= 1) & 1) != last_bit_value)
	  last_bit_value ^= 1, transitions++;
#endif

      return transitions <= 1;
    }
  else
    return 0;
}

/* Return 1 if the operand is either a non-special register or a constant
   that can be used as the operand of a PowerPC64 logical AND insn.  */

int
and64_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (fixed_regs[CR0_REGNO])	/* CR0 not available, don't do andi./andis. */
    return (gpc_reg_operand (op, mode) || mask64_operand (op, mode));

  return (logical_operand (op, mode) || mask64_operand (op, mode));
}

/* Return 1 if the operand is either a non-special register or a
   constant that can be used as the operand of an RS/6000 logical AND insn.  */

int
and_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (fixed_regs[CR0_REGNO])	/* CR0 not available, don't do andi./andis. */
    return (gpc_reg_operand (op, mode) || mask_operand (op, mode));

  return (logical_operand (op, mode) || mask_operand (op, mode));
}

/* Return 1 if the operand is a general register or memory operand.  */

int
reg_or_mem_operand (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || memory_operand (op, mode)
	  || volatile_mem_operand (op, mode));
}

/* Return 1 if the operand is a general register or memory operand without
   pre_inc or pre_dec which produces invalid form of PowerPC lwa
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
   this file and the function is not weakly defined. */

int
current_file_function_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == SYMBOL_REF
	  && (SYMBOL_REF_FLAG (op)
	      || (op == XEXP (DECL_RTL (current_function_decl), 0)
	          && ! DECL_WEAK (current_function_decl))));
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

  /* Only a tiny bit of handling for CONSTANT_P_RTX is necessary.  */
  if (GET_CODE (op) == CONSTANT_P_RTX)
    return 1;

  /* For floating-point, easy constants are valid.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && CONSTANT_P (op)
      && easy_fp_constant (op, mode))
    return 1;

  /* Allow any integer constant.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && (GET_CODE (op) == CONST_INT
	  || GET_CODE (op) == CONST_DOUBLE))
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

  /* A SYMBOL_REF referring to the TOC is valid.  */
  if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (op))
    return 1;

  /* A constant pool expression (relative to the TOC) is valid */
  if (TOC_RELATIVE_EXPR_P (op))
    return 1;

  /* V.4 allows SYMBOL_REFs and CONSTs that are in the small data region
     to be valid.  */
  if ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
      && (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST)
      && small_data_operand (op, Pmode))
    return 1;

  return 0;
}

/* Return 1 for an operand in small memory on V.4/eabi */

int
small_data_operand (op, mode)
     rtx op ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
#if TARGET_ELF
  rtx sym_ref;

  if (rs6000_sdata == SDATA_NONE || rs6000_sdata == SDATA_DATA)
    return 0;

  if (DEFAULT_ABI != ABI_V4 && DEFAULT_ABI != ABI_SOLARIS)
    return 0;

  if (GET_CODE (op) == SYMBOL_REF)
    sym_ref = op;

  else if (GET_CODE (op) != CONST
	   || GET_CODE (XEXP (op, 0)) != PLUS
	   || GET_CODE (XEXP (XEXP (op, 0), 0)) != SYMBOL_REF
	   || GET_CODE (XEXP (XEXP (op, 0), 1)) != CONST_INT)
    return 0;

  else
    {
      rtx sum = XEXP (op, 0);
      HOST_WIDE_INT summand;

      /* We have to be careful here, because it is the referenced address
        that must be 32k from _SDA_BASE_, not just the symbol.  */
      summand = INTVAL (XEXP (sum, 1));
      if (summand < 0 || summand > g_switch_value)
       return 0;

      sym_ref = XEXP (sum, 0);
    }

  if (*XSTR (sym_ref, 0) != '@')
    return 0;

  return 1;

#else
  return 0;
#endif
}

static int 
constant_pool_expr_1 (op, have_sym, have_toc) 
    rtx op;
    int *have_sym;
    int *have_toc;
{
  switch (GET_CODE(op)) 
    {
    case SYMBOL_REF:
	if (CONSTANT_POOL_ADDRESS_P (op))
	  {
	   if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (op)))
	     {
	       *have_sym = 1;
	       return 1;
	     }
	   else
	     return 0;
	  }
	else if (! strcmp (XSTR (op, 0), toc_label_name))
	  {
	    *have_toc = 1;
	    return 1;
	  }
	else
	  return 0;
    case PLUS:
    case MINUS:
	return constant_pool_expr_1 (XEXP (op, 0), have_sym, have_toc) &&
	    	constant_pool_expr_1 (XEXP (op, 1), have_sym, have_toc);
    case CONST:
	return constant_pool_expr_1 (XEXP (op, 0), have_sym, have_toc);
    case CONST_INT:
	return 1;
    default:
	return 0;
    }
}

int
constant_pool_expr_p (op)
    rtx op;
{
  int have_sym = 0;
  int have_toc = 0;
  return constant_pool_expr_1 (op, &have_sym, &have_toc) && have_sym;
}

int
toc_relative_expr_p (op)
    rtx op;
{
    int have_sym = 0;
    int have_toc = 0;
    return constant_pool_expr_1 (op, &have_sym, &have_toc) && have_toc;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This is used from only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE is passed so that this macro can use GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   On RS/6000, first check for the sum of a register with a constant
   integer that is out of range.  If so, generate code to add the
   constant with the low-order 16 bits masked to the register and force
   this result into another register (this can be done with `cau').
   Then generate an address of REG+(CONST&0xffff), allowing for the
   possibility of bit 16 being a one.

   Then check for the sum of a register and something not constant, try to
   load the other things into a register and return the sum.  */
rtx
rs6000_legitimize_address (x, oldx, mode)
     rtx x;
     rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{ 
  if (GET_CODE (x) == PLUS 
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (unsigned HOST_WIDE_INT) (INTVAL (XEXP (x, 1)) + 0x8000) >= 0x10000)
    { 
      HOST_WIDE_INT high_int, low_int;
      rtx sum;
      high_int = INTVAL (XEXP (x, 1)) & (~ (HOST_WIDE_INT) 0xffff);
      low_int = INTVAL (XEXP (x, 1)) & 0xffff;
      if (low_int & 0x8000)
	high_int += 0x10000, low_int |= ((HOST_WIDE_INT) -1) << 16;
      sum = force_operand (gen_rtx_PLUS (Pmode, XEXP (x, 0),
					 GEN_INT (high_int)), 0);
      return gen_rtx_PLUS (Pmode, sum, GEN_INT (low_int));
    }
  else if (GET_CODE (x) == PLUS 
	   && GET_CODE (XEXP (x, 0)) == REG
	   && GET_CODE (XEXP (x, 1)) != CONST_INT
	   && (TARGET_HARD_FLOAT || TARGET_POWERPC64 || mode != DFmode)
	   && (TARGET_POWERPC64 || mode != DImode)
	   && mode != TImode)
    {
      return gen_rtx_PLUS (Pmode, XEXP (x, 0),
			   force_reg (Pmode, force_operand (XEXP (x, 1), 0)));
    }
  else if (TARGET_ELF && TARGET_32BIT && TARGET_NO_TOC && ! flag_pic
	   && GET_CODE (x) != CONST_INT
	   && GET_CODE (x) != CONST_DOUBLE 
	   && CONSTANT_P (x)
	   && (TARGET_HARD_FLOAT || mode != DFmode)
	   && mode != DImode 
	   && mode != TImode)
    {
      rtx reg = gen_reg_rtx (Pmode);
      emit_insn (gen_elf_high (reg, (x)));
      return gen_rtx_LO_SUM (Pmode, reg, (x));
    }
  else if (TARGET_TOC 
	   && CONSTANT_POOL_EXPR_P (x)
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (x)))
    {
      return create_TOC_reference (x);
    }
  else
    return NULL_RTX;
}



/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   For incoming args we set the number of arguments in the prototype large
   so we never return a PARALLEL.  */

void
init_cumulative_args (cum, fntype, libname, incoming)
     CUMULATIVE_ARGS *cum;
     tree fntype;
     rtx libname ATTRIBUTE_UNUSED;
     int incoming;
{
  static CUMULATIVE_ARGS zero_cumulative;

  *cum = zero_cumulative;
  cum->words = 0;
  cum->fregno = FP_ARG_MIN_REG;
  cum->prototype = (fntype && TYPE_ARG_TYPES (fntype));
  cum->call_cookie = CALL_NORMAL;
  cum->sysv_gregno = GP_ARG_MIN_REG;

  if (incoming)
    cum->nargs_prototype = 1000;		/* don't return a PARALLEL */

  else if (cum->prototype)
    cum->nargs_prototype = (list_length (TYPE_ARG_TYPES (fntype)) - 1
			    + (TYPE_MODE (TREE_TYPE (fntype)) == BLKmode
			       || RETURN_IN_MEMORY (TREE_TYPE (fntype))));

  else
    cum->nargs_prototype = 0;

  cum->orig_nargs = cum->nargs_prototype;

  /* Check for longcall's */
  if (fntype && lookup_attribute ("longcall", TYPE_ATTRIBUTES (fntype)))
    cum->call_cookie = CALL_LONG;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " ret code = %s,",
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}

      if (cum->call_cookie & CALL_LONG)
	fprintf (stderr, " longcall,");

      fprintf (stderr, " proto = %d, nargs = %d\n",
	       cum->prototype, cum->nargs_prototype);
    }
}

/* If defined, a C expression which determines whether, and in which
   direction, to pad out an argument with extra space.  The value
   should be of type `enum direction': either `upward' to pad above
   the argument, `downward' to pad below, or `none' to inhibit
   padding.

   For the AIX ABI structs are always stored left shifted in their
   argument slot.  */

enum direction
function_arg_padding (mode, type)
     enum machine_mode mode;
     tree type;
{
  if (type != 0 && AGGREGATE_TYPE_P (type))
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

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined, 
   PARM_BOUNDARY is used for all arguments.
   
   V.4 wants long longs to be double word aligned.  */

int
function_arg_boundary (mode, type)
     enum machine_mode mode;
     tree type ATTRIBUTE_UNUSED;
{
  if ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
      && (mode == DImode || mode == DFmode))
    return 64;
  else
    return PARM_BOUNDARY;
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

  if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
    {
      if (TARGET_HARD_FLOAT
	  && (mode == SFmode || mode == DFmode))
	{
	  if (cum->fregno <= FP_ARG_V4_MAX_REG)
	    cum->fregno++;
	  else
	    {
	      if (mode == DFmode)
	        cum->words += cum->words & 1;
	      cum->words += RS6000_ARG_SIZE (mode, type, 1);
	    }
	}
      else
	{
	  int n_words;
	  int gregno = cum->sysv_gregno;

	  /* Aggregates and IEEE quad get passed by reference.  */
	  if ((type && AGGREGATE_TYPE_P (type))
	      || mode == TFmode)
	    n_words = 1;
	  else 
	    n_words = RS6000_ARG_SIZE (mode, type, 1);

	  /* Long long is put in odd registers.  */
	  if (n_words == 2 && (gregno & 1) == 0)
	    gregno += 1;

	  /* Long long is not split between registers and stack.  */
	  if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	    {
	      /* Long long is aligned on the stack.  */
	      if (n_words == 2)
		cum->words += cum->words & 1;
	      cum->words += n_words;
	    }

	  /* Note: continuing to accumulate gregno past when we've started
	     spilling to the stack indicates the fact that we've started
	     spilling to the stack to expand_builtin_saveregs.  */
	  cum->sysv_gregno = gregno + n_words;
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "gregno = %2d, nargs = %4d, proto = %d, ",
		   cum->sysv_gregno, cum->nargs_prototype, cum->prototype);
	  fprintf (stderr, "mode = %4s, named = %d\n",
		   GET_MODE_NAME (mode), named);
	}
    }
  else
    {
      int align = (TARGET_32BIT && (cum->words & 1) != 0
		   && function_arg_boundary (mode, type) == 64) ? 1 : 0;
      cum->words += align;

      if (named)
	{
	  cum->words += RS6000_ARG_SIZE (mode, type, named);
	  if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_HARD_FLOAT)
	    cum->fregno++;
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "nargs = %4d, proto = %d, mode = %4s, ",
		   cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode));
	  fprintf (stderr, "named = %d, align = %d\n", named, align);
	}
    }
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
   doesn't support PARALLEL anyway.  */

struct rtx_def *
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  enum rs6000_abi abi = DEFAULT_ABI;

  /* Return a marker to indicate whether CR1 needs to set or clear the bit
     that V.4 uses to say fp args were passed in registers.  Assume that we
     don't need the marker for software floating point, or compiler generated
     library calls.  */
  if (mode == VOIDmode)
    {
      if ((abi == ABI_V4 || abi == ABI_SOLARIS)
	  && TARGET_HARD_FLOAT
	  && cum->nargs_prototype < 0
	  && type && (cum->prototype || TARGET_NO_PROTOTYPE))
	{
	  return GEN_INT (cum->call_cookie
			  | ((cum->fregno == FP_ARG_MIN_REG)
			     ? CALL_V4_SET_FP_ARGS
			     : CALL_V4_CLEAR_FP_ARGS));
	}

      return GEN_INT (cum->call_cookie);
    }

  if (abi == ABI_V4 || abi == ABI_SOLARIS)
    {
      if (TARGET_HARD_FLOAT
	  && (mode == SFmode || mode == DFmode))
	{
	  if (cum->fregno <= FP_ARG_V4_MAX_REG)
	    return gen_rtx_REG (mode, cum->fregno);
	  else
	    return NULL;
	}
      else
	{
	  int n_words;
	  int gregno = cum->sysv_gregno;

	  /* Aggregates and IEEE quad get passed by reference.  */
	  if ((type && AGGREGATE_TYPE_P (type))
	      || mode == TFmode)
	    n_words = 1;
	  else 
	    n_words = RS6000_ARG_SIZE (mode, type, 1);

	  /* Long long is put in odd registers.  */
	  if (n_words == 2 && (gregno & 1) == 0)
	    gregno += 1;

	  /* Long long is not split between registers and stack.  */
	  if (gregno + n_words - 1 <= GP_ARG_MAX_REG)
	    return gen_rtx_REG (mode, gregno);
	  else
	    return NULL;
	}
    }
  else
    {
      int align = (TARGET_32BIT && (cum->words & 1) != 0
	           && function_arg_boundary (mode, type) == 64) ? 1 : 0;
      int align_words = cum->words + align;

      if (! named)
	return NULL_RTX;

      if (type && TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
        return NULL_RTX;

      if (USE_FP_FOR_ARG_P (*cum, mode, type))
	{
	  if (! type
	      || ((cum->nargs_prototype > 0)
	          /* IBM AIX extended its linkage convention definition always
		     to require FP args after register save area hole on the
		     stack.  */
	          && (DEFAULT_ABI != ABI_AIX
		      || ! TARGET_XL_CALL
		      || (align_words < GP_ARG_NUM_REG))))
	    return gen_rtx_REG (mode, cum->fregno);

          return gen_rtx_PARALLEL (mode,
	    gen_rtvec (2,
		       gen_rtx_EXPR_LIST (VOIDmode,
				((align_words >= GP_ARG_NUM_REG)
				 ? NULL_RTX
				 : (align_words
				    + RS6000_ARG_SIZE (mode, type, named)
				    > GP_ARG_NUM_REG
				    /* If this is partially on the stack, then
				       we only include the portion actually
				       in registers here.  */
				    ? gen_rtx_REG (SImode,
					       GP_ARG_MIN_REG + align_words)
				    : gen_rtx_REG (mode,
					       GP_ARG_MIN_REG + align_words))),
				const0_rtx),
		       gen_rtx_EXPR_LIST (VOIDmode,
				gen_rtx_REG (mode, cum->fregno),
				const0_rtx)));
	}
      else if (align_words < GP_ARG_NUM_REG)
	return gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);
      else
	return NULL_RTX;
    }
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

  if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
    return 0;

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
     CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
      && ((type && AGGREGATE_TYPE_P (type))
	  || mode == TFmode))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: aggregate\n");

      return 1;
    }

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
  CUMULATIVE_ARGS next_cum;
  int reg_size = TARGET_32BIT ? 4 : 8;
  rtx save_area, mem;
  int first_reg_offset, set;

  if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
    {
      tree fntype;
      int stdarg_p;

      fntype = TREE_TYPE (current_function_decl);
      stdarg_p = (TYPE_ARG_TYPES (fntype) != 0
		  && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		      != void_type_node));

      /* For varargs, we do not want to skip the dummy va_dcl argument.
         For stdargs, we do want to skip the last named argument.  */
      next_cum = *cum;
      if (stdarg_p)
	function_arg_advance (&next_cum, mode, type, 1);

      /* Indicate to allocate space on the stack for varargs save area.  */
      /* ??? Does this really have to be located at a magic spot on the
	 stack, or can we allocate this with assign_stack_local instead.  */
      rs6000_sysv_varargs_p = 1;
      if (! no_rtl)
	save_area = plus_constant (virtual_stack_vars_rtx,
				   - RS6000_VARARGS_SIZE);

      first_reg_offset = next_cum.sysv_gregno - GP_ARG_MIN_REG;
    }
  else
    {
      save_area = virtual_incoming_args_rtx;
      rs6000_sysv_varargs_p = 0;

      first_reg_offset = cum->words;
      if (MUST_PASS_IN_STACK (mode, type))
	first_reg_offset += RS6000_ARG_SIZE (TYPE_MODE (type), type, 1);
    }

  set = get_varargs_alias_set ();
  if (! no_rtl && first_reg_offset < GP_ARG_NUM_REG)
    {
      mem = gen_rtx_MEM (BLKmode,
		         plus_constant (save_area,
					first_reg_offset * reg_size)),
      MEM_ALIAS_SET (mem) = set;

      move_block_from_reg
	(GP_ARG_MIN_REG + first_reg_offset, mem,
	 GP_ARG_NUM_REG - first_reg_offset,
	 (GP_ARG_NUM_REG - first_reg_offset) * UNITS_PER_WORD);

      /* ??? Does ABI_V4 need this at all?  */
      *pretend_size = (GP_ARG_NUM_REG - first_reg_offset) * UNITS_PER_WORD;
    }

  /* Save FP registers if needed.  */
  if ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
      && TARGET_HARD_FLOAT && ! no_rtl
      && next_cum.fregno <= FP_ARG_V4_MAX_REG)
    {
      int fregno = next_cum.fregno;
      rtx cr1 = gen_rtx_REG (CCmode, CR1_REGNO);
      rtx lab = gen_label_rtx ();
      int off = (GP_ARG_NUM_REG * reg_size) + ((fregno - FP_ARG_MIN_REG) * 8);

      emit_jump_insn (gen_rtx_SET (VOIDmode,
				   pc_rtx,
				   gen_rtx_IF_THEN_ELSE (VOIDmode,
					    gen_rtx_NE (VOIDmode, cr1,
						        const0_rtx),
					    gen_rtx_LABEL_REF (VOIDmode, lab),
					    pc_rtx)));

      while (fregno <= FP_ARG_V4_MAX_REG)
	{
	  mem = gen_rtx_MEM (DFmode, plus_constant (save_area, off));
          MEM_ALIAS_SET (mem) = set;
	  emit_move_insn (mem, gen_rtx_REG (DFmode, fregno));
	  fregno++;
	  off += 8;
	}

      emit_label (lab);
    }
}

/* Create the va_list data type.  */

tree
rs6000_build_va_list ()
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  /* For AIX, prefer 'char *' because that's what the system
     header files like.  */
  if (DEFAULT_ABI != ABI_V4 && DEFAULT_ABI != ABI_SOLARIS)
    return build_pointer_type (char_type_node);

  record = make_lang_type (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (FIELD_DECL, get_identifier ("gpr"), 
		      unsigned_char_type_node);
  f_fpr = build_decl (FIELD_DECL, get_identifier ("fpr"), 
		      unsigned_char_type_node);
  f_ovf = build_decl (FIELD_DECL, get_identifier ("overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (FIELD_DECL, get_identifier ("reg_save_area"),
		      ptr_type_node);

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  TREE_CHAIN (f_gpr) = f_fpr;
  TREE_CHAIN (f_fpr) = f_ovf;
  TREE_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start.  */

void
rs6000_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg;
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  /* Only SVR4 needs something special.  */
  if (DEFAULT_ABI != ABI_V4 && DEFAULT_ABI != ABI_SOLARIS)
    {
      std_expand_builtin_va_start (stdarg_p, valist, nextarg);
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  /* Count number of gp and fp argument registers used.  */
  words = current_function_args_info.words;
  n_gpr = current_function_args_info.sysv_gregno - GP_ARG_MIN_REG;
  n_fpr = current_function_args_info.fregno - FP_ARG_MIN_REG;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "va_start: words = %d, n_gpr = %d, n_fpr = %d\n",
	     words, n_gpr, n_fpr);

  t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr, build_int_2 (n_gpr, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build (MODIFY_EXPR, TREE_TYPE (fpr), fpr, build_int_2 (n_fpr, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the overflow area.  */
  t = make_tree (TREE_TYPE (ovf), virtual_incoming_args_rtx);
  if (words != 0)
    t = build (PLUS_EXPR, TREE_TYPE (ovf), t,
	       build_int_2 (words * UNITS_PER_WORD, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the register save area.  */
  t = make_tree (TREE_TYPE (sav), virtual_stack_vars_rtx);
  t = build (PLUS_EXPR, TREE_TYPE (sav), t,
	     build_int_2 (-RS6000_VARARGS_SIZE, -1));
  t = build (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement va_arg.  */

rtx
rs6000_va_arg (valist, type)
     tree valist, type;
{
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, reg, t, u;
  int indirect_p, size, rsize, n_reg, sav_ofs, sav_scale;
  rtx lab_false, lab_over, addr_rtx, r;

  /* For AIX, the rule is that structures are passed left-aligned in
     their stack slot.  However, GCC does not presently do this:
     structures which are the same size as integer types are passed
     right-aligned, as if they were in fact integers.  This only
     matters for structures of size 1 or 2, or 4 when TARGET_64BIT.  */
  if (DEFAULT_ABI != ABI_V4 && DEFAULT_ABI != ABI_SOLARIS)
    {
      HOST_WIDE_INT align, rounded_size;
      enum machine_mode mode;
      tree addr_tree;

      /* Compute the rounded size of the type.  */
      align = PARM_BOUNDARY / BITS_PER_UNIT;
      rounded_size = (((int_size_in_bytes (type) + align - 1) / align)
		      * align);

      addr_tree = valist;

      mode = TYPE_MODE (type);
      if (mode != BLKmode)
	{
	  HOST_WIDE_INT adj;
	  adj = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_UNIT;
	  if (rounded_size > align)
	    adj = rounded_size;
	  
	  addr_tree = build (PLUS_EXPR, TREE_TYPE (addr_tree), addr_tree,
			     build_int_2 (rounded_size - adj, 0));
	}

      addr_rtx = expand_expr (addr_tree, NULL_RTX, Pmode, EXPAND_NORMAL);
      addr_rtx = copy_to_reg (addr_rtx);
      
      /* Compute new value for AP.  */
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist,
		 build (PLUS_EXPR, TREE_TYPE (valist), valist,
			build_int_2 (rounded_size, 0)));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
      
      return addr_rtx;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (AGGREGATE_TYPE_P (type) || TYPE_MODE (type) == TFmode)
    {
      /* Aggregates and long doubles are passed by reference.  */
      indirect_p = 1;
      reg = gpr;
      n_reg = 1;
      sav_ofs = 0;
      sav_scale = 4;
      size = rsize = UNITS_PER_WORD;
    }
  else if (FLOAT_TYPE_P (type) && ! TARGET_SOFT_FLOAT)
    {
      /* FP args go in FP registers, if present.  */
      indirect_p = 0;
      reg = fpr;
      n_reg = 1;
      sav_ofs = 8*4;
      sav_scale = 8;
    }
  else
    {
      /* Otherwise into GP registers.  */
      indirect_p = 0;
      reg = gpr;
      n_reg = rsize;
      sav_ofs = 0;
      sav_scale = 4;
    }

  /*
   * Pull the value out of the saved registers ...
   */

  lab_false = gen_label_rtx ();
  lab_over = gen_label_rtx ();
  addr_rtx = gen_reg_rtx (Pmode);

  emit_cmp_and_jump_insns (expand_expr (reg, NULL_RTX, QImode, EXPAND_NORMAL),
			   GEN_INT (8 - n_reg + 1),
			   GE, const1_rtx, QImode, 1, 1, lab_false);

  /* Long long is aligned in the registers.  */
  if (n_reg > 1)
    {
      u = build (BIT_AND_EXPR, TREE_TYPE (reg), reg,
		 build_int_2 (n_reg - 1, 0));
      u = build (PLUS_EXPR, TREE_TYPE (reg), reg, u);
      u = build (MODIFY_EXPR, TREE_TYPE (reg), reg, u);
      TREE_SIDE_EFFECTS (u) = 1;
      expand_expr (u, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  if (sav_ofs)
    t = build (PLUS_EXPR, ptr_type_node, sav, build_int_2 (sav_ofs, 0));
  else
    t = sav;

  u = build (POSTINCREMENT_EXPR, TREE_TYPE (reg), reg, build_int_2 (n_reg, 0));
  TREE_SIDE_EFFECTS (u) = 1;

  u = build1 (CONVERT_EXPR, integer_type_node, u);
  TREE_SIDE_EFFECTS (u) = 1;

  u = build (MULT_EXPR, integer_type_node, u, build_int_2 (sav_scale, 0));
  TREE_SIDE_EFFECTS (u) = 1;

  t = build (PLUS_EXPR, ptr_type_node, t, u);
  TREE_SIDE_EFFECTS (t) = 1;

  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);

  emit_jump_insn (gen_jump (lab_over));
  emit_barrier ();
  emit_label (lab_false);

  /*
   * ... otherwise out of the overflow area.
   */

  /* Make sure we don't find reg 7 for the next int arg.  */
  if (n_reg > 1)
    {
      t = build (MODIFY_EXPR, TREE_TYPE (reg), reg, build_int_2 (8, 0));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  /* Care for on-stack alignment if needed.  */
  if (rsize <= 1)
    t = ovf;
  else
    {
      t = build (PLUS_EXPR, TREE_TYPE (ovf), ovf, build_int_2 (7, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t, build_int_2 (-8, -1));
    }
  t = save_expr (t);

  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);

  t = build (PLUS_EXPR, TREE_TYPE (t), t, build_int_2 (size, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  emit_label (lab_over);

  if (indirect_p)
    {
      r = gen_rtx_MEM (Pmode, addr_rtx);
      MEM_ALIAS_SET (r) = get_varargs_alias_set ();
      emit_move_insn (addr_rtx, r);
    }

  return addr_rtx;
}

/* Generate a memory reference for expand_block_move, copying volatile,
   and other bits from an original memory reference.  */

static rtx
expand_block_move_mem (mode, addr, orig_mem)
     enum machine_mode mode;
     rtx addr;
     rtx orig_mem;
{
  rtx mem = gen_rtx_MEM (mode, addr);

  RTX_UNCHANGING_P (mem) = RTX_UNCHANGING_P (orig_mem);
  MEM_COPY_ATTRIBUTES (mem, orig_mem);
#ifdef MEM_UNALIGNED_P
  MEM_UNALIGNED_P (mem) = MEM_UNALIGNED_P (orig_mem);
#endif
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
  rtx orig_dest = operands[0];
  rtx orig_src	= operands[1];
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align;
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
  if (! constp)
    return 0;

  /* If this is not a fixed size alignment, abort */
  if (GET_CODE (align_rtx) != CONST_INT)
    abort ();
  align = INTVAL (align_rtx);

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  /* Don't support real large moves.  If string instructions are not used,
     then don't generate more than 8 loads.  */
  if (TARGET_STRING)
    {
      if (bytes > 8*4)
	return 0;
    }
  else if (! STRICT_ALIGNMENT)
    {
      if (TARGET_POWERPC64 && align >= 4)
	{
	  if (bytes > 8*8)
	    return 0;
	}
      else
	if (bytes > 8*4)
	  return 0;
    }
  else if (bytes > 8*align)
    return 0;

  /* Move the address into scratch registers.  */
  dest_reg = copy_addr_to_reg (XEXP (orig_dest, 0));
  src_reg  = copy_addr_to_reg (XEXP (orig_src,  0));

  if (TARGET_STRING)	/* string instructions are available */
    {
      for ( ; bytes > 0; bytes -= move_bytes)
	{
	  if (bytes > 24		/* move up to 32 bytes at a time */
	      && ! fixed_regs[5]
	      && ! fixed_regs[6]
	      && ! fixed_regs[7]
	      && ! fixed_regs[8]
	      && ! fixed_regs[9]
	      && ! fixed_regs[10]
	      && ! fixed_regs[11]
	      && ! fixed_regs[12])
	    {
	      move_bytes = (bytes > 32) ? 32 : bytes;
	      emit_insn (gen_movstrsi_8reg (expand_block_move_mem (BLKmode,
								   dest_reg,
								   orig_dest),
					    expand_block_move_mem (BLKmode,
								   src_reg,
								   orig_src),
					    GEN_INT ((move_bytes == 32)
						     ? 0 : move_bytes),
					    align_rtx));
	    }
	  else if (bytes > 16	/* move up to 24 bytes at a time */
		   && ! fixed_regs[5]
		   && ! fixed_regs[6]
		   && ! fixed_regs[7]
		   && ! fixed_regs[8]
		   && ! fixed_regs[9]
		   && ! fixed_regs[10])
	    {
	      move_bytes = (bytes > 24) ? 24 : bytes;
	      emit_insn (gen_movstrsi_6reg (expand_block_move_mem (BLKmode,
								   dest_reg,
								   orig_dest),
					    expand_block_move_mem (BLKmode,
								   src_reg,
								   orig_src),
					    GEN_INT (move_bytes),
					    align_rtx));
	    }
	  else if (bytes > 8	/* move up to 16 bytes at a time */
		   && ! fixed_regs[5]
		   && ! fixed_regs[6]
		   && ! fixed_regs[7]
		   && ! fixed_regs[8])
	    {
	      move_bytes = (bytes > 16) ? 16 : bytes;
	      emit_insn (gen_movstrsi_4reg (expand_block_move_mem (BLKmode,
								   dest_reg,
								   orig_dest),
					    expand_block_move_mem (BLKmode,
								   src_reg,
								   orig_src),
					    GEN_INT (move_bytes),
					    align_rtx));
	    }
	  else if (bytes > 4 && ! TARGET_POWERPC64)
	    {			/* move up to 8 bytes at a time */
	      move_bytes = (bytes > 8) ? 8 : bytes;
	      emit_insn (gen_movstrsi_2reg (expand_block_move_mem (BLKmode,
								   dest_reg,
								   orig_dest),
					    expand_block_move_mem (BLKmode,
								   src_reg,
								   orig_src),
					    GEN_INT (move_bytes),
					    align_rtx));
	    }
	  else if (bytes >= 4 && (align >= 4 || ! STRICT_ALIGNMENT))
	    {			/* move 4 bytes */
	      move_bytes = 4;
	      tmp_reg = gen_reg_rtx (SImode);
	      emit_move_insn (tmp_reg,
			      expand_block_move_mem (SImode,
						     src_reg, orig_src));
	      emit_move_insn (expand_block_move_mem (SImode,
						     dest_reg, orig_dest),
			      tmp_reg);
	    }
	  else if (bytes == 2 && (align >= 2 || ! STRICT_ALIGNMENT))
	    {			/* move 2 bytes */
	      move_bytes = 2;
	      tmp_reg = gen_reg_rtx (HImode);
	      emit_move_insn (tmp_reg,
			      expand_block_move_mem (HImode,
						     src_reg, orig_src));
	      emit_move_insn (expand_block_move_mem (HImode,
						     dest_reg, orig_dest),
			      tmp_reg);
	    }
	  else if (bytes == 1)	/* move 1 byte */
	    {
	      move_bytes = 1;
	      tmp_reg = gen_reg_rtx (QImode);
	      emit_move_insn (tmp_reg,
			      expand_block_move_mem (QImode,
						     src_reg, orig_src));
	      emit_move_insn (expand_block_move_mem (QImode,
						     dest_reg, orig_dest),
			      tmp_reg);
	    }
	  else
	    {			/* move up to 4 bytes at a time */
	      move_bytes = (bytes > 4) ? 4 : bytes;
	      emit_insn (gen_movstrsi_1reg (expand_block_move_mem (BLKmode,
								   dest_reg,
								   orig_dest),
					    expand_block_move_mem (BLKmode,
								   src_reg,
								   orig_src),
					    GEN_INT (move_bytes),
					    align_rtx));
	    }

	  if (bytes > move_bytes)
	    {
	      if (! TARGET_POWERPC64)
		{
		  emit_insn (gen_addsi3 (src_reg, src_reg,
					 GEN_INT (move_bytes)));
		  emit_insn (gen_addsi3 (dest_reg, dest_reg,
					 GEN_INT (move_bytes)));
		}
	      else
		{
		  emit_insn (gen_adddi3 (src_reg, src_reg,
					 GEN_INT (move_bytes)));
		  emit_insn (gen_adddi3 (dest_reg, dest_reg,
					 GEN_INT (move_bytes)));
		}
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
	      src_addr = plus_constant (src_reg, offset);
	      dest_addr = plus_constant (dest_reg, offset);
	    }

	  /* Generate the appropriate load and store, saving the stores
	     for later.  */
	  if (bytes >= 8 && TARGET_POWERPC64
	      /* 64-bit loads and stores require word-aligned displacements. */
	      && (align >= 8 || (! STRICT_ALIGNMENT && align >= 4)))
	    {
	      move_bytes = 8;
	      tmp_reg = gen_reg_rtx (DImode);
	      emit_insn (gen_movdi (tmp_reg,
				    expand_block_move_mem (DImode,
							   src_addr,
							   orig_src)));
	      stores[num_reg++] = gen_movdi (expand_block_move_mem (DImode,
								    dest_addr,
								    orig_dest),
					     tmp_reg);
	    }
	  else if (bytes >= 4 && (align >= 4 || ! STRICT_ALIGNMENT))
	    {
	      move_bytes = 4;
	      tmp_reg = gen_reg_rtx (SImode);
	      emit_insn (gen_movsi (tmp_reg,
				    expand_block_move_mem (SImode,
							   src_addr,
							   orig_src)));
	      stores[num_reg++] = gen_movsi (expand_block_move_mem (SImode,
								    dest_addr,
								    orig_dest),
					     tmp_reg);
	    }
	  else if (bytes >= 2 && (align >= 2 || ! STRICT_ALIGNMENT))
	    {
	      move_bytes = 2;
	      tmp_reg = gen_reg_rtx (HImode);
	      emit_insn (gen_movhi (tmp_reg,
				    expand_block_move_mem (HImode,
							   src_addr,
							   orig_src)));
	      stores[num_reg++] = gen_movhi (expand_block_move_mem (HImode,
								    dest_addr,
								    orig_dest),
					     tmp_reg);
	    }
	  else
	    {
	      move_bytes = 1;
	      tmp_reg = gen_reg_rtx (QImode);
	      emit_insn (gen_movqi (tmp_reg,
				    expand_block_move_mem (QImode,
							   src_addr,
							   orig_src)));
	      stores[num_reg++] = gen_movqi (expand_block_move_mem (QImode,
								    dest_addr,
								    orig_dest),
					       tmp_reg);
	    }

	  if (num_reg >= MAX_MOVE_REG)
	    {
	      for (i = 0; i < num_reg; i++)
		emit_insn (stores[i]);
	      num_reg = 0;
	    }
	}

      for (i = 0; i < num_reg; i++)
	emit_insn (stores[i]);
    }

  return 1;
}


/* Return 1 if OP is a load multiple operation.  It is known to be a
   PARALLEL and the first section will be tested.  */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
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
     enum machine_mode mode ATTRIBUTE_UNUSED;
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

/* Return 1 for an PARALLEL suitable for mtcrf. */

int
mtcrf_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  int i;
  int bitmap = 0;
  rtx src_reg;

  /* Perform a quick check so we don't blow up below.  */
  if (count < 2
      || GET_CODE (XVECEXP (op, 0, 0)) != USE
      || GET_CODE (XEXP (XVECEXP (op, 0, 0), 0)) != CONST_INT
      || GET_CODE (XVECEXP (op, 0, 1)) != SET
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 1))) != UNSPEC
      || XVECLEN (SET_SRC (XVECEXP (op, 0, 1)), 0) != 2)
    return 0;
  src_reg = XVECEXP (SET_SRC (XVECEXP (op, 0, 1)), 0, 0);
  
  if (GET_CODE (src_reg) != REG
      || GET_MODE (src_reg) != SImode
      || ! INT_REGNO_P (REGNO (src_reg)))
    return 0;

  for (i = 1; i < count; i++)
    {
      rtx exp = XVECEXP (op, 0, i);
      rtx unspec;
      int maskval;
      
      if (GET_CODE (exp) != SET
	  || GET_CODE (SET_DEST (exp)) != REG
	  || GET_MODE (SET_DEST (exp)) != CCmode
	  || ! CR_REGNO_P (REGNO (SET_DEST (exp))))
	return 0;
      unspec = SET_SRC (exp);
      maskval = 1 << (MAX_CR_REGNO - REGNO (SET_DEST (exp)));
      bitmap |= maskval;
      
      if (GET_CODE (unspec) != UNSPEC
	  || XINT (unspec, 1) != 20
	  || XVECLEN (unspec, 0) != 2
	  || XVECEXP (unspec, 0, 0) != src_reg
	  || GET_CODE (XVECEXP (unspec, 0, 1)) != CONST_INT
	  || INTVAL (XVECEXP (unspec, 0, 1)) != maskval)
	return 0;
    }
  return INTVAL (XEXP (XVECEXP (op, 0, 0), 0)) == bitmap;
}

/* Return 1 for an PARALLEL suitable for lmw. */

int
lmw_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int base_regno;
  HOST_WIDE_INT offset;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  if (dest_regno > 31
      || count != 32 - dest_regno)
    return 0;

  if (LEGITIMATE_INDIRECT_ADDRESS_P (src_addr))
    {
      offset = 0;
      base_regno = REGNO (src_addr);
      if (base_regno == 0)
	return 0;
    }
  else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, src_addr))
    {
      offset = INTVAL (XEXP (src_addr, 1));
      base_regno = REGNO (XEXP (src_addr, 0));
    }
  else
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      rtx newaddr;
      rtx addr_reg;
      HOST_WIDE_INT newoffset;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode)
	return 0;
      newaddr = XEXP (SET_SRC (elt), 0);
      if (LEGITIMATE_INDIRECT_ADDRESS_P (newaddr))
	{
	  newoffset = 0;
	  addr_reg = newaddr;
	}
      else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, newaddr))
	{
	  addr_reg = XEXP (newaddr, 0);
	  newoffset = INTVAL (XEXP (newaddr, 1));
	}
      else
	return 0;
      if (REGNO (addr_reg) != base_regno
	  || newoffset != offset + 4 * i)
	return 0;
    }

  return 1;
}

/* Return 1 for an PARALLEL suitable for stmw. */

int
stmw_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  int base_regno;
  HOST_WIDE_INT offset;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  if (src_regno > 31
      || count != 32 - src_regno)
    return 0;

  if (LEGITIMATE_INDIRECT_ADDRESS_P (dest_addr))
    {
      offset = 0;
      base_regno = REGNO (dest_addr);
      if (base_regno == 0)
	return 0;
    }
  else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, dest_addr))
    {
      offset = INTVAL (XEXP (dest_addr, 1));
      base_regno = REGNO (XEXP (dest_addr, 0));
    }
  else
    return 0;

  for (i = 0; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      rtx newaddr;
      rtx addr_reg;
      HOST_WIDE_INT newoffset;

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode)
	return 0;
      newaddr = XEXP (SET_DEST (elt), 0);
      if (LEGITIMATE_INDIRECT_ADDRESS_P (newaddr))
	{
	  newoffset = 0;
	  addr_reg = newaddr;
	}
      else if (LEGITIMATE_OFFSET_ADDRESS_P (SImode, newaddr))
	{
	  addr_reg = XEXP (newaddr, 0);
	  newoffset = INTVAL (XEXP (newaddr, 1));
	}
      else
	return 0;
      if (REGNO (addr_reg) != base_regno
	  || newoffset != offset + 4 * i)
	return 0;
    }

  return 1;
}

/* Return 1 if OP is a comparison operation that is valid for a branch insn.
   We only check the opcode against the mode of the CC value here.  */

int
branch_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
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

int
trap_comparison_operator (op, mode)
    rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  return (GET_RTX_CLASS (GET_CODE (op)) == '<'
          || GET_CODE (op) == EQ || GET_CODE (op) == NE);
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
  unsigned HOST_WIDE_INT shift_mask = ~(unsigned HOST_WIDE_INT) 0;

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
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx in;
{
  int regno;

#if TARGET_ELF
  /* We can not copy a symbolic operand directly into anything other than
     BASE_REGS for TARGET_ELF.  So indicate that a register from BASE_REGS
     is needed as an intermediate register.  */
  if (class != BASE_REGS
      && (GET_CODE (in) == SYMBOL_REF
	  || GET_CODE (in) == HIGH
	  || GET_CODE (in) == LABEL_REF
	  || GET_CODE (in) == CONST))
    return BASE_REGS;
#endif

  if (GET_CODE (in) == REG)
    {
      regno = REGNO (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  regno = true_regnum (in);
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    regno = -1;
	}
    }
  else if (GET_CODE (in) == SUBREG)
    {
      regno = true_regnum (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = -1;
    }
  else
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
  rtx reg;

  if (GET_RTX_CLASS (code) != '<')
    return -1;

  reg = XEXP (op, 0);

  if (GET_CODE (reg) != REG
      || ! CR_REGNO_P (REGNO (reg)))
    abort ();

  cc_mode = GET_MODE (reg);
  cc_regnum = REGNO (reg);
  base_bit = 4 * (cc_regnum - CR0_REGNO);

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
    case GT:  case GTU:  case UNLE:
      return base_bit + 1;
    case LT:  case LTU:  case UNGE:
      return base_bit;
    case ORDERED:  case UNORDERED:
      return base_bit + 3;

    case GE:  case GEU:
      /* If floating-point, we will have done a cror to put the bit in the
	 unordered position.  So test that bit.  For integer, this is ! LT
	 unless this is an scc insn.  */
      return cc_mode == CCFPmode || scc_p ? base_bit + 3 : base_bit;

    case LE:  case LEU:
      return cc_mode == CCFPmode || scc_p ? base_bit + 3 : base_bit + 1;

    case UNEQ: case UNGT: case UNLT: case LTGT:
      return base_bit + 3;

    default:
      abort ();
    }
}

/* Given a comparison operation, say whether the bit tested (as returned
   by ccr_bit) should be negated.  */

static int
ccr_bit_negated_p (op)
     rtx op;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode mode = GET_MODE (XEXP (op, 0));
  
  if (code == EQ
      || code == LT || code == GT
      || code == LTU || code == GTU)
    return 0;
  else if (mode != CCFPmode
      || code == NE
      || code == ORDERED
      || code == UNGE || code == UNLE)
    return 1;
  else
    return 0;
}

/* Return the GOT register.  */

struct rtx_def *
rs6000_got_register (value)
     rtx value ATTRIBUTE_UNUSED;
{
  /* The second flow pass currently (June 1999) can't update regs_ever_live
     without disturbing other parts of the compiler, so update it here to
     make the prolog/epilogue code happy. */
  if (no_new_pseudos && ! regs_ever_live[PIC_OFFSET_TABLE_REGNUM])
    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;

  current_function_uses_pic_offset_table = 1;

  return pic_offset_table_rtx;
}

/* Define the structure for the machine field in struct function.  */
struct machine_function
{
  int sysv_varargs_p;
};

/* Functions to save and restore sysv_varargs_p.
   These will be called, via pointer variables,
   from push_function_context and pop_function_context.  */

void
rs6000_save_machine_status (p)
     struct function *p;
{
  struct machine_function *machine =
    (struct machine_function *) xmalloc (sizeof (struct machine_function));

  p->machine = machine;
  machine->sysv_varargs_p = rs6000_sysv_varargs_p;
}

void
rs6000_restore_machine_status (p)
     struct function *p;
{
  struct machine_function *machine = p->machine;

  rs6000_sysv_varargs_p = machine->sysv_varargs_p;

  free (machine);
  p->machine = (struct machine_function *)0;
}

/* Do anything needed before RTL is emitted for each function.  */

void
rs6000_init_expanders ()
{
  /* Reset varargs */
  rs6000_sysv_varargs_p = 0;

  /* Arrange to save and restore machine status around nested functions.  */
  save_machine_status = rs6000_save_machine_status;
  restore_machine_status = rs6000_restore_machine_status;
}


/* Print an operand.  Recognize special options, documented below.  */

#if TARGET_ELF
#define SMALL_DATA_RELOC ((rs6000_sdata == SDATA_EABI) ? "sda21" : "sdarel")
#define SMALL_DATA_REG ((rs6000_sdata == SDATA_EABI) ? 0 : 13)
#else
#define SMALL_DATA_RELOC "sda21"
#define SMALL_DATA_REG 0
#endif

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    int code;
{
  int i;
  HOST_WIDE_INT val;

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

    case '$':
      /* Write out either a '.' or '$' for the current location, depending
	 on whether this is Solaris or not.  */
      putc ((DEFAULT_ABI == ABI_SOLARIS) ? '.' : '$', file);
      return;

      /* %a is output_address.  */

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

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 0xffff);
      return;

    case 'B':
      /* If the low-order bit is zero, write 'r'; otherwise, write 'l'
	 for 64-bit mask direction.  */
      putc (((INT_LOWPART(x) & 1) == 0 ? 'r' : 'l'), file);
      return;

      /* %c is output_addr_const if a CONSTANT_ADDRESS_P, otherwise
	 output_operand.  */

    case 'C':
      {
	enum rtx_code code = GET_CODE (x);
	
	/* This is an optional cror needed for certain floating-point
	   comparisons.  Otherwise write nothing.  */
	if ((code == LE || code == GE
	     || code == UNEQ || code == LTGT
	     || code == UNGT || code == UNLT)
	    && GET_MODE (XEXP (x, 0)) == CCFPmode)
	  {
	    int base_bit = 4 * (REGNO (XEXP (x, 0)) - CR0_REGNO);
	    int bit0, bit1;
	    
	    if (code == UNEQ)
	      bit0 = 2;
	    else if (code == UNGT || code == GE)
	      bit0 = 1;
	    else
	      bit0 = 0;
	    if (code == LTGT)
	      bit1 = 1;
	    else if (code == LE || code == GE)
	      bit1 = 2;
	    else
	      bit1 = 3;
	    
	    fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		     base_bit + bit1, base_bit + bit0);
	  }
      }
      return;

    case 'D':
      /* Similar, except that this is for an scc, so we must be able to
	 encode the test in a single bit that is one.  We do the above
	 for any LE, GE, GEU, or LEU and invert the bit for NE.  */
      if (GET_CODE (x) == LE || GET_CODE (x) == GE
	  || GET_CODE (x) == LEU || GET_CODE (x) == GEU)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - CR0_REGNO);

	  fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2,
		   base_bit + (GET_CODE (x) == GE || GET_CODE (x) == GEU));
	}

      else if (GET_CODE (x) == NE)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - CR0_REGNO);

	  fprintf (file, "crnor %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2, base_bit + 2);
	}
      return;

    case 'E':
      /* X is a CR register.  Print the number of the third bit of the CR */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%E value");

      fprintf(file, "%d", 4 * (REGNO (x) - CR0_REGNO) + 3);
      return;

    case 'f':
      /* X is a CR register.  Print the shift count needed to move it
	 to the high-order four bits.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%f value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'F':
      /* Similar, but print the count for the rotate in the opposite
	 direction.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%F value");
      else
	fprintf (file, "%d", 32 - 4 * (REGNO (x) - CR0_REGNO));
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
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 31);
      else
	print_operand (file, x, 0);
      return;

    case 'H':
      /* If constant, output low-order six bits.  Otherwise,
	 write normally. */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 63);
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

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ~ INT_LOWPART (x));
      return;

    case 'K':
      /* X must be a symbolic constant on ELF.  Write an
	 expression suitable for an 'addi' that adds in the low 16
	 bits of the MEM.  */
      if (GET_CODE (x) != CONST)
	{
	  print_operand_address (file, x);
	  fputs ("@l", file);
	}
      else
	{
	  if (GET_CODE (XEXP (x, 0)) != PLUS
	      || (GET_CODE (XEXP (XEXP (x, 0), 0)) != SYMBOL_REF
		  && GET_CODE (XEXP (XEXP (x, 0), 0)) != LABEL_REF)
	      || GET_CODE (XEXP (XEXP (x, 0), 1)) != CONST_INT)
	    output_operand_lossage ("invalid %%l value");
	  print_operand_address (file, XEXP (XEXP (x, 0), 0));
	  fputs ("@l", file);
	  print_operand (file, XEXP (XEXP (x, 0), 1), 0);
	}
      return;

      /* %l is output_asm_label.  */

    case 'L':
      /* Write second word of DImode or DFmode reference.  Works on register
	 or non-indexed memory only.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 1]);
      else if (GET_CODE (x) == MEM)
	{
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of word.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant_for_output (XEXP (XEXP (x, 0), 0),
						      UNITS_PER_WORD));
	  else
	    output_address (plus_constant_for_output (XEXP (x, 0),
						      UNITS_PER_WORD));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
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
      if ((val & 0x80000000u) && ((val & 1) == 0))
	{
	  putc ('0', file);
	  return;
	}
      else if ((val & 0x80000000u) == 0)
	{
	  for (i = 1; i < 32; i++)
	    if ((val <<= 1) & 0x80000000u)
	      break;
	  fprintf (file, "%d", i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the right.  The result is its
	 number plus 1. We know the low-order bit is one.  */
      for (i = 0; i < 32; i++)
	if (((val >>= 1) & 1) == 0)
	  break;

      /* If we ended in ...01, i would be 0.  The correct value is 31, so
	 we want 31 - i.  */
      fprintf (file, "%d", 31 - i);
      return;

    case 'M':
      /* ME value for a mask operand.  */
      if (! mask_operand (x, VOIDmode))
	output_operand_lossage ("invalid %%M value");

      val = INT_LOWPART (x);

      /* If the low bit is set and the high bit is not, the value is 31.
	 If the low bit is zero, the value is the first 1 bit we find from
	 the right.  */
      if ((val & 1) && ((val & 0x80000000u) == 0))
	{
	  fputs ("31", file);
	  return;
	}
      else if ((val & 1) == 0)
	{
	  for (i = 0; i < 32; i++)
	    if ((val >>= 1) & 1)
	      break;

	  /* If we had ....10, i would be 0.  The result should be
	     30, so we need 30 - i.  */
	  fprintf (file, "%d", 30 - i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the left.  The result is its
	 number minus 1. We know the high-order bit is one.  */
      for (i = 0; i < 32; i++)
	if (((val <<= 1) & 0x80000000u) == 0)
	  break;

      fprintf (file, "%d", i);
      return;

      /* %n outputs the negative of its operand.  */

    case 'N':
      /* Write the number of elements in the vector times 4.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, "%d", XVECLEN (x, 0) * 4);
      return;

    case 'O':
      /* Similar, but subtract 1 first.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%O value");

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
	fprintf (file, "%d", 128 >> (REGNO (x) - CR0_REGNO));
      return;

    case 's':
      /* Low 5 bits of 32 - value */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%s value");

      fprintf (file, HOST_WIDE_INT_PRINT_DEC, (32 - INT_LOWPART (x)) & 31);
      return;

    case 'S':
      /* PowerPC64 mask position.  All 0's and all 1's are excluded.
	 CONST_INT 32-bit mask is considered sign-extended so any
	 transition must occur within the CONST_INT, not on the boundary.  */
      if (! mask64_operand (x, VOIDmode))
	output_operand_lossage ("invalid %%S value");

      val = INT_LOWPART (x);

      if (val & 1)      /* Clear Left */
	{
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT; i++)
	    if (!((val >>= 1) & 1))
	      break;

#if HOST_BITS_PER_WIDE_INT == 32
	  if (GET_CODE (x) == CONST_DOUBLE && i == 32)
	    {
	      val = CONST_DOUBLE_HIGH (x);

	      if (val == 0)
		--i;
	      else
		for (i = 32; i < 64; i++)
		  if (!((val >>= 1) & 1))
		    break;
	    }
#endif
	/* i = index of last set bit from right
	   mask begins at 63 - i from left */
	  if (i > 63)
	    output_operand_lossage ("%%S computed all 1's mask");
	  fprintf (file, "%d", 63 - i);
	  return;
	}
      else	/* Clear Right */
	{
	  for (i = 0; i < HOST_BITS_PER_WIDE_INT; i++)
	    if ((val >>= 1) & 1)
	      break;

#if HOST_BITS_PER_WIDE_INT == 32
	if (GET_CODE (x) == CONST_DOUBLE && i == 32)
	  {
	    val = CONST_DOUBLE_HIGH (x);

	    if (val == (HOST_WIDE_INT) -1)
	      --i;
	    else
	      for (i = 32; i < 64; i++)
		if ((val >>= 1) & 1)
		  break;
	  }
#endif
	/* i = index of last clear bit from right
	   mask ends at 62 - i from left */
	  if (i > 62)
	    output_operand_lossage ("%%S computed all 0's mask");
	  fprintf (file, "%d", 62 - i);
	  return;
	}

    case 't':
      /* Write 12 if this jump operation will branch if true, 4 otherwise. */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%t value");

      else if (! ccr_bit_negated_p (x))
	fputs ("12", file);
      else
	putc ('4', file);
      return;
      
    case 'T':
      /* Opposite of 't': write 4 if this jump operation will branch if true,
	 12 otherwise.   */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%T value");

      else if (! ccr_bit_negated_p (x))
	putc ('4', file);
      else
	fputs ("12", file);
      return;
      
    case 'u':
      /* High-order 16 bits of constant for use in unsigned operand.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%u value");

      fprintf (file, HOST_WIDE_INT_PRINT_HEX, 
	       (INT_LOWPART (x) >> 16) & 0xffff);
      return;

    case 'v':
      /* High-order 16 bits of constant for use in signed operand.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%v value");

      {
	int value = (INT_LOWPART (x) >> 16) & 0xffff;

	/* Solaris assembler doesn't like lis 0,0x8000 */
	if (DEFAULT_ABI == ABI_SOLARIS && (value & 0x8000) != 0)
	  fprintf (file, "%d", value | (~0 << 16));
	else
	  fprintf (file, "0x%x", value);
	return;
      }

    case 'U':
      /* Print `u' if this has an auto-increment or auto-decrement.  */
      if (GET_CODE (x) == MEM
	  && (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC))
	putc ('u', file);
      return;

    case 'V':
      /* Print the trap code for this operand.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("eq", file);   /* 4 */
	  break;
	case NE:
	  fputs ("ne", file);   /* 24 */
	  break;
	case LT:
	  fputs ("lt", file);   /* 16 */
	  break;
	case LE:
	  fputs ("le", file);   /* 20 */
	  break;
	case GT:
	  fputs ("gt", file);   /* 8 */
	  break;
	case GE:
	  fputs ("ge", file);   /* 12 */
	  break;
	case LTU:
	  fputs ("llt", file);  /* 2 */
	  break;
	case LEU:
	  fputs ("lle", file);  /* 6 */
	  break;
	case GTU:
	  fputs ("lgt", file);  /* 1 */
	  break;
	case GEU:
	  fputs ("lge", file);  /* 5 */
	  break;
	default:
	  abort ();
	}
      break;

    case 'w':
      /* If constant, low-order 16 bits of constant, signed.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, 
		 ((INT_LOWPART (x) & 0xffff) ^ 0x8000) - 0x8000);
      else
	print_operand (file, x, 0);
      return;

    case 'W':
      /* If constant, low-order 16 bits of constant, unsigned.
	 Otherwise, write normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INT_LOWPART (x) & 0xffff);
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
	fprintf (file, "%s", reg_names[REGNO (x) + 2]);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 8));
	  else
	    output_address (plus_constant (XEXP (x, 0), 8));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
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

      if (XSTR (x, 0)[0] != '.')
	{
	  switch (DEFAULT_ABI)
	    {
	    default:
	      abort ();

	    case ABI_AIX:
	      putc ('.', file);
	      break;

	    case ABI_V4:
	    case ABI_AIX_NODESC:
	    case ABI_SOLARIS:
	      break;
	    }
	}
#if TARGET_AIX
      RS6000_OUTPUT_BASENAME (file, XSTR (x, 0));
#else
      assemble_name (file, XSTR (x, 0));
#endif
      return;

    case 'Z':
      /* Like 'L', for last word of TImode.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x) + 3]);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 12));
	  else
	    output_address (plus_constant (XEXP (x, 0), 12));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
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
	    fprintf (file, "%d(%s)", GET_MODE_SIZE (GET_MODE (x)),
		     reg_names[REGNO (XEXP (XEXP (x, 0), 0))]);
	  else if (GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fprintf (file, "%d(%s)", - GET_MODE_SIZE (GET_MODE (x)),
		     reg_names[REGNO (XEXP (XEXP (x, 0), 0))]);
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
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST
	   || GET_CODE (x) == LABEL_REF)
    {
      output_addr_const (file, x);
      if (small_data_operand (x, GET_MODE (x)))
	fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		 reg_names[SMALL_DATA_REG]);
      else if (TARGET_TOC)
	abort();
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
    {
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (XEXP (x, 1)));
      fprintf (file, "(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#if TARGET_ELF
  else if (GET_CODE (x) == LO_SUM && GET_CODE (XEXP (x, 0)) == REG
           && CONSTANT_P (XEXP (x, 1)))
    {
      output_addr_const (file, XEXP (x, 1));
      fprintf (file, "@l(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#endif
  else if (LEGITIMATE_CONSTANT_POOL_ADDRESS_P (x))
    {
      if (TARGET_AIX)
	{
	  rtx contains_minus = XEXP (x, 1); 
	  rtx minus;
	  
	  /* Find the (minus (sym) (toc)) buried in X, and temporarily
	     turn it into (sym) for output_addr_const. */
	  while (GET_CODE (XEXP (contains_minus, 0)) != MINUS)
	    contains_minus = XEXP (contains_minus, 0);

	  minus = XEXP (contains_minus, 0); 
	  XEXP (contains_minus, 0) = XEXP (minus, 0);
	  output_addr_const (file, XEXP (x, 1)); 	  
	  XEXP (contains_minus, 0) = minus;
	}
      else
	output_addr_const (file, XEXP (x, 1));

      fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
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
    if (regs_ever_live[first_reg] 
	&& (! call_used_regs[first_reg]
	    || (first_reg == PIC_OFFSET_TABLE_REGNUM
		&& (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
		&& flag_pic == 1)))
      break;

  if (profile_flag)
    {
      /* AIX must save/restore every register that contains a parameter
	 before/after the .__mcount call plus an additional register
	 for the static chain, if needed; use registers from 30 down to 22
	 to do this.  */
      if (DEFAULT_ABI == ABI_AIX)
	{
	  int last_parm_reg, profile_first_reg;

	  /* Figure out last used parameter register.  The proper thing
	     to do is to walk incoming args of the function.  A function
	     might have live parameter registers even if it has no
	     incoming args.  */
	  for (last_parm_reg = 10;
	       last_parm_reg > 2 && ! regs_ever_live [last_parm_reg];
	       last_parm_reg--)
	    ;

	  /* Calculate first reg for saving parameter registers
	     and static chain.
	     Skip reg 31 which may contain the frame pointer.  */
	  profile_first_reg = (33 - last_parm_reg
			       - (current_function_needs_context ? 1 : 0));
	  /* Do not save frame pointer if no parameters needs to be saved.  */
	  if (profile_first_reg == 31)
	    profile_first_reg = 32;

	  if (first_reg > profile_first_reg)
	    first_reg = profile_first_reg;
	}

      /* SVR4 may need one register to preserve the static chain.  */
      else if (current_function_needs_context)
	{
	  /* Skip reg 31 which may contain the frame pointer.  */
	  if (first_reg > 30)
	    first_reg = 30;
	}
    }

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

/* Calculate the stack information for the current function.  This is
   complicated by having two separate calling sequences, the AIX calling
   sequence and the V.4 calling sequence.

   AIX stack frames look like:
							  32-bit  64-bit
	SP---->	+---------------------------------------+
		| back chain to caller			| 0	  0
		+---------------------------------------+
		| saved CR				| 4       8 (8-11)
		+---------------------------------------+
		| saved LR				| 8       16
		+---------------------------------------+
		| reserved for compilers		| 12      24
		+---------------------------------------+
		| reserved for binders			| 16      32
		+---------------------------------------+
		| saved TOC pointer			| 20      40
		+---------------------------------------+
		| Parameter save area (P)		| 24      48
		+---------------------------------------+
		| Alloca space (A)			| 24+P    etc.
		+---------------------------------------+
		| Local variable space (L)		| 24+P+A
		+---------------------------------------+
		| Float/int conversion temporary (X)	| 24+P+A+L
		+---------------------------------------+
		| Save area for GP registers (G)	| 24+P+A+X+L
		+---------------------------------------+
		| Save area for FP registers (F)	| 24+P+A+X+L+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   The required alignment for AIX configurations is two words (i.e., 8
   or 16 bytes).


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
		| Float/int conversion temporary (X)	| 8+P+A+V+L
		+---------------------------------------+
		| saved CR (C)				| 8+P+A+V+L+X
		+---------------------------------------+    
		| Save area for GP registers (G)	| 8+P+A+V+L+X+C
		+---------------------------------------+    
		| Save area for FP registers (F)	| 8+P+A+V+L+X+C+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   The required alignment for V.4 is 16 bytes, or 8 bytes if -meabi is
   given.  (But note below and in sysv4.h that we require only 8 and
   may round up the size of our stack frame anyways.  The historical
   reason is early versions of powerpc-linux which didn't properly
   align the stack at program startup.  A happy side-effect is that
   -mno-eabi libraries can be used with -meabi programs.)


   The EABI configuration defaults to the V.4 layout, unless
   -mcall-aix is used, in which case the AIX layout is used.  However,
   the stack alignment requirements may differ.  If -mno-eabi is not
   given, the required stack alignment is 8 bytes; if -mno-eabi is
   given, the required alignment is 16 bytes.  (But see V.4 comment
   above.)  */

#ifndef ABI_STACK_BOUNDARY
#define ABI_STACK_BOUNDARY STACK_BOUNDARY
#endif

rs6000_stack_t *
rs6000_stack_info ()
{
  static rs6000_stack_t info, zero_info;
  rs6000_stack_t *info_ptr = &info;
  int reg_size = TARGET_POWERPC64 ? 8 : 4;
  enum rs6000_abi abi;
  int total_raw_size;

  /* Zero all fields portably */
  info = zero_info;

  /* Select which calling sequence */
  info_ptr->abi = abi = DEFAULT_ABI;

  /* Calculate which registers need to be saved & save area size */
  info_ptr->first_gp_reg_save = first_reg_to_save ();
  /* Assume that we will have to save PIC_OFFSET_TABLE_REGNUM, 
     even if it currently looks like we won't.  */
  if (flag_pic == 1 
      && (abi == ABI_V4 || abi == ABI_SOLARIS)
      && info_ptr->first_gp_reg_save > PIC_OFFSET_TABLE_REGNUM)
    info_ptr->gp_size = reg_size * (32 - PIC_OFFSET_TABLE_REGNUM);
  else
    info_ptr->gp_size = reg_size * (32 - info_ptr->first_gp_reg_save);

  info_ptr->first_fp_reg_save = first_fp_reg_to_save ();
  info_ptr->fp_size = 8 * (64 - info_ptr->first_fp_reg_save);

  /* Does this function call anything? */
  info_ptr->calls_p = ! current_function_is_leaf;

  /* Determine if we need to save the link register */
  if (regs_ever_live[LINK_REGISTER_REGNUM]
      || (DEFAULT_ABI == ABI_AIX && profile_flag)
#ifdef TARGET_RELOCATABLE
      || (TARGET_RELOCATABLE && (get_pool_size () != 0))
#endif
      || (info_ptr->first_fp_reg_save != 64
	  && !FP_SAVE_INLINE (info_ptr->first_fp_reg_save))
      || (abi == ABI_V4 && current_function_calls_alloca)
      || (abi == ABI_SOLARIS && current_function_calls_alloca)
      || info_ptr->calls_p)
    {
      info_ptr->lr_save_p = 1;
      regs_ever_live[LINK_REGISTER_REGNUM] = 1;
    }

  /* Determine if we need to save the condition code registers.  */
  if (regs_ever_live[CR2_REGNO] 
      || regs_ever_live[CR3_REGNO]
      || regs_ever_live[CR4_REGNO])
    {
      info_ptr->cr_save_p = 1;
      if (abi == ABI_V4 || abi == ABI_SOLARIS)
	info_ptr->cr_size = reg_size;
    }

  /* Determine various sizes */
  info_ptr->reg_size     = reg_size;
  info_ptr->fixed_size   = RS6000_SAVE_AREA;
  info_ptr->varargs_size = RS6000_VARARGS_AREA;
  info_ptr->vars_size    = RS6000_ALIGN (get_frame_size (), 8);
  info_ptr->parm_size    = RS6000_ALIGN (current_function_outgoing_args_size, 8);
  info_ptr->save_size    = RS6000_ALIGN (info_ptr->fp_size
				  + info_ptr->gp_size
				  + info_ptr->cr_size
				  + info_ptr->lr_size
				  + info_ptr->toc_size, 8);

  /* Calculate the offsets */
  switch (abi)
    {
    case ABI_NONE:
    default:
      abort ();

    case ABI_AIX:
    case ABI_AIX_NODESC:
      info_ptr->fp_save_offset   = - info_ptr->fp_size;
      info_ptr->gp_save_offset   = info_ptr->fp_save_offset - info_ptr->gp_size;
      info_ptr->cr_save_offset   = reg_size; /* first word when 64-bit.  */
      info_ptr->lr_save_offset   = 2*reg_size;
      break;

    case ABI_V4:
    case ABI_SOLARIS:
      info_ptr->fp_save_offset   = - info_ptr->fp_size;
      info_ptr->gp_save_offset   = info_ptr->fp_save_offset - info_ptr->gp_size;
      info_ptr->cr_save_offset   = info_ptr->gp_save_offset - info_ptr->cr_size;
      info_ptr->toc_save_offset  = info_ptr->cr_save_offset - info_ptr->toc_size;
      info_ptr->lr_save_offset   = reg_size;
      break;
    }

  total_raw_size	 = (info_ptr->vars_size
			    + info_ptr->parm_size
			    + info_ptr->save_size
			    + info_ptr->varargs_size
			    + info_ptr->fixed_size);

  info_ptr->total_size   = RS6000_ALIGN (total_raw_size, ABI_STACK_BOUNDARY / BITS_PER_UNIT);

  /* Determine if we need to allocate any stack frame:

     For AIX we need to push the stack if a frame pointer is needed (because
     the stack might be dynamically adjusted), if we are debugging, if we
     make calls, or if the sum of fp_save, gp_save, and local variables
     are more than the space needed to save all non-volatile registers:
     32-bit: 18*8 + 19*4 = 220 or 64-bit: 18*8 + 18*8 = 288 (GPR13 reserved).

     For V.4 we don't have the stack cushion that AIX uses, but assume that
     the debugger can handle stackless frames.  */

  if (info_ptr->calls_p)
    info_ptr->push_p = 1;

  else if (abi == ABI_V4 || abi == ABI_SOLARIS)
    info_ptr->push_p = (total_raw_size > info_ptr->fixed_size
			|| info_ptr->calls_p);

  else
    info_ptr->push_p = (frame_pointer_needed
			|| write_symbols != NO_DEBUG
			|| ((total_raw_size - info_ptr->fixed_size)
			    > (TARGET_32BIT ? 220 : 288)));

  /* Zero offsets if we're not saving those registers */
  if (info_ptr->fp_size == 0)
    info_ptr->fp_save_offset = 0;

  if (info_ptr->gp_size == 0)
    info_ptr->gp_save_offset = 0;

  if (! info_ptr->lr_save_p)
    info_ptr->lr_save_offset = 0;

  if (! info_ptr->cr_save_p)
    info_ptr->cr_save_offset = 0;

  if (! info_ptr->toc_save_p)
    info_ptr->toc_save_offset = 0;

  return info_ptr;
}

void
debug_stack_info (info)
     rs6000_stack_t *info;
{
  const char *abi_string;

  if (! info)
    info = rs6000_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  switch (info->abi)
    {
    default:		 abi_string = "Unknown";	break;
    case ABI_NONE:	 abi_string = "NONE";		break;
    case ABI_AIX:	 abi_string = "AIX";		break;
    case ABI_AIX_NODESC: abi_string = "AIX";		break;
    case ABI_V4:	 abi_string = "V.4";		break;
    case ABI_SOLARIS:	 abi_string = "Solaris";	break;
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

  if (info->toc_save_p)
    fprintf (stderr, "\ttoc_save_p          = %5d\n", info->toc_save_p);

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

  if (info->toc_save_offset)
    fprintf (stderr, "\ttoc_save_offset     = %5d\n", info->toc_save_offset);

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

 if (info->lr_size)
    fprintf (stderr, "\tlr_size             = %5d\n", info->cr_size);

  if (info->cr_size)
    fprintf (stderr, "\tcr_size             = %5d\n", info->cr_size);

 if (info->toc_size)
    fprintf (stderr, "\ttoc_size            = %5d\n", info->toc_size);

  if (info->save_size)
    fprintf (stderr, "\tsave_size           = %5d\n", info->save_size);

  if (info->reg_size != 4)
    fprintf (stderr, "\treg_size            = %5d\n", info->reg_size);

  fprintf (stderr, "\n");
}

/* Emit instructions needed to load the TOC register.
   This is only needed when TARGET_TOC, TARGET_MINIMAL_TOC, and there is
   a constant pool; or for SVR4 -fpic.  */

void
rs6000_emit_load_toc_table (fromprolog)
     int fromprolog;
{
  rtx dest;
  dest = gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);

  if (TARGET_ELF)
    {
      if ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS) 
	  && flag_pic == 1)
	{
	  rtx temp = (fromprolog 
		      ? gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)
		      : gen_reg_rtx (Pmode));
	  if (TARGET_32BIT)
	    emit_insn (gen_load_toc_v4_pic_si (temp));
	  else
	    emit_insn (gen_load_toc_v4_pic_di (temp));
	  emit_move_insn (dest, temp);
	}
      else if (flag_pic == 2)
        {
	  char buf[30];
	  rtx tempLR = (fromprolog 
			? gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM)
			: gen_reg_rtx (Pmode));
	  rtx temp0 = (fromprolog
			? gen_rtx_REG (Pmode, 0)
			: gen_reg_rtx (Pmode));
	  rtx symF;

	  /* possibly create the toc section */
	  if (! toc_initialized)
	    {
	      toc_section ();
	      function_section (current_function_decl);
	    }
  
	  if (fromprolog)
	    {
	      rtx symL;
	  
	      ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	      symF = gen_rtx_SYMBOL_REF (Pmode, ggc_alloc_string (buf, -1));

	      ASM_GENERATE_INTERNAL_LABEL (buf, "LCL", rs6000_pic_labelno);
	      symL = gen_rtx_SYMBOL_REF (Pmode, ggc_alloc_string (buf, -1));

	      emit_insn (gen_load_toc_v4_PIC_1 (tempLR, symF));
	      emit_move_insn (dest, tempLR);
	      emit_insn (gen_load_toc_v4_PIC_2 (temp0, dest, symL, symF));
	    }
	  else
	    {
	      rtx tocsym;
	      static int reload_toc_labelno = 0;

	      tocsym = gen_rtx_SYMBOL_REF (Pmode, 
		      ggc_alloc_string (toc_label_name, -1));

	      ASM_GENERATE_INTERNAL_LABEL (buf, "LCG", reload_toc_labelno++);
	      symF = gen_rtx_SYMBOL_REF (Pmode, ggc_alloc_string (buf, -1));

	      emit_insn (gen_load_toc_v4_PIC_1b (tempLR, symF, tocsym));
	      emit_move_insn (dest, tempLR);
	      emit_move_insn (temp0, gen_rtx_MEM (Pmode, dest));
	    }
	  emit_insn (gen_addsi3 (dest, temp0, dest));
	}
      else if (flag_pic == 0 && TARGET_MINIMAL_TOC)
        {
	  /* This is for AIX code running in non-PIC ELF.  */
	  char buf[30];
	  rtx realsym;
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
	  realsym = gen_rtx_SYMBOL_REF (Pmode, ggc_alloc_string (buf, -1));
	  
	  emit_insn (gen_elf_high (dest, realsym));
	  emit_insn (gen_elf_low (dest, dest, realsym));
	}
      else
        abort();
    }
  else
    {
      if (TARGET_32BIT)
        emit_insn (gen_load_toc_aix_si (dest));
      else
        emit_insn (gen_load_toc_aix_di (dest));
    }
}

int   
get_TOC_alias_set ()
{
    static int set = -1;
    if (set == -1)
      set = new_alias_set ();
    return set;
}   

/* This retuns nonzero if the current function uses the TOC.  This is
   determined by the presence of (unspec ... 7), which is generated by
   the various load_toc_* patterns.  */
int
uses_TOC () 
{
    rtx insn;

    for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	{
	  rtx pat = PATTERN (insn);
	  int i;

	  if (GET_CODE(pat) == PARALLEL) 
	    for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	      if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == UNSPEC 
		 && XINT (XVECEXP (PATTERN (insn), 0, i), 1) == 7)
		  return 1;
	}
    return 0;
}

rtx
create_TOC_reference(symbol) 
    rtx symbol;
{
    return gen_rtx_PLUS (Pmode, 
	     gen_rtx_REG (Pmode, TOC_REGISTER),
	       gen_rtx_CONST (Pmode, 
		 gen_rtx_MINUS (Pmode, symbol, 
		   gen_rtx_SYMBOL_REF (Pmode,
		     ggc_alloc_string (toc_label_name, -1)))));
}

#if TARGET_AIX
/* __throw will restore its own return address to be the same as the
   return address of the function that the throw is being made to.
   This is unfortunate, because we want to check the original
   return address to see if we need to restore the TOC.
   So we have to squirrel it away here.  
   This is used only in compiling __throw and __rethrow. 

   Most of this code should be removed by CSE.  */
static rtx insn_after_throw;

/* This does the saving... */
void
rs6000_aix_emit_builtin_unwind_init ()
{
  rtx mem;
  rtx stack_top = gen_reg_rtx (Pmode);
  rtx opcode_addr = gen_reg_rtx (Pmode);

  insn_after_throw = gen_reg_rtx (SImode);

  mem = gen_rtx_MEM (Pmode, hard_frame_pointer_rtx);
  emit_move_insn (stack_top, mem);

  mem = gen_rtx_MEM (Pmode, 
		     gen_rtx_PLUS (Pmode, stack_top, 
				   GEN_INT (2 * GET_MODE_SIZE (Pmode))));
  emit_move_insn (opcode_addr, mem);
  emit_move_insn (insn_after_throw, gen_rtx_MEM (SImode, opcode_addr));
}

/* Emit insns to _restore_ the TOC register, at runtime (specifically in _eh.o).
   Only used on AIX.

   The idea is that on AIX, function calls look like this:
	bl  somefunction-trampoline
	lwz r2,20(sp)

    and later,
	somefunction-trampoline:
	stw r2,20(sp)
	 ... load function address in the count register ...
	bctr
   or like this, if the linker determines that this is not a cross-module call
   and so the TOC need not be restored:
	bl  somefunction
	nop
   or like this, if the compiler could determine that this is not a
   cross-module call:
	bl  somefunction
   now, the tricky bit here is that register 2 is saved and restored
   by the _linker_, so we can't readily generate debugging information
   for it.  So we need to go back up the call chain looking at the
   insns at return addresses to see which calls saved the TOC register
   and so see where it gets restored from.

   Oh, and all this gets done in RTL inside the eh_epilogue pattern,
   just before the actual epilogue.

   On the bright side, this incurs no space or time overhead unless an
   exception is thrown, except for the extra code in libgcc.a.  

   The parameter STACKSIZE is a register containing (at runtime)
   the amount to be popped off the stack in addition to the stack frame
   of this routine (which will be __throw or __rethrow, and so is
   guaranteed to have a stack frame).  */
void
rs6000_emit_eh_toc_restore (stacksize)
     rtx stacksize;
{
  rtx top_of_stack;
  rtx bottom_of_stack = gen_reg_rtx (Pmode);
  rtx tocompare = gen_reg_rtx (SImode);
  rtx opcode = gen_reg_rtx (SImode);
  rtx opcode_addr = gen_reg_rtx (Pmode);
  rtx mem;
  rtx loop_start = gen_label_rtx ();
  rtx no_toc_restore_needed = gen_label_rtx ();
  rtx loop_exit = gen_label_rtx ();
  
  mem = gen_rtx_MEM (Pmode, hard_frame_pointer_rtx);
  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;
  emit_move_insn (bottom_of_stack, mem);

  top_of_stack = expand_binop (Pmode, add_optab, 
			       bottom_of_stack, stacksize,
			       NULL_RTX, 1, OPTAB_WIDEN);

  emit_move_insn (tocompare, 
		  GEN_INT (trunc_int_for_mode (TARGET_32BIT 
					       ? 0x80410014 
					       : 0xE8410028, SImode)));

  if (insn_after_throw == NULL_RTX)
    abort();
  emit_move_insn (opcode, insn_after_throw);
  
  emit_note (NULL_PTR, NOTE_INSN_LOOP_BEG);
  emit_label (loop_start);
  
  do_compare_rtx_and_jump (opcode, tocompare, NE, 1,
			   SImode, NULL_RTX, 0, NULL_RTX,
			   no_toc_restore_needed);
  
  mem = gen_rtx_MEM (Pmode, 
		     gen_rtx_PLUS (Pmode, bottom_of_stack, 
				   GEN_INT (5 * GET_MODE_SIZE (Pmode))));
  emit_move_insn (gen_rtx_REG (Pmode, 2), mem);

  emit_label (no_toc_restore_needed);
  do_compare_rtx_and_jump (top_of_stack, bottom_of_stack, EQ, 1,
			   Pmode, NULL_RTX, 0, NULL_RTX,
			   loop_exit);

  mem = gen_rtx_MEM (Pmode, bottom_of_stack);
  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;
  emit_move_insn (bottom_of_stack, mem);
  
  mem = gen_rtx_MEM (Pmode, 
		     gen_rtx_PLUS (Pmode, bottom_of_stack, 
				   GEN_INT (2 * GET_MODE_SIZE (Pmode))));
  emit_move_insn (opcode_addr, mem);
  emit_move_insn (opcode, gen_rtx_MEM (SImode, opcode_addr));

  emit_note (NULL_PTR, NOTE_INSN_LOOP_CONT);
  emit_jump (loop_start);
  emit_note (NULL_PTR, NOTE_INSN_LOOP_END);
  emit_label (loop_exit);
}
#endif /* TARGET_AIX */

/* This ties together stack memory 
   (MEM with an alias set of rs6000_sr_alias_set)
   and the change to the stack pointer.  */
static void
rs6000_emit_stack_tie ()
{
  rtx mem;
  mem = gen_rtx_MEM (BLKmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM));
  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;
  emit_insn (gen_stack_tie (mem));
}

/* Emit the correct code for allocating stack space, as insns.
   If COPY_R12, make sure a copy of the old frame is left in r12.
   The generated code may use hard register 0 as a temporary.  */

static void
rs6000_emit_allocate_stack (size, copy_r12)
     HOST_WIDE_INT size;
     int copy_r12;
{
  rtx insn;
  rtx stack_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx tmp_reg = gen_rtx_REG (Pmode, 0);
  rtx todec = GEN_INT (-size);

  if (current_function_limit_stack)
    {
      if (REG_P (stack_limit_rtx)
	  && REGNO (stack_limit_rtx) > 1 
	  && REGNO (stack_limit_rtx) <= 31)
	{
	  emit_insn (Pmode == SImode
		     ? gen_addsi3 (tmp_reg,
				   stack_limit_rtx,
				   GEN_INT (size))
		     : gen_adddi3 (tmp_reg,
				   stack_limit_rtx,
				   GEN_INT (size)));
	  
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg,
				    const0_rtx));
	}
      else if (GET_CODE (stack_limit_rtx) == SYMBOL_REF
	       && TARGET_32BIT
	       && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS))
	{
	  rtx toload = gen_rtx_CONST (VOIDmode,
				      gen_rtx_PLUS (Pmode, 
						    stack_limit_rtx, 
						    GEN_INT (size)));
	  
	  emit_insn (gen_elf_high (tmp_reg, toload));
	  emit_insn (gen_elf_low (tmp_reg, tmp_reg, toload));
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg,
				    const0_rtx));
	}
      else
	warning ("stack limit expression is not supported");
    }

  if (copy_r12 || ! TARGET_UPDATE)
    emit_move_insn (gen_rtx_REG (Pmode, 12), stack_reg);

  if (TARGET_UPDATE)
    {
      if (size > 32767)
	{
	  /* Need a note here so that try_split doesn't get confused.  */
	  if (get_last_insn() == NULL_RTX)
	    emit_note (0, NOTE_INSN_DELETED);
	  insn = emit_move_insn (tmp_reg, todec);
	  try_split (PATTERN (insn), insn, 0);
	  todec = tmp_reg;
	}
      
      if (Pmode == SImode)
	insn = emit_insn (gen_movsi_update (stack_reg, stack_reg, 
					    todec, stack_reg));
      else
	insn = emit_insn (gen_movdi_update (stack_reg, stack_reg, 
					    todec, stack_reg));
    }
  else
    {
      if (Pmode == SImode)
	insn = emit_insn (gen_addsi3 (stack_reg, stack_reg, todec));
      else
	insn = emit_insn (gen_adddi3 (stack_reg, stack_reg, todec));
      emit_move_insn (gen_rtx_MEM (Pmode, stack_reg),
		      gen_rtx_REG (Pmode, 12));
    }
  
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = 
    gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
		       gen_rtx_SET (VOIDmode, stack_reg, 
				    gen_rtx_PLUS (Pmode, stack_reg,
						  GEN_INT (-size))),
		       REG_NOTES (insn));
}

/* Add to 'insn' a note which is PATTERN (INSN) but with REG replaced with
   (plus:P (reg 1) VAL), and with REG2 replaced with RREG if REG2 is not 
   NULL.  
   It would be nice if dwarf2out_frame_debug_expr could deduce these
   equivalences by itself so it wasn't necessary to hold its hand so much.  */

static void
rs6000_frame_related (insn, reg, val, reg2, rreg)
     rtx insn;
     rtx reg;
     HOST_WIDE_INT val;
     rtx reg2;
     rtx rreg;
{
  rtx real, temp;

  real = copy_rtx (PATTERN (insn));

  real = replace_rtx (real, reg, 
		      gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode,
							STACK_POINTER_REGNUM),
				    GEN_INT (val)));
  
  /* We expect that 'real' is either a SET or a PARALLEL containing
     SETs (and possibly other stuff).  In a PARALLEL, all the SETs
     are important so they all have to be marked RTX_FRAME_RELATED_P.  */

  if (GET_CODE (real) == SET)
    {
      rtx set = real;
      
      temp = simplify_rtx (SET_SRC (set));
      if (temp)
	SET_SRC (set) = temp;
      temp = simplify_rtx (SET_DEST (set));
      if (temp)
	SET_DEST (set) = temp;
      if (GET_CODE (SET_DEST (set)) == MEM)
	{
	  temp = simplify_rtx (XEXP (SET_DEST (set), 0));
	  if (temp)
	    XEXP (SET_DEST (set), 0) = temp;
	}
    }
  else if (GET_CODE (real) == PARALLEL)
    {
      int i;
      for (i = 0; i < XVECLEN (real, 0); i++)
	if (GET_CODE (XVECEXP (real, 0, i)) == SET)
	  {
	    rtx set = XVECEXP (real, 0, i);
	    
	    temp = simplify_rtx (SET_SRC (set));
	    if (temp)
	      SET_SRC (set) = temp;
	    temp = simplify_rtx (SET_DEST (set));
	    if (temp)
	      SET_DEST (set) = temp;
	    if (GET_CODE (SET_DEST (set)) == MEM)
	      {
		temp = simplify_rtx (XEXP (SET_DEST (set), 0));
		if (temp)
		  XEXP (SET_DEST (set), 0) = temp;
	      }
	    RTX_FRAME_RELATED_P (set) = 1;
	  }
    }
  else
    abort();
  
  if (reg2 != NULL_RTX)
    real = replace_rtx (real, reg2, rreg);
  
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					real,
					REG_NOTES (insn));
}

/* Emit function prologue as insns.  */

void
rs6000_emit_prologue()
{
  rs6000_stack_t *info = rs6000_stack_info ();
  enum machine_mode reg_mode = TARGET_POWERPC64 ? DImode : SImode;
  int reg_size = TARGET_POWERPC64 ? 8 : 4;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx frame_ptr_rtx = gen_rtx_REG (Pmode, 12);
  rtx frame_reg_rtx = sp_reg_rtx;
  rtx cr_save_rtx = NULL;
  rtx insn;
  int saving_FPRs_inline;
  int using_store_multiple;
  HOST_WIDE_INT sp_offset = 0;
  
  using_store_multiple = (TARGET_MULTIPLE && ! TARGET_POWERPC64
			  && info->first_gp_reg_save < 31);
  saving_FPRs_inline = (info->first_fp_reg_save == 64
			|| FP_SAVE_INLINE (info->first_fp_reg_save));

  /* For V.4, update stack before we do any saving and set back pointer.  */
  if (info->push_p && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS))
    {
      if (info->total_size < 32767)
	sp_offset = info->total_size;
      else
	frame_reg_rtx = frame_ptr_rtx;
      rs6000_emit_allocate_stack (info->total_size, 
				  (frame_reg_rtx != sp_reg_rtx
				   && (info->cr_save_p
				       || info->lr_save_p
				       || info->first_fp_reg_save < 64
				       || info->first_gp_reg_save < 32
				       )));
      if (frame_reg_rtx != sp_reg_rtx)
	rs6000_emit_stack_tie ();
    }

  /* If we use the link register, get it into r0.  */
  if (info->lr_save_p)
    emit_move_insn (gen_rtx_REG (Pmode, 0), 
		    gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM));

  /* If we need to save CR, put it into r12.  */
  if (info->cr_save_p && frame_reg_rtx != frame_ptr_rtx)
    {
      cr_save_rtx = gen_rtx_REG (SImode, 12);
      emit_insn (gen_movesi_from_cr (cr_save_rtx));
    }

  /* Do any required saving of fpr's.  If only one or two to save, do it
     ourself.  Otherwise, call function.  */
  if (saving_FPRs_inline)
    {
      int i;
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	if ((regs_ever_live[info->first_fp_reg_save+i] 
	     && ! call_used_regs[info->first_fp_reg_save+i]))
	  {
	    rtx addr, reg, mem;
	    reg = gen_rtx_REG (DFmode, info->first_fp_reg_save + i);
	    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
				 GEN_INT (info->fp_save_offset 
					  + sp_offset 
					  + 8*i));
	    mem = gen_rtx_MEM (DFmode, addr);
	    MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	    insn = emit_move_insn (mem, reg);
	    rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
				  NULL_RTX, NULL_RTX);
	  }
    }
  else if (info->first_fp_reg_save != 64)
    {
      int i;
      char rname[30];
      char *alloc_rname;
      rtvec p;
      p = rtvec_alloc (2 + 64 - info->first_fp_reg_save);
      
      RTVEC_ELT (p, 0) = gen_rtx_CLOBBER (VOIDmode, 
					  gen_rtx_REG (Pmode, 
						       LINK_REGISTER_REGNUM));
      sprintf (rname, "%s%d%s", SAVE_FP_PREFIX,
	       info->first_fp_reg_save - 32, SAVE_FP_SUFFIX);
      alloc_rname = ggc_alloc_string (rname, -1);
      RTVEC_ELT (p, 1) = gen_rtx_USE (VOIDmode,
				      gen_rtx_SYMBOL_REF (Pmode,
							  alloc_rname));
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	{
	  rtx addr, reg, mem;
	  reg = gen_rtx_REG (DFmode, info->first_fp_reg_save + i);
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->fp_save_offset 
					+ sp_offset + 8*i));
	  mem = gen_rtx_MEM (DFmode, addr);
	  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	  RTVEC_ELT (p, i + 2) = gen_rtx_SET (VOIDmode, mem, reg);
	}
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    NULL_RTX, NULL_RTX);
    }

  /* Save GPRs.  This is done as a PARALLEL if we are using
     the store-multiple instructions.  */
  if (using_store_multiple)
    {
      rtvec p, dwarfp;
      int i;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      dwarfp = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	{
	  rtx addr, reg, mem;
	  reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
			       GEN_INT (info->gp_save_offset 
					+ sp_offset 
					+ reg_size * i));
	  mem = gen_rtx_MEM (reg_mode, addr);
	  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	  RTVEC_ELT (p, i) = gen_rtx_SET (VOIDmode, mem, reg);
	}
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    NULL_RTX, NULL_RTX);
    }
  else
    {
      int i;
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	if ((regs_ever_live[info->first_gp_reg_save+i] 
	     && ! call_used_regs[info->first_gp_reg_save+i])
	    || (i+info->first_gp_reg_save == PIC_OFFSET_TABLE_REGNUM
		&& (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
		&& flag_pic == 1))
	  {
	    rtx addr, reg, mem;
	    reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);
	    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
				 GEN_INT (info->gp_save_offset 
					  + sp_offset 
					  + reg_size * i));
	    mem = gen_rtx_MEM (reg_mode, addr);
	    MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	    insn = emit_move_insn (mem, reg);
	    rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
				  NULL_RTX, NULL_RTX);
	  }
    }

  /* Save lr if we used it.  */
  if (info->lr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->lr_save_offset + sp_offset));
      rtx reg = gen_rtx_REG (Pmode, 0);
      rtx mem = gen_rtx_MEM (Pmode, addr);
      /* This should not be of rs6000_sr_alias_set, because of
	 __builtin_return_address.  */
      
      insn = emit_move_insn (mem, reg);
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    reg, gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM));
    }

  /* Save CR if we use any that must be preserved.  */
  if (info->cr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->cr_save_offset + sp_offset));
      rtx mem = gen_rtx_MEM (SImode, addr);
      MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

      /* If r12 was used to hold the original sp, copy cr into r0 now
	 that it's free.  */
      if (REGNO (frame_reg_rtx) == 12)
	{
	  cr_save_rtx = gen_rtx_REG (SImode, 0);
	  emit_insn (gen_movesi_from_cr (cr_save_rtx));
	}
      insn = emit_move_insn (mem, cr_save_rtx);

      /* Now, there's no way that dwarf2out_frame_debug_expr is going
	 to understand '(unspec:SI [(reg:CC 68) ...] 19)'.  But that's
	 OK.  All we have to do is specify that _one_ condition code
	 register is saved in this stack slot.  The thrower's epilogue
	 will then restore all the call-saved registers.  */
      rs6000_frame_related (insn, frame_ptr_rtx, info->total_size, 
			    cr_save_rtx, gen_rtx_REG (SImode, CR0_REGNO));
    }

  /* Update stack and set back pointer unless this is V.4, 
     for which it was done previously.  */
  if (info->push_p && DEFAULT_ABI != ABI_V4 && DEFAULT_ABI != ABI_SOLARIS)
    rs6000_emit_allocate_stack (info->total_size, FALSE);

  /* Set frame pointer, if needed.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (gen_rtx_REG (reg_mode, FRAME_POINTER_REGNUM), 
			     sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If we are using PIC_OFFSET_TABLE_REGNUM, we need to set it up.  */
  if ((TARGET_TOC && TARGET_MINIMAL_TOC && get_pool_size () != 0)
      || ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS) 
	  && flag_pic == 1 && regs_ever_live[PIC_OFFSET_TABLE_REGNUM]))
  {
    /* If emit_load_toc_table will use the link register, we need to save
       it.  We use R11 for this purpose because emit_load_toc_table
       can use register 0.  This allows us to use a plain 'blr' to return
       from the procedure more often.  */
    int save_LR_around_toc_setup = (TARGET_ELF && flag_pic != 0 && 
				    ! info->lr_save_p);
    if (save_LR_around_toc_setup)
      emit_move_insn (gen_rtx_REG (Pmode, 11), 
		      gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM));
    
    rs6000_emit_load_toc_table (TRUE);

    if (save_LR_around_toc_setup)
      emit_move_insn (gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM), 
		      gen_rtx_REG (Pmode, 11));
  }
}


/* Write function prologue.  */
void
output_prolog (file, size)
     FILE *file;
     int size ATTRIBUTE_UNUSED;
{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (TARGET_DEBUG_STACK)
    debug_stack_info (info);

  /* Write .extern for any function we will call to save and restore fp
     values.  */
  if (info->first_fp_reg_save < 64 && !FP_SAVE_INLINE (info->first_fp_reg_save))
    fprintf (file, "\t.extern %s%d%s\n\t.extern %s%d%s\n",
	     SAVE_FP_PREFIX, info->first_fp_reg_save - 32, SAVE_FP_SUFFIX,
	     RESTORE_FP_PREFIX, info->first_fp_reg_save - 32, RESTORE_FP_SUFFIX);

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

  if (! HAVE_prologue)
    {
      start_sequence ();
      
      /* A NOTE_INSN_DELETED is supposed to be at the start
	 and end of the "toplevel" insn chain.  */
      emit_note (0, NOTE_INSN_DELETED);
      rs6000_emit_prologue ();
      emit_note (0, NOTE_INSN_DELETED);
      
      if (TARGET_DEBUG_STACK)
	debug_rtx_list (get_insns(), 100);
      final (get_insns(), file, FALSE, FALSE);
      end_sequence ();
    }

  rs6000_pic_labelno++;
}
  
/* Emit function epilogue as insns.

   At present, dwarf2out_frame_debug_expr doesn't understand
   register restores, so we don't bother setting RTX_FRAME_RELATED_P
   anywhere in the epilogue.  Most of the insns below would in any case
   need special notes to explain where r11 is in relation to the stack.  */

void
rs6000_emit_epilogue(sibcall)
     int sibcall;
{
  rs6000_stack_t *info;
  int restoring_FPRs_inline;
  int using_load_multiple;
  int using_mfcr_multiple;
  int use_backchain_to_restore_sp;
  int sp_offset = 0;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, 1);
  rtx frame_reg_rtx = sp_reg_rtx;
  enum machine_mode reg_mode = TARGET_POWERPC64 ? DImode : SImode;
  int reg_size = TARGET_POWERPC64 ? 8 : 4;
  int i;

  info = rs6000_stack_info ();
  using_load_multiple = (TARGET_MULTIPLE && ! TARGET_POWERPC64
			 && info->first_gp_reg_save < 31);
  restoring_FPRs_inline = (sibcall
			   || info->first_fp_reg_save == 64
			   || FP_SAVE_INLINE (info->first_fp_reg_save));
  use_backchain_to_restore_sp = (frame_pointer_needed 
				 || current_function_calls_alloca
				 || info->total_size > 32767);
  using_mfcr_multiple = (rs6000_cpu == PROCESSOR_PPC601
			 || rs6000_cpu == PROCESSOR_PPC603
			 || rs6000_cpu == PROCESSOR_PPC750
			 || optimize_size);

  /* If we have a frame pointer, a call to alloca,  or a large stack
     frame, restore the old stack pointer using the backchain.  Otherwise,
     we know what size to update it with.  */
  if (use_backchain_to_restore_sp)
    {
      /* Under V.4, don't reset the stack pointer until after we're done
	 loading the saved registers.  */
      if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
	frame_reg_rtx = gen_rtx_REG (Pmode, 11);

      emit_move_insn (frame_reg_rtx,
		      gen_rtx_MEM (Pmode, sp_reg_rtx));
      
    }
  else if (info->push_p)
    {
      if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
	sp_offset = info->total_size;
      else
	{
	  emit_insn (TARGET_32BIT
		     ? gen_addsi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (info->total_size))
		     : gen_adddi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (info->total_size)));
	}
    }
  
  /* Get the old lr if we saved it.  */
  if (info->lr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->lr_save_offset + sp_offset));
      rtx mem = gen_rtx_MEM (Pmode, addr);
      MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

      emit_move_insn (gen_rtx_REG (Pmode, 0), mem);
    }
  
  /* Get the old cr if we saved it.  */
  if (info->cr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->cr_save_offset + sp_offset));
      rtx mem = gen_rtx_MEM (SImode, addr);
      MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

      emit_move_insn (gen_rtx_REG (SImode, 12), mem);
    }
  
  /* Set LR here to try to overlap restores below.  */
  if (info->lr_save_p)
    emit_move_insn (gen_rtx_REG (Pmode, LINK_REGISTER_REGNUM),
		    gen_rtx_REG (Pmode, 0));
  
  
  /* Restore GPRs.  This is done as a PARALLEL if we are using
     the load-multiple instructions.  */
  if (using_load_multiple)
    {
      rtvec p;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	{
	  rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
				   GEN_INT (info->gp_save_offset 
					    + sp_offset 
					    + reg_size * i));
	  rtx mem = gen_rtx_MEM (reg_mode, addr);
	  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	  RTVEC_ELT (p, i) = 
	    gen_rtx_SET (VOIDmode,
			 gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			 mem);
	}
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }
  else
    for (i = 0; i < 32 - info->first_gp_reg_save; i++)
      if ((regs_ever_live[info->first_gp_reg_save+i] 
	   && ! call_used_regs[info->first_gp_reg_save+i])
	  || (i+info->first_gp_reg_save == PIC_OFFSET_TABLE_REGNUM
	      && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
	      && flag_pic == 1))
	{
	  rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, 
				   GEN_INT (info->gp_save_offset 
					    + sp_offset 
					    + reg_size * i));
	  rtx mem = gen_rtx_MEM (reg_mode, addr);
	  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	  emit_move_insn (gen_rtx_REG (reg_mode, 
				       info->first_gp_reg_save + i),
			  mem);
	}

  /* Restore fpr's if we need to do it without calling a function.  */
  if (restoring_FPRs_inline)
    for (i = 0; i < 64 - info->first_fp_reg_save; i++)
      if ((regs_ever_live[info->first_fp_reg_save+i] 
	   && ! call_used_regs[info->first_fp_reg_save+i]))
	{
	  rtx addr, mem;
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->fp_save_offset 
					+ sp_offset 
					+ 8*i));
	  mem = gen_rtx_MEM (DFmode, addr);
	  MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	  emit_move_insn (gen_rtx_REG (DFmode, 
				       info->first_fp_reg_save + i),
			  mem);
	}

  /* If we saved cr, restore it here.  Just those that were used.  */
  if (info->cr_save_p)
    {
      rtx r12_rtx = gen_rtx_REG (SImode, 12);
      
      if (using_mfcr_multiple)
	{
	  rtvec p;
	  int mask = 0;
	  int count = 0;

	  for (i = 0; i < 8; i++)
	    if (regs_ever_live[CR0_REGNO+i] && ! call_used_regs[CR0_REGNO+i])
	      {
		mask |= 1 << (7-i);
		count++;
	      }
	  if (count == 0)
	    abort();
	  
	  p = rtvec_alloc (count + 1);

	  RTVEC_ELT (p, 0) = gen_rtx_USE (VOIDmode, GEN_INT (mask));
	  count = 1;
	  for (i = 0; i < 8; i++)
	    if (regs_ever_live[CR0_REGNO+i] && ! call_used_regs[CR0_REGNO+i])
	      {
		rtvec r = rtvec_alloc (2);
		RTVEC_ELT (r, 0) = r12_rtx;
		RTVEC_ELT (r, 1) = GEN_INT (1 << (7-i));
		RTVEC_ELT (p, count) =
		  gen_rtx_SET (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO+i), 
			       gen_rtx_UNSPEC (CCmode, r, 20));
		count++;
	      }
	  emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
	}
      else
	for (i = 0; i < 8; i++)
	  if (regs_ever_live[CR0_REGNO+i] && ! call_used_regs[CR0_REGNO+i])
	    {
	      emit_insn (gen_movsi_to_cr_one (gen_rtx_REG (CCmode, 
							   CR0_REGNO+i),
					      r12_rtx));
	    }
    }

  /* If this is V.4, unwind the stack pointer after all of the loads
     have been done.  We need to emit a block here so that sched
     doesn't decide to move the sp change before the register restores
     (which may not have any obvious dependency on the stack).  This
     doesn't hurt performance, because there is no scheduling that can
     be done after this point.  */
  if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
    {
      if (frame_reg_rtx != sp_reg_rtx)
	  rs6000_emit_stack_tie ();

      if (use_backchain_to_restore_sp)
	{
	  emit_move_insn (sp_reg_rtx, frame_reg_rtx);
	}
      else if (sp_offset != 0)
	{
	  emit_insn (Pmode == SImode
		     ? gen_addsi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (sp_offset))
		     : gen_adddi3 (sp_reg_rtx, sp_reg_rtx,
				   GEN_INT (sp_offset)));
	}
    }

  if (!sibcall)
    {
      rtvec p;
      if (! restoring_FPRs_inline)
	p = rtvec_alloc (3 + 64 - info->first_fp_reg_save);
      else
	p = rtvec_alloc (2);

      RTVEC_ELT (p, 0) = gen_rtx_USE (VOIDmode, 
				      gen_rtx_REG (Pmode, 
						   LINK_REGISTER_REGNUM));
      RTVEC_ELT (p, 1) = gen_rtx_RETURN (VOIDmode);

      /* If we have to restore more than two FP registers, branch to the
	 restore function.  It will return to our caller.  */
      if (! restoring_FPRs_inline)
	{
	  int i;
	  char rname[30];
	  char *alloc_rname;

	  sprintf (rname, "%s%d%s", RESTORE_FP_PREFIX, 
		   info->first_fp_reg_save - 32, RESTORE_FP_SUFFIX);
	  alloc_rname = ggc_alloc_string (rname, -1);
	  RTVEC_ELT (p, 2) = gen_rtx_USE (VOIDmode,
					  gen_rtx_SYMBOL_REF (Pmode,
							      alloc_rname));

	  for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	    {
	      rtx addr, mem;
	      addr = gen_rtx_PLUS (Pmode, sp_reg_rtx,
				   GEN_INT (info->fp_save_offset + 8*i));
	      mem = gen_rtx_MEM (DFmode, addr);
	      MEM_ALIAS_SET (mem) = rs6000_sr_alias_set;

	      RTVEC_ELT (p, i+3) = 
		gen_rtx_SET (VOIDmode,
			     gen_rtx_REG (DFmode, info->first_fp_reg_save + i),
			     mem);
	    }
	}
      
      emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size ATTRIBUTE_UNUSED;
{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (! HAVE_epilogue)
    {
      rtx insn = get_last_insn ();
      /* If the last insn was a BARRIER, we don't have to write anything except
	 the trace table.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn == 0 ||  GET_CODE (insn) != BARRIER)
	{
	  /* This is slightly ugly, but at least we don't have two
	     copies of the epilogue-emitting code.  */
	  start_sequence ();

	  /* A NOTE_INSN_DELETED is supposed to be at the start
	     and end of the "toplevel" insn chain.  */
	  emit_note (0, NOTE_INSN_DELETED);
	  rs6000_emit_epilogue (FALSE);
	  emit_note (0, NOTE_INSN_DELETED);

	  if (TARGET_DEBUG_STACK)
	    debug_rtx_list (get_insns(), 100);
	  final (get_insns(), file, FALSE, FALSE);
	  end_sequence ();
	}
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
     different traceback table.  */
  if (DEFAULT_ABI == ABI_AIX && ! flag_inhibit_size_directive)
    {
      const char *fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
      int fixed_parms, float_parms, parm_info;
      int i;

      while (*fname == '.')	/* V.4 encodes . in the name */
	fname++;

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
      fputs ("\t.long 0\n", file);

      /* Tbtab format type.  Use format type 0.  */
      fputs ("\t.byte 0,", file);

      /* Language type.  Unfortunately, there doesn't seem to be any
	 official way to get this info, so we use language_string.  C
	 is 0.  C++ is 9.  No number defined for Obj-C, so use the
	 value for C for now.  There is no official value for Java,
         although IBM appears to be using 13.  There is no official value
	 for Chill, so we've choosen 44 pseudo-randomly.  */
      if (! strcmp (language_string, "GNU C")
	  || ! strcmp (language_string, "GNU Obj-C"))
	i = 0;
      else if (! strcmp (language_string, "GNU F77"))
	i = 1;
      else if (! strcmp (language_string, "GNU Ada"))
	i = 3;
      else if (! strcmp (language_string, "GNU Pascal"))
	i = 2;
      else if (! strcmp (language_string, "GNU C++"))
	i = 9;
      else if (! strcmp (language_string, "GNU Java"))
	i = 13;
      else if (! strcmp (language_string, "GNU CHILL"))
	i = 44;
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
      fputs ("\t.long ", file);
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
#if TARGET_AIX
      RS6000_OUTPUT_BASENAME (file, fname);
#else
      assemble_name (file, fname);
#endif
      fputs ("-.", file);
#if TARGET_AIX
      RS6000_OUTPUT_BASENAME (file, fname);
#else
      assemble_name (file, fname);
#endif
      putc ('\n', file);

      /* Interrupt handler mask.  */
      /* Omit this long, since we never set the interrupt handler bit
	 above.  */

      /* Number of CTL (controlled storage) anchors.  */
      /* Omit this long, since the has_ctl bit is never set above.  */

      /* Displacement into stack of each CTL anchor.  */
      /* Omit this list of longs, because there are no CTL anchors.  */

      /* Length of function name.  */
      fprintf (file, "\t.short %d\n", (int) strlen (fname));

      /* Function name.  */
      assemble_string (fname, strlen (fname));

      /* Register for alloca automatic storage; this is always reg 31.
	 Only emit this if the alloca bit was set above.  */
      if (frame_pointer_needed)
	fputs ("\t.byte 31\n", file);
    }
}

/* A C compound statement that outputs the assembler code for a thunk function,
   used to implement C++ virtual function calls with multiple inheritance.  The
   thunk acts as a wrapper around a virtual function, adjusting the implicit
   object parameter before handing control off to the real function.

   First, emit code to add the integer DELTA to the location that contains the
   incoming first argument.  Assume that this argument contains a pointer, and
   is the one used to pass the `this' pointer in C++.  This is the incoming
   argument *before* the function prologue, e.g. `%o0' on a sparc.  The
   addition must preserve the values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does not touch
   the return address.  Hence returning from FUNCTION will return to whoever
   called the current `thunk'.

   The effect must be as if FUNCTION had been called directly with the adjusted
   first argument.  This macro is responsible for emitting all of the code for
   a thunk function; `FUNCTION_PROLOGUE' and `FUNCTION_EPILOGUE' are not
   invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already been
   extracted from it.)  It might possibly be useful on some targets, but
   probably not.

   If you do not define this macro, the target-independent code in the C++
   frontend will generate a less efficient heavyweight thunk that calls
   FUNCTION instead of jumping to it.  The generic approach does not support
   varargs.  */

void
output_mi_thunk (file, thunk_fndecl, delta, function)
     FILE *file;
     tree thunk_fndecl ATTRIBUTE_UNUSED;
     int delta;
     tree function;
{
  const char *this_reg = reg_names[ aggregate_value_p (TREE_TYPE (TREE_TYPE (function))) ? 4 : 3 ];
  const char *prefix;
  const char *fname;
  const char *r0	 = reg_names[0];
  const char *toc	 = reg_names[2];
  const char *schain	 = reg_names[11];
  const char *r12	 = reg_names[12];
  char buf[512];
  static int labelno = 0;

  /* Small constants that can be done by one add instruction */
  if (delta >= -32768 && delta <= 32767)
    {
      if (! TARGET_NEW_MNEMONICS)
	fprintf (file, "\tcal %s,%d(%s)\n", this_reg, delta, this_reg);
      else
	fprintf (file, "\taddi %s,%s,%d\n", this_reg, this_reg, delta);
    }

  /* Large constants that can be done by one addis instruction */
  else if ((delta & 0xffff) == 0 && num_insns_constant_wide (delta) == 1)
    asm_fprintf (file, "\t{cau|addis} %s,%s,%d\n", this_reg, this_reg,
		 delta >> 16);

  /* 32-bit constants that can be done by an add and addis instruction.  */
  else if (TARGET_32BIT || num_insns_constant_wide (delta) == 1)
    {
      /* Break into two pieces, propagating the sign bit from the low word to
	 the upper word.  */
      int delta_high = delta >> 16;
      int delta_low  = delta & 0xffff;
      if ((delta_low & 0x8000) != 0)
	{
	  delta_high++;
	  delta_low = (delta_low ^ 0x8000) - 0x8000;	/* sign extend */
	}

      asm_fprintf (file, "\t{cau|addis} %s,%s,%d\n", this_reg, this_reg,
		   delta_high);

      if (! TARGET_NEW_MNEMONICS)
	fprintf (file, "\tcal %s,%d(%s)\n", this_reg, delta_low, this_reg);
      else
	fprintf (file, "\taddi %s,%s,%d\n", this_reg, this_reg, delta_low);
    }

  /* 64-bit constants, fixme */
  else
    abort ();

  /* Get the prefix in front of the names.  */
  switch (DEFAULT_ABI)
    {
    default:
      abort ();

    case ABI_AIX:
      prefix = ".";
      break;

    case ABI_V4:
    case ABI_AIX_NODESC:
    case ABI_SOLARIS:
      prefix = "";
      break;
    }

  /* If the function is compiled in this module, jump to it directly.
     Otherwise, load up its address and jump to it.  */

  fname = XSTR (XEXP (DECL_RTL (function), 0), 0);

  if (current_file_function_operand (XEXP (DECL_RTL (function), 0), VOIDmode)
      && ! lookup_attribute ("longcall",
			     TYPE_ATTRIBUTES (TREE_TYPE (function))))
    {
      fprintf (file, "\tb %s", prefix);
      assemble_name (file, fname);
      if (DEFAULT_ABI == ABI_V4 && flag_pic) fputs ("@local", file);
      putc ('\n', file);
    }

  else
    {
      switch (DEFAULT_ABI)
	{
	default:
	  abort ();

	case ABI_AIX:
	  /* Set up a TOC entry for the function.  */
	  ASM_GENERATE_INTERNAL_LABEL (buf, "Lthunk", labelno);
	  toc_section ();
	  ASM_OUTPUT_INTERNAL_LABEL (file, "Lthunk", labelno);
	  labelno++;

	  /* Note, MINIMAL_TOC doesn't make sense in the case of a thunk, since
	     there will be only one TOC entry for this function.  */
	  fputs ("\t.tc\t", file);
	  assemble_name (file, buf);
	  fputs ("[TC],", file);
	  assemble_name (file, buf);
	  putc ('\n', file);
	  text_section ();
	  asm_fprintf (file, (TARGET_32BIT) ? "\t{l|lwz} %s," : "\tld %s", r12);
	  assemble_name (file, buf);
	  asm_fprintf (file, "(%s)\n", reg_names[2]);
	  asm_fprintf (file,
		       (TARGET_32BIT) ? "\t{l|lwz} %s,0(%s)\n" : "\tld %s,0(%s)\n",
		       r0, r12);

	  asm_fprintf (file,
		       (TARGET_32BIT) ? "\t{l|lwz} %s,4(%s)\n" : "\tld %s,8(%s)\n",
		       toc, r12);

	  asm_fprintf (file, "\tmtctr %s\n", r0);
	  asm_fprintf (file,
		       (TARGET_32BIT) ? "\t{l|lwz} %s,8(%s)\n" : "\tld %s,16(%s)\n",
		       schain, r12);

	  asm_fprintf (file, "\tbctr\n");
	  break;

	case ABI_AIX_NODESC:
	case ABI_SOLARIS:
	case ABI_V4:
	  fprintf (file, "\tb %s", prefix);
	  assemble_name (file, fname);
	  if (flag_pic) fputs ("@plt", file);
	  putc ('\n', file);
	  break;
	}
    }
}


/* A quick summary of the various types of 'constant-pool tables'
   under PowerPC:

   Target	Flags		Name		One table per	
   AIX		(none)		AIX TOC		object file
   AIX		-mfull-toc	AIX TOC		object file
   AIX		-mminimal-toc	AIX minimal TOC	translation unit
   SVR4/EABI	(none)		SVR4 SDATA	object file
   SVR4/EABI	-fpic		SVR4 pic	object file
   SVR4/EABI	-fPIC		SVR4 PIC	translation unit
   SVR4/EABI	-mrelocatable	EABI TOC	function
   SVR4/EABI	-maix		AIX TOC		object file
   SVR4/EABI	-maix -mminimal-toc 
				AIX minimal TOC	translation unit

   Name			Reg.	Set by	entries	      contains:
					made by	 addrs?	fp?	sum?

   AIX TOC		2	crt0	as	 Y	option	option
   AIX minimal TOC	30	prolog	gcc	 Y	Y	option
   SVR4 SDATA		13	crt0	gcc	 N	Y	N
   SVR4 pic		30	prolog	ld	 Y	not yet	N
   SVR4 PIC		30	prolog	gcc	 Y	option	option
   EABI TOC		30	prolog	gcc	 Y	option	option

*/

/* Hash table stuff for keeping track of TOC entries.  */

struct toc_hash_struct 
{
  /* `key' will satisfy CONSTANT_P; in fact, it will satisfy
     ASM_OUTPUT_SPECIAL_POOL_ENTRY_P.  */
  rtx key;
  int labelno;
};

static htab_t toc_hash_table;

/* Hash functions for the hash table.  */

static unsigned
rs6000_hash_constant (k)
     rtx k;
{
  unsigned result = GET_CODE (k);
  const char *format = GET_RTX_FORMAT (GET_CODE (k));
  int flen = strlen (format);
  int fidx;
  
  if (GET_CODE (k) == LABEL_REF)
    return result * 1231 + XINT (XEXP (k, 0), 3);

  if (GET_CODE (k) == CONST_DOUBLE)
    fidx = 2;
  else if (GET_CODE (k) == CODE_LABEL)
    fidx = 3;
  else
    fidx = 0;

  for (; fidx < flen; fidx++)
    switch (format[fidx])
      {
      case 's':
	{
	  unsigned i, len;
	  const char *str = XSTR (k, fidx);
	  len = strlen (str);
	  result = result * 613 + len;
	  for (i = 0; i < len; i++)
	    result = result * 613 + (unsigned) str[i];
	  break;
	}
      case 'u':
      case 'e':
	result = result * 1231 + rs6000_hash_constant (XEXP (k, fidx));
	break;
      case 'i':
      case 'n':
	result = result * 613 + (unsigned) XINT (k, fidx);
	break;
      case 'w':
	if (sizeof (unsigned) >= sizeof (HOST_WIDE_INT))
	  result = result * 613 + (unsigned) XWINT (k, fidx);
	else
	  {
	    size_t i;
	    for (i = 0; i < sizeof(HOST_WIDE_INT)/sizeof(unsigned); i++)
	      result = result * 613 + (unsigned) (XWINT (k, fidx)
						  >> CHAR_BIT * i);
	  }
	break;
      default:
	abort();
      }
  return result;
}

static unsigned
toc_hash_function (hash_entry)
     const void * hash_entry;
{
  return rs6000_hash_constant (((const struct toc_hash_struct *) 
				hash_entry)->key);
}

/* Compare H1 and H2 for equivalence.  */

static int
toc_hash_eq (h1, h2)
     const void * h1;
     const void * h2;
{
  rtx r1 = ((const struct toc_hash_struct *) h1)->key;
  rtx r2 = ((const struct toc_hash_struct *) h2)->key;

  /* Gotcha:  One of these const_doubles will be in memory.
     The other may be on the constant-pool chain.
     So rtx_equal_p will think they are different... */
  if (r1 == r2)
    return 1;
  if (GET_CODE (r1) != GET_CODE (r2)
      || GET_MODE (r1) != GET_MODE (r2))
    return 0;
  if (GET_CODE (r1) == CONST_DOUBLE)
    {
      int format_len = strlen (GET_RTX_FORMAT (CONST_DOUBLE));
      int i;
      for (i = 2; i < format_len; i++)
	if (XWINT (r1, i) != XWINT (r2, i))
	  return 0;
      
      return 1;
    }
  else if (GET_CODE (r1) == LABEL_REF)
    return XINT (XEXP (r1, 0), 3) == XINT (XEXP (r2, 0), 3);
  else
    return rtx_equal_p (r1, r2);
}

/* Mark the hash table-entry HASH_ENTRY.  */

static int
toc_hash_mark_entry (hash_slot, unused)
     void * hash_slot;
     void * unused ATTRIBUTE_UNUSED;
{
  const struct toc_hash_struct * hash_entry = 
    *(const struct toc_hash_struct **) hash_slot;
  rtx r = hash_entry->key;
  ggc_set_mark (hash_entry);
  /* For CODE_LABELS, we don't want to drag in the whole insn chain... */
  if (GET_CODE (r) == LABEL_REF)
    {
      ggc_set_mark (r);
      ggc_set_mark (XEXP (r, 0));
    }
  else
    ggc_mark_rtx (r);
  return 1;
}

/* Mark all the elements of the TOC hash-table *HT.  */

static void
toc_hash_mark_table (vht)
     void *vht;
{
  htab_t *ht = vht;
  
  htab_traverse (*ht, toc_hash_mark_entry, (void *)0);
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
  const char *name = buf;
  const char *real_name;
  rtx base = x;
  int offset = 0;

  if (TARGET_NO_TOC)
    abort ();

  /* When the linker won't eliminate them, don't output duplicate
     TOC entries (this happens on AIX if there is any kind of TOC,
     and on SVR4 under -fPIC or -mrelocatable).
     This won't work if we are not garbage collecting, so 
     we don't do it, sorry.  */
  if (TARGET_TOC && ggc_p)
    {
      struct toc_hash_struct *h;
      void * * found;
      
      h = ggc_alloc (sizeof (*h));
      h->key = x;
      h->labelno = labelno;
      
      found = htab_find_slot (toc_hash_table, h, 1);
      if (*found == NULL)
	*found = h;
      else  /* This is indeed a duplicate.  
	       Set this label equal to that label.  */
	{
	  fputs ("\t.set ", file);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d,", labelno);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d\n", ((*(const struct toc_hash_struct **) 
					      found)->labelno));
	  return;
	}
    }

  /* If we're going to put a double constant in the TOC, make sure it's
     aligned properly when strict alignment is on.  */
  if (GET_CODE (x) == CONST_DOUBLE
      && STRICT_ALIGNMENT
      && GET_MODE (x) == DFmode
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)) {
    ASM_OUTPUT_ALIGN (file, 3);
  }

  ASM_OUTPUT_INTERNAL_LABEL (file, "LC", labelno);

  /* Handle FP constants specially.  Note that if we have a minimal
     TOC, things we put here aren't actually in the TOC, so we can allow
     FP constants.  */
  if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC))
    {
      REAL_VALUE_TYPE rv;
      long k[2];

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_DOUBLE (rv, k);

      if (TARGET_64BIT)
	{
	  if (TARGET_MINIMAL_TOC)
	    fprintf (file, "\t.llong 0x%lx%08lx\n", k[0], k[1]);
	  else
	    fprintf (file, "\t.tc FD_%lx_%lx[TC],0x%lx%08lx\n",
		     k[0], k[1], k[0] & 0xffffffffu, k[1] & 0xffffffffu);
	  return;
	}
      else
	{
	  if (TARGET_MINIMAL_TOC)
	    fprintf (file, "\t.long 0x%lx\n\t.long 0x%lx\n", k[0], k[1]);
	  else
	    fprintf (file, "\t.tc FD_%lx_%lx[TC],0x%lx,0x%lx\n",
		     k[0], k[1], k[0], k[1]);
	  return;
	}
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode
	   && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC))
    {
      REAL_VALUE_TYPE rv;
      long l;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);

      if (TARGET_64BIT)
	{
	  if (TARGET_MINIMAL_TOC)
	    fprintf (file, "\t.llong 0x%lx00000000\n", l);
	  else
	    fprintf (file, "\t.tc FS_%lx[TC],0x%lx00000000\n", l, l);
	  return;
	}
      else
	{
	  if (TARGET_MINIMAL_TOC)
	    fprintf (file, "\t.long 0x%lx\n", l);
	  else
	    fprintf (file, "\t.tc FS_%lx[TC],0x%lx\n", l, l);
	  return;
	}
    }
  else if (GET_MODE (x) == DImode
	   && (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE)
	   && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC))
    {
      HOST_WIDE_INT low;
      HOST_WIDE_INT high;

      if (GET_CODE (x) == CONST_DOUBLE)
	{
	  low = CONST_DOUBLE_LOW (x);
	  high = CONST_DOUBLE_HIGH (x);
	}
      else
#if HOST_BITS_PER_WIDE_INT == 32
	{
	  low = INTVAL (x);
	  high = (low < 0) ? ~0 : 0;
	}
#else
	{
          low = INTVAL (x) & 0xffffffff;
          high = (HOST_WIDE_INT) INTVAL (x) >> 32;
	}
#endif

      if (TARGET_64BIT)
	{
	  if (TARGET_MINIMAL_TOC)
	    fprintf (file, "\t.llong 0x%lx%08lx\n", (long)high, (long)low);
	  else
	    fprintf (file, "\t.tc ID_%lx_%lx[TC],0x%lx%08lx\n",
		     (long)high, (long)low, (long)high, (long)low);
	  return;
	}
      else
	{
	  if (TARGET_MINIMAL_TOC)
	    fprintf (file, "\t.long 0x%lx\n\t.long 0x%lx\n",
		     (long)high, (long)low);
	  else
	    fprintf (file, "\t.tc ID_%lx_%lx[TC],0x%lx,0x%lx\n",
		     (long)high, (long)low, (long)high, (long)low);
	  return;
	}
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

  STRIP_NAME_ENCODING (real_name, name);
  if (TARGET_MINIMAL_TOC)
    fputs (TARGET_32BIT ? "\t.long " : "\t.llong ", file);
  else
    {
      fprintf (file, "\t.tc %s", real_name);

      if (offset < 0)
	fprintf (file, ".N%d", - offset);
      else if (offset)
	fprintf (file, ".P%d", offset);

      fputs ("[TC],", file);
    }

  /* Currently C++ toc references to vtables can be emitted before it
     is decided whether the vtable is public or private.  If this is
     the case, then the linker will eventually complain that there is
     a TOC reference to an unknown section.  Thus, for vtables only,
     we emit the TOC reference to reference the symbol and not the
     section.  */
  if (! strncmp ("_vt.", name, 4))
    {
      assemble_name (file, name);
      if (offset < 0)
	fprintf (file, "%d", offset);
      else if (offset > 0)
	fprintf (file, "+%d", offset);
    }
  else
    output_addr_const (file, x);
  putc ('\n', file);
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
     const char *p;
     int n;
{
  char c;
  int i, count_string;
  const char *for_string = "\t.byte \"";
  const char *for_decimal = "\t.byte ";
  const char *to_close = NULL;

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
    fputs (to_close, file);
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
     const char *filename;
     const char *section_desc;
{
  const char *q, *after_last_slash, *last_period = 0;
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

      else if (ISALNUM (*q))
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
  /* The last used parameter register.  */
  int last_parm_reg;
  int i, j;
  char buf[100];

  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
  switch (DEFAULT_ABI)
    {
    default:
      abort ();

    case ABI_V4:
    case ABI_SOLARIS:
    case ABI_AIX_NODESC:
      fprintf (file, "\tmflr %s\n", reg_names[0]);
      if (flag_pic == 1)
	{
	  fputs ("\tbl _GLOBAL_OFFSET_TABLE_@local-4\n", file);
	  asm_fprintf (file, "\t{st|stw} %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  asm_fprintf (file, "\tmflr %s\n", reg_names[12]);
	  asm_fprintf (file, "\t{l|lwz} %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@got(%s)\n", reg_names[12]);
	}
      else if (flag_pic > 1)
	{
	  asm_fprintf (file, "\t{st|stw} %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  /* Now, we need to get the address of the label.  */
	  fputs ("\tbl 1f\n\t.long ", file);
	  assemble_name (file, buf);
	  fputs ("-.\n1:", file);
	  asm_fprintf (file, "\tmflr %s\n", reg_names[11]);
	  asm_fprintf (file, "\t{l|lwz} %s,0(%s)\n", 
		       reg_names[0], reg_names[11]);
	  asm_fprintf (file, "\t{cax|add} %s,%s,%s\n",
		       reg_names[0], reg_names[0], reg_names[11]);
	}
      else
	{
	  asm_fprintf (file, "\t{liu|lis} %s,", reg_names[12]);
	  assemble_name (file, buf);
	  fputs ("@ha\n", file);
	  asm_fprintf (file, "\t{st|stw} %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  asm_fprintf (file, "\t{cal|la} %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@l(%s)\n", reg_names[12]);
	}

      if (current_function_needs_context)
	asm_fprintf (file, "\tmr %s,%s\n",
		     reg_names[30], reg_names[STATIC_CHAIN_REGNUM]);
      fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
      if (current_function_needs_context)
	asm_fprintf (file, "\tmr %s,%s\n",
		     reg_names[STATIC_CHAIN_REGNUM], reg_names[30]);
      break;

    case ABI_AIX:
      /* Set up a TOC entry for the profiler label.  */
      toc_section ();
      ASM_OUTPUT_INTERNAL_LABEL (file, "LPC", labelno);
      if (TARGET_MINIMAL_TOC)
	{
	  fputs (TARGET_32BIT ? "\t.long " : "\t.llong ", file);
	  assemble_name (file, buf);
	  putc ('\n', file);
	}
      else
	{
	  fputs ("\t.tc\t", file);
	  assemble_name (file, buf);
	  fputs ("[TC],", file);
	  assemble_name (file, buf);
	  putc ('\n', file);
	}
      text_section ();

  /* Figure out last used parameter register.  The proper thing to do is
     to walk incoming args of the function.  A function might have live
     parameter registers even if it has no incoming args.  */

      for (last_parm_reg = 10;
	   last_parm_reg > 2 && ! regs_ever_live [last_parm_reg];
	   last_parm_reg--)
	;

  /* Save parameter registers in regs 23-30 and static chain in r22.
     Don't overwrite reg 31, since it might be set up as the frame pointer.  */

      for (i = 3, j = 30; i <= last_parm_reg; i++, j--)
	asm_fprintf (file, "\tmr %d,%d\n", j, i);
      if (current_function_needs_context)
	asm_fprintf (file, "\tmr %d,%d\n", j, STATIC_CHAIN_REGNUM);

  /* Load location address into r3, and call mcount.  */

      ASM_GENERATE_INTERNAL_LABEL (buf, "LPC", labelno);
      asm_fprintf (file, TARGET_32BIT ? "\t{l|lwz} %s," : "\tld %s,",
		   reg_names[3]);
      assemble_name (file, buf);
      asm_fprintf (file, "(%s)\n\tbl %s\n\t", reg_names[2], RS6000_MCOUNT);
      asm_fprintf (file, RS6000_CALL_GLUE);
      putc('\n', file);

  /* Restore parameter registers and static chain.  */

      for (i = 3, j = 30; i <= last_parm_reg; i++, j--)
	asm_fprintf (file, "\tmr %d,%d\n", i, j);
      if (current_function_needs_context)
	asm_fprintf (file, "\tmr %d,%d\n", STATIC_CHAIN_REGNUM, j);

      break;
    }
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

int
rs6000_adjust_cost (insn, link, dep_insn, cost)
     rtx insn;
     rtx link;
     rtx dep_insn ATTRIBUTE_UNUSED;
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

/* A C statement (sans semicolon) to update the integer scheduling priority
   INSN_PRIORITY (INSN).  Reduce the priority to execute the INSN earlier,
   increase the priority to execute INSN later.  Do not define this macro if
   you do not need to adjust the scheduling priorities of insns.  */

int
rs6000_adjust_priority (insn, priority)
     rtx insn ATTRIBUTE_UNUSED;
     int priority;
{
  /* On machines (like the 750) which have asymetric integer units, where one
     integer unit can do multiply and divides and the other can't, reduce the
     priority of multiply/divide so it is scheduled before other integer
     operationss.  */

#if 0
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return priority;

  if (GET_CODE (PATTERN (insn)) == USE)
    return priority;

  switch (rs6000_cpu_attr) {
  case CPU_PPC750:
    switch (get_attr_type (insn))
      {
      default:
	break;

      case TYPE_IMUL:
      case TYPE_IDIV:
	fprintf (stderr, "priority was %#x (%d) before adjustment\n",
		 priority, priority);
	if (priority >= 0 && priority < 0x01000000)
	  priority >>= 3;
	break;
      }
  }
#endif

  return priority;
}

/* Return how many instructions the machine can issue per cycle */
int get_issue_rate()
{
  switch (rs6000_cpu_attr) {
  case CPU_RIOS1:  /* ? */
  case CPU_RS64A:
  case CPU_PPC601: /* ? */
    return 3;
  case CPU_PPC603:
  case CPU_PPC750:
    return 2; 
  case CPU_RIOS2:
  case CPU_PPC604:
  case CPU_PPC604E:
  case CPU_PPC620:
  case CPU_PPC630:
    return 4;
  default:
    return 1;
  }
}


/* Length in units of the trampoline for entering a nested function.  */

int
rs6000_trampoline_size ()
{
  int ret = 0;

  switch (DEFAULT_ABI)
    {
    default:
      abort ();

    case ABI_AIX:
      ret = (TARGET_32BIT) ? 12 : 24;
      break;

    case ABI_V4:
    case ABI_SOLARIS:
    case ABI_AIX_NODESC:
      ret = (TARGET_32BIT) ? 40 : 48;
      break;
    }

  return ret;
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

void
rs6000_initialize_trampoline (addr, fnaddr, cxt)
     rtx addr;
     rtx fnaddr;
     rtx cxt;
{
  enum machine_mode pmode = Pmode;
  int regsize = (TARGET_32BIT) ? 4 : 8;
  rtx ctx_reg = force_reg (pmode, cxt);

  switch (DEFAULT_ABI)
    {
    default:
      abort ();

/* Macros to shorten the code expansions below.  */
#define MEM_DEREF(addr) gen_rtx_MEM (pmode, memory_address (pmode, addr))
#define MEM_PLUS(addr,offset) \
  gen_rtx_MEM (pmode, memory_address (pmode, plus_constant (addr, offset)))

    /* Under AIX, just build the 3 word function descriptor */
    case ABI_AIX:
      {
	rtx fn_reg = gen_reg_rtx (pmode);
	rtx toc_reg = gen_reg_rtx (pmode);
	emit_move_insn (fn_reg, MEM_DEREF (fnaddr));
	emit_move_insn (toc_reg, MEM_PLUS (fnaddr, 4));
	emit_move_insn (MEM_DEREF (addr), fn_reg);
	emit_move_insn (MEM_PLUS (addr, regsize), toc_reg);
	emit_move_insn (MEM_PLUS (addr, 2*regsize), ctx_reg);
      }
      break;

    /* Under V.4/eabi, call __trampoline_setup to do the real work.  */
    case ABI_V4:
    case ABI_SOLARIS:
    case ABI_AIX_NODESC:
      emit_library_call (gen_rtx_SYMBOL_REF (SImode, "__trampoline_setup"),
			 FALSE, VOIDmode, 4,
			 addr, pmode,
			 GEN_INT (rs6000_trampoline_size ()), SImode,
			 fnaddr, pmode,
			 ctx_reg, pmode);
      break;
    }

  return;
}


/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */

int
rs6000_valid_decl_attribute_p (decl, attributes, identifier, args)
     tree decl ATTRIBUTE_UNUSED;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier ATTRIBUTE_UNUSED;
     tree args ATTRIBUTE_UNUSED;
{
  return 0;
}

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for TYPE.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */

int
rs6000_valid_type_attribute_p (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  if (TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != FIELD_DECL
      && TREE_CODE (type) != TYPE_DECL)
    return 0;

  /* Longcall attribute says that the function is not within 2**26 bytes
     of the current function, and to do an indirect call.  */
  if (is_attribute_p ("longcall", identifier))
    return (args == NULL_TREE);

  return 0;
}

/* If defined, a C expression whose value is zero if the attributes on
   TYPE1 and TYPE2 are incompatible, one if they are compatible, and
   two if they are nearly compatible (which causes a warning to be
   generated).  */

int
rs6000_comp_type_attributes (type1, type2)
     tree type1 ATTRIBUTE_UNUSED;
     tree type2 ATTRIBUTE_UNUSED;
{
  return 1;
}

/* If defined, a C statement that assigns default attributes to newly
   defined TYPE.  */

void
rs6000_set_default_type_attributes (type)
     tree type ATTRIBUTE_UNUSED;
{
  return;
}

/* Return a reference suitable for calling a function with the
   longcall attribute.  */
struct rtx_def *
rs6000_longcall_ref (call_ref)
     rtx call_ref;
{
  const char *call_name;
  tree node;

  if (GET_CODE (call_ref) != SYMBOL_REF)
    return call_ref;

  /* System V adds '.' to the internal name, so skip them.  */
  call_name = XSTR (call_ref, 0);
  if (*call_name == '.')
    {
      while (*call_name == '.')
	call_name++;

      node = get_identifier (call_name);
      call_ref = gen_rtx_SYMBOL_REF (VOIDmode, IDENTIFIER_POINTER (node));
    }

  return force_reg (Pmode, call_ref);
}


/* A C statement or statements to switch to the appropriate section
   for output of RTX in mode MODE.  You can assume that RTX is some
   kind of constant in RTL.  The argument MODE is redundant except in
   the case of a `const_int' rtx.  Select the section by calling
   `text_section' or one of the alternatives for other sections.

   Do not define this macro if you put all constants in the read-only
   data section.  */

#ifdef USING_SVR4_H

void
rs6000_select_rtx_section (mode, x)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx x;
{
  if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (x))
    toc_section ();
  else
    const_section ();
}

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.  */

void
rs6000_select_section (decl, reloc)
     tree decl;
     int reloc;
{
  int size = int_size_in_bytes (TREE_TYPE (decl));

  if (TREE_CODE (decl) == STRING_CST)
    {
      if (! flag_writable_strings)
	const_section ();
      else
	data_section ();
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      if ((flag_pic && reloc)
	  || ! TREE_READONLY (decl)
	  || TREE_SIDE_EFFECTS (decl)
	  || ! DECL_INITIAL (decl)
	  || (DECL_INITIAL (decl) != error_mark_node
	      && ! TREE_CONSTANT (DECL_INITIAL (decl))))
	{
	  if (rs6000_sdata != SDATA_NONE && (size > 0)
	      && (size <= g_switch_value))
	    sdata_section ();
	  else
	    data_section ();
	}
      else
	{
	  if (rs6000_sdata != SDATA_NONE && (size > 0)
	      && (size <= g_switch_value))
	    {
	      if (rs6000_sdata == SDATA_EABI)
		sdata2_section ();
	      else
		sdata_section ();  /* System V doesn't have .sdata2/.sbss2 */
	    }
	  else
	    const_section ();
	}
    }
  else
    const_section ();
}


/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  For real AIX calling sequences, we also replace the
   function name with the real name (1 or 2 leading .'s), rather than
   the function descriptor name.  This saves a lot of overriding code
   to read the prefixes.  */

void
rs6000_encode_section_info (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      rtx sym_ref = XEXP (DECL_RTL (decl), 0);
      if ((TREE_ASM_WRITTEN (decl) || ! TREE_PUBLIC (decl))
          && ! DECL_WEAK (decl))
	SYMBOL_REF_FLAG (sym_ref) = 1;

      if (DEFAULT_ABI == ABI_AIX)
	{
	  size_t len1 = (DEFAULT_ABI == ABI_AIX) ? 1 : 2;
	  size_t len2 = strlen (XSTR (sym_ref, 0));
	  char *str;

	  if (ggc_p)
	    str = ggc_alloc_string (NULL, len1 + len2);
	  else
	    str = permalloc (len1 + len2 + 1);

	  str[0] = '.';
	  str[1] = '.';
	  memcpy (str + len1, XSTR (sym_ref, 0), len2 + 1);

	  XSTR (sym_ref, 0) = str;
	}
    }
  else if (rs6000_sdata != SDATA_NONE
	   && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_SOLARIS)
	   && TREE_CODE (decl) == VAR_DECL)
    {
      int size = int_size_in_bytes (TREE_TYPE (decl));
      tree section_name = DECL_SECTION_NAME (decl);
      const char *name = (char *)0;
      int len = 0;

      if (section_name)
	{
	  if (TREE_CODE (section_name) == STRING_CST)
	    {
	      name = TREE_STRING_POINTER (section_name);
	      len = TREE_STRING_LENGTH (section_name);
	    }
	  else
	    abort ();
	}

      if ((size > 0 && size <= g_switch_value)
	  || (name
	      && ((len == sizeof (".sdata") - 1
		   && strcmp (name, ".sdata") == 0)
		  || (len == sizeof (".sdata2") - 1
		      && strcmp (name, ".sdata2") == 0)
		  || (len == sizeof (".sbss") - 1
		      && strcmp (name, ".sbss") == 0)
		  || (len == sizeof (".sbss2") - 1
		      && strcmp (name, ".sbss2") == 0)
		  || (len == sizeof (".PPC.EMB.sdata0") - 1
		      && strcmp (name, ".PPC.EMB.sdata0") == 0)
		  || (len == sizeof (".PPC.EMB.sbss0") - 1
		      && strcmp (name, ".PPC.EMB.sbss0") == 0))))
	{
	  rtx sym_ref = XEXP (DECL_RTL (decl), 0);
	  size_t len = strlen (XSTR (sym_ref, 0));
	  char *str;

	  if (ggc_p)
	    str = ggc_alloc_string (NULL, len + 1);
	  else
	    str = permalloc (len + 2);
	  str[0] = '@';
	  memcpy (str + 1, XSTR (sym_ref, 0), len + 1);

	  XSTR (sym_ref, 0) = str;
	}
    }
}

#endif /* USING_SVR4_H */


/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.

   r0 is special and we must not select it as an address
   register by this routine since our caller will try to
   increment the returned register via an "la" instruction.  */

struct rtx_def *
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && REGNO (XEXP (addr, 0)) != 0)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG
	       && REGNO (XEXP (addr, 1)) != 0)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG && REGNO (addr) != 0)
    return addr;
  abort ();
}

void
rs6000_fatal_bad_address (op)
  rtx op;
{
  fatal_insn ("bad address", op);
}

/* Called to register all of our global variables with the garbage
   collector.  */

static void
rs6000_add_gc_roots ()
{
  ggc_add_rtx_root (&rs6000_compare_op0, 1);
  ggc_add_rtx_root (&rs6000_compare_op1, 1);

  toc_hash_table = htab_create (1021, toc_hash_function, toc_hash_eq, NULL);
  ggc_add_root (&toc_hash_table, 1, sizeof (toc_hash_table), 
		toc_hash_mark_table);
}
