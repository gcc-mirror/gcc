/* Subroutines for insn-output.c for MIPS
   Copyright (C) 1989, 90, 91, 93-97, 1998 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky, lich@inria.inria.fr.
   Changes by Michael Meissner, meissner@osf.org.
   64 bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
   Brendan Eich, brendan@microunity.com.

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

/* ??? The TARGET_FP_CALL_32 macros are intended to simulate a 32 bit
   calling convention in 64 bit mode.  It doesn't work though, and should
   be replaced with something better designed.  */

#include "config.h"

#include <stdio.h>

#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "recog.h"
#include "output.h"

#undef MAX			/* sys/param.h may also define these */
#undef MIN

#include <signal.h>
#include <sys/types.h>
#include <sys/file.h>
#include <ctype.h>
#include "tree.h"
#include "expr.h"
#include "flags.h"

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#endif

#if defined(USG) || defined(NO_STAB_H)
#include "gstab.h"  /* If doing DBX on sysV, use our own stab.h.  */
#else
#include <stab.h>  /* On BSD, use the system's stab.h.  */
#endif /* not USG */

#ifdef __GNU_STAB__
#define STAB_CODE_TYPE enum __stab_debug_code
#else
#define STAB_CODE_TYPE int
#endif

extern void   abort ();
extern int    atoi ();
extern char  *getenv ();
extern char  *mktemp ();
 
extern rtx    adj_offsettable_operand ();
extern rtx    copy_to_reg ();
extern void   error ();
extern void   fatal ();
extern tree   lookup_name ();
extern void   pfatal_with_name ();
extern void   warning ();

extern FILE  *asm_out_file;

/* Enumeration for all of the relational tests, so that we can build
   arrays indexed by the test type, and not worry about the order
   of EQ, NE, etc. */

enum internal_test {
    ITEST_EQ,
    ITEST_NE,
    ITEST_GT,
    ITEST_GE,
    ITEST_LT,
    ITEST_LE,
    ITEST_GTU,
    ITEST_GEU,
    ITEST_LTU,
    ITEST_LEU,
    ITEST_MAX
  };

/* Global variables for machine-dependent things.  */

/* Threshold for data being put into the small data/bss area, instead
   of the normal data area (references to the small data/bss area take
   1 instruction, and use the global pointer, references to the normal
   data area takes 2 instructions).  */
int mips_section_threshold = -1;

/* Count the number of .file directives, so that .loc is up to date.  */
int num_source_filenames = 0;

/* Count the number of sdb related labels are generated (to find block
   start and end boundaries).  */
int sdb_label_count = 0;

/* Next label # for each statement for Silicon Graphics IRIS systems. */
int sym_lineno = 0;

/* Non-zero if inside of a function, because the stupid MIPS asm can't
   handle .files inside of functions.  */
int inside_function = 0;

/* Files to separate the text and the data output, so that all of the data
   can be emitted before the text, which will mean that the assembler will
   generate smaller code, based on the global pointer.  */
FILE *asm_out_data_file;
FILE *asm_out_text_file;

/* Linked list of all externals that are to be emitted when optimizing
   for the global pointer if they haven't been declared by the end of
   the program with an appropriate .comm or initialization.  */

struct extern_list {
  struct extern_list *next;	/* next external */
  char *name;			/* name of the external */
  int size;			/* size in bytes */
} *extern_head = 0;

/* Name of the file containing the current function.  */
char *current_function_file = "";

/* Warning given that Mips ECOFF can't support changing files
   within a function.  */
int file_in_function_warning = FALSE;

/* Whether to suppress issuing .loc's because the user attempted
   to change the filename within a function.  */
int ignore_line_number = FALSE;

/* Number of nested .set noreorder, noat, nomacro, and volatile requests.  */
int set_noreorder;
int set_noat;
int set_nomacro;
int set_volatile;

/* The next branch instruction is a branch likely, not branch normal.  */
int mips_branch_likely;

/* Count of delay slots and how many are filled.  */
int dslots_load_total;
int dslots_load_filled;
int dslots_jump_total;
int dslots_jump_filled;

/* # of nops needed by previous insn */
int dslots_number_nops;

/* Number of 1/2/3 word references to data items (ie, not jal's).  */
int num_refs[3];

/* registers to check for load delay */
rtx mips_load_reg, mips_load_reg2, mips_load_reg3, mips_load_reg4;

/* Cached operands, and operator to compare for use in set/branch on
   condition codes.  */
rtx branch_cmp[2];

/* what type of branch to use */
enum cmp_type branch_type;

/* Number of previously seen half-pic pointers and references.  */
static int prev_half_pic_ptrs = 0;
static int prev_half_pic_refs = 0;

/* which cpu are we scheduling for */
enum processor_type mips_cpu;

/* which instruction set architecture to use.  */
int mips_isa;

#ifdef MIPS_ABI_DEFAULT
/* which ABI to use.  This is defined to a constant in mips.h if the target
   doesn't support multiple ABIs.  */
enum mips_abi_type mips_abi;
#endif

/* Strings to hold which cpu and instruction set architecture to use.  */
char *mips_cpu_string;		/* for -mcpu=<xxx> */
char *mips_isa_string;		/* for -mips{1,2,3,4} */
char *mips_abi_string;		/* for -mabi={o32,32,n32,n64,64,eabi} */

/* If TRUE, we split addresses into their high and low parts in the RTL.  */
int mips_split_addresses;

/* Generating calls to position independent functions?  */
enum mips_abicalls_type mips_abicalls;

/* High and low marks for floating point values which we will accept
   as legitimate constants for LEGITIMATE_CONSTANT_P.  These are
   initialized in override_options.  */
REAL_VALUE_TYPE dfhigh, dflow, sfhigh, sflow;

/* Array giving truth value on whether or not a given hard register
   can support a given mode.  */
char mips_hard_regno_mode_ok[(int)MAX_MACHINE_MODE][FIRST_PSEUDO_REGISTER];

/* Current frame information calculated by compute_frame_size.  */
struct mips_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
struct mips_frame_info zero_frame_info;

/* Temporary filename used to buffer .text until end of program
   for -mgpopt.  */
static char *temp_filename;

/* Pseudo-reg holding the address of the current function when
   generating embedded PIC code.  Created by LEGITIMIZE_ADDRESS, used
   by mips_finalize_pic if it was created.  */
rtx embedded_pic_fnaddr_rtx;

/* List of all MIPS punctuation characters used by print_operand.  */
char mips_print_operand_punct[256];

/* Map GCC register number to debugger register number.  */
int mips_dbx_regno[FIRST_PSEUDO_REGISTER];

/* Buffer to use to enclose a load/store operation with %{ %} to
   turn on .set volatile.  */
static char volatile_buffer[60];

/* Hardware names for the registers.  If -mrnames is used, this
   will be overwritten with mips_sw_reg_names.  */

char mips_reg_names[][8] =
{
 "$0",   "$1",   "$2",   "$3",   "$4",   "$5",   "$6",   "$7",
 "$8",   "$9",   "$10",  "$11",  "$12",  "$13",  "$14",  "$15",
 "$16",  "$17",  "$18",  "$19",  "$20",  "$21",  "$22",  "$23",
 "$24",  "$25",  "$26",  "$27",  "$28",  "$sp",  "$fp",  "$31",
 "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",
 "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
 "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
 "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",
 "hi",   "lo",   "accum","$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",
 "$fcc5","$fcc6","$fcc7","$rap"
};

/* Mips software names for the registers, used to overwrite the
   mips_reg_names array.  */

char mips_sw_reg_names[][8] =
{
  "$zero","$at",  "$v0",  "$v1",  "$a0",  "$a1",  "$a2",  "$a3",
  "$t0",  "$t1",  "$t2",  "$t3",  "$t4",  "$t5",  "$t6",  "$t7",
  "$s0",  "$s1",  "$s2",  "$s3",  "$s4",  "$s5",  "$s6",  "$s7",
  "$t8",  "$t9",  "$k0",  "$k1",  "$gp",  "$sp",  "$fp",  "$ra",
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",
  "hi",   "lo",   "accum","$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",
  "$fcc5","$fcc6","$fcc7","$rap"
};

/* Map hard register number to register class */
enum reg_class mips_regno_to_class[] =
{
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  HI_REG,	LO_REG,		HILO_REG,	ST_REGS,
  ST_REGS,	ST_REGS,	ST_REGS,	ST_REGS,
  ST_REGS,	ST_REGS,	ST_REGS,	GR_REGS
};

/* Map register constraint character to register class.  */
enum reg_class mips_char_to_class[256] =
{
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
};


/* Return truth value of whether OP can be used as an operands
   where a register or 16 bit unsigned integer is needed.  */

int
uns_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT_UNSIGNED (op))
    return TRUE;

  return register_operand (op, mode);
}

/* Return truth value of whether OP can be used as an operands
   where a 16 bit integer is needed  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT (op))
    return TRUE;

  return register_operand (op, mode);
}

/* Return truth value of whether OP can be used as an operand in a two
   address arithmetic insn (such as set 123456,%o4) of mode MODE.  */

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return TRUE;

  return register_operand (op, mode);
}

/* Return truth value of whether OP is a integer which fits in 16 bits  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Return truth value of whether OP is a 32 bit integer which is too big to
   be loaded with one instruction.  */

int
large_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  HOST_WIDE_INT value;

  if (GET_CODE (op) != CONST_INT)
    return FALSE;

  value = INTVAL (op);
  if ((value & ~0x0000ffff) == 0)			/* ior reg,$r0,value */
    return FALSE;

  if (((unsigned long)(value + 32768)) <= 32767)	/* subu reg,$r0,value */
    return FALSE;

  if ((value & 0x0000ffff) == 0)			/* lui reg,value>>16 */
    return FALSE;

  return TRUE;
}

/* Return truth value of whether OP is a register or the constant 0.  */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    default:
      break;

    case CONST_INT:
      return (INTVAL (op) == 0);

    case CONST_DOUBLE:
      if (op != CONST0_RTX (mode))
	return FALSE;

      return TRUE;

    case REG:
    case SUBREG:
      return register_operand (op, mode);
    }

  return FALSE;
}

/* Return truth value if a CONST_DOUBLE is ok to be a legitimate constant.  */

int
mips_const_double_ok (op, mode)
     rtx op;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE d;

  if (GET_CODE (op) != CONST_DOUBLE)
    return FALSE;

  if (mode == VOIDmode)
    return TRUE;

  if (mode != SFmode && mode != DFmode)
    return FALSE;

  if (op == CONST0_RTX (mode))
    return TRUE;

  /* ??? li.s does not work right with SGI's Irix 6 assembler.  */
  if (mips_abi != ABI_32 && mips_abi != ABI_EABI)
    return FALSE;

  REAL_VALUE_FROM_CONST_DOUBLE (d, op);

  if (REAL_VALUE_ISNAN (d))
    return FALSE;

  if (REAL_VALUE_NEGATIVE (d))
    d = REAL_VALUE_NEGATE (d);

  if (mode == DFmode)
    {
      if (REAL_VALUES_LESS (d, dfhigh)
	  && REAL_VALUES_LESS (dflow, d))
	return TRUE;
    }
  else
    {
      if (REAL_VALUES_LESS (d, sfhigh)
	  && REAL_VALUES_LESS (sflow, d))
	return TRUE;
    }

  return FALSE;
}

/* Accept the floating point constant 1 in the appropriate mode.  */

int
const_float_1_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE d;
  static REAL_VALUE_TYPE onedf;
  static REAL_VALUE_TYPE onesf;
  static int one_initialized;

  if (GET_CODE (op) != CONST_DOUBLE
      || mode != GET_MODE (op)
      || (mode != DFmode && mode != SFmode))
    return FALSE;

  REAL_VALUE_FROM_CONST_DOUBLE (d, op);

  /* We only initialize these values if we need them, since we will
     never get called unless mips_isa >= 4.  */
  if (! one_initialized)
    {
      onedf = REAL_VALUE_ATOF ("1.0", DFmode);
      onesf = REAL_VALUE_ATOF ("1.0", SFmode);
      one_initialized = TRUE;
    }

  if (mode == DFmode)
    return REAL_VALUES_EQUAL (d, onedf);
  else
    return REAL_VALUES_EQUAL (d, onesf);
}

/* Return truth value if a memory operand fits in a single instruction
   (ie, register + small offset).  */

int
simple_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx addr, plus0, plus1;

  /* Eliminate non-memory operations */
  if (GET_CODE (op) != MEM)
    return FALSE;

  /* dword operations really put out 2 instructions, so eliminate them.  */
  /* ??? This isn't strictly correct.  It is OK to accept multiword modes
     here, since the length attributes are being set correctly, but only
     if the address is offsettable.  LO_SUM is not offsettable.  */
  if (GET_MODE_SIZE (GET_MODE (op)) > UNITS_PER_WORD)
    return FALSE;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    default:
      break;

    case REG:
    case LO_SUM:
      return TRUE;

    case CONST_INT:
      return SMALL_INT (op);

    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);
      if (GET_CODE (plus0) == REG
	  && GET_CODE (plus1) == CONST_INT
	  && SMALL_INT (plus1))
	return TRUE;

      else if (GET_CODE (plus1) == REG
	       && GET_CODE (plus0) == CONST_INT
	       && SMALL_INT (plus0))
	return TRUE;

      else
	return FALSE;

#if 0
      /* We used to allow small symbol refs here (ie, stuff in .sdata
	 or .sbss), but this causes some bugs in G++.  Also, it won't
	 interfere if the MIPS linker rewrites the store instruction
	 because the function is PIC.  */

    case LABEL_REF:		/* never gp relative */
      break;

    case CONST:
      /* If -G 0, we can never have a GP relative memory operation.
	 Also, save some time if not optimizing.  */
      if (!TARGET_GP_OPT)
	return FALSE;

      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (XEXP (addr, 0), &offset);
	if (GET_CODE (op) != SYMBOL_REF)
	  return FALSE;

	/* let's be paranoid.... */
	if (! SMALL_INT (offset))
	  return FALSE;
      }
      /* fall through */

    case SYMBOL_REF:
      return SYMBOL_REF_FLAG (addr);
#endif
    }

  return FALSE;
}

/* Return true if the code of this rtx pattern is EQ or NE.  */

int
equality_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return FALSE;

  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return true if the code is a relational operations (EQ, LE, etc.) */

int
cmp_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return FALSE;

  return (GET_RTX_CLASS (GET_CODE (op)) == '<');
}

/* Return true if the operand is either the PC or a label_ref.  */

int
pc_or_label_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (op == pc_rtx)
    return TRUE;

  if (GET_CODE (op) == LABEL_REF)
    return TRUE;

  return FALSE;
}

/* Test for a valid operand for a call instruction.
   Don't allow the arg pointer register or virtual regs
   since they may change into reg + const, which the patterns
   can't handle yet.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (CONSTANT_ADDRESS_P (op)
      || (GET_CODE (op) == REG && op != arg_pointer_rtx
	  && ! (REGNO (op) >= FIRST_PSEUDO_REGISTER
		&& REGNO (op) <= LAST_VIRTUAL_REGISTER)))
    return 1;
  return 0;
}

/* Return true if OPERAND is valid as a source operand for a move
   instruction.  */

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Accept any general operand after reload has started; doing so
     avoids losing if reload does an in-place replacement of a register
     with a SYMBOL_REF or CONST.  */
  return (general_operand (op, mode)
	  && (! (mips_split_addresses && mips_check_split (op, mode))
	      || reload_in_progress
	      || reload_completed));
		
	
}

/* Return true if OPERAND is valid as a source operand for movdi.
   This accepts not only general_operand, but also sign extended
   constants and registers.  We need to accept sign extended constants
   in case a sign extended register which is used in an expression,
   and is equivalent to a constant, is spilled.  */

int
movdi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && (GET_MODE (XEXP (op, 0)) == SImode
	  || (GET_CODE (XEXP (op, 0)) == CONST_INT
	      && GET_MODE (XEXP (op, 0)) == VOIDmode))
      && (register_operand (XEXP (op, 0), SImode)
	  || immediate_operand (XEXP (op, 0), SImode)))
    return 1;

  return general_operand (op, mode);
}

/* Like register_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return register_operand (op, mode);
}

/* Like reg_or_0_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return reg_or_0_operand (op, mode);
}

/* Like uns_arith_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_uns_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return uns_arith_operand (op, mode);
}

/* Like arith_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return arith_operand (op, mode);
}

/* Like nonmemory_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_nonmemory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return nonmemory_operand (op, mode);
}

/* Like nonimmediate_operand, but when in 64 bit mode also accept a
   sign extend of a 32 bit register, since the value is known to be
   already sign extended.  */

int
se_nonimmediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return nonimmediate_operand (op, mode);
}

/* Return true if we split the address into high and low parts.  */

/* ??? We should also handle reg+array somewhere.  We get four
   instructions currently, lui %hi/addui %lo/addui reg/lw.  Better is
   lui %hi/addui reg/lw %lo.  Fixing GO_IF_LEGITIMATE_ADDRESS to accept
   (plus (reg) (symbol_ref)) doesn't work because the SYMBOL_REF is broken
   out of the address, then we have 4 instructions to combine.  Perhaps
   add a 3->2 define_split for combine.  */

/* ??? We could also split a CONST_INT here if it is a large_int().
   However, it doesn't seem to be very useful to have %hi(constant).
   We would be better off by doing the masking ourselves and then putting
   the explicit high part of the constant in the RTL.  This will give better
   optimization.  Also, %hi(constant) needs assembler changes to work.
   There is already a define_split that does this.  */

int
mips_check_split (address, mode)
     rtx address;
     enum machine_mode mode;
{     
  /* ??? This is the same check used in simple_memory_operand.
     We use it here because LO_SUM is not offsettable.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return 0;

  if ((GET_CODE (address) == SYMBOL_REF && ! SYMBOL_REF_FLAG (address))
      || (GET_CODE (address) == CONST
	  && GET_CODE (XEXP (XEXP (address, 0), 0)) == SYMBOL_REF
	  && ! SYMBOL_REF_FLAG (XEXP (XEXP (address, 0), 0)))
      || GET_CODE (address) == LABEL_REF)
    return 1;

  return 0;
}

/* Returns an operand string for the given instruction's delay slot,
   after updating filled delay slot statistics.

   We assume that operands[0] is the target register that is set.

   In order to check the next insn, most of this functionality is moved
   to FINAL_PRESCAN_INSN, and we just set the global variables that
   it needs.  */

/* ??? This function no longer does anything useful, because final_prescan_insn
   now will never emit a nop.  */

char *
mips_fill_delay_slot (ret, type, operands, cur_insn)
     char *ret;			/* normal string to return */
     enum delay_type type;	/* type of delay */
     rtx operands[];		/* operands to use */
     rtx cur_insn;		/* current insn */
{
  register rtx set_reg;
  register enum machine_mode mode;
  register rtx next_insn	= (cur_insn) ? NEXT_INSN (cur_insn) : (rtx)0;
  register int num_nops;

  if (type == DELAY_LOAD || type == DELAY_FCMP)
    num_nops = 1;

  else if (type == DELAY_HILO)
    num_nops = 2;

  else
    num_nops = 0;

  /* Make sure that we don't put nop's after labels.  */
  next_insn = NEXT_INSN (cur_insn);
  while (next_insn != (rtx)0 && GET_CODE (next_insn) == NOTE)
    next_insn = NEXT_INSN (next_insn);

  dslots_load_total += num_nops;
  if (TARGET_DEBUG_F_MODE
      || !optimize
      || type == DELAY_NONE
      || operands == (rtx *)0
      || cur_insn == (rtx)0
      || next_insn == (rtx)0
      || GET_CODE (next_insn) == CODE_LABEL
      || (set_reg = operands[0]) == (rtx)0)
    {
      dslots_number_nops = 0;
      mips_load_reg  = (rtx)0;
      mips_load_reg2 = (rtx)0;
      mips_load_reg3 = (rtx)0;
      mips_load_reg4 = (rtx)0;
      return ret;
    }

  set_reg = operands[0];
  if (set_reg == (rtx)0)
    return ret;

  while (GET_CODE (set_reg) == SUBREG)
    set_reg = SUBREG_REG (set_reg);

  mode = GET_MODE (set_reg);
  dslots_number_nops = num_nops;
  mips_load_reg = set_reg;
  if (GET_MODE_SIZE (mode)
      > (FP_REG_P (REGNO (set_reg)) ? UNITS_PER_FPREG : UNITS_PER_WORD))
    mips_load_reg2 = gen_rtx (REG, SImode, REGNO (set_reg) + 1);
  else
    mips_load_reg2 = 0;

  if (type == DELAY_HILO)
    {
      mips_load_reg3 = gen_rtx (REG, SImode, MD_REG_FIRST);
      mips_load_reg4 = gen_rtx (REG, SImode, MD_REG_FIRST+1);
    }
  else
    {
      mips_load_reg3 = 0;
      mips_load_reg4 = 0;
    }

  return ret;
}


/* Determine whether a memory reference takes one (based off of the GP pointer),
   two (normal), or three (label + reg) instructions, and bump the appropriate
   counter for -mstats.  */

void
mips_count_memory_refs (op, num)
     rtx op;
     int num;
{
  int additional = 0;
  int n_words = 0;
  rtx addr, plus0, plus1;
  enum rtx_code code0, code1;
  int looping;

  if (TARGET_DEBUG_B_MODE)
    {
      fprintf (stderr, "\n========== mips_count_memory_refs:\n");
      debug_rtx (op);
    }

  /* Skip MEM if passed, otherwise handle movsi of address.  */
  addr = (GET_CODE (op) != MEM) ? op : XEXP (op, 0);

  /* Loop, going through the address RTL */
  do
    {
      looping = FALSE;
      switch (GET_CODE (addr))
	{
	default:
	  break;

	case REG:
	case CONST_INT:
	case LO_SUM:
	  break;

	case PLUS:
	  plus0 = XEXP (addr, 0);
	  plus1 = XEXP (addr, 1);
	  code0 = GET_CODE (plus0);
	  code1 = GET_CODE (plus1);

	  if (code0 == REG)
	    {
	      additional++;
	      addr = plus1;
	      looping = TRUE;
	      continue;
	    }

	  if (code0 == CONST_INT)
	    {
	      addr = plus1;
	      looping = TRUE;
	      continue;
	    }

	  if (code1 == REG)
	    {
	      additional++;
	      addr = plus0;
	      looping = TRUE;
	      continue;
	    }

	  if (code1 == CONST_INT)
	    {
	      addr = plus0;
	      looping = TRUE;
	      continue;
	    }

	  if (code0 == SYMBOL_REF || code0 == LABEL_REF || code0 == CONST)
	    {
	      addr = plus0;
	      looping = TRUE;
	      continue;
	    }

	  if (code1 == SYMBOL_REF || code1 == LABEL_REF || code1 == CONST)
	    {
	      addr = plus1;
	      looping = TRUE;
	      continue;
	    }

	  break;

	case LABEL_REF:
	  n_words = 2;		/* always 2 words */
	  break;

	case CONST:
	  addr = XEXP (addr, 0);
	  looping = TRUE;
	  continue;

	case SYMBOL_REF:
	  n_words = SYMBOL_REF_FLAG (addr) ? 1 : 2;
	  break;
	}
    }
  while (looping);

  if (n_words == 0)
    return;

  n_words += additional;
  if (n_words > 3)
    n_words = 3;

  num_refs[n_words-1] += num;
}


/* Return RTL for the offset from the current function to the
   argument.  */

rtx
embedded_pic_offset (x)
     rtx x;
{
  if (embedded_pic_fnaddr_rtx == NULL)
    {
      rtx seq;

      embedded_pic_fnaddr_rtx = gen_reg_rtx (Pmode);
      
      /* Output code at function start to initialize the pseudo-reg.  */
      /* ??? We used to do this in FINALIZE_PIC, but that does not work for
	 inline functions, because it is called after RTL for the function
	 has been copied.  The pseudo-reg in embedded_pic_fnaddr_rtx however
	 does not get copied, and ends up not matching the rest of the RTL.
	 This solution works, but means that we get unnecessary code to
	 initialize this value every time a function is inlined into another
	 function.  */
      start_sequence ();
      emit_insn (gen_get_fnaddr (embedded_pic_fnaddr_rtx,
				 XEXP (DECL_RTL (current_function_decl), 0)));
      seq = gen_sequence ();
      end_sequence ();
      push_topmost_sequence ();
      emit_insn_after (seq, get_insns ());
      pop_topmost_sequence ();
    }

  return gen_rtx (CONST, Pmode,
		  gen_rtx (MINUS, Pmode, x,
			   XEXP (DECL_RTL (current_function_decl), 0)));
}

/* Return the appropriate instructions to move one operand to another.  */

char *
mips_move_1word (operands, insn, unsignedp)
     rtx operands[];
     rtx insn;
     int unsignedp;
{
  char *ret = 0;
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  enum rtx_code code0 = GET_CODE (op0);
  enum rtx_code code1 = GET_CODE (op1);
  enum machine_mode mode = GET_MODE (op0);
  int subreg_word0 = 0;
  int subreg_word1 = 0;
  enum delay_type delay = DELAY_NONE;

  while (code0 == SUBREG)
    {
      subreg_word0 += SUBREG_WORD (op0);
      op0 = SUBREG_REG (op0);
      code0 = GET_CODE (op0);
    }

  while (code1 == SUBREG)
    {
      subreg_word1 += SUBREG_WORD (op1);
      op1 = SUBREG_REG (op1);
      code1 = GET_CODE (op1);
    }

  /* For our purposes, a condition code mode is the same as SImode.  */
  if (mode == CCmode)
    mode = SImode;

  if (code0 == REG)
    {
      int regno0 = REGNO (op0) + subreg_word0;

      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_word1;

	  /* Just in case, don't do anything for assigning a register
	     to itself, unless we are filling a delay slot.  */
	  if (regno0 == regno1 && set_nomacro == 0)
	    ret = "";

	  else if (GP_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		ret = "move\t%0,%1";

	      else if (MD_REG_P (regno1))
		{
		  delay = DELAY_HILO;
		  if (regno1 != HILO_REGNUM)
		    ret = "mf%1\t%0";
		  else
		    ret = "mflo\t%0";
		}

	      else if (ST_REG_P (regno1) && mips_isa >= 4)
		ret = "li\t%0,1\n\tmovf\t%0,%.,%1";

	      else
		{
		  delay = DELAY_LOAD;
		  if (FP_REG_P (regno1))
		    ret = "mfc1\t%0,%1";

		  else if (regno1 == FPSW_REGNUM && mips_isa < 4)
		    ret = "cfc1\t%0,$31";
		}
	    }

	  else if (FP_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%1,%0";
		}

	      if (FP_REG_P (regno1))
		ret = "mov.s\t%0,%1";
	    }

	  else if (MD_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		{
		  delay = DELAY_HILO;
		  if (regno0 != HILO_REGNUM)
		    ret = "mt%0\t%1";
		}
	    }

	  else if (regno0 == FPSW_REGNUM && mips_isa < 4)
	    {
	      if (GP_REG_P (regno1))
		{
		  delay = DELAY_LOAD;
		  ret = "ctc1\t%0,$31";
		}
	    }
	}

      else if (code1 == MEM)
	{
	  delay = DELAY_LOAD;

	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 1);

	  if (GP_REG_P (regno0))
	    {
	      /* For loads, use the mode of the memory item, instead of the
		 target, so zero/sign extend can use this code as well.  */
	      switch (GET_MODE (op1))
		{
		default:
		  break;
		case SFmode:
		  ret = "lw\t%0,%1";
		  break;
		case SImode:
		case CCmode:
		  ret = ((unsignedp && TARGET_64BIT)
			 ? "lwu\t%0,%1"
			 : "lw\t%0,%1");
		  break;
		case HImode:
		  ret = (unsignedp) ? "lhu\t%0,%1" : "lh\t%0,%1";
		  break;
		case QImode:
		  ret = (unsignedp) ? "lbu\t%0,%1" : "lb\t%0,%1";
		  break;
		}
	    }

	  else if (FP_REG_P (regno0) && (mode == SImode || mode == SFmode))
	    ret = "l.s\t%0,%1";

	  if (ret != (char *)0 && MEM_VOLATILE_P (op1))
	    {
	      int i = strlen (ret);
	      if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
		abort ();

	      sprintf (volatile_buffer, "%%{%s%%}", ret);
	      ret = volatile_buffer;
	    }
	}

      else if (code1 == CONST_INT
	       || (code1 == CONST_DOUBLE
		   && GET_MODE (op1) == VOIDmode))
	{
	  if (code1 == CONST_DOUBLE)
	    {
	      /* This can happen when storing constants into long long
                 bitfields.  Just store the least significant word of
                 the value.  */
	      operands[1] = op1 = GEN_INT (CONST_DOUBLE_LOW (op1));
	    }

	  if (INTVAL (op1) == 0)
	    {
	      if (GP_REG_P (regno0))
		ret = "move\t%0,%z1";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%z1,%0";
		}

	      else if (MD_REG_P (regno0))
		{
		  delay = DELAY_HILO;
		  ret = "mt%0\t%.";
		}
	    }

	  else if (GP_REG_P (regno0))
	    /* Don't use X format, because that will give out of range
	       numbers for 64 bit host and 32 bit target.  */
	    ret = "li\t%0,%1\t\t\t# %X1";
	}

      else if (code1 == CONST_DOUBLE && mode == SFmode)
	{
	  if (op1 == CONST0_RTX (SFmode))
	    {
	      if (GP_REG_P (regno0))
		ret = "move\t%0,%.";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%.,%0";
		}
	    }

	  else
	    {
	      delay = DELAY_LOAD;
	      ret = "li.s\t%0,%1";
	    }
	}

      else if (code1 == LABEL_REF)
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 1);

	  ret = "la\t%0,%a1";
	}

      else if (code1 == SYMBOL_REF || code1 == CONST)
	{
	  if (HALF_PIC_P () && CONSTANT_P (op1) && HALF_PIC_ADDRESS_P (op1))
	    {
	      rtx offset = const0_rtx;

	      if (GET_CODE (op1) == CONST)
		op1 = eliminate_constant_term (XEXP (op1, 0), &offset);

	      if (GET_CODE (op1) == SYMBOL_REF)
		{
		  operands[2] = HALF_PIC_PTR (op1);

		  if (TARGET_STATS)
		    mips_count_memory_refs (operands[2], 1);

		  if (INTVAL (offset) == 0)
		    {
		      delay = DELAY_LOAD;
		      ret = (unsignedp && TARGET_64BIT
			     ? "lwu\t%0,%2"
			     : "lw\t%0,%2");
		    }
		  else
		    {
		      dslots_load_total++;
		      operands[3] = offset;
		      if (unsignedp && TARGET_64BIT)
			ret = (SMALL_INT (offset))
				  ? "lwu\t%0,%2%#\n\tadd\t%0,%0,%3"
				  : "lwu\t%0,%2%#\n\t%[li\t%@,%3\n\tadd\t%0,%0,%@%]";
		      else
			ret = (SMALL_INT (offset))
				  ? "lw\t%0,%2%#\n\tadd\t%0,%0,%3"
				  : "lw\t%0,%2%#\n\t%[li\t%@,%3\n\tadd\t%0,%0,%@%]";
		    }
		}
	    }
	  else
	    {
	      if (TARGET_STATS)
		mips_count_memory_refs (op1, 1);

	      ret = "la\t%0,%a1";
	    }
	}

      else if (code1 == PLUS)
	{
	  rtx add_op0 = XEXP (op1, 0);
	  rtx add_op1 = XEXP (op1, 1);

	  if (GET_CODE (XEXP (op1, 1)) == REG && GET_CODE (XEXP (op1, 0)) == CONST_INT)
	    {
	      add_op0 = XEXP (op1, 1);		/* reverse operands */
	      add_op1 = XEXP (op1, 0);
	    }

	  operands[2] = add_op0;
	  operands[3] = add_op1;
	  ret = "add%:\t%0,%2,%3";
	}

      else if (code1 == HIGH)
	{
	  operands[1] = XEXP (op1, 0);
	  ret = "lui\t%0,%%hi(%1)";
	}
    }

  else if (code0 == MEM)
    {
      if (TARGET_STATS)
	mips_count_memory_refs (op0, 1);

      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_word1;

	  if (GP_REG_P (regno1))
	    {
	      switch (mode)
		{
		default: break;
		case SFmode: ret = "sw\t%1,%0"; break;
		case SImode: ret = "sw\t%1,%0"; break;
		case HImode: ret = "sh\t%1,%0"; break;
		case QImode: ret = "sb\t%1,%0"; break;
		}
	    }

	  else if (FP_REG_P (regno1) && (mode == SImode || mode == SFmode))
	    ret = "s.s\t%1,%0";
	}

      else if (code1 == CONST_INT && INTVAL (op1) == 0)
	{
	  switch (mode)
	    {
	    default: break;
	    case SFmode: ret = "sw\t%z1,%0"; break;
	    case SImode: ret = "sw\t%z1,%0"; break;
	    case HImode: ret = "sh\t%z1,%0"; break;
	    case QImode: ret = "sb\t%z1,%0"; break;
	    }
	}

      else if (code1 == CONST_DOUBLE && op1 == CONST0_RTX (mode))
	{
	  switch (mode)
	    {
	    default: break;
	    case SFmode: ret = "sw\t%.,%0"; break;
	    case SImode: ret = "sw\t%.,%0"; break;
	    case HImode: ret = "sh\t%.,%0"; break;
	    case QImode: ret = "sb\t%.,%0"; break;
	    }
	}

      if (ret != (char *)0 && MEM_VOLATILE_P (op0))
	{
	  int i = strlen (ret);
	  if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
	    abort ();
	  
	  sprintf (volatile_buffer, "%%{%s%%}", ret);
	  ret = volatile_buffer;
	}
    }

  if (ret == (char *)0)
    {
      abort_with_insn (insn, "Bad move");
      return 0;
    }

  if (delay != DELAY_NONE)
    return mips_fill_delay_slot (ret, delay, operands, insn);

  return ret;
}


/* Return the appropriate instructions to move 2 words */

char *
mips_move_2words (operands, insn)
     rtx operands[];
     rtx insn;
{
  char *ret = 0;
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  int subreg_word0 = 0;
  int subreg_word1 = 0;
  enum delay_type delay = DELAY_NONE;

  while (code0 == SUBREG)
    {
      subreg_word0 += SUBREG_WORD (op0);
      op0 = SUBREG_REG (op0);
      code0 = GET_CODE (op0);
    }

  if (code1 == SIGN_EXTEND)
    {
      op1 = XEXP (op1, 0);
      code1 = GET_CODE (op1);
    }

  while (code1 == SUBREG)
    {
      subreg_word1 += SUBREG_WORD (op1);
      op1 = SUBREG_REG (op1);
      code1 = GET_CODE (op1);
    }
      
  /* Sanity check.  */
  if (GET_CODE (operands[1]) == SIGN_EXTEND
      && code1 != REG
      && code1 != CONST_INT
      /* The following three can happen as the result of a questionable
	 cast.  */
      && code1 != LABEL_REF
      && code1 != SYMBOL_REF
      && code1 != CONST)
    abort ();

  if (code0 == REG)
    {
      int regno0 = REGNO (op0) + subreg_word0;

      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_word1;

	  /* Just in case, don't do anything for assigning a register
	     to itself, unless we are filling a delay slot.  */
	  if (regno0 == regno1 && set_nomacro == 0)
	    ret = "";

	  else if (FP_REG_P (regno0))
	    {
	      if (FP_REG_P (regno1))
		ret = "mov.d\t%0,%1";

	      else
		{
		  delay = DELAY_LOAD;
		  if (TARGET_FLOAT64)
		    {
		      if (!TARGET_64BIT)
			abort_with_insn (insn, "Bad move");
#ifdef TARGET_FP_CALL_32
		      if (FP_CALL_GP_REG_P (regno1))
			ret = "dsll\t%1,32\n\tor\t%1,%D1\n\tdmtc1\t%1,%0";
		      else
#endif
			ret = "dmtc1\t%1,%0";
		    }
		  else
		    ret = "mtc1\t%L1,%0\n\tmtc1\t%M1,%D0";
		}
	    }

	  else if (FP_REG_P (regno1))
	    {
	      delay = DELAY_LOAD;
	      if (TARGET_FLOAT64)
		{
		  if (!TARGET_64BIT)
		    abort_with_insn (insn, "Bad move");
#ifdef TARGET_FP_CALL_32
		  if (FP_CALL_GP_REG_P (regno0))
		    ret = "dmfc1\t%0,%1\n\tmfc1\t%D0,%1\n\tdsrl\t%0,32";
		  else
#endif
		    ret = "dmfc1\t%0,%1";
		}
	      else
		ret = "mfc1\t%L0,%1\n\tmfc1\t%M0,%D1";
	    }

	  else if (MD_REG_P (regno0) && GP_REG_P (regno1))
	    {
	      delay = DELAY_HILO;
	      if (TARGET_64BIT)
		{
		  if (regno0 != HILO_REGNUM)
		    ret = "mt%0\t%1";
		  else if (regno1 == 0)
		    ret = "mtlo\t%.\n\tmthi\t%.";
		}
	      else
		ret = "mthi\t%M1\n\tmtlo\t%L1";
	    }

	  else if (GP_REG_P (regno0) && MD_REG_P (regno1))
	    {
	      delay = DELAY_HILO;
	      if (TARGET_64BIT)
		{
		  if (regno1 != HILO_REGNUM)
		    ret = "mf%1\t%0";
		}
	      else
		ret = "mfhi\t%M0\n\tmflo\t%L0";
	    }

	  else if (TARGET_64BIT)
	    ret = "move\t%0,%1";

	  else if (regno0 != (regno1+1))
	    ret = "move\t%0,%1\n\tmove\t%D0,%D1";

	  else
	    ret = "move\t%D0,%D1\n\tmove\t%0,%1";
	}

      else if (code1 == CONST_DOUBLE)
	{
	  /* Move zero from $0 unless !TARGET_64BIT and recipient
	     is 64-bit fp reg, in which case generate a constant.  */
	  if (op1 != CONST0_RTX (GET_MODE (op1))
	      || (TARGET_FLOAT64 && !TARGET_64BIT && FP_REG_P (regno0)))
	    {
	      if (GET_MODE (op1) == DFmode)
		{
		  delay = DELAY_LOAD;
#ifdef TARGET_FP_CALL_32
		  if (FP_CALL_GP_REG_P (regno0))
		    {
		      if (TARGET_FLOAT64 && !TARGET_64BIT)
			{
			  split_double (op1, operands + 2, operands + 3);
			  ret = "li\t%0,%2\n\tli\t%D0,%3";
			}
		      else
			ret = "li.d\t%0,%1\n\tdsll\t%D0,%0,32\n\tdsrl\t%D0,32\n\tdsrl\t%0,32";
		    }
		  else
#endif
		    ret = "li.d\t%0,%1";
		}

	      else if (TARGET_64BIT)
		ret = "dli\t%0,%1";

	      else
		{
		  split_double (op1, operands + 2, operands + 3);
		  ret = "li\t%0,%2\n\tli\t%D0,%3";
		}
	    }

	  else
	    {
	      if (GP_REG_P (regno0))
		ret = (TARGET_64BIT
#ifdef TARGET_FP_CALL_32
		       && ! FP_CALL_GP_REG_P (regno0)
#endif
		       )
		  ? "move\t%0,%."
		    : "move\t%0,%.\n\tmove\t%D0,%.";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = (TARGET_64BIT)
				? "dmtc1\t%.,%0"
				: "mtc1\t%.,%0\n\tmtc1\t%.,%D0";
		}
	    }
	}

      else if (code1 == CONST_INT && INTVAL (op1) == 0)
	{
	  if (GP_REG_P (regno0))
	    ret = (TARGET_64BIT)
	                        ? "move\t%0,%."
				: "move\t%0,%.\n\tmove\t%D0,%.";
	  
	  else if (FP_REG_P (regno0))
	    {
	      delay = DELAY_LOAD;
	      ret = (TARGET_64BIT)
				? "dmtc1\t%.,%0"
				: (TARGET_FLOAT64
				   ? "li.d\t%0,%1"
				   : "mtc1\t%.,%0\n\tmtc1\t%.,%D0");
	    }
	  else if (MD_REG_P (regno0))
	    {
	      delay = DELAY_HILO;
	      if (regno0 != HILO_REGNUM)
		ret = "mt%0\t%.\n";
	      else
		ret = "mtlo\t%.\n\tmthi\t%.";
	    }
	}
	
      else if (code1 == CONST_INT && GET_MODE (op0) == DImode && GP_REG_P (regno0))
	{
	  if (TARGET_64BIT)
	    {
	      if (GET_CODE (operands[1]) == SIGN_EXTEND)
		ret = "li\t%0,%1\t\t# %X1";
	      else if (HOST_BITS_PER_WIDE_INT < 64)
		/* We can't use 'X' for negative numbers, because then we won't
		   get the right value for the upper 32 bits.  */
		ret = ((INTVAL (op1) < 0) ? "dli\t%0,%1\t\t\t# %X1"
		       : "dli\t%0,%X1\t\t# %1");
	      else
		/* We must use 'X', because otherwise LONG_MIN will print as
		   a number that the assembler won't accept.  */
		ret = "dli\t%0,%X1\t\t# %1";
	    }
	  else if (HOST_BITS_PER_WIDE_INT < 64)
	    {
	      operands[2] = GEN_INT (INTVAL (operands[1]) >= 0 ? 0 : -1);
	      ret = "li\t%M0,%2\n\tli\t%L0,%1";
	    }
	  else
	    {
	      /* We use multiple shifts here, to avoid warnings about out
		 of range shifts on 32 bit hosts.  */
	      operands[2] = GEN_INT (INTVAL (operands[1]) >> 16 >> 16);
	      operands[1] = GEN_INT (INTVAL (operands[1]) << 16 << 16 >> 16 >> 16);
	      ret = "li\t%M0,%2\n\tli\t%L0,%1";
	    }
	}

      else if (code1 == MEM)
	{
	  delay = DELAY_LOAD;

	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 2);

	  if (FP_REG_P (regno0))
	    ret = "l.d\t%0,%1";

	  else if (TARGET_64BIT)
	    {
#ifdef TARGET_FP_CALL_32
	      if (FP_CALL_GP_REG_P (regno0))
		{
                  if (offsettable_address_p (FALSE, SImode, op1))
                    ret = "lwu\t%0,%1\n\tlwu\t%D0,4+%1";
                  else
                    ret = "ld\t%0,%1\n\tdsll\t%D0,%0,32\n\tdsrl\t%D0,32\n\tdsrl\t%0,32";
		}
	      else
#endif
		ret = "ld\t%0,%1";
	    }

	  else if (offsettable_address_p (1, DFmode, XEXP (op1, 0)))
	    {
	      operands[2] = adj_offsettable_operand (op1, 4);
	      if (reg_mentioned_p (op0, op1))
		ret = "lw\t%D0,%2\n\tlw\t%0,%1";
	      else
		ret = "lw\t%0,%1\n\tlw\t%D0,%2";
	    }

	  if (ret != (char *)0 && MEM_VOLATILE_P (op1))
	    {
	      int i = strlen (ret);
	      if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
		abort ();

	      sprintf (volatile_buffer, "%%{%s%%}", ret);
	      ret = volatile_buffer;
	    }
	}

      else if (code1 == LABEL_REF
	       || code1 == SYMBOL_REF
	       || code1 == CONST)
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 2);

	  if (GET_CODE (operands[1]) == SIGN_EXTEND)
	    /* We deliberately remove the 'a' from '%1', so that we don't
	       have to add SIGN_EXTEND support to print_operand_address.
	       print_operand will just call print_operand_address in this
	       case, so there is no problem.  */
	    ret = "la\t%0,%1";
	  else
	    ret = "dla\t%0,%a1";
	}
    }

  else if (code0 == MEM)
    {
      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_word1;

	  if (FP_REG_P (regno1))
	    ret = "s.d\t%1,%0";

	  else if (TARGET_64BIT)
	    {
#ifdef TARGET_FP_CALL_32
	      if (FP_CALL_GP_REG_P (regno1))
		ret = "dsll\t%1,32\n\tor\t%1,%D1\n\tsd\t%1,%0";
	      else
#endif
		ret = "sd\t%1,%0";
	    }

	  else if (offsettable_address_p (1, DFmode, XEXP (op0, 0)))
	    {
	      operands[2] = adj_offsettable_operand (op0, 4);
	      ret = "sw\t%1,%0\n\tsw\t%D1,%2";
	    }
	}

      else if (((code1 == CONST_INT && INTVAL (op1) == 0)
		|| (code1 == CONST_DOUBLE
		    && op1 == CONST0_RTX (GET_MODE (op1))))
	       && (TARGET_64BIT
		   || offsettable_address_p (1, DFmode, XEXP (op0, 0))))
	{
	  if (TARGET_64BIT)
	    ret = "sd\t%.,%0";
	  else
	    {
	      operands[2] = adj_offsettable_operand (op0, 4);
	      ret = "sw\t%.,%0\n\tsw\t%.,%2";
	    }
	}

      if (TARGET_STATS)
	mips_count_memory_refs (op0, 2);

      if (ret != (char *)0 && MEM_VOLATILE_P (op0))
	{
	  int i = strlen (ret);
	  if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
	    abort ();
	  
	  sprintf (volatile_buffer, "%%{%s%%}", ret);
	  ret = volatile_buffer;
	}
    }

  if (ret == (char *)0)
    {
      abort_with_insn (insn, "Bad move");
      return 0;
    }

  if (delay != DELAY_NONE)
    return mips_fill_delay_slot (ret, delay, operands, insn);

  return ret;
}


/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  */

int
mips_address_cost (addr)
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    default:
      break;

    case LO_SUM:
      return 1;

    case LABEL_REF:
      return 2;

    case CONST:
      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (XEXP (addr, 0), &offset);
	if (GET_CODE (addr) == LABEL_REF)
	  return 2;

	if (GET_CODE (addr) != SYMBOL_REF)
	  return 4;

	if (! SMALL_INT (offset))
	  return 2;
      }
      /* fall through */

    case SYMBOL_REF:
      return SYMBOL_REF_FLAG (addr) ? 1 : 2;

    case PLUS:
      {
	register rtx plus0 = XEXP (addr, 0);
	register rtx plus1 = XEXP (addr, 1);

	if (GET_CODE (plus0) != REG && GET_CODE (plus1) == REG)
	  {
	    plus0 = XEXP (addr, 1);
	    plus1 = XEXP (addr, 0);
	  }

	if (GET_CODE (plus0) != REG)
	  break;

	switch (GET_CODE (plus1))
	  {
	  default:
	    break;

	  case CONST_INT:
	    return (SMALL_INT (plus1) ? 1 : 2);

	  case CONST:
	  case SYMBOL_REF:
	  case LABEL_REF:
	  case HIGH:
	  case LO_SUM:
	    return mips_address_cost (plus1) + 1;
	  }
      }
    }

  return 4;
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

/* Make normal rtx_code into something we can index from an array */

static enum internal_test
map_test_to_internal_test (test_code)
     enum rtx_code test_code;
{
  enum internal_test test = ITEST_MAX;

  switch (test_code)
    {
    default:			break;
    case EQ:  test = ITEST_EQ;  break;
    case NE:  test = ITEST_NE;  break;
    case GT:  test = ITEST_GT;  break;
    case GE:  test = ITEST_GE;  break;
    case LT:  test = ITEST_LT;  break;
    case LE:  test = ITEST_LE;  break;
    case GTU: test = ITEST_GTU; break;
    case GEU: test = ITEST_GEU; break;
    case LTU: test = ITEST_LTU; break;
    case LEU: test = ITEST_LEU; break;
    }

  return test;
}


/* Generate the code to compare two integer values.  The return value is:
   (reg:SI xx)		The pseudo register the comparison is in
   (rtx)0	       	No register, generate a simple branch.

   ??? This is called with result nonzero by the Scond patterns in
   mips.md.  These patterns are called with a target in the mode of
   the Scond instruction pattern.  Since this must be a constant, we
   must use SImode.  This means that if RESULT is non-zero, it will
   always be an SImode register, even if TARGET_64BIT is true.  We
   cope with this by calling convert_move rather than emit_move_insn.
   This will sometimes lead to an unnecessary extension of the result;
   for example:

   long long
   foo (long long i)
   {
     return i < 5;
   }

   */

rtx
gen_int_relational (test_code, result, cmp0, cmp1, p_invert)
     enum rtx_code test_code;	/* relational test (EQ, etc) */
     rtx result;		/* result to store comp. or 0 if branch */
     rtx cmp0;			/* first operand to compare */
     rtx cmp1;			/* second operand to compare */
     int *p_invert;		/* NULL or ptr to hold whether branch needs */
				/* to reverse its test */
{
  struct cmp_info {
    enum rtx_code test_code;	/* code to use in instruction (LT vs. LTU) */
    int const_low;		/* low bound of constant we can accept */
    int const_high;		/* high bound of constant we can accept */
    int const_add;		/* constant to add (convert LE -> LT) */
    int reverse_regs;		/* reverse registers in test */
    int invert_const;		/* != 0 if invert value if cmp1 is constant */
    int invert_reg;		/* != 0 if invert value if cmp1 is register */
    int unsignedp;		/* != 0 for unsigned comparisons.  */
  };

  static struct cmp_info info[ (int)ITEST_MAX ] = {

    { XOR,	 0,  65535,  0,	 0,  0,	 0, 0 },	/* EQ  */
    { XOR,	 0,  65535,  0,	 0,  1,	 1, 0 },	/* NE  */
    { LT,   -32769,  32766,  1,	 1,  1,	 0, 0 },	/* GT  */
    { LT,   -32768,  32767,  0,	 0,  1,	 1, 0 },	/* GE  */
    { LT,   -32768,  32767,  0,	 0,  0,	 0, 0 },	/* LT  */
    { LT,   -32769,  32766,  1,	 1,  0,	 1, 0 },	/* LE  */
    { LTU,  -32769,  32766,  1,	 1,  1,	 0, 1 },	/* GTU */
    { LTU,  -32768,  32767,  0,	 0,  1,	 1, 1 },	/* GEU */
    { LTU,  -32768,  32767,  0,	 0,  0,	 0, 1 },	/* LTU */
    { LTU,  -32769,  32766,  1,	 1,  0,	 1, 1 },	/* LEU */
  };

  enum internal_test test;
  enum machine_mode mode;
  struct cmp_info *p_info;
  int branch_p;
  int eqne_p;
  int invert;
  rtx reg;
  rtx reg2;

  test = map_test_to_internal_test (test_code);
  if (test == ITEST_MAX)
    abort ();

  p_info = &info[ (int)test ];
  eqne_p = (p_info->test_code == XOR);

  mode = GET_MODE (cmp0);
  if (mode == VOIDmode)
    mode = GET_MODE (cmp1);

  /* Eliminate simple branches */
  branch_p = (result == (rtx)0);
  if (branch_p)
    {
      if (GET_CODE (cmp0) == REG || GET_CODE (cmp0) == SUBREG)
	{
	  /* Comparisons against zero are simple branches */
	  if (GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) == 0)
	    return (rtx)0;

	  /* Test for beq/bne.  */
	  if (eqne_p)
	    return (rtx)0;
	}

      /* allocate a pseudo to calculate the value in.  */
      result = gen_reg_rtx (mode);
    }

  /* Make sure we can handle any constants given to us.  */
  if (GET_CODE (cmp0) == CONST_INT)
    cmp0 = force_reg (mode, cmp0);

  if (GET_CODE (cmp1) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL (cmp1);
      if (value < p_info->const_low
	  || value > p_info->const_high
	  /* ??? Why?  And why wasn't the similar code below modified too?  */
	  || (TARGET_64BIT
	      && HOST_BITS_PER_WIDE_INT < 64
	      && p_info->const_add != 0
	      && ((p_info->unsignedp
		   ? ((unsigned HOST_WIDE_INT) (value + p_info->const_add)
		      > INTVAL (cmp1))
		   : (value + p_info->const_add) > INTVAL (cmp1))
		  != (p_info->const_add > 0))))
	cmp1 = force_reg (mode, cmp1);
    }

  /* See if we need to invert the result.  */
  invert = (GET_CODE (cmp1) == CONST_INT)
		? p_info->invert_const
		: p_info->invert_reg;

  if (p_invert != (int *)0)
    {
      *p_invert = invert;
      invert = FALSE;
    }

  /* Comparison to constants, may involve adding 1 to change a LT into LE.
     Comparison between two registers, may involve switching operands.  */
  if (GET_CODE (cmp1) == CONST_INT)
    {
      if (p_info->const_add != 0)
	{
	  HOST_WIDE_INT new = INTVAL (cmp1) + p_info->const_add;
	  /* If modification of cmp1 caused overflow,
	     we would get the wrong answer if we follow the usual path;
	     thus, x > 0xffffffffU would turn into x > 0U.  */
	  if ((p_info->unsignedp
	       ? (unsigned HOST_WIDE_INT) new > INTVAL (cmp1)
	       : new > INTVAL (cmp1))
	      != (p_info->const_add > 0))
	    {
	      /* This test is always true, but if INVERT is true then
		 the result of the test needs to be inverted so 0 should
		 be returned instead.  */
	      emit_move_insn (result, invert ? const0_rtx : const_true_rtx);
	      return result;
	    }
	  else
	    cmp1 = GEN_INT (new);
	}
    }
  else if (p_info->reverse_regs)
    {
      rtx temp = cmp0;
      cmp0 = cmp1;
      cmp1 = temp;
    }

  if (test == ITEST_NE && GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) == 0)
    reg = cmp0;
  else
    {
      reg = (invert || eqne_p) ? gen_reg_rtx (mode) : result;
      convert_move (reg, gen_rtx (p_info->test_code, mode, cmp0, cmp1), 0);
    }

  if (test == ITEST_NE)
    {
      convert_move (result, gen_rtx (GTU, mode, reg, const0_rtx), 0);
      invert = FALSE;
    }

  else if (test == ITEST_EQ)
    {
      reg2 = (invert) ? gen_reg_rtx (mode) : result;
      convert_move (reg2, gen_rtx (LTU, mode, reg, const1_rtx), 0);
      reg = reg2;
    }

  if (invert)
    convert_move (result, gen_rtx (XOR, mode, reg, const1_rtx), 0);

  return result;
}


/* Emit the common code for doing conditional branches.
   operand[0] is the label to jump to.
   The comparison operands are saved away by cmp{si,di,sf,df}.  */

void
gen_conditional_branch (operands, test_code)
     rtx operands[];
     enum rtx_code test_code;
{
  enum cmp_type type = branch_type;
  rtx cmp0 = branch_cmp[0];
  rtx cmp1 = branch_cmp[1];
  enum machine_mode mode;
  rtx reg;
  int invert;
  rtx label1, label2;

  switch (type)
    {
    default:
      abort_with_insn (gen_rtx (test_code, VOIDmode, cmp0, cmp1), "bad test");

    case CMP_SI:
    case CMP_DI:
      mode = type == CMP_SI ? SImode : DImode;
      invert = FALSE;
      reg = gen_int_relational (test_code, NULL_RTX, cmp0, cmp1, &invert);
      if (reg)
	{
	  cmp0 = reg;
	  cmp1 = const0_rtx;
	  test_code = NE;
	}
      else if (GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) != 0)
	{
	  /* We don't want to build a comparison against a non-zero
             constant.  */
	  cmp1 = force_reg (mode, cmp1);
	}
      break;

    case CMP_SF:
    case CMP_DF:
      if (mips_isa < 4)
	reg = gen_rtx (REG, CCmode, FPSW_REGNUM);
      else
	reg = gen_reg_rtx (CCmode);

      /* For cmp0 != cmp1, build cmp0 == cmp1, and test for result ==
         0 in the instruction built below.  The MIPS FPU handles
         inequality testing by testing for equality and looking for a
         false result.  */
      emit_insn (gen_rtx (SET, VOIDmode,
			  reg,
			  gen_rtx (test_code == NE ? EQ : test_code,
				   CCmode, cmp0, cmp1)));

      test_code = test_code == NE ? EQ : NE;
      mode = CCmode;
      cmp0 = reg;
      cmp1 = const0_rtx;
      invert = FALSE;
      break;
    }

  /* Generate the branch.  */

  label1 = gen_rtx (LABEL_REF, VOIDmode, operands[0]);
  label2 = pc_rtx;

  if (invert)
    {
      label2 = label1;
      label1 = pc_rtx;
    }

  emit_jump_insn (gen_rtx (SET, VOIDmode,
			   pc_rtx,
			   gen_rtx (IF_THEN_ELSE, VOIDmode,
				    gen_rtx (test_code, mode, cmp0, cmp1),
				    label1,
				    label2)));
}

/* Emit the common code for conditional moves.  OPERANDS is the array
   of operands passed to the conditional move defined_expand.  */

void
gen_conditional_move (operands)
     rtx *operands;
{
  rtx op0 = branch_cmp[0];
  rtx op1 = branch_cmp[1];
  enum machine_mode mode = GET_MODE (branch_cmp[0]);
  enum rtx_code cmp_code = GET_CODE (operands[1]);
  enum rtx_code move_code = NE;
  enum machine_mode op_mode = GET_MODE (operands[0]);
  enum machine_mode cmp_mode;
  rtx cmp_reg;

  if (GET_MODE_CLASS (mode) != MODE_FLOAT)
    {
      switch (cmp_code)
	{
	case EQ:
	  cmp_code = XOR;
	  move_code = EQ;
	  break;
	case NE:
	  cmp_code = XOR;
	  break;
	case LT:
	  break;
	case GE:
	  cmp_code = LT;
	  move_code = EQ;
	  break;
	case GT:
	  cmp_code = LT;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  break;
	case LE:
	  cmp_code = LT;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  move_code = EQ;
	  break;
	case LTU:
	  break;
	case GEU:
	  cmp_code = LTU;
	  move_code = EQ;
	  break;
	case GTU:
	  cmp_code = LTU;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  break;
	case LEU:
	  cmp_code = LTU;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  move_code = EQ;
	  break;
	default:
	  abort ();
	}
    }
  else
    {
      if (cmp_code == NE)
	{
	  cmp_code = EQ;
	  move_code = EQ;
	}
    }
	  
  if (mode == SImode || mode == DImode)
    cmp_mode = mode;
  else if (mode == SFmode || mode == DFmode)
    cmp_mode = CCmode;
  else
    abort ();

  cmp_reg = gen_reg_rtx (cmp_mode);
  emit_insn (gen_rtx (SET, cmp_mode,
		      cmp_reg,
		      gen_rtx (cmp_code, cmp_mode, op0, op1)));
  emit_insn (gen_rtx (SET, op_mode,
		      operands[0],
		      gen_rtx (IF_THEN_ELSE, op_mode,
			       gen_rtx (move_code, VOIDmode,
					cmp_reg,
					CONST0_RTX (SImode)),
			       operands[2],
			       operands[3])));
}

/* Write a loop to move a constant number of bytes.  Generate load/stores as follows:

   do {
     temp1 = src[0];
     temp2 = src[1];
     ...
     temp<last> = src[MAX_MOVE_REGS-1];
     dest[0] = temp1;
     dest[1] = temp2;
     ...
     dest[MAX_MOVE_REGS-1] = temp<last>;
     src += MAX_MOVE_REGS;
     dest += MAX_MOVE_REGS;
   } while (src != final);

   This way, no NOP's are needed, and only MAX_MOVE_REGS+3 temp
   registers are needed.

   Aligned moves move MAX_MOVE_REGS*4 bytes every (2*MAX_MOVE_REGS)+3
   cycles, unaligned moves move MAX_MOVE_REGS*4 bytes every
   (4*MAX_MOVE_REGS)+3 cycles, assuming no cache misses.  */

#define MAX_MOVE_REGS 4
#define MAX_MOVE_BYTES (MAX_MOVE_REGS * UNITS_PER_WORD)

static void
block_move_loop (dest_reg, src_reg, bytes, align, orig_dest, orig_src)
     rtx dest_reg;		/* register holding destination address */
     rtx src_reg;		/* register holding source address */
     int bytes;			/* # bytes to move */
     int align;			/* alignment */
     rtx orig_dest;		/* original dest for change_address */
     rtx orig_src;		/* original source for making a reg note */
{
  rtx dest_mem		= change_address (orig_dest, BLKmode, dest_reg);
  rtx src_mem		= change_address (orig_src, BLKmode, src_reg);
  rtx align_rtx		= GEN_INT (align);
  rtx label;
  rtx final_src;
  rtx bytes_rtx;
  int leftover;

  if (bytes < 2*MAX_MOVE_BYTES)
    abort ();

  leftover = bytes % MAX_MOVE_BYTES;
  bytes -= leftover;

  label = gen_label_rtx ();
  final_src = gen_reg_rtx (Pmode);
  bytes_rtx = GEN_INT (bytes);

  if (bytes > 0x7fff)
    {
      if (TARGET_LONG64)
	{
	  emit_insn (gen_movdi (final_src, bytes_rtx));
	  emit_insn (gen_adddi3 (final_src, final_src, src_reg));
	}
      else
	{
	  emit_insn (gen_movsi (final_src, bytes_rtx));
	  emit_insn (gen_addsi3 (final_src, final_src, src_reg));
	}
    }
  else
    {
      if (TARGET_LONG64)
	emit_insn (gen_adddi3 (final_src, src_reg, bytes_rtx));
      else
	emit_insn (gen_addsi3 (final_src, src_reg, bytes_rtx));
    }

  emit_label (label);

  bytes_rtx = GEN_INT (MAX_MOVE_BYTES);
  emit_insn (gen_movstrsi_internal (dest_mem, src_mem, bytes_rtx, align_rtx));
  if (TARGET_LONG64)
    {
      emit_insn (gen_adddi3 (src_reg, src_reg, bytes_rtx));
      emit_insn (gen_adddi3 (dest_reg, dest_reg, bytes_rtx));
      emit_insn (gen_cmpdi (src_reg, final_src));
    }
  else
    {
      emit_insn (gen_addsi3 (src_reg, src_reg, bytes_rtx));
      emit_insn (gen_addsi3 (dest_reg, dest_reg, bytes_rtx));
      emit_insn (gen_cmpsi (src_reg, final_src));
    }
  emit_jump_insn (gen_bne (label));

  if (leftover)
    emit_insn (gen_movstrsi_internal (dest_mem, src_mem,
				      GEN_INT (leftover),
				      align_rtx));
}

/* Use a library function to move some bytes.  */

static void
block_move_call (dest_reg, src_reg, bytes_rtx)
     rtx dest_reg;
     rtx src_reg;
     rtx bytes_rtx;
{
  /* We want to pass the size as Pmode, which will normally be SImode
     but will be DImode if we are using 64 bit longs and pointers.  */
  if (GET_MODE (bytes_rtx) != VOIDmode
      && GET_MODE (bytes_rtx) != Pmode)
    bytes_rtx = convert_to_mode (Pmode, bytes_rtx, TRUE);

#ifdef TARGET_MEM_FUNCTIONS
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
		     VOIDmode, 3,
		     dest_reg, Pmode,
		     src_reg, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype), bytes_rtx,
				      TREE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
#else
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"), 0,
		     VOIDmode, 3,
		     src_reg, Pmode,
		     dest_reg, Pmode,
		     convert_to_mode (TYPE_MODE (integer_type_node),
				       bytes_rtx,
				       TREE_UNSIGNED (integer_type_node)),
		     TYPE_MODE (integer_type_node));
#endif
}


/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

void
expand_block_move (operands)
     rtx operands[];
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int bytes	= (constp ? INTVAL (bytes_rtx) : 0);
  int align	= INTVAL (align_rtx);
  rtx orig_src	= operands[1];
  rtx orig_dest	= operands[0];
  rtx src_reg;
  rtx dest_reg;

  if (constp && bytes <= 0)
    return;

  if (align > UNITS_PER_WORD)
    align = UNITS_PER_WORD;

  /* Move the address into scratch registers.  */
  dest_reg = copy_addr_to_reg (XEXP (orig_dest, 0));
  src_reg  = copy_addr_to_reg (XEXP (orig_src, 0));

  if (TARGET_MEMCPY)
    block_move_call (dest_reg, src_reg, bytes_rtx);

  else if (constp && bytes <= 2*MAX_MOVE_BYTES)
    emit_insn (gen_movstrsi_internal (change_address (orig_dest, BLKmode,
						      dest_reg),
				      change_address (orig_src, BLKmode,
						      src_reg),
				      bytes_rtx, align_rtx));

  else if (constp && align >= UNITS_PER_WORD && optimize)
    block_move_loop (dest_reg, src_reg, bytes, align, orig_dest, orig_src);

  else if (constp && optimize)
    {
      /* If the alignment is not word aligned, generate a test at
	 runtime, to see whether things wound up aligned, and we
	 can use the faster lw/sw instead ulw/usw.  */

      rtx temp		= gen_reg_rtx (Pmode);
      rtx aligned_label = gen_label_rtx ();
      rtx join_label	= gen_label_rtx ();
      int leftover	= bytes % MAX_MOVE_BYTES;

      bytes -= leftover;

      if (TARGET_LONG64)
	{
	  emit_insn (gen_iordi3 (temp, src_reg, dest_reg));
	  emit_insn (gen_anddi3 (temp, temp, GEN_INT (UNITS_PER_WORD-1)));
	  emit_insn (gen_cmpdi (temp, const0_rtx));
	}
      else
	{
	  emit_insn (gen_iorsi3 (temp, src_reg, dest_reg));
	  emit_insn (gen_andsi3 (temp, temp, GEN_INT (UNITS_PER_WORD-1)));
	  emit_insn (gen_cmpsi (temp, const0_rtx));
	}
      emit_jump_insn (gen_beq (aligned_label));

      /* Unaligned loop.  */
      block_move_loop (dest_reg, src_reg, bytes, 1, orig_dest, orig_src);
      emit_jump_insn (gen_jump (join_label));
      emit_barrier ();

      /* Aligned loop.  */
      emit_label (aligned_label);
      block_move_loop (dest_reg, src_reg, bytes, UNITS_PER_WORD, orig_dest,
		       orig_src);
      emit_label (join_label);

      /* Bytes at the end of the loop.  */
      if (leftover)
	emit_insn (gen_movstrsi_internal (change_address (orig_dest, BLKmode,
							  dest_reg),
					  change_address (orig_src, BLKmode,
							  src_reg),
					  GEN_INT (leftover),
					  GEN_INT (align)));
    }

  else
    block_move_call (dest_reg, src_reg, bytes_rtx);
}


/* Emit load/stores for a small constant block_move. 

   operands[0] is the memory address of the destination.
   operands[1] is the memory address of the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.
   operands[4] is a temp register.
   operands[5] is a temp register.
   ...
   operands[3+num_regs] is the last temp register.

   The block move type can be one of the following:
	BLOCK_MOVE_NORMAL	Do all of the block move.
	BLOCK_MOVE_NOT_LAST	Do all but the last store.
	BLOCK_MOVE_LAST		Do just the last store. */

char *
output_block_move (insn, operands, num_regs, move_type)
     rtx insn;
     rtx operands[];
     int num_regs;
     enum block_move_type move_type;
{
  rtx dest_reg		= XEXP (operands[0], 0);
  rtx src_reg		= XEXP (operands[1], 0);
  int bytes		= INTVAL (operands[2]);
  int align		= INTVAL (operands[3]);
  int num		= 0;
  int offset		= 0;
  int use_lwl_lwr	= FALSE;
  int last_operand	= num_regs+4;
  int safe_regs		= 4;
  int i;
  rtx xoperands[10];

  struct {
    char *load;			/* load insn without nop */
    char *load_nop;		/* load insn with trailing nop */
    char *store;		/* store insn */
    char *final;		/* if last_store used: NULL or swr */
    char *last_store;		/* last store instruction */
    int offset;			/* current offset */
    enum machine_mode mode;	/* mode to use on (MEM) */
  } load_store[4];

  /* Detect a bug in GCC, where it can give us a register
     the same as one of the addressing registers and reduce
     the number of registers available.  */
  for (i = 4;
       i < last_operand && safe_regs < (sizeof(xoperands) / sizeof(xoperands[0]));
       i++)
    {
      if (!reg_mentioned_p (operands[i], operands[0])
	  && !reg_mentioned_p (operands[i], operands[1]))

	xoperands[safe_regs++] = operands[i];
    }

  if (safe_regs < last_operand)
    {
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      xoperands[2] = operands[2];
      xoperands[3] = operands[3];
      return output_block_move (insn, xoperands, safe_regs-4, move_type);
    }

  /* If we are given global or static addresses, and we would be
     emitting a few instructions, try to save time by using a
     temporary register for the pointer.  */
  /* ??? The SGI Irix6 assembler fails when a SYMBOL_REF is used in
     an ldl/ldr instruction pair.  We play it safe, and always move
     constant addresses into registers when generating N32/N64 code, just
     in case we might emit an unaligned load instruction.  */
  if (num_regs > 2 && (bytes > 2*align || move_type != BLOCK_MOVE_NORMAL
		       || mips_abi == ABI_N32 || mips_abi == ABI_64))
    {
      if (CONSTANT_P (src_reg))
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (operands[1], 1);

	  src_reg = operands[ 3 + num_regs-- ];
	  if (move_type != BLOCK_MOVE_LAST)
	    {
	      xoperands[1] = operands[1];
	      xoperands[0] = src_reg;
	      if (Pmode == DImode)
		output_asm_insn ("dla\t%0,%1", xoperands);
	      else
		output_asm_insn ("la\t%0,%1", xoperands);
	    }
	}

      if (CONSTANT_P (dest_reg))
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (operands[0], 1);

	  dest_reg = operands[ 3 + num_regs-- ];
	  if (move_type != BLOCK_MOVE_LAST)
	    {
	      xoperands[1] = operands[0];
	      xoperands[0] = dest_reg;
	      if (Pmode == DImode)
		output_asm_insn ("dla\t%0,%1", xoperands);
	      else
		output_asm_insn ("la\t%0,%1", xoperands);
	    }
	}
    }

  /* ??? We really shouldn't get any LO_SUM addresses here, because they
     are not offsettable, however, offsettable_address_p says they are
     offsettable. I think this is a bug in offsettable_address_p.
     For expediency, we fix this by just loading the address into a register
     if we happen to get one.  */

  if (GET_CODE (src_reg) == LO_SUM)
    {
      src_reg = operands[ 3 + num_regs-- ];
      if (move_type != BLOCK_MOVE_LAST)
	{
	  xoperands[2] = XEXP (XEXP (operands[1], 0), 1);
	  xoperands[1] = XEXP (XEXP (operands[1], 0), 0);
	  xoperands[0] = src_reg;
	  if (Pmode == DImode)
	    output_asm_insn ("daddiu\t%0,%1,%%lo(%2)", xoperands);
	  else
	    output_asm_insn ("addiu\t%0,%1,%%lo(%2)", xoperands);
	}
    }

  if (GET_CODE (dest_reg) == LO_SUM)
    {
      dest_reg = operands[ 3 + num_regs-- ];
      if (move_type != BLOCK_MOVE_LAST)
	{
	  xoperands[2] = XEXP (XEXP (operands[0], 0), 1);
	  xoperands[1] = XEXP (XEXP (operands[0], 0), 0);
	  xoperands[0] = dest_reg;
	  if (Pmode == DImode)
	    output_asm_insn ("daddiu\t%0,%1,%%lo(%2)", xoperands);
	  else
	    output_asm_insn ("addiu\t%0,%1,%%lo(%2)", xoperands);
	}
    }

  if (num_regs > (sizeof (load_store) / sizeof (load_store[0])))
    num_regs = (sizeof (load_store) / sizeof (load_store[0]));

  else if (num_regs < 1)
    abort_with_insn (insn, "Cannot do block move, not enough scratch registers");

  while (bytes > 0)
    {
      load_store[num].offset = offset;

      if (TARGET_64BIT && bytes >= 8 && align >= 8)
	{
	  load_store[num].load       = "ld\t%0,%1";
	  load_store[num].load_nop   = "ld\t%0,%1%#";
	  load_store[num].store      = "sd\t%0,%1";
	  load_store[num].last_store = "sd\t%0,%1";
	  load_store[num].final      = (char *)0;
	  load_store[num].mode       = DImode;
	  offset += 8;
	  bytes -= 8;
	}

      else if (TARGET_64BIT && bytes >= 8)
	{
	  if (BYTES_BIG_ENDIAN)
	    {
	      load_store[num].load       = "ldl\t%0,%1\n\tldr\t%0,%2";
	      load_store[num].load_nop   = "ldl\t%0,%1\n\tldr\t%0,%2%#";
	      load_store[num].store      = "sdl\t%0,%1\n\tsdr\t%0,%2";
	      load_store[num].last_store = "sdr\t%0,%2";
	      load_store[num].final      = "sdl\t%0,%1";
	    }
	  else
	    {
	      load_store[num].load	     = "ldl\t%0,%2\n\tldr\t%0,%1";
	      load_store[num].load_nop   = "ldl\t%0,%2\n\tldr\t%0,%1%#";
	      load_store[num].store	     = "sdl\t%0,%2\n\tsdr\t%0,%1";
	      load_store[num].last_store = "sdr\t%0,%1";
	      load_store[num].final      = "sdl\t%0,%2";
	    }
	  load_store[num].mode = DImode;
	  offset += 8;
	  bytes -= 8;
	  use_lwl_lwr = TRUE;
	}

      else if (bytes >= 4 && align >= 4)
	{
	  load_store[num].load       = "lw\t%0,%1";
	  load_store[num].load_nop   = "lw\t%0,%1%#";
	  load_store[num].store      = "sw\t%0,%1";
	  load_store[num].last_store = "sw\t%0,%1";
	  load_store[num].final      = (char *)0;
	  load_store[num].mode       = SImode;
	  offset += 4;
	  bytes -= 4;
	}

      else if (bytes >= 4)
	{
	  if (BYTES_BIG_ENDIAN)
	    {
	      load_store[num].load       = "lwl\t%0,%1\n\tlwr\t%0,%2";
	      load_store[num].load_nop   = "lwl\t%0,%1\n\tlwr\t%0,%2%#";
	      load_store[num].store      = "swl\t%0,%1\n\tswr\t%0,%2";
	      load_store[num].last_store = "swr\t%0,%2";
	      load_store[num].final      = "swl\t%0,%1";
	    }
	  else
	    {
	      load_store[num].load	     = "lwl\t%0,%2\n\tlwr\t%0,%1";
	      load_store[num].load_nop   = "lwl\t%0,%2\n\tlwr\t%0,%1%#";
	      load_store[num].store	     = "swl\t%0,%2\n\tswr\t%0,%1";
	      load_store[num].last_store = "swr\t%0,%1";
	      load_store[num].final      = "swl\t%0,%2";
	    }
	  load_store[num].mode = SImode;
	  offset += 4;
	  bytes -= 4;
	  use_lwl_lwr = TRUE;
	}

      else if (bytes >= 2 && align >= 2)
	{
	  load_store[num].load	     = "lh\t%0,%1";
	  load_store[num].load_nop   = "lh\t%0,%1%#";
	  load_store[num].store	     = "sh\t%0,%1";
	  load_store[num].last_store = "sh\t%0,%1";
	  load_store[num].final      = (char *)0;
	  load_store[num].mode	     = HImode;
	  offset += 2;
	  bytes -= 2;
	}

      else
	{
	  load_store[num].load	     = "lb\t%0,%1";
	  load_store[num].load_nop   = "lb\t%0,%1%#";
	  load_store[num].store	     = "sb\t%0,%1";
	  load_store[num].last_store = "sb\t%0,%1";
	  load_store[num].final      = (char *)0;
	  load_store[num].mode	     = QImode;
	  offset++;
	  bytes--;
	}

      if (TARGET_STATS && move_type != BLOCK_MOVE_LAST)
	{
	  dslots_load_total++;
	  dslots_load_filled++;

	  if (CONSTANT_P (src_reg))
	    mips_count_memory_refs (src_reg, 1);

	  if (CONSTANT_P (dest_reg))
	    mips_count_memory_refs (dest_reg, 1);
	}

      /* Emit load/stores now if we have run out of registers or are
	 at the end of the move.  */

      if (++num == num_regs || bytes == 0)
	{
	  /* If only load/store, we need a NOP after the load.  */
	  if (num == 1)
	    {
	      load_store[0].load = load_store[0].load_nop;
	      if (TARGET_STATS && move_type != BLOCK_MOVE_LAST)
		dslots_load_filled--;
	    }

	  if (move_type != BLOCK_MOVE_LAST)
	    {
	      for (i = 0; i < num; i++)
		{
		  int offset;

		  if (!operands[i+4])
		    abort ();

		  if (GET_MODE (operands[i+4]) != load_store[i].mode)
		    operands[i+4] = gen_rtx (REG, load_store[i].mode, REGNO (operands[i+4]));

		  offset = load_store[i].offset;
		  xoperands[0] = operands[i+4];
		  xoperands[1] = gen_rtx (MEM, load_store[i].mode,
					  plus_constant (src_reg, offset));

		  if (use_lwl_lwr)
		    {
		      int extra_offset;
		      extra_offset = GET_MODE_SIZE (load_store[i].mode) - 1;
		      xoperands[2] = gen_rtx (MEM, load_store[i].mode,
					      plus_constant (src_reg,
							     extra_offset
							     + offset));
		    }

		  output_asm_insn (load_store[i].load, xoperands);
		}
	    }

	  for (i = 0; i < num; i++)
	    {
	      int last_p = (i == num-1 && bytes == 0);
	      int offset = load_store[i].offset;

	      xoperands[0] = operands[i+4];
	      xoperands[1] = gen_rtx (MEM, load_store[i].mode,
				      plus_constant (dest_reg, offset));


	      if (use_lwl_lwr)
		{
		  int extra_offset;
		  extra_offset = GET_MODE_SIZE (load_store[i].mode) - 1;
		  xoperands[2] = gen_rtx (MEM, load_store[i].mode,
					  plus_constant (dest_reg,
							 extra_offset
							 + offset));
		}

	      if (move_type == BLOCK_MOVE_NORMAL)
		output_asm_insn (load_store[i].store, xoperands);

	      else if (move_type == BLOCK_MOVE_NOT_LAST)
		{
		  if (!last_p)
		    output_asm_insn (load_store[i].store, xoperands);

		  else if (load_store[i].final != (char *)0)
		    output_asm_insn (load_store[i].final, xoperands);
		}

	      else if (last_p)
		output_asm_insn (load_store[i].last_store, xoperands);
	    }

	  num = 0;		/* reset load_store */
	  use_lwl_lwr = FALSE;
	}
    }

  return "";
}


/* Argument support functions.  */

/* Initialize CUMULATIVE_ARGS for a function.  */

void
init_cumulative_args (cum, fntype, libname)
     CUMULATIVE_ARGS *cum;	/* argument info to initialize */
     tree fntype;		/* tree ptr for function decl */
     rtx libname;		/* SYMBOL_REF of library name or 0 */
{
  static CUMULATIVE_ARGS zero_cum;
  tree param, next_param;

  if (TARGET_DEBUG_E_MODE)
    {
      fprintf (stderr, "\ninit_cumulative_args, fntype = 0x%.8lx", (long)fntype);
      if (!fntype)
	fputc ('\n', stderr);

      else
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, ", fntype code = %s, ret code = %s\n",
		   tree_code_name[ (int)TREE_CODE (fntype) ],
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}
    }

  *cum = zero_cum;

  /* Determine if this function has variable arguments.  This is
     indicated by the last argument being 'void_type_mode' if there
     are no variable arguments.  The standard MIPS calling sequence
     passes all arguments in the general purpose registers in this
     case. */

  for (param = (fntype) ? TYPE_ARG_TYPES (fntype) : 0;
       param != (tree)0;
       param = next_param)
    {
      next_param = TREE_CHAIN (param);
      if (next_param == (tree)0 && TREE_VALUE (param) != void_type_node)
	cum->gp_reg_found = 1;
    }
}

/* Advance the argument to the next argument position.  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* whether or not the argument was named */
{
  if (TARGET_DEBUG_E_MODE)
    fprintf (stderr,
	     "function_adv( {gp reg found = %d, arg # = %2d, words = %2d}, %4s, 0x%.8x, %d )\n\n",
	     cum->gp_reg_found, cum->arg_number, cum->arg_words, GET_MODE_NAME (mode),
	     type, named);

  cum->arg_number++;
  switch (mode)
    {
    case VOIDmode:
      break;

    default:
      if (GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
	abort ();
      cum->gp_reg_found = 1;
      cum->arg_words += ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)
			 / UNITS_PER_WORD);
      break;

    case BLKmode:
      cum->gp_reg_found = 1;
      cum->arg_words += ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
			 / UNITS_PER_WORD);
      break;

    case SFmode:
      if (mips_abi == ABI_EABI && ! TARGET_SOFT_FLOAT)
	cum->fp_arg_words++;
      else
	cum->arg_words++;
      break;

    case DFmode:
      if (mips_abi == ABI_EABI && ! TARGET_SOFT_FLOAT && ! TARGET_SINGLE_FLOAT)
	cum->fp_arg_words += (TARGET_64BIT ? 1 : 2);
      else
	cum->arg_words += (TARGET_64BIT ? 1 : 2);
      break;

    case DImode:
      cum->gp_reg_found = 1;
      cum->arg_words += (TARGET_64BIT ? 1 : 2);
      break;

    case QImode:
    case HImode:
    case SImode:
      cum->gp_reg_found = 1;
      cum->arg_words++;
      break;
    }
}

/* Return an RTL expression containing the register for the given mode,
   or 0 if the argument is to be passed on the stack.  */

struct rtx_def *
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  rtx ret;
  int regbase = -1;
  int bias = 0;
  int *arg_words = &cum->arg_words;
  int struct_p = ((type != (tree)0)
		  && (TREE_CODE (type) == RECORD_TYPE
		      || TREE_CODE (type) == UNION_TYPE));

  if (TARGET_DEBUG_E_MODE)
    fprintf (stderr,
	     "function_arg( {gp reg found = %d, arg # = %2d, words = %2d}, %4s, 0x%.8x, %d ) = ",
	     cum->gp_reg_found, cum->arg_number, cum->arg_words, GET_MODE_NAME (mode),
	     type, named);

  cum->last_arg_fp = 0;
  switch (mode)
    {
    case SFmode:
      if (mips_abi == ABI_32)
	{
	  if (cum->gp_reg_found || cum->arg_number >= 2 || TARGET_SOFT_FLOAT)
	    regbase = GP_ARG_FIRST;
	  else
	    {
	      regbase = FP_ARG_FIRST;
	      /* If the first arg was a float in a floating point register,
		 then set bias to align this float arg properly.  */
	      if (cum->arg_words == 1)
		bias = 1;
	    }
	}
      else if (mips_abi == ABI_EABI && ! TARGET_SOFT_FLOAT)
	{
	  if (! TARGET_64BIT)
	    cum->fp_arg_words += cum->fp_arg_words & 1;
	  cum->last_arg_fp = 1;
	  arg_words = &cum->fp_arg_words;
	  regbase = FP_ARG_FIRST;
	}
      else
	regbase = (TARGET_SOFT_FLOAT || ! named ? GP_ARG_FIRST : FP_ARG_FIRST);
      break;

    case DFmode:
      if (! TARGET_64BIT)
	{
	  if (mips_abi == ABI_EABI
	      && ! TARGET_SOFT_FLOAT
	      && ! TARGET_SINGLE_FLOAT)
	    cum->fp_arg_words += cum->fp_arg_words & 1;
	  else
	    cum->arg_words += cum->arg_words & 1;
	}
      if (mips_abi == ABI_32)
	regbase = ((cum->gp_reg_found
		    || TARGET_SOFT_FLOAT
		    || TARGET_SINGLE_FLOAT
		    || cum->arg_number >= 2)
		   ? GP_ARG_FIRST
		   : FP_ARG_FIRST);
      else if (mips_abi == ABI_EABI
	       && ! TARGET_SOFT_FLOAT
	       && ! TARGET_SINGLE_FLOAT)
	{
	  cum->last_arg_fp = 1;
	  arg_words = &cum->fp_arg_words;
	  regbase = FP_ARG_FIRST;
	}
      else
	regbase = (TARGET_SOFT_FLOAT || TARGET_SINGLE_FLOAT || ! named
		   ? GP_ARG_FIRST : FP_ARG_FIRST);
      break;

    default:
      if (GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
	abort ();

      /* Drops through.  */
    case BLKmode:
      if (type != (tree)0 && TYPE_ALIGN (type) > BITS_PER_WORD
	  && ! TARGET_64BIT && mips_abi != ABI_EABI)
	cum->arg_words += (cum->arg_words & 1);
      regbase = GP_ARG_FIRST;
      break;

    case VOIDmode:
    case QImode:
    case HImode:
    case SImode:
      regbase = GP_ARG_FIRST;
      break;

    case DImode:
      if (! TARGET_64BIT)
	cum->arg_words += (cum->arg_words & 1);
      regbase = GP_ARG_FIRST;
    }

  if (*arg_words >= MAX_ARGS_IN_REGISTERS)
    {
      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "<stack>%s\n", struct_p ? ", [struct]" : "");

      ret = (rtx)0;
    }
  else
    {
      if (regbase == -1)
	abort ();

      if (! type || TREE_CODE (type) != RECORD_TYPE || mips_abi == ABI_32
	  || mips_abi == ABI_EABI || ! named)
	ret = gen_rtx (REG, mode, regbase + *arg_words + bias);
      else
	{
	  /* The Irix 6 n32/n64 ABIs say that if any 64 bit chunk of the
	     structure contains a double in its entirety, then that 64 bit
	     chunk is passed in a floating point register.  */
	  tree field;

	  /* First check to see if there is any such field.  */
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    if (TREE_CODE (field) == FIELD_DECL
		&& TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
		&& TYPE_PRECISION (TREE_TYPE (field)) == BITS_PER_WORD
		&& (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field))
		    % BITS_PER_WORD == 0))
	      break;

	  /* If the whole struct fits a DFmode register,
	     we don't need the PARALLEL.  */
	  if (! field || mode == DFmode)
	    ret = gen_rtx (REG, mode, regbase + *arg_words + bias);
	  else
	    {
	      /* Now handle the special case by returning a PARALLEL
		 indicating where each 64 bit chunk goes.  */
	      int chunks;
	      int bitpos;
	      int regno;
	      int i;

	      /* ??? If this is a packed structure, then the last hunk won't
		 be 64 bits.  */

	      chunks = TREE_INT_CST_LOW (TYPE_SIZE (type)) / BITS_PER_WORD;
	      if (chunks + *arg_words + bias > MAX_ARGS_IN_REGISTERS)
		chunks = MAX_ARGS_IN_REGISTERS - *arg_words - bias;

	      /* assign_parms checks the mode of ENTRY_PARM, so we must
		 use the actual mode here.  */
	      ret = gen_rtx (PARALLEL, mode, rtvec_alloc (chunks));

	      bitpos = 0;
	      regno = regbase + *arg_words + bias;
	      field = TYPE_FIELDS (type);
	      for (i = 0; i < chunks; i++)
		{
		  rtx reg;

		  for (; field; field = TREE_CHAIN (field))
		    if (TREE_CODE (field) == FIELD_DECL
			&& (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field))
			    >= bitpos))
		      break;

		  if (field
		      && TREE_INT_CST_LOW (DECL_FIELD_BITPOS (field)) == bitpos
		      && TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
		      && TYPE_PRECISION (TREE_TYPE (field)) == BITS_PER_WORD)
		    reg = gen_rtx (REG, DFmode,
				   regno + FP_ARG_FIRST - GP_ARG_FIRST);
		  else
		    reg = gen_rtx (REG, word_mode, regno);

		  XVECEXP (ret, 0, i) = gen_rtx (EXPR_LIST, VOIDmode, reg,
						 GEN_INT (bitpos / BITS_PER_UNIT));

		  bitpos += 64;
		  regno++;
		}
	    }
	}

      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "%s%s\n", reg_names[regbase + *arg_words + bias],
		 struct_p ? ", [struct]" : "");

      /* The following is a hack in order to pass 1 byte structures
	 the same way that the MIPS compiler does (namely by passing
	 the structure in the high byte or half word of the register).
	 This also makes varargs work.  If we have such a structure,
	 we save the adjustment RTL, and the call define expands will
	 emit them.  For the VOIDmode argument (argument after the
	 last real argument), pass back a parallel vector holding each
	 of the adjustments.  */

      /* ??? function_arg can be called more than once for each argument.
	 As a result, we compute more adjustments than we need here.
	 See the CUMULATIVE_ARGS definition in mips.h.  */

      /* ??? This scheme requires everything smaller than the word size to
	 shifted to the left, but when TARGET_64BIT and ! TARGET_INT64,
	 that would mean every int needs to be shifted left, which is very
	 inefficient.  Let's not carry this compatibility to the 64 bit
	 calling convention for now.  */

      if (struct_p && int_size_in_bytes (type) < UNITS_PER_WORD
	  && ! TARGET_64BIT && mips_abi != ABI_EABI)
	{
	  rtx amount = GEN_INT (BITS_PER_WORD
				- int_size_in_bytes (type) * BITS_PER_UNIT);
	  rtx reg = gen_rtx (REG, word_mode, regbase + *arg_words + bias);
	  if (TARGET_64BIT)
	    cum->adjust[ cum->num_adjusts++ ] = gen_ashldi3 (reg, reg, amount);
	  else
	    cum->adjust[ cum->num_adjusts++ ] = gen_ashlsi3 (reg, reg, amount);
	}
    }

  if (mode == VOIDmode && cum->num_adjusts > 0)
    ret = gen_rtx (PARALLEL, VOIDmode, gen_rtvec_v (cum->num_adjusts, cum->adjust));

  return ret;
}


int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  if ((mode == BLKmode
       || GET_MODE_CLASS (mode) != MODE_COMPLEX_INT
       || GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
      && cum->arg_words < MAX_ARGS_IN_REGISTERS
      && mips_abi != ABI_EABI)
    {
      int words;
      if (mode == BLKmode)
	words = ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
		 / UNITS_PER_WORD);
      else
	words = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

      if (words + cum->arg_words <= MAX_ARGS_IN_REGISTERS)
	return 0;		/* structure fits in registers */

      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "function_arg_partial_nregs = %d\n",
		 MAX_ARGS_IN_REGISTERS - cum->arg_words);

      return MAX_ARGS_IN_REGISTERS - cum->arg_words;
    }

  else if (mode == DImode && cum->arg_words == MAX_ARGS_IN_REGISTERS-1
	   && ! TARGET_64BIT
	   && mips_abi != ABI_EABI)
    {
      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "function_arg_partial_nregs = 1\n");

      return 1;
    }

  return 0;
}

/* Abort after printing out a specific insn.  */

void
abort_with_insn (insn, reason)
     rtx insn;
     char *reason;
{
  error (reason);
  debug_rtx (insn);
  abort ();
}

/* Write a message to stderr (for use in macros expanded in files that do not
   include stdio.h).  */

void
trace (s, s1, s2)
     char *s, *s1, *s2;
{
  fprintf (stderr, s, s1, s2);
}


#ifdef SIGINFO

static void
siginfo (signo)
     int signo;
{
  fprintf (stderr, "compiling '%s' in '%s'\n",
	   (current_function_name != (char *)0) ? current_function_name : "<toplevel>",
	   (current_function_file != (char *)0) ? current_function_file : "<no file>");
  fflush (stderr);
}
#endif /* SIGINFO */


/* Set up the threshold for data to go into the small data area, instead
   of the normal data area, and detect any conflicts in the switches.  */

void
override_options ()
{
  register int i, start;
  register int regno;
  register enum machine_mode mode;

  mips_section_threshold = (g_switch_set) ? g_switch_value : MIPS_DEFAULT_GVALUE;

  if (mips_section_threshold <= 0)
    target_flags &= ~MASK_GPOPT;
  else if (optimize)
    target_flags |= MASK_GPOPT;

  /* Get the architectural level.  */
  if (mips_isa_string == (char *)0)
    {
#ifdef MIPS_ISA_DEFAULT
      mips_isa = MIPS_ISA_DEFAULT;
#else
      mips_isa = 1;
#endif
    }

  else if (isdigit (*mips_isa_string))
    {
      mips_isa = atoi (mips_isa_string);
      if (mips_isa < 1 || mips_isa > 4)
	{
	  error ("-mips%d not supported", mips_isa);
	  mips_isa = 1;
	}
    }

  else
    {
      error ("bad value (%s) for -mips switch", mips_isa_string);
      mips_isa = 1;
    }

#ifdef MIPS_ABI_DEFAULT
  /* Get the ABI to use.  Currently this code is only used for Irix 6.  */
  if (mips_abi_string == (char *) 0)
    mips_abi = MIPS_ABI_DEFAULT;
  else if (! strcmp (mips_abi_string, "32")
	   || ! strcmp (mips_abi_string, "o32"))
    mips_abi = ABI_32;
  else if (! strcmp (mips_abi_string, "n32"))
    mips_abi = ABI_N32;
  else if (! strcmp (mips_abi_string, "64")
	   || ! strcmp (mips_abi_string, "n64"))
    mips_abi = ABI_64;
  else if (! strcmp (mips_abi_string, "eabi"))
    mips_abi = ABI_EABI;
  else
    error ("bad value (%s) for -mabi= switch", mips_abi_string);

  /* A specified ISA defaults the ABI if it was not specified.  */
  if (mips_abi_string == 0 && mips_isa_string && mips_abi != ABI_EABI)
    {
      if (mips_isa <= 2)
	mips_abi = ABI_32;
      else
	mips_abi = ABI_64;
    }
  /* A specified ABI defaults the ISA if it was not specified.  */
  else if (mips_isa_string == 0 && mips_abi_string && mips_abi != ABI_EABI)
    {
      if (mips_abi == ABI_32)
	mips_isa = 1;
      else if (mips_abi == ABI_N32)
	mips_isa = 3;
      else
	mips_isa = 4;
    }
  /* If both ABI and ISA were specified, check for conflicts.  */
  else if (mips_isa_string && mips_abi_string)
    {
      if ((mips_isa <= 2 && (mips_abi == ABI_N32 || mips_abi == ABI_64))
	  || (mips_isa >= 3 && mips_abi == ABI_32))
	error ("-mabi=%s does not support -mips%d", mips_abi_string, mips_isa);
    }

  /* Override TARGET_DEFAULT if necessary.  */
  if (mips_abi == ABI_32)
    target_flags &= ~ (MASK_FLOAT64|MASK_64BIT);

  /* In the EABI in 64 bit mode, longs and pointers are 64 bits.  Likewise
   for the SGI Irix6 N64 ABI.  */
  if ((mips_abi == ABI_EABI && TARGET_64BIT)
      || mips_abi == ABI_64)
    target_flags |= MASK_LONG64;

  /* ??? This doesn't work yet, so don't let people try to use it.  */
  if (mips_abi == ABI_32)
    error ("The -mabi=32 support does not work yet.");
#else
  if (mips_abi_string)
    error ("This target does not support the -mabi switch.");
#endif

#ifdef MIPS_CPU_STRING_DEFAULT
  /* ??? There is a minor inconsistency here.  If the user specifies an ISA
     greater than that supported by the default processor, then the user gets
     an error.  Normally, the compiler will just default to the base level cpu
     for the indicated isa.  */
  if (mips_cpu_string == (char *)0)
    mips_cpu_string = MIPS_CPU_STRING_DEFAULT;
#endif

  /* Identify the processor type */
  if (mips_cpu_string == (char *)0
      || !strcmp (mips_cpu_string, "default")
      || !strcmp (mips_cpu_string, "DEFAULT"))
    {
      switch (mips_isa)
	{
	default:
	  mips_cpu_string = "3000";
	  mips_cpu = PROCESSOR_R3000;
	  break;
	case 2:
	  mips_cpu_string = "6000";
	  mips_cpu = PROCESSOR_R6000;
	  break;
	case 3:
	  mips_cpu_string = "4000";
	  mips_cpu = PROCESSOR_R4000;
	  break;
	case 4:
	  mips_cpu_string = "8000";
	  mips_cpu = PROCESSOR_R8000;
	  break;
	}
    }

  else
    {
      char *p = mips_cpu_string;
      int seen_v = FALSE;

      /* We need to cope with the various "vr" prefixes for the NEC 4300
	 and 4100 processors.  */
      if (*p == 'v' || *p == 'V')
	{
	  seen_v = TRUE;
	  p++;
        }
      if (*p == 'r' || *p == 'R')
	p++;

      /* Since there is no difference between a R2000 and R3000 in
	 terms of the scheduler, we collapse them into just an R3000. */

      mips_cpu = PROCESSOR_DEFAULT;
      switch (*p)
	{
	case '2':
	  if (!strcmp (p, "2000") || !strcmp (p, "2k") || !strcmp (p, "2K"))
	    mips_cpu = PROCESSOR_R3000;
	  break;

	case '3':
	  if (!strcmp (p, "3000") || !strcmp (p, "3k") || !strcmp (p, "3K"))
	    mips_cpu = PROCESSOR_R3000;
	  else if (!strcmp (p, "3900"))
	    mips_cpu = PROCESSOR_R3900;
	  break;

	case '4':
	  if (!strcmp (p, "4000") || !strcmp (p, "4k") || !strcmp (p, "4K"))
	    mips_cpu = PROCESSOR_R4000;
          /* The vr4100 is a non-FP ISA III processor with some extra
             instructions.  */
	  else if (!strcmp (p, "4100")) {
            mips_cpu = PROCESSOR_R4100;
            target_flags |= MASK_SOFT_FLOAT ;
          }
	  /* The vr4300 is a standard ISA III processor, but with a different
	     pipeline.  */
	  else if (!strcmp (p, "4300"))
            mips_cpu = PROCESSOR_R4300;
	  /* The r4400 is exactly the same as the r4000 from the compiler's
	     viewpoint.  */
	  else if (!strcmp (p, "4400"))
	    mips_cpu = PROCESSOR_R4000;
	  else if (!strcmp (p, "4600"))
	    mips_cpu = PROCESSOR_R4600;
	  else if (!strcmp (p, "4650"))
	    mips_cpu = PROCESSOR_R4650;
	  break;

	case '5':
	  if (!strcmp (p, "5000") || !strcmp (p, "5k") || !strcmp (p, "5K"))
	    mips_cpu = PROCESSOR_R5000;
	  break;

	case '6':
	  if (!strcmp (p, "6000") || !strcmp (p, "6k") || !strcmp (p, "6K"))
	    mips_cpu = PROCESSOR_R6000;
	  break;

	case '8':
	  if (!strcmp (p, "8000"))
	    mips_cpu = PROCESSOR_R8000;
	  break;

	case 'o':
	  if (!strcmp (p, "orion"))
	    mips_cpu = PROCESSOR_R4600;
	  break;
	}

      if (seen_v
	  && mips_cpu != PROCESSOR_R4300
	  && mips_cpu != PROCESSOR_R4100
	  && mips_cpu != PROCESSOR_R5000)
	mips_cpu = PROCESSOR_DEFAULT;

      if (mips_cpu == PROCESSOR_DEFAULT)
	{
	  error ("bad value (%s) for -mcpu= switch", mips_cpu_string);
	  mips_cpu_string = "default";
	}
    }

  if ((mips_cpu == PROCESSOR_R3000 && mips_isa > 1)
      || (mips_cpu == PROCESSOR_R6000 && mips_isa > 2)
      || ((mips_cpu == PROCESSOR_R4000
           || mips_cpu == PROCESSOR_R4100
           || mips_cpu == PROCESSOR_R4300
	   || mips_cpu == PROCESSOR_R4600
	   || mips_cpu == PROCESSOR_R4650)
	  && mips_isa > 3))
    error ("-mcpu=%s does not support -mips%d", mips_cpu_string, mips_isa);

  /* make sure sizes of ints/longs/etc. are ok */
  if (mips_isa < 3)
    {
      if (TARGET_INT64)
	fatal ("Only MIPS-III or MIPS-IV CPUs can support 64 bit ints");

      else if (TARGET_LONG64)
	fatal ("Only MIPS-III or MIPS-IV CPUs can support 64 bit longs");

      else if (TARGET_FLOAT64)
	fatal ("Only MIPS-III or MIPS-IV CPUs can support 64 bit fp registers");

      else if (TARGET_64BIT)
	fatal ("Only MIPS-III or MIPS-IV CPUs can support 64 bit gp registers");
    }

  if (mips_abi != ABI_32)
    flag_pcc_struct_return = 0;

  /* Tell halfpic.c that we have half-pic code if we do.  */
  if (TARGET_HALF_PIC)
    HALF_PIC_INIT ();

  /* -fpic (-KPIC) is the default when TARGET_ABICALLS is defined.  We need
     to set flag_pic so that the LEGITIMATE_PIC_OPERAND_P macro will work.  */
  /* ??? -non_shared turns off pic code generation, but this is not
     implemented.  */
  if (TARGET_ABICALLS)
    {
      mips_abicalls = MIPS_ABICALLS_YES;
      flag_pic = 1;
      if (mips_section_threshold > 0)
	warning ("-G is incompatible with PIC code which is the default");
    }
  else
    mips_abicalls = MIPS_ABICALLS_NO;

  /* -membedded-pic is a form of PIC code suitable for embedded
     systems.  All calls are made using PC relative addressing, and
     all data is addressed using the $gp register.  This requires gas,
     which does most of the work, and GNU ld, which automatically
     expands PC relative calls which are out of range into a longer
     instruction sequence.  All gcc really does differently is
     generate a different sequence for a switch.  */
  if (TARGET_EMBEDDED_PIC)
    {
      flag_pic = 1;
      if (TARGET_ABICALLS)
	warning ("-membedded-pic and -mabicalls are incompatible");
      if (g_switch_set)
	warning ("-G and -membedded-pic are incompatible");
      /* Setting mips_section_threshold is not required, because gas
	 will force everything to be GP addressable anyhow, but
	 setting it will cause gcc to make better estimates of the
	 number of instructions required to access a particular data
	 item.  */
      mips_section_threshold = 0x7fffffff;
    }

  /* This optimization requires a linker that can support a R_MIPS_LO16
     relocation which is not immediately preceded by a R_MIPS_HI16 relocation.
     GNU ld has this support, but not all other MIPS linkers do, so we enable
     this optimization only if the user requests it, or if GNU ld is the
     standard linker for this configuration.  */
  /* ??? This does not work when target addresses are DImode.
     This is because we are missing DImode high/lo_sum patterns.  */
  if (TARGET_GAS && TARGET_SPLIT_ADDRESSES && optimize && ! flag_pic
      && Pmode == SImode)
    mips_split_addresses = 1;
  else
    mips_split_addresses = 0;

  /* -mrnames says to use the MIPS software convention for register
     names instead of the hardware names (ie, $a0 instead of $4).
     We do this by switching the names in mips_reg_names, which the
     reg_names points into via the REGISTER_NAMES macro.  */

  if (TARGET_NAME_REGS)
    bcopy ((char *) mips_sw_reg_names, (char *) mips_reg_names, sizeof (mips_reg_names));

  /* If this is OSF/1, set up a SIGINFO handler so we can see what function
     is currently being compiled.  */
#ifdef SIGINFO
  if (getenv ("GCC_SIGINFO") != (char *)0)
    {
      struct sigaction action;
      action.sa_handler = siginfo;
      action.sa_mask = 0;
      action.sa_flags = SA_RESTART;
      sigaction (SIGINFO, &action, (struct sigaction *)0);
    }
#endif

#if defined(_IOLBF)
#if defined(ultrix) || defined(__ultrix) || defined(__OSF1__) || defined(__osf__) || defined(osf)
  /* If -mstats and -quiet, make stderr line buffered.  */
  if (quiet_flag && TARGET_STATS)
    setvbuf (stderr, (char *)0, _IOLBF, BUFSIZ);
#endif
#endif

  /* Initialize the high and low values for legitimate floating point
     constants.  Rather than trying to get the accuracy down to the
     last bit, just use approximate ranges.  */
  dfhigh = REAL_VALUE_ATOF ("1.0e300", DFmode);
  dflow = REAL_VALUE_ATOF ("1.0e-300", DFmode);
  sfhigh = REAL_VALUE_ATOF ("1.0e38", SFmode);
  sflow = REAL_VALUE_ATOF ("1.0e-38", SFmode);

  mips_print_operand_punct['?'] = TRUE;
  mips_print_operand_punct['#'] = TRUE;
  mips_print_operand_punct['&'] = TRUE;
  mips_print_operand_punct['!'] = TRUE;
  mips_print_operand_punct['*'] = TRUE;
  mips_print_operand_punct['@'] = TRUE;
  mips_print_operand_punct['.'] = TRUE;
  mips_print_operand_punct['('] = TRUE;
  mips_print_operand_punct[')'] = TRUE;
  mips_print_operand_punct['['] = TRUE;
  mips_print_operand_punct[']'] = TRUE;
  mips_print_operand_punct['<'] = TRUE;
  mips_print_operand_punct['>'] = TRUE;
  mips_print_operand_punct['{'] = TRUE;
  mips_print_operand_punct['}'] = TRUE;
  mips_print_operand_punct['^'] = TRUE;

  mips_char_to_class['d'] = GR_REGS;
  mips_char_to_class['f'] = ((TARGET_HARD_FLOAT) ? FP_REGS : NO_REGS);
  mips_char_to_class['h'] = HI_REG;
  mips_char_to_class['l'] = LO_REG;
  mips_char_to_class['a'] = HILO_REG;
  mips_char_to_class['x'] = MD_REGS;
  mips_char_to_class['b'] = ALL_REGS;
  mips_char_to_class['y'] = GR_REGS;
  mips_char_to_class['z'] = ST_REGS;

  /* Set up array to map GCC register number to debug register number.
     Ignore the special purpose register numbers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    mips_dbx_regno[i] = -1;

  start = GP_DBX_FIRST - GP_REG_FIRST;
  for (i = GP_REG_FIRST; i <= GP_REG_LAST; i++)
    mips_dbx_regno[i] = i + start;

  start = FP_DBX_FIRST - FP_REG_FIRST;
  for (i = FP_REG_FIRST; i <= FP_REG_LAST; i++)
    mips_dbx_regno[i] = i + start;

  /* Set up array giving whether a given register can hold a given mode.
     At present, restrict ints from being in FP registers, because reload
     is a little enthusiastic about storing extra values in FP registers,
     and this is not good for things like OS kernels.  Also, due to the
     mandatory delay, it is as fast to load from cached memory as to move
     from the FP register.  */

  for (mode = VOIDmode;
       mode != MAX_MACHINE_MODE;
       mode = (enum machine_mode)((int)mode + 1))
    {
      register int size		     = GET_MODE_SIZE (mode);
      register enum mode_class class = GET_MODE_CLASS (mode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  register int temp;

	  if (mode == CCmode)
	    {
	      if (mips_isa < 4)
		temp = (regno == FPSW_REGNUM);
	      else
		temp = (ST_REG_P (regno)
			|| GP_REG_P (regno)
			|| FP_REG_P (regno));
	    }

	  else if (GP_REG_P (regno))
	    temp = ((regno & 1) == 0 || (size <= UNITS_PER_WORD));

	  else if (FP_REG_P (regno))
	    temp = ((TARGET_FLOAT64 || ((regno & 1) == 0))
		    && (class == MODE_FLOAT
			|| class == MODE_COMPLEX_FLOAT
			|| (TARGET_DEBUG_H_MODE && class == MODE_INT))
		    && (! TARGET_SINGLE_FLOAT || size <= 4));

	  else if (MD_REG_P (regno))
	    temp = (class == MODE_INT
		    && (size <= UNITS_PER_WORD
			|| (regno == MD_REG_FIRST && size == 2 * UNITS_PER_WORD)));

	  else
	    temp = FALSE;

	  mips_hard_regno_mode_ok[(int)mode][regno] = temp;
	}
    }
}


/*
 * The MIPS debug format wants all automatic variables and arguments
 * to be in terms of the virtual frame pointer (stack pointer before
 * any adjustment in the function), while the MIPS 3.0 linker wants
 * the frame pointer to be the stack pointer after the initial
 * adjustment.  So, we do the adjustment here.  The arg pointer (which
 * is eliminated) points to the virtual frame pointer, while the frame
 * pointer (which may be eliminated) points to the stack pointer after
 * the initial adjustments.
 */

int
mips_debugger_offset (addr, offset)
     rtx addr;
     int offset;
{
  rtx offset2 = const0_rtx;
  rtx reg = eliminate_constant_term (addr, &offset2);

  if (!offset)
    offset = INTVAL (offset2);

  if (reg == stack_pointer_rtx || reg == frame_pointer_rtx)
    {
      int frame_size = (!current_frame_info.initialized)
				? compute_frame_size (get_frame_size ())
				: current_frame_info.total_size;

      offset = offset - frame_size;
    }
  /* sdbout_parms does not want this to crash for unrecognized cases.  */
#if 0
  else if (reg != arg_pointer_rtx)
    abort_with_insn (addr, "mips_debugger_offset called with non stack/frame/arg pointer.");
#endif

  return offset;
}


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the `%' specification that was used to request
   printing of the operand.  If the specification was just `%DIGIT'
   then CODE is 0; if the specification was `%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is
   `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   The MIPS specific codes are:

   'X'  X is CONST_INT, prints 32 bits in hexadecimal format = "0x%08x",
   'x'  X is CONST_INT, prints 16 bits in hexadecimal format = "0x%04x",
   'd'  output integer constant in decimal,
   'z'	if the operand is 0, use $0 instead of normal operand.
   'D'  print second register of double-word register operand.
   'L'  print low-order register of double-word register operand.
   'M'  print high-order register of double-word register operand.
   'C'  print part of opcode for a branch condition.
   'N'  print part of opcode for a branch condition, inverted.
   'S'  X is CODE_LABEL, print with prefix of "LS" (for embedded switch).
   'B'  print 'z' for EQ, 'n' for NE
   'b'  print 'n' for EQ, 'z' for NE
   'T'  print 'f' for EQ, 't' for NE
   't'  print 't' for EQ, 'f' for NE
   'Z'  print register and a comma, but print nothing for $fcc0
   '('	Turn on .set noreorder
   ')'	Turn on .set reorder
   '['	Turn on .set noat
   ']'	Turn on .set at
   '<'	Turn on .set nomacro
   '>'	Turn on .set macro
   '{'	Turn on .set volatile (not GAS)
   '}'	Turn on .set novolatile (not GAS)
   '&'	Turn on .set noreorder if filling delay slots
   '*'	Turn on both .set noreorder and .set nomacro if filling delay slots
   '!'	Turn on .set nomacro if filling delay slots
   '#'	Print nop if in a .set noreorder section.
   '?'	Print 'l' if we are to use a branch likely instead of normal branch.
   '@'	Print the name of the assembler temporary register (at or $1).
   '.'	Print the name of the register with a hard-wired zero (zero or $0).
   '^'	Print the name of the pic call-through register (t9 or $25).  */

void
print_operand (file, op, letter)
     FILE *file;		/* file to write to */
     rtx op;			/* operand to print */
     int letter;		/* %<letter> or 0 */
{
  register enum rtx_code code;

  if (PRINT_OPERAND_PUNCT_VALID_P (letter))
    {
      switch (letter)
	{
	default:
	  error ("PRINT_OPERAND: Unknown punctuation '%c'", letter);
	  break;

	case '?':
	  if (mips_branch_likely)
	    putc ('l', file);
	  break;

	case '@':
	  fputs (reg_names [GP_REG_FIRST + 1], file);
	  break;

	case '^':
	  fputs (reg_names [PIC_FUNCTION_ADDR_REGNUM], file);
	  break;

	case '.':
	  fputs (reg_names [GP_REG_FIRST + 0], file);
	  break;

	case '&':
	  if (final_sequence != 0 && set_noreorder++ == 0)
	    fputs (".set\tnoreorder\n\t", file);
	  break;

	case '*':
	  if (final_sequence != 0)
	    {
	      if (set_noreorder++ == 0)
		fputs (".set\tnoreorder\n\t", file);

	      if (set_nomacro++ == 0)
		fputs (".set\tnomacro\n\t", file);
	    }
	  break;

	case '!':
	  if (final_sequence != 0 && set_nomacro++ == 0)
	    fputs ("\n\t.set\tnomacro", file);
	  break;

	case '#':
	  if (set_noreorder != 0)
	    fputs ("\n\tnop", file);

	  else if (TARGET_STATS)
	    fputs ("\n\t#nop", file);

	  break;

	case '(':
	  if (set_noreorder++ == 0)
	    fputs (".set\tnoreorder\n\t", file);
	  break;

	case ')':
	  if (set_noreorder == 0)
	    error ("internal error: %%) found without a %%( in assembler pattern");

	  else if (--set_noreorder == 0)
	    fputs ("\n\t.set\treorder", file);

	  break;

	case '[':
	  if (set_noat++ == 0)
	    fputs (".set\tnoat\n\t", file);
	  break;

	case ']': 
	  if (set_noat == 0)
	    error ("internal error: %%] found without a %%[ in assembler pattern");

	  else if (--set_noat == 0)
	    fputs ("\n\t.set\tat", file);

	  break;

	case '<':
	  if (set_nomacro++ == 0)
	    fputs (".set\tnomacro\n\t", file);
	  break;

	case '>':
	  if (set_nomacro == 0)
	    error ("internal error: %%> found without a %%< in assembler pattern");

	  else if (--set_nomacro == 0)
	    fputs ("\n\t.set\tmacro", file);

	  break;

	case '{':
	  if (set_volatile++ == 0)
	    fprintf (file, "%s.set\tvolatile\n\t", (TARGET_MIPS_AS) ? "" : "#");
	  break;

	case '}':
	  if (set_volatile == 0)
	    error ("internal error: %%} found without a %%{ in assembler pattern");

	  else if (--set_volatile == 0)
	    fprintf (file, "\n\t%s.set\tnovolatile", (TARGET_MIPS_AS) ? "" : "#");

	  break;
	}
      return;
    }

  if (! op)
    {
      error ("PRINT_OPERAND null pointer");
      return;
    }

  code = GET_CODE (op);

  if (code == SIGN_EXTEND)
    {
      op = XEXP (op, 0);
      code = GET_CODE (op);
    }

  if (letter == 'C')
    switch (code)
      {
      case EQ:	fputs ("eq",  file); break;
      case NE:	fputs ("ne",  file); break;
      case GT:	fputs ("gt",  file); break;
      case GE:	fputs ("ge",  file); break;
      case LT:	fputs ("lt",  file); break;
      case LE:	fputs ("le",  file); break;
      case GTU: fputs ("gtu", file); break;
      case GEU: fputs ("geu", file); break;
      case LTU: fputs ("ltu", file); break;
      case LEU: fputs ("leu", file); break;

      default:
	abort_with_insn (op, "PRINT_OPERAND, invalid insn for %%C");
      }

  else if (letter == 'N')
    switch (code)
      {
      case EQ:	fputs ("ne",  file); break;
      case NE:	fputs ("eq",  file); break;
      case GT:	fputs ("le",  file); break;
      case GE:	fputs ("lt",  file); break;
      case LT:	fputs ("ge",  file); break;
      case LE:	fputs ("gt",  file); break;
      case GTU: fputs ("leu", file); break;
      case GEU: fputs ("ltu", file); break;
      case LTU: fputs ("geu", file); break;
      case LEU: fputs ("gtu", file); break;

      default:
	abort_with_insn (op, "PRINT_OPERAND, invalid insn for %%N");
      }

  else if (letter == 'S')
    {
      char buffer[100];

      ASM_GENERATE_INTERNAL_LABEL (buffer, "LS", CODE_LABEL_NUMBER (op));
      assemble_name (file, buffer);
    }

  else if (letter == 'Z')
    {
      register int regnum;

      if (code != REG)
	abort ();
      regnum = REGNO (op);
      if (! ST_REG_P (regnum))
	abort ();
      if (regnum != ST_REG_FIRST)
	fprintf (file, "%s,", reg_names[regnum]);
    }

  else if (code == REG || code == SUBREG)
    {
      register int regnum;

      if (code == REG)
	regnum = REGNO (op);
      else
	regnum = true_regnum (op);

      if ((letter == 'M' && ! WORDS_BIG_ENDIAN)
	  || (letter == 'L' && WORDS_BIG_ENDIAN)
	  || letter == 'D')
	regnum++;

      fprintf (file, "%s", reg_names[regnum]);
    }

  else if (code == MEM)
    output_address (XEXP (op, 0));

  else if (code == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT)
    {
      REAL_VALUE_TYPE d;
      char s[30];

      REAL_VALUE_FROM_CONST_DOUBLE (d, op);
      REAL_VALUE_TO_DECIMAL (d, "%.20e", s);
      fprintf (file, s);
    }

  else if ((letter == 'x') && (GET_CODE(op) == CONST_INT))
    fprintf (file, "0x%04x", 0xffff & (INTVAL(op)));

  else if ((letter == 'X') && (GET_CODE(op) == CONST_INT)
	   && HOST_BITS_PER_WIDE_INT == 32)
    fprintf (file, "0x%08x", INTVAL(op));

  else if ((letter == 'X') && (GET_CODE(op) == CONST_INT)
	   && HOST_BITS_PER_WIDE_INT == 64)
    fprintf (file, "0x%016lx", INTVAL(op));

  else if ((letter == 'd') && (GET_CODE(op) == CONST_INT))
    fprintf (file, "%d", (INTVAL(op)));

  else if (letter == 'z'
	   && (GET_CODE (op) == CONST_INT)
	   && INTVAL (op) == 0)
    fputs (reg_names[GP_REG_FIRST], file);

  else if (letter == 'd' || letter == 'x' || letter == 'X')
    fatal ("PRINT_OPERAND: letter %c was found & insn was not CONST_INT", letter);

  else if (letter == 'B')
    fputs (code == EQ ? "z" : "n", file);
  else if (letter == 'b')
    fputs (code == EQ ? "n" : "z", file);
  else if (letter == 'T')
    fputs (code == EQ ? "f" : "t", file);
  else if (letter == 't')
    fputs (code == EQ ? "t" : "f", file);

  else
    output_addr_const (file, op);
}


/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  if (!addr)
    error ("PRINT_OPERAND_ADDRESS, null pointer");

  else
    switch (GET_CODE (addr))
      {
      default:
	abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #1");
	break;

      case REG:
	if (REGNO (addr) == ARG_POINTER_REGNUM)
	  abort_with_insn (addr, "Arg pointer not eliminated.");

	fprintf (file, "0(%s)", reg_names [REGNO (addr)]);
	break;

      case LO_SUM:
	{
	  register rtx arg0   = XEXP (addr, 0);
	  register rtx arg1   = XEXP (addr, 1);

	  if (! mips_split_addresses)
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, Spurious LO_SUM.");

	  if (GET_CODE (arg0) != REG)
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, LO_SUM with #1 not REG.");

	  fprintf (file, "%%lo(");
	  print_operand_address (file, arg1);
	  fprintf (file, ")(%s)", reg_names [REGNO (arg0)]);
	}
	break;

      case PLUS:
	{
	  register rtx reg    = (rtx)0;
	  register rtx offset = (rtx)0;
	  register rtx arg0   = XEXP (addr, 0);
	  register rtx arg1   = XEXP (addr, 1);

	  if (GET_CODE (arg0) == REG)
	    {
	      reg = arg0;
	      offset = arg1;
	      if (GET_CODE (offset) == REG)
		abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, 2 regs");
	    }
	  else if (GET_CODE (arg1) == REG)
	    {
	      reg = arg1;
	      offset = arg0;
	    }
	  else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))
	    {
	      output_addr_const (file, addr);
	      break;
	    }
	  else
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, no regs");

	  if (!CONSTANT_P (offset))
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #2");

	  if (REGNO (reg) == ARG_POINTER_REGNUM)
	    abort_with_insn (addr, "Arg pointer not eliminated.");

	  output_addr_const (file, offset);
	  fprintf (file, "(%s)", reg_names [REGNO (reg)]);
	}
	break;

      case LABEL_REF:
      case SYMBOL_REF:
      case CONST_INT:
      case CONST:
	output_addr_const (file, addr);
	break;
    }
}


/* If optimizing for the global pointer, keep track of all of
   the externs, so that at the end of the file, we can emit
   the appropriate .extern declaration for them, before writing
   out the text section.  We assume that all names passed to
   us are in the permanent obstack, so that they will be valid
   at the end of the compilation.

   If we have -G 0, or the extern size is unknown, or the object is in
   a user specified section that is not .sbss/.sdata, don't bother
   emitting the .externs.  In the case of user specified sections this
   behaviour is required as otherwise GAS will think the object lives in
   .sbss/.sdata.  */

int
mips_output_external (file, decl, name)
     FILE *file;
     tree decl;
     char *name;
{
  register struct extern_list *p;
  int len;
  tree section_name;

  if (TARGET_GP_OPT
      && ((TREE_CODE (decl)) != FUNCTION_DECL)
      && ((len = int_size_in_bytes (TREE_TYPE (decl))) > 0)
      && (((section_name = DECL_SECTION_NAME (decl)) == NULL)
	  || strcmp (TREE_STRING_POINTER (section_name), ".sbss") == 0
	  || strcmp (TREE_STRING_POINTER (section_name), ".sdata") == 0))
    {
      p = (struct extern_list *)permalloc ((long) sizeof (struct extern_list));
      p->next = extern_head;
      p->name = name;
      p->size = len;
      extern_head = p;
    }

#ifdef ASM_OUTPUT_UNDEF_FUNCTION
  if (TREE_CODE (decl) == FUNCTION_DECL
      /* ??? Don't include alloca, since gcc will always expand it
	 inline.  If we don't do this, the C++ library fails to build.  */
      && strcmp (name, "alloca")
      /* ??? Don't include __builtin_next_arg, because then gcc will not
	 bootstrap under Irix 5.1.  */
      && strcmp (name, "__builtin_next_arg"))
    {
      p = (struct extern_list *)permalloc ((long) sizeof (struct extern_list));
      p->next = extern_head;
      p->name = name;
      p->size = -1;
      extern_head = p;
    }
#endif

  return 0;
}

#ifdef ASM_OUTPUT_UNDEF_FUNCTION
int
mips_output_external_libcall (file, name)
     FILE *file;
     char *name;
{
  register struct extern_list *p;

  p = (struct extern_list *)permalloc ((long) sizeof (struct extern_list));
  p->next = extern_head;
  p->name = name;
  p->size = -1;
  extern_head = p;

  return 0;
}
#endif


/* Compute a string to use as a temporary file name.  */

/* On MSDOS, write temp files in current dir
   because there's no place else we can expect to use.  */
#if __MSDOS__
#ifndef P_tmpdir
#define P_tmpdir "./"
#endif
#endif

static FILE *
make_temp_file ()
{
  FILE *stream;
  char *base = getenv ("TMPDIR");
  int len;

  if (base == (char *)0)
    {
#ifdef P_tmpdir
      if (access (P_tmpdir, R_OK | W_OK) == 0)
	base = P_tmpdir;
      else
#endif
	if (access ("/usr/tmp", R_OK | W_OK) == 0)
	  base = "/usr/tmp/";
	else
	  base = "/tmp/";
    }

  len = strlen (base);
  /* temp_filename is global, so we must use malloc, not alloca.  */
  temp_filename = (char *) xmalloc (len + sizeof("/ctXXXXXX"));
  strcpy (temp_filename, base);
  if (len > 0 && temp_filename[len-1] != '/')
    temp_filename[len++] = '/';

  strcpy (temp_filename + len, "ctXXXXXX");
  mktemp (temp_filename);

  stream = fopen (temp_filename, "w+");
  if (!stream)
    pfatal_with_name (temp_filename);

#ifndef __MSDOS__
  /* In MSDOS, we cannot unlink the temporary file until we are finished using
     it.  Otherwise, we delete it now, so that it will be gone even if the
     compiler happens to crash.  */
  unlink (temp_filename);
#endif
  return stream;
}


/* Emit a new filename to a stream.  If this is MIPS ECOFF, watch out
   for .file's that start within a function.  If we are smuggling stabs, try to
   put out a MIPS ECOFF file and a stab.  */

void
mips_output_filename (stream, name)
     FILE *stream;
     char *name;
{
  static int first_time = TRUE;
  char ltext_label_name[100];

  if (first_time)
    {
      first_time = FALSE;
      SET_FILE_NUMBER ();
      current_function_file = name;
      ASM_OUTPUT_FILENAME (stream, num_source_filenames, name);
      /* This tells mips-tfile that stabs will follow.  */
      if (!TARGET_GAS && write_symbols == DBX_DEBUG)
	fprintf (stream, "\t#@stabs\n");
    }

  else if (write_symbols == DBX_DEBUG)
    {
      ASM_GENERATE_INTERNAL_LABEL (ltext_label_name, "Ltext", 0);
      fprintf (stream, "%s ", ASM_STABS_OP);
      output_quoted_string (stream, name);
      fprintf (stream, ",%d,0,0,%s\n", N_SOL, &ltext_label_name[1]);
    }

  else if (name != current_function_file
      && strcmp (name, current_function_file) != 0)
    {
      if (inside_function && !TARGET_GAS)
	{
	  if (!file_in_function_warning)
	    {
	      file_in_function_warning = TRUE;
	      ignore_line_number = TRUE;
	      warning ("MIPS ECOFF format does not allow changing filenames within functions with #line");
	    }
	}
      else
	{
	  SET_FILE_NUMBER ();
	  current_function_file = name;
	  ASM_OUTPUT_FILENAME (stream, num_source_filenames, name);
	}
    }
}


/* Emit a linenumber.  For encapsulated stabs, we need to put out a stab
   as well as a .loc, since it is possible that MIPS ECOFF might not be
   able to represent the location for inlines that come from a different
   file.  */

void
mips_output_lineno (stream, line)
     FILE *stream;
     int line;
{
  if (write_symbols == DBX_DEBUG)
    {
      ++sym_lineno;
      fprintf (stream, "%sLM%d:\n\t%s %d,0,%d,%sLM%d\n",
	       LOCAL_LABEL_PREFIX, sym_lineno, ASM_STABN_OP, N_SLINE, line,
	       LOCAL_LABEL_PREFIX, sym_lineno);
    }

  else
    {
      fprintf (stream, "\n\t%s.loc\t%d %d\n",
	       (ignore_line_number) ? "#" : "",
	       num_source_filenames, line);
  
      LABEL_AFTER_LOC (stream);
    }
}


/* If defined, a C statement to be executed just prior to the
   output of assembler code for INSN, to modify the extracted
   operands so they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.  The
   contents of this vector are what will be used to convert the
   insn template into assembler code, so you can change the
   assembler output by changing the contents of the vector.

   We use it to check if the current insn needs a nop in front of it
   because of load delays, and also to update the delay slot
   statistics.  */

/* ??? There is no real need for this function, because it never actually
   emits a NOP anymore.  */

void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx opvec[];
     int noperands;
{
  if (dslots_number_nops > 0)
    {
      rtx pattern = PATTERN (insn);
      int length = get_attr_length (insn);

      /* Do we need to emit a NOP? */
      if (length == 0
	  || (mips_load_reg  != (rtx)0 && reg_mentioned_p (mips_load_reg,  pattern))
	  || (mips_load_reg2 != (rtx)0 && reg_mentioned_p (mips_load_reg2, pattern))
	  || (mips_load_reg3 != (rtx)0 && reg_mentioned_p (mips_load_reg3, pattern))
	  || (mips_load_reg4 != (rtx)0 && reg_mentioned_p (mips_load_reg4, pattern)))
	fputs ("\t#nop\n", asm_out_file);

      else
	dslots_load_filled++;

      while (--dslots_number_nops > 0)
	fputs ("\t#nop\n", asm_out_file);

      mips_load_reg  = (rtx)0;
      mips_load_reg2 = (rtx)0;
      mips_load_reg3 = (rtx)0;
      mips_load_reg4 = (rtx)0;
    }

  if (TARGET_STATS)
    {
      enum rtx_code code = GET_CODE (insn);
      if (code == JUMP_INSN || code == CALL_INSN)
	dslots_jump_total++;
    }
}


/* Output at beginning of assembler file.
   If we are optimizing to use the global pointer, create a temporary
   file to hold all of the text stuff, and write it out to the end.
   This is needed because the MIPS assembler is evidently one pass,
   and if it hasn't seen the relevant .comm/.lcomm/.extern/.sdata
   declaration when the code is processed, it generates a two
   instruction sequence.  */

void
mips_asm_file_start (stream)
     FILE *stream;
{
  ASM_OUTPUT_SOURCE_FILENAME (stream, main_input_filename);

  /* Versions of the MIPS assembler before 2.20 generate errors
     if a branch inside of a .set noreorder section jumps to a
     label outside of the .set noreorder section.  Revision 2.20
     just set nobopt silently rather than fixing the bug.  */

  if (TARGET_MIPS_AS && optimize && flag_delayed_branch)
    fprintf (stream, "\t.set\tnobopt\n");

  /* Generate the pseudo ops that System V.4 wants.  */
#ifndef ABICALLS_ASM_OP
#define ABICALLS_ASM_OP ".abicalls"
#endif
  if (TARGET_ABICALLS)
    /* ??? but do not want this (or want pic0) if -non-shared? */
    fprintf (stream, "\t%s\n", ABICALLS_ASM_OP);

  /* Start a section, so that the first .popsection directive is guaranteed
     to have a previously defined section to pop back to.  */
  if (mips_abi != ABI_32 && mips_abi != ABI_EABI)
    fprintf (stream, "\t.section\t.text\n");

  /* This code exists so that we can put all externs before all symbol
     references.  This is necessary for the MIPS assembler's global pointer
     optimizations to work.  */
  if (TARGET_FILE_SWITCHING)
    {
      asm_out_data_file = stream;
      asm_out_text_file = make_temp_file ();
    }
  else
    asm_out_data_file = asm_out_text_file = stream;

  if (flag_verbose_asm)
    fprintf (stream, "\n%s -G value = %d, Cpu = %s, ISA = %d\n",
	     ASM_COMMENT_START,
	     mips_section_threshold, mips_cpu_string, mips_isa);
}


/* If we are optimizing the global pointer, emit the text section now
   and any small externs which did not have .comm, etc that are
   needed.  Also, give a warning if the data area is more than 32K and
   -pic because 3 instructions are needed to reference the data
   pointers.  */

void
mips_asm_file_end (file)
     FILE *file;
{
  char buffer[8192];
  tree name_tree;
  struct extern_list *p;
  int len;

  if (HALF_PIC_P ())
    HALF_PIC_FINISH (file);

  if (extern_head)
    {
      fputs ("\n", file);

      for (p = extern_head; p != 0; p = p->next)
	{
	  name_tree = get_identifier (p->name);

	  /* Positively ensure only one .extern for any given symbol.  */
	  if (! TREE_ASM_WRITTEN (name_tree))
	    {
	      TREE_ASM_WRITTEN (name_tree) = 1;
#ifdef ASM_OUTPUT_UNDEF_FUNCTION
	      if (p->size == -1)
		ASM_OUTPUT_UNDEF_FUNCTION (file, p->name);
	      else
#endif
		{
		  fputs ("\t.extern\t", file);
		  assemble_name (file, p->name);
		  fprintf (file, ", %d\n", p->size);
		}
	    }
	}
    }
      
  if (TARGET_FILE_SWITCHING)
    {
      fprintf (file, "\n\t.text\n");
      rewind (asm_out_text_file);
      if (ferror (asm_out_text_file))
	fatal_io_error (temp_filename);

      while ((len = fread (buffer, 1, sizeof (buffer), asm_out_text_file)) > 0)
	if (fwrite (buffer, 1, len, file) != len)
	  pfatal_with_name (asm_file_name);

      if (len < 0)
	pfatal_with_name (temp_filename);

      if (fclose (asm_out_text_file) != 0)
	pfatal_with_name (temp_filename);

#ifdef __MSDOS__
      unlink (temp_filename);
#endif
    }
}


/* Emit either a label, .comm, or .lcomm directive, and mark
   that the symbol is used, so that we don't emit an .extern
   for it in mips_asm_file_end.  */

void
mips_declare_object (stream, name, init_string, final_string, size)
     FILE *stream;
     char *name;
     char *init_string;
     char *final_string;
     int size;
{
  fputs (init_string, stream);		/* "", "\t.comm\t", or "\t.lcomm\t" */
  assemble_name (stream, name);
  fprintf (stream, final_string, size);	/* ":\n", ",%u\n", ",%u\n" */

  if (TARGET_GP_OPT)
    {
      tree name_tree = get_identifier (name);
      TREE_ASM_WRITTEN (name_tree) = 1;
    }
}


/* Output a double precision value to the assembler.  If both the
   host and target are IEEE, emit the values in hex.  */

void
mips_output_double (stream, value)
     FILE *stream;
     REAL_VALUE_TYPE value;
{
#ifdef REAL_VALUE_TO_TARGET_DOUBLE
  long value_long[2];
  REAL_VALUE_TO_TARGET_DOUBLE (value, value_long);

  fprintf (stream, "\t.word\t0x%08lx\t\t# %.20g\n\t.word\t0x%08lx\n",
	   value_long[0], value, value_long[1]);
#else
  fprintf (stream, "\t.double\t%.20g\n", value);
#endif
}


/* Output a single precision value to the assembler.  If both the
   host and target are IEEE, emit the values in hex.  */

void
mips_output_float (stream, value)
     FILE *stream;
     REAL_VALUE_TYPE value;
{
#ifdef REAL_VALUE_TO_TARGET_SINGLE
  long value_long;
  REAL_VALUE_TO_TARGET_SINGLE (value, value_long);

  fprintf (stream, "\t.word\t0x%08lx\t\t# %.12g (float)\n", value_long, value);
#else
  fprintf (stream, "\t.float\t%.12g\n", value);
#endif
}


/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   Mips stack frames look like:

             Before call		        After call
        +-----------------------+	+-----------------------+
   high |			|       |      			|
   mem. |		        |	|			|
        |  caller's temps.    	|       |  caller's temps.    	|
	|       		|       |       	        |
        +-----------------------+	+-----------------------+
 	|       		|	|		        |
        |  arguments on stack.  |	|  arguments on stack.  |
	|       		|	|			|
        +-----------------------+	+-----------------------+
 	|  4 words to save     	|	|  4 words to save	|
	|  arguments passed	|	|  arguments passed	|
	|  in registers, even	|	|  in registers, even	|
    SP->|  if not passed.       |  VFP->|  if not passed.	|
	+-----------------------+       +-----------------------+
					|		        |
                                        |  fp register save     |
					|			|
					+-----------------------+
					|		        |
                                        |  gp register save     |
                                        |       		|
					+-----------------------+
					|			|
					|  local variables	|
					|			|
					+-----------------------+
					|			|
                                        |  alloca allocations   |
        				|			|
					+-----------------------+
					|			|
					|  GP save for V.4 abi	|
					|			|
					+-----------------------+
					|			|
                                        |  arguments on stack   |
        				|		        |
					+-----------------------+
                                        |  4 words to save      |
					|  arguments passed     |
                                        |  in registers, even   |
   low                              SP->|  if not passed.       |
   memory        			+-----------------------+

*/

long
compute_frame_size (size)
     int size;			/* # of var. bytes allocated */
{
  int regno;
  long total_size;		/* # bytes that the entire frame takes up */
  long var_size;		/* # bytes that variables take up */
  long args_size;		/* # bytes that outgoing arguments take up */
  long extra_size;		/* # extra bytes */
  long gp_reg_rounded;		/* # bytes needed to store gp after rounding */
  long gp_reg_size;		/* # bytes needed to store gp regs */
  long fp_reg_size;		/* # bytes needed to store fp regs */
  long mask;			/* mask of saved gp registers */
  long fmask;			/* mask of saved fp registers */
  int  fp_inc;			/* 1 or 2 depending on the size of fp regs */
  long fp_bits;			/* bitmask to use for each fp register */

  gp_reg_size	 = 0;
  fp_reg_size	 = 0;
  mask		 = 0;
  fmask		 = 0;
  extra_size	 = MIPS_STACK_ALIGN (((TARGET_ABICALLS) ? UNITS_PER_WORD : 0));
  var_size	 = MIPS_STACK_ALIGN (size);
  args_size	 = MIPS_STACK_ALIGN (current_function_outgoing_args_size);

  /* The MIPS 3.0 linker does not like functions that dynamically
     allocate the stack and have 0 for STACK_DYNAMIC_OFFSET, since it
     looks like we are trying to create a second frame pointer to the
     function, so allocate some stack space to make it happy.  */

  if (args_size == 0 && current_function_calls_alloca)
    args_size = 4*UNITS_PER_WORD;

  total_size = var_size + args_size + extra_size;

  /* Calculate space needed for gp registers.  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      if (MUST_SAVE_REGISTER (regno))
	{
	  gp_reg_size += UNITS_PER_WORD;
	  mask |= 1L << (regno - GP_REG_FIRST);
	}
    }

  /* Calculate space needed for fp registers.  */
  if (TARGET_FLOAT64 || TARGET_SINGLE_FLOAT)
    {
      fp_inc = 1;
      fp_bits = 1;
    }
  else
    {
      fp_inc = 2;
      fp_bits = 3;
    }

  for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno += fp_inc)
    {
      if (regs_ever_live[regno] && !call_used_regs[regno])
	{
	  fp_reg_size += fp_inc * UNITS_PER_FPREG;
	  fmask |= fp_bits << (regno - FP_REG_FIRST);
	}
    }

  gp_reg_rounded = MIPS_STACK_ALIGN (gp_reg_size);
  total_size += gp_reg_rounded + MIPS_STACK_ALIGN (fp_reg_size);

  /* The gp reg is caller saved in the 32 bit ABI, so there is no need
     for leaf routines (total_size == extra_size) to save the gp reg.
     The gp reg is callee saved in the 64 bit ABI, so all routines must
     save the gp reg.  This is not a leaf routine if -p, because of the
     call to mcount.  */
  if (total_size == extra_size && (mips_abi == ABI_32 || mips_abi == ABI_EABI)
      && ! profile_flag)
    total_size = extra_size = 0;
  else if (TARGET_ABICALLS)
    {
      /* Add the context-pointer to the saved registers.  */
      gp_reg_size += UNITS_PER_WORD;
      mask |= 1L << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST);
      total_size -= gp_reg_rounded;
      gp_reg_rounded = MIPS_STACK_ALIGN (gp_reg_size);
      total_size += gp_reg_rounded;
    }

  /* Add in space reserved on the stack by the callee for storing arguments
     passed in registers.  */
  if (mips_abi != ABI_32)
    total_size += MIPS_STACK_ALIGN (current_function_pretend_args_size);

  /* Save other computed information.  */
  current_frame_info.total_size  = total_size;
  current_frame_info.var_size    = var_size;
  current_frame_info.args_size   = args_size;
  current_frame_info.extra_size  = extra_size;
  current_frame_info.gp_reg_size = gp_reg_size;
  current_frame_info.fp_reg_size = fp_reg_size;
  current_frame_info.mask	 = mask;
  current_frame_info.fmask	 = fmask;
  current_frame_info.initialized = reload_completed;
  current_frame_info.num_gp	 = gp_reg_size / UNITS_PER_WORD;
  current_frame_info.num_fp	 = fp_reg_size / (fp_inc * UNITS_PER_FPREG);

  if (mask)
    {
      unsigned long offset = (args_size + extra_size + var_size
			      + gp_reg_size - UNITS_PER_WORD);
      current_frame_info.gp_sp_offset = offset;
      current_frame_info.gp_save_offset = offset - total_size;
    }
  else
    {
      current_frame_info.gp_sp_offset = 0;
      current_frame_info.gp_save_offset = 0;
    }


  if (fmask)
    {
      unsigned long offset = (args_size + extra_size + var_size
			      + gp_reg_rounded + fp_reg_size
			      - fp_inc * UNITS_PER_FPREG);
      current_frame_info.fp_sp_offset = offset;
      current_frame_info.fp_save_offset = offset - total_size;
    }
  else
    {
      current_frame_info.fp_sp_offset = 0;
      current_frame_info.fp_save_offset = 0;
    }

  /* Ok, we're done.  */
  return total_size;
}


/* Common code to emit the insns (or to write the instructions to a file)
   to save/restore registers.

   Other parts of the code assume that MIPS_TEMP1_REGNUM (aka large_reg)
   is not modified within save_restore_insns.  */

#define BITSET_P(value,bit) (((value) & (1L << (bit))) != 0)

static void
save_restore_insns (store_p, large_reg, large_offset, file)
     int store_p;		/* true if this is prologue */
     rtx large_reg;		/* register holding large offset constant or NULL */
     long large_offset;		/* large constant offset value */
     FILE *file;		/* file to write instructions to instead of making RTL */
{
  long mask		= current_frame_info.mask;
  long fmask		= current_frame_info.fmask;
  int regno;
  rtx base_reg_rtx;
  long base_offset;
  long gp_offset;
  long fp_offset;
  long end_offset;
  rtx insn;

  if (frame_pointer_needed && !BITSET_P (mask, FRAME_POINTER_REGNUM - GP_REG_FIRST))
    abort ();

  if (mask == 0 && fmask == 0)
    return;

  /* Save registers starting from high to low.  The debuggers prefer
     at least the return register be stored at func+4, and also it
     allows us not to need a nop in the epilog if at least one
     register is reloaded in addition to return address.  */

  /* Save GP registers if needed.  */
  if (mask)
    {
      /* Pick which pointer to use as a base register.  For small
	 frames, just use the stack pointer.  Otherwise, use a
	 temporary register.  Save 2 cycles if the save area is near
	 the end of a large frame, by reusing the constant created in
	 the prologue/epilogue to adjust the stack frame.  */

      gp_offset  = current_frame_info.gp_sp_offset;
      end_offset = gp_offset - (current_frame_info.gp_reg_size - UNITS_PER_WORD);

      if (gp_offset < 0 || end_offset < 0)
	fatal ("gp_offset (%ld) or end_offset (%ld) is less than zero.",
	       gp_offset, end_offset);

      else if (gp_offset < 32768)
	{
	  base_reg_rtx = stack_pointer_rtx;
	  base_offset  = 0;
	}

      else if (large_reg != (rtx)0
	       && (((unsigned long)(large_offset - gp_offset))  < 32768)
	       && (((unsigned long)(large_offset - end_offset)) < 32768))
	{
	  base_reg_rtx = gen_rtx (REG, Pmode, MIPS_TEMP2_REGNUM);
	  base_offset  = large_offset;
	  if (file == (FILE *)0)
	    {
	      if (TARGET_LONG64)
		insn = emit_insn (gen_adddi3 (base_reg_rtx, large_reg, stack_pointer_rtx));
	      else
		insn = emit_insn (gen_addsi3 (base_reg_rtx, large_reg, stack_pointer_rtx));
	      if (store_p)
		RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    fprintf (file, "\t%s\t%s,%s,%s\n",
		     TARGET_LONG64 ? "daddu" : "addu",
		     reg_names[MIPS_TEMP2_REGNUM],
		     reg_names[REGNO (large_reg)],
		     reg_names[STACK_POINTER_REGNUM]);
	}

      else
	{
	  base_reg_rtx = gen_rtx (REG, Pmode, MIPS_TEMP2_REGNUM);
	  base_offset  = gp_offset;
	  if (file == (FILE *)0)
	    {
	      rtx gp_offset_rtx = GEN_INT (gp_offset);

	      /* Instruction splitting doesn't preserve the RTX_FRAME_RELATED_P
		 bit, so make sure that we don't emit anything that can be
		 split.  */
	      /* ??? There is no DImode ori immediate pattern, so we can only
		 do this for 32 bit code.  */
	      if (large_int (gp_offset_rtx)
		  && GET_MODE (base_reg_rtx) == SImode)
		{
		  insn = emit_move_insn (base_reg_rtx,
					 GEN_INT (gp_offset & 0xffff0000));
		  if (store_p)
		    RTX_FRAME_RELATED_P (insn) = 1;
		  insn = emit_insn (gen_iorsi3 (base_reg_rtx, base_reg_rtx,
						GEN_INT (gp_offset & 0x0000ffff)));
		  if (store_p)
		    RTX_FRAME_RELATED_P (insn) = 1;
		}
	      else
		{
		  insn = emit_move_insn (base_reg_rtx, gp_offset_rtx);
		  if (store_p)
		    RTX_FRAME_RELATED_P (insn) = 1;
		}

	      if (TARGET_LONG64)
		insn = emit_insn (gen_adddi3 (base_reg_rtx, base_reg_rtx, stack_pointer_rtx));
	      else
		insn = emit_insn (gen_addsi3 (base_reg_rtx, base_reg_rtx, stack_pointer_rtx));
	      if (store_p)
		RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    fprintf (file, "\tli\t%s,0x%.08lx\t# %ld\n\t%s\t%s,%s,%s\n",
		     reg_names[MIPS_TEMP2_REGNUM],
		     (long)base_offset,
		     (long)base_offset,
		     TARGET_LONG64 ? "daddu" : "addu",
		     reg_names[MIPS_TEMP2_REGNUM],
		     reg_names[MIPS_TEMP2_REGNUM],
		     reg_names[STACK_POINTER_REGNUM]);
	}

      for  (regno = GP_REG_LAST; regno >= GP_REG_FIRST; regno--)
	{
	  if (BITSET_P (mask, regno - GP_REG_FIRST))
	    {
	      if (file == (FILE *)0)
		{
		  rtx reg_rtx = gen_rtx (REG, word_mode, regno);
		  rtx mem_rtx = gen_rtx (MEM, word_mode,
					 gen_rtx (PLUS, Pmode, base_reg_rtx,
						  GEN_INT (gp_offset - base_offset)));

		  if (store_p)
		    {
		      insn = emit_move_insn (mem_rtx, reg_rtx);
		      RTX_FRAME_RELATED_P (insn) = 1;
		    }
		  else if (!TARGET_ABICALLS || mips_abi != ABI_32
			   || regno != (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))
		    emit_move_insn (reg_rtx, mem_rtx);
		}
	      else
		{
		  if (store_p || !TARGET_ABICALLS || mips_abi != ABI_32
		      || regno != (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))
		    fprintf (file, "\t%s\t%s,%ld(%s)\n",
			     (TARGET_64BIT
			      ? (store_p) ? "sd" : "ld"
			      : (store_p) ? "sw" : "lw"),
			     reg_names[regno],
			     gp_offset - base_offset,
			     reg_names[REGNO(base_reg_rtx)]);

		}
	      gp_offset -= UNITS_PER_WORD;
	    }
	}
    }
  else
    {
      base_reg_rtx = (rtx)0;		/* Make sure these are initialized */
      base_offset  = 0;
    }

  /* Save floating point registers if needed.  */
  if (fmask)
    {
      int fp_inc = (TARGET_FLOAT64 || TARGET_SINGLE_FLOAT) ? 1 : 2;
      int fp_size = fp_inc * UNITS_PER_FPREG;

      /* Pick which pointer to use as a base register.  */
      fp_offset  = current_frame_info.fp_sp_offset;
      end_offset = fp_offset - (current_frame_info.fp_reg_size - fp_size);

      if (fp_offset < 0 || end_offset < 0)
	fatal ("fp_offset (%ld) or end_offset (%ld) is less than zero.",
	       fp_offset, end_offset);

      else if (fp_offset < 32768)
	{
	  base_reg_rtx = stack_pointer_rtx;
	  base_offset  = 0;
	}

      else if (base_reg_rtx != (rtx)0
	       && (((unsigned long)(base_offset - fp_offset))  < 32768)
	       && (((unsigned long)(base_offset - end_offset)) < 32768))
	{
	  ;			/* already set up for gp registers above */
	}

      else if (large_reg != (rtx)0
	       && (((unsigned long)(large_offset - fp_offset))  < 32768)
	       && (((unsigned long)(large_offset - end_offset)) < 32768))
	{
	  base_reg_rtx = gen_rtx (REG, Pmode, MIPS_TEMP2_REGNUM);
	  base_offset  = large_offset;
	  if (file == (FILE *)0)
	    {
	      if (TARGET_LONG64)
		insn = emit_insn (gen_adddi3 (base_reg_rtx, large_reg, stack_pointer_rtx));
	      else
		insn = emit_insn (gen_addsi3 (base_reg_rtx, large_reg, stack_pointer_rtx));
	      if (store_p)
		RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    fprintf (file, "\t%s\t%s,%s,%s\n",
		     TARGET_LONG64 ? "daddu" : "addu",
		     reg_names[MIPS_TEMP2_REGNUM],
		     reg_names[REGNO (large_reg)],
		     reg_names[STACK_POINTER_REGNUM]);
	}

      else
	{
	  base_reg_rtx = gen_rtx (REG, Pmode, MIPS_TEMP2_REGNUM);
	  base_offset  = fp_offset;
	  if (file == (FILE *)0)
	    {
	      rtx fp_offset_rtx = GEN_INT (fp_offset);

	      /* Instruction splitting doesn't preserve the RTX_FRAME_RELATED_P
		 bit, so make sure that we don't emit anything that can be
		 split.  */
	      /* ??? There is no DImode ori immediate pattern, so we can only
		 do this for 32 bit code.  */
	      if (large_int (fp_offset_rtx)
		  && GET_MODE (base_reg_rtx) == SImode)
		{
		  insn = emit_move_insn (base_reg_rtx,
					 GEN_INT (fp_offset & 0xffff0000));
		  if (store_p)
		    RTX_FRAME_RELATED_P (insn) = 1;
		  insn = emit_insn (gen_iorsi3 (base_reg_rtx, base_reg_rtx,
						GEN_INT (fp_offset & 0x0000ffff)));
		  if (store_p)
		    RTX_FRAME_RELATED_P (insn) = 1;
		}
	      else
		{
		  insn = emit_move_insn (base_reg_rtx, fp_offset_rtx);
		  if (store_p)
		    RTX_FRAME_RELATED_P (insn) = 1;
		}

	      if (store_p)
		RTX_FRAME_RELATED_P (insn) = 1;
	      if (TARGET_LONG64)
		insn = emit_insn (gen_adddi3 (base_reg_rtx, base_reg_rtx, stack_pointer_rtx));
	      else
		insn = emit_insn (gen_addsi3 (base_reg_rtx, base_reg_rtx, stack_pointer_rtx));
	      if (store_p)
		RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    fprintf (file, "\tli\t%s,0x%.08lx\t# %ld\n\t%s\t%s,%s,%s\n",
		     reg_names[MIPS_TEMP2_REGNUM],
		     (long)base_offset,
		     (long)base_offset,
		     TARGET_LONG64 ? "daddu" : "addu",
		     reg_names[MIPS_TEMP2_REGNUM],
		     reg_names[MIPS_TEMP2_REGNUM],
		     reg_names[STACK_POINTER_REGNUM]);
	}

      for  (regno = FP_REG_LAST-1; regno >= FP_REG_FIRST; regno -= fp_inc)
	{
	  if (BITSET_P (fmask, regno - FP_REG_FIRST))
	    {
	      if (file == (FILE *)0)
		{
		  enum machine_mode sz =
		    TARGET_SINGLE_FLOAT ? SFmode : DFmode;
		  rtx reg_rtx = gen_rtx (REG, sz, regno);
		  rtx mem_rtx = gen_rtx (MEM, sz,
					 gen_rtx (PLUS, Pmode, base_reg_rtx,
						  GEN_INT (fp_offset - base_offset)));

		  if (store_p)
		    {
		      insn = emit_move_insn (mem_rtx, reg_rtx);
		      RTX_FRAME_RELATED_P (insn) = 1;
		    }
		  else
		    emit_move_insn (reg_rtx, mem_rtx);
		}
	      else
		fprintf (file, "\t%s\t%s,%ld(%s)\n",
			 (TARGET_SINGLE_FLOAT
			  ? ((store_p) ? "s.s" : "l.s")
			  : ((store_p) ? "s.d" : "l.d")),
			 reg_names[regno],
			 fp_offset - base_offset,
			 reg_names[REGNO(base_reg_rtx)]);


	      fp_offset -= fp_size;
	    }
	}
    }
}


/* Set up the stack and frame (if desired) for the function.  */

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  char *fnname;
  long tsize = current_frame_info.total_size;

  ASM_OUTPUT_SOURCE_FILENAME (file, DECL_SOURCE_FILE (current_function_decl));

#ifdef SDB_DEBUGGING_INFO
  if (debug_info_level != DINFO_LEVEL_TERSE && write_symbols == SDB_DEBUG)
    ASM_OUTPUT_SOURCE_LINE (file, DECL_SOURCE_LINE (current_function_decl));
#endif

  inside_function = 1;

#ifndef FUNCTION_NAME_ALREADY_DECLARED
  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.ent\t", file);
      assemble_name (file, fnname);
      fputs ("\n", file);
    }

  assemble_name (file, fnname);
  fputs (":\n", file);
#endif

  if (!flag_inhibit_size_directive)
    {
      fprintf (file, "\t.frame\t%s,%d,%s\t\t# vars= %d, regs= %d/%d, args= %d, extra= %d\n",
	      reg_names[ (frame_pointer_needed) ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM ],
	      tsize,
	      reg_names[31 + GP_REG_FIRST],
	      current_frame_info.var_size,
	      current_frame_info.num_gp,
	      current_frame_info.num_fp,
	      current_function_outgoing_args_size,
	      current_frame_info.extra_size);

      fprintf (file, "\t.mask\t0x%08lx,%d\n\t.fmask\t0x%08lx,%d\n",
	       current_frame_info.mask,
	       current_frame_info.gp_save_offset,
	       current_frame_info.fmask,
	       current_frame_info.fp_save_offset);
    }

  if (TARGET_ABICALLS && mips_abi == ABI_32)
    {
      char *sp_str = reg_names[STACK_POINTER_REGNUM];

      fprintf (file, "\t.set\tnoreorder\n\t.cpload\t%s\n\t.set\treorder\n",
	       reg_names[PIC_FUNCTION_ADDR_REGNUM]);
      if (tsize > 0)
	{
	  fprintf (file, "\t%s\t%s,%s,%d\n",
		   (TARGET_LONG64 ? "dsubu" : "subu"),
		   sp_str, sp_str, tsize);
	  fprintf (file, "\t.cprestore %d\n", current_frame_info.args_size);
	}

      if (dwarf2out_do_frame ())
	dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, tsize);
    }
}


/* Expand the prologue into a bunch of separate insns.  */

void
mips_expand_prologue ()
{
  int regno;
  long tsize;
  rtx tmp_rtx	 = (rtx)0;
  char *arg_name = (char *)0;
  tree fndecl	 = current_function_decl;
  tree fntype	 = TREE_TYPE (fndecl);
  tree fnargs	 = DECL_ARGUMENTS (fndecl);
  rtx next_arg_reg;
  int i;
  tree next_arg;
  tree cur_arg;
  CUMULATIVE_ARGS args_so_far;

  /* If struct value address is treated as the first argument, make it so.  */
  if (aggregate_value_p (DECL_RESULT (fndecl))
      && ! current_function_returns_pcc_struct
      && struct_value_incoming_rtx == 0)
    {
      tree type = build_pointer_type (fntype);
      tree function_result_decl = build_decl (PARM_DECL, NULL_TREE, type);
      DECL_ARG_TYPE (function_result_decl) = type;
      TREE_CHAIN (function_result_decl) = fnargs;
      fnargs = function_result_decl;
    }

  /* Determine the last argument, and get its name.  */

  INIT_CUMULATIVE_ARGS (args_so_far, fntype, (rtx)0, 0);
  regno = GP_ARG_FIRST;

  for (cur_arg = fnargs; cur_arg != (tree)0; cur_arg = next_arg)
    {
      tree passed_type = DECL_ARG_TYPE (cur_arg);
      enum machine_mode passed_mode = TYPE_MODE (passed_type);
      rtx entry_parm;

      if (TREE_ADDRESSABLE (passed_type))
	{
	  passed_type = build_pointer_type (passed_type);
	  passed_mode = Pmode;
	}

      entry_parm = FUNCTION_ARG (args_so_far, passed_mode, passed_type, 1);

      if (entry_parm)
	{
	  int words;

	  /* passed in a register, so will get homed automatically */
	  if (GET_MODE (entry_parm) == BLKmode)
	    words = (int_size_in_bytes (passed_type) + 3) / 4;
	  else
	    words = (GET_MODE_SIZE (GET_MODE (entry_parm)) + 3) / 4;

	  regno = REGNO (entry_parm) + words - 1;
	}
      else
	{
	  regno = GP_ARG_LAST+1;
	  break;
	}

      FUNCTION_ARG_ADVANCE (args_so_far, passed_mode, passed_type, 1);

      next_arg = TREE_CHAIN (cur_arg);
      if (next_arg == (tree)0)
	{
	  if (DECL_NAME (cur_arg))
	    arg_name = IDENTIFIER_POINTER (DECL_NAME (cur_arg));

	  break;
	}
    }

  /* In order to pass small structures by value in registers
     compatibly with the MIPS compiler, we need to shift the value
     into the high part of the register.  Function_arg has encoded a
     PARALLEL rtx, holding a vector of adjustments to be made as the
     next_arg_reg variable, so we split up the insns, and emit them
     separately.  */

  next_arg_reg = FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1);
  if (next_arg_reg != (rtx)0 && GET_CODE (next_arg_reg) == PARALLEL)
    {
      rtvec adjust = XVEC (next_arg_reg, 0);
      int num = GET_NUM_ELEM (adjust);

      for (i = 0; i < num; i++)
	{
	  rtx pattern = RTVEC_ELT (adjust, i);
	  if (GET_CODE (pattern) != SET
	      || GET_CODE (SET_SRC (pattern)) != ASHIFT)
	    abort_with_insn (pattern, "Insn is not a shift");

	  PUT_CODE (SET_SRC (pattern), ASHIFTRT);
	  emit_insn (pattern);
	}
    }

  tsize = compute_frame_size (get_frame_size ());

  /* If this function is a varargs function, store any registers that
     would normally hold arguments ($4 - $7) on the stack.  */
  if (mips_abi == ABI_32
      && ((TYPE_ARG_TYPES (fntype) != 0
	   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype))) != void_type_node))
	  || (arg_name != (char *)0
	      && ((arg_name[0] == '_' && strcmp (arg_name, "__builtin_va_alist") == 0)
		  || (arg_name[0] == 'v' && strcmp (arg_name, "va_alist") == 0)))))
    {
      int offset = (regno - GP_ARG_FIRST) * UNITS_PER_WORD;
      rtx ptr = stack_pointer_rtx;

      /* If we are doing svr4-abi, sp has already been decremented by tsize. */
      if (TARGET_ABICALLS)
	offset += tsize;

      for (; regno <= GP_ARG_LAST; regno++)
	{
	  if (offset != 0)
	    ptr = gen_rtx (PLUS, Pmode, stack_pointer_rtx, GEN_INT (offset));
	  emit_move_insn (gen_rtx (MEM, word_mode, ptr),
			  gen_rtx (REG, word_mode, regno));
	  offset += UNITS_PER_WORD;
	}
    }

  if (tsize > 0)
    {
      rtx tsize_rtx = GEN_INT (tsize);

      /* If we are doing svr4-abi, sp move is done by function_prologue.  */
      if (!TARGET_ABICALLS || mips_abi != ABI_32)
	{
	  rtx insn;

	  if (tsize > 32767)
	    {
	      tmp_rtx = gen_rtx (REG, Pmode, MIPS_TEMP1_REGNUM);

	      /* Instruction splitting doesn't preserve the RTX_FRAME_RELATED_P
		 bit, so make sure that we don't emit anything that can be
		 split.  */
	      /* ??? There is no DImode ori immediate pattern, so we can only
		 do this for 32 bit code.  */
	      if (large_int (tsize_rtx) && GET_MODE (tmp_rtx) == SImode)
		{
		  insn = emit_move_insn (tmp_rtx,
					 GEN_INT (tsize & 0xffff0000));
		  RTX_FRAME_RELATED_P (insn) = 1;
		  insn = emit_insn (gen_iorsi3 (tmp_rtx, tmp_rtx,
						GEN_INT (tsize & 0x0000ffff)));
		  RTX_FRAME_RELATED_P (insn) = 1;
		}
	      else
		{
		  insn = emit_move_insn (tmp_rtx, tsize_rtx);
		  RTX_FRAME_RELATED_P (insn) = 1;
		}

	      tsize_rtx = tmp_rtx;
	    }

	  if (TARGET_LONG64)
	    insn = emit_insn (gen_subdi3 (stack_pointer_rtx, stack_pointer_rtx,
					  tsize_rtx));
	  else
	    insn = emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx,
					  tsize_rtx));

	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      save_restore_insns (TRUE, tmp_rtx, tsize, (FILE *)0);

      if (frame_pointer_needed)
	{
	  rtx insn;

	  if (TARGET_64BIT)
	    insn= emit_insn (gen_movdi (frame_pointer_rtx, stack_pointer_rtx));
	  else
	    insn= emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));

	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      if (TARGET_ABICALLS && mips_abi != ABI_32)
	emit_insn (gen_loadgp (XEXP (DECL_RTL (current_function_decl), 0),
			       gen_rtx (REG, DImode, 25)));
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */

  if (profile_flag || profile_block_flag)
    emit_insn (gen_blockage ());
}


/* Do any necessary cleanup after a function to restore stack, frame, and regs. */

#define RA_MASK ((long) 0x80000000)	/* 1 << 31 */
#define PIC_OFFSET_TABLE_MASK (1 << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  char *fnname;

#ifndef FUNCTION_NAME_ALREADY_DECLARED
  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.end\t", file);
      assemble_name (file, fnname);
      fputs ("\n", file);
    }
#endif

  if (TARGET_STATS)
    {
      int num_gp_regs = current_frame_info.gp_reg_size / 4;
      int num_fp_regs = current_frame_info.fp_reg_size / 8;
      int num_regs    = num_gp_regs + num_fp_regs;
      char *name      = fnname;

      if (name[0] == '*')
	name++;

      dslots_load_total += num_regs;

      fprintf (stderr,
	       "%-20s fp=%c leaf=%c alloca=%c setjmp=%c stack=%4ld arg=%3ld reg=%2d/%d delay=%3d/%3dL %3d/%3dJ refs=%3d/%3d/%3d",
	       name,
	       (frame_pointer_needed) ? 'y' : 'n',
	       ((current_frame_info.mask & RA_MASK) != 0) ? 'n' : 'y',
	       (current_function_calls_alloca) ? 'y' : 'n',
	       (current_function_calls_setjmp) ? 'y' : 'n',
	       (long)current_frame_info.total_size,
	       (long)current_function_outgoing_args_size,
	       num_gp_regs, num_fp_regs,
	       dslots_load_total, dslots_load_filled,
	       dslots_jump_total, dslots_jump_filled,
	       num_refs[0], num_refs[1], num_refs[2]);

      if (HALF_PIC_NUMBER_PTRS > prev_half_pic_ptrs)
	{
	  fprintf (stderr, " half-pic=%3d", HALF_PIC_NUMBER_PTRS - prev_half_pic_ptrs);
	  prev_half_pic_ptrs = HALF_PIC_NUMBER_PTRS;
	}

      if (HALF_PIC_NUMBER_REFS > prev_half_pic_refs)
	{
	  fprintf (stderr, " pic-ref=%3d", HALF_PIC_NUMBER_REFS - prev_half_pic_refs);
	  prev_half_pic_refs = HALF_PIC_NUMBER_REFS;
	}

      fputc ('\n', stderr);
    }

  /* Reset state info for each function.  */
  inside_function    = FALSE;
  ignore_line_number = FALSE;
  dslots_load_total  = 0;
  dslots_jump_total  = 0;
  dslots_load_filled = 0;
  dslots_jump_filled = 0;
  num_refs[0]	     = 0;
  num_refs[1]	     = 0;
  num_refs[2]	     = 0;
  mips_load_reg      = (rtx)0;
  mips_load_reg2     = (rtx)0;
  current_frame_info = zero_frame_info;

  /* Restore the output file if optimizing the GP (optimizing the GP causes
     the text to be diverted to a tempfile, so that data decls come before
     references to the data).  */

  if (TARGET_GP_OPT)
    asm_out_file = asm_out_data_file;
}


/* Expand the epilogue into a bunch of separate insns.  */

void
mips_expand_epilogue ()
{
  long tsize = current_frame_info.total_size;
  rtx tsize_rtx = GEN_INT (tsize);
  rtx tmp_rtx = (rtx)0;

  if (mips_can_use_return_insn ())
    {
      emit_insn (gen_return ());
      return;
    }

  if (tsize > 32767)
    {
      tmp_rtx = gen_rtx (REG, Pmode, MIPS_TEMP1_REGNUM);
      emit_move_insn (tmp_rtx, tsize_rtx);
      tsize_rtx = tmp_rtx;
    }

  if (tsize > 0)
    {
      if (frame_pointer_needed)
	{
	  emit_insn (gen_blockage ());
	  if (TARGET_LONG64)
	    emit_insn (gen_movdi (stack_pointer_rtx, frame_pointer_rtx));
	  else
	    emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));
	}
      /* The GP/PIC register is implicitly used by all SYMBOL_REFs, so if we
	 are going to restore it, then we must emit a blockage insn to
	 prevent the scheduler from moving the restore out of the epilogue.  */
      else if (TARGET_ABICALLS && mips_abi != ABI_32
	       && (current_frame_info.mask
		   & (1L << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))))
	emit_insn (gen_blockage ());

      save_restore_insns (FALSE, tmp_rtx, tsize, (FILE *)0);

      emit_insn (gen_blockage ());
      if (TARGET_LONG64)
	emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
			       tsize_rtx));
      else
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			       tsize_rtx));
    }

  emit_jump_insn (gen_return_internal ());
}


/* Return true if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

int
mips_can_use_return_insn ()
{
  if (!reload_completed)
    return 0;

  if (regs_ever_live[31] || profile_flag)
    return 0;

  if (current_frame_info.initialized)
    return current_frame_info.total_size == 0;

  return (compute_frame_size (get_frame_size ())) == 0;
}

/* Choose the section to use for the constant rtx expression X that has
   mode MODE.  */

mips_select_rtx_section (mode, x)
     enum machine_mode mode;
     rtx x;
{
  if (TARGET_EMBEDDED_DATA)
    {
      /* For embedded applications, always put constants in read-only data,
	 in order to reduce RAM usage.  */
      READONLY_DATA_SECTION ();
    }
  else
    {
      /* For hosted applications, always put constants in small data if
	 possible, as this gives the best performance.  */
     
      if (GET_MODE_SIZE (mode) <= mips_section_threshold
	  && mips_section_threshold > 0)
	SMALL_DATA_SECTION ();
      else
	READONLY_DATA_SECTION ();
    }
}

/* Choose the section to use for DECL.  RELOC is true if its value contains
   any relocatable expression.  */

mips_select_section (decl, reloc)
     tree decl;
     int reloc;
{
  int size = int_size_in_bytes (TREE_TYPE (decl));

  if (TARGET_EMBEDDED_PIC
      && TREE_CODE (decl) == STRING_CST
      && !flag_writable_strings)
    {
      /* For embedded position independent code, put constant strings
	 in the text section, because the data section is limited to
	 64K in size.  */

      text_section ();
    }
  else if (TARGET_EMBEDDED_DATA)
    {
      /* For embedded applications, always put an object in read-only data
	 if possible, in order to reduce RAM usage.  */

      if (((TREE_CODE (decl) == VAR_DECL
	    && TREE_READONLY (decl) && !TREE_SIDE_EFFECTS (decl)
	    && DECL_INITIAL (decl)
	    && (DECL_INITIAL (decl) == error_mark_node
		|| TREE_CONSTANT (DECL_INITIAL (decl))))
	   /* Deal with calls from output_constant_def_contents.  */
	   || (TREE_CODE (decl) != VAR_DECL
	       && (TREE_CODE (decl) != STRING_CST
		   || !flag_writable_strings)))
	  && ! (flag_pic && reloc))
	READONLY_DATA_SECTION ();
      else if (size > 0 && size <= mips_section_threshold)
	SMALL_DATA_SECTION ();
      else
	data_section ();
    }
  else
    {
      /* For hosted applications, always put an object in small data if
	 possible, as this gives the best performance.  */

      if (size > 0 && size <= mips_section_threshold)
	SMALL_DATA_SECTION ();
      else if (((TREE_CODE (decl) == VAR_DECL
		 && TREE_READONLY (decl) && !TREE_SIDE_EFFECTS (decl)
		 && DECL_INITIAL (decl)
		 && (DECL_INITIAL (decl) == error_mark_node
		     || TREE_CONSTANT (DECL_INITIAL (decl))))
		/* Deal with calls from output_constant_def_contents.  */
		|| (TREE_CODE (decl) != VAR_DECL
		    && (TREE_CODE (decl) != STRING_CST
			|| !flag_writable_strings)))
	       && ! (flag_pic && reloc))
	READONLY_DATA_SECTION ();
      else
	data_section ();
    }
}

#ifdef MIPS_ABI_DEFAULT
/* Support functions for the 64 bit ABI.  */

/* Return register to use for a function return value with VALTYPE for function
   FUNC.  */

rtx
mips_function_value (valtype, func)
     tree valtype;
     tree func;
{
  int reg = GP_RETURN;
  enum machine_mode mode = TYPE_MODE (valtype);
  enum mode_class mclass = GET_MODE_CLASS (mode);

  /* ??? How should we return complex float?  */
  if (mclass == MODE_FLOAT || mclass == MODE_COMPLEX_FLOAT)
    {
      if (TARGET_SINGLE_FLOAT
	  && (mclass == MODE_FLOAT
	      ? GET_MODE_SIZE (mode) > 4
	      : GET_MODE_SIZE (mode) / 2 > 4))
	reg = GP_RETURN;
      else
	reg = FP_RETURN;
    }
  else if (TREE_CODE (valtype) == RECORD_TYPE
	   && mips_abi != ABI_32 && mips_abi != ABI_EABI)
    {
      /* A struct with only one or two floating point fields is returned in
	 the floating point registers.  */
      tree field, fields[2];
      int i;

      for (i = 0, field = TYPE_FIELDS (valtype); field;
	   field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
	  if (TREE_CODE (TREE_TYPE (field)) != REAL_TYPE || i >= 2)
	    break;

	  fields[i++] = field;
	}
	  
      /* Must check i, so that we reject structures with no elements.  */
      if (! field)
	{
	  if (i == 1)
	    {
	      /* The structure has DImode, but we don't allow DImode values
		 in FP registers, so we use a PARALLEL even though it isn't
		 strictly necessary.  */
	      enum machine_mode field_mode = TYPE_MODE (TREE_TYPE (fields[0]));

	      return gen_rtx (PARALLEL, mode,
			      gen_rtvec (1,
					 gen_rtx (EXPR_LIST, VOIDmode,
						  gen_rtx (REG, field_mode, FP_RETURN),
						  const0_rtx)));
	    }
	  else if (i == 2)
	    {
	      enum machine_mode first_mode
		= TYPE_MODE (TREE_TYPE (fields[0]));
	      enum machine_mode second_mode
		= TYPE_MODE (TREE_TYPE (fields[1]));
	      int first_offset
		= TREE_INT_CST_LOW (DECL_FIELD_BITPOS (fields[0]));
	      int second_offset
		= TREE_INT_CST_LOW (DECL_FIELD_BITPOS (fields[1]));

	      return gen_rtx (PARALLEL, mode,
			      gen_rtvec (2,
					 gen_rtx (EXPR_LIST, VOIDmode,
						  gen_rtx (REG, first_mode, FP_RETURN),
						  GEN_INT (first_offset / BITS_PER_UNIT)),
					 gen_rtx (EXPR_LIST, VOIDmode,
						  gen_rtx (REG, second_mode, FP_RETURN + 2),
						  GEN_INT (second_offset / BITS_PER_UNIT))));
	    }
	}
    }

  return gen_rtx (REG, mode, reg);
}

/* The implementation of FUNCTION_ARG_PASS_BY_REFERENCE.  Return
   nonzero when an argument must be passed by reference.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int size;

  if (mips_abi != ABI_EABI)
    return 0;

  /* ??? How should SCmode be handled?  */
  if (type == NULL_TREE || mode == DImode || mode == DFmode)
    return 0;

  size = int_size_in_bytes (type);
  return size == -1 || size > UNITS_PER_WORD;
}

#endif

/* This function returns the register class required for a secondary
   register when copying between one of the registers in CLASS, and X,
   using MODE.  If IN_P is nonzero, the copy is going from X to the
   register, otherwise the register is the source.  A return value of
   NO_REGS means that no secondary register is required.  */

enum reg_class
mips_secondary_reload_class (class, mode, x, in_p)
     enum reg_class class;
     enum machine_mode mode;
     rtx x;
     int in_p;
{
  int regno = -1;

  if (GET_CODE (x) == SIGN_EXTEND)
    {
      int off = 0;

      x = XEXP (x, 0);

      /* We may be called with reg_renumber NULL from regclass.
	 ??? This is probably a bug.  */
      if (reg_renumber)
	regno = true_regnum (x);
      else
	{
	  while (GET_CODE (x) == SUBREG)
	    {
	      off += SUBREG_WORD (x);
	      x = SUBREG_REG (x);
	    }
	  if (GET_CODE (x) == REG)
	    regno = REGNO (x) + off;
	}
    }
  else if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
    regno = true_regnum (x);

  /* We always require a general register when copying anything to
     HILO_REGNUM, except when copying an SImode value from HILO_REGNUM
     to a general register, or when copying from register 0.  */
  if (class == HILO_REG && regno != GP_REG_FIRST + 0)
    {
      if (! in_p
	  && GP_REG_P (regno)
	  && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (SImode))
	return NO_REGS;
      return GR_REGS;
    }
  if (regno == HILO_REGNUM)
    {
      if (in_p
	  && class == GR_REGS
	  && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (SImode))
	return NO_REGS;
      return GR_REGS;
    }

  /* Copying from HI or LO to anywhere other than a general register
     requires a general register.  */
  if (class == HI_REG || class == LO_REG || class == MD_REGS)
    {
      if (GP_REG_P (regno))
	return NO_REGS;
      return GR_REGS;
    }
  if (MD_REG_P (regno))
    {
      if (class == GR_REGS)
	return NO_REGS;
      return GR_REGS;
    }

  /* We can only copy a value to a condition code register from a
     floating point register, and even then we require a scratch
     floating point register.  We can only copy a value out of a
     condition code register into a general register.  */
  if (class == ST_REGS)
    {
      if (in_p)
	return FP_REGS;
      if (GP_REG_P (regno))
	return NO_REGS;
      return GR_REGS;
    }
  if (ST_REG_P (regno))
    {
      if (! in_p)
	return FP_REGS;
      if (class == GR_REGS)
	return NO_REGS;
      return GR_REGS;
    }

  return NO_REGS;
}
