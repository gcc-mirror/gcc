/* Subroutines for insn-output.c for MIPS
   Contributed by A. Lichnewsky, lich@inria.inria.fr.
   Changes by     Michael Meissner, meissner@osf.org.
   Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
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

#include <stdio.h>
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

extern tree   current_function_decl;
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

/* Next label # for each statment for Silicon Graphics IRIS systems. */
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

/* Strings to hold which cpu and instruction set architecture to use.  */
char *mips_cpu_string;		/* for -mcpu=<xxx> */
char *mips_isa_string;		/* for -mips{1,2,3} */

/* Array to RTX class classification.  At present, we care about
   whether the operator is an add-type operator, or a divide/modulus,
   and if divide/modulus, whether it is unsigned.  This is for the
   peephole code.  */
char mips_rtx_classify[NUM_RTX_CODE];

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
 "hi",   "lo",   "$fcr31"
};

/* Mips software names for the registers, used to overwrite the
   mips_reg_names array.  */

char mips_sw_reg_names[][8] =
{
  "$0",   "at",   "v0",   "v1",   "a0",   "a1",   "a2",   "a3",
  "t0",   "t1",   "t2",   "t3",   "t4",   "t5",   "t6",   "t7",
  "s0",   "s1",   "s2",   "s3",   "s4",   "s5",   "s6",   "s7",
  "t8",   "t9",   "k0",   "k1",   "gp",   "sp",   "$fp",   "ra",
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",
  "hi",   "lo",   "$fcr31"
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
  HI_REG,	LO_REG,		ST_REGS
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

/* Return truth value of whether OP is an integer which is too big to
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

  if ((value & 0xffff0000) == value)			/* lui reg,value>>16 */
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
      if (CONST_DOUBLE_HIGH (op) != 0 || CONST_DOUBLE_LOW (op) != 0)
	return FALSE;

      return TRUE;

    case REG:
    case SUBREG:
      return register_operand (op, mode);
    }

  return FALSE;
}

/* Return truth value of whether OP is one of the special multiply/divide
   registers (hi, lo).  */

int
md_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_CODE (op) == REG
	  && MD_REG_P (REGNO (op)));
}

/* Return truth value of whether OP is the FP status register.  */

int
fpsw_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == REG && ST_REG_P (REGNO (op)));
}

/* Return truth value if a CONST_DOUBLE is ok to be a legitimate constant.  */

int
mips_const_double_ok (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_DOUBLE)
    return FALSE;

  if (mode == DImode)
    return TRUE;

  if (mode != SFmode && mode != DFmode)
    return FALSE;

  if (CONST_DOUBLE_HIGH (op) == 0 && CONST_DOUBLE_LOW (op) == 0)
    return TRUE;

#if HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
  if (TARGET_MIPS_AS)		/* gas doesn't like li.d/li.s yet */
    {
      union { double d; int i[2]; } u;
      double d;

      u.i[0] = CONST_DOUBLE_LOW (op);
      u.i[1] = CONST_DOUBLE_HIGH (op);
      d = u.d;

      if (d != d)
	return FALSE;		/* NAN */

      if (d < 0.0)
	d = - d;

      /* Rather than trying to get the accuracy down to the last bit,
	 just use approximate ranges.  */

      if (mode == DFmode && d > 1.0e-300 && d < 1.0e300)
	return TRUE;

      if (mode == SFmode && d > 1.0e-38 && d < 1.0e+38)
	return TRUE;
    }
#endif

  return FALSE;
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
  if (GET_MODE_SIZE (GET_MODE (op)) > (HAVE_64BIT_P () ? 8 : 4))
    return FALSE;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    default:
      break;

    case REG:
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
      if (mips_section_threshold == 0 || !optimize || !TARGET_GP_OPT)
	return FALSE;

      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (addr, &offset);
	if (GET_CODE (op) != SYMBOL_REF)
	  return FALSE;

	/* let's be paranoid.... */
	if (INTVAL (offset) < 0 || INTVAL (offset) > 0xffff)
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

  return (classify_op (op, mode) & CLASS_EQUALITY_OP) != 0;
}

/* Return true if the code is a relational operations (EQ, LE, etc.) */

int
cmp_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return FALSE;

  return (classify_op (op, mode) & CLASS_CMP_OP) != 0;
}


/* Genrecog does not take the type of match_operator into consideration,
   and would complain about two patterns being the same if the same
   function is used, so make it believe they are different.  */

int
cmp2_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return FALSE;

  return (classify_op (op, mode) & CLASS_CMP_OP) != 0;
}

/* Return true if the code is an unsigned relational operations (LEU, etc.) */

int
uns_cmp_op (op,mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return FALSE;

  return (classify_op (op, mode) & CLASS_UNS_CMP_OP) == CLASS_UNS_CMP_OP;
}

/* Return true if the code is a relational operation FP can use.  */

int
fcmp_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return FALSE;

  return (classify_op (op, mode) & CLASS_FCMP_OP) != 0;
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


/* Return an operand string if the given instruction's delay slot or
   wrap it in a .set noreorder section.  This is for filling delay
   slots on load type instructions under GAS, which does no reordering
   on its own.  For the MIPS assembler, all we do is update the filled
   delay slot statistics.

   We assume that operands[0] is the target register that is set.

   In order to check the next insn, most of this functionality is moved
   to FINAL_PRESCAN_INSN, and we just set the global variables that
   it needs.  */

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
  mips_load_reg  = set_reg;
  mips_load_reg2 = (mode == DImode || mode == DFmode)
			? gen_rtx (REG, SImode, REGNO (set_reg) + 1)
			: (rtx)0;

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

  if (TARGET_GAS && set_noreorder++ == 0)
    fputs ("\t.set\tnoreorder\n", asm_out_file);

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
		  ret = "mf%1\t%0";
		}

	      else
		{
		  delay = DELAY_LOAD;
		  if (FP_REG_P (regno1))
		    ret = "mfc1\t%0,%1";

		  else if (regno1 == FPSW_REGNUM)
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
		  ret = "mt%0\t%1";
		}
	    }

	  else if (regno0 == FPSW_REGNUM)
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
		default:							break;
		case SFmode: ret = "lw\t%0,%1";					break;
		case SImode: ret = "lw\t%0,%1";					break;
		case HImode: ret = (unsignedp) ? "lhu\t%0,%1" : "lh\t%0,%1";	break;
		case QImode: ret = (unsignedp) ? "lbu\t%0,%1" : "lb\t%0,%1";	break;
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

      else if (code1 == CONST_INT)
	{
	  if (INTVAL (op1) == 0)
	    {
	      if (GP_REG_P (regno0))
		ret = "move\t%0,%z1";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%z1,%0";
		}
	    }

	  else if (GP_REG_P (regno0))
	    ret = (INTVAL (op1) < 0) ? "li\t%0,%1\t\t\t# %X1" : "li\t%0,%X1\t\t# %1";
	}

      else if (code1 == CONST_DOUBLE && mode == SFmode)
	{
	  if (CONST_DOUBLE_HIGH (op1) == 0 && CONST_DOUBLE_LOW (op1) == 0)
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
		      ret = "lw\t%0,%2";
		    }
		  else
		    {
		      dslots_load_total++;
		      operands[3] = offset;
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

      else if (code1 == CONST_DOUBLE && CONST_DOUBLE_HIGH (op1) == 0 && CONST_DOUBLE_LOW (op1) == 0)
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

  while (code1 == SUBREG)
    {
      subreg_word1 += SUBREG_WORD (op1);
      op1 = SUBREG_REG (op1);
      code1 = GET_CODE (op1);
    }
      
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
		  ret = (TARGET_FLOAT64)
				? "dmtc1\t%1,%0"
				: "mtc1\t%L1,%0\n\tmtc1\t%M1,%D0";
		}
	    }

	  else if (FP_REG_P (regno1))
	    {
	      delay = DELAY_LOAD;
	      ret = (TARGET_FLOAT64)
			? "dmfc1\t%0,%1"
			: "mfc1\t%L0,%1\n\tmfc1\t%M0,%D1";
	    }

	  else if (MD_REG_P (regno0) && GP_REG_P (regno1))
	    {
	      delay = DELAY_HILO;
	      ret = "mthi\t%M1\n\tmtlo\t%L1";
	    }

	  else if (GP_REG_P (regno0) && MD_REG_P (regno1))
	    {
	      delay = DELAY_HILO;
	      ret = "mfhi\t%M0\n\tmflo\t%L0";
	    }

	  else if (regno0 != (regno1+1))
	    ret = "move\t%0,%1\n\tmove\t%D0,%D1";

	  else
	    ret = "move\t%D0,%D1\n\tmove\t%0,%1";
	}

      else if (code1 == CONST_DOUBLE)
	{
	  if (CONST_DOUBLE_HIGH (op1) != 0 || CONST_DOUBLE_LOW (op1) != 0)
	    {
	      if (GET_MODE (op1) == DFmode)
		{
		  delay = DELAY_LOAD;
		  ret = "li.d\t%0,%1";
		}

	      else
		{
		  operands[2] = GEN_INT (CONST_DOUBLE_LOW (op1));
		  operands[3] = GEN_INT (CONST_DOUBLE_HIGH (op1));
		  ret = "li\t%M0,%3\n\tli\t%L0,%2";
		}
	    }

	  else
	    {
	      if (GP_REG_P (regno0))
		ret = "move\t%0,%.\n\tmove\t%D0,%.";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = (TARGET_FLOAT64)
				? "dmtc1\t%.,%0"
				: "mtc1\t%.,%0\n\tmtc1\t%.,%D0";
		}
	    }
	}

      else if (code1 == CONST_INT && INTVAL (op1) == 0)
	{
	  if (GP_REG_P (regno0))
	    ret = "move\t%0,%.\n\tmove\t%D0,%.";
	  
	  else if (FP_REG_P (regno0))
	    {
	      delay = DELAY_LOAD;
	      ret = (TARGET_FLOAT64)
				? "dmtc1\t%.,%0"
				: "mtc1\t%.,%0\n\tmtc1\t%.,%D0";
	    }
	}
	
      else if (code1 == CONST_INT && GET_MODE (op0) == DImode && GP_REG_P (regno0))
	{
	  operands[2] = GEN_INT (INTVAL (operands[1]) >= 0 ? 0 : -1);
	  ret = "li\t%M0,%2\n\tli\t%L0,%1";
	}

      else if (code1 == MEM)
	{
	  delay = DELAY_LOAD;

	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 2);

	  if (FP_REG_P (regno0))
	    ret = "l.d\t%0,%1";

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
    }

  else if (code0 == MEM)
    {
      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_word1;

	  if (FP_REG_P (regno1))
	    ret = "s.d\t%1,%0";

	  else if (offsettable_address_p (1, DFmode, XEXP (op0, 0)))
	    {
	      operands[2] = adj_offsettable_operand (op0, 4);
	      ret = "sw\t%1,%0\n\tsw\t%D1,%2";
	    }
	}

      else if (code1 == CONST_DOUBLE
	       && CONST_DOUBLE_HIGH (op1) == 0
	       && CONST_DOUBLE_LOW (op1) == 0
	       && offsettable_address_p (1, DFmode, XEXP (op0, 0)))
	{
	  if (TARGET_FLOAT64)
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
    case HIGH:
      return 1;

    case LABEL_REF:
      return 2;

    case CONST:
      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (addr, &offset);
	if (GET_CODE (addr) == LABEL_REF)
	  return 2;

	if (GET_CODE (addr) != SYMBOL_REF)
	  return 4;

	if (INTVAL (offset) < -32768 || INTVAL (offset) > 32767)
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
	    {
	      int value = INTVAL (plus1);
	      return (value < -32768 || value > 32767) ? 2 : 1;
	    }

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
   (rtx)0	       	No register, generate a simple branch.  */

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
      result = gen_reg_rtx (SImode);
    }

  /* Make sure we can handle any constants given to us.  */
  if (GET_CODE (cmp0) == CONST_INT)
    cmp0 = force_reg (SImode, cmp0);

  if (GET_CODE (cmp1) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL (cmp1);
      if (value < p_info->const_low || value > p_info->const_high)
	cmp1 = force_reg (SImode, cmp1);
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
	     thus, x > 0xffffffffu would turn into x > 0u.  */
	  if ((p_info->unsignedp
	       ? (unsigned HOST_WIDE_INT) new > INTVAL (cmp1)
	       : new > INTVAL (cmp1))
	      != (p_info->const_add > 0))
	    /* 1 is the right value in the LE and LEU case.
	       In the GT and GTU case, *p_invert is already set,
	       so this is effectively 0.  */
	    return force_reg (SImode, const1_rtx);
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
      reg = (invert || eqne_p) ? gen_reg_rtx (SImode) : result;
      emit_move_insn (reg, gen_rtx (p_info->test_code, SImode, cmp0, cmp1));
    }

  if (test == ITEST_NE)
    {
      emit_move_insn (result, gen_rtx (GTU, SImode, reg, const0_rtx));
      invert = FALSE;
    }

  else if (test == ITEST_EQ)
    {
      reg2 = (invert) ? gen_reg_rtx (SImode) : result;
      emit_move_insn (reg2, gen_rtx (LTU, SImode, reg, const1_rtx));
      reg = reg2;
    }

  if (invert)
    emit_move_insn (result, gen_rtx (XOR, SImode, reg, const1_rtx));

  return result;
}


/* Emit the common code for doing conditional branches.
   operand[0] is the label to jump to.
   The comparison operands are saved away by cmp{si,sf,df}.  */

void
gen_conditional_branch (operands, test_code)
     rtx operands[];
     enum rtx_code test_code;
{
  static enum machine_mode mode_map[(int)CMP_MAX][(int)ITEST_MAX] = {
    {				/* CMP_SI */
      SImode,			/* eq  */
      SImode,			/* ne  */
      SImode,			/* gt  */
      SImode,			/* ge  */
      SImode,			/* lt  */
      SImode,			/* le  */
      SImode,			/* gtu */
      SImode,			/* geu */
      SImode,			/* ltu */
      SImode,			/* leu */
    },
    {				/* CMP_SF */
      CC_FPmode,		/* eq  */
      CC_REV_FPmode,		/* ne  */
      CC_FPmode,		/* gt  */
      CC_FPmode,		/* ge  */
      CC_FPmode,		/* lt  */
      CC_FPmode,		/* le  */
      VOIDmode,			/* gtu */
      VOIDmode,			/* geu */
      VOIDmode,			/* ltu */
      VOIDmode,			/* leu */
    },
    {				/* CMP_DF */
      CC_FPmode,		/* eq  */
      CC_REV_FPmode,		/* ne  */
      CC_FPmode,		/* gt  */
      CC_FPmode,		/* ge  */
      CC_FPmode,		/* lt  */
      CC_FPmode,		/* le  */
      VOIDmode,			/* gtu */
      VOIDmode,			/* geu */
      VOIDmode,			/* ltu */
      VOIDmode,			/* leu */
    },
  };

  enum machine_mode mode;
  enum cmp_type type	  = branch_type;
  rtx cmp0		  = branch_cmp[0];
  rtx cmp1		  = branch_cmp[1];
  rtx label1		  = gen_rtx (LABEL_REF, VOIDmode, operands[0]);
  rtx label2		  = pc_rtx;
  rtx reg		  = (rtx)0;
  int invert		  = 0;
  enum internal_test test = map_test_to_internal_test (test_code);

  if (test == ITEST_MAX)
    {
      mode = SImode;
      goto fail;
    }

  /* Get the machine mode to use (CCmode, CC_EQmode, CC_FPmode, or CC_REV_FPmode).  */
  mode = mode_map[(int)type][(int)test];
  if (mode == VOIDmode)
    goto fail;

  switch (branch_type)
    {
    default:
      goto fail;

    case CMP_SI:
      reg = gen_int_relational (test_code, (rtx)0, cmp0, cmp1, &invert);
      if (reg != (rtx)0)
	{
	  cmp0 = reg;
	  cmp1 = const0_rtx;
	  test_code = NE;
	}

      /* Make sure not non-zero constant if ==/!= */
      else if (GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) != 0)
	cmp1 = force_reg (SImode, cmp1);

      break;

    case CMP_DF:
    case CMP_SF:
      {
	rtx reg = gen_rtx (REG, mode, FPSW_REGNUM);
	emit_insn (gen_rtx (SET, VOIDmode, reg, gen_rtx (test_code, mode, cmp0, cmp1)));
	cmp0 = reg;
	cmp1 = const0_rtx;
	test_code = NE;
      }
      break;
    }

  /* Generate the jump */
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

  return;

fail:
  abort_with_insn (gen_rtx (test_code, mode, cmp0, cmp1), "bad test");
}


#define UNITS_PER_SHORT (SHORT_TYPE_SIZE / BITS_PER_UNIT)

/* Internal code to generate the load and store of one word/short/byte.
   The load is emitted directly, and the store insn is returned.  */

#if 0
static rtx
block_move_load_store (dest_reg, src_reg, p_bytes, p_offset, align, orig_src)
     rtx src_reg;		/* register holding source memory address */
     rtx dest_reg;		/* register holding dest. memory address */
     int *p_bytes;		/* pointer to # bytes remaining */
     int *p_offset;		/* pointer to current offset */
     int align;			/* alignment */
     rtx orig_src;		/* original source for making a reg note */
{
  int bytes;			/* # bytes remaining */
  int offset;			/* offset to use */
  int size;			/* size in bytes of load/store */
  enum machine_mode mode;	/* mode to use for load/store */
  rtx reg;			/* temporary register */
  rtx src_addr;			/* source address */
  rtx dest_addr;		/* destination address */
  rtx insn;			/* insn of the load */
  rtx orig_src_addr;		/* original source address */
  rtx (*load_func)();		/* function to generate load insn */
  rtx (*store_func)();		/* function to generate destination insn */

  bytes = *p_bytes;
  if (bytes <= 0 || align <= 0)
    abort ();

  if (bytes >= UNITS_PER_WORD && align >= UNITS_PER_WORD)
    {
      mode = SImode;
      size = UNITS_PER_WORD;
      load_func = gen_movsi;
      store_func = gen_movsi;
    }

#if 0
  /* Don't generate unligned moves here, rather defer those to the
     general movestrsi_internal pattern.  */
  else if (bytes >= UNITS_PER_WORD)
    {
      mode = SImode;
      size = UNITS_PER_WORD;
      load_func = gen_movsi_ulw;
      store_func = gen_movsi_usw;
    }
#endif

  else if (bytes >= UNITS_PER_SHORT && align >= UNITS_PER_SHORT)
    {
      mode = HImode;
      size = UNITS_PER_SHORT;
      load_func = gen_movhi;
      store_func = gen_movhi;
    }

  else
    {
      mode = QImode;
      size = 1;
      load_func = gen_movqi;
      store_func = gen_movqi;
    }

  offset = *p_offset;
  *p_offset = offset + size;
  *p_bytes = bytes - size;

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

  reg = gen_reg_rtx (mode);
  insn = emit_insn ((*load_func) (reg, gen_rtx (MEM, mode, src_addr)));
  orig_src_addr = XEXP (orig_src, 0);
  if (CONSTANT_P (orig_src_addr))
    REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_EQUIV,
				plus_constant (orig_src_addr, offset),
				REG_NOTES (insn));

  return (*store_func) (gen_rtx (MEM, mode, dest_addr), reg);
}
#endif


/* Write a series of loads/stores to move some bytes.  Generate load/stores as follows:

   load  1
   load  2
   load  3
   store 1
   load  4
   store 2
   load  5
   store 3
   ...

   This way, no NOP's are needed, except at the end, and only
   two temp registers are needed.  Two delay slots are used
   in deference to the R4000.  */

#if 0
static void
block_move_sequence (dest_reg, src_reg, bytes, align, orig_src)
     rtx dest_reg;		/* register holding destination address */
     rtx src_reg;		/* register holding source address */
     int bytes;			/* # bytes to move */
     int align;			/* max alignment to assume */
     rtx orig_src;		/* original source for making a reg note */
{
  int offset		= 0;
  rtx prev2_store	= (rtx)0;
  rtx prev_store	= (rtx)0;
  rtx cur_store		= (rtx)0;

  while (bytes > 0)
    {
      /* Is there a store to do? */
      if (prev2_store)
	emit_insn (prev2_store);

      prev2_store = prev_store;
      prev_store = cur_store;
      cur_store = block_move_load_store (dest_reg, src_reg,
					 &bytes, &offset,
					 align, orig_src);
    }

  /* Finish up last three stores.  */
  if (prev2_store)
    emit_insn (prev2_store);

  if (prev_store)
    emit_insn (prev_store);

  if (cur_store)
    emit_insn (cur_store);
}
#endif


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
block_move_loop (dest_reg, src_reg, bytes, align, orig_src)
     rtx dest_reg;		/* register holding destination address */
     rtx src_reg;		/* register holding source address */
     int bytes;			/* # bytes to move */
     int align;			/* alignment */
     rtx orig_src;		/* original source for making a reg note */
{
  rtx dest_mem		= gen_rtx (MEM, BLKmode, dest_reg);
  rtx src_mem		= gen_rtx (MEM, BLKmode, src_reg);
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
      emit_insn (gen_movsi (final_src, bytes_rtx));
      emit_insn (gen_addsi3 (final_src, final_src, src_reg));
    }
  else
    emit_insn (gen_addsi3 (final_src, src_reg, bytes_rtx));

  emit_label (label);

  bytes_rtx = GEN_INT (MAX_MOVE_BYTES);
  emit_insn (gen_movstrsi_internal (dest_mem, src_mem, bytes_rtx, align_rtx));
  emit_insn (gen_addsi3 (src_reg, src_reg, bytes_rtx));
  emit_insn (gen_addsi3 (dest_reg, dest_reg, bytes_rtx));
  emit_insn (gen_cmpsi (src_reg, final_src));
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
#ifdef TARGET_MEM_FUNCTIONS
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
		     VOIDmode, 3,
		     dest_reg, Pmode,
		     src_reg, Pmode,
		     bytes_rtx, SImode);
#else
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"), 0,
		     VOIDmode, 3,
		     src_reg, Pmode,
		     dest_reg, Pmode,
		     bytes_rtx, SImode);
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
  rtx src_reg;
  rtx dest_reg;

  if (constp && bytes <= 0)
    return;

  if (align > UNITS_PER_WORD)
    align = UNITS_PER_WORD;

  /* Move the address into scratch registers.  */
  dest_reg = copy_addr_to_reg (XEXP (operands[0], 0));
  src_reg  = copy_addr_to_reg (XEXP (orig_src, 0));

  if (TARGET_MEMCPY)
    block_move_call (dest_reg, src_reg, bytes_rtx);

#if 0
  else if (constp && bytes <= 3*align)
    block_move_sequence (dest_reg, src_reg, bytes, align, orig_src);
#endif

  else if (constp && bytes <= 2*MAX_MOVE_BYTES)
    emit_insn (gen_movstrsi_internal (gen_rtx (MEM, BLKmode, dest_reg),
				      gen_rtx (MEM, BLKmode, src_reg),
				      bytes_rtx, align_rtx));

  else if (constp && align >= UNITS_PER_WORD && optimize)
    block_move_loop (dest_reg, src_reg, bytes, align, orig_src);

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

      emit_insn (gen_iorsi3 (temp, src_reg, dest_reg));
      emit_insn (gen_andsi3 (temp, temp, GEN_INT (UNITS_PER_WORD-1)));
      emit_insn (gen_cmpsi (temp, const0_rtx));
      emit_jump_insn (gen_beq (aligned_label));

      /* Unaligned loop.  */
      block_move_loop (dest_reg, src_reg, bytes, 1, orig_src);
      emit_jump_insn (gen_jump (join_label));
      emit_barrier ();

      /* Aligned loop.  */
      emit_label (aligned_label);
      block_move_loop (dest_reg, src_reg, bytes, UNITS_PER_WORD, orig_src);
      emit_label (join_label);

      /* Bytes at the end of the loop.  */
      if (leftover)
	{
#if 0
	  if (leftover <= 3*align)
	    block_move_sequence (dest_reg, src_reg, leftover, align, orig_src);

	  else
#endif
	    emit_insn (gen_movstrsi_internal (gen_rtx (MEM, BLKmode, dest_reg),
					      gen_rtx (MEM, BLKmode, src_reg),
					      GEN_INT (leftover),
					      GEN_INT (align)));
	}
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
     the same as one of the addressing registers.  */
  for (i = 4; i < last_operand; i++)
    {
      if (reg_mentioned_p (operands[i], operands[0])
	  || reg_mentioned_p (operands[i], operands[1]))
	{
	  abort_with_insn (insn, "register passed as address and temp register to block move");
	}
    }

  /* If we are given global or static addresses, and we would be
     emitting a few instructions, try to save time by using a
     temporary register for the pointer.  */
  if (bytes > 2*align || move_type != BLOCK_MOVE_NORMAL)
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
	      output_asm_insn ("la\t%0,%1", xoperands);
	    }
	}
    }

  if (num_regs > (sizeof (load_store) / sizeof (load_store[0])))
    num_regs = (sizeof (load_store) / sizeof (load_store[0]));

  else if (num_regs < 1)
    abort ();

  if (TARGET_GAS && move_type != BLOCK_MOVE_LAST && set_noreorder++ == 0)
    output_asm_insn (".set\tnoreorder", operands);

  while (bytes > 0)
    {
      load_store[num].offset = offset;

      if (bytes >= UNITS_PER_WORD && align >= UNITS_PER_WORD)
	{
	  load_store[num].load       = "lw\t%0,%1";
	  load_store[num].load_nop   = "lw\t%0,%1%#";
	  load_store[num].store      = "sw\t%0,%1";
	  load_store[num].last_store = "sw\t%0,%1";
	  load_store[num].final      = (char *)0;
	  load_store[num].mode       = SImode;
	  offset += UNITS_PER_WORD;
	  bytes -= UNITS_PER_WORD;
	}

      else if (bytes >= UNITS_PER_WORD)
	{
#if BYTES_BIG_ENDIAN
	  load_store[num].load       = "lwl\t%0,%1\n\tlwr\t%0,%2";
	  load_store[num].load_nop   = "lwl\t%0,%1\n\tlwr\t%0,%2%#";
	  load_store[num].store      = "swl\t%0,%1\n\tswr\t%0,%2";
	  load_store[num].last_store = "swr\t%0,%2";
	  load_store[num].final      = "swl\t%0,%1";
#else
	  load_store[num].load	     = "lwl\t%0,%2\n\tlwr\t%0,%1";
	  load_store[num].load_nop   = "lwl\t%0,%2\n\tlwr\t%0,%1%#";
	  load_store[num].store	     = "swl\t%0,%2\n\tswr\t%0,%1";
	  load_store[num].last_store = "swr\t%0,%1";
	  load_store[num].final      = "swl\t%0,%2";
#endif
	  load_store[num].mode = SImode;
	  offset += UNITS_PER_WORD;
	  bytes -= UNITS_PER_WORD;
	  use_lwl_lwr = TRUE;
	}

      else if (bytes >= UNITS_PER_SHORT && align >= UNITS_PER_SHORT)
	{
	  load_store[num].load	     = "lh\t%0,%1";
	  load_store[num].load_nop   = "lh\t%0,%1%#";
	  load_store[num].store	     = "sh\t%0,%1";
	  load_store[num].last_store = "sh\t%0,%1";
	  load_store[num].final      = (char *)0;
	  load_store[num].offset     = offset;
	  load_store[num].mode	     = HImode;
	  offset += UNITS_PER_SHORT;
	  bytes -= UNITS_PER_SHORT;
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
		    xoperands[2] = gen_rtx (MEM, load_store[i].mode,
					    plus_constant (src_reg, UNITS_PER_WORD-1+offset));

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
		xoperands[2] = gen_rtx (MEM, load_store[i].mode,
					plus_constant (dest_reg, UNITS_PER_WORD-1+offset));

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
	  use_lwl_lwr = FALSE;	/* reset whether or not we used lwl/lwr */
	}
    }

  if (TARGET_GAS && move_type != BLOCK_MOVE_LAST && --set_noreorder == 0)
    output_asm_insn (".set\treorder", operands);

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
    default:
      error ("Illegal mode given to function_arg_advance");
      break;

    case VOIDmode:
      break;

    case BLKmode:
      cum->gp_reg_found = 1;
      cum->arg_words += (int_size_in_bytes (type) + 3) / 4;
      break;

    case SFmode:
      cum->arg_words++;
      break;

    case DFmode:
      cum->arg_words += 2;
      break;

    case DImode:
      cum->gp_reg_found = 1;
      cum->arg_words += 2;
      break;

    case QImode:
    case HImode:
    case SImode:
      cum->gp_reg_found = 1;
      cum->arg_words++;
      break;
    }
}

/* Return a RTL expression containing the register for the given mode,
   or 0 if the argument is too be passed on the stack.  */

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
  int struct_p = ((type != (tree)0)
		  && (TREE_CODE (type) == RECORD_TYPE
		      || TREE_CODE (type) == UNION_TYPE));

  if (TARGET_DEBUG_E_MODE)
    fprintf (stderr,
	     "function_arg( {gp reg found = %d, arg # = %2d, words = %2d}, %4s, 0x%.8x, %d ) = ",
	     cum->gp_reg_found, cum->arg_number, cum->arg_words, GET_MODE_NAME (mode),
	     type, named);

  switch (mode)
    {
    default:
      error ("Illegal mode given to function_arg");
      break;

    case SFmode:
      if (cum->gp_reg_found || cum->arg_number >= 2)
	regbase = GP_ARG_FIRST;
      else {
	regbase = (TARGET_SOFT_FLOAT) ? GP_ARG_FIRST : FP_ARG_FIRST;
	if (cum->arg_words == 1)	/* first arg was float */
	  bias = 1;			/* use correct reg */
      }

      break;

    case DFmode:
      cum->arg_words += (cum->arg_words & 1);
      regbase = (cum->gp_reg_found || TARGET_SOFT_FLOAT)
			? GP_ARG_FIRST
			: FP_ARG_FIRST;
      break;

    case BLKmode:
      if (type != (tree)0 && TYPE_ALIGN (type) > BITS_PER_WORD)
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
      cum->arg_words += (cum->arg_words & 1);
      regbase = GP_ARG_FIRST;
    }

  if (cum->arg_words >= MAX_ARGS_IN_REGISTERS)
    {
      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "<stack>%s\n", struct_p ? ", [struct]" : "");

      ret = (rtx)0;
    }
  else
    {
      if (regbase == -1)
	abort ();

      ret = gen_rtx (REG, mode, regbase + cum->arg_words + bias);

      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "%s%s\n", reg_names[regbase + cum->arg_words + bias],
		 struct_p ? ", [struct]" : "");

      /* The following is a hack in order to pass 1 byte structures
	 the same way that the MIPS compiler does (namely by passing
	 the structure in the high byte or half word of the register).
	 This also makes varargs work.  If we have such a structure,
	 we save the adjustment RTL, and the call define expands will
	 emit them.  For the VOIDmode argument (argument after the
	 last real argument, pass back a parallel vector holding each
	 of the adjustments.  */

      if (struct_p && (mode == QImode || mode == HImode))
	{
	  rtx amount = GEN_INT (BITS_PER_WORD - GET_MODE_BITSIZE (mode));
	  rtx reg = gen_rtx (REG, SImode, regbase + cum->arg_words + bias);
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
  if (mode == BLKmode && cum->arg_words < MAX_ARGS_IN_REGISTERS)
    {
      int words = (int_size_in_bytes (type) + 3) / 4;

      if (words + cum->arg_words < MAX_ARGS_IN_REGISTERS)
	return 0;		/* structure fits in registers */

      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "function_arg_partial_nregs = %d\n",
		 MAX_ARGS_IN_REGISTERS - cum->arg_words);

      return MAX_ARGS_IN_REGISTERS - cum->arg_words;
    }

  else if (mode == DImode && cum->arg_words == MAX_ARGS_IN_REGISTERS-1)
    {
      if (TARGET_DEBUG_E_MODE)
	fprintf (stderr, "function_arg_partial_nregs = 1\n");

      return 1;
    }

  return 0;
}


/* Print the options used in the assembly file.  */

static struct {char *name; int value;} target_switches []
  = TARGET_SWITCHES;

void
print_options (out)
     FILE *out;
{
  int line_len;
  int len;
  int j;
  char **p;
  int mask = TARGET_DEFAULT;

  /* Allow assembly language comparisons with -mdebug eliminating the
     compiler version number and switch lists.  */

  if (TARGET_DEBUG_MODE)
    return;

  fprintf (out, "\n # %s %s", language_string, version_string);
#ifdef TARGET_VERSION_INTERNAL
  TARGET_VERSION_INTERNAL (out);
#endif
#ifdef __GNUC__
  fprintf (out, " compiled by GNU C\n\n");
#else
  fprintf (out, " compiled by CC\n\n");
#endif

  fprintf (out, " # Cc1 defaults:");
  line_len = 32767;
  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    {
      if (target_switches[j].name[0] != '\0'
	  && target_switches[j].value > 0
	  && (target_switches[j].value & mask) == target_switches[j].value)
	{
	  mask &= ~ target_switches[j].value;
	  len = strlen (target_switches[j].name) + 1;
	  if (len + line_len > 79)
	    {
	      line_len = 2;
	      fputs ("\n #", out);
	    }
	  fprintf (out, " -m%s", target_switches[j].name);
	  line_len += len;
	}
    }

  fprintf (out, "\n\n # Cc1 arguments (-G value = %d, Cpu = %s, ISA = %d):",
	   mips_section_threshold, mips_cpu_string, mips_isa);

  line_len = 32767;
  for (p = &save_argv[1]; *p != (char *)0; p++)
    {
      char *arg = *p;
      if (*arg == '-')
	{
	  len = strlen (arg) + 1;
	  if (len + line_len > 79)
	    {
	      line_len = 2;
	      fputs ("\n #", out);
	    }
	  fprintf (out, " %s", *p);
	  line_len += len;
	}
    }

  fputs ("\n\n", out);
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

  if (g_switch_set)
    mips_section_threshold = g_switch_value;

  else
    mips_section_threshold = (TARGET_MIPS_AS) ? 8 : 0;

  /* Identify the processor type */
  if (mips_cpu_string == (char *)0
      || !strcmp (mips_cpu_string, "default")
      || !strcmp (mips_cpu_string, "DEFAULT"))
    {
      mips_cpu_string = "default";
      mips_cpu = PROCESSOR_DEFAULT;
    }

  else
    {
      char *p = mips_cpu_string;

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
	  break;

	case '4':
	  if (!strcmp (p, "4000") || !strcmp (p, "4k") || !strcmp (p, "4K"))
	    mips_cpu = PROCESSOR_R4000;
	  break;

	case '6':
	  if (!strcmp (p, "6000") || !strcmp (p, "6k") || !strcmp (p, "6K"))
	    mips_cpu = PROCESSOR_R6000;
	  break;
	}

      if (mips_cpu == PROCESSOR_DEFAULT)
	{
	  error ("bad value (%s) for -mcpu= switch", mips_cpu_string);
	  mips_cpu_string = "default";
	}
    }

  /* Now get the architectural level.  */
  if (mips_isa_string == (char *)0)
    mips_isa = 1;

  else if (isdigit (*mips_isa_string))
    mips_isa = atoi (mips_isa_string);

  else
    {
      error ("bad value (%s) for -mips switch", mips_isa_string);
      mips_isa = 1;
    }

  if (mips_isa < 0 || mips_isa > 3)
    error ("-mips%d not supported", mips_isa);

  else if (mips_isa > 1
	   && (mips_cpu == PROCESSOR_DEFAULT || mips_cpu == PROCESSOR_R3000))
    error ("-mcpu=%s does not support -mips%d", mips_cpu_string, mips_isa);

  else if (mips_cpu == PROCESSOR_R6000 && mips_isa > 2)
    error ("-mcpu=%s does not support -mips%d", mips_cpu_string, mips_isa);

  /* make sure sizes of ints/longs/etc. are ok */
  if (mips_isa < 3)
    {
      if (TARGET_INT64)
	fatal ("Only the r4000 can support 64 bit ints");

      else if (TARGET_LONG64)
	fatal ("Only the r4000 can support 64 bit longs");

      else if (TARGET_LLONG128)
	fatal ("Only the r4000 can support 128 bit long longs");

      else if (TARGET_FLOAT64)
	fatal ("Only the r4000 can support 64 bit fp registers");
    }
  else if (TARGET_INT64 || TARGET_LONG64 || TARGET_LLONG128 || TARGET_FLOAT64)
    warning ("r4000 64/128 bit types not yet supported");

  /* Tell halfpic.c that we have half-pic code if we do.  */
  if (TARGET_HALF_PIC)
    HALF_PIC_INIT ();

  /* -mrnames says to use the MIPS software convention for register
     names instead of the hardware names (ie, a0 instead of $4).
     We do this by switching the names in mips_reg_names, which the
     reg_names points into via the REGISTER_NAMES macro.  */

  if (TARGET_NAME_REGS)
    {
      if (TARGET_GAS)
	{
	  target_flags &= ~ MASK_NAME_REGS;
	  error ("Gas does not support the MIPS software register name convention.");
	}
      else
	bcopy ((char *) mips_sw_reg_names, (char *) mips_reg_names, sizeof (mips_reg_names));
    }

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

  /* Set up the classification arrays now.  */
  mips_rtx_classify[(int)PLUS]  = CLASS_ADD_OP;
  mips_rtx_classify[(int)MINUS] = CLASS_ADD_OP;
  mips_rtx_classify[(int)DIV]   = CLASS_DIVMOD_OP;
  mips_rtx_classify[(int)MOD]   = CLASS_DIVMOD_OP;
  mips_rtx_classify[(int)UDIV]  = CLASS_DIVMOD_OP | CLASS_UNSIGNED_OP;
  mips_rtx_classify[(int)UMOD]  = CLASS_DIVMOD_OP | CLASS_UNSIGNED_OP;
  mips_rtx_classify[(int)EQ]    = CLASS_CMP_OP | CLASS_EQUALITY_OP | CLASS_FCMP_OP;
  mips_rtx_classify[(int)NE]    = CLASS_CMP_OP | CLASS_EQUALITY_OP | CLASS_FCMP_OP;
  mips_rtx_classify[(int)GT]    = CLASS_CMP_OP | CLASS_FCMP_OP;
  mips_rtx_classify[(int)GE]    = CLASS_CMP_OP | CLASS_FCMP_OP;
  mips_rtx_classify[(int)LT]    = CLASS_CMP_OP | CLASS_FCMP_OP;
  mips_rtx_classify[(int)LE]    = CLASS_CMP_OP | CLASS_FCMP_OP;
  mips_rtx_classify[(int)GTU]   = CLASS_CMP_OP | CLASS_UNSIGNED_OP;
  mips_rtx_classify[(int)GEU]   = CLASS_CMP_OP | CLASS_UNSIGNED_OP;
  mips_rtx_classify[(int)LTU]   = CLASS_CMP_OP | CLASS_UNSIGNED_OP;
  mips_rtx_classify[(int)LEU]   = CLASS_CMP_OP | CLASS_UNSIGNED_OP;

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

  mips_char_to_class['d'] = GR_REGS;
  mips_char_to_class['f'] = ((TARGET_HARD_FLOAT) ? FP_REGS : NO_REGS);
  mips_char_to_class['h'] = HI_REG;
  mips_char_to_class['l'] = LO_REG;
  mips_char_to_class['x'] = MD_REGS;
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

	  if (mode == CC_FPmode || mode == CC_REV_FPmode)
	    temp = (regno == FPSW_REGNUM);

	  else if (GP_REG_P (regno))
	    temp = ((regno & 1) == 0 || (size <= UNITS_PER_WORD));

	  else if (FP_REG_P (regno))
	    temp = ((TARGET_FLOAT64 || ((regno & 1) == 0))
		    && (class == MODE_FLOAT
			|| class == MODE_COMPLEX_FLOAT
			|| (TARGET_DEBUG_H_MODE && class == MODE_INT)));

	  else if (MD_REG_P (regno))
	    temp = (mode == SImode || (regno == MD_REG_FIRST && mode == DImode));

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
   '.'	Print the name of the register with a hard-wired zero (zero or $0).  */

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

	  else if (TARGET_GAS || TARGET_STATS)
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
	abort_with_insn (op, "PRINT_OPERAND, illegal insn for %%C");
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
	abort_with_insn (op, "PRINT_OPERAND, illegal insn for %%N");
      }

  else if (code == REG)
    {
      register int regnum = REGNO (op);

      if (letter == 'M')
	regnum += MOST_SIGNIFICANT_WORD;

      else if (letter == 'L')
	regnum += LEAST_SIGNIFICANT_WORD;

      else if (letter == 'D')
	regnum++;

      fprintf (file, "%s", reg_names[regnum]);
    }

  else if (code == MEM)
    output_address (XEXP (op, 0));

  else if (code == CONST_DOUBLE)
    {
#if HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
      union { double d; int i[2]; } u;
      u.i[0] = CONST_DOUBLE_LOW (op);
      u.i[1] = CONST_DOUBLE_HIGH (op);
      if (GET_MODE (op) == SFmode)
	{
	  float f;
	  f = u.d;
	  u.d = f;
	}
      fprintf (file, "%.20e", u.d);
#else
      fatal ("CONST_DOUBLE found in cross compilation");
#endif
    }

  else if ((letter == 'x') && (GET_CODE(op) == CONST_INT))
    fprintf (file, "0x%04x", 0xffff & (INTVAL(op)));

  else if ((letter == 'X') && (GET_CODE(op) == CONST_INT))
    fprintf (file, "0x%08x", INTVAL(op));

  else if ((letter == 'd') && (GET_CODE(op) == CONST_INT))
    fprintf (file, "%d", (INTVAL(op)));

  else if (letter == 'z'
	   && (GET_CODE (op) == CONST_INT)
	   && INTVAL (op) == 0)
    fputs (reg_names[GP_REG_FIRST], file);

  else if (letter == 'd' || letter == 'x' || letter == 'X')
    fatal ("PRINT_OPERAND: letter %c was found & insn was not CONST_INT", letter);

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
	abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, illegal insn #1");
	break;

      case REG:
	if (REGNO (addr) == ARG_POINTER_REGNUM)
	  abort_with_insn (addr, "Arg pointer not eliminated.");

	fprintf (file, "0(%s)", reg_names [REGNO (addr)]);
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
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, illegal insn #2");

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

   If we have -G 0, or the extern size is unknown, don't bother
   emitting the .externs.  */

int
mips_output_external (file, decl, name)
     FILE *file;
     tree decl;
     char *name;
{
  register struct extern_list *p;
  int len;

  if (TARGET_GP_OPT
      && mips_section_threshold != 0
      && ((TREE_CODE (decl)) != FUNCTION_DECL)
      && ((len = int_size_in_bytes (TREE_TYPE (decl))) > 0))
    {
      p = (struct extern_list *)permalloc ((long) sizeof (struct extern_list));
      p->next = extern_head;
      p->name = name;
      p->size = len;
      extern_head = p;
    }
  return 0;
}


/* Compute a string to use as a temporary file name.  */

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
  temp_filename = (char *) alloca (len + sizeof("/ccXXXXXX"));
  strcpy (temp_filename, base);
  if (len > 0 && temp_filename[len-1] != '/')
    temp_filename[len++] = '/';

  strcpy (temp_filename + len, "ccXXXXXX");
  mktemp (temp_filename);

  stream = fopen (temp_filename, "w+");
  if (!stream)
    pfatal_with_name (temp_filename);

  unlink (temp_filename);
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
      fprintf (stream, "\t.file\t%d \"%s\"\n", num_source_filenames, name);
      if (!TARGET_GAS && write_symbols == DBX_DEBUG)
	fprintf (stream, "\t#@stabs\n");
    }

  else if (!TARGET_GAS && write_symbols == DBX_DEBUG)
    {
      ASM_GENERATE_INTERNAL_LABEL (ltext_label_name, "Ltext", 0);
      fprintf (stream, "%s \"%s\",%d,0,0,%s\n", ASM_STABS_OP,
	       name, N_SOL, &ltext_label_name[1]);
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

	  fprintf (stream, "\t#.file\t%d \"%s\"\n", num_source_filenames, name);
	}

      else
	{
	  SET_FILE_NUMBER ();
	  current_function_file = name;
	  fprintf (stream, "\t.file\t%d \"%s\"\n", num_source_filenames, name);
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
  if (!TARGET_GAS && write_symbols == DBX_DEBUG)
    {
      ++sym_lineno;
      fprintf (stream, "$LM%d:\n\t%s %d,0,%d,$LM%d\n",
	       sym_lineno, ASM_STABN_OP, N_SLINE, line, sym_lineno);
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
	fputs ((set_noreorder) ? "\tnop\n" : "\t#nop\n", asm_out_file);

      else
	dslots_load_filled++;

      while (--dslots_number_nops > 0)
	fputs ((set_noreorder) ? "\tnop\n" : "\t#nop\n", asm_out_file);

      mips_load_reg  = (rtx)0;
      mips_load_reg2 = (rtx)0;
      mips_load_reg3 = (rtx)0;
      mips_load_reg4 = (rtx)0;

      if (set_noreorder && --set_noreorder == 0)
	fputs ("\t.set\treorder\n", asm_out_file);
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

  /* Generate the pseudo ops that the Pyramid based System V.4 wants.  */
  if (TARGET_ABICALLS)
    fprintf (stream, "\t.abicalls\n");

  if (TARGET_GP_OPT)
    {
      asm_out_data_file = stream;
      asm_out_text_file = make_temp_file ();
    }
  else
    asm_out_data_file = asm_out_text_file = stream;

  if (TARGET_NAME_REGS)
    fprintf (asm_out_file, "#include <regdef.h>\n");

  print_options (stream);
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

  if (TARGET_GP_OPT)
    {
      if (extern_head)
	fputs ("\n", file);

      for (p = extern_head; p != 0; p = p->next)
	{
	  name_tree = get_identifier (p->name);

	  /* Positively ensure only one .extern for any given symbol.  */
	  if (! TREE_ASM_WRITTEN (name_tree))
	    {
	      TREE_ASM_WRITTEN (name_tree) = 1;
	      fputs ("\t.extern\t", file);
	      assemble_name (file, p->name);
	      fprintf (file, ", %d\n", p->size);
	    }
	}

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

  if (TARGET_GP_OPT && mips_section_threshold != 0)
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


/* Return TRUE if any register used in the epilogue is used.  This to insure
   any insn put into the epilogue delay slots is safe.  */

int
epilogue_reg_mentioned_p (insn)
     rtx insn;
{
  register char *fmt;
  register int i;
  register enum rtx_code code;
  register int regno;

  if (insn == (rtx)0)
    return 0;

  if (GET_CODE (insn) == LABEL_REF)
    return 0;

  code = GET_CODE (insn);
  switch (code)
    {
    case REG:
      regno = REGNO (insn);
      if (regno == STACK_POINTER_REGNUM)
	return 1;

      if (regno == FRAME_POINTER_REGNUM && frame_pointer_needed)
	return 1;

      if (!call_used_regs[regno])
	return 1;

      if (regno != MIPS_TEMP1_REGNUM && regno != MIPS_TEMP2_REGNUM)
	return 0;

      if (!current_frame_info.initialized)
	compute_frame_size (get_frame_size ());

      return (current_frame_info.total_size >= 32768);

    case SCRATCH:
    case CC0:
    case PC:
    case CONST_INT:
    case CONST_DOUBLE:
      return 0;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (insn, i) - 1; j >= 0; j--)
	    if (epilogue_reg_mentioned_p (XVECEXP (insn, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && epilogue_reg_mentioned_p (XEXP (insn, i)))
	return 1;
    }

  return 0;
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
    SP->|  if not passed.       |   FP->|  if not passed.	|
	+-----------------------+       +-----------------------+
					|			|
					|  GP save for V.4 abi	|
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
					|  local variables	|
					|			|
					+-----------------------+
					|			|
                                        |  alloca allocations   |
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
  if (TARGET_FLOAT64)
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
	  fp_reg_size += 2*UNITS_PER_WORD;
	  fmask |= fp_bits << (regno - FP_REG_FIRST);
	}
    }

  gp_reg_rounded = MIPS_STACK_ALIGN (gp_reg_size);
  total_size += gp_reg_rounded + fp_reg_size;

  if (total_size == extra_size)
    total_size = extra_size = 0;

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
  current_frame_info.num_fp	 = fp_reg_size / (2*UNITS_PER_WORD);

  if (mask)
    {
      unsigned long offset = args_size + var_size + gp_reg_size - UNITS_PER_WORD;
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
      unsigned long offset = args_size + var_size + gp_reg_rounded + fp_reg_size - 2*UNITS_PER_WORD;
      current_frame_info.fp_sp_offset = offset;
      current_frame_info.fp_save_offset = offset - total_size + UNITS_PER_WORD;
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
	    emit_insn (gen_addsi3 (base_reg_rtx, large_reg, stack_pointer_rtx));
	  else
	    fprintf (file, "\taddu\t%s,%s,%s\n",
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
	      emit_move_insn (base_reg_rtx, GEN_INT (gp_offset));
	      emit_insn (gen_addsi3 (base_reg_rtx, base_reg_rtx, stack_pointer_rtx));
	    }
	  else
	    fprintf (file, "\tli\t%s,0x%.08lx\t# %ld\n\taddu\t%s,%s,%s\n",
		     reg_names[MIPS_TEMP2_REGNUM],
		     (long)base_offset,
		     (long)base_offset,
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
		  rtx reg_rtx = gen_rtx (REG, Pmode, regno);
		  rtx mem_rtx = gen_rtx (MEM, Pmode,
					 gen_rtx (PLUS, Pmode, base_reg_rtx,
						  GEN_INT (gp_offset - base_offset)));

		  if (store_p)
		    emit_move_insn (mem_rtx, reg_rtx);
		  else
		    emit_move_insn (reg_rtx, mem_rtx);
		}
	      else
		fprintf (file, "\t%s\t%s,%ld(%s)\n",
			 (store_p) ? "sw" : "lw",
			 reg_names[regno],
			 gp_offset - base_offset,
			 reg_names[REGNO(base_reg_rtx)]);

	      gp_offset -= UNITS_PER_WORD;
	    }
	}
    }
  else
    {
      base_reg_rtx = (rtx)0;		/* Make sure these are initialzed */
      base_offset  = 0;
    }

  /* Save floating point registers if needed.  */
  if (fmask)
    {
      int fp_inc = (TARGET_FLOAT64) ? 1 : 2;

      /* Pick which pointer to use as a base register.  */
      fp_offset  = current_frame_info.fp_sp_offset;
      end_offset = fp_offset - (current_frame_info.fp_reg_size - UNITS_PER_WORD);

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
	    emit_insn (gen_addsi3 (base_reg_rtx, large_reg, stack_pointer_rtx));
	  else
	    fprintf (file, "\taddu\t%s,%s,%s\n",
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
	      emit_move_insn (base_reg_rtx, GEN_INT (fp_offset));
	      emit_insn (gen_addsi3 (base_reg_rtx, base_reg_rtx, stack_pointer_rtx));
	    }
	  else
	    fprintf (file, "\tli\t%s,0x%.08lx\t# %ld\n\taddu\t%s,%s,%s\n",
		     reg_names[MIPS_TEMP2_REGNUM],
		     (long)base_offset,
		     (long)base_offset,
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
		  rtx reg_rtx = gen_rtx (REG, DFmode, regno);
		  rtx mem_rtx = gen_rtx (MEM, DFmode,
					 gen_rtx (PLUS, Pmode, base_reg_rtx,
						  GEN_INT (fp_offset - base_offset)));

		  if (store_p)
		    emit_move_insn (mem_rtx, reg_rtx);
		  else
		    emit_move_insn (reg_rtx, mem_rtx);
		}
	      else
		fprintf (file, "\t%s\t%s,%ld(%s)\n",
			 (store_p) ? "s.d" : "l.d",
			 reg_names[regno],
			 fp_offset - base_offset,
			 reg_names[REGNO(base_reg_rtx)]);


	      fp_offset -= 2*UNITS_PER_WORD;
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
  long tsize = current_frame_info.total_size;

  ASM_OUTPUT_SOURCE_FILENAME (file, DECL_SOURCE_FILE (current_function_decl));

  if (debug_info_level != DINFO_LEVEL_TERSE)
    ASM_OUTPUT_SOURCE_LINE (file, DECL_SOURCE_LINE (current_function_decl));

  inside_function = 1;
  fputs ("\t.ent\t", file);
  assemble_name (file, current_function_name);
  fputs ("\n", file);

  assemble_name (file, current_function_name);
  fputs (":\n", file);

  if (TARGET_ABICALLS)
    fprintf (file,
	     "\t.set\tnoreorder\n\t.cpload\t%s\n\t.set\treorder\n",
	     reg_names[ GP_REG_FIRST + 25 ]);

  tsize = current_frame_info.total_size;
  if (tsize > 0 && TARGET_ABICALLS)
    fprintf (file, "\t.cprestore %d\n", tsize + STARTING_FRAME_OFFSET);

  fprintf (file, "\t.frame\t%s,%d,%s\t\t# vars= %d, regs= %d/%d, args = %d, extra= %d\n",
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
  tree fnargs	 = (TREE_CODE (fntype) != METHOD_TYPE)
			? DECL_ARGUMENTS (fndecl)
			: 0;
  rtx next_arg_reg;
  int i;
  tree next_arg;
  tree cur_arg;
  CUMULATIVE_ARGS args_so_far;

  /* Determine the last argument, and get its name.  */

  INIT_CUMULATIVE_ARGS (args_so_far, fntype, (rtx)0);
  regno = GP_ARG_FIRST;

  for (cur_arg = fnargs; cur_arg != (tree)0; cur_arg = next_arg)
    {
      tree type = DECL_ARG_TYPE (cur_arg);
      enum machine_mode passed_mode = TYPE_MODE (type);
      rtx entry_parm = FUNCTION_ARG (args_so_far,
				     passed_mode,
				     DECL_ARG_TYPE (cur_arg),
				     1);

      if (entry_parm)
	{
	  int words;

	  /* passed in a register, so will get homed automatically */
	  if (GET_MODE (entry_parm) == BLKmode)
	    words = (int_size_in_bytes (type) + 3) / 4;
	  else
	    words = (GET_MODE_SIZE (GET_MODE (entry_parm)) + 3) / 4;

	  regno = REGNO (entry_parm) + words - 1;
	}
      else
	{
	  regno = GP_ARG_LAST+1;
	  break;
	}

      FUNCTION_ARG_ADVANCE (args_so_far,
			    passed_mode,
			    DECL_ARG_TYPE (cur_arg),
			    1);

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

  /* If this function is a varargs function, store any registers that
     would normally hold arguments ($4 - $7) on the stack.  */
  if ((TYPE_ARG_TYPES (fntype) != 0
       && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype))) != void_type_node))
      || (arg_name != (char *)0
	  && ((arg_name[0] == '_' && strcmp (arg_name, "__builtin_va_alist") == 0)
	      || (arg_name[0] == 'v' && strcmp (arg_name, "va_alist") == 0))))
    {
      for (; regno <= GP_ARG_LAST; regno++)
	{
	  rtx ptr = stack_pointer_rtx;
	  if (regno != GP_ARG_FIRST)
	    ptr = gen_rtx (PLUS, Pmode, ptr,
			   GEN_INT ((regno - GP_ARG_FIRST) * UNITS_PER_WORD));

	  emit_move_insn (gen_rtx (MEM, Pmode, ptr), gen_rtx (REG, Pmode, regno));
	}
    }

  tsize = compute_frame_size (get_frame_size ());
  if (tsize > 0)
    {
      rtx tsize_rtx = GEN_INT (tsize);

      if (tsize > 32767)
	{
	  tmp_rtx = gen_rtx (REG, SImode, MIPS_TEMP1_REGNUM);
	  emit_move_insn (tmp_rtx, tsize_rtx);
	  tsize_rtx = tmp_rtx;
	}

      emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx, tsize_rtx));

      save_restore_insns (TRUE, tmp_rtx, tsize, (FILE *)0);

      if (frame_pointer_needed)
	emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */

  if (profile_flag || profile_block_flag)
    emit_insn (gen_blockage ());
}


/* Do any necessary cleanup after a function to restore stack, frame, and regs. */

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  long tsize;
  char *sp_str = reg_names[STACK_POINTER_REGNUM];
  char *t1_str = reg_names[MIPS_TEMP1_REGNUM];
  rtx epilogue_delay = current_function_epilogue_delay_list;
  int noreorder = !TARGET_MIPS_AS || (epilogue_delay != 0);
  int noepilogue = FALSE;
  int load_nop = FALSE;
  int load_only_r31;
  rtx tmp_rtx = (rtx)0;
  rtx restore_rtx;
  int i;

  /* The epilogue does not depend on any registers, but the stack
     registers, so we assume that if we have 1 pending nop, it can be
     ignored, and 2 it must be filled (2 nops occur for integer
     multiply and divide).  */

  if (dslots_number_nops > 0)
    {
      if (dslots_number_nops == 1)
	{
	  dslots_number_nops = 0;
	  dslots_load_filled++;
	}
      else
	{
	  while (--dslots_number_nops > 0)
	    fputs ((set_noreorder) ? "\tnop\n" : "\t#nop\n", asm_out_file);
	}

      if (set_noreorder > 0 && --set_noreorder == 0)
	fputs ("\t.set\treorder\n", file);
    }

  if (set_noat != 0)
    {
      set_noat = 0;
      fputs ("\t.set\tat\n", file);
      error ("internal gcc error: .set noat left on in epilogue");
    }

  if (set_nomacro != 0)
    {
      set_nomacro = 0;
      fputs ("\t.set\tmacro\n", file);
      error ("internal gcc error: .set nomacro left on in epilogue");
    }

  if (set_noreorder != 0)
    {
      set_noreorder = 0;
      fputs ("\t.set\treorder\n", file);
      error ("internal gcc error: .set noreorder left on in epilogue");
    }

  if (set_volatile != 0)
    {
      set_volatile = 0;
      fprintf (file, "\t#.set\tnovolatile\n", (TARGET_MIPS_AS) ? "" : "#");
      error ("internal gcc error: .set volatile left on in epilogue");
    }

  size = MIPS_STACK_ALIGN (size);
  tsize = (!current_frame_info.initialized)
		? compute_frame_size (size)
		: current_frame_info.total_size;

  if (tsize == 0 && epilogue_delay == 0)
    {
      rtx insn = get_last_insn ();

      /* If the last insn was a BARRIER, we don't have to write any code
	 because a jump (aka return) was put there.  */
      if (GET_CODE (insn) == NOTE)
	insn = prev_nonnote_insn (insn);
      if (insn && GET_CODE (insn) == BARRIER)
	noepilogue = TRUE;

      noreorder = FALSE;
    }

  if (!noepilogue)
    {
      /* In the reload sequence, we don't need to fill the load delay
	 slots for most of the loads, also see if we can fill the final
	 delay slot if not otherwise filled by the reload sequence.  */

      if (noreorder)
	fprintf (file, "\t.set\tnoreorder\n");

      if (tsize > 32767)
	{
	  fprintf (file, "\tli\t%s,0x%.08lx\t# %ld\n", t1_str, (long)tsize, (long)tsize);
	  tmp_rtx = gen_rtx (REG, Pmode, MIPS_TEMP1_REGNUM);
	}

      if (frame_pointer_needed)
	fprintf (file, "\tmove\t%s,%s\t\t\t# sp not trusted here\n",
		 sp_str, reg_names[FRAME_POINTER_REGNUM]);

      save_restore_insns (FALSE, tmp_rtx, tsize, file);

      load_only_r31 = (current_frame_info.mask == (1 << 31)
		       && current_frame_info.fmask == 0);

      if (noreorder)
	{
	  /* If the only register saved is the return address, we need a
	     nop, unless we have an instruction to put into it.  Otherwise
	     we don't since reloading multiple registers doesn't reference
	     the register being loaded.  */

	  if (load_only_r31)
	    {
	      if (epilogue_delay)
		  final_scan_insn (XEXP (epilogue_delay, 0),
				   file,
				   1,	 		/* optimize */
				   -2,	 		/* prescan */
				   1);			/* nopeepholes */
	      else
		{
		  fprintf (file, "\tnop\n");
		  load_nop = TRUE;
		}
	    }

	  fprintf (file, "\tj\t%s\n", reg_names[GP_REG_FIRST + 31]);

	  if (tsize > 32767)
	    fprintf (file, "\taddu\t%s,%s,%s\n", sp_str, sp_str, t1_str);

	  else if (tsize > 0)
	    fprintf (file, "\taddu\t%s,%s,%d\n", sp_str, sp_str, tsize);

	  else if (!load_only_r31 && epilogue_delay != 0)
	    final_scan_insn (XEXP (epilogue_delay, 0),
			     file,
			     1, 		/* optimize */
			     -2, 		/* prescan */
			     1);		/* nopeepholes */

	  fprintf (file, "\t.set\treorder\n");
	}

      else
	{
	  if (tsize > 32767)
	    fprintf (file, "\taddu\t%s,%s,%s\n", sp_str, sp_str, t1_str);

	  else if (tsize > 0)
	    fprintf (file, "\taddu\t%s,%s,%d\n", sp_str, sp_str, tsize);

	  fprintf (file, "\tj\t%s\n", reg_names[GP_REG_FIRST + 31]);
	}
    }

  fputs ("\t.end\t", file);
  assemble_name (file, current_function_name);
  fputs ("\n", file);

  if (TARGET_STATS)
    {
      int num_gp_regs = current_frame_info.gp_reg_size / 4;
      int num_fp_regs = current_frame_info.fp_reg_size / 8;
      int num_regs    = num_gp_regs + num_fp_regs;
      char *name      = current_function_name;

      if (name[0] == '*')
	name++;

      dslots_load_total += num_regs;

      if (!noepilogue)
	dslots_jump_total++;

      if (noreorder)
	{
	  dslots_load_filled += num_regs;

	  /* If the only register saved is the return register, we
	     can't fill this register's delay slot.  */

	  if (load_only_r31 && epilogue_delay == 0)
	    dslots_load_filled--;

	  if (tsize > 0 || (!load_only_r31 && epilogue_delay != 0))
	    dslots_jump_filled++;
	}

      fprintf (stderr,
	       "%-20s fp=%c leaf=%c alloca=%c setjmp=%c stack=%4ld arg=%3ld reg=%2d/%d delay=%3d/%3dL %3d/%3dJ refs=%3d/%3d/%3d",
	       name,
	       (frame_pointer_needed) ? 'y' : 'n',
	       ((current_frame_info.mask & (1 << 31)) != 0) ? 'n' : 'y',
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

  if (tsize > 32767)
    {
      tmp_rtx = gen_rtx (REG, SImode, MIPS_TEMP1_REGNUM);
      emit_move_insn (tmp_rtx, tsize_rtx);
      tsize_rtx = tmp_rtx;
    }

  if (tsize > 0)
    {
      if (frame_pointer_needed)
	emit_insn (gen_movsi (stack_pointer_rtx, frame_pointer_rtx));

      save_restore_insns (FALSE, tmp_rtx, tsize, (FILE *)0);

      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, tsize_rtx));
    }

  emit_jump_insn (gen_return_internal (gen_rtx (REG, Pmode, GP_REG_FIRST+31)));
}


/* Define the number of delay slots needed for the function epilogue.

   On the mips, we need a slot if either no stack has been allocated,
   or the only register saved is the return register.  */

int
mips_epilogue_delay_slots ()
{
  if (!current_frame_info.initialized)
    (void) compute_frame_size (get_frame_size ());

  if (current_frame_info.total_size == 0)
    return 1;

  if (current_frame_info.mask == (1 << 31) && current_frame_info.fmask == 0)
    return 1;

  return 0;
}


/* Return true if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

int
simple_epilogue_p ()
{
  if (!reload_completed)
    return 0;

  if (current_frame_info.initialized)
    return current_frame_info.total_size == 0;

  return (compute_frame_size (get_frame_size ())) == 0;
}
