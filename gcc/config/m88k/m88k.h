/* Definitions of target machine for GNU compiler.
   Motorola m88100 in an 88open OCS/BCS environment.
   Copyright (C) 1988, 1989, 1990, 1991 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)
   Enhanced by Michael Meissner (meissner@osf.org)
   Currently supported by Tom Wood (wood@dg-rtp.dg.com)

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

/* The m88100 port of GNU CC adheres to the various standards from 88open.
   These documents are available by writing:

	88open Consortium Ltd.
	100 Homeland Court, Suite 800
	San Jose, CA  95112
	(408) 436-6600

   In brief, the current standards are:

   Binary Compatibility Standard, Release 1.1A, May 1991
	This provides for portability of application-level software at the
	executable level for AT&T System V Release 3.2.

   Object Compatibility Standard, Release 1.1A, May 1991
	This provides for portability of application-level software at the
	object file and library level for C, Fortran, and Cobol, and again,
	largely for SVR3.

   Under development are standards for AT&T System V Release 4, based on the
   [generic] System V Application Binary Interface from AT&T.  These include:

   System V Application Binary Interface, Motorola 88000 Processor Supplement
	Another document from AT&T for SVR4 specific to the m88100.
	Available from Prentice Hall.

   System V Application Binary Interface, Motorola 88000 Processor Supplement,
   Release 1.1, Draft H, May 6, 1991
	A proposed update to the AT&T document from 88open.

   System V ABI Implementation Guide for the M88000 Processor,
   Release 1.0, January 1991
	A companion ABI document from 88open.  */

/* Other m88k*.h files include this one and override certain items.
   At present, these are m88kv3.h, m88kv4.h, m88kdgux.h, and m88kluna.h.
   Additionally, m88kv4.h and m88kdgux.h include svr4.h first.  All other
   m88k targets except m88kluna.h are based on svr3.h.  */

/* Choose SVR3 as the default.  */
#if !defined(DBX_DEBUGGING_INFO) && !defined(DWARF_DEBUGGING_INFO)
#include "svr3.h"
#endif

/* External types used.  */

/* What instructions are needed to manufacture an integer constant.  */
enum m88k_instruction {
  m88k_zero,
  m88k_or,
  m88k_subu,
  m88k_or_lo16,
  m88k_or_lo8,
  m88k_set,
  m88k_oru_hi16,
  m88k_oru_or
};

/* External variables/functions defined in m88k.c.  */

extern char *m88k_pound_sign;
extern char *m88k_short_data;

extern int m88k_gp_threshold;
extern int m88k_prologue_done;
extern int m88k_function_number;
extern int m88k_fp_offset;
extern int m88k_stack_size;
extern int m88k_case_index;

extern struct rtx_def *m88k_compare_reg;
extern struct rtx_def *m88k_compare_op0;
extern struct rtx_def *m88k_compare_op1;

extern int null_epilogue ();
extern int integer_ok_for_set ();
extern int m88k_debugger_offset ();
extern void m88k_handle_pragma_token ();

extern void emit_bcnd ();
extern void expand_block_move ();
extern void check_float_value ();
extern void m88k_layout_frame ();
extern void m88k_output_prologue ();
extern void m88k_output_epilogue ();
extern void output_function_profiler ();
extern void output_function_block_profiler ();
extern void output_block_profiler ();
extern void output_file_start ();
extern void output_ascii ();
extern void output_label ();
extern void print_operand ();
extern void print_operand_address ();

extern char *output_load_const_int ();
extern char *output_load_const_float ();
extern char *output_load_const_double ();
extern char *output_load_const_dimode ();
extern char *output_and ();
extern char *output_ior ();
extern char *output_xor ();
extern char *output_call ();

extern struct rtx_def *emit_test ();
extern struct rtx_def *legitimize_address ();
extern struct rtx_def *legitimize_operand ();
extern struct rtx_def *m88k_function_arg ();
extern struct rtx_def *m88k_builtin_saveregs ();

extern enum m88k_instruction classify_integer ();

/* external variables defined elsewhere in the compiler */

extern int target_flags;			/* -m compiler switches */
extern int frame_pointer_needed;		/* current function has a FP */
extern int current_function_pretend_args_size;	/* args size without ... */
extern int flag_delayed_branch;			/* -fdelayed-branch */
extern int flag_pic;				/* -fpic */
extern char * reg_names[];

/* Specify the default monitors.  The meaning of these values can
   be obtained by doing "grep MONITOR_GCC *m88k*".  Generally, the
   values downward from 0x8000 are tests that will soon go away.
   values upward from 0x1 are generally useful tests that will remain.  */

#ifndef MONITOR_GCC
#define MONITOR_GCC 0
#endif

/*** Controlling the Compilation Driver, `gcc' ***/

/* Some machines may desire to change what optimizations are performed for
   various optimization levels.   This macro, if defined, is executed once
   just after the optimization level is determined and before the remainder
   of the command options have been parsed.  Values set in this macro are
   used as the default values for the other command line options.

   LEVEL is the optimization level specified; 2 if -O2 is specified,
   1 if -O is specified, and 0 if neither is specified.  */

/* This macro used to store 0 in flag_signed_bitfields.
   Not only is that misuse of this macro; the whole idea is wrong.

   The GNU C dialect makes bitfields signed by default,
   regardless of machine type.  Making any machine inconsistent in this
   regard is bad for portability.

   I chose to make bitfields signed by default because this is consistent
   with the way ordinary variables are handled: `int' equals `signed int'.
   If there is a good reason to prefer making bitfields unsigned by default,
   it cannot have anything to do with the choice of machine.
   If the reason is good enough, we should change the convention for all machines.

   -- rms, 20 July 1991.  */

#define OPTIMIZATION_OPTIONS(LEVEL)			\
  do {							\
    if (LEVEL)						\
      {							\
	flag_omit_frame_pointer = 1;			\
      }							\
  } while (0)

/* LIB_SPEC, LINK_SPEC, and STARTFILE_SPEC defined in svr3.h.
   ASM_SPEC, ASM_FINAL_SPEC, LIB_SPEC, LINK_SPEC, and STARTFILE_SPEC redefined
   in svr4.h.
   CPP_SPEC, ASM_SPEC, ASM_FINAL_SPEC, LIB_SPEC, LINK_SPEC, and
   STARTFILE_SPEC redefined in m88kdgux.h.  */

/*** Run-time Target Specification ***/

/* Names to predefine in the preprocessor for this target machine.
   Redefined in m88kv3.h, m88kv4.h, m88kdgux.h, and m88kluna.h.  */
#define CPP_PREDEFINES "-Dm88000 -Dm88k -Dunix -D__CLASSIFY_TYPE__=2"

#define TARGET_VERSION fprintf (stderr, " (%s%s)", \
				VERSION_INFO1, VERSION_INFO2)

/* Print subsidiary information on the compiler version in use.
   Redefined in m88kv4.h, and m88kluna.h.  */
#define VERSION_INFO1	"88open OCS/BCS, "
#define VERSION_INFO2	"12 Feb 1992"
#define VERSION_STRING	version_string
#define	TM_SCCS_ID	"@(#)m88k.h	1.96.5.5 12 Feb 1992 12:59:25"

/* Run-time compilation parameters selecting different hardware subsets.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define MASK_88100		0x00000001 /* Target m88100 */
#define MASK_88110		0x00000002 /* Target m88110 */
#define MASK_OCS_DEBUG_INFO	0x00000004 /* Emit .tdesc info */
#define MASK_OCS_FRAME_POSITION	0x00000008 /* Debug frame = CFA, not r30 */
#define MASK_SVR4		0x00000010 /* Target is AT&T System V.4 */
#define MASK_VERSION_0300	0x00000020 /* Use version 03.00 syntax */
#define MASK_NO_UNDERSCORES	0x00000040 /* Don't emit a leading `_' */
#define MASK_BIG_PIC		0x00000080 /* PIC with large got-rel's -fPIC */
#define MASK_TRAP_LARGE_SHIFT	0x00000100 /* Trap if shift not <= 31 */
#define MASK_HANDLE_LARGE_SHIFT	0x00000200 /* Handle shift count >= 32 */
#define MASK_CHECK_ZERO_DIV	0x00000400 /* Check for int div. by 0 */
#define MASK_USE_DIV		0x00000800 /* No signed div. checks */
#define MASK_IDENTIFY_REVISION	0x00001000 /* Emit ident, with GCC rev */
#define MASK_WARN_PASS_STRUCT	0x00002000 /* Warn about passed structs */
#define MASK_OPTIMIZE_ARG_AREA	0x00004000 /* Save stack space */

#define MASK_88000 (MASK_88100 | MASK_88110)
#define MASK_EITHER_LARGE_SHIFT	(MASK_TRAP_LARGE_SHIFT | \
				 MASK_HANDLE_LARGE_SHIFT)

#define TARGET_88100   		 ((target_flags & MASK_88000) == MASK_88100)
#define TARGET_88110		 ((target_flags & MASK_88000) == MASK_88110)
#define TARGET_88000		 ((target_flags & MASK_88000) == MASK_88000)

#define TARGET_OCS_DEBUG_INFO	  (target_flags & MASK_OCS_DEBUG_INFO)
#define TARGET_OCS_FRAME_POSITION (target_flags & MASK_OCS_FRAME_POSITION)
#define TARGET_SVR4		  (target_flags & MASK_SVR4)
#define TARGET_VERSION_0300	  (target_flags & MASK_VERSION_0300)
#define TARGET_NO_UNDERSCORES	  (target_flags & MASK_NO_UNDERSCORES)
#define TARGET_BIG_PIC		  (target_flags & MASK_BIG_PIC)
#define TARGET_TRAP_LARGE_SHIFT   (target_flags & MASK_TRAP_LARGE_SHIFT)
#define TARGET_HANDLE_LARGE_SHIFT (target_flags & MASK_HANDLE_LARGE_SHIFT)
#define TARGET_CHECK_ZERO_DIV	  (target_flags & MASK_CHECK_ZERO_DIV)
#define	TARGET_USE_DIV		  (target_flags & MASK_USE_DIV)
#define TARGET_IDENTIFY_REVISION  (target_flags & MASK_IDENTIFY_REVISION)
#define TARGET_WARN_PASS_STRUCT   (target_flags & MASK_WARN_PASS_STRUCT)
#define TARGET_OPTIMIZE_ARG_AREA  (target_flags & MASK_OPTIMIZE_ARG_AREA)

#define TARGET_EITHER_LARGE_SHIFT (target_flags & MASK_EITHER_LARGE_SHIFT)

/*  Redefined in m88kv3.h,m88kv4.h, and m88kdgux.h.  */
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV)
#define CPU_DEFAULT MASK_88100

#define TARGET_SWITCHES \
  { \
    { "88110",				 MASK_88110 }, \
    { "88100",				 MASK_88100 }, \
    { "88000",			         MASK_88000 }, \
    { "ocs-debug-info",			 MASK_OCS_DEBUG_INFO }, \
    { "no-ocs-debug-info",		-MASK_OCS_DEBUG_INFO }, \
    { "ocs-frame-position",		 MASK_OCS_FRAME_POSITION }, \
    { "no-ocs-frame-position",		-MASK_OCS_FRAME_POSITION }, \
    { "svr4",			         MASK_SVR4 }, \
    { "svr3",			        -MASK_SVR4 }, \
    { "version-03.00",		         MASK_VERSION_0300 }, \
    { "no-underscores",			 MASK_NO_UNDERSCORES }, \
    { "big-pic",			 MASK_BIG_PIC }, \
    { "trap-large-shift",		 MASK_TRAP_LARGE_SHIFT }, \
    { "handle-large-shift",		 MASK_HANDLE_LARGE_SHIFT }, \
    { "check-zero-division",		 MASK_CHECK_ZERO_DIV }, \
    { "no-check-zero-division",		-MASK_CHECK_ZERO_DIV }, \
    { "use-div-instruction",		 MASK_USE_DIV }, \
    { "identify-revision",		 MASK_IDENTIFY_REVISION }, \
    { "warn-passed-structs",		 MASK_WARN_PASS_STRUCT }, \
    { "optimize-arg-area",		 MASK_OPTIMIZE_ARG_AREA }, \
    { "no-optimize-arg-area",		-MASK_OPTIMIZE_ARG_AREA }, \
    SUBTARGET_SWITCHES \
    /* Default switches */ \
    { "",				 TARGET_DEFAULT }, \
  }

/* Redefined in m88kdgux.h.  */
#define SUBTARGET_SWITCHES

/* Macro to define table for command options with values.  */

#define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }

/* Do any checking or such that is needed after processing the -m switches.  */

#define OVERRIDE_OPTIONS						     \
  do {									     \
    register int i;							     \
									     \
    if ((target_flags & MASK_88000) == 0)				     \
      target_flags |= CPU_DEFAULT;					     \
									     \
    if (TARGET_BIG_PIC)							     \
      flag_pic = 2;							     \
									     \
    if ((target_flags & MASK_EITHER_LARGE_SHIFT) == MASK_EITHER_LARGE_SHIFT) \
      error ("-mtrap-large-shift and -mhandle-large-shift are incompatible");\
									     \
    if (VERSION_0300_SYNTAX)						     \
      {									     \
	for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)			     \
	  reg_names[i]--;						     \
	m88k_pound_sign = "#";						     \
      }									     \
									     \
    if (m88k_short_data)						     \
      {									     \
	char *p = m88k_short_data;					     \
	while (*p)							     \
	  if (*p >= '0' && *p <= '9')					     \
	    p++;							     \
	  else								     \
	    {								     \
	      error ("Invalid option `-mshort-data-%s'", m88k_short_data);   \
	      break;							     \
	    }								     \
	m88k_gp_threshold = atoi (m88k_short_data);			     \
	if (flag_pic)							     \
	  error ("-mshort-data-%s and PIC are incompatible", m88k_short_data); \
      }									     \
  } while (0)

/*** Storage Layout ***/

/* Sizes in bits of the various types.  */
#define CHAR_TYPE_SIZE		 8
#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define	DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   Somewhat arbitrary.  It matches the bit field patterns.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.
   That is true on the m88000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.
   For the m88000 we can decide arbitrarily since there are no machine
   instructions for them.  */
#define WORDS_BIG_ENDIAN 1

/* Number of bits in an addressible storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Largest alignment for stack parameters (if greater than PARM_BOUNDARY).  */
#define MAX_PARM_BOUNDARY 64

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 128

/* Allocation boundary (in *bits*) for the code of a function.
   Pack code tightly when compiling crtstuff.c.  */
#define FUNCTION_BOUNDARY (flag_inhibit_size_directive ? 32 : 128)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Alignment of field after `int : 0' in a structure.
   Ignored with PCC_BITFIELD_TYPE_MATTERS.  */
/* #define EMPTY_FIELD_BOUNDARY 8 */

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Maximum size (in bits) to use for the largest integral type that
   replaces a BLKmode type. */
/* #define MAX_FIXED_MODE_SIZE 0 */

/* Report errors on floating point, if we are given NaN's, or such.  Leave
   the number as is, though, since we output the number in hex, and the
   assemble won't choak on it.  */
#define CHECK_FLOAT_VALUE(MODE,VALUE) check_float_value (MODE, VALUE)

/* A code distinguishing the floating point format of the target machine.  */
/* #define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT */

/*** Register Usage ***/

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   The m88100 has 32 fullword registers.

   The pseudo argument pointer is said to be register 0.  This prohibits
   the use of r0 as a general register and causes no trouble.
   Using register 0 is useful, in that it keeps the number of
   registers down to 32, and GNU can use a long as a bitmask
   for the registers.  */
#define FIRST_PSEUDO_REGISTER 32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   Registers 14-25 are expected to be preserved across
   function calls.

   On the 88000, these are:
   Reg 0	= Pseudo argument pointer (hardware fixed to 0).
   Reg 1	= Subroutine return pointer (hardware).
   Reg 2-9	= Parameter registers (OCS).
   Reg 10	= OCS reserved temporary.
   Reg 11	= Static link if needed [OCS reserved temporary].
   Reg 12	= Address of structure return (OCS).
   Reg 13	= OCS reserved temporary.
   Reg 14-25	= Preserved register set.
   Reg 26-29	= Reserved by OCS and ABI.
   Reg 30	= Frame pointer (Common use).
   Reg 31	= Stack pointer.  */

#define FIXED_REGISTERS \
 {1, 1, 0, 0,  0, 0, 0, 0,   0, 0, 0, 0,  0, 0, 0, 0, \
  0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 1, 1,  1, 1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS \
 {1, 1, 1, 1,  1, 1, 1, 1,   1, 1, 1, 1,  1, 1, 0, 0, \
  0, 0, 0, 0,  0, 0, 0, 0,   0, 0, 1, 1,  1, 1, 1, 1}

/* Macro to conditionally modify fixed_regs/call_used_regs.  */
#define CONDITIONAL_REGISTER_USAGE			\
  {							\
    if (flag_pic)					\
      {							\
	fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
	call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
	global_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
      }							\
  }

/* These interfaces that don't apply to the m88000.  */
/* OVERLAPPING_REGNO_P(REGNO) 0 */
/* INSN_CLOBBERS_REGNO_P(INSN, REGNO) 0 */
/* PRESERVE_DEATH_INFO_REGNO_P(REGNO) 0 */

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the m88000, ordinary registers hold 32 bits worth;
   a single floating point register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.

   For double integers, we never put the value into an odd register so that
   the operators don't run into the situation where the high part of one of
   the inputs is the low part of the result register (it's ok if the output
   registers are the same as the input registers.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)	\
  (((MODE) != DImode && (MODE) != DFmode && (MODE) != DCmode) ||	\
   ((REGNO) & 1) == 0)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (((MODE1) == DFmode || (MODE1) == DCmode || (MODE1) == DImode) \
   == ((MODE2) == DFmode || (MODE2) == DCmode || (MODE2) == DImode))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* the m88000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 31

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 30

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 0

/* Register used in cases where a temporary is known to be safe to use.  */
#define TEMP_REGNUM 10

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 11

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 12

/* Register to hold the addressing base for position independent
   code access to data items.  */
#define PIC_OFFSET_TABLE_REGNUM 25

/* Order in which registers are preferred (most to least).  Use temp
   registers, then param registers top down.  Preserve registers are
   top down to maximize use of double memory ops for register save.
   The 88open reserved registers (26-29) may commonly be used in most
   environments with the -fcall-used- or -fcall-saved- options.  */
#define REG_ALLOC_ORDER \
 {13, 12, 11, 10, 29, 28, 27, 26, \
   1,  9,  8,  7,  6,  5,  4,  3, \
   2, 25, 24, 23, 22, 21, 20, 19, \
  18, 17, 16, 15, 14, 30, 31, 0}

/*** Register Classes ***/

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

/* The m88100 hardware has one kind of register.  However, we denote
   the arg pointer as a separate class.  */

enum reg_class { NO_REGS, AP_REG, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES {"NO_REGS", "AP_REG", "GENERAL_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */
#define REG_CLASS_CONTENTS {0, 1, -2, -1}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
#define REGNO_REG_CLASS(REGNO) ((REGNO) ? GENERAL_REGS : AP_REG)

/* The class value for index registers, and the one for base regs.  */
#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) NO_REGS

/* Macros to check register numbers against specific register classes.
   These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < FIRST_PSEUDO_REGISTER || \
   (unsigned) reg_renumber[REGNO] < FIRST_PSEUDO_REGISTER)
#define REGNO_OK_FOR_INDEX_P(REGNO) \
  (((REGNO) && (REGNO) < FIRST_PSEUDO_REGISTER) || \
   (unsigned) reg_renumber[REGNO] < FIRST_PSEUDO_REGISTER)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   Double constants should be in a register iff they can be made cheaply.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Letters in the range `I' through `P' in a register constraint string can
   be used to stand for particular ranges of immediate operands.  The C
   expression is true iff C is a known letter and VALUE is appropriate for
   that letter.

   For the m88000,  the following contraints are used:
   `I' requires a non-negative 16-bit value.
   `J' requires a non-positive 16-bit value.
   `K' is unused.
   `L' requires a constant with only the upper 16-bits set.
   `M' requires constant values that can be formed with `set'.
   `N' requires a negative value.
   `O' requires zero.
   `P' requires a non-negative value.  */

/* Quick tests for certain values.  */
#define SMALL_INT(X) (SMALL_INTVAL (INTVAL (X)))
#define SMALL_INTVAL(I) ((unsigned) (I) < 0x10000)
#define ADD_INT(X) (ADD_INTVAL (INTVAL (X)))
#define ADD_INTVAL(I) ((unsigned) (I) + 0xffff < 0x1ffff)
#define POWER_OF_2(I) ((I) && POWER_OF_2_or_0(I))
#define POWER_OF_2_or_0(I) (((I) & ((unsigned)(I) - 1)) == 0)

#define CONST_OK_FOR_LETTER_P(VALUE, C)			\
  ((C) == 'I' ? SMALL_INTVAL (VALUE)			\
   : (C) == 'J' ? SMALL_INTVAL (-(VALUE))		\
   : (C) == 'L' ? ((VALUE) & 0xffff) == 0		\
   : (C) == 'M' ? integer_ok_for_set (VALUE)		\
   : (C) == 'N' ? (VALUE) < 0				\
   : (C) == 'O' ? (VALUE) == 0				\
   : (C) == 'P' ? (VALUE) >= 0				\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  For the m88000, the
   constraints are:  `G' requires zero, and `H' requires one or two.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'G' ? (CONST_DOUBLE_HIGH (VALUE) == 0				\
		 && CONST_DOUBLE_LOW (VALUE) == 0)			\
   : 0)

/* Letters in the range `Q' through `U' in a register constraint string
   may be defined in a machine-dependent fashion to stand for arbitrary
   operand types.

   For the m88k, `Q' handles addresses in a call context.  */

#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q' ? symbolic_address_p (OP) : 0)

/*** Describing Stack Layout ***/

/* Define this if pushing a word on the stack moves the stack pointer
   to a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the addresses of local variable slots are at negative
   offsets from the frame pointer.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset from the frame pointer to the first local variable slot to be
   allocated. For the m88k, the debugger wants the return address (r1)
   stored at location r30+4, and the previous frame pointer stored at
   location r30.  */
#define STARTING_FRAME_OFFSET 8

/* If we generate an insn to push BYTES bytes, this says how many the
   stack pointer really advances by.  The m88k has no push instruction.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  Use the default value zero.  */
/* #define STACK_POINTER_OFFSET 0 */

/* Offset of first parameter from the argument pointer register value.
   Using an argument pointer, this is 0 for the m88k.  GCC knows
   how to eliminate the argument pointer references if necessary.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define this if functions should assume that stack space has been
   allocated for arguments even when their values are passed in
   registers.

   The value of this macro is the size, in bytes, of the area reserved for
   arguments passed in registers.

   This space can either be allocated by the caller or be a part of the
   machine-dependent stack frame: `OUTGOING_REG_PARM_STACK_SPACE'
   says which.  */
#define REG_PARM_STACK_SPACE(FNDECL) 32

/* Define this macro if REG_PARM_STACK_SPACE is defined but stack
   parameters don't skip the area specified by REG_PARM_STACK_SPACE.
   Normally, when a parameter is not passed in registers, it is placed on
   the stack beyond the REG_PARM_STACK_SPACE area.  Defining this macro
   suppresses this behavior and causes the parameter to be passed on the
   stack in its natural location.  */
#define STACK_PARMS_IN_REG_PARM_AREA

/* Define this if it is the responsibility of the caller to allocate the
   area reserved for arguments passed in registers.  If
   `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect of this
   macro is to determine whether the space is included in
   `current_function_outgoing_args_size'.  */
/* #define OUTGOING_REG_PARM_STACK_SPACE */

/* Offset from the stack pointer register to an item dynamically allocated
   on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.  */
/* #define STACK_DYNAMIC_OFFSET(FUNDECL) ... */

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */
#define RETURN_POPS_ARGS(FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx (REG, \
	   TYPE_MODE (VALTYPE) == BLKmode ? SImode : TYPE_MODE (VALTYPE), \
	   2)

/* Define this if it differs from FUNCTION_VALUE.  */
/* #define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC) ... */

/* Disable the promotion of some structures and unions to registers. */
#define RETURN_IN_MEMORY(TYPE) \
  ((TREE_CODE (TYPE) == RECORD_TYPE || TREE_CODE(TYPE) == UNION_TYPE) \
   && !(TYPE_MODE (TYPE) == SImode \
	|| (TYPE_MODE (TYPE) == BLKmode \
	    && TYPE_ALIGN (TYPE) == BITS_PER_WORD \
	    && int_size_in_bytes (TYPE) == UNITS_PER_WORD)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 2)

/* True if N is a possible register number for a function value
   as seen by the caller.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 2)

/* Determine whether a function argument is passed in a register, and
   which register.  See m88k.c.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  m88k_function_arg (CUM, MODE, TYPE, NAMED)

/* Define this if it differs from FUNCTION_ARG.  */
/* #define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) ... */

/* A C expression for the number of words, at the beginning of an
   argument, must be put in registers.  The value must be zero for
   arguments that are passed entirely in registers or that are entirely
   pushed on the stack.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) (0)

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of the
   argument itself.  The pointer is passed in whatever way is appropriate
   for passing a pointer to that type.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) (0)

/* A C type for declaring a variable that is used as the first argument
   of `FUNCTION_ARG' and other related values.  It suffices to count
   the number of words of argument so far.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS for a call to a
   function whose data type is FNTYPE.  For a library call, FNTYPE is 0. */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME) ((CUM) = 0)

/* A C statement (sans semicolon) to update the summarizer variable
   CUM to advance past an argument in the argument list.  The values
   MODE, TYPE and NAMED describe that argument.  Once this is done,
   the variable CUM is suitable for analyzing the *following* argument
   with `FUNCTION_ARG', etc.  (TYPE is null for libcalls where that
   information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  do {									\
    enum machine_mode __mode = (TYPE) ? TYPE_MODE (TYPE) : (MODE);	\
    if ((CUM & 1)							\
	&& (__mode == DImode || __mode == DFmode			\
	    || ((TYPE) && TYPE_ALIGN (TYPE) > BITS_PER_WORD)))		\
      CUM++;								\
    CUM += (((__mode != BLKmode)					\
	     ? GET_MODE_SIZE (MODE) : int_size_in_bytes (TYPE))		\
	    + 3) / 4;							\
  } while (0)

/* True if N is a possible register number for function argument passing.
   On the m88000, these are registers 2 through 9.  */
#define FUNCTION_ARG_REGNO_P(N) ((N) <= 9 && (N) >= 2)

/* A C expression which determines whether, and in which direction,
   to pad out an argument with extra space.  The value should be of
   type `enum direction': either `upward' to pad above the argument,
   `downward' to pad below, or `none' to inhibit padding.

   This macro does not control the *amount* of padding; that is always
   just enough to reach the next multiple of `FUNCTION_ARG_BOUNDARY'.  */
#define FUNCTION_ARG_PADDING(MODE, TYPE) \
  ((MODE) == BLKmode \
   || ((TYPE) && (TREE_CODE (TYPE) == RECORD_TYPE \
		  || TREE_CODE (TYPE) == UNION_TYPE)) \
   ? upward : GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY ? downward : none)

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  (((TYPE) ? TYPE_ALIGN (TYPE) : GET_MODE_SIZE (MODE)) <= PARM_BOUNDARY \
    ? PARM_BOUNDARY : 2 * PARM_BOUNDARY)

/* Generate necessary RTL for __builtin_saveregs().
   ARGLIST is the argument list; see expr.c.  */
#define EXPAND_BUILTIN_SAVEREGS(ARGLIST) m88k_builtin_saveregs (ARGLIST)

/* Generate the assembly code for function entry. */
#define FUNCTION_PROLOGUE(FILE, SIZE) m88k_output_prologue(FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  Redefined in m88kv3.h, m88kv4.h and
   m88kdgux.h.  */
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "mcount", 1)

/* Output assembler code to FILE to initialize basic-block profiling for
   the current module.  LABELNO is unique to each instance.  */
#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO) \
  output_function_block_profiler (FILE, LABELNO)

/* Output assembler code to FILE to increment the count associated with
   the basic block number BLOCKNO.  */
#define BLOCK_PROFILER(FILE, BLOCKNO) output_block_profiler (FILE, BLOCKNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK (1)

/* Generate the assembly code for function exit. */
#define FUNCTION_EPILOGUE(FILE, SIZE) m88k_output_epilogue(FILE, SIZE)

/* Define the number of delay slots needed for the function epilogue.
   These are used for scheduling the function epilogue and depend on
   what the epilogue looks like.  */
#define DELAY_SLOTS_FOR_EPILOGUE delay_slots_for_epilogue ()

/* Define whether INSN can be placed in delay slot N for the epilogue.  */
#define ELIGIBLE_FOR_EPILOGUE_DELAY(INSN,N) \
  eligible_for_epilogue_delay (INSN)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED \
  (frame_pointer_needed \
   || (write_symbols != NO_DEBUG && !TARGET_OCS_FRAME_POSITION))

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the m88k.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS				\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* Given FROM and TO register numbers, say whether this elimination
   is allowed.  */
#define CAN_ELIMINATE(FROM, TO) \
  (!((FROM) == FRAME_POINTER_REGNUM && FRAME_POINTER_REQUIRED))

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			 \
{ m88k_layout_frame ();							 \
  if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	 \
      (OFFSET) = m88k_fp_offset;					 \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM) \
    (OFFSET) = m88k_stack_size - m88k_fp_offset;			 \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
    (OFFSET) = m88k_stack_size;						 \
  else									 \
    abort ();								 \
}

/*** Trampolines for Nested Functions ***/

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   This block is placed on the stack and filled in.  It is aligned
   0 mod 128 and those portions that are executed are constant.
   This should work for instruction caches that have cache lines up
   to the aligned amount (128 is arbitrary), provided no other code
   producer is attempting to play the same game.  This of course is
   in violation of any number of 88open standards.  */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  /* Save the return address (r1) in the static chain reg (r11).  */	\
  fprintf (FILE, "\tor\t %s,%s,0\n", reg_names[11], reg_names[1]);	\
  /* Locate this block; transfer to the next instruction.  */		\
  fprintf (FILE, "\tbsr\t 1\n");					\
  /* Save r10; use it as the relative pointer; restore r1.  */		\
  fprintf (FILE, "\tst\t %s,%s,24\n", reg_names[10], reg_names[1]);	\
  fprintf (FILE, "\tor\t %s,%s,0\n", reg_names[10], reg_names[1]);	\
  fprintf (FILE, "\tor\t %s,%s,0\n", reg_names[1], reg_names[11]);	\
  /* Load the function's address and go there.  */			\
  fprintf (FILE, "\tld\t %s,%s,32\n", reg_names[11], reg_names[10]);	\
  fprintf (FILE, "\tjmp.n\t %s\n", reg_names[11]);			\
  /* Restore r10 and load the static chain register.  */		\
  fprintf (FILE, "\tld.d\t %s,%s,24\n", reg_names[10], reg_names[10]);	\
  /* Storage: r10 save area, static chain, function address.  */	\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
}

/* Length in units of the trampoline for entering a nested function.
   This is really two components.  The first 32 bytes are fixed and
   must be copied; the last 12 bytes are just storage that's filled
   in later.  So for allocation purposes, it's 32+12 bytes, but for
   initializaiton purposes, it's 32 bytes.  */

#define TRAMPOLINE_SIZE (32+12)

/* Alignment required for a trampoline.  128 is used to find the
   beginning of a line in the instruction cache and to allow for
   instruction cache lines of up to 128 bytes.  */

#define TRAMPOLINE_ALIGNMENT 128

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 40)), FNADDR); \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 36)), CXT); \
}

/*** Library Subroutine Names ***/

/* Define this macro if GNU CC should generate calls to the System V
   (and ANSI C) library functions `memcpy' and `memset' rather than
   the BSD functions `bcopy' and `bzero'.  */
#define TARGET_MEM_FUNCTIONS

/*** Addressing Modes ***/

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) (CONSTANT_P (X))

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 2

/* The condition for memory shift insns.  */
#define SCALED_ADDRESS_P(ADDR)			\
  (GET_CODE (ADDR) == PLUS			\
   && (GET_CODE (XEXP (ADDR, 0)) == MULT	\
       || GET_CODE (XEXP (ADDR, 1)) == MULT))

/* Can the reference to X be made short?  */
#define SHORT_ADDRESS_P(X,TEMP) \
  ((TEMP) = (GET_CODE (X) == CONST ? get_related_value (X) : X), \
   ((TEMP) && GET_CODE (TEMP) == SYMBOL_REF && SYMBOL_REF_FLAG (TEMP)))

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On the m88000, a legitimate address has the form REG, REG+REG,
   REG+SMALLINT, REG+(REG*modesize) (REG[REG]), or SMALLINT.

   The register elimination process should deal with the argument
   pointer and frame pointer changing to REG+SMALLINT.  */

#define LEGITIMATE_INDEX_P(X, MODE)			\
   ((GET_CODE (X) == CONST_INT				\
     && SMALL_INT (X))					\
    || (REG_P (X)					\
	&& REG_OK_FOR_INDEX_P (X))			\
    || (GET_CODE (X) == MULT				\
	&& REG_P (XEXP (X, 0))				\
	&& REG_OK_FOR_INDEX_P (XEXP (X, 0))		\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& INTVAL (XEXP (X, 1)) == GET_MODE_SIZE (MODE)))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{							\
  register rtx _x;					\
  if (REG_P (X))					\
    {							\
      if (REG_OK_FOR_BASE_P (X))			\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      register rtx _x0 = XEXP (X, 0);			\
      register rtx _x1 = XEXP (X, 1);			\
      if ((flag_pic					\
	   && _x0 == pic_offset_table_rtx		\
	   && (flag_pic == 2				\
	       ? REG_P (_x1)				\
	       : (GET_CODE (_x1) == SYMBOL_REF		\
		  || GET_CODE (_x1) == LABEL_REF)))	\
	  || (REG_P (_x0)				\
	      && (REG_OK_FOR_BASE_P (_x0)		\
		  && LEGITIMATE_INDEX_P (_x1, MODE)))	\
	  || (REG_P (_x1)				\
	      && (REG_OK_FOR_BASE_P (_x1)		\
		  && LEGITIMATE_INDEX_P (_x0, MODE))))	\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == LO_SUM)			\
    {							\
      register rtx _x0 = XEXP (X, 0);			\
      register rtx _x1 = XEXP (X, 1);			\
      if (((REG_P (_x0)					\
	    && REG_OK_FOR_BASE_P (_x0))			\
	   || (GET_CODE (_x0) == SUBREG			\
	       && REG_P (SUBREG_REG (_x0))		\
	       && REG_OK_FOR_BASE_P (SUBREG_REG (_x0)))) \
	  && CONSTANT_P (_x1))				\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == CONST_INT			\
	   && SMALL_INT (X))				\
    goto ADDR;						\
  else if (SHORT_ADDRESS_P (X, _x))			\
    goto ADDR;						\
}

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  Not the argument pointer.  */
#define REG_OK_FOR_INDEX_P(X) (X)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (1)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

/* On the m88000, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{								\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   copy_to_mode_reg (SImode, XEXP (X, 1)));	\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   copy_to_mode_reg (SImode, XEXP (X, 0)));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == CONST	\
	   || GET_CODE (X) == LABEL_REF)			\
    (X) = legitimize_address (flag_pic, X, gen_reg_rtx (Pmode)); \
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the the m88000 this is never true.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
#define LEGITIMATE_CONSTANT_P(X) (1)

/*** Condition Code Information ***/

/* C code for a data type which is used for declaring the `mdep'
   component of `cc_status'.  It defaults to `int'.  */
/* #define CC_STATUS_MDEP int */

/* A C expression to initialize the `mdep' field to "empty".  */
/* #define CC_STATUS_MDEP_INIT (cc_status.mdep = 0) */

/* Macro to zap the normal portions of CC_STATUS, but leave the
   machine dependent parts (ie, literal synthesis) alone.  */
/* #define CC_STATUS_INIT_NO_MDEP \
  (cc_status.flags = 0, cc_status.value1 = 0, cc_status.value2 = 0) */

/* When using a register to hold the condition codes, the cc_status
   mechanism cannot be used.  */
#define NOTICE_UPDATE_CC(EXP, INSN) (0)

/*** Miscellaneous Parameters ***/

/* Define the codes that are matched by predicates in m88k.c.  */
#define PREDICATE_CODES	  						\
  {"move_operand", {SUBREG, REG, CONST_INT, LO_SUM, MEM}},		\
  {"call_address_operand", {SUBREG, REG, SYMBOL_REF, LABEL_REF, CONST}}, \
  {"arith_operand", {SUBREG, REG, CONST_INT}},				\
  {"arith5_operand", {SUBREG, REG, CONST_INT}},				\
  {"arith32_operand", {SUBREG, REG, CONST_INT}},			\
  {"arith64_operand", {SUBREG, REG, CONST_INT}},			\
  {"int5_operand", {CONST_INT}},					\
  {"int32_operand", {CONST_INT}},					\
  {"add_operand", {SUBREG, REG, CONST_INT}},				\
  {"reg_or_bbx_mask_operand", {SUBREG, REG, CONST_INT}},		\
  {"real_or_0_operand", {SUBREG, REG, CONST_DOUBLE}},			\
  {"relop", {EQ, NE, LT, LE, GE, GT, LTU, LEU, GEU, GTU}},		\
  {"relop_no_unsigned", {EQ, NE, LT, LE, GE, GT}},			\
  {"equality_op", {EQ, NE}},						\
  {"pc_or_label_ref", {PC, LABEL_REF}},

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define this macro if jump-tables should contain relative addresses.  */
#define CASE_VECTOR_PC_RELATIVE

/* Define this if control falls through a `case' insn when the index
   value is out of range.  This means the specified default-label is
   actually ignored by the `case' insn proper.  */
/* #define CASE_DROPS_THROUGH */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* The 88open ABI says size_t is unsigned int.  */
#define SIZE_TYPE "unsigned int"

/* Allow and ignore #sccs directives */
#define SCCS_DIRECTIVE

/* Code to handle #pragma directives.  The interface is a bit messy,
   but there's no simpler way to do this while still using yylex.  */
#define HANDLE_PRAGMA(FILE)					\
  do {								\
    while (c == ' ' || c == '\t')				\
      c = getc (FILE);						\
    if (c == '\n' || c == EOF)					\
      {								\
	m88k_handle_pragma_token (0, 0);			\
	return c;						\
      }								\
    ungetc (c, FILE);						\
    switch (yylex ())						\
      {								\
      case IDENTIFIER:						\
      case TYPENAME:						\
      case STRING:						\
      case CONSTANT:						\
	m88k_handle_pragma_token (token_buffer, yylval.ttype);	\
	break;							\
      default:							\
	m88k_handle_pragma_token (token_buffer, 0);		\
      }								\
    if (nextchar >= 0)						\
      c = nextchar, nextchar = -1;				\
    else							\
      c = getc (FILE);						\
  } while (1)

/* Tell when to handle #pragma weak.  This is only done for V.4.  */
#define HANDLE_PRAGMA_WEAK TARGET_SVR4

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 64

/* Define if normal loads of shorter-than-word items from memory clears
   the rest of the bigs in the register.  */
#define BYTE_LOADS_ZERO_EXTEND

/* Zero if access to memory by bytes is faster.  */
#define SLOW_BYTE_ACCESS 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Define this macro if an argument declared as `char' or
   `short' in a prototype should actually be passed as an
   `int'.  In addition to avoiding errors in certain cases of
   mismatch, it also makes for better code on certain machines.  */
#define PROMOTE_PROTOTYPES

/* Define this macro if a float function always returns float
   (even in traditional mode).  Redefined in m88kluna.h.  */
#define TRADITIONAL_RETURN_FLOAT

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE -1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a word address (for indexing purposes)
   so give the MEM rtx word mode.  */
#define FUNCTION_MODE SImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.

   We assume that any 16 bit integer can easily be recreated, so we
   indicate 0 cost, in an attempt to get GCC not to optimize things
   like comparison against a constant.

   The cost of CONST_DOUBLE is zero (if it can be placed in an insn, it
   is as good as a register; since it can't be placed in any insn, it
   won't do anything in cse, but it will cause expand_binop to pass the
   constant to the define_expands).  */
#define CONST_COSTS(RTX,CODE)				\
  case CONST_INT:					\
    if (SMALL_INT (RTX))				\
      return 0;						\
    else if (SMALL_INTVAL (- INTVAL (RTX)))		\
      return 2;						\
    else if (classify_integer (SImode, INTVAL (RTX)) != m88k_oru_or) \
      return 4;						\
    return 7;						\
  case HIGH:						\
    return 2;						\
  case CONST:						\
  case LABEL_REF:					\
  case SYMBOL_REF:					\
    if (flag_pic)					\
      return (flag_pic == 2) ? 11 : 8;			\
    return 5;						\
  case CONST_DOUBLE:					\
    return 0;

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, it's cost is irrelavent.
   REG+REG is made slightly more expensive because it might keep
   a register live for longer than we might like.  */
#define ADDRESS_COST(ADDR)				\
  (GET_CODE (ADDR) == REG ? 1 :				\
   GET_CODE (ADDR) == LO_SUM ? 1 :			\
   GET_CODE (ADDR) == HIGH ? 2 :			\
   GET_CODE (ADDR) == MULT ? 1 :			\
   GET_CODE (ADDR) != PLUS ? 4 :			\
   (REG_P (XEXP (ADDR, 0)) && REG_P (XEXP (ADDR, 1))) ? 2 : 1)

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */
#define RTX_COSTS(X,CODE)				\
  case MEM:						\
    return COSTS_N_INSNS (2);				\
  case MULT:						\
    return COSTS_N_INSNS (3);				\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (38);

/* A C expressions returning the cost of moving data of MODE from a register
   to or from memory.  This is more costly than between registers.  */
#define MEMORY_MOVE_COST(MODE) 4

/* Provide the cost of a branch.  Exact meaning under development.  */
#define BRANCH_COST (TARGET_88100 ? 1 : 2)

/* Define this to be nonzero if the character `$' should be allowed
   by default in identifier names.  */
#define	DOLLARS_IN_IDENTIFIERS	1

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/*** Output of Assembler Code ***/

/* Control the assembler format that we output.  */

/* Which assembler syntax.  Redefined in m88kdgux.h.  */
#define VERSION_0300_SYNTAX TARGET_SVR4

/* Allow pseudo-ops to be overridden.  Override these in svr[34].h.  */
#undef	INT_ASM_OP
#undef	ASCII_DATA_ASM_OP
#undef	INIT_SECTION_ASM_OP
#undef	CONST_SECTION_ASM_OP
#undef	CTORS_SECTION_ASM_OP
#undef	DTORS_SECTION_ASM_OP
#undef	INIT_SECTION_ASM_OP
#undef	FINI_SECTION_ASM_OP
#undef	TYPE_ASM_OP
#undef	SIZE_ASM_OP

/* These are used in varasm.c as well.  */
#define TEXT_SECTION_ASM_OP	"\ttext"
#define DATA_SECTION_ASM_OP	"\tdata"

/* Other sections.  */
#define CONST_SECTION_ASM_OP (VERSION_0300_SYNTAX		\
			      ? "\tsection\t .rodata,\"a\"\n"	\
			      : "\tsection\t .rodata,\"x\"\n")
#define TDESC_SECTION_ASM_OP (VERSION_0300_SYNTAX		\
			      ? "\tsection\t .tdesc,\"a\""	\
			      : "\tsection\t .tdesc,\"x\"")

/* These must be constant strings for crtstuff.c.  */
#define CTORS_SECTION_ASM_OP "\tsection\t .ctors,\"d\"\n"
#define DTORS_SECTION_ASM_OP "\tsection\t .dtors,\"d\"\n"
#define INIT_SECTION_ASM_OP	"\tsection\t .init,\"x\""
#define FINI_SECTION_ASM_OP	"\tsection\t .fini,\"x\""

/* These are pretty much common to all assemblers.  */
#define IDENT_ASM_OP		"\tident"
#define FILE_ASM_OP		"\tfile"
#define SECTION_ASM_OP		"\tsection"
#define DEF_ASM_OP		"\tdef"
#define GLOBAL_ASM_OP		"\tglobal"
#define ALIGN_ASM_OP		"\talign"
#define SKIP_ASM_OP		"\tzero"
#define COMMON_ASM_OP		"\tcomm"
#define LOCAL_ASM_OP		"\tbss"
#define FLOAT_ASM_OP		"\tfloat"
#define DOUBLE_ASM_OP		"\tdouble"
#define INT_ASM_OP		"\tword"
#define ASM_LONG		INT_ASM_OP
#define SHORT_ASM_OP		"\thalf"
#define CHAR_ASM_OP		"\tbyte"
#define ASCII_DATA_ASM_OP	"\tstring"

/* These are particular to the global pool optimization.  */
#define SBSS_ASM_OP		"\tsbss"
#define SCOMM_ASM_OP		"\tscomm"
#define SDATA_SECTION_ASM_OP	"\tsdata"

/* These are specific to PIC.  */
#define TYPE_ASM_OP		"\ttype"
#define SIZE_ASM_OP		"\tsize"
#define WEAK_ASM_OP		"\tweak"
#ifndef AS_BUG_POUND_TYPE /* Faulty assemblers require @ rather than #.  */
#undef	TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"#%s"
#endif

/* These are specific to version 03.00 assembler syntax.  */
#define INTERNAL_ASM_OP		"\tlocal"
#define VERSION_ASM_OP		"\tversion"
#define ASM_DWARF_POP_SECTION(FILE) fputs ("\tprevious\n", FILE)
#define UNALIGNED_SHORT_ASM_OP	"\tuahalf"
#define UNALIGNED_INT_ASM_OP	"\tuaword"

/* Output any initial stuff to the assembly file.  Always put out
   a file directive, even if not debugging.

   Immediately after putting out the file, put out a "sem.<value>"
   declaration.  This should be harmless on other systems, and
   is used in DG/UX by the debuggers to suppliment COFF.  The
   fields in the integer value are as follows:

   Bits	Value	Meaning
   ----	-----	-------
   0-1	0	No information about stack locations
	1	Auto/param locations are based on r30
	2	Auto/param locations are based on CFA

   3-2	0	No information on dimension order
	1	Array dims in sym table matches source language
	2	Array dims in sym table is in reverse order

   5-4	0	No information about the case of global names
	1	Global names appear in the symbol table as in the source
	2	Global names have been converted to lower case
	3	Global names have been converted to upper case.  */

#ifdef SDB_DEBUGGING_INFO
#define ASM_COFFSEM(FILE)						\
    if (write_symbols == SDB_DEBUG)					\
      {									\
	fprintf (FILE, "\nsem.%x:\t\t; %s\n",				\
		 (((TARGET_OCS_FRAME_POSITION) ? 2 : 1) << 0) + (1 << 2) + (1 << 4),\
		 (TARGET_OCS_FRAME_POSITION)				\
			? "frame is CFA, normal array dims, case unchanged" \
			: "frame is r30, normal array dims, case unchanged"); \
      }
#else
#define ASM_COFFSEM(FILE)
#endif

/* Output the first line of the assembly file.  Redefined in m88kdgux.h.  */

#define ASM_FIRST_LINE(FILE)						\
  do {									\
    if (VERSION_0300_SYNTAX)						\
      fprintf (FILE, "%s\t \"03.00\"\n", VERSION_ASM_OP);		\
  } while (0)

/* Override svr[34].h.  */
#undef	ASM_FILE_START
#define ASM_FILE_START(FILE) \
  output_file_start (FILE, f_options, sizeof f_options / sizeof f_options[0], \
		     W_options, sizeof W_options / sizeof W_options[0])

#undef	ASM_FILE_END

#define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME) \
  fprintf (FILE, "%s\t \"%s\"\n", FILE_ASM_OP, NAME)

#ifdef SDB_DEBUGGING_INFO
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)			\
  if (m88k_prologue_done)					\
    fprintf (FILE, "\n\tln\t %d\t\t\t\t; Real source line %d\n",\
	     LINE - sdb_begin_function_line, LINE)
#endif

/* Code to handle #ident directives.  Override svr[34].h definition.  */
#undef	ASM_OUTPUT_IDENT
#ifdef DBX_DEBUGGING_INFO
#define ASM_OUTPUT_IDENT(FILE, NAME)
#else
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf(FILE, "%s\t \"%s\"\n", IDENT_ASM_OP, NAME)
#endif

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF ""

/* Format the assembly opcode so that the arguments are all aligned.
   The maximum instruction size is 8 characters (fxxx.xxx), so a tab and a
   space will do to align the output.  Abandon the output if a `%' is
   encountered.  */
#define ASM_OUTPUT_OPCODE(STREAM, PTR)					\
  {									\
    int ch;								\
    char *orig_ptr;							\
									\
    for (orig_ptr = (PTR);						\
	 (ch = *(PTR)) && ch != ' ' && ch != '\t' && ch != '\n' && ch != '%'; \
	 (PTR)++)							\
      putc (ch, STREAM);						\
									\
    if (ch == ' ' && orig_ptr != (PTR) && (PTR) - orig_ptr < 8)		\
      putc ('\t', STREAM);						\
  }

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number.
   Updated by OVERRIDE_OPTIONS to include the # for version 03.00 syntax.  */

#define REGISTER_NAMES \
  {"#r0"+1, "#r1"+1, "#r2"+1, "#r3"+1, "#r4"+1, "#r5"+1, "#r6"+1, "#r7"+1, \
   "#r8"+1, "#r9"+1, "#r10"+1,"#r11"+1,"#r12"+1,"#r13"+1,"#r14"+1,"#r15"+1,\
   "#r16"+1,"#r17"+1,"#r18"+1,"#r19"+1,"#r20"+1,"#r21"+1,"#r22"+1,"#r23"+1,\
   "#r24"+1,"#r25"+1,"#r26"+1,"#r27"+1,"#r28"+1,"#r29"+1,"#r30"+1,"#r31"+1}

/* How to renumber registers for dbx and gdb.  */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Tell when to declare ASM names.  Override svr4.h to provide this hook.  */
#undef	DECLARE_ASM_NAME
#define DECLARE_ASM_NAME TARGET_SVR4

/* Write the extra assembler code needed to declare a function properly.  */
#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    if (DECLARE_ASM_NAME)						\
      {									\
	fprintf (FILE, "%s\t ", TYPE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	putc (',', FILE);						\
	fprintf (FILE, TYPE_OPERAND_FMT, "function");			\
	putc ('\n', FILE);						\
      }									\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* Write the extra assembler code needed to declare an object properly.  */
#undef	ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    if (DECLARE_ASM_NAME)						\
      {									\
	fprintf (FILE, "%s\t ", TYPE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	putc (',', FILE);						\
	fprintf (FILE, TYPE_OPERAND_FMT, "object");			\
	putc ('\n', FILE);						\
	if (!flag_inhibit_size_directive)				\
	  {								\
	    fprintf (FILE, "%s\t ", SIZE_ASM_OP);			\
	    assemble_name (FILE, NAME);					\
	    fprintf (FILE, ",%d\n",  int_size_in_bytes (TREE_TYPE (decl))); \
	  }								\
      }									\
    ASM_OUTPUT_LABEL(FILE, NAME);					\
  } while (0)

/* This is how to declare the size of a function.  */
#undef	ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (DECLARE_ASM_NAME)						\
      {									\
	if (!flag_inhibit_size_directive)				\
	  {								\
	    char label[256];						\
	    static int labelno;						\
	    labelno++;							\
	    ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);	\
	    ASM_OUTPUT_INTERNAL_LABEL (FILE, "Lfe", labelno);		\
	    fprintf (FILE, "%s\t ", SIZE_ASM_OP);			\
	    assemble_name (FILE, (FNAME));				\
	    fprintf (FILE, ",%s-", &label[1]);				\
	    assemble_name (FILE, (FNAME));				\
	    putc ('\n', FILE);						\
	  }								\
      }									\
  } while (0)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */
#define ASM_GLOBALIZE_LABEL(FILE,NAME)			\
  do {							\
    fprintf (FILE, "%s\t ", GLOBAL_ASM_OP);		\
    assemble_name (FILE, NAME);				\
    putc ('\n', FILE);					\
  } while (0)

/* This is how to output a reference to a user-level label named NAME.
   Override svr[34].h.  */
#undef	ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)			\
  {							\
    if (! TARGET_NO_UNDERSCORES && ! VERSION_0300_SYNTAX) \
      fputc ('_', FILE);				\
    fputs (NAME, FILE);					\
  }

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   For V.4, labels use `.' rather than `@'.  */

#ifdef AS_BUG_DOT_LABELS /* The assembler requires a declaration of local.  */
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
  fprintf (FILE, VERSION_0300_SYNTAX ? ".%s%d:\n%s\t .%s%d\n" : "@%s%d:\n", \
	   PREFIX, NUM, INTERNAL_ASM_OP, PREFIX, NUM)
#else
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
  fprintf (FILE, VERSION_0300_SYNTAX ? ".%s%d:\n" : "@%s%d:\n", PREFIX, NUM)
#endif /* AS_BUG_DOT_LABELS */

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  This must agree
   with ASM_OUTPUT_INTERNAL_LABEL above, except for being prefixed
   with an `*'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf (LABEL, VERSION_0300_SYNTAX ? "*.%s%d" : "*@%s%d", PREFIX, NUM)

/* Internal macro to get a single precision floating point value into
   an int, so we can print it's value in hex.  */
#define FLOAT_TO_INT_INTERNAL( FVALUE, IVALUE )				\
  { union {								\
      REAL_VALUE_TYPE d;						\
      struct {								\
	unsigned sign      :  1;					\
	unsigned exponent1 :  1;					\
	unsigned exponent2 :  3;					\
	unsigned exponent3 :  7;					\
	unsigned mantissa1 : 20;					\
	unsigned mantissa2 :  3;					\
	unsigned mantissa3 : 29;					\
      } s;								\
    } _u;								\
									\
    union {								\
      int i;								\
      struct {								\
        unsigned sign      :  1;					\
	unsigned exponent1 :  1;					\
	unsigned exponent3 :  7;					\
        unsigned mantissa1 : 20;					\
        unsigned mantissa2 :  3;					\
      } s;								\
    } _u2;								\
									\
    _u.d = REAL_VALUE_TRUNCATE (SFmode, FVALUE);			\
    _u2.s.sign = _u.s.sign;						\
    _u2.s.exponent1 = _u.s.exponent1;					\
    _u2.s.exponent3 = _u.s.exponent3;					\
    _u2.s.mantissa1 = _u.s.mantissa1;					\
    _u2.s.mantissa2 = _u.s.mantissa2;					\
    IVALUE = _u2.i;							\
  }

/* This is how to output an assembler line defining a `double' constant.
   Use "word" pseudos to avoid printing NaNs, infinity, etc.  */
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  do {									\
    union { REAL_VALUE_TYPE d; long l[2]; } x;				\
    x.d = (VALUE);							\
    fprintf (FILE, "%s\t 0x%.8x, 0x%.8x\n", INT_ASM_OP,			\
	     x.l[0], x.l[1]);						\
  } while (0)

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  do {									\
    int i;								\
    FLOAT_TO_INT_INTERNAL (VALUE, i);					\
    fprintf (FILE, "%s\t 0x%.8x\n", INT_ASM_OP, i);			\
  } while (0)

/* Likewise for `int', `short', and `char' constants.  */
#define ASM_OUTPUT_INT(FILE,VALUE)					\
( fprintf (FILE, "%s\t ", INT_ASM_OP),					\
  output_addr_const (FILE, (VALUE)),					\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_SHORT(FILE,VALUE)					\
( fprintf (FILE, "%s\t ", SHORT_ASM_OP),				\
  output_addr_const (FILE, (VALUE)),					\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)					\
( fprintf (FILE, "%s\t ", CHAR_ASM_OP),					\
  output_addr_const (FILE, (VALUE)),					\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */
#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "%s\t 0x%x\n", CHAR_ASM_OP, (VALUE))

/* The singl-byte pseudo-op is the default.  Override svr[34].h.  */
#undef	ASM_BYTE_OP
#define ASM_BYTE_OP "\tbyte"
#undef	ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, P, SIZE)  \
  output_ascii ((FILE), (P), (SIZE))

/* Epilogue for case labels.  This jump instruction is called by casesi
   to transfer to the appropriate branch instruction within the table.
   The label `@L<n>e' is coined to mark the end of the table.  */
#define ASM_OUTPUT_CASE_END(FILE, NUM, TABLE)				\
  do {									\
    char label[256];							\
    ASM_GENERATE_INTERNAL_LABEL (label, "L", NUM);			\
    fprintf (FILE, "%se:\n", &label[1]);				\
    if (! flag_delayed_branch)						\
      fprintf (FILE, "\tlda\t %s,%s[%s]\n", reg_names[1], reg_names[1],	\
	       reg_names[m88k_case_index]);				\
    fprintf (FILE, "\tjmp\t %s\n", reg_names[1]);			\
  } while (0)

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)				\
  do {									\
    char buffer[256];							\
    ASM_GENERATE_INTERNAL_LABEL (buffer, "L", VALUE);			\
    fprintf (FILE, "\tbr\t %s\n", &buffer[1]);				\
  } while (0)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  ASM_OUTPUT_ADDR_VEC_ELT (FILE, VALUE)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "%s\t %d\n", ALIGN_ASM_OP, 1<<(LOG))

/* Align the text address to half a cache boundary when it can only be
   reached by jumping.  Pack code tightly when compiling crtstuff.c.  */
#define ASM_OUTPUT_ALIGN_CODE(FILE) \
  ASM_OUTPUT_ALIGN (FILE, (flag_inhibit_size_directive ? 2 : 3))

/* Override svr[34].h.  */
#undef	ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "%s\t %u\n", SKIP_ASM_OP, (SIZE))

/* Override svr4.h.  */
#undef	ASM_OUTPUT_EXTERNAL_LIBCALL

/* This says how to output an assembler line to define a global common
   symbol.  Size can be zero for the unusual case of a `struct { int : 0; }'.
   Override svr[34].h.  */
#undef	ASM_OUTPUT_COMMON
#undef	ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fprintf ((FILE), "%s\t ",				\
	   (ROUNDED) <= m88k_gp_threshold ? SCOMM_ASM_OP : COMMON_ASM_OP), \
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%u\n", (SIZE) ? (SIZE) : 1))

/* This says how to output an assember line to define a local common
   symbol.  Override svr[34].h.  */
#undef	ASM_OUTPUT_LOCAL
#undef	ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fprintf ((FILE), "%s\t ",				\
	   (ROUNDED) <= m88k_gp_threshold ? SBSS_ASM_OP : LOCAL_ASM_OP), \
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%u,%d\n", (SIZE) ? (SIZE) : 1, (SIZE) <= 4 ? 4 : 8))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsubu\t %s,%s,%d\n\tst\t %s,%s,0\n",	\
	   reg_names[STACK_POINTER_REGNUM],		\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT),		\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM])

/* This is how to output an insn to pop a register from the stack.  */
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tld\t %s,%s,0\n\taddu\t %s,%s,%d\n",	\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM],		\
	   reg_names[STACK_POINTER_REGNUM],		\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Macros to deal with OCS debug information */

#define OCS_START_PREFIX	"Ltb"
#define OCS_END_PREFIX		"Lte"

#define PUT_OCS_FUNCTION_START(FILE) \
  { ASM_OUTPUT_INTERNAL_LABEL (FILE, OCS_START_PREFIX, m88k_function_number); }

#define PUT_OCS_FUNCTION_END(FILE) \
  { ASM_OUTPUT_INTERNAL_LABEL (FILE, OCS_END_PREFIX, m88k_function_number); }

/* Macros for debug information */
#define DEBUGGER_AUTO_OFFSET(X) \
  (m88k_debugger_offset (X, 0) \
   + (TARGET_OCS_FRAME_POSITION ? 0 : m88k_stack_size - m88k_fp_offset))

#define DEBUGGER_ARG_OFFSET(OFFSET, X) \
  (m88k_debugger_offset (X, OFFSET) \
   + (TARGET_OCS_FRAME_POSITION ? 0 : m88k_stack_size - m88k_fp_offset))

/* Macros to deal with SDB debug information */
#ifdef SDB_DEBUGGING_INFO

/* Output structure tag names even when it causes a forward reference. */
#define SDB_ALLOW_FORWARD_REFERENCES

/* Print out extra debug information in the assembler file */
#define PUT_SDB_SCL(a)						\
  do {								\
    register int s = (a);					\
    register char *scl;						\
    switch (s)							\
      {								\
      case C_EFCN:	scl = "end of function";	break;	\
      case C_NULL:	scl = "NULL storage class";	break;	\
      case C_AUTO:	scl = "automatic";		break;	\
      case C_EXT:	scl = "external";		break;	\
      case C_STAT:	scl = "static";			break;	\
      case C_REG:	scl = "register";		break;	\
      case C_EXTDEF:	scl = "external definition";	break;	\
      case C_LABEL:	scl = "label";			break;	\
      case C_ULABEL:	scl = "undefined label";	break;	\
      case C_MOS:	scl = "structure member";	break;	\
      case C_ARG:	scl = "argument";		break;	\
      case C_STRTAG:	scl = "structure tag";		break;	\
      case C_MOU:	scl = "union member";		break;	\
      case C_UNTAG:	scl = "union tag";		break;	\
      case C_TPDEF:	scl = "typedef";		break;	\
      case C_USTATIC:	scl = "uninitialized static";	break;	\
      case C_ENTAG:	scl = "enumeration tag";	break;	\
      case C_MOE:	scl = "member of enumeration";	break;	\
      case C_REGPARM:	scl = "register parameter";	break;	\
      case C_FIELD:	scl = "bit field";		break;	\
      case C_BLOCK:	scl = "block start/end";	break;	\
      case C_FCN:	scl = "function start/end";	break;	\
      case C_EOS:	scl = "end of structure";	break;	\
      case C_FILE:	scl = "filename";		break;	\
      case C_LINE:	scl = "line";			break;	\
      case C_ALIAS:	scl = "duplicated tag";		break;	\
      case C_HIDDEN:	scl = "hidden";			break;	\
      default:		scl = "unknown";		break;	\
      }								\
								\
    fprintf(asm_out_file, "\tscl\t %d\t\t\t\t; %s\n", s, scl);	\
  } while (0)

#define PUT_SDB_TYPE(a)						\
  do {								\
    register int t = (a);					\
    static char buffer[100];					\
    register char *p = buffer, *q;				\
    register int typ = t;					\
    register int i,d;						\
								\
    for (i = 0; i <= 5; i++)					\
      {								\
	switch ((typ >> ((i*N_TSHIFT) + N_BTSHFT)) & 03)	\
	  {							\
	  case DT_PTR:						\
	    strcpy (p, "ptr to ");				\
	    p += sizeof("ptr to");				\
	    break;						\
								\
	  case DT_ARY:						\
	    strcpy (p, "array of ");				\
	    p += sizeof("array of");				\
	    break;						\
								\
	  case DT_FCN:						\
	    strcpy (p, "func ret ");				\
	    p += sizeof("func ret");				\
	    break;						\
	  }							\
      }								\
								\
  switch (typ & N_BTMASK)					\
    {								\
    case T_NULL:	q = "<no type>";	break;		\
    case T_CHAR:	q = "char";		break;		\
    case T_SHORT:	q = "short";		break;		\
    case T_INT:		q = "int";		break;		\
    case T_LONG:	q = "long";		break;		\
    case T_FLOAT:	q = "float";		break;		\
    case T_DOUBLE:	q = "double";		break;		\
    case T_STRUCT:	q = "struct";		break;		\
    case T_UNION:	q = "union";		break;		\
    case T_ENUM:	q = "enum";		break;		\
    case T_MOE:		q = "enum member";	break;		\
    case T_UCHAR:	q = "unsigned char";	break;		\
    case T_USHORT:	q = "unsigned short";	break;		\
    case T_UINT:	q = "unsigned int";	break;		\
    case T_ULONG:	q = "unsigned long";	break;		\
    default:		q = "void";		break;		\
    }								\
								\
    strcpy (p, q);						\
    fprintf(asm_out_file, "\ttype\t %d\t\t\t\t; %s\n",		\
	    t, buffer);						\
  } while (0)

#define PUT_SDB_INT_VAL(a) \
  fprintf (asm_out_file, "\tval\t %d\n", (a))

#define PUT_SDB_VAL(a)					\
( fprintf (asm_out_file, "\tval\t "),			\
  output_addr_const (asm_out_file, (a)),		\
  fputc ('\n', asm_out_file))

#define PUT_SDB_DEF(a)						\
  do { fprintf (asm_out_file, "\tsdef\t ");			\
    ASM_OUTPUT_LABELREF (asm_out_file, a);			\
    fputc ('\n', asm_out_file);					\
  } while (0)

#define PUT_SDB_PLAIN_DEF(a) \
  fprintf(asm_out_file,"\tsdef\t .%s\n", a)

/* Simply and endef now.  */
#define PUT_SDB_ENDEF \
  fputs("\tendef\n\n", asm_out_file)

#define PUT_SDB_SIZE(a) \
  fprintf (asm_out_file, "\tsize\t %d\n", (a))

/* Max dimensions to store for debug information (limited by COFF).  */
#define SDB_MAX_DIM 6

/* New method for dim operations.  */
#define PUT_SDB_START_DIM \
  fputs("\tdim\t ", asm_out_file)

/* How to end the DIM sequence.  */
#define PUT_SDB_LAST_DIM(a) \
  fprintf(asm_out_file, "%d\n", a)

#define PUT_SDB_TAG(a)						\
  do {								\
    fprintf (asm_out_file, "\ttag\t ");				\
    ASM_OUTPUT_LABELREF (asm_out_file, a);			\
    fputc ('\n', asm_out_file);					\
  } while( 0 )

#define PUT_SDB_BLOCK_OR_FUNCTION(NAME, SCL, LINE)		\
  do {								\
    fprintf (asm_out_file, "\n\tsdef\t %s\n\tval\t .\n",	\
	     NAME);						\
    PUT_SDB_SCL( SCL );						\
    fprintf (asm_out_file, "\tline\t %d\n\tendef\n\n",		\
	     (LINE));						\
  } while (0)

#define PUT_SDB_BLOCK_START(LINE) \
  PUT_SDB_BLOCK_OR_FUNCTION (".bb", C_BLOCK, (LINE))

#define PUT_SDB_BLOCK_END(LINE) \
  PUT_SDB_BLOCK_OR_FUNCTION (".eb", C_BLOCK, (LINE))

#define PUT_SDB_FUNCTION_START(LINE)				\
  do {								\
    fprintf (asm_out_file, "\tln\t 1\n");			\
    PUT_SDB_BLOCK_OR_FUNCTION (".bf", C_FCN, (LINE));		\
  } while (0)

#define PUT_SDB_FUNCTION_END(LINE)				\
  do {								\
    PUT_SDB_BLOCK_OR_FUNCTION (".ef", C_FCN, (LINE));		\
  } while (0)

#define PUT_SDB_EPILOGUE_END(NAME)				\
  do {								\
    text_section ();						\
    fprintf (asm_out_file, "\n\tsdef\t ");			\
    ASM_OUTPUT_LABELREF(asm_out_file, (NAME));			\
    fputc('\n', asm_out_file);					\
    PUT_SDB_SCL( C_EFCN );					\
    fprintf (asm_out_file, "\tendef\n\n");			\
  } while (0)

#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), ".%dfake", (NUMBER));

#endif /* SDB_DEBUGGING_INFO */

/* Support const and tdesc sections.  Generally, a const section will
   be distinct from the text section whenever we do V.4-like things
   and so follows DECLARE_ASM_NAME.  Note that strings go in text
   rather than const.  Override svr[34].h.  */

#undef	USE_CONST_SECTION
#undef	EXTRA_SECTIONS

#define USE_CONST_SECTION DECLARE_ASM_NAME

#if defined(CTORS_SECTION_FUNCTION) /* SVR4 */

#define EXTRA_SECTIONS in_const, in_tdesc, in_sdata, in_ctors, in_dtors
#define INIT_SECTION_FUNCTION
#define FINI_SECTION_FUNCTION

#elif defined(FINI_SECTION_FUNCTION) /* SVR3 */

#define EXTRA_SECTIONS in_const, in_tdesc, in_sdata, in_init, in_fini
#define CTORS_SECTION_FUNCTION
#define DTORS_SECTION_FUNCTION

#else /* m88kluna or other not based on svr[34].h.  */

#define EXTRA_SECTIONS in_const, in_tdesc, in_sdata
#define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  text_section();							\
}
#define CTORS_SECTION_FUNCTION
#define DTORS_SECTION_FUNCTION
#define INIT_SECTION_FUNCTION
#define FINI_SECTION_FUNCTION

#endif /* CTORS_SECTION_FUNCTION */

#undef	EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
									\
void									\
tdesc_section ()							\
{									\
  if (in_section != in_tdesc)						\
    {									\
      fprintf (asm_out_file, "%s\n", TDESC_SECTION_ASM_OP);		\
      in_section = in_tdesc;						\
    }									\
}									\
									\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION						\
  INIT_SECTION_FUNCTION							\
  FINI_SECTION_FUNCTION

#undef READONLY_DATA_SECTION

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.

   For strings, the section is selected before the segment info is encoded.  */
#undef	SELECT_SECTION
#define SELECT_SECTION(DECL,RELOC)					\
{									\
  if (TREE_CODE (DECL) == STRING_CST)					\
    {									\
      if (! flag_writable_strings)					\
	const_section ();						\
      else if (m88k_gp_threshold > 0					\
	       && TREE_STRING_LENGTH (DECL) <= m88k_gp_threshold)	\
	sdata_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (DECL) == VAR_DECL)				\
    {									\
      if (SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)))			\
	sdata_section ();						\
      else if ((flag_pic && RELOC)					\
	  || !TREE_READONLY (DECL) || TREE_SIDE_EFFECTS (DECL))		\
	data_section ();						\
      else								\
	const_section ();						\
    }									\
  else									\
    const_section ();							\
}

/* Define this macro if references to a symbol must be treated differently
   depending on something about the variable or function named by the
   symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the rtl for
   DECL has been created and stored in `DECL_RTL (DECL)'.  The value of the
   rtl will be a `mem' whose address is a `symbol_ref'.

   For the m88k, determine if the item should go in the global pool.  */
#define ENCODE_SECTION_INFO(DECL)					\
  do {									\
    if (m88k_gp_threshold > 0)						\
      if (TREE_CODE (DECL) == VAR_DECL)					\
	{								\
	  if (!TREE_READONLY (DECL) || TREE_SIDE_EFFECTS (DECL))	\
	    {								\
	      int size = int_size_in_bytes (TREE_TYPE (DECL));		\
									\
	      if (size > 0 && size <= m88k_gp_threshold)		\
		SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;	\
	    }								\
	}								\
      else if (TREE_CODE (DECL) == STRING_CST				\
	       && flag_writable_strings					\
	       && TREE_STRING_LENGTH (DECL) <= m88k_gp_threshold)	\
	SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (DECL), 0)) = 1;		\
  } while (0)

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
#define PRINT_OPERAND_PUNCT_VALID_P(c) \
  ((c) == '#' || (c) == '.' || (c) == '!' || (c) == '*' || (c) == ';')

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

/* Print a memory address as an operand to reference that memory location.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)
