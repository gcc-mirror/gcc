/* Definitions of target machine for GNU compiler for Hitachi Super-H.
   Copyright (C) 1993-1998 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com).
   Improved by Jim Wilson (wilson@cygnus.com).

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


#define TARGET_VERSION \
  fputs (" (Hitachi SH)", stderr);

/* Unfortunately, insn-attrtab.c doesn't include insn-codes.h.  We can't
  include it here, because hconfig.h is also included by gencodes.c .  */
extern int code_for_indirect_jump_scratch;

/* Generate SDB debugging information.  */

#define SDB_DEBUGGING_INFO

/* Output DBX (stabs) debugging information if doing -gstabs.  */

#include "dbxcoff.h"

#define SDB_DELIM ";"

#define CPP_SPEC "%{ml:-D__LITTLE_ENDIAN__} \
%{m1:-D__sh1__} \
%{m2:-D__sh2__} \
%{m3:-D__sh3__} \
%{m3e:-D__SH3E__} \
%{m4-single-only:-D__SH4_SINGLE_ONLY__} \
%{m4-single:-D__SH4_SINGLE__} \
%{m4:-D__SH4__} \
%{!m1:%{!m2:%{!m3:%{!m3e:%{!m4:%{!m4-single:%{!m4-single-only:-D__sh1__}}}}}}} \
%{mhitachi:-D__HITACHI__}"

#define CPP_PREDEFINES "-D__sh__ -Acpu(sh) -Amachine(sh)"

#define ASM_SPEC  "%{ml:-little} %{mrelax:-relax}"

#define LINK_SPEC "%{ml:-m shl} %{mrelax:-relax}"

/* We can not debug without a frame pointer.  */
/* #define CAN_DEBUG_WITHOUT_FP */

#define CONDITIONAL_REGISTER_USAGE					\
  if (! TARGET_SH4 || ! TARGET_FMOVD)					\
    {									\
      int regno;							\
      for (regno = FIRST_XD_REG; regno <= LAST_XD_REG; regno++)		\
	fixed_regs[regno] = call_used_regs[regno] = 1;			\
      if (! TARGET_SH4)							\
	{								\
	  if (! TARGET_SH3E)						\
	    {								\
	      int regno;						\
	      for (regno = FIRST_FP_REG; regno <= LAST_FP_REG; regno++)	\
		fixed_regs[regno] = call_used_regs[regno] = 1;		\
	      fixed_regs[FPUL_REG] = call_used_regs[FPUL_REG] = 1;	\
	    }								\
	}								\
    }									\
  /* Hitachi saves and restores mac registers on call.  */		\
  if (TARGET_HITACHI)							\
    {									\
      call_used_regs[MACH_REG] = 0;					\
      call_used_regs[MACL_REG] = 0;					\
    }

/* ??? Need to write documentation for all SH options and add it to the
   invoke.texi file.  */

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;
#define ISIZE_BIT      	(1<<1)
#define DALIGN_BIT     	(1<<6)
#define SH1_BIT	       	(1<<8)
#define SH2_BIT	       	(1<<9)
#define SH3_BIT	       	(1<<10)
#define SH3E_BIT	(1<<11)
#define HARD_SH4_BIT	(1<<5)
#define FPU_SINGLE_BIT	(1<<7)
#define SH4_BIT	       	(1<<12)
#define FMOVD_BIT	(1<<4)
#define SPACE_BIT 	(1<<13)
#define BIGTABLE_BIT  	(1<<14)
#define RELAX_BIT	(1<<15)
#define HITACHI_BIT     (1<<22)
#define PADSTRUCT_BIT  (1<<28)
#define LITTLE_ENDIAN_BIT (1<<29)
#define IEEE_BIT (1<<30)

/* Nonzero if we should dump out instruction size info.  */
#define TARGET_DUMPISIZE  (target_flags & ISIZE_BIT)

/* Nonzero to align doubles on 64 bit boundaries.  */
#define TARGET_ALIGN_DOUBLE (target_flags & DALIGN_BIT)

/* Nonzero if we should generate code using type 1 insns.  */
#define TARGET_SH1 (target_flags & SH1_BIT)

/* Nonzero if we should generate code using type 2 insns.  */
#define TARGET_SH2 (target_flags & SH2_BIT)

/* Nonzero if we should generate code using type 3 insns.  */
#define TARGET_SH3 (target_flags & SH3_BIT)

/* Nonzero if we should generate code using type 3E insns.  */
#define TARGET_SH3E (target_flags & SH3E_BIT)

/* Nonzero if the cache line size is 32. */
#define TARGET_CACHE32 (target_flags & HARD_SH4_BIT)

/* Nonzero if we schedule for a superscalar implementation. */
#define TARGET_SUPERSCALAR (target_flags & HARD_SH4_BIT)

/* Nonzero if the target has separate instruction and data caches.  */
#define TARGET_HARWARD (target_flags & HARD_SH4_BIT)

/* Nonzero if compiling for SH4 hardware (to be used for insn costs etc.)  */
#define TARGET_HARD_SH4 (target_flags & HARD_SH4_BIT)

/* Nonzero if the default precision of th FPU is single */
#define TARGET_FPU_SINGLE (target_flags & FPU_SINGLE_BIT)

/* Nonzero if we should generate code using type 4 insns.  */
#define TARGET_SH4 (target_flags & SH4_BIT)

/* Nonzero if we should generate fmovd.  */
#define TARGET_FMOVD (target_flags & FMOVD_BIT)

/* Nonzero if we respect NANs.  */
#define TARGET_IEEE (target_flags & IEEE_BIT)

/* Nonzero if we should generate smaller code rather than faster code.  */
#define TARGET_SMALLCODE   (target_flags & SPACE_BIT)

/* Nonzero to use long jump tables.  */
#define TARGET_BIGTABLE     (target_flags & BIGTABLE_BIT)

/* Nonzero to generate pseudo-ops needed by the assembler and linker
   to do function call relaxing.  */
#define TARGET_RELAX (target_flags & RELAX_BIT)

/* Nonzero if using Hitachi's calling convention.  */
#define TARGET_HITACHI 		(target_flags & HITACHI_BIT)

/* Nonzero if padding structures to a multiple of 4 bytes.  This is
   incompatible with Hitachi's compiler, and gives unusual structure layouts
   which confuse programmers.
   ??? This option is not useful, but is retained in case there are people
   who are still relying on it.  It may be deleted in the future.  */
#define TARGET_PADSTRUCT       (target_flags & PADSTRUCT_BIT)

/* Nonzero if generating code for a little endian SH.  */
#define TARGET_LITTLE_ENDIAN     (target_flags & LITTLE_ENDIAN_BIT)

#define TARGET_SWITCHES  			\
{ {"1",	        SH1_BIT},			\
  {"2",	        SH2_BIT},			\
  {"3",	        SH3_BIT|SH2_BIT},		\
  {"3e",	SH3E_BIT|SH3_BIT|SH2_BIT|FPU_SINGLE_BIT},	\
  {"4-single-only",	SH3E_BIT|SH3_BIT|SH2_BIT|SH3E_BIT|HARD_SH4_BIT|FPU_SINGLE_BIT},	\
  {"4-single",	SH4_BIT|SH3E_BIT|SH3_BIT|SH2_BIT|HARD_SH4_BIT|FPU_SINGLE_BIT},\
  {"4",	        SH4_BIT|SH3E_BIT|SH3_BIT|SH2_BIT|HARD_SH4_BIT},	\
  {"b",		-LITTLE_ENDIAN_BIT},  		\
  {"bigtable", 	BIGTABLE_BIT},			\
  {"dalign",  	DALIGN_BIT},			\
  {"fmovd",  	FMOVD_BIT},			\
  {"hitachi",	HITACHI_BIT},			\
  {"ieee",  	IEEE_BIT},			\
  {"isize", 	ISIZE_BIT},			\
  {"l",		LITTLE_ENDIAN_BIT},  		\
  {"no-ieee",  	-IEEE_BIT},			\
  {"padstruct", PADSTRUCT_BIT},    		\
  {"relax",	RELAX_BIT},			\
  {"space", 	SPACE_BIT},			\
  SUBTARGET_SWITCHES                            \
  {"",   	TARGET_DEFAULT} 		\
}

/* This are meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES

#define TARGET_DEFAULT  (0)

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)				\
do {									\
  if (LEVEL)								\
    flag_omit_frame_pointer = -1;					\
  if (LEVEL)								\
    sh_flag_remove_dead_before_cse = 1;					\
  if (SIZE)								\
    target_flags |= SPACE_BIT;						\
} while (0)

#define ASSEMBLER_DIALECT assembler_dialect

extern int assembler_dialect;

#define OVERRIDE_OPTIONS 						\
do {									\
  sh_cpu = CPU_SH1;							\
  assembler_dialect = 0;						\
  if (TARGET_SH2)							\
    sh_cpu = CPU_SH2;							\
  if (TARGET_SH3)							\
    sh_cpu = CPU_SH3;							\
  if (TARGET_SH3E)							\
    sh_cpu = CPU_SH3E;							\
  if (TARGET_SH4)							\
    {									\
      assembler_dialect = 1;						\
      sh_cpu = CPU_SH4;							\
    }									\
  if (! TARGET_SH4 || ! TARGET_FMOVD)					\
    {									\
      /* Prevent usage of explicit register names for variables		\
	 for registers not present / not addressable in the		\
	 target architecture.  */					\
      int regno;							\
      for (regno = (TARGET_SH3E) ? 17 : 0; 				\
	   regno <= 24; regno++)					\
	fp_reg_names[regno][0] = 0;					\
    }									\
  if (flag_omit_frame_pointer < 0)					\
   /* The debugging information is sufficient,				\
      but gdb doesn't implement this yet */				\
   if (0)								\
    flag_omit_frame_pointer						\
      = (PREFERRED_DEBUGGING_TYPE == DWARF_DEBUG			\
	 || PREFERRED_DEBUGGING_TYPE == DWARF2_DEBUG);			\
   else									\
    flag_omit_frame_pointer = 0;					\
									\
  /* Never run scheduling before reload, since that can			\
     break global alloc, and generates slower code anyway due		\
     to the pressure on R0.  */						\
  flag_schedule_insns = 0;						\
  sh_addr_diff_vec_mode = TARGET_BIGTABLE ? SImode : HImode;		\
} while (0)

/* Target machine storage layout.  */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */

#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#if defined(__LITTLE_ENDIAN__)
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#endif

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT  8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD  32
#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD	4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE  32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY  	32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  BIGGEST_ALIGNMENT

/* The log (base 2) of the cache line size, in bytes.  Processors prior to
   SH2 have no actual cache, but they fetch code in chunks of 4 bytes.
   The SH2/3 have 16 byte cache lines, and the SH4 has a 32 byte cache line */
#define CACHE_LOG (TARGET_CACHE32 ? 5 : TARGET_SH2 ? 4 : 2)

/* Allocation boundary (in *bits*) for the code of a function.
   32 bit alignment is faster, because instructions are always fetched as a
   pair from a longword boundary.  */
#define FUNCTION_BOUNDARY  (TARGET_SMALLCODE ? 16 : (1 << CACHE_LOG) * 8)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  (TARGET_ALIGN_DOUBLE ? 64 : 32)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)	\
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
    ? FASTEST_ALIGNMENT : (ALIGN))

#ifndef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 128
#endif

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Number of bits which any structure or union's size must be a
   multiple of.  Each structure or union's size is rounded up to a
   multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY (TARGET_PADSTRUCT ? 32 : 8)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* If LABEL_AFTER_BARRIER demands an alignment, return its base 2 logarithm.  */
#define LABEL_ALIGN_AFTER_BARRIER(LABEL_AFTER_BARRIER) \
  barrier_align (LABEL_AFTER_BARRIER)

#define LOOP_ALIGN(A_LABEL) \
  ((! optimize || TARGET_HARWARD || TARGET_SMALLCODE) ? 0 : 2)

#define LABEL_ALIGN(A_LABEL) \
(									\
  (PREV_INSN (A_LABEL)							\
   && GET_CODE (PREV_INSN (A_LABEL)) == INSN				\
   && GET_CODE (PATTERN (PREV_INSN (A_LABEL))) == UNSPEC_VOLATILE	\
   && XINT (PATTERN (PREV_INSN (A_LABEL)), 1) == 1)			\
   /* explicit alignment insn in constant tables. */			\
  ? INTVAL (XVECEXP (PATTERN (PREV_INSN (A_LABEL)), 0, 0))		\
  : 0)

/* Jump tables must be 32 bit aligned, no matter the size of the element.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 2

/* The base two logarithm of the known minimum alignment of an insn length.  */
#define INSN_LENGTH_ALIGNMENT(A_INSN)					\
  (GET_CODE (A_INSN) == INSN						\
   ? 1									\
   : GET_CODE (A_INSN) == JUMP_INSN || GET_CODE (A_INSN) == CALL_INSN	\
   ? 1									\
   : CACHE_LOG)

/* Standard register usage.  */

/* Register allocation for the Hitachi calling convention:

        r0		arg return
	r1..r3          scratch
	r4..r7		args in
	r8..r13		call saved
	r14		frame pointer/call saved
	r15		stack pointer
	ap		arg pointer (doesn't really exist, always eliminated)
	pr		subroutine return address
	t               t bit
	mach		multiply/accumulate result, high part
	macl		multiply/accumulate result, low part.
	fpul		fp/int communication register
	rap		return address pointer register
	fr0		fp arg return
	fr1..fr3	scratch floating point registers
	fr4..fr11	fp args in
	fr12..fr15	call saved floating point registers  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

#define AP_REG   16
#define PR_REG   17
#define T_REG    18
#define GBR_REG  19
#define MACH_REG 20
#define MACL_REG 21
#define SPECIAL_REG(REGNO) ((REGNO) >= 18 && (REGNO) <= 21)
#define FPUL_REG 22
#define RAP_REG 23
#define FIRST_FP_REG 24
#define LAST_FP_REG 39
#define FIRST_XD_REG 40
#define LAST_XD_REG 47
#define FPSCR_REG 48

#define FIRST_PSEUDO_REGISTER 49

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   Mach register is fixed 'cause it's only 10 bits wide for SH1.
   It is 32 bits wide for SH2.  */

#define FIXED_REGISTERS  	\
  { 0,  0,  0,  0, 		\
    0,  0,  0,  0, 		\
    0,  0,  0,  0, 		\
    0,  0,  0,  1, 		\
    1,  1,  1,  1, 		\
    1,  1,  0,  1,		\
    0,  0,  0,  0,		\
    0,  0,  0,  0,		\
    0,  0,  0,  0,		\
    0,  0,  0,  0,		\
    0,  0,  0,  0,		\
    0,  0,  0,  0,		\
    1,				\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS 	\
  { 1,  1,  1,  1,		\
    1,  1,  1,  1, 		\
    0,  0,  0,  0,		\
    0,  0,  0,  1,		\
    1,  0,  1,  1,		\
    1,  1,  1,  1,		\
    1,  1,  1,  1,		\
    1,  1,  1,  1,		\
    1,  1,  1,  1,		\
    0,  0,  0,  0,		\
    1,  1,  1,  1,		\
    1,  1,  0,  0,		\
    1,				\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the SH all but the XD regs are UNITS_PER_WORD bits wide.  */

#define HARD_REGNO_NREGS(REGNO, MODE) \
   ((REGNO) >= FIRST_XD_REG && (REGNO) <= LAST_XD_REG \
    ? (GET_MODE_SIZE (MODE) / (2 * UNITS_PER_WORD)) \
    : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)) \

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   We can allow any mode in any general register.  The special registers
   only allow SImode.  Don't allow any mode in the PR.  */

/* We cannot hold DCmode values in the XD registers because alter_reg
   handles subregs of them incorrectly.  We could work around this by
   spacing the XD registers like the DR registers, but this would require
   additional memory in every compilation to hold larger register vectors.
   We could hold SFmode / SCmode values in XD registers, but that
   would require a tertiary reload when reloading from / to memory,
   and a secondary reload to reload from / to general regs; that
   seems to be a loosing proposition.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)		\
  (SPECIAL_REG (REGNO) ? (MODE) == SImode	\
   : (REGNO) == FPUL_REG ? (MODE) == SImode || (MODE) == SFmode	\
   : (REGNO) >= FIRST_FP_REG && (REGNO) <= LAST_FP_REG && (MODE) == SFmode \
   ? 1 \
   : (REGNO) >= FIRST_FP_REG && (REGNO) <= LAST_FP_REG \
   ? ((MODE) == SFmode \
      || (TARGET_SH3E && (MODE) == SCmode) \
      || (((TARGET_SH4 && (MODE) == DFmode) || (MODE) == DCmode) \
	  && (((REGNO) - FIRST_FP_REG) & 1) == 0)) \
   : (REGNO) >= FIRST_XD_REG && (REGNO) <= LAST_XD_REG \
   ? (MODE) == DFmode \
   : (REGNO) == PR_REG ? 0			\
   : (REGNO) == FPSCR_REG ? (MODE) == PSImode \
   : 1)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) == (MODE2) || GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Define this if the program counter is overloaded on a register.  */
/* #define PC_REGNUM		15*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM	15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM	14

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM 23

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.  */

#define FRAME_POINTER_REQUIRED	0

/* Definitions for register eliminations.

   We have three registers that can be eliminated on the SH.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.
   Third, there is the return address pointer, which can also be replaced
   with either the stack or the frame pointer.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

/* If you add any registers here that are not actually hard registers,
   and that have any alternative of elimination that doesn't always
   apply, you need to amend calc_live_regs to exclude it, because
   reload spills all eliminable registers where it sees an
   can_eliminate == 0 entry, thus making them 'live' .
   If you add any hard registers that can be eliminated in different
   ways, you have to patch reload to spill them only when all alternatives
   of elimination fail.  */

#define ELIMINABLE_REGS						\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { RETURN_ADDRESS_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},}

/* Given FROM and TO register numbers, say whether this elimination
   is allowed.  */
#define CAN_ELIMINATE(FROM, TO) \
  (!((FROM) == FRAME_POINTER_REGNUM && FRAME_POINTER_REQUIRED))

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = initial_elimination_offset ((FROM), (TO))

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	16

/* Register in which the static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM	13

/* The register in which a struct value address is passed.  */

#define STRUCT_VALUE_REGNUM 2

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */

/*#define STRUCT_VALUE ((rtx)0)*/

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */

#define DEFAULT_PCC_STRUCT_RETURN 0

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

/* The SH has two sorts of general registers, R0 and the rest.  R0 can
   be used as the destination of some of the arithmetic ops. There are
   also some special purpose registers; the T bit register, the
   Procedure Return Register and the Multiply Accumulate Registers.  */
/* Place GENERAL_REGS after FPUL_REGS so that it will be preferred by
   reg_class_subunion.  We don't want to have an actual union class
   of these, because it would only be used when both classes are calculated
   to give the same cost, but there is only one FPUL register.
   Besides, regclass fails to notice the different REGISTER_MOVE_COSTS
   applying to the actual instruction alternative considered.  E.g., the
   y/r alternative of movsi_ie is considered to have no more cost that
   the r/r alternative, which is patently untrue.  */

enum reg_class
{
  NO_REGS,
  R0_REGS,
  PR_REGS,
  T_REGS,
  MAC_REGS,
  FPUL_REGS,
  GENERAL_REGS,
  FP0_REGS,
  FP_REGS,
  DF_REGS,
  FPSCR_REGS,
  GENERAL_FP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES	\
{			\
  "NO_REGS",		\
  "R0_REGS",		\
  "PR_REGS",		\
  "T_REGS",		\
  "MAC_REGS",		\
  "FPUL_REGS",		\
  "GENERAL_REGS",	\
  "FP0_REGS",		\
  "FP_REGS",		\
  "DF_REGS",		\
  "FPSCR_REGS",		\
  "GENERAL_FP_REGS",	\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS				\
{							\
  { 0x00000000, 0x00000000 }, /* NO_REGS	*/	\
  { 0x00000001, 0x00000000 }, /* R0_REGS	*/	\
  { 0x00020000, 0x00000000 }, /* PR_REGS	*/	\
  { 0x00040000, 0x00000000 }, /* T_REGS		*/	\
  { 0x00300000, 0x00000000 }, /* MAC_REGS	*/	\
  { 0x00400000, 0x00000000 }, /* FPUL_REGS	*/	\
  { 0x0081FFFF, 0x00000000 }, /* GENERAL_REGS	*/	\
  { 0x01000000, 0x00000000 }, /* FP0_REGS	*/	\
  { 0xFF000000, 0x000000FF }, /* FP_REGS	*/	\
  { 0xFF000000, 0x0000FFFF }, /* DF_REGS	*/	\
  { 0x00000000, 0x00010000 }, /* FPSCR_REGS	*/	\
  { 0xFF81FFFF, 0x0000FFFF }, /* GENERAL_FP_REGS */	\
  { 0xFFFFFFFF, 0x0001FFFF }, /* ALL_REGS	*/	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern int regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[(REGNO)]

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers.  */

#define SMALL_REGISTER_CLASSES 1

/* The order in which register should be allocated.  */
/* Sometimes FP0_REGS becomes the preferred class of a floating point pseudo,
   and GENERAL_FP_REGS the alternate class.  Since FP0 is likely to be
   spilled or used otherwise, we better have the FP_REGS allocated first.  */
#define REG_ALLOC_ORDER \
  { 25,26,27,28,29,30,31,24,32,33,34,35,36,37,38,39,	\
    40,41,42,43,44,45,46,47,48,				\
    1,2,3,7,6,5,4,0,8,9,10,11,12,13,14,			\
    22,15,16,17,18,19,20,21,23 }

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  R0_REGS
#define BASE_REG_CLASS	 GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine
   description.  */
extern enum reg_class reg_class_from_letter[];

#define REG_CLASS_FROM_LETTER(C) \
   ( (C) >= 'a' && (C) <= 'z' ? reg_class_from_letter[(C)-'a'] : NO_REGS )

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.
	I: arithmetic operand -127..128, as used in add, sub, etc
	K: shift operand 1,2,8 or 16
	L: logical operand 0..255, as used in and, or, etc.
	M: constant 1
	N: constant 0  */

#define CONST_OK_FOR_I(VALUE) (((int)(VALUE))>= -128 && ((int)(VALUE)) <= 127)
#define CONST_OK_FOR_K(VALUE) ((VALUE)==1||(VALUE)==2||(VALUE)==8||(VALUE)==16)
#define CONST_OK_FOR_L(VALUE) (((int)(VALUE))>=    0 && ((int)(VALUE)) <= 255)
#define CONST_OK_FOR_M(VALUE) ((VALUE)==1)
#define CONST_OK_FOR_N(VALUE) ((VALUE)==0)
#define CONST_OK_FOR_LETTER_P(VALUE, C)		\
     ((C) == 'I' ? CONST_OK_FOR_I (VALUE)	\
    : (C) == 'K' ? CONST_OK_FOR_K (VALUE)	\
    : (C) == 'L' ? CONST_OK_FOR_L (VALUE)	\
    : (C) == 'M' ? CONST_OK_FOR_M (VALUE)	\
    : (C) == 'N' ? CONST_OK_FOR_N (VALUE)	\
    : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)	\
((C) == 'G' ? fp_zero_operand (VALUE)		\
 : (C) == 'H' ? fp_one_operand (VALUE)		\
 : (C) == 'F')

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS,MODE,X) \
  ((((((CLASS) == FP_REGS || (CLASS) == FP0_REGS			\
	|| (CLASS) == DF_REGS)						\
      && (GET_CODE (X) == REG && REGNO (X) <= AP_REG))			\
     || (((CLASS) == GENERAL_REGS || (CLASS) == R0_REGS)		\
	 && GET_CODE (X) == REG						\
	 && REGNO (X) >= FIRST_FP_REG && REGNO (X) <= LAST_FP_REG))	\
    && MODE == SFmode)							\
   ? FPUL_REGS								\
   : ((CLASS) == FPUL_REGS						\
      && (GET_CODE (X) == MEM						\
	  || (GET_CODE (X) == REG && REGNO (X) >= FIRST_PSEUDO_REGISTER)))\
   ? GENERAL_REGS							\
   : (((CLASS) == MAC_REGS || (CLASS) == PR_REGS)			\
      && GET_CODE (X) == REG && REGNO (X) > 15				\
      && (CLASS) != REGNO_REG_CLASS (REGNO (X)))			\
   ? GENERAL_REGS : NO_REGS)

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS,MODE,X)  \
  ((((CLASS) == FP_REGS || (CLASS) == FP0_REGS || (CLASS) == DF_REGS)	\
    && immediate_operand ((X), (MODE))					\
    && ! ((fp_zero_operand (X) || fp_one_operand (X)) && (MODE) == SFmode))\
   ? R0_REGS								\
   : CLASS == FPUL_REGS && immediate_operand ((X), (MODE))		\
   ? (GET_CODE (X) == CONST_INT && CONST_OK_FOR_I (INTVAL (X))		\
      ? GENERAL_REGS							\
      : R0_REGS)							\
   : (CLASS == FPSCR_REGS						\
      && ((GET_CODE (X) == REG && REGNO (X) >= FIRST_PSEUDO_REGISTER)	\
	  || GET_CODE (X) == MEM && GET_CODE (XEXP ((X), 0)) == PLUS))	\
   ? GENERAL_REGS							\
   : SECONDARY_OUTPUT_RELOAD_CLASS((CLASS),(MODE),(X)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On SH this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
     ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the size of the object.  */

#define CLASS_CANNOT_CHANGE_SIZE	DF_REGS

/* Stack layout; function entry, exit and calling.  */

/* Define the number of registers that can hold parameters.
   These macros are used only in other macro definitions below.  */

#define NPARM_REGS(MODE) \
  (TARGET_SH3E && (MODE) == SFmode \
   ? 8 \
   : TARGET_SH4 && (GET_MODE_CLASS (MODE) == MODE_FLOAT \
		    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
   ? 8 \
   : 4)

#define FIRST_PARM_REG 4
#define FIRST_RET_REG  0

#define FIRST_FP_PARM_REG (FIRST_FP_REG + 4)
#define FIRST_FP_RET_REG FIRST_FP_REG

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/*  Define this macro if the addresses of local variable slots are at
    negative offsets from the frame pointer.

    The SH only has positive indexes, so grow the frame up.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */
#define STARTING_FRAME_OFFSET  0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
/* Don't define PUSH_ROUNDING, since the hardware doesn't do this.
   When PUSH_ROUNDING is not defined, PARM_BOUNDARY will cause gcc to
   do correct alignment.  */
#if 0
#define PUSH_ROUNDING(NPUSHED)  (((NPUSHED) + 3) & ~3)
#endif

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  0

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the SH, the caller does not pop any of its arguments that were passed
   on the stack.  */
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)  0

/* Nonzero if we do not know how to pass TYPE solely in registers.
   Values that come in registers with inconvenient padding are stored
   to memory at the function start.  */

#define MUST_PASS_IN_STACK(MODE,TYPE)			\
  ((TYPE) != 0						\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST	\
       || TREE_ADDRESSABLE (TYPE)))
/* Some subroutine macros specific to this machine. */

#define BASE_RETURN_VALUE_REG(MODE) \
  ((TARGET_SH3E && ((MODE) == SFmode))			\
   ? FIRST_FP_RET_REG					\
   : TARGET_SH3E && (MODE) == SCmode		\
   ? FIRST_FP_RET_REG					\
   : (TARGET_SH4					\
      && ((MODE) == DFmode || (MODE) == SFmode		\
	  || (MODE) == DCmode || (MODE) == SCmode ))	\
   ? FIRST_FP_RET_REG					\
   : FIRST_RET_REG)

#define BASE_ARG_REG(MODE) \
  ((TARGET_SH3E && ((MODE) == SFmode))			\
   ? FIRST_FP_PARM_REG					\
   : TARGET_SH4 && (GET_MODE_CLASS (MODE) == MODE_FLOAT	\
		    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)\
   ? FIRST_FP_PARM_REG					\
   : FIRST_PARM_REG)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.
   For the SH, this is like LIBCALL_VALUE, except that we must change the
   mode like PROMOTE_MODE does.
   ??? PROMOTE_MODE is ignored for non-scalar types.  The set of types
   tested here has to be kept in sync with the one in explow.c:promote_mode.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  gen_rtx (REG,								\
	   ((GET_MODE_CLASS (TYPE_MODE (VALTYPE)) == MODE_INT		\
	     && GET_MODE_SIZE (TYPE_MODE (VALTYPE)) < UNITS_PER_WORD	\
	     && (TREE_CODE (VALTYPE) == INTEGER_TYPE			\
		 || TREE_CODE (VALTYPE) == ENUMERAL_TYPE		\
		 || TREE_CODE (VALTYPE) == BOOLEAN_TYPE			\
		 || TREE_CODE (VALTYPE) == CHAR_TYPE			\
		 || TREE_CODE (VALTYPE) == REAL_TYPE			\
		 || TREE_CODE (VALTYPE) == OFFSET_TYPE))		\
	    ? SImode : TYPE_MODE (VALTYPE)),				\
	   BASE_RETURN_VALUE_REG (TYPE_MODE (VALTYPE)))
     
/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) \
  gen_rtx (REG, (MODE), BASE_RETURN_VALUE_REG (MODE))

/* 1 if N is a possible register number for a function value. */
#define FUNCTION_VALUE_REGNO_P(REGNO) \
  ((REGNO) == FIRST_RET_REG || (TARGET_SH3E && (REGNO) == FIRST_FP_RET_REG))

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  (((REGNO) >= FIRST_PARM_REG && (REGNO) < (FIRST_PARM_REG + 4))        \
   || (TARGET_SH3E                                                      \
       && (REGNO) >= FIRST_FP_PARM_REG && (REGNO) < (FIRST_FP_PARM_REG + 8)))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SH, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus NARGREGS or more means all following args should go on the stack.  */

enum sh_arg_class { SH_ARG_INT = 0, SH_ARG_FLOAT = 1 };
struct sh_args {
    int arg_count[2];
};

#define CUMULATIVE_ARGS  struct sh_args

#define GET_SH_ARG_CLASS(MODE) \
  ((TARGET_SH3E && (MODE) == SFmode) \
   ? SH_ARG_FLOAT \
   : TARGET_SH4 && (GET_MODE_CLASS (MODE) == MODE_FLOAT \
		    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
   ? SH_ARG_FLOAT : SH_ARG_INT)

#define ROUND_ADVANCE(SIZE) \
  (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round a register number up to a proper boundary for an arg of mode
   MODE.

   The SH doesn't care about double alignment, so we only
   round doubles to even regs when asked to explicitly.  */

#define ROUND_REG(CUM, MODE) \
   (((TARGET_ALIGN_DOUBLE					\
      || (TARGET_SH4 && ((MODE) == DFmode || (MODE) == DCmode)	\
	  && (CUM).arg_count[(int) SH_ARG_FLOAT] < NPARM_REGS (MODE)))\
     && GET_MODE_UNIT_SIZE ((MODE)) > UNITS_PER_WORD)		\
    ? ((CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)]		\
       + ((CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)] & 1))	\
    : (CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)])

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On SH, the offset always starts at 0: the first parm reg is always
   the same reg for a given argument class.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT) \
  do {								\
    (CUM).arg_count[(int) SH_ARG_INT] = 0;			\
    (CUM).arg_count[(int) SH_ARG_FLOAT] = 0;			\
  } while (0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be
   available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 if (! TARGET_SH4 || PASS_IN_REG_P ((CUM), (MODE), (TYPE))) \
   ((CUM).arg_count[(int) GET_SH_ARG_CLASS (MODE)]	\
    = (ROUND_REG ((CUM), (MODE))			\
       + ((MODE) == BLKmode				\
	  ? ROUND_ADVANCE (int_size_in_bytes (TYPE))	\
	  : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))))

/* Return boolean indicating arg of mode MODE will be passed in a reg.
   This macro is only used in this file. */

#define PASS_IN_REG_P(CUM, MODE, TYPE) \
  (((TYPE) == 0 \
    || (! TREE_ADDRESSABLE ((tree)(TYPE))) \
	&& (! TARGET_HITACHI || ! AGGREGATE_TYPE_P (TYPE))) \
   && (TARGET_SH3E \
       ? ((MODE) == BLKmode \
	  ? (((CUM).arg_count[(int) SH_ARG_INT] * UNITS_PER_WORD \
	      + int_size_in_bytes (TYPE)) \
	     <= NPARM_REGS (SImode) * UNITS_PER_WORD) \
	  : ((ROUND_REG((CUM), (MODE)) \
	      + HARD_REGNO_NREGS (BASE_ARG_REG (MODE), (MODE))) \
	     <= NPARM_REGS (MODE))) \
       : ROUND_REG ((CUM), (MODE)) < NPARM_REGS (MODE)))

/* Define where to put the arguments to a function.
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

   On SH the first args are normally in registers
   and the rest are pushed.  Any arg that starts within the first
   NPARM_REGS words is at least partially passed in a register unless
   its data type forbids.  */

extern int current_function_varargs;

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  ((PASS_IN_REG_P ((CUM), (MODE), (TYPE))				\
    && ((NAMED)								\
	|| (! TARGET_HITACHI && (TARGET_SH3E || ! current_function_varargs)))) \
   ? gen_rtx (REG, (MODE),						\
	      ((BASE_ARG_REG (MODE) + ROUND_REG ((CUM), (MODE))) 	\
	       ^ ((MODE) == SFmode && TARGET_SH4			\
		  && TARGET_LITTLE_ENDIAN != 0)))			\
   : 0)

#define PRETEND_OUTGOING_VARARGS_NAMED (! TARGET_HITACHI)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.

   We sometimes split args.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  ((PASS_IN_REG_P ((CUM), (MODE), (TYPE))			\
    && ! TARGET_SH4						\
    && (ROUND_REG ((CUM), (MODE))				\
	+ ((MODE) != BLKmode					\
	   ? ROUND_ADVANCE (GET_MODE_SIZE (MODE))		\
	   : ROUND_ADVANCE (int_size_in_bytes (TYPE)))		\
	- NPARM_REGS (MODE) > 0))				\
   ? NPARM_REGS (MODE) - ROUND_REG ((CUM), (MODE))		\
   : 0)

extern int current_function_anonymous_args;

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.  */

#define SETUP_INCOMING_VARARGS(ASF, MODE, TYPE, PAS, ST) \
  current_function_anonymous_args = 1;

/* Call the function profiler with a given profile label.
   We use two .aligns, so as to make sure that both the .long is aligned
   on a 4 byte boundary, and that the .long is a fixed distance (2 bytes)
   from the trapa instruction.  */

#define FUNCTION_PROFILER(STREAM,LABELNO)			\
{								\
	fprintf((STREAM), "\t.align\t2\n");			\
	fprintf((STREAM), "\ttrapa\t#33\n");			\
 	fprintf((STREAM), "\t.align\t2\n");			\
	asm_fprintf((STREAM), "\t.long\t%LLP%d\n", (LABELNO));	\
}

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */

#define PROFILE_BEFORE_PROLOGUE

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Generate the assembly code for function exit
   Just dump out any accumulated constant table.  */

#define FUNCTION_EPILOGUE(STREAM, SIZE)  function_epilogue ((STREAM), (SIZE))

/* 
   On the SH, the trampoline looks like
   2 0002 DD02     	   	mov.l	l2,r13
   1 0000 D301     		mov.l	l1,r3
   3 0004 4D2B     		jmp	@r13
   4 0006 0009     		nop
   5 0008 00000000 	l1:  	.long   function
   6 000c 00000000 	l2:	.long   area  */

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  16

/* Alignment required for a trampoline in bits .  */
#define TRAMPOLINE_ALIGNMENT \
  ((CACHE_LOG < 3 || TARGET_SMALLCODE && ! TARGET_HARWARD) ? 32 : 64)

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, (TRAMP)),			\
		  GEN_INT (TARGET_LITTLE_ENDIAN ? 0xd301dd02 : 0xdd02d301));\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 4)),	\
		  GEN_INT (TARGET_LITTLE_ENDIAN ? 0x00094d2b : 0x4d2b0009));\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 8)),	\
		  (CXT));						\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 12)),	\
		  (FNADDR));						\
  if (TARGET_HARWARD)							\
    emit_insn (gen_ic_invalidate_line (TRAMP));				\
}

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is already the frame pointer of the COUNT frame, so we
   can ignore COUNT.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)	\
  (((COUNT) == 0)				\
   ? gen_rtx (MEM, Pmode, gen_rtx (REG, Pmode, RETURN_ADDRESS_POINTER_REGNUM)) \
   : (rtx) 0)

/* Generate necessary RTL for __builtin_saveregs().
   ARGLIST is the argument list; see expr.c.  */
extern struct rtx_def *sh_builtin_saveregs ();
#define EXPAND_BUILTIN_SAVEREGS(ARGLIST) sh_builtin_saveregs (ARGLIST)

/* Addressing modes, and classification of registers for them.  */
#define HAVE_POST_INCREMENT  1
/*#define HAVE_PRE_INCREMENT   1*/
/*#define HAVE_POST_DECREMENT  1*/
#define HAVE_PRE_DECREMENT   1

#define USE_LOAD_POST_INCREMENT(mode)    ((mode == SImode || mode == DImode) \
                                           ? 0 : 1)
#define USE_LOAD_PRE_DECREMENT(mode)     0
#define USE_STORE_POST_INCREMENT(mode)   0
#define USE_STORE_PRE_DECREMENT(mode)    ((mode == SImode || mode == DImode) \
                                           ? 0 : 1)

#define MOVE_BY_PIECES_P(SIZE, ALIGN)  (move_by_pieces_ninsns (SIZE, ALIGN) \
                                        < (TARGET_SMALLCODE ? 2 :           \
                                           ((ALIGN >= 4) ? 16 : 2)))

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < PR_REG || (unsigned) reg_renumber[(REGNO)] < PR_REG)
#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) == 0 || (unsigned) reg_renumber[(REGNO)] == 0)

/* Maximum number of registers that can appear in a valid memory
   address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)	(GET_CODE (X) == LABEL_REF)

/* Nonzero if the constant value X is a legitimate general operand.  */

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE (X) != CONST_DOUBLE						\
   || GET_MODE (X) == DFmode || GET_MODE (X) == SFmode			\
   || (TARGET_SH3E && (fp_zero_operand (X) || fp_one_operand (X))))

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (REGNO (X) <= 16 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) == 0 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X/OFFSET is a hard reg that can be used as an index
   or if X is a pseudo reg.  */
#define SUBREG_OK_FOR_INDEX_P(X, OFFSET) \
  ((REGNO (X) == 0 && OFFSET == 0) || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#else

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) \
  REGNO_OK_FOR_BASE_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) \
  REGNO_OK_FOR_INDEX_P (REGNO (X))

/* Nonzero if X/OFFSET is a hard reg that can be used as an index.  */
#define SUBREG_OK_FOR_INDEX_P(X, OFFSET) \
  (REGNO_OK_FOR_INDEX_P (REGNO (X)) && (OFFSET) == 0)

#endif

/* The 'Q' constraint is a pc relative load operand.  */
#define EXTRA_CONSTRAINT_Q(OP)                          		\
  (GET_CODE (OP) == MEM && 						\
   ((GET_CODE (XEXP ((OP), 0)) == LABEL_REF)				\
    || (GET_CODE (XEXP ((OP), 0)) == CONST                		\
	&& GET_CODE (XEXP (XEXP ((OP), 0), 0)) == PLUS 			\
	&& GET_CODE (XEXP (XEXP (XEXP ((OP), 0), 0), 0)) == LABEL_REF	\
	&& GET_CODE (XEXP (XEXP (XEXP ((OP), 0), 0), 1)) == CONST_INT)))

#define EXTRA_CONSTRAINT(OP, C)		\
  ((C) == 'Q' ? EXTRA_CONSTRAINT_Q (OP)	\
   : 0)

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS.  */

#define MODE_DISP_OK_4(X,MODE) \
(GET_MODE_SIZE (MODE) == 4 && (unsigned) INTVAL (X) < 64	\
 && ! (INTVAL (X) & 3) && ! (TARGET_SH3E && (MODE) == SFmode))

#define MODE_DISP_OK_8(X,MODE) \
((GET_MODE_SIZE(MODE)==8) && ((unsigned)INTVAL(X)<60)	\
 && ! (INTVAL(X) & 3) && ! (TARGET_SH4 && (MODE) == DFmode))

#define BASE_REGISTER_RTX_P(X)				\
  ((GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))	\
   || (GET_CODE (X) == SUBREG				\
       && GET_CODE (SUBREG_REG (X)) == REG		\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

/* Since this must be r0, which is a single register class, we must check
   SUBREGs more carefully, to be sure that we don't accept one that extends
   outside the class.  */
#define INDEX_REGISTER_RTX_P(X)				\
  ((GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))	\
   || (GET_CODE (X) == SUBREG				\
       && GET_CODE (SUBREG_REG (X)) == REG		\
       && SUBREG_OK_FOR_INDEX_P (SUBREG_REG (X), SUBREG_WORD (X))))

/* Jump to LABEL if X is a valid address RTX.  This must also take
   REG_OK_STRICT into account when deciding about valid registers, but it uses
   the above macros so we are in luck.

   Allow  REG
	  REG+disp
	  REG+r0
	  REG++
	  --REG  */

/* ??? The SH3e does not have the REG+disp addressing mode when loading values
   into the FRx registers.  We implement this by setting the maximum offset
   to zero when the value is SFmode.  This also restricts loading of SFmode
   values into the integer registers, but that can't be helped.  */

/* The SH allows a displacement in a QI or HI amode, but only when the
   other operand is R0. GCC doesn't handle this very well, so we forgo
   all of that.

   A legitimate index for a QI or HI is 0, SI can be any number 0..63,
   DI can be any number 0..60.  */

#define GO_IF_LEGITIMATE_INDEX(MODE, OP, LABEL)  			\
  do {									\
    if (GET_CODE (OP) == CONST_INT) 					\
      {									\
	if (MODE_DISP_OK_4 ((OP), (MODE)))  goto LABEL;		      	\
	if (MODE_DISP_OK_8 ((OP), (MODE)))  goto LABEL;		      	\
      }									\
  } while(0)

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
{									\
  if (BASE_REGISTER_RTX_P (X))						\
    goto LABEL;								\
  else if ((GET_CODE (X) == POST_INC || GET_CODE (X) == PRE_DEC)	\
	   && BASE_REGISTER_RTX_P (XEXP ((X), 0)))			\
    goto LABEL;								\
  else if (GET_CODE (X) == PLUS						\
	   && ((MODE) != PSImode || reload_completed))			\
    {									\
      rtx xop0 = XEXP ((X), 0);						\
      rtx xop1 = XEXP ((X), 1);						\
      if (GET_MODE_SIZE (MODE) <= 8 && BASE_REGISTER_RTX_P (xop0))	\
	GO_IF_LEGITIMATE_INDEX ((MODE), xop1, LABEL);			\
      if (GET_MODE_SIZE (MODE) <= 4					\
	  || TARGET_SH4 && TARGET_FMOVD && MODE == DFmode)	\
	{								\
	  if (BASE_REGISTER_RTX_P (xop1) && INDEX_REGISTER_RTX_P (xop0))\
	    goto LABEL;							\
	  if (INDEX_REGISTER_RTX_P (xop1) && BASE_REGISTER_RTX_P (xop0))\
	    goto LABEL;							\
	}								\
    }									\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the SH, if X is almost suitable for indexing, but the offset is
   out of range, convert it into a normal form so that cse has a chance
   of reducing the number of address registers used.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{								\
  if (GET_CODE (X) == PLUS					\
      && (GET_MODE_SIZE (MODE) == 4				\
	  || GET_MODE_SIZE (MODE) == 8)				\
      && GET_CODE (XEXP ((X), 1)) == CONST_INT			\
      && BASE_REGISTER_RTX_P (XEXP ((X), 0))			\
      && ! (TARGET_SH4 && (MODE) == DFmode)			\
      && ! (TARGET_SH3E && (MODE) == SFmode))			\
    {								\
      rtx index_rtx = XEXP ((X), 1);				\
      HOST_WIDE_INT offset = INTVAL (index_rtx), offset_base;	\
      rtx sum;							\
								\
      GO_IF_LEGITIMATE_INDEX ((MODE), index_rtx, WIN);		\
      /* On rare occasions, we might get an unaligned pointer	\
	 that is indexed in a way to give an aligned address.	\
	 Therefore, keep the lower two bits in offset_base.  */ \
      /* Instead of offset_base 128..131 use 124..127, so that	\
	 simple add suffices.  */				\
      if (offset > 127)						\
	{							\
	  offset_base = ((offset + 4) & ~60) - 4;		\
	}							\
      else							\
	offset_base = offset & ~60;				\
      /* Sometimes the normal form does not suit DImode.  We	\
	 could avoid that by using smaller ranges, but that	\
	 would give less optimized code when SImode is		\
	 prevalent.  */						\
      if (GET_MODE_SIZE (MODE) + offset - offset_base <= 64)	\
	{							\
	  sum = expand_binop (Pmode, add_optab, XEXP ((X), 0),	\
			      GEN_INT (offset_base), NULL_RTX, 0, \
			      OPTAB_LIB_WIDEN);			\
                                                                \
	  (X) = gen_rtx (PLUS, Pmode, sum, GEN_INT (offset - offset_base)); \
	  goto WIN;						\
	}							\
    }								\
}

/* A C compound statement that attempts to replace X, which is an address
   that needs reloading, with a valid memory address for an operand of
   mode MODE.  WIN is a C statement label elsewhere in the code.

   Like for LEGITIMIZE_ADDRESS, for the SH we try to get a normal form
   of the address.  That will allow inheritance of the address reloads.  */

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_LEVELS,WIN)	\
{									\
  if (GET_CODE (X) == PLUS						\
      && (GET_MODE_SIZE (MODE) == 4 || GET_MODE_SIZE (MODE) == 8)	\
      && GET_CODE (XEXP (X, 1)) == CONST_INT				\
      && BASE_REGISTER_RTX_P (XEXP (X, 0))				\
      && ! (TARGET_SH4 && (MODE) == DFmode)				\
      && ! ((MODE) == PSImode && (TYPE) == RELOAD_FOR_INPUT_ADDRESS))	\
    {									\
      rtx index_rtx = XEXP (X, 1);					\
      HOST_WIDE_INT offset = INTVAL (index_rtx), offset_base;		\
      rtx sum;								\
									\
      if (TARGET_SH3E && MODE == SFmode)				\
	{								\
	  X = copy_rtx (X);						\
	  push_reload (index_rtx, NULL_RTX, &XEXP (X, 1), NULL_PTR,	\
		       INDEX_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM),	\
		       (TYPE));						\
	  goto WIN;							\
	}								\
      /* Instead of offset_base 128..131 use 124..127, so that		\
	 simple add suffices.  */					\
      if (offset > 127)							\
	{								\
	  offset_base = ((offset + 4) & ~60) - 4;			\
	}								\
      else								\
	offset_base = offset & ~60;					\
      /* Sometimes the normal form does not suit DImode.  We		\
	 could avoid that by using smaller ranges, but that		\
	 would give less optimized code when SImode is			\
	 prevalent.  */							\
      if (GET_MODE_SIZE (MODE) + offset - offset_base <= 64)		\
	{								\
	  sum = gen_rtx (PLUS, Pmode, XEXP (X, 0),			\
			 GEN_INT (offset_base));			\
	  X = gen_rtx (PLUS, Pmode, sum, GEN_INT (offset - offset_base));\
	  push_reload (sum, NULL_RTX, &XEXP (X, 0), NULL_PTR,	\
		       BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM),	\
		       (TYPE));						\
	  goto WIN;							\
	}								\
    }									\
  /* We must re-recognize what we created before.  */			\
  else if (GET_CODE (X) == PLUS						\
	   && (GET_MODE_SIZE (MODE) == 4 || GET_MODE_SIZE (MODE) == 8)	\
	   && GET_CODE (XEXP (X, 0)) == PLUS				\
	   && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT		\
	   && BASE_REGISTER_RTX_P (XEXP (XEXP (X, 0), 0))		\
	   && GET_CODE (XEXP (X, 1)) == CONST_INT			\
	   && ! (TARGET_SH3E && MODE == SFmode))			\
    {									\
      /* Because this address is so complex, we know it must have	\
	 been created by LEGITIMIZE_RELOAD_ADDRESS before; thus,	\
	 it is already unshared, and needs no further unsharing.  */	\
      push_reload (XEXP ((X), 0), NULL_RTX, &XEXP ((X), 0), NULL_PTR,	\
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0, (OPNUM), (TYPE));\
      goto WIN;								\
    }									\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   ??? Strictly speaking, we should also include all indexed addressing,
   because the index scale factor is the length of the operand.
   However, the impact of GO_IF_MODE_DEPENDENT_ADDRESS would be to
   high if we did that.  So we rely on reload to fix things up.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)			\
{									\
  if (GET_CODE(ADDR) == PRE_DEC || GET_CODE(ADDR) == POST_INC)		\
    goto LABEL;								\
}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_BIGTABLE ? SImode : HImode)

#define CASE_VECTOR_SHORTEN_MODE(MIN_OFFSET, MAX_OFFSET, BODY) \
((MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 127 \
 ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 0, QImode) \
 : (MIN_OFFSET) >= 0 && (MAX_OFFSET) <= 255 \
 ? (ADDR_DIFF_VEC_FLAGS (BODY).offset_unsigned = 1, QImode) \
 : (MIN_OFFSET) >= -32768 && (MAX_OFFSET) <= 32767 ? HImode \
 : SImode)

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE 1

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR  FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR  TRUNC_DIV_EXPR

/* Since the SH3e has only `float' support, it is desirable to make all
   floating point types equivalent to `float'.  */
#define DOUBLE_TYPE_SIZE ((TARGET_SH3E && ! TARGET_SH4) ? 32 : 64)

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  1

/* The type of size_t unsigned int.  */
#define SIZE_TYPE "unsigned int"

#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Don't cse the address of the function being compiled.  */
/*#define NO_RECURSIVE_FUNCTION_CSE 1*/

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Max number of bytes we want move_by_pieces to be able to copy
   efficiently.  */
#define MOVE_MAX_PIECES (TARGET_SH4 ? 8 : 4)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* Define this if zero-extension is slow (more than one real instruction).
   On the SH, it's only one instruction.  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is no faster than for words.  */
#define SLOW_BYTE_ACCESS 1

/* Force sizeof(bool) == 1 to maintain binary compatibility; otherwise, the
   change in SLOW_BYTE_ACCESS would have changed it to 4.  */

#define BOOL_TYPE_SIZE (flag_new_abi ? INT_TYPE_SIZE : CHAR_TYPE_SIZE)

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Immediate shift counts are truncated by the output routines (or was it
   the assembler?).  Shift counts in a register are truncated by SH.  Note
   that the native compiler puts too large (> 32) immediate shift counts
   into a register and shifts by the register, letting the SH decide what
   to do instead of doing that itself.  */
/* ??? The library routines in lib1funcs.asm truncate the shift count.
   However, the SH3 has hardware shifts that do not truncate exactly as gcc
   expects - the sign bit is significant - so it appears that we need to
   leave this zero for correct SH3 code.  */
#define SHIFT_COUNT_TRUNCATED (! TARGET_SH3)

/* All integers have the same format so truncation is easy.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
/*#define NO_FUNCTION_CSE 1*/

/* Chars and shorts should be passed as ints.  */
#define PROMOTE_PROTOTYPES 1

/* The machine modes of pointers and functions.  */
#define Pmode  SImode
#define FUNCTION_MODE  Pmode

/* The relative costs of various types of constants.  Note that cse.c defines
   REG = 1, SUBREG = 2, any node = (2 + sum of subnodes).  */

#define CONST_COSTS(RTX, CODE, OUTER_CODE)	\
  case CONST_INT:				\
    if (INTVAL (RTX) == 0)			\
      return 0;					\
    else if (CONST_OK_FOR_I (INTVAL (RTX)))	\
      return 1;					\
    else if (((OUTER_CODE) == AND || (OUTER_CODE) == IOR || (OUTER_CODE) == XOR) \
	     && CONST_OK_FOR_L (INTVAL (RTX)))	\
      return 1;					\
    else					\
      return 8;					\
  case CONST: 					\
  case LABEL_REF:				\
  case SYMBOL_REF:				\
    return 5;					\
  case CONST_DOUBLE:				\
      return 10;

#define RTX_COSTS(X, CODE, OUTER_CODE)			\
  case PLUS:						\
    return (COSTS_N_INSNS (1)				\
	    + rtx_cost (XEXP ((X), 0), PLUS)		\
	    + (rtx_equal_p (XEXP ((X), 0), XEXP ((X), 1))\
	       ? 0 : rtx_cost (XEXP ((X), 1), PLUS)));\
  case AND:						\
    return COSTS_N_INSNS (andcosts (X));		\
  case MULT:						\
    return COSTS_N_INSNS (multcosts (X));		\
  case ASHIFT:						\
  case ASHIFTRT:					\
  case LSHIFTRT:					\
    /* Add one extra unit for the matching constraint.	\
       Otherwise loop strength reduction would think that\
       a shift with different sourc and destination is	\
       as cheap as adding a constant to a register.  */	\
    return (COSTS_N_INSNS (shiftcosts (X))		\
	    + rtx_cost (XEXP ((X), 0), (CODE))		\
	    + 1);					\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (20);				\
  case FLOAT:						\
  case FIX:						\
    return 100;

/* The multiply insn on the SH1 and the divide insns on the SH1 and SH2
   are actually function calls with some special constraints on arguments
   and register usage.

   These macros tell reorg that the references to arguments and
   register clobbers for insns of type sfunc do not appear to happen
   until after the millicode call.  This allows reorg to put insns
   which set the argument registers into the delay slot of the millicode
   call -- thus they act more like traditional CALL_INSNs.

   get_attr_is_sfunc will try to recognize the given insn, so make sure to
   filter out things it will not accept -- SEQUENCE, USE and CLOBBER insns
   in particular.  */

#define INSN_SETS_ARE_DELAYED(X) 		\
  ((GET_CODE (X) == INSN			\
    && GET_CODE (PATTERN (X)) != SEQUENCE	\
    && GET_CODE (PATTERN (X)) != USE		\
    && GET_CODE (PATTERN (X)) != CLOBBER	\
    && get_attr_is_sfunc (X)))

#define INSN_REFERENCES_ARE_DELAYED(X) 		\
  ((GET_CODE (X) == INSN			\
    && GET_CODE (PATTERN (X)) != SEQUENCE	\
    && GET_CODE (PATTERN (X)) != USE		\
    && GET_CODE (PATTERN (X)) != CLOBBER	\
    && get_attr_is_sfunc (X)))

/* Compute the cost of an address.  For the SH, all valid addresses are
   the same cost.  */
/* ??? Perhaps we should make reg+reg addresses have higher cost because
   they add to register pressure on r0.  */

#define ADDRESS_COST(RTX) 1

/* Compute extra cost of moving data between one register class
   and another.  */

/* Regclass always uses 2 for moves in the same register class;
   If SECONDARY*_RELOAD_CLASS says something about the src/dst pair,
   it uses this information.  Hence, the general register <-> floating point
   register information here is not used for SFmode.  */
#define REGISTER_MOVE_COST(SRCCLASS, DSTCLASS) \
  ((((DSTCLASS) == T_REGS) || ((DSTCLASS) == PR_REG)) ? 10		\
   : ((((DSTCLASS) == FP0_REGS || (DSTCLASS) == FP_REGS || (DSTCLASS) == DF_REGS) \
       && ((SRCCLASS) == GENERAL_REGS || (SRCCLASS) == R0_REGS))	\
      || (((DSTCLASS) == GENERAL_REGS || (DSTCLASS) == R0_REGS)		\
	  && ((SRCCLASS) == FP0_REGS || (SRCCLASS) == FP_REGS		\
	      || (SRCCLASS) == DF_REGS)))				\
   ? TARGET_FMOVD ? 8 : 12						\
   : (((DSTCLASS) == FPUL_REGS						\
       && ((SRCCLASS) == GENERAL_REGS || (SRCCLASS) == R0_REGS))	\
      || (SRCCLASS == FPUL_REGS						\
	  && ((DSTCLASS) == GENERAL_REGS || (DSTCLASS) == R0_REGS)))	\
   ? 5									\
   : (((DSTCLASS) == FPUL_REGS						\
       && ((SRCCLASS) == PR_REGS || (SRCCLASS) == MAC_REGS))		\
      || ((SRCCLASS) == FPUL_REGS					\
	  && ((DSTCLASS) == PR_REGS || (DSTCLASS) == MAC_REGS)))	\
   ? 7									\
   : 2)

/* ??? Perhaps make MEMORY_MOVE_COST depend on compiler option?  This
   would be so that people would slow memory systems could generate
   different code that does fewer memory accesses.  */

/* Assembler output control.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */
#define ASM_COMMENT_START "!"

/* The text to go at the start of the assembler file.  */
#define ASM_FILE_START(STREAM) \
  output_file_start (STREAM)

#define ASM_FILE_END(STREAM)

#define ASM_APP_ON  		""
#define ASM_APP_OFF  		""
#define FILE_ASM_OP 		"\t.file\n"
#define IDENT_ASM_OP 		"\t.ident\n"
#define SET_ASM_OP		".set"

/* How to change between sections.  */

#define TEXT_SECTION_ASM_OP  		"\t.text"
#define DATA_SECTION_ASM_OP  		"\t.data"
#define CTORS_SECTION_ASM_OP 		"\t.section\t.ctors\n"
#define DTORS_SECTION_ASM_OP 		"\t.section\t.dtors\n"
#define EXTRA_SECTIONS 			in_ctors, in_dtors
#define EXTRA_SECTION_FUNCTIONS					\
void								\
ctors_section()							\
{								\
  if (in_section != in_ctors)					\
    {								\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);	\
      in_section = in_ctors;					\
    }								\
}								\
void								\
dtors_section()							\
{								\
  if (in_section != in_dtors)					\
    {								\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);	\
      in_section = in_dtors;					\
    }								\
}

/* Define this so that jump tables go in same section as the current function,
   which could be text or it could be a user defined section.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC) \
   do { fprintf (FILE, ".section\t%s\n", NAME); } while (0)

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME) \
   do { ctors_section();  asm_fprintf((FILE),"\t.long\t%U%s\n", (NAME)); } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME) \
   do {  dtors_section();  asm_fprintf((FILE),"\t.long\t%U%s\n", (NAME)); } while (0)

#undef DO_GLOBAL_CTORS_BODY

#define DO_GLOBAL_CTORS_BODY			\
{						\
  typedef (*pfunc)();				\
  extern pfunc __ctors[];			\
  extern pfunc __ctors_end[];			\
  pfunc *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}

#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY			\
{						\
  typedef (*pfunc)();				\
  extern pfunc __dtors[];			\
  extern pfunc __dtors_end[];			\
  pfunc *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}

#define ASM_OUTPUT_REG_PUSH(file, v) \
  fprintf ((file), "\tmov.l\tr%s,-@r15\n", (v));

#define ASM_OUTPUT_REG_POP(file, v) \
  fprintf ((file), "\tmov.l\t@r15+,r%s\n", (v));

/* The assembler's names for the registers.  RFP need not always be used as
   the Real framepointer; it can also be used as a normal general register.
   Note that the name `fp' is horribly misleading since `fp' is in fact only
   the argument-and-return-context pointer.  */

extern char fp_reg_names[][5];

#define REGISTER_NAMES  				\
{				                   	\
  "r0", "r1", "r2",  "r3",  "r4",  "r5",  "r6",  "r7", 	\
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",	\
  "ap", "pr", "t",   "gbr", "mach","macl", fp_reg_names[16], "rap", \
  fp_reg_names[0],  fp_reg_names[1] , fp_reg_names[2],  fp_reg_names[3], \
  fp_reg_names[4],  fp_reg_names[5],  fp_reg_names[6],  fp_reg_names[7], \
  fp_reg_names[8],  fp_reg_names[9],  fp_reg_names[10], fp_reg_names[11], \
  fp_reg_names[12], fp_reg_names[13], fp_reg_names[14], fp_reg_names[15], \
  fp_reg_names[17], fp_reg_names[18], fp_reg_names[19], fp_reg_names[20], \
  fp_reg_names[21], fp_reg_names[22], fp_reg_names[23], fp_reg_names[24], \
  "fpscr", \
}

#define DEBUG_REGISTER_NAMES  				\
{				                   	\
  "r0", "r1", "r2",  "r3",  "r4",  "r5",  "r6",  "r7", 	\
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",	\
  "ap", "pr", "t",  "gbr", "mach","macl", "fpul","rap", \
  "fr0","fr1","fr2", "fr3", "fr4", "fr5", "fr6", "fr7", \
  "fr8","fr9","fr10","fr11","fr12","fr13","fr14","fr15",\
  "xd0","xd2","xd4", "xd6", "xd8", "xd10","xd12","xd14", \
  "fpscr", \
}

/* DBX register number for a given compiler register number.  */
/* GDB has FPUL at 23 and FP0 at 25, so we must add one to all FP registers
   to match gdb.  */
#define DBX_REGISTER_NUMBER(REGNO)	\
  (((REGNO) >= 22 && (REGNO) <= 39) ? ((REGNO) + 1) : (REGNO))

/* Output a label definition.  */
#define ASM_OUTPUT_LABEL(FILE,NAME) \
  do { assemble_name ((FILE), (NAME)); fputs (":\n", (FILE)); } while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf ((FILE), "\t.align %d\n", (LOG))

/* Output a function label definition.  */
#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL) \
    ASM_OUTPUT_LABEL((STREAM), (NAME))

/* Output a globalising directive for a label.  */
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)	\
  (fprintf ((STREAM), "\t.global\t"),		\
   assemble_name ((STREAM), (NAME)),		\
   fputc ('\n', (STREAM)))

/* The prefix to add to user-visible assembler symbols. */

#define USER_LABEL_PREFIX "_"

/* The prefix to add to an internally generated label. */

#define LOCAL_LABEL_PREFIX ""

/* Make an internal label into a string.  */
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf ((STRING), "*%s%s%d", LOCAL_LABEL_PREFIX, (PREFIX), (NUM))

/* Output an internal label definition.  */
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM) \
  asm_fprintf ((FILE), "%L%s%d:\n", (PREFIX), (NUM))

/* #define ASM_OUTPUT_CASE_END(STREAM,NUM,TABLE)	    */

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)	\
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),	\
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Output a relative address table.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)  		\
  switch (GET_MODE (BODY))						\
    {									\
    case SImode:							\
      asm_fprintf ((STREAM), "\t.long\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case HImode:							\
      asm_fprintf ((STREAM), "\t.word\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    case QImode:							\
      asm_fprintf ((STREAM), "\t.byte\t%LL%d-%LL%d\n", (VALUE),(REL));	\
      break;								\
    }

/* Output an absolute table element.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  				\
  if (TARGET_BIGTABLE) 							\
    asm_fprintf ((STREAM), "\t.long\t%LL%d\n", (VALUE)); 			\
  else									\
    asm_fprintf ((STREAM), "\t.word\t%LL%d\n", (VALUE)); 			\

/* Output various types of constants.  */

/* This is how to output an assembler line defining a `double'.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf ((FILE), "\t.double %s\n", dstr);		\
   } while (0)

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf ((FILE), "\t.float %s\n", dstr);		\
   } while (0)

#define ASM_OUTPUT_INT(STREAM, EXP)		\
  (fprintf ((STREAM), "\t.long\t"),      	\
   output_addr_const ((STREAM), (EXP)),  	\
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_SHORT(STREAM, EXP)	\
  (fprintf ((STREAM), "\t.short\t"),	\
   output_addr_const ((STREAM), (EXP)),	\
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_CHAR(STREAM, EXP)		\
  (fprintf ((STREAM), "\t.byte\t"),      	\
   output_addr_const ((STREAM), (EXP)),  	\
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_BYTE(STREAM, VALUE)  	\
  fprintf ((STREAM), "\t.byte\t%d\n", (VALUE)) 	\

/* The next two are used for debug info when compiling with -gdwarf.  */
#define UNALIGNED_SHORT_ASM_OP	".uaword"
#define UNALIGNED_INT_ASM_OP	".ualong"

/* Loop alignment is now done in machine_dependent_reorg, so that
   branch shortening can know about it.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE) \
  fprintf ((FILE), "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* The assembler's parentheses characters.  */
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Target characters.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015

/* A C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.
   The contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler output
   by changing the contents of the vector.  */

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  final_prescan_insn ((INSN), (OPVEC), (NOPERANDS))

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(STREAM, X, CODE)  print_operand ((STREAM), (X), (CODE))

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(STREAM,X)  print_operand_address ((STREAM), (X))

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR) == '.' || (CHAR) == '#' || (CHAR) == '@' || (CHAR) == ','	\
   || (CHAR) == '$')

extern struct rtx_def *sh_compare_op0;
extern struct rtx_def *sh_compare_op1;
extern struct rtx_def *prepare_scc_operands();

/* Which processor to schedule for.  The elements of the enumeration must
   match exactly the cpu attribute in the sh.md file.  */

enum processor_type {
  PROCESSOR_SH1,
  PROCESSOR_SH2,
  PROCESSOR_SH3,
  PROCESSOR_SH3E,
  PROCESSOR_SH4
};

#define sh_cpu_attr ((enum attr_cpu)sh_cpu)
extern enum processor_type sh_cpu;

extern enum machine_mode sh_addr_diff_vec_mode;

extern int optimize; /* needed for gen_casesi.  */

/* Declare functions defined in sh.c and used in templates.  */

extern char *output_branch();
extern char *output_ieee_ccmpeq();
extern char *output_branchy_insn();
extern char *output_shift();
extern char *output_movedouble();
extern char *output_movepcrel();
extern char *output_jump_label_table();
extern char *output_far_jump();

enum mdep_reorg_phase_e
{
  SH_BEFORE_MDEP_REORG,
  SH_INSERT_USES_LABELS,
  SH_SHORTEN_BRANCHES0,
  SH_FIXUP_PCLOAD,
  SH_SHORTEN_BRANCHES1,
  SH_AFTER_MDEP_REORG
};

extern enum mdep_reorg_phase_e mdep_reorg_phase;

void machine_dependent_reorg ();
struct rtx_def *sfunc_uses_reg ();
int barrier_align ();

#define MACHINE_DEPENDENT_REORG(X) machine_dependent_reorg(X)

/* Generate calls to memcpy, memcmp and memset.  */

#define TARGET_MEM_FUNCTIONS

/* Define this macro if you want to implement any pragmas.  If defined, it
   is a C expression whose value is 1 if the pragma was handled by the
   macro, zero otherwise.  */
#define HANDLE_PRAGMA(GETC, UNGETC, NODE) sh_handle_pragma (GETC, UNGETC, NODE)
extern int sh_handle_pragma ();

/* Set when processing a function with pragma interrupt turned on.  */

extern int pragma_interrupt;

/* Set to an RTX containing the address of the stack to switch to
   for interrupt functions.  */
extern struct rtx_def *sp_switch;

/* A C expression whose value is nonzero if IDENTIFIER with arguments ARGS
   is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */
extern int sh_valid_machine_decl_attribute ();
#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, IDENTIFIER, ARGS) \
sh_valid_machine_decl_attribute (DECL, ATTRIBUTES, IDENTIFIER, ARGS)

extern void sh_pragma_insert_attributes ();
#define PRAGMA_INSERT_ATTRIBUTES(node, pattr, prefix_attr) \
  sh_pragma_insert_attributes (node, pattr, prefix_attr)

extern int sh_flag_remove_dead_before_cse;
extern int rtx_equal_function_value_matters;
extern struct rtx_def *fpscr_rtx;
extern struct rtx_def *get_fpscr_rtx ();


/* Instructions with unfilled delay slots take up an extra two bytes for
   the nop in the delay slot.  */

#define ADJUST_INSN_LENGTH(X, LENGTH)				\
  if (((GET_CODE (X) == INSN					\
	&& GET_CODE (PATTERN (X)) != USE			\
	&& GET_CODE (PATTERN (X)) != CLOBBER)			\
       || GET_CODE (X) == CALL_INSN				\
       || (GET_CODE (X) == JUMP_INSN				\
	   && GET_CODE (PATTERN (X)) != ADDR_DIFF_VEC		\
	   && GET_CODE (PATTERN (X)) != ADDR_VEC))		\
      && GET_CODE (PATTERN (NEXT_INSN (PREV_INSN (X)))) != SEQUENCE \
      && get_attr_needs_delay_slot (X) == NEEDS_DELAY_SLOT_YES)	\
    (LENGTH) += 2;

/* Define the codes that are matched by predicates in sh.c.  */
#define PREDICATE_CODES \
  {"arith_operand", {SUBREG, REG, CONST_INT}},				\
  {"arith_reg_operand", {SUBREG, REG}},					\
  {"arith_reg_or_0_operand", {SUBREG, REG, CONST_INT}},			\
  {"binary_float_operator", {PLUS, MULT}},				\
  {"braf_label_ref_operand", {LABEL_REF}},				\
  {"commutative_float_operator", {PLUS, MULT}},				\
  {"fp_arith_reg_operand", {SUBREG, REG}},				\
  {"fp_extended_operand", {SUBREG, REG, FLOAT_EXTEND}},			\
  {"fpscr_operand", {REG}},						\
  {"general_movsrc_operand", {SUBREG, REG, CONST_INT, MEM}},		\
  {"general_movdst_operand", {SUBREG, REG, CONST_INT, MEM}},		\
  {"logical_operand", {SUBREG, REG, CONST_INT}},			\
  {"noncommutative_float_operator", {MINUS, DIV}},			\
  {"register_operand", {SUBREG, REG}},

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Leaving the unsignedp unchanged gives better code than always setting it
   to 0.  This is despite the fact that we have only signed char and short
   load instructions.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
  if (GET_MODE_CLASS (MODE) == MODE_INT			\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)		\
    (MODE) = SImode;

/* Defining PROMOTE_FUNCTION_ARGS eliminates some unnecessary zero/sign
   extensions applied to char/short functions arguments.  Defining
   PROMOTE_FUNCTION_RETURN does the same for function returns.  */

#define PROMOTE_FUNCTION_ARGS
#define PROMOTE_FUNCTION_RETURN

/* ??? Define ACCUMULATE_OUTGOING_ARGS?  This is more efficient than pushing
   and poping arguments.  However, we do have push/pop instructions, and
   rather limited offsets (4 bits) in load/store instructions, so it isn't
   clear if this would give better code.  If implemented, should check for
   compatibility problems.  */

/* A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  This can be used for example to specify to
   the scheduler that an output- or anti-dependence does not incur
   the same cost as a data-dependence.  */

#define ADJUST_COST(insn,link,dep_insn,cost)				\
do {									\
  rtx reg;								\
									\
  if (GET_CODE(insn) == CALL_INSN)					\
    {									\
      /* The only input for a call that is timing-critical is the	\
	 function's address.  */					\
      rtx call = PATTERN (insn);					\
									\
      if (GET_CODE (call) == PARALLEL)					\
	call = XVECEXP (call, 0 ,0);					\
      if (GET_CODE (call) == SET)					\
	call = SET_SRC (call);						\
      if (GET_CODE (call) == CALL && GET_CODE (XEXP (call, 0)) == MEM	\
	  && ! reg_set_p (XEXP (XEXP (call, 0), 0), dep_insn))		\
	(cost) = 0;							\
    }									\
  /* All sfunc calls are parallels with at least four components.	\
     Exploit this to avoid unnecessary calls to sfunc_uses_reg.  */	\
  else if (GET_CODE (PATTERN (insn)) == PARALLEL			\
	   && XVECLEN (PATTERN (insn), 0) >= 4				\
	   && (reg = sfunc_uses_reg (insn)))				\
    {									\
      /* Likewise, the most timing critical input for an sfuncs call	\
	 is the function address.  However, sfuncs typically start	\
	 using their arguments pretty quickly.				\
	 Assume a four cycle delay before they are needed.  */		\
      if (! reg_set_p (reg, dep_insn))					\
	cost -= TARGET_SUPERSCALAR ? 40 : 4;				\
    }									\
  /* Adjust load_si / pcload_si type insns latency.  Use the known	\
     nominal latency and form of the insn to speed up the check.  */	\
  else if (cost == 3							\
	   && GET_CODE (PATTERN (dep_insn)) == SET			\
	   /* Latency for dmpy type insns is also 3, so check the that	\
	      it's actually a move insn.  */				\
	   && general_movsrc_operand (SET_SRC (PATTERN (dep_insn)), SImode))\
    cost = 2;								\
  else if (cost == 30							\
	   && GET_CODE (PATTERN (dep_insn)) == SET			\
	   && GET_MODE (SET_SRC (PATTERN (dep_insn))) == SImode)	\
    cost = 20;								\
} while (0)								\

/* For the sake of libgcc2.c, indicate target supports atexit.  */
#define HAVE_ATEXIT

#define SH_DYNAMIC_SHIFT_COST \
  (TARGET_HARD_SH4 ? 1 : TARGET_SH3 ? (TARGET_SMALLCODE ? 1 : 2) : 20)
