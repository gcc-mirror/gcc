/* Definitions of target machine for GNU compiler, Renesas M32R cpu.
   Copyright (C) 1996-2017 Free Software Foundation, Inc.

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

/* Things to do:
- longlong.h?
*/

#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef CPP_SPEC
#undef ASM_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC

#undef ASM_APP_ON
#undef ASM_APP_OFF


/* M32R/X overrides.  */

/* Additional flags for the preprocessor.  */
#define CPP_CPU_SPEC "%{m32rx:-D__M32RX__ -D__m32rx__ -U__M32R2__ -U__m32r2__} \
%{m32r2:-D__M32R2__ -D__m32r2__ -U__M32RX__ -U__m32rx__} \
%{m32r:-U__M32RX__  -U__m32rx__ -U__M32R2__ -U__m32r2__} \
 "

/* Assembler switches.  */
#define ASM_CPU_SPEC \
"%{m32r} %{m32rx} %{m32r2} %{!O0: %{O*: -O}} --no-warn-explicit-parallel-conflicts"

/* Use m32rx specific crt0/crtinit/crtfini files.  */
#define STARTFILE_CPU_SPEC "%{!shared:crt0.o%s} %{m32rx:m32rx/crtinit.o%s} %{!m32rx:crtinit.o%s}"
#define ENDFILE_CPU_SPEC "-lgloss %{m32rx:m32rx/crtfini.o%s} %{!m32rx:crtfini.o%s}"

/* Define this macro as a C expression for the initializer of an array of
   strings to tell the driver program which options are defaults for this
   target and thus do not need to be handled specially when using
   `MULTILIB_OPTIONS'.  */
#define SUBTARGET_MULTILIB_DEFAULTS , "m32r"

/* Number of additional registers the subtarget defines.  */
#define SUBTARGET_NUM_REGISTERS 1

/* 1 for registers that cannot be allocated.  */
#define SUBTARGET_FIXED_REGISTERS , 1

/* 1 for registers that are not available across function calls.  */
#define SUBTARGET_CALL_USED_REGISTERS , 1

/* Order to allocate model specific registers.  */
#define SUBTARGET_REG_ALLOC_ORDER , 19

/* Registers which are accumulators.  */
#define SUBTARGET_REG_CLASS_ACCUM 0x80000

/* All registers added.  */
#define SUBTARGET_REG_CLASS_ALL SUBTARGET_REG_CLASS_ACCUM

/* Additional accumulator registers.  */
#define SUBTARGET_ACCUM_P(REGNO) ((REGNO) == 19)

/* Define additional register names.  */
#define SUBTARGET_REGISTER_NAMES , "a1"
/* end M32R/X overrides.  */

/* Names to predefine in the preprocessor for this target machine.  */
/* __M32R__ is defined by the existing compiler so we use that.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__M32R__");		\
      builtin_define ("__m32r__");		\
      builtin_assert ("cpu=m32r");		\
      builtin_assert ("machine=m32r");		\
      builtin_define (TARGET_BIG_ENDIAN		\
                      ? "__BIG_ENDIAN__" : "__LITTLE_ENDIAN__"); \
    }						\
  while (0)

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GCC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#endif

#ifndef	ASM_CPU_SPEC
#define	ASM_CPU_SPEC ""
#endif

#ifndef	CPP_CPU_SPEC
#define	CPP_CPU_SPEC ""
#endif

#ifndef	CC1_CPU_SPEC
#define	CC1_CPU_SPEC ""
#endif

#ifndef	LINK_CPU_SPEC
#define	LINK_CPU_SPEC ""
#endif

#ifndef STARTFILE_CPU_SPEC
#define STARTFILE_CPU_SPEC "%{!shared:crt0.o%s} crtinit.o%s"
#endif

#ifndef ENDFILE_CPU_SPEC
#define ENDFILE_CPU_SPEC "-lgloss crtfini.o%s"
#endif

#ifndef RELAX_SPEC
#if 0 /* Not supported yet.  */
#define RELAX_SPEC "%{mrelax:-relax}"
#else
#define RELAX_SPEC ""
#endif
#endif

#define EXTRA_SPECS							\
  { "asm_cpu",			ASM_CPU_SPEC },				\
  { "cpp_cpu",			CPP_CPU_SPEC },				\
  { "cc1_cpu",			CC1_CPU_SPEC },				\
  { "link_cpu",			LINK_CPU_SPEC },			\
  { "startfile_cpu",		STARTFILE_CPU_SPEC },			\
  { "endfile_cpu",		ENDFILE_CPU_SPEC },			\
  { "relax",			RELAX_SPEC },				\
  SUBTARGET_EXTRA_SPECS

#define CPP_SPEC "%(cpp_cpu)"

#undef  CC1_SPEC
#define CC1_SPEC "%{G*} %(cc1_cpu)"

/* Options to pass on to the assembler.  */
#undef  ASM_SPEC
#define ASM_SPEC "%(asm_cpu) %(relax) %{" FPIE_OR_FPIC_SPEC ":-K PIC}"

#define LINK_SPEC "%{v} %(link_cpu) %(relax)"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%(startfile_cpu)"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "%(endfile_cpu)"

#undef LIB_SPEC

/* Run-time compilation parameters selecting different hardware subsets.  */

#define TARGET_M32R             (! TARGET_M32RX && ! TARGET_M32R2)

#ifndef TARGET_LITTLE_ENDIAN
#define TARGET_LITTLE_ENDIAN	0
#endif
#define TARGET_BIG_ENDIAN       (! TARGET_LITTLE_ENDIAN)

/* This defaults us to m32r.  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

#ifndef M32R_OPTS_H
#include "config/m32r/m32r-opts.h"
#endif

/* Define this macro as a C expression for the initializer of an array of
   strings to tell the driver program which options are defaults for this
   target and thus do not need to be handled specially when using
   `MULTILIB_OPTIONS'.  */
#ifndef SUBTARGET_MULTILIB_DEFAULTS
#define SUBTARGET_MULTILIB_DEFAULTS
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mmodel=small" SUBTARGET_MULTILIB_DEFAULTS }
#endif

#ifndef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS
#endif

/* Target machine storage layout.  */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
    {						\
      (MODE) = SImode;				\
    }

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* ALIGN FRAMES on word boundaries */
#define M32R_STACK_ALIGN(LOC) (((LOC) + 3) & ~ 3)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)	\
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  (TREE_CODE (TYPE) == ARRAY_TYPE					\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode				\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define LAVEL_ALIGN to calculate code length of PNOP at labels.  */
#define LABEL_ALIGN(insn) 2

/* Layout of source language data types.  */

#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		32
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE		32
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long int"
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

#define M32R_NUM_REGISTERS 	19

#ifndef SUBTARGET_NUM_REGISTERS
#define SUBTARGET_NUM_REGISTERS 0
#endif

#define FIRST_PSEUDO_REGISTER (M32R_NUM_REGISTERS + SUBTARGET_NUM_REGISTERS)
	
/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   0-3   - arguments/results
   4-5   - call used [4 is used as a tmp during prologue/epilogue generation]
   6     - call used, gptmp
   7     - call used, static chain pointer
   8-11  - call saved
   12    - call saved [reserved for global pointer]
   13    - frame pointer
   14    - subroutine link register
   15    - stack pointer
   16    - arg pointer
   17    - carry flag
   18	 - accumulator
   19    - accumulator 1 in the m32r/x
   By default, the extension registers are not available.  */

#ifndef SUBTARGET_FIXED_REGISTERS
#define SUBTARGET_FIXED_REGISTERS
#endif

#define FIXED_REGISTERS		\
{				\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 1,	\
  1, 1, 1			\
  SUBTARGET_FIXED_REGISTERS	\
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#ifndef SUBTARGET_CALL_USED_REGISTERS
#define SUBTARGET_CALL_USED_REGISTERS
#endif

#define CALL_USED_REGISTERS	\
{				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1			\
  SUBTARGET_CALL_USED_REGISTERS	\
}

#define CALL_REALLY_USED_REGISTERS CALL_USED_REGISTERS

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GCC should
   prefer to use them (from most preferred to least).  */

#ifndef SUBTARGET_REG_ALLOC_ORDER
#define SUBTARGET_REG_ALLOC_ORDER
#endif

#if 1 /* Better for int code.  */
#define REG_ALLOC_ORDER				\
{						\
  4,  5,  6,  7,  2,  3,  8,  9, 10,		\
  11, 12, 13, 14,  0,  1, 15, 16, 17, 18	\
  SUBTARGET_REG_ALLOC_ORDER			\
}

#else /* Better for fp code at expense of int code.  */
#define REG_ALLOC_ORDER				\
{						\
   0,  1,  2,  3,  4,  5,  6,  7,  8,		\
   9, 10, 11, 12, 13, 14, 15, 16, 17, 18	\
  SUBTARGET_REG_ALLOC_ORDER			\
}
#endif

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */
extern const unsigned int m32r_hard_regno_mode_ok[FIRST_PSEUDO_REGISTER];
extern unsigned int m32r_mode_class[];
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((m32r_hard_regno_mode_ok[REGNO] & m32r_mode_class[MODE]) != 0)

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

/* Tie QI/HI/SI modes together.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 		\
  (   GET_MODE_CLASS (MODE1) == MODE_INT	\
   && GET_MODE_CLASS (MODE2) == MODE_INT	\
   && GET_MODE_SIZE (MODE1) <= UNITS_PER_WORD	\
   && GET_MODE_SIZE (MODE2) <= UNITS_PER_WORD)

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
  m32r_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Register classes and constants.  */

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
   class that represents their union.

   It is important that any condition codes have class NO_REGS.
   See `register_operand'.  */

enum reg_class
{
  NO_REGS, CARRY_REG, ACCUM_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES \
  { "NO_REGS", "CARRY_REG", "ACCUM_REGS", "GENERAL_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#ifndef SUBTARGET_REG_CLASS_CARRY
#define SUBTARGET_REG_CLASS_CARRY 0
#endif

#ifndef SUBTARGET_REG_CLASS_ACCUM
#define SUBTARGET_REG_CLASS_ACCUM 0
#endif

#ifndef SUBTARGET_REG_CLASS_GENERAL
#define SUBTARGET_REG_CLASS_GENERAL 0
#endif

#ifndef SUBTARGET_REG_CLASS_ALL
#define SUBTARGET_REG_CLASS_ALL 0
#endif

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000 },								\
  { 0x20000 | SUBTARGET_REG_CLASS_CARRY },				\
  { 0x40000 | SUBTARGET_REG_CLASS_ACCUM },				\
  { 0x1ffff | SUBTARGET_REG_CLASS_GENERAL },				\
  { 0x7ffff | SUBTARGET_REG_CLASS_ALL },				\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
extern enum reg_class m32r_regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) (m32r_regno_reg_class[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < FIRST_PSEUDO_REGISTER			\
   ? GPR_P (REGNO) || (REGNO) == ARG_POINTER_REGNUM	\
   : GPR_P (reg_renumber[REGNO]))

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P(REGNO)

/* Return true if a value is inside a range.  */
#define IN_RANGE_P(VALUE, LOW, HIGH)			\
  (((unsigned HOST_WIDE_INT)((VALUE) - (LOW)))		\
   <= ((unsigned HOST_WIDE_INT)((HIGH) - (LOW))))

/* Some range macros.  */
#define INT16_P(X)     ((X) >= - 0x8000 && (X) <= 0x7fff)
#define CMP_INT16_P(X) ((X) >= - 0x7fff && (X) <= 0x8000)
#define UINT16_P(X)   (((unsigned HOST_WIDE_INT) (X)) <= 0x0000ffff)
#define UINT24_P(X)   (((unsigned HOST_WIDE_INT) (X)) <= 0x00ffffff)

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Offset from frame pointer to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
/* The frame pointer points at the same place as the stack pointer, except if
   alloca has been called.  */
#define STARTING_FRAME_OFFSET \
  M32R_STACK_ALIGN (crtl->outgoing_args_size)

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET 0

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 13

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 16

/* Register in which static-chain is passed to a function.
   This must not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM  7

/* These aren't official macros.  */
#define PROLOGUE_TMP_REGNUM  4
#define RETURN_ADDR_REGNUM  14
/* #define GP_REGNUM        12 */
#define CARRY_REGNUM        17
#define ACCUM_REGNUM        18
#define M32R_MAX_INT_REGS   16

#ifndef SUBTARGET_GPR_P
#define SUBTARGET_GPR_P(REGNO) 0
#endif

#ifndef SUBTARGET_ACCUM_P
#define SUBTARGET_ACCUM_P(REGNO) 0
#endif

#ifndef SUBTARGET_CARRY_P
#define SUBTARGET_CARRY_P(REGNO) 0
#endif

#define GPR_P(REGNO)   (IN_RANGE_P ((REGNO), 0, 15) || SUBTARGET_GPR_P (REGNO))
#define ACCUM_P(REGNO) ((REGNO) == ACCUM_REGNUM || SUBTARGET_ACCUM_P (REGNO))
#define CARRY_P(REGNO) ((REGNO) == CARRY_REGNUM || SUBTARGET_CARRY_P (REGNO))

/* Eliminating the frame and arg pointers.  */

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS					\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM }}

/* This macro returns the initial difference between the specified pair
   of registers.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)				\
  do										\
    {										\
      int size = m32r_compute_frame_size (get_frame_size ());			\
										\
      if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
	(OFFSET) = 0;								\
      else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)	\
	(OFFSET) = size - crtl->args.pretend_args_size;			\
      else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
	(OFFSET) = size - crtl->args.pretend_args_size;			\
      else									\
	gcc_unreachable ();								\
    }										\
  while (0)

/* Function argument passing.  */

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `crtl->outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM) = 0)

/* The number of registers used for parameter passing.  Local to this file.  */
#define M32R_MAX_PARM_REGS 4

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) \
  ((unsigned) (N) < M32R_MAX_PARM_REGS)


/* Function results.  */

/* Tell GCC to use TARGET_RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Function entry and exit.  */

/* Initialize data used by insn expanders.  This is called from
   init_emit, once for each function, before code is generated.  */
#define INIT_EXPANDERS m32r_init_expanders ()

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 1

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)			\
  do								\
    {								\
      if (flag_pic)						\
	{							\
	  fprintf (FILE, "\tld24 r14,#mcount\n");		\
	  fprintf (FILE, "\tadd r14,r12\n");			\
	  fprintf (FILE, "\tld r14,@r14\n");			\
	  fprintf (FILE, "\tjl r14\n");				\
	}							\
      else							\
	{							\
	  if (TARGET_ADDR24)					\
	    fprintf (FILE, "\tbl mcount\n");			\
	  else							\
	    {							\
	      fprintf (FILE, "\tseth r14,#high(mcount)\n");	\
	      fprintf (FILE, "\tor3 r14,r14,#low(mcount)\n");	\
	      fprintf (FILE, "\tjl r14\n");			\
	    }							\
	}							\
      fprintf (FILE, "\taddi sp,#4\n");				\
    }								\
  while (0)

/* Trampolines.  */

/* On the M32R, the trampoline is:

        mv      r7, lr   -> bl L1        ; 178e 7e01
L1:     add3    r6, lr, #L2-L1           ; 86ae 000c (L2 - L1 = 12)
        mv      lr, r7   -> ld r7,@r6+   ; 1e87 27e6
        ld      r6, @r6  -> jmp r6       ; 26c6 1fc6
L2:     .word STATIC
        .word FUNCTION  */

#ifndef CACHE_FLUSH_FUNC
#define CACHE_FLUSH_FUNC "_flush_cache"
#endif
#ifndef CACHE_FLUSH_TRAP
#define CACHE_FLUSH_TRAP 12
#endif

/* Length in bytes of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE 24


#define RETURN_ADDR_RTX(COUNT, FRAME) m32r_return_addr (COUNT)

#define INCOMING_RETURN_ADDR_RTX   gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM)

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* We have post-inc load and pre-dec,pre-inc store,
   but only for 4 byte vals.  */
#define HAVE_PRE_DECREMENT  1
#define HAVE_PRE_INCREMENT  1
#define HAVE_POST_INCREMENT 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X)   \
  (    GET_CODE (X) == LABEL_REF  \
   ||  GET_CODE (X) == SYMBOL_REF \
   ||  CONST_INT_P (X)  \
   || (GET_CODE (X) == CONST      \
       && ! (flag_pic && ! m32r_legitimate_pic_operand_p (X))))

/* Condition code usage.  */

/* Return nonzero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */
#define REVERSIBLE_CC_MODE(MODE) 1 /*???*/

/* Costs.  */

/* The cost of a branch insn.  */
/* A value of 2 here causes GCC to avoid using branches in comparisons like
   while (a < N && a).  Branches aren't that expensive on the M32R so
   we define this as 1.  Defining it as 2 had a heavy hit in fp-bit.c.  */
#define BRANCH_COST(speed_p, predictable_p) ((TARGET_BRANCH_COST) ? 2 : 1)

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Section selection.  */

#define TEXT_SECTION_ASM_OP	"\t.section .text"
#define DATA_SECTION_ASM_OP	"\t.section .data"
#define BSS_SECTION_ASM_OP	"\t.section .bss"

/* Define this macro if jump tables (for tablejump insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.
   This macro is irrelevant if there is no separate readonly data section.  */
#define JUMP_TABLES_IN_TEXT_SECTION (flag_pic)

/* Position Independent Code.  */

/* The register number of the register used to address a table of static
   data addresses in memory.  In some cases this register is defined by a
   processor's ``application binary interface'' (ABI).  When this macro
   is defined, RTL is generated for this register once, as with the stack
   pointer and frame pointer registers.  If this macro is not defined, it
   is up to the machine-dependent files to allocate such a register (if
   necessary).  */
#define PIC_OFFSET_TABLE_REGNUM 12

/* Define this macro if the register defined by PIC_OFFSET_TABLE_REGNUM is
   clobbered by calls.  Do not define this macro if PIC_OFFSET_TABLE_REGNUM
   is not defined.  */
/* This register is call-saved on the M32R.  */
/*#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED*/

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent code.
   You can assume that X satisfies CONSTANT_P, so you need not
   check this.  You can also assume `flag_pic' is true, so you need not
   check it either.  You need not define this macro if all constants
   (including SYMBOL_REF) can be immediate operands when generating
   position independent code.  */
#define LEGITIMATE_PIC_OPERAND_P(X) m32r_legitimate_pic_operand_p (X)

/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will
   end at the end of the line.  */
#define ASM_COMMENT_START ";"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */
#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */
#define ASM_APP_OFF ""

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* We do not use DBX_LINES_FUNCTION_RELATIVE or
   dbxout_stab_value_internal_label_diff here because
   we need to use .debugsym for the line label.  */

#define DBX_OUTPUT_SOURCE_LINE(file, line, counter)			\
  do									\
    {									\
      const char * begin_label =					\
	XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);		\
      char label[64];							\
      ASM_GENERATE_INTERNAL_LABEL (label, "LM", counter);		\
									\
      dbxout_begin_stabn_sline (line);					\
      assemble_name (file, label);					\
      putc ('-', file);							\
      assemble_name (file, begin_label);				\
      fputs ("\n\t.debugsym ", file);					\
      assemble_name (file, label);					\
      putc ('\n', file);						\
      counter += 1;							\
     }									\
  while (0)

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */
#ifndef SUBTARGET_REGISTER_NAMES
#define SUBTARGET_REGISTER_NAMES
#endif

#define REGISTER_NAMES					\
{							\
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",	\
  "r8", "r9", "r10", "r11", "r12", "fp", "lr", "sp",	\
  "ap", "cbit", "a0"					\
  SUBTARGET_REGISTER_NAMES				\
}

/* If defined, a C initializer for an array of structures containing
   a name and a register number.  This macro defines additional names
   for hard registers, thus allowing the `asm' option in declarations
   to refer to registers using alternate names.  */
#ifndef SUBTARGET_ADDITIONAL_REGISTER_NAMES
#define SUBTARGET_ADDITIONAL_REGISTER_NAMES
#endif

#define ADDITIONAL_REGISTER_NAMES	\
{					\
  /*{ "gp", GP_REGNUM },*/		\
  { "r13", FRAME_POINTER_REGNUM },	\
  { "r14", RETURN_ADDR_REGNUM },	\
  { "r15", STACK_POINTER_REGNUM },	\
  SUBTARGET_ADDITIONAL_REGISTER_NAMES	\
}

/* If defined, C string expressions to be used for the `%R', `%L',
   `%U', and `%I' options of `asm_fprintf' (see `final.c').  These
   are useful when a single `md' file must support multiple assembler
   formats.  In that case, the various `tm.h' files can define these
   macros differently.  */
#define REGISTER_PREFIX		""
#define LOCAL_LABEL_PREFIX	".L"
#define USER_LABEL_PREFIX	""
#define IMMEDIATE_PREFIX	"#"

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)		\
   do							\
     {							\
       char label[30];					\
       ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
       fprintf (FILE, "\t.word\t");			\
       assemble_name (FILE, label);			\
       fprintf (FILE, "\n");				\
     }							\
  while (0)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)\
  do							\
    {							\
      char label[30];					\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
      fprintf (FILE, "\t.word\t");			\
      assemble_name (FILE, label);			\
      fprintf (FILE, "-");				\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", REL);	\
      assemble_name (FILE, label);			\
      fprintf (FILE, "\n");				\
    }							\
  while (0)

/* The desired alignment for the location counter at the beginning
   of a loop.  */
/* On the M32R, align loops to 32 byte boundaries (cache line size)
   if -malign-loops.  */
#define LOOP_ALIGN(LABEL) (TARGET_ALIGN_LOOPS ? 5 : 0)

/* Define this to be the maximum number of insns to move around when moving
   a loop test from the top of a loop to the bottom
   and seeing whether to duplicate it.  The default is thirty.

   Loop unrolling currently doesn't like this optimization, so
   disable doing if we are unrolling loops and saving space.  */
#define LOOP_TEST_THRESHOLD (optimize_size				\
			     && !flag_unroll_loops			\
			     && !flag_unroll_all_loops ? 2 : 30)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
/* .balign is used to avoid confusion.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)			\
  do							\
    {							\
      if ((LOG) != 0)					\
	fprintf (FILE, "\t.balign %d\n", 1 << (LOG));	\
    }							\
  while (0)

/* Like `ASM_OUTPUT_COMMON' except takes the required alignment as a
   separate, explicit argument.  If you define this macro, it is used in
   place of `ASM_OUTPUT_COMMON', and gives you more flexibility in
   handling the required alignment of the variable.  The alignment is
   specified as the number of bits.  */

#define SCOMMON_ASM_OP "\t.scomm\t"

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      if (! TARGET_SDATA_NONE						\
	  && (SIZE) > 0							\
	  && (SIZE) <= (unsigned HOST_WIDE_INT) g_switch_value)		\
	fprintf ((FILE), "%s", SCOMMON_ASM_OP);				\
      else								\
	fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%u,%u\n", (int)(SIZE), (ALIGN) / BITS_PER_UNIT);\
    }									\
  while (0)

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      if (! TARGET_SDATA_NONE						\
          && (SIZE) > 0							\
	  && (SIZE) <= (unsigned HOST_WIDE_INT) g_switch_value)		\
        switch_to_section (get_named_section (NULL, ".sbss", 0));	\
      else								\
        switch_to_section (bss_section);				\
      ASM_OUTPUT_ALIGN (FILE, floor_log2 (ALIGN / BITS_PER_UNIT));	\
      last_assemble_variable_decl = DECL;				\
      ASM_DECLARE_OBJECT_NAME (FILE, NAME, DECL);			\
      ASM_OUTPUT_SKIP (FILE, SIZE ? SIZE : 1);				\
    }									\
  while (0)

/* Debugging information.  */

/* Generate DBX and DWARF debugging information.  */
#define DBX_DEBUGGING_INFO    1
#define DWARF2_DEBUGGING_INFO 1

/* Use DWARF2 debugging info by default.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Turn off splitting of long stabs.  */
#define DBX_CONTIN_LENGTH 0

/* Miscellaneous.  */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (flag_pic ? SImode : Pmode)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Max number of bytes we can move from memory
   to memory in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
/* ??? The M32R doesn't have full 32-bit pointers, but making this PSImode has
   its own problems (you have to add extendpsisi2 and truncsipsi2).
   Try to avoid it.  */
#define Pmode SImode

/* A function address in a call instruction.  */
#define FUNCTION_MODE SImode

/* M32R function types.  */
enum m32r_function_type
{
  M32R_FUNCTION_UNKNOWN, M32R_FUNCTION_NORMAL, M32R_FUNCTION_INTERRUPT
};

#define M32R_INTERRUPT_P(TYPE) ((TYPE) == M32R_FUNCTION_INTERRUPT)

/* The maximum number of bytes to copy using pairs of load/store instructions.
   If a block is larger than this then a loop will be generated to copy
   MAX_MOVE_BYTES chunks at a time.  The value of 32 is a semi-arbitrary choice.
   A customer uses Dhrystome as their benchmark, and Dhrystone has a 31 byte
   string copy in it.  */
#define MAX_MOVE_BYTES 32
