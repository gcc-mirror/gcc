/* Definitions of target machine for GNU compiler, Argonaut ARC cpu.
   Copyright (C) 1994, 1995, 1997, 1998 Free Software Foundation, Inc.

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

/* ??? This is an old port, and is undoubtedly suffering from bit rot.  */

/* Things to do:

   - PREDICATE_CODES
   - incscc, decscc?
   - print active compiler options in assembler output
*/

/* ??? Create elf.h and have svr4.h include it.  */
#include "svr4.h"

#undef ASM_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ENDFILE_SPEC
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (arc)")

/* Names to predefine in the preprocessor for this target machine.  */
#define CPP_PREDEFINES "-Acpu(arc) -Amachine(arc) -D__arc__"

/* Additional flags for the preprocessor.  */
#define CPP_SPEC "\
%{!mcpu=*:-D__base__} %{mcpu=base:-D__base__} \
%{EB:-D__big_endian__} \
"

/* Pass -mmangle-cpu if we get -mcpu=*.
   Doing it this way lets one have it on as default with -mcpu=*,
   but also lets one turn it off with -mno-mangle-cpu.  */
#define CC1_SPEC "\
%{mcpu=*:-mmangle-cpu} \
%{EB:%{EL:%emay not use both -EB and -EL}} \
%{EB:-mbig-endian} %{EL:-mlittle-endian} \
"

#define ASM_SPEC "%{v} %{EB} %{EL}"

#define LINK_SPEC "%{v} %{EB} %{EL}"

#define STARTFILE_SPEC "%{!shared:crt0.o%s} crtinit.o%s"

#define ENDFILE_SPEC "crtfini.o%s"

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Mangle all user symbols for the specified cpu.
   ARC's can be shipped in which a collection of cpus are coupled together.
   Each CPU may be different in some way, and thus we may need to distinguish
   code compiled for one to ensure it isn't linked with code compiled for
   another.  */
#define TARGET_MASK_MANGLE_CPU 1
#define TARGET_MANGLE_CPU (target_flags & TARGET_MASK_MANGLE_CPU)

#if 0
/* Mangle libgcc symbols by adding a suffix for the specified cpu.  */
#define TARGET_MASK_MANGLE_CPU_LIBGCC 2
#define TARGET_MANGLE_CPU_LIBGCC (target_flags & TARGET_MASK_MANGLE_CPU_LIBGCC)
#endif

/* Align loops to 32 byte boundaries (cache line size).  */
#define TARGET_MASK_ALIGN_LOOPS 4
#define TARGET_ALIGN_LOOPS (target_flags & TARGET_MASK_ALIGN_LOOPS)

/* Big Endian.  */
#define TARGET_MASK_BIG_ENDIAN 8
#define TARGET_BIG_ENDIAN (target_flags & TARGET_MASK_BIG_ENDIAN)

/* Turn off conditional execution optimization,
   so we can see how well it does, or in case it's buggy.  */
#define TARGET_MASK_NO_COND_EXEC 0x10
#define TARGET_NO_COND_EXEC (target_flags & TARGET_MASK_NO_COND_EXEC)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES \
{ \
    { "mangle-cpu",		TARGET_MASK_MANGLE_CPU },		\
    { "no-mangle-cpu",		-TARGET_MASK_MANGLE_CPU },		\
/*  { "mangle-cpu-libgcc",	TARGET_MASK_MANGLE_CPU_LIBGCC }, */	\
/*  { "no-mangle-cpu-libgcc",	-TARGET_MASK_MANGLE_CPU_LIBGCC }, */	\
    { "align-loops",		TARGET_MASK_ALIGN_LOOPS },		\
    { "no-align-loops",		-TARGET_MASK_ALIGN_LOOPS },		\
    { "big-endian",		TARGET_MASK_BIG_ENDIAN },		\
    { "little-endian",		-TARGET_MASK_BIG_ENDIAN },		\
    { "no-cond-exec",		TARGET_MASK_NO_COND_EXEC },		\
    SUBTARGET_SWITCHES							\
    { "", TARGET_DEFAULT }						\
}

#define TARGET_DEFAULT (0)

#define SUBTARGET_SWITCHES

/* Instruction set characteristics.
   These are internal macros, set by the appropriate -mcpu= option.  */

/* Non-zero means the cpu has a barrel shifter.  */
#define TARGET_SHIFTER 0

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable. 
   The variable, type `char *', is set to the variable part of the
   given option if the fixed part matches.  The actual option name
   is made by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

	extern char *m88k_short_data;
	#define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }  */

extern char *arc_cpu_string;
extern char *arc_text_string,*arc_data_string,*arc_rodata_string;

#define TARGET_OPTIONS \
{						\
  { "cpu=",	&arc_cpu_string		},	\
  { "text=",	&arc_text_string	},	\
  { "data=",	&arc_data_string	},	\
  { "rodata=",	&arc_rodata_string	},	\
}

/* Which cpu we're compiling for.  */
extern int arc_cpu_type;

/* Check if CPU is an extension and set `arc_cpu_type' and `arc_mangle_cpu'
   appropriately.  The result should be non-zero if the cpu is recognized,
   otherwise zero.  This is intended to be redefined in a cover file.
   This is used by arc_init.  */
#define ARC_EXTENSION_CPU(cpu) 0

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

extern void arc_init ();

#define OVERRIDE_OPTIONS \
do {				\
  /* These need to be done at start up.  It's convenient to do them here.  */ \
  arc_init ();			\
} while (0)

/* Target machine storage layout.  */

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifdef __big_endian__
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Number of bits in an addressable storage unit.  */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE) \
if (GET_MODE_CLASS (MODE) == MODE_INT		\
    && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
{						\
  (MODE) = SImode;				\
}

/* Define this macro if the promotion described by `PROMOTE_MODE'
   should also be done for outgoing function arguments.  */
#define PROMOTE_FUNCTION_ARGS

/* Likewise, if the function return value is promoted.  */
#define PROMOTE_FUNCTION_RETURN

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* ALIGN FRAMES on word boundaries */
#define ARC_STACK_ALIGN(LOC) (((LOC)+7) & ~7)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
/* This is bigger than currently necessary for the ARC.  If 8 byte floats are
   ever added it's not clear whether they'll need such alignment or not.  For
   now we assume they will.  We can always relax it if necessary but the
   reverse isn't true.  */
#define BIGGEST_ALIGNMENT 64

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
/* On the ARC the lower address bits are masked to 0 as necessary.  The chip
   won't croak when given an unaligned address, but the insn will still fail
   to produce the correct result.  */
#define STRICT_ALIGNMENT 1

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

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
/* Registers 61, 62, and 63 are not really registers and we needn't treat
   them as such.  We still need a register for the condition code.  */
#define FIRST_PSEUDO_REGISTER 62

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   0-28  - general purpose registers
   29    - ilink1 (interrupt link register)
   30    - ilink2 (interrupt link register)
   31    - blink (branch link register)
   32-59 - reserved for extensions
   60    - LP_COUNT
   61    - condition code

   For doc purposes:
   61    - short immediate data indicator (setting flags)
   62    - long immediate data indicator
   63    - short immediate data indicator (not setting flags).

   The general purpose registers are further broken down into:
   0-7   - arguments/results
   8-15  - call used
   16-23 - call saved
   24    - call used, static chain pointer
   25    - call used, gptmp
   26    - global pointer
   27    - frame pointer
   28    - stack pointer

   By default, the extension registers are not available.  */

#define FIXED_REGISTERS \
{ 0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 1, 1, 1, 1, 0,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS \
{ 1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1 }

/* If defined, an initializer for a vector of integers, containing the
   numbers of hard registers in the order in which GNU CC should
   prefer to use them (from most preferred to least).  */
#define REG_ALLOC_ORDER \
{ 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0, 1,			\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 31,			\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,		\
  27, 28, 29, 30 }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE) \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.  */
extern unsigned int arc_hard_regno_mode_ok[];
extern unsigned int arc_mode_class[];
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
((arc_hard_regno_mode_ok[REGNO] & arc_mode_class[MODE]) != 0)

/* A C expression that is nonzero if it is desirable to choose
   register allocation so as to avoid move instructions between a
   value of mode MODE1 and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R,
   MODE2)' are ever different for any R, then `MODES_TIEABLE_P (MODE1,
   MODE2)' must be zero.  */

/* Tie QI/HI/SI modes together.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
(GET_MODE_CLASS (MODE1) == MODE_INT		\
 && GET_MODE_CLASS (MODE2) == MODE_INT		\
 && GET_MODE_SIZE (MODE1) <= UNITS_PER_WORD	\
 && GET_MODE_SIZE (MODE2) <= UNITS_PER_WORD)

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

enum reg_class {
  NO_REGS, LPCOUNT_REG, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES \
{ "NO_REGS", "LPCOUNT_REG", "GENERAL_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{ {0, 0}, {0, 0x10000000}, {0xffffffff, 0xfffffff}, \
  {0xffffffff, 0x1fffffff} }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
extern enum reg_class arc_regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) \
(arc_regno_reg_class[REGNO])

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */
#define REG_CLASS_FROM_LETTER(C) \
((C) == 'l' ? LPCOUNT_REG /* ??? needed? */ \
 : NO_REGS)

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < 29 || (unsigned) reg_renumber[REGNO] < 29)
#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < 29 || (unsigned) reg_renumber[REGNO] < 29)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) \
(CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */
/* 'I' is used for short immediates (always signed).
   'J' is used for long immediates.
   'K' is used for any constant up to 64 bits (for 64x32 situations?).  */

/* local to this file */
#define SMALL_INT(X) ((unsigned) ((X) + 0x100) < 0x200)
/* local to this file */
#define LARGE_INT(X) \
((X) >= (-(HOST_WIDE_INT) 0x7fffffff - 1) \
 && (X) <= (unsigned HOST_WIDE_INT) 0xffffffff)

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
((C) == 'I' ? SMALL_INT (VALUE)		\
 : (C) == 'J' ? LARGE_INT (VALUE)	\
 : (C) == 'K' ? 1			\
 : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */
/* 'G' is used for integer values for the multiplication insns where the
   operands are extended from 4 bytes to 8 bytes.
   'H' is used when any 64 bit constant is allowed.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
((C) == 'G' ? arc_double_limm_p (VALUE) \
 : (C) == 'H' ? 1 \
 : 0)

/* A C expression that defines the optional machine-dependent constraint
   letters that can be used to segregate specific types of operands,
   usually memory references, for the target machine.  It should return 1 if
   VALUE corresponds to the operand type represented by the constraint letter
   C.  If C is not defined as an extra constraint, the value returned should
   be 0 regardless of VALUE.  */
/* ??? This currently isn't used.  Waiting for PIC.  */
#if 0
#define EXTRA_CONSTRAINT(VALUE, C) \
((C) == 'R' ? (SYMBOL_REF_FLAG (VALUE) || GET_CODE (VALUE) == LABEL_REF) \
 : 0)
#endif

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET FIRST_PARM_OFFSET (0)

/* Offset of first parameter from the argument pointer register value.  */
/* 4 bytes for each of previous fp, return address, and previous gp.
   4 byte reserved area for future considerations.  */
#define FIRST_PARM_OFFSET(FNDECL) 16

/* A C expression whose value is RTL representing the address in a
   stack frame where the pointer to the caller's frame is stored.
   Assume that FRAMEADDR is an RTL expression for the address of the
   stack frame itself.

   If you don't define this macro, the default is to return the value
   of FRAMEADDR--that is, the stack frame address is also the address
   of the stack word that points to the previous frame.  */
/* ??? unfinished */
/*define DYNAMIC_CHAIN_ADDRESS (FRAMEADDR)*/

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current frame.
   FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME'
   is defined.  */
/* The current return address is in r31.  The return address of anything
   farther back is at [%fp,4].  */
#if 0 /* The default value should work.  */
#define RETURN_ADDR_RTX(COUNT, FRAME) \
(((COUNT) == -1)				\
 ? gen_rtx (REG, Pmode, 31)			\
 : copy_to_reg (gen_rtx (MEM, Pmode,		\
			 memory_address (Pmode, plus_constant ((FRAME), UNITS_PER_WORD)))))
#endif

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 28

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 27

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  This must
   not be a register used by the prologue.  */
#define STATIC_CHAIN_REGNUM 24

/* A C expression which is nonzero if a function must have and use a
   frame pointer.  This expression is evaluated in the reload pass.
   If its value is nonzero the function will have a frame pointer.  */
#define FRAME_POINTER_REQUIRED \
(current_function_calls_alloca)

/* C statement to store the difference between the frame pointer
   and the stack pointer values immediately after the function prologue.  */
#define INITIAL_FRAME_POINTER_OFFSET(VAR) \
((VAR) = arc_compute_frame_size (get_frame_size ()))

/* Function argument passing.  */

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */
#define RETURN_POPS_ARGS(DECL, FUNTYPE, SIZE) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */
#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT) \
((CUM) = 0)

/* The number of registers used for parameter passing.  Local to this file.  */
#define MAX_ARC_PARM_REGS 8

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) \
((unsigned) (N) < MAX_ARC_PARM_REGS)

/* The ROUND_ADVANCE* macros are local to this file.  */
/* Round SIZE up to a word boundary.  */
#define ROUND_ADVANCE(SIZE) \
(((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round arg MODE/TYPE up to the next word boundary.  */
#define ROUND_ADVANCE_ARG(MODE, TYPE) \
((MODE) == BLKmode				\
 ? ROUND_ADVANCE (int_size_in_bytes (TYPE))	\
 : ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

/* Round CUM up to the necessary point for argument MODE/TYPE.  */
#define ROUND_ADVANCE_CUM(CUM, MODE, TYPE) \
((((MODE) == BLKmode ? TYPE_ALIGN (TYPE) : GET_MODE_BITSIZE (MODE)) \
  > BITS_PER_WORD)	\
 ? ((CUM) + 1 & ~1)	\
 : (CUM))

/* Return boolean indicating arg of type TYPE and mode MODE will be passed in
   a reg.  This includes arguments that have to be passed by reference as the
   pointer to them is passed in a reg if one is available (and that is what
   we're given).
   When passing arguments NAMED is always 1.  When receiving arguments NAMED
   is 1 for each argument except the last in a stdarg/varargs function.  In
   a stdarg function we want to treat the last named arg as named.  In a
   varargs function we want to treat the last named arg (which is
   `__builtin_va_alist') as unnamed.
   This macro is only used in this file.  */
extern int current_function_varargs;
#define PASS_IN_REG_P(CUM, MODE, TYPE, NAMED) \
((!current_function_varargs || (NAMED))					\
 && (CUM) < MAX_ARC_PARM_REGS						\
 && ((ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE))				\
      + ROUND_ADVANCE_ARG ((MODE), (TYPE))				\
      <= MAX_ARC_PARM_REGS)))

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
    (otherwise it is an extra parameter matching an ellipsis).  */
/* On the ARC the first MAX_ARC_PARM_REGS args are normally in registers
   and the rest are pushed.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
(PASS_IN_REG_P ((CUM), (MODE), (TYPE), (NAMED))				\
 ? gen_rtx (REG, (MODE), ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)))	\
 : 0)

/* A C expression for the number of words, at the beginning of an
   argument, must be put in registers.  The value must be zero for
   arguments that are passed entirely in registers or that are entirely
   pushed on the stack.

   On some machines, certain arguments must be passed partially in
   registers and partially in memory.  On these machines, typically the
   first @var{n} words of arguments are passed in registers, and the rest
   on the stack.  If a multi-word argument (a @code{double} or a
   structure) crosses that boundary, its first few words must be passed
   in registers and the rest must be pushed.  This macro tells the
   compiler when this occurs, and how many of the words should go in
   registers.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */
/* All aggregates and arguments greater than 8 bytes are passed this way.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
(TYPE					\
 && (AGGREGATE_TYPE_P (TYPE)		\
     || int_size_in_bytes (TYPE) > 8))

/* A C expression that indicates when it is the called function's
   responsibility to make copies of arguments passed by reference.
   If the callee can determine that the argument won't be modified, it can
   avoid the copy.  */
/* ??? We'd love to be able to use NAMED here.  Unfortunately, it doesn't
   include the last named argument so we keep track of the args ourselves.  */

#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) \
FUNCTION_ARG_PASS_BY_REFERENCE ((CUM), (MODE), (TYPE), (NAMED))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
((CUM) = (ROUND_ADVANCE_CUM ((CUM), (MODE), (TYPE)) \
	  + ROUND_ADVANCE_ARG ((MODE), (TYPE))))

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined, 
   PARM_BOUNDARY is used for all arguments.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
(((TYPE) ? TYPE_ALIGN (TYPE) : GET_MODE_BITSIZE (MODE)) <= PARM_BOUNDARY \
 ? PARM_BOUNDARY \
 : 2 * PARM_BOUNDARY)

/* This macro offers an alternative
   to using `__builtin_saveregs' and defining the macro
   `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have
   been passed consecutively on the stack.  Once this is done, you
   can use the standard implementation of varargs that works for
   machines that pass all their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure,
   containing the values that obtain after processing of the named
   arguments.  The arguments MODE and TYPE describe the last named
   argument--its machine mode and its data type as a tree node.

   The macro implementation should do two things: first, push onto the
   stack all the argument registers *not* used for the named
   arguments, and second, store the size of the data thus pushed into
   the `int'-valued variable whose name is supplied as the argument
   PRETEND_SIZE.  The value that you store here will serve as
   additional offset for setting up the stack frame.

   If the argument NO_RTL is nonzero, it means that the
   arguments of the function are being analyzed for the second time.
   This happens for an inline function, which is not actually
   compiled until the end of the source file.  The macro
   `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */

#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_SIZE, NO_RTL) \
arc_setup_incoming_varargs(&ARGS_SO_FAR, MODE, TYPE, &PRETEND_SIZE, NO_RTL)

/* Function results.  */

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, 0)

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */
/* ??? What about r1 in DI/DF values.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value says
   to return the function value in memory, just as large structures are
   always returned.  Here TYPE will be a C expression of type `tree',
   representing the data type of the value.  */
#define RETURN_IN_MEMORY(TYPE) \
(AGGREGATE_TYPE_P (TYPE) \
 || int_size_in_bytes (TYPE) > 8 \
 || TREE_ADDRESSABLE (TYPE))

/* Tell GCC to use RETURN_IN_MEMORY.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Register in which address to store a structure value
   is passed to a function, or 0 to use `invisible' first argument.  */
#define STRUCT_VALUE 0

/* Function entry and exit.  */

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */
#define FUNCTION_PROLOGUE(FILE, SIZE) \
arc_output_function_prologue (FILE, SIZE)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 0

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */
#define FUNCTION_EPILOGUE(FILE, SIZE) \
arc_output_function_epilogue (FILE, SIZE)

/* Epilogue delay slots.  */
#define DELAY_SLOTS_FOR_EPILOGUE arc_delay_slots_for_epilogue ()

#define ELIGIBLE_FOR_EPILOGUE_DELAY(TRIAL, SLOTS_FILLED) \
arc_eligible_for_epilogue_delay (TRIAL, SLOTS_FILLED)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
#define FUNCTION_PROFILER(FILE, LABELNO)

/* Trampolines.  */
/* ??? This doesn't work yet because GCC will use as the address of a nested
   function the address of the trampoline.  We need to use that address
   right shifted by 2.  It looks like we'll need PSImode after all. :-(  */

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */
/* On the ARC, the trampoline is quite simple as we have 32 bit immediate
   constants.

	mov r24,STATIC
	j.nd FUNCTION
*/
#define TRAMPOLINE_TEMPLATE(FILE) \
do { \
  ASM_OUTPUT_INT (FILE, GEN_INT (0x631f7c00)); \
  ASM_OUTPUT_INT (FILE, const0_rtx); \
  ASM_OUTPUT_INT (FILE, GEN_INT (0x381f0000)); \
  ASM_OUTPUT_INT (FILE, const0_rtx); \
} while (0)

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE 16

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) \
do { \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 4)), CXT); \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 12)), FNADDR); \
  emit_insn (gen_flush_icache (validize_mem (gen_rtx (MEM, SImode, TRAMP)))); \
} while (0)

/* Library calls.  */

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Addressing modes, and classification of registers for them.  */

/* Maximum number of registers that can appear in a valid memory address.  */
/* The `ld' insn allows 2, but the `st' insn only allows 1.  */
#define MAX_REGS_PER_ADDRESS 1

/* We have pre inc/dec (load/store with update).  */
#define HAVE_PRE_INCREMENT 1
#define HAVE_PRE_DECREMENT 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) \
(GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
 || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST)

/* Nonzero if the constant value X is a legitimate general operand.
   We can handle any 32 or 64 bit constant.  */
/* "1" should work since the largest constant should be a 64 bit critter.  */
/* ??? Not sure what to do for 64x32 compiler.  */
#define LEGITIMATE_CONSTANT_P(X) 1

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
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
((unsigned) REGNO (X) - 29 >= FIRST_PSEUDO_REGISTER - 29)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
((unsigned) REGNO (X) - 29 >= FIRST_PSEUDO_REGISTER - 29)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  */
/* The `ld' insn allows [reg],[reg+shimm],[reg+limm],[reg+reg],[limm]
   but the `st' insn only allows [reg],[reg+shimm],[limm].
   The only thing we can do is only allow the most strict case `st' and hope
   other parts optimize out the restrictions for `ld'.  */

/* local to this file */
#define RTX_OK_FOR_BASE_P(X) \
(REG_P (X) && REG_OK_FOR_BASE_P (X))

/* local to this file */
#define RTX_OK_FOR_INDEX_P(X) \
(0 && /*???*/ REG_P (X) && REG_OK_FOR_INDEX_P (X))

/* local to this file */
/* ??? Loads can handle any constant, stores can only handle small ones.  */
#define RTX_OK_FOR_OFFSET_P(X) \
(GET_CODE (X) == CONST_INT && SMALL_INT (INTVAL (X)))

#define LEGITIMATE_OFFSET_ADDRESS_P(MODE, X) \
(GET_CODE (X) == PLUS				\
 && RTX_OK_FOR_BASE_P (XEXP (X, 0))		\
 && (RTX_OK_FOR_INDEX_P (XEXP (X, 1))		\
     || RTX_OK_FOR_OFFSET_P (XEXP (X, 1))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (RTX_OK_FOR_BASE_P (X))				\
    goto ADDR;						\
  if (LEGITIMATE_OFFSET_ADDRESS_P ((MODE), (X)))	\
    goto ADDR;						\
  if (GET_CODE (X) == CONST_INT && LARGE_INT (INTVAL (X))) \
    goto ADDR;						\
  if (GET_CODE (X) == SYMBOL_REF			\
	   || GET_CODE (X) == LABEL_REF			\
	   || GET_CODE (X) == CONST)			\
    goto ADDR;						\
  if ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == PRE_INC) \
      /* We're restricted here by the `st' insn.  */	\
      && RTX_OK_FOR_BASE_P (XEXP ((X), 0)))		\
    goto ADDR;						\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL) \
{ if (GET_CODE (ADDR) == PRE_DEC)	\
    goto LABEL;				\
  if (GET_CODE (ADDR) == PRE_INC)	\
    goto LABEL;				\
}

/* Condition code usage.  */

/* Some insns set all condition code flags, some only set the ZNC flags, and
   some only set the ZN flags.  */

#define EXTRA_CC_MODES CCZNCmode, CCZNmode

#define EXTRA_CC_NAMES "CCZNC", "CCZN"

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
extern enum machine_mode arc_select_cc_mode ();
#define SELECT_CC_MODE(OP, X, Y) \
arc_select_cc_mode (OP, X, Y)

/* Return non-zero if SELECT_CC_MODE will never return MODE for a
   floating point inequality comparison.  */
#define REVERSIBLE_CC_MODE(MODE) 1 /*???*/

/* Costs.  */

/* An insn is define to cost 4 "units", and we work from there.
   COSTS_N_INSNS (N) is defined as (N) * 4 - 2 so that seems reasonable.
   Some values are supposed to be defined relative to each other and thus
   aren't necessarily related to COSTS_N_INSNS.  */

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */
/* Small integers are as cheap as registers.  4 byte values can be fetched
   as immediate constants - let's give that the cost of an extra insn. */
#define CONST_COSTS(X, CODE, OUTER_CODE) \
  case CONST_INT :						\
    if (SMALL_INT (INTVAL (X)))					\
      return 0;							\
    /* fall through */						\
  case CONST :							\
  case LABEL_REF :						\
  case SYMBOL_REF :						\
    return 4;							\
  case CONST_DOUBLE :						\
    {								\
      rtx high, low;						\
      split_double (X, &high, &low);				\
      return 4 * (!SMALL_INT (INTVAL (high))			\
		  + !SMALL_INT (INTVAL (low)));			\
    }

/* Compute the cost of an address.  */
#define ADDRESS_COST(ADDR) (REG_P (ADDR) ? 1 : arc_address_cost (ADDR))

/* Compute extra cost of moving data between one register class
   and another.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2) 2

/* Compute the cost of moving data between registers and memory.  */
/* Memory is 3 times as expensive as registers.
   ??? Is that the right way to look at it?  */
#define MEMORY_MOVE_COST(MODE,CLASS,IN) \
(GET_MODE_SIZE (MODE) <= UNITS_PER_WORD ? 6 : 12)

/* The cost of a branch insn.  */
/* ??? What's the right value here?  Branches are certainly more
   expensive than reg->reg moves.  */
#define BRANCH_COST 2

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  The purpose for the cost of MULT is to encourage
   `synth_mult' to find a synthetic multiply when reasonable.

   If we need more than 12 insns to do a multiply, then go out-of-line,
   since the call overhead will be < 10% of the cost of the multiply.  */
#define RTX_COSTS(X, CODE, OUTER_CODE) \
  case ASHIFT :						\
  case ASHIFTRT :					\
  case LSHIFTRT :					\
    if (TARGET_SHIFTER)					\
      return COSTS_N_INSNS (1);				\
    if (GET_CODE (XEXP ((X), 1)) != CONST_INT)		\
      return COSTS_N_INSNS (16);			\
    return COSTS_N_INSNS (INTVAL (XEXP ((X), 1)));

/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
/* On the ARC, calling through registers is slow.  */
#define NO_FUNCTION_CSE

/* Define this macro if it is as good or better for a function to call
   itself with an explicit address than to call an address kept in a
   register.  */
/* On the ARC, calling through registers is slow.  */
#define NO_RECURSIVE_FUNCTION_CSE

/* Section selection.  */
/* WARNING: These section names also appear in dwarfout.c.  */

/* The names of the text, data, and readonly-data sections are runtime
   selectable.  */

#define ARC_SECTION_FORMAT		"\t.section %s"
#define ARC_DEFAULT_TEXT_SECTION	".text"
#define ARC_DEFAULT_DATA_SECTION	".data"
#define ARC_DEFAULT_RODATA_SECTION	".rodata"

extern char *arc_text_section,*arc_data_section,*arc_rodata_section;

/* initfini.c uses this in an asm.  */
#if defined (CRT_INIT) || defined (CRT_FINI)
#define TEXT_SECTION_ASM_OP	"\t.section .text"
#else
#define TEXT_SECTION_ASM_OP	arc_text_section /*"\t.section .text"*/
#endif
#define DATA_SECTION_ASM_OP	arc_data_section /*"\t.section .data"*/

#undef CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP	arc_rodata_section /*"\t.section .rodata"*/

#define BSS_SECTION_ASM_OP	"\t.section .bss"

/* Define this macro if jump tables (for tablejump insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.
   This macro is irrelevant if there is no separate readonly data section.  */
/*#define JUMP_TABLES_IN_TEXT_SECTION*/

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to store a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).  */

/* On the ARC, function addresses are not the same as normal addresses.
   Branch to absolute address insns take an address that is right-shifted
   by 2.  We encode the fact that we have a function here, and then emit a
   special assembler op when outputting the address.  */
#define ENCODE_SECTION_INFO(DECL) \
do {							\
  if (TREE_CODE (DECL) == FUNCTION_DECL)		\
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;	\
} while (0)

/* Decode SYM_NAME and store the real name part in VAR, sans
   the characters that encode section info.  Define this macro if
   ENCODE_SECTION_INFO alters the symbol's name string.  */
/*#define STRIP_NAME_ENCODING(VAR, SYM_NAME)*/

/* For DWARF.  Marginally different than default so output is "prettier"
   (and consistent with above).  */
#define PUSHSECTION_FORMAT "\t%s %s\n"

/* Tell crtstuff.c we're using ELF.  */
#define OBJECT_FORMAT_ELF

/* PIC */

/* The register number of the register used to address a table of static
   data addresses in memory.  In some cases this register is defined by a
   processor's ``application binary interface'' (ABI).  When this macro
   is defined, RTL is generated for this register once, as with the stack
   pointer and frame pointer registers.  If this macro is not defined, it
   is up to the machine-dependent files to allocate such a register (if
   necessary).  */
#define PIC_OFFSET_TABLE_REGNUM 26

/* Define this macro if the register defined by PIC_OFFSET_TABLE_REGNUM is
   clobbered by calls.  Do not define this macro if PIC_OFFSET_TABLE_REGNUM
   is not defined.  */
/* This register is call-saved on the ARC.  */
/*#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED*/

/* By generating position-independent code, when two different programs (A
   and B) share a common library (libC.a), the text of the library can be
   shared whether or not the library is linked at the same address for both
   programs.  In some of these environments, position-independent code
   requires not only the use of different addressing modes, but also
   special code to enable the use of these addressing modes.

   The FINALIZE_PIC macro serves as a hook to emit these special
   codes once the function is being compiled into assembly code, but not
   before.  (It is not done before, because in the case of compiling an
   inline function, it would lead to multiple PIC prologues being
   included in functions which used inline functions and were compiled to
   assembly language.)  */

#define FINALIZE_PIC arc_finalize_pic ()

/* A C expression that is nonzero if X is a legitimate immediate
   operand on the target machine when generating position independent code.
   You can assume that X satisfies CONSTANT_P, so you need not
   check this.  You can also assume `flag_pic' is true, so you need not
   check it either.  You need not define this macro if all constants
   (including SYMBOL_REF) can be immediate operands when generating
   position independent code.  */
/*#define LEGITIMATE_PIC_OPERAND_P(X)*/

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */
extern void arc_asm_file_start ();
#undef ASM_FILE_START
#define ASM_FILE_START(FILE) arc_asm_file_start (FILE)

/* A C statement to output assembler commands which will identify the
   object file as having been compiled with GNU CC (or another GNU
   compiler).  */
#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE) /* nothing */

/* Needed because we define ASM_IDENTIFY_GCC.  */
#define ASM_IDENTIFY_LANGUAGE(FILE) output_lang_identify (FILE)

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

/* This is how to output an assembler line defining a `char' constant.  */
#define ASM_OUTPUT_CHAR(FILE, VALUE) \
( fprintf (FILE, "\t.byte\t"),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining a `short' constant.  */
#define ASM_OUTPUT_SHORT(FILE, VALUE) \
( fprintf (FILE, "\t.hword\t"),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line defining an `int' constant.
   We also handle symbol output here.  Code addresses must be right shifted
   by 2 because that's how the jump instruction wants them.  */
#define ASM_OUTPUT_INT(FILE, VALUE) \
do {									\
  fprintf (FILE, "\t.word\t");						\
  if ((GET_CODE (VALUE) == SYMBOL_REF && SYMBOL_REF_FLAG (VALUE))	\
      || GET_CODE (VALUE) == LABEL_REF)					\
    {									\
      fprintf (FILE, "%%st(");						\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, ")");						\
    }									\
  else									\
    output_addr_const (FILE, (VALUE));					\
  fprintf (FILE, "\n");							\
} while (0)

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE) \
{							\
  long t;						\
  char str[30];						\
  REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
  fprintf (FILE, "\t.word\t0x%lx %s %s\n",		\
	   t, ASM_COMMENT_START, str);			\
}

/* This is how to output an assembler line defining a `double' constant.  */
#define ASM_OUTPUT_DOUBLE(FILE, VALUE) \
{							\
  long t[2];						\
  char str[30];						\
  REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), t);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);	\
  fprintf (FILE, "\t.word\t0x%lx %s %s\n\t.word\t0x%lx\n", \
	   t[0], ASM_COMMENT_START, str, t[1]);		\
}

/* This is how to output an assembler line for a numeric constant byte.  */
#define ASM_BYTE_OP	".byte"
#define ASM_OUTPUT_BYTE(FILE, VALUE)  \
  fprintf (FILE, "\t%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* The assembler's parentheses characters.  */
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
#define ASM_OUTPUT_LABEL(FILE, NAME) \
do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */
#define ASM_GLOBALIZE_LABEL(FILE, NAME) \
do {				\
  fputs ("\t.global\t", FILE);	\
  assemble_name (FILE, NAME);	\
  fputs ("\n", FILE);		\
} while (0)

/* A C statement (sans semicolon) to output on FILE an assembler pseudo-op to
   declare a library function name external.  The name of the library function
   is given by SYMREF, which has type RTX and is a SYMBOL_REF.  */
#if 0
/* On the ARC we want to have libgcc's for multiple cpus in one binary.
   We can't use `assemble_name' here as that will call ASM_OUTPUT_LABELREF
   and we'll get another suffix added on if -mmangle-cpu.  */
extern char *arc_mangle_cpu;
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, SYMREF) \
do {							\
  if (TARGET_MANGLE_CPU_LIBGCC)				\
    {							\
      fprintf (FILE, "\t.rename\t_%s, _%s%s\n",		\
	       XSTR (SYMREF, 0), XSTR (SYMREF, 0),	\
	       arc_mangle_suffix);			\
    }							\
} while (0)
#endif

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
/* We mangle all user labels to provide protection from linking code
   compiled for different cpus.  */
/* We work around a dwarfout.c deficiency by watching for labels from it and
   not adding the '_' prefix nor the cpu suffix.  There is a comment in
   dwarfout.c that says it should be using ASM_OUTPUT_INTERNAL_LABEL.  */
extern char *arc_mangle_cpu;
#define ASM_OUTPUT_LABELREF(FILE, NAME) \
do {							\
  if ((NAME)[0] == '.' && (NAME)[1] == 'L')		\
    fprintf (FILE, "%s", NAME);				\
  else							\
    {							\
      fputc ('_', FILE);				\
      if (TARGET_MANGLE_CPU && arc_mangle_cpu != NULL)	\
	fprintf (FILE, "%s_", arc_mangle_cpu);		\
      fprintf (FILE, "%s", NAME);			\
    }							\
} while (0)

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */
#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM) \
do {						\
  arc_ccfsm_at_label (PREFIX, NUM);		\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM);	\
} while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO) \
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Assembler pseudo-op to equate one value with another.  */
/* ??? This is needed because dwarfout.c provides a default definition too
   late for defaults.h (which contains the default definition of ASM_OUTPUT_DEF
   that we use).  */
#define SET_ASM_OP ".set"

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE, NAME) \
do {					\
  ctors_section ();			\
  fprintf (FILE, "\t.word\t%%st(");	\
  assemble_name (FILE, NAME);		\
  fprintf (FILE, ")\n");		\
} while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE, NAME) \
do {					\
  dtors_section ();			\
  fprintf (FILE, "\t.word\t%%st(");	\
  assemble_name (FILE, NAME);		\
  fprintf (FILE, ")\n");		\
} while (0)

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */
#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",		\
 "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",		\
 "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",	\
 "r24", "r25", "r26", "fp", "sp", "ilink1", "ilink2", "blink",	\
 "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",	\
 "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47",	\
 "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",	\
 "r56", "r57", "r58", "r59", "lp_count", "cc"}

/* Entry to the insn conditionalizer.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
arc_final_prescan_insn (INSN, OPVEC, NOPERANDS)

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  */
extern char arc_punct_chars[];
#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
arc_punct_chars[(unsigned char) (CHAR)]

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
#define PRINT_OPERAND(FILE, X, CODE) \
arc_print_operand (FILE, X, CODE)

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
arc_print_operand_address (FILE, ADDR)

/* This is how to output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do {							\
  char label[30];					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
  fprintf (FILE, "\t.word %%st(");			\
  assemble_name (FILE, label);				\
  fprintf (FILE, ")\n");				\
} while (0)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
do {							\
  char label[30];					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", VALUE);	\
  fprintf (FILE, "\t.word %%st(");			\
  assemble_name (FILE, label);				\
  fprintf (FILE, "-");					\
  ASM_GENERATE_INTERNAL_LABEL (label, "L", REL);	\
  assemble_name (FILE, label);				\
  fprintf (FILE, ")\n");				\
} while (0)

/* The desired alignment for the location counter at the beginning
   of a loop.  */
/* On the ARC, align loops to 32 byte boundaries (cache line size)
   if -malign-loops.  */
#define LOOP_ALIGN(LABEL) (TARGET_ALIGN_LOOPS ? 5 : 0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
do { if ((LOG) != 0) fprintf (FILE, "\t.align %d\n", 1 << (LOG)); } while (0)

/* Debugging information.  */

/* Generate DBX and DWARF debugging information.  */
#define DBX_DEBUGGING_INFO
#define DWARF_DEBUGGING_INFO

/* Prefer STABS (for now).  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* How to renumber registers for dbx and gdb.  */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Turn off splitting of long stabs.  */
#define DBX_CONTIN_LENGTH 0

/* Miscellaneous.  */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* It's not clear what PIC will look like or whether we want to use -fpic
   for the embedded form currently being talked about.  For now require -fpic
   to get pc relative switch tables.  */
/*#define CASE_VECTOR_PC_RELATIVE 1 */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
/* ??? The arc doesn't have full 32 bit pointers, but making this PSImode has
   its own problems (you have to add extendpsisi2 and trucnsipsi2 but how does
   one do it without getting excess code?).  Try to avoid it.  */
#define Pmode SImode

/* A function address in a call instruction.  */
#define FUNCTION_MODE SImode

/* A C expression whose value is nonzero if IDENTIFIER with arguments ARGS
   is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */
extern int arc_valid_machine_attribute ();
#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, IDENTIFIER, ARGS) \
arc_valid_machine_decl_attribute (DECL, ATTRIBUTES, IDENTIFIER, ARGS)

/* A C expression that returns zero if the attributes on TYPE1 and TYPE2 are
   incompatible, one if they are compatible, and two if they are
   nearly compatible (which causes a warning to be generated).  */
extern int arc_comp_type_attributes ();
#define COMP_TYPE_ATTRIBUTES(TYPE1, TYPE2) \
arc_comp_type_attributes (TYPE1, TYPE2)

/* Give newly defined TYPE some default attributes.  */
extern void arc_set_default_type_attributes ();
#define SET_DEFAULT_TYPE_ATTRIBUTES(TYPE) \
arc_set_default_type_attributes (TYPE)

/* Define this if the target system supports the function
   atexit from the ANSI C standard.  If this is not defined,
   and INIT_SECTION_ASM_OP is not defined, a default
   exit function will be provided to support C++.  */
#define HAVE_ATEXIT

/* alloca should avoid clobbering the old register save area.  */
/* ??? Not defined in tm.texi.  */
#define SETJMP_VIA_SAVE_AREA

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
extern struct rtx_def *arc_compare_op0, *arc_compare_op1;

/* Define the function that build the compare insn for scc and bcc.  */
extern struct rtx_def *gen_compare_reg ();

/* Declarations for various fns used in the .md file.  */
extern char *output_shift ();

/* ARC function types.   */
enum arc_function_type {
  ARC_FUNCTION_UNKNOWN, ARC_FUNCTION_NORMAL,
  /* These are interrupt handlers.  The name corresponds to the register
     name that contains the return address.  */
  ARC_FUNCTION_ILINK1, ARC_FUNCTION_ILINK2
};
#define ARC_INTERRUPT_P(TYPE) \
((TYPE) == ARC_FUNCTION_ILINK1 || (TYPE) == ARC_FUNCTION_ILINK2)
/* Compute the type of a function from its DECL.  */
enum arc_function_type arc_compute_function_type ();
