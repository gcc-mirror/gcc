/* Definitions of target machine for GNU compiler, for Intel 860.
   Copyright (C) 1989, 1991 Free Software Foundation, Inc.

   Written by Richard Stallman (rms@ai.mit.edu).

   Hacked substantially by Ron Guilmette (rfg@ncd.com) to cater to
   the whims of the System V Release 4 assembler.

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


/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Di860 -Dunix"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (i860)");

/* Run-time compilation parameters selecting different hardware subsets
   or supersets.

   On the i860, we have one: TARGET_XP.  This option allows gcc to generate
   additional instructions available only on the newer i860 XP (but not on
   the older i860 XR).
*/

extern int target_flags;

/* Nonzero if we should generate code to use the fpu.  */
#define TARGET_XP (target_flags & 1)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"xp", 1},			\
    {"noxp", -1},		\
    {"xr", -1},			\
    { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 0

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is a moot question on the i860 due to the lack of bit-field insns.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on i860 in the mode we will use.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* For the i860 this goes with BYTES_BIG_ENDIAN.  */
/* NOTE: GCC probably cannot support a big-endian i860
   because GCC fundamentally assumes that the order of words
   in memory as the same as the order in registers.
   That's not true for the big-endian i860.
   The big-endian i860 isn't important enough to
   justify the trouble of changing this assumption.  */
#define WORDS_BIG_ENDIAN 0

/* number of bits in an addressable storage unit */
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

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 128

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 64

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Minimum size in bits of the largest boundary to which any
   and all fundamental data types supported by the hardware
   might need to be aligned. No data type wants to be aligned
   rounder than this.  The i860 supports 128-bit (long double)
   floating point quantities, and the System V Release 4 i860
   ABI requires these to be aligned to 16-byte (128-bit)
   boundaries.  */
#define BIGGEST_ALIGNMENT 128

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* If bit field type is int, dont let it cross an int,
   and give entire struct the alignment of an int.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   i860 has 32 fullword registers and 32 floating point registers.  */

#define FIRST_PSEUDO_REGISTER 64

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the i860, this includes the always-0 registers
   and fp, sp, arg pointer, and the return address.
   Also r31, used for special purposes for constant addresses.  */
#define FIXED_REGISTERS  \
 {1, 1, 1, 1, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 1,	\
  1, 1, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   On the i860, these are r0-r3, r16-r31, f0, f1, and f16-f31.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1}

/* Try to get a non-preserved register before trying to get one we will
   have to preserve.  Try to get an FP register only *after* trying to
   get a general register, because it is relatively expensive to move
   into or out of an FP register.  */

#define REG_ALLOC_ORDER			\
 {31, 30, 29, 28, 27, 26, 25, 24,	\
  23, 22, 21, 20, 19, 18, 17, 16,	\
  15, 14, 13, 12, 11, 10,  9,  8,	\
   7,  6,  5,  4,  3,  2,  1,  0,	\
  63, 62, 61, 60, 59, 58, 57, 56,	\
  55, 54, 53, 52, 51, 50, 49, 48,	\
  47, 46, 45, 44, 43, 42, 41, 40,	\
  39, 38, 37, 36, 35, 34, 33, 32}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the i860, all registers hold 32 bits worth.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

#define REGNO_MODE_ALIGNED(REGNO, MODE) \
  (((REGNO) % ((GET_MODE_UNIT_SIZE (MODE) + 3) / 4)) == 0)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.

   On the i860, we allow anything to go into any registers, but we require
   any sort of value going into the FP registers to be properly aligned
   (based on its size) within the FP register set.
*/
#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  (((REGNO) < 32) 							\
   || (MODE) == VOIDmode || (MODE) == BLKmode				\
   || REGNO_MODE_ALIGNED (REGNO, MODE))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
/* I think that is not always true; alignment restrictions for doubles
   should not prevent tying them with singles.  So try allowing that.
   On the other hand, don't let fixed and floating be tied;
   this restriction is not necessary, but may make better code.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT		\
    || GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)	\
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT		\
       || GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* i860 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 2

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 3

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 28

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 29

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 16

/* Register to use when a source of a floating-point zero is needed.  */
#define F0_REGNUM	32

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
   
/* The i860 has two kinds of registers, hence four classes.  */

enum reg_class { NO_REGS, GENERAL_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "GENERAL_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS	\
 {{0, 0}, {0xffffffff, 0},	\
  {0, 0xffffffff}, {0xffffffff, 0xffffffff}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
 ((REGNO) >= 32 ? FP_REGS : GENERAL_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FP_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the i860, `I' is used for the range of constants 
   an add/subtract insn can actually contain.
   But not including -0x8000, since we need
   to negate the constant sometimes.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for the range allowed in bte.
   `L' is used for the range allowed in logical insns.  */

#define SMALL_INT(X) ((unsigned) (INTVAL (X) + 0x7fff) < 0xffff)

#define LOGIC_INT(X) ((unsigned) INTVAL (X) < 0x10000)

#define SMALL_INTVAL(X) ((unsigned) ((X) + 0x7fff) < 0xffff)

#define LOGIC_INTVAL(X) ((unsigned) (X) < 0x10000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? ((unsigned) (VALUE) + 0x7fff) < 0xffff	\
   : (C) == 'J' ? (VALUE) == 0				\
   : (C) == 'K' ? (unsigned) (VALUE) < 0x20	\
   : (C) == 'L' ? (unsigned) (VALUE) < 0x10000	\
   : 0)

/* Return non-zero if the given VALUE is acceptable for the
   constraint letter C.  For the i860, constraint letter 'G'
   permits only a floating-point zero value.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  	\
  ((C) == 'G' && CONST_DOUBLE_LOW ((VALUE)) == 0	\
   && CONST_DOUBLE_HIGH ((VALUE)) == 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   If we are trying to put an integer constant into some register, prefer an
   integer register to an FP register.  If we are trying to put a 
   non-zero floating-point constant into some register, use an integer
   register if the constant is SFmode and GENERAL_REGS is one of our options.
   Otherwise, put the constant into memory.

   When reloading something smaller than a word, use a general reg
   rather than an FP reg.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  \
  ((CLASS) == ALL_REGS && GET_CODE (X) == CONST_INT ? GENERAL_REGS	\
   : ((GET_MODE (X) == HImode || GET_MODE (X) == QImode)		\
      && (CLASS) == ALL_REGS)						\
   ? GENERAL_REGS							\
   : (GET_CODE (X) == CONST_DOUBLE					\
      && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT			\
      && ! CONST_DOUBLE_OK_FOR_LETTER_P (X, 'G'))			\
   ? ((CLASS) == ALL_REGS && GET_MODE (X) == SFmode ? GENERAL_REGS	\
      : (CLASS) == GENERAL_REGS && GET_MODE (X) == SFmode ? (CLASS)	\
      : NO_REGS)							\
   : (CLASS))

/* Return the register class of a scratch register needed to copy IN into
   a register in CLASS in MODE.  If it can be done directly, NO_REGS is
   returned.  */

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS,MODE,IN) \
  ((CLASS) == FP_REGS && GET_CODE (IN) == CONST_INT ? GENERAL_REGS : NO_REGS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the i860, this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
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

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the i860, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

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

/* On the i860, the value register depends on the mode.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE),				\
	   (GET_MODE_CLASS (TYPE_MODE (VALTYPE)) == MODE_FLOAT	\
	    ? 40 : 16))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)				\
  gen_rtx (REG, MODE,					\
	   (GET_MODE_CLASS ((MODE)) == MODE_FLOAT	\
	    ? 40 : 16))

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 40 || (N) == 16)

/* 1 if N is a possible register number for function argument passing.
   On the i860, these are r16-r27 and f8-f15.  */

#define FUNCTION_ARG_REGNO_P(N)		\
  (((N) < 28 && (N) > 15) || ((N) < 48 && (N) >= 40))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the i860, we must count separately the number of general registers used
   and the number of float registers used.  */

struct cumulative_args { int ints, floats; };
#define CUMULATIVE_ARGS struct cumulative_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the i860, the general-reg offset normally starts at 0,
   but starts at 4 bytes
   when the function gets a structure-value-address as an
   invisible first argument.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
 ((CUM).ints = ((FNTYPE) != 0 && aggregate_value_p ((FNTYPE)) \
		? 4 : 0),			\
  (CUM).floats = 0)

/* Machine-specific subroutines of the following macros.  */
#define CEILING(X,Y)  (((X) + (Y) - 1) / (Y))
#define ROUNDUP(X,Y)  (CEILING ((X), (Y)) * (Y))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)
   Floats, and doubleword ints, are returned in f regs;
   other ints, in r regs.
   Aggregates, even short ones, are passed in memory.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
 ((TYPE) != 0 && (TREE_CODE ((TYPE)) == RECORD_TYPE		\
		  || TREE_CODE ((TYPE)) == UNION_TYPE)		\
  ? 0								\
  : GET_MODE_CLASS ((MODE)) == MODE_FLOAT || (MODE) == DImode	\
  ? ((CUM).floats = (ROUNDUP ((CUM).floats, GET_MODE_SIZE ((MODE)))	\
		     + ROUNDUP (GET_MODE_SIZE (MODE), 4)))	\
  : GET_MODE_CLASS ((MODE)) == MODE_INT				\
  ? ((CUM).ints = (ROUNDUP ((CUM).ints, GET_MODE_SIZE ((MODE))) \
		   + ROUNDUP (GET_MODE_SIZE (MODE), 4)))	\
  : 0)

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

/* On the i860, the first 12 words of integer arguments go in r16-r27,
   and the first 8 words of floating arguments go in f8-f15.
   DImode values are treated as floats.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)		\
 ((TYPE) != 0 && (TREE_CODE ((TYPE)) == RECORD_TYPE	\
		  || TREE_CODE ((TYPE)) == UNION_TYPE)	\
  ? 0							\
  : GET_MODE_CLASS ((MODE)) == MODE_FLOAT || (MODE) == DImode	\
  ? (ROUNDUP ((CUM).floats, GET_MODE_SIZE ((MODE))) < 32	\
     ? gen_rtx (REG, (MODE),				\
		40+(ROUNDUP ((CUM).floats,		\
			     GET_MODE_SIZE ((MODE)))	\
		    / 4))				\
     : 0)						\
  : GET_MODE_CLASS ((MODE)) == MODE_INT			\
  ? (ROUNDUP ((CUM).ints, GET_MODE_SIZE ((MODE))) < 48	\
     ? gen_rtx (REG, (MODE),				\
		16+(ROUNDUP ((CUM).ints,		\
			     GET_MODE_SIZE ((MODE)))	\
		    / 4))				\
     : 0)						\
  : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* If defined, a C expression that gives the alignment boundary, in
   bits, of an argument with the specified mode and type.  If it is
   not defined,  `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)			\
  (((TYPE) != 0)						\
   ? ((TYPE_ALIGN(TYPE) <= PARM_BOUNDARY)			\
      ? PARM_BOUNDARY						\
      : TYPE_ALIGN(TYPE))					\
   : ((GET_MODE_ALIGNMENT(MODE) <= PARM_BOUNDARY)		\
      ? PARM_BOUNDARY						\
      : GET_MODE_ALIGNMENT(MODE)))

/* This macro generates the assembly code for function entry.

   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
*/

#define FUNCTION_PROLOGUE(FILE, SIZE) function_prologue ((FILE), (SIZE))

/* Output a no-op just before the beginning of the function,
   to ensure that there does not appear to be a delayed branch there.
   Such a thing would confuse interrupt recovery.  */
#define ASM_OUTPUT_FUNCTION_PREFIX(FILE,NAME) \
  fprintf (FILE, "\tnop\n")

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   abort ();

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit.

   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.
*/

#define FUNCTION_EPILOGUE(FILE, SIZE) function_epilogue ((FILE), (SIZE))

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.

   On the i860, FRAME_POINTER_REQUIRED is always 1, so the definition of this
   macro doesn't matter.  But it must be defined.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) \
  do { (DEPTH) = 0; } while (0)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the i860, the trampoline contains five instructions:
     orh #TOP_OF_FUNCTION,r0,r31
     or #BOTTOM_OF_FUNCTION,r31,r31
     orh #TOP_OF_STATIC,r0,r29
     bri r31
     or #BOTTOM_OF_STATIC,r29,r29  */
#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0xec1f0000));	\
  ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0xe7ff0000));	\
  ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0xec1d0000));	\
  ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x4000f800));	\
  ASM_OUTPUT_INT (FILE, gen_rtx (CONST_INT, VOIDmode, 0xe7bd0000));	\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 20

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   Store hi function at +0, low function at +4,
   hi static at +8, low static at +16  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  rtx low_cxt = expand_shift (RSHIFT_EXPR, SImode, CXT,			\
			      size_int (16), 0, 0);			\
  rtx low_fn = expand_shift (RSHIFT_EXPR, SImode, FNADDR,		\
			     size_int (16), 0, 0);			\
  emit_move_insn (gen_rtx (MEM, HImode, plus_constant (TRAMP, 16)),	\
		  gen_rtx (SUBREG, HImode, CXT));			\
  emit_move_insn (gen_rtx (MEM, HImode, plus_constant (TRAMP, 4)),	\
		  gen_rtx (SUBREG, HImode, FNADDR));			\
  emit_move_insn (gen_rtx (MEM, HImode, plus_constant (TRAMP, 8)),	\
		  gen_rtx (SUBREG, HImode, low_cxt));			\
  emit_move_insn (gen_rtx (MEM, HImode, plus_constant (TRAMP, 0)),	\
		  gen_rtx (SUBREG, HImode, low_fn));			\
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_FP_P(REGNO) \
(((REGNO) ^ 0x20) < 32 || (unsigned) (reg_renumber[REGNO] ^ 0x20) < 32)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the i860, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the Sparc, this is anything but a CONST_DOUBLE.
   Let's try permitting CONST_DOUBLEs and see what happens.  */

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
#define REG_OK_FOR_INDEX_P(X) (((unsigned) REGNO (X)) - 32 >= 14)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (((unsigned) REGNO (X)) - 32 >= 14)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On the i860, the actual addresses must be REG+REG or REG+SMALLINT.
   But we can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT.

   The displacement in an address must be a multiple of the alignment.

   Try making SYMBOL_REF (and other things which are CONSTANT_ADDRESS_P)
   a legitimate address, regardless.  Because the only insns which can use
   memory are load or store insns, the added hair in the machine description
   is not that bad.  It should also speed up the compiler by halving the number
   of insns it must manage for each (MEM (SYMBOL_REF ...)) involved.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (GET_CODE (X) == REG)				\
    { if (REG_OK_FOR_BASE_P (X)) goto ADDR; }		\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      if (GET_CODE (XEXP (X, 0)) == REG			\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
	{						\
	  if (GET_CODE (XEXP (X, 1)) == CONST_INT	\
	      && INTVAL (XEXP (X, 1)) >= -0x8000	\
	      && INTVAL (XEXP (X, 1)) < 0x8000		\
	      && (INTVAL (XEXP (X, 1)) & (GET_MODE_SIZE (MODE) - 1)) == 0) \
	    goto ADDR;					\
	}						\
      else if (GET_CODE (XEXP (X, 1)) == REG		\
	  && REG_OK_FOR_BASE_P (XEXP (X, 1)))		\
	{						\
	  if (GET_CODE (XEXP (X, 0)) == CONST_INT	\
	      && INTVAL (XEXP (X, 0)) >= -0x8000	\
	      && INTVAL (XEXP (X, 0)) < 0x8000		\
	      && (INTVAL (XEXP (X, 0)) & (GET_MODE_SIZE (MODE) - 1)) == 0) \
	    goto ADDR;					\
	}						\
    }							\
  else if (CONSTANT_ADDRESS_P (X))			\
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

/* On the i860, change COMPLICATED + CONSTANT to REG+CONSTANT.
   Also change a symbolic constant to a REG,
   though that may not be necessary.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	\
{ if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == PLUS)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == PLUS)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) != REG	\
      && GET_CODE (XEXP (X, 0)) != CONST_INT)			\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   copy_to_mode_reg (SImode, XEXP (X, 0)));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) != REG	\
      && GET_CODE (XEXP (X, 1)) != CONST_INT)			\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   copy_to_mode_reg (SImode, XEXP (X, 1)));	\
  if (GET_CODE (x) == SYMBOL_REF)				\
    (X) = copy_to_reg (X);					\
  if (GET_CODE (x) == CONST)					\
    (X) = copy_to_reg (X);					\
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the i860 this is never true.
   There are some addresses that are invalid in wide modes
   but valid for narrower modes, but they shouldn't affect
   the places that use this macro.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Must pass floats to libgcc functions as doubles.  */
#define LIBGCC_NEEDS_DOUBLE 1

#define DIVSI3_LIBCALL "*.div"
#define UDIVSI3_LIBCALL "*.udiv"
#define REMSI3_LIBCALL "*.rem"
#define UREMSI3_LIBCALL "*.urem"

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 16

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Value is 1 if it generates better code to perform an unsigned comparison
   on the given literal integer value in the given mode when we are only
   looking for an equal/non-equal result.  */
/* For the i860, if the immediate value has its high-order 27 bits zero,
   then we want to engineer an unsigned comparison for EQ/NE because
   such values can fit in the 5-bit immediate field of a bte or btne
   instruction (which gets zero extended before comparing).  For all
   other immediate values on the i860, we will use signed compares
   because that avoids the need for doing explicit xor's to zero_extend
   the non-constant operand in cases where it was (mem:QI ...) or a
   (mem:HI ...) which always gets automatically sign-extended by the
   hardware upon loading.  */

#define LITERAL_COMPARE_BETTER_UNSIGNED(intval, mode)                   \
  (((unsigned) (intval) & 0x1f) == (unsigned) (intval))

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE, OUTER_CODE)			\
  case CONST_INT:						\
    if (INTVAL (RTX) == 0)					\
      return 0;							\
    if (INTVAL (RTX) < 0x2000 && INTVAL (RTX) >= -0x2000) return 1; \
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 4;							\
  case CONST_DOUBLE:						\
    return 6;

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch.

   Set this to 3 on the i860 since branches may often take three cycles.  */

#define BRANCH_COST 3

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* This holds the value sourcing h%r31.  We keep this info
   around so that mem/mem ops, such as increment and decrement,
   etc, can be performed reasonably.  */
#define CC_STATUS_MDEP rtx

#define CC_STATUS_MDEP_INIT (cc_status.mdep = 0)

#define CC_NEGATED	01000

/* We use this macro in those places in the i860.md file where we would
   normally just do a CC_STATUS_INIT (for other machines).  This macro
   differs from CC_STATUS_INIT in that it doesn't mess with the special
   bits or fields which describe what is currently in the special r31
   scratch register, but it does clear out everything that actually
   relates to the condition code bit of the i860.  */

#define CC_STATUS_PARTIAL_INIT						\
 (cc_status.flags &= (CC_KNOW_HI_R31 | CC_HI_R31_ADJ),			\
  cc_status.value1 = 0,							\
  cc_status.value2 = 0)

/* Nonzero if we know the value of h%r31.  */
#define CC_KNOW_HI_R31 0100000

/* Nonzero if h%r31 is actually ha%something, rather than h%something.  */
#define CC_HI_R31_ADJ 0200000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the i860, only compare insns set a useful condition code.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ cc_status.flags &= (CC_KNOW_HI_R31 | CC_HI_R31_ADJ);	\
  cc_status.value1 = 0; cc_status.value2 = 0; }

/* Control the assembler format that we output.  */

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_BYTE_OP "\t.byte"
#define ASM_SHORT "\t.short"
#define ASM_LONG "\t.long"
#define ASM_DOUBLE "\t.double"

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#define ASM_FILE_START(FILE)
#if 0
#define ASM_FILE_START(FILE)					\
  do { output_file_directive ((FILE), main_input_filename);	\
       if (optimize) ASM_FILE_START_1 (FILE);			\
     } while (0)
#endif

#define ASM_FILE_START_1(FILE)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "sp", "fp", "r4", "r5", "r6", "r7", "r8", "r9",		\
 "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19",	\
 "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",	\
 "r30", "r31",								\
 "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9",		\
 "f10", "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f19",	\
 "f20", "f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29",	\
 "f30", "f31" }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)					\
  do { fputs (".globl ", FILE);					\
	assemble_name (FILE, NAME);					\
	fputs ("\n", FILE);						\
  } while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.

   This definition is overridden in i860v4.h because under System V
   Release 4, user-level symbols are *not* prefixed with underscores in
   the generated assembly code.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* This is how to output an internal numbered label which
   labels a jump table.  */

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, JUMPTABLE)		\
do { ASM_OUTPUT_ALIGN ((FILE), 2);					\
     ASM_OUTPUT_INTERNAL_LABEL ((FILE), PREFIX, NUM);			\
   } while (0)

/* Output at the end of a jump table.  */

#define ASM_OUTPUT_CASE_END(FILE,NUM,INSN)	\
  fprintf (FILE, ".text\n")

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double %.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.float %.12e\n", (VALUE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.short "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output code to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)					\
  fprintf (FILE, "\taddu -16,%ssp,%ssp\n\t%sst.l %s%s,0(%ssp)\n",	\
	i860_reg_prefix, i860_reg_prefix,				\
	((REGNO) < 32 ? "" : "f"),					\
	i860_reg_prefix, reg_names[REGNO],				\
	i860_reg_prefix)

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)					\
  fprintf (FILE, "\t%sld.l 0(%ssp),%s%s\n\taddu 16,%ssp,%ssp\n",	\
	((REGNO) < 32 ? "" : "f"),					\
	i860_reg_prefix,						\
	i860_reg_prefix, reg_names[REGNO],				\
	i860_reg_prefix, i860_reg_prefix)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (The i860 does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", 1 << (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.blkb %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

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

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   In the following comments, the term "constant address" is used frequently.
   For an exact definition of what constitutes a "constant address" see the
   output_addr_const routine in final.c

   On the i860, the following target-specific special codes are recognized:

	`r'	The operand can be anything, but if is is an immediate zero
		value (either integer or floating point) then it will be
		represented as `r0' or as `f0' (respectively).

	`m'	The operand is a memory ref (to a constant address) but print
		its address as a constant.

	`L'	The operand is a numeric constant, a constant address, or
		a memory ref to a constant address.  Print the correct
		notation to yield the low part of the given value or
		address or the low part of the address of the referred
		to memory object.

	`H'	The operand is a numeric constant, a constant address, or
		a memory ref to a constant address.  Print the correct
		notation to yield the high part of the given value or
		address or the high part of the address of the referred
		to memory object.

	`h'	The operand is a numeric constant, a constant address, or
		a memory ref to a constant address.  Either print the
		correct notation to yield the plain high part of the
		given value or address (or the plain high part of the
		address of the memory object) or else print the correct
		notation to yield the "adjusted" high part of the given
		address (or of the address of the referred to memory object).

		The choice of what to print depends upon whether the address
		in question is relocatable or not.  If it is relocatable,
		print the notation to get the adjusted high part.  Otherwise
		just print the notation to get the plain high part.  Note
		that "adjusted" high parts are generally used *only* when
		the next following instruction uses the low part of the
		address as an offset, as in `offset(reg)'.

	`R'	The operand is a floating-pointer register.  Print the
		name of the next following (32-bit) floating-point register.
		(This is used when moving a value into just the most
		significant part of a floating-point register pair.)

	`?'	(takes no operand) Substitute the value of i860_reg_prefix
		at this point.  The value of i860_reg_prefix is typically
		a null string for most i860 targets, but for System V
		Release 4 the i860 assembler syntax requires that all
		names of registers be prefixed with a percent-sign, so
		for SVR4, the value of i860_reg_prefix is initialized to
		"%" in i860.c.
*/

extern char *i860_reg_prefix;
extern unsigned long sfmode_constant_to_ulong ();

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '?')

/* The following macro definition is overridden in i860v4.h
   because the svr4 i860 assembler required a different syntax
   for getting parts of constant/relocatable values.  */

#define PRINT_OPERAND_PART(FILE, X, PART_CODE)				\
  do { fprintf (FILE, "%s%%", PART_CODE);				\
	output_address (X);						\
  } while (0)

#define OPERAND_LOW_PART	"l"
#define OPERAND_HIGH_PART	"h"
/* NOTE: All documentation available for the i860 sez that you must
   use "ha" to get the relocated high part of a relocatable, but
   reality sez different.  */
#define OPERAND_HIGH_ADJ_PART	"ha"

#define PRINT_OPERAND(FILE, X, CODE)					\
{ if ((CODE) == '?')							\
    fprintf (FILE, "%s", i860_reg_prefix);				\
  else if (CODE == 'R')							\
    fprintf (FILE, "%s%s", i860_reg_prefix, reg_names[REGNO (X) + 1]);	\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s%s", i860_reg_prefix, reg_names[REGNO (X)]);	\
  else if ((CODE) == 'm')						\
    output_address (XEXP (X, 0));					\
  else if ((CODE) == 'L')						\
    {									\
      if (GET_CODE (X) == MEM)						\
	PRINT_OPERAND_PART (FILE, XEXP (X, 0), OPERAND_LOW_PART);	\
      else								\
	PRINT_OPERAND_PART (FILE, X, OPERAND_LOW_PART);			\
    }									\
  else if ((CODE) == 'H')						\
    {									\
      if (GET_CODE (X) == MEM)						\
	PRINT_OPERAND_PART (FILE, XEXP (X, 0), OPERAND_HIGH_PART);	\
      else								\
	PRINT_OPERAND_PART (FILE, X, OPERAND_HIGH_PART);		\
    }									\
  else if ((CODE) == 'h')						\
    {									\
      if (GET_CODE (X) == MEM)						\
	PRINT_OPERAND_PART (FILE, XEXP (X, 0), OPERAND_HIGH_ADJ_PART);	\
      else								\
	PRINT_OPERAND_PART (FILE, X, OPERAND_HIGH_ADJ_PART);		\
    }									\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if ((CODE) == 'r' && (X) == const0_rtx)				\
    fprintf (FILE, "%sr0", i860_reg_prefix);				\
  else if ((CODE) == 'r' && (X) == CONST0_RTX (GET_MODE (X)))		\
    fprintf (FILE, "%sf0", i860_reg_prefix);				\
  else if (GET_CODE (X) == CONST_DOUBLE)				\
    fprintf (FILE, "0x%x", sfmode_constant_to_ulong (X));		\
  else									\
    output_addr_const (FILE, X); }

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;					\
  int offset = 0;						\
  register rtx addr = ADDR;					\
  if (GET_CODE (addr) == REG)					\
    {								\
      fprintf (FILE, "0(%s%s)",					\
	i860_reg_prefix, reg_names[REGNO (addr)]);		\
    }								\
  else if (GET_CODE (addr) == CONST_DOUBLE			\
            && GET_MODE (addr) == SFmode)			\
    fprintf (FILE, "0x%x", sfmode_constant_to_ulong (addr));	\
  else if (GET_CODE (addr) == PLUS)				\
    {								\
      if ((GET_CODE (XEXP (addr, 0)) == CONST_INT)		\
	  && (GET_CODE (XEXP (addr, 1)) == REG))		\
	fprintf (FILE, "%d(%s%s)", INTVAL (XEXP (addr, 0)),	\
	    i860_reg_prefix, reg_names[REGNO (XEXP (addr, 1))]);\
      else if ((GET_CODE (XEXP (addr, 1)) == CONST_INT)		\
	  && (GET_CODE (XEXP (addr, 0)) == REG))		\
	fprintf (FILE, "%d(%s%s)", INTVAL (XEXP (addr, 1)),	\
	    i860_reg_prefix, reg_names[REGNO (XEXP (addr, 0))]);\
      else if ((GET_CODE (XEXP (addr, 0)) == REG)		\
	  && (GET_CODE (XEXP (addr, 1)) == REG))		\
	fprintf (FILE, "%s%s(%s%s)",				\
	    i860_reg_prefix, reg_names[REGNO (XEXP (addr, 0))],	\
	    i860_reg_prefix, reg_names[REGNO (XEXP (addr, 1))]);\
      else							\
	output_addr_const (FILE, addr);				\
    }								\
  else								\
    {								\
      output_addr_const (FILE, addr);				\
    }								\
}

/* The following #defines are used when compiling the routines in
   libgcc1.c.  Since the i860 calling conventions require single
   precision floats to be passed in the floating-point registers
   (rather than in the general registers) we have to build the
   libgcc1.c routines in such a way that they know the actual types
   of their formal arguments and the actual types of their return
   values.  Otherwise, gcc will generate calls to the libgcc1.c
   routines, passing arguments in the floating-point registers,
   but the libgcc1.c routines will expect their arguments on the
   stack (where the i860 calling conventions require structs &
   unions to be passed).  */

#define FLOAT_TYPE_VALUE	float
#define INTIFY(FLOATVAL)	(FLOATVAL)
#define FLOATIFY(INTVAL)	(INTVAL)
#define FLOAT_ARG_TYPE		float


/* Optionally define this if you have added predicates to
   `MACHINE.c'.  This macro is called within an initializer of an
   array of structures.  The first field in the structure is the
   name of a predicate and the second field is an array of rtl
   codes.  For each predicate, list all rtl codes that can be in
   expressions matched by the predicate.  The list should have a
   trailing comma.  Here is an example of two entries in the list
   for a typical RISC machine:

   #define PREDICATE_CODES \
     {"gen_reg_rtx_operand", {SUBREG, REG}},  \
     {"reg_or_short_cint_operand", {SUBREG, REG, CONST_INT}},

   Defining this macro does not affect the generated code (however,
   incorrect definitions that omit an rtl code that may be matched
   by the predicate can cause the compiler to malfunction). 
   Instead, it allows the table built by `genrecog' to be more
   compact and efficient, thus speeding up the compiler.  The most
   important predicates to include in the list specified by this
   macro are thoses used in the most insn patterns.

   Note that for the i860 we have one more predicate, i.e.
   `single_insn_src_operand', however this is used only
   infrequently, so we don't put in the PREDICATE_CODES list.
*/

#define PREDICATE_CODES							\
   {"reg_or_0_operand",		{REG, SUBREG, CONST_INT}},		\
   {"arith_operand",		{REG, SUBREG, CONST_INT}},		\
   {"logic_operand",		{REG, SUBREG, CONST_INT}},		\
   {"shift_operand",		{REG, SUBREG, CONST_INT}},		\
   {"compare_operand",		{REG, SUBREG, CONST_INT}},		\
   {"arith_const_operand",	{CONST_INT}},				\
   {"logic_const_operand",	{CONST_INT}},				\
   {"bte_operand",		{REG, SUBREG, CONST_INT}},		\
   {"indexed_operand",		{MEM}},					\
   {"load_operand",		{MEM}},

/* Define the information needed to generate branch insns.  This is stored
   from the compare operation.  Note that we can't use "rtx" here since it
   hasn't been defined!  */

extern struct rtx_def *i860_compare_op0, *i860_compare_op1;

/* Declare things which are defined in i860.c but called from
   insn-output.c.  */

extern unsigned long sfmode_constant_to_ulong ();
extern char *output_load ();
extern char *output_store ();
extern char *output_move_double ();
extern char *output_fp_move_double ();
extern char *output_block_move ();
extern char *output_delay_insn ();
extern char *output_delayed_branch ();
extern void output_load_address ();
