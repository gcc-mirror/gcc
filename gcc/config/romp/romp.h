/* Definitions of target machine for GNU compiler, for ROMP chip.
   Copyright (C) 1989, 1991, 1993, 1995, 1996, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dibm032 -Dunix -Asystem=unix -Asystem=bsd  -Acpu=ibm032 -Amachine=ibm032"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION ;

/* Add -lfp_p when running with -p or -pg.  */
#define LIB_SPEC "%{pg:-lfp_p}%{p:-lfp_p} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

/* Run-time compilation parameters selecting different hardware subsets.  */

/* Flag to generate all multiplies as an in-line sequence of multiply-step
   insns instead of calling a library routine.  */
#define TARGET_IN_LINE_MUL (target_flags & 1)

/* Flag to generate padded floating-point data blocks.  Otherwise, we generate
   them the minimum size.  This trades off execution speed against size.  */
#define TARGET_FULL_FP_BLOCKS (target_flags & 2)

/* Flag to pass and return floating point values in floating point registers.
   Since this violates the linkage convention, we feel free to destroy fr2
   and fr3 on function calls.
   fr1-fr3 are used to pass the arguments.  */
#define TARGET_FP_REGS (target_flags & 4)

/* Flag to return structures of more than one word in memory.  This is for
   compatibility with the MetaWare HighC (hc) compiler.  */
#define TARGET_HC_STRUCT_RETURN (target_flags & 010)

extern int target_flags;

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES		\
  { {"in-line-mul", 1},		\
    {"call-lib-mul", -1},	\
    {"full-fp-blocks", 2},	\
    {"minimum-fp-blocks", -2},	\
    {"fp-arg-in-fpregs", 4},	\
    {"fp-arg-in-gregs", -4},	\
    {"hc-struct-return", 010},  \
    {"nohc-struct-return", - 010}, \
    { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 3

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
/* That is true on ROMP.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on ROMP.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered. 

   For ROMP we can decide arbitrarily since there are no machine instructions
   for them.  Might as well be consistent with bits and bytes.  */
#define WORDS_BIG_ENDIAN 1

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   ROMP has 16 fullword registers and 8 floating point registers.

   In addition, the difference between the frame and argument pointers is
   a function of the number of registers saved, so we need to have a register
   to use for AP that will later be eliminated in favor of sp or fp.  This is
   a normal register, but it is fixed.  */

#define FIRST_PSEUDO_REGISTER 25

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On ROMP, r1 is used for the stack and r14 is used for a
   data area pointer.

   HACK WARNING:  On the RT, there is a bug in code generation for
   the MC68881 when the first and third operands are the same floating-point
   register.  See the definition of the FINAL_PRESCAN_INSN macro for details.
   Here we need to reserve fr0 for this purpose.  */
#define FIXED_REGISTERS  \
 {0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  1,							\
  1, 0, 0, 0, 0, 0, 0, 0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS				\
 {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
  1,							\
  1, 1, 0, 0, 0, 0, 0, 0}

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   We allocate in the following order:
	fr0, fr1	(not saved)
	fr2 ... fr6
	fr7		(more expensive for some FPA's)
	r0		(not saved and won't conflict with parameter register)
	r4, r3, r2	(not saved, highest used first to make less conflict)
	r5		(not saved, but forces r6 to be saved if DI/DFmode)
	r15, r14, r13, r12, r11, r10, r9, r8, r7, r6 (less to save)
	r1, ap 			*/

#define REG_ALLOC_ORDER		\
  {17, 18,			\
   19, 20, 21, 22, 23,		\
   24,				\
   0,				\
   4, 3, 2,			\
   5,				\
   15, 14, 13, 12, 11, 10,	\
   9, 8, 7, 6, 			\
   1, 16}

/* True if register is floating-point.  */
#define FP_REGNO_P(N) ((N) >= 17)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On ROMP, ordinary registers hold 32 bits worth;
   a single floating point register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (FP_REGNO_P (REGNO) ? GET_MODE_NUNITS (MODE)	\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On ROMP, the cpu registers can hold any mode but the float registers
   can hold only floating point.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (! FP_REGNO_P (REGNO) || GET_MODE_CLASS (MODE) == MODE_FLOAT	\
   || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT		\
    || GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)	\
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT		\
       || GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

/* A C expression returning the cost of moving data from a register of class
   CLASS1 to one of CLASS2.

   On the ROMP, access to floating-point registers is expensive (even between
   two FP regs.)  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)	\
  (2 + 10 * ((CLASS1) == FP_REGS) + 10 * (CLASS2 == FP_REGS))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* ROMP pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 1

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 13

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 16

/* Place to put static chain when calling a function that requires it.  */
#define STATIC_CHAIN							\
  gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, -36))

/* Place where static chain is found upon entry to routine.  */
#define STATIC_CHAIN_INCOMING						\
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, -20))

/* Place that structure value return address is placed.

   On the ROMP, it is passed as an extra parameter.  */
#define STRUCT_VALUE	0

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
   
/* The ROMP has two types of registers, general and floating-point.

   However, r0 is special in that it cannot be used as a base register.
   So make a class for registers valid as base registers.

   For floating-point support, add classes that just consist of r0 and
   r15, respectively.  */

enum reg_class { NO_REGS, R0_REGS, R15_REGS, BASE_REGS, GENERAL_REGS,
		 FP_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "R0_REGS", "R15_REGS", "BASE_REGS", "GENERAL_REGS", \
  "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0}, {0x00001}, {0x08000}, {0x1fffe}, {0x1ffff},  \
			    {0x1fe0000}, {0x1ffffff} }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
 ((REGNO) == 0 ? GENERAL_REGS : FP_REGNO_P (REGNO) ? FP_REGS : BASE_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS BASE_REGS
#define BASE_REG_CLASS BASE_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FP_REGS		\
   : (C) == 'b' ? BASE_REGS	\
   : (C) == 'z' ? R0_REGS	\
   : (C) == 't' ? R15_REGS	\
   : NO_REGS)

/* The letters I, J, K, L, M, N, and P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is constants less than 16
   `J' is negative constants greater than -16
   `K' is the range for a normal D insn.
   `L' is a constant with only the low-order 16 bits set
   `M' is a constant with only the high-order 16 bits set
   `N' is a single-bit constant
   `O' is a constant with either the high-order or low-order 16 bits all ones
   `P' is the complement of a single-bit constant
  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  		   \
   ( (C) == 'I' ? (unsigned) (VALUE) < 0x10		   \
   : (C) == 'J' ? (VALUE) < 0 && (VALUE) > -16		   \
   : (C) == 'K' ? (unsigned) ((VALUE) + 0x8000) < 0x10000  \
   : (C) == 'L' ? ((VALUE) & 0xffff0000) == 0		   \
   : (C) == 'M' ? ((VALUE) & 0xffff) == 0		   \
   : (C) == 'N' ? exact_log2 (VALUE) >= 0		   \
   : (C) == 'O' ? ((VALUE) & 0xffff) == 0xffff		   \
		  || ((VALUE) & 0xffff0000) == 0xffff0000  \
   : (C) == 'P' ? exact_log2 (~ (VALUE)) >= 0		   \
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.
   No floating-point constants on ROMP.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  0

/* Optional extra constraints for this machine.

   For the ROMP, `Q' means that this is a memory operand but not a symbolic
   memory operand.  Note that an unassigned pseudo register is such a
   memory operand.  If register allocation has not been done, we reject
   pseudos, since we assume (hope) that they will get hard registers.

   `R' means that this is a constant pool reference to the current function.
   This is just r14 and so can be treated as a register.  We bother with this
   just in move insns as that is the only place it is likely to occur.

   `S' means that this is the address of a constant pool location.  This is
   equal to r14 plus a constant.  We also only check for this in move insns.  */

#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q' ?						\
   ((GET_CODE (OP) == REG				\
     && REGNO (OP) >= FIRST_PSEUDO_REGISTER		\
     && reg_renumber != 0				\
     && reg_renumber[REGNO (OP)] < 0)			\
    || (GET_CODE (OP) == MEM				\
        && ! symbolic_memory_operand (OP, VOIDmode)))	\
   : (C) == 'R' ? current_function_operand (OP, VOIDmode) \
   : (C) == 'S' ? constant_pool_address_operand (OP, VOIDmode) \
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   For the ROMP, if X is a memory reference that involves a symbol,
   we must use a BASE_REGS register instead of GENERAL_REGS
   to do the reload. The argument of MEM be either REG, PLUS, or SYMBOL_REF
   to be valid, so we assume that this is the case.

   Also, if X is an integer class, ensure that floating-point registers
   aren't used.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)					\
  ((CLASS) == FP_REGS && GET_MODE_CLASS (GET_MODE (X)) == MODE_INT	\
   ? GENERAL_REGS :							\
   (CLASS) != GENERAL_REGS ? (CLASS) :					\
   GET_CODE (X) != MEM ? GENERAL_REGS :					\
   GET_CODE (XEXP (X, 0)) == SYMBOL_REF ? BASE_REGS :			\
   GET_CODE (XEXP (X, 0)) == LABEL_REF ? BASE_REGS :			\
   GET_CODE (XEXP (X, 0)) == CONST ? BASE_REGS :			\
   GET_CODE (XEXP (X, 0)) == REG ? GENERAL_REGS :			\
   GET_CODE (XEXP (X, 0)) != PLUS ? GENERAL_REGS :			\
   GET_CODE (XEXP (XEXP (X, 0), 1)) == SYMBOL_REF ? BASE_REGS :		\
   GET_CODE (XEXP (XEXP (X, 0), 1)) == LABEL_REF ? BASE_REGS :		\
   GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST ? BASE_REGS : GENERAL_REGS)

/* Return the register class of a scratch register needed to store into
   OUT from a register of class CLASS in MODE.  

   On the ROMP, we cannot store into a symbolic memory address from an
   integer register; we need a BASE_REGS register as a scratch to do it.  */

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, OUT) \
  (GET_MODE_CLASS (MODE) == MODE_INT && symbolic_memory_operand (OUT, MODE) \
   ? BASE_REGS : NO_REGS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.

   On ROMP, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FP_REGS ? 1			\
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

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
   of the first local allocated.
   On the ROMP, if we set the frame pointer to 15 words below the highest
   address of the highest local variable, the first 16 words will be
   addressable via D-short insns.  */
#define STARTING_FRAME_OFFSET 64

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On ROMP, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   On the ROMP, we define the argument pointer to the start of the argument
   area.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define this if stack space is still allocated for a parameter passed
   in a register.  The value is the number of bytes.  */
#define REG_PARM_STACK_SPACE(FNDECL) 16

/* This is the difference between the logical top of stack and the actual sp.

   For the ROMP, sp points past the words allocated for the first four outgoing
   arguments (they are part of the callee's frame).  */
#define STACK_POINTER_OFFSET -16

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable current_function_outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.

   On ROMP the value is found in r2, unless the machine specific option
   fp-arg-in-fpregs is selected, in which case FP return values are in fr1 */

#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  gen_rtx_REG (TYPE_MODE (VALTYPE),					\
	       (TARGET_FP_REGS						\
		&& GET_MODE_CLASS (TYPE_MODE (VALTYPE)) == MODE_FLOAT)	\
	       ? 18 : 2)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, 2)

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.

   For the ROMP, if compatibility with HC is required, anything of
   type DImode is returned in memory.  */

#define RETURN_IN_MEMORY(type) \
  (TYPE_MODE (type) == BLKmode \
   || (TARGET_HC_STRUCT_RETURN && TYPE_MODE (type) == DImode))

/* 1 if N is a possible register number for a function value
   as seen by the caller.

   On ROMP, r2 is the only register thus used unless fp values are to be
   returned in fp regs, in which case fr1 is also used.  */

#define FUNCTION_VALUE_REGNO_P(N)  ((N) == 2 || ((N) == 18 && TARGET_FP_REGS))

/* 1 if N is a possible register number for function argument passing.
   On ROMP, these are r2-r5 (and fr1-fr4 if fp regs are used).  */

#define FUNCTION_ARG_REGNO_P(N)	\
  (((N) <= 5 && (N) >= 2) || (TARGET_FP_REGS && (N) > 17 && (N) < 21))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the ROMP, this is a structure.  The first word is the number of
   words of (integer only if -mfp-arg-in-fpregs is specified) arguments
   scanned so far (including the invisible argument, if any, which holds
   the structure-value-address).  The second word hold the corresponding
   value for floating-point arguments, except that both single and double
   count as one register.  */

struct rt_cargs {int gregs, fregs; };
#define CUMULATIVE_ARGS struct rt_cargs 

#define USE_FP_REG(MODE,CUM)					\
  (TARGET_FP_REGS && GET_MODE_CLASS (MODE) == MODE_FLOAT	\
   && (CUM).fregs < 3)

/* Define intermediate macro to compute the size (in registers) of an argument
   for the ROMP.  */

#define ROMP_ARG_SIZE(MODE, TYPE, NAMED)				\
(! (NAMED) ? 0								\
 : (MODE) != BLKmode							\
 ? (GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD 	\
 : (int_size_in_bytes (TYPE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On ROMP, the offset normally starts at 0, but starts at 4 bytes
   when the function gets a structure-value-address as an
   invisible first argument.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
  (CUM).gregs = 0,				\
  (CUM).fregs = 0

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
{ if (NAMED)						\
    {							\
      if (USE_FP_REG(MODE, CUM))			\
	(CUM).fregs++;					\
      else						\
	(CUM).gregs += ROMP_ARG_SIZE (MODE, TYPE, NAMED); \
    }							\
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

   On ROMP the first four words of args are normally in registers
   and the rest are pushed.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)				\
  (! (NAMED) ? 0							\
   : ((TYPE) != 0 && TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST) ? 0	\
   : USE_FP_REG(MODE,CUM) ? gen_rtx_REG ((MODE), (CUM).fregs + 17)	\
   : (CUM).gregs < 4 ? gen_rtx_REG ((MODE), 2 + (CUM).gregs) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)		\
  (! (NAMED) ? 0							\
   : USE_FP_REG(MODE,CUM) ? 0						\
   : (((CUM).gregs < 4							\
       && 4 < ((CUM).gregs + ROMP_ARG_SIZE (MODE, TYPE, NAMED)))	\
      ? 4 - (CUM).gregs : 0))

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if (TARGET_FP_REGS)							\
    error ("can't have varargs with -mfp-arg-in-fp-regs");		\
  else if ((CUM).gregs < 4)						\
    {									\
      int first_reg_offset = (CUM).gregs;				\
									\
      if (MUST_PASS_IN_STACK (MODE, TYPE))				\
	first_reg_offset += ROMP_ARG_SIZE (TYPE_MODE (TYPE), TYPE, 1);	\
									\
      if (first_reg_offset > 4)						\
	first_reg_offset = 4;						\
									\
      if (! NO_RTL && first_reg_offset != 4)				\
	move_block_from_reg						\
	  (2 + first_reg_offset,					\
	   gen_rtx_MEM (BLKmode,					\
			plus_constant (virtual_incoming_args_rtx,	\
				       first_reg_offset * 4)), 		\
	   4 - first_reg_offset, (4 - first_reg_offset) * UNITS_PER_WORD); \
      PRETEND_SIZE = (4 - first_reg_offset) * UNITS_PER_WORD;		\
    }									\
}

/* This macro produces the initial definition of a function name.
   On the ROMP, we need to place an extra '.' in the function name.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL)	\
{ if (TREE_PUBLIC(DECL))				\
    fprintf (FILE, "\t.globl _.%s\n", NAME);		\
  fprintf (FILE, "_.%s:\n", NAME);			\
}

/* This macro is used to output the start of the data area.

   On the ROMP, the _name is a pointer to the data area.  At that
   location is the address of _.name, which is really the name of
   the function.  We need to set all this up here.

   The global declaration of the data area, if needed, is done in 
   `assemble_function', where it thinks it is globalizing the function
   itself.  */

#define ASM_OUTPUT_POOL_PROLOGUE(FILE, NAME, DECL, SIZE)	\
{ extern int data_offset;					\
  data_section ();						\
  fprintf (FILE, "\t.align 2\n");				\
  ASM_OUTPUT_LABEL (FILE, NAME);				\
  fprintf (FILE, "\t.long _.%s, 0, ", NAME);			\
  if (current_function_calls_alloca)				\
    fprintf (FILE, "0x%x\n",					\
	     0xf6900000 + current_function_outgoing_args_size); \
  else								\
    fprintf (FILE, "0\n");					\
  data_offset = ((SIZE) + 12 + 3) / 4;				\
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)	\
  fprintf(FILE, "\tcas r0,r15,r0\n\tbali r15,mcount\n");

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
/* #define EXIT_IGNORE_STACK	1	*/

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.

   On the ROMP, we have a problem.  There are no free registers to use
   to construct the static chain and function addresses.  Hence we use
   the following kludge:  r15 (the return address) is first saved in mq.
   Then we use r15 to form the function address.  We then branch to the
   function and restore r15 in the delay slot.  This makes it appear that
   the function was called directly from the caller.

   (Note that the function address built is actually that of the data block.
   This is passed in r0 and the actual routine address is loaded into r15.)

   In addition, note that the address of the "called function", in this case
   the trampoline, is actually the address of the data area.  So we need to
   make a fake data area that will contain the address of the trampoline.
   Note that this must be defined as two half-words, since the trampoline
   template (as opposed to the trampoline on the stack) is only half-word
   aligned.  */

#define TRAMPOLINE_TEMPLATE(FILE)	\
{					\
  fprintf (FILE, "\t.short 0,0\n");	\
  fprintf (FILE, "\tcau r0,0(r0)\n");	\
  fprintf (FILE, "\toil r0,r0,0\n");	\
  fprintf (FILE, "\tmts r10,r15\n");	\
  fprintf (FILE, "\tst r0,-36(r1)\n");	\
  fprintf (FILE, "\tcau r15,0(r0)\n");	\
  fprintf (FILE, "\toil r15,r15,0\n");	\
  fprintf (FILE, "\tcas r0,r15,r0\n");	\
  fprintf (FILE, "\tls r15,0(r15)\n");	\
  fprintf (FILE, "\tbrx r15\n");	\
  fprintf (FILE, "\tmfs r10,r15\n");	\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE    36

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   On the RT, the static chain and function addresses are written in
   two 16-bit sections.

   We also need to write the address of the first instruction in
   the trampoline into the first word of the trampoline to simulate a
   data area.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CXT)		\
	romp_initialize_trampoline (ADDR, FNADDR, CXT)

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the ROMP.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.

   In addition, we use the elimination mechanism to see if r14 is needed.
   Initially we assume that it isn't.  If it is, we spill it.  This is done
   by making it an eliminable register.  It doesn't matter what we replace
   it with, since it will never occur in the rtl at this point.  */

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS				\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { 14, 0}}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   For the ROMP, if frame pointer elimination is being done, we would like to
   convert ap into fp, not sp.

   We need r14 if various conditions (tested in romp_using_r14) are true.

   All other eliminations are valid.  */
#define CAN_ELIMINATE(FROM, TO)					\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM	\
  ? ! frame_pointer_needed					\
  : (FROM) == 14 ? ! romp_using_r14 ()				\
  : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{ if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
    {									\
      if (romp_pushes_stack ())						\
	(OFFSET) = ((get_frame_size () - 64)				\
		    + current_function_outgoing_args_size);		\
      else								\
	(OFFSET) = - (romp_sa_size () + 64);				\
    }									\
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM) \
    (OFFSET) = romp_sa_size () - 16 + 64;				\
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
    {									\
      if (romp_pushes_stack ())						\
	(OFFSET) = (get_frame_size () + (romp_sa_size () - 16)		\
		    + current_function_outgoing_args_size);		\
      else								\
	(OFFSET) = -16;							\
    }									\
  else if ((FROM) == 14)						\
    (OFFSET) = 0;							\
  else									\
    abort ();								\
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT 0 */
/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_DECREMENT 0 */
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_OK_FOR_BASE_P(REGNO)				\
((REGNO) < FIRST_PSEUDO_REGISTER				\
 ? (REGNO) < 16 && (REGNO) != 0 && (REGNO) != 16		\
 : (reg_renumber[REGNO] < 16 && reg_renumber[REGNO] >= 0	\
    && reg_renumber[REGNO] != 16))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the ROMP, there is a bit of a hack here.  Basically, we wish to
   only issue instructions that are not `as' macros.  However, in the
   case of `get', `load', and `store', if the operand is a relocatable
   symbol (possibly +/- an integer), there is no way to express the
   resulting split-relocation except with the macro.  Therefore, allow
   either a constant valid in a normal (sign-extended) D-format insn or
   a relocatable expression.

   Also, for DFmode and DImode, we must ensure that both words are
   addressable.

   We define two macros: The first is given an offset (0 or 4) and indicates
   that the operand is a CONST_INT that is valid for that offset.  The second
   indicates a valid non-CONST_INT constant.  */

#define LEGITIMATE_ADDRESS_INTEGER_P(X,OFFSET)				\
  (GET_CODE (X) == CONST_INT						\
   && (unsigned) (INTVAL (X) + (OFFSET) + 0x8000) < 0x10000)

#define LEGITIMATE_ADDRESS_CONSTANT_P(X)				\
 (GET_CODE (X) == SYMBOL_REF						\
  || GET_CODE (X) == LABEL_REF						\
  || (GET_CODE (X) == CONST						\
      && (GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF		\
          || GET_CODE (XEXP (XEXP (X, 0), 0)) == LABEL_REF)		\
      && GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT))

/* Include all constant integers and constant double, but exclude 
   SYMBOL_REFs that are to be obtained from the data area (see below).  */
#define LEGITIMATE_CONSTANT_P(X)		\
  ((LEGITIMATE_ADDRESS_CONSTANT_P (X)		\
    || GET_CODE (X) == CONST_INT		\
    || GET_CODE (X) == CONST_DOUBLE)		\
   && ! (GET_CODE (X) == SYMBOL_REF && SYMBOL_REF_FLAG (X)))

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
#define REG_OK_FOR_INDEX_P(X) 0
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)		\
  (REGNO (X) != 0 && (REGNO (X) < 17 || REGNO (X) >= FIRST_PSEUDO_REGISTER))

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

   On the ROMP, a legitimate address is either a legitimate constant,
   a register plus a legitimate constant, or a register.  See the
   discussion at the LEGITIMATE_ADDRESS_CONSTANT_P macro.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{ if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
    goto ADDR;								\
  if (GET_CODE (X) != CONST_INT && LEGITIMATE_ADDRESS_CONSTANT_P (X))	\
    goto ADDR;								\
  if (GET_CODE (X) == PLUS						\
      && GET_CODE (XEXP (X, 0)) == REG					\
      && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
      && LEGITIMATE_ADDRESS_CONSTANT_P (XEXP (X, 1)))			\
	goto ADDR;							\
  if (GET_CODE (X) == PLUS						\
      && GET_CODE (XEXP (X, 0)) == REG					\
      && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
      && LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 0)			\
      && (((MODE) != DFmode && (MODE) != DImode)			\
	  || (LEGITIMATE_ADDRESS_INTEGER_P (XEXP (X, 1), 4))))		\
	goto ADDR;							\
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

   On ROMP, check for the sum of a register with a constant
   integer that is out of range.  If so, generate code to add the
   constant with the low-order 16 bits masked to the register and force
   this result into another register (this can be done with `cau').
   Then generate an address of REG+(CONST&0xffff), allowing for the 
   possibility of bit 16 being a one.

   If the register is not OK for a base register, abort.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)			\
{ if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == REG	\
    && GET_CODE (XEXP (X, 1)) == CONST_INT			\
    && (unsigned) (INTVAL (XEXP (X, 1)) + 0x8000) >= 0x10000)	\
    { int high_int, low_int;					\
      if (! REG_OK_FOR_BASE_P (XEXP (X, 0)))			\
	abort ();						\
      high_int = INTVAL (XEXP (X, 1)) >> 16;			\
      low_int = INTVAL (XEXP (X, 1)) & 0xffff;			\
      if (low_int & 0x8000)					\
	high_int += 1, low_int |= 0xffff0000;			\
      (X) = gen_rtx_PLUS (SImode,				\
			  force_operand (plus_constant (XEXP (X, 0),  \
							high_int << 16), 0), \
			  GEN_INT (low_int));			\
    }								\
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   On the ROMP this is true only if the address is valid with a zero offset
   but not with an offset of four (this means it cannot be used as an
   address for DImode or DFmode).  Since we know it is valid, we just check
   for an address that is not valid with an offset of four.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)		\
{ if (GET_CODE (ADDR) == PLUS					\
      && ! LEGITIMATE_ADDRESS_CONSTANT_P (XEXP (ADDR, 1))	\
      && ! LEGITIMATE_ADDRESS_INTEGER_P (XEXP (ADDR, 1), 4))	\
    goto LABEL;							\
}

/* Define this if some processing needs to be done immediately before
   emitting code for an insn.

   This is used on the ROMP, to compensate for a bug in the floating-point
   code.  When a floating-point operation is done with the first and third
   operands both the same floating-point register, it will generate bad code
   for the MC68881.  So we must detect this.  If it occurs, we patch the 
   first operand to be fr0 and insert a move insn to move it to the desired
   destination.  */
#define FINAL_PRESCAN_INSN(INSN,OPERANDS,NOPERANDS)			\
  { rtx op0, op1, op2, operation, tem;					\
    if (NOPERANDS >= 3	&& get_attr_type (INSN) == TYPE_FP)		\
      {									\
	op0 = OPERANDS[0];						\
	operation = OPERANDS[1];					\
	if (float_conversion (operation, VOIDmode))			\
	  operation = XEXP (operation, 0);				\
        if (float_binary (operation, VOIDmode))				\
	  {								\
	    op1 = XEXP (operation, 0), op2 = XEXP (operation, 1);	\
	    if (float_conversion (op1, VOIDmode))			\
	      op1 = XEXP (op1, 0);					\
	    if (float_conversion (op2, VOIDmode))			\
	      op2 = XEXP (op2, 0);					\
	    if (rtx_equal_p (op0, op2)					\
		&& (GET_CODE (operation) == PLUS			\
		    || GET_CODE (operation) == MULT))			\
	      tem = op1, op1 = op2, op2 = tem;				\
	    if (GET_CODE (op0) == REG && FP_REGNO_P (REGNO (op0))	\
		&& GET_CODE (op2) == REG && FP_REGNO_P (REGNO (op2))	\
		&& REGNO (op0) == REGNO (op2))				\
	      {								\
		tem = gen_rtx_REG (GET_MODE (op0), 17);		\
		emit_insn_after (gen_move_insn (op0, tem), INSN);	\
		SET_DEST (XVECEXP (PATTERN (INSN), 0, 0)) = tem; 	\
		OPERANDS[0] = tem;					\
	      }								\
	  }								\
      }									\
  }

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.

   We actually lie a bit here as overflow conditions are different.  But
   they aren't being checked anyway.  */

#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Nonzero if access to memory by bytes is no faster than for words.
   Also nonzero if doing byte operations (specifically shifts) in registers
   is undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO 1

/* Define the letter code used in a stabs entry for parameters passed
   with the register attribute.

   GCC's default value, 'P', is used by dbx to refers to an external
   procedure. The section 5 manual page for dbx implies that 'R' would be the
   right letter, but dbx 1.5 has a bug in it that precludes its use.
   Probably that is why neither hc or pcc use this. pcc puts in two
   stabs entries: one for the parameter location and one for the register
   location. The letter `r' (register)
   would be okay, but it loses parameter attribute of the stabs entry.  */
#define DBX_REGPARM_STABS_LETTER 'R'

/* A C expression for the integer offset value of an automatic variable
   (N_LSYM) having address X (an RTX). This gets used in .stabs entries
   for the local variables. Compare with the default definition.  */
#define DEBUGGER_AUTO_OFFSET(X)                        \
  (GET_CODE (X) == PLUS                                \
   ? romp_debugger_auto_correction (INTVAL (XEXP (X, 1)) ) \
   : 0 )

/* A C expression for the integer offset value of an argument (N_PSYM)
   having address X (an RTX).  The nominal offset is OFFSET.  */
#define DEBUGGER_ARG_OFFSET(OFFSET, X)             \
  romp_debugger_arg_correction (OFFSET);

/* We don't have GAS for the RT yet, so don't write out special
   .stabs in cc1plus.  */
   
#define FASCIST_ASSEMBLER

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Don't try to use the `x' type-cross-reference character in DBX data.
   Also has the consequence of putting each struct, union or enum
   into a separate .stabs, containing only cross-refs to the others.  */
#define DBX_NO_XREFS

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* Mode of a function address in a call instruction (for indexing purposes).

   Doesn't matter on ROMP.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Define this if shift instructions ignore all but the low-order
   few bits.

   This is not true on the RT since it uses the low-order 6, not 5, bits.
   At some point, this should be extended to see how to express that.  */

/* #define SHIFT_COUNT_TRUNCATED */

/* Compute the cost of computing a constant rtl expression RTX whose
   rtx-code is CODE, contained within an expression of code OUTER_CODE.
   The body of this macro is a portion of a switch statement.  If the
   code is computed here, return it with a return statement.  Otherwise,
   break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (((OUTER_CODE) == IOR && exact_log2 (INTVAL (RTX)) >= 0)	\
	|| ((OUTER_CODE) == AND && exact_log2 (~INTVAL (RTX)) >= 0) \
	|| (((OUTER_CODE) == PLUS || (OUTER_CODE) == MINUS)	\
	    && (unsigned int) (INTVAL (RTX) + 15) < 31)		\
	|| ((OUTER_CODE) == SET && (unsigned int) INTVAL (RTX) < 16))\
      return 0;							\
    return ((unsigned int) (INTVAL(RTX) + 0x8000) < 0x10000		\
	    || (INTVAL (RTX) & 0xffff0000) == 0) ? 0 : COSTS_N_INSNS (2);\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    if (current_function_operand (RTX, Pmode)) return 0;	\
    return COSTS_N_INSNS (2);					\
  case CONST_DOUBLE:						\
    if ((RTX) == CONST0_RTX (GET_MODE (RTX))) return 2;		\
    return ((GET_MODE_CLASS (GET_MODE (RTX)) == MODE_FLOAT)	\
	    ? COSTS_N_INSNS (5) : COSTS_N_INSNS (4));

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. 

   References to our own data area are really references to r14, so they
   are very cheap.  Multiples and divides are very expensive.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case MEM:						\
    return current_function_operand (X, Pmode) ? 0 : COSTS_N_INSNS (2);	\
  case MULT:						\
    return (TARGET_IN_LINE_MUL && GET_MODE_CLASS (GET_MODE (X)) == MODE_INT)\
	   ? COSTS_N_INSNS (19) : COSTS_N_INSNS (25);	\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (45);

/* Compute the cost of an address.  This is meant to approximate the size
   and/or execution delay of an insn using that address.  If the cost is
   approximated by the RTL complexity, including CONST_COSTS above, as
   is usually the case for CISC machines, this macro should not be defined.
   For aggressively RISCy machines, only one insn format is allowed, so
   this macro should be a constant.  The value of this macro only matters
   for valid addresses.

   For the ROMP, everything is cost 0 except for addresses involving
   symbolic constants, which are cost 1.  */

#define ADDRESS_COST(RTX)				\
  ((GET_CODE (RTX) == SYMBOL_REF			\
    && ! CONSTANT_POOL_ADDRESS_P (RTX))			\
   || GET_CODE (RTX) == LABEL_REF			\
   || (GET_CODE (RTX) == CONST				\
       && ! constant_pool_address_operand (RTX, Pmode))	\
   || (GET_CODE (RTX) == PLUS				\
       && ((GET_CODE (XEXP (RTX, 1)) == SYMBOL_REF	\
	    && ! CONSTANT_POOL_ADDRESS_P (XEXP (RTX, 0))) \
	   || GET_CODE (XEXP (RTX, 1)) == LABEL_REF	\
	   || GET_CODE (XEXP (RTX, 1)) == CONST)))

/* Adjust the length of an INSN.  LENGTH is the currently-computed length and
   should be adjusted to reflect any required changes.  This macro is used when
   there is some systematic length adjustment required that would be difficult
   to express in the length attribute.

   On the ROMP, there are two adjustments:  First, a 2-byte insn in the delay
   slot of a CALL (including floating-point operations) actually takes four
   bytes.  Second, we have to make the worst-case alignment assumption for
   address vectors.  */

#define ADJUST_INSN_LENGTH(X,LENGTH)					\
  if (GET_CODE (X) == INSN && GET_CODE (PATTERN (X)) == SEQUENCE	\
      && GET_CODE (XVECEXP (PATTERN (X), 0, 0)) != JUMP_INSN		\
      && get_attr_length (XVECEXP (PATTERN (X), 0, 1)) == 2)		\
    (LENGTH) += 2;							\
  else if (GET_CODE (X) == JUMP_INSN && GET_CODE (PATTERN (X)) == ADDR_VEC) \
    (LENGTH) += 2;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Set if condition code (really not-Z) is stored in `test bit'.  */
#define CC_IN_TB	 01000

/* Set if condition code is set by an unsigned compare.  */
#define	CC_UNSIGNED        02000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(BODY,INSN) \
  update_cc (BODY, INSN)

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)				\
{ const char *p;					\
							\
  fprintf (FILE, "\t.globl .oVncs\n\t.set .oVncs,0\n") ; \
  fprintf (FILE, "\t.globl .oVgcc");			\
  for (p = version_string; *p != ' ' && *p != 0; p++)	\
    fprintf (FILE, "%c", *p);				\
  fprintf (FILE, "\n\t.set .oVgcc");			\
  for (p = version_string; *p != ' ' && *p != 0; p++)	\
    fprintf (FILE, "%c", *p);				\
  fprintf (FILE, ",0\n");				\
}

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before instructions and read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9",	\
 "r10", "r11", "r12", "r13", "r14", "r15", "ap",		\
 "fr0", "fr1", "fr2", "fr3", "fr4", "fr5", "fr6", "fr7" }

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to output a label for a jump table.  Arguments are the same as
   for ASM_OUTPUT_INTERNAL_LABEL, except the insn for the jump table is
   passed.  */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 2); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output code to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsis r1,4\n\tsts %s,0(r1)\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tls r1,0(r1)\n\tais r1,4\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   Don't define this if it is not supported.  */

/* #define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) */

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE,ROUNDED)	\
( fputs (".lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand (FILE, X, CODE)

/* Define which CODE values are valid.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)	\
  ((CODE) == '.' || (CODE) == '#')

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)			\
{ register rtx addr = ADDR;					\
  register rtx base = 0, offset = addr;				\
  if (GET_CODE (addr) == REG)					\
    base = addr, offset = const0_rtx;				\
  else if (GET_CODE (addr) == PLUS				\
	   && GET_CODE (XEXP (addr, 0)) == REG)			\
    base = XEXP (addr, 0), offset = XEXP (addr, 1);		\
  else if (GET_CODE (addr) == SYMBOL_REF			\
	   && CONSTANT_POOL_ADDRESS_P (addr))			\
    {								\
      offset = GEN_INT (get_pool_offset (addr) + 12);  		\
      base = gen_rtx_REG (SImode, 14);				\
    }								\
  else if (GET_CODE (addr) == CONST				\
	   && GET_CODE (XEXP (addr, 0)) == PLUS			\
	   && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT	\
	   && GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF	\
	   && CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (addr, 0), 0))) \
    {								\
      offset = plus_constant (XEXP (XEXP (addr, 0), 1),		\
			      (get_pool_offset (XEXP (XEXP (addr, 0), 0)) \
			       + 12));				\
      base = gen_rtx_REG (SImode, 14);				\
    }								\
  output_addr_const (FILE, offset);				\
  if (base)							\
    fprintf (FILE, "(%s)", reg_names [REGNO (base)]);		\
}

/* Define the codes that are matched by predicates in aux-output.c.  */

#define PREDICATE_CODES \
  {"zero_memory_operand", {SUBREG, MEM}},			\
  {"short_memory_operand", {SUBREG, MEM}},			\
  {"symbolic_memory_operand", {SUBREG, MEM}},			\
  {"current_function_operand", {MEM}},				\
  {"constant_pool_address_operand", {SUBREG, CONST}},		\
  {"romp_symbolic_operand", {LABEL_REF, SYMBOL_REF, CONST}},	\
  {"constant_operand", {LABEL_REF, SYMBOL_REF, PLUS, CONST, CONST_INT}}, \
  {"reg_or_cint_operand", {SUBREG, REG, CONST_INT}},		\
  {"reg_or_any_cint_operand", {SUBREG, REG, CONST_INT}},	\
  {"short_cint_operand", {CONST_INT}},				\
  {"reg_or_D_operand", {SUBREG, REG, CONST_INT}},		\
  {"reg_or_add_operand", {SUBREG, REG, LABEL_REF, SYMBOL_REF,	\
			  PLUS, CONST, CONST_INT}}, 		\
  {"reg_or_and_operand", {SUBREG, REG, CONST_INT}},		\
  {"reg_or_mem_operand", {SUBREG, REG, MEM}},			\
  {"reg_or_nonsymb_mem_operand", {SUBREG, REG, MEM}},		\
  {"romp_operand", {SUBREG, MEM, REG, CONST_INT, CONST, LABEL_REF, \
		    SYMBOL_REF, CONST_DOUBLE}},			\
  {"reg_0_operand", {REG}},					\
  {"reg_15_operand", {REG}},					\
  {"float_binary", {PLUS, MINUS, MULT, DIV}},			\
  {"float_unary", {NEG, ABS}},					\
  {"float_conversion", {FLOAT_TRUNCATE, FLOAT_EXTEND, FLOAT, FIX}},

