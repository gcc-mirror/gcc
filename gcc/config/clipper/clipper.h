/* Definitions of target machine for GNU compiler.  Clipper version.
   Copyright (C) 1987, 1988, 1991, 1993, 1994, 1995, 1996, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Contributed by Holger Teutsch (holger@hotbso.rhein-main.de)

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

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (clipper)");

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES		\
  { { "c400", 1, "Generate code for the C400" },		\
    { "c300", -1, "Generate code for the C300" },		\
    { "", TARGET_DEFAULT, NULL} }

#define TARGET_C400 1
#define TARGET_C300 0

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT TARGET_C300
#endif

/* Show that we can debug generated code without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields. */

#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */

#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */

#define WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit */
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

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 128

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* No structure field wants to be aligned rounder than this.  */
#define BIGGEST_FIELD_ALIGNMENT 64

/*  Make strcpy of constants fast. */
#define CONSTANT_ALIGNMENT(CODE, TYPEALIGN) \
  ((TYPEALIGN) < 32 ? 32 : (TYPEALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Let's keep the stack somewhat aligned.  */
#define STACK_BOUNDARY 64

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   For Clipper, we always store objects in a full register. */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)	\
    {						\
      (UNSIGNEDP) = 0;				\
      (MODE) = SImode;				\
    }


/* Define this if function arguments should also be promoted using the above
   procedure.  */

/* FIXME: do we loose compatibility to acc if we define this? */

/* #define PROMOTE_FUNCTION_ARGS */

/* Likewise, if the function return value is promoted.  */

/* #define PROMOTE_FUNCTION_RETURN */


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the clipper, these are the FP and SP .  */
#define FIXED_REGISTERS \
{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,\
 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1} /* Default: C300 */

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
{1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,\
 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1} /* default: C300 */

/* Zero or more C statements that may conditionally modify two
   variables `fixed_regs' and `call_used_regs' (both of type `char
   []') after they have been initialized from the two preceding
   macros. A C400 has additional floating registers f8 -> f15 */

#define CONDITIONAL_REGISTER_USAGE	\
   if (target_flags & TARGET_C400)	\
     { int i;				\
       for (i = 24; i < 32; i++) fixed_regs[i] = call_used_regs[i] = 0; }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   On the clipper, fp registers are 64 bits.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) >= 16 ? 1 \
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the clipper 0-15 may hold any mode but DImode and DFmode must be even.
   Registers 16-31 hold SFmode and DFmode */

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  ((REGNO) < 16 							\
   ? (((MODE) != DImode && (MODE) != DFmode) || ((REGNO) & 1) == 0)	\
   : ((MODE) == SFmode || (MODE) == DFmode))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  ((MODE1) == (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* clipper has extra PC  */
/* #define PC_REGNUM */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 14

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED \
   (! leaf_function_p ())

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 2

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 0

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
   
/* The clipper has general and FP regs.  */

enum reg_class { NO_REGS, GENERAL_REGS, FLOAT_REGS, ALL_REGS, LIM_REG_CLASSES};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "GENERAL_REGS", "FLOAT_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS { {0}, {0x0000ffff}, {0xffff0000}, {0xffffffff} }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ((REGNO) >= 16 ? FLOAT_REGS : GENERAL_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'r' ? GENERAL_REGS : ((C) == 'f' ? FLOAT_REGS: NO_REGS))

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C. */

#define CONST_OK_FOR_LETTER_P(VALUE, C) 0

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself. */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

/* Optional extra constraints for this machine. */

/* #define EXTRA_CONSTRAINT(OP, C) */


/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FLOAT_REGS			\
  ? 1					\
  : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if longjmp restores from saved registers
   rather than from what setjmp saved.  */
/* #define LONGJMP_RESTORE_FROM_STACK */

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

/* Given an rtx for the address of a frame,
   return an rtx for the address of the word in the frame
   that holds the dynamic chain--the previous frame's address.  */
#define DYNAMIC_CHAIN_ADDRESS(frame) (frame)

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by. */

/* #define PUSH_ROUNDING(BYTES) (BYTES) */

/* Keep the stack pointer constant throughout the function. */
/* we can't set this for clipper as library calls may have 3 args and we pass
   only 2 args in regs. */

/* #define ACCUMULATE_OUTGOING_ARGS */
  

/* Offset of first parameter from the argument pointer register value.
   size of PC + FP  */

#define FIRST_PARM_OFFSET(FNDECL) 8

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack. */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), ((TYPE_MODE (VALTYPE) == SFmode ||\
				      TYPE_MODE (VALTYPE) == DFmode) ? \
				     16 : 0))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  \
  gen_rtx_REG ((MODE), ((MODE) == SFmode || (MODE) == DFmode ? 16 : 0))


/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N) == 16)

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) \
  ((N) == 0 || (N) == 1 || (N) == 16 || (N) == 17)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values. Old Green Hills C-Clipper returns static
   structs but the newer Apogee compiler passes structs as hidden arg 0.
   Structs etc are always passed in memory */

/* #define PCC_STATIC_STRUCT_RETURN */


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   Clipper uses 2 register 'slots' that pass arguments in r0/r1 or f0/f1.
   An argument that must be passed in memory (struct... ) leaves that slot
   free.
   We pass 'long long' only in registers when both slots are free.
   Returned structs must be allocated by the caller, the address is passed
   in r0.

   struct ss {..}

   fun (i,j,k)		i in r0, j in r1, k on stack
   fun (s,j,k)		s on stack, j in r1, k on stack
   fun (i,s,k)		i in r0, s on stack, k on stack
   s1 = fun (i,s,k)	&s1 in r0, i in r1, s on stack, k on stack

   We must keep enough information for varargs/stdargs.

   _clipper_cum_args is a struct of 2 integers, with
	num =  slots used
	size = size of all stack args = offset to next arg without alignment

   If we use stdarg.h, size points to the first unnamed arg,
   see va-clipper.h */

struct _clipper_cum_args { int num; int size; };

#define CUMULATIVE_ARGS struct _clipper_cum_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   clipper passes the address of a struct in r0, set num = 1 in this case */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT) \
  ((CUM).num = ((FNTYPE) != 0 && aggregate_value_p (TREE_TYPE (FNTYPE))), \
   (CUM).size = 0)

/* internal helper : size of an argument */

#define CLIPPER_ARG_SIZE(MODE, TYPE)				\
(((MODE) != BLKmode							\
  ? (GET_MODE_SIZE (MODE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD 	\
  : (int_size_in_bytes (TYPE) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)	\
 * UNITS_PER_WORD)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			      \
do									      \
{									      \
  int reg = 0;								      \
									      \
  if ((CUM).num < 2							      \
      && (GET_MODE_CLASS(MODE)==MODE_INT || GET_MODE_CLASS(MODE)==MODE_FLOAT) \
      && (GET_MODE_SIZE (MODE) <= 8)					      \
      && ((TYPE) == NULL || !AGGREGATE_TYPE_P(TYPE))			      \
      && ((MODE) != DImode || (CUM).num == 0))				      \
    {									      \
      reg = 1;								      \
      if ((MODE) == DImode)						      \
	(CUM).num = 1;							      \
    }									      \
									      \
  (CUM).num++;								      \
									      \
  if (! reg)								      \
    {									      \
      int align = FUNCTION_ARG_BOUNDARY (MODE, TYPE) / BITS_PER_UNIT;	      \
      (CUM).size += align - 1;						      \
      (CUM).size &= ~(align - 1);					      \
      (CUM).size += CLIPPER_ARG_SIZE (MODE, TYPE);			      \
    }									      \
} while (0)

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

   2 args may go into regs. These must be MODE_INT or MODE_FLOAT but only
   if they really fit into ONE register. The exception is a DImode arg
   that occupies both register slots. */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)				     \
  (((CUM).num < 2							     \
    && (GET_MODE_CLASS(MODE)==MODE_INT || GET_MODE_CLASS(MODE)==MODE_FLOAT)  \
    && (GET_MODE_SIZE (MODE) <= 8)					     \
    && ((TYPE) == NULL || !AGGREGATE_TYPE_P(TYPE))			     \
    && ((MODE) != DImode || (CUM).num == 0))				     \
   ? gen_rtx_REG ((MODE),						     \
		  GET_MODE_CLASS(MODE) == MODE_FLOAT			     \
		  ? (CUM).num+16 : (CUM).num)				     \
   : 0)

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  (((TYPE) ? TYPE_ALIGN (TYPE) : GET_MODE_SIZE (MODE)) <= PARM_BOUNDARY \
    ? PARM_BOUNDARY : 2 * PARM_BOUNDARY)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
   Clipper never passed args partially in regs/mem. */

/* #define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)  0 */

/* Generate necessary RTL for __builtin_saveregs().
   ARGLIST is the argument list; see expr.c.  */

#define EXPAND_BUILTIN_SAVEREGS() clipper_builtin_saveregs ()

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) output_function_prologue (FILE,SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  /* FIXME */

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  /* FIXME */

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)	/* FIXME */

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) output_function_epilogue(FILE,SIZE)

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved. */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) \
  DEPTH = clipper_frame_size (get_frame_size ())


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

#define TRAMPOLINE_TEMPLATE(FILE)					      \
{									      \
  fputs ("\t.word  0x459F,0x0004\t# call   sp,.+4\n", FILE);		      \
  fputs ("\tmovw   (sp),r3\n", FILE);					      \
  fputs ("\taddq   $4,sp\n", FILE);					      \
  fputs ("\tloadw  20(r3),r2\n", FILE);					      \
  fputs ("\tloadw  24(r3),r3\n", FILE);					      \
  fputs ("\tb      (r3)\n", FILE);					      \
  fputs ("\t.long  0,0\n", FILE);					      \
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 32

/* Alignment required for a trampoline.  128 is used to find the
   beginning of a line in the instruction cache and to allow for
   instruction cache lines of up to 128 bytes.  */

#define TRAMPOLINE_ALIGNMENT 128

/* Section in which to place the trampoline.  */

#define TRAMPOLINE_SECTION text_section

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 24)), CXT); \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 28)), FNADDR); \
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(regno)  \
((regno) < 16 || (unsigned)reg_renumber[regno] < 16)
#define REGNO_OK_FOR_BASE_P(regno) \
((regno) < 16 || (unsigned)reg_renumber[regno] < 16)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

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

  /* clipper doesn't have true indexing */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */

#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) < 16 || REGNO(X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */

#define REG_OK_FOR_BASE_P(X) \
  (REGNO (X) < 16 || REGNO(X) >= FIRST_PSEUDO_REGISTER)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) (REGNO(X) < 16)

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) (REGNO(X) < 16)

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

/* Non-zero if X is an address which can be indirected. */

#define INDIRECTABLE_CONSTANT_ADDRESS_P(X) 0

#define INDIRECTABLE_ADDRESS_P(X)  \
  (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */

#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)	\
{ if (CONSTANT_ADDRESS_P (X)) goto ADDR;	\
  if (INDIRECTABLE_ADDRESS_P (X)) goto ADDR; }

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) \
{ register rtx xfoo = (X);			\
  GO_IF_NONINDEXED_ADDRESS (xfoo, ADDR);	\
  if (GET_CODE (xfoo) == PLUS)			\
    { register rtx xfoo0, xfoo1;		\
      xfoo0 = XEXP (xfoo, 0);			\
      xfoo1 = XEXP (xfoo, 1);			\
    /* handle reg + reg -> [r1](r0) */		\
      if (INDIRECTABLE_ADDRESS_P (xfoo0) && INDIRECTABLE_ADDRESS_P (xfoo1)) \
	goto ADDR;							\
    /* Handle <symbol>(reg) -> xxx(r0) */				\
      if (INDIRECTABLE_ADDRESS_P (xfoo0) && CONSTANT_ADDRESS_P (xfoo1))	\
	goto ADDR;							\
      if (INDIRECTABLE_ADDRESS_P (xfoo1) && CONSTANT_ADDRESS_P (xfoo0))	\
	goto ADDR; }}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the clipper, nothing needs to be done.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for. */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL) {}


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Define this if the case instruction drops through after the table
   when the index is out of range.  Don't define it if the case insn
   jumps to the default label instead.  */
/* #define CASE_DROPS_THROUGH */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction pairs, we will do a movstr or libcall instead.

   Make this large on clipper, since the block move is very
   inefficient with small blocks, and the hard register needs of the
   block move require much reload work. */

#define MOVE_RATIO 20

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* This machine uses IEEE floats.  */

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* Check a `double' value for validity for a particular machine mode.
   This is defined to avoid crashes outputting certain constants.
   Since we output the number in hex, the assembler won't choke on it.  */
/* #define CHECK_FLOAT_VALUE(MODE,VALUE) */


/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

/* On a Clipper, constants from 0..15 are cheap because they can use the
   'quick' mode. */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (0 <= INTVAL (RTX) && INTVAL(RTX) <= 15 ) return 0;	\
      return 1;							\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    return 5;

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case MULT:						\
    return COSTS_N_INSNS (4);				\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (40);				\
  case ASHIFT:						\
  case LSHIFTRT:					\
  case ASHIFTRT:					\
    return COSTS_N_INSNS (2);				\
 case SIGN_EXTEND:					\
    return (GET_CODE (XEXP (X,0)) == REG ? COSTS_N_INSNS (3) : 4);

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch */

/* #define BRANCH_COST 0 */


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the clipper.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{									      \
  enum attr_cc cc = get_attr_cc (INSN);					      \
  rtx dest = SET_DEST (EXP);						      \
  switch (cc)								      \
    {									      \
    case CC_CHANGE0:							      \
      if (GET_CODE (EXP) == PARALLEL) abort();				      \
      if ((cc_status.value1 && rtx_equal_p (dest, cc_status.value1)) ||	      \
	  (cc_status.value2 && rtx_equal_p (dest, cc_status.value2)))	      \
	CC_STATUS_INIT;							      \
      break;								      \
									      \
    case CC_SET1:							      \
      if (GET_CODE (EXP) == PARALLEL) abort();				      \
      cc_status.flags = 0;						      \
      cc_status.value1 = dest;						      \
      cc_status.value2 = 0;						      \
      break;								      \
									      \
    case CC_SET2:							      \
      if (GET_CODE (EXP) == PARALLEL) abort();				      \
      cc_status.flags = 0;						      \
      cc_status.value1 = dest;						      \
      cc_status.value2 = SET_SRC (EXP);					      \
      break;								      \
									      \
    case CC_UNCHANGED:							      \
      break;								      \
									      \
    case CC_CLOBBER:							      \
      CC_STATUS_INIT;							      \
      break;								      \
									      \
    default:								      \
      abort ();								      \
    }									      \
}


/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE) fprintf (FILE, "#NO_APP\n");

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", \
 "r9", "r10", "r11", "r12", "r13", "fp", "sp", \
 "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", \
 "f9", "f10", "f11", "f12", "f13", "f14", "f15" }

/* How to renumber registers for dbx and gdb.
   Clipper needs no change in the numeration.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)


/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsubq   $8,sp\n\t%s  %s,(sp)\n",	\
	   (REGNO) < 16 ? "storw" : "stord", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\t%s  (sp),%s\n\t\addq  $8,sp\n",	\
	   (REGNO) < 16 ? "loadw" : "loadd", reg_names[REGNO])
/* This is how to output an element of a case-vector that is absolute */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %u\n", (SIZE))

/* This says how to output an assembler line
   to define a local common symbol.  */
/* ??? The use of .bss here seems odd.  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE,NAME,SIZE,ALIGN)	\
( data_section (),					\
  fputs ("\t.bss\t", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN)/BITS_PER_UNIT))

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

/* Print an instruction operand X on file FILE.
   CODE is the code from the %-spec that requested printing this operand;
   if `%z3' was used to print operand 3, then CODE is 'z'.

Clipper operand formatting codes:

 letter	   print
   C	reverse branch condition
*/

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == 'C')

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == 'C')							\
    fputs (rev_cond_name (X), FILE);					\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in output-clipper.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
 print_operand_address (FILE, ADDR)

/* Define the codes that are matched by predicates in clipper.c */

#define PREDICATE_CODES \
  {"int_reg_operand", {SUBREG, REG}},	\
  {"fp_reg_operand", {SUBREG, REG}},

/* Define the `__builtin_va_list' type for the ABI.  */
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = clipper_build_va_list ()

/* Implement `va_start' for varargs and stdarg.  */
#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  clipper_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  clipper_va_arg (valist, type)
