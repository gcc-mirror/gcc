/* Definitions of target machine for GNU compiler.
   Matsushita MN10200 series
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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


/* Get rid of svr4.h stuff we don't want/need.  */
#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#undef LIB_SPEC
#undef ENDFILE_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-D__mn10200__ -D__MN10200__ -D__LONG_MAX__=2147483647L -D__LONG_LONG_MAX__=2147483647L -D__INT_MAX__=32767"

/* Run-time compilation parameters selecting different hardware subsets.  */

/* We don't have any switched on the mn10200.  Though there are some things
   that might be worth a switch:

   -mspace to optimize even more for space.

   -mrelax to enable the relaxing linker.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  {{ "", TARGET_DEFAULT, 0}}

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (MN10200)");


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the Matsushita MN10300.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is not true on the Matsushita MN10200.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.
   This is not true on the Matsushita MN10200.  */
#define WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.

   This is a white lie.  Registers are really 24bits, but most operations
   only operate on 16 bits.   GCC chokes badly if we set this to a value
   that is not a power of two.  */
#define BITS_PER_WORD		16

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		2

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.

   This differs from Pmode because we need to allocate 32bits of space
   to hold the 24bit pointers on this machine.  */
#define POINTER_SIZE 		32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY		16

/* The stack goes in 16 bit lumps.  */
#define STACK_BOUNDARY 		16

/* Allocation boundary (in *bits*) for the code of a function.
   8 is the minimum boundary; it's unclear if bigger alignments
   would improve performance.  */
#define FUNCTION_BOUNDARY 8

/* No data type wants to be aligned rounder than this.   */
#define BIGGEST_ALIGNMENT	16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* Seems to be how the Matsushita compiler does things, and there's
   no real reason to be different.  */
#define STRUCTURE_SIZE_BOUNDARY 16
#undef PCC_BITFIELD_TYPE_MATTERS

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.

   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   XXX Long term we should probably expose the MDR register, we use
   it for division, multiplication, and some extension operations.  */

#define FIRST_PSEUDO_REGISTER 8

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
  { 0, 0, 0, 0, 0, 0, 0, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.  */

#define CALL_USED_REGISTERS \
  { 1, 1, 0, 0, 1, 0, 0, 1}

#define REG_ALLOC_ORDER \
  { 0, 1, 4, 2, 3, 5, 6, 7}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((MODE) == PSImode ? 1 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) \
			    / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.

   We allow any register to hold a PSImode value.  We allow any register
   to hold values <= 16 bits.  For values > 16 bits we require aligned
   register pairs.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
 ((MODE) == PSImode ? 1 : ((REGNO) & 1) == 0 || GET_MODE_SIZE (MODE) <= 2)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (MODE1 == MODE2 || (GET_MODE_SIZE (MODE1) <= 2 && GET_MODE_SIZE (MODE2) <= 2))

/* 4 data, and effectively 2 address registers is small as far as I'm
   concerned.  Especially since we use 2 data registers for argument
   passing and return values.

   We used to define CLASS_LIKELY_SPILLED_P as true for DATA_REGS too,
   but we've made improvements to the port which greatly reduce register
   pressure.  As a result we no longer need to define CLASS_LIKELY_SPILLED_P
   for DATA_REGS (and by not defining it we get significantly better code).  */
#define SMALL_REGISTER_CLASSES 1
#define CLASS_LIKELY_SPILLED_P(CLASS) (CLASS == ADDRESS_REGS)

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
   
enum reg_class {
  NO_REGS, DATA_REGS, ADDRESS_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
{ "NO_REGS", "DATA_REGS", "ADDRESS_REGS", \
  "GENERAL_REGS", "ALL_REGS", "LIM_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS  			\
{     {0},		/* No regs      */	\
   {0x0f},		/* DATA_REGS */		\
   {0xf0},		/* ADDRESS_REGS */	\
   {0xff},		/* GENERAL_REGS */    	\
   {0xff},		/* ALL_REGS 	*/	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
  ((REGNO) < 4 ? DATA_REGS : ADDRESS_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS DATA_REGS
#define BASE_REG_CLASS  ADDRESS_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'd' ? DATA_REGS : \
   (C) == 'a' ? ADDRESS_REGS : NO_REGS)

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
 
#define REGNO_OK_FOR_BASE_P(regno) \
  (((regno) > 3 && regno < FIRST_PSEUDO_REGISTER)	\
   || (reg_renumber[regno] > 3 && reg_renumber[regno] < FIRST_PSEUDO_REGISTER))

#define REGNO_OK_FOR_INDEX_P(regno) \
  (IN_RANGE ((regno), 0, 3) \
   || (reg_renumber[regno] >= 0 && reg_renumber[regno] < 4))


/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS) \
  ((GET_MODE (X) != PSImode && GET_MODE (X) != VOIDmode) ? DATA_REGS : CLASS)

/* We want to use DATA_REGS for anything that is not PSImode.  */
#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  ((MODE != PSImode && MODE != VOIDmode) ? DATA_REGS : CLASS)

/* We have/need secondary reloads on the mn10200.  Mostly to deal
   with problems using address registers.  */
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class(CLASS,MODE,IN, 1)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class(CLASS,MODE,IN, 0)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((MODE) == PSImode ? 1 : (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define INT_8_BITS(VALUE) ((unsigned) (VALUE) + 0x80 < 0x100)
#define INT_16_BITS(VALUE) ((unsigned) (VALUE) + 0x8000 < 0x10000)

#define CONST_OK_FOR_I(VALUE) ((VALUE) == 0)
#define CONST_OK_FOR_J(VALUE) ((VALUE) >= 1 && (VALUE) <= 3)
#define CONST_OK_FOR_K(VALUE) ((VALUE) >= 1 && (VALUE) <= 4)
#define CONST_OK_FOR_L(VALUE) ((VALUE) == 15)
#define CONST_OK_FOR_M(VALUE) ((VALUE) == 255)

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'I' ? CONST_OK_FOR_I (VALUE) : \
   (C) == 'J' ? CONST_OK_FOR_J (VALUE) : \
   (C) == 'K' ? CONST_OK_FOR_K (VALUE) : \
   (C) == 'L' ? CONST_OK_FOR_L (VALUE) : \
   (C) == 'M' ? CONST_OK_FOR_M (VALUE) : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself. 
     
  `G' is a floating-point zero.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'G' ? (GET_MODE_CLASS (GET_MODE (VALUE)) == MODE_FLOAT	\
		 && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))		\
   : 0)



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

/* Offset of first parameter from the argument pointer register value.  */
/* Is equal to the size of the saved fp + pc, even if an fp isn't
   saved since the value is used before we know.  */

#define FIRST_PARM_OFFSET(FNDECL) (current_function_needs_context ? 8 : 4)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 7

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 6

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 6

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 4

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.

   We allow frame pointers to be eliminated when not having one will
   not interfere with debugging.  */
#define ACCUMULATE_OUTGOING_ARGS 1
#define FRAME_POINTER_REQUIRED 0
#define CAN_DEBUG_WITHOUT_FP

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = total_frame_size()

/* Various type size information.

   The mn10200 has a limited number of small registers.  Sizes of basic
   data types are adjusted accordingly.  */
#define SHORT_TYPE_SIZE         16
#define INT_TYPE_SIZE           16
#define LONG_TYPE_SIZE          32
#define LONG_LONG_TYPE_SIZE     32
#define FLOAT_TYPE_SIZE         32
#define DOUBLE_TYPE_SIZE        32
#define LONG_DOUBLE_TYPE_SIZE   DOUBLE_TYPE_SIZE

/* Any size less than 64bits will work; but a smarter definition
   can make G++ code smaller and faster.  Most operations on the
   mn10200 occur on 16bit hunks, so the best size for a boolean
   is 16bits.  */
#define BOOL_TYPE_SIZE		16

/* The difference of two pointers must be at least 24bits since pointers
   are 24bits; however, no basic data type is 24bits, so we have to round
   up to a 32bits for the difference of pointers.  */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long unsigned int"

/* Note sizeof (WCHAR_TYPE) must be equal to the value of WCHAR_TYPE_SIZE!  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#define MAX_FIXED_MODE_SIZE     32

/* A guess for the MN10200.  */
#define PROMOTE_PROTOTYPES 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) ((N) <= 1)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */

#define CUMULATIVE_ARGS struct cum_arg
struct cum_arg { int nbytes; };

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the MN10200, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
 ((CUM).nbytes = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM).nbytes += ((MODE) != BLKmode			\
	? (MODE) == PSImode ? 2 :			\
	    (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD \
	: (int_size_in_bytes (TYPE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD))

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
    (otherwise it is an extra parameter matching an ellipsis).  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, NAMED)

/* Implement `va_arg'.  */
#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  mn10200_va_arg (valist, type)

/* For "large" items, we pass them by invisible reference, and the
   callee is responsible for copying the data item if it might be
   modified.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  ((TYPE) && int_size_in_bytes (TYPE) > 8)
 
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) \
  ((TYPE) && int_size_in_bytes (TYPE) > 8)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.   */
   
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx_REG (TYPE_MODE (VALTYPE), TYPE_MODE (VALTYPE) == PSImode ? 4 : 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, (MODE) == PSImode ? 4 : 0)

/* 1 if N is a possible register number for a function value.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N) == 4)

/* Return values > 8 bytes in length in memory.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
#define RETURN_IN_MEMORY(TYPE)  \
  (int_size_in_bytes (TYPE) > 8 || TYPE_MODE (TYPE) == BLKmode)

/* Register in which address to store a structure value
   is passed to a function.  On the MN10200 it's passed as
   the first parameter.  */

#define STRUCT_VALUE 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.

   ?!? Profiling is not currently supported.  */

#define FUNCTION_PROFILER(FILE, LABELNO) ;

/* Yes, we actually support trampolines on this machine, even though
   nobody is likely to ever use them.  */
#define TRAMPOLINE_TEMPLATE(FILE)			\
  do {							\
    fprintf (FILE, "\t.byte 0xfd\n");			\
    fprintf (FILE, "\t.byte 0x00\n");			\
    fprintf (FILE, "\t.byte 0x00\n");			\
    fprintf (FILE, "\tmov (a3),a0\n");			\
    fprintf (FILE, "\tadd -4,a3\n");			\
    fprintf (FILE, "\tmov a0,(0,a3)\n");		\
    fprintf (FILE, "\tmov (21,a0),a0\n");		\
    fprintf (FILE, "\tmov a0,(4,a3)\n");		\
    fprintf (FILE, "\tmov (0,a3),a0\n");		\
    fprintf (FILE, "\tmov (17,a0),a0\n");		\
    fprintf (FILE, "\tadd 4,a3\n");			\
    fprintf (FILE, "\trts\n");				\
    fprintf (FILE, "\t.long 0\n");			\
    fprintf (FILE, "\t.long 0\n");			\
  } while (0)

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 0x1c

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (PSImode, plus_constant ((TRAMP), 20)),	\
		  (CXT));						\
  emit_move_insn (gen_rtx_MEM (PSImode, plus_constant ((TRAMP), 24)),	\
		  (FNADDR));						\
}

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)   \
  ((COUNT == 0)                         \
   ? gen_rtx_MEM (Pmode, frame_pointer_rtx) \
   : (rtx) 0)


/* Addressing modes, and classification of registers for them.  */


/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   CONSTANT_P (X)

/* Extra constraints.  */
#define OK_FOR_R(OP) \
   (GET_CODE (OP) == MEM					\
    && GET_MODE (OP) == QImode					\
    && REG_P (XEXP (OP, 0)))
 
/* Q is used for sp + <something> in the {zero,sign}_extendpsisi2 patterns.  */
#define EXTRA_CONSTRAINT(OP, C) \
 ((C) == 'R' ? OK_FOR_R (OP) : \
  (C) == 'S' ? GET_CODE (OP) == SYMBOL_REF : \
  (C) == 'Q' ? GET_CODE (OP) == PLUS : 0)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

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
#define REG_OK_FOR_INDEX_P(X)  \
  (IN_RANGE (REGNO (X), 0, 3) || REGNO (X) >= FIRST_PSEUDO_REGISTER)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (((REGNO (X) >= 4 && REGNO(X) <= 8) || REGNO (X) >= FIRST_PSEUDO_REGISTER))
#else
/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) \
  REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) \
  REGNO_OK_FOR_BASE_P (REGNO (X))
#endif


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   We used to allow reg+reg addresses for QImode and HImode; however,
   they tended to cause the register allocator to run out of registers.
   Basically, an indexed load/store always keeps 2 data and one address
   register live, which is just too many for this machine.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

/* Accept either REG or SUBREG where a register is valid.  */
  
#define RTX_OK_FOR_BASE_P(X)					\
  ((REG_P (X) && REG_OK_FOR_BASE_P (X))				\
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))		\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)    	\
{							\
  if ((MODE != PSImode) && CONSTANT_ADDRESS_P (X))	\
    goto ADDR;						\
  if (RTX_OK_FOR_BASE_P (X))				\
    goto ADDR;						\
  if (GET_CODE (X) == PLUS)				\
    {							\
      rtx base = 0, index = 0;				\
      if (RTX_OK_FOR_BASE_P (XEXP (X, 0)))		\
	base = XEXP (X, 0), index = XEXP (X, 1);	\
      if (RTX_OK_FOR_BASE_P (XEXP (X, 1)))		\
	base = XEXP (X, 1), index = XEXP (X, 0);	\
      if (base != 0 && index != 0)			\
	{						\
	  if (GET_CODE (index) == CONST_INT)		\
	    goto ADDR;					\
	}						\
    }							\
}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.   */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)  {}

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the VAX.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define CC_OVERFLOW_UNUSABLE 0x200
#define CC_NO_CARRY CC_NO_OVERFLOW
#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP, INSN)

/* The mn10200 has a limited number of registers, so CSE of function
   addresses generally makes code worse due to register pressure.  */
#define NO_FUNCTION_CSE

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:							\
    /* Zeros are extremely cheap.  */					\
    if (INTVAL (RTX) == 0)						\
      return 0;								\
    /* If it fits in 8 bits, then it's still relatively cheap.  */	\
    if (INT_8_BITS (INTVAL (RTX)))					\
      return 1;								\
    /* This is the "base" cost, includes constants where either the	\
       upper or lower 16bits are all zeros.  */				\
    if (INT_16_BITS (INTVAL (RTX))					\
	|| (INTVAL (RTX) & 0xffff) == 0					\
	|| (INTVAL (RTX) & 0xffff0000) == 0)				\
      return 2;								\
    return 4;								\
  /* These are more costly than a CONST_INT, but we can relax them,	\
     so they're less costly than a CONST_DOUBLE.  */			\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 6;								\
  /* We don't optimize CONST_DOUBLEs well nor do we relax them well,	\
     so their cost is very high.  */					\
  case CONST_DOUBLE:							\
    return 8;

/* Make moves between different classes more expensive than moves
   within the same class.  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)  (CLASS1 != CLASS2 ? 4 : 2)

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. 

   ?!? This probably needs more work.  The definitions below were first
   taken from the H8 port, then tweaked slightly to improve code density
   on various sample codes.  */

#define RTX_COSTS(RTX,CODE,OUTER_CODE) \
  case MOD:						\
  case DIV:						\
    return 8;						\
  case MULT:						\
    return (GET_MODE (RTX) == SImode ? 20 : 8);

/* Nonzero if access to memory by bytes or half words is no faster
   than accessing full words.  */
#define SLOW_BYTE_ACCESS 1

/* According expr.c, a value of around 6 should minimize code size, and
   for the MN10200 series, code size our primary concern.  */
#define MOVE_RATIO 6

#define TEXT_SECTION_ASM_OP "\t.section .text"
#define DATA_SECTION_ASM_OP "\t.section .data"
#define BSS_SECTION_ASM_OP "\t.section .bss"

/* Output at beginning/end of assembler file.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE) asm_file_start(FILE)

#define ASM_COMMENT_START "#"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* This says how to output the assembler to define a global
   uninitialized but not common symbol.
   Try to use asm_output_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE, NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE, NAME)	\
  do { fputs ("\t.global ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME)	          \
  do {                                            \
  const char* real_name;                          \
  STRIP_NAME_ENCODING (real_name, (NAME));        \
  fprintf (FILE, "_%s", real_name);               \
  } while (0)           

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* This is how we tell the assembler that two symbols have the same value.  */

#define ASM_OUTPUT_DEF(FILE,NAME1,NAME2) \
  do { assemble_name(FILE, NAME1); 	 \
       fputs(" = ", FILE);		 \
       assemble_name(FILE, NAME2);	 \
       fputc('\n', FILE); } while (0)


/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{ "d0", "d1", "d2", "d3", "a0", "a1", "a2", "a3"}

/* Print an instruction operand X on file FILE.
   look in mn10200.c for details */

#define PRINT_OPERAND(FILE, X, CODE)  print_operand(FILE,X,CODE)

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in output-vax.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)
#define ASM_OUTPUT_REG_POP(FILE,REGNO)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  asm_fprintf (FILE, "\t%s .L%d\n", ".long", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t%s .L%d-.L%d\n", ".long", VALUE, REL)

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* We don't have to worry about dbx compatibility for the mn10200.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Use stabs debugging info by default.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* GDB always assumes the current function's frame begins at the value
   of the stack pointer upon entry to the current function.  Accessing
   local variables and parameters passed on the stack is done using the
   base of the frame + an offset provided by GCC.

   For functions which have frame pointers this method works fine;
   the (frame pointer) == (stack pointer at function entry) and GCC provides
   an offset relative to the frame pointer.

   This loses for functions without a frame pointer; GCC provides an offset
   which is relative to the stack pointer after adjusting for the function's
   frame size.  GDB would prefer the offset to be relative to the value of
   the stack pointer at the function's entry.  Yuk!  */
#define DEBUGGER_AUTO_OFFSET(X) \
  ((GET_CODE (X) == PLUS ? INTVAL (XEXP (X, 1)) : 0) \
    + (frame_pointer_needed ? 0 : -total_frame_size ()))

#define DEBUGGER_ARG_OFFSET(OFFSET, X) \
  ((GET_CODE (X) == PLUS ? OFFSET : 0) \
    + (frame_pointer_needed ? 0 : -total_frame_size ()))

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
#define REAL_ARITHMETIC

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Dispatch tables on the mn10200 are extremely expensive in terms of code
   and readonly data size.  So we crank up the case threshold value to
   encourage a series of if/else comparisons to implement many small switch
   statements.  In theory, this value could be increased much more if we
   were solely optimizing for space, but we keep it "reasonable" to avoid
   serious code efficiency lossage.  */
#define CASE_VALUES_THRESHOLD 8

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* We could define this either way.  Using ZERO_EXTEND for QImode makes slightly
   fast and more compact code.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX	2

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) (OUTPREC != 32)

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode PSImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Perform target dependent optabs initialization.  */
#define MODHI3_LIBCALL "__modhi3"
#define DIVHI3_LIBCALL "__divhi3"

#define INIT_TARGET_OPTABS \
  do { \
    sdiv_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (DIVHI3_LIBCALL);		\
    smod_optab->handlers[(int) HImode].libfunc		\
      = init_one_libfunc (MODHI3_LIBCALL);		\
  } while (0)

/* The assembler op to get a word.  */

#define FILE_ASM_OP "\t.file\n"

#define PREDICATE_CODES							\
  {"call_address_operand",	{ SYMBOL_REF, REG }},			\
  {"constant_memory_operand",	{ MEM }},				\
  {"psimode_truncation_operand",{ PLUS, CONST_INT, CONST_DOUBLE, CONST,	\
				  SYMBOL_REF, LABEL_REF, SUBREG, REG, MEM }},\
  {"extendpsi_operand",		{ PLUS, CONST_INT, CONST_DOUBLE, CONST,	\
				  SYMBOL_REF, LABEL_REF, SUBREG, REG, MEM }}, \
  {"nshift_operator",		{ ASHIFTRT, LSHIFTRT, ASHIFT }},

extern struct rtx_def *zero_dreg;
extern struct rtx_def *zero_areg;
