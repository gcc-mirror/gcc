/* Definitions of target machine for GNU compiler. 
   Matsushita MN10300 series
   Copyright (C) 1996 Free Software Foundation, Inc.
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

#include "svr4.h"

#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#undef LIB_SPEC
#undef ENDFILE_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-D__mn10300__ -D__MN10300__"

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  {{ "", TARGET_DEFAULT}}

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (MN10300)");


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the Matsushita MN1003.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is not true on the Matsushita MN10300.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.
   This is not true on the Matsushita MN10300.  */
#define WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD		32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 		32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY		32

/* The stack goes in 32 bit lumps.  */
#define STACK_BOUNDARY 		32

/* Allocation boundary (in *bits*) for the code of a function.
   8 is the minimum boundary; it's unclear if bigger alignments
   would improve performance.  */
#define FUNCTION_BOUNDARY 8

/* No data type wants to be aligned rounder than this.   */
#define BIGGEST_ALIGNMENT	32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

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

#define FIRST_PSEUDO_REGISTER 9

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
  { 0, 0, 0, 0, 0, 0, 0, 0, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.  */

#define CALL_USED_REGISTERS \
  { 1, 1, 0, 0, 1, 1, 0, 0, 1}

#define REG_ALLOC_ORDER \
  { 0, 1, 4, 5, 2, 3, 6, 7, 8}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
 (REGNO_REG_CLASS (REGNO) == DATA_REGS 			\
  ? ((REGNO) & 1) == 0 || GET_MODE_SIZE (MODE) <= 4	\
  : ((REGNO) & 1) == 0 || GET_MODE_SIZE (MODE) == 4)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (MODE1 == MODE2 || GET_MODE_SIZE (MODE1) <= 4 && GET_MODE_SIZE (MODE2) <= 4)

/* 4 data, and effectively 3 address registers is small as far as I'm
   concerned.  */
#define SMALL_REGISTER_CLASSES 1

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
  NO_REGS, DATA_REGS, ADDRESS_REGS, SP_REGS, DATA_OR_ADDRESS_REGS, DATA_OR_SP_REGS, SP_OR_ADDRESS_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
{ "NO_REGS", "DATA_REGS", "ADDRESS_REGS", \
  "SP_REGS", "DATA_OR_ADDRESS_REGS", "DATA_OR_SP_REGS", \
  "SP_OR_ADDRESS_REGS",	"GENERAL_REGS", "ALL_REGS", "LIM_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS  			\
{      0,		/* No regs      */	\
   0x00f,		/* DATA_REGS */		\
   0x0f0,		/* ADDRESS_REGS */	\
   0x100,		/* SP_REGS */		\
   0x0ff,		/* DATA_OR_ADDRESS_REGS */\
   0x00f,		/* DATA_OR_SP_REGS */	\
   0x0f0,		/* SP_OR_ADDRESS_REGS */\
   0x0ff,		/* GENERAL_REGS */    	\
   0x0ff,		/* ALL_REGS 	*/	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
  ((REGNO) < 4 ? DATA_REGS : \
   (REGNO) < 8 ? ADDRESS_REGS : \
    (REGNO) == 8 ? SP_REGS: 0)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS DATA_REGS
#define BASE_REG_CLASS  SP_OR_ADDRESS_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'd' ? DATA_REGS : \
   (C) == 'a' ? ADDRESS_REGS : \
   (C) == 'x' ? SP_REGS : NO_REGS)

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
  (((regno) >= 0 && regno < 4)	\
   || (reg_renumber[regno] >= 0 && reg_renumber[regno] < 4))


/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS) \
  (X == stack_pointer_rtx && CLASS != SP_REGS ? ADDRESS_REGS : CLASS)

#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  ((MODE == QImode || MODE == HImode) ? DATA_REGS : CLASS)

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class(CLASS,MODE,IN)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define INT_8_BITS(VALUE) ((unsigned) (VALUE) + 0x80 < 0x100)
#define INT_16_BITS(VALUE) ((unsigned) (VALUE) + 0x8000 < 0x10000)

#define CONST_OK_FOR_I(VALUE) ((VALUE) == 0)
#define CONST_OK_FOR_J(VALUE) ((VALUE) == 1)
#define CONST_OK_FOR_K(VALUE) ((VALUE) == 2)
#define CONST_OK_FOR_L(VALUE) ((VALUE) == 4)

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'I' ? CONST_OK_FOR_I (VALUE) : \
   (C) == 'J' ? CONST_OK_FOR_J (VALUE) : \
   (C) == 'K' ? CONST_OK_FOR_K (VALUE) : \
   (C) == 'L' ? CONST_OK_FOR_L (VALUE) : 0)


/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself. 
     
  `G' is a floating-point zero.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  0


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

#define FIRST_PARM_OFFSET(FNDECL) (-4 + 20)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 8

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 7

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 7

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 5

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c. 

   Currently we always need a frame pointer.  In the future we'd like
   to be able to eliminate it.  */
#define FRAME_POINTER_REQUIRED 1

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 20

/* A guess for the MN10300.  */
#define PROMOTE_PROTOTYPES 1

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* On the mn10300, the caller is responsible for allocating and deallocating
   a stack slot for the "call" and "calls" instructions to save their return
   pointer.  We used to do this in the "call" and "call_value" expanders,
   but that generated poor code.

   Now we pretend that we have an outgoing register parameter space so that
   the generic function calling code will allocate the slot.  */
   
#define REG_PARM_STACK_SPACE(FNDECL) 4
#define OUTGOING_REG_PARM_STACK_SPACE

/* 1 if N is a possible register number for function argument passing.
   On the MN10300, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the MN10300, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the MN10300, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
 ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3	\
	    : (int_size_in_bytes (TYPE) + 3) & ~3))

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

/* On the MN10300 all args are pushed.  */   

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0


#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  ((TYPE) && int_size_in_bytes (TYPE) > 8)
 
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) \
  ((TYPE) && int_size_in_bytes (TYPE) > 8)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.   */
   
#define FUNCTION_VALUE(VALTYPE, FUNC) gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, 0)

/* 1 if N is a possible register number for a function value.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* Return values > 8 bytes in length in memory.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
#define RETURN_IN_MEMORY(TYPE)  \
  (int_size_in_bytes (TYPE) > 8 || TYPE_MODE (TYPE) == BLKmode)

/* Register in which address to store a structure value
   is passed to a function.  On the MN10300 it's passed as
   the first parameter.  */

#define STRUCT_VALUE 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) ;

#define TRAMPOLINE_TEMPLATE(FILE)			\
  do {							\
    fprintf (FILE, "\tadd -4,sp\n");			\
    fprintf (FILE, "\t.long 0x0004fffa\n");			\
    fprintf (FILE, "\tadd 4,sp\n");			\
    fprintf (FILE, "\tmov mdr,d0\n");		\
    fprintf (FILE, "\tmov d0,a0\n");		\
    fprintf (FILE, "\tmov (15,a0),a1\n");		\
    fprintf (FILE, "\tmov (19,a0),a0\n");		\
    fprintf (FILE, "\tjmp (a0)\n");			\
    fprintf (FILE, "\t.long 0\n");			\
    fprintf (FILE, "\t.long 0\n");			\
  } while (0)

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 0x1d

#define TRAMPOLINE_ALIGNMENT 32

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 0x16)),	\
 		 (CXT));						\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 0x1a)),	\
		 (FNADDR));						\
}

/* Addressing modes, and classification of registers for them.  */


/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   CONSTANT_P (X)

/* Extra constraints.  */
 
#define EXTRA_CONSTRAINT(OP, C) \
 ((C) == 'S' ? GET_CODE (OP) == SYMBOL_REF : 0)

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
  ((REGNO (X) >= 0 && REGNO(X) <= 3) || REGNO (X) >= FIRST_PSEUDO_REGISTER)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  ((REGNO (X) >= 4 && REGNO(X) <= 8) || REGNO (X) >= FIRST_PSEUDO_REGISTER)
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

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually
   machine-independent.  */

/* Accept either REG or SUBREG where a register is valid.  */
  
#define RTX_OK_FOR_BASE_P(X)					\
  ((REG_P (X) && REG_OK_FOR_BASE_P (X))				\
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))		\
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)    	\
{							\
  if (CONSTANT_ADDRESS_P (X))				\
    goto ADDR;						\
  if (RTX_OK_FOR_BASE_P (X))				\
    goto ADDR;						\
  if (GET_CODE (X) == PLUS)				\
    {							\
      rtx base = 0, index = 0;				\
      if (REG_P (XEXP (X, 0))				\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
	base = XEXP (X, 0), index = XEXP (X, 1);	\
      if (REG_P (XEXP (X, 1))				\
	  && REG_OK_FOR_BASE_P (XEXP (X, 1)))		\
	base = XEXP (X, 1), index = XEXP (X, 0);	\
      if (base != 0 && index != 0)			\
	{						\
	  if (GET_CODE (index) == CONST_INT)		\
	    goto ADDR;					\
	  if (GET_CODE (index) == REG			\
	      && REG_OK_FOR_INDEX_P (index))		\
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

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT) \


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the vax.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define CC_OVERFLOW_UNUSABLE 0x200
#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP, INSN)

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  default: { int _zxy= const_costs(RTX, CODE);	\
	     if(_zxy) return _zxy; break;}

#define REGISTER_MOVE_COST(CLASS1, CLASS2)  3

/* A crude cut at RTX_COSTS for the MN10300.  */

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. 

   There aren't DImode MOD, DIV or MULT operations, so call them
   very expensive.  Everything else is pretty much a costant cost.  */

#define RTX_COSTS(RTX,CODE,OUTER_CODE) \
  case MOD:		\
  case DIV:		\
    return 60;		\
  case MULT:		\
    return 20;

/* Nonzero if access to memory by bytes or half words is no faster
   than accessing full words.  */
#define SLOW_BYTE_ACCESS 1

/* According expr.c, a value of around 6 should minimize code size, and
   for the MN10300 series, that's our primary concern.  */
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

/* This is how to output an assembler line defining a `double' constant.
   It is .dfloat or .gfloat, depending.  */

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf (FILE, "\t.double %s\n", dstr);		\
   } while (0)


/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE)			\
do { char dstr[30];					\
     REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);	\
     fprintf (FILE, "\t.float %s\n", dstr);		\
   } while (0)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE, VALUE)		\
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE, VALUE)		\
( fprintf (FILE, "\t.hword "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE, VALUE)		\
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */
#define ASM_OUTPUT_BYTE(FILE, VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* This says how to output the assembler to define a global
   uninitialized but not common symbol.
   Try to use asm_output_bss to implement this macro.  */

#define ASM_OUTPUT_BSS(FILE, DECL, NAME, SIZE, ROUNDED) \
  asm_output_bss ((FILE), (DECL), (NAME), (SIZE), (ROUNDED))

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
  char* real_name;                                \
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
{ "d0", "d1", "d2", "d3", "a0", "a1", "a2", "a3", "sp" }

/* Print an instruction operand X on file FILE.
   look in mn10300.c for details */

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

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t%s .L%d-.L%d\n", ".long", VALUE, REL)

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* We don't have to worry about dbx compatability for the mn10300.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Use stabs debugging info by default.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define DBX_REGISTER_NUMBER(REGNO) REGNO

/* Define to use software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
#define REAL_ARITHMETIC

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define this if the case instruction drops through after the table
   when the index is out of range.  Don't define it if the case insn
   jumps to the default label instead.  */
#define CASE_DROPS_THROUGH

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX	4

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* The assembler op to get a word.  */

#define FILE_ASM_OP "\t.file\n"

extern void asm_file_start ();
extern int const_costs ();
extern void print_operand ();
extern void print_operand_address ();
extern void expand_prologue ();
extern void expand_epilogue ();
extern void notice_update_cc ();
extern int call_address_operand ();
extern enum reg_class secondary_reload_class ();
