/* Definitions of target machine for GNU compiler.  AT&T we32000 version.
   Copyright (C) 1991, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.
   Contributed by John Wehle (john@feith1.uucp)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
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

#define CPP_PREDEFINES "-Dwe32000 -Du3b2 -Dunix -Asystem(unix) -Acpu(we32000) -Amachine(we32000)"

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (we32000)");

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 0


/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields. */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the we32000.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword is lowest numbered.  */
/* For we32000 we can decide arbitrarily
   since there are no machine instructions for them.  */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a we32000, this would still be 32.
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
#define STACK_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define number of bits in most basic integer type.
   (If undefined, default is BITS_PER_WORD).  */
#define INT_TYPE_SIZE 32

/* Integer bit fields should have the same size and alignment
   as actual integers */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Specify the size_t type.  */
#define SIZE_TYPE "unsigned int"

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers. */
#define FIRST_PSEUDO_REGISTER 16

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator. */
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, \
  0, 1, 1, 1, 1, 1, 1, 1, }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
 {1, 1, 1, 0, 0, 0, 0, 0, \
  0, 1, 1, 1, 1, 1, 1, 1, }

/* Make sure everything's fine if we *don't* have a given processor.
   This assumes that putting a register in fixed_regs will keep the
   compilers mitt's completely off it.  We don't bother to zero it out
   of register classes.  */
/* #define CONDITIONAL_REGISTER_USAGE */

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers. */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register used for the program counter */
#define PC_REGNUM  15

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 12

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 9

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 10

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 8

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 2

/* Order in which to allocate registers. */
#define REG_ALLOC_ORDER  \
 {0, 1, 8, 7, 6, 5, 4, 3}

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

enum reg_class { NO_REGS, GENERAL_REGS,
  ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 { "NO_REGS", "GENERAL_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{							\
 0,			/* NO_REGS */		\
 0x000017ff,		/* GENERAL_REGS */	\
 0x0000ffff,		/* ALL_REGS */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)	\
  (((REGNO) < 11 || (REGNO) == 12) ? GENERAL_REGS : ALL_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.
   We do a trick here to modify the effective constraints on the
   machine description; we zorch the constraint letters that aren't
   appropriate for a specific target.  This allows us to guarantee
   that a specific kind of register will not be used for a given target
   without fiddling with the register classes above. */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'r' ? GENERAL_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C. */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  0

/*
 */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  0

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class. */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by. */
#define PUSH_ROUNDING(BYTES) (((BYTES) + 3) & ~3)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name. */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) (SIZE)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the we32000 the return value is in r0 regardless.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the we32000 the return value is in r0 regardless.  */

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 0)

/* 1 if N is a possible register number for a function value.
   On the we32000, r0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

/* #define PCC_STATIC_STRUCT_RETURN */

/* 1 if N is a possible register number for function argument passing.
   On the we32000, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the we32k, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the we32k, the offset starts at 0.  */

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

/* On the we32000 all args are pushed */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int nregs_to_save;					\
  register int regno;						\
  extern char call_used_regs[];					\
  nregs_to_save = 0;						\
  for (regno = 8; regno > 2; regno--)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      nregs_to_save = (9 - regno);				\
  fprintf (FILE, "\tsave &%d\n", nregs_to_save);  		\
  if (SIZE)							\
    fprintf (FILE, "\taddw2 &%d,%%sp\n", ((SIZE) + 3) & ~3);	}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tmovw &.LP%d,%%r0\n\tjsb _mcount\n", (LABELNO))

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tcmpw .LPBX0,&0\n\tjne .LPI%d\n\tpushw &.LPBX0\n\tcall &1,__bb_init_func\n.LPI%d:\n",  \
	   LABELNO, LABELNO);

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)	\
  fprintf (FILE, "\taddw2 &1,.LPBX2+%d\n", 4 * BLOCKNO)

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
{ register int nregs_to_restore;				\
  register int regno;						\
  extern char call_used_regs[];					\
  nregs_to_restore = 0;						\
  for (regno = 8; regno > 2; regno--)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       nregs_to_restore = (9 - regno);				\
  fprintf (FILE, "\tret &%d\n", nregs_to_restore);  		}

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.

   On the we32k, FRAME_POINTER_REQUIRED is always 1, so the definition of this
   macro doesn't matter.  But it must be defined.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0;

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the we32k, the trampoline contains two instructions:
     mov #STATIC,%r8
     jmp #FUNCTION */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  ASM_OUTPUT_SHORT (FILE, GEN_INT (0x844f));				\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_CHAR (FILE, GEN_INT (0x48));				\
  ASM_OUTPUT_SHORT (FILE, GEN_INT (0x247f));				\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 13

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 2)), CXT); \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 9)), FNADDR); \
}

/* Generate calls to memcpy() and memset() rather
   than bcopy() and bzero() */
#define TARGET_MEM_FUNCTIONS

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

#define REGNO_OK_FOR_INDEX_P(REGNO)	0

#define REGNO_OK_FOR_BASE_P(REGNO)	\
  ((REGNO) < 11 || (REGNO) == 12 ||	\
  (unsigned)reg_renumber[REGNO] < 11 || (unsigned)reg_renumber[REGNO] == 12)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  */

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

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) 0

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
  (REGNO(X) < 11 || REGNO(X) == 12 || REGNO(X) >= FIRST_PSEUDO_REGISTER)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address. */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
{ register rtx Addr = X;						\
  if ((MODE) == QImode || (MODE) == HImode ||				\
    (MODE) == PSImode || (MODE) == SImode || (MODE) == SFmode)		\
    if (GET_CODE(Addr) == MEM)						\
      Addr = XEXP(Addr, 0);						\
  if (CONSTANT_ADDRESS_P(Addr))						\
    goto LABEL;								\
  if (REG_P(Addr) && REG_OK_FOR_BASE_P(Addr))				\
    goto LABEL;								\
  if (GET_CODE(Addr) == PLUS &&						\
    ((REG_P(XEXP(Addr, 0)) && REG_OK_FOR_BASE_P(XEXP(Addr, 0)) &&	\
     CONSTANT_ADDRESS_P(XEXP(Addr, 1))) ||				\
     (REG_P(XEXP(Addr, 1)) && REG_OK_FOR_BASE_P(XEXP(Addr, 1)) &&	\
     CONSTANT_ADDRESS_P(XEXP(Addr, 0)))))				\
    goto LABEL;								\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output. */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)   { }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for. */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	{ }

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE -1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE, OUTER_CODE)				\
  case CONST_INT:							\
    if (INTVAL (RTX) >= -16 && INTVAL (RTX) <= 63) return 0;		\
    if (INTVAL (RTX) >= -128 && INTVAL (RTX) <= 127) return 1;		\
    if (INTVAL (RTX) >= -32768 && INTVAL (RTX) <= 32767) return 2;	\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 3;								\
  case CONST_DOUBLE:							\
    return 5;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{								\
  { CC_STATUS_INIT; }						\
}

/* Control the assembler format that we output.  */

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"

#define ENDFILE_SPEC "crtn.o%s"

/* The .file command should always begin the output.  */

#define ASM_FILE_START(FILE) output_file_directive ((FILE), main_input_filename)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* Output before code.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Read-only data goes in the data section because
   AT&T's assembler doesn't guarantee the proper alignment
   of data in the text section even if an align statement
   is used. */

#define READONLY_DATA_SECTION() data_section()

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",	\
 "r8", "fp", "ap", "psw", "sp", "pcbp", "isp", "pc"	}

/* How to renumber registers for dbx and gdb. */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Output SDB debugging info in response to the -g option.  */

#define SDB_DEBUGGING_INFO

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do {					\
    fputs (".globl ", FILE);		\
    assemble_name (FILE, NAME);		\
    fputs ("\n", FILE);			\
  } while (0)

/* The prefix to add to user-visible assembler symbols. */

#define USER_LABEL_PREFIX ""

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, ".%s%d", PREFIX, NUM)

/* This is how to output an internal numbered label which
   labels a jump table.  */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLE)	\
  do {							\
    ASM_OUTPUT_ALIGN (FILE, 2);				\
    ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM);	\
  } while (0)

/* Assembler pseudo to introduce byte constants.  */

#define ASM_BYTE_OP "\t.byte"

/* This is how to output an assembler line defining a `double' constant.  */

/* This is how to output an assembler line defining a `float' constant.  */

/* AT&T's assembler can't handle floating constants written as floating.
   However, when cross-compiling, always use that in case format differs.  */

#ifdef CROSS_COMPILER

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)	\
  fprintf (FILE, "\t.double 0r%.20g\n", (VALUE))

#define ASM_OUTPUT_FLOAT(FILE,VALUE)	\
  fprintf (FILE, "\t.float 0r%.10g\n", (VALUE))

#else

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)	\
do { union { double d; long l[2];} tem;				\
     tem.d = (VALUE);						\
     fprintf (FILE, "\t.word 0x%x, 0x%x\n", tem.l[0], tem.l[1]);\
   } while (0)

#define ASM_OUTPUT_FLOAT(FILE,VALUE)	\
do { union { float f; long l;} tem;				\
     tem.f = (VALUE);						\
     fprintf (FILE, "\t.word 0x%x\n", tem.l);			\
   } while (0)

#endif /* not CROSS_COMPILER */

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.half "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)  \
do {							\
  unsigned char *s;					\
  int i;						\
  for (i = 0, s = (unsigned char *)(PTR); i < (LEN); s++, i++)	\
    {							\
      if ((i % 8) == 0)					\
	fprintf ((FILE),"%s\t.byte\t",(i?"\n":""));	\
      fprintf ((FILE), "%s0x%x", (i%8?",":""), (unsigned)*s); \
    }							\
  fputs ("\n", (FILE));					\
} while (0)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO) \
  fprintf (FILE, "\tpushw %s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)	\
  fprintf (FILE, "\tPOPW %s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute. */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.word .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", 1 << (LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

/* The `space' pseudo in the text segment outputs nop insns rather than 0s,
   so we must output 0s explicitly in the text segment.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  if (in_text_section ())                                           	    \
    {									    \
      int i;								    \
      for (i = 0; i < (SIZE) - 20; i += 20)				    \
	fprintf (FILE, "\t.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0\n"); \
      if (i < (SIZE))							    \
        {								    \
	  fprintf (FILE, "\t.byte 0");					    \
	  i++;								    \
	  for (; i < (SIZE); i++)					    \
	    fprintf (FILE, ",0");					    \
	  fprintf (FILE, "\n");						    \
	}								    \
    }									    \
  else									    \
    fprintf ((FILE), "\t.set .,.+%u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    data_section();					\
    fputs ("\t.comm ", (FILE));				\
    assemble_name ((FILE), (NAME));			\
    fprintf ((FILE), ",%u\n", (SIZE));			\
  } while (0)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do {							\
    data_section();					\
    ASM_OUTPUT_ALIGN ((FILE), 2);			\
    ASM_OUTPUT_LABEL ((FILE), (NAME));			\
    fprintf ((FILE), "\t.zero %u\n", (SIZE));		\
  } while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME)

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
   For `%' followed by punctuation, CODE is the punctuation and X is null. */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) 0

#define PRINT_OPERAND(FILE, X, CODE)  \
{ int i;								\
  if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%%%s", reg_names[REGNO (X)]);			\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
         {								\
         union { double d; long l[2]; } dtem;				\
         union { float f; long l; } ftem;				\
									\
         dtem.l[0] = CONST_DOUBLE_LOW (X);				\
         dtem.l[1] = CONST_DOUBLE_HIGH (X);				\
         ftem.f = dtem.d;						\
         fprintf(FILE, "&0x%lx", ftem.l);				\
         }								\
  else { putc ('&', FILE); output_addr_const (FILE, X); }}

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx Addr = ADDR;						\
  rtx offset;								\
  rtx reg;								\
  if (GET_CODE (Addr) == MEM) {						\
    putc ('*', FILE);							\
    Addr = XEXP (Addr, 0);						\
    if (GET_CODE (Addr) == REG)						\
      putc ('0', FILE);							\
    }									\
  switch (GET_CODE (Addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "(%%%s)", reg_names[REGNO (Addr)]);		\
      break;								\
									\
    case PLUS:								\
      offset = NULL;							\
      if (CONSTANT_ADDRESS_P (XEXP (Addr, 0)))				\
	{								\
	  offset = XEXP (Addr, 0);					\
	  Addr = XEXP (Addr, 1);					\
	}								\
      else if (CONSTANT_ADDRESS_P (XEXP (Addr, 1)))			\
	{								\
	  offset = XEXP (Addr, 1);					\
	  Addr = XEXP (Addr, 0);					\
	}								\
      else								\
        abort();							\
      if (REG_P (Addr))							\
        reg = Addr;							\
      else								\
        abort();							\
      output_addr_const(FILE, offset);					\
      fprintf(FILE, "(%%%s)", reg_names[REGNO(reg)]);			\
      break;								\
									\
    default:								\
      if ( !CONSTANT_ADDRESS_P(Addr))					\
	abort();							\
      output_addr_const (FILE, Addr);					\
    }}

/*
Local variables:
version-control: t
End:
*/
