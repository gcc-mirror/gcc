/* Definitions of target machine for GNU compiler.  Vax version.
   Copyright (C) 1987, 1988, 1991, 1993, 1994 Free Software Foundation, Inc.

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


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dvax -D__vax__ -Dunix -Asystem(unix) -Asystem(bsd) -Acpu(vax) -Amachine(vax)"

/* If using g-format floating point, alter math.h.  */

#define	CPP_SPEC "%{mg:-DGFLOAT}"

/* Choose proper libraries depending on float format.
   Note that there are no profiling libraries for g-format.
   Also use -lg for the sake of dbx.  */

#define LIB_SPEC "%{g:-lg}\
 %{mg:%{lm:-lmg} -lcg \
  %{p:%eprofiling not supported with -mg\n}\
  %{pg:%eprofiling not supported with -mg\n}}\
 %{!mg:%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (vax)");

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Nonzero if compiling code that Unix assembler can assemble.  */
#define TARGET_UNIX_ASM (target_flags & 1)

/* Nonzero if compiling with VAX-11 "C" style structure alignment */
#define	TARGET_VAXC_ALIGNMENT (target_flags & 2)

/* Nonzero if compiling with `G'-format floating point */
#define TARGET_G_FLOAT (target_flags & 4)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"unix", 1},  \
    {"gnu", -1},  \
    {"vaxc-alignment", 2}, \
    {"g", 4}, \
    {"g-float", 4}, \
    {"d", -4},	\
    {"d-float", -4}, \
    { "", TARGET_DEFAULT}}

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 1
#endif

/* Target machine storage layout */

/* Define for software floating point emulation of VAX format
   when cross compiling from a non-VAX host. */
/* #define REAL_ARITHMETIC */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the vax.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on the vax.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* This is not true on the vax.  */
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

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY (TARGET_VAXC_ALIGNMENT ? 8 : 32)

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS (! TARGET_VAXC_ALIGNMENT)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* No structure field wants to be aligned rounder than this.  */
#define BIGGEST_FIELD_ALIGNMENT (TARGET_VAXC_ALIGNMENT ? 8 : 32)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* Let's keep the stack somewhat aligned.  */
#define STACK_BOUNDARY 32

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 16

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the vax, these are the AP, FP, SP and PC.  */
#define FIXED_REGISTERS {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   On the vax, all registers are one word long.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the vax, all registers can hold all modes.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Vax pc is overloaded on a register.  */
#define PC_REGNUM 15

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 13

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 12

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 0

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 1

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
   
/* The vax has only one kind of registers, so NO_REGS and ALL_REGS
   are the only classes.  */

enum reg_class { NO_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {0, 0xffff}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ALL_REGS

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS ALL_REGS
#define BASE_REG_CLASS ALL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) NO_REGS

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is the constant zero.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'I' ? (VALUE) == 0		\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself. 

   `G' is a floating-point zero.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'G' ? ((VALUE) == CONST0_RTX (DFmode)		\
		 || (VALUE) == CONST0_RTX (SFmode))	\
   : 0)

/* Optional extra constraints for this machine.

   For the VAX, `Q' means that OP is a MEM that does not have a mode-dependent
   address.  */

#define EXTRA_CONSTRAINT(OP, C) \
  ((C) == 'Q'								\
   ? GET_CODE (OP) == MEM && ! mode_dependent_address_p (XEXP (OP, 0))	\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the vax, this is always the size of MODE in words,
   since all registers are the same size.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if longjmp restores from saved registers
   rather than from what setjmp saved.  */
#define LONGJMP_RESTORE_FROM_STACK

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
#define DYNAMIC_CHAIN_ADDRESS(frame) \
gen_rtx (PLUS, Pmode, frame, gen_rtx (CONST_INT, VOIDmode, 12))

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the vax, -(sp) pushes only the bytes of the operands.  */
#define PUSH_ROUNDING(BYTES) (BYTES)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 4

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the Vax, the RET insn always pops all the args for any function.  */

#define RETURN_POPS_ARGS(FUNTYPE,SIZE) (SIZE)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the Vax the return value is in R0 regardless.  */   

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the Vax the return value is in R0 regardless.  */   

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 0)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for a function value.
   On the Vax, R0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* 1 if N is a possible register number for function argument passing.
   On the Vax, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the vax, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the vax, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
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

/* On the vax all args are pushed.  */   

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  extern char call_used_regs[];					\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && !call_used_regs[regno])	\
       mask |= 1 << regno;					\
  fprintf (FILE, "\t.word 0x%x\n", mask);			\
  MAYBE_VMS_FUNCTION_PROLOGUE(FILE)				\
  if ((SIZE) >= 64) fprintf (FILE, "\tmovab %d(sp),sp\n", -SIZE);\
  else if (SIZE) fprintf (FILE, "\tsubl2 $%d,sp\n", (SIZE)); }

/* vms.h redefines this.  */
#define MAYBE_VMS_FUNCTION_PROLOGUE(FILE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tmovab LP%d,r0\n\tjsb mcount\n", (LABELNO));

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\ttstl LPBX0\n\tjneq LPI%d\n\tpushal LPBX0\n\tcalls $1,__bb_init_func\nLPI%d:\n",  \
	   LABELNO, LABELNO);

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  This is a real pain in the
   sphincter on a VAX, since we do not want to change any of the bits in the
   processor status word.  The way it is done here, it is pushed onto the stack
   before any flags have changed, and then the stack is fixed up to account for
   the fact that the instruction to restore the flags only reads a word.
   It may seem a bit clumsy, but at least it works.
*/

#define BLOCK_PROFILER(FILE, BLOCKNO)	\
  fprintf (FILE, "\tmovpsl -(sp)\n\tmovw (sp),2(sp)\n\taddl2 $2,sp\n\taddl2 $1,LPBX2+%d\n\tbicpsw $255\n\tbispsw (sp)+\n", \
		4 * BLOCKNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

/* #define FUNCTION_EPILOGUE(FILE, SIZE)  */

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.

   On the Vax, FRAME_POINTER_REQUIRED is always 1, so the definition of this
   macro doesn't matter.  But it must be defined.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0;

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the vax, the trampoline contains an entry mask and two instructions:
     .word NN
     movl $STATIC,r0   (store the functions static chain)
     jmp  *$FUNCTION   (jump to function code at address FUNCTION)  */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x8fd0));	\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
  ASM_OUTPUT_BYTE  (FILE, 0x50+STATIC_CHAIN_REGNUM);			\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x9f17));	\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 15

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

/* We copy the register-mask from the function's pure code
   to the start of the trampoline.  */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_insn (gen_rtx (ASM_INPUT, VOIDmode,				\
		      "movpsl -(sp)\n\tpushal 1(pc)\n\trei"));		\
  emit_move_insn (gen_rtx (MEM, HImode, TRAMP),				\
		  gen_rtx (MEM, HImode, FNADDR));			\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 4)), CXT);\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 11)),	\
		  plus_constant (FNADDR, 2));				\
}

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT
/* #define HAVE_POST_DECREMENT */

#define HAVE_PRE_DECREMENT
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(regno)  \
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)
#define REGNO_OK_FOR_BASE_P(regno) \
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

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

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) 1
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) 1

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
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

#ifdef NO_EXTERNAL_INDIRECT_ADDRESS

/* Zero if this contains a (CONST (PLUS (SYMBOL_REF) (...))) and the
   symbol in the SYMBOL_REF is an external symbol.  */

#define INDIRECTABLE_CONSTANT_P(X) \
 (! (GET_CODE ((X)) == CONST					\
     && GET_CODE (XEXP ((X), 0)) == PLUS			\
     && GET_CODE (XEXP (XEXP ((X), 0), 0)) == SYMBOL_REF	\
     && SYMBOL_REF_FLAG (XEXP (XEXP ((X), 0), 0))))

/* Re-definition of CONSTANT_ADDRESS_P, which is true only when there
   are no SYMBOL_REFs for external symbols present.  */

#define INDIRECTABLE_CONSTANT_ADDRESS_P(X)   				\
  (GET_CODE (X) == LABEL_REF 						\
   || (GET_CODE (X) == SYMBOL_REF && !SYMBOL_REF_FLAG (X))		\
   || (GET_CODE (X) == CONST && INDIRECTABLE_CONSTANT_P(X))		\
   || GET_CODE (X) == CONST_INT)


/* Non-zero if X is an address which can be indirected.  External symbols
   could be in a sharable image library, so we disallow those.  */

#define INDIRECTABLE_ADDRESS_P(X)  \
  (INDIRECTABLE_CONSTANT_ADDRESS_P (X) 					\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == PLUS						\
       && GET_CODE (XEXP (X, 0)) == REG					\
       && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
       && INDIRECTABLE_CONSTANT_ADDRESS_P (XEXP (X, 1))))

#else /* not NO_EXTERNAL_INDIRECT_ADDRESS */

#define INDIRECTABLE_CONSTANT_ADDRESS_P(X) CONSTANT_ADDRESS_P(X)

/* Non-zero if X is an address which can be indirected.  */
#define INDIRECTABLE_ADDRESS_P(X)  \
  (CONSTANT_ADDRESS_P (X)						\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == PLUS						\
       && GET_CODE (XEXP (X, 0)) == REG					\
       && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
       && CONSTANT_ADDRESS_P (XEXP (X, 1))))

#endif /* not NO_EXTERNAL_INDIRECT_ADDRESS */

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */
#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)  \
{ register rtx xfoob = (X);						\
  if (GET_CODE (xfoob) == REG)						\
    {									\
      extern rtx *reg_equiv_mem;					\
      if (! reload_in_progress						\
	  || reg_equiv_mem[REGNO (xfoob)] == 0				\
	  || INDIRECTABLE_ADDRESS_P (reg_equiv_mem[REGNO (xfoob)]))	\
	goto ADDR;							\
    }									\
  if (CONSTANT_ADDRESS_P (xfoob)) goto ADDR;				\
  if (INDIRECTABLE_ADDRESS_P (xfoob)) goto ADDR;			\
  xfoob = XEXP (X, 0);							\
  if (GET_CODE (X) == MEM && INDIRECTABLE_ADDRESS_P (xfoob))		\
    goto ADDR;								\
  if ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_INC)		\
      && GET_CODE (xfoob) == REG && REG_OK_FOR_BASE_P (xfoob))		\
    goto ADDR; }

/* 1 if PROD is either a reg times size of mode MODE
   or just a reg, if MODE is just one byte.
   This macro's expansion uses the temporary variables xfoo0 and xfoo1
   that must be declared in the surrounding context.  */
#define INDEX_TERM_P(PROD, MODE)   \
(GET_MODE_SIZE (MODE) == 1						\
 ? (GET_CODE (PROD) == REG && REG_OK_FOR_BASE_P (PROD))			\
 : (GET_CODE (PROD) == MULT						\
    &&									\
    (xfoo0 = XEXP (PROD, 0), xfoo1 = XEXP (PROD, 1),			\
     ((GET_CODE (xfoo0) == CONST_INT					\
       && INTVAL (xfoo0) == GET_MODE_SIZE (MODE)			\
       && GET_CODE (xfoo1) == REG					\
       && REG_OK_FOR_INDEX_P (xfoo1))					\
      ||								\
      (GET_CODE (xfoo1) == CONST_INT					\
       && INTVAL (xfoo1) == GET_MODE_SIZE (MODE)			\
       && GET_CODE (xfoo0) == REG					\
       && REG_OK_FOR_INDEX_P (xfoo0))))))

/* Go to ADDR if X is the sum of a register
   and a valid index term for mode MODE.  */
#define GO_IF_REG_PLUS_INDEX(X, MODE, ADDR)	\
{ register rtx xfooa;							\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 0)) == REG					\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
	  && (xfooa = XEXP (X, 1),					\
	      INDEX_TERM_P (xfooa, MODE)))				\
	goto ADDR;							\
      if (GET_CODE (XEXP (X, 1)) == REG					\
	  && REG_OK_FOR_BASE_P (XEXP (X, 1))				\
	  && (xfooa = XEXP (X, 0),					\
	      INDEX_TERM_P (xfooa, MODE)))				\
	goto ADDR; } }

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{ register rtx xfoo, xfoo0, xfoo1;					\
  GO_IF_NONINDEXED_ADDRESS (X, ADDR);					\
  if (GET_CODE (X) == PLUS)						\
    { /* Handle <address>[index] represented with index-sum outermost */\
      xfoo = XEXP (X, 0);						\
      if (INDEX_TERM_P (xfoo, MODE))					\
	{ GO_IF_NONINDEXED_ADDRESS (XEXP (X, 1), ADDR); }		\
      xfoo = XEXP (X, 1);						\
      if (INDEX_TERM_P (xfoo, MODE))					\
	{ GO_IF_NONINDEXED_ADDRESS (XEXP (X, 0), ADDR); }		\
      /* Handle offset(reg)[index] with offset added outermost */	\
      if (INDIRECTABLE_CONSTANT_ADDRESS_P (XEXP (X, 0)))		\
	{ if (GET_CODE (XEXP (X, 1)) == REG				\
	      && REG_OK_FOR_BASE_P (XEXP (X, 1)))			\
	    goto ADDR;							\
	  GO_IF_REG_PLUS_INDEX (XEXP (X, 1), MODE, ADDR); }		\
      if (INDIRECTABLE_CONSTANT_ADDRESS_P (XEXP (X, 1)))		\
	{ if (GET_CODE (XEXP (X, 0)) == REG				\
	      && REG_OK_FOR_BASE_P (XEXP (X, 0)))			\
	    goto ADDR;							\
	  GO_IF_REG_PLUS_INDEX (XEXP (X, 0), MODE, ADDR); } } }

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the vax, nothing needs to be done.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the VAX, the predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand)
   and all indexed address depend thus (because the index scale factor
   is the length of the operand).  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 { if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC)	\
     goto LABEL; 							\
   if (GET_CODE (ADDR) == PLUS)						\
     { if (CONSTANT_ADDRESS_P (XEXP (ADDR, 0))				\
	   && GET_CODE (XEXP (ADDR, 1)) == REG);			\
       else if (CONSTANT_ADDRESS_P (XEXP (ADDR, 1))			\
		&& GET_CODE (XEXP (ADDR, 0)) == REG);			\
       else goto LABEL; }}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define this if the case instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE

/* Define this if the case instruction drops through after the table
   when the index is out of range.  Don't define it if the case insn
   jumps to the default label instead.  */
#define CASE_DROPS_THROUGH

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
#define MOVE_MAX 8

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

/* This machine doesn't use IEEE floats.  */

#define TARGET_FLOAT_FORMAT VAX_FLOAT_FORMAT

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

/* On a VAX, constants from 0..63 are cheap because they can use the
   1 byte literal constant format.  compare to -1 should be made cheap
   so that decrement-and-branch insns can be formed more easily (if
   the value -1 is copied to a register some decrement-and-branch patterns
   will not match).  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) == 0) return 0;				\
    if ((OUTER_CODE) == AND)					\
      return ((unsigned) ~INTVAL (RTX) <= 077) ? 1 : 2;		\
    if ((unsigned) INTVAL (RTX) <= 077) return 1;		\
    if ((OUTER_CODE) == COMPARE && INTVAL (RTX) == -1)		\
      return 1;							\
    if ((OUTER_CODE) == PLUS && (unsigned) -INTVAL (RTX) <= 077)\
      return 1;							\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    if (GET_MODE_CLASS (GET_MODE (RTX)) == MODE_FLOAT)		\
      return vax_float_literal (RTX) ? 5 : 8;			\
    else							\
      return (((CONST_DOUBLE_HIGH (RTX) == 0			\
		&& (unsigned) CONST_DOUBLE_LOW (RTX) < 64)	\
	       || ((OUTER_CODE) == PLUS				\
		   && CONST_DOUBLE_HIGH (RTX) == -1		\
		   && (unsigned)-CONST_DOUBLE_LOW (RTX) < 64))	\
	      ? 2 : 5);

#define RTX_COSTS(RTX,CODE,OUTER_CODE) case FIX: case FLOAT:	\
 case MULT: case DIV: case UDIV: case MOD: case UMOD:		\
 case ASHIFT: case LSHIFTRT: case ASHIFTRT:			\
 case ROTATE: case ROTATERT: case PLUS: case MINUS: case IOR:	\
 case XOR: case AND: case NEG: case NOT: case ZERO_EXTRACT:	\
 case SIGN_EXTRACT: case MEM: return vax_rtx_cost(RTX)

#define	ADDRESS_COST(RTX) (1 + (GET_CODE (RTX) == REG ? 0 : vax_address_cost(RTX)))

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch.

   Branches are extremely cheap on the VAX while the shift insns often
   used to replace branches can be expensive.  */

#define BRANCH_COST 0

/*
 * We can use the BSD C library routines for the libgcc calls that are
 * still generated, since that's what they boil down to anyways.
 */

#define UDIVSI3_LIBCALL "*udiv"
#define UMODSI3_LIBCALL "*urem"

/* Check a `double' value for validity for a particular machine mode.  */

/* note that it is very hard to accidentally create a number that fits in a
   double but not in a float, since their ranges are almost the same */

#define CHECK_FLOAT_VALUE(MODE, D, OVERFLOW) \
  ((OVERFLOW) = check_float_value (MODE, &D, OVERFLOW))

/* For future reference:
   D Float: 9 bit, sign magnitude, excess 128 binary exponent
            normalized 56 bit fraction, redundant bit not represented
            approximately 16 decimal digits of precision

   The values to use if we trust decimal to binary conversions:
#define MAX_D_FLOAT 1.7014118346046923e+38
#define MIN_D_FLOAT .29387358770557188e-38

   G float: 12 bit, sign magnitude, excess 1024 binary exponent
            normalized 53 bit fraction, redundant bit not represented
            approximately 15 decimal digits precision

   The values to use if we trust decimal to binary conversions:
#define MAX_G_FLOAT .898846567431157e+308
#define MIN_G_FLOAT .556268464626800e-308
*/

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the vax.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ if (GET_CODE (EXP) == SET)					\
    { if (GET_CODE (SET_SRC (EXP)) == CALL)			\
	CC_STATUS_INIT;						\
      else if (GET_CODE (SET_DEST (EXP)) != PC)			\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (EXP);			\
	  cc_status.value2 = SET_SRC (EXP); } }			\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    {								\
      if (GET_CODE (SET_SRC (XVECEXP (EXP, 0, 0))) == CALL)	\
	CC_STATUS_INIT;						\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) != PC) \
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (XVECEXP (EXP, 0, 0));	\
	  cc_status.value2 = SET_SRC (XVECEXP (EXP, 0, 0)); } } \
  /* PARALLELs whose first element sets the PC are aob, sob insns.	\
     They do change the cc's.  So drop through and forget the cc's.  */ \
  else CC_STATUS_INIT;						\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))	\
    cc_status.value2 = 0;					\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM	\
      && cc_status.value2					\
      && GET_CODE (cc_status.value2) == MEM)			\
    cc_status.value2 = 0; }
/* Actual condition, one line up, should be that value2's address
   depends on value1, but that is too much of a pain.  */

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  \
{ if (cc_status.flags & CC_NO_OVERFLOW)				\
    return NO_OV;						\
  return NORMAL; }

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
 "r9", "r10", "r11", "ap", "fp", "sp", "pc"}

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* How to renumber registers for dbx and gdb.
   Vax needs no change in the numeration.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't use the `xsfoo;' construct in DBX output; this system
   doesn't support it.  */

#define DBX_NO_XREFS

/* Output the .stabs for a C `static' variable in the data section.  */
#define DBX_STATIC_STAB_DATA_SECTION

/* Vax specific: which type character is used for type double?  */

#define ASM_DOUBLE_CHAR (TARGET_G_FLOAT ? 'g' : 'd')

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.
   It is .dfloat or .gfloat, depending.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { char dstr[30];							\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", dstr);			\
     fprintf (FILE, "\t.%cfloat 0%c%s\n", ASM_DOUBLE_CHAR, 		\
					  ASM_DOUBLE_CHAR, dstr);	\
   } while (0);

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  do { char dstr[30];						\
       REAL_VALUE_TO_DECIMAL (VALUE, "%.20e", dstr);		\
       fprintf (FILE, "\t.float 0f%s\n", dstr); } while (0);

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
  fprintf (FILE, "\tpushl %s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmovl (sp)+,%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
   (The Vax does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  fprintf (FILE, "\t.align %d\n", (LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %u\n", (SIZE))

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

/* When debugging, we want to output an extra dummy label so that gas
   can distinguish between D_float and G_float prior to processing the
   .stabs directive identifying type double.  */

#define ASM_IDENTIFY_LANGUAGE(FILE)	\
  do {								\
    output_lang_identify (FILE);				\
    if (write_symbols == DBX_DEBUG)				\
      fprintf (FILE, "___vax_%c_doubles:\n", ASM_DOUBLE_CHAR);	\
  } while (0)

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

VAX operand formatting codes:

 letter	   print
   C	reverse branch condition
   D	64-bit immediate operand
   B	the low 8 bits of the complement of a constant operand
   H	the low 16 bits of the complement of a constant operand
   M	a mask for the N highest bits of a word
   N	the complement of a constant integer operand
   P	constant operand plus 1
   R	32 - constant operand
   b	the low 8 bits of a negated constant operand
   h	the low 16 bits of a negated constant operand
   #	'd' or 'g' depending on whether dfloat or gfloat is used  */

/* The purpose of D is to get around a quirk or bug in vax assembler
   whereby -1 in a 64-bit immediate operand means 0x00000000ffffffff,
   which is not a 64-bit minus one.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '#')

#define PRINT_OPERAND(FILE, X, CODE)  \
{ extern char *rev_cond_name ();					\
  if (CODE == '#') fputc (ASM_DOUBLE_CHAR, FILE);			\
  else if (CODE == 'C')							\
    fputs (rev_cond_name (X), FILE);					\
  else if (CODE == 'D' && GET_CODE (X) == CONST_INT && INTVAL (X) < 0)	\
    fprintf (FILE, "$0xffffffff%08x", INTVAL (X));			\
  else if (CODE == 'P' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", INTVAL (X) + 1);				\
  else if (CODE == 'N' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", ~ INTVAL (X));				\
  /* rotl instruction cannot deal with negative arguments.  */		\
  else if (CODE == 'R' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", 32 - INTVAL (X));				\
  else if (CODE == 'H' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", 0xffff & ~ INTVAL (X));			\
  else if (CODE == 'h' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", (short) - INTVAL (x));			\
  else if (CODE == 'B' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", 0xff & ~ INTVAL (X));				\
  else if (CODE == 'b' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", 0xff & - INTVAL (X));				\
  else if (CODE == 'M' && GET_CODE (X) == CONST_INT)			\
    fprintf (FILE, "$%d", ~((1 << INTVAL (x)) - 1));			\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { REAL_VALUE_TYPE r; char dstr[30];					\
      REAL_VALUE_FROM_CONST_DOUBLE (r, X);				\
      REAL_VALUE_TO_DECIMAL (r, "%.20e", dstr);				\
      fprintf (FILE, "$0f%s", dstr); }					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == DFmode)	\
    { REAL_VALUE_TYPE r; char dstr[30];					\
      REAL_VALUE_FROM_CONST_DOUBLE (r, X);				\
      REAL_VALUE_TO_DECIMAL (r, "%.20e", dstr);				\
      fprintf (FILE, "$0%c%s", ASM_DOUBLE_CHAR, dstr); }		\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in output-vax.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
 print_operand_address (FILE, ADDR)
