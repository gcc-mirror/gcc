/* Definitions of target machine for GNU compiler, for SPUR chip.
   Copyright (C) 1988, 1995 Free Software Foundation, Inc.

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


/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dspur -Acpu(spur) -Amachine(spur)"

/* Link with libg.a when debugging, for dbx's sake.  */

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (spur)");

/* Run-time compilation parameters selecting different hardware subsets.

   On the SPUR, we don't yet need any.  */

extern int target_flags;

/* Nonzero if we should generate code to use the fpu.  */
#define TARGET_FPU (target_flags & 1)

/* Nonzero if we should expand constant shifts into series of shift
   instructions.  */
#define TARGET_EXPAND_SHIFTS (target_flags & 2)

/* Nonzero if we should generate long jumps for compares. */
#define TARGET_LONG_JUMPS (target_flags & 4)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"fpu", 1},			\
    {"soft-float", -1},		\
    {"expand-shifts", 2},       \
    {"lib-shifts", -2},		\
    {"long-jumps", 4},		\
    {"short-jumps", -4},	\
    { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 0

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is a moot question on the SPUR due to the lack of bit-field insns.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on SPUR.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* For SPUR we can decide arbitrarily
   since there are no machine instructions for them.  */
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
#define PARM_BOUNDARY 64

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   SPUR has 32 fullword registers and 15 floating point registers.  */

#define FIRST_PSEUDO_REGISTER 47

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On SPUR, this includes all the global registers
   and the callee return address register.  */
#define FIXED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 0, 0, 0, 0, 0,		\
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 0, 0, 0, 0, 0,		\
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 0, 0, 0, 0, 0,		\
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1,	\
  1, 1, 1, 1, 1, 1,		\
  1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On SPUR, ordinary registers hold 32 bits worth;
   a single floating point register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) >= 32 ? GET_MODE_NUNITS ((MODE))	\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On SPUR, the cpu registers can hold any mode but the float registers
   can hold only floating point.  And they can't hold anything if use
   of hardware floating point is disabled.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (((REGNO) < 32							\
    && (REGNO) + ((GET_MODE_UNIT_SIZE ((MODE)) + 3) / 4) <= 32)		\
   || (TARGET_FPU && ((MODE) == SFmode || (MODE) == DFmode		\
		      || (MODE) == SCmode || (MODE) == DCmode)))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (((MODE1) == SFmode || (MODE1) == DFmode		\
    || (MODE1) == SCmode || (MODE1) == DCmode)		\
   == ((MODE2) == SFmode || (MODE2) == DFmode		\
       || (MODE2) == SCmode || (MODE2) == DCmode))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* SPUR pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 4

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 25

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 25

/* Register in which static-chain is passed to a function.  */
/* ??? */
#define STATIC_CHAIN_REGNUM 8

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 27
#define STRUCT_VALUE_INCOMING_REGNUM 11

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
   
/* The 68000 has two kinds of registers, hence four classes.  */

enum reg_class { NO_REGS, GENERAL_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "GENERAL_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0, 0}, {-1, 0}, {0, 0x7fff}, {-1, 0x7fff}}

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

   For SPUR, `I' is used for the range of constants an insn
   can actually contain.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for the 5-bit operand of a compare insns.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (unsigned) ((VALUE) + 0x2000) < 0x4000	\
   : (C) == 'J' ? (VALUE) == 0				\
   : (C) == 'K' ? (unsigned) (VALUE) < 0x20		\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  	\
  ((C) == 'G' && CONST_DOUBLE_HIGH (VALUE) == 0		\
   && CONST_DOUBLE_LOW (VALUE) == 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On SPUR, this is the size of MODE in words,
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
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On SPUR, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

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
   otherwise, FUNC is 0.  */

/* On SPUR the value is found in the second "output" register.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 27)

/* But the called function leaves it in the second "input" register.  */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 11)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 27)

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On SPUR, the first "output" reg is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 27)

/* 1 if N is a possible register number for function argument passing.
   On SPUR, these are the "output" registers.  */

#define FUNCTION_ARG_REGNO_P(N) ((N) < 32 && (N) > 26)

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the called function
   corresponding to register number OUT as seen by the calling function.
   Return OUT if register number OUT is not an outbound register.  */

#define INCOMING_REGNO(OUT) \
 (((OUT) < 27 || (OUT) > 31) ? (OUT) : (OUT) - 16)

/* Define this macro if the target machine has "register windows".  This
   C expression returns the register number as seen by the calling function
   corresponding to register number IN as seen by the called function.
   Return IN if register number IN is not an inbound register.  */

#define OUTGOING_REGNO(IN) \
 (((IN) < 11 || (IN) > 15) ? (IN) : (IN) + 16)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SPUR, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 5 or more means all following args should go on the stack.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On SPUR, the offset normally starts at 0, but starts at 4 bytes
   when the function gets a structure-value-address as an
   invisible first argument.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
 ((CUM) = ((FNTYPE) != 0 && aggregate_value_p (TREE_TYPE ((FNTYPE)))))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) / 4	\
	    : (int_size_in_bytes (TYPE) + 3) / 4))

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

/* On SPUR the first five words of args are normally in registers
   and the rest are pushed.  But any arg that won't entirely fit in regs
   is pushed.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)		\
(5 >= ((CUM)						\
       + ((MODE) == BLKmode				\
	  ? (int_size_in_bytes (TYPE) + 3) / 4		\
	  : (GET_MODE_SIZE (MODE) + 3) / 4))		\
 ? gen_rtx (REG, (MODE), 27 + (CUM))			\
 : 0)

/* Define where a function finds its arguments.
   This is different from FUNCTION_ARG because of register windows.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)	\
(5 >= ((CUM)						\
       + ((MODE) == BLKmode				\
	  ? (int_size_in_bytes (TYPE) + 3) / 4		\
	  : (GET_MODE_SIZE (MODE) + 3) / 4))		\
 ? gen_rtx (REG, (MODE), 11 + (CUM))			\
 : 0)

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

/* On spur, move-double insns between fpu and cpu need an 8-byte block
   of memory.  If any fpu reg is used in the function, we allocate
   such a block here, at the bottom of the frame, just in case it's needed.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  extern int current_function_pretend_args_size;		\
  int fsize = ((SIZE) + 7) & ~7;				\
  int nregs, i, fp_used = 0;					\
  for (i = 32, nregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
    {								\
      if (regs_ever_live[i] && ! call_used_regs[i])		\
        nregs++;						\
      if (regs_ever_live[i]) fp_used = 1;			\
    }								\
  if (fp_used) fsize += 8;					\
  fprintf (FILE, "0:\trd_special r24,pc\n");			\
  fprintf (FILE, "\tand r24,r24,$~0x3\n");			\
  fprintf (FILE, "\tadd_nt r25,r4,$%d\n",			\
	   - current_function_pretend_args_size);		\
  if (fsize + nregs != 0 || current_function_pretend_args_size > 0)\
    {								\
      int n = - fsize - nregs * 16;				\
      if (n >= -8192)						\
        fprintf (FILE, "\tadd_nt r4,r25,$%d\n", n);		\
      else							\
        {							\
	  fprintf (FILE, "\tadd_nt r4,r25,$-8192\n");		\
	  n += 8192;						\
          while (n < -8192)					\
            fprintf (FILE, "\tadd_nt r4,r4,$-8192\n"), n += 8192; \
	  if (n != 0)						\
            fprintf (FILE, "\tadd_nt r4,r4,$%d\n", n);		\
        }							\
      }								\
  for (i = 32, nregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
    if (regs_ever_live[i] && ! call_used_regs[i])		\
      {								\
        fprintf (FILE, "\tst_ext1 %s,r4,$%d\n",			\
	         reg_names[i], 8 * nregs++);			\
        fprintf (FILE, "\tst_ext2 %s,r4,$%d\n",			\
	         reg_names[i], 8 * nregs++);			\
      }								\
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   abort ();

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int current_function_calls_alloca;
extern int current_function_pretend_args_size;

#define EXIT_IGNORE_STACK	\
 (get_frame_size () != 0	\
  || current_function_calls_alloca || current_function_pretend_args_size)

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  extern int current_function_calls_alloca;			\
  extern int current_function_pretend_args_size;		\
  int fsize = ((SIZE) + 7) & ~7;				\
  int nregs, i, fp_used = 0;					\
  for (i = 32, nregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
    {								\
      if (regs_ever_live[i] && ! call_used_regs[i])		\
	nregs++;						\
      if (regs_ever_live[i]) fp_used = 1;			\
    }								\
  if (fp_used) fsize += 8;					\
  if (nregs != 0)						\
    {								\
      fprintf (FILE, "\tadd_nt r4,r25,$%d\n", - fsize - nregs * 16); \
      for (i = 32, nregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
        if (regs_ever_live[i] && ! call_used_regs[i])		\
	  {							\
            fprintf (FILE, "\tld_ext1 %s,r4,$%d\n\tnop\n",	\
		     reg_names[i], 8 * nregs++);		\
            fprintf (FILE, "\tld_ext2 %s,r4,$%d\n\tnop\n",	\
		     reg_names[i], 8 * nregs++);		\
	  }							\
    }								\
  if (fsize != 0 || nregs != 0 || current_function_calls_alloca	\
      || current_function_pretend_args_size > 0)		\
    fprintf (FILE, "\tadd_nt r4,r25,$%d\n",			\
	     current_function_pretend_args_size);		\
  fprintf (FILE, "\treturn r10,$8\n\tnop\n");			\
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
(((REGNO) ^ 0x20) < 14 || (unsigned) (reg_renumber[REGNO] ^ 0x20) < 14)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the SPUR, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X)		\
 ((GET_CODE (X) == CONST_INT			\
   && (unsigned) (INTVAL (X) + 0x2000) < 0x4000)\
  || (GET_CODE (X) == SYMBOL_REF && (X)->unchanging))

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

   On SPUR, the actual legitimate addresses must be REG+SMALLINT or REG+REG.
   Actually, REG+REG is not legitimate for stores, so 
   it is obtained only by combination on loads.
   We can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{ if (GET_CODE (X) == REG			\
      && REG_OK_FOR_BASE_P (X))			\
    goto ADDR;					\
  if (GET_CODE (X) == SYMBOL_REF && (X)->unchanging)	\
    goto ADDR;					\
  if (GET_CODE (X) == PLUS			\
      && GET_CODE (XEXP (X, 0)) == REG		\
      && REG_OK_FOR_BASE_P (XEXP (X, 0)))	\
    {						\
      if (GET_CODE (XEXP (X, 1)) == CONST_INT	\
	  && INTVAL (XEXP (X, 1)) >= -0x2000	\
	  && INTVAL (XEXP (X, 1)) < 0x2000)	\
	goto ADDR;				\
    }						\
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

/* On SPUR, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	\
{ if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   copy_to_mode_reg (SImode, XEXP (X, 1)));	\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   copy_to_mode_reg (SImode, XEXP (X, 0)));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the SPUR this is never true.  */

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

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* This is BSD, so it wants DBX format.  */
#define DBX_DEBUGGING_INFO

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

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) < 0x2000 && INTVAL (RTX) >= -0x2000) return 1; \
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 2;							\
  case CONST_DOUBLE:						\
    return 4;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* (None are needed on SPUR.)  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* The SPUR does not really have a condition code.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ CC_STATUS_INIT; }

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)

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
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9",		\
 "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19",	\
 "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",	\
 "r30", "r31",								\
 "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9",	\
 "f10", "f11", "f12", "f13", "f14" }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

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

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double %.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.single %.12e\n", (VALUE))

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

/* This is how to output code to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tadd_nt r4,r4,$-4\n\tst_32 %s,r4,$0\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tld_32 %s,r4,$0\n\tadd_nt r4,r4,$4\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (SPUR does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

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

   On SPUR, the CODE can be `r', meaning this is a register-only operand
   and an immediate zero should be represented as `r0'.  */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE)				\
    abort ();								\
  else if ((CODE) == 'r' && (X) == const0_rtx)				\
    fprintf (FILE, "r0");						\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;					\
  int offset = 0;						\
  register rtx addr = ADDR;					\
  if (GET_CODE (addr) == REG)					\
    {								\
      fprintf (FILE, "%s,$0", reg_names[REGNO (addr)]);		\
    }								\
  else if (GET_CODE (addr) == PLUS)				\
    {								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);\
      else							\
	base = XEXP (addr, 0), index = XEXP (addr, 1);		\
      fprintf (FILE, "%s,", reg_names[REGNO (base)]);		\
      if (index == 0)						\
	fprintf (FILE, "$%d", offset);				\
      else							\
	fprintf (FILE, "%s,", reg_names[REGNO (index)]);	\
    }								\
  else								\
    {								\
      fprintf (FILE, "r24,$(");					\
      output_addr_const (FILE, addr);				\
      fprintf (FILE, "-0b)");					\
    }								\
}
