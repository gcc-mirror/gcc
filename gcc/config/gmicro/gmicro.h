/* Definitions of target machine for GNU compiler.  Gmicro (TRON) version.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.
   Contributed by Masanobu Yuhara, Fujitsu Laboratories LTD.
   (yuhara@flab.fujitsu.co.jp)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.   */


/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dgmicro -Acpu(tron) -Amachine(tron)"

/* #define CPP_SPEC    ** currently not defined **/

/* #define CC1_SPEC    ** currently not defined **/


/* Print subsidiary information on the compiler version in use.  */
/*
#define TARGET_VERSION fprintf (stderr, " (Gmicro syntax)");
*/

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Compile for a Gmicro/300.  */
#define TARGET_G300 (target_flags & 1)
/* Compile for a Gmicro/200. */
#define TARGET_G200 (target_flags & 2)
/* Compile for a Gmicro/100. */
#define TARGET_G100 (target_flags & 4)

/* Compile FPU insns for floating point (not library calls).  */
#define TARGET_FPU (target_flags & 8)

/* Pop up arguments by called function. */
#define TARGET_RTD (target_flags & 0x10)

/* Compile passing first args in regs 0 and 1.
   This exists only to test compiler features that will be needed for
   RISC chips. It is not usable and is not intended to be usable on
   this cpu ;-< */
#define TARGET_REGPARM (target_flags & 0x20)

#define TARGET_BITFIELD (target_flags & 0x40)

#define TARGET_NEWRETURN (target_flags & 0x80)

/* Do not expand __builtin_smov (strcpy) to multiple movs.
   Use the smov instruction. */
#define TARGET_FORCE_SMOV (target_flags & 0x100)

/* default options are -m300, -mFPU,
   with bitfield instructions added because it won't always work otherwise.
   If there are versions of the gmicro that don't support bitfield instructions
   then it will take some thinking to figure out how to make them work.  */
#define TARGET_DEFAULT 0x49

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { { "g300", 1},				\
    { "g200", 2},				\
    { "g100", 4},				\
    { "fpu", 8},				\
    { "soft-float", -8},			\
    { "rtd", 0x10},				\
    { "no-rtd", -0x10},				\
    { "regparm", 0x20},				\
    { "no-regparm", -0x20},			\
#if 0 /* Since we don't define PCC_BITFIELD_TYPE_MATTERS or use a large
	 STRUCTURE_SIZE_BOUNDARY, we must have bitfield instructions.  */
    { "bitfield", 0x40},			\
    { "no-bitfield", -0x40},			\
#endif
    { "newreturn", 0x80},			\
    { "no-newreturn", -0x80},			\
    { "force-smov", 0x100},			\
    { "no-force-smov", -0x100},			\
    { "", TARGET_DEFAULT}}


/* Blow away G100 flag silently off TARGET_fpu (since we can't clear
   any bits in TARGET_SWITCHES above) */
#define OVERRIDE_OPTIONS		\
{					\
  if (TARGET_G100) target_flags &= ~8;	\
}

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is true for Gmicro insns.
   We make it true always by avoiding using the single-bit insns
   except in special cases with constant bit numbers.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the Gmicro.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* For Gmicro we can decide arbitrarily
   since there are no machine instructions for them.  ????? */
#define WORDS_BIG_ENDIAN 0

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register. */
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
/* Instructions of the Gmicro should be on half-word boundary */
/* But word boundary gets better performance */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* No data type wants to be aligned rounder than this. */
/* This is not necessarily 32 on the Gmicro */
#define BIGGEST_ALIGNMENT 32

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.
   Unaligned data is allowed on Gmicro, though the access is slow. */

#define STRICT_ALIGNMENT 1
#define SLOW_UNALIGNED_ACCESS 1

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Define number of bits in most basic integer type.
   (If undefined, default is BITS_PER_WORD).  */
#define INT_TYPE_SIZE 32

/* #define PCC_BITFIELD_TYPE_MATTERS 1 ????? */

/* #define CHECK_FLOAT_VALUE (MODE, VALUE) ????? */


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.
   For the Gmicro, we give the general registers numbers 0-15,
   and the FPU floating point registers numbers 16-31.  */
#define FIRST_PSEUDO_REGISTER 32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the Gmicro, the stack pointer and the frame pointer are
   such registers.  */
/* frame pointer is not indicated as fixed, because fp may be used freely
   when a frame is not built. */
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 1, \
  /* FPU registers.  */   \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
 {1, 1, 1, 1, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 1, \
  /* FPU registers.  */   \
  1, 1, 1, 1, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, }


/* Make sure everything's fine if we *don't* have a given processor.
   This assumes that putting a register in fixed_regs will keep the
   compilers mitt's completely off it.  We don't bother to zero it out
   of register classes.  If TARGET_FPU is not set,
   the compiler won't touch since no instructions that use these
   registers will be valid.  */
/*  This Macro is not defined now.
    #define CONDITIONAL_REGISTER_USAGE */

/* The Gmicro has no overlapping register */
/* #define OVERLAPPING_REGNO_P(REGNO) */

/* #define INSN_CLOBBERS_REGNO_P(INSN,REGNO)  */
/* #define PRESERVE_DEATH_INFO_REGNO_P(REGNO)  */

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the Gmicro, ordinary registers hold 32 bits worth;
   for the Gmicro/FPU registers, a single register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) >= 16 ? 1				\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the Gmicro, the cpu registers can hold any mode but the FPU registers
   can hold only SFmode or DFmode.  And the FPU registers can't hold anything
   if FPU use is disabled. */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((REGNO) < 16								\
   || ((REGNO) < 32							\
       ? TARGET_FPU && (GET_MODE_CLASS (MODE) == MODE_FLOAT ||		\
			GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)	\
       : 0 ))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)			\
  (! TARGET_FPU						\
   || ((GET_MODE_CLASS (MODE1) == MODE_FLOAT ||		\
	GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)	\
       == ((MODE2) == SFmode || (MODE2) == DFmode)))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Gmicro pc isn't overloaded on a register.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 14

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
/* The Gmicro does not have hardware ap. Fp is treated as ap */
#define ARG_POINTER_REGNUM 14

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

/* The Gmicro has two kinds of registers, so four classes would be
   a complete set.  */

enum reg_class { NO_REGS, FPU_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
   { "NO_REGS", "FPU_REGS", "GENERAL_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{						\
     0,			/* NO_REGS */		\
     0xffff0000,	/* FPU_REGS */		\
     0x0000ffff,	/* GENERAL_REGS */	\
     0xffffffff		/* ALL_REGS */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) ( (REGNO < 16) ? GENERAL_REGS : FPU_REGS )

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS  GENERAL_REGS
  
/* Get reg_class from a letter such as appears in the machine description.
   We do a trick here to modify the effective constraints on the
   machine description; we zorch the constraint letters that aren't
   appropriate for a specific target.  This allows us to guarantee
   that a specific kind of register will not be used for a given target
   without fiddling with the register classes above. */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'r' ? GENERAL_REGS :			\
   ((C) == 'f' ? (TARGET_FPU ? FPU_REGS : NO_REGS) :	\
     NO_REGS))

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the Gmicro, all immediate value optimizations are done 
   by assembler, so no machine dependent definition is necessary ??? */

/* #define CONST_OK_FOR_LETTER_P(VALUE, C) ((C) == 'I') */
#define CONST_OK_FOR_LETTER_P(VALUE, C) 0

/*
 * The letters G defines all of the floating constants tha are *NOT*
 * Gmicro-FPU constant.
 */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)	\
    ((C) == 'F' ||				\
     (C) == 'G' && !(TARGET_FPU && standard_fpu_constant_p (VALUE)))

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class. */
/* On the Gmicro series, there is no restriction on GENERAL_REGS,
   so CLASS is returned. I do not know whether I should treat FPU_REGS
   specially or not (at least, m68k does not). */

#define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the Gmicro, this is the size of MODE in words,
   except in the FPU regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FPU_REGS ? \
   1 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

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
/* On the Gmicro, FP points to the old FP and the first local variables are
   at (FP - 4). */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by. */
/* On the Gmicro, sp is decremented by the exact size of the operand */
#define PUSH_ROUNDING(BYTES) (BYTES)

/* Offset of first parameter from the argument pointer register value.  */
/* On the Gmicro, the first argument is found at (ap + 8) where ap is fp. */
#define FIRST_PARM_OFFSET(FNDECL) 8

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack. 

   On the Gmicro, the EXITD insn may be used to pop them if the number
   of args is fixed, but if the number is variable then the caller must pop
   them all. The adjsp operand of the EXITD insn can't be used for library
   calls now because the library is compiled with the standard compiler.
   Use of adjsp operand is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.
   On the m68k this is an RTD option, so I use the same name
   for the Gmicro. The option name may be changed in the future. */

#define RETURN_POPS_ARGS(FUNTYPE,SIZE)   \
  ((TARGET_RTD && TREE_CODE (FUNTYPE) != IDENTIFIER_NODE	\
    && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	|| (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	    = void_type_node)))					\
   ? (SIZE) : 0)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the Gmicro the floating return value is in fr0 not r0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  LIBCALL_VALUE (TYPE_MODE (VALTYPE))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)	\
  (gen_rtx (REG, (MODE),		\
    ((TARGET_FPU && ((MODE) == SFmode || (MODE) == DFmode)) ? 16 : 0)))


/* 1 if N is a possible register number for a function value.
   On the Gmicro, r0 and fp0 are the possible registers.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N) == 16)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for function argument passing.
   On the Gmicro, no registers are used in this way.  */
/* Really? For the performance improvement, registers should be used !! */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the Gmicro, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the Gmicro, the offset starts at 0.  */

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

/* On the Gmicro all args are pushed, except if -mregparm is specified
   then the first two words of arguments are passed in d0, d1.
   *NOTE* -mregparm does not work.
   It exists only to test register calling conventions.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8) ? gen_rtx (REG, (MODE), (CUM) / 4) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8					\
  && 8 < ((CUM) + ((MODE) == BLKmode				\
		      ? int_size_in_bytes (TYPE)		\
		      : GET_MODE_SIZE (MODE))))  		\
 ? 2 - (CUM) / 4 : 0)

/* The following macro is defined to output register list.
   The LSB of Mask is the lowest number register.
   Regoff is MY_GREG_OFF or MY_FREG_OFF.
   Do NOT use <i> in File, Mask, Regoff !!
   Should be changed from macros to functions.    M.Yuhara */

#define MY_GREG_OFF 0
#define MY_FREG_OFF 16

#define MY_PRINT_MASK(File, Mask, Regoff)		\
{							\
    int i, first = -1;					\
    if ((Mask) == 0) {					\
	fprintf(File, "#0");				\
    } else {						\
	fprintf(File, "(");				\
	for (i = 0; i < 16; i++) {			\
	    if ( (Mask) & (1 << i) ) {			\
		if (first < 0) {			\
		    if (first == -2) {			\
			fprintf(File, ",");		\
		    }					\
		    first = i;				\
		    fprintf(File, "%s", reg_names[Regoff + i]);	\
		}					\
	    } else if (first >= 0) {			\
		if (i > first + 1) {			\
		    fprintf(File, "-%s", reg_names[Regoff + i - 1]);	\
		}					\
		first = -2;				\
	    }						\
	}						\
	if ( (first >= 0) && (first != 15) )		\
	    fprintf(File, "-%s", reg_names[Regoff + 15]);\
	fprintf(File, ")");				\
    }							\
}


#define MY_PRINT_ONEREG_L(FILE,MASK)		\
{   register int i;				\
    for (i = 0; i < 16; i++)			\
	if ( (1 << i) & (MASK)) {		\
	    fprintf(FILE, "%s", reg_names[i]);	\
	    (MASK) &= ~(1 << i);		\
	    break;				\
	}					\
}


#define MY_PRINT_ONEREG_H(FILE,MASK)		\
{   register int i;				\
    for (i = 15; i >= 0; i--)			\
	if ( (1 << i) & (MASK)) {		\
	    fprintf(FILE, "%s", reg_names[i]);	\
	    (MASK) &= ~(1 << i);		\
	    break;				\
	}					\
}

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

/* The next macro needs much optimization !!
   M.Yuhara */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  register int nregs = 0;					\
  static char *reg_names[] = REGISTER_NAMES;			\
  extern char call_used_regs[];					\
  int fsize = ((SIZE) + 3) & -4;				\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && !call_used_regs[regno]) {	\
	mask |= (1 << regno);					\
	nregs++;						\
    }								\
  if (frame_pointer_needed) {					\
    mask &= ~(1 << FRAME_POINTER_REGNUM);			\
    if (nregs > 4) {						\
	fprintf(FILE, "\tenter.w #%d,", fsize);			\
	MY_PRINT_MASK(FILE, mask, MY_GREG_OFF);			\
	fprintf(FILE,"\n");					\
    } else {							\
	fprintf(FILE, "\tmov.w fp,@-sp\n");			\
	fprintf(FILE, "\tmov.w sp,fp\n");			\
	if (fsize > 0)						\
	    myoutput_sp_adjust(FILE, "sub", fsize);		\
	while (nregs--) {					\
	    fprintf(FILE, "\tmov.w ");				\
	    MY_PRINT_ONEREG_H(FILE, mask);			\
	    fprintf(FILE, ",@-sp\n");				\
	}							\
    }								\
  } else {							\
    if (fsize > 0)						\
	myoutput_sp_adjust(FILE, "sub", fsize);			\
    if (mask != 0) {						\
	if (nregs > 4) {					\
	    fprintf(FILE, "\tstm.w ");				\
	    MY_PRINT_MASK(FILE, mask, MY_GREG_OFF);		\
	    fprintf(FILE, ",@-sp\n");				\
	} else {						\
	    while (nregs--) {					\
		fprintf(FILE, "\tmov.w ");			\
		MY_PRINT_ONEREG_H(FILE, mask);			\
		fprintf(FILE, ",@-sp\n");			\
	    }							\
	}							\
    }								\
  }								\
  mask = 0;							\
  for (regno = 16; regno < 32; regno++)				\
        if (regs_ever_live[regno] && !call_used_regs[regno])	\
            mask |= 1 << (regno - 16);				\
  if (mask != 0) {						\
	fprintf(FILE, "\tfstm.w ");				\
	MY_PRINT_MASK(FILE, mask, MY_FREG_OFF);			\
	fprintf(FILE, ",@-sp\n", mask);				\
  }								\
}


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
/* ??? M.Yuhara */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tmova @LP%d,r0\n\tjsr mcount\n", (LABELNO))

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tcmp #0,@LPBX0\n\tbne LPI%d\n\tpusha @LPBX0\n\tjsr ___bb_init_func\n\tadd #4,sp\nLPI%d:\n",  \
	   LABELNO, LABELNO);

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)	\
  fprintf (FILE, "\tadd #1,@(LPBX2+%d)\n", 4 * BLOCKNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer (when
   frame_pinter_needed)  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

/* The Gmicro FPU seems to be unable to fldm/fstm double or single
   floating. It only allows extended !! */
/* Optimization is not enough, especially FREGs load !! M.Yuhara */

#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno;						\
  register int mask, fmask;					\
  register int nregs, nfregs;					\
  int offset, foffset;						\
  extern char call_used_regs[];					\
  static char *reg_names[] = REGISTER_NAMES;			\
  int fsize = ((SIZE) + 3) & -4;				\
  FUNCTION_EXTRA_EPILOGUE (FILE, SIZE);				\
  nfregs = 0;  fmask = 0; 					\
  for (regno = 16; regno < 31; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nfregs++; fmask |= 1 << (regno - 16); }			\
  foffset = nfregs * 12;					\
  nregs = 0;  mask = 0;						\
  if (frame_pointer_needed) regs_ever_live[FRAME_POINTER_REGNUM] = 0; \
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; mask |= 1 << regno; }				\
  if (frame_pointer_needed) {					\
    offset = nregs * 4 + fsize;					\
    if (nfregs > 0) {						\
	fprintf(FILE, "\tfldm.x @(%d,fp),", -(foffset + offset));\
	MY_PRINT_MASK(FILE, fmask, MY_FREG_OFF);		\
	fprintf(FILE, "\n");					\
    }								\
    if (nregs > 4						\
	|| current_function_pops_args) {			\
	fprintf(FILE, "\tmova @(%d,fp),sp\n", -offset);		\
	fprintf(FILE, "\texitd ");				\
	MY_PRINT_MASK(FILE, mask, MY_GREG_OFF);			\
	fprintf(FILE, ",#%d\n", current_function_pops_args);	\
    } else {							\
	while (nregs--) {					\
	    fprintf(FILE, "\tmov:l.w @(%d,fp),", -offset);	\
	    MY_PRINT_ONEREG_L(FILE, mask);			\
	    fprintf(FILE, "\n");				\
	    offset -= 4;					\
	}							\
	if (TARGET_NEWRETURN) {					\
	    fprintf(FILE, "\tmova.w @(4,fp),sp\n");		\
	    fprintf(FILE, "\tmov:l.w @fp,fp\n");		\
	} else {						\
	    fprintf(FILE, "\tmov.w fp,sp\n");			\
	    fprintf(FILE, "\tmov.w @sp+,fp\n");			\
	}							\
	fprintf(FILE, "\trts\n");				\
    }								\
  } else {							\
    if (nfregs > 0) {						\
	fprintf(FILE, "\tfldm.w @sp+,");			\
	MY_PRINT_MASK(FILE, fmask, MY_FREG_OFF);		\
	fprintf(FILE, "\n");					\
    }								\
    if (nregs > 4) {						\
	fprintf(FILE, "\tldm.w @sp+,");				\
	MY_PRINT_MASK(FILE, mask, MY_GREG_OFF);			\
	fprintf(FILE, "\n");					\
    } else {							\
	while (nregs--) {					\
	    fprintf(FILE, "\tmov.w @sp+,");			\
	    MY_PRINT_ONEREG_L(FILE,mask);			\
	    fprintf(FILE, "\n");				\
	}							\
    }								\
      if (current_function_pops_args) {				\
	myoutput_sp_adjust(FILE, "add", 			\
	    (fsize + 4 + current_function_pops_args));		\
	fprintf(FILE, "\tjmp @(%d,sp)\n", current_function_pops_args);\
    } else {							\
	if (fsize > 0)						\
	    myoutput_sp_adjust(FILE, "add", fsize);		\
	fprintf(FILE, "\trts\n");				\
    }								\
  }								\
}

/* This is a hook for other tm files to change.  */
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)

/* If the memory address ADDR is relative to the frame pointer,
   correct it to be relative to the stack pointer instead.
   This is for when we don't use a frame pointer.
   ADDR should be a variable name.  */

/* You have to change the next macro if you want to use more complex
   addressing modes (such as double indirection and  more than one
   chain-addressing stages). */

#define FIX_FRAME_POINTER_ADDRESS(ADDR,DEPTH)  \
{ int offset = -1;							\
  rtx regs = stack_pointer_rtx;						\
  if (ADDR == frame_pointer_rtx)					\
    offset = 0;								\
  else if (GET_CODE (ADDR) == PLUS && XEXP (ADDR, 0) == frame_pointer_rtx \
	   && GET_CODE (XEXP (ADDR, 1)) == CONST_INT)			\
    offset = INTVAL (XEXP (ADDR, 1));					\
  else if (GET_CODE (ADDR) == PLUS && XEXP (ADDR, 0) == frame_pointer_rtx) \
    { rtx other_reg = XEXP (ADDR, 1);					\
      offset = 0;							\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  else if (GET_CODE (ADDR) == PLUS && XEXP (ADDR, 1) == frame_pointer_rtx) \
    { rtx other_reg = XEXP (ADDR, 0);					\
      offset = 0;							\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  else if (GET_CODE (ADDR) == PLUS					\
	   && GET_CODE (XEXP (ADDR, 0)) == PLUS				\
	   && XEXP (XEXP (ADDR, 0), 0) == frame_pointer_rtx		\
	   && GET_CODE (XEXP (ADDR, 1)) == CONST_INT)			\
    { rtx other_reg = XEXP (XEXP (ADDR, 0), 1);				\
      offset = INTVAL (XEXP (ADDR, 1));					\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  else if (GET_CODE (ADDR) == PLUS					\
	   && GET_CODE (XEXP (ADDR, 0)) == PLUS				\
	   && XEXP (XEXP (ADDR, 0), 1) == frame_pointer_rtx		\
	   && GET_CODE (XEXP (ADDR, 1)) == CONST_INT)			\
    { rtx other_reg = XEXP (XEXP (ADDR, 0), 0);				\
      offset = INTVAL (XEXP (ADDR, 1));					\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  if (offset >= 0)							\
    { int regno;							\
      extern char call_used_regs[];					\
      for (regno = 16; regno < 32; regno++)				\
        if (regs_ever_live[regno] && ! call_used_regs[regno])		\
          offset += 12;							\
      for (regno = 0; regno < 16; regno++)				\
	if (regs_ever_live[regno] && ! call_used_regs[regno])		\
	  offset += 4;							\
      offset -= 4;							\
      ADDR = plus_constant (regs, offset + (DEPTH)); } }

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

/* Gmicro */
#define REGNO_OK_FOR_GREG_P(REGNO) \
((REGNO) < 16 || (unsigned) reg_renumber[REGNO] < 16)
#define REGNO_OK_FOR_FPU_P(REGNO) \
(((REGNO) ^ 0x10) < 16 || (unsigned) (reg_renumber[REGNO] ^ 0x10) < 16)

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_GREG_P(REGNO)
#define REGNO_OK_FOR_BASE_P(REGNO) REGNO_OK_FOR_GREG_P(REGNO)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the Gmicro, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fpu register.  */

#define FPU_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FPU_P (REGNO (X)))

/* I used GREG_P in the gmicro.md file. */

#ifdef REG_OK_STRICT
#define GREG_P(X) (REG_P (X) && REGNO_OK_FOR_GREG_P (REGNO(X)))
#else
#define GREG_P(X) (REG_P (X) && ((REGNO (X) & ~0xf) != 0x10))
#endif

/* Maximum number of registers that can appear in a valid memory address.  */

/* The Gmicro allows more registers in the chained addressing mode.
   But I do not know gcc supports such an architecture. */

#define MAX_REGS_PER_ADDRESS 2

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
#define REG_OK_FOR_INDEX_P(X) ((REGNO (X) & ~0xf) != 0x10)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) ((REGNO (X) & ~0xf) != 0x10)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* The gcc uses the following effective address of the Gmicro.
					       (without using PC!!).
   {@} ( {Rbase} + {Disp} + {Rindex * [1,2,4,8]} )
	where
		@:     memory indirection.
		Rbase: Base Register = General Register.
		Disp:  Displacement (up to 32bits)
		Rindex: Index Register = General Register.
		[1,2,4,8]: Scale of Index. 1 or 2 or 4 or 8.
		The inside of { } can be omitted.
    This restricts the chained addressing up to 1 stage.  */



/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

#define REG_CODE_BASE_P(X) \
  (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))

#define REG_CODE_INDEX_P(X) \
  (GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))

/* GET_CODE(X) must be PLUS. This macro does not check for PLUS! */
#define BASE_PLUS_DISP_P(X) \
   ( REG_CODE_BASE_P (XEXP (X, 0)) 		\
     && CONSTANT_ADDRESS_P (XEXP (X, 1))	\
    ||						\
     REG_CODE_BASE_P (XEXP (X, 1)) 		\
     && CONSTANT_ADDRESS_P (XEXP (X, 0)) )

/* 1 if X is {0,Rbase} + {0,disp}.  */
#define BASED_ADDRESS_P(X)  \
  (CONSTANT_ADDRESS_P (X)	\
   || REG_CODE_BASE_P (X)	\
   || (GET_CODE (X) == PLUS)	\
       && BASE_PLUS_DISP_P (X))

/* 1 if X is 1 or 2 or 4 or 8. GET_CODE(X) must be CONST_INT. */
#define SCALE_OF_INDEX_P(X) \
  ( INTVAL(X) == 4	\
    || INTVAL(X) == 2	\
    || INTVAL(X) == 8	\
    || INTVAL(X) == 1 )

/* #define INDEX_TERM_P(X,MODE)  */
#define INDEX_TERM_P(X)  \
  ( REG_CODE_INDEX_P(X)					\
    || (GET_CODE (X) == MULT				\
	&& ( (xfoo0 = XEXP (X, 0)), (xfoo1 = XEXP(X, 1)), \
	     ( ( (GET_CODE (xfoo0) == CONST_INT)	\
	       && SCALE_OF_INDEX_P (xfoo0)		\
	       && REG_CODE_INDEX_P (xfoo1) )		\
	      ||					\
	       ( (GET_CODE (xfoo1) == CONST_INT)		\
	       && SCALE_OF_INDEX_P (xfoo1)		\
	       && REG_CODE_INDEX_P (xfoo0) ) ))))

/* Assumes there are no cases such that X = (Ireg + Disp) + Disp */
#define BASE_DISP_INDEX_P(X)  \
  ( BASED_ADDRESS_P (X)							\
   || ( (GET_CODE (X) == PLUS)						\
      && ( ( (xboo0 = XEXP (X, 0)), (xboo1 = XEXP (X, 1)),		\
	  (REG_CODE_BASE_P (xboo0)					\
	    && (GET_CODE (xboo1) == PLUS)				\
	    && ( ( CONSTANT_ADDRESS_P (XEXP (xboo1, 0))			\
		   && INDEX_TERM_P (XEXP (xboo1, 1)) )			\
	         || ( CONSTANT_ADDRESS_P (XEXP (xboo1, 1))		\
		     && INDEX_TERM_P (XEXP (xboo1, 0))) )))		\
	 ||								\
	  (CONSTANT_ADDRESS_P (xboo0)					\
	    && (GET_CODE (xboo1) == PLUS)				\
	    && ( ( REG_CODE_BASE_P (XEXP (xboo1, 0))			\
	 	   && INDEX_TERM_P (XEXP (xboo1, 1)) )			\
	         || ( REG_CODE_BASE_P (XEXP (xboo1, 1))			\
		      && INDEX_TERM_P (XEXP (xboo1, 0))) ))		\
	||								\
	  (INDEX_TERM_P (xboo0)						\
	    && ( ( (GET_CODE (xboo1) == PLUS)				\
	          && ( ( REG_CODE_BASE_P (XEXP (xboo1, 0))		\
		      && CONSTANT_ADDRESS_P (XEXP (xboo1, 1)) )		\
	               || ( REG_CODE_BASE_P (XEXP (xboo1, 1))   	\
		      && CONSTANT_ADDRESS_P (XEXP (xboo1, 0))) ))	\
		||							\
		 (CONSTANT_ADDRESS_P (xboo1))				\
		||							\
		 (REG_CODE_BASE_P (xboo1)) )))))

/*
	If you want to allow double-indirection,
	you have to change the <fp-relative> => <sp-relative> conversion
	routine. M.Yuhara

#ifdef REG_OK_STRICT
#define DOUBLE_INDIRECTION(X,ADDR) {\
    if (BASE_DISP_INDEX_P (XEXP (XEXP (X, 0), 0) )) goto ADDR; \
    }
#else
#define DOUBLE_INDIRECTION(X,ADDR) { }
#endif
*/


#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) {\
  register rtx xboo0, xboo1, xfoo0, xfoo1;		\
  if (GET_CODE (X) == MEM) { 				\
    /*							\
    if (GET_CODE (XEXP (X,0)) == MEM) {			\
	DOUBLE_INDIRECTION(X,ADDR);			\
    } else {						\
	if (BASE_DISP_INDEX_P (XEXP (X, 0))) goto ADDR;	\
    }							\
    */							\
  } else {						\
	if (BASE_DISP_INDEX_P (X)) goto ADDR;		\
	if ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_INC)	\
	    && REG_P (XEXP (X, 0))					\
	    && (REGNO (XEXP (X, 0)) == STACK_POINTER_REGNUM))		\
		goto ADDR;						\
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
   opportunities to optimize the output.

   For the Gmicro, nothing is done now. */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN) {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the VAX, the predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand)
   and all indexed address depend thus (because the index scale factor
   is the length of the operand).
   The Gmicro mimics the VAX now. Since ADDE is legitimate, it cannot
   include auto-inc/dec. */

/* Unnecessary ??? */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 { if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC)	\
     goto LABEL; }


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
/* #define CASE_VECTOR_MODE HImode */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

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

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

/* #define STORE_FLAG_VALUE -1 */

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

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if ((unsigned) INTVAL (RTX) < 8) return 0;			\
    if ((unsigned) (INTVAL (RTX) + 0x80) < 0x100) return 1;	\
    if ((unsigned) (INTVAL (RTX) + 0x8000) < 0x10000) return 2;	\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    return 5;

/* Define subroutines to call to handle multiply and divide.
   The `*' prevents an underscore from being prepended by the compiler.  */
/* Use libgcc on Gmicro */
/* #define UDIVSI3_LIBCALL "*udiv" */
/* #define UMODSI3_LIBCALL "*urem" */


/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Set if the cc value is actually in the FPU, so a floating point
   conditional branch must be output.  */
#define CC_IN_FPU 04000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* Since Gmicro's compare instructions depend on the branch condition,
   all branch should be kept.
   More work must be done to optimize condition code !! M.Yuhara */

#define NOTICE_UPDATE_CC(EXP, INSN) {CC_STATUS_INIT;}

/* The skeleton of the next macro is taken from "vax.h".
   FPU-reg manipulation is added.  M.Yuhara */
/* Now comment out.
#define NOTICE_UPDATE_CC(EXP, INSN) {	\
  if (GET_CODE (EXP) == SET) {					\
      if ( !FPU_REG_P (XEXP (EXP, 0))				\
	  && (XEXP (EXP, 0) != cc0_rtx)				\
	  && (FPU_REG_P (XEXP (EXP, 1))				\
	      || GET_CODE (XEXP (EXP, 1)) == FIX		\
	      || GET_CODE (XEXP (EXP, 1)) == FLOAT_TRUNCATE	\
	      || GET_CODE (XEXP (EXP, 1)) == FLOAT_EXTEND)) {	\
	 CC_STATUS_INIT;					\
      } else if (GET_CODE (SET_SRC (EXP)) == CALL) {		\
	 CC_STATUS_INIT;					\
      } else if (GET_CODE (SET_DEST (EXP)) != PC) {		\
	  cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (EXP);			\
	  cc_status.value2 = SET_SRC (EXP);			\
      }								\
  } else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET		\
	   && GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) != PC) {\
      cc_status.flags = 0;					\
      cc_status.value1 = SET_DEST (XVECEXP (EXP, 0, 0));	\
      cc_status.value2 = SET_SRC (XVECEXP (EXP, 0, 0)); 	\
  /* PARALLELs whose first element sets the PC are aob, sob VAX insns.	\
     They do change the cc's.  So drop through and forget the cc's. * / \
  } else CC_STATUS_INIT;						\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))	\
    cc_status.value2 = 0;					\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM	\
      && cc_status.value2					\
      && GET_CODE (cc_status.value2) == MEM)			\
    cc_status.value2 = 0;					\
  if ( (cc_status.value1 && FPU_REG_P (cc_status.value1))	\
      || (cc_status.value2 && FPU_REG_P (cc_status.value2)))	\
    cc_status.flags = CC_IN_FPU;				\
}
*/

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  \
{ if (cc_prev_status.flags & CC_IN_FPU)		\
    return FLOAT;				\
  if (cc_prev_status.flags & CC_NO_OVERFLOW)	\
    return NO_OV;				\
  return NORMAL; }

/* Control the assembler format that we output.  */

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".section text,code,align=4"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".section data,data,align=4"

/* Output before uninitialized data. */

#define BSS_SECTION_ASM_OP ".section bss,data,align=4"

#define EXTRA_SECTIONS in_bss

#define EXTRA_SECTION_FUNCTIONS	\
void								\
bss_section ()							\
{								\
    if (in_section != in_bss) {					\
	fprintf (asm_out_file, "%s\n", BSS_SECTION_ASM_OP);	\
	in_section = in_bss;					\
    }								\
}

/* Output at beginning of assembler file.
   It is not appropriate for this to print a list of the options used,
   since that's not the convention that we use.  */

#define ASM_FILE_START(FILE)

/* Output at the end of assembler file. */

#define ASM_FILE_END(FILE)  fprintf (FILE, "\t.end\n");


/* Don't try to define `gcc_compiled.' since the assembler do not
   accept symbols with periods and GDB doesn't run on this machine anyway.  */
#define ASM_IDENTIFY_GCC(FILE)


/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""
/* #define ASM_APP_ON "#APP\n" */

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""
/* #define ASM_APP_OFF ";#NO_APP\n" */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",	\
 "r8", "r9", "r10", "r11", "r12", "r13", "fp", "sp",	\
 "fr0", "fr1", "fr2", "fr3", "fr4", "fr5", "fr6", "fr7", \
 "fr8", "fr9", "fr10", "fr11", "fr12", "fr13", "fr14", "fr15"}

/* How to renumber registers for dbx and gdb. */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Define this if gcc should produce debugging output for dbx in response
   to the -g flag. This does not work for the Gmicro now */

#define DBX_DEBUGGING_INFO

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME) {	\
    assemble_name (FILE, NAME);	\
    fputs (":\n", FILE);	\
}

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME) {\
    fputs ("\t.global ", FILE);	\
    assemble_name (FILE, NAME);	\
    fputs ("\n", FILE);		\
}

/* This is how to output a command to make the external label named NAME
   which are not defined in the file to be referable */
/* ".import" does not work ??? */

#define ASM_OUTPUT_EXTERNAL(FILE,DECL,NAME) { \
    fputs ("\t.global ", FILE);	\
    assemble_name (FILE, NAME);	\
    fputs ("\n", FILE);		\
}


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

/* do {...} while(0) is necessary, because these macros are used as
    if (xxx) MACRO; else ....
		  ^
*/


#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
do { union { double d; long l[2];} tem;					\
     tem.d = (VALUE);							\
     fprintf (FILE, "\t.fdata.d h'%x%08x.d\n", tem.l[0], tem.l[1]);	\
} while(0)


/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { union { float f; long l;} tem;			\
     tem.f = (VALUE);					\
     fprintf (FILE, "\t.fdata.s h'%x.s\n", tem.l);	\
} while(0)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.data.w "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.data.h "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.data.b "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.data.b h'%x\n", (VALUE))

#define ASM_OUTPUT_ASCII(FILE,P,SIZE)  \
  output_ascii ((FILE), (P), (SIZE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmov %s,@-sp\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmov @sp+,%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
   (The Gmicro does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.data.w L%d\n", VALUE)


/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.data.w L%d-L%d\n", VALUE, REL)


/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  fprintf (FILE, "\t.align %d\n", (1 << (LOG)));

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.res.b %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( bss_section (),				\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ":\t.res.b %d\n", (ROUNDED)),\
  fprintf ((FILE), "\t.export "),		\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), "\n") )

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( bss_section (),				\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ":\t.res.b %d\n", (ROUNDED)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

/* $__ is unique ????? M.Yuhara */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "$__%s%d", (NAME), (LABELNO)))

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

/* Output a float value (represented as a C double) as an immediate operand.
   This macro is a Gmicro/68k-specific macro.  */

#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)	\
do { union { float f; long l;} tem;		\
  tem.f = (VALUE);				\
  fprintf (FILE, "#h'%x.s", tem.l);		\
} while(0)


/* Output a double value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)	\
do { union { double d; long l[2];} tem;		\
  tem.d = (VALUE);				\
  fprintf (FILE, "#h'%x%08x.d", tem.l[0], tem.l[1]);	\
} while(0)

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the Gmicro, we use several CODE characters:
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'b' for branch target label.
   '-' for an operand pushing on the stack.
   '+' for an operand pushing on the stack.
   '#' for an immediate operand prefix 
*/

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)	\
  ( (CODE) == '#' || (CODE) == '-'		\
     || (CODE) == '+' || (CODE) == '@' || (CODE) == '!')


#define PRINT_OPERAND(FILE, X, CODE)  \
{ int i;								\
  static char *reg_name[] = REGISTER_NAMES;				\
/* fprintf (stderr, "PRINT_OPERAND CODE=%c(0x%x), ", CODE, CODE);\
myprcode(GET_CODE(X)); */	\
  if (CODE == '#') fprintf (FILE, "#");					\
  else if (CODE == '-') fprintf (FILE, "@-sp");				\
  else if (CODE == '+') fprintf (FILE, "@sp+");				\
  else if (CODE == 's') fprintf (stderr, "err: PRINT_OPERAND <s>\n"); 	\
  else if (CODE == '!') fprintf (stderr, "err: PRINT_OPERAND <!>\n"); 	\
  else if (CODE == '.') fprintf (stderr, "err: PRINT_OPERAND <.>\n"); 	\
  else if (CODE == 'b') {						\
    if (GET_CODE (X) == MEM)						\
	output_addr_const (FILE, XEXP (X, 0));  /* for bsr */		\
    else								\
	output_addr_const (FILE, X);  /* for bcc */			\
  }									\
  else if (CODE == 'p')							\
    print_operand_address (FILE, X);					\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_name[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      u1.f = u.d;							\
      if (CODE == 'f')							\
	ASM_OUTPUT_FLOAT_OPERAND (FILE, u1.f);				\
      else								\
	fprintf (FILE, "#h'%x", u1.i); }				\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == DFmode)	\
    { union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      ASM_OUTPUT_DOUBLE_OPERAND (FILE, u.d); }				\
  else { putc ('#', FILE);						\
output_addr_const (FILE, X); }}

/* Note that this contains a kludge that knows that the only reason
   we have an address (plus (label_ref...) (reg...))
   is in the insn before a tablejump, and we know that m68k.md
   generates a label LInnn: on such an insn.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
    { print_operand_address (FILE, ADDR); }

/*
Local variables:
version-control: t
End:
*/
