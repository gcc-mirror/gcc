/* Definitions of target machine for GNU compiler, for Acorn RISC Machine.
   Copyright (C) 1991, 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rwe11@cl.cam.ac.uk)
   
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

/* Sometimes the directive `riscos' is checked.  This does not imply that this
   tm file can be used unchanged to build a GCC for RISC OS.
   (Since in fact, it can't.)  */

extern void output_func_prologue ();
extern void output_func_epilogue ();
extern char *output_add_immediate ();
extern char *output_call ();
extern char *output_call_mem ();
extern char *output_move_double ();
extern char *output_mov_double_fpu_from_arm ();
extern char *output_mov_double_arm_from_fpu ();
extern char *output_mov_long_double_fpu_from_arm ();
extern char *output_mov_long_double_arm_from_fpu ();
extern char *output_mov_long_double_arm_from_arm ();
extern char *output_mov_immediate ();
extern char *output_multi_immediate ();
extern char *output_return_instruction ();
extern char *output_load_symbol ();
extern char *fp_immediate_constant ();
extern struct rtx_def *gen_compare_reg ();
extern struct rtx_def *arm_gen_store_multiple ();
extern struct rtx_def *arm_gen_load_multiple ();

extern char *arm_condition_codes[];

/* This is needed by the tail-calling peepholes */
extern int frame_pointer_needed;


#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES  "-Darm -Acpu(arm) -Amachine(arm)"
#endif

#ifndef CPP_SPEC
#define CPP_SPEC "%{m6:-D__arm6__}"
#endif

/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION  \
  fputs (" (ARM/generic)", stderr);
#endif

/* Run-time compilation parameters selecting different hardware subsets.
   On the ARM, misuse it in a different way.  */
extern int target_flags;

/* Nonzero if the function prologue (and epilogue) should obey
   the ARM Procedure Call Standard.  */
#define TARGET_APCS	(target_flags & 1)

/* Nonzero if the function prologue should output the function name to enable
   the post mortem debugger to print a backtrace (very useful on RISCOS,
   unused on RISCiX).  Specifying this flag also enables -mapcs.
   XXX Must still be implemented in the prologue.  */
#define TARGET_POKE_FUNCTION_NAME	(target_flags & 2)

/* Nonzero if floating point instructions are emulated by the FPE, in which
   case instruction scheduling becomes very uninteresting.  */
#define TARGET_FPE	(target_flags & 4)

/* Nonzero if destined for an ARM6xx.  Takes out bits that assume restoration
   of condition flags when returning from a branch & link (ie. a function) */
#define TARGET_6        (target_flags & 8)

/* Leave some bits for new processor variants */

/* Nonzero if shorts must be loaded byte at a time.  This is not necessary
   for the arm processor chip, but it is needed for some MMU chips.  */
#define TARGET_SHORT_BY_BYTES	(target_flags & 0x200)

/* Nonzero if GCC should use a floating point library.
   GCC will assume the fp regs don't exist and will not emit any fp insns.
   Note that this is different than fp emulation which still uses fp regs
   and insns - the kernel catches the trap and performs the operation.  */
#define TARGET_SOFT_FLOAT	(target_flags & 0x400)
#define TARGET_HARD_FLOAT	(! TARGET_SOFT_FLOAT)

/* SUBTARGET_SWITCHES is used to add flags on a per-config basis.
   Bit 31 is reserved.  See riscix.h.  */
#ifndef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES
#endif

#define TARGET_SWITCHES  				\
{                         				\
  {"apcs",		 	 1},			\
  {"poke-function-name", 	 2},			\
  {"fpe",		 	 4},			\
  {"6",				 8},			\
  {"2",				-8},			\
  {"3",				-8},			\
  {"short-load-bytes",		 (0x200)},		\
  {"no-short-load-bytes",	-(0x200)},		\
  {"short-load-words", 		-(0x200)},		\
  {"no-short-load-words",	 (0x200)},		\
  {"soft-float",		 (0x400)},		\
  {"hard-float",		-(0x400)},		\
  SUBTARGET_SWITCHES					\
  {"",   		 	 TARGET_DEFAULT }	\
}

/* Which processor we are running on.  Currently this is only used to
   get the condition code clobbering attribute right when we are running on
   an arm 6 */

enum processor_type 
{
  PROCESSOR_ARM2,
  PROCESSOR_ARM3,
  PROCESSOR_ARM6
};

/* Recast the cpu class to be the cpu attribute. */

/* Recast the cpu class to be the cpu attribute.  */
#define arm_cpu_attr ((enum attr_cpu)arm_cpu)

extern enum processor_type arm_cpu;

/* What sort of floating point unit do we have? Hardware or software.  */
enum floating_point_type
{
  FP_HARD,
  FP_SOFT
};

/* Recast the floating point class to be the floating point attribute.  */
#define arm_fpu_attr ((enum attr_fpu) arm_fpu)

extern enum floating_point_type arm_fpu;

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT  0
#endif

#define TARGET_MEM_FUNCTIONS 1

/* OVERRIDE_OPTIONS takes care of the following:
   - if -mpoke-function-name, then -mapcs.
   - if doing debugging, then -mapcs; if RISCOS, then -mpoke-function-name.
   - if floating point is done by emulation, forget about instruction
     scheduling.  Note that this only saves compilation time; it doesn't
     matter for the final code.  */

#define OVERRIDE_OPTIONS  \
{								\
  if (write_symbols != NO_DEBUG && flag_omit_frame_pointer)	\
    warning ("-g without a frame pointer may not give sensible debugging");\
  if (TARGET_POKE_FUNCTION_NAME)				\
    target_flags |= 1;						\
  if (TARGET_FPE)						\
    flag_schedule_insns = flag_schedule_insns_after_reload = 0;	\
  arm_cpu = TARGET_6 ? PROCESSOR_ARM6: PROCESSOR_ARM2;		\
}

/* Target machine storage Layout.  */


/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

/* It is far faster to zero extend chars than to sign extend them */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)      	\
    {						\
      if (MODE == QImode)			\
	UNSIGNEDP = 1;				\
      else if (MODE == HImode)			\
	UNSIGNEDP = TARGET_SHORT_BY_BYTES != 0;	\
      (MODE) = SImode;				\
    }

/* Define for XFmode extended real floating point support.
   This will automatically cause REAL_ARITHMETIC to be defined.  */
/* For the ARM:
   I think I have added all the code to make this work.  Unfortunately,
   early releases of the floating point emulation code on RISCiX used a
   different format for extended precision numbers.  On my RISCiX box there
   is a bug somewhere which causes the machine to lock up when running enquire
   with long doubles.  There is the additional aspect that Norcroft C
   treats long doubles as doubles and we ought to remain compatible.
   Perhaps someone with an FPA coprocessor and not running RISCiX would like
   to try this someday. */
/* #define LONG_DOUBLE_TYPE_SIZE 96 */

/* Disable XFmode patterns in md file */
#define ENABLE_XF_PATTERNS 0

/* Define if you don't want extended real, but do want to use the
   software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
/* See comment above */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  
   Most ARM processors are run in little endian mode, so that is the default.
   If you want to have it run-time selectable, change the definition in a
   cover file to be TARGET_BIG_ENDIAN.  */
#define BYTES_BIG_ENDIAN  0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN  0

/* Define this if most significant word of doubles is the lowest numbered */
#define FLOAT_WORDS_BIG_ENDIAN 1

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT  8

#define BITS_PER_WORD  32

#define UNITS_PER_WORD	4

#define POINTER_SIZE  32

#define PARM_BOUNDARY  	32

#define STACK_BOUNDARY  32

#define FUNCTION_BOUNDARY  32

#define EMPTY_FIELD_BOUNDARY  32

#define BIGGEST_ALIGNMENT  32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST        \
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Every structures size must be a multiple of 32 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 32

/* Non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* Define number of bits in most basic integer type.
   (If undefined, default is BITS_PER_WORD).  */
/* #define INT_TYPE_SIZE */

/* Standard register usage.  */

/* Register allocation in ARM Procedure Call Standard (as used on RISCiX):
   (S - saved over call).

	r0	   *	argument word/integer result
	r1-r3		argument word

	r4-r8	     S	register variable
	r9	     S	(rfp) register variable (real frame pointer)

	r10  	   F S	(sl) stack limit (not currently used)
	r11 	   F S	(fp) argument pointer
	r12		(ip) temp workspace
	r13  	   F S	(sp) lower end of current stack frame
	r14		(lr) link address/workspace
	r15	   F	(pc) program counter

	f0		floating point result
	f1-f3		floating point scratch

	f4-f7	     S	floating point variable

	cc		This is NOT a real register, but is used internally
	                to represent things that use or set the condition
			codes.
	sfp             This isn't either.  It is used during rtl generation
	                since the offset between the frame pointer and the
			auto's isn't known until after register allocation.
	afp		Nor this, we only need this because of non-local
	                goto.  Without it fp appears to be used and the
			elimination code won't get rid of sfp.  It tracks
			fp exactly at all times.

   *: See CONDITIONAL_REGISTER_USAGE  */

/* The stack backtrace structure is as follows:
  fp points to here:  |  save code pointer  |      [fp]
                      |  return link value  |      [fp, #-4]
                      |  return sp value    |      [fp, #-8]
                      |  return fp value    |      [fp, #-12]
                     [|  saved r10 value    |]
                     [|  saved r9 value     |]
                     [|  saved r8 value     |]
                     [|  saved r7 value     |]
                     [|  saved r6 value     |]
                     [|  saved r5 value     |]
                     [|  saved r4 value     |]
                     [|  saved r3 value     |]
                     [|  saved r2 value     |]
                     [|  saved r1 value     |]
                     [|  saved r0 value     |]
                     [|  saved f7 value     |]     three words
                     [|  saved f6 value     |]     three words
                     [|  saved f5 value     |]     three words
                     [|  saved f4 value     |]     three words
  r0-r3 are not normally saved in a C function.  */

/* The number of hard registers is 16 ARM + 8 FPU + 1 CC + 1 SFP.  */
#define FIRST_PSEUDO_REGISTER  27

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS  \
{                        \
  0,0,0,0,0,0,0,0,	 \
  0,0,1,1,0,1,0,1,	 \
  0,0,0,0,0,0,0,0,	 \
  1,1,1			 \
}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.
   The CC is not preserved over function calls on the ARM 6, so it is 
   easier to assume this for all.  SFP is preserved, since FP is. */
#define CALL_USED_REGISTERS  \
{                            \
  1,1,1,1,0,0,0,0,	     \
  0,0,1,1,1,1,1,1,	     \
  1,1,1,1,0,0,0,0,	     \
  1,1,1			     \
}

/* If doing stupid life analysis, avoid a bug causing a return value r0 to be
   trampled.  This effectively reduces the number of available registers by 1.
   XXX It is a hack, I know.
   XXX Is this still needed?  */
#define CONDITIONAL_REGISTER_USAGE  \
{							\
  if (obey_regdecls)					\
    fixed_regs[0] = 1;					\
  if (TARGET_SOFT_FLOAT)				\
    {							\
      int regno;					\
      for (regno = 16; regno < 24; ++regno)		\
	fixed_regs[regno] = call_used_regs[regno] = 1;	\
    }							\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the ARM regs are UNITS_PER_WORD bits wide; FPU regs can hold any FP
   mode.  */
#define HARD_REGNO_NREGS(REGNO, MODE)  					\
    (((REGNO) >= 16 && REGNO != FRAME_POINTER_REGNUM			\
      && (REGNO) != ARG_POINTER_REGNUM) ? 1				\
     : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   This is TRUE for ARM regs since they can hold anything, and TRUE for FPU
   regs holding FP.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)  			\
  ((GET_MODE_CLASS (MODE) == MODE_CC) ? (REGNO == CC_REGNUM) :	\
  ((REGNO) < 16 || REGNO == FRAME_POINTER_REGNUM		\
   || REGNO == ARG_POINTER_REGNUM				\
   || GET_MODE_CLASS (MODE) == MODE_FLOAT))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  \
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Define this if the program counter is overloaded on a register.  */
#define PC_REGNUM		15

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM	13

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM	25

/* Define this to be where the real frame pointer is if it is not possible to
   work out the offset between the frame pointer and the automatic variables
   until after register allocation has taken place.  FRAME_POINTER_REGNUM
   should point to a special register that we will make sure is eliminated. */
#define HARD_FRAME_POINTER_REGNUM 11

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.  
   If we have to have a frame pointer we might as well make use of it.
   APCS says that the frame pointer does not need to be pushed in leaf
   functions.  */
#define FRAME_POINTER_REQUIRED		\
  (current_function_has_nonlocal_label || (TARGET_APCS && !leaf_function_p ()))

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	26

/* The native (Norcroft) Pascal compiler for the ARM passes the static chain
   as an invisible last argument (possible since varargs don't exist in
   Pascal), so the following is not true.  */
#define STATIC_CHAIN_REGNUM	8

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM	0

/* Internal, so that we don't need to refer to a raw number */
#define CC_REGNUM		24

/* The order in which register should be allocated.  It is good to use ip
   since no saving is required (though calls clobber it) and it never contains
   function parameters.  It is quite good to use lr since other calls may
   clobber it anyway.  Allocate r0 through r3 in reverse order since r3 is 
   least likely to contain a function parameter; in addition results are
   returned in r0.
   */
#define REG_ALLOC_ORDER  \
{                                   \
    3, 2, 1, 0, 12, 14,	4, 5,       \
    6, 7, 8, 10, 9, 11, 13, 15,     \
    16, 17, 18, 19, 20, 21, 22, 23, \
    24, 25			    \
}

/* Register and constant classes.  */

/* Register classes: all ARM regs or all FPU regs---simple! */
enum reg_class
{
  NO_REGS,
  FPU_REGS,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES  \
{			\
  "NO_REGS",		\
  "FPU_REGS",		\
  "GENERAL_REGS",	\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */
#define REG_CLASS_CONTENTS  \
{				\
  0x0000000, /* NO_REGS  */	\
  0x0FF0000, /* FPU_REGS */	\
  0x200FFFF, /* GENERAL_REGS */	\
  0x2FFFFFF  /* ALL_REGS */	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
#define REGNO_REG_CLASS(REGNO)  			\
  (((REGNO) < 16 || REGNO == FRAME_POINTER_REGNUM	\
    || REGNO == ARG_POINTER_REGNUM)			\
   ? GENERAL_REGS : (REGNO) == CC_REGNUM		\
   ? NO_REGS : FPU_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  GENERAL_REGS
#define BASE_REG_CLASS	GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.
   We only need constraint `f' for FPU_REGS (`r' == GENERAL_REGS).  */
#define REG_CLASS_FROM_LETTER(C)  \
  ((C)=='f' ? FPU_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.
	I: immediate arithmetic operand (i.e. 8 bits shifted as required).
	J: valid indexing constants.  
	K: ~value ok in rhs argument of data operand.
	L: -value ok in rhs argument of data operand. 
        M: 0..32, or a power of 2  (for shifts, or mult done by shift).  */
#define CONST_OK_FOR_LETTER_P(VALUE, C)  		\
  ((C) == 'I' ? const_ok_for_arm (VALUE) :		\
   (C) == 'J' ? ((VALUE) < 4096 && (VALUE) > -4096) :	\
   (C) == 'K' ? (const_ok_for_arm (~(VALUE))) :		\
   (C) == 'L' ? (const_ok_for_arm (-(VALUE))) :		\
   (C) == 'M' ? (((VALUE >= 0 && VALUE <= 32))		\
		 || (((VALUE) & ((VALUE) - 1)) == 0))	\
   : 0)

/* For the ARM, `Q' means that this is a memory operand that is just
   an offset from a register.  
   `S' means any symbol that has the SYMBOL_REF_FLAG set or a CONSTANT_POOL
   address.  This means that the symbol is in the text segment and can be
   accessed without using a load. */

#define EXTRA_CONSTRAINT(OP, C)                                         \
  ((C) == 'Q' ? GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == REG  \
   : (C) == 'R' ? (GET_CODE (OP) == MEM					\
		   && GET_CODE (XEXP (OP, 0)) == SYMBOL_REF		\
		   && CONSTANT_POOL_ADDRESS_P (XEXP (OP, 0)))		\
   : (C) == 'S' ? (optimize > 0 && CONSTANT_ADDRESS_P (OP)) : 0)

/* Constant letter 'G' for the FPU immediate constants. 
   'H' means the same constant negated.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(X,C)			\
    ((C) == 'G' ? const_double_rtx_ok_for_fpu (X) 		\
     : (C) == 'H' ? neg_const_double_rtx_ok_for_fpu (X) : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS)  (CLASS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS,MODE,X)	\
  (((MODE) == DFmode && (CLASS) == GENERAL_REGS		\
    && true_regnum (X) == -1 && TARGET_HARD_FLOAT)	\
   ? GENERAL_REGS					\
   : ((MODE) == HImode && true_regnum (X) == -1) ? GENERAL_REGS : NO_REGS)

/* If we need to load shorts byte-at-a-time, then we need a scratch. */
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS,MODE,X)			\
  (((MODE) == HImode && TARGET_SHORT_BY_BYTES && true_regnum (X) == -1)	\
   ? GENERAL_REGS : NO_REGS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.
   ARM regs are UNITS_PER_WORD bits while FPU regs can hold any FP mode */
#define CLASS_MAX_NREGS(CLASS, MODE)  \
    ((CLASS) == FPU_REGS ? 1					       \
     : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Moves between FPU_REGS and GENERAL_REGS are two memory insns.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2)  \
  ((((CLASS1) == FPU_REGS && (CLASS2) != FPU_REGS)	\
    || ((CLASS2) == FPU_REGS && (CLASS1) != FPU_REGS))	\
   ? 20 : 2)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD  1

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET  0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
#define PUSH_ROUNDING(NPUSHED)  (((NPUSHED) + 3) & ~3)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  4

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the ARM, the caller does not pop any of its arguments that were passed
   on the stack.  */
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE)  0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  (GET_MODE_CLASS (TYPE_MODE (VALTYPE)) == MODE_FLOAT && TARGET_HARD_FLOAT \
   ? gen_rtx (REG, TYPE_MODE (VALTYPE), 16) \
   : gen_rtx (REG, TYPE_MODE (VALTYPE), 0))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)  \
  (GET_MODE_CLASS (MODE) == MODE_FLOAT && TARGET_HARD_FLOAT \
   ? gen_rtx (REG, MODE, 16) \
   : gen_rtx (REG, MODE, 0))

/* 1 if N is a possible register number for a function value.
   On the ARM, only r0 and f0 can return results.  */
#define FUNCTION_VALUE_REGNO_P(REGNO)  \
  ((REGNO) == 0 || ((REGNO) == 16) && TARGET_HARD_FLOAT)

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

   On the ARM, normally the first 16 bytes are passed in registers r0-r3; all
   other arguments are passed on the stack.  If (NAMED == 0) (which happens
   only in assign_parms, since SETUP_INCOMING_VARARGS is defined), say it is
   passed in the stack (function_prologue will indeed make it pass in the
   stack if necessary).  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)  \
  ((NAMED)						\
   ? ((CUM) >= 16 ? 0 : gen_rtx (REG, MODE, (CUM) / 4))	\
   : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)  \
  ((CUM) < 16 && 16 < (CUM) + ((MODE) != BLKmode            \
			       ? GET_MODE_SIZE (MODE)       \
			       : int_size_in_bytes (TYPE))  \
   ? 4 - (CUM) / 4 : 0)

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the
   type `int' suffices and can hold the number of bytes of argument so far.

   On the ARM, this is the number of bytes of arguments scanned so far.  */
#define CUMULATIVE_ARGS  int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.
   On the ARM, the offset starts at 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME)  \
  ((CUM) = (((FNTYPE) && aggregate_value_p (TREE_TYPE ((FNTYPE)))) ? 4 : 0))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)  \
  (CUM) += ((MODE) != BLKmode                       \
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3       \
	    : (int_size_in_bytes (TYPE) + 3) & ~3)  \

/* 1 if N is a possible register number for function argument passing.
   On the ARM, r0-r3 are used to pass args.  */
#define FUNCTION_ARG_REGNO_P(REGNO)  \
  ((REGNO) >= 0 && (REGNO) <= 3)

/* Perform any actions needed for a function that is receiving a variable
   number of arguments.  CUM is as above.  MODE and TYPE are the mode and type
   of the current parameter.  PRETEND_SIZE is a variable that should be set to
   the amount of stack that must be pushed by the prolog to pretend that our
   caller pushed it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.

   On the ARM, PRETEND_SIZE is set in order to have the prologue push the last
   named arg and all anonymous args onto the stack.
   XXX I know the prologue shouldn't be pushing registers, but it is faster
   that way.  */
#define SETUP_INCOMING_VARARGS(CUM, MODE, TYPE, PRETEND_SIZE, NO_RTL)  \
{									\
  extern int current_function_anonymous_args;				\
  current_function_anonymous_args = 1;					\
  if ((CUM) < 16)							\
    (PRETEND_SIZE) = 16 - (CUM);					\
}

/* Generate assembly output for the start of a function.  */
#define FUNCTION_PROLOGUE(STREAM, SIZE)  \
  output_func_prologue ((STREAM), (SIZE))

/* Call the function profiler with a given profile label.  The Acorn compiler
   puts this BEFORE the prolog but gcc pust it afterwards.  The ``mov ip,lr''
   seems like a good idea to stick with cc convention.  ``prof'' doesn't seem
   to mind about this!  */
#define FUNCTION_PROFILER(STREAM,LABELNO)  				    \
{									    \
    fprintf(STREAM, "\tmov\t%sip, %slr\n", REGISTER_PREFIX, REGISTER_PREFIX); \
    fprintf(STREAM, "\tbl\tmcount\n");					    \
    fprintf(STREAM, "\t.word\tLP%d\n", (LABELNO));			    \
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.

   On the ARM, the function epilogue recovers the stack pointer from the
   frame.  */
#define EXIT_IGNORE_STACK 1

/* Generate the assembly code for function exit. */
#define FUNCTION_EPILOGUE(STREAM, SIZE)  \
  output_func_epilogue ((STREAM), (SIZE))

/* Determine if the epilogue should be output as RTL.
   You should override this if you define FUNCTION_EXTRA_EPILOGUE.  */
#define USE_RETURN_INSN use_return_insn ()

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the ARM.  First, the
   arg pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the pseudo frame pointer register can always
   be eliminated; it is replaced with either the stack or the real frame
   pointer. */

#define ELIMINABLE_REGS					\
{{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 {ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   All eliminations are permissible.  Note that ARG_POINTER_REGNUM and
   HARD_FRAME_POINTER_REGNUM are in fact the same thing.  If we need a frame
   pointer, we must eliminate FRAME_POINTER_REGNUM into
   HARD_FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM.  */
#define CAN_ELIMINATE(FROM, TO)		\
  (((TO) == STACK_POINTER_REGNUM && frame_pointer_needed) ? 0 : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  int volatile_func = arm_volatile_func ();				\
  if ((FROM) == ARG_POINTER_REGNUM && (TO) == HARD_FRAME_POINTER_REGNUM)\
    (OFFSET) = 0;							\
  else if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)\
    (OFFSET) = (get_frame_size () + 3 & ~3);				\
  else									\
    {									\
      int regno;							\
      int offset = 12;							\
      int saved_hard_reg = 0;						\
									\
      if (! volatile_func)						\
        {								\
          for (regno = 0; regno <= 10; regno++)				\
	    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
	      saved_hard_reg = 1, offset += 4;				\
          for (regno = 16; regno <=23; regno++)				\
	    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
	      offset += 12;						\
	}								\
      if ((FROM) == FRAME_POINTER_REGNUM)				\
	(OFFSET) = -offset;						\
      else								\
	{								\
	   if (! frame_pointer_needed)					\
	     offset -= 16;						\
	   if (! volatile_func && (regs_ever_live[14] || saved_hard_reg)) \
	     offset += 4;						\
	   (OFFSET) = (get_frame_size () + 3 & ~3) + offset;		\
         }								\
    }									\
}

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   On the ARM, (if r8 is the static chain regnum, and remembering that
   referencing pc adds an offset of 8) the trampoline looks like:
	   ldr 		r8, [pc, #0]
	   ldr		pc, [pc]
	   .word	static chain value
	   .word	function's address  */
#define TRAMPOLINE_TEMPLATE(FILE)				\
{								\
  fprintf ((FILE), "\tldr\t%sr8, [%spc, #0]\n",			\
	   REGISTER_PREFIX, REGISTER_PREFIX);			\
  fprintf ((FILE), "\tldr\t%spc, [%spc, #0]\n",			\
	   REGISTER_PREFIX, REGISTER_PREFIX);			\
  fprintf ((FILE), "\t.word\t0\n");				\
  fprintf ((FILE), "\t.word\t0\n");				\
}

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  16

/* Alignment required for a trampoline in units.  */
#define TRAMPOLINE_ALIGN  4

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)  \
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 8)),	\
		  (CXT));						\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant ((TRAMP), 12)),	\
		  (FNADDR));						\
}


/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT  1
#define HAVE_PRE_INCREMENT  1
#define HAVE_POST_DECREMENT  1
#define HAVE_PRE_DECREMENT  1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.

   On the ARM, don't allow the pc to be used.  */
#define REGNO_OK_FOR_BASE_P(REGNO)				\
  ((REGNO) < 15 || (REGNO) == FRAME_POINTER_REGNUM		\
   || (REGNO) == ARG_POINTER_REGNUM				\
   || (unsigned) reg_renumber[(REGNO)] < 15			\
   || (unsigned) reg_renumber[(REGNO)] == FRAME_POINTER_REGNUM	\
   || (unsigned) reg_renumber[(REGNO)] == ARG_POINTER_REGNUM)
#define REGNO_OK_FOR_INDEX_P(REGNO) \
  REGNO_OK_FOR_BASE_P(REGNO)

/* Maximum number of registers that can appear in a valid memory address.
   Shifts in addresses can't be by a register. */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */
/* XXX We can address any constant, eventually...  */
#if 0
#define CONSTANT_ADDRESS_P(X)	\
    ( GET_CODE(X) == LABEL_REF	\
  ||  GET_CODE(X) == SYMBOL_REF \
  ||  GET_CODE(X) == CONST_INT	\
  ||  GET_CODE(X) == CONST )
#endif

#define CONSTANT_ADDRESS_P(X)  			\
  (GET_CODE (X) == SYMBOL_REF 			\
   && (CONSTANT_POOL_ADDRESS_P (X)		\
       || (optimize > 0 && SYMBOL_REF_FLAG (X))))

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the ARM, allow any integer (invalid ones are removed later by insn
   patterns), nice doubles and symbol_refs which refer to the function's
   constant pool XXX.  */
#define LEGITIMATE_CONSTANT_P(X)				\
  (GET_CODE (X) == CONST_INT					\
   || (GET_CODE (X) == CONST_DOUBLE				\
       && (const_double_rtx_ok_for_fpu (X)			\
	   || neg_const_double_rtx_ok_for_fpu (X)))		\
   || CONSTANT_ADDRESS_P (X))

/* Symbols in the text segment can be accessed without indirecting via the
   constant pool; it may take an extra binary operation, but this is still
   faster than indirecting via memory.  Don't do this when not optimizing,
   since we won't be calculating al of the offsets necessary to do this
   simplification.  */

#define ENCODE_SECTION_INFO(decl)					\
{									\
  if (optimize > 0 && TREE_CONSTANT (decl)				\
      && (!flag_writable_strings || TREE_CODE (decl) != STRING_CST))	\
    {									\
      rtx rtl = (TREE_CODE_CLASS (TREE_CODE (decl)) != 'd'		\
                 ? TREE_CST_RTL (decl) : DECL_RTL (decl));		\
      SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;				\
    }									\
}

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.  */
#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X)  				\
  (REGNO (X) < 16 || REGNO (X) >= FIRST_PSEUDO_REGISTER \
   || REGNO (X) == FRAME_POINTER_REGNUM || REGNO (X) == ARG_POINTER_REGNUM)

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X)  \
  REG_OK_FOR_BASE_P(X)

#define REG_OK_FOR_PRE_POST_P(X)  			\
  (REGNO (X) < 16 || REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || REGNO (X) == FRAME_POINTER_REGNUM || REGNO (X) == ARG_POINTER_REGNUM)

#else

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X)  REGNO_OK_FOR_BASE_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X)  REGNO_OK_FOR_INDEX_P (REGNO (X))

#define REG_OK_FOR_PRE_POST_P(X)  					   \
  (REGNO (X) < 16 || (unsigned) reg_renumber[REGNO (X)] < 16		   \
   || REGNO (X) == FRAME_POINTER_REGNUM || REGNO (X) == ARG_POINTER_REGNUM \
   || (unsigned) reg_renumber[REGNO (X)] == FRAME_POINTER_REGNUM	   \
   || (unsigned) reg_renumber[REGNO (X)] == ARG_POINTER_REGNUM)

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS.  */
#define BASE_REGISTER_RTX_P(X)  \
  (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))

#define INDEX_REGISTER_RTX_P(X)  \
  (GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))

/* A C statement (sans semicolon) to jump to LABEL for legitimate index RTXs
   used by the macro GO_IF_LEGITIMATE_ADDRESS.  Floating point indices can
   only be small constants. */
#define GO_IF_LEGITIMATE_INDEX(MODE, BASE_REGNO, INDEX, LABEL)  	\
do									\
{									\
  HOST_WIDE_INT range;							\
  enum rtx_code code = GET_CODE (INDEX);				\
									\
  if (GET_MODE_CLASS (MODE) == MODE_FLOAT)				\
    {									\
      if (code == CONST_INT && INTVAL (INDEX) < 1024			\
	  && INTVAL (INDEX) > -1024					\
	  && (INTVAL (INDEX) & 3) == 0)					\
	goto LABEL;							\
    }									\
  else									\
    {									\
      if (INDEX_REGISTER_RTX_P (INDEX) && GET_MODE_SIZE (MODE) <= 4)	\
	goto LABEL;							\
      if (GET_MODE_SIZE (MODE) <= 4  && code == MULT)			\
	{								\
	  rtx xiop0 = XEXP (INDEX, 0);					\
	  rtx xiop1 = XEXP (INDEX, 1);					\
	  if (INDEX_REGISTER_RTX_P (xiop0)				\
	      && power_of_two_operand (xiop1, SImode))			\
	    goto LABEL;							\
	  if (INDEX_REGISTER_RTX_P (xiop1)				\
	      && power_of_two_operand (xiop0, SImode))			\
	    goto LABEL;							\
	}								\
      if (GET_MODE_SIZE (MODE) <= 4					\
	  && (code == LSHIFTRT || code == ASHIFTRT			\
	      || code == ASHIFT || code == ROTATERT))			\
	{								\
	  rtx op = XEXP (INDEX, 1);					\
	  if (INDEX_REGISTER_RTX_P (XEXP (INDEX, 0))			\
	      && GET_CODE (op) == CONST_INT && INTVAL (op) > 0		\
	      && INTVAL (op) <= 31)					\
	    goto LABEL;							\
        }								\
      range = (MODE) == HImode ? 4095 : 4096;				\
      if (code == CONST_INT && INTVAL (INDEX) < range			\
	  && INTVAL (INDEX) > -range)  	      				\
        goto LABEL;							\
    }									\
} while (0)

/* Jump to LABEL if X is a valid address RTX.  This must also take
   REG_OK_STRICT into account when deciding about valid registers, but it uses
   the above macros so we are in luck.  Allow REG, REG+REG, REG+INDEX,
   INDEX+REG, REG-INDEX, and non floating SYMBOL_REF to the constant pool.
   Allow REG-only and AUTINC-REG if handling TImode or HImode.  Other symbol
   refs must be forced though a static cell to ensure addressability.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)  \
{									\
  if (BASE_REGISTER_RTX_P (X))						\
    goto LABEL;								\
  else if ((GET_CODE (X) == POST_INC || GET_CODE (X) == PRE_DEC)	\
	   && GET_CODE (XEXP (X, 0)) == REG				\
	   && REG_OK_FOR_PRE_POST_P (XEXP (X, 0)))			\
    goto LABEL;								\
  else if ((MODE) == TImode)						\
    ;									\
  else if (GET_CODE (X) == PLUS)					\
    {									\
      rtx xop0 = XEXP(X,0);						\
      rtx xop1 = XEXP(X,1);						\
									\
      if (BASE_REGISTER_RTX_P (xop0))					\
	GO_IF_LEGITIMATE_INDEX (MODE, REGNO (xop0), xop1, LABEL);	\
      else if (BASE_REGISTER_RTX_P (xop1))				\
	GO_IF_LEGITIMATE_INDEX (MODE, REGNO (xop1), xop0, LABEL);	\
    }									\
  else if (GET_CODE (X) == MINUS)					\
    {									\
      rtx xop0 = XEXP (X,0);						\
      rtx xop1 = XEXP (X,1);						\
									\
      if (BASE_REGISTER_RTX_P (xop0))					\
	GO_IF_LEGITIMATE_INDEX (MODE, -1, xop1, LABEL);			\
    }									\
  else if (GET_MODE_CLASS (MODE) != MODE_FLOAT				\
	   && GET_CODE (X) == SYMBOL_REF				\
	   && CONSTANT_POOL_ADDRESS_P (X))				\
    goto LABEL;								\
  else if ((GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_DEC)	\
	   && GET_CODE (XEXP (X, 0)) == REG				\
	   && REG_OK_FOR_PRE_POST_P (XEXP (X, 0)))			\
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
   opportunities to optimize the output.

   On the ARM, try to convert [REG, #BIGCONST]
   into ADD BASE, REG, #UPPERCONST and [BASE, #VALIDCONST],
   where VALIDCONST == 0 in case of TImode.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				 \
{									 \
  if (GET_CODE (X) == PLUS)						 \
    {									 \
      rtx xop0 = XEXP (X, 0);						 \
      rtx xop1 = XEXP (X, 1);						 \
									 \
      if (CONSTANT_P (xop0) && ! LEGITIMATE_CONSTANT_P (xop0))		 \
	xop0 = force_reg (SImode, xop0);				 \
      if (CONSTANT_P (xop1) && ! LEGITIMATE_CONSTANT_P (xop1))		 \
	xop1 = force_reg (SImode, xop1);				 \
      if (BASE_REGISTER_RTX_P (xop0) && GET_CODE (xop1) == CONST_INT)	 \
	{								 \
	  HOST_WIDE_INT n, low_n;					 \
	  rtx base_reg, val;						 \
	  n = INTVAL (xop1);						 \
									 \
	  if (MODE == DImode)						 \
	    {								 \
	      low_n = n & 0x0f;						 \
	      n &= ~0x0f;						 \
	      if (low_n > 4)						 \
		{							 \
		  n += 16;						 \
		  low_n -= 16;						 \
		}							 \
	    }								 \
	  else								 \
	    {								 \
	      low_n = ((MODE) == TImode ? 0				 \
		       : n >= 0 ? (n & 0xfff) : -((-n) & 0xfff));	 \
	      n -= low_n;						 \
	    }								 \
	  base_reg = gen_reg_rtx (SImode);				 \
	  val = force_operand (gen_rtx (PLUS, SImode, xop0,		 \
					GEN_INT (n)), NULL_RTX);	 \
	  emit_move_insn (base_reg, val);				 \
	  (X) = (low_n == 0 ? base_reg					 \
		 : gen_rtx (PLUS, SImode, base_reg, GEN_INT (low_n)));	 \
	}								 \
      else if (xop0 != XEXP (X, 0) || xop1 != XEXP (x, 1))		 \
	(X) = gen_rtx (PLUS, SImode, xop0, xop1);			 \
    }									 \
  else if (GET_CODE (X) == MINUS)					 \
    {									 \
      rtx xop0 = XEXP (X, 0);						 \
      rtx xop1 = XEXP (X, 1);						 \
									 \
      if (CONSTANT_P (xop0))						 \
	xop0 = force_reg (SImode, xop0);				 \
      if (CONSTANT_P (xop1) && ! LEGITIMATE_CONSTANT_P (xop1))		 \
	xop1 = force_reg (SImode, xop1);				 \
      if (xop0 != XEXP (X, 0) || xop1 != XEXP (X, 1))			 \
	(X) = gen_rtx (MINUS, SImode, xop0, xop1);			 \
    }									 \
  if (memory_address_p (MODE, X))					 \
    goto WIN;								 \
}


/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)  \
{									\
  if (GET_CODE(ADDR) == PRE_DEC || GET_CODE(ADDR) == POST_DEC		\
      || GET_CODE(ADDR) == PRE_INC || GET_CODE(ADDR) == POST_INC)	\
    goto LABEL;								\
}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR  FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR  TRUNC_DIV_EXPR

/* signed 'char' is most compatible, but RISC OS wants it unsigned.
   unsigned is probably best, but may break some code.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR  0
#endif

/* Don't cse the address of the function being compiled.  */
#define NO_RECURSIVE_FUNCTION_CSE 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE)						\
  ((MODE) == QImode ? ZERO_EXTEND					\
   : ((BYTES_BIG_ENDIAN && (MODE) == HImode) ? SIGN_EXTEND : NIL))

/* Define this if zero-extension is slow (more than one real instruction).
   On the ARM, it is more than one instruction only if not fetching from
   memory.  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Immediate shift counts are truncated by the output routines (or was it
   the assembler?).  Shift counts in a register are truncated by ARM.  Note
   that the native compiler puts too large (> 32) immediate shift counts
   into a register and shifts by the register, letting the ARM decide what
   to do instead of doing that itself.  */
/* This is all wrong.  Defining SHIFT_COUNT_TRUNCATED tells combine that
   code like (X << (Y % 32)) for register X, Y is equivalent to (X << Y).
   On the arm, Y in a register is used modulo 256 for the shift. Only for
   rotates is modulo 32 used. */
/* #define SHIFT_COUNT_TRUNCATED 1 */

/* XX This is not true, is it?  */
/* All integers have the same format so truncation is easy.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Calling from registers is a massive pain.  */
#define NO_FUNCTION_CSE 1

/* Chars and shorts should be passed as ints.  */
#define PROMOTE_PROTOTYPES 1

/* The machine modes of pointers and functions */
#define Pmode  SImode
#define FUNCTION_MODE  Pmode

/* The structure type of the machine dependent info field of insns
   No uses for this yet.  */
/* #define INSN_MACHINE_INFO  struct machine_info  */

/* The relative costs of various types of constants.  Note that cse.c defines
   REG = 1, SUBREG = 2, any node = (2 + sum of subnodes).  */
#define CONST_COSTS(RTX, CODE, OUTER_CODE)			\
  case CONST_INT:						\
    if (const_ok_for_arm (INTVAL (RTX)))			\
      return (OUTER_CODE) == SET ? 2 : -1;	    		\
    else if (OUTER_CODE == AND                  		\
             && const_ok_for_arm (~INTVAL (RTX)))		\
      return -1;	                              		\
    else if ((OUTER_CODE == COMPARE             		\
              || OUTER_CODE == PLUS || OUTER_CODE == MINUS)     \
             && const_ok_for_arm (-INTVAL (RTX)))		\
      return -1;	                              		\
    else                                        		\
      return 5;		                               		\
  case CONST: 							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 6;							\
  case CONST_DOUBLE:						\
    if (const_double_rtx_ok_for_fpu (RTX))			\
      return (OUTER_CODE) == SET ? 2 : -1;			\
    else if (((OUTER_CODE) == COMPARE || (OUTER_CODE) == PLUS)	\
	     && neg_const_double_rtx_ok_for_fpu (RTX))		\
       return -1;						\
    return(7);

#define ARM_FRAME_RTX(X)				\
  ((X) == frame_pointer_rtx || (X) == stack_pointer_rtx	\
   || (X) == arg_pointer_rtx)

#define RTX_COSTS(X,CODE,OUTER_CODE)                                    \
  default:								\
   return arm_rtx_costs (X, CODE, OUTER_CODE);

/* Moves to and from memory are quite expensive */
#define MEMORY_MOVE_COST(MODE)  10

/* All address computations that can be done are free, but rtx cost returns
   the same for practically all of them.  So we weight the different types
   of address here in the order (most pref first):
   PRE/POST_INC/DEC, SHIFT or NON-INT sum, INT sum, REG, MEM or LABEL. */
#define ADDRESS_COST(X)							     \
  (10 - ((GET_CODE (X) == MEM || GET_CODE (X) == LABEL_REF		     \
	  || GET_CODE (X) == SYMBOL_REF)				     \
	 ? 0								     \
	 : ((GET_CODE (X) == PRE_INC || GET_CODE (X) == PRE_DEC		     \
	     || GET_CODE (X) == POST_INC || GET_CODE (X) == POST_DEC)	     \
	    ? 10							     \
	    : (((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS)		     \
		? 6 + (GET_CODE (XEXP (X, 1)) == CONST_INT ? 2 		     \
		       : ((GET_RTX_CLASS (GET_CODE (XEXP (X, 0))) == '2'     \
			   || GET_RTX_CLASS (GET_CODE (XEXP (X, 0))) == 'c'  \
			   || GET_RTX_CLASS (GET_CODE (XEXP (X, 1))) == '2'  \
			   || GET_RTX_CLASS (GET_CODE (XEXP (X, 1))) == 'c') \
			  ? 1 : 0))					     \
		: 4)))))
	 
   

/* Try to generate sequences that don't involve branches, we can then use
   conditional instructions */
#define BRANCH_COST 4

/* Condition code information. */
/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison. 
   CCFPEmode should be used with floating inequalities,
   CCFPmode should be used with floating equalities.
   CC_NOOVmode should be used with SImode integer equalities.
   CCmode should be used otherwise. */

#define EXTRA_CC_MODES CC_NOOVmode, CCFPmode, CCFPEmode

#define EXTRA_CC_NAMES "CC_NOOV", "CCFP", "CCFPE"

#define SELECT_CC_MODE(OP,X,Y)				      		\
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT				\
   ? ((OP == EQ || OP == NE) ? CCFPmode : CCFPEmode)			\
   : ((GET_MODE (X) == SImode)						\
      && ((OP) == EQ || (OP) == NE)					\
      && (GET_CODE (X) == PLUS || GET_CODE (X) == MINUS			\
	  || GET_CODE (X) == AND || GET_CODE (X) == IOR			\
	  || GET_CODE (X) == XOR || GET_CODE (X) == MULT		\
	  || GET_CODE (X) == NOT || GET_CODE (X) == NEG			\
	  || GET_CODE (X) == LSHIFTRT					\
	  || GET_CODE (X) == ASHIFT || GET_CODE (X) == ASHIFTRT		\
	  || GET_CODE (X) == ROTATERT || GET_CODE (X) == ZERO_EXTRACT)	\
      ? CC_NOOVmode							\
      : GET_MODE (X) == QImode ? CC_NOOVmode : CCmode))

#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPEmode)

#define STORE_FLAG_VALUE 1

/* Define the information needed to generate branch insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *arm_compare_op0, *arm_compare_op1;
extern int arm_compare_fp;

/* Define the codes that are matched by predicates in arm.c */
#define PREDICATE_CODES							\
  {"s_register_operand", {SUBREG, REG}},				\
  {"arm_add_operand", {SUBREG, REG, CONST_INT}},			\
  {"fpu_add_operand", {SUBREG, REG, CONST_DOUBLE}},			\
  {"arm_rhs_operand", {SUBREG, REG, CONST_INT}},			\
  {"fpu_rhs_operand", {SUBREG, REG, CONST_DOUBLE}},			\
  {"arm_not_operand", {SUBREG, REG, CONST_INT}},			\
  {"shiftable_operator", {PLUS, MINUS, AND, IOR, XOR}},			\
  {"minmax_operator", {SMIN, SMAX, UMIN, UMAX}},			\
  {"shift_operator", {ASHIFT, ASHIFTRT, LSHIFTRT, ROTATERT, MULT}},	\
  {"di_operand", {SUBREG, REG, CONST_INT, CONST_DOUBLE, MEM}},		\
  {"soft_df_operand", {SUBREG, REG, CONST_DOUBLE, MEM}},		\
  {"load_multiple_operation", {PARALLEL}},				\
  {"store_multiple_operation", {PARALLEL}},				\
  {"equality_operator", {EQ, NE}},					\
  {"arm_rhsm_operand", {SUBREG, REG, CONST_INT, MEM}},			\
  {"const_shift_operand", {CONST_INT}},					\
  {"index_operand", {SUBREG, REG, CONST_INT}},				\
  {"reg_or_int_operand", {SUBREG, REG, CONST_INT}},			\
  {"multi_register_push", {PARALLEL}},					\
  {"cc_register", {REG}},						\
  {"reversible_cc_register", {REG}},


/* Assembler output control */

#ifndef ARM_OS_NAME
#define ARM_OS_NAME "(generic)"
#endif

/* The text to go at the start of the assembler file */
#define ASM_FILE_START(STREAM)						  \
{									  \
  extern char *version_string;						  \
  fprintf (STREAM,"%s Generated by gcc %s for ARM/%s\n",		  \
	   ASM_COMMENT_START, version_string, ARM_OS_NAME);		  \
  fprintf (STREAM,"%srfp\t.req\t%sr9\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%ssl\t.req\t%sr10\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%sfp\t.req\t%sr11\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%sip\t.req\t%sr12\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%ssp\t.req\t%sr13\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%slr\t.req\t%sr14\n", REGISTER_PREFIX, REGISTER_PREFIX); \
  fprintf (STREAM,"%spc\t.req\t%sr15\n", REGISTER_PREFIX, REGISTER_PREFIX); \
}

#define ASM_APP_ON  ""
#define ASM_APP_OFF  ""

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  ".text"
#define DATA_SECTION_ASM_OP  ".data"

#define REGISTER_PREFIX ""
#define USER_LABEL_PREFIX "_"
#define LOCAL_LABEL_PREFIX ""

/* The assembler's names for the registers.  */
#ifndef REGISTER_NAMES
#define REGISTER_NAMES  \
{				                   \
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",  \
  "r8", "r9", "sl", "fp", "ip", "sp", "lr", "pc",  \
  "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",  \
  "cc", "sfp", "afp"				   \
}
#endif

#ifndef ADDITIONAL_REGISTER_NAMES
#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {"a1", 0},					\
  {"a2", 1},					\
  {"a3", 2},					\
  {"a4", 3},					\
  {"v1", 4},					\
  {"v2", 5},					\
  {"v3", 6},					\
  {"v4", 7},					\
  {"v5", 8},					\
  {"v6", 9},					\
  {"rfp", 9}, /* Gcc used to call it this */	\
  {"sb", 9},					\
  {"v7", 10},					\
  {"r10", 10},					\
  {"r11", 11},	/* fp */			\
  {"r12", 12},	/* ip */			\
  {"r13", 13},	/* sp */			\
  {"r14", 14},	/* lr */			\
  {"r15", 15}	/* pc */			\
}
#endif

/* Arm Assembler barfs on dollars */
#define DOLLARS_IN_IDENTIFIERS 0

#define NO_DOLLAR_IN_LABEL

/* DBX register number for a given compiler register number */
#define DBX_REGISTER_NUMBER(REGNO)  (REGNO)

/* Generate DBX debugging information.  riscix.h will undefine this because
   the native assembler does not support stabs. */
#define DBX_DEBUGGING_INFO  1

/* Acorn dbx moans about continuation chars, so don't use any.  */
#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH  0
#endif

/* Output a source filename for the debugger. RISCiX dbx insists that the
   ``desc'' field is set to compiler version number >= 315 (sic).  */
#define DBX_OUTPUT_MAIN_SOURCE_FILENAME(STREAM,NAME) 			\
do {									\
  fprintf (STREAM, ".stabs \"%s\",%d,0,315,%s\n", (NAME), N_SO,		\
	   &ltext_label_name[1]);					\
  text_section ();							\
  ASM_OUTPUT_INTERNAL_LABEL (STREAM, "Ltext", 0);			\
} while (0)
  
/* Output a label definition.  */
#define ASM_OUTPUT_LABEL(STREAM,NAME)  \
  arm_asm_output_label ((STREAM), (NAME))

/* Output a function label definition.  */
#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL) \
    ASM_OUTPUT_LABEL(STREAM, NAME)

/* Output a globalising directive for a label.  */
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)  \
  (fprintf (STREAM, "\t.global\t"),	  \
   assemble_name (STREAM, NAME),	  \
   fputc ('\n',STREAM))                   \

/* Output a reference to a label.  */
#define ASM_OUTPUT_LABELREF(STREAM,NAME)  \
  fprintf (STREAM, "%s%s", USER_LABEL_PREFIX, NAME)

/* Make an internal label into a string.  */
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*%s%d", PREFIX, NUM)

/* Output an internal label definition.  */
#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM)  \
  do                                    	      	   		\
    {						      	   		\
      char *s = (char *) alloca (11 + strlen (PREFIX));	   		\
      extern int arm_target_label, arm_ccfsm_state;	   		\
      extern rtx arm_target_insn;					\
						           		\
      if (arm_ccfsm_state == 3 && arm_target_label == (NUM)   		\
	&& !strcmp (PREFIX, "L"))					\
	{								\
	  arm_ccfsm_state = 0;				        	\
	  arm_target_insn = NULL;					\
	}								\
	strcpy (s, "*");				      		\
	sprintf (&s[strlen (s)], "%s%d", (PREFIX), (NUM));   		\
	arm_asm_output_label (STREAM, s);		                \
    } while (0)

/* Nothing special is done about jump tables */
/* #define ASM_OUTPUT_CASE_LABEL(STREAM,PREFIX,NUM,TABLE)   */
/* #define ASM_OUTPUT_CASE_END(STREAM,NUM,TABLE)	    */

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)  \
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),  \
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Output a push or a pop instruction (only used when profiling).  */
#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO) \
  fprintf(STREAM,"\tstmfd\t%ssp!,{%s%s}\n", \
	  REGISTER_PREFIX, REGISTER_PREFIX, reg_names[REGNO])

#define ASM_OUTPUT_REG_POP(STREAM,REGNO) \
  fprintf(STREAM,"\tldmfd\t%ssp!,{%s%s}\n", \
	  REGISTER_PREFIX, REGISTER_PREFIX, reg_names[REGNO])

/* Output a relative address. Not needed since jump tables are absolute
   but we must define it anyway.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,VALUE,REL)  \
  fputs ("- - - ASM_OUTPUT_ADDR_DIFF_ELT called!\n", STREAM)

/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  \
   fprintf (STREAM, "\t.word\tL%d\n", VALUE)

/* Output various types of constants.  For real numbers we output hex, with
   a comment containing the "human" value, this allows us to pass NaN's which
   the riscix assembler doesn't understand (it also makes cross-assembling
   less likely to fail). */

#define ASM_OUTPUT_LONG_DOUBLE(STREAM,VALUE)				\
do { char dstr[30];							\
     long l[3];								\
     arm_increase_location (12);					\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);			\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.20g", dstr);			\
     if (sizeof (int) == sizeof (long))					\
       fprintf (STREAM, "\t.long 0x%x,0x%x,0x%x\t%s long double %s\n",	\
		l[2], l[1], l[0], ASM_COMMENT_START, dstr);		\
     else								\
       fprintf (STREAM, "\t.long 0x%lx,0x%lx,0x%lx\t%s long double %s\n",\
		l[0], l[1], l[2], ASM_COMMENT_START, dstr);		\
   } while (0)

    
#define ASM_OUTPUT_DOUBLE(STREAM, VALUE)  				\
do { char dstr[30];							\
     long l[2];								\
     arm_increase_location (8);						\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);				\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.14g", dstr);			\
     if (sizeof (int) == sizeof (long))					\
       fprintf (STREAM, "\t.long 0x%x, 0x%x\t%s double %s\n", l[0],	\
		l[1], ASM_COMMENT_START, dstr);				\
     else								\
       fprintf (STREAM, "\t.long 0x%lx, 0x%lx\t%s double %s\n", l[0],	\
		l[1], ASM_COMMENT_START, dstr);				\
   } while (0)

#define ASM_OUTPUT_FLOAT(STREAM, VALUE)					\
do { char dstr[30];							\
     long l;								\
     arm_increase_location (4);						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);				\
     REAL_VALUE_TO_DECIMAL (VALUE, "%.7g", dstr);			\
     if (sizeof (int) == sizeof (long))					\
       fprintf (STREAM, "\t.word 0x%x\t%s float %s\n", l,		\
		ASM_COMMENT_START, dstr);				\
     else								\
       fprintf (STREAM, "\t.word 0x%lx\t%s float %s\n", l,		\
		ASM_COMMENT_START, dstr);				\
   } while (0);

#define ASM_OUTPUT_INT(STREAM, EXP)	\
  (fprintf (STREAM, "\t.word\t"),	\
   output_addr_const (STREAM, (EXP)),	\
   arm_increase_location (4),		\
   fputc ('\n', STREAM))

#define ASM_OUTPUT_SHORT(STREAM, EXP)  \
  (fprintf (STREAM, "\t.short\t"),     \
   output_addr_const (STREAM, (EXP)),  \
   arm_increase_location (2),          \
   fputc ('\n', STREAM))

#define ASM_OUTPUT_CHAR(STREAM, EXP)  \
  (fprintf (STREAM, "\t.byte\t"),      \
   output_addr_const (STREAM, (EXP)),  \
   arm_increase_location (1),          \
   fputc ('\n', STREAM))

#define ASM_OUTPUT_BYTE(STREAM, VALUE)  \
  (fprintf (STREAM, "\t.byte\t%d\n", VALUE),  \
   arm_increase_location (1))

#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN)  \
  output_ascii_pseudo_op ((STREAM), (unsigned char *)(PTR), (LEN))

/* Output a gap.  In fact we fill it with nulls.  */
#define ASM_OUTPUT_SKIP(STREAM, NBYTES)  \
  (arm_increase_location (NBYTES),              \
   fprintf (STREAM, "\t.space\t%d\n", NBYTES))

/* Align output to a power of two.  Horrible /bin/as.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER)  \
  do                                                           \
    {                                                          \
      register int amount = 1 << (POWER);                      \
      extern int arm_text_location;			       \
                                                               \
      if (amount == 2)                                         \
	fprintf (STREAM, "\t.even\n");                         \
      else                                                     \
	fprintf (STREAM, "\t.align\t%d\n", amount - 4);        \
                                                               \
      if (in_text_section ())                                  \
	arm_text_location = ((arm_text_location + amount - 1)  \
			     & ~(amount - 1));                 \
    } while (0)

/* Output a common block */
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)  		\
  (fprintf (STREAM, "\t.comm\t"), 		     			\
   assemble_name ((STREAM), (NAME)),		     			\
   fprintf(STREAM, ", %d\t%s %d\n", ROUNDED, ASM_COMMENT_START, SIZE))

/* Output a local common block.  /bin/as can't do this, so hack a `.space' into
   the bss segment.  Note that this is *bad* practice.  */
#define ASM_OUTPUT_LOCAL(STREAM,NAME,SIZE,ROUNDED)  \
  output_lcomm_directive (STREAM, NAME, SIZE, ROUNDED)

/* Output a source line for the debugger.  */
/* #define ASM_OUTPUT_SOURCE_LINE(STREAM,LINE) */

/* Output a #ident directive.  */
#define ASM_OUTPUT_IDENT(STREAM,STRING)  \
  fprintf (STREAM,"- - - ident %s\n",STRING)

/* The assembler's parentheses characters.  */
#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Target characters.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015

/* Only perform branch elimination (by making instructions conditional) if
   we're optimising.  Otherwise it's of no use anyway.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)  \
  if (optimize)					    \
    final_prescan_insn (INSN, OPVEC, NOPERANDS)

#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START "@"
#endif

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)	\
  ((CODE) == '?' || (CODE) == '|' || (CODE) == '@')
/* Output an operand of an instruction.  */
#define PRINT_OPERAND(STREAM, X, CODE)  \
  arm_print_operand (STREAM, X, CODE)

#define ARM_SIGN_EXTEND(x)  ((HOST_WIDE_INT)		\
  (HOST_BITS_PER_WIDE_INT <= 32 ? (x)			\
   : (((x) & (unsigned HOST_WIDE_INT) 0xffffffff) |	\
      (((x) & (unsigned HOST_WIDE_INT) 0x80000000)	\
       ? ((~ (HOST_WIDE_INT) 0)				\
	  & ~ (unsigned HOST_WIDE_INT) 0xffffffff)	\
       : 0))))

/* Output the address of an operand.  */
#define PRINT_OPERAND_ADDRESS(STREAM,X)  \
{									\
    int is_minus = GET_CODE (X) == MINUS;				\
									\
    if (GET_CODE (X) == REG)						\
	fprintf (STREAM, "[%s%s, #0]", REGISTER_PREFIX,			\
		 reg_names[REGNO (X)]);					\
    else if (GET_CODE (X) == PLUS || is_minus)				\
      {									\
	rtx base = XEXP (X, 0);						\
	rtx index = XEXP (X, 1);					\
	char *base_reg_name;						\
	HOST_WIDE_INT offset = 0;					\
	if (GET_CODE (base) != REG)					\
	  {								\
	    /* Ensure that BASE is a register (one of them must be). */	\
	    rtx temp = base;						\
	    base = index;						\
	    index = temp;						\
	  }								\
	base_reg_name = reg_names[REGNO (base)];			\
	switch (GET_CODE (index))					\
	  {								\
	  case CONST_INT:						\
	    offset = INTVAL (index);					\
	    if (is_minus)						\
	      offset = -offset;						\
	    fprintf (STREAM, "[%s%s, #%d]", REGISTER_PREFIX,		\
		     base_reg_name, offset);				\
	    break;							\
									\
	  case REG:							\
	    fprintf (STREAM, "[%s%s, %s%s%s]", REGISTER_PREFIX,		\
		     base_reg_name, is_minus ? "-" : "",		\
		     REGISTER_PREFIX, reg_names[REGNO (index)] );	\
	    break;							\
									\
	  case MULT:							\
	  case ASHIFTRT:						\
	  case LSHIFTRT:						\
	  case ASHIFT:							\
	  case ROTATERT:						\
	  {								\
	    fprintf (STREAM, "[%s%s, %s%s%s", REGISTER_PREFIX,		\
		     base_reg_name, is_minus ? "-" : "", REGISTER_PREFIX,\
		     reg_names[REGNO (XEXP (index, 0))]);		\
	    arm_print_operand (STREAM, index, 'S');			\
	    fputs ("]", STREAM);					\
	    break;							\
	  }								\
	    								\
	  default:							\
	    abort();							\
	}								\
    }							        	\
  else if (GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_INC		\
	   || GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_DEC)	\
    {									\
      extern int output_memory_reference_mode;				\
      									\
      if (GET_CODE (XEXP (X, 0)) != REG)				\
	abort ();							\
									\
      if (GET_CODE (X) == PRE_DEC || GET_CODE (X) == PRE_INC)		\
	fprintf (STREAM, "[%s%s, #%s%d]!", REGISTER_PREFIX,		\
		 reg_names[REGNO (XEXP (X, 0))],			\
		 GET_CODE (X) == PRE_DEC ? "-" : "",			\
		 GET_MODE_SIZE (output_memory_reference_mode));		\
      else								\
	fprintf (STREAM, "[%s%s], #%s%d", REGISTER_PREFIX,		\
		 reg_names[REGNO (XEXP (X, 0))],			\
		 GET_CODE (X) == POST_DEC ? "-" : "",			\
		 GET_MODE_SIZE (output_memory_reference_mode));		\
    }									\
  else output_addr_const(STREAM, X);					\
}
