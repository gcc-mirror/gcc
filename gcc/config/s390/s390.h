/* Definitions of target machine for GNU compiler, for IBM S/390
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).
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

#ifndef _S390_H
#define _S390_H

/* Override the __fixdfdi etc. routines when building libgcc2.
   ??? This should be done in a cleaner way ...  */
#if defined (IN_LIBGCC2) && !defined (__s390x__)
#include <s390/fixdfdi.h>
#endif


/* Run-time target specification.  */

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_assert ("cpu=s390");			\
      builtin_assert ("machine=s390");			\
      builtin_define ("__s390__");			\
      if (TARGET_64BIT)					\
        builtin_define ("__s390x__");			\
    }							\
  while (0)

/* Optional target features.  */
extern int target_flags;

#define TARGET_HARD_FLOAT          (target_flags & 1)
#define TARGET_SOFT_FLOAT          (!(target_flags & 1))
#define TARGET_BACKCHAIN           (target_flags & 2)
#define TARGET_SMALL_EXEC          (target_flags & 4)
#define TARGET_DEBUG_ARG           (target_flags & 8)
#define TARGET_64BIT               (target_flags & 16)
#define TARGET_MVCLE               (target_flags & 32)

/* ??? Once this actually works, it could be made a runtime option.  */
#define TARGET_IBM_FLOAT           0
#define TARGET_IEEE_FLOAT          1

#ifdef DEFAULT_TARGET_64BIT
#define TARGET_DEFAULT             0x13
#else
#define TARGET_DEFAULT             0x3
#endif

#define TARGET_SWITCHES           		       		       \
{ { "hard-float",    1, N_("Use hardware fp")},         		       \
  { "soft-float",   -1, N_("Don't use hardware fp")},	      	       \
  { "backchain",     2, N_("Set backchain")},           		       \
  { "no-backchain", -2, N_("Don't set backchain (faster, but debug harder")}, \
  { "small-exec",    4, N_("Use bras for executable < 64k")},           \
  { "no-small-exec",-4, N_("Don't use bras")},            	       \
  { "debug",         8, N_("Additional debug prints")},        	       \
  { "no-debug",     -8, N_("Don't print additional debug prints")},     \
  { "64",           16, N_("64 bit mode")},         	               \
  { "31",          -16, N_("31 bit mode")},                             \
  { "mvcle",        32, N_("mvcle use")},         	               \
  { "no-mvcle",    -32, N_("mvc&ex")},                                  \
  { "", TARGET_DEFAULT, 0 } }

/* Target version string.  Overridden by the OS header.  */
#ifdef DEFAULT_TARGET_64BIT
#define TARGET_VERSION fprintf (stderr, " (zSeries)");
#else
#define TARGET_VERSION fprintf (stderr, " (S/390)");
#endif

/* Hooks to override options.  */
#define OPTIMIZATION_OPTIONS(LEVEL, SIZE) optimization_options(LEVEL, SIZE)
#define OVERRIDE_OPTIONS override_options ()

/* Frame pointer is not used for debugging.  */
#define CAN_DEBUG_WITHOUT_FP


/* In libgcc2, determine target settings as compile-time constants.  */
#ifdef IN_LIBGCC2
#undef TARGET_64BIT
#ifdef __s390x__
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif


/* Target machine storage layout.  */

/* Everything is big-endian.  */
#define BITS_BIG_ENDIAN 1
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#ifndef IN_LIBGCC2
#define MIN_UNITS_PER_WORD 4
#endif
#define MAX_BITS_PER_WORD 64

/* Function arguments and return values are promoted to word size.  */
#define PROMOTE_FUNCTION_ARGS
#define PROMOTE_FUNCTION_RETURN
#define PROMOTE_FOR_CALL_ONLY

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)		\
if (INTEGRAL_MODE_P (MODE) &&	        	    	\
    GET_MODE_SIZE (MODE) < UNITS_PER_WORD) { 		\
  (MODE) = Pmode;					\
	  }

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_64BIT ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 64

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Alignment on even addresses for LARL instruction.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN) (ALIGN) < 16 ? 16 : (ALIGN)
#define DATA_ALIGNMENT(TYPE, ALIGN) (ALIGN) < 16 ? 16 : (ALIGN)

/* Alignment is not required by the hardware.  */
#define STRICT_ALIGNMENT 0

/* Mode of stack savearea.
   FUNCTION is VOIDmode because calling convention maintains SP.
   BLOCK needs Pmode for SP.
   NONLOCAL needs twice Pmode to maintain both backchain and SP.  */
#define STACK_SAVEAREA_MODE(LEVEL)      \
  (LEVEL == SAVE_FUNCTION ? VOIDmode    \
  : LEVEL == SAVE_NONLOCAL ? (TARGET_64BIT ? TImode : DImode) : Pmode)

/* Define target floating point format.  */
#define TARGET_FLOAT_FORMAT \
  (TARGET_IEEE_FLOAT? IEEE_FLOAT_FORMAT : IBM_FLOAT_FORMAT)


/* Type layout.  */

/* Sizes in bits of the source language data types.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE (TARGET_64BIT ? 64 : 32)
#define MAX_LONG_TYPE_SIZE 64
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64  /* ??? Should support extended format.  */

/* We use "unsigned char" as default.  */
#define DEFAULT_SIGNED_CHAR 0


/* Register usage.  */

/* We have 16 general purpose registers (registers 0-15),
   and 16 floating point registers (registers 16-31).
   (On non-IEEE machines, we have only 4 fp registers.)
 
   Amongst the general purpose registers, some are used
   for specific purposes:
   GPR 11: Hard frame pointer (if needed)
   GPR 12: Global offset table pointer (if needed)
   GPR 13: Literal pool base register
   GPR 14: Return address register
   GPR 15: Stack pointer
 
   Registers 32-34 are 'fake' hard registers that do not
   correspond to actual hardware:
   Reg 32: Argument pointer
   Reg 33: Condition code
   Reg 34: Frame pointer  */

#define FIRST_PSEUDO_REGISTER 35

/* Standard register usage.  */
#define GENERAL_REGNO_P(N)	((int)(N) >= 0 && (N) < 16)
#define ADDR_REGNO_P(N)		((N) >= 1 && (N) < 16)
#define FP_REGNO_P(N)		((N) >= 16 && (N) < (TARGET_IEEE_FLOAT? 32 : 20))
#define CC_REGNO_P(N)		((N) == 33)
#define FRAME_REGNO_P(N)	((N) == 32 || (N) == 34)

#define GENERAL_REG_P(X)	(REG_P (X) && GENERAL_REGNO_P (REGNO (X)))
#define ADDR_REG_P(X)		(REG_P (X) && ADDR_REGNO_P (REGNO (X)))
#define FP_REG_P(X)		(REG_P (X) && FP_REGNO_P (REGNO (X)))
#define CC_REG_P(X)		(REG_P (X) && CC_REGNO_P (REGNO (X)))
#define FRAME_REG_P(X)		(REG_P (X) && FRAME_REGNO_P (REGNO (X)))

#define BASE_REGISTER 13
#define RETURN_REGNUM 14
#define CC_REGNUM 33

/* Set up fixed registers and calling convention:

   GPRs 0-5 are always call-clobbered,
   GPRs 6-15 are always call-saved.
   GPR 12 is fixed if used as GOT pointer.
   GPR 13 is always fixed (as literal pool pointer).
   GPR 14 is always fixed (as return address).
   GPR 15 is always fixed (as stack pointer).
   The 'fake' hard registers are call-clobbered and fixed.

   On 31-bit, FPRs 18-19 are call-clobbered;
   on 64-bit, FPRs 24-31 are call-clobbered.
   The remaining FPRs are call-saved.  */

#define FIXED_REGISTERS				\
{ 0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 1, 1, 1,					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  1, 1, 1 }

#define CALL_USED_REGISTERS			\
{ 1, 1, 1, 1, 					\
  1, 1, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 1, 1, 1,					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1 }

#define CALL_REALLY_USED_REGISTERS		\
{ 1, 1, 1, 1, 					\
  1, 1, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0,					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1 }

#define CONDITIONAL_REGISTER_USAGE				\
do								\
  {								\
    int i;							\
								\
    if (flag_pic)						\
      {								\
	fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
	call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
      }								\
    if (TARGET_64BIT)						\
      {								\
        for (i = 24; i < 32; i++)				\
	    call_used_regs[i] = call_really_used_regs[i] = 0;	\
      }								\
    else							\
      {								\
        for (i = 18; i < 20; i++)				\
	    call_used_regs[i] = call_really_used_regs[i] = 0;	\
      }								\
 } while (0)

/* Preferred register allocation order.  */
#define REG_ALLOC_ORDER                                         \
{  1, 2, 3, 4, 5, 0, 14, 13, 12, 11, 10, 9, 8, 7, 6,            \
   16, 17, 18, 19, 20, 21, 22, 23,                              \
   24, 25, 26, 27, 28, 29, 30, 31,                              \
   15, 32, 33, 34 }


/* Fitting values into registers.  */
 
/* Integer modes <= word size fit into any GPR.
   Integer modes > word size fit into successive GPRs, starting with
   an even-numbered register.
   SImode and DImode fit into FPRs as well.
 
   Floating point modes <= word size fit into any FPR or GPR.
   Floating point modes > word size (i.e. DFmode on 32-bit) fit
   into any FPR, or an even-odd GPR pair.
 
   Complex floating point modes fit either into two FPRs, or into
   successive GPRs (again starting with an even number).
 
   Condition code modes fit only into the CC register.  */

#define HARD_REGNO_NREGS(REGNO, MODE)                           \
  (FP_REGNO_P(REGNO)?                                           \
    (GET_MODE_CLASS(MODE) == MODE_COMPLEX_FLOAT ? 2 : 1) :      \
   GENERAL_REGNO_P(REGNO)?                                      \
    ((GET_MODE_SIZE(MODE)+UNITS_PER_WORD-1) / UNITS_PER_WORD) : \
   1)

#define HARD_REGNO_MODE_OK(REGNO, MODE)                             \
  (FP_REGNO_P(REGNO)?                                               \
   ((MODE) == SImode || (MODE) == DImode ||                         \
    GET_MODE_CLASS(MODE) == MODE_FLOAT ||                           \
    GET_MODE_CLASS(MODE) == MODE_COMPLEX_FLOAT) :                   \
   GENERAL_REGNO_P(REGNO)?                                          \
    (HARD_REGNO_NREGS(REGNO, MODE) == 1 || !((REGNO) & 1)) :        \
   CC_REGNO_P(REGNO)?                                               \
     GET_MODE_CLASS (MODE) == MODE_CC :                             \
   FRAME_REGNO_P(REGNO)?                                            \
     (enum machine_mode) (MODE) == Pmode :                          \
   0)

#define MODES_TIEABLE_P(MODE1, MODE2)		\
   (((MODE1) == SFmode || (MODE1) == DFmode)	\
   == ((MODE2) == SFmode || (MODE2) == DFmode))

/* Maximum number of registers to represent a value of mode MODE
   in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE)   					\
     ((CLASS) == FP_REGS ? 						\
      (GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT ? 2 : 1) :  		\
      (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* If a 4-byte value is loaded into a FPR, it is placed into the
   *upper* half of the register, not the lower.  Therefore, we
   cannot use SUBREGs to switch between modes in FP registers.  */
#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS)		\
  (GET_MODE_SIZE (FROM) != GET_MODE_SIZE (TO)			\
   ? reg_classes_intersect_p (FP_REGS, CLASS) : 0)

/* Register classes.  */
 
/* We use the following register classes:
   GENERAL_REGS     All general purpose registers
   ADDR_REGS        All general purpose registers except %r0
                    (These registers can be used in address generation)
   FP_REGS          All floating point registers
 
   GENERAL_FP_REGS  Union of GENERAL_REGS and FP_REGS
   ADDR_FP_REGS     Union of ADDR_REGS and FP_REGS
 
   NO_REGS          No registers
   ALL_REGS         All registers
 
   Note that the 'fake' frame pointer and argument pointer registers
   are included amongst the address registers here.  The condition
   code register is only included in ALL_REGS.  */

enum reg_class
{
  NO_REGS, ADDR_REGS, GENERAL_REGS,
  FP_REGS, ADDR_FP_REGS, GENERAL_FP_REGS,
  ALL_REGS, LIM_REG_CLASSES
};
#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES                                                 \
{ "NO_REGS", "ADDR_REGS", "GENERAL_REGS", 				\
  "FP_REGS", "ADDR_FP_REGS", "GENERAL_FP_REGS", "ALL_REGS" }

/* Class -> register mapping.  */
#define REG_CLASS_CONTENTS \
{				       			\
  { 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0x0000fffe, 0x00000005 },	/* ADDR_REGS */		\
  { 0x0000ffff, 0x00000005 },	/* GENERAL_REGS */	\
  { 0xffff0000, 0x00000000 },	/* FP_REGS */		\
  { 0xfffffffe, 0x00000005 },	/* ADDR_FP_REGS */	\
  { 0xffffffff, 0x00000005 },	/* GENERAL_FP_REGS */	\
  { 0xffffffff, 0x00000007 },	/* ALL_REGS */		\
}

/* Register -> class mapping.  */
extern const enum reg_class regclass_map[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) (regclass_map[REGNO])

/* ADDR_REGS can be used as base or index register.  */
#define INDEX_REG_CLASS ADDR_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Check whether REGNO is a hard register of the suitable class
   or a pseudo register currently allocated to one such.  */
#define REGNO_OK_FOR_INDEX_P(REGNO)					\
    (((REGNO) < FIRST_PSEUDO_REGISTER 					\
     && REGNO_REG_CLASS ((REGNO)) == ADDR_REGS) 			\
    || (reg_renumber[REGNO] > 0 && reg_renumber[REGNO] < 16))
#define REGNO_OK_FOR_BASE_P(REGNO) REGNO_OK_FOR_INDEX_P (REGNO)


/* Given an rtx X being reloaded into a reg required to be in class CLASS,
   return the class of reg to actually use.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS)	\
  s390_preferred_reload_class ((X), (CLASS))

/* We need a secondary reload when loading a PLUS which is
   not a valid operand for LOAD ADDRESS.  */
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, IN)	\
  s390_secondary_input_reload_class ((CLASS), (MODE), (IN))

/* We need a secondary reload when storing a double-word
   to a non-offsettable memory address.  */
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, OUT)	\
  s390_secondary_output_reload_class ((CLASS), (MODE), (OUT))

/* We need secondary memory to move data between GPRs and FPRs.  */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) \
 ((CLASS1) != (CLASS2) && ((CLASS1) == FP_REGS || (CLASS2) == FP_REGS))

/* Get_secondary_mem widens its argument to BITS_PER_WORD which loses on 64bit
   because the movsi and movsf patterns don't handle r/f moves.  */
#define SECONDARY_MEMORY_NEEDED_MODE(MODE)		\
 (GET_MODE_BITSIZE (MODE) < 32				\
  ? mode_for_size (32, GET_MODE_CLASS (MODE), 0)	\
  : MODE)


/* Define various machine-dependent constraint letters.  */

#define REG_CLASS_FROM_LETTER(C)                                        \
  ((C) == 'a' ? ADDR_REGS :                                             \
   (C) == 'd' ? GENERAL_REGS :                                          \
   (C) == 'f' ? FP_REGS : NO_REGS)

#define CONST_OK_FOR_LETTER_P(VALUE, C)                                 \
  ((C) == 'I' ? (unsigned long) (VALUE) < 256 :                         \
   (C) == 'J' ? (unsigned long) (VALUE) < 4096 :                        \
   (C) == 'K' ? (VALUE) >= -32768 && (VALUE) < 32768 :                  \
   (C) == 'L' ? (unsigned long) (VALUE) < 65536 : 0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  1

#define EXTRA_CONSTRAINT(OP, C)                               	\
     ((C) == 'Q' ?  q_constraint (OP) : 			\
      (C) == 'S' ?  larl_operand (OP, GET_MODE (OP)) : 0)

#define EXTRA_MEMORY_CONSTRAINT(C) ((C) == 'Q')


/* Stack layout and calling conventions.  */
 
/* Our stack grows from higher to lower addresses.  However, local variables
   are accessed by positive offsets, and function arguments are stored at
   increasing addresses.  */
#define STACK_GROWS_DOWNWARD
/* #undef FRAME_GROWS_DOWNWARD */
/* #undef ARGS_GROW_DOWNWARD */

/* The basic stack layout looks like this: the stack pointer points
   to the register save area for called functions.  Above that area
   is the location to place outgoing arguments.  Above those follow
   dynamic allocations (alloca), and finally the local variables.  */

/* Offset from stack-pointer to first location of outgoing args.  */
#define STACK_POINTER_OFFSET (TARGET_64BIT ? 160 : 96)

/* Offset within stack frame to start allocating local variables at.  */
extern int current_function_outgoing_args_size;
#define STARTING_FRAME_OFFSET \
     (STACK_POINTER_OFFSET + current_function_outgoing_args_size)

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.  */
#define STACK_DYNAMIC_OFFSET(FUNDECL) (STARTING_FRAME_OFFSET)

/* Offset of first parameter from the argument pointer register value.
   We have a fake argument pointer register that points directly to
   the argument area.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* The return address of the current frame is retrieved 
   from the initial value of register RETURN_REGNUM.
   For frames farther back, we use the stack slot where
   the corresponding RETURN_REGNUM register was saved.  */

#define DYNAMIC_CHAIN_ADDRESS(FRAME)						\
  ((FRAME) != hard_frame_pointer_rtx ? (FRAME) :				\
   plus_constant (arg_pointer_rtx, -STACK_POINTER_OFFSET))
     
#define RETURN_ADDR_RTX(COUNT, FRAME)						\
  s390_return_addr_rtx ((COUNT), DYNAMIC_CHAIN_ADDRESS ((FRAME)))

/* In 31-bit mode, we need to mask off the high bit of return addresses.  */
#define MASK_RETURN_ADDR (TARGET_64BIT ? GEN_INT (-1) : GEN_INT (0x7fffffff))


/* Exception handling.  */
 
/* Describe calling conventions for DWARF-2 exception handling.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, RETURN_REGNUM)
#define INCOMING_FRAME_SP_OFFSET STACK_POINTER_OFFSET
#define DWARF_FRAME_RETURN_COLUMN  14

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 6 : INVALID_REGNUM)
#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, \
                                     TARGET_64BIT? -48 : -40))

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)			    \
  (flag_pic								    \
    ? ((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4 \
   : DW_EH_PE_absptr)


/* Frame registers.  */

#define STACK_POINTER_REGNUM 15
#define FRAME_POINTER_REGNUM 34
#define HARD_FRAME_POINTER_REGNUM 11
#define ARG_POINTER_REGNUM 32

/* The static chain must be call-clobbered, but not used for 
   function argument passing.  As register 1 is clobbered by 
   the trampoline code, we only have one option.  */
#define STATIC_CHAIN_REGNUM 0

/* Number of hardware registers that go into the DWARF-2 unwind info.
   To avoid ABI incompatibility, this number must not change even as
   'fake' hard registers are added or removed.  */
#define DWARF_FRAME_REGISTERS 34


/* Frame pointer and argument pointer elimination.  */

#define FRAME_POINTER_REQUIRED 0

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0

#define ELIMINABLE_REGS				        \
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	        \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},    \
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	        \
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}  

#define CAN_ELIMINATE(FROM, TO) (1)

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) 			  \
{ if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) 	  \
  { (OFFSET) = 0; }     						  \
  else  if ((FROM) == FRAME_POINTER_REGNUM                                \
	    && (TO) == HARD_FRAME_POINTER_REGNUM)                	  \
  { (OFFSET) = 0; }     						  \
  else if ((FROM) == ARG_POINTER_REGNUM                                   \
            && (TO) == HARD_FRAME_POINTER_REGNUM)                         \
  { (OFFSET) = s390_arg_frame_offset (); }     				  \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)  \
  { (OFFSET) = s390_arg_frame_offset (); }     				  \
  else									  \
    abort();								  \
}


/* Stack arguments.  */
 
/* We need current_function_outgoing_args to be valid.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Return doesn't modify the stack.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0


/* Register arguments.  */
 
typedef struct s390_arg_structure
{
  int gprs;			/* gpr so far */
  int fprs;			/* fpr so far */
}
CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, NN) \
  ((CUM).gprs=0, (CUM).fprs=0)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)                    \
  s390_function_arg_advance (&CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)   \
  s390_function_arg (&CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  s390_function_arg_pass_by_reference (MODE, TYPE)

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* Arguments can be placed in general registers 2 to 6,
   or in floating point registers 0 and 2.  */
#define FUNCTION_ARG_REGNO_P(N) (((N) >=2 && (N) <7) || \
                                 (N) == 16 || (N) == 17)


/* Scalar return values.  */
 
/* We return scalars in general purpose register 2 for integral values,
   and floating point register 0 for fp values.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)				\
  gen_rtx_REG ((INTEGRAL_TYPE_P (VALTYPE)			\
		&& TYPE_PRECISION (VALTYPE) < BITS_PER_WORD)	\
	       || POINTER_TYPE_P (VALTYPE)			\
	       ? word_mode : TYPE_MODE (VALTYPE),		\
	       TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_HARD_FLOAT ? 16 : 2)

/* Define how to find the value returned by a library function assuming
   the value has mode MODE.  */
#define RET_REG(MODE) ((GET_MODE_CLASS (MODE) == MODE_INT       \
                       || TARGET_SOFT_FLOAT ) ? 2 : 16)
#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, RET_REG (MODE))

/* Only gpr 2 and fpr 0 are ever used as return registers.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 2 || (N) == 16)


/* Aggregate return values.  */

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.  */
#define RETURN_IN_MEMORY(type)       				\
  (TYPE_MODE (type) == BLKmode || 				\
   GET_MODE_SIZE (TYPE_MODE (type)) > 8	||			\
   GET_MODE_CLASS (TYPE_MODE (type)) == MODE_COMPLEX_INT  ||	\
   GET_MODE_CLASS (TYPE_MODE (type)) == MODE_COMPLEX_FLOAT ||	\
   GET_MODE_CLASS (TYPE_MODE (type)) == MODE_VECTOR_INT ||	\
   GET_MODE_CLASS (TYPE_MODE (type)) == MODE_VECTOR_FLOAT)

/* Structure value address is passed as invisible first argument (gpr 2).  */
#define STRUCT_VALUE 0


/* Function entry and exit.  */
 
/* When returning from a function, the stack pointer does not matter.  */
#define EXIT_IGNORE_STACK       1


/* Profiling.  */

#define FUNCTION_PROFILER(FILE, LABELNO) 			\
  s390_function_profiler ((FILE), ((LABELNO)))

#define PROFILE_BEFORE_PROLOGUE 1


/* Implementing the varargs macros.  */

#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = s390_build_va_list ()

#define EXPAND_BUILTIN_VA_START(valist, nextarg) \
  s390_va_start (valist, nextarg)

#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  s390_va_arg (valist, type)


/* Trampolines for nested functions.  */

#define TRAMPOLINE_SIZE (TARGET_64BIT ? 36 : 20)

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CXT)                       \
   s390_initialize_trampoline ((ADDR), (FNADDR), (CXT))

#define TRAMPOLINE_TEMPLATE(FILE)                                       \
   s390_trampoline_template (FILE)


/* Library calls.  */
 
/* We should use memcpy, not bcopy.  */
#define TARGET_MEM_FUNCTIONS


/* Addressing modes, and classification of registers for them.  */

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) 0

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 2

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx and check
   its validity for a certain class.  We have two alternate definitions
   for each of them.  The usual definition accepts all pseudo regs; the
   other rejects them all.  The symbol REG_OK_STRICT causes the latter
   definition to be used.

   Most source files want to accept pseudo regs in the hope that they will
   get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#define REG_OK_FOR_INDEX_NONSTRICT_P(X)   	\
((GET_MODE (X) == Pmode) &&			\
 ((REGNO (X) >= FIRST_PSEUDO_REGISTER) 		\
  || REGNO_REG_CLASS (REGNO (X)) == ADDR_REGS))  

#define REG_OK_FOR_BASE_NONSTRICT_P(X)    REG_OK_FOR_INDEX_NONSTRICT_P (X)

#define REG_OK_FOR_INDEX_STRICT_P(X) 				\
((GET_MODE (X) == Pmode) && (REGNO_OK_FOR_INDEX_P (REGNO (X))))

#define REG_OK_FOR_BASE_STRICT_P(X)				\
((GET_MODE (X) == Pmode) && (REGNO_OK_FOR_BASE_P (REGNO (X))))

#ifndef REG_OK_STRICT
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_NONSTRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_NONSTRICT_P(X)
#else
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_STRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_STRICT_P(X)
#endif

/* S/390 has no mode dependent addresses.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression that is a
   valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.  */
#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)                         \
{                                                                       \
  if (legitimate_address_p (MODE, X, 1))                                \
    goto ADDR;                                                          \
}
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)                         \
{                                                                       \
  if (legitimate_address_p (MODE, X, 0))                                \
    goto ADDR;                                                          \
}
#endif

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.  */
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)                          \
{                                                                       \
  (X) = legitimize_address (X, OLDX, MODE);                             \
  if (memory_address_p (MODE, X))                                       \
    goto WIN;                                                           \
}

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
#define LEGITIMATE_CONSTANT_P(X) \
     legitimate_constant_p (X)

/* Helper macro for s390.c and s390.md to check for symbolic constants.  */
#define SYMBOLIC_CONST(X)       \
(GET_CODE (X) == SYMBOL_REF                                             \
 || GET_CODE (X) == LABEL_REF                                           \
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

#define TLS_SYMBOLIC_CONST(X)	\
((GET_CODE (X) == SYMBOL_REF && tls_symbolic_operand (X))	\
 || (GET_CODE (X) == CONST && tls_symbolic_reference_mentioned_p (X)))


/* Condition codes.  */

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  */
#define SELECT_CC_MODE(OP, X, Y) s390_select_ccmode ((OP), (X), (Y))
 
/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
extern struct rtx_def *s390_compare_op0, *s390_compare_op1;


/* Relative costs of operations.  */

/* A part of a C `switch' statement that describes the relative costs
   of constant RTL expressions.  It must contain `case' labels for
   expression codes `const_int', `const', `symbol_ref', `label_ref'
   and `const_double'.  Each case must ultimately reach a `return'
   statement to return the relative cost of the use of that kind of
   constant value in an expression.  The cost may depend on the
   precise value of the constant, which is available for examination
   in X, and the rtx code of the expression in which it is contained,
   found in OUTER_CODE.

   CODE is the expression code--redundant, since it can be obtained
   with `GET_CODE (X)'.  */
/* Force_const_mem does not work out of reload, because the saveable_obstack
   is set to reload_obstack, which does not live long enough. 
   Because of this we cannot use force_const_mem in addsi3.
   This leads to problems with gen_add2_insn with a constant greater
   than a short. Because of that we give an addition of greater
   constants a cost of 3 (reload1.c 10096).  */

#define CONST_COSTS(RTX, CODE, OUTER_CODE)                      \
  case CONST:                                                   \
    if ((GET_CODE (XEXP (RTX, 0)) == MINUS) &&                  \
	(GET_CODE (XEXP (XEXP (RTX, 0), 1)) != CONST_INT))      \
     return 1000;                                               \
  case CONST_INT:                                               \
       if ((OUTER_CODE == PLUS) &&                              \
	   ((INTVAL (RTX) > 32767) ||                           \
	   (INTVAL (RTX) < -32768))) 	                        \
         return COSTS_N_INSNS (3);                              \
  case LABEL_REF:                                               \
  case SYMBOL_REF:                                              \
  case CONST_DOUBLE:                                            \
    return 0;                                                   \


/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast
   instructions.  OUTER_CODE is the code of the expression in which X
   is contained.  */

#define RTX_COSTS(X, CODE, OUTER_CODE)                                  \
  case ASHIFT:                                                          \
  case ASHIFTRT:                                                        \
  case LSHIFTRT:                                                        \
  case PLUS:                                                            \
  case AND:                                                             \
  case IOR:                                                             \
  case XOR:                                                             \
  case MINUS:                                                           \
  case NEG:                                                             \
  case NOT:                                                             \
    return COSTS_N_INSNS (1);                                           \
  case MULT:                                                            \
    if (GET_MODE (XEXP (X, 0)) == DImode)                               \
      return COSTS_N_INSNS (40);                                        \
    else                                                                \
      return COSTS_N_INSNS (7);                                         \
  case DIV:                                                             \
  case UDIV:                                                            \
  case MOD:                                                             \
  case UMOD:                                                            \
    return COSTS_N_INSNS (33);


/* An expression giving the cost of an addressing mode that contains
   ADDRESS.  If not defined, the cost is computed from the ADDRESS
   expression and the `CONST_COSTS' values.  */
#define ADDRESS_COST(RTX) s390_address_cost ((RTX))

/* On s390, copy between fprs and gprs is expensive.  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)                        \
  ((   (   reg_classes_intersect_p ((CLASS1), GENERAL_REGS)		\
        && reg_classes_intersect_p ((CLASS2), FP_REGS))			\
    || (   reg_classes_intersect_p ((CLASS1), FP_REGS)			\
        && reg_classes_intersect_p ((CLASS2), GENERAL_REGS))) ? 10 : 1)

/* A C expression for the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.  */
#define MEMORY_MOVE_COST(M, C, I) 1

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */
#define BRANCH_COST 1

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* The maximum number of bytes that a single instruction can move quickly
   between memory and registers or between two memory locations. */
#define MOVE_MAX (TARGET_64BIT ? 16 : 8)
#define MAX_MOVE_MAX 16

/* Determine whether to use move_by_pieces or block move insn.  */
#define MOVE_BY_PIECES_P(SIZE, ALIGN)		\
  ( (SIZE) == 1 || (SIZE) == 2 || (SIZE) == 4	\
    || (TARGET_64BIT && (SIZE) == 8) )

/* Determine whether to use clear_by_pieces or block clear insn.  */
#define CLEAR_BY_PIECES_P(SIZE, ALIGN)		\
  ( (SIZE) == 1 || (SIZE) == 2 || (SIZE) == 4	\
    || (TARGET_64BIT && (SIZE) == 8) )

/* Don't perform CSE on function addresses.  */
#define NO_FUNCTION_CSE


/* Sections.  */

/* Output before read-only data.  */
#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable (initialized) data.  */
#define DATA_SECTION_ASM_OP ".data"

/* Output before writable (uninitialized) data.  */
#define BSS_SECTION_ASM_OP ".bss"

/* S/390 constant pool breaks the devices in crtstuff.c to control section
   in where code resides.  We have to write it as asm code.  */
#ifndef __s390x__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC) \
    asm (SECTION_OP "\n\
	bras\t%r2,1f\n\
0:	.long\t" USER_LABEL_PREFIX #FUNC " - 0b\n\
1:	l\t%r3,0(%r2)\n\
	bas\t%r14,0(%r3,%r2)\n\
	.previous");
#endif


/* Position independent code.  */

extern int flag_pic; 

#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? 12 : INVALID_REGNUM)

#define LEGITIMATE_PIC_OPERAND_P(X)  legitimate_pic_operand_p (X)


/* Assembler file format.  */

/* Character to start a comment.  */
#define ASM_COMMENT_START "#"

/* Declare an uninitialized external linkage data object.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".globl "

/* Advance the location counter to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE, LOG) \
  if ((LOG)) fprintf ((FILE), "\t.align\t%d\n", 1 << (LOG))

/* Advance the location counter by SIZE bytes.  */
#define ASM_OUTPUT_SKIP(FILE, SIZE) \
  fprintf ((FILE), "\t.set\t.,.+%u\n", (SIZE))

/* Output a reference to a user-level label named NAME.  */
#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  asm_fprintf ((FILE), "%U%s", (*targetm.strip_name_encoding) (NAME))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
  ((OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
   sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* The LOCAL_LABEL_PREFIX variable is used by dbxelf.h.  */
#define LOCAL_LABEL_PREFIX "."

/* Either simplify a location expression, or return the original.  */
#define ASM_SIMPLIFY_DWARF_ADDR(X) \
  s390_simplify_dwarf_addr (X)

/* How to refer to registers in assembler output.  This sequence is
   indexed by compiler's hard-register-number (see above).  */
#define REGISTER_NAMES							\
{ "%r0",  "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",	\
  "%r8",  "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",	\
  "%f0",  "%f2",  "%f4",  "%f6",  "%f1",  "%f3",  "%f5",  "%f7",	\
  "%f8",  "%f10", "%f12", "%f14", "%f9", "%f11", "%f13", "%f15",	\
  "%ap",  "%cc",  "%fp"							\
}

/* Emit a dtp-relative reference to a TLS variable.  */
#ifdef HAVE_AS_TLS
#define ASM_OUTPUT_DWARF_DTPREL(FILE, SIZE, X) \
  s390_output_dwarf_dtprel (FILE, SIZE, X)
#endif

/* Print operand X (an rtx) in assembler syntax to file FILE.  */
#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* Output an element of a case-vector that is absolute.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)				\
do {									\
  char buf[32];								\
  fputs (integer_asm_op (UNITS_PER_WORD, TRUE), (FILE));		\
  ASM_GENERATE_INTERNAL_LABEL (buf, "L", (VALUE));			\
  assemble_name ((FILE), buf);						\
  fputc ('\n', (FILE));							\
} while (0)

/* Output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)		\
do {									\
  char buf[32];								\
  fputs (integer_asm_op (UNITS_PER_WORD, TRUE), (FILE));		\
  ASM_GENERATE_INTERNAL_LABEL (buf, "L", (VALUE));			\
  assemble_name ((FILE), buf);						\
  fputc ('-', (FILE));							\
  ASM_GENERATE_INTERNAL_LABEL (buf, "L", (REL));			\
  assemble_name ((FILE), buf);						\
  fputc ('\n', (FILE));							\
} while (0)


/* Constant Pool for all symbols operands which are changed with
   force_const_mem during insn generation (expand_insn).  */

extern int s390_pool_count;
extern int s390_nr_constants;

#define ASM_OUTPUT_POOL_PROLOGUE(FILE, FUNNAME, fndecl, size)  	        \
{								       	\
  struct pool_constant *pool;					       	\
								        \
    if (s390_pool_count == -1)                                        	\
     {							                \
       s390_nr_constants = 0;				                \
       for (pool = first_pool; pool; pool = pool->next)	                \
	 if (pool->mark) s390_nr_constants++;		                \
       return;                                      	                \
     }                                                                  \
}

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, EXP, MODE, ALIGN, LABELNO, WIN) \
{									    \
  fprintf (FILE, ".LC%d:\n", LABELNO);					    \
									    \
  /* Output the value of the constant itself.  */			    \
  switch (GET_MODE_CLASS (MODE))					    \
    {									    \
    case MODE_FLOAT:							    \
      if (GET_CODE (EXP) != CONST_DOUBLE)				    \
	abort ();							    \
									    \
      REAL_VALUE_FROM_CONST_DOUBLE (r, EXP);				    \
      assemble_real (r, MODE, ALIGN);					    \
      break;								    \
									    \
    case MODE_INT:							    \
    case MODE_PARTIAL_INT:						    \
      if (GET_CODE (EXP) == CONST					    \
	  || GET_CODE (EXP) == SYMBOL_REF				    \
	  || GET_CODE (EXP) == LABEL_REF)				    \
        {								    \
	  fputs (integer_asm_op (UNITS_PER_WORD, TRUE), FILE);		    \
          s390_output_symbolic_const (FILE, EXP);			    \
          fputc ('\n', (FILE));						    \
	}								    \
      else								    \
	{								    \
	  assemble_integer (EXP, GET_MODE_SIZE (MODE), ALIGN, 1);	    \
	  if (GET_MODE_SIZE (MODE) == 1)				    \
	    ASM_OUTPUT_SKIP ((FILE), 1);				    \
	}								    \
      break;								    \
									    \
    default:								    \
      abort ();								    \
    }									    \
  goto WIN;								    \
}


/* Miscellaneous parameters.  */

/* Define the codes that are matched by predicates in aux-output.c.  */
#define PREDICATE_CODES							\
  {"s_operand",       { SUBREG, MEM }},					\
  {"s_imm_operand",   { CONST_INT, CONST_DOUBLE, SUBREG, MEM }},	\
  {"bras_sym_operand",{ SYMBOL_REF, CONST }},				\
  {"larl_operand",    { SYMBOL_REF, CONST, CONST_INT, CONST_DOUBLE }},	\
  {"load_multiple_operation", {PARALLEL}},			        \
  {"store_multiple_operation", {PARALLEL}},			        \
  {"const0_operand",  { CONST_INT, CONST_DOUBLE }},			\
  {"consttable_operand", { SYMBOL_REF, LABEL_REF, CONST, 		\
			   CONST_INT, CONST_DOUBLE }},			\
  {"s390_plus_operand", { PLUS }},

/* Specify the machine mode that this machine uses for the index in the
   tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_64BIT ? DImode : SImode)

/* Load from integral MODE < SI from memory into register makes sign_extend
   or zero_extend  
   In our case sign_extension happens for Halfwords, other no extension.  */
#define LOAD_EXTEND_OP(MODE) 					\
(TARGET_64BIT ? ((MODE) == QImode ? ZERO_EXTEND :               \
                 (MODE) == HImode ? SIGN_EXTEND : NIL)          \
              : ((MODE) == HImode ? SIGN_EXTEND : NIL))

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)  1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode ((enum machine_mode) (TARGET_64BIT ? DImode : SImode))

/* A function address in a call instruction is a byte address (for
   indexing purposes) so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* This macro definition sets up a default value for `main' to return.  */
#define DEFAULT_MAIN_RETURN  c_expand_return (integer_zero_node)

/* In rare cases, correct code generation requires extra machine dependent
   processing between the second jump optimization pass and delayed branch
   scheduling.  On those machines, define this macro as a C statement to act on
   the code starting at INSN.  */
#define MACHINE_DEPENDENT_REORG(INSN) s390_machine_dependent_reorg (INSN)

#endif 
