/* Definitions of target machine for Mitsubishi D30V.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef GCC_D30V_H

/* D30V specific macros */

/* Align an address */
#define D30V_ALIGN(addr,align) (((addr) + (align) - 1) & ~((align) - 1))


/* Driver configuration */

/* Defined in svr4.h.  */
/* #define SWITCH_TAKES_ARG(CHAR) */

/* Defined in svr4.h.  */
/* #define WORD_SWITCH_TAKES_ARG(NAME) */

/* Defined in svr4.h.  */
#undef	ASM_SPEC
#define ASM_SPEC "\
%{!mno-asm-optimize: %{O*: %{!O0: -O} %{O0: %{masm-optimize: -O}}}} \
%{v} %{n} %{T} %{Ym,*} %{Yd,*} %{Wa,*:%*}"

/* Defined in svr4.h.  */
#undef	LINK_SPEC
#define LINK_SPEC "\
%{h*} %{v:-V} \
%{b} %{Wl,*:%*} \
%{static:-dn -Bstatic} \
%{shared:-G -dy -z text} \
%{symbolic:-Bsymbolic -G -dy -z text} \
%{G:-G} \
%{YP,*} \
%{Qy:} %{!Qn:-Qy} \
%{mextmem: -m d30v_e} %{mextmemory: -m d30v_e} %{monchip: -m d30v_o}"

/* Defined in svr4.h.  */
#undef	LIB_SPEC
#define LIB_SPEC "--start-group -lsim -lc --end-group"

/* Defined in svr4.h.  */
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crtbegin%O%s"

/* Defined in svr4.h.  */
#undef	ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s"

/* Defined in svr4.h for host compilers.  */
/* #define MD_EXEC_PREFIX "" */

/* Defined in svr4.h for host compilers.  */
/* #define MD_STARTFILE_PREFIX "" */


/* Run-time target specifications */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__D30V__");		\
      builtin_assert ("machine=d30v");		\
    }						\
  while (0)

/* This declaration should be present.  */
extern int target_flags;

#define MASK_NO_COND_MOVE	0x00000001	/* disable conditional moves */

#define MASK_DEBUG_ARG		0x10000000	/* debug argument handling */
#define MASK_DEBUG_STACK	0x20000000	/* debug stack allocations */
#define MASK_DEBUG_ADDR		0x40000000	/* debug GO_IF_LEGITIMATE_ADDRESS */

#define TARGET_NO_COND_MOVE	(target_flags & MASK_NO_COND_MOVE)
#define TARGET_DEBUG_ARG	(target_flags & MASK_DEBUG_ARG)
#define TARGET_DEBUG_STACK	(target_flags & MASK_DEBUG_STACK)
#define TARGET_DEBUG_ADDR	(target_flags & MASK_DEBUG_ADDR)

#define TARGET_COND_MOVE	(! TARGET_NO_COND_MOVE)

/* Default switches used.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#define TARGET_SWITCHES							\
{									\
  { "cond-move",	-MASK_NO_COND_MOVE,				\
      N_("Enable use of conditional move instructions") },		\
									\
  { "no-cond-move",	MASK_NO_COND_MOVE,				\
      N_("Disable use of conditional move instructions") },		\
									\
  { "debug-arg",	 MASK_DEBUG_ARG,				\
      N_("Debug argument support in compiler") },			\
									\
  { "debug-stack",	 MASK_DEBUG_STACK,				\
      N_("Debug stack support in compiler") },				\
									\
  { "debug-addr",	 MASK_DEBUG_ADDR,				\
      N_("Debug memory address support in compiler") },			\
									\
  { "asm-optimize",	 0,						\
      N_("Make adjacent short instructions parallel if possible") },	\
									\
  { "no-asm-optimize",	 0,						\
      N_("Do not make adjacent short instructions parallel") },	\
									\
  { "extmem",		 0,						\
      N_("Link programs/data to be in external memory by default") },	\
									\
  { "extmemory",	 0,						\
      N_("Link programs/data to be in external memory by default") },	\
									\
  { "onchip",		 0,						\
      N_("Link programs/data to be in onchip memory by default") },	\
									\
  { "",			 TARGET_DEFAULT, "" },				\
}

#define TARGET_OPTIONS							\
{									\
  {"branch-cost=",  &d30v_branch_cost_string,				\
     N_("Change the branch costs within the compiler"), 0},		\
									\
  {"cond-exec=",    &d30v_cond_exec_string,				\
     N_("Change the threshold for conversion to conditional execution"), 0}, \
}

#define TARGET_VERSION fprintf (stderr, " d30v")

#define OVERRIDE_OPTIONS override_options ()

#define CAN_DEBUG_WITHOUT_FP


/* Storage Layout */

#define BITS_BIG_ENDIAN 1

#define BYTES_BIG_ENDIAN 1

#define WORDS_BIG_ENDIAN 1

#define UNITS_PER_WORD 4

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)				\
do {									\
  if (GET_MODE_CLASS (MODE) == MODE_INT					\
      && GET_MODE_SIZE (MODE) < 4)					\
    (MODE) = SImode;							\
} while (0)

#define PARM_BOUNDARY 32

#define STACK_BOUNDARY 64

#define FUNCTION_BOUNDARY 64

#define BIGGEST_ALIGNMENT 64

/* Defined in svr4.h.  */
/* #define MAX_OFILE_ALIGNMENT */

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define STRICT_ALIGNMENT 1

/* Defined in svr4.h.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32

#define SHORT_TYPE_SIZE 16

#define LONG_TYPE_SIZE 32

#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32

#define DOUBLE_TYPE_SIZE 64

#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

/* Defined in svr4.h.  */
/* #define SIZE_TYPE */

/* Defined in svr4.h.  */
/* #define PTRDIFF_TYPE */

/* Defined in svr4.h.  */
/* #define WCHAR_TYPE */

/* Defined in svr4.h.  */
/* #define WCHAR_TYPE_SIZE */


/* D30V register layout.  */

/* Return true if a value is inside a range */
#define IN_RANGE_P(VALUE, LOW, HIGH) \
  (((unsigned)((VALUE) - (LOW))) <= ((unsigned)((HIGH) - (LOW))))

/* General purpose registers.  */
#define GPR_FIRST	0			/* First gpr */
#define GPR_LAST	(GPR_FIRST + 63)	/* Last gpr */
#define GPR_R0		GPR_FIRST		/* R0, constant 0 */
#define GPR_ARG_FIRST	(GPR_FIRST + 2)		/* R2, first argument reg */
#define GPR_ARG_LAST	(GPR_FIRST + 17)	/* R17, last argument reg */
#define GPR_RET_VALUE	GPR_ARG_FIRST		/* R2, function return reg */
#define GPR_ATMP_FIRST	(GPR_FIRST + 20)	/* R20, tmp to save accs */
#define GPR_ATMP_LAST	(GPR_FIRST + 21)	/* R21, tmp to save accs */
#define GPR_STACK_TMP	(GPR_FIRST + 22)	/* R22, tmp for saving stack */
#define GPR_RES_FIRST	(GPR_FIRST + 32)	/* R32, first reserved reg */
#define GPR_RES_LAST	(GPR_FIRST + 35)	/* R35, last reserved reg */
#define GPR_FP		(GPR_FIRST + 61)	/* Frame pointer */
#define GPR_LINK	(GPR_FIRST + 62)	/* Return address register */
#define GPR_SP		(GPR_FIRST + 63)	/* Stack pointer */

/* Argument register that is eliminated in favor of the frame and/or stack
   pointer.  Also add register to point to where the return address is
   stored.  */
#define SPECIAL_REG_FIRST		(GPR_LAST + 1)
#define SPECIAL_REG_LAST		(SPECIAL_REG_FIRST)
#define ARG_POINTER_REGNUM		(SPECIAL_REG_FIRST + 0)
#define SPECIAL_REG_P(R)		((R) == SPECIAL_REG_FIRST)

#define GPR_OR_SPECIAL_REG_P(R)		IN_RANGE_P (R, GPR_FIRST, SPECIAL_REG_LAST)
#define GPR_P(R)			IN_RANGE_P (R, GPR_FIRST, GPR_LAST)
#define GPR_OR_PSEUDO_P(R)		(GPR_OR_SPECIAL_REG_P (R)	\
					 || (R) >= FIRST_PSEUDO_REGISTER)

/* Flag bits.  */
#define FLAG_FIRST	(SPECIAL_REG_LAST + 1)	/* First flag */
#define FLAG_LAST	(FLAG_FIRST + 7)	/* Last flag */
#define FLAG_F0		(FLAG_FIRST)		/* F0, used in prediction */
#define FLAG_F1		(FLAG_FIRST + 1)	/* F1, used in prediction */
#define FLAG_F2		(FLAG_FIRST + 2)	/* F2, general flag */
#define FLAG_F3		(FLAG_FIRST + 3)	/* F3, general flag */
#define FLAG_SAT	(FLAG_FIRST + 4)	/* F4, saturation flag */
#define FLAG_OVERFLOW	(FLAG_FIRST + 5)	/* F5, overflow flag */
#define FLAG_ACC_OVER	(FLAG_FIRST + 6)	/* F6, accumulated overflow */
#define FLAG_CARRY	(FLAG_FIRST + 7)	/* F7, carry/borrow flag */
#define FLAG_BORROW	FLAG_CARRY

#define FLAG_P(R)		IN_RANGE_P (R, FLAG_FIRST, FLAG_LAST)
#define FLAG_OR_PSEUDO_P(R)	(FLAG_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

#define BR_FLAG_P(R)		IN_RANGE_P (R, FLAG_F0, FLAG_F1)
#define BR_FLAG_OR_PSEUDO_P(R)	(BR_FLAG_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

/* Accumulators */
#define ACCUM_FIRST	(FLAG_LAST + 1)		/* First accumulator */
#define ACCUM_A0	ACCUM_FIRST		/* Register A0 */
#define ACCUM_A1	(ACCUM_FIRST + 1)	/* Register A1 */
#define ACCUM_LAST	(ACCUM_FIRST + 1)	/* Last accumulator */

#define ACCUM_P(R)		IN_RANGE_P (R, ACCUM_FIRST, ACCUM_LAST)
#define ACCUM_OR_PSEUDO_P(R)	(ACCUM_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

/* Special registers.  Note, we only define the registers that can actually
   be used.  */
#define CR_FIRST	(ACCUM_LAST + 1)	/* First CR */
#define CR_LAST		(CR_FIRST + 14)		/* Last CR */
#define CR_PSW		(CR_FIRST + 0)		/* CR0, Program status word */
#define CR_BPSW		(CR_FIRST + 1)		/* CR1, Backup PSW */
#define CR_PC		(CR_FIRST + 2)		/* CR2, Program counter */
#define CR_BPC		(CR_FIRST + 3)		/* CR3, Backup PC */
#define CR_DPSW		(CR_FIRST + 4)		/* CR4, Debug PSW */
#define CR_DPC		(CR_FIRST + 5)		/* CR5, Debug PC */
#define CR_RPT_C	(CR_FIRST + 6)		/* CR7, loop count register */
#define CR_RPT_S	(CR_FIRST + 7)		/* CR8, loop start address */
#define CR_RPT_E	(CR_FIRST + 8)		/* CR9, loop end address */
#define CR_MOD_S	(CR_FIRST + 9)		/* CR10, modulo address start*/
#define CR_MOD_E	(CR_FIRST + 10)		/* CR11, modulo address */
#define CR_IBA		(CR_FIRST + 11)		/* CR14, Interrupt break addr */
#define CR_EIT_VB	(CR_FIRST + 12)		/* CR15, EIT vector address */
#define CR_INT_S	(CR_FIRST + 13)		/* CR16, Interrupt status */
#define CR_INT_M	(CR_FIRST + 14)		/* CR17, Interrupt mask */

#define CR_P(R)			IN_RANGE_P (R, CR_FIRST, CR_LAST)
#define CR_OR_PSEUDO_P(R)	(CR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)


/* Register Basics */

/* Number of hardware registers known to the compiler.  They receive numbers 0
   through `FIRST_PSEUDO_REGISTER-1'; thus, the first pseudo register's number
   really is assigned the number `FIRST_PSEUDO_REGISTER'.  */
#define FIRST_PSEUDO_REGISTER (CR_LAST + 1)

/* An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation.  These would include the stack pointer, the frame pointer
   (except on machines where that can be used as a general register when no
   frame pointer is needed), the program counter on machines where that is
   considered one of the addressable registers, and any other numbered register
   with a standard use.

   This information is expressed as a sequence of numbers, separated by commas
   and surrounded by braces.  The Nth number is 1 if register N is fixed, 0
   otherwise.

   The table initialized from this macro, and the table initialized by the
   following one, may be overridden at run time either automatically, by the
   actions of the macro `CONDITIONAL_REGISTER_USAGE', or by the user with the
   command options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.  */
#define FIXED_REGISTERS							\
{									\
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* R0  - R15 */	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,  /* R16 - R31 */	\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* R32 - R47 */	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  /* R48 - R63 */	\
  1,						   /* ARG ptr */	\
  0, 0, 0, 0, 1, 1, 1, 1,			   /* F0 - F7 */	\
  0, 0,						   /* A0 - A1 */	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	   /* CRs */		\
}

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */
#define CALL_USED_REGISTERS		        			\
{					        			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* R0  - R15 */	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* R16 - R31 */	\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  /* R32 - R47 */	\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  /* R48 - R63 */	\
  1,						   /* ARG ptr */	\
  1, 1, 1, 1, 1, 1, 1, 1,			   /* F0 - F7 */	\
  1, 0,						   /* A0 - A1 */	\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	   /* CRs */		\
}


/* Order of allocation of registers */

/* If defined, an initializer for a vector of integers, containing the numbers
   of hard registers in the order in which GCC should prefer to use them
   (from most preferred to least).

   If this macro is not defined, registers are used lowest numbered first (all
   else being equal).

   One use of this macro is on machines where the highest numbered registers
   must always be saved and the save-multiple-registers instruction supports
   only sequences of consecutive registers.  On such machines, define
   `REG_ALLOC_ORDER' to be an initializer that lists the highest numbered
   allocatable register first.  */

#define REG_ALLOC_ORDER							\
{									\
  /* volatile registers */						\
  GPR_FIRST + 2,    GPR_FIRST + 3,    GPR_FIRST + 4,    GPR_FIRST + 5,	\
  GPR_FIRST + 6,    GPR_FIRST + 7,    GPR_FIRST + 8,    GPR_FIRST + 9,	\
  GPR_FIRST + 10,   GPR_FIRST + 11,   GPR_FIRST + 12,   GPR_FIRST + 13,	\
  GPR_FIRST + 14,   GPR_FIRST + 15,   GPR_FIRST + 16,   GPR_FIRST + 17,	\
  GPR_FIRST + 18,   GPR_FIRST + 19,   GPR_FIRST + 20,   GPR_FIRST + 21,	\
  GPR_FIRST + 22,   GPR_FIRST + 23,   GPR_FIRST + 24,   GPR_FIRST + 25, \
  GPR_FIRST + 1,							\
									\
  /* saved registers */							\
  GPR_FIRST + 34,   GPR_FIRST + 35,   GPR_FIRST + 36,   GPR_FIRST + 37,	\
  GPR_FIRST + 38,   GPR_FIRST + 39,   GPR_FIRST + 40,   GPR_FIRST + 41,	\
  GPR_FIRST + 42,   GPR_FIRST + 43,   GPR_FIRST + 44,   GPR_FIRST + 45,	\
  GPR_FIRST + 46,   GPR_FIRST + 47,   GPR_FIRST + 48,   GPR_FIRST + 49,	\
  GPR_FIRST + 50,   GPR_FIRST + 51,   GPR_FIRST + 52,   GPR_FIRST + 53,	\
  GPR_FIRST + 54,   GPR_FIRST + 55,   GPR_FIRST + 56,   GPR_FIRST + 57,	\
  GPR_FIRST + 58,   GPR_FIRST + 59,   GPR_FIRST + 60,   GPR_FIRST + 61,	\
  GPR_FIRST + 62,							\
									\
  /* flags */								\
  FLAG_F2,          FLAG_F3,          FLAG_F0,          FLAG_F1,	\
  FLAG_SAT,         FLAG_OVERFLOW,    FLAG_ACC_OVER,    FLAG_CARRY,	\
									\
  /* accumultors */							\
  ACCUM_FIRST + 0,  ACCUM_FIRST + 1,					\
									\
  /* fixed registers */							\
  GPR_FIRST + 0,    GPR_FIRST + 26,   GPR_FIRST + 27,   GPR_FIRST + 28,	\
  GPR_FIRST + 29,   GPR_FIRST + 30,   GPR_FIRST + 31,   GPR_FIRST + 32,	\
  GPR_FIRST + 33,   GPR_FIRST + 63,					\
  CR_PSW,	    CR_BPSW,	      CR_PC,		CR_BPC,		\
  CR_DPSW,	    CR_DPC,	      CR_RPT_C,		CR_RPT_S,	\
  CR_RPT_E,	    CR_MOD_S,	      CR_MOD_E,		CR_IBA,		\
  CR_EIT_VB,	    CR_INT_S,	      CR_INT_M,				\
  ARG_POINTER_REGNUM,							\
}


/* How Values Fit in Registers */

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.

   On a machine where all registers are exactly one word, a suitable definition
   of this macro is

        #define HARD_REGNO_NREGS(REGNO, MODE)            \
           ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
            / UNITS_PER_WORD))  */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
(ACCUM_P (REGNO) ? ((GET_MODE_SIZE (MODE) + 2*UNITS_PER_WORD - 1)	\
		    / (2*UNITS_PER_WORD))				\
		 : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)		\
		    / UNITS_PER_WORD))

/* A C expression that is nonzero if it is permissible to store a value of mode
   MODE in hard register number REGNO (or in several registers starting with
   that one).  For a machine where all registers are equivalent, a suitable
   definition is

        #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

   It is not necessary for this macro to check for the numbers of fixed
   registers, because the allocation mechanism considers them to be always
   occupied.

   On some machines, double-precision values must be kept in even/odd register
   pairs.  The way to implement that is to define this macro to reject odd
   register numbers for such modes.

   The minimum requirement for a mode to be OK in a register is that the
   `movMODE' instruction pattern support moves between the register and any
   other hard register for which the mode is OK; and that moving a value into
   the register and back out not alter it.

   Since the same instruction used to move `SImode' will work for all narrower
   integer modes, it is not necessary on any machine for `HARD_REGNO_MODE_OK'
   to distinguish between these modes, provided you define patterns `movhi',
   etc., to take advantage of this.  This is useful because of the interaction
   between `HARD_REGNO_MODE_OK' and `MODES_TIEABLE_P'; it is very desirable for
   all integer modes to be tieable.

   Many machines have special registers for floating point arithmetic.  Often
   people assume that floating point machine modes are allowed only in floating
   point registers.  This is not true.  Any registers that can hold integers
   can safely *hold* a floating point machine mode, whether or not floating
   arithmetic can be done on it in those registers.  Integer move instructions
   can be used to move the values.

   On some machines, though, the converse is true: fixed-point machine modes
   may not go in floating registers.  This is true if the floating registers
   normalize any value stored in them, because storing a non-floating value
   there would garble it.  In this case, `HARD_REGNO_MODE_OK' should reject
   fixed-point machine modes in floating registers.  But if the floating
   registers do not automatically normalize, if you can store any bit pattern
   in one and retrieve it unchanged without a trap, then any machine mode may
   go in a floating register, so you can define this macro to say so.

   The primary significance of special floating registers is rather that they
   are the registers acceptable in floating point arithmetic instructions.
   However, this is of no concern to `HARD_REGNO_MODE_OK'.  You handle it by
   writing the proper constraints for those instructions.

   On some machines, the floating registers are especially slow to access, so
   that it is better to store a value in a stack frame than in such a register
   if floating point arithmetic is not being done.  As long as the floating
   registers are not in class `GENERAL_REGS', they will not be used unless some
   pattern's constraint asks for one.  */

extern unsigned char hard_regno_mode_ok[][FIRST_PSEUDO_REGISTER];
#define HARD_REGNO_MODE_OK(REGNO, MODE) hard_regno_mode_ok[ (int)MODE ][ REGNO ]

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */

extern unsigned char modes_tieable_p[];
#define MODES_TIEABLE_P(MODE1, MODE2) \
  modes_tieable_p[ (((int)(MODE1)) * (NUM_MACHINE_MODES)) + (int)(MODE2) ]

/* Define this macro if the compiler should avoid copies to/from CCmode
   registers.  You should only define this macro if support fo copying to/from
   CCmode is incomplete.  */
   
/* On the D30V, copying to/from CCmode is complete, but since there are only
   two CC registers usable for conditional tests, this helps gcse not compound
   the reload problem.  */
#define AVOID_CCMODE_COPIES


/* Register Classes */

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there are.

   Each register class has a number, which is the value of casting the class
   name to type `int'.  The number serves as an index in many of the tables
   described below.  */
enum reg_class
{
  NO_REGS,
  REPEAT_REGS,
  CR_REGS,
  ACCUM_REGS,
  OTHER_FLAG_REGS,
  F0_REGS,
  F1_REGS,
  BR_FLAG_REGS,
  FLAG_REGS,
  EVEN_REGS,
  GPR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS GPR_REGS

/* The number of distinct register classes, defined as follows:

        #define N_REG_CLASSES (int) LIM_REG_CLASSES  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "REPEAT_REGS",							\
  "CR_REGS",								\
  "ACCUM_REGS",								\
  "OTHER_FLAG_REGS",							\
  "F0_REGS",								\
  "F1_REGS",								\
  "BR_FLAG_REGS",							\
  "FLAG_REGS",								\
  "EVEN_REGS",								\
  "GPR_REGS",								\
  "ALL_REGS",								\
}

/* Create mask bits for 3rd word of REG_CLASS_CONTENTS */
#define MASK_WORD3(REG) ((long)1 << ((REG) - 64))

#define NO_MASK		0
#define REPEAT_MASK	MASK_WORD3 (CR_RPT_C)
#define CR_MASK		(MASK_WORD3 (CR_PSW)	 | MASK_WORD3 (CR_BPSW)	  \
			 | MASK_WORD3 (CR_PC)	 | MASK_WORD3 (CR_BPC)	  \
			 | MASK_WORD3 (CR_DPSW)	 | MASK_WORD3 (CR_DPC)	  \
			 | MASK_WORD3 (CR_RPT_C) | MASK_WORD3 (CR_RPT_S)  \
			 | MASK_WORD3 (CR_RPT_E) | MASK_WORD3 (CR_MOD_S)  \
			 | MASK_WORD3 (CR_MOD_E) | MASK_WORD3 (CR_IBA)	  \
			 | MASK_WORD3 (CR_EIT_VB) | MASK_WORD3 (CR_INT_S) \
			 | MASK_WORD3 (CR_INT_M))

#define ACCUM_MASK	(MASK_WORD3 (ACCUM_A0)	 | MASK_WORD3 (ACCUM_A1))
#define OTHER_FLAG_MASK	(MASK_WORD3 (FLAG_F2)	 | MASK_WORD3 (FLAG_F3)	\
			 | MASK_WORD3 (FLAG_SAT) | MASK_WORD3 (FLAG_OVERFLOW) \
			 | MASK_WORD3 (FLAG_ACC_OVER) | MASK_WORD3 (FLAG_CARRY))

#define F0_MASK		MASK_WORD3 (FLAG_F0)
#define F1_MASK		MASK_WORD3 (FLAG_F1)
#define BR_FLAG_MASK	(F0_MASK | F1_MASK)
#define FLAG_MASK	(BR_FLAG_MASK | OTHER_FLAG_MASK)
#define SPECIAL_MASK	MASK_WORD3 (ARG_POINTER_REGNUM)

#define ALL_MASK	(CR_MASK | ACCUM_MASK | FLAG_MASK | SPECIAL_MASK)

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not suffice.
   Then the integers are replaced by sub-initializers, braced groupings
   containing several integers.  Each sub-initializer must be suitable as an
   initializer for the type `HARD_REG_SET' which is defined in
   `hard-reg-set.h'.  */
#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, NO_MASK },		/* NO_REGS */		\
  { 0x00000000, 0x00000000, REPEAT_MASK },	/* REPEAT_REGS */	\
  { 0x00000000, 0x00000000, CR_MASK },		/* CR_REGS */		\
  { 0x00000000, 0x00000000, ACCUM_MASK },	/* ACCUM_REGS */	\
  { 0x00000000, 0x00000000, OTHER_FLAG_MASK },	/* OTHER_FLAG_REGS */	\
  { 0x00000000, 0x00000000, F0_MASK },		/* F0_REGS */		\
  { 0x00000000, 0x00000000, F1_MASK },		/* F1_REGS */		\
  { 0x00000000, 0x00000000, BR_FLAG_MASK },	/* BR_FLAG_REGS */	\
  { 0x00000000, 0x00000000, FLAG_MASK },	/* FLAG_REGS */		\
  { 0xfffffffc, 0x3fffffff, NO_MASK },		/* EVEN_REGS */		\
  { 0xffffffff, 0xffffffff, SPECIAL_MASK },	/* GPR_REGS */		\
  { 0xffffffff, 0xffffffff, ALL_MASK },		/* ALL_REGS */		\
}

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */

extern enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[ (REGNO) ]

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GPR_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS GPR_REGS

/* A C expression which defines the machine-dependent operand constraint
   letters for register classes.  If CHAR is such a letter, the value should be
   the register class corresponding to it.  Otherwise, the value should be
   `NO_REGS'.  The register letter `r', corresponding to class `GENERAL_REGS',
   will not be passed to this macro; you do not need to handle it.

   The following letters are unavailable, due to being used as
   constraints:
	'0'..'9'
	'<', '>'
	'E', 'F', 'G', 'H'
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P'
	'Q', 'R', 'S', 'T', 'U'
	'V', 'X'
	'g', 'i', 'm', 'n', 'o', 'p', 'r', 's' */

extern enum reg_class reg_class_from_letter[256];
#define REG_CLASS_FROM_LETTER(CHAR) reg_class_from_letter[(unsigned char)(CHAR)]

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.  */

#define REGNO_OK_FOR_BASE_P(NUM) 					\
((NUM) < FIRST_PSEUDO_REGISTER						\
 ? GPR_P (NUM)								\
 : (reg_renumber[NUM] >= 0 && GPR_P (reg_renumber[NUM])))


/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */

#define REGNO_OK_FOR_INDEX_P(NUM)					\
((NUM) < FIRST_PSEUDO_REGISTER						\
 ? GPR_P (NUM)								\
 : (reg_renumber[NUM] >= 0 && GPR_P (reg_renumber[NUM])))

/* A C expression that places additional restrictions on the register class to
   use when it is necessary to copy value X into a register in class CLASS.
   The value is a register class; perhaps CLASS, or perhaps another, smaller
   class.  On many machines, the following definition is safe:

        #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

   Sometimes returning a more restrictive class makes better code.  For
   example, on the 68000, when X is an integer constant that is in range for a
   `moveq' instruction, the value of this macro is always `DATA_REGS' as long
   as CLASS includes the data registers.  Requiring a data register guarantees
   that a `moveq' will be used.

   If X is a `const_double', by returning `NO_REGS' you can force X into a
   memory constant.  This is useful on certain machines where immediate
   floating values cannot be loaded into certain kinds of registers.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) CLASS

/* Many machines have some registers that cannot be copied directly to or from
   memory or even from other types of registers.  An example is the `MQ'
   register, which on most machines, can only be copied to or from general
   registers, but not memory.  Some machines allow copying all registers to and
   from memory, but require a scratch register for stores to some memory
   locations (e.g., those with symbolic address on the RT, and those with
   certain symbolic address on the SPARC when compiling PIC).  In some cases,
   both an intermediate and a scratch register are required.

   You should define these macros to indicate to the reload phase that it may
   need to allocate at least one register for a reload in addition to the
   register to contain the data.  Specifically, if copying X to a register
   CLASS in MODE requires an intermediate register, you should define
   `SECONDARY_INPUT_RELOAD_CLASS' to return the largest register class all of
   whose registers can be used as intermediate registers or scratch registers.

   If copying a register CLASS in MODE to X requires an intermediate or scratch
   register, `SECONDARY_OUTPUT_RELOAD_CLASS' should be defined to return the
   largest register class required.  If the requirements for input and output
   reloads are the same, the macro `SECONDARY_RELOAD_CLASS' should be used
   instead of defining both macros identically.

   The values returned by these macros are often `GENERAL_REGS'.  Return
   `NO_REGS' if no spare register is needed; i.e., if X can be directly copied
   to or from a register of CLASS in MODE without requiring a scratch register.
   Do not define this macro if it would always return `NO_REGS'.

   If a scratch register is required (either with or without an intermediate
   register), you should define patterns for `reload_inM' or `reload_outM', as
   required (*note Standard Names::..  These patterns, which will normally be
   implemented with a `define_expand', should be similar to the `movM'
   patterns, except that operand 2 is the scratch register.

   Define constraints for the reload register and scratch register that contain
   a single register class.  If the original reload register (whose class is
   CLASS) can meet the constraint given in the pattern, the value returned by
   these macros is used for the class of the scratch register.  Otherwise, two
   additional reload registers are required.  Their classes are obtained from
   the constraints in the insn pattern.

   X might be a pseudo-register or a `subreg' of a pseudo-register, which could
   either be in a hard register or in memory.  Use `true_regnum' to find out;
   it will return -1 if the pseudo is in memory and the hard register number if
   it is in a register.

   These macros should not be used in the case where a particular class of
   registers can only be copied to memory and not to another class of
   registers.  In that case, secondary reload registers are not needed and
   would not be helpful.  Instead, a stack location must be used to perform the
   copy and the `movM' pattern should use memory as an intermediate storage.
   This case often occurs between floating-point and general registers.  */

#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X)				\
((CLASS) == GPR_REGS		? NO_REGS				\
 : (CLASS) == EVEN_REGS		? NO_REGS				\
 : (CLASS) == ACCUM_REGS	? EVEN_REGS				\
 :				  GPR_REGS)

/* A C expression whose value is nonzero if pseudos that have been assigned to
   registers of class CLASS would likely be spilled because registers of CLASS
   are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one register
   and zero otherwise.  On most machines, this default should be used.  Only
   define this macro to some other expression if pseudo allocated by
   `local-alloc.c' end up in memory because their hard registers were needed
   for spill registers.  If this macro returns nonzero for those classes, those
   pseudos will only be allocated by `global.c', which knows how to reallocate
   the pseudo to another register.  If there would not be another register
   available for reallocation, you should not change the definition of this
   macro since the only effect of such a definition would be to slow down
   register allocation.  */
#define CLASS_LIKELY_SPILLED_P(CLASS) \
  ((CLASS) != GPR_REGS && (CLASS) != EVEN_REGS)

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.  */

#define CLASS_MAX_NREGS(CLASS, MODE)					\
(((CLASS) == ACCUM_REGS)						\
 ? ((GET_MODE_SIZE (MODE) + 8 - 1) / 8)					\
 : ((GET_MODE_SIZE (MODE) + 4 - 1) / 4))

/* A C expression that defines the machine-dependent operand constraint letters
   (`I', `J', `K', .. 'P') that specify particular ranges of integer values.
   If C is one of those letters, the expression should check that VALUE, an
   integer, is in the appropriate range and return 1 if so, 0 otherwise.  If C
   is not one of those letters, the value should be 0 regardless of VALUE.  */
#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
((C) == 'I'	? IN_RANGE_P (VALUE, -32, 31)				\
 : (C) == 'J'	? IN_RANGE_P (VALUE, 0, 31)				\
 : (C) == 'K'	? IN_RANGE_P (exact_log2 (VALUE), 0, 31)		\
 : (C) == 'L'	? IN_RANGE_P (exact_log2 (~ (VALUE)), 0, 31)		\
 : (C) == 'M'	? ((VALUE) == 32)					\
 : (C) == 'N'	? ((VALUE) == 1)					\
 : (C) == 'O'	? ((VALUE) == 0)					\
 : (C) == 'P'	? IN_RANGE_P (VALUE, 32, 63)				\
 :		  FALSE)

/* A C expression that defines the machine-dependent operand constraint letters
   (`G', `H') that specify particular ranges of `const_double' values.

   If C is one of those letters, the expression should check that VALUE, an RTX
   of code `const_double', is in the appropriate range and return 1 if so, 0
   otherwise.  If C is not one of those letters, the value should be 0
   regardless of VALUE.

   `const_double' is used for all floating-point constants and for `DImode'
   fixed-point constants.  A given letter can accept either or both kinds of
   values.  It can use `GET_MODE' to distinguish between these kinds.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
((C) == 'G'	? (CONST_DOUBLE_LOW (VALUE) == 0			\
		   && CONST_DOUBLE_HIGH (VALUE) == 0)			\
 : (C) == 'H'	? FALSE							\
 :		  FALSE)

/* A C expression that defines the optional machine-dependent constraint
   letters (`Q', `R', `S', `T', `U') that can be used to segregate specific
   types of operands, usually memory references, for the target machine.
   Normally this macro will not be defined.  If it is required for a particular
   target machine, it should return 1 if VALUE corresponds to the operand type
   represented by the constraint letter C.  If C is not defined as an extra
   constraint, the value returned should be 0 regardless of VALUE.

   For example, on the ROMP, load instructions cannot have their output in r0
   if the memory reference contains a symbolic address.  Constraint letter `Q'
   is defined as representing a memory address that does *not* contain a
   symbolic address.  An alternative is specified with a `Q' constraint on the
   input and `r' on the output.  The next alternative specifies `m' on the
   input and a register class that does not include r0 on the output.  */

#define EXTRA_CONSTRAINT(VALUE, C)					\
(((C) == 'Q')	? short_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'R')	? single_reg_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'S')	? const_addr_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'T')	? long_memory_operand ((VALUE), GET_MODE (VALUE))	\
 : ((C) == 'U')	? FALSE							\
 :		  FALSE)


/* Basic Stack Layout */

/* Stack layout */

/* Structure used to define the d30v stack */
typedef struct d30v_stack {
  int varargs_p;		/* whether this is a varargs function */
  int varargs_size;		/* size to hold varargs args passed in regs */
  int vars_size;		/* variable save area size */
  int parm_size;		/* outgoing parameter size */
  int gpr_size;			/* size of saved GPR registers */
  int accum_size;		/* size of saved ACCUM registers */
  int total_size;		/* total bytes allocated for stack */
				/* which registers are to be saved */
  int save_offset;		/* offset from new sp to start saving vars at */
  int link_offset;		/* offset r62 is saved at */
  int memrefs_varargs;		/* # of 2 word memory references for varargs */
  int memrefs_2words;		/* # of 2 word memory references */
  int memrefs_1word;		/* # of 1 word memory references */
				/* 1 for ldw/stw ops; 2 for ld2w/st2w ops */
  unsigned char save_p[FIRST_PSEUDO_REGISTER];
} d30v_stack_t;

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.

   When we say, "define this macro if ...," it means that the compiler checks
   this macro only with `#ifdef' so the precise definition used does not
   matter.  */
#define STACK_GROWS_DOWNWARD 1

/* Offset from the frame pointer to the first local variable slot to be
   allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by subtracting the
   first slot's length from `STARTING_FRAME_OFFSET'.  Otherwise, it is found by
   adding the length of the first slot to the value `STARTING_FRAME_OFFSET'.  */

#define STARTING_FRAME_OFFSET						\
  (D30V_ALIGN (current_function_outgoing_args_size,			\
	       (STACK_BOUNDARY / BITS_PER_UNIT)))

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   argument's address.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */

/* ??? This definition fails for leaf functions.  There is currently no
   general solution for this problem.  */

/* ??? There appears to be no way to get the return address of any previous
   frame except by disassembling instructions in the prologue/epilogue.
   So currently we support only the current frame.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)					\
  ((COUNT) == 0 ? d30v_return_addr() : const0_rtx)

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */

/* Before the prologue, RA lives in r62.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx (REG, Pmode, GPR_LINK)

/* A C expression whose value is an integer giving the offset, in bytes, from
   the value of the stack pointer register to the top of the stack frame at the
   beginning of any function, before the prologue.  The top of the frame is
   defined to be the value of the stack pointer in the previous frame, just
   before the call instruction.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_FRAME_SP_OFFSET 0

/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */

#define INIT_EXPANDERS  d30v_init_expanders ()


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */
#define STACK_POINTER_REGNUM GPR_SP

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */
#define FRAME_POINTER_REGNUM GPR_FP

/* Register numbers used for passing a function's static chain pointer.  If
   register windows are used, the register number as seen by the called
   function is `STATIC_CHAIN_INCOMING_REGNUM', while the register number as
   seen by the calling function is `STATIC_CHAIN_REGNUM'.  If these registers
   are the same, `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be defined;
   instead, the next two macros should be defined.  */

#define STATIC_CHAIN_REGNUM (GPR_FIRST + 18)


/* Eliminating the Frame Pointer and the Arg Pointer */

/* A C expression which is nonzero if a function must have and use a frame
   pointer.  This expression is evaluated in the reload pass.  If its value is
   nonzero the function will have a frame pointer.

   The expression can in principle examine the current function and decide
   according to the facts, but on most machines the constant 0 or the constant
   1 suffices.  Use 0 when the machine allows code to be generated with no
   frame pointer, and doing so saves some time or space.  Use 1 when there is
   no possible advantage to avoiding a frame pointer.

   In certain cases, the compiler does not know how to produce valid code
   without a frame pointer.  The compiler recognizes those cases and
   automatically gives the function a frame pointer regardless of what
   `FRAME_POINTER_REQUIRED' says.  You don't need to worry about them.

   In a function that does not require a frame pointer, the frame pointer
   register can be allocated for ordinary usage, unless you mark it as a fixed
   register.  See `FIXED_REGISTERS' for more information.  */
#define FRAME_POINTER_REQUIRED 0

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.

   On some machines, the position of the argument pointer is not known until
   the compilation is completed.  In such a case, a separate hard register must
   be used for the argument pointer.  This register can be eliminated by
   replacing it with either the frame pointer or the argument pointer,
   depending on whether or not the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack pointer is
   specified first since that is the preferred elimination.  */
#define ELIMINABLE_REGS							\
{									\
  { ARG_POINTER_REGNUM,		STACK_POINTER_REGNUM },			\
  { ARG_POINTER_REGNUM,		FRAME_POINTER_REGNUM },			\
  { FRAME_POINTER_REGNUM,	STACK_POINTER_REGNUM }			\
}

/* A C expression that returns nonzero if the compiler is allowed to try to
   replace register number FROM-REG with register number TO-REG.  This macro
   need only be defined if `ELIMINABLE_REGS' is defined, and will usually be
   the constant 1, since most of the cases preventing register elimination are
   things that the compiler already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM		\
  ? ! frame_pointer_needed						\
  : 1)

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It specifies the
   initial difference between the specified pair of registers.  This macro must
   be defined if `ELIMINABLE_REGS' is defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  d30v_stack_t *info = d30v_stack_info ();				\
									\
  if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = 0;							\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
    (OFFSET) = info->total_size - current_function_pretend_args_size;	\
  else									\
    abort ();								\
}


/* Passing Function Arguments on the Stack */

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the
   stack frame size by this amount.

   Defining both `PUSH_ROUNDING' and `ACCUMULATE_OUTGOING_ARGS' is not
   proper.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* A C expression that should indicate the number of bytes of its own arguments
   that a function pops on returning, or 0 if the function pops no arguments
   and the caller must therefore pop them all after the function returns.

   FUNDECL is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_DECL' that
   describes the declaration of the function.  From this it is possible to
   obtain the DECL_ATTRIBUTES of the function.

   FUNTYPE is a C variable whose value is a tree node that describes the
   function in question.  Normally it is a node of type `FUNCTION_TYPE' that
   describes the data type of the function.  From this it is possible to obtain
   the data types of the value and arguments (if known).

   When a call to a library function is being considered, FUNTYPE will contain
   an identifier node for the library function.  Thus, if you need to
   distinguish among various library functions, you can do so by their names.
   Note that "library function" in this context means a function used to
   perform arithmetic, whose name is known specially in the compiler and was
   not mentioned in the C code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the stack.  If a
   variable number of bytes is passed, it is zero, and argument popping will
   always be the responsibility of the calling function.

   On the VAX, all functions always pop their arguments, so the definition of
   this macro is STACK-SIZE.  On the 68000, using the standard calling
   convention, no functions pop their arguments, so the value of the macro is
   always 0 in this case.  But an alternative calling convention is available
   in which functions that take a fixed number of arguments pop them but other
   functions (such as `printf') pop nothing (the caller pops all).  When this
   convention is in use, FUNTYPE is examined to determine whether a function
   takes a fixed number of arguments.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0


/* Function Arguments in Registers */

/* A C expression that controls whether a function argument is passed in a
   register, and which register.

   The arguments are CUM, which summarizes all the previous arguments; MODE,
   the machine mode of the argument; TYPE, the data type of the argument as a
   tree node or 0 if that is not known (which happens for C support library
   functions); and NAMED, which is 1 for an ordinary argument and 0 for
   nameless arguments that correspond to `...' in the called function's
   prototype.

   The value of the expression should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the argument on the
   stack.

   For machines like the VAX and 68000, where normally all arguments are
   pushed, zero suffices as a definition.

   The usual way to make the ANSI library `stdarg.h' work on a machine where
   some arguments are usually passed in registers, is to cause nameless
   arguments to be passed on the stack instead.  This is done by making
   `FUNCTION_ARG' return 0 whenever NAMED is 0.

   You may use the macro `MUST_PASS_IN_STACK (MODE, TYPE)' in the definition of
   this macro to determine if this argument is of a type that must be passed in
   the stack.  If `REG_PARM_STACK_SPACE' is not defined and `FUNCTION_ARG'
   returns nonzero for such an argument, the compiler will abort.  If
   `REG_PARM_STACK_SPACE' is defined, the argument will be computed in the
   stack and then loaded into a register.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg (&CUM, (int)MODE, TYPE, NAMED, FALSE)

/* Define this macro if the target machine has "register windows", so that the
   register in which a function sees an arguments is not necessarily the same
   as the one in which the caller passed the argument.

   For such machines, `FUNCTION_ARG' computes the register in which the caller
   passes the value, and `FUNCTION_INCOMING_ARG' should be defined in a similar
   fashion to tell the function being called where the arguments will arrive.

   If `FUNCTION_INCOMING_ARG' is not defined, `FUNCTION_ARG' serves both
   purposes.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg (&CUM, (int)MODE, TYPE, NAMED, TRUE)

/* A C expression for the number of words, at the beginning of an argument,
   must be put in registers.  The value must be zero for arguments that are
   passed entirely in registers or that are entirely pushed on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory.  On these machines, typically the first N words of
   arguments are passed in registers, and the rest on the stack.  If a
   multi-word argument (a `double' or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be pushed.
   This macro tells the compiler when this occurs, and how many of the words
   should go in registers.

   `FUNCTION_ARG' for these arguments should return the first register to be
   used by the caller for this argument; likewise `FUNCTION_INCOMING_ARG', for
   the called function.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg_partial_nregs (&CUM, (int)MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by reference.
   If nonzero for an argument, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself.  The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   On machines where `REG_PARM_STACK_SPACE' is not defined, a suitable
   definition of this macro might be
        #define FUNCTION_ARG_PASS_BY_REFERENCE\
        (CUM, MODE, TYPE, NAMED)  \
          MUST_PASS_IN_STACK (MODE, TYPE)  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) 0

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  */
#define CUMULATIVE_ARGS int

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  d30v_init_cumulative_args (&CUM, FNTYPE, LIBNAME, FNDECL, FALSE)

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.

   The value passed for LIBNAME is always 0, since library routines with
   special calling conventions are never compiled with GCC.  The argument
   LIBNAME exists for symmetry with `INIT_CUMULATIVE_ARGS'.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
  d30v_init_cumulative_args (&CUM, FNTYPE, LIBNAME, NULL, TRUE)

/* A C statement (sans semicolon) to update the summarizer variable CUM to
   advance past an argument in the argument list.  The values MODE, TYPE and
   NAMED describe that argument.  Once this is done, the variable CUM is
   suitable for analyzing the *following* argument with `FUNCTION_ARG', etc.

   This macro need not do anything if the argument in question was passed on
   the stack.  The compiler knows how to track the amount of stack space used
   for arguments without any special help.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
  d30v_function_arg_advance (&CUM, (int) MODE, TYPE, NAMED)

/* If defined, a C expression that gives the alignment boundary, in bits, of an
   argument with the specified mode and type.  If it is not defined,
   `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  d30v_function_arg_boundary ((int) MODE, TYPE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */

#define FUNCTION_ARG_REGNO_P(REGNO) \
  IN_RANGE_P (REGNO, GPR_ARG_FIRST, GPR_ARG_LAST)


/* How Scalar Function Values are Returned */

/* A C expression to create an RTX representing the place where a function
   returns a value of data type VALTYPE.  VALTYPE is a tree node representing a
   data type.  Write `TYPE_MODE (VALTYPE)' to get the machine mode used to
   represent that type.  On many machines, only the mode is relevant.
   (Actually, on most machines, scalar values are returned in the same place
   regardless of mode).

   If `PROMOTE_FUNCTION_RETURN' is defined, you must apply the same promotion
   rules specified in `PROMOTE_MODE' if VALTYPE is a scalar type.

   If the precise function being called is known, FUNC is a tree node
   (`FUNCTION_DECL') for it; otherwise, FUNC is a null pointer.  This makes it
   possible to use a different value-returning convention for specific
   functions when all their calls are known.

   `FUNCTION_VALUE' is not used for return vales with aggregate data types,
   because these are returned in another way.  See `STRUCT_VALUE_REGNUM' and
   related macros, below.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx (REG, TYPE_MODE (VALTYPE), GPR_RET_VALUE)

/* A C expression to create an RTX representing the place where a library
   function returns a value of mode MODE.  If the precise function being called
   is known, FUNC is a tree node (`FUNCTION_DECL') for it; otherwise, FUNC is a
   null pointer.  This makes it possible to use a different value-returning
   convention for specific functions when all their calls are known.

   Note that "library function" in this context means a compiler support
   routine, used to perform arithmetic, whose name is known specially by the
   compiler and was not mentioned in the C code being compiled.

   The definition of `LIBRARY_VALUE' need not be concerned aggregate data
   types, because none of the library functions returns such types.  */

#define LIBCALL_VALUE(MODE) gen_rtx (REG, MODE, GPR_RET_VALUE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which the values of called function may come back.

   A register whose use for returning values is limited to serving as the
   second of a pair (for a value of type `double', say) need not be recognized
   by this macro.  So for most machines, this definition suffices:

        #define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

   If the machine has register windows, so that the caller and the called
   function use different registers for the return value, this macro should
   recognize only the caller's register numbers.  */

#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == GPR_RET_VALUE)


/* How Large Values are Returned */

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */

#define STRUCT_VALUE_REGNUM GPR_ARG_FIRST

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place where the
   address is passed.  If it returns 0, the address is passed as an "invisible"
   first argument.  */

#define STRUCT_VALUE 0


/* Define this macro as a C expression that is nonzero for registers
   are used by the epilogue or the `return' pattern.  The stack and
   frame pointer registers are already be assumed to be used as
   needed.  */
#define EPILOGUE_USES(REGNO)  ((REGNO) == GPR_LINK)

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct machine_function GTY(())
{
  /* Additionsl stack adjustment in __builtin_eh_throw.  */
  rtx eh_epilogue_sp_ofs;
} machine_function;


/* Generating Code for Profiling.  */

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GCC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.  */

#define FUNCTION_PROFILER(FILE, LABELNO) d30v_function_profiler (FILE, LABELNO)


/* Implementing the Varargs Macros.  */

/* If defined, is a C expression that produces the machine-specific code for a
   call to `__builtin_saveregs'.  This code will be moved to the very beginning
   of the function, before any parameter access are made.  The return value of
   this function should be an RTX that contains the value to use as the return
   of `__builtin_saveregs'.

   If this macro is not defined, the compiler will output an ordinary call to
   the library function `__builtin_saveregs'.  */

#define EXPAND_BUILTIN_SAVEREGS() d30v_expand_builtin_saveregs ()

/* This macro offers an alternative to using `__builtin_saveregs' and defining
   the macro `EXPAND_BUILTIN_SAVEREGS'.  Use it to store the anonymous register
   arguments into the stack so that all the arguments appear to have been
   passed consecutively on the stack.  Once this is done, you can use the
   standard implementation of varargs that works for machines that pass all
   their arguments on the stack.

   The argument ARGS_SO_FAR is the `CUMULATIVE_ARGS' data structure, containing
   the values that obtain after processing of the named arguments.  The
   arguments MODE and TYPE describe the last named argument--its machine mode
   and its data type as a tree node.

   The macro implementation should do two things: first, push onto the stack
   all the argument registers *not* used for the named arguments, and second,
   store the size of the data thus pushed into the `int'-valued variable whose
   name is supplied as the argument PRETEND_ARGS_SIZE.  The value that you
   store here will serve as additional offset for setting up the stack frame.

   Because you must generate code to push the anonymous arguments at compile
   time without knowing their data types, `SETUP_INCOMING_VARARGS' is only
   useful on machines that have just a single category of argument register and
   use it uniformly for all data types.

   If the argument SECOND_TIME is nonzero, it means that the arguments of the
   function are being analyzed for the second time.  This happens for an inline
   function, which is not actually compiled until the end of the source file.
   The macro `SETUP_INCOMING_VARARGS' should not generate any instructions in
   this case.  */

#define SETUP_INCOMING_VARARGS(ARGS_SO_FAR, MODE, TYPE, PRETEND_ARGS_SIZE, SECOND_TIME) \
  d30v_setup_incoming_varargs (&ARGS_SO_FAR, (int) MODE, TYPE,		\
			       &PRETEND_ARGS_SIZE, SECOND_TIME)

/* Implement the stdarg/varargs va_start macro.  STDARG_P is nonzero if this
   is stdarg.h instead of varargs.h.  VALIST is the tree of the va_list
   variable to initialize.  NEXTARG is the machine independent notion of the
   'next' argument after the variable arguments.  If not defined, a standard
   implementation will be defined that works for arguments passed on the stack.  */

#define EXPAND_BUILTIN_VA_START(VALIST, NEXTARG)		\
  d30v_expand_builtin_va_start(VALIST, NEXTARG)

/* Implement the stdarg/varargs va_arg macro.  VALIST is the variable of type
   va_list as a tree, TYPE is the type passed to va_arg.  */

#define EXPAND_BUILTIN_VA_ARG(VALIST, TYPE)				\
(d30v_expand_builtin_va_arg (VALIST, TYPE))

/* Trampolines for Nested Functions.  */

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE (d30v_trampoline_size ())

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT' is used for
   aligning trampolines.  */
#define TRAMPOLINE_ALIGNMENT 64

/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
  d30v_initialize_trampoline (ADDR, FNADDR, STATIC_CHAIN)


/* Addressing Modes */

/* Define this macro if the machine supports post-increment addressing.  */
#define HAVE_POST_INCREMENT 1

/* Similar for other kinds of addressing.  */
#define HAVE_POST_DECREMENT 1

/* A C expression that is 1 if the RTX X is a constant which is a valid
   address.  On most machines, this can be defined as `CONSTANT_P (X)', but a
   few machines are more restrictive in which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are not
   explicitly known, such as `symbol_ref', `label_ref', and `high' expressions
   and `const' arithmetic expressions, in addition to `const_int' and
   `const_double' expressions.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

/* A number, the maximum number of registers that can appear in a valid memory
   address.  Note that it is up to you to specify a value equal to the maximum
   number that `GO_IF_LEGITIMATE_ADDRESS' would ever accept.  */
#define MAX_REGS_PER_ADDRESS 2

/* A C compound statement with a conditional `goto LABEL;' executed if X (an
   RTX) is a legitimate memory address on the target machine for a memory
   operand of mode MODE.  */

#ifdef	REG_OK_STRICT
#define REG_OK_STRICT_P 1
#else
#define REG_OK_STRICT_P 0
#endif

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
do {									\
    if (d30v_legitimate_address_p ((int)MODE, X, REG_OK_STRICT_P))	\
      goto ADDR;							\
} while (0)

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  For hard registers, it should always accept those
   which the hardware permits and reject the others.  Whether the macro accepts
   or rejects pseudo registers must be controlled by `REG_OK_STRICT' as
   described above.  This usually requires two variant definitions, of which
   `REG_OK_STRICT' controls the one actually used.  */

#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) (GPR_P (REGNO (X)))
#else
#define REG_OK_FOR_BASE_P(X) (GPR_OR_PSEUDO_P (REGNO (X)))
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

/* A C compound statement that attempts to replace X with a valid memory
   address for an operand of mode MODE.  WIN will be a C statement label
   elsewhere in the code; the macro definition may use

        GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs', and OLDX
   will be the operand that was given to that function to produce X.

   The code generated by this macro should not alter the substructure of X.  If
   it transforms X into a more legitimate form, it should assign X (which will
   always be a C variable) a new value.

   It is not necessary for this macro to come up with a legitimate address.
   The compiler has standard ways of doing so in all cases.  In fact, it is
   safe for this macro to do nothing.  But often a machine-dependent strategy
   can generate better code.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
do {									\
  rtx y = d30v_legitimize_address (X, OLDX, (int)MODE, REG_OK_STRICT_P); \
  if (y)								\
    {									\
      X = y;								\
      GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);				\
    }									\
} while (0)

/* A C statement or compound statement with a conditional `goto LABEL;'
   executed if memory address X (an RTX) can have different meanings depending
   on the machine mode of the memory reference it is used for or if the address
   is valid for some modes but not others.

   Autoincrement and autodecrement addresses typically have mode-dependent
   effects because the amount of the increment or decrement is the size of the
   operand being addressed.  Some machines have other mode-dependent addresses.
   Many RISC machines have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)			\
do {									\
  if (d30v_mode_dependent_address_p (ADDR))				\
    goto LABEL;								\
} while (0)								\

/* A C expression that is nonzero if X is a legitimate constant for an
   immediate operand on the target machine.  You can assume that X satisfies
   `CONSTANT_P', so you need not check this.  In fact, `1' is a suitable
   definition for this macro on machines where anything `CONSTANT_P' is valid.  */
#define LEGITIMATE_CONSTANT_P(X) 1


/* Describing Relative Costs of Operations */

/* A C expression for the cost of moving data from a register in class FROM to
   one in class TO.  The classes are expressed using the enumeration values
   such as `GENERAL_REGS'.  A value of 4 is the default; other values are
   interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.

   If reload sees an insn consisting of a single `set' between two hard
   registers, and if `REGISTER_MOVE_COST' applied to their classes returns a
   value of 2, reload does not check to ensure that the constraints of the insn
   are met.  Setting a cost of other than 2 will allow reload to verify that
   the constraints are met.  You should do this if the `movM' pattern's
   constraints do not allow such copying.  */

#define REGISTER_MOVE_COST(MODE, FROM, TO)				\
  (((FROM) != GPR_REGS && (FROM) != EVEN_REGS				\
   && (TO) != GPR_REGS && (TO) != EVEN_REGS) ? 4 : 2)

/* A C expression for the cost of moving data of mode M between a register and
   memory.  A value of 2 is the default; this cost is relative to those in
   `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than between two
   registers, you should define this macro to express the relative cost.  */
#define MEMORY_MOVE_COST(M,C,I) 4

/* A C expression for the cost of a branch instruction.  A value of 1 is the
   default; other values are interpreted relative to that.  */

#define BRANCH_COST d30v_branch_cost

#define D30V_DEFAULT_BRANCH_COST 2

/* Values of the -mbranch-cost=n string.  */
extern int d30v_branch_cost;
extern const char *d30v_branch_cost_string;

/* Here are additional macros which do not specify precise relative costs, but
   only that certain actions are more expensive than GCC would ordinarily
   expect.  */

/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory, i.e., if such access require more than one instruction or if
   there is no difference in cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by finding
   the smallest containing object; when it is defined, a fullword load will be
   used if alignment permits.  Unless bytes accesses are faster than word
   accesses, using word accesses is preferable since it may eliminate
   subsequent memory access if subsequent accesses occur to other fields in the
   same word of the structure, but to different bytes.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  Normally `".text"' is
   right.  */
#define TEXT_SECTION_ASM_OP "\t.text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  Normally
   `".data"' is right.  */
#define DATA_SECTION_ASM_OP "\t.data"

/* If defined, a C expression whose value is a string containing the
   assembler operation to identify the following data as
   uninitialized global data.  If not defined, and neither
   `ASM_OUTPUT_BSS' nor `ASM_OUTPUT_ALIGNED_BSS' are defined,
   uninitialized global data will be output in the data section if
   `-fno-common' is passed, otherwise `ASM_OUTPUT_COMMON' will be
   used.  */
#define BSS_SECTION_ASM_OP "\t.section .bss"


/* The Overall Framework of an Assembler File.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at the
   end of the line.  */
#define ASM_COMMENT_START ";"

/* A C string constant for text to be output before each `asm' statement or
   group of consecutive ones.  Normally this is `"#APP"', which is a comment
   that has no effect on most assemblers but tells the GNU assembler that it
   must check the lines that follow for all valid assembler constructs.  */
#define ASM_APP_ON "#APP\n"

/* A C string constant for text to be output after each `asm' statement or
   group of consecutive ones.  Normally this is `"#NO_APP"', which tells the
   GNU assembler to resume making the time-saving assumptions that are valid
   for ordinary compiler output.  */
#define ASM_APP_OFF "#NO_APP\n"


/* Output and Generation of Labels.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "


/* Macros Controlling Initialization Routines.  */

/* If defined, `main' will call `__main' despite the presence of
   `INIT_SECTION_ASM_OP'.  This macro should be defined for systems where the
   init section is not actually run automatically, but is still useful for
   collecting the lists of constructors and destructors.  */
#define INVOKE__main


/* Output of Assembler Instructions.  */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  This is what translates register numbers
   in the compiler into assembler language.  */
#define REGISTER_NAMES							\
{									\
  "r0",		"r1",		"r2",		"r3",			\
  "r4",		"r5",		"r6",		"r7",			\
  "r8",		"r9",		"r10",		"r11",			\
  "r12",	"r13",		"r14",		"r15",			\
  "r16",	"r17",		"r18",		"r19",			\
  "r20",	"r21",		"r22",		"r23",			\
  "r24",	"r25",		"r26",		"r27",			\
  "r28",	"r29",		"r30",		"r31",			\
  "r32",	"r33",		"r34",		"r35",			\
  "r36",	"r37",		"r38",		"r39",			\
  "r40",	"r41",		"r42",		"r43",			\
  "r44",	"r45",		"r46",		"r47",			\
  "r48",	"r49",		"r50",		"r51",			\
  "r52",	"r53",		"r54",		"r55",			\
  "r56",	"r57",		"r58",		"r59",			\
  "r60",	"r61",		"link",		"sp",			\
  "ap",									\
  "f0",		"f1",		"f2",		"f3",			\
  "s",		"v",		"va",		"c",			\
  "a0",		"a1",							\
  "psw",	"bpsw",		"pc",		"bpc",			\
  "dpsw",	"dpc",		"rpt_c",	"rpt_s",		\
  "rpt_e",	"mod_s",	"mod_e",	"iba",			\
  "eit_vb",	"int_s",	"int_m",				\
}

/* If defined, a C initializer for an array of structures containing a name and
   a register number.  This macro defines additional names for hard registers,
   thus allowing the `asm' option in declarations to refer to registers using
   alternate names.  */
#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {"r62",	GPR_LINK},			\
  {"r63",	GPR_SP},			\
  {"f4",	FLAG_SAT},			\
  {"f5",	FLAG_OVERFLOW},			\
  {"f6",	FLAG_ACC_OVER},			\
  {"f7",	FLAG_CARRY},			\
  {"carry",	FLAG_CARRY},			\
  {"borrow",	FLAG_BORROW},			\
  {"b",		FLAG_BORROW},			\
  {"cr0",	CR_PSW},			\
  {"cr1",	CR_BPSW},			\
  {"cr2",	CR_PC},				\
  {"cr3",	CR_BPC},			\
  {"cr4",	CR_DPSW},			\
  {"cr5",	CR_DPC},			\
  {"cr7",	CR_RPT_C},			\
  {"cr8",	CR_RPT_S},			\
  {"cr9",	CR_RPT_E},			\
  {"cr10",	CR_MOD_S},			\
  {"cr11",	CR_MOD_E},			\
  {"cr14",	CR_IBA},			\
  {"cr15",	CR_EIT_VB},			\
  {"cr16",	CR_INT_S},			\
  {"cr17",	CR_INT_M}			\
}

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand X.  X is an RTL expression.

   CODE is a value that can be used to specify one of several ways of printing
   the operand.  It is used when identical operands must be printed differently
   depending on the context.  CODE comes from the `%' specification that was
   used to request printing of the operand.  If the specification was just
   `%DIGIT' then CODE is 0; if the specification was `%LTR DIGIT' then CODE is
   the ASCII code for LTR.

   If X is a register, this macro should print the register's name.  The names
   can be found in an array `reg_names' whose type is `char *[]'.  `reg_names'
   is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%' followed by
   a punctuation character), this macro is called with a null pointer for X and
   the punctuation character for CODE.

   Standard operand flags that are handled elsewhere:
	`='  Output a number unique to each instruction in the compilation.
	`a'  Substitute an operand as if it were a memory reference.
	`c'  Omit the syntax that indicates an immediate operand.
	`l'  Substitute a LABEL_REF into a jump instruction.
	`n'  Like %cDIGIT, except negate the value before printing.

   The d30v specific operand flags are:
	`.'  Print r0.
	`f'  Print a SF constant as an int.
	`s'  Subtract 32 and negate.
	`A'  Print accumulator number without an `a' in front of it.
	`B'  Print bit offset for BSET, etc. instructions.
	`E'  Print u if this is zero extend, nothing if this is sign extend.
	`F'  Emit /{f,t,x}{f,t,x} for executing a false condition.
	`L'  Print the lower half of a 64 bit item.
	`M'  Print a memory reference for ld/st instructions.
	`R'  Return appropriate cmp instruction for relational test.
	`S'  Subtract 32.
	`T'  Emit /{f,t,x}{f,t,x} for executing a true condition.
	`U'  Print the upper half of a 64 bit item.  */

#define PRINT_OPERAND(STREAM, X, CODE) d30v_print_operand (STREAM, X, CODE)

/* A C expression which evaluates to true if CODE is a valid punctuation
   character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no punctuation
   characters (except for the standard one, `%') are used in this way.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '.' || (CODE) == ':')

/* A C compound statement to output to stdio stream STREAM the assembler syntax
   for an instruction operand that is a memory reference whose address is X.  X
   is an RTL expression.  */

#define PRINT_OPERAND_ADDRESS(STREAM, X) d30v_print_operand_address (STREAM, X)

/* If defined, C string expressions to be used for the `%R', `%L', `%U', and
   `%I' options of `asm_fprintf' (see `final.c').  These are useful when a
   single `md' file must support multiple assembler formats.  In that case, the
   various `tm.h' files can define these macros differently.

   USER_LABEL_PREFIX is defined in svr4.h.  */

#define REGISTER_PREFIX "%"
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX ""


/* Output of dispatch tables.  */

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a difference between two labels.
   VALUE and REL are the numbers of two internal labels.  The definitions of
   these labels are output using `(*targetm.asm_out.internal_label)', and they must be
   printed in the same way here.  For example,

        fprintf (STREAM, "\t.word L%d-L%d\n", VALUE, REL)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
fprintf (STREAM, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This macro should be provided on machines where the addresses in a dispatch
   table are absolute.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a reference to a label.  VALUE
   is the number of an internal label whose definition is output using
   `(*targetm.asm_out.internal_label)'.  For example,

        fprintf (STREAM, "\t.word L%d\n", VALUE)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
fprintf (STREAM, "\t.word .L%d\n", VALUE)


/* Assembler Commands for Alignment.  */

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  POWER
   will be a C expression of type `int'.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))


/* Macros Affecting all Debug Formats.  */

/* A C expression that returns the DBX register number for the compiler
   register number REGNO.  In simple cases, the value of this expression may be
   REGNO itself.  But sometimes there are some registers that the compiler
   knows about and DBX does not, or vice versa.  In such cases, some register
   may need to have one number in the compiler and another for DBX.

   If two registers have consecutive numbers inside GCC, and they can be
   used as a pair to hold a multiword value, then they *must* have consecutive
   numbers after renumbering with `DBX_REGISTER_NUMBER'.  Otherwise, debuggers
   will be unable to access such a pair, because they expect register pairs to
   be consecutive in their own numbering scheme.

   If you find yourself defining `DBX_REGISTER_NUMBER' in way that does not
   preserve register pairs, then what you must do instead is redefine the
   actual register numbering scheme.  */
#define DBX_REGISTER_NUMBER(REGNO)					\
(GPR_P (REGNO)			 ? ((REGNO) - GPR_FIRST)		\
 : ACCUM_P (REGNO)		 ? ((REGNO) - ACCUM_FIRST + 84)		\
 : FLAG_P (REGNO)		 ? 66 /* return psw for all flags */	\
 : (REGNO) == ARG_POINTER_REGNUM ? (GPR_SP - GPR_FIRST)			\
 : (REGNO) == CR_PSW		 ? (66 + 0)				\
 : (REGNO) == CR_BPSW		 ? (66 + 1)				\
 : (REGNO) == CR_PC		 ? (66 + 2)				\
 : (REGNO) == CR_BPC		 ? (66 + 3)				\
 : (REGNO) == CR_DPSW		 ? (66 + 4)				\
 : (REGNO) == CR_DPC		 ? (66 + 5)				\
 : (REGNO) == CR_RPT_C		 ? (66 + 7)				\
 : (REGNO) == CR_RPT_S		 ? (66 + 8)				\
 : (REGNO) == CR_RPT_E		 ? (66 + 9)				\
 : (REGNO) == CR_MOD_S		 ? (66 + 10)				\
 : (REGNO) == CR_MOD_E		 ? (66 + 11)				\
 : (REGNO) == CR_IBA		 ? (66 + 14)				\
 : (REGNO) == CR_EIT_VB		 ? (66 + 15)				\
 : (REGNO) == CR_INT_S		 ? (66 + 16)				\
 : (REGNO) == CR_INT_M		 ? (66 + 17)				\
 :				   -1)

/* A C expression that returns the type of debugging output GCC produces
   when the user specifies `-g' or `-ggdb'.  Define this if you have arranged
   for GCC to support more than one format of debugging output.  Currently,
   the allowable values are `DBX_DEBUG', `SDB_DEBUG', `DWARF_DEBUG',
   `DWARF2_DEBUG', and `XCOFF_DEBUG'.

   The value of this macro only affects the default debugging output; the user
   can always get a specific type of output by using `-gstabs', `-gcoff',
   `-gdwarf-1', `-gdwarf-2', or `-gxcoff'.

   Defined in svr4.h.  */

#undef	PREFERRED_DEBUGGING_TYPE
#define	PREFERRED_DEBUGGING_TYPE DBX_DEBUG


/* Miscellaneous Parameters.  */

/* Define this if you have defined special-purpose predicates in the file
   `MACHINE.c'.  This macro is called within an initializer of an array of
   structures.  The first field in the structure is the name of a predicate and
   the second field is an array of rtl codes.  For each predicate, list all rtl
   codes that can be in expressions matched by the predicate.  The list should
   have a trailing comma.  Here is an example of two entries in the list for a
   typical RISC machine:

        #define PREDICATE_CODES \
          {"gen_reg_rtx_operand", {SUBREG, REG}},  \
          {"reg_or_short_cint_operand", {SUBREG, REG, CONST_INT}},

   Defining this macro does not affect the generated code (however, incorrect
   definitions that omit an rtl code that may be matched by the predicate can
   cause the compiler to malfunction).  Instead, it allows the table built by
   `genrecog' to be more compact and efficient, thus speeding up the compiler.
   The most important predicates to include in the list specified by this macro
   are thoses used in the most insn patterns.  */

#define PREDICATE_CODES							\
  { "short_memory_operand",		{ MEM }},			\
  { "long_memory_operand",		{ MEM }},			\
  { "d30v_memory_operand",		{ MEM }},			\
  { "single_reg_memory_operand",	{ MEM }},			\
  { "const_addr_memory_operand",	{ MEM }},			\
  { "call_operand",			{ MEM }},			\
  { "gpr_operand",			{ REG, SUBREG }},		\
  { "accum_operand",			{ REG, SUBREG }},		\
  { "gpr_or_accum_operand",		{ REG, SUBREG }},		\
  { "cr_operand",			{ REG, SUBREG }},		\
  { "repeat_operand",			{ REG, SUBREG }},		\
  { "flag_operand",			{ REG, SUBREG }},		\
  { "br_flag_operand",			{ REG, SUBREG }},		\
  { "br_flag_or_constant_operand",	{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_br_flag_operand",		{ REG, SUBREG }},		\
  { "f0_operand",			{ REG, SUBREG }},		\
  { "f1_operand",			{ REG, SUBREG }},		\
  { "carry_operand",			{ REG, SUBREG }},		\
  { "reg_or_0_operand",			{ REG, SUBREG, CONST_INT,	\
					  CONST_DOUBLE }},		\
  { "gpr_or_signed6_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_unsigned5_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_unsigned6_operand",		{ REG, SUBREG, CONST_INT }},	\
  { "gpr_or_constant_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF }},			\
  { "gpr_or_dbl_const_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, CONST_DOUBLE }},	\
  { "gpr_or_memory_operand",		{ REG, SUBREG, MEM }},		\
  { "move_input_operand",		{ REG, SUBREG, MEM, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, CONST_DOUBLE }},	\
  { "move_output_operand",		{ REG, SUBREG, MEM }},		\
  { "signed6_operand",			{ CONST_INT }},			\
  { "unsigned5_operand",		{ CONST_INT }},			\
  { "unsigned6_operand",		{ CONST_INT }},			\
  { "bitset_operand",			{ CONST_INT }},			\
  { "condexec_test_operator",		{ EQ, NE }},			\
  { "condexec_branch_operator",		{ EQ, NE }},			\
  { "condexec_unary_operator",		{ ABS, NEG, NOT, ZERO_EXTEND }}, \
  { "condexec_addsub_operator",		{ PLUS, MINUS }},		\
  { "condexec_binary_operator",		{ MULT, AND, IOR, XOR,		\
					  ASHIFT, ASHIFTRT, LSHIFTRT,	\
					  ROTATE, ROTATERT }},		\
  { "condexec_shiftl_operator",		{ ASHIFT, ROTATE }},		\
  { "condexec_extend_operator",		{ SIGN_EXTEND, ZERO_EXTEND }},	\
  { "branch_zero_operator",		{ EQ, NE }},			\
  { "cond_move_dest_operand",		{ REG, SUBREG, MEM }},		\
  { "cond_move_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, MEM }},		\
  { "cond_exec_operand",		{ REG, SUBREG, CONST_INT,	\
					  CONST, SYMBOL_REF,		\
					  LABEL_REF, MEM }},		\
  { "srelational_si_operator",		{ EQ, NE, LT, LE, GT, GE }},	\
  { "urelational_si_operator",		{ LTU, LEU, GTU, GEU }},	\
  { "relational_di_operator",		{ EQ, NE, LT, LE, GT, GE,	\
					  LTU, LEU, GTU, GEU }},

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define this macro if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  Most RISC machines
   have this property and most CISC machines do not.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define this macro to be a C expression indicating when insns that read
   memory in MODE, an integral mode narrower than a word, set the bits outside
   of MODE to be either the sign-extension or the zero-extension of the data
   read.  Return `SIGN_EXTEND' for values of MODE for which the insn
   sign-extends, `ZERO_EXTEND' for which it zero-extends, and `NIL' for other
   modes.

   This macro is not called with MODE non-integral or with a width greater than
   or equal to `BITS_PER_WORD', so you may return any value in this case.  Do
   not define this macro if it would always return `NIL'.  On machines where
   this macro is defined, you will normally define it as the constant
   `SIGN_EXTEND' or `ZERO_EXTEND'.  */

#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 8

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* An alias for the machine mode for pointers.  On most machines, define this
   to be the integer mode corresponding to the width of a hardware pointer;
   `SImode' on 32-bit machine or `DImode' on 64-bit machines.  On some machines
   you must define this to be one of the partial integer modes, such as
   `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to `Pmode'.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  On most machines this should be
   `QImode'.  */
#define FUNCTION_MODE QImode

/* Define this macro to handle System V style pragmas (particularly #pack).

   Defined in svr4.h.  */
#define HANDLE_SYSV_PRAGMA 1

/* A C expression for the maximum number of instructions to execute via
   conditional execution instructions instead of a branch.  A value of
   BRANCH_COST+1 is the default if the machine does not use cc0, and 1 if it
   does use cc0. */
#define MAX_CONDITIONAL_EXECUTE d30v_cond_exec

#define D30V_DEFAULT_MAX_CONDITIONAL_EXECUTE 4

/* Values of the -mcond-exec=n string.  */
extern int d30v_cond_exec;
extern const char *d30v_cond_exec_string;

#endif /* GCC_D30V_H */
