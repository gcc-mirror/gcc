/* Definitions of target machine for GNU compiler,
   for Motorola M*CORE Processor.
   Copyright (C) 1993, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

#ifndef GCC_MCORE_H
#define GCC_MCORE_H

/* RBE: need to move these elsewhere.  */
#undef	LIKE_PPC_ABI 
#define	MCORE_STRUCT_ARGS
/* RBE: end of "move elsewhere".  */

#include "hwint.h"

#ifndef HAVE_MACHINE_MODES
#include "machmode.h"
#endif

/* Run-time Target Specification.  */
#define TARGET_MCORE

/* Get tree.c to declare a target-specific specialization of
   merge_decl_attributes.  */
#define TARGET_DLLIMPORT_DECL_ATTRIBUTES

/* Support the __declspec keyword by turning them into attributes.
   We currently only support: dllexport and dllimport.
   Note that the current way we do this may result in a collision with
   predefined attributes later on.  This can be solved by using one attribute,
   say __declspec__, and passing args to it.  The problem with that approach
   is that args are not accumulated: each new appearance would clobber any
   existing args.  XXX- FIXME the definition below relies upon string
   concatenation, which is non-portable.  */
#define CPP_PREDEFINES \
  "-D__mcore__ -D__MCORE__=1 -D__declspec(x)=__attribute__((x))" SUBTARGET_CPP_PREDEFINES

/* If -m4align is ever re-enabled then uncomment this line as well:
   #define CPP_SPEC "%{!m4align:-D__MCORE_ALIGN_8__} %{m4align:-D__MCORE__ALIGN_4__}" */

#undef  CPP_SPEC
#define CPP_SPEC "							\
%{mbig-endian:								\
  %{mlittle-endian:%echoose either big or little endian, not both}	\
  -D__MCOREBE__}							\
%{m210:									\
  %{m340:%echoose either m340 or m210 not both}				\
  %{mlittle-endian:%ethe m210 does not have little endian support}	\
  -D__M210__}								\
%{!mbig-endian: -D__MCORELE__}						\
%{!m210: -D__M340__}							\
"
/* If -m4align is ever re-enabled then add this line to the definition of CPP_SPEC
   %{!m4align:-D__MCORE_ALIGN_8__} %{m4align:-D__MCORE__ALIGN_4__} */

/* We don't have a -lg library, so don't put it in the list.  */
#undef	LIB_SPEC
#define LIB_SPEC "%{!shared: %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

#undef	ASM_SPEC
#define	ASM_SPEC "%{mbig-endian:-EB} %{m210:-cpu=210 -EB}"

#undef  LINK_SPEC
#define LINK_SPEC "%{mbig-endian:-EB} %{m210:-EB} -X"

/* Can only count on 16 bits of availability; change to long would affect
   many architecture specific files (other architectures...).  */
extern int target_flags;

#define HARDLIT_BIT	   (1 << 0) /* Build in-line literals using 2 insns */
#define ALIGN8_BIT	   (1 << 1) /* Max alignment goes to 8 instead of 4 */
#define DIV_BIT		   (1 << 2) /* Generate divide instructions */
#define RELAX_IMM_BIT	   (1 << 3) /* Arbitrary immediates in and, or, tst */
#define W_FIELD_BIT	   (1 << 4) /* Generate bit insv/extv using SImode */
#define	OVERALIGN_FUNC_BIT (1 << 5) /* Align functions to 4 byte boundary */
#define CGDATA_BIT	   (1 << 6) /* Generate callgraph data */
#define SLOW_BYTES_BIT     (1 << 7) /* Slow byte access */
#define LITTLE_END_BIT     (1 << 8) /* Generate little endian code */
#define M340_BIT           (1 << 9) /* Generate code for the m340 */

#define TARGET_DEFAULT     \
 (HARDLIT_BIT | ALIGN8_BIT | DIV_BIT | RELAX_IMM_BIT | M340_BIT | LITTLE_END_BIT)

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "mlittle-endian", "m340" }
#endif

#define TARGET_HARDLIT		(target_flags & HARDLIT_BIT)
/* The ability to have 4 byte alignment is being suppressed for now.
   If this ability is reenabled, you must enable the definition below
   *and* edit t-mcore to enable multilibs for 4 byte alignment code.  */
#if 0 
#define TARGET_8ALIGN		(target_flags & ALIGN8_BIT)
#else
#define TARGET_8ALIGN		1
#endif
#define TARGET_DIV		(target_flags & DIV_BIT)
#define TARGET_RELAX_IMM        (target_flags & RELAX_IMM_BIT)
#define TARGET_W_FIELD          (target_flags & W_FIELD_BIT)
#define TARGET_OVERALIGN_FUNC   (target_flags & OVERALIGN_FUNC_BIT)
#define TARGET_CG_DATA 		(target_flags & CGDATA_BIT)
#define TARGET_CG_DATA 		(target_flags & CGDATA_BIT)
#define TARGET_SLOW_BYTES 	(target_flags & SLOW_BYTES_BIT)
#define TARGET_LITTLE_END	(target_flags & LITTLE_END_BIT)
#define TARGET_M340 		(target_flags & M340_BIT)


#define TARGET_SWITCHES							\
{ {"hardlit", 	            HARDLIT_BIT,				\
     N_("Inline constants if it can be done in 2 insns or less") },	\
  {"no-hardlit",          - HARDLIT_BIT,				\
     N_("Inline constants if it only takes 1 instruction") },		\
  {"4align",              - ALIGN8_BIT,					\
     N_("Set maximum alignment to 4") },				\
  {"8align",	            ALIGN8_BIT,					\
     N_("Set maximum alignment to 8") },				\
  {"div",                   DIV_BIT,					\
     "" },								\
  {"no-div",	          - DIV_BIT,					\
     N_("Do not use the divide instruction") },				\
  {"relax-immediates",      RELAX_IMM_BIT,				\
     "" },								\
  {"no-relax-immediates", - RELAX_IMM_BIT,				\
     N_("Do not arbitrary sized immediates in bit operations") },	\
  {"wide-bitfields",        W_FIELD_BIT,				\
     N_("Always treat bit-field as int-sized") },			\
  {"no-wide-bitfields",   - W_FIELD_BIT,				\
     "" },								\
  {"4byte-functions",       OVERALIGN_FUNC_BIT,				\
     N_("Force functions to be aligned to a 4 byte boundary") },	\
  {"no-4byte-functions",  - OVERALIGN_FUNC_BIT,				\
     N_("Force functions to be aligned to a 2 byte boundary") },	\
  {"callgraph-data",        CGDATA_BIT,					\
     N_("Emit call graph information") },				\
  {"no-callgraph-data",   - CGDATA_BIT,					\
     "" },								\
  {"slow-bytes",            SLOW_BYTES_BIT,				\
     N_("Prefer word accesses over byte accesses") },			\
  {"no-slow-bytes",       - SLOW_BYTES_BIT,				\
     "" },								\
  { "no-lsim",              0, "" },			 		\
  {"little-endian",         LITTLE_END_BIT,				\
     N_("Generate little endian code") },				\
  {"big-endian",          - LITTLE_END_BIT,				\
     "" },								\
  {"210",                 - M340_BIT,					\
     "" },								\
  {"340",                   M340_BIT,					\
     N_("Generate code for the M*Core M340") },				\
  {"",   	            TARGET_DEFAULT,				\
     "" }								\
}

extern char * mcore_current_function_name;
 
/* Target specific options (as opposed to the switches above).  */
extern const char * mcore_stack_increment_string;

#define	TARGET_OPTIONS							\
{									\
  {"stack-increment=", & mcore_stack_increment_string,			\
     N_("Maximum amount for a single stack increment operation")}	\
}

#ifndef CC1_SPEC
/* The MCore ABI says that bitfields are unsigned by default.  */
#define CC1_SPEC "-funsigned-bitfields"
#endif

/* What options are we going to default to specific settings when
   -O* happens; the user can subsequently override these settings.
  
   Omitting the frame pointer is a very good idea on the MCore.
   Scheduling isn't worth anything on the current MCore implementation.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)	\
{						\
  if (LEVEL)					\
    {						\
      flag_no_function_cse = 1;			\
      flag_omit_frame_pointer = 1;		\
						\
      if (LEVEL >= 2)				\
        {					\
          flag_caller_saves = 0;		\
          flag_schedule_insns = 0;		\
          flag_schedule_insns_after_reload = 0;	\
        }					\
    }						\
  if (SIZE)					\
    {						\
      target_flags &= ~ HARDLIT_BIT;		\
    }						\
}

/* What options are we going to force to specific settings,
   regardless of what the user thought he wanted.
   We also use this for some post-processing of options. */
#define OVERRIDE_OPTIONS  mcore_override_options ()

/* Target machine storage Layout.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  	\
  if (GET_MODE_CLASS (MODE) == MODE_INT         \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    {						\
      (MODE) = SImode;				\
      (UNSIGNEDP) = 1;				\
    }

#define PROMOTE_FUNCTION_ARGS

#define PROMOTE_FUNCTION_RETURN

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (! TARGET_LITTLE_END)

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN (! TARGET_LITTLE_END)

#define LIBGCC2_WORDS_BIG_ENDIAN 1
#ifdef __MCORELE__
#undef  LIBGCC2_WORDS_BIG_ENDIAN
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD	4

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY  	32

/* Doubles must be alogned to an 8 byte boundary.  */
#define FUNCTION_ARG_BOUNDARY(MODE, TYPE) \
  ((MODE != BLKmode && (GET_MODE_SIZE (MODE) == 8)) \
   ? BIGGEST_ALIGNMENT : PARM_BOUNDARY)
     
/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY  (TARGET_8ALIGN ? 64 : 32)

/* Largest increment in UNITS we allow the stack to grow in a single operation.  */
extern int mcore_stack_increment;
#define STACK_UNITS_MAXSTEP  4096

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY  ((TARGET_OVERALIGN_FUNC) ? 32 : 16)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT  (TARGET_8ALIGN ? 64 : 32)

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use 
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS	1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))
     
/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Register allocation for our first guess 

	r0		stack pointer
	r1		scratch, target reg for xtrb?
	r2-r7		arguments.
	r8-r14		call saved
	r15		link register
	ap		arg pointer (doesn't really exist, always eliminated)
	c               c bit
	fp		frame pointer (doesn't really exist, always eliminated)
	x19		two control registers  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   MCore has 16 integer registers and 2 control registers + the arg
   pointer.  */

#define FIRST_PSEUDO_REGISTER 20

#define R1_REG  1	/* where literals are forced */
#define LK_REG	15	/* overloaded on general register */
#define AP_REG  16	/* fake arg pointer register */
/* RBE: mcore.md depends on CC_REG being set to 17 */
#define CC_REG	17	/* can't name it C_REG */
#define FP_REG  18	/* fake frame pointer register */

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */


#undef PC_REGNUM /* Define this if the program counter is overloaded on a register.  */
#define STACK_POINTER_REGNUM 0 /* Register to use for pushing function arguments.  */
#define FRAME_POINTER_REGNUM 8 /* When we need FP, use r8.  */

/* The assembler's names for the registers.  RFP need not always be used as
   the Real framepointer; it can also be used as a normal general register.
   Note that the name `fp' is horribly misleading since `fp' is in fact only
   the argument-and-return-context pointer.  */
#define REGISTER_NAMES  				\
{				                   	\
  "sp", "r1", "r2",  "r3",  "r4",  "r5",  "r6",  "r7", 	\
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",	\
  "apvirtual",  "c", "fpvirtual", "x19" \
}

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS  \
 /*  r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  r10 r11 r12 r13 r14 r15 ap  c  fp x19 */ \
   { 1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* RBE: r15 {link register} not available across calls,
 *  But we don't mark it that way here... */
#define CALL_USED_REGISTERS \
 /*  r0  r1  r2  r3  r4  r5  r6  r7  r8  r9  r10 r11 r12 r13 r14 r15 ap  c   fp x19 */ \
   { 1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1, 1}

/* The order in which register should be allocated.  */
#define REG_ALLOC_ORDER  \
 /* r7  r6  r5  r4  r3  r2  r15 r14 r13 r12 r11 r10  r9  r8  r1  r0  ap  c   fp x19*/ \
  {  7,  6,  5,  4,  3,  2,  15, 14, 13, 12, 11, 10,  9,  8,  1,  0, 16, 17, 18, 19}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the MCore regs are UNITS_PER_WORD bits wide; */
#define HARD_REGNO_NREGS(REGNO, MODE)  \
   (((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   We may keep double values in even registers.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)  \
  ((TARGET_8ALIGN && GET_MODE_SIZE (MODE) > UNITS_PER_WORD) ? (((REGNO) & 1) == 0) : (REGNO < 18))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((MODE1) == (MODE2) || GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.  */
#define FRAME_POINTER_REQUIRED	0

/* Definitions for register eliminations.

   We have two registers that can be eliminated on the MCore.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer.  */

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	16

/* Register in which the static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM	1

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */
#define ELIMINABLE_REGS				\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM},}

/* Given FROM and TO register numbers, say whether this elimination
   is allowed.  */
#define CAN_ELIMINATE(FROM, TO) \
  (!((FROM) == FRAME_POINTER_REGNUM && FRAME_POINTER_REQUIRED))

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = mcore_initial_elimination_offset (FROM, TO)

/* Place that structure value return address is placed.  */
#define STRUCT_VALUE 0

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

/* The MCore has only general registers. There are
   also some special purpose registers: the T bit register, the
   procedure Link and the Count Registers */
enum reg_class
{
  NO_REGS,
  ONLYR1_REGS,
  LRW_REGS,
  GENERAL_REGS,
  C_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES  \
{			\
  "NO_REGS",		\
  "ONLYR1_REGS",	\
  "LRW_REGS",		\
  "GENERAL_REGS",	\
  "C_REGS",		\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

/* ??? STACK_POINTER_REGNUM should be excluded from LRW_REGS.  */
#define REG_CLASS_CONTENTS      	\
{					\
  {0x000000},  /* NO_REGS       */	\
  {0x000002},  /* ONLYR1_REGS   */	\
  {0x007FFE},  /* LRW_REGS      */	\
  {0x01FFFF},  /* GENERAL_REGS  */	\
  {0x020000},  /* C_REGS        */	\
  {0x0FFFFF}   /* ALL_REGS      */	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern const int regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[REGNO]

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers.  */
#define SMALL_REGISTER_CLASSES 1
 
/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  NO_REGS
#define BASE_REG_CLASS	 GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine 
   description.  */
extern const enum reg_class reg_class_from_letter[];

#define REG_CLASS_FROM_LETTER(C) \
   ( ISLOWER (C) ? reg_class_from_letter[(C) - 'a'] : NO_REGS )

/* The letters I, J, K, L, M, N, O, and P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.
	I: loadable by movi (0..127)
	J: arithmetic operand 1..32
	K: shift operand 0..31
	L: negative arithmetic operand -1..-32
	M: powers of two, constants loadable by bgeni
	N: powers of two minus 1, constants loadable by bmaski, including -1
        O: allowed by cmov with two constants +/- 1 of each other
        P: values we will generate 'inline' -- without an 'lrw'

   Others defined for use after reload
        Q: constant 1
	R: a label
        S: 0/1/2 cleared bits out of 32	[for bclri's]
        T: 2 set bits out of 32	[for bseti's]
        U: constant 0
        xxxS: 1 cleared bit out of 32 (complement of power of 2). for bclri
        xxxT: 2 cleared bits out of 32. for pairs of bclris.  */
#define CONST_OK_FOR_I(VALUE) (((int)(VALUE)) >= 0 && ((int)(VALUE)) <= 0x7f)
#define CONST_OK_FOR_J(VALUE) (((int)(VALUE)) >  0 && ((int)(VALUE)) <= 32)
#define CONST_OK_FOR_L(VALUE) (((int)(VALUE)) <  0 && ((int)(VALUE)) >= -32)
#define CONST_OK_FOR_K(VALUE) (((int)(VALUE)) >= 0 && ((int)(VALUE)) <= 31)
#define CONST_OK_FOR_M(VALUE) (exact_log2 (VALUE) >= 0)
#define CONST_OK_FOR_N(VALUE) (((int)(VALUE)) == -1 || exact_log2 ((VALUE) + 1) >= 0)
#define CONST_OK_FOR_O(VALUE) (CONST_OK_FOR_I(VALUE) || \
                               CONST_OK_FOR_M(VALUE) || \
                               CONST_OK_FOR_N(VALUE) || \
                               CONST_OK_FOR_M((int)(VALUE) - 1) || \
                               CONST_OK_FOR_N((int)(VALUE) + 1))

#define CONST_OK_FOR_P(VALUE) (mcore_const_ok_for_inline (VALUE)) 

#define CONST_OK_FOR_LETTER_P(VALUE, C)     \
     ((C) == 'I' ? CONST_OK_FOR_I (VALUE)   \
    : (C) == 'J' ? CONST_OK_FOR_J (VALUE)   \
    : (C) == 'L' ? CONST_OK_FOR_L (VALUE)   \
    : (C) == 'K' ? CONST_OK_FOR_K (VALUE)   \
    : (C) == 'M' ? CONST_OK_FOR_M (VALUE)   \
    : (C) == 'N' ? CONST_OK_FOR_N (VALUE)   \
    : (C) == 'P' ? CONST_OK_FOR_P (VALUE)   \
    : (C) == 'O' ? CONST_OK_FOR_O (VALUE)   \
    : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
   ((C) == 'G' ? CONST_OK_FOR_I (CONST_DOUBLE_HIGH (VALUE)) \
	      && CONST_OK_FOR_I (CONST_DOUBLE_LOW (VALUE))  \
    : 0)

/* Letters in the range `Q' through `U' in a register constraint string
   may be defined in a machine-dependent fashion to stand for arbitrary
   operand types.  */
#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'R' ? (GET_CODE (OP) == MEM			\
		 && GET_CODE (XEXP (OP, 0)) == LABEL_REF) \
   : (C) == 'S' ? (GET_CODE (OP) == CONST_INT \
                   && mcore_num_zeros (INTVAL (OP)) <= 2) \
   : (C) == 'T' ? (GET_CODE (OP) == CONST_INT \
                   && mcore_num_ones (INTVAL (OP)) == 2) \
   : (C) == 'Q' ? (GET_CODE (OP) == CONST_INT \
                   && INTVAL(OP) == 1) \
   : (C) == 'U' ? (GET_CODE (OP) == CONST_INT \
                   && INTVAL(OP) == 0) \
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS) mcore_reload_class (X, CLASS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */
#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X) \
  mcore_secondary_reload_class (CLASS, MODE, X)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS. 

   On MCore this is the size of MODE in words.  */
#define CLASS_MAX_NREGS(CLASS, MODE)  \
     (ROUND_ADVANCE (GET_MODE_SIZE (MODE)))

/* Stack layout; function entry, exit and calling.  */

/* Define the number of register that can hold parameters.
   These two macros are used only in other macro definitions below.  */
#define NPARM_REGS 6
#define FIRST_PARM_REG 2
#define FIRST_RET_REG 2

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD  

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* We don't define this, because the MCore does not support
   addresses with negative offsets.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET  0

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL)  0

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the MCore, the callee does not pop any of its arguments that were passed
   on the stack.  */
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  mcore_function_value (VALTYPE, FUNC)

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* how we are going to return big values */
/*
 * #define RETURN_IN_MEMORY(TYPE) \
 *   (TYPE_MODE (TYPE) == BLKmode \
 *    || ((TREE_CODE (TYPE) == RECORD_TYPE || TREE_CODE(TYPE) == UNION_TYPE) \
 *        && !(TYPE_MODE (TYPE) == SImode \
 * 	    || (TYPE_MODE (TYPE) == BLKmode \
 * 		&& TYPE_ALIGN (TYPE) == BITS_PER_WORD \
 * 		&& int_size_in_bytes (TYPE) == UNITS_PER_WORD))))
 */ 


/* How many registers to use for struct return.  */
#define	RETURN_IN_MEMORY(TYPE) (int_size_in_bytes (TYPE) > 2 * UNITS_PER_WORD)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, FIRST_RET_REG)

/* 1 if N is a possible register number for a function value.
   On the MCore, only r4 can return results.  */
#define FUNCTION_VALUE_REGNO_P(REGNO)  ((REGNO) == FIRST_RET_REG)

#define	MUST_PASS_IN_STACK(MODE,TYPE)  \
  mcore_must_pass_on_stack (MODE, TYPE)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(REGNO)  \
  ((REGNO) >= FIRST_PARM_REG && (REGNO) < (NPARM_REGS + FIRST_PARM_REG))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On MCore, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus NARGREGS or more means all following args should go on the stack.  */
#define CUMULATIVE_ARGS  int

#define ROUND_ADVANCE(SIZE)	\
  ((SIZE + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Round a register number up to a proper boundary for an arg of mode 
   MODE. 
   
   We round to an even reg for things larger than a word.  */
#define ROUND_REG(X, MODE) 				\
  ((TARGET_8ALIGN 					\
   && GET_MODE_UNIT_SIZE ((MODE)) > UNITS_PER_WORD) 	\
   ? ((X) + ((X) & 1)) : (X))


/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On MCore, the offset always starts at 0: the first parm reg is always
   the same reg.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT)  \
  ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be
   available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	   \
 ((CUM) = (ROUND_REG ((CUM), (MODE))			   \
	   + ((NAMED) * mcore_num_arg_regs (MODE, TYPE)))) \

/* Define where to put the arguments to a function.  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  mcore_function_arg (CUM, MODE, TYPE, NAMED)

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  MUST_PASS_IN_STACK (MODE, TYPE)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
   Any arg that starts in the first NPARM_REGS regs but won't entirely
   fit in them needs partial registers on the MCore.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  mcore_function_arg_partial_nregs (CUM, MODE, TYPE, NAMED)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.  */
#define SETUP_INCOMING_VARARGS(ASF, MODE, TYPE, PAS, ST) \
  mcore_setup_incoming_varargs (ASF, MODE, TYPE, & PAS)

/* Call the function profiler with a given profile label.  */
#define FUNCTION_PROFILER(STREAM,LABELNO)		\
{							\
  fprintf (STREAM, "	trap	1\n");			\
  fprintf (STREAM, "	.align	2\n");			\
  fprintf (STREAM, "	.long	LP%d\n", (LABELNO));	\
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
#define EXIT_IGNORE_STACK 0

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   On the MCore, the trapoline looks like:
   	lrw	r1,  function
     	lrw	r13, area
   	jmp	r13
   	or	r0, r0
    .literals                                                */
#define TRAMPOLINE_TEMPLATE(FILE)  		\
{						\
  fprintf ((FILE), "	.short	0x7102\n");	\
  fprintf ((FILE), "	.short	0x7d02\n");	\
  fprintf ((FILE), "	.short	0x00cd\n");     \
  fprintf ((FILE), "	.short	0x1e00\n");	\
  fprintf ((FILE), "	.long	0\n");		\
  fprintf ((FILE), "	.long	0\n");		\
}

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  12

/* Alignment required for a trampoline in bits.  */
#define TRAMPOLINE_ALIGNMENT  32

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

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) < AP_REG || (unsigned) reg_renumber[(REGNO)] < AP_REG)

#define REGNO_OK_FOR_INDEX_P(REGNO)   0

/* Maximum number of registers that can appear in a valid memory 
   address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) 	 (GET_CODE (X) == LABEL_REF)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the MCore, allow anything but a double.  */
#define LEGITIMATE_CONSTANT_P(X) (GET_CODE(X) != CONST_DOUBLE)

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)
/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.  */
#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
    	(REGNO (X) <= 16 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X)	0

#else

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X)	\
	REGNO_OK_FOR_BASE_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X)	0

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


/* Jump to LABEL if X is a valid address RTX.  This must also take
   REG_OK_STRICT into account when deciding about valid registers, but it uses
   the above macros so we are in luck.  
 
   Allow  REG
	  REG+disp 

   A legitimate index for a QI is 0..15, for HI is 0..30, for SI is 0..60,
   and for DI is 0..56 because we use two SI loads, etc.  */
#define GO_IF_LEGITIMATE_INDEX(MODE, REGNO, OP, LABEL)			\
  do									\
    {									\
      if (GET_CODE (OP) == CONST_INT) 					\
        {								\
	  if (GET_MODE_SIZE (MODE) >= 4					\
	      && (((unsigned)INTVAL (OP)) % 4) == 0			\
	      &&  ((unsigned)INTVAL (OP)) <= 64 - GET_MODE_SIZE (MODE))	\
	    goto LABEL;							\
	  if (GET_MODE_SIZE (MODE) == 2 				\
	      && (((unsigned)INTVAL (OP)) % 2) == 0			\
	      &&  ((unsigned)INTVAL (OP)) <= 30)			\
	    goto LABEL;							\
	  if (GET_MODE_SIZE (MODE) == 1 				\
	      && ((unsigned)INTVAL (OP)) <= 15)				\
	    goto LABEL;							\
        }								\
    }									\
  while (0)

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)                  \
{ 								  \
  if (BASE_REGISTER_RTX_P (X))					  \
    goto LABEL;							  \
  else if (GET_CODE (X) == PLUS || GET_CODE (X) == LO_SUM) 	  \
    {								  \
      rtx xop0 = XEXP (X,0);					  \
      rtx xop1 = XEXP (X,1);					  \
      if (BASE_REGISTER_RTX_P (xop0))				  \
	GO_IF_LEGITIMATE_INDEX (MODE, REGNO (xop0), xop1, LABEL); \
      if (BASE_REGISTER_RTX_P (xop1))				  \
	GO_IF_LEGITIMATE_INDEX (MODE, REGNO (xop1), xop0, LABEL); \
    }								  \
}								   
								   
/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)  \
{									\
  if (   GET_CODE (ADDR) == PRE_DEC || GET_CODE (ADDR) == POST_DEC	\
      || GET_CODE (ADDR) == PRE_INC || GET_CODE (ADDR) == POST_INC)	\
    goto LABEL;								\
}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* 'char' is signed by default.  */
#define DEFAULT_SIGNED_CHAR  0

/* The type of size_t unsigned int.  */
#define SIZE_TYPE "unsigned int"

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
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS TARGET_SLOW_BYTES

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */
#define STORE_FLAG_VALUE 1

/* Immediate shift counts are truncated by the output routines (or was it
   the assembler?).  Shift counts in a register are truncated by ARM.  Note
   that the native compiler puts too large (> 32) immediate shift counts
   into a register and shifts by the register, letting the ARM decide what
   to do instead of doing that itself.  */
#define SHIFT_COUNT_TRUNCATED 1

/* All integers have the same format so truncation is easy.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC,INPREC)  1

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
/* why is this defined??? -- dac */
#define NO_FUNCTION_CSE 1

/* Chars and shorts should be passed as ints.  */
#define PROMOTE_PROTOTYPES 1

/* The machine modes of pointers and functions.  */
#define Pmode          SImode
#define FUNCTION_MODE  Pmode

/* The relative costs of various types of constants.  Note that cse.c defines
   REG = 1, SUBREG = 2, any node = (2 + sum of subnodes).  */
#define CONST_COSTS(RTX, CODE, OUTER_CODE)      \
  case CONST_INT:				\
    return mcore_const_costs (RTX, OUTER_CODE); \
  case CONST: 					\
  case LABEL_REF:				\
  case SYMBOL_REF:				\
    return 5;					\
  case CONST_DOUBLE:				\
      return 10;

/* provide the cost for an address calculation.
   All addressing modes cost the same on the MCore.  */
#define	ADDRESS_COST(RTX)	1

/* Provide the cost of an rtl expression. */
#define RTX_COSTS(X, CODE, OUTER_CODE)			\
  case AND:                                             \
    return COSTS_N_INSNS (mcore_and_cost (X));          \
  case IOR:                                             \
    return COSTS_N_INSNS (mcore_ior_cost (X));          \
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (100);				\
  case FLOAT:						\
  case FIX:						\
    return 100;

/* Compute extra cost of moving data between one register class
   and another.  All register moves are cheap.  */
#define REGISTER_MOVE_COST(MODE, SRCCLASS, DSTCLASS) 2

#define WORD_REGISTER_OPERATIONS

/* Implicit library calls should use memcpy, not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* Assembler output control.  */
#define ASM_COMMENT_START "\t//"

#define ASM_APP_ON	"// inline asm begin\n"
#define ASM_APP_OFF	"// inline asm end\n"

#define FILE_ASM_OP     "\t.file\n"

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

#undef  EXTRA_SECTIONS
#define EXTRA_SECTIONS SUBTARGET_EXTRA_SECTIONS

#undef  EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS			\
  SUBTARGET_EXTRA_SECTION_FUNCTIONS		\
  SWITCH_SECTION_FUNCTION

/* Switch to SECTION (an `enum in_section').

   ??? This facility should be provided by GCC proper.
   The problem is that we want to temporarily switch sections in
   ASM_DECLARE_OBJECT_NAME and then switch back to the original section
   afterwards.  */
#define SWITCH_SECTION_FUNCTION					\
static void switch_to_section PARAMS ((enum in_section, tree));	\
static void							\
switch_to_section (section, decl)				\
     enum in_section section;					\
     tree decl;							\
{								\
  switch (section)						\
    {								\
      case in_text: text_section (); break;			\
      case in_data: data_section (); break;			\
      case in_named: named_section (decl, NULL, 0); break;	\
      SUBTARGET_SWITCH_SECTIONS      				\
      default: abort (); break;					\
    }								\
}

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  mcore_asm_named_section

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */
#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tsubi\t %s,%d\n\tstw\t %s,(%s)\n",	\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT),		\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM])

/* Length in instructions of the code output by ASM_OUTPUT_REG_PUSH.  */
#define REG_PUSH_LENGTH 2

/* This is how to output an insn to pop a register from the stack.  */
#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tldw\t %s,(%s)\n\taddi\t %s,%d\n",	\
	   reg_names[REGNO],				\
	   reg_names[STACK_POINTER_REGNUM],		\
	   reg_names[STACK_POINTER_REGNUM],		\
	   (STACK_BOUNDARY / BITS_PER_UNIT))

  
/* Output a reference to a label.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)  \
  fprintf (STREAM, "%s%s", USER_LABEL_PREFIX, \
	   (* targetm.strip_name_encoding) (NAME))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align\t%d\n", LOG)

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

#define MULTIPLE_SYMBOL_SPACES 1

#define SUPPORTS_ONE_ONLY 1

/* A pair of macros to output things for the callgraph data.
   VALUE means (to the tools that reads this info later):
  	0 a call from src to dst
  	1 the call is special (e.g. dst is "unknown" or "alloca")
  	2 the call is special (e.g., the src is a table instead of routine)
  
   Frame sizes are augmented with timestamps to help later tools 
   differentiate between static entities with same names in different
   files.  */
extern long mcore_current_compilation_timestamp;
#define	ASM_OUTPUT_CG_NODE(FILE,SRCNAME,VALUE)				\
  do									\
    {									\
      if (mcore_current_compilation_timestamp == 0)			\
        mcore_current_compilation_timestamp = time (0);			\
      fprintf ((FILE),"\t.equ\t__$frame$size$_%s_$_%08lx,%d\n",		\
             (SRCNAME), mcore_current_compilation_timestamp, (VALUE));	\
    }									\
  while (0)

#define	ASM_OUTPUT_CG_EDGE(FILE,SRCNAME,DSTNAME,VALUE)		\
  do								\
    {								\
      fprintf ((FILE),"\t.equ\t__$function$call$_%s_$_%s,%d\n",	\
             (SRCNAME), (DSTNAME), (VALUE));			\
    }								\
  while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.export\t"

/* The prefix to add to user-visible assembler symbols. */
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* Make an internal label into a string.  */
#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*.%s%ld", PREFIX, (long) NUM)

/* Output an internal label definition.  */
#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)  \
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),  \
   sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* Jump tables must be 32 bit aligned. */
#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(STREAM,PREFIX,NUM,TABLE) \
  fprintf (STREAM, "\t.align 2\n.%s%d:\n", PREFIX, NUM);

/* Output a relative address. Not needed since jump tables are absolute
   but we must define it anyway.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)  \
  fputs ("- - - ASM_OUTPUT_ADDR_DIFF_ELT called!\n", STREAM)

/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  \
    fprintf (STREAM, "\t.long\t.L%d\n", VALUE)

/* Output various types of constants.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */
#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.fill %d, 1\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol, with alignment information.  */
/* XXX - for now we ignore the alignment.  */     
#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)	\
  do								\
    {								\
      if (mcore_dllexport_name_p (NAME))			\
	MCORE_EXPORT_NAME (FILE, NAME)				\
      if (! mcore_dllimport_name_p (NAME))			\
        {							\
          fputs ("\t.comm\t", FILE);				\
          assemble_name (FILE, NAME);				\
          fprintf (FILE, ",%d\n", SIZE);			\
        }							\
    }								\
  while (0)

/* This says how to output an assembler line
   to define an external symbol.  */
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)   \
  do						\
    {						\
      fputs ("\t.import\t", (FILE));		\
      assemble_name ((FILE), (NAME));		\
      fputs ("\n", (FILE));			\
    }						\
  while (0)
     
#undef	ASM_OUTPUT_EXTERNAL
/* RBE: we undefined this and let gas do it's "undefined is imported"
   games. This is because when we use this, we get a marked 
   reference through the call to assemble_name and this forces C++
   inlined member functions (or any inlined function) to be instantiated
   regardless of whether any callsites remain.
   This makes this aspect of the compiler non-ABI compliant.  */

/* Similar, but for libcall. FUN is an rtx.  */
#undef  ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)	\
  do						\
    {						\
      fprintf (FILE, "\t.import\t");		\
      assemble_name (FILE, XSTR (FUN, 0));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)


/* This says how to output an assembler line
   to define a local common symbol...  */
#undef  ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  (fputs ("\t.lcomm\t", FILE),				\
  assemble_name (FILE, NAME),				\
  fprintf (FILE, ",%d\n", SIZE))

/* ... and how to define a local common symbol whose alignment
   we wish to specify.  ALIGN comes in as bits, we have to turn
   it into bytes.  */
#undef  ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      fputs ("\t.bss\t", (FILE));					\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%d,%d\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
    }									\
  while (0)

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */
#define PRINT_OPERAND(STREAM, X, CODE)  mcore_print_operand (STREAM, X, CODE)

/* Print a memory address as an operand to reference that memory location.  */
#define PRINT_OPERAND_ADDRESS(STREAM,X)  mcore_print_operand_address (STREAM, X)

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR)=='.' || (CHAR) == '#' || (CHAR) == '*' || (CHAR) == '^' || (CHAR) == '!')

/* This is to handle loads from the constant pool.  */
#define MACHINE_DEPENDENT_REORG(X) mcore_dependent_reorg (X)

#define PREDICATE_CODES							\
  { "mcore_arith_reg_operand",		{ REG, SUBREG }},		\
  { "mcore_general_movsrc_operand",	{ MEM, CONST_INT, REG, SUBREG }},\
  { "mcore_general_movdst_operand",	{ MEM, CONST_INT, REG, SUBREG }},\
  { "mcore_reload_operand",	        { MEM, REG, SUBREG }},		\
  { "mcore_arith_J_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_K_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_K_operand_not_0",	{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_M_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_K_S_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_O_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_imm_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_arith_any_imm_operand",	{ CONST_INT, REG, SUBREG }},	\
  { "mcore_literal_K_operand",		{ CONST_INT }},			\
  { "mcore_addsub_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_compare_operand",		{ CONST_INT, REG, SUBREG }},	\
  { "mcore_load_multiple_operation",	{ PARALLEL }},			\
  { "mcore_store_multiple_operation",	{ PARALLEL }},			\
  { "mcore_call_address_operand",	{ REG, SUBREG, CONST_INT }},	\

#endif /* ! GCC_MCORE_H */
