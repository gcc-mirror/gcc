/* GCC backend definitions for the TI MSP430 Processor
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Red Hat.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* Run-time Target Specification */

/* True if the MSP430x extensions are enabled.  */
#ifndef IN_LIBGCC2
extern bool msp430x;
#endif

#define TARGET_CPU_CPP_BUILTINS()               \
  do                                            \
    {                                           \
      builtin_define ("NO_TRAMPOLINES");        \
      builtin_define ("__MSP430__"); 		\
      if (msp430x)				\
	{					\
	  builtin_define ("__MSP430X__");	\
	  builtin_assert ("cpu=MSP430X");	\
	  if (TARGET_LARGE)			\
	    builtin_define ("__MSP430X_LARGE__");	\
	}					\
      else					\
	builtin_assert ("cpu=MSP430"); 		\
    }                                           \
  while (0)

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:crt0.o%s} crtbegin.o%s"

/* -lgcc is included because crtend.o needs __mspabi_func_epilog_1.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s -lgcc"

#define ASM_SPEC "-mP " /* Enable polymorphic instructions.  */ \
  "%{mmcu=msp430x:-mmcu=msp430X;mmcu=*:-mmcu=%*} " /* Pass the MCU type on to the assembler.  */  \
  "%{mrelax=-mQ} " /* Pass the relax option on to the assembler.  */ \
  "%{mlarge:-ml} " /* Tell the assembler if we are building for the LARGE pointer model.  */ \
  "%{ffunction-sections:-gdwarf-sections}" /* If function sections are being created then create DWARF line number sections as well.  */

/* Enable linker section garbage collection by default, unless we
   are creating a relocatable binary (gc does not work) or debugging
   is enabled  (the GDB testsuite relies upon unused entities not being deleted).  */
#define LINK_SPEC "%{mrelax:--relax} %{mlarge:%{!r:%{!g:--gc-sections}}}"

#undef  LIB_SPEC
#define LIB_SPEC "					\
--start-group						\
-lc							\
-lgcc							\
%{msim:-lsim}						\
%{!msim:-lnosys}					\
--end-group					   	\
%{!T*: %{msim: %{mlarge:%Tmsp430xl-sim.ld}%{!mlarge:%Tmsp430-sim.ld}}%{!msim:%Tmsp430.ld}}	\
"


/* Storage Layout */

#define BITS_BIG_ENDIAN 		0
#define BYTES_BIG_ENDIAN 		0
#define WORDS_BIG_ENDIAN 		0


#ifdef IN_LIBGCC2
/* This is to get correct SI and DI modes in libgcc2.c (32 and 64 bits).  */
#define	UNITS_PER_WORD			4
/* We have a problem with libgcc2.  It only defines two versions of
   each function, one for "int" and one for "long long".  Ie it assumes
   that "sizeof (int) == sizeof (long)".  For the MSP430 this is not true
   and we need a third set of functions.  We explicitly define
   LIBGCC2_UNITS_PER_WORD here so that it is clear that we are expecting
   to get the SI and DI versions from the libgcc2.c sources, and we
   provide our own set of HI functions, which is why this
   definition is surrounded by #ifndef..#endif.  */
#ifndef LIBGCC2_UNITS_PER_WORD
#define LIBGCC2_UNITS_PER_WORD 		4
#endif
#else
/* Actual width of a word, in units (bytes).  */
#define	UNITS_PER_WORD 			2
#endif

#define SHORT_TYPE_SIZE			16
#define INT_TYPE_SIZE			16
#define LONG_TYPE_SIZE			32
#define LONG_LONG_TYPE_SIZE		64

#define FLOAT_TYPE_SIZE 		32
#define DOUBLE_TYPE_SIZE 		64
#define LONG_DOUBLE_TYPE_SIZE		64 /*DOUBLE_TYPE_SIZE*/

#define LIBGCC2_HAS_DF_MODE		1
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE   64

#define DEFAULT_SIGNED_CHAR		0

#define STRICT_ALIGNMENT 		1
#define FUNCTION_BOUNDARY 		16
#define BIGGEST_ALIGNMENT 		16
#define STACK_BOUNDARY 			16
#define PARM_BOUNDARY 			8
#define PCC_BITFIELD_TYPE_MATTERS	1

#define STACK_GROWS_DOWNWARD		1
#define FRAME_GROWS_DOWNWARD		1
#define FIRST_PARM_OFFSET(FNDECL) 	0

#define MAX_REGS_PER_ADDRESS 		1

#define Pmode 				(TARGET_LARGE ? PSImode : HImode)
/* Note: 32 is a lie.  Large pointers are actually 20-bits wide.  But gcc
   thinks that any non-power-of-2 pointer size equates to BLKmode, which
   causes all kinds of problems...  */
#define POINTER_SIZE			(TARGET_LARGE ? 32 : 16)
#define	POINTERS_EXTEND_UNSIGNED	1

#define ADDR_SPACE_NEAR	1
#define ADDR_SPACE_FAR	2

#define REGISTER_TARGET_PRAGMAS() msp430_register_pragmas()

#if 1 /* XXX */
/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 2)      	\
    (MODE) = HImode;
#endif

/* Layout of Source Language Data Types */

#undef  SIZE_TYPE
#define SIZE_TYPE			(TARGET_LARGE ? "long unsigned int" : "unsigned int")
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE			(TARGET_LARGE ? "long int" : "int")
#undef  WCHAR_TYPE
#define WCHAR_TYPE			"long int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE			BITS_PER_WORD
#define FUNCTION_MODE 			HImode
#define CASE_VECTOR_MODE		Pmode
#define HAS_LONG_COND_BRANCH		0
#define HAS_LONG_UNCOND_BRANCH		0

#define LOAD_EXTEND_OP(M)		ZERO_EXTEND
/*#define WORD_REGISTER_OPERATIONS	1*/

#define MOVE_MAX 			8
#define STARTING_FRAME_OFFSET		0

#define INCOMING_RETURN_ADDR_RTX \
  msp430_incoming_return_addr_rtx ()

#define RETURN_ADDR_RTX(COUNT, FA)		\
  msp430_return_addr_rtx (COUNT)

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)   1

#define SLOW_BYTE_ACCESS		0


/* Register Usage */

/* gas doesn't recognize PC (R0), SP (R1), and SR (R2) as register
   names.  */
#define REGISTER_NAMES						\
{								\
  "R0", "R1", "R2", "R3", "R4", "R5", "R6", "R7",		\
    "R8", "R9", "R10", "R11", "R12", "R13", "R14", "R15",	\
  "argptr"							\
}

enum reg_class
{
  NO_REGS,
  R12_REGS,
  R13_REGS,
  GEN_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define REG_CLASS_NAMES \
{			\
  "NO_REGS",		\
  "R12_REGS",		\
  "R13_REGS",		\
  "GEN_REGS",		\
  "ALL_REGS"		\
}

#define REG_CLASS_CONTENTS \
{			   \
  0x00000000,		   \
  0x00001000,		   \
  0x00002000,		   \
  0x0000fff2,		   \
  0x0001ffff		   \
}

#define GENERAL_REGS			GEN_REGS
#define BASE_REG_CLASS  		GEN_REGS
#define INDEX_REG_CLASS			GEN_REGS
#define N_REG_CLASSES			(int) LIM_REG_CLASSES

#define PC_REGNUM 		        0
#define STACK_POINTER_REGNUM 	        1
#define CC_REGNUM                       2
#define FRAME_POINTER_REGNUM 		4 /* not usually used, call preserved */
#define ARG_POINTER_REGNUM 		16
#define STATIC_CHAIN_REGNUM 		5 /* FIXME */

#define FIRST_PSEUDO_REGISTER 		17

#define REGNO_REG_CLASS(REGNO)          ((REGNO) < 17 \
					 ? GEN_REGS : NO_REGS)

#define TRAMPOLINE_SIZE			4 /* FIXME */
#define TRAMPOLINE_ALIGNMENT		16 /* FIXME */

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM },	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	\
  (OFFSET) = msp430_initial_elimination_offset ((FROM), (TO))


#define FUNCTION_ARG_REGNO_P(N)	  	((N) >= 8 && (N) < ARG_POINTER_REGNUM)
#define DEFAULT_PCC_STRUCT_RETURN	0

/* 1 == register can't be used by gcc, in general
   0 == register can be used by gcc, in general */
#define FIXED_REGISTERS					\
{							\
  1,0,1,1, 0,0,0,0,					\
  0,0,0,0, 0,0,0,0,					\
  1,							\
}

/* 1 == value changes across function calls
   0 == value is the same after a call      */
/* R4 through R10 are callee-saved */
#define CALL_USED_REGISTERS				\
{							\
  1,0,1,1, 0,0,0,0,					\
  0,0,0,1, 1,1,1,1,					\
  1,						\
}

#define REG_ALLOC_ORDER					\
  { 12, 13, 14, 15, 10, 9, 8, 7, 6, 5, 4, 11, 0, 1, 2, 3, 16 }
/*  { 11, 15, 14, 13, 12, 10, 9, 8, 7, 6, 5, 4, 0, 1, 2, 3, 16 }*/

#define REGNO_OK_FOR_BASE_P(regno)	1
#define REGNO_OK_FOR_INDEX_P(regno)	1



typedef struct
{
  /* These two are the current argument status.  */
  char reg_used[4];
#define CA_FIRST_REG 12
  char can_split;
  /* These two are temporaries used internally.  */
  char start_reg;
  char reg_count;
  char mem_count;
  char special_p;
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CA, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  msp430_init_cumulative_args (&CA, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS)


/* FIXME */
#define NO_PROFILE_COUNTERS     1
#define PROFILE_BEFORE_PROLOGUE 1

#define FUNCTION_PROFILER(FILE, LABELNO)	\
    fprintf (FILE, "\tcall\t__mcount\n");

#define HARD_REGNO_NREGS(REGNO, MODE)            \
  msp430_hard_regno_nregs (REGNO, MODE)

#define HARD_REGNO_MODE_OK(REGNO, MODE) 			\
  msp430_hard_regno_mode_ok (REGNO, MODE)

#define MODES_TIEABLE_P(MODE1, MODE2)				\
  msp430_modes_tieable_p (MODE1, MODE2)

/* Exception Handling */

/* R12,R13,R14 - EH data
   R15 - stack adjustment */

#define EH_RETURN_DATA_REGNO(N) \
  (((N) < 3) ? ((N) + 12) : INVALID_REGNUM)

#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM(Pmode, gen_rtx_PLUS (Pmode, gen_rtx_REG(Pmode, SP_REGNO), gen_rtx_REG (Pmode, 15)))

#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (Pmode, 15)

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) DW_EH_PE_udata4


/* Stack Layout and Calling Conventions */


/* Addressing Modes */



#define TEXT_SECTION_ASM_OP ".text"
#define DATA_SECTION_ASM_OP ".data"
#define BSS_SECTION_ASM_OP   "\t.section .bss"

#define ASM_COMMENT_START	" ;"
#define ASM_APP_ON		""
#define ASM_APP_OFF 		""
#define LOCAL_LABEL_PREFIX	".L"
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX	""

#define GLOBAL_ASM_OP 		"\t.global\t"

#define ASM_OUTPUT_LABELREF(FILE, SYM) msp430_output_labelref ((FILE), (SYM))

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.long .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   Note: The local label referenced by the "3b" below is emitted by
   the tablejump insn.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t.long .L%d - 1b\n", VALUE)


#define ASM_OUTPUT_ALIGN(STREAM, LOG)		\
  do						\
    {						\
      if ((LOG) == 0)				\
        break;					\
      fprintf (STREAM, "\t.balign %d\n", 1 << (LOG));	\
    }						\
  while (0)

#define JUMP_TABLES_IN_TEXT_SECTION	1

#undef	DWARF2_ADDR_SIZE
#define	DWARF2_ADDR_SIZE			4

#define INCOMING_FRAME_SP_OFFSET		(POINTER_SIZE / BITS_PER_UNIT)

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#define DWARF2_ASM_LINE_DEBUG_INFO		1

/* Prevent reload (and others) from choosing HImode stack slots
   when spilling hard registers when they may contain PSImode values.  */
#define HARD_REGNO_CALLER_SAVE_MODE(REGNO,NREGS,MODE) \
  ((TARGET_LARGE && ((NREGS) <= 2)) ? PSImode : choose_hard_reg_mode ((REGNO), (NREGS), false))

/* Also stop GCC from thinking that it can eliminate (SUBREG:PSI (SI)).  */
#define CANNOT_CHANGE_MODE_CLASS(FROM,TO,CLASS) \
  (   ((TO) == PSImode && (FROM) == SImode)	\
   || ((TO) == SImode  && (FROM) == PSImode)    \
   || ((TO) == DImode  && (FROM) == PSImode)    \
   || ((TO) == PSImode && (FROM) == DImode)     \
      )

#define ACCUMULATE_OUTGOING_ARGS 1
