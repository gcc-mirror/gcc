/* Definitions of target machine for TI PRU.
   Copyright (C) 2014-2020 Free Software Foundation, Inc.
   Contributed by Dimitar Dimitrov <dimitar@dinux.eu>

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

#ifndef GCC_PRU_H
#define GCC_PRU_H

#include "config/pru/pru-opts.h"

/* Define built-in preprocessor macros.  */
#define TARGET_CPU_CPP_BUILTINS()		    \
  do						    \
    {						    \
      builtin_define_std ("__PRU__");		    \
      builtin_define_std ("__pru__");		    \
      builtin_define_std ("__PRU_V3__");	    \
      builtin_define_std ("__LITTLE_ENDIAN__");	    \
      builtin_define_std ("__little_endian__");	    \
      /* Trampolines are disabled for now.  */	    \
      builtin_define_std ("NO_TRAMPOLINES");	    \
    }						    \
  while (0)

/* TI ABI implementation is not feature-complete enough (e.g. function
   pointers are not supported), so we cannot list it as a multilib variant.
   To prevent misuse from users, do not link any of the standard libraries.  */
#define DRIVER_SELF_SPECS			      \
  "%{mabi=ti:-nodefaultlibs} "			      \
  "%{mmcu=*:-specs=device-specs/%*%s %<mmcu=*} "

#undef CPP_SPEC
#define CPP_SPEC					\
  "%(cpp_device) "					\
  "%{mabi=ti:-D__PRU_EABI_TI__; :-D__PRU_EABI_GNU__}"

/* Do not relax when in TI ABI mode since TI tools do not always
   put PRU_S10_PCREL.  */
#undef  LINK_SPEC
#define LINK_SPEC					    \
  "%(link_device) "					    \
  "%{mabi=ti:--no-relax;:%{mno-relax:--no-relax;:--relax}} "   \
  "%{shared:%eshared is not supported} "

/* CRT0 is carefully maintained to be compatible with both GNU and TI ABIs.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC							\
  "%{!pg:%{minrt:crt0-minrt.o%s}%{!minrt:crt0.o%s}} %{!mabi=ti:-lgcc} "

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "%{!mabi=ti:-lgloss} "

/* TI ABI mandates that ELF symbols do not start with any prefix.  */
#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX ".L"

/* Storage layout.  */

#define DEFAULT_SIGNED_CHAR 0
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

/* PRU is represented in GCC as an 8-bit CPU with fast 16-bit and 32-bit
   arithmetic.  */
#define BITS_PER_WORD 8

#ifdef IN_LIBGCC2
/* This is to get correct SI and DI modes in libgcc2.c (32 and 64 bits).  */
#define UNITS_PER_WORD 4
#else
/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 1
#endif

#define POINTER_SIZE 32
#define BIGGEST_ALIGNMENT 8
#define STRICT_ALIGNMENT 0
#define FUNCTION_BOUNDARY 8	/* Func pointers are word-addressed.  */
#define PARM_BOUNDARY 8
#define STACK_BOUNDARY 8
#define MAX_FIXED_MODE_SIZE 64

#define POINTERS_EXTEND_UNSIGNED 1

/* Layout of source language data types.  */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE DOUBLE_TYPE_SIZE

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"


/* Basic characteristics of PRU registers:

   Regno  Name
   0      r0		  Caller Saved.  Also used as a static chain register.
   1      r1		  Caller Saved.  Also used as a temporary by function.
			  profiler and function prologue/epilogue.
   2      r2       sp	  Stack Pointer.
   3*     r3.w0		  Caller saved.
   3*     r3.w2    ra	  Return Address (16-bit).
   4      r4       fp	  Frame Pointer, also called Argument Pointer in ABI.
   5-13   r5-r13	  Callee Saved Registers.
   14-29  r14-r29	  Register Arguments.  Caller Saved Registers.
   14-15  r14-r15	  Return Location.
   30     r30		  Special I/O register.  Not used by compiler.
   31     r31		  Special I/O register.  Not used by compiler.

   32     loop_cntr	  Internal register used as a counter by LOOP insns.

   33     pc		  Not an actual register.

   34     fake_fp	  Fake Frame Pointer (always eliminated).
   35     fake_ap	  Fake Argument Pointer (always eliminated).
   36			  First Pseudo Register.

   The definitions for some hard register numbers are located in pru.md.
   Note that GCC's internal register numbering differs from the conventional
   register naming in PRU ISA.  PRU ISA defines word-based register numbers
   and sub-register suffixes (e.g. RA is r3.w0).  GCC uses linear numbering
   of 8 bit sub-registers (e.g. RA starts at r12).  When outputting assembly,
   GCC will take into account the RTL operand size (e.g. r12:HI) in order to
   translate to the conventional PRU ISA format expected by GAS (r3.w0).
*/

#define FIXED_REGISTERS				\
  {						\
/*   0 */  0,0,0,0, 0,0,0,0, 1,1,1,1, 0,0,1,1,	\
/*   4 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*   8 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*  12 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*  16 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*  20 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*  24 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*  28 */  0,0,0,0, 0,0,0,0, 1,1,1,1, 1,1,1,1,	\
/*  32 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1	\
  }

/* Call used == caller saved + fixed regs + args + ret vals.  */
#define CALL_USED_REGISTERS			\
  {						\
/*   0 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,	\
/*   4 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*   8 */  0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,	\
/*  12 */  0,0,0,0, 0,0,0,0, 1,1,1,1, 1,1,1,1,	\
/*  16 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,	\
/*  20 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,	\
/*  24 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,	\
/*  28 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1,	\
/*  32 */  1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1	\
  }

#define PRU_SEQ_R(X)  (X) * 4 + 0, (X) * 4 + 1, (X) * 4 + 2, (X) * 4 + 3
#define PRU_SEQ_R_W0(X)  (X) * 4 + 0, (X) * 4 + 1
#define PRU_SEQ_R_W2(X)  (X) * 4 + 2, (X) * 4 + 3
#define REG_ALLOC_ORDER							    \
  {									    \
    /* Call-clobbered, yet not used for parameters.  */			    \
    PRU_SEQ_R (0),  PRU_SEQ_R ( 1),					    \
    PRU_SEQ_R_W0 (3),							    \
									    \
    PRU_SEQ_R (14), PRU_SEQ_R (15), PRU_SEQ_R (16), PRU_SEQ_R (17),	    \
    PRU_SEQ_R (18), PRU_SEQ_R (19), PRU_SEQ_R (20), PRU_SEQ_R (21),	    \
    PRU_SEQ_R (22), PRU_SEQ_R (23), PRU_SEQ_R (24), PRU_SEQ_R (25),	    \
    PRU_SEQ_R (26), PRU_SEQ_R (27), PRU_SEQ_R (28), PRU_SEQ_R (29),	    \
									    \
    PRU_SEQ_R ( 5), PRU_SEQ_R ( 6), PRU_SEQ_R ( 7), PRU_SEQ_R ( 8),	    \
    PRU_SEQ_R ( 9), PRU_SEQ_R (10), PRU_SEQ_R (11), PRU_SEQ_R (12),	    \
    PRU_SEQ_R (13),							    \
									    \
    PRU_SEQ_R ( 4),							    \
    PRU_SEQ_R ( 2),							    \
    PRU_SEQ_R_W2 (3),							    \
									    \
    /* I/O and virtual registers.  */					    \
    PRU_SEQ_R (30), PRU_SEQ_R (31), PRU_SEQ_R (32), PRU_SEQ_R (33),	    \
    PRU_SEQ_R (34), PRU_SEQ_R (35)					    \
  }

/* Register Classes.  */

enum reg_class
{
  NO_REGS,
  SIB_REGS,
  LOOPCNTR_REGS,
  MULDST_REGS,
  MULSRC0_REGS,
  MULSRC1_REGS,
  GP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES   \
  {  "NO_REGS",		  \
     "SIB_REGS",	  \
     "LOOPCNTR_REGS",	  \
     "MULDST_REGS",	  \
     "MULSRC0_REGS",	  \
     "MULSRC1_REGS",	  \
     "GP_REGS",		  \
     "ALL_REGS" }

#define GENERAL_REGS ALL_REGS

#define REG_CLASS_CONTENTS					\
  {								\
    /* NO_REGS	      */ { 0, 0, 0, 0, 0},			\
    /* SIB_REGS	      */ { 0xf, 0xff000000, ~0, 0xffffff, 0},	\
    /* LOOPCNTR_REGS  */ { 0, 0, 0, 0, 0xf},			\
    /* MULDST_REGS    */ { 0, 0, 0, 0x00000f00, 0},		\
    /* MULSRC0_REGS   */ { 0, 0, 0, 0x000f0000, 0},		\
    /* MULSRC1_REGS   */ { 0, 0, 0, 0x00f00000, 0},		\
    /* GP_REGS	      */ { ~0, ~0, ~0, ~0, 0},			\
    /* ALL_REGS	      */ { ~0,~0, ~0, ~0, ~0}			\
  }


#define GP_REG_P(REGNO) ((unsigned)(REGNO) <= LAST_GP_REGNUM)
#define REGNO_REG_CLASS(REGNO)						    \
	((REGNO) == MULDST_REGNUM ? MULDST_REGS				    \
	 : (REGNO) == MULSRC0_REGNUM ? MULSRC0_REGS			    \
	 : (REGNO) == MULSRC1_REGNUM ? MULSRC1_REGS			    \
	 : (REGNO) >= FIRST_ARG_REGNUM					    \
	    && (REGNO) <= LAST_ARG_REGNUM ? SIB_REGS			    \
	 : (REGNO) == STATIC_CHAIN_REGNUM ? SIB_REGS			    \
	 : (REGNO) == LOOPCNTR_REGNUM ? LOOPCNTR_REGS			    \
	 : (REGNO) <= LAST_NONIO_GP_REGNUM ? GP_REGS			    \
	 : ALL_REGS)

#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Arbitrarily set to a non-argument register.  Not defined by TI ABI.  */
#define STATIC_CHAIN_REGNUM      0	/* r0 */

/* Tests for various kinds of constants used in the PRU port.  */
#define SHIFT_INT(X) (IN_RANGE ((X), 0, 31))

#define UHWORD_INT(X) (IN_RANGE ((X), 0, 0xffff))
#define SHWORD_INT(X) (IN_RANGE ((X), -32768, 32767))
#define UBYTE_INT(X) (IN_RANGE ((X), 0, 0xff))
#define SBYTE_INT(X) (IN_RANGE ((X), -128, 127))

/* Say that the epilogue uses the return address register.  Note that
   in the case of sibcalls, the values "used by the epilogue" are
   considered live at the start of the called function.  */
#define EPILOGUE_USES(REGNO) (epilogue_completed		\
			      && (((REGNO) == RA_REGNUM)		\
				  || (REGNO) == (RA_REGNUM + 1)))

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Trampolines are not supported, but put a define to keep the build.  */
#define TRAMPOLINE_SIZE 4

/* Stack layout.  */
#define STACK_GROWS_DOWNWARD  1
#undef FRAME_GROWS_DOWNWARD
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* Before the prologue, RA lives in r3.w2.  */
#define INCOMING_RETURN_ADDR_RTX	gen_rtx_REG (HImode, RA_REGNUM)

#define RETURN_ADDR_RTX(C,F) pru_get_return_address (C)

#define DWARF_FRAME_RETURN_COLUMN RA_REGNUM

/* The CFA includes the pretend args.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL) \
  (gcc_assert ((FNDECL) == current_function_decl), \
   FIRST_PARM_OFFSET (FNDECL) + crtl->args.pretend_args_size)

/* Frame/arg pointer elimination settings.  */
#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = pru_initial_elimination_offset ((FROM), (TO))

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
  pru_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Calling convention definitions.  */
#if !defined(IN_LIBGCC2)

#define NUM_ARG_REGS (LAST_ARG_REGNUM - FIRST_ARG_REGNUM + 1)

typedef struct pru_args
{
  bool regs_used[NUM_ARG_REGS];
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS)  \
  do {									  \
      memset ((CUM).regs_used, 0, sizeof ((CUM).regs_used));		  \
  } while (0)

#define FUNCTION_ARG_REGNO_P(REGNO) \
  ((REGNO) >= FIRST_ARG_REGNUM && (REGNO) <= LAST_ARG_REGNUM)

/* Passing function arguments on stack.  */
#define PUSH_ARGS 0
#define ACCUMULATE_OUTGOING_ARGS 1

/* We define TARGET_RETURN_IN_MEMORY, so set to zero.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Profiling.  */
#define PROFILE_BEFORE_PROLOGUE
#define NO_PROFILE_COUNTERS 1
#define FUNCTION_PROFILER(FILE, LABELNO) \
  pru_function_profiler ((FILE), (LABELNO))

#endif	/* IN_LIBGCC2 */

/* Addressing modes.  */

#define CONSTANT_ADDRESS_P(X) \
  (CONSTANT_P (X) && memory_address_p (SImode, X))

#define MAX_REGS_PER_ADDRESS 2
#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS ALL_REGS

#define REGNO_OK_FOR_BASE_P(REGNO) pru_regno_ok_for_base_p ((REGNO), true)
#define REGNO_OK_FOR_INDEX_P(REGNO) pru_regno_ok_for_index_p ((REGNO), true)

/* Limited by the insns in pru-ldst-multiple.md.  */
#define MOVE_MAX 8
#define SLOW_BYTE_ACCESS 1

/* It is as good to call a constant function address as to call an address
   kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Define output assembler language.  */

#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"

#define ASM_COMMENT_START "# "

#define GLOBAL_ASM_OP "\t.global\t"

#define PRU_NAME_R(X)  X".b0", X".b1", X".b2", X".b3"
#define REGISTER_NAMES		  \
  {				  \
    PRU_NAME_R ("r0"),		  \
    PRU_NAME_R ("r1"),		  \
    PRU_NAME_R ("r2"),		  \
    PRU_NAME_R ("r3"),		  \
    PRU_NAME_R ("r4"),		  \
    PRU_NAME_R ("r5"),		  \
    PRU_NAME_R ("r6"),		  \
    PRU_NAME_R ("r7"),		  \
    PRU_NAME_R ("r8"),		  \
    PRU_NAME_R ("r9"),		  \
    PRU_NAME_R ("r10"),		  \
    PRU_NAME_R ("r11"),		  \
    PRU_NAME_R ("r12"),		  \
    PRU_NAME_R ("r13"),		  \
    PRU_NAME_R ("r14"),		  \
    PRU_NAME_R ("r15"),		  \
    PRU_NAME_R ("r16"),		  \
    PRU_NAME_R ("r17"),		  \
    PRU_NAME_R ("r18"),		  \
    PRU_NAME_R ("r19"),		  \
    PRU_NAME_R ("r20"),		  \
    PRU_NAME_R ("r21"),		  \
    PRU_NAME_R ("r22"),		  \
    PRU_NAME_R ("r23"),		  \
    PRU_NAME_R ("r24"),		  \
    PRU_NAME_R ("r25"),		  \
    PRU_NAME_R ("r26"),		  \
    PRU_NAME_R ("r27"),		  \
    PRU_NAME_R ("r28"),		  \
    PRU_NAME_R ("r29"),		  \
    PRU_NAME_R ("r30"),		  \
    PRU_NAME_R ("r31"),		  \
    PRU_NAME_R ("loopcntr_reg"),  \
    PRU_NAME_R ("pc"),		  \
    PRU_NAME_R ("fake_fp"),	  \
    PRU_NAME_R ("fake_ap"),	  \
}

#define PRU_OVERLAP_R(X)	      \
  { "r" #X	, X * 4	    ,  4 },   \
  { "r" #X ".w0", X * 4 + 0 ,  2 },   \
  { "r" #X ".w1", X * 4 + 1 ,  2 },   \
  { "r" #X ".w2", X * 4 + 2 ,  2 }

#define OVERLAPPING_REGISTER_NAMES  \
  {				    \
    /* Aliases.  */		    \
    { "sp", 2 * 4, 4 },		    \
    { "ra", 3 * 4, 2 },		    \
    { "fp", 4 * 4, 4 },		    \
    PRU_OVERLAP_R (0),		    \
    PRU_OVERLAP_R (1),		    \
    PRU_OVERLAP_R (2),		    \
    PRU_OVERLAP_R (3),		    \
    PRU_OVERLAP_R (4),		    \
    PRU_OVERLAP_R (5),		    \
    PRU_OVERLAP_R (6),		    \
    PRU_OVERLAP_R (7),		    \
    PRU_OVERLAP_R (8),		    \
    PRU_OVERLAP_R (9),		    \
    PRU_OVERLAP_R (10),		    \
    PRU_OVERLAP_R (11),		    \
    PRU_OVERLAP_R (12),		    \
    PRU_OVERLAP_R (13),		    \
    PRU_OVERLAP_R (14),		    \
    PRU_OVERLAP_R (15),		    \
    PRU_OVERLAP_R (16),		    \
    PRU_OVERLAP_R (17),		    \
    PRU_OVERLAP_R (18),		    \
    PRU_OVERLAP_R (19),		    \
    PRU_OVERLAP_R (20),		    \
    PRU_OVERLAP_R (21),		    \
    PRU_OVERLAP_R (22),		    \
    PRU_OVERLAP_R (23),		    \
    PRU_OVERLAP_R (24),		    \
    PRU_OVERLAP_R (25),		    \
    PRU_OVERLAP_R (26),		    \
    PRU_OVERLAP_R (27),		    \
    PRU_OVERLAP_R (28),		    \
    PRU_OVERLAP_R (29),		    \
    PRU_OVERLAP_R (30),		    \
    PRU_OVERLAP_R (31),		    \
}

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)				    \
  do									    \
    {									    \
      fputs (integer_asm_op (POINTER_SIZE / BITS_PER_UNIT, TRUE), FILE);    \
      fprintf (FILE, "%%pmem(.L%u)\n", (unsigned) (VALUE));		    \
    }									    \
  while (0)

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		    \
  do									    \
    {									    \
      fputs (integer_asm_op (POINTER_SIZE / BITS_PER_UNIT, TRUE), STREAM);  \
      fprintf (STREAM, "%%pmem(.L%u-.L%u)\n", (unsigned) (VALUE),	    \
	       (unsigned) (REL));					    \
    }									    \
  while (0)

/* Section directives.  */

/* Output before read-only data.  */
#define TEXT_SECTION_ASM_OP "\t.section\t.text"

/* Output before writable data.  */
#define DATA_SECTION_ASM_OP "\t.section\t.data"

/* Output before uninitialized data.  */
#define BSS_SECTION_ASM_OP "\t.section\t.bss"

#define CTORS_SECTION_ASM_OP "\t.section\t.init_array,\"aw\",%init_array"
#define DTORS_SECTION_ASM_OP "\t.section\t.fini_array,\"aw\",%fini_array"

#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#define INIT_ARRAY_SECTION_ASM_OP CTORS_SECTION_ASM_OP
#define FINI_ARRAY_SECTION_ASM_OP DTORS_SECTION_ASM_OP

/* Since we use .init_array/.fini_array we don't need the markers at
   the start and end of the ctors/dtors arrays.  */
#define CTOR_LIST_BEGIN asm (CTORS_SECTION_ASM_OP)
#define CTOR_LIST_END		/* empty */
#define DTOR_LIST_BEGIN asm (DTORS_SECTION_ASM_OP)
#define DTOR_LIST_END		/* empty */

#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR pru_elf_asm_constructor

#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR pru_elf_asm_destructor

#define ASM_OUTPUT_ALIGN(FILE, LOG)		      \
  do {						      \
    fprintf ((FILE), "%s%d\n", ALIGN_ASM_OP, (LOG));  \
  } while (0)

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do									\
  {									\
    fprintf ((FILE), "%s", COMMON_ASM_OP);				\
    assemble_name ((FILE), (NAME));					\
    fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED ",%u\n", (SIZE),	\
	     (ALIGN) / BITS_PER_UNIT);					\
  }									\
while (0)


/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#undef  ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  switch_to_section (bss_section);					\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
  if (!flag_inhibit_size_directive)					\
    ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, SIZE);			\
  ASM_OUTPUT_ALIGN ((FILE), exact_log2 ((ALIGN) / BITS_PER_UNIT));      \
  ASM_OUTPUT_LABEL (FILE, NAME);					\
  ASM_OUTPUT_SKIP ((FILE), (SIZE) ? (SIZE) : 1);			\
} while (0)

/* Misc parameters.  */

#define TARGET_SUPPORTS_WIDE_INT 1

#define STORE_FLAG_VALUE 1
#define Pmode SImode
#define FUNCTION_MODE Pmode

#define CASE_VECTOR_MODE Pmode

/* Jumps are cheap on PRU.  */
#define LOGICAL_OP_NON_SHORT_CIRCUIT		0

/* Unfortunately the LBBO instruction does not zero-extend data.  */
#undef LOAD_EXTEND_OP

#undef WORD_REGISTER_OPERATIONS

#define HAS_LONG_UNCOND_BRANCH			1
#define HAS_LONG_COND_BRANCH			1

#define REGISTER_TARGET_PRAGMAS() pru_register_pragmas ()

#endif /* GCC_PRU_H */
