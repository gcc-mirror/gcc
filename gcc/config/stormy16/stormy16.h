/* Xstormy16 cpu description.
   Copyright (C) 1997-2013 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* Driver configuration.  */

#undef  ASM_SPEC
#define ASM_SPEC ""

#undef  LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
		   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"

/* For xstormy16:
   - If -msim is specified, everything is built and linked as for the sim.
   - If -T is specified, that linker script is used, and it should provide
     appropriate libraries.
   - If neither is specified, everything is built as for the sim, but no
     I/O support is assumed.  */
#undef  LIB_SPEC
#define LIB_SPEC "-( -lc %{msim:-lsim}%{!msim:%{!T*:-lnosys}} -)"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s crti.o%s crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"


/* Run-time target specifications.  */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("xstormy16");		\
      builtin_assert ("machine=xstormy16");	\
      builtin_assert ("cpu=xstormy16");		\
    }						\
  while (0)

/* Storage Layout.  */

#define BITS_BIG_ENDIAN 1

#define BYTES_BIG_ENDIAN 0

#define WORDS_BIG_ENDIAN 0

#define UNITS_PER_WORD 2

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)	\
  do						\
    {						\
      if (GET_MODE_CLASS (MODE) == MODE_INT	\
	  && GET_MODE_SIZE (MODE) < 2)		\
	(MODE) = HImode;			\
    }						\
  while (0)

#define PARM_BOUNDARY 16

#define STACK_BOUNDARY 16

#define FUNCTION_BOUNDARY 16

#define BIGGEST_ALIGNMENT 16

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define STRICT_ALIGNMENT 1

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Layout of Source Language Data Types.  */

#define INT_TYPE_SIZE 16

#define SHORT_TYPE_SIZE 16

#define LONG_TYPE_SIZE 32

#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32

#define DOUBLE_TYPE_SIZE 64

#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 0

#define SIZE_TYPE "unsigned int"

#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* Register Basics.  */

#define FIRST_PSEUDO_REGISTER 19

#define FIXED_REGISTERS \
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1 }

#define CALL_USED_REGISTERS \
  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1 }


/* Order of allocation of registers.  */

#define REG_ALLOC_ORDER { 7, 6, 5, 4, 3, 2, 1, 0, 9, 8, 10, 11, 12, 13, 14, 15, 16 }


/* How Values Fit in Registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE) 				\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(REGNO, MODE) ((REGNO) != 16 || (MODE) == BImode)

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `HARD_REGNO_MODE_OK (R, MODE1)' and `HARD_REGNO_MODE_OK (R, MODE2)' are
   ever different for any R, then `MODES_TIEABLE_P (MODE1, MODE2)' must be
   zero.  */
#define MODES_TIEABLE_P(MODE1, MODE2) ((MODE1) != BImode && (MODE2) != BImode)


/* Register Classes.  */

enum reg_class
{
  NO_REGS,
  R0_REGS,
  R1_REGS,
  TWO_REGS,
  R2_REGS,
  EIGHT_REGS,
  R8_REGS,
  ICALL_REGS,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

#define REG_CLASS_NAMES				\
{						\
  "NO_REGS", 					\
  "R0_REGS", 					\
  "R1_REGS",					\
  "TWO_REGS",					\
  "R2_REGS",					\
  "EIGHT_REGS",					\
  "R8_REGS",					\
  "ICALL_REGS",					\
  "GENERAL_REGS",				\
  "ALL_REGS"					\
}

#define REG_CLASS_CONTENTS			\
{						\
  { 0x00000 },					\
  { 0x00001 },					\
  { 0x00002 },					\
  { 0x00003 },					\
  { 0x00004 },					\
  { 0x000FF },					\
  { 0x00100 },					\
  { 0x00300 },					\
  { 0x6FFFF },					\
  { (1 << FIRST_PSEUDO_REGISTER) - 1 }		\
}

#define REGNO_REG_CLASS(REGNO) 			\
  (  (REGNO) ==  0 ? R0_REGS			\
   : (REGNO) ==  1 ? R1_REGS			\
   : (REGNO) ==  2 ? R2_REGS			\
   : (REGNO) <   8 ? EIGHT_REGS			\
   : (REGNO) ==  8 ? R8_REGS			\
   : (REGNO) <= 18 ? GENERAL_REGS		\
   : ALL_REGS)

#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS GENERAL_REGS

#define REGNO_OK_FOR_BASE_P(NUM) 1

#define REGNO_OK_FOR_INDEX_P(NUM) REGNO_OK_FOR_BASE_P (NUM)

/* This chip has the interesting property that only the first eight
   registers can be moved to/from memory.  */
#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X)			\
  xstormy16_secondary_reload_class (CLASS, MODE, X)


/* Basic Stack Layout.  */

/* We want to use post-increment instructions to push things on the stack,
   because we don't have any pre-increment ones.  */
#define STACK_PUSH_CODE POST_INC

#define FRAME_GROWS_DOWNWARD 0

#define ARGS_GROW_DOWNWARD 1

#define STARTING_FRAME_OFFSET 0

#define FIRST_PARM_OFFSET(FUNDECL) 0

#define RETURN_ADDR_RTX(COUNT, FRAMEADDR)	\
  ((COUNT) == 0					\
   ? gen_rtx_MEM (Pmode, arg_pointer_rtx)	\
   : NULL_RTX)

#define INCOMING_RETURN_ADDR_RTX  \
   gen_rtx_MEM (SImode, gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (-4)))

#define INCOMING_FRAME_SP_OFFSET (xstormy16_interrupt_function_p () ? -6 : -4)


/* Register That Address the Stack Frame.  */

#define STATIC_CHAIN_REGNUM	 1
#define HARD_FRAME_POINTER_REGNUM 13
#define STACK_POINTER_REGNUM	15
#define CARRY_REGNUM		16
#define FRAME_POINTER_REGNUM	17
#define ARG_POINTER_REGNUM	18


/* Eliminating the Frame Pointer and the Arg Pointer.  */

#define ELIMINABLE_REGS					\
{							\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
  {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},		\
  {ARG_POINTER_REGNUM,	 HARD_FRAME_POINTER_REGNUM},	\
}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = xstormy16_initial_elimination_offset (FROM, TO)


/* Passing Function Arguments on the Stack.  */

#define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)


/* Function Arguments in Registers.  */

#define NUM_ARGUMENT_REGISTERS  6
#define FIRST_ARGUMENT_REGISTER 2

#define XSTORMY16_WORD_SIZE(TYPE, MODE)				\
  ((((TYPE) ? int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE))	\
    + 1) 							\
   / 2)

/* For this platform, the value of CUMULATIVE_ARGS is the number of words
   of arguments that have been passed in registers so far.  */
#define CUMULATIVE_ARGS int

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  (CUM) = 0

#define FUNCTION_ARG_REGNO_P(REGNO)					\
  ((REGNO) >= FIRST_ARGUMENT_REGISTER 					\
   && (REGNO) < FIRST_ARGUMENT_REGISTER + NUM_ARGUMENT_REGISTERS)


/* How Scalar Function Values are Returned.  */

/* The number of the hard register that is used to return a scalar value from a
   function call.  */
#define RETURN_VALUE_REGNUM	FIRST_ARGUMENT_REGISTER


/* Function Entry and Exit.  */

#define EPILOGUE_USES(REGNO) \
  xstormy16_epilogue_uses (REGNO)


/* Generating Code for Profiling.  */

/* This declaration must be present, but it can be an abort if profiling is
   not implemented.  */
     
#define FUNCTION_PROFILER(FILE, LABELNO) xstormy16_function_profiler ()


/* Trampolines for Nested Functions.  */

#define TRAMPOLINE_SIZE 8
#define TRAMPOLINE_ALIGNMENT 16


/* Addressing Modes.  */

#define HAVE_POST_INCREMENT 1

#define HAVE_PRE_DECREMENT 1

#define MAX_REGS_PER_ADDRESS 1


/* Describing Relative Costs of Operations.  */

#define BRANCH_COST(speed_p, predictable_p) 5

#define SLOW_BYTE_ACCESS 0

#define NO_FUNCTION_CSE


/* Dividing the output into sections.  */

#define TEXT_SECTION_ASM_OP ".text"

#define DATA_SECTION_ASM_OP ".data"

#define BSS_SECTION_ASM_OP "\t.section\t.bss"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.
   There are no shared libraries on this target so these sections need
   not be writable.

   Defined in elfos.h.  */

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"a\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"a\""

#define TARGET_ASM_INIT_SECTIONS xstormy16_asm_init_sections

#define JUMP_TABLES_IN_TEXT_SECTION 1

/* The Overall Framework of an Assembler File.  */

#define ASM_COMMENT_START ";"

#define ASM_APP_ON "#APP\n"

#define ASM_APP_OFF "#NO_APP\n"

/* Output of Data.  */

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) ((C) == '|')

#define ASM_OUTPUT_ALIGNED_DECL_COMMON(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
  xstormy16_asm_output_aligned_common (STREAM, DECL, NAME, SIZE, ALIGNMENT, 1)
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
  xstormy16_asm_output_aligned_common (STREAM, DECL, NAME, SIZE, ALIGNMENT, 0)


/* Output and Generation of Labels.  */
#define SYMBOL_FLAG_XSTORMY16_BELOW100	(SYMBOL_FLAG_MACH_DEP << 0)

#define ASM_OUTPUT_SYMBOL_REF(STREAM, SYMBOL)	\
  do						\
    {						\
      const char *rn = XSTR (SYMBOL, 0);	\
						\
      if (SYMBOL_REF_FUNCTION_P (SYMBOL))	\
	ASM_OUTPUT_LABEL_REF ((STREAM), rn);	\
      else					\
	assemble_name (STREAM, rn);		\
    }						\
  while (0)

#define ASM_OUTPUT_LABEL_REF(STREAM, NAME)	\
  do						\
    {						\
      fputs ("@fptr(", STREAM);			\
      assemble_name (STREAM, NAME);		\
      fputc (')', STREAM);			\
    }						\
  while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "


/* Output of Assembler Instructions.  */

#define REGISTER_NAMES							\
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10",	\
  "r11", "r12", "r13", "psw", "sp", "carry", "fp", "ap" }

#define ADDITIONAL_REGISTER_NAMES		\
  { { "r14", 14 },				\
    { "r15", 15 } }

#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX ""
#define IMMEDIATE_PREFIX "#"

#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO) \
  fprintf (STREAM, "\tpush %d\n", REGNO)

#define ASM_OUTPUT_REG_POP(STREAM, REGNO) \
  fprintf (STREAM, "\tpop %d\n", REGNO)


/* Output of dispatch tables.  */

/* This port does not use the ASM_OUTPUT_ADDR_VEC_ELT macro, because
   this could cause label alignment to appear between the 'br' and the table,
   which would be bad.  Instead, it controls the output of the table
   itself.  */
#define ASM_OUTPUT_ADDR_VEC(LABEL, BODY) \
  xstormy16_output_addr_vec (file, LABEL, BODY)

/* Alignment for ADDR_VECs is the same as for code.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 1


/* Assembler Commands for Exception Regions.  */

#define DWARF2_UNWIND_INFO 		0
#define DWARF_CIE_DATA_ALIGNMENT	1

/* Assembler Commands for Alignment.  */

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))


/* Macros Affecting all Debug Formats.  */

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG


/* Macros for SDB and Dwarf Output.  */

/* Define this macro if addresses in Dwarf 2 debugging info should not
   be the same size as pointers on the target architecture.  The
   macro's value should be the size, in bytes, to use for addresses in
   the debugging info.

   Some architectures use word addresses to refer to code locations,
   but Dwarf 2 info always uses byte addresses.  On such machines,
   Dwarf 2 addresses need to be larger than the architecture's
   pointers.  */
#define DWARF2_ADDR_SIZE 4


/* Miscellaneous Parameters.  */

#define CASE_VECTOR_MODE SImode

#define WORD_REGISTER_OPERATIONS

#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

#define MOVE_MAX 2

#define SHIFT_COUNT_TRUNCATED 1

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define Pmode HImode

#define FUNCTION_MODE HImode

#define NO_IMPLICIT_EXTERN_C
