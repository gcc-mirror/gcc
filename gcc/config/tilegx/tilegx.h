/* Definitions of target machine for GNU compiler for TILE-Gx.
   Copyright (C) 2011, 2012
   Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

/* This is used by tilegx_cpu_cpp_builtins to indicate the byte order
   we're compiling for.  */
#define TILEGX_CPU_CPP_ENDIAN_BUILTINS()	\
  do						\
    {						\
      if (TARGET_BIG_ENDIAN)			\
	builtin_define ("__BIG_ENDIAN__");	\
      else					\
	builtin_define ("__LITTLE_ENDIAN__");	\
    }						\
  while (0)


/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS() \
  tilegx_cpu_cpp_builtins (pfile)

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_32BIT ? "int" : "long int")

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_32BIT ? "unsigned int" : "long unsigned int")

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* Target machine storage layout */

#define TARGET_BIG_ENDIAN 0
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN TARGET_BIG_ENDIAN
#define WORDS_BIG_ENDIAN TARGET_BIG_ENDIAN

#define UNITS_PER_WORD 8
#define PARM_BOUNDARY BITS_PER_WORD
#define STACK_BOUNDARY 64
#define FUNCTION_BOUNDARY 64
#define BIGGEST_ALIGNMENT 64
#define STRICT_ALIGNMENT 1

#define INT_TYPE_SIZE         32
#define LONG_TYPE_SIZE        (TARGET_32BIT ? 32 : 64)
#define LONG_LONG_TYPE_SIZE   64
#define FLOAT_TYPE_SIZE       32
#define DOUBLE_TYPE_SIZE      64
#define LONG_DOUBLE_TYPE_SIZE 64
#define POINTER_SIZE          LONG_TYPE_SIZE

#define PCC_BITFIELD_TYPE_MATTERS 1
#define FASTEST_ALIGNMENT 64
#define BIGGEST_FIELD_ALIGNMENT 64
#define WIDEST_HARDWARE_FP_SIZE 64

/* Unaligned moves trap and are very slow.  */
#define SLOW_UNALIGNED_ACCESS(MODE, ALIGN) 1

/* Make strings word-aligned so strcpy from constants will be
   faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Make local arrays of chars word-aligned for the same reasons.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) DATA_ALIGNMENT (TYPE, ALIGN)


/* Standard register usage.  */

#define FIRST_PSEUDO_REGISTER (64 + 4)

#define FIXED_REGISTERS \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1}
#define CALL_USED_REGISTERS \
 {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1}

#define CALL_REALLY_USED_REGISTERS \
 CALL_USED_REGISTERS

#define REG_ALLOC_ORDER {				\
      10, 11, 12, 13, 14, /* call used */		\
      15, 16, 17, 18, 19,				\
      20, 21, 22, 23, 24,				\
      25, 26, 27, 28, 29,				\
							\
      9, 8, 7, 6, 5,      /* argument */		\
      4, 3, 2, 1, 0,					\
							\
      55,	          /* return address */		\
							\
      30, 31, 32, 33, 34, /* call saved registers */	\
      35, 36, 37, 38, 39,				\
      40, 41, 42, 43, 44,				\
      45, 46, 47, 48, 49,				\
      50, 51,						\
							\
      52, 		  /* hard frame pointer */	\
      53, 54, 		  /* tp, sp */			\
							\
      56, 57, 58, 59, 60, /* special purpose */		\
      61, 62, 63, 64, 65, /* or fake registers */	\
      66, 67						\
}

#define HARD_REGNO_NREGS(REGNO, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

#define MODES_TIEABLE_P(MODE1, MODE2)  1

/* Register that holds an address into the text segment that can be
   used by pic code.  */
#define TILEGX_PIC_TEXT_LABEL_REGNUM (flag_pic ? 50 : INVALID_REGNUM)
#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? 51 : INVALID_REGNUM)
#define HARD_FRAME_POINTER_REGNUM 52
#define THREAD_POINTER_REGNUM 53
#define STACK_POINTER_REGNUM 54
#define TILEGX_LINK_REGNUM 55
#define FRAME_POINTER_REGNUM 64
#define ARG_POINTER_REGNUM 65
/* SPR storing the comparison value for compare and exchange.  */
#define TILEGX_CMPEXCH_REGNUM 66
/* Pseudo registers used to enforce order between instructions that
   touch the networks.  */
#define TILEGX_NETORDER_REGNUM 67
#define STATIC_CHAIN_REGNUM 10


enum reg_class
{
  NO_REGS,
  R0_REGS,
  R1_REGS,
  R2_REGS,
  R3_REGS,
  R4_REGS,
  R5_REGS,
  R6_REGS,
  R7_REGS,
  R8_REGS,
  R9_REGS,
  R10_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS, don't give it a 
   different class number; just make it an alias.  */
#define GENERAL_REGS ALL_REGS

#define REG_CLASS_NAMES	\
  { \
    "NO_REGS", \
    "R0_REGS", \
    "R1_REGS", \
    "R2_REGS", \
    "R3_REGS", \
    "R4_REGS", \
    "R5_REGS", \
    "R6_REGS", \
    "R7_REGS", \
    "R8_REGS", \
    "R9_REGS", \
    "R10_REGS", \
    "ALL_REGS" \
  }

#define REG_CLASS_CONTENTS \
  { \
    { 0 }, \
    { 1 << 0 }, \
    { 1 << 1 }, \
    { 1 << 2 }, \
    { 1 << 3 }, \
    { 1 << 4 }, \
    { 1 << 5 }, \
    { 1 << 6 }, \
    { 1 << 7 }, \
    { 1 << 8 }, \
    { 1 << 9 }, \
    { 1 << 10 }, \
    { 0xffffffff, 0xffffffff } \
  }

#define REGNO_REG_CLASS(REGNO) \
  ((unsigned)(REGNO) <= 10 ? \
   (enum reg_class)(R0_REGS + (REGNO)) : ALL_REGS)

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS ALL_REGS

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)


/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD
#define FRAME_GROWS_DOWNWARD 1
#define STARTING_FRAME_OFFSET 0

#define DYNAMIC_CHAIN_ADDRESS(FRAME) \
  plus_constant (Pmode, (FRAME), UNITS_PER_WORD)

#define FIRST_PARM_OFFSET(FNDECL) 0

#define ACCUMULATE_OUTGOING_ARGS 1

#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1

#define INCOMING_FRAME_SP_OFFSET 0

#define STACK_POINTER_OFFSET (2 * UNITS_PER_WORD)

#define ARG_POINTER_CFA_OFFSET(FNDECL) (-STACK_POINTER_OFFSET)

#define DEFAULT_PCC_STRUCT_RETURN 0

/* The first 10 registers may hold return value.  */
#define TILEGX_NUM_RETURN_REGS 10

/* The first 10 registers hold function arguments.  */
#define TILEGX_NUM_ARG_REGS 10

#define FUNCTION_ARG_REGNO_P(N) ((N) < TILEGX_NUM_ARG_REGS)

/* The type used to store the number of words of arguments scanned so
   far during argument scanning.  This includes any space that is
   skipped.  */
#define CUMULATIVE_ARGS int

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM) = 0)


#define ELIMINABLE_REGS					\
  {{ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},		\
  {ARG_POINTER_REGNUM,	 HARD_FRAME_POINTER_REGNUM},	\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
  {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = tilegx_initial_elimination_offset((FROM),(TO)))

#define FUNCTION_PROFILER(FILE, LABELNO) \
  tilegx_function_profiler (FILE, LABELNO)

#define TRAMPOLINE_SIZE (TARGET_32BIT ? 48 : 56)
#define TRAMPOLINE_ALIGNMENT 64
#define TRAMPOLINE_SECTION text_section


/* Call frame debugging information.  */

#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, TILEGX_LINK_REGNUM)

#define RETURN_ADDR_RTX tilegx_return_addr

#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (TILEGX_LINK_REGNUM)

#define DWARF_ZERO_REG 63

#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N + 12) : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, 11)
#define EH_RETURN_HANDLER_RTX tilegx_eh_return_handler_rtx ()

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  tilegx_asm_preferred_eh_data_format ((CODE), (GLOBAL))


/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 1
#define HAVE_POST_MODIFY_DISP 1

#define REGNO_OK_FOR_INDEX_P(regno) 0
#define REGNO_OK_FOR_BASE_P(regno)	\
  ((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

#define MAX_REGS_PER_ADDRESS 1

#define CONSTANT_ADDRESS_P(X) 0

#define LEGITIMATE_PIC_OPERAND_P(X) tilegx_legitimate_pic_operand_p (X)


#define CASE_VECTOR_MODE Pmode
#define CASE_VECTOR_PC_RELATIVE 0
#define JUMP_TABLES_IN_TEXT_SECTION 0

#define DEFAULT_SIGNED_CHAR 1

#define MOVE_MAX UNITS_PER_WORD

/* Use a value of 11 for MOVE_RATIO and friends, because TILEPro
   returns structs as large as 10 words in registers.  Because of some
   some code generation inefficiency, we never get smaller code for
   turning that into a memcpy, so pick a value that guarantees this
   doesn't happen.  */
#define TILEGX_CALL_RATIO 11
#define MOVE_RATIO(speed) ((speed) ? 15 : TILEGX_CALL_RATIO)
#define CLEAR_RATIO(speed) ((speed) ? 15 : TILEGX_CALL_RATIO)
#define SET_RATIO(speed) ((speed) ? 15 : TILEGX_CALL_RATIO)

#define WORD_REGISTER_OPERATIONS

#define LOAD_EXTEND_OP(MODE) ((MODE) == SImode ? SIGN_EXTEND : ZERO_EXTEND)

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    {                                           \
      if ((MODE) == SImode)                     \
        (UNSIGNEDP) = 0;                        \
      (MODE) = DImode;                          \
    }

/* Define SLOW_BYTE_ACCESS to avoid making a QI or HI mode
   register.  */
#define SLOW_BYTE_ACCESS 1

#define SHIFT_COUNT_TRUNCATED 0

#define SHORT_IMMEDIATES_SIGN_EXTEND

/* We represent all SI values as sign-extended DI values in
   registers.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) \
  ((INPREC) <= 32 || (OUTPREC) > 32)

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 64, 1)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 64, 1)

#define Pmode (TARGET_32BIT ? SImode : DImode)

#define STACK_SIZE_MODE Pmode

#define STORE_FLAG_VALUE 1

#define FUNCTION_MODE DImode

#define NO_FUNCTION_CSE 1

#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  ((LENGTH) = tilegx_adjust_insn_length ((INSN), (LENGTH)))

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

#define BRANCH_COST(speed_p, predictable_p) ((predictable_p) ? 2 : 6)


/* Control the assembler format that we output.  */

#undef NO_DOLLAR_IN_LABEL

#define ASM_COMMENT_START "##"

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP "\t.section\t.rodata, \"a\""

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	"\t.section\t.bss, \"wa\""

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	"\t.section\t.init, \"ax\""

#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP	"\t.section\t.fini, \"ax\""

#define GLOBAL_ASM_OP ".global "

#define SUPPORTS_WEAK 1

#define USER_LABEL_PREFIX ""

#define REGISTER_PREFIX ""
#define REGISTER_NAMES                                                  \
  { "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7",       \
    "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "r15",      \
    "r16",  "r17",  "r18",  "r19",  "r20",  "r21",  "r22",  "r23",      \
    "r24",  "r25",  "r26",  "r27",  "r28",  "r29",  "r30",  "r31",      \
    "r32",  "r33",  "r34",  "r35",  "r36",  "r37",  "r38",  "r39",      \
    "r40",  "r41",  "r42",  "r43",  "r44",  "r45",  "r46",  "r47",      \
    "r48",  "r49",  "r50",  "r51",  "r52",  "tp",   "sp",   "lr",       \
    "?r56?","idn0", "idn1", "udn0", "udn1", "udn2", "udn3", "zero",     \
    "?FRAME?", "?ARG?", "?CMPEXCH?", "?NET?" }

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  tilegx_final_prescan_insn (insn)

#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
  (PTR = tilegx_asm_output_opcode (STREAM, PTR))

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)			\
  do								\
    {								\
      char label[256];						\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));	\
      fprintf (FILE, "\t%s ",					\
               integer_asm_op (GET_MODE_SIZE (Pmode), TRUE));	\
      assemble_name (FILE, label);				\
      fprintf (FILE, "\n");					\
    }								\
  while (0)

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)	\
  do								\
    {								\
      char label[256];						\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));	\
      fprintf (FILE, "\t%s ", 					\
               integer_asm_op (GET_MODE_SIZE (Pmode), TRUE));	\
      assemble_name (FILE, label);				\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", (REL));		\
      fprintf (FILE, "-");					\
      assemble_name (FILE, label);				\
      fprintf (FILE, "\n");					\
    }								\
  while (0)

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  do { if ((LOG) != 0) fprintf (FILE, "\t.align %d\n", 1 << (LOG)); } while (0)

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  ( fputs (".comm ", (FILE)),				\
    assemble_name ((FILE), (NAME)),			\
    fprintf ((FILE), ",%u\n", (unsigned int)(ROUNDED)))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  ( fputs (".lcomm ", (FILE)),				\
    assemble_name ((FILE), (NAME)),			\
    fprintf ((FILE), ",%u\n", (unsigned int)(ROUNDED)))



#define INIT_EXPANDERS tilegx_init_expanders ()

/* A C structure for machine-specific, per-function data.  This is
   added to the cfun structure.  */
typedef struct GTY(()) machine_function
{
  /* Symbol for the text label used for pic.  */
  rtx text_label_symbol;

  /* Register for the text label.  */
  rtx text_label_rtx;

  /* Register for the pic offset table.  */
  rtx got_rtx;

  /* The function calls tls_get_addr.  */
  int calls_tls_get_addr;
} machine_function;

#ifndef HAVE_AS_TLS
#define HAVE_AS_TLS 0
#endif
