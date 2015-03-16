/* Target Definitions for NVPTX.
   Copyright (C) 2014-2015 Free Software Foundation, Inc.
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

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

#ifndef GCC_NVPTX_H
#define GCC_NVPTX_H

/* Run-time Target.  */

#define STARTFILE_SPEC "%{mmainkernel:crt0.o}"

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_assert ("machine=nvptx");		\
      builtin_assert ("cpu=nvptx");		\
      builtin_define ("__nvptx__");		\
    } while (0)

/* Avoid the default in ../../gcc.c, which adds "-pthread", which is not
   supported for nvptx.  */
#define GOMP_SELF_SPECS ""

/* Storage Layout.  */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

/* Chosen such that we won't have to deal with multi-word subregs.  */
#define UNITS_PER_WORD 8

#define PARM_BOUNDARY 8
#define STACK_BOUNDARY 64
#define FUNCTION_BOUNDARY 32
#define BIGGEST_ALIGNMENT 64
#define STRICT_ALIGNMENT 1

/* Copied from elf.h and other places.  We'd otherwise use
   BIGGEST_ALIGNMENT and fail a number of testcases.  */
#define MAX_OFILE_ALIGNMENT (32768 * 8)

/* Type Layout.  */

#define DEFAULT_SIGNED_CHAR 1

#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE (TARGET_ABI64 ? 64 : 32)
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_ABI64 ? "long unsigned int" : "unsigned int")
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_ABI64 ? "long int" : "int")

#define POINTER_SIZE (TARGET_ABI64 ? 64 : 32)

#define Pmode (TARGET_ABI64 ? DImode : SImode)

/* Registers.  Since ptx is a virtual target, we just define a few
   hard registers for special purposes and leave pseudos unallocated.  */

#define FIRST_PSEUDO_REGISTER 16
#define FIXED_REGISTERS					\
  { 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1 }
#define CALL_USED_REGISTERS				\
  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }

#define HARD_REGNO_NREGS(regno, mode)	1
#define CANNOT_CHANGE_MODE_CLASS(M1, M2, CLS) ((CLS) == RETURN_REG)
#define HARD_REGNO_MODE_OK(REG, MODE) nvptx_hard_regno_mode_ok (REG, MODE)

/* Register Classes.  */

enum reg_class
  {
    NO_REGS,
    RETURN_REG,
    ALL_REGS,
    LIM_REG_CLASSES
  };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES {	  \
    "RETURN_REG",		  \
    "NO_REGS",			  \
    "ALL_REGS" }

#define REG_CLASS_CONTENTS	\
{				\
  /* NO_REGS.  */		\
  { 0x0000 },			\
  /* RETURN_REG.  */		\
  { 0x0008 },			\
  /* ALL_REGS.  */		\
  { 0xFFFF },			\
}

#define GENERAL_REGS ALL_REGS

#define REGNO_REG_CLASS(R) ((R) == 4 ? RETURN_REG : ALL_REGS)

#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_BASE_P(X) true
#define REGNO_OK_FOR_INDEX_P(X) false

#define CLASS_MAX_NREGS(class, mode) \
  ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define MODES_TIEABLE_P(M1, M2) false

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)		\
  if (GET_MODE_CLASS (MODE) == MODE_INT			\
      && GET_MODE_SIZE (MODE) < GET_MODE_SIZE (SImode))	\
    {							\
      (MODE) = SImode;					\
    }

/* Address spaces.  */
#define ADDR_SPACE_GLOBAL 1
#define ADDR_SPACE_SHARED 3
#define ADDR_SPACE_CONST 4
#define ADDR_SPACE_LOCAL 5
#define ADDR_SPACE_PARAM 101

/* Stack and Calling.  */

#define STARTING_FRAME_OFFSET 0
#define FRAME_GROWS_DOWNWARD 0
#define STACK_GROWS_DOWNWARD

#define STACK_POINTER_REGNUM 1
#define HARD_FRAME_POINTER_REGNUM 2
#define NVPTX_PUNNING_BUFFER_REGNUM 3
#define NVPTX_RETURN_REGNUM 4
#define FRAME_POINTER_REGNUM 15
#define ARG_POINTER_REGNUM 14
#define RETURN_ADDR_REGNO 13

#define STATIC_CHAIN_REGNUM 12
#define OUTGOING_ARG_POINTER_REGNUM 11
#define OUTGOING_STATIC_CHAIN_REGNUM 10

#define FIRST_PARM_OFFSET(FNDECL) 0
#define PUSH_ARGS_REVERSED 1

#define ACCUMULATE_OUTGOING_ARGS 1

#ifdef HOST_WIDE_INT
struct nvptx_args {
  union tree_node *fntype;
  /* Number of arguments passed in registers so far.  */
  int count;
  /* Offset into the stdarg area so far.  */
  HOST_WIDE_INT off;
};
#endif

#define CUMULATIVE_ARGS struct nvptx_args

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  do { (CUM).fntype = (FNTYPE); (CUM).count = 0; (CUM).off = 0; } while (0)

#define FUNCTION_ARG_REGNO_P(r) 0

#define DEFAULT_PCC_STRUCT_RETURN 0

#define FUNCTION_PROFILER(file, labelno) \
  fatal_error (input_location, \
	       "profiling is not yet implemented for this architecture")

#define TRAMPOLINE_SIZE 32
#define TRAMPOLINE_ALIGNMENT 256

/* We don't run reload, so this isn't actually used, but it still needs to be
   defined.  Showing an argp->fp elimination also stops
   expand_builtin_setjmp_receiver from generating invalid insns.  */
#define ELIMINABLE_REGS					\
  {							\
    { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
    { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}	\
  }

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = 0)

/* Addressing Modes.  */

#define MAX_REGS_PER_ADDRESS 1

#define LEGITIMATE_PIC_OPERAND_P(X) 1


struct nvptx_pseudo_info
{
  int true_size;
  int renumber;
};

#if defined HOST_WIDE_INT
struct GTY(()) machine_function
{
  rtx_expr_list *call_args;
  rtx start_call;
  tree funtype;
  bool has_call_with_varargs;
  bool has_call_with_sc;
  struct GTY((skip)) nvptx_pseudo_info *pseudos;
  HOST_WIDE_INT outgoing_stdarg_size;
  int ret_reg_mode;
  int punning_buffer_size;
};
#endif

/* Costs.  */

#define NO_FUNCTION_CSE 1
#define SLOW_BYTE_ACCESS 0
#define BRANCH_COST(speed_p, predictable_p) 6

/* Assembler Format.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
  nvptx_declare_function_name (FILE, NAME, DECL)

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL) \
  nvptx_function_end (STREAM)

#define DWARF2_ASM_LINE_DEBUG_INFO 1

#undef ASM_APP_ON
#define ASM_APP_ON "\t// #APP \n"
#undef ASM_APP_OFF
#define ASM_APP_OFF "\t// #NO_APP \n"

#define ASM_OUTPUT_COMMON(stream, name, size, rounded)
#define ASM_OUTPUT_LOCAL(stream, name, size, rounded)

#define REGISTER_NAMES							\
  {									\
    "%hr0", "%outargs", "%hfp", "%punbuffer", "%retval", "%retval_in", "%hr6", "%hr7",	\
    "%hr8", "%hr9", "%hr10", "%hr11", "%hr12", "%hr13", "%argp", "%frame" \
  }

#define DBX_REGISTER_NUMBER(N) N

#define TEXT_SECTION_ASM_OP ""
#define DATA_SECTION_ASM_OP ""

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
  do								\
    {								\
      char *__p;						\
      __p = stpcpy (&(LABEL)[1], PREFIX);			\
      (LABEL)[0] = '$';						\
      sprint_ul (__p, (unsigned long) (NUM));			\
    }								\
  while (0)

#define ASM_OUTPUT_ALIGN(FILE, POWER)		\
  do						\
    {						\
      (void) (FILE);				\
      (void) (POWER);				\
    }						\
  while (0)

#define ASM_OUTPUT_SKIP(FILE, N)		\
  nvptx_output_skip (FILE, N)

#undef  ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)			\
  nvptx_output_ascii (FILE, STR, LENGTH);

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)	\
  nvptx_declare_object_name (FILE, NAME, DECL)

#undef  ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      fprintf (FILE, "// BEGIN%s VAR DEF: ",				\
	       TREE_PUBLIC (DECL) ? " GLOBAL" : "");			\
      assemble_name_raw (FILE, NAME);					\
      fputc ('\n', FILE);						\
      const char *sec = nvptx_section_for_decl (DECL);			\
      fprintf (FILE, ".visible%s.align %d .b8 ", sec,			\
	       (ALIGN) / BITS_PER_UNIT);				\
      assemble_name ((FILE), (NAME));					\
      if ((SIZE) > 0)							\
	fprintf (FILE, "["HOST_WIDE_INT_PRINT_DEC"]", (SIZE));		\
      fprintf (FILE, ";\n");						\
    }									\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      fprintf (FILE, "// BEGIN VAR DEF: ");				\
      assemble_name_raw (FILE, NAME);					\
      fputc ('\n', FILE);						\
      const char *sec = nvptx_section_for_decl (DECL);			\
      fprintf (FILE, ".visible%s.align %d .b8 ", sec,			\
	       (ALIGN) / BITS_PER_UNIT);				\
      assemble_name ((FILE), (NAME));					\
      if ((SIZE) > 0)							\
	fprintf (FILE, "["HOST_WIDE_INT_PRINT_DEC"]", (SIZE));		\
      fprintf (FILE, ";\n");						\
    }									\
  while (0)

#define CASE_VECTOR_PC_RELATIVE flag_pic
#define JUMP_TABLES_IN_TEXT_SECTION flag_pic

#define ADDR_VEC_ALIGN(VEC) (JUMP_TABLES_IN_TEXT_SECTION ? 5 : 2)

/* Misc.  */

#define DWARF2_DEBUGGING_INFO 1

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_BITSIZE ((MODE)), 2)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_BITSIZE ((MODE)), 2)

#define NO_DOT_IN_LABEL
#define ASM_COMMENT_START "//"

#define STORE_FLAG_VALUE -1
#define FLOAT_STORE_FLAG_VALUE(MODE) REAL_VALUE_ATOF("1.0", (MODE))

#define CASE_VECTOR_MODE SImode
#define MOVE_MAX 4
#define MOVE_RATIO(SPEED) 4
#define TRULY_NOOP_TRUNCATION(outprec, inprec) 1
#define FUNCTION_MODE QImode
#define HAS_INIT_SECTION 1

/* The C++ front end insists to link against libstdc++ -- which we don't build.
   Tell it to instead link against the innocuous libgcc.  */
#define LIBSTDCXX "gcc"

#endif /* GCC_NVPTX_H */
