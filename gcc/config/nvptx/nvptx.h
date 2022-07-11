/* Target Definitions for NVPTX.
   Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

#ifndef NVPTX_OPTS_H
#include "config/nvptx/nvptx-opts.h"
#endif

/* Run-time Target.  */

/* Use '--with-arch' for default '-misa'.  */
#define OPTION_DEFAULT_SPECS \
  { "arch", "%{!misa=*:-misa=%(VALUE)}" }, \

/* Assembler supports '-v' option; handle similar to
   '../../gcc.cc:asm_options', 'HAVE_GNU_AS'.  */
#define ASM_SPEC "%{v}"

#define STARTFILE_SPEC "%{mmainkernel:crt0.o%s}"

#define TARGET_CPU_CPP_BUILTINS() nvptx_cpu_cpp_builtins ()

/* Avoid the default in ../../gcc.cc, which adds "-pthread", which is not
   supported for nvptx.  */
#define GOMP_SELF_SPECS ""

/* Storage Layout.  */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

/* Chosen such that we won't have to deal with multi-word subregs.  */
#define UNITS_PER_WORD 8

/* Alignments in bits.  */
#define PARM_BOUNDARY 32
#define STACK_BOUNDARY 128
#define FUNCTION_BOUNDARY 32
#define BIGGEST_ALIGNMENT 128
#define STRICT_ALIGNMENT 1

#define MAX_STACK_ALIGNMENT (1024 * 8)

#define DATA_ALIGNMENT nvptx_data_alignment

/* Copied from elf.h and other places.  We'd otherwise use
   BIGGEST_ALIGNMENT and fail a number of testcases.  */
#define MAX_OFILE_ALIGNMENT (32768 * 8)

/* Type Layout.  */

#define DEFAULT_SIGNED_CHAR 1

#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define LONG_TYPE_SIZE (TARGET_ABI64 ? 64 : 32)
#define LONG_LONG_TYPE_SIZE 64
#define TARGET_SUPPORTS_WIDE_INT 1

#define MAX_FIXED_MODE_SIZE 128

#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_ABI64 ? "long unsigned int" : "unsigned int")
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_ABI64 ? "long int" : "int")

#define POINTER_SIZE (TARGET_ABI64 ? 64 : 32)
#define Pmode (TARGET_ABI64 ? DImode : SImode)
#define STACK_SIZE_MODE Pmode

/* We always have to maintain the '-msoft-stack' pointer, but the PTX "native"
   stack pointer is handled implicitly at function level.  */
#define STACK_SAVEAREA_MODE(LEVEL) \
  (TARGET_SOFT_STACK ? Pmode \
   : (LEVEL == SAVE_FUNCTION ? VOIDmode \
      : Pmode))

#include "nvptx-gen.h"

/* There are no 'TARGET_PTX_3_1' and smaller conditionals: our baseline is
   PTX ISA Version 3.1.  */
#define TARGET_PTX_4_1 (ptx_version_option >= PTX_VERSION_4_1)
#define TARGET_PTX_4_2 (ptx_version_option >= PTX_VERSION_4_2)
#define TARGET_PTX_6_0 (ptx_version_option >= PTX_VERSION_6_0)
#define TARGET_PTX_6_3 (ptx_version_option >= PTX_VERSION_6_3)
#define TARGET_PTX_7_0 (ptx_version_option >= PTX_VERSION_7_0)
#define TARGET_PTX_7_3 (ptx_version_option >= PTX_VERSION_7_3)
#define TARGET_PTX_7_8 (ptx_version_option >= PTX_VERSION_7_8)

/* Registers.  Since ptx is a virtual target, we just define a few
   hard registers for special purposes and leave pseudos unallocated.
   We have to have some available hard registers, to keep gcc setup
   happy.  */
#define FIRST_PSEUDO_REGISTER 16
#define FIXED_REGISTERS	    { 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
#define CALL_USED_REGISTERS { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }

/* Register Classes.  */
enum reg_class             {  NO_REGS,    ALL_REGS,	LIM_REG_CLASSES };
#define REG_CLASS_NAMES    { "NO_REGS",  "ALL_REGS" }
#define REG_CLASS_CONTENTS { { 0x0000 }, { 0xFFFF } }
#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS ALL_REGS
#define REGNO_REG_CLASS(R) ((void)(R), ALL_REGS)
#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_BASE_P(X) true
#define REGNO_OK_FOR_INDEX_P(X) false

#define CLASS_MAX_NREGS(class, mode) \
  ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)		\
  if ((MODE) == QImode || (MODE) == HImode)		\
    {							\
      (MODE) = SImode;					\
      (void)(UNSIGNEDP);				\
      (void)(TYPE);					\
    }

/* Stack and Calling.  */

#define FRAME_GROWS_DOWNWARD 0
#define STACK_GROWS_DOWNWARD 1

#define NVPTX_RETURN_REGNUM 0
#define STACK_POINTER_REGNUM 1
#define FRAME_POINTER_REGNUM 2
#define ARG_POINTER_REGNUM 3
#define STATIC_CHAIN_REGNUM 4
/* This register points to the shared memory location with the current warp's
   soft stack pointer (__nvptx_stacks[tid.y]).  */
#define SOFTSTACK_SLOT_REGNUM 5
/* This register is used to save the previous value of the soft stack pointer
   in the prologue and restore it when returning.  */
#define SOFTSTACK_PREV_REGNUM 6

#define REGISTER_NAMES							\
  {									\
    "%value", "%stack", "%frame", "%args",                              \
    "%chain", "%sspslot", "%sspprev", "%hr7",                           \
    "%hr8", "%hr9", "%hr10", "%hr11", "%hr12", "%hr13", "%hr14", "%hr15" \
  }

#define FIRST_PARM_OFFSET(FNDECL) ((void)(FNDECL), 0)
#define PUSH_ARGS_REVERSED 1
#define ACCUMULATE_OUTGOING_ARGS 1

/* Avoid using the argument pointer for frame-related things.  */
#define FRAME_POINTER_CFA_OFFSET(FNDECL) ((void)(FNDECL), 0)

#ifdef HOST_WIDE_INT
struct nvptx_args {
  tree fntype;
  /* Number of arguments passed in registers so far.  */
  int count;
};
#endif

#define CUMULATIVE_ARGS struct nvptx_args

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  ((CUM).fntype = (FNTYPE), (CUM).count = 0, (void)0)

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
    { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}	\
  }

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = 0)

/* Addressing Modes.  */

#define MAX_REGS_PER_ADDRESS 1

#define LEGITIMATE_PIC_OPERAND_P(X) 1


#if defined HOST_WIDE_INT
struct GTY(()) machine_function
{
  rtx_expr_list *call_args;  /* Arg list for the current call.  */
  bool doing_call; /* Within a CALL_ARGS ... CALL_ARGS_END sequence.  */
  bool is_variadic;  /* This call is variadic  */
  bool has_variadic;  /* Current function has a variadic call.  */
  bool has_chain; /* Current function has outgoing static chain.  */
  bool has_softstack; /* Current function has a soft stack frame.  */
  bool has_simtreg; /* Current function has an OpenMP SIMD region.  */
  int num_args;	/* Number of args of current call.  */
  int return_mode; /* Return mode of current fn.
		      (machine_mode not defined yet.) */
  rtx axis_predicate[2]; /* Neutering predicates.  */
  int axis_dim[2]; /* Maximum number of threads on each axis, dim[0] is
		      vector_length, dim[1] is num_workers.  */
  bool axis_dim_init_p;
  rtx bcast_partition; /* Register containing the size of each
			  vector's partition of share-memory used to
			  broadcast state.  */
  rtx red_partition; /* Similar to bcast_partition, except for vector
			reductions.  */
  rtx sync_bar; /* Synchronization barrier ID for vectors.  */
  rtx unisimt_master; /* 'Master lane index' for -muniform-simt.  */
  rtx unisimt_predicate; /* Predicate for -muniform-simt.  */
  rtx unisimt_outside_simt_predicate; /* Predicate for -muniform-simt.  */
  rtx unisimt_location; /* Mask location for -muniform-simt.  */
  /* The following two fields hold the maximum size resp. alignment required
     for per-lane storage in OpenMP SIMD regions.  */
  unsigned HOST_WIDE_INT simt_stack_size;
  unsigned HOST_WIDE_INT simt_stack_align;
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

#define DEBUGGER_REGNO(N) N

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
  nvptx_output_aligned_decl (FILE, NAME, DECL, SIZE, ALIGN)

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
  nvptx_output_aligned_decl (FILE, NAME, DECL, SIZE, ALIGN)

#define CASE_VECTOR_PC_RELATIVE flag_pic
#define JUMP_TABLES_IN_TEXT_SECTION flag_pic

#define ADDR_VEC_ALIGN(VEC) (JUMP_TABLES_IN_TEXT_SECTION ? 5 : 2)

/* Misc.  */

#define DWARF2_LINENO_DEBUGGING_INFO 1

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_BITSIZE ((MODE)), 2)
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) \
  ((VALUE) = GET_MODE_BITSIZE ((MODE)), 2)

#define SUPPORTS_WEAK 1

#define MAKE_DECL_ONE_ONLY(DECL) \
  (DECL_WEAK (DECL) = 1)

/* The documentation states that ASM_OUTPUT_DEF_FROM_DECLS is used in
   preference to ASM_OUTPUT_DEF if the tree nodes are available.  However, we
   need the tree nodes to emit the prototype, so at this point it's not clear
   how we can support ASM_OUTPUT_DEF.  Still, we need to define it, or
   ASM_OUTPUT_DEF_FROM_DECLS is ignored.  For now, assert, and once we run
   into it possibly improve by somehow emitting the prototype elsewhere, or
   emitting a reasonable error message.  */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)	\
  do						\
    {						\
      (void) (FILE);				\
      (void) (LABEL1);				\
      (void) (LABEL2);				\
      gcc_unreachable ();			\
    }						\
  while (0)
#define ASM_OUTPUT_DEF_FROM_DECLS(STREAM, NAME, VALUE)	\
  nvptx_asm_output_def_from_decls (STREAM, NAME, VALUE)

/* ..., but also override other macros to avoid 'gcc/defaults.h'-initialization
   due to that dummy 'ASM_OUTPUT_DEF'.  */
#define TARGET_USE_LOCAL_THUNK_ALIAS_P(DECL) TARGET_SUPPORTS_ALIASES
#define TARGET_SUPPORTS_ALIASES (nvptx_alias != 0)

#define NO_DOT_IN_LABEL
#define ASM_COMMENT_START "//"

#define STORE_FLAG_VALUE 1
#define FLOAT_STORE_FLAG_VALUE(MODE) REAL_VALUE_ATOF("1.0", (MODE))

#define CASE_VECTOR_MODE SImode
#define MOVE_MAX 8
#define MOVE_RATIO(SPEED) 4
#define FUNCTION_MODE QImode

/* Implement global constructor, destructor support in a conceptually simpler
   way than using 'collect2' (the program): implement the respective
   functionality in the nvptx-tools 'ld'.  This however still requires the
   compiler-side effects corresponding to 'USE_COLLECT2': the global
   constructor, destructor support functions need to have external linkage, and
   therefore names that are "unique across the whole link".  Use
   '!targetm.have_ctors_dtors' to achieve this (..., and thus don't need to
   provide 'targetm.asm_out.constructor', 'targetm.asm_out.destructor').  */
#define TARGET_HAVE_CTORS_DTORS false

/* See 'libgcc/config/nvptx/crt0.c' for wrapping of 'main'.  */
#define HAS_INIT_SECTION

/* The default doesn't fly ('internal compiler error: in simplify_subreg' when
   'dw2_assemble_integer' -> 'assemble_integer' attempts to simplify
   '(minus:DI (symbol_ref:DI ("$LEHB0")) (symbol_ref:DI ("$LFB3")))', for
   example.  Just emit something; it's not getting used, anyway.  */
#define ASM_OUTPUT_DWARF_DELTA(STREAM, SIZE, LABEL1, LABEL2) \
  do \
    { \
      fprintf (STREAM, "%s[%d]: ", targetm.asm_out.byte_op, SIZE); \
      const char *label1 = LABEL1; \
      assemble_name_raw (STREAM, label1 ? label1 : "*nil"); \
      fprintf (STREAM, " - "); \
      const char *label2 = LABEL2; \
      assemble_name_raw (STREAM, label2 ? label2 : "*nil"); \
    } \
  while (0)

#endif /* GCC_NVPTX_H */
