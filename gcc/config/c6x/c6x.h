/* Target Definitions for TI C6X.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
   Contributed by Andrew Jenner <andrew@codesourcery.com>
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

#ifndef GCC_C6X_H
#define GCC_C6X_H

/* Feature bit definitions that enable specific insns.  */
#define C6X_INSNS_C62X		1
#define C6X_INSNS_C64X		2
#define C6X_INSNS_C64XP		4
#define C6X_INSNS_C67X		8
#define C6X_INSNS_C67XP		16
#define C6X_INSNS_C674X		32
#define C6X_INSNS_ATOMIC	64
#define C6X_INSNS_ALL_CPU_BITS	127

#define C6X_DEFAULT_INSN_MASK						\
  (C6X_INSNS_C62X | C6X_INSNS_C64X | C6X_INSNS_C64XP)

/* A mask of allowed insn types, as defined above.  */
extern unsigned long c6x_insn_mask;

/* Value of -march= */
extern c6x_cpu_t c6x_arch;
#define C6X_DEFAULT_ARCH C6X_CPU_C64XP

/* True if the target has C64x instructions.  */
#define TARGET_INSNS_64		((c6x_insn_mask & C6X_INSNS_C64X) != 0)
/* True if the target has C64x+ instructions.  */
#define TARGET_INSNS_64PLUS	((c6x_insn_mask & C6X_INSNS_C64XP) != 0)
/* True if the target has C67x instructions.  */
#define TARGET_INSNS_67		((c6x_insn_mask & C6X_INSNS_C67X) != 0)
/* True if the target has C67x+ instructions.  */
#define TARGET_INSNS_67PLUS	((c6x_insn_mask & C6X_INSNS_C67XP) != 0)

/* True if the target supports doubleword loads.  */
#define TARGET_LDDW		(TARGET_INSNS_64 || TARGET_INSNS_67)
/* True if the target supports doubleword loads.  */
#define TARGET_STDW		TARGET_INSNS_64
/* True if the target supports the MPY32 family of instructions.  */
#define TARGET_MPY32		TARGET_INSNS_64PLUS
/* True if the target has floating point hardware.  */
#define TARGET_FP		TARGET_INSNS_67
/* True if the target has C67x+ floating point extensions.  */
#define TARGET_FP_EXT		TARGET_INSNS_67PLUS

#define TARGET_DEFAULT 0

/* Run-time Target.  */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_assert ("machine=tic6x");		\
      builtin_assert ("cpu=tic6x");		\
      builtin_define ("__TMS320C6X__");		\
      builtin_define ("_TMS320C6X");		\
						\
      if (TARGET_DSBT)				\
	builtin_define ("__DSBT__");		\
						\
      if (TARGET_BIG_ENDIAN)			\
	builtin_define ("_BIG_ENDIAN");		\
      else					\
	builtin_define ("_LITTLE_ENDIAN");	\
						\
      switch (c6x_arch)				\
	{					\
	case C6X_CPU_C62X:			\
	  builtin_define ("_TMS320C6200");	\
	  break;				\
						\
	case C6X_CPU_C64XP:			\
	  builtin_define ("_TMS320C6400_PLUS");	\
	  /* ... fall through ... */		\
	case C6X_CPU_C64X:			\
	  builtin_define ("_TMS320C6400");	\
	  break;				\
						\
	case C6X_CPU_C67XP:			\
	  builtin_define ("_TMS320C6700_PLUS");	\
	  /* ... fall through ... */		\
	case C6X_CPU_C67X:			\
	  builtin_define ("_TMS320C6700");	\
	  break;				\
						\
	case C6X_CPU_C674X:			\
	  builtin_define ("_TMS320C6740");	\
	  builtin_define ("_TMS320C6700_PLUS");	\
	  builtin_define ("_TMS320C6700");	\
	  builtin_define ("_TMS320C6400_PLUS");	\
	  builtin_define ("_TMS320C6400");	\
	  break;				\
	}					\
    } while (0)

#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!march=*:-march=%(VALUE)}" }

/* Storage Layout.  */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

#define REG_WORDS_BIG_ENDIAN 0

#define UNITS_PER_WORD 4
#define PARM_BOUNDARY 8
#define STACK_BOUNDARY 64
#define FUNCTION_BOUNDARY 32
#define BIGGEST_ALIGNMENT 64
#define STRICT_ALIGNMENT 1

/* The ABI requires static arrays must be at least 8 byte aligned.
   Really only externally visible arrays must be aligned this way, as
   only those are directly visible from another compilation unit.  But
   we don't have that information available here.  */
#define DATA_ABI_ALIGNMENT(TYPE, ALIGN)					\
  (((ALIGN) < BITS_PER_UNIT * 8 && TREE_CODE (TYPE) == ARRAY_TYPE)	\
   ? BITS_PER_UNIT * 8 : (ALIGN))

/* Type Layout.  */

#define DEFAULT_SIGNED_CHAR 1

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Registers.  */

#define FIRST_PSEUDO_REGISTER 67
#define FIXED_REGISTERS					\
  { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,	\
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	\
    1, 1, 1}
#define CALL_USED_REGISTERS				\
  { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,	\
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1,	\
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	\
    1, 1, 1}

/* This lists call-used non-predicate registers first, followed by call-used
   registers, followed by predicate registers.  We want to avoid allocating
   the predicate registers for other uses as much as possible.  */
#define REG_ALLOC_ORDER							\
  {									\
    REG_A0, REG_A3, REG_A4, REG_A5, REG_A6, REG_A7, REG_A8, REG_A9,	\
    REG_A16, REG_A17, REG_A18, REG_A19, REG_A20, REG_A21, REG_A22, REG_A23, \
    REG_A24, REG_A25, REG_A26, REG_A27, REG_A28, REG_A29, REG_A30, REG_A31, \
    REG_B4, REG_B5, REG_B6, REG_B7, REG_B8, REG_B9, REG_B16,	\
    REG_B17, REG_B18, REG_B19, REG_B20, REG_B21, REG_B22, REG_B23, REG_B24, \
    REG_B25, REG_B26, REG_B27, REG_B28, REG_B29, REG_B30, REG_B31,	\
    REG_A10, REG_A11, REG_A12, REG_A13, REG_A14, REG_A15,		\
    REG_B3, REG_B10, REG_B11, REG_B12, REG_B13, REG_B14, REG_B15,	\
    REG_A1, REG_A2, REG_B0, REG_B1, REG_B2, REG_ILC			\
  }

#define HARD_REGNO_NREGS(regno, mode)		\
  ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1)  \
   / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(reg, mode) (GET_MODE_SIZE (mode) <= UNITS_PER_WORD \
				       ? 1 : ((reg) & 1) == 0)

#define MODES_TIEABLE_P(mode1, mode2)	       \
  ((mode1) == (mode2) ||		       \
   (GET_MODE_SIZE (mode1) <= UNITS_PER_WORD && \
    GET_MODE_SIZE (mode2) <= UNITS_PER_WORD))


/* Register Classes.  */

enum reg_class
  {
    NO_REGS,
    PREDICATE_A_REGS,
    PREDICATE_B_REGS,
    PREDICATE_REGS,
    PICREG,
    SPREG,
    CALL_USED_B_REGS,
    NONPREDICATE_A_REGS,
    NONPREDICATE_B_REGS,
    NONPREDICATE_REGS,
    A_REGS,
    B_REGS,
    GENERAL_REGS,
    ALL_REGS,
    LIM_REG_CLASSES
  };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES {	  \
    "NO_REGS",			  \
    "PREDICATE_A_REGS",		  \
    "PREDICATE_B_REGS",		  \
    "PREDICATE_REGS",		  \
    "PICREG",			  \
    "SPREG",			  \
    "CALL_USED_B_REGS",		  \
    "NONPREDICATE_A_REGS",	  \
    "NONPREDICATE_B_REGS",	  \
    "NONPREDICATE_REGS",	  \
    "A_REGS",			  \
    "B_REGS",			  \
    "GENERAL_REGS",		  \
    "ALL_REGS" }

#define REG_CLASS_CONTENTS			\
{						\
  /* NO_REGS.  */				\
  { 0x00000000, 0x00000000, 0 },		\
  /* PREDICATE_A_REGS.  */			\
  { 0x00000006, 0x00000000, 0 },		\
  /* PREDICATE_B_REGS.  */			\
  { 0x00000000, 0x00000007, 0 },		\
  /* PREDICATE_REGS.  */			\
  { 0x00000006, 0x00000007, 0 },		\
  /* PICREG.  */				\
  { 0x00000000, 0x00004000, 0 },		\
  /* SPREG.  */					\
  { 0x00000000, 0x00008000, 0 },		\
  /* CALL_USED_B_REGS.  */			\
  { 0x00000000, 0xFFFF03FF, 0 },		\
  /* NONPREDICATE_A_REGS.  */			\
  { 0xFFFFFFF9, 0x00000000, 0 },		\
  /* NONPREDICATE_B_REGS.  */			\
  { 0x00000000, 0xFFFFFFF8, 0 },		\
  /* NONPREDICATE_REGS.  */			\
  { 0xFFFFFFF9, 0xFFFFFFF8, 0 },		\
  /* A_REGS.  */				\
  { 0xFFFFFFFF, 0x00000000, 3 },		\
  /* B_REGS.  */				\
  { 0x00000000, 0xFFFFFFFF, 3 },		\
  /* GENERAL_REGS.  */				\
  { 0xFFFFFFFF, 0xFFFFFFFF, 3 },		\
  /* ALL_REGS.  */				\
  { 0xFFFFFFFF, 0xFFFFFFFF, 7 },		\
}

#define A_REGNO_P(N) ((N) <= REG_A31)
#define B_REGNO_P(N) ((N) >= REG_B0 && (N) <= REG_B31)

#define A_REG_P(X) (REG_P (X) && A_REGNO_P (REGNO (X)))
#define CROSS_OPERANDS(X0,X1) \
  (A_REG_P (X0) == A_REG_P (X1) ? CROSS_N : CROSS_Y)

#define REGNO_REG_CLASS(reg) \
    ((reg) >= REG_A1 && (reg) <= REG_A2 ? PREDICATE_A_REGS	\
     : (reg) == REG_A0 && TARGET_INSNS_64 ? PREDICATE_A_REGS	\
     : (reg) >= REG_B0 && (reg) <= REG_B2 ? PREDICATE_B_REGS	\
     : A_REGNO_P (reg) ? NONPREDICATE_A_REGS			\
     : call_used_regs[reg] ? CALL_USED_B_REGS : B_REGS)

#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS ALL_REGS

#define REGNO_OK_FOR_BASE_STRICT_P(X)				\
  ((X) < FIRST_PSEUDO_REGISTER					\
   || (reg_renumber[X] >= 0 && reg_renumber[X] < FIRST_PSEUDO_REGISTER))
#define REGNO_OK_FOR_BASE_NONSTRICT_P(X) 1

#define REGNO_OK_FOR_INDEX_STRICT_P(X)				\
  ((X) < FIRST_PSEUDO_REGISTER					\
   || (reg_renumber[X] >= 0 && reg_renumber[X] < FIRST_PSEUDO_REGISTER))
#define REGNO_OK_FOR_INDEX_NONSTRICT_P(X) 1

#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_STRICT_P (X)
#define REGNO_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_STRICT_P (X)
#else
#define REGNO_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_NONSTRICT_P (X)
#define REGNO_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_NONSTRICT_P (X)
#endif

#define CLASS_MAX_NREGS(class, mode) \
  ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define REGNO_OK_FOR_INDIRECT_JUMP_P(REGNO, MODE) B_REGNO_P (REGNO)

/* Stack and Calling.  */

/* SP points to 4 bytes below the first word of the frame.  */
#define STACK_POINTER_OFFSET 4
/* Likewise for AP (which is the incoming stack pointer).  */
#define FIRST_PARM_OFFSET(fundecl) 4
#define STARTING_FRAME_OFFSET 0
#define FRAME_GROWS_DOWNWARD 1
#define STACK_GROWS_DOWNWARD

#define STACK_POINTER_REGNUM REG_B15
#define HARD_FRAME_POINTER_REGNUM REG_A15
/* These two always get eliminated in favour of the stack pointer
   or the hard frame pointer.  */
#define FRAME_POINTER_REGNUM REG_FRAME
#define ARG_POINTER_REGNUM REG_ARGP

#define PIC_OFFSET_TABLE_REGNUM REG_B14

/* We keep the stack pointer constant rather than using push/pop
   instructions.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Before the prologue, the return address is in the B3 register.  */
#define RETURN_ADDR_REGNO REG_B3
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, RETURN_ADDR_REGNO)
#define DWARF_FRAME_RETURN_COLUMN	DWARF_FRAME_REGNUM (RETURN_ADDR_REGNO)

#define RETURN_ADDR_RTX(COUNT, FRAME) c6x_return_addr_rtx (COUNT)

#define INCOMING_FRAME_SP_OFFSET 0
#define ARG_POINTER_CFA_OFFSET(fundecl) 0

#define STATIC_CHAIN_REGNUM REG_A2

struct c6x_args {
  /* Number of arguments to pass in registers.  */
  int nregs;
  /* Number of arguments passed in registers so far.  */
  int count;
};

#define CUMULATIVE_ARGS struct c6x_args

#define INIT_CUMULATIVE_ARGS(cum, fntype, libname, fndecl, n_named_args) \
  c6x_init_cumulative_args (&cum, fntype, libname, n_named_args)

#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (c6x_block_reg_pad_upward (MODE, TYPE, FIRST) ? upward : downward)

#define FUNCTION_ARG_REGNO_P(r) \
    (((r) >= REG_A4 && (r) <= REG_A13) || ((r) >= REG_B4 && (r) <= REG_B13))

#define DEFAULT_PCC_STRUCT_RETURN 0

#define FUNCTION_PROFILER(file, labelno) \
  fatal_error ("profiling is not yet implemented for this architecture")


/* Trampolines.  */
#define TRAMPOLINE_SIZE 32
#define TRAMPOLINE_ALIGNMENT 256

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}	\

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = c6x_initial_elimination_offset ((FROM), (TO)))

/* Addressing Modes.  */

#define CONSTANT_ADDRESS_P(x) (CONSTANT_P(x) && GET_CODE(x) != CONST_DOUBLE)
#define MAX_REGS_PER_ADDRESS 2

#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_DECREMENT 1
#define HAVE_PRE_INCREMENT 1
#define HAVE_POST_INCREMENT 1

/* Register forms are available, but due to scaling we currently don't
   support them.  */
#define HAVE_PRE_MODIFY_DISP 1
#define HAVE_POST_MODIFY_DISP 1

#define LEGITIMATE_PIC_OPERAND_P(X) \
  (!symbolic_operand (X, SImode))

struct GTY(()) machine_function
{
  /* True if we expanded a sibling call.  */
  int contains_sibcall;
};

/* Costs.  */
#define NO_FUNCTION_CSE 1

#define SLOW_BYTE_ACCESS 0

#define BRANCH_COST(speed_p, predictable_p) 6


/* Model costs for the vectorizer.  */

/* Cost of conditional branch.  */
#ifndef TARG_COND_BRANCH_COST
#define TARG_COND_BRANCH_COST        6
#endif

/* Cost of any scalar operation, excluding load and store.  */
#ifndef TARG_SCALAR_STMT_COST
#define TARG_SCALAR_STMT_COST        1
#endif

/* Cost of scalar load. */
#undef TARG_SCALAR_LOAD_COST
#define TARG_SCALAR_LOAD_COST        2 /* load + rotate */

/* Cost of scalar store.  */
#undef TARG_SCALAR_STORE_COST
#define TARG_SCALAR_STORE_COST       10

/* Cost of any vector operation, excluding load, store,
   or vector to scalar operation.  */
#undef TARG_VEC_STMT_COST
#define TARG_VEC_STMT_COST           1

/* Cost of vector to scalar operation.  */
#undef TARG_VEC_TO_SCALAR_COST
#define TARG_VEC_TO_SCALAR_COST      1

/* Cost of scalar to vector operation.  */
#undef TARG_SCALAR_TO_VEC_COST
#define TARG_SCALAR_TO_VEC_COST      1

/* Cost of aligned vector load.  */
#undef TARG_VEC_LOAD_COST
#define TARG_VEC_LOAD_COST           1

/* Cost of misaligned vector load.  */
#undef TARG_VEC_UNALIGNED_LOAD_COST
#define TARG_VEC_UNALIGNED_LOAD_COST 2

/* Cost of vector store.  */
#undef TARG_VEC_STORE_COST
#define TARG_VEC_STORE_COST          1

/* Cost of vector permutation.  */
#ifndef TARG_VEC_PERMUTE_COST
#define TARG_VEC_PERMUTE_COST        1
#endif

/* ttype entries (the only interesting data references used) are
   sb-relative got-indirect (aka .ehtype).  */
#define ASM_PREFERRED_EH_DATA_FORMAT(code, data) \
  (((code) == 0 && (data) == 1) ? (DW_EH_PE_datarel | DW_EH_PE_indirect) \
				: DW_EH_PE_absptr)

/* This should be the same as the definition in elfos.h, plus the call
   to output special unwinding directives.  */
#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      c6x_output_file_unwind (FILE);				\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "function");	\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));		\
      ASM_OUTPUT_LABEL (FILE, NAME);				\
    }								\
  while (0)

/* This should be the same as the definition in elfos.h, plus the call
   to output special unwinding directives.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL) \
  c6x_function_end (STREAM, NAME)

/* Arbitrarily choose A4/A5.  */
#define EH_RETURN_DATA_REGNO(N) (((N) < 2) ? (N) + 4 : INVALID_REGNUM)

/* The register that holds the return address in exception handlers.  */
#define C6X_EH_STACKADJ_REGNUM  3
#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (SImode, C6X_EH_STACKADJ_REGNUM)


/* Assembler Format.  */

#define DWARF2_ASM_LINE_DEBUG_INFO 1

#undef ASM_APP_ON
#define ASM_APP_ON "\t; #APP \n"
#undef ASM_APP_OFF
#define ASM_APP_OFF "\t; #NO_APP \n"

#define ASM_OUTPUT_COMMON(stream, name, size, rounded)
#define ASM_OUTPUT_LOCAL(stream, name, size, rounded)

#define GLOBAL_ASM_OP "\t.global\t"

#define REGISTER_NAMES						\
  {								\
    "A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7",		\
    "A8", "A9", "A10", "A11", "A12", "A13", "A14", "A15",	\
    "A16", "A17", "A18", "A19", "A20", "A21", "A22", "A23",	\
    "A24", "A25", "A26", "A27", "A28", "A29", "A30", "A31",     \
    "B0", "B1", "B2", "B3", "B4", "B5", "B6", "B7",             \
    "B8", "B9", "B10", "B11", "B12", "B13", "B14", "B15",       \
    "B16", "B17", "B18", "B19", "B20", "B21", "B22", "B23",     \
    "B24", "B25", "B26", "B27", "B28", "B29", "B30", "B31",	\
    "FP", "ARGP", "ILC" }

#define DBX_REGISTER_NUMBER(N) (dbx_register_map[(N)])

extern unsigned const dbx_register_map[FIRST_PSEUDO_REGISTER];

#define FINAL_PRESCAN_INSN c6x_final_prescan_insn

#define TEXT_SECTION_ASM_OP ".text;"
#define DATA_SECTION_ASM_OP ".data;"

#define ASM_OUTPUT_ALIGN(stream, power)			    \
  do							    \
    {							    \
      if (power)					    \
        fprintf ((stream), "\t.align\t%d\n", power);	    \
    }                                                       \
  while (0)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)    	\
do { char __buf[256];					\
     fprintf (FILE, "\t.long\t");				\
     ASM_GENERATE_INTERNAL_LABEL (__buf, "L", VALUE);	\
     assemble_name (FILE, __buf);			\
     fputc ('\n', FILE);				\
   } while (0)

/* Determine whether to place EXP (an expression or a decl) should be
   placed into one of the small data sections.  */
#define PLACE_IN_SDATA_P(EXP) \
  (c6x_sdata_mode == C6X_SDATA_NONE ? false	\
   : c6x_sdata_mode == C6X_SDATA_ALL ? true	\
   : !AGGREGATE_TYPE_P (TREE_TYPE (EXP)))

#define SCOMMON_ASM_OP "\t.scomm\t"

#undef  ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      if (DECL != NULL && PLACE_IN_SDATA_P (DECL))			\
	fprintf ((FILE), "%s", SCOMMON_ASM_OP);				\
      else								\
	fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ",%u,%u\n", (int)(SIZE), (ALIGN) / BITS_PER_UNIT);\
    }									\
  while (0)

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  */

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
do {									\
  if (PLACE_IN_SDATA_P (DECL))						\
    switch_to_section (sbss_section);					\
  else									\
    switch_to_section (bss_section);					\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
  if (!flag_inhibit_size_directive)					\
    ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, SIZE);			\
  ASM_OUTPUT_ALIGN ((FILE), exact_log2((ALIGN) / BITS_PER_UNIT));	\
  ASM_OUTPUT_LABEL(FILE, NAME);						\
  ASM_OUTPUT_SKIP((FILE), (SIZE) ? (SIZE) : 1);				\
} while (0)

#define CASE_VECTOR_PC_RELATIVE flag_pic
#define JUMP_TABLES_IN_TEXT_SECTION flag_pic

#define ADDR_VEC_ALIGN(VEC) (JUMP_TABLES_IN_TEXT_SECTION ? 5 : 2)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  do { char buf[100];					\
       fputs ("\t.long ", FILE);			\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", VALUE);	\
       assemble_name (FILE, buf);			\
       putc ('-', FILE);				\
       ASM_GENERATE_INTERNAL_LABEL (buf, "L", REL);	\
       assemble_name (FILE, buf);			\
       putc ('\n', FILE);				\
     } while (0)

/* Misc.  */

#define CASE_VECTOR_MODE SImode
#define MOVE_MAX 4
#define MOVE_RATIO(SPEED) 4
#define TRULY_NOOP_TRUNCATION(outprec, inprec) 1
#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 32, 1)
#define Pmode SImode
#define FUNCTION_MODE QImode

#define CPU_UNITS_QUERY 1

extern int c6x_initial_flag_pic;

#endif /* GCC_C6X_H */
