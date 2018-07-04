/* Copyright (C) 2006-2018 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* Run-time Target */
#define TARGET_CPU_CPP_BUILTINS()	spu_cpu_cpp_builtins(pfile)

#define C_COMMON_OVERRIDE_OPTIONS spu_c_common_override_options()

#define INIT_EXPANDERS spu_init_expanders()

/* Which processor to generate code or schedule for.  */
enum processor_type
{
  PROCESSOR_CELL,
  PROCESSOR_CELLEDP
};

extern GTY(()) int spu_arch;
extern GTY(()) int spu_tune;

/* Support for a compile-time default architecture and tuning.  The rules are:
   --with-arch is ignored if -march is specified.
   --with-tune is ignored if -mtune is specified.  */
#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!march=*:-march=%(VALUE)}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }

/* Default target_flags if no switches specified.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ERROR_RELOC | MASK_SAFE_DMA | MASK_BRANCH_HINTS \
			| MASK_SAFE_HINTS | MASK_ADDRESS_SPACE_CONVERSION)
#endif


/* Storage Layout */

#define BITS_BIG_ENDIAN 1

#define BYTES_BIG_ENDIAN 1

#define WORDS_BIG_ENDIAN 1

/* GCC uses word_mode in many places, assuming that it is the fastest
   integer mode.  That is not the case for SPU though.  We can't use
   32 here because (of some reason I can't remember.) */
#define BITS_PER_WORD 128

#define UNITS_PER_WORD (BITS_PER_WORD/BITS_PER_UNIT)

/* When building libgcc, we need to assume 4 words per units even
   though UNITS_PER_WORD is 16, because the SPU has basically a 32-bit
   instruction set although register size is 128 bits.  In particular,
   this causes libgcc to contain __divdi3 instead of __divti3 etc.
   However, we allow this default to be re-defined on the command
   line, so that we can use the LIB2_SIDITI_CONV_FUNCS mechanism
   to get (in addition) TImode versions of some routines.  */
#ifndef LIBGCC2_UNITS_PER_WORD
#define LIBGCC2_UNITS_PER_WORD 4
#endif

#define POINTER_SIZE 32

#define PARM_BOUNDARY 128

#define STACK_BOUNDARY 128

/* We want it 8-byte aligned so we can properly use dual-issue
   instructions, which can only happen on an 8-byte aligned address. */
#define FUNCTION_BOUNDARY 64

/* We would like to allow a larger alignment for data objects (for DMA)
   but the aligned attribute is limited by BIGGEST_ALIGNMENT.  We don't
   define BIGGEST_ALIGNMENT as larger because it is used in other places
   and would end up wasting space.  (Is this still true?)  */
#define BIGGEST_ALIGNMENT 128

#define MINIMUM_ATOMIC_ALIGNMENT 128

/* Make all static objects 16-byte aligned.  This allows us to assume
   they are also padded to 16-bytes, which means we can use a single
   load or store instruction to access them.  Do the same for objects
   on the stack.  (Except a bug (?) allows some stack objects to be
   unaligned.)  */
#define DATA_ALIGNMENT(TYPE,ALIGN) ((ALIGN) > 128 ? (ALIGN) : 128)
#define LOCAL_ALIGNMENT(TYPE,ALIGN) ((ALIGN) > 128 ? (ALIGN) : 128)

#define EMPTY_FIELD_BOUNDARY 32

#define STRICT_ALIGNMENT 1

/* symbol_ref's of functions are not aligned to 16 byte boundary. */
#define ALIGNED_SYMBOL_REF_P(X) \
	(GET_CODE (X) == SYMBOL_REF \
          && (SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_ALIGN1) == 0 \
	  && (! SYMBOL_REF_FUNCTION_P (X) \
	      || align_functions_value >= 16))

#define PCC_BITFIELD_TYPE_MATTERS 1

#define MAX_FIXED_MODE_SIZE 128

#define STACK_SAVEAREA_MODE(save_level) \
  (save_level == SAVE_FUNCTION ? VOIDmode \
    : save_level == SAVE_NONLOCAL ? SImode \
      : Pmode)

#define STACK_SIZE_MODE SImode


/* Type Layout */

#define INT_TYPE_SIZE 32

#define LONG_TYPE_SIZE 32

#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32

#define DOUBLE_TYPE_SIZE 64

#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 0

#define STDINT_LONG32 0


/* Register Basics */

/* 128-130 are special registers that never appear in assembly code. */
#define FIRST_PSEUDO_REGISTER 131

#define FIXED_REGISTERS {			    \
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    1, 1, 1 \
}

#define CALL_USED_REGISTERS {			    \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
    1, 1, 1 \
}


/* Register Classes */

enum reg_class { 
   NO_REGS, 
   GENERAL_REGS,
   ALL_REGS,
   LIM_REG_CLASSES 
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
{  "NO_REGS", \
   "GENERAL_REGS", \
   "ALL_REGS" \
}

#define REG_CLASS_CONTENTS { \
    {0, 0, 0, 0, 0}, /* no regs */ \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3}, /* general regs */ \
    {0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff, 0x3}} /* all regs */

#define REGNO_REG_CLASS(REGNO) ((void)(REGNO), GENERAL_REGS)


#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS GENERAL_REGS

#define REGNO_OK_FOR_BASE_P(regno) \
   ((regno) < FIRST_PSEUDO_REGISTER || (regno > LAST_VIRTUAL_REGISTER && reg_renumber[regno] >= 0))

#define REGNO_OK_FOR_INDEX_P(regno)  \
   ((regno) < FIRST_PSEUDO_REGISTER || (regno > LAST_VIRTUAL_REGISTER && reg_renumber[regno] >= 0))

#define INT_REG_OK_FOR_INDEX_P(X,STRICT) \
	((!(STRICT) || REGNO_OK_FOR_INDEX_P (REGNO (X))))
#define INT_REG_OK_FOR_BASE_P(X,STRICT) \
	((!(STRICT) || REGNO_OK_FOR_BASE_P (REGNO (X))))

#define REGISTER_TARGET_PRAGMAS() do {					\
c_register_addr_space ("__ea", ADDR_SPACE_EA);				\
targetm.resolve_overloaded_builtin = spu_resolve_overloaded_builtin;	\
}while (0)


/* Frame Layout */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

#define STACK_POINTER_OFFSET 32

#define FIRST_PARM_OFFSET(FNDECL) (0)

#define DYNAMIC_CHAIN_ADDRESS(FP) plus_constant (Pmode, (FP), -16)

#define RETURN_ADDR_RTX(COUNT,FP) (spu_return_addr (COUNT, FP))

/* Should this be defined?  Would it simplify our implementation. */
/* #define RETURN_ADDR_IN_PREVIOUS_FRAME */

#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG(Pmode, LINK_REGISTER_REGNUM)

#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (LINK_REGISTER_REGNUM)

#define ARG_POINTER_CFA_OFFSET(FNDECL) \
  (crtl->args.pretend_args_size - STACK_POINTER_OFFSET)


/* Stack Checking */

/* We store the Available Stack Size in the second slot of the stack
   register.   We emit stack checking code during the prologue.  */
#define STACK_CHECK_BUILTIN 1


/* Frame Registers, and other registers */

#define STACK_POINTER_REGNUM 1

/* Will be eliminated. */
#define FRAME_POINTER_REGNUM 128

/* This is not specified in any ABI, so could be set to anything. */
#define HARD_FRAME_POINTER_REGNUM 127

/* Will be eliminated. */
#define ARG_POINTER_REGNUM 129

#define STATIC_CHAIN_REGNUM 2

#define LINK_REGISTER_REGNUM 0

/* Used to keep track of instructions that have clobbered the hint
 * buffer.  Users can also specify it in inline asm. */
#define HBR_REGNUM 130

#define MAX_REGISTER_ARGS    72
#define FIRST_ARG_REGNUM     3
#define LAST_ARG_REGNUM      (FIRST_ARG_REGNUM + MAX_REGISTER_ARGS - 1)

#define MAX_REGISTER_RETURN  72
#define FIRST_RETURN_REGNUM  3
#define LAST_RETURN_REGNUM   (FIRST_RETURN_REGNUM + MAX_REGISTER_RETURN - 1)


/* Elimination */

#define ELIMINABLE_REGS  \
  {{ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},				\
  {ARG_POINTER_REGNUM,	 HARD_FRAME_POINTER_REGNUM},			\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
  {FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = spu_initial_elimination_offset((FROM),(TO)))


/* Stack Arguments */

#define ACCUMULATE_OUTGOING_ARGS 1

#define REG_PARM_STACK_SPACE(FNDECL) 0

#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1


/* Register Arguments */

#define CUMULATIVE_ARGS int

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
		((CUM) = 0)

#define PAD_VARARGS_DOWN 0

#define FUNCTION_ARG_REGNO_P(N) ((N) >= (FIRST_ARG_REGNUM) && (N) <= (LAST_ARG_REGNUM))

/* Scalar Return */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
        (spu_function_value((VALTYPE),(FUNC)))

#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, FIRST_RETURN_REGNUM)

#define FUNCTION_VALUE_REGNO_P(N) ((N) >= (FIRST_RETURN_REGNUM) && (N) <= (LAST_RETURN_REGNUM))


/* Machine-specific symbol_ref flags.  */
#define SYMBOL_FLAG_ALIGN1	(SYMBOL_FLAG_MACH_DEP << 0)

/* Aggregate Return */

#define DEFAULT_PCC_STRUCT_RETURN 0


/* Function Entry */

#define EXIT_IGNORE_STACK 0

#define EPILOGUE_USES(REGNO) ((REGNO)==1 ? 1 : 0)


/* Profiling */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  spu_function_profiler ((FILE), (LABELNO));

#define NO_PROFILE_COUNTERS 1

#define PROFILE_BEFORE_PROLOGUE 1


/* Trampolines */

#define TRAMPOLINE_SIZE (TARGET_LARGE_MEM ? 20 : 16)

#define TRAMPOLINE_ALIGNMENT 128

/* Addressing Modes */

#define CONSTANT_ADDRESS_P(X)   spu_constant_address_p(X)

#define MAX_REGS_PER_ADDRESS 2

#define LEGITIMIZE_RELOAD_ADDRESS(AD, MODE, OPNUM, TYPE, IND, WIN)	\
do {									\
  rtx new_rtx = spu_legitimize_reload_address (AD, MODE, OPNUM,		\
					       (int)(TYPE));		\
  if (new_rtx)								\
    {									\
      (AD) = new_rtx;							\
      goto WIN;								\
    }									\
} while (0)


/* Costs */

#define BRANCH_COST(speed_p, predictable_p) spu_branch_cost

#define SLOW_BYTE_ACCESS 0

#define MOVE_RATIO(speed) ((speed)? 32 : 4)

#define NO_FUNCTION_CSE 1


/* Sections */

#define TEXT_SECTION_ASM_OP ".text"

#define DATA_SECTION_ASM_OP ".data"

#define JUMP_TABLES_IN_TEXT_SECTION 1


/* PIC */
#define PIC_OFFSET_TABLE_REGNUM 126


/* File Framework */

#define ASM_APP_ON ""

#define ASM_APP_OFF ""


/* Uninitialized Data */
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (ROUNDED)))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%d\n", (ROUNDED)))


/* Label Output */
#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  asm_fprintf (FILE, "%U%s", default_strip_name_encoding (NAME))

#define ASM_OUTPUT_SYMBOL_REF(FILE, X) \
  do							\
    {							\
      tree decl;					\
      assemble_name (FILE, XSTR ((X), 0));		\
      if ((decl = SYMBOL_REF_DECL ((X))) != 0		\
	  && TREE_CODE (decl) == VAR_DECL		\
	  && TYPE_ADDR_SPACE (TREE_TYPE (decl)))	\
	fputs ("@ppu", FILE);				\
    } while (0)


/* Instruction Output */
#define REGISTER_NAMES \
{"$lr", "$sp", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9", "$10", "$11", "$12", "$13", "$14", "$15", \
 "$16", "$17", "$18", "$19", "$20", "$21", "$22", "$23", "$24", "$25", "$26", "$27", "$28", "$29", "$30", "$31", \
 "$32", "$33", "$34", "$35", "$36", "$37", "$38", "$39", "$40", "$41", "$42", "$43", "$44", "$45", "$46", "$47", \
 "$48", "$49", "$50", "$51", "$52", "$53", "$54", "$55", "$56", "$57", "$58", "$59", "$60", "$61", "$62", "$63", \
 "$64", "$65", "$66", "$67", "$68", "$69", "$70", "$71", "$72", "$73", "$74", "$75", "$76", "$77", "$78", "$79", \
 "$80", "$81", "$82", "$83", "$84", "$85", "$86", "$87", "$88", "$89", "$90", "$91", "$92", "$93", "$94", "$95", \
 "$96", "$97", "$98", "$99", "$100", "$101", "$102", "$103", "$104", "$105", "$106", "$107", "$108", "$109", "$110", "$111", \
 "$112", "$113", "$114", "$115", "$116", "$117", "$118", "$119", "$120", "$121", "$122", "$123", "$124", "$125", "$126", "$127", \
 "$vfp", "$vap", "hbr" \
}

#define PRINT_OPERAND(FILE, X, CODE)  print_operand(FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
 print_operand_address (FILE, ADDR)

#define LOCAL_LABEL_PREFIX "."

#define USER_LABEL_PREFIX ""

#define ASM_COMMENT_START "#"


/* Dispatch Tables */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.word .L%d-.L%d\n", VALUE, REL)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.word .L%d\n", VALUE)


/* Alignment Output */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  do { if (LOG!=0) fprintf (FILE, "\t.align\t%d\n", (LOG)); } while (0)


/* Misc */

#define CASE_VECTOR_MODE SImode

#define MOVE_MAX 16 

#define STORE_FLAG_VALUE -1

#define Pmode SImode

#define FUNCTION_MODE QImode

#define NO_IMPLICIT_EXTERN_C 1


/* Address spaces.  */
#define ADDR_SPACE_EA	1


/* Builtins.  */

enum spu_builtin_type
{
  B_INSN,
  B_JUMP,
  B_BISLED,
  B_CALL,
  B_HINT,
  B_OVERLOAD,
  B_INTERNAL
};

struct spu_builtin_description
{
  int fcode;
  int icode;
  const char *name;
  enum spu_builtin_type type;

  /* The first element of parm is always the return type.  The rest
     are a zero terminated list of parameters.  */
  int parm[5];
};

extern struct spu_builtin_description spu_builtins[];

