/* Definitions of target machine for GNU compiler.  
   Vitesse IQ2000 processors
   Copyright (C) 2003-2017 Free Software Foundation, Inc.

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

/* Driver configuration.  */

/* A generic LIB_SPEC with -leval and --*group tacked on.  */
#undef  LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:--start-group -lc -leval -lgcc --end-group}}"

#undef STARTFILE_SPEC
#undef ENDFILE_SPEC

#undef  LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
		   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"


/* Run-time target specifications.  */

#define TARGET_CPU_CPP_BUILTINS()               \
  do                                            \
    {                                           \
      builtin_define ("__iq2000__"); 		\
      builtin_assert ("cpu=iq2000"); 		\
      builtin_assert ("machine=iq2000");	\
    }                                           \
  while (0)

/* Macros used in the machine description to test the flags.  */

#define TARGET_STATS		0

#define TARGET_DEBUG_MODE	0
#define TARGET_DEBUG_A_MODE	0
#define TARGET_DEBUG_B_MODE	0
#define TARGET_DEBUG_C_MODE	0
#define TARGET_DEBUG_D_MODE	0

#ifndef IQ2000_ISA_DEFAULT
#define IQ2000_ISA_DEFAULT 1
#endif

/* Storage Layout.  */

#define BITS_BIG_ENDIAN 		0
#define BYTES_BIG_ENDIAN 		1 
#define WORDS_BIG_ENDIAN 		1
#define BITS_PER_WORD 			32
#define MAX_BITS_PER_WORD 		64
#define UNITS_PER_WORD 			4
#define MIN_UNITS_PER_WORD 		4
#define POINTER_SIZE 			32

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   We promote any value smaller than SImode up to SImode.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)		\
    (MODE) = SImode;

#define PARM_BOUNDARY 32

#define STACK_BOUNDARY 64

#define FUNCTION_BOUNDARY 32

#define BIGGEST_ALIGNMENT 64

#undef  DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define EMPTY_FIELD_BOUNDARY 32

#define STRUCTURE_SIZE_BOUNDARY 8

#define STRICT_ALIGNMENT 1

#define PCC_BITFIELD_TYPE_MATTERS 1


/* Layout of Source Language Data Types.  */

#define INT_TYPE_SIZE 		32
#define SHORT_TYPE_SIZE 	16
#define LONG_TYPE_SIZE 		32
#define LONG_LONG_TYPE_SIZE 	64
#define CHAR_TYPE_SIZE		BITS_PER_UNIT
#define FLOAT_TYPE_SIZE 	32
#define DOUBLE_TYPE_SIZE 	64
#define LONG_DOUBLE_TYPE_SIZE	64
#define DEFAULT_SIGNED_CHAR	1

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD


/* Register Basics.  */

/* On the IQ2000, we have 32 integer registers.  */
#define FIRST_PSEUDO_REGISTER 33

#define FIXED_REGISTERS							\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1			\
}

#define CALL_USED_REGISTERS						\
{									\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1			\
}


/* Order of allocation of registers.  */

#define REG_ALLOC_ORDER							\
{  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,	\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31	\
}


/* How Values Fit in Registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define AVOID_CCMODE_COPIES


/* Register Classes.  */

enum reg_class
{
  NO_REGS,			/* No registers in set.  */
  GR_REGS,			/* Integer registers.  */
  ALL_REGS,			/* All registers.  */
  LIM_REG_CLASSES		/* Max value + 1.  */
};

#define GENERAL_REGS GR_REGS

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES						\
{								\
  "NO_REGS",							\
  "GR_REGS",							\
  "ALL_REGS"							\
}

#define REG_CLASS_CONTENTS					\
{								\
  { 0x00000000, 0x00000000 },	/* No registers,  */		\
  { 0xffffffff, 0x00000000 },	/* Integer registers.  */	\
  { 0xffffffff, 0x00000001 }	/* All registers.  */		\
}

#define REGNO_REG_CLASS(REGNO) \
((REGNO) <= GP_REG_LAST + 1 ? GR_REGS : NO_REGS)

#define BASE_REG_CLASS  (GR_REGS)

#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_INDEX_P(regno)	0

#define PREFERRED_RELOAD_CLASS(X,CLASS)				\
  ((CLASS) != ALL_REGS						\
   ? (CLASS)							\
   : ((GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT		\
       || GET_MODE_CLASS (GET_MODE (X)) == MODE_COMPLEX_FLOAT)	\
      ? (GR_REGS)						\
      : ((GET_MODE_CLASS (GET_MODE (X)) == MODE_INT		\
	  || GET_MODE (X) == VOIDmode)				\
	 ? (GR_REGS)						\
	 : (CLASS))))


/* Basic Stack Layout.  */

#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 0

#define STARTING_FRAME_OFFSET						\
  (crtl->outgoing_args_size)

/* Use the default value zero.  */
/* #define STACK_POINTER_OFFSET 0 */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* The return address for the current frame is in r31 if this is a leaf
   function.  Otherwise, it is on the stack.  It is at a variable offset
   from sp/fp/ap, so we define a fake hard register rap which is a
   pointer to the return address on the stack.  This always gets eliminated
   during reload to be either the frame pointer or the stack pointer plus
   an offset.  */

#define RETURN_ADDR_RTX(count, frame)                                   \
  (((count) == 0)                                                       \
   ? (leaf_function_p ()                                                \
      ? gen_rtx_REG (Pmode, GP_REG_FIRST + 31)                          \
      : gen_rtx_MEM (Pmode, gen_rtx_REG (Pmode,                         \
                                         RETURN_ADDRESS_POINTER_REGNUM))) \
    : (rtx) 0)

/* Before the prologue, RA lives in r31.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, GP_REG_FIRST + 31)


/* Register That Address the Stack Frame.  */

#define STACK_POINTER_REGNUM 		(GP_REG_FIRST + 29)
#define FRAME_POINTER_REGNUM 		(GP_REG_FIRST + 1)
#define HARD_FRAME_POINTER_REGNUM 	(GP_REG_FIRST + 27)
#define ARG_POINTER_REGNUM 		GP_REG_FIRST
#define RETURN_ADDRESS_POINTER_REGNUM	RAP_REG_NUM
#define STATIC_CHAIN_REGNUM 		(GP_REG_FIRST + 2)


/* Eliminating the Frame Pointer and the Arg Pointer.  */

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM},			\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, GP_REG_FIRST + 31},			\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			 \
        (OFFSET) = iq2000_initial_elimination_offset ((FROM), (TO))

/* Passing Function Arguments on the Stack.  */

/* #define PUSH_ROUNDING(BYTES) 0 */

#define ACCUMULATE_OUTGOING_ARGS 1

#define REG_PARM_STACK_SPACE(FNDECL) 0

#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1


/* Function Arguments in Registers.  */

#define MAX_ARGS_IN_REGISTERS 8

typedef struct iq2000_args
{
  int gp_reg_found;		/* Whether a gp register was found yet.  */
  unsigned int arg_number;	/* Argument number.  */
  unsigned int arg_words;	/* # total words the arguments take.  */
  unsigned int fp_arg_words;	/* # words for FP args (IQ2000_EABI only).  */
  int last_arg_fp;		/* Nonzero if last arg was FP (EABI only).  */
  int fp_code;			/* Mode of FP arguments.  */
  unsigned int num_adjusts;	/* Number of adjustments made.  */
				/* Adjustments made to args pass in regs.  */
  rtx adjust[MAX_ARGS_IN_REGISTERS * 2];
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  init_cumulative_args (& CUM, FNTYPE, LIBNAME)				\

#define FUNCTION_ARG_PADDING(MODE, TYPE)				\
  (! BYTES_BIG_ENDIAN							\
   ? upward								\
   : (((MODE) == BLKmode						\
       ? ((TYPE) && TREE_CODE (TYPE_SIZE (TYPE)) == INTEGER_CST		\
	  && int_size_in_bytes (TYPE) < (PARM_BOUNDARY / BITS_PER_UNIT))\
       : (GET_MODE_BITSIZE (MODE) < PARM_BOUNDARY			\
	  && (GET_MODE_CLASS (MODE) == MODE_INT)))			\
      ? downward : upward))

#define FUNCTION_ARG_REGNO_P(N)						\
  (((N) >= GP_ARG_FIRST && (N) <= GP_ARG_LAST))			


/* On the IQ2000, R2 and R3 are the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) iq2000_function_value_regno_p (N)


/* How Large Values are Returned.  */

#define DEFAULT_PCC_STRUCT_RETURN 0

/* Function Entry and Exit.  */

#define EXIT_IGNORE_STACK 1


/* Generating Code for Profiling.  */

#define FUNCTION_PROFILER(FILE, LABELNO)				\
{									\
  fprintf (FILE, "\t.set\tnoreorder\n");				\
  fprintf (FILE, "\t.set\tnoat\n");					\
  fprintf (FILE, "\tmove\t%s,%s\t\t# save current return address\n",	\
	   reg_names[GP_REG_FIRST + 1], reg_names[GP_REG_FIRST + 31]);	\
  fprintf (FILE, "\tjal\t_mcount\n");					\
  fprintf (FILE,							\
	   "\t%s\t%s,%s,%d\t\t# _mcount pops 2 words from  stack\n",	\
	   "subu",							\
	   reg_names[STACK_POINTER_REGNUM],				\
	   reg_names[STACK_POINTER_REGNUM],				\
	   Pmode == DImode ? 16 : 8);					\
  fprintf (FILE, "\t.set\treorder\n");					\
  fprintf (FILE, "\t.set\tat\n");					\
}


/* Trampolines for Nested Functions.  */

#define TRAMPOLINE_CODE_SIZE  (8*4)
#define TRAMPOLINE_SIZE       (TRAMPOLINE_CODE_SIZE + 2*GET_MODE_SIZE (Pmode))
#define TRAMPOLINE_ALIGNMENT  GET_MODE_ALIGNMENT (Pmode)


/* Addressing Modes.  */

#define CONSTANT_ADDRESS_P(X)						\
  (   (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
    || GET_CODE (X) == CONST_INT || GET_CODE (X) == HIGH		\
    || (GET_CODE (X) == CONST)))

#define MAX_REGS_PER_ADDRESS 1

#define REG_OK_FOR_INDEX_P(X) 0


/* Describing Relative Costs of Operations.  */

#define REGISTER_MOVE_COST(MODE, FROM, TO)	2

#define MEMORY_MOVE_COST(MODE,CLASS,TO_P)	\
  (TO_P ? 2 : 16)

#define BRANCH_COST(speed_p, predictable_p) 2

#define SLOW_BYTE_ACCESS 1

#define NO_FUNCTION_CSE 1

#define ADJUST_COST(INSN,LINK,DEP_INSN,COST)				\
  if (REG_NOTE_KIND (LINK) != 0)					\
    (COST) = 0; /* Anti or output dependence.  */


/* Dividing the output into sections.  */

#define TEXT_SECTION_ASM_OP	"\t.text"	/* Instructions.  */

#define DATA_SECTION_ASM_OP	"\t.data"	/* Large data.  */


/* The Overall Framework of an Assembler File.  */

#define ASM_COMMENT_START " #"

#define ASM_APP_ON "#APP\n"

#define ASM_APP_OFF "#NO_APP\n"


/* Output and Generation of Labels.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long) (NUM))

#define GLOBAL_ASM_OP "\t.globl\t"


/* Output of Assembler Instructions.  */

#define REGISTER_NAMES							\
{									\
 "%0",   "%1",   "%2",   "%3",   "%4",   "%5",   "%6",   "%7",		\
 "%8",   "%9",   "%10",  "%11",  "%12",  "%13",  "%14",  "%15",		\
 "%16",  "%17",  "%18",  "%19",  "%20",  "%21",  "%22",  "%23",		\
 "%24",  "%25",  "%26",  "%27",  "%28",  "%29",  "%30",  "%31",  "%rap"	\
}

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "%0",	 0 + GP_REG_FIRST },					\
  { "%1",	 1 + GP_REG_FIRST },					\
  { "%2",	 2 + GP_REG_FIRST },					\
  { "%3",	 3 + GP_REG_FIRST },					\
  { "%4",	 4 + GP_REG_FIRST },					\
  { "%5",	 5 + GP_REG_FIRST },					\
  { "%6",	 6 + GP_REG_FIRST },					\
  { "%7",	 7 + GP_REG_FIRST },					\
  { "%8",	 8 + GP_REG_FIRST },					\
  { "%9",	 9 + GP_REG_FIRST },					\
  { "%10",	10 + GP_REG_FIRST },					\
  { "%11",	11 + GP_REG_FIRST },					\
  { "%12",	12 + GP_REG_FIRST },					\
  { "%13",	13 + GP_REG_FIRST },					\
  { "%14",	14 + GP_REG_FIRST },					\
  { "%15",	15 + GP_REG_FIRST },					\
  { "%16",	16 + GP_REG_FIRST },					\
  { "%17",	17 + GP_REG_FIRST },					\
  { "%18",	18 + GP_REG_FIRST },					\
  { "%19",	19 + GP_REG_FIRST },					\
  { "%20",	20 + GP_REG_FIRST },					\
  { "%21",	21 + GP_REG_FIRST },					\
  { "%22",	22 + GP_REG_FIRST },					\
  { "%23",	23 + GP_REG_FIRST },					\
  { "%24",	24 + GP_REG_FIRST },					\
  { "%25",	25 + GP_REG_FIRST },					\
  { "%26",	26 + GP_REG_FIRST },					\
  { "%27",	27 + GP_REG_FIRST },					\
  { "%28",	28 + GP_REG_FIRST },					\
  { "%29",	29 + GP_REG_FIRST },					\
  { "%30",	27 + GP_REG_FIRST },					\
  { "%31",	31 + GP_REG_FIRST },					\
  { "%rap",	32 + GP_REG_FIRST },					\
}

/* Check if the current insn needs a nop in front of it
   because of load delays, and also update the delay slot statistics.  */

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)			\
  final_prescan_insn (INSN, OPVEC, NOPERANDS)

#define DBR_OUTPUT_SEQEND(STREAM)					\
do									\
  {									\
    fputs ("\n", STREAM);						\
  }									\
while (0)

#define LOCAL_LABEL_PREFIX	"$"

#define USER_LABEL_PREFIX	""


/* Output of dispatch tables.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do									\
    {									\
      fprintf (STREAM, "\t%s\t%sL%d\n",					\
	       Pmode == DImode ? ".dword" : ".word",			\
	       LOCAL_LABEL_PREFIX, VALUE);				\
    }									\
  while (0)

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "\t%s\t%sL%d\n",					\
	   Pmode == DImode ? ".dword" : ".word",			\
	   LOCAL_LABEL_PREFIX,						\
	   VALUE)


/* Assembler Commands for Alignment.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM,SIZE)					\
  fprintf (STREAM, "\t.space\t" HOST_WIDE_INT_PRINT_UNSIGNED "\n",	\
           (unsigned HOST_WIDE_INT)(SIZE))

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
  if ((LOG) != 0)                       				\
    fprintf (STREAM, "\t.balign %d\n", 1<<(LOG))


/* Macros Affecting all Debug Formats.  */

#define DEBUGGER_AUTO_OFFSET(X)  \
  iq2000_debugger_offset (X, (HOST_WIDE_INT) 0)

#define DEBUGGER_ARG_OFFSET(OFFSET, X)  \
  iq2000_debugger_offset (X, (HOST_WIDE_INT) OFFSET)

#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#define DWARF2_DEBUGGING_INFO 1


/* Miscellaneous Parameters.  */

#define CASE_VECTOR_MODE SImode

#define WORD_REGISTER_OPERATIONS 1

#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

#define MOVE_MAX 4

#define MAX_MOVE_MAX 8

#define SHIFT_COUNT_TRUNCATED 1

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define STORE_FLAG_VALUE 1

#define Pmode SImode

#define FUNCTION_MODE SImode

/* IQ2000 external variables defined in iq2000.c.  */

/* Comparison type.  */
enum cmp_type
{
  CMP_SI,				/* Compare four byte integers.  */
  CMP_DI,				/* Compare eight byte integers.  */
  CMP_SF,				/* Compare single precision floats.  */
  CMP_DF,				/* Compare double precision floats.  */
  CMP_MAX				/* Max comparison type.  */
};

/* Types of delay slot.  */
enum delay_type
{
  DELAY_NONE,				/* No delay slot.  */
  DELAY_LOAD,				/* Load from memory delay.  */
  DELAY_FCMP				/* Delay after doing c.<xx>.{d,s}.  */
};

/* Recast the cpu class to be the cpu attribute.  */
#define iq2000_cpu_attr ((enum attr_cpu) iq2000_tune)

#define BITMASK_UPPER16	((unsigned long) 0xffff << 16)	/* 0xffff0000 */
#define BITMASK_LOWER16	((unsigned long) 0xffff)	/* 0x0000ffff */


#define GENERATE_BRANCHLIKELY  (ISA_HAS_BRANCHLIKELY)

/* Macros to decide whether certain features are available or not,
   depending on the instruction set architecture level.  */

#define BRANCH_LIKELY_P()	GENERATE_BRANCHLIKELY

/* ISA has branch likely instructions.  */
#define ISA_HAS_BRANCHLIKELY	(iq2000_isa == 1)


#undef ASM_SPEC


/* The mapping from gcc register number to DWARF 2 CFA column number.  */
#define DWARF_FRAME_REGNUM(REG)        (REG)

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN (GP_REG_FIRST + 31)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + GP_ARG_FIRST : INVALID_REGNUM)

/* The EH_RETURN_STACKADJ_RTX macro returns RTL which describes the
   location used to store the amount to adjust the stack.  This is
   usually a register that is available from end of the function's body
   to the end of the epilogue. Thus, this cannot be a register used as a
   temporary by the epilogue.

   This must be an integer register.  */
#define EH_RETURN_STACKADJ_REGNO        3
#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, EH_RETURN_STACKADJ_REGNO)

/* The EH_RETURN_HANDLER_RTX macro returns RTL which describes the
   location used to store the address the processor should jump to
   catch exception.  This is usually a registers that is available from
   end of the function's body to the end of the epilogue. Thus, this
   cannot be a register used as a temporary by the epilogue.

   This must be an address register.  */
#define EH_RETURN_HANDLER_REGNO         26
#define EH_RETURN_HANDLER_RTX           \
        gen_rtx_REG (Pmode, EH_RETURN_HANDLER_REGNO)

/* Offsets recorded in opcodes are a multiple of this alignment factor.  */
#define DWARF_CIE_DATA_ALIGNMENT 4

/* For IQ2000, width of a floating point register.  */
#define UNITS_PER_FPREG 4

/* Force right-alignment for small varargs in 32 bit little_endian mode */

#define PAD_VARARGS_DOWN !BYTES_BIG_ENDIAN

/* Internal macros to classify a register number as to whether it's a
   general purpose register, a floating point register, a
   multiply/divide register, or a status register.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)

#define RAP_REG_NUM   32
#define AT_REGNUM	(GP_REG_FIRST + 1)

#define GP_REG_P(REGNO)	\
  ((unsigned int) ((int) (REGNO) - GP_REG_FIRST) < GP_REG_NUM)

/* IQ2000 registers used in prologue/epilogue code when the stack frame
   is larger than 32K bytes.  These registers must come from the
   scratch register set, and not used for passing and returning
   arguments and any other information used in the calling sequence.  */

#define IQ2000_TEMP1_REGNUM (GP_REG_FIRST + 12)
#define IQ2000_TEMP2_REGNUM (GP_REG_FIRST + 13)

/* This macro is used later on in the file.  */
#define GR_REG_CLASS_P(CLASS)						\
  ((CLASS) == GR_REGS)

#define SMALL_INT(X) ((unsigned HOST_WIDE_INT) (INTVAL (X) + 0x8000) < 0x10000)
#define SMALL_INT_UNSIGNED(X) ((unsigned HOST_WIDE_INT) (INTVAL (X)) < 0x10000)

/* Certain machines have the property that some registers cannot be
   copied to some other registers without using memory.  Define this
   macro on those machines to be a C expression that is nonzero if
   objects of mode MODE in registers of CLASS1 can only be copied to
   registers of class CLASS2 by storing a register of CLASS1 into
   memory and loading that memory location into a register of CLASS2.

   Do not define this macro if its value would always be zero.  */

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_UNITS(mode, size)						\
  ((GET_MODE_SIZE (mode) + (size) - 1) / (size))

/* If defined, gives a class of registers that cannot be used as the
   operand of a SUBREG that changes the mode of the object illegally.  */

#define CLASS_CANNOT_CHANGE_MODE 0

/* Defines illegal mode changes for CLASS_CANNOT_CHANGE_MODE.  */

#define CLASS_CANNOT_CHANGE_MODE_P(FROM,TO) \
  (GET_MODE_SIZE (FROM) != GET_MODE_SIZE (TO))

/* Make sure 4 words are always allocated on the stack.  */

#ifndef STACK_ARGS_ADJUST
#define STACK_ARGS_ADJUST(SIZE)						\
  {									\
    if (SIZE.constant < 4 * UNITS_PER_WORD)				\
      SIZE.constant = 4 * UNITS_PER_WORD;				\
  }
#endif


/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN (GP_REG_FIRST + 2)

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST (GP_REG_FIRST + 4)
#define GP_ARG_LAST  (GP_REG_FIRST + 11)

#define MAX_ARGS_IN_REGISTERS	8


/* Tell prologue and epilogue if register REGNO should be saved / restored.  */

#define MUST_SAVE_REGISTER(regno) \
  ((df_regs_ever_live_p (regno) && !call_used_regs[regno])		\
  || (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)	\
   || (regno == (GP_REG_FIRST + 31) && df_regs_ever_live_p (GP_REG_FIRST + 31)))

/* ALIGN FRAMES on double word boundaries */
#ifndef IQ2000_STACK_ALIGN
#define IQ2000_STACK_ALIGN(LOC) (((LOC) + 7) & ~7)
#endif


/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define BASE_REG_P(regno, mode)					\
  (GP_REG_P (regno))

#define GP_REG_OR_PSEUDO_STRICT_P(regno, mode)				    \
  BASE_REG_P((regno < FIRST_PSEUDO_REGISTER) ? regno : reg_renumber[regno], \
	     (mode))

#define GP_REG_OR_PSEUDO_NONSTRICT_P(regno, mode) \
  (((regno) >= FIRST_PSEUDO_REGISTER) || (BASE_REG_P ((regno), (mode))))

#define REGNO_MODE_OK_FOR_BASE_P(regno, mode) \
  GP_REG_OR_PSEUDO_STRICT_P ((int) (regno), (mode))

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  iq2000_reg_mode_ok_for_base_p (X, MODE, 0)
#else
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  iq2000_reg_mode_ok_for_base_p (X, MODE, 1)
#endif

#if 1
#define GO_PRINTF(x)	fprintf (stderr, (x))
#define GO_PRINTF2(x,y)	fprintf (stderr, (x), (y))
#define GO_DEBUG_RTX(x) debug_rtx (x)

#else
#define GO_PRINTF(x)
#define GO_PRINTF2(x,y)
#define GO_DEBUG_RTX(x)
#endif

/* If defined, modifies the length assigned to instruction INSN as a
   function of the context in which it is used.  LENGTH is an lvalue
   that contains the initially computed length of the insn and should
   be updated with the correct length of the insn.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  ((LENGTH) = iq2000_adjust_insn_length ((INSN), (LENGTH)))




/* How to tell the debugger about changes of source files.  */

#ifndef SET_FILE_NUMBER
#define SET_FILE_NUMBER() ++ num_source_filenames
#endif

/* This is how to output a note the debugger telling it the line number
   to which the following sequence of instructions corresponds.  */

#ifndef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)
#endif


/* Default to -G 8 */
#ifndef IQ2000_DEFAULT_GVALUE
#define IQ2000_DEFAULT_GVALUE 8
#endif

#define SDATA_SECTION_ASM_OP	"\t.sdata"	/* Small data.  */


/* Which instruction set architecture to use.  */
extern int iq2000_isa;

enum iq2000_builtins
{
  IQ2000_BUILTIN_ADO16,
  IQ2000_BUILTIN_CFC0,
  IQ2000_BUILTIN_CFC1,
  IQ2000_BUILTIN_CFC2,
  IQ2000_BUILTIN_CFC3,
  IQ2000_BUILTIN_CHKHDR,
  IQ2000_BUILTIN_CTC0,
  IQ2000_BUILTIN_CTC1,
  IQ2000_BUILTIN_CTC2,
  IQ2000_BUILTIN_CTC3,
  IQ2000_BUILTIN_LU,
  IQ2000_BUILTIN_LUC32L,
  IQ2000_BUILTIN_LUC64,
  IQ2000_BUILTIN_LUC64L,
  IQ2000_BUILTIN_LUK,
  IQ2000_BUILTIN_LULCK,
  IQ2000_BUILTIN_LUM32,
  IQ2000_BUILTIN_LUM32L,
  IQ2000_BUILTIN_LUM64,
  IQ2000_BUILTIN_LUM64L,
  IQ2000_BUILTIN_LUR,
  IQ2000_BUILTIN_LURL,
  IQ2000_BUILTIN_MFC0,
  IQ2000_BUILTIN_MFC1,
  IQ2000_BUILTIN_MFC2,
  IQ2000_BUILTIN_MFC3,
  IQ2000_BUILTIN_MRGB,
  IQ2000_BUILTIN_MTC0,
  IQ2000_BUILTIN_MTC1,
  IQ2000_BUILTIN_MTC2,
  IQ2000_BUILTIN_MTC3,
  IQ2000_BUILTIN_PKRL,
  IQ2000_BUILTIN_RAM,
  IQ2000_BUILTIN_RB,
  IQ2000_BUILTIN_RX,
  IQ2000_BUILTIN_SRRD,
  IQ2000_BUILTIN_SRRDL,
  IQ2000_BUILTIN_SRULC,
  IQ2000_BUILTIN_SRULCK,
  IQ2000_BUILTIN_SRWR,
  IQ2000_BUILTIN_SRWRU,
  IQ2000_BUILTIN_TRAPQF,
  IQ2000_BUILTIN_TRAPQFL,
  IQ2000_BUILTIN_TRAPQN,
  IQ2000_BUILTIN_TRAPQNE,
  IQ2000_BUILTIN_TRAPRE,
  IQ2000_BUILTIN_TRAPREL,
  IQ2000_BUILTIN_WB,
  IQ2000_BUILTIN_WBR,
  IQ2000_BUILTIN_WBU,
  IQ2000_BUILTIN_WX,
  IQ2000_BUILTIN_SYSCALL
};
