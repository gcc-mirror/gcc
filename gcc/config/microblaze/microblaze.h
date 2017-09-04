/* Definitions of target machine for GNU compiler for Xilinx MicroBlaze.
   Copyright (C) 2009-2017 Free Software Foundation, Inc.

   Contributed by Michael Eager <eager@eagercon.com>.

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

/* Standard GCC variables that we reference.  */

/* MicroBlaze external variables defined in microblaze.c.  */

/* Which pipeline to schedule for.  */
enum pipeline_type
{
  MICROBLAZE_PIPE_3 = 0,
  MICROBLAZE_PIPE_5 = 1
};

#define MICROBLAZE_MASK_NO_UNSAFE_DELAY         0x00000001

/* print_operand punctuation chars */
extern char microblaze_print_operand_punct[];

/* # bytes of data/sdata cutoff */
extern int microblaze_section_threshold;

/* Map register # to debug register # */
extern int microblaze_dbx_regno[];

extern int microblaze_no_unsafe_delay;
extern int microblaze_has_clz;
extern enum pipeline_type microblaze_pipe;

#define OBJECT_FORMAT_ELF

#if TARGET_BIG_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT    0
#define TARGET_ENDIAN_OPTION     "mbig-endian"
#else
#define TARGET_ENDIAN_DEFAULT    MASK_LITTLE_ENDIAN
#define TARGET_ENDIAN_OPTION     "mlittle-endian"
#endif

/* Default target_flags if no switches are specified  */
#define TARGET_DEFAULT      (MASK_SOFT_MUL | MASK_SOFT_DIV | MASK_SOFT_FLOAT \
                             | TARGET_ENDIAN_DEFAULT)

/* Do we have CLZ?  */
#define TARGET_HAS_CLZ      (TARGET_PATTERN_COMPARE && microblaze_has_clz)

/* The default is to support PIC.  */
#define TARGET_SUPPORTS_PIC 1

/* The default is to not need GOT for TLS.  */
#define TLS_NEEDS_GOT 0

/* What is the default setting for -mcpu= . We set it to v4.00.a even though 
   we are actually ahead. This is safest version that has generate code 
   compatible for the original ISA */
#define MICROBLAZE_DEFAULT_CPU      "v4.00.a"

/* Macros to decide whether certain features are available or not,
   depending on the instruction set architecture level.  */

#define DRIVER_SELF_SPECS    				\
	"%{mxl-soft-mul:%<mno-xl-soft-mul}", 		\
	"%{mno-xl-barrel-shift:%<mxl-barrel-shift}", 	\
	"%{mno-xl-pattern-compare:%<mxl-pattern-compare}", \
	"%{mxl-soft-div:%<mno-xl-soft-div}", 		\
	"%{mxl-reorder:%<mno-xl-reorder}", 		\
	"%{msoft-float:%<mhard-float}"

/* Tell collect what flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-Bn"
#endif

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS() microblaze_cpp_define (pfile)

/* Assembler specs.  */

#define TARGET_ASM_SPEC ""

#define ASM_SPEC "\
%(target_asm_spec) \
%{mbig-endian:-EB} \
%{mlittle-endian:-EL}"

/* Extra switches sometimes passed to the linker.  */
/* -xl-mode-xmdstub translated to -Zxl-mode-xmdstub -- deprecated.  */

#define LINK_SPEC "%{shared:-shared} -N -relax \
  %{mbig-endian:-EB --oformat=elf32-microblaze} \
  %{mlittle-endian:-EL --oformat=elf32-microblazeel} \
  %{Zxl-mode-xmdstub:-defsym _TEXT_START_ADDR=0x800} \
  %{mxl-mode-xmdstub:-defsym _TEXT_START_ADDR=0x800} \
  %{mxl-gp-opt:%{G*}} %{!mxl-gp-opt: -G 0} \
  %{!T*: -dT xilinx.ld%s}"

/* Specs for the compiler proper  */

#ifndef CC1_SPEC
#define CC1_SPEC " \
%{G*} \
%(subtarget_cc1_spec) \
%{mxl-multiply-high:-mcpu=v6.00.a} \
"
#endif

#define EXTRA_SPECS							\
  { "target_asm_spec", TARGET_ASM_SPEC },				\
  SUBTARGET_EXTRA_SPECS

/* Local compiler-generated symbols must have a prefix that the assembler
   understands.   */

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"$"
#endif

/* fixed registers.  */
#define MB_ABI_BASE_REGNUM                   0
#define MB_ABI_STACK_POINTER_REGNUM          1
#define MB_ABI_GPRO_REGNUM                   2
#define MB_ABI_GPRW_REGNUM                  13
#define MB_ABI_INTR_RETURN_ADDR_REGNUM      14
#define MB_ABI_SUB_RETURN_ADDR_REGNUM       15
#define MB_ABI_DEBUG_RETURN_ADDR_REGNUM     16
#define MB_ABI_EXCEPTION_RETURN_ADDR_REGNUM 17
#define MB_ABI_ASM_TEMP_REGNUM              18	
/* This is our temp register.  */
#define MB_ABI_FRAME_POINTER_REGNUM         19
#define MB_ABI_PIC_ADDR_REGNUM              20
#define MB_ABI_PIC_FUNC_REGNUM              21
/* Volatile registers.  */
#define MB_ABI_INT_RETURN_VAL_REGNUM         3
#define MB_ABI_INT_RETURN_VAL2_REGNUM        4
#define MB_ABI_FIRST_ARG_REGNUM              5
#define MB_ABI_LAST_ARG_REGNUM              10
#define MB_ABI_MAX_ARG_REGS                 (MB_ABI_LAST_ARG_REGNUM 	\
					     - MB_ABI_FIRST_ARG_REGNUM + 1)
#define MB_ABI_STATIC_CHAIN_REGNUM           3
#define MB_ABI_TEMP1_REGNUM                 11
#define MB_ABI_TEMP2_REGNUM                 12
#define MB_ABI_MSR_SAVE_REG                 11	
/* Volatile register used to save MSR in interrupt handlers.  */


/* Debug stuff.  */

/* How to renumber registers for dbx and gdb.  */
#define DBX_REGISTER_NUMBER(REGNO) microblaze_dbx_regno[(REGNO)]

/* Generate DWARF exception handling info.  */
#define DWARF2_UNWIND_INFO 1

/* Don't generate .loc operations.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 0

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN \
	(GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM)

/* Initial state of return address on entry to func = R15.
   Actually, the RA is at R15+8, but gcc doesn't know how 
   to generate this. 
   NOTE:  GDB has a workaround and expects this incorrect value.
   If this is fixed, a corresponding fix to GDB is needed.  */
#define INCOMING_RETURN_ADDR_RTX  			\
  gen_rtx_REG (Pmode, GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM)

/* Specifies the offset from INCOMING_RETURN_ADDR_RTX and the actual return PC.  */
#define RETURN_ADDR_OFFSET (8)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N)					\
  (((N) < 2) ? MB_ABI_FIRST_ARG_REGNUM + (N) : INVALID_REGNUM)

#define MB_EH_STACKADJ_REGNUM  MB_ABI_INT_RETURN_VAL2_REGNUM
#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, MB_EH_STACKADJ_REGNUM)

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL) \
  ((flag_pic || GLOBAL) ? DW_EH_PE_aligned : DW_EH_PE_absptr)

/* Use DWARF 2 debugging information by default.  */
#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Target machine storage layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_LITTLE_ENDIAN == 0)
#define WORDS_BIG_ENDIAN (BYTES_BIG_ENDIAN)
#define BITS_PER_WORD           32
#define UNITS_PER_WORD          4
#define MIN_UNITS_PER_WORD      4
#define INT_TYPE_SIZE           32
#define SHORT_TYPE_SIZE         16
#define LONG_TYPE_SIZE          32
#define LONG_LONG_TYPE_SIZE     64
#define FLOAT_TYPE_SIZE         32
#define DOUBLE_TYPE_SIZE        64
#define LONG_DOUBLE_TYPE_SIZE   64
#define POINTER_SIZE            32
#define PARM_BOUNDARY           32
#define FUNCTION_BOUNDARY       32
#define EMPTY_FIELD_BOUNDARY    32
#define STRUCTURE_SIZE_BOUNDARY 8
#define BIGGEST_ALIGNMENT       32
#define STRICT_ALIGNMENT        1
#define PCC_BITFIELD_TYPE_MATTERS 1

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD						\
	? BITS_PER_WORD							\
	: (ALIGN))

#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

#define LOCAL_ALIGNMENT(TYPE, ALIGN)     				\
    (((TREE_CODE (TYPE) == ARRAY_TYPE 					\
       && TYPE_MODE (TREE_TYPE (TYPE)) == QImode)			\
     && (ALIGN) < BITS_PER_WORD) ? BITS_PER_WORD : (ALIGN))

#define WORD_REGISTER_OPERATIONS 1

#define LOAD_EXTEND_OP(MODE)  ZERO_EXTEND

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)		\
    (MODE) = SImode;

/* Standard register usage.  */

/* On the MicroBlaze, we have 32 integer registers */

#define FIRST_PSEUDO_REGISTER 36

#define FIXED_REGISTERS							\
{									\
  1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,			\
  1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  1, 1, 1, 1 								\
}

#define CALL_USED_REGISTERS						\
{									\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  1, 1, 1, 1								\
}
#define GP_REG_FIRST    0
#define GP_REG_LAST     31
#define GP_REG_NUM      (GP_REG_LAST - GP_REG_FIRST + 1)
#define GP_DBX_FIRST    0

#define ST_REG		32
#define AP_REG_NUM      33
#define RAP_REG_NUM     34
#define FRP_REG_NUM     35

#define GP_REG_P(REGNO) ((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define ST_REG_P(REGNO) ((REGNO) == ST_REG)

#define HARD_REGNO_NREGS(REGNO, MODE)					\
	((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define MODES_TIEABLE_P(MODE1, MODE2)					\
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT ||				\
    GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)			\
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT ||				\
       GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

#define STACK_POINTER_REGNUM   (GP_REG_FIRST + MB_ABI_STACK_POINTER_REGNUM)

#define STACK_POINTER_OFFSET   FIRST_PARM_OFFSET(FNDECL)

/* Base register for access to local variables of the function.  We
   pretend that the frame pointer is
   MB_ABI_INTR_RETURN_ADDR_REGNUM, and then eliminate it
   to HARD_FRAME_POINTER_REGNUM.  We can get away with this because
   rMB_ABI_INTR_RETUREN_ADDR_REGNUM is a fixed
   register(return address for interrupt), and will not be used for
   anything else.  */
   
#define FRAME_POINTER_REGNUM 		FRP_REG_NUM
#define HARD_FRAME_POINTER_REGNUM       \
        (GP_REG_FIRST + MB_ABI_FRAME_POINTER_REGNUM)
#define ARG_POINTER_REGNUM		AP_REG_NUM
#define RETURN_ADDRESS_POINTER_REGNUM	RAP_REG_NUM
#define STATIC_CHAIN_REGNUM             \
        (GP_REG_FIRST + MB_ABI_STATIC_CHAIN_REGNUM)

/* registers used in prologue/epilogue code when the stack frame
   is larger than 32K bytes.  These registers must come from the
   scratch register set, and not used for passing and returning
   arguments and any other information used in the calling sequence
   (such as pic).  */

#define MICROBLAZE_TEMP1_REGNUM         \
        (GP_REG_FIRST + MB_ABI_TEMP1_REGNUM)

#define MICROBLAZE_TEMP2_REGNUM         \
        (GP_REG_FIRST + MB_ABI_TEMP2_REGNUM)

#define NO_FUNCTION_CSE                 1

#define PIC_OFFSET_TABLE_REGNUM   (GP_REG_FIRST + MB_ABI_PIC_ADDR_REGNUM)

enum reg_class
{
  NO_REGS,			/* no registers in set.  */
  GR_REGS,			/* integer registers.  */
  ST_REGS,			/* status register.  */
  ALL_REGS,			/* all registers.  */
  LIM_REG_CLASSES		/* max value + 1.  */
};

#define N_REG_CLASSES 		(int) LIM_REG_CLASSES

#define GENERAL_REGS 		GR_REGS

#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "GR_REGS",								\
  "ST_REGS",								\
  "ALL_REGS"								\
}

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000 },		/* no registers.  */		\
  { 0xffffffff, 0x00000000 },		/* integer registers.  */	\
  { 0x00000000, 0x00000001 },		/* status registers.  */	\
  { 0xffffffff, 0x0000000f }		/* all registers.  */		\
}

extern enum reg_class microblaze_regno_to_class[];

#define REGNO_REG_CLASS(REGNO) 		microblaze_regno_to_class[ (REGNO) ]

#define BASE_REG_CLASS  		GR_REGS

#define INDEX_REG_CLASS 		GR_REGS

#define GR_REG_CLASS_P(CLASS) 		((CLASS) == GR_REGS)

/* REGISTER AND CONSTANT CLASSES */

#define SMALL_INT(X) ((unsigned HOST_WIDE_INT) (INTVAL (X) + 0x8000) < 0x10000)
#define LARGE_INT(X) \
  (INTVAL (X) > 0 && UINTVAL (X) >= 0x80000000 && UINTVAL (X) <= 0xffffffff)
#define PLT_ADDR_P(X) (GET_CODE (X) == UNSPEC && XINT (X,1) == UNSPEC_PLT)
/* Test for a valid operand for a call instruction.
   Don't allow the arg pointer register or virtual regs
   since they may change into reg + const, which the patterns
   can't handle yet.  */
#define CALL_INSN_OP(X) (CONSTANT_ADDRESS_P (X) \
                         || (GET_CODE (X) == REG && X != arg_pointer_rtx\
                             && ! (REGNO (X) >= FIRST_PSEUDO_REGISTER	\
                             && REGNO (X) <= LAST_VIRTUAL_REGISTER)))

/* True if VALUE is a signed 16-bit number.  */
#define SMALL_OPERAND(VALUE) 						\
  ((unsigned HOST_WIDE_INT) (VALUE) + 0x8000 < 0x10000)

/* Constant which cannot be loaded in one instruction.  */
#define LARGE_OPERAND(VALUE)						\
  ((((VALUE) & ~0x0000ffff) != 0)					\
   && (((VALUE) & ~0x0000ffff) != ~0x0000ffff)				\
   && (((VALUE) & 0x0000ffff) != 0					\
       || (((VALUE) & ~2147483647) != 0					\
	   && ((VALUE) & ~2147483647) != ~2147483647)))
	
#define PREFERRED_RELOAD_CLASS(X,CLASS)					\
  ((CLASS) != ALL_REGS							\
   ? (CLASS)							\
   : ((GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT			\
       || GET_MODE_CLASS (GET_MODE (X)) == MODE_COMPLEX_FLOAT)		\
      ? (GR_REGS)			\
      : ((GET_MODE_CLASS (GET_MODE (X)) == MODE_INT			\
	  || GET_MODE (X) == VOIDmode)					\
	 ? (GR_REGS) : (CLASS))))

/* Stack layout; function entry, exit and calling.  */

#define STACK_GROWS_DOWNWARD 1

/* Changed the starting frame offset to including the new link stuff */
#define STARTING_FRAME_OFFSET						\
   (crtl->outgoing_args_size + FIRST_PARM_OFFSET(FNDECL))

/* The return address for the current frame is in r31 if this is a leaf
   function.  Otherwise, it is on the stack.  It is at a variable offset
   from sp/fp/ap, so we define a fake hard register rap which is a
   poiner to the return address on the stack.  This always gets eliminated
   during reload to be either the frame pointer or the stack pointer plus
   an offset.  */

#define RETURN_ADDR_RTX(count, frame)			\
  microblaze_return_addr(count,frame)

extern struct microblaze_frame_info current_frame_info;

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   GP_REG_FIRST + MB_ABI_FRAME_POINTER_REGNUM},	\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, 					\
   GP_REG_FIRST + MB_ABI_FRAME_POINTER_REGNUM},				\
 { RETURN_ADDRESS_POINTER_REGNUM, 					\
   GP_REG_FIRST + MB_ABI_SUB_RETURN_ADDR_REGNUM},			\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, GP_REG_FIRST + MB_ABI_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			 \
        (OFFSET) = microblaze_initial_elimination_offset ((FROM), (TO))

#define ACCUMULATE_OUTGOING_ARGS        1

#define FIRST_PARM_OFFSET(FNDECL)		(UNITS_PER_WORD)

#define ARG_POINTER_CFA_OFFSET(FNDECL)		0

#define REG_PARM_STACK_SPACE(FNDECL)  		(MAX_ARGS_IN_REGISTERS * UNITS_PER_WORD)

#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE)	1

#define STACK_BOUNDARY				32

#define NUM_OF_ARGS				6

#define GP_RETURN				(GP_REG_FIRST + MB_ABI_INT_RETURN_VAL_REGNUM)

#define GP_ARG_FIRST				(GP_REG_FIRST + MB_ABI_FIRST_ARG_REGNUM)
#define GP_ARG_LAST				(GP_REG_FIRST + MB_ABI_LAST_ARG_REGNUM)

#define MAX_ARGS_IN_REGISTERS			MB_ABI_MAX_ARG_REGS

#define LIBCALL_VALUE(MODE)						\
  gen_rtx_REG (								\
	   ((GET_MODE_CLASS (MODE) != MODE_INT				\
	     || GET_MODE_SIZE (MODE) >= 4)				\
	    ? (MODE)							\
	    : SImode), GP_RETURN)

/* 1 if N is a possible register number for a function value.
   On the MicroBlaze, R2 R3 are the only register thus used.
   Currently, R2 are only implemented  here (C has no complex type)  */

#define FUNCTION_VALUE_REGNO_P(N)		((N) == GP_RETURN)

#define FUNCTION_ARG_REGNO_P(N)			(((N) >= GP_ARG_FIRST && (N) <= GP_ARG_LAST))

typedef struct microblaze_args
{
  int gp_reg_found;		/* whether a gp register was found yet */
  int arg_number;		/* argument number */
  int arg_words;		/* # total words the arguments take */
  int fp_arg_words;		/* # words for FP args */
  int last_arg_fp;		/* nonzero if last arg was FP (EABI only) */
  int fp_code;			/* Mode of FP arguments */
  int num_adjusts;		/* number of adjustments made */
  /* Adjustments made to args pass in regs.  */
  /* ??? The size is doubled to work around a bug in the code that sets the 
     adjustments in function_arg.  */
  rtx adjust[MAX_ARGS_IN_REGISTERS * 2];
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS)	\
  init_cumulative_args (&CUM, FNTYPE, LIBNAME)

#define NO_PROFILE_COUNTERS			1

#define FUNCTION_PROFILER(FILE, LABELNO) { \
  {                                        \
    fprintf (FILE, "\tbrki\tr16,_mcount\n");           \
  }                                                    \
 }

#define EXIT_IGNORE_STACK			1

/* 4 insns + 2 words of data.  */
#define TRAMPOLINE_SIZE				(6 * 4)

#define TRAMPOLINE_ALIGNMENT			32

#define REGNO_OK_FOR_BASE_P(regno)		microblaze_regno_ok_for_base_p ((regno), 1)

#define REGNO_OK_FOR_INDEX_P(regno)		microblaze_regno_ok_for_base_p ((regno), 1)

#ifndef REG_OK_STRICT
#define REG_STRICT_FLAG				0
#else
#define REG_STRICT_FLAG				1
#endif

#define REG_OK_FOR_BASE_P(X)    \
  microblaze_regno_ok_for_base_p (REGNO (X), REG_STRICT_FLAG)

#define REG_OK_FOR_INDEX_P(X)   \
  microblaze_regno_ok_for_base_p (REGNO (X), REG_STRICT_FLAG)

#define MAX_REGS_PER_ADDRESS 2


/* Identify valid constant addresses.  Exclude if PIC addr which 
   needs scratch register.  */
#define CONSTANT_ADDRESS_P(X)						\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
    || GET_CODE (X) == CONST_INT 		                        \
    || (GET_CODE (X) == CONST						\
	&& ! (flag_pic && pic_address_needs_scratch (X))))

/* Define this, so that when PIC, reload won't try to reload invalid
   addresses which require two reload registers.  */
#define LEGITIMATE_PIC_OPERAND_P(X)  microblaze_legitimate_pic_operand (X)

#define CASE_VECTOR_MODE			(SImode)

#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR			1
#endif

#define MOVE_MAX				4
#define MAX_MOVE_MAX				8

#define SLOW_BYTE_ACCESS			1

/* sCOND operations return 1.  */
#define STORE_FLAG_VALUE			1

#define SHIFT_COUNT_TRUNCATED			1

/* This results in inefficient code for 64 bit to 32 conversions.
   Something needs to be done about this.  Perhaps not use any 32 bit
   instructions?  Perhaps use PROMOTE_MODE?  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)  1

#define Pmode SImode

#define FUNCTION_MODE   SImode

/* Mode should always be SImode */
#define REGISTER_MOVE_COST(MODE, FROM, TO)			\
  ( GR_REG_CLASS_P (FROM) && GR_REG_CLASS_P (TO) ? 2 		\
   : (FROM) == ST_REGS && GR_REG_CLASS_P (TO) ? 4		\
   : 12)

#define MEMORY_MOVE_COST(MODE,CLASS,TO_P) \
  (4 + memory_move_secondary_cost ((MODE), (CLASS), (TO_P)))

#define BRANCH_COST(speed_p, predictable_p)	2

/* Control the assembler format that we output.  */
#define ASM_APP_ON " #APP\n"
#define ASM_APP_OFF " #NO_APP\n"

#define REGISTER_NAMES {						\
  "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7",		\
  "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "r15",	\
  "r16",  "r17",  "r18",  "r19",  "r20",  "r21",  "r22",  "r23",	\
  "r24",  "r25",  "r26",  "r27",  "r28",  "r29",  "r30",  "r31",	\
  "rmsr", "$ap",  "$rap", "$frp" }

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "r0",	 0 + GP_REG_FIRST },					\
  { "r1",	 1 + GP_REG_FIRST },					\
  { "r2",	 2 + GP_REG_FIRST },					\
  { "r3",	 3 + GP_REG_FIRST },					\
  { "r4",	 4 + GP_REG_FIRST },					\
  { "r5",	 5 + GP_REG_FIRST },					\
  { "r6",	 6 + GP_REG_FIRST },					\
  { "r7",	 7 + GP_REG_FIRST },					\
  { "r8",	 8 + GP_REG_FIRST },					\
  { "r9",	 9 + GP_REG_FIRST },					\
  { "r10",	10 + GP_REG_FIRST },					\
  { "r11",	11 + GP_REG_FIRST },					\
  { "r12",	12 + GP_REG_FIRST },					\
  { "r13",	13 + GP_REG_FIRST },					\
  { "r14",	14 + GP_REG_FIRST },					\
  { "r15",	15 + GP_REG_FIRST },					\
  { "r16",	16 + GP_REG_FIRST },					\
  { "r17",	17 + GP_REG_FIRST },					\
  { "r18",	18 + GP_REG_FIRST },					\
  { "r19",	19 + GP_REG_FIRST },					\
  { "r20",	20 + GP_REG_FIRST },					\
  { "r21",	21 + GP_REG_FIRST },					\
  { "r22",	22 + GP_REG_FIRST },					\
  { "r23",	23 + GP_REG_FIRST },					\
  { "r24",	24 + GP_REG_FIRST },					\
  { "r25",	25 + GP_REG_FIRST },					\
  { "r26",	26 + GP_REG_FIRST },					\
  { "r27",	27 + GP_REG_FIRST },					\
  { "r28",	28 + GP_REG_FIRST },					\
  { "r29",	29 + GP_REG_FIRST },					\
  { "r30",	30 + GP_REG_FIRST },					\
  { "r31",	31 + GP_REG_FIRST },					\
  { "rmsr",     ST_REG}							\
}

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) microblaze_print_operand_punct[CODE]

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)

/* ASM_OUTPUT_ALIGNED_COMMON and ASM_OUTPUT_ALIGNED_LOCAL

   Unfortunately, we still need to set the section explicitly. Somehow,
   our binutils assign .comm and .lcomm variables to the "current" section 
   in the assembly file, rather than where they implicitly belong. We need to
   remove this explicit setting in GCC when binutils can understand sections
   better.  */
#undef	ASM_OUTPUT_ALIGNED_COMMON
#define	ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  if ((SIZE) > 0 && (SIZE) <= INT_MAX					\
      && (int) (SIZE) <= microblaze_section_threshold			\
      && TARGET_XLGPOPT)						\
    {                                                                   \
      switch_to_section (sbss_section);					\
    }									\
  else									\
    {									\
      switch_to_section (bss_section);					\
    }                                                                   \
  fprintf (FILE, "%s", COMMON_ASM_OP);                                  \
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
           (SIZE), (ALIGN) / BITS_PER_UNIT);                            \
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
} while (0)

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define	ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  if ((SIZE) > 0 && (SIZE) <= INT_MAX					\
      && (int) (SIZE) <= microblaze_section_threshold			\
      && TARGET_XLGPOPT)						\
    {                                                                   \
      switch_to_section (sbss_section);					\
    }									\
  else									\
    {									\
      switch_to_section (bss_section);					\
    }                                                                   \
  fprintf (FILE, "%s", LCOMMON_ASM_OP);                                 \
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
           (SIZE), (ALIGN) / BITS_PER_UNIT);                            \
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
} while (0)

#define	ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
do {									\
  ASM_OUTPUT_ALIGNED_LOCAL (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)                     \
{                                                                       \
}

#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR microblaze_elf_asm_constructor

#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR microblaze_elf_asm_destructor

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long)(NUM))

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "\t%s\t%sL%d\n",					\
	   ".gpword",                                                   \
	   LOCAL_LABEL_PREFIX, VALUE)

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
do {									\
  if (flag_pic == 2)                                               \
    fprintf (STREAM, "\t%s\t%sL%d@GOTOFF\n",                            \
	     ".gpword",                                                 \
	     LOCAL_LABEL_PREFIX, VALUE);				\
  else                                                                  \
    fprintf (STREAM, "\t%s\t%sL%d\n",					\
	     ".gpword",                                                 \
	     LOCAL_LABEL_PREFIX, VALUE);				\
} while (0)

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
  fprintf (STREAM, "\t.align\t%d\n", (LOG))

#define ASM_OUTPUT_SKIP(STREAM,SIZE)					\
  fprintf (STREAM, "\t.space\t%lu\n", (SIZE))

#define ASCII_DATA_ASM_OP		"\t.ascii\t"
#define STRING_ASM_OP			"\t.asciz\t"

#undef TARGET_ASM_OUTPUT_IDENT
#define TARGET_ASM_OUTPUT_IDENT microblaze_asm_output_ident

/* Default to -G 8 */
#ifndef MICROBLAZE_DEFAULT_GVALUE
#define MICROBLAZE_DEFAULT_GVALUE 8
#endif

/* Given a decl node or constant node, choose the section to output it in
   and select that section.  */

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 13),			\
  sprintf ((OUTPUT), "%s.%lu", (NAME), (unsigned long)(LABELNO)))

/* How to start an assembler comment.
   The leading space is important (the microblaze assembler requires it).  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START		" #"
#endif

#define BSS_VAR         1
#define SBSS_VAR        2
#define DATA_VAR        4
#define SDATA_VAR       5
#define RODATA_VAR      6
#define SDATA2_VAR      7

/* These definitions are used in with the shift_type flag in the rtl.  */
#define SHIFT_CONST     1
#define SHIFT_REG       2
#define USE_ADDK        3

/* Handle interrupt attribute.  */
extern int interrupt_handler;
extern int fast_interrupt;
extern int save_volatiles;

#define INTERRUPT_HANDLER_NAME "_interrupt_handler"
/* The function name for the function tagged with attribute break_handler
   has been set in the RTL as _break_handler. This function name is used
   in the generation of directives .ent .end and .global. */
#define BREAK_HANDLER_NAME "_break_handler"
#define FAST_INTERRUPT_NAME "_fast_interrupt"

/* The following #defines are used in the headers files. Always retain these.  */

/* Added for declaring size at the end of the function.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      {									\
        char label[256];						\
	static int labelno;						\
	labelno++;							\
	ASM_GENERATE_INTERNAL_LABEL (label, "Lfe", labelno);		\
        (*targetm.asm_out.internal_label) (FILE, "Lfe", labelno);	\
	fprintf (FILE, "%s", SIZE_ASM_OP);				\
	assemble_name (FILE, (FNAME));					\
        fprintf (FILE, ",");						\
	assemble_name (FILE, label);					\
        fprintf (FILE, "-");						\
	assemble_name (FILE, (FNAME));					\
	putc ('\n', FILE);						\
      }									\
  } while (0)

#define GLOBAL_ASM_OP			"\t.globl\t"
#define TYPE_ASM_OP			"\t.type\t"
#define SIZE_ASM_OP			"\t.size\t"
#define COMMON_ASM_OP			"\t.comm\t"
#define LCOMMON_ASM_OP			"\t.lcomm\t"

#define MAX_OFILE_ALIGNMENT		(32768*8)

#define TYPE_OPERAND_FMT        	"@%s"

/* Write the extra assembler code needed to declare an object properly.  */
#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
  do {									\
    fprintf (FILE, "%s", TYPE_ASM_OP);			         	\
    assemble_name (FILE, NAME);						\
    putc (',', FILE);							\
    fprintf (FILE, TYPE_OPERAND_FMT, "object");				\
    putc ('\n', FILE);							\
    size_directive_output = 0;						\
    if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
      {									\
	size_directive_output = 1;					\
	fprintf (FILE, "%s", SIZE_ASM_OP);				\
	assemble_name (FILE, NAME);					\
	fprintf (FILE, "," HOST_WIDE_INT_PRINT_DEC "\n",		\
	int_size_in_bytes (TREE_TYPE (DECL)));				\
      }									\
    microblaze_declare_object (FILE, NAME, "", ":\n", 0);			\
  } while (0)

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 fprintf (FILE, "%s", SIZE_ASM_OP);			         \
	 assemble_name (FILE, name);					 \
	 fprintf (FILE, "," HOST_WIDE_INT_PRINT_DEC "\n",		 \
		  int_size_in_bytes (TREE_TYPE (DECL)));		 \
       }								 \
   } while (0)

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)                            \
 do { fputc ( '\t', FILE);                                            \
      assemble_name (FILE, LABEL1);                                   \
      fputs ( " = ", FILE);                                           \
      assemble_name (FILE, LABEL2);                                   \
      fputc ( '\n', FILE);                                            \
 } while (0)

#define ASM_WEAKEN_LABEL(FILE,NAME) 					\
 do { fputs ("\t.weakext\t", FILE);					\
      assemble_name (FILE, NAME);					\
      fputc ('\n', FILE);						\
    } while (0)

#define MAKE_DECL_ONE_ONLY(DECL)	(DECL_WEAK (DECL) = 1)
#undef UNIQUE_SECTION_P
#define UNIQUE_SECTION_P(DECL)		(DECL_ONE_ONLY (DECL))

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION        default_elf_asm_named_section

/* Define the strings to put out for each section in the object file.  
   
   Note: For ctors/dtors, we want to give these sections the SHF_WRITE 
   attribute to allow shared libraries to patch/resolve addresses into 
   these locations.  On Microblaze, there is no concept of shared libraries 
   yet, so this is for future use.  */
#define TEXT_SECTION_ASM_OP	"\t.text"
#define DATA_SECTION_ASM_OP	"\t.data"
#define READONLY_DATA_SECTION_ASM_OP    \
                                "\t.rodata"
#define BSS_SECTION_ASM_OP      "\t.bss"
#define CTORS_SECTION_ASM_OP    "\t.section\t.ctors,\"aw\""
#define DTORS_SECTION_ASM_OP    "\t.section\t.dtors,\"aw\""
#define INIT_SECTION_ASM_OP     "\t.section\t.init,\"ax\""
#define FINI_SECTION_ASM_OP     "\t.section\t.fini,\"ax\""

#define SDATA_SECTION_ASM_OP	"\t.sdata"	/* Small RW initialized data   */
#define SDATA2_SECTION_ASM_OP	"\t.sdata2"	/* Small RO initialized data   */
#define SBSS_SECTION_ASM_OP     "\t.sbss"	/* Small RW uninitialized data */
#define SBSS2_SECTION_ASM_OP    "\t.sbss2"	/* Small RO uninitialized data */

/* We do this to save a few 10s of code space that would be taken up
   by the call_FUNC () wrappers, used by the generic CRT_CALL_STATIC_FUNCTION
   definition in crtstuff.c.  */
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
    asm ( SECTION_OP "\n"                               \
          "\tbrlid   r15, " #FUNC "\n\t nop\n"         \
          TEXT_SECTION_ASM_OP);

/* We need to group -lm as well, since some Newlib math functions 
   reference __errno!  */
#undef LIB_SPEC
#define LIB_SPEC \
"%{!nostdlib: \
%{pg:-start-group -lxilprofile -lgloss -lxil -lc -lm -end-group } \
%{!pg:-start-group -lgloss -lxil -lc -lm -end-group }} "

/* microblaze-unknown-elf target has no support of C99 runtime */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#define STARTFILE_EXECUTABLE_SPEC   "crt0.o%s crti.o%s crtbegin.o%s"
#define STARTFILE_XMDSTUB_SPEC      "crt1.o%s crti.o%s crtbegin.o%s"
#define STARTFILE_BOOTSTRAP_SPEC    "crt2.o%s crti.o%s crtbegin.o%s"
#define STARTFILE_NOVECTORS_SPEC    "crt3.o%s crti.o%s crtbegin.o%s"
#define STARTFILE_CRTINIT_SPEC      "%{!pg: %{!mno-clearbss: crtinit.o%s} \
%{mno-clearbss: sim-crtinit.o%s}} \
%{pg: %{!mno-clearbss: pgcrtinit.o%s} %{mno-clearbss: sim-pgcrtinit.o%s}}"

#define STARTFILE_DEFAULT_SPEC      STARTFILE_EXECUTABLE_SPEC

#undef SUBTARGET_EXTRA_SPECS
#define	SUBTARGET_EXTRA_SPECS						\
  { "startfile_executable",	STARTFILE_EXECUTABLE_SPEC },		\
  { "startfile_xmdstub",	STARTFILE_XMDSTUB_SPEC },		\
  { "startfile_bootstrap",	STARTFILE_BOOTSTRAP_SPEC },		\
  { "startfile_novectors",	STARTFILE_NOVECTORS_SPEC },		\
  { "startfile_crtinit",        STARTFILE_CRTINIT_SPEC },               \
  { "startfile_default",	STARTFILE_DEFAULT_SPEC },

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC  "\
%{Zxl-mode-executable   : %(startfile_executable)  ; \
  mxl-mode-executable   : %(startfile_executable)  ; \
  Zxl-mode-xmdstub      : %(startfile_xmdstub)     ; \
  mxl-mode-xmdstub      : %(startfile_xmdstub)     ; \
  Zxl-mode-bootstrap    : %(startfile_bootstrap)   ; \
  mxl-mode-bootstrap    : %(startfile_bootstrap)   ; \
  Zxl-mode-novectors    : %(startfile_novectors)   ; \
  mxl-mode-novectors    : %(startfile_novectors)   ; \
  Zxl-mode-xilkernel    : %(startfile_xilkernel)   ; \
  mxl-mode-xilkernel    : %(startfile_xilkernel)   ; \
                        : %(startfile_default)       \
} \
%(startfile_crtinit)"
