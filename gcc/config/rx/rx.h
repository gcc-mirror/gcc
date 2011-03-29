/* GCC backend definitions for the Renesas RX processor.
   Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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


#define TARGET_CPU_CPP_BUILTINS()               \
  do                                            \
    {                                           \
      builtin_define ("__RX__"); 		\
      builtin_assert ("cpu=RX"); 		\
      if (rx_cpu_type == RX610)			\
	{					\
          builtin_define ("__RX610__");		\
          builtin_assert ("machine=RX610");	\
	}					\
     else					\
        builtin_assert ("machine=RX600");	\
      						\
      if (TARGET_BIG_ENDIAN_DATA)		\
	builtin_define ("__RX_BIG_ENDIAN__");	\
      else					\
	builtin_define ("__RX_LITTLE_ENDIAN__");\
      						\
      if (TARGET_64BIT_DOUBLES)			\
	builtin_define ("__RX_64BIT_DOUBLES__");\
      else					\
	builtin_define ("__RX_32BIT_DOUBLES__");\
      						\
      if (ALLOW_RX_FPU_INSNS)			\
	builtin_define ("__RX_FPU_INSNS__");	\
						\
      if (TARGET_AS100_SYNTAX)			\
	builtin_define ("__RX_AS100_SYNTAX__"); \
      else					\
	builtin_define ("__RX_GAS_SYNTAX__");   \
    }                                           \
  while (0)

enum rx_cpu_types
{
  RX600,
  RX610,
  RX200
};

extern enum rx_cpu_types  rx_cpu_type;

#undef  CC1_SPEC
#define CC1_SPEC "\
  %{mas100-syntax:%{gdwarf*:%e-mas100-syntax is incompatible with -gdwarf}} \
  %{mcpu=rx200:%{fpu:%erx200 cpu does not have FPU hardware}}"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:crt0.o%s} crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef  ASM_SPEC
#define ASM_SPEC "\
%{mbig-endian-data:-mbig-endian-data} \
%{m64bit-doubles:-m64bit-doubles} \
%{!m64bit-doubles:-m32bit-doubles} \
%{msmall-data-limit*:-msmall-data-limit} \
%{mrelax:-relax} \
"

#undef  LIB_SPEC
#define LIB_SPEC "					\
--start-group						\
-lc							\
%{msim:-lsim}%{!msim:-lnosys}				\
%{fprofile-arcs|fprofile-generate|coverage:-lgcov} 	\
--end-group					   	\
%{!T*: %{msim:%Trx-sim.ld}%{!msim:%Trx.ld}}		\
"

#undef  LINK_SPEC
#define LINK_SPEC "%{mbig-endian-data:--oformat elf32-rx-be} %{mrelax:-relax}"


#define BITS_BIG_ENDIAN 		0
#define BYTES_BIG_ENDIAN 		TARGET_BIG_ENDIAN_DATA
#define WORDS_BIG_ENDIAN 		TARGET_BIG_ENDIAN_DATA

#define UNITS_PER_WORD 			4

#define INT_TYPE_SIZE			32
#define LONG_TYPE_SIZE			32
#define LONG_LONG_TYPE_SIZE		64

#define FLOAT_TYPE_SIZE 		32
#define DOUBLE_TYPE_SIZE 		(TARGET_64BIT_DOUBLES ? 64 : 32)
#define LONG_DOUBLE_TYPE_SIZE		DOUBLE_TYPE_SIZE

#ifdef __RX_32BIT_DOUBLES__
#define LIBGCC2_HAS_DF_MODE		0
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE   32
#else
#define LIBGCC2_HAS_DF_MODE		1
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE   64
#endif

#define DEFAULT_SIGNED_CHAR		0

#define STRICT_ALIGNMENT 		1
#define FUNCTION_BOUNDARY 		8
#define BIGGEST_ALIGNMENT 		32
#define STACK_BOUNDARY 			32
#define PARM_BOUNDARY 			8

#define STACK_GROWS_DOWNWARD		1
#define FRAME_GROWS_DOWNWARD		0
#define FIRST_PARM_OFFSET(FNDECL) 	0

#define MAX_REGS_PER_ADDRESS 		2

#define Pmode 				SImode
#define POINTER_SIZE			32
#undef  SIZE_TYPE
#define SIZE_TYPE			"long unsigned int"
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE			"long int"
#undef  WCHAR_TYPE
#define WCHAR_TYPE			"long int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE			BITS_PER_WORD
#define POINTERS_EXTEND_UNSIGNED	1
#define FUNCTION_MODE 			QImode
#define CASE_VECTOR_MODE		Pmode
#define WORD_REGISTER_OPERATIONS	1
#define HAS_LONG_COND_BRANCH		0
#define HAS_LONG_UNCOND_BRANCH		0

#define MOVE_MAX 			4
#define STARTING_FRAME_OFFSET		0

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)   1

#define LEGITIMATE_CONSTANT_P(X) 	rx_is_legitimate_constant (X)

#define HAVE_PRE_DECCREMENT		1
#define HAVE_POST_INCREMENT		1

#define MOVE_RATIO(SPEED) 		((SPEED) ? 4 : 2)
#define SLOW_BYTE_ACCESS		1

#define STORE_FLAG_VALUE		1
#define LOAD_EXTEND_OP(MODE)		SIGN_EXTEND
#define SHORT_IMMEDIATES_SIGN_EXTEND	1

enum reg_class
{
  NO_REGS,			/* No registers in set.  */
  GR_REGS,			/* Integer registers.  */
  ALL_REGS,			/* All registers.  */
  LIM_REG_CLASSES		/* Max value + 1.  */
};

#define REG_CLASS_NAMES					\
{							\
  "NO_REGS",						\
  "GR_REGS",						\
  "ALL_REGS"						\
}

#define REG_CLASS_CONTENTS				\
{							\
  { 0x00000000 },	/* No registers,  */		\
  { 0x0000ffff },	/* Integer registers.  */	\
  { 0x0000ffff }	/* All registers.  */		\
}

#define IRA_COVER_CLASSES				\
  {							\
    GR_REGS, LIM_REG_CLASSES				\
  }

#define SMALL_REGISTER_CLASSES 		0
#define N_REG_CLASSES			(int) LIM_REG_CLASSES
#define CLASS_MAX_NREGS(CLASS, MODE)    ((GET_MODE_SIZE (MODE) \
					  + UNITS_PER_WORD - 1) \
					 / UNITS_PER_WORD)

#define GENERAL_REGS			GR_REGS
#define BASE_REG_CLASS  		GR_REGS
#define INDEX_REG_CLASS			GR_REGS

#define FIRST_PSEUDO_REGISTER 		17

#define REGNO_REG_CLASS(REGNO)          ((REGNO) < FIRST_PSEUDO_REGISTER \
					 ? GR_REGS : NO_REGS)

#define STACK_POINTER_REGNUM 	        0
#define FUNC_RETURN_REGNUM              1
#define FRAME_POINTER_REGNUM 		6
#define ARG_POINTER_REGNUM 		7
#define STATIC_CHAIN_REGNUM 		8
#define TRAMPOLINE_TEMP_REGNUM		9
#define STRUCT_VAL_REGNUM		15
#define CC_REGNUM                       16

/* This is the register which is used to hold the address of the start
   of the small data area, if that feature is being used.  Note - this
   register must not be call_used because otherwise library functions
   that are compiled without small data support might clobber it.

   FIXME: The function gcc/config/rx/rx.c:rx_gen_move_template() has a
   built in copy of this register's name, rather than constructing the
   name from this #define.  */
#define GP_BASE_REGNUM			13

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM },	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM }}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	\
  (OFFSET) = rx_initial_elimination_offset ((FROM), (TO))


#define FUNCTION_ARG_REGNO_P(N)	  	(((N) >= 1) && ((N) <= 4))
#define FUNCTION_VALUE_REGNO_P(N) 	((N) == FUNC_RETURN_REGNUM)
#define DEFAULT_PCC_STRUCT_RETURN	0

#define FIXED_REGISTERS					\
{							\
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1	\
}

#define CALL_USED_REGISTERS				\
{							\
  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1	\
}

#define LIBCALL_VALUE(MODE)				\
  gen_rtx_REG (((GET_MODE_CLASS (MODE) != MODE_INT	\
                 || COMPLEX_MODE_P (MODE)		\
		 || GET_MODE_SIZE (MODE) >= 4)		\
		? (MODE)				\
		: SImode),				\
	       FUNC_RETURN_REGNUM)

/* Order of allocation of registers.  */

#define REG_ALLOC_ORDER						\
{  7,  10,  11,  12,  13,  14,  4,  3,  2,  1, 9, 8, 6, 5, 15	\
}

#define REGNO_IN_RANGE(REGNO, MIN, MAX)		\
  (IN_RANGE ((REGNO), (MIN), (MAX)) 		\
   || (reg_renumber != NULL			\
       && reg_renumber[(REGNO)] >= (MIN)	\
       && reg_renumber[(REGNO)] <= (MAX)))

#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(regno)      REGNO_IN_RANGE (regno, 0, 15)
#else
#define REGNO_OK_FOR_BASE_P(regno)	1
#endif

#define REGNO_OK_FOR_INDEX_P(regno)	REGNO_OK_FOR_BASE_P (regno)

#define RTX_OK_FOR_BASE(X, STRICT)				\
  ((STRICT) ?							\
   (   (REG_P (X)						\
        && REGNO_IN_RANGE (REGNO (X), 0, 15))			\
    || (GET_CODE (X) == SUBREG					\
        && REG_P (SUBREG_REG (X))				\
        && REGNO_IN_RANGE (REGNO (SUBREG_REG (X)), 0, 15)))	\
   :								\
    ( (REG_P (X)						\
       || (GET_CODE (X) == SUBREG				\
	   && REG_P (SUBREG_REG (X))))))


#define RETURN_ADDR_RTX(COUNT, FRAMEADDR)				\
  ((COUNT) == 0								\
   ? gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, arg_pointer_rtx, GEN_INT (-4))) \
   : NULL_RTX)

#define INCOMING_RETURN_ADDR_RTX	gen_rtx_MEM (Pmode, stack_pointer_rtx)

#define ACCUMULATE_OUTGOING_ARGS	1

typedef unsigned int CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  (CUM) = 0


#define TRAMPOLINE_SIZE 	(! TARGET_BIG_ENDIAN_DATA ? 14 : 20)
#define TRAMPOLINE_ALIGNMENT 	32

#define NO_PROFILE_COUNTERS     1
#define PROFILE_BEFORE_PROLOGUE 1

#define FUNCTION_PROFILER(FILE, LABELNO)	\
    fprintf (FILE, "\tbsr\t__mcount\n");


#define HARD_REGNO_NREGS(REGNO, MODE)   CLASS_MAX_NREGS (0, MODE)

#define HARD_REGNO_MODE_OK(REGNO, MODE) 			\
  REGNO_REG_CLASS (REGNO) == GR_REGS

#define MODES_TIEABLE_P(MODE1, MODE2)				\
  (   (   GET_MODE_CLASS (MODE1) == MODE_FLOAT			\
       || GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)		\
   == (   GET_MODE_CLASS (MODE2) == MODE_FLOAT			\
       || GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))


#define REGISTER_NAMES						\
  {								\
    "r0",  "r1",  "r2",   "r3",   "r4",   "r5",   "r6",   "r7",	\
      "r8",  "r9",  "r10",  "r11",  "r12",  "r13",  "r14",  "r15", "cc"	\
  }

#define ADDITIONAL_REGISTER_NAMES	\
{					\
    { "sp",    STACK_POINTER_REGNUM }	\
  , { "fp",    FRAME_POINTER_REGNUM }	\
  , { "arg",   ARG_POINTER_REGNUM }	\
  , { "chain", STATIC_CHAIN_REGNUM }	\
}

#define DATA_SECTION_ASM_OP	      			\
  (TARGET_AS100_SYNTAX ? "\t.SECTION D,DATA" 		\
   : "\t.section D,\"aw\",@progbits\n\t.p2align 2")

#define SDATA_SECTION_ASM_OP	      			\
  (TARGET_AS100_SYNTAX ? "\t.SECTION D_2,DATA,ALIGN=2" 	\
   : "\t.section D_2,\"aw\",@progbits\n\t.p2align 1")

#undef  READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP  			\
  (TARGET_AS100_SYNTAX ? "\t.SECTION C,ROMDATA,ALIGN=4" \
   : "\t.section C,\"a\",@progbits\n\t.p2align 2")

#define BSS_SECTION_ASM_OP	      			\
  (TARGET_AS100_SYNTAX ? "\t.SECTION B,DATA,ALIGN=4" 	\
   : "\t.section B,\"w\",@nobits\n\t.p2align 2")

#define SBSS_SECTION_ASM_OP	      			\
  (TARGET_AS100_SYNTAX ? "\t.SECTION B_2,DATA,ALIGN=2" 	\
   : "\t.section B_2,\"w\",@nobits\n\t.p2align 1")

/* The following definitions are conditional depending upon whether the
   compiler is being built or crtstuff.c is being compiled by the built
   compiler.  */
#if defined CRT_BEGIN || defined CRT_END
# ifdef __RX_AS100_SYNTAX
#  define TEXT_SECTION_ASM_OP	      "\t.SECTION P,CODE"
#  define CTORS_SECTION_ASM_OP	      "\t.SECTION init_array,CODE"
#  define DTORS_SECTION_ASM_OP	      "\t.SECTION fini_array,CODE"
#  define INIT_ARRAY_SECTION_ASM_OP   "\t.SECTION init_array,CODE"
#  define FINI_ARRAY_SECTION_ASM_OP   "\t.SECTION fini_array,CODE"
# else
#  define TEXT_SECTION_ASM_OP	      "\t.section P,\"ax\""
#  define CTORS_SECTION_ASM_OP	      \
  "\t.section\t.init_array,\"aw\",@init_array"
#  define DTORS_SECTION_ASM_OP	      \
  "\t.section\t.fini_array,\"aw\",@fini_array"
#  define INIT_ARRAY_SECTION_ASM_OP   \
  "\t.section\t.init_array,\"aw\",@init_array"
#  define FINI_ARRAY_SECTION_ASM_OP   \
  "\t.section\t.fini_array,\"aw\",@fini_array"
# endif
#else
# define TEXT_SECTION_ASM_OP	      \
  (TARGET_AS100_SYNTAX ? "\t.SECTION P,CODE" : "\t.section P,\"ax\"")

# define CTORS_SECTION_ASM_OP			      \
  (TARGET_AS100_SYNTAX ? "\t.SECTION init_array,CODE" \
   : "\t.section\t.init_array,\"aw\",@init_array")

# define DTORS_SECTION_ASM_OP			      \
  (TARGET_AS100_SYNTAX ? "\t.SECTION fini_array,CODE" \
   : "\t.section\t.fini_array,\"aw\",@fini_array")

# define INIT_ARRAY_SECTION_ASM_OP		      \
  (TARGET_AS100_SYNTAX ? "\t.SECTION init_array,CODE" \
   : "\t.section\t.init_array,\"aw\",@init_array")

# define FINI_ARRAY_SECTION_ASM_OP		      \
  (TARGET_AS100_SYNTAX ? "\t.SECTION fini_array,CODE" \
   : "\t.section\t.fini_array,\"aw\",@fini_array")
#endif

#define GLOBAL_ASM_OP 		\
  (TARGET_AS100_SYNTAX ? "\t.GLB\t" : "\t.global\t")
#define ASM_COMMENT_START	" ;"
#define ASM_APP_ON		""
#define ASM_APP_OFF 		""
#define LOCAL_LABEL_PREFIX	"L"
#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX	"_"

#define ASM_OUTPUT_ALIGN(STREAM, LOG)		\
  do						\
    {						\
      if ((LOG) == 0)				\
        break;					\
      if (TARGET_AS100_SYNTAX)			\
	{					\
	  if ((LOG) >= 2)			\
	    fprintf (STREAM, "\t.ALIGN 4\t; %d alignment actually requested\n", 1 << (LOG)); \
	  else					\
	    fprintf (STREAM, "\t.ALIGN 2\n");	\
	}					\
      else					\
	fprintf (STREAM, "\t.balign %d\n", 1 << (LOG));	\
    }						\
  while (0)

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, TARGET_AS100_SYNTAX ? "\t.LWORD L%d\n" : "\t.long .L%d\n", \
	   VALUE)

/* This is how to output an element of a case-vector that is relative.
   Note: The local label referenced by the "3b" below is emitted by
   the tablejump insn.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, TARGET_AS100_SYNTAX \
	   ? "\t.LWORD L%d - ?-\n" : "\t.long .L%d - 1b\n", VALUE)

#define ASM_OUTPUT_SIZE_DIRECTIVE(STREAM, NAME, SIZE)			\
  do									\
    {									\
      HOST_WIDE_INT size_ = (SIZE);					\
									\
      /* The as100 assembler does not have an equivalent of the SVR4    \
	 .size pseudo-op.  */						\
      if (TARGET_AS100_SYNTAX)						\
	break;								\
									\
      fputs (SIZE_ASM_OP, STREAM);					\
      assemble_name (STREAM, NAME);					\
      fprintf (STREAM, ", " HOST_WIDE_INT_PRINT_DEC "\n", size_);	\
    }									\
  while (0)

#define ASM_OUTPUT_MEASURED_SIZE(STREAM, NAME)				\
  do									\
    {									\
      /* The as100 assembler does not have an equivalent of the SVR4    \
	 .size pseudo-op.  */						\
      if (TARGET_AS100_SYNTAX)						\
	break;								\
      fputs (SIZE_ASM_OP, STREAM);					\
      assemble_name (STREAM, NAME);					\
      fputs (", .-", STREAM);						\
      assemble_name (STREAM, NAME);					\
      putc ('\n', STREAM);						\
    }									\
  while (0)

#define ASM_OUTPUT_TYPE_DIRECTIVE(STREAM, NAME, TYPE)			\
  do									\
    {									\
      /* The as100 assembler does not have an equivalent of the SVR4    \
	 .size pseudo-op.  */						\
      if (TARGET_AS100_SYNTAX)						\
	break;								\
      fputs (TYPE_ASM_OP, STREAM);					\
      assemble_name (STREAM, NAME);					\
      fputs (", ", STREAM);						\
      fprintf (STREAM, TYPE_OPERAND_FMT, TYPE);				\
      putc ('\n', STREAM);						\
    }									\
  while (0)

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)		\
  do								\
    {								\
      sprintf (LABEL, TARGET_AS100_SYNTAX ? "*%s%u" : "*.%s%u", \
	       PREFIX, (unsigned) (NUM));			\
    }								\
  while (0)

#undef  ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)			\
  do								\
    {								\
      if (TARGET_AS100_SYNTAX)					\
	targetm.asm_out.globalize_label (FILE, NAME);		\
      default_elf_asm_output_external (FILE, DECL, NAME);	\
    }								\
  while (0)

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      if (TARGET_AS100_SYNTAX)						\
	{								\
	  fprintf ((FILE), "\t.GLB\t");					\
	  assemble_name ((FILE), (NAME));				\
	  fprintf ((FILE), "\n");					\
          assemble_name ((FILE), (NAME));				\
	  switch ((ALIGN) / BITS_PER_UNIT)				\
            {								\
            case 4:							\
              fprintf ((FILE), ":\t.BLKL\t"HOST_WIDE_INT_PRINT_UNSIGNED"\n",\
		       (SIZE) / 4);					\
	      break;							\
            case 2:							\
              fprintf ((FILE), ":\t.BLKW\t"HOST_WIDE_INT_PRINT_UNSIGNED"\n",\
		       (SIZE) / 2);					\
	      break;							\
            default:							\
              fprintf ((FILE), ":\t.BLKB\t"HOST_WIDE_INT_PRINT_UNSIGNED"\n",\
		       (SIZE));						\
	      break;							\
            }								\
        }								\
      else								\
        {								\
          fprintf ((FILE), "%s", COMMON_ASM_OP);			\
          assemble_name ((FILE), (NAME));				\
          fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",	\
	           (SIZE), (ALIGN) / BITS_PER_UNIT);			\
	}								\
    }									\
  while (0)

#undef  SKIP_ASM_OP
#define SKIP_ASM_OP   (TARGET_AS100_SYNTAX ? "\t.BLKB\t" : "\t.zero\t")

#undef  ASM_OUTPUT_LIMITED_STRING
#define ASM_OUTPUT_LIMITED_STRING(FILE, STR)		\
  do							\
    {							\
      const unsigned char *_limited_str =		\
	(const unsigned char *) (STR);			\
      unsigned ch;					\
							\
      fprintf ((FILE), TARGET_AS100_SYNTAX 		\
	       ? "\t.BYTE\t\"" : "\t.string\t\"");	\
							\
      for (; (ch = *_limited_str); _limited_str++)	\
        {						\
	  int escape;					\
							\
	  switch (escape = ESCAPES[ch])			\
	    {						\
	    case 0:					\
	      putc (ch, (FILE));			\
	      break;					\
	    case 1:					\
	      fprintf ((FILE), "\\%03o", ch);		\
	      break;					\
	    default:					\
	      putc ('\\', (FILE));			\
	      putc (escape, (FILE));			\
	      break;					\
	    }						\
        }						\
							\
      fprintf ((FILE), TARGET_AS100_SYNTAX ? "\"\n\t.BYTE\t0\n" : "\"\n");\
    }							\
  while (0)

#undef  IDENT_ASM_OP
#define IDENT_ASM_OP  (TARGET_AS100_SYNTAX \
		       ? "\t.END\t; Built by: ": "\t.ident\t")

/* For PIC put jump tables into the text section so that the offsets that
   they contain are always computed between two same-section symbols.  */
#define JUMP_TABLES_IN_TEXT_SECTION	(flag_pic)

/* This is a version of REG_P that also returns TRUE for SUBREGs.  */
#define RX_REG_P(rtl) (REG_P (rtl) || GET_CODE (rtl) == SUBREG)

/* Like REG_P except that this macro is true for SET expressions.  */
#define SET_P(rtl)    (GET_CODE (rtl) == SET)

/* The AS100 assembler does not support .leb128 and .uleb128, but
   the compiler-build-time configure tests will have enabled their
   use because GAS supports them.  So default to generating STABS
   debug information instead of DWARF2 when generating AS100
   compatible output.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE (TARGET_AS100_SYNTAX \
				  ? DBX_DEBUG : DWARF2_DEBUG)

#define INCOMING_FRAME_SP_OFFSET		4
#define ARG_POINTER_CFA_OFFSET(FNDECL)		4
#define FRAME_POINTER_CFA_OFFSET(FNDECL)	4

#define TARGET_USE_FPU		(! TARGET_NO_USE_FPU)

/* This macro is used to decide when RX FPU instructions can be used.  */
#define ALLOW_RX_FPU_INSNS	(TARGET_USE_FPU)

#define BRANCH_COST(SPEED,PREDICT)       1
#define REGISTER_MOVE_COST(MODE,FROM,TO) 2

#define SELECT_CC_MODE(OP,X,Y)  rx_select_cc_mode((OP), (X), (Y))

#define LABEL_ALIGN_AFTER_BARRIER(x)		rx_align_for_label ()

#define ASM_OUTPUT_MAX_SKIP_ALIGN(STREAM, LOG, MAX_SKIP)	\
  do						\
    {						\
      if ((LOG) == 0 || (MAX_SKIP) == 0)	\
        break;					\
      if (TARGET_AS100_SYNTAX)			\
	{					\
	  if ((LOG) >= 2)			\
	    fprintf (STREAM, "\t.ALIGN 4\t; %d alignment actually requested\n", 1 << (LOG)); \
	  else					\
	    fprintf (STREAM, "\t.ALIGN 2\n");	\
	}					\
      else					\
	fprintf (STREAM, "\t.balign %d,3,%d\n", 1 << (LOG), (MAX_SKIP));	\
    }						\
  while (0)
