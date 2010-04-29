/* Definitions of target machine for GNU compiler,
   for ATMEL AVR at90s8515, ATmega103/103L, ATmega603/603L microcontrollers.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
   2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Denis Chertykov (chertykov@gmail.com)

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

/* Names to predefine in the preprocessor for this target machine.  */

struct base_arch_s {
  /* Assembler only.  */
  int asm_only;

  /* Core have 'MUL*' instructions.  */
  int have_mul;

  /* Core have 'CALL' and 'JMP' instructions.  */
  int have_jmp_call;

  /* Core have 'MOVW' and 'LPM Rx,Z' instructions.  */
  int have_movw_lpmx;

  /* Core have 'ELPM' instructions.  */
  int have_elpm;

  /* Core have 'ELPM Rx,Z' instructions.  */
  int have_elpmx;

  /* Core have 'EICALL' and 'EIJMP' instructions.  */
  int have_eijmp_eicall;

  /* Reserved for xmega architecture.  */
  int reserved;

  /* Reserved for xmega architecture.  */
  int reserved2;
  
  /* Default start of data section address for architecture.  */
  int default_data_section_start;

  const char *const macro;
  
  /* Architecture name.  */
  const char *const arch_name;  
};

/* These names are used as the index into the avr_arch_types[] table 
   above.  */

enum avr_arch
{
  ARCH_UNKNOWN,
  ARCH_AVR1,
  ARCH_AVR2,
  ARCH_AVR25,
  ARCH_AVR3,
  ARCH_AVR31,
  ARCH_AVR35,
  ARCH_AVR4,
  ARCH_AVR5,
  ARCH_AVR51,
  ARCH_AVR6
};

struct mcu_type_s {
  /* Device name.  */
  const char *const name;
  
  /* Index in avr_arch_types[].  */
  int arch; 
  
  /* Must lie outside user's namespace.  NULL == no macro.  */
  const char *const macro;
  
  /* Stack pointer have 8 bits width.  */
  int short_sp;
  
  /* Start of data section.  */
  int data_section_start;
  
  /* Name of device library.  */
  const char *const library_name; 
};

/* Preprocessor macros to define depending on MCU type.  */
extern const char *avr_extra_arch_macro;
extern const struct base_arch_s *avr_current_arch;
extern const struct mcu_type_s *avr_current_device;
extern const struct mcu_type_s avr_mcu_types[];
extern const struct base_arch_s avr_arch_types[];

#define TARGET_CPU_CPP_BUILTINS()	avr_cpu_cpp_builtins (pfile)

#if !defined(IN_LIBGCC2) && !defined(IN_TARGET_LIBS)
extern GTY(()) section *progmem_section;
#endif

#define AVR_HAVE_JMP_CALL (avr_current_arch->have_jmp_call && !TARGET_SHORT_CALLS)
#define AVR_HAVE_MUL (avr_current_arch->have_mul)
#define AVR_HAVE_MOVW (avr_current_arch->have_movw_lpmx)
#define AVR_HAVE_LPMX (avr_current_arch->have_movw_lpmx)
#define AVR_HAVE_RAMPZ (avr_current_arch->have_elpm)
#define AVR_HAVE_EIJMP_EICALL (avr_current_arch->have_eijmp_eicall)
#define AVR_HAVE_8BIT_SP (avr_current_device->short_sp || TARGET_TINY_STACK)

#define AVR_2_BYTE_PC (!AVR_HAVE_EIJMP_EICALL)
#define AVR_3_BYTE_PC (AVR_HAVE_EIJMP_EICALL)

#define TARGET_VERSION fprintf (stderr, " (GNU assembler syntax)");

#define OVERRIDE_OPTIONS avr_override_options ()

#define CAN_DEBUG_WITHOUT_FP

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

#ifdef IN_LIBGCC2
/* This is to get correct SI and DI modes in libgcc2.c (32 and 64 bits).  */
#define UNITS_PER_WORD 4
#else
/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 1
#endif

#define POINTER_SIZE 16


/* Maximum sized of reasonable data type
   DImode or Dfmode ...  */
#define MAX_FIXED_MODE_SIZE 32

#define PARM_BOUNDARY 8

#define FUNCTION_BOUNDARY 8

#define EMPTY_FIELD_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 8

#define MAX_OFILE_ALIGNMENT (32768 * 8)

#define TARGET_VTABLE_ENTRY_ALIGN 8

#define STRICT_ALIGNMENT 0

#define INT_TYPE_SIZE (TARGET_INT8 ? 8 : 16)
#define SHORT_TYPE_SIZE (INT_TYPE_SIZE == 8 ? INT_TYPE_SIZE : 16)
#define LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 16 : 32)
#define LONG_LONG_TYPE_SIZE (INT_TYPE_SIZE == 8 ? 32 : 64)
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 32
#define LONG_DOUBLE_TYPE_SIZE 32

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE (INT_TYPE_SIZE == 8 ? "long unsigned int" : "unsigned int")
#define PTRDIFF_TYPE (INT_TYPE_SIZE == 8 ? "long int" :"int")

#define WCHAR_TYPE_SIZE 16

#define FIRST_PSEUDO_REGISTER 36

#define FIXED_REGISTERS {\
  1,1,/* r0 r1 */\
  0,0,/* r2 r3 */\
  0,0,/* r4 r5 */\
  0,0,/* r6 r7 */\
  0,0,/* r8 r9 */\
  0,0,/* r10 r11 */\
  0,0,/* r12 r13 */\
  0,0,/* r14 r15 */\
  0,0,/* r16 r17 */\
  0,0,/* r18 r19 */\
  0,0,/* r20 r21 */\
  0,0,/* r22 r23 */\
  0,0,/* r24 r25 */\
  0,0,/* r26 r27 */\
  0,0,/* r28 r29 */\
  0,0,/* r30 r31 */\
  1,1,/*  STACK */\
  1,1 /* arg pointer */  }

#define CALL_USED_REGISTERS {			\
  1,1,/* r0 r1 */				\
    0,0,/* r2 r3 */				\
    0,0,/* r4 r5 */				\
    0,0,/* r6 r7 */				\
    0,0,/* r8 r9 */				\
    0,0,/* r10 r11 */				\
    0,0,/* r12 r13 */				\
    0,0,/* r14 r15 */				\
    0,0,/* r16 r17 */				\
    1,1,/* r18 r19 */				\
    1,1,/* r20 r21 */				\
    1,1,/* r22 r23 */				\
    1,1,/* r24 r25 */				\
    1,1,/* r26 r27 */				\
    0,0,/* r28 r29 */				\
    1,1,/* r30 r31 */				\
    1,1,/*  STACK */				\
    1,1 /* arg pointer */  }

#define REG_ALLOC_ORDER {			\
    24,25,					\
    18,19,					\
    20,21,					\
    22,23,					\
    30,31,					\
    26,27,					\
    28,29,					\
    17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,	\
    0,1,					\
    32,33,34,35					\
    }

#define ADJUST_REG_ALLOC_ORDER order_regs_for_local_alloc ()


#define HARD_REGNO_NREGS(REGNO, MODE) ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(REGNO, MODE) avr_hard_regno_mode_ok(REGNO, MODE)

#define MODES_TIEABLE_P(MODE1, MODE2) 1

enum reg_class {
  NO_REGS,
  R0_REG,			/* r0 */
  POINTER_X_REGS,		/* r26 - r27 */
  POINTER_Y_REGS,		/* r28 - r29 */
  POINTER_Z_REGS,		/* r30 - r31 */
  STACK_REG,			/* STACK */
  BASE_POINTER_REGS,		/* r28 - r31 */
  POINTER_REGS,			/* r26 - r31 */
  ADDW_REGS,			/* r24 - r31 */
  SIMPLE_LD_REGS,		/* r16 - r23 */
  LD_REGS,			/* r16 - r31 */
  NO_LD_REGS,			/* r0 - r15 */
  GENERAL_REGS,			/* r0 - r31 */
  ALL_REGS, LIM_REG_CLASSES
};


#define N_REG_CLASSES (int)LIM_REG_CLASSES

#define REG_CLASS_NAMES {					\
		 "NO_REGS",					\
		   "R0_REG",	/* r0 */                        \
		   "POINTER_X_REGS", /* r26 - r27 */		\
		   "POINTER_Y_REGS", /* r28 - r29 */		\
		   "POINTER_Z_REGS", /* r30 - r31 */		\
		   "STACK_REG",	/* STACK */			\
		   "BASE_POINTER_REGS",	/* r28 - r31 */		\
		   "POINTER_REGS", /* r26 - r31 */		\
		   "ADDW_REGS",	/* r24 - r31 */			\
                   "SIMPLE_LD_REGS", /* r16 - r23 */            \
		   "LD_REGS",	/* r16 - r31 */			\
                   "NO_LD_REGS", /* r0 - r15 */                 \
		   "GENERAL_REGS", /* r0 - r31 */		\
		   "ALL_REGS" }

#define REG_CLASS_CONTENTS {						\
  {0x00000000,0x00000000},	/* NO_REGS */				\
  {0x00000001,0x00000000},	/* R0_REG */                            \
  {3 << REG_X,0x00000000},      /* POINTER_X_REGS, r26 - r27 */		\
  {3 << REG_Y,0x00000000},      /* POINTER_Y_REGS, r28 - r29 */		\
  {3 << REG_Z,0x00000000},      /* POINTER_Z_REGS, r30 - r31 */		\
  {0x00000000,0x00000003},	/* STACK_REG, STACK */			\
  {(3 << REG_Y) | (3 << REG_Z),						\
     0x00000000},		/* BASE_POINTER_REGS, r28 - r31 */	\
  {(3 << REG_X) | (3 << REG_Y) | (3 << REG_Z),				\
     0x00000000},		/* POINTER_REGS, r26 - r31 */		\
  {(3 << REG_X) | (3 << REG_Y) | (3 << REG_Z) | (3 << REG_W),		\
     0x00000000},		/* ADDW_REGS, r24 - r31 */		\
  {0x00ff0000,0x00000000},	/* SIMPLE_LD_REGS r16 - r23 */          \
  {(3 << REG_X)|(3 << REG_Y)|(3 << REG_Z)|(3 << REG_W)|(0xff << 16),	\
     0x00000000},	/* LD_REGS, r16 - r31 */			\
  {0x0000ffff,0x00000000},	/* NO_LD_REGS  r0 - r15 */              \
  {0xffffffff,0x00000000},	/* GENERAL_REGS, r0 - r31 */		\
  {0xffffffff,0x00000003}	/* ALL_REGS */				\
}

#define REGNO_REG_CLASS(R) avr_regno_reg_class(R)

/* The following macro defines cover classes for Integrated Register
   Allocator.  Cover classes is a set of non-intersected register
   classes covering all hard registers used for register allocation
   purpose.  Any move between two registers of a cover class should be
   cheaper than load or store of the registers.  The macro value is
   array of register classes with LIM_REG_CLASSES used as the end
   marker.  */

#define IRA_COVER_CLASSES               \
{                                       \
  GENERAL_REGS, LIM_REG_CLASSES         \
}

#define BASE_REG_CLASS (reload_completed ? BASE_POINTER_REGS : POINTER_REGS)

#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_BASE_P(r) (((r) < FIRST_PSEUDO_REGISTER		\
				 && ((r) == REG_X			\
				     || (r) == REG_Y			\
				     || (r) == REG_Z			\
				     || (r) == ARG_POINTER_REGNUM))	\
				|| (reg_renumber			\
				    && (reg_renumber[r] == REG_X	\
					|| reg_renumber[r] == REG_Y	\
					|| reg_renumber[r] == REG_Z	\
					|| (reg_renumber[r]		\
					    == ARG_POINTER_REGNUM))))

#define REGNO_OK_FOR_INDEX_P(NUM) 0

#define PREFERRED_RELOAD_CLASS(X, CLASS) preferred_reload_class(X,CLASS)

#define SMALL_REGISTER_CLASSES 1

#define CLASS_LIKELY_SPILLED_P(c) class_likely_spilled_p(c)

#define CLASS_MAX_NREGS(CLASS, MODE)   class_max_nregs (CLASS, MODE)

#define STACK_PUSH_CODE POST_DEC

#define STACK_GROWS_DOWNWARD

#define STARTING_FRAME_OFFSET 1

#define STACK_POINTER_OFFSET 1

#define FIRST_PARM_OFFSET(FUNDECL) 0

#define STACK_BOUNDARY 8

#define STACK_POINTER_REGNUM 32

#define FRAME_POINTER_REGNUM REG_Y

#define ARG_POINTER_REGNUM 34

#define STATIC_CHAIN_REGNUM 2

/* Offset from the frame pointer register value to the top of the stack.  */
#define FRAME_POINTER_CFA_OFFSET(FNDECL) 0

#define ELIMINABLE_REGS {					\
      {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},		\
	{FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}		\
       ,{FRAME_POINTER_REGNUM+1,STACK_POINTER_REGNUM+1}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  OFFSET = avr_initial_elimination_offset (FROM, TO)

#define RETURN_ADDR_RTX(count, tem) avr_return_addr_rtx (count, tem)

/* Don't use Push rounding. expr.c: emit_single_push_insn is broken 
   for POST_DEC targets (PR27386).  */
/*#define PUSH_ROUNDING(NPUSHED) (NPUSHED)*/

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) (function_arg (&(CUM), MODE, TYPE, NAMED))

typedef struct avr_args {
  int nregs;			/* # registers available for passing */
  int regno;			/* next available register number */
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  init_cumulative_args (&(CUM), FNTYPE, LIBNAME, FNDECL)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (function_arg_advance (&CUM, MODE, TYPE, NAMED))

#define FUNCTION_ARG_REGNO_P(r) function_arg_regno_p(r)

extern int avr_reg_order[];

#define RET_REGISTER avr_ret_register ()

#define LIBCALL_VALUE(MODE)  avr_libcall_value (MODE)

#define FUNCTION_VALUE_REGNO_P(N) ((int) (N) == RET_REGISTER)

#define DEFAULT_PCC_STRUCT_RETURN 0

#define EPILOGUE_USES(REGNO) avr_epilogue_uses(REGNO)

#define HAVE_POST_INCREMENT 1
#define HAVE_PRE_DECREMENT 1

#define MAX_REGS_PER_ADDRESS 1

#define REG_OK_FOR_BASE_NOSTRICT_P(X) \
  (REGNO (X) >= FIRST_PSEUDO_REGISTER || REG_OK_FOR_BASE_STRICT_P(X))

#define REG_OK_FOR_BASE_STRICT_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

/* LEGITIMIZE_RELOAD_ADDRESS will allow register R26/27 to be used, where it
   is no worse than normal base pointers R28/29 and R30/31. For example:
   If base offset is greater than 63 bytes or for R++ or --R addressing.  */
   
#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND_LEVELS, WIN)    \
do {									    \
  if (1&&(GET_CODE (X) == POST_INC || GET_CODE (X) == PRE_DEC))	    \
    {									    \
      push_reload (XEXP (X,0), XEXP (X,0), &XEXP (X,0), &XEXP (X,0),	    \
	           POINTER_REGS, GET_MODE (X),GET_MODE (X) , 0, 0,	    \
		   OPNUM, RELOAD_OTHER);				    \
      goto WIN;								    \
    }									    \
  if (GET_CODE (X) == PLUS						    \
      && REG_P (XEXP (X, 0))						    \
      && reg_equiv_constant[REGNO (XEXP (X, 0))] == 0			    \
      && GET_CODE (XEXP (X, 1)) == CONST_INT				    \
      && INTVAL (XEXP (X, 1)) >= 1)					    \
    {									    \
      int fit = INTVAL (XEXP (X, 1)) <= (64 - GET_MODE_SIZE (MODE));	    \
      if (fit)								    \
	{								    \
          if (reg_equiv_address[REGNO (XEXP (X, 0))] != 0)		    \
	    {								    \
	      int regno = REGNO (XEXP (X, 0));				    \
	      rtx mem = make_memloc (X, regno);				    \
	      push_reload (XEXP (mem,0), NULL, &XEXP (mem,0), NULL,         \
		           POINTER_REGS, Pmode, VOIDmode, 0, 0,		    \
		           1, ADDR_TYPE (TYPE));			    \
	      push_reload (mem, NULL_RTX, &XEXP (X, 0), NULL,		    \
		           BASE_POINTER_REGS, GET_MODE (X), VOIDmode, 0, 0, \
		           OPNUM, TYPE);				    \
	      goto WIN;							    \
	    }								    \
	}								    \
      else if (! (frame_pointer_needed && XEXP (X,0) == frame_pointer_rtx)) \
	{								    \
	  push_reload (X, NULL_RTX, &X, NULL,				    \
		       POINTER_REGS, GET_MODE (X), VOIDmode, 0, 0,	    \
		       OPNUM, TYPE);					    \
          goto WIN;							    \
	}								    \
    }									    \
} while(0)

#define LEGITIMATE_CONSTANT_P(X) 1

#define REGISTER_MOVE_COST(MODE, FROM, TO) ((FROM) == STACK_REG ? 6 \
					    : (TO) == STACK_REG ? 12 \
					    : 2)

#define MEMORY_MOVE_COST(MODE,CLASS,IN) ((MODE)==QImode ? 2 :	\
					 (MODE)==HImode ? 4 :	\
					 (MODE)==SImode ? 8 :	\
					 (MODE)==SFmode ? 8 : 16)

#define BRANCH_COST(speed_p, predictable_p) 0

#define SLOW_BYTE_ACCESS 0

#define NO_FUNCTION_CSE

#define TEXT_SECTION_ASM_OP "\t.text"

#define DATA_SECTION_ASM_OP "\t.data"

#define BSS_SECTION_ASM_OP "\t.section .bss"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.
   There are no shared libraries on this target, and these sections are
   placed in the read-only program memory, so they are not writable.  */

#undef CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP "\t.section .ctors,\"a\",@progbits"

#undef DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP "\t.section .dtors,\"a\",@progbits"

#define TARGET_ASM_CONSTRUCTOR avr_asm_out_ctor

#define TARGET_ASM_DESTRUCTOR avr_asm_out_dtor

#define SUPPORTS_INIT_PRIORITY 0

#define JUMP_TABLES_IN_TEXT_SECTION 0

#define ASM_COMMENT_START " ; "

#define ASM_APP_ON "/* #APP */\n"

#define ASM_APP_OFF "/* #NOAPP */\n"

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section
#define TARGET_ASM_INIT_SECTIONS avr_asm_init_sections

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)	 gas_output_ascii (FILE,P,SIZE)

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) ((C) == '\n' || ((C) == '$'))

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)			   \
do {									   \
     fputs ("\t.comm ", (STREAM));					   \
     assemble_name ((STREAM), (NAME));					   \
     fprintf ((STREAM), ",%lu,1\n", (unsigned long)(SIZE));		   \
} while (0)

#define ASM_OUTPUT_BSS(FILE, DECL, NAME, SIZE, ROUNDED)			\
  asm_output_bss ((FILE), (DECL), (NAME), (SIZE), (ROUNDED))

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)			\
do {									\
     fputs ("\t.lcomm ", (STREAM));					\
     assemble_name ((STREAM), (NAME));					\
     fprintf ((STREAM), ",%d\n", (int)(SIZE));				\
} while (0)

#undef TYPE_ASM_OP
#undef SIZE_ASM_OP
#undef WEAK_ASM_OP
#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"
#define WEAK_ASM_OP	"\t.weak\t"
/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */


#undef TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"@%s"
/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
avr_asm_declare_function_name ((FILE), (NAME), (DECL))

#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do {									\
    if (!flag_inhibit_size_directive)					\
      ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);				\
  } while (0)

#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)			\
do {									\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
  size_directive_output = 0;						\
  if (!flag_inhibit_size_directive && DECL_SIZE (DECL))			\
    {									\
      size_directive_output = 1;					\
      ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME,				\
				 int_size_in_bytes (TREE_TYPE (DECL)));	\
    }									\
  ASM_OUTPUT_LABEL(FILE, NAME);						\
} while (0)

#undef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		 \
     HOST_WIDE_INT size;						 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 size = int_size_in_bytes (TREE_TYPE (DECL));			 \
	 ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);			 \
       }								 \
   } while (0)


#define ESCAPES \
"\1\1\1\1\1\1\1\1btn\1fr\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\0\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"
/* A table of bytes codes used by the ASM_OUTPUT_ASCII and
   ASM_OUTPUT_LIMITED_STRING macros.  Each byte in the table
   corresponds to a particular byte value [0..255].  For any
   given byte value, if the value in the corresponding table
   position is zero, the given character can be output directly.
   If the table value is 1, the byte must be output as a \ooo
   octal escape.  If the tables value is anything else, then the
   byte value should be output as a \ followed by the value
   in the table.  Note that we can use standard UN*X escape
   sequences for many control characters, but we don't use
   \a to represent BEL because some svr4 assemblers (e.g. on
   the i386) don't know about that.  Also, we don't use \v
   since some versions of gas, such as 2.2 did not accept it.  */

#define STRING_LIMIT	((unsigned) 64)
#define STRING_ASM_OP	"\t.string\t"
/* Some svr4 assemblers have a limit on the number of characters which
   can appear in the operand of a .string directive.  If your assembler
   has such a limitation, you should define STRING_LIMIT to reflect that
   limit.  Note that at least some svr4 assemblers have a limit on the
   actual number of bytes in the double-quoted string, and that they
   count each character in an escape sequence as one byte.  Thus, an
   escape sequence like \377 would count as four bytes.

   If your target assembler doesn't support the .string directive, you
   should define this to zero.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".global\t"

#define SET_ASM_OP	"\t.set\t"

#define ASM_WEAKEN_LABEL(FILE, NAME)	\
  do					\
    {					\
      fputs ("\t.weak\t", (FILE));	\
      assemble_name ((FILE), (NAME));	\
      fputc ('\n', (FILE));		\
    }					\
  while (0)

#define SUPPORTS_WEAK 1

#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)	\
sprintf (STRING, "*.%s%lu", PREFIX, (unsigned long)(NUM))

#define HAS_INIT_SECTION 1

#define REGISTER_NAMES {				\
  "r0","r1","r2","r3","r4","r5","r6","r7",		\
    "r8","r9","r10","r11","r12","r13","r14","r15",	\
    "r16","r17","r18","r19","r20","r21","r22","r23",	\
    "r24","r25","r26","r27","r28","r29","r30","r31",	\
    "__SP_L__","__SP_H__","argL","argH"}

#define FINAL_PRESCAN_INSN(insn, operand, nop) final_prescan_insn (insn, operand,nop)

#define PRINT_OPERAND(STREAM, X, CODE) print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '~' || (CODE) == '!')

#define PRINT_OPERAND_ADDRESS(STREAM, X) print_operand_address(STREAM, X)

#define USER_LABEL_PREFIX ""

#define ASSEMBLER_DIALECT AVR_HAVE_MOVW

#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)	\
{						\
  gcc_assert (REGNO < 32);			\
  fprintf (STREAM, "\tpush\tr%d", REGNO);	\
}

#define ASM_OUTPUT_REG_POP(STREAM, REGNO)	\
{						\
  gcc_assert (REGNO < 32);			\
  fprintf (STREAM, "\tpop\tr%d", REGNO);	\
}

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)		\
  avr_output_addr_vec_elt(STREAM, VALUE)

#define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, TABLE) \
  (switch_to_section (progmem_section), \
   (*targetm.asm_out.internal_label) (STREAM, PREFIX, NUM))

#define ASM_OUTPUT_SKIP(STREAM, N)		\
fprintf (STREAM, "\t.skip %lu,0\n", (unsigned long)(N))

#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do {							\
      if ((POWER) > 1)					\
          fprintf (STREAM, "\t.p2align\t%d\n", POWER);	\
  } while (0)

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME) \
  default_elf_asm_output_external (FILE, DECL, NAME)

#define CASE_VECTOR_MODE HImode

#undef WORD_REGISTER_OPERATIONS

#define MOVE_MAX 4

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define Pmode HImode

#define FUNCTION_MODE HImode

#define DOLLARS_IN_IDENTIFIERS 0

#define NO_DOLLAR_IN_LABEL 1

#define TRAMPOLINE_SIZE 4

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP, INSN)

/* The add insns don't set overflow in a usable way.  */
#define CC_OVERFLOW_UNUSABLE 01000
/* The mov,and,or,xor insns don't set carry.  That's ok though as the
   Z bit is all we need when doing unsigned comparisons on the result of
   these insns (since they're always with 0).  However, conditions.h has
   CC_NO_OVERFLOW defined for this purpose.  Rename it to something more
   understandable.  */
#define CC_NO_CARRY CC_NO_OVERFLOW


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "/* profiler %d */", (LABELNO))

#define ADJUST_INSN_LENGTH(INSN, LENGTH) (LENGTH =\
					  adjust_insn_length (INSN, LENGTH))

extern const char *avr_device_to_arch (int argc, const char **argv);
extern const char *avr_device_to_data_start (int argc, const char **argv);
extern const char *avr_device_to_startfiles (int argc, const char **argv);
extern const char *avr_device_to_devicelib (int argc, const char **argv);

#define EXTRA_SPEC_FUNCTIONS \
  { "device_to_arch", avr_device_to_arch }, \
  { "device_to_data_start", avr_device_to_data_start }, \
  { "device_to_startfile", avr_device_to_startfiles }, \
  { "device_to_devicelib", avr_device_to_devicelib },

#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

#define CC1_SPEC "%{profile:-p}"

#define CC1PLUS_SPEC "%{!frtti:-fno-rtti} \
    %{!fenforce-eh-specs:-fno-enforce-eh-specs} \
    %{!fexceptions:-fno-exceptions}"
/* A C string constant that tells the GCC driver program options to
   pass to `cc1plus'.  */

#define ASM_SPEC "%{mmcu=avr25:-mmcu=avr2;mmcu=avr35:-mmcu=avr3;mmcu=avr31:-mmcu=avr3;mmcu=avr51:-mmcu=avr5;\
mmcu=*:-mmcu=%*}"

#define LINK_SPEC "\
%{mrelax:--relax\
         %{mpmem-wrap-around:%{mmcu=at90usb8*:--pmem-wrap-around=8k}\
                             %{mmcu=atmega16*:--pmem-wrap-around=16k}\
                             %{mmcu=atmega32*|\
                               mmcu=at90can32*:--pmem-wrap-around=32k}\
                             %{mmcu=atmega64*|\
                               mmcu=at90can64*|\
                               mmcu=at90usb64*:--pmem-wrap-around=64k}}}\
%:device_to_arch(%{mmcu=*:%*})\
%:device_to_data_start(%{mmcu=*:%*})"

#define LIB_SPEC \
  "%{!mmcu=at90s1*:%{!mmcu=attiny11:%{!mmcu=attiny12:%{!mmcu=attiny15:%{!mmcu=attiny28: -lc }}}}}"

#define LIBSTDCXX "-lgcc"
/* No libstdc++ for now.  Empty string doesn't work.  */

#define LIBGCC_SPEC \
  "%{!mmcu=at90s1*:%{!mmcu=attiny11:%{!mmcu=attiny12:%{!mmcu=attiny15:%{!mmcu=attiny28: -lgcc }}}}}"

#define STARTFILE_SPEC "%:device_to_startfile(%{mmcu=*:%*})"

#define ENDFILE_SPEC ""

/* This is the default without any -mmcu=* option (AT90S*).  */
#define MULTILIB_DEFAULTS { "mmcu=avr2" }

#define TEST_HARD_REG_CLASS(CLASS, REGNO) \
  TEST_HARD_REG_BIT (reg_class_contents[ (int) (CLASS)], REGNO)

/* Note that the other files fail to use these
   in some of the places where they should.  */

#if defined(__STDC__) || defined(ALMOST_STDC)
#define AS2(a,b,c) #a " " #b "," #c
#define AS2C(b,c) " " #b "," #c
#define AS3(a,b,c,d) #a " " #b "," #c "," #d
#define AS1(a,b) #a " " #b
#else
#define AS1(a,b) "a	b"
#define AS2(a,b,c) "a	b,c"
#define AS2C(b,c) " b,c"
#define AS3(a,b,c,d) "a	b,c,d"
#endif
#define OUT_AS1(a,b) output_asm_insn (AS1(a,b), operands)
#define OUT_AS2(a,b,c) output_asm_insn (AS2(a,b,c), operands)
#define CR_TAB "\n\t"

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define DWARF2_DEBUGGING_INFO 1

#define DWARF2_ADDR_SIZE 4

#define OBJECT_FORMAT_ELF

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG) \
  avr_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
struct GTY(()) machine_function
{
  /* 'true' - if current function is a naked function.  */
  int is_naked;

  /* 'true' - if current function is an interrupt function 
     as specified by the "interrupt" attribute.  */
  int is_interrupt;

  /* 'true' - if current function is a signal function 
     as specified by the "signal" attribute.  */
  int is_signal;
  
  /* 'true' - if current function is a 'task' function 
     as specified by the "OS_task" attribute.  */
  int is_OS_task;

  /* 'true' - if current function is a 'main' function 
     as specified by the "OS_main" attribute.  */
  int is_OS_main;
  
  /* Current function stack size.  */
  int stack_usage;
};
