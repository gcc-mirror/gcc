/* Definitions of target machine for GNU compiler.
   Matsushita MN10300 series
   Copyright (C) 1996-2017 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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

#undef ASM_SPEC
#undef LIB_SPEC
#undef ENDFILE_SPEC
#undef  LINK_SPEC
#define LINK_SPEC "%{mrelax:%{!r:--relax}}"
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{!mno-crt0:%{!shared:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}}}"

/* Names to predefine in the preprocessor for this target machine.  */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__mn10300__");		\
      builtin_define ("__MN10300__");		\
      builtin_assert ("cpu=mn10300");		\
      builtin_assert ("machine=mn10300");	\
						\
      if (TARGET_AM34)				\
        { 					\
          builtin_define ("__AM33__=4");	\
          builtin_define ("__AM34__");		\
        }					\
      else if (TARGET_AM33_2)			\
        { 					\
          builtin_define ("__AM33__=2");	\
          builtin_define ("__AM33_2__");	\
        }					\
      else if (TARGET_AM33)			\
        builtin_define ("__AM33__=1");		\
						\
      builtin_define (TARGET_ALLOW_LIW ?	\
		      "__LIW__" : "__NO_LIW__");\
						\
      builtin_define (TARGET_ALLOW_SETLB  ?	\
		      "__SETLB__" : "__NO_SETLB__");\
    }						\
  while (0)

#ifndef MN10300_OPTS_H
#include "config/mn10300/mn10300-opts.h"
#endif

extern enum processor_type mn10300_tune_cpu;

#define TARGET_AM33	(mn10300_processor >= PROCESSOR_AM33)
#define TARGET_AM33_2	(mn10300_processor >= PROCESSOR_AM33_2)
#define TARGET_AM34	(mn10300_processor >= PROCESSOR_AM34)

#ifndef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_MN10300
#endif


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the Matsushita MN1003.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is not true on the Matsushita MN10300.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.
   This is not true on the Matsushita MN10300.  */
#define WORDS_BIG_ENDIAN 0

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		4

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY		32

/* The stack goes in 32-bit lumps.  */
#define STACK_BOUNDARY 		32

/* Allocation boundary (in *bits*) for the code of a function.
   8 is the minimum boundary; it's unclear if bigger alignments
   would improve performance.  */
#define FUNCTION_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT	32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY    32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.

   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */

#define FIRST_PSEUDO_REGISTER 52

/* Specify machine-specific register numbers.  The commented out entries
   are defined in mn10300.md.  */
#define FIRST_DATA_REGNUM      0
#define LAST_DATA_REGNUM       3
#define FIRST_ADDRESS_REGNUM   4
/* #define PIC_REG             6 */
#define LAST_ADDRESS_REGNUM    8
/* #define SP_REG              9 */
#define FIRST_EXTENDED_REGNUM 10
#define LAST_EXTENDED_REGNUM  17
#define FIRST_FP_REGNUM       18
#define LAST_FP_REGNUM        49
/* #define MDR_REG            50 */
/* #define CC_REG             51 */
#define FIRST_ARGUMENT_REGNUM  0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM (LAST_ADDRESS_REGNUM + 1)

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM (LAST_ADDRESS_REGNUM - 1)

/* Base register for access to arguments of the function.  This
   is a fake register and will be eliminated into either the frame
   pointer or stack pointer.  */
#define ARG_POINTER_REGNUM LAST_ADDRESS_REGNUM

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (FIRST_ADDRESS_REGNUM + 1)

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
  { 0, 0, 0, 0,				/* data regs */		\
    0, 0, 0, 0,				/* addr regs */		\
    1,					/* arg reg */		\
    1,					/* sp reg */		\
    0, 0, 0, 0, 0, 0, 0, 0,		/* extended regs */	\
    0, 0,				/* fp regs (18-19) */	\
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* fp regs (20-29) */	\
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* fp regs (30-39) */	\
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* fp regs (40-49) */	\
    0,					/* mdr reg */		\
    1					/* cc reg */		\
  }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.  */

#define CALL_USED_REGISTERS \
  { 1, 1, 0, 0,				/* data regs */		\
    1, 1, 0, 0,				/* addr regs */		\
    1,					/* arg reg */		\
    1,					/* sp reg */		\
    1, 1, 1, 1, 0, 0, 0, 0,		/* extended regs */	\
    1, 1,				/* fp regs (18-19) */	\
    1, 1, 0, 0, 0, 0, 0, 0, 0, 0,	/* fp regs (20-29) */	\
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1,	/* fp regs (30-39) */	\
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* fp regs (40-49) */	\
    1,					/* mdr reg */		\
    1					/* cc reg */		\
  }

/* Note: The definition of CALL_REALLY_USED_REGISTERS is not
   redundant.  It is needed when compiling in PIC mode because
   the a2 register becomes fixed (and hence must be marked as
   call_used) but in order to preserve the ABI it is not marked
   as call_really_used.  */
#define CALL_REALLY_USED_REGISTERS CALL_USED_REGISTERS

#define REG_ALLOC_ORDER \
  { 0, 1, 4, 5, 2, 3, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 8, 9 \
  , 42, 43, 44, 45, 46, 47, 48, 49, 34, 35, 36, 37, 38, 39, 40, 41 \
  , 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 50, 51 \
  }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  mn10300_hard_regno_mode_ok ((REGNO), (MODE))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  mn10300_modes_tieable ((MODE1), (MODE2))

/* 4 data, and effectively 3 address registers is small as far as I'm
   concerned.  */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P hook_bool_mode_true

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS, DATA_REGS, ADDRESS_REGS, SP_REGS, SP_OR_ADDRESS_REGS,
  EXTENDED_REGS, FP_REGS, FP_ACC_REGS, CC_REGS, MDR_REGS,
  GENERAL_REGS, SP_OR_GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES					   		\
{ "NO_REGS", "DATA_REGS", "ADDRESS_REGS", "SP_REGS", "SP_OR_ADDRESS_REGS", \
  "EXTENDED_REGS", "FP_REGS", "FP_ACC_REGS", "CC_REGS", "MDR_REGS",	\
  "GENERAL_REGS", "SP_OR_GENERAL_REGS", "ALL_REGS", "LIM_REGS"		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS					\
{ { 0,	        0 },	  /* No regs */				\
  { 0x0000000f, 0 },	  /* DATA_REGS */			\
  { 0x000001f0, 0 },	  /* ADDRESS_REGS */			\
  { 0x00000200, 0 },	  /* SP_REGS */				\
  { 0x000003f0, 0 },	  /* SP_OR_ADDRESS_REGS */		\
  { 0x0003fc00, 0 },	  /* EXTENDED_REGS */			\
  { 0xfffc0000, 0x3ffff },/* FP_REGS */				\
  { 0x03fc0000, 0 },	  /* FP_ACC_REGS */			\
  { 0x00000000, 0x80000 },/* CC_REGS */				\
  { 0x00000000, 0x40000 },/* MDR_REGS */			\
  { 0x0003fdff, 0 }, 	  /* GENERAL_REGS */			\
  { 0x0003ffff, 0 },      /* SP_OR_GENERAL_REGS */		\
  { 0xffffffff, 0xfffff } /* ALL_REGS */			\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)			     \
  ((REGNO) <= LAST_DATA_REGNUM ? DATA_REGS :	     \
   (REGNO) <= LAST_ADDRESS_REGNUM ? ADDRESS_REGS :   \
   (REGNO) == STACK_POINTER_REGNUM ? SP_REGS :	     \
   (REGNO) <= LAST_EXTENDED_REGNUM ? EXTENDED_REGS : \
   (REGNO) <= LAST_FP_REGNUM ? FP_REGS :	     \
   (REGNO) == MDR_REG ? MDR_REGS :		     \
   (REGNO) == CC_REG ? CC_REGS :		     \
   NO_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS \
  (TARGET_AM33 ? GENERAL_REGS : DATA_REGS)
#define BASE_REG_CLASS \
  (TARGET_AM33 ? SP_OR_GENERAL_REGS : SP_OR_ADDRESS_REGS)

/* Macros to check register numbers against specific register classes.  */

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */

#ifndef REG_OK_STRICT
# define REG_STRICT 0
#else
# define REG_STRICT 1
#endif

#define REGNO_DATA_P(regno, strict) \
  mn10300_regno_in_class_p (regno, DATA_REGS, strict)
#define REGNO_ADDRESS_P(regno, strict) \
  mn10300_regno_in_class_p (regno, ADDRESS_REGS, strict)
#define REGNO_EXTENDED_P(regno, strict) \
  mn10300_regno_in_class_p (regno, EXTENDED_REGS, strict)
#define REGNO_GENERAL_P(regno, strict) \
  mn10300_regno_in_class_p (regno, GENERAL_REGS, strict)

#define REGNO_STRICT_OK_FOR_BASE_P(regno, strict) \
  mn10300_regno_in_class_p (regno, BASE_REG_CLASS, strict)
#define REGNO_OK_FOR_BASE_P(regno) \
  (REGNO_STRICT_OK_FOR_BASE_P ((regno), REG_STRICT))
#define REG_OK_FOR_BASE_P(X) \
  (REGNO_OK_FOR_BASE_P (REGNO (X)))

#define REGNO_STRICT_OK_FOR_BIT_BASE_P(regno, strict) \
  mn10300_regno_in_class_p (regno, ADDRESS_REGS, strict)
#define REGNO_OK_FOR_BIT_BASE_P(regno) \
  (REGNO_STRICT_OK_FOR_BIT_BASE_P ((regno), REG_STRICT))
#define REG_OK_FOR_BIT_BASE_P(X) \
  (REGNO_OK_FOR_BIT_BASE_P (REGNO (X)))

#define REGNO_STRICT_OK_FOR_INDEX_P(regno, strict) \
  mn10300_regno_in_class_p (regno, INDEX_REG_CLASS, strict)
#define REGNO_OK_FOR_INDEX_P(regno) \
  (REGNO_STRICT_OK_FOR_INDEX_P ((regno), REG_STRICT))
#define REG_OK_FOR_INDEX_P(X) \
  (REGNO_OK_FOR_INDEX_P (REGNO (X)))

#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  (!TARGET_AM33 && (MODE == QImode || MODE == HImode) ? DATA_REGS : CLASS)

/* A class that contains registers which the compiler must always
   access in a mode that is the same size as the mode in which it
   loaded the register.  */
#define CLASS_CANNOT_CHANGE_SIZE FP_REGS

/* Return 1 if VALUE is in the range specified.  */

#define INT_8_BITS(VALUE) ((unsigned) (VALUE) + 0x80 < 0x100)
#define INT_16_BITS(VALUE) ((unsigned) (VALUE) + 0x8000 < 0x10000)


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */

#define STACK_GROWS_DOWNWARD 1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */

#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */

#define STARTING_FRAME_OFFSET 0

/* Offset of first parameter from the argument pointer register value.  */
/* Is equal to the size of the saved fp + pc, even if an fp isn't
   saved since the value is used before we know.  */

#define FIRST_PARM_OFFSET(FNDECL) 4

/* But the CFA is at the arg pointer directly, not at the first argument.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL) 0

#define ELIMINABLE_REGS				\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  OFFSET = mn10300_initial_offset (FROM, TO)

/* We use d0/d1 for passing parameters, so allocate 8 bytes of space
   for a register flushback area.  */
#define REG_PARM_STACK_SPACE(DECL) 8
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1
#define ACCUMULATE_OUTGOING_ARGS 1

/* So we can allocate space for return pointers once for the function
   instead of around every call.  */
#define STACK_POINTER_OFFSET 4

/* 1 if N is a possible register number for function argument passing.
   On the MN10300, d0 and d1 are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) ((N) <= 1)


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the MN10300, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS struct cum_arg

struct cum_arg
{
  int nbytes;
};

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the MN10300, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM).nbytes = 0)

#define FUNCTION_VALUE_REGNO_P(N)  mn10300_function_value_regno_p (N)

#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) ;

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE		16
#define TRAMPOLINE_ALIGNMENT	32

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.

   On the mn10300, the return address is not at a constant location
   due to the frame layout.  Luckily, it is at a constant offset from
   the argument pointer, so we define RETURN_ADDR_RTX to return a
   MEM using arg_pointer_rtx.  Reload will replace arg_pointer_rtx
   with a reference to the stack/frame pointer + an appropriate offset.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)   \
  ((COUNT == 0)                         \
   ? gen_rtx_MEM (Pmode, arg_pointer_rtx) \
   : (rtx) 0)

/* The return address is saved both in the stack and in MDR.  Using
   the stack location is handiest for what unwinding needs.  */
#define INCOMING_RETURN_ADDR_RTX \
  gen_rtx_MEM (Pmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2


/* We have post-increments.  */
#define HAVE_POST_INCREMENT	TARGET_AM33
#define HAVE_POST_MODIFY_DISP	TARGET_AM33

/* ... But we don't want to use them for block moves.  Small offsets are
   just as effective, at least for inline block move sizes, and appears
   to produce cleaner code.  */
#define USE_LOAD_POST_INCREMENT(M)	0
#define USE_STORE_POST_INCREMENT(M)	0

/* Accept either REG or SUBREG where a register is valid.  */

#define RTX_OK_FOR_BASE_P(X, strict)				\
  ((REG_P (X) && REGNO_STRICT_OK_FOR_BASE_P (REGNO (X),		\
 					     (strict))) 	\
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))		\
       && REGNO_STRICT_OK_FOR_BASE_P (REGNO (SUBREG_REG (X)),	\
 				      (strict))))

#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND_L,WIN)		     \
do {									     \
  rtx new_x = mn10300_legitimize_reload_address (X, MODE, OPNUM, TYPE, IND_L); \
  if (new_x)								     \
    {									     \
      X = new_x;							     \
      goto WIN;								     \
    }									     \
} while (0)


/* Zero if this needs fixing up to become PIC.  */

#define LEGITIMATE_PIC_OPERAND_P(X) \
  mn10300_legitimate_pic_operand_p (X)

/* Register to hold the addressing base for
   position independent code access to data items.  */
#define PIC_OFFSET_TABLE_REGNUM	PIC_REG

/* The name of the pseudo-symbol representing the Global Offset Table.  */
#define GOT_SYMBOL_NAME "*_GLOBAL_OFFSET_TABLE_"

#define SYMBOLIC_CONST_P(X)	\
((GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == LABEL_REF)	\
  && ! LEGITIMATE_PIC_OPERAND_P (X))

/* Non-global SYMBOL_REFs have SYMBOL_REF_FLAG enabled.  */
#define MN10300_GLOBAL_P(X) (! SYMBOL_REF_FLAG (X))

#define SELECT_CC_MODE(OP, X, Y)  mn10300_select_cc_mode (OP, X, Y)
#define REVERSIBLE_CC_MODE(MODE)  0

/* Nonzero if access to memory by bytes or half words is no faster
   than accessing full words.  */
#define SLOW_BYTE_ACCESS 1

#define NO_FUNCTION_CSE 1

/* According expr.c, a value of around 6 should minimize code size, and
   for the MN10300 series, that's our primary concern.  */
#define MOVE_RATIO(speed) 6

#define TEXT_SECTION_ASM_OP "\t.section .text"
#define DATA_SECTION_ASM_OP "\t.section .data"
#define BSS_SECTION_ASM_OP  "\t.section .bss"

#define ASM_COMMENT_START "#"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

#undef  USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

/* This says how to output the assembler to define a global
   uninitialized but not common symbol.
   Try to use asm_output_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global "

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME) \
  asm_fprintf (FILE, "%U%s", (*targetm.strip_name_encoding) (NAME))

/* This is how we tell the assembler that two symbols have the same value.  */

#define ASM_OUTPUT_DEF(FILE,NAME1,NAME2) \
  do					 \
    {					 \
      assemble_name (FILE, NAME1);	 \
      fputs (" = ", FILE);		 \
      assemble_name (FILE, NAME2);	 \
      fputc ('\n', FILE);		 \
    }					 \
  while (0)

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{ "d0", "d1", "d2", "d3", "a0", "a1", "a2", "a3", "ap", "sp",		\
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7"			\
, "fs0", "fs1", "fs2", "fs3", "fs4", "fs5", "fs6", "fs7"		\
, "fs8", "fs9", "fs10", "fs11", "fs12", "fs13", "fs14", "fs15"		\
, "fs16", "fs17", "fs18", "fs19", "fs20", "fs21", "fs22", "fs23"	\
, "fs24", "fs25", "fs26", "fs27", "fs28", "fs29", "fs30", "fs31"	\
, "mdr", "EPSW"								\
}

#define ADDITIONAL_REGISTER_NAMES				\
{ {"r8",  4}, {"r9",  5}, {"r10", 6}, {"r11", 7},		\
  {"r12", 0}, {"r13", 1}, {"r14", 2}, {"r15", 3},		\
  {"e0", 10}, {"e1", 11}, {"e2", 12}, {"e3", 13},		\
  {"e4", 14}, {"e5", 15}, {"e6", 16}, {"e7", 17}		\
, {"fd0", 18}, {"fd2", 20}, {"fd4", 22}, {"fd6", 24}		\
, {"fd8", 26}, {"fd10", 28}, {"fd12", 30}, {"fd14", 32}		\
, {"fd16", 34}, {"fd18", 36}, {"fd20", 38}, {"fd22", 40}	\
, {"fd24", 42}, {"fd26", 44}, {"fd28", 46}, {"fd30", 48}	\
, {"cc", CC_REG}						\
}

/* Print an instruction operand X on file FILE.
   look in mn10300.c for details */

#define PRINT_OPERAND(FILE, X, CODE) \
  mn10300_print_operand (FILE, X, CODE)

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in output-vax.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) \
  mn10300_print_operand_address (FILE, ADDR)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t%s .L%d\n", ".long", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t%s .L%d-.L%d\n", ".long", VALUE, REL)

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* We don't have to worry about dbx compatibility for the mn10300.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Use dwarf2 debugging info by default.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DWARF2_DEBUGGING_INFO 1
#define DWARF2_ASM_LINE_DEBUG_INFO 1

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX	4

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* The assembler op to get a word.  */

#define FILE_ASM_OP "\t.file\n"

