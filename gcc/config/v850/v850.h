/* Definitions of target machine for GNU compiler. NEC V850 series
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

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_V850_H
#define GCC_V850_H

extern GTY(()) rtx v850_compare_op0;
extern GTY(()) rtx v850_compare_op1;

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:--start-group -lc -lgcc --end-group}}"

#undef ENDFILE_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC
#undef ASM_SPEC

#define TARGET_CPU_generic 	1
#define TARGET_CPU_v850e   	2
#define TARGET_CPU_v850e1	3
#define TARGET_CPU_v850e2	4
#define TARGET_CPU_v850e2v3	5
#define TARGET_CPU_v850e3v5	6

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT	TARGET_CPU_generic
#endif

#define MASK_DEFAULT            MASK_V850
#define SUBTARGET_ASM_SPEC 	"%{!mv*:-mv850}"
#define SUBTARGET_CPP_SPEC 	"%{!mv*:-D__v850__}"

/* Choose which processor will be the default.
   We must pass a -mv850xx option to the assembler if no explicit -mv* option
   is given, because the assembler's processor default may not be correct.  */
#if TARGET_CPU_DEFAULT == TARGET_CPU_v850e
#undef  MASK_DEFAULT
#define MASK_DEFAULT            MASK_V850E
#undef  SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC 	"%{!mv*:-mv850e}"
#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC 	"%{!mv*:-D__v850e__}"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_v850e1
#undef  MASK_DEFAULT
#define MASK_DEFAULT            MASK_V850E     /* No practical difference.  */     
#undef  SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC	"%{!mv*:-mv850e1}"
#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC	"%{!mv*:-D__v850e1__} %{mv850e1:-D__v850e1__}"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_v850e2
#undef  MASK_DEFAULT
#define MASK_DEFAULT            MASK_V850E2	
#undef  SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC 	"%{!mv*:-mv850e2}"
#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC 	"%{!mv*:-D__v850e2__} %{mv850e2:-D__v850e2__}"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_v850e2v3
#undef  MASK_DEFAULT
#define MASK_DEFAULT            MASK_V850E2V3
#undef  SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC	"%{!mv*:-mv850e2v3}"
#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC	"%{!mv*:-D__v850e2v3__} %{mv850e2v3:-D__v850e2v3__}"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_v850e3v5
#undef  MASK_DEFAULT
#define MASK_DEFAULT            MASK_V850E3V5
#undef  SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC	"%{!mv*:-mv850e3v5}"
#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC	"%{!mv*:-D__v850e3v5__} %{mv850e3v5:-D__v850e3v5__}"
#undef  TARGET_VERSION
#define TARGET_VERSION		fprintf (stderr, " (Renesas V850E3V5)");
#endif

#define TARGET_V850E3V5_UP ((TARGET_V850E3V5))     
#define TARGET_V850E2V3_UP ((TARGET_V850E2V3) || TARGET_V850E3V5_UP)
#define TARGET_V850E2_UP   ((TARGET_V850E2)   || TARGET_V850E2V3_UP)
#define TARGET_V850E_UP    ((TARGET_V850E)    || TARGET_V850E2_UP)
#define TARGET_ALL         ((TARGET_V850)     || TARGET_V850E_UP)

#define ASM_SPEC "%{m850es:-mv850e1}%{!mv850es:%{mv*:-mv%*}} \
%{mrelax:-mrelax} \
%{m8byte-align:-m8byte-align} \
%{msoft-float:-msoft-float} \
%{mhard-float:-mhard-float} \
%{mgcc-abi:-mgcc-abi}"

#define LINK_SPEC "%{mgcc-abi:-m v850}"

#define CPP_SPEC "\
  %{mv850e3v5:-D__v850e3v5__} \
  %{mv850e2v3:-D__v850e2v3__} \
  %{mv850e2:-D__v850e2__} \
  %{mv850es:-D__v850e1__} \
  %{mv850e1:-D__v850e1__} \
  %{mv850e:-D__v850e__} \
  %{mv850:-D__v850__} \
  %(subtarget_cpp_spec) \
  %{mep:-D__EP__}"

#define EXTRA_SPECS \
 { "subtarget_asm_spec", SUBTARGET_ASM_SPEC }, \
 { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC } 


/* Macro to decide when FPU instructions can be used.  */
#define TARGET_USE_FPU  (TARGET_V850E2V3_UP && ! TARGET_SOFT_FLOAT)

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define( "__v851__" );		\
      builtin_define( "__v850" );		\
      builtin_define( "__v850__" );		\
      builtin_assert( "machine=v850" );		\
      builtin_assert( "cpu=v850" );		\
      if (TARGET_EP)				\
	builtin_define ("__EP__");		\
      if (TARGET_GCC_ABI)			\
	builtin_define ("__V850_GCC_ABI__");	\
      else					\
	builtin_define ("__V850_RH850_ABI__");	\
      if (! TARGET_DISABLE_CALLT)		\
	builtin_define ("__V850_CALLT__");	\
      if (TARGET_8BYTE_ALIGN)			\
	builtin_define ("__V850_8BYTE_ALIGN__");\
      builtin_define (TARGET_USE_FPU ?		\
		      "__FPU_OK__" : "__NO_FPU__");\
    }						\
  while(0)

#define MASK_CPU (MASK_V850 | MASK_V850E | MASK_V850E1 | MASK_V850E2 | MASK_V850E2V3 | MASK_V850E3V5)

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the NEC V850.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is not true on the NEC V850.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered.
   This is not true on the NEC V850.  */
#define WORDS_BIG_ENDIAN 0

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   Some simple experiments have shown that leaving UNSIGNEDP alone
   generates the best overall code.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT \
      && GET_MODE_SIZE (MODE) < 4)      \
    { (MODE) = SImode; }

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY		32

/* The stack goes in 32-bit lumps.  */
#define STACK_BOUNDARY 		BIGGEST_ALIGNMENT

/* Allocation boundary (in *bits*) for the code of a function.
   16 is the minimum boundary; 32 would give better performance.  */
#define FUNCTION_BOUNDARY 	(((! TARGET_GCC_ABI) || optimize_size) ? 16 : 32)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT	(TARGET_8BYTE_ALIGN ? 64 : 32)

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* No structure field wants to be aligned rounder than this.  */
#define BIGGEST_FIELD_ALIGNMENT BIGGEST_ALIGNMENT

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT  (!TARGET_NO_STRICT_ALIGN)

/* Define this as 1 if `char' should by default be signed; else as 0.

   On the NEC V850, loads do sign extension, so make this default.  */
#define DEFAULT_SIGNED_CHAR 1

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

#define FIRST_PSEUDO_REGISTER 36

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS \
  { 1, 1, 1, 1, 1, 1, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 1, 0, \
    1, 1,	\
    1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.  */

#define CALL_USED_REGISTERS \
  { 1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 1, 1, 1, 1, \
    1, 1, 1, 1, 0, 0, 0, 0, \
    0, 0, 0, 0, 0, 0, 1, 1, \
    1, 1,	\
    1, 1}

/* List the order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.

   On the 850, we make the return registers first, then all of the volatile
   registers, then the saved registers in reverse order to better save the
   registers with an out of line function, and finally the fixed
   registers.  */

#define REG_ALLOC_ORDER							\
{									\
  10, 11,				/* return registers */		\
  12, 13, 14, 15, 16, 17, 18, 19,	/* scratch registers */		\
   6,  7,  8,  9, 31,			/* argument registers */	\
  29, 28, 27, 26, 25, 24, 23, 22,	/* saved registers */		\
  21, 20,  2,								\
   0,  1,  3,  4,  5, 30, 32, 33,      /* fixed registers */           \
  34, 35								\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If TARGET_HARD_REGNO_MODE_OK could produce different values for MODE1
   and MODE2, for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (MODE1 == MODE2 || (GET_MODE_SIZE (MODE1) <= 4 && GET_MODE_SIZE (MODE2) <= 4))


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
  NO_REGS, EVEN_REGS, GENERAL_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES \
{ "NO_REGS", "EVEN_REGS", "GENERAL_REGS", "ALL_REGS", "LIM_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS                     \
{                                              \
  { 0x00000000,0x0 }, /* NO_REGS      */       \
  { 0x55555554,0x0 }, /* EVEN_REGS */          \
  { 0xfffffffe,0x0 }, /* GENERAL_REGS */       \
  { 0xffffffff,0x0 }, /* ALL_REGS      */      \
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)  ((REGNO == CC_REGNUM || REGNO == FCC_REGNUM) ? NO_REGS : GENERAL_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS  GENERAL_REGS

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */
 
#define REGNO_OK_FOR_BASE_P(regno)             \
  (((regno) < FIRST_PSEUDO_REGISTER            \
    && (regno) != CC_REGNUM                    \
    && (regno) != FCC_REGNUM)                  \
   || reg_renumber[regno] >= 0)

#define REGNO_OK_FOR_INDEX_P(regno) 0

/* Convenience wrappers around insn_const_int_ok_for_constraint.  */

#define CONST_OK_FOR_I(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_I)
#define CONST_OK_FOR_J(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_J)
#define CONST_OK_FOR_K(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_K)
#define CONST_OK_FOR_L(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_L)
#define CONST_OK_FOR_M(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_M)
#define CONST_OK_FOR_N(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_N)
#define CONST_OK_FOR_O(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_O)
#define CONST_OK_FOR_W(VALUE) \
  insn_const_int_ok_for_constraint (VALUE, CONSTRAINT_W)

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

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM SP_REGNUM

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 34

/* Register containing return address from latest function call.  */
#define LINK_POINTER_REGNUM LP_REGNUM
     
/* On some machines the offset between the frame pointer and starting
   offset of the automatic variables is not known until after register
   allocation has been done (for example, because the saved registers
   are between these two locations).  On those machines, define
   `FRAME_POINTER_REGNUM' the number of a special, fixed register to
   be used internally until the offset is known, and define
   `HARD_FRAME_POINTER_REGNUM' to be actual the hard register number
   used for the frame pointer.

   You should define this macro only in the very rare circumstances
   when it is not possible to calculate the offset between the frame
   pointer and the automatic variables until after register
   allocation has been completed.  When this macro is defined, you
   must also indicate in your definition of `ELIMINABLE_REGS' how to
   eliminate `FRAME_POINTER_REGNUM' into either
   `HARD_FRAME_POINTER_REGNUM' or `STACK_POINTER_REGNUM'.

   Do not define this macro if it would be the same as
   `FRAME_POINTER_REGNUM'.  */
#undef  HARD_FRAME_POINTER_REGNUM 
#define HARD_FRAME_POINTER_REGNUM 29

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 35

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 20

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   The definition of this macro is a list of structure
   initializations, each of which specifies an original and
   replacement register.

   On some machines, the position of the argument pointer is not
   known until the compilation is completed.  In such a case, a
   separate hard register must be used for the argument pointer.
   This register can be eliminated by replacing it with either the
   frame pointer or the argument pointer, depending on whether or not
   the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS							\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },			\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },			\
 { ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM },			\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM }}			\

/* This macro returns the initial difference between the specified pair
   of registers.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = get_frame_size () + crtl->outgoing_args_size;	\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
   (OFFSET) = compute_frame_size (get_frame_size (), (long *)0);	\
  else									\
    gcc_unreachable ();							\
}

/* Keep the stack pointer constant throughout the function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

#define RETURN_ADDR_RTX(COUNT, FP) v850_return_addr (COUNT)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */

#define CUMULATIVE_ARGS struct cum_arg
struct cum_arg { int nbytes; };

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  do { (CUM).nbytes = 0; } while (0)

/* When a parameter is passed in a register, stack space is still
   allocated for it.  */
#define REG_PARM_STACK_SPACE(DECL) 0

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) (N >= 6 && N <= 9)

#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Define this macro as a C expression that is nonzero for registers
   used by the epilogue or the `return' pattern.  */

#define EPILOGUE_USES(REGNO) \
  (reload_completed && (REGNO) == LINK_POINTER_REGNUM)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) ;

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 24

/* Addressing modes, and classification of registers for them.  */


/* 1 if X is an rtx for a constant that is a valid address.  */

/* ??? This seems too exclusive.  May get better code by accepting more
   possibilities here, in particular, should accept ZDA_NAME SYMBOL_REFs.  */

#define CONSTANT_ADDRESS_P(X) constraint_satisfied_p (X, CONSTRAINT_K)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.

   For floating-point equality comparisons, CCFPEQmode should be used.
   VOIDmode should be used in all other cases.

   For integer comparisons against zero, reduce to CCNOmode or CCZmode if
   possible, to allow for more combinations.  */

#define SELECT_CC_MODE(OP, X, Y)       v850_select_cc_mode (OP, X, Y)

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the VAX.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define CC_OVERFLOW_UNUSABLE 0x200
#define CC_NO_CARRY CC_NO_OVERFLOW
#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP, INSN)

/* Nonzero if access to memory by bytes or half words is no faster
   than accessing full words.  */
#define SLOW_BYTE_ACCESS 1

/* According expr.c, a value of around 6 should minimize code size, and
   for the V850 series, that's our primary concern.  */
#define MOVE_RATIO(speed) 6

/* Indirect calls are expensive, never turn a direct call
   into an indirect call.  */
#define NO_FUNCTION_CSE 1

/* The four different data regions on the v850.  */
typedef enum 
{
  DATA_AREA_NORMAL,
  DATA_AREA_SDA,
  DATA_AREA_TDA,
  DATA_AREA_ZDA
} v850_data_area;

#define TEXT_SECTION_ASM_OP  "\t.section .text"
#define DATA_SECTION_ASM_OP  "\t.section .data"
#define BSS_SECTION_ASM_OP   "\t.section .bss"
#define SDATA_SECTION_ASM_OP "\t.section .sdata,\"aw\""
#define SBSS_SECTION_ASM_OP  "\t.section .sbss,\"aw\""

#define SCOMMON_ASM_OP 	       "\t.scomm\t"
#define ZCOMMON_ASM_OP 	       "\t.zcomm\t"
#define TCOMMON_ASM_OP 	       "\t.tcomm\t"

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
   uninitialized but not common symbol.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

#undef  ASM_OUTPUT_ALIGNED_BSS 
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  v850_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* This says how to output the assembler to define a global
   uninitialized, common symbol.  */
#undef  ASM_OUTPUT_ALIGNED_COMMON
#undef  ASM_OUTPUT_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN) \
     v850_output_common (FILE, DECL, NAME, SIZE, ALIGN)

/* This says how to output the assembler to define a local
   uninitialized symbol.  */
#undef  ASM_OUTPUT_ALIGNED_LOCAL
#undef  ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN) \
     v850_output_local (FILE, DECL, NAME, SIZE, ALIGN)
     
/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global "

#define ASM_PN_FORMAT "%s___%lu"

/* This is how we tell the assembler that two symbols have the same value.  */

#define ASM_OUTPUT_DEF(FILE,NAME1,NAME2) \
  do { assemble_name(FILE, NAME1); 	 \
       fputs(" = ", FILE);		 \
       assemble_name(FILE, NAME2);	 \
       fputc('\n', FILE); } while (0)


/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES                                         \
{  "r0",  "r1",  "r2",  "sp",  "gp",  "r5",  "r6" , "r7",      \
   "r8",  "r9", "r10", "r11", "r12", "r13", "r14", "r15",      \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",      \
  "r24", "r25", "r26", "r27", "r28", "r29",  "ep", "r31",      \
  "psw", "fcc",      \
  ".fp", ".ap"}

/* Register numbers */

#define ADDITIONAL_REGISTER_NAMES              \
{ { "zero",    ZERO_REGNUM },                  \
  { "hp",      2 },                            \
  { "r3",      3 },                            \
  { "r4",      4 },                            \
  { "tp",      5 },                            \
  { "fp",      29 },                           \
  { "r30",     30 },                           \
  { "lp",      LP_REGNUM} }

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t%s .L%d\n",					\
	   (TARGET_BIG_SWITCH ? ".long" : ".short"), VALUE)

/* This is how to output an element of a case-vector that is relative.  */

/* Disable the shift, which is for the currently disabled "switch"
   opcode.  Se casesi in v850.md.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) 		\
  fprintf (FILE, "\t%s %s.L%d-.L%d%s\n",				\
	   (TARGET_BIG_SWITCH ? ".long" : ".short"),			\
	   (0 && ! TARGET_BIG_SWITCH && (TARGET_V850E_UP) ? "(" : ""),             \
	   VALUE, REL,							\
	   (0 && ! TARGET_BIG_SWITCH && (TARGET_V850E_UP) ? ")>>1" : ""))

#define ASM_OUTPUT_ALIGN(FILE, LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (LOG))

/* We don't have to worry about dbx compatibility for the v850.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* Use dwarf2 debugging info by default.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE   DWARF2_DEBUG

#define DWARF2_FRAME_INFO          1
#define DWARF2_UNWIND_INFO         0
#define INCOMING_RETURN_ADDR_RTX   gen_rtx_REG (Pmode, LINK_POINTER_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN  DWARF_FRAME_REGNUM (LINK_POINTER_REGNUM)

#ifndef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*.%s%u", PREFIX, (unsigned int)(NUM))
#endif

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_BIG_SWITCH ? SImode : HImode)

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE 1

/* The switch instruction requires that the jump table immediately follow
   it.  */
#define JUMP_TABLES_IN_TEXT_SECTION (!TARGET_JUMP_TABLES_IN_DATA_SECTION)

#undef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE,PREFIX,NUM,TABLE) \
  ASM_OUTPUT_ALIGN ((FILE), (TARGET_BIG_SWITCH ? 2 : 1));

#define WORD_REGISTER_OPERATIONS 1

/* Byte and short loads sign extend the value to a word.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

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

/* Tell compiler we want to support GHS pragmas */
#define REGISTER_TARGET_PRAGMAS() do {				\
  c_register_pragma ("ghs", "interrupt", ghs_pragma_interrupt);	\
  c_register_pragma ("ghs", "section",   ghs_pragma_section);	\
  c_register_pragma ("ghs", "starttda",  ghs_pragma_starttda);	\
  c_register_pragma ("ghs", "startsda",  ghs_pragma_startsda);	\
  c_register_pragma ("ghs", "startzda",  ghs_pragma_startzda);	\
  c_register_pragma ("ghs", "endtda",    ghs_pragma_endtda);	\
  c_register_pragma ("ghs", "endsda",    ghs_pragma_endsda);	\
  c_register_pragma ("ghs", "endzda",    ghs_pragma_endzda);	\
} while (0)

/* enum GHS_SECTION_KIND is an enumeration of the kinds of sections that
   can appear in the "ghs section" pragma.  These names are used to index
   into the GHS_default_section_names[] and GHS_current_section_names[]
   that are defined in v850.c, and so the ordering of each must remain
   consistent. 

   These arrays give the default and current names for each kind of 
   section defined by the GHS pragmas.  The current names can be changed
   by the "ghs section" pragma.  If the current names are null, use 
   the default names.  Note that the two arrays have different types.

   For the *normal* section kinds (like .data, .text, etc.) we do not
   want to explicitly force the name of these sections, but would rather
   let the linker (or at least the back end) choose the name of the 
   section, UNLESS the user has forced a specific name for these section
   kinds.  To accomplish this set the name in ghs_default_section_names
   to null.  */

enum GHS_section_kind
{ 
  GHS_SECTION_KIND_DEFAULT,

  GHS_SECTION_KIND_TEXT,
  GHS_SECTION_KIND_DATA, 
  GHS_SECTION_KIND_RODATA,
  GHS_SECTION_KIND_BSS,
  GHS_SECTION_KIND_SDATA,
  GHS_SECTION_KIND_ROSDATA,
  GHS_SECTION_KIND_TDATA,
  GHS_SECTION_KIND_ZDATA,
  GHS_SECTION_KIND_ROZDATA,

  COUNT_OF_GHS_SECTION_KINDS  /* must be last */
};

/* The following code is for handling pragmas supported by the
   v850 compiler produced by Green Hills Software.  This is at
   the specific request of a customer.  */

typedef struct data_area_stack_element
{
  struct data_area_stack_element * prev;
  v850_data_area                   data_area; /* Current default data area.  */
} data_area_stack_element;

/* Track the current data area set by the
   data area pragma (which can be nested).  */
extern data_area_stack_element * data_area_stack;

/* Names of the various data areas used on the v850.  */
extern const char * GHS_default_section_names [(int) COUNT_OF_GHS_SECTION_KINDS];
extern const char * GHS_current_section_names [(int) COUNT_OF_GHS_SECTION_KINDS];

/* The assembler op to start the file.  */

#define FILE_ASM_OP "\t.file\n"

/* Implement ZDA, TDA, and SDA */

#define EP_REGNUM 30	/* ep register number */

#define SYMBOL_FLAG_ZDA		(SYMBOL_FLAG_MACH_DEP << 0)
#define SYMBOL_FLAG_TDA		(SYMBOL_FLAG_MACH_DEP << 1)
#define SYMBOL_FLAG_SDA		(SYMBOL_FLAG_MACH_DEP << 2)
#define SYMBOL_REF_ZDA_P(X)	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_ZDA) != 0)
#define SYMBOL_REF_TDA_P(X)	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_TDA) != 0)
#define SYMBOL_REF_SDA_P(X)	((SYMBOL_REF_FLAGS (X) & SYMBOL_FLAG_SDA) != 0)

#define TARGET_ASM_INIT_SECTIONS v850_asm_init_sections

/* Define this so that the cc1plus will not think that system header files
   need an implicit 'extern "C" { ... }' assumed.  This breaks testing C++
   in a build directory where the libstdc++ header files are found via a
   -isystem <path-to-build-dir>.  */
#define NO_IMPLICIT_EXTERN_C

#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  ((LENGTH) = v850_adjust_insn_length ((INSN), (LENGTH)))

#endif /* ! GCC_V850_H */
