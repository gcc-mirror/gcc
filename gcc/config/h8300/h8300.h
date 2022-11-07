/* Definitions of target machine for GNU compiler.
   Renesas H8/300 (generic)
   Copyright (C) 1992-2022 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com),
   Jim Wilson (wilson@cygnus.com), and Doug Evans (dje@cygnus.com).

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

#ifndef GCC_H8300_H
#define GCC_H8300_H

extern int cpu_type;

/* Various globals defined in h8300.cc.  */

extern const char *h8_push_op, *h8_pop_op, *h8_mov_op;
extern const char * const *h8_reg_names;

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      if (TARGET_H8300SX)				\
	{						\
	  builtin_define ("__H8300SX__");		\
	  if (TARGET_NORMAL_MODE)			\
	    {						\
	      builtin_define ("__NORMAL_MODE__");	\
	    }						\
	}						\
      else if (TARGET_H8300S)				\
	{						\
	  builtin_define ("__H8300S__");		\
	  builtin_assert ("cpu=h8300s");		\
	  builtin_assert ("machine=h8300s");		\
	  if (TARGET_NORMAL_MODE)			\
	    {						\
	      builtin_define ("__NORMAL_MODE__");	\
	    }						\
	}						\
      else						\
	{						\
	  builtin_define ("__H8300H__");		\
	  builtin_assert ("cpu=h8300h");		\
	  builtin_assert ("machine=h8300h");		\
	  if (TARGET_NORMAL_MODE)			\
	    {						\
	      builtin_define ("__NORMAL_MODE__");	\
	    }						\
	}						\
    }							\
  while (0)

#define LIB_SPEC "%{mrelax:-relax} %{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

/* Macros used in the machine description to test the flags.  */

/* Select between the H8/300 and H8/300H CPUs.  */
#define TARGET_H8300S	(TARGET_H8300S_1 || TARGET_H8300SX)
/* Some multiply instructions are not available in all H8SX variants.
   Use this macro instead of TARGET_H8300SX to indicate this, even
   though we don't actually generate different code for now.  */
#define TARGET_H8300SXMUL TARGET_H8300SX

#ifdef IN_LIBGCC2
#undef TARGET_H8300H
#undef TARGET_H8300S
#undef TARGET_NORMAL_MODE
/* If compiling libgcc2, make these compile time constants based on what
   flags are we actually compiling with.  */
#ifdef __H8300H__
#define TARGET_H8300H	1
#else
#define TARGET_H8300H	0
#endif
#ifdef __H8300S__
#define TARGET_H8300S	1
#else
#define TARGET_H8300S	0
#endif
#ifdef __NORMAL_MODE__
#define TARGET_NORMAL_MODE 1
#else
#define TARGET_NORMAL_MODE 0
#endif
#endif /* !IN_LIBGCC2 */

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_H8300H | MASK_QUICKCALL)
#endif

/* We want dwarf2 info available to gdb.  */
#define DWARF2_DEBUGGING_INFO        1

/* The return address is pushed on the stack.  */
#define INCOMING_RETURN_ADDR_RTX   gen_rtx_MEM (Pmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM))
#define INCOMING_FRAME_SP_OFFSET   (POINTER_SIZE / 8)

#define DWARF_CIE_DATA_ALIGNMENT	2

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.

   Calls through a register are cheaper than calls to named
   functions; however, the register pressure this causes makes
   CSEing of function addresses generally a lose.  */
#define NO_FUNCTION_CSE 1

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the H8/300.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the H8/300.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

#define MAX_BITS_PER_WORD	32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD		4
#define MIN_UNITS_PER_WORD	2

#define SHORT_TYPE_SIZE	16
#define INT_TYPE_SIZE		(TARGET_INT32 ? 32 : 16)
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64
#define FLOAT_TYPE_SIZE	32
#define DOUBLE_TYPE_SIZE	32
#define LONG_DOUBLE_TYPE_SIZE	DOUBLE_TYPE_SIZE

#define MAX_FIXED_MODE_SIZE	32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
/* One can argue this should be 32 for -mint32, but since 32-bit ints only
   need 16-bit alignment, this is left as is so that -mint32 doesn't change
   structure layouts.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this.
   32-bit values are aligned as such on the H8/300H and H8S for speed.  */
#define BIGGEST_ALIGNMENT \
((! TARGET_ALIGN_300) ? 32 : 16)

/* The stack goes in 32 bit lumps.  */
#define STACK_BOUNDARY 32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
/* On the H8/300, longs can be aligned on halfword boundaries, but not
   byte boundaries.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.

   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   Reg 9 does not correspond to any hardware register, but instead
   appears in the RTL as an argument pointer prior to reload, and is
   eliminated during reloading in favor of either the stack or frame
   pointer.  */

#define FIRST_PSEUDO_REGISTER 13

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */

#define FIXED_REGISTERS				\
/* r0 r1 r2 r3 r4 r5 r6 r7 mac ap rap fp cc */	\
  { 0, 0, 0, 0, 0, 0, 0, 1,  0, 1,  1, 1, 1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.

   H8 destroys r0,r1,r2,r3.  */

#define CALL_USED_REGISTERS			\
/* r0 r1 r2 r3 r4 r5 r6 r7 mac ap rap fp */	\
  { 1, 1, 1, 1, 0, 0, 0, 1,  1, 1,  1, 1, 1 }

#define REG_ALLOC_ORDER				\
/* r0 r1 r2 r3 r4 r5 r6 r7 mac ap rap  fp */	\
  { 2, 3, 0, 1, 4, 5, 6, 8,  7, 9, 10, 11, 12 }

/* A C expression that is nonzero if hard register NEW_REG can be
   considered for use as a rename register for OLD_REG register */

#define HARD_REGNO_RENAME_OK(OLD_REG, NEW_REG)		\
   h8300_hard_regno_rename_ok (OLD_REG, NEW_REG)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* H8/300 pc is not overloaded on a register.  */

/*#define PC_REGNUM 15*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM SP_REG

/* Base register for access to local variables of the function.  */
#define HARD_FRAME_POINTER_REGNUM HFP_REG

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM FP_REG

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM AP_REG

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM SC_REG

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM RAP_REG

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is already the frame pointer of the COUNT frame, assuming
   a stack layout with the frame pointer as the first saved register.  */
#define RETURN_ADDR_RTX(COUNT, FRAME) h8300_return_addr_rtx ((COUNT), (FRAME))

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

enum reg_class {
  NO_REGS, COUNTER_REGS, SOURCE_REGS, DESTINATION_REGS,
  NOT_R0_REGS, NOT_R1_REGS, NOT_R2_REGS, NOT_R3_REGS,
  NOT_R4_REGS, NOT_R5_REGS, NOT_R6_REGS, NOT_SP_REGS,
  GENERAL_REGS, MAC_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES \
{ "NO_REGS", "COUNTER_REGS", "SOURCE_REGS", "DESTINATION_REGS", \
  "NOT_R0_REGS", "NOT_R1_REGS", "NOT_R2_REGS", "NOT_R3_REGS", \
  "NOT_R4_REGS", "NOT_R5_REGS", "NOT_R6_REGS", "NOT_SP_REGS", \
  "GENERAL_REGS", "MAC_REGS", "ALL_REGS", "LIM_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS			\
{      {0},		/* No regs      */	\
   {0x010},		/* COUNTER_REGS */	\
   {0x020},		/* SOURCE_REGS */	\
   {0x040},		/* DESTINATION_REGS */	\
   {0x0fe},		/* NOT_R0_REGS */	\
   {0x0fd},		/* NOT_R1_REGS */	\
   {0x0fb},		/* NOT_R2_REGS */	\
   {0x0f7},		/* NOT_R3_REGS */	\
   {0x0ef},		/* NOT_R4_REGS */	\
   {0x0df},		/* NOT_R5_REGS */	\
   {0x0bf},		/* NOT_R6_REGS */	\
   {0x07f},		/* NOT_SP_REGS */	\
   {0xeff},		/* GENERAL_REGS */	\
   {0x100},		/* MAC_REGS */		\
   {0xfff},		/* ALL_REGS	*/	\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)				\
  ((REGNO) == MAC_REG ? MAC_REGS			\
   : (REGNO) == COUNTER_REG ? COUNTER_REGS		\
   : (REGNO) == SOURCE_REG ? SOURCE_REGS		\
   : (REGNO) == DESTINATION_REG ? DESTINATION_REGS	\
   : GENERAL_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS (TARGET_H8300SX ? GENERAL_REGS : NO_REGS)
#define BASE_REG_CLASS  GENERAL_REGS

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */

#define STACK_GROWS_DOWNWARD 1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */

#define FRAME_GROWS_DOWNWARD 1

#define PUSH_ROUNDING(BYTES) h8300_push_rounding (BYTES)

/* Offset of first parameter from the argument pointer register value.  */
/* Is equal to the size of the saved fp + pc, even if an fp isn't
   saved since the value is used before we know.  */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have three registers that can be eliminated on the h8300.
   First, the frame pointer register can often be eliminated in favor
   of the stack pointer register.  Secondly, the argument pointer
   register and the return address pointer register are always
   eliminated; they are replaced with either the stack or frame
   pointer.  */

#define ELIMINABLE_REGS						\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { RETURN_ADDRESS_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},			\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)		\
  ((OFFSET) = h8300_initial_elimination_offset ((FROM), (TO)))

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

/*#define PCC_STATIC_STRUCT_RETURN*/

/* 1 if N is a possible register number for function argument passing.
   On the H8, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) (TARGET_QUICKCALL ? N < 3 : 0)

/* When this hook returns true for MODE, the compiler allows
   registers explicitly used in the rtl to be used as spill registers
   but prevents the compiler from extending the lifetime of these
   registers.  */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P hook_bool_mode_true

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the H8/300, this is a two item struct, the first is the number
   of bytes scanned so far and the second is the rtx of the called
   library function if any.  */

#define CUMULATIVE_ARGS struct cum_arg
struct cum_arg
{
  int nbytes;
  rtx libcall;
};

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the H8/300, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM).nbytes = 0, (CUM).libcall = LIBNAME)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\t%s\t#.LP%d,%s\n\tjsr @mcount\n", \
	   h8_mov_op, (LABELNO), h8_reg_names[0]);

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE ((Pmode == HImode) ? 8 : 12)

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1
#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_DECREMENT TARGET_H8300SX
#define HAVE_PRE_INCREMENT TARGET_H8300SX

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.cc during register
   allocation.  */

#define REGNO_OK_FOR_INDEX_P(regno) 0

#define REGNO_OK_FOR_BASE_P(regno)				\
  (((regno) < FIRST_PSEUDO_REGISTER && regno != MAC_REG)	\
   || reg_renumber[regno] >= 0)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)					\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF	\
   || (GET_CODE (X) == CONST_INT				\
       /* We handle signed and unsigned offsets here.  */	\
       && INTVAL (X) > -0x1000000				\
       && INTVAL (X) < 0x1000000)				\
   || (GET_CODE (X) == HIGH || GET_CODE (X) == CONST))

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

/* Non-strict versions.  */
#define REG_OK_FOR_INDEX_NONSTRICT_P(X) 0
/* Don't use REGNO_OK_FOR_BASE_P here because it uses reg_renumber.  */
#define REG_OK_FOR_BASE_NONSTRICT_P(X)				\
  (REGNO (X) >= FIRST_PSEUDO_REGISTER || REGNO (X) != MAC_REG)

/* Strict versions.  */
#define REG_OK_FOR_INDEX_STRICT_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_STRICT_P(X)  REGNO_OK_FOR_BASE_P (REGNO (X))

#ifndef REG_OK_STRICT

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_INDEX_NONSTRICT_P (X)
#define REG_OK_FOR_BASE_P(X)  REG_OK_FOR_BASE_NONSTRICT_P (X)

#else

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_INDEX_STRICT_P (X)
#define REG_OK_FOR_BASE_P(X)  REG_OK_FOR_BASE_STRICT_P (X)

#endif


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define this as 1 if `char' should by default be signed; else as 0.

   On the H8/300, sign extension is expensive, so we'll say that chars
   are unsigned.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX	4
#define MAX_MOVE_MAX	4

#define SELECT_CC_MODE(OP, X, Y)       h8300_select_cc_mode (OP, X, Y)

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS TARGET_SLOWBYTE

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode								      \
  (!TARGET_NORMAL_MODE ? SImode : HImode)

/* ANSI C types.
   We use longs for the H8/300H and the H8S because ints can be 16 or 32.
   GCC requires SIZE_TYPE to be the same size as pointers.  */
#define SIZE_TYPE								\
  (TARGET_NORMAL_MODE ? TARGET_INT32 ? "short unsigned int" : "unsigned int" : "long unsigned int")
#define PTRDIFF_TYPE						\
  (TARGET_NORMAL_MODE ? TARGET_INT32 ? "short int" : "int" : "long int")

#define POINTER_SIZE							\
  (!TARGET_NORMAL_MODE ? 32 : 16)

#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE 16

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Return the length of JUMP's delay slot insn (0 if it has none).
   If JUMP is a delayed branch, NEXT_INSN (PREV_INSN (JUMP)) will
   be the containing SEQUENCE, not JUMP itself.  */
#define DELAY_SLOT_LENGTH(JUMP) \
  (NEXT_INSN (PREV_INSN (JUMP)) == JUMP ? 0 : 2)

#define BRANCH_COST(speed_p, predictable_p) 0

/* Tell final.cc how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the h8300.  */

/* The add insns don't set overflow in a usable way.  */
#define CC_OVERFLOW_UNUSABLE 01000
/* The mov,and,or,xor insns don't set carry.  That's OK though as the
   Z bit is all we need when doing unsigned comparisons on the result of
   these insns (since they're always with 0).  However, conditions.h has
   CC_NO_OVERFLOW defined for this purpose.  Rename it to something more
   understandable.  */
#define CC_NO_CARRY CC_NO_OVERFLOW

/* Control the assembler format that we output.  */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "; #APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "; #NO_APP\n"

#define FILE_ASM_OP "\t.file\n"

/* The assembler op to get a word, 2 bytes for the H8/300, 4 for H8/300H.  */
#define ASM_WORD_OP							\
  (TARGET_NORMAL_MODE ? "\t.word\t" : "\t.long\t")

#define TEXT_SECTION_ASM_OP "\t.section .text"
#define DATA_SECTION_ASM_OP "\t.section .data"
#define BSS_SECTION_ASM_OP "\t.section .bss"

#undef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY			\
{						\
  extern func_ptr __ctors[];			\
  extern func_ptr __ctors_end[];		\
  func_ptr *p;					\
  for (p = __ctors_end; p > __ctors; )		\
    {						\
      (*--p)();					\
    }						\
}

#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY			\
{						\
  extern func_ptr __dtors[];			\
  extern func_ptr __dtors_end[];		\
  func_ptr *p;					\
  for (p = __dtors; p < __dtors_end; p++)	\
    {						\
      (*p)();					\
    }						\
}

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "sp", "mac", "ap", "rap", "fp", "cc" }

#define ADDITIONAL_REGISTER_NAMES \
{ {"er0", 0}, {"er1", 1}, {"er2", 2}, {"er3", 3}, {"er4", 4}, \
  {"er5", 5}, {"er6", 6}, {"er7", 7}, {"r7", 7} }

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global "

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
   ASM_OUTPUT_LABEL (FILE, NAME)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.

   N.B.: The h8300.md branch_true and branch_false patterns also know
   how to generate internal labels.  */
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*.%s%lu", PREFIX, (unsigned long)(NUM))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO) \
  fprintf (FILE, "\t%s\t%s\n", h8_push_op, h8_reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE, REGNO) \
  fprintf (FILE, "\t%s\t%s\n", h8_pop_op, h8_reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "%s.L%d\n", ASM_WORD_OP, VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "%s.L%d-.L%d\n", ASM_WORD_OP, VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)		\
  if ((LOG) != 0)				\
    fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE, SIZE) \
  fprintf (FILE, "\t.space %d\n", (int)(SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.comm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%lu\n", (unsigned long)(SIZE)))

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (int)(SIZE)))

#define ASM_PN_FORMAT "%s___%lu"

/* H8300 specific pragmas.  */
#define REGISTER_TARGET_PRAGMAS()				\
  do								\
    {								\
      c_register_pragma (0, "saveall", h8300_pr_saveall);	\
      c_register_pragma (0, "interrupt", h8300_pr_interrupt);	\
    }								\
  while (0)

#define FINAL_PRESCAN_INSN(insn, operand, nop)	\
  final_prescan_insn (insn, operand, nop)

extern int h8300_move_ratio;
#define MOVE_RATIO(speed) h8300_move_ratio

/* Machine-specific symbol_ref flags.  */
#define SYMBOL_FLAG_FUNCVEC_FUNCTION	(SYMBOL_FLAG_MACH_DEP << 0)
#define SYMBOL_FLAG_EIGHTBIT_DATA	(SYMBOL_FLAG_MACH_DEP << 1)
#define SYMBOL_FLAG_TINY_DATA		(SYMBOL_FLAG_MACH_DEP << 2)

#endif /* ! GCC_H8300_H */
