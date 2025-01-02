/* Target Definitions for OpenRISC.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.
   Contributed by Stafford Horne.

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

#ifndef GCC_OR1K_H
#define GCC_OR1K_H

#include "config/or1k/or1k-opts.h"

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__OR1K__");		\
      builtin_define ("__OR1K_DELAY__");	\
      builtin_define ("__or1k__");		\
      if (TARGET_CMOV)				\
	builtin_define ("__or1k_cmov__");	\
      if (TARGET_HARD_FLOAT)			\
	builtin_define ("__or1k_hard_float__");	\
      builtin_assert ("cpu=or1k");		\
      builtin_assert ("machine=or1k");		\
    }						\
  while (0)

#define TARGET_CMODEL_SMALL \
  (or1k_code_model == CMODEL_SMALL)
#define TARGET_CMODEL_LARGE \
  (or1k_code_model == CMODEL_LARGE)

/* Storage layout.  */

#define DEFAULT_SIGNED_CHAR 1
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1
#define BITS_PER_WORD 32
#define UNITS_PER_WORD 4
#define POINTER_SIZE 32
#define BIGGEST_ALIGNMENT 32
#define STRICT_ALIGNMENT 1
#define FUNCTION_BOUNDARY 32
#define PARM_BOUNDARY 32
#define STACK_BOUNDARY 32
#define PREFERRED_STACK_BOUNDARY 32
#define MAX_FIXED_MODE_SIZE 64

/* Layout of source language data types.  */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define WCHAR_TYPE_SIZE 32

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

/* Describing Relative Costs of Operations.  */
#define MOVE_MAX 4
#define SLOW_BYTE_ACCESS 1

/* Register usage, class and contents.  */

/* In OpenRISC there are 32 general purpose registers with the following
   designations:

   r0    always 0
   r1    stack pointer
   r2    frame pointer (optional)
   r3    arg 0
   r4    arg 1
   r5    arg 2
   r6    arg 3
   r7    arg 4
   r8    arg 5
   r9    function call return link address
   r10   thread local storage
   r11   function return value & static chain
   r12   function return value high (upper 64-bit)
   r13   temporary (used in prologue and epilogue)
   r14   callee saved
   r15   temporary
   r16   callee saved & pic base register
   r17   temporary
   r18   callee saved
   r19   temporary
   r20   callee saved
   r21   temporary
   r22   callee saved
   r23   temporary
   r24   callee saved
   r25   temporary
   r26   callee saved
   r27   temporary
   r28   callee saved
   r29   temporary
   r30   callee saved
   r31   temporary

   r32   soft argument pointer
   r33   soft frame pointer
   r34   SR[F] (bit) register

   This ABI has no adjacent call-saved register, which means that
   DImode/DFmode pseudos cannot be call-saved and will always be
   spilled across calls.  To solve this without changing the ABI,
   remap the compiler internal register numbers to place the even
   call-saved registers r16-r30 in 24-31, and the odd call-clobbered
   registers r17-r31 in 16-23.  */

#define FIRST_PSEUDO_REGISTER  35

#define HW_TO_GCC_REGNO(X)		\
  ((X) < 16 || (X) > 31 ? (X)		\
   : (X) & 1 ? ((X) - 16) / 2 + 16	\
   : ((X) - 16) / 2 + 24)

#define GCC_TO_HW_REGNO(X)		\
  ((X) < 16 || (X) > 31 ? (X)		\
   : (X) < 24 ? ((X) - 16) * 2 + 17	\
   : ((X) - 24) * 2 + 16)

#define DEBUGGER_REGNO(X)  GCC_TO_HW_REGNO(X)

#define REGISTER_NAMES { \
  "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7",   \
  "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "r15",  \
  "r17",  "r19",  "r21",  "r23",  "r25",  "r27",  "r29",  "r31",  \
  "r16",  "r18",  "r20",  "r22",  "r24",  "r26",  "r28",  "r30",  \
  "?ap",  "?fp",  "?sr_f" }

#define FIXED_REGISTERS		\
{ 1, 1, 0, 0, 0, 0, 0, 0,	\
  0, 0, 1, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1 }

/* Caller saved/temporary registers + args + fixed */
#define CALL_USED_REGISTERS	\
{ 1, 1, 0, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 0, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1 }

/* List the order in which to allocate registers.  Each register must
   be listed once, even those in FIXED_REGISTERS.

   ??? Note that placing REAL_PIC_OFFSET_TABLE_REGNUM (r16 = 24) first
   happens to make it most likely selected *as* the pic register when
   compiling without optimization, simply because the pic pseudo happens
   to be allocated with the lowest pseudo regno.  */

#define REG_ALLOC_ORDER { \
  16, 17, 18, 19, 20, 21, 22, 23,	/* r17-r31 (odd), non-saved */	\
  13, 15,				/* non-saved */			\
  12, 11,				/* non-saved return values */	\
  8, 7, 6, 5, 4, 3,			/* non-saved argument regs */	\
  24,					/* r16, saved, pic reg */	\
  25, 26, 27, 28, 29, 30, 31,		/* r18-r31 (even), saved */	\
  14,					/* r14, saved */		\
  2,					/* saved hard frame pointer */	\
  9,					/* saved return address */	\
  0,					/* fixed zero reg */		\
  1,					/* fixed stack pointer */	\
  10,					/* fixed thread pointer */	\
  32, 33, 34,				/* fixed ap, fp, sr[f], */	\
}

enum reg_class
{
  NO_REGS,
  SIBCALL_REGS,
  DOUBLE_REGS,
  GOT_REGS,
  GENERAL_REGS,
  FLAG_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES {	\
  "NO_REGS", 			\
  "SIBCALL_REGS",		\
  "DOUBLE_REGS",		\
  "GOT_REGS",			\
  "GENERAL_REGS",		\
  "FLAG_REGS",			\
  "ALL_REGS" }

/* The SIBCALL_REGS must be call-clobbered, and not used as a temporary
   in the epilogue.  This excludes R9 (LR), R11 (STATIC_CHAIN), and
   R13 (PE_TMP_REGNUM).  */
#define SIBCALL_REGS_MASK  0x00ff95f8u

#define REG_CLASS_CONTENTS      \
{ { 0x00000000, 0x00000000 },	\
  { SIBCALL_REGS_MASK,   0 },	\
  { 0x7f7ffffe, 0x00000000 },	\
  { 0xfffffdff, 0x00000000 },	\
  { 0xffffffff, 0x00000003 },	\
  { 0x00000000, 0x00000004 },	\
  { 0xffffffff, 0x00000007 }	\
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */
#define REGNO_REG_CLASS(REGNO) \
  ((REGNO) >= SR_F_REGNUM ? FLAG_REGS \
   : (REGNO) < 32 && ((SIBCALL_REGS_MASK >> (REGNO)) & 1) ? SIBCALL_REGS \
   : GENERAL_REGS)

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)               \
do {                                                    \
  if (GET_MODE_CLASS (MODE) == MODE_INT                 \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)         \
    (MODE) = word_mode;                                 \
} while (0)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

/* Assembly definitions.  */

#define ASM_APP_ON ""
#define ASM_APP_OFF ""

#define ASM_COMMENT_START "# "

#define GLOBAL_ASM_OP "\t.global\t"
#define TEXT_SECTION_ASM_OP "\t.section\t.text"
#define DATA_SECTION_ASM_OP "\t.section\t.data"
#define BSS_SECTION_ASM_OP "\t.section\t.bss"
#define SBSS_SECTION_ASM_OP "\t.section\t.sbss"

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)			\
  do							\
    {							\
      if ((LOG) != 0)					\
	fprintf (FILE, "\t.align %d\n", 1 << (LOG));	\
    }							\
  while (0)

/* This is used in crtstuff to create call stubs in the
   _init() and _fini() functions.  Defining this here saves
   a few bytes created by the dummy call_xxx() functions.  */
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
  asm (SECTION_OP "\n"					\
"	l.jal " #FUNC "\n"				\
"	 l.nop\n"					\
"	.previous");


#define PRINT_OPERAND_PUNCT_VALID_P(CODE) (code == '#')

/* Calling convention definitions.  */
#define CUMULATIVE_ARGS int
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  do { (CUM) = 0; } while (0)


/* Trampolines, for nested functions */
#define TRAMPOLINE_SIZE      20
#define TRAMPOLINE_ALIGNMENT 32

/* Pointer mode */
#define Pmode SImode
#define FUNCTION_MODE SImode
#define STACK_POINTER_REGNUM SP_REGNUM
#define FRAME_POINTER_REGNUM SFP_REGNUM
#define HARD_FRAME_POINTER_REGNUM HFP_REGNUM
#define STATIC_CHAIN_REGNUM RV_REGNUM

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM AP_REGNUM

/* Position Independent Code.  See or1k_init_pic_reg.  */
#define REAL_PIC_OFFSET_TABLE_REGNUM  HW_TO_GCC_REGNO (16)

/* ??? Follow i386 in working around gimple costing estimation, which
   happens without properly initializing the pic_offset_table pseudo.  */
#define PIC_OFFSET_TABLE_REGNUM \
  (pic_offset_table_rtx ? INVALID_REGNUM : REAL_PIC_OFFSET_TABLE_REGNUM)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r >= 3 && r <= 8)

#define MAX_REGS_PER_ADDRESS 1

/* The ELIMINABLE_REGS macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame. Note,
   the only elimination attempted by the compiler is to replace references
   to the frame pointer with references to the stack pointer.  */

#define ELIMINABLE_REGS					\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM }}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  do {							\
    (OFFSET) = or1k_initial_elimination_offset ((FROM), (TO)); \
  } while (0)

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_OK_FOR_BASE_P(REGNO)  ((REGNO) <= SFP_REGNUM)

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   'crtl->outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue
   should increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Stack layout and stack pointer usage.  */

/* This plus ARG_POINTER_REGNUM points to the first word of incoming args.  */
#define FIRST_PARM_OFFSET(FNDECL) (0)

/* This plus STACK_POINTER_REGNUM points to the first work of outgoing args.  */
#define STACK_POINTER_OFFSET (0)

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

#define FRAME_GROWS_DOWNWARD 1

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

#define STORE_FLAG_VALUE 1

/* Indicates how loads of narrow mode values are loaded into words.  */
#define LOAD_EXTEND_OP(MODE) (ZERO_EXTEND)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  */
#define EXIT_IGNORE_STACK 1

/* Macros related to the access of the stack frame chain.  */
#define INITIAL_FRAME_ADDRESS_RTX  or1k_initial_frame_addr ()
#define DYNAMIC_CHAIN_ADDRESS      or1k_dynamic_chain_addr
#define RETURN_ADDR_RTX            or1k_return_addr

/* Always pass the SYMBOL_REF for direct calls to the expanders.  */
#define NO_FUNCTION_CSE 1

#define NO_PROFILE_COUNTERS 1

/* Emit rtl for profiling.  Output assembler code to call "_mcount" for
   profiling a function entry.  */
#define PROFILE_HOOK(LABEL)  or1k_profile_hook()

/* All the work is done in PROFILE_HOOK, but this is still required.  */
#define FUNCTION_PROFILER(STREAM, LABELNO) do { } while (0)

/* Dwarf 2 Support */
#define DWARF2_DEBUGGING_INFO 1
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, LR_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN LR_REGNUM

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_REGNUM HW_TO_GCC_REGNO (23)
/* Use r25, r27, r29 and r31 (clobber regs) for exception data.
   Recall that these are remapped consecutively.  */
#define EH_RETURN_DATA_REGNO(N)	\
    ((N) < 4 ? HW_TO_GCC_REGNO (25) + (N) : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (Pmode, EH_RETURN_REGNUM)

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)

#endif /* GCC_OR1K_H */
