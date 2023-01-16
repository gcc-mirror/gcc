/* Definitions of target machine for Altera Nios II.
   Copyright (C) 2012-2023 Free Software Foundation, Inc.
   Contributed by Jonah Graham (jgraham@altera.com), 
   Will Reece (wreece@altera.com), and Jeff DaSilva (jdasilva@altera.com).
   Contributed by Mentor Graphics, Inc.

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

#ifndef GCC_NIOS2_H
#define GCC_NIOS2_H

/* Indicate R2 ISA level support.  */
#define TARGET_ARCH_R2 (nios2_arch_option == ARCH_R2)

/* FPU insn codes declared here.  */
#include "config/nios2/nios2-opts.h"

/* Define built-in preprocessor macros.  */
#define TARGET_CPU_CPP_BUILTINS()                   \
  do                                                \
    {                                               \
      builtin_define_std ("NIOS2");                 \
      builtin_define_std ("nios2");                 \
      if (TARGET_BIG_ENDIAN)                        \
        builtin_define_std ("nios2_big_endian");    \
      else                                          \
        builtin_define_std ("nios2_little_endian"); \
      builtin_define_with_int_value (		    \
        "__nios2_arch__", (int) nios2_arch_option); \
    }						    \
  while (0)

/* We're little endian, unless otherwise specified by defining
   BIG_ENDIAN_FLAG.  */
#ifndef TARGET_ENDIAN_DEFAULT
# define TARGET_ENDIAN_DEFAULT 0
#endif

/* Default target_flags if no switches specified.  */
#ifndef TARGET_DEFAULT
# define TARGET_DEFAULT (MASK_HAS_MUL | TARGET_ENDIAN_DEFAULT)
#endif

#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!march=*:%{!mcpu=*:-march=%(VALUE)}}" }

#define CC1_SPEC "%{G*}"

#if TARGET_ENDIAN_DEFAULT == 0
# define ASM_SPEC "%{!meb:-EL} %{meb:-EB} %{march=*:-march=%*}"
# define LINK_SPEC_ENDIAN "%{!meb:-EL} %{meb:-EB}"
# define MULTILIB_DEFAULTS { "EL" }
#else
# define ASM_SPEC "%{!mel:-EB} %{mel:-EL} %{march=*:-march=%*}"
# define LINK_SPEC_ENDIAN "%{!mel:-EB} %{mel:-EL}"
# define MULTILIB_DEFAULTS { "EB" }
#endif

#define LINK_SPEC LINK_SPEC_ENDIAN \
  " %{shared:-shared} \
    %{static:-Bstatic}"


/* Storage layout.  */

#define DEFAULT_SIGNED_CHAR 1
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)
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

#define LABEL_ALIGN(LABEL) nios2_label_align (LABEL)

/* Layout of source language data types.  */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE DOUBLE_TYPE_SIZE

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"


/* Basic characteristics of Nios II registers:

   Regno  Name
   0      r0       zero    always zero
   1      r1       at      Assembler Temporary
   2-3    r2-r3            Return Location
   4-7    r4-r7            Register Arguments
   8-15   r8-r15           Caller Saved Registers
   16-22  r16-r22          Callee Saved Registers
   22     r22              Global Offset Table pointer (Linux ABI only)
   23     r23              Thread pointer (Linux ABI only)
   24     r24      et      Exception Temporary
   25     r25      bt      Breakpoint Temporary
   26     r26      gp      Global Pointer
   27     r27      sp      Stack Pointer
   28     r28      fp      Frame Pointer
   29     r29      ea      Exception Return Address
   30     r30      ba      Breakpoint Return Address
   31     r31      ra      Return Address
			   
   32     ctl0     status
   33     ctl1     estatus STATUS saved by exception
   34     ctl2     bstatus STATUS saved by break
   35     ctl3     ipri    Interrupt Priority Mask
   36     ctl4     ecause  Exception Cause

   37     pc               Not an actual register

   38     fake_fp          Fake Frame Pointer (always eliminated)
   39     fake_ap          Fake Argument Pointer (always eliminated)
   40                      First Pseudo Register

   In addition, r12 is used as the static chain register and r13, r14, and r15
   are clobbered by PLT code sequences.  

   The definitions for all the hard register numbers are located in nios2.md.
*/

#define FIXED_REGISTERS                      \
  {					     \
/*        +0  1  2  3  4  5  6  7  8  9 */   \
/*   0 */  1, 1, 0, 0, 0, 0, 0, 0, 0, 0,     \
/*  10 */  0, 0, 0, 0, 0, 0, 0, 0, 0, 0,     \
/*  20 */  0, 0, TARGET_LINUX_ABI, TARGET_LINUX_ABI, 1, 1, 1, 1, 0, 1,     \
/*  30 */  1, 0, 1, 1, 1, 1, 1, 1, 1, 1,     \
  }

/* Call used == caller saved + fixed regs + args + ret vals.  */
#define CALL_USED_REGISTERS                  \
  {					     \
/*        +0  1  2  3  4  5  6  7  8  9 */   \
/*   0 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
/*  10 */  1, 1, 1, 1, 1, 1, 0, 0, 0, 0,     \
/*  20 */  0, 0, TARGET_LINUX_ABI, TARGET_LINUX_ABI, 1, 1, 1, 1, 0, 1,     \
/*  30 */  1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     \
  }

/* Order in which to allocate registers.  Each register must be
   listed once.  This is the default ordering for R1 and non-CDX R2
   code.  For CDX, we overwrite this in ADJUST_REG_ALLOC_ORDER.  */
#define REG_ALLOC_ORDER							\
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, \
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, \
      37, 38, 39 }

#define ADJUST_REG_ALLOC_ORDER nios2_adjust_reg_alloc_order ()

/* Caller-save costs can be less emphasized under R2 CDX, where we can
   use push.n/pop.n.  */
#define HONOR_REG_ALLOC_ORDER (TARGET_HAS_CDX)

/* Register Classes.  */

enum reg_class
{
  NO_REGS,
  SIB_REGS,
  IJMP_REGS,
  GP_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES   \
  {  "NO_REGS",		  \
     "SIB_REGS",	  \
     "IJMP_REGS",	  \
     "GP_REGS",           \
     "ALL_REGS" }

#define GENERAL_REGS ALL_REGS

#define REG_CLASS_CONTENTS			\
  {						\
    /* NO_REGS    */ { 0, 0},			\
    /* SIB_REGS   */ { 0xfe0c, 0},		\
    /* IJMP_REGS  */ { 0x7fffffff, 0},		\
    /* GP_REGS    */ {~0, 0},			\
    /* ALL_REGS   */ {~0,~0}			\
  }


#define GP_REG_P(REGNO) ((unsigned)(REGNO) <= LAST_GP_REG)
#define REGNO_REG_CLASS(REGNO) (GP_REG_P (REGNO) ? GP_REGS : ALL_REGS)
#define CLASS_MAX_NREGS(CLASS, MODE)					\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define CDX_REG_P(REGNO)						\
  ((REGNO) == 16 || (REGNO) == 17 || ((REGNO) >= 2 && (REGNO) <= 7))

/* Tests for various kinds of constants used in the Nios II port.  */

#define SMALL_INT(X) ((unsigned HOST_WIDE_INT)(X) + 0x8000 < 0x10000)
#define SMALL_INT12(X) ((unsigned HOST_WIDE_INT)(X) + 0x800 < 0x1000)
#define SMALL_INT_UNSIGNED(X) ((X) >= 0 && (X) < 0x10000)
#define UPPER16_INT(X) (((X) & 0xffff) == 0)
#define SHIFT_INT(X) ((X) >= 0 && (X) <= 31)
#define RDWRCTL_INT(X) ((X) >= 0 && (X) <= 31)
#define CUSTOM_INSN_OPCODE(X) ((X) >= 0 && (X) <= 255)
#define ANDCLEAR_INT(X) \
  (((X) & 0xffff) == 0xffff || (((X) >> 16) & 0xffff) == 0xffff)

/* Say that the epilogue uses the return address register.  Note that
   in the case of sibcalls, the values "used by the epilogue" are
   considered live at the start of the called function.  */
#define EPILOGUE_USES(REGNO) (epilogue_completed && (REGNO) == RA_REGNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Trampolines use a 5-instruction sequence.  */
#define TRAMPOLINE_SIZE 20

/* Stack layout.  */
#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* Before the prologue, RA lives in r31.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, RA_REGNO)
#define RETURN_ADDR_RTX(C,F) nios2_get_return_address (C)

#define DWARF_FRAME_RETURN_COLUMN RA_REGNO

/* The CFA includes the pretend args.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL) \
  (gcc_assert ((FNDECL) == current_function_decl), \
   FIRST_PARM_OFFSET (FNDECL) + crtl->args.pretend_args_size)

/* Frame/arg pointer elimination settings.  */
#define ELIMINABLE_REGS                                                 \
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},                         \
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM},                    \
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},                         \
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  (OFFSET) = nios2_initial_elimination_offset ((FROM), (TO))

/* Calling convention definitions.  */
typedef struct nios2_args
{
  int regs_used;
} CUMULATIVE_ARGS;

#define NUM_ARG_REGS (LAST_ARG_REGNO - FIRST_ARG_REGNO + 1)

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  do { (CUM).regs_used = 0; } while (0)

#define PAD_VARARGS_DOWN \
  (targetm.calls.function_arg_padding (TYPE_MODE (type), type) == PAD_DOWNWARD)

#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (nios2_block_reg_padding ((MODE), (TYPE), (FIRST)))

#define FUNCTION_ARG_REGNO_P(REGNO) \
  ((REGNO) >= FIRST_ARG_REGNO && (REGNO) <= LAST_ARG_REGNO)

/* Passing function arguments on stack.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* We define TARGET_RETURN_IN_MEMORY, so set to zero.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Profiling.  */
#define PROFILE_BEFORE_PROLOGUE
#define NO_PROFILE_COUNTERS 1
#define FUNCTION_PROFILER(FILE, LABELNO) \
  nios2_function_profiler ((FILE), (LABELNO))

/* Addressing modes.  */

#define CONSTANT_ADDRESS_P(X) \
  (CONSTANT_P (X) && memory_address_p (SImode, X))

#define MAX_REGS_PER_ADDRESS 1
#define BASE_REG_CLASS ALL_REGS
#define INDEX_REG_CLASS NO_REGS

#define REGNO_OK_FOR_BASE_P(REGNO) nios2_regno_ok_for_base_p ((REGNO), true)
#define REGNO_OK_FOR_INDEX_P(REGNO) 0

/* Describing Relative Costs of Operations.  */
#define MOVE_MAX 4
#define SLOW_BYTE_ACCESS 1

/* It is as good to call a constant function address as to call an address
   kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Position independent code.  */

#define PIC_OFFSET_TABLE_REGNUM 22
#define LEGITIMATE_PIC_OPERAND_P(X) nios2_legitimate_pic_operand_p (X)

/* Define output assembler language.  */

#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"

#define ASM_COMMENT_START "# "

#define GLOBAL_ASM_OP "\t.global\t"

#define REGISTER_NAMES \
  {		       \
    "zero", \
    "at", \
    "r2", \
    "r3", \
    "r4", \
    "r5", \
    "r6", \
    "r7", \
    "r8", \
    "r9", \
    "r10", \
    "r11", \
    "r12", \
    "r13", \
    "r14", \
    "r15", \
    "r16", \
    "r17", \
    "r18", \
    "r19", \
    "r20", \
    "r21", \
    "r22", \
    "r23", \
    "et", \
    "bt", \
    "gp", \
    "sp", \
    "fp", \
    "ta", \
    "ba", \
    "ra", \
    "status", \
    "estatus", \
    "bstatus", \
    "ipri", \
    "ecause", \
    "pc", \
    "fake_fp", \
    "fake_ap", \
}

#define ADDITIONAL_REGISTER_NAMES       \
{					\
  {"r0", 0},				\
  {"r1", 1},				\
  {"r24", 24},                          \
  {"r25", 25},                          \
  {"r26", 26},                          \
  {"r27", 27},                          \
  {"r28", 28},                          \
  {"r29", 29},                          \
  {"r30", 30},                          \
  {"r31", 31}                           \
}

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  do									\
    {									\
      fputs (integer_asm_op (POINTER_SIZE / BITS_PER_UNIT, TRUE), FILE); \
      fprintf (FILE, ".L%u\n", (unsigned) (VALUE));			\
    }									\
  while (0)

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)\
  do									\
    {									\
      fputs (integer_asm_op (POINTER_SIZE / BITS_PER_UNIT, TRUE), STREAM); \
      fprintf (STREAM, ".L%u-.L%u\n", (unsigned) (VALUE), (unsigned) (REL)); \
    }									\
  while (0)

/* Section directives.  */

/* Output before read-only data.  */
#define TEXT_SECTION_ASM_OP "\t.section\t.text"

/* Output before writable data.  */
#define DATA_SECTION_ASM_OP "\t.section\t.data"

/* Output before uninitialized data.  */
#define BSS_SECTION_ASM_OP "\t.section\t.bss"

/* Output before 'small' uninitialized data.  */
#define SBSS_SECTION_ASM_OP "\t.section\t.sbss"

#ifndef USED_FOR_TARGET
/* Default the definition of "small data" to 8 bytes.  */
extern unsigned HOST_WIDE_INT nios2_section_threshold;
#endif

#define NIOS2_DEFAULT_GVALUE 8

/* This says how to output assembler code to declare an
   uninitialized external linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */
#undef COMMON_ASM_OP
#define COMMON_ASM_OP   "\t.comm\t"

#define ASM_OUTPUT_ALIGN(FILE, LOG)		     \
  do {						     \
    fprintf ((FILE), "%s%d\n", ALIGN_ASM_OP, (LOG)); \
  } while (0)

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)              \
do                                                                      \
  {									\
    fprintf ((FILE), "%s", COMMON_ASM_OP);				\
    assemble_name ((FILE), (NAME));					\
    fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n", (SIZE),	\
	     (ALIGN) / BITS_PER_UNIT);					\
  }									\
while (0)


/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#undef  ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
do {                                                                    \
 if (targetm.in_small_data_p (DECL))					\
    switch_to_section (sbss_section);					\
  else                                                                  \
    switch_to_section (bss_section);					\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");                     \
  if (!flag_inhibit_size_directive)                                     \
    ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, SIZE);                       \
  ASM_OUTPUT_ALIGN ((FILE), exact_log2((ALIGN) / BITS_PER_UNIT));       \
  ASM_OUTPUT_LABEL(FILE, NAME);                                         \
  ASM_OUTPUT_SKIP((FILE), (SIZE) ? (SIZE) : 1);                         \
} while (0)

/* Put the jump tables in .text because when using position-independent code,
   Nios II elf has no relocation that can represent arbitrary differences
   between symbols in different sections.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Exception handling.  */

/* Describe __builtin_eh_return.  */
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG (Pmode, LAST_RETVAL_REGNO)
#define EH_RETURN_DATA_REGNO(N) ((N) <= (LAST_ARG_REGNO - FIRST_ARG_REGNO) \
				 ? (N) + FIRST_ARG_REGNO : INVALID_REGNUM)

/* For PIC, use indirect for global references; it'll end up using a dynamic
   relocation, which we want to keep out of read-only EH sections.
   For local references, we want to use GOT-relative offsets provided
   the assembler supports them.  For non-PIC, use an absolute encoding.  */
#ifdef HAVE_AS_NIOS2_GOTOFF_RELOCATION
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)		\
  (flag_pic							\
   ? ((GLOBAL)							\
      ? DW_EH_PE_indirect | DW_EH_PE_absptr			\
      : DW_EH_PE_datarel | DW_EH_PE_sdata4)			\
   : DW_EH_PE_absptr)

#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(FILE, ENCODING, SIZE, ADDR, DONE) \
  do {									\
      if (((ENCODING) & 0xf0) == DW_EH_PE_datarel)			\
      {									\
	fputs ("\t.4byte %gotoff(", FILE);				\
	output_addr_const (FILE, ADDR);					\
	fputs (")", FILE);						\
	goto DONE;							\
      }									\
  } while (0)

#else
/* We don't have %gotoff support in the assembler.  Fall back to the encoding
   it used to use instead before the assembler was fixed.  This has known
   bugs but mostly works.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)		\
  (flag_pic							\
   ? ((GLOBAL)							\
      ? DW_EH_PE_indirect | DW_EH_PE_absptr			\
      : DW_EH_PE_aligned)					\
   : DW_EH_PE_absptr)
#endif

/* Misc. parameters.  */

#define STORE_FLAG_VALUE 1
#define Pmode SImode
#define FUNCTION_MODE QImode

#define CASE_VECTOR_MODE Pmode

#define LOAD_EXTEND_OP(MODE) (ZERO_EXTEND)

#define WORD_REGISTER_OPERATIONS 1

#endif /* GCC_NIOS2_H */
