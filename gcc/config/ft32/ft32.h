/* Target Definitions for ft32.
   Copyright (C) 2015-2016 Free Software Foundation, Inc.
   Contributed by FTDI <support@ftdi.com>

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

#ifndef GCC_FT32_H
#define GCC_FT32_H

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s %{msim:crti.o%s} %{!msim:crti-hw.o%s} crtbegin.o%s"

/* Provide an ENDFILE_SPEC appropriate for svr4.  Here we tack on our own
   magical crtend.o file (see crtstuff.c) which provides part of the
   support for getting C++ file-scope static object constructed before
   entering `main', followed by the normal svr3/svr4 "finalizer" file,
   which is either `gcrtn.o' or `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* Provide a LIB_SPEC appropriate for svr4.  Here we tack on the default
   standard C library (unless we are building a shared library) and
   the simulator BSP code.  */

#undef LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}} \
                   %{msim:-Tsim.ld}"

#undef  LINK_SPEC
#define LINK_SPEC "%{h*} %{v:-V} \
                   %{static:-Bstatic} %{shared:-shared} %{symbolic:-Bsymbolic}"

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#define REGISTER_NAMES {        \
  "$fp", "$sp", "$r0", "$r1",   \
  "$r2", "$r3", "$r4", "$r5",   \
  "$r6", "$r7", "$r8", "$r9",   \
  "$r10", "$r11", "$r12", "$r13",   \
  "$r14", "$r15", "$r16", "$r17", "$r18", "$r19", "$r20", "$r21", "$r22", "$r23", "$r24", "$r25", "$r26", "$r27", "$r28", "$cc", \
  "?fp", "?ap", "$pc", "?cc" }

#define FT32_FP     0
#define FT32_SP     1
#define FT32_R0     2
#define FT32_R1     3
#define FT32_R2     4
#define FT32_R3     5
#define FT32_R4     6
#define FT32_R5     7
#define FT32_R6     8
#define FT32_R7     9
#define FT32_R8     10
#define FT32_R9     11
#define FT32_R10    12
#define FT32_R11    13
#define FT32_R12    14
#define FT32_R13    15
#define FT32_R14    16
#define FT32_R15    17
#define FT32_R16    18
#define FT32_R17    19
#define FT32_R18    20
#define FT32_R19    21
#define FT32_R20    22
#define FT32_R21    23
#define FT32_R22    24
#define FT32_R23    25
#define FT32_R24    26
#define FT32_R25    27
#define FT32_R26    28
#define FT32_R27    29
#define FT32_R28    30
#define FT32_R29    31
#define FT32_QAP    (32 + 1)
#define FT32_PC     (32 + 2)
#define FT32_CC     (32 + 3)
#define FIRST_PSEUDO_REGISTER (32 + 4)

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  CC_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define REG_CLASS_CONTENTS \
{ { 0x00000000, 0x00000000 }, /* Empty */                      \
  { 0xFFFFFFFF, 0x00000003 }, /* $fp, $sp, $r0 to $r13, ?fp */ \
  { 0x00000000, 0x00000004 }, /* $pc */                        \
  { 0x00000000, 0x00000008 }, /* ?cc */                        \
  { 0xFFFFFFFF, 0x0000000F }  /* All registers */              \
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {\
    "NO_REGS", \
    "GENERAL_REGS", \
    "SPECIAL_REGS", \
    "CC_REGS", \
    "ALL_REGS" }

#define FIXED_REGISTERS  /*  fp  sp  r0  r1 */   { 1, 1, 0, 0, \
                         /*  r2  r3  r4  r5 */     0, 0, 0, 0, \
                         /*  r6  r7  r8  r9 */     0, 0, 0, 0, \
                         /* r10 r11 r12 r13 */     0, 0, 0, 0, \
                         /* r14 r15 r16 r17 */     0, 0, 0, 0, \
                         /* r18 r19 r20 r21 */     0, 0, 0, 0, \
                         /* r22 r23 r24 r25 */     0, 0, 0, 0, \
                         /* r26 r27 r28 r29 */     0, 0, 1, 1, \
                         /* r30 r31         */     1, 1, 1, 1 }

#define CALL_USED_REGISTERS \
                         /*  fp  sp  r0  r1 */   { 1, 1, 1, 1, \
                         /*  r2  r3  r4  r5 */     1, 1, 1, 1, \
                         /*  r6  r7  r8  r9 */     1, 1, 1, 1, \
                         /* r10 r11 r12 r13 */     1, 1, 1, 0, \
                         /* r14 r15 r16 r17 */     0, 0, 0, 0, \
                         /* r18 r19 r20 r21 */     0, 0, 0, 0, \
                         /* r22 r23 r24 r25 */     0, 0, 0, 0, \
                         /* r26 r27 r28 r29 */     0, 0, 1, 1, \
                         /* r30 r31         */     1, 1, 1, 1 }

/* We can't copy to or from our CC register. */
#define AVOID_CCMODE_COPIES 1

/* A C expression that is nonzero if it is permissible to store a
   value of mode MODE in hard register number REGNO (or in several
   registers starting with that one).  All gstore registers are
   equivalent, so we can set this to 1.  */
#define HARD_REGNO_MODE_OK(R,M) 1

/* A C expression whose value is a register class containing hard
   register REGNO.  */
#define REGNO_REG_CLASS(R) ((R < FT32_PC) ? GENERAL_REGS :                \
                            (R == FT32_CC ? CC_REGS : SPECIAL_REGS))

/* A C expression for the number of consecutive hard registers,
   starting at register number REGNO, required to hold a value of mode
   MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)                      \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)             \
   / UNITS_PER_WORD)

/* A C expression that is nonzero if a value of mode MODE1 is
   accessible in mode MODE2 without copying.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* The Overall Framework of an Assembler File */

#undef  ASM_SPEC
#define ASM_COMMENT_START "#"
#define ASM_APP_ON ""
#define ASM_APP_OFF ""

#define FILE_ASM_OP     "\t.file\n"

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_ALIGN(STREAM,POWER) \
        fprintf (STREAM, "\t.p2align\t%d\n", POWER);

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  */
#define PRINT_OPERAND(STREAM, X, CODE) ft32_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_ADDRESS(STREAM ,X) ft32_print_operand_address (STREAM, X)

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

#define JUMP_TABLES_IN_TEXT_SECTION 1

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
    fprintf (FILE, "\tjmp\t.L%d\n", VALUE);				\

/* Passing Arguments in Registers */

/* A C type for declaring a variable that is used as the first
   argument of `FUNCTION_ARG' and other related values.  */
#define CUMULATIVE_ARGS unsigned int

/* If defined, the maximum amount of space required for outgoing arguments
   will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue should
   increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* A C statement (sans semicolon) for initializing the variable CUM
   for the state at the beginning of the argument list.
   For ft32, the first arg is passed in register 2 (aka $r0).  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (CUM = FT32_R0)

/* How Scalar Function Values Are Returned */

/* STACK AND CALLING */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */
#define STARTING_FRAME_OFFSET 0

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  */
#define FIRST_PARM_OFFSET(F) 0

/* Define this macro to nonzero value if the addresses of local variable slots
   are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* Define this macro as a C expression that is nonzero for registers that are
   used by the epilogue or the return pattern.  The stack and frame
   pointer registers are already assumed to be used as needed.  */
#define EPILOGUE_USES(R) (R == FT32_R5)

/* A C expression whose value is RTL representing the location of the
   incoming return address at the beginning of any function, before
   the prologue.  */
#define INCOMING_RETURN_ADDR_RTX                                        \
  gen_frame_mem (Pmode,                                                 \
                 plus_constant (Pmode, stack_pointer_rtx, 333 * UNITS_PER_WORD))

#define RETURN_ADDR_RTX(COUNT, FRAMEADDR)				\
  ((COUNT) == 0								\
   ? gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, arg_pointer_rtx, GEN_INT (-4))) \
   : NULL_RTX)

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N+2) : INVALID_REGNUM)

/* Store the return handler into the call frame.  */
#define EH_RETURN_HANDLER_RTX                                           \
  gen_frame_mem (Pmode,                                                 \
                 plus_constant (Pmode, frame_pointer_rtx, UNITS_PER_WORD))

/* Storage Layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 0
#define WORDS_BIG_ENDIAN 0

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 32

#define BRANCH_COST(speed_p, predictable_p)     2

/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory.  */
#define SLOW_BYTE_ACCESS 1

#define STORE_FLAG_VALUE                        1

#define MOVE_RATIO(speed) ((speed) ? 6 : 2)

/* Number of storage units in a word; normally the size of a
   general-purpose register, a power of two from 1 or 8.  */
#define UNITS_PER_WORD 4

/* Define this macro to the minimum alignment enforced by hardware
   for the stack pointer on this machine.  The definition is a C
   expression for the desired alignment (measured in bits).  */
#define STACK_BOUNDARY 32

/* Normal alignment required for function parameters on the stack, in
   bits.  All stack parameters receive at least this much alignment
   regardless of data type.  */
#define PARM_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY  32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 32

/* Align definitions of arrays, unions and structures so that
   initializations and copies can be made more efficient.  This is not
   ABI-changing, so it only affects places where we can see the
   definition. Increasing the alignment tends to introduce padding,
   so don't do this when optimizing for size/conserving stack space. */
#define FT32_EXPAND_ALIGNMENT(COND, EXP, ALIGN)				\
  (((COND) && ((ALIGN) < BITS_PER_WORD)					\
    && (TREE_CODE (EXP) == ARRAY_TYPE					\
	|| TREE_CODE (EXP) == UNION_TYPE				\
	|| TREE_CODE (EXP) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)             \
  (TREE_CODE (TYPE) == ARRAY_TYPE               \
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode    \
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* Similarly, make sure that objects on the stack are sensibly aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)				\
  FT32_EXPAND_ALIGNMENT(/*!flag_conserve_stack*/ 1, EXP, ALIGN)

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define PCC_BITFIELD_TYPE_MATTERS       1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST       \
    && (ALIGN) < FASTEST_ALIGNMENT)     \
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Generating Code for Profiling */
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/* Trampolines for Nested Functions.  */
#define TRAMPOLINE_SIZE (2 + 6 + 6 + 2 + 2 + 6)

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 32

/* An alias for the machine mode for pointers.  */
#define Pmode         SImode

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE) \
do { \
  if (((MODE) == HImode) \
   || ((MODE) == QImode)) \
       (MODE) = SImode;  \
} while (0)

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE QImode

#define STATIC_CHAIN_REGNUM  FT32_R28

/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  */
#define STACK_POINTER_REGNUM FT32_SP

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  */
#define FRAME_POINTER_REGNUM FT32_FP

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM FT32_QAP

#define ELIMINABLE_REGS					\
{{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},		\
 {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}


/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)                    \
  do {                                                                  \
    (OFFSET) = ft32_initial_elimination_offset ((FROM), (TO));            \
  } while (0)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r >= FT32_R0 && r <= FT32_R5)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

#define HARD_REGNO_OK_FOR_BASE_P(NUM) \
  ((unsigned) (NUM) < FIRST_PSEUDO_REGISTER \
   && (REGNO_REG_CLASS(NUM) == GENERAL_REGS \
       || (NUM) == HARD_FRAME_POINTER_REGNUM))

/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(NUM)                 \
  (HARD_REGNO_OK_FOR_BASE_P(NUM)                 \
   || HARD_REGNO_OK_FOR_BASE_P(reg_renumber[(NUM)]))
#else
#define REGNO_OK_FOR_BASE_P(NUM)                 \
  ((NUM) >= FIRST_PSEUDO_REGISTER || HARD_REGNO_OK_FOR_BASE_P(NUM))
#endif

/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(NUM) FT32_FP

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 4
#define TRULY_NOOP_TRUNCATION(op,ip) 1

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* All load operations zero extend.  */
#define LOAD_EXTEND_OP(MEM) ZERO_EXTEND

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Run-time Target Specification */

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_define ("__FT32__");          \
  }

#define HAS_LONG_UNCOND_BRANCH true

#define NO_FUNCTION_CSE 1

#define ADDR_SPACE_PM 1

#define REGISTER_TARGET_PRAGMAS() do { \
  c_register_addr_space ("__flash__", ADDR_SPACE_PM); \
} while (0);

extern int ft32_is_mem_pm(rtx o);

#define ASM_OUTPUT_SYMBOL_REF(stream, sym) \
  do { \
    assemble_name (stream, XSTR (sym, 0)); \
    int section_debug = in_section && \
      (SECTION_STYLE (in_section) == SECTION_NAMED) && \
      (in_section->named.common.flags & SECTION_DEBUG); \
    if (!section_debug && SYMBOL_REF_FLAGS (sym) & 0x1000) \
      asm_fprintf (stream, "-0x800000"); \
  } while (0)

#endif /* GCC_FT32_H */
