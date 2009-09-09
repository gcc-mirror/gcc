/* Target Definitions for moxie.
   Copyright (C) 2008, 2009  Free Software Foundation, Inc.
   Contributed by Anthony Green.

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

#ifndef GCC_MOXIE_H
#define GCC_MOXIE_H

/* This is defined by svr4.h, which is included prior to this file.
   However, we should undefine it for moxie-elf, since we don't provide
   functions like access() and mkdir() in newlib.  This will have to
   be defined again for a Linux port.  */
#undef TARGET_POSIX_IO

/* Another C string constant used much like `LINK_SPEC'.  The difference
   between the two is that `STARTFILE_SPEC' is used at the very beginning of
   the command given to the linker.

   If this macro is not defined, a default is provided that loads the standard
   C startup file from the usual place.  See `gcc.c'.

   Defined in svr4.h.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crti.o%s crtbegin.o%s"

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
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

/* Layout of Source Language Data Types */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define DEFAULT_SIGNED_CHAR 1

/* Registers...

   $fp  - frame pointer
   $sp  - stack pointer
   $r0  - general purpose 32-bit register.
   $r1  - general purpose 32-bit register.
   $r2  - general purpose 32-bit register.
   $r3  - general purpose 32-bit register.
   $r4  - general purpose 32-bit register.
   $r5  - general purpose 32-bit register.
   $r6  - general purpose 32-bit register.
   $r7  - general purpose 32-bit register.
   $r8  - general purpose 32-bit register.
   $r9  - general purpose 32-bit register.
   $r10 - general purpose 32-bit register.
   $r11 - general purpose 32-bit register.
   $r12 - general purpose 32-bit register.
   $r13 - reserved for execution environment.

   Special Registers...

   $pc - 32-bit program counter.
   
*/

#define REGISTER_NAMES {	\
  "$fp", "$sp", "$r0", "$r1",   \
  "$r2", "$r3", "$r4", "$r5",   \
  "$r6", "$r7", "$r8", "$r9",   \
  "$r10", "$r11", "$r12", "$r13",   \
  "?fp", "?ap", "$pc", "?cc" }

#define MOXIE_FP     0
#define MOXIE_SP     1
#define MOXIE_R0     2
#define MOXIE_R1     3 
#define MOXIE_R2     4
#define MOXIE_R3     5
#define MOXIE_R4     6
#define MOXIE_R5     7
#define MOXIE_R6     8
#define MOXIE_R7     9
#define MOXIE_R8     10
#define MOXIE_R9     11
#define MOXIE_R10    12
#define MOXIE_R11    13
#define MOXIE_R12    14
#define MOXIE_R13    15
#define MOXIE_QFP    16
#define MOXIE_QAP    17
#define MOXIE_PC     18
#define MOXIE_CC     19

#define FIRST_PSEUDO_REGISTER 20

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  CC_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};


/* The following macro defines cover classes for Integrated Register
   Allocator.  Cover classes is a set of non-intersected register
   classes covering all hard registers used for register allocation
   purpose.  Any move between two registers of a cover class should be
   cheaper than load or store of the registers.  The macro value is
   array of register classes with LIM_REG_CLASSES used as the end
   marker.  */
#define IRA_COVER_CLASSES { GENERAL_REGS, LIM_REG_CLASSES }

#define REG_CLASS_CONTENTS \
{ { 0x00000000 }, /* Empty */			   \
  { 0x0003FFFF }, /* $fp, $sp, $r0 to $r13, ?fp */ \
  { 0x00040000 }, /* $pc */	                   \
  { 0x00080000 }, /* ?cc */                        \
  { 0x000FFFFF }  /* All registers */              \
}

#define N_REG_CLASSES LIM_REG_CLASSES

#define REG_CLASS_NAMES {\
    "NO_REGS", \
    "GENERAL_REGS", \
    "SPECIAL_REGS", \
    "CC_REGS", \
    "ALL_REGS" }

#define FIXED_REGISTERS     { 1, 1, 0, 0, \
			      0, 0, 0, 0, \
			      0, 0, 0, 0, \
			      0, 0, 0, 1, \
                              1, 1, 1, 1 }

#define CALL_USED_REGISTERS { 1, 1, 1, 1, \
			      1, 1, 1, 1, \
			      0, 0, 0, 0, \
			      0, 0, 1, 1, \
                              1, 1, 1, 1 }

/* We can't copy to or from our CC register. */
#define AVOID_CCMODE_COPIES 1

/* A C expression that is nonzero if it is permissible to store a
   value of mode MODE in hard register number REGNO (or in several
   registers starting with that one).  All gstore registers are 
   equivalent, so we can set this to 1.  */
#define HARD_REGNO_MODE_OK(R,M) 1

/* A C expression whose value is a register class containing hard
   register REGNO.  */
#define REGNO_REG_CLASS(R) ((R < MOXIE_PC) ? GENERAL_REGS :		\
                            (R == MOXIE_CC ? CC_REGS : SPECIAL_REGS))

/* A C expression for the number of consecutive hard registers,
   starting at register number REGNO, required to hold a value of mode
   MODE.  */
#define HARD_REGNO_NREGS(REGNO, MODE)			   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)		   \
   / UNITS_PER_WORD)

/* A C expression that is nonzero if a value of mode MODE1 is
   accessible in mode MODE2 without copying.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 1

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.  */
#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C expression that places additional restrictions on the register
   class to use when it is necessary to copy value X into a register
   in class CLASS.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

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
#define PRINT_OPERAND(STREAM, X, CODE) moxie_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_ADDRESS(STREAM ,X) moxie_print_operand_address (STREAM, X)

/* Output and Generation of Labels */

#define GLOBAL_ASM_OP "\t.global\t"

/* Passing Arguments in Registers */

/* A C expression that controls whether a function argument is passed
   in a register, and which register.  */
#define FUNCTION_ARG(CUM,MODE,TYPE,NAMED) \
  moxie_function_arg(CUM,MODE,TYPE,NAMED)

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
   For moxie, the first arg is passed in register 2 (aka $r0).  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  (CUM = MOXIE_R0)

#define MOXIE_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

#define FUNCTION_ARG_ADVANCE(CUM,MODE,TYPE,NAMED) \
  (CUM = (CUM < MOXIE_R6 ?                        \
          CUM + ((3 + MOXIE_FUNCTION_ARG_SIZE(MODE,TYPE))/4) : CUM ))

/* How Scalar Function Values Are Returned */

/* These macros are deprecated, but we still need them for now since
   the version of gcc we're using doesn't fully support
   TARGET_FUNCTION_VALUE.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  moxie_function_value (VALTYPE, FUNC, 0)
#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC) \
  moxie_function_value (VALTYPE, FUNC, 1)

/* A C expression to create an RTX representing the place where a
   library function returns a value of mode MODE.  */
#define LIBCALL_VALUE(MODE) gen_rtx_REG (MODE, 2)

/* STACK AND CALLING */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0

/* Offset from the frame pointer to the first local variable slot to
   be allocated.  */
#define STARTING_FRAME_OFFSET 0

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE(FNTYPE) 1
#define STACK_PARMS_IN_REG_PARM_AREA

/* Define this if it is the responsibility of the caller to allocate
   the area reserved for arguments passed in registers.  */
#define REG_PARM_STACK_SPACE(FNDECL) (6 * UNITS_PER_WORD)

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  */
#define FIRST_PARM_OFFSET(F) 12

/* Define this macro to nonzero value if the addresses of local variable slots
   are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* Define this macro as a C expression that is nonzero for registers that are
   used by the epilogue or the return pattern.  The stack and frame
   pointer registers are already assumed to be used as needed.  */
#define EPILOGUE_USES(R) (R == MOXIE_R5)

#define OVERRIDE_OPTIONS moxie_override_options ()

/* Storage Layout */

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 16

/* Define this macro as a C expression which is nonzero if accessing
   less than a word of memory (i.e. a `char' or a `short') is no
   faster than accessing a word of memory.  */
#define SLOW_BYTE_ACCESS 1

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

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use 
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define	PCC_BITFIELD_TYPE_MATTERS	1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 32

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  ((TREE_CODE (EXP) == STRING_CST	\
    && (ALIGN) < FASTEST_ALIGNMENT)	\
   ? FASTEST_ALIGNMENT : (ALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))
     
/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Generating Code for Profiling */
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/* Trampolines for Nested Functions.  */
#define TRAMPOLINE_SIZE (2 + 6 + 6 + 2 + 6)

/* Alignment required for trampolines, in bits.  */
#define TRAMPOLINE_ALIGNMENT 16

/* A C statement to initialize the variable parts of a trampoline.  ADDR is an
   RTX for the address of the trampoline; FNADDR is an RTX for the address of
   the nested function; STATIC_CHAIN is an RTX for the static chain value that
   should be passed to the function when it is called.  */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN)		      \
do									      \
{									      \
  emit_move_insn (gen_rtx_MEM (SImode,                                        \
                               plus_constant (ADDR, 4)), STATIC_CHAIN);       \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (ADDR, 18)), FNADDR);    \
} while (0);

/* A C statement to output, on the stream FILE, assembler code for a
   block of data that contains the constant parts of a trampoline.
   This code should not include a label--the label is taken care of
   automatically.  */
#define TRAMPOLINE_TEMPLATE(FILE)	       	\
{						\
  fprintf (FILE, "\tpush  $sp, $r0\n");         \
  fprintf (FILE, "\tldi.l $r0, 0x0\n"); 	\
  fprintf (FILE, "\tsto.l 0x8($fp), $r0\n");	\
  fprintf (FILE, "\tpop   $sp, $r0\n");		\
  fprintf (FILE, "\tjmpa  0x0\n");	        \
}

/* An alias for the machine mode for pointers.  */
#define Pmode         SImode

/* An alias for the machine mode used for memory references to
   functions being called, in `call' RTL expressions.  */
#define FUNCTION_MODE QImode

/* The register number of the stack pointer register, which must also
   be a fixed register according to `FIXED_REGISTERS'.  */
#define STACK_POINTER_REGNUM MOXIE_SP

/* The register number of the frame pointer register, which is used to
   access automatic variables in the stack frame.  */
#define FRAME_POINTER_REGNUM MOXIE_QFP

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM MOXIE_QAP

/* If the static chain is passed in memory, these macros provide rtx
   giving 'mem' expressions that denote where they are stored.
   'STATIC_CHAIN' and 'STATIC_CHAIN_INCOMING' give the locations as
   seen by the calling and called functions, respectively.  */

#define STATIC_CHAIN							\
  gen_rtx_MEM (Pmode, plus_constant (stack_pointer_rtx, -UNITS_PER_WORD))

#define STATIC_CHAIN_INCOMING						\
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, 2 * UNITS_PER_WORD))

#define HARD_FRAME_POINTER_REGNUM MOXIE_FP

#define ELIMINABLE_REGS							\
{{ FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM },			\
 { ARG_POINTER_REGNUM,   HARD_FRAME_POINTER_REGNUM }}			

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  do {									\
    (OFFSET) = moxie_initial_elimination_offset ((FROM), (TO));		\
  } while (0)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r >= MOXIE_R0 && r <= MOXIE_R5)

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which the values of called function may come back.  */
#define FUNCTION_VALUE_REGNO_P(r) (r == MOXIE_R0)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

#define HARD_REGNO_OK_FOR_BASE_P(NUM) \
  ((NUM) >= 0 && (NUM) < FIRST_PSEUDO_REGISTER \
   && (REGNO_REG_CLASS(NUM) == GENERAL_REGS \
       || (NUM) == HARD_FRAME_POINTER_REGNUM))

/* A C expression which is nonzero if register number NUM is suitable
   for use as a base register in operand addresses.  */
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(NUM)		 \
  (HARD_REGNO_OK_FOR_BASE_P(NUM) 		 \
   || HARD_REGNO_OK_FOR_BASE_P(reg_renumber[(NUM)]))
#else
#define REGNO_OK_FOR_BASE_P(NUM)		 \
  ((NUM) >= FIRST_PSEUDO_REGISTER || HARD_REGNO_OK_FOR_BASE_P(NUM))
#endif

/* A C expression which is nonzero if register number NUM is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(NUM) MOXIE_FP

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 4
#define TRULY_NOOP_TRUNCATION(op,ip) 1

/* All load operations zero extend.  */
#define LOAD_EXTEND_OP(MEM) ZERO_EXTEND

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0

/* A C expression that is nonzero if X is a legitimate constant for
   an immediate operand on the target machine.  */
#define LEGITIMATE_CONSTANT_P(X) 1

/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  */
#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)

/* A number, the maximum number of registers that can appear in a
   valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

#define TRULY_NOOP_TRUNCATION(op,ip) 1

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* A C compound statement with a conditional `goto LABEL;' executed
   if X (an RTX) is a legitimate memory address on the target machine
   for a memory operand of mode MODE.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE,X,LABEL)		\
  do {                                                  \
    if (GET_CODE(X) == PLUS)				\
      {							\
	rtx op1,op2;					\
	op1 = XEXP(X,0);				\
	op2 = XEXP(X,1);				\
	if (GET_CODE(op1) == REG			\
	    && CONSTANT_ADDRESS_P(op2)			\
	    && REGNO_OK_FOR_BASE_P(REGNO(op1)))		\
	  goto LABEL;					\
      }							\
    if (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))	\
      goto LABEL;					\
    if (GET_CODE (X) == SYMBOL_REF			\
	|| GET_CODE (X) == LABEL_REF			\
	|| GET_CODE (X) == CONST)			\
      goto LABEL;					\
  } while (0)

/* Run-time Target Specification */

#define TARGET_CPU_CPP_BUILTINS() \
  { \
    builtin_define_std ("moxie");		\
    builtin_define_std ("MOXIE");		\
  }

#define HAS_LONG_UNCOND_BRANCH true

#endif /* GCC_MOXIE_H */
