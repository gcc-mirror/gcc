/* Definition of the eBPF target for GCC.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_BPF_H
#define GCC_BPF_H

/**** Controlling the Compilation Driver.  */

#define ASM_SPEC "%{mbig-endian:-EB} %{!mbig-endian:-EL} %{mxbpf:-mxbpf}"
#define LINK_SPEC "%{mbig-endian:-EB} %{!mbig-endian:-EL}"
#define LIB_SPEC ""
#define STARTFILE_SPEC ""

/**** Run-time Target Specification.  */

#define TARGET_CPU_CPP_BUILTINS() bpf_target_macros (pfile)

/**** Storage Layout.  */

/* Endianness and word size.  */
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)
#define BITS_PER_WORD 64
#define UNITS_PER_WORD 8

/* When storing an integer whose size is less than 64-bit in a
   register, promote it to a DImode.  */
#define PROMOTE_MODE(M, UNSIGNEDP, TYPE)	\
  do						\
    {						\
      if (GET_MODE_CLASS (M) == MODE_INT	\
	  && GET_MODE_SIZE (M) < 8)		\
	M = DImode;				\
    } while (0)

/* Align argument parameters on the stack to 64-bit, at a minimum.  */
#define PARM_BOUNDARY 64

/* The hardware enforces that the stack pointer should be aligned to
   64-bit at any time.  */
#define STACK_BOUNDARY 64

/* Function entry points are aligned to 64 bits.  */
#define FUNCTION_BOUNDARY 64

/* Maximum alignment required by data of any type.  */
#define BIGGEST_ALIGNMENT 64

/* The best alignment to use in cases where we have a choice.  */
#define FASTEST_ALIGNMENT 64

/* Use a fast alignment when storing arrays of chars in a local.  */
#define LOCAL_ALIGNMENT(TYPE, ALIGN)					\
  (TREE_CODE (TYPE) == ARRAY_TYPE					\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode				\
   && (ALIGN) < FASTEST_ALIGNMENT ? FASTEST_ALIGNMENT : (ALIGN))

/* The load and store instructions won't work if the data is not in
   it's expected alignment.  */
#define STRICT_ALIGNMENT 1

/* We use Pmode as the mode of the size increment operand in an
   `allocate_stack' pattern.  */
#define STACK_SIZE_MODE Pmode

/**** Layout of Source Language Data Types.  */

#define INT_TYPE_SIZE         32
#define SHORT_TYPE_SIZE       16
#define LONG_TYPE_SIZE        64
#define LONG_LONG_TYPE_SIZE   64
#define CHAR_TYPE_SIZE         8
#define FLOAT_TYPE_SIZE       32
#define DOUBLE_TYPE_SIZE      64
#define LONG_DOUBLE_TYPE_SIZE 64

#define INTPTR_TYPE	"long int"
#define UINTPTR_TYPE	"long unsigned int"
#define SIZE_TYPE	"long unsigned int"
#define PTRDIFF_TYPE "long int"

#define SIG_ATOMIC_TYPE "char"

#define INT8_TYPE "char"
#define INT16_TYPE "short int"
#define INT32_TYPE "int"
#define INT64_TYPE "long int"
#define UINT8_TYPE "unsigned char"
#define UINT16_TYPE "short unsigned int"
#define UINT32_TYPE "unsigned int"
#define UINT64_TYPE "long unsigned int"

#define INT_LEAST8_TYPE INT8_TYPE
#define INT_LEAST16_TYPE INT16_TYPE
#define INT_LEAST32_TYPE INT32_TYPE
#define INT_LEAST64_TYPE INT64_TYPE
#define UINT_LEAST8_TYPE UINT8_TYPE
#define UINT_LEAST16_TYPE UINT16_TYPE
#define UINT_LEAST32_TYPE UINT32_TYPE
#define UINT_LEAST64_TYPE UINT64_TYPE

#define INT_FAST8_TYPE INT8_TYPE
#define INT_FAST16_TYPE INT16_TYPE
#define INT_FAST32_TYPE INT32_TYPE
#define INT_FAST64_TYPE INT64_TYPE
#define UINT_FAST8_TYPE UINT8_TYPE
#define UINT_FAST16_TYPE UINT16_TYPE
#define UINT_FAST32_TYPE UINT32_TYPE
#define UINT_FAST64_TYPE UINT64_TYPE

/* `char' is signed by default, like in x86.  */
#define DEFAULT_SIGNED_CHAR 1

/* `wchar_t' is a signed 32-bit type.  The second constant is used by
   cpp, which can't use WCHAR_TYPE.  */
#define WCHAR_TYPE "int"
#define WCHAR_TYPE_SIZE 32

/* `wint_t' is a signed 32-bit type.  */
#define WINT_TYPE "int"
#define WINT_TYPE_SIZE 32

/**** Register Usage.  */

/*** Basic Characteristics of Registers.  */

#define BPF_R0	0
#define BPF_R1	1
#define BPF_R2	2
#define BPF_R3	3
#define BPF_R4	4
#define BPF_R5	5
#define BPF_R6	6
#define BPF_CTX BPF_R6
#define BPF_R7	7
#define BPF_R8	8
#define BPF_R9	9
#define BPF_SP BPF_R9
#define BPF_R10	10
#define BPF_FP  BPF_R10
/* 11 is not a real eBPF hard register and is eliminated or not used
   in the final assembler.  See below.  */

#define FIRST_PSEUDO_REGISTER 12

/* The registers %r0..%r8 are available for general allocation.
   %r9 is the pseudo-stack pointer.
   %r10 is the stack frame, which is read-only.
   %r11 (__arg__) is a fake register that always gets eliminated.  */
#define FIXED_REGISTERS				\
  {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1}

/* %r0..%r5 are clobbered by function calls.  */
#define CALL_USED_REGISTERS				\
  {1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1}

/**** Register Classes.  */

enum reg_class
{
  NO_REGS,		/* no registers in set.  */
  ALL_REGS,		/* all registers.  */
  LIM_REG_CLASSES	/* max value + 1.  */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES
#define GENERAL_REGS ALL_REGS

/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */
#define REG_CLASS_NAMES				\
{						\
  "NO_REGS",					\
  "ALL_REGS"					\
}

/* An initializer containing the contents of the register classes, as
   integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if `MASK & (1 << R)' is 1.

   In eBPF all the hard registers are considered general-purpose
   integer registers.  */
#define REG_CLASS_CONTENTS			\
{						\
   0x00000000, /* NO_REGS */			\
   0x00000fff, /* ALL_REGS */		        \
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */
#define REGNO_REG_CLASS(REGNO) GENERAL_REGS

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */
#define INDEX_REG_CLASS NO_REGS

/* C expression which is nonzero if register number REGNO is suitable
   for use as a base register in operand addresses.  In eBPF every
   hard register can be used for this purpose.  */
#define REGNO_OK_FOR_BASE_P(REGNO) 			\
  ((REGNO) < FIRST_PSEUDO_REGISTER)

/* C expression which is nonzero if register number REGNO is suitable
   for use as an index register in operand addresses.  */
#define REGNO_OK_FOR_INDEX_P(REGNO) false

/**** Debugging Info ****/

/* We cannot support DWARF2 because of the limitations of eBPF.  */

/* elfos.h insists in using DWARF.  Undo that here.  */
#ifdef DWARF2_DEBUGGING_INFO
# undef DWARF2_DEBUGGING_INFO
#endif
#ifdef PREFERRED_DEBUGGING_TYPE
# undef PREFERRED_DEBUGGING_TYPE
#endif

#define DBX_DEBUGGING_INFO

/**** Stack Layout and Calling Conventions.  */

/*** Basic Stack Layout.  */

#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1

/* The argument pointer always points to the first argument.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Unsupported.  */
#define RETURN_ADDR_RTX(count, frame) const0_rtx

/*** Registers That Address the Stack Frame.  */

#define FRAME_POINTER_REGNUM 10
#define STACK_POINTER_REGNUM 9
#define ARG_POINTER_REGNUM 11
#define STATIC_CHAIN_REGNUM 8

/*** Registers elimination.  */

#define ELIMINABLE_REGS					\
  {{ ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM },	\
   { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM }}

/* Define the offset between two registers, one to be eliminated, and
   the other its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)		\
  do								\
    {								\
      (OFFSET) = bpf_initial_elimination_offset ((FROM), (TO));	\
    } while (0)

/*** Passing Function Arguments on the Stack.  */

/* The eBPF ABI doesn't support passing arguments on the stack.  Only
   in the first five registers.  Code in bpf.c assures the stack is
   never used when passing arguments.  However, we still have to
   define the constants below.  */

/* If nonzero, push insns will be used to pass outgoing arguments.  */
#define PUSH_ARGS 0

/* If nonzero, function arguments will be evaluated from last to
   first, rather than from first to last.  */
#define PUSH_ARGS_REVERSED 1

/* Allocate stack space for arguments at the beginning of each
   function.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/*** Passing Arguments in Registers.  */

/* Use an integer in order to keep track of the number of arguments
   passed to a function in integer registers, up to
   MAX_ARGS_IN_REGISTERS.  */
#define CUMULATIVE_ARGS int

/* INIT_CUMULATIVE_ARGS initializes a variable CUM of type
   CUMULATIVE_ARGS for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,FNDECL,N_NAMED_ARGS) \
  memset (&(CUM), 0, sizeof (CUM))

/* Nonzero if N is the number of a hard register in which function
   arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(N) ((N) >= 1 && (N) <= 5)

/*** How Scalar Function Values are Returned.  */

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  This is always %r0 for eBPF.  */
#define LIBCALL_VALUE(MODE)  \
  gen_rtx_REG ((MODE), 0)

/*** Generating Code for Profiling.  */

/* We do not support profiling yet, so do not call `mcount'.  */
#define FUNCTION_PROFILER(FILE, LABELNO) do { } while (0)

/*** Function Entry and Exit.  */

/* We do not require an accurate stack pointer at function return.
   This is because the stack pointer's original value is initialized
   from the frame pointer, rather than decreased, to satisfy the
   kernel's verifier.  Thus, we have to save the stack pointer in
   function prologue and restore it in function epilogue.  If
   EXIT_IGNORE_STACK is not set, then superfluous instructions are
   generated to save and restore the stack pointer after and before
   the function epilogue, respectively.  */
#define EXIT_IGNORE_STACK 1

/**** Support for Nested Functions.  */

/* We have to define TRAMPOLINE_SIZE even if we don't ever generate
   them.  Set to 64 arbitrarily.  */
#define TRAMPOLINE_SIZE 64

/**** Addressing Modes.  */

/* Maximum number of registers that can appear in a valid memory
   address.  */
#define MAX_REGS_PER_ADDRESS 1

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X) 0

/**** Describing Relative Costs of Operations.  */

/* Cost of a branch instruction.  A value of 1 is the default.  */
#define BRANCH_COST(SPEED_P,PREDICTABLE_P) 1

/* The SPARC port says: Nonzero if access to memory by bytes is slow
   and undesirable.  For RISC chips, it means that access to memory by
   bytes is no better than access by words when possible, so grab a
   whole word and maybe make use of that.  */
#define SLOW_BYTE_ACCESS 1

/* Threshold of number of scalar memory-to-memory move instructions,
   _below_ which a sequence of insns should be generated instead of a
   string move insn or a library call.  */
#define MOVE_RATIO(speed) 128

/* Threshold of number of scalar move instructions, _below_ which a
   sequence of insns should be generated to clear memory instead of a
   string clear insn or a library call.  */
#define CLEAR_RATIO(speed) 128

/* Threshold of number of scalar move instructions, _below_ which a
   sequence of insns should be generated to set memory to a constant
   value, instead of a block set insn or a library call.  */
#define SET_RATIO(speed) 128

/* True if it is as good or better to call a constant function address
   than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/**** Dividing the Output into Sections.  */

#define TEXT_SECTION_ASM_OP "\t.text"
#define DATA_SECTION_ASM_OP "\t.data"
#define BSS_SECTION_ASM_OP "\t.bss"

/**** Defining the Output Assembler Language.  */

/*** The Overall Framework of an Assembler File.  */

#define ASM_COMMENT_START ";"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#ifndef ASM_APP_ON
#define ASM_APP_ON " #APP\n"
#endif

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#ifndef ASM_APP_OFF
#define ASM_APP_OFF " #NO_APP\n"
#endif

/*** Output of Data.  */

/*** Output of Uninitialized Variables.  */

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)	\
  do									\
    {									\
      ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
      fprintf ((FILE), "%s", "\t.lcomm\t");				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED ",%u\n",	\
	       (SIZE), (ALIGN) / BITS_PER_UNIT);			\
    }									\
  while (0)

/*** Output and Generation of Labels.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long)(NUM))

/*** Output of Assembler Instructions.  */

#define REGISTER_NAMES						\
  { "%r0", "%r1", "%r2", "%r3", "%r4", "%r5", "%r6", "%r7",	\
    "%r8", "%r9", "%fp", "__arg__" }

#define ADDITIONAL_REGISTER_NAMES		\
  { { "%a", 0 }, { "%ctx", 6 }, { "%r10" , 10 } }

#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX  ""

#define PRINT_OPERAND(STREAM,X,CODE)		\
  bpf_print_operand ((STREAM),(X),(CODE))

#define PRINT_OPERAND_ADDRESS(STREAM,X)		\
  bpf_print_operand_address ((STREAM), (X))

/*** Assembler Commands for Alignment.  */

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(STREAM,LOG)		\
  fprintf (STREAM, "\t.align\t%d\n", (LOG))

/**** Miscellaneous Parameters.  */

/* Specify the machine mode that this machine uses for the index in
   the tablejump instruction.  */
#define CASE_VECTOR_MODE DImode

/* Define if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  */
#define WORD_REGISTER_OPERATIONS 1

/* C expression indicating when insns that read memory in MEM_MODE, an
   integral mode narrower than a word, set the bits outsize of
   MEM_MODE to be either the sign-extension or the zero-extension of
   the data read.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* The maximum number of bytes that a single instruction can move
   quickly between memory and registers or between two memory
   locations.  */
#define MOVE_MAX 8

/* An alias for the machine mode for pointers.  */
#define Pmode DImode

/* An alias for the machine mode used for memory references to
   functions being called, in 'call' RTL expressions.  */
#define FUNCTION_MODE Pmode

/* No libm on eBPF (for now.)  */
#define MATH_LIBRARY ""

/**** libgcc settings.  */

/* Iterating over the global constructors and destructors and
   executing them requires the ability of doing indirect calls.

   eBPF doesn't support indirect calls, so no chance of supporting
   constructors and destructors.  */
#define DO_GLOBAL_CTORS_BODY			\
  do { } while (0)
#define DO_GLOBAL_DTORS_BODY			\
  do { } while (0)

#endif /* ! GCC_BPF_H */
