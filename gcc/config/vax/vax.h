/* Definitions of target machine for GNU compiler.  VAX version.
   Copyright (C) 1987-2025 Free Software Foundation, Inc.

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


/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__vax__");		\
      builtin_assert ("cpu=vax");		\
      builtin_assert ("machine=vax");		\
      if (TARGET_G_FLOAT)			\
	{					\
	  builtin_define ("__GFLOAT");		\
	  builtin_define ("__GFLOAT__");	\
	}					\
    }						\
  while (0)

/* Use -J option for long branch support with Unix assembler.  */

#define ASM_SPEC "-J"

/* Choose proper libraries depending on float format.
   Note that there are no profiling libraries for g-format.
   Also use -lg for the sake of dbx.  */

#define LIB_SPEC "%{g:-lg}\
 %{mg:%{lm:-lmg} -lcg \
  %{p:%eprofiling not supported with -mg\n}\
  %{pg:%eprofiling not supported with -mg\n}}\
 %{!mg:%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"

/* Run-time compilation parameters selecting different hardware subsets.  */

/* Nonzero if ELF.  Redefined by vax/elf.h.  */
#define TARGET_ELF 0

/* Use BSD names for udiv and umod libgcc calls.  */
#define TARGET_BSD_DIVMOD 1

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_UNIX_ASM)
#endif


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the VAX.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on the VAX.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* This is not true on the VAX.  */
#define WORDS_BIG_ENDIAN 0

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY (TARGET_VAXC_ALIGNMENT ? 8 : 32)

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS (! TARGET_VAXC_ALIGNMENT)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* No structure field wants to be aligned rounder than this.  */
#define BIGGEST_FIELD_ALIGNMENT (TARGET_VAXC_ALIGNMENT ? 8 : 32)

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* Let's keep the stack somewhat aligned.  */
#define STACK_BOUNDARY 32

/* The table of an ADDR_DIFF_VEC must be contiguous with the case
   opcode, it is part of the case instruction.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 0

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 17

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the VAX, these are the AP, FP, SP and PC.  */
#define FIXED_REGISTERS {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1}

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* VAX pc is overloaded on a register.  */
#define PC_REGNUM VAX_PC_REGNUM

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM VAX_SP_REGNUM

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM VAX_FP_REGNUM

/* Offset from the frame pointer register value to the top of stack.  */
#define FRAME_POINTER_CFA_OFFSET(FNDECL) 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM VAX_AP_REGNUM

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 0

/* Register in which address to store a structure value
   is passed to a function.  */
#define VAX_STRUCT_VALUE_REGNUM 1

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

/* The VAX has only one kind of registers, so NO_REGS and ALL_REGS
   are the only classes.  */

enum reg_class { NO_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES	\
  { "NO_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0}, {0xffff}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ((void)(REGNO), ALL_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS ALL_REGS
#define BASE_REG_CLASS ALL_REGS


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Given an rtx for the address of a frame,
   return an rtx for the address of the word in the frame
   that holds the dynamic chain--the previous frame's address.  */
#define DYNAMIC_CHAIN_ADDRESS(FRAME) plus_constant (Pmode, (FRAME), 12)

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the VAX, -(sp) pushes only the bytes of the operands.  */
#define PUSH_ROUNDING(BYTES) (BYTES)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 4

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the VAX the return value is in R0 regardless.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)	\
  gen_rtx_REG (TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the VAX the return value is in R0 regardless.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, 0)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for a function value.
   On the VAX, R0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* 1 if N is a possible register number for function argument passing.
   On the VAX, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) ((void) (N), 0)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the VAX, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the VAX, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM) = 0)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define VAX_FUNCTION_PROFILER_NAME "mcount"
#define FUNCTION_PROFILER(FILE, LABELNO)			\
  do								\
    {								\
      char label[256];						\
      ASM_GENERATE_INTERNAL_LABEL (label, "LP", (LABELNO));	\
      fprintf (FILE, "\tmovab ");				\
      assemble_name (FILE, label);				\
      asm_fprintf (FILE, ",%Rr0\n\tjsb %s\n",			\
		   VAX_FUNCTION_PROFILER_NAME);			\
    }								\
  while (0)

/* This macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  */
#define ELIMINABLE_REGS {{FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

/* On the VAX, FRAME_POINTER_REQUIRED is always 1, so the definition of this
   macro doesn't matter for register eliminations, but it should still
   give realistic data for rtx_addr_can_trap_p.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = get_frame_size ())

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 15

/* Byte offset of return address in a stack frame.  The "saved PC" field
   is in element [4] when treating the frame as an array of longwords.  */

#define RETURN_ADDRESS_OFFSET	(4 * UNITS_PER_WORD)	/* 16 */

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.
   FRAMEADDR is already the frame pointer of the COUNT frame, so we
   can ignore COUNT.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)					\
  ((COUNT == 0)								\
   ? gen_rtx_MEM (Pmode, plus_constant (Pmode, FRAME,			\
					RETURN_ADDRESS_OFFSET))		\
   : (rtx) 0)


/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1

#define HAVE_PRE_DECREMENT 1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.cc during register
   allocation.  */

#define REGNO_OK_FOR_INDEX_P(regno)	\
  ((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)
#define REGNO_OK_FOR_BASE_P(regno)	\
  ((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X) legitimate_constant_address_p (X)

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

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) 1

/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) 1

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))

/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE 1

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* If a memory-to-memory move would take MOVE_RATIO or more simple
   move-instruction pairs, we will do a cpymem or libcall instead.  */
#define MOVE_RATIO(speed) ((speed) ? 6 : 3)
#define CLEAR_RATIO(speed) ((speed) ? 6 : 2)
#define SET_RATIO(speed) ((speed) ? 6 : 2)

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* We need to reject symbol references in PIC code except for address
   loads, handled elsewhere.  */
#define LEGITIMATE_PIC_OPERAND_P(x)	\
  vax_acceptable_pic_operand_p ((x), false, false)

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Specify the cost of a branch insn; roughly the number of extra insns that
   should be added to avoid a branch.

   Branches are extremely cheap on the VAX while the shift insns often
   used to replace branches can be expensive.  */

#define BRANCH_COST(speed_p, predictable_p) 0

/* Given a comparison code (NE, EQ, etc.) and the operands of a COMPARE,
   return the mode to be used for the comparison.  As we have the same
   interpretation of condition codes across all the instructions we just
   return the narrowest mode suitable for the comparison code requested.  */
#define SELECT_CC_MODE(OP, X, Y) vax_select_cc_mode (OP, X, Y)

/* Control the assembler format that we output.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at
   the end of the line.  */

#define ASM_COMMENT_START "#"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).
   The register names will be prefixed by REGISTER_PREFIX, if any.  */

#define REGISTER_PREFIX ""
#define REGISTER_NAMES					\
  { "r0", "r1",  "r2",  "r3", "r4", "r5", "r6", "r7",	\
    "r8", "r9", "r10", "r11", "ap", "fp", "sp", "pc",	\
    "psl" }

/* VAX specific: which type character is used for type double?  */

#define ASM_DOUBLE_CHAR (TARGET_G_FLOAT ? 'g' : 'd')

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".globl "

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%ld", PREFIX, (long)(NUM))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tpushl %s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)					\
  fprintf (FILE, "\tmovl (%s)+,%s\n", reg_names[STACK_POINTER_REGNUM],	\
	   reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
   (The VAX does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)		\
  do							\
    {							\
      char label[256];					\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));\
      fprintf (FILE, "\t.long ");			\
      assemble_name (FILE, label);			\
      fprintf (FILE, "\n");				\
    }							\
  while (0)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)	\
  do								\
    {								\
      char label[256];						\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", (VALUE));	\
      fprintf (FILE, "\t.word ");				\
      assemble_name (FILE, label);				\
      ASM_GENERATE_INTERNAL_LABEL (label, "L", (REL));		\
      fprintf (FILE, "-");					\
      assemble_name (FILE, label);				\
      fprintf (FILE, "\n");					\
    }								\
  while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  fprintf (FILE, "\t.align %d\n", (LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %u\n", (int)(SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  ( fputs (".comm ", (FILE)),				\
    assemble_name ((FILE), (NAME)),			\
    fprintf ((FILE), ",%u\n", (int)(ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  ( fputs (".lcomm ", (FILE)),				\
    assemble_name ((FILE), (NAME)),			\
    fprintf ((FILE), ",%u\n", (int)(ROUNDED)))

/* Print an instruction operand X on file FILE.
   CODE is the code from the %-spec that requested printing this operand;
   if `%z3' was used to print operand 3, then CODE is 'z'.

VAX operand formatting codes:

 letter	   print
   c	direct branch condition
   C	reverse branch condition
   D	64-bit immediate operand
   B	the low 8 bits of the complement of a constant operand
   H	the low 16 bits of the complement of a constant operand
   M	a mask for the N highest bits of a word
   N	the complement of a constant integer operand
   P	constant operand plus 1
   R	32 - constant operand
   b	the low 8 bits of a negated constant operand
   h	the low 16 bits of a negated constant operand
   #	'd' or 'g' depending on whether dfloat or gfloat is used
   |	register prefix  */

/* The purpose of D is to get around a quirk or bug in VAX assembler
   whereby -1 in a 64-bit immediate operand means 0x00000000ffffffff,
   which is not a 64-bit minus one.  As a workaround, we output negative
   values in hex.  */
#if HOST_BITS_PER_WIDE_INT == 64
#  define NEG_HWI_PRINT_HEX16 HOST_WIDE_INT_PRINT_HEX
#else
#  define NEG_HWI_PRINT_HEX16 "0xffffffff%08lx"
#endif

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)  \
  ((CODE) == '#' || (CODE) == '|')

#define PRINT_OPERAND(FILE, X, CODE)  \
  print_operand (FILE, X, CODE)

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in output-vax.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
  print_operand_address (FILE, ADDR)

/* This is a blatent lie.  However, it's good enough, since we don't
   actually have any code whatsoever for which this isn't overridden
   by the proper FDE definition.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, PC_REGNUM)

/* Upon failure to find the bit the FFS hardware instruction returns
   the position of the bit immediately following the field specified.  */
#define CTZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE)	\
  ((VALUE) = GET_MODE_BITSIZE (MODE), 2)
