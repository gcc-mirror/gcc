/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 1989, 1993 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for C/370 MVS by Dave Pitts (pitts@mcdata.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef sun
#include <sys/types.h>
#include <ctype.h>
#endif
#include <time.h>

#define TARGET_VERSION printf (" (370/MVS)");

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-DGCC -Dgcc -DMVS -Dmvs -Asystem(mvs) -Acpu(i370) -Amachine(i370)"

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* The sizes of the code and literals on the current page.  */

extern int mvs_page_code, mvs_page_lit;

/* The current page number and the base page number for the function.  */

extern int mvs_page_num, function_base_page;

/* True if a label has been emitted.  */

extern int mvs_label_emited;

/* The name of the current function.  */

extern char *mvs_function_name;

/* The length of the function name malloc'd area.  */

extern int mvs_function_name_length;

/* The amount of space used for outgoing arguments.  */

extern int current_function_outgoing_args_size;

/* Compile using char instructins (mvc, nc, oc, xc).  On 4341 use this since
   these are more than twice as fast as load-op-store.
   On 3090 don't use this since load-op-store is much faster.  */

#define TARGET_CHAR_INSTRUCTIONS (target_flags & 1)

/* Default target switches */

#define TARGET_DEFAULT 1

/* Macro to define tables used to set the flags.  This is a list in braces
   of pairs in braces, each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							\
{ { "char-instructions", 1},						\
  { "no-char-instructions", -1},					\
  { "", TARGET_DEFAULT} }

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered in instructions
   that operate on numbered bit-fields.  */

#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */

#define BYTES_BIG_ENDIAN 1

/* Define this if MS word of a multiword is the lowest numbered.  */

#define WORDS_BIG_ENDIAN 1

/* Number of bits in an addressible storage unit.  */

#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.  */

#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */

#define UNITS_PER_WORD 4

/* Width in bits of a pointer.  See also the macro `Pmode' defined below.  */

#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */

#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */

#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */

#define STACK_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */

#define FUNCTION_BOUNDARY 32

/* There is no point aligning anything to a rounder boundary than this.  */

#define BIGGEST_ALIGNMENT 64

/* Alignment of field after `int : 0' in a structure.  */

#define EMPTY_FIELD_BOUNDARY 32

/* Define this if move instructions will actually fail to work when given
   unaligned data.  */

#define STRICT_ALIGNMENT 0

/* Define target floating point format.  */

#define TARGET_FLOAT_FORMAT IBM_FLOAT_FORMAT

/* Define character mapping for cross-compiling.  */

#define TARGET_EBCDIC 1

#ifdef HOST_EBCDIC
#define MAP_CHARACTER(c) ((char)(c))
#else
#define MAP_CHARACTER(c) ((char)mvs_map_char (c))
#endif

/* Define maximum length of page minus page escape overhead.  */

#define MAX_MVS_PAGE_LENGTH 4080

/* Define if special allocation order desired.  */

#define REG_ALLOC_ORDER							\
{ 0, 1, 2, 3, 14, 15, 12, 10, 9, 8, 7, 6, 5, 4, 16, 17, 18, 19, 11, 13 }

/* Standard register usage.  */

/* Number of actual hardware registers.  The hardware registers are
   assigned numbers for the compiler from 0 to just below
   FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.
   For the 370, we give the data registers numbers 0-15,
   and the floating point registers numbers 16-19.  */

#define FIRST_PSEUDO_REGISTER 20

/* Define base and page registers.  */

#define BASE_REGISTER 3
#define PAGE_REGISTER 4

/* 1 for registers that have pervasive standard uses and are not available
   for the register allocator.  On the 370 under C/370, R13 is stack (DSA)
   pointer, R12 is the TCA pointer, R3 is the base register, R4 is the page
   origin table pointer and R11 is the arg pointer.  */

#define FIXED_REGISTERS 						\
{ 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/

/* 1 for registers not available across function calls.  These must include
   the FIXED_REGISTERS and also any registers that can be used without being
   saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   NOTE: all floating registers are undefined across calls.  */

#define CALL_USED_REGISTERS 						\
{ 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 }
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19*/

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE) 					\
  ((REGNO) > 15 ? 1 : (GET_MODE_SIZE(MODE)+UNITS_PER_WORD-1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the 370, the cpu registers can hold QI, HI, SI, SF and DF.  The
   even registers can hold DI.  The floating point registers can hold
   either SF or DF.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  ((REGNO) < 16 ? ((REGNO) & 1) == 0 || (MODE) != DImode		\
		: (MODE) == SFmode || (MODE) == DFmode)

/* Value is 1 if it is a good idea to tie two pseudo registers when one has
   mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2)					\
  (((MODE1) == SFmode || (MODE1) == DFmode)				\
   == ((MODE2) == SFmode || (MODE2) == DFmode))

/* Mark external references.  */

#define ENCODE_SECTION_INFO(decl)  					\
  if (DECL_EXTERNAL (decl) && TREE_PUBLIC (decl)) 			\
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* 370 PC isn't overloaded on a register.  */

/* #define PC_REGNUM */

/* Register to use for pushing function arguments.  */

#define STACK_POINTER_REGNUM 13

/* Base register for access to local variables of the function.  */

#define FRAME_POINTER_REGNUM 13

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be
   accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */

#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */

#define ARG_POINTER_REGNUM 11

/* Register in which static-chain is passed to a function.  */

#define STATIC_CHAIN_REGNUM 10

/* Register in which address to store a structure value is passed to
   a function.  */

#define STRUCT_VALUE_REGNUM 1

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
    NO_REGS, ADDR_REGS, DATA_REGS,
    FP_REGS, ALL_REGS, LIM_REG_CLASSES
  };

#define GENERAL_REGS DATA_REGS
#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES 						\
{ "NO_REGS", "ADDR_REGS", "DATA_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.  This is an initializer for
   a vector of HARD_REG_SET of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {0, 0x0fffe, 0x0ffff, 0xf0000, 0xfffff}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) 						\
  ((REGNO) >= 16 ? FP_REGS : (REGNO) != 0 ? ADDR_REGS : DATA_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS ADDR_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)					\
  ((C) == 'a' ? ADDR_REGS :						\
  ((C) == 'd' ? DATA_REGS :						\
  ((C) == 'f' ? FP_REGS   : NO_REGS)))

/* The letters I, J, K, L and M in a register constraint string can be used
   to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
  ((C) == 'I' ? (unsigned) (VALUE) < 256 :				\
   (C) == 'J' ? (unsigned) (VALUE) < 4096 :				\
   (C) == 'K' ? (VALUE) >= -32768 && (VALUE) < 32768 : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  1

/* Given an rtx X being reloaded into a reg required to be in class CLASS,
   return the class of reg to actually use.  In general this is just CLASS;
   but on some machines in some cases it is preferable to use a more
   restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS)				\
    (GET_CODE(X) == CONST_DOUBLE ? FP_REGS :				\
     GET_CODE(X) == CONST_INT ? DATA_REGS :				\
     GET_CODE(X) == LABEL_REF ||					\
     GET_CODE(X) == SYMBOL_REF ||					\
     GET_CODE(X) == CONST ? ADDR_REGS : (CLASS))

/* Return the maximum number of consecutive registers needed to represent
   mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)					\
  ((CLASS) == FP_REGS ? 1 :						\
   (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack makes the stack pointer a
   smaller address.  */

/* #define STACK_GROWS_DOWNWARD */

/* Define this if the nominal address of the stack frame is at the
   high-address end of the local variables; that is, each additional local
   variable allocated goes at a more negative offset in the frame.  */

/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */

#define STARTING_FRAME_OFFSET  						\
     (STACK_POINTER_OFFSET + current_function_outgoing_args_size)

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = STARTING_FRAME_OFFSET

/* If we generate an insn to push BYTES bytes, this says how many the stack
   pointer really advances by.  On the 370, we have no push instruction.  */

/* #define PUSH_ROUNDING(BYTES) */

/* Accumulate the outgoing argument count so we can request the right
   DSA size and determine stack offset.  */

#define ACCUMULATE_OUTGOING_ARGS

/* Define offset from stack pointer, to location where a parm can be
   pushed.  */

#define STACK_POINTER_OFFSET 148

/* Offset of first parameter from the argument pointer register value.  */

#define FIRST_PARM_OFFSET(FNDECL) 0

/* 1 if N is a possible register number for function argument passing.
   On the 370, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list during
   the scan of that argument list.  This data type should hold all
   necessary information about the function itself and about the args
   processed so far, enough to enable macros such as FUNCTION_ARG to
   determine where the next arg should go.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS for a call to
   a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME)  ((CUM) = 0)

/* Update the data in CUM to advance over an argument of mode MODE and
   data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.) */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
 ((CUM) += ((MODE) == DFmode || (MODE) == SFmode			\
	    ? 256							\
	    : (MODE) != BLKmode                 			\
	    ? (GET_MODE_SIZE (MODE) + 3) / 4 				\
	    : (int_size_in_bytes (TYPE) + 3) / 4))

/* Define where to put the arguments to a function.  Value is zero to push
   the argument on the stack, or a hard register in which to store the
   argument.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

/* For an arg passed partly in registers and partly in memory, this is the
   number of registers used.  For args passed entirely in registers or
   entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* Define if returning from a function call automatically pops the
   arguments described by the number-of-args field in the call.  */

#define RETURN_POPS_ARGS(FUNTYPE, STACKSIZE) 0

/* Define how to find the value returned by a function.  VALTYPE is the
   data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 15.  */

#define RET_REG(MODE)	((MODE) == DFmode || (MODE) == SFmode ? 16 : 15)

/* On the 370 the return value is in R15 or R16.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  					\
  gen_rtx(REG, TYPE_MODE (VALTYPE), RET_REG(TYPE_MODE(VALTYPE)))

/* Define how to find the value returned by a library function assuming
   the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx(REG, MODE, RET_REG(MODE))

/* 1 if N is a possible register number for a function value.
   On the 370 under C/370, R15 and R16 are thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 15 || (N) == 16)

/* This macro definition sets up a default value for `main' to return.  */

#define DEFAULT_MAIN_RETURN  c_expand_return (integer_zero_node)

/* This macro generates the assembly code for function entry.
   All of the C/370 environment is preserved.  */

#define FUNCTION_PROLOGUE(FILE, LSIZE)					\
{ 									\
  static int function_label_index = 1;					\
  static int function_first = 0;					\
  static int function_year, function_month, function_day;		\
  static int function_hour, function_minute, function_second;		\
  int i;								\
  if (!function_first)							\
    {									\
      struct tm *function_time;						\
      time_t lcltime;							\
      time (&lcltime);							\
      function_time = localtime (&lcltime);				\
      function_year = function_time->tm_year + 1900;			\
      function_month = function_time->tm_mon + 1;			\
      function_day = function_time->tm_mday;				\
      function_hour = function_time->tm_hour;				\
      function_minute = function_time->tm_min;				\
      function_second = function_time->tm_sec;				\
    }									\
  fprintf (FILE, "\tUSING\t*,15\n");					\
  fprintf (FILE, "\tB\tFPL%03d\n", function_label_index);		\
  fprintf (FILE, "\tDC\tAL1(FPL%03d+4-*)\n", function_label_index + 1);	\
  fprintf (FILE, "\tDC\tX'CE',X'A0',X'10'\n");				\
  fprintf (FILE, "\tDC\tA($PPA2)\n");					\
  fprintf (FILE, "\tDC\tF'%d'\n", 0);					\
  fprintf (FILE, "\tDC\tF'%d'\n", STACK_POINTER_OFFSET + LSIZE		\
			+ current_function_outgoing_args_size);		\
  fprintf (FILE, "FPL%03d\tEQU\t*\n", function_label_index + 1);	\
  fprintf (FILE, "\tDC\tAL2(%d),C'%s'\n", strlen (mvs_function_name),	\
	mvs_function_name);						\
  fprintf (FILE, "\tDS\t0F\n");						\
  if (!function_first)							\
    {									\
      fprintf (FILE, "$PPA2\tEQU\t*\n");				\
      fprintf (FILE, "\tDC\tX'03',X'00',X'33',X'00'\n");		\
      fprintf (FILE, "\tDC\tV(CEESTART),A(0)\n");			\
      fprintf (FILE, "\tDC\tA($TIMES)\n");				\
      fprintf (FILE, "\tDS\t0F\n");					\
      fprintf (FILE, "$TIMES\tEQU\t*\n");				\
      fprintf (FILE, "\tDC\tCL4'%d',CL4'%02d%02d',CL6'%02d%02d00'\n",	\
		      function_year, function_month, function_day,	\
		      function_hour, function_minute, function_second);	\
      fprintf (FILE, "\tDC\tCL2'01',CL4'0100'\n");			\
    }									\
  fprintf (FILE, "\tDS\t0H\n");						\
  fprintf (FILE, "FPL%03d\tEQU\t*\n", function_label_index);		\
  fprintf (FILE, "\tSTM\t14,12,12(13)\n");				\
  fprintf (FILE, "\tL\t2,76(,13)\n");					\
  fprintf (FILE, "\tL\t0,16(,15)\n");					\
  fprintf (FILE, "\tALR\t0,2\n");					\
  fprintf (FILE, "\tCL\t0,12(,12)\n");					\
  fprintf (FILE, "\tBNH\t*+10\n");					\
  fprintf (FILE, "\tL\t15,116(,12)\n");					\
  fprintf (FILE, "\tBALR\t14,15\n");					\
  fprintf (FILE, "\tL\t15,72(,13)\n");					\
  fprintf (FILE, "\tSTM\t15,0,72(2)\n");				\
  fprintf (FILE, "\tMVI\t0(2),X'10'\n");				\
  fprintf (FILE, "\tST\t13,4(,2)\n ");					\
  fprintf (FILE, "\tLR\t13,2\n");					\
  fprintf (FILE, "\tLR\t11,1\n");					\
  fprintf (FILE, "\tDROP\t15\n");					\
  fprintf (FILE, "\tBALR\t%d,0\n", BASE_REGISTER);			\
  fprintf (FILE, "PG%d\tEQU\t*\n", mvs_page_num );			\
  fprintf (FILE, "\tUSING\t*,%d\n", BASE_REGISTER);			\
  fprintf (FILE, "\tL\t%d,=A(PGT%d)\n", PAGE_REGISTER, mvs_page_num);	\
  mvs_page_code = 4;							\
  mvs_page_lit = 4;							\
  mvs_check_page (FILE, 0, 0);						\
  function_base_page = mvs_page_num;					\
  function_first = 1;							\
  function_label_index += 2;						\
}

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
{									\
  if (strlen (NAME) * 2 > mvs_function_name_length)			\
    {									\
      if (mvs_function_name)						\
	free (mvs_function_name);					\
      mvs_function_name = 0;						\
    }									\
  if (!mvs_function_name)						\
    {									\
      mvs_function_name_length = strlen (NAME) * 2;			\
      mvs_function_name = (char *) malloc (mvs_function_name_length);	\
      if (mvs_function_name == 0)					\
	{								\
	  fatal ("virtual memory exceeded");				\
	  abort ();							\
	}								\
    }									\
  if (!strcmp (NAME, "main"))						\
    strcpy (mvs_function_name, "gccmain");				\
  else									\
    strcpy (mvs_function_name, NAME);					\
  fprintf (FILE, "\tDS\t0H\n");						\
  assemble_name (FILE, mvs_function_name);				\
  fputs ("\tCSECT\n", FILE);						\
}

/* This macro generates the assembly code for function exit, on machines
   that need it.  If FUNCTION_EPILOGUE is not defined then individual
   return instructions are generated for each return statement.  Args are
   same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, LSIZE)					\
{									\
  int i;								\
  check_label_emit();							\
  mvs_check_page (FILE,14,0);						\
  fprintf (FILE, "\tL\t13,4(,13)\n");					\
  fprintf (FILE, "\tL\t14,12(,13)\n");					\
  fprintf (FILE, "\tLM\t2,12,28(13)\n");				\
  fprintf (FILE, "\tBALR\t1,14\n");					\
  fprintf (FILE, "\tDC\tA(");						\
  mvs_page_num++;							\
  assemble_name (FILE, mvs_function_name);				\
  fprintf (FILE, ")\n" );						\
  fprintf (FILE, "\tDS\t0F\n" );					\
  fprintf (FILE, "\tLTORG\n");						\
  fprintf (FILE, "\tDS\t0F\n");						\
  fprintf (FILE, "PGT%d\tEQU\t*\n", function_base_page);		\
  mvs_free_label();							\
  for ( i = function_base_page; i < mvs_page_num; i++ )			\
    fprintf (FILE, "\tDC\tA(PG%d)\n", i);				\
}

/* Output assembler code for a block containing the constant parts of a
   trampoline, leaving space for the variable parts.

   On the 370, the trampoline contains these instructions:

        BALR  14,0
        USING *,14
        L     STATIC_CHAIN_REGISTER,X
        L     15,Y
        BR    15
   X    DS    0F
   Y    DS    0F  */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x05E0));	\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x5800 | 	\
			   STATIC_CHAIN_REGNUM << 4));			\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0xE00A));	\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x58F0)); 	\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0xE00E));	\
  ASM_OUTPUT_SHORT (FILE, gen_rtx (CONST_INT, VOIDmode, 0x07FF));	\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);					\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 20

/* Emit RTL insns to initialize the variable parts of a trampoline.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 12)), CXT); \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 16)), FNADDR); \
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) 				\
  fprintf (FILE, "Error: No profiling availble.\n")

/* Define EXIT_IGNORE_STACK if, when returning from a function, the stack
   pointer does not matter (provided there is a frame pointer).  */

#define EXIT_IGNORE_STACK	1

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* These assume that REGNO is a hard or pseudo reg number.  They give
   nonzero only if REGNO is a hard reg of the suitable class or a pseudo
   reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) 					\
  (((REGNO) > 0 && (REGNO) < 16)					\
    || (reg_renumber[REGNO] > 0 && reg_renumber[REGNO] < 16))

#define REGNO_OK_FOR_BASE_P(REGNO) REGNO_OK_FOR_INDEX_P(REGNO)

#define REGNO_OK_FOR_DATA_P(REGNO) 					\
  ((REGNO) < 16 || (unsigned) reg_renumber[REGNO] < 16)

#define REGNO_OK_FOR_FP_P(REGNO) 					\
  ((unsigned) ((REGNO) - 16) < 4 || (unsigned) (reg_renumber[REGNO] - 16) < 4)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.  */

/* 1 if X is a data register.  */

#define DATA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_DATA_P (REGNO (X)))

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* 1 if X is an address register.  */

#define ADDRESS_REG_P(X) (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)						\
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
  || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE		\
  || (GET_CODE (X) == CONST						\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == LABEL_REF)		\
  || (GET_CODE (X) == CONST						\
	  && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF		\
	  && !SYMBOL_REF_FLAG (XEXP (XEXP (X, 0), 0))))

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx and check
   its validity for a certain class.  We have two alternate definitions
   for each of them.  The usual definition accepts all pseudo regs; the
   other rejects them all.  The symbol REG_OK_STRICT causes the latter
   definition to be used.

   Most source files want to accept pseudo regs in the hope that they will
   get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index or if it is
  a pseudo reg.  */

#define REG_OK_FOR_INDEX_P(X)						\
  ((REGNO(X) > 0 && REGNO(X) < 16) || REGNO(X) >= 20)

/* Nonzero if X is a hard reg that can be used as a base reg or if it is
   a pseudo reg.  */

#define REG_OK_FOR_BASE_P(X)	REG_OK_FOR_INDEX_P(X)

#else /* REG_OK_STRICT */

/* Nonzero if X is a hard reg that can be used as an index.  */

#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P(REGNO(X))

/* Nonzero if X is a hard reg that can be used as a base reg.  */

#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P(REGNO(X))

#endif /* REG_OK_STRICT */

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression that is a
   valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

#define COUNT_REGS(X, REGS, FAIL)					\
 if (REG_P (X) && REG_OK_FOR_BASE_P (X))				\
   REGS += 1;								\
 else if (GET_CODE (X) != CONST_INT || (unsigned) INTVAL (X) >= 4096)	\
   goto FAIL;

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (REG_P (X) && REG_OK_FOR_BASE_P (X))				\
    goto ADDR;								\
  if (GET_CODE (X) == PLUS)						\
    {									\
      int regs = 0;							\
      rtx x0 = XEXP (X, 0);						\
      rtx x1 = XEXP (X, 1);						\
      if (GET_CODE (x0) == PLUS)					\
	{								\
	  COUNT_REGS (XEXP (x0, 0), regs, FAIL);			\
	  COUNT_REGS (XEXP (x0, 1), regs, FAIL);			\
	  COUNT_REGS (x1, regs, FAIL);					\
	  if (regs == 2)						\
	    goto ADDR;							\
	}								\
      else if (GET_CODE (x1) == PLUS)					\
	{								\
	  COUNT_REGS (x0, regs, FAIL);					\
	  COUNT_REGS (XEXP (x1, 0), regs, FAIL);			\
	  COUNT_REGS (XEXP (x1, 1), regs, FAIL);			\
	  if (regs == 2)						\
	    goto ADDR;							\
	}								\
      else								\
	{								\
	  COUNT_REGS (x0, regs, FAIL);					\
	  COUNT_REGS (x1, regs, FAIL);					\
	  if (regs != 0)						\
	    goto ADDR;							\
	}								\
    }									\
  FAIL: ;								\
}

/* The 370 has no mode dependent addresses.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
{									\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))		\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),				\
		   copy_to_mode_reg (SImode, XEXP (X, 1)));		\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))		\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),				\
		   copy_to_mode_reg (SImode, XEXP (X, 0)));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)		\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),				\
		   force_operand (XEXP (X, 0), 0));			\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)		\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),				\
		   force_operand (XEXP (X, 1), 0));			\
  if (memory_address_p (MODE, X))					\
    goto WIN;								\
}

/* Specify the machine mode that this machine uses for the index in the
   tablejump instruction.  */

#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table to contain
   offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */

/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */

#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* Define this if fixuns_trunc is the same as fix_trunc.  */

#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* We use "unsigned char" as default.  */

#define DEFAULT_SIGNED_CHAR 0

/* This is the kind of divide that is easiest to do in the general case.  */

#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Max number of bytes we can move from memory to memory in one reasonably
   fast instruction.  */

#define MOVE_MAX 256

/* Define this if zero-extension is slow (more than one real instruction).  */

#define SLOW_ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */

#define SLOW_BYTE_ACCESS 1

/* Define if shifts truncate the shift count which implies one can omit
   a sign-extension or zero-extension of a shift count.  */

/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)	(OUTPREC != 16)

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

/* #define STORE_FLAG_VALUE -1 */

/* When a prototype says `char' or `short', really pass an `int'.  */

#define PROMOTE_PROTOTYPES

/* Don't perform CSE on function addresses.  */

#define NO_FUNCTION_CSE

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode SImode

/* A function address in a call instruction is a byte address (for
   indexing purposes) so give the MEM rtx a byte's mode.  */

#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX whose
   rtx-code is CODE.  The body of this macro is a portion of a switch
   statement.  If the code is computed here, return it with a return
   statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX, CODE, OUTERCODE)				\
  case CONST_INT:							\
    if ((unsigned) INTVAL (RTX) < 0xfff) return 1;			\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 2;								\
  case CONST_DOUBLE:							\
    return 4;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Store in cc_status the expressions that the condition codes will
   describe after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.

   On the 370, load insns do not alter the cc's.  However, in some
   cases these instructions can make it possibly invalid to use the
   saved cc's.  In those cases we clear out some or all of the saved
   cc's so they won't be used.  */

#define NOTICE_UPDATE_CC(EXP, INSN)					\
{									\
  rtx exp = (EXP);							\
  if (GET_CODE (exp) == PARALLEL) /* Check this */			\
    exp = XVECEXP (exp, 0, 0);						\
  if (GET_CODE (exp) != SET)						\
    CC_STATUS_INIT;							\
  else									\
    {									\
      if (XEXP (exp, 0) == cc0_rtx)					\
	{								\
	  cc_status.value1 = XEXP (exp, 0);				\
	  cc_status.value2 = XEXP (exp, 1);				\
	  cc_status.flags = 0;						\
	}								\
      else								\
	{								\
	  if (cc_status.value1						\
	      && reg_mentioned_p (XEXP (exp, 0), cc_status.value1))	\
	    cc_status.value1 = 0;					\
	  if (cc_status.value2						\
	      && reg_mentioned_p (XEXP (exp, 0), cc_status.value2))	\
	    cc_status.value2 = 0;					\
	  switch (GET_CODE (XEXP (exp, 1)))				\
	    {								\
	      case PLUS:     case MINUS: case MULT:   /* case UMULT: */	\
	      case DIV:      case UDIV:  case NEG:    case ASHIFT:	\
	      case ASHIFTRT: case AND:   case IOR:    case XOR:		\
	      case ABS:      case NOT:					\
		CC_STATUS_SET (XEXP (exp, 0), XEXP (exp, 1));		\
	    }								\
	}								\
    }									\
}


#define CC_STATUS_SET(V1, V2)						\
{									\
  cc_status.flags = 0;							\
  cc_status.value1 = (V1);						\
  cc_status.value2 = (V2);						\
  if (cc_status.value1							\
      && reg_mentioned_p (cc_status.value1, cc_status.value2))		\
    cc_status.value2 = 0;						\
}

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV) 				\
{ if (cc_status.flags & CC_NO_OVERFLOW)	return NO_OV; return NORMAL; }

/* Control the assembler format that we output.  */

#define TEXT_SECTION_ASM_OP "* Program text area"
#define DATA_SECTION_ASM_OP "* Program data area"
#define INIT_SECTION_ASM_OP "* Program initialization area"
#define CTOR_LIST_BEGIN		/* NO OP */
#define CTOR_LIST_END		/* NO OP */

/* How to refer to registers in assembler output.  This sequence is
   indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{ "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",			\
  "8",  "9", "10", "11", "12", "13", "14", "15",			\
  "0",  "2",  "4",  "6"							\
}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#define ASM_FILE_START(FILE) fputs ("\tCSECT\n", FILE);
#define ASM_FILE_END(FILE) fputs ("\tEND\n", FILE);
#define ASM_IDENTIFY_GCC(FILE)
#define ASM_COMMENT_START "*"
#define ASM_APP_OFF ""
#define ASM_APP_ON ""

#define ASM_OUTPUT_LABEL(FILE, NAME) 					\
{ assemble_name (FILE, NAME); fputs ("\tEQU\t*\n", FILE); }

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	/* NO OP */

#define ASM_GLOBALIZE_LABEL(FILE, NAME)					\
{ fputs ("\tENTRY\t", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE); }

/* MVS externals are limited to 8 characters, upper case only.
   The '_' is mapped to '@', except for MVS functions, then '#'.  */

#define MAX_MVS_LABEL_SIZE 8

#define ASM_OUTPUT_LABELREF(FILE, NAME)					\
{									\
  char *bp, ch, temp[MAX_MVS_LABEL_SIZE + 1];				\
  if (strlen (NAME) > MAX_MVS_LABEL_SIZE)				\
    {									\
      strncpy (temp, NAME, MAX_MVS_LABEL_SIZE);				\
      temp[MAX_MVS_LABEL_SIZE] = '\0';					\
    }									\
  else									\
    strcpy (temp,NAME);							\
  if (!strcmp (temp,"main"))						\
    strcpy (temp,"gccmain");						\
  if (mvs_function_check (temp))					\
    ch = '#';								\
  else									\
    ch = '@';								\
  for (bp = temp; *bp; bp++)						\
    {									\
      if (islower (*bp)) *bp = toupper (*bp);				\
      if (*bp == '_') *bp = ch;						\
    }									\
  fprintf (FILE, "%s", temp);						\
}

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* Generate internal label.  Since we can branch here from off page, we
   must reload the base register.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM) 			\
{									\
  if (!strcmp (PREFIX,"L"))						\
    {									\
      mvs_add_label(NUM);						\
      mvs_label_emited = 1;						\
    }									\
  fprintf (FILE, "%s%d\tEQU\t*\n", PREFIX, NUM);			\
}

/* Generate case label.  */

#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE)			\
   fprintf (FILE, "%s%d\tEQU\t*\n", PREFIX, NUM)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  				\
  mvs_check_page (FILE, 4, 0);						\
  fprintf (FILE, "\tDC\tA(L%d)\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) 			\
  mvs_check_page (FILE, 4, 0);						\
  fprintf (FILE, "\tDC\tA(L%d-L%d)\n", VALUE, REL)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE, REGNO)				\
  mvs_check_page (FILE, 8, 4);						\
  fprintf (FILE, "\tS\t13,=F'4'\n\tST\t%s,%d(13)\n",			\
     reg_names[REGNO], STACK_POINTER_OFFSET)

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE, REGNO)					\
  mvs_check_page (FILE, 8, 0);						\
  fprintf (FILE, "\tL\t%s,%d(13)\n\tLA\t13,4(13)\n",			\
     reg_names[REGNO], STACK_POINTER_OFFSET)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)					\
  fprintf (FILE, "\tDC\tD'%.18G'\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE, VALUE)					\
  fprintf (FILE, "\tDC\tE'%.9G'\n", (VALUE))

/* This outputs an integer, if not a CONST_INT must be address constant.  */

#define ASM_OUTPUT_INT(FILE, EXP)					\
{ 									\
  if (GET_CODE (EXP) == CONST_INT)					\
    {									\
      fprintf (FILE, "\tDC\tF'");					\
      output_addr_const (FILE, EXP);					\
      fprintf (FILE, "'\n");						\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tDC\tA(");					\
      output_addr_const (FILE, EXP);					\
      fprintf (FILE, ")\n");						\
    }									\
}

/* This outputs a short integer.  */

#define ASM_OUTPUT_SHORT(FILE, EXP)					\
{									\
  fprintf (FILE, "\tDC\tH'");						\
  output_addr_const (FILE, EXP);					\
  fprintf (FILE, "'\n");						\
}

/* This outputs a byte sized integer.  */

#define ASM_OUTPUT_CHAR(FILE, EXP)					\
  fprintf (FILE, "\tDC\tX'%02X'\n", INTVAL (EXP) )

#define ASM_OUTPUT_BYTE(FILE, VALUE)					\
  fprintf (FILE, "\tDC\tX'%02X'\n", VALUE)

/* This outputs a text string.  The string are chopped up to fit into
   an 80 byte record.  Also, control and special characters, interpreted
   by the IBM assembler, are output numerically.  */

#define MVS_ASCII_TEXT_LENGTH 48

#define ASM_OUTPUT_ASCII(FILE, PTR, LEN)				\
{									\
  int i, j;								\
  int c;								\
  for (j = 0, i = 0; i < LEN; j++, i++)					\
    {									\
      c = PTR[i];							\
      if (iscntrl (c) || c == '&')					\
	{								\
	  if (j % MVS_ASCII_TEXT_LENGTH != 0 )				\
	    fprintf (FILE, "'\n");					\
	  j = -1;							\
	  if (c == '&') c = MAP_CHARACTER (c);				\
	  fprintf (FILE, "\tDC\tX'%X'\n", c );				\
	}								\
      else								\
	{								\
	  if (j % MVS_ASCII_TEXT_LENGTH == 0)				\
            fprintf (FILE, "\tDC\tC'%c", c);				\
          if ( c == '\'' )                                       	\
	    fprintf (FILE, "%c%c", c, c);                        	\
	  else                                                   	\
	    fprintf (FILE, "%c", c);                             	\
	  if (j % MVS_ASCII_TEXT_LENGTH == MVS_ASCII_TEXT_LENGTH - 1)	\
	    fprintf (FILE, "'\n" );					\
	}								\
    }									\
  if (j % MVS_ASCII_TEXT_LENGTH != 0)					\
    fprintf (FILE, "'\n");						\
}

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE, LOG)					\
  if (LOG)								\
    {									\
      if ((LOG) == 1)							\
        fprintf (FILE, "\tDS\t0H\n" );					\
      else 								\
        fprintf (FILE, "\tDS\t0F\n" );					\
    }									\

/* The maximum length of memory that the IBM assembler will allow in one
   DS operation.  */

#define MAX_CHUNK 32767

/* A C statement to output to the stdio stream FILE an assembler
   instruction to advance the location counter by SIZE bytes. Those
   bytes should be zero when loaded.  */

#define ASM_OUTPUT_SKIP(FILE, SIZE)  					\
{									\
  int s, k;								\
  for (s = (SIZE); s > 0; s -= MAX_CHUNK)				\
    {									\
      if (s > MAX_CHUNK)						\
	k = MAX_CHUNK;							\
      else								\
	k = s;								\
      fprintf (FILE, "\tDS\tXL%d\n", k);				\
    }									\
}

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of a common-label named NAME whose
   size is SIZE bytes.  The variable ROUNDED is the size rounded up
   to whatever alignment the caller wants.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED) 			\
{									\
  fputs ("\tENTRY\t", FILE);						\
  assemble_name (FILE, NAME);						\
  fputs ("\n", FILE);							\
  fprintf (FILE, "\tDS\t0F\n");						\
  ASM_OUTPUT_LABEL (FILE,NAME);						\
  ASM_OUTPUT_SKIP (FILE,SIZE);						\
}

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of a local-common-label named NAME
   whose size is SIZE bytes.  The variable ROUNDED is the size
   rounded up to whatever alignment the caller wants.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED) 			\
{									\
  fprintf (FILE, "\tDS\t0F\n");						\
  ASM_OUTPUT_LABEL (FILE,NAME);						\
  ASM_OUTPUT_SKIP (FILE,SIZE);						\
}

/* Store in OUTPUT a string (made with alloca) containing an
   assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)  		\
{									\
  (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10);			\
  sprintf ((OUTPUT), "%s%d", (NAME), (LABELNO));			\
}

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */

#define TARGET_BELL	47
#define TARGET_BS	22
#define TARGET_TAB	5
#define TARGET_NEWLINE	21
#define TARGET_VT	11
#define TARGET_FF	12
#define TARGET_CR	13

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE)					\
{									\
  switch (GET_CODE (X))							\
    {									\
      static char curreg[4];						\
      case REG:								\
	if (CODE == 'N')						\
	    strcpy (curreg, reg_names[REGNO (X) + 1]);			\
	else								\
	    strcpy (curreg, reg_names[REGNO (X)]);			\
	fprintf (FILE, "%s", curreg);					\
	break;								\
      case MEM:								\
	{								\
	  rtx addr = XEXP (X, 0);					\
	  if (CODE == 'O')						\
	    {								\
	      if (GET_CODE (addr) == PLUS)				\
		fprintf (FILE, "%d", INTVAL (XEXP (addr, 1)));		\
	      else							\
		fprintf (FILE, "0");					\
	    }								\
	  else if (CODE == 'R')						\
	    {								\
	      if (GET_CODE (addr) == PLUS)				\
		fprintf (FILE, "%s", reg_names[REGNO (XEXP (addr, 0))]);\
	      else							\
		fprintf (FILE, "%s", reg_names[REGNO (addr)]);		\
	    }								\
	  else								\
	    output_address (XEXP (X, 0));				\
	}								\
	break;								\
      case SYMBOL_REF:							\
      case LABEL_REF:							\
	mvs_page_lit += 4;						\
	if (SYMBOL_REF_FLAG (X)) fprintf (FILE, "=V(");			\
	else                     fprintf (FILE, "=A(");			\
	output_addr_const (FILE, X);					\
	fprintf (FILE, ")");						\
	break;								\
      case CONST_INT:					        	\
	if (CODE == 'B')						\
	  fprintf (FILE, "%d", INTVAL (X) & 0xff);			\
	else if (CODE == 'X')						\
	  fprintf (FILE, "%02X", INTVAL (X) & 0xff);			\
	else if (CODE == 'h')						\
	  fprintf (FILE, "%d", (INTVAL (X) << 16) >> 16);		\
	else if (CODE == 'H')						\
	  {								\
	    mvs_page_lit += 4;						\
	    fprintf (FILE, "=F'%d'", (INTVAL (X) << 16) >> 16);		\
	  }								\
	else								\
	  {								\
	    mvs_page_lit += 4;						\
	    fprintf (FILE, "=F'%d'", INTVAL (X));			\
	  }								\
	break;								\
      case CONST_DOUBLE:						\
	if (GET_MODE (X) == DImode)					\
	  {								\
	    if (CODE == 'M')						\
	      {								\
		mvs_page_lit += 4;					\
		fprintf (FILE, "=XL4'%08X'", CONST_DOUBLE_LOW (X));	\
	      }								\
	    else if (CODE == 'L')					\
	      {								\
		mvs_page_lit += 4;					\
		fprintf (FILE, "=XL4'%08X'", CONST_DOUBLE_HIGH (X));	\
	      }								\
	    else							\
	      {								\
		mvs_page_lit += 8;					\
		fprintf (FILE, "=XL8'%08X%08X'", CONST_DOUBLE_LOW (X),	\
			CONST_DOUBLE_HIGH (X));				\
	      }								\
	  }								\
	else								\
	  { 								\
	    union { double d; int i[2]; } u;				\
	    u.i[0] = CONST_DOUBLE_LOW (X);				\
	    u.i[1] = CONST_DOUBLE_HIGH (X);				\
	    if (GET_MODE (X) == SFmode)					\
	      {								\
		mvs_page_lit += 4;					\
		fprintf (FILE, "=E'%.9G'", u.d);			\
	      }								\
	    else							\
	      {								\
		mvs_page_lit += 8;					\
		fprintf (FILE, "=D'%.18G'", u.d);			\
	      }								\
	  }								\
	break;								\
      case CONST:							\
	if (GET_CODE (XEXP (X, 0)) == PLUS				\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
	  {								\
	    mvs_page_lit += 4;						\
	    if (SYMBOL_REF_FLAG (XEXP (XEXP (X, 0), 0)))		\
	      {								\
		fprintf (FILE, "=V(");					\
		ASM_OUTPUT_LABELREF (FILE,				\
				  XSTR (XEXP (XEXP (X, 0), 0), 0));	\
		fprintf (FILE, ")\n\tA\t%s,=F'%d'", curreg,		\
				  INTVAL (XEXP (XEXP (X, 0), 1)));	\
	      }								\
	    else							\
	      {								\
		fprintf (FILE, "=A(");					\
		output_addr_const (FILE, X);				\
		fprintf (FILE, ")");					\
	      }								\
	  }								\
	else								\
	  {								\
	    mvs_page_lit += 4;						\
	    fprintf (FILE, "=F'");					\
	    output_addr_const (FILE, X);				\
	    fprintf (FILE, "'");					\
	  }								\
	break;								\
      default:								\
	abort();							\
    }									\
}

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)				\
{									\
  rtx breg, xreg, offset, plus;						\
  									\
  switch (GET_CODE (ADDR))						\
    {									\
      case REG:								\
	fprintf (FILE, "0(%s)", reg_names[REGNO (ADDR)]);		\
	break;								\
      case PLUS:							\
	breg = 0;							\
	xreg = 0;							\
	offset = 0;							\
	if (GET_CODE (XEXP (ADDR, 0)) == PLUS)				\
	  {								\
	    if (GET_CODE (XEXP (ADDR, 1)) == REG)			\
	      breg = XEXP (ADDR, 1);					\
	    else							\
	      offset = XEXP (ADDR, 1);					\
	    plus = XEXP (ADDR, 0);					\
	  }								\
	else								\
	  {								\
	    if (GET_CODE (XEXP (ADDR, 0)) == REG)			\
	      breg = XEXP (ADDR, 0);					\
	    else							\
	      offset = XEXP (ADDR, 0);					\
	    plus = XEXP (ADDR, 1);					\
	  }								\
	if (GET_CODE (plus) == PLUS)					\
	  {								\
	    if (GET_CODE (XEXP (plus, 0)) == REG)			\
	      {								\
		if (breg)						\
		  xreg = XEXP (plus, 0);				\
		else							\
		  breg = XEXP (plus, 0);				\
	      }								\
	    else							\
	      {								\
		offset = XEXP (plus, 0);				\
	      }								\
	    if (GET_CODE (XEXP (plus, 1)) == REG)			\
	      {								\
		if (breg)						\
		  xreg = XEXP (plus, 1);				\
		else							\
		  breg = XEXP (plus, 1);				\
	      }								\
	    else							\
	      {								\
		offset = XEXP (plus, 1);				\
	      }								\
	  }								\
	else if (GET_CODE (plus) == REG)				\
	  {								\
	    if (breg)							\
	      xreg = plus;						\
	    else							\
	      breg = plus;						\
	  }								\
	else								\
	  {								\
	    offset = plus;						\
	  }								\
	if (offset)							\
	  {								\
	    if (GET_CODE (offset) == LABEL_REF)				\
	      fprintf (FILE, "L%d",					\
			CODE_LABEL_NUMBER (XEXP (offset, 0)));		\
	    else							\
	      output_addr_const (FILE, offset);				\
	  }								\
	else								\
	  fprintf (FILE, "0");						\
	if (xreg)							\
	    fprintf (FILE, "(%s,%s)",					\
		    reg_names[REGNO (xreg)], reg_names[REGNO (breg)]); 	\
	else								\
	  fprintf (FILE, "(%s)", reg_names[REGNO (breg)]);		\
	break;								\
      default:								\
	mvs_page_lit += 4;						\
	if (SYMBOL_REF_FLAG (ADDR)) fprintf (FILE, "=V(");		\
	else                        fprintf (FILE, "=A(");		\
	output_addr_const (FILE, ADDR);					\
	fprintf (FILE, ")");						\
	break;								\
    }									\
}
