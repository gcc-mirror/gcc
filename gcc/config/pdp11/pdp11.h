/* Definitions of target machine for GNU compiler, for the pdp-11
   Copyright (C) 1994-2017 Free Software Foundation, Inc.
   Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

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

#define CONSTANT_POOL_BEFORE_FUNCTION	0

/* check whether load_fpu_reg or not */
#define LOAD_FPU_REG_P(x) ((x) >= AC0_REGNUM && (x) <= AC3_REGNUM)
#define NO_LOAD_FPU_REG_P(x) ((x) == AC4_REGNUM || (x) == AC5_REGNUM)
#define FPU_REG_P(x)	(LOAD_FPU_REG_P(x) || NO_LOAD_FPU_REG_P(x))
#define CPU_REG_P(x)	((x) <= PC_REGNUM)

/* Names to predefine in the preprocessor for this target machine.  */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("pdp11");		\
    }						\
  while (0)


/* Generate DBX debugging information.  */

#define DBX_DEBUGGING_INFO

#define TARGET_40_PLUS		(TARGET_40 || TARGET_45)
#define TARGET_10		(! TARGET_40_PLUS)

#define TARGET_UNIX_ASM_DEFAULT	0

#define ASSEMBLER_DIALECT	(TARGET_UNIX_ASM ? 1 : 0)



/* TYPE SIZES */
#define SHORT_TYPE_SIZE		16
#define INT_TYPE_SIZE		(TARGET_INT16 ? 16 : 32)
#define LONG_TYPE_SIZE		32
#define LONG_LONG_TYPE_SIZE	64     

/* if we set FLOAT_TYPE_SIZE to 32, we could have the benefit 
   of saving core for huge arrays - the definitions are 
   already in md - but floats can never reside in 
   an FPU register - we keep the FPU in double float mode 
   all the time !! */
#define FLOAT_TYPE_SIZE		(TARGET_FLOAT32 ? 32 : 64)
#define DOUBLE_TYPE_SIZE	64
#define LONG_DOUBLE_TYPE_SIZE	64

/* machine types from ansi */
#define SIZE_TYPE "unsigned int" 	/* definition of size_t */
#define WCHAR_TYPE "int" 		/* or long int???? */
#define WCHAR_TYPE_SIZE 16

#define PTRDIFF_TYPE "int"

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is first.  */
#define WORDS_BIG_ENDIAN 1

/* Define that floats are in VAX order, not high word first as for ints.  */
#define FLOAT_WORDS_BIG_ENDIAN 0

/* Width of a word, in units (bytes). 

   UNITS OR BYTES - seems like units */
#define UNITS_PER_WORD 2

/* This machine doesn't use IEEE floats.  */
/* Because the pdp11 (at least Unix) convention for 32-bit ints is
   big endian, opposite for what you need for float, the vax float
   conversion routines aren't actually used directly.  But the underlying
   format is indeed the vax/pdp11 float format.  */
extern const struct real_format pdp11_f_format;
extern const struct real_format pdp11_d_format;

/* Maximum sized of reasonable data type 
   DImode or Dfmode ...*/
#define MAX_FIXED_MODE_SIZE 64	

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 16

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 16

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 16

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   we have 8 integer registers, plus 6 float 
   (don't use scratch float !) */

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the pdp, these are:
   Reg 7	= pc;
   reg 6	= sp;
   reg 5	= fp;  not necessarily! 
*/

#define FIXED_REGISTERS  \
{0, 0, 0, 0, 0, 0, 1, 1, \
 0, 0, 0, 0, 0, 0, 1, 1 }



/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* don't know about fp */
#define CALL_USED_REGISTERS  \
{1, 1, 0, 0, 0, 0, 1, 1, \
 0, 0, 0, 0, 0, 0, 1, 1 }


/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register in which static-chain is passed to a function.  */
/* ??? - i don't want to give up a reg for this! */
#define STATIC_CHAIN_REGNUM 4

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
   
/* The pdp has a couple of classes:

MUL_REGS are used for odd numbered regs, to use in 16-bit multiplication
         (even numbered do 32-bit multiply)
LMUL_REGS long multiply registers (even numbered regs )
	  (don't need them, all 32-bit regs are even numbered!)
GENERAL_REGS is all cpu
LOAD_FPU_REGS is the first four cpu regs, they are easier to load
NO_LOAD_FPU_REGS is ac4 and ac5, currently - difficult to load them
FPU_REGS is all fpu regs 
*/

enum reg_class { NO_REGS, MUL_REGS, GENERAL_REGS, LOAD_FPU_REGS, NO_LOAD_FPU_REGS, FPU_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* have to allow this till cmpsi/tstsi are fixed in a better way !! */
#define TARGET_SMALL_REGISTER_CLASSES_FOR_MODE_P hook_bool_mode_true

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

/* #define GENERAL_REGS ALL_REGS */

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES {"NO_REGS", "MUL_REGS", "GENERAL_REGS", "LOAD_FPU_REGS", "NO_LOAD_FPU_REGS", "FPU_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0}, {0x00aa}, {0xc0ff}, {0x0f00}, {0x3000}, {0x3f00}, {0xffff}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) pdp11_regno_reg_class (REGNO)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
((CLASS == GENERAL_REGS || CLASS == MUL_REGS)?				\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD):	\
  1									\
)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.
*/
#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the pdp11, the stack is on an even boundary */
#define PUSH_ROUNDING(BYTES) ((BYTES + 1) & ~1)

/* current_first_parm_offset stores the # of registers pushed on the 
   stack */
extern int current_first_parm_offset;

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define BASE_RETURN_VALUE_REG(MODE) \
 (FLOAT_MODE_P (MODE) ? AC0_REGNUM : RETVAL_REGNUM) 

/* 1 if N is a possible register number for function argument passing.
   - not used on pdp */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

*/

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   ...., the offset normally starts at 0, but starts at 1 word
   when the function gets a structure-value-address as an
   invisible first argument.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
 ((CUM) = 0)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   gcc_unreachable ();

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int may_call_alloca;

#define EXIT_IGNORE_STACK	1

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   There are two registers that can always be eliminated on the pdp11.
   The frame pointer and the arg pointer can be replaced by either the
   hard frame pointer or to the stack pointer, depending upon the
   circumstances.  The hard frame pointer is not used before reload and
   so it is not eligible for elimination.  */

#define ELIMINABLE_REGS					\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM}}	\

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = pdp11_initial_elimination_offset ((FROM), (TO)))


/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1

#define HAVE_PRE_DECREMENT 1

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in reginfo.c during register
   allocation.  */

#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) <= PC_REGNUM || (unsigned) reg_renumber[REGNO] <= PC_REGNUM || \
   (REGNO) == ARG_POINTER_REGNUM || (REGNO) == FRAME_POINTER_REGNUM)

#define REGNO_OK_FOR_INDEX_P(REGNO) REGNO_OK_FOR_BASE_P (REGNO)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.
*/



/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

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
#define REG_OK_FOR_INDEX_P(X) (1)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (1)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define this if a raw index is all that is needed for a
   `tablejump' insn.  */
#define CASE_TAKES_INDEX_RAW

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  
*/

#define MOVE_MAX 2

/* Nonzero if access to memory by byte is slow and undesirable. -
*/
#define SLOW_BYTE_ACCESS 0

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Give a comparison code (EQ, NE etc) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  */

#define SELECT_CC_MODE(OP,X,Y)	\
(GET_MODE_CLASS(GET_MODE(X)) == MODE_FLOAT? CCFPmode : CCmode)

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode HImode

/* A function address in a call instruction
   is a word address (for indexing purposes)
   so give the MEM rtx a word's mode.  */
#define FUNCTION_MODE HImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
/* #define NO_FUNCTION_CSE */


/* Tell emit-rtl.c how to initialize special values on a per-function base.  */
extern rtx cc0_reg_rtx;

#define CC_STATUS_MDEP rtx

#define CC_STATUS_MDEP_INIT (cc_status.mdep = 0)

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

#define CC_IN_FPU 04000 

/* Do UPDATE_CC if EXP is a set, used in
   NOTICE_UPDATE_CC 

   floats only do compare correctly, else nullify ...

   get cc0 out soon ...
*/

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ if (GET_CODE (EXP) == SET)					\
    {								\
      notice_update_cc_on_set(EXP, INSN);			\
    }								\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    {								\
      notice_update_cc_on_set(XVECEXP (EXP, 0, 0), INSN);	\
    }								\
  else if (GET_CODE (EXP) == CALL)				\
    { /* all bets are off */ CC_STATUS_INIT; }			\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2)) \
    { 								\
      printf ("here!\n");					\
      cc_status.value2 = 0;					\
    }								\
}

/* Control the assembler format that we output.  */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text\n"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data\n"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "sp", "pc",     \
 "ac0", "ac1", "ac2", "ac3", "ac4", "ac5", "fp", "ap" }

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

/* The prefix to add to user-visible assembler symbols.  */

#define USER_LABEL_PREFIX "_"

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s_%lu", PREFIX, (unsigned long)(NUM))

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)  \
  output_ascii (FILE, P, SIZE)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t%sL_%d\n", TARGET_UNIX_ASM ? "" : ".word ", VALUE)

/* This is how to output an element of a case-vector that is relative.
   Don't define this if it is not supported.  */

/* #define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) */

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes. 

   who needs this????
*/

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  switch (LOG)				\
    {					\
      case 0:				\
	break;				\
      case 1:				\
	fprintf (FILE, "\t.even\n");	\
	break;				\
      default:				\
	gcc_unreachable ();		\
    }

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.=.+ %#ho\n", (unsigned short)(SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)  \
    pdp11_asm_output_var (FILE, NAME, SIZE, ALIGN, true)


/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN) \
    pdp11_asm_output_var (FILE, NAME, SIZE, ALIGN, false)

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
 print_operand_address (FILE, ADDR)

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)			\
(							\
  fprintf (FILE, "\tmov %s, -(sp)\n", reg_names[REGNO])	\
)

#define ASM_OUTPUT_REG_POP(FILE,REGNO)                 		\
(                                                       	\
  fprintf (FILE, "\tmov (sp)+, %s\n", reg_names[REGNO])     	\
)

#define TRAMPOLINE_SIZE 8
#define TRAMPOLINE_ALIGNMENT 16

/* there is no point in avoiding branches on a pdp, 
   since branches are really cheap - I just want to find out
   how much difference the BRANCH_COST macro makes in code */
#define BRANCH_COST(speed_p, predictable_p) pdp11_branch_cost ()

#define COMPARE_FLAG_MODE HImode

#define TARGET_HAVE_NAMED_SECTIONS false

/* pdp11-unknown-aout target has no support of C99 runtime */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function
