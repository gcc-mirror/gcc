/* Definitions of target machine for GNU compiler, for the pdp-11
   Copyright (C) 1994, 95, 96, 98, 99, 2000 Free Software Foundation, Inc.
   Contributed by Michael K. Gschwind (mike@vlsivie.tuwien.ac.at).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* check whether load_fpu_reg or not */
#define LOAD_FPU_REG_P(x) ((x)>=8 && (x)<=11)
#define NO_LOAD_FPU_REG_P(x) ((x)==12 || (x)==13)
#define FPU_REG_P(x)	(LOAD_FPU_REG_P(x) || NO_LOAD_FPU_REG_P(x))
#define CPU_REG_P(x)	((x)<8)

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dpdp11"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (pdp11)");


/* Generate DBX debugging information.  */

/* #define DBX_DEBUGGING_INFO */

/* Run-time compilation parameters selecting different hardware subsets.
*/

extern int target_flags;

/* Macro to define tables used to set the flags.
   This is a list in braces of triplets in braces,
   each triplet being { "NAME", VALUE, DOC }
   where VALUE is the bits to set or minus the bits to clear and DOC
   is the documentation for --help (NULL if intentionally undocumented).
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
{   { "fpu", 1, "Use hardware floating point" },		\
    { "soft-float", -1, "Do not use hardware floating point" }, \
/* return float result in ac0 */				\
    { "ac0", 2, "Return floating point results in ac0" },	\
    { "no-ac0", -2, "Return floating point results in memory" },\
/* is 11/40 */							\
    { "40", 4, "Generate code for an 11/40" },			\
    { "no-40", -4, "" },					\
/* is 11/45 */							\
    { "45", 8, "Generate code for an 11/45" },			\
    { "no-45", -8, "" },					\
/* is 11/10 */							\
    { "10", -12, "Generate code for an 11/10" },		\
/* use movstrhi for bcopy */					\
    { "bcopy", 16, NULL },					\
    { "bcopy-builtin", -16, NULL },				\
/* use 32 bit for int */					\
    { "int32", 32, "Use 32 bit int" },				\
    { "no-int16", 32, "Use 32 bit int" },			\
    { "int16", -32, "Use 16 bit int" },				\
    { "no-int32", -32, "Use 16 bit int" },			\
/* use 32 bit for float */					\
    { "float32", 64, "Use 32 bit float" },			\
    { "no-float64", 64, "Use 32 bit float" },			\
    { "float64", -64, "Use 64 bit float" },			\
    { "no-float32", -64, "Use 64 bit float" },			\
/* allow abshi pattern? - can trigger "optimizations" which make code SLOW! */\
    { "abshi", 128, NULL },					\
    { "no-abshi", -128, NULL },					\
/* is branching expensive - on a PDP, it's actually really cheap */ \
/* this is just to play around and check what code gcc generates */ \
    { "branch-expensive", 256, NULL }, 				\
    { "branch-cheap", -256, NULL },				\
/* split instruction and data memory? */ 			\
    { "split", 1024, "Target has split I&D" },			\
    { "no-split", -1024, "Target does not have split I&D" },	\
/* UNIX assembler syntax?  */					\
    { "unix-asm", 2048, "Use UNIX assembler syntax" },		\
    { "dec-asm", -2048, "Use DEC assembler syntax" },		\
/* default */			\
    { "", TARGET_DEFAULT, NULL}	\
}

#define TARGET_DEFAULT (1 | 8 | 128 | TARGET_UNIX_ASM_DEFAULT)

#define TARGET_FPU 		(target_flags & 1)
#define TARGET_SOFT_FLOAT 	(!TARGET_FPU)

#define TARGET_AC0		((target_flags & 2) && TARGET_FPU)
#define TARGET_NO_AC0		(! TARGET_AC0)

#define TARGET_45		(target_flags & 8)
#define TARGET_40_PLUS		((target_flags & 4) || (target_flags & 8))
#define TARGET_10		(! TARGET_40_PLUS)

#define TARGET_BCOPY_BUILTIN	(! (target_flags & 16))

#define TARGET_INT16		(! TARGET_INT32)
#define TARGET_INT32		(target_flags & 32)

#define TARGET_FLOAT32		(target_flags & 64)
#define TARGET_FLOAT64		(! TARGET_FLOAT32)

#define TARGET_ABSHI_BUILTIN	(target_flags & 128)

#define TARGET_BRANCH_EXPENSIVE	(target_flags & 256)
#define TARGET_BRANCH_CHEAP 	(!TARGET_BRANCH_EXPENSIVE)

#define TARGET_SPLIT		(target_flags & 1024)
#define TARGET_NOSPLIT		(! TARGET_SPLIT)

#define TARGET_UNIX_ASM		(target_flags & 2048)
#define TARGET_UNIX_ASM_DEFAULT	0

#define ASSEMBLER_DIALECT	(TARGET_UNIX_ASM ? 1 : 0)



/* TYPE SIZES */
#define CHAR_TYPE_SIZE		8
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

/* is used in cexp.y - we don't have target_flags there, 
   so just give default definition 

   hope it does not come back to haunt us! */
#define WCHAR_TYPE "int" 		/* or long int???? */
#define WCHAR_TYPE_SIZE 16

#define PTRDIFF_TYPE "int"

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is numbered.  */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
/*  This is a machine with 16-bit registers */
#define BITS_PER_WORD 16

/* Width of a word, in units (bytes). 

   UNITS OR BYTES - seems like units */
#define UNITS_PER_WORD 2

/* Maximum sized of reasonable data type 
   DImode or Dfmode ...*/
#define MAX_FIXED_MODE_SIZE 64	

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 16

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 16

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 16

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

#define FIRST_PSEUDO_REGISTER 14

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the pdp, these are:
   Reg 7	= pc;
   reg 6	= sp;
   reg 5	= fp;  not necessarily! 
*/

/* don't let them touch fp regs for the time being !*/

#define FIXED_REGISTERS  \
{0, 0, 0, 0, 0, 0, 1, 1, \
 0, 0, 0, 0, 0, 0     }



/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

/* don't know about fp */
#define CALL_USED_REGISTERS  \
{1, 1, 0, 0, 0, 0, 1, 1, \
 0, 0, 0, 0, 0, 0 }


/* Make sure everything's fine if we *don't* have an FPU.
   This assumes that putting a register in fixed_regs will keep the
   compiler's mitts completely off it.  We don't bother to zero it out
   of register classes.  Also fix incompatible register naming with
   the UNIX assembler.
*/
#define CONDITIONAL_REGISTER_USAGE \
{ 						\
  int i; 					\
  HARD_REG_SET x; 				\
  if (!TARGET_FPU)				\
    { 						\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FPU_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ ) \
       if (TEST_HARD_REG_BIT (x, i)) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    } 						\
						\
  if (TARGET_AC0)				\
      call_used_regs[8] = 1;			\
  if (TARGET_UNIX_ASM)				\
    {						\
      /* Change names of FPU registers for the UNIX assembler.  */ \
      reg_names[8] = "fr0";			\
      reg_names[9] = "fr1";			\
      reg_names[10] = "fr2";			\
      reg_names[11] = "fr3";			\
      reg_names[12] = "fr4";			\
      reg_names[13] = "fr5";			\
    }						\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
*/

#define HARD_REGNO_NREGS(REGNO, MODE)   \
((REGNO < 8)?								\
    ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)	\
    :1)
    

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the pdp, the cpu registers can hold any mode - check alignment

   FPU can only hold DF - simplifies life!
*/
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
(((REGNO) < 8)?						\
  ((GET_MODE_BITSIZE(MODE) <= 16) 			\
   || (GET_MODE_BITSIZE(MODE) == 32 && !((REGNO) & 1)))	\
  :(MODE) == DFmode)
    

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) 0

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* the pdp11 pc overloaded on a register that the compiler knows about.  */
#define PC_REGNUM  7

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 6

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 5

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.
  */

#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 5

/* Register in which static-chain is passed to a function.  */
/* ??? - i don't want to give up a reg for this! */
#define STATIC_CHAIN_REGNUM 4

/* Register in which address to store a structure value
   is passed to a function.  
   let's make it an invisible first argument!!! */

#define STRUCT_VALUE 0


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

MUL_REGS are used for odd numbered regs, to use in 16 bit multiplication
         (even numbered do 32 bit multiply)
LMUL_REGS long multiply registers (even numbered regs )
	  (don't need them, all 32 bit regs are even numbered!)
GENERAL_REGS is all cpu
LOAD_FPU_REGS is the first four cpu regs, they are easier to load
NO_LOAD_FPU_REGS is ac4 and ac5, currently - difficult to load them
FPU_REGS is all fpu regs 
*/

enum reg_class { NO_REGS, MUL_REGS, GENERAL_REGS, LOAD_FPU_REGS, NO_LOAD_FPU_REGS, FPU_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* have to allow this till cmpsi/tstsi are fixed in a better way !! */
#define SMALL_REGISTER_CLASSES 1

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

/* #define GENERAL_REGS ALL_REGS */

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES {"NO_REGS", "MUL_REGS", "GENERAL_REGS", "LOAD_FPU_REGS", "NO_LOAD_FPU_REGS", "FPU_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0}, {0x00aa}, {0x00ff}, {0x0f00}, {0x3000}, {0x3f00}, {0x3fff}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) 		\
((REGNO)>=8?((REGNO)<=11?LOAD_FPU_REGS:NO_LOAD_FPU_REGS):(((REGNO)&1)?MUL_REGS:GENERAL_REGS))


/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)	\
((C) == 'f' ? FPU_REGS :			\
  ((C) == 'd' ? MUL_REGS : 			\
   ((C) == 'a' ? LOAD_FPU_REGS : NO_REGS)))
    

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   I		bits 31-16 0000
   J		bits 15-00 0000
   K		completely random 32 bit
   L,M,N	-1,1,0 respectively
   O 		where doing shifts in sequence is faster than 
                one big shift 
*/

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? ((VALUE) & 0xffff0000) == 0		\
   : (C) == 'J' ? ((VALUE) & 0x0000ffff) == 0  	       	\
   : (C) == 'K' ? (((VALUE) & 0xffff0000) != 0		\
		   && ((VALUE) & 0x0000ffff) != 0)	\
   : (C) == 'L' ? ((VALUE) == 1)			\
   : (C) == 'M' ? ((VALUE) == -1)			\
   : (C) == 'N' ? ((VALUE) == 0)			\
   : (C) == 'O' ? (abs(VALUE) >1 && abs(VALUE) <= 4)		\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' && XINT (VALUE, 0) == 0 && XINT (VALUE, 1) == 0)


/* Letters in the range `Q' through `U' may be defined in a
   machine-dependent fashion to stand for arbitrary operand types. 
   The machine description macro `EXTRA_CONSTRAINT' is passed the
   operand as its first argument and the constraint letter as its
   second operand.

   `Q'	is for memory references using take more than 1 instruction.
   `R'	is for memory references which take 1 word for the instruction.  */

#define EXTRA_CONSTRAINT(OP,CODE)					\
  ((GET_CODE (OP) != MEM) ? 0						\
   : !legitimate_address_p (GET_MODE (OP), XEXP (OP, 0)) ? 0		\
   : ((CODE) == 'Q')	  ? !simple_memory_operand (OP, GET_MODE (OP))	\
   : ((CODE) == 'R')	  ? simple_memory_operand (OP, GET_MODE (OP))	\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  

loading is easier into LOAD_FPU_REGS than FPU_REGS! */

#define PREFERRED_RELOAD_CLASS(X,CLASS) 	\
(((CLASS) != FPU_REGS)?(CLASS):LOAD_FPU_REGS)

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,x)	\
(((CLASS) == NO_LOAD_FPU_REGS && !(REG_P(x) && LOAD_FPU_REG_P(REGNO(x))))?LOAD_FPU_REGS:NO_REGS)

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
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.
*/
#define FRAME_GROWS_DOWNWARD

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

/* Offset of first parameter from the argument pointer register value.  
   For the pdp11, this is non-zero to account for the return address.
	1 - return address
	2 - frame pointer (always saved, even when not used!!!!)
		-- chnage some day !!!:q!

*/
#define FIRST_PARM_OFFSET(FNDECL) 4

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define BASE_RETURN_VALUE_REG(MODE) \
 ((MODE) == DFmode ? 8 : 0) 

/* On the pdp11 the value is found in R0 (or ac0??? 
not without FPU!!!! ) */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), BASE_RETURN_VALUE_REG(TYPE_MODE(VALTYPE)))

/* and the called function leaves it in the first register.
   Difference only on machines with register windows.  */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)  \
  gen_rtx_REG (TYPE_MODE (VALTYPE), BASE_RETURN_VALUE_REG(TYPE_MODE(VALTYPE)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG (MODE, BASE_RETURN_VALUE_REG(MODE))

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On the pdp, the first "output" reg is the only register thus used. 

maybe ac0 ? - as option someday! */

#define FUNCTION_VALUE_REGNO_P(N) (((N) == 0) || (TARGET_AC0 && (N) == 8))

/* should probably return DImode and DFmode in memory,lest
   we fill up all regs!

 have to, else we crash - exception: maybe return result in 
 ac0 if DFmode and FPU present - compatibility problem with
 libraries for non-floating point ...
*/

#define RETURN_IN_MEMORY(TYPE)	\
  (TYPE_MODE(TYPE) == DImode || (TYPE_MODE(TYPE) == DFmode && ! TARGET_AC0))


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

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
 ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  

*/


#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE))		\
	    : (int_size_in_bytes (TYPE))))	

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)  0

/* Define where a function finds its arguments.
   This would be different from FUNCTION_ARG if we had register windows.  */
/*
#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)	\
  FUNCTION_ARG (CUM, MODE, TYPE, NAMED)
*/

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* This macro generates the assembly code for function entry. */
#define FUNCTION_PROLOGUE(FILE, SIZE) \
    output_function_prologue(FILE, SIZE);

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   abort ();

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int may_call_alloca;

#define EXIT_IGNORE_STACK	1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.
*/

#define FUNCTION_EPILOGUE(FILE, SIZE) \
    output_function_epilogue(FILE, SIZE);
  
#define INITIAL_FRAME_POINTER_OFFSET(DEPTH_VAR)	\
{								\
  int offset, regno;		      				\
  offset = get_frame_size();					\
  for (regno = 0; regno < 8; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += 2;						\
  for (regno = 8; regno < 14; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += 8;						\
  /* offset -= 2;   no fp on stack frame */			\
  (DEPTH_VAR) = offset;						\
}   
    

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT 1
/* #define HAVE_POST_DECREMENT 0 */

#define HAVE_PRE_DECREMENT 1
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) < 8 || (unsigned) reg_renumber[REGNO] < 8)
#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) < 8 || (unsigned) reg_renumber[REGNO] < 8)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.
*/



/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) (1)

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

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

*/

#define GO_IF_LEGITIMATE_ADDRESS(mode, operand, ADDR) \
{						      \
    rtx xfoob;								\
									\
    /* accept (R0) */							\
    if (GET_CODE (operand) == REG					\
	&& REG_OK_FOR_BASE_P(operand))					\
      goto ADDR;							\
									\
    /* accept @#address */						\
    if (CONSTANT_ADDRESS_P (operand))					\
      goto ADDR;							\
    									\
    /* accept X(R0) */							\
    if (GET_CODE (operand) == PLUS       				\
	&& GET_CODE (XEXP (operand, 0)) == REG				\
	&& REG_OK_FOR_BASE_P (XEXP (operand, 0))			\
	&& CONSTANT_ADDRESS_P (XEXP (operand, 1)))			\
      goto ADDR;							\
    									\
    /* accept -(R0) */							\
    if (GET_CODE (operand) == PRE_DEC					\
	&& GET_CODE (XEXP (operand, 0)) == REG				\
	&& REG_OK_FOR_BASE_P (XEXP (operand, 0)))			\
      goto ADDR;							\
									\
    /* accept (R0)+ */							\
    if (GET_CODE (operand) == POST_INC					\
	&& GET_CODE (XEXP (operand, 0)) == REG				\
	&& REG_OK_FOR_BASE_P (XEXP (operand, 0)))			\
      goto ADDR;							\
    									\
    /* handle another level of indirection ! */				\
    if (GET_CODE(operand) != MEM)					\
      goto fail;							\
									\
    xfoob = XEXP (operand, 0);						\
									\
    /* (MEM:xx (MEM:xx ())) is not valid for SI, DI and currently */    \
    /* also forbidden for float, because we have to handle this */  	\
    /* in output_move_double and/or output_move_quad() - we could */   	\
    /* do it, but currently it's not worth it!!! */			\
    /* now that DFmode cannot go into CPU register file, */		\
    /* maybe I should allow float ... */				\
    /*  but then I have to handle memory-to-memory moves in movdf ?? */ \
									\
    if (GET_MODE_BITSIZE(mode) > 16)					\
      goto fail;							\
									\
    /* accept @(R0) - which is @0(R0) */				\
    if (GET_CODE (xfoob) == REG						\
	&& REG_OK_FOR_BASE_P(xfoob))					\
      goto ADDR;							\
									\
    /* accept @address */						\
    if (CONSTANT_ADDRESS_P (xfoob))					\
      goto ADDR;							\
    									\
    /* accept @X(R0) */							\
    if (GET_CODE (xfoob) == PLUS       					\
	&& GET_CODE (XEXP (xfoob, 0)) == REG				\
	&& REG_OK_FOR_BASE_P (XEXP (xfoob, 0))				\
	&& CONSTANT_ADDRESS_P (XEXP (xfoob, 1)))			\
      goto ADDR;							\
									\
    /* accept @-(R0) */							\
    if (GET_CODE (xfoob) == PRE_DEC					\
	&& GET_CODE (XEXP (xfoob, 0)) == REG				\
	&& REG_OK_FOR_BASE_P (XEXP (xfoob, 0)))				\
      goto ADDR;							\
									\
    /* accept @(R0)+ */							\
    if (GET_CODE (xfoob) == POST_INC					\
	&& GET_CODE (XEXP (xfoob, 0)) == REG				\
	&& REG_OK_FOR_BASE_P (XEXP (xfoob, 0)))				\
      goto ADDR;							\
									\
  /* anything else is invalid */					\
  fail: ;								\
}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	{}


/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the pdp this is for predec/postinc */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 { if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC)	\
     goto LABEL; 							\
 }


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define this if a raw index is all that is needed for a
   `tablejump' insn.  */
#define CASE_TAKES_INDEX_RAW

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  
*/

#define MOVE_MAX 2

/* Zero extension is faster if the target is known to be zero */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by byte is slow and undesirable. -
*/
#define SLOW_BYTE_ACCESS 0

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1


/* Add any extra modes needed to represent the condition code.

   CCFPmode is used for FPU, but should we use a separate reg? */
#define EXTRA_CC_MODES CC(CCFPmode, "CCFP")

/* Give a comparison code (EQ, NE etc) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used. */

#define SELECT_CC_MODE(OP,X,Y)	\
(GET_MODE_CLASS(GET_MODE(X)) == MODE_FLOAT? CCFPmode : CCmode)

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

/* #define STORE_FLAG_VALUE 1 */

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

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch. 

   -1, 0, 1 are cheaper for add, sub ... 
*/

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL(RTX) == 0					\
	|| INTVAL(RTX) == -1					\
	|| INTVAL(RTX) == 1)					\
      return 0;							\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    /* twice as expensive as REG */				\
    return 2;							\
  case CONST_DOUBLE:						\
    /* twice (or 4 times) as expensive as 16 bit */		\
    return 4;

/* cost of moving one register class to another */
#define REGISTER_MOVE_COST(CLASS1, CLASS2) register_move_cost(CLASS1, CLASS2)

/* Tell emit-rtl.c how to initialize special values on a per-function base.  */
extern int optimize;
extern struct rtx_def *cc0_reg_rtx;

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

/* Output at beginning of assembler file.  */

#if 0
#define ASM_FILE_START(FILE) \
(								\
fprintf (FILE, "\t.data\n"),					\
fprintf (FILE, "$help$: . = .+8 ; space for tmp moves!\n")	\
/* do we need reg def's R0 = %0 etc ??? */			\
)
#else
#define ASM_FILE_START(FILE)
#endif


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
 "ac0", "ac1", "ac2", "ac3", "ac4", "ac5" }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.globl ", FILE); assemble_name (FILE, NAME); fputs("\n", FILE); } while (0)

/* The prefix to add to user-visible assembler symbols. */

#define USER_LABEL_PREFIX "_"

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s_%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s_%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\tdouble %.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\tfloat %.12e\n", (VALUE))

/* Likewise for `short' and `char' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, TARGET_UNIX_ASM ? "\t" : "\t.word "),	\
  output_addr_const_pdp11 (FILE, (VALUE)),		\
  fprintf (FILE, " /*short*/\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const_pdp11 (FILE, (VALUE)),		\
  fprintf (FILE, " /* char */\n"))

/* This is how to output an assembler line for a numeric constant byte.
   This won't actually be used since we define ASM_OUTPUT_CHAR.
*/

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte %o\n", (VALUE))

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)  \
  output_ascii (FILE, P, SIZE)

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t%sL_%d\n", TARGET_UNIX_ASM ? "" : ".word ", VALUE)

/* This is how to output an element of a case-vector that is relative.
   Don't define this if it is not supported. */

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
	abort ();			\
    }

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.=.+ %o\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fprintf ((FILE), ".globl "),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), "\n"),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ": .=.+ %o\n", (ROUNDED))		\
)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( assemble_name ((FILE), (NAME)),				\
  fprintf ((FILE), ":\t.=.+ %o\n", (ROUNDED)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "["
#define ASM_CLOSE_PAREN "]"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

*/


#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (CODE == '#') fprintf (FILE, "#");					\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != SImode)	\
    { union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      fprintf (FILE, "#%.20e", u.d); }					\
  else { putc ('$', FILE); output_addr_const_pdp11 (FILE, X); }}

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


#define ASM_IDENTIFY_GCC(FILE)			\
    fprintf(FILE, "gcc_compiled:\n")

/* trampoline - how should i do it in separate i+d ? 
   have some allocate_trampoline magic??? 

   the following should work for shared I/D: */

/* lets see whether this works as trampoline:
MV	#STATIC, $4	0x940Y	0x0000 <- STATIC; Y = STATIC_CHAIN_REGNUM
JMP	FUNCTION	0x0058  0x0000 <- FUNCTION
*/

#define TRAMPOLINE_TEMPLATE(FILE)	\
{					\
  if (TARGET_SPLIT)			\
    abort();				\
					\
  ASM_OUTPUT_SHORT (FILE, GEN_INT (0x9400+STATIC_CHAIN_REGNUM)); \
  ASM_OUTPUT_SHORT (FILE, const0_rtx);				\
  ASM_OUTPUT_SHORT (FILE, GEN_INT(0x0058));			\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);				\
}

#define TRAMPOLINE_SIZE 8
#define TRAMPOLINE_ALIGN 16

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP,FNADDR,CXT)	\
{					\
  if (TARGET_SPLIT)			\
    abort();				\
					\
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant (TRAMP, 2)), CXT); \
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant (TRAMP, 6)), FNADDR); \
}


/* Some machines may desire to change what optimizations are
   performed for various optimization levels.   This macro, if
   defined, is executed once just after the optimization level is
   determined and before the remainder of the command options have
   been parsed.  Values set in this macro are used as the default
   values for the other command line options.

   LEVEL is the optimization level specified; 2 if -O2 is
   specified, 1 if -O is specified, and 0 if neither is specified.  */

#define OPTIMIZATION_OPTIONS(LEVEL,SIZE)				\
{									\
  if (LEVEL >= 3)							\
    {									\
      if (! SIZE)							\
        flag_inline_functions		= 1;				\
      flag_omit_frame_pointer		= 1;				\
      /* flag_unroll_loops			= 1; */			\
    }									\
}


/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. 

   we don't say how expensive SImode is - pretty expensive!!!

   there is something wrong in MULT because MULT is not 
   as cheap as total = 2 even if we can shift!

   if optimizing for size make mult etc cheap, but not 1, so when 
   in doubt the faster insn is chosen.
*/

#define RTX_COSTS(X,CODE,OUTER_CODE) \
  case MULT:								\
    if (optimize_size)							\
      total = COSTS_N_INSNS(2);						\
    else								\
      total = COSTS_N_INSNS (11);					\
    break;								\
  case DIV:								\
    if (optimize_size)							\
      total = COSTS_N_INSNS(2);						\
    else								\
      total = COSTS_N_INSNS (25);					\
    break;								\
  case MOD:								\
    if (optimize_size)							\
      total = COSTS_N_INSNS(2);						\
    else								\
      total = COSTS_N_INSNS (26);					\
    break;								\
  case ABS:								\
    /* equivalent to length, so same for optimize_size */		\
    total = COSTS_N_INSNS (3);						\
    break;								\
  case ZERO_EXTEND:							\
    /* only used for: qi->hi */						\
    total = COSTS_N_INSNS(1);						\
    break;								\
  case SIGN_EXTEND:							\
    if (GET_MODE(X) == HImode)						\
      	total = COSTS_N_INSNS(1);					\
    else if (GET_MODE(X) == SImode)					\
	total = COSTS_N_INSNS(6);					\
    else								\
	total = COSTS_N_INSNS(2);					\
    break;								\
  /* case LSHIFT: */		       					\
  case ASHIFT:								\
  case LSHIFTRT:							\
  case ASHIFTRT:							\
    if (optimize_size)							\
      total = COSTS_N_INSNS(1);						\
    else if (GET_MODE(X) ==  QImode)					\
    {									\
      if (GET_CODE(XEXP (X,1)) != CONST_INT)				\
   	total = COSTS_N_INSNS(8); /* worst case */ 			\
      else                                                              \
	total = COSTS_N_INSNS(INTVAL(XEXP (X,1)));			\
    }									\
    else if (GET_MODE(X) == HImode)					\
    {									\
      if (GET_CODE(XEXP (X,1)) == CONST_INT)				\
      {									\
	if (abs (INTVAL (XEXP (X, 1))) == 1)				\
          total = COSTS_N_INSNS(1);					\
        else								\
	  total = COSTS_N_INSNS(2.5 + 0.5 *INTVAL(XEXP(X,1)));		\
      }									\
      else /* worst case */						\
        total = COSTS_N_INSNS (10);					\
    }									\
    else if (GET_MODE(X) == SImode)					\
    {									\
      if (GET_CODE(XEXP (X,1)) == CONST_INT)				\
	  total = COSTS_N_INSNS(2.5 + 0.5 *INTVAL(XEXP(X,1)));		\
      else /* worst case */						\
        total = COSTS_N_INSNS(18);					\
    }									\
    break;


/* there is no point in avoiding branches on a pdp, 
   since branches are really cheap - I just want to find out
   how much difference the BRANCH_COST macro makes in code */
#define BRANCH_COST (TARGET_BRANCH_CHEAP ? 0 : 1)


#define COMPARE_FLAG_MODE HImode

