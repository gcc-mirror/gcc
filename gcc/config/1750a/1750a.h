/* Definitions of target machine for GNU compiler.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by O.M.Kellogg, DASA (okellogg@salyko.cube.net).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Names to predefine in the preprocessor for this target machine.  */

/* See tm-sun3.h, tm-sun2.h, tm-isi68.h for different CPP_PREDEFINES.  */
#define CPP_PREDEFINES ""

/* Print subsidiary information on the compiler version in use.  */
#ifdef IEEE
#define TARGET_VERSION fprintf (stderr, " (1750A, IEEE syntax)");
#else
#define TARGET_VERSION fprintf (stderr, " (MIL-STD-1750A)");
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

#define TARGET_SWITCHES  \
  { {"vaxc-alignment", 2}, \
    { "", TARGET_DEFAULT}}

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 1
#endif

/*****************************************************************************/

/* SPECIAL ADDITION FOR MIL-STD-1750A     by O.M.Kellogg, 15-Apr-1993 */
/* See file aux-output.c for the actual data instances. */
struct datalabel_array {
    char *name;
    char value[14];
    int size;
};
struct jumplabel_array {
    int pc;
    int num;
};
enum section { Init, Normal, Konst, Static };
#define DATALBL_ARRSIZ 256
#define JMPLBL_ARRSIZ  256
#ifndef __datalbl
extern struct datalabel_array datalbl[];
extern struct jumplabel_array jmplbl[];
extern int datalbl_ndx, jmplbl_ndx, label_pending, program_counter;
extern enum section current_section;
extern char *sectname[4];
extern char *strdup(), *float_label();
#endif
/*--------------------------------------------------------------------*/

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   Though 1750 actually counts bits in big-endian fashion, the sign bit
   is still the most significant bit, which is leftmost. Therefore leaving
   this little-endian. Adjust short before assembler output when needed:
   e.g. in QImode, a GCC bit n is a 1750 bit (15-n). */
#define BITS_BIG_ENDIAN 0 

/* Define this if most significant byte of a word is the lowest numbered.  */
/* For 1750 we can decide arbitrarily
   since there are no machine instructions for them.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword value is lowest
   numbered.
   True for 1750. */
#define WORDS_BIG_ENDIAN 1

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT        16

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD        16

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD       1

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE         16

#define PTRDIFF_TYPE        "int"

/* Type to use for `size_t'. If undefined, uses `long unsigned int'. */
#define SIZE_TYPE           "int"

/* 1750a preliminary
   #define TARGET_FLOAT_FORMAT UNKNOWN_FLOAT_FORMAT
*/

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY     16

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
/* 1750: should have had to make this 32 when BITS_PER_WORD is 32. */
#define PARM_BOUNDARY        16

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY       16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY    16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT    16

/* Define this to 1 if move instructions will actually fail to work
   when given unaligned data. */
#define STRICT_ALIGNMENT 0

/* Define number of bits in most basic integer type.
   (If undefined, default is BITS_PER_WORD).
   #define INT_TYPE_SIZE  16  */

/* Define number of bits in short integer type.
   (If undefined, default is half of BITS_PER_WORD). */
#define SHORT_TYPE_SIZE 16

/* Define number of bits in long integer type.
   (If undefined, default is BITS_PER_WORD). */
#define LONG_TYPE_SIZE  32

/* Define number of bits in long long integer type.
   (If undefined, default is twice BITS_PER_WORD). */
/* 1750 PRELIMINARY : no processor support for `long long', therefore
        need to check out the long-long opencodings ! */
#define LONG_LONG_TYPE_SIZE  64

/* Define number of bits in char type.
   (If undefined, default is one fourth of BITS_PER_WORD). */
#define CHAR_TYPE_SIZE  16

/* Define number of bits in float type.
   (If undefined, default is BITS_PER_WORD). */
#define FLOAT_TYPE_SIZE  32

/* Define number of bits in double type.
   (If undefined, default is twice BITS_PER_WORD). */
#define DOUBLE_TYPE_SIZE  48

/*****************************************************************************/

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers. */
#define FIRST_PSEUDO_REGISTER 16

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   R15 is the 1750A stack pointer. R14 is the frame pointer. */

#define FIXED_REGISTERS  \
 { 0, 0, 0, 0, 0, 0, 0, 0, \
   0, 0, 0, 0, 0, 0, 1, 1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.
   1750: return value in R0 foll. (depending on size of retval).
   Should be possible to refine this (how many regs are actually used) */

#define CALL_USED_REGISTERS \
 { 1, 1, 1, 1, 1, 1, 1, 1, \
   1, 1, 1, 1, 1, 1, 1, 1 }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   All 1750 registers are one word long. */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output. */
#define MODES_TIEABLE_P(MODE1, MODE2)	1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* 1750A pc isn't overloaded on a register.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 14

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c. */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 14

/* Define this if successive args to a function occupy decreasing addresses
   on the stack. 
   #define ARGS_GROW_DOWNWARD
*/

/* Register in which static-chain is passed to a function. */
#define STATIC_CHAIN_REGNUM 13

/* Register in which address to store a structure value
   is passed to a function. */
#define STRUCT_VALUE_REGNUM 12

/* Define this to be 1 if all structure return values must be in memory. */
#define DEFAUT_PCC_STRUCT_RETURN 0

/*****************************************************************************/

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

/* 1750 note: The names (BASE_REGS/INDEX_REGS) are used in their *gcc sense*
   (i.e. *opposite* to the MIL-STD-1750A defined meanings). This means that
   R1..R15 are called "base" regs and R12..R15 are "index" regs.
   Index reg mode (in the gcc sense) is not yet implemented (these are the
   1750 "Base with Index Reg" instructions, LBX etc. See 1750.md)

   Here's an example to drive this point home: in "LBX B12,R5"
   B12 shall be called the "index" reg and R5 shall be the "base" reg.
   This naming inversion is due to the GCC defined capabilities of
   "Base" vs. "Index" regs. */

enum reg_class { NO_REGS, INDEX_REGS, BASE_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias. */
#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 { "NO_REGS", "INDEX_REGS", "BASE_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.
   1750 "index" (remember, in the *GCC* sense!) regs are R12 through R15. 
   The only 1750 register not usable as BASE_REG is R0. */

#define REG_CLASS_CONTENTS  {0, 0xf000, 0xfffe, 0xffff}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
#define REGNO_REG_CLASS(REGNO)	\
 ((REGNO) >= 12 ? INDEX_REGS : (REGNO) >  0 ? BASE_REGS : ALL_REGS)

/* The class value for index registers, and the one for base regs. */

#define BASE_REG_CLASS  BASE_REGS
#define INDEX_REG_CLASS INDEX_REGS

/* Get reg_class from a letter such as appears in the machine description.
   For the 1750, we have 'b' for gcc Base regs and 'x' for gcc Index regs. */

#define REG_CLASS_FROM_LETTER(C) ((C) == 'b' ? BASE_REGS : \
				  (C) == 'x' ? INDEX_REGS : NO_REGS)

/* The letters I,J,K,.. to P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the 1750A, 
   `I' is used for ISP mode instructions,
   `J' is used for ISN mode instructions,
   `K' is used for the STC instruction's constant range,
   `L' is used for unsigned 8-bit address displacements in instructions
       of addressing mode "Base Relative",
   `M' is for IM mode instructions et al.,
   `O' is a synonym for (const_int 0). */

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'I' ? (VALUE) > 0 && (VALUE) <=  16 :			\
   (C) == 'J' ? (VALUE) < 0 && (VALUE) >= -16 :			\
   (C) == 'K' ? (VALUE) >= 0 && (VALUE) <= 15 :			\
   (C) == 'L' ? (VALUE) >= 0 && (VALUE) <= 0xFF :		\
   (C) == 'M' ? (VALUE) >= -0x8000 && (VALUE) <= 0x7FFF : 	\
   (C) == 'O' ? (VALUE) == 0 :				0)

/* Similar, but for floating constants, and defining letter 'G'.
   Here VALUE is the CONST_DOUBLE rtx itself.  */
#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  0

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   For the 1750A, we force an immediate CONST_DOUBLE value to memory. */
#define PREFERRED_RELOAD_CLASS(X,CLASS)  \
		(GET_CODE(X) == CONST_DOUBLE ? NO_REGS : CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.
   On the 1750A, this is the size of MODE in words,
   since class doesn't make any difference. */
#define CLASS_MAX_NREGS(CLASS,MODE)  GET_MODE_SIZE(MODE)

/*****************************************************************************/

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   goes at a more negative offset in the frame. 
   #define FRAME_GROWS_DOWNWARD
*/

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.
*/
#define STARTING_FRAME_OFFSET 1

/* This is the default anyway:
   #define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) FRAMEADDR
*/

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   1750 note: what GCC calls a "byte" is really a 16-bit word,
   because BITS_PER_UNIT is 16. */

#define PUSH_ROUNDING(BYTES) (BYTES)

/* Define this macro if functions should assume that stack space has
   been allocated for arguments even when their values are passed in
   registers.
   Size, in bytes, of the area reserved for arguments passed in
   registers for the function represented by FNDECL. 
   #define REG_PARM_STACK_SPACE(FNDECL) 14 */

/* Define this if it is the responsibility of the caller to allocate
   the area reserved for arguments passed in registers. 
   #define OUTGOING_REG_PARM_STACK_SPACE */

/* Offset of first parameter from the argument pointer register value.
   1750 note:
   Parameters appear in reversed order on the frame (so when they are
   popped, they come off in the normal left-to-right order.)
   Computed as follows:
   one word for the caller's (PC+1) (i.e. the return address)
   plus total size of called function's "auto" variables
   plus one word for the caller's frame pointer (i.e. the old FP) */

#define FIRST_PARM_OFFSET(FNDECL) \
   (1 + get_frame_size() + 1)

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
*/

#define RETURN_POPS_ARGS(FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0. */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx(REG,TYPE_MODE(VALTYPE),0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE. */
/* 1750 note: no libcalls yet */

#define LIBCALL_VALUE(MODE)  printf("LIBCALL_VALUE called!\n"), \
  gen_rtx(REG,MODE,0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N)  ((N) == 0)

/* 1 if the tree TYPE should be returned in memory instead of in regs. 
   #define RETURN_IN_MEMORY(TYPE) \
   (int_size_in_bytes(TYPE) > 12)
*/

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values. 
   #define PCC_STATIC_STRUCT_RETURN  */

/* 1 if N is a possible register number for function argument passing. */

#define FUNCTION_ARG_REGNO_P(N)  ((N) < 12)

/*****************************************************************************/

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   For 1750A, this is a single integer, which is a number of words
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   For 1750A, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)   ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)

   1750 note: "int_size_in_bytes()" returns a unit relative to
   BITS_PER_UNIT, so in our case not bytes, but 16-bit words.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += (MODE) == BLKmode ? int_size_in_bytes(TYPE) : GET_MODE_SIZE(MODE))

/* Define where to put the arguments to a function.
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

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
	(rtx) function_arg(CUM,MODE,TYPE,NAMED)
/*
 (! MUST_PASS_IN_STACK(MODE,TYPE) &&				\
  14 >= (CUM) +							\
  ((MODE)==BLKmode ? int_size_in_bytes(TYPE) : GET_MODE_SIZE (MODE))  \
 ? gen_rtx (REG, MODE, CUM)					\
 : 0)
*/

/* Define the following macro if function calls on the target machine
   do not preserve any registers; in other words, if `CALL_USED_REGISTERS'
   has 1 for all registers. This macro enables `-fcaller-saves' by
   default. Eventually that option will be nabled by default on all
   machines and both the option and this macro will be eliminated. */

#define DEFAULT_CALLER_SAVES


/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */


#define FUNCTION_PROLOGUE(FILE, SIZE) {   \
  register int regno, none_used=1;				\
  extern char call_used_regs[];					\
  fprintf(FILE, "; regs used in this function: ");		\
  for (regno = 0; regno < 15; regno++)				\
    if (regs_ever_live[regno]) {				\
	fprintf(FILE," %s",reg_names[regno]);			\
	none_used = 0;						\
    }								\
  if (none_used)						\
    fprintf(FILE," (none)");				 	\
  fprintf(FILE,"\n");					 	\
  if (SIZE > 0)							\
    fprintf(FILE,"\t%s\tr15,%d  ; reserve local-variable space\n",\
			 (SIZE <= 16 ? "sisp" : "sim"),SIZE);	\
  fprintf(FILE,"\tpshm\tr14,r14 ; push old frame\n");		\
  fprintf(FILE,"\tlr\tr14,r15 ; set new frame\n");		\
  program_counter = 0; jmplbl_ndx = -1;				\
}

/************* 1750: PROFILER HANDLING NOT YET DONE !!!!!!! *************/
/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "; got into FUNCTION_PROFILER with label # %d\n", (LABELNO))

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */
#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "; got into FUNCTION_BLOCK_PROFILER with label # %d\n",LABELNO)

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */
#define BLOCK_PROFILER(FILE, BLOCKNO)	\
  fprintf (FILE, "; got into BLOCK_PROFILER with block # %d\n",BLOCKNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning. */

#define FUNCTION_EPILOGUE(FILE, SIZE) {			\
  if (SIZE > 0)							\
    fprintf(FILE,"\t%s\tr14,%d ; free up local-var space\n",	\
			 (SIZE <= 16 ? "aisp" : "aim"),SIZE);	\
  fprintf(FILE,"\tlr\tr15,r14 ; set stack to return addr\n");	\
  fprintf(FILE,"\tpopm\tr14,r14 ; restore prev. frame ptr\n");	\
  fprintf(FILE,"\turs\tr15\n"); }

/* If the memory address ADDR is relative to the frame pointer,
   correct it to be relative to the stack pointer instead.
   This is for when we don't use a frame pointer.
   ADDR should be a variable name. */

#define FIX_FRAME_POINTER_ADDRESS(ADDR,DEPTH)  \
   fprintf(stderr,"FIX_FRAME_POINTER_ADDRESS called, DEPTH=%d\n"), \
           DEPTH), abort()

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.
*/
#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) DEPTH = 0

/* 1750: not needed 'cause we have INITIAL_FRAME_POINTER_OFFSET.
   #define ELIMINABLE_REGS { \
	{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },  \
	{ ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM },  \
	{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM } }

   #define CAN_ELIMINATE(FROM, TO)   1

   #define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) { OFFSET = 0; }
*/


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

#define TRAMPOLINE_TEMPLATE(FILE)  fprintf(FILE,"TRAMPOLINE_TEMPLATE called\n")

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 2

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)  printf("INITIALIZE_TRAMPO called\n")
/* {									\
  emit_move_insn (gen_rtx (MEM, QImode, plus_constant (TRAMP, 1)), CXT); \
  emit_move_insn (gen_rtx (MEM, QImode, plus_constant (TRAMP, 6)), FNADDR); \
} */


/*****************************************************************************/

/* Addressing modes, and classification of registers for them.  */

/* 1750 doesn't have a lot of auto-incr./decr. - just for the stack ptr. */

/* #define HAVE_POST_INCREMENT  just for R15 (stack pointer) */
/* #define HAVE_POST_DECREMENT */
/* #define HAVE_PRE_DECREMENT   just for R15 (stack pointer) */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c. 
   1750 note: The words BASE and INDEX are used in their GCC senses:
   The "Index Registers", R12 through R15, can have an address displacement
   int the range 0..255 words.
   */

#define REGNO_OK_FOR_BASE_P(REGNO)  \
 ((REGNO) > 0 && (REGNO) <= 15 ||   \
  reg_renumber[REGNO] > 0 && reg_renumber[REGNO] < 15)
#define REGNO_OK_FOR_INDEX_P(REGNO) \
 ((REGNO) >= 12 && (REGNO) <= 15 || \
  reg_renumber[REGNO] >= 12 && reg_renumber[REGNO] <= 15)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

/* 1 if X is an address register  */

#define ADDRESS_REG_P(X) (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */
#define MAX_REGS_PER_ADDRESS 1

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)  CONSTANT_P(X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

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

#ifdef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#else

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) (REGNO (X) >= 12)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (REGNO (X) > 0)

#endif


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.
   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS.

   1750 note: Currently we don't implement address expressions that use
   GCC "Index"-class regs. To be expanded to handle the 1750 "Base with Index"
   instructions (see also MAX_REGS_PER_ADDRESS and others). */

#define GO_IF_BASED_ADDRESS(X, ADDR) {					\
   if ((GET_CODE (X) == REG && REG_OK_FOR_BASE_P(X)))			\
     goto ADDR;								\
   if (GET_CODE (X) == PLUS)						\
    { register rtx x0 = XEXP(X,0), x1 = XEXP(X,1);			\
      if ((REG_P(x0) && REG_OK_FOR_BASE_P(x0) && CONSTANT_ADDRESS_P(x1)) \
       || (REG_P(x1) && REG_OK_FOR_BASE_P(x1) && CONSTANT_ADDRESS_P(x0))) \
     goto ADDR; } }

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR) {			\
	if (CONSTANT_ADDRESS_P(X)) goto ADDR;				\
	GO_IF_BASED_ADDRESS(X,ADDR) }


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output. */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 68000, only predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */
/* 1750: not used. */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/*****************************************************************************/

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE QImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 0

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
/* (was: "1750: not counting the MOV instruction") */
#define MOVE_MAX 16

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count. */
/* #define SHIFT_COUNT_TRUNCATED 1 */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'. 
   1750: for now, `char' is 16 bits wide anyway.
   #define PROMOTE_PROTOTYPES */

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode QImode

/* A function address in a call instruction
   is a 16-bit address (for indexing purposes) */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */
/* 1750 note: haven't paid attention to this yet. */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL(RTX) >= -16 && INTVAL(RTX) <= 16) return 1;	\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 5;							\
  case CONST_DOUBLE:						\
    return 7;

#define ADDRESS_COST(ADDRESS)	(memop_valid(ADDRESS) ?  3 : 1000)

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */
/* MIL-STD-1750: none -- just has the garden variety C,P,Z,N flags. */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.
   1750: See file out-1750a.c for notice_update_cc().  */

#define NOTICE_UPDATE_CC(EXP, INSN) notice_update_cc(EXP)

/**********************************************/
/* Produce debugging info in the DWARF format 
   #define DWARF_DEBUGGING_INFO
*/

/*****************************************************************************/

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)   {					\
   char *p, name[40];							\
   if ((p = (char *)strrchr(main_input_filename,'/')) != NULL ? 1 :	\
       (p = (char *)strrchr(main_input_filename,']')) != NULL)		\
	p++;								\
   else									\
	p = main_input_filename;					\
   strcpy(name,p);							\
   if (p = (char *)strchr(name,'.'))					\
	*p = '\0';							\
   fprintf(FILE,"\tname %s\n",name); 					\
   fprintf(FILE,"\tnolist\n\tinclude \"ms1750.inc\"\n\tlist\n\n");	\
   fprintf(FILE,"\tglobal\t__main\n\n");  }

/* Output at end of assembler file.  
   For 1750, we copy the data labels accrued in datalbl[] from the Constants 
   section (Konst) to the Writable-Data section (Static).     */

#define ASM_FILE_END(FILE)	\
   do {									\
      if (datalbl_ndx >= 0) {						\
         int i, cum_size=0;						\
         fprintf(FILE,"\n\tstatic\ninit_srel\n");			\
         for (i = 0; i <= datalbl_ndx; i++) {				\
	    if (datalbl[i].name == NULL)				\
	    {								\
	       fprintf(stderr, "asm_file_end internal datalbl err\n");	\
	       exit (0);						\
	    }								\
            fprintf(FILE,"%s \tblock %d\n",				\
                 datalbl[i].name,datalbl[i].size);			\
            cum_size += datalbl[i].size;				\
	 }								\
         fprintf(FILE,"\n\tinit\n");					\
         fprintf(FILE,"\tlim\tr0,init_srel\n");           /* destin. */	\
         fprintf(FILE,"\tlim\tr1,%d\n",cum_size);         /* count */	\
         fprintf(FILE,"\tlim\tr2,K%s\n",datalbl[0].name); /* source */	\
         fprintf(FILE,"\tmov\tr0,r2\n");				\
         fprintf(FILE,"\n\tnormal\n");					\
         datalbl_ndx = -1;			/* reset stuff */	\
         for (i = 0; i < DATALBL_ARRSIZ; i++)				\
            datalbl[i].size = 0;					\
      }									\
      fprintf(FILE,"\n\tend\n");					\
   } while (0)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "\n\tif 0\n; by ASM_APP_ON\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "\n\tendif\n"


#define EXTRA_SECTIONS  in_readonly_data

#define EXTRA_SECTION_FUNCTIONS		\
    void const_section()				\
    {							\
	fprintf(asm_out_file,"\tkonst\n");		\
	current_section = Konst;				\
    }							\
    check_section(enum section sect)			\
    {							\
        if (current_section != sect) {			\
	    fprintf(asm_out_file,"\t%s\n",sectname[(int)sect]); \
	    current_section = sect;			\
	}						\
	switch (sect) {					\
	  case Init:					\
	  case Normal:					\
	    in_section = in_text;			\
	    break;					\
	  case Static:					\
	    in_section = in_data;			\
	    break;					\
	  case Konst:					\
	    in_section = in_readonly_data;		\
	    break;					\
	}						\
    }

		
/* Function that switches to the read-only data section (optional) */
#define READONLY_DATA_SECTION	const_section

/* Output before program init section */
#define INIT_SECTION_ASM_OP "\n\tinit     ; init_section\n"

/* Output before program text section */
#define TEXT_SECTION_ASM_OP "\n\tnormal   ; text_section\n"

/* Output before writable data.  */
#define DATA_SECTION_ASM_OP "\n\tstatic   ; data_section\n"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
 { "0", "1", "2", "3", "4", "5", "6", "7", \
   "8", "9","10","11","12","13","14","15" }

/* How to renumber registers for dbx and gdb. */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/******************  Assembler output formatting  **********************/

#define ASM_IDENTIFY_GCC(FILE)  fputs ("; gcc2_compiled:\n", FILE)

#define ASM_COMMENT_START  ";"

#define ASM_OUTPUT_FUNNAM(FILE,NAME)	\
	fprintf(FILE,"%s\n",NAME)

#define ASM_OUTPUT_OPCODE(FILE,PTR)  do {		\
	while (*(PTR) != '\0' && *(PTR) != ' ') {	\
	    putc (*(PTR), FILE);			\
	    (PTR)++;					\
	  }						\
	while (*(PTR) == ' ')				\
	    (PTR)++;					\
	putc ('\t', FILE);				\
	program_counter += 2;				\
     } while (0)

#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL)	\
	fprintf(FILE,"%s\n",NAME)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
/* 1750 note: Labels are prefixed with a 'K'. This is because handling
   has been changed for labels to be output in the "Constants" section
   (named "Konst"), and special initialization code takes care of copying
   the Const-section data into the writable data section (named "Static").
   In the Static section we therefore have the true label names (i.e.
   not prefixed with 'K').  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do {  if (NAME[0] == '.') {					\
	   fprintf(stderr,"Oops! label %s can't begin with '.'\n",NAME); \
	   abort();						\
	}							\
	else {							\
	   check_section(Konst);				\
	   fprintf(FILE,"K%s\n",NAME);				\
	   datalbl[++datalbl_ndx].name = (char *)strdup (NAME);	\
	   label_pending = 1;					\
	}							\
  } while (0)


/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME) do {		\
	   fprintf (FILE, "\tglobal  %s\t; export\n", NAME);	\
  } while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
	  fprintf (FILE, "%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)		\
	do {							\
	  if (strcmp(PREFIX,"LC") == 0) {			\
	     label_pending = 1;					\
	     datalbl[++datalbl_ndx].name = (char *) malloc (9); \
	     sprintf(datalbl[datalbl_ndx].name,"LC%d",NUM);	\
	     check_section(Konst);				\
	     fprintf(FILE,"K%s%d\n",PREFIX,NUM);		\
	  }							\
	  else if (find_jmplbl(NUM) < 0) {			\
	     jmplbl[++jmplbl_ndx].num = NUM;			\
	     jmplbl[jmplbl_ndx].pc = program_counter;		\
	     fprintf(FILE, "%s%d\n", PREFIX, NUM);		\
	  }							\
	} while (0)


/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
	  sprintf (LABEL, "%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a 1750A `float'
   constant.  */

#define ASM_OUTPUT_SHORT_FLOAT(FILE,VALUE) 			\
  do {								\
      if (label_pending)					\
	 label_pending = 0;					\
      else							\
         datalbl[++datalbl_ndx].name = float_label('D',VALUE);	\
      sprintf (datalbl[datalbl_ndx].value, "%lf", (double) VALUE); \
      datalbl[datalbl_ndx].size = 2;				\
      fprintf (FILE, "\tdataf\t%lf\n",VALUE);			\
  } while(0)

/* This is how to output an assembler line defining a 1750A `double'
    constant. */

#define ASM_OUTPUT_THREE_QUARTER_FLOAT(FILE,VALUE)		\
  do {								\
      if (label_pending)					\
	 label_pending = 0;					\
      else							\
         datalbl[++datalbl_ndx].name = float_label('E',VALUE);	\
      sprintf (datalbl[datalbl_ndx].value, "%lf", VALUE);	\
      datalbl[datalbl_ndx].size = 3;				\
      fprintf(FILE,"\tdataef\t%lf\n",VALUE);			\
  } while (0)

/* This is how to output an assembler line defining a string constant.  */

#define ASM_OUTPUT_ASCII(FILE, PTR, LEN)  do {		\
	int i;								\
	if (! label_pending)						\
	   fprintf(FILE,";in ASM_OUTPUT_ASCII without label_pending\n");\
	else {								\
	   label_pending = 0;						\
	   datalbl[datalbl_ndx].size = LEN;				\
	}								\
	for (i = 0; i < LEN; i++)					\
	  if (PTR[i] >= 32 && PTR[i] < 127)				\
	    fprintf(FILE,"\tdata\t%d\t; '%c'\n",PTR[i],PTR[i]);		\
	  else								\
	    fprintf(FILE,"\tdata\t%d\t; (ascii)\n",PTR[i]);		\
  } while (0)

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  do {	  \
	if (! label_pending) 						\
	   fprintf(FILE,";in ASM_OUTPUT_INT without label_pending\n");	\
	else {								\
	   label_pending = 0;						\
	   datalbl[datalbl_ndx].size = 1;				\
	}								\
	fprintf(FILE, "\tdata\t"); output_addr_const(FILE,VALUE);	\
	fprintf(FILE, "\n"); } while (0)

/* This is how to output an assembler line defining a `long int' constant. */

#define ASM_OUTPUT_LONG_INT(FILE,VALUE) do {	  \
	if (! label_pending)						\
	   fprintf(FILE,";in ASM_OUTPUT_LONG_INT without label_pending\n");\
	else {								\
	   label_pending = 0;						\
	   datalbl[datalbl_ndx].size = 2;				\
	}								\
	fprintf(FILE, "\tdatal\t"); output_addr_const(FILE,VALUE);	\
	fprintf(FILE, "\n"); } while (0)

/* Likewise for `short' and `char' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  ASM_OUTPUT_INT(FILE,VALUE)

/* For 1750, we treat char same as word. Tektronix 1750
   Assembler does a better (packing) job with strings.  */
#define ASM_OUTPUT_CHAR(FILE,VALUE)   ASM_OUTPUT_INT(FILE,VALUE)

/* This is how to output an assembler line for a numeric constant byte.  */
/* 1750: For the time being, treating this same as word. Tektronix 1750
   Assembler does a better (packing) job with strings.  */
#define ASM_OUTPUT_BYTE(FILE,VALUE)  ASM_OUTPUT_INT(FILE,VALUE)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tPSHM R%s,R%s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
	fprintf (FILE, "\tPOPM R%s,R%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute. */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)   \
	fprintf (FILE, "\tdata\tL%d ;addr_vec_elt\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
	fprintf (FILE, "\tdata\tL%d-L%d ;addr_diff_elt\n", VALUE,REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
 fprintf(FILE,"; in ASM_OUTPUT_ALIGN: pwr_of_2_bytcnt=%d\n",LOG)

#define ASM_OUTPUT_SKIP(FILE,SIZE)	\
   fprintf(FILE,"; in ASM_OUTPUT_SKIP: size=%d\n",SIZE)

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  do {	\
	fprintf (FILE, "\tcommon  %s,%d\n", NAME, SIZE);	\
     } while (0)

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)  do {		\
	fprintf (FILE, "\tglobal  %s\t; import\n", NAME);	\
     }  while (0)

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  do {  \
	check_section (Static);					\
	fprintf(FILE,"%s \tblock   %d\t; local common\n",NAME,SIZE);	\
     } while (0)

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

#define ASM_OUTPUT_CONSTRUCTOR(FILE, NAME)  do {	\
	fprintf(FILE, "\tinit\n\t"); assemble_name(NAME); \
        fprintf(FILE,"  ;constructor"); } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE, NAME)  do {	\
	fprintf(FILE, "\tinit\n\t"); assemble_name(NAME); \
        fprintf(FILE,"  ;destructor"); } while (0)

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015


/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.
   1750 note:  there are three special CODE characters:
        'D', 'E': print a reference to a floating point constant (D=double,
		  E=single precision) label name
	'F': print a label defining a floating-point constant value
	'J': print the absolute value of a negative INT_CONST
	     (this is used in LISN/CISN/MISN/SISP and others)   */

/* 1750A: see file aux-output.c */
#define PRINT_OPERAND(FILE, X, CODE)  print_operand(FILE,X,CODE)
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE,ADDR)

