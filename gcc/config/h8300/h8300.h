/* Definitions of target machine for GNU compiler. 
   Hitachi H8/300 version generating coff 
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
   Contributed by Steve Chamberlain (sac@cygnus.com)

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

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-D__H8300__ -D_DOUBLE_IS_32BITS -Acpu(h8300) -Amachine(h8300)"

#define LIB_SPEC "%{mrelax:-relax} %{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (Hitachi H8/300)");

/* Run-time compilation parameters selecting different hardware subsets.  */
int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"int32",8},	\
    {"long16",16},	\
    {"double64",32},	\
    {"addresses",64 },	\
    {"quickcall",128},  \
    {"slowbyte",256},  \
    {"noquick",512},  \
    {"relax",1024},  \
    { "", TARGET_DEFAULT}}

#define OPTIMIZATION_OPTIONS(OPTIMIZE)          \
{                                               \
  obey_regdecls = 0;                            \
  flag_omit_frame_pointer = (OPTIMIZE > 1);     \
}

#define OVERRIDE_OPTIONS                        \
{                                               \
  flag_no_function_cse = 1;                     \
}

#define TARGET_INT32 	 (target_flags & 8)
#define TARGET_LONG16 	 (target_flags & 16)
#define TARGET_DOUBLE32  (!(target_flags & 32))
#define TARGET_ADDRESSES (target_flags & 64)
#define TARGET_QUICKCALL (target_flags & 128)
#define TARGET_SLOWBYTE  (target_flags & 256)
#define TARGET_NOQUICK   (target_flags & 512)

/* Default target_flags if no switches specified.  */
#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#define TARGET_NO_IMPORT (target_flags & MASK_NO_IMPORT)

/* Target machine storage layout.  */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the H8/300.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the H8/300.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.  

   This is true on an H8/300 (actually we can make it up, but we choose to
   be consistent.  */
#define WORDS_BIG_ENDIAN 1

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

#define UNITS_PER_PTR 2

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 	16
#define MAX_BITS_PER_WORD 16

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 2

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 16

#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE (TARGET_INT32 ? 32 : 16)
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE (TARGET_DOUBLE32 ? 32 : 64)
#define FLOAT_TYPE_SIZE 32
#define LONG_DOUBLE_TYPE_SIZE DOUBLE_TYPE_SIZE

#define MAX_FIXED_MODE_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY (16)

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 16

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS  0

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 16

/* No structure field wants to be aligned rounder than this.  */
#define BIGGEST_FIELD_ALIGNMENT 16

/* The stack goes in 16 bit lumps.  */
#define STACK_BOUNDARY 16

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.

   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  

   Reg 8 does not correspond to any hardware register, but instead
   appears in the RTL as an argument pointer prior to reload, and is
   eliminated during reloading in favor of either the stack or frame
   pointer.  */
#define FIRST_PSEUDO_REGISTER 9

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator. 

   r7 is the stack pointer, r8 is the arg pointer.  */
#define FIXED_REGISTERS \
   {  0,0,0,0,  0,0,0,1, 1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you
   like.  

   The H8/300 destroys r0, r1, r4 and r5.  */

#define CALL_USED_REGISTERS \
    {1,1,0,0,  1,1,0,1, 1 }

/* This is the order in which to allocate registers
   normally.  */
#define REG_ALLOC_ORDER { 0, 1, 4, 5, 2, 3, 6, 7, 8 }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.

   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.

   If an even reg, then anything goes.  Otherwise the mode must be QI
   or HI.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
 ((((REGNO) & 1) == 0) || (MODE == HImode) || (MODE == QImode))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  ((MODE1) == (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* H8/300 pc is not overloaded on a register.  */
/*#define PC_REGNUM 15*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 7

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 6

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 8

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 0

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
   
/* The H8/300 has only one kind of register, but we mustn't do byte by
   byte operations on the sp, so we keep it as a different class.  */

enum reg_class { NO_REGS, LONG_REGS, GENERAL_REGS, SP_REG, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES \
 {"NO_REGS", "LONG_REGS", "GENERAL_REGS", "SP_REG", "ALL_REGS", "LIM_REGS"}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS  				\
{     0,   			/* No regs      */	\
   0x7f,                        /* LONG_REGS    */      \
   0x7f,			/* GENERAL_REGS */	\
   0x80,			/* SP_REG       */     	\
   0xff,			/* ALL_REGS 	*/	\
}

  /* The same information, inverted:
     Return the class number of the smallest class containing
     reg number REGNO.  This could be a conditional expression
     or could index an array.  */

#define REGNO_REG_CLASS(REGNO)			\
 ((REGNO) >= STACK_POINTER_REGNUM		\
  ? ((REGNO) >= FRAME_POINTER_REGNUM		\
     ? SP_REG					\
     : GENERAL_REGS)				\
  : LONG_REGS)

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS NO_REGS
#define BASE_REG_CLASS  GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
 ((C) == 'a' ? SP_REG : (((C) == 'l') ? LONG_REGS : NO_REGS))

/* The letters I, J, K, L, M, N, O, P in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  		\
  ((C) == 'I' ? (VALUE) == 0 : 				\
   (C) == 'J' ? (((VALUE) & 0xff00) == 0) : 		\
   (C) == 'K' ? ((VALUE) == 1 || (VALUE) == 2) :	\
   (C) == 'L' ? ((VALUE) == -1 || (VALUE) == -2) :      \
   (C) == 'M' ? ((VALUE) == 3 || (VALUE) == 4) :        \
   (C) == 'N' ? ((VALUE) == -3 || (VALUE) == -4) :      \
   (C) == 'O' ? (potg8 (VALUE)) :			\
   (C) == 'P' ? (potl8 (VALUE)) :			\
   0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself. 

     `G' is a floating-point zero.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)          \
  ((C) == 'G' ? ((VALUE) == CONST0_RTX (DFmode)         \
                 || (VALUE) == CONST0_RTX (DFmode))     \
   : 0)

/* Extra constraints - 'U' if for an operand valid for a bset
   destination; i.e. a register or register indirect target.  */
#define EXTRA_CONSTRAINT(OP, C) 			\
  (((C) == 'U')						\
   ? ((GET_CODE (OP) == REG) 				\
      || ((GET_CODE (OP) == MEM) 			\
	  && GET_CODE (XEXP (OP, 0)) == REG))		\
   : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

/* On the H8, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Any SI register to register move may need to be reloaded, 
   so define REGISTER_MOVE_COST to be > 2 so that reload never
   shortcuts.  */
#define REGISTER_MOVE_COST(CLASS1, CLASS2) 3

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.

   On the H8/300, @-sp really pushes a byte if you ask it to - but that's
   dangerous, so we claim that it always pushes a word, then we catch
   the mov.b rx,@-sp and turn it into a mov.w rx,@-sp on output.  */
#define PUSH_ROUNDING(BYTES)   (((BYTES) + 1) & ~1)

/* Offset of first parameter from the argument pointer register value.  */

/* Is equal to the size of the saved fp + pc, even if an fp isn't
   saved since the value is used before we know.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the H8/300 the return does not pop anything.  */
#define RETURN_POPS_ARGS(FUNTYPE,SIZE) 0

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the i386.  First, the
   frame pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the argument pointer register can always be
   eliminated; it is replaced with either the stack or frame pointer. */
#define ELIMINABLE_REGS				\
{{ ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM},   \
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

#define CAN_ELIMINATE(FROM, TO)					\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM	\
  ? ! frame_pointer_needed					\
  : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) OFFSET = io (FROM,TO)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0. 
   
   On the H8/300 the return value is in R0/R1.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
 	gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the H8/300 the return value is in R0/R1.  */
#define LIBCALL_VALUE(MODE) \
	gen_rtx (REG, MODE, 0)

/* 1 if N is a possible register number for a function value.
   On the H8/300, R0 is the only register thus used.  */
#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */
#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for function argument passing.
   On the H8/300, no registers are used in this way.  */
#define FUNCTION_ARG_REGNO_P(N) 0

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE 0
#define STRUCT_VALUE_REGNUM 0
#define STRUCT_VALUE_INCOMING 0

/* Return true if X should be returned in memory.  */
#define RETURN_IN_MEMORY(X) \
  (TYPE_MODE (X) == BLKmode || GET_MODE_SIZE (TYPE_MODE (X)) > 4)

/* Don't default to pcc-struct-return, because we have already specified
   exactly how to return structures in the RETURN_IN_MEMORY macro.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers.  */
#define SMALL_REGISTER_CLASSES

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   Hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the H8/300, this is a two item struct, the first is the number of bytes
   scanned so far, the second the name of any libcall.  */

#define CUMULATIVE_ARGS struct cum_arg
struct cum_arg { int nbytes; struct rtx_def* libcall; };

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the H8/300, the offset starts at 0.  */
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
	((CUM).nbytes = 0, (CUM).libcall = LIBNAME)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.

   All sizes rounded up to even bytes.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)    \
 ((CUM).nbytes += ((MODE) != BLKmode                    \
		   ? (GET_MODE_SIZE (MODE) + 1) & ~1    \
		   : (int_size_in_bytes (TYPE) + 1) & ~1))

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

/* On the H8/300 all normal args are pushed, we pass args to named
   functions in registers.  */

extern struct rtx_def *function_arg();
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, NAMED)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) \
 function_prologue (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  	\
   fprintf (FILE, "\t; not implemented\n", (LABELNO));

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
 fprintf (FILE, "\t; not implemented \n", LABELNO, LABELNO);

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)		\
 fprintf (FILE, "\t; not implemented\n");

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 0

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

#define FUNCTION_EPILOGUE(FILE, SIZE)		\
 function_epilogue (FILE, SIZE)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

#define TRAMPOLINE_TEMPLATE(FILE) 		\
 fprintf (FILE, " trampolines not implemented\n");

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 15

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

/* We copy the register-mask from the function's pure code
   to the start of the trampoline.  */
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, HImode, TRAMP),				\
		  gen_rtx (MEM, HImode, FNADDR));			\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 4)), CXT);\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 11)),	\
		  plus_constant (FNADDR, 2));				\
}

/* Addressing modes, and classification of registers for them.
   Although the H8/300 has pre decrement destinations and post
   increment sources on moves, they are not general enough to be much
   use to gcc */

/*#define HAVE_POST_INCREMENT*/
/*#define HAVE_POST_DECREMENT */

/*#define HAVE_PRE_DECREMENT*/
/*#define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
 
#define REGNO_OK_FOR_INDEX_P(regno)  0

#define REGNO_OK_FOR_BASE_P(regno) \
  ((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) (GET_CODE (X) != CONST_DOUBLE)

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
#define REG_OK_FOR_INDEX_P(X) 0
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) 1

#define REG_OK_FOR_INDEX_P_STRICT(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_P_STRICT(X)  REGNO_OK_FOR_BASE_P (REGNO (X))

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

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually
   machine-independent.  


   On the H8/300, a legitimate address has the form
   REG, REG+CONSTANT_ADDRESS or CONSTANT_ADDRESS.  */

/* Accept either REG or SUBREG where a register is valid.  */
  
#define RTX_OK_FOR_BASE_P(X)                                    \
  ((REG_P (X) && REG_OK_FOR_BASE_P (X))                         \
   || (GET_CODE (X) == SUBREG && REG_P (SUBREG_REG (X))         \
       && REG_OK_FOR_BASE_P (SUBREG_REG (X))))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)         \
  if (RTX_OK_FOR_BASE_P (X)) goto ADDR;                 \
  if (CONSTANT_ADDRESS_P (X)) goto ADDR;                \
  if (GET_CODE (X) == PLUS                              \
      && CONSTANT_ADDRESS_P (XEXP (X, 1))               \
      && RTX_OK_FOR_BASE_P (XEXP (X, 0))) goto ADDR;

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the H8/300, don't do anything.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   On the H8/300, the predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand)
   and all indexed address depend thus (because the index scale factor
   is the length of the operand).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)			\
 if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC) goto LABEL;

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define this if the case instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/*#define CASE_VECTOR_PC_RELATIVE*/

/* Define this if the case instruction drops through after the table
   when the index is out of range.  Don't define it if the case insn
   jumps to the default label instead.  */
#define CASE_DROPS_THROUGH

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.

   On the H8/300, sign extension is expensive, so we'll say that chars
   are unsigned.  */
#define DEFAULT_SIGNED_CHAR 0

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 2

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS TARGET_SLOWBYTE

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode HImode

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) 	\
    case CONST_INT:				\
      switch (INTVAL (RTX))			\
	{					\
	case 0:					\
	case 1:					\
	case 2:					\
	case -1:				\
	case -2:				\
	  return 0;				\
	default:				\
          return 1;				\
	}					\
    case CONST:					\
    case LABEL_REF:				\
    case SYMBOL_REF:				\
      return 3;					\
    case CONST_DOUBLE:				\
      return 20;				

#define BRANCH_COST 2

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  */

#define RTX_COSTS(RTX,CODE,OUTER_CODE) 		\
   case MOD: 					\
   case DIV: 					\
     return 60;					\
   case MULT: 					\
     return 20; 				\
   case LSHIFT:					\
   case ASHIFT:					\
   case ASHIFTRT:				\
   case LSHIFTRT:				\
   case ROTATE:					\
   case ROTATERT:				\
    if (GET_MODE (RTX) == HImode) 		\
      return 2; 				\
    return 10; 					\

/* Tell final.c how to eliminate redundant test instructions.  */
  
/* Store in cc_status the expressions that the condition codes will describe
   after execution of an instruction whose pattern is EXP. Do not
   alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) 					\
switch (get_attr_cc (INSN))						\
  {	        							\
  case CC_NONE:								\
    /* Insn does not affect the CC at all.  */				\
    break;								\
  case CC_NONE_0HIT:							\
    /* Insn does not change the CC, but the 0'th operand has been	\
       changed.  */				        		\
    if (cc_status.value1 != 0						\
        && reg_overlap_mentioned_p (recog_operand[0], cc_status.value1))\
    cc_status.value1 = 0;						\
									\
    if (cc_status.value2 != 0						\
        && reg_overlap_mentioned_p (recog_operand[0], cc_status.value2))\
    cc_status.value2 = 0;						\
    break;								\
  case CC_SET:								\
    /* Insn sets CC to recog_operand[0], but overflow is impossible.  */\
    CC_STATUS_INIT;							\
    cc_status.flags |= CC_NO_OVERFLOW;					\
    cc_status.value1 = recog_operand[0];				\
    break;								\
  case CC_COMPARE:							\
    /* The insn is a compare instruction.  */				\
    CC_STATUS_INIT;							\
    cc_status.value1 = recog_operand[0];				\
    cc_status.value1 = recog_operand[1];				\
    break;								\
  case CC_WHOOPS:							\
  case CC_CLOBBER:							\
    /* Insn clobbers CC.  */						\
    CC_STATUS_INIT;							\
    break;								\
  }									\

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  			\
{								\
  if (cc_status.flags & CC_NO_OVERFLOW)				\
    return NO_OV;						\
  return NORMAL;						\
}

/* Control the assembler format that we output.  */

#define ASM_IDENTIFY_GCC /* nothing */

/* Output at beginning of assembler file.  */
#define ASM_FILE_START(FILE)					\
{								\
  fprintf (FILE, ";\tGCC For the Hitachi H8/300\n");		\
  if (optimize)							\
    fprintf (FILE, "; -O%d\n", optimize);			\
  fprintf (FILE, "\n\n");					\
  output_file_directive (FILE, main_input_filename);		\
}

#define ASM_FILE_END(FILE)					\
  fprintf (FILE, "\t.end\n");

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON 	"; #APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF 	"; #NO_APP\n"

#define FILE_ASM_OP 	"\t.file\n"
#define IDENT_ASM_OP 	"\t.ident\n"

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.section .text"
#define DATA_SECTION_ASM_OP "\t.section .data"
#define BSS_SECTION_ASM_OP  "\t.section .bss"

#define EXTRA_SECTIONS in_user

#define EXTRA_SECTION_FUNCTIONS				\
void							\
user_section (name)					\
    char *name;						\
{							\
  fprintf (asm_out_file, "\t.section\t%s\n", name);	\
  in_section = in_user;					\
}

#define MAX_TEXT_ALIGN 16

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{ "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "ap"}

/* How to renumber registers for dbx and gdb.
   H8/300 needs no change in the numeration.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */
#define SDB_DEBUGGING_INFO
#define SDB_DELIM	"\n"

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/*#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME) */

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */
#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.global ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/*#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
  ASM_OUTPUT_LABEL(FILE, NAME); */

/* This is how to output a reference to a user-level label named NAME.  */
#define ASM_OUTPUT_LABELREF(FILE, NAME)	\
 asm_output_labelref (FILE, NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE, PREFIX, NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)	\
  sprintf (LABEL, "*.%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.
   It is .dfloat or .gfloat, depending.  */

/*#define ASM_OUTPUT_DOUBLE(FILE, VALUE)		\
  fprintf (FILE, "\t.double %.20e\n", (VALUE))*/

/* This is how to output an assembler line defining a `float' constant.  */
#define ASM_OUTPUT_FLOAT(FILE, VALUE)		\
  fprintf (FILE, "\t.float %.20e\n", 		\
	   ((VALUE) > 1e30 ? 1e30		\
	    : ((VALUE) < -1e30) ? -1e30 : (double) ((float) VALUE)));

/* This is how to output an assembler line defining an `int' constant.  */
#define ASM_OUTPUT_INT(FILE, VALUE)  		\
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */
#define ASM_OUTPUT_CHAR(FILE, VALUE)  		\
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_SHORT(FILE, VALUE)  		\
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */
#define ASM_OUTPUT_BYTE(FILE, VALUE)  		\
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */
#define ASM_OUTPUT_REG_PUSH(FILE, REGNO)  	\
  fprintf (FILE, "\tpush %s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */
#define ASM_OUTPUT_REG_POP(FILE, REGNO)  	\
  fprintf (FILE, "\tpop\t%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute. */
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  asm_fprintf (FILE, "\t.word .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word.w .L%d-.L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE, LOG)		\
  if ((LOG) != 0)				\
    fprintf (FILE, "\t.align %d\n", 1 << (LOG))

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "%s\t \"%s\"\n", IDENT_ASM_OP, NAME)

#define ASM_OUTPUT_SKIP(FILE, SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  	\
( fputs ("\t.comm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE,ROUNDED)	\
( fputs ("\t.lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%d\n", (SIZE)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)	 \
  ((CODE) == '#')

#define PRINT_OPERAND(FILE, X, CODE)		\
 print_operand (FILE, X, CODE)

/* Print a memory operand whose address is X, on file FILE.
   This uses a function in h8300.c.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) 	 \
 print_operand_address (FILE, ADDR)

#define HANDLE_PRAGMA(FILE) handle_pragma (FILE)

#define FINAL_PRESCAN_INSN(insn, operand, nop)	\
 final_prescan_insn (insn, operand, nop)

/* Define this macro if GNU CC should generate calls to the System V
   (and ANSI C) library functions `memcpy' and `memset' rather than
   the BSD functions `bcopy' and `bzero'.  */
#define TARGET_MEM_FUNCTIONS 1

/* Define subroutines to call to handle various operations not
   supported in the hardware */

#define DIVHI3_LIBCALL "__divhi3"
#define UDIVHI3_LIBCALL "__udivhi3"
#define MULHI3_LIBCALL "__mulhi3"
#define MODHI3_LIBCALL "__modhi3"
#define UMODHI3_LIBCALL "__umodhi3"
#define ADDSI3_LIBCALL "__addsi3"
#define SUBSI3_LIBCALL "__subsi3"

#define MOVE_RATIO 3

#if 0
/* This stuff doesn't work currently.  */
#undef SELECT_SECTION
#define SELECT_SECTION(DECL,RELOC) select_section(DECL, RELOC)
#define SELECT_FUNCTION_SECTION(a,b) SELECT_SECTION(a,b)
#endif
