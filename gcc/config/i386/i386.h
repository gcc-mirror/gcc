/* Definitions of target machine for GNU compiler for Intel 80386.
   Copyright (C) 1988, 1992 Free Software Foundation, Inc.

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


/* The purpose of this file is to define the characteristics of the i386,
   independent of assembler syntax or operating system.

   Three other files build on this one to describe a specific assembler syntax:
   bsd386.h, att386.h, and sun386.h.

   The actual tm.h file for a particular system should include
   this file, and then the file for the appropriate assembler syntax.

   Many macros that specify assembler syntax are omitted entirely from
   this file because they really belong in the files for particular
   assemblers.  These include AS1, AS2, AS3, RP, IP, LPREFIX, L_SIZE,
   PUT_OP_SIZE, USE_STAR, ADDR_BEG, ADDR_END, PRINT_IREG, PRINT_SCALE,
   PRINT_B_I_S, and many that start with ASM_ or end in ASM_OP.  */

/* Names to predefine in the preprocessor for this target machine.  */

#define I386 1

/* Stubs for half-pic support if not OSF/1 reference platform.  */

#ifndef HALF_PIC_P
#define HALF_PIC_P() 0
#define HALF_PIC_NUMBER_PTRS 0
#define HALF_PIC_NUMBER_REFS 0
#define HALF_PIC_ENCODE(DECL)
#define HALF_PIC_DECLARE(NAME)
#define HALF_PIC_INIT()	error ("half-pic init called on systems that don't support it.")
#define HALF_PIC_ADDRESS_P(X) 0
#define HALF_PIC_PTR(X) X
#define HALF_PIC_FINISH(STREAM)
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* configure can arrage to make this 2, to force a 486.  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

/* Compile 80387 insns for floating point (not library calls).  */
#define TARGET_80387 (target_flags & 1)
/* Compile code for an i486. */
#define TARGET_486 (target_flags & 2)
/* Compile using ret insn that pops args.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.  */  
#define TARGET_RTD (target_flags & 8)
/* Compile passing first two args in regs 0 and 1.
   This exists only to test compiler features that will
   be needed for RISC chips.  It is not usable
   and is not intended to be usable on this cpu.  */
#define TARGET_REGPARM (target_flags & 020)

/* Put uninitialized locals into bss, not data.
   Meaningful only on svr3.  */
#define TARGET_SVR3_SHLIB (target_flags & 040)

/* Use IEEE floating point comparisons.  These handle correctly the cases
   where the result of a comparison is unordered.  Normally SIGFPE is
   generated in such cases, in which case this isn't needed.  */
#define TARGET_IEEE_FP (target_flags & 0100)

/* Functions that return a floating point value may return that value
   in the 387 FPU or in 386 integer registers.  If set, this flag causes
   the 387 to be used, which is compatible with most calling conventions. */
#define TARGET_FLOAT_RETURNS_IN_80387 (target_flags & 0200)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { { "80387", 1},				\
    { "no-80387", -1},				\
    { "soft-float", -1},			\
    { "no-soft-float", 1},			\
    { "486", 2},				\
    { "no-486", -2},				\
    { "386", -2},				\
    { "rtd", 8},				\
    { "no-rtd", -8},				\
    { "regparm", 020},				\
    { "no-regparm", -020},			\
    { "svr3-shlib", 040},			\
    { "no-svr3-shlib", -040},			\
    { "ieee-fp", 0100},				\
    { "no-ieee-fp", -0100},			\
    { "fp-ret-in-387", 0200},			\
    { "no-fp-ret-in-387", -0200},		\
    SUBTARGET_SWITCHES                          \
    { "", TARGET_DEFAULT | TARGET_CPU_DEFAULT}}

/* This is meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES


/* target machine storage layout */

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the 80386.  */

#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on the 80386.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
/* Not true for 80386 */
#define WORDS_BIG_ENDIAN 0

/* number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 80386, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.
   For i486, we get better performance by aligning to a cache
   line (i.e. 16 byte) boundary.  */
#define FUNCTION_BOUNDARY (TARGET_486 ? 128 : 32)

/* Alignment of field after `int : 0' in a structure. */

#define EMPTY_FIELD_BOUNDARY 32

/* Minimum size in bits of the largest boundary to which any
   and all fundamental data types supported by the hardware
   might need to be aligned. No data type wants to be aligned
   rounder than this.  The i386 supports 64-bit floating point
   quantities, but these can be aligned on any 32-bit boundary.  */
#define BIGGEST_ALIGNMENT 32

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* If bit field type is int, don't let it cross an int,
   and give entire struct the alignment of an int.  */
/* Required on the 386 since it doesn't have bitfield insns.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Align loop starts for optimal branching.  */
#define ASM_OUTPUT_LOOP_ALIGN(FILE) \
  ASM_OUTPUT_ALIGN (FILE, 2)

/* This is how to align an instruction for optimal branching.
   On i486 we'll get better performance by aligning on a
   cache line (i.e. 16 byte) boundary.  */
#define ASM_OUTPUT_ALIGN_CODE(FILE)	\
  ASM_OUTPUT_ALIGN ((FILE), (TARGET_486 ? 4 : 2))

/* Standard register usage.  */

/* This processor has special stack-like registers.  See reg-stack.c
   for details. */

#define STACK_REGS

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   In the 80386 we give the 8 general purpose registers the numbers 0-7.
   We number the floating point registers 8-15.
   Note that registers 0-7 can be accessed as a  short or int,
   while only 0-3 may be used with byte `mov' instructions.

   Reg 16 does not correspond to any hardware register, but instead
   appears in the RTL as an argument pointer prior to reload, and is
   eliminated during reloading in favor of either the stack or frame
   pointer. */

#define FIRST_PSEUDO_REGISTER 17

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the 80386, the stack pointer is such, as is the arg pointer. */
#define FIXED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg*/       \
{  0, 0, 0, 0, 0, 0, 0, 1, 0,  0,  0,  0,  0,  0,  0,  0,  1 }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg*/ \
{  1, 1, 1, 0, 0, 0, 0, 1, 1,  1,  1,  1,  1,  1,  1,  1,  1 }

/* Macro to conditionally modify fixed_regs/call_used_regs.  */
#define CONDITIONAL_REGISTER_USAGE			\
  {							\
    if (flag_pic)					\
      {							\
	fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
	call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;	\
      }							\
    if (! TARGET_80387 && ! TARGET_FLOAT_RETURNS_IN_80387) \
      { 						\
	int i; 						\
	HARD_REG_SET x;					\
        COPY_HARD_REG_SET (x, reg_class_contents[(int)FLOAT_REGS]); \
        for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ )	\
         if (TEST_HARD_REG_BIT (x, i)) 			\
	  fixed_regs[i] = call_used_regs[i] = 1; 	\
      }							\
  }

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   Actually there are no two word move instructions for consecutive 
   registers.  And only registers 0-3 may have mov byte instructions
   applied to them.
   */

#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (FP_REGNO_P (REGNO) ? 1 \
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the 80386, the first 4 cpu registers can hold any mode
   while the floating point registers may hold only floating point.
   Make it clear that the fp regs could not hold a 16-byte float.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((REGNO) < 2 ? 1						\
   : (REGNO) < 4 ? 1						\
   : FP_REGNO_P ((REGNO))					\
   ? ((GET_MODE_CLASS (MODE) == MODE_FLOAT			\
       || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)		\
      && GET_MODE_UNIT_SIZE (MODE) <= 8)			\
   : (MODE) != QImode)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2) ((MODE1) == (MODE2))

/* A C expression returning the cost of moving data from a register of class
   CLASS1 to one of CLASS2.

   On the i386, copying between floating-point and fixed-point
   registers is expensive.  */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)		\
  ((((CLASS1) == FLOAT_REGS && (CLASS2) != FLOAT_REGS)	\
    || ((CLASS2) == FLOAT_REGS && (CLASS1) != FLOAT_REGS))	\
   ? 10 : 2)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* on the 386 the pc register is %eip, and is not usable as a general
   register.  The ordinary mov instructions won't work */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 7

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 6

/* First floating point reg */
#define FIRST_FLOAT_REG 8

/* First & last stack-like regs */
#define FIRST_STACK_REG FIRST_FLOAT_REG
#define LAST_STACK_REG (FIRST_FLOAT_REG + 7)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 16

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 2

/* Register to hold the addressing base for position independent
   code access to data items.  */
#define PIC_OFFSET_TABLE_REGNUM 3

/* Register in which address to store a structure value
   arrives in the function.  On the 386, the prologue
   copies this from the stack to register %eax.  */
#define STRUCT_VALUE_INCOMING 0

/* Place in which caller passes the structure value address.
   0 means push the value on the stack like an argument.  */
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
   class that represents their union.

   It might seem that class BREG is unnecessary, since no useful 386
   opcode needs reg %ebx.  But some systems pass args to the OS in ebx,
   and the "b" register constraint is useful in asms for syscalls.  */

enum reg_class
{
  NO_REGS,
  AREG, DREG, CREG, BREG,
  Q_REGS,			/* %eax %ebx %ecx %edx */
  SIREG, DIREG,
  INDEX_REGS,			/* %eax %ebx %ecx %edx %esi %edi %ebp */
  GENERAL_REGS,			/* %eax %ebx %ecx %edx %esi %edi %ebp %esp */
  FP_TOP_REG, FP_SECOND_REG,	/* %st(0) %st(1) */
  FLOAT_REGS,
  ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
{  "NO_REGS",				\
   "AREG", "DREG", "CREG", "BREG",	\
   "Q_REGS",				\
   "SIREG", "DIREG",			\
   "INDEX_REGS",			\
   "GENERAL_REGS",			\
   "FP_TOP_REG", "FP_SECOND_REG",	\
   "FLOAT_REGS",			\
   "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{      0,							\
     0x1,    0x2,  0x4,	 0x8,	/* AREG, DREG, CREG, BREG */	\
     0xf,			/* Q_REGS */			\
    0x10,   0x20,		/* SIREG, DIREG */		\
 0x1007f,			/* INDEX_REGS */		\
 0x100ff,			/* GENERAL_REGS */		\
  0x0100, 0x0200,		/* FP_TOP_REG, FP_SECOND_REG */	\
  0xff00,			/* FLOAT_REGS */		\
 0x1ffff }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class regclass_map[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) (regclass_map[REGNO])

/* When defined, the compiler allows registers explicitly used in the
   rtl to be used as spill registers but prevents the compiler from
   extending the lifetime of these registers. */

#define SMALL_REGISTER_CLASSES

#define QI_REG_P(X) \
  (REG_P (X) && REGNO (X) < 4)
#define NON_QI_REG_P(X) \
  (REG_P (X) && REGNO (X) >= 4 && REGNO (X) < FIRST_PSEUDO_REGISTER)

#define FP_REG_P(X) (REG_P (X) && FP_REGNO_P (REGNO (X)))
#define FP_REGNO_P(n) ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG)
  
#define STACK_REG_P(xop) (REG_P (xop) &&		       	\
			  REGNO (xop) >= FIRST_STACK_REG &&	\
			  REGNO (xop) <= LAST_STACK_REG)

#define NON_STACK_REG_P(xop) (REG_P (xop) && ! STACK_REG_P (xop))

#define STACK_TOP_P(xop) (REG_P (xop) && REGNO (xop) == FIRST_STACK_REG)

/* Try to maintain the accuracy of the death notes for regs satisfying the
   following.  Important for stack like regs, to know when to pop. */

/* #define PRESERVE_DEATH_INFO_REGNO_P(x) FP_REGNO_P(x) */

/* 1 if register REGNO can magically overlap other regs.
   Note that nonzero values work only in very special circumstances. */

/* #define OVERLAPPING_REGNO_P(REGNO) FP_REGNO_P (REGNO) */

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS INDEX_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)	\
  ((C) == 'r' ? GENERAL_REGS :					\
   (C) == 'q' ? Q_REGS :					\
   (C) == 'f' ? (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387	\
		 ? FLOAT_REGS					\
		 : NO_REGS) :					\
   (C) == 't' ? (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387	\
		 ? FP_TOP_REG					\
		 : NO_REGS) :					\
   (C) == 'u' ? (TARGET_80387 || TARGET_FLOAT_RETURNS_IN_80387	\
		 ? FP_SECOND_REG				\
		 : NO_REGS) :					\
   (C) == 'a' ? AREG :						\
   (C) == 'b' ? BREG :						\
   (C) == 'c' ? CREG :						\
   (C) == 'd' ? DREG :						\
   (C) == 'D' ? DIREG :						\
   (C) == 'S' ? SIREG : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   I is for non-DImode shifts.
   J is for DImode shifts.
   K and L are for an `andsi' optimization.
   M is for shifts that can be executed by the "lea" opcode.
   */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (VALUE) >= 0 && (VALUE) <= 31 :	\
   (C) == 'J' ? (VALUE) >= 0 && (VALUE) <= 63 :	\
   (C) == 'K' ? (VALUE) == 0xff :		\
   (C) == 'L' ? (VALUE) == 0xffff :		\
   (C) == 'M' ? (VALUE) >= 0 && (VALUE) <= 3 :	\
   0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  We allow constants even if
   TARGET_387 isn't set, because the stack register converter may need to
   load 0.0 into the function value register. */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? standard_80387_constant_p (VALUE) : 0)

/* Place additional restrictions on the register class to use when it
   is necessary to be able to hold a value of mode @var{mode} in a reload
   register for which class @var{class} would ordinarily be used. */

#define LIMIT_RELOAD_CLASS(MODE, CLASS) \
  ((MODE) == QImode && ((CLASS) == ALL_REGS || (CLASS) == GENERAL_REGS) \
   ? Q_REGS : (CLASS))

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   On the 80386 series, we prevent floating constants from being
   reloaded into floating registers (since no move-insn can do that)
   and we ensure that QImodes aren't reloaded into the esi or edi reg.  */

/* Don't put CONST_DOUBLE into FLOAT_REGS.
   QImode must go into class Q_REGS.
   MODE_INT must not go into FLOAT_REGS. */

#define PREFERRED_RELOAD_CLASS(X,CLASS)			\
  (GET_CODE (X) == CONST_DOUBLE				\
   ? (reg_class_subset_p ((CLASS), GENERAL_REGS) || (CLASS) == ALL_REGS \
      ? (CLASS) : NO_REGS)				\
   : GET_MODE (X) == QImode				\
   ? (! reg_class_subset_p ((CLASS), Q_REGS) ? Q_REGS : (CLASS))	\
   : (GET_MODE_CLASS (GET_MODE (X)) == MODE_INT && (CLASS) == FLOAT_REGS ? \
      GENERAL_REGS : (CLASS)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the 80386, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FLOAT_REGS ? 1 :		\
  (CLASS) == FP_TOP_REG ? 1 :		\
  (CLASS) == FP_SECOND_REG ? 1 :	\
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

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
   On 386 pushw decrements by exactly 2 no matter what the position was.
   On the 386 there is no pushb; we use pushw instead, and this
   has the effect of rounding up to 2.  */

#define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & (-2))

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 80386, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.  */

#define RETURN_POPS_ARGS(FUNTYPE,SIZE)   \
  (TREE_CODE (FUNTYPE) == IDENTIFIER_NODE ? 0			\
   : (TARGET_RTD						\
      && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
	  || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE)))	\
	      == void_type_node))) ? (SIZE)			\
   : (aggregate_value_p (FUNTYPE)) ? GET_MODE_SIZE (Pmode) : 0)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC)  \
   gen_rtx (REG, TYPE_MODE (VALTYPE), \
	    VALUE_REGNO (TYPE_MODE (VALTYPE)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE) \
  gen_rtx (REG, MODE, VALUE_REGNO (MODE))

/* 1 if N is a possible register number for function argument passing.
   On the 80386, no registers are used in this way.
      *NOTE* -mregparm does not work.
   It exists only to test register calling conventions.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the 80386, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the 80386, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
 ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3	\
	    : (int_size_in_bytes (TYPE) + 3) & ~3))

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


/* On the 80386 all args are pushed, except if -mregparm is specified
   then the first two words of arguments are passed in EAX, EDX.
   *NOTE* -mregparm does not work.
   It exists only to test register calling conventions.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8) ? gen_rtx (REG, (MODE), (CUM) / 4) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */


#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8					\
  && 8 < ((CUM) + ((MODE) == BLKmode				\
		      ? int_size_in_bytes (TYPE)		\
		      : GET_MODE_SIZE (MODE))))  		\
 ? 2 - (CUM) / 4 : 0)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
  function_prologue (FILE, SIZE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
{									\
  if (flag_pic)								\
    {									\
      fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%edx\n",		\
	       LPREFIX, (LABELNO));					\
      fprintf (FILE, "\tcall *_mcount@GOT(%%ebx)\n");			\
    }									\
  else									\
    {									\
      fprintf (FILE, "\tmovl $%sP%d,%%edx\n", LPREFIX, (LABELNO));	\
      fprintf (FILE, "\tcall _mcount\n");				\
    }									\
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */
/* Note on the 386 it might be more efficient not to define this since 
   we have to restore it ourselves from the frame pointer, in order to
   use pop */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.

   If the last non-note insn in the function is a BARRIER, then there
   is no need to emit a function prologue, because control does not fall
   off the end.  This happens if the function ends in an "exit" call, or
   if a `return' insn is emitted directly into the function. */

#define FUNCTION_EPILOGUE(FILE, SIZE) 		\
do {						\
  rtx last = get_last_insn ();			\
  if (last && GET_CODE (last) == NOTE)		\
    last = prev_nonnote_insn (last);		\
  if (! last || GET_CODE (last) != BARRIER)	\
    function_epilogue (FILE, SIZE);		\
} while (0)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the 386, the trampoline contains three instructions:
     mov #STATIC,ecx
     mov #FUNCTION,eax
     jmp @eax  */
#define TRAMPOLINE_TEMPLATE(FILE)			\
{							\
  ASM_OUTPUT_CHAR (FILE, GEN_INT (0xb9));		\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);			\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);			\
  ASM_OUTPUT_CHAR (FILE, GEN_INT (0xb8));		\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);			\
  ASM_OUTPUT_SHORT (FILE, const0_rtx);			\
  ASM_OUTPUT_CHAR (FILE, GEN_INT (0xff));		\
  ASM_OUTPUT_CHAR (FILE, GEN_INT (0xe0));		\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 12

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 1)), CXT); \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (TRAMP, 6)), FNADDR); \
}

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

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   For the i386, if frame pointer elimination is being done, we would like to
   convert ap into sp, not fp.

   All other eliminations are valid.  */

#define CAN_ELIMINATE(FROM, TO)					\
 ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM	\
  ? ! frame_pointer_needed					\
  : 1)

/* Define the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{									\
  if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)	\
    (OFFSET) = 8;	/* Skip saved PC and previous frame pointer */	\
  else									\
    {									\
      int regno;							\
      int offset = 0;							\
									\
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)		\
	if ((regs_ever_live[regno] && ! call_used_regs[regno])		\
	    || (current_function_uses_pic_offset_table			\
		&& regno == PIC_OFFSET_TABLE_REGNUM))			\
	  offset += 4;							\
									\
      (OFFSET) = offset + get_frame_size ();				\
									\
      if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	\
	(OFFSET) += 4;	/* Skip saved PC */				\
    }									\
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) < STACK_POINTER_REGNUM \
   || (unsigned) reg_renumber[REGNO] < STACK_POINTER_REGNUM)

#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) <= STACK_POINTER_REGNUM \
   || (REGNO) == ARG_POINTER_REGNUM \
   || (unsigned) reg_renumber[REGNO] <= STACK_POINTER_REGNUM)

#define REGNO_OK_FOR_SIREG_P(REGNO) ((REGNO) == 4 || reg_renumber[REGNO] == 4)
#define REGNO_OK_FOR_DIREG_P(REGNO) ((REGNO) == 5 || reg_renumber[REGNO] == 5)

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

/* Nonzero if X is a hard reg that can be used as an index or if
   it is a pseudo reg.  */

#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) < STACK_POINTER_REGNUM \
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Nonzero if X is a hard reg that can be used as a base reg
   of if it is a pseudo reg.  */
  /* ?wfs */

#define REG_OK_FOR_BASE_P(X) \
  (REGNO (X) <= STACK_POINTER_REGNUM \
   || REGNO (X) == ARG_POINTER_REGNUM \
   || REGNO(X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_STRREG_P(X) \
  (REGNO (X) == 4 || REGNO (X) == 5 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))
#define REG_OK_FOR_STRREG_P(X) \
  (REGNO_OK_FOR_DIREG_P (REGNO (X)) || REGNO_OK_FOR_SIREG_P (REGNO (X)))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is usually machine-independent.

   See legitimize_pic_address in i386.c for details as to what
   constitutes a legitimate address when -fpic is used.  */

#define MAX_REGS_PER_ADDRESS 2

#define CONSTANT_ADDRESS_P(X)   CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

#define GO_IF_INDEXABLE_BASE(X, ADDR)	\
 if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X)) goto ADDR

#define LEGITIMATE_INDEX_REG_P(X)   \
  (GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))

/* Return 1 if X is an index or an index times a scale.  */

#define LEGITIMATE_INDEX_P(X)   \
   (LEGITIMATE_INDEX_REG_P (X)				\
    || (GET_CODE (X) == MULT				\
	&& LEGITIMATE_INDEX_REG_P (XEXP (X, 0))		\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& (INTVAL (XEXP (X, 1)) == 2			\
	    || INTVAL (XEXP (X, 1)) == 4		\
	    || INTVAL (XEXP (X, 1)) == 8)))

/* Go to ADDR if X is an index term, a base reg, or a sum of those.  */

#define GO_IF_INDEXING(X, ADDR)	\
{ if (LEGITIMATE_INDEX_P (X)) goto ADDR;				\
  GO_IF_INDEXABLE_BASE (X, ADDR);					\
  if (GET_CODE (X) == PLUS && LEGITIMATE_INDEX_P (XEXP (X, 0)))		\
    { GO_IF_INDEXABLE_BASE (XEXP (X, 1), ADDR); }			\
  if (GET_CODE (X) == PLUS && LEGITIMATE_INDEX_P (XEXP (X, 1)))		\
    { GO_IF_INDEXABLE_BASE (XEXP (X, 0), ADDR); } }

/* We used to allow this, but it isn't ever used.
   || ((GET_CODE (X) == POST_DEC || GET_CODE (X) == POST_INC)		\
       && REG_P (XEXP (X, 0))						\
       && REG_OK_FOR_STRREG_P (XEXP (X, 0)))				\
*/

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)	\
{									\
  if (CONSTANT_ADDRESS_P (X)						\
      && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (X)))			\
    goto ADDR;								\
  GO_IF_INDEXING (X, ADDR);						\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))		\
    {									\
      rtx x0 = XEXP (X, 0);						\
      if (! flag_pic || ! SYMBOLIC_CONST (XEXP (X, 1)))			\
	{ GO_IF_INDEXING (x0, ADDR); }					\
      else if (x0 == pic_offset_table_rtx)				\
	goto ADDR;							\
      else if (GET_CODE (x0) == PLUS)					\
	{								\
	  if (XEXP (x0, 0) == pic_offset_table_rtx)			\
	    { GO_IF_INDEXABLE_BASE (XEXP (x0, 1), ADDR); }		\
	  if (XEXP (x0, 1) == pic_offset_table_rtx)			\
	    { GO_IF_INDEXABLE_BASE (XEXP (x0, 0), ADDR); }		\
	}								\
    }									\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 80386, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in a general reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in a general reg.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address in i386.c for details.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)   \
{ extern rtx legitimize_pic_address ();					\
  int ch = (X) != (OLDX);						\
  if (flag_pic && SYMBOLIC_CONST (X))					\
    {									\
      (X) = legitimize_pic_address (X, 0);				\
      if (memory_address_p (MODE, X))					\
	goto WIN;							\
    }									\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 0)) == MULT)				\
	ch = 1, XEXP (X, 0) = force_operand (XEXP (X, 0), 0);		\
      if (GET_CODE (XEXP (X, 1)) == MULT)				\
	ch = 1, XEXP (X, 1) = force_operand (XEXP (X, 1), 0);		\
      if (ch && GET_CODE (XEXP (X, 1)) == REG				\
	  && GET_CODE (XEXP (X, 0)) == REG)				\
	goto WIN;							\
      if (flag_pic && SYMBOLIC_CONST (XEXP (X, 1)))			\
        ch = 1, (X) = legitimize_pic_address (X, 0);			\
      if (ch) { GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN); }		\
      if (GET_CODE (XEXP (X, 0)) == REG)                                \
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 1), temp);		\
	  if (val != temp) emit_move_insn (temp, val);			\
	  XEXP (X, 1) = temp;						\
	  goto WIN; }							\
      else if (GET_CODE (XEXP (X, 1)) == REG)				\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 0), temp);		\
	  if (val != temp) emit_move_insn (temp, val);			\
	  XEXP (X, 0) = temp;						\
	  goto WIN; }}}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and 
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_PIC_OPERAND_P(X) \
  (! SYMBOLIC_CONST (X)							\
   || (GET_CODE (X) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (X)))

#define SYMBOLIC_CONST(X)	\
(GET_CODE (X) == SYMBOL_REF						\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 80386, only postdecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == POST_DEC) goto LABEL

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On i386, if using PIC, mark a SYMBOL_REF for a non-global symbol
   so that we may access it directly in the GOT.  */

#define ENCODE_SECTION_INFO(DECL) \
do									\
  {									\
    if (flag_pic)							\
      {									\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		   ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));		\
	SYMBOL_REF_FLAG (XEXP (rtl, 0))					\
	  = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'			\
	     || ! TREE_PUBLIC (DECL));					\
      }									\
  }									\
while (0)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.
   This should be changed to take advantage of fist --wfs ??
 */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* MOVE_RATIO is the number of move instructions that is better than a
   block move.  Make this large on i386, since the block move is very
   inefficient with small blocks, and the hard register needs of the
   block move require much reload work. */
#define MOVE_RATIO 5

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* One i386, shifts do truncate the count.  But bit opcodes don't. */

/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.
   (The 386 can't easily push less than an int.)  */

#define PROMOTE_PROTOTYPES

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on the 386 because a CALL with a constant address is
   not much slower than one with a register address.  */
#define NO_FUNCTION_CSE

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. */

#define RTX_COSTS(X,CODE,OUTER_CODE)			\
  case MULT:						\
    return COSTS_N_INSNS (10);				\
  case DIV:						\
  case UDIV:						\
  case MOD:						\
  case UMOD:						\
    return COSTS_N_INSNS (40);				\
  case PLUS:						\
    if (GET_CODE (XEXP (X, 0)) == REG			\
        && GET_CODE (XEXP (X, 1)) == CONST_INT)		\
      return 1;						\
    break;


/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return flag_pic && SYMBOLIC_CONST (RTX) ? 2 : 0;		\
  case CONST_DOUBLE:						\
    {								\
      int code = standard_80387_constant_p (RTX);		\
      return code == 1 ? 0 :					\
	     code == 2 ? 1 :					\
			 2;					\
    }

/* Compute the cost of an address.  This is meant to approximate the size
   and/or execution delay of an insn using that address.  If the cost is
   approximated by the RTL complexity, including CONST_COSTS above, as
   is usually the case for CISC machines, this macro should not be defined.
   For aggressively RISCy machines, only one insn format is allowed, so
   this macro should be a constant.  The value of this macro only matters
   for valid addresses.

   For i386, it is better to use a complex address than let gcc copy
   the address into a reg and make a new pseudo.  But not if the address
   requires to two regs - that would mean more pseudos with longer
   lifetimes.  */

#define ADDRESS_COST(RTX) \
  ((CONSTANT_P (RTX)						\
    || (GET_CODE (RTX) == PLUS && CONSTANT_P (XEXP (RTX, 1))	\
	&& REG_P (XEXP (RTX, 0)))) ? 0				\
   : REG_P (RTX) ? 1						\
   : 2)

/* Add any extra modes needed to represent the condition code.

   For the i386, we need separate modes when floating-point equality
   comparisons are being done.  */

#define EXTRA_CC_MODES CCFPEQmode

/* Define the names for the modes specified above.  */
#define EXTRA_CC_NAMES "CCFPEQ"

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.

   For floating-point equality comparisons, CCFPEQmode should be used.
   VOIDmode should be used in all other cases.  */

#define SELECT_CC_MODE(OP,X,Y) \
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT			\
   && ((OP) == EQ || (OP) == NE) ? CCFPEQmode : CCmode)

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

extern struct rtx_def *i386_compare_op0, *i386_compare_op1;
extern struct rtx_def *(*i386_compare_gen)(), *(*i386_compare_gen_eq)();

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Set if the cc value is actually in the 80387, so a floating point
   conditional branch must be output.  */
#define CC_IN_80387 04000

/* Set if the CC value was stored in a nonstandard way, so that
   the state of equality is indicated by zero in the carry bit.  */
#define CC_Z_IN_NOT_C 010000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
  notice_update_cc((EXP))

/* Output a signed jump insn.  Use template NORMAL ordinarily, or
   FLOAT following a floating point comparison.
   Use NO_OV following an arithmetic insn that set the cc's
   before a test insn that was deleted.
   NO_OV may be zero, meaning final should reinsert the test insn
   because the jump cannot be handled properly without it.  */

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)			\
{								\
  if (cc_prev_status.flags & CC_IN_80387)			\
    return FLOAT;						\
  if (cc_prev_status.flags & CC_NO_OVERFLOW)			\
    return NO_OV;						\
  return NORMAL;						\
}

/* Control the assembler format that we output, to the extent
   this does not vary between assemblers.  */

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above). */

/* In order to refer to the first 8 regs as 32 bit regs prefix an "e"
   For non floating point regs, the following are the HImode names.

   For float regs, the stack top is sometimes referred to as "%st(0)"
   instead of just "%st".  PRINT_REG handles this with the "y" code.  */

#define HI_REGISTER_NAMES \
{"ax","dx","cx","bx","si","di","bp","sp",          \
 "st","st(1)","st(2)","st(3)","st(4)","st(5)","st(6)","st(7)","" }

#define REGISTER_NAMES HI_REGISTER_NAMES

/* Table of additional register names to use in user input.  */

#define ADDITIONAL_REGISTER_NAMES \
{ "eax", 0, "edx", 1, "ecx", 2, "ebx", 3,	\
  "esi", 4, "edi", 5, "ebp", 6, "esp", 7,	\
  "al", 0, "dl", 1, "cl", 2, "bl", 3,		\
  "ah", 0, "dh", 1, "ch", 2, "bh", 3 }

/* Note we are omitting these since currently I don't know how
to get gcc to use these, since they want the same but different
number as al, and ax.
*/

/* note the last four are not really qi_registers, but
   the md will have to never output movb into one of them
   only a movw .  There is no movb into the last four regs */

#define QI_REGISTER_NAMES \
{"al", "dl", "cl", "bl", "si", "di", "bp", "sp",}

/* These parallel the array above, and can be used to access bits 8:15
   of regs 0 through 3. */

#define QI_HIGH_REGISTER_NAMES \
{"ah", "dh", "ch", "bh", }

/* How to renumber registers for dbx and gdb.  */

/* {0,2,1,3,6,7,4,5,12,13,14,15,16,17}  */
#define DBX_REGISTER_NUMBER(n) \
((n) == 0 ? 0 : \
 (n) == 1 ? 2 : \
 (n) == 2 ? 1 : \
 (n) == 3 ? 3 : \
 (n) == 4 ? 6 : \
 (n) == 5 ? 7 : \
 (n) == 6 ? 4 : \
 (n) == 7 ? 5 : \
 (n) + 4)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  (assemble_name (FILE, NAME), fputs (":\n", FILE))

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "%s %.22e\n", ASM_DOUBLE, (VALUE))


/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { union { float f; long l;} tem;			\
     tem.f = (VALUE);					\
     fprintf((FILE), "%s 0x%x\n", ASM_LONG, tem.l);	\
   } while (0)


/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))



/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "%s ", ASM_LONG),		\
  output_addr_const (FILE,(VALUE)),		\
  putc('\n',FILE))

/* Likewise for `char' and `short' constants.  */
/* is this supposed to do align too?? */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "%s ", ASM_SHORT),		\
  output_addr_const (FILE,(VALUE)),		\
  putc('\n',FILE))

/*
#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "%s ", ASM_BYTE_OP),		\
  output_addr_const (FILE,(VALUE)),		\
  fputs (",", FILE),		      		\
  output_addr_const (FILE,(VALUE)),		\
  fputs (" >> 8\n",FILE))
*/


#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "%s ", ASM_BYTE_OP),		\
  output_addr_const (FILE, (VALUE)),		\
  putc ('\n', FILE))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf ((FILE), "%s 0x%x\n", ASM_BYTE_OP, (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tpushl e%s\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tpopl e%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
     */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "%s %s%d\n", ASM_LONG, LPREFIX, VALUE)

/* This is how to output an element of a case-vector that is relative.
   We don't use these on the 386 yet, because the ATT assembler can't do
   forward reference the differences.  
 */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
  fprintf (FILE, "\t.word %s%d-%s%d\n",LPREFIX, VALUE,LPREFIX, REL)

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN ""
#define ASM_CLOSE_PAREN ""

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
   The CODE z takes the size of operand from the following digit, and
   outputs b,w,or l respectively.

   On the 80386, we use several such letters:
   f -- float insn (print a CONST_DOUBLE as a float rather than in hex).
   L,W,B,Q,S -- print the opcode suffix for specified size of operand.
   R -- print the prefix for register names.
   z -- print the opcode suffix for the size of the current operand.
   * -- print a star (in certain assembler syntax)
   w -- print the operand as if it's a "word" (HImode) even if it isn't.
   b -- print the operand as if it's a byte (QImode) even if it isn't.
   c -- don't print special prefixes before constant operands.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '*')

/* Print the name of a register based on its machine mode and number.
   If CODE is 'w', pretend the mode is HImode.
   If CODE is 'b', pretend the mode is QImode.
   If CODE is 'k', pretend the mode is SImode.
   If CODE is 'h', pretend the reg is the `high' byte register.
   If CODE is 'y', print "st(0)" instead of "st", if the reg is stack op. */

extern char *hi_reg_name[];
extern char *qi_reg_name[];
extern char *qi_high_reg_name[];

#define PRINT_REG(X, CODE, FILE) \
  do { if (REGNO (X) == ARG_POINTER_REGNUM)		\
	 abort ();					\
       fprintf (FILE, "%s", RP);			\
       switch ((CODE == 'w' ? 2 			\
		: CODE == 'b' ? 1			\
		: CODE == 'k' ? 4			\
		: CODE == 'y' ? 3			\
		: CODE == 'h' ? 0			\
		: GET_MODE_SIZE (GET_MODE (X))))	\
	 {						\
	 case 3:					\
	   if (STACK_TOP_P (X))				\
	     {						\
	       fputs ("st(0)", FILE);			\
	       break;					\
	     }						\
	 case 4:					\
	 case 8:					\
	   if (! FP_REG_P (X)) fputs ("e", FILE);	\
	 case 2:					\
	   fputs (hi_reg_name[REGNO (X)], FILE);	\
	   break;					\
	 case 1:					\
	   fputs (qi_reg_name[REGNO (X)], FILE);	\
	   break;					\
	 case 0:					\
	   fputs (qi_high_reg_name[REGNO (X)], FILE);	\
	   break;					\
	 }						\
     } while (0)

#define PRINT_OPERAND(FILE, X, CODE)  \
  print_operand (FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
  print_operand_address (FILE, ADDR)

/* Print the name of a register for based on its machine mode and number.
   This macro is used to print debugging output.
   This macro is different from PRINT_REG in that it may be used in
   programs that are not linked with aux-output.o.  */

#define DEBUG_PRINT_REG(X, CODE, FILE) \
  do { static char *hi_name[] = HI_REGISTER_NAMES;	\
       static char *qi_name[] = QI_REGISTER_NAMES;	\
       fprintf (FILE, "%s", RP);			\
       if (REGNO (X) == ARG_POINTER_REGNUM)		\
	 { fputs ("argp", FILE); break; }		\
       if (STACK_TOP_P (X))				\
	 { fputs ("st(0)", FILE); break; }		\
       switch (GET_MODE_SIZE (GET_MODE (X)))		\
	 {						\
	 case 8:					\
	 case 4:					\
	   if (! FP_REG_P (X)) fputs ("e", FILE);	\
	 case 2:					\
	   fputs (hi_name[REGNO (X)], FILE);		\
	   break;					\
	 case 1:					\
	   fputs (qi_name[REGNO (X)], FILE);		\
	   break;					\
	 }						\
     } while (0)

/* Output the prefix for an immediate operand, or for an offset operand.  */
#define PRINT_IMMED_PREFIX(FILE)  fputs (IP, (FILE))
#define PRINT_OFFSET_PREFIX(FILE)  fputs (IP, (FILE))

/* Routines in libgcc that return floats must return them in an fp reg,
   just as other functions do which return such values.
   These macros make that happen.  */

#define FLOAT_VALUE_TYPE float
#define INTIFY(FLOATVAL) FLOATVAL

/* Nonzero if INSN magically clobbers register REGNO.  */

/* #define INSN_CLOBBERS_REGNO_P(INSN, REGNO)	\
    (FP_REGNO_P (REGNO)				\
     && (GET_CODE (INSN) == JUMP_INSN || GET_CODE (INSN) == BARRIER))
*/

/* a letter which is not needed by the normal asm syntax, which
   we can use for operand syntax in the extended asm */

#define ASM_OPERAND_LETTER '#'

#define RET return ""
#define AT_SP(mode) (gen_rtx (MEM, (mode), stack_pointer_rtx))

/*
Local variables:
version-control: t
End:
*/
