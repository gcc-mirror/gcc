/* Definitions of target machine for GNU compiler for Intel X86
   (386, 486, Pentium).
   Copyright (C) 1988, 1992, 1994, 1995 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


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

/* configure can arrange to make this 2, to force a 486.  */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

/* Masks for the -m switches */
#define MASK_80387		000000000001	/* Hardware floating point */
#define MASK_486		000000000002	/* 80486 specific */
#define MASK_NOTUSED1		000000000004	/* bit not currently used */
#define MASK_RTD		000000000010	/* Use ret that pops args */
#define MASK_ALIGN_DOUBLE	000000000020	/* align doubles to 2 word boundary */
#define MASK_SVR3_SHLIB		000000000040	/* Uninit locals into bss */
#define MASK_IEEE_FP		000000000100	/* IEEE fp comparisons */
#define MASK_FLOAT_RETURNS	000000000200	/* Return float in st(0) */
#define MASK_NO_FANCY_MATH_387	000000000400	/* Disable sin, cos, sqrt */

						/* Temporary codegen switches */
#define MASK_DEBUG_ADDR		000001000000	/* Debug GO_IF_LEGITIMATE_ADDRESS */
#define MASK_NO_WIDE_MULTIPLY	000002000000	/* Disable 32x32->64 multiplies */
#define MASK_NO_MOVE		000004000000	/* Don't generate mem->mem */
#define MASK_DEBUG_ARG		000010000000	/* Debug function_arg */   

/* Use the floating point instructions */
#define TARGET_80387 (target_flags & MASK_80387)

/* Compile using ret insn that pops args.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.  */  
#define TARGET_RTD (target_flags & MASK_RTD)

/* Align doubles to a two word boundary.  This breaks compatibility with
   the published ABI's for structures containing doubles, but produces
   faster code on the pentium.  */
#define TARGET_ALIGN_DOUBLE (target_flags & MASK_ALIGN_DOUBLE)

/* Put uninitialized locals into bss, not data.
   Meaningful only on svr3.  */
#define TARGET_SVR3_SHLIB (target_flags & MASK_SVR3_SHLIB)

/* Use IEEE floating point comparisons.  These handle correctly the cases
   where the result of a comparison is unordered.  Normally SIGFPE is
   generated in such cases, in which case this isn't needed.  */
#define TARGET_IEEE_FP (target_flags & MASK_IEEE_FP)

/* Functions that return a floating point value may return that value
   in the 387 FPU or in 386 integer registers.  If set, this flag causes
   the 387 to be used, which is compatible with most calling conventions. */
#define TARGET_FLOAT_RETURNS_IN_80387 (target_flags & MASK_FLOAT_RETURNS)

/* Disable generation of FP sin, cos and sqrt operations for 387.
   This is because FreeBSD lacks these in the math-emulator-code */
#define TARGET_NO_FANCY_MATH_387 (target_flags & MASK_NO_FANCY_MATH_387)

/* Temporary switches for tuning code generation */

/* Disable 32x32->64 bit multiplies that are used for long long multiplies
   and division by constants, but sometimes cause reload problems.  */
#define TARGET_NO_WIDE_MULTIPLY (target_flags & MASK_NO_WIDE_MULTIPLY)
#define TARGET_WIDE_MULTIPLY (!TARGET_NO_WIDE_MULTIPLY)

/* Debug GO_IF_LEGITIMATE_ADDRESS */
#define TARGET_DEBUG_ADDR (target_flags & MASK_DEBUG_ADDR)

/* Debug FUNCTION_ARG macros */
#define TARGET_DEBUG_ARG (target_flags & MASK_DEBUG_ARG)

/* Hack macros for tuning code generation */
#define TARGET_MOVE	((target_flags & MASK_NO_MOVE) == 0)	/* Don't generate memory->memory */

/* Specific hardware switches */
#define TARGET_486	(target_flags & MASK_486)	/* 80486DX, 80486SX, 80486DX[24] */
#define TARGET_386	(!TARGET_486) 			/* 80386 */

#define TARGET_SWITCHES							\
{ { "80387",			 MASK_80387 },				\
  { "no-80387",			-MASK_80387 },				\
  { "hard-float",		 MASK_80387 },				\
  { "soft-float",		-MASK_80387 },				\
  { "no-soft-float",		 MASK_80387 },				\
  { "386",			-MASK_486 },				\
  { "no-386",			 MASK_486 },				\
  { "486",			 MASK_486 },				\
  { "no-486",			-MASK_486 },				\
  { "rtd",			 MASK_RTD },				\
  { "no-rtd",			-MASK_RTD },				\
  { "align-double",		 MASK_ALIGN_DOUBLE },			\
  { "no-align-double",		-MASK_ALIGN_DOUBLE },			\
  { "svr3-shlib",		 MASK_SVR3_SHLIB },			\
  { "no-svr3-shlib",		-MASK_SVR3_SHLIB },			\
  { "ieee-fp",			 MASK_IEEE_FP },			\
  { "no-ieee-fp",		-MASK_IEEE_FP },			\
  { "fp-ret-in-387",		 MASK_FLOAT_RETURNS },			\
  { "no-fp-ret-in-387",		-MASK_FLOAT_RETURNS },			\
  { "no-fancy-math-387",	 MASK_NO_FANCY_MATH_387 },		\
  { "fancy-math-387",		-MASK_NO_FANCY_MATH_387 },		\
  { "no-wide-multiply",		 MASK_NO_WIDE_MULTIPLY },		\
  { "wide-multiply",		-MASK_NO_WIDE_MULTIPLY },		\
  { "debug-addr",		 MASK_DEBUG_ADDR },			\
  { "no-debug-addr",		-MASK_DEBUG_ADDR },			\
  { "move",			-MASK_NO_MOVE },			\
  { "no-move",			 MASK_NO_MOVE },			\
  { "debug-arg",		 MASK_DEBUG_ARG },			\
  { "no-debug-arg",		-MASK_DEBUG_ARG },			\
  SUBTARGET_SWITCHES							\
  { "", TARGET_DEFAULT | TARGET_CPU_DEFAULT}}

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable.  The
   variable, type `char *', is set to the variable part of the given
   option if the fixed part matches.  The actual option name is made
   by appending `-m' to the specified name.  */
#define TARGET_OPTIONS							\
{ { "reg-alloc=",	&i386_reg_alloc_order },			\
  { "regparm=",		&i386_regparm_string },				\
  { "align-loops=",	&i386_align_loops_string },			\
  { "align-jumps=",	&i386_align_jumps_string },			\
  { "align-functions=",	&i386_align_funcs_string },			\
  SUBTARGET_OPTIONS							\
}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

#define OVERRIDE_OPTIONS override_options ()

/* These are meant to be redefined in the host dependent files */
#define SUBTARGET_SWITCHES
#define SUBTARGET_OPTIONS


/* target machine storage layout */

/* Define for XFmode extended real floating point support.
   This will automatically cause REAL_ARITHMETIC to be defined.  */
#define LONG_DOUBLE_TYPE_SIZE 96

/* Define if you don't want extended real, but do want to use the
   software floating point emulator for REAL_ARITHMETIC and
   decimal <-> binary conversion. */
/* #define REAL_ARITHMETIC */

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
#define FUNCTION_BOUNDARY (1 << (i386_align_funcs + 3))

/* Alignment of field after `int : 0' in a structure. */

#define EMPTY_FIELD_BOUNDARY 32

/* Minimum size in bits of the largest boundary to which any
   and all fundamental data types supported by the hardware
   might need to be aligned. No data type wants to be aligned
   rounder than this.  The i386 supports 64-bit floating point
   quantities, but these can be aligned on any 32-bit boundary.
   The published ABIs say that doubles should be aligned on word
   boundaries, but the Pentium gets better performance with them
   aligned on 64 bit boundaries. */
#define BIGGEST_ALIGNMENT (TARGET_ALIGN_DOUBLE ? 64 : 32)

/* Set this non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 0

/* If bit field type is int, don't let it cross an int,
   and give entire struct the alignment of an int.  */
/* Required on the 386 since it doesn't have bitfield insns.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Maximum power of 2 that code can be aligned to.  */
#define MAX_CODE_ALIGN	6			/* 64 byte alignment */

/* Align loop starts for optimal branching.  */
#define ASM_OUTPUT_LOOP_ALIGN(FILE) ASM_OUTPUT_ALIGN (FILE, i386_align_loops)

/* This is how to align an instruction for optimal branching.
   On i486 we'll get better performance by aligning on a
   cache line (i.e. 16 byte) boundary.  */
#define ASM_OUTPUT_ALIGN_CODE(FILE) ASM_OUTPUT_ALIGN ((FILE), i386_align_jumps)


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

/* Order in which to allocate registers.  Each register must be
   listed once, even those in FIXED_REGISTERS.  List frame pointer
   late and fixed registers last.  Note that, in general, we prefer
   registers listed in CALL_USED_REGISTERS, keeping the others
   available for storage of persistent values.

   Three different versions of REG_ALLOC_ORDER have been tried:

   If the order is edx, ecx, eax, ... it produces a slightly faster compiler,
   but slower code on simple functions returning values in eax.

   If the order is eax, ecx, edx, ... it causes reload to abort when compiling
   perl 4.036 due to not being able to create a DImode register (to hold a 2
   word union).

   If the order is eax, edx, ecx, ... it produces better code for simple
   functions, and a slightly slower compiler.  Users complained about the code
   generated by allocating edx first, so restore the 'natural' order of things. */

#define REG_ALLOC_ORDER \
/*ax,dx,cx,bx,si,di,bp,sp,st,st1,st2,st3,st4,st5,st6,st7,arg*/ \
{  0, 1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16 }

/* A C statement (sans semicolon) to choose the order in which to
   allocate hard registers for pseudo-registers local to a basic
   block.

   Store the desired register order in the array `reg_alloc_order'.
   Element 0 should be the register to allocate first; element 1, the
   next register; and so on.

   The macro body should not assume anything about the contents of
   `reg_alloc_order' before execution of the macro.

   On most machines, it is not necessary to define this macro.  */

#define ORDER_REGS_FOR_LOCAL_ALLOC order_regs_for_local_alloc ()

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

/* The casts to int placate a compiler on a microvax,
   for cross-compiler testing.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((REGNO) < 2 ? 1						\
   : (REGNO) < 4 ? 1						\
   : FP_REGNO_P (REGNO)						\
   ? (((int) GET_MODE_CLASS (MODE) == (int) MODE_FLOAT		\
       || (int) GET_MODE_CLASS (MODE) == (int) MODE_COMPLEX_FLOAT)	\
      && GET_MODE_UNIT_SIZE (MODE) <= 12)			\
   : (int) (MODE) != (int) QImode)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2) ((MODE1) == (MODE2))

/* A C expression returning the cost of moving data from a register of class
   CLASS1 to one of CLASS2.

   On the i386, copying between floating-point and fixed-point
   registers is expensive.  */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)			\
  (((FLOAT_CLASS_P (CLASS1) && ! FLOAT_CLASS_P (CLASS2))		\
    || (! FLOAT_CLASS_P (CLASS1) && FLOAT_CLASS_P (CLASS2))) ? 10	\
   : 2)

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

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value
   says to return the function value in memory, just as large
   structures are always returned.  Here TYPE will be a C expression
   of type `tree', representing the data type of the value.

   Note that values of mode `BLKmode' must be explicitly handled by
   this macro.  Also, the option `-fpcc-struct-return' takes effect
   regardless of this macro.  On most systems, it is possible to
   leave the macro undefined; this causes a default definition to be
   used, whose value is the constant 1 for `BLKmode' values, and 0
   otherwise.

   Do not use this macro to indicate that structures and unions
   should always be returned in memory.  You should instead use
   `DEFAULT_PCC_STRUCT_RETURN' to indicate this.  */

#define RETURN_IN_MEMORY(TYPE) \
  ((TYPE_MODE (TYPE) == BLKmode) || int_size_in_bytes (TYPE) > 12)


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
  AD_REGS,			/* %eax/%edx for DImode */
  Q_REGS,			/* %eax %ebx %ecx %edx */
  SIREG, DIREG,
  INDEX_REGS,			/* %eax %ebx %ecx %edx %esi %edi %ebp */
  GENERAL_REGS,			/* %eax %ebx %ecx %edx %esi %edi %ebp %esp */
  FP_TOP_REG, FP_SECOND_REG,	/* %st(0) %st(1) */
  FLOAT_REGS,
  ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define FLOAT_CLASS_P(CLASS) (reg_class_subset_p (CLASS, FLOAT_REGS))

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
{  "NO_REGS",				\
   "AREG", "DREG", "CREG", "BREG",	\
   "AD_REGS",				\
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
     0x3,			/* AD_REGS */			\
     0xf,			/* Q_REGS */			\
    0x10,   0x20,		/* SIREG, DIREG */		\
 0x07f,				/* INDEX_REGS */		\
 0x100ff,			/* GENERAL_REGS */		\
  0x0100, 0x0200,		/* FP_TOP_REG, FP_SECOND_REG */	\
  0xff00,			/* FLOAT_REGS */		\
 0x1ffff }

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

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
   (C) == 'A' ? AD_REGS :					\
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
   (C) == 'N' ? (VALUE) >= 0 && (VALUE) <= 255 :\
   0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  We allow constants even if
   TARGET_387 isn't set, because the stack register converter may need to
   load 0.0 into the function value register. */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? standard_80387_constant_p (VALUE) : 0)

/* Place additional restrictions on the register class to use when it
   is necessary to be able to hold a value of mode MODE in a reload
   register for which class CLASS would ordinarily be used. */

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

/* Put float CONST_DOUBLE in the constant pool instead of fp regs.
   QImode must go into class Q_REGS.
   Narrow ALL_REGS to GENERAL_REGS.  This supports allowing movsf and
   movdf to do mem-to-mem moves through integer regs. */

#define PREFERRED_RELOAD_CLASS(X,CLASS)	\
  (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != VOIDmode ? NO_REGS	\
   : GET_MODE (X) == QImode && ! reg_class_subset_p (CLASS, Q_REGS) ? Q_REGS \
   : ((CLASS) == ALL_REGS						\
      && GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT) ? GENERAL_REGS	\
   : (CLASS))

/* If we are copying between general and FP registers, we need a memory
   location.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1,CLASS2,MODE) \
  ((FLOAT_CLASS_P (CLASS1) && ! FLOAT_CLASS_P (CLASS2))	\
   || (! FLOAT_CLASS_P (CLASS1) && FLOAT_CLASS_P (CLASS2)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the 80386, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 (FLOAT_CLASS_P (CLASS) ? 1 :		\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* A C expression whose value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one
   register and zero otherwise.  On most machines, this default
   should be used.  Only define this macro to some other expression
   if pseudo allocated by `local-alloc.c' end up in memory because
   their hard registers were needed for spill registers.  If this
   macro returns nonzero for those classes, those pseudos will only
   be allocated by `global.c', which knows how to reallocate the
   pseudo to another register.  If there would not be another
   register available for reallocation, you should not change the
   definition of this macro since the only effect of such a
   definition would be to slow down register allocation.  */

#define CLASS_LIKELY_SPILLED_P(CLASS)					\
  (((CLASS) == AREG)							\
   || ((CLASS) == DREG)							\
   || ((CLASS) == CREG)							\
   || ((CLASS) == BREG)							\
   || ((CLASS) == AD_REGS)						\
   || ((CLASS) == SIREG)						\
   || ((CLASS) == DIREG))


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
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 80386, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RTD on a per module basis.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  (i386_return_pops_args (FUNDECL, FUNTYPE, SIZE))

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

/* Define the size of the result block used for communication between
   untyped_call and untyped_return.  The block contains a DImode value
   followed by the block used by fnsave and frstor.  */

#define APPLY_RESULT_SIZE (8+108)

/* 1 if N is a possible register number for function argument passing.  */
#define FUNCTION_ARG_REGNO_P(N) ((N) >= 0 && (N) < REGPARM_MAX)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.  */

typedef struct i386_args {
  int words;			/* # words passed so far */
  int nregs;			/* # registers available for passing */
  int regno;			/* next available register number */
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)	\
  (init_cumulative_args (&CUM, FNTYPE, LIBNAME))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (function_arg_advance (&CUM, MODE, TYPE, NAMED))

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
  (function_arg (&CUM, MODE, TYPE, NAMED))

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  (function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED))

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

/* A C statement or compound statement to output to FILE some
   assembler code to initialize basic-block profiling for the current
   object module.  This code should call the subroutine
   `__bb_init_func' once per object module, passing it as its sole
   argument the address of a block allocated in the object module.

   The name of the block is a local symbol made with this statement:

	ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

   Of course, since you are writing the definition of
   `ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
   can take a short cut in the definition of this macro and use the
   name that you know will result.

   The first word of this block is a flag which will be nonzero if the
   object module has already been initialized.  So test this word
   first, and do not call `__bb_init_func' if the flag is nonzero.  */

#undef	FUNCTION_BLOCK_PROFILER
#define FUNCTION_BLOCK_PROFILER(STREAM, LABELNO)			\
do									\
  {									\
    static int num_func = 0;						\
    rtx xops[8];							\
    char block_table[80], false_label[80];				\
									\
    ASM_GENERATE_INTERNAL_LABEL (block_table, "LPBX", 0);		\
    ASM_GENERATE_INTERNAL_LABEL (false_label, "LPBZ", num_func);	\
									\
    xops[0] = const0_rtx;						\
    xops[1] = gen_rtx (SYMBOL_REF, VOIDmode, block_table);		\
    xops[2] = gen_rtx (MEM, Pmode, gen_rtx (SYMBOL_REF, VOIDmode, false_label)); \
    xops[3] = gen_rtx (MEM, Pmode, gen_rtx (SYMBOL_REF, VOIDmode, "__bb_init_func")); \
    xops[4] = gen_rtx (MEM, Pmode, xops[1]);				\
    xops[5] = stack_pointer_rtx;					\
    xops[6] = GEN_INT (4);						\
    xops[7] = gen_rtx (REG, Pmode, 0);	/* eax */			\
									\
    CONSTANT_POOL_ADDRESS_P (xops[1]) = TRUE;				\
    CONSTANT_POOL_ADDRESS_P (xops[2]) = TRUE;				\
									\
    output_asm_insn (AS2(cmp%L4,%0,%4), xops);				\
    output_asm_insn (AS1(jne,%2), xops);				\
									\
    if (!flag_pic)							\
      output_asm_insn (AS1(push%L1,%1), xops);				\
    else								\
      {									\
	output_asm_insn (AS2 (lea%L7,%a1,%7), xops);			\
	output_asm_insn (AS1 (push%L7,%7), xops);			\
      }									\
									\
    output_asm_insn (AS1(call,%P3), xops);				\
    output_asm_insn (AS2(add%L0,%6,%5), xops);				\
    ASM_OUTPUT_INTERNAL_LABEL (STREAM, "LPBZ", num_func);		\
    num_func++;								\
  }									\
while (0)


/* A C statement or compound statement to increment the count
   associated with the basic block number BLOCKNO.  Basic blocks are
   numbered separately from zero within each compilation.  The count
   associated with block number BLOCKNO is at index BLOCKNO in a
   vector of words; the name of this array is a local symbol made
   with this statement:

	ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 2);

   Of course, since you are writing the definition of
   `ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
   can take a short cut in the definition of this macro and use the
   name that you know will result.  */

#define BLOCK_PROFILER(STREAM, BLOCKNO)					\
do									\
  {									\
    rtx xops[1], cnt_rtx;						\
    char counts[80];							\
									\
    ASM_GENERATE_INTERNAL_LABEL (counts, "LPBX", 2);			\
    cnt_rtx = gen_rtx (SYMBOL_REF, VOIDmode, counts);			\
    SYMBOL_REF_FLAG (cnt_rtx) = TRUE;					\
									\
    if (BLOCKNO)							\
      cnt_rtx = plus_constant (cnt_rtx, (BLOCKNO)*4);			\
									\
    if (flag_pic)							\
      cnt_rtx = gen_rtx (PLUS, Pmode, pic_offset_table_rtx, cnt_rtx);	\
									\
    xops[0] = gen_rtx (MEM, SImode, cnt_rtx);				\
    output_asm_insn (AS1(inc%L0,%0), xops);				\
  }									\
while (0)

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


/* Non strict versions, pseudos are ok */
#define REG_OK_FOR_INDEX_NONSTRICT_P(X)					\
  (REGNO (X) < STACK_POINTER_REGNUM					\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_BASE_NONSTRICT_P(X)					\
  (REGNO (X) <= STACK_POINTER_REGNUM					\
   || REGNO (X) == ARG_POINTER_REGNUM					\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER)

#define REG_OK_FOR_STRREG_NONSTRICT_P(X)				\
  (REGNO (X) == 4 || REGNO (X) == 5 || REGNO (X) >= FIRST_PSEUDO_REGISTER)

/* Strict versions, hard registers only */
#define REG_OK_FOR_INDEX_STRICT_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_STRICT_P(X)  REGNO_OK_FOR_BASE_P (REGNO (X))
#define REG_OK_FOR_STRREG_STRICT_P(X)					\
  (REGNO_OK_FOR_DIREG_P (REGNO (X)) || REGNO_OK_FOR_SIREG_P (REGNO (X)))

#ifndef REG_OK_STRICT
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_NONSTRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_NONSTRICT_P(X)
#define REG_OK_FOR_STRREG_P(X) REG_OK_FOR_STRREG_NONSTRICT_P(X)

#else
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_STRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_STRICT_P(X)
#define REG_OK_FOR_STRREG_P(X) REG_OK_FOR_STRREG_STRICT_P(X)
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

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) 1

#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (legitimate_address_p (MODE, X, 1))				\
    goto ADDR;								\
}

#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  if (legitimate_address_p (MODE, X, 0))				\
    goto ADDR;								\
}

#endif

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

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)				\
{									\
  rtx orig_x = (X);							\
  (X) = legitimize_address (X, OLDX, MODE);				\
  if (memory_address_p (MODE, X))					\
    goto WIN;								\
}

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

/* Initialize data used by insn expanders.  This is called from
   init_emit, once for each function, before code is generated.
   For 386, clear stack slot assignments remembered from previous
   functions. */

#define INIT_EXPANDERS clear_386_stack_locals ()

/* The `FINALIZE_PIC' macro serves as a hook to emit these special
   codes once the function is being compiled into assembly code, but
   not before.  (It is not done before, because in the case of
   compiling an inline function, it would lead to multiple PIC
   prologues being included in functions which used inline functions
   and were compiled to assembly language.)  */

#define FINALIZE_PIC							\
do									\
  {									\
    extern int current_function_uses_pic_offset_table;			\
									\
    current_function_uses_pic_offset_table |= profile_flag | profile_block_flag; \
  }									\
while (0)


/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, NAME, ARGS) \
  (i386_valid_decl_attribute_p (DECL, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for TYPE.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, NAME, ARGS) \
  (i386_valid_type_attribute_p (TYPE, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is zero if the attributes on
   TYPE1 and TYPE2 are incompatible, one if they are compatible, and
   two if they are nearly compatible (which causes a warning to be
   generated).  */

#define COMP_TYPE_ATTRIBUTES(TYPE1, TYPE2) \
  (i386_comp_type_attributes (TYPE1, TYPE2))

/* If defined, a C statement that assigns default attributes to newly
   defined TYPE.  */

/* #define SET_DEFAULT_TYPE_ATTRIBUTES (TYPE) */

/* Max number of args passed in registers.  If this is more than 3, we will
   have problems with ebx (register #4), since it is a caller save register and
   is also used as the pic register in ELF.  So for now, don't allow more than
   3 registers to be passed in registers.  */

#define REGPARM_MAX 3


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
   not much slower than one with a register address.  On a 486,
   it is faster to call with a constant address than indirect.  */
#define NO_FUNCTION_CSE

/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE. */

#define RTX_COSTS(X,CODE,OUTER_CODE)				\
  case MULT:							\
    return COSTS_N_INSNS (20);					\
  case DIV:							\
  case UDIV:							\
  case MOD:							\
  case UMOD:							\
    return COSTS_N_INSNS (20);					\
  case ASHIFTRT:						\
  case LSHIFTRT:						\
  case ASHIFT:							\
    return (4 + rtx_cost (XEXP (X, 0), OUTER_CODE)		\
	    + rtx_cost (XEXP (X, 1), OUTER_CODE));		\
  case PLUS:							\
    if (GET_CODE (XEXP (X, 0)) == MULT				\
	&& GET_CODE (XEXP (XEXP (X, 0), 1)) == CONST_INT	\
	&& (INTVAL (XEXP (XEXP (X, 0), 1)) == 2			\
	    || INTVAL (XEXP (XEXP (X, 0), 1)) == 4		\
	    || INTVAL (XEXP (XEXP (X, 0), 1)) == 8))		\
      return (2 + rtx_cost (XEXP (XEXP (X, 0), 0), OUTER_CODE)	\
	      + rtx_cost (XEXP (X, 1), OUTER_CODE));		\
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
      int code;							\
      if (GET_MODE (RTX) == VOIDmode)				\
	return 2;						\
      code = standard_80387_constant_p (RTX);			\
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
   && ((OP) == EQ || (OP) == NE) ? CCFPEQmode : VOIDmode)

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

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

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
do { long l[2];								\
     REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);				\
     if (sizeof (int) == sizeof (long))					\
       fprintf (FILE, "%s 0x%x,0x%x\n", ASM_LONG, l[0], l[1]);		\
     else								\
       fprintf (FILE, "%s 0x%lx,0x%lx\n", ASM_LONG, l[0], l[1]);	\
   } while (0)

/* This is how to output a `long double' extended real constant. */

#undef ASM_OUTPUT_LONG_DOUBLE
#define ASM_OUTPUT_LONG_DOUBLE(FILE,VALUE)  		\
do { long l[3];						\
     REAL_VALUE_TO_TARGET_LONG_DOUBLE (VALUE, l);	\
     if (sizeof (int) == sizeof (long))			\
       fprintf (FILE, "%s 0x%x,0x%x,0x%x\n", ASM_LONG, l[0], l[1], l[2]); \
     else						\
       fprintf (FILE, "%s 0x%lx,0x%lx,0x%lx\n", ASM_LONG, l[0], l[1], l[2]); \
   } while (0)

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)			\
do { long l;						\
     REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
     if (sizeof (int) == sizeof (long))			\
       fprintf ((FILE), "%s 0x%x\n", ASM_LONG, l);	\
     else						\
       fprintf ((FILE), "%s 0x%lx\n", ASM_LONG, l);	\
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
   L,W,B,Q,S,T -- print the opcode suffix for specified size of operand.
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
	 case 12:					\
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
       fprintf (FILE, "%d %s", REGNO (X), RP);	\
       if (REGNO (X) == ARG_POINTER_REGNUM)		\
	 { fputs ("argp", FILE); break; }		\
       if (STACK_TOP_P (X))				\
	 { fputs ("st(0)", FILE); break; }		\
       if (FP_REG_P (X))				\
	 { fputs (hi_name[REGNO(X)], FILE); break; }	\
       switch (GET_MODE_SIZE (GET_MODE (X)))		\
	 {						\
	 default:					\
	   fputs ("e", FILE);				\
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

/* Functions in i386.c */
extern void override_options ();
extern void order_regs_for_local_alloc ();
extern int i386_valid_decl_attribute_p ();
extern int i386_valid_type_attribute_p ();
extern int i386_return_pops_args ();
extern int i386_comp_type_attributes ();
extern void init_cumulative_args ();
extern void function_arg_advance ();
extern struct rtx_def *function_arg ();
extern int function_arg_partial_nregs ();
extern void output_op_from_reg ();
extern void output_to_reg ();
extern char *singlemove_string ();
extern char *output_move_double ();
extern char *output_move_memory ();
extern char *output_move_pushmem ();
extern int standard_80387_constant_p ();
extern char *output_move_const_single ();
extern int symbolic_operand ();
extern int call_insn_operand ();
extern int expander_call_insn_operand ();
extern int symbolic_reference_mentioned_p ();
extern void emit_pic_move ();
extern void function_prologue ();
extern int simple_386_epilogue ();
extern void function_epilogue ();
extern int legitimate_address_p ();
extern struct rtx_def *legitimize_pic_address ();
extern struct rtx_def *legitimize_address ();
extern void print_operand ();
extern void print_operand_address ();
extern void notice_update_cc ();
extern void split_di ();
extern int binary_387_op ();
extern int shift_op ();
extern int VOIDmode_compare_op ();
extern char *output_387_binary_op ();
extern char *output_fix_trunc ();
extern char *output_float_compare ();
extern char *output_fp_cc0_set ();
extern void save_386_machine_status ();
extern void restore_386_machine_status ();
extern void clear_386_stack_locals ();
extern struct rtx_def *assign_386_stack_local ();

/* Variables in i386.c */
extern char *i386_reg_alloc_order;		/* register allocation order */
extern char *i386_regparm_string;		/* # registers to use to pass args */
extern char *i386_align_loops_string;		/* power of two alignment for loops */
extern char *i386_align_jumps_string;		/* power of two alignment for non-loop jumps */
extern char *i386_align_funcs_string;		/* power of two alignment for functions */
extern int i386_regparm;			/* i386_regparm_string as a number */
extern int i386_align_loops;			/* power of two alignment for loops */
extern int i386_align_jumps;			/* power of two alignment for non-loop jumps */
extern int i386_align_funcs;			/* power of two alignment for functions */
extern char *hi_reg_name[];			/* names for 16 bit regs */
extern char *qi_reg_name[];			/* names for 8 bit regs (low) */
extern char *qi_high_reg_name[];		/* names for 8 bit regs (high) */
extern enum reg_class regclass_map[];		/* smalled class containing REGNO */
extern struct rtx_def *i386_compare_op0;	/* operand 0 for comparisons */
extern struct rtx_def *i386_compare_op1;	/* operand 1 for comparisons */

/* External variables used */
extern int optimize;			/* optimization level */
extern int obey_regdecls;		/* TRUE if stupid register allocation */

/* External functions used */
extern struct rtx_def *force_operand ();

/*
Local variables:
version-control: t
End:
*/
