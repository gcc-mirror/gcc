/* Definitions of target machine for GNU compiler.  NS32000 version.
   Copyright (C) 1988, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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


/* Note that some other tm.h files include this one and then override
   many of the definitions that relate to assembler syntax.  */

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dns32000 -Dunix -Asystem(unix) -Acpu(ns32k) -Amachine(ns32k)"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (32000, GAS syntax)");


/* ABSOLUTE PREFIX, IMMEDIATE_PREFIX and EXTERNAL_PREFIX can be defined
   to cover most NS32k addressing syntax variations.  This way we don't
   need to redefine long macros in all the tm.h files for just slight
   variations in assembler syntax. */

#ifndef ABSOLUTE_PREFIX
#define ABSOLUTE_PREFIX '@'
#endif

#if defined(IMMEDIATE_PREFIX) && IMMEDIATE_PREFIX
#define PUT_IMMEDIATE_PREFIX(FILE) putc(IMMEDIATE_PREFIX, FILE)
#else
#define PUT_IMMEDIATE_PREFIX(FILE)
#endif
#if defined(ABSOLUTE_PREFIX) && ABSOLUTE_PREFIX
#define PUT_ABSOLUTE_PREFIX(FILE) putc(ABSOLUTE_PREFIX, FILE)
#else
#define PUT_ABSOLUTE_PREFIX(FILE)
#endif
#if defined(EXTERNAL_PREFIX) && EXTERNAL_PREFIX
#define PUT_EXTERNAL_PREFIX(FILE) putc(EXTERNAL_PREFIX, FILE)
#else
#define PUT_EXTERNAL_PREFIX(FILE)
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Compile 32081 insns for floating point (not library calls). */
#define TARGET_32081 (target_flags & 1)
#define TARGET_32381 (target_flags & 256)

/* The use of multiply-add instructions is optional because there may
 * be cases where it produces worse code.
 */

#define TARGET_MULT_ADD (target_flags & 512)

/* Compile using rtd insn calling sequence.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.  */
#define TARGET_RTD (target_flags & 2)

/* Compile passing first two args in regs 0 and 1.  */
#define TARGET_REGPARM (target_flags & 4)

/* Options to select type of CPU, for better optimization.
   The output is correct for any kind of 32000 regardless of these options.  */
#define TARGET_32532 (target_flags & 8)
#define TARGET_32332 (target_flags & 16)

/* Ok to use the static base register (and presume it's 0) */
#define TARGET_SB    ((target_flags & 32) == 0)
#define TARGET_HIMEM (target_flags & 128)

/* Compile using bitfield insns.  */
#define TARGET_BITFIELD ((target_flags & 64) == 0)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							      \
  { { "32081", 1, "Use hardware fp"},					      \
    { "soft-float", -257, "Don't use hardware fp"},			      \
    { "rtd", 2, "Alternative calling convention"},			      \
    { "nortd", -2, "Use normal calling convention"},			      \
    { "regparm", 4, "Pass some arguments in registers"},		      \
    { "noregparm", -4, "Pass all arguments on stack"},			      \
    { "32532", 24, "Optimize for 32532 cpu"},				      \
    { "32332", 16, "Optimize for 32332 cpu"},				      \
    { "32332", -8, 0},							      \
    { "32032", -24, "Optimize for 32032"},				      \
    { "sb", -32, "Register sb is zero. Use for absolute addressing"},	      \
    { "nosb", 32, "Do not use register sb"},				      \
    { "bitfield", -64, "Do not use bitfield instructions"},		      \
    { "nobitfield", 64, "Use bitfield instructions"},			      \
    { "himem", 128, "Generate code for high memory"},			      \
    { "nohimem", -128, "Generate code for low memory"},			      \
    { "32381", 256, "32381 fpu"},					      \
    { "mult-add", 512, "Use multiply-accumulate fp instructions"},	      \
    { "nomult-add", -512, "Do not use multiply-accumulate fp instructions" }, \
    { "src", 1024, "\"Small register classes\" kludge"},		      \
    { "nosrc", -1024, "No \"Small register classes\" kludge"},		      \
    { "", TARGET_DEFAULT, 0}}

/* TARGET_DEFAULT is defined in encore.h, pc532.h, etc.  */

/* When we are generating PIC, the sb is used as a pointer
   to the GOT. 32381 is a superset of 32081  */

#define OVERRIDE_OPTIONS				\
{							\
  if (flag_pic || TARGET_HIMEM) target_flags |= 32;	\
  if (TARGET_32381) target_flags |= 1;			\
  else target_flags &= ~512;				\
}

/* Zero or more C statements that may conditionally modify two
   variables `fixed_regs' and `call_used_regs' (both of type `char
   []') after they have been initialized from the two preceding
   macros.

   This is necessary in case the fixed or call-clobbered registers
   depend on target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target
   flags, you may indicate this to GCC by using this macro to modify
   `fixed_regs' and `call_used_regs' to 1 for each of the registers in
   the classes which should not be used by GCC.  Also define the macro
   `REG_CLASS_FROM_LETTER' to return `NO_REGS' if it is called with a
   letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all
   of the insn patterns whose constraints permit this class are
   controlled by target switches, then GCC will automatically avoid
   using these registers when the target switches are opposed to
   them.)  */

#define CONDITIONAL_REGISTER_USAGE					\
do									\
  {									\
    if (!TARGET_32081)						\
      {									\
	int regno;							\
									\
	for (regno = F0_REGNUM; regno <= F0_REGNUM + 8; regno++)	\
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
    if (!TARGET_32381)						\
      {									\
	int regno;							\
									\
	for (regno = L1_REGNUM; regno <= L1_REGNUM + 8; regno++)	\
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
  }									\
while (0)


/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the ns32k.  */
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on the ns32k.  */
#define BYTES_BIG_ENDIAN 0

/* Define this if most significant word of a multiword number is lowest
   numbered. This is not true on the ns32k.  */
#define WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 32000, this would still be 32.
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

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  National claims that the NS32032
   works without strict alignment, but rumor has it that operands
   crossing a page boundary cause unpredictable results.  */
#define STRICT_ALIGNMENT 1

/* If bit field type is int, dont let it cross an int,
   and give entire struct the alignment of an int.  */
/* Required on the 386 since it doesn't have a full set of bitfield insns.
   (There is no signed extv insn.)  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 26

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the ns32k, these are the FP, SP, (SB and PC are not included here).  */
#define FIXED_REGISTERS {0, 0, 0, 0, 0, 0, 0, 0, \
			 0, 0, 0, 0, 0, 0, 0, 0, \
                         0, 0, 0, 0, 0, 0, 0, 0, \
			 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS {1, 1, 1, 0, 0, 0, 0, 0, \
			     1, 1, 1, 1, 0, 0, 0, 0, \
			     1, 1, 0, 0, 0, 0, 0, 0, \
			     1, 1}

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", \
 "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", \
 "l1", "l1h","l3", "l3h","l5", "l5h","l7", "l7h", \
 "fp", "sp"}


#define ADDITIONAL_REGISTER_NAMES \
{{"l0", 8}, {"l2", 10}, {"l4", 12}, {"l6", 14}}

/* l0-7 are not recognized by the assembler. These are the names to use,
 * but we don't want ambiguous names in REGISTER_NAMES
 */
#define OUTPUT_REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", \
 "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", \
 "f1", "l1h","f3", "l3h","f5", "l5h","f7", "f7h", \
 "fp", "sp"}

#define REG_ALLOC_ORDER \
{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 10, 11, 18, 12, 13, 20, 14, 15, 22, 24, 25, 17, 19, 23}

/* How to renumber registers for dbx and gdb.
   NS32000 may need more change in the numeration. XXX */

#define DBX_REGISTER_NUMBER(REGNO) \
  ((REGNO) < L1_REGNUM? (REGNO) \
   : (REGNO) < FRAME_POINTER_REGNUM? (REGNO) - L1_REGNUM + 22 \
   : (REGNO) == FRAME_POINTER_REGNUM? 17 \
   : 16)

/* dwarf2out.c can't understand the funny DBX register numbering.
 * We use dwarf2out.c for exception handling even though we use DBX
 * for debugging
 */
#define DWARF_FRAME_REGNUM(REGNO) (REGNO)



#define R0_REGNUM 0
#define F0_REGNUM 8
#define L1_REGNUM 16

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* NS32000 pc is not overloaded on a register.  */
/* #define PC_REGNUM */

/* Register to use for pushing function arguments. */
#define STACK_POINTER_REGNUM 25

/* Base register for access to local variables of the function. */
#define FRAME_POINTER_REGNUM 24


/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   On the ns32k, all registers are 32 bits long except for the 32381 "long"
   registers but we treat those as pairs  */
#define LONG_FP_REGS_P(REGNO) ((REGNO) >= L1_REGNUM && (REGNO) < L1_REGNUM + 8)
#define HARD_REGNO_NREGS(REGNO, MODE)   \
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE. */
#define HARD_REGNO_MODE_OK(REGNO, MODE) hard_regno_mode_ok (REGNO, MODE)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.

   Early documentation says SI and DI are not tieable if some reg can
   be OK for SI but not for DI. However other ports (mips, i860, mvs
   and tahoe) don't meet the above criterion. Evidently the real
   requirement is somewhat laxer. Documentation was changed for gcc
   2.8 but was not picked up by egcs (at least egcs 1.0). Having all
   integer modes tieable definitely generates faster code. */

#define MODES_TIEABLE_P(MODE1, MODE2)					\
  ((FLOAT_MODE_P(MODE1) && FLOAT_MODE_P(MODE2)				\
    && (GET_MODE_UNIT_SIZE(MODE1) == GET_MODE_UNIT_SIZE(MODE2)))	\
   || (!FLOAT_MODE_P(MODE1) && !FLOAT_MODE_P(MODE2)))

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 24

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 1

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 2

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
{ NO_REGS, GENERAL_REGS, FLOAT_REG0, LONG_FLOAT_REG0, FLOAT_REGS,
  FP_REGS, GEN_AND_FP_REGS, FRAME_POINTER_REG, STACK_POINTER_REG,
  GEN_AND_MEM_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "GENERAL_REGS", "FLOAT_REG0", "LONG_FLOAT_REG0", "FLOAT_REGS", \
  "FP_REGS", "GEN_AND_FP_REGS", "FRAME_POINTER_REG", "STACK_POINTER_REG", \
  "GEN_AND_MEM_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS				\
	{{0},			/* NO_REGS */		\
	 {0x00ff},		/* GENERAL_REGS */	\
	 {0x100},		/* FLOAT_REG0 */	\
	 {0x300},		/* LONG_FLOAT_REG0 */	\
	 {0xff00},		/* FLOAT_REGS */	\
         {0xffff00},		/* FP_REGS */		\
         {0xffffff},		/* GEN_AND_FP_REGS */	\
         {0x1000000},		/* FRAME_POINTER_REG */	\
         {0x2000000},		/* STACK_POINTER_REG */	\
         {0x30000ff},		/* GEN_AND_MEM_REGS */	\
	 {0x3ffffff}		/* ALL_REGS */		\
	}

#define SUBSET_P(CLASS1, CLASS2)			\
   ((ns32k_reg_class_contents[CLASS1][0]		\
     & ~ns32k_reg_class_contents[CLASS2][0]) == 0)

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO)  (regclass_map[REGNO])

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS  GEN_AND_MEM_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)		\
 ((C) == 'u' ? FLOAT_REG0			\
  : (C) == 'v' ? LONG_FLOAT_REG0		\
  : (C) == 'f' ? FLOAT_REGS			\
  : (C) == 'l' ? FP_REGS			\
  : (C) == 'x' ? FRAME_POINTER_REG		\
  : (C) == 'y' ? STACK_POINTER_REG		\
  : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   On the ns32k, these letters are used as follows:

   I : Matches integers which are valid shift amounts for scaled indexing.
       These are 0, 1, 2, 3 for byte, word, double, and quadword.
       Used for matching arithmetic shifts only on 32032 & 32332.
   J : Matches integers which fit a "quick" operand.
   K : Matches integers 0 to 7 (for inss and exts instructions).
  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((VALUE) < 8 && (VALUE) + 8 >= 0 ?		\
   ((C) == 'I' ? (!TARGET_32532 && 0 <= (VALUE) && (VALUE) <= 3) : \
    (C) == 'J' ? (VALUE) <= 7 :			\
    (C) == 'K' ? 0 <= (VALUE) : 0) : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 1

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

/* We return GENERAL_REGS instead of GEN_AND_MEM_REGS.
   The latter offers no real additional possibilities
   and can cause spurious secondary reloading.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS) \
 ((CLASS) == GEN_AND_MEM_REGS ? GENERAL_REGS : (CLASS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the 32000, this is the size of MODE in words */

#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

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

/* A C expression whose value is RTL representing the location of the
   incoming return address at the beginning of any function, before
   the prologue.  This RTL is either a `REG', indicating that the
   return value is saved in `REG', or a `MEM' representing a location
   in the stack.

   You only need to define this macro if you want to support call
   frame debugging information like that provided by DWARF 2.

   Before the prologue, RA is at 0(sp).  */

#define INCOMING_RETURN_ADDR_RTX \
  gen_rtx (MEM, VOIDmode, gen_rtx (REG, VOIDmode, STACK_POINTER_REGNUM))

/* A C expression whose value is RTL representing the value of the
   return address for the frame COUNT steps up from the current frame,
   after the prologue.  FRAMEADDR is the frame pointer of the COUNT
   frame, or the frame pointer of the COUNT - 1 frame if
   `RETURN_ADDR_IN_PREVIOUS_FRAME' is defined.

   After the prologue, RA is at 4(fp) in the current frame.  */

#define RETURN_ADDR_RTX(COUNT, FRAME)					\
  (gen_rtx (MEM, Pmode, gen_rtx (PLUS, Pmode, (FRAME), GEN_INT(4))))

/* A C expression whose value is an integer giving the offset, in
   bytes, from the value of the stack pointer register to the top of
   the stack frame at the beginning of any function, before the
   prologue.  The top of the frame is defined to be the value of the
   stack pointer in the previous frame, just before the call
   instruction.

   You only need to define this macro if you want to support call
   frame debugging information like that provided by DWARF 2. */

#define INCOMING_FRAME_SP_OFFSET 4

/* Offset of the CFA from the argument pointer register value.  */
#define ARG_POINTER_CFA_OFFSET 8

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the 32000, sp@- in a byte insn really pushes a BYTE.  */
#define PUSH_ROUNDING(BYTES) (BYTES)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 8

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 32000, the RET insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RET can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RET is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RTD on a per module basis.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  (ns32k_return_pops_args (FUNDECL, FUNTYPE, SIZE))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the 32000 the return value is in R0,
   or perhaps in F0 if there is fp support.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) LIBCALL_VALUE(TYPE_MODE (VALTYPE))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the 32000 the return value is in R0,
   or perhaps F0 is there is fp support.  */

#define LIBCALL_VALUE(MODE)  \
  gen_rtx_REG (MODE,				  \
	       FLOAT_MODE_P(MODE) && TARGET_32081 ? F0_REGNUM: R0_REGNUM)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for a function value.
   On the 32000, R0 and F0 are the only registers thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) (((N) & ~8) == 0)

/* 1 if N is a possible register number for function argument passing.
   On the 32000, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the ns32k, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the ns32k, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
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

/* On the 32000 all args are pushed, except if -mregparm is specified
   then the first two words of arguments are passed in r0, r1.
   *NOTE* -mregparm does not work.
   It exists only to test register calling conventions.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8) ? gen_rtx_REG ((MODE), (CUM) / 4) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)	\
((TARGET_REGPARM && (CUM) < 8					\
  && 8 < ((CUM) + ((MODE) == BLKmode				\
		      ? int_size_in_bytes (TYPE)		\
		      : GET_MODE_SIZE (MODE))))  		\
 ? 2 - (CUM) / 4 : 0)

#ifndef MAIN_FUNCTION_PROLOGUE
#define MAIN_FUNCTION_PROLOGUE
#endif

/*
 * The function prologue for the ns32k is fairly simple.
 * If a frame pointer is needed (decided in reload.c ?) then
 * we need assembler of the form
 *
 *  # Save the oldframe pointer, set the new frame pointer, make space
 *  # on the stack and save any general purpose registers necessary
 *
 *  enter [<general purpose regs to save>], <local stack space>
 *
 *  movf  fn, tos    # Save any floating point registers necessary
 *  .
 *  .
 *
 * If a frame pointer is not needed we need assembler of the form
 *
 *  # Make space on the stack
 *
 *  adjspd <local stack space + 4>
 *
 *  # Save any general purpose registers necessary
 *
 *  save [<general purpose regs to save>]
 *
 *  movf  fn, tos    # Save any floating point registers necessary
 *  .
 *  .
 */
#if defined(IMMEDIATE_PREFIX) && IMMEDIATE_PREFIX
#define ADJSP(FILE, n) \
        fprintf (FILE, "\tadjspd %c%d\n", IMMEDIATE_PREFIX, (n))
#else
#define ADJSP(FILE, n) \
        fprintf (FILE, "\tadjspd %d\n", (n))
#endif

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno, g_regs_used = 0;				\
  int used_regs_buf[8], *bufp = used_regs_buf;			\
  int used_fregs_buf[17], *fbufp = used_fregs_buf;		\
  extern char call_used_regs[];					\
  MAIN_FUNCTION_PROLOGUE;					\
  for (regno = R0_REGNUM; regno < F0_REGNUM; regno++)		\
    if (regs_ever_live[regno]					\
	&& ! call_used_regs[regno])				\
      {								\
        *bufp++ = regno; g_regs_used++;				\
      }								\
  *bufp = -1;							\
  for (; regno < FRAME_POINTER_REGNUM; regno++)			\
    if (regs_ever_live[regno] && !call_used_regs[regno])	\
      {								\
        *fbufp++ = regno;					\
      }								\
  *fbufp = -1;							\
  bufp = used_regs_buf;						\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tenter [");				\
  else								\
    {								\
      if (SIZE)							\
        ADJSP (FILE, SIZE + 4);					\
      if (g_regs_used && g_regs_used > 4)			\
        fprintf (FILE, "\tsave [");				\
      else							\
	{							\
	  while (*bufp >= 0)					\
            fprintf (FILE, "\tmovd r%d,tos\n", *bufp++);	\
	  g_regs_used = 0;					\
	}							\
    }								\
  while (*bufp >= 0)						\
    {								\
      fprintf (FILE, "r%d", *bufp++);				\
      if (*bufp >= 0)						\
	fputc (',', FILE);					\
    }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, "],%d\n", SIZE);				\
  else if (g_regs_used)						\
    fprintf (FILE, "]\n");					\
  fbufp = used_fregs_buf;					\
  while (*fbufp >= 0)						\
    {								\
      if ((*fbufp & 1) || (fbufp[0] != fbufp[1] - 1))	\
	fprintf (FILE, "\tmovf %s,tos\n", ns32k_out_reg_names[*fbufp++]); \
      else							\
	{							\
	  fprintf (FILE, "\tmovl %s,tos\n",                     \
		   ns32k_out_reg_names[fbufp[0]]);                    \
	  fbufp += 2;						\
	}							\
    }								\
  if (flag_pic && current_function_uses_pic_offset_table)	\
    {								\
      fprintf (FILE, "\tsprd sb,tos\n");			\
      if (TARGET_REGPARM)					\
	{							\
	  fprintf (FILE, "\taddr __GLOBAL_OFFSET_TABLE_(pc),tos\n"); \
	  fprintf (FILE, "\tlprd sb,tos\n");			\
	}							\
      else							\
	{							\
	  fprintf (FILE, "\taddr __GLOBAL_OFFSET_TABLE_(pc),r0\n"); \
	  fprintf (FILE, "\tlprd sb,r0\n");			\
	}							\
    }								\
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.

   THIS DEFINITION FOR THE 32000 IS A GUESS.  IT HAS NOT BEEN TESTED.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\taddr LP%d,r0\n\tbsr mcount\n", (LABELNO))

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.

   We use 0, because using 1 requires hair in FUNCTION_EPILOGUE
   that is worse than the stack adjust we could save.  */

/* #define EXIT_IGNORE_STACK 1 */

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer,
   if EXIT_IGNORE_STACK is nonzero.  That doesn't apply here.

   If a frame pointer is needed (decided in reload.c ?) then
   we need assembler of the form

    movf  tos, fn	# Restore any saved floating point registers
    .
    .

    # Restore any saved general purpose registers, restore the stack
    # pointer from the frame pointer, restore the old frame pointer.
    exit [<general purpose regs to save>]

   If a frame pointer is not needed we need assembler of the form
    # Restore any general purpose registers saved

    movf  tos, fn	# Restore any saved floating point registers
    .
    .
    .
    restore [<general purpose regs to save>]

    # reclaim space allocated on stack

    adjspd <-(local stack space + 4)> */


#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno, g_regs_used = 0, f_regs_used = 0;		\
  int used_regs_buf[8], *bufp = used_regs_buf;			\
  int used_fregs_buf[17], *fbufp = used_fregs_buf;		\
  extern char call_used_regs[];					\
  if (flag_pic && current_function_uses_pic_offset_table)	\
    fprintf (FILE, "\tlprd sb,tos\n");				\
  *fbufp++ = -2;						\
  for (regno = F0_REGNUM; regno < FRAME_POINTER_REGNUM; regno++) \
    if (regs_ever_live[regno] && !call_used_regs[regno])	\
      {								\
       *fbufp++ = regno; f_regs_used++;				\
      }								\
  fbufp--;							\
  for (regno = 0; regno < F0_REGNUM; regno++)			\
    if (regs_ever_live[regno]					\
	&& ! call_used_regs[regno])				\
      {                                                        	\
        *bufp++ = regno; g_regs_used++;				\
      }                                                        	\
  while (fbufp > used_fregs_buf)				\
    {								\
      if ((*fbufp & 1) && fbufp[0] == fbufp[-1] + 1)	        \
	{							\
	  fprintf (FILE, "\tmovl tos,%s\n",                     \
		   ns32k_out_reg_names[fbufp[-1]]);                   \
	  fbufp -= 2;						\
	}							\
      else fprintf (FILE, "\tmovf tos,%s\n", ns32k_out_reg_names[*fbufp--]); \
    }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\texit [");					\
  else								\
    {								\
      if (g_regs_used && g_regs_used > 4)			\
        fprintf (FILE, "\trestore [");				\
      else							\
        {							\
	  while (bufp > used_regs_buf)				\
            fprintf (FILE, "\tmovd tos,r%d\n", *--bufp);	\
	  g_regs_used = 0;					\
        }							\
    }								\
  while (bufp > used_regs_buf)					\
    {								\
      fprintf (FILE, "r%d", *--bufp);				\
      if (bufp > used_regs_buf)					\
	fputc (',', FILE);					\
    }								\
  if (g_regs_used || frame_pointer_needed)			\
    fprintf (FILE, "]\n");					\
  if (SIZE && !frame_pointer_needed)				\
    ADJSP (FILE, -(SIZE + 4));					\
  if (current_function_pops_args)				\
    fprintf (FILE, "\tret %d\n", current_function_pops_args);	\
  else fprintf (FILE, "\tret 0\n"); }

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.  */

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH)			\
{								\
  int regno;							\
  int offset = -4;						\
  for (regno = 0; regno < L1_REGNUM; regno++)			\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += 4;						\
  for (; regno < FRAME_POINTER_REGNUM; regno++)			\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      offset += 8;						\
  if (flag_pic && current_function_uses_pic_offset_table)	\
    offset += 4;						\
  (DEPTH) = (offset + get_frame_size ()				\
	     + (get_frame_size () == 0 ? 0 : 4));		\
}


/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.  */

/* On the 32k, the trampoline looks like this:
     addr  0(pc),r2
     jump  @__trampoline
     .int STATIC
     .int FUNCTION
Doing trampolines with a library assist function is easier than figuring
out how to do stores to memory in reverse byte order (the way immediate
operands on the 32k are stored).  */

#define TRAMPOLINE_TEMPLATE(FILE)					\
{									\
  fprintf (FILE, "\taddr 0(pc),r2\n" );					\
  fprintf (FILE, "\tjump " );						\
  PUT_ABSOLUTE_PREFIX (FILE);						\
  fprintf (FILE, "__trampoline\n" );					\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
  ASM_OUTPUT_INT (FILE, const0_rtx);					\
}

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE 20

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			     \
{									     \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 12)), CXT);    \
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (TRAMP, 16)), FNADDR); \
}

/* This is the library routine that is used
   to transfer control from the trampoline
   to the actual nested function.  */

/* The function name __transfer_from_trampoline is not actually used.
   The function definition just permits use of "asm with operands"
   (though the operand list is empty).  */
#define TRANSFER_FROM_TRAMPOLINE	\
void					\
__transfer_from_trampoline ()		\
{					\
  asm (".globl __trampoline");		\
  asm ("__trampoline:");		\
  asm ("movd 16(r2),tos");		\
  asm ("movd 12(r2),r1");		\
  asm ("ret 0");			\
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT 0 */
/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_DECREMENT 0 */
/* #define HAVE_PRE_INCREMENT 0 */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

/* note that FP and SP cannot be used as an index. What about PC? */
#define REGNO_OK_FOR_INDEX_P(REGNO)  \
((REGNO) < F0_REGNUM || (unsigned)reg_renumber[REGNO] < F0_REGNUM)
#define REGNO_OK_FOR_BASE_P(REGNO)   \
((REGNO) < F0_REGNUM || (unsigned)reg_renumber[REGNO] < F0_REGNUM \
 || (REGNO) == FRAME_POINTER_REGNUM || (REGNO) == STACK_POINTER_REGNUM)

#define FP_REG_P(X) \
 (GET_CODE (X) == REG && REGNO (X) >= F0_REGNUM && REGNO (X) < FRAME_POINTER_REGNUM)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.
   This might not work on future ns32k processors as negative
   displacements are not officially allowed but a mode reserved
   to National.  This works on processors up to 32532, though,
   and we don't expect any new ones in the series ;-( */

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST						\
   || (GET_CODE (X) == CONST_INT					\
       && NS32K_DISPLACEMENT_P (INTVAL (X))))

#define CONSTANT_ADDRESS_NO_LABEL_P(X)   \
  (GET_CODE (X) == CONST_INT						\
   && NS32K_DISPLACEMENT_P (INTVAL (X)))

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  secondary_reload_class (CLASS, MODE, IN)

/*  Certain machines have the property that some registers cannot be
    copied to some other registers without using memory.  Define this
    macro on those machines to be a C expression that is non-zero if
    objects of mode M in registers of CLASS1 can only be copied to
    registers of class CLASS2 by storing a register of CLASS1 into
    memory and loading that memory location into a register of CLASS2.

    On the ns32k, floating point regs can only be loaded through memory

    The movdf and movsf insns in ns32k.md copy between general and
    floating registers using the stack. In principle, we could get
    better code not allowing that case in the constraints and defining
    SECONDARY_MEMORY_NEEDED in practice, though the stack slots used
    are not available for optimization.  */

#if 0
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, M)			\
     secondary_memory_needed(CLASS1, CLASS2, M)
#endif

/* SMALL_REGISTER_CLASSES is a run time option. This should no longer
   be necessay and should go when we have confidence that we won't run
   out of spill registers */
#define SMALL_REGISTER_CLASSES (target_flags & 1024)

/* A C expression whose value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.

   The default definition won't do because class LONG_FLOAT_REG0 has two
   registers which are always acessed as a pair */

#define CLASS_LIKELY_SPILLED_P(CLASS) \
  (reg_class_size[(int) (CLASS)] == 1 || (CLASS) == LONG_FLOAT_REG0)


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

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
  (REGNO (X) < F0_REGNUM || REGNO (X) >= FIRST_PSEUDO_REGISTER)
/* Nonzero if X is a hard reg that can be used as a base reg
   of if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (REGNO (X) < F0_REGNUM || REGNO (X) >= FRAME_POINTER_REGNUM)
/* Nonzero if X is a floating point reg or a pseudo reg.  */

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

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS.  */

/* 1 if X is an address that we could indirect through.  */
/***** NOTE ***** There is a bug in the Sequent assembler which fails
 to fixup addressing information for symbols used as offsets
 from registers which are not FP or SP (or SB or PC).  This
 makes _x(fp) valid, while _x(r0) is invalid.  */

#define INDIRECTABLE_1_ADDRESS_P(X)  \
  (CONSTANT_ADDRESS_P (X)						\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == PLUS						\
       && GET_CODE (XEXP (X, 0)) == REG					\
       && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
       && ((flag_pic || TARGET_HIMEM) ? 				\
	     CONSTANT_ADDRESS_NO_LABEL_P (XEXP (X, 1))	 		\
	   :								\
	     CONSTANT_ADDRESS_P (XEXP (X, 1))) 				\
       && (GET_CODE (X) != CONST_INT || NS32K_DISPLACEMENT_P (INTVAL (X)))))

/* 1 if integer I will fit in a 4 byte displacement field.
   Strictly speaking, we can't be sure that a symbol will fit this range.
   But, in practice, it always will.  */

/* idall@eleceng.adelaide.edu.au says that the 32016 and 32032
   can handle the full range of displacements--it is only the addresses
   that have a limited range.  So the following was deleted:
 (((i) <= 16777215 && (i) >= -16777216)
  || ((TARGET_32532 || TARGET_32332) && ...))  */
#define NS32K_DISPLACEMENT_P(i) 				\
  ((i) < (1 << 29) && (i) >= - (1 << 29))

/* Check for frame pointer or stack pointer.  */
#define MEM_REG(X) \
  (GET_CODE (X) == REG && (REGNO (X) == FRAME_POINTER_REGNUM  \
			   || REGNO(X) == STACK_POINTER_REGNUM))

/* A memory ref whose address is the FP or SP, with optional integer offset,
   or (on certain machines) a constant address.  */
#define INDIRECTABLE_2_ADDRESS_P(X)  \
  (GET_CODE (X) == MEM							\
   && (((xfoo0 = XEXP (X, 0), MEM_REG (xfoo0))				\
       || (GET_CODE (xfoo0) == PLUS					\
	   && MEM_REG (XEXP (xfoo0, 0))					\
	   && CONSTANT_ADDRESS_NO_LABEL_P (XEXP (xfoo0, 1))))		\
       || (TARGET_SB && CONSTANT_ADDRESS_P (xfoo0))))

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */
#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)		\
{ 							\
  if (INDIRECTABLE_1_ADDRESS_P (X)) goto ADDR;		\
  if (INDIRECTABLE_2_ADDRESS_P (X)) goto ADDR;		\
  if (GET_CODE (X) == PLUS)				\
    if (CONSTANT_ADDRESS_NO_LABEL_P (XEXP (X, 1)))	\
      if (INDIRECTABLE_2_ADDRESS_P (XEXP (X, 0)))	\
	goto ADDR;					\
}

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */
#define GO_IF_INDEXING(X, MODE, ADDR)  \
{ register rtx xfoob = (X);						\
  if (GET_CODE (xfoob) == PLUS && INDEX_TERM_P (XEXP (xfoob, 0), MODE))	\
    GO_IF_INDEXABLE_ADDRESS (XEXP (xfoob, 1), ADDR);			\
  if (GET_CODE (xfoob) == PLUS && INDEX_TERM_P (XEXP (xfoob, 1), MODE))	\
    GO_IF_INDEXABLE_ADDRESS (XEXP (xfoob, 0), ADDR); }			\

#define GO_IF_INDEXABLE_ADDRESS(X, ADDR) \
{ if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X)) goto ADDR;		\
  if (INDIRECTABLE_2_ADDRESS_P (X)) goto ADDR;				\
  if (INDIRECTABLE_1_ADDRESS_P (X)) goto ADDR;				\
}

/* 1 if PROD is either a reg times size of mode MODE
   or just a reg, if MODE is just one byte. Actually, on the ns32k,
   since the index mode is independent of the operand size,
   we can match more stuff...

   This macro's expansion uses the temporary variables xfoo0, xfoo1
   and xfoo2 that must be declared in the surrounding context.  */
#define INDEX_TERM_P(PROD, MODE)   \
((GET_CODE (PROD) == REG && REG_OK_FOR_INDEX_P (PROD))			\
 || (GET_CODE (PROD) == MULT						\
     &&	(xfoo0 = XEXP (PROD, 0), xfoo1 = XEXP (PROD, 1),		\
	 (GET_CODE (xfoo1) == CONST_INT					\
	  && GET_CODE (xfoo0) == REG					\
	  && FITS_INDEX_RANGE (INTVAL (xfoo1))				\
	  && REG_OK_FOR_INDEX_P (xfoo0)))))

#define FITS_INDEX_RANGE(X)  \
  ((xfoo2 = (unsigned)(X)-1),						\
   ((xfoo2 < 4 && xfoo2 != 2) || xfoo2 == 7))

/* Note that xfoo0, xfoo1, xfoo2 are used in some of the submacros above.  */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{ register rtx xfooy, xfoo0, xfoo1;					\
  unsigned xfoo2;							\
  xfooy = X;								\
  if (flag_pic && cfun && ! current_function_uses_pic_offset_table	\
      && global_symbolic_reference_mentioned_p (X, 1))			\
    current_function_uses_pic_offset_table = 1;				\
  GO_IF_NONINDEXED_ADDRESS (xfooy, ADDR);				\
  if (GET_CODE (xfooy) == PLUS)						\
    {									\
      if (CONSTANT_ADDRESS_NO_LABEL_P (XEXP (xfooy, 1))			\
	  && GET_CODE (XEXP (xfooy, 0)) == PLUS)			\
	xfooy = XEXP (xfooy, 0);					\
      else if (CONSTANT_ADDRESS_NO_LABEL_P (XEXP (xfooy, 0))		\
	  && GET_CODE (XEXP (xfooy, 1)) == PLUS)			\
	xfooy = XEXP (xfooy, 1);					\
      GO_IF_INDEXING (xfooy, MODE, ADDR);				\
    }									\
  else if (INDEX_TERM_P (xfooy, MODE))					\
    goto ADDR;								\
  else if (GET_CODE (xfooy) == PRE_DEC)					\
    {									\
      if (REGNO (XEXP (xfooy, 0)) == STACK_POINTER_REGNUM) goto ADDR;	\
      else abort ();							\
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

   For the ns32k, we do nothing */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)   {}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_PIC_OPERAND_P(X) \
  (((! current_function_uses_pic_offset_table			\
     && symbolic_reference_mentioned_p (X))?			\
      (current_function_uses_pic_offset_table = 1):0		\
   ), (! SYMBOLIC_CONST (X)					\
   || GET_CODE (X) == SYMBOL_REF || GET_CODE (X) == LABEL_REF))

#define SYMBOLIC_CONST(X)	\
(GET_CODE (X) == SYMBOL_REF						\
 || GET_CODE (X) == LABEL_REF						\
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On the ns32k, if using PIC, mark a SYMBOL_REF for a non-global
   symbol or a code symbol. These symbols are referenced via pc
   and not via sb. */

#define ENCODE_SECTION_INFO(DECL) \
do									\
  {									\
    extern int flag_pic;						\
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

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the ns32k, only predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 { if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC)	\
     goto LABEL;}

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for DECL.
   The attributes in ATTRIBUTES have previously been assigned to DECL.  */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTRIBUTES, NAME, ARGS) \
  (ns32k_valid_decl_attribute_p (DECL, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is nonzero if IDENTIFIER
   with arguments ARGS is a valid machine specific attribute for TYPE.
   The attributes in ATTRIBUTES have previously been assigned to TYPE.  */

#define VALID_MACHINE_TYPE_ATTRIBUTE(TYPE, ATTRIBUTES, NAME, ARGS) \
  (ns32k_valid_type_attribute_p (TYPE, ATTRIBUTES, NAME, ARGS))

/* If defined, a C expression whose value is zero if the attributes on
   TYPE1 and TYPE2 are incompatible, one if they are compatible, and
   two if they are nearly compatible (which causes a warning to be
   generated).  */

#define COMP_TYPE_ATTRIBUTES(TYPE1, TYPE2) \
  (ns32k_comp_type_attributes (TYPE1, TYPE2))

/* If defined, a C statement that assigns default attributes to newly
   defined TYPE.  */

/* #define SET_DEFAULT_TYPE_ATTRIBUTES (TYPE) */

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.
   HI mode is more efficient but the range is not wide enough for
   all programs. */
#define CASE_VECTOR_MODE SImode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE 1

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* The number of scalar move insns which should be generated instead
   of a string move insn or a library call.
   
   We have a smart movstrsi insn */
#define MOVE_RATIO 0

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of address ADDRESS. */

#define ADDRESS_COST(RTX) calc_address_cost (RTX)

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) <= 7 && INTVAL (RTX) >= -8) return 0;	\
    if (INTVAL (RTX) < 0x2000 && INTVAL (RTX) >= -0x2000)	\
      return 1;							\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    return 5;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* This bit means that what ought to be in the Z bit
   should be tested in the F bit.  */
#define CC_Z_IN_F 04000

/* This bit means that what ought to be in the Z bit
   is complemented in the F bit.  */
#define CC_Z_IN_NOT_F 010000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ if (GET_CODE (EXP) == SET)					\
    { if (GET_CODE (SET_DEST (EXP)) == CC0)			\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (EXP);			\
	  cc_status.value2 = SET_SRC (EXP);			\
	}							\
      else if (GET_CODE (SET_SRC (EXP)) == CALL)		\
	{ CC_STATUS_INIT; }					\
      else if (GET_CODE (SET_DEST (EXP)) == REG)		\
	{ if (cc_status.value1					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value1)) \
	    cc_status.value1 = 0;				\
	  if (cc_status.value2					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value2)) \
	    cc_status.value2 = 0;				\
	}							\
      else if (GET_CODE (SET_DEST (EXP)) == MEM)		\
	{ CC_STATUS_INIT; }					\
    }								\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    { if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == CC0)	\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (XVECEXP (EXP, 0, 0));	\
	  cc_status.value2 = SET_SRC (XVECEXP (EXP, 0, 0));	\
	}							\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == REG) \
	{ if (cc_status.value1					\
	      && reg_overlap_mentioned_p (SET_DEST (XVECEXP (EXP, 0, 0)), cc_status.value1)) \
	    cc_status.value1 = 0;				\
	  if (cc_status.value2					\
	      && reg_overlap_mentioned_p (SET_DEST (XVECEXP (EXP, 0, 0)), cc_status.value2)) \
	    cc_status.value2 = 0;				\
	}							\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == MEM) \
	{ CC_STATUS_INIT; }					\
    }								\
  else if (GET_CODE (EXP) == CALL)				\
    { /* all bets are off */ CC_STATUS_INIT; }			\
  else { /* nothing happens? CC_STATUS_INIT; */}		\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))	\
    abort ();			\
}

/* Describe the costs of the following register moves which are discouraged:
   1.) Moves between the Floating point registers and the frame pointer and stack pointer
   2.) Moves between the stack pointer and the frame pointer
   3.) Moves between the floating point and general registers

  These all involve two memory references. This is worse than a memory
  to memory move (default cost 4)
 */

#define REGISTER_MOVE_COST(CLASS1, CLASS2)  register_move_cost(CLASS1, CLASS2)

#define OUTPUT_JUMP(NORMAL, NO_OV)  \
{ if (cc_status.flags & CC_NO_OVERFLOW)				\
    return NO_OV;						\
  return NORMAL; }

/* Dividing the output into sections */

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Define the output Assembly Language */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE) fprintf (FILE, "#NO_APP\n");

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* Output of Data */

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double 0d%.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.float 0f%.20e\n", (VALUE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an assembler line defining an external/static
   address which is not in tree format (for collect.c).  */

/* The prefix to add to user-visible assembler symbols. */
#define USER_LABEL_PREFIX "_"

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmovd %s,tos\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmovd tos,%s\n", reg_names[REGNO])

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#ifndef COLLECT
#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)
#else
#define ASM_OUTPUT_LABEL(STREAM,NAME)					\
do {									\
  fprintf (STREAM, "%s:\n", NAME);					\
} while (0)
#endif

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#ifndef COLLECT
#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)
#else
#define ASM_GLOBALIZE_LABEL(STREAM,NAME)				\
do {									\
  fprintf (STREAM, "\t.globl\t%s\n", NAME);				\
} while (0)
#endif

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%ld", PREFIX, (long) NUM)

/* This is how to align the code that follows an unconditional branch.  */

#define LABEL_ALIGN_AFTER_BARRIER(LABEL) (2)

/* This is how to output an element of a case-vector that is absolute.
   (The ns32k does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */
/* ** Notice that the second element is LI format! */
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  fprintf (FILE, "\t.long L%d-LI%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

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

/* Print an instruction operand X on file FILE.
   CODE is the code from the %-spec that requested printing this operand;
   if `%z3' was used to print operand 3, then CODE is 'z'. */

/* %$ means print the prefix for an immediate operand.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '$' || (CODE) == '?')

#define PRINT_OPERAND(FILE, X, CODE)       print_operand(FILE, X, CODE)

/* Print a memory operand whose address is X, on file FILE.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address(FILE, ADDR)

extern unsigned int ns32k_reg_class_contents[N_REG_CLASSES][1];
extern const char *const ns32k_out_reg_names[];
extern enum reg_class regclass_map[];		/* smallest class containing REGNO */

/*
Local variables:
version-control: t
End:
*/
